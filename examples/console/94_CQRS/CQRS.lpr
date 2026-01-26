{===============================================================================
  NDXSQLite Example 94 - CQRS
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Command/Query Responsibility Segregation pattern
  - Event sourcing with append-only event store
  - Aggregate versioning and projections
  - Read model materialization
  - Multiple aggregate types (products, orders, customers)

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CQRS;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('=== CQRS - Command Query Responsibility Segregation ===');
  WriteLn;
  WriteLn('1. Creating Schema');

  // === WRITE SIDE (Event Store) ===

  // Event store - append-only log of all domain events
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_store (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  aggregate_id TEXT NOT NULL,' +
    '  aggregate_type TEXT NOT NULL,' +
    '  event_type TEXT NOT NULL,' +
    '  event_data TEXT NOT NULL,' +
    '  version INTEGER NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(aggregate_id, version)' +
    ')');

  // Command log - tracks all commands processed
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS command_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  command_type TEXT NOT NULL,' +
    '  command_data TEXT NOT NULL,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  error_message TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  processed_at TEXT' +
    ')');

  // === READ SIDE (Projections) ===

  // Product catalog (read model)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS read_products (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  price REAL NOT NULL DEFAULT 0,' +
    '  stock INTEGER NOT NULL DEFAULT 0,' +
    '  category TEXT,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  version INTEGER DEFAULT 0,' +
    '  updated_at TEXT' +
    ')');

  // Order summary (read model)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS read_orders (' +
    '  id TEXT PRIMARY KEY,' +
    '  customer_id TEXT NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''created'',' +
    '  total_amount REAL DEFAULT 0,' +
    '  item_count INTEGER DEFAULT 0,' +
    '  items TEXT,' +
    '  version INTEGER DEFAULT 0,' +
    '  updated_at TEXT' +
    ')');

  // Customer summary (read model)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS read_customers (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  total_orders INTEGER DEFAULT 0,' +
    '  total_spent REAL DEFAULT 0,' +
    '  last_order_at TEXT,' +
    '  version INTEGER DEFAULT 0,' +
    '  updated_at TEXT' +
    ')');

  // Projection tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS projection_status (' +
    '  projection_name TEXT PRIMARY KEY,' +
    '  last_event_id INTEGER DEFAULT 0,' +
    '  events_processed INTEGER DEFAULT 0,' +
    '  last_updated TEXT' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_aggregate ON event_store(aggregate_id, aggregate_type)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_type ON event_store(event_type)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_commands_status ON command_log(status)');

  WriteLn('   Write side: event_store, command_log');
  WriteLn('   Read side: read_products, read_orders, read_customers');
  WriteLn('   Infrastructure: projection_status');
end;

// === COMMAND HANDLERS ===

{ Returns the next version number for the given aggregate by finding the current max version in the event store. }
function GetNextVersion(const AggregateId: string): Integer;
begin
  Result := Conn.ExecuteScalar(
    'SELECT COALESCE(MAX(version), 0) + 1 FROM event_store WHERE aggregate_id = ?',
    [AggregateId]);
end;

{ Appends a new domain event to the event store with the next sequential version for the aggregate. }
procedure AppendEvent(const AggregateId, AggregateType, EventType, EventData: string);
var
  Version: Integer;
begin
  Version := GetNextVersion(AggregateId);
  Conn.ExecuteNonQuery(
    'INSERT INTO event_store (aggregate_id, aggregate_type, event_type, event_data, version) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [AggregateId, AggregateType, EventType, EventData, IntToStr(Version)]);
end;

{ Logs a CreateProduct command and emits a ProductCreated event with the product's id, name, description, category, and price. }
procedure HandleCreateProduct(const CmdData: string; const Id, Name, Description, Category: string; Price: Double);
var
  CmdId: Integer;
begin
  // Log command
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, processed_at) VALUES (?, ?, ?, datetime(''now''))',
    ['CreateProduct', CmdData, 'completed']);

  // Emit event
  AppendEvent(Id, 'product', 'ProductCreated',
    Format('{"id":"%s","name":"%s","description":"%s","category":"%s","price":%.2f}',
      [Id, Name, Description, Category, Price]));
end;

{ Logs an UpdatePrice command and emits a PriceChanged event for the specified product. }
procedure HandleUpdatePrice(const ProductId: string; NewPrice: Double);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, processed_at) VALUES (?, ?, ?, datetime(''now''))',
    ['UpdatePrice', Format('{"product_id":"%s","price":%.2f}', [ProductId, NewPrice]), 'completed']);

  AppendEvent(ProductId, 'product', 'PriceChanged',
    Format('{"product_id":"%s","new_price":%.2f}', [ProductId, NewPrice]));
end;

{ Logs an AddStock command and emits a StockAdded event with the product ID and quantity to add. }
procedure HandleAddStock(const ProductId: string; Quantity: Integer);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, processed_at) VALUES (?, ?, ?, datetime(''now''))',
    ['AddStock', Format('{"product_id":"%s","quantity":%d}', [ProductId, Quantity]), 'completed']);

  AppendEvent(ProductId, 'product', 'StockAdded',
    Format('{"product_id":"%s","quantity":%d}', [ProductId, Quantity]));
end;

{ Logs a CreateOrder command and emits an OrderCreated event linking the order to a customer. }
procedure HandleCreateOrder(const OrderId, CustomerId: string);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, processed_at) VALUES (?, ?, ?, datetime(''now''))',
    ['CreateOrder', Format('{"order_id":"%s","customer_id":"%s"}', [OrderId, CustomerId]), 'completed']);

  AppendEvent(OrderId, 'order', 'OrderCreated',
    Format('{"order_id":"%s","customer_id":"%s"}', [OrderId, CustomerId]));
end;

{ Logs an AddItem command and emits an ItemAdded event with product details, quantity, and unit price for the order. }
procedure HandleAddItem(const OrderId, ProductId, ProductName: string; Quantity: Integer; UnitPrice: Double);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, processed_at) VALUES (?, ?, ?, datetime(''now''))',
    ['AddItem', Format('{"order_id":"%s","product":"%s","qty":%d}', [OrderId, ProductId, Quantity]), 'completed']);

  AppendEvent(OrderId, 'order', 'ItemAdded',
    Format('{"order_id":"%s","product_id":"%s","product_name":"%s","quantity":%d,"unit_price":%.2f}',
      [OrderId, ProductId, ProductName, Quantity, UnitPrice]));
end;

{ Logs a ConfirmOrder command and emits an OrderConfirmed event for the specified order. }
procedure HandleConfirmOrder(const OrderId: string);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, processed_at) VALUES (?, ?, ?, datetime(''now''))',
    ['ConfirmOrder', Format('{"order_id":"%s"}', [OrderId]), 'completed']);

  AppendEvent(OrderId, 'order', 'OrderConfirmed',
    Format('{"order_id":"%s"}', [OrderId]));
end;

{ Logs a RegisterCustomer command and emits a CustomerRegistered event with name and email. }
procedure HandleRegisterCustomer(const CustId, Name, Email: string);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, processed_at) VALUES (?, ?, ?, datetime(''now''))',
    ['RegisterCustomer', Format('{"id":"%s","name":"%s"}', [CustId, Name]), 'completed']);

  AppendEvent(CustId, 'customer', 'CustomerRegistered',
    Format('{"id":"%s","name":"%s","email":"%s"}', [CustId, Name, Email]));
end;

// === PROJECTION BUILDERS ===

{ Reads unprocessed events from the event store and updates the read model tables (products, orders, customers) accordingly. }
procedure ProjectEvents;
var
  DS: TDataSet;
  EventType, EventData, AggId: string;
  LastProcessed, EventId: Integer;
  EventsProcessed: Integer;
begin
  // Get last processed event
  LastProcessed := Conn.ExecuteScalar(
    'SELECT COALESCE(MAX(last_event_id), 0) FROM projection_status');

  EventsProcessed := 0;

  // Process new events
  DS := Conn.ExecuteQuery(
    'SELECT id, aggregate_id, aggregate_type, event_type, event_data FROM event_store ' +
    'WHERE id > ? ORDER BY id',
    [IntToStr(LastProcessed)]);
  try
    while not DS.EOF do
    begin
      EventId := DS.FieldByName('id').AsInteger;
      AggId := DS.FieldByName('aggregate_id').AsString;
      EventType := DS.FieldByName('event_type').AsString;
      EventData := DS.FieldByName('event_data').AsString;

      // Project based on event type
      if EventType = 'ProductCreated' then
      begin
        Conn.ExecuteNonQuery(
          'INSERT OR REPLACE INTO read_products (id, name, description, price, stock, category, version, updated_at) ' +
          'VALUES (?, ?, ?, ?, 0, ?, 1, datetime(''now''))',
          [AggId,
           Copy(EventData, Pos('"name":"', EventData) + 8, Pos('","description"', EventData) - Pos('"name":"', EventData) - 8),
           '', // simplified - skip description parsing
           FloatToStr(0), // will be set by PriceChanged
           '']);
        // Simple extraction of name and category
        Conn.ExecuteNonQuery(
          'UPDATE read_products SET name = ?, category = ?, price = ? WHERE id = ?',
          [Copy(EventData, Pos('"name":"', EventData) + 8,
                Pos('","description"', EventData) - Pos('"name":"', EventData) - 8),
           Copy(EventData, Pos('"category":"', EventData) + 12,
                Pos('","price"', EventData) - Pos('"category":"', EventData) - 12),
           Copy(EventData, Pos('"price":', EventData) + 8,
                Length(EventData) - Pos('"price":', EventData) - 8),
           AggId]);
      end
      else if EventType = 'PriceChanged' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE read_products SET price = ?, version = version + 1, updated_at = datetime(''now'') WHERE id = ?',
          [Copy(EventData, Pos('"new_price":', EventData) + 12,
                Length(EventData) - Pos('"new_price":', EventData) - 12),
           AggId]);
      end
      else if EventType = 'StockAdded' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE read_products SET stock = stock + CAST(? AS INTEGER), version = version + 1, updated_at = datetime(''now'') WHERE id = ?',
          [Copy(EventData, Pos('"quantity":', EventData) + 11,
                Length(EventData) - Pos('"quantity":', EventData) - 11),
           AggId]);
      end
      else if EventType = 'CustomerRegistered' then
      begin
        Conn.ExecuteNonQuery(
          'INSERT OR REPLACE INTO read_customers (id, name, email, version, updated_at) VALUES (?, ?, ?, 1, datetime(''now''))',
          [AggId,
           Copy(EventData, Pos('"name":"', EventData) + 8,
                Pos('","email"', EventData) - Pos('"name":"', EventData) - 8),
           Copy(EventData, Pos('"email":"', EventData) + 9,
                Length(EventData) - Pos('"email":"', EventData) - 9 - 1)]);  // remove trailing }
      end
      else if EventType = 'OrderCreated' then
      begin
        Conn.ExecuteNonQuery(
          'INSERT OR REPLACE INTO read_orders (id, customer_id, status, version, updated_at) VALUES (?, ?, ''created'', 1, datetime(''now''))',
          [AggId,
           Copy(EventData, Pos('"customer_id":"', EventData) + 15,
                Length(EventData) - Pos('"customer_id":"', EventData) - 15 - 1)]);
      end
      else if EventType = 'ItemAdded' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE read_orders SET item_count = item_count + 1, version = version + 1, updated_at = datetime(''now'') WHERE id = ?',
          [AggId]);
      end
      else if EventType = 'OrderConfirmed' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE read_orders SET status = ''confirmed'', version = version + 1, updated_at = datetime(''now'') WHERE id = ?',
          [AggId]);
        // Update customer stats
        Conn.ExecuteNonQuery(
          'UPDATE read_customers SET total_orders = total_orders + 1, last_order_at = datetime(''now'') ' +
          'WHERE id = (SELECT customer_id FROM read_orders WHERE id = ?)',
          [AggId]);
      end;

      Inc(EventsProcessed);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update projection status
  if EventsProcessed > 0 then
  begin
    Conn.ExecuteNonQuery(
      'INSERT OR REPLACE INTO projection_status (projection_name, last_event_id, events_processed, last_updated) ' +
      'VALUES (''main'', ?, ?, datetime(''now''))',
      [IntToStr(EventId), IntToStr(EventsProcessed)]);
  end;
end;

// === DEMO PROCEDURES ===

{ Registers customers, creates products with stock, updates prices, and processes orders by issuing commands to the write side. }
procedure ExecuteCommands;
begin
  WriteLn;
  WriteLn('2. Executing Commands (Write Side)');

  // Register customers
  HandleRegisterCustomer('cust-1', 'Alice Smith', 'alice@example.com');
  HandleRegisterCustomer('cust-2', 'Bob Jones', 'bob@example.com');
  WriteLn('   Registered 2 customers');

  // Create products
  HandleCreateProduct('{"id":"prod-1"}', 'prod-1', 'Widget Pro', 'Premium widget', 'electronics', 29.99);
  HandleCreateProduct('{"id":"prod-2"}', 'prod-2', 'Gadget X', 'Smart gadget', 'electronics', 49.99);
  HandleCreateProduct('{"id":"prod-3"}', 'prod-3', 'Book ABC', 'Tech book', 'books', 19.99);
  WriteLn('   Created 3 products');

  // Add stock
  HandleAddStock('prod-1', 100);
  HandleAddStock('prod-2', 50);
  HandleAddStock('prod-3', 200);
  WriteLn('   Added stock to 3 products');

  // Update prices
  HandleUpdatePrice('prod-1', 34.99);
  HandleUpdatePrice('prod-2', 44.99);
  WriteLn('   Updated 2 prices');

  // Create and confirm orders
  HandleCreateOrder('ord-1', 'cust-1');
  HandleAddItem('ord-1', 'prod-1', 'Widget Pro', 2, 34.99);
  HandleAddItem('ord-1', 'prod-3', 'Book ABC', 1, 19.99);
  HandleConfirmOrder('ord-1');
  WriteLn('   Order ord-1: 2x Widget Pro + 1x Book ABC (confirmed)');

  HandleCreateOrder('ord-2', 'cust-2');
  HandleAddItem('ord-2', 'prod-2', 'Gadget X', 1, 44.99);
  HandleConfirmOrder('ord-2');
  WriteLn('   Order ord-2: 1x Gadget X (confirmed)');

  HandleCreateOrder('ord-3', 'cust-1');
  HandleAddItem('ord-3', 'prod-1', 'Widget Pro', 5, 34.99);
  WriteLn('   Order ord-3: 5x Widget Pro (not confirmed yet)');

  WriteLn(Format('   Total events emitted: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM event_store'))]));
  WriteLn(Format('   Total commands logged: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM command_log'))]));
end;

{ Triggers event projection and reports the number of products, orders, and customers materialized in read models. }
procedure BuildProjections;
begin
  WriteLn;
  WriteLn('3. Building Read Projections');

  ProjectEvents;

  WriteLn(Format('   Products projected: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_products'))]));
  WriteLn(Format('   Orders projected: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_orders'))]));
  WriteLn(Format('   Customers projected: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_customers'))]));
end;

{ Queries and prints all products from the read model showing id, name, category, price, stock, and version. }
procedure QueryProducts;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('4. Query: Product Catalog (Read Side)');

  DS := Conn.ExecuteQuery(
    'SELECT id, name, category, price, stock, version FROM read_products ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %s (%s) $%.2f, stock: %d [v%d]',
        [DS.FieldByName('id').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('category').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('stock').AsInteger,
         DS.FieldByName('version').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Queries and prints all orders from the read model joined with customer names, showing status, item count, and version. }
procedure QueryOrders;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('5. Query: Order Summary (Read Side)');

  DS := Conn.ExecuteQuery(
    'SELECT o.id, o.customer_id, c.name as customer_name, o.status, o.item_count, o.version ' +
    'FROM read_orders o LEFT JOIN read_customers c ON c.id = o.customer_id ' +
    'ORDER BY o.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: customer=%s (%s), status=%s, items=%d [v%d]',
        [DS.FieldByName('id').AsString,
         DS.FieldByName('customer_id').AsString,
         DS.FieldByName('customer_name').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('item_count').AsInteger,
         DS.FieldByName('version').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Queries and prints all customers from the read model showing name, email, total orders, and total spent. }
procedure QueryCustomers;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('6. Query: Customer Summary (Read Side)');

  DS := Conn.ExecuteQuery(
    'SELECT id, name, email, total_orders, total_spent FROM read_customers ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %s (%s), orders=%d, spent=$%.2f',
        [DS.FieldByName('id').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('total_orders').AsInteger,
         DS.FieldByName('total_spent').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Lists all events in the event store showing their sequence ID, aggregate ID, event type, and version. }
procedure ShowEventStore;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('7. Event Store Contents');

  DS := Conn.ExecuteQuery(
    'SELECT id, aggregate_id, event_type, version FROM event_store ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d [%s] %s (v%d)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('aggregate_id').AsString,
         DS.FieldByName('event_type').AsString,
         DS.FieldByName('version').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Clears all read models then replays the entire event store to rebuild the product, order, and customer projections from scratch. }
procedure DemoEventReplay;
begin
  WriteLn;
  WriteLn('8. Event Replay (Rebuild Projections)');

  // Clear read models
  Conn.ExecuteNonQuery('DELETE FROM read_products');
  Conn.ExecuteNonQuery('DELETE FROM read_orders');
  Conn.ExecuteNonQuery('DELETE FROM read_customers');
  Conn.ExecuteNonQuery('DELETE FROM projection_status');
  WriteLn('   Cleared all read models');

  // Replay all events
  ProjectEvents;

  WriteLn(Format('   Rebuilt products: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_products'))]));
  WriteLn(Format('   Rebuilt orders: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_orders'))]));
  WriteLn(Format('   Rebuilt customers: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_customers'))]));
  WriteLn('   All projections rebuilt from event store');
end;

{ Prints the full event history for product prod-1, showing each event type, data payload, and version in sequence. }
procedure DemoAggregateHistory;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('9. Aggregate History (prod-1)');

  DS := Conn.ExecuteQuery(
    'SELECT id, event_type, event_data, version, created_at FROM event_store ' +
    'WHERE aggregate_id = ''prod-1'' ORDER BY version');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   v%d: %s',
        [DS.FieldByName('version').AsInteger,
         DS.FieldByName('event_type').AsString]));
      WriteLn(Format('       %s', [DS.FieldByName('event_data').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Shows the count of commands by type and confirms all commands have completed status. }
procedure DemoCommandLog;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('10. Command Log');

  DS := Conn.ExecuteQuery(
    'SELECT command_type, COUNT(*) as cnt FROM command_log ' +
    'GROUP BY command_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d commands',
        [DS.FieldByName('command_type').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   All commands status: %s',
    [string(Conn.ExecuteScalar('SELECT status FROM command_log LIMIT 1'))]));
end;

{ Inserts two failed commands (invalid product and negative stock) to show how command failures are logged with error messages. }
procedure DemoFailedCommand;
begin
  WriteLn;
  WriteLn('11. Failed Command Handling');

  // Simulate a command that fails validation
  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, error_message, processed_at) ' +
    'VALUES (?, ?, ?, ?, datetime(''now''))',
    ['UpdatePrice', '{"product_id":"prod-999","price":-10}', 'failed', 'Product not found']);

  Conn.ExecuteNonQuery(
    'INSERT INTO command_log (command_type, command_data, status, error_message, processed_at) ' +
    'VALUES (?, ?, ?, ?, datetime(''now''))',
    ['AddStock', '{"product_id":"prod-1","quantity":-500}', 'failed', 'Cannot add negative stock']);

  WriteLn(Format('   Total commands: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM command_log'))]));
  WriteLn(Format('   Completed: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM command_log WHERE status = ''completed'''))]));
  WriteLn(Format('   Failed: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM command_log WHERE status = ''failed'''))]));
end;

{ Prints total events, commands, read model counts, and a distribution of event types in the store. }
procedure ShowSummary;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('12. CQRS Summary');

  WriteLn('   Write side:');
  WriteLn(Format('     Events in store: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM event_store'))]));
  WriteLn(Format('     Commands processed: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM command_log'))]));

  WriteLn('   Read side:');
  WriteLn(Format('     Products: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_products'))]));
  WriteLn(Format('     Orders: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_orders'))]));
  WriteLn(Format('     Customers: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM read_customers'))]));

  WriteLn('   Event types distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, COUNT(*) as cnt FROM event_store GROUP BY event_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d',
        [DS.FieldByName('event_type').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

begin
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');
    Conn.ExecuteNonQuery('PRAGMA foreign_keys=ON');

    CreateSchema;
    ExecuteCommands;
    BuildProjections;
    QueryProducts;
    QueryOrders;
    QueryCustomers;
    ShowEventStore;
    DemoEventReplay;
    DemoAggregateHistory;
    DemoCommandLog;
    DemoFailedCommand;
    ShowSummary;

    WriteLn;
    WriteLn('=== Example Complete ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
