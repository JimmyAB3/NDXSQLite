{===============================================================================
  NDXSQLite Example 102 - Inventory Management
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Stock receiving, shipping, and transfers
  - Stock reservation and fulfillment
  - Multi-warehouse inventory tracking
  - Reorder point calculations
  - Inventory movement history

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program InventoryManagement;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // Products with stock levels and thresholds
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  sku TEXT UNIQUE,' +
    '  category TEXT,' +
    '  unit_price REAL,' +
    '  reorder_level INTEGER DEFAULT 10,' +
    '  max_stock INTEGER DEFAULT 1000' +
    ')');

  // Stock levels per warehouse location
  Conn.ExecuteNonQuery(
    'CREATE TABLE stock_levels (' +
    '  product_id TEXT NOT NULL,' +
    '  warehouse TEXT NOT NULL,' +
    '  quantity INTEGER DEFAULT 0,' +
    '  reserved INTEGER DEFAULT 0,' +
    '  PRIMARY KEY (product_id, warehouse),' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')');

  // Stock movements (in/out)
  Conn.ExecuteNonQuery(
    'CREATE TABLE stock_movements (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  product_id TEXT NOT NULL,' +
    '  warehouse TEXT NOT NULL,' +
    '  movement_type TEXT NOT NULL,' +  // receive, ship, transfer_in, transfer_out, adjust, return
    '  quantity INTEGER NOT NULL,' +
    '  lot_number TEXT,' +
    '  reference TEXT,' +
    '  moved_at TEXT NOT NULL,' +
    '  moved_by TEXT,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')');

  // Lot/batch tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE lots (' +
    '  lot_number TEXT PRIMARY KEY,' +
    '  product_id TEXT NOT NULL,' +
    '  warehouse TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  manufactured_at TEXT,' +
    '  expires_at TEXT,' +
    '  received_at TEXT,' +
    '  supplier TEXT,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')');

  // Reservations (pending orders)
  Conn.ExecuteNonQuery(
    'CREATE TABLE reservations (' +
    '  id TEXT PRIMARY KEY,' +
    '  product_id TEXT NOT NULL,' +
    '  warehouse TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  order_ref TEXT,' +
    '  status TEXT DEFAULT ''active'',' +  // active, fulfilled, cancelled
    '  reserved_at TEXT,' +
    '  expires_at TEXT,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')');
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  // Products
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, sku, category, unit_price, reorder_level, max_stock) VALUES ' +
    '(''prod-001'', ''Widget A'', ''WDG-A-001'', ''components'', 12.50, 20, 500)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, sku, category, unit_price, reorder_level, max_stock) VALUES ' +
    '(''prod-002'', ''Gadget B'', ''GDG-B-002'', ''assemblies'', 45.00, 10, 200)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, sku, category, unit_price, reorder_level, max_stock) VALUES ' +
    '(''prod-003'', ''Sensor C'', ''SNS-C-003'', ''components'', 8.75, 50, 1000)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, sku, category, unit_price, reorder_level, max_stock) VALUES ' +
    '(''prod-004'', ''Motor D'', ''MTR-D-004'', ''assemblies'', 125.00, 5, 100)');

  // Initial stock levels
  Conn.ExecuteNonQuery('INSERT INTO stock_levels (product_id, warehouse, quantity) VALUES (''prod-001'', ''warehouse-A'', 100)');
  Conn.ExecuteNonQuery('INSERT INTO stock_levels (product_id, warehouse, quantity) VALUES (''prod-001'', ''warehouse-B'', 50)');
  Conn.ExecuteNonQuery('INSERT INTO stock_levels (product_id, warehouse, quantity) VALUES (''prod-002'', ''warehouse-A'', 30)');
  Conn.ExecuteNonQuery('INSERT INTO stock_levels (product_id, warehouse, quantity) VALUES (''prod-003'', ''warehouse-A'', 200)');
  Conn.ExecuteNonQuery('INSERT INTO stock_levels (product_id, warehouse, quantity) VALUES (''prod-003'', ''warehouse-B'', 150)');
  Conn.ExecuteNonQuery('INSERT INTO stock_levels (product_id, warehouse, quantity) VALUES (''prod-004'', ''warehouse-A'', 8)');
end;

{ Increases the stock level for a product at a warehouse, records the receive movement, and creates a lot record with supplier, manufacturing, and expiry details. }
procedure ReceiveStock(const ProductId, Warehouse: string; Qty: Integer;
  const LotNum, Supplier, ManufDate, ExpiryDate, Timestamp, ReceivedBy: string);
begin
  // Update stock level
  Conn.ExecuteNonQuery(
    'UPDATE stock_levels SET quantity = quantity + ' + IntToStr(Qty) +
    ' WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + Warehouse + '''');

  // Record movement
  Conn.ExecuteNonQuery(
    'INSERT INTO stock_movements (product_id, warehouse, movement_type, quantity, lot_number, reference, moved_at, moved_by) VALUES (' +
    '''' + ProductId + ''', ''' + Warehouse + ''', ''receive'', ' + IntToStr(Qty) + ', ' +
    '''' + LotNum + ''', ''' + Supplier + ''', ''' + Timestamp + ''', ''' + ReceivedBy + ''')');

  // Create lot record
  if LotNum <> '' then
    Conn.ExecuteNonQuery(
      'INSERT INTO lots (lot_number, product_id, warehouse, quantity, manufactured_at, expires_at, received_at, supplier) VALUES (' +
      '''' + LotNum + ''', ''' + ProductId + ''', ''' + Warehouse + ''', ' + IntToStr(Qty) + ', ' +
      '''' + ManufDate + ''', ''' + ExpiryDate + ''', ''' + Timestamp + ''', ''' + Supplier + ''')');
end;

{ Decreases the stock level for a product at a warehouse and records a ship movement with the order reference. }
procedure ShipStock(const ProductId, Warehouse: string; Qty: Integer;
  const OrderRef, Timestamp, ShippedBy: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE stock_levels SET quantity = quantity - ' + IntToStr(Qty) +
    ' WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + Warehouse + '''');

  Conn.ExecuteNonQuery(
    'INSERT INTO stock_movements (product_id, warehouse, movement_type, quantity, reference, moved_at, moved_by) VALUES (' +
    '''' + ProductId + ''', ''' + Warehouse + ''', ''ship'', -' + IntToStr(Qty) + ', ' +
    '''' + OrderRef + ''', ''' + Timestamp + ''', ''' + ShippedBy + ''')');
end;

{ Moves stock from one warehouse to another by decreasing the source, increasing the destination, and recording both transfer_out and transfer_in movements. }
procedure TransferStock(const ProductId, FromWarehouse, ToWarehouse: string; Qty: Integer;
  const Timestamp, TransferBy: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE stock_levels SET quantity = quantity - ' + IntToStr(Qty) +
    ' WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + FromWarehouse + '''');
  Conn.ExecuteNonQuery(
    'UPDATE stock_levels SET quantity = quantity + ' + IntToStr(Qty) +
    ' WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + ToWarehouse + '''');

  Conn.ExecuteNonQuery(
    'INSERT INTO stock_movements (product_id, warehouse, movement_type, quantity, reference, moved_at, moved_by) VALUES (' +
    '''' + ProductId + ''', ''' + FromWarehouse + ''', ''transfer_out'', -' + IntToStr(Qty) + ', ' +
    '''to:' + ToWarehouse + ''', ''' + Timestamp + ''', ''' + TransferBy + ''')');
  Conn.ExecuteNonQuery(
    'INSERT INTO stock_movements (product_id, warehouse, movement_type, quantity, reference, moved_at, moved_by) VALUES (' +
    '''' + ProductId + ''', ''' + ToWarehouse + ''', ''transfer_in'', ' + IntToStr(Qty) + ', ' +
    '''from:' + FromWarehouse + ''', ''' + Timestamp + ''', ''' + TransferBy + ''')');
end;

{ Checks available stock (quantity minus reserved), creates a reservation record if sufficient, and increases the reserved count for the product/warehouse. }
function ReserveStock(const ResId, ProductId, Warehouse: string; Qty: Integer;
  const OrderRef, Timestamp, ExpiresAt: string): Boolean;
var
  Available: Integer;
begin
  Result := False;
  DS := Conn.ExecuteQuery(
    'SELECT quantity - reserved as available FROM stock_levels ' +
    'WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + Warehouse + '''');
  try
    if DS.EOF then Exit;
    Available := DS.FieldByName('available').AsInteger;
  finally
    DS.Free;
  end;

  if Available < Qty then Exit;

  Conn.ExecuteNonQuery(
    'UPDATE stock_levels SET reserved = reserved + ' + IntToStr(Qty) +
    ' WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + Warehouse + '''');

  Conn.ExecuteNonQuery(
    'INSERT INTO reservations (id, product_id, warehouse, quantity, order_ref, status, reserved_at, expires_at) VALUES (' +
    '''' + ResId + ''', ''' + ProductId + ''', ''' + Warehouse + ''', ' + IntToStr(Qty) + ', ' +
    '''' + OrderRef + ''', ''active'', ''' + Timestamp + ''', ''' + ExpiresAt + ''')');

  Result := True;
end;

{ Looks up an active reservation, reduces both reserved and quantity from stock, marks the reservation fulfilled, and records a ship movement. }
procedure FulfillReservation(const ResId, Timestamp, FulfilledBy: string);
var
  ProductId, Warehouse: string;
  Qty: Integer;
begin
  DS := Conn.ExecuteQuery('SELECT product_id, warehouse, quantity FROM reservations WHERE id = ''' + ResId + ''' AND status = ''active''');
  try
    if DS.EOF then Exit;
    ProductId := DS.FieldByName('product_id').AsString;
    Warehouse := DS.FieldByName('warehouse').AsString;
    Qty := DS.FieldByName('quantity').AsInteger;
  finally
    DS.Free;
  end;

  // Release reservation and reduce stock
  Conn.ExecuteNonQuery(
    'UPDATE stock_levels SET reserved = reserved - ' + IntToStr(Qty) + ', ' +
    'quantity = quantity - ' + IntToStr(Qty) +
    ' WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + Warehouse + '''');

  Conn.ExecuteNonQuery(
    'UPDATE reservations SET status = ''fulfilled'' WHERE id = ''' + ResId + '''');

  Conn.ExecuteNonQuery(
    'INSERT INTO stock_movements (product_id, warehouse, movement_type, quantity, reference, moved_at, moved_by) VALUES (' +
    '''' + ProductId + ''', ''' + Warehouse + ''', ''ship'', -' + IntToStr(Qty) + ', ' +
    '''reservation:' + ResId + ''', ''' + Timestamp + ''', ''' + FulfilledBy + ''')');
end;

{ Looks up an active reservation, decreases the reserved count to release the stock, and marks the reservation as cancelled. }
procedure CancelReservation(const ResId: string);
var
  ProductId, Warehouse: string;
  Qty: Integer;
begin
  DS := Conn.ExecuteQuery('SELECT product_id, warehouse, quantity FROM reservations WHERE id = ''' + ResId + ''' AND status = ''active''');
  try
    if DS.EOF then Exit;
    ProductId := DS.FieldByName('product_id').AsString;
    Warehouse := DS.FieldByName('warehouse').AsString;
    Qty := DS.FieldByName('quantity').AsInteger;
  finally
    DS.Free;
  end;

  Conn.ExecuteNonQuery(
    'UPDATE stock_levels SET reserved = reserved - ' + IntToStr(Qty) +
    ' WHERE product_id = ''' + ProductId + ''' AND warehouse = ''' + Warehouse + '''');

  Conn.ExecuteNonQuery(
    'UPDATE reservations SET status = ''cancelled'' WHERE id = ''' + ResId + '''');
end;

{ Demo procedures }

procedure DemoInitialStock;
begin
  WriteLn('=== 1. Initial Stock Levels ===');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT p.name, p.sku, sl.warehouse, sl.quantity, sl.reserved, p.reorder_level ' +
    'FROM stock_levels sl JOIN products p ON sl.product_id = p.id ' +
    'ORDER BY p.name, sl.warehouse');
  try
    WriteLn(Format('   %-12s %-12s %-14s %5s %5s %5s', ['Product', 'SKU', 'Warehouse', 'Qty', 'Rsvd', 'Reord']));
    WriteLn('   ' + StringOfChar('-', 65));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-12s %-14s %5d %5d %5d',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('sku').AsString,
         DS.FieldByName('warehouse').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('reserved').AsInteger,
         DS.FieldByName('reorder_level').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Receives stock with lot numbers, ships orders, transfers between warehouses, and prints the updated stock levels showing quantity changes. }
procedure DemoStockMovements;
begin
  WriteLn('=== 2. Stock Movements ===');
  WriteLn;

  // Receive new stock with lot tracking
  ReceiveStock('prod-001', 'warehouse-A', 50, 'LOT-2024-001', 'SupplierX',
    '2024-01-15', '2025-01-15', '2024-02-01T09:00:00', 'receiver.joe');
  WriteLn('   Received: 50x Widget A at warehouse-A (LOT-2024-001, expires 2025-01-15)');

  ReceiveStock('prod-003', 'warehouse-B', 100, 'LOT-2024-002', 'SupplierY',
    '2024-01-20', '2024-12-31', '2024-02-01T09:30:00', 'receiver.joe');
  WriteLn('   Received: 100x Sensor C at warehouse-B (LOT-2024-002, expires 2024-12-31)');

  // Ship stock
  ShipStock('prod-002', 'warehouse-A', 5, 'ORD-5001', '2024-02-02T10:00:00', 'shipper.anna');
  WriteLn('   Shipped: 5x Gadget B from warehouse-A (order ORD-5001)');

  ShipStock('prod-001', 'warehouse-A', 20, 'ORD-5002', '2024-02-02T11:00:00', 'shipper.anna');
  WriteLn('   Shipped: 20x Widget A from warehouse-A (order ORD-5002)');

  // Transfer between warehouses
  TransferStock('prod-001', 'warehouse-A', 'warehouse-B', 30, '2024-02-03T08:00:00', 'logistics.bob');
  WriteLn('   Transfer: 30x Widget A from warehouse-A to warehouse-B');

  WriteLn;
  WriteLn('   Updated stock levels:');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, sl.warehouse, sl.quantity ' +
    'FROM stock_levels sl JOIN products p ON sl.product_id = p.id ' +
    'ORDER BY p.name, sl.warehouse');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %-12s %-14s qty=%d',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('warehouse').AsString,
         DS.FieldByName('quantity').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates stock reservation and release workflows. }
procedure DemoReservations;
var
  Ok: Boolean;
begin
  WriteLn('=== 3. Stock Reservations ===');
  WriteLn;

  // Reserve stock for pending orders
  Ok := ReserveStock('res-001', 'prod-001', 'warehouse-A', 25, 'ORD-5010',
    '2024-02-04T09:00:00', '2024-02-11T09:00:00');
  WriteLn(Format('   Reserve 25x Widget A for ORD-5010: %s', [BoolToStr(Ok, 'SUCCESS', 'FAILED')]));

  Ok := ReserveStock('res-002', 'prod-002', 'warehouse-A', 10, 'ORD-5011',
    '2024-02-04T09:30:00', '2024-02-11T09:30:00');
  WriteLn(Format('   Reserve 10x Gadget B for ORD-5011: %s', [BoolToStr(Ok, 'SUCCESS', 'FAILED')]));

  Ok := ReserveStock('res-003', 'prod-004', 'warehouse-A', 15, 'ORD-5012',
    '2024-02-04T10:00:00', '2024-02-11T10:00:00');
  WriteLn(Format('   Reserve 15x Motor D for ORD-5012:  %s (only 8 available)', [BoolToStr(Ok, 'SUCCESS', 'FAILED')]));

  WriteLn;
  WriteLn('   Stock with reservations:');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, sl.warehouse, sl.quantity, sl.reserved, (sl.quantity - sl.reserved) as available ' +
    'FROM stock_levels sl JOIN products p ON sl.product_id = p.id ' +
    'WHERE sl.reserved > 0 ORDER BY p.name');
  try
    WriteLn(Format('   %-12s %-14s %5s %5s %5s', ['Product', 'Warehouse', 'Qty', 'Rsvd', 'Avail']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-14s %5d %5d %5d',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('warehouse').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('reserved').AsInteger,
         DS.FieldByName('available').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Fulfill one reservation
  WriteLn;
  FulfillReservation('res-001', '2024-02-05T14:00:00', 'shipper.anna');
  WriteLn('   Fulfilled reservation res-001 (25x Widget A shipped)');

  // Cancel another
  CancelReservation('res-002');
  WriteLn('   Cancelled reservation res-002 (stock released)');

  WriteLn;
  WriteLn('   Reservation status:');
  DS := Conn.ExecuteQuery(
    'SELECT r.id, p.name, r.quantity, r.status, r.order_ref ' +
    'FROM reservations r JOIN products p ON r.product_id = p.id ORDER BY r.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %s: %dx %s [%s] order=%s',
        [DS.FieldByName('id').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('order_ref').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Receives additional lots with expiry dates, lists all lots sorted by expiry, and identifies lots expiring before a cutoff date. }
procedure DemoLotTracking;
begin
  WriteLn('=== 4. Lot/Batch Tracking ===');
  WriteLn;

  // Add more lots
  ReceiveStock('prod-001', 'warehouse-A', 75, 'LOT-2024-003', 'SupplierX',
    '2024-02-10', '2025-02-10', '2024-02-15T09:00:00', 'receiver.joe');
  ReceiveStock('prod-004', 'warehouse-A', 12, 'LOT-2024-004', 'SupplierZ',
    '2024-02-01', '2026-02-01', '2024-02-15T10:00:00', 'receiver.joe');

  WriteLn('   All lots in inventory:');
  DS := Conn.ExecuteQuery(
    'SELECT l.lot_number, p.name, l.warehouse, l.quantity, l.supplier, l.manufactured_at, l.expires_at ' +
    'FROM lots l JOIN products p ON l.product_id = p.id ORDER BY l.expires_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %s  %-10s  %-12s  qty=%3d  supplier=%-10s  mfg=%s  exp=%s',
        [DS.FieldByName('lot_number').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('warehouse').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('supplier').AsString,
         DS.FieldByName('manufactured_at').AsString,
         DS.FieldByName('expires_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Check for expiring lots
  WriteLn;
  WriteLn('   Lots expiring before 2025-01-31:');
  DS := Conn.ExecuteQuery(
    'SELECT l.lot_number, p.name, l.quantity, l.expires_at ' +
    'FROM lots l JOIN products p ON l.product_id = p.id ' +
    'WHERE l.expires_at < ''2025-01-31'' ORDER BY l.expires_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    WARNING: %s (%s) qty=%d expires %s',
        [DS.FieldByName('lot_number').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('expires_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Ships stock to trigger low inventory, prints products where available quantity is at or below reorder level, and shows total stock per product across all warehouses. }
procedure DemoLowStockAlerts;
begin
  WriteLn('=== 5. Low-Stock Alerts ===');
  WriteLn;

  // Ship more to trigger low stock
  ShipStock('prod-004', 'warehouse-A', 16, 'ORD-5020', '2024-02-16T10:00:00', 'shipper.anna');
  WriteLn('   Shipped 16x Motor D (stock now below reorder level)');
  WriteLn;

  // Check stock vs reorder level
  WriteLn('   Low-stock alerts (quantity below reorder level):');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, p.sku, sl.warehouse, sl.quantity, sl.reserved, ' +
    '(sl.quantity - sl.reserved) as available, p.reorder_level ' +
    'FROM stock_levels sl JOIN products p ON sl.product_id = p.id ' +
    'WHERE (sl.quantity - sl.reserved) <= p.reorder_level ' +
    'ORDER BY (sl.quantity - sl.reserved) ASC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    ALERT: %s (%s) at %s: available=%d, reorder_level=%d',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('sku').AsString,
         DS.FieldByName('warehouse').AsString,
         DS.FieldByName('available').AsInteger,
         DS.FieldByName('reorder_level').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Total stock across all warehouses
  WriteLn;
  WriteLn('   Total stock per product (all warehouses):');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, p.sku, SUM(sl.quantity) as total_qty, SUM(sl.reserved) as total_reserved, ' +
    'p.reorder_level, p.max_stock ' +
    'FROM stock_levels sl JOIN products p ON sl.product_id = p.id ' +
    'GROUP BY p.id ORDER BY p.name');
  try
    while not DS.EOF do
    begin
      Write(Format('    %-12s total=%3d  reserved=%2d  reorder=%2d',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('total_qty').AsInteger,
         DS.FieldByName('total_reserved').AsInteger,
         DS.FieldByName('reorder_level').AsInteger]));
      if DS.FieldByName('total_qty').AsInteger <= DS.FieldByName('reorder_level').AsInteger then
        Write('  ** LOW **');
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Prints the complete movement history for Widget A showing timestamp, movement type, quantity delta, lot number, and reference for each entry. }
procedure DemoMovementHistory;
begin
  WriteLn('=== 6. Movement History ===');
  WriteLn;

  WriteLn('   All movements for Widget A:');
  DS := Conn.ExecuteQuery(
    'SELECT sm.movement_type, sm.warehouse, sm.quantity, sm.lot_number, sm.reference, sm.moved_at, sm.moved_by ' +
    'FROM stock_movements sm WHERE sm.product_id = ''prod-001'' ORDER BY sm.moved_at');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('quantity').AsInteger >= 0 then
        Write(Format('    %s %-12s +%d',
          [DS.FieldByName('moved_at').AsString,
           DS.FieldByName('movement_type').AsString,
           DS.FieldByName('quantity').AsInteger]))
      else
        Write(Format('    %s %-12s %d',
          [DS.FieldByName('moved_at').AsString,
           DS.FieldByName('movement_type').AsString,
           DS.FieldByName('quantity').AsInteger]));
      if DS.FieldByName('lot_number').AsString <> '' then
        Write(Format(' lot=%s', [DS.FieldByName('lot_number').AsString]));
      if DS.FieldByName('reference').AsString <> '' then
        Write(Format(' ref=%s', [DS.FieldByName('reference').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Calculates and prints per-product valuation (quantity times unit price) and the grand total inventory value across all products. }
procedure DemoStockValuation;
begin
  WriteLn('=== 7. Stock Valuation ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT p.name, p.sku, SUM(sl.quantity) as total_qty, p.unit_price, ' +
    'CAST(SUM(sl.quantity) * p.unit_price AS REAL) as total_value ' +
    'FROM stock_levels sl JOIN products p ON sl.product_id = p.id ' +
    'GROUP BY p.id ORDER BY total_value DESC');
  try
    WriteLn(Format('   %-12s %-12s %5s %10s %12s', ['Product', 'SKU', 'Qty', 'Unit $', 'Total $']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-12s %5d %10.2f %12.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('sku').AsString,
         DS.FieldByName('total_qty').AsInteger,
         DS.FieldByName('unit_price').AsFloat,
         DS.FieldByName('total_value').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT CAST(SUM(sl.quantity * p.unit_price) AS REAL) as grand_total ' +
    'FROM stock_levels sl JOIN products p ON sl.product_id = p.id');
  try
    WriteLn(Format('   Grand total inventory value: $%.2f', [DS.FieldByName('grand_total').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates inventory statistics across warehouses and products. }
procedure DemoStats;
var
  MoveCount, LotCount, ResCount: Integer;
begin
  WriteLn('=== 8. Inventory Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM stock_movements');
  try MoveCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM lots');
  try LotCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM reservations');
  try ResCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  WriteLn(Format('   Stock movements: %d', [MoveCount]));
  WriteLn(Format('   Active lots: %d', [LotCount]));
  WriteLn(Format('   Reservations: %d', [ResCount]));

  WriteLn;
  WriteLn('   Movements by type:');
  DS := Conn.ExecuteQuery(
    'SELECT movement_type, COUNT(*) as cnt, SUM(quantity) as total_qty ' +
    'FROM stock_movements GROUP BY movement_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %-14s %d movements, net qty: %d',
        [DS.FieldByName('movement_type').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('total_qty').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays a summary of inventory management patterns demonstrated. }
procedure DemoSummary;
begin
  WriteLn('=== Inventory Management Summary ===');
  WriteLn;
  WriteLn('   Patterns demonstrated:');
  WriteLn('   - Multi-warehouse stock levels');
  WriteLn('   - Stock movements (receive, ship, transfer, return)');
  WriteLn('   - Lot/batch tracking with expiry dates');
  WriteLn('   - Stock reservations (reserve, fulfill, cancel)');
  WriteLn('   - Low-stock alerts (available vs reorder level)');
  WriteLn('   - Movement history and audit trail');
  WriteLn('   - Stock valuation reporting');
  WriteLn('   - Expiring lot detection');
end;

begin
  WriteLn('Example 102: Inventory Management - Movements, Reservations, Lots');
  WriteLn('===================================================================');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    SetupDatabase;
    SeedData;

    WriteLn('Database setup complete.');
    WriteLn;

    DemoInitialStock;
    DemoStockMovements;
    DemoReservations;
    DemoLotTracking;
    DemoLowStockAlerts;
    DemoMovementHistory;
    DemoStockValuation;
    DemoStats;
    DemoSummary;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
