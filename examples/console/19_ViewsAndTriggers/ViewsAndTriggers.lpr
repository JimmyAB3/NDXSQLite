{===============================================================================
  NDXSQLite Example 19 - Views and Triggers
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Creating and using views
  - INSTEAD OF triggers for views
  - Audit logging with triggers
  - BEFORE/AFTER triggers

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ViewsAndTriggers;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  // Products table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT,' +
    '  price REAL,' +
    '  stock INTEGER DEFAULT 0' +
    ')');

  // Categories table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT' +
    ')');

  // Insert sample data
  Connection.ExecuteNonQuery('INSERT INTO categories (name, description) VALUES (''Electronics'', ''Electronic devices'')');
  Connection.ExecuteNonQuery('INSERT INTO categories (name, description) VALUES (''Books'', ''Printed and digital books'')');
  Connection.ExecuteNonQuery('INSERT INTO categories (name, description) VALUES (''Clothing'', ''Apparel and accessories'')');

  Connection.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Laptop'', ''Electronics'', 999.99, 50)');
  Connection.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Phone'', ''Electronics'', 699.99, 100)');
  Connection.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Pascal Book'', ''Books'', 49.99, 200)');
  Connection.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''T-Shirt'', ''Clothing'', 19.99, 500)');
end;

{ Creates a view filtering products with prices above 100 and queries it. }
procedure DemoSimpleView;
var
  DS: TDataSet;
begin
  WriteLn('1. Simple View');
  WriteLn('   -----------');

  // Create a simple view
  Connection.ExecuteNonQuery(
    'CREATE VIEW IF NOT EXISTS expensive_products AS ' +
    'SELECT id, name, price FROM products WHERE price > 100');

  WriteLn('   Created view: expensive_products (price > 100)');

  DS := Connection.ExecuteQuery('SELECT * FROM expensive_products');
  try
    WriteLn('   Results:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: $%.2f',
        [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a view that joins products with their category details. }
procedure DemoViewWithJoin;
var
  DS: TDataSet;
begin
  WriteLn('2. View with JOIN');
  WriteLn('   --------------');

  Connection.ExecuteNonQuery(
    'CREATE VIEW IF NOT EXISTS product_details AS ' +
    'SELECT p.id, p.name, p.price, p.stock, c.name as category_name, c.description as category_desc ' +
    'FROM products p ' +
    'LEFT JOIN categories c ON p.category = c.name');

  WriteLn('   Created view: product_details (products + categories)');

  DS := Connection.ExecuteQuery('SELECT name, price, category_name FROM product_details');
  try
    WriteLn('   Results:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s ($%.2f) - Category: %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('category_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a view that computes per-category product counts, average prices, and stock totals. }
procedure DemoViewWithAggregation;
var
  DS: TDataSet;
begin
  WriteLn('3. View with Aggregation');
  WriteLn('   ---------------------');

  Connection.ExecuteNonQuery(
    'CREATE VIEW IF NOT EXISTS category_stats AS ' +
    'SELECT category, COUNT(*) as product_count, ' +
    '       AVG(price) as avg_price, SUM(stock) as total_stock ' +
    'FROM products GROUP BY category');

  WriteLn('   Created view: category_stats');

  DS := Connection.ExecuteQuery('SELECT * FROM category_stats');
  try
    WriteLn('   Category statistics:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d products, avg $%.2f, %d in stock',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('product_count').AsInteger,
         DS.FieldByName('avg_price').AsFloat,
         DS.FieldByName('total_stock').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates an AFTER UPDATE trigger that logs price changes to an audit table. }
procedure DemoAuditTrigger;
var
  DS: TDataSet;
begin
  WriteLn('4. Audit Trigger (AFTER UPDATE)');
  WriteLn('   ----------------------------');

  // Create audit log table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS audit_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  table_name TEXT,' +
    '  operation TEXT,' +
    '  record_id INTEGER,' +
    '  old_value TEXT,' +
    '  new_value TEXT,' +
    '  changed_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Create trigger for price changes
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS audit_price_change ' +
    'AFTER UPDATE OF price ON products ' +
    'WHEN OLD.price <> NEW.price ' +
    'BEGIN ' +
    '  INSERT INTO audit_log (table_name, operation, record_id, old_value, new_value) ' +
    '  VALUES (''products'', ''UPDATE'', NEW.id, OLD.price, NEW.price); ' +
    'END');

  WriteLn('   Created trigger: audit_price_change');

  // Update a price
  Connection.ExecuteNonQuery('UPDATE products SET price = 899.99 WHERE name = ''Laptop''');
  Connection.ExecuteNonQuery('UPDATE products SET price = 649.99 WHERE name = ''Phone''');
  WriteLn('   Updated Laptop and Phone prices');

  // Check audit log
  DS := Connection.ExecuteQuery('SELECT * FROM audit_log');
  try
    WriteLn('   Audit log:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %s #%d changed from %s to %s at %s',
        [DS.FieldByName('table_name').AsString,
         DS.FieldByName('operation').AsString,
         DS.FieldByName('record_id').AsInteger,
         DS.FieldByName('old_value').AsString,
         DS.FieldByName('new_value').AsString,
         DS.FieldByName('changed_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a BEFORE INSERT trigger that rejects rows with negative stock or price values. }
procedure DemoBeforeInsertTrigger;
var
  DS: TDataSet;
begin
  WriteLn('5. BEFORE INSERT Trigger (Validation)');
  WriteLn('   ----------------------------------');

  // Create trigger to validate stock
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS validate_stock ' +
    'BEFORE INSERT ON products ' +
    'BEGIN ' +
    '  SELECT CASE ' +
    '    WHEN NEW.stock < 0 THEN RAISE(ABORT, ''Stock cannot be negative'') ' +
    '    WHEN NEW.price < 0 THEN RAISE(ABORT, ''Price cannot be negative'') ' +
    '  END; ' +
    'END');

  WriteLn('   Created trigger: validate_stock');

  // Try to insert with negative stock
  try
    Connection.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Bad Product'', ''Test'', 10, -5)');
    WriteLn('   ERROR: Should have failed!');
  except
    on E: Exception do
      WriteLn('   Correctly rejected: ', E.Message);
  end;

  // Valid insert
  Connection.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Good Product'', ''Test'', 10, 5)');
  WriteLn('   Valid insert succeeded');
  WriteLn('');
end;

{ Creates triggers that automatically set created_at on insert and updated_at on modification. }
procedure DemoAutoTimestampTrigger;
var
  DS: TDataSet;
begin
  WriteLn('6. Auto-Timestamp Trigger');
  WriteLn('   ----------------------');

  // Create table with timestamps
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS articles (' +
    '  id INTEGER PRIMARY KEY,' +
    '  title TEXT,' +
    '  content TEXT,' +
    '  created_at TEXT,' +
    '  updated_at TEXT' +
    ')');

  // Trigger for created_at
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS set_created_at ' +
    'AFTER INSERT ON articles ' +
    'BEGIN ' +
    '  UPDATE articles SET created_at = datetime(''now''), updated_at = datetime(''now'') WHERE id = NEW.id; ' +
    'END');

  // Trigger for updated_at
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS set_updated_at ' +
    'AFTER UPDATE ON articles ' +
    'WHEN OLD.updated_at = NEW.updated_at ' +
    'BEGIN ' +
    '  UPDATE articles SET updated_at = datetime(''now'') WHERE id = NEW.id; ' +
    'END');

  WriteLn('   Created timestamp triggers');

  // Insert article
  Connection.ExecuteNonQuery('INSERT INTO articles (title, content) VALUES (''Hello World'', ''My first article'')');

  DS := Connection.ExecuteQuery('SELECT title, created_at, updated_at FROM articles');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Article: %s', [DS.FieldByName('title').AsString]));
      WriteLn(Format('   Created: %s', [DS.FieldByName('created_at').AsString]));
      WriteLn(Format('   Updated: %s', [DS.FieldByName('updated_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Queries sqlite_master to enumerate all views and triggers defined in the database. }
procedure DemoListViewsAndTriggers;
var
  DS: TDataSet;
begin
  WriteLn('7. List Views and Triggers');
  WriteLn('   -----------------------');

  // List views
  DS := Connection.ExecuteQuery('SELECT name FROM sqlite_master WHERE type = ''view''');
  try
    WriteLn('   Views:');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // List triggers
  DS := Connection.ExecuteQuery('SELECT name, tbl_name FROM sqlite_master WHERE type = ''trigger''');
  try
    WriteLn('   Triggers:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (on %s)',
        [DS.FieldByName('name').AsString, DS.FieldByName('tbl_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deletes the database file created by this example. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 19: Views and Triggers ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example19.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      DemoSimpleView;
      DemoViewWithJoin;
      DemoViewWithAggregation;
      DemoAuditTrigger;
      DemoBeforeInsertTrigger;
      DemoAutoTimestampTrigger;
      DemoListViewsAndTriggers;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
