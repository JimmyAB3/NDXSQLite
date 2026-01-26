{===============================================================================
  NDXSQLite Example 97 - RETURNING Clause
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - INSERT...RETURNING for generated values
  - UPDATE...RETURNING for affected rows
  - DELETE...RETURNING for audit logging
  - RETURNING with computed expressions
  - Bulk operations with RETURNING

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ReturningClause;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // Users table
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT NOT NULL UNIQUE,' +
    '  email TEXT NOT NULL,' +
    '  role TEXT NOT NULL DEFAULT ''user'',' +
    '  status TEXT NOT NULL DEFAULT ''active'',' +
    '  score INTEGER NOT NULL DEFAULT 0,' +
    '  created_at TEXT NOT NULL DEFAULT (datetime(''now''))' +
    ')');

  // Products table
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL DEFAULT 0,' +
    '  is_active INTEGER NOT NULL DEFAULT 1,' +
    '  updated_at TEXT' +
    ')');

  // Orders table
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  product_id INTEGER NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  unit_price REAL NOT NULL,' +
    '  total REAL NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  created_at TEXT NOT NULL DEFAULT (datetime(''now''))' +
    ')');

  // Audit log table
  Conn.ExecuteNonQuery(
    'CREATE TABLE audit_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  table_name TEXT NOT NULL,' +
    '  operation TEXT NOT NULL,' +
    '  record_id INTEGER,' +
    '  details TEXT,' +
    '  created_at TEXT NOT NULL DEFAULT (datetime(''now''))' +
    ')');

  // Archive table for deleted records
  Conn.ExecuteNonQuery(
    'CREATE TABLE users_archive (' +
    '  id INTEGER,' +
    '  username TEXT,' +
    '  email TEXT,' +
    '  role TEXT,' +
    '  archived_at TEXT NOT NULL DEFAULT (datetime(''now''))' +
    ')');
end;

{ Inserts five users and retrieves their auto-generated IDs, usernames, roles, and default values using RETURNING. }
procedure DemoInsertReturning;
begin
  WriteLn('=== 1. INSERT ... RETURNING - Get Generated IDs ===');
  WriteLn('');
  WriteLn('   Inserting users and getting auto-generated IDs:');

  DS := Conn.ExecuteQuery(
    'INSERT INTO users (username, email, role) VALUES ' +
    '(''alice'', ''alice@example.com'', ''admin'') ' +
    'RETURNING id, username, email, role');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Inserted: id=', DS.FieldByName('id').AsInteger,
        ', user=', DS.FieldByName('username').AsString,
        ', email=', DS.FieldByName('email').AsString,
        ', role=', DS.FieldByName('role').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'INSERT INTO users (username, email, role) VALUES ' +
    '(''bob'', ''bob@example.com'', ''user'') ' +
    'RETURNING id, username, role');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Inserted: id=', DS.FieldByName('id').AsInteger,
        ', user=', DS.FieldByName('username').AsString,
        ', role=', DS.FieldByName('role').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'INSERT INTO users (username, email, role) VALUES ' +
    '(''charlie'', ''charlie@example.com'', ''moderator'') ' +
    'RETURNING id, username, role');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Inserted: id=', DS.FieldByName('id').AsInteger,
        ', user=', DS.FieldByName('username').AsString,
        ', role=', DS.FieldByName('role').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'INSERT INTO users (username, email) VALUES ' +
    '(''diana'', ''diana@example.com'') ' +
    'RETURNING id, username, role, status');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Inserted: id=', DS.FieldByName('id').AsInteger,
        ', user=', DS.FieldByName('username').AsString,
        ', role=', DS.FieldByName('role').AsString,
        ' (default), status=', DS.FieldByName('status').AsString, ' (default)');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'INSERT INTO users (username, email) VALUES ' +
    '(''eve'', ''eve@example.com'') ' +
    'RETURNING id, username');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Inserted: id=', DS.FieldByName('id').AsInteger,
        ', user=', DS.FieldByName('username').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Inserts products and returns computed values like price with tax, inventory value, and uppercase category. }
procedure DemoInsertReturningExpressions;
begin
  WriteLn('=== 2. INSERT ... RETURNING with Expressions ===');
  WriteLn('');
  WriteLn('   Inserting products with computed return values:');

  DS := Conn.ExecuteQuery(
    'INSERT INTO products (name, category, price, stock) VALUES ' +
    '(''Laptop Pro'', ''Electronics'', 1299.99, 25) ' +
    'RETURNING id, name, price, stock, ROUND(price * 1.2, 2) as price_with_tax');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('name').AsString,
        ': $', DS.FieldByName('price').AsFloat:0:2,
        ' (with tax: $', DS.FieldByName('price_with_tax').AsFloat:0:2, ')',
        ', stock=', DS.FieldByName('stock').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'INSERT INTO products (name, category, price, stock) VALUES ' +
    '(''Wireless Mouse'', ''Accessories'', 49.99, 100) ' +
    'RETURNING id, name, category, price * stock as inventory_value');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('name').AsString,
        ' (', DS.FieldByName('category').AsString, ')',
        ', inventory value: $', DS.FieldByName('inventory_value').AsFloat:0:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'INSERT INTO products (name, category, price, stock) VALUES ' +
    '(''USB-C Cable'', ''Accessories'', 19.99, 200) ' +
    'RETURNING id, name, upper(category) as cat_upper, price, stock');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('name').AsString,
        ' [', DS.FieldByName('cat_upper').AsString, ']',
        ', $', DS.FieldByName('price').AsFloat:0:2,
        ', stock=', DS.FieldByName('stock').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Add more products for later demos
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Headphones'', ''Electronics'', 89.99, 50)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Keyboard'', ''Accessories'', 79.99, 75)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Monitor'', ''Electronics'', 399.99, 15)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Webcam'', ''Electronics'', 69.99, 40)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock) VALUES (''Desk Lamp'', ''Office'', 34.99, 60)');

  WriteLn('');
end;

{ Updates user scores and product prices, returning the new values and computed old/new price comparisons. }
procedure DemoUpdateReturning;
begin
  WriteLn('=== 3. UPDATE ... RETURNING - See Modified Rows ===');
  WriteLn('');
  WriteLn('   Updating user scores and seeing results:');

  DS := Conn.ExecuteQuery(
    'UPDATE users SET score = score + 100 ' +
    'WHERE role = ''admin'' ' +
    'RETURNING id, username, score');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Updated: id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('username').AsString,
        ', new score=', DS.FieldByName('score').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Bulk score update with RETURNING:');

  DS := Conn.ExecuteQuery(
    'UPDATE users SET score = score + 50 ' +
    'WHERE role IN (''user'', ''moderator'') ' +
    'RETURNING id, username, role, score');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('username').AsString:10,
        ' (', DS.FieldByName('role').AsString, ')',
        ', score=', DS.FieldByName('score').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Price adjustment with old/new comparison:');

  // First show old prices
  WriteLn('   Before: Electronics products');
  DS := Conn.ExecuteQuery('SELECT id, name, price FROM products WHERE category = ''Electronics''');
  try
    while not DS.EOF do
    begin
      WriteLn('     id=', DS.FieldByName('id').AsInteger,
        ' ', DS.FieldByName('name').AsString:14,
        ' $', DS.FieldByName('price').AsFloat:0:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Applying 10% price increase to Electronics:');
  DS := Conn.ExecuteQuery(
    'UPDATE products SET price = ROUND(price * 1.10, 2),' +
    '  updated_at = datetime(''now'') ' +
    'WHERE category = ''Electronics'' ' +
    'RETURNING id, name, price as new_price, ROUND(price / 1.10, 2) as was_price');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> id=', DS.FieldByName('id').AsInteger,
        ' ', DS.FieldByName('name').AsString:14,
        ': was $', DS.FieldByName('was_price').AsFloat:0:2,
        ' -> now $', DS.FieldByName('new_price').AsFloat:0:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deletes inactive and banned users while capturing and displaying their data through RETURNING. }
procedure DemoDeleteReturning;
begin
  WriteLn('=== 4. DELETE ... RETURNING - Capture Deleted Rows ===');
  WriteLn('');

  // Add some users to delete
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, status) VALUES (''temp1'', ''temp1@test.com'', ''inactive'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, status) VALUES (''temp2'', ''temp2@test.com'', ''inactive'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, status) VALUES (''temp3'', ''temp3@test.com'', ''banned'')');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users');
  try
    WriteLn('   Users before cleanup: ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('   Deleting inactive/banned users (capturing deleted data):');

  DS := Conn.ExecuteQuery(
    'DELETE FROM users ' +
    'WHERE status IN (''inactive'', ''banned'') ' +
    'RETURNING id, username, email, status');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Deleted: id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('username').AsString,
        ' (', DS.FieldByName('email').AsString, ')',
        ', was: ', DS.FieldByName('status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users');
  try
    WriteLn('   Users after cleanup: ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Uses RETURNING to capture inserted/updated record IDs and details, then logs them to an audit_log table. }
procedure DemoReturningForAudit;
begin
  WriteLn('=== 5. RETURNING for Audit Trail ===');
  WriteLn('');
  WriteLn('   Using RETURNING to populate audit log:');

  // Insert a product and log it
  DS := Conn.ExecuteQuery(
    'INSERT INTO products (name, category, price, stock) VALUES ' +
    '(''Smart Watch'', ''Electronics'', 249.99, 30) ' +
    'RETURNING id, name');
  try
    if not DS.EOF then
    begin
      Conn.ExecuteNonQuery(
        'INSERT INTO audit_log (table_name, operation, record_id, details) VALUES ' +
        '(''products'', ''INSERT'', ' + DS.FieldByName('id').AsString + ', ' +
        '''Created product: ' + DS.FieldByName('name').AsString + ''')');
      WriteLn('   -> Created product id=', DS.FieldByName('id').AsInteger,
        ' (', DS.FieldByName('name').AsString, ') - logged');
    end;
  finally
    DS.Free;
  end;

  // Update and log
  DS := Conn.ExecuteQuery(
    'UPDATE products SET stock = stock - 5 ' +
    'WHERE name = ''Smart Watch'' ' +
    'RETURNING id, name, stock');
  try
    if not DS.EOF then
    begin
      Conn.ExecuteNonQuery(
        'INSERT INTO audit_log (table_name, operation, record_id, details) VALUES ' +
        '(''products'', ''UPDATE'', ' + DS.FieldByName('id').AsString + ', ' +
        '''Stock reduced to ' + DS.FieldByName('stock').AsString + ''')');
      WriteLn('   -> Updated product id=', DS.FieldByName('id').AsInteger,
        ', stock now=', DS.FieldByName('stock').AsInteger, ' - logged');
    end;
  finally
    DS.Free;
  end;

  // Show audit log
  WriteLn('');
  WriteLn('   Audit log entries:');
  DS := Conn.ExecuteQuery('SELECT * FROM audit_log ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('id').AsInteger, '] ',
        DS.FieldByName('table_name').AsString, '.',
        DS.FieldByName('operation').AsString,
        ' (record #', DS.FieldByName('record_id').AsInteger, '): ',
        DS.FieldByName('details').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deducts stock via UPDATE RETURNING to verify remaining inventory, rejecting orders when stock is insufficient. }
procedure DemoReturningWithStockManagement;
begin
  WriteLn('=== 6. RETURNING for Stock Management ===');
  WriteLn('');
  WriteLn('   Processing orders with stock deduction:');

  // Create orders and reduce stock using RETURNING
  DS := Conn.ExecuteQuery(
    'UPDATE products SET stock = stock - 2 ' +
    'WHERE name = ''Laptop Pro'' AND stock >= 2 ' +
    'RETURNING id, name, stock as remaining_stock');
  try
    if not DS.EOF then
    begin
      WriteLn('   -> Order for Laptop Pro (qty: 2)');
      WriteLn('      Remaining stock: ', DS.FieldByName('remaining_stock').AsInteger);

      // Create the order
      Conn.ExecuteNonQuery(
        'INSERT INTO orders (user_id, product_id, quantity, unit_price, total) VALUES ' +
        '(1, ' + DS.FieldByName('id').AsString + ', 2, 1429.99, 2859.98)');
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'UPDATE products SET stock = stock - 3 ' +
    'WHERE name = ''Wireless Mouse'' AND stock >= 3 ' +
    'RETURNING id, name, stock as remaining_stock');
  try
    if not DS.EOF then
    begin
      WriteLn('   -> Order for Wireless Mouse (qty: 3)');
      WriteLn('      Remaining stock: ', DS.FieldByName('remaining_stock').AsInteger);

      Conn.ExecuteNonQuery(
        'INSERT INTO orders (user_id, product_id, quantity, unit_price, total) VALUES ' +
        '(2, ' + DS.FieldByName('id').AsString + ', 3, 49.99, 149.97)');
    end;
  finally
    DS.Free;
  end;

  // Try to order more than available
  WriteLn('');
  WriteLn('   Attempting to order 1000 Keyboards (insufficient stock):');
  DS := Conn.ExecuteQuery(
    'UPDATE products SET stock = stock - 1000 ' +
    'WHERE name = ''Keyboard'' AND stock >= 1000 ' +
    'RETURNING id, name, stock');
  try
    if DS.EOF then
      WriteLn('   -> Order REJECTED: insufficient stock (no rows returned)')
    else
      WriteLn('   -> Order processed (stock=', DS.FieldByName('stock').AsInteger, ')');
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Current orders:');
  DS := Conn.ExecuteQuery(
    'SELECT o.id, o.user_id, p.name, o.quantity, o.total ' +
    'FROM orders o JOIN products p ON o.product_id = p.id ' +
    'ORDER BY o.id');
  try
    while not DS.EOF do
    begin
      WriteLn('   Order #', DS.FieldByName('id').AsInteger,
        ': user=', DS.FieldByName('user_id').AsInteger,
        ', ', DS.FieldByName('name').AsString,
        ' x', DS.FieldByName('quantity').AsInteger,
        ' = $', DS.FieldByName('total').AsFloat:0:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Shows INSERT and UPDATE with RETURNING * to retrieve all columns including auto-generated and default values. }
procedure DemoReturningAll;
begin
  WriteLn('=== 7. RETURNING * - Return All Columns ===');
  WriteLn('');
  WriteLn('   INSERT with RETURNING * (all columns including defaults):');

  DS := Conn.ExecuteQuery(
    'INSERT INTO users (username, email) VALUES ' +
    '(''frank'', ''frank@example.com'') ' +
    'RETURNING *');
  try
    if not DS.EOF then
    begin
      WriteLn('   -> id=', DS.FieldByName('id').AsInteger,
        ', username=', DS.FieldByName('username').AsString,
        ', email=', DS.FieldByName('email').AsString,
        ', role=', DS.FieldByName('role').AsString,
        ', status=', DS.FieldByName('status').AsString,
        ', score=', DS.FieldByName('score').AsInteger);
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   UPDATE with RETURNING * :');

  DS := Conn.ExecuteQuery(
    'UPDATE users SET role = ''admin'', score = 200 ' +
    'WHERE username = ''frank'' ' +
    'RETURNING *');
  try
    if not DS.EOF then
    begin
      WriteLn('   -> id=', DS.FieldByName('id').AsInteger,
        ', username=', DS.FieldByName('username').AsString,
        ', role=', DS.FieldByName('role').AsString,
        ', score=', DS.FieldByName('score').AsInteger);
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deactivates low-stock products then reactivates them with stock replenishment, returning affected rows each time. }
procedure DemoBulkOperations;
begin
  WriteLn('=== 8. Bulk Operations with RETURNING ===');
  WriteLn('');
  WriteLn('   Bulk status update - deactivate low-stock products:');

  DS := Conn.ExecuteQuery(
    'UPDATE products SET is_active = 0 ' +
    'WHERE stock < 20 ' +
    'RETURNING id, name, stock, is_active');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Deactivated: id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('name').AsString,
        ' (stock=', DS.FieldByName('stock').AsInteger, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Reactivate with stock replenishment:');

  DS := Conn.ExecuteQuery(
    'UPDATE products SET is_active = 1, stock = stock + 50 ' +
    'WHERE is_active = 0 ' +
    'RETURNING id, name, stock as new_stock');
  try
    while not DS.EOF do
    begin
      WriteLn('   -> Reactivated: id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('name').AsString,
        ', new stock=', DS.FieldByName('new_stock').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deletes a user while capturing the data via RETURNING, then inserts the deleted record into an archive table. }
procedure DemoDeleteArchive;
begin
  WriteLn('=== 9. DELETE ... RETURNING for Archiving ===');
  WriteLn('');
  WriteLn('   Archiving user ''frank'' (delete + capture + insert to archive):');

  // Delete and capture
  DS := Conn.ExecuteQuery(
    'DELETE FROM users WHERE username = ''frank'' ' +
    'RETURNING id, username, email, role');
  try
    if not DS.EOF then
    begin
      WriteLn('   -> Deleted from users: id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('username').AsString);

      // Archive the deleted record
      Conn.ExecuteNonQuery(
        'INSERT INTO users_archive (id, username, email, role) VALUES (' +
        DS.FieldByName('id').AsString + ', ' +
        '''' + DS.FieldByName('username').AsString + ''', ' +
        '''' + DS.FieldByName('email').AsString + ''', ' +
        '''' + DS.FieldByName('role').AsString + ''')');
      WriteLn('   -> Archived to users_archive');
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Archive contents:');
  DS := Conn.ExecuteQuery('SELECT * FROM users_archive');
  try
    while not DS.EOF do
    begin
      WriteLn('   id=', DS.FieldByName('id').AsInteger,
        ', ', DS.FieldByName('username').AsString,
        ', ', DS.FieldByName('email').AsString,
        ', role=', DS.FieldByName('role').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Updates user scores and product timestamps, returning CASE expressions to classify tiers and stock status. }
procedure DemoReturningConditional;
begin
  WriteLn('=== 10. RETURNING with Conditional Expressions ===');
  WriteLn('');
  WriteLn('   Update scores with tier classification:');

  // Give all remaining users some scores
  Conn.ExecuteNonQuery('UPDATE users SET score = 150 WHERE username = ''alice''');
  Conn.ExecuteNonQuery('UPDATE users SET score = 80 WHERE username = ''bob''');
  Conn.ExecuteNonQuery('UPDATE users SET score = 120 WHERE username = ''charlie''');
  Conn.ExecuteNonQuery('UPDATE users SET score = 45 WHERE username = ''diana''');
  Conn.ExecuteNonQuery('UPDATE users SET score = 30 WHERE username = ''eve''');

  DS := Conn.ExecuteQuery(
    'UPDATE users SET score = score + 25 ' +
    'RETURNING id, username, score,' +
    '  CASE ' +
    '    WHEN score >= 150 THEN ''Gold'' ' +
    '    WHEN score >= 100 THEN ''Silver'' ' +
    '    WHEN score >= 50 THEN ''Bronze'' ' +
    '    ELSE ''Basic'' ' +
    '  END as tier');
  try
    WriteLn('   Name        Score   Tier');
    WriteLn('   ' + StringOfChar('-', 30));
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('username').AsString:10,
        '  ', DS.FieldByName('score').AsInteger:5,
        '   ', DS.FieldByName('tier').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Product inventory value check:');

  DS := Conn.ExecuteQuery(
    'UPDATE products SET updated_at = datetime(''now'') ' +
    'WHERE is_active = 1 ' +
    'RETURNING id, name, price, stock, ' +
    '  ROUND(price * stock, 2) as inventory_value,' +
    '  CASE ' +
    '    WHEN stock > 100 THEN ''Overstocked'' ' +
    '    WHEN stock > 30 THEN ''Normal'' ' +
    '    ELSE ''Low'' ' +
    '  END as stock_status');
  try
    WriteLn('   Product          Price     Stock   Value        Status');
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:14,
        '  $', DS.FieldByName('price').AsFloat:7:2,
        '  ', DS.FieldByName('stock').AsInteger:5,
        '  $', DS.FieldByName('inventory_value').AsFloat:10:2,
        '  ', DS.FieldByName('stock_status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Prints final row counts for all tables and lists the RETURNING clause patterns demonstrated. }
procedure PrintSummary;
var
  UserCount, ProductCount, OrderCount, AuditCount, ArchiveCount: Integer;
begin
  WriteLn('=== RETURNING Clause Summary ===');
  WriteLn('');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users');
  try UserCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try ProductCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM orders');
  try OrderCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM audit_log');
  try AuditCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users_archive');
  try ArchiveCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  WriteLn('   Final state: users=', UserCount,
    ', products=', ProductCount,
    ', orders=', OrderCount,
    ', audit_log=', AuditCount,
    ', archive=', ArchiveCount);
  WriteLn('');
  WriteLn('   Patterns demonstrated:');
  WriteLn('   - INSERT ... RETURNING id          (get auto-generated IDs)');
  WriteLn('   - INSERT ... RETURNING *           (get all columns with defaults)');
  WriteLn('   - INSERT ... RETURNING expr        (computed values)');
  WriteLn('   - UPDATE ... RETURNING columns     (see modified values)');
  WriteLn('   - UPDATE ... RETURNING CASE...     (conditional classification)');
  WriteLn('   - DELETE ... RETURNING *           (capture before deletion)');
  WriteLn('   - RETURNING for audit logging      (track changes)');
  WriteLn('   - RETURNING for stock management   (verify after update)');
  WriteLn('   - RETURNING for archiving          (delete + archive pattern)');
  WriteLn('   - RETURNING for validation         (insufficient stock check)');
  WriteLn('');
end;

begin
  WriteLn('Example 97: RETURNING Clause');
  WriteLn(StringOfChar('=', 50));
  WriteLn('');
  WriteLn('Note: RETURNING clause requires SQLite 3.35.0+ (2021-03-12)');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    // Check SQLite version
    DS := Conn.ExecuteQuery('SELECT sqlite_version() as ver');
    try
      WriteLn('SQLite version: ', DS.FieldByName('ver').AsString);
    finally
      DS.Free;
    end;
    WriteLn('');

    SetupDatabase;
    WriteLn('Database setup complete.');
    WriteLn('');

    DemoInsertReturning;
    DemoInsertReturningExpressions;
    DemoUpdateReturning;
    DemoDeleteReturning;
    DemoReturningForAudit;
    DemoReturningWithStockManagement;
    DemoReturningAll;
    DemoBulkOperations;
    DemoDeleteArchive;
    DemoReturningConditional;
    PrintSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('Done.');
end.
