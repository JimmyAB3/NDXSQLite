{===============================================================================
  NDXSQLite Example 30 - Upsert Operations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - INSERT OR REPLACE
  - INSERT OR IGNORE
  - ON CONFLICT DO UPDATE (true upsert)
  - ON CONFLICT DO NOTHING
  - Conflict targets (specific columns)
  - EXCLUDED table reference

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Upsert;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Displays table information. }
procedure ShowTable(const ATableName: string);
var
  DS: TDataSet;
  I: Integer;
begin
  DS := Connection.ExecuteQuery('SELECT * FROM ' + ATableName);
  try
    WriteLn('   Current data in ', ATableName, ':');
    while not DS.EOF do
    begin
      Write('     ');
      for I := 0 to DS.FieldCount - 1 do
      begin
        if I > 0 then Write(', ');
        Write(DS.Fields[I].FieldName, '=', DS.Fields[I].AsString);
      end;
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Creates a products table, inserts rows, then uses INSERT OR REPLACE to
  overwrite an existing row by primary key, showing its delete-then-insert behavior. }
procedure DemoInsertOrReplace;
begin
  WriteLn('1. INSERT OR REPLACE');
  WriteLn('   ------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  price REAL,' +
    '  stock INTEGER DEFAULT 0' +
    ')');

  // Initial inserts
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (1, ''Laptop'', 999.99, 10)');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (2, ''Mouse'', 29.99, 50)');
  ShowTable('products');

  WriteLn('');
  WriteLn('   INSERT OR REPLACE with existing id=1:');

  // This DELETES the old row and inserts a new one
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO products (id, name, price, stock) ' +
    'VALUES (1, ''Gaming Laptop'', 1499.99, 5)');

  ShowTable('products');

  WriteLn('   WARNING: INSERT OR REPLACE deletes then inserts!');
  WriteLn('   - Triggers ON DELETE fire');
  WriteLn('   - Auto-increment changes');
  WriteLn('   - Unspecified columns get defaults, not old values');
  WriteLn('');
end;

{ Creates a users table with a UNIQUE email column and shows that INSERT OR
  IGNORE silently skips duplicate entries without raising an error. }
procedure DemoInsertOrIgnore;
begin
  WriteLn('2. INSERT OR IGNORE');
  WriteLn('   -----------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  email TEXT UNIQUE,' +
    '  name TEXT' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO users VALUES (1, ''alice@test.com'', ''Alice'')');
  Connection.ExecuteNonQuery('INSERT INTO users VALUES (2, ''bob@test.com'', ''Bob'')');
  ShowTable('users');

  WriteLn('');
  WriteLn('   INSERT OR IGNORE with duplicate email:');

  // Silently ignored - no error, no insert
  Connection.ExecuteNonQuery(
    'INSERT OR IGNORE INTO users (email, name) VALUES (''alice@test.com'', ''Alice Updated'')');

  ShowTable('users');
  WriteLn('   Note: Row was silently ignored, no error raised.');

  // New email works
  Connection.ExecuteNonQuery(
    'INSERT OR IGNORE INTO users (email, name) VALUES (''charlie@test.com'', ''Charlie'')');
  WriteLn('');
  WriteLn('   After inserting new email:');
  ShowTable('users');

  WriteLn('');
end;

{ Creates an inventory table and uses ON CONFLICT(sku) DO UPDATE to add quantity
  to existing rows or insert new ones, referencing excluded.column values. }
procedure DemoOnConflictDoUpdate;
begin
  WriteLn('3. ON CONFLICT DO UPDATE (true upsert)');
  WriteLn('   ------------------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE inventory (' +
    '  sku TEXT PRIMARY KEY,' +
    '  name TEXT,' +
    '  quantity INTEGER DEFAULT 0,' +
    '  last_updated TEXT' +
    ')');

  Connection.ExecuteNonQuery(
    'INSERT INTO inventory VALUES (''SKU001'', ''Widget'', 100, datetime(''now''))');
  Connection.ExecuteNonQuery(
    'INSERT INTO inventory VALUES (''SKU002'', ''Gadget'', 50, datetime(''now''))');
  ShowTable('inventory');

  WriteLn('');
  WriteLn('   Upsert existing SKU001 (add 25 to quantity):');

  // True upsert - update if exists, insert if not
  Connection.ExecuteNonQuery(
    'INSERT INTO inventory (sku, name, quantity, last_updated) ' +
    'VALUES (''SKU001'', ''Widget'', 25, datetime(''now'')) ' +
    'ON CONFLICT(sku) DO UPDATE SET ' +
    '  quantity = quantity + excluded.quantity,' +
    '  last_updated = excluded.last_updated');

  ShowTable('inventory');

  WriteLn('');
  WriteLn('   Upsert new SKU003:');

  Connection.ExecuteNonQuery(
    'INSERT INTO inventory (sku, name, quantity, last_updated) ' +
    'VALUES (''SKU003'', ''Gizmo'', 75, datetime(''now'')) ' +
    'ON CONFLICT(sku) DO UPDATE SET ' +
    '  quantity = quantity + excluded.quantity,' +
    '  last_updated = excluded.last_updated');

  ShowTable('inventory');

  WriteLn('   NOTE: excluded.column refers to the values that would have been inserted.');
  WriteLn('');
end;

{ Creates a settings table and uses ON CONFLICT DO NOTHING to silently skip
  inserts when a primary key conflict occurs. }
procedure DemoOnConflictDoNothing;
begin
  WriteLn('4. ON CONFLICT DO NOTHING');
  WriteLn('   -----------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE settings (' +
    '  key TEXT PRIMARY KEY,' +
    '  value TEXT' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO settings VALUES (''theme'', ''dark'')');
  Connection.ExecuteNonQuery('INSERT INTO settings VALUES (''language'', ''en'')');
  ShowTable('settings');

  WriteLn('');
  WriteLn('   Insert with ON CONFLICT DO NOTHING:');

  // Same as INSERT OR IGNORE but more explicit
  Connection.ExecuteNonQuery(
    'INSERT INTO settings (key, value) VALUES (''theme'', ''light'') ' +
    'ON CONFLICT DO NOTHING');

  ShowTable('settings');
  WriteLn('   Row ignored silently (same as INSERT OR IGNORE).');
  WriteLn('');
end;

{ Creates an orders table with a composite UNIQUE constraint and uses
  ON CONFLICT(customer_id, product_id) to increment quantity on conflict. }
procedure DemoConflictTarget;
begin
  WriteLn('5. Conflict target (specific columns)');
  WriteLn('   -----------------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER,' +
    '  product_id INTEGER,' +
    '  quantity INTEGER,' +
    '  UNIQUE(customer_id, product_id)' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (1, 100, 1, 2)');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (2, 100, 2, 1)');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (3, 101, 1, 3)');
  ShowTable('orders');

  WriteLn('');
  WriteLn('   Upsert on composite unique (customer_id, product_id):');

  // Conflict on composite unique constraint
  Connection.ExecuteNonQuery(
    'INSERT INTO orders (customer_id, product_id, quantity) VALUES (100, 1, 5) ' +
    'ON CONFLICT(customer_id, product_id) DO UPDATE SET ' +
    '  quantity = quantity + excluded.quantity');

  ShowTable('orders');
  WriteLn('   Customer 100 + Product 1: quantity increased from 2 to 7');
  WriteLn('');
end;

{ Creates a prices table and uses ON CONFLICT DO UPDATE with a WHERE clause
  that only updates the price when it meets the minimum price threshold. }
procedure DemoConditionalUpdate;
begin
  WriteLn('6. Conditional upsert (with WHERE)');
  WriteLn('   --------------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE prices (' +
    '  product_id INTEGER PRIMARY KEY,' +
    '  price REAL,' +
    '  min_price REAL' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO prices VALUES (1, 100.00, 80.00)');
  Connection.ExecuteNonQuery('INSERT INTO prices VALUES (2, 50.00, 40.00)');
  ShowTable('prices');

  WriteLn('');
  WriteLn('   Update only if new price >= min_price:');

  // Only update if condition is met
  Connection.ExecuteNonQuery(
    'INSERT INTO prices (product_id, price, min_price) VALUES (1, 75.00, 80.00) ' +
    'ON CONFLICT(product_id) DO UPDATE SET price = excluded.price ' +
    'WHERE excluded.price >= prices.min_price');

  ShowTable('prices');
  WriteLn('   Product 1: NOT updated (75 < 80 min_price)');

  WriteLn('');
  WriteLn('   Try with valid price:');

  Connection.ExecuteNonQuery(
    'INSERT INTO prices (product_id, price, min_price) VALUES (1, 85.00, 80.00) ' +
    'ON CONFLICT(product_id) DO UPDATE SET price = excluded.price ' +
    'WHERE excluded.price >= prices.min_price');

  ShowTable('prices');
  WriteLn('   Product 1: Updated to 85 (>= 80 min_price)');
  WriteLn('');
end;

{ Performs 1000 upserts within a transaction, incrementing 10 named counters
  on conflict to show efficient bulk upsert throughput. }
procedure DemoBulkUpsert;
var
  I: Integer;
  DS: TDataSet;
begin
  WriteLn('7. Bulk upsert pattern');
  WriteLn('   --------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE counters (' +
    '  name TEXT PRIMARY KEY,' +
    '  count INTEGER DEFAULT 0' +
    ')');

  WriteLn('   Upserting 1000 counter increments...');

  Connection.BeginTransaction;
  for I := 1 to 1000 do
  begin
    Connection.ExecuteNonQuery(
      'INSERT INTO counters (name, count) VALUES (?, 1) ' +
      'ON CONFLICT(name) DO UPDATE SET count = count + 1',
      [Format('counter_%d', [I mod 10])]);  // Only 10 unique counters
  end;
  Connection.Commit;

  DS := Connection.ExecuteQuery('SELECT * FROM counters ORDER BY name');
  try
    WriteLn('   Results (each counter incremented ~100 times):');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('name').AsString, ' = ',
              DS.FieldByName('count').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates the RETURNING clause combined with upsert operations. }
procedure DemoReturning;
var
  DS: TDataSet;
begin
  WriteLn('8. RETURNING clause with upsert');
  WriteLn('   -----------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE tokens (' +
    '  user_id INTEGER PRIMARY KEY,' +
    '  token TEXT,' +
    '  created_at TEXT' +
    ')');

  WriteLn('   Insert new token with RETURNING:');

  DS := Connection.ExecuteQuery(
    'INSERT INTO tokens (user_id, token, created_at) ' +
    'VALUES (1, ''abc123'', datetime(''now'')) ' +
    'ON CONFLICT(user_id) DO UPDATE SET ' +
    '  token = excluded.token, created_at = excluded.created_at ' +
    'RETURNING user_id, token, created_at');
  try
    if not DS.IsEmpty then
      WriteLn('     Returned: user_id=', DS.FieldByName('user_id').AsInteger,
              ', token=', DS.FieldByName('token').AsString);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Update existing with RETURNING:');

  DS := Connection.ExecuteQuery(
    'INSERT INTO tokens (user_id, token, created_at) ' +
    'VALUES (1, ''xyz789'', datetime(''now'')) ' +
    'ON CONFLICT(user_id) DO UPDATE SET ' +
    '  token = excluded.token, created_at = excluded.created_at ' +
    'RETURNING user_id, token, created_at');
  try
    if not DS.IsEmpty then
      WriteLn('     Returned: user_id=', DS.FieldByName('user_id').AsInteger,
              ', token=', DS.FieldByName('token').AsString);
  finally
    DS.Free;
  end;

  WriteLn('   RETURNING works with both insert and update cases.');
  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 30: Upsert Operations ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example30.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      DemoInsertOrReplace;
      DemoInsertOrIgnore;
      DemoOnConflictDoUpdate;
      DemoOnConflictDoNothing;
      DemoConflictTarget;
      DemoConditionalUpdate;
      DemoBulkUpsert;
      DemoReturning;

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
