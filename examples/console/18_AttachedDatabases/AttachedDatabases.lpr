{===============================================================================
  NDXSQLite Example 18 - Attached Databases
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - ATTACH DATABASE to connect multiple databases
  - Cross-database queries
  - Data migration between databases
  - DETACH DATABASE

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program AttachedDatabases;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  MainDBPath, SecondDBPath, ThirdDBPath: string;

{ Initializes all databases with required tables and data. }
procedure SetupDatabases;
var
  Conn2: TNDXSQLiteConnection;
begin
  // Create main database with customers
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO customers (name, email) VALUES (''Alice'', ''alice@example.com'')');
  Connection.ExecuteNonQuery('INSERT INTO customers (name, email) VALUES (''Bob'', ''bob@example.com'')');
  Connection.ExecuteNonQuery('INSERT INTO customers (name, email) VALUES (''Charlie'', ''charlie@example.com'')');

  // Create second database with orders
  Conn2 := TNDXSQLiteConnection.Create(SecondDBPath);
  try
    Conn2.Open;
    Conn2.ExecuteNonQuery(
      'CREATE TABLE IF NOT EXISTS orders (' +
      '  id INTEGER PRIMARY KEY,' +
      '  customer_id INTEGER,' +
      '  product TEXT,' +
      '  amount REAL' +
      ')');

    Conn2.ExecuteNonQuery('INSERT INTO orders (customer_id, product, amount) VALUES (1, ''Laptop'', 999.99)');
    Conn2.ExecuteNonQuery('INSERT INTO orders (customer_id, product, amount) VALUES (1, ''Mouse'', 29.99)');
    Conn2.ExecuteNonQuery('INSERT INTO orders (customer_id, product, amount) VALUES (2, ''Keyboard'', 79.99)');
    Conn2.ExecuteNonQuery('INSERT INTO orders (customer_id, product, amount) VALUES (3, ''Monitor'', 299.99)');
    Conn2.Close;
  finally
    Conn2.Free;
  end;
end;

{ Attaches a second database file and lists all currently connected databases. }
procedure DemoAttachDatabase;
var
  DS: TDataSet;
begin
  WriteLn('1. ATTACH DATABASE');
  WriteLn('   ---------------');

  // Attach the orders database
  Connection.ExecuteNonQuery(Format('ATTACH DATABASE ''%s'' AS orders_db', [SecondDBPath]));
  WriteLn('   Attached: ', SecondDBPath, ' as "orders_db"');

  // List attached databases
  DS := Connection.ExecuteQuery('PRAGMA database_list');
  try
    WriteLn('   Attached databases:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %s', [DS.FieldByName('name').AsString, DS.FieldByName('file').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Joins the customers table from the main database with orders from the attached database. }
procedure DemoCrossDatabaseQuery;
var
  DS: TDataSet;
begin
  WriteLn('2. Cross-Database Query');
  WriteLn('   --------------------');

  // Query joining tables from different databases
  WriteLn('   Query: SELECT customers with their orders');
  DS := Connection.ExecuteQuery(
    'SELECT c.name, o.product, o.amount ' +
    'FROM main.customers c ' +
    'JOIN orders_db.orders o ON c.id = o.customer_id ' +
    'ORDER BY c.name, o.product');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %s ($%.2f)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('product').AsString,
         DS.FieldByName('amount').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Computes order counts and totals per customer by aggregating across attached databases. }
procedure DemoAggregateAcrossDatabases;
var
  DS: TDataSet;
begin
  WriteLn('3. Aggregate Across Databases');
  WriteLn('   --------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT c.name, COUNT(o.id) as order_count, SUM(o.amount) as total ' +
    'FROM main.customers c ' +
    'LEFT JOIN orders_db.orders o ON c.id = o.customer_id ' +
    'GROUP BY c.id, c.name ' +
    'ORDER BY total DESC');
  try
    WriteLn('   Customer order totals:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d orders, $%.2f total',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('order_count').AsInteger,
         DS.FieldByName('total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Copies order records into a newly attached archive database with customer name denormalization. }
procedure DemoDataMigration;
var
  Count: Integer;
begin
  WriteLn('4. Data Migration Between Databases');
  WriteLn('   ---------------------------------');

  // Create archive database
  Connection.ExecuteNonQuery(Format('ATTACH DATABASE ''%s'' AS archive_db', [ThirdDBPath]));
  WriteLn('   Attached archive database');

  // Create archive table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS archive_db.archived_orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_name TEXT,' +
    '  product TEXT,' +
    '  amount REAL,' +
    '  archived_at TEXT' +
    ')');

  // Copy data from orders_db to archive_db with transformation
  Connection.ExecuteNonQuery(
    'INSERT INTO archive_db.archived_orders (customer_name, product, amount, archived_at) ' +
    'SELECT c.name, o.product, o.amount, datetime(''now'') ' +
    'FROM orders_db.orders o ' +
    'JOIN main.customers c ON o.customer_id = c.id');

  Count := Connection.ExecuteScalar('SELECT COUNT(*) FROM archive_db.archived_orders');
  WriteLn(Format('   Migrated %d records to archive', [Count]));
  WriteLn('');
end;

{ Inserts a new record directly into a table in the attached orders database. }
procedure DemoInsertAcrossDatabases;
begin
  WriteLn('5. Insert Across Databases');
  WriteLn('   -----------------------');

  // Insert into attached database
  Connection.ExecuteNonQuery(
    'INSERT INTO orders_db.orders (customer_id, product, amount) ' +
    'VALUES (2, ''Webcam'', 49.99)');
  WriteLn('   Inserted new order into orders_db');

  // Verify
  WriteLn('   Bob''s orders now: ', Integer(Connection.ExecuteScalar(
    'SELECT COUNT(*) FROM orders_db.orders WHERE customer_id = 2')));
  WriteLn('');
end;

{ Detaches the attached databases and verifies only the main database remains. }
procedure DemoDetachDatabase;
var
  DS: TDataSet;
begin
  WriteLn('6. DETACH DATABASE');
  WriteLn('   ---------------');

  // Detach archive database
  Connection.ExecuteNonQuery('DETACH DATABASE archive_db');
  WriteLn('   Detached: archive_db');

  // Detach orders database
  Connection.ExecuteNonQuery('DETACH DATABASE orders_db');
  WriteLn('   Detached: orders_db');

  // Verify
  DS := Connection.ExecuteQuery('PRAGMA database_list');
  try
    WriteLn('   Remaining databases:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s', [DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Prints recommended practices for working with multiple attached databases. }
procedure DemoBestPractices;
begin
  WriteLn('7. Best Practices');
  WriteLn('   --------------');
  WriteLn('   - Use schema prefix (main., db_alias.) for clarity');
  WriteLn('   - DETACH databases when no longer needed');
  WriteLn('   - Be aware of transaction scope across databases');
  WriteLn('   - Use ATTACH for data migration between databases');
  WriteLn('   - Maximum 10 attached databases by default');
  WriteLn('   - Attached databases share the same connection');
  WriteLn('');
end;

{ Deletes all database files created by this example. }
procedure Cleanup;
begin
  if FileExists(MainDBPath) then DeleteFile(MainDBPath);
  if FileExists(SecondDBPath) then DeleteFile(SecondDBPath);
  if FileExists(ThirdDBPath) then DeleteFile(ThirdDBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 18: Attached Databases ===');
  WriteLn('');

  MainDBPath := ExtractFilePath(ParamStr(0)) + 'example18_main.db';
  SecondDBPath := ExtractFilePath(ParamStr(0)) + 'example18_orders.db';
  ThirdDBPath := ExtractFilePath(ParamStr(0)) + 'example18_archive.db';

  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := MainDBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupDatabases;
      DemoAttachDatabase;
      DemoCrossDatabaseQuery;
      DemoAggregateAcrossDatabases;
      DemoDataMigration;
      DemoInsertAcrossDatabases;
      DemoDetachDatabase;
      DemoBestPractices;

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
