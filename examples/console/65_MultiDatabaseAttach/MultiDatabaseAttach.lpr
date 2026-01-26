{===============================================================================
  NDXSQLite Example 65 - Multi-Database Attach
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Attaching multiple databases to a single connection
  - Cross-database queries with JOINs
  - Copying tables between databases
  - Using in-memory attached databases
  - Detaching databases
  - Managing attached database lifecycle

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program MultiDatabaseAttach;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteattached;

var
  Conn: INDXSQLiteConnection;
  Attached: TNDXSQLiteAttached;
  MainDB, CustomersDB, OrdersDB: string;
  DS: TDataSet;
  DBList: TStringList;
  I: Integer;
  V: Variant;

begin
  WriteLn('=== NDXSQLite Example 65: Multi-Database Attach ===');
  WriteLn;

  MainDB := ExtractFilePath(ParamStr(0)) + 'example65_main.db';
  CustomersDB := ExtractFilePath(ParamStr(0)) + 'example65_customers.db';
  OrdersDB := ExtractFilePath(ParamStr(0)) + 'example65_orders.db';

  // Cleanup
  if FileExists(MainDB) then DeleteFile(MainDB);
  if FileExists(CustomersDB) then DeleteFile(CustomersDB);
  if FileExists(OrdersDB) then DeleteFile(OrdersDB);

  // 1. Create separate databases with data
  WriteLn('1. Creating Separate Databases:');

  // Customers database
  Conn := TNDXSQLiteConnection.Create(CustomersDB);
  Conn.Open;
  Conn.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  email TEXT' +
    ')');
  Conn.ExecuteNonQuery('INSERT INTO customers VALUES (1, ''Alice Smith'', ''alice@example.com'')');
  Conn.ExecuteNonQuery('INSERT INTO customers VALUES (2, ''Bob Johnson'', ''bob@example.com'')');
  Conn.ExecuteNonQuery('INSERT INTO customers VALUES (3, ''Carol White'', ''carol@example.com'')');
  Conn.Close;
  WriteLn('   Created customers.db with 3 customers');

  // Orders database
  Conn := TNDXSQLiteConnection.Create(OrdersDB);
  Conn.Open;
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER,' +
    '  product TEXT,' +
    '  amount REAL' +
    ')');
  Conn.ExecuteNonQuery('INSERT INTO orders VALUES (1, 1, ''Laptop'', 999.99)');
  Conn.ExecuteNonQuery('INSERT INTO orders VALUES (2, 1, ''Mouse'', 29.99)');
  Conn.ExecuteNonQuery('INSERT INTO orders VALUES (3, 2, ''Keyboard'', 79.99)');
  Conn.ExecuteNonQuery('INSERT INTO orders VALUES (4, 3, ''Monitor'', 299.99)');
  Conn.ExecuteNonQuery('INSERT INTO orders VALUES (5, 2, ''Webcam'', 49.99)');
  Conn.Close;
  WriteLn('   Created orders.db with 5 orders');
  WriteLn;

  // 2. Open main database and attach others
  WriteLn('2. Attaching Databases:');
  Conn := TNDXSQLiteConnection.Create(MainDB);
  Conn.Open;

  // Create main table
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  price REAL' +
    ')');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (1, ''Laptop'', 999.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (2, ''Mouse'', 29.99)');

  Attached := TNDXSQLiteAttached.Create(Conn);
  try
    // Attach databases
    Attached.Attach(CustomersDB, 'customers_db');
    WriteLn('   Attached customers_db');

    Attached.Attach(OrdersDB, 'orders_db');
    WriteLn('   Attached orders_db');
    WriteLn;

    // 3. List attached databases
    WriteLn('3. Listing Attached Databases:');
    DBList := Attached.GetAttachedList;
    try
      for I := 0 to DBList.Count - 1 do
        WriteLn('   - ', DBList[I]);
    finally
      DBList.Free;
    end;
    WriteLn;

    // 4. Cross-database queries
    WriteLn('4. Cross-Database Queries:');

    // Query customers from attached database
    DS := Conn.ExecuteQuery('SELECT * FROM customers_db.customers');
    try
      WriteLn('   Customers from attached DB:');
      while not DS.EOF do
      begin
        WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
          DS.FieldByName('name').AsString);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;

    // 5. Cross-database JOIN
    WriteLn('5. Cross-Database JOIN:');
    DS := Conn.ExecuteQuery(
      'SELECT c.name, o.product, o.amount ' +
      'FROM customers_db.customers c ' +
      'JOIN orders_db.orders o ON c.id = o.customer_id ' +
      'ORDER BY c.name, o.amount DESC');
    try
      WriteLn('   Customer Orders:');
      while not DS.EOF do
      begin
        WriteLn(Format('     %s: %s ($%.2f)', [
          DS.FieldByName('name').AsString,
          DS.FieldByName('product').AsString,
          DS.FieldByName('amount').AsFloat
        ]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;

    // 6. Aggregate across databases
    WriteLn('6. Aggregations Across Databases:');
    DS := Conn.ExecuteQuery(
      'SELECT c.name, COUNT(o.id) as order_count, SUM(o.amount) as total ' +
      'FROM customers_db.customers c ' +
      'LEFT JOIN orders_db.orders o ON c.id = o.customer_id ' +
      'GROUP BY c.id ' +
      'ORDER BY total DESC');
    try
      WriteLn('   Customer Totals:');
      while not DS.EOF do
      begin
        WriteLn(Format('     %s: %d orders, $%.2f total', [
          DS.FieldByName('name').AsString,
          DS.FieldByName('order_count').AsInteger,
          DS.FieldByName('total').AsFloat
        ]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;

    // 7. Copy table between databases
    WriteLn('7. Copying Table Between Databases:');
    Attached.CopyTable('customers_db', 'customers', 'main', 'customers_backup', True);
    V := Conn.ExecuteScalar('SELECT COUNT(*) FROM customers_backup');
    WriteLn('   Copied customers to main.customers_backup: ', Integer(V), ' rows');
    WriteLn;

    // 8. Attach in-memory database
    WriteLn('8. Attaching In-Memory Database:');
    Attached.AttachMemory('temp_db');
    WriteLn('   Attached in-memory database as temp_db');

    // Create table in memory DB
    Conn.ExecuteNonQuery(
      'CREATE TABLE temp_db.session_data (' +
      '  key TEXT PRIMARY KEY,' +
      '  value TEXT' +
      ')');
    Conn.ExecuteNonQuery('INSERT INTO temp_db.session_data VALUES (''user'', ''admin'')');
    Conn.ExecuteNonQuery('INSERT INTO temp_db.session_data VALUES (''token'', ''abc123'')');

    V := Conn.ExecuteScalar('SELECT value FROM temp_db.session_data WHERE key = ''user''');
    WriteLn('   Session user: ', VarToStr(V));
    WriteLn;

    // 9. Check if database is attached
    WriteLn('9. Checking Attached Status:');
    WriteLn('   Is customers_db attached? ', Attached.IsAttached('customers_db'));
    WriteLn('   Is unknown_db attached? ', Attached.IsAttached('unknown_db'));
    WriteLn('   Is temp_db attached? ', Attached.IsAttached('temp_db'));
    WriteLn;

    // 10. Get database file paths (using PRAGMA database_list)
    WriteLn('10. Database File Paths:');
    DS := Attached.GetDatabaseList;
    try
      while not DS.EOF do
      begin
        WriteLn('    ', DS.FieldByName('name').AsString, ': ',
          DS.FieldByName('file').AsString);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;

    // 11. Detaching databases
    WriteLn('11. Detaching Databases:');
    Attached.Detach('temp_db');
    WriteLn('    Detached temp_db');

    DBList := Attached.GetAttachedList;
    try
      WriteLn('    Remaining attached databases:');
      for I := 0 to DBList.Count - 1 do
        WriteLn('      - ', DBList[I]);
    finally
      DBList.Free;
    end;
    WriteLn;

    // 12. Best practices
    WriteLn('12. Multi-Database Best Practices:');
    WriteLn('    - Use descriptive alias names');
    WriteLn('    - Qualify table names with database prefix');
    WriteLn('    - Detach databases when no longer needed');
    WriteLn('    - Use in-memory DB for temporary data');
    WriteLn('    - Transactions span all attached databases');
    WriteLn('    - Maximum 10 attached databases (SQLite limit)');
    WriteLn;

    // Detach all remaining
    Attached.DetachAll;

  finally
    Attached.Free;
  end;

  Conn.Close;
  Conn := nil;

  WriteLn('=== Example completed successfully! ===');

  // Cleanup
  if FileExists(MainDB) then DeleteFile(MainDB);
  if FileExists(CustomersDB) then DeleteFile(CustomersDB);
  if FileExists(OrdersDB) then DeleteFile(OrdersDB);
end.
