{===============================================================================
  NDXSQLite Example 03 - Prepared Statements
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Using parameterized queries
  - Preventing SQL injection
  - Performance benefits of prepared statements
  - Different parameter types

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program PreparedStatements;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection;

var
  Connection: TNDXSQLiteConnection;
  DBPath: string;
  DataSet: TDataSet;
  I: Integer;
  StartTime: TDateTime;
  ElapsedMs: Int64;

begin
  WriteLn('=== NDXSQLite Example 03: Prepared Statements ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example03.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Connection := TNDXSQLiteConnection.Create(DBPath);
  try
    Connection.Open;

    // Create table
    WriteLn('1. Creating products table...');
    Connection.ExecuteNonQuery(
      'CREATE TABLE products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name TEXT NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  quantity INTEGER DEFAULT 0,' +
      '  active INTEGER DEFAULT 1' +
      ')'
    );
    WriteLn('   Table created.');
    WriteLn;

    // Insert with parameters - Safe from SQL injection
    WriteLn('2. Inserting data with parameterized queries...');
    Connection.ExecuteNonQuery(
      'INSERT INTO products (name, price, quantity, active) VALUES (?, ?, ?, ?)',
      ['Laptop', 999.99, 10, 1]);
    Connection.ExecuteNonQuery(
      'INSERT INTO products (name, price, quantity, active) VALUES (?, ?, ?, ?)',
      ['Mouse', 29.99, 50, 1]);
    Connection.ExecuteNonQuery(
      'INSERT INTO products (name, price, quantity, active) VALUES (?, ?, ?, ?)',
      ['Keyboard', 79.99, 30, 1]);
    Connection.ExecuteNonQuery(
      'INSERT INTO products (name, price, quantity, active) VALUES (?, ?, ?, ?)',
      ['Monitor', 299.99, 15, 0]);
    WriteLn('   4 products inserted with parameterized queries.');
    WriteLn;

    // SQL Injection prevention demo
    WriteLn('3. SQL Injection prevention:');
    WriteLn('   Attempting to search with malicious input...');

    // This malicious input would cause problems with string concatenation
    // but is safely handled by parameterized queries
    DataSet := Connection.ExecuteQuery(
      'SELECT * FROM products WHERE name = ?',
      ['Laptop''; DROP TABLE products; --']);
    try
      WriteLn(Format('   Results found: %d (table is safe!)', [DataSet.RecordCount]));
    finally
      DataSet.Free;
    end;
    WriteLn;

    // Query with parameters
    WriteLn('4. Querying with parameters:');

    WriteLn('   Products with price > $50:');
    DataSet := Connection.ExecuteQuery(
      'SELECT name, price FROM products WHERE price > ? ORDER BY price',
      [50.00]);
    try
      while not DataSet.EOF do
      begin
        WriteLn(Format('   - %s: $%.2f', [
          DataSet.FieldByName('name').AsString,
          DataSet.FieldByName('price').AsFloat
        ]));
        DataSet.Next;
      end;
    finally
      DataSet.Free;
    end;
    WriteLn;

    // Multiple parameters
    WriteLn('5. Multiple parameters (price range and active):');
    DataSet := Connection.ExecuteQuery(
      'SELECT name, price, quantity FROM products ' +
      'WHERE price BETWEEN ? AND ? AND active = ? ' +
      'ORDER BY price',
      [20.00, 500.00, 1]);
    try
      while not DataSet.EOF do
      begin
        WriteLn(Format('   - %s: $%.2f (qty: %d)', [
          DataSet.FieldByName('name').AsString,
          DataSet.FieldByName('price').AsFloat,
          DataSet.FieldByName('quantity').AsInteger
        ]));
        DataSet.Next;
      end;
    finally
      DataSet.Free;
    end;
    WriteLn;

    // ExecuteScalar with parameters
    WriteLn('6. ExecuteScalar with parameters:');
    WriteLn(Format('   Total value of active inventory: $%.2f', [
      Double(Connection.ExecuteScalar(
        'SELECT SUM(price * quantity) FROM products WHERE active = ?', [1]))
    ]));
    WriteLn(Format('   Count of products priced over $100: %d', [
      Integer(Connection.ExecuteScalar(
        'SELECT COUNT(*) FROM products WHERE price > ?', [100.00]))
    ]));
    WriteLn;

    // Performance comparison
    WriteLn('7. Performance comparison (1000 inserts):');

    // Clear and recreate for test
    Connection.ExecuteNonQuery('DELETE FROM products');

    // With parameters (simulated prepared statement reuse)
    StartTime := Now;
    Connection.BeginTransaction;
    for I := 1 to 1000 do
    begin
      Connection.ExecuteNonQuery(
        'INSERT INTO products (name, price, quantity) VALUES (?, ?, ?)',
        ['Product ' + IntToStr(I), Random * 100, Random(100)]);
    end;
    Connection.Commit;
    ElapsedMs := MilliSecondsBetween(Now, StartTime);
    WriteLn(Format('   Parameterized inserts: %d ms', [ElapsedMs]));

    // Clear for next test
    Connection.ExecuteNonQuery('DELETE FROM products');

    // Without parameters (string concatenation - NOT recommended)
    StartTime := Now;
    Connection.BeginTransaction;
    for I := 1 to 1000 do
    begin
      Connection.ExecuteNonQuery(Format(
        'INSERT INTO products (name, price, quantity) VALUES (''Product %d'', %.2f, %d)',
        [I, Random * 100, Random(100)]));
    end;
    Connection.Commit;
    ElapsedMs := MilliSecondsBetween(Now, StartTime);
    WriteLn(Format('   String concatenation: %d ms', [ElapsedMs]));
    WriteLn('   (Parameterized queries are typically faster and always safer)');
    WriteLn;

    // Update with parameters
    WriteLn('8. Update with parameters:');
    Connection.ExecuteNonQuery(
      'UPDATE products SET price = price * ? WHERE price < ?',
      [1.10, 50.00]); // 10% increase for products under $50
    WriteLn(Format('   Updated %d rows (10%% price increase for items under $50)',
      [Connection.GetChangesCount]));
    WriteLn;

    Connection.Close;

  finally
    Connection.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
