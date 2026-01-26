{===============================================================================
  NDXSQLite Example 23 - Generated Columns
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Generated (computed) columns
  - STORED vs VIRTUAL generated columns
  - STRICT tables
  - WITHOUT ROWID tables

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program GeneratedColumns;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates a products table with VIRTUAL generated columns for total and price_category, inserts rows, and displays results. }
procedure DemoGeneratedColumns;
var
  DS: TDataSet;
begin
  WriteLn('1. Generated Columns (VIRTUAL)');
  WriteLn('   ---------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  total REAL GENERATED ALWAYS AS (price * quantity) VIRTUAL,' +
    '  price_category TEXT GENERATED ALWAYS AS (' +
    '    CASE ' +
    '      WHEN price < 10 THEN ''cheap'' ' +
    '      WHEN price < 100 THEN ''medium'' ' +
    '      ELSE ''expensive'' ' +
    '    END' +
    '  ) VIRTUAL' +
    ')');

  WriteLn('   Created table with VIRTUAL generated columns');
  WriteLn('   - total = price * quantity');
  WriteLn('   - price_category = CASE expression');

  Connection.ExecuteNonQuery('INSERT INTO products (name, price, quantity) VALUES (''Pen'', 2.50, 100)');
  Connection.ExecuteNonQuery('INSERT INTO products (name, price, quantity) VALUES (''Notebook'', 15.00, 50)');
  Connection.ExecuteNonQuery('INSERT INTO products (name, price, quantity) VALUES (''Laptop'', 999.99, 5)');

  DS := Connection.ExecuteQuery('SELECT name, price, quantity, total, price_category FROM products');
  try
    WriteLn('   Products with computed columns:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: $%.2f x %d = $%.2f (%s)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('total').AsFloat,
         DS.FieldByName('price_category').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a users table with STORED generated columns for full_name and email_domain, then displays the computed values. }
procedure DemoStoredGeneratedColumns;
var
  DS: TDataSet;
begin
  WriteLn('2. STORED Generated Columns');
  WriteLn('   ------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  first_name TEXT NOT NULL,' +
    '  last_name TEXT NOT NULL,' +
    '  full_name TEXT GENERATED ALWAYS AS (first_name || '' '' || last_name) STORED,' +
    '  email TEXT,' +
    '  email_domain TEXT GENERATED ALWAYS AS (' +
    '    SUBSTR(email, INSTR(email, ''@'') + 1)' +
    '  ) STORED' +
    ')');

  WriteLn('   STORED columns are physically saved (faster queries, uses storage)');
  WriteLn('   VIRTUAL columns are computed on read (no storage, computed each time)');

  Connection.ExecuteNonQuery('INSERT INTO users (first_name, last_name, email) VALUES (''John'', ''Doe'', ''john@example.com'')');
  Connection.ExecuteNonQuery('INSERT INTO users (first_name, last_name, email) VALUES (''Jane'', ''Smith'', ''jane@company.org'')');

  DS := Connection.ExecuteQuery('SELECT full_name, email, email_domain FROM users');
  try
    WriteLn('   Users with stored computed columns:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s <%s> (domain: %s)',
        [DS.FieldByName('full_name').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('email_domain').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a STRICT table and shows that inserting a TEXT value into an INTEGER column is rejected. }
procedure DemoStrictTables;
var
  DS: TDataSet;
begin
  WriteLn('3. STRICT Tables');
  WriteLn('   -------------');

  // STRICT tables enforce type checking
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS strict_data (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  count INTEGER NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  data BLOB' +
    ') STRICT');

  WriteLn('   STRICT table enforces column types');
  WriteLn('   Allowed types: INTEGER, REAL, TEXT, BLOB, ANY');

  // Valid insert
  Connection.ExecuteNonQuery('INSERT INTO strict_data (name, count, price) VALUES (''Item A'', 10, 99.99)');

  // This would fail with type mismatch in a STRICT table:
  // Connection.ExecuteNonQuery('INSERT INTO strict_data (name, count, price) VALUES (''Item B'', ''not a number'', 50.00)');

  WriteLn('   Inserted valid data');

  // Try inserting wrong type
  try
    Connection.ExecuteNonQuery('INSERT INTO strict_data (name, count, price) VALUES (''Bad'', ''text'', 10.00)');
    WriteLn('   ERROR: Should have failed!');
  except
    on E: Exception do
      WriteLn('   Correctly rejected wrong type: cannot store TEXT in INTEGER column');
  end;

  DS := Connection.ExecuteQuery('SELECT * FROM strict_data');
  try
    WriteLn('   Data in strict table:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: count=%d, price=%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('count').AsInteger,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a WITHOUT ROWID key-value table with a TEXT primary key, inserts entries, and lists them. }
procedure DemoWithoutRowid;
var
  DS: TDataSet;
begin
  WriteLn('4. WITHOUT ROWID Tables');
  WriteLn('   --------------------');

  // WITHOUT ROWID tables are more efficient for certain use cases
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS key_value (' +
    '  key TEXT PRIMARY KEY,' +
    '  value TEXT NOT NULL,' +
    '  updated_at TEXT DEFAULT (datetime(''now''))' +
    ') WITHOUT ROWID');

  WriteLn('   WITHOUT ROWID optimizes tables with non-integer primary keys');
  WriteLn('   Best for: key-value stores, lookup tables');

  Connection.ExecuteNonQuery('INSERT INTO key_value (key, value) VALUES (''config.theme'', ''dark'')');
  Connection.ExecuteNonQuery('INSERT INTO key_value (key, value) VALUES (''config.lang'', ''en'')');
  Connection.ExecuteNonQuery('INSERT INTO key_value (key, value) VALUES (''user.name'', ''Alice'')');

  DS := Connection.ExecuteQuery('SELECT * FROM key_value ORDER BY key');
  try
    WriteLn('   Key-value data:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s = %s',
        [DS.FieldByName('key').AsString,
         DS.FieldByName('value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates an index on the stored email_domain column and shows the query plan using it. }
procedure DemoGeneratedColumnIndex;
var
  DS: TDataSet;
begin
  WriteLn('5. Index on Generated Column');
  WriteLn('   -------------------------');

  // Can create index on STORED generated columns
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_email_domain ON users(email_domain)');

  WriteLn('   Created index on stored generated column (email_domain)');

  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM users WHERE email_domain = ''example.com''');
  try
    WriteLn('   Query plan:');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a STRICT table with an ANY column and inserts integer, text, and real values to show mixed-type storage. }
procedure DemoAnyType;
var
  DS: TDataSet;
begin
  WriteLn('6. ANY Type in STRICT Tables');
  WriteLn('   -------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS flexible_strict (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  metadata ANY' +
    ') STRICT');

  WriteLn('   ANY column accepts any type in STRICT table');

  Connection.ExecuteNonQuery('INSERT INTO flexible_strict (name, metadata) VALUES (''Item 1'', 42)');
  Connection.ExecuteNonQuery('INSERT INTO flexible_strict (name, metadata) VALUES (''Item 2'', ''text value'')');
  Connection.ExecuteNonQuery('INSERT INTO flexible_strict (name, metadata) VALUES (''Item 3'', 3.14159)');

  DS := Connection.ExecuteQuery('SELECT name, metadata, typeof(metadata) as type FROM flexible_strict');
  try
    WriteLn('   Mixed types in ANY column:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %s (type: %s)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('metadata').AsString,
         DS.FieldByName('type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Outputs usage guidelines for generated columns, STRICT tables, and WITHOUT ROWID tables. }
procedure DemoBestPractices;
begin
  WriteLn('7. Best Practices');
  WriteLn('   --------------');
  WriteLn('   Generated Columns:');
  WriteLn('   - Use VIRTUAL for rarely queried computed values');
  WriteLn('   - Use STORED for frequently queried/indexed values');
  WriteLn('   - Cannot reference other generated columns');
  WriteLn('');
  WriteLn('   STRICT Tables:');
  WriteLn('   - Use for data integrity enforcement');
  WriteLn('   - Prevents accidental type mismatches');
  WriteLn('   - Use ANY for truly dynamic columns');
  WriteLn('');
  WriteLn('   WITHOUT ROWID:');
  WriteLn('   - Use for TEXT/BLOB primary keys');
  WriteLn('   - Good for small tables accessed by key');
  WriteLn('   - Requires explicit PRIMARY KEY');
  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 23: Generated Columns ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example23.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      DemoGeneratedColumns;
      DemoStoredGeneratedColumns;
      DemoStrictTables;
      DemoWithoutRowid;
      DemoGeneratedColumnIndex;
      DemoAnyType;
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
