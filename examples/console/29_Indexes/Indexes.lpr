{===============================================================================
  NDXSQLite Example 29 - Indexes
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Creating indexes for performance
  - Unique indexes
  - Composite (multi-column) indexes
  - Partial indexes (WHERE clause)
  - Expression indexes
  - EXPLAIN QUERY PLAN to verify index usage
  - Index statistics

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Indexes;

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
var
  I: Integer;
begin
  // Create table with lots of data for meaningful demos
  Connection.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  email TEXT,' +
    '  first_name TEXT,' +
    '  last_name TEXT,' +
    '  country TEXT,' +
    '  city TEXT,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  created_at TEXT,' +
    '  total_orders INTEGER DEFAULT 0' +
    ')');

  // Insert sample data
  Connection.BeginTransaction;
  for I := 1 to 10000 do
  begin
    Connection.ExecuteNonQuery(
      'INSERT INTO customers (email, first_name, last_name, country, city, is_active, created_at, total_orders) ' +
      'VALUES (?, ?, ?, ?, ?, ?, datetime(''now'', ''-'' || ? || '' days''), ?)',
      [Format('user%d@example.com', [I]),
       Format('First%d', [I]),
       Format('Last%d', [I mod 100]),
       Format('Country%d', [I mod 10]),
       Format('City%d', [I mod 50]),
       Ord(I mod 5 <> 0),  // 80% active
       I mod 365,
       I mod 100]);
  end;
  Connection.Commit;
end;

{ Creates an index on the email column and compares EXPLAIN QUERY PLAN output
  before and after to show the transition from a table scan to an index search. }
procedure DemoBasicIndex;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic index creation');
  WriteLn('   ---------------------');

  // Query without index
  WriteLn('   Query plan WITHOUT index:');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE email = ''user5000@example.com''');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Create index
  Connection.ExecuteNonQuery('CREATE INDEX idx_customers_email ON customers(email)');
  WriteLn('   Created: idx_customers_email');

  // Query with index
  WriteLn('   Query plan WITH index:');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE email = ''user5000@example.com''');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates a UNIQUE index on the email column and verifies that inserting a
  duplicate email value is rejected with a constraint error. }
procedure DemoUniqueIndex;
begin
  WriteLn('2. Unique index');
  WriteLn('   -------------');

  // Create unique index
  Connection.ExecuteNonQuery(
    'CREATE UNIQUE INDEX idx_customers_email_unique ON customers(email)');
  WriteLn('   Created: idx_customers_email_unique (UNIQUE)');

  // Try to insert duplicate
  try
    Connection.ExecuteNonQuery(
      'INSERT INTO customers (email, first_name, last_name) ' +
      'VALUES (''user1@example.com'', ''Duplicate'', ''User'')');
    WriteLn('   ERROR: Should have failed!');
  except
    on E: Exception do
      WriteLn('   Duplicate insert rejected: UNIQUE constraint');
  end;

  WriteLn('');
end;

{ Creates a composite index on (country, city) and shows via EXPLAIN QUERY PLAN
  that it helps queries on both columns or the leading column only, but not the
  trailing column alone. }
procedure DemoCompositeIndex;
var
  DS: TDataSet;
begin
  WriteLn('3. Composite (multi-column) index');
  WriteLn('   -------------------------------');

  // Create composite index
  Connection.ExecuteNonQuery(
    'CREATE INDEX idx_customers_country_city ON customers(country, city)');
  WriteLn('   Created: idx_customers_country_city (country, city)');

  // Query using both columns
  WriteLn('   Query using both columns:');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE country = ''Country1'' AND city = ''City10''');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Query using only first column (index still helps)
  WriteLn('   Query using only first column (country):');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE country = ''Country1''');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Query using only second column (index NOT used!)
  WriteLn('   Query using only second column (city) - INDEX NOT USED:');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE city = ''City10''');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   NOTE: Column order matters! Index on (A,B) helps A or A+B, NOT B alone.');
  WriteLn('');
end;

{ Creates a partial index filtered by is_active=1 and shows via EXPLAIN QUERY
  PLAN that it is used only when the query matches the index WHERE condition. }
procedure DemoPartialIndex;
var
  DS: TDataSet;
begin
  WriteLn('4. Partial index (with WHERE clause)');
  WriteLn('   ----------------------------------');

  // Create partial index only for active customers
  Connection.ExecuteNonQuery(
    'CREATE INDEX idx_active_customers ON customers(last_name) WHERE is_active = 1');
  WriteLn('   Created: idx_active_customers (only rows where is_active=1)');

  // Query that matches the partial index condition
  WriteLn('   Query matching partial index condition:');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE last_name = ''Last50'' AND is_active = 1');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Query that doesn't match (index not used)
  WriteLn('   Query NOT matching (is_active=0):');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE last_name = ''Last50'' AND is_active = 0');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Partial indexes save space and are faster when you only query a subset.');
  WriteLn('');
end;

{ Creates an expression index on lower(email) and shows via EXPLAIN QUERY PLAN
  that it is used for case-insensitive email lookups. }
procedure DemoExpressionIndex;
var
  DS: TDataSet;
begin
  WriteLn('5. Expression index');
  WriteLn('   -----------------');

  // Create index on expression (lowercase email)
  Connection.ExecuteNonQuery(
    'CREATE INDEX idx_customers_email_lower ON customers(lower(email))');
  WriteLn('   Created: idx_customers_email_lower (on lower(email))');

  // Query using same expression
  WriteLn('   Query using lower():');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE lower(email) = ''user100@example.com''');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Expression indexes are great for case-insensitive searches.');
  WriteLn('');
end;

{ Creates a covering index on (country, first_name, last_name) and shows that
  a query selecting only those columns avoids table lookups entirely. }
procedure DemoCoveringIndex;
var
  DS: TDataSet;
begin
  WriteLn('6. Covering index');
  WriteLn('   ---------------');

  // Create index that includes all columns needed by query
  Connection.ExecuteNonQuery(
    'CREATE INDEX idx_customers_covering ON customers(country, first_name, last_name)');
  WriteLn('   Created: idx_customers_covering (country, first_name, last_name)');

  // Query that can be satisfied entirely from index
  WriteLn('   Query satisfied from index only (no table access):');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT first_name, last_name FROM customers WHERE country = ''Country5''');
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Covering indexes avoid table lookups, improving performance.');
  WriteLn('');
end;

{ Queries sqlite_master to list all user-created indexes with their table names
  and SQL definitions. }
procedure DemoListIndexes;
var
  DS: TDataSet;
begin
  WriteLn('7. List all indexes');
  WriteLn('   -----------------');

  DS := Connection.ExecuteQuery(
    'SELECT name, tbl_name, sql FROM sqlite_master ' +
    'WHERE type = ''index'' AND sql IS NOT NULL ORDER BY tbl_name, name');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString, ' on ', DS.FieldByName('tbl_name').AsString);
      WriteLn('     ', DS.FieldByName('sql').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Uses PRAGMA index_info to display the column positions and names within
  the composite index idx_customers_country_city. }
procedure DemoIndexInfo;
var
  DS: TDataSet;
begin
  WriteLn('8. Index details (PRAGMA index_info)');
  WriteLn('   ----------------------------------');

  WriteLn('   Columns in idx_customers_country_city:');
  DS := Connection.ExecuteQuery('PRAGMA index_info(idx_customers_country_city)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     Position %d: %s', [
        DS.FieldByName('seqno').AsInteger,
        DS.FieldByName('name').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates the ANALYZE command for updating query optimizer statistics. }
procedure DemoAnalyze;
var
  DS: TDataSet;
begin
  WriteLn('9. ANALYZE for query optimizer');
  WriteLn('   ----------------------------');

  // Run ANALYZE to gather statistics
  Connection.ExecuteNonQuery('ANALYZE');
  WriteLn('   Ran ANALYZE to update statistics');

  // Check statistics
  DS := Connection.ExecuteQuery(
    'SELECT tbl, idx, stat FROM sqlite_stat1 WHERE tbl = ''customers'' LIMIT 5');
  try
    WriteLn('   Statistics (sqlite_stat1):');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s.%s: %s', [
        DS.FieldByName('tbl').AsString,
        DS.FieldByName('idx').AsString,
        DS.FieldByName('stat').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   ANALYZE helps SQLite choose the best index for queries.');
  WriteLn('');
end;

{ Drops the idx_customers_email and idx_active_customers indexes using
  DROP INDEX IF EXISTS. }
procedure DemoDropIndex;
begin
  WriteLn('10. Drop index');
  WriteLn('    -----------');

  Connection.ExecuteNonQuery('DROP INDEX IF EXISTS idx_customers_email');
  WriteLn('    Dropped: idx_customers_email');

  Connection.ExecuteNonQuery('DROP INDEX IF EXISTS idx_active_customers');
  WriteLn('    Dropped: idx_active_customers');

  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 29: Indexes ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example29.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      WriteLn('Setting up test data (10,000 rows)...');
      SetupTables;
      WriteLn('Done.');
      WriteLn('');

      DemoBasicIndex;
      DemoUniqueIndex;
      DemoCompositeIndex;
      DemoPartialIndex;
      DemoExpressionIndex;
      DemoCoveringIndex;
      DemoListIndexes;
      DemoIndexInfo;
      DemoAnalyze;
      DemoDropIndex;

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
