{===============================================================================
  NDXSQLite Example 16 - Performance Optimization
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Index creation and usage
  - EXPLAIN QUERY PLAN analysis
  - Bulk insert with/without transactions
  - Performance measurement techniques

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program PerformanceOptimization;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Math,
  ndxsqliteconnection, ndxsqliteconnectionoptions, ndxsqlitetypes;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Returns the current time of day in milliseconds for performance measurement. }
function GetTimeMs: Int64;
begin
  Result := MilliSecondOfTheDay(Now);
end;

{ Creates the products table used for performance benchmarking. }
procedure CreateTestTable;
begin
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT,' +
    '  price REAL,' +
    '  stock INTEGER,' +
    '  created_at TEXT' +
    ')');
end;

{ Drops test tables to clean up. }
procedure DropTestTable;
begin
  Connection.ExecuteNonQuery('DROP TABLE IF EXISTS products');
  Connection.ExecuteNonQuery('DROP TABLE IF EXISTS products_noindex');
end;

{ Compares the performance of 1000 individual inserts with and without an enclosing transaction. }
procedure DemoBulkInsertComparison;
var
  StartTime: Int64;
  TimeWithoutTx, TimeWithTx: Int64;
  I: Integer;
begin
  WriteLn('1. Bulk Insert: Transaction vs No Transaction');
  WriteLn('   -----------------------------------------');

  // Without transaction (slower)
  Connection.ExecuteNonQuery('DELETE FROM products');
  StartTime := GetTimeMs;
  for I := 1 to 1000 do
  begin
    Connection.ExecuteNonQuery(Format(
      'INSERT INTO products (name, category, price, stock, created_at) ' +
      'VALUES (''Product %d'', ''Cat%d'', %f, %d, ''%s'')',
      [I, I mod 10, Random * 100, Random(1000), FormatDateTime('yyyy-mm-dd', Now)]));
  end;
  TimeWithoutTx := GetTimeMs - StartTime;
  WriteLn(Format('   Without transaction: 1000 inserts in %d ms', [TimeWithoutTx]));

  // With transaction (faster)
  Connection.ExecuteNonQuery('DELETE FROM products');
  StartTime := GetTimeMs;
  Connection.BeginTransaction;
  try
    for I := 1 to 1000 do
    begin
      Connection.ExecuteNonQuery(Format(
        'INSERT INTO products (name, category, price, stock, created_at) ' +
        'VALUES (''Product %d'', ''Cat%d'', %f, %d, ''%s'')',
        [I, I mod 10, Random * 100, Random(1000), FormatDateTime('yyyy-mm-dd', Now)]));
    end;
    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;
  TimeWithTx := GetTimeMs - StartTime;
  WriteLn(Format('   With transaction: 1000 inserts in %d ms', [TimeWithTx]));
  WriteLn(Format('   Speedup: %.1fx faster with transaction', [TimeWithoutTx / Max(TimeWithTx, 1)]));
  WriteLn('');
end;

{ Measures query execution time before and after adding an index on the category column. }
procedure DemoIndexPerformance;
var
  StartTime: Int64;
  TimeNoIndex, TimeWithIndex: Int64;
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('2. Index Performance Comparison');
  WriteLn('   ----------------------------');

  // Create table without index for comparison
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products_noindex (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT,' +
    '  price REAL' +
    ')');

  // Insert test data
  Connection.BeginTransaction;
  try
    for I := 1 to 10000 do
    begin
      Connection.ExecuteNonQuery(Format(
        'INSERT INTO products_noindex (name, category, price) ' +
        'VALUES (''Product %d'', ''Category%d'', %f)',
        [I, I mod 100, Random * 1000]));
    end;
    Connection.Commit;
  except
    Connection.Rollback;
  end;

  // Query without index
  StartTime := GetTimeMs;
  for I := 1 to 100 do
  begin
    DS := Connection.ExecuteQuery(Format(
      'SELECT * FROM products_noindex WHERE category = ''Category%d''', [I mod 100]));
    DS.Free;
  end;
  TimeNoIndex := GetTimeMs - StartTime;
  WriteLn(Format('   Without index: 100 queries in %d ms', [TimeNoIndex]));

  // Create index
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_category ON products_noindex(category)');
  WriteLn('   Index created on category column');

  // Query with index
  StartTime := GetTimeMs;
  for I := 1 to 100 do
  begin
    DS := Connection.ExecuteQuery(Format(
      'SELECT * FROM products_noindex WHERE category = ''Category%d''', [I mod 100]));
    DS.Free;
  end;
  TimeWithIndex := GetTimeMs - StartTime;
  WriteLn(Format('   With index: 100 queries in %d ms', [TimeWithIndex]));
  WriteLn(Format('   Speedup: %.1fx faster with index', [TimeNoIndex / Max(TimeWithIndex, 1)]));
  WriteLn('');
end;

{ Uses EXPLAIN QUERY PLAN to show how SQLite resolves queries before and after index creation. }
procedure DemoExplainQueryPlan;
var
  DS: TDataSet;
begin
  WriteLn('3. EXPLAIN QUERY PLAN Analysis');
  WriteLn('   ---------------------------');

  // Query without index usage
  WriteLn('   Query: SELECT * FROM products WHERE price > 50');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM products WHERE price > 50');
  try
    while not DS.EOF do
    begin
      WriteLn('   Plan: ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Create index on price
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_price ON products(price)');
  WriteLn('');
  WriteLn('   After creating index on price:');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM products WHERE price > 50');
  try
    while not DS.EOF do
    begin
      WriteLn('   Plan: ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a composite index on (category, price) and shows its effect on multi-column filter queries. }
procedure DemoCompositeIndex;
var
  DS: TDataSet;
begin
  WriteLn('4. Composite Index');
  WriteLn('   ---------------');

  // Create composite index
  Connection.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_cat_price ON products(category, price)');
  WriteLn('   Created composite index on (category, price)');

  // This query will use the composite index
  WriteLn('');
  WriteLn('   Query: SELECT * FROM products WHERE category = ''Cat5'' AND price > 50');
  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM products WHERE category = ''Cat5'' AND price > 50');
  try
    while not DS.EOF do
    begin
      WriteLn('   Plan: ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a conditional index limited to rows where price > 80 and verifies its usage via the query plan. }
procedure DemoPartialIndex;
var
  DS: TDataSet;
begin
  WriteLn('5. Partial Index (WHERE clause in index)');
  WriteLn('   -------------------------------------');

  // Create partial index (only for high-value products)
  Connection.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_expensive ON products(price) WHERE price > 80');
  WriteLn('   Created partial index: price WHERE price > 80');

  DS := Connection.ExecuteQuery(
    'EXPLAIN QUERY PLAN SELECT * FROM products WHERE price > 90');
  try
    while not DS.EOF do
    begin
      WriteLn('   Plan: ', DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Compares query throughput using a small (100 KB) versus large (8 MB) page cache. }
procedure DemoCacheSizeImpact;
var
  StartTime: Int64;
  TimeSmallCache, TimeLargeCache: Int64;
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('6. Cache Size Impact');
  WriteLn('   -----------------');

  // Small cache
  Connection.SetCacheSize(100);  // 100 KB
  StartTime := GetTimeMs;
  for I := 1 to 50 do
  begin
    DS := Connection.ExecuteQuery('SELECT * FROM products ORDER BY RANDOM() LIMIT 100');
    DS.Free;
  end;
  TimeSmallCache := GetTimeMs - StartTime;
  WriteLn(Format('   Small cache (100 KB): 50 queries in %d ms', [TimeSmallCache]));

  // Large cache
  Connection.SetCacheSize(8000);  // 8 MB
  StartTime := GetTimeMs;
  for I := 1 to 50 do
  begin
    DS := Connection.ExecuteQuery('SELECT * FROM products ORDER BY RANDOM() LIMIT 100');
    DS.Free;
  end;
  TimeLargeCache := GetTimeMs - StartTime;
  WriteLn(Format('   Large cache (8 MB): 50 queries in %d ms', [TimeLargeCache]));
  WriteLn('');
end;

{ Prints a summary of recommended SQLite performance optimization techniques. }
procedure DemoOptimizationTips;
begin
  WriteLn('7. Performance Optimization Tips');
  WriteLn('   ------------------------------');
  WriteLn('   - Always use transactions for bulk operations');
  WriteLn('   - Create indexes on columns used in WHERE clauses');
  WriteLn('   - Use composite indexes for multi-column queries');
  WriteLn('   - Use partial indexes for filtered queries');
  WriteLn('   - Increase cache size for read-heavy workloads');
  WriteLn('   - Use WAL mode for concurrent access');
  WriteLn('   - Run ANALYZE after bulk data changes');
  WriteLn('   - Use EXPLAIN QUERY PLAN to analyze queries');
  WriteLn('   - Avoid SELECT * - select only needed columns');
  WriteLn('   - Use LIMIT for large result sets');
  WriteLn('');
end;

begin
  WriteLn('=== NDXSQLite Example 16: Performance Optimization ===');
  WriteLn('');

  Randomize;
  DBPath := ExtractFilePath(ParamStr(0)) + 'example16.db';

  // Delete old database
  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;
    Options.JournalMode := jmWAL;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;
      CreateTestTable;

      DemoBulkInsertComparison;
      DemoIndexPerformance;
      DemoExplainQueryPlan;
      DemoCompositeIndex;
      DemoPartialIndex;
      DemoCacheSizeImpact;
      DemoOptimizationTips;

      // Run ANALYZE for statistics
      Connection.ExecuteNonQuery('ANALYZE');
      WriteLn('8. ANALYZE executed to update query planner statistics.');
      WriteLn('');

      DropTestTable;
      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');

  WriteLn('=== Example completed successfully! ===');
end.
