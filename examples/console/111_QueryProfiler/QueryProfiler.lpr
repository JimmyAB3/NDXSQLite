{===============================================================================
  NDXSQLite Example 111 - Query Profiler
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Query execution time measurement
  - Slow query detection and logging
  - EXPLAIN QUERY PLAN analysis
  - Query statistics aggregation
  - Performance bottleneck identification

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program QueryProfiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

{
  Example 111: Query Profiler
  - Query timing (execution time measurement)
  - Slow query log (threshold-based detection)
  - Query plan analysis (EXPLAIN QUERY PLAN)
  - Query statistics (count, avg, min, max per query)
  - Query plan cache (store analyzed plans)
  - Index impact analysis (with/without index comparison)
}

var
  Conn: TNDXSQLiteConnection;

// ============================================================================
// Profiler Infrastructure
// ============================================================================

const
  MAX_PROFILE_ENTRIES = 100;
  SLOW_QUERY_THRESHOLD_MS = 5; // 5ms threshold for slow queries

type
  TProfileEntry = record
    SQL: string;
    ExecutionTimeMs: Double;
    RowCount: Integer;
    Timestamp: string;
    IsSlow: Boolean;
  end;

  TQueryStats = record
    SQL: string;
    CallCount: Integer;
    TotalTimeMs: Double;
    MinTimeMs: Double;
    MaxTimeMs: Double;
    TotalRows: Integer;
  end;

  TQueryPlanEntry = record
    SQL: string;
    PlanDetail: string;
    AnalyzedAt: string;
  end;

var
  ProfileLog: array[0..MAX_PROFILE_ENTRIES-1] of TProfileEntry;
  ProfileCount: Integer = 0;
  SlowQueryLog: array[0..MAX_PROFILE_ENTRIES-1] of TProfileEntry;
  SlowQueryCount: Integer = 0;
  QueryStats: array[0..49] of TQueryStats;
  StatsCount: Integer = 0;
  PlanCache: array[0..49] of TQueryPlanEntry;
  PlanCacheCount: Integer = 0;

{ Returns the current system tick count in milliseconds for timing measurements. }
function GetTimeMs: Int64;
begin
  Result := GetTickCount64;
end;

{ Records a query execution in the profile log and adds it to the slow query
  log if execution time exceeds the threshold. }
procedure AddProfileEntry(const SQL: string; TimeMs: Double; Rows: Integer);
var
  Entry: TProfileEntry;
begin
  Entry.SQL := SQL;
  Entry.ExecutionTimeMs := TimeMs;
  Entry.RowCount := Rows;
  Entry.Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
  Entry.IsSlow := TimeMs >= SLOW_QUERY_THRESHOLD_MS;

  if ProfileCount < MAX_PROFILE_ENTRIES then
  begin
    ProfileLog[ProfileCount] := Entry;
    Inc(ProfileCount);
  end;

  if Entry.IsSlow and (SlowQueryCount < MAX_PROFILE_ENTRIES) then
  begin
    SlowQueryLog[SlowQueryCount] := Entry;
    Inc(SlowQueryCount);
  end;
end;

{ Finds or creates a statistics entry for the SQL pattern and updates its call
  count, total/min/max times, and row count. }
procedure UpdateQueryStats(const SQL: string; TimeMs: Double; Rows: Integer);
var
  I, Idx: Integer;
begin
  Idx := -1;
  for I := 0 to StatsCount - 1 do
    if QueryStats[I].SQL = SQL then
    begin
      Idx := I;
      Break;
    end;

  if Idx = -1 then
  begin
    if StatsCount >= 50 then Exit;
    Idx := StatsCount;
    Inc(StatsCount);
    QueryStats[Idx].SQL := SQL;
    QueryStats[Idx].CallCount := 0;
    QueryStats[Idx].TotalTimeMs := 0;
    QueryStats[Idx].MinTimeMs := 999999;
    QueryStats[Idx].MaxTimeMs := 0;
    QueryStats[Idx].TotalRows := 0;
  end;

  Inc(QueryStats[Idx].CallCount);
  QueryStats[Idx].TotalTimeMs := QueryStats[Idx].TotalTimeMs + TimeMs;
  QueryStats[Idx].TotalRows := QueryStats[Idx].TotalRows + Rows;
  if TimeMs < QueryStats[Idx].MinTimeMs then
    QueryStats[Idx].MinTimeMs := TimeMs;
  if TimeMs > QueryStats[Idx].MaxTimeMs then
    QueryStats[Idx].MaxTimeMs := TimeMs;
end;

// Profiled query execution - returns TDataSet
{ Executes a query with profiling and returns the result dataset. }
function ProfiledQuery(const SQL: string): TDataSet;
var
  StartTime: Int64;
  ElapsedMs: Double;
  Rows: Integer;
begin
  StartTime := GetTimeMs;
  Result := Conn.ExecuteQuery(SQL);
  ElapsedMs := GetTimeMs - StartTime;

  // Count rows
  Rows := 0;
  Result.First;
  while not Result.EOF do
  begin
    Inc(Rows);
    Result.Next;
  end;
  Result.First;

  AddProfileEntry(SQL, ElapsedMs, Rows);
  UpdateQueryStats(SQL, ElapsedMs, Rows);
end;

// Profiled non-query execution
{ Executes a non-query statement with timing, then logs the execution time and
  updates statistics (row count recorded as 0). }
procedure ProfiledExec(const SQL: string);
var
  StartTime: Int64;
  ElapsedMs: Double;
begin
  StartTime := GetTimeMs;
  Conn.ExecuteNonQuery(SQL);
  ElapsedMs := GetTimeMs - StartTime;

  AddProfileEntry(SQL, ElapsedMs, 0);
  UpdateQueryStats(SQL, ElapsedMs, 0);
end;

// Get and cache query plan
{ Returns the EXPLAIN QUERY PLAN output for the SQL, using a cached result if
  available or running the analysis and storing it in the plan cache. }
function GetQueryPlan(const SQL: string): string;
var
  DS: TDataSet;
  Plan: string;
  I: Integer;
begin
  // Check cache first
  for I := 0 to PlanCacheCount - 1 do
    if PlanCache[I].SQL = SQL then
    begin
      Result := PlanCache[I].PlanDetail;
      Exit;
    end;

  // Analyze query plan
  Plan := '';
  DS := Conn.ExecuteQuery('EXPLAIN QUERY PLAN ' + SQL);
  try
    while not DS.EOF do
    begin
      if Plan <> '' then
        Plan := Plan + ' | ';
      Plan := Plan + DS.FieldByName('detail').AsString;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Cache the plan
  if PlanCacheCount < 50 then
  begin
    PlanCache[PlanCacheCount].SQL := SQL;
    PlanCache[PlanCacheCount].PlanDetail := Plan;
    PlanCache[PlanCacheCount].AnalyzedAt := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    Inc(PlanCacheCount);
  end;

  Result := Plan;
end;

// ============================================================================
// Test Data Setup
// ============================================================================

{ Inserts test data for demonstrations. }
procedure CreateTestData;
var
  I: Integer;
  Categories: array[0..4] of string;
begin
  Categories[0] := 'Electronics';
  Categories[1] := 'Books';
  Categories[2] := 'Clothing';
  Categories[3] := 'Food';
  Categories[4] := 'Tools';

  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL,' +
    '  created_at TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  product_id INTEGER NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  total REAL NOT NULL,' +
    '  order_date TEXT NOT NULL,' +
    '  customer TEXT NOT NULL' +
    ')');

  // Profiler tables
  Conn.ExecuteNonQuery(
    'CREATE TABLE query_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sql_text TEXT NOT NULL,' +
    '  execution_ms REAL NOT NULL,' +
    '  row_count INTEGER,' +
    '  logged_at TEXT NOT NULL,' +
    '  is_slow INTEGER DEFAULT 0' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE query_plan_cache (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sql_text TEXT NOT NULL UNIQUE,' +
    '  plan_detail TEXT NOT NULL,' +
    '  cached_at TEXT NOT NULL,' +
    '  hit_count INTEGER DEFAULT 0' +
    ')');

  // Insert test data (1000 products)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 1000 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO products (name, category, price, stock, created_at) ' +
      'VALUES (''Product_%d'', ''%s'', %.2f, %d, ''2024-%s-%s'')',
      [I, Categories[I mod 5], (I mod 100) * 9.99 + 0.99,
       I mod 500, Format('%.2d', [(I mod 12) + 1]), Format('%.2d', [(I mod 28) + 1])]));
  end;
  Conn.ExecuteNonQuery('COMMIT');

  // Insert orders (5000 orders)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 5000 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO orders (product_id, quantity, total, order_date, customer) ' +
      'VALUES (%d, %d, %.2f, ''2024-%s-%s'', ''Customer_%d'')',
      [(I mod 1000) + 1, (I mod 10) + 1, ((I mod 100) + 1) * 15.50,
       Format('%.2d', [(I mod 12) + 1]), Format('%.2d', [(I mod 28) + 1]),
       (I mod 200) + 1]));
  end;
  Conn.ExecuteNonQuery('COMMIT');
end;

// ============================================================================
// Demo 1: Basic Query Timing
// ============================================================================

{ Executes various query types (COUNT, filtered, JOIN with aggregation) with
  profiling enabled and prints execution time and row count for each. }
procedure DemoQueryTiming;
var
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('=== 1. Query Timing ===');
  WriteLn;

  // Simple SELECT
  DS := ProfiledQuery('SELECT COUNT(*) as cnt FROM products');
  try
    WriteLn(Format('   SELECT COUNT(*) FROM products: %s rows, %.1f ms',
      [DS.FieldByName('cnt').AsString, ProfileLog[ProfileCount-1].ExecutionTimeMs]));
  finally
    DS.Free;
  end;

  // Filtered query
  DS := ProfiledQuery('SELECT * FROM products WHERE category = ''Electronics''');
  try
    WriteLn(Format('   SELECT WHERE category: %d rows, %.1f ms',
      [ProfileLog[ProfileCount-1].RowCount, ProfileLog[ProfileCount-1].ExecutionTimeMs]));
  finally
    DS.Free;
  end;

  // JOIN query
  DS := ProfiledQuery(
    'SELECT p.name, SUM(o.quantity) as total_qty FROM products p ' +
    'JOIN orders o ON o.product_id = p.id GROUP BY p.id ORDER BY total_qty DESC LIMIT 10');
  try
    WriteLn(Format('   JOIN + GROUP BY + ORDER: %d rows, %.1f ms',
      [ProfileLog[ProfileCount-1].RowCount, ProfileLog[ProfileCount-1].ExecutionTimeMs]));
  finally
    DS.Free;
  end;

  // Multiple executions for statistics
  for I := 1 to 5 do
  begin
    DS := ProfiledQuery('SELECT * FROM products WHERE price > 500');
    DS.Free;
  end;
  WriteLn(Format('   SELECT WHERE price > 500 (5 runs): avg %.1f ms',
    [QueryStats[StatsCount-1].TotalTimeMs / QueryStats[StatsCount-1].CallCount]));

  WriteLn;
end;

// ============================================================================
// Demo 2: Slow Query Detection
// ============================================================================

{ Runs expensive queries (aggregations, cross joins, subqueries) to trigger slow
  query detection, then prints and persists any queries exceeding the threshold. }
procedure DemoSlowQueryLog;
var
  DS: TDataSet;
  I: Integer;
  StartCount: Integer;
begin
  WriteLn(Format('=== 2. Slow Query Log (threshold: %d ms) ===', [SLOW_QUERY_THRESHOLD_MS]));
  WriteLn;

  StartCount := SlowQueryCount;

  // Run queries that might be slow (full table scans on larger data)
  DS := ProfiledQuery(
    'SELECT p.category, COUNT(*) as cnt, AVG(p.price) as avg_price, ' +
    'SUM(o.quantity) as total_orders FROM products p ' +
    'LEFT JOIN orders o ON o.product_id = p.id ' +
    'GROUP BY p.category ORDER BY total_orders DESC');
  DS.Free;

  // Cross join (intentionally expensive)
  DS := ProfiledQuery(
    'SELECT COUNT(*) as cnt FROM products p1, products p2 WHERE p1.price > p2.price AND p1.id < 50 AND p2.id < 50');
  DS.Free;

  // Subquery
  DS := ProfiledQuery(
    'SELECT * FROM products WHERE id IN (SELECT product_id FROM orders GROUP BY product_id HAVING COUNT(*) > 3)');
  DS.Free;

  // Show slow queries detected
  if SlowQueryCount > StartCount then
  begin
    WriteLn(Format('   Detected %d slow queries:', [SlowQueryCount - StartCount]));
    for I := StartCount to SlowQueryCount - 1 do
    begin
      WriteLn(Format('   [SLOW] %.1f ms | %d rows | %s',
        [SlowQueryLog[I].ExecutionTimeMs, SlowQueryLog[I].RowCount,
         Copy(SlowQueryLog[I].SQL, 1, 60)]));
    end;
  end
  else
    WriteLn('   No slow queries detected (all under threshold)');

  // Persist slow queries to database
  for I := StartCount to SlowQueryCount - 1 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO query_log (sql_text, execution_ms, row_count, logged_at, is_slow) ' +
      'VALUES (''%s'', %.2f, %d, ''%s'', 1)',
      [StringReplace(SlowQueryLog[I].SQL, '''', '''''', [rfReplaceAll]),
       SlowQueryLog[I].ExecutionTimeMs, SlowQueryLog[I].RowCount,
       SlowQueryLog[I].Timestamp]));
  end;

  WriteLn;
end;

// ============================================================================
// Demo 3: Query Plan Analysis
// ============================================================================

{ Runs EXPLAIN QUERY PLAN on various query patterns (full scan, primary key,
  join, subquery, ORDER BY) and prints the execution plan for each. }
procedure DemoQueryPlanAnalysis;
var
  Plan: string;
begin
  WriteLn('=== 3. Query Plan Analysis (EXPLAIN QUERY PLAN) ===');
  WriteLn;

  // Full table scan
  Plan := GetQueryPlan('SELECT * FROM products WHERE category = ''Electronics''');
  WriteLn('   Query: SELECT * FROM products WHERE category = ''Electronics''');
  WriteLn('   Plan:  ' + Plan);
  WriteLn;

  // Primary key lookup
  Plan := GetQueryPlan('SELECT * FROM products WHERE id = 42');
  WriteLn('   Query: SELECT * FROM products WHERE id = 42');
  WriteLn('   Plan:  ' + Plan);
  WriteLn;

  // JOIN query plan
  Plan := GetQueryPlan(
    'SELECT p.name, o.total FROM products p JOIN orders o ON o.product_id = p.id WHERE p.category = ''Books''');
  WriteLn('   Query: SELECT p.name, o.total FROM products p JOIN orders o...');
  WriteLn('   Plan:  ' + Plan);
  WriteLn;

  // Subquery plan
  Plan := GetQueryPlan(
    'SELECT * FROM products WHERE price > (SELECT AVG(price) FROM products)');
  WriteLn('   Query: SELECT * WHERE price > (SELECT AVG(price)...)');
  WriteLn('   Plan:  ' + Plan);
  WriteLn;

  // ORDER BY without index
  Plan := GetQueryPlan('SELECT * FROM products ORDER BY price DESC LIMIT 10');
  WriteLn('   Query: SELECT * FROM products ORDER BY price DESC LIMIT 10');
  WriteLn('   Plan:  ' + Plan);
  WriteLn;
end;

// ============================================================================
// Demo 4: Query Plan Cache
// ============================================================================

{ Shows cache behavior by requesting the same query plan twice, verifying cache
  hit, then persists all cached plans to the query_plan_cache table. }
procedure DemoQueryPlanCache;
var
  Plan1, Plan2: string;
  I: Integer;
  DS: TDataSet;
begin
  WriteLn('=== 4. Query Plan Cache ===');
  WriteLn;

  // First call - cache miss, analyzes plan
  Plan1 := GetQueryPlan('SELECT * FROM orders WHERE customer = ''Customer_1''');
  WriteLn('   First call (cache miss): ' + Plan1);

  // Second call - cache hit
  Plan2 := GetQueryPlan('SELECT * FROM orders WHERE customer = ''Customer_1''');
  WriteLn('   Second call (cache hit): ' + Plan2);
  WriteLn(Format('   Plans match: %s', [BoolToStr(Plan1 = Plan2, 'YES', 'NO')]));

  // Persist plan cache to database
  for I := 0 to PlanCacheCount - 1 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT OR REPLACE INTO query_plan_cache (sql_text, plan_detail, cached_at) ' +
      'VALUES (''%s'', ''%s'', ''%s'')',
      [StringReplace(PlanCache[I].SQL, '''', '''''', [rfReplaceAll]),
       StringReplace(PlanCache[I].PlanDetail, '''', '''''', [rfReplaceAll]),
       PlanCache[I].AnalyzedAt]));
  end;

  WriteLn;
  WriteLn(Format('   Plan cache entries: %d', [PlanCacheCount]));
  DS := Conn.ExecuteQuery('SELECT sql_text, plan_detail FROM query_plan_cache ORDER BY id LIMIT 3');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     SQL: %s', [Copy(DS.FieldByName('sql_text').AsString, 1, 55)]));
      WriteLn(Format('     Plan: %s', [Copy(DS.FieldByName('plan_detail').AsString, 1, 55)]));
      WriteLn('     ---');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 5: Index Impact Analysis
// ============================================================================

{ Measures query time and plan before and after creating an index, showing the
  performance improvement and plan change from full table scan to index use. }
procedure DemoIndexImpact;
var
  DS: TDataSet;
  TimeWithout, TimeWith: Double;
  PlanBefore, PlanAfter: string;
  StartTime: Int64;
  I: Integer;
begin
  WriteLn('=== 5. Index Impact Analysis ===');
  WriteLn;

  // Measure without index
  PlanBefore := GetQueryPlan('SELECT * FROM products WHERE category = ''Electronics'' AND price > 100');
  WriteLn('   Before index:');
  WriteLn('   Plan: ' + PlanBefore);

  StartTime := GetTimeMs;
  for I := 1 to 100 do
  begin
    DS := Conn.ExecuteQuery('SELECT * FROM products WHERE category = ''Electronics'' AND price > 100');
    DS.Free;
  end;
  TimeWithout := GetTimeMs - StartTime;
  WriteLn(Format('   100 executions: %.1f ms total', [TimeWithout]));

  // Create index
  Conn.ExecuteNonQuery('CREATE INDEX idx_products_category_price ON products(category, price)');
  WriteLn;
  WriteLn('   Created index: idx_products_category_price ON products(category, price)');

  // Clear plan cache entry for this query
  PlanCacheCount := PlanCacheCount - 1; // Remove last cached plan

  // Measure with index
  PlanAfter := GetQueryPlan('SELECT * FROM products WHERE category = ''Electronics'' AND price > 100');
  WriteLn;
  WriteLn('   After index:');
  WriteLn('   Plan: ' + PlanAfter);

  StartTime := GetTimeMs;
  for I := 1 to 100 do
  begin
    DS := Conn.ExecuteQuery('SELECT * FROM products WHERE category = ''Electronics'' AND price > 100');
    DS.Free;
  end;
  TimeWith := GetTimeMs - StartTime;
  WriteLn(Format('   100 executions: %.1f ms total', [TimeWith]));

  WriteLn;
  if (TimeWithout > 0) and (TimeWith > 0) then
    WriteLn(Format('   Improvement: %.1fx faster with index', [TimeWithout / TimeWith]))
  else
    WriteLn('   Both completed in under 1ms');

  // Additional index for orders
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_product_id ON orders(product_id)');
  WriteLn;
  WriteLn('   Created index: idx_orders_product_id ON orders(product_id)');

  PlanAfter := GetQueryPlan(
    'SELECT p.name, COUNT(o.id) FROM products p JOIN orders o ON o.product_id = p.id GROUP BY p.id');
  WriteLn('   JOIN plan with index: ' + Copy(PlanAfter, 1, 80));
  WriteLn;
end;

// ============================================================================
// Demo 6: Query Statistics Summary
// ============================================================================

{ Prints profiler totals (queries, slow queries, unique patterns, cached plans)
  and lists multi-execution queries with call count, total, avg, and max time. }
procedure DemoQueryStatistics;
var
  I: Integer;
  AvgMs: Double;
begin
  WriteLn('=== 6. Query Statistics Summary ===');
  WriteLn;

  WriteLn(Format('   Total profiled queries: %d', [ProfileCount]));
  WriteLn(Format('   Slow queries detected:  %d', [SlowQueryCount]));
  WriteLn(Format('   Unique query patterns:  %d', [StatsCount]));
  WriteLn(Format('   Cached query plans:     %d', [PlanCacheCount]));

  WriteLn;
  WriteLn('   Top queries by total time:');
  WriteLn(Format('   %-55s %5s %8s %8s %8s', ['SQL', 'Calls', 'Total', 'Avg', 'Max']));
  WriteLn('   ' + StringOfChar('-', 90));

  for I := 0 to StatsCount - 1 do
  begin
    if QueryStats[I].CallCount > 1 then
    begin
      AvgMs := QueryStats[I].TotalTimeMs / QueryStats[I].CallCount;
      WriteLn(Format('   %-55s %5d %6.1fms %6.1fms %6.1fms', [
        Copy(QueryStats[I].SQL, 1, 55),
        QueryStats[I].CallCount,
        QueryStats[I].TotalTimeMs,
        AvgMs,
        QueryStats[I].MaxTimeMs]));
    end;
  end;

  WriteLn;
  WriteLn('   Single-execution queries:');
  for I := 0 to StatsCount - 1 do
  begin
    if QueryStats[I].CallCount = 1 then
      WriteLn(Format('   %.1fms | %d rows | %s',
        [QueryStats[I].TotalTimeMs, QueryStats[I].TotalRows,
         Copy(QueryStats[I].SQL, 1, 60)]));
  end;
  WriteLn;
end;

// ============================================================================
// Demo 7: Profile Log Persistence
// ============================================================================

{ Inserts all in-memory profile entries into the query_log table, then queries
  the table to show summary statistics (total, slow count, avg, max time). }
procedure DemoProfilePersistence;
var
  I: Integer;
  DS: TDataSet;
begin
  WriteLn('=== 7. Profile Log Persistence ===');
  WriteLn;

  // Persist all profile entries to database
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 0 to ProfileCount - 1 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO query_log (sql_text, execution_ms, row_count, logged_at, is_slow) ' +
      'VALUES (''%s'', %.2f, %d, ''%s'', %d)',
      [StringReplace(ProfileLog[I].SQL, '''', '''''', [rfReplaceAll]),
       ProfileLog[I].ExecutionTimeMs, ProfileLog[I].RowCount,
       ProfileLog[I].Timestamp,
       Ord(ProfileLog[I].IsSlow)]));
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn(Format('   Persisted %d profile entries to query_log table', [ProfileCount]));

  // Query the persisted log
  WriteLn;
  WriteLn('   Stored log summary:');
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as total, ' +
    'SUM(CASE WHEN is_slow = 1 THEN 1 ELSE 0 END) as slow_count, ' +
    'AVG(execution_ms) as avg_ms, ' +
    'MAX(execution_ms) as max_ms ' +
    'FROM query_log');
  try
    WriteLn(Format('     Total entries:  %s', [DS.FieldByName('total').AsString]));
    WriteLn(Format('     Slow queries:   %s', [DS.FieldByName('slow_count').AsString]));
    WriteLn(Format('     Avg time:       %.2f ms', [DS.FieldByName('avg_ms').AsFloat]));
    WriteLn(Format('     Max time:       %.2f ms', [DS.FieldByName('max_ms').AsFloat]));
  finally
    DS.Free;
  end;

  // Query plan cache stats
  WriteLn;
  WriteLn(Format('   Query plan cache entries: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM query_plan_cache'))]));
  WriteLn;
end;

// ============================================================================
// Demo 8: Profiler Reset & Cleanup
// ============================================================================

{ Clears all in-memory profiler arrays (profile log, slow log, stats, plan
  cache) while retaining the persisted data in database tables. }
procedure DemoProfilerReset;
begin
  WriteLn('=== 8. Profiler Reset ===');
  WriteLn;

  WriteLn(Format('   Before reset: %d entries, %d slow, %d stats, %d plans',
    [ProfileCount, SlowQueryCount, StatsCount, PlanCacheCount]));

  // Reset in-memory profiler
  ProfileCount := 0;
  SlowQueryCount := 0;
  StatsCount := 0;
  PlanCacheCount := 0;

  WriteLn(Format('   After reset:  %d entries, %d slow, %d stats, %d plans',
    [ProfileCount, SlowQueryCount, StatsCount, PlanCacheCount]));
  WriteLn('   Note: Persisted data in query_log and query_plan_cache tables retained');
  WriteLn;
end;

// ============================================================================
// Main Program
// ============================================================================

begin
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateTestData;

    WriteLn('Example 111: Query Profiler - Timing, Slow Query Log, Query Plan Cache');
    WriteLn(StringOfChar('=', 75));
    WriteLn;

    DemoQueryTiming;
    DemoSlowQueryLog;
    DemoQueryPlanAnalysis;
    DemoQueryPlanCache;
    DemoIndexImpact;
    DemoQueryStatistics;
    DemoProfilePersistence;
    DemoProfilerReset;

    WriteLn('Done.');
    Conn.Close;
  finally
    Conn.Free;
  end;
end.
