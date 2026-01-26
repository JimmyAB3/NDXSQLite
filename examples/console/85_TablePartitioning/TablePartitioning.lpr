{===============================================================================
  NDXSQLite Example 85 - Table Partitioning
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates table partitioning patterns in SQLite:
  - Date-based partitioning (monthly tables)
  - Range-based partitioning (by value ranges)
  - List partitioning (by category/region)
  - Union ALL views across partitions
  - Insert routing to correct partition
  - Partition pruning (targeted queries)
  - Partition lifecycle (create, archive, drop)
  - Cross-partition aggregation
  - Performance comparison

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program TablePartitioning;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants, DateUtils,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

{ Executes a SQL query multiple times and returns the average duration in milliseconds. }
function TimeQuery(const SQL: string; Iterations: Integer = 1): Double;
var
  DS: TDataSet;
  Start: TDateTime;
  I: Integer;
begin
  Start := Now;
  for I := 1 to Iterations do
  begin
    DS := Conn.ExecuteQuery(SQL);
    try
      while not DS.EOF do
        DS.Next;
    finally
      DS.Free;
    end;
  end;
  Result := MilliSecondsBetween(Now, Start) / Iterations;
end;

// =============================================================================
// Demo: Date-Based Partitioning
// =============================================================================
{ Creates 12 monthly partition tables with CHECK constraints, a UNION ALL view, and inserts 6000 events routed by month. }
procedure DemoDatePartitioning;
var
  I, Month: Integer;
  TableName: string;
  DS: TDataSet;
begin
  WriteLn('1. Date-Based Partitioning (Monthly)');
  WriteLn('   ====================================');
  WriteLn('');

  // Create monthly partition tables
  WriteLn('   Creating monthly partition tables for 2024:');
  for Month := 1 to 12 do
  begin
    TableName := Format('events_2024_%s', [Format('%.2d', [Month])]);
    Conn.ExecuteNonQuery(
      'CREATE TABLE ' + TableName + ' (' +
      '  id INTEGER PRIMARY KEY,' +
      '  event_type TEXT NOT NULL,' +
      '  event_date TEXT NOT NULL,' +
      '  user_id INTEGER,' +
      '  payload TEXT,' +
      '  CHECK (event_date >= ''' + Format('2024-%s-01', [Format('%.2d', [Month])]) + '''' +
      '    AND event_date < ''' + Format('2024-%s-01', [Format('%.2d', [Month + 1])]) + ''')' +
      ')');
    Conn.ExecuteNonQuery('CREATE INDEX idx_' + TableName + '_date ON ' + TableName + '(event_date)');
    Conn.ExecuteNonQuery('CREATE INDEX idx_' + TableName + '_type ON ' + TableName + '(event_type)');
  end;
  WriteLn('   Created 12 monthly tables (events_2024_01 to events_2024_12)');

  // Create union view
  WriteLn('');
  WriteLn('   Creating UNION ALL view across all partitions:');
  Conn.ExecuteNonQuery(
    'CREATE VIEW events_all AS ' +
    'SELECT * FROM events_2024_01 UNION ALL ' +
    'SELECT * FROM events_2024_02 UNION ALL ' +
    'SELECT * FROM events_2024_03 UNION ALL ' +
    'SELECT * FROM events_2024_04 UNION ALL ' +
    'SELECT * FROM events_2024_05 UNION ALL ' +
    'SELECT * FROM events_2024_06 UNION ALL ' +
    'SELECT * FROM events_2024_07 UNION ALL ' +
    'SELECT * FROM events_2024_08 UNION ALL ' +
    'SELECT * FROM events_2024_09 UNION ALL ' +
    'SELECT * FROM events_2024_10 UNION ALL ' +
    'SELECT * FROM events_2024_11 UNION ALL ' +
    'SELECT * FROM events_2024_12');
  WriteLn('   Created view: events_all');

  // Insert data into correct partitions
  WriteLn('');
  WriteLn('   Inserting 6000 events (500/month) with routing:');
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 6000 do
  begin
    Month := ((I - 1) mod 12) + 1;
    TableName := Format('events_2024_%s', [Format('%.2d', [Month])]);
    Conn.ExecuteNonQuery(
      'INSERT INTO ' + TableName + ' (event_type, event_date, user_id, payload) VALUES (?, ?, ?, ?)',
      [IfThen(I mod 3 = 0, 'click', IfThen(I mod 3 = 1, 'view', 'purchase')),
       Format('2024-%s-%s', [Format('%.2d', [Month]), Format('%.2d', [(I mod 28) + 1])]),
       (I mod 100) + 1,
       Format('event_data_%d', [I])]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Inserted 6000 events across 12 partitions');

  // Show partition sizes
  WriteLn('');
  WriteLn('   Partition sizes:');
  WriteLn(Format('   %-20s | %s', ['Partition', 'Rows']));
  WriteLn('   ' + StringOfChar('-', 35));
  for Month := 1 to 12 do
  begin
    TableName := Format('events_2024_%s', [Format('%.2d', [Month])]);
    DS := Conn.ExecuteQuery('SELECT COUNT(*) AS cnt FROM ' + TableName);
    try
      WriteLn(Format('   %-20s | %s', [TableName, DS.FieldByName('cnt').AsString]));
    finally
      DS.Free;
    end;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Partition Pruning
// =============================================================================
{ Compares query timing between the UNION ALL view and direct partition access to show the benefit of partition pruning. }
procedure DemoPartitionPruning;
var
  TimeAll, TimePartition: Double;
  DS: TDataSet;
  CountAll, CountPartition: Integer;
begin
  WriteLn('2. Partition Pruning');
  WriteLn('   ====================');
  WriteLn('');

  // Query all partitions via view
  WriteLn('   Query via view (all partitions):');
  WriteLn('   SELECT COUNT(*) FROM events_all WHERE event_date LIKE ''2024-03%''');
  TimeAll := TimeQuery('SELECT COUNT(*) FROM events_all WHERE event_date LIKE ''2024-03%''', 10);
  CountAll := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM events_all WHERE event_date LIKE ''2024-03%'''));
  WriteLn(Format('   Result: %d rows, Avg time: %.2f ms', [CountAll, TimeAll]));

  // Query specific partition (pruning)
  WriteLn('');
  WriteLn('   Direct partition query (pruned):');
  WriteLn('   SELECT COUNT(*) FROM events_2024_03');
  TimePartition := TimeQuery('SELECT COUNT(*) FROM events_2024_03', 10);
  CountPartition := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM events_2024_03'));
  WriteLn(Format('   Result: %d rows, Avg time: %.2f ms', [CountPartition, TimePartition]));

  WriteLn('');
  WriteLn('   Pruning advantage: Query only 1 of 12 tables');
  WriteLn(Format('   View query:     %.2f ms (scans all 12 tables)', [TimeAll]));
  WriteLn(Format('   Direct query:   %.2f ms (scans 1 table)', [TimePartition]));

  // Cross-partition query (two months)
  WriteLn('');
  WriteLn('   Two-partition query (Q1 events):');
  TimePartition := TimeQuery(
    'SELECT COUNT(*) FROM events_2024_01 UNION ALL ' +
    'SELECT COUNT(*) FROM events_2024_02 UNION ALL ' +
    'SELECT COUNT(*) FROM events_2024_03', 10);
  WriteLn(Format('   Q1 query time: %.2f ms (3 partitions)', [TimePartition]));

  WriteLn('');
end;

// =============================================================================
// Demo: Range-Based Partitioning
// =============================================================================
{ Creates five order tables partitioned by amount range with CHECK constraints and inserts 3000 orders distributed across ranges. }
procedure DemoRangePartitioning;
var
  I, RangeIdx: Integer;
  TableName: string;
  DS: TDataSet;
  Ranges: array[0..4] of string;
begin
  WriteLn('3. Range-Based Partitioning (by Amount)');
  WriteLn('   ========================================');
  WriteLn('');

  Ranges[0] := 'small';     // 0-99
  Ranges[1] := 'medium';    // 100-499
  Ranges[2] := 'large';     // 500-999
  Ranges[3] := 'xlarge';    // 1000-4999
  Ranges[4] := 'premium';   // 5000+

  // Create range partition tables
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_small (id INTEGER PRIMARY KEY, customer TEXT, amount REAL, order_date TEXT, ' +
    'CHECK (amount >= 0 AND amount < 100))');
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_medium (id INTEGER PRIMARY KEY, customer TEXT, amount REAL, order_date TEXT, ' +
    'CHECK (amount >= 100 AND amount < 500))');
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_large (id INTEGER PRIMARY KEY, customer TEXT, amount REAL, order_date TEXT, ' +
    'CHECK (amount >= 500 AND amount < 1000))');
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_xlarge (id INTEGER PRIMARY KEY, customer TEXT, amount REAL, order_date TEXT, ' +
    'CHECK (amount >= 1000 AND amount < 5000))');
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_premium (id INTEGER PRIMARY KEY, customer TEXT, amount REAL, order_date TEXT, ' +
    'CHECK (amount >= 5000))');

  WriteLn('   Created 5 range partitions:');
  WriteLn('     orders_small:   amount [0, 100)');
  WriteLn('     orders_medium:  amount [100, 500)');
  WriteLn('     orders_large:   amount [500, 1000)');
  WriteLn('     orders_xlarge:  amount [1000, 5000)');
  WriteLn('     orders_premium: amount [5000+)');

  // Create union view
  Conn.ExecuteNonQuery(
    'CREATE VIEW orders_all AS ' +
    'SELECT * FROM orders_small UNION ALL ' +
    'SELECT * FROM orders_medium UNION ALL ' +
    'SELECT * FROM orders_large UNION ALL ' +
    'SELECT * FROM orders_xlarge UNION ALL ' +
    'SELECT * FROM orders_premium');

  // Insert with routing
  WriteLn('');
  WriteLn('   Inserting 3000 orders with amount-based routing:');
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 3000 do
  begin
    // Distribute amounts across ranges
    case I mod 10 of
      0, 1, 2: begin RangeIdx := 0; TableName := 'orders_small'; end;      // 30% small
      3, 4, 5: begin RangeIdx := 1; TableName := 'orders_medium'; end;     // 30% medium
      6, 7:    begin RangeIdx := 2; TableName := 'orders_large'; end;       // 20% large
      8:       begin RangeIdx := 3; TableName := 'orders_xlarge'; end;      // 10% xlarge
    else       begin RangeIdx := 4; TableName := 'orders_premium'; end;     // 10% premium
    end;

    Conn.ExecuteNonQuery(
      'INSERT INTO ' + TableName + ' (customer, amount, order_date) VALUES (?, ?, ?)',
      [Format('Customer_%d', [(I mod 50) + 1]),
       IfThen(RangeIdx = 0, IntToStr(10 + (I mod 89)),
         IfThen(RangeIdx = 1, IntToStr(100 + (I mod 399)),
           IfThen(RangeIdx = 2, IntToStr(500 + (I mod 499)),
             IfThen(RangeIdx = 3, IntToStr(1000 + (I mod 3999)),
               IntToStr(5000 + (I mod 5000)))))),
       Format('2024-%s-%s', [Format('%.2d', [(I mod 12) + 1]), Format('%.2d', [(I mod 28) + 1])])]);
  end;
  Conn.ExecuteNonQuery('COMMIT');

  // Show distribution
  WriteLn('');
  WriteLn('   Partition distribution:');
  WriteLn(Format('   %-20s | %-8s | %-12s | %s', ['Partition', 'Rows', 'Min Amount', 'Max Amount']));
  WriteLn('   ' + StringOfChar('-', 65));

  for I := 0 to 4 do
  begin
    TableName := 'orders_' + Ranges[I];
    DS := Conn.ExecuteQuery(Format(
      'SELECT COUNT(*) AS cnt, MIN(amount) AS mn, MAX(amount) AS mx FROM %s', [TableName]));
    try
      WriteLn(Format('   %-20s | %-8s | %-12s | %s',
        [TableName, DS.FieldByName('cnt').AsString,
         DS.FieldByName('mn').AsString, DS.FieldByName('mx').AsString]));
    finally
      DS.Free;
    end;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: List Partitioning
// =============================================================================
{ Creates four region-based sales tables, a UNION ALL view, and inserts 2000 sales routed by region. }
procedure DemoListPartitioning;
var
  I: Integer;
  Region, TableName: string;
  DS: TDataSet;
  Regions: array[0..3] of string;
begin
  WriteLn('4. List Partitioning (by Region)');
  WriteLn('   ================================');
  WriteLn('');

  Regions[0] := 'europe'; Regions[1] := 'americas';
  Regions[2] := 'asia'; Regions[3] := 'africa';

  // Create region partitions
  for I := 0 to 3 do
  begin
    TableName := 'sales_' + Regions[I];
    Conn.ExecuteNonQuery(
      'CREATE TABLE ' + TableName + ' (' +
      '  id INTEGER PRIMARY KEY,' +
      '  product TEXT NOT NULL,' +
      '  amount REAL NOT NULL,' +
      '  sale_date TEXT,' +
      '  region TEXT NOT NULL' +
      ')');
    Conn.ExecuteNonQuery('CREATE INDEX idx_' + TableName + '_date ON ' + TableName + '(sale_date)');
  end;

  WriteLn('   Created 4 region partitions: sales_europe, sales_americas, sales_asia, sales_africa');

  // Union view
  Conn.ExecuteNonQuery(
    'CREATE VIEW sales_all AS ' +
    'SELECT * FROM sales_europe UNION ALL ' +
    'SELECT * FROM sales_americas UNION ALL ' +
    'SELECT * FROM sales_asia UNION ALL ' +
    'SELECT * FROM sales_africa');

  // Insert with region routing
  WriteLn('   Inserting 2000 sales with region-based routing:');
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 2000 do
  begin
    Region := Regions[I mod 4];
    TableName := 'sales_' + Region;
    Conn.ExecuteNonQuery(
      'INSERT INTO ' + TableName + ' (product, amount, sale_date, region) VALUES (?, ?, ?, ?)',
      [Format('Product_%d', [(I mod 30) + 1]),
       50.0 + (I mod 200) * 5.0,
       Format('2024-%s-%s', [Format('%.2d', [(I mod 12) + 1]), Format('%.2d', [(I mod 28) + 1])]),
       Region]);
  end;
  Conn.ExecuteNonQuery('COMMIT');

  // Show distribution
  WriteLn('');
  WriteLn('   Region distribution:');
  WriteLn(Format('   %-20s | %-8s | %-12s | %s', ['Partition', 'Rows', 'Total Sales', 'Avg Sale']));
  WriteLn('   ' + StringOfChar('-', 65));

  for I := 0 to 3 do
  begin
    TableName := 'sales_' + Regions[I];
    DS := Conn.ExecuteQuery(Format(
      'SELECT COUNT(*) AS cnt, SUM(amount) AS total, AVG(amount) AS avg_amt FROM %s', [TableName]));
    try
      WriteLn(Format('   %-20s | %-8s | %-12.0f | %.2f',
        [TableName, DS.FieldByName('cnt').AsString,
         DS.FieldByName('total').AsFloat, DS.FieldByName('avg_amt').AsFloat]));
    finally
      DS.Free;
    end;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Partition Lifecycle
// =============================================================================
{ Archives a partition by renaming, creates a new period partition, drops the archived table, and rebuilds the UNION ALL view. }
procedure DemoPartitionLifecycle;
var
  DS: TDataSet;
  TableCount: Integer;
begin
  WriteLn('5. Partition Lifecycle Management');
  WriteLn('   =================================');
  WriteLn('');

  // Show current event partitions
  TableCount := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE type = ''table'' AND name LIKE ''events_2024_%'''));
  WriteLn(Format('   Current event partitions: %d', [TableCount]));

  // Archive old partition (rename)
  WriteLn('');
  WriteLn('   Archiving January partition (rename):');
  WriteLn('   ALTER TABLE events_2024_01 RENAME TO events_2024_01_archived');
  Conn.ExecuteNonQuery('ALTER TABLE events_2024_01 RENAME TO events_2024_01_archived');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) AS cnt FROM events_2024_01_archived');
  try
    WriteLn(Format('   Archived partition has %s rows', [DS.FieldByName('cnt').AsString]));
  finally
    DS.Free;
  end;

  // Create new partition for next period
  WriteLn('');
  WriteLn('   Creating partition for January 2025:');
  Conn.ExecuteNonQuery(
    'CREATE TABLE events_2025_01 (' +
    '  id INTEGER PRIMARY KEY,' +
    '  event_type TEXT NOT NULL,' +
    '  event_date TEXT NOT NULL,' +
    '  user_id INTEGER,' +
    '  payload TEXT,' +
    '  CHECK (event_date >= ''2025-01-01'' AND event_date < ''2025-02-01'')' +
    ')');
  Conn.ExecuteNonQuery('CREATE INDEX idx_events_2025_01_date ON events_2025_01(event_date)');
  WriteLn('   Created events_2025_01');

  // Insert some data
  Conn.ExecuteNonQuery(
    'INSERT INTO events_2025_01 (event_type, event_date, user_id, payload) VALUES (?, ?, ?, ?)',
    ['view', '2025-01-15', 42, 'new_year_event']);
  WriteLn('   Inserted test row into events_2025_01');

  // Drop old archived partition
  WriteLn('');
  WriteLn('   Dropping archived partition:');
  WriteLn('   DROP TABLE events_2024_01_archived');
  Conn.ExecuteNonQuery('DROP TABLE events_2024_01_archived');
  WriteLn('   Dropped events_2024_01_archived (data permanently removed)');

  // Update view to reflect changes
  WriteLn('');
  WriteLn('   Updating view to include new partition:');
  Conn.ExecuteNonQuery('DROP VIEW events_all');
  Conn.ExecuteNonQuery(
    'CREATE VIEW events_all AS ' +
    'SELECT * FROM events_2024_02 UNION ALL ' +
    'SELECT * FROM events_2024_03 UNION ALL ' +
    'SELECT * FROM events_2024_04 UNION ALL ' +
    'SELECT * FROM events_2024_05 UNION ALL ' +
    'SELECT * FROM events_2024_06 UNION ALL ' +
    'SELECT * FROM events_2024_07 UNION ALL ' +
    'SELECT * FROM events_2024_08 UNION ALL ' +
    'SELECT * FROM events_2024_09 UNION ALL ' +
    'SELECT * FROM events_2024_10 UNION ALL ' +
    'SELECT * FROM events_2024_11 UNION ALL ' +
    'SELECT * FROM events_2024_12 UNION ALL ' +
    'SELECT * FROM events_2025_01');
  WriteLn('   Updated events_all view (2024-02 to 2025-01)');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) AS cnt FROM events_all');
  try
    WriteLn(Format('   Total events in view: %s', [DS.FieldByName('cnt').AsString]));
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Cross-Partition Aggregation
// =============================================================================
{ Runs GROUP BY aggregations across event, sales, and order partitions via their UNION ALL views and displays totals per group. }
procedure DemoCrossPartitionAggregation;
var
  DS: TDataSet;
begin
  WriteLn('6. Cross-Partition Aggregation');
  WriteLn('   ==============================');
  WriteLn('');

  // Aggregate across all event partitions
  WriteLn('   Monthly event counts (from view):');
  WriteLn(Format('   %-12s | %s', ['Month', 'Events']));
  WriteLn('   ' + StringOfChar('-', 25));

  DS := Conn.ExecuteQuery(
    'SELECT substr(event_date, 1, 7) AS month, COUNT(*) AS cnt ' +
    'FROM events_all GROUP BY month ORDER BY month');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s | %s', [DS.FieldByName('month').AsString, DS.FieldByName('cnt').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Cross-region totals
  WriteLn('');
  WriteLn('   Cross-region sales totals:');
  DS := Conn.ExecuteQuery(
    'SELECT region, COUNT(*) AS orders, SUM(amount) AS total ' +
    'FROM sales_all GROUP BY region ORDER BY total DESC');
  try
    WriteLn(Format('   %-12s | %-8s | %s', ['Region', 'Orders', 'Total']));
    WriteLn('   ' + StringOfChar('-', 40));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s | %-8s | %.2f',
        [DS.FieldByName('region').AsString,
         DS.FieldByName('orders').AsString,
         DS.FieldByName('total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Range partition totals
  WriteLn('');
  WriteLn('   Order range distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT ''small (0-99)'' AS range_name, COUNT(*) AS cnt, SUM(amount) AS total FROM orders_small UNION ALL ' +
    'SELECT ''medium (100-499)'', COUNT(*), SUM(amount) FROM orders_medium UNION ALL ' +
    'SELECT ''large (500-999)'', COUNT(*), SUM(amount) FROM orders_large UNION ALL ' +
    'SELECT ''xlarge (1000-4999)'', COUNT(*), SUM(amount) FROM orders_xlarge UNION ALL ' +
    'SELECT ''premium (5000+)'', COUNT(*), SUM(amount) FROM orders_premium');
  try
    WriteLn(Format('   %-22s | %-8s | %s', ['Range', 'Orders', 'Total Amount']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s | %-8s | %.2f',
        [DS.FieldByName('range_name').AsString,
         DS.FieldByName('cnt').AsString,
         DS.FieldByName('total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Performance Comparison
// =============================================================================
{ Creates a single equivalent table and times full scans, month queries, aggregations, and range queries against both single-table and partitioned approaches. }
procedure DemoPerformanceComparison;
var
  I: Integer;
  TimeSingle, TimePartitioned: Double;
begin
  WriteLn('7. Performance Comparison');
  WriteLn('   =========================');
  WriteLn('');

  // Create equivalent single table for comparison
  Conn.ExecuteNonQuery(
    'CREATE TABLE events_single (' +
    '  id INTEGER PRIMARY KEY,' +
    '  event_type TEXT NOT NULL,' +
    '  event_date TEXT NOT NULL,' +
    '  user_id INTEGER,' +
    '  payload TEXT' +
    ')');
  Conn.ExecuteNonQuery('CREATE INDEX idx_events_single_date ON events_single(event_date)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_events_single_type ON events_single(event_type)');

  // Copy data from partitions (exclude id to avoid conflicts)
  Conn.ExecuteNonQuery('INSERT INTO events_single (event_type, event_date, user_id, payload) ' +
    'SELECT event_type, event_date, user_id, payload FROM events_all');
  WriteLn(Format('   Single table rows: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM events_single'))]));
  WriteLn(Format('   Partitioned total: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM events_all'))]));
  WriteLn('');

  // Test 1: Full scan comparison
  WriteLn('   Test 1: Count all events');
  TimeSingle := TimeQuery('SELECT COUNT(*) FROM events_single', 20);
  TimePartitioned := TimeQuery('SELECT COUNT(*) FROM events_all', 20);
  WriteLn(Format('     Single table:  %.2f ms', [TimeSingle]));
  WriteLn(Format('     All partitions: %.2f ms', [TimePartitioned]));

  // Test 2: Single month query
  WriteLn('');
  WriteLn('   Test 2: Query single month (March)');
  TimeSingle := TimeQuery(
    'SELECT COUNT(*) FROM events_single WHERE event_date >= ''2024-03-01'' AND event_date < ''2024-04-01''', 20);
  TimePartitioned := TimeQuery('SELECT COUNT(*) FROM events_2024_03', 20);
  WriteLn(Format('     Single table (WHERE clause): %.2f ms', [TimeSingle]));
  WriteLn(Format('     Direct partition:            %.2f ms', [TimePartitioned]));

  // Test 3: Aggregation
  WriteLn('');
  WriteLn('   Test 3: Group by event_type (full data)');
  TimeSingle := TimeQuery(
    'SELECT event_type, COUNT(*) FROM events_single GROUP BY event_type', 20);
  TimePartitioned := TimeQuery(
    'SELECT event_type, COUNT(*) FROM events_all GROUP BY event_type', 20);
  WriteLn(Format('     Single table:    %.2f ms', [TimeSingle]));
  WriteLn(Format('     Partitioned view: %.2f ms', [TimePartitioned]));

  // Test 4: Range scan with pruning
  WriteLn('');
  WriteLn('   Test 4: Q1 range query (Jan-Mar)');
  TimeSingle := TimeQuery(
    'SELECT COUNT(*) FROM events_single WHERE event_date >= ''2024-01-01'' AND event_date < ''2024-04-01''', 20);
  TimePartitioned := TimeQuery(
    'SELECT COUNT(*) FROM (' +
    'SELECT * FROM events_2024_02 UNION ALL ' +
    'SELECT * FROM events_2024_03)', 20);
  WriteLn(Format('     Single table:       %.2f ms', [TimeSingle]));
  WriteLn(Format('     2 partitions only:  %.2f ms', [TimePartitioned]));

  WriteLn('');
end;

// =============================================================================
// Demo: Partition Metadata
// =============================================================================
{ Creates a partition_registry table and populates it with metadata for all date, range, and list partitions. }
procedure DemoPartitionMetadata;
var
  DS: TDataSet;
begin
  WriteLn('8. Partition Metadata');
  WriteLn('   =====================');
  WriteLn('');

  // Create partition registry
  Conn.ExecuteNonQuery(
    'CREATE TABLE partition_registry (' +
    '  id INTEGER PRIMARY KEY,' +
    '  partition_name TEXT NOT NULL,' +
    '  partition_type TEXT NOT NULL,' +
    '  table_name TEXT NOT NULL,' +
    '  range_start TEXT,' +
    '  range_end TEXT,' +
    '  status TEXT DEFAULT ''active'',' +
    '  created_date TEXT DEFAULT (date(''now'')),' +
    '  row_count INTEGER DEFAULT 0' +
    ')');

  // Register date partitions
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  Conn.ExecuteNonQuery(
    'INSERT INTO partition_registry (partition_name, partition_type, table_name, range_start, range_end, row_count) VALUES ' +
    '(''events_2024_02'', ''date'', ''events'', ''2024-02-01'', ''2024-03-01'', 500)');
  Conn.ExecuteNonQuery(
    'INSERT INTO partition_registry (partition_name, partition_type, table_name, range_start, range_end, row_count) VALUES ' +
    '(''events_2024_03'', ''date'', ''events'', ''2024-03-01'', ''2024-04-01'', 500)');
  Conn.ExecuteNonQuery(
    'INSERT INTO partition_registry (partition_name, partition_type, table_name, range_start, range_end, row_count) VALUES ' +
    '(''events_2025_01'', ''date'', ''events'', ''2025-01-01'', ''2025-02-01'', 1)');

  // Register range partitions
  Conn.ExecuteNonQuery(
    'INSERT INTO partition_registry (partition_name, partition_type, table_name, range_start, range_end, row_count) VALUES ' +
    '(''orders_small'', ''range'', ''orders'', ''0'', ''100'', 900)');
  Conn.ExecuteNonQuery(
    'INSERT INTO partition_registry (partition_name, partition_type, table_name, range_start, range_end, row_count) VALUES ' +
    '(''orders_premium'', ''range'', ''orders'', ''5000'', ''inf'', 300)');

  // Register list partitions
  Conn.ExecuteNonQuery(
    'INSERT INTO partition_registry (partition_name, partition_type, table_name, range_start, range_end, row_count) VALUES ' +
    '(''sales_europe'', ''list'', ''sales'', ''europe'', ''europe'', 500)');
  Conn.ExecuteNonQuery(
    'INSERT INTO partition_registry (partition_name, partition_type, table_name, range_start, range_end, row_count) VALUES ' +
    '(''sales_americas'', ''list'', ''sales'', ''americas'', ''americas'', 500)');
  Conn.ExecuteNonQuery('COMMIT');

  // Display registry
  WriteLn('   Partition Registry:');
  WriteLn(Format('   %-22s | %-6s | %-10s | %-12s | %-12s | %s',
    ['Partition', 'Type', 'Table', 'Start', 'End', 'Rows']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery('SELECT * FROM partition_registry ORDER BY partition_type, partition_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s | %-6s | %-10s | %-12s | %-12s | %s',
        [DS.FieldByName('partition_name').AsString,
         DS.FieldByName('partition_type').AsString,
         DS.FieldByName('table_name').AsString,
         DS.FieldByName('range_start').AsString,
         DS.FieldByName('range_end').AsString,
         DS.FieldByName('row_count').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Partition Summary
// =============================================================================
{ Displays a summary of created partitions, views, and indexes. }
procedure DemoSummary;
var
  DS: TDataSet;
  TableCount, ViewCount, IndexCount: Integer;
begin
  WriteLn('9. Partitioning Summary');
  WriteLn('   ========================');
  WriteLn('');

  TableCount := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE type = ''table'' AND name != ''partition_registry'''));
  ViewCount := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE type = ''view'''));
  IndexCount := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE type = ''index'' AND sql IS NOT NULL'));

  WriteLn(Format('   Total partition tables: %d', [TableCount]));
  WriteLn(Format('   Union views:            %d', [ViewCount]));
  WriteLn(Format('   Partition indexes:      %d', [IndexCount]));
  WriteLn('');

  WriteLn('   Partition types used:');
  WriteLn('     - Date-based:  12 monthly tables (events_2024_*)');
  WriteLn('     - Range-based: 5 amount range tables (orders_*)');
  WriteLn('     - List-based:  4 region tables (sales_*)');
  WriteLn('     - Total:       1 new period (events_2025_01)');
  WriteLn('');

  WriteLn('   All tables in database:');
  DS := Conn.ExecuteQuery(
    'SELECT name, type FROM sqlite_master WHERE type IN (''table'', ''view'') ORDER BY type, name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s', [DS.FieldByName('type').AsString, DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 85: Table Partitioning ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    DemoDatePartitioning;
    DemoPartitionPruning;
    DemoRangePartitioning;
    DemoListPartitioning;
    DemoPartitionLifecycle;
    DemoCrossPartitionAggregation;
    DemoPerformanceComparison;
    DemoPartitionMetadata;
    DemoSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
