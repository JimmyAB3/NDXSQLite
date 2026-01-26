{===============================================================================
  NDXSQLite Example 113 - Data Archiving
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Hot/cold storage tier management
  - Partition rotation strategies
  - Compressed archive storage
  - Archive policy configuration
  - Restore and purge operations

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataArchiving;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

{
  Example 113: Data Archiving
  - Hot/cold storage (active vs archived data)
  - Partition rotation (time-based table partitioning)
  - Compressed archives (aggregated/summarized storage)
  - Archive/restore operations
  - Data lifecycle policies (age-based archival and purge)
  - Archive statistics and space analysis
}

var
  Conn: TNDXSQLiteConnection;

// ============================================================================
// Schema Setup
// ============================================================================

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Hot storage: active orders (current data)
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_hot (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  customer TEXT NOT NULL,' +
    '  product TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  order_date TEXT NOT NULL,' +
    '  region TEXT NOT NULL' +
    ')');

  // Cold storage: archived orders (historical, less accessed)
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_cold (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer TEXT NOT NULL,' +
    '  product TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  status TEXT NOT NULL,' +
    '  order_date TEXT NOT NULL,' +
    '  region TEXT NOT NULL,' +
    '  archived_at TEXT NOT NULL' +
    ')');

  // Compressed archive: aggregated monthly summaries
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_archive (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  period TEXT NOT NULL,' +        // YYYY-MM
    '  region TEXT NOT NULL,' +
    '  customer_count INTEGER NOT NULL,' +
    '  order_count INTEGER NOT NULL,' +
    '  total_quantity INTEGER NOT NULL,' +
    '  total_amount REAL NOT NULL,' +
    '  avg_amount REAL NOT NULL,' +
    '  min_amount REAL NOT NULL,' +
    '  max_amount REAL NOT NULL,' +
    '  compressed_at TEXT NOT NULL,' +
    '  UNIQUE(period, region)' +
    ')');

  // Archive policies
  Conn.ExecuteNonQuery(
    'CREATE TABLE archive_policies (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  policy_name TEXT NOT NULL UNIQUE,' +
    '  source_table TEXT NOT NULL,' +
    '  hot_retention_days INTEGER NOT NULL,' +   // days in hot before moving to cold
    '  cold_retention_days INTEGER NOT NULL,' +  // days in cold before compressing
    '  purge_after_days INTEGER NOT NULL,' +     // days before permanent deletion
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Archive log
  Conn.ExecuteNonQuery(
    'CREATE TABLE archive_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  operation TEXT NOT NULL,' +     // hot_to_cold, cold_to_archive, purge, restore
    '  source_table TEXT NOT NULL,' +
    '  target_table TEXT NOT NULL,' +
    '  records_affected INTEGER NOT NULL,' +
    '  executed_at TEXT NOT NULL' +
    ')');

  // Partitioned tables (monthly)
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_2024_01 (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer TEXT, product TEXT, quantity INTEGER, amount REAL, ' +
    '  status TEXT, order_date TEXT, region TEXT)');
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_2024_02 (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer TEXT, product TEXT, quantity INTEGER, amount REAL, ' +
    '  status TEXT, order_date TEXT, region TEXT)');
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_2024_03 (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer TEXT, product TEXT, quantity INTEGER, amount REAL, ' +
    '  status TEXT, order_date TEXT, region TEXT)');
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders_2024_04 (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer TEXT, product TEXT, quantity INTEGER, amount REAL, ' +
    '  status TEXT, order_date TEXT, region TEXT)');
end;

// ============================================================================
// Test Data
// ============================================================================

{ Inserts test data for demonstrations. }
procedure InsertTestData;
var
  I: Integer;
  Regions: array[0..4] of string;
  Products: array[0..7] of string;
  Statuses: array[0..2] of string;
begin
  Regions[0] := 'North'; Regions[1] := 'South'; Regions[2] := 'East';
  Regions[3] := 'West'; Regions[4] := 'Central';
  Products[0] := 'Widget'; Products[1] := 'Gadget'; Products[2] := 'Sprocket';
  Products[3] := 'Bolt'; Products[4] := 'Nut'; Products[5] := 'Washer';
  Products[6] := 'Gear'; Products[7] := 'Spring';
  Statuses[0] := 'completed'; Statuses[1] := 'completed'; Statuses[2] := 'shipped';

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Insert 2000 orders spanning 4 months
  for I := 1 to 2000 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO orders_hot (customer, product, quantity, amount, status, order_date, region) ' +
      'VALUES (''Customer_%d'', ''%s'', %d, %.2f, ''%s'', ''2024-%.2d-%.2d'', ''%s'')',
      [(I mod 100) + 1, Products[I mod 8], (I mod 10) + 1,
       ((I mod 50) + 1) * 29.99, Statuses[I mod 3],
       (I mod 4) + 1, (I mod 28) + 1, Regions[I mod 5]]));
  end;

  Conn.ExecuteNonQuery('COMMIT');

  // Set up archive policy
  Conn.ExecuteNonQuery(
    'INSERT INTO archive_policies (policy_name, source_table, hot_retention_days, cold_retention_days, purge_after_days) ' +
    'VALUES (''orders_monthly'', ''orders_hot'', 30, 90, 365)');
end;

// ============================================================================
// Demo 1: Hot/Cold Storage
// ============================================================================

{ Moves January and February orders from hot to cold storage, logging each
  operation and printing record counts before and after each transfer. }
procedure DemoHotColdStorage;
var
  HotCount, ColdCount: Integer;
  Moved: Integer;
begin
  WriteLn('=== 1. Hot/Cold Storage ===');
  WriteLn;

  HotCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_hot'));
  ColdCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Initial state: Hot=%d, Cold=%d', [HotCount, ColdCount]));

  // Move January orders to cold storage (older than 30 days)
  WriteLn('   Archiving January orders to cold storage...');
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  Conn.ExecuteNonQuery(
    'INSERT INTO orders_cold (id, customer, product, quantity, amount, status, order_date, region, archived_at) ' +
    'SELECT id, customer, product, quantity, amount, status, order_date, region, datetime(''now'') ' +
    'FROM orders_hot WHERE order_date LIKE ''2024-01-%''');
  Moved := Integer(Conn.ExecuteScalar('SELECT changes()'));

  Conn.ExecuteNonQuery('DELETE FROM orders_hot WHERE order_date LIKE ''2024-01-%''');

  // Log the operation
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO archive_log (operation, source_table, target_table, records_affected, executed_at) ' +
    'VALUES (''hot_to_cold'', ''orders_hot'', ''orders_cold'', %d, datetime(''now''))', [Moved]));

  Conn.ExecuteNonQuery('COMMIT');

  HotCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_hot'));
  ColdCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Moved %d records to cold storage', [Moved]));
  WriteLn(Format('   After archival: Hot=%d, Cold=%d', [HotCount, ColdCount]));

  // Move February orders too
  WriteLn;
  WriteLn('   Archiving February orders to cold storage...');
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  Conn.ExecuteNonQuery(
    'INSERT INTO orders_cold (id, customer, product, quantity, amount, status, order_date, region, archived_at) ' +
    'SELECT id, customer, product, quantity, amount, status, order_date, region, datetime(''now'') ' +
    'FROM orders_hot WHERE order_date LIKE ''2024-02-%''');
  Moved := Integer(Conn.ExecuteScalar('SELECT changes()'));

  Conn.ExecuteNonQuery('DELETE FROM orders_hot WHERE order_date LIKE ''2024-02-%''');

  Conn.ExecuteNonQuery(Format(
    'INSERT INTO archive_log (operation, source_table, target_table, records_affected, executed_at) ' +
    'VALUES (''hot_to_cold'', ''orders_hot'', ''orders_cold'', %d, datetime(''now''))', [Moved]));

  Conn.ExecuteNonQuery('COMMIT');

  HotCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_hot'));
  ColdCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Moved %d records to cold storage', [Moved]));
  WriteLn(Format('   After archival: Hot=%d, Cold=%d', [HotCount, ColdCount]));
  WriteLn;
end;

// ============================================================================
// Demo 2: Partition Rotation
// ============================================================================

{ Distributes cold storage records into monthly partition tables, prints row
  counts per partition, then drops the oldest partition to illustrate rotation. }
procedure DemoPartitionRotation;
var
  DS: TDataSet;
  I: Integer;
  PartName: string;
  RowCount: Integer;
begin
  WriteLn('=== 2. Partition Rotation (Monthly Tables) ===');
  WriteLn;

  // Distribute cold data into monthly partitions
  WriteLn('   Distributing cold data into monthly partitions...');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  Conn.ExecuteNonQuery(
    'INSERT INTO orders_2024_01 (id, customer, product, quantity, amount, status, order_date, region) ' +
    'SELECT id, customer, product, quantity, amount, status, order_date, region ' +
    'FROM orders_cold WHERE order_date LIKE ''2024-01-%''');

  Conn.ExecuteNonQuery(
    'INSERT INTO orders_2024_02 (id, customer, product, quantity, amount, status, order_date, region) ' +
    'SELECT id, customer, product, quantity, amount, status, order_date, region ' +
    'FROM orders_cold WHERE order_date LIKE ''2024-02-%''');

  Conn.ExecuteNonQuery('COMMIT');

  // Show partition contents
  WriteLn('   Partition contents:');
  for I := 1 to 4 do
  begin
    PartName := Format('orders_2024_%.2d', [I]);
    RowCount := Integer(Conn.ExecuteScalar(Format('SELECT COUNT(*) FROM "%s"', [PartName])));
    WriteLn(Format('     %s: %d rows', [PartName, RowCount]));
  end;

  // Show partition that can be rotated (dropped)
  WriteLn;
  WriteLn('   Partition rotation: dropping oldest partition (orders_2024_01)...');
  RowCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_2024_01'));
  WriteLn(Format('   Dropping %d rows from orders_2024_01', [RowCount]));
  Conn.ExecuteNonQuery('DROP TABLE orders_2024_01');

  // Verify remaining partitions
  WriteLn;
  WriteLn('   Remaining partitions:');
  DS := Conn.ExecuteQuery(
    'SELECT name FROM sqlite_master WHERE type = ''table'' AND name LIKE ''orders_2024_%'' ORDER BY name');
  try
    while not DS.EOF do
    begin
      PartName := DS.FieldByName('name').AsString;
      RowCount := Integer(Conn.ExecuteScalar(Format('SELECT COUNT(*) FROM "%s"', [PartName])));
      WriteLn(Format('     %s: %d rows', [PartName, RowCount]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 3: Compressed Archives
// ============================================================================

{ Aggregates cold storage data into monthly/regional summaries with order count,
  totals, and averages, printing the compression ratio and summary table. }
procedure DemoCompressedArchives;
var
  DS: TDataSet;
  ColdCount, ArchiveCount: Integer;
begin
  WriteLn('=== 3. Compressed Archives (Aggregated Summaries) ===');
  WriteLn;

  ColdCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Cold storage: %d individual records', [ColdCount]));

  // Compress cold data into monthly/regional aggregates
  WriteLn('   Compressing into monthly regional summaries...');

  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO orders_archive (period, region, customer_count, order_count, ' +
    'total_quantity, total_amount, avg_amount, min_amount, max_amount, compressed_at) ' +
    'SELECT substr(order_date, 1, 7), region, ' +
    'COUNT(DISTINCT customer), COUNT(*), SUM(quantity), SUM(amount), ' +
    'AVG(amount), MIN(amount), MAX(amount), datetime(''now'') ' +
    'FROM orders_cold GROUP BY substr(order_date, 1, 7), region');

  ArchiveCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_archive'));
  WriteLn(Format('   Archive entries: %d summary records', [ArchiveCount]));
  WriteLn(Format('   Compression ratio: %d:1 (records)', [ColdCount div ArchiveCount]));

  // Show archive contents
  WriteLn;
  WriteLn(Format('   %-7s %-8s %6s %6s %10s %10s',
    ['Period', 'Region', 'Orders', 'Custs', 'Total $', 'Avg $']));
  WriteLn('   ' + StringOfChar('-', 55));

  DS := Conn.ExecuteQuery(
    'SELECT period, region, order_count, customer_count, total_amount, avg_amount ' +
    'FROM orders_archive ORDER BY period, region');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-7s %-8s %6d %6d %10.2f %10.2f', [
        DS.FieldByName('period').AsString,
        DS.FieldByName('region').AsString,
        DS.FieldByName('order_count').AsInteger,
        DS.FieldByName('customer_count').AsInteger,
        DS.FieldByName('total_amount').AsFloat,
        DS.FieldByName('avg_amount').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Log compression operation
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO archive_log (operation, source_table, target_table, records_affected, executed_at) ' +
    'VALUES (''cold_to_archive'', ''orders_cold'', ''orders_archive'', %d, datetime(''now''))', [ColdCount]));

  WriteLn;
end;

// ============================================================================
// Demo 4: Archive Policies
// ============================================================================

{ Inserts additional archive policies (quarterly and audit log), then lists all
  policies showing retention periods for hot, cold, and purge stages. }
procedure DemoArchivePolicies;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Archive Policies ===');
  WriteLn;

  // Add more policies
  Conn.ExecuteNonQuery(
    'INSERT OR IGNORE INTO archive_policies (policy_name, source_table, hot_retention_days, cold_retention_days, purge_after_days) ' +
    'VALUES (''orders_quarterly'', ''orders_hot'', 90, 180, 730)');
  Conn.ExecuteNonQuery(
    'INSERT OR IGNORE INTO archive_policies (policy_name, source_table, hot_retention_days, cold_retention_days, purge_after_days, is_active) ' +
    'VALUES (''audit_log'', ''audit_log'', 7, 30, 90, 1)');

  // Show policies
  WriteLn(Format('   %-20s %-12s %5s %5s %5s %s',
    ['Policy', 'Table', 'Hot', 'Cold', 'Purge', 'Active']));
  WriteLn('   ' + StringOfChar('-', 65));

  DS := Conn.ExecuteQuery('SELECT * FROM archive_policies ORDER BY policy_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-12s %4dd %4dd %4dd %s', [
        DS.FieldByName('policy_name').AsString,
        DS.FieldByName('source_table').AsString,
        DS.FieldByName('hot_retention_days').AsInteger,
        DS.FieldByName('cold_retention_days').AsInteger,
        DS.FieldByName('purge_after_days').AsInteger,
        BoolToStr(DS.FieldByName('is_active').AsInteger = 1, 'YES', 'NO')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Data lifecycle:');
  WriteLn('     HOT (active) -> COLD (archived) -> COMPRESSED -> PURGED');
  WriteLn('     orders_monthly: 30d hot -> 90d cold -> compressed -> 365d purge');
  WriteLn;
end;

// ============================================================================
// Demo 5: Restore from Archive
// ============================================================================

{ Demonstrates restoring archived records from cold back to hot storage. }
procedure DemoRestore;
var
  HotBefore, HotAfter, ColdBefore, ColdAfter: Integer;
  Restored: Integer;
begin
  WriteLn('=== 5. Restore from Cold Storage ===');
  WriteLn;

  HotBefore := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_hot'));
  ColdBefore := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Before restore: Hot=%d, Cold=%d', [HotBefore, ColdBefore]));

  // Restore February orders back to hot (e.g., needed for audit)
  WriteLn('   Restoring February orders back to hot storage...');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  Conn.ExecuteNonQuery(
    'INSERT INTO orders_hot (id, customer, product, quantity, amount, status, order_date, region) ' +
    'SELECT id, customer, product, quantity, amount, status, order_date, region ' +
    'FROM orders_cold WHERE order_date LIKE ''2024-02-%''');
  Restored := Integer(Conn.ExecuteScalar('SELECT changes()'));

  Conn.ExecuteNonQuery('DELETE FROM orders_cold WHERE order_date LIKE ''2024-02-%''');

  Conn.ExecuteNonQuery(Format(
    'INSERT INTO archive_log (operation, source_table, target_table, records_affected, executed_at) ' +
    'VALUES (''restore'', ''orders_cold'', ''orders_hot'', %d, datetime(''now''))', [Restored]));

  Conn.ExecuteNonQuery('COMMIT');

  HotAfter := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_hot'));
  ColdAfter := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Restored %d records from cold to hot', [Restored]));
  WriteLn(Format('   After restore: Hot=%d, Cold=%d', [HotAfter, ColdAfter]));
  WriteLn;
end;

// ============================================================================
// Demo 6: Data Purge
// ============================================================================

{ Demonstrates permanent purge of aged records from cold storage. }
procedure DemoPurge;
var
  ColdBefore, ColdAfter: Integer;
  Purged: Integer;
begin
  WriteLn('=== 6. Data Purge (Permanent Deletion) ===');
  WriteLn;

  ColdBefore := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Cold storage before purge: %d records', [ColdBefore]));

  // Purge January data (beyond retention period)
  WriteLn('   Purging January data (beyond 365-day retention)...');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  Purged := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM orders_cold WHERE order_date LIKE ''2024-01-%'''));

  Conn.ExecuteNonQuery('DELETE FROM orders_cold WHERE order_date LIKE ''2024-01-%''');

  Conn.ExecuteNonQuery(Format(
    'INSERT INTO archive_log (operation, source_table, target_table, records_affected, executed_at) ' +
    'VALUES (''purge'', ''orders_cold'', ''(deleted)'', %d, datetime(''now''))', [Purged]));

  Conn.ExecuteNonQuery('COMMIT');

  ColdAfter := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  WriteLn(Format('   Purged %d records permanently', [Purged]));
  WriteLn(Format('   Cold storage after purge: %d records', [ColdAfter]));
  WriteLn('   Note: Compressed archive still retains aggregated data');

  // Verify archive still has the summary
  WriteLn(Format('   Archive entries for 2024-01: %s',
    [VarToStr(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM orders_archive WHERE period = ''2024-01'''))]));
  WriteLn;
end;

// ============================================================================
// Demo 7: Archive Log & Audit Trail
// ============================================================================

{ Prints the full archive_log table showing each operation type, source/target
  tables, record counts, and timestamps, plus summary totals. }
procedure DemoArchiveLog;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Archive Log (Audit Trail) ===');
  WriteLn;

  WriteLn(Format('   %-12s %-15s %-15s %8s %s',
    ['Operation', 'Source', 'Target', 'Records', 'Timestamp']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery('SELECT * FROM archive_log ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-15s %-15s %8d %s', [
        DS.FieldByName('operation').AsString,
        DS.FieldByName('source_table').AsString,
        DS.FieldByName('target_table').AsString,
        DS.FieldByName('records_affected').AsInteger,
        DS.FieldByName('executed_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total archive operations: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM archive_log'))]));
  WriteLn(Format('   Total records processed: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT SUM(records_affected) FROM archive_log'))]));
  WriteLn;
end;

// ============================================================================
// Demo 8: Statistics Summary
// ============================================================================

{ Demonstrates archiving statistics across hot and cold storage tiers. }
procedure DemoStatistics;
var
  HotCount, ColdCount, ArchiveCount: Integer;
  HotTotal, ArchiveTotal: Double;
begin
  WriteLn('=== 8. Archiving Statistics ===');
  WriteLn;

  HotCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_hot'));
  ColdCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_cold'));
  ArchiveCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders_archive'));

  WriteLn('   Storage Tier Summary:');
  WriteLn(Format('     Hot (orders_hot):       %5d records (active, fully queryable)', [HotCount]));
  WriteLn(Format('     Cold (orders_cold):     %5d records (archived, queryable)', [ColdCount]));
  WriteLn(Format('     Archive (compressed):   %5d records (aggregated summaries)', [ArchiveCount]));
  WriteLn(Format('     Total tracked:          %5d', [HotCount + ColdCount + ArchiveCount]));

  WriteLn;
  WriteLn('   Amount Totals by Tier:');
  HotTotal := Double(Conn.ExecuteScalar('SELECT COALESCE(SUM(amount), 0) FROM orders_hot'));
  ArchiveTotal := Double(Conn.ExecuteScalar('SELECT COALESCE(SUM(total_amount), 0) FROM orders_archive'));
  WriteLn(Format('     Hot tier total:         $%.2f', [HotTotal]));
  WriteLn(Format('     Archive tier total:     $%.2f', [ArchiveTotal]));

  WriteLn;
  WriteLn('   Space Analysis:');
  WriteLn(Format('     Hot records (full data): ~%d bytes estimated', [HotCount * 80]));
  WriteLn(Format('     Cold records (full):     ~%d bytes estimated', [ColdCount * 85]));
  WriteLn(Format('     Archive records (agg):   ~%d bytes estimated', [ArchiveCount * 100]));
  if ColdCount + HotCount > 0 then
    WriteLn(Format('     Archive efficiency:      %d raw records -> %d summaries',
      [ColdCount + Integer(Conn.ExecuteScalar(
        'SELECT COALESCE(SUM(order_count), 0) FROM orders_archive')), ArchiveCount]));

  WriteLn;
  WriteLn('   Archive Operations Summary:');
  WriteLn(Format('     hot_to_cold:  %s operations',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM archive_log WHERE operation = ''hot_to_cold'''))]));
  WriteLn(Format('     cold_to_arch: %s operations',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM archive_log WHERE operation = ''cold_to_archive'''))]));
  WriteLn(Format('     restore:      %s operations',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM archive_log WHERE operation = ''restore'''))]));
  WriteLn(Format('     purge:        %s operations',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM archive_log WHERE operation = ''purge'''))]));
  WriteLn;
end;

// ============================================================================
// Main Program
// ============================================================================

begin
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertTestData;

    WriteLn('Example 113: Data Archiving - Hot/Cold Storage, Partition Rotation, Compressed Archives');
    WriteLn(StringOfChar('=', 80));
    WriteLn;

    DemoHotColdStorage;
    DemoPartitionRotation;
    DemoCompressedArchives;
    DemoArchivePolicies;
    DemoRestore;
    DemoPurge;
    DemoArchiveLog;
    DemoStatistics;

    WriteLn('Done.');
    Conn.Close;
  finally
    Conn.Free;
  end;
end.
