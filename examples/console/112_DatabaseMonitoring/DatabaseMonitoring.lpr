{===============================================================================
  NDXSQLite Example 112 - Database Monitoring
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Database size and fragmentation metrics
  - Table and index statistics
  - Connection tracking and management
  - Cache configuration monitoring
  - Health check dashboards

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DatabaseMonitoring;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

{
  Example 112: Database Monitoring
  - Database size metrics (page_count, page_size, file size)
  - Fragmentation analysis (freelist_count, auto_vacuum)
  - Table/index statistics (row counts, schema info)
  - Cache configuration and monitoring
  - Connection lifecycle tracking
  - PRAGMA-based health checks
  - Storage analysis per table
}

var
  Conn: TNDXSQLiteConnection;
  DbPath: string;

// ============================================================================
// Monitoring Infrastructure
// ============================================================================

const
  MAX_CONNECTIONS = 10;

type
  TConnectionInfo = record
    Id: Integer;
    OpenedAt: string;
    ClosedAt: string;
    Status: string; // open, closed
    QueryCount: Integer;
  end;

  THealthMetric = record
    Name: string;
    Value: string;
    Status: string; // ok, warning, critical
    Threshold: string;
  end;

var
  Connections: array[0..MAX_CONNECTIONS-1] of TConnectionInfo;
  ConnectionCount: Integer = 0;
  NextConnId: Integer = 1;

{ Adds a new connection entry to the tracking array with current timestamp and
  open status, returning the assigned connection ID. }
function RegisterConnection: Integer;
begin
  if ConnectionCount < MAX_CONNECTIONS then
  begin
    Connections[ConnectionCount].Id := NextConnId;
    Connections[ConnectionCount].OpenedAt := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    Connections[ConnectionCount].ClosedAt := '';
    Connections[ConnectionCount].Status := 'open';
    Connections[ConnectionCount].QueryCount := 0;
    Inc(ConnectionCount);
    Result := NextConnId;
    Inc(NextConnId);
  end
  else
    Result := -1;
end;

{ Finds the connection with the given ID and marks it as closed with the
  current timestamp. }
procedure CloseConnection(Id: Integer);
var
  I: Integer;
begin
  for I := 0 to ConnectionCount - 1 do
    if (Connections[I].Id = Id) and (Connections[I].Status = 'open') then
    begin
      Connections[I].ClosedAt := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
      Connections[I].Status := 'closed';
      Break;
    end;
end;

{ Returns the open connection count. }
function GetOpenConnectionCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ConnectionCount - 1 do
    if Connections[I].Status = 'open' then
      Inc(Result);
end;

// ============================================================================
// Test Data Setup
// ============================================================================

{ Inserts test data for demonstrations. }
procedure CreateTestData;
var
  I: Integer;
begin
  // Create tables with various types
  Conn.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT UNIQUE,' +
    '  city TEXT,' +
    '  balance REAL DEFAULT 0,' +
    '  created_at TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL DEFAULT 0,' +
    '  description TEXT,' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  customer_id INTEGER REFERENCES customers(id),' +
    '  product_id INTEGER REFERENCES products(id),' +
    '  quantity INTEGER NOT NULL,' +
    '  total REAL NOT NULL,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  order_date TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE audit_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  table_name TEXT NOT NULL,' +
    '  action TEXT NOT NULL,' +
    '  record_id INTEGER,' +
    '  details TEXT,' +
    '  logged_at TEXT NOT NULL' +
    ')');

  // Create indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_customers_city ON customers(city)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_products_category ON products(category)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_customer ON orders(customer_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_product ON orders(product_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_date ON orders(order_date)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_audit_table ON audit_log(table_name)');

  // Insert data
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  for I := 1 to 500 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO customers (name, email, city, balance, created_at) ' +
      'VALUES (''Customer_%d'', ''cust%d@example.com'', ''City_%d'', %.2f, ''2024-%s-%.2d'')',
      [I, I, (I mod 20) + 1, I * 50.0, Format('%.2d', [(I mod 12) + 1]), (I mod 28) + 1]));

  for I := 1 to 200 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO products (name, category, price, stock, description, is_active) ' +
      'VALUES (''Product_%d'', ''Cat_%d'', %.2f, %d, ''Description for product %d with some text'', %d)',
      [I, (I mod 10) + 1, I * 12.99, I * 5, I, Ord(I mod 5 <> 0)]));

  for I := 1 to 3000 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO orders (customer_id, product_id, quantity, total, status, order_date) ' +
      'VALUES (%d, %d, %d, %.2f, ''%s'', ''2024-%s-%.2d'')',
      [(I mod 500) + 1, (I mod 200) + 1, (I mod 10) + 1, ((I mod 100) + 1) * 25.0,
       'completed', Format('%.2d', [(I mod 12) + 1]), (I mod 28) + 1]));

  for I := 1 to 1000 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO audit_log (table_name, action, record_id, details, logged_at) ' +
      'VALUES (''%s'', ''%s'', %d, ''Change details for record %d'', ''2024-%s-%.2d 10:%.2d:00'')',
      ['orders', 'INSERT', I, I,
       Format('%.2d', [(I mod 12) + 1]), (I mod 28) + 1, I mod 60]));

  Conn.ExecuteNonQuery('COMMIT');
end;

// ============================================================================
// Demo 1: Database Size Metrics
// ============================================================================

{ Queries page_count, page_size, and freelist_count PRAGMAs to calculate and
  print total, used, and free space along with space efficiency percentage. }
procedure DemoDatabaseSize;
var
  PageCount, PageSize, FreelistCount: Integer;
  TotalSize, UsedSize, FreeSize: Int64;
begin
  WriteLn('=== 1. Database Size Metrics ===');
  WriteLn;

  PageCount := Integer(Conn.ExecuteScalar('PRAGMA page_count'));
  PageSize := Integer(Conn.ExecuteScalar('PRAGMA page_size'));
  FreelistCount := Integer(Conn.ExecuteScalar('PRAGMA freelist_count'));

  TotalSize := Int64(PageCount) * Int64(PageSize);
  FreeSize := Int64(FreelistCount) * Int64(PageSize);
  UsedSize := TotalSize - FreeSize;

  WriteLn(Format('   Page size:       %d bytes', [PageSize]));
  WriteLn(Format('   Total pages:     %d', [PageCount]));
  WriteLn(Format('   Used pages:      %d', [PageCount - FreelistCount]));
  WriteLn(Format('   Free pages:      %d', [FreelistCount]));
  WriteLn;
  WriteLn(Format('   Total size:      %d bytes (%.1f KB)', [TotalSize, TotalSize / 1024.0]));
  WriteLn(Format('   Used size:       %d bytes (%.1f KB)', [UsedSize, UsedSize / 1024.0]));
  WriteLn(Format('   Free size:       %d bytes (%.1f KB)', [FreeSize, FreeSize / 1024.0]));

  if PageCount > 0 then
    WriteLn(Format('   Space efficiency: %.1f%%', [(PageCount - FreelistCount) / PageCount * 100.0]))
  else
    WriteLn('   Space efficiency: N/A');

  WriteLn;
end;

// ============================================================================
// Demo 2: Fragmentation Analysis
// ============================================================================

{ Demonstrates database fragmentation analysis and free page detection. }
procedure DemoFragmentation;
var
  FreelistCount, PageCount: Integer;
  FragPercent: Double;
  AutoVacuum: Integer;
begin
  WriteLn('=== 2. Fragmentation Analysis ===');
  WriteLn;

  FreelistCount := Integer(Conn.ExecuteScalar('PRAGMA freelist_count'));
  PageCount := Integer(Conn.ExecuteScalar('PRAGMA page_count'));
  AutoVacuum := Integer(Conn.ExecuteScalar('PRAGMA auto_vacuum'));

  if PageCount > 0 then
    FragPercent := FreelistCount / PageCount * 100.0
  else
    FragPercent := 0;

  WriteLn(Format('   Freelist pages:   %d', [FreelistCount]));
  WriteLn(Format('   Fragmentation:    %.1f%%', [FragPercent]));

  case AutoVacuum of
    0: WriteLn('   Auto-vacuum:      OFF (manual VACUUM needed)');
    1: WriteLn('   Auto-vacuum:      FULL (automatic space reclaim)');
    2: WriteLn('   Auto-vacuum:      INCREMENTAL (partial reclaim)');
  end;

  // Simulate fragmentation by deleting rows
  WriteLn;
  WriteLn('   Simulating fragmentation (deleting 50% of audit_log)...');
  Conn.ExecuteNonQuery('DELETE FROM audit_log WHERE id % 2 = 0');

  FreelistCount := Integer(Conn.ExecuteScalar('PRAGMA freelist_count'));
  PageCount := Integer(Conn.ExecuteScalar('PRAGMA page_count'));
  if PageCount > 0 then
    FragPercent := FreelistCount / PageCount * 100.0
  else
    FragPercent := 0;
  WriteLn(Format('   After delete - Freelist: %d pages (%.1f%% fragmented)', [FreelistCount, FragPercent]));

  // Run VACUUM to defragment
  WriteLn('   Running VACUUM...');
  Conn.ExecuteNonQuery('VACUUM');

  FreelistCount := Integer(Conn.ExecuteScalar('PRAGMA freelist_count'));
  PageCount := Integer(Conn.ExecuteScalar('PRAGMA page_count'));
  if PageCount > 0 then
    FragPercent := FreelistCount / PageCount * 100.0
  else
    FragPercent := 0;
  WriteLn(Format('   After VACUUM - Freelist: %d pages (%.1f%% fragmented)', [FreelistCount, FragPercent]));
  WriteLn(Format('   New total pages: %d', [PageCount]));

  WriteLn;
end;

// ============================================================================
// Demo 3: Table Statistics
// ============================================================================

{ Lists each table with its row count, column count from PRAGMA table_info,
  and estimated storage size. }
procedure DemoTableStats;
var
  DS, DS2: TDataSet;
  TableName: string;
  RowCount, I: Integer;
begin
  WriteLn('=== 3. Table Statistics ===');
  WriteLn;

  WriteLn(Format('   %-15s %8s %8s %10s', ['Table', 'Rows', 'Columns', 'Size Est.']));
  WriteLn('   ' + StringOfChar('-', 45));

  DS := Conn.ExecuteQuery(
    'SELECT name FROM sqlite_master WHERE type = ''table'' AND name NOT LIKE ''sqlite_%'' ORDER BY name');
  try
    while not DS.EOF do
    begin
      TableName := DS.FieldByName('name').AsString;
      RowCount := Integer(Conn.ExecuteScalar(Format('SELECT COUNT(*) FROM "%s"', [TableName])));

      // Get column count
      DS2 := Conn.ExecuteQuery(Format('PRAGMA table_info("%s")', [TableName]));
      try
        I := 0;
        while not DS2.EOF do
        begin
          Inc(I);
          DS2.Next;
        end;
        WriteLn(Format('   %-15s %8d %8d %8.1f KB', [TableName, RowCount, I,
          RowCount * I * 20.0 / 1024])); // rough estimate
      finally
        DS2.Free;
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 4: Index Statistics
// ============================================================================

{ Queries sqlite_master for user-defined indexes and prints each index name,
  its table, the indexed columns, and the total index count. }
procedure DemoIndexStats;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Index Statistics ===');
  WriteLn;

  WriteLn(Format('   %-35s %-15s %s', ['Index', 'Table', 'Columns']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT name, tbl_name, sql FROM sqlite_master WHERE type = ''index'' AND sql IS NOT NULL ORDER BY tbl_name, name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-35s %-15s %s', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('tbl_name').AsString,
        Copy(DS.FieldByName('sql').AsString, Pos('(', DS.FieldByName('sql').AsString), 50)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total indexes: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_master WHERE type = ''index'''))]));
  WriteLn;
end;

// ============================================================================
// Demo 5: Cache Configuration
// ============================================================================

{ Shows current cache size and memory usage, adjusts cache to 4000 pages, then
  prints journal mode, synchronous, temp store, and locking mode settings. }
procedure DemoCacheConfig;
var
  CacheSize, PageSize: Integer;
begin
  WriteLn('=== 5. Cache Configuration ===');
  WriteLn;

  CacheSize := Integer(Conn.ExecuteScalar('PRAGMA cache_size'));
  PageSize := Integer(Conn.ExecuteScalar('PRAGMA page_size'));

  WriteLn(Format('   Cache size:      %d pages', [Abs(CacheSize)]));
  if CacheSize < 0 then
    WriteLn(Format('   Cache memory:    %d KB (negative = KB mode)', [-CacheSize]))
  else
    WriteLn(Format('   Cache memory:    %.1f KB (pages * page_size)', [CacheSize * PageSize / 1024.0]));

  WriteLn(Format('   Page size:       %d bytes', [PageSize]));

  // Adjust cache size
  WriteLn;
  WriteLn('   Adjusting cache to 4000 pages...');
  Conn.ExecuteNonQuery('PRAGMA cache_size = 4000');
  CacheSize := Integer(Conn.ExecuteScalar('PRAGMA cache_size'));
  WriteLn(Format('   New cache size:  %d pages (%.1f MB)', [CacheSize, CacheSize * PageSize / 1048576.0]));

  // Show other cache-related settings
  WriteLn;
  WriteLn(Format('   Journal mode:    %s', [VarToStr(Conn.ExecuteScalar('PRAGMA journal_mode'))]));
  WriteLn(Format('   Synchronous:     %s', [VarToStr(Conn.ExecuteScalar('PRAGMA synchronous'))]));
  WriteLn(Format('   Temp store:      %s', [VarToStr(Conn.ExecuteScalar('PRAGMA temp_store'))]));
  WriteLn(Format('   Locking mode:    %s', [VarToStr(Conn.ExecuteScalar('PRAGMA locking_mode'))]));
  WriteLn;
end;

// ============================================================================
// Demo 6: Connection Tracking
// ============================================================================

{ Opens multiple connections, tracks them with RegisterConnection, closes some,
  and prints the connection history with open/close timestamps and status. }
procedure DemoConnectionTracking;
var
  I: Integer;
  Conn2, Conn3: TNDXSQLiteConnection;
  Id1, Id2, Id3: Integer;
begin
  WriteLn('=== 6. Connection Tracking ===');
  WriteLn;

  // Register main connection
  Id1 := RegisterConnection;
  WriteLn(Format('   Connection #%d opened (main)', [Id1]));

  // Open additional connections
  Conn2 := TNDXSQLiteConnection.Create(DbPath);
  Conn2.Open;
  Id2 := RegisterConnection;
  WriteLn(Format('   Connection #%d opened (read replica)', [Id2]));

  Conn3 := TNDXSQLiteConnection.Create(DbPath);
  Conn3.Open;
  Id3 := RegisterConnection;
  WriteLn(Format('   Connection #%d opened (background worker)', [Id3]));

  WriteLn(Format('   Active connections: %d', [GetOpenConnectionCount]));

  // Close one connection
  Conn3.Close;
  Conn3.Free;
  CloseConnection(Id3);
  WriteLn(Format('   Connection #%d closed', [Id3]));
  WriteLn(Format('   Active connections: %d', [GetOpenConnectionCount]));

  // Close another
  Conn2.Close;
  Conn2.Free;
  CloseConnection(Id2);
  WriteLn(Format('   Connection #%d closed', [Id2]));
  WriteLn(Format('   Active connections: %d', [GetOpenConnectionCount]));

  // Show connection history
  WriteLn;
  WriteLn('   Connection History:');
  WriteLn(Format('   %-5s %-20s %-20s %-8s', ['ID', 'Opened', 'Closed', 'Status']));
  WriteLn('   ' + StringOfChar('-', 58));
  for I := 0 to ConnectionCount - 1 do
  begin
    WriteLn(Format('   %-5d %-20s %-20s %-8s', [
      Connections[I].Id,
      Connections[I].OpenedAt,
      Connections[I].ClosedAt,
      Connections[I].Status]));
  end;
  WriteLn;
end;

// ============================================================================
// Demo 7: Health Check
// ============================================================================

{ Runs integrity check, foreign key check, evaluates fragmentation and cache
  size against thresholds, and reports health status for each metric. }
procedure DemoHealthCheck;
var
  DS: TDataSet;
  IntegrityOk: Boolean;
  PageCount, FreelistCount, CacheSize: Integer;
  FragPercent: Double;
begin
  WriteLn('=== 7. Database Health Check ===');
  WriteLn;

  // Integrity check
  DS := Conn.ExecuteQuery('PRAGMA integrity_check');
  try
    IntegrityOk := (not DS.EOF) and (DS.Fields[0].AsString = 'ok');
    if IntegrityOk then
      WriteLn('   [OK] Integrity check: PASSED')
    else
    begin
      WriteLn('   [!!] Integrity check: FAILED');
      while not DS.EOF do
      begin
        WriteLn('        ' + DS.Fields[0].AsString);
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  // Foreign key check
  DS := Conn.ExecuteQuery('PRAGMA foreign_key_check');
  try
    if DS.EOF then
      WriteLn('   [OK] Foreign key check: No violations')
    else
      WriteLn('   [!!] Foreign key check: Violations found');
  finally
    DS.Free;
  end;

  // Fragmentation check
  PageCount := Integer(Conn.ExecuteScalar('PRAGMA page_count'));
  FreelistCount := Integer(Conn.ExecuteScalar('PRAGMA freelist_count'));
  if PageCount > 0 then
    FragPercent := FreelistCount / PageCount * 100.0
  else
    FragPercent := 0;

  if FragPercent < 5 then
    WriteLn(Format('   [OK] Fragmentation: %.1f%% (threshold: 5%%)', [FragPercent]))
  else if FragPercent < 20 then
    WriteLn(Format('   [!!] Fragmentation: %.1f%% (threshold: 5%%, consider VACUUM)', [FragPercent]))
  else
    WriteLn(Format('   [XX] Fragmentation: %.1f%% (critical, VACUUM recommended)', [FragPercent]));

  // Cache size check
  CacheSize := Abs(Integer(Conn.ExecuteScalar('PRAGMA cache_size')));
  if CacheSize >= 2000 then
    WriteLn(Format('   [OK] Cache size: %d pages (minimum: 2000)', [CacheSize]))
  else
    WriteLn(Format('   [!!] Cache size: %d pages (below minimum: 2000)', [CacheSize]));

  // Journal mode check
  WriteLn(Format('   [OK] Journal mode: %s', [VarToStr(Conn.ExecuteScalar('PRAGMA journal_mode'))]));

  // Table count
  WriteLn(Format('   [OK] Tables: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%'''))]));
  WriteLn(Format('   [OK] Indexes: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_master WHERE type=''index'''))]));

  WriteLn;
end;

// ============================================================================
// Demo 8: Storage Analysis
// ============================================================================

{ Calculates total database size from page metrics, lists row counts per table,
  shows schema object counts by type, and prints the SQLite version. }
procedure DemoStorageAnalysis;
var
  DS: TDataSet;
  TableName: string;
  RowCount: Integer;
  PageCount, PageSize: Integer;
  TotalRows: Integer;
begin
  WriteLn('=== 8. Storage Analysis ===');
  WriteLn;

  PageCount := Integer(Conn.ExecuteScalar('PRAGMA page_count'));
  PageSize := Integer(Conn.ExecuteScalar('PRAGMA page_size'));

  WriteLn(Format('   Database: %d pages x %d bytes = %.1f KB',
    [PageCount, PageSize, PageCount * PageSize / 1024.0]));
  WriteLn;

  // Row distribution
  WriteLn('   Row Distribution:');
  TotalRows := 0;
  DS := Conn.ExecuteQuery(
    'SELECT name FROM sqlite_master WHERE type = ''table'' AND name NOT LIKE ''sqlite_%'' ORDER BY name');
  try
    while not DS.EOF do
    begin
      TableName := DS.FieldByName('name').AsString;
      RowCount := Integer(Conn.ExecuteScalar(Format('SELECT COUNT(*) FROM "%s"', [TableName])));
      TotalRows := TotalRows + RowCount;
      WriteLn(Format('     %-15s: %5d rows', [TableName, RowCount]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('     %-15s: %5d rows', ['TOTAL', TotalRows]));

  // Schema objects
  WriteLn;
  WriteLn('   Schema Objects:');
  DS := Conn.ExecuteQuery(
    'SELECT type, COUNT(*) as cnt FROM sqlite_master WHERE name NOT LIKE ''sqlite_%'' GROUP BY type ORDER BY type');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-10s: %d', [DS.FieldByName('type').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // SQLite compile options (subset)
  WriteLn;
  WriteLn(Format('   SQLite version: %s', [VarToStr(Conn.ExecuteScalar('SELECT sqlite_version()'))]));
  WriteLn;
end;

// ============================================================================
// Main Program
// ============================================================================

var
  I: Integer;

begin
  // Use a file-based database for meaningful size metrics
  DbPath := GetTempDir + 'monitoring_demo.db';
  // Remove old file if exists
  if FileExists(DbPath) then
    DeleteFile(DbPath);

  Conn := TNDXSQLiteConnection.Create(DbPath);
  try
    Conn.Open;
    CreateTestData;

    WriteLn('Example 112: Database Monitoring - Size, Fragmentation, Connections, Cache');
    WriteLn(StringOfChar('=', 75));
    WriteLn(Format('   Database file: %s', [DbPath]));
    WriteLn;

    DemoDatabaseSize;
    DemoFragmentation;
    DemoTableStats;
    DemoIndexStats;
    DemoCacheConfig;
    DemoConnectionTracking;
    DemoHealthCheck;
    DemoStorageAnalysis;

    WriteLn('Done.');
    Conn.Close;
  finally
    Conn.Free;
    // Cleanup temp file
    if FileExists(DbPath) then
      DeleteFile(DbPath);
  end;
end.
