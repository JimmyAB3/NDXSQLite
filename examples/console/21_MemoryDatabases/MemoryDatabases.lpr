{===============================================================================
  NDXSQLite Example 21 - Memory Databases
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - In-memory databases (:memory:)
  - Named memory databases with shared cache (URI mode)
  - Temporary tables and databases
  - Memory vs disk performance comparison
  - Memory statistics and monitoring
  - Copy disk database to memory
  - Backup memory database to disk
  - Practical use case patterns

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program MemoryDatabases;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Math, Variants,
  ndxsqlite3api,
  ndxsqliteconnection,
  ndxsqliteconnectionfactory, ndxsqliteconnectionintf;

const
  WORK_DIR = 'memory_demo';

var
  WorkPath: string;

{ Returns the time ms. }
function GetTimeMs: Int64;
begin
  Result := MilliSecondOfTheDay(Now);
end;

{ Creates the working directory for temporary files if it does not already exist. }
procedure EnsureWorkDir;
begin
  WorkPath := ExtractFilePath(ParamStr(0)) + WORK_DIR + PathDelim;
  if not DirectoryExists(WorkPath) then
    CreateDir(WorkPath);
end;

{ Deletes all files in the working directory and removes the directory itself. }
procedure CleanupWorkDir;
var
  SR: TSearchRec;
begin
  if DirectoryExists(WorkPath) then
  begin
    if FindFirst(WorkPath + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
          DeleteFile(WorkPath + SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    RemoveDir(WorkPath);
  end;
end;

// =============================================================================
// 1. Basic In-Memory Database
// =============================================================================
{ Creates an in-memory database, inserts users, displays them, then closes to show data is lost. }
procedure DemoBasicMemoryDatabase;
var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;
begin
  WriteLn('1. Basic In-Memory Database');
  WriteLn('   ========================');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    WriteLn('   Database path: ', Conn.DatabasePath);

    // Create table and insert data
    Conn.ExecuteNonQuery('CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, email TEXT)');
    Conn.ExecuteNonQuery('INSERT INTO users (name, email) VALUES (''Alice'', ''alice@example.com'')');
    Conn.ExecuteNonQuery('INSERT INTO users (name, email) VALUES (''Bob'', ''bob@example.com'')');
    Conn.ExecuteNonQuery('INSERT INTO users (name, email) VALUES (''Carol'', ''carol@example.com'')');

    WriteLn('   Created table with 3 users');

    DS := Conn.ExecuteQuery('SELECT * FROM users ORDER BY id');
    try
      WriteLn('   Data in memory:');
      while not DS.EOF do
      begin
        WriteLn('     - ', DS.FieldByName('name').AsString, ' <', DS.FieldByName('email').AsString, '>');
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    Conn.Close;
    WriteLn('   Connection closed - all data is now LOST!');
    WriteLn('   (This is the key characteristic of memory databases)');
  finally
    Conn.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// 2. Memory Database with Factory
// =============================================================================
{ Creates a memory database using TNDXSQLiteConnectionFactory, stores config entries, and queries them. }
procedure DemoMemoryWithFactory;
var
  Factory: TNDXSQLiteConnectionFactory;
  Conn: INDXSQLiteConnection;
  Value: Variant;
begin
  WriteLn('2. Memory Database with Factory');
  WriteLn('   ============================');

  Factory := TNDXSQLiteConnectionFactory.Create(':memory:');
  try
    Conn := Factory.CreateMemoryConnection;
    Conn.Open;

    Conn.ExecuteNonQuery('CREATE TABLE config (key TEXT PRIMARY KEY, value TEXT)');
    Conn.ExecuteNonQuery('INSERT INTO config VALUES (''app_name'', ''MyApp'')');
    Conn.ExecuteNonQuery('INSERT INTO config VALUES (''version'', ''2.1.0'')');
    Conn.ExecuteNonQuery('INSERT INTO config VALUES (''debug_mode'', ''true'')');

    WriteLn('   Factory-created memory connection');

    Value := Conn.ExecuteScalar('SELECT value FROM config WHERE key=''app_name''');
    WriteLn('   App name: ', VarToStr(Value));

    Value := Conn.ExecuteScalar('SELECT value FROM config WHERE key=''version''');
    WriteLn('   Version: ', VarToStr(Value));

    Value := Conn.ExecuteScalar('SELECT COUNT(*) FROM config');
    WriteLn('   Config entries: ', Integer(Value));

    Conn.Close;
    Conn := nil;  // Release interface reference
  finally
    Factory.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// 3. Performance: Memory vs Disk
// =============================================================================
{ Benchmarks 50000 inserts and aggregate queries on memory vs disk databases, then reports speed ratios. }
procedure DemoPerformanceComparison;
var
  MemConn, DiskConn: TNDXSQLiteConnection;
  StartTime, MemInsertTime, DiskInsertTime: Int64;
  MemQueryTime, DiskQueryTime: Int64;
  I, Count: Integer;
  DBPath: string;
  DS: TDataSet;
begin
  WriteLn('3. Performance: Memory vs Disk');
  WriteLn('   ===========================');

  Count := 50000;
  WriteLn(Format('   Test: %d INSERT operations + query', [Count]));
  WriteLn('');

  // Memory database test
  MemConn := TNDXSQLiteConnection.Create(':memory:');
  try
    MemConn.Open;
    MemConn.ExecuteNonQuery('CREATE TABLE benchmark (id INTEGER PRIMARY KEY, name TEXT, value REAL)');

    // INSERT benchmark
    StartTime := GetTimeMs;
    MemConn.BeginTransaction;
    try
      for I := 1 to Count do
        MemConn.ExecuteNonQuery(Format(
          'INSERT INTO benchmark (name, value) VALUES (''Item_%d'', %f)',
          [I, Random * 1000]));
      MemConn.Commit;
    except
      MemConn.Rollback;
      raise;
    end;
    MemInsertTime := GetTimeMs - StartTime;

    // Query benchmark
    StartTime := GetTimeMs;
    DS := MemConn.ExecuteQuery('SELECT COUNT(*), AVG(value), MAX(value) FROM benchmark');
    try
      DS.First;
    finally
      DS.Free;
    end;
    MemQueryTime := GetTimeMs - StartTime;

    WriteLn(Format('   Memory INSERT: %d ms (%d rows/sec)', [MemInsertTime, (Count * 1000) div Max(MemInsertTime, 1)]));
    WriteLn(Format('   Memory QUERY:  %d ms', [MemQueryTime]));

    MemConn.Close;
  finally
    MemConn.Free;
  end;

  // Disk database test
  DBPath := WorkPath + 'benchmark.db';
  if FileExists(DBPath) then DeleteFile(DBPath);

  DiskConn := TNDXSQLiteConnection.Create(DBPath);
  try
    DiskConn.Open;
    DiskConn.ExecuteNonQuery('PRAGMA synchronous = NORMAL');  // Fair comparison
    DiskConn.ExecuteNonQuery('CREATE TABLE benchmark (id INTEGER PRIMARY KEY, name TEXT, value REAL)');

    // INSERT benchmark
    StartTime := GetTimeMs;
    DiskConn.BeginTransaction;
    try
      for I := 1 to Count do
        DiskConn.ExecuteNonQuery(Format(
          'INSERT INTO benchmark (name, value) VALUES (''Item_%d'', %f)',
          [I, Random * 1000]));
      DiskConn.Commit;
    except
      DiskConn.Rollback;
      raise;
    end;
    DiskInsertTime := GetTimeMs - StartTime;

    // Query benchmark
    StartTime := GetTimeMs;
    DS := DiskConn.ExecuteQuery('SELECT COUNT(*), AVG(value), MAX(value) FROM benchmark');
    try
      DS.First;
    finally
      DS.Free;
    end;
    DiskQueryTime := GetTimeMs - StartTime;

    WriteLn(Format('   Disk INSERT:   %d ms (%d rows/sec)', [DiskInsertTime, (Count * 1000) div Max(DiskInsertTime, 1)]));
    WriteLn(Format('   Disk QUERY:    %d ms', [DiskQueryTime]));

    DiskConn.Close;
  finally
    DiskConn.Free;
  end;

  WriteLn('');
  WriteLn(Format('   INSERT speedup: Memory is %.1fx faster', [DiskInsertTime / Max(MemInsertTime, 1)]));
  WriteLn(Format('   QUERY speedup:  Memory is %.1fx faster', [Max(DiskQueryTime, 1) / Max(MemQueryTime, 1)]));

  if FileExists(DBPath) then DeleteFile(DBPath);
  WriteLn('');
end;

// =============================================================================
// 4. Temporary Tables
// =============================================================================
{ Creates a products table and a TEMP cart table, lists all tables, and computes the cart total via join. }
procedure DemoTemporaryTables;
var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;
begin
  WriteLn('4. Temporary Tables');
  WriteLn('   =================');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    // Create regular table
    Conn.ExecuteNonQuery('CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT, price REAL)');
    Conn.ExecuteNonQuery('INSERT INTO products VALUES (1, ''Laptop'', 999.99)');
    Conn.ExecuteNonQuery('INSERT INTO products VALUES (2, ''Phone'', 599.99)');
    Conn.ExecuteNonQuery('INSERT INTO products VALUES (3, ''Tablet'', 399.99)');

    // Create temporary table for session calculations
    Conn.ExecuteNonQuery('CREATE TEMP TABLE cart (product_id INTEGER, quantity INTEGER)');
    Conn.ExecuteNonQuery('INSERT INTO cart VALUES (1, 2)');  // 2 laptops
    Conn.ExecuteNonQuery('INSERT INTO cart VALUES (3, 1)');  // 1 tablet

    WriteLn('   Created permanent table "products" and temp table "cart"');

    // List all tables (including temp)
    WriteLn('');
    WriteLn('   All tables:');
    DS := Conn.ExecuteQuery(
      'SELECT ''main'' as db, name FROM sqlite_master WHERE type=''table'' ' +
      'UNION ALL ' +
      'SELECT ''temp'' as db, name FROM sqlite_temp_master WHERE type=''table'' ' +
      'ORDER BY db, name');
    try
      while not DS.EOF do
      begin
        WriteLn('     [', DS.FieldByName('db').AsString, '] ', DS.FieldByName('name').AsString);
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    // Calculate cart total using join
    WriteLn('');
    WriteLn('   Cart contents:');
    DS := Conn.ExecuteQuery(
      'SELECT p.name, c.quantity, p.price, (c.quantity * p.price) as subtotal ' +
      'FROM cart c JOIN products p ON c.product_id = p.id');
    try
      while not DS.EOF do
      begin
        WriteLn(Format('     %dx %s @ $%.2f = $%.2f', [
          DS.FieldByName('quantity').AsInteger,
          DS.FieldByName('name').AsString,
          DS.FieldByName('price').AsFloat,
          DS.FieldByName('subtotal').AsFloat
        ]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    WriteLn(Format('   Cart total: $%.2f', [
      Double(Conn.ExecuteScalar('SELECT SUM(c.quantity * p.price) FROM cart c JOIN products p ON c.product_id = p.id'))
    ]));

    WriteLn('');
    WriteLn('   Note: TEMP tables are automatically dropped when connection closes');

    Conn.Close;
  finally
    Conn.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// 5. Named Memory Database with Shared Cache
// =============================================================================
{ Opens two connections to a named shared-cache memory database and shows they share the same data. }
procedure DemoSharedCacheMemory;
var
  Conn1, Conn2: TNDXSQLiteConnection;
  SharedURI: string;
  DS: TDataSet;
begin
  WriteLn('5. Named Memory Database with Shared Cache');
  WriteLn('   ========================================');

  // URI format for shared memory database
  // file:dbname?mode=memory&cache=shared
  SharedURI := 'file:shared_mem_db?mode=memory&cache=shared';

  WriteLn('   URI: ', SharedURI);
  WriteLn('');

  // First connection creates the database
  Conn1 := TNDXSQLiteConnection.Create(SharedURI);
  try
    Conn1.Open;
    WriteLn('   Connection 1: Opened shared memory database');

    // Create table and insert data
    Conn1.ExecuteNonQuery('CREATE TABLE shared_data (id INTEGER PRIMARY KEY, message TEXT)');
    Conn1.ExecuteNonQuery('INSERT INTO shared_data VALUES (1, ''Hello from Connection 1'')');
    Conn1.ExecuteNonQuery('INSERT INTO shared_data VALUES (2, ''Shared memory rocks!'')');
    WriteLn('   Connection 1: Created table with 2 rows');

    // Second connection accesses SAME database
    Conn2 := TNDXSQLiteConnection.Create(SharedURI);
    try
      Conn2.Open;
      WriteLn('   Connection 2: Opened SAME shared memory database');

      // Read data inserted by Conn1
      DS := Conn2.ExecuteQuery('SELECT * FROM shared_data ORDER BY id');
      try
        WriteLn('   Connection 2: Reading data from shared table:');
        while not DS.EOF do
        begin
          WriteLn('     [', DS.FieldByName('id').AsInteger, '] ', DS.FieldByName('message').AsString);
          DS.Next;
        end;
      finally
        DS.Free;
      end;

      // Conn2 inserts data that Conn1 can see
      Conn2.ExecuteNonQuery('INSERT INTO shared_data VALUES (3, ''Added by Connection 2'')');
      WriteLn('   Connection 2: Inserted new row');

      // Conn1 sees the new data
      WriteLn('   Connection 1: Row count = ', Integer(Conn1.ExecuteScalar('SELECT COUNT(*) FROM shared_data')));

      Conn2.Close;
    finally
      Conn2.Free;
    end;

    WriteLn('');
    WriteLn('   Both connections shared the SAME in-memory database!');
    WriteLn('   (Unlike :memory: which creates separate databases)');

    Conn1.Close;
  finally
    Conn1.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// 6. Memory Statistics and Monitoring
// =============================================================================
{ Displays page size, page count, and cache size before and after inserting 1000 BLOB rows into memory. }
procedure DemoMemoryStatistics;
var
  Conn: TNDXSQLiteConnection;
  PageSize, PageCount, CacheSize: Integer;
  I: Integer;
begin
  WriteLn('6. Memory Statistics and Monitoring');
  WriteLn('   =================================');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    // Get initial memory stats
    PageSize := Conn.ExecuteScalar('PRAGMA page_size');
    PageCount := Conn.ExecuteScalar('PRAGMA page_count');
    CacheSize := Conn.ExecuteScalar('PRAGMA cache_size');

    WriteLn('   Initial state:');
    WriteLn('     Page size:    ', PageSize, ' bytes');
    WriteLn('     Page count:   ', PageCount);
    if CacheSize < 0 then
      WriteLn('     Cache size:   ', Abs(CacheSize), ' KB')
    else
      WriteLn('     Cache size:   ', CacheSize, ' pages');

    // Create table and fill with data
    Conn.ExecuteNonQuery('CREATE TABLE large_data (id INTEGER PRIMARY KEY, data BLOB)');

    WriteLn('');
    WriteLn('   Inserting 1000 rows with 1KB BLOBs...');

    Conn.BeginTransaction;
    try
      for I := 1 to 1000 do
        Conn.ExecuteNonQuery(Format('INSERT INTO large_data VALUES (%d, zeroblob(1024))', [I]));
      Conn.Commit;
    except
      Conn.Rollback;
      raise;
    end;

    // Get updated stats
    PageCount := Conn.ExecuteScalar('PRAGMA page_count');

    WriteLn('');
    WriteLn('   After inserting ~1MB of data:');
    WriteLn('     Page count:   ', PageCount);
    WriteLn('     Estimated DB size: ', (PageCount * PageSize) div 1024, ' KB');

    // Note: sqlite3_memory_used() and sqlite3_memory_highwater() could be used
    // if compiled with SQLITE_ENABLE_MEMORY_MANAGEMENT - not always available

    // Database-specific stats
    WriteLn('');
    WriteLn('   Database statistics:');
    WriteLn('     Total rows: ', Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM large_data')));
    WriteLn('     Total BLOB size: ', Integer(Conn.ExecuteScalar('SELECT SUM(LENGTH(data)) FROM large_data')) div 1024, ' KB');

    // Freelist pages (unused space)
    WriteLn('     Freelist pages: ', Integer(Conn.ExecuteScalar('PRAGMA freelist_count')));

    Conn.Close;
  finally
    Conn.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// 7. Copy Disk Database to Memory
// =============================================================================
{ Creates a disk database, attaches it to a memory database, copies tables via CREATE AS SELECT, then verifies. }
procedure DemoCopyDiskToMemory;
var
  DiskConn, MemConn: TNDXSQLiteConnection;
  DS: TDataSet;
  DBPath: string;
  StartTime, ElapsedTime: Int64;
begin
  WriteLn('7. Copy Disk Database to Memory');
  WriteLn('   =============================');

  // First, create a disk database with sample data
  DBPath := WorkPath + 'source_data.db';
  if FileExists(DBPath) then DeleteFile(DBPath);

  DiskConn := TNDXSQLiteConnection.Create(DBPath);
  try
    DiskConn.Open;
    DiskConn.ExecuteNonQuery('CREATE TABLE customers (id INTEGER PRIMARY KEY, name TEXT, city TEXT)');
    DiskConn.ExecuteNonQuery('INSERT INTO customers VALUES (1, ''Alice Smith'', ''New York'')');
    DiskConn.ExecuteNonQuery('INSERT INTO customers VALUES (2, ''Bob Johnson'', ''Los Angeles'')');
    DiskConn.ExecuteNonQuery('INSERT INTO customers VALUES (3, ''Carol Williams'', ''Chicago'')');
    DiskConn.ExecuteNonQuery('INSERT INTO customers VALUES (4, ''David Brown'', ''Houston'')');
    DiskConn.ExecuteNonQuery('INSERT INTO customers VALUES (5, ''Eve Davis'', ''Phoenix'')');
    DiskConn.Close;
  finally
    DiskConn.Free;
  end;

  WriteLn('   Created disk database with 5 customers');

  // Method 1: Using ATTACH/DETACH
  WriteLn('');
  WriteLn('   Method 1: ATTACH + CREATE TABLE AS SELECT');

  MemConn := TNDXSQLiteConnection.Create(':memory:');
  try
    MemConn.Open;

    StartTime := GetTimeMs;

    // Attach disk database
    MemConn.ExecuteNonQuery(Format('ATTACH DATABASE ''%s'' AS disk_db', [DBPath]));

    // Copy table structure and data
    MemConn.ExecuteNonQuery('CREATE TABLE customers AS SELECT * FROM disk_db.customers');

    // Detach disk database
    MemConn.ExecuteNonQuery('DETACH DATABASE disk_db');

    ElapsedTime := GetTimeMs - StartTime;

    WriteLn(Format('   Copied to memory in %d ms', [ElapsedTime]));

    // Verify data
    DS := MemConn.ExecuteQuery('SELECT * FROM customers ORDER BY id');
    try
      WriteLn('   Data in memory:');
      while not DS.EOF do
      begin
        WriteLn('     ', DS.FieldByName('name').AsString, ' - ', DS.FieldByName('city').AsString);
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    // Now we can query fast in memory
    WriteLn('   Total customers: ', Integer(MemConn.ExecuteScalar('SELECT COUNT(*) FROM customers')));

    MemConn.Close;
  finally
    MemConn.Free;
  end;

  if FileExists(DBPath) then DeleteFile(DBPath);
  WriteLn('');
end;

// =============================================================================
// 8. Backup Memory Database to Disk
// =============================================================================
{ Populates a memory database with transactions, backs it up to disk using VACUUM INTO, and verifies integrity. }
procedure DemoBackupMemoryToDisk;
var
  MemConn, VerifyConn: TNDXSQLiteConnection;
  BackupPath: string;
  I: Integer;
  StartTime: Int64;
  SR: TSearchRec;
  BackupSize: Int64;
begin
  WriteLn('8. Backup Memory Database to Disk');
  WriteLn('   ===============================');

  BackupPath := WorkPath + 'memory_backup.db';
  if FileExists(BackupPath) then DeleteFile(BackupPath);

  // Create memory database with data
  MemConn := TNDXSQLiteConnection.Create(':memory:');
  try
    MemConn.Open;

    // Create schema and populate
    MemConn.ExecuteNonQuery('CREATE TABLE transactions (id INTEGER PRIMARY KEY, amount REAL, description TEXT, created_at TEXT)');
    MemConn.ExecuteNonQuery('CREATE INDEX idx_amount ON transactions(amount)');

    MemConn.BeginTransaction;
    try
      for I := 1 to 100 do
        MemConn.ExecuteNonQuery(Format(
          'INSERT INTO transactions (amount, description, created_at) VALUES (%.2f, ''Transaction %d'', datetime(''now''))',
          [Random * 1000, I]));
      MemConn.Commit;
    except
      MemConn.Rollback;
      raise;
    end;

    WriteLn('   Created memory database with 100 transactions');
    WriteLn('   Total amount: $', FormatFloat('0.00', Double(MemConn.ExecuteScalar('SELECT SUM(amount) FROM transactions'))));

    // Backup using VACUUM INTO (SQLite 3.27+)
    WriteLn('');
    WriteLn('   Backing up to disk using VACUUM INTO...');

    StartTime := GetTimeMs;
    MemConn.ExecuteNonQuery(Format('VACUUM INTO ''%s''', [BackupPath]));

    WriteLn('   Backup successful!');
    WriteLn('     Destination: ', BackupPath);

    // Get file size using FindFirst
    BackupSize := 0;
    if FindFirst(BackupPath, faAnyFile, SR) = 0 then
    begin
      BackupSize := SR.Size;
      FindClose(SR);
    end;
    WriteLn('     Size: ', BackupSize div 1024, ' KB');
    WriteLn('     Time: ', GetTimeMs - StartTime, ' ms');

    MemConn.Close;
  finally
    MemConn.Free;
  end;

  // Verify backup by opening it
  WriteLn('');
  WriteLn('   Verifying backup...');
  VerifyConn := TNDXSQLiteConnection.Create(BackupPath);
  try
    VerifyConn.Open;
    WriteLn('   Rows in backup: ', Integer(VerifyConn.ExecuteScalar('SELECT COUNT(*) FROM transactions')));
    WriteLn('   Backup integrity: ', VarToStr(VerifyConn.ExecuteScalar('PRAGMA integrity_check')));
    VerifyConn.Close;
  finally
    VerifyConn.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// 9. Practical Use Cases with Code
// =============================================================================
{ Shows four practical memory database patterns: unit testing, query caching, data analysis, and session storage. }
procedure DemoUseCases;
var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;
  I: Integer;
  StartTime: Int64;
begin
  WriteLn('9. Practical Use Cases');
  WriteLn('   ====================');

  // Use Case 1: Unit Testing
  WriteLn('');
  WriteLn('   Use Case 1: Unit Testing (isolated, fast, no cleanup)');
  WriteLn('   -----------------------------------------------------');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    // Setup test schema
    Conn.ExecuteNonQuery('CREATE TABLE users (id INTEGER PRIMARY KEY, email TEXT UNIQUE)');
    // Run test
    Conn.ExecuteNonQuery('INSERT INTO users (email) VALUES (''test@example.com'')');
    // Assert
    if Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE email=''test@example.com''')) = 1 then
      WriteLn('   Test PASSED: User created successfully')
    else
      WriteLn('   Test FAILED');
    // No cleanup needed - database vanishes!
    Conn.Close;
  finally
    Conn.Free;
  end;

  // Use Case 2: Query Result Cache
  WriteLn('');
  WriteLn('   Use Case 2: Query Result Caching');
  WriteLn('   ---------------------------------');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    // Create cache table
    Conn.ExecuteNonQuery('CREATE TABLE query_cache (query_hash TEXT PRIMARY KEY, result TEXT, cached_at INTEGER)');
    // Cache a result
    Conn.ExecuteNonQuery('INSERT INTO query_cache VALUES (''abc123'', ''{"data": [1,2,3]}'', strftime(''%s'',''now''))');
    // Retrieve from cache
    WriteLn('   Cached result: ', VarToStr(Conn.ExecuteScalar('SELECT result FROM query_cache WHERE query_hash=''abc123''')));
    Conn.Close;
  finally
    Conn.Free;
  end;

  // Use Case 3: Complex Data Processing
  WriteLn('');
  WriteLn('   Use Case 3: Complex Sorting/Filtering Pipeline');
  WriteLn('   -----------------------------------------------');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    // Import raw data
    Conn.ExecuteNonQuery('CREATE TABLE raw_data (value REAL)');
    Conn.BeginTransaction;
    for I := 1 to 10000 do
      Conn.ExecuteNonQuery(Format('INSERT INTO raw_data VALUES (%f)', [Random * 100]));
    Conn.Commit;

    StartTime := GetTimeMs;
    // Complex analysis using SQL
    DS := Conn.ExecuteQuery(
      'SELECT ' +
      '  COUNT(*) as total, ' +
      '  AVG(value) as mean, ' +
      '  MIN(value) as min_val, ' +
      '  MAX(value) as max_val, ' +
      '  (SELECT AVG(value) FROM (SELECT value FROM raw_data ORDER BY value LIMIT 5000 OFFSET 2500)) as median_approx ' +
      'FROM raw_data');
    try
      DS.First;
      WriteLn(Format('   Analyzed 10000 values in %d ms:', [GetTimeMs - StartTime]));
      WriteLn(Format('     Mean: %.2f, Min: %.2f, Max: %.2f',
        [DS.FieldByName('mean').AsFloat,
         DS.FieldByName('min_val').AsFloat,
         DS.FieldByName('max_val').AsFloat]));
    finally
      DS.Free;
    end;
    Conn.Close;
  finally
    Conn.Free;
  end;

  // Use Case 4: Session Storage
  WriteLn('');
  WriteLn('   Use Case 4: Web Session Storage');
  WriteLn('   --------------------------------');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    Conn.ExecuteNonQuery(
      'CREATE TABLE sessions (' +
      '  session_id TEXT PRIMARY KEY, ' +
      '  user_id INTEGER, ' +
      '  data TEXT, ' +
      '  expires_at INTEGER)');
    Conn.ExecuteNonQuery(
      'CREATE INDEX idx_expires ON sessions(expires_at)');

    // Create session
    Conn.ExecuteNonQuery(
      'INSERT INTO sessions VALUES (''sess_abc123'', 42, ''{"cart": []}'', strftime(''%s'',''now'', ''+1 hour''))');

    // Get session
    WriteLn('   Session data: ', VarToStr(Conn.ExecuteScalar(
      'SELECT data FROM sessions WHERE session_id=''sess_abc123'' AND expires_at > strftime(''%s'',''now'')')));

    // Cleanup expired (would run periodically)
    Conn.ExecuteNonQuery('DELETE FROM sessions WHERE expires_at <= strftime(''%s'',''now'')');

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// 10. Summary
// =============================================================================
{ Displays a summary of memory database advantages and considerations. }
procedure DemoSummary;
begin
  WriteLn('10. Memory Database Summary');
  WriteLn('    =======================');
  WriteLn('');
  WriteLn('    Advantages:');
  WriteLn('      + Extremely fast (no disk I/O)');
  WriteLn('      + No file cleanup needed');
  WriteLn('      + Isolated for testing');
  WriteLn('      + Can be shared via URI mode');
  WriteLn('      + Can be backed up to disk');
  WriteLn('');
  WriteLn('    Limitations:');
  WriteLn('      - Data lost when all connections close');
  WriteLn('      - Limited by available RAM');
  WriteLn('      - Cannot be accessed by other processes (unless shared cache)');
  WriteLn('      - No crash recovery');
  WriteLn('');
  WriteLn('    Connection strings:');
  WriteLn('      :memory:                              Private memory DB');
  WriteLn('      file:name?mode=memory&cache=shared    Shared memory DB');
  WriteLn('      file::memory:?cache=shared            Anonymous shared DB');
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 21: Memory Databases ===');
  WriteLn('');

  Randomize;
  EnsureWorkDir;

  try
    DemoBasicMemoryDatabase;
    DemoMemoryWithFactory;
    DemoPerformanceComparison;
    DemoTemporaryTables;
    DemoSharedCacheMemory;
    DemoMemoryStatistics;
    DemoCopyDiskToMemory;
    DemoBackupMemoryToDisk;
    DemoUseCases;
    DemoSummary;

    WriteLn('=== Example completed successfully! ===');
  finally
    CleanupWorkDir;
  end;

  WriteLn('');
end.
