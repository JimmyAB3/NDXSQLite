{===============================================================================
  NDXSQLite Example 10 - Health Check
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Database health monitoring
  - Integrity checks
  - Database statistics
  - Maintenance operations (VACUUM, ANALYZE)

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program HealthCheck;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteconnectionfactory,
  ndxsqlitehealthcheck, ndxsqliteconnectionoptions;

var
  Factory: INDXSQLiteConnectionFactory;
  HealthChecker: TNDXSQLiteHealthCheck;
  HealthResult: TNDXSQLiteHealthCheckResult;
  DbInfo: TNDXSQLiteDatabaseInfo;
  DbStats: TNDXSQLiteDatabaseStats;
  TableList: TStringList;
  DBPath: string;
  Conn: INDXSQLiteConnection;
  I: Integer;

begin
  WriteLn('=== NDXSQLite Example 10: Health Check ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example10.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  // Create factory and health checker
  Factory := TNDXSQLiteConnectionFactory.Create(DBPath);
  HealthChecker := TNDXSQLiteHealthCheck.Create(Factory);

  try
    // Setup test database with some content
    WriteLn('1. Setting up test database...');
    Conn := Factory.CreateConnection;
    Conn.Open;

    Conn.ExecuteNonQuery(
      'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, email TEXT)');
    Conn.ExecuteNonQuery(
      'CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER, amount REAL)');
    Conn.ExecuteNonQuery(
      'CREATE INDEX idx_orders_user ON orders(user_id)');
    Conn.ExecuteNonQuery(
      'CREATE VIEW user_orders AS SELECT u.name, o.amount FROM users u JOIN orders o ON u.id = o.user_id');
    Conn.ExecuteNonQuery(
      'CREATE TRIGGER log_insert AFTER INSERT ON users BEGIN SELECT 1; END');

    // Insert test data
    for I := 1 to 100 do
    begin
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO users (name, email) VALUES (''User%d'', ''user%d@test.com'')',
        [I, I]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO orders (user_id, amount) VALUES (%d, %.2f)',
        [I, Random * 1000]));
    end;
    Conn.Close;
    WriteLn('   Test data created (100 users, 100 orders).');
    WriteLn;

    // 2. Basic health check
    WriteLn('2. Basic health check:');
    HealthResult := HealthChecker.CheckHealth;
    WriteLn('   Is healthy: ', HealthResult.IsHealthy);
    WriteLn('   Message: ', HealthResult.Message);
    WriteLn('   Response time: ', HealthResult.ResponseTimeMs, ' ms');
    if HealthResult.ExceptionMessage <> '' then
      WriteLn('   Error: ', HealthResult.ExceptionMessage);
    WriteLn;

    // 3. Database information
    WriteLn('3. Database information:');
    DbInfo := HealthChecker.GetDatabaseInfo;
    WriteLn('   SQLite version: ', DbInfo.SQLiteVersion);
    WriteLn('   Database path: ', DbInfo.DatabasePath);
    WriteLn('   Database size: ', DbInfo.DatabaseSizeBytes, ' bytes (',
      DbInfo.DatabaseSizeBytes div 1024, ' KB)');
    WriteLn('   Page size: ', DbInfo.PageSize, ' bytes');
    WriteLn('   Page count: ', DbInfo.PageCount);
    WriteLn('   Journal mode: ', DbInfo.JournalMode);
    WriteLn('   Encoding: ', DbInfo.Encoding);
    WriteLn('   Free list count: ', DbInfo.FreelistCount);
    WriteLn('   Auto vacuum: ', DbInfo.AutoVacuum);
    WriteLn('   Cache size: ', DbInfo.CacheSize, ' KB');
    WriteLn;

    // 4. Database statistics
    WriteLn('4. Database statistics:');
    DbStats := HealthChecker.GetDatabaseStats;
    WriteLn('   Table count: ', DbStats.TableCount);
    WriteLn('   Index count: ', DbStats.IndexCount);
    WriteLn('   View count: ', DbStats.ViewCount);
    WriteLn('   Trigger count: ', DbStats.TriggerCount);
    WriteLn;

    // 5. Table list
    WriteLn('5. Table list:');
    TableList := HealthChecker.GetTableList;
    try
      for I := 0 to TableList.Count - 1 do
        WriteLn('   - ', TableList[I]);
    finally
      TableList.Free;
    end;
    WriteLn;

    // 6. Integrity checks
    WriteLn('6. Integrity checks:');
    WriteLn('   Quick check: ', HealthChecker.CheckIntegrity(False));
    WriteLn('   Full check: ', HealthChecker.CheckIntegrity(True));
    WriteLn('   Foreign keys: ', HealthChecker.CheckForeignKeys);
    WriteLn;

    // 7. Maintenance operations
    WriteLn('7. Maintenance operations:');

    Write('   Running ANALYZE...');
    HealthChecker.Analyze;
    WriteLn(' Done');

    Write('   Running OPTIMIZE...');
    HealthChecker.Optimize;
    WriteLn(' Done');

    Write('   Running VACUUM...');
    HealthChecker.Vacuum;
    WriteLn(' Done');

    WriteLn;

    // 8. Size after vacuum
    WriteLn('8. Database info after maintenance:');
    DbInfo := HealthChecker.GetDatabaseInfo;
    WriteLn('   Database size: ', DbInfo.DatabaseSizeBytes, ' bytes');
    WriteLn('   Free list count: ', DbInfo.FreelistCount);
    WriteLn;

    // 9. Individual count methods
    WriteLn('9. Individual count methods:');
    WriteLn('   GetTableCount: ', HealthChecker.GetTableCount);
    WriteLn('   GetIndexCount: ', HealthChecker.GetIndexCount);
    WriteLn('   GetViewCount: ', HealthChecker.GetViewCount);
    WriteLn('   GetTriggerCount: ', HealthChecker.GetTriggerCount);
    WriteLn;

    // 10. Monitoring recommendations
    WriteLn('10. Health monitoring recommendations:');
    WriteLn('    - Run CheckHealth periodically');
    WriteLn('    - Monitor database size growth');
    WriteLn('    - Run ANALYZE after bulk operations');
    WriteLn('    - Run VACUUM periodically to reclaim space');
    WriteLn('    - Check integrity after system crashes');
    WriteLn;

  finally
    HealthChecker.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
