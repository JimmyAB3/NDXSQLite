{===============================================================================
  NDXSQLite Example 08 - Connection Pool
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Connection pooling for multi-threaded applications
  - Pool configuration (min/max size)
  - Acquiring and releasing connections
  - Pool statistics
  - Thread-safe access

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ConnectionPool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteconnectionpool,
  ndxsqliteconnectionoptions, ndxsqlitetypes;

var
  Pool: TNDXSQLiteConnectionPool;
  Options: TNDXSQLiteConnectionOptions;
  Conn1, Conn2, Conn3: INDXSQLiteConnection;
  PooledConn: TNDXPooledConnection;
  DBPath: string;
  Stats: TNDXPoolStatistics;

begin
  WriteLn('=== NDXSQLite Example 08: Connection Pool ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example08.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  // 1. Create pool with options
  WriteLn('1. Creating connection pool...');
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;
    // WAL mode is essential for concurrent access with multiple connections
    Options.JournalMode := jmWAL;
    Options.BusyTimeout := 5000;

    // Pool with min 2, max 5 connections
    Pool := TNDXSQLiteConnectionPool.Create(Options, 2, 5);
  finally
    Options.Free;
  end;

  try
    WriteLn('   Pool created (min: ', Pool.MinSize, ', max: ', Pool.MaxSize, ')');
    WriteLn('   Idle connections: ', Pool.IdleCount);
    WriteLn('   Active connections: ', Pool.ActiveCount);
    WriteLn;

    // 2. Acquire connection
    WriteLn('2. Acquiring a connection...');
    Conn1 := Pool.Acquire;
    WriteLn('   Connection acquired (ID: ', Conn1.Id, ')');
    WriteLn('   Idle: ', Pool.IdleCount, ', Active: ', Pool.ActiveCount);

    // Create test table
    Conn1.ExecuteNonQuery(
      'CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)');
    Conn1.ExecuteNonQuery('INSERT INTO test VALUES (1, ''from conn1'')');
    WriteLn;

    // 3. Acquire more connections
    WriteLn('3. Acquiring more connections...');
    Conn2 := Pool.Acquire;
    Conn3 := Pool.Acquire;
    WriteLn('   Connection 2 ID: ', Conn2.Id);
    WriteLn('   Connection 3 ID: ', Conn3.Id);
    WriteLn('   Idle: ', Pool.IdleCount, ', Active: ', Pool.ActiveCount);

    // Both can read from the same database
    WriteLn('   Conn2 reads: ', Conn2.ExecuteScalar('SELECT value FROM test'));
    Conn3.ExecuteNonQuery('INSERT INTO test VALUES (2, ''from conn3'')');
    WriteLn('   Conn3 inserted data');
    WriteLn;

    // 4. Release connections
    WriteLn('4. Releasing connections...');
    Pool.Release(Conn1);
    WriteLn('   Released Conn1 - Idle: ', Pool.IdleCount, ', Active: ', Pool.ActiveCount);
    Pool.Release(Conn2);
    WriteLn('   Released Conn2 - Idle: ', Pool.IdleCount, ', Active: ', Pool.ActiveCount);
    Pool.Release(Conn3);
    WriteLn('   Released Conn3 - Idle: ', Pool.IdleCount, ', Active: ', Pool.ActiveCount);
    WriteLn;

    // 5. Pool reuses connections
    WriteLn('5. Connection reuse demonstration:');
    Conn1 := Pool.Acquire;
    WriteLn('   First acquire: ID ', Conn1.Id);
    Pool.Release(Conn1);

    Conn2 := Pool.Acquire;
    WriteLn('   Second acquire: ID ', Conn2.Id, ' (may be same connection)');
    Pool.Release(Conn2);
    WriteLn;

    // 6. TNDXPooledConnection wrapper
    WriteLn('6. Using TNDXPooledConnection (auto-release):');
    PooledConn := TNDXPooledConnection.Create(Pool);
    try
      WriteLn('   PooledConnection ID: ', PooledConn.Connection.Id);
      WriteLn('   Active: ', Pool.ActiveCount);
      WriteLn('   Data: ', PooledConn.Connection.ExecuteScalar(
        'SELECT COUNT(*) FROM test'), ' rows');
    finally
      PooledConn.Free; // Automatically releases connection
    end;
    WriteLn('   After Free - Active: ', Pool.ActiveCount);
    WriteLn;

    // 7. TryAcquire with timeout
    WriteLn('7. TryAcquire with timeout:');
    if Pool.TryAcquire(Conn1, 1000) then
    begin
      WriteLn('   Acquired connection within timeout');
      Pool.Release(Conn1);
    end
    else
      WriteLn('   Timeout waiting for connection');
    WriteLn;

    // 8. Pool statistics
    WriteLn('8. Pool statistics:');
    Stats := Pool.Statistics;
    WriteLn('   Total created: ', Stats.TotalCreated);
    WriteLn('   Total acquisitions: ', Stats.TotalAcquisitions);
    WriteLn('   Total releases: ', Stats.TotalReleases);
    WriteLn('   Peak active: ', Stats.PeakActive);
    WriteLn;

    // 9. Pool configuration
    WriteLn('9. Pool configuration options:');
    Pool.ValidateOnAcquire := True;
    Pool.ValidationQuery := 'SELECT 1';
    Pool.AcquireTimeoutMs := 5000;
    WriteLn('   ValidateOnAcquire: ', Pool.ValidateOnAcquire);
    WriteLn('   ValidationQuery: ', Pool.ValidationQuery);
    WriteLn('   AcquireTimeoutMs: ', Pool.AcquireTimeoutMs);
    WriteLn;

    // 10. Benefits summary
    WriteLn('10. Benefits of Connection Pooling:');
    WriteLn('    - Reduced connection overhead');
    WriteLn('    - Thread-safe connection management');
    WriteLn('    - Automatic resource cleanup');
    WriteLn('    - Limits concurrent connections');
    WriteLn('    - Connection reuse improves performance');
    WriteLn;

  finally
    Pool.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then
    DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then
    DeleteFile(DBPath + '-shm');
end.
