{===============================================================================
  NDXSQLite Example 06 - Connection Options
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Configuring connection options before opening
  - WAL mode vs DELETE mode
  - Cache size configuration
  - Busy timeout settings
  - Synchronous mode
  - Foreign key enforcement
  - Reading PRAGMA values

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ConnectionOptions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Variants,
  ndxsqliteconnection, ndxsqlitetypes, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

begin
  WriteLn('=== NDXSQLite Example 06: Connection Options ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example06.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then
    DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then
    DeleteFile(DBPath + '-shm');

  // 1. Create and configure options BEFORE opening connection
  WriteLn('1. Creating Connection Options:');
  WriteLn('   Options must be configured BEFORE opening the connection');
  WriteLn('   because some PRAGMAs cannot be changed inside a transaction.');
  WriteLn;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;
    Options.CreateIfNotExists := True;

    // 2. Configure Journal Mode
    WriteLn('2. Journal Mode Configuration:');
    WriteLn('   Setting: WAL (Write-Ahead Logging)');
    Options.JournalMode := jmWAL;
    WriteLn('   WAL benefits:');
    WriteLn('   - Better concurrent read performance');
    WriteLn('   - Writers don''t block readers');
    WriteLn('   - Faster for many small transactions');
    WriteLn;

    // 3. Configure Synchronous Mode
    WriteLn('3. Synchronous Mode Configuration:');
    WriteLn('   Setting: NORMAL (balanced safety/performance)');
    Options.SyncMode := smNormal;
    WriteLn('   Options: OFF(0)=Fast, NORMAL(1)=Balanced, FULL(2)=Safe');
    WriteLn;

    // 4. Configure Cache Size
    WriteLn('4. Cache Size Configuration:');
    WriteLn('   Setting: 8192 KB (8 MB)');
    Options.CacheSize := 8192;
    WriteLn('   Larger cache = faster queries, more memory');
    WriteLn;

    // 5. Configure Busy Timeout
    WriteLn('5. Busy Timeout Configuration:');
    WriteLn('   Setting: 5000 ms (5 seconds)');
    Options.BusyTimeout := 5000;
    WriteLn('   Prevents SQLITE_BUSY errors during lock contention');
    WriteLn;

    // 6. Configure Foreign Keys
    WriteLn('6. Foreign Key Configuration:');
    WriteLn('   Setting: Enabled');
    Options.ForeignKeys := True;
    WriteLn('   Critical for referential integrity!');
    WriteLn;

    // 7. Configure Temp Store
    WriteLn('7. Temp Store Configuration:');
    WriteLn('   Setting: MEMORY (2)');
    Options.TempStore := 2;
    WriteLn('   Temporary tables/indices stored in RAM');
    WriteLn;

    // Now create and open connection with these options
    WriteLn('--- Opening Connection with Configured Options ---');
    WriteLn;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      // 8. Verify applied settings by reading PRAGMA values
      WriteLn('8. Verifying Applied Settings:');
      WriteLn('   journal_mode: ', Connection.GetPragmaValue('journal_mode'));
      WriteLn('   synchronous: ', Connection.GetPragmaValue('synchronous'),
        ' (0=OFF, 1=NORMAL, 2=FULL)');
      WriteLn('   cache_size: ', Connection.GetCacheSize, ' KB');
      WriteLn('   busy_timeout: ', Connection.GetBusyTimeout, ' ms');
      WriteLn('   foreign_keys: ', Connection.IsForeignKeysEnabled);
      WriteLn('   temp_store: ', Connection.GetPragmaValue('temp_store'),
        ' (0=DEFAULT, 1=FILE, 2=MEMORY)');
      WriteLn;

      // 9. Other useful PRAGMA values
      WriteLn('9. Other PRAGMA Values:');
      WriteLn('   page_size: ', Connection.GetPragmaValue('page_size'), ' bytes');
      WriteLn('   page_count: ', Connection.GetPragmaValue('page_count'));
      WriteLn('   encoding: ', Connection.GetPragmaValue('encoding'));
      WriteLn('   auto_vacuum: ', Connection.GetPragmaValue('auto_vacuum'));
      WriteLn;

      // 10. Recommended settings summary
      WriteLn('10. Recommended Settings for Different Use Cases:');
      WriteLn;
      WriteLn('   Desktop Application:');
      WriteLn('   - JournalMode = jmWAL');
      WriteLn('   - SyncMode = smNormal');
      WriteLn('   - CacheSize = 8192 (8 MB)');
      WriteLn('   - BusyTimeout = 5000');
      WriteLn('   - ForeignKeys = True');
      WriteLn;
      WriteLn('   Server/High Concurrency:');
      WriteLn('   - JournalMode = jmWAL');
      WriteLn('   - SyncMode = smNormal');
      WriteLn('   - CacheSize = 32768 (32 MB)');
      WriteLn('   - BusyTimeout = 30000');
      WriteLn('   - ForeignKeys = True');
      WriteLn;
      WriteLn('   Embedded/Memory Constrained:');
      WriteLn('   - JournalMode = jmDelete');
      WriteLn('   - SyncMode = smFull');
      WriteLn('   - CacheSize = 2000 (2 MB)');
      WriteLn('   - BusyTimeout = 1000');
      WriteLn('   - ForeignKeys = True');
      WriteLn;

      Connection.Close;

    finally
      Connection.Free;
    end;

  finally
    Options.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then
    DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then
    DeleteFile(DBPath + '-shm');
end.
