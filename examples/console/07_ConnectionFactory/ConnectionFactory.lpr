{===============================================================================
  NDXSQLite Example 07 - Connection Factory
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Using TNDXSQLiteConnectionFactory
  - Creating different connection types
  - Factory pattern benefits
  - Connection options management

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ConnectionFactory;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteconnectionfactory,
  ndxsqliteconnectionoptions, ndxsqlitetypes;

var
  Factory: INDXSQLiteConnectionFactory;
  Conn1, Conn2, Conn3: INDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

begin
  WriteLn('=== NDXSQLite Example 07: Connection Factory ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example07.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  // 1. Create factory with simple path
  WriteLn('1. Creating factory with database path...');
  Factory := TNDXSQLiteConnectionFactory.Create(DBPath);
  WriteLn('   Factory created.');
  WriteLn;

  // 2. Create standard connection
  WriteLn('2. Creating standard connection:');
  Conn1 := Factory.CreateConnection;
  Conn1.Open;
  WriteLn('   Connection ID: ', Conn1.Id);
  WriteLn('   Database path: ', Conn1.DatabasePath);
  WriteLn('   Is primary: ', Conn1.IsPrimaryConnection);

  // Setup test data
  Conn1.ExecuteNonQuery(
    'CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)');
  Conn1.ExecuteNonQuery('INSERT INTO test VALUES (1, ''Hello'')');
  WriteLn('   Test data created.');
  WriteLn;

  // 3. Create primary connection
  WriteLn('3. Creating primary connection:');
  Conn2 := Factory.CreatePrimaryConnection;
  Conn2.Open;
  WriteLn('   Connection ID: ', Conn2.Id);
  WriteLn('   Is primary: ', Conn2.IsPrimaryConnection);
  WriteLn('   Primary connections don''t auto-close.');
  WriteLn;

  // 4. Create read-only connection
  WriteLn('4. Creating read-only connection:');
  Conn3 := Factory.CreateReadOnlyConnection;
  Conn3.Open;
  WriteLn('   Connection ID: ', Conn3.Id);
  WriteLn('   Can read: ', Conn3.ExecuteScalar('SELECT value FROM test WHERE id = 1'));

  try
    Conn3.ExecuteNonQuery('INSERT INTO test VALUES (2, ''World'')');
    WriteLn('   ERROR: Write succeeded on read-only connection!');
  except
    on E: Exception do
      WriteLn('   Write attempt blocked (expected): read-only');
  end;
  WriteLn;

  // Close connections before testing memory database
  Conn1.Close;
  Conn2.Close;
  Conn3.Close;

  // 5. Create memory connection
  WriteLn('5. Creating in-memory connection:');
  Conn1 := Factory.CreateMemoryConnection;
  Conn1.Open;
  WriteLn('   Database path: ', Conn1.DatabasePath);

  Conn1.ExecuteNonQuery('CREATE TABLE memory_test (x INTEGER)');
  Conn1.ExecuteNonQuery('INSERT INTO memory_test VALUES (42)');
  WriteLn('   Memory database works: ',
    Conn1.ExecuteScalar('SELECT x FROM memory_test'));
  WriteLn('   (Data lost when connection closes)');
  Conn1.Close;
  WriteLn;

  // 6. Create factory with full options
  WriteLn('6. Factory with custom options:');
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;
    Options.JournalMode := jmWAL;
    Options.CacheSize := 4096;
    Options.BusyTimeout := 5000;
    Options.ForeignKeys := True;

    Factory := TNDXSQLiteConnectionFactory.Create(Options);

    Conn1 := Factory.CreateConnection;
    Conn1.Open;

    WriteLn('   Journal mode: ', Conn1.GetPragmaValue('journal_mode'));
    WriteLn('   Cache size: ', Conn1.GetCacheSize, ' KB');
    WriteLn('   Busy timeout: ', Conn1.GetBusyTimeout, ' ms');
    WriteLn('   Foreign keys: ', Conn1.IsForeignKeysEnabled);

    Conn1.Close;
  finally
    Options.Free;
  end;
  WriteLn;

  // 7. Multiple connections from same factory
  WriteLn('7. Multiple connections from same factory:');
  Conn1 := Factory.CreateConnection;
  Conn2 := Factory.CreateConnection;
  Conn3 := Factory.CreateConnection;

  Conn1.Open;
  Conn2.Open;
  Conn3.Open;

  WriteLn('   Connection 1 ID: ', Conn1.Id);
  WriteLn('   Connection 2 ID: ', Conn2.Id);
  WriteLn('   Connection 3 ID: ', Conn3.Id);
  WriteLn('   (Each connection has unique ID)');

  Conn1.Close;
  Conn2.Close;
  Conn3.Close;
  WriteLn;

  // 8. Benefits of factory pattern
  WriteLn('8. Benefits of Factory Pattern:');
  WriteLn('   - Centralized configuration');
  WriteLn('   - Consistent connection settings');
  WriteLn('   - Easy to create specialized connections');
  WriteLn('   - Decouples creation from usage');
  WriteLn('   - Simplifies testing (can mock factory)');
  WriteLn;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then
    DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then
    DeleteFile(DBPath + '-shm');
end.
