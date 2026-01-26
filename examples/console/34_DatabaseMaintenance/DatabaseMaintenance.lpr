{===============================================================================
  NDXSQLite Example 34 - Database Maintenance
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - VACUUM to reclaim space
  - ANALYZE to update statistics
  - PRAGMA integrity_check
  - PRAGMA quick_check
  - REINDEX to rebuild indexes
  - PRAGMA optimize
  - Database file size monitoring

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DatabaseMaintenance;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Returns the file size. }
function GetFileSize(const AFileName: string): Int64;
var
  F: file of Byte;
begin
  Result := 0;
  if FileExists(AFileName) then
  begin
    AssignFile(F, AFileName);
    Reset(F);
    try
      Result := FileSize(F);
    finally
      CloseFile(F);
    end;
  end;
end;

{ Creates the 'data' table with two indexes and inserts 10,000 rows of test data with random values and BLOB payloads. }
procedure SetupDatabaseWithData;
var
  I: Integer;
begin
  WriteLn('Setting up test database...');

  Connection.ExecuteNonQuery(
    'CREATE TABLE data (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  value REAL,' +
    '  payload BLOB' +
    ')');

  Connection.ExecuteNonQuery('CREATE INDEX idx_data_name ON data(name)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_data_value ON data(value)');

  // Insert lots of data
  Connection.BeginTransaction;
  for I := 1 to 10000 do
  begin
    Connection.ExecuteNonQuery(
      'INSERT INTO data (name, value, payload) VALUES (?, ?, ?)',
      [Format('Item_%d', [I]), Random * 10000, StringOfChar('X', 100)]);
  end;
  Connection.Commit;

  WriteLn('Inserted 10,000 rows');
  WriteLn('');
end;

{ Runs PRAGMA integrity_check and prints its result to verify the database is not corrupted. }
procedure DemoIntegrityCheck;
var
  DS: TDataSet;
begin
  WriteLn('1. PRAGMA integrity_check');
  WriteLn('   -----------------------');

  DS := Connection.ExecuteQuery('PRAGMA integrity_check');
  try
    WriteLn('   Result:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.Fields[0].AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   (''ok'' means database is not corrupted)');
  WriteLn('');
end;

{ Runs PRAGMA quick_check, a faster but less thorough integrity verification that skips UNIQUE and index content checks. }
procedure DemoQuickCheck;
var
  DS: TDataSet;
begin
  WriteLn('2. PRAGMA quick_check (faster, less thorough)');
  WriteLn('   -------------------------------------------');

  DS := Connection.ExecuteQuery('PRAGMA quick_check');
  try
    WriteLn('   Result:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.Fields[0].AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   quick_check skips verifying UNIQUE constraints and index content');
  WriteLn('');
end;

{ Demonstrates the ANALYZE command for gathering index statistics. }
procedure DemoAnalyze;
var
  DS: TDataSet;
  StatCount: Integer;
begin
  WriteLn('3. ANALYZE - Update query optimizer statistics');
  WriteLn('   --------------------------------------------');

  // Check if stats exist (sqlite_stat1 may not exist yet)
  DS := Connection.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM sqlite_master WHERE type=''table'' AND name=''sqlite_stat1''');
  try
    if DS.FieldByName('cnt').AsInteger > 0 then
    begin
      DS.Free;
      DS := Connection.ExecuteQuery('SELECT COUNT(*) as cnt FROM sqlite_stat1');
      StatCount := DS.FieldByName('cnt').AsInteger;
    end
    else
      StatCount := 0;
    WriteLn('   Statistics before ANALYZE: ', StatCount, ' entries');
  finally
    DS.Free;
  end;

  // Run ANALYZE
  Connection.ExecuteNonQuery('ANALYZE');
  WriteLn('   Ran ANALYZE');

  // Check stats again
  DS := Connection.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM sqlite_stat1');
  try
    WriteLn('   Statistics after ANALYZE: ', DS.FieldByName('cnt').AsInteger, ' entries');
  finally
    DS.Free;
  end;

  // Show sample stats
  DS := Connection.ExecuteQuery('SELECT * FROM sqlite_stat1 LIMIT 5');
  try
    WriteLn('   Sample statistics:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s.%s: %s', [
        DS.FieldByName('tbl').AsString,
        DS.FieldByName('idx').AsString,
        DS.FieldByName('stat').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Statistics help SQLite choose optimal query plans');
  WriteLn('');
end;

{ Demonstrates the VACUUM command to reclaim unused disk space. }
procedure DemoVacuum;
var
  SizeBefore, SizeAfter: Int64;
begin
  WriteLn('4. VACUUM - Reclaim unused space');
  WriteLn('   ------------------------------');

  // Delete half the data to create free space
  Connection.ExecuteNonQuery('DELETE FROM data WHERE id % 2 = 0');
  WriteLn('   Deleted 5,000 rows');

  SizeBefore := GetFileSize(DBPath);
  WriteLn('   File size before VACUUM: ', SizeBefore, ' bytes');

  // VACUUM rebuilds the database file
  Connection.ExecuteNonQuery('VACUUM');
  WriteLn('   Ran VACUUM');

  SizeAfter := GetFileSize(DBPath);
  WriteLn('   File size after VACUUM: ', SizeAfter, ' bytes');
  WriteLn('   Space reclaimed: ', SizeBefore - SizeAfter, ' bytes');

  WriteLn('');
end;

{ Demonstrates the REINDEX command to rebuild database indexes. }
procedure DemoReindex;
begin
  WriteLn('5. REINDEX - Rebuild indexes');
  WriteLn('   --------------------------');

  // Reindex specific index
  Connection.ExecuteNonQuery('REINDEX idx_data_name');
  WriteLn('   Rebuilt: idx_data_name');

  // Reindex all indexes on a table
  Connection.ExecuteNonQuery('REINDEX data');
  WriteLn('   Rebuilt all indexes on: data');

  // Reindex entire database
  Connection.ExecuteNonQuery('REINDEX');
  WriteLn('   Rebuilt all indexes in database');

  WriteLn('   REINDEX is useful after changing collation or after corruption');
  WriteLn('');
end;

{ Runs PRAGMA optimize which selectively analyzes tables that would benefit based on recent query patterns. }
procedure DemoPragmaOptimize;
begin
  WriteLn('6. PRAGMA optimize - Auto-optimization');
  WriteLn('   ------------------------------------');

  // optimize runs ANALYZE on tables that would benefit
  Connection.ExecuteNonQuery('PRAGMA optimize');
  WriteLn('   Ran PRAGMA optimize');

  WriteLn('   optimize analyzes tables that need it based on query patterns');
  WriteLn('   Best practice: run before closing long-lived connections');
  WriteLn('');
end;

{ Queries and displays the current auto_vacuum setting and explains the available modes (none, full, incremental). }
procedure DemoAutoVacuum;
var
  DS: TDataSet;
begin
  WriteLn('7. Auto-vacuum settings');
  WriteLn('   ---------------------');

  DS := Connection.ExecuteQuery('PRAGMA auto_vacuum');
  try
    WriteLn('   Current auto_vacuum: ', DS.Fields[0].AsInteger);
    WriteLn('   (0=none, 1=full, 2=incremental)');
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Auto-vacuum modes:');
  WriteLn('     0 (none): Manual VACUUM required');
  WriteLn('     1 (full): Automatic after each transaction');
  WriteLn('     2 (incremental): Use PRAGMA incremental_vacuum(N)');

  WriteLn('');
  WriteLn('   Note: auto_vacuum must be set before creating tables');
  WriteLn('');
end;

{ Queries and displays page_size, page_count, and freelist_count PRAGMAs to show database storage metrics. }
procedure DemoPageInfo;
var
  DS: TDataSet;
begin
  WriteLn('8. Database page information');
  WriteLn('   --------------------------');

  DS := Connection.ExecuteQuery('PRAGMA page_size');
  try
    WriteLn('   Page size: ', DS.Fields[0].AsInteger, ' bytes');
  finally
    DS.Free;
  end;

  DS := Connection.ExecuteQuery('PRAGMA page_count');
  try
    WriteLn('   Page count: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  DS := Connection.ExecuteQuery('PRAGMA freelist_count');
  try
    WriteLn('   Free pages: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates parent/child tables, inserts an orphan row with FK enforcement disabled, then runs PRAGMA foreign_key_check to detect violations. }
procedure DemoForeignKeyCheck;
var
  DS: TDataSet;
begin
  WriteLn('9. Foreign key integrity check');
  WriteLn('   ----------------------------');

  // Create tables with FK
  Connection.ExecuteNonQuery('CREATE TABLE parent (id INTEGER PRIMARY KEY, name TEXT)');
  Connection.ExecuteNonQuery(
    'CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER REFERENCES parent(id))');

  Connection.ExecuteNonQuery('INSERT INTO parent VALUES (1, ''Parent1'')');
  Connection.ExecuteNonQuery('INSERT INTO child VALUES (1, 1)');

  // Disable FK to create violation
  Connection.ExecuteNonQuery('PRAGMA foreign_keys = OFF');
  Connection.ExecuteNonQuery('INSERT INTO child VALUES (2, 999)'); // Orphan!
  Connection.ExecuteNonQuery('PRAGMA foreign_keys = ON');

  DS := Connection.ExecuteQuery('PRAGMA foreign_key_check');
  try
    if DS.IsEmpty then
      WriteLn('   No FK violations found')
    else
    begin
      WriteLn('   FK violations found:');
      while not DS.EOF do
      begin
        WriteLn(Format('     Table: %s, RowID: %s, Parent: %s',
          [DS.Fields[0].AsString, DS.Fields[1].AsString, DS.Fields[2].AsString]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Prints a recommended maintenance schedule outlining when to run optimize, ANALYZE, integrity_check, VACUUM, and REINDEX. }
procedure DemoMaintenanceSchedule;
begin
  WriteLn('10. Recommended maintenance schedule');
  WriteLn('    ---------------------------------');
  WriteLn('');
  WriteLn('    Daily (busy databases):');
  WriteLn('      PRAGMA optimize;');
  WriteLn('');
  WriteLn('    Weekly:');
  WriteLn('      ANALYZE;');
  WriteLn('      PRAGMA integrity_check;');
  WriteLn('');
  WriteLn('    Monthly (or after heavy deletions):');
  WriteLn('      VACUUM;');
  WriteLn('');
  WriteLn('    After schema changes:');
  WriteLn('      REINDEX;');
  WriteLn('      ANALYZE;');
  WriteLn('');
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 34: Database Maintenance ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example34.db';
  Cleanup;
  Randomize;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupDatabaseWithData;

      DemoIntegrityCheck;
      DemoQuickCheck;
      DemoAnalyze;
      DemoVacuum;
      DemoReindex;
      DemoPragmaOptimize;
      DemoAutoVacuum;
      DemoPageInfo;
      DemoForeignKeyCheck;
      DemoMaintenanceSchedule;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
