{===============================================================================
  NDXSQLite Example 35 - WAL Mode and Checkpointing
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Enabling WAL (Write-Ahead Logging) mode
  - Understanding WAL files (-wal, -shm)
  - Manual checkpointing
  - Checkpoint modes (PASSIVE, FULL, RESTART, TRUNCATE)
  - WAL size monitoring
  - Auto-checkpoint configuration

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program WALCheckpointing;

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

{ Displays wal files information. }
procedure ShowWALFiles;
var
  DBSize, WALSize, SHMSize: Int64;
begin
  DBSize := GetFileSize(DBPath);
  WALSize := GetFileSize(DBPath + '-wal');
  SHMSize := GetFileSize(DBPath + '-shm');

  WriteLn('   Database files:');
  WriteLn('     Main DB:  ', DBSize, ' bytes');
  WriteLn('     WAL file: ', WALSize, ' bytes');
  WriteLn('     SHM file: ', SHMSize, ' bytes');
end;

{ Switches the database journal mode from its default to WAL and prints the mode before and after. }
procedure DemoEnableWAL;
var
  DS: TDataSet;
begin
  WriteLn('1. Enabling WAL mode');
  WriteLn('   ------------------');

  // Check current mode
  DS := Connection.ExecuteQuery('PRAGMA journal_mode');
  try
    WriteLn('   Current journal mode: ', DS.Fields[0].AsString);
  finally
    DS.Free;
  end;

  // Enable WAL
  DS := Connection.ExecuteQuery('PRAGMA journal_mode = WAL');
  try
    WriteLn('   After change: ', DS.Fields[0].AsString);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   WAL advantages:');
  WriteLn('     - Better concurrency (readers don''t block writers)');
  WriteLn('     - Faster for many small transactions');
  WriteLn('     - Atomic commits without blocking reads');
  WriteLn('');
end;

{ Inserts 1000 rows and shows WAL file size before and after to illustrate how the WAL file grows with writes. }
procedure DemoWALGrowth;
var
  I: Integer;
begin
  WriteLn('2. WAL file growth');
  WriteLn('   ----------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS data (id INTEGER PRIMARY KEY, value TEXT)');

  ShowWALFiles;

  WriteLn('');
  WriteLn('   Inserting 1000 rows...');

  Connection.BeginTransaction;
  for I := 1 to 1000 do
    Connection.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)',
      [StringOfChar('X', 100)]);
  Connection.Commit;

  ShowWALFiles;

  WriteLn('');
  WriteLn('   Note: WAL grows with writes, main DB unchanged until checkpoint');
  WriteLn('');
end;

{ Runs a PASSIVE checkpoint that transfers WAL pages to the main database without blocking readers or writers, and reports results. }
procedure DemoCheckpointPassive;
var
  DS: TDataSet;
begin
  WriteLn('3. Checkpoint PASSIVE');
  WriteLn('   -------------------');

  WriteLn('   Before checkpoint:');
  ShowWALFiles;

  // PASSIVE: checkpoint as much as possible without blocking
  DS := Connection.ExecuteQuery('PRAGMA wal_checkpoint(PASSIVE)');
  try
    WriteLn('');
    WriteLn('   PRAGMA wal_checkpoint(PASSIVE) result:');
    WriteLn('     Busy: ', DS.Fields[0].AsInteger, ' (0=success, 1=blocked)');
    WriteLn('     Log pages: ', DS.Fields[1].AsInteger);
    WriteLn('     Checkpointed: ', DS.Fields[2].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   After PASSIVE checkpoint:');
  ShowWALFiles;

  WriteLn('');
  WriteLn('   PASSIVE: Non-blocking, may not checkpoint everything');
  WriteLn('');
end;

{ Inserts 500 rows then runs a FULL checkpoint that waits for writers to finish and checkpoints all WAL pages. }
procedure DemoCheckpointFull;
var
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('4. Checkpoint FULL');
  WriteLn('   ----------------');

  // Add more data
  Connection.BeginTransaction;
  for I := 1 to 500 do
    Connection.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)',
      [StringOfChar('Y', 100)]);
  Connection.Commit;

  WriteLn('   Before FULL checkpoint:');
  ShowWALFiles;

  // FULL: blocks writers until complete, but not readers
  DS := Connection.ExecuteQuery('PRAGMA wal_checkpoint(FULL)');
  try
    WriteLn('');
    WriteLn('   PRAGMA wal_checkpoint(FULL) result:');
    WriteLn('     Busy: ', DS.Fields[0].AsInteger);
    WriteLn('     Log pages: ', DS.Fields[1].AsInteger);
    WriteLn('     Checkpointed: ', DS.Fields[2].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   After FULL checkpoint:');
  ShowWALFiles;

  WriteLn('');
  WriteLn('   FULL: Waits for writers, checkpoints everything');
  WriteLn('');
end;

{ Inserts 500 rows then runs a TRUNCATE checkpoint that checkpoints all pages and resets the WAL file to zero bytes. }
procedure DemoCheckpointTruncate;
var
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('5. Checkpoint TRUNCATE');
  WriteLn('   --------------------');

  // Add more data
  Connection.BeginTransaction;
  for I := 1 to 500 do
    Connection.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)',
      [StringOfChar('Z', 100)]);
  Connection.Commit;

  WriteLn('   Before TRUNCATE checkpoint:');
  ShowWALFiles;

  // TRUNCATE: like FULL but also truncates WAL file to zero
  DS := Connection.ExecuteQuery('PRAGMA wal_checkpoint(TRUNCATE)');
  try
    WriteLn('');
    WriteLn('   PRAGMA wal_checkpoint(TRUNCATE) result:');
    WriteLn('     Busy: ', DS.Fields[0].AsInteger);
    WriteLn('     Log pages: ', DS.Fields[1].AsInteger);
    WriteLn('     Checkpointed: ', DS.Fields[2].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   After TRUNCATE checkpoint:');
  ShowWALFiles;

  WriteLn('');
  WriteLn('   TRUNCATE: Resets WAL to zero bytes (best for backups)');
  WriteLn('');
end;

{ Reads, changes, disables, and re-enables the wal_autocheckpoint threshold that controls automatic checkpointing. }
procedure DemoAutoCheckpoint;
var
  DS: TDataSet;
begin
  WriteLn('6. Auto-checkpoint configuration');
  WriteLn('   ------------------------------');

  // Check current setting
  DS := Connection.ExecuteQuery('PRAGMA wal_autocheckpoint');
  try
    WriteLn('   Current auto-checkpoint threshold: ', DS.Fields[0].AsInteger, ' pages');
  finally
    DS.Free;
  end;

  // Change threshold (default is 1000 pages)
  Connection.ExecuteNonQuery('PRAGMA wal_autocheckpoint = 500');

  DS := Connection.ExecuteQuery('PRAGMA wal_autocheckpoint');
  try
    WriteLn('   New threshold: ', DS.Fields[0].AsInteger, ' pages');
  finally
    DS.Free;
  end;

  // Disable auto-checkpoint
  Connection.ExecuteNonQuery('PRAGMA wal_autocheckpoint = 0');
  WriteLn('   Disabled (set to 0) - manual checkpoint only');

  // Re-enable
  Connection.ExecuteNonQuery('PRAGMA wal_autocheckpoint = 1000');
  WriteLn('   Re-enabled at 1000 pages (default)');

  WriteLn('');
  WriteLn('   Auto-checkpoint runs automatically after this many WAL pages');
  WriteLn('');
end;

{ Prints a comparison table of the four checkpoint modes (PASSIVE, FULL, RESTART, TRUNCATE) and their blocking behavior. }
procedure DemoCheckpointModes;
begin
  WriteLn('7. Checkpoint modes comparison');
  WriteLn('   ----------------------------');
  WriteLn('');
  WriteLn('   | Mode     | Blocks Writers | Blocks Readers | Truncates WAL |');
  WriteLn('   |----------|----------------|----------------|---------------|');
  WriteLn('   | PASSIVE  | No             | No             | No            |');
  WriteLn('   | FULL     | Yes (waits)    | No             | No            |');
  WriteLn('   | RESTART  | Yes (waits)    | Yes (briefly)  | No            |');
  WriteLn('   | TRUNCATE | Yes (waits)    | Yes (briefly)  | Yes           |');
  WriteLn('');
  WriteLn('   Use PASSIVE for regular maintenance');
  WriteLn('   Use TRUNCATE before backup to minimize WAL size');
  WriteLn('');
end;

{ Displays the current synchronous mode and explains the available levels (OFF, NORMAL, FULL, EXTRA) in the context of WAL. }
procedure DemoSynchronousWithWAL;
var
  DS: TDataSet;
begin
  WriteLn('8. Synchronous mode with WAL');
  WriteLn('   --------------------------');

  DS := Connection.ExecuteQuery('PRAGMA synchronous');
  try
    WriteLn('   Current synchronous: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Synchronous modes with WAL:');
  WriteLn('     OFF (0):    No sync - fast but risky');
  WriteLn('     NORMAL (1): Sync at critical moments (recommended for WAL)');
  WriteLn('     FULL (2):   Sync after each write');
  WriteLn('     EXTRA (3):  Extra sync (overkill with WAL)');
  WriteLn('');
  WriteLn('   WAL + NORMAL gives good durability with performance');
  WriteLn('');
end;

{ Reads and sets the busy_timeout PRAGMA to 5000 ms to prevent SQLITE_BUSY errors during concurrent access. }
procedure DemoBusyTimeout;
var
  DS: TDataSet;
begin
  WriteLn('9. Busy timeout for checkpointing');
  WriteLn('   --------------------------------');

  DS := Connection.ExecuteQuery('PRAGMA busy_timeout');
  try
    WriteLn('   Current busy_timeout: ', DS.Fields[0].AsInteger, ' ms');
  finally
    DS.Free;
  end;

  // Set a reasonable timeout
  Connection.ExecuteNonQuery('PRAGMA busy_timeout = 5000');

  DS := Connection.ExecuteQuery('PRAGMA busy_timeout');
  try
    WriteLn('   New busy_timeout: ', DS.Fields[0].AsInteger, ' ms');
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   busy_timeout prevents SQLITE_BUSY errors during concurrent access');
  WriteLn('');
end;

{ Performs a TRUNCATE checkpoint then switches the journal mode back to DELETE, which removes the WAL and SHM files. }
procedure DemoSwitchBackToDelete;
var
  DS: TDataSet;
begin
  WriteLn('10. Switching back to DELETE mode');
  WriteLn('    -------------------------------');

  // First checkpoint everything
  Connection.ExecuteNonQuery('PRAGMA wal_checkpoint(TRUNCATE)');
  WriteLn('    Checkpointed all data');

  // Switch back to DELETE journal mode
  DS := Connection.ExecuteQuery('PRAGMA journal_mode = DELETE');
  try
    WriteLn('    Journal mode: ', DS.Fields[0].AsString);
  finally
    DS.Free;
  end;

  WriteLn('');
  ShowWALFiles;
  WriteLn('');
  WriteLn('    WAL and SHM files removed when switching away from WAL mode');
  WriteLn('');
end;

{ Deletes the main database file and its WAL and SHM companion files from disk if they exist. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  WriteLn('=== NDXSQLite Example 35: WAL Mode and Checkpointing ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example35.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      DemoEnableWAL;
      DemoWALGrowth;
      DemoCheckpointPassive;
      DemoCheckpointFull;
      DemoCheckpointTruncate;
      DemoAutoCheckpoint;
      DemoCheckpointModes;
      DemoSynchronousWithWAL;
      DemoBusyTimeout;
      DemoSwitchBackToDelete;

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
