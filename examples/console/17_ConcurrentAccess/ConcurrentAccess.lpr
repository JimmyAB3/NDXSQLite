{===============================================================================
  NDXSQLite Example 17 - Concurrent Access
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Multi-threaded database access
  - WAL mode for concurrent reads/writes
  - Busy timeout handling
  - Thread-safe patterns

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ConcurrentAccess;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions, ndxsqlitetypes;

var
  DBPath: string;
  ReadCount: Integer = 0;
  WriteCount: Integer = 0;
  ErrorCount: Integer = 0;
  Lock: TRTLCriticalSection;

type
  TReaderThread = class(TThread)
  private
    FMyThreadId: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadId: Integer);
  end;

  TWriterThread = class(TThread)
  private
    FMyThreadId: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadId: Integer);
  end;

constructor TReaderThread.Create(AThreadId: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FMyThreadId := AThreadId;
end;

{ Executes concurrent read operations on the database. }
procedure TReaderThread.Execute;
var
  Conn: TNDXSQLiteConnection;
  Opts: TNDXSQLiteConnectionOptions;
  DS: TDataSet;
  I, Count: Integer;
begin
  Opts := TNDXSQLiteConnectionOptions.Create;
  try
    Opts.DatabasePath := DBPath;
    Opts.JournalMode := jmWAL;
    Opts.BusyTimeout := 5000;

    Conn := TNDXSQLiteConnection.Create(Opts);
    try
      Conn.Open;

      for I := 1 to 20 do
      begin
        if Terminated then Break;
        try
          DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM data');
          try
            Count := DS.FieldByName('cnt').AsInteger;
            EnterCriticalSection(Lock);
            Inc(ReadCount);
            LeaveCriticalSection(Lock);
          finally
            DS.Free;
          end;
          Sleep(10 + Random(20));
        except
          on E: Exception do
          begin
            EnterCriticalSection(Lock);
            Inc(ErrorCount);
            LeaveCriticalSection(Lock);
          end;
        end;
      end;

      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Opts.Free;
  end;
end;

constructor TWriterThread.Create(AThreadId: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FMyThreadId := AThreadId;
end;

{ Executes concurrent write operations on the database. }
procedure TWriterThread.Execute;
var
  Conn: TNDXSQLiteConnection;
  Opts: TNDXSQLiteConnectionOptions;
  I: Integer;
begin
  Opts := TNDXSQLiteConnectionOptions.Create;
  try
    Opts.DatabasePath := DBPath;
    Opts.JournalMode := jmWAL;
    Opts.BusyTimeout := 5000;

    Conn := TNDXSQLiteConnection.Create(Opts);
    try
      Conn.Open;

      for I := 1 to 10 do
      begin
        if Terminated then Break;
        try
          Conn.BeginTransaction;
          try
            Conn.ExecuteNonQuery(Format(
              'INSERT INTO data (thread_id, value, created_at) VALUES (%d, %d, ''%s'')',
              [FMyThreadId, Random(1000), FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
            Conn.Commit;
            EnterCriticalSection(Lock);
            Inc(WriteCount);
            LeaveCriticalSection(Lock);
          except
            Conn.Rollback;
            raise;
          end;
          Sleep(20 + Random(30));
        except
          on E: Exception do
          begin
            EnterCriticalSection(Lock);
            Inc(ErrorCount);
            LeaveCriticalSection(Lock);
          end;
        end;
      end;

      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Opts.Free;
  end;
end;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
var
  Conn: TNDXSQLiteConnection;
  Opts: TNDXSQLiteConnectionOptions;
begin
  Opts := TNDXSQLiteConnectionOptions.Create;
  try
    Opts.DatabasePath := DBPath;
    Opts.JournalMode := jmWAL;

    Conn := TNDXSQLiteConnection.Create(Opts);
    try
      Conn.Open;
      Conn.ExecuteNonQuery(
        'CREATE TABLE IF NOT EXISTS data (' +
        '  id INTEGER PRIMARY KEY,' +
        '  thread_id INTEGER,' +
        '  value INTEGER,' +
        '  created_at TEXT' +
        ')');
      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Opts.Free;
  end;
end;

{ Opens a connection in WAL journal mode and displays its concurrency advantages. }
procedure DemoWALMode;
var
  Conn: TNDXSQLiteConnection;
  Opts: TNDXSQLiteConnectionOptions;
begin
  WriteLn('1. WAL Mode Configuration');
  WriteLn('   ----------------------');

  Opts := TNDXSQLiteConnectionOptions.Create;
  try
    Opts.DatabasePath := DBPath;
    Opts.JournalMode := jmWAL;

    Conn := TNDXSQLiteConnection.Create(Opts);
    try
      Conn.Open;
      WriteLn('   Journal mode: ', Conn.GetPragmaValue('journal_mode'));
      WriteLn('   WAL advantages:');
      WriteLn('   - Readers do not block writers');
      WriteLn('   - Writers do not block readers');
      WriteLn('   - Better performance for many small transactions');
      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Opts.Free;
  end;
  WriteLn('');
end;

{ Configures a 5-second busy timeout so SQLite retries on lock contention instead of failing immediately. }
procedure DemoBusyTimeout;
var
  Conn: TNDXSQLiteConnection;
  Opts: TNDXSQLiteConnectionOptions;
begin
  WriteLn('2. Busy Timeout Configuration');
  WriteLn('   --------------------------');

  Opts := TNDXSQLiteConnectionOptions.Create;
  try
    Opts.DatabasePath := DBPath;
    Opts.BusyTimeout := 5000;  // 5 seconds

    Conn := TNDXSQLiteConnection.Create(Opts);
    try
      Conn.Open;
      WriteLn('   Busy timeout: ', Conn.GetBusyTimeout, ' ms');
      WriteLn('   SQLite will retry for 5 seconds before SQLITE_BUSY error');
      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Opts.Free;
  end;
  WriteLn('');
end;

{ Spawns 5 reader threads that simultaneously query the database and reports the total read count. }
procedure DemoConcurrentReads;
var
  Readers: array[0..4] of TReaderThread;
  I: Integer;
  StartTime: TDateTime;
begin
  WriteLn('3. Concurrent Reads (5 readers)');
  WriteLn('   ----------------------------');

  ReadCount := 0;
  ErrorCount := 0;

  StartTime := Now;
  for I := 0 to 4 do
    Readers[I] := TReaderThread.Create(I + 1);

  // Wait for completion
  while ReadCount < 100 do
  begin
    Sleep(50);
    if MilliSecondsBetween(Now, StartTime) > 10000 then Break;
  end;
  Sleep(200);  // Extra wait for thread cleanup

  WriteLn(Format('   Completed: %d reads, %d errors', [ReadCount, ErrorCount]));
  WriteLn('   Multiple readers can access simultaneously in WAL mode');
  WriteLn('');
end;

{ Runs 3 reader threads and 2 writer threads simultaneously to verify WAL-mode concurrent access. }
procedure DemoConcurrentReadWrite;
var
  Readers: array[0..2] of TReaderThread;
  Writers: array[0..1] of TWriterThread;
  I: Integer;
  StartTime: TDateTime;
begin
  WriteLn('4. Concurrent Reads + Writes (3 readers, 2 writers)');
  WriteLn('   ------------------------------------------------');

  ReadCount := 0;
  WriteCount := 0;
  ErrorCount := 0;

  StartTime := Now;

  // Start readers
  for I := 0 to 2 do
    Readers[I] := TReaderThread.Create(I + 1);

  // Start writers
  for I := 0 to 1 do
    Writers[I] := TWriterThread.Create(I + 100);

  // Wait for completion
  while (ReadCount < 60) or (WriteCount < 20) do
  begin
    Sleep(50);
    if MilliSecondsBetween(Now, StartTime) > 15000 then Break;
  end;
  Sleep(300);  // Extra wait for thread cleanup

  WriteLn(Format('   Completed: %d reads, %d writes, %d errors', [ReadCount, WriteCount, ErrorCount]));
  WriteLn('   WAL mode allows concurrent reads during writes');
  WriteLn('');
end;

{ Prints recommended practices for safe multi-threaded SQLite access. }
procedure DemoBestPractices;
begin
  WriteLn('5. Concurrency Best Practices');
  WriteLn('   --------------------------');
  WriteLn('   - Use WAL mode for concurrent access');
  WriteLn('   - Set appropriate busy_timeout (e.g., 5000 ms)');
  WriteLn('   - Use short transactions');
  WriteLn('   - Handle SQLITE_BUSY errors gracefully');
  WriteLn('   - Use connection pooling for thread safety');
  WriteLn('   - Each thread should have its own connection');
  WriteLn('   - Avoid long-running transactions');
  WriteLn('   - Consider read-only connections for readers');
  WriteLn('');
end;

{ Deletes the database file and its WAL/SHM companion files. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  WriteLn('=== NDXSQLite Example 17: Concurrent Access ===');
  WriteLn('');

  InitCriticalSection(Lock);
  Randomize;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example17.db';
  Cleanup;

  SetupDatabase;

  DemoWALMode;
  DemoBusyTimeout;
  DemoConcurrentReads;
  DemoConcurrentReadWrite;
  DemoBestPractices;

  DoneCriticalSection(Lock);
  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
