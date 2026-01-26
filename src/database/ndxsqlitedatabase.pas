{===============================================================================
  NDXSQLite - Native Database Wrapper
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 2 (depends on ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions)

  Low-level wrapper for sqlite3* database handle.
  Provides clean object-oriented access to SQLite database connections.
===============================================================================}
unit ndxsqlitedatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs,
  ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions;

type
  { Forward declaration }
  TNDXSQLiteDatabase = class;

  { Transaction types }
  TNDXTransactionType = (
    ttDeferred,    // Default - acquire lock when needed
    ttImmediate,   // Acquire RESERVED lock immediately
    ttExclusive    // Acquire EXCLUSIVE lock immediately
  );

  { Backup progress callback }
  TNDXBackupProgressCallback = procedure(Remaining, Total: Integer;
    var Cancel: Boolean) of object;

  { Trace callback }
  TNDXTraceCallback = procedure(const SQL: string) of object;

  { Profile callback }
  TNDXProfileCallback = procedure(const SQL: string; TimeNs: UInt64) of object;

  { Update callback }
  TNDXUpdateCallback = procedure(Operation: Integer; const DatabaseName,
    TableName: string; RowId: Int64) of object;

  { Commit callback - return True to rollback }
  TNDXCommitCallback = function: Boolean of object;

  { Rollback callback }
  TNDXRollbackCallback = procedure of object;

  { Busy callback - return True to retry }
  TNDXBusyCallback = function(Count: Integer): Boolean of object;

  { Progress callback - return True to interrupt }
  TNDXProgressCallback = function: Boolean of object;

  { Native SQLite database wrapper }
  TNDXSQLiteDatabase = class
  private
    FHandle: Psqlite3;
    FDatabasePath: string;
    FIsOpen: Boolean;
    FInTransaction: Boolean;
    FTransactionType: TNDXTransactionType;
    FSavepointLevel: Integer;
    FBusyTimeoutMs: Integer;
    FLock: TCriticalSection;
    FOwnsLock: Boolean;

    // Callbacks
    FOnTrace: TNDXTraceCallback;
    FOnProfile: TNDXProfileCallback;
    FOnUpdate: TNDXUpdateCallback;
    FOnCommit: TNDXCommitCallback;
    FOnRollback: TNDXRollbackCallback;
    FOnBusy: TNDXBusyCallback;
    FOnProgress: TNDXProgressCallback;

    procedure CheckOpen;
    procedure CheckClosed;
    procedure CheckResult(AResult: Integer; const AContext: string = '');
    procedure SetBusyTimeout(AValue: Integer);
    procedure InternalSetCallbacks;

    function GetLastErrorCode: Integer;
    function GetLastErrorMessage: string;
    function GetLastInsertRowId: Int64;
    function GetChanges: Integer;
    function GetTotalChanges: Integer;
    function GetAutoCommit: Boolean;
    function GetReadOnly: Boolean;

  public
    constructor Create; overload;
    constructor Create(ALock: TCriticalSection); overload;
    destructor Destroy; override;

    { Connection management }

    { Opens a database at the given path with optional flags. Uses default read/write/create flags if -1. }
    procedure Open(const APath: string; AFlags: Integer = -1);
    { Opens a database in read-only mode. }
    procedure OpenReadOnly(const APath: string);
    { Opens an in-memory database with an optional shared-cache name. }
    procedure OpenMemory(const AName: string = '');
    { Closes the database connection and releases the handle. }
    procedure Close;

    { Direct SQL execution }

    { Executes a SQL statement and returns the number of affected rows. }
    function Execute(const ASQL: string): Integer;
    { Executes a SQL query and returns the first column of the first row. }
    function ExecuteScalar(const ASQL: string): Variant;
    { Executes a SQL query and returns the first value, or ADefault if null. }
    function ExecuteScalarDef(const ASQL: string; ADefault: Variant): Variant;

    { Transaction management }

    { Begins a transaction with the specified locking type. }
    procedure BeginTransaction(AType: TNDXTransactionType = ttDeferred);
    { Commits the current transaction. }
    procedure Commit;
    { Rolls back the current transaction. }
    procedure Rollback;

    { Savepoint management }

    { Creates a named savepoint and returns its name. Auto-generates a name if empty. }
    function Savepoint(const AName: string = ''): string;
    { Releases a savepoint, committing its changes into the parent transaction. }
    procedure ReleaseSavepoint(const AName: string);
    { Rolls back to the named savepoint without ending the transaction. }
    procedure RollbackToSavepoint(const AName: string);

    { Pragma access }

    { Sets a PRAGMA value as a string. }
    procedure SetPragma(const AName, AValue: string);
    { Returns the current value of a PRAGMA as a string. }
    function GetPragma(const AName: string): string;
    { Sets a PRAGMA value as an integer. }
    procedure SetPragmaInt(const AName: string; AValue: Integer);
    { Returns the current value of a PRAGMA as an integer. }
    function GetPragmaInt(const AName: string): Integer;

    { Backup }

    { Creates a full backup of the database to the specified file path. }
    procedure BackupTo(const ADestPath: string;
      AProgressCallback: TNDXBackupProgressCallback = nil);
    { Creates a full backup to another open database instance. }
    procedure BackupToDatabase(ADest: TNDXSQLiteDatabase;
      AProgressCallback: TNDXBackupProgressCallback = nil);

    { WAL control }

    { Runs a WAL checkpoint with the specified mode. }
    procedure WalCheckpoint(AMode: Integer = SQLITE_CHECKPOINT_PASSIVE);
    { Switches the database journal mode to WAL. }
    procedure EnableWalMode;

    { Utility }

    { Interrupts any pending database operation on this connection. }
    procedure Interrupt;
    { Returns True if the named table exists in the database. }
    function TableExists(const ATableName: string): Boolean;
    { Returns True if the named index exists in the database. }
    function IndexExists(const AIndexName: string): Boolean;
    { Returns True if the named column exists in the specified table. }
    function ColumnExists(const ATableName, AColumnName: string): Boolean;
    { Returns a list of all table names in the database. Caller must free. }
    function GetTableList: TStringList;
    { Returns a list of column names for the specified table. Caller must free. }
    function GetColumnList(const ATableName: string): TStringList;

    { Schema information }

    { Returns the schema version number (PRAGMA schema_version). }
    function GetSchemaVersion: Integer;
    { Sets the schema version number. }
    procedure SetSchemaVersion(AVersion: Integer);
    { Returns the user version number (PRAGMA user_version). }
    function GetUserVersion: Integer;
    { Sets the user version number. }
    procedure SetUserVersion(AVersion: Integer);

    { Statistics }

    { Returns the total number of pages in the database file. }
    function GetPageCount: Integer;
    { Returns the page size in bytes. }
    function GetPageSize: Integer;
    { Returns the number of unused pages in the database file. }
    function GetFreePageCount: Integer;
    { Returns the total database file size in bytes. }
    function GetDatabaseSize: Int64;

    { Memory }

    { Releases unused memory held by this database connection. }
    procedure ReleaseMemory;

    { Thread safety }

    { Acquires the connection mutex. Must be paired with Unlock. }
    procedure Lock;
    { Releases the connection mutex. }
    procedure Unlock;
    { Attempts to acquire the mutex without blocking. Returns True on success. }
    function TryLock: Boolean;

    { Properties }
    property Handle: Psqlite3 read FHandle;
    property DatabasePath: string read FDatabasePath;
    property IsOpen: Boolean read FIsOpen;
    property InTransaction: Boolean read FInTransaction;
    property TransactionType: TNDXTransactionType read FTransactionType;
    property SavepointLevel: Integer read FSavepointLevel;
    property BusyTimeoutMs: Integer read FBusyTimeoutMs write SetBusyTimeout;
    property LastErrorCode: Integer read GetLastErrorCode;
    property LastErrorMessage: string read GetLastErrorMessage;
    property LastInsertRowId: Int64 read GetLastInsertRowId;
    property Changes: Integer read GetChanges;
    property TotalChanges: Integer read GetTotalChanges;
    property AutoCommit: Boolean read GetAutoCommit;
    property ReadOnly: Boolean read GetReadOnly;

    { Callbacks }
    property OnTrace: TNDXTraceCallback read FOnTrace write FOnTrace;
    property OnProfile: TNDXProfileCallback read FOnProfile write FOnProfile;
    property OnUpdate: TNDXUpdateCallback read FOnUpdate write FOnUpdate;
    property OnCommit: TNDXCommitCallback read FOnCommit write FOnCommit;
    property OnRollback: TNDXRollbackCallback read FOnRollback write FOnRollback;
    property OnBusy: TNDXBusyCallback read FOnBusy write FOnBusy;
    property OnProgress: TNDXProgressCallback read FOnProgress write FOnProgress;
  end;

implementation

uses
  Variants;

const
  DEFAULT_OPEN_FLAGS = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or
                       SQLITE_OPEN_FULLMUTEX;
  DEFAULT_BUSY_TIMEOUT = 30000;

var
  // Global instance for callbacks (thread-local would be better, but complex)
  CurrentDatabase: TNDXSQLiteDatabase;

{ Callback wrappers }

procedure TraceCallback(pArg: Pointer; zSQL: PAnsiChar); cdecl;
var
  DB: TNDXSQLiteDatabase;
begin
  DB := TNDXSQLiteDatabase(pArg);
  if Assigned(DB) and Assigned(DB.FOnTrace) then
    DB.FOnTrace(string(zSQL));
end;

procedure ProfileCallback(pArg: Pointer; zSQL: PAnsiChar; time: UInt64); cdecl;
var
  DB: TNDXSQLiteDatabase;
begin
  DB := TNDXSQLiteDatabase(pArg);
  if Assigned(DB) and Assigned(DB.FOnProfile) then
    DB.FOnProfile(string(zSQL), time);
end;

procedure UpdateCallback(pArg: Pointer; op: Integer; zDb, zTable: PAnsiChar;
  rowid: Int64); cdecl;
var
  DB: TNDXSQLiteDatabase;
begin
  DB := TNDXSQLiteDatabase(pArg);
  if Assigned(DB) and Assigned(DB.FOnUpdate) then
    DB.FOnUpdate(op, string(zDb), string(zTable), rowid);
end;

function CommitCallback(pArg: Pointer): Integer; cdecl;
var
  DB: TNDXSQLiteDatabase;
begin
  Result := 0;
  DB := TNDXSQLiteDatabase(pArg);
  if Assigned(DB) and Assigned(DB.FOnCommit) then
  begin
    if DB.FOnCommit() then
      Result := 1;  // Return non-zero to rollback
  end;
end;

procedure RollbackCallback(pArg: Pointer); cdecl;
var
  DB: TNDXSQLiteDatabase;
begin
  DB := TNDXSQLiteDatabase(pArg);
  if Assigned(DB) and Assigned(DB.FOnRollback) then
    DB.FOnRollback;
end;

function BusyCallback(pArg: Pointer; count: Integer): Integer; cdecl;
var
  DB: TNDXSQLiteDatabase;
begin
  Result := 0;
  DB := TNDXSQLiteDatabase(pArg);
  if Assigned(DB) and Assigned(DB.FOnBusy) then
  begin
    if DB.FOnBusy(count) then
      Result := 1;  // Return non-zero to retry
  end;
end;

function ProgressCallback(pArg: Pointer): Integer; cdecl;
var
  DB: TNDXSQLiteDatabase;
begin
  Result := 0;
  DB := TNDXSQLiteDatabase(pArg);
  if Assigned(DB) and Assigned(DB.FOnProgress) then
  begin
    if DB.FOnProgress() then
      Result := 1;  // Return non-zero to interrupt
  end;
end;

{ TNDXSQLiteDatabase }

constructor TNDXSQLiteDatabase.Create;
begin
  inherited Create;
  FHandle := nil;
  FDatabasePath := '';
  FIsOpen := False;
  FInTransaction := False;
  FTransactionType := ttDeferred;
  FSavepointLevel := 0;
  FBusyTimeoutMs := DEFAULT_BUSY_TIMEOUT;
  FLock := TCriticalSection.Create;
  FOwnsLock := True;

  // Ensure library is loaded
  if not IsSQLite3LibraryLoaded then
  begin
    if not LoadSQLite3Library then
      raise ENDXSQLiteLibraryException.Create('Failed to load SQLite3 library');
  end;
end;

constructor TNDXSQLiteDatabase.Create(ALock: TCriticalSection);
begin
  Create;
  if ALock <> nil then
  begin
    FreeAndNil(FLock);
    FLock := ALock;
    FOwnsLock := False;
  end;
end;

destructor TNDXSQLiteDatabase.Destroy;
begin
  if FIsOpen then
  begin
    try
      Close;
    except
      // Ignore close errors in destructor
    end;
  end;

  if FOwnsLock then
    FreeAndNil(FLock);

  inherited Destroy;
end;

procedure TNDXSQLiteDatabase.CheckOpen;
begin
  if not FIsOpen then
    raise ENDXSQLiteException.Create('Database is not open');
end;

procedure TNDXSQLiteDatabase.CheckClosed;
begin
  if FIsOpen then
    raise ENDXSQLiteException.Create('Database is already open');
end;

procedure TNDXSQLiteDatabase.CheckResult(AResult: Integer; const AContext: string);
var
  Msg: string;
begin
  if AResult <> SQLITE_OK then
  begin
    if FHandle <> nil then
      Msg := string(sqlite3_errmsg(FHandle))
    else
      Msg := string(sqlite3_errstr(AResult));

    if AContext <> '' then
      Msg := AContext + ': ' + Msg;

    case AResult of
      SQLITE_BUSY, SQLITE_LOCKED:
        raise ENDXSQLiteBusyException.Create(Msg);
      SQLITE_CONSTRAINT:
        raise ENDXSQLiteConstraintException.Create(Msg, 'UNKNOWN');
      SQLITE_CORRUPT, SQLITE_NOTADB:
        raise ENDXSQLiteCorruptException.Create(Msg);
      SQLITE_READONLY:
        raise ENDXSQLiteReadOnlyException.Create(Msg);
      SQLITE_IOERR:
        raise ENDXSQLiteIOException.Create(Msg);
      SQLITE_FULL:
        raise ENDXSQLiteDiskFullException.Create(Msg);
      SQLITE_CANTOPEN:
        raise ENDXSQLiteCantOpenException.Create(Msg);
    else
      raise ENDXSQLiteException.Create(Msg, AResult);
    end;
  end;
end;

procedure TNDXSQLiteDatabase.SetBusyTimeout(AValue: Integer);
begin
  FBusyTimeoutMs := AValue;
  if FIsOpen and Assigned(sqlite3_busy_timeout) then
    sqlite3_busy_timeout(FHandle, AValue);
end;

procedure TNDXSQLiteDatabase.InternalSetCallbacks;
begin
  if not FIsOpen then
    Exit;

  // Set callbacks if assigned
  if Assigned(FOnTrace) and Assigned(sqlite3_trace) then
    sqlite3_trace(FHandle, @TraceCallback, Self);

  if Assigned(FOnProfile) and Assigned(sqlite3_profile) then
    sqlite3_profile(FHandle, @ProfileCallback, Self);

  if Assigned(FOnUpdate) and Assigned(sqlite3_update_hook) then
    sqlite3_update_hook(FHandle, @UpdateCallback, Self);

  if Assigned(FOnCommit) and Assigned(sqlite3_commit_hook) then
    sqlite3_commit_hook(FHandle, @CommitCallback, Self);

  if Assigned(FOnRollback) and Assigned(sqlite3_rollback_hook) then
    sqlite3_rollback_hook(FHandle, @RollbackCallback, Self);

  if Assigned(FOnBusy) and Assigned(sqlite3_busy_handler) then
    sqlite3_busy_handler(FHandle, @BusyCallback, Self);

  if Assigned(FOnProgress) and Assigned(sqlite3_progress_handler) then
    sqlite3_progress_handler(FHandle, 100, @ProgressCallback, Self);
end;

function TNDXSQLiteDatabase.GetLastErrorCode: Integer;
begin
  if FHandle <> nil then
    Result := sqlite3_errcode(FHandle)
  else
    Result := SQLITE_OK;
end;

function TNDXSQLiteDatabase.GetLastErrorMessage: string;
begin
  if FHandle <> nil then
    Result := string(sqlite3_errmsg(FHandle))
  else
    Result := '';
end;

function TNDXSQLiteDatabase.GetLastInsertRowId: Int64;
begin
  if FHandle <> nil then
    Result := sqlite3_last_insert_rowid(FHandle)
  else
    Result := 0;
end;

function TNDXSQLiteDatabase.GetChanges: Integer;
begin
  if FHandle <> nil then
    Result := sqlite3_changes(FHandle)
  else
    Result := 0;
end;

function TNDXSQLiteDatabase.GetTotalChanges: Integer;
begin
  if FHandle <> nil then
    Result := sqlite3_total_changes(FHandle)
  else
    Result := 0;
end;

function TNDXSQLiteDatabase.GetAutoCommit: Boolean;
begin
  if FHandle <> nil then
    Result := sqlite3_get_autocommit(FHandle) <> 0
  else
    Result := True;
end;

function TNDXSQLiteDatabase.GetReadOnly: Boolean;
begin
  if FHandle <> nil then
    Result := sqlite3_db_readonly(FHandle, 'main') = 1
  else
    Result := False;
end;

procedure TNDXSQLiteDatabase.Open(const APath: string; AFlags: Integer);
var
  Flags: Integer;
  Res: Integer;
begin
  CheckClosed;

  if AFlags < 0 then
    Flags := DEFAULT_OPEN_FLAGS
  else
    Flags := AFlags;

  Res := sqlite3_open_v2(PAnsiChar(AnsiString(APath)), FHandle, Flags, nil);
  if Res <> SQLITE_OK then
  begin
    if FHandle <> nil then
    begin
      sqlite3_close_v2(FHandle);
      FHandle := nil;
    end;
    CheckResult(Res, 'Open database');
  end;

  FDatabasePath := APath;
  FIsOpen := True;
  FInTransaction := False;
  FSavepointLevel := 0;

  // Set default timeout
  SetBusyTimeout(FBusyTimeoutMs);

  // Setup callbacks
  InternalSetCallbacks;
end;

procedure TNDXSQLiteDatabase.OpenReadOnly(const APath: string);
begin
  Open(APath, SQLITE_OPEN_READONLY or SQLITE_OPEN_FULLMUTEX);
end;

procedure TNDXSQLiteDatabase.OpenMemory(const AName: string);
var
  Path: string;
begin
  if AName = '' then
    Path := ':memory:'
  else
    Path := 'file:' + AName + '?mode=memory&cache=shared';

  Open(Path, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or
             SQLITE_OPEN_MEMORY or SQLITE_OPEN_URI or SQLITE_OPEN_FULLMUTEX);
end;

procedure TNDXSQLiteDatabase.Close;
var
  Res: Integer;
begin
  if not FIsOpen then
    Exit;

  // Rollback any pending transaction
  if FInTransaction then
  begin
    try
      Rollback;
    except
      // Ignore rollback errors on close
    end;
  end;

  Res := sqlite3_close_v2(FHandle);
  if Res <> SQLITE_OK then
  begin
    // Try to finalize any remaining statements
    // sqlite3_close_v2 should handle this, but just in case...
  end;

  FHandle := nil;
  FIsOpen := False;
  FDatabasePath := '';
  FInTransaction := False;
  FSavepointLevel := 0;
end;

function TNDXSQLiteDatabase.Execute(const ASQL: string): Integer;
var
  ErrMsg: PAnsiChar;
  Res: Integer;
begin
  CheckOpen;

  ErrMsg := nil;
  Res := sqlite3_exec(FHandle, PAnsiChar(AnsiString(ASQL)), nil, nil, @ErrMsg);

  if Res <> SQLITE_OK then
  begin
    try
      if ErrMsg <> nil then
        raise ENDXSQLiteException.Create(string(ErrMsg), Res)
      else
        CheckResult(Res, 'Execute');
    finally
      if ErrMsg <> nil then
        sqlite3_free(ErrMsg);
    end;
  end;

  Result := sqlite3_changes(FHandle);
end;

function TNDXSQLiteDatabase.ExecuteScalar(const ASQL: string): Variant;
var
  Stmt: Psqlite3_stmt;
  Res: Integer;
  ColType: Integer;
  BlobPtr: Pointer;
  BlobSize: Integer;
  BlobData: TBytes;
begin
  CheckOpen;
  Result := Null;
  Stmt := nil;

  try
    Res := sqlite3_prepare_v2(FHandle, PAnsiChar(AnsiString(ASQL)), -1, Stmt, nil);
    CheckResult(Res, 'Prepare');

    Res := sqlite3_step(Stmt);
    if Res = SQLITE_ROW then
    begin
      ColType := sqlite3_column_type(Stmt, 0);
      case ColType of
        SQLITE_INTEGER:
          Result := sqlite3_column_int64(Stmt, 0);
        SQLITE_FLOAT:
          Result := sqlite3_column_double(Stmt, 0);
        SQLITE_TEXT:
          Result := string(sqlite3_column_text(Stmt, 0));
        SQLITE_BLOB:
          begin
            BlobSize := sqlite3_column_bytes(Stmt, 0);
            BlobPtr := sqlite3_column_blob(Stmt, 0);
            if (BlobSize > 0) and (BlobPtr <> nil) then
            begin
              SetLength(BlobData, BlobSize);
              Move(BlobPtr^, BlobData[0], BlobSize);
              Result := BlobData;
            end;
          end;
        SQLITE_NULL:
          Result := Null;
      end;
    end
    else if Res <> SQLITE_DONE then
      CheckResult(Res, 'Step');
  finally
    if Stmt <> nil then
      sqlite3_finalize(Stmt);
  end;
end;

function TNDXSQLiteDatabase.ExecuteScalarDef(const ASQL: string;
  ADefault: Variant): Variant;
begin
  Result := ExecuteScalar(ASQL);
  if VarIsNull(Result) then
    Result := ADefault;
end;

procedure TNDXSQLiteDatabase.BeginTransaction(AType: TNDXTransactionType);
var
  SQL: string;
begin
  CheckOpen;

  if FInTransaction then
    raise ENDXSQLiteException.Create('Transaction already active');

  case AType of
    ttDeferred:  SQL := 'BEGIN DEFERRED';
    ttImmediate: SQL := 'BEGIN IMMEDIATE';
    ttExclusive: SQL := 'BEGIN EXCLUSIVE';
  end;

  Execute(SQL);
  FInTransaction := True;
  FTransactionType := AType;
end;

procedure TNDXSQLiteDatabase.Commit;
begin
  CheckOpen;

  if not FInTransaction then
    raise ENDXSQLiteException.Create('No transaction active');

  // Release all savepoints first
  while FSavepointLevel > 0 do
    ReleaseSavepoint('sp_' + IntToStr(FSavepointLevel));

  Execute('COMMIT');
  FInTransaction := False;
end;

procedure TNDXSQLiteDatabase.Rollback;
begin
  CheckOpen;

  if not FInTransaction then
  begin
    // Check if SQLite thinks we're in a transaction
    if not AutoCommit then
      Execute('ROLLBACK')
    else
      Exit;  // No transaction to rollback
  end
  else
  begin
    FSavepointLevel := 0;  // Clear savepoint count
    Execute('ROLLBACK');
    FInTransaction := False;
  end;
end;

function TNDXSQLiteDatabase.Savepoint(const AName: string): string;
var
  SpName: string;
begin
  CheckOpen;

  if AName = '' then
  begin
    Inc(FSavepointLevel);
    SpName := 'sp_' + IntToStr(FSavepointLevel);
  end
  else
    SpName := AName;

  // Validate savepoint name (prevent SQL injection)
  if not IsValidIdentifier(SpName) then
    raise ENDXSQLiteException.Create('Invalid savepoint name: ' + SpName);

  Execute('SAVEPOINT ' + SpName);
  Result := SpName;
end;

procedure TNDXSQLiteDatabase.ReleaseSavepoint(const AName: string);
begin
  CheckOpen;

  if not IsValidIdentifier(AName) then
    raise ENDXSQLiteException.Create('Invalid savepoint name: ' + AName);

  Execute('RELEASE SAVEPOINT ' + AName);

  if (FSavepointLevel > 0) and (AName = 'sp_' + IntToStr(FSavepointLevel)) then
    Dec(FSavepointLevel);
end;

procedure TNDXSQLiteDatabase.RollbackToSavepoint(const AName: string);
begin
  CheckOpen;

  if not IsValidIdentifier(AName) then
    raise ENDXSQLiteException.Create('Invalid savepoint name: ' + AName);

  Execute('ROLLBACK TO SAVEPOINT ' + AName);
end;

procedure TNDXSQLiteDatabase.SetPragma(const AName, AValue: string);
begin
  CheckOpen;

  // Validate pragma name (prevent SQL injection)
  if not IsValidIdentifier(AName) then
    raise ENDXSQLiteException.Create('Invalid pragma name: ' + AName);

  Execute('PRAGMA ' + AName + ' = ' + QuotedStr(AValue));
end;

function TNDXSQLiteDatabase.GetPragma(const AName: string): string;
var
  V: Variant;
begin
  CheckOpen;

  // Validate pragma name (prevent SQL injection)
  if not IsValidIdentifier(AName) then
    raise ENDXSQLiteException.Create('Invalid pragma name: ' + AName);

  V := ExecuteScalar('PRAGMA ' + AName);
  if VarIsNull(V) then
    Result := ''
  else
    Result := VarToStr(V);
end;

procedure TNDXSQLiteDatabase.SetPragmaInt(const AName: string; AValue: Integer);
begin
  CheckOpen;

  if not IsValidIdentifier(AName) then
    raise ENDXSQLiteException.Create('Invalid pragma name: ' + AName);

  Execute('PRAGMA ' + AName + ' = ' + IntToStr(AValue));
end;

function TNDXSQLiteDatabase.GetPragmaInt(const AName: string): Integer;
var
  V: Variant;
begin
  CheckOpen;

  if not IsValidIdentifier(AName) then
    raise ENDXSQLiteException.Create('Invalid pragma name: ' + AName);

  V := ExecuteScalar('PRAGMA ' + AName);
  if VarIsNull(V) then
    Result := 0
  else
    Result := V;
end;

procedure TNDXSQLiteDatabase.BackupTo(const ADestPath: string;
  AProgressCallback: TNDXBackupProgressCallback);
var
  DestDB: TNDXSQLiteDatabase;
begin
  DestDB := TNDXSQLiteDatabase.Create;
  try
    DestDB.Open(ADestPath);
    BackupToDatabase(DestDB, AProgressCallback);
  finally
    DestDB.Free;
  end;
end;

procedure TNDXSQLiteDatabase.BackupToDatabase(ADest: TNDXSQLiteDatabase;
  AProgressCallback: TNDXBackupProgressCallback);
var
  Backup: Psqlite3_backup;
  Res: Integer;
  Remaining, Total: Integer;
  Cancel: Boolean;
begin
  CheckOpen;
  ADest.CheckOpen;

  Backup := sqlite3_backup_init(ADest.Handle, 'main', FHandle, 'main');
  if Backup = nil then
    raise ENDXSQLiteException.Create('Failed to initialize backup: ' +
      ADest.LastErrorMessage);

  try
    Cancel := False;
    repeat
      Res := sqlite3_backup_step(Backup, 100);  // Copy 100 pages at a time

      if Assigned(AProgressCallback) then
      begin
        Remaining := sqlite3_backup_remaining(Backup);
        Total := sqlite3_backup_pagecount(Backup);
        AProgressCallback(Remaining, Total, Cancel);
        if Cancel then
          Break;
      end;

      // SQLITE_OK means more pages to copy, SQLITE_DONE means finished
      // SQLITE_BUSY/SQLITE_LOCKED are retryable - keep looping
    until (Res <> SQLITE_OK) and (Res <> SQLITE_BUSY) and (Res <> SQLITE_LOCKED);

    if Cancel then
      raise ENDXSQLiteException.Create('Backup cancelled by user');

    if Res <> SQLITE_DONE then
      CheckResult(Res, 'Backup step');
  finally
    sqlite3_backup_finish(Backup);
  end;
end;

procedure TNDXSQLiteDatabase.WalCheckpoint(AMode: Integer);
var
  LogFrames, CheckpointedFrames: Integer;
  Res: Integer;
begin
  CheckOpen;

  Res := sqlite3_wal_checkpoint_v2(FHandle, nil, AMode,
    LogFrames, CheckpointedFrames);

  // SQLITE_BUSY is acceptable for PASSIVE mode
  if (Res <> SQLITE_OK) and not ((AMode = SQLITE_CHECKPOINT_PASSIVE) and
    (Res = SQLITE_BUSY)) then
    CheckResult(Res, 'WAL checkpoint');
end;

procedure TNDXSQLiteDatabase.EnableWalMode;
begin
  Execute('PRAGMA journal_mode = WAL');
end;

procedure TNDXSQLiteDatabase.Interrupt;
begin
  if FHandle <> nil then
    sqlite3_interrupt(FHandle);
end;

function TNDXSQLiteDatabase.TableExists(const ATableName: string): Boolean;
var
  V: Variant;
begin
  V := ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE type = ''table'' AND name = ' +
    QuotedStr(ATableName));
  Result := (not VarIsNull(V)) and (Integer(V) > 0);
end;

function TNDXSQLiteDatabase.IndexExists(const AIndexName: string): Boolean;
var
  V: Variant;
begin
  V := ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE type = ''index'' AND name = ' +
    QuotedStr(AIndexName));
  Result := (not VarIsNull(V)) and (Integer(V) > 0);
end;

function TNDXSQLiteDatabase.ColumnExists(const ATableName,
  AColumnName: string): Boolean;
var
  Stmt: Psqlite3_stmt;
  Res: Integer;
  I: Integer;
  ColName: string;
begin
  Result := False;
  CheckOpen;

  if not IsValidIdentifier(ATableName) then
    raise ENDXSQLiteException.Create('Invalid table name: ' + ATableName);

  Stmt := nil;
  try
    Res := sqlite3_prepare_v2(FHandle,
      PAnsiChar(AnsiString('PRAGMA table_info(' + ATableName + ')')),
      -1, Stmt, nil);
    CheckResult(Res, 'Prepare');

    while sqlite3_step(Stmt) = SQLITE_ROW do
    begin
      ColName := string(sqlite3_column_text(Stmt, 1));  // Column 1 is 'name'
      if SameText(ColName, AColumnName) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    if Stmt <> nil then
      sqlite3_finalize(Stmt);
  end;
end;

function TNDXSQLiteDatabase.GetTableList: TStringList;
var
  Stmt: Psqlite3_stmt;
  Res: Integer;
begin
  Result := TStringList.Create;
  CheckOpen;

  Stmt := nil;
  try
    Res := sqlite3_prepare_v2(FHandle,
      'SELECT name FROM sqlite_master WHERE type = ''table'' ' +
      'AND name NOT LIKE ''sqlite_%'' ORDER BY name',
      -1, Stmt, nil);
    CheckResult(Res, 'Prepare');

    while sqlite3_step(Stmt) = SQLITE_ROW do
      Result.Add(string(sqlite3_column_text(Stmt, 0)));
  finally
    if Stmt <> nil then
      sqlite3_finalize(Stmt);
  end;
end;

function TNDXSQLiteDatabase.GetColumnList(const ATableName: string): TStringList;
var
  Stmt: Psqlite3_stmt;
  Res: Integer;
begin
  Result := TStringList.Create;
  CheckOpen;

  if not IsValidIdentifier(ATableName) then
    raise ENDXSQLiteException.Create('Invalid table name: ' + ATableName);

  Stmt := nil;
  try
    Res := sqlite3_prepare_v2(FHandle,
      PAnsiChar(AnsiString('PRAGMA table_info(' + ATableName + ')')),
      -1, Stmt, nil);
    CheckResult(Res, 'Prepare');

    while sqlite3_step(Stmt) = SQLITE_ROW do
      Result.Add(string(sqlite3_column_text(Stmt, 1)));  // Column 1 is 'name'
  finally
    if Stmt <> nil then
      sqlite3_finalize(Stmt);
  end;
end;

function TNDXSQLiteDatabase.GetSchemaVersion: Integer;
begin
  Result := GetPragmaInt('schema_version');
end;

procedure TNDXSQLiteDatabase.SetSchemaVersion(AVersion: Integer);
begin
  SetPragmaInt('schema_version', AVersion);
end;

function TNDXSQLiteDatabase.GetUserVersion: Integer;
begin
  Result := GetPragmaInt('user_version');
end;

procedure TNDXSQLiteDatabase.SetUserVersion(AVersion: Integer);
begin
  SetPragmaInt('user_version', AVersion);
end;

function TNDXSQLiteDatabase.GetPageCount: Integer;
begin
  Result := GetPragmaInt('page_count');
end;

function TNDXSQLiteDatabase.GetPageSize: Integer;
begin
  Result := GetPragmaInt('page_size');
end;

function TNDXSQLiteDatabase.GetFreePageCount: Integer;
begin
  Result := GetPragmaInt('freelist_count');
end;

function TNDXSQLiteDatabase.GetDatabaseSize: Int64;
begin
  Result := Int64(GetPageCount) * Int64(GetPageSize);
end;

procedure TNDXSQLiteDatabase.ReleaseMemory;
begin
  if FHandle <> nil then
    sqlite3_db_release_memory(FHandle);
end;

procedure TNDXSQLiteDatabase.Lock;
begin
  if FLock <> nil then
    FLock.Enter;
end;

procedure TNDXSQLiteDatabase.Unlock;
begin
  if FLock <> nil then
    FLock.Leave;
end;

function TNDXSQLiteDatabase.TryLock: Boolean;
begin
  if FLock <> nil then
    Result := FLock.TryEnter
  else
    Result := True;
end;

end.
