{===============================================================================
  NDXSQLite - Main Connection Class
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 4 (depends on ndxsqlitetypes, ndxsqliteexceptions,
             ndxsqliteconnectionintf, ndxsqliteconnectionoptions)

  NOTE: This class uses native SQLite API via TNDXSQLiteDatabase.
        No SQLDB dependency.
===============================================================================}
unit ndxsqliteconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SyncObjs, fptimer, DateUtils, Variants,
  ndxsqlite3api, ndxsqlitedatabase, ndxsqlitestatement, ndxsqlitedataset,
  ndxsqlitetypes, ndxsqliteexceptions, ndxsqliteconnectionintf,
  ndxsqliteconnectionoptions, ndxsqliteplatform;

type
  { TNDXSQLiteConnection }
  TNDXSQLiteConnection = class(TInterfacedObject, INDXSQLiteConnection)
  private
    class var FConnectionCounter: Integer;
  private
    FId: Integer;
    FCreatedAt: TDateTime;
    FOptions: TNDXSQLiteConnectionOptions;
    FDatabase: TNDXSQLiteDatabase;
    FState: TNDXSQLiteConnectionState;
    FIsTransactionActive: Boolean;
    FLastAction: string;
    FActionHistory: TStringList;
    FAutoCloseTimer: TFPTimer;
    FLock: TCriticalSection;
    FDisposed: Boolean;

    procedure OnAutoCloseTimer(Sender: TObject);
    procedure LogAction(const AAction, ADescription: string);
    procedure ApplyPragmas;
    procedure ThrowIfDisposed;
    procedure EnsureConnectionOpen;
    function GetExecutionTimeMs(AStartTime: TDateTime): Int64;

  protected
    function GetId: Integer;
    function GetCreatedAt: TDateTime;
    function GetState: TNDXSQLiteConnectionState;
    function GetIsTransactionActive: Boolean;
    function GetIsPrimaryConnection: Boolean;
    function GetDatabasePath: string;
    function GetLastAction: string;
    function GetActionHistory: TStrings;
    function GetIsOpen: Boolean;
    function GetConnectionHandle: Pointer;

  public
    { Creates a connection from a full options object. }
    constructor Create(AOptions: TNDXSQLiteConnectionOptions); overload;
    { Creates a connection with a database path and optional primary flag. }
    constructor Create(const ADatabasePath: string; AIsPrimary: Boolean = False); overload;
    destructor Destroy; override;

    // Connection

    { Opens the database, applies configured PRAGMAs, and starts the auto-close timer. }
    procedure Open;
    { Closes the database, stops the auto-close timer, and releases internal resources. }
    procedure Close;

    // Transactions

    { Begins a transaction with the specified locking mode. Thread-safe. }
    function BeginTransaction(AMode: TNDXSQLiteTransactionMode = tmDeferred): Boolean;
    { Commits the active transaction. Raises an exception if no transaction is active. }
    procedure Commit;
    { Rolls back the active transaction. Raises an exception if no transaction is active. }
    procedure Rollback;

    // Savepoints (nested transactions)

    { Creates a named savepoint for nested transaction support. }
    procedure Savepoint(const AName: string);
    { Releases a savepoint, committing its changes to the parent transaction. }
    procedure ReleaseSavepoint(const AName: string);
    { Rolls back to a savepoint, discarding all changes made after it. }
    procedure RollbackToSavepoint(const AName: string);

    // Queries

    { Executes a non-query SQL statement. Returns the number of affected rows. }
    function ExecuteNonQuery(const ASQL: string): Integer; overload;
    { Executes a parameterized non-query statement. Returns the number of affected rows. }
    function ExecuteNonQuery(const ASQL: string; const AParams: array of Variant): Integer; overload;
    { Executes a query and returns the first column of the first row as Variant. }
    function ExecuteScalar(const ASQL: string): Variant; overload;
    { Executes a parameterized query and returns the first column of the first row. }
    function ExecuteScalar(const ASQL: string; const AParams: array of Variant): Variant; overload;
    { Executes a query and returns a TDataSet with all result rows. Caller must free. }
    function ExecuteQuery(const ASQL: string): TDataSet; overload;
    { Executes a parameterized query and returns a TDataSet. Caller must free. }
    function ExecuteQuery(const ASQL: string; const AParams: array of Variant): TDataSet; overload;

    // INT64 queries (native API to avoid truncation)

    { Executes a scalar query returning an Int64 value directly (avoids Variant truncation). }
    function ExecuteScalarInt64(const ASQL: string): Int64;
    { Executes a non-query with a named Int64 parameter (avoids Variant truncation). }
    function ExecuteNonQueryInt64(const ASQL: string; const AParamName: string; AValue: Int64): Integer;

    // Utilities

    { Resets the auto-close timer, preventing inactivity-based disconnection. }
    procedure ResetAutoCloseTimer;
    { Creates a new TNDXSQLiteDataSet query instance bound to this connection. }
    function CreateQuery(const ASQL: string = ''): TDataSet;
    { Returns the rowid generated by the most recent successful INSERT. }
    function GetLastInsertRowId: Int64;
    { Returns the number of rows changed by the most recent statement. }
    function GetChangesCount: Integer;
    { Returns the total number of rows changed since the connection was opened. }
    function GetTotalChangesCount: Integer;

    // PRAGMAs

    { Sets the journal mode (DELETE, TRUNCATE, PERSIST, MEMORY, WAL, OFF). }
    procedure SetJournalMode(AMode: TNDXSQLiteJournalMode);
    { Returns the current journal mode. }
    function GetJournalMode: TNDXSQLiteJournalMode;
    { Sets the synchronous mode for disk write operations. }
    procedure SetSyncMode(AMode: TNDXSQLiteSyncMode);
    { Returns the current synchronous mode. }
    function GetSyncMode: TNDXSQLiteSyncMode;
    { Sets the page cache size in kilobytes. }
    procedure SetCacheSize(ASizeKB: Integer);
    { Returns the current page cache size. }
    function GetCacheSize: Integer;
    { Sets the busy timeout for locked database retries (milliseconds). }
    procedure SetBusyTimeout(ATimeoutMs: Integer);
    { Returns the current busy timeout in milliseconds. }
    function GetBusyTimeout: Integer;
    { Enables or disables foreign key constraint enforcement. }
    procedure EnableForeignKeys(AEnabled: Boolean);
    { Returns True if foreign key constraints are currently enforced. }
    function IsForeignKeysEnabled: Boolean;
    { Reads an arbitrary PRAGMA value by name. }
    function GetPragmaValue(const APragmaName: string): Variant;
    { Sets an arbitrary PRAGMA value by name. }
    procedure SetPragmaValue(const APragmaName: string; const AValue: Variant);

    // Encryption (SQLCipher)

    { Changes the encryption key of the database. Requires SQLCipher library.
      If NewKey is empty, encryption is removed from the database. }
    procedure ChangeEncryptionKey(const ANewKey: string);
    { Returns True if the database appears to be encrypted (SQLCipher).
      Note: This performs a test query that may fail if wrong key is used. }
    function IsEncrypted: Boolean;
    { Returns True if SQLCipher library is loaded (supports encryption). }
    class function IsSQLCipherAvailable: Boolean;

    // Backup

    { Creates a full online backup of the database to the specified file path. }
    procedure BackupTo(const ADestPath: string);
    { Restores the database from a backup file, replacing all current content. }
    procedure RestoreFrom(const ASourcePath: string);

    // Properties
    property Id: Integer read FId;
    property CreatedAt: TDateTime read FCreatedAt;
    property State: TNDXSQLiteConnectionState read FState;
    property IsTransactionActive: Boolean read FIsTransactionActive;
    property IsPrimaryConnection: Boolean read GetIsPrimaryConnection;
    property DatabasePath: string read GetDatabasePath;
    property LastAction: string read FLastAction;
    property ActionHistory: TStrings read GetActionHistory;
    property IsOpen: Boolean read GetIsOpen;
    property Options: TNDXSQLiteConnectionOptions read FOptions;
    property Database: TNDXSQLiteDatabase read FDatabase;
  end;

implementation

uses
  StrUtils;

{ Helper function to copy files (not available in standard SysUtils) }
function CopyFile(const ASource, ADest: string): Boolean;
var
  SrcStream, DstStream: TFileStream;
begin
  Result := False;
  try
    SrcStream := TFileStream.Create(ASource, fmOpenRead or fmShareDenyWrite);
    try
      DstStream := TFileStream.Create(ADest, fmCreate);
      try
        DstStream.CopyFrom(SrcStream, SrcStream.Size);
        Result := True;
      finally
        DstStream.Free;
      end;
    finally
      SrcStream.Free;
    end;
  except
    Result := False;
  end;
end;

{ TNDXSQLiteConnection }

constructor TNDXSQLiteConnection.Create(AOptions: TNDXSQLiteConnectionOptions);
begin
  inherited Create;

  if AOptions = nil then
    raise ENDXSQLiteException.Create('Options cannot be nil', 0, 'Create');

  FOptions := AOptions.Clone;
  FId := InterlockedIncrement(FConnectionCounter);
  FCreatedAt := Now;
  FState := scsDisconnected;
  FIsTransactionActive := False;
  FDisposed := False;

  FLock := TCriticalSection.Create;
  FActionHistory := TStringList.Create;

  // Create native database wrapper
  FDatabase := TNDXSQLiteDatabase.Create(FLock);

  // Auto-close timer
  if not FOptions.IsPrimaryConnection and not FOptions.DisableAutoClose then
  begin
    FAutoCloseTimer := TFPTimer.Create(nil);
    FAutoCloseTimer.Enabled := False;
    FAutoCloseTimer.Interval := FOptions.AutoCloseTimeoutMs;
    FAutoCloseTimer.OnTimer := @OnAutoCloseTimer;
  end;

  LogAction('Create', Format('Connection #%d created (%s)',
    [FId, IfThen(FOptions.IsPrimaryConnection, 'primary', 'secondary')]));
end;

constructor TNDXSQLiteConnection.Create(const ADatabasePath: string; AIsPrimary: Boolean);
var
  Opts: TNDXSQLiteConnectionOptions;
begin
  Opts := TNDXSQLiteConnectionOptions.Create;
  try
    Opts.DatabasePath := ADatabasePath;
    Opts.IsPrimaryConnection := AIsPrimary;
    Create(Opts);
  finally
    Opts.Free;
  end;
end;

destructor TNDXSQLiteConnection.Destroy;
begin
  if FLock <> nil then
    FLock.Enter;
  try
    if not FDisposed then
    begin
      FDisposed := True;

      if Assigned(FAutoCloseTimer) then
      begin
        FAutoCloseTimer.Enabled := False;
        FreeAndNil(FAutoCloseTimer);
      end;

      if FIsTransactionActive and Assigned(FDatabase) then
      begin
        try
          FDatabase.Rollback;
        except
          // Ignore
        end;
        FIsTransactionActive := False;
      end;

      try
        FreeAndNil(FDatabase);
      except
        // Ignore - may happen on corrupted databases
        FDatabase := nil;
      end;

      FreeAndNil(FActionHistory);
      FreeAndNil(FOptions);
    end;
  finally
    if FLock <> nil then
      FLock.Leave;
  end;

  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TNDXSQLiteConnection.Open;
var
  DBPath: string;
  Flags: Integer;
begin
  ThrowIfDisposed;

  FLock.Enter;
  try
    if FState = scsConnected then
    begin
      ResetAutoCloseTimer;
      Exit;
    end;

    FState := scsConnecting;

    try
      // Configure path
      if FOptions.MemoryDatabase then
        DBPath := ':memory:'
      else
      begin
        // Normalize and validate path according to platform
        DBPath := TNDXPlatform.ValidateDatabasePath(FOptions.DatabasePath);
        // Ensure directory exists
        TNDXPlatform.EnsureDirectoryExists(ExtractFilePath(DBPath));
      end;

      // Build open flags
      if FOptions.ReadOnly then
        Flags := SQLITE_OPEN_READONLY or SQLITE_OPEN_FULLMUTEX
      else
        Flags := SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or SQLITE_OPEN_FULLMUTEX;

      // Open connection
      FDatabase.Open(DBPath, Flags);

      // Set busy timeout
      FDatabase.BusyTimeoutMs := FOptions.BusyTimeout;

      // Apply PRAGMAs
      ApplyPragmas;

      FState := scsConnected;
      LogAction('Open', Format('Connected to: %s', [DBPath]));

      ResetAutoCloseTimer;

    except
      on E: Exception do
      begin
        FState := scsError;
        raise ENDXSQLiteConnectionException.Create(
          Format('Unable to open database: %s', [E.Message]),
          FOptions.DatabasePath);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.Close;
begin
  FLock.Enter;
  try
    if FState = scsDisconnected then
      Exit;

    if Assigned(FAutoCloseTimer) then
      FAutoCloseTimer.Enabled := False;

    if FIsTransactionActive then
    begin
      try
        FDatabase.Rollback;
        FIsTransactionActive := False;
      except
        // Ignore
      end;
    end;

    if FDatabase.IsOpen then
    begin
      try
        FDatabase.Close;
      except
        // Ignore errors - may happen on corrupted databases
      end;
    end;

    FState := scsDisconnected;
    LogAction('Close', 'Connection closed');

  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.ApplyPragmas;
begin
  if not FDatabase.IsOpen then
    Exit;

  // IMPORTANT: Encryption key MUST be set FIRST before any other operation
  // This is required by SQLCipher - PRAGMA key must be the first command
  if FOptions.EncryptionKey <> '' then
  begin
    // Use PRAGMA key for SQLCipher encryption
    // The key should be quoted as a string literal
    FDatabase.Execute('PRAGMA key = ' + QuotedStr(FOptions.EncryptionKey));
    LogAction('ApplyPragmas', 'Encryption key applied (SQLCipher)');
  end;

  // Foreign keys
  if FOptions.ForeignKeys then
    FDatabase.SetPragmaInt('foreign_keys', 1)
  else
    FDatabase.SetPragmaInt('foreign_keys', 0);

  // Journal mode
  FDatabase.SetPragma('journal_mode', TNDXSQLiteHelper.JournalModeToString(FOptions.JournalMode));

  // Synchronous mode
  FDatabase.SetPragma('synchronous', TNDXSQLiteHelper.SyncModeToString(FOptions.SyncMode));

  // Temp store
  FDatabase.SetPragmaInt('temp_store', FOptions.TempStore);

  // Cache size (negative = KB)
  FDatabase.SetPragmaInt('cache_size', -FOptions.CacheSize);

  LogAction('ApplyPragmas', 'PRAGMA configuration applied');
end;

function TNDXSQLiteConnection.BeginTransaction(AMode: TNDXSQLiteTransactionMode): Boolean;
var
  TransType: TNDXTransactionType;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  FLock.Enter;
  try
    if FIsTransactionActive then
      raise ENDXSQLiteTransactionException.Create(
        'A transaction is already active',
        TNDXSQLiteHelper.TransactionModeToString(AMode));

    case AMode of
      tmImmediate: TransType := ttImmediate;
      tmExclusive: TransType := ttExclusive;
    else
      TransType := ttDeferred;
    end;

    FDatabase.BeginTransaction(TransType);
    FIsTransactionActive := True;

    // Disable timer during transaction
    if Assigned(FAutoCloseTimer) then
      FAutoCloseTimer.Enabled := False;

    LogAction('BeginTransaction', Format('Transaction started (mode: %s)',
      [TNDXSQLiteHelper.TransactionModeToString(AMode)]));
    Result := True;

  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.Commit;
begin
  FLock.Enter;
  try
    if not FIsTransactionActive then
      Exit;

    FDatabase.Commit;
    FIsTransactionActive := False;

    LogAction('Commit', 'Transaction committed');

    // Reset auto-close timer
    if Assigned(FAutoCloseTimer) then
    begin
      FAutoCloseTimer.Enabled := False;
      FAutoCloseTimer.Interval := FOptions.AutoCloseTimeoutMs;
      FAutoCloseTimer.Enabled := True;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.Rollback;
begin
  FLock.Enter;
  try
    if not FIsTransactionActive then
      Exit;

    try
      FDatabase.Rollback;
    except
      on E: Exception do
        LogAction('Rollback', 'Rollback error: ' + E.Message);
    end;

    FIsTransactionActive := False;
    LogAction('Rollback', 'Transaction rolled back');

    // Reset auto-close timer
    if Assigned(FAutoCloseTimer) then
    begin
      FAutoCloseTimer.Enabled := False;
      FAutoCloseTimer.Interval := FOptions.AutoCloseTimeoutMs;
      FAutoCloseTimer.Enabled := True;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.Savepoint(const AName: string);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  FLock.Enter;
  try
    FDatabase.Savepoint(AName);
    LogAction('Savepoint', Format('Savepoint created: %s', [AName]));
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.ReleaseSavepoint(const AName: string);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  FLock.Enter;
  try
    FDatabase.ReleaseSavepoint(AName);
    LogAction('ReleaseSavepoint', Format('Savepoint released: %s', [AName]));
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.RollbackToSavepoint(const AName: string);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  FLock.Enter;
  try
    FDatabase.RollbackToSavepoint(AName);
    LogAction('RollbackToSavepoint', Format('Rolled back to savepoint: %s', [AName]));
  finally
    FLock.Leave;
  end;
end;

function TNDXSQLiteConnection.ExecuteNonQuery(const ASQL: string): Integer;
begin
  Result := ExecuteNonQuery(ASQL, []);
end;

function TNDXSQLiteConnection.ExecuteNonQuery(const ASQL: string;
  const AParams: array of Variant): Integer;
var
  Stmt: TNDXSQLiteStatement;
  StartTime: TDateTime;
  I: Integer;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  StartTime := Now;

  FLock.Enter;
  try
    FState := scsExecuting;
  finally
    FLock.Leave;
  end;

  Result := 0;

  if Length(AParams) = 0 then
  begin
    // Simple execution without parameters
    Result := FDatabase.Execute(ASQL);
  end
  else
  begin
    // Parameterized execution
    Stmt := TNDXSQLiteStatement.Create(FDatabase);
    try
      Stmt.Prepare(ASQL);

      for I := Low(AParams) to High(AParams) do
        Stmt.BindVariant(I + 1, AParams[I]);

      Result := Stmt.Execute;
    finally
      Stmt.Free;
    end;
  end;

  LogAction('ExecuteNonQuery', Format('%s (%d rows, %d ms)',
    [Copy(ASQL, 1, 50), Result, GetExecutionTimeMs(StartTime)]));

  FLock.Enter;
  try
    FState := scsConnected;
  finally
    FLock.Leave;
  end;

  ResetAutoCloseTimer;
end;

function TNDXSQLiteConnection.ExecuteScalar(const ASQL: string): Variant;
begin
  Result := ExecuteScalar(ASQL, []);
end;

function TNDXSQLiteConnection.ExecuteScalar(const ASQL: string;
  const AParams: array of Variant): Variant;
var
  Stmt: TNDXSQLiteStatement;
  StartTime: TDateTime;
  I: Integer;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  StartTime := Now;

  FLock.Enter;
  try
    FState := scsExecuting;
  finally
    FLock.Leave;
  end;

  Result := Null;

  if Length(AParams) = 0 then
  begin
    // Simple execution without parameters
    Result := FDatabase.ExecuteScalar(ASQL);
  end
  else
  begin
    // Parameterized execution
    Stmt := TNDXSQLiteStatement.Create(FDatabase);
    try
      Stmt.Prepare(ASQL);

      for I := Low(AParams) to High(AParams) do
        Stmt.BindVariant(I + 1, AParams[I]);

      Result := Stmt.ExecuteScalar;
    finally
      Stmt.Free;
    end;
  end;

  LogAction('ExecuteScalar', Format('%s (%d ms)',
    [Copy(ASQL, 1, 50), GetExecutionTimeMs(StartTime)]));

  FLock.Enter;
  try
    FState := scsConnected;
  finally
    FLock.Leave;
  end;

  ResetAutoCloseTimer;
end;

function TNDXSQLiteConnection.ExecuteQuery(const ASQL: string): TDataSet;
begin
  Result := ExecuteQuery(ASQL, []);
end;

function TNDXSQLiteConnection.ExecuteQuery(const ASQL: string;
  const AParams: array of Variant): TDataSet;
var
  DS: TNDXSQLiteDataSet;
  I: Integer;
  StartTime: TDateTime;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  StartTime := Now;

  FLock.Enter;
  try
    FState := scsExecuting;
  finally
    FLock.Leave;
  end;

  DS := TNDXSQLiteDataSet.Create(nil);
  try
    DS.Database := FDatabase;
    DS.SQL.Text := ASQL;

    // Bind parameters
    for I := Low(AParams) to High(AParams) do
    begin
      if I < DS.Params.Count then
        DS.Params[I].Value := AParams[I];
    end;

    DS.Open;

    LogAction('ExecuteQuery', Format('%s (%d rows, %d ms)',
      [Copy(ASQL, 1, 50), DS.RecordCount, GetExecutionTimeMs(StartTime)]));

    FLock.Enter;
    try
      FState := scsConnected;
    finally
      FLock.Leave;
    end;

    ResetAutoCloseTimer;

    Result := DS;
    DS := nil;  // Prevent freeing in finally block
  finally
    DS.Free;  // Only frees if exception occurred (DS not nil)
  end;
end;

function TNDXSQLiteConnection.ExecuteScalarInt64(const ASQL: string): Int64;
var
  Stmt: TNDXSQLiteStatement;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  Result := 0;

  Stmt := TNDXSQLiteStatement.Create(FDatabase);
  try
    Stmt.Prepare(ASQL);
    if Stmt.Step then
      Result := Stmt.ColumnAsInteger(0);
  finally
    Stmt.Free;
  end;

  LogAction('ExecuteScalarInt64', Copy(ASQL, 1, 50));
  ResetAutoCloseTimer;
end;

function TNDXSQLiteConnection.ExecuteNonQueryInt64(const ASQL: string;
  const AParamName: string; AValue: Int64): Integer;
var
  Stmt: TNDXSQLiteStatement;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  Result := 0;

  Stmt := TNDXSQLiteStatement.Create(FDatabase);
  try
    Stmt.Prepare(ASQL);
    Stmt.BindIntegerByName(AParamName, AValue);
    Result := Stmt.Execute;
  finally
    Stmt.Free;
  end;

  LogAction('ExecuteNonQueryInt64', Copy(ASQL, 1, 50));
  ResetAutoCloseTimer;
end;

function TNDXSQLiteConnection.CreateQuery(const ASQL: string): TDataSet;
var
  DS: TNDXSQLiteDataSet;
begin
  ThrowIfDisposed;
  DS := TNDXSQLiteDataSet.Create(nil);
  DS.Database := FDatabase;
  if ASQL <> '' then
    DS.SQL.Text := ASQL;
  Result := DS;
end;

function TNDXSQLiteConnection.GetLastInsertRowId: Int64;
begin
  Result := FDatabase.LastInsertRowId;
end;

function TNDXSQLiteConnection.GetChangesCount: Integer;
begin
  Result := FDatabase.Changes;
end;

function TNDXSQLiteConnection.GetTotalChangesCount: Integer;
begin
  Result := FDatabase.TotalChanges;
end;

procedure TNDXSQLiteConnection.SetJournalMode(AMode: TNDXSQLiteJournalMode);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  FDatabase.SetPragma('journal_mode', TNDXSQLiteHelper.JournalModeToString(AMode));
end;

function TNDXSQLiteConnection.GetJournalMode: TNDXSQLiteJournalMode;
var
  S: string;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  S := FDatabase.GetPragma('journal_mode');
  Result := TNDXSQLiteHelper.StringToJournalMode(S);
end;

procedure TNDXSQLiteConnection.SetSyncMode(AMode: TNDXSQLiteSyncMode);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  FDatabase.SetPragma('synchronous', TNDXSQLiteHelper.SyncModeToString(AMode));
end;

function TNDXSQLiteConnection.GetSyncMode: TNDXSQLiteSyncMode;
var
  V: Integer;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  V := FDatabase.GetPragmaInt('synchronous');
  case V of
    0: Result := smOff;
    1: Result := smNormal;
    2: Result := smFull;
    3: Result := smExtra;
  else
    Result := smNormal;
  end;
end;

procedure TNDXSQLiteConnection.SetCacheSize(ASizeKB: Integer);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  FDatabase.SetPragmaInt('cache_size', -ASizeKB);
end;

function TNDXSQLiteConnection.GetCacheSize: Integer;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  Result := Abs(FDatabase.GetPragmaInt('cache_size'));
end;

procedure TNDXSQLiteConnection.SetBusyTimeout(ATimeoutMs: Integer);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  FDatabase.BusyTimeoutMs := ATimeoutMs;
end;

function TNDXSQLiteConnection.GetBusyTimeout: Integer;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  Result := FDatabase.BusyTimeoutMs;
end;

procedure TNDXSQLiteConnection.EnableForeignKeys(AEnabled: Boolean);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  if AEnabled then
    FDatabase.SetPragmaInt('foreign_keys', 1)
  else
    FDatabase.SetPragmaInt('foreign_keys', 0);
end;

function TNDXSQLiteConnection.IsForeignKeysEnabled: Boolean;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  Result := FDatabase.GetPragmaInt('foreign_keys') = 1;
end;

function TNDXSQLiteConnection.GetPragmaValue(const APragmaName: string): Variant;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  Result := FDatabase.ExecuteScalar('PRAGMA ' + APragmaName);
end;

procedure TNDXSQLiteConnection.SetPragmaValue(const APragmaName: string;
  const AValue: Variant);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;
  FDatabase.Execute(Format('PRAGMA %s = %s', [APragmaName, VarToStr(AValue)]));
end;

procedure TNDXSQLiteConnection.ChangeEncryptionKey(const ANewKey: string);
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  FLock.Enter;
  try
    if ANewKey = '' then
    begin
      // Remove encryption - rekey with empty key
      FDatabase.Execute('PRAGMA rekey = ''''');
      FOptions.EncryptionKey := '';
      LogAction('ChangeEncryptionKey', 'Encryption removed from database');
    end
    else
    begin
      // Change to new key
      FDatabase.Execute('PRAGMA rekey = ' + QuotedStr(ANewKey));
      FOptions.EncryptionKey := ANewKey;
      LogAction('ChangeEncryptionKey', 'Encryption key changed');
    end;
  finally
    FLock.Leave;
  end;
end;

function TNDXSQLiteConnection.IsEncrypted: Boolean;
var
  CipherVersion: Variant;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  Result := False;
  try
    // SQLCipher provides cipher_version pragma
    CipherVersion := FDatabase.ExecuteScalar('PRAGMA cipher_version');
    Result := not VarIsNull(CipherVersion) and (VarToStr(CipherVersion) <> '');
  except
    // If cipher_version fails, SQLCipher is not available
    Result := False;
  end;
end;

class function TNDXSQLiteConnection.IsSQLCipherAvailable: Boolean;
var
  TempDB: TNDXSQLiteDatabase;
  CipherVersion: Variant;
begin
  Result := False;
  TempDB := TNDXSQLiteDatabase.Create(nil);
  try
    try
      TempDB.Open(':memory:', SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE);
      CipherVersion := TempDB.ExecuteScalar('PRAGMA cipher_version');
      Result := not VarIsNull(CipherVersion) and (VarToStr(CipherVersion) <> '');
      TempDB.Close;
    except
      Result := False;
    end;
  finally
    TempDB.Free;
  end;
end;

procedure TNDXSQLiteConnection.BackupTo(const ADestPath: string);
var
  NormalizedPath: string;
begin
  ThrowIfDisposed;
  EnsureConnectionOpen;

  // Normalize path for platform
  NormalizedPath := TNDXPlatform.NormalizePath(ADestPath);
  TNDXPlatform.EnsureDirectoryExists(ExtractFilePath(NormalizedPath));

  FDatabase.BackupTo(NormalizedPath);
  LogAction('BackupTo', Format('Backup to: %s', [NormalizedPath]));
end;

procedure TNDXSQLiteConnection.RestoreFrom(const ASourcePath: string);
var
  NormalizedSource, DBPath: string;
  WalPath, ShmPath: string;
begin
  // Normalize paths
  NormalizedSource := TNDXPlatform.NormalizePath(ASourcePath);
  DBPath := TNDXPlatform.NormalizePath(FOptions.DatabasePath);

  if not FileExists(NormalizedSource) then
    raise ENDXSQLiteException.Create(Format('Source file not found: %s', [NormalizedSource]));

  // Build WAL/SHM paths
  WalPath := DBPath + '-wal';
  ShmPath := DBPath + '-shm';

  // Close connection
  Close;

  // Delete WAL/SHM files if present
  if FileExists(WalPath) then
  begin
    if not DeleteFile(WalPath) then
      raise ENDXSQLiteException.Create(Format('Unable to delete WAL file: %s', [WalPath]));
  end;
  if FileExists(ShmPath) then
  begin
    if not DeleteFile(ShmPath) then
      raise ENDXSQLiteException.Create(Format('Unable to delete SHM file: %s', [ShmPath]));
  end;

  // Replace file
  if FileExists(DBPath) then
  begin
    if not DeleteFile(DBPath) then
      raise ENDXSQLiteException.Create(Format('Unable to delete existing database: %s', [DBPath]));
  end;

  // Copy and verify success
  if not CopyFile(NormalizedSource, DBPath) then
    raise ENDXSQLiteException.Create(Format('Copy failed: %s -> %s', [NormalizedSource, DBPath]));

  // Verify file exists after copy
  if not FileExists(DBPath) then
    raise ENDXSQLiteException.Create('Destination file does not exist after copy');

  // Reopen
  Open;
  LogAction('RestoreFrom', Format('Restored from: %s', [NormalizedSource]));
end;

procedure TNDXSQLiteConnection.LogAction(const AAction, ADescription: string);
var
  Entry: string;
begin
  Entry := Format('[%s] [%s] %s',
    [FormatDateTime('hh:nn:ss.zzz', Now), AAction, ADescription]);

  FLock.Enter;
  try
    // Rolling history
    if FActionHistory.Count >= NDX_MAX_ACTION_HISTORY then
      FActionHistory.Delete(FActionHistory.Count - 1);

    if FLastAction <> '' then
      FActionHistory.Insert(0, FLastAction);

    FLastAction := Entry;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnection.ThrowIfDisposed;
begin
  if FDisposed then
    raise ENDXSQLiteDisposedException.Create('TNDXSQLiteConnection');
end;

procedure TNDXSQLiteConnection.EnsureConnectionOpen;
begin
  if not IsOpen then
    Open;
end;

procedure TNDXSQLiteConnection.ResetAutoCloseTimer;
begin
  if Assigned(FAutoCloseTimer) and not FIsTransactionActive then
  begin
    FAutoCloseTimer.Enabled := False;
    FAutoCloseTimer.Interval := FOptions.AutoCloseTimeoutMs;
    FAutoCloseTimer.Enabled := True;
  end;
end;

procedure TNDXSQLiteConnection.OnAutoCloseTimer(Sender: TObject);
begin
  if FIsTransactionActive or FOptions.DisableAutoClose or FOptions.IsPrimaryConnection then
    Exit;

  try
    if IsOpen then
    begin
      Close;
      LogAction('AutoClose', 'Auto-close (timeout)');
    end;
  except
    // Ignore errors
  end;
end;

function TNDXSQLiteConnection.GetExecutionTimeMs(AStartTime: TDateTime): Int64;
begin
  Result := MilliSecondsBetween(Now, AStartTime);
end;

{ Getters }

function TNDXSQLiteConnection.GetId: Integer;
begin
  Result := FId;
end;

function TNDXSQLiteConnection.GetCreatedAt: TDateTime;
begin
  Result := FCreatedAt;
end;

function TNDXSQLiteConnection.GetState: TNDXSQLiteConnectionState;
begin
  Result := FState;
end;

function TNDXSQLiteConnection.GetIsTransactionActive: Boolean;
begin
  Result := FIsTransactionActive;
end;

function TNDXSQLiteConnection.GetIsPrimaryConnection: Boolean;
begin
  Result := FOptions.IsPrimaryConnection;
end;

function TNDXSQLiteConnection.GetDatabasePath: string;
begin
  Result := FOptions.DatabasePath;
end;

function TNDXSQLiteConnection.GetLastAction: string;
begin
  Result := FLastAction;
end;

function TNDXSQLiteConnection.GetActionHistory: TStrings;
begin
  Result := FActionHistory;
end;

function TNDXSQLiteConnection.GetIsOpen: Boolean;
begin
  Result := (FDatabase <> nil) and FDatabase.IsOpen;
end;

function TNDXSQLiteConnection.GetConnectionHandle: Pointer;
begin
  if (FDatabase <> nil) and FDatabase.IsOpen then
    Result := FDatabase.Handle
  else
    Result := nil;
end;

end.
