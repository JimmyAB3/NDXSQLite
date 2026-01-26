{===============================================================================
  NDXSQLite - Configuration Options
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 2 (depends on ndxsqlitetypes)
===============================================================================}
unit ndxsqliteconnectionoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ndxsqlitetypes;

type
  { Configuration options for SQLite connection }
  TNDXSQLiteConnectionOptions = class(TPersistent)
  private
    FDatabasePath: string;
    FCreateIfNotExists: Boolean;
    FReadOnly: Boolean;
    FIsPrimaryConnection: Boolean;
    FAutoCloseTimeoutMs: Integer;
    FDisableAutoClose: Boolean;
    FJournalMode: TNDXSQLiteJournalMode;
    FSyncMode: TNDXSQLiteSyncMode;
    FCacheSize: Integer;
    FBusyTimeout: Integer;
    FForeignKeys: Boolean;
    FSharedCache: Boolean;
    FMemoryDatabase: Boolean;
    FEncryptionKey: string;
    FConnectionTimeoutMs: Integer;
    FPageSize: Integer;
    FLockingMode: TNDXSQLiteLockingMode;
    FTempStore: Integer;
    FMaxPageCount: Integer;
    FWALAutoCheckpoint: Integer;

  public
    { Creates a new options instance with default values (WAL, NORMAL sync, 4KB pages). }
    constructor Create;
    destructor Destroy; override;
    { Copies all option values from the source object. }
    procedure Assign(Source: TPersistent); override;
    { Creates an independent copy of this options instance. Caller must free. }
    function Clone: TNDXSQLiteConnectionOptions;
    { Builds a descriptive connection string summarizing the current options. }
    function BuildConnectionString: string;
    { Returns True if the options are valid (database path set, values in range). }
    function Validate: Boolean;
    { Validates the options and returns a detailed error message if invalid. }
    function ValidateWithMessage(out AMessage: string): Boolean;

  published
    { Path to database file }
    property DatabasePath: string read FDatabasePath write FDatabasePath;
    { Create database if it does not exist }
    property CreateIfNotExists: Boolean read FCreateIfNotExists write FCreateIfNotExists default True;
    { Open in read-only mode }
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    { Primary connection (no auto-close) }
    property IsPrimaryConnection: Boolean read FIsPrimaryConnection write FIsPrimaryConnection default False;
    { Timeout before auto-close (ms) }
    property AutoCloseTimeoutMs: Integer read FAutoCloseTimeoutMs write FAutoCloseTimeoutMs default 28000;
    { Disable auto-close }
    property DisableAutoClose: Boolean read FDisableAutoClose write FDisableAutoClose default False;
    { SQLite journal mode (DELETE by default for compatibility) }
    property JournalMode: TNDXSQLiteJournalMode read FJournalMode write FJournalMode default jmDelete;
    { Synchronization mode }
    property SyncMode: TNDXSQLiteSyncMode read FSyncMode write FSyncMode default smNormal;
    { Cache size in KB }
    property CacheSize: Integer read FCacheSize write FCacheSize default 2000;
    { Lock timeout (ms) }
    property BusyTimeout: Integer read FBusyTimeout write FBusyTimeout default 5000;
    { Enable foreign keys }
    property ForeignKeys: Boolean read FForeignKeys write FForeignKeys default True;
    { Shared cache mode }
    property SharedCache: Boolean read FSharedCache write FSharedCache default False;
    { In-memory database }
    property MemoryDatabase: Boolean read FMemoryDatabase write FMemoryDatabase default False;
    { Encryption key (SQLCipher) }
    property EncryptionKey: string read FEncryptionKey write FEncryptionKey;
    { Connection timeout (ms) }
    property ConnectionTimeoutMs: Integer read FConnectionTimeoutMs write FConnectionTimeoutMs default 30000;
    { Page size in bytes }
    property PageSize: Integer read FPageSize write FPageSize default 4096;
    { Locking mode }
    property LockingMode: TNDXSQLiteLockingMode read FLockingMode write FLockingMode default lmNormal;
    { Temporary storage (0=DEFAULT, 1=FILE, 2=MEMORY) }
    property TempStore: Integer read FTempStore write FTempStore default 2;
    { Maximum page count (0=unlimited) }
    property MaxPageCount: Integer read FMaxPageCount write FMaxPageCount default 0;
    { WAL auto-checkpoint (number of pages) }
    property WALAutoCheckpoint: Integer read FWALAutoCheckpoint write FWALAutoCheckpoint default 1000;
  end;

implementation

{ TNDXSQLiteConnectionOptions }

constructor TNDXSQLiteConnectionOptions.Create;
begin
  inherited Create;
  FDatabasePath := '';
  FCreateIfNotExists := True;
  FReadOnly := False;
  FIsPrimaryConnection := False;
  FAutoCloseTimeoutMs := NDX_DEFAULT_AUTO_CLOSE_TIMEOUT;
  FDisableAutoClose := False;
  FJournalMode := jmDelete;
  FSyncMode := smNormal;
  FCacheSize := NDX_DEFAULT_CACHE_SIZE;
  FBusyTimeout := NDX_DEFAULT_BUSY_TIMEOUT;
  FForeignKeys := True;
  FSharedCache := False;
  FMemoryDatabase := False;
  FEncryptionKey := '';
  FConnectionTimeoutMs := NDX_DEFAULT_CONNECTION_TIMEOUT;
  FPageSize := NDX_DEFAULT_PAGE_SIZE;
  FLockingMode := lmNormal;
  FTempStore := 2; // MEMORY
  FMaxPageCount := 0;
  FWALAutoCheckpoint := 1000;
end;

destructor TNDXSQLiteConnectionOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TNDXSQLiteConnectionOptions.Assign(Source: TPersistent);
var
  Src: TNDXSQLiteConnectionOptions;
begin
  if Source is TNDXSQLiteConnectionOptions then
  begin
    Src := TNDXSQLiteConnectionOptions(Source);
    FDatabasePath := Src.FDatabasePath;
    FCreateIfNotExists := Src.FCreateIfNotExists;
    FReadOnly := Src.FReadOnly;
    FIsPrimaryConnection := Src.FIsPrimaryConnection;
    FAutoCloseTimeoutMs := Src.FAutoCloseTimeoutMs;
    FDisableAutoClose := Src.FDisableAutoClose;
    FJournalMode := Src.FJournalMode;
    FSyncMode := Src.FSyncMode;
    FCacheSize := Src.FCacheSize;
    FBusyTimeout := Src.FBusyTimeout;
    FForeignKeys := Src.FForeignKeys;
    FSharedCache := Src.FSharedCache;
    FMemoryDatabase := Src.FMemoryDatabase;
    FEncryptionKey := Src.FEncryptionKey;
    FConnectionTimeoutMs := Src.FConnectionTimeoutMs;
    FPageSize := Src.FPageSize;
    FLockingMode := Src.FLockingMode;
    FTempStore := Src.FTempStore;
    FMaxPageCount := Src.FMaxPageCount;
    FWALAutoCheckpoint := Src.FWALAutoCheckpoint;
  end
  else
    inherited Assign(Source);
end;

function TNDXSQLiteConnectionOptions.Clone: TNDXSQLiteConnectionOptions;
begin
  Result := TNDXSQLiteConnectionOptions.Create;
  Result.Assign(Self);
end;

function TNDXSQLiteConnectionOptions.BuildConnectionString: string;
begin
  if FMemoryDatabase then
    Result := ':memory:'
  else
    Result := FDatabasePath;
end;

function TNDXSQLiteConnectionOptions.Validate: Boolean;
var
  Msg: string;
begin
  Result := ValidateWithMessage(Msg);
end;

function TNDXSQLiteConnectionOptions.ValidateWithMessage(out AMessage: string): Boolean;
begin
  Result := True;
  AMessage := '';

  // Database path validation
  if not FMemoryDatabase and (Trim(FDatabasePath) = '') then
  begin
    AMessage := 'Database path is required';
    Result := False;
    Exit;
  end;

  // Page size validation
  if FPageSize < 512 then
  begin
    AMessage := 'Page size must be at least 512 bytes';
    Result := False;
    Exit;
  end;

  // Page size must be a power of 2
  if (FPageSize and (FPageSize - 1)) <> 0 then
  begin
    AMessage := 'Page size must be a power of 2';
    Result := False;
    Exit;
  end;

  // Busy timeout validation
  if FBusyTimeout < 0 then
  begin
    AMessage := 'BusyTimeout cannot be negative';
    Result := False;
    Exit;
  end;

  // Cache size validation
  if FCacheSize < 0 then
  begin
    AMessage := 'Cache size cannot be negative';
    Result := False;
    Exit;
  end;

  // Connection timeout validation
  if FConnectionTimeoutMs < 0 then
  begin
    AMessage := 'ConnectionTimeoutMs cannot be negative';
    Result := False;
    Exit;
  end;

  // Auto-close timeout validation
  if FAutoCloseTimeoutMs < 0 then
  begin
    AMessage := 'AutoCloseTimeoutMs cannot be negative';
    Result := False;
    Exit;
  end;

  // TempStore validation
  if (FTempStore < 0) or (FTempStore > 2) then
  begin
    AMessage := 'TempStore must be 0 (DEFAULT), 1 (FILE) or 2 (MEMORY)';
    Result := False;
    Exit;
  end;

  // WALAutoCheckpoint validation
  if FWALAutoCheckpoint < 0 then
  begin
    AMessage := 'WALAutoCheckpoint cannot be negative';
    Result := False;
    Exit;
  end;
end;

end.
