{===============================================================================
  NDXSQLite - Schema Migrations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Provides a versioned schema migration framework for SQLite databases.
  Migrations are registered with unique version numbers and executed in order.
  Each migration supports both upgrade (Up) and rollback (Down) operations.
  Migration history is tracked in a dedicated database table, enabling
  incremental schema evolution and safe rollback capabilities.

  Usage:
    Manager := TNDXMigrationManager.Create(Connection);
    Manager.RegisterSQLMigration(1, 'CreateUsers',
      'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)',
      'DROP TABLE users');
    Manager.MigrateUp;  // Applies all pending migrations
===============================================================================}
unit ndxsqlitemigration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants, DateUtils,
  ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Callback for migration progress logging messages. }
  TNDXMigrationLogEvent = procedure(const AMessage: string) of object;

  { Direction of migration execution. }
  TNDXMigrationDirection = (mdUp, mdDown);

  { Current status of a migration version. }
  TNDXMigrationStatus = (msPending, msApplied, msFailed);

  { Record containing metadata about a migration version. }
  TNDXMigrationInfo = record
    Version: Integer;          // Sequential version number
    Name: string;              // Human-readable migration name
    Description: string;       // Optional description of schema changes
    AppliedAt: TDateTime;      // Timestamp when the migration was applied
    Status: TNDXMigrationStatus; // Current migration status
    ExecutionTimeMs: Integer;  // Execution time in milliseconds
  end;

  { Abstract base class for database migrations.
    Subclasses must implement DoUp and DoDown to define schema changes.
    Each migration is identified by a unique sequential version number. }
  TNDXMigration = class
  private
    FVersion: Integer;
    FName: string;
    FDescription: string;
  protected
    { Applies the schema changes. Override in descendants. }
    procedure DoUp(AConnection: INDXSQLiteConnection); virtual; abstract;
    { Reverts the schema changes. Override in descendants. }
    procedure DoDown(AConnection: INDXSQLiteConnection); virtual; abstract;
  public
    { Creates a migration with a version number, name, and optional description. }
    constructor Create(AVersion: Integer; const AName: string;
      const ADescription: string = '');

    { Executes the forward migration (applies schema changes). }
    procedure Up(AConnection: INDXSQLiteConnection);
    { Executes the reverse migration (reverts schema changes). }
    procedure Down(AConnection: INDXSQLiteConnection);

    property Version: Integer read FVersion;
    property Name: string read FName;
    property Description: string read FDescription;
  end;

  { SQL-based migration that executes one or more SQL statements.
    Stores separate lists of SQL statements for up (apply) and down (rollback)
    directions. Statements are executed sequentially in the order they were added. }
  TNDXSQLMigration = class(TNDXMigration)
  private
    FUpSQL: TStringList;
    FDownSQL: TStringList;
  protected
    procedure DoUp(AConnection: INDXSQLiteConnection); override;
    procedure DoDown(AConnection: INDXSQLiteConnection); override;
  public
    constructor Create(AVersion: Integer; const AName: string;
      const ADescription: string = '');
    destructor Destroy; override;

    { Adds a SQL statement to the forward migration sequence. }
    procedure AddUpSQL(const ASQL: string);
    { Adds a SQL statement to the rollback migration sequence. }
    procedure AddDownSQL(const ASQL: string);

    property UpSQL: TStringList read FUpSQL;
    property DownSQL: TStringList read FDownSQL;
  end;

  { Manages schema migration registration, execution, and history tracking.
    Automatically creates a migration tracking table (default: _migrations) to
    record which versions have been applied. Migrations run within transactions
    for atomicity and are rolled back on failure. }
  TNDXMigrationManager = class
  private
    FConnection: INDXSQLiteConnection;
    FMigrations: TList;
    FTableName: string;
    FOnProgress: TNotifyEvent;
    FOnLog: TNDXMigrationLogEvent;

    { Creates the migration tracking table if it does not exist. }
    procedure EnsureMigrationTable;
    { Returns the highest applied version number (0 if none). }
    function GetCurrentVersion: Integer;
    { Returns a list of TNDXMigrationInfo records for all applied migrations. Caller must free. }
    function GetAppliedMigrations: TList;
    { Finds a registered migration by version number. Returns nil if not found. }
    function FindMigration(AVersion: Integer): TNDXMigration;
    { Records a migration execution (insert for Up, delete for Down). }
    procedure RecordMigration(AMigration: TNDXMigration; ADirection: TNDXMigrationDirection;
      AExecutionTimeMs: Integer);
    { Removes a migration record from the tracking table. }
    procedure RemoveMigrationRecord(AVersion: Integer);
    { Sends a message to the OnLog callback if assigned. }
    procedure Log(const AMessage: string);

  public
    { Creates a migration manager for the given connection.
      @param AConnection  The database connection to manage.
      @param ATableName   Name of the migration tracking table (default: _migrations). }
    constructor Create(AConnection: INDXSQLiteConnection;
      const ATableName: string = '_migrations');
    destructor Destroy; override;

    { Migration registration }

    { Registers a migration instance. The manager takes ownership. }
    procedure RegisterMigration(AMigration: TNDXMigration);
    { Creates and registers a simple SQL migration with up and down statements. }
    procedure RegisterSQLMigration(AVersion: Integer; const AName: string;
      const AUpSQL, ADownSQL: string);

    { Migration execution }

    { Applies all pending migrations in version order. }
    procedure MigrateUp; overload;
    { Applies pending migrations up to and including the target version. }
    procedure MigrateUp(ATargetVersion: Integer); overload;
    { Rolls back the most recently applied migration. }
    procedure MigrateDown; overload;
    { Rolls back migrations down to the target version (exclusive). }
    procedure MigrateDown(ATargetVersion: Integer); overload;
    { Migrates up or down to reach the exact target version. }
    procedure MigrateTo(ATargetVersion: Integer);
    { Rolls back all applied migrations (equivalent to MigrateDown(0)). }
    procedure Reset;

    { Information }

    { Returns a list of registered migrations not yet applied. Caller must free. }
    function GetPendingMigrations: TList;
    { Returns a list of TNDXMigrationInfo records for applied migrations. Caller must free. }
    function GetMigrationHistory: TList;
    { Returns a human-readable status report as a string list. Caller must free. }
    function GetMigrationStatus: TStringList;
    { Returns True if there are registered migrations not yet applied. }
    function HasPendingMigrations: Boolean;

    property CurrentVersion: Integer read GetCurrentVersion;
    property TableName: string read FTableName;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnLog: TNDXMigrationLogEvent read FOnLog write FOnLog;
  end;

implementation

{ TNDXMigration - Abstract base class for versioned schema changes }

constructor TNDXMigration.Create(AVersion: Integer; const AName: string;
  const ADescription: string);
begin
  inherited Create;
  FVersion := AVersion;
  FName := AName;
  FDescription := ADescription;
end;

procedure TNDXMigration.Up(AConnection: INDXSQLiteConnection);
begin
  DoUp(AConnection);
end;

procedure TNDXMigration.Down(AConnection: INDXSQLiteConnection);
begin
  DoDown(AConnection);
end;

{ TNDXSQLMigration - Executes SQL statement lists for up/down migrations }

constructor TNDXSQLMigration.Create(AVersion: Integer; const AName: string;
  const ADescription: string);
begin
  inherited Create(AVersion, AName, ADescription);
  FUpSQL := TStringList.Create;
  FDownSQL := TStringList.Create;
end;

destructor TNDXSQLMigration.Destroy;
begin
  FreeAndNil(FUpSQL);
  FreeAndNil(FDownSQL);
  inherited Destroy;
end;

procedure TNDXSQLMigration.AddUpSQL(const ASQL: string);
begin
  FUpSQL.Add(ASQL);
end;

procedure TNDXSQLMigration.AddDownSQL(const ASQL: string);
begin
  FDownSQL.Add(ASQL);
end;

procedure TNDXSQLMigration.DoUp(AConnection: INDXSQLiteConnection);
var
  I: Integer;
begin
  for I := 0 to FUpSQL.Count - 1 do
  begin
    if Trim(FUpSQL[I]) <> '' then
      AConnection.ExecuteNonQuery(FUpSQL[I]);
  end;
end;

procedure TNDXSQLMigration.DoDown(AConnection: INDXSQLiteConnection);
var
  I: Integer;
begin
  for I := 0 to FDownSQL.Count - 1 do
  begin
    if Trim(FDownSQL[I]) <> '' then
      AConnection.ExecuteNonQuery(FDownSQL[I]);
  end;
end;

{ TNDXMigrationManager - Orchestrates migration registration and execution }

constructor TNDXMigrationManager.Create(AConnection: INDXSQLiteConnection;
  const ATableName: string);
begin
  inherited Create;
  FConnection := AConnection;
  FTableName := ATableName;
  FMigrations := TList.Create;
  EnsureMigrationTable;
end;

destructor TNDXMigrationManager.Destroy;
var
  I: Integer;
begin
  for I := FMigrations.Count - 1 downto 0 do
    TNDXMigration(FMigrations[I]).Free;
  FreeAndNil(FMigrations);
  inherited Destroy;
end;

procedure TNDXMigrationManager.EnsureMigrationTable;
begin
  FConnection.ExecuteNonQuery(Format(
    'CREATE TABLE IF NOT EXISTS %s (' +
    '  version INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  applied_at TEXT NOT NULL,' +
    '  execution_time_ms INTEGER' +
    ')', [FTableName]));
end;

function TNDXMigrationManager.GetCurrentVersion: Integer;
var
  V: Variant;
begin
  V := FConnection.ExecuteScalar(Format(
    'SELECT MAX(version) FROM %s', [FTableName]));
  if VarIsNull(V) then
    Result := 0
  else
    Result := V;
end;

function TNDXMigrationManager.GetAppliedMigrations: TList;
var
  DS: TDataSet;
  Info: ^TNDXMigrationInfo;
begin
  Result := TList.Create;
  DS := FConnection.ExecuteQuery(Format(
    'SELECT version, name, description, applied_at, execution_time_ms ' +
    'FROM %s ORDER BY version', [FTableName]));
  try
    while not DS.EOF do
    begin
      New(Info);
      Info^.Version := DS.FieldByName('version').AsInteger;
      Info^.Name := DS.FieldByName('name').AsString;
      Info^.Description := DS.FieldByName('description').AsString;
      // Use ScanDateTime with ISO format to avoid locale issues
      Info^.AppliedAt := ScanDateTime('yyyy-mm-dd hh:nn:ss',
        DS.FieldByName('applied_at').AsString);
      Info^.Status := msApplied;
      Info^.ExecutionTimeMs := DS.FieldByName('execution_time_ms').AsInteger;
      Result.Add(Info);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

function TNDXMigrationManager.FindMigration(AVersion: Integer): TNDXMigration;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FMigrations.Count - 1 do
  begin
    if TNDXMigration(FMigrations[I]).Version = AVersion then
    begin
      Result := TNDXMigration(FMigrations[I]);
      Break;
    end;
  end;
end;

procedure TNDXMigrationManager.RecordMigration(AMigration: TNDXMigration;
  ADirection: TNDXMigrationDirection; AExecutionTimeMs: Integer);
begin
  if ADirection = mdUp then
  begin
    FConnection.ExecuteNonQuery(Format(
      'INSERT INTO %s (version, name, description, applied_at, execution_time_ms) ' +
      'VALUES (%d, ''%s'', ''%s'', ''%s'', %d)',
      [FTableName, AMigration.Version, AMigration.Name,
       StringReplace(AMigration.Description, '''', '''''', [rfReplaceAll]),
       FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AExecutionTimeMs]));
  end
  else
  begin
    RemoveMigrationRecord(AMigration.Version);
  end;
end;

procedure TNDXMigrationManager.RemoveMigrationRecord(AVersion: Integer);
begin
  FConnection.ExecuteNonQuery(Format(
    'DELETE FROM %s WHERE version = %d', [FTableName, AVersion]));
end;

procedure TNDXMigrationManager.Log(const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage);
end;

procedure TNDXMigrationManager.RegisterMigration(AMigration: TNDXMigration);
begin
  FMigrations.Add(AMigration);
end;

procedure TNDXMigrationManager.RegisterSQLMigration(AVersion: Integer;
  const AName: string; const AUpSQL, ADownSQL: string);
var
  Migration: TNDXSQLMigration;
begin
  Migration := TNDXSQLMigration.Create(AVersion, AName);
  Migration.AddUpSQL(AUpSQL);
  Migration.AddDownSQL(ADownSQL);
  RegisterMigration(Migration);
end;

procedure TNDXMigrationManager.MigrateUp;
var
  I: Integer;
  Migration: TNDXMigration;
  CurrentVer: Integer;
  StartTime: TDateTime;
  ExecTimeMs: Integer;
begin
  CurrentVer := GetCurrentVersion;

  // Sort migrations by version to ensure correct execution order
  for I := 0 to FMigrations.Count - 2 do
  begin
    if TNDXMigration(FMigrations[I]).Version > TNDXMigration(FMigrations[I + 1]).Version then
      FMigrations.Exchange(I, I + 1);
  end;

  // Apply migrations
  for I := 0 to FMigrations.Count - 1 do
  begin
    Migration := TNDXMigration(FMigrations[I]);
    if Migration.Version > CurrentVer then
    begin
      Log(Format('Applying migration %d: %s', [Migration.Version, Migration.Name]));

      FConnection.BeginTransaction;
      try
        StartTime := Now;
        Migration.Up(FConnection);
        ExecTimeMs := MilliSecondsBetween(Now, StartTime);
        RecordMigration(Migration, mdUp, ExecTimeMs);
        FConnection.Commit;

        Log(Format('Migration %d applied successfully (%d ms)',
          [Migration.Version, ExecTimeMs]));

        if Assigned(FOnProgress) then
          FOnProgress(Self);
      except
        on E: Exception do
        begin
          FConnection.Rollback;
          Log(Format('Migration %d failed: %s', [Migration.Version, E.Message]));
          raise;
        end;
      end;
    end;
  end;
end;

procedure TNDXMigrationManager.MigrateUp(ATargetVersion: Integer);
var
  I: Integer;
  Migration: TNDXMigration;
  CurrentVer: Integer;
  StartTime: TDateTime;
  ExecTimeMs: Integer;
begin
  CurrentVer := GetCurrentVersion;

  for I := 0 to FMigrations.Count - 1 do
  begin
    Migration := TNDXMigration(FMigrations[I]);
    if (Migration.Version > CurrentVer) and (Migration.Version <= ATargetVersion) then
    begin
      Log(Format('Applying migration %d: %s', [Migration.Version, Migration.Name]));

      FConnection.BeginTransaction;
      try
        StartTime := Now;
        Migration.Up(FConnection);
        ExecTimeMs := MilliSecondsBetween(Now, StartTime);
        RecordMigration(Migration, mdUp, ExecTimeMs);
        FConnection.Commit;

        Log(Format('Migration %d applied successfully (%d ms)',
          [Migration.Version, ExecTimeMs]));
      except
        on E: Exception do
        begin
          FConnection.Rollback;
          Log(Format('Migration %d failed: %s', [Migration.Version, E.Message]));
          raise;
        end;
      end;
    end;
  end;
end;

procedure TNDXMigrationManager.MigrateDown;
var
  Migration: TNDXMigration;
  CurrentVer: Integer;
  StartTime: TDateTime;
  ExecTimeMs: Integer;
begin
  CurrentVer := GetCurrentVersion;
  if CurrentVer = 0 then
    Exit;

  Migration := FindMigration(CurrentVer);
  if Migration = nil then
    raise ENDXSQLiteException.Create(Format('Migration %d not found', [CurrentVer]));

  Log(Format('Rolling back migration %d: %s', [Migration.Version, Migration.Name]));

  FConnection.BeginTransaction;
  try
    StartTime := Now;
    Migration.Down(FConnection);
    ExecTimeMs := MilliSecondsBetween(Now, StartTime);
    RecordMigration(Migration, mdDown, ExecTimeMs);
    FConnection.Commit;

    Log(Format('Migration %d rolled back successfully (%d ms)',
      [Migration.Version, ExecTimeMs]));
  except
    on E: Exception do
    begin
      FConnection.Rollback;
      Log(Format('Rollback of migration %d failed: %s', [Migration.Version, E.Message]));
      raise;
    end;
  end;
end;

procedure TNDXMigrationManager.MigrateDown(ATargetVersion: Integer);
var
  CurrentVer: Integer;
begin
  CurrentVer := GetCurrentVersion;
  while CurrentVer > ATargetVersion do
  begin
    MigrateDown;
    CurrentVer := GetCurrentVersion;
  end;
end;

procedure TNDXMigrationManager.MigrateTo(ATargetVersion: Integer);
var
  CurrentVer: Integer;
begin
  CurrentVer := GetCurrentVersion;
  if ATargetVersion > CurrentVer then
    MigrateUp(ATargetVersion)
  else if ATargetVersion < CurrentVer then
    MigrateDown(ATargetVersion);
end;

procedure TNDXMigrationManager.Reset;
begin
  MigrateDown(0);
end;

function TNDXMigrationManager.GetPendingMigrations: TList;
var
  I: Integer;
  Migration: TNDXMigration;
  CurrentVer: Integer;
begin
  Result := TList.Create;
  CurrentVer := GetCurrentVersion;

  for I := 0 to FMigrations.Count - 1 do
  begin
    Migration := TNDXMigration(FMigrations[I]);
    if Migration.Version > CurrentVer then
      Result.Add(Migration);
  end;
end;

function TNDXMigrationManager.GetMigrationHistory: TList;
begin
  Result := GetAppliedMigrations;
end;

function TNDXMigrationManager.GetMigrationStatus: TStringList;
var
  I: Integer;
  Migration: TNDXMigration;
  CurrentVer: Integer;
  Status: string;
begin
  Result := TStringList.Create;
  CurrentVer := GetCurrentVersion;

  Result.Add(Format('Current version: %d', [CurrentVer]));
  Result.Add('');
  Result.Add('Migrations:');

  for I := 0 to FMigrations.Count - 1 do
  begin
    Migration := TNDXMigration(FMigrations[I]);
    if Migration.Version <= CurrentVer then
      Status := '[APPLIED]'
    else
      Status := '[PENDING]';

    Result.Add(Format('  %s %d: %s', [Status, Migration.Version, Migration.Name]));
  end;
end;

function TNDXMigrationManager.HasPendingMigrations: Boolean;
var
  Pending: TList;
begin
  Pending := GetPendingMigrations;
  try
    Result := Pending.Count > 0;
  finally
    Pending.Free;
  end;
end;

end.
