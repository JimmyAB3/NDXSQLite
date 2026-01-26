{===============================================================================
  NDXSQLite - Multi-Database Attachment Support
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for ATTACH/DETACH DATABASE operations.
  Allows querying multiple SQLite databases in a single connection.
===============================================================================}
unit ndxsqliteattached;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions,
  ndxsqliteplatform;

type
  { Attached database information }
  TNDXAttachedDatabase = class
  private
    FAlias: string;
    FFilePath: string;
    FIsReadOnly: Boolean;
  public
    property Alias: string read FAlias;
    property FilePath: string read FFilePath;
    property IsReadOnly: Boolean read FIsReadOnly;
  end;

  { Multi-Database Attachment Manager }
  TNDXSQLiteAttached = class
  private
    FConnection: INDXSQLiteConnection;
    FAttachedDatabases: TList;

    function GetDBHandle: Psqlite3;
    function FindAttached(const AAlias: string): TNDXAttachedDatabase;
    procedure ClearAttached;
    function EscapeString(const S: string): string;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Attaches an external database file under the given alias for cross-database queries. }
    procedure Attach(const AFilePath, AAlias: string);

    { Attaches an external database file as read-only under the given alias. }
    procedure AttachReadOnly(const AFilePath, AAlias: string);

    { Attaches a new in-memory database under the given alias. }
    procedure AttachMemory(const AAlias: string);

    { Detaches the database identified by the given alias. }
    procedure Detach(const AAlias: string);

    { Returns True if a database with the given alias is currently attached. }
    function IsAttached(const AAlias: string): Boolean;

    { Returns the information record for the attached database with the given alias. }
    function GetAttachedInfo(const AAlias: string): TNDXAttachedDatabase;

    { Returns a list of all currently attached database aliases. Caller must free. }
    function GetAttachedList: TStringList;

    { Returns a dataset with all attached databases from PRAGMA database_list. Caller must free. }
    function GetDatabaseList: TDataSet;

    { Detaches all currently attached databases. }
    procedure DetachAll;

    { Copies a table including schema and data between two attached databases. }
    procedure CopyTable(const ASourceAlias, ASourceTable,
      ADestAlias, ADestTable: string; ACreateIfNotExists: Boolean = True);

    { Copies only the table structure (no data) between two attached databases. }
    procedure CopyTableSchema(const ASourceAlias, ASourceTable,
      ADestAlias, ADestTable: string);

    { Executes a SELECT query that may reference multiple attached databases. Caller must free. }
    function QueryAttached(const ASQL: string): TDataSet;
    { Executes a non-query SQL statement across attached databases and returns rows affected. }
    function ExecuteAttached(const ASQL: string): Integer;

    { Returns a list of all table names in the specified attached database. Caller must free. }
    function GetTables(const AAlias: string): TStringList;

    { Returns the CREATE TABLE SQL statement for the specified table in the attached database. }
    function GetTableSchema(const AAlias, ATableName: string): string;

    { Returns the maximum number of databases that can be attached simultaneously. }
    function GetMaxAttachLimit: Integer;

    { Returns the number of currently attached databases (excluding main and temp). }
    function GetAttachmentCount: Integer;

    property Connection: INDXSQLiteConnection read FConnection;
  end;

implementation

{ TNDXSQLiteAttached }

constructor TNDXSQLiteAttached.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FAttachedDatabases := TList.Create;
end;

destructor TNDXSQLiteAttached.Destroy;
begin
  DetachAll;
  FAttachedDatabases.Free;
  inherited Destroy;
end;

function TNDXSQLiteAttached.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

function TNDXSQLiteAttached.FindAttached(const AAlias: string): TNDXAttachedDatabase;
var
  I: Integer;
  DB: TNDXAttachedDatabase;
begin
  Result := nil;
  for I := 0 to FAttachedDatabases.Count - 1 do
  begin
    DB := TNDXAttachedDatabase(FAttachedDatabases[I]);
    if SameText(DB.FAlias, AAlias) then
    begin
      Result := DB;
      Exit;
    end;
  end;
end;

procedure TNDXSQLiteAttached.ClearAttached;
var
  I: Integer;
begin
  for I := 0 to FAttachedDatabases.Count - 1 do
    TNDXAttachedDatabase(FAttachedDatabases[I]).Free;
  FAttachedDatabases.Clear;
end;

function TNDXSQLiteAttached.EscapeString(const S: string): string;
begin
  Result := StringReplace(S, '''', '''''', [rfReplaceAll]);
end;

procedure TNDXSQLiteAttached.Attach(const AFilePath, AAlias: string);
var
  NormalizedPath: string;
  DBInfo: TNDXAttachedDatabase;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to attach databases');

  // Check if already attached
  if FindAttached(AAlias) <> nil then
    raise ENDXSQLiteException.CreateFmt('Database alias "%s" is already in use', [AAlias]);

  // Normalize path
  NormalizedPath := TNDXPlatform.NormalizePath(AFilePath);

  // Check if file exists
  if not FileExists(NormalizedPath) then
    raise ENDXSQLiteException.CreateFmt('Database file not found: %s', [NormalizedPath]);

  // Attach the database
  FConnection.ExecuteNonQuery(Format('ATTACH DATABASE ''%s'' AS %s',
    [TNDXPlatform.EscapeSQLPath(NormalizedPath), AAlias]));

  // Track attachment
  DBInfo := TNDXAttachedDatabase.Create;
  DBInfo.FAlias := AAlias;
  DBInfo.FFilePath := NormalizedPath;
  DBInfo.FIsReadOnly := False;
  FAttachedDatabases.Add(DBInfo);
end;

procedure TNDXSQLiteAttached.AttachReadOnly(const AFilePath, AAlias: string);
var
  NormalizedPath: string;
  DBInfo: TNDXAttachedDatabase;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to attach databases');

  // Check if already attached
  if FindAttached(AAlias) <> nil then
    raise ENDXSQLiteException.CreateFmt('Database alias "%s" is already in use', [AAlias]);

  // Normalize path
  NormalizedPath := TNDXPlatform.NormalizePath(AFilePath);

  // Check if file exists
  if not FileExists(NormalizedPath) then
    raise ENDXSQLiteException.CreateFmt('Database file not found: %s', [NormalizedPath]);

  // Attach the database with read-only mode via URI
  FConnection.ExecuteNonQuery(Format('ATTACH DATABASE ''file:%s?mode=ro'' AS %s',
    [TNDXPlatform.EscapeSQLPath(NormalizedPath), AAlias]));

  // Track attachment
  DBInfo := TNDXAttachedDatabase.Create;
  DBInfo.FAlias := AAlias;
  DBInfo.FFilePath := NormalizedPath;
  DBInfo.FIsReadOnly := True;
  FAttachedDatabases.Add(DBInfo);
end;

procedure TNDXSQLiteAttached.AttachMemory(const AAlias: string);
var
  DBInfo: TNDXAttachedDatabase;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to attach databases');

  // Check if already attached
  if FindAttached(AAlias) <> nil then
    raise ENDXSQLiteException.CreateFmt('Database alias "%s" is already in use', [AAlias]);

  // Attach in-memory database
  FConnection.ExecuteNonQuery(Format('ATTACH DATABASE '':memory:'' AS %s', [AAlias]));

  // Track attachment
  DBInfo := TNDXAttachedDatabase.Create;
  DBInfo.FAlias := AAlias;
  DBInfo.FFilePath := ':memory:';
  DBInfo.FIsReadOnly := False;
  FAttachedDatabases.Add(DBInfo);
end;

procedure TNDXSQLiteAttached.Detach(const AAlias: string);
var
  DBInfo: TNDXAttachedDatabase;
begin
  DBInfo := FindAttached(AAlias);
  if DBInfo = nil then
    raise ENDXSQLiteException.CreateFmt('Database "%s" is not attached', [AAlias]);

  // Detach the database
  FConnection.ExecuteNonQuery(Format('DETACH DATABASE %s', [AAlias]));

  // Remove from tracking
  FAttachedDatabases.Remove(DBInfo);
  DBInfo.Free;
end;

function TNDXSQLiteAttached.IsAttached(const AAlias: string): Boolean;
begin
  Result := FindAttached(AAlias) <> nil;
end;

function TNDXSQLiteAttached.GetAttachedInfo(const AAlias: string): TNDXAttachedDatabase;
begin
  Result := FindAttached(AAlias);
end;

function TNDXSQLiteAttached.GetAttachedList: TStringList;
var
  I: Integer;
  DBInfo: TNDXAttachedDatabase;
begin
  Result := TStringList.Create;
  for I := 0 to FAttachedDatabases.Count - 1 do
  begin
    DBInfo := TNDXAttachedDatabase(FAttachedDatabases[I]);
    if DBInfo.FIsReadOnly then
      Result.Add(Format('%s (read-only): %s', [DBInfo.FAlias, DBInfo.FFilePath]))
    else
      Result.Add(Format('%s: %s', [DBInfo.FAlias, DBInfo.FFilePath]));
  end;
end;

function TNDXSQLiteAttached.GetDatabaseList: TDataSet;
begin
  Result := FConnection.ExecuteQuery('PRAGMA database_list');
end;

procedure TNDXSQLiteAttached.DetachAll;
var
  I: Integer;
  DBInfo: TNDXAttachedDatabase;
begin
  // Detach in reverse order
  for I := FAttachedDatabases.Count - 1 downto 0 do
  begin
    DBInfo := TNDXAttachedDatabase(FAttachedDatabases[I]);
    if FConnection.IsOpen then
    begin
      try
        FConnection.ExecuteNonQuery(Format('DETACH DATABASE %s', [DBInfo.FAlias]));
      except
        // Ignore errors during cleanup
      end;
    end;
  end;

  ClearAttached;
end;

procedure TNDXSQLiteAttached.CopyTable(const ASourceAlias, ASourceTable,
  ADestAlias, ADestTable: string; ACreateIfNotExists: Boolean);
var
  SourceFull, DestFull: string;
  Schema: string;
begin
  SourceFull := Format('%s.%s', [ASourceAlias, ASourceTable]);
  DestFull := Format('%s.%s', [ADestAlias, ADestTable]);

  // Create table if requested
  if ACreateIfNotExists then
  begin
    Schema := GetTableSchema(ASourceAlias, ASourceTable);
    if Schema <> '' then
    begin
      // Replace table name in schema
      Schema := StringReplace(Schema, ASourceTable, DestFull, []);
      Schema := StringReplace(Schema, 'CREATE TABLE', 'CREATE TABLE IF NOT EXISTS', [rfIgnoreCase]);
      FConnection.ExecuteNonQuery(Schema);
    end;
  end;

  // Copy data
  FConnection.ExecuteNonQuery(Format('INSERT INTO %s SELECT * FROM %s',
    [DestFull, SourceFull]));
end;

procedure TNDXSQLiteAttached.CopyTableSchema(const ASourceAlias, ASourceTable,
  ADestAlias, ADestTable: string);
var
  Schema: string;
  DestFull: string;
begin
  DestFull := Format('%s.%s', [ADestAlias, ADestTable]);
  Schema := GetTableSchema(ASourceAlias, ASourceTable);

  if Schema <> '' then
  begin
    // Replace source table name with destination
    Schema := StringReplace(Schema, ASourceTable, DestFull, []);
    FConnection.ExecuteNonQuery(Schema);
  end;
end;

function TNDXSQLiteAttached.QueryAttached(const ASQL: string): TDataSet;
begin
  Result := FConnection.ExecuteQuery(ASQL);
end;

function TNDXSQLiteAttached.ExecuteAttached(const ASQL: string): Integer;
begin
  Result := FConnection.ExecuteNonQuery(ASQL);
end;

function TNDXSQLiteAttached.GetTables(const AAlias: string): TStringList;
var
  DS: TDataSet;
begin
  Result := TStringList.Create;
  DS := FConnection.ExecuteQuery(Format(
    'SELECT name FROM %s.sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%%'' ORDER BY name',
    [AAlias]));
  try
    while not DS.EOF do
    begin
      Result.Add(DS.Fields[0].AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

function TNDXSQLiteAttached.GetTableSchema(const AAlias, ATableName: string): string;
var
  V: Variant;
begin
  V := FConnection.ExecuteScalar(Format(
    'SELECT sql FROM %s.sqlite_master WHERE type=''table'' AND name=''%s''',
    [AAlias, EscapeString(ATableName)]));

  if VarIsNull(V) then
    Result := ''
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteAttached.GetMaxAttachLimit: Integer;
begin
  Result := sqlite3_limit(GetDBHandle, SQLITE_LIMIT_ATTACHED, -1);
end;

function TNDXSQLiteAttached.GetAttachmentCount: Integer;
begin
  Result := FAttachedDatabases.Count;
end;

end.
