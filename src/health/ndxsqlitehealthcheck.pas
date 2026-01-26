{===============================================================================
  NDXSQLite - Health Check
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 5 (depends on ndxsqliteconnectionfactory, ndxsqliteconnectionintf)
===============================================================================}
unit ndxsqlitehealthcheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, DateUtils, Variants,
  ndxsqliteconnectionfactory, ndxsqliteconnectionintf;

type
  { Health check result }
  TNDXSQLiteHealthCheckResult = record
    IsHealthy: Boolean;
    Message: string;
    ResponseTimeMs: Int64;
    ExceptionMessage: string;
  end;

  { Database information }
  TNDXSQLiteDatabaseInfo = record
    SQLiteVersion: string;
    DatabasePath: string;
    DatabaseSizeBytes: Int64;
    PageSize: Integer;
    PageCount: Integer;
    JournalMode: string;
    Encoding: string;
    FreelistCount: Integer;
    AutoVacuum: Integer;
    CacheSize: Integer;
    MaxPageCount: Integer;
  end;

  { Database statistics }
  TNDXSQLiteDatabaseStats = record
    TableCount: Integer;
    IndexCount: Integer;
    TriggerCount: Integer;
    ViewCount: Integer;
  end;

  { Health check }
  TNDXSQLiteHealthCheck = class
  private
    FFactory: INDXSQLiteConnectionFactory;

  public
    constructor Create(AFactory: INDXSQLiteConnectionFactory);

    { Basic health check }

    { Tests the connection with a simple query and returns timing and status. }
    function CheckHealth: TNDXSQLiteHealthCheckResult;

    { Detailed information }

    { Returns database metadata including version, page size, journal mode, and encoding. }
    function GetDatabaseInfo: TNDXSQLiteDatabaseInfo;
    { Returns counts of tables, indexes, triggers, and views. }
    function GetDatabaseStats: TNDXSQLiteDatabaseStats;

    { Integrity check }

    { Runs PRAGMA integrity_check. Set AFull for a complete page-by-page scan. }
    function CheckIntegrity(AFull: Boolean = False): Boolean;
    { Verifies all foreign key constraints are satisfied. }
    function CheckForeignKeys: Boolean;

    { Maintenance }

    { Rebuilds the database file, reclaiming unused space. }
    procedure Vacuum;
    { Updates query planner statistics for all tables and indexes. }
    procedure Analyze;
    { Runs PRAGMA optimize for automatic query planner improvements. }
    procedure Optimize;
    { Performs a WAL checkpoint to transfer WAL content to the database file. }
    procedure Checkpoint;

    { Schema statistics }

    { Returns the number of user tables in the database. }
    function GetTableCount: Integer;
    { Returns the number of indexes in the database. }
    function GetIndexCount: Integer;
    { Returns the number of triggers in the database. }
    function GetTriggerCount: Integer;
    { Returns the number of views in the database. }
    function GetViewCount: Integer;
    { Returns a list of all user table names. Caller must free. }
    function GetTableList: TStringList;
  end;

implementation

{ TNDXSQLiteHealthCheck }

constructor TNDXSQLiteHealthCheck.Create(AFactory: INDXSQLiteConnectionFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TNDXSQLiteHealthCheck.CheckHealth: TNDXSQLiteHealthCheckResult;
var
  Conn: INDXSQLiteConnection;
  StartTime: TDateTime;
  V: Variant;
begin
  StartTime := Now;
  Result.IsHealthy := False;
  Result.Message := '';
  Result.ExceptionMessage := '';

  Conn := nil;
  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;

      V := Conn.ExecuteScalar('SELECT 1');
      Result.ResponseTimeMs := MilliSecondsBetween(Now, StartTime);

      if (not VarIsNull(V)) and (Integer(V) = 1) then
      begin
        Result.IsHealthy := True;
        Result.Message := 'SQLite connection functional';
      end
      else
      begin
        Result.Message := 'Unexpected response from server';
      end;
    except
      on E: Exception do
      begin
        Result.ResponseTimeMs := MilliSecondsBetween(Now, StartTime);
        Result.Message := 'Connection error';
        Result.ExceptionMessage := E.Message;
      end;
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.GetDatabaseInfo: TNDXSQLiteDatabaseInfo;
var
  Conn: INDXSQLiteConnection;
  V: Variant;
begin
  // Initialize record properly - don't use FillChar on records with managed types (strings)
  Result := Default(TNDXSQLiteDatabaseInfo);

  Conn := nil;
  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;

      Result.SQLiteVersion := VarToStr(Conn.ExecuteScalar('SELECT sqlite_version()'));
      Result.DatabasePath := Conn.DatabasePath;

      V := Conn.ExecuteScalar('PRAGMA page_size');
      if not VarIsNull(V) then
        Result.PageSize := V;

      V := Conn.ExecuteScalar('PRAGMA page_count');
      if not VarIsNull(V) then
        Result.PageCount := V;

      Result.DatabaseSizeBytes := Int64(Result.PageSize) * Int64(Result.PageCount);

      Result.JournalMode := VarToStr(Conn.ExecuteScalar('PRAGMA journal_mode'));
      Result.Encoding := VarToStr(Conn.ExecuteScalar('PRAGMA encoding'));

      V := Conn.ExecuteScalar('PRAGMA freelist_count');
      if not VarIsNull(V) then
        Result.FreelistCount := V;

      V := Conn.ExecuteScalar('PRAGMA auto_vacuum');
      if not VarIsNull(V) then
        Result.AutoVacuum := V;

      V := Conn.ExecuteScalar('PRAGMA cache_size');
      if not VarIsNull(V) then
        Result.CacheSize := Abs(Integer(V));

      V := Conn.ExecuteScalar('PRAGMA max_page_count');
      if not VarIsNull(V) then
        Result.MaxPageCount := V;
    except
      on E: Exception do
      begin
        Result.SQLiteVersion := 'Error: ' + E.Message;
      end;
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.GetDatabaseStats: TNDXSQLiteDatabaseStats;
begin
  Result.TableCount := GetTableCount;
  Result.IndexCount := GetIndexCount;
  Result.TriggerCount := GetTriggerCount;
  Result.ViewCount := GetViewCount;
end;

function TNDXSQLiteHealthCheck.CheckIntegrity(AFull: Boolean): Boolean;
var
  Conn: INDXSQLiteConnection;
  V: Variant;
begin
  Result := False;
  Conn := nil;

  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;

      if AFull then
        V := Conn.ExecuteScalar('PRAGMA integrity_check')
      else
        V := Conn.ExecuteScalar('PRAGMA quick_check');

      Result := (not VarIsNull(V)) and (VarToStr(V) = 'ok');
    except
      Result := False;
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.CheckForeignKeys: Boolean;
var
  Conn: INDXSQLiteConnection;
  V: Variant;
begin
  Result := True;
  Conn := nil;

  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;

      V := Conn.ExecuteScalar('PRAGMA foreign_key_check');
      // If the query returns something, there are violations
      Result := VarIsNull(V);
    except
      Result := False;
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

procedure TNDXSQLiteHealthCheck.Vacuum;
var
  Conn: INDXSQLiteConnection;
begin
  Conn := nil;
  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      Conn.ExecuteNonQuery('VACUUM');
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

procedure TNDXSQLiteHealthCheck.Analyze;
var
  Conn: INDXSQLiteConnection;
begin
  Conn := nil;
  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      Conn.ExecuteNonQuery('ANALYZE');
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

procedure TNDXSQLiteHealthCheck.Optimize;
var
  Conn: INDXSQLiteConnection;
begin
  Conn := nil;
  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      Conn.ExecuteNonQuery('PRAGMA optimize');
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

procedure TNDXSQLiteHealthCheck.Checkpoint;
var
  Conn: INDXSQLiteConnection;
begin
  Conn := nil;
  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      Conn.ExecuteNonQuery('PRAGMA wal_checkpoint(TRUNCATE)');
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.GetTableCount: Integer;
var
  Conn: INDXSQLiteConnection;
  V: Variant;
begin
  Result := 0;
  Conn := nil;

  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      V := Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type = ''table'' AND name NOT LIKE ''sqlite_%''');
      if not VarIsNull(V) then
        Result := V;
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.GetIndexCount: Integer;
var
  Conn: INDXSQLiteConnection;
  V: Variant;
begin
  Result := 0;
  Conn := nil;

  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      V := Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type = ''index''');
      if not VarIsNull(V) then
        Result := V;
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.GetTriggerCount: Integer;
var
  Conn: INDXSQLiteConnection;
  V: Variant;
begin
  Result := 0;
  Conn := nil;

  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      V := Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type = ''trigger''');
      if not VarIsNull(V) then
        Result := V;
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.GetViewCount: Integer;
var
  Conn: INDXSQLiteConnection;
  V: Variant;
begin
  Result := 0;
  Conn := nil;

  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      V := Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type = ''view''');
      if not VarIsNull(V) then
        Result := V;
    except
      // Ignore errors
    end;
  finally
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

function TNDXSQLiteHealthCheck.GetTableList: TStringList;
var
  Conn: INDXSQLiteConnection;
  DS: TDataSet;
begin
  Result := TStringList.Create;
  Conn := nil;
  DS := nil;

  try
    try
      Conn := FFactory.CreateConnection;
      Conn.Open;
      DS := Conn.ExecuteQuery(
        'SELECT name FROM sqlite_master WHERE type = ''table'' AND name NOT LIKE ''sqlite_%'' ORDER BY name');
      while not DS.EOF do
      begin
        Result.Add(DS.Fields[0].AsString);
        DS.Next;
      end;
    except
      // Ignore errors
    end;
  finally
    DS.Free;
    if (Conn <> nil) and Conn.IsOpen then
      Conn.Close;
  end;
end;

end.
