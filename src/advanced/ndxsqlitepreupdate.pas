{===============================================================================
  NDXSQLite - Preupdate Hooks Support
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for SQLite preupdate hook mechanism.
  Allows inspection of row data before INSERT/UPDATE/DELETE operations.
  Requires SQLite compiled with SQLITE_ENABLE_PREUPDATE_HOOK.
===============================================================================}
unit ndxsqlitepreupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Preupdate operation type }
  TNDXPreupdateOp = (
    poInsert = SQLITE_INSERT,
    poUpdate = SQLITE_UPDATE,
    poDelete = SQLITE_DELETE
  );

  { Preupdate information record }
  TNDXPreupdateInfo = record
    Operation: TNDXPreupdateOp;
    DatabaseName: string;
    TableName: string;
    OldRowId: Int64;
    NewRowId: Int64;
    ColumnCount: Integer;
    TriggerDepth: Integer;
  end;

  { Forward declaration }
  TNDXSQLitePreupdate = class;

  { Preupdate callback event }
  TNDXPreupdateEvent = procedure(Sender: TNDXSQLitePreupdate;
    const Info: TNDXPreupdateInfo) of object;

  { Column change record for tracking }
  TNDXColumnChange = record
    ColumnIndex: Integer;
    ColumnName: string;
    OldValue: Variant;
    NewValue: Variant;
  end;
  TNDXColumnChanges = array of TNDXColumnChange;

  { Preupdate Hook Manager }
  TNDXSQLitePreupdate = class
  private
    FConnection: INDXSQLiteConnection;
    FEnabled: Boolean;
    FOnPreupdate: TNDXPreupdateEvent;
    FLogEnabled: Boolean;
    FLog: TStringList;
    FTrackChanges: Boolean;
    FLastChanges: TNDXColumnChanges;
    FCurrentDB: Psqlite3;

    function GetDBHandle: Psqlite3;
    procedure DoPreupdateCallback(AOperation: Integer;
      const ADatabase, ATable: string; AOldRowId, ANewRowId: Int64);

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Registers the preupdate hook callback with the SQLite connection. }
    procedure Enable;
    { Unregisters the preupdate hook from the SQLite connection. }
    procedure Disable;

    { Sets the event handler called before each INSERT, UPDATE, or DELETE operation. }
    procedure SetCallback(ACallback: TNDXPreupdateEvent);
    { Removes the currently assigned preupdate event handler. }
    procedure ClearCallback;

    { Returns the old value of the column at the given index. Only valid during a callback. }
    function GetOldValue(AColumnIndex: Integer): Variant;
    { Returns the new value of the column at the given index. Only valid during a callback. }
    function GetNewValue(AColumnIndex: Integer): Variant;
    { Returns the number of columns in the table being modified. Only valid during a callback. }
    function GetColumnCount: Integer;
    { Returns the current trigger nesting depth. Only valid during a callback. }
    function GetTriggerDepth: Integer;

    { Returns an array of all column changes for the current UPDATE operation. Only valid during a callback. }
    function GetChanges: TNDXColumnChanges;

    { Returns True if the SQLite library was compiled with SQLITE_ENABLE_PREUPDATE_HOOK. }
    class function IsAvailable: Boolean;

    { Enables logging of all preupdate events to an internal string list. }
    procedure EnableLogging;
    { Disables preupdate event logging. }
    procedure DisableLogging;
    { Clears all accumulated log entries. }
    procedure ClearLog;
    { Returns the internal log string list. Do not free. }
    function GetLog: TStringList;

    { Converts a preupdate operation type to its string representation (INSERT, UPDATE, DELETE). }
    class function OperationToString(AOp: TNDXPreupdateOp): string;

    { Properties }
    property Connection: INDXSQLiteConnection read FConnection;
    property Enabled: Boolean read FEnabled;
    property OnPreupdate: TNDXPreupdateEvent read FOnPreupdate write FOnPreupdate;
    property LogEnabled: Boolean read FLogEnabled;
    property TrackChanges: Boolean read FTrackChanges write FTrackChanges;
    property LastChanges: TNDXColumnChanges read FLastChanges;
  end;

  { High-level change tracking helper that records all modifications as structured entries. }
  TNDXChangeTracker = class
  private
    FPreupdate: TNDXSQLitePreupdate;
    FChanges: TStringList;
    FTableFilter: string;

    procedure HandlePreupdate(Sender: TNDXSQLitePreupdate;
      const Info: TNDXPreupdateInfo);

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Begins recording changes, optionally filtering to a specific table name. }
    procedure Start(const ATableFilter: string = '');
    { Stops recording changes. }
    procedure Stop;
    { Clears all recorded change entries. }
    procedure Clear;

    { Returns all recorded changes as a JSON-formatted string. }
    function GetChangesAsJSON: string;
    { Returns the number of recorded change entries. }
    function GetChangeCount: Integer;

    property Changes: TStringList read FChanges;
    property TableFilter: string read FTableFilter;
  end;

implementation

var
  GlobalPreupdate: TNDXSQLitePreupdate = nil;

{ Helper to convert sqlite3_value to Variant }
function ValueToVariant(AValue: Psqlite3_value): Variant;
var
  ValType: Integer;
begin
  if AValue = nil then
  begin
    Result := Null;
    Exit;
  end;

  ValType := sqlite3_value_type(AValue);
  case ValType of
    SQLITE_INTEGER:
      Result := sqlite3_value_int64(AValue);
    SQLITE_FLOAT:
      Result := sqlite3_value_double(AValue);
    SQLITE_TEXT:
      Result := string(PAnsiChar(sqlite3_value_text(AValue)));
    SQLITE_BLOB:
      Result := '<BLOB>';
    SQLITE_NULL:
      Result := Null;
  else
    Result := Null;
  end;
end;

{ Preupdate callback - called by SQLite }
procedure PreupdateCallback(pCtx: Pointer; db: Psqlite3;
  op: Integer; zDb, zTable: PAnsiChar; iKey1, iKey2: Int64); cdecl;
var
  Preupdate: TNDXSQLitePreupdate;
  Database, Table: string;
begin
  Preupdate := TNDXSQLitePreupdate(pCtx);
  if Preupdate = nil then
    Exit;

  if zDb <> nil then
    Database := string(zDb)
  else
    Database := '';

  if zTable <> nil then
    Table := string(zTable)
  else
    Table := '';

  Preupdate.FCurrentDB := db;
  try
    Preupdate.DoPreupdateCallback(op, Database, Table, iKey1, iKey2);
  finally
    Preupdate.FCurrentDB := nil;
  end;
end;

{ TNDXSQLitePreupdate }

constructor TNDXSQLitePreupdate.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FEnabled := False;
  FOnPreupdate := nil;
  FLogEnabled := False;
  FLog := TStringList.Create;
  FTrackChanges := False;
  SetLength(FLastChanges, 0);
  FCurrentDB := nil;
end;

destructor TNDXSQLitePreupdate.Destroy;
begin
  Disable;
  FLog.Free;
  SetLength(FLastChanges, 0);
  inherited Destroy;
end;

function TNDXSQLitePreupdate.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

procedure TNDXSQLitePreupdate.DoPreupdateCallback(AOperation: Integer;
  const ADatabase, ATable: string; AOldRowId, ANewRowId: Int64);
var
  Info: TNDXPreupdateInfo;
  LogEntry: string;
  I, ColCount: Integer;
  OldVal, NewVal: Variant;
begin
  // Build info record
  Info.Operation := TNDXPreupdateOp(AOperation);
  Info.DatabaseName := ADatabase;
  Info.TableName := ATable;
  Info.OldRowId := AOldRowId;
  Info.NewRowId := ANewRowId;
  Info.ColumnCount := GetColumnCount;
  Info.TriggerDepth := GetTriggerDepth;

  // Track changes if enabled
  if FTrackChanges and (Info.Operation = poUpdate) then
  begin
    ColCount := Info.ColumnCount;
    SetLength(FLastChanges, ColCount);
    for I := 0 to ColCount - 1 do
    begin
      FLastChanges[I].ColumnIndex := I;
      FLastChanges[I].ColumnName := ''; // Would need schema query
      FLastChanges[I].OldValue := GetOldValue(I);
      FLastChanges[I].NewValue := GetNewValue(I);
    end;
  end;

  // Log if enabled
  if FLogEnabled then
  begin
    LogEntry := Format('[%s] %s on %s.%s: RowId %d -> %d',
      [FormatDateTime('hh:nn:ss.zzz', Now),
       OperationToString(Info.Operation),
       ADatabase, ATable,
       AOldRowId, ANewRowId]);

    if FTrackChanges and (Info.Operation = poUpdate) then
    begin
      for I := 0 to High(FLastChanges) do
      begin
        OldVal := FLastChanges[I].OldValue;
        NewVal := FLastChanges[I].NewValue;
        if not VarSameValue(OldVal, NewVal) then
          LogEntry := LogEntry + Format(' [Col%d: %s -> %s]',
            [I, VarToStr(OldVal), VarToStr(NewVal)]);
      end;
    end;

    FLog.Add(LogEntry);
  end;

  // Call user callback
  if Assigned(FOnPreupdate) then
    FOnPreupdate(Self, Info);
end;

procedure TNDXSQLitePreupdate.Enable;
begin
  if FEnabled then Exit;

  if not IsAvailable then
    raise ENDXSQLiteException.Create('Preupdate hooks are not available. ' +
      'SQLite must be compiled with SQLITE_ENABLE_PREUPDATE_HOOK.');

  sqlite3_preupdate_hook(GetDBHandle, @PreupdateCallback, Self);
  FEnabled := True;
  GlobalPreupdate := Self;
end;

procedure TNDXSQLitePreupdate.Disable;
begin
  if not FEnabled then Exit;

  if IsAvailable then
    sqlite3_preupdate_hook(GetDBHandle, nil, nil);

  FEnabled := False;

  if GlobalPreupdate = Self then
    GlobalPreupdate := nil;
end;

procedure TNDXSQLitePreupdate.SetCallback(ACallback: TNDXPreupdateEvent);
begin
  FOnPreupdate := ACallback;
end;

procedure TNDXSQLitePreupdate.ClearCallback;
begin
  FOnPreupdate := nil;
end;

function TNDXSQLitePreupdate.GetOldValue(AColumnIndex: Integer): Variant;
var
  pValue: Psqlite3_value;
  RC: Integer;
  DB: Psqlite3;
begin
  Result := Null;

  if not IsAvailable then Exit;

  DB := FCurrentDB;
  if DB = nil then
    DB := GetDBHandle;

  pValue := nil;
  RC := sqlite3_preupdate_old(DB, AColumnIndex, pValue);
  if RC = SQLITE_OK then
    Result := ValueToVariant(pValue);
end;

function TNDXSQLitePreupdate.GetNewValue(AColumnIndex: Integer): Variant;
var
  pValue: Psqlite3_value;
  RC: Integer;
  DB: Psqlite3;
begin
  Result := Null;

  if not IsAvailable then Exit;

  DB := FCurrentDB;
  if DB = nil then
    DB := GetDBHandle;

  pValue := nil;
  RC := sqlite3_preupdate_new(DB, AColumnIndex, pValue);
  if RC = SQLITE_OK then
    Result := ValueToVariant(pValue);
end;

function TNDXSQLitePreupdate.GetColumnCount: Integer;
var
  DB: Psqlite3;
begin
  Result := 0;

  if not IsAvailable then Exit;

  DB := FCurrentDB;
  if DB = nil then
    DB := GetDBHandle;

  Result := sqlite3_preupdate_count(DB);
end;

function TNDXSQLitePreupdate.GetTriggerDepth: Integer;
var
  DB: Psqlite3;
begin
  Result := 0;

  if not IsAvailable then Exit;

  DB := FCurrentDB;
  if DB = nil then
    DB := GetDBHandle;

  Result := sqlite3_preupdate_depth(DB);
end;

function TNDXSQLitePreupdate.GetChanges: TNDXColumnChanges;
var
  I, ColCount: Integer;
begin
  ColCount := GetColumnCount;
  SetLength(Result, ColCount);

  for I := 0 to ColCount - 1 do
  begin
    Result[I].ColumnIndex := I;
    Result[I].ColumnName := '';
    Result[I].OldValue := GetOldValue(I);
    Result[I].NewValue := GetNewValue(I);
  end;
end;

class function TNDXSQLitePreupdate.IsAvailable: Boolean;
begin
  Result := Assigned(sqlite3_preupdate_hook) and
            Assigned(sqlite3_preupdate_old) and
            Assigned(sqlite3_preupdate_new) and
            Assigned(sqlite3_preupdate_count);
end;

procedure TNDXSQLitePreupdate.EnableLogging;
begin
  FLogEnabled := True;
end;

procedure TNDXSQLitePreupdate.DisableLogging;
begin
  FLogEnabled := False;
end;

procedure TNDXSQLitePreupdate.ClearLog;
begin
  FLog.Clear;
end;

function TNDXSQLitePreupdate.GetLog: TStringList;
begin
  Result := FLog;
end;

class function TNDXSQLitePreupdate.OperationToString(AOp: TNDXPreupdateOp): string;
begin
  case AOp of
    poInsert: Result := 'INSERT';
    poUpdate: Result := 'UPDATE';
    poDelete: Result := 'DELETE';
  else
    Result := Format('UNKNOWN(%d)', [Integer(AOp)]);
  end;
end;

{ TNDXChangeTracker }

constructor TNDXChangeTracker.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FPreupdate := TNDXSQLitePreupdate.Create(AConnection);
  FChanges := TStringList.Create;
  FTableFilter := '';
end;

destructor TNDXChangeTracker.Destroy;
begin
  Stop;
  FChanges.Free;
  FPreupdate.Free;
  inherited Destroy;
end;

procedure TNDXChangeTracker.HandlePreupdate(Sender: TNDXSQLitePreupdate;
  const Info: TNDXPreupdateInfo);
var
  Entry: string;
  I: Integer;
  ColChanges: TNDXColumnChanges;
begin
  // Apply table filter
  if (FTableFilter <> '') and (not SameText(Info.TableName, FTableFilter)) then
    Exit;

  Entry := Format('{"op":"%s","db":"%s","table":"%s","oldRowId":%d,"newRowId":%d',
    [TNDXSQLitePreupdate.OperationToString(Info.Operation),
     Info.DatabaseName,
     Info.TableName,
     Info.OldRowId,
     Info.NewRowId]);

  // Add column changes for UPDATE
  if Info.Operation = poUpdate then
  begin
    ColChanges := Sender.GetChanges;
    Entry := Entry + ',"changes":[';
    for I := 0 to High(ColChanges) do
    begin
      if I > 0 then
        Entry := Entry + ',';
      Entry := Entry + Format('{"col":%d,"old":"%s","new":"%s"}',
        [ColChanges[I].ColumnIndex,
         VarToStr(ColChanges[I].OldValue),
         VarToStr(ColChanges[I].NewValue)]);
    end;
    Entry := Entry + ']';
  end;

  Entry := Entry + '}';
  FChanges.Add(Entry);
end;

procedure TNDXChangeTracker.Start(const ATableFilter: string);
begin
  FTableFilter := ATableFilter;
  FPreupdate.TrackChanges := True;
  FPreupdate.SetCallback(@HandlePreupdate);
  FPreupdate.Enable;
end;

procedure TNDXChangeTracker.Stop;
begin
  FPreupdate.Disable;
  FPreupdate.ClearCallback;
end;

procedure TNDXChangeTracker.Clear;
begin
  FChanges.Clear;
end;

function TNDXChangeTracker.GetChangesAsJSON: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to FChanges.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + FChanges[I];
  end;
  Result := Result + ']';
end;

function TNDXChangeTracker.GetChangeCount: Integer;
begin
  Result := FChanges.Count;
end;

end.
