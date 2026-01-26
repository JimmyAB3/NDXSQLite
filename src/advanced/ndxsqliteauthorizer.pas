{===============================================================================
  NDXSQLite - Custom Authorizer Support
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for SQLite authorizer callback mechanism.
  Allows fine-grained SQL access control at runtime.
===============================================================================}
unit ndxsqliteauthorizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Authorization action codes }
  TNDXAuthAction = (
    aaCreateIndex = SQLITE_CREATE_INDEX,         // Index creation
    aaCreateTable = SQLITE_CREATE_TABLE,         // Table creation
    aaCreateTempIndex = SQLITE_CREATE_TEMP_INDEX,// Temp index
    aaCreateTempTable = SQLITE_CREATE_TEMP_TABLE,// Temp table
    aaCreateTempTrigger = SQLITE_CREATE_TEMP_TRIGGER,
    aaCreateTempView = SQLITE_CREATE_TEMP_VIEW,
    aaCreateTrigger = SQLITE_CREATE_TRIGGER,
    aaCreateView = SQLITE_CREATE_VIEW,
    aaDelete = SQLITE_DELETE,
    aaDropIndex = SQLITE_DROP_INDEX,
    aaDropTable = SQLITE_DROP_TABLE,
    aaDropTempIndex = SQLITE_DROP_TEMP_INDEX,
    aaDropTempTable = SQLITE_DROP_TEMP_TABLE,
    aaDropTempTrigger = SQLITE_DROP_TEMP_TRIGGER,
    aaDropTempView = SQLITE_DROP_TEMP_VIEW,
    aaDropTrigger = SQLITE_DROP_TRIGGER,
    aaDropView = SQLITE_DROP_VIEW,
    aaInsert = SQLITE_INSERT,
    aaPragma = SQLITE_PRAGMA,
    aaRead = SQLITE_READ,
    aaSelect = SQLITE_SELECT,
    aaTransaction = SQLITE_TRANSACTION,
    aaUpdate = SQLITE_UPDATE,
    aaAttach = SQLITE_ATTACH,
    aaDetach = SQLITE_DETACH,
    aaAlterTable = SQLITE_ALTER_TABLE,
    aaReindex = SQLITE_REINDEX,
    aaAnalyze = SQLITE_ANALYZE,
    aaCreateVTable = SQLITE_CREATE_VTABLE,
    aaDropVTable = SQLITE_DROP_VTABLE,
    aaFunction = SQLITE_FUNCTION,
    aaSavepoint = SQLITE_SAVEPOINT,
    aaRecursive = SQLITE_RECURSIVE
  );

  { Authorization result }
  TNDXAuthResult = (
    arOK = SQLITE_OK,           // Allow the action
    arDeny = SQLITE_DENY,       // Deny - abort with error
    arIgnore = SQLITE_IGNORE    // Ignore - treat as NULL/no-op
  );

  { Authorization request info }
  TNDXAuthRequest = record
    Action: TNDXAuthAction;
    Param1: string;   // Table name, index name, etc.
    Param2: string;   // Column name, etc.
    Database: string; // Database name (main, temp, attached)
    Trigger: string;  // Trigger name if applicable
  end;

  { Authorization callback event }
  TNDXAuthorizerCallback = function(const Request: TNDXAuthRequest): TNDXAuthResult of object;

  { Authorization rule for simple filtering }
  TNDXAuthRule = record
    Action: TNDXAuthAction;
    TablePattern: string;  // '*' for any, or specific table name
    ColumnPattern: string; // '*' for any, or specific column name
    Result: TNDXAuthResult;
  end;
  TNDXAuthRules = array of TNDXAuthRule;

  { Custom Authorizer Manager }
  TNDXSQLiteAuthorizer = class
  private
    FConnection: INDXSQLiteConnection;
    FCallback: TNDXAuthorizerCallback;
    FRules: TNDXAuthRules;
    FEnabled: Boolean;
    FDenyByDefault: Boolean;
    FLogEnabled: Boolean;
    FLog: TStringList;

    function GetDBHandle: Psqlite3;
    function MatchPattern(const AValue, APattern: string): Boolean;
    function EvaluateRules(const ARequest: TNDXAuthRequest): TNDXAuthResult;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Registers the authorizer callback with the SQLite connection. }
    procedure Enable;
    { Unregisters the authorizer from the SQLite connection, allowing all operations. }
    procedure Disable;

    { Sets a custom callback function to handle authorization decisions. }
    procedure SetCallback(ACallback: TNDXAuthorizerCallback);
    { Removes the custom authorization callback. }
    procedure ClearCallback;

    { Adds a rule that matches the given action and table/column patterns with the specified result. }
    procedure AddRule(AAction: TNDXAuthAction; const ATablePattern, AColumnPattern: string;
      AResult: TNDXAuthResult);
    { Adds a rule that denies the specified action on matching tables and columns. }
    procedure AddDenyRule(AAction: TNDXAuthAction; const ATablePattern: string = '*';
      const AColumnPattern: string = '*');
    { Adds a rule that allows the specified action on matching tables and columns. }
    procedure AddAllowRule(AAction: TNDXAuthAction; const ATablePattern: string = '*';
      const AColumnPattern: string = '*');
    { Adds a rule that silently ignores the specified action on matching tables and columns. }
    procedure AddIgnoreRule(AAction: TNDXAuthAction; const ATablePattern: string = '*';
      const AColumnPattern: string = '*');
    { Removes all authorization rules. }
    procedure ClearRules;

    { Denies all INSERT, UPDATE, and DELETE operations. }
    procedure DenyAllWrites;
    { Denies all access (read and write) to the specified table. }
    procedure DenyTableAccess(const ATableName: string);
    { Denies read access to the specified column in the given table. }
    procedure DenyColumnAccess(const ATableName, AColumnName: string);
    { Configures the authorizer to allow only SELECT and read operations. }
    procedure AllowReadOnly;
    { Denies all DDL operations (CREATE, DROP, ALTER). }
    procedure DenyDDL;
    { Denies ATTACH and DETACH database operations. }
    procedure DenyAttach;

    { Enables logging of all authorization requests to an internal string list. }
    procedure EnableLogging;
    { Disables authorization request logging. }
    procedure DisableLogging;
    { Clears all accumulated authorization log entries. }
    procedure ClearLog;
    { Returns the internal authorization log. Do not free. }
    function GetLog: TStringList;

    { Converts an authorization action code to its human-readable string name. }
    class function ActionToString(AAction: TNDXAuthAction): string;
    { Converts an authorization result code to its string representation (OK, DENY, IGNORE). }
    class function ResultToString(AResult: TNDXAuthResult): string;

    property Connection: INDXSQLiteConnection read FConnection;
    property Enabled: Boolean read FEnabled;
    property DenyByDefault: Boolean read FDenyByDefault write FDenyByDefault;
    property Rules: TNDXAuthRules read FRules;
    property LogEnabled: Boolean read FLogEnabled;
  end;

implementation

var
  GlobalAuthorizer: TNDXSQLiteAuthorizer = nil;

{ Authorizer callback - called by SQLite }
function AuthorizerCallback(pUserData: Pointer; actionCode: Integer;
  zArg1, zArg2, zDb, zTrigger: PAnsiChar): Integer; cdecl;
var
  Auth: TNDXSQLiteAuthorizer;
  Request: TNDXAuthRequest;
  AuthResult: TNDXAuthResult;
  LogEntry: string;
begin
  Auth := TNDXSQLiteAuthorizer(pUserData);
  if Auth = nil then
  begin
    Result := Integer(arOK);
    Exit;
  end;

  // Build request
  Request.Action := TNDXAuthAction(actionCode);
  if zArg1 <> nil then Request.Param1 := string(zArg1) else Request.Param1 := '';
  if zArg2 <> nil then Request.Param2 := string(zArg2) else Request.Param2 := '';
  if zDb <> nil then Request.Database := string(zDb) else Request.Database := '';
  if zTrigger <> nil then Request.Trigger := string(zTrigger) else Request.Trigger := '';

  // Evaluate
  if Assigned(Auth.FCallback) then
    AuthResult := Auth.FCallback(Request)
  else
    AuthResult := Auth.EvaluateRules(Request);

  // Log if enabled
  if Auth.FLogEnabled then
  begin
    LogEntry := Format('[%s] %s: %s.%s -> %s',
      [FormatDateTime('hh:nn:ss.zzz', Now),
       TNDXSQLiteAuthorizer.ActionToString(Request.Action),
       Request.Param1, Request.Param2,
       TNDXSQLiteAuthorizer.ResultToString(AuthResult)]);
    Auth.FLog.Add(LogEntry);
  end;

  Result := Integer(AuthResult);
end;

{ TNDXSQLiteAuthorizer }

constructor TNDXSQLiteAuthorizer.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FCallback := nil;
  SetLength(FRules, 0);
  FEnabled := False;
  FDenyByDefault := False;
  FLogEnabled := False;
  FLog := TStringList.Create;
end;

destructor TNDXSQLiteAuthorizer.Destroy;
begin
  Disable;
  FLog.Free;
  SetLength(FRules, 0);
  inherited Destroy;
end;

function TNDXSQLiteAuthorizer.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

function TNDXSQLiteAuthorizer.MatchPattern(const AValue, APattern: string): Boolean;
begin
  if APattern = '*' then
    Result := True
  else
    Result := SameText(AValue, APattern);
end;

function TNDXSQLiteAuthorizer.EvaluateRules(const ARequest: TNDXAuthRequest): TNDXAuthResult;
var
  I: Integer;
  Rule: TNDXAuthRule;
begin
  // Check rules in order
  for I := 0 to High(FRules) do
  begin
    Rule := FRules[I];
    if (Rule.Action = ARequest.Action) and
       MatchPattern(ARequest.Param1, Rule.TablePattern) and
       MatchPattern(ARequest.Param2, Rule.ColumnPattern) then
    begin
      Result := Rule.Result;
      Exit;
    end;
  end;

  // Default
  if FDenyByDefault then
    Result := arDeny
  else
    Result := arOK;
end;

procedure TNDXSQLiteAuthorizer.Enable;
var
  RC: Integer;
begin
  if FEnabled then Exit;

  if not Assigned(sqlite3_set_authorizer) then
    raise ENDXSQLiteException.Create('sqlite3_set_authorizer is not available');

  RC := sqlite3_set_authorizer(GetDBHandle, @AuthorizerCallback, Self);

  if RC <> SQLITE_OK then
    raise ENDXSQLiteException.CreateFmt('Failed to set authorizer: %d', [RC]);

  FEnabled := True;
  GlobalAuthorizer := Self;
end;

procedure TNDXSQLiteAuthorizer.Disable;
begin
  if not FEnabled then Exit;

  sqlite3_set_authorizer(GetDBHandle, nil, nil);
  FEnabled := False;

  if GlobalAuthorizer = Self then
    GlobalAuthorizer := nil;
end;

procedure TNDXSQLiteAuthorizer.SetCallback(ACallback: TNDXAuthorizerCallback);
begin
  FCallback := ACallback;
end;

procedure TNDXSQLiteAuthorizer.ClearCallback;
begin
  FCallback := nil;
end;

procedure TNDXSQLiteAuthorizer.AddRule(AAction: TNDXAuthAction;
  const ATablePattern, AColumnPattern: string; AResult: TNDXAuthResult);
var
  Idx: Integer;
begin
  Idx := Length(FRules);
  SetLength(FRules, Idx + 1);
  FRules[Idx].Action := AAction;
  FRules[Idx].TablePattern := ATablePattern;
  FRules[Idx].ColumnPattern := AColumnPattern;
  FRules[Idx].Result := AResult;
end;

procedure TNDXSQLiteAuthorizer.AddDenyRule(AAction: TNDXAuthAction;
  const ATablePattern, AColumnPattern: string);
begin
  AddRule(AAction, ATablePattern, AColumnPattern, arDeny);
end;

procedure TNDXSQLiteAuthorizer.AddAllowRule(AAction: TNDXAuthAction;
  const ATablePattern, AColumnPattern: string);
begin
  AddRule(AAction, ATablePattern, AColumnPattern, arOK);
end;

procedure TNDXSQLiteAuthorizer.AddIgnoreRule(AAction: TNDXAuthAction;
  const ATablePattern, AColumnPattern: string);
begin
  AddRule(AAction, ATablePattern, AColumnPattern, arIgnore);
end;

procedure TNDXSQLiteAuthorizer.ClearRules;
begin
  SetLength(FRules, 0);
end;

procedure TNDXSQLiteAuthorizer.DenyAllWrites;
begin
  AddDenyRule(aaInsert);
  AddDenyRule(aaUpdate);
  AddDenyRule(aaDelete);
end;

procedure TNDXSQLiteAuthorizer.DenyTableAccess(const ATableName: string);
begin
  AddDenyRule(aaSelect, ATableName);
  AddDenyRule(aaInsert, ATableName);
  AddDenyRule(aaUpdate, ATableName);
  AddDenyRule(aaDelete, ATableName);
  AddDenyRule(aaRead, ATableName);
end;

procedure TNDXSQLiteAuthorizer.DenyColumnAccess(const ATableName, AColumnName: string);
begin
  AddDenyRule(aaRead, ATableName, AColumnName);
  AddDenyRule(aaUpdate, ATableName, AColumnName);
end;

procedure TNDXSQLiteAuthorizer.AllowReadOnly;
begin
  FDenyByDefault := True;
  AddAllowRule(aaSelect);
  AddAllowRule(aaRead);
  AddAllowRule(aaFunction);
  AddAllowRule(aaTransaction);
end;

procedure TNDXSQLiteAuthorizer.DenyDDL;
begin
  AddDenyRule(aaCreateTable);
  AddDenyRule(aaCreateIndex);
  AddDenyRule(aaCreateTrigger);
  AddDenyRule(aaCreateView);
  AddDenyRule(aaCreateTempTable);
  AddDenyRule(aaCreateTempIndex);
  AddDenyRule(aaCreateTempTrigger);
  AddDenyRule(aaCreateTempView);
  AddDenyRule(aaDropTable);
  AddDenyRule(aaDropIndex);
  AddDenyRule(aaDropTrigger);
  AddDenyRule(aaDropView);
  AddDenyRule(aaDropTempTable);
  AddDenyRule(aaDropTempIndex);
  AddDenyRule(aaDropTempTrigger);
  AddDenyRule(aaDropTempView);
  AddDenyRule(aaAlterTable);
end;

procedure TNDXSQLiteAuthorizer.DenyAttach;
begin
  AddDenyRule(aaAttach);
  AddDenyRule(aaDetach);
end;

procedure TNDXSQLiteAuthorizer.EnableLogging;
begin
  FLogEnabled := True;
end;

procedure TNDXSQLiteAuthorizer.DisableLogging;
begin
  FLogEnabled := False;
end;

procedure TNDXSQLiteAuthorizer.ClearLog;
begin
  FLog.Clear;
end;

function TNDXSQLiteAuthorizer.GetLog: TStringList;
begin
  Result := FLog;
end;

class function TNDXSQLiteAuthorizer.ActionToString(AAction: TNDXAuthAction): string;
begin
  case AAction of
    aaCreateIndex: Result := 'CREATE_INDEX';
    aaCreateTable: Result := 'CREATE_TABLE';
    aaCreateTempIndex: Result := 'CREATE_TEMP_INDEX';
    aaCreateTempTable: Result := 'CREATE_TEMP_TABLE';
    aaCreateTempTrigger: Result := 'CREATE_TEMP_TRIGGER';
    aaCreateTempView: Result := 'CREATE_TEMP_VIEW';
    aaCreateTrigger: Result := 'CREATE_TRIGGER';
    aaCreateView: Result := 'CREATE_VIEW';
    aaDelete: Result := 'DELETE';
    aaDropIndex: Result := 'DROP_INDEX';
    aaDropTable: Result := 'DROP_TABLE';
    aaDropTempIndex: Result := 'DROP_TEMP_INDEX';
    aaDropTempTable: Result := 'DROP_TEMP_TABLE';
    aaDropTempTrigger: Result := 'DROP_TEMP_TRIGGER';
    aaDropTempView: Result := 'DROP_TEMP_VIEW';
    aaDropTrigger: Result := 'DROP_TRIGGER';
    aaDropView: Result := 'DROP_VIEW';
    aaInsert: Result := 'INSERT';
    aaPragma: Result := 'PRAGMA';
    aaRead: Result := 'READ';
    aaSelect: Result := 'SELECT';
    aaTransaction: Result := 'TRANSACTION';
    aaUpdate: Result := 'UPDATE';
    aaAttach: Result := 'ATTACH';
    aaDetach: Result := 'DETACH';
    aaAlterTable: Result := 'ALTER_TABLE';
    aaReindex: Result := 'REINDEX';
    aaAnalyze: Result := 'ANALYZE';
    aaCreateVTable: Result := 'CREATE_VTABLE';
    aaDropVTable: Result := 'DROP_VTABLE';
    aaFunction: Result := 'FUNCTION';
    aaSavepoint: Result := 'SAVEPOINT';
    aaRecursive: Result := 'RECURSIVE';
  else
    Result := Format('UNKNOWN(%d)', [Integer(AAction)]);
  end;
end;

class function TNDXSQLiteAuthorizer.ResultToString(AResult: TNDXAuthResult): string;
begin
  case AResult of
    arOK: Result := 'OK';
    arDeny: Result := 'DENY';
    arIgnore: Result := 'IGNORE';
  else
    Result := Format('UNKNOWN(%d)', [Integer(AResult)]);
  end;
end;

end.
