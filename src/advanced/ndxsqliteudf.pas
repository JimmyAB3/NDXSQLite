{===============================================================================
  NDXSQLite - User-Defined Functions (UDF)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for creating custom SQL functions in Pascal.
  Supports scalar functions and aggregate functions.
===============================================================================}
unit ndxsqliteudf;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Variants,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Wrapper around a sqlite3_value pointer for reading function arguments. }
  TNDXSQLiteValue = record
  private
    FValue: Psqlite3_value;
  public
    { Returns True if the argument value is SQL NULL. }
    function IsNull: Boolean;
    { Returns the argument as a 32-bit integer. }
    function AsInteger: Integer;
    { Returns the argument as a 64-bit integer. }
    function AsInt64: Int64;
    { Returns the argument as a double-precision floating-point number. }
    function AsDouble: Double;
    { Returns the argument as a UTF-8 string. }
    function AsString: string;
    { Returns the argument as a byte array (BLOB). }
    function AsBlob: TBytes;
    { Returns the SQLite fundamental type code (SQLITE_INTEGER, SQLITE_FLOAT, etc.). }
    function GetType: Integer;
    { Creates a TNDXSQLiteValue wrapper from a raw sqlite3_value pointer. }
    class function Create(AValue: Psqlite3_value): TNDXSQLiteValue; static;
  end;

  { Wrapper around a sqlite3_context pointer for setting function return values. }
  TNDXSQLiteResult = record
  private
    FContext: Psqlite3_context;
  public
    { Sets the function result to SQL NULL. }
    procedure SetNull;
    { Sets the function result to a 32-bit integer value. }
    procedure SetInteger(AValue: Integer);
    { Sets the function result to a 64-bit integer value. }
    procedure SetInt64(AValue: Int64);
    { Sets the function result to a double-precision floating-point value. }
    procedure SetDouble(AValue: Double);
    { Sets the function result to a UTF-8 string value. }
    procedure SetString(const AValue: string);
    { Sets the function result to a BLOB value. }
    procedure SetBlob(const AValue: TBytes);
    { Signals an error with the given message, causing the SQL statement to abort. }
    procedure SetError(const AMessage: string);
    { Signals an error with the given SQLite error code. }
    procedure SetErrorCode(ACode: Integer);
    { Creates a TNDXSQLiteResult wrapper from a raw sqlite3_context pointer. }
    class function Create(AContext: Psqlite3_context): TNDXSQLiteResult; static;
  end;

  { Scalar function callback type }
  TNDXScalarFunction = procedure(var AResult: TNDXSQLiteResult;
    const AArgs: array of TNDXSQLiteValue) of object;

  { Aggregate step callback type }
  TNDXAggregateStep = procedure(AContext: Pointer;
    const AArgs: array of TNDXSQLiteValue) of object;

  { Aggregate final callback type }
  TNDXAggregateFinal = procedure(var AResult: TNDXSQLiteResult;
    AContext: Pointer) of object;

  { Registered function info }
  TNDXRegisteredFunction = class
  private
    FName: string;
    FArgCount: Integer;
    FIsAggregate: Boolean;
    FScalarFunc: TNDXScalarFunction;
    FAggregateStep: TNDXAggregateStep;
    FAggregateFinal: TNDXAggregateFinal;
    FAggregateContextSize: Integer;
  public
    property Name: string read FName;
    property ArgCount: Integer read FArgCount;
    property IsAggregate: Boolean read FIsAggregate;
  end;

  { User-Defined Functions Manager }
  TNDXSQLiteUDF = class
  private
    FConnection: INDXSQLiteConnection;
    FRegisteredFunctions: TList;

    function GetDBHandle: Psqlite3;
    function FindFunction(const AName: string): TNDXRegisteredFunction;
    procedure ClearFunctions;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Registers a scalar SQL function callable from any SQL statement. }
    procedure RegisterScalarFunction(const AName: string; AArgCount: Integer;
      AFunc: TNDXScalarFunction);

    { Registers an aggregate SQL function with step and finalize callbacks. }
    procedure RegisterAggregateFunction(const AName: string; AArgCount: Integer;
      AStep: TNDXAggregateStep; AFinal: TNDXAggregateFinal;
      AContextSize: Integer = SizeOf(Pointer));

    { Removes a previously registered function by name. }
    procedure UnregisterFunction(const AName: string);

    { Returns True if a function with the given name is currently registered. }
    function IsFunctionRegistered(const AName: string): Boolean;

    { Returns the names of all currently registered functions. Caller must free. }
    function GetRegisteredFunctions: TStringList;

    { Removes all registered functions from the connection. }
    procedure UnregisterAll;

    property Connection: INDXSQLiteConnection read FConnection;
  end;

implementation

{ Global callback dispatcher - needed because SQLite callbacks are cdecl }
type
  PFunctionUserData = ^TFunctionUserData;
  TFunctionUserData = record
    FunctionInfo: TNDXRegisteredFunction;
  end;

procedure ScalarFunctionCallback(ctx: Psqlite3_context;
  argc: Integer; argv: PPsqlite3_value); cdecl;
var
  UserData: PFunctionUserData;
  FuncInfo: TNDXRegisteredFunction;
  Args: array of TNDXSQLiteValue;
  Result: TNDXSQLiteResult;
  I: Integer;
  ArgPtr: PPsqlite3_value;
begin
  UserData := PFunctionUserData(sqlite3_user_data(ctx));
  if not Assigned(UserData) then Exit;

  FuncInfo := UserData^.FunctionInfo;
  if not Assigned(FuncInfo) or not Assigned(FuncInfo.FScalarFunc) then Exit;

  // Build arguments array
  SetLength(Args, argc);
  ArgPtr := argv;
  for I := 0 to argc - 1 do
  begin
    Args[I] := TNDXSQLiteValue.Create(ArgPtr^);
    Inc(ArgPtr);
  end;

  // Call the Pascal function
  Result := TNDXSQLiteResult.Create(ctx);
  try
    FuncInfo.FScalarFunc(Result, Args);
  except
    on E: Exception do
      Result.SetError(E.Message);
  end;
end;

procedure AggregateStepCallback(ctx: Psqlite3_context;
  argc: Integer; argv: PPsqlite3_value); cdecl;
var
  UserData: PFunctionUserData;
  FuncInfo: TNDXRegisteredFunction;
  AggContext: Pointer;
  Args: array of TNDXSQLiteValue;
  I: Integer;
  ArgPtr: PPsqlite3_value;
begin
  UserData := PFunctionUserData(sqlite3_user_data(ctx));
  if not Assigned(UserData) then Exit;

  FuncInfo := UserData^.FunctionInfo;
  if not Assigned(FuncInfo) or not Assigned(FuncInfo.FAggregateStep) then Exit;

  // Get aggregate context
  AggContext := sqlite3_aggregate_context(ctx, FuncInfo.FAggregateContextSize);

  // Build arguments array
  SetLength(Args, argc);
  ArgPtr := argv;
  for I := 0 to argc - 1 do
  begin
    Args[I] := TNDXSQLiteValue.Create(ArgPtr^);
    Inc(ArgPtr);
  end;

  // Call the step function
  try
    FuncInfo.FAggregateStep(AggContext, Args);
  except
    // Ignore errors in step - will be reported in final
  end;
end;

procedure AggregateFinalCallback(ctx: Psqlite3_context); cdecl;
var
  UserData: PFunctionUserData;
  FuncInfo: TNDXRegisteredFunction;
  AggContext: Pointer;
  Result: TNDXSQLiteResult;
begin
  UserData := PFunctionUserData(sqlite3_user_data(ctx));
  if not Assigned(UserData) then Exit;

  FuncInfo := UserData^.FunctionInfo;
  if not Assigned(FuncInfo) or not Assigned(FuncInfo.FAggregateFinal) then Exit;

  // Get aggregate context (don't allocate if not exists)
  AggContext := sqlite3_aggregate_context(ctx, 0);

  Result := TNDXSQLiteResult.Create(ctx);
  try
    FuncInfo.FAggregateFinal(Result, AggContext);
  except
    on E: Exception do
      Result.SetError(E.Message);
  end;
end;

procedure DestroyFunctionCallback(pArg: Pointer); cdecl;
var
  UserData: PFunctionUserData;
begin
  UserData := PFunctionUserData(pArg);
  if Assigned(UserData) then
    Dispose(UserData);
end;

{ TNDXSQLiteValue }

class function TNDXSQLiteValue.Create(AValue: Psqlite3_value): TNDXSQLiteValue;
begin
  Result.FValue := AValue;
end;

function TNDXSQLiteValue.IsNull: Boolean;
begin
  Result := sqlite3_value_type(FValue) = SQLITE_NULL;
end;

function TNDXSQLiteValue.AsInteger: Integer;
begin
  Result := sqlite3_value_int(FValue);
end;

function TNDXSQLiteValue.AsInt64: Int64;
begin
  Result := sqlite3_value_int64(FValue);
end;

function TNDXSQLiteValue.AsDouble: Double;
begin
  Result := sqlite3_value_double(FValue);
end;

function TNDXSQLiteValue.AsString: string;
var
  P: PAnsiChar;
begin
  P := sqlite3_value_text(FValue);
  if P = nil then
    Result := ''
  else
    Result := string(P);
end;

function TNDXSQLiteValue.AsBlob: TBytes;
var
  P: Pointer;
  Len: Integer;
begin
  P := sqlite3_value_blob(FValue);
  Len := sqlite3_value_bytes(FValue);
  SetLength(Result, Len);
  if (Len > 0) and (P <> nil) then
    Move(P^, Result[0], Len);
end;

function TNDXSQLiteValue.GetType: Integer;
begin
  Result := sqlite3_value_type(FValue);
end;

{ TNDXSQLiteResult }

class function TNDXSQLiteResult.Create(AContext: Psqlite3_context): TNDXSQLiteResult;
begin
  Result.FContext := AContext;
end;

procedure TNDXSQLiteResult.SetNull;
begin
  sqlite3_result_null(FContext);
end;

procedure TNDXSQLiteResult.SetInteger(AValue: Integer);
begin
  sqlite3_result_int(FContext, AValue);
end;

procedure TNDXSQLiteResult.SetInt64(AValue: Int64);
begin
  sqlite3_result_int64(FContext, AValue);
end;

procedure TNDXSQLiteResult.SetDouble(AValue: Double);
begin
  sqlite3_result_double(FContext, AValue);
end;

procedure TNDXSQLiteResult.SetString(const AValue: string);
var
  S: AnsiString;
begin
  S := AnsiString(AValue);
  sqlite3_result_text(FContext, PAnsiChar(S), Length(S), SQLITE_TRANSIENT);
end;

procedure TNDXSQLiteResult.SetBlob(const AValue: TBytes);
begin
  if Length(AValue) = 0 then
    sqlite3_result_zeroblob(FContext, 0)
  else
    sqlite3_result_blob(FContext, @AValue[0], Length(AValue), SQLITE_TRANSIENT);
end;

procedure TNDXSQLiteResult.SetError(const AMessage: string);
var
  S: AnsiString;
begin
  S := AnsiString(AMessage);
  sqlite3_result_error(FContext, PAnsiChar(S), Length(S));
end;

procedure TNDXSQLiteResult.SetErrorCode(ACode: Integer);
begin
  sqlite3_result_error_code(FContext, ACode);
end;

{ TNDXSQLiteUDF }

constructor TNDXSQLiteUDF.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FRegisteredFunctions := TList.Create;
end;

destructor TNDXSQLiteUDF.Destroy;
begin
  UnregisterAll;
  FRegisteredFunctions.Free;
  inherited Destroy;
end;

function TNDXSQLiteUDF.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

function TNDXSQLiteUDF.FindFunction(const AName: string): TNDXRegisteredFunction;
var
  I: Integer;
  Func: TNDXRegisteredFunction;
begin
  Result := nil;
  for I := 0 to FRegisteredFunctions.Count - 1 do
  begin
    Func := TNDXRegisteredFunction(FRegisteredFunctions[I]);
    if SameText(Func.FName, AName) then
    begin
      Result := Func;
      Exit;
    end;
  end;
end;

procedure TNDXSQLiteUDF.ClearFunctions;
var
  I: Integer;
begin
  for I := 0 to FRegisteredFunctions.Count - 1 do
    TNDXRegisteredFunction(FRegisteredFunctions[I]).Free;
  FRegisteredFunctions.Clear;
end;

procedure TNDXSQLiteUDF.RegisterScalarFunction(const AName: string;
  AArgCount: Integer; AFunc: TNDXScalarFunction);
var
  FuncInfo: TNDXRegisteredFunction;
  UserData: PFunctionUserData;
  RC: Integer;
  NameAnsi: AnsiString;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to register functions');

  // Check if already registered
  if FindFunction(AName) <> nil then
    raise ENDXSQLiteException.CreateFmt('Function "%s" is already registered', [AName]);

  // Create function info
  FuncInfo := TNDXRegisteredFunction.Create;
  FuncInfo.FName := AName;
  FuncInfo.FArgCount := AArgCount;
  FuncInfo.FIsAggregate := False;
  FuncInfo.FScalarFunc := AFunc;

  // Create user data
  New(UserData);
  UserData^.FunctionInfo := FuncInfo;

  // Register with SQLite
  NameAnsi := AnsiString(AName);
  RC := sqlite3_create_function_v2(
    GetDBHandle,
    PAnsiChar(NameAnsi),
    AArgCount,
    SQLITE_UTF8,
    UserData,
    @ScalarFunctionCallback,
    nil,
    nil,
    @DestroyFunctionCallback
  );

  if RC <> SQLITE_OK then
  begin
    Dispose(UserData);
    FuncInfo.Free;
    raise ENDXSQLiteException.CreateFmt('Failed to register function "%s": %s',
      [AName, string(sqlite3_errmsg(GetDBHandle))]);
  end;

  FRegisteredFunctions.Add(FuncInfo);
end;

procedure TNDXSQLiteUDF.RegisterAggregateFunction(const AName: string;
  AArgCount: Integer; AStep: TNDXAggregateStep; AFinal: TNDXAggregateFinal;
  AContextSize: Integer);
var
  FuncInfo: TNDXRegisteredFunction;
  UserData: PFunctionUserData;
  RC: Integer;
  NameAnsi: AnsiString;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to register functions');

  // Check if already registered
  if FindFunction(AName) <> nil then
    raise ENDXSQLiteException.CreateFmt('Function "%s" is already registered', [AName]);

  // Create function info
  FuncInfo := TNDXRegisteredFunction.Create;
  FuncInfo.FName := AName;
  FuncInfo.FArgCount := AArgCount;
  FuncInfo.FIsAggregate := True;
  FuncInfo.FAggregateStep := AStep;
  FuncInfo.FAggregateFinal := AFinal;
  FuncInfo.FAggregateContextSize := AContextSize;

  // Create user data
  New(UserData);
  UserData^.FunctionInfo := FuncInfo;

  // Register with SQLite
  NameAnsi := AnsiString(AName);
  RC := sqlite3_create_function_v2(
    GetDBHandle,
    PAnsiChar(NameAnsi),
    AArgCount,
    SQLITE_UTF8,
    UserData,
    nil,
    @AggregateStepCallback,
    @AggregateFinalCallback,
    @DestroyFunctionCallback
  );

  if RC <> SQLITE_OK then
  begin
    Dispose(UserData);
    FuncInfo.Free;
    raise ENDXSQLiteException.CreateFmt('Failed to register aggregate function "%s": %s',
      [AName, string(sqlite3_errmsg(GetDBHandle))]);
  end;

  FRegisteredFunctions.Add(FuncInfo);
end;

procedure TNDXSQLiteUDF.UnregisterFunction(const AName: string);
var
  FuncInfo: TNDXRegisteredFunction;
  RC: Integer;
  NameAnsi: AnsiString;
begin
  FuncInfo := FindFunction(AName);
  if FuncInfo = nil then
    Exit;

  // Unregister from SQLite by passing nil callbacks
  NameAnsi := AnsiString(AName);
  RC := sqlite3_create_function_v2(
    GetDBHandle,
    PAnsiChar(NameAnsi),
    FuncInfo.FArgCount,
    SQLITE_UTF8,
    nil,
    nil,
    nil,
    nil,
    nil
  );

  // Remove from list regardless of result
  FRegisteredFunctions.Remove(FuncInfo);
  FuncInfo.Free;
end;

function TNDXSQLiteUDF.IsFunctionRegistered(const AName: string): Boolean;
begin
  Result := FindFunction(AName) <> nil;
end;

function TNDXSQLiteUDF.GetRegisteredFunctions: TStringList;
var
  I: Integer;
  FuncInfo: TNDXRegisteredFunction;
begin
  Result := TStringList.Create;
  for I := 0 to FRegisteredFunctions.Count - 1 do
  begin
    FuncInfo := TNDXRegisteredFunction(FRegisteredFunctions[I]);
    if FuncInfo.FIsAggregate then
      Result.Add(Format('%s (aggregate, %d args)', [FuncInfo.FName, FuncInfo.FArgCount]))
    else
      Result.Add(Format('%s (scalar, %d args)', [FuncInfo.FName, FuncInfo.FArgCount]));
  end;
end;

procedure TNDXSQLiteUDF.UnregisterAll;
var
  I: Integer;
  FuncInfo: TNDXRegisteredFunction;
  NameAnsi: AnsiString;
begin
  // Unregister all from SQLite
  for I := FRegisteredFunctions.Count - 1 downto 0 do
  begin
    FuncInfo := TNDXRegisteredFunction(FRegisteredFunctions[I]);
    if FConnection.IsOpen then
    begin
      NameAnsi := AnsiString(FuncInfo.FName);
      sqlite3_create_function_v2(
        GetDBHandle,
        PAnsiChar(NameAnsi),
        FuncInfo.FArgCount,
        SQLITE_UTF8,
        nil,
        nil,
        nil,
        nil,
        nil
      );
    end;
  end;

  ClearFunctions;
end;

end.
