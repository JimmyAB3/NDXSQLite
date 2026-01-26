{===============================================================================
  NDXSQLite - Prepared Statement Wrapper
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 3 (depends on ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions,
            ndxsqlitedatabase)

  Complete wrapper for sqlite3_stmt prepared statements.
  Provides type-safe parameter binding and column access.
===============================================================================}
unit ndxsqlitestatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions, ndxsqlitedatabase;

type
  { Statement state }
  TNDXStatementState = (
    ssUnprepared,   // Not yet prepared
    ssPrepared,     // Prepared, ready to execute
    ssExecuting,    // Step returned SQLITE_ROW
    ssDone,         // Step returned SQLITE_DONE
    ssError         // Error occurred
  );

  { Column information }
  TNDXColumnInfo = record
    Name: string;
    DeclaredType: string;
    DatabaseName: string;
    TableName: string;
    OriginName: string;
    NotNull: Boolean;
    PrimaryKey: Boolean;
    AutoIncrement: Boolean;
  end;

  { Forward declaration }
  TNDXSQLiteStatement = class;

  { Blob stream for reading BLOB data }
  TNDXSQLiteBlobStream = class(TCustomMemoryStream)
  private
    FData: TBytes;
  public
    constructor Create(const AData: TBytes);
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

  { Prepared statement wrapper }
  TNDXSQLiteStatement = class
  private
    FDatabase: TNDXSQLiteDatabase;
    FHandle: Psqlite3_stmt;
    FSQL: string;
    FState: TNDXStatementState;
    FParamCount: Integer;
    FColumnCount: Integer;
    FColumnInfoCache: array of TNDXColumnInfo;
    FOwnsDatabase: Boolean;

    procedure CheckPrepared;
    procedure CheckHasRow;
    procedure CheckResult(AResult: Integer; const AContext: string = '');
    procedure CacheColumnInfo;
    function GetParamIndex(const AName: string): Integer;

  public
    constructor Create(ADatabase: TNDXSQLiteDatabase; AOwnsDatabase: Boolean = False);
    destructor Destroy; override;

    { Preparation }

    { Compiles the SQL string into a prepared statement. }
    procedure Prepare(const ASQL: string);
    { Releases the prepared statement and resets state to ssUnprepared. }
    procedure Unprepare;
    { Returns True if the statement is currently prepared. }
    function IsPrepared: Boolean;

    { Parameter binding by index (1-based) }

    { Binds NULL to the parameter at the given index. }
    procedure BindNull(AIndex: Integer);
    { Binds an integer value to the parameter at the given index. }
    procedure BindInteger(AIndex: Integer; AValue: Int64);
    { Binds a floating-point value to the parameter at the given index. }
    procedure BindDouble(AIndex: Integer; AValue: Double);
    { Binds a text value to the parameter at the given index. }
    procedure BindText(AIndex: Integer; const AValue: string);
    { Binds a BLOB from a pointer and byte count. }
    procedure BindBlob(AIndex: Integer; AData: Pointer; ASize: Integer); overload;
    { Binds a BLOB from a byte array. }
    procedure BindBlob(AIndex: Integer; const AData: TBytes); overload;
    { Binds a BLOB by reading the entire content of a stream. }
    procedure BindBlob(AIndex: Integer; AStream: TStream); overload;
    { Binds a TDateTime as an ISO 8601 date+time string. }
    procedure BindDateTime(AIndex: Integer; AValue: TDateTime);
    { Binds a TDateTime as an ISO 8601 date-only string. }
    procedure BindDate(AIndex: Integer; AValue: TDateTime);
    { Binds a TDateTime as a time-only string. }
    procedure BindTime(AIndex: Integer; AValue: TDateTime);
    { Binds a boolean as integer 0 or 1. }
    procedure BindBoolean(AIndex: Integer; AValue: Boolean);
    { Binds a Variant, automatically detecting the appropriate SQLite type. }
    procedure BindVariant(AIndex: Integer; AValue: Variant);
    { Binds a zero-filled BLOB of the specified size (for later incremental write). }
    procedure BindZeroBlob(AIndex: Integer; ASize: Integer);
    { Resets all parameter bindings to NULL. }
    procedure ClearBindings;

    { Parameter binding by name }

    { Binds NULL to the named parameter. }
    procedure BindNullByName(const AName: string);
    { Binds an integer value to the named parameter. }
    procedure BindIntegerByName(const AName: string; AValue: Int64);
    { Binds a floating-point value to the named parameter. }
    procedure BindDoubleByName(const AName: string; AValue: Double);
    { Binds a text value to the named parameter. }
    procedure BindTextByName(const AName: string; const AValue: string);
    { Binds a BLOB from a pointer and byte count to the named parameter. }
    procedure BindBlobByName(const AName: string; AData: Pointer; ASize: Integer); overload;
    { Binds a BLOB from a byte array to the named parameter. }
    procedure BindBlobByName(const AName: string; const AData: TBytes); overload;
    { Binds a TDateTime as an ISO 8601 string to the named parameter. }
    procedure BindDateTimeByName(const AName: string; AValue: TDateTime);
    { Binds a boolean as integer 0 or 1 to the named parameter. }
    procedure BindBooleanByName(const AName: string; AValue: Boolean);
    { Binds a Variant to the named parameter, auto-detecting the type. }
    procedure BindVariantByName(const AName: string; AValue: Variant);

    { Execution }

    { Advances to the next result row. Returns True if a row is available. }
    function Step: Boolean;
    { Resets the statement to allow re-execution with new bindings. }
    procedure Reset;
    { Executes the statement as a non-query and returns the number of affected rows. }
    function Execute: Integer;
    { Executes the statement and returns the first column of the first row. }
    function ExecuteScalar: Variant;

    { Column metadata (0-based index) }

    { Returns the number of columns in the result set. }
    function ColumnCount: Integer;
    { Returns the column name at the given index. }
    function ColumnName(AIndex: Integer): string;
    { Returns the SQLite type code for the column value at the given index. }
    function ColumnType(AIndex: Integer): Integer;
    { Returns the declared type string from the CREATE TABLE statement. }
    function ColumnDeclaredType(AIndex: Integer): string;
    { Returns the originating database name for the column. }
    function ColumnDatabaseName(AIndex: Integer): string;
    { Returns the originating table name for the column. }
    function ColumnTableName(AIndex: Integer): string;
    { Returns the originating column name from the schema. }
    function ColumnOriginName(AIndex: Integer): string;
    { Returns full column metadata including nullability and primary key status. }
    function GetColumnInfo(AIndex: Integer): TNDXColumnInfo;
    { Returns the column index for the given name, or -1 if not found. }
    function FindColumnIndex(const AName: string): Integer;

    { Column values (0-based index) }

    { Returns True if the column value is NULL. }
    function ColumnIsNull(AIndex: Integer): Boolean;
    { Returns the column value as a 64-bit integer. }
    function ColumnAsInteger(AIndex: Integer): Int64;
    { Returns the column value as a floating-point number. }
    function ColumnAsDouble(AIndex: Integer): Double;
    { Returns the column value as a string. }
    function ColumnAsString(AIndex: Integer): string;
    { Returns the column value as a byte array. }
    function ColumnAsBlob(AIndex: Integer): TBytes;
    { Returns the column value as a read-only memory stream. }
    function ColumnAsBlobStream(AIndex: Integer): TStream;
    { Returns the column value parsed as a TDateTime from ISO 8601 format. }
    function ColumnAsDateTime(AIndex: Integer): TDateTime;
    { Returns the column value parsed as a date-only TDateTime. }
    function ColumnAsDate(AIndex: Integer): TDateTime;
    { Returns the column value parsed as a time-only TDateTime. }
    function ColumnAsTime(AIndex: Integer): TDateTime;
    { Returns the column value as a boolean (nonzero = True). }
    function ColumnAsBoolean(AIndex: Integer): Boolean;
    { Returns the column value as a Variant matching its SQLite type. }
    function ColumnAsVariant(AIndex: Integer): Variant;

    { Column values by name }

    { Returns True if the named column value is NULL. }
    function ColumnIsNullByName(const AName: string): Boolean;
    { Returns the named column value as a 64-bit integer. }
    function ColumnAsIntegerByName(const AName: string): Int64;
    { Returns the named column value as a floating-point number. }
    function ColumnAsDoubleByName(const AName: string): Double;
    { Returns the named column value as a string. }
    function ColumnAsStringByName(const AName: string): string;
    { Returns the named column value as a byte array. }
    function ColumnAsBlobByName(const AName: string): TBytes;
    { Returns the named column value parsed as a TDateTime. }
    function ColumnAsDateTimeByName(const AName: string): TDateTime;
    { Returns the named column value as a boolean. }
    function ColumnAsBooleanByName(const AName: string): Boolean;
    { Returns the named column value as a Variant. }
    function ColumnAsVariantByName(const AName: string): Variant;

    { Parameter information }

    { Returns the number of bindable parameters in the statement. }
    function ParamCount: Integer;
    { Returns the name of the parameter at the given 1-based index. }
    function ParamName(AIndex: Integer): string;
    { Returns the 1-based index of the named parameter, or 0 if not found. }
    function ParamIndex(const AName: string): Integer;

    { SQL information }

    { Returns the original SQL text used to prepare the statement. }
    function GetSQL: string;
    { Returns the SQL with bound parameter values expanded inline. }
    function GetExpandedSQL: string;
    { Returns True if the statement does not modify the database. }
    function IsReadOnly: Boolean;

    { Properties }
    property Handle: Psqlite3_stmt read FHandle;
    property Database: TNDXSQLiteDatabase read FDatabase;
    property SQL: string read FSQL;
    property State: TNDXStatementState read FState;
  end;

implementation

uses
  DateUtils;

const
  DATE_FORMAT = 'YYYY-MM-DD';
  TIME_FORMAT = 'HH:NN:SS';
  DATETIME_FORMAT = 'YYYY-MM-DD HH:NN:SS';
  DATETIME_FORMAT_MS = 'YYYY-MM-DD HH:NN:SS.ZZZ';

{ TNDXSQLiteBlobStream }

constructor TNDXSQLiteBlobStream.Create(const AData: TBytes);
begin
  inherited Create;
  FData := AData;
  if Length(FData) > 0 then
    SetPointer(@FData[0], Length(FData));
end;

function TNDXSQLiteBlobStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  raise ENDXSQLiteException.Create('Blob stream is read-only');
end;

{ TNDXSQLiteStatement }

constructor TNDXSQLiteStatement.Create(ADatabase: TNDXSQLiteDatabase;
  AOwnsDatabase: Boolean);
begin
  inherited Create;
  FDatabase := ADatabase;
  FOwnsDatabase := AOwnsDatabase;
  FHandle := nil;
  FSQL := '';
  FState := ssUnprepared;
  FParamCount := 0;
  FColumnCount := 0;
  SetLength(FColumnInfoCache, 0);
end;

destructor TNDXSQLiteStatement.Destroy;
begin
  Unprepare;
  if FOwnsDatabase then
    FreeAndNil(FDatabase);
  inherited Destroy;
end;

procedure TNDXSQLiteStatement.CheckPrepared;
begin
  if FState = ssUnprepared then
    raise ENDXSQLiteException.Create('Statement is not prepared');
end;

procedure TNDXSQLiteStatement.CheckHasRow;
begin
  CheckPrepared;
  if FState <> ssExecuting then
    raise ENDXSQLiteException.Create('No row available - call Step first');
end;

procedure TNDXSQLiteStatement.CheckResult(AResult: Integer; const AContext: string);
var
  Msg: string;
begin
  if AResult <> SQLITE_OK then
  begin
    FState := ssError;
    if FDatabase.Handle <> nil then
      Msg := string(sqlite3_errmsg(FDatabase.Handle))
    else
      Msg := string(sqlite3_errstr(AResult));

    if AContext <> '' then
      Msg := AContext + ': ' + Msg;

    raise ENDXSQLiteException.Create(Msg, AResult);
  end;
end;

procedure TNDXSQLiteStatement.CacheColumnInfo;
var
  I: Integer;
  DataType, CollSeq: PAnsiChar;
  NotNull, PK, AutoInc: Integer;
begin
  FColumnCount := sqlite3_column_count(FHandle);
  SetLength(FColumnInfoCache, FColumnCount);

  for I := 0 to FColumnCount - 1 do
  begin
    FColumnInfoCache[I].Name := string(sqlite3_column_name(FHandle, I));
    FColumnInfoCache[I].DeclaredType := string(sqlite3_column_decltype(FHandle, I));
    FColumnInfoCache[I].DatabaseName := string(sqlite3_column_database_name(FHandle, I));
    FColumnInfoCache[I].TableName := string(sqlite3_column_table_name(FHandle, I));
    FColumnInfoCache[I].OriginName := string(sqlite3_column_origin_name(FHandle, I));

    // Get detailed column info if table name is available
    if (FColumnInfoCache[I].TableName <> '') and
       (FColumnInfoCache[I].OriginName <> '') then
    begin
      if sqlite3_table_column_metadata(FDatabase.Handle,
           PAnsiChar(AnsiString(FColumnInfoCache[I].DatabaseName)),
           PAnsiChar(AnsiString(FColumnInfoCache[I].TableName)),
           PAnsiChar(AnsiString(FColumnInfoCache[I].OriginName)),
           DataType, CollSeq, NotNull, PK, AutoInc) = SQLITE_OK then
      begin
        FColumnInfoCache[I].NotNull := NotNull <> 0;
        FColumnInfoCache[I].PrimaryKey := PK <> 0;
        FColumnInfoCache[I].AutoIncrement := AutoInc <> 0;
      end;
    end;
  end;
end;

function TNDXSQLiteStatement.GetParamIndex(const AName: string): Integer;
var
  NameToFind: string;
begin
  CheckPrepared;

  // SQLite expects parameters with prefix (: @ $)
  if (Length(AName) > 0) and (AName[1] in [':', '@', '$']) then
    NameToFind := AName
  else
    NameToFind := ':' + AName;

  Result := sqlite3_bind_parameter_index(FHandle, PAnsiChar(AnsiString(NameToFind)));

  // Try other prefixes if not found
  if Result = 0 then
  begin
    NameToFind := '@' + AName;
    Result := sqlite3_bind_parameter_index(FHandle, PAnsiChar(AnsiString(NameToFind)));
  end;

  if Result = 0 then
  begin
    NameToFind := '$' + AName;
    Result := sqlite3_bind_parameter_index(FHandle, PAnsiChar(AnsiString(NameToFind)));
  end;

  if Result = 0 then
    raise ENDXSQLiteException.Create('Parameter not found: ' + AName);
end;

procedure TNDXSQLiteStatement.Prepare(const ASQL: string);
var
  Res: Integer;
begin
  // Unprepare any existing statement
  if FState <> ssUnprepared then
    Unprepare;

  if not FDatabase.IsOpen then
    raise ENDXSQLiteException.Create('Database is not open');

  Res := sqlite3_prepare_v2(FDatabase.Handle,
    PAnsiChar(AnsiString(ASQL)), -1, FHandle, nil);
  CheckResult(Res, 'Prepare');

  FSQL := ASQL;
  FState := ssPrepared;
  FParamCount := sqlite3_bind_parameter_count(FHandle);

  // Cache column info
  CacheColumnInfo;
end;

procedure TNDXSQLiteStatement.Unprepare;
begin
  if FHandle <> nil then
  begin
    sqlite3_finalize(FHandle);
    FHandle := nil;
  end;
  FSQL := '';
  FState := ssUnprepared;
  FParamCount := 0;
  FColumnCount := 0;
  SetLength(FColumnInfoCache, 0);
end;

function TNDXSQLiteStatement.IsPrepared: Boolean;
begin
  Result := FState <> ssUnprepared;
end;

procedure TNDXSQLiteStatement.BindNull(AIndex: Integer);
begin
  CheckPrepared;
  CheckResult(sqlite3_bind_null(FHandle, AIndex), 'BindNull');
end;

procedure TNDXSQLiteStatement.BindInteger(AIndex: Integer; AValue: Int64);
begin
  CheckPrepared;
  CheckResult(sqlite3_bind_int64(FHandle, AIndex, AValue), 'BindInteger');
end;

procedure TNDXSQLiteStatement.BindDouble(AIndex: Integer; AValue: Double);
begin
  CheckPrepared;
  CheckResult(sqlite3_bind_double(FHandle, AIndex, AValue), 'BindDouble');
end;

procedure TNDXSQLiteStatement.BindText(AIndex: Integer; const AValue: string);
var
  UTF8: AnsiString;
begin
  CheckPrepared;
  UTF8 := AnsiString(AValue);
  CheckResult(sqlite3_bind_text(FHandle, AIndex, PAnsiChar(UTF8),
    Length(UTF8), SQLITE_TRANSIENT), 'BindText');
end;

procedure TNDXSQLiteStatement.BindBlob(AIndex: Integer; AData: Pointer; ASize: Integer);
begin
  CheckPrepared;
  if (AData = nil) or (ASize <= 0) then
    CheckResult(sqlite3_bind_null(FHandle, AIndex), 'BindBlob')
  else
    CheckResult(sqlite3_bind_blob(FHandle, AIndex, AData, ASize, SQLITE_TRANSIENT),
      'BindBlob');
end;

procedure TNDXSQLiteStatement.BindBlob(AIndex: Integer; const AData: TBytes);
begin
  if Length(AData) = 0 then
    // Bind empty blob (not NULL) - LENGTH() will return 0
    BindZeroBlob(AIndex, 0)
  else
    BindBlob(AIndex, @AData[0], Length(AData));
end;

procedure TNDXSQLiteStatement.BindBlob(AIndex: Integer; AStream: TStream);
var
  Data: TBytes;
begin
  if (AStream = nil) or (AStream.Size = 0) then
  begin
    BindNull(AIndex);
    Exit;
  end;

  SetLength(Data, AStream.Size);
  AStream.Position := 0;
  AStream.ReadBuffer(Data[0], AStream.Size);
  BindBlob(AIndex, Data);
end;

procedure TNDXSQLiteStatement.BindDateTime(AIndex: Integer; AValue: TDateTime);
var
  S: string;
begin
  if AValue = 0 then
    BindNull(AIndex)
  else
  begin
    S := FormatDateTime(DATETIME_FORMAT, AValue);
    BindText(AIndex, S);
  end;
end;

procedure TNDXSQLiteStatement.BindDate(AIndex: Integer; AValue: TDateTime);
var
  S: string;
begin
  if AValue = 0 then
    BindNull(AIndex)
  else
  begin
    S := FormatDateTime(DATE_FORMAT, AValue);
    BindText(AIndex, S);
  end;
end;

procedure TNDXSQLiteStatement.BindTime(AIndex: Integer; AValue: TDateTime);
var
  S: string;
begin
  S := FormatDateTime(TIME_FORMAT, AValue);
  BindText(AIndex, S);
end;

procedure TNDXSQLiteStatement.BindBoolean(AIndex: Integer; AValue: Boolean);
begin
  if AValue then
    BindInteger(AIndex, 1)
  else
    BindInteger(AIndex, 0);
end;

procedure TNDXSQLiteStatement.BindVariant(AIndex: Integer; AValue: Variant);
var
  VType: TVarType;
  VBaseType: TVarType;
  BlobData: TBytes;
  I, ArrLow, ArrHigh: Integer;
begin
  VType := VarType(AValue);
  VBaseType := VType and varTypeMask;

  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    BindNull(AIndex)
  else if VarIsArray(AValue) then
  begin
    // Handle array variants - check if byte array for BLOB
    if VBaseType = varByte then
    begin
      // Byte array - use directly as BLOB
      BlobData := AValue;
      BindBlob(AIndex, BlobData);
    end
    else
    begin
      // Other array types - try to convert to byte array
      try
        ArrLow := VarArrayLowBound(AValue, 1);
        ArrHigh := VarArrayHighBound(AValue, 1);
        SetLength(BlobData, ArrHigh - ArrLow + 1);
        for I := ArrLow to ArrHigh do
          BlobData[I - ArrLow] := Byte(AValue[I]);
        BindBlob(AIndex, BlobData);
      except
        // If array conversion fails, convert to string
        BindText(AIndex, VarToStr(AValue));
      end;
    end;
  end
  else case VBaseType of
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
      BindInteger(AIndex, Integer(AValue));
    varInt64, varUInt64:
      BindInteger(AIndex, Int64(AValue));
    varSingle, varDouble, varCurrency:
      BindDouble(AIndex, Double(AValue));
    varDate:
      BindDateTime(AIndex, TDateTime(AValue));
    varBoolean:
      BindBoolean(AIndex, Boolean(AValue));
    varString, varOleStr, varUString:
      BindText(AIndex, VarToStr(AValue));
  else
    // Try to convert to string
    BindText(AIndex, VarToStr(AValue));
  end;
end;

procedure TNDXSQLiteStatement.BindZeroBlob(AIndex: Integer; ASize: Integer);
begin
  CheckPrepared;
  CheckResult(sqlite3_bind_zeroblob(FHandle, AIndex, ASize), 'BindZeroBlob');
end;

procedure TNDXSQLiteStatement.ClearBindings;
begin
  CheckPrepared;
  CheckResult(sqlite3_clear_bindings(FHandle), 'ClearBindings');
end;

procedure TNDXSQLiteStatement.BindNullByName(const AName: string);
begin
  BindNull(GetParamIndex(AName));
end;

procedure TNDXSQLiteStatement.BindIntegerByName(const AName: string; AValue: Int64);
begin
  BindInteger(GetParamIndex(AName), AValue);
end;

procedure TNDXSQLiteStatement.BindDoubleByName(const AName: string; AValue: Double);
begin
  BindDouble(GetParamIndex(AName), AValue);
end;

procedure TNDXSQLiteStatement.BindTextByName(const AName: string; const AValue: string);
begin
  BindText(GetParamIndex(AName), AValue);
end;

procedure TNDXSQLiteStatement.BindBlobByName(const AName: string;
  AData: Pointer; ASize: Integer);
begin
  BindBlob(GetParamIndex(AName), AData, ASize);
end;

procedure TNDXSQLiteStatement.BindBlobByName(const AName: string; const AData: TBytes);
begin
  BindBlob(GetParamIndex(AName), AData);
end;

procedure TNDXSQLiteStatement.BindDateTimeByName(const AName: string; AValue: TDateTime);
begin
  BindDateTime(GetParamIndex(AName), AValue);
end;

procedure TNDXSQLiteStatement.BindBooleanByName(const AName: string; AValue: Boolean);
begin
  BindBoolean(GetParamIndex(AName), AValue);
end;

procedure TNDXSQLiteStatement.BindVariantByName(const AName: string; AValue: Variant);
begin
  BindVariant(GetParamIndex(AName), AValue);
end;

function TNDXSQLiteStatement.Step: Boolean;
var
  Res: Integer;
begin
  CheckPrepared;

  Res := sqlite3_step(FHandle);
  case Res of
    SQLITE_ROW:
      begin
        FState := ssExecuting;
        Result := True;
      end;
    SQLITE_DONE:
      begin
        FState := ssDone;
        Result := False;
      end;
  else
    FState := ssError;
    CheckResult(Res, 'Step');
    Result := False;  // Never reached
  end;
end;

procedure TNDXSQLiteStatement.Reset;
begin
  CheckPrepared;
  sqlite3_reset(FHandle);
  FState := ssPrepared;
end;

function TNDXSQLiteStatement.Execute: Integer;
begin
  CheckPrepared;

  // Execute until done
  while Step do
    ; // Nothing, just consume all rows

  Result := sqlite3_changes(FDatabase.Handle);
  Reset;
end;

function TNDXSQLiteStatement.ExecuteScalar: Variant;
begin
  CheckPrepared;
  Result := Null;

  if Step then
    Result := ColumnAsVariant(0);

  Reset;
end;

function TNDXSQLiteStatement.ColumnCount: Integer;
begin
  CheckPrepared;
  Result := FColumnCount;
end;

function TNDXSQLiteStatement.ColumnName(AIndex: Integer): string;
begin
  CheckPrepared;
  if (AIndex >= 0) and (AIndex < FColumnCount) then
    Result := FColumnInfoCache[AIndex].Name
  else
    raise ENDXSQLiteException.Create('Column index out of range: ' + IntToStr(AIndex));
end;

function TNDXSQLiteStatement.ColumnType(AIndex: Integer): Integer;
begin
  CheckHasRow;
  Result := sqlite3_column_type(FHandle, AIndex);
end;

function TNDXSQLiteStatement.ColumnDeclaredType(AIndex: Integer): string;
begin
  CheckPrepared;
  if (AIndex >= 0) and (AIndex < FColumnCount) then
    Result := FColumnInfoCache[AIndex].DeclaredType
  else
    raise ENDXSQLiteException.Create('Column index out of range: ' + IntToStr(AIndex));
end;

function TNDXSQLiteStatement.ColumnDatabaseName(AIndex: Integer): string;
begin
  CheckPrepared;
  if (AIndex >= 0) and (AIndex < FColumnCount) then
    Result := FColumnInfoCache[AIndex].DatabaseName
  else
    raise ENDXSQLiteException.Create('Column index out of range: ' + IntToStr(AIndex));
end;

function TNDXSQLiteStatement.ColumnTableName(AIndex: Integer): string;
begin
  CheckPrepared;
  if (AIndex >= 0) and (AIndex < FColumnCount) then
    Result := FColumnInfoCache[AIndex].TableName
  else
    raise ENDXSQLiteException.Create('Column index out of range: ' + IntToStr(AIndex));
end;

function TNDXSQLiteStatement.ColumnOriginName(AIndex: Integer): string;
begin
  CheckPrepared;
  if (AIndex >= 0) and (AIndex < FColumnCount) then
    Result := FColumnInfoCache[AIndex].OriginName
  else
    raise ENDXSQLiteException.Create('Column index out of range: ' + IntToStr(AIndex));
end;

function TNDXSQLiteStatement.GetColumnInfo(AIndex: Integer): TNDXColumnInfo;
begin
  CheckPrepared;
  if (AIndex >= 0) and (AIndex < FColumnCount) then
    Result := FColumnInfoCache[AIndex]
  else
    raise ENDXSQLiteException.Create('Column index out of range: ' + IntToStr(AIndex));
end;

function TNDXSQLiteStatement.FindColumnIndex(const AName: string): Integer;
var
  I: Integer;
begin
  CheckPrepared;
  for I := 0 to FColumnCount - 1 do
  begin
    if SameText(FColumnInfoCache[I].Name, AName) then
    begin
      Result := I;
      Exit;
    end;
  end;
  raise ENDXSQLiteException.Create('Column not found: ' + AName);
end;

function TNDXSQLiteStatement.ColumnIsNull(AIndex: Integer): Boolean;
begin
  CheckHasRow;
  Result := sqlite3_column_type(FHandle, AIndex) = SQLITE_NULL;
end;

function TNDXSQLiteStatement.ColumnAsInteger(AIndex: Integer): Int64;
begin
  CheckHasRow;
  if sqlite3_column_type(FHandle, AIndex) = SQLITE_NULL then
    Result := 0
  else
    Result := sqlite3_column_int64(FHandle, AIndex);
end;

function TNDXSQLiteStatement.ColumnAsDouble(AIndex: Integer): Double;
begin
  CheckHasRow;
  if sqlite3_column_type(FHandle, AIndex) = SQLITE_NULL then
    Result := 0.0
  else
    Result := sqlite3_column_double(FHandle, AIndex);
end;

function TNDXSQLiteStatement.ColumnAsString(AIndex: Integer): string;
var
  P: PAnsiChar;
begin
  CheckHasRow;
  if sqlite3_column_type(FHandle, AIndex) = SQLITE_NULL then
    Result := ''
  else
  begin
    P := sqlite3_column_text(FHandle, AIndex);
    if P <> nil then
      Result := string(P)
    else
      Result := '';
  end;
end;

function TNDXSQLiteStatement.ColumnAsBlob(AIndex: Integer): TBytes;
var
  BlobPtr: Pointer;
  BlobSize: Integer;
begin
  CheckHasRow;
  SetLength(Result, 0);

  if sqlite3_column_type(FHandle, AIndex) = SQLITE_NULL then
    Exit;

  BlobSize := sqlite3_column_bytes(FHandle, AIndex);
  if BlobSize <= 0 then
    Exit;

  BlobPtr := sqlite3_column_blob(FHandle, AIndex);
  if BlobPtr = nil then
    Exit;

  SetLength(Result, BlobSize);
  Move(BlobPtr^, Result[0], BlobSize);
end;

function TNDXSQLiteStatement.ColumnAsBlobStream(AIndex: Integer): TStream;
begin
  Result := TNDXSQLiteBlobStream.Create(ColumnAsBlob(AIndex));
end;

function TNDXSQLiteStatement.ColumnAsDateTime(AIndex: Integer): TDateTime;
var
  S: string;
  ColType: Integer;
begin
  CheckHasRow;
  Result := 0;

  ColType := sqlite3_column_type(FHandle, AIndex);
  if ColType = SQLITE_NULL then
    Exit;

  // SQLite can store dates as text, real (Julian day), or integer (Unix timestamp)
  case ColType of
    SQLITE_INTEGER:
      begin
        // Unix timestamp
        Result := UnixToDateTime(sqlite3_column_int64(FHandle, AIndex));
      end;
    SQLITE_FLOAT:
      begin
        // Julian day number
        Result := sqlite3_column_double(FHandle, AIndex) - 2415018.5;
      end;
    SQLITE_TEXT:
      begin
        S := ColumnAsString(AIndex);
        if S <> '' then
        begin
          // Try various formats
          if not TryStrToDateTime(S, Result) then
          begin
            // Try ISO format
            if Length(S) >= 10 then
            begin
              try
                Result := EncodeDate(
                  StrToIntDef(Copy(S, 1, 4), 0),
                  StrToIntDef(Copy(S, 6, 2), 1),
                  StrToIntDef(Copy(S, 9, 2), 1));
                if Length(S) >= 19 then
                  Result := Result + EncodeTime(
                    StrToIntDef(Copy(S, 12, 2), 0),
                    StrToIntDef(Copy(S, 15, 2), 0),
                    StrToIntDef(Copy(S, 18, 2), 0),
                    0);
              except
                Result := 0;
              end;
            end;
          end;
        end;
      end;
  end;
end;

function TNDXSQLiteStatement.ColumnAsDate(AIndex: Integer): TDateTime;
begin
  Result := DateOf(ColumnAsDateTime(AIndex));
end;

function TNDXSQLiteStatement.ColumnAsTime(AIndex: Integer): TDateTime;
begin
  Result := TimeOf(ColumnAsDateTime(AIndex));
end;

function TNDXSQLiteStatement.ColumnAsBoolean(AIndex: Integer): Boolean;
var
  ColType: Integer;
  S: string;
begin
  CheckHasRow;
  Result := False;

  ColType := sqlite3_column_type(FHandle, AIndex);
  case ColType of
    SQLITE_NULL:
      Result := False;
    SQLITE_INTEGER:
      Result := sqlite3_column_int64(FHandle, AIndex) <> 0;
    SQLITE_FLOAT:
      Result := sqlite3_column_double(FHandle, AIndex) <> 0.0;
    SQLITE_TEXT:
      begin
        S := UpperCase(ColumnAsString(AIndex));
        Result := (S = 'TRUE') or (S = 'YES') or (S = '1') or (S = 'T') or (S = 'Y');
      end;
  end;
end;

function TNDXSQLiteStatement.ColumnAsVariant(AIndex: Integer): Variant;
var
  ColType: Integer;
begin
  CheckHasRow;

  ColType := sqlite3_column_type(FHandle, AIndex);
  case ColType of
    SQLITE_NULL:
      Result := Null;
    SQLITE_INTEGER:
      Result := sqlite3_column_int64(FHandle, AIndex);
    SQLITE_FLOAT:
      Result := sqlite3_column_double(FHandle, AIndex);
    SQLITE_TEXT:
      Result := ColumnAsString(AIndex);
    SQLITE_BLOB:
      Result := ColumnAsBlob(AIndex);
  else
    Result := Null;
  end;
end;

function TNDXSQLiteStatement.ColumnIsNullByName(const AName: string): Boolean;
begin
  Result := ColumnIsNull(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ColumnAsIntegerByName(const AName: string): Int64;
begin
  Result := ColumnAsInteger(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ColumnAsDoubleByName(const AName: string): Double;
begin
  Result := ColumnAsDouble(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ColumnAsStringByName(const AName: string): string;
begin
  Result := ColumnAsString(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ColumnAsBlobByName(const AName: string): TBytes;
begin
  Result := ColumnAsBlob(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ColumnAsDateTimeByName(const AName: string): TDateTime;
begin
  Result := ColumnAsDateTime(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ColumnAsBooleanByName(const AName: string): Boolean;
begin
  Result := ColumnAsBoolean(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ColumnAsVariantByName(const AName: string): Variant;
begin
  Result := ColumnAsVariant(FindColumnIndex(AName));
end;

function TNDXSQLiteStatement.ParamCount: Integer;
begin
  CheckPrepared;
  Result := FParamCount;
end;

function TNDXSQLiteStatement.ParamName(AIndex: Integer): string;
begin
  CheckPrepared;
  Result := string(sqlite3_bind_parameter_name(FHandle, AIndex));
end;

function TNDXSQLiteStatement.ParamIndex(const AName: string): Integer;
begin
  Result := GetParamIndex(AName);
end;

function TNDXSQLiteStatement.GetSQL: string;
begin
  if FHandle <> nil then
    Result := string(sqlite3_sql(FHandle))
  else
    Result := FSQL;
end;

function TNDXSQLiteStatement.GetExpandedSQL: string;
var
  P: PAnsiChar;
begin
  Result := '';
  if FHandle <> nil then
  begin
    P := sqlite3_expanded_sql(FHandle);
    if P <> nil then
    begin
      Result := string(P);
      sqlite3_free(P);
    end;
  end;
end;

function TNDXSQLiteStatement.IsReadOnly: Boolean;
begin
  if FHandle <> nil then
    Result := sqlite3_stmt_readonly(FHandle) <> 0
  else
    Result := True;
end;

end.
