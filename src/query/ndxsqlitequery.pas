{===============================================================================
  NDXSQLite - Query Component
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (depends on ndxsqlitedataset)

  Specialized query component for executing SQL statements.
  Provides additional convenience methods for common operations.
===============================================================================}
unit ndxsqlitequery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants,
  ndxsqlitedataset, ndxsqlitedatabase, ndxsqlitestatement, ndxsqliteexceptions;

type
  { Query component - specialized for SQL execution }
  TNDXSQLiteQuery = class(TNDXSQLiteDataSet)
  private
    FUniDirectional: Boolean;
    FRequestLive: Boolean;

  public
    constructor Create(AOwner: TComponent); override;

    { Execute methods }

    { Executes the current SQL as a non-query and returns affected row count. }
    function ExecSQL: Integer;
    { Executes the current SQL and returns the first column of the first row. }
    function ExecSQLScalar: Variant;

    { Convenience methods }

    { Closes the dataset and clears the SQL text. }
    procedure SQL_Clear;
    { Appends a line to the SQL text. }
    procedure SQL_Add(const ALine: string);
    { Closes the dataset and replaces the SQL text entirely. }
    procedure SQL_Text(const ASQL: string);

    { First value shortcuts - execute a query and return the scalar result. }

    { Returns the first value as Int64. }
    function GetInt(const ASQL: string): Int64; overload;
    { Returns the first value as Int64 with positional parameters. }
    function GetInt(const ASQL: string; const AParams: array of Variant): Int64; overload;
    { Returns the first value as string. }
    function GetStr(const ASQL: string): string; overload;
    { Returns the first value as string with positional parameters. }
    function GetStr(const ASQL: string; const AParams: array of Variant): string; overload;
    { Returns the first value as Double. }
    function GetFloat(const ASQL: string): Double; overload;
    { Returns the first value as Double with positional parameters. }
    function GetFloat(const ASQL: string; const AParams: array of Variant): Double; overload;
    { Returns the first value as TDateTime. }
    function GetDateTime(const ASQL: string): TDateTime; overload;
    { Returns the first value as TDateTime with positional parameters. }
    function GetDateTime(const ASQL: string; const AParams: array of Variant): TDateTime; overload;
    { Returns the first value as Boolean. }
    function GetBool(const ASQL: string): Boolean; overload;
    { Returns the first value as Boolean with positional parameters. }
    function GetBool(const ASQL: string; const AParams: array of Variant): Boolean; overload;

    { Existence check }

    { Returns True if the query produces at least one row. }
    function Exists(const ASQL: string): Boolean; overload;
    { Returns True if the parameterized query produces at least one row. }
    function Exists(const ASQL: string; const AParams: array of Variant): Boolean; overload;

    { Count shortcut }

    { Returns the row count from the specified table with an optional WHERE clause. }
    function GetCount(const ATableName: string; const AWhereClause: string = ''): Int64;

  published
    property UniDirectional: Boolean read FUniDirectional write FUniDirectional default False;
    property RequestLive: Boolean read FRequestLive write FRequestLive default False;
  end;

  { Table component - specialized for direct table access }
  TNDXSQLiteTable = class(TNDXSQLiteDataSet)
  private
    FIndexFieldNames: string;
    FMasterSource: TDataSource;
    FMasterFields: string;
    FDetailFields: string;

    procedure SetMasterSource(AValue: TDataSource);
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoOnNewRecord; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Table operations }

    { Creates the table using the given column definition SQL. }
    procedure CreateTable(const ATableDef: string);
    { Drops the table from the database. }
    procedure DropTable;
    { Deletes all rows from the table without dropping it. }
    procedure EmptyTable;
    { Renames the table to the specified new name. }
    procedure RenameTable(const ANewName: string);

    { Index operations }

    { Creates an index on the specified fields. Set AUnique for a unique constraint. }
    procedure CreateIndex(const AIndexName, AFields: string; AUnique: Boolean = False);
    { Drops the named index from the database. }
    procedure DropIndex(const AIndexName: string);

    { Open for table }

    { Opens the dataset by generating a SELECT * FROM TableName query. }
    procedure OpenTable;

  published
    property IndexFieldNames: string read FIndexFieldNames write FIndexFieldNames;
    property MasterSource: TDataSource read FMasterSource write SetMasterSource;
    property MasterFields: string read FMasterFields write FMasterFields;
    property DetailFields: string read FDetailFields write FDetailFields;
  end;

  { Batch executor for multiple statements }
  TNDXSQLiteBatchExecutor = class
  private
    FDatabase: TNDXSQLiteDatabase;
    FStatements: TStringList;
    FUseTransaction: Boolean;
    FStopOnError: Boolean;
    FLastError: string;
    FExecutedCount: Integer;

  public
    constructor Create(ADatabase: TNDXSQLiteDatabase);
    destructor Destroy; override;

    { Removes all queued SQL statements. }
    procedure Clear;
    { Adds a SQL statement to the batch. }
    procedure Add(const ASQL: string);
    { Adds a formatted SQL statement to the batch. }
    procedure AddFormat(const ASQL: string; const AArgs: array of const);
    { Executes all queued statements. Returns True if all succeeded. }
    function Execute: Boolean;
    { Splits a multi-statement script by semicolons and executes each. }
    function ExecuteScript(const AScript: string): Boolean;

    property UseTransaction: Boolean read FUseTransaction write FUseTransaction;
    property StopOnError: Boolean read FStopOnError write FStopOnError;
    property LastError: string read FLastError;
    property ExecutedCount: Integer read FExecutedCount;
  end;

procedure Register;

implementation

uses
  ndxsqlitetypes;

procedure Register;
begin
  RegisterComponents('NDXSQLite', [TNDXSQLiteQuery, TNDXSQLiteTable]);
end;

{ TNDXSQLiteQuery }

constructor TNDXSQLiteQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUniDirectional := False;
  FRequestLive := False;
end;

function TNDXSQLiteQuery.ExecSQL: Integer;
begin
  Result := inherited ExecSQL;
end;

function TNDXSQLiteQuery.ExecSQLScalar: Variant;
var
  Stmt: TNDXSQLiteStatement;
  I: Integer;
begin
  Result := Null;
  EnsureDatabaseOpen;

  Stmt := TNDXSQLiteStatement.Create(Database);
  try
    Stmt.Prepare(SQL.Text);

    // Bind parameters
    for I := 0 to Params.Count - 1 do
    begin
      if Params[I].Bound then
        Stmt.BindVariantByName(Params[I].Name, Params[I].Value)
      else if Params[I].IsNull then
        Stmt.BindNullByName(Params[I].Name);
    end;

    Result := Stmt.ExecuteScalar;
  finally
    Stmt.Free;
  end;
end;

procedure TNDXSQLiteQuery.SQL_Clear;
begin
  Close;
  SQL.Clear;
end;

procedure TNDXSQLiteQuery.SQL_Add(const ALine: string);
begin
  SQL.Add(ALine);
end;

procedure TNDXSQLiteQuery.SQL_Text(const ASQL: string);
begin
  Close;
  SQL.Text := ASQL;
end;

function TNDXSQLiteQuery.GetInt(const ASQL: string): Int64;
var
  V: Variant;
begin
  SQL_Text(ASQL);
  V := ExecSQLScalar;
  if VarIsNull(V) then
    Result := 0
  else
    Result := V;
end;

function TNDXSQLiteQuery.GetInt(const ASQL: string;
  const AParams: array of Variant): Int64;
var
  I: Integer;
begin
  SQL_Text(ASQL);
  for I := 0 to High(AParams) do
  begin
    if I < Params.Count then
      Params[I].Value := AParams[I];
  end;
  Result := GetInt(ASQL);
end;

function TNDXSQLiteQuery.GetStr(const ASQL: string): string;
var
  V: Variant;
begin
  SQL_Text(ASQL);
  V := ExecSQLScalar;
  if VarIsNull(V) then
    Result := ''
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteQuery.GetStr(const ASQL: string;
  const AParams: array of Variant): string;
var
  I: Integer;
begin
  SQL_Text(ASQL);
  for I := 0 to High(AParams) do
  begin
    if I < Params.Count then
      Params[I].Value := AParams[I];
  end;
  Result := GetStr(ASQL);
end;

function TNDXSQLiteQuery.GetFloat(const ASQL: string): Double;
var
  V: Variant;
begin
  SQL_Text(ASQL);
  V := ExecSQLScalar;
  if VarIsNull(V) then
    Result := 0.0
  else
    Result := V;
end;

function TNDXSQLiteQuery.GetFloat(const ASQL: string;
  const AParams: array of Variant): Double;
var
  I: Integer;
begin
  SQL_Text(ASQL);
  for I := 0 to High(AParams) do
  begin
    if I < Params.Count then
      Params[I].Value := AParams[I];
  end;
  Result := GetFloat(ASQL);
end;

function TNDXSQLiteQuery.GetDateTime(const ASQL: string): TDateTime;
var
  V: Variant;
begin
  SQL_Text(ASQL);
  V := ExecSQLScalar;
  if VarIsNull(V) then
    Result := 0
  else if VarType(V) = varDate then
    Result := V
  else
    Result := StrToDateTimeDef(VarToStr(V), 0);
end;

function TNDXSQLiteQuery.GetDateTime(const ASQL: string;
  const AParams: array of Variant): TDateTime;
var
  I: Integer;
begin
  SQL_Text(ASQL);
  for I := 0 to High(AParams) do
  begin
    if I < Params.Count then
      Params[I].Value := AParams[I];
  end;
  Result := GetDateTime(ASQL);
end;

function TNDXSQLiteQuery.GetBool(const ASQL: string): Boolean;
var
  V: Variant;
begin
  SQL_Text(ASQL);
  V := ExecSQLScalar;
  if VarIsNull(V) then
    Result := False
  else if VarType(V) = varBoolean then
    Result := V
  else
    Result := V <> 0;
end;

function TNDXSQLiteQuery.GetBool(const ASQL: string;
  const AParams: array of Variant): Boolean;
var
  I: Integer;
begin
  SQL_Text(ASQL);
  for I := 0 to High(AParams) do
  begin
    if I < Params.Count then
      Params[I].Value := AParams[I];
  end;
  Result := GetBool(ASQL);
end;

function TNDXSQLiteQuery.Exists(const ASQL: string): Boolean;
var
  V: Variant;
begin
  SQL_Text('SELECT EXISTS(' + ASQL + ')');
  V := ExecSQLScalar;
  Result := (not VarIsNull(V)) and (V <> 0);
end;

function TNDXSQLiteQuery.Exists(const ASQL: string;
  const AParams: array of Variant): Boolean;
var
  I: Integer;
begin
  SQL_Text('SELECT EXISTS(' + ASQL + ')');
  for I := 0 to High(AParams) do
  begin
    if I < Params.Count then
      Params[I].Value := AParams[I];
  end;
  Result := Exists(ASQL);
end;

function TNDXSQLiteQuery.GetCount(const ATableName: string;
  const AWhereClause: string): Int64;
var
  LSQL: string;
begin
  LSQL := 'SELECT COUNT(*) FROM ' + QuoteIdentifier(ATableName);
  if AWhereClause <> '' then
    LSQL := LSQL + ' WHERE ' + AWhereClause;
  Result := GetInt(LSQL);
end;

{ TNDXSQLiteTable }

constructor TNDXSQLiteTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndexFieldNames := '';
  FMasterSource := nil;
  FMasterFields := '';
  FDetailFields := '';
end;

destructor TNDXSQLiteTable.Destroy;
begin
  MasterSource := nil;  // Unhook
  inherited Destroy;
end;

procedure TNDXSQLiteTable.SetMasterSource(AValue: TDataSource);
begin
  if FMasterSource <> AValue then
  begin
    if FMasterSource <> nil then
    begin
      FMasterSource.RemoveFreeNotification(Self);
      // Unhook events - we'd need to store the original handlers
    end;

    FMasterSource := AValue;

    if FMasterSource <> nil then
    begin
      FMasterSource.FreeNotification(Self);
      // Hook events
    end;
  end;
end;

procedure TNDXSQLiteTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FMasterSource) then
    FMasterSource := nil;
end;

procedure TNDXSQLiteTable.MasterChanged(Sender: TObject);
begin
  // Refresh when master changes
  if Active then
    Refresh;
end;

procedure TNDXSQLiteTable.MasterDisabled(Sender: TObject);
begin
  // Close when master closes
  Close;
end;

procedure TNDXSQLiteTable.DoOnNewRecord;
begin
  inherited DoOnNewRecord;

  // Copy master field values to detail
  if (FMasterSource <> nil) and (FMasterSource.DataSet <> nil) and
     (FMasterFields <> '') and (FDetailFields <> '') then
  begin
    // Parse and copy field values
    // This is a simplified version - a full implementation would handle
    // multiple fields separated by semicolons
  end;
end;

procedure TNDXSQLiteTable.CreateTable(const ATableDef: string);
begin
  EnsureDatabaseOpen;
  Database.Execute('CREATE TABLE IF NOT EXISTS ' +
    QuoteIdentifier(TableName) + ' (' + ATableDef + ')');
end;

procedure TNDXSQLiteTable.DropTable;
begin
  Close;
  EnsureDatabaseOpen;
  Database.Execute('DROP TABLE IF EXISTS ' + QuoteIdentifier(TableName));
end;

procedure TNDXSQLiteTable.EmptyTable;
begin
  EnsureDatabaseOpen;
  Database.Execute('DELETE FROM ' + QuoteIdentifier(TableName));
  if Active then
    Refresh;
end;

procedure TNDXSQLiteTable.RenameTable(const ANewName: string);
var
  OldName: string;
begin
  Close;
  EnsureDatabaseOpen;
  OldName := TableName;
  Database.Execute('ALTER TABLE ' + QuoteIdentifier(OldName) +
    ' RENAME TO ' + QuoteIdentifier(ANewName));
  TableName := ANewName;
end;

procedure TNDXSQLiteTable.CreateIndex(const AIndexName, AFields: string;
  AUnique: Boolean);
var
  LSQL: string;
begin
  EnsureDatabaseOpen;

  LSQL := 'CREATE ';
  if AUnique then
    LSQL := LSQL + 'UNIQUE ';
  LSQL := LSQL + 'INDEX IF NOT EXISTS ' + QuoteIdentifier(AIndexName) +
    ' ON ' + QuoteIdentifier(TableName) + ' (' + AFields + ')';

  Database.Execute(LSQL);
end;

procedure TNDXSQLiteTable.DropIndex(const AIndexName: string);
begin
  EnsureDatabaseOpen;
  Database.Execute('DROP INDEX IF EXISTS ' + QuoteIdentifier(AIndexName));
end;

procedure TNDXSQLiteTable.OpenTable;
var
  LSQL: string;
begin
  if TableName = '' then
    raise ENDXSQLiteException.Create('TableName not specified');

  LSQL := 'SELECT rowid, * FROM ' + QuoteIdentifier(TableName);

  if FIndexFieldNames <> '' then
    LSQL := LSQL + ' ORDER BY ' + FIndexFieldNames;

  SQL.Text := LSQL;
  Open;
end;

{ TNDXSQLiteBatchExecutor }

constructor TNDXSQLiteBatchExecutor.Create(ADatabase: TNDXSQLiteDatabase);
begin
  inherited Create;
  FDatabase := ADatabase;
  FStatements := TStringList.Create;
  FUseTransaction := True;
  FStopOnError := True;
  FLastError := '';
  FExecutedCount := 0;
end;

destructor TNDXSQLiteBatchExecutor.Destroy;
begin
  FreeAndNil(FStatements);
  inherited Destroy;
end;

procedure TNDXSQLiteBatchExecutor.Clear;
begin
  FStatements.Clear;
  FLastError := '';
  FExecutedCount := 0;
end;

procedure TNDXSQLiteBatchExecutor.Add(const ASQL: string);
begin
  if Trim(ASQL) <> '' then
    FStatements.Add(ASQL);
end;

procedure TNDXSQLiteBatchExecutor.AddFormat(const ASQL: string;
  const AArgs: array of const);
begin
  Add(Format(ASQL, AArgs));
end;

function TNDXSQLiteBatchExecutor.Execute: Boolean;
var
  I: Integer;
begin
  Result := True;
  FLastError := '';
  FExecutedCount := 0;

  if FStatements.Count = 0 then
    Exit;

  if FUseTransaction then
    FDatabase.BeginTransaction;

  try
    for I := 0 to FStatements.Count - 1 do
    begin
      try
        FDatabase.Execute(FStatements[I]);
        Inc(FExecutedCount);
      except
        on E: Exception do
        begin
          FLastError := Format('Statement %d failed: %s', [I + 1, E.Message]);
          Result := False;

          if FStopOnError then
          begin
            if FUseTransaction then
              FDatabase.Rollback;
            Exit;
          end;
        end;
      end;
    end;

    if FUseTransaction then
      FDatabase.Commit;
  except
    on E: Exception do
    begin
      FLastError := E.Message;
      if FUseTransaction and FDatabase.InTransaction then
        FDatabase.Rollback;
      Result := False;
    end;
  end;
end;

function TNDXSQLiteBatchExecutor.ExecuteScript(const AScript: string): Boolean;
var
  Lines: TStringList;
  CurrentStmt: string;
  I: Integer;
  Line: string;
  InString: Boolean;
  J: Integer;
begin
  Clear;
  Lines := TStringList.Create;
  try
    Lines.Text := AScript;
    CurrentStmt := '';
    InString := False;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);

      // Skip comments
      if (Length(Line) >= 2) and (Copy(Line, 1, 2) = '--') then
        Continue;

      // Check for string literals
      for J := 1 to Length(Line) do
      begin
        if Line[J] = '''' then
          InString := not InString;
      end;

      CurrentStmt := CurrentStmt + ' ' + Line;

      // Check for statement end
      if (not InString) and (Length(Line) > 0) and (Line[Length(Line)] = ';') then
      begin
        Add(Trim(CurrentStmt));
        CurrentStmt := '';
      end;
    end;

    // Add any remaining statement
    if Trim(CurrentStmt) <> '' then
      Add(Trim(CurrentStmt));

    Result := Execute;
  finally
    Lines.Free;
  end;
end;

end.
