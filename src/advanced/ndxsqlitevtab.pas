{===============================================================================
  NDXSQLite - Virtual Table Support
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for SQLite Virtual Table mechanism.
  Allows creating custom table implementations in Pascal.
===============================================================================}
unit ndxsqlitevtab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Forward declarations }
  TNDXVirtualTable = class;
  TNDXVirtualCursor = class;
  TNDXVirtualTableModule = class;

  { Column definition for virtual table }
  TNDXVTColumnDef = record
    Name: string;
    DataType: string;       // TEXT, INTEGER, REAL, BLOB
    NotNull: Boolean;
    PrimaryKey: Boolean;
    Hidden: Boolean;        // Hidden columns for internal use
  end;
  TNDXVTColumnDefs = array of TNDXVTColumnDef;

  { Index constraint for query optimization }
  TNDXVTConstraint = record
    Column: Integer;        // Column index
    Op: Integer;            // SQLITE_INDEX_CONSTRAINT_EQ, etc.
    Usable: Boolean;
    Value: Variant;         // Value for the constraint
  end;
  TNDXVTConstraints = array of TNDXVTConstraint;

  { Index order by }
  TNDXVTOrderBy = record
    Column: Integer;
    Desc: Boolean;
  end;
  TNDXVTOrderBys = array of TNDXVTOrderBy;

  { Query plan info }
  TNDXVTQueryPlan = record
    EstimatedCost: Double;
    EstimatedRows: Int64;
    IdxNum: Integer;
    IdxStr: string;
    OrderByConsumed: Boolean;
    ConstraintsUsed: array of Boolean;
  end;

  { Row data }
  TNDXVTRow = record
    RowId: Int64;
    Values: array of Variant;
  end;
  TNDXVTRows = array of TNDXVTRow;

  { Virtual cursor class - iterates over virtual table data }
  TNDXVirtualCursor = class
  private
    FTable: TNDXVirtualTable;
    FCurrentRow: Integer;
    FRowCount: Integer;
    FData: TNDXVTRows;
    FEof: Boolean;
    FFilterIdxNum: Integer;
    FFilterIdxStr: string;
    FFilterArgs: array of Variant;

  public
    { Creates a new cursor for the given virtual table. }
    constructor Create(ATable: TNDXVirtualTable);
    destructor Destroy; override;

    { Called when filter is applied }
    procedure Filter(AIdxNum: Integer; const AIdxStr: string;
      const AArgs: array of Variant); virtual;

    { Move to next row }
    procedure Next; virtual;

    { Check if at end }
    function Eof: Boolean; virtual;

    { Get current rowid }
    function RowId: Int64; virtual;

    { Get column value }
    function Column(AIndex: Integer): Variant; virtual;

    { Reset cursor }
    procedure Reset; virtual;

    property Table: TNDXVirtualTable read FTable;
    property CurrentRow: Integer read FCurrentRow;
  end;

  { Virtual table class - base class for custom implementations }
  TNDXVirtualTable = class
  private
    FModule: TNDXVirtualTableModule;
    FTableName: string;
    FColumns: TNDXVTColumnDefs;
    FData: TNDXVTRows;
    FNextRowId: Int64;

  protected
    { Override these in descendants }
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    function DoConnect: Boolean; virtual;
    procedure DoDisconnect; virtual;

    { Data access - override for custom data sources }
    function GetRowCount: Integer; virtual;
    function GetRow(AIndex: Integer): TNDXVTRow; virtual;
    function GetColumnValue(ARow, ACol: Integer): Variant; virtual;

    { Query optimization - override for custom indexing }
    function DoBestIndex(const AConstraints: TNDXVTConstraints;
      const AOrderBy: TNDXVTOrderBys): TNDXVTQueryPlan; virtual;

    { Data modification - override if table is writable }
    function DoInsert(ARowId: Int64; const AValues: array of Variant): Int64; virtual;
    function DoUpdate(ARowId: Int64; const AValues: array of Variant): Boolean; virtual;
    function DoDelete(ARowId: Int64): Boolean; virtual;

  public
    constructor Create(AModule: TNDXVirtualTableModule; const ATableName: string);
    destructor Destroy; override;

    { Column management }

    { Adds a column definition to the virtual table schema. }
    procedure AddColumn(const AName, ADataType: string;
      ANotNull: Boolean = False; APrimaryKey: Boolean = False;
      AHidden: Boolean = False);
    { Removes all column definitions from the virtual table. }
    procedure ClearColumns;
    { Returns the number of defined columns. }
    function GetColumnCount: Integer;
    { Returns the column definition at the given index. }
    function GetColumnDef(AIndex: Integer): TNDXVTColumnDef;

    { Create cursor for iteration }
    function CreateCursor: TNDXVirtualCursor; virtual;

    { Generate CREATE TABLE statement }
    function GetCreateStatement: string;

    { In-memory data management (for simple implementations) }

    { Appends a row with the given column values to the in-memory data. }
    procedure AddRow(const AValues: array of Variant);
    { Removes all rows from the in-memory data store. }
    procedure ClearData;
    { Returns the index of the row with the given RowId, or -1 if not found. }
    function FindRow(ARowId: Int64): Integer;

    property Module: TNDXVirtualTableModule read FModule;
    property TableName: string read FTableName;
    property Columns: TNDXVTColumnDefs read FColumns;
    property Data: TNDXVTRows read FData;
    property RowCount: Integer read GetRowCount;
    property ColumnCount: Integer read GetColumnCount;
  end;

  { Virtual table class reference }
  TNDXVirtualTableClass = class of TNDXVirtualTable;

  { Module registration info }
  TNDXVTModuleInfo = record
    Name: string;
    TableClass: TNDXVirtualTableClass;
    ReadOnly: Boolean;
  end;

  { Virtual Table Module Manager }
  TNDXVirtualTableModule = class
  private
    FConnection: INDXSQLiteConnection;
    FModules: array of TNDXVTModuleInfo;
    FTables: TList;

    function GetDBHandle: Psqlite3;
    function FindModule(const AName: string): Integer;
    procedure CleanupTables;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Register a virtual table module }
    procedure RegisterModule(const AModuleName: string;
      ATableClass: TNDXVirtualTableClass; AReadOnly: Boolean = True);

    { Unregister a module }
    procedure UnregisterModule(const AModuleName: string);

    { Check if module is registered }
    function IsModuleRegistered(const AModuleName: string): Boolean;

    { Create a virtual table instance }
    function CreateVirtualTable(const ATableName, AModuleName: string;
      const AArgs: array of string): TNDXVirtualTable;

    { Drop a virtual table }
    procedure DropVirtualTable(const ATableName: string);

    { Get list of registered modules }
    function GetRegisteredModules: TStringList;

    { Get list of virtual tables }
    function GetVirtualTables: TStringList;

    property Connection: INDXSQLiteConnection read FConnection;
  end;

  { Pre-built virtual table: Series generator (similar to generate_series) }
  TNDXSeriesVirtualTable = class(TNDXVirtualTable)
  private
    FStart: Int64;
    FStop: Int64;
    FStep: Int64;

  protected
    function GetRowCount: Integer; override;
    function GetRow(AIndex: Integer): TNDXVTRow; override;
    function DoBestIndex(const AConstraints: TNDXVTConstraints;
      const AOrderBy: TNDXVTOrderBys): TNDXVTQueryPlan; override;

  public
    constructor Create(AModule: TNDXVirtualTableModule; const ATableName: string);

    { Configures the integer series range from AStart to AStop with the given step. }
    procedure SetRange(AStart, AStop: Int64; AStep: Int64 = 1);

    property Start: Int64 read FStart write FStart;
    property Stop: Int64 read FStop write FStop;
    property Step: Int64 read FStep write FStep;
  end;

  { Pre-built virtual table: CSV file reader }
  TNDXCSVVirtualTable = class(TNDXVirtualTable)
  private
    FFilePath: string;
    FDelimiter: Char;
    FHasHeader: Boolean;
    FLoaded: Boolean;

    procedure LoadCSV;

  protected
    procedure DoCreate; override;

  public
    constructor Create(AModule: TNDXVirtualTableModule; const ATableName: string);

    { Sets the CSV file path, delimiter character, and whether the first row is a header. }
    procedure SetFile(const AFilePath: string; ADelimiter: Char = ',';
      AHasHeader: Boolean = True);

    property FilePath: string read FFilePath;
    property Delimiter: Char read FDelimiter;
    property HasHeader: Boolean read FHasHeader;
  end;

  { Pre-built virtual table: In-memory key-value store }
  TNDXKeyValueVirtualTable = class(TNDXVirtualTable)
  private
    FKeys: TStringList;
    FValues: TStringList;

  protected
    function GetRowCount: Integer; override;
    function GetRow(AIndex: Integer): TNDXVTRow; override;
    function GetColumnValue(ARow, ACol: Integer): Variant; override;
    function DoInsert(ARowId: Int64; const AValues: array of Variant): Int64; override;
    function DoUpdate(ARowId: Int64; const AValues: array of Variant): Boolean; override;
    function DoDelete(ARowId: Int64): Boolean; override;

  public
    constructor Create(AModule: TNDXVirtualTableModule; const ATableName: string);
    destructor Destroy; override;

    { Stores a value associated with the given key, overwriting any existing value. }
    procedure SetValue(const AKey: string; const AValue: Variant);
    { Returns the value for the given key, or Null if the key does not exist. }
    function GetValue(const AKey: string): Variant;
    { Returns True if the given key exists in the store. }
    function HasKey(const AKey: string): Boolean;
    { Removes the entry with the given key from the store. }
    procedure DeleteKey(const AKey: string);
    { Removes all key-value entries from the store. }
    procedure Clear;

    property Keys: TStringList read FKeys;
  end;

implementation

{ TNDXVirtualCursor }

constructor TNDXVirtualCursor.Create(ATable: TNDXVirtualTable);
begin
  inherited Create;
  FTable := ATable;
  FCurrentRow := -1;
  FRowCount := 0;
  FEof := True;
  SetLength(FData, 0);
  SetLength(FFilterArgs, 0);
end;

destructor TNDXVirtualCursor.Destroy;
begin
  SetLength(FData, 0);
  SetLength(FFilterArgs, 0);
  inherited Destroy;
end;

procedure TNDXVirtualCursor.Filter(AIdxNum: Integer; const AIdxStr: string;
  const AArgs: array of Variant);
var
  I: Integer;
begin
  FFilterIdxNum := AIdxNum;
  FFilterIdxStr := AIdxStr;

  // Copy filter arguments
  SetLength(FFilterArgs, Length(AArgs));
  for I := Low(AArgs) to High(AArgs) do
    FFilterArgs[I] := AArgs[I];

  // Reset cursor
  Reset;

  // Get data from table
  FRowCount := FTable.GetRowCount;
  FEof := FRowCount = 0;
  FCurrentRow := 0;
end;

procedure TNDXVirtualCursor.Next;
begin
  Inc(FCurrentRow);
  FEof := FCurrentRow >= FRowCount;
end;

function TNDXVirtualCursor.Eof: Boolean;
begin
  Result := FEof;
end;

function TNDXVirtualCursor.RowId: Int64;
var
  Row: TNDXVTRow;
begin
  if FEof then
    Result := 0
  else
  begin
    Row := FTable.GetRow(FCurrentRow);
    Result := Row.RowId;
  end;
end;

function TNDXVirtualCursor.Column(AIndex: Integer): Variant;
begin
  if FEof then
    Result := Null
  else
    Result := FTable.GetColumnValue(FCurrentRow, AIndex);
end;

procedure TNDXVirtualCursor.Reset;
begin
  FCurrentRow := 0;
  FEof := False;
end;

{ TNDXVirtualTable }

constructor TNDXVirtualTable.Create(AModule: TNDXVirtualTableModule;
  const ATableName: string);
begin
  inherited Create;
  FModule := AModule;
  FTableName := ATableName;
  SetLength(FColumns, 0);
  SetLength(FData, 0);
  FNextRowId := 1;
  DoCreate;
end;

destructor TNDXVirtualTable.Destroy;
begin
  DoDestroy;
  SetLength(FColumns, 0);
  SetLength(FData, 0);
  inherited Destroy;
end;

procedure TNDXVirtualTable.DoCreate;
begin
  // Override in descendants
end;

procedure TNDXVirtualTable.DoDestroy;
begin
  // Override in descendants
end;

function TNDXVirtualTable.DoConnect: Boolean;
begin
  Result := True;
end;

procedure TNDXVirtualTable.DoDisconnect;
begin
  // Override in descendants
end;

function TNDXVirtualTable.GetRowCount: Integer;
begin
  Result := Length(FData);
end;

function TNDXVirtualTable.GetRow(AIndex: Integer): TNDXVTRow;
begin
  if (AIndex >= 0) and (AIndex < Length(FData)) then
    Result := FData[AIndex]
  else
  begin
    Result.RowId := 0;
    SetLength(Result.Values, 0);
  end;
end;

function TNDXVirtualTable.GetColumnValue(ARow, ACol: Integer): Variant;
begin
  if (ARow >= 0) and (ARow < Length(FData)) and
     (ACol >= 0) and (ACol < Length(FData[ARow].Values)) then
    Result := FData[ARow].Values[ACol]
  else
    Result := Null;
end;

function TNDXVirtualTable.DoBestIndex(const AConstraints: TNDXVTConstraints;
  const AOrderBy: TNDXVTOrderBys): TNDXVTQueryPlan;
var
  I: Integer;
begin
  // Default implementation: full table scan
  Result.EstimatedCost := GetRowCount;
  Result.EstimatedRows := GetRowCount;
  Result.IdxNum := 0;
  Result.IdxStr := '';
  Result.OrderByConsumed := False;

  SetLength(Result.ConstraintsUsed, Length(AConstraints));
  for I := 0 to High(AConstraints) do
    Result.ConstraintsUsed[I] := False;
end;

function TNDXVirtualTable.DoInsert(ARowId: Int64;
  const AValues: array of Variant): Int64;
var
  Idx, I: Integer;
begin
  Idx := Length(FData);
  SetLength(FData, Idx + 1);

  if ARowId <= 0 then
  begin
    ARowId := FNextRowId;
    Inc(FNextRowId);
  end
  else if ARowId >= FNextRowId then
    FNextRowId := ARowId + 1;

  FData[Idx].RowId := ARowId;
  SetLength(FData[Idx].Values, Length(AValues));
  for I := Low(AValues) to High(AValues) do
    FData[Idx].Values[I] := AValues[I];

  Result := ARowId;
end;

function TNDXVirtualTable.DoUpdate(ARowId: Int64;
  const AValues: array of Variant): Boolean;
var
  Idx, I: Integer;
begin
  Idx := FindRow(ARowId);
  if Idx < 0 then
  begin
    Result := False;
    Exit;
  end;

  SetLength(FData[Idx].Values, Length(AValues));
  for I := Low(AValues) to High(AValues) do
    FData[Idx].Values[I] := AValues[I];

  Result := True;
end;

function TNDXVirtualTable.DoDelete(ARowId: Int64): Boolean;
var
  Idx, I: Integer;
begin
  Idx := FindRow(ARowId);
  if Idx < 0 then
  begin
    Result := False;
    Exit;
  end;

  // Shift remaining rows
  for I := Idx to High(FData) - 1 do
    FData[I] := FData[I + 1];
  SetLength(FData, Length(FData) - 1);

  Result := True;
end;

procedure TNDXVirtualTable.AddColumn(const AName, ADataType: string;
  ANotNull, APrimaryKey, AHidden: Boolean);
var
  Idx: Integer;
begin
  Idx := Length(FColumns);
  SetLength(FColumns, Idx + 1);
  FColumns[Idx].Name := AName;
  FColumns[Idx].DataType := ADataType;
  FColumns[Idx].NotNull := ANotNull;
  FColumns[Idx].PrimaryKey := APrimaryKey;
  FColumns[Idx].Hidden := AHidden;
end;

procedure TNDXVirtualTable.ClearColumns;
begin
  SetLength(FColumns, 0);
end;

function TNDXVirtualTable.GetColumnCount: Integer;
begin
  Result := Length(FColumns);
end;

function TNDXVirtualTable.GetColumnDef(AIndex: Integer): TNDXVTColumnDef;
begin
  if (AIndex >= 0) and (AIndex < Length(FColumns)) then
    Result := FColumns[AIndex]
  else
  begin
    Result.Name := '';
    Result.DataType := '';
    Result.NotNull := False;
    Result.PrimaryKey := False;
    Result.Hidden := False;
  end;
end;

function TNDXVirtualTable.CreateCursor: TNDXVirtualCursor;
begin
  Result := TNDXVirtualCursor.Create(Self);
end;

function TNDXVirtualTable.GetCreateStatement: string;
var
  I: Integer;
  ColDef: string;
begin
  Result := 'CREATE TABLE ' + FTableName + ' (';

  for I := 0 to High(FColumns) do
  begin
    if I > 0 then
      Result := Result + ', ';

    ColDef := FColumns[I].Name + ' ' + FColumns[I].DataType;

    if FColumns[I].PrimaryKey then
      ColDef := ColDef + ' PRIMARY KEY';
    if FColumns[I].NotNull then
      ColDef := ColDef + ' NOT NULL';
    if FColumns[I].Hidden then
      ColDef := ColDef + ' HIDDEN';

    Result := Result + ColDef;
  end;

  Result := Result + ')';
end;

procedure TNDXVirtualTable.AddRow(const AValues: array of Variant);
begin
  DoInsert(0, AValues);
end;

procedure TNDXVirtualTable.ClearData;
begin
  SetLength(FData, 0);
  FNextRowId := 1;
end;

function TNDXVirtualTable.FindRow(ARowId: Int64): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FData) do
  begin
    if FData[I].RowId = ARowId then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

{ TNDXVirtualTableModule }

constructor TNDXVirtualTableModule.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  SetLength(FModules, 0);
  FTables := TList.Create;
end;

destructor TNDXVirtualTableModule.Destroy;
begin
  CleanupTables;
  FTables.Free;
  SetLength(FModules, 0);
  inherited Destroy;
end;

function TNDXVirtualTableModule.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

function TNDXVirtualTableModule.FindModule(const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FModules) do
  begin
    if SameText(FModules[I].Name, AName) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TNDXVirtualTableModule.CleanupTables;
var
  I: Integer;
begin
  for I := 0 to FTables.Count - 1 do
    TNDXVirtualTable(FTables[I]).Free;
  FTables.Clear;
end;

procedure TNDXVirtualTableModule.RegisterModule(const AModuleName: string;
  ATableClass: TNDXVirtualTableClass; AReadOnly: Boolean);
var
  Idx: Integer;
begin
  if FindModule(AModuleName) >= 0 then
    raise ENDXSQLiteException.CreateFmt('Module "%s" is already registered', [AModuleName]);

  Idx := Length(FModules);
  SetLength(FModules, Idx + 1);
  FModules[Idx].Name := AModuleName;
  FModules[Idx].TableClass := ATableClass;
  FModules[Idx].ReadOnly := AReadOnly;
end;

procedure TNDXVirtualTableModule.UnregisterModule(const AModuleName: string);
var
  Idx, I: Integer;
begin
  Idx := FindModule(AModuleName);
  if Idx < 0 then
    Exit;

  // Remove module
  for I := Idx to High(FModules) - 1 do
    FModules[I] := FModules[I + 1];
  SetLength(FModules, Length(FModules) - 1);
end;

function TNDXVirtualTableModule.IsModuleRegistered(const AModuleName: string): Boolean;
begin
  Result := FindModule(AModuleName) >= 0;
end;

function TNDXVirtualTableModule.CreateVirtualTable(const ATableName,
  AModuleName: string; const AArgs: array of string): TNDXVirtualTable;
var
  Idx: Integer;
  Table: TNDXVirtualTable;
  SQL: string;
  I: Integer;
begin
  Idx := FindModule(AModuleName);
  if Idx < 0 then
    raise ENDXSQLiteException.CreateFmt('Module "%s" is not registered', [AModuleName]);

  // Create table instance
  Table := FModules[Idx].TableClass.Create(Self, ATableName);
  FTables.Add(Table);

  // For eponymous-only modules, we need to create the table via SQL
  // For now, we simulate virtual table behavior with regular tables
  // and the Pascal objects manage the data

  // Build CREATE TABLE SQL from table definition
  if Table.GetColumnCount > 0 then
  begin
    SQL := Table.GetCreateStatement;
    try
      FConnection.ExecuteNonQuery(SQL);
    except
      // Table might already exist
    end;
  end;

  Result := Table;
end;

procedure TNDXVirtualTableModule.DropVirtualTable(const ATableName: string);
var
  I: Integer;
  Table: TNDXVirtualTable;
begin
  // Find and remove table
  for I := FTables.Count - 1 downto 0 do
  begin
    Table := TNDXVirtualTable(FTables[I]);
    if SameText(Table.TableName, ATableName) then
    begin
      FTables.Delete(I);
      Table.Free;
      Break;
    end;
  end;

  // Drop from database
  try
    FConnection.ExecuteNonQuery(Format('DROP TABLE IF EXISTS %s', [ATableName]));
  except
    // Ignore errors
  end;
end;

function TNDXVirtualTableModule.GetRegisteredModules: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to High(FModules) do
    Result.Add(FModules[I].Name);
end;

function TNDXVirtualTableModule.GetVirtualTables: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to FTables.Count - 1 do
    Result.Add(TNDXVirtualTable(FTables[I]).TableName);
end;

{ TNDXSeriesVirtualTable }

constructor TNDXSeriesVirtualTable.Create(AModule: TNDXVirtualTableModule;
  const ATableName: string);
begin
  inherited Create(AModule, ATableName);
  FStart := 0;
  FStop := 0;
  FStep := 1;

  AddColumn('value', 'INTEGER', False, True);
  AddColumn('start', 'INTEGER HIDDEN', False, False, True);
  AddColumn('stop', 'INTEGER HIDDEN', False, False, True);
  AddColumn('step', 'INTEGER HIDDEN', False, False, True);
end;

procedure TNDXSeriesVirtualTable.SetRange(AStart, AStop: Int64; AStep: Int64);
begin
  FStart := AStart;
  FStop := AStop;
  if AStep = 0 then
    FStep := 1
  else
    FStep := AStep;

  // Populate data
  ClearData;
  if FStep > 0 then
  begin
    while AStart <= AStop do
    begin
      AddRow([AStart]);
      Inc(AStart, FStep);
    end;
  end
  else
  begin
    while AStart >= AStop do
    begin
      AddRow([AStart]);
      Inc(AStart, FStep);
    end;
  end;
end;

function TNDXSeriesVirtualTable.GetRowCount: Integer;
begin
  if FStep = 0 then
    Result := 0
  else if FStep > 0 then
    Result := ((FStop - FStart) div FStep) + 1
  else
    Result := ((FStart - FStop) div Abs(FStep)) + 1;

  if Result < 0 then
    Result := 0;
end;

function TNDXSeriesVirtualTable.GetRow(AIndex: Integer): TNDXVTRow;
begin
  Result.RowId := AIndex + 1;
  SetLength(Result.Values, 1);
  Result.Values[0] := FStart + (Int64(AIndex) * FStep);
end;

function TNDXSeriesVirtualTable.DoBestIndex(const AConstraints: TNDXVTConstraints;
  const AOrderBy: TNDXVTOrderBys): TNDXVTQueryPlan;
begin
  Result := inherited DoBestIndex(AConstraints, AOrderBy);
  // Series can provide exact row count
  Result.EstimatedRows := GetRowCount;
  Result.EstimatedCost := GetRowCount;
end;

{ TNDXCSVVirtualTable }

constructor TNDXCSVVirtualTable.Create(AModule: TNDXVirtualTableModule;
  const ATableName: string);
begin
  inherited Create(AModule, ATableName);
  FFilePath := '';
  FDelimiter := ',';
  FHasHeader := True;
  FLoaded := False;
end;

procedure TNDXCSVVirtualTable.DoCreate;
begin
  inherited DoCreate;
end;

procedure TNDXCSVVirtualTable.SetFile(const AFilePath: string;
  ADelimiter: Char; AHasHeader: Boolean);
begin
  FFilePath := AFilePath;
  FDelimiter := ADelimiter;
  FHasHeader := AHasHeader;
  FLoaded := False;
  LoadCSV;
end;

procedure TNDXCSVVirtualTable.LoadCSV;
var
  Lines: TStringList;
  I, J, ColCount: Integer;
  Fields: TStringList;
  Line: string;
  Values: array of Variant;
begin
  if FFilePath = '' then
    Exit;
  if not FileExists(FFilePath) then
    raise ENDXSQLiteException.CreateFmt('CSV file not found: %s', [FFilePath]);

  ClearColumns;
  ClearData;

  Lines := TStringList.Create;
  Fields := TStringList.Create;
  try
    Lines.LoadFromFile(FFilePath);
    if Lines.Count = 0 then
      Exit;

    // Parse first line for headers or column count
    Fields.Delimiter := FDelimiter;
    Fields.StrictDelimiter := True;
    Fields.DelimitedText := Lines[0];
    ColCount := Fields.Count;

    // Create columns
    if FHasHeader then
    begin
      for J := 0 to ColCount - 1 do
        AddColumn(Fields[J], 'TEXT');
    end
    else
    begin
      for J := 0 to ColCount - 1 do
        AddColumn(Format('col%d', [J + 1]), 'TEXT');
    end;

    // Load data
    for I := Ord(FHasHeader) to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Line = '' then
        Continue;

      Fields.DelimitedText := Line;

      SetLength(Values, ColCount);
      for J := 0 to ColCount - 1 do
      begin
        if J < Fields.Count then
          Values[J] := Fields[J]
        else
          Values[J] := '';
      end;

      AddRow(Values);
    end;

    FLoaded := True;
  finally
    Fields.Free;
    Lines.Free;
  end;
end;

{ TNDXKeyValueVirtualTable }

constructor TNDXKeyValueVirtualTable.Create(AModule: TNDXVirtualTableModule;
  const ATableName: string);
begin
  inherited Create(AModule, ATableName);
  FKeys := TStringList.Create;
  // Don't use sorted - we'll manage key lookup manually to keep indices aligned with FValues
  FValues := TStringList.Create;

  AddColumn('key', 'TEXT', True, True);
  AddColumn('value', 'TEXT');
end;

destructor TNDXKeyValueVirtualTable.Destroy;
begin
  FValues.Free;
  FKeys.Free;
  inherited Destroy;
end;

function TNDXKeyValueVirtualTable.GetRowCount: Integer;
begin
  Result := FKeys.Count;
end;

function TNDXKeyValueVirtualTable.GetRow(AIndex: Integer): TNDXVTRow;
begin
  Result.RowId := AIndex + 1;
  SetLength(Result.Values, 2);
  if (AIndex >= 0) and (AIndex < FKeys.Count) then
  begin
    Result.Values[0] := FKeys[AIndex];
    Result.Values[1] := FValues[AIndex];
  end
  else
  begin
    Result.Values[0] := Null;
    Result.Values[1] := Null;
  end;
end;

function TNDXKeyValueVirtualTable.GetColumnValue(ARow, ACol: Integer): Variant;
begin
  if (ARow >= 0) and (ARow < FKeys.Count) then
  begin
    case ACol of
      0: Result := FKeys[ARow];
      1: Result := FValues[ARow];
    else
      Result := Null;
    end;
  end
  else
    Result := Null;
end;

function TNDXKeyValueVirtualTable.DoInsert(ARowId: Int64;
  const AValues: array of Variant): Int64;
begin
  if Length(AValues) >= 2 then
    SetValue(VarToStr(AValues[0]), AValues[1]);
  Result := FKeys.IndexOf(VarToStr(AValues[0])) + 1;
end;

function TNDXKeyValueVirtualTable.DoUpdate(ARowId: Int64;
  const AValues: array of Variant): Boolean;
begin
  if Length(AValues) >= 2 then
    SetValue(VarToStr(AValues[0]), AValues[1]);
  Result := True;
end;

function TNDXKeyValueVirtualTable.DoDelete(ARowId: Int64): Boolean;
var
  Idx: Integer;
begin
  Idx := ARowId - 1;
  if (Idx >= 0) and (Idx < FKeys.Count) then
  begin
    FKeys.Delete(Idx);
    FValues.Delete(Idx);
    Result := True;
  end
  else
    Result := False;
end;

procedure TNDXKeyValueVirtualTable.SetValue(const AKey: string; const AValue: Variant);
var
  Idx: Integer;
begin
  Idx := FKeys.IndexOf(AKey);
  if Idx >= 0 then
  begin
    FValues[Idx] := VarToStr(AValue);
  end
  else
  begin
    FKeys.Add(AKey);
    FValues.Add(VarToStr(AValue));
  end;
end;

function TNDXKeyValueVirtualTable.GetValue(const AKey: string): Variant;
var
  Idx: Integer;
begin
  Idx := FKeys.IndexOf(AKey);
  if Idx >= 0 then
    Result := FValues[Idx]
  else
    Result := Null;
end;

function TNDXKeyValueVirtualTable.HasKey(const AKey: string): Boolean;
begin
  Result := FKeys.IndexOf(AKey) >= 0;
end;

procedure TNDXKeyValueVirtualTable.DeleteKey(const AKey: string);
var
  Idx: Integer;
begin
  Idx := FKeys.IndexOf(AKey);
  if Idx >= 0 then
  begin
    FKeys.Delete(Idx);
    FValues.Delete(Idx);
  end;
end;

procedure TNDXKeyValueVirtualTable.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  ClearData;
end;

end.
