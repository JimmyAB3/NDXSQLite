{===============================================================================
  NDXSQLite - R-Tree Spatial Index Support
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for SQLite R-Tree extension for spatial indexing.
  Provides efficient queries for rectangles, bounding boxes, and ranges.
===============================================================================}
unit ndxsqliteertree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Bounding box for 2D spatial queries }
  TNDXBoundingBox2D = record
    MinX, MaxX: Double;
    MinY, MaxY: Double;
  end;

  { Bounding box for 3D spatial queries }
  TNDXBoundingBox3D = record
    MinX, MaxX: Double;
    MinY, MaxY: Double;
    MinZ, MaxZ: Double;
  end;

  { R-Tree dimension types }
  TNDXRTreeDimension = (rtd1D, rtd2D, rtd3D, rtd4D, rtd5D);

  { R-Tree query type }
  TNDXRTreeQueryType = (
    rtqContains,      // R-Tree entry contains the query box
    rtqWithin,        // R-Tree entry is within the query box
    rtqOverlaps,      // R-Tree entry overlaps with the query box
    rtqNearest        // Nearest neighbors (requires aux columns)
  );

  { R-Tree entry for 2D }
  TNDXRTreeEntry2D = record
    ID: Int64;
    MinX, MaxX: Double;
    MinY, MaxY: Double;
  end;

  { R-Tree entry for 3D }
  TNDXRTreeEntry3D = record
    ID: Int64;
    MinX, MaxX: Double;
    MinY, MaxY: Double;
    MinZ, MaxZ: Double;
  end;

  { R-Tree Spatial Index Manager }
  TNDXSQLiteRTree = class
  private
    FConnection: INDXSQLiteConnection;
    FTableName: string;
    FDimension: TNDXRTreeDimension;
    FAuxColumns: TStringList;

    function GetDBHandle: Psqlite3;
    function BuildColumnList: string;
    function BuildAuxColumnList: string;
    function EscapeString(const S: string): string;
    function DimensionToColumnCount: Integer;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Creates a new R-Tree virtual table with the specified dimension (1D to 5D). }
    procedure CreateRTree(const ATableName: string;
      ADimension: TNDXRTreeDimension = rtd2D);

    { Creates a new R-Tree virtual table with additional auxiliary data columns. }
    procedure CreateRTreeWithAux(const ATableName: string;
      ADimension: TNDXRTreeDimension; const AAuxColumns: array of string);

    { Drops the specified R-Tree virtual table and its shadow tables. }
    procedure DropRTree(const ATableName: string);

    { Returns True if the named R-Tree table exists in the database. }
    function RTreeExists(const ATableName: string): Boolean;

    { Opens an existing R-Tree table for subsequent insert, update, and query operations. }
    procedure OpenRTree(const ATableName: string);

    { Inserts a 2D bounding box entry with explicit coordinate values. }
    procedure Insert2D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY: Double); overload;
    { Inserts a 2D entry from a pre-built record. }
    procedure Insert2D(const AEntry: TNDXRTreeEntry2D); overload;

    { Inserts a 3D bounding box entry with explicit coordinate values. }
    procedure Insert3D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY,
      AMinZ, AMaxZ: Double); overload;
    { Inserts a 3D entry from a pre-built record. }
    procedure Insert3D(const AEntry: TNDXRTreeEntry3D); overload;

    { Inserts a 2D point as a zero-area bounding box (MinX=MaxX, MinY=MaxY). }
    procedure InsertPoint2D(AID: Int64; AX, AY: Double);
    { Inserts a 3D point as a zero-volume bounding box. }
    procedure InsertPoint3D(AID: Int64; AX, AY, AZ: Double);

    { Inserts a 2D entry with additional auxiliary column values. }
    procedure Insert2DWithAux(AID: Int64; AMinX, AMaxX, AMinY, AMaxY: Double;
      const AAuxValues: array of Variant);

    { Updates the bounding box coordinates of an existing 2D entry. }
    procedure Update2D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY: Double);
    { Updates the bounding box coordinates of an existing 3D entry. }
    procedure Update3D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY,
      AMinZ, AMaxZ: Double);

    { Deletes the R-Tree entry with the specified ID. }
    procedure Delete(AID: Int64);

    { Returns all entries whose bounding box overlaps the given 2D rectangle. Caller must free. }
    function QueryBox2D(AMinX, AMaxX, AMinY, AMaxY: Double): TDataSet;
    { Returns all entries whose bounding box overlaps the given 3D box. Caller must free. }
    function QueryBox3D(AMinX, AMaxX, AMinY, AMaxY, AMinZ, AMaxZ: Double): TDataSet;

    { Queries entries overlapping a 2D bounding box record. Caller must free. }
    function QueryBox(const ABox: TNDXBoundingBox2D): TDataSet; overload;
    { Queries entries overlapping a 3D bounding box record. Caller must free. }
    function QueryBox(const ABox: TNDXBoundingBox3D): TDataSet; overload;

    { Returns all entries whose bounding box contains the given 2D point. Caller must free. }
    function QueryContainsPoint2D(AX, AY: Double): TDataSet;
    { Returns all entries whose bounding box contains the given 3D point. Caller must free. }
    function QueryContainsPoint3D(AX, AY, AZ: Double): TDataSet;

    { Returns all entries within the specified radius of a 2D center point. Caller must free. }
    function QueryWithinRadius2D(ACenterX, ACenterY, ARadius: Double): TDataSet;
    { Returns all entries within the specified radius of a 3D center point. Caller must free. }
    function QueryWithinRadius3D(ACenterX, ACenterY, ACenterZ, ARadius: Double): TDataSet;

    { Executes a custom query on the R-Tree table with the given WHERE clause. Caller must free. }
    function QueryCustom(const AWhereClause: string): TDataSet;

    { Returns the number of entries whose bounding box overlaps the given 2D rectangle. }
    function CountInBox2D(AMinX, AMaxX, AMinY, AMaxY: Double): Int64;

    { Returns all entries in the R-Tree table. Caller must free. }
    function GetAll: TDataSet;

    { Returns the entry with the specified ID. Caller must free. }
    function GetByID(AID: Int64): TDataSet;

    { Computes the overall 2D bounding box enclosing all entries. }
    function GetTotalBounds2D: TNDXBoundingBox2D;
    { Computes the overall 3D bounding box enclosing all entries. }
    function GetTotalBounds3D: TNDXBoundingBox3D;

    { Returns the total number of entries in the R-Tree table. }
    function GetCount: Int64;

    { Begins a transaction for bulk insertion to improve performance. }
    procedure BeginBulkInsert;
    { Commits the transaction started by BeginBulkInsert. }
    procedure EndBulkInsert;

    { Deletes all entries from the R-Tree table. }
    procedure Clear;

    { Rebuilds the R-Tree index to optimize storage and query performance. }
    procedure Rebuild;

    property Connection: INDXSQLiteConnection read FConnection;
    property TableName: string read FTableName;
    property Dimension: TNDXRTreeDimension read FDimension;
    property AuxColumns: TStringList read FAuxColumns;
  end;

{ Creates a 2D bounding box record from the given coordinate values. }
function BoundingBox2D(AMinX, AMaxX, AMinY, AMaxY: Double): TNDXBoundingBox2D;
{ Creates a 3D bounding box record from the given coordinate values. }
function BoundingBox3D(AMinX, AMaxX, AMinY, AMaxY, AMinZ, AMaxZ: Double): TNDXBoundingBox3D;
{ Creates a zero-area 2D bounding box representing a single point. }
function PointToBox2D(AX, AY: Double): TNDXBoundingBox2D;
{ Creates a zero-volume 3D bounding box representing a single point. }
function PointToBox3D(AX, AY, AZ: Double): TNDXBoundingBox3D;
{ Expands a 2D bounding box by the given margin in all directions. }
function ExpandBox2D(const ABox: TNDXBoundingBox2D; AMargin: Double): TNDXBoundingBox2D;
{ Returns True if the two 2D bounding boxes overlap. }
function BoxesOverlap2D(const A, B: TNDXBoundingBox2D): Boolean;
{ Returns True if the 2D bounding box contains the specified point. }
function BoxContainsPoint2D(const ABox: TNDXBoundingBox2D; AX, AY: Double): Boolean;

implementation

uses
  Variants;

{ Helper functions }

function BoundingBox2D(AMinX, AMaxX, AMinY, AMaxY: Double): TNDXBoundingBox2D;
begin
  Result.MinX := AMinX;
  Result.MaxX := AMaxX;
  Result.MinY := AMinY;
  Result.MaxY := AMaxY;
end;

function BoundingBox3D(AMinX, AMaxX, AMinY, AMaxY, AMinZ, AMaxZ: Double): TNDXBoundingBox3D;
begin
  Result.MinX := AMinX;
  Result.MaxX := AMaxX;
  Result.MinY := AMinY;
  Result.MaxY := AMaxY;
  Result.MinZ := AMinZ;
  Result.MaxZ := AMaxZ;
end;

function PointToBox2D(AX, AY: Double): TNDXBoundingBox2D;
begin
  Result.MinX := AX;
  Result.MaxX := AX;
  Result.MinY := AY;
  Result.MaxY := AY;
end;

function PointToBox3D(AX, AY, AZ: Double): TNDXBoundingBox3D;
begin
  Result.MinX := AX;
  Result.MaxX := AX;
  Result.MinY := AY;
  Result.MaxY := AY;
  Result.MinZ := AZ;
  Result.MaxZ := AZ;
end;

function ExpandBox2D(const ABox: TNDXBoundingBox2D; AMargin: Double): TNDXBoundingBox2D;
begin
  Result.MinX := ABox.MinX - AMargin;
  Result.MaxX := ABox.MaxX + AMargin;
  Result.MinY := ABox.MinY - AMargin;
  Result.MaxY := ABox.MaxY + AMargin;
end;

function BoxesOverlap2D(const A, B: TNDXBoundingBox2D): Boolean;
begin
  Result := not ((A.MaxX < B.MinX) or (A.MinX > B.MaxX) or
                 (A.MaxY < B.MinY) or (A.MinY > B.MaxY));
end;

function BoxContainsPoint2D(const ABox: TNDXBoundingBox2D; AX, AY: Double): Boolean;
begin
  Result := (AX >= ABox.MinX) and (AX <= ABox.MaxX) and
            (AY >= ABox.MinY) and (AY <= ABox.MaxY);
end;

{ TNDXSQLiteRTree }

constructor TNDXSQLiteRTree.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FTableName := '';
  FDimension := rtd2D;
  FAuxColumns := TStringList.Create;
end;

destructor TNDXSQLiteRTree.Destroy;
begin
  FAuxColumns.Free;
  inherited Destroy;
end;

function TNDXSQLiteRTree.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

function TNDXSQLiteRTree.EscapeString(const S: string): string;
begin
  Result := StringReplace(S, '''', '''''', [rfReplaceAll]);
end;

function TNDXSQLiteRTree.DimensionToColumnCount: Integer;
begin
  case FDimension of
    rtd1D: Result := 2;  // minX, maxX
    rtd2D: Result := 4;  // minX, maxX, minY, maxY
    rtd3D: Result := 6;  // minX, maxX, minY, maxY, minZ, maxZ
    rtd4D: Result := 8;  // minX, maxX, minY, maxY, minZ, maxZ, minW, maxW
    rtd5D: Result := 10; // 5 dimensions
  else
    Result := 4;
  end;
end;

function TNDXSQLiteRTree.BuildColumnList: string;
begin
  case FDimension of
    rtd1D: Result := 'id, minX, maxX';
    rtd2D: Result := 'id, minX, maxX, minY, maxY';
    rtd3D: Result := 'id, minX, maxX, minY, maxY, minZ, maxZ';
    rtd4D: Result := 'id, minX, maxX, minY, maxY, minZ, maxZ, minW, maxW';
    rtd5D: Result := 'id, minX, maxX, minY, maxY, minZ, maxZ, minW, maxW, minV, maxV';
  else
    Result := 'id, minX, maxX, minY, maxY';
  end;
end;

function TNDXSQLiteRTree.BuildAuxColumnList: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FAuxColumns.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + '+' + FAuxColumns[I];
  end;
end;

procedure TNDXSQLiteRTree.CreateRTree(const ATableName: string;
  ADimension: TNDXRTreeDimension);
var
  SQL: string;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to create R-Tree');

  FTableName := ATableName;
  FDimension := ADimension;
  FAuxColumns.Clear;

  SQL := Format('CREATE VIRTUAL TABLE %s USING rtree(%s)',
    [ATableName, BuildColumnList]);

  FConnection.ExecuteNonQuery(SQL);
end;

procedure TNDXSQLiteRTree.CreateRTreeWithAux(const ATableName: string;
  ADimension: TNDXRTreeDimension; const AAuxColumns: array of string);
var
  SQL, AuxCols: string;
  I: Integer;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to create R-Tree');

  FTableName := ATableName;
  FDimension := ADimension;
  FAuxColumns.Clear;

  // Build auxiliary columns list
  AuxCols := '';
  for I := Low(AAuxColumns) to High(AAuxColumns) do
  begin
    FAuxColumns.Add(AAuxColumns[I]);
    AuxCols := AuxCols + ', +' + AAuxColumns[I];
  end;

  SQL := Format('CREATE VIRTUAL TABLE %s USING rtree(%s%s)',
    [ATableName, BuildColumnList, AuxCols]);

  FConnection.ExecuteNonQuery(SQL);
end;

procedure TNDXSQLiteRTree.DropRTree(const ATableName: string);
begin
  FConnection.ExecuteNonQuery(Format('DROP TABLE IF EXISTS %s', [ATableName]));
  if SameText(FTableName, ATableName) then
  begin
    FTableName := '';
    FAuxColumns.Clear;
  end;
end;

function TNDXSQLiteRTree.RTreeExists(const ATableName: string): Boolean;
var
  V: Variant;
begin
  V := FConnection.ExecuteScalar(Format(
    'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name=''%s''',
    [EscapeString(ATableName)]));
  Result := (not VarIsNull(V)) and (Integer(V) > 0);
end;

procedure TNDXSQLiteRTree.OpenRTree(const ATableName: string);
var
  DS: TDataSet;
begin
  if not RTreeExists(ATableName) then
    raise ENDXSQLiteException.CreateFmt('R-Tree table "%s" does not exist', [ATableName]);

  FTableName := ATableName;

  // Detect dimension from table structure
  DS := FConnection.ExecuteQuery(Format('PRAGMA table_info(%s)', [ATableName]));
  try
    case DS.RecordCount of
      3: FDimension := rtd1D;   // id + 2 coords
      5: FDimension := rtd2D;   // id + 4 coords
      7: FDimension := rtd3D;   // id + 6 coords
      9: FDimension := rtd4D;   // id + 8 coords
      11: FDimension := rtd5D;  // id + 10 coords
    else
      // Check for auxiliary columns (more than base)
      if DS.RecordCount > 5 then
      begin
        FDimension := rtd2D;
        // Detect aux columns - columns starting with + are aux
        DS.First;
        while not DS.EOF do
        begin
          // Skip base columns
          DS.Next;
        end;
      end;
    end;
  finally
    DS.Free;
  end;
end;

procedure TNDXSQLiteRTree.Insert2D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY: Double);
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO %s (id, minX, maxX, minY, maxY) VALUES (%d, %g, %g, %g, %g)',
    [FTableName, AID, AMinX, AMaxX, AMinY, AMaxY]));
end;

procedure TNDXSQLiteRTree.Insert2D(const AEntry: TNDXRTreeEntry2D);
begin
  Insert2D(AEntry.ID, AEntry.MinX, AEntry.MaxX, AEntry.MinY, AEntry.MaxY);
end;

procedure TNDXSQLiteRTree.Insert3D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY,
  AMinZ, AMaxZ: Double);
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO %s (id, minX, maxX, minY, maxY, minZ, maxZ) VALUES (%d, %g, %g, %g, %g, %g, %g)',
    [FTableName, AID, AMinX, AMaxX, AMinY, AMaxY, AMinZ, AMaxZ]));
end;

procedure TNDXSQLiteRTree.Insert3D(const AEntry: TNDXRTreeEntry3D);
begin
  Insert3D(AEntry.ID, AEntry.MinX, AEntry.MaxX, AEntry.MinY, AEntry.MaxY,
    AEntry.MinZ, AEntry.MaxZ);
end;

procedure TNDXSQLiteRTree.InsertPoint2D(AID: Int64; AX, AY: Double);
begin
  Insert2D(AID, AX, AX, AY, AY);
end;

procedure TNDXSQLiteRTree.InsertPoint3D(AID: Int64; AX, AY, AZ: Double);
begin
  Insert3D(AID, AX, AX, AY, AY, AZ, AZ);
end;

procedure TNDXSQLiteRTree.Insert2DWithAux(AID: Int64; AMinX, AMaxX, AMinY, AMaxY: Double;
  const AAuxValues: array of Variant);
var
  SQL, Values: string;
  I: Integer;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Values := '';
  for I := Low(AAuxValues) to High(AAuxValues) do
  begin
    if VarIsNull(AAuxValues[I]) then
      Values := Values + ', NULL'
    else if VarIsStr(AAuxValues[I]) then
      Values := Values + ', ''' + EscapeString(VarToStr(AAuxValues[I])) + ''''
    else
      Values := Values + ', ' + VarToStr(AAuxValues[I]);
  end;

  SQL := Format(
    'INSERT INTO %s (id, minX, maxX, minY, maxY%s) VALUES (%d, %g, %g, %g, %g%s)',
    [FTableName, BuildAuxColumnList, AID, AMinX, AMaxX, AMinY, AMaxY, Values]);

  FConnection.ExecuteNonQuery(SQL);
end;

procedure TNDXSQLiteRTree.Update2D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY: Double);
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  FConnection.ExecuteNonQuery(Format(
    'UPDATE %s SET minX=%g, maxX=%g, minY=%g, maxY=%g WHERE id=%d',
    [FTableName, AMinX, AMaxX, AMinY, AMaxY, AID]));
end;

procedure TNDXSQLiteRTree.Update3D(AID: Int64; AMinX, AMaxX, AMinY, AMaxY,
  AMinZ, AMaxZ: Double);
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  FConnection.ExecuteNonQuery(Format(
    'UPDATE %s SET minX=%g, maxX=%g, minY=%g, maxY=%g, minZ=%g, maxZ=%g WHERE id=%d',
    [FTableName, AMinX, AMaxX, AMinY, AMaxY, AMinZ, AMaxZ, AID]));
end;

procedure TNDXSQLiteRTree.Delete(AID: Int64);
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  FConnection.ExecuteNonQuery(Format('DELETE FROM %s WHERE id=%d', [FTableName, AID]));
end;

function TNDXSQLiteRTree.QueryBox2D(AMinX, AMaxX, AMinY, AMaxY: Double): TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s WHERE minX <= %g AND maxX >= %g AND minY <= %g AND maxY >= %g',
    [FTableName, AMaxX, AMinX, AMaxY, AMinY]));
end;

function TNDXSQLiteRTree.QueryBox3D(AMinX, AMaxX, AMinY, AMaxY,
  AMinZ, AMaxZ: Double): TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s WHERE minX <= %g AND maxX >= %g AND ' +
    'minY <= %g AND maxY >= %g AND minZ <= %g AND maxZ >= %g',
    [FTableName, AMaxX, AMinX, AMaxY, AMinY, AMaxZ, AMinZ]));
end;

function TNDXSQLiteRTree.QueryBox(const ABox: TNDXBoundingBox2D): TDataSet;
begin
  Result := QueryBox2D(ABox.MinX, ABox.MaxX, ABox.MinY, ABox.MaxY);
end;

function TNDXSQLiteRTree.QueryBox(const ABox: TNDXBoundingBox3D): TDataSet;
begin
  Result := QueryBox3D(ABox.MinX, ABox.MaxX, ABox.MinY, ABox.MaxY, ABox.MinZ, ABox.MaxZ);
end;

function TNDXSQLiteRTree.QueryContainsPoint2D(AX, AY: Double): TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s WHERE minX <= %g AND maxX >= %g AND minY <= %g AND maxY >= %g',
    [FTableName, AX, AX, AY, AY]));
end;

function TNDXSQLiteRTree.QueryContainsPoint3D(AX, AY, AZ: Double): TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s WHERE minX <= %g AND maxX >= %g AND ' +
    'minY <= %g AND maxY >= %g AND minZ <= %g AND maxZ >= %g',
    [FTableName, AX, AX, AY, AY, AZ, AZ]));
end;

function TNDXSQLiteRTree.QueryWithinRadius2D(ACenterX, ACenterY, ARadius: Double): TDataSet;
begin
  // First filter by bounding box, then check actual distance
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s WHERE ' +
    'minX <= %g AND maxX >= %g AND minY <= %g AND maxY >= %g AND ' +
    '((minX + maxX)/2.0 - %g) * ((minX + maxX)/2.0 - %g) + ' +
    '((minY + maxY)/2.0 - %g) * ((minY + maxY)/2.0 - %g) <= %g',
    [FTableName,
     ACenterX + ARadius, ACenterX - ARadius,
     ACenterY + ARadius, ACenterY - ARadius,
     ACenterX, ACenterX, ACenterY, ACenterY,
     ARadius * ARadius]));
end;

function TNDXSQLiteRTree.QueryWithinRadius3D(ACenterX, ACenterY, ACenterZ,
  ARadius: Double): TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s WHERE ' +
    'minX <= %g AND maxX >= %g AND minY <= %g AND maxY >= %g AND ' +
    'minZ <= %g AND maxZ >= %g AND ' +
    '((minX + maxX)/2.0 - %g) * ((minX + maxX)/2.0 - %g) + ' +
    '((minY + maxY)/2.0 - %g) * ((minY + maxY)/2.0 - %g) + ' +
    '((minZ + maxZ)/2.0 - %g) * ((minZ + maxZ)/2.0 - %g) <= %g',
    [FTableName,
     ACenterX + ARadius, ACenterX - ARadius,
     ACenterY + ARadius, ACenterY - ARadius,
     ACenterZ + ARadius, ACenterZ - ARadius,
     ACenterX, ACenterX, ACenterY, ACenterY, ACenterZ, ACenterZ,
     ARadius * ARadius]));
end;

function TNDXSQLiteRTree.QueryCustom(const AWhereClause: string): TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format('SELECT * FROM %s WHERE %s',
    [FTableName, AWhereClause]));
end;

function TNDXSQLiteRTree.CountInBox2D(AMinX, AMaxX, AMinY, AMaxY: Double): Int64;
var
  V: Variant;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  V := FConnection.ExecuteScalar(Format(
    'SELECT COUNT(*) FROM %s WHERE minX <= %g AND maxX >= %g AND minY <= %g AND maxY >= %g',
    [FTableName, AMaxX, AMinX, AMaxY, AMinY]));

  if VarIsNull(V) then
    Result := 0
  else
    Result := V;
end;

function TNDXSQLiteRTree.GetAll: TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format('SELECT * FROM %s', [FTableName]));
end;

function TNDXSQLiteRTree.GetByID(AID: Int64): TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  Result := FConnection.ExecuteQuery(Format('SELECT * FROM %s WHERE id=%d',
    [FTableName, AID]));
end;

function TNDXSQLiteRTree.GetTotalBounds2D: TNDXBoundingBox2D;
var
  DS: TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  DS := FConnection.ExecuteQuery(Format(
    'SELECT MIN(minX), MAX(maxX), MIN(minY), MAX(maxY) FROM %s', [FTableName]));
  try
    if DS.IsEmpty or DS.Fields[0].IsNull then
    begin
      Result.MinX := 0;
      Result.MaxX := 0;
      Result.MinY := 0;
      Result.MaxY := 0;
    end
    else
    begin
      Result.MinX := DS.Fields[0].AsFloat;
      Result.MaxX := DS.Fields[1].AsFloat;
      Result.MinY := DS.Fields[2].AsFloat;
      Result.MaxY := DS.Fields[3].AsFloat;
    end;
  finally
    DS.Free;
  end;
end;

function TNDXSQLiteRTree.GetTotalBounds3D: TNDXBoundingBox3D;
var
  DS: TDataSet;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  DS := FConnection.ExecuteQuery(Format(
    'SELECT MIN(minX), MAX(maxX), MIN(minY), MAX(maxY), MIN(minZ), MAX(maxZ) FROM %s',
    [FTableName]));
  try
    if DS.IsEmpty or DS.Fields[0].IsNull then
    begin
      Result.MinX := 0;
      Result.MaxX := 0;
      Result.MinY := 0;
      Result.MaxY := 0;
      Result.MinZ := 0;
      Result.MaxZ := 0;
    end
    else
    begin
      Result.MinX := DS.Fields[0].AsFloat;
      Result.MaxX := DS.Fields[1].AsFloat;
      Result.MinY := DS.Fields[2].AsFloat;
      Result.MaxY := DS.Fields[3].AsFloat;
      Result.MinZ := DS.Fields[4].AsFloat;
      Result.MaxZ := DS.Fields[5].AsFloat;
    end;
  finally
    DS.Free;
  end;
end;

function TNDXSQLiteRTree.GetCount: Int64;
var
  V: Variant;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  V := FConnection.ExecuteScalar(Format('SELECT COUNT(*) FROM %s', [FTableName]));

  if VarIsNull(V) then
    Result := 0
  else
    Result := V;
end;

procedure TNDXSQLiteRTree.BeginBulkInsert;
begin
  FConnection.BeginTransaction;
end;

procedure TNDXSQLiteRTree.EndBulkInsert;
begin
  FConnection.Commit;
end;

procedure TNDXSQLiteRTree.Clear;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  FConnection.ExecuteNonQuery(Format('DELETE FROM %s', [FTableName]));
end;

procedure TNDXSQLiteRTree.Rebuild;
var
  TempTable, OriginalTable: string;
begin
  if FTableName = '' then
    raise ENDXSQLiteException.Create('No R-Tree table opened');

  // R-Tree tables can be rebuilt by recreating them
  TempTable := FTableName + '_rebuild_temp';
  OriginalTable := FTableName;

  FConnection.BeginTransaction;
  try
    // Create temp regular table with data
    FConnection.ExecuteNonQuery(Format(
      'CREATE TABLE %s AS SELECT * FROM %s', [TempTable, FTableName]));

    // Drop original
    FConnection.ExecuteNonQuery(Format('DROP TABLE %s', [FTableName]));

    // Recreate R-Tree
    CreateRTree(OriginalTable, FDimension);

    // Copy data back
    FConnection.ExecuteNonQuery(Format(
      'INSERT INTO %s SELECT * FROM %s', [FTableName, TempTable]));

    // Drop temp
    FConnection.ExecuteNonQuery(Format('DROP TABLE %s', [TempTable]));

    FConnection.Commit;
  except
    FConnection.Rollback;
    raise;
  end;
end;

end.
