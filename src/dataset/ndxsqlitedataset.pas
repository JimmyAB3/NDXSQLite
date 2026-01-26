{===============================================================================
  NDXSQLite - TDataSet Implementation
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 5 (depends on all lower levels)

  Complete TDataSet implementation for SQLite using native API.
  Compatible with all Lazarus data-aware controls.
===============================================================================}
unit ndxsqlitedataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants, FGL,
  ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions,
  ndxsqlitedatabase, ndxsqlitestatement;

type
  { Forward declarations }
  TNDXSQLiteDataSet = class;
  TNDXSQLiteParam = class;
  TNDXSQLiteParams = class;

  { Record state }
  TNDXRecordState = (rsClean, rsInserted, rsModified, rsDeleted);

  { Internal record structure }
  PNDXRecordInfo = ^TNDXRecordInfo;
  TNDXRecordInfo = record
    RecordNumber: Integer;
    BookmarkFlag: TBookmarkFlag;
    RecordState: TNDXRecordState;
  end;

  { Parameter data type }
  TNDXParamType = (
    ptUnknown,
    ptString,
    ptInteger,
    ptFloat,
    ptDateTime,
    ptDate,
    ptTime,
    ptBoolean,
    ptBlob,
    ptMemo
  );

  { Parameter }
  TNDXSQLiteParam = class(TCollectionItem)
  private
    FName: string;
    FValue: Variant;
    FParamType: TNDXParamType;
    FIsNull: Boolean;
    FBound: Boolean;

    procedure SetAsString(const AValue: string);
    function GetAsString: string;
    procedure SetAsInteger(AValue: Int64);
    function GetAsInteger: Int64;
    procedure SetAsFloat(AValue: Double);
    function GetAsFloat: Double;
    procedure SetAsDateTime(AValue: TDateTime);
    function GetAsDateTime: TDateTime;
    procedure SetAsBoolean(AValue: Boolean);
    function GetAsBoolean: Boolean;
    procedure SetAsVariant(AValue: Variant);
    function GetAsVariant: Variant;

  public
    constructor Create(ACollection: TCollection); override;
    { Resets the parameter to unbound null state. }
    procedure Clear;
    { Sets the parameter value to binary blob data from a pointer and size. }
    procedure SetBlobData(AData: Pointer; ASize: Integer); overload;
    { Sets the parameter value to binary blob data from a byte array. }
    procedure SetBlobData(const AData: TBytes); overload;
    { Returns the parameter blob value as a byte array. }
    function GetBlobData: TBytes;
    { Loads the parameter blob value from a stream. }
    procedure LoadFromStream(AStream: TStream);
    { Writes the parameter blob value to a stream. }
    procedure SaveToStream(AStream: TStream);

    property Name: string read FName write FName;
    property ParamType: TNDXParamType read FParamType write FParamType;
    property IsNull: Boolean read FIsNull;
    property Bound: Boolean read FBound;

    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  { Parameter collection }
  TNDXSQLiteParams = class(TCollection)
  private
    FOwner: TNDXSQLiteDataSet;
    function GetParam(AIndex: Integer): TNDXSQLiteParam;
    procedure SetParam(AIndex: Integer; AValue: TNDXSQLiteParam);

  protected
    function GetOwner: TPersistent; override;

  public
    constructor Create(AOwner: TNDXSQLiteDataSet);
    { Returns the parameter with the given name. Raises an exception if not found. }
    function ParamByName(const AName: string): TNDXSQLiteParam;
    { Returns the parameter with the given name, or nil if not found. }
    function FindParam(const AName: string): TNDXSQLiteParam;
    { Creates and adds a new parameter with the given name and type. }
    function CreateParam(const AName: string; AType: TNDXParamType = ptUnknown): TNDXSQLiteParam;
    { Copies parameter values from another collection (matching by name). }
    procedure AssignValues(ASource: TNDXSQLiteParams);
    { Parses the SQL string and creates parameters for each :name placeholder. }
    procedure ParseSQL(const ASQL: string);

    property Items[AIndex: Integer]: TNDXSQLiteParam read GetParam write SetParam; default;
  end;

  { Blob stream for field access }
  TNDXSQLiteBlobFieldStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TNDXSQLiteDataSet;
    FMode: TBlobStreamMode;
    FModified: Boolean;

  protected
    procedure SetSize(NewSize: LongInt); override;

  public
    { Creates a blob stream for reading or writing the given blob field. }
    constructor Create(AField: TBlobField; AMode: TBlobStreamMode);
    destructor Destroy; override;
    { Reads up to Count bytes from the blob into Buffer. }
    function Read(var Buffer; Count: LongInt): LongInt; override;
    { Writes Count bytes from Buffer to the blob. }
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

  { Record data storage }
  TNDXRecordData = class
  private
    FValues: array of Variant;
    FNulls: array of Boolean;
    FRowId: Int64;
    FState: TNDXRecordState;

  public
    { Creates a record data buffer for the given number of fields. }
    constructor Create(AFieldCount: Integer);
    { Sets the field value at the given index. }
    procedure SetValue(AIndex: Integer; AValue: Variant);
    { Returns the field value at the given index. }
    function GetValue(AIndex: Integer): Variant;
    { Sets the null flag for the field at the given index. }
    procedure SetNull(AIndex: Integer; AIsNull: Boolean);
    { Returns True if the field at the given index is null. }
    function IsNull(AIndex: Integer): Boolean;
    { Copies all field values, null flags, and state from another record. }
    procedure CopyFrom(ASource: TNDXRecordData);

    property RowId: Int64 read FRowId write FRowId;
    property State: TNDXRecordState read FState write FState;
  end;

  { Record list }
  TNDXRecordList = specialize TFPGObjectList<TNDXRecordData>;

  { Main DataSet class }
  TNDXSQLiteDataSet = class(TDataSet)
  private
    FDatabase: TNDXSQLiteDatabase;
    FOwnsDatabase: Boolean;
    FSQL: TStrings;
    FParams: TNDXSQLiteParams;
    FTableName: string;
    FPrimaryKey: string;

    FRecords: TNDXRecordList;
    FCurrentRecord: Integer;
    FRecordSize: Integer;
    FBookmarkSize: Integer;
    FCursorOpen: Boolean;

    FReadOnly: Boolean;
    FCachedUpdates: Boolean;
    FAutoIncrement: Boolean;
    FLastInsertId: Int64;
    FFetchAll: Boolean;

    FStatement: TNDXSQLiteStatement;
    FFieldMap: array of Integer;  // Maps field index to column index
    FModified: Boolean;
    FPrepared: Boolean;
    FEditingRecord: Integer;  // Record being edited/inserted (-1 if none)

    FDatabasePath: string;
    FAutoOpen: Boolean;

    procedure SetSQL(AValue: TStrings);
    procedure SQLChanged(Sender: TObject);
    procedure InternalPrepare;
    procedure InternalUnprepare;
    procedure FetchRecords;
    function FindFieldColumn(AField: TField): Integer;
    procedure SetDatabasePath(const AValue: string);
    procedure SetDatabase(AValue: TNDXSQLiteDatabase);

    function GenerateInsertSQL: string;
    function GenerateUpdateSQL: string;
    function GenerateDeleteSQL: string;

  protected
    procedure EnsureDatabaseOpen;
    { Buffer management }
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecordSize: Word; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;

    { Navigation }
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    { Bookmark support }
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;

    { Data access }
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    { Dataset state }
    function IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;
    function GetCanModify: Boolean; override;

    { Editing }
    procedure InternalEdit; override;
    procedure InternalPost; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;
    procedure InternalInsert; override;
    procedure InternalAddRecord(Buffer: Pointer; AAppend: Boolean); override;

    { Refresh }
    procedure InternalRefresh; override;

    { Misc }
    function GetRecordCount: Integer; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Open database }

    { Opens the SQLite database at the given file path. }
    procedure OpenDatabase(const APath: string);
    { Closes the currently open database connection. }
    procedure CloseDatabase;

    { SQL execution }

    { Executes the current SQL as a non-query and returns the number of affected rows. }
    function ExecSQL: Integer;
    { Prepares the SQL statement for execution. }
    procedure Prepare;
    { Releases the prepared statement resources. }
    procedure UnPrepare;

    { Parameter access }

    { Returns the parameter with the given name. Raises an exception if not found. }
    function ParamByName(const AName: string): TNDXSQLiteParam;
    { Sets the value of the named parameter. Creates it if it does not exist. }
    procedure SetParamValue(const AName: string; AValue: Variant);

    { Record access }

    { Searches for a record matching the given field values. Returns True if found. }
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    { Returns field values from the record matching the given key values. }
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;

    { Blob support }

    { Creates a stream for reading or writing the given blob field. }
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    { Cached updates }

    { Writes all pending insert/update/delete changes to the database. }
    procedure ApplyUpdates;
    { Discards all pending cached changes and reverts to the last applied state. }
    procedure CancelUpdates;

    { Refresh }

    { Reloads data from the database, preserving the current record position if possible. }
    procedure Refresh; reintroduce;

    { Transaction support }

    { Begins a new database transaction. }
    procedure StartTransaction;
    { Commits the current transaction. }
    procedure Commit;
    { Rolls back the current transaction. }
    procedure Rollback;

    { Utility }

    { Returns the value of the named field as a Variant. }
    function GetFieldAsVariant(const AFieldName: string): Variant;
    { Sets the value of the named field from a Variant. }
    procedure SetFieldAsVariant(const AFieldName: string; AValue: Variant);
    { Returns the SQLite rowid of the current record. }
    function GetRowId: Int64;

    { Properties }
    property Database: TNDXSQLiteDatabase read FDatabase write SetDatabase;
    property LastInsertId: Int64 read FLastInsertId;
    property Prepared: Boolean read FPrepared;
    property Modified: Boolean read FModified;

  published
    property DatabasePath: string read FDatabasePath write SetDatabasePath;
    property SQL: TStrings read FSQL write SetSQL;
    property Params: TNDXSQLiteParams read FParams;
    property TableName: string read FTableName write FTableName;
    property PrimaryKey: string read FPrimaryKey write FPrimaryKey;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property CachedUpdates: Boolean read FCachedUpdates write FCachedUpdates default False;
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen default False;

    { Inherited published properties }
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnFilterRecord;
    property OnNewRecord;
  end;

procedure Register;

implementation

uses
  StrUtils, DateUtils;

procedure Register;
begin
  RegisterComponents('NDXSQLite', [TNDXSQLiteDataSet]);
end;

const
  RECORD_INFO_SIZE = SizeOf(TNDXRecordInfo);

{ TNDXSQLiteParam }

constructor TNDXSQLiteParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := '';
  FValue := Null;
  FParamType := ptUnknown;
  FIsNull := True;
  FBound := False;
end;

procedure TNDXSQLiteParam.Clear;
begin
  FValue := Null;
  FIsNull := True;
  FBound := False;
  FParamType := ptUnknown;
end;

procedure TNDXSQLiteParam.SetAsString(const AValue: string);
begin
  FValue := AValue;
  FIsNull := False;
  FBound := True;
  if FParamType = ptUnknown then
    FParamType := ptString;
end;

function TNDXSQLiteParam.GetAsString: string;
begin
  if FIsNull then
    Result := ''
  else
    Result := VarToStr(FValue);
end;

procedure TNDXSQLiteParam.SetAsInteger(AValue: Int64);
begin
  FValue := AValue;
  FIsNull := False;
  FBound := True;
  if FParamType = ptUnknown then
    FParamType := ptInteger;
end;

function TNDXSQLiteParam.GetAsInteger: Int64;
begin
  if FIsNull then
    Result := 0
  else
    Result := Int64(FValue);
end;

procedure TNDXSQLiteParam.SetAsFloat(AValue: Double);
begin
  FValue := AValue;
  FIsNull := False;
  FBound := True;
  if FParamType = ptUnknown then
    FParamType := ptFloat;
end;

function TNDXSQLiteParam.GetAsFloat: Double;
begin
  if FIsNull then
    Result := 0.0
  else
    Result := Double(FValue);
end;

procedure TNDXSQLiteParam.SetAsDateTime(AValue: TDateTime);
begin
  FValue := AValue;
  FIsNull := AValue = 0;
  FBound := True;
  if FParamType = ptUnknown then
    FParamType := ptDateTime;
end;

function TNDXSQLiteParam.GetAsDateTime: TDateTime;
begin
  if FIsNull then
    Result := 0
  else
    Result := TDateTime(FValue);
end;

procedure TNDXSQLiteParam.SetAsBoolean(AValue: Boolean);
begin
  FValue := AValue;
  FIsNull := False;
  FBound := True;
  if FParamType = ptUnknown then
    FParamType := ptBoolean;
end;

function TNDXSQLiteParam.GetAsBoolean: Boolean;
begin
  if FIsNull then
    Result := False
  else
    Result := Boolean(FValue);
end;

procedure TNDXSQLiteParam.SetAsVariant(AValue: Variant);
begin
  FValue := AValue;
  FIsNull := VarIsNull(AValue) or VarIsEmpty(AValue);
  FBound := True;
end;

function TNDXSQLiteParam.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TNDXSQLiteParam.SetBlobData(AData: Pointer; ASize: Integer);
var
  Data: TBytes;
begin
  if (AData = nil) or (ASize <= 0) then
  begin
    FValue := Null;
    FIsNull := True;
  end
  else
  begin
    SetLength(Data, ASize);
    Move(AData^, Data[0], ASize);
    FValue := Data;
    FIsNull := False;
  end;
  FBound := True;
  FParamType := ptBlob;
end;

procedure TNDXSQLiteParam.SetBlobData(const AData: TBytes);
begin
  if Length(AData) = 0 then
  begin
    FValue := Null;
    FIsNull := True;
  end
  else
  begin
    FValue := AData;
    FIsNull := False;
  end;
  FBound := True;
  FParamType := ptBlob;
end;

function TNDXSQLiteParam.GetBlobData: TBytes;
begin
  if FIsNull then
    SetLength(Result, 0)
  else
    Result := FValue;
end;

procedure TNDXSQLiteParam.LoadFromStream(AStream: TStream);
var
  Data: TBytes;
begin
  if (AStream = nil) or (AStream.Size = 0) then
  begin
    SetBlobData(nil, 0);
    Exit;
  end;

  SetLength(Data, AStream.Size);
  AStream.Position := 0;
  AStream.ReadBuffer(Data[0], AStream.Size);
  SetBlobData(Data);
end;

procedure TNDXSQLiteParam.SaveToStream(AStream: TStream);
var
  Data: TBytes;
begin
  if (AStream = nil) or FIsNull then
    Exit;

  Data := GetBlobData;
  if Length(Data) > 0 then
    AStream.WriteBuffer(Data[0], Length(Data));
end;

{ TNDXSQLiteParams }

constructor TNDXSQLiteParams.Create(AOwner: TNDXSQLiteDataSet);
begin
  inherited Create(TNDXSQLiteParam);
  FOwner := AOwner;
end;

function TNDXSQLiteParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TNDXSQLiteParams.GetParam(AIndex: Integer): TNDXSQLiteParam;
begin
  Result := TNDXSQLiteParam(inherited Items[AIndex]);
end;

procedure TNDXSQLiteParams.SetParam(AIndex: Integer; AValue: TNDXSQLiteParam);
begin
  inherited Items[AIndex] := AValue;
end;

function TNDXSQLiteParams.ParamByName(const AName: string): TNDXSQLiteParam;
begin
  Result := FindParam(AName);
  if Result = nil then
    raise ENDXSQLiteException.Create('Parameter not found: ' + AName);
end;

function TNDXSQLiteParams.FindParam(const AName: string): TNDXSQLiteParam;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if SameText(Items[I].Name, AName) then
    begin
      Result := Items[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function TNDXSQLiteParams.CreateParam(const AName: string;
  AType: TNDXParamType): TNDXSQLiteParam;
begin
  Result := FindParam(AName);
  if Result = nil then
  begin
    Result := TNDXSQLiteParam(Add);
    Result.Name := AName;
  end;
  Result.ParamType := AType;
end;

procedure TNDXSQLiteParams.AssignValues(ASource: TNDXSQLiteParams);
var
  I: Integer;
  SrcParam, DstParam: TNDXSQLiteParam;
begin
  if ASource = nil then
    Exit;

  for I := 0 to ASource.Count - 1 do
  begin
    SrcParam := ASource[I];
    DstParam := FindParam(SrcParam.Name);
    if DstParam <> nil then
    begin
      DstParam.Value := SrcParam.Value;
      DstParam.ParamType := SrcParam.ParamType;
    end;
  end;
end;

procedure TNDXSQLiteParams.ParseSQL(const ASQL: string);
var
  I: Integer;
  InString: Boolean;
  ParamName: string;
  C: Char;
  PositionalIdx: Integer;
begin
  Clear;
  InString := False;
  I := 1;
  PositionalIdx := 0;

  while I <= Length(ASQL) do
  begin
    C := ASQL[I];

    // Handle string literals
    if C = '''' then
      InString := not InString
    else if not InString and (C = '?') then
    begin
      // Positional parameter (?)
      Inc(PositionalIdx);
      CreateParam(IntToStr(PositionalIdx));  // Create param with numeric index as name
    end
    else if not InString and (C in [':', '@', '$']) then
    begin
      // Found named parameter prefix
      ParamName := '';
      Inc(I);
      while (I <= Length(ASQL)) and (ASQL[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      begin
        ParamName := ParamName + ASQL[I];
        Inc(I);
      end;

      if ParamName <> '' then
        CreateParam(ParamName);

      Continue;  // Don't increment I again
    end;

    Inc(I);
  end;
end;

{ TNDXSQLiteBlobFieldStream }

constructor TNDXSQLiteBlobFieldStream.Create(AField: TBlobField; AMode: TBlobStreamMode);
var
  RecData: TNDXRecordData;
  BlobData: TBytes;
  RecIdx: Integer;
begin
  inherited Create;
  FField := AField;
  FDataSet := AField.DataSet as TNDXSQLiteDataSet;
  FMode := AMode;
  FModified := False;

  if FMode <> bmWrite then
  begin
    // Load existing data
    RecIdx := FDataSet.FCurrentRecord;
    if (RecIdx >= 0) and (RecIdx < FDataSet.FRecords.Count) then
    begin
      RecData := FDataSet.FRecords[RecIdx];
      if not RecData.IsNull(FField.FieldNo - 1) then
      begin
        BlobData := RecData.GetValue(FField.FieldNo - 1);
        if Length(BlobData) > 0 then
          WriteBuffer(BlobData[0], Length(BlobData));
        Position := 0;
      end;
    end;
  end;
end;

destructor TNDXSQLiteBlobFieldStream.Destroy;
var
  RecData: TNDXRecordData;
  BlobData: TBytes;
  RecIdx: Integer;
begin
  if FModified then
  begin
    // Save data back to record
    RecIdx := FDataSet.FCurrentRecord;
    if (RecIdx >= 0) and (RecIdx < FDataSet.FRecords.Count) then
    begin
      RecData := FDataSet.FRecords[RecIdx];
      if Size > 0 then
      begin
        SetLength(BlobData, Size);
        Position := 0;
        ReadBuffer(BlobData[0], Size);
        RecData.SetValue(FField.FieldNo - 1, BlobData);
        RecData.SetNull(FField.FieldNo - 1, False);
      end
      else
      begin
        RecData.SetNull(FField.FieldNo - 1, True);
      end;
      FDataSet.FModified := True;
    end;
  end;
  inherited Destroy;
end;

procedure TNDXSQLiteBlobFieldStream.SetSize(NewSize: LongInt);
begin
  inherited SetSize(NewSize);
  FModified := True;
end;

function TNDXSQLiteBlobFieldStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := inherited Read(Buffer, Count);
end;

function TNDXSQLiteBlobFieldStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

{ TNDXRecordData }

constructor TNDXRecordData.Create(AFieldCount: Integer);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FValues, AFieldCount);
  SetLength(FNulls, AFieldCount);
  // Initialize all fields as null (important for newly inserted records)
  for I := 0 to AFieldCount - 1 do
  begin
    FValues[I] := Null;
    FNulls[I] := True;
  end;
  FRowId := 0;
  FState := rsClean;
end;

procedure TNDXRecordData.SetValue(AIndex: Integer; AValue: Variant);
begin
  if (AIndex >= 0) and (AIndex < Length(FValues)) then
  begin
    FValues[AIndex] := AValue;
    FNulls[AIndex] := VarIsNull(AValue);
  end;
end;

function TNDXRecordData.GetValue(AIndex: Integer): Variant;
begin
  if (AIndex >= 0) and (AIndex < Length(FValues)) then
    Result := FValues[AIndex]
  else
    Result := Null;
end;

procedure TNDXRecordData.SetNull(AIndex: Integer; AIsNull: Boolean);
begin
  if (AIndex >= 0) and (AIndex < Length(FNulls)) then
  begin
    FNulls[AIndex] := AIsNull;
    if AIsNull then
      FValues[AIndex] := Null;
  end;
end;

function TNDXRecordData.IsNull(AIndex: Integer): Boolean;
begin
  if (AIndex >= 0) and (AIndex < Length(FNulls)) then
    Result := FNulls[AIndex]
  else
    Result := True;
end;

procedure TNDXRecordData.CopyFrom(ASource: TNDXRecordData);
var
  I: Integer;
begin
  if ASource = nil then
    Exit;

  SetLength(FValues, Length(ASource.FValues));
  SetLength(FNulls, Length(ASource.FNulls));

  for I := 0 to High(FValues) do
  begin
    FValues[I] := ASource.FValues[I];
    FNulls[I] := ASource.FNulls[I];
  end;

  FRowId := ASource.FRowId;
  FState := ASource.FState;
end;

{ TNDXSQLiteDataSet }

constructor TNDXSQLiteDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := @SQLChanged;
  FParams := TNDXSQLiteParams.Create(Self);
  FRecords := TNDXRecordList.Create(True);  // Owns objects

  FDatabase := nil;
  FOwnsDatabase := False;
  FStatement := nil;
  FCurrentRecord := -1;
  FCursorOpen := False;
  FRecordSize := RECORD_INFO_SIZE;
  FBookmarkSize := SizeOf(Integer);
  FReadOnly := False;
  FCachedUpdates := False;
  FAutoIncrement := True;
  FFetchAll := True;
  FPrepared := False;
  FModified := False;
  FAutoOpen := False;
  FEditingRecord := -1;
  FDatabasePath := '';
  FTableName := '';
  FPrimaryKey := 'id';  // Default primary key
end;

destructor TNDXSQLiteDataSet.Destroy;
begin
  Close;
  InternalUnprepare;

  if FOwnsDatabase and (FDatabase <> nil) then
  begin
    FDatabase.Close;
    FreeAndNil(FDatabase);
  end;

  FreeAndNil(FRecords);
  FreeAndNil(FParams);
  FreeAndNil(FSQL);
  inherited Destroy;
end;

procedure TNDXSQLiteDataSet.SetSQL(AValue: TStrings);
begin
  if FSQL.Text <> AValue.Text then
  begin
    Close;
    InternalUnprepare;
    FSQL.Assign(AValue);
  end;
end;

procedure TNDXSQLiteDataSet.SQLChanged(Sender: TObject);
begin
  InternalUnprepare;
  FParams.ParseSQL(FSQL.Text);
end;

procedure TNDXSQLiteDataSet.SetDatabasePath(const AValue: string);
begin
  if FDatabasePath <> AValue then
  begin
    Close;
    CloseDatabase;
    FDatabasePath := AValue;
    if FAutoOpen and (FDatabasePath <> '') then
      OpenDatabase(FDatabasePath);
  end;
end;

procedure TNDXSQLiteDataSet.SetDatabase(AValue: TNDXSQLiteDatabase);
begin
  if FDatabase <> AValue then
  begin
    Close;
    // Free existing database if we own it
    if FOwnsDatabase and (FDatabase <> nil) then
      FDatabase.Free;
    // Use the provided database (we don't own it)
    FDatabase := AValue;
    FOwnsDatabase := False;
    if FDatabase <> nil then
      FDatabasePath := FDatabase.DatabasePath;
  end;
end;

procedure TNDXSQLiteDataSet.EnsureDatabaseOpen;
begin
  if FDatabase = nil then
  begin
    if FDatabasePath <> '' then
      OpenDatabase(FDatabasePath)
    else
      raise ENDXSQLiteException.Create('Database not opened - set DatabasePath or call OpenDatabase');
  end;

  if not FDatabase.IsOpen then
    FDatabase.Open(FDatabasePath);
end;

function TNDXSQLiteDataSet.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

procedure TNDXSQLiteDataSet.InternalPrepare;
var
  I: Integer;
  ParamIdx: Integer;
begin
  if FPrepared then
    Exit;

  if Trim(FSQL.Text) = '' then
    raise ENDXSQLiteException.Create('SQL statement is empty');

  EnsureDatabaseOpen;

  FreeAndNil(FStatement);
  FStatement := TNDXSQLiteStatement.Create(FDatabase);
  FStatement.Prepare(FSQL.Text);

  // Bind parameters
  for I := 0 to FParams.Count - 1 do
  begin
    if FParams[I].Bound then
    begin
      // Check if positional parameter (numeric name like "1", "2", etc.)
      if TryStrToInt(FParams[I].Name, ParamIdx) then
        FStatement.BindVariant(ParamIdx, FParams[I].Value)
      else
        FStatement.BindVariantByName(FParams[I].Name, FParams[I].Value);
    end
    else if FParams[I].IsNull then
    begin
      if TryStrToInt(FParams[I].Name, ParamIdx) then
        FStatement.BindNull(ParamIdx)
      else
        FStatement.BindNullByName(FParams[I].Name);
    end;
  end;

  FPrepared := True;
end;

procedure TNDXSQLiteDataSet.InternalUnprepare;
begin
  FreeAndNil(FStatement);
  FPrepared := False;
end;

procedure TNDXSQLiteDataSet.FetchRecords;
var
  RecData: TNDXRecordData;
  I, ColCount, RowIdCol, PKCol: Integer;
  V: Variant;
  ColInfo: TNDXColumnInfo;
begin
  FRecords.Clear;
  FCurrentRecord := -1;

  if FStatement = nil then
    Exit;

  ColCount := FStatement.ColumnCount;
  RowIdCol := -1;
  PKCol := -1;

  // Find rowid column if present, or INTEGER PRIMARY KEY column
  for I := 0 to ColCount - 1 do
  begin
    if SameText(FStatement.ColumnName(I), 'rowid') or
       SameText(FStatement.ColumnName(I), '_rowid_') or
       SameText(FStatement.ColumnName(I), 'oid') then
    begin
      RowIdCol := I;
      Break;
    end;

    // Check for INTEGER PRIMARY KEY (which is an alias for rowid in SQLite)
    // Note: Only INTEGER PRIMARY KEY is rowid alias, not INT PRIMARY KEY
    ColInfo := FStatement.GetColumnInfo(I);
    if ColInfo.PrimaryKey and SameText(ColInfo.DeclaredType, 'INTEGER') then
      PKCol := I;
  end;

  // If no explicit rowid column, use PRIMARY KEY column
  if (RowIdCol < 0) and (PKCol >= 0) then
    RowIdCol := PKCol;

  while FStatement.Step do
  begin
    RecData := TNDXRecordData.Create(FieldDefs.Count);

    // Map columns to fields
    for I := 0 to FieldDefs.Count - 1 do
    begin
      if (I < Length(FFieldMap)) and (FFieldMap[I] >= 0) and
         (FFieldMap[I] < ColCount) then
      begin
        V := FStatement.ColumnAsVariant(FFieldMap[I]);
        RecData.SetValue(I, V);
        RecData.SetNull(I, FStatement.ColumnIsNull(FFieldMap[I]));
      end
      else
      begin
        RecData.SetNull(I, True);
      end;
    end;

    // Set rowid if available
    if RowIdCol >= 0 then
      RecData.RowId := FStatement.ColumnAsInteger(RowIdCol);

    RecData.State := rsClean;
    FRecords.Add(RecData);
  end;

  FStatement.Reset;
end;

function TNDXSQLiteDataSet.FindFieldColumn(AField: TField): Integer;
begin
  if (AField.FieldNo > 0) and (AField.FieldNo <= Length(FFieldMap)) then
    Result := FFieldMap[AField.FieldNo - 1]
  else
    Result := -1;
end;

procedure TNDXSQLiteDataSet.OpenDatabase(const APath: string);
begin
  CloseDatabase;

  FDatabase := TNDXSQLiteDatabase.Create;
  FOwnsDatabase := True;
  FDatabasePath := APath;

  FDatabase.Open(APath);
end;

procedure TNDXSQLiteDataSet.CloseDatabase;
begin
  Close;
  InternalUnprepare;

  if FOwnsDatabase and (FDatabase <> nil) then
  begin
    if FDatabase.IsOpen then
      FDatabase.Close;
    FreeAndNil(FDatabase);
  end;

  FDatabase := nil;
  FOwnsDatabase := False;
end;

function TNDXSQLiteDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecordSize);
end;

procedure TNDXSQLiteDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

function TNDXSQLiteDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

procedure TNDXSQLiteDataSet.InternalInitRecord(Buffer: TRecordBuffer);
var
  Info: PNDXRecordInfo;
begin
  if Buffer = nil then
    Exit;
  Info := PNDXRecordInfo(Buffer);
  Info^.RecordNumber := -1;
  Info^.BookmarkFlag := bfCurrent;
  Info^.RecordState := rsClean;
end;

function TNDXSQLiteDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Info: PNDXRecordInfo;
begin
  Result := grOK;

  case GetMode of
    gmNext:
      if FCurrentRecord < FRecords.Count - 1 then
        Inc(FCurrentRecord)
      else
        Result := grEOF;

    gmPrior:
      if FCurrentRecord > 0 then
        Dec(FCurrentRecord)
      else
        Result := grBOF;

    gmCurrent:
      if (FCurrentRecord < 0) or (FCurrentRecord >= FRecords.Count) then
        Result := grError;
  end;

  if (Result = grOK) and (Buffer <> nil) then
  begin
    Info := PNDXRecordInfo(Buffer);
    Info^.RecordNumber := FCurrentRecord;
    Info^.BookmarkFlag := bfCurrent;
    if FCurrentRecord < FRecords.Count then
      Info^.RecordState := FRecords[FCurrentRecord].State;
  end
  else if (Result = grError) and DoCheck then
    DatabaseError('No current record');
end;

procedure TNDXSQLiteDataSet.InternalFirst;
begin
  FCurrentRecord := -1;
end;

procedure TNDXSQLiteDataSet.InternalLast;
begin
  FCurrentRecord := FRecords.Count;
end;

procedure TNDXSQLiteDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
var
  Info: PNDXRecordInfo;
begin
  Info := PNDXRecordInfo(Buffer);
  FCurrentRecord := Info^.RecordNumber;
end;

function TNDXSQLiteDataSet.GetRecNo: Integer;
var
  RecBuf: TRecordBuffer;
  RecInfo: PNDXRecordInfo;
begin
  Result := 0;

  // Get record number from active buffer for consistency with GetFieldData
  RecBuf := ActiveBuffer;
  if RecBuf <> nil then
  begin
    RecInfo := PNDXRecordInfo(RecBuf);
    if (RecInfo^.RecordNumber >= 0) and (RecInfo^.RecordNumber < FRecords.Count) then
      Result := RecInfo^.RecordNumber + 1;
  end;
end;

procedure TNDXSQLiteDataSet.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value <= FRecords.Count) then
  begin
    FCurrentRecord := Value - 1;
    Resync([]);
  end;
end;

function TNDXSQLiteDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  if Buffer = nil then
  begin
    Result := bfCurrent;
    Exit;
  end;
  Result := PNDXRecordInfo(Buffer)^.BookmarkFlag;
end;

procedure TNDXSQLiteDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  if Buffer = nil then
    Exit;
  PNDXRecordInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TNDXSQLiteDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if (Buffer = nil) or (Data = nil) then
    Exit;
  PInteger(Data)^ := PNDXRecordInfo(Buffer)^.RecordNumber;
end;

procedure TNDXSQLiteDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if (Buffer = nil) or (Data = nil) then
    Exit;
  PNDXRecordInfo(Buffer)^.RecordNumber := PInteger(Data)^;
end;

procedure TNDXSQLiteDataSet.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentRecord := PInteger(ABookmark)^;
end;

function TNDXSQLiteDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecData: TNDXRecordData;
  FieldIdx: Integer;
  V: Variant;
  S: string;
  DT: TDateTime;
  B: Boolean;
  BlobData: TBytes;
  RecBuf: TRecordBuffer;
  RecInfo: PNDXRecordInfo;
  RecIdx: Integer;
begin
  Result := False;

  // Get the record index from the active buffer, not from FCurrentRecord
  // This ensures we read the correct record even during buffer operations
  RecBuf := ActiveBuffer;
  if RecBuf = nil then
    Exit;

  RecInfo := PNDXRecordInfo(RecBuf);
  RecIdx := RecInfo^.RecordNumber;

  // For insert mode, RecordNumber is -1, use FEditingRecord instead
  if RecIdx < 0 then
    RecIdx := FEditingRecord;

  if (RecIdx < 0) or (RecIdx >= FRecords.Count) then
    Exit;

  RecData := FRecords[RecIdx];

  // Use FieldDefs index for reliable mapping (Field.FieldNo may not match)
  FieldIdx := FieldDefs.IndexOf(Field.FieldName);

  if (FieldIdx < 0) or RecData.IsNull(FieldIdx) then
    Exit;

  V := RecData.GetValue(FieldIdx);
  if VarIsNull(V) then
    Exit;

  Result := True;

  if Buffer = nil then
    Exit;  // Just checking if data exists

  case Field.DataType of
    ftString, ftFixedChar, ftWideString, ftFixedWideChar, ftMemo, ftWideMemo:
      begin
        S := VarToStr(V);
        if Field.DataType in [ftString, ftFixedChar] then
        begin
          if Length(S) > Field.Size then
            S := Copy(S, 1, Field.Size);
          StrLCopy(PAnsiChar(Buffer), PAnsiChar(AnsiString(S)), Field.Size);
        end
        else
        begin
          PWideChar(Buffer)^ := #0;
          if S <> '' then
            StrLCopy(PWideChar(Buffer), PWideChar(WideString(S)), Field.Size);
        end;
      end;

    ftSmallInt:
      PSmallInt(Buffer)^ := SmallInt(V);

    ftInteger, ftAutoInc:
      PInteger(Buffer)^ := Integer(V);

    ftLargeInt:
      PInt64(Buffer)^ := Int64(V);

    ftWord:
      PWord(Buffer)^ := Word(V);

    ftBoolean:
      begin
        B := Boolean(V);
        PWordBool(Buffer)^ := B;
      end;

    ftFloat, ftCurrency:
      PDouble(Buffer)^ := Double(V);

    ftDate, ftTime, ftDateTime:
      begin
        if VarType(V) = varDate then
          DT := TDateTime(V)
        else if VarIsStr(V) then
        begin
          S := VarToStr(V);
          if not TryStrToDateTime(S, DT) then
            DT := 0;
        end
        else
          DT := TDateTime(V);
        PDateTime(Buffer)^ := DT;
      end;

    ftBlob, ftGraphic, ftTypedBinary:
      begin
        BlobData := V;
        if Length(BlobData) > 0 then
          Move(BlobData[0], Buffer^, Length(BlobData));
      end;
  else
    Result := False;
  end;
end;

procedure TNDXSQLiteDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecData: TNDXRecordData;
  FieldIdx: Integer;
  V: Variant;
  S: AnsiString;
  WS: WideString;
  RecBuf: TRecordBuffer;
  RecInfo: PNDXRecordInfo;
  RecIdx: Integer;
begin
  if FReadOnly then
    DatabaseError('Dataset is read-only');

  // Get the record index from the active buffer, not from FCurrentRecord
  RecBuf := ActiveBuffer;
  if RecBuf = nil then
    Exit;

  RecInfo := PNDXRecordInfo(RecBuf);
  RecIdx := RecInfo^.RecordNumber;

  // For insert mode, RecordNumber is -1, use FEditingRecord instead
  if RecIdx < 0 then
    RecIdx := FEditingRecord;

  if (RecIdx < 0) or (RecIdx >= FRecords.Count) then
    Exit;

  RecData := FRecords[RecIdx];

  // Use FieldDefs index for reliable mapping
  FieldIdx := FieldDefs.IndexOf(Field.FieldName);
  if FieldIdx < 0 then
    Exit;

  if Buffer = nil then
  begin
    RecData.SetNull(FieldIdx, True);
    FModified := True;
    Exit;
  end;

  case Field.DataType of
    ftString, ftFixedChar:
      begin
        S := PAnsiChar(Buffer);
        V := string(S);
      end;

    ftWideString, ftFixedWideChar:
      begin
        WS := PWideChar(Buffer);
        V := string(WS);
      end;

    ftMemo, ftWideMemo:
      V := string(PAnsiChar(Buffer));

    ftSmallInt:
      V := PSmallInt(Buffer)^;

    ftInteger, ftAutoInc:
      V := PInteger(Buffer)^;

    ftLargeInt:
      V := PInt64(Buffer)^;

    ftWord:
      V := PWord(Buffer)^;

    ftBoolean:
      V := PWordBool(Buffer)^;

    ftFloat, ftCurrency:
      V := PDouble(Buffer)^;

    ftDate, ftTime, ftDateTime:
      V := PDateTime(Buffer)^;

    ftBlob, ftGraphic, ftTypedBinary:
      ; // Handled by BlobStream
  else
    Exit;
  end;

  RecData.SetValue(FieldIdx, V);
  RecData.SetNull(FieldIdx, False);
  if RecData.State = rsClean then
    RecData.State := rsModified;
  FModified := True;
end;

function TNDXSQLiteDataSet.IsCursorOpen: Boolean;
begin
  Result := FCursorOpen;
end;

procedure TNDXSQLiteDataSet.InternalOpen;
var
  I, ColIdx: Integer;
begin
  InternalPrepare;

  // Build field definitions
  InternalInitFieldDefs;

  if DefaultFields then
    CreateFields;

  BindFields(True);

  // Build field to column mapping
  SetLength(FFieldMap, FieldDefs.Count);
  for I := 0 to FieldDefs.Count - 1 do
  begin
    ColIdx := -1;
    if FStatement <> nil then
    begin
      try
        ColIdx := FStatement.FindColumnIndex(FieldDefs[I].Name);
      except
        ColIdx := -1;
      end;
    end;
    FFieldMap[I] := ColIdx;
  end;

  // Fetch all records
  FetchRecords;

  // Position before first record (BOF state)
  FCurrentRecord := -1;

  // Mark cursor as open
  FCursorOpen := True;
end;

procedure TNDXSQLiteDataSet.InternalClose;
begin
  FCursorOpen := False;
  FRecords.Clear;
  FCurrentRecord := -1;
  SetLength(FFieldMap, 0);

  if DefaultFields then
    DestroyFields;

  InternalUnprepare;
end;

procedure TNDXSQLiteDataSet.InternalInitFieldDefs;
var
  I: Integer;
  ColInfo: TNDXColumnInfo;
  FieldType: TFieldType;
  FieldSize: Integer;
  DeclType: string;
begin
  FieldDefs.Clear;

  if FStatement = nil then
    Exit;

  for I := 0 to FStatement.ColumnCount - 1 do
  begin
    ColInfo := FStatement.GetColumnInfo(I);
    DeclType := UpperCase(ColInfo.DeclaredType);

    // Determine field type based on declared type
    FieldType := ftString;
    FieldSize := 255;

    if (DeclType = 'INTEGER') or (DeclType = 'INT') or
       (DeclType = 'SMALLINT') or (DeclType = 'MEDIUMINT') or
       (DeclType = 'TINYINT') or (DeclType = 'BIGINT') then
    begin
      if ColInfo.AutoIncrement then
        FieldType := ftAutoInc
      else if (DeclType = 'BIGINT') then
        FieldType := ftLargeInt
      else
        FieldType := ftInteger;
      FieldSize := 0;
    end
    else if (DeclType = 'REAL') or (DeclType = 'DOUBLE') or
            (DeclType = 'DOUBLE PRECISION') or (DeclType = 'FLOAT') then
    begin
      FieldType := ftFloat;
      FieldSize := 0;
    end
    else if (DeclType = 'NUMERIC') or (DeclType = 'DECIMAL') or
            (Pos('DECIMAL', DeclType) > 0) then
    begin
      FieldType := ftCurrency;
      FieldSize := 0;
    end
    else if (DeclType = 'BOOLEAN') or (DeclType = 'BOOL') then
    begin
      FieldType := ftBoolean;
      FieldSize := 0;
    end
    else if (DeclType = 'DATE') then
    begin
      FieldType := ftDate;
      FieldSize := 0;
    end
    else if (DeclType = 'TIME') then
    begin
      FieldType := ftTime;
      FieldSize := 0;
    end
    else if (DeclType = 'DATETIME') or (DeclType = 'TIMESTAMP') then
    begin
      FieldType := ftDateTime;
      FieldSize := 0;
    end
    else if (DeclType = 'BLOB') or (Pos('BLOB', DeclType) > 0) then
    begin
      FieldType := ftBlob;
      FieldSize := 0;
    end
    else if (DeclType = 'TEXT') then
    begin
      // SQLite TEXT is commonly used for regular strings (not CLOB/Memo)
      // Map to ftString with large size for practical use
      FieldType := ftString;
      FieldSize := 8192;  // Large but not unlimited
    end
    else if Pos('CLOB', DeclType) > 0 then
    begin
      FieldType := ftMemo;
      FieldSize := 0;
    end
    else if Pos('VARCHAR', DeclType) > 0 then
    begin
      FieldType := ftString;
      // Try to extract size from VARCHAR(n)
      FieldSize := 255;
    end
    else if Pos('CHAR', DeclType) > 0 then
    begin
      FieldType := ftFixedChar;
      FieldSize := 255;
    end;

    FieldDefs.Add(ColInfo.Name, FieldType, FieldSize, not ColInfo.NotNull);
  end;
end;

function TNDXSQLiteDataSet.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

procedure TNDXSQLiteDataSet.InternalEdit;
begin
  // Save the record being edited
  FEditingRecord := FCurrentRecord;
end;

procedure TNDXSQLiteDataSet.InternalPost;
var
  RecData: TNDXRecordData;
  InsertSQL, UpdateSQL: string;
  Stmt: TNDXSQLiteStatement;
  I, ParamIdx: Integer;
  RecIdx: Integer;
begin
  // Use FEditingRecord instead of FCurrentRecord since FCurrentRecord might have been
  // changed by TDataSet internal operations (like Resync calling InternalLast)
  RecIdx := FEditingRecord;
  if (RecIdx < 0) or (RecIdx >= FRecords.Count) then
    Exit;

  RecData := FRecords[RecIdx];

  if not FCachedUpdates then
  begin
    EnsureDatabaseOpen;

    case RecData.State of
      rsInserted:
        begin
          InsertSQL := GenerateInsertSQL;
          if InsertSQL <> '' then
          begin
            Stmt := TNDXSQLiteStatement.Create(FDatabase);
            try
              Stmt.Prepare(InsertSQL);
              // Bind field values (skip auto-increment to match GenerateInsertSQL)
              ParamIdx := 1;
              for I := 0 to FieldDefs.Count - 1 do
              begin
                if FieldDefs[I].DataType = ftAutoInc then
                  Continue;  // Skip auto-increment - not in INSERT SQL

                if not RecData.IsNull(I) then
                  Stmt.BindVariant(ParamIdx, RecData.GetValue(I))
                else
                  Stmt.BindNull(ParamIdx);
                Inc(ParamIdx);
              end;
              Stmt.Execute;
              FLastInsertId := FDatabase.LastInsertRowId;
              RecData.RowId := FLastInsertId;
              // Update the auto-increment field value in the record
              for I := 0 to FieldDefs.Count - 1 do
              begin
                if FieldDefs[I].DataType = ftAutoInc then
                begin
                  RecData.SetValue(I, FLastInsertId);
                  RecData.SetNull(I, False);
                  Break;
                end;
              end;
            finally
              Stmt.Free;
            end;
          end;
        end;

      rsModified:
        begin
          UpdateSQL := GenerateUpdateSQL;
          if (UpdateSQL <> '') and (RecData.RowId > 0) then
          begin
            Stmt := TNDXSQLiteStatement.Create(FDatabase);
            try
              Stmt.Prepare(UpdateSQL);
              // Bind field values + rowid
              for I := 0 to FieldDefs.Count - 1 do
              begin
                if not RecData.IsNull(I) then
                  Stmt.BindVariant(I + 1, RecData.GetValue(I))
                else
                  Stmt.BindNull(I + 1);
              end;
              Stmt.BindInteger(FieldDefs.Count + 1, RecData.RowId);
              Stmt.Execute;
            finally
              Stmt.Free;
            end;
          end;
        end;
    end;
  end;

  RecData.State := rsClean;
  FModified := False;
end;

procedure TNDXSQLiteDataSet.InternalCancel;
var
  RecData: TNDXRecordData;
begin
  // If we're cancelling an insert, remove the inserted record
  if (FEditingRecord >= 0) and (FEditingRecord < FRecords.Count) then
  begin
    RecData := FRecords[FEditingRecord];
    if RecData.State = rsInserted then
    begin
      // Remove the newly inserted record
      FRecords.Delete(FEditingRecord);
      // Adjust current record position
      if FCurrentRecord >= FRecords.Count then
        FCurrentRecord := FRecords.Count - 1;
    end;
  end;

  // Reset editing record
  FEditingRecord := -1;
  if not FCachedUpdates then
    FModified := False;
end;

procedure TNDXSQLiteDataSet.InternalDelete;
var
  RecData: TNDXRecordData;
  DeleteSQL: string;
  Stmt: TNDXSQLiteStatement;
begin
  if (FCurrentRecord < 0) or (FCurrentRecord >= FRecords.Count) then
    Exit;

  RecData := FRecords[FCurrentRecord];

  if FCachedUpdates then
  begin
    RecData.State := rsDeleted;
  end
  else
  begin
    EnsureDatabaseOpen;

    DeleteSQL := GenerateDeleteSQL;
    if (DeleteSQL <> '') and (RecData.RowId > 0) then
    begin
      Stmt := TNDXSQLiteStatement.Create(FDatabase);
      try
        Stmt.Prepare(DeleteSQL);
        Stmt.BindInteger(1, RecData.RowId);
        Stmt.Execute;
      finally
        Stmt.Free;
      end;
    end;

    FRecords.Delete(FCurrentRecord);
    if FCurrentRecord >= FRecords.Count then
      FCurrentRecord := FRecords.Count - 1;
  end;
end;

procedure TNDXSQLiteDataSet.InternalInsert;
var
  RecData: TNDXRecordData;
begin
  RecData := TNDXRecordData.Create(FieldDefs.Count);
  RecData.State := rsInserted;
  FRecords.Add(RecData);
  FCurrentRecord := FRecords.Count - 1;
  FEditingRecord := FCurrentRecord;  // Save the record being inserted
end;

procedure TNDXSQLiteDataSet.InternalAddRecord(Buffer: Pointer; AAppend: Boolean);
begin
  // Called after post - AAppend parameter not used but required by interface
end;

procedure TNDXSQLiteDataSet.InternalRefresh;
var
  SavedRec: Integer;
begin
  // Save current position
  SavedRec := FCurrentRecord;

  // Clear existing records but keep field definitions
  FRecords.Clear;
  FCurrentRecord := -1;

  // Re-prepare statement if needed
  if not FPrepared then
    InternalPrepare;

  // Re-fetch all records
  FetchRecords;

  // Restore position if possible
  if (SavedRec >= 0) and (SavedRec < FRecords.Count) then
    FCurrentRecord := SavedRec
  else if FRecords.Count > 0 then
    FCurrentRecord := 0;
end;

procedure TNDXSQLiteDataSet.Refresh;
var
  SavedRecNo: Integer;
begin
  // Public Refresh method that properly handles dataset state
  if not Active then
    Exit;

  SavedRecNo := RecNo;
  DisableControls;
  try
    Close;
    Open;
    // Try to restore position
    if (SavedRecNo > 0) and (SavedRecNo <= RecordCount) then
      RecNo := SavedRecNo;
  finally
    EnableControls;
  end;
end;

function TNDXSQLiteDataSet.ExecSQL: Integer;
begin
  EnsureDatabaseOpen;
  InternalPrepare;
  try
    Result := FStatement.Execute;
    FLastInsertId := FDatabase.LastInsertRowId;
  finally
    InternalUnprepare;
  end;
end;

procedure TNDXSQLiteDataSet.Prepare;
begin
  InternalPrepare;
end;

procedure TNDXSQLiteDataSet.UnPrepare;
begin
  InternalUnprepare;
end;

function TNDXSQLiteDataSet.ParamByName(const AName: string): TNDXSQLiteParam;
begin
  Result := FParams.ParamByName(AName);
end;

procedure TNDXSQLiteDataSet.SetParamValue(const AName: string; AValue: Variant);
begin
  FParams.ParamByName(AName).Value := AValue;
end;

function TNDXSQLiteDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  I, J: Integer;
  FieldList: TStringList;
  RecData: TNDXRecordData;
  FieldIdx: Integer;
  Match: Boolean;
  SearchVal, RecVal: Variant;
begin
  Result := False;
  CheckBrowseMode;

  FieldList := TStringList.Create;
  try
    FieldList.Delimiter := ';';
    FieldList.StrictDelimiter := True;
    FieldList.DelimitedText := KeyFields;

    for I := 0 to FRecords.Count - 1 do
    begin
      RecData := FRecords[I];
      Match := True;

      for J := 0 to FieldList.Count - 1 do
      begin
        FieldIdx := FieldDefs.IndexOf(FieldList[J]);
        if FieldIdx < 0 then
        begin
          Match := False;
          Break;
        end;

        if VarIsArray(KeyValues) then
          SearchVal := KeyValues[J]
        else
          SearchVal := KeyValues;

        RecVal := RecData.GetValue(FieldIdx);

        if loPartialKey in Options then
        begin
          if loCaseInsensitive in Options then
            Match := Pos(UpperCase(VarToStr(SearchVal)), UpperCase(VarToStr(RecVal))) > 0
          else
            Match := Pos(VarToStr(SearchVal), VarToStr(RecVal)) > 0;
        end
        else
        begin
          if loCaseInsensitive in Options then
            Match := SameText(VarToStr(SearchVal), VarToStr(RecVal))
          else
            Match := (SearchVal = RecVal);
        end;

        if not Match then
          Break;
      end;

      if Match then
      begin
        FCurrentRecord := I;
        Resync([rmExact, rmCenter]);
        Result := True;
        Exit;
      end;
    end;
  finally
    FieldList.Free;
  end;
end;

function TNDXSQLiteDataSet.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
var
  SavedRec: Integer;
begin
  Result := Null;
  SavedRec := FCurrentRecord;
  try
    if Locate(KeyFields, KeyValues, []) then
    begin
      if Pos(';', ResultFields) > 0 then
        Result := FieldValues[ResultFields]
      else
        Result := FieldByName(ResultFields).Value;
    end;
  finally
    FCurrentRecord := SavedRec;
  end;
end;

function TNDXSQLiteDataSet.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := TNDXSQLiteBlobFieldStream.Create(Field as TBlobField, Mode);
end;

procedure TNDXSQLiteDataSet.ApplyUpdates;
var
  I: Integer;
  RecData: TNDXRecordData;
begin
  EnsureDatabaseOpen;

  FDatabase.BeginTransaction;
  try
    for I := FRecords.Count - 1 downto 0 do
    begin
      RecData := FRecords[I];
      case RecData.State of
        rsInserted:
          begin
            // Execute insert
            FCurrentRecord := I;
            InternalPost;
          end;
        rsModified:
          begin
            // Execute update
            FCurrentRecord := I;
            InternalPost;
          end;
        rsDeleted:
          begin
            // Execute delete and remove from list
            FCurrentRecord := I;
            InternalDelete;
          end;
      end;
    end;
    FDatabase.Commit;
  except
    FDatabase.Rollback;
    raise;
  end;
end;

procedure TNDXSQLiteDataSet.CancelUpdates;
begin
  // Reload all data
  Refresh;
end;

procedure TNDXSQLiteDataSet.StartTransaction;
begin
  EnsureDatabaseOpen;
  FDatabase.BeginTransaction;
end;

procedure TNDXSQLiteDataSet.Commit;
begin
  if (FDatabase <> nil) and FDatabase.InTransaction then
    FDatabase.Commit;
end;

procedure TNDXSQLiteDataSet.Rollback;
begin
  if (FDatabase <> nil) and FDatabase.InTransaction then
    FDatabase.Rollback;
end;

function TNDXSQLiteDataSet.GetFieldAsVariant(const AFieldName: string): Variant;
var
  F: TField;
begin
  F := FindField(AFieldName);
  if F <> nil then
    Result := F.Value
  else
    Result := Null;
end;

procedure TNDXSQLiteDataSet.SetFieldAsVariant(const AFieldName: string; AValue: Variant);
var
  F: TField;
begin
  F := FindField(AFieldName);
  if F <> nil then
    F.Value := AValue;
end;

function TNDXSQLiteDataSet.GetRowId: Int64;
var
  RecData: TNDXRecordData;
begin
  if (FCurrentRecord >= 0) and (FCurrentRecord < FRecords.Count) then
  begin
    RecData := FRecords[FCurrentRecord];
    Result := RecData.RowId;
  end
  else
    Result := 0;
end;

function TNDXSQLiteDataSet.GenerateInsertSQL: string;
var
  I: Integer;
  FieldNames, Values: string;
begin
  if FTableName = '' then
  begin
    Result := '';
    Exit;
  end;

  FieldNames := '';
  Values := '';

  for I := 0 to FieldDefs.Count - 1 do
  begin
    if FieldDefs[I].DataType = ftAutoInc then
      Continue;  // Skip auto-increment

    if FieldNames <> '' then
    begin
      FieldNames := FieldNames + ', ';
      Values := Values + ', ';
    end;
    FieldNames := FieldNames + QuoteIdentifier(FieldDefs[I].Name);
    Values := Values + '?';
  end;

  Result := Format('INSERT INTO %s (%s) VALUES (%s)',
    [QuoteIdentifier(FTableName), FieldNames, Values]);
end;

function TNDXSQLiteDataSet.GenerateUpdateSQL: string;
var
  I: Integer;
  SetClause: string;
begin
  if FTableName = '' then
  begin
    Result := '';
    Exit;
  end;

  SetClause := '';

  for I := 0 to FieldDefs.Count - 1 do
  begin
    if SetClause <> '' then
      SetClause := SetClause + ', ';
    SetClause := SetClause + QuoteIdentifier(FieldDefs[I].Name) + ' = ?';
  end;

  Result := Format('UPDATE %s SET %s WHERE rowid = ?',
    [QuoteIdentifier(FTableName), SetClause]);
end;

function TNDXSQLiteDataSet.GenerateDeleteSQL: string;
begin
  if FTableName = '' then
  begin
    Result := '';
    Exit;
  end;

  Result := Format('DELETE FROM %s WHERE rowid = ?',
    [QuoteIdentifier(FTableName)]);
end;

end.
