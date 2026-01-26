{===============================================================================
  NDXSQLite GUI Example - Search, Filter, and Sort
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - Locate method for finding specific records
  - Filter property for subset display
  - Dynamic SQL ORDER BY for sorting
  - Incremental search with partial matching
  - Column header click sorting
  - Combined filter + sort operations

  This example shows various techniques for searching, filtering, and sorting
  data in TNDXSQLiteDataSet. These are essential operations for any data
  browsing interface, allowing users to quickly find and organize records.

  Key Concepts:
  - Locate: Positions cursor on matching record (client-side search)
  - Filter: Displays only matching records (client-side filtering)
  - ORDER BY: Server-side sorting via SQL (most efficient for large datasets)
  - Indexed fields improve search/sort performance significantly
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, DBCtrls, DB, ComCtrls,
  ndxsqlitedataset;

type
  TSortDirection = (sdNone, sdAscending, sdDescending);

  { TfrmMain }
  TfrmMain = class(TForm)
    btnLocate: TButton;
    btnLocateNext: TButton;
    btnApplyFilter: TButton;
    btnClearFilter: TButton;
    btnRefresh: TButton;
    btnClearLog: TButton;
    cboSearchField: TComboBox;
    cboSearchMode: TComboBox;
    cboFilterField: TComboBox;
    cboFilterOperator: TComboBox;
    cboSortField: TComboBox;
    cboSortDirection: TComboBox;
    chkCaseInsensitive: TCheckBox;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    edtSearchValue: TEdit;
    edtFilterValue: TEdit;
    gbSearch: TGroupBox;
    gbFilter: TGroupBox;
    gbSort: TGroupBox;
    lblSearchField: TLabel;
    lblSearchMode: TLabel;
    lblSearchValue: TLabel;
    lblFilterField: TLabel;
    lblFilterOperator: TLabel;
    lblFilterValue: TLabel;
    lblSortField: TLabel;
    lblSortDirection: TLabel;
    lblRecordCount: TLabel;
    memoLog: TMemo;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlStatus: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLocateClick(Sender: TObject);
    procedure btnLocateNextClick(Sender: TObject);
    procedure btnApplyFilterClick(Sender: TObject);
    procedure btnClearFilterClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure cboSortFieldChange(Sender: TObject);
    procedure cboSortDirectionChange(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure edtSearchValueKeyPress(Sender: TObject; var Key: char);
  private
    FDataSet: TNDXSQLiteDataSet;
    FLastSearchValue: string;
    FLastSearchField: string;
    FLastSearchMode: Integer;
    FSortColumn: string;
    FSortDirection: TSortDirection;
    FFilterActive: Boolean;
    { Appends a timestamped message to the activity log. }
    procedure Log(const AMessage: string);
    { Initializes the database and creates sample data. }
    procedure InitializeDatabase;
    { Populates field combo boxes with available columns. }
    procedure PopulateFieldLists;
    { Updates the status bar with current record count. }
    procedure UpdateStatusBar;
    { Applies sorting based on current settings. }
    procedure ApplySorting;
    { Returns the base SQL without ORDER BY clause. }
    function GetBaseSQL: string;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  Variants;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Initialize sort tracking
  FSortColumn := '';
  FSortDirection := sdNone;

  // Create dataset
  FDataSet := TNDXSQLiteDataSet.Create(Self);
  DataSource1.DataSet := FDataSet;

  // Initialize database
  InitializeDatabase;

  // Populate UI controls
  PopulateFieldLists;

  // Set default values
  cboSearchMode.ItemIndex := 0;      // Contains
  cboFilterOperator.ItemIndex := 0;  // Contains
  cboSortDirection.ItemIndex := 0;   // Ascending

  // Welcome message
  Log('Search, Filter, and Sort Demo started');
  Log('');
  Log('LOCATE: Finds and positions on a matching record');
  Log('FILTER: Shows only records matching the criteria');
  Log('SORT: Click column headers or use sort controls');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FDataSet) then
  begin
    FDataSet.Close;
    FDataSet.CloseDatabase;
  end;
end;

procedure TfrmMain.InitializeDatabase;
var
  DBPath: string;
begin
  DBPath := ExtractFilePath(ParamStr(0)) + 'searchfilter_demo.db';

  FDataSet.OpenDatabase(DBPath);

  // Create a products table with various searchable fields
  FDataSet.SQL.Text := 'CREATE TABLE IF NOT EXISTS products (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'name TEXT NOT NULL, ' +
    'category TEXT, ' +
    'price REAL, ' +
    'stock INTEGER, ' +
    'supplier TEXT, ' +
    'created_at TEXT DEFAULT (datetime(''now'', ''localtime''))' +
    ')';
  FDataSet.ExecSQL;

  // Create index on commonly searched fields for better performance
  FDataSet.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_products_name ON products(name)';
  FDataSet.ExecSQL;
  FDataSet.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_products_category ON products(category)';
  FDataSet.ExecSQL;

  // Check if sample data exists
  FDataSet.SQL.Text := 'SELECT COUNT(*) FROM products';
  FDataSet.Open;
  if FDataSet.Fields[0].AsInteger = 0 then
  begin
    FDataSet.Close;
    // Insert sample products for demonstration
    FDataSet.SQL.Text := 'INSERT INTO products (name, category, price, stock, supplier) VALUES ' +
      '(''Laptop Pro 15'', ''Electronics'', 1299.99, 25, ''TechCorp''), ' +
      '(''Laptop Basic 14'', ''Electronics'', 599.99, 50, ''TechCorp''), ' +
      '(''Desktop Tower'', ''Electronics'', 899.99, 15, ''CompuBuild''), ' +
      '(''Wireless Mouse'', ''Accessories'', 29.99, 200, ''PeripheralWorld''), ' +
      '(''Mechanical Keyboard'', ''Accessories'', 89.99, 75, ''PeripheralWorld''), ' +
      '(''USB Hub 7-Port'', ''Accessories'', 24.99, 150, ''CableKing''), ' +
      '(''Monitor 27 inch'', ''Displays'', 349.99, 40, ''ScreenMaster''), ' +
      '(''Monitor 32 inch 4K'', ''Displays'', 549.99, 20, ''ScreenMaster''), ' +
      '(''Webcam HD'', ''Accessories'', 79.99, 100, ''VideoTech''), ' +
      '(''Headset Pro'', ''Audio'', 149.99, 60, ''SoundMax''), ' +
      '(''Speakers 2.1'', ''Audio'', 89.99, 45, ''SoundMax''), ' +
      '(''Microphone Studio'', ''Audio'', 199.99, 30, ''SoundMax''), ' +
      '(''Printer Laser'', ''Office'', 299.99, 25, ''PrintPro''), ' +
      '(''Scanner Flatbed'', ''Office'', 149.99, 35, ''PrintPro''), ' +
      '(''External SSD 1TB'', ''Storage'', 109.99, 80, ''DataStore''), ' +
      '(''External HDD 4TB'', ''Storage'', 89.99, 60, ''DataStore''), ' +
      '(''USB Flash 64GB'', ''Storage'', 12.99, 500, ''DataStore''), ' +
      '(''Network Router'', ''Networking'', 79.99, 55, ''NetGear''), ' +
      '(''Ethernet Cable 10m'', ''Networking'', 9.99, 300, ''CableKing''), ' +
      '(''Wireless Adapter'', ''Networking'', 19.99, 120, ''NetGear'')';
    FDataSet.ExecSQL;
  end
  else
    FDataSet.Close;

  // Configure dataset for browsing
  FDataSet.TableName := 'products';
  FDataSet.PrimaryKey := 'id';
  FDataSet.SQL.Text := GetBaseSQL;
  FDataSet.Open;

  UpdateStatusBar;
  Log('Database initialized: ' + DBPath);
  Log(Format('Loaded %d products', [FDataSet.RecordCount]));
end;

function TfrmMain.GetBaseSQL: string;
begin
  // Base SQL without ORDER BY - sorting is added dynamically
  Result := 'SELECT id, name, category, price, stock, supplier, created_at FROM products';
end;

procedure TfrmMain.PopulateFieldLists;
var
  I: Integer;
begin
  // Populate combo boxes with field names from the dataset
  cboSearchField.Items.Clear;
  cboFilterField.Items.Clear;
  cboSortField.Items.Clear;

  cboSortField.Items.Add('(None)');

  for I := 0 to FDataSet.FieldCount - 1 do
  begin
    // Add all fields to search/filter lists
    cboSearchField.Items.Add(FDataSet.Fields[I].FieldName);
    cboFilterField.Items.Add(FDataSet.Fields[I].FieldName);
    cboSortField.Items.Add(FDataSet.Fields[I].FieldName);
  end;

  // Set defaults to 'name' field which is commonly searched
  cboSearchField.ItemIndex := cboSearchField.Items.IndexOf('name');
  cboFilterField.ItemIndex := cboFilterField.Items.IndexOf('name');
  cboSortField.ItemIndex := 0;  // (None)
end;

procedure TfrmMain.btnLocateClick(Sender: TObject);
var
  FieldName, SearchValue, FieldValue: string;
  Found: Boolean;
  SearchMode: Integer;
  CaseInsensitive: Boolean;
begin
  // Locate positions the cursor on the first matching record
  if cboSearchField.ItemIndex < 0 then
  begin
    Log('ERROR: Please select a search field.');
    Exit;
  end;

  if Trim(edtSearchValue.Text) = '' then
  begin
    Log('ERROR: Please enter a search value.');
    Exit;
  end;

  FieldName := cboSearchField.Text;
  SearchValue := edtSearchValue.Text;
  SearchMode := cboSearchMode.ItemIndex;
  CaseInsensitive := chkCaseInsensitive.Checked;

  if CaseInsensitive then
    SearchValue := LowerCase(SearchValue);

  // Manual search to support all modes (Contains, Exact, Starts, Ends)
  Found := False;
  FDataSet.First;

  while not FDataSet.EOF do
  begin
    FieldValue := FDataSet.FieldByName(FieldName).AsString;
    if CaseInsensitive then
      FieldValue := LowerCase(FieldValue);

    case SearchMode of
      0: // Contains
        Found := Pos(SearchValue, FieldValue) > 0;
      1: // Exact Match
        Found := FieldValue = SearchValue;
      2: // Starts With
        Found := AnsiStartsStr(SearchValue, FieldValue);
      3: // Ends With
        Found := AnsiEndsStr(SearchValue, FieldValue);
    end;

    if Found then
      Break;

    FDataSet.Next;
  end;

  if Found then
  begin
    Log(Format('LOCATE [%s]: Found "%s" in field [%s] at record %d',
      [cboSearchMode.Text, edtSearchValue.Text, FieldName, FDataSet.RecNo]));

    // Store for "Find Next" functionality
    FLastSearchField := FieldName;
    FLastSearchValue := edtSearchValue.Text;
    FLastSearchMode := SearchMode;
    btnLocateNext.Enabled := True;
  end
  else
  begin
    Log(Format('LOCATE [%s]: No match found for "%s" in field [%s]',
      [cboSearchMode.Text, edtSearchValue.Text, FieldName]));
    FDataSet.First;
    btnLocateNext.Enabled := False;
  end;
end;

procedure TfrmMain.btnLocateNextClick(Sender: TObject);
var
  Found: Boolean;
  StartRecNo, CheckedCount: Integer;
  SearchValue, FieldValue: string;
  CaseInsensitive, Wrapped: Boolean;
begin
  // Find the next occurrence after the current record
  if FLastSearchField = '' then Exit;

  StartRecNo := FDataSet.RecNo;
  SearchValue := FLastSearchValue;
  CaseInsensitive := chkCaseInsensitive.Checked;

  if CaseInsensitive then
    SearchValue := LowerCase(SearchValue);

  // Move to next record before searching
  FDataSet.Next;
  if FDataSet.EOF then
  begin
    Log('LOCATE NEXT: Reached end of dataset. Wrapping to beginning.');
    FDataSet.First;
  end;

  // Search from current position, count records to avoid infinite loop
  Found := False;
  Wrapped := False;
  CheckedCount := 0;

  while CheckedCount < FDataSet.RecordCount do
  begin
    // Skip the starting position until we've wrapped
    if FDataSet.RecNo = StartRecNo then
    begin
      if Wrapped then
        Break;  // We've checked everything
    end;

    FieldValue := FDataSet.FieldByName(FLastSearchField).AsString;
    if CaseInsensitive then
      FieldValue := LowerCase(FieldValue);

    case FLastSearchMode of
      0: // Contains
        Found := Pos(SearchValue, FieldValue) > 0;
      1: // Exact Match
        Found := FieldValue = SearchValue;
      2: // Starts With
        Found := AnsiStartsStr(SearchValue, FieldValue);
      3: // Ends With
        Found := AnsiEndsStr(SearchValue, FieldValue);
    end;

    if Found and (FDataSet.RecNo <> StartRecNo) then
      Break;

    Found := False;  // Reset if we found but it's the start position
    Inc(CheckedCount);
    FDataSet.Next;
    if FDataSet.EOF then
    begin
      FDataSet.First;
      Wrapped := True;
    end;
  end;

  if Found then
  begin
    Log(Format('LOCATE NEXT: Found at record %d', [FDataSet.RecNo]));
  end
  else
  begin
    Log('LOCATE NEXT: No more matches found.');
    // Return to original position
    FDataSet.RecNo := StartRecNo;
  end;
end;

procedure TfrmMain.btnApplyFilterClick(Sender: TObject);
var
  FieldName, Value, WhereClause, NewSQL: string;
  TotalCount: Integer;
begin
  // Filter displays only records matching the criteria using SQL WHERE
  if cboFilterField.ItemIndex < 0 then
  begin
    Log('ERROR: Please select a filter field.');
    Exit;
  end;

  FieldName := cboFilterField.Text;
  Value := Trim(edtFilterValue.Text);

  if Value = '' then
  begin
    Log('ERROR: Please enter a filter value.');
    Exit;
  end;

  // Escape single quotes in value for SQL
  Value := StringReplace(Value, '''', '''''', [rfReplaceAll]);

  // Build SQL WHERE clause based on selected operator
  // SQLite uses % as wildcard for LIKE, and LIKE is case-insensitive for ASCII
  case cboFilterOperator.ItemIndex of
    0: // Contains
      WhereClause := Format('%s LIKE ''%%%s%%''', [FieldName, Value]);
    1: // Equals
      WhereClause := Format('%s = ''%s''', [FieldName, Value]);
    2: // Starts with
      WhereClause := Format('%s LIKE ''%s%%''', [FieldName, Value]);
    3: // Ends with
      WhereClause := Format('%s LIKE ''%%%s''', [FieldName, Value]);
    4: // Greater than (for numeric fields)
      WhereClause := Format('%s > %s', [FieldName, Value]);
    5: // Less than (for numeric fields)
      WhereClause := Format('%s < %s', [FieldName, Value]);
  else
    WhereClause := Format('%s LIKE ''%%%s%%''', [FieldName, Value]);
  end;

  // Get total count before filtering
  TotalCount := FDataSet.RecordCount;

  // Build new SQL with WHERE clause
  NewSQL := GetBaseSQL + ' WHERE ' + WhereClause;

  // Add ORDER BY if sorting is active
  if FSortColumn <> '' then
  begin
    if FSortDirection = sdAscending then
      NewSQL := NewSQL + Format(' ORDER BY %s ASC', [FSortColumn])
    else
      NewSQL := NewSQL + Format(' ORDER BY %s DESC', [FSortColumn]);
  end;

  // Apply the filter via SQL
  FDataSet.Close;
  FDataSet.SQL.Text := NewSQL;
  FDataSet.Open;

  FFilterActive := True;
  Log(Format('FILTER: Applied WHERE %s', [WhereClause]));
  Log(Format('  Showing %d of %d records', [FDataSet.RecordCount, TotalCount]));
  UpdateStatusBar;
end;

procedure TfrmMain.btnClearFilterClick(Sender: TObject);
var
  NewSQL: string;
begin
  // Remove the filter by restoring base SQL
  NewSQL := GetBaseSQL;

  // Preserve sorting if active
  if FSortColumn <> '' then
  begin
    if FSortDirection = sdAscending then
      NewSQL := NewSQL + Format(' ORDER BY %s ASC', [FSortColumn])
    else
      NewSQL := NewSQL + Format(' ORDER BY %s DESC', [FSortColumn]);
  end;

  FDataSet.Close;
  FDataSet.SQL.Text := NewSQL;
  FDataSet.Open;

  FFilterActive := False;
  Log('FILTER: Cleared - showing all records');
  Log(Format('  Total: %d records', [FDataSet.RecordCount]));
  UpdateStatusBar;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
var
  OldSQL: string;
begin
  // Refresh data from database while preserving current SQL (filter + sort)
  OldSQL := FDataSet.SQL.Text;

  FDataSet.Close;
  FDataSet.SQL.Text := OldSQL;
  FDataSet.Open;

  Log('Data refreshed from database');
  Log(Format('  Total: %d records', [FDataSet.RecordCount]));
  UpdateStatusBar;
end;

procedure TfrmMain.cboSortFieldChange(Sender: TObject);
begin
  ApplySorting;
end;

procedure TfrmMain.cboSortDirectionChange(Sender: TObject);
begin
  ApplySorting;
end;

procedure TfrmMain.ApplySorting;
var
  SortField, Direction, NewSQL: string;
begin
  // Apply sorting via SQL ORDER BY clause
  // This is more efficient than client-side sorting for large datasets
  if cboSortField.ItemIndex <= 0 then
  begin
    // No sorting - use base SQL
    NewSQL := GetBaseSQL;
    FSortColumn := '';
    FSortDirection := sdNone;
  end
  else
  begin
    SortField := cboSortField.Text;
    if cboSortDirection.ItemIndex = 0 then
      Direction := 'ASC'
    else
      Direction := 'DESC';

    NewSQL := GetBaseSQL + Format(' ORDER BY %s %s', [SortField, Direction]);
    FSortColumn := SortField;
    if cboSortDirection.ItemIndex = 0 then
      FSortDirection := sdAscending
    else
      FSortDirection := sdDescending;
  end;

  // Preserve filter state while changing SQL
  FDataSet.Close;
  FDataSet.SQL.Text := NewSQL;
  FDataSet.Open;

  if FSortColumn <> '' then
    Log(Format('SORT: %s %s', [FSortColumn,
      IfThen(FSortDirection = sdAscending, 'ASC', 'DESC')]))
  else
    Log('SORT: Cleared');

  UpdateStatusBar;
end;

procedure TfrmMain.DBGrid1TitleClick(Column: TColumn);
var
  ClickedField: string;
begin
  // Toggle sort direction when clicking column headers
  ClickedField := Column.FieldName;

  if FSortColumn = ClickedField then
  begin
    // Toggle direction
    case FSortDirection of
      sdNone, sdDescending: FSortDirection := sdAscending;
      sdAscending: FSortDirection := sdDescending;
    end;
  end
  else
  begin
    // New column - start with ascending
    FSortColumn := ClickedField;
    FSortDirection := sdAscending;
  end;

  // Update combo boxes to reflect the change
  cboSortField.ItemIndex := cboSortField.Items.IndexOf(FSortColumn);
  if FSortDirection = sdAscending then
    cboSortDirection.ItemIndex := 0
  else
    cboSortDirection.ItemIndex := 1;

  ApplySorting;
end;

procedure TfrmMain.edtSearchValueKeyPress(Sender: TObject; var Key: char);
begin
  // Trigger search on Enter key
  if Key = #13 then
  begin
    Key := #0;  // Consume the key
    btnLocateClick(Sender);
  end;
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  Log('Log cleared');
end;

procedure TfrmMain.UpdateStatusBar;
begin
  if FDataSet.Active then
  begin
    if FFilterActive then
      pnlStatus.Caption := Format('Showing %d records (filtered) | Sort: %s',
        [FDataSet.RecordCount,
         IfThen(FSortColumn <> '', FSortColumn + ' ' +
           IfThen(FSortDirection = sdAscending, 'ASC', 'DESC'), 'None')])
    else
      pnlStatus.Caption := Format('Total: %d records | Sort: %s',
        [FDataSet.RecordCount,
         IfThen(FSortColumn <> '', FSortColumn + ' ' +
           IfThen(FSortDirection = sdAscending, 'ASC', 'DESC'), 'None')]);
  end
  else
    pnlStatus.Caption := 'Dataset closed';

  lblRecordCount.Caption := Format('Records: %d', [FDataSet.RecordCount]);
end;

procedure TfrmMain.Log(const AMessage: string);
begin
  if AMessage = '' then
    memoLog.Lines.Add('')
  else
    memoLog.Lines.Add(Format('[%s] %s',
      [FormatDateTime('hh:nn:ss', Now), AMessage]));

  memoLog.SelStart := Length(memoLog.Text);
end;

end.
