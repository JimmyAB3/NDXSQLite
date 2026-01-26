{===============================================================================
  NDXSQLite GUI Example - DBGrid with Navigator
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - TNDXSQLiteDataSet as TDataSet-compatible component
  - DBGrid for displaying and editing data
  - DBNavigator for record navigation and CRUD operations
  - TDataSource as mediator between dataset and controls
  - Cross-platform GUI (Windows, Linux, macOS)

  This example shows the simplest way to use TNDXSQLiteDataSet with standard
  Lazarus data-aware controls. The dataset automatically handles:
  - Opening and creating SQLite databases
  - Executing SQL queries and returning results
  - Insert, Update, and Delete operations via DBNavigator
  - Field type mapping between SQLite and Pascal types
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, DBCtrls,
  StdCtrls, ExtCtrls, DB,
  ndxsqlitedataset;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    pnlTop: TPanel;
    pnlStatus: TPanel;
    lblInfo: TLabel;
    btnRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    FDataSet: TNDXSQLiteDataSet;
    { Initializes the database connection and creates sample data if needed. }
    procedure InitializeDatabase;
    { Updates the status bar with current record position information. }
    procedure UpdateStatusBar;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Create the dataset instance with the form as owner for automatic cleanup
  FDataSet := TNDXSQLiteDataSet.Create(Self);

  // Connect the dataset to the DataSource, which acts as a mediator between
  // the dataset and data-aware controls (DBGrid, DBNavigator)
  DataSource1.DataSet := FDataSet;

  // Initialize database connection and load initial data
  InitializeDatabase;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Properly close the dataset and release database resources
  // Note: FDataSet is owned by the form and will be freed automatically,
  // but we explicitly close connections to ensure clean shutdown
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
  // Place the database file in the same directory as the executable
  // This ensures the example works without additional configuration
  DBPath := ExtractFilePath(ParamStr(0)) + 'dbgrid_demo.db';

  // Open (or create) the SQLite database file
  FDataSet.OpenDatabase(DBPath);

  // Create the customers table using standard SQL DDL
  // IF NOT EXISTS ensures idempotent execution
  FDataSet.SQL.Text := 'CREATE TABLE IF NOT EXISTS customers (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +  // Auto-increment primary key
    'name TEXT NOT NULL, ' +                     // Required customer name
    'email TEXT, ' +                             // Optional email address
    'phone TEXT, ' +                             // Optional phone number
    'city TEXT, ' +                              // Optional city
    'created_at TEXT DEFAULT (datetime(''now'', ''localtime''))' +  // Auto-timestamp
    ')';
  FDataSet.ExecSQL;

  // Check if the table is empty and populate with sample data for demonstration
  FDataSet.SQL.Text := 'SELECT COUNT(*) FROM customers';
  FDataSet.Open;
  if FDataSet.Fields[0].AsInteger = 0 then
  begin
    FDataSet.Close;
    // Insert sample customer records using a single multi-value INSERT
    FDataSet.SQL.Text := 'INSERT INTO customers (name, email, phone, city) VALUES ' +
      '(''John Smith'', ''john.smith@email.com'', ''555-0101'', ''New York''), ' +
      '(''Jane Doe'', ''jane.doe@email.com'', ''555-0102'', ''Los Angeles''), ' +
      '(''Robert Johnson'', ''r.johnson@email.com'', ''555-0103'', ''Chicago''), ' +
      '(''Emily Davis'', ''e.davis@email.com'', ''555-0104'', ''Houston''), ' +
      '(''Michael Wilson'', ''m.wilson@email.com'', ''555-0105'', ''Phoenix'')';
    FDataSet.ExecSQL;
  end
  else
    FDataSet.Close;

  // Configure the dataset for browsing and editing operations
  // TableName and PrimaryKey are required for automatic INSERT/UPDATE/DELETE generation
  FDataSet.TableName := 'customers';
  FDataSet.PrimaryKey := 'id';
  FDataSet.SQL.Text := 'SELECT id, name, email, phone, city, created_at FROM customers ORDER BY name';
  FDataSet.Open;

  // Configure grid columns programmatically for better control over display
  // This approach allows setting column widths, titles, and read-only status
  DBGrid1.Columns.Clear;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'id';
    Title.Caption := 'ID';
    Width := 50;
    ReadOnly := True;  // Auto-increment field should not be editable
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'name';
    Title.Caption := 'Name';
    Width := 150;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'email';
    Title.Caption := 'Email';
    Width := 180;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'phone';
    Title.Caption := 'Phone';
    Width := 100;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'city';
    Title.Caption := 'City';
    Width := 120;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'created_at';
    Title.Caption := 'Created';
    Width := 140;
    ReadOnly := True;  // Timestamp is auto-generated
  end;

  // Display database path and update status bar
  UpdateStatusBar;
  lblInfo.Caption := 'Database: ' + DBPath;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  // Refresh the dataset by closing and reopening
  // This reloads all data from the database, discarding any local changes
  // Note: Use Close/Open instead of Refresh for proper state management
  FDataSet.Close;
  FDataSet.Open;
  UpdateStatusBar;
end;

procedure TfrmMain.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // Called whenever the current record changes or field data is modified
  // Update the status bar to reflect the new position
  UpdateStatusBar;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  // Display current record position in the status panel
  if FDataSet.Active then
    pnlStatus.Caption := Format('Record %d of %d', [FDataSet.RecNo, FDataSet.RecordCount])
  else
    pnlStatus.Caption := 'Dataset closed';
end;

end.
