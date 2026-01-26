{===============================================================================
  NDXSQLite GUI Example - Cached Updates
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - CachedUpdates mode for in-memory modifications
  - ApplyUpdates to commit all changes to database
  - CancelUpdates to discard pending changes
  - Visual feedback for modified state
  - Transaction-like behavior without explicit transactions
  - Cross-platform GUI (Windows, Linux, macOS)

  This example shows how to use the CachedUpdates feature to batch multiple
  changes before committing them to the database. This provides:
  - Better performance for multiple edits (single database write)
  - Ability to cancel all changes at once
  - Transaction-like semantics without explicit BEGIN/COMMIT
  - Visual indication of pending changes for user awareness

  Key difference from direct mode:
  - Direct mode: Each edit immediately writes to the database
  - Cached mode: Edits are held in memory until ApplyUpdates is called
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, DBCtrls,
  StdCtrls, ExtCtrls, DB, StrUtils,
  ndxsqlitedataset;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    pnlTop: TPanel;
    pnlStatus: TPanel;
    pnlButtons: TPanel;
    lblInfo: TLabel;
    lblPendingChanges: TLabel;
    btnApplyUpdates: TButton;
    btnCancelUpdates: TButton;
    btnRefresh: TButton;
    chkCachedUpdates: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnApplyUpdatesClick(Sender: TObject);
    procedure btnCancelUpdatesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure chkCachedUpdatesClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure DBGrid1CellClick(Column: TColumn);
  private
    FDataSet: TNDXSQLiteDataSet;
    FPendingInserts: Integer;
    FPendingUpdates: Integer;
    FPendingDeletes: Integer;
    { Initializes database connection and creates sample employee data. }
    procedure InitializeDatabase;
    { Updates the label showing count of pending insert/update/delete operations. }
    procedure UpdatePendingChangesDisplay;
    { Enables or disables Apply/Cancel buttons based on current state. }
    procedure UpdateButtonStates;
    { Updates the status bar with record position and current mode. }
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
  // Initialize counters for tracking pending changes
  // These are used for display purposes to show the user what will be committed
  FPendingInserts := 0;
  FPendingUpdates := 0;
  FPendingDeletes := 0;

  // Create the dataset with the form as owner
  FDataSet := TNDXSQLiteDataSet.Create(Self);
  DataSource1.DataSet := FDataSet;

  // Set up the database and load initial data
  InitializeDatabase;

  // Configure initial button states based on dataset state
  UpdateButtonStates;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Clean up database resources
  // Note: Any uncommitted changes will be lost if user confirmed close
  if Assigned(FDataSet) then
  begin
    FDataSet.Close;
    FDataSet.CloseDatabase;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Warn user about unsaved changes before closing the application
  // This is important UX for cached updates mode where changes aren't auto-saved
  if FDataSet.CachedUpdates and FDataSet.Modified then
  begin
    case MessageDlg('Unsaved Changes',
      'You have pending changes that have not been saved.' + LineEnding +
      'Do you want to apply them before closing?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          // User wants to save - attempt to apply updates
          try
            FDataSet.ApplyUpdates;
            CanClose := True;
          except
            on E: Exception do
            begin
              // Save failed - inform user and prevent close
              MessageDlg('Error', 'Failed to apply updates: ' + E.Message,
                mtError, [mbOK], 0);
              CanClose := False;
            end;
          end;
        end;
      mrNo:
        // User chose to discard changes - allow close
        CanClose := True;
      mrCancel:
        // User cancelled - stay in the application
        CanClose := False;
    end;
  end
  else
    // No pending changes - safe to close
    CanClose := True;
end;

procedure TfrmMain.InitializeDatabase;
var
  DBPath: string;
begin
  // Create database in executable directory
  DBPath := ExtractFilePath(ParamStr(0)) + 'cached_updates_demo.db';

  // Open the SQLite database
  FDataSet.OpenDatabase(DBPath);

  // Create employees table with various field types for demonstration
  // Includes auto-increment ID, required fields, and default values
  FDataSet.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS employees (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'first_name TEXT NOT NULL, ' +
    'last_name TEXT NOT NULL, ' +
    'email TEXT, ' +
    'department TEXT, ' +
    'salary REAL DEFAULT 0, ' +
    'hire_date TEXT DEFAULT (date(''now'', ''localtime''))' +
    ')';
  FDataSet.ExecSQL;

  // Populate with sample employee data if table is empty
  FDataSet.SQL.Text := 'SELECT COUNT(*) FROM employees';
  FDataSet.Open;
  if FDataSet.Fields[0].AsInteger = 0 then
  begin
    FDataSet.Close;
    // Insert diverse sample data representing different departments
    FDataSet.SQL.Text :=
      'INSERT INTO employees (first_name, last_name, email, department, salary) VALUES ' +
      '(''Alice'', ''Johnson'', ''alice.j@company.com'', ''Engineering'', 75000), ' +
      '(''Bob'', ''Smith'', ''bob.s@company.com'', ''Marketing'', 65000), ' +
      '(''Carol'', ''Williams'', ''carol.w@company.com'', ''Engineering'', 80000), ' +
      '(''David'', ''Brown'', ''david.b@company.com'', ''Sales'', 70000), ' +
      '(''Eve'', ''Davis'', ''eve.d@company.com'', ''HR'', 60000), ' +
      '(''Frank'', ''Miller'', ''frank.m@company.com'', ''Engineering'', 85000), ' +
      '(''Grace'', ''Wilson'', ''grace.w@company.com'', ''Finance'', 72000), ' +
      '(''Henry'', ''Moore'', ''henry.m@company.com'', ''Marketing'', 68000)';
    FDataSet.ExecSQL;
  end
  else
    FDataSet.Close;

  // Enable cached updates mode by default for this demo
  // In cached mode, changes are held in memory until ApplyUpdates is called
  FDataSet.CachedUpdates := True;
  chkCachedUpdates.Checked := True;

  // Configure dataset for editing operations
  // TableName and PrimaryKey are required for auto-generated SQL statements
  FDataSet.TableName := 'employees';
  FDataSet.PrimaryKey := 'id';
  FDataSet.SQL.Text := 'SELECT id, first_name, last_name, email, department, salary, hire_date FROM employees ORDER BY last_name, first_name';
  FDataSet.Open;

  // Configure grid columns with appropriate widths and read-only settings
  DBGrid1.Columns.Clear;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'id';
    Title.Caption := 'ID';
    Width := 40;
    ReadOnly := True;  // Auto-increment field
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'first_name';
    Title.Caption := 'First Name';
    Width := 100;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'last_name';
    Title.Caption := 'Last Name';
    Width := 100;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'email';
    Title.Caption := 'Email';
    Width := 180;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'department';
    Title.Caption := 'Department';
    Width := 100;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'salary';
    Title.Caption := 'Salary';
    Width := 80;
  end;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'hire_date';
    Title.Caption := 'Hire Date';
    Width := 100;
    ReadOnly := True;  // Auto-populated with current date
  end;

  // Update UI elements
  lblInfo.Caption := 'Database: ' + ExtractFileName(DBPath);
  UpdateStatusBar;
  UpdatePendingChangesDisplay;
end;

procedure TfrmMain.btnApplyUpdatesClick(Sender: TObject);
begin
  // Commit all pending changes to the database
  // This writes all inserts, updates, and deletes in a single operation
  try
    Screen.Cursor := crHourGlass;  // Visual feedback for potentially long operation
    try
      FDataSet.ApplyUpdates;
      // Reset pending counters after successful commit
      FPendingInserts := 0;
      FPendingUpdates := 0;
      FPendingDeletes := 0;
      UpdatePendingChangesDisplay;
      UpdateButtonStates;
      MessageDlg('Success', 'All changes have been saved to the database.',
        mtInformation, [mbOK], 0);
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      MessageDlg('Error', 'Failed to apply updates: ' + E.Message,
        mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.btnCancelUpdatesClick(Sender: TObject);
begin
  // Discard all pending changes and reload original data
  // This provides an "undo all" functionality
  if MessageDlg('Cancel Changes',
    'Are you sure you want to discard all pending changes?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FDataSet.CancelUpdates;
    // Reset all pending counters
    FPendingInserts := 0;
    FPendingUpdates := 0;
    FPendingDeletes := 0;
    UpdatePendingChangesDisplay;
    UpdateButtonStates;
  end;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  // Reload data from database, warning about pending changes
  if FDataSet.Modified then
  begin
    if MessageDlg('Pending Changes',
      'Refreshing will discard all pending changes. Continue?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;

  // Reload data from database using Close/Open pattern
  // This ensures proper state management
  FDataSet.Close;
  FDataSet.Open;
  // Reset pending counters since we reloaded from database
  FPendingInserts := 0;
  FPendingUpdates := 0;
  FPendingDeletes := 0;
  UpdatePendingChangesDisplay;
  UpdateButtonStates;
end;

procedure TfrmMain.chkCachedUpdatesClick(Sender: TObject);
begin
  // Toggle between cached and direct update modes
  // Switching modes with pending changes requires special handling
  if FDataSet.Modified and not chkCachedUpdates.Checked then
  begin
    // Turning off cached mode with pending changes - must apply first
    if MessageDlg('Pending Changes',
      'Disabling cached updates will apply all pending changes. Continue?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      // User cancelled - restore checkbox state
      chkCachedUpdates.Checked := True;
      Exit;
    end;
    // Apply pending changes before switching to direct mode
    FDataSet.ApplyUpdates;
  end;

  // Switch the update mode
  FDataSet.CachedUpdates := chkCachedUpdates.Checked;
  // Reset counters since mode changed
  FPendingInserts := 0;
  FPendingUpdates := 0;
  FPendingDeletes := 0;
  UpdatePendingChangesDisplay;
  UpdateButtonStates;

  // Update info label to show current mode
  if FDataSet.CachedUpdates then
    lblInfo.Caption := 'Mode: Cached Updates (changes held in memory)'
  else
    lblInfo.Caption := 'Mode: Direct Updates (changes saved immediately)';
end;

procedure TfrmMain.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // Called on any data or navigation change
  UpdateStatusBar;
  UpdateButtonStates;
end;

procedure TfrmMain.DBGrid1CellClick(Column: TColumn);
begin
  // Update status when user clicks a cell
  UpdateStatusBar;
end;

procedure TfrmMain.UpdatePendingChangesDisplay;
var
  HasChanges: Boolean;
begin
  // Check if there are any uncommitted changes
  HasChanges := FDataSet.Modified or (FPendingInserts + FPendingUpdates + FPendingDeletes > 0);

  if HasChanges then
  begin
    // Display pending change counts with warning styling
    lblPendingChanges.Caption := Format('Pending: %d insert(s), %d update(s), %d delete(s)',
      [FPendingInserts, FPendingUpdates, FPendingDeletes]);
    lblPendingChanges.Font.Color := clRed;     // Red text for visibility
    lblPendingChanges.Font.Style := [fsBold];  // Bold for emphasis
  end
  else
  begin
    // No changes - show clean state with positive styling
    lblPendingChanges.Caption := 'No pending changes';
    lblPendingChanges.Font.Color := clGreen;   // Green indicates saved state
    lblPendingChanges.Font.Style := [];        // Normal weight
  end;
end;

procedure TfrmMain.UpdateButtonStates;
var
  HasChanges: Boolean;
begin
  // Enable Apply/Cancel buttons only when there are changes to commit
  // and we're in cached updates mode
  HasChanges := FDataSet.Modified;
  btnApplyUpdates.Enabled := HasChanges and FDataSet.CachedUpdates;
  btnCancelUpdates.Enabled := HasChanges and FDataSet.CachedUpdates;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  // Show current record position and update mode
  if FDataSet.Active then
    pnlStatus.Caption := Format('Record %d of %d | %s',
      [FDataSet.RecNo, FDataSet.RecordCount,
       IfThen(FDataSet.CachedUpdates, 'Cached Mode', 'Direct Mode')])
  else
    pnlStatus.Caption := 'Dataset closed';
end;

end.
