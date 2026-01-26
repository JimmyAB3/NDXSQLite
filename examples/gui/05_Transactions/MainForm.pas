{===============================================================================
  NDXSQLite GUI Example - Transaction Management
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - Explicit transaction control (BEGIN, COMMIT, ROLLBACK)
  - Savepoints for partial rollback within transactions
  - Visual transaction state feedback
  - Error recovery with rollback
  - WAL mode for concurrent access

  This example provides a hands-on interface for understanding SQLite
  transaction semantics. Users can start transactions, create savepoints,
  make changes, and selectively commit or rollback to specific points.

  Key Concepts:
  - Transactions group multiple operations into atomic units
  - Savepoints allow nested "sub-transactions" within a main transaction
  - Rollback undoes all changes since the last commit (or savepoint)
  - WAL mode enables concurrent readers during write transactions
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, DBCtrls, DB, Spin,
  ndxsqlitedataset, ndxsqlitedatabase;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    btnBeginTransaction: TButton;
    btnCommit: TButton;
    btnRollback: TButton;
    btnSavepoint: TButton;
    btnRollbackToSavepoint: TButton;
    btnReleaseSavepoint: TButton;
    btnInsertRow: TButton;
    btnUpdateRandom: TButton;
    btnDeleteLast: TButton;
    btnClearLog: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    edtSavepointName: TEdit;
    gbTransactionControl: TGroupBox;
    gbSavepoints: TGroupBox;
    gbDataOperations: TGroupBox;
    lblSavepointName: TLabel;
    lblTransactionStatus: TLabel;
    lblSavepointList: TLabel;
    lstSavepoints: TListBox;
    memoLog: TMemo;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlStatus: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnBeginTransactionClick(Sender: TObject);
    procedure btnCommitClick(Sender: TObject);
    procedure btnRollbackClick(Sender: TObject);
    procedure btnSavepointClick(Sender: TObject);
    procedure btnRollbackToSavepointClick(Sender: TObject);
    procedure btnReleaseSavepointClick(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure btnUpdateRandomClick(Sender: TObject);
    procedure btnDeleteLastClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
    FDataSet: TNDXSQLiteDataSet;
    FSavepointCounter: Integer;
    { Appends a timestamped message to the activity log. }
    procedure Log(const AMessage: string);
    { Initializes the database connection and creates sample data. }
    procedure InitializeDatabase;
    { Updates the transaction status indicator and button states. }
    procedure UpdateTransactionStatus;
    { Updates the status bar with current record count. }
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
  // Initialize the savepoint counter for generating unique names
  FSavepointCounter := 0;

  // Create the dataset with form as owner for automatic cleanup
  FDataSet := TNDXSQLiteDataSet.Create(Self);
  DataSource1.DataSet := FDataSet;

  // Initialize database and load data
  InitializeDatabase;

  // Set initial UI state
  UpdateTransactionStatus;

  // Welcome message
  Log('Transaction Management Demo started');
  Log('Use the controls to experiment with transactions and savepoints');
  Log('');
  Log('Transaction Basics:');
  Log('  - BEGIN starts a new transaction');
  Log('  - COMMIT saves all changes permanently');
  Log('  - ROLLBACK undoes all changes since BEGIN');
  Log('');
  Log('Savepoint Basics:');
  Log('  - Savepoints mark positions within a transaction');
  Log('  - ROLLBACK TO undoes changes back to a savepoint');
  Log('  - RELEASE removes a savepoint (changes kept)');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Ensure clean shutdown - rollback any pending transaction
  if Assigned(FDataSet) then
  begin
    if FDataSet.Database.InTransaction then
    begin
      Log('Rolling back uncommitted transaction on exit');
      FDataSet.Rollback;
    end;
    FDataSet.Close;
    FDataSet.CloseDatabase;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Warn user if there's an active transaction with uncommitted changes
  if Assigned(FDataSet) and FDataSet.Database.InTransaction then
  begin
    case MessageDlg('Uncommitted Transaction',
      'There is an active transaction with uncommitted changes.' + LineEnding +
      'Do you want to commit before closing?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          FDataSet.Commit;
          Log('Transaction committed on exit');
          CanClose := True;
        end;
      mrNo:
        begin
          FDataSet.Rollback;
          Log('Transaction rolled back on exit');
          CanClose := True;
        end;
      mrCancel:
        CanClose := False;
    end;
  end
  else
    CanClose := True;
end;

procedure TfrmMain.InitializeDatabase;
var
  DBPath: string;
begin
  // Create database in the executable directory
  DBPath := ExtractFilePath(ParamStr(0)) + 'transactions_demo.db';

  // Open database with WAL mode enabled for better concurrency
  FDataSet.OpenDatabase(DBPath);

  // Enable WAL mode - allows concurrent reads during writes
  // This is optional but recommended for multi-user scenarios
  FDataSet.SQL.Text := 'PRAGMA journal_mode=WAL';
  FDataSet.ExecSQL;

  // Create the accounts table - a typical scenario for transactions
  FDataSet.SQL.Text := 'CREATE TABLE IF NOT EXISTS accounts (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'account_name TEXT NOT NULL, ' +
    'balance REAL DEFAULT 0.0, ' +
    'last_modified TEXT DEFAULT (datetime(''now'', ''localtime''))' +
    ')';
  FDataSet.ExecSQL;

  // Check if sample data is needed
  FDataSet.SQL.Text := 'SELECT COUNT(*) FROM accounts';
  FDataSet.Open;
  if FDataSet.Fields[0].AsInteger = 0 then
  begin
    FDataSet.Close;
    // Insert sample accounts for demonstration
    FDataSet.SQL.Text := 'INSERT INTO accounts (account_name, balance) VALUES ' +
      '(''Checking Account'', 1000.00), ' +
      '(''Savings Account'', 5000.00), ' +
      '(''Investment Fund'', 10000.00), ' +
      '(''Emergency Fund'', 2500.00), ' +
      '(''Vacation Savings'', 750.00)';
    FDataSet.ExecSQL;
  end
  else
    FDataSet.Close;

  // Configure dataset for browsing and editing
  FDataSet.TableName := 'accounts';
  FDataSet.PrimaryKey := 'id';
  FDataSet.SQL.Text := 'SELECT id, account_name, balance, last_modified FROM accounts ORDER BY id';
  FDataSet.Open;

  UpdateStatusBar;
  Log('Database initialized: ' + DBPath);
end;

procedure TfrmMain.btnBeginTransactionClick(Sender: TObject);
begin
  // Start a new explicit transaction
  // All subsequent changes will be held until COMMIT or ROLLBACK
  if FDataSet.Database.InTransaction then
  begin
    Log('ERROR: Transaction already active. Commit or rollback first.');
    Exit;
  end;

  FDataSet.StartTransaction;
  lstSavepoints.Clear;
  FSavepointCounter := 0;

  Log('BEGIN TRANSACTION');
  Log('  Changes are now buffered until COMMIT or ROLLBACK');
  UpdateTransactionStatus;
end;

procedure TfrmMain.btnCommitClick(Sender: TObject);
begin
  // Commit the current transaction - make all changes permanent
  if not FDataSet.Database.InTransaction then
  begin
    Log('ERROR: No active transaction to commit.');
    Exit;
  end;

  FDataSet.Commit;
  lstSavepoints.Clear;

  Log('COMMIT');
  Log('  All changes have been saved permanently');
  UpdateTransactionStatus;
  UpdateStatusBar;
end;

procedure TfrmMain.btnRollbackClick(Sender: TObject);
var
  RecordsBefore: Integer;
begin
  // Rollback the entire transaction - discard all changes
  if not FDataSet.Database.InTransaction then
  begin
    Log('ERROR: No active transaction to rollback.');
    Exit;
  end;

  RecordsBefore := FDataSet.RecordCount;
  FDataSet.Rollback;
  lstSavepoints.Clear;

  // Refresh the dataset to show the reverted state
  FDataSet.Close;
  FDataSet.Open;

  Log('ROLLBACK');
  Log(Format('  All changes discarded. Records: %d -> %d',
    [RecordsBefore, FDataSet.RecordCount]));
  UpdateTransactionStatus;
  UpdateStatusBar;
end;

procedure TfrmMain.btnSavepointClick(Sender: TObject);
var
  SavepointName: string;
begin
  // Create a savepoint within the current transaction
  if not FDataSet.Database.InTransaction then
  begin
    Log('ERROR: Cannot create savepoint without an active transaction.');
    Log('  Start a transaction first with BEGIN TRANSACTION.');
    Exit;
  end;

  // Generate or use provided savepoint name
  if Trim(edtSavepointName.Text) <> '' then
    SavepointName := Trim(edtSavepointName.Text)
  else
  begin
    Inc(FSavepointCounter);
    SavepointName := 'SP_' + IntToStr(FSavepointCounter);
  end;

  // Create the savepoint using the dataset's method
  FDataSet.Database.Savepoint(SavepointName);
  lstSavepoints.Items.Add(SavepointName);

  Log('SAVEPOINT ' + SavepointName);
  Log('  Marked current state. Can rollback to this point later.');
  edtSavepointName.Clear;
end;

procedure TfrmMain.btnRollbackToSavepointClick(Sender: TObject);
var
  SavepointName: string;
  Index: Integer;
begin
  // Rollback to a specific savepoint - discard changes made after it
  if lstSavepoints.ItemIndex < 0 then
  begin
    Log('ERROR: Please select a savepoint from the list.');
    Exit;
  end;

  SavepointName := lstSavepoints.Items[lstSavepoints.ItemIndex];
  Index := lstSavepoints.ItemIndex;

  FDataSet.Database.RollbackToSavepoint(SavepointName);

  // Remove savepoints created after this one (they're invalidated)
  while lstSavepoints.Count > Index + 1 do
    lstSavepoints.Items.Delete(lstSavepoints.Count - 1);

  // Refresh dataset to show reverted state
  FDataSet.Close;
  FDataSet.Open;

  Log('ROLLBACK TO SAVEPOINT ' + SavepointName);
  Log('  Changes after this savepoint have been discarded');
  UpdateStatusBar;
end;

procedure TfrmMain.btnReleaseSavepointClick(Sender: TObject);
var
  SavepointName: string;
  Index: Integer;
begin
  // Release a savepoint - removes the marker but keeps changes
  if lstSavepoints.ItemIndex < 0 then
  begin
    Log('ERROR: Please select a savepoint from the list.');
    Exit;
  end;

  SavepointName := lstSavepoints.Items[lstSavepoints.ItemIndex];
  Index := lstSavepoints.ItemIndex;

  FDataSet.Database.ReleaseSavepoint(SavepointName);

  // Remove this and all subsequent savepoints
  while lstSavepoints.Count > Index do
    lstSavepoints.Items.Delete(lstSavepoints.Count - 1);

  Log('RELEASE SAVEPOINT ' + SavepointName);
  Log('  Savepoint removed. Changes between savepoints are preserved.');
end;

procedure TfrmMain.btnInsertRowClick(Sender: TObject);
var
  NewName: string;
  NewBalance: Double;
begin
  // Insert a new row to demonstrate transaction behavior
  NewName := 'Account_' + FormatDateTime('hhnnss', Now);
  NewBalance := Random(10000) / 100;

  FDataSet.Append;
  FDataSet.FieldByName('account_name').AsString := NewName;
  FDataSet.FieldByName('balance').AsFloat := NewBalance;
  FDataSet.Post;

  Log(Format('INSERT: %s with balance $%.2f', [NewName, NewBalance]));
  if FDataSet.Database.InTransaction then
    Log('  (Change is pending until COMMIT)')
  else
    Log('  (Auto-committed - no active transaction)');
  UpdateStatusBar;
end;

procedure TfrmMain.btnUpdateRandomClick(Sender: TObject);
var
  OldBalance, NewBalance, Delta: Double;
  AccountName: string;
begin
  // Update a random row's balance
  if FDataSet.RecordCount = 0 then
  begin
    Log('ERROR: No records to update.');
    Exit;
  end;

  // Move to a random record
  FDataSet.RecNo := Random(FDataSet.RecordCount) + 1;
  AccountName := FDataSet.FieldByName('account_name').AsString;
  OldBalance := FDataSet.FieldByName('balance').AsFloat;

  // Apply a random change (+/- up to $500)
  Delta := (Random(10000) - 5000) / 100;
  NewBalance := OldBalance + Delta;

  FDataSet.Edit;
  FDataSet.FieldByName('balance').AsFloat := NewBalance;
  FDataSet.FieldByName('last_modified').AsString :=
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  FDataSet.Post;

  Log(Format('UPDATE: %s balance $%.2f -> $%.2f (delta: %+.2f)',
    [AccountName, OldBalance, NewBalance, Delta]));
  if FDataSet.Database.InTransaction then
    Log('  (Change is pending until COMMIT)');
end;

procedure TfrmMain.btnDeleteLastClick(Sender: TObject);
var
  AccountName: string;
begin
  // Delete the last record
  if FDataSet.RecordCount = 0 then
  begin
    Log('ERROR: No records to delete.');
    Exit;
  end;

  FDataSet.Last;
  AccountName := FDataSet.FieldByName('account_name').AsString;
  FDataSet.Delete;

  Log('DELETE: ' + AccountName);
  if FDataSet.Database.InTransaction then
    Log('  (Deletion is pending until COMMIT)')
  else
    Log('  (Auto-committed - no active transaction)');
  UpdateStatusBar;
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  Log('Log cleared');
end;

procedure TfrmMain.UpdateTransactionStatus;
begin
  // Update visual indicators based on transaction state
  if FDataSet.Database.InTransaction then
  begin
    lblTransactionStatus.Caption := 'TRANSACTION ACTIVE';
    lblTransactionStatus.Font.Color := clRed;
    lblTransactionStatus.Font.Style := [fsBold];
    btnBeginTransaction.Enabled := False;
    btnCommit.Enabled := True;
    btnRollback.Enabled := True;
    gbSavepoints.Enabled := True;
  end
  else
  begin
    lblTransactionStatus.Caption := 'No Active Transaction';
    lblTransactionStatus.Font.Color := clGreen;
    lblTransactionStatus.Font.Style := [];
    btnBeginTransaction.Enabled := True;
    btnCommit.Enabled := False;
    btnRollback.Enabled := False;
    gbSavepoints.Enabled := False;
  end;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  if FDataSet.Active then
    pnlStatus.Caption := Format('Records: %d | %s',
      [FDataSet.RecordCount,
       IfThen(FDataSet.Database.InTransaction, 'Transaction Active', 'Auto-Commit Mode')])
  else
    pnlStatus.Caption := 'Dataset closed';
end;

procedure TfrmMain.Log(const AMessage: string);
begin
  if AMessage = '' then
    memoLog.Lines.Add('')
  else
    memoLog.Lines.Add(Format('[%s] %s',
      [FormatDateTime('hh:nn:ss', Now), AMessage]));

  // Auto-scroll to the latest entry
  memoLog.SelStart := Length(memoLog.Text);
end;

end.
