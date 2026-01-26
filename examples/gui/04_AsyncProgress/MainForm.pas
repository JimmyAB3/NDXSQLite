{===============================================================================
  NDXSQLite GUI Example - Asynchronous Operations with Progress
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - TNDXSQLiteAsyncConnection for non-blocking database operations
  - Progress bar updates during long operations
  - Cancellation support via INDXCancellationToken
  - Responsive UI during background database work
  - Cross-platform GUI (Windows, Linux, macOS)

  This example showcases the async capabilities of NDXSQLite for performing
  long-running database operations without freezing the user interface.
  Key features demonstrated:
  - Asynchronous query execution with callbacks
  - Progress reporting during bulk operations
  - Cancellation tokens for aborting long operations
  - Database backup with progress tracking
  - Proper resource cleanup on form close

  Architecture notes:
  - Main thread handles UI updates via synchronized callbacks
  - Background thread performs database I/O
  - CancellationToken allows cooperative cancellation
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Spin, DB, DBGrids, DBCtrls,
  ndxsqliteasyncconnection, ndxsqliteasynctypes, ndxsqlitecancellation,
  ndxsqlitetypes, ndxsqlitedataset;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    pnlTop: TPanel;
    pnlProgress: TPanel;
    pnlStatus: TPanel;
    lblProgress: TLabel;
    ProgressBar1: TProgressBar;
    btnInsertData: TButton;
    btnQueryData: TButton;
    btnBackup: TButton;
    btnCancel: TButton;
    edtRecordCount: TSpinEdit;
    lblRecordCount: TLabel;
    memoLog: TMemo;
    Splitter1: TSplitter;
    pnlData: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnInsertDataClick(Sender: TObject);
    procedure btnQueryDataClick(Sender: TObject);
    procedure btnBackupClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FConnection: TNDXSQLiteAsyncConnection;
    FDataSet: TNDXSQLiteDataSet;
    FCancellationSource: TNDXCancellationTokenSource;
    FOperationInProgress: Boolean;
    FDBPath: string;
    { Initializes database connection asynchronously. }
    procedure InitializeDatabase;
    { Appends a timestamped message to the log memo. }
    procedure Log(const AMessage: string);
    { Updates UI state based on whether an operation is in progress. }
    procedure SetOperationInProgress(AInProgress: Boolean);
    { Updates the progress bar with current/total values. }
    procedure UpdateProgressBar(ACurrent, ATotal: Integer);

    { Callback invoked when async Open operation completes.
      Signature must match TNDXAsyncCallback. }
    procedure OnOpenComplete(const ASuccess: Boolean; const AError: string);
    { Callback invoked when async insert operation completes.
      Receives row count affected and timing information. }
    procedure OnInsertComplete(const AResult: TNDXAsyncResultInt);
    { Callback invoked when async query operation completes.
      Receives the result dataset. }
    procedure OnQueryComplete(const AResult: TNDXAsyncResultDataSet);
    { Callback invoked periodically during backup for progress updates. }
    procedure OnBackupProgress(ACurrent, ATotal: Integer; const AMessage: string);
    { Callback invoked when async backup operation completes. }
    procedure OnBackupComplete(const ASuccess: Boolean; const AError: string);
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  DateUtils;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Initialize state tracking
  FOperationInProgress := False;
  FCancellationSource := nil;

  // Set up database path in executable directory
  FDBPath := ExtractFilePath(ParamStr(0)) + 'async_demo.db';

  // Create async connection with auto-create flag
  // The second parameter (True) creates the database if it doesn't exist
  FConnection := TNDXSQLiteAsyncConnection.Create(FDBPath, True);

  // Create a standard dataset for displaying query results in the grid
  // Note: Async queries return data that we copy to this synchronous dataset
  FDataSet := TNDXSQLiteDataSet.Create(Self);
  DataSource1.DataSet := FDataSet;

  // Log startup information
  Log('Application started');
  Log('Database: ' + FDBPath);

  // Begin async database initialization
  InitializeDatabase;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Cancel any pending async operations
  if Assigned(FCancellationSource) then
  begin
    FCancellationSource.Cancel;
    FreeAndNil(FCancellationSource);
  end;

  // Wait for all background operations to complete before cleanup
  // Timeout prevents indefinite hang on close
  if Assigned(FConnection) then
  begin
    FConnection.WaitForAllOperations(5000);  // 5 second timeout
    FConnection.Free;
  end;

  // Clean up the display dataset
  if Assigned(FDataSet) then
  begin
    FDataSet.Close;
    FDataSet.CloseDatabase;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Warn user if an async operation is still running
  if FOperationInProgress then
  begin
    if MessageDlg('Operation In Progress',
      'An operation is currently running. Do you want to cancel it and close?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // User confirmed - cancel the operation and allow close
      if Assigned(FCancellationSource) then
        FCancellationSource.Cancel;
      CanClose := True;
    end
    else
      // User wants to wait - prevent close
      CanClose := False;
  end
  else
    CanClose := True;
end;

procedure TfrmMain.InitializeDatabase;
begin
  Log('Opening database asynchronously...');
  SetOperationInProgress(True);

  // Open the database connection asynchronously
  // The callback (OnOpenComplete) will be invoked on the main thread
  // when the operation finishes
  FConnection.OpenAsync(@OnOpenComplete, nil);
end;

procedure TfrmMain.OnOpenComplete(const ASuccess: Boolean; const AError: string);
begin
  // This callback runs on the main thread (synchronized by the async connection)
  if ASuccess then
  begin
    Log('Database opened successfully');

    // Create the test table asynchronously
    // This demonstrates chaining async operations via callbacks
    FConnection.ExecuteNonQueryAsync(
      'CREATE TABLE IF NOT EXISTS benchmark_data (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'name TEXT, ' +
      'value REAL, ' +
      'created_at TEXT DEFAULT (datetime(''now'', ''localtime''))' +
      ')',
      @OnInsertComplete, nil);
  end
  else
  begin
    Log('ERROR: Failed to open database - ' + AError);
    SetOperationInProgress(False);
  end;
end;

procedure TfrmMain.btnInsertDataClick(Sender: TObject);
var
  RecordCount, I, BatchSize: Integer;
  SQL: string;
begin
  RecordCount := edtRecordCount.Value;

  // Validate input
  if RecordCount <= 0 then
  begin
    MessageDlg('Invalid Count', 'Please enter a valid record count.', mtError, [mbOK], 0);
    Exit;
  end;

  Log(Format('Starting insert of %d records...', [RecordCount]));
  SetOperationInProgress(True);

  // Create a cancellation token source for this operation
  // The token allows the user to abort the operation mid-execution
  FCancellationSource := TNDXCancellationTokenSource.Create;

  // Determine batch size for progress updates
  BatchSize := 100;
  if RecordCount < BatchSize then
    BatchSize := RecordCount;

  // Build a transaction-wrapped batch insert for performance
  // Wrapping multiple inserts in a transaction is much faster than
  // individual commits for each row
  SQL := 'BEGIN TRANSACTION; ';
  for I := 1 to RecordCount do
  begin
    SQL := SQL + Format(
      'INSERT INTO benchmark_data (name, value) VALUES (''Item_%d'', %.2f); ',
      [I, Random * 1000]);

    // Update progress bar periodically to show activity
    if (I mod 100 = 0) or (I = RecordCount) then
      UpdateProgressBar(I, RecordCount);
  end;
  SQL := SQL + 'COMMIT;';

  // Execute the batch insert asynchronously
  // The cancellation token is passed to allow abort if needed
  FConnection.ExecuteNonQueryAsync(SQL, @OnInsertComplete, FCancellationSource.Token);
end;

procedure TfrmMain.OnInsertComplete(const AResult: TNDXAsyncResultInt);
begin
  // Clean up operation state
  SetOperationInProgress(False);
  FreeAndNil(FCancellationSource);
  ProgressBar1.Position := 0;

  if AResult.Success then
  begin
    // Log success with performance metrics
    Log(Format('Insert completed: %d rows affected (%d ms)',
      [AResult.Data, AResult.ExecutionTimeMs]));
  end
  else
  begin
    // Check if the failure was due to cancellation
    if Pos('canceled', LowerCase(AResult.ErrorMessage)) > 0 then
      Log('Insert operation was canceled')
    else
      Log('ERROR: Insert failed - ' + AResult.ErrorMessage);
  end;
end;

procedure TfrmMain.btnQueryDataClick(Sender: TObject);
begin
  Log('Querying data asynchronously...');
  SetOperationInProgress(True);

  // Create cancellation token for this query
  FCancellationSource := TNDXCancellationTokenSource.Create;

  // Execute a SELECT query asynchronously
  // Results will be delivered via the OnQueryComplete callback
  // LIMIT clause prevents loading too much data for the demo
  FConnection.ExecuteQueryAsync(
    'SELECT id, name, value, created_at FROM benchmark_data ORDER BY id DESC LIMIT 1000',
    @OnQueryComplete, FCancellationSource.Token);
end;

procedure TfrmMain.OnQueryComplete(const AResult: TNDXAsyncResultDataSet);
begin
  SetOperationInProgress(False);
  FreeAndNil(FCancellationSource);

  if AResult.Success then
  begin
    // Close any existing data in the display dataset
    FDataSet.Close;

    // For GUI display, we use a synchronous dataset
    // Copy the async query results to our display dataset by re-executing
    // the same query synchronously (this is a simple approach for the demo)
    FDataSet.OpenDatabase(FDBPath);
    FDataSet.SQL.Text := 'SELECT id, name, value, created_at FROM benchmark_data ORDER BY id DESC LIMIT 1000';
    FDataSet.Open;

    // Free the async result dataset since we don't need it anymore
    if Assigned(AResult.Data) then
      AResult.Data.Free;

    // Configure grid columns for display
    DBGrid1.Columns.Clear;
    with DBGrid1.Columns.Add do
    begin
      FieldName := 'id';
      Title.Caption := 'ID';
      Width := 60;
    end;
    with DBGrid1.Columns.Add do
    begin
      FieldName := 'name';
      Title.Caption := 'Name';
      Width := 150;
    end;
    with DBGrid1.Columns.Add do
    begin
      FieldName := 'value';
      Title.Caption := 'Value';
      Width := 100;
    end;
    with DBGrid1.Columns.Add do
    begin
      FieldName := 'created_at';
      Title.Caption := 'Created';
      Width := 150;
    end;

    // Log success with performance metrics
    Log(Format('Query completed: %d records loaded (%d ms)',
      [FDataSet.RecordCount, AResult.ExecutionTimeMs]));
  end
  else
  begin
    // Check if the failure was due to cancellation
    if Pos('canceled', LowerCase(AResult.ErrorMessage)) > 0 then
      Log('Query operation was canceled')
    else
      Log('ERROR: Query failed - ' + AResult.ErrorMessage);
  end;
end;

procedure TfrmMain.btnBackupClick(Sender: TObject);
var
  BackupPath: string;
begin
  // Generate a unique backup filename with timestamp
  BackupPath := ExtractFilePath(ParamStr(0)) + 'backup_' +
    FormatDateTime('yyyymmdd_hhnnss', Now) + '.db';

  Log('Starting backup to: ' + BackupPath);
  SetOperationInProgress(True);

  // Create cancellation token for the backup operation
  FCancellationSource := TNDXCancellationTokenSource.Create;

  // Start async backup with progress reporting
  // The progress callback (OnBackupProgress) is called periodically
  // The completion callback (OnBackupComplete) is called when done
  FConnection.BackupToAsync(BackupPath, @OnBackupProgress, @OnBackupComplete,
    FCancellationSource.Token);
end;

procedure TfrmMain.OnBackupProgress(ACurrent, ATotal: Integer; const AMessage: string);
var
  Percent: Integer;
begin
  // Update visual progress indicators
  UpdateProgressBar(ACurrent, ATotal);

  // Calculate and display percentage
  if ATotal > 0 then
    Percent := (ACurrent * 100) div ATotal
  else
    Percent := 0;
  lblProgress.Caption := Format('Backup progress: %d%% %s', [Percent, AMessage]);
end;

procedure TfrmMain.OnBackupComplete(const ASuccess: Boolean; const AError: string);
begin
  // Clean up operation state
  SetOperationInProgress(False);
  FreeAndNil(FCancellationSource);
  ProgressBar1.Position := 0;
  lblProgress.Caption := '';

  if ASuccess then
    Log('Backup completed successfully')
  else
  begin
    // Check if the failure was due to cancellation
    if Pos('canceled', LowerCase(AError)) > 0 then
      Log('Backup operation was canceled')
    else
      Log('ERROR: Backup failed - ' + AError);
  end;
end;

procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  // Request cancellation of the current async operation
  // The operation will check the token periodically and abort cooperatively
  if Assigned(FCancellationSource) then
  begin
    Log('Canceling operation...');
    FCancellationSource.Cancel;
    // Also interrupt any currently executing SQLite operation
    FConnection.InterruptCurrentOperation;
  end;
end;

procedure TfrmMain.Log(const AMessage: string);
begin
  // Append timestamped message to the log memo
  memoLog.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss.zzz', Now), AMessage]));
  // Auto-scroll to show the latest entry
  memoLog.SelStart := Length(memoLog.Text);
end;

procedure TfrmMain.SetOperationInProgress(AInProgress: Boolean);
begin
  // Update state tracking
  FOperationInProgress := AInProgress;

  // Enable/disable buttons based on operation state
  // Operation buttons are disabled while an operation is running
  btnInsertData.Enabled := not AInProgress;
  btnQueryData.Enabled := not AInProgress;
  btnBackup.Enabled := not AInProgress;
  // Cancel button is only enabled during an operation
  btnCancel.Enabled := AInProgress;
  // Disable spin edit during operation to prevent changes
  edtRecordCount.Enabled := not AInProgress;

  // Update status bar text
  if AInProgress then
    pnlStatus.Caption := 'Operation in progress...'
  else
    pnlStatus.Caption := 'Ready';
end;

procedure TfrmMain.UpdateProgressBar(ACurrent, ATotal: Integer);
begin
  // Update progress bar to show operation progress
  if ATotal > 0 then
  begin
    ProgressBar1.Max := ATotal;
    ProgressBar1.Position := ACurrent;
  end;
  // Process pending UI events to ensure the progress bar updates visually
  // This is necessary because the callback runs on the main thread
  Application.ProcessMessages;
end;

end.
