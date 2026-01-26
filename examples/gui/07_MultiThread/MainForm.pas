{===============================================================================
  NDXSQLite GUI Example - Multi-Thread with Connection Pool
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - TNDXSQLiteConnectionPool for thread-safe connection management
  - Multiple worker threads performing concurrent database operations
  - Thread synchronization for UI updates
  - WAL mode for optimal concurrent access
  - Progress tracking across multiple threads
  - Graceful thread cancellation

  This example shows how to safely use SQLite in a multi-threaded application.
  Each worker thread acquires its own connection from the pool, performs
  operations, and returns the connection when done. The connection pool
  handles all synchronization automatically.

  Key Concepts:
  - Never share a single connection across threads
  - Use connection pool for efficient resource management
  - WAL mode allows concurrent reads during writes
  - Use TThread.Synchronize for UI updates from worker threads
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Spin,
  ndxsqliteconnectionpool, ndxsqliteconnectionoptions, ndxsqliteconnectionintf,
  ndxsqlitetypes;

type
  { TWorkerThread }
  TWorkerThread = class(TThread)
  private
    FPool: TNDXSQLiteConnectionPool;
    FWorkerIndex: Integer;
    FOperationCount: Integer;
    FSuccessCount: Integer;
    FErrorCount: Integer;
    FLastError: string;
    FOnProgress: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    procedure DoProgress;
    procedure DoComplete;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TNDXSQLiteConnectionPool; AWorkerIndex: Integer;
      AOperationCount: Integer);
    property WorkerIndex: Integer read FWorkerIndex;
    property OperationCount: Integer read FOperationCount;
    property SuccessCount: Integer read FSuccessCount;
    property ErrorCount: Integer read FErrorCount;
    property LastError: string read FLastError;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

  { TfrmMain }
  TfrmMain = class(TForm)
    btnStartWorkers: TButton;
    btnStopWorkers: TButton;
    btnClearLog: TButton;
    btnInitDatabase: TButton;
    gbPoolConfig: TGroupBox;
    gbWorkerConfig: TGroupBox;
    gbProgress: TGroupBox;
    lblMinConnections: TLabel;
    lblMaxConnections: TLabel;
    lblThreadCount: TLabel;
    lblOperationsPerThread: TLabel;
    lblPoolStatus: TLabel;
    lblThread1: TLabel;
    lblThread2: TLabel;
    lblThread3: TLabel;
    lblThread4: TLabel;
    memoLog: TMemo;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    ProgressBar4: TProgressBar;
    Splitter1: TSplitter;
    spnMinConnections: TSpinEdit;
    spnMaxConnections: TSpinEdit;
    spnThreadCount: TSpinEdit;
    spnOperationsPerThread: TSpinEdit;
    TimerStatus: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnInitDatabaseClick(Sender: TObject);
    procedure btnStartWorkersClick(Sender: TObject);
    procedure btnStopWorkersClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
  private
    FPool: TNDXSQLiteConnectionPool;
    FThreads: array[0..3] of TWorkerThread;
    FProgressBars: array[0..3] of TProgressBar;
    FThreadLabels: array[0..3] of TLabel;
    FRunning: Boolean;
    { Appends a timestamped message to the activity log. }
    procedure Log(const AMessage: string);
    { Creates the connection pool with current settings. }
    procedure CreatePool;
    { Updates the pool status display. }
    procedure UpdatePoolStatus;
    { Handler for worker thread progress updates. }
    procedure OnThreadProgress(Sender: TObject);
    { Handler for worker thread completion. }
    procedure OnThreadComplete(Sender: TObject);
    { Checks if all threads have completed. }
    procedure CheckAllComplete;
    { Verifies results in database after all threads complete. }
    procedure VerifyResults;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TWorkerThread }

constructor TWorkerThread.Create(APool: TNDXSQLiteConnectionPool;
  AWorkerIndex: Integer; AOperationCount: Integer);
begin
  inherited Create(True);  // Create suspended
  FreeOnTerminate := False;
  FPool := APool;
  FWorkerIndex := AWorkerIndex;
  FOperationCount := AOperationCount;
  FSuccessCount := 0;
  FErrorCount := 0;
  FLastError := '';
end;

procedure TWorkerThread.Execute;
var
  Conn: INDXSQLiteConnection;
  I: Integer;
  RandomValue: Double;
  SQL: string;
begin
  // Each thread performs multiple database operations
  // Each operation acquires a connection, does work, and releases it
  for I := 1 to FOperationCount do
  begin
    if Terminated then
      Break;

    try
      // Acquire a connection from the pool
      // This blocks if no connections are available and pool is at max size
      Conn := FPool.Acquire;
      try
        // Simulate some database work
        // Each thread inserts a record with its ID and a random value
        RandomValue := Random * 1000;
        SQL := Format(
          'INSERT INTO thread_results (thread_id, operation_num, value, created_at) ' +
          'VALUES (%d, %d, %.2f, datetime(''now''))',
          [FWorkerIndex, I, RandomValue]);

        Conn.ExecuteNonQuery(SQL);
        Inc(FSuccessCount);
      finally
        // Return connection to the pool
        FPool.Release(Conn);
      end;

      // Report progress to main thread
      if (I mod 10 = 0) or (I = FOperationCount) then
        Synchronize(@DoProgress);

      // Small delay to simulate real work and allow interleaving
      Sleep(5 + Random(10));

    except
      on E: Exception do
      begin
        Inc(FErrorCount);
        FLastError := E.Message;
        // Continue with next operation
      end;
    end;
  end;

  // Signal completion
  Synchronize(@DoComplete);
end;

procedure TWorkerThread.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TWorkerThread.DoComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Initialize array references for easier access
  FProgressBars[0] := ProgressBar1;
  FProgressBars[1] := ProgressBar2;
  FProgressBars[2] := ProgressBar3;
  FProgressBars[3] := ProgressBar4;

  FThreadLabels[0] := lblThread1;
  FThreadLabels[1] := lblThread2;
  FThreadLabels[2] := lblThread3;
  FThreadLabels[3] := lblThread4;

  FRunning := False;
  FPool := nil;

  // Set default values
  spnMinConnections.Value := 2;
  spnMaxConnections.Value := 4;
  spnThreadCount.Value := 4;
  spnOperationsPerThread.Value := 100;

  Log('Multi-Thread Connection Pool Demo');
  Log('');
  Log('This example demonstrates safe multi-threaded SQLite access.');
  Log('Each worker thread gets its own connection from the pool.');
  Log('');
  Log('Steps:');
  Log('1. Click "Initialize Database" to create pool and tables');
  Log('2. Configure thread count and operations');
  Log('3. Click "Start Workers" to begin concurrent operations');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  // Stop any running threads
  for I := 0 to High(FThreads) do
  begin
    if Assigned(FThreads[I]) then
    begin
      FThreads[I].Terminate;
      FThreads[I].WaitFor;
      FreeAndNil(FThreads[I]);
    end;
  end;

  // Free the connection pool
  FreeAndNil(FPool);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
  HasRunning: Boolean;
begin
  HasRunning := False;
  for I := 0 to High(FThreads) do
  begin
    if Assigned(FThreads[I]) and not FThreads[I].Finished then
    begin
      HasRunning := True;
      Break;
    end;
  end;

  if HasRunning then
  begin
    if MessageDlg('Worker threads are still running. Stop them and close?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      btnStopWorkersClick(nil);
      CanClose := True;
    end
    else
      CanClose := False;
  end
  else
    CanClose := True;
end;

procedure TfrmMain.btnInitDatabaseClick(Sender: TObject);
var
  Conn: INDXSQLiteConnection;
begin
  // Create or recreate the connection pool
  CreatePool;

  if FPool = nil then
    Exit;

  // Initialize the database schema using a pooled connection
  try
    Conn := FPool.Acquire;
    try
      // Enable WAL mode for better concurrent access
      // WAL allows multiple readers while one writer is active
      Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');

      // Create table for storing thread results
      Conn.ExecuteNonQuery(
        'CREATE TABLE IF NOT EXISTS thread_results (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
        '  thread_id INTEGER NOT NULL, ' +
        '  operation_num INTEGER NOT NULL, ' +
        '  value REAL, ' +
        '  created_at TEXT' +
        ')');

      // Clear previous results
      Conn.ExecuteNonQuery('DELETE FROM thread_results');

      // Create index for faster queries by thread_id
      Conn.ExecuteNonQuery(
        'CREATE INDEX IF NOT EXISTS idx_thread_results_thread ' +
        'ON thread_results(thread_id)');

      Log('Database initialized successfully');
      Log('  - WAL mode enabled for concurrent access');
      Log('  - thread_results table ready');

      btnStartWorkers.Enabled := True;
    finally
      FPool.Release(Conn);
    end;
  except
    on E: Exception do
      Log('ERROR initializing database: ' + E.Message);
  end;

  UpdatePoolStatus;
end;

procedure TfrmMain.CreatePool;
var
  DBPath: string;
  Options: TNDXSQLiteConnectionOptions;
begin
  // Free existing pool
  FreeAndNil(FPool);

  DBPath := ExtractFilePath(ParamStr(0)) + 'multithread_demo.db';

  // Configure connection options
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;
    Options.CreateIfNotExists := True;
    Options.BusyTimeout := 5000;  // 5 second timeout for busy waits
    Options.JournalMode := jmWAL;  // WAL mode for concurrent access

    // Create the connection pool
    FPool := TNDXSQLiteConnectionPool.Create(
      Options,
      spnMinConnections.Value,  // Minimum connections to maintain
      spnMaxConnections.Value   // Maximum connections allowed
    );

    Log(Format('Connection pool created: min=%d, max=%d',
      [spnMinConnections.Value, spnMaxConnections.Value]));
    Log('  Database: ' + DBPath);
  finally
    Options.Free;
  end;
end;

procedure TfrmMain.btnStartWorkersClick(Sender: TObject);
var
  I, ThreadCount: Integer;
begin
  if FPool = nil then
  begin
    Log('ERROR: Initialize database first.');
    Exit;
  end;

  ThreadCount := spnThreadCount.Value;
  FRunning := True;

  // Update UI state
  btnStartWorkers.Enabled := False;
  btnStopWorkers.Enabled := True;
  gbPoolConfig.Enabled := False;
  gbWorkerConfig.Enabled := False;

  Log('');
  Log(Format('Starting %d worker threads, %d operations each...',
    [ThreadCount, spnOperationsPerThread.Value]));

  // Create and start worker threads
  for I := 0 to ThreadCount - 1 do
  begin
    // Reset progress bar
    FProgressBars[I].Position := 0;
    FProgressBars[I].Max := spnOperationsPerThread.Value;
    FThreadLabels[I].Caption := Format('Thread %d: Starting...', [I + 1]);

    // Create thread
    FThreads[I] := TWorkerThread.Create(
      FPool,
      I + 1,  // Worker index (1-based)
      spnOperationsPerThread.Value
    );
    FThreads[I].OnProgress := @OnThreadProgress;
    FThreads[I].OnComplete := @OnThreadComplete;

    // Start thread
    FThreads[I].Start;
  end;

  // Enable status timer
  TimerStatus.Enabled := True;
end;

procedure TfrmMain.btnStopWorkersClick(Sender: TObject);
var
  I: Integer;
begin
  Log('Stopping worker threads...');

  // Signal all threads to terminate
  for I := 0 to High(FThreads) do
  begin
    if Assigned(FThreads[I]) then
      FThreads[I].Terminate;
  end;

  // Wait for threads to finish
  for I := 0 to High(FThreads) do
  begin
    if Assigned(FThreads[I]) then
    begin
      FThreads[I].WaitFor;
      FreeAndNil(FThreads[I]);
    end;
  end;

  FRunning := False;
  TimerStatus.Enabled := False;

  // Update UI
  btnStartWorkers.Enabled := True;
  btnStopWorkers.Enabled := False;
  gbPoolConfig.Enabled := True;
  gbWorkerConfig.Enabled := True;

  Log('All worker threads stopped.');
  UpdatePoolStatus;
end;

procedure TfrmMain.OnThreadProgress(Sender: TObject);
var
  Thread: TWorkerThread;
begin
  Thread := TWorkerThread(Sender);

  // Update progress bar for this thread
  if (Thread.WorkerIndex >= 1) and (Thread.WorkerIndex <= 4) then
  begin
    FProgressBars[Thread.WorkerIndex - 1].Position := Thread.SuccessCount;
    FThreadLabels[Thread.WorkerIndex - 1].Caption :=
      Format('Thread %d: %d/%d (errors: %d)',
        [Thread.WorkerIndex, Thread.SuccessCount, Thread.OperationCount, Thread.ErrorCount]);
  end;
end;

procedure TfrmMain.OnThreadComplete(Sender: TObject);
var
  Thread: TWorkerThread;
begin
  Thread := TWorkerThread(Sender);

  Log(Format('Thread %d completed: %d success, %d errors',
    [Thread.WorkerIndex, Thread.SuccessCount, Thread.ErrorCount]));

  if Thread.ErrorCount > 0 then
    Log(Format('  Last error: %s', [Thread.LastError]));

  FThreadLabels[Thread.WorkerIndex - 1].Caption :=
    Format('Thread %d: DONE (%d/%d)',
      [Thread.WorkerIndex, Thread.SuccessCount, Thread.OperationCount]);

  CheckAllComplete;
end;

procedure TfrmMain.CheckAllComplete;
var
  I: Integer;
  AllDone: Boolean;
  TotalSuccess, TotalErrors: Integer;
begin
  AllDone := True;
  TotalSuccess := 0;
  TotalErrors := 0;

  for I := 0 to spnThreadCount.Value - 1 do
  begin
    if Assigned(FThreads[I]) then
    begin
      if not FThreads[I].Finished then
        AllDone := False;
      TotalSuccess := TotalSuccess + FThreads[I].SuccessCount;
      TotalErrors := TotalErrors + FThreads[I].ErrorCount;
    end;
  end;

  if AllDone then
  begin
    FRunning := False;
    TimerStatus.Enabled := False;

    Log('');
    Log('=== All threads completed ===');
    Log(Format('Total operations: %d success, %d errors',
      [TotalSuccess, TotalErrors]));

    // Verify results in database
    VerifyResults;

    // Update UI
    btnStartWorkers.Enabled := True;
    btnStopWorkers.Enabled := False;
    gbPoolConfig.Enabled := True;
    gbWorkerConfig.Enabled := True;

    // Clean up threads
    for I := 0 to High(FThreads) do
      FreeAndNil(FThreads[I]);

    UpdatePoolStatus;
  end;
end;

procedure TfrmMain.VerifyResults;
var
  Conn: INDXSQLiteConnection;
  Count: Variant;
begin
  // Query the database to verify all inserts were recorded
  try
    Conn := FPool.Acquire;
    try
      // Count total records inserted
      Count := Conn.ExecuteScalar('SELECT COUNT(*) FROM thread_results');
      Log(Format('Database verification: %d records in thread_results table', [Integer(Count)]));
    finally
      FPool.Release(Conn);
    end;
  except
    on E: Exception do
      Log('Error verifying: ' + E.Message);
  end;
end;

procedure TfrmMain.TimerStatusTimer(Sender: TObject);
begin
  UpdatePoolStatus;
end;

procedure TfrmMain.UpdatePoolStatus;
begin
  if Assigned(FPool) then
    lblPoolStatus.Caption := Format('Pool: %d active, %d idle',
      [FPool.ActiveCount, FPool.IdleCount])
  else
    lblPoolStatus.Caption := 'Pool: Not initialized';
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  Log('Log cleared');
end;

procedure TfrmMain.Log(const AMessage: string);
begin
  if AMessage = '' then
    memoLog.Lines.Add('')
  else
    memoLog.Lines.Add(Format('[%s] %s',
      [FormatDateTime('hh:nn:ss.zzz', Now), AMessage]));

  memoLog.SelStart := Length(memoLog.Text);
  Application.ProcessMessages;
end;

end.
