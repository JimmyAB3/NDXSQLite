{===============================================================================
  NDXSQLite - Async Worker Thread
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 3 (depends on ndxsqliteasynctypes, ndxsqlitecancellation)
===============================================================================}
unit ndxsqliteasyncworker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DB, DateUtils,
  ndxsqliteasynctypes, ndxsqlitecancellation;

type
  { Async operation type }
  TNDXAsyncOperationType = (
    aotOpen,
    aotClose,
    aotExecuteNonQuery,
    aotExecuteScalar,
    aotExecuteQuery,
    aotBeginTransaction,
    aotCommit,
    aotRollback,
    aotBackup
  );

  { Execution callback for the worker }
  TNDXWorkerExecuteProc = procedure of object;

  { Generic worker thread }
  TNDXAsyncWorker = class(TThread)
  private
    class var FWorkerCounter: Integer;
  private
    FId: Integer;
    FOperationType: TNDXAsyncOperationType;
    FStatus: TNDXAsyncStatus;
    FSQL: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FExceptionMessage: string;
    FExceptionClass: string;
    FCancellationToken: INDXCancellationToken;
    FTimeoutMs: Integer;
    FCompletedEvent: TEvent;

    // Results
    FResultInt: Integer;
    FResultInt64: Int64;
    FResultVariant: Variant;
    FResultDataSet: TDataSet;
    FResultBoolean: Boolean;

    // Callbacks
    FOnComplete: TNotifyEvent;
    FOnError: TNotifyEvent;
    FOnProgress: TNDXProgressCallback;

    // Custom execution procedure
    FExecuteProc: TNDXWorkerExecuteProc;

    function GetExecutionTimeMs: Int64;
    procedure DoComplete;
    procedure DoError;

  protected
    procedure Execute; override;
    procedure ExecuteOperation; virtual;

  public
    { Creates a suspended worker thread for the specified operation type. }
    constructor Create(AOperationType: TNDXAsyncOperationType;
      ACancellationToken: INDXCancellationToken = nil);
    destructor Destroy; override;

    { Requests cancellation of the running operation via the cancellation token. }
    procedure Cancel;
    { Blocks until the operation completes or the timeout expires. Returns True if completed. }
    function Wait(ATimeoutMs: Integer = $7FFFFFFF): Boolean;
    { Raises ENDXOperationCanceledException if cancellation has been requested. }
    procedure CheckCancellation;

    property Id: Integer read FId;
    property OperationType: TNDXAsyncOperationType read FOperationType;
    property Status: TNDXAsyncStatus read FStatus;
    property SQL: string read FSQL write FSQL;
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
    property ExecutionTimeMs: Int64 read GetExecutionTimeMs;
    property ExceptionMessage: string read FExceptionMessage;
    property ExceptionClass: string read FExceptionClass;
    property CancellationToken: INDXCancellationToken read FCancellationToken;
    property TimeoutMs: Integer read FTimeoutMs write FTimeoutMs;

    // Results
    property ResultInt: Integer read FResultInt write FResultInt;
    property ResultInt64: Int64 read FResultInt64 write FResultInt64;
    property ResultVariant: Variant read FResultVariant write FResultVariant;
    property ResultDataSet: TDataSet read FResultDataSet write FResultDataSet;
    property ResultBoolean: Boolean read FResultBoolean write FResultBoolean;

    // Callbacks
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnProgress: TNDXProgressCallback read FOnProgress write FOnProgress;

    // Custom execution procedure
    property ExecuteProc: TNDXWorkerExecuteProc read FExecuteProc write FExecuteProc;
  end;

  { Async operations manager }
  TNDXAsyncOperationManager = class
  private
    FOperations: TThreadList;
    FMaxConcurrent: Integer;
    FLock: TCriticalSection;

    function GetActiveCount: Integer;
    function GetPendingCount: Integer;
    function GetTotalCount: Integer;

  public
    constructor Create(AMaxConcurrent: Integer = 10);
    destructor Destroy; override;

    { Adds a worker to the managed operation list. }
    procedure RegisterOperation(AWorker: TNDXAsyncWorker);
    { Removes a worker from the managed operation list. }
    procedure UnregisterOperation(AWorker: TNDXAsyncWorker);
    { Frees all workers that have reached a terminal state. }
    procedure CleanupCompletedOperations;

    { Requests cancellation of all active operations. }
    procedure CancelAll;
    { Blocks until all managed operations complete or the timeout expires. }
    procedure WaitForAll(ATimeoutMs: Integer = $7FFFFFFF);

    property ActiveCount: Integer read GetActiveCount;
    property PendingCount: Integer read GetPendingCount;
    property TotalCount: Integer read GetTotalCount;
    property MaxConcurrent: Integer read FMaxConcurrent write FMaxConcurrent;
  end;

implementation

{ TNDXAsyncWorker }

constructor TNDXAsyncWorker.Create(AOperationType: TNDXAsyncOperationType;
  ACancellationToken: INDXCancellationToken);
begin
  inherited Create(True); // Created suspended
  FreeOnTerminate := False;

  FId := InterlockedIncrement(FWorkerCounter);
  FOperationType := AOperationType;
  FStatus := asPending;
  FCancellationToken := ACancellationToken;
  FTimeoutMs := 30000;
  FCompletedEvent := TEvent.Create(nil, True, False, '');
  FStartTime := 0;
  FEndTime := 0;
  FResultInt := 0;
  FResultInt64 := 0;
  FResultBoolean := False;
  FResultDataSet := nil;
end;

destructor TNDXAsyncWorker.Destroy;
begin
  FreeAndNil(FCompletedEvent);
  FCancellationToken := nil;
  inherited Destroy;
end;

procedure TNDXAsyncWorker.Execute;
begin
  FStartTime := Now;
  FStatus := asRunning;

  try
    // Check cancellation before execution
    if Assigned(FCancellationToken) and FCancellationToken.IsCancellationRequested then
    begin
      FStatus := asCanceled;
      FExceptionMessage := 'Operation canceled before execution';
      FCompletedEvent.SetEvent;
      Exit;
    end;

    ExecuteOperation;

    FStatus := asCompleted;
    FEndTime := Now;

    if Assigned(FOnComplete) then
      Synchronize(@DoComplete);

  except
    on E: ENDXOperationCanceledException do
    begin
      FStatus := asCanceled;
      FExceptionMessage := E.Message;
      FExceptionClass := E.ClassName;
      FEndTime := Now;
    end;
    on E: Exception do
    begin
      FStatus := asFaulted;
      FExceptionMessage := E.Message;
      FExceptionClass := E.ClassName;
      FEndTime := Now;

      if Assigned(FOnError) then
        Synchronize(@DoError);
    end;
  end;

  FCompletedEvent.SetEvent;
end;

procedure TNDXAsyncWorker.ExecuteOperation;
begin
  // Execute the custom procedure if defined
  if Assigned(FExecuteProc) then
    FExecuteProc;
end;

procedure TNDXAsyncWorker.DoComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

procedure TNDXAsyncWorker.DoError;
begin
  if Assigned(FOnError) then
    FOnError(Self);
end;

procedure TNDXAsyncWorker.Cancel;
begin
  Terminate;
  FStatus := asCanceled;
end;

function TNDXAsyncWorker.Wait(ATimeoutMs: Integer): Boolean;
begin
  if ATimeoutMs < 0 then
    ATimeoutMs := $7FFFFFFF;
  Result := FCompletedEvent.WaitFor(ATimeoutMs) = wrSignaled;
end;

procedure TNDXAsyncWorker.CheckCancellation;
begin
  if Terminated then
    raise ENDXOperationCanceledException.Create('Operation canceled');

  if Assigned(FCancellationToken) then
    FCancellationToken.ThrowIfCancellationRequested;
end;

function TNDXAsyncWorker.GetExecutionTimeMs: Int64;
begin
  if FEndTime > 0 then
    Result := MilliSecondsBetween(FEndTime, FStartTime)
  else if FStartTime > 0 then
    Result := MilliSecondsBetween(Now, FStartTime)
  else
    Result := 0;
end;

{ TNDXAsyncOperationManager }

constructor TNDXAsyncOperationManager.Create(AMaxConcurrent: Integer);
begin
  inherited Create;
  FOperations := TThreadList.Create;
  FLock := TCriticalSection.Create;
  FMaxConcurrent := AMaxConcurrent;
end;

destructor TNDXAsyncOperationManager.Destroy;
begin
  CancelAll;
  WaitForAll(5000);
  FreeAndNil(FOperations);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TNDXAsyncOperationManager.RegisterOperation(AWorker: TNDXAsyncWorker);
var
  List: TList;
begin
  List := FOperations.LockList;
  try
    if List.IndexOf(AWorker) < 0 then
      List.Add(AWorker);
  finally
    FOperations.UnlockList;
  end;
end;

procedure TNDXAsyncOperationManager.UnregisterOperation(AWorker: TNDXAsyncWorker);
var
  List: TList;
begin
  List := FOperations.LockList;
  try
    List.Remove(AWorker);
  finally
    FOperations.UnlockList;
  end;
end;

procedure TNDXAsyncOperationManager.CleanupCompletedOperations;
var
  List: TList;
  I: Integer;
  Worker: TNDXAsyncWorker;
begin
  List := FOperations.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Worker := TNDXAsyncWorker(List[I]);
      if Worker.Status.IsTerminal then
      begin
        List.Delete(I);
        Worker.Free;
      end;
    end;
  finally
    FOperations.UnlockList;
  end;
end;

function TNDXAsyncOperationManager.GetActiveCount: Integer;
var
  List: TList;
  I: Integer;
  Worker: TNDXAsyncWorker;
begin
  Result := 0;
  List := FOperations.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Worker := TNDXAsyncWorker(List[I]);
      if Worker.Status = asRunning then
        Inc(Result);
    end;
  finally
    FOperations.UnlockList;
  end;
end;

function TNDXAsyncOperationManager.GetPendingCount: Integer;
var
  List: TList;
  I: Integer;
  Worker: TNDXAsyncWorker;
begin
  Result := 0;
  List := FOperations.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Worker := TNDXAsyncWorker(List[I]);
      if Worker.Status = asPending then
        Inc(Result);
    end;
  finally
    FOperations.UnlockList;
  end;
end;

function TNDXAsyncOperationManager.GetTotalCount: Integer;
var
  List: TList;
begin
  List := FOperations.LockList;
  try
    Result := List.Count;
  finally
    FOperations.UnlockList;
  end;
end;

procedure TNDXAsyncOperationManager.CancelAll;
var
  List: TList;
  I: Integer;
  Worker: TNDXAsyncWorker;
begin
  List := FOperations.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Worker := TNDXAsyncWorker(List[I]);
      Worker.Cancel;
    end;
  finally
    FOperations.UnlockList;
  end;
end;

procedure TNDXAsyncOperationManager.WaitForAll(ATimeoutMs: Integer);
var
  List: TList;
  I: Integer;
  Worker: TNDXAsyncWorker;
  StartTime: TDateTime;
  RemainingMs: Integer;
begin
  StartTime := Now;

  List := FOperations.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Worker := TNDXAsyncWorker(List[I]);

      if ATimeoutMs > 0 then
      begin
        RemainingMs := ATimeoutMs - MilliSecondsBetween(Now, StartTime);
        if RemainingMs <= 0 then
          Break;
        Worker.Wait(RemainingMs);
      end
      else
        Worker.Wait(ATimeoutMs);
    end;
  finally
    FOperations.UnlockList;
  end;
end;

end.
