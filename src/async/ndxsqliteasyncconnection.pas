{===============================================================================
  NDXSQLite - Asynchronous Connection
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 5 (depends on ndxsqliteconnection, ndxsqliteasyncworker)
===============================================================================}
unit ndxsqliteasyncconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants, SyncObjs,
  ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions, ndxsqliteasynctypes,
  ndxsqlitecancellation, ndxsqliteconnection, ndxsqliteconnectionintf,
  ndxsqliteconnectionoptions, ndxsqliteasyncworker;

type
  { Forward declaration }
  TNDXAsyncOperationThread = class;

  { SQLite asynchronous connection }
  TNDXSQLiteAsyncConnection = class(TNDXSQLiteConnection, INDXSQLiteAsyncConnection)
  private
    FOperationManager: TNDXAsyncOperationManager;
    FAsyncLock: TCriticalSection;  // Serializes async operations
    FCurrentThread: TNDXAsyncOperationThread;

  public
    constructor Create(AOptions: TNDXSQLiteConnectionOptions); reintroduce; overload;
    constructor Create(const ADatabasePath: string; AIsPrimary: Boolean = False); reintroduce; overload;
    destructor Destroy; override;

    { Opens the database connection asynchronously with completion callback. }
    procedure OpenAsync(AOnComplete: TNDXAsyncCallback;
      ACancellationToken: INDXCancellationToken = nil);
    { Closes the database connection asynchronously. }
    procedure CloseAsync(AOnComplete: TNDXAsyncCallback = nil);

    { Executes a non-query SQL statement asynchronously. Returns rows affected via callback. }
    procedure ExecuteNonQueryAsync(const ASQL: string;
      AOnComplete: TNDXAsyncCallbackInt;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a parameterized non-query SQL statement asynchronously. }
    procedure ExecuteNonQueryAsync(const ASQL: string;
      const AParams: array of Variant;
      AOnComplete: TNDXAsyncCallbackInt;
      ACancellationToken: INDXCancellationToken = nil); overload;

    { Executes a scalar SQL query asynchronously. Returns the first column of the first row. }
    procedure ExecuteScalarAsync(const ASQL: string;
      AOnComplete: TNDXAsyncCallbackVariant;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a parameterized scalar query asynchronously. }
    procedure ExecuteScalarAsync(const ASQL: string;
      const AParams: array of Variant;
      AOnComplete: TNDXAsyncCallbackVariant;
      ACancellationToken: INDXCancellationToken = nil); overload;

    { Executes a SQL query asynchronously and returns a dataset via callback. }
    procedure ExecuteQueryAsync(const ASQL: string;
      AOnComplete: TNDXAsyncCallbackDataSet;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a parameterized SQL query asynchronously. }
    procedure ExecuteQueryAsync(const ASQL: string;
      const AParams: array of Variant;
      AOnComplete: TNDXAsyncCallbackDataSet;
      ACancellationToken: INDXCancellationToken = nil); overload;

    { Begins a transaction asynchronously with the specified mode. }
    procedure BeginTransactionAsync(AMode: TNDXSQLiteTransactionMode;
      AOnComplete: TNDXAsyncCallbackBoolean;
      ACancellationToken: INDXCancellationToken = nil);
    { Commits the active transaction asynchronously. }
    procedure CommitAsync(AOnComplete: TNDXAsyncCallback;
      ACancellationToken: INDXCancellationToken = nil);
    { Rolls back the active transaction asynchronously. }
    procedure RollbackAsync(AOnComplete: TNDXAsyncCallback = nil);

    { Performs an asynchronous backup with progress reporting and optional cancellation. }
    procedure BackupToAsync(const ADestPath: string;
      AOnProgress: TNDXProgressCallback;
      AOnComplete: TNDXAsyncCallback;
      ACancellationToken: INDXCancellationToken = nil);

    { Requests cancellation of all pending asynchronous operations. }
    procedure CancelAllPendingOperations;
    { Blocks until all pending operations complete or the timeout expires. }
    procedure WaitForAllOperations(ATimeoutMs: Integer = -1);
    { Returns the number of operations currently pending or running. }
    function GetPendingOperationsCount: Integer;
    { Sends an interrupt signal to the currently executing SQLite operation. }
    procedure InterruptCurrentOperation;

    property OperationManager: TNDXAsyncOperationManager read FOperationManager;
  end;

  { Specialized thread for async operations }
  TNDXAsyncOperationThread = class(TThread)
  private
    FConnection: TNDXSQLiteAsyncConnection;
    FOperationType: TNDXAsyncOperationType;
    FSQL: string;
    FParams: array of Variant;
    FTransactionMode: TNDXSQLiteTransactionMode;
    FDestPath: string;
    FCancellationToken: INDXCancellationToken;

    // Callbacks
    FOnCompleteCallback: TNDXAsyncCallback;
    FOnCompleteIntCallback: TNDXAsyncCallbackInt;
    FOnCompleteVariantCallback: TNDXAsyncCallbackVariant;
    FOnCompleteDataSetCallback: TNDXAsyncCallbackDataSet;
    FOnCompleteBoolCallback: TNDXAsyncCallbackBoolean;
    FOnProgressCallback: TNDXProgressCallback;

    // Results
    FResultSuccess: Boolean;
    FResultError: string;
    FResultInt: Integer;
    FResultVariant: Variant;
    FResultDataSet: TDataSet;
    FResultBool: Boolean;

    procedure DoCompleteCallback;
    procedure DoCompleteIntCallback;
    procedure DoCompleteVariantCallback;
    procedure DoCompleteDataSetCallback;
    procedure DoCompleteBoolCallback;

  protected
    procedure Execute; override;

  public
    constructor Create(AConnection: TNDXSQLiteAsyncConnection;
      AOperationType: TNDXAsyncOperationType;
      ACancellationToken: INDXCancellationToken = nil);
    destructor Destroy; override;

    procedure SetParams(const AParams: array of Variant);

    property SQL: string read FSQL write FSQL;
    property TransactionMode: TNDXSQLiteTransactionMode write FTransactionMode;
    property DestPath: string write FDestPath;
    property OnCompleteCallback: TNDXAsyncCallback write FOnCompleteCallback;
    property OnCompleteIntCallback: TNDXAsyncCallbackInt write FOnCompleteIntCallback;
    property OnCompleteVariantCallback: TNDXAsyncCallbackVariant write FOnCompleteVariantCallback;
    property OnCompleteDataSetCallback: TNDXAsyncCallbackDataSet write FOnCompleteDataSetCallback;
    property OnCompleteBoolCallback: TNDXAsyncCallbackBoolean write FOnCompleteBoolCallback;
    property OnProgressCallback: TNDXProgressCallback write FOnProgressCallback;
  end;

implementation

{ TNDXAsyncOperationThread }

constructor TNDXAsyncOperationThread.Create(AConnection: TNDXSQLiteAsyncConnection;
  AOperationType: TNDXAsyncOperationType;
  ACancellationToken: INDXCancellationToken);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FConnection := AConnection;
  FOperationType := AOperationType;
  FCancellationToken := ACancellationToken;
  FResultSuccess := False;
  FResultError := '';
  FResultInt := 0;
  FResultBool := False;
  FResultDataSet := nil;
  SetLength(FParams, 0);
end;

destructor TNDXAsyncOperationThread.Destroy;
begin
  FCancellationToken := nil;
  SetLength(FParams, 0);
  inherited Destroy;
end;

procedure TNDXAsyncOperationThread.SetParams(const AParams: array of Variant);
var
  I: Integer;
begin
  SetLength(FParams, Length(AParams));
  for I := 0 to High(AParams) do
    FParams[I] := AParams[I];
end;

procedure TNDXAsyncOperationThread.Execute;
begin
  // Serialize access to the connection - wait for any previous operation to complete
  FConnection.FAsyncLock.Enter;
  try
    // Register as current thread so we can be interrupted if needed
    FConnection.FCurrentThread := Self;
    try
      // Check cancellation before starting
      if Assigned(FCancellationToken) and FCancellationToken.IsCancellationRequested then
      begin
        FResultSuccess := False;
        FResultError := 'Operation canceled';
        if Assigned(FOnCompleteCallback) then
          Synchronize(@DoCompleteCallback);
        Exit;
      end;

      case FOperationType of
        aotOpen:
          begin
            FConnection.Open;
            FResultSuccess := True;
            if Assigned(FOnCompleteCallback) then
              Synchronize(@DoCompleteCallback);
          end;

        aotClose:
          begin
            FConnection.Close;
            FResultSuccess := True;
            if Assigned(FOnCompleteCallback) then
              Synchronize(@DoCompleteCallback);
          end;

        aotExecuteNonQuery:
          begin
            if Length(FParams) > 0 then
              FResultInt := FConnection.ExecuteNonQuery(FSQL, FParams)
            else
              FResultInt := FConnection.ExecuteNonQuery(FSQL);
            FResultSuccess := True;
            if Assigned(FOnCompleteIntCallback) then
              Synchronize(@DoCompleteIntCallback);
          end;

        aotExecuteScalar:
          begin
            if Length(FParams) > 0 then
              FResultVariant := FConnection.ExecuteScalar(FSQL, FParams)
            else
              FResultVariant := FConnection.ExecuteScalar(FSQL);
            FResultSuccess := True;
            if Assigned(FOnCompleteVariantCallback) then
              Synchronize(@DoCompleteVariantCallback);
          end;

        aotExecuteQuery:
          begin
            if Length(FParams) > 0 then
              FResultDataSet := FConnection.ExecuteQuery(FSQL, FParams)
            else
              FResultDataSet := FConnection.ExecuteQuery(FSQL);
            FResultSuccess := True;
            if Assigned(FOnCompleteDataSetCallback) then
              Synchronize(@DoCompleteDataSetCallback);
          end;

        aotBeginTransaction:
          begin
            FConnection.BeginTransaction(FTransactionMode);
            FResultBool := True;
            FResultSuccess := True;
            if Assigned(FOnCompleteBoolCallback) then
              Synchronize(@DoCompleteBoolCallback);
          end;

        aotCommit:
          begin
            FConnection.Commit;
            FResultSuccess := True;
            if Assigned(FOnCompleteCallback) then
              Synchronize(@DoCompleteCallback);
          end;

        aotRollback:
          begin
            FConnection.Rollback;
            FResultSuccess := True;
            if Assigned(FOnCompleteCallback) then
              Synchronize(@DoCompleteCallback);
          end;

        aotBackup:
          begin
            FConnection.BackupTo(FDestPath);
            FResultSuccess := True;
            if Assigned(FOnCompleteCallback) then
              Synchronize(@DoCompleteCallback);
          end;
      end;

    except
      on E: Exception do
      begin
        FResultSuccess := False;
        FResultError := E.Message;

        // Call the appropriate error callback
        case FOperationType of
          aotOpen, aotClose, aotCommit, aotRollback, aotBackup:
            if Assigned(FOnCompleteCallback) then
              Synchronize(@DoCompleteCallback);
          aotExecuteNonQuery:
            if Assigned(FOnCompleteIntCallback) then
              Synchronize(@DoCompleteIntCallback);
          aotExecuteScalar:
            if Assigned(FOnCompleteVariantCallback) then
              Synchronize(@DoCompleteVariantCallback);
          aotExecuteQuery:
            if Assigned(FOnCompleteDataSetCallback) then
              Synchronize(@DoCompleteDataSetCallback);
          aotBeginTransaction:
            if Assigned(FOnCompleteBoolCallback) then
              Synchronize(@DoCompleteBoolCallback);
        end;
      end;
    end;
  finally
    // Clear the current thread reference
    FConnection.FCurrentThread := nil;
    FConnection.FAsyncLock.Leave;
  end;
end;

procedure TNDXAsyncOperationThread.DoCompleteCallback;
begin
  if Assigned(FOnCompleteCallback) then
    FOnCompleteCallback(FResultSuccess, FResultError);
end;

procedure TNDXAsyncOperationThread.DoCompleteIntCallback;
var
  Res: TNDXAsyncResultInt;
begin
  if Assigned(FOnCompleteIntCallback) then
  begin
    if FResultSuccess then
      Res := TNDXAsyncResultInt.CreateSuccess(FResultInt, 0)
    else
      Res := TNDXAsyncResultInt.CreateError(FResultError);
    FOnCompleteIntCallback(Res);
  end;
end;

procedure TNDXAsyncOperationThread.DoCompleteVariantCallback;
var
  Res: TNDXAsyncResultVariant;
begin
  if Assigned(FOnCompleteVariantCallback) then
  begin
    if FResultSuccess then
      Res := TNDXAsyncResultVariant.CreateSuccess(FResultVariant, 0)
    else
      Res := TNDXAsyncResultVariant.CreateError(FResultError);
    FOnCompleteVariantCallback(Res);
  end;
end;

procedure TNDXAsyncOperationThread.DoCompleteDataSetCallback;
var
  Res: TNDXAsyncResultDataSet;
begin
  if Assigned(FOnCompleteDataSetCallback) then
  begin
    if FResultSuccess then
      Res := TNDXAsyncResultDataSet.CreateSuccess(FResultDataSet, 0)
    else
      Res := TNDXAsyncResultDataSet.CreateError(FResultError);
    FOnCompleteDataSetCallback(Res);
  end;
end;

procedure TNDXAsyncOperationThread.DoCompleteBoolCallback;
var
  Res: TNDXAsyncResultBoolean;
begin
  if Assigned(FOnCompleteBoolCallback) then
  begin
    if FResultSuccess then
      Res := TNDXAsyncResultBoolean.CreateSuccess(FResultBool, 0)
    else
      Res := TNDXAsyncResultBoolean.CreateError(FResultError);
    FOnCompleteBoolCallback(Res);
  end;
end;

{ TNDXSQLiteAsyncConnection }

constructor TNDXSQLiteAsyncConnection.Create(const ADatabasePath: string;
  AIsPrimary: Boolean);
begin
  inherited Create(ADatabasePath, AIsPrimary);
  FOperationManager := TNDXAsyncOperationManager.Create(10);
  FAsyncLock := TCriticalSection.Create;
  FCurrentThread := nil;
end;

constructor TNDXSQLiteAsyncConnection.Create(AOptions: TNDXSQLiteConnectionOptions);
begin
  inherited Create(AOptions);
  FOperationManager := TNDXAsyncOperationManager.Create(10);
  FAsyncLock := TCriticalSection.Create;
  FCurrentThread := nil;
end;

destructor TNDXSQLiteAsyncConnection.Destroy;
begin
  CancelAllPendingOperations;
  WaitForAllOperations(5000);
  FreeAndNil(FOperationManager);
  FreeAndNil(FAsyncLock);
  FCurrentThread := nil;
  inherited Destroy;
end;

procedure TNDXSQLiteAsyncConnection.InterruptCurrentOperation;
var
  Handle: Pointer;
begin
  FAsyncLock.Enter;
  try
    if Assigned(FCurrentThread) and (not FCurrentThread.Finished) then
    begin
      // Use sqlite3_interrupt to interrupt long-running queries
      Handle := GetConnectionHandle;
      if Handle <> nil then
        sqlite3_interrupt(Handle);
    end;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TNDXSQLiteAsyncConnection.OpenAsync(AOnComplete: TNDXAsyncCallback;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotOpen, ACancellationToken);
  Thread.OnCompleteCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.CloseAsync(AOnComplete: TNDXAsyncCallback);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotClose, nil);
  Thread.OnCompleteCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.ExecuteNonQueryAsync(const ASQL: string;
  AOnComplete: TNDXAsyncCallbackInt;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotExecuteNonQuery, ACancellationToken);
  Thread.SQL := ASQL;
  Thread.OnCompleteIntCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.ExecuteNonQueryAsync(const ASQL: string;
  const AParams: array of Variant;
  AOnComplete: TNDXAsyncCallbackInt;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotExecuteNonQuery, ACancellationToken);
  Thread.SQL := ASQL;
  Thread.SetParams(AParams);
  Thread.OnCompleteIntCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.ExecuteScalarAsync(const ASQL: string;
  AOnComplete: TNDXAsyncCallbackVariant;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotExecuteScalar, ACancellationToken);
  Thread.SQL := ASQL;
  Thread.OnCompleteVariantCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.ExecuteScalarAsync(const ASQL: string;
  const AParams: array of Variant;
  AOnComplete: TNDXAsyncCallbackVariant;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotExecuteScalar, ACancellationToken);
  Thread.SQL := ASQL;
  Thread.SetParams(AParams);
  Thread.OnCompleteVariantCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.ExecuteQueryAsync(const ASQL: string;
  AOnComplete: TNDXAsyncCallbackDataSet;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotExecuteQuery, ACancellationToken);
  Thread.SQL := ASQL;
  Thread.OnCompleteDataSetCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.ExecuteQueryAsync(const ASQL: string;
  const AParams: array of Variant;
  AOnComplete: TNDXAsyncCallbackDataSet;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotExecuteQuery, ACancellationToken);
  Thread.SQL := ASQL;
  Thread.SetParams(AParams);
  Thread.OnCompleteDataSetCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.BeginTransactionAsync(
  AMode: TNDXSQLiteTransactionMode;
  AOnComplete: TNDXAsyncCallbackBoolean;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotBeginTransaction, ACancellationToken);
  Thread.TransactionMode := AMode;
  Thread.OnCompleteBoolCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.CommitAsync(AOnComplete: TNDXAsyncCallback;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotCommit, ACancellationToken);
  Thread.OnCompleteCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.RollbackAsync(AOnComplete: TNDXAsyncCallback);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotRollback, nil);
  Thread.OnCompleteCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.BackupToAsync(const ADestPath: string;
  AOnProgress: TNDXProgressCallback;
  AOnComplete: TNDXAsyncCallback;
  ACancellationToken: INDXCancellationToken);
var
  Thread: TNDXAsyncOperationThread;
begin
  Thread := TNDXAsyncOperationThread.Create(Self, aotBackup, ACancellationToken);
  Thread.DestPath := ADestPath;
  Thread.OnProgressCallback := AOnProgress;
  Thread.OnCompleteCallback := AOnComplete;
  Thread.Start;
end;

procedure TNDXSQLiteAsyncConnection.CancelAllPendingOperations;
begin
  FOperationManager.CancelAll;
  // Also interrupt any currently executing operation
  InterruptCurrentOperation;
end;

procedure TNDXSQLiteAsyncConnection.WaitForAllOperations(ATimeoutMs: Integer);
begin
  if ATimeoutMs < 0 then
    ATimeoutMs := $7FFFFFFF;
  FOperationManager.WaitForAll(ATimeoutMs);
end;

function TNDXSQLiteAsyncConnection.GetPendingOperationsCount: Integer;
begin
  Result := FOperationManager.TotalCount;
end;

end.
