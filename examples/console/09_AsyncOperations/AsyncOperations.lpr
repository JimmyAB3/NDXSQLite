{===============================================================================
  NDXSQLite Example 09 - Async Operations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Asynchronous database operations
  - Cancellation tokens
  - Async callbacks
  - Non-blocking queries

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program AsyncOperations;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteasyncconnection,
  ndxsqliteconnectionoptions, ndxsqlitecancellation, ndxsqliteasynctypes;

type
  { Helper class for async callbacks }
  TAsyncCallbackHandler = class
  private
    FOperationComplete: Boolean;
    FOperationResult: string;
  public
    property OperationComplete: Boolean read FOperationComplete write FOperationComplete;
    property OperationResult: string read FOperationResult write FOperationResult;

    procedure OnOpenComplete(const ASuccess: Boolean; const AError: string);
    procedure OnQueryComplete(const AResult: TNDXAsyncResultDataSet);
    procedure OnScalarComplete(const AResult: TNDXAsyncResultVariant);
    procedure OnNonQueryComplete(const AResult: TNDXAsyncResultInt);
  end;

var
  AsyncConn: TNDXSQLiteAsyncConnection;
  Options: TNDXSQLiteConnectionOptions;
  CancelTokenSource: TNDXCancellationTokenSource;
  DBPath: string;
  Handler: TAsyncCallbackHandler;

{ TAsyncCallbackHandler }

procedure TAsyncCallbackHandler.OnOpenComplete(const ASuccess: Boolean; const AError: string);
begin
  if ASuccess then
    FOperationResult := 'Open completed successfully'
  else
    FOperationResult := 'Open failed: ' + AError;
  FOperationComplete := True;
end;

{ Handles completion of an asynchronous query operation. }
procedure TAsyncCallbackHandler.OnQueryComplete(const AResult: TNDXAsyncResultDataSet);
begin
  if AResult.Success then
  begin
    if Assigned(AResult.Data) then
    begin
      FOperationResult := Format('Query returned %d rows', [AResult.Data.RecordCount]);
      AResult.Data.Free;
    end
    else
      FOperationResult := 'Query completed (no results)';
  end
  else
    FOperationResult := 'Query failed: ' + AResult.ErrorMessage;
  FOperationComplete := True;
end;

{ Handles completion of an asynchronous scalar query operation. }
procedure TAsyncCallbackHandler.OnScalarComplete(const AResult: TNDXAsyncResultVariant);
begin
  if AResult.Success then
    FOperationResult := 'Scalar value: ' + VarToStr(AResult.Data)
  else
    FOperationResult := 'Scalar failed: ' + AResult.ErrorMessage;
  FOperationComplete := True;
end;

{ Handles completion of an asynchronous non-query operation. }
procedure TAsyncCallbackHandler.OnNonQueryComplete(const AResult: TNDXAsyncResultInt);
begin
  if AResult.Success then
    FOperationResult := Format('NonQuery affected %d rows', [AResult.Data])
  else
    FOperationResult := 'NonQuery failed: ' + AResult.ErrorMessage;
  FOperationComplete := True;
end;

{ Waits for the current async operation to complete with a timeout. }
procedure WaitForOperation(ATimeoutMs: Integer = 5000);
var
  StartTick: QWord;
begin
  StartTick := GetTickCount64;
  while not Handler.OperationComplete do
  begin
    Sleep(10);
    CheckSynchronize(10); // Process pending synchronizations
    if (GetTickCount64 - StartTick) > QWord(ATimeoutMs) then
    begin
      Handler.OperationResult := 'Operation timed out';
      Handler.OperationComplete := True;
      Break;
    end;
  end;
end;

begin
  WriteLn('=== NDXSQLite Example 09: Async Operations ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example09.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Handler := TAsyncCallbackHandler.Create;
  try
    // 1. Create async connection
    WriteLn('1. Creating async connection...');
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := DBPath;
      AsyncConn := TNDXSQLiteAsyncConnection.Create(Options);
    finally
      Options.Free;
    end;

    try
      // 2. Async open
      WriteLn('2. Opening connection asynchronously...');
      Handler.OperationComplete := False;
      AsyncConn.OpenAsync(@Handler.OnOpenComplete, nil);
      WaitForOperation;
      WriteLn('   ', Handler.OperationResult);
      WriteLn;

      // Create table synchronously for setup
      AsyncConn.ExecuteNonQuery(
        'CREATE TABLE items (' +
        '  id INTEGER PRIMARY KEY,' +
        '  name TEXT,' +
        '  value INTEGER' +
        ')');

      // Insert test data synchronously
      AsyncConn.BeginTransaction;
      AsyncConn.ExecuteNonQuery('INSERT INTO items (name, value) VALUES (?, ?)', ['Item A', 100]);
      AsyncConn.ExecuteNonQuery('INSERT INTO items (name, value) VALUES (?, ?)', ['Item B', 200]);
      AsyncConn.ExecuteNonQuery('INSERT INTO items (name, value) VALUES (?, ?)', ['Item C', 300]);
      AsyncConn.Commit;
      WriteLn('   Test data created.');
      WriteLn;

      // 3. Async query
      WriteLn('3. Executing async query...');
      Handler.OperationComplete := False;
      AsyncConn.ExecuteQueryAsync('SELECT * FROM items ORDER BY name', @Handler.OnQueryComplete, nil);
      WriteLn('   Query started in background...');
      WriteLn('   (Main thread can do other work here)');
      WaitForOperation;
      WriteLn('   ', Handler.OperationResult);
      WriteLn;

      // 4. Async scalar
      WriteLn('4. Executing async scalar query...');
      Handler.OperationComplete := False;
      AsyncConn.ExecuteScalarAsync('SELECT SUM(value) FROM items', @Handler.OnScalarComplete, nil);
      WaitForOperation;
      WriteLn('   ', Handler.OperationResult);
      WriteLn;

      // 5. Async non-query
      WriteLn('5. Executing async non-query...');
      Handler.OperationComplete := False;
      AsyncConn.ExecuteNonQueryAsync(
        'UPDATE items SET value = value * 2 WHERE value > ?', [150],
        @Handler.OnNonQueryComplete, nil);
      WaitForOperation;
      WriteLn('   ', Handler.OperationResult);
      WriteLn;

      // 6. Cancellation tokens
      WriteLn('6. Using cancellation tokens:');
      CancelTokenSource := TNDXCancellationTokenSource.Create;
      try
        WriteLn('   Token created, IsCancellationRequested: ', CancelTokenSource.Token.IsCancellationRequested);

        // Cancel immediately for demo
        CancelTokenSource.Cancel;
        WriteLn('   After Cancel(), IsCancellationRequested: ', CancelTokenSource.Token.IsCancellationRequested);

        Handler.OperationComplete := False;
        AsyncConn.ExecuteQueryAsync('SELECT * FROM items', @Handler.OnQueryComplete, CancelTokenSource.Token);
        WaitForOperation;
        WriteLn('   ', Handler.OperationResult);
        WriteLn;
      finally
        CancelTokenSource.Free;
      end;

      // 7. Multiple async operations
      WriteLn('7. Multiple async operations:');

      // Launch async operation 1
      Handler.OperationComplete := False;
      AsyncConn.ExecuteScalarAsync('SELECT COUNT(*) FROM items', @Handler.OnScalarComplete, nil);
      WaitForOperation;
      WriteLn('   Count query: ', Handler.OperationResult);

      // Launch async operation 2
      Handler.OperationComplete := False;
      AsyncConn.ExecuteScalarAsync('SELECT MAX(value) FROM items', @Handler.OnScalarComplete, nil);
      WaitForOperation;
      WriteLn('   Max query: ', Handler.OperationResult);
      WriteLn;

      // 8. Benefits summary
      WriteLn('8. Benefits of Async Operations:');
      WriteLn('   - Non-blocking UI');
      WriteLn('   - Better responsiveness');
      WriteLn('   - Cancellable operations');
      WriteLn('   - Parallel query execution');
      WriteLn('   - Progress reporting (for long operations)');
      WriteLn;

      // Sync close
      AsyncConn.Close;

    finally
      AsyncConn.Free;
    end;
  finally
    Handler.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
