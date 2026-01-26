{===============================================================================
  NDXSQLite Example 36 - Cancellable Queries and Operations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Using TNDXCancellationTokenSource for cancellation
  - IsCancellationRequested polling pattern
  - ThrowIfCancellationRequested exception pattern
  - Reset for token reuse
  - TNDXCancellationToken.None for non-cancellable operations

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CancellableQueries;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions, ndxsqlitecancellation;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
var
  I: Integer;
begin
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS data (id INTEGER PRIMARY KEY, value TEXT)');

  // Insert sample data
  Connection.BeginTransaction;
  for I := 1 to 10000 do
    Connection.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)',
      [StringOfChar('X', 50)]);
  Connection.Commit;
end;

{ Creates a cancellation token, checks its initial state, calls Cancel, and verifies the state changes to cancelled. }
procedure DemoBasicCancellation;
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
begin
  WriteLn('1. Basic cancellation pattern');
  WriteLn('   --------------------------');

  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.Token;

    WriteLn('   Token created, checking status...');
    WriteLn('   IsCancellationRequested: ', Token.IsCancellationRequested);

    WriteLn('   Calling Cancel...');
    CTS.Cancel;

    WriteLn('   IsCancellationRequested: ', Token.IsCancellationRequested);
    WriteLn('');
    WriteLn('   Key point: Token remains valid after source cancels');
    WriteLn('');
  finally
    CTS.Free;
  end;
end;

{ Iterates query results and checks IsCancellationRequested every 1000 rows, cancelling at row 5000 to show the polling approach. }
procedure DemoPollingPattern;
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
  Count: Integer;
  DS: TDataSet;
begin
  WriteLn('2. Polling pattern (IsCancellationRequested)');
  WriteLn('   ------------------------------------------');

  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.Token;

    // Simulate cancellation after some processing
    WriteLn('   Processing rows with periodic cancellation check...');

    DS := Connection.ExecuteQuery('SELECT * FROM data');
    try
      Count := 0;
      while not DS.EOF do
      begin
        Inc(Count);

        // Check cancellation every 1000 rows
        if (Count mod 1000) = 0 then
        begin
          if Token.IsCancellationRequested then
          begin
            WriteLn('   Cancellation detected at row ', Count);
            Break;
          end;
          Write('   Processed ', Count, ' rows... ');

          // Simulate user requesting cancellation at row 5000
          if Count = 5000 then
          begin
            WriteLn('(Cancelling now)');
            CTS.Cancel;
          end
          else
            WriteLn('');
        end;

        DS.Next;
      end;

      WriteLn('   Processing stopped. Total processed: ', Count, ' rows');
    finally
      DS.Free;
    end;

    WriteLn('');
    WriteLn('   Polling pattern: Check IsCancellationRequested at safe points');
    WriteLn('');
  finally
    CTS.Free;
  end;
end;

{ Runs a loop that calls ThrowIfCancellationRequested each iteration, catching ENDXOperationCanceledException when cancelled at step 5. }
procedure DemoExceptionPattern;
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
  I: Integer;
begin
  WriteLn('3. Exception pattern (ThrowIfCancellationRequested)');
  WriteLn('   -------------------------------------------------');

  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.Token;

    WriteLn('   Processing with exception-based cancellation...');

    try
      for I := 1 to 10 do
      begin
        WriteLn('   Step ', I, ' of 10...');

        // Simulate cancellation at step 6
        if I = 5 then
          CTS.Cancel;

        // This will raise ENDXOperationCanceledException if canceled
        Token.ThrowIfCancellationRequested;

        Sleep(100);
      end;
      WriteLn('   Completed all steps');
    except
      on E: ENDXOperationCanceledException do
        WriteLn('   Caught: ', E.Message);
    end;

    WriteLn('');
    WriteLn('   Exception pattern: Let exceptions propagate for cleanup');
    WriteLn('');
  finally
    CTS.Free;
  end;
end;

{ Demonstrates resetting a cancellation token for reuse across queries. }
procedure DemoReset;
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
begin
  WriteLn('4. Reset for token reuse');
  WriteLn('   ---------------------');

  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.Token;

    WriteLn('   Initial state: ', Token.IsCancellationRequested);

    WriteLn('   Cancelling...');
    CTS.Cancel;
    WriteLn('   After Cancel: ', Token.IsCancellationRequested);

    WriteLn('   Resetting...');
    CTS.Reset;
    WriteLn('   After Reset: ', Token.IsCancellationRequested);

    WriteLn('');
    WriteLn('   Reset: Reuse token source without recreating');
    WriteLn('');
  finally
    CTS.Free;
  end;
end;

{ Obtains the static TNDXCancellationToken.None token and shows it is never in a cancelled state. }
procedure DemoNoneToken;
var
  Token: INDXCancellationToken;
begin
  WriteLn('5. TNDXCancellationToken.None');
  WriteLn('   --------------------------');

  // Get the static "None" token (never canceled)
  Token := TNDXCancellationToken.None;

  WriteLn('   None token IsCancellationRequested: ', Token.IsCancellationRequested);

  WriteLn('');
  WriteLn('   Use TNDXCancellationToken.None when:');
  WriteLn('     - Function requires a token but cancellation not needed');
  WriteLn('     - Passing default parameter to cancellable operations');
  WriteLn('');
end;

{ Processes a large result set and cancels at row 3000, simulating a timeout-based cancellation of a long-running query. }
procedure DemoLongQueryCancellation;
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
  DS: TDataSet;
  RowCount: Integer;
  Cancelled: Boolean;
  MaxRows: Integer;
begin
  WriteLn('6. Long query with manual timeout simulation');
  WriteLn('   -----------------------------------------');

  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.Token;
    MaxRows := 3000; // Simulate timeout by cancelling at row 3000

    WriteLn('   Starting large query (will cancel at row ', MaxRows, ')...');
    WriteLn('   (Processing 10000 rows with simulated work)');

    DS := Connection.ExecuteQuery('SELECT * FROM data');
    try
      RowCount := 0;
      Cancelled := False;

      while not DS.EOF do
      begin
        Inc(RowCount);

        // Simulate a timeout by cancelling at MaxRows
        if RowCount = MaxRows then
          CTS.Cancel;

        // Check for cancellation periodically
        if (RowCount mod 500) = 0 then
        begin
          if Token.IsCancellationRequested then
          begin
            WriteLn('   Query cancelled at row ', RowCount);
            Cancelled := True;
            Break;
          end;
        end;

        DS.Next;
      end;

      if not Cancelled then
        WriteLn('   Query completed: ', RowCount, ' rows');

    finally
      DS.Free;
    end;

    WriteLn('');
    WriteLn('   Pattern: Use timeouts/limits to control long operations');
    WriteLn('');
  finally
    CTS.Free;
  end;
end;

{ Executes a multi-phase workflow using both polling and ThrowIfCancellationRequested, cancelling during phase 2 at step 4. }
procedure DemoCombinedPatterns;
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
  I: Integer;
  ShouldCancel: Boolean;
begin
  WriteLn('7. Combined patterns example');
  WriteLn('   -------------------------');

  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.Token;
    ShouldCancel := False;

    WriteLn('   Starting multi-phase operation...');

    try
      // Phase 1: Quick check
      WriteLn('   Phase 1: Initialization');
      Token.ThrowIfCancellationRequested;
      Sleep(100);

      // Phase 2: Processing loop
      WriteLn('   Phase 2: Processing');
      for I := 1 to 5 do
      begin
        // Simulate external cancellation request at step 4
        if I = 4 then
        begin
          ShouldCancel := True;
          CTS.Cancel;
        end;

        if Token.IsCancellationRequested then
        begin
          WriteLn('     Cancelled during processing at step ', I);
          raise ENDXOperationCanceledException.Create;
        end;
        Write('     Step ', I, '/5...');
        Sleep(100);
        WriteLn(' done');
      end;

      // Phase 3: Finalization
      WriteLn('   Phase 3: Finalization');
      Token.ThrowIfCancellationRequested;
      Sleep(100);

      WriteLn('   Operation completed successfully!');

    except
      on E: ENDXOperationCanceledException do
        WriteLn('   Operation cancelled: ', E.Message);
    end;

    WriteLn('');
    WriteLn('   Combine polling + exceptions for complex workflows');
    WriteLn('');
  finally
    CTS.Free;
  end;
end;

{ Runs three batch operations reusing a single TokenSource by calling Reset before each batch, cancelling only batch 2 at step 3. }
procedure DemoSourceReusability;
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
  I, J: Integer;
begin
  WriteLn('8. Source reusability pattern');
  WriteLn('   --------------------------');

  CTS := TNDXCancellationTokenSource.Create;
  try
    for I := 1 to 3 do
    begin
      CTS.Reset;  // Reset before each use
      Token := CTS.Token;

      WriteLn('   Batch ', I, ':');

      // Simulate batch processing that might be cancelled
      for J := 1 to 5 do
      begin
        if Token.IsCancellationRequested then
        begin
          WriteLn('     Cancelled at step ', J);
          Break;
        end;

        Write('     Step ', J, '... ');

        // Cancel batch 2 at step 3
        if (I = 2) and (J = 3) then
        begin
          CTS.Cancel;
          WriteLn('CANCELLED');
        end
        else
          WriteLn('ok');
      end;
    end;

    WriteLn('');
    WriteLn('   Reset allows reusing one TokenSource for multiple operations');
    WriteLn('');
  finally
    CTS.Free;
  end;
end;

{ Prints a summary of common cancellation scenarios: user-initiated, timeout-based, graceful shutdown, and resource limits. }
procedure DemoCancellationUseCases;
begin
  WriteLn('9. Common cancellation use cases');
  WriteLn('   -----------------------------');
  WriteLn('');
  WriteLn('   1. User-initiated cancellation (Cancel button)');
  WriteLn('      - User clicks Cancel, CTS.Cancel is called');
  WriteLn('      - Background operation checks IsCancellationRequested');
  WriteLn('');
  WriteLn('   2. Timeout-based cancellation');
  WriteLn('      - Set a timer, call CTS.Cancel when it fires');
  WriteLn('      - Or use CTS.CancelAfter(milliseconds)');
  WriteLn('');
  WriteLn('   3. Graceful shutdown');
  WriteLn('      - Application receives shutdown signal');
  WriteLn('      - Global CTS.Cancel() stops all operations');
  WriteLn('');
  WriteLn('   4. Resource limits');
  WriteLn('      - Cancel when row count exceeds limit');
  WriteLn('      - Cancel when memory usage too high');
  WriteLn('');
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 36: Cancellable Queries ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example36.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupDatabase;
      WriteLn('Database setup complete (10000 rows)');
      WriteLn('');

      DemoBasicCancellation;
      DemoPollingPattern;
      DemoExceptionPattern;
      DemoReset;
      DemoNoneToken;
      DemoLongQueryCancellation;
      DemoCombinedPatterns;
      DemoSourceReusability;
      DemoCancellationUseCases;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
