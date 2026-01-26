{===============================================================================
  NDXSQLite Example 69 - Unlock Notify
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Checking unlock_notify availability
  - Simulating database lock contention with two connections
  - Using ExecuteWithRetry for automatic retry on lock
  - Using threads to demonstrate concurrent access
  - Configuring retry parameters
  - Using the step retry helper

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program UnlockNotify;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Variants,
  ndxsqlite3api,
  ndxsqliteconnection, ndxsqliteconnectionintf,
  ndxsqliteunlocknotify;

const
  DB_FILE = 'unlock_demo.db';

type
  { Writer thread - tries to write while main thread holds a lock }
  TWriterThread = class(TThread)
  private
    FSuccess: Boolean;
    FMessage: string;
    FRetryCount: Integer;
  protected
    procedure Execute; override;
  public
    property Success: Boolean read FSuccess;
    property Message: string read FMessage;
    property RetryCount: Integer read FRetryCount;
  end;

{ Executes write operations using unlock-notify for retry handling. }
procedure TWriterThread.Execute;
var
  Conn2: INDXSQLiteConnection;
  Unlock: TNDXSQLiteUnlockNotify;
begin
  FSuccess := False;
  FRetryCount := 0;

  try
    // Create second connection to the same database
    Conn2 := TNDXSQLiteConnection.Create(DB_FILE, False);
    Conn2.Open;

    Unlock := TNDXSQLiteUnlockNotify.Create(Conn2);
    try
      Unlock.DefaultTimeout := 3000;  // 3 seconds timeout
      Unlock.AutoRetry := True;
      Unlock.MaxRetries := 10;

      // Try to insert while main connection has exclusive lock
      // This will retry until lock is released or timeout
      FSuccess := Unlock.ExecuteWithRetry(
        'INSERT INTO products VALUES (100, ''ThreadProduct'', 99.99)');

      if FSuccess then
        FMessage := 'INSERT succeeded after lock was released'
      else
        FMessage := 'INSERT failed: ' + Unlock.LastError;

    finally
      Unlock.Free;
    end;

    Conn2.Close;
    Conn2 := nil;
  except
    on E: Exception do
    begin
      FMessage := 'Exception: ' + E.Message;
      FSuccess := False;
    end;
  end;
end;

var
  Conn: INDXSQLiteConnection;
  Conn2: INDXSQLiteConnection;
  Unlock: TNDXSQLiteUnlockNotify;
  Retry: TNDXSQLiteStepRetry;
  WriterThread: TWriterThread;
  V: Variant;
  Success: Boolean;
  RowsAffected: Integer;
  I: Integer;

begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 69: Unlock Notify ===');
  WriteLn('');

  // Cleanup
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  // =====================================================================
  // 1. Check Availability
  // =====================================================================
  WriteLn('1. Checking Unlock Notify Availability:');
  if TNDXSQLiteUnlockNotify.IsAvailable then
    WriteLn('   sqlite3_unlock_notify is AVAILABLE')
  else
    WriteLn('   sqlite3_unlock_notify is NOT available (fallback retry will be used)');
  WriteLn('');

  // =====================================================================
  // 2. Setup Database
  // =====================================================================
  WriteLn('2. Setting Up Database:');
  Conn := TNDXSQLiteConnection.Create(DB_FILE, False);
  Conn.Open;

  // Use WAL mode for better concurrency demonstration
  Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');
  WriteLn('   Journal mode: WAL (Write-Ahead Logging)');

  Conn.ExecuteNonQuery('CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT, price REAL)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (1, ''Widget'', 9.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (2, ''Gadget'', 19.99)');
  WriteLn('   Created table with 2 products');

  // Checkpoint to ensure WAL is flushed
  Conn.ExecuteNonQuery('PRAGMA wal_checkpoint(TRUNCATE)');
  WriteLn('');

  // =====================================================================
  // 3. Simulate Lock Contention (Two Connections)
  // =====================================================================
  WriteLn('3. Simulating Lock Contention (Two Connections):');

  // Open second connection BEFORE starting any transaction
  try
    Conn2 := TNDXSQLiteConnection.Create(DB_FILE, False);
    Conn2.Open;
    WriteLn('   Connection 2: Opened successfully');
  except
    on E: Exception do
    begin
      WriteLn('   Connection 2: Could not open - ', E.Message);
      WriteLn('   (Skipping two-connection demo)');
      Conn2 := nil;
    end;
  end;

  if Conn2 <> nil then
  begin
    WriteLn('   Connection 1: Starting IMMEDIATE transaction...');

    // Begin immediate transaction on first connection (holds RESERVED lock)
    Conn.ExecuteNonQuery('BEGIN IMMEDIATE');
    Conn.ExecuteNonQuery('INSERT INTO products VALUES (3, ''ExclusiveProduct'', 29.99)');
    WriteLn('   Connection 1: Holding write lock (not committed yet)');

    // Try to write from second connection (will be blocked)
    WriteLn('   Connection 2: Attempting to write (should wait for lock)...');

    Unlock := TNDXSQLiteUnlockNotify.Create(Conn2);
    try
      Unlock.DefaultTimeout := 100;  // Short timeout for demo
      Unlock.AutoRetry := True;
      Unlock.MaxRetries := 3;

      // This should fail because Conn holds write lock
      Success := Unlock.ExecuteWithRetry('INSERT INTO products VALUES (4, ''BlockedProduct'', 39.99)');

      if Success then
        WriteLn('   Connection 2: INSERT succeeded (unexpected)')
      else
        WriteLn('   Connection 2: INSERT blocked as expected - ', Unlock.LastError);
    finally
      Unlock.Free;
    end;

    // Release the lock
    WriteLn('   Connection 1: Committing transaction (releasing lock)...');
    Conn.Commit;

    // Now try again - should succeed
    Unlock := TNDXSQLiteUnlockNotify.Create(Conn2);
    try
      Unlock.DefaultTimeout := 5000;
      Unlock.AutoRetry := True;
      Unlock.MaxRetries := 5;

      Success := Unlock.ExecuteWithRetry('INSERT INTO products VALUES (4, ''UnblockedProduct'', 39.99)');

      if Success then
        WriteLn('   Connection 2: INSERT succeeded after lock released!')
      else
        WriteLn('   Connection 2: INSERT still failed - ', Unlock.LastError);
    finally
      Unlock.Free;
    end;

    Conn2.Close;
    Conn2 := nil;
  end
  else
  begin
    // Single connection fallback - just show the API works
    Conn.ExecuteNonQuery('INSERT INTO products VALUES (3, ''ExclusiveProduct'', 29.99)');
    Conn.ExecuteNonQuery('INSERT INTO products VALUES (4, ''UnblockedProduct'', 39.99)');
    WriteLn('   (Two-connection demo skipped - added products directly)');
  end;
  WriteLn('');

  // =====================================================================
  // 4. Threaded Lock Contention Demo
  // =====================================================================
  WriteLn('4. Threaded Lock Contention Demo:');
  WriteLn('   Main thread: Starting IMMEDIATE transaction...');

  // Use IMMEDIATE instead of EXCLUSIVE - less aggressive locking
  Conn.ExecuteNonQuery('BEGIN IMMEDIATE');
  Conn.ExecuteNonQuery('UPDATE products SET price = price + 1 WHERE id = 1');
  WriteLn('   Main thread: Holding write lock for 1 second...');

  // Start writer thread
  WriterThread := TWriterThread.Create(True);  // Create suspended
  WriterThread.FreeOnTerminate := False;
  WriterThread.Start;

  WriteLn('   Writer thread: Started, attempting to write...');

  // Hold lock for 1 second
  Sleep(1000);

  // Release lock
  WriteLn('   Main thread: Committing (releasing lock)...');
  Conn.Commit;

  // Wait for writer thread to complete
  WriterThread.WaitFor;

  if WriterThread.Success then
    WriteLn('   Writer thread: ', WriterThread.Message)
  else
    WriteLn('   Writer thread: FAILED - ', WriterThread.Message);

  WriterThread.Free;
  WriteLn('');

  // =====================================================================
  // 5. Configure Unlock Notify Settings
  // =====================================================================
  WriteLn('5. Configuring Unlock Notify Settings:');
  Unlock := TNDXSQLiteUnlockNotify.Create(Conn);
  try
    WriteLn('   Default settings:');
    WriteLn('     Timeout: ', Unlock.DefaultTimeout, 'ms');
    WriteLn('     Auto retry: ', Unlock.AutoRetry);
    WriteLn('     Max retries: ', Unlock.MaxRetries);

    Unlock.DefaultTimeout := 10000;  // 10 seconds
    Unlock.AutoRetry := True;
    Unlock.MaxRetries := 20;

    WriteLn('   New settings:');
    WriteLn('     Timeout: ', Unlock.DefaultTimeout, 'ms');
    WriteLn('     Auto retry: ', Unlock.AutoRetry);
    WriteLn('     Max retries: ', Unlock.MaxRetries);
  finally
    Unlock.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 6. Execute With Retry - Various Operations
  // =====================================================================
  WriteLn('6. Execute With Retry - Various Operations:');
  Unlock := TNDXSQLiteUnlockNotify.Create(Conn);
  try
    Unlock.DefaultTimeout := 5000;
    Unlock.AutoRetry := True;
    Unlock.MaxRetries := 5;

    // INSERT
    Write('   INSERT: ');
    Success := Unlock.ExecuteWithRetry('INSERT INTO products VALUES (5, ''RetryProduct'', 49.99)');
    if Success then
      WriteLn('OK')
    else
      WriteLn('FAILED - ', Unlock.LastError);

    // UPDATE
    Write('   UPDATE: ');
    RowsAffected := Unlock.ExecuteWithRetryNonQuery('UPDATE products SET price = price * 1.1');
    if RowsAffected >= 0 then
      WriteLn('OK - ', RowsAffected, ' rows affected')
    else
      WriteLn('FAILED - ', Unlock.LastError);

    // DELETE
    Write('   DELETE: ');
    RowsAffected := Unlock.ExecuteWithRetryNonQuery('DELETE FROM products WHERE id = 5');
    if RowsAffected >= 0 then
      WriteLn('OK - ', RowsAffected, ' row(s) deleted')
    else
      WriteLn('FAILED - ', Unlock.LastError);
  finally
    Unlock.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 7. Step Retry Helper
  // =====================================================================
  WriteLn('7. Step Retry Helper:');
  WriteLn('   For low-level prepared statement stepping with retry');
  Retry := TNDXSQLiteStepRetry.Create(Conn);
  try
    WriteLn('   Default max retries: ', Retry.MaxRetries);
    WriteLn('   Default retry delay: ', Retry.RetryDelay, 'ms');

    Retry.MaxRetries := 10;
    Retry.RetryDelay := 50;

    WriteLn('   Configured max retries: ', Retry.MaxRetries);
    WriteLn('   Configured retry delay: ', Retry.RetryDelay, 'ms');
    WriteLn('   (Use StepWithRetry(stmt) for prepared statements)');
  finally
    Retry.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 8. Wait Result Types
  // =====================================================================
  WriteLn('8. Wait Result Types:');
  WriteLn('   uwrSuccess     - Database was unlocked successfully');
  WriteLn('   uwrTimeout     - Timeout waiting for unlock');
  WriteLn('   uwrError       - An error occurred');
  WriteLn('   uwrNotSupported - unlock_notify not available');
  WriteLn('');

  // =====================================================================
  // 9. Callback Notification
  // =====================================================================
  WriteLn('9. Callback Notification:');
  WriteLn('   You can assign OnUnlock event for notification:');
  WriteLn('');
  WriteLn('   procedure TMyHandler.HandleUnlock(Sender: TObject);');
  WriteLn('   begin');
  WriteLn('     WriteLn(''Database unlocked!'');');
  WriteLn('   end;');
  WriteLn('');
  WriteLn('   Unlock.OnUnlock := @MyHandler.HandleUnlock;');
  WriteLn('');

  // =====================================================================
  // 10. Final Database State
  // =====================================================================
  WriteLn('10. Final Database State:');
  V := Conn.ExecuteScalar('SELECT COUNT(*) FROM products');
  WriteLn('    Total products: ', Integer(V));
  V := Conn.ExecuteScalar('SELECT SUM(price) FROM products');
  WriteLn('    Total value: $', FormatFloat('0.00', Double(V)));

  WriteLn('');
  WriteLn('    Product list:');
  // Show all products
  for I := 1 to 10 do
  begin
    try
      V := Conn.ExecuteScalar('SELECT name FROM products WHERE id = ' + IntToStr(I));
      if not VarIsNull(V) then
        WriteLn('      ID ', I, ': ', VarToStr(V));
    except
      // Product doesn't exist
    end;
  end;
  WriteLn('');

  Conn.Close;
  Conn := nil;

  // Cleanup
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
  if FileExists(DB_FILE + '-wal') then
    DeleteFile(DB_FILE + '-wal');
  if FileExists(DB_FILE + '-shm') then
    DeleteFile(DB_FILE + '-shm');

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
