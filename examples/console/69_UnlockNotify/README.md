# Example 69: Unlock Notify

This example demonstrates SQLite unlock_notify mechanism for handling database locks gracefully in concurrent access scenarios.

## What you'll learn

- How to check if unlock_notify is available
- How to simulate and handle database lock contention
- How to use two connections with exclusive locks
- How to use threads for concurrent database access
- How to use ExecuteWithRetry for automatic retry on lock
- How to configure retry parameters
- How to use the step retry helper class

## Key concepts

### The Problem: Database Locking

When multiple connections access the same SQLite database, locks can occur:
- **EXCLUSIVE lock**: One connection locks the entire database for writing
- **SQLITE_LOCKED**: Other connections get this error when trying to write

### Checking Availability

```pascal
if TNDXSQLiteUnlockNotify.IsAvailable then
  WriteLn('unlock_notify is available')
else
  WriteLn('Using fallback retry mechanism');
```

### Simulating Lock Contention

```pascal
// Connection 1: Hold exclusive lock
Conn1.ExecuteNonQuery('BEGIN EXCLUSIVE');
Conn1.ExecuteNonQuery('INSERT INTO table VALUES (1, ''data'')');
// Lock is held until COMMIT or ROLLBACK

// Connection 2: Try to write (will be blocked)
Unlock := TNDXSQLiteUnlockNotify.Create(Conn2);
try
  Unlock.DefaultTimeout := 5000;  // Wait up to 5 seconds
  Unlock.AutoRetry := True;
  Unlock.MaxRetries := 10;

  // This will retry until lock is released or timeout
  if Unlock.ExecuteWithRetry('INSERT INTO table VALUES (2, ''data'')') then
    WriteLn('Success!')
  else
    WriteLn('Failed: ', Unlock.LastError);
finally
  Unlock.Free;
end;

// Connection 1: Release lock
Conn1.Commit;
```

### Threaded Concurrent Access

```pascal
type
  TWriterThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TWriterThread.Execute;
var
  Conn: INDXSQLiteConnection;
  Unlock: TNDXSQLiteUnlockNotify;
begin
  Conn := TNDXSQLiteConnection.Create('database.db', False);
  Conn.Open;

  Unlock := TNDXSQLiteUnlockNotify.Create(Conn);
  try
    Unlock.DefaultTimeout := 5000;
    Unlock.AutoRetry := True;

    // Will wait for lock to be released
    Unlock.ExecuteWithRetry('INSERT INTO table VALUES (...)');
  finally
    Unlock.Free;
  end;

  Conn.Close;
end;
```

### Execute With Retry (Non-Query)

```pascal
RowsAffected := Unlock.ExecuteWithRetryNonQuery('UPDATE table SET x = 1');
if RowsAffected >= 0 then
  WriteLn('Updated ', RowsAffected, ' rows')
else
  WriteLn('Failed: ', Unlock.LastError);
```

### Wait for Unlock

```pascal
case Unlock.WaitForUnlock(5000) of
  uwrSuccess: WriteLn('Database unlocked');
  uwrTimeout: WriteLn('Timeout waiting');
  uwrError: WriteLn('Error occurred');
  uwrNotSupported: WriteLn('Not available');
end;
```

### Step Retry Helper

```pascal
Retry := TNDXSQLiteStepRetry.Create(Connection);
try
  Retry.MaxRetries := 10;
  Retry.RetryDelay := 100;  // 100ms between retries

  // Use with prepared statements
  RC := Retry.StepWithRetry(Statement);
finally
  Retry.Free;
end;
```

### Callback Notification

```pascal
procedure TMyHandler.OnUnlock(Sender: TObject);
begin
  WriteLn('Database was unlocked!');
end;

Unlock.OnUnlock := @MyHandler.OnUnlock;
```

## Wait Results

| Result | Description |
|--------|-------------|
| uwrSuccess | Database was unlocked successfully |
| uwrTimeout | Timeout waiting for unlock |
| uwrError | An error occurred |
| uwrNotSupported | unlock_notify not available |

## Building

```bash
lazbuild UnlockNotify.lpi
```

## Running

```bash
./UnlockNotify      # Linux/macOS
UnlockNotify.exe    # Windows
```

## Expected output

### When unlock_notify is available and concurrent connections work:

```
=== NDXSQLite Example 69: Unlock Notify ===

1. Checking Unlock Notify Availability:
   sqlite3_unlock_notify is AVAILABLE

2. Setting Up Database:
   Journal mode: WAL (Write-Ahead Logging)
   Created table with 2 products

3. Simulating Lock Contention (Two Connections):
   Connection 2: Opened successfully
   Connection 1: Starting IMMEDIATE transaction...
   Connection 1: Holding write lock (not committed yet)
   Connection 2: Attempting to write (should wait for lock)...
   Connection 2: INSERT blocked as expected - Max retries exceeded
   Connection 1: Committing transaction (releasing lock)...
   Connection 2: INSERT succeeded after lock released!

4. Threaded Lock Contention Demo:
   Main thread: Starting IMMEDIATE transaction...
   Main thread: Holding write lock for 1 second...
   Writer thread: Started, attempting to write...
   Main thread: Committing (releasing lock)...
   Writer thread: INSERT succeeded after lock was released

5. Configuring Unlock Notify Settings:
   ...

=== Example completed successfully! ===
```

### When unlock_notify is NOT available:

```
=== NDXSQLite Example 69: Unlock Notify ===

1. Checking Unlock Notify Availability:
   sqlite3_unlock_notify is NOT available (fallback retry will be used)

2. Setting Up Database:
   Journal mode: WAL (Write-Ahead Logging)
   Created table with 2 products

3. Simulating Lock Contention (Two Connections):
   Connection 2: Could not open - Unable to open database: database is locked
   (Skipping two-connection demo)
   (Two-connection demo skipped - added products directly)

4. Threaded Lock Contention Demo:
   Main thread: Starting IMMEDIATE transaction...
   Main thread: Holding write lock for 1 second...
   Writer thread: Started, attempting to write...
   Main thread: Committing (releasing lock)...
   Writer thread: FAILED - Exception: Unable to open database: database is locked

5. Configuring Unlock Notify Settings:
   Default settings:
     Timeout: 30000ms
     Auto retry: TRUE
     Max retries: 5
   ...

6. Execute With Retry - Various Operations:
   INSERT: OK
   UPDATE: OK - 5 rows affected
   DELETE: OK - 1 row(s) deleted

...

10. Final Database State:
    Total products: 4
    Total value: $111.06

=== Example completed successfully! ===
```

## When to use

- **WAL mode**: Multiple readers with one writer
- **Shared cache mode**: Multiple connections sharing cache
- **High concurrency**: Many connections to the same database
- **Long transactions**: When transactions may block others
- **Connection pooling**: Managing pool of database connections
- **Multi-threaded applications**: Multiple threads accessing same database

## Lock Types in SQLite

| Lock Type | Description |
|-----------|-------------|
| SHARED | Multiple readers allowed |
| RESERVED | Writer preparing to write |
| PENDING | Writer waiting for readers |
| EXCLUSIVE | Single writer, no readers |

## Best Practices

1. **Use WAL mode** for better concurrency:
   ```pascal
   Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');
   ```

2. **Keep transactions short** to minimize lock duration

3. **Set reasonable timeouts** to avoid infinite waits

4. **Use retry with backoff** for high-contention scenarios

5. **Consider connection pooling** for multi-threaded apps

## Notes

- `unlock_notify` requires SQLite compiled with `SQLITE_ENABLE_UNLOCK_NOTIFY`
- If not available, the wrapper provides fallback retry with sleep
- WAL mode allows concurrent readers with one writer
- IMMEDIATE/EXCLUSIVE transactions block other writers

## System Requirements for Multi-Connection Demos

The multi-connection demonstrations (sections 3 and 4) require:
- SQLite built with proper concurrent access support
- File system that supports proper file locking
- WAL mode enabled (recommended for concurrency)

If these requirements are not met, the demo will gracefully skip the multi-connection scenarios and show the API functionality using a single connection.

## Cross-Platform

This example works on Windows, Linux, and macOS.
