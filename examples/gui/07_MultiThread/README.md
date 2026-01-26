# NDXSQLite GUI Example 07 - Multi-Thread with Connection Pool

A demonstration of thread-safe SQLite access using `TNDXSQLiteConnectionPool` for concurrent database operations.

## Overview

This example shows how to safely use SQLite in a multi-threaded application. Multiple worker threads perform concurrent database operations, each acquiring its own connection from a shared pool. The connection pool handles all synchronization automatically.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| Connection Pool | Thread-safe connection management |
| Worker Threads | Concurrent database operations |
| WAL Mode | Write-Ahead Logging for concurrent access |
| Thread Synchronization | Safe UI updates from worker threads |
| Progress Tracking | Real-time monitoring of thread activity |
| Graceful Cancellation | Clean shutdown of running threads |

## Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                         Main Thread (UI)                            │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │  TfrmMain                                                      │ │
│  │  - Creates and manages TNDXSQLiteConnectionPool                │ │
│  │  - Spawns worker threads                                       │ │
│  │  - Receives progress updates via TThread.Synchronize           │ │
│  └────────────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────────┘
         │
         │ Creates
         ▼
┌────────────────────────────────────────────────────────────────────┐
│                    TNDXSQLiteConnectionPool                         │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐           │
│  │ Conn #1  │  │ Conn #2  │  │ Conn #3  │  │ Conn #4  │           │
│  │ (active) │  │ (active) │  │ (avail.) │  │ (avail.) │           │
│  └────┬─────┘  └────┬─────┘  └──────────┘  └──────────┘           │
│       │             │                                              │
└───────┼─────────────┼──────────────────────────────────────────────┘
        │             │
        │ Acquire     │ Acquire
        ▼             ▼
┌───────────────┐  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│ Worker Thread │  │ Worker Thread │  │ Worker Thread │  │ Worker Thread │
│      #1       │  │      #2       │  │      #3       │  │      #4       │
│               │  │               │  │               │  │               │
│ INSERT INTO...│  │ INSERT INTO...│  │ INSERT INTO...│  │ INSERT INTO...│
└───────────────┘  └───────────────┘  └───────────────┘  └───────────────┘
        │             │
        │ Release     │ Release
        ▼             ▼
┌────────────────────────────────────────────────────────────────────┐
│                        SQLite Database                              │
│                     (WAL Mode Enabled)                              │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │ thread_results table                                           │ │
│  │ - Receives concurrent INSERTs from all threads                 │ │
│  │ - WAL mode allows concurrent writes with minimal blocking      │ │
│  └────────────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────────┘
```

## Key Concepts

### Why Use a Connection Pool?

1. **Thread Safety**: Each thread gets its own connection - no sharing
2. **Resource Efficiency**: Connections are reused, not created/destroyed repeatedly
3. **Controlled Concurrency**: Pool limits total connections to prevent overload
4. **Automatic Management**: Pool handles acquire/release synchronization

### WAL Mode Benefits

```
Without WAL (Rollback Journal):
  Writer blocks ALL readers
  Only one connection can write at a time

With WAL (Write-Ahead Logging):
  Writers don't block readers
  Readers see consistent snapshots
  Better performance for concurrent workloads
```

## Database Schema

```sql
CREATE TABLE thread_results (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    thread_id INTEGER NOT NULL,
    operation_num INTEGER NOT NULL,
    value REAL,
    created_at TEXT
);

CREATE INDEX idx_thread_results_thread ON thread_results(thread_id);
```

## Key Implementation Details

### Creating the Connection Pool

```pascal
procedure TfrmMain.CreatePool;
var
  Options: TNDXSQLiteConnectionOptions;
begin
  Options := TNDXSQLiteConnectionOptions.Create;
  Options.DatabasePath := 'mydb.db';
  Options.OpenMode := omReadWrite;
  Options.CreateIfMissing := True;
  Options.BusyTimeoutMs := 5000;  // Wait up to 5 seconds if busy

  FPool := TNDXSQLiteConnectionPool.Create(
    Options,
    2,   // Minimum connections to keep ready
    4    // Maximum connections allowed
  );
end;
```

### Worker Thread Implementation

```pascal
procedure TWorkerThread.Execute;
var
  Conn: INDXSQLitePooledConnection;
begin
  for I := 1 to FOperationCount do
  begin
    if Terminated then Break;

    // Acquire connection from pool (blocks if none available)
    Conn := FPool.Acquire;
    try
      // Perform database operation
      Conn.Connection.Execute('INSERT INTO ...');
    finally
      // Connection auto-released when Conn goes out of scope
      // (INDXSQLitePooledConnection is reference-counted)
    end;

    // Report progress to main thread
    Synchronize(@DoProgress);
  end;
end;
```

### Safe UI Updates

```pascal
procedure TWorkerThread.DoProgress;
begin
  // This runs in main thread context (via Synchronize)
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

// In main form:
procedure TfrmMain.OnThreadProgress(Sender: TObject);
var
  Thread: TWorkerThread;
begin
  Thread := TWorkerThread(Sender);
  FProgressBars[Thread.ThreadId - 1].Position := Thread.SuccessCount;
end;
```

## Usage

### Step 1: Configure Pool

- Set **Min connections**: Connections kept ready even when idle
- Set **Max connections**: Upper limit for concurrent connections

### Step 2: Configure Workers

- Set **Thread count**: Number of worker threads (1-4)
- Set **Operations per thread**: How many database operations each performs

### Step 3: Initialize and Run

1. Click **Initialize Database** - Creates pool and tables
2. Click **Start Workers** - Launches worker threads
3. Watch progress bars and log
4. Click **Stop Workers** to cancel (if needed)

## Building and Running

### Build
```bash
cd examples/gui/07_MultiThread
lazbuild MultiThreadDemo.lpi
```

### Run
- **Linux/macOS**: `./MultiThreadDemo`
- **Windows**: `MultiThreadDemo.exe`

## Project Files

| File | Description |
|------|-------------|
| `MultiThreadDemo.lpi` | Lazarus project configuration |
| `MultiThreadDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form and TWorkerThread implementation |
| `MainForm.lfm` | Visual form design |

## Best Practices

### Do's

1. **One connection per thread** - Never share connections across threads
2. **Use connection pool** - Don't create/destroy connections repeatedly
3. **Enable WAL mode** - Essential for concurrent write performance
4. **Set busy timeout** - Prevents immediate failures on contention
5. **Use Synchronize** - For all UI updates from worker threads

### Don'ts

1. **Don't share connections** - Each thread needs its own
2. **Don't hold connections** - Acquire, use, release quickly
3. **Don't ignore errors** - Handle SQLITE_BUSY and other errors
4. **Don't update UI directly** - Always use Synchronize

## Performance Tuning

| Setting | Effect |
|---------|--------|
| Pool Size | More connections = more parallelism (up to a point) |
| WAL Mode | Essential for concurrent writes |
| Busy Timeout | Higher = more resilient to contention |
| PRAGMA synchronous | NORMAL is faster than FULL |
| PRAGMA cache_size | Larger cache = fewer disk reads |

## Related Examples

- **05_Transactions**: Transaction management in single-threaded context
- **04_AsyncProgress**: Async operations without manual thread management
- **06_SearchFilter**: Database queries (can be combined with threading)

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
