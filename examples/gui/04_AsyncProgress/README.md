# NDXSQLite GUI Example 04 - Asynchronous Operations with Progress

Demonstrates non-blocking database operations with progress reporting, cancellation support, and responsive UI.

## Overview

This example showcases `TNDXSQLiteAsyncConnection` for performing long-running database operations in background threads while keeping the user interface responsive. It includes real-time progress updates, operation logging, and cooperative cancellation.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| Async Connection | Non-blocking database open and close |
| Background Queries | Execute SELECT statements without freezing UI |
| Batch Insert | Insert thousands of records with progress reporting |
| Async Backup | Hot database backup with page-by-page progress |
| Cancellation Tokens | Cooperative cancellation of long-running operations |
| Synchronized Callbacks | Safe UI updates from background threads |

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                           TfrmMain                                  │
├─────────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ Records: [10000 ▾]  [Insert Data] [Query Data] [Backup DB]  │    │
│  │                                              [Cancel]       │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ [████████████████████░░░░░░░░░░] 65%  Inserting records...  │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ DBGrid (benchmark_data)                                      │   │
│  │ ┌────┬─────────────────┬─────────┬─────────────────────┐     │   │
│  │ │ ID │ Name            │ Value   │ Created At          │     │   │
│  │ ├────┼─────────────────┼─────────┼─────────────────────┤     │   │
│  │ │ 1  │ Item_1          │ 123.45  │ 2026-01-25 10:30:00 │     │   │
│  │ │ 2  │ Item_2          │ 678.90  │ 2026-01-25 10:30:00 │     │   │
│  │ └────┴─────────────────┴─────────┴─────────────────────┘     │   │
│  └──────────────────────────────────────────────────────────────┘   │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ Log:                                                         │   │
│  │ [10:30:00.123] Application started                           │   │
│  │ [10:30:00.456] Database opened successfully                  │   │
│  │ [10:30:05.789] Insert completed: 10000 rows (4523 ms)        │   │
│  │ [10:30:06.012] Query completed: 1000 records (89 ms)         │   │
│  └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────────┐
│                  TNDXSQLiteAsyncConnection                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Main Thread (UI)                    Background Thread              │
│       │                                    │                        │
│       │── OpenAsync() ──────────────────>  │                        │
│       │   (returns immediately)            │── sqlite3_open()       │
│       │                                    │                        │
│       │<── OnOpenComplete() ──────────────│                        │
│       │   (synchronized to main thread)    │                        │
│       │                                    │                        │
│       │── ExecuteQueryAsync() ──────────> │                        │
│       │   (UI stays responsive)            │── sqlite3_prepare()    │
│       │                                    │── sqlite3_step() ×N    │
│       │                                    │                        │
│       │<── OnQueryComplete() ─────────────│                        │
│       │   (results delivered safely)       │                        │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
                          │
                          ▼
                 ┌───────────────┐
                 │ SQLite Database│
                 │ (async_demo.db)│
                 └───────────────┘
```

## Database Schema

```sql
CREATE TABLE benchmark_data (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT,
    value REAL,
    created_at TEXT DEFAULT (datetime('now', 'localtime'))
);
```

## Key Implementation Details

### Creating Async Connection

```pascal
// Create async connection with auto-create flag
FConnection := TNDXSQLiteAsyncConnection.Create(DBPath, True);

// Open asynchronously - callback invoked on main thread when complete
FConnection.OpenAsync(@OnOpenComplete, nil);
```

### Async Query with Callback

```pascal
// Execute query in background thread
FConnection.ExecuteQueryAsync(
  'SELECT id, name, value, created_at FROM benchmark_data ORDER BY id DESC LIMIT 1000',
  @OnQueryComplete, FCancellationSource.Token);

// Callback runs on main thread when query completes
procedure TfrmMain.OnQueryComplete(const AResult: TNDXAsyncResultDataSet);
begin
  if AResult.Success then
  begin
    // Safe to update UI here - we're on the main thread
    Log(Format('Query completed: %d records (%d ms)',
      [AResult.Data.RecordCount, AResult.ExecutionTimeMs]));
  end
  else
    Log('ERROR: ' + AResult.ErrorMessage);
end;
```

### Cancellation Token Pattern

```pascal
// Create cancellation source before starting operation
FCancellationSource := TNDXCancellationTokenSource.Create;

// Pass token to async operation
FConnection.ExecuteNonQueryAsync(SQL, @OnInsertComplete,
  FCancellationSource.Token);

// Cancel button handler
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  if Assigned(FCancellationSource) then
  begin
    FCancellationSource.Cancel;           // Signal cancellation
    FConnection.InterruptCurrentOperation; // Interrupt SQLite
  end;
end;
```

### Async Backup with Progress

```pascal
// Start backup with dual callbacks
FConnection.BackupToAsync(BackupPath,
  @OnBackupProgress,   // Called repeatedly during backup
  @OnBackupComplete,   // Called once when finished
  FCancellationSource.Token);

// Progress callback updates UI
procedure TfrmMain.OnBackupProgress(ACurrent, ATotal: Integer;
  const AMessage: string);
begin
  ProgressBar1.Max := ATotal;
  ProgressBar1.Position := ACurrent;
  lblProgress.Caption := Format('Backup: %d%%', [(ACurrent * 100) div ATotal]);
end;
```

### Proper Resource Cleanup

```pascal
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Cancel any pending operations
  if Assigned(FCancellationSource) then
  begin
    FCancellationSource.Cancel;
    FreeAndNil(FCancellationSource);
  end;

  // Wait for background operations with timeout
  if Assigned(FConnection) then
  begin
    FConnection.WaitForAllOperations(5000);  // 5 second timeout
    FConnection.Free;
  end;
end;
```

## Async Operations Summary

| Method | Return Type | Use Case |
|--------|-------------|----------|
| `OpenAsync` | Boolean | Non-blocking database connection |
| `ExecuteNonQueryAsync` | Integer (rows affected) | INSERT, UPDATE, DELETE |
| `ExecuteQueryAsync` | TDataSet | SELECT queries |
| `BackupToAsync` | Boolean | Hot database backup |
| `WaitForAllOperations` | - | Clean shutdown |
| `InterruptCurrentOperation` | - | Cancel running operation |

## Building and Running

### Build
```bash
cd examples/gui/04_AsyncProgress
lazbuild AsyncProgressDemo.lpi
```

### Run
- **Linux/macOS**: `./AsyncProgressDemo`
- **Windows**: `AsyncProgressDemo.exe`

The database file `async_demo.db` is created automatically on first run.

## Project Files

| File | Description |
|------|-------------|
| `AsyncProgressDemo.lpi` | Lazarus project configuration |
| `AsyncProgressDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form with async operation logic |
| `MainForm.lfm` | Visual form design |

## Testing the Feature

1. **Batch Insert**: Set count to 10000+, click "Insert Data"
   - UI remains responsive during insert
   - Progress bar updates in real-time
   - Try clicking "Cancel" mid-operation

2. **Query Data**: Click "Query Data"
   - Records load in background
   - Grid populates when complete
   - Timing shown in log

3. **Database Backup**: Click "Backup DB"
   - Progress bar shows page progress
   - Backup file appears in app directory
   - Cancellable mid-backup

4. **Close During Operation**: Start an operation and close the form
   - Prompted to cancel or wait
   - Proper cleanup prevents crashes

## Common Use Cases

| Scenario | Benefit |
|----------|---------|
| Large Data Imports | Import millions of records without UI freeze |
| Complex Analytics | Run aggregation queries in background |
| Scheduled Backups | Create backups while users continue working |
| Network Storage | Handle high-latency database access |
| Batch Processing | Process records with progress feedback |

## Requirements

- **IDE**: Lazarus 2.2+ with Free Pascal 3.2+
- **Library**: NDXSQLite (parent project)
- **Platform**: Windows, Linux, or macOS

## Cross-Platform Notes

This example works identically on all platforms. Threading is handled by the Free Pascal RTL which provides cross-platform thread support. Callback synchronization uses `TThread.Synchronize` for safe UI updates.

## Related Examples

- **01_DBGrid**: Basic single-table CRUD operations
- **02_MasterDetail**: Two linked datasets with automatic filtering
- **03_CachedUpdates**: Batch editing with Apply/Cancel

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
