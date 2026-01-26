# NDXSQLite GUI Example 05 - Transaction Management

A comprehensive demonstration of SQLite transaction control including explicit transactions, savepoints, and rollback operations.

## Overview

This example provides an interactive interface for understanding SQLite transaction semantics. Users can manually control transactions, create savepoints for partial rollbacks, and observe the effects of commit and rollback operations in real-time.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| Explicit Transactions | Manual BEGIN, COMMIT, and ROLLBACK control |
| Savepoints | Nested transaction markers for partial rollback |
| Rollback to Savepoint | Undo changes back to a specific point |
| Release Savepoint | Remove savepoint while keeping changes |
| WAL Mode | Write-Ahead Logging for better concurrency |
| Transaction State UI | Visual feedback of current transaction status |

## Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                         TfrmMain                                    │
├──────────────────────────┬─────────────────────────────────────────┤
│  Transaction Control     │              Data View                   │
│  ┌────────────────────┐  │  ┌─────────────────────────────────────┐ │
│  │ [BEGIN TRANSACTION]│  │  │           TDBGrid                   │ │
│  │ [COMMIT] [ROLLBACK]│  │  │  ID │ Account Name │ Balance │ ... │ │
│  └────────────────────┘  │  └─────────────────────────────────────┘ │
│  Savepoints              │  ┌─────────────────────────────────────┐ │
│  ┌────────────────────┐  │  │         TDBNavigator                │ │
│  │ Name: [___] [Create]│  │  │    [<] [>] [+] [-] [Post] [Cancel] │ │
│  │ ┌────────────────┐ │  │  └─────────────────────────────────────┘ │
│  │ │ SP_1           │ │  │  ┌─────────────────────────────────────┐ │
│  │ │ SP_2           │ │  │  │         Activity Log                │ │
│  │ │ SP_checkpoint  │ │  │  │ [12:30:45] BEGIN TRANSACTION        │ │
│  │ └────────────────┘ │  │  │ [12:30:52] INSERT: Account_123...   │ │
│  │ [Rollback To][Release]│  │ [12:31:05] SAVEPOINT SP_1           │ │
│  └────────────────────┘  │  └─────────────────────────────────────┘ │
└──────────────────────────┴─────────────────────────────────────────┘
```

## Transaction Concepts

### Explicit vs Implicit Transactions

```
Implicit (Auto-commit):          Explicit (Manual):
┌─────────────────────┐          ┌─────────────────────┐
│ INSERT INTO ...     │ ──COMMIT │ BEGIN TRANSACTION   │
├─────────────────────┤          │ INSERT INTO ...     │
│ UPDATE ...          │ ──COMMIT │ UPDATE ...          │
├─────────────────────┤          │ DELETE ...          │
│ DELETE ...          │ ──COMMIT │ COMMIT (or ROLLBACK)│
└─────────────────────┘          └─────────────────────┘
   Each statement                  All statements as
   committed separately            one atomic unit
```

### Savepoints

Savepoints create nested transaction markers:

```
BEGIN TRANSACTION
  │
  ├── INSERT row 1
  │
  ├── SAVEPOINT SP_A ─────────────┐
  │     │                         │
  │     ├── INSERT row 2          │ Can ROLLBACK TO SP_A
  │     │                         │ to undo only these
  │     ├── INSERT row 3          │
  │     │                         │
  │     └── SAVEPOINT SP_B ──┐    │
  │           │              │    │
  │           └── INSERT 4   │    │
  │                          │    │
  │   ROLLBACK TO SP_B ──────┘    │
  │   (undoes row 4 only)         │
  │                               │
  └── ROLLBACK TO SP_A ───────────┘
      (undoes rows 2, 3, 4)

COMMIT (saves row 1 only)
```

## Database Schema

```sql
CREATE TABLE accounts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    account_name TEXT NOT NULL,
    balance REAL DEFAULT 0.0,
    last_modified TEXT DEFAULT (datetime('now', 'localtime'))
);
```

## Key Implementation Details

### Starting a Transaction

```pascal
procedure TfrmMain.btnBeginTransactionClick(Sender: TObject);
begin
  if FDataSet.InTransaction then
  begin
    Log('ERROR: Transaction already active.');
    Exit;
  end;

  FDataSet.BeginTransaction;
  UpdateTransactionStatus;
end;
```

### Creating Savepoints

```pascal
procedure TfrmMain.btnSavepointClick(Sender: TObject);
begin
  if not FDataSet.InTransaction then
    Exit;

  FDataSet.Savepoint('MySavepoint');
  lstSavepoints.Items.Add('MySavepoint');
end;
```

### Rollback to Savepoint

```pascal
procedure TfrmMain.btnRollbackToSavepointClick(Sender: TObject);
begin
  FDataSet.RollbackToSavepoint('MySavepoint');

  // Refresh dataset to show reverted state
  FDataSet.Close;
  FDataSet.Open;
end;
```

### Handling Exit with Active Transaction

```pascal
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FDataSet.InTransaction then
  begin
    case MessageDlg('Uncommitted changes. Commit before closing?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: FDataSet.Commit;
      mrNo: FDataSet.Rollback;
      mrCancel: CanClose := False;
    end;
  end;
end;
```

## Usage Scenarios

### Scenario 1: Simple Transaction

1. Click **BEGIN TRANSACTION**
2. Make changes (insert, update, delete)
3. Click **COMMIT** to save or **ROLLBACK** to discard

### Scenario 2: Using Savepoints

1. Click **BEGIN TRANSACTION**
2. Make some changes
3. Create a savepoint: **SP_checkpoint**
4. Make more changes
5. Click **Rollback To** to undo changes after checkpoint
6. Click **COMMIT** to save remaining changes

### Scenario 3: Error Recovery

1. Start a transaction
2. Perform operations
3. If an error occurs, **ROLLBACK** undoes everything
4. Database returns to state before transaction started

## Building and Running

### Build
```bash
cd examples/gui/05_Transactions
lazbuild TransactionsDemo.lpi
```

### Run
- **Linux/macOS**: `./TransactionsDemo`
- **Windows**: `TransactionsDemo.exe`

## Project Files

| File | Description |
|------|-------------|
| `TransactionsDemo.lpi` | Lazarus project configuration |
| `TransactionsDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form with transaction management logic |
| `MainForm.lfm` | Visual form design |

## Best Practices

1. **Keep transactions short** - Long transactions block other writers
2. **Always handle errors** - Use try/except with rollback on failure
3. **Use savepoints sparingly** - They add overhead; use only when needed
4. **Check transaction state** - Use `InTransaction` before operations
5. **Clean up on exit** - Rollback uncommitted transactions when closing

## WAL Mode

This example enables WAL (Write-Ahead Logging) mode:

```pascal
FDataSet.SQL.Text := 'PRAGMA journal_mode=WAL';
FDataSet.ExecSQL;
```

Benefits:
- Concurrent readers during write transactions
- Better performance for write-heavy workloads
- Crash recovery without blocking readers

## Related Examples

- **01_DBGrid**: Basic data operations
- **03_CachedUpdates**: Client-side transaction buffering
- **07_MultiThread**: Transactions in multi-threaded environment

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
