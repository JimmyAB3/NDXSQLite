# Example 02: Transactions

This example demonstrates transaction handling in NDXSQLite.

## What you'll learn

- How to begin a transaction
- How to commit changes
- How to rollback on errors
- How to check transaction state
- Atomic operations pattern

## Key concepts

### Basic transaction pattern

```pascal
Connection.BeginTransaction;
try
  Connection.ExecuteNonQuery('UPDATE ...');
  Connection.ExecuteNonQuery('INSERT ...');
  Connection.Commit;
except
  Connection.Rollback;
  raise;
end;
```

### Checking transaction state

```pascal
if Connection.IsTransactionActive then
  WriteLn('Currently in a transaction');
```

### Why use transactions?

1. **Atomicity**: All operations succeed or all fail
2. **Consistency**: Database remains in valid state
3. **Isolation**: Concurrent operations don't interfere
4. **Durability**: Committed changes persist

## Building

```bash
lazbuild Transactions.lpi
```

## Running

```bash
./Transactions      # Linux/macOS
Transactions.exe    # Windows
```

## Expected output

The example simulates a banking scenario with account transfers, showing both successful and failed transactions.

## Cross-Platform

This example works on Windows, Linux, and macOS.
