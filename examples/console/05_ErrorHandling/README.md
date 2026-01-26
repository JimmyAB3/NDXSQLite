# Example 05: Error Handling

This example demonstrates proper error handling with SQLite exceptions.

## What you'll learn

- Catching SQLite errors
- Handling constraint violations (UNIQUE, CHECK, NOT NULL)
- Transaction error recovery
- Error detection patterns

## Key concepts

### Catching SQLite exceptions

SQLite errors are raised as `ESQLDatabaseError` exceptions. You can detect specific error types by checking the message content:

```pascal
try
  Connection.ExecuteNonQuery('INSERT ...');
except
  on E: Exception do
  begin
    if Pos('UNIQUE', UpperCase(E.Message)) > 0 then
      WriteLn('Duplicate value detected')
    else if Pos('CHECK', UpperCase(E.Message)) > 0 then
      WriteLn('Check constraint violated')
    else if Pos('NOT NULL', UpperCase(E.Message)) > 0 then
      WriteLn('Required field missing')
    else
      WriteLn('Database error: ', E.Message);
  end;
end;
```

### Error detection patterns

| Error Type | Detection Pattern |
|------------|-------------------|
| UNIQUE constraint | `Pos('UNIQUE', Message) > 0` |
| CHECK constraint | `Pos('CHECK', Message) > 0` |
| NOT NULL constraint | `Pos('NOT NULL', Message) > 0` |
| Table not found | `Pos('no such table', Message) > 0` |
| Syntax error | `Pos('syntax error', Message) > 0` |

### Transaction recovery pattern

```pascal
Connection.BeginTransaction;
try
  Connection.ExecuteNonQuery('INSERT ...');
  Connection.ExecuteNonQuery('UPDATE ...');
  Connection.Commit;
except
  on E: Exception do
  begin
    Connection.Rollback;
    WriteLn('Error: ', E.Message);
    WriteLn('Transaction rolled back.');
  end;
end;
```

### Checking transaction state

```pascal
if Connection.IsTransactionActive then
  WriteLn('Currently in a transaction');
```

## Building

```bash
lazbuild ErrorHandling.lpi
```

## Running

```bash
./ErrorHandling      # Linux/macOS
ErrorHandling.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 05: Error Handling ===

1. SQL Syntax Error:
   Exception: ESQLDatabaseError
   Message: TSQLite3Connection : near "SELCT": syntax error

2. UNIQUE Constraint Violation:
   Exception: ESQLDatabaseError
   Message: TSQLite3Connection : UNIQUE constraint failed: users.email
   -> Detected as UNIQUE constraint violation

3. CHECK Constraint Violation:
   ...
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
