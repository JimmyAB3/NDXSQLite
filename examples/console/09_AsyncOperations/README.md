# Example 09: Async Operations

This example demonstrates asynchronous database operations.

## What you'll learn

- Creating async connections
- Async query execution with callbacks
- Cancellation tokens
- Managing pending operations
- Non-blocking database access

## Key concepts

### Async callback types

The async operations use result records that contain both data and error information:

```pascal
// Query callback - receives TNDXAsyncResultDataSet
procedure OnQueryComplete(const AResult: TNDXAsyncResultDataSet);
begin
  if AResult.Success then
    WriteLn('Rows: ', AResult.Data.RecordCount)
  else
    WriteLn('Error: ', AResult.ErrorMessage);
  if Assigned(AResult.Data) then
    AResult.Data.Free;
end;

// Scalar callback - receives TNDXAsyncResultVariant
procedure OnScalarComplete(const AResult: TNDXAsyncResultVariant);
begin
  if AResult.Success then
    WriteLn('Value: ', VarToStr(AResult.Data))
  else
    WriteLn('Error: ', AResult.ErrorMessage);
end;

// NonQuery callback - receives TNDXAsyncResultInt
procedure OnNonQueryComplete(const AResult: TNDXAsyncResultInt);
begin
  if AResult.Success then
    WriteLn('Rows affected: ', AResult.Data)
  else
    WriteLn('Error: ', AResult.ErrorMessage);
end;
```

### Creating async connection

```pascal
Options := TNDXSQLiteConnectionOptions.Create;
Options.DatabasePath := 'database.db';
AsyncConn := TNDXSQLiteAsyncConnection.Create(Options);
```

### Async open

```pascal
AsyncConn.OpenAsync(@OnOpenComplete, nil);
```

### Async query

```pascal
AsyncConn.ExecuteQueryAsync(
  'SELECT * FROM items',
  @OnQueryComplete,
  CancellationToken);  // Optional token
```

### Cancellation tokens

```pascal
TokenSource := TNDXCancellationTokenSource.Create;
try
  // Start async operation with token
  AsyncConn.ExecuteQueryAsync('SELECT ...', @Callback, TokenSource.Token);

  // Cancel if needed
  TokenSource.Cancel;
  WriteLn('Cancelled: ', TokenSource.Token.IsCancellationRequested);
finally
  TokenSource.Free;
end;
```

### Managing pending operations

```pascal
// Check pending count
Count := AsyncConn.GetPendingOperationsCount;

// Wait for all operations with timeout
AsyncConn.WaitForAllOperations(5000);  // 5 second timeout
```

## Building

```bash
lazbuild AsyncOperations.lpi
```

## Running

```bash
./AsyncOperations      # Linux/macOS
AsyncOperations.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
