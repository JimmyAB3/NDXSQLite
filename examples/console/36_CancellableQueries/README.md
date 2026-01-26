# Example 36: Cancellable Queries and Operations

This example demonstrates using cancellation tokens to cancel long-running operations.

## What you'll learn

- Creating cancellation tokens
- Polling for cancellation (IsCancellationRequested)
- Exception-based cancellation (ThrowIfCancellationRequested)
- Automatic timeouts (CancelAfter)
- Token reuse (Reset)
- Non-cancellable operations (TNDXCancellationToken.None)

## Key concepts

### Creating a cancellation token

```pascal
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
begin
  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.Token;
    // Use token for cancellable operations
    CTS.Cancel; // Trigger cancellation
  finally
    CTS.Free;
  end;
end;
```

### Polling pattern

```pascal
while not DS.EOF do
begin
  // Check periodically
  if Token.IsCancellationRequested then
  begin
    WriteLn('Operation cancelled');
    Break;
  end;

  // Process row
  DS.Next;
end;
```

### Exception pattern

```pascal
try
  for I := 1 to 1000 do
  begin
    // Throws ENDXOperationCanceledException if cancelled
    Token.ThrowIfCancellationRequested;

    // Do work
  end;
except
  on E: ENDXOperationCanceledException do
    WriteLn('Cancelled: ', E.Message);
end;
```

### Automatic timeout

```pascal
// Cancel automatically after 5 seconds
CTS.CancelAfter(5000);

// Work until timeout
while not Token.IsCancellationRequested do
begin
  ProcessNextItem;
end;
```

### Reset for reuse

```pascal
// First operation
CTS.CancelAfter(1000);
DoSomething(Token);

// Reset for next operation
CTS.Reset;
CTS.CancelAfter(2000);
DoSomethingElse(Token);
```

### Non-cancellable operations

```pascal
// Use when cancellation is not needed
Token := TNDXCancellationToken.None;
DoOperation(Token); // Will never be cancelled
```

## Cancellation patterns comparison

| Pattern | Use Case | Pros | Cons |
|---------|----------|------|------|
| Polling | Long loops | Simple, explicit control | Must check manually |
| Exception | Multi-phase operations | Automatic propagation | Try/except overhead |
| CancelAfter | Timeouts | Automatic timing | Background thread |

## Best practices

1. **Check at safe points**: Only check cancellation where you can safely abort
2. **Clean up resources**: Use try/finally to ensure cleanup on cancellation
3. **Use appropriate pattern**: Polling for loops, exceptions for complex flows
4. **Set reasonable timeouts**: Balance responsiveness vs. completion
5. **Reset before reuse**: Always Reset before reusing a TokenSource

## Thread safety

- `TNDXCancellationTokenSource` is thread-safe
- Multiple threads can check `IsCancellationRequested` simultaneously
- Only one thread should call `Cancel` (though multiple calls are safe)

## Building

```bash
lazbuild CancellableQueries.lpi
```

## Running

```bash
./CancellableQueries      # Linux/macOS
CancellableQueries.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
