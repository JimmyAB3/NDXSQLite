# Example 17: Concurrent Access

This example demonstrates multi-threaded database access patterns with SQLite.

## What you'll learn

- WAL mode for concurrent access
- Busy timeout configuration
- Multi-threaded read/write patterns
- Thread-safe database access

## Key concepts

### WAL mode configuration

```pascal
Options := TNDXSQLiteConnectionOptions.Create;
Options.JournalMode := jmWAL;  // Write-Ahead Logging
Options.BusyTimeoutMs := 5000; // 5 second timeout
```

### Thread-safe pattern

```pascal
// Each thread should have its own connection
procedure TMyThread.Execute;
var
  Conn: TNDXSQLiteConnection;
begin
  Conn := TNDXSQLiteConnection.Create(DBPath);
  try
    Conn.Open;
    // ... do work ...
    Conn.Close;
  finally
    Conn.Free;
  end;
end;
```

### Handling busy errors

```pascal
try
  Connection.ExecuteNonQuery('INSERT INTO ...');
except
  on E: Exception do
    if Pos('SQLITE_BUSY', E.Message) > 0 then
      // Retry or handle gracefully
end;
```

## WAL vs DELETE journal mode

| Feature | WAL | DELETE |
|---------|-----|--------|
| Concurrent readers | Yes | Limited |
| Readers block writers | No | Yes |
| Writers block readers | No | Yes |
| Performance (many transactions) | Better | Slower |
| Disk usage | Higher | Lower |

## Best practices

- Use WAL mode for concurrent access
- Set appropriate busy_timeout
- Keep transactions short
- Each thread gets its own connection
- Use connection pooling for efficiency
- Handle SQLITE_BUSY errors gracefully

## Building

```bash
lazbuild ConcurrentAccess.lpi
```

## Running

```bash
./ConcurrentAccess      # Linux/macOS
ConcurrentAccess.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
