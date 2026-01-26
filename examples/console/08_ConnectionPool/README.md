# Example 08: Connection Pool

This example demonstrates connection pooling for multi-threaded applications.

## What you'll learn

- Creating and configuring a connection pool
- Acquiring and releasing connections
- Pool statistics and monitoring
- Thread-safe connection management
- Auto-release with TNDXPooledConnection

## Key concepts

### Creating a pool

```pascal
Options := TNDXSQLiteConnectionOptions.Create;
Options.DatabasePath := 'database.db';

// Min 2, max 10 connections
Pool := TNDXSQLiteConnectionPool.Create(Options, 2, 10);
```

### Basic acquire/release

```pascal
Conn := Pool.Acquire;
try
  Conn.ExecuteQuery('SELECT ...');
finally
  Pool.Release(Conn);
end;
```

### Auto-release wrapper

```pascal
PooledConn := TNDXPooledConnection.Create(Pool);
try
  PooledConn.Connection.ExecuteQuery('SELECT ...');
finally
  PooledConn.Free; // Automatically releases
end;
```

### TryAcquire with timeout

```pascal
if Pool.TryAcquire(Conn, 5000) then // 5 second timeout
begin
  try
    // Use connection
  finally
    Pool.Release(Conn);
  end;
end
else
  WriteLn('Timeout waiting for connection');
```

## Pool Properties

| Property | Description |
|----------|-------------|
| MinSize | Minimum connections to maintain |
| MaxSize | Maximum concurrent connections |
| ActiveCount | Currently in-use connections |
| IdleCount | Available connections |
| ValidateOnAcquire | Check connection before returning |

## Building

```bash
lazbuild ConnectionPool.lpi
```

## Running

```bash
./ConnectionPool      # Linux/macOS
ConnectionPool.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
