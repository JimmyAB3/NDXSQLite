# Example 07: Connection Factory

This example demonstrates the Factory pattern for creating database connections.

## What you'll learn

- Using TNDXSQLiteConnectionFactory
- Creating different connection types
- Factory pattern benefits
- Centralized configuration

## Available Factory Methods

| Method | Description |
|--------|-------------|
| `CreateConnection` | Standard connection |
| `CreatePrimaryConnection` | Long-lived, no auto-close |
| `CreateReadOnlyConnection` | Read-only access |
| `CreateMemoryConnection` | In-memory database |

## Key concepts

### Simple factory creation

```pascal
Factory := TNDXSQLiteConnectionFactory.Create('database.db');
Conn := Factory.CreateConnection;
```

### Factory with options

```pascal
Options := TNDXSQLiteConnectionOptions.Create;
Options.DatabasePath := 'database.db';
Options.JournalMode := jmWAL;
Options.CacheSizeKB := 8192;

Factory := TNDXSQLiteConnectionFactory.Create(Options);
```

### Different connection types

```pascal
// Standard connection
Conn := Factory.CreateConnection;

// Read-only (can't modify data)
ReadConn := Factory.CreateReadOnlyConnection;

// In-memory (temporary, fast)
MemConn := Factory.CreateMemoryConnection;

// Primary (won't auto-close)
PrimaryConn := Factory.CreatePrimaryConnection;
```

## Building

```bash
lazbuild ConnectionFactory.lpi
```

## Running

```bash
./ConnectionFactory      # Linux/macOS
ConnectionFactory.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
