# Example 06: Connection Options

This example demonstrates SQLite connection configuration using TNDXSQLiteConnectionOptions.

## What you'll learn

- Configuring options BEFORE opening the connection
- WAL mode vs DELETE mode
- Cache size configuration
- Busy timeout settings
- Synchronous mode options
- Foreign key enforcement
- Reading PRAGMA values after connection

## Important Note

**Options must be configured BEFORE opening the connection** because some PRAGMAs (like journal_mode, synchronous) cannot be changed inside a transaction.

## Key Options

### Journal Mode

| Mode | Description |
|------|-------------|
| jmDelete | Traditional rollback journal (default) |
| jmWAL | Write-Ahead Logging - better concurrency |
| jmMemory | In-memory journal - fast but no crash safety |
| jmOff | No journal - fastest but unsafe |

### Synchronous Mode

| Mode | Safety | Speed |
|------|--------|-------|
| smOff | Dangerous | Fastest |
| smNormal | Safe with WAL | Balanced |
| smFull | Maximum safety | Slowest |

## Key concepts

### Using TNDXSQLiteConnectionOptions

```pascal
Options := TNDXSQLiteConnectionOptions.Create;
try
  Options.DatabasePath := 'database.db';
  Options.CreateIfNotExists := True;

  // Configure BEFORE opening
  Options.JournalMode := jmWAL;
  Options.SyncMode := smNormal;
  Options.CacheSize := 8192;  // 8 MB
  Options.BusyTimeout := 5000; // 5 seconds
  Options.ForeignKeys := True;
  Options.TempStore := 2;  // MEMORY

  // Create connection with options
  Connection := TNDXSQLiteConnection.Create(Options);
  try
    Connection.Open;
    // Options are now applied
  finally
    Connection.Free;
  end;
finally
  Options.Free;
end;
```

### Verifying applied settings

```pascal
// After opening, verify settings
WriteLn('journal_mode: ', Connection.GetPragmaValue('journal_mode'));
WriteLn('synchronous: ', Connection.GetPragmaValue('synchronous'));
WriteLn('cache_size: ', Connection.GetCacheSize);
WriteLn('busy_timeout: ', Connection.GetBusyTimeout);
WriteLn('foreign_keys: ', Connection.IsForeignKeysEnabled);
```

## Recommended Settings

### Desktop Application
```pascal
Options.JournalMode := jmWAL;
Options.SyncMode := smNormal;
Options.CacheSize := 8192;    // 8 MB
Options.BusyTimeout := 5000;  // 5 seconds
Options.ForeignKeys := True;
```

### Server/High Concurrency
```pascal
Options.JournalMode := jmWAL;
Options.SyncMode := smNormal;
Options.CacheSize := 32768;   // 32 MB
Options.BusyTimeout := 30000; // 30 seconds
Options.ForeignKeys := True;
```

### Embedded/Memory Constrained
```pascal
Options.JournalMode := jmDelete;
Options.SyncMode := smFull;
Options.CacheSize := 2000;    // 2 MB
Options.BusyTimeout := 1000;  // 1 second
Options.ForeignKeys := True;
```

## Building

```bash
lazbuild ConnectionOptions.lpi
```

## Running

```bash
./ConnectionOptions      # Linux/macOS
ConnectionOptions.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
