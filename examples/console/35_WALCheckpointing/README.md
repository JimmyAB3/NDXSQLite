# Example 35: WAL Mode and Checkpointing

This example demonstrates SQLite's Write-Ahead Logging (WAL) mode and checkpointing.

## What you'll learn

- Enabling WAL mode
- Understanding WAL files (-wal, -shm)
- Manual checkpointing with different modes
- Auto-checkpoint configuration
- Synchronous settings with WAL
- Switching journal modes

## Key concepts

### Enabling WAL mode

```pascal
// Enable WAL (recommended for most applications)
DS := Connection.ExecuteQuery('PRAGMA journal_mode = WAL');

// Check current mode
DS := Connection.ExecuteQuery('PRAGMA journal_mode');
```

### WAL files

When using WAL mode, SQLite creates:
- `database.db-wal` - Write-ahead log (uncommitted changes)
- `database.db-shm` - Shared memory (coordination between connections)

### Manual checkpointing

```pascal
// PASSIVE: Non-blocking, checkpoint what's possible
DS := Connection.ExecuteQuery('PRAGMA wal_checkpoint(PASSIVE)');

// FULL: Wait for writers, checkpoint everything
DS := Connection.ExecuteQuery('PRAGMA wal_checkpoint(FULL)');

// RESTART: Like FULL, also briefly blocks readers
DS := Connection.ExecuteQuery('PRAGMA wal_checkpoint(RESTART)');

// TRUNCATE: Like RESTART, also truncates WAL to zero
DS := Connection.ExecuteQuery('PRAGMA wal_checkpoint(TRUNCATE)');

// Result columns: busy, log_pages, checkpointed_pages
```

### Auto-checkpoint

```pascal
// Check current threshold (default 1000 pages)
DS := Connection.ExecuteQuery('PRAGMA wal_autocheckpoint');

// Change threshold
Connection.ExecuteNonQuery('PRAGMA wal_autocheckpoint = 500');

// Disable auto-checkpoint (manual only)
Connection.ExecuteNonQuery('PRAGMA wal_autocheckpoint = 0');
```

### Synchronous mode with WAL

```pascal
// NORMAL is recommended with WAL (good balance)
Connection.ExecuteNonQuery('PRAGMA synchronous = NORMAL');

// Modes: OFF(0), NORMAL(1), FULL(2), EXTRA(3)
```

### Busy timeout

```pascal
// Set wait time when database is busy (ms)
Connection.ExecuteNonQuery('PRAGMA busy_timeout = 5000');
```

### Switching back from WAL

```pascal
// Checkpoint first
Connection.ExecuteNonQuery('PRAGMA wal_checkpoint(TRUNCATE)');

// Switch to DELETE mode
DS := Connection.ExecuteQuery('PRAGMA journal_mode = DELETE');
// WAL and SHM files are removed
```

## Checkpoint modes comparison

| Mode | Blocks Writers | Blocks Readers | Truncates WAL |
|------|----------------|----------------|---------------|
| PASSIVE | No | No | No |
| FULL | Yes (waits) | No | No |
| RESTART | Yes (waits) | Yes (briefly) | No |
| TRUNCATE | Yes (waits) | Yes (briefly) | Yes |

## WAL advantages

- **Better concurrency**: Readers don't block writers
- **Faster commits**: Writes go to WAL, not main file
- **Crash recovery**: WAL provides automatic recovery

## WAL disadvantages

- **Extra files**: -wal and -shm files alongside database
- **Network issues**: Don't use WAL on network filesystems
- **Memory usage**: Slightly higher memory for shared memory

## Best practices

1. **Use WAL for most applications**
2. **Set `synchronous = NORMAL`** with WAL
3. **Use TRUNCATE checkpoint** before backups
4. **Set reasonable `busy_timeout`** for concurrent access
5. **Don't use WAL** on network filesystems

## Building

```bash
lazbuild WALCheckpointing.lpi
```

## Running

```bash
./WALCheckpointing      # Linux/macOS
WALCheckpointing.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
