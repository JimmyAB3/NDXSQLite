# Example 34: Database Maintenance

This example demonstrates SQLite database maintenance operations.

## What you'll learn

- VACUUM to reclaim unused space
- ANALYZE to update optimizer statistics
- PRAGMA integrity_check for corruption detection
- PRAGMA quick_check for fast validation
- REINDEX to rebuild indexes
- PRAGMA optimize for automatic optimization
- Database size monitoring

## Key concepts

### Integrity check

```pascal
// Full integrity check (thorough but slow)
DS := Connection.ExecuteQuery('PRAGMA integrity_check');
// Returns 'ok' if no issues, or list of problems

// Quick check (faster, less thorough)
DS := Connection.ExecuteQuery('PRAGMA quick_check');
// Skips UNIQUE and index content verification
```

### ANALYZE - Update statistics

```pascal
// Update statistics for all tables
Connection.ExecuteNonQuery('ANALYZE');

// Update statistics for specific table
Connection.ExecuteNonQuery('ANALYZE tablename');

// View statistics
DS := Connection.ExecuteQuery('SELECT * FROM sqlite_stat1');
```

### VACUUM - Reclaim space

```pascal
// Rebuild entire database, reclaim deleted space
Connection.ExecuteNonQuery('VACUUM');

// Check file size before/after to see reclaimed space
```

### REINDEX - Rebuild indexes

```pascal
// Rebuild specific index
Connection.ExecuteNonQuery('REINDEX idx_name');

// Rebuild all indexes on a table
Connection.ExecuteNonQuery('REINDEX tablename');

// Rebuild all indexes in database
Connection.ExecuteNonQuery('REINDEX');
```

### PRAGMA optimize

```pascal
// Automatic optimization based on query patterns
Connection.ExecuteNonQuery('PRAGMA optimize');
// Best practice: run before closing long-lived connections
```

### Auto-vacuum

```pascal
// Check current setting
DS := Connection.ExecuteQuery('PRAGMA auto_vacuum');
// 0 = none, 1 = full, 2 = incremental

// Set before creating tables (in new database)
Connection.ExecuteNonQuery('PRAGMA auto_vacuum = FULL');

// For incremental mode
Connection.ExecuteNonQuery('PRAGMA incremental_vacuum(100)'); // vacuum 100 pages
```

### Page information

```pascal
DS := Connection.ExecuteQuery('PRAGMA page_size');      // bytes per page
DS := Connection.ExecuteQuery('PRAGMA page_count');     // total pages
DS := Connection.ExecuteQuery('PRAGMA freelist_count'); // unused pages
```

### Foreign key check

```pascal
DS := Connection.ExecuteQuery('PRAGMA foreign_key_check');
// Returns rows violating FK constraints
```

## Maintenance commands summary

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `PRAGMA integrity_check` | Detect corruption | Weekly or after crash |
| `PRAGMA quick_check` | Fast validation | Daily |
| `ANALYZE` | Update statistics | After bulk changes |
| `VACUUM` | Reclaim space | After heavy deletions |
| `REINDEX` | Rebuild indexes | After schema changes |
| `PRAGMA optimize` | Auto-optimization | Before closing connection |

## Recommended schedule

```
Daily:
  PRAGMA optimize;

Weekly:
  ANALYZE;
  PRAGMA integrity_check;

Monthly (or after heavy deletions):
  VACUUM;

After schema changes:
  REINDEX;
  ANALYZE;
```

## Building

```bash
lazbuild DatabaseMaintenance.lpi
```

## Running

```bash
./DatabaseMaintenance      # Linux/macOS
DatabaseMaintenance.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
