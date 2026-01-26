# Example 10: Health Check

This example demonstrates database health monitoring and maintenance.

## What you'll learn

- Basic health check
- Database information retrieval
- Integrity verification
- Maintenance operations

## Key concepts

### Basic health check

```pascal
Factory := TNDXSQLiteConnectionFactory.Create('database.db');
HealthCheck := TNDXSQLiteHealthCheck.Create(Factory);

Result := HealthCheck.CheckHealth;
if Result.IsHealthy then
  WriteLn('Database OK, response: ', Result.ResponseTimeMs, 'ms');
```

### Database information

```pascal
Info := HealthCheck.GetDatabaseInfo;
WriteLn('SQLite: ', Info.SQLiteVersion);
WriteLn('Size: ', Info.DatabaseSizeBytes);
WriteLn('Journal: ', Info.JournalMode);
```

### Integrity checks

```pascal
// Quick check (fast)
HealthCheck.CheckIntegrity(False);

// Full check (thorough)
HealthCheck.CheckIntegrity(True);

// Foreign key violations
HealthCheck.CheckForeignKeys;
```

### Maintenance operations

```pascal
// Update query planner statistics
HealthChecker.Analyze;

// Optimize based on usage patterns
HealthChecker.Optimize;

// Compact database, reclaim space
HealthChecker.Vacuum;
```

## Building

```bash
lazbuild HealthCheck.lpi
```

## Running

```bash
./HealthCheck      # Linux/macOS
HealthCheck.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
