# Example 40: Event Log Database

This example demonstrates using SQLite as an event log/journal with rotation and archiving.

## What you'll learn

- Structured event logging
- Log levels and categories
- Efficient querying with indexes
- Log rotation and archiving
- Time-based partitioning
- Log analysis and statistics

## Key concepts

### Schema design

```sql
CREATE TABLE event_log (
  id INTEGER PRIMARY KEY,
  timestamp TEXT DEFAULT (datetime('now')),
  level INTEGER NOT NULL,      -- 0=debug, 1=info, 2=warning, 3=error, 4=critical
  category TEXT,               -- e.g., "auth", "database", "api"
  source TEXT,                 -- module or function name
  message TEXT NOT NULL,
  context TEXT,                -- JSON additional data
  user_id TEXT,
  session_id TEXT,
  ip_address TEXT
);

-- Essential indexes
CREATE INDEX idx_log_timestamp ON event_log(timestamp);
CREATE INDEX idx_log_level ON event_log(level);
CREATE INDEX idx_log_category ON event_log(category);
CREATE INDEX idx_log_level_time ON event_log(level, timestamp);
```

### Logging functions

```pascal
procedure LogEvent(ALevel: Integer; const ACategory, ASource, AMessage: string);
begin
  Connection.ExecuteNonQuery(
    'INSERT INTO event_log (level, category, source, message) VALUES (?, ?, ?, ?)',
    [ALevel, ACategory, ASource, AMessage]);
end;

// Convenience wrappers
LogDebug('auth', 'login', 'User login attempt');
LogInfo('api', 'handler', 'Request processed');
LogWarning('system', 'monitor', 'High memory usage');
LogError('database', 'query', 'Query timeout', '{"query_id": 123}');
LogCritical('system', 'main', 'Out of memory');
```

### Querying logs

```sql
-- Recent errors
SELECT * FROM event_log
WHERE level >= 3
ORDER BY timestamp DESC LIMIT 100;

-- Events by category
SELECT category, COUNT(*) as count
FROM event_log
GROUP BY category;

-- Time-based filtering
SELECT * FROM event_log
WHERE timestamp >= datetime('now', '-1 hour');

-- Search in messages
SELECT * FROM event_log
WHERE message LIKE '%timeout%';
```

### Log rotation

```sql
-- Archive old logs
INSERT INTO event_log_archive
SELECT *, datetime('now') as archived_at
FROM event_log
WHERE timestamp < datetime('now', '-7 days');

-- Delete archived entries
DELETE FROM event_log
WHERE timestamp < datetime('now', '-7 days');
```

### Statistics

```sql
-- Error rate by category
SELECT category,
       COUNT(*) as total,
       SUM(CASE WHEN level >= 3 THEN 1 ELSE 0 END) as errors,
       ROUND(100.0 * SUM(CASE WHEN level >= 3 THEN 1 ELSE 0 END) / COUNT(*), 1) as error_rate
FROM event_log
GROUP BY category;

-- Events per hour
SELECT strftime('%Y-%m-%d %H:00', timestamp) as hour,
       COUNT(*) as count
FROM event_log
GROUP BY hour
ORDER BY hour;
```

## Log levels

| Level | Name | Use Case |
|-------|------|----------|
| 0 | DEBUG | Detailed debugging information |
| 1 | INFO | General operational messages |
| 2 | WARNING | Potential issues |
| 3 | ERROR | Error conditions |
| 4 | CRITICAL | System-critical errors |

## Best practices

1. **Index wisely**: Create indexes on frequently queried columns
2. **Rotate regularly**: Archive and delete old logs
3. **Use JSON context**: Store structured data in context field
4. **Batch inserts**: Use transactions for high-volume logging
5. **Separate archives**: Keep archive table for historical queries

## Building

```bash
lazbuild EventLog.lpi
```

## Running

```bash
./EventLog      # Linux/macOS
EventLog.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
