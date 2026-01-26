# Example 39: Configuration Store

This example demonstrates using SQLite as a configuration/settings store with key-value semantics.

## What you'll learn

- Key-value storage patterns
- Typed configuration values
- Configuration sections
- Change history and auditing
- JSON configuration values
- Default values and validation
- Export/Import configuration

## Key concepts

### Schema design

```sql
CREATE TABLE config (
  key TEXT PRIMARY KEY,
  value TEXT,
  value_type TEXT DEFAULT 'string',  -- string, integer, float, boolean, json
  section TEXT DEFAULT 'general',
  description TEXT,
  created_at TEXT DEFAULT (datetime('now')),
  updated_at TEXT DEFAULT (datetime('now'))
);
```

### Basic operations

```pascal
// Set configuration
SetConfig('app.name', 'My App', 'string', 'general');
SetConfigInteger('app.port', 8080, 'server');
SetConfigBoolean('app.debug', True, 'general');

// Get configuration with defaults
Value := GetConfigString('app.name', 'Default App');
Port := GetConfigInteger('app.port', 3000);
Debug := GetConfigBoolean('app.debug', False);
```

### UPSERT pattern

```sql
INSERT INTO config (key, value, value_type, section)
VALUES (?, ?, ?, ?)
ON CONFLICT(key) DO UPDATE SET value = excluded.value;
```

### Configuration sections

```sql
-- Get all keys in a section
SELECT key, value FROM config WHERE section = 'database';

-- List all sections
SELECT section, COUNT(*) FROM config GROUP BY section;
```

### Hierarchical keys

```sql
-- Get all server.http.* settings
SELECT key, value FROM config WHERE key LIKE 'server.http.%';
```

### JSON configuration

```sql
-- Store JSON
INSERT INTO config (key, value, value_type)
VALUES ('smtp', '{"host": "localhost", "port": 25}', 'json');

-- Extract JSON fields
SELECT JSON_EXTRACT(value, '$.host') as host,
       JSON_EXTRACT(value, '$.port') as port
FROM config WHERE key = 'smtp';
```

### Change history

```sql
CREATE TABLE config_history (
  id INTEGER PRIMARY KEY,
  key TEXT,
  old_value TEXT,
  new_value TEXT,
  changed_at TEXT DEFAULT (datetime('now')),
  changed_by TEXT
);

-- Trigger to track changes
CREATE TRIGGER config_update_trigger
AFTER UPDATE ON config
BEGIN
  INSERT INTO config_history (key, old_value, new_value)
  VALUES (OLD.key, OLD.value, NEW.value);
END;
```

### Defaults with validation

```sql
CREATE TABLE config_defaults (
  key TEXT PRIMARY KEY,
  default_value TEXT,
  min_value TEXT,
  max_value TEXT,
  valid_values TEXT  -- JSON array
);

-- Get effective value with fallback
SELECT COALESCE(c.value, d.default_value) as value
FROM config_defaults d
LEFT JOIN config c ON d.key = c.key
WHERE d.key = ?;
```

## Advantages of SQLite for configuration

- **Atomic updates**: ACID transactions for config changes
- **Queryable**: SQL for searching and filtering
- **Typed**: Enforce types with CHECK constraints
- **Auditable**: Track all changes with triggers
- **Portable**: Single file, easy backup/restore
- **Concurrent**: Multiple readers, single writer

## Building

```bash
lazbuild ConfigurationStore.lpi
```

## Running

```bash
./ConfigurationStore      # Linux/macOS
ConfigurationStore.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
