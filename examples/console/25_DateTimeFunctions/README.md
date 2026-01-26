# Example 25: Date/Time Functions

This example demonstrates SQLite date and time functions.

## What you'll learn

- Current date/time functions
- Date arithmetic
- strftime() formatting
- Unix timestamp conversion
- Date comparisons and filtering

## Key concepts

### Current date/time

```sql
SELECT date('now');                    -- 2025-01-18
SELECT time('now');                    -- 14:30:45
SELECT datetime('now');                -- 2025-01-18 14:30:45
SELECT datetime('now', 'localtime');   -- Local time
```

### Date arithmetic

```sql
SELECT date('now', '+1 day');          -- Tomorrow
SELECT date('now', '-1 week');         -- Last week
SELECT date('now', '+1 month');        -- Next month
SELECT date('now', 'start of month');  -- First of month
SELECT datetime('now', '+2 hours');    -- 2 hours from now
```

### strftime() formatting

```sql
SELECT strftime('%Y-%m-%d', 'now');    -- 2025-01-18
SELECT strftime('%H:%M', 'now');       -- 14:30
SELECT strftime('%s', 'now');          -- Unix timestamp
SELECT strftime('%W', 'now');          -- Week number
```

### Date difference

```sql
SELECT julianday('2025-12-31') - julianday('2025-01-01');  -- Days between
```

## Format specifiers

| Specifier | Description | Example |
|-----------|-------------|---------|
| %Y | Year (4 digits) | 2025 |
| %m | Month (01-12) | 01 |
| %d | Day (01-31) | 18 |
| %H | Hour (00-23) | 14 |
| %M | Minute (00-59) | 30 |
| %S | Second (00-59) | 45 |
| %w | Weekday (0=Sun) | 6 |
| %W | Week number | 03 |
| %j | Day of year | 018 |
| %s | Unix timestamp | 1737210645 |

## Building

```bash
lazbuild DateTimeFunctions.lpi
```

## Running

```bash
./DateTimeFunctions      # Linux/macOS
DateTimeFunctions.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
