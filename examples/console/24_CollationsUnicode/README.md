# Example 24: Collations and Unicode

This example demonstrates collations and Unicode handling in SQLite.

## What you'll learn

- Built-in collations (BINARY, NOCASE, RTRIM)
- Case-sensitive vs case-insensitive operations
- Unicode string storage and retrieval
- Sorting with collations

## Key concepts

### Built-in collations

```sql
-- BINARY: exact byte comparison (default)
SELECT * FROM t WHERE name = 'Apple';  -- won't match 'apple'

-- NOCASE: case-insensitive for ASCII
SELECT * FROM t WHERE name = 'Apple' COLLATE NOCASE;  -- matches 'apple', 'APPLE'

-- RTRIM: ignores trailing spaces
SELECT * FROM t WHERE name = 'Apple' COLLATE RTRIM;  -- matches 'Apple   '
```

### Column-level collation

```sql
CREATE TABLE users (
  username TEXT COLLATE NOCASE UNIQUE,
  email TEXT COLLATE NOCASE
);
```

### Sorting with collation

```sql
SELECT * FROM fruits ORDER BY name COLLATE NOCASE;
```

## Collation comparison

| Collation | Case sensitive | Trailing spaces | Best for |
|-----------|---------------|-----------------|----------|
| BINARY | Yes | Significant | Exact matching |
| NOCASE | No (ASCII) | Significant | Usernames, emails |
| RTRIM | Yes | Ignored | Fixed-width data |

## Unicode notes

- SQLite stores all text as UTF-8
- LENGTH() returns character count
- UPPER/LOWER only work for ASCII
- LIKE is case-insensitive for ASCII only

## Building

```bash
lazbuild CollationsUnicode.lpi
```

## Running

```bash
./CollationsUnicode      # Linux/macOS
CollationsUnicode.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
