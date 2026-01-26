# Example 23: Generated Columns

This example demonstrates generated columns, STRICT tables, and WITHOUT ROWID.

## What you'll learn

- VIRTUAL vs STORED generated columns
- STRICT tables for type enforcement
- WITHOUT ROWID tables
- Index on generated columns

## Key concepts

### Generated columns

```sql
CREATE TABLE products (
  price REAL,
  quantity INTEGER,
  total REAL GENERATED ALWAYS AS (price * quantity) VIRTUAL,
  total_stored REAL GENERATED ALWAYS AS (price * quantity) STORED
);
```

### STRICT tables

```sql
CREATE TABLE data (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  count INTEGER NOT NULL
) STRICT;
-- Type mismatches will be rejected
```

### WITHOUT ROWID

```sql
CREATE TABLE key_value (
  key TEXT PRIMARY KEY,
  value TEXT
) WITHOUT ROWID;
-- Optimized for non-integer primary keys
```

## VIRTUAL vs STORED

| Aspect | VIRTUAL | STORED |
|--------|---------|--------|
| Storage | None | Yes |
| Read performance | Slower | Faster |
| Can be indexed | No | Yes |
| Best for | Rarely used | Frequently queried |

## STRICT table types

- INTEGER
- REAL
- TEXT
- BLOB
- ANY (accepts any type)

## Building

```bash
lazbuild GeneratedColumns.lpi
```

## Running

```bash
./GeneratedColumns      # Linux/macOS
GeneratedColumns.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
