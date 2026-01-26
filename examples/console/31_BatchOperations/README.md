# Example 31: Batch Operations

This example demonstrates efficient bulk data operations in SQLite.

## What you'll learn

- Why transactions are critical for bulk inserts
- Prepared statement reuse for performance
- Multi-row INSERT syntax
- Bulk UPDATE and DELETE operations
- Performance optimization PRAGMAs
- INSERT from SELECT for data copying

## Key concepts

### The transaction difference

```pascal
// SLOW: Each insert auto-commits (disk write per row!)
for I := 1 to 10000 do
  Connection.ExecuteNonQuery('INSERT INTO t VALUES (...)');

// FAST: All inserts in one transaction (single disk write)
Connection.BeginTransaction;
try
  for I := 1 to 10000 do
    Connection.ExecuteNonQuery('INSERT INTO t VALUES (...)');
  Connection.Commit;
except
  Connection.Rollback;
  raise;
end;
```

### Prepared statements with parameters

```pascal
// Parameters allow statement caching
Connection.BeginTransaction;
for I := 1 to 50000 do
begin
  Connection.ExecuteNonQuery(
    'INSERT INTO records (name, value) VALUES (?, ?)',
    [Format('Item %d', [I]), Random * 1000]);
end;
Connection.Commit;
```

### Multi-row INSERT

```pascal
// Insert multiple rows in one statement
Connection.ExecuteNonQuery(
  'INSERT INTO records (name, value) VALUES ' +
  '(''Item1'', 100),' +
  '(''Item2'', 200),' +
  '(''Item3'', 300)');

// Build batch dynamically
SQL := 'INSERT INTO records (name, value) VALUES ';
for I := 1 to 500 do
begin
  if I > 1 then SQL := SQL + ',';
  SQL := SQL + Format('(''Item%d'', %f)', [I, Random * 1000]);
end;
Connection.ExecuteNonQuery(SQL);
```

### Bulk UPDATE

```pascal
// Update many rows at once
Connection.ExecuteNonQuery(
  'UPDATE records SET value = value * 1.1 WHERE category = ''Cat5''');

// Conditional update with CASE
Connection.ExecuteNonQuery(
  'UPDATE records SET value = CASE ' +
  '  WHEN category = ''A'' THEN value * 0.9 ' +
  '  WHEN category = ''B'' THEN value * 1.1 ' +
  '  ELSE value END');
```

### Bulk DELETE

```pascal
// Delete many rows
Connection.ExecuteNonQuery(
  'DELETE FROM records WHERE category IN (''Cat7'', ''Cat8'')');

// Delete with subquery
Connection.ExecuteNonQuery(
  'DELETE FROM records WHERE id IN ' +
  '(SELECT id FROM records WHERE value < 10 LIMIT 1000)');
```

### INSERT from SELECT

```pascal
// Copy data between tables
Connection.ExecuteNonQuery(
  'INSERT INTO archive (name, value) ' +
  'SELECT name, value FROM records WHERE created_at < date(''now'', ''-30 days'')');
```

### Performance PRAGMAs

```pascal
// For bulk operations (temporary - reset after!)
Connection.ExecuteNonQuery('PRAGMA synchronous = OFF');
Connection.ExecuteNonQuery('PRAGMA journal_mode = MEMORY');
Connection.ExecuteNonQuery('PRAGMA cache_size = 10000');

// WARNING: These reduce durability!
// - synchronous = OFF: Data loss on crash
// - journal_mode = MEMORY: No recovery possible
// Only use for initial data loads, not production writes
```

## Performance comparison

| Method | 50,000 rows | Rate |
|--------|-------------|------|
| No transaction | ~5 min | ~170 rows/sec |
| With transaction | ~2 sec | ~25,000 rows/sec |
| + Prepared stmt | ~1.5 sec | ~33,000 rows/sec |
| + Multi-row INSERT | ~0.8 sec | ~62,000 rows/sec |

*Actual numbers vary by hardware and SQLite version*

## Best practices

1. **Always use transactions** for bulk operations
2. **Use parameterized queries** for repeated statements
3. **Batch multi-row INSERTs** (500-1000 rows per statement)
4. **Disable synchronous temporarily** for initial loads
5. **Create indexes AFTER** bulk inserts
6. **Run ANALYZE** after large data changes

## Building

```bash
lazbuild BatchOperations.lpi
```

## Running

```bash
./BatchOperations      # Linux/macOS
BatchOperations.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
