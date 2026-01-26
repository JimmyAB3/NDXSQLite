# Example 16: Performance Optimization

This example demonstrates performance optimization techniques for SQLite databases.

## What you'll learn

- Bulk insert with/without transactions
- Index creation and usage
- EXPLAIN QUERY PLAN analysis
- Composite and partial indexes
- Cache size impact
- Performance measurement techniques

## Key concepts

### Bulk insert with transactions

```pascal
// Without transaction: each INSERT is a separate transaction (slow)
for I := 1 to 1000 do
  Connection.ExecuteNonQuery('INSERT INTO ...');

// With transaction: all INSERTs in one transaction (fast)
Connection.BeginTransaction;
try
  for I := 1 to 1000 do
    Connection.ExecuteNonQuery('INSERT INTO ...');
  Connection.Commit;
except
  Connection.Rollback;
end;
```

### Creating indexes

```pascal
// Simple index
Connection.ExecuteNonQuery('CREATE INDEX idx_category ON products(category)');

// Composite index (multi-column)
Connection.ExecuteNonQuery('CREATE INDEX idx_cat_price ON products(category, price)');

// Partial index (conditional)
Connection.ExecuteNonQuery('CREATE INDEX idx_expensive ON products(price) WHERE price > 100');
```

### EXPLAIN QUERY PLAN

```pascal
DS := Connection.ExecuteQuery('EXPLAIN QUERY PLAN SELECT * FROM products WHERE price > 50');
while not DS.EOF do
begin
  WriteLn(DS.FieldByName('detail').AsString);
  DS.Next;
end;
```

### Cache size configuration

```pascal
Connection.SetCacheSize(8000);  // 8 MB cache
```

## Performance tips

| Technique | Impact |
|-----------|--------|
| Use transactions for bulk ops | 10-50x faster |
| Create appropriate indexes | 2-100x faster queries |
| Increase cache size | Faster repeated queries |
| Use WAL mode | Better concurrent access |
| Run ANALYZE | Better query plans |

## Building

```bash
lazbuild PerformanceOptimization.lpi
```

## Running

```bash
./PerformanceOptimization      # Linux/macOS
PerformanceOptimization.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
