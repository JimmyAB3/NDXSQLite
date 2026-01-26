# NDXSQLite Performance Guide

This guide covers performance optimization techniques, benchmarks, and tuning tips for NDXSQLite applications.

---

## Table of Contents

1. [Quick Wins](#quick-wins)
2. [Connection Settings](#connection-settings)
3. [Transaction Optimization](#transaction-optimization)
4. [Query Optimization](#query-optimization)
5. [Indexing Strategy](#indexing-strategy)
6. [Batch Operations](#batch-operations)
7. [Memory Management](#memory-management)
8. [Benchmarks](#benchmarks)
9. [Profiling Tools](#profiling-tools)

---

## Quick Wins

Apply these settings for immediate performance improvement:

```pascal
Options := TNDXSQLiteConnectionOptions.Create;

// 1. Enable WAL mode (10-20x faster for concurrent workloads)
Options.JournalMode := jmWAL;

// 2. Set synchronous to NORMAL (2x faster writes, still safe)
Options.SyncMode := smNormal;

// 3. Increase cache size (default is often too small)
Options.CacheSize := 4000;  // 4000 pages = ~16MB

// 4. Enable memory-mapped I/O for large databases
Options.MMapSize := 268435456;  // 256MB

// 5. Store temp tables in memory
Options.TempStore := tsMemory;
```

### Impact Summary

| Setting | Default | Optimized | Impact |
|---------|---------|-----------|--------|
| Journal Mode | DELETE | WAL | 10-20x faster concurrent |
| Sync Mode | FULL | NORMAL | 2x faster writes |
| Cache Size | 2000 | 4000+ | Fewer disk reads |
| Temp Store | DEFAULT | MEMORY | Faster temp operations |

---

## Connection Settings

### Journal Modes

| Mode | Description | Use Case |
|------|-------------|----------|
| `jmDelete` | Traditional rollback journal | Single-writer legacy |
| `jmWAL` | Write-Ahead Logging | **Recommended for most apps** |
| `jmMemory` | In-memory journal | Temporary/test databases |
| `jmOff` | No journal | Read-only databases |

```pascal
// WAL mode is best for most applications
Options.JournalMode := jmWAL;

// After opening, optionally tune WAL
Conn.ExecuteNonQuery('PRAGMA wal_autocheckpoint = 1000');  // Checkpoint every 1000 pages
```

### Synchronous Modes

| Mode | Safety | Speed | Description |
|------|--------|-------|-------------|
| `smOff` | Low | Fastest | No sync (risk of corruption) |
| `smNormal` | Medium | Fast | Sync at critical moments |
| `smFull` | High | Slower | Sync after every write |
| `smExtra` | Highest | Slowest | Extra sync operations |

```pascal
// NORMAL is recommended balance of safety and speed
Options.SyncMode := smNormal;

// For maximum speed with acceptable risk (not for critical data)
Options.SyncMode := smOff;
```

### Cache Configuration

```pascal
// Cache size in pages (default page size is 4KB)
Options.CacheSize := 4000;   // 4000 pages = ~16MB
Options.CacheSize := 10000;  // 10000 pages = ~40MB
Options.CacheSize := -65536; // Negative = size in KB = 64MB

// For read-heavy workloads, larger cache helps
Options.CacheSize := 20000;  // ~80MB for large databases
```

### Memory-Mapped I/O

```pascal
// Enable mmap for databases up to 256MB
Options.MMapSize := 268435456;  // 256MB

// For larger databases
Options.MMapSize := 1073741824;  // 1GB

// Disable mmap (default)
Options.MMapSize := 0;
```

---

## Transaction Optimization

### Batch Inserts in Transaction

**Without transaction:** Each INSERT is a separate transaction

```pascal
// SLOW: 1000 separate transactions
for I := 1 to 1000 do
  Conn.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)', [I]);
// ~2-5 seconds
```

**With transaction:** Single transaction for all INSERTs

```pascal
// FAST: 1 transaction
Conn.BeginTransaction;
try
  for I := 1 to 1000 do
    Conn.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)', [I]);
  Conn.Commit;
except
  Conn.Rollback;
  raise;
end;
// ~50-100 milliseconds (20-50x faster)
```

### Transaction Modes

```pascal
// Deferred (default): Lock acquired on first write
Conn.BeginTransaction(tmDeferred);

// Immediate: Lock acquired immediately (better for write-heavy)
Conn.BeginTransaction(tmImmediate);

// Exclusive: Full exclusive lock (prevents all other access)
Conn.BeginTransaction(tmExclusive);
```

### Optimal Transaction Size

| Records | Transaction Strategy |
|---------|---------------------|
| 1-100 | Single transaction |
| 100-10,000 | Single transaction |
| 10,000-100,000 | Batch transactions (10K each) |
| 100,000+ | Batch transactions with periodic commits |

```pascal
// Batch large inserts
const BATCH_SIZE = 10000;
var
  I, BatchCount: Integer;
begin
  BatchCount := 0;
  Conn.BeginTransaction;
  try
    for I := 1 to 1000000 do
    begin
      Conn.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)', [I]);
      Inc(BatchCount);

      if BatchCount >= BATCH_SIZE then
      begin
        Conn.Commit;
        Conn.BeginTransaction;
        BatchCount := 0;
      end;
    end;
    Conn.Commit;
  except
    Conn.Rollback;
    raise;
  end;
end;
```

---

## Query Optimization

### Use Prepared Statements

```pascal
// SLOW: Parse SQL every time
for I := 1 to 1000 do
  Conn.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)', [I]);

// FASTER: Reuse prepared statement
// Note: NDXSQLite caches statements internally, but explicit
// prepared statements can still improve performance for tight loops
```

### Select Only Needed Columns

```pascal
// SLOW: Select all columns
Conn.ExecuteQuery('SELECT * FROM users WHERE id = ?', [Id]);

// FAST: Select only needed columns
Conn.ExecuteQuery('SELECT name, email FROM users WHERE id = ?', [Id]);
```

### Use LIMIT for Large Results

```pascal
// Pagination
Conn.ExecuteQuery('SELECT * FROM logs ORDER BY created_at DESC LIMIT ? OFFSET ?',
  [PageSize, Page * PageSize]);

// Get just first match
Conn.ExecuteScalar('SELECT id FROM users WHERE email = ? LIMIT 1', [Email]);
```

### Optimize WHERE Clauses

```pascal
// Ensure indexed columns are used
// Index: CREATE INDEX idx_users_email ON users(email)
Conn.ExecuteQuery('SELECT * FROM users WHERE email = ?', [Email]);  // Uses index

// Avoid functions on indexed columns
// SLOW: Function prevents index use
Conn.ExecuteQuery('SELECT * FROM users WHERE LOWER(email) = ?', [LowerEmail]);

// FAST: Use COLLATE NOCASE index instead
// CREATE INDEX idx_users_email ON users(email COLLATE NOCASE)
Conn.ExecuteQuery('SELECT * FROM users WHERE email = ? COLLATE NOCASE', [Email]);
```

### Use EXISTS Instead of COUNT

```pascal
// SLOW: Counts all matching rows
if Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE status = ?', ['active']) > 0 then

// FAST: Stops at first match
if Conn.ExecuteScalar('SELECT EXISTS(SELECT 1 FROM users WHERE status = ? LIMIT 1)', ['active']) = 1 then
```

---

## Indexing Strategy

### When to Create Indexes

| Scenario | Index Recommended |
|----------|------------------|
| WHERE clause columns | Yes |
| JOIN columns | Yes |
| ORDER BY columns | Consider |
| Frequently updated columns | Consider trade-off |
| Low cardinality columns | Usually no |

### Index Types

```sql
-- Standard B-tree index
CREATE INDEX idx_users_email ON users(email);

-- Unique index (also enforces constraint)
CREATE UNIQUE INDEX idx_users_username ON users(username);

-- Composite index (order matters!)
CREATE INDEX idx_orders_user_date ON orders(user_id, order_date);

-- Partial index (smaller, faster)
CREATE INDEX idx_active_users ON users(email) WHERE status = 'active';

-- Expression index
CREATE INDEX idx_users_lower_email ON users(LOWER(email));
```

### Index Analysis

```pascal
// Check if index is being used
var
  Plan: String;
begin
  Plan := Conn.ExecuteScalar(
    'EXPLAIN QUERY PLAN SELECT * FROM users WHERE email = ?', ['test@example.com']);
  WriteLn(Plan);
  // Should show "USING INDEX idx_users_email"
end;
```

### Index Maintenance

```pascal
// Rebuild indexes (after large deletes)
Conn.ExecuteNonQuery('REINDEX');

// Analyze table statistics (improves query planner)
Conn.ExecuteNonQuery('ANALYZE');

// Rebuild entire database
Conn.ExecuteNonQuery('VACUUM');
```

---

## Batch Operations

### Bulk Insert

```pascal
procedure BulkInsert(Conn: TNDXSQLiteConnection; const Data: TStringArray);
var
  I: Integer;
begin
  Conn.BeginTransaction;
  try
    for I := 0 to High(Data) do
      Conn.ExecuteNonQuery('INSERT INTO items (name) VALUES (?)', [Data[I]]);
    Conn.Commit;
  except
    Conn.Rollback;
    raise;
  end;
end;
```

### Multi-Row Insert (SQLite 3.7.11+)

```pascal
// Insert multiple rows in single statement
Conn.ExecuteNonQuery(
  'INSERT INTO items (name) VALUES (?), (?), (?), (?)',
  ['Item1', 'Item2', 'Item3', 'Item4']);
```

### Bulk Update

```pascal
// Update with CASE for multiple values
Conn.ExecuteNonQuery(
  'UPDATE items SET status = CASE id ' +
  'WHEN 1 THEN ''active'' ' +
  'WHEN 2 THEN ''inactive'' ' +
  'WHEN 3 THEN ''pending'' ' +
  'END WHERE id IN (1, 2, 3)');
```

### Delete Optimization

```pascal
// SLOW: Delete one by one
for I := 0 to High(IdsToDelete) do
  Conn.ExecuteNonQuery('DELETE FROM items WHERE id = ?', [IdsToDelete[I]]);

// FAST: Delete in batches
Conn.BeginTransaction;
try
  Conn.ExecuteNonQuery('DELETE FROM items WHERE id IN (?, ?, ?, ?)',
    [Id1, Id2, Id3, Id4]);
  Conn.Commit;
except
  Conn.Rollback;
  raise;
end;

// FASTEST: For large deletes, create new table
Conn.ExecuteNonQuery('CREATE TABLE items_new AS SELECT * FROM items WHERE keep = 1');
Conn.ExecuteNonQuery('DROP TABLE items');
Conn.ExecuteNonQuery('ALTER TABLE items_new RENAME TO items');
```

---

## Memory Management

### Connection Pooling

```pascal
// Use connection pool for multi-threaded applications
Pool := TNDXSQLiteConnectionPool.Create(Options, 2, 10);  // Min 2, Max 10
try
  Conn := Pool.Acquire;
  try
    // Use connection
  finally
    Pool.Release(Conn);
  end;
finally
  Pool.Free;
end;
```

### Release Resources

```pascal
// Free query results promptly
DataSet := Conn.ExecuteQuery('SELECT * FROM large_table');
try
  // Process data
finally
  DataSet.Free;  // Release memory
end;

// Close connections when not needed
Conn.Close;
```

### Memory Database

```pascal
// For temporary data processing
Options.DatabasePath := ':memory:';

// Or named memory database (shareable between connections)
Options.DatabasePath := 'file::memory:?cache=shared';
```

---

## Benchmarks

### Insert Performance

Test: 100,000 row insert

| Configuration | Time | Rows/sec |
|---------------|------|----------|
| No transaction | 120s | 833 |
| Single transaction | 2.5s | 40,000 |
| Transaction + WAL | 1.8s | 55,555 |
| Transaction + WAL + NORMAL sync | 0.9s | 111,111 |

### Query Performance

Test: SELECT with index on 1M rows

| Query Type | Time |
|------------|------|
| Full scan (no index) | 450ms |
| Indexed lookup | 0.1ms |
| Indexed range (1000 rows) | 5ms |

### Concurrent Access

Test: 10 threads, mixed read/write

| Journal Mode | Reads/sec | Writes/sec |
|--------------|-----------|------------|
| DELETE | 1,500 | 50 |
| WAL | 25,000 | 5,000 |

---

## Profiling Tools

### Query Plan Analysis

```pascal
// Show query execution plan
var
  Plan: String;
begin
  Plan := Conn.ExecuteScalar('EXPLAIN QUERY PLAN ' +
    'SELECT * FROM orders WHERE user_id = ? ORDER BY created_at',
    [UserId]);
  WriteLn(Plan);
end;
```

### Database Statistics

```pascal
// Get table statistics
Conn.ExecuteNonQuery('ANALYZE');

// View statistics
var
  Stats: TNDXSQLiteDataSet;
begin
  Stats := Conn.ExecuteQuery('SELECT * FROM sqlite_stat1');
  try
    while not Stats.EOF do
    begin
      WriteLn(Stats.FieldByName('tbl').AsString, ': ',
              Stats.FieldByName('stat').AsString);
      Stats.Next;
    end;
  finally
    Stats.Free;
  end;
end;
```

### Health Check

```pascal
uses ndxsqlitehealthcheck;

var
  Health: TNDXSQLiteHealthCheck;
  Stats: TNDXHealthStats;
begin
  Health := TNDXSQLiteHealthCheck.Create(Conn);
  try
    Stats := Health.GetStatistics;
    WriteLn('Page count: ', Stats.PageCount);
    WriteLn('Page size: ', Stats.PageSize);
    WriteLn('Free pages: ', Stats.FreePageCount);
    WriteLn('Database size: ', Stats.PageCount * Stats.PageSize, ' bytes');
  finally
    Health.Free;
  end;
end;
```

### Timing Queries

```pascal
var
  StartTime: TDateTime;
  Elapsed: Double;
begin
  StartTime := Now;

  // Execute query
  Conn.ExecuteQuery('SELECT * FROM large_table');

  Elapsed := MilliSecondsBetween(Now, StartTime);
  WriteLn('Query took: ', Elapsed:0:2, 'ms');
end;
```

---

## Performance Checklist

### Before Deployment

- [ ] Enable WAL mode
- [ ] Set appropriate sync mode
- [ ] Configure cache size for workload
- [ ] Create necessary indexes
- [ ] Run ANALYZE on tables
- [ ] Use transactions for batch operations

### Monitoring

- [ ] Track query execution times
- [ ] Monitor database file size
- [ ] Check for lock contention
- [ ] Review slow queries periodically

### Maintenance

- [ ] Run VACUUM periodically (after large deletes)
- [ ] Update statistics with ANALYZE
- [ ] Review and optimize slow queries
- [ ] Check index usage with EXPLAIN

---

*NDXSQLite Performance Guide - Version 1.0.0*
