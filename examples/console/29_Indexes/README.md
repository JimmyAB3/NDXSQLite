# Example 29: Indexes

This example demonstrates index creation and usage in SQLite for query optimization.

## What you'll learn

- Creating indexes for performance
- Unique indexes for data integrity
- Composite (multi-column) indexes
- Partial indexes with WHERE clauses
- Expression indexes
- Covering indexes
- Using EXPLAIN QUERY PLAN
- ANALYZE for query optimizer statistics

## Key concepts

### Basic index

```pascal
// Create index on frequently queried column
Connection.ExecuteNonQuery('CREATE INDEX idx_customers_email ON customers(email)');

// Verify index is used
DS := Connection.ExecuteQuery(
  'EXPLAIN QUERY PLAN SELECT * FROM customers WHERE email = ''test@example.com''');
// Look for "USING INDEX idx_customers_email"
```

### Unique index

```pascal
// Enforces uniqueness (like UNIQUE constraint)
Connection.ExecuteNonQuery(
  'CREATE UNIQUE INDEX idx_users_email ON users(email)');

// Duplicate inserts will fail
```

### Composite index

```pascal
// Index on multiple columns
Connection.ExecuteNonQuery(
  'CREATE INDEX idx_customers_country_city ON customers(country, city)');

// Works for:
// - WHERE country = ?
// - WHERE country = ? AND city = ?
// Does NOT help:
// - WHERE city = ?  (second column alone)
```

### Partial index

```pascal
// Index only specific rows (saves space, faster)
Connection.ExecuteNonQuery(
  'CREATE INDEX idx_active_users ON users(email) WHERE is_active = 1');

// Only used when query matches the WHERE condition
SELECT * FROM users WHERE email = ? AND is_active = 1;  -- uses index
SELECT * FROM users WHERE email = ? AND is_active = 0;  -- does NOT use index
```

### Expression index

```pascal
// Index on computed expression
Connection.ExecuteNonQuery(
  'CREATE INDEX idx_users_email_lower ON users(lower(email))');

// Used when query uses same expression
SELECT * FROM users WHERE lower(email) = 'test@example.com';
```

### Covering index

```pascal
// Include all columns needed by query
Connection.ExecuteNonQuery(
  'CREATE INDEX idx_covering ON customers(country, first_name, last_name)');

// Query satisfied entirely from index (no table lookup)
SELECT first_name, last_name FROM customers WHERE country = 'US';
```

### EXPLAIN QUERY PLAN

```pascal
// See how SQLite will execute a query
DS := Connection.ExecuteQuery('EXPLAIN QUERY PLAN SELECT * FROM users WHERE email = ?');
// Returns query execution details including index usage
```

### ANALYZE

```pascal
// Gather statistics for query optimizer
Connection.ExecuteNonQuery('ANALYZE');

// Check statistics
DS := Connection.ExecuteQuery('SELECT * FROM sqlite_stat1');
```

### List indexes

```pascal
// All indexes in database
DS := Connection.ExecuteQuery(
  'SELECT name, tbl_name, sql FROM sqlite_master WHERE type = ''index''');

// Indexes on specific table
DS := Connection.ExecuteQuery('PRAGMA index_list(customers)');

// Columns in an index
DS := Connection.ExecuteQuery('PRAGMA index_info(idx_customers_email)');
```

### Drop index

```pascal
Connection.ExecuteNonQuery('DROP INDEX IF EXISTS idx_customers_email');
```

## Index types comparison

| Type | Use Case |
|------|----------|
| Basic | Speed up WHERE, JOIN, ORDER BY |
| Unique | Enforce uniqueness + speed |
| Composite | Multi-column filters |
| Partial | Subset of rows (sparse data) |
| Expression | Case-insensitive search, computed values |
| Covering | Avoid table lookups |

## Best practices

1. **Index columns used in WHERE, JOIN, ORDER BY**
2. **Composite index column order matters** - put most selective first
3. **Don't over-index** - each index slows writes
4. **Use EXPLAIN QUERY PLAN** to verify index usage
5. **Run ANALYZE** after bulk data changes
6. **Consider partial indexes** for filtered queries

## Building

```bash
lazbuild Indexes.lpi
```

## Running

```bash
./Indexes      # Linux/macOS
Indexes.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
