# Example 21: Memory Databases

This example provides a comprehensive demonstration of in-memory SQLite databases, including shared cache connections, memory statistics, backup/restore operations, and practical use case patterns.

## What you'll learn

- Creating and using in-memory databases (`:memory:`)
- Named memory databases with shared cache (URI mode)
- Temporary tables within memory databases
- Performance comparison: memory vs disk
- Memory statistics and monitoring (PRAGMAs)
- Copying disk databases to memory for fast processing
- Backing up memory databases to disk (persistence)
- Practical use case patterns with working code

## Key concepts

### Creating a memory database

The simplest way to create an in-memory database:

```pascal
// Using :memory: special filename
Conn := TNDXSQLiteConnection.Create(':memory:');
Conn.Open;

// All data exists only in RAM
Conn.ExecuteNonQuery('CREATE TABLE data (id INTEGER PRIMARY KEY, value TEXT)');
Conn.ExecuteNonQuery('INSERT INTO data VALUES (1, ''Hello Memory!'')');

Conn.Close;  // Data is LOST when connection closes!
```

### Using the connection factory

```pascal
Factory := TNDXSQLiteConnectionFactory.Create(':memory:');
try
  Conn := Factory.CreateMemoryConnection;
  Conn.Open;
  // Use connection...
  Conn.Close;
finally
  Factory.Free;
end;
```

### Named memory database with shared cache

Multiple connections can share the same in-memory database using URI syntax:

```pascal
// URI format: file:dbname?mode=memory&cache=shared
SharedURI := 'file:my_shared_db?mode=memory&cache=shared';

// Connection 1 creates the database
Conn1 := TNDXSQLiteConnection.Create(SharedURI);
Conn1.Open;
Conn1.ExecuteNonQuery('CREATE TABLE shared_data (id INTEGER, msg TEXT)');
Conn1.ExecuteNonQuery('INSERT INTO shared_data VALUES (1, ''From Conn1'')');

// Connection 2 accesses the SAME database
Conn2 := TNDXSQLiteConnection.Create(SharedURI);
Conn2.Open;

// Conn2 can see data from Conn1!
DS := Conn2.ExecuteQuery('SELECT * FROM shared_data');

// Conn2 inserts data that Conn1 can see
Conn2.ExecuteNonQuery('INSERT INTO shared_data VALUES (2, ''From Conn2'')');

// Conn1 sees the new row immediately
Count := Conn1.ExecuteScalar('SELECT COUNT(*) FROM shared_data');  // Returns 2
```

### Temporary tables

Temp tables exist only for the current session:

```pascal
Conn.ExecuteNonQuery('CREATE TEMP TABLE session_cart (product_id INTEGER, qty INTEGER)');
Conn.ExecuteNonQuery('INSERT INTO session_cart VALUES (101, 2)');

// Temp table is automatically dropped when connection closes
```

### Memory statistics and monitoring

```pascal
// Database size info
PageSize := Conn.ExecuteScalar('PRAGMA page_size');      // Typically 4096 bytes
PageCount := Conn.ExecuteScalar('PRAGMA page_count');    // Number of pages
DBSize := PageSize * PageCount;                          // Total size in bytes

// Cache configuration
CacheSize := Conn.ExecuteScalar('PRAGMA cache_size');    // Negative = KB, Positive = pages

// Unused space
FreelistPages := Conn.ExecuteScalar('PRAGMA freelist_count');

// Global SQLite memory (requires ndxsqlite3api)
if Assigned(sqlite3_memory_used) then
begin
  CurrentMem := sqlite3_memory_used();          // Current allocation
  HighwaterMem := sqlite3_memory_highwater(0);  // Peak allocation
end;
```

### Copy disk database to memory

Load a disk database into memory for fast processing:

```pascal
MemConn := TNDXSQLiteConnection.Create(':memory:');
MemConn.Open;

// Attach the disk database
MemConn.ExecuteNonQuery('ATTACH DATABASE ''data.db'' AS disk_db');

// Copy tables to memory
MemConn.ExecuteNonQuery('CREATE TABLE customers AS SELECT * FROM disk_db.customers');
MemConn.ExecuteNonQuery('CREATE TABLE orders AS SELECT * FROM disk_db.orders');

// Detach disk database
MemConn.ExecuteNonQuery('DETACH DATABASE disk_db');

// Now query at memory speed!
DS := MemConn.ExecuteQuery('SELECT * FROM customers JOIN orders ON ...');
```

### Backup memory database to disk

Persist a memory database using VACUUM INTO (SQLite 3.27+):

```pascal
MemConn := TNDXSQLiteConnection.Create(':memory:');
MemConn.Open;
// ... populate database ...

// Backup to disk file using VACUUM INTO (atomic, fast)
MemConn.ExecuteNonQuery('VACUUM INTO ''backup.db''');

// Verify backup
VerifyConn := TNDXSQLiteConnection.Create('backup.db');
VerifyConn.Open;
WriteLn('Rows: ', VerifyConn.ExecuteScalar('SELECT COUNT(*) FROM mytable'));
WriteLn('Integrity: ', VerifyConn.ExecuteScalar('PRAGMA integrity_check'));
VerifyConn.Close;
VerifyConn.Free;

MemConn.Close;
MemConn.Free;
```

## Memory vs Disk comparison

| Aspect | Memory (`:memory:`) | Disk |
|--------|---------------------|------|
| Speed | Extremely fast (no I/O) | Slower (disk I/O) |
| Persistence | Lost on close | Permanent |
| Size limit | Available RAM | Disk space |
| Concurrent access | Single process* | Multi-process |
| Crash recovery | None | WAL/Journal |
| Use case | Temp data, testing, caching | Production data |

\* Unless using shared cache URI mode

## Connection string formats

| Format | Description |
|--------|-------------|
| `:memory:` | Private memory database (new for each connection) |
| `file:name?mode=memory&cache=shared` | Named shared memory database |
| `file::memory:?cache=shared` | Anonymous shared memory database |

## Use cases

### Unit Testing
```pascal
// Fast, isolated tests with no cleanup
Conn := TNDXSQLiteConnection.Create(':memory:');
Conn.Open;
Conn.ExecuteNonQuery('CREATE TABLE users (email TEXT UNIQUE)');
Conn.ExecuteNonQuery('INSERT INTO users VALUES (''test@example.com'')');
// Assert...
Conn.Close;  // Database vanishes - no cleanup needed!
```

### Query Result Caching
```pascal
// Cache expensive query results
Conn.ExecuteNonQuery('CREATE TABLE cache (key TEXT PRIMARY KEY, value TEXT, expires INTEGER)');
Conn.ExecuteNonQuery('INSERT INTO cache VALUES (''query_hash'', ''{"result":...}'', ...)');
```

### Data Processing Pipeline
```pascal
// Load data into memory for complex analysis
Conn.ExecuteNonQuery('CREATE TABLE raw_data (value REAL)');
// Import data...
DS := Conn.ExecuteQuery('SELECT AVG(value), STDDEV(value), ... FROM raw_data');
```

### Session Storage
```pascal
// In-memory session storage for web applications
Conn.ExecuteNonQuery('CREATE TABLE sessions (id TEXT PRIMARY KEY, data TEXT, expires INTEGER)');
Conn.ExecuteNonQuery('CREATE INDEX idx_expires ON sessions(expires)');
```

## Building

```bash
lazbuild MemoryDatabases.lpi
```

## Running

```bash
./MemoryDatabases      # Linux/macOS
MemoryDatabases.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 21: Memory Databases ===

1. Basic In-Memory Database
   ========================
   Database path: :memory:
   Connection is open: TRUE
   Created table with 3 users
   Data in memory:
     - Alice <alice@example.com>
     - Bob <bob@example.com>
     - Carol <carol@example.com>
   Connection closed - all data is now LOST!
   (This is the key characteristic of memory databases)

2. Memory Database with Factory
   ============================
   Factory-created memory connection
   App name: MyApp
   Version: 2.1.0
   Config entries: 3

3. Performance: Memory vs Disk
   ===========================
   Test: 50000 INSERT operations + query

   Memory INSERT: 892 ms (56053 rows/sec)
   Memory QUERY:  1 ms
   Disk INSERT:   1245 ms (40160 rows/sec)
   Disk QUERY:    2 ms

   INSERT speedup: Memory is 1.4x faster
   QUERY speedup:  Memory is 2.0x faster

4. Temporary Tables
   =================
   Created permanent table "products" and temp table "cart"

   All tables:
     [main] products
     [temp] cart

   Cart contents:
     2x Laptop @ $999.99 = $1999.98
     1x Tablet @ $399.99 = $399.99
   Cart total: $2399.97

   Note: TEMP tables are automatically dropped when connection closes

5. Named Memory Database with Shared Cache
   ========================================
   URI: file:shared_mem_db?mode=memory&cache=shared

   Connection 1: Opened shared memory database
   Connection 1: Created table with 2 rows
   Connection 2: Opened SAME shared memory database
   Connection 2: Reading data from shared table:
     [1] Hello from Connection 1
     [2] Shared memory rocks!
   Connection 2: Inserted new row
   Connection 1: Row count = 3

   Both connections shared the SAME in-memory database!
   (Unlike :memory: which creates separate databases)

6. Memory Statistics and Monitoring
   =================================
   Initial state:
     Page size:    4096 bytes
     Page count:   1
     Cache size:   2000 KB

   Inserting 1000 rows with 1KB BLOBs...

   After inserting ~1MB of data:
     Page count:   274
     Estimated DB size: 1096 KB

   SQLite global memory usage:
     Current:   1542 KB
     Highwater: 1876 KB

   Database statistics:
     Total rows: 1000
     Total BLOB size: 1000 KB
     Freelist pages: 0

7. Copy Disk Database to Memory
   =============================
   Created disk database with 5 customers

   Method 1: ATTACH + CREATE TABLE AS SELECT
   Copied to memory in 2 ms
   Data in memory:
     Alice Smith - New York
     Bob Johnson - Los Angeles
     Carol Williams - Chicago
     David Brown - Houston
     Eve Davis - Phoenix
   Total customers: 5

8. Backup Memory Database to Disk
   ===============================
   Created memory database with 100 transactions
   Total amount: $49823.45

   Backing up to disk using VACUUM INTO...
   Backup successful!
     Destination: .../memory_backup.db
     Size: 20 KB
     Time: 6 ms

   Verifying backup...
   Rows in backup: 100
   Backup integrity: ok

9. Practical Use Cases
   ====================

   Use Case 1: Unit Testing (isolated, fast, no cleanup)
   -----------------------------------------------------
   Test PASSED: User created successfully

   Use Case 2: Query Result Caching
   ---------------------------------
   Cached result: {"data": [1,2,3]}

   Use Case 3: Complex Sorting/Filtering Pipeline
   -----------------------------------------------
   Analyzed 10000 values in 15 ms:
     Mean: 49.87, Min: 0.01, Max: 99.99

   Use Case 4: Web Session Storage
   --------------------------------
   Session data: {"cart": []}

10. Memory Database Summary
    =======================

    Advantages:
      + Extremely fast (no disk I/O)
      + No file cleanup needed
      + Isolated for testing
      + Can be shared via URI mode
      + Can be backed up to disk

    Limitations:
      - Data lost when all connections close
      - Limited by available RAM
      - Cannot be accessed by other processes (unless shared cache)
      - No crash recovery

    Connection strings:
      :memory:                              Private memory DB
      file:name?mode=memory&cache=shared    Shared memory DB
      file::memory:?cache=shared            Anonymous shared DB

=== Example completed successfully! ===
```

## Best practices

1. **Use transactions for bulk inserts** - Even in memory, transactions improve performance significantly

2. **Monitor memory usage** - Use `sqlite3_memory_used()` and PRAGMAs to track consumption

3. **Backup important data** - Use the Backup API to persist memory databases if needed

4. **Choose the right connection string**:
   - `:memory:` for isolated, single-connection use
   - URI mode for shared access between connections

5. **Set appropriate cache size** - Memory databases can use larger caches:
   ```pascal
   Conn.ExecuteNonQuery('PRAGMA cache_size = -64000');  // 64MB cache
   ```

6. **Use TEMP tables** for session-scoped data within a connection

## Notes

- Memory databases are created fresh for each `:memory:` connection
- Shared cache mode requires URI syntax: `file:name?mode=memory&cache=shared`
- The Backup API can save memory databases to disk files
- Memory databases support all SQLite features (FTS, JSON, R-Tree, etc.)
- Performance varies by system - memory databases eliminate I/O but not CPU work

## Cross-Platform

This example works on Windows, Linux, and macOS.
