# Example 112: Database Monitoring - Size, Fragmentation, Connections, Cache

## Overview

This example demonstrates **Database Monitoring** with size metrics via PRAGMA, fragmentation analysis with VACUUM, table/index statistics, cache configuration, connection lifecycle tracking, health checks, and storage analysis.

## Features Demonstrated

### 1. Database Size Metrics
- PRAGMA page_count: total pages in database
- PRAGMA page_size: bytes per page (default 4096)
- PRAGMA freelist_count: unused/free pages
- Calculated: total size, used size, free size, space efficiency

### 2. Fragmentation Analysis
- Freelist pages ratio as fragmentation indicator
- PRAGMA auto_vacuum status (OFF/FULL/INCREMENTAL)
- Simulated fragmentation via row deletion
- VACUUM command to defragment and reclaim space
- Before/after comparison showing page count reduction

### 3. Table Statistics
- Row counts per table via SELECT COUNT(*)
- Column counts via PRAGMA table_info
- Estimated storage size per table
- Schema enumeration from sqlite_master

### 4. Index Statistics
- List all user-created indexes
- Index-to-table mapping
- Column coverage per index
- Total index count across database

### 5. Cache Configuration
- PRAGMA cache_size (pages or KB mode)
- Cache memory calculation
- Dynamic cache size adjustment
- Related PRAGMAs: journal_mode, synchronous, temp_store, locking_mode

### 6. Connection Tracking
- Connection lifecycle (open/close timestamps)
- Active connection counting
- Multiple simultaneous connections to same database
- Connection history log

### 7. Database Health Check
- PRAGMA integrity_check (B-tree structural validation)
- PRAGMA foreign_key_check (referential integrity)
- Fragmentation threshold alerts (OK/Warning/Critical)
- Cache size adequacy check
- Schema object counts

### 8. Storage Analysis
- Total database size breakdown
- Row distribution across tables
- Schema object counts by type
- SQLite version information

## Architecture

```
+------------------+     +------------------+
| Monitoring       |     | SQLite PRAGMAs   |
+------------------+     +------------------+
| DemoSize()       |---->| page_count       |
| DemoFragment()   |     | page_size        |
| DemoTableStats() |     | freelist_count   |
| DemoCacheConfig()|     | cache_size       |
| DemoHealthCheck()|     | integrity_check  |
+------------------+     | auto_vacuum      |
        |                 +------------------+
        v
+------------------+     +------------------+
| Connection       |     | Health Metrics   |
| Tracker          |     +------------------+
+------------------+     | Integrity: OK    |
| RegisterConn()   |     | FK Check: OK     |
| CloseConn()      |     | Fragment: 0.0%   |
| GetOpenCount()   |     | Cache: 4000 pg   |
+------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| customers (500)  |     | products (200)   |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| name, email      |     | name, category   |
| city, balance    |     | price, stock     |
| created_at       |     | description      |
+------------------+     | is_active        |
        |                 +------------------+
        v                         |
+------------------+              v
| orders (3000)    |     +------------------+
+------------------+     | audit_log (1000) |
| id (PK, AUTO)    |     +------------------+
| customer_id (FK) |     | id (PK, AUTO)    |
| product_id (FK)  |     | table_name       |
| quantity, total   |     | action           |
| status           |     | record_id        |
| order_date       |     | details          |
+------------------+     | logged_at        |
                          +------------------+
```

## Compilation

```bash
cd 112_DatabaseMonitoring
lazbuild DatabaseMonitoring.lpi
./DatabaseMonitoring
```

## Sample Output

```
1. Database Size Metrics
   Page size:       4096 bytes
   Total pages:     112
   Total size:      448.0 KB
   Space efficiency: 100.0%

2. Fragmentation Analysis
   After delete - Freelist: 1 pages (0.9% fragmented)
   After VACUUM - Freelist: 0 pages (0.0% fragmented)

7. Database Health Check
   [OK] Integrity check: PASSED
   [OK] Foreign key check: No violations
   [OK] Fragmentation: 0.0%
   [OK] Cache size: 4000 pages

8. Storage Analysis
   SQLite version: 3.45.1
```

## Related Examples

- **111_QueryProfiler** - Query performance measurement
- **113_DataArchiving** - Data lifecycle management

## Best Practices

1. **Regular integrity checks**: Run PRAGMA integrity_check on schedules for early corruption detection
2. **Monitor fragmentation**: Track freelist_count; VACUUM when fragmentation exceeds 10-20%
3. **Cache sizing**: Set cache_size based on working set; larger cache = fewer disk reads
4. **Connection pooling**: Track open connections to prevent resource leaks
5. **Index awareness**: Too many indexes slow writes; too few slow reads. Monitor usage
6. **VACUUM timing**: Run during low-traffic periods; it locks the database and creates a copy
7. **Page size selection**: 4096 is default; larger pages better for large BLOBs, smaller for many small records
8. **Health dashboards**: Combine metrics into periodic health reports for proactive maintenance
