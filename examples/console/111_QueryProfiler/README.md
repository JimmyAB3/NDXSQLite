# Example 111: Query Profiler - Timing, Slow Query Log, Query Plan Cache

## Overview

This example demonstrates a **Query Profiler** with execution timing, slow query detection and logging, query plan analysis via EXPLAIN QUERY PLAN, plan caching, index impact measurement, and persisted query statistics.

## Features Demonstrated

### 1. Query Timing
- Measure execution time of each query using GetTickCount64
- Track row counts per query
- Profile both SELECT (ExecuteQuery) and DML (ExecuteNonQuery)
- Multiple executions for statistical averaging

### 2. Slow Query Log
- Configurable threshold (default: 5ms)
- Automatic detection of queries exceeding threshold
- Logging with SQL text, execution time, row count, timestamp
- Persistence to query_log table for later analysis

### 3. Query Plan Analysis
- EXPLAIN QUERY PLAN for any SQL query
- Identifies: SCAN (full table scan), SEARCH (index lookup)
- Detects USE TEMP B-TREE (sort without index)
- Shows SCALAR SUBQUERY execution strategy

### 4. Query Plan Cache
- Cache analyzed plans to avoid repeated EXPLAIN calls
- Cache hit detection on subsequent calls
- Persistence to query_plan_cache table
- Useful for repeated query patterns

### 5. Index Impact Analysis
- Compare execution time before/after index creation
- Show plan change (SCAN vs SEARCH USING INDEX)
- Measure improvement ratio over multiple executions
- Demonstrate covering index optimization

### 6. Query Statistics Summary
- Total profiled queries count
- Unique query patterns tracked
- Per-query: call count, total/avg/min/max time, total rows
- Separate view for single vs repeated queries

### 7. Profile Log Persistence
- Batch persist all profile entries to database
- Aggregated summary (total, slow count, avg/max time)
- Query plan cache persistence with hit counts

### 8. Profiler Reset
- Clear in-memory profiler state
- Retain persisted data in database tables
- Enable fresh profiling sessions

## Architecture

```
+------------------+     +------------------+
| Application      |     | Profiler Engine  |
+------------------+     +------------------+
| ProfiledQuery()  |---->| AddProfileEntry  |
| ProfiledExec()   |     | UpdateQueryStats |
| GetQueryPlan()   |     | SlowQueryCheck   |
+------------------+     +------------------+
        |                         |
        v                         v
+------------------+     +------------------+
| NDXSQLite        |     | In-Memory State  |
| Connection       |     +------------------+
+------------------+     | ProfileLog[]     |
| ExecuteQuery     |     | SlowQueryLog[]   |
| ExecuteNonQuery  |     | QueryStats[]     |
| EXPLAIN QP       |     | PlanCache[]      |
+------------------+     +------------------+
                                  |
                                  v
                          +------------------+
                          | Persistence      |
                          +------------------+
                          | query_log        |
                          | query_plan_cache |
                          +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| query_log        |     | query_plan_cache |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| sql_text         |     | sql_text (UNIQUE)|
| execution_ms     |     | plan_detail      |
| row_count        |     | cached_at        |
| logged_at        |     | hit_count        |
| is_slow          |     +------------------+
+------------------+

Domain tables:
+------------------+     +------------------+
| products (1000)  |     | orders (5000)    |
+------------------+     +------------------+
| id, name         |     | id, product_id   |
| category, price  |     | quantity, total   |
| stock, created_at|     | order_date       |
+------------------+     | customer         |
                          +------------------+
```

## Compilation

```bash
cd 111_QueryProfiler
lazbuild QueryProfiler.lpi
./QueryProfiler
```

## Sample Output

```
1. Query Timing
   SELECT COUNT(*) FROM products: 1000 rows, 0.0 ms
   JOIN + GROUP BY + ORDER: 10 rows, 3.0 ms

2. Slow Query Log (threshold: 5 ms)
   [SLOW] 6.0 ms | 5 rows | SELECT p.category, COUNT(*)...

3. Query Plan Analysis
   Plan: SCAN products (full table scan)
   Plan: SEARCH products USING INTEGER PRIMARY KEY (rowid=?)
   Plan: SCAN products | USE TEMP B-TREE FOR ORDER BY

5. Index Impact Analysis
   Before: SCAN products - 68ms/100 runs
   After:  SEARCH USING INDEX idx_products_category_price
```

## Related Examples

- **112_DatabaseMonitoring** - Database health and performance metrics
- **113_DataArchiving** - Data lifecycle management

## Best Practices

1. **Profile selectively**: Don't profile every query in production; use sampling
2. **Threshold tuning**: Adjust slow query threshold based on your SLA requirements
3. **Plan cache invalidation**: Clear cache after schema changes (CREATE INDEX, ALTER TABLE)
4. **Index analysis**: Test with realistic data volumes; small datasets may not benefit from indexes
5. **Persistence**: Store profiling data for trend analysis and capacity planning
6. **Statistics reset**: Reset counters periodically to track current performance, not historical
7. **Query plan awareness**: SCAN = full table scan (consider indexing); SEARCH = indexed lookup (good)
8. **Covering indexes**: Include all needed columns in the index to avoid table lookups
