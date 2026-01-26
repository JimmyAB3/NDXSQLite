# Example 83: Query Plan Analysis

## Overview

This example demonstrates how to use **EXPLAIN QUERY PLAN** to analyze and optimize SQLite queries. It covers full table scans, index usage, composite indexes, covering indexes, JOIN ordering, subquery analysis, and performance measurement — essential skills for optimizing database-driven applications.

## Features Demonstrated

### 1. Full Table Scan Detection
- SCAN TABLE identification (O(n) operations)
- PRIMARY KEY lookup (O(log n))
- Recognizing when indexes are needed

### 2. Index Impact Measurement
- Before/after index performance comparison
- SCAN to SEARCH transition
- Timing queries with multiple iterations

### 3. Composite Index Analysis
- Multi-column index effectiveness
- Prefix usage (leftmost columns)
- Column order importance

### 4. Covering Index
- All required columns in index
- Avoiding table lookups
- USING COVERING INDEX in plans

### 5. JOIN Order Analysis
- Two, three, and four-table JOINs
- SQLite's automatic join reordering
- Index selection for join conditions

### 6. Subquery Plans
- Correlated subqueries (CORRELATED SCALAR SUBQUERY)
- IN subquery (LIST SUBQUERY)
- EXISTS optimization
- Rewriting as JOIN for efficiency

### 7. ORDER BY / GROUP BY
- Temp B-tree detection for sorting
- Index-assisted ordering
- GROUP BY with and without index

### 8. Performance Comparisons
- SELECT * vs specific columns
- COUNT(*) vs COUNT(column)
- LIMIT impact on performance

## Database Schema

```
+------------------+     +------------------+
| orders (5000)    |     | customers (100)  |
+------------------+     +------------------+
| id (PK)          |     | id (PK)          |
| customer_id (FK) |---->| name             |
| product_id (FK)  |     | email (UNIQUE)   |
| order_date       |     | city             |
| amount           |     | country          |
| status           |     | tier             |
| region           |     +------------------+
| notes            |
+------------------+     +------------------+
        |                 | products (50)    |
        |                 +------------------+
        |                 | id (PK)          |
        v                 | name             |
+------------------+      | category         |
| order_items(5000)|      | price            |
+------------------+      | stock            |
| id (PK)          |      +------------------+
| order_id (FK)    |
| product_id (FK)  |---->
| quantity         |
| unit_price       |
+------------------+
```

## Indexes Created

| Index | Table | Columns | Purpose |
|-------|-------|---------|---------|
| idx_orders_status | orders | status | Single column filter |
| idx_orders_customer | orders | customer_id | JOIN optimization |
| idx_orders_date | orders | order_date | Date range/ORDER BY |
| idx_orders_region | orders | region | Region filter |
| idx_status_region | orders | status, region | Composite filter |
| idx_cover_status_amount | orders | status, amount | Covering index |
| idx_order_items_order | order_items | order_id | JOIN to orders |
| idx_order_items_product | order_items | product_id | JOIN to products |

## Query Plan Keywords

| Keyword | Meaning |
|---------|---------|
| SCAN | Full table scan (reads every row) |
| SEARCH | Index-assisted lookup (selective) |
| USING INDEX | Named index being used |
| USING COVERING INDEX | All columns from index (no table access) |
| USING INTEGER PRIMARY KEY | Rowid lookup (fastest) |
| USE TEMP B-TREE | Temporary sort/group structure |
| CORRELATED SCALAR SUBQUERY | Per-row subquery execution |
| LIST SUBQUERY | Materialized subquery result |

## Key Patterns

### Reading Query Plans
```pascal
procedure ShowQueryPlan(const SQL: string);
var
  DS: TDataSet;
begin
  DS := Conn.ExecuteQuery('EXPLAIN QUERY PLAN ' + SQL);
  try
    while not DS.EOF do
    begin
      WriteLn(DS.FieldByName('detail').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;
```

### Timing Queries
```pascal
function TimeQuery(const SQL: string; Iterations: Integer): Double;
var
  DS: TDataSet;
  Start: TDateTime;
begin
  Start := Now;
  for I := 1 to Iterations do
  begin
    DS := Conn.ExecuteQuery(SQL);
    try
      while not DS.EOF do DS.Next;
    finally
      DS.Free;
    end;
  end;
  Result := MilliSecondsBetween(Now, Start) / Iterations;
end;
```

### Index Impact Comparison
```pascal
// Before index
ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped''');
// -> SCAN orders

Conn.ExecuteNonQuery('CREATE INDEX idx_status ON orders(status)');

// After index
ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped''');
// -> SEARCH orders USING INDEX idx_status (status=?)
```

## Demonstration Sections

1. **Schema Creation** - Tables with various column types
2. **Test Data** - 5000 orders, 100 customers, 50 products
3. **Full Table Scan** - Detecting O(n) operations
4. **Index Impact** - SCAN to SEARCH improvement
5. **Composite Index** - Multi-column index behavior
6. **Covering Index** - Avoiding table lookups
7. **JOIN Order** - Multi-table join analysis
8. **Subquery Plans** - Correlated vs materialized
9. **ORDER BY / GROUP BY** - Sort optimization
10. **Performance** - Timing comparisons
11. **Index Summary** - All indexes listed

## Optimization Guidelines

| Problem | Solution |
|---------|----------|
| SCAN on large table | Add index on WHERE columns |
| USE TEMP B-TREE for ORDER BY | Add index matching ORDER BY |
| Correlated subquery | Rewrite as JOIN |
| SELECT * with index | Select only needed columns |
| No LIMIT on large result | Add LIMIT for pagination |
| Multiple single-column indexes | Use composite index |
| Second column of composite not used | Reorder composite or add separate index |

## Compilation

```bash
cd 83_QueryPlanAnalysis
lazbuild QueryPlanAnalysis.lpi
./QueryPlanAnalysis
```

## Sample Output

```
3. Full Table Scan Analysis
   Query: SELECT * FROM orders WHERE status = 'shipped'
   Plan (no index on status):
       SCAN orders
   -> SCAN TABLE = reads every row (O(n))

   Query: SELECT * FROM orders WHERE id = 42
   Plan (PRIMARY KEY lookup):
       SEARCH orders USING INTEGER PRIMARY KEY (rowid=?)

4. Index Impact Analysis
   BEFORE adding index on orders(status):
       SCAN orders
   Avg time: 4.70 ms

   AFTER adding index on orders(status):
       SEARCH orders USING INDEX idx_orders_status (status=?)
   Avg time: 4.50 ms

5. Composite Index Analysis
   Plan (with composite index):
       SEARCH orders USING INDEX idx_status_region (status=? AND region=?)

7. JOIN Order Analysis
   Three-table JOIN:
       SEARCH o USING INDEX idx_status_region (status=? AND region=?)
       SEARCH c USING INTEGER PRIMARY KEY (rowid=?)
       SEARCH p USING INTEGER PRIMARY KEY (rowid=?)

10. Optimization Comparisons
    SELECT * vs specific columns:
      SELECT *:            4.50 ms
      SELECT id, amount:   1.40 ms
    LIMIT impact:
      Without LIMIT:  4.50 ms
      With LIMIT 10:  0.20 ms
```

## Related Examples

- **84_DatabaseStatistics** - ANALYZE and sqlite_stat tables
- **85_TablePartitioning** - Partitioning strategies
- **55_Indexes** - Basic index creation

## Performance Tips

1. **Always check plans** before deploying queries on large tables
2. **Composite indexes** - put most selective column first
3. **Covering indexes** - include SELECT columns to avoid table access
4. **LIMIT early** - reduces rows processed dramatically
5. **Avoid SELECT *** - fetch only needed columns
6. **Rewrite subqueries** as JOINs when possible
7. **Run ANALYZE** after significant data changes (see Example 84)
