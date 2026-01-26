# Example 96: Advanced CTEs (Common Table Expressions)

## Overview

This example demonstrates **advanced CTE patterns** in SQLite, including non-recursive CTEs for query simplification, chained CTEs, recursive CTEs for hierarchies and data generation, CTEs with INSERT/UPDATE/DELETE, multi-CTE joins, and pivot-like operations.

## Features Demonstrated

### 1. Non-Recursive CTEs
- Named subqueries for readability
- Department salary statistics
- Filtering with CTE-computed aggregates

### 2. Chained CTEs
- Multiple CTEs in one WITH clause
- Progressive data refinement (raw -> segmented -> summarized)
- Customer segmentation pipeline

### 3. Recursive CTEs
- Organization chart (employee hierarchy)
- Category tree traversal with depth
- Subtree aggregation (product counts)
- Management chain path building

### 4. Data Generation
- Number sequences (1 to N)
- Date series with day-of-week
- Fibonacci numbers
- Powers of 2

### 5. CTEs with INSERT
- Populate summary tables from computed data
- Monthly aggregation inserted in one statement

### 6. CTEs with UPDATE
- Conditional stock updates for low-inventory items
- Subquery-based conditional modifications

### 7. CTEs with DELETE
- Targeted removal of old records
- CTE-based filtering for deletion

### 8. Multiple CTE Joins
- 4 CTEs joined in a single query
- Manager info, team size, salary rank, department budget

### 9. Recursive Path Finding
- Full management chains from leaf to root
- Path concatenation with depth tracking

### 10. Pivot Simulation
- Cross-tab reporting with CTE + CASE
- Monthly order counts per customer

## Database Schema

```
+------------------+     +------------------+     +------------------+
| employees        |     | categories       |     | products         |
+------------------+     +------------------+     +------------------+
| id (PK)          |     | id (PK)          |     | id (PK)          |
| name             |     | name             |     | name             |
| manager_id (FK)  |     | parent_id (self) |     | category_id (FK) |
| department       |     +------------------+     | price            |
| salary           |                               | stock            |
| hire_date        |     +------------------+     +------------------+
+------------------+     | orders           |
                         +------------------+     +------------------+
                         | id (PK)          |     | monthly_summary  |
                         | customer_id      |     +------------------+
                         | order_date       |     | month            |
                         | total            |     | total_orders     |
                         | status           |     | total_revenue    |
                         +------------------+     | avg_order        |
                                                  +------------------+
```

## CTE Syntax

### Basic CTE
```sql
WITH cte_name AS (
  SELECT ... FROM ...
)
SELECT ... FROM cte_name;
```

### Chained CTEs
```sql
WITH
  cte1 AS (SELECT ...),
  cte2 AS (SELECT ... FROM cte1),
  cte3 AS (SELECT ... FROM cte2)
SELECT ... FROM cte3;
```

### Recursive CTE
```sql
WITH RECURSIVE tree AS (
  -- Base case (anchor)
  SELECT ... FROM table WHERE parent IS NULL
  UNION ALL
  -- Recursive case
  SELECT ... FROM table JOIN tree ON ...
)
SELECT ... FROM tree;
```

## Key Patterns

### Hierarchy Traversal
```pascal
DS := Conn.ExecuteQuery(
  'WITH RECURSIVE org_chart AS (' +
  '  SELECT id, name, manager_id, 0 as level ' +
  '  FROM employees WHERE manager_id IS NULL ' +
  '  UNION ALL ' +
  '  SELECT e.id, e.name, e.manager_id, oc.level + 1 ' +
  '  FROM employees e JOIN org_chart oc ON e.manager_id = oc.id' +
  ') SELECT * FROM org_chart ORDER BY level');
```

### Data Generation
```pascal
DS := Conn.ExecuteQuery(
  'WITH RECURSIVE dates AS (' +
  '  SELECT date(''2024-01-01'') as d ' +
  '  UNION ALL ' +
  '  SELECT date(d, ''+1 day'') FROM dates WHERE d < ''2024-01-31''' +
  ') SELECT d FROM dates');
```

### CTE with INSERT
```pascal
Conn.ExecuteNonQuery(
  'INSERT INTO monthly_summary (month, total_orders, total_revenue, avg_order) ' +
  'WITH monthly AS (' +
  '  SELECT strftime(''%Y-%m'', order_date) as month,' +
  '    COUNT(*) as total_orders, SUM(total) as total_revenue, AVG(total) as avg_order ' +
  '  FROM orders GROUP BY strftime(''%Y-%m'', order_date)' +
  ') SELECT * FROM monthly');
```

### Customer Segmentation Pipeline
```pascal
DS := Conn.ExecuteQuery(
  'WITH ' +
  '  customer_orders AS (SELECT ... GROUP BY customer_id),' +
  '  customer_segments AS (SELECT ..., CASE WHEN ... END as segment FROM customer_orders),' +
  '  segment_summary AS (SELECT segment, COUNT(*), AVG(total) FROM customer_segments GROUP BY segment)' +
  'SELECT * FROM segment_summary');
```

## Compilation

```bash
cd 96_CTEAdvanced
lazbuild CTEAdvanced.lpi
./CTEAdvanced
```

## Sample Output

```
3. Recursive CTEs - Hierarchy Traversal
   CEO Alice (Executive, $150000)
      VP Bob (Engineering, $120000)
         Dir Dave (Engineering, $100000)
            Mgr Frank (Engineering, $85000)
               Dev Henry (Engineering, $75000)

4. Data Generation with Recursive CTEs
   Fibonacci sequence: 0 1 1 2 3 5 8 13 21 34 55 89
   Powers of 2: 2^0=1 2^1=2 ... 2^10=1024

8. Multiple CTEs with Joins
   VP Bob    Engineering  $120000  #1  CEO Alice   2  $620000
   Dir Dave  Engineering  $100000  #2  VP Bob      1  $620000

9. Management Chain
   [depth 4] CEO Alice > VP Bob > Dir Dave > Mgr Frank > Dev Henry
```

## Related Examples

- **95_WindowFunctions** - Window functions (often combined with CTEs)
- **97_ReturningClause** - RETURNING clause
- **93_StateMachine** - Hierarchy patterns

## Best Practices

1. **Name CTEs clearly**: Use descriptive names that explain what the CTE computes
2. **Chain progressively**: Each CTE should build on the previous one logically
3. **Recursive base case**: Always start with a clear anchor (WHERE parent IS NULL)
4. **Termination condition**: Recursive CTEs must have a WHERE clause to stop recursion
5. **Path building**: Use string concatenation for readable hierarchy paths
6. **CTE with DML**: INSERT/UPDATE/DELETE can use CTEs for complex filtering
7. **Avoid deep recursion**: SQLite defaults to 1000 recursion depth limit
8. **Combine with window functions**: CTEs pre-aggregate, windows rank/compare
