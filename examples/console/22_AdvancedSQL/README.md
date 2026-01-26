# Example 22: Advanced SQL

This example demonstrates advanced SQL features supported by SQLite.

## What you'll learn

- Common Table Expressions (CTE)
- Recursive CTEs
- Window functions
- Set operations (UNION, INTERSECT, EXCEPT)

## Key concepts

### Simple CTE

```sql
WITH high_earners AS (
  SELECT * FROM employees WHERE salary > 100000
)
SELECT * FROM high_earners;
```

### Recursive CTE (hierarchy)

```sql
WITH RECURSIVE org_chart(id, name, level) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, oc.level + 1
  FROM employees e JOIN org_chart oc ON e.manager_id = oc.id
)
SELECT * FROM org_chart;
```

### Window functions

```sql
SELECT name, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) as rank,
  AVG(salary) OVER (PARTITION BY department) as dept_avg,
  SUM(salary) OVER (ORDER BY id ROWS UNBOUNDED PRECEDING) as running_total
FROM employees;
```

### LAG/LEAD

```sql
SELECT date, amount,
  LAG(amount) OVER (ORDER BY date) as previous,
  LEAD(amount) OVER (ORDER BY date) as next
FROM sales;
```

## Window functions reference

| Function | Description |
|----------|-------------|
| ROW_NUMBER() | Unique row number |
| RANK() | Rank with gaps |
| DENSE_RANK() | Rank without gaps |
| LAG(col, n) | Value n rows before |
| LEAD(col, n) | Value n rows after |
| SUM() OVER | Running sum |
| AVG() OVER | Running average |

## Building

```bash
lazbuild AdvancedSQL.lpi
```

## Running

```bash
./AdvancedSQL      # Linux/macOS
AdvancedSQL.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
