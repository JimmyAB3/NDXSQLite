# Example 37: Recursive Queries with CTEs

This example demonstrates Common Table Expressions (CTEs) and recursive queries in SQLite.

## What you'll learn

- Basic CTEs (WITH clause)
- Recursive CTEs (WITH RECURSIVE)
- Tree traversal (hierarchies, org charts)
- Graph traversal (paths, routes)
- Bill of Materials explosion
- Date/number series generation

## Key concepts

### Basic CTE

```sql
WITH summary AS (
  SELECT category, SUM(amount) AS total
  FROM sales
  GROUP BY category
)
SELECT * FROM summary WHERE total > 1000;
```

### Recursive CTE structure

```sql
WITH RECURSIVE cte_name(columns) AS (
  -- Base case (anchor)
  SELECT initial_values

  UNION ALL

  -- Recursive case
  SELECT derived_values
  FROM table
  JOIN cte_name ON condition
  WHERE termination_condition
)
SELECT * FROM cte_name;
```

### Employee hierarchy

```sql
WITH RECURSIVE org_chart(id, name, level, path) AS (
  -- Start from root (CEO)
  SELECT id, name, 0, name
  FROM employees
  WHERE manager_id IS NULL

  UNION ALL

  -- Add subordinates
  SELECT e.id, e.name, oc.level + 1, oc.path || ' > ' || e.name
  FROM employees e
  JOIN org_chart oc ON e.manager_id = oc.id
)
SELECT * FROM org_chart ORDER BY path;
```

### Finding ancestors (bottom-up)

```sql
WITH RECURSIVE ancestors(id, name, parent_id, level) AS (
  -- Start from leaf node
  SELECT id, name, parent_id, 0
  FROM categories WHERE id = 8

  UNION ALL

  -- Walk up to parents
  SELECT c.id, c.name, c.parent_id, a.level + 1
  FROM categories c
  JOIN ancestors a ON c.id = a.parent_id
)
SELECT * FROM ancestors;
```

### Finding descendants (top-down)

```sql
WITH RECURSIVE descendants(id, name, level) AS (
  -- Start from root node
  SELECT id, name, 0
  FROM categories WHERE id = 1

  UNION ALL

  -- Walk down to children
  SELECT c.id, c.name, d.level + 1
  FROM categories c
  JOIN descendants d ON c.parent_id = d.id
)
SELECT * FROM descendants;
```

### Graph paths with cycle detection

```sql
WITH RECURSIVE paths(current, path, visited) AS (
  SELECT 'A', 'A', '|A|'

  UNION ALL

  SELECT e.to_node,
         p.path || ' -> ' || e.to_node,
         p.visited || e.to_node || '|'
  FROM edges e
  JOIN paths p ON e.from_node = p.current
  WHERE p.visited NOT LIKE '%|' || e.to_node || '|%'  -- Avoid cycles
)
SELECT * FROM paths WHERE current = 'Z';
```

### Generating number series

```sql
WITH RECURSIVE numbers(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM numbers WHERE n < 100
)
SELECT n FROM numbers;
```

### Generating date series

```sql
WITH RECURSIVE dates(d) AS (
  SELECT date('2024-01-01')
  UNION ALL
  SELECT date(d, '+1 day') FROM dates WHERE d < date('2024-01-31')
)
SELECT d FROM dates;
```

## Common patterns

| Pattern | Use Case |
|---------|----------|
| Tree traversal | Org charts, category trees, file systems |
| Ancestor lookup | Breadcrumbs, permission inheritance |
| Descendant lookup | Subtree operations, cascading |
| Graph paths | Route finding, dependencies |
| Bill of Materials | Product composition, quantity calculation |
| Series generation | Filling gaps, date ranges |

## Performance tips

1. **Always have a termination condition** - prevent infinite recursion
2. **Limit recursion depth** - use WHERE clause or LIMIT
3. **Index join columns** - especially parent_id, foreign keys
4. **Use UNION (not UNION ALL)** when duplicates possible in graphs
5. **Track visited nodes** for graph traversal to prevent cycles

## Building

```bash
lazbuild RecursiveQueries.lpi
```

## Running

```bash
./RecursiveQueries      # Linux/macOS
RecursiveQueries.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
