# Example 60: Tree Structures

This example demonstrates four different patterns for storing and querying hierarchical data with NDXSQLite.

## Features Demonstrated

- **Adjacency List**: Simple parent_id reference
- **Nested Sets**: Left/right boundary encoding
- **Materialized Path**: Path string storage
- **Closure Table**: Ancestor-descendant pairs
- **Recursive CTEs**: Tree traversal queries

## Tree Structure Models

### 1. Adjacency List (Most Common)
```sql
CREATE TABLE categories_adj (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  parent_id INTEGER,
  FOREIGN KEY (parent_id) REFERENCES categories_adj(id)
);
```

**Pros**: Simple, easy updates
**Cons**: Requires recursive queries for deep trees

### 2. Nested Sets
```sql
CREATE TABLE categories_nested (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  lft INTEGER NOT NULL,   -- Left boundary
  rgt INTEGER NOT NULL    -- Right boundary
);
```

**Pros**: Fast subtree queries, no recursion needed
**Cons**: Slow inserts/deletes (must renumber)

### 3. Materialized Path
```sql
CREATE TABLE categories_path (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  path TEXT NOT NULL,     -- '/1/2/4/'
  depth INTEGER NOT NULL
);
```

**Pros**: Easy breadcrumbs, fast with LIKE queries
**Cons**: Path string maintenance

### 4. Closure Table
```sql
CREATE TABLE categories_closure_nodes (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE categories_closure (
  ancestor_id INTEGER NOT NULL,
  descendant_id INTEGER NOT NULL,
  depth INTEGER NOT NULL,
  PRIMARY KEY (ancestor_id, descendant_id)
);
```

**Pros**: Most flexible, efficient queries
**Cons**: More storage, more maintenance

## Query Examples

### Adjacency List - Get Subtree (Recursive CTE)
```sql
WITH RECURSIVE subtree AS (
  SELECT id, name, 0 as level
  FROM categories_adj WHERE id = 1
  UNION ALL
  SELECT c.id, c.name, s.level + 1
  FROM categories_adj c
  JOIN subtree s ON c.parent_id = s.id
)
SELECT * FROM subtree ORDER BY level, name;
```

### Adjacency List - Path to Root
```sql
WITH RECURSIVE ancestors AS (
  SELECT id, name, parent_id FROM categories_adj WHERE id = 4
  UNION ALL
  SELECT c.id, c.name, c.parent_id
  FROM categories_adj c
  JOIN ancestors a ON c.id = a.parent_id
)
SELECT name FROM ancestors;
```

### Nested Sets - Get All Descendants
```sql
SELECT child.id, child.name
FROM categories_nested parent, categories_nested child
WHERE parent.id = 1
  AND child.lft > parent.lft
  AND child.rgt < parent.rgt
ORDER BY child.lft;
```

### Nested Sets - Get All Ancestors
```sql
SELECT parent.id, parent.name
FROM categories_nested parent, categories_nested child
WHERE child.id = 6
  AND parent.lft < child.lft
  AND parent.rgt > child.rgt
ORDER BY parent.lft;
```

### Nested Sets - Leaf Nodes
```sql
SELECT id, name FROM categories_nested
WHERE rgt = lft + 1;  -- No children
```

### Nested Sets - Descendant Count
```sql
SELECT id, name, (rgt - lft - 1) / 2 as descendant_count
FROM categories_nested
ORDER BY descendant_count DESC;
```

### Materialized Path - Get Descendants
```sql
SELECT id, name, path, depth
FROM categories_path
WHERE path LIKE '/1/%'  -- All under node 1
ORDER BY path;
```

### Materialized Path - Breadcrumb
```sql
SELECT c.id, c.name
FROM categories_path c, categories_path target
WHERE target.id = 6
  AND target.path LIKE c.path || '%'
ORDER BY c.depth;
```

### Closure Table - Get Descendants
```sql
SELECT n.id, n.name, c.depth
FROM categories_closure c
JOIN categories_closure_nodes n ON c.descendant_id = n.id
WHERE c.ancestor_id = 1 AND c.depth > 0
ORDER BY c.depth, n.name;
```

### Closure Table - Get Ancestors
```sql
SELECT n.id, n.name, c.depth
FROM categories_closure c
JOIN categories_closure_nodes n ON c.ancestor_id = n.id
WHERE c.descendant_id = 4 AND c.depth > 0
ORDER BY c.depth DESC;
```

## Model Comparison

| Operation          | Adjacency | Nested Sets | Path    | Closure |
|--------------------|-----------|-------------|---------|---------|
| Get children       | Fast      | Medium      | Fast    | Fast    |
| Get all descendants| Slow*     | Fast        | Fast    | Fast    |
| Get ancestors      | Slow*     | Fast        | Fast    | Fast    |
| Insert node        | Fast      | Slow        | Fast    | Medium  |
| Move subtree       | Fast      | Slow        | Medium  | Medium  |
| Delete node        | Fast      | Slow        | Fast    | Medium  |
| Storage overhead   | Low       | Low         | Medium  | High    |

*Requires recursive CTE (SQLite 3.8.3+)

## Build and Run

```bash
cd 60_TreeStructures
fpc TreeStructures.lpr
./TreeStructures
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- **Adjacency List**: Best for shallow trees with frequent writes
- **Nested Sets**: Best for read-heavy, rarely-modified trees
- **Materialized Path**: Good balance, easy breadcrumbs
- **Closure Table**: Most flexible, good for deep trees
- Consider hybrid approaches for complex requirements
- Always index tree-related columns
- Use recursive CTEs for adjacency list traversal
- Validate tree integrity with constraints/triggers
