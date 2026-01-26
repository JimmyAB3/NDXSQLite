# Example 32: Pagination

This example demonstrates efficient pagination patterns in SQLite.

## What you'll learn

- LIMIT/OFFSET pagination (simple but slow)
- Cursor-based (keyset) pagination (fast)
- Performance comparison
- Pagination with custom sorting
- Getting total count efficiently
- Infinite scroll pattern

## Key concepts

### LIMIT/OFFSET pagination

```pascal
// Simple but gets slower with higher offsets
PageNum := 5;
PageSize := 20;
DS := Connection.ExecuteQuery(
  Format('SELECT * FROM posts ORDER BY id LIMIT %d OFFSET %d',
    [PageSize, (PageNum - 1) * PageSize]));
```

**Problem**: SQLite must scan and discard OFFSET rows, so page 5000 is much slower than page 1.

### Cursor-based pagination (recommended)

```pascal
// First page
DS := Connection.ExecuteQuery(
  'SELECT id, title FROM posts ORDER BY id LIMIT 20');

// Store the last id from results
LastId := 20;

// Next page - use WHERE instead of OFFSET
DS := Connection.ExecuteQuery(
  Format('SELECT id, title FROM posts WHERE id > %d ORDER BY id LIMIT 20',
    [LastId]));
```

**Advantage**: Uses index, O(1) performance regardless of page depth.

### Cursor pagination with custom sort

```pascal
// Sort by views DESC with id as tiebreaker
// First page
DS := Connection.ExecuteQuery(
  'SELECT id, views FROM posts ORDER BY views DESC, id LIMIT 20');

// Remember last row's values
LastViews := 9500;
LastId := 42;

// Next page - composite cursor condition
DS := Connection.ExecuteQuery(
  Format('SELECT id, views FROM posts ' +
         'WHERE (views < %d) OR (views = %d AND id > %d) ' +
         'ORDER BY views DESC, id LIMIT 20',
    [LastViews, LastViews, LastId]));
```

### Total count

```pascal
// Method 1: Separate query
TotalRows := Connection.ExecuteScalar('SELECT COUNT(*) FROM posts');
TotalPages := (TotalRows + PageSize - 1) div PageSize;

// Method 2: Window function (single query, SQLite 3.25+)
DS := Connection.ExecuteQuery(
  'SELECT id, title, COUNT(*) OVER() as total ' +
  'FROM posts ORDER BY id LIMIT 20');
```

### Pagination with filters

```pascal
// Apply filter in both WHERE clauses
DS := Connection.ExecuteQuery(
  'SELECT id, title FROM posts ' +
  'WHERE author = ? AND id > ? ' +
  'ORDER BY id LIMIT 20',
  [AuthorFilter, LastId]);
```

### Infinite scroll pattern

```pascal
// Client sends: lastId (0 for first request)
// Server returns: items + lastId for next request

if LastId = 0 then
  DS := Connection.ExecuteQuery(
    'SELECT id, title FROM posts ORDER BY id LIMIT 20')
else
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title FROM posts WHERE id > %d ORDER BY id LIMIT 20',
      [LastId]));

// Return items and new LastId to client
```

## Performance comparison

| Method | Page 1 | Page 1000 | Page 5000 |
|--------|--------|-----------|-----------|
| LIMIT/OFFSET | 1ms | 50ms | 200ms+ |
| Cursor-based | 1ms | 1ms | 1ms |

## When to use each method

| Method | Use Case |
|--------|----------|
| LIMIT/OFFSET | Small datasets, random page access needed |
| Cursor-based | Large datasets, sequential navigation |
| Window COUNT | Need total + page data in one query |

## Best practices

1. **Use cursor pagination for large datasets**
2. **Create indexes on sort columns**
3. **Include tiebreaker column** (usually id) for consistent ordering
4. **Cache total count** if it doesn't change often
5. **Consider approximate counts** for very large tables

## Building

```bash
lazbuild Pagination.lpi
```

## Running

```bash
./Pagination      # Linux/macOS
Pagination.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
