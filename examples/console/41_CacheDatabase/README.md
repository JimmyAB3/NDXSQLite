# Example 41: Cache Database with TTL

This example demonstrates using SQLite as a cache store with time-to-live (TTL) expiration.

## What you'll learn

- Key-value caching with expiration
- TTL-based automatic expiration
- LRU (Least Recently Used) eviction
- Cache namespaces
- Hit/miss statistics
- Cleanup strategies

## Key concepts

### Schema design

```sql
CREATE TABLE cache (
  key TEXT PRIMARY KEY,
  value TEXT,
  namespace TEXT DEFAULT 'default',
  created_at TEXT DEFAULT (datetime('now')),
  expires_at TEXT,                    -- NULL = never expires
  last_accessed TEXT DEFAULT (datetime('now')),
  access_count INTEGER DEFAULT 0,
  size_bytes INTEGER DEFAULT 0
);

CREATE INDEX idx_cache_expires ON cache(expires_at);
CREATE INDEX idx_cache_namespace ON cache(namespace);
CREATE INDEX idx_cache_lru ON cache(last_accessed);
```

### Basic operations

```pascal
// Set with TTL (seconds)
CacheSet('user:1:name', 'Alice', 3600);  // Expires in 1 hour

// Get (returns empty if expired or missing)
Value := CacheGet('user:1:name');

// Check existence
if CacheExists('user:1:name') then ...

// Delete
CacheDelete('user:1:name');

// Clear namespace
CacheClear('session');
```

### TTL expiration

```sql
-- Set with expiration
INSERT INTO cache (key, value, expires_at)
VALUES ('temp', 'data', datetime('now', '+3600 seconds'));

-- Get only non-expired
SELECT value FROM cache
WHERE key = ?
  AND (expires_at IS NULL OR expires_at > datetime('now'));
```

### Cleanup expired entries

```sql
DELETE FROM cache
WHERE expires_at IS NOT NULL
  AND expires_at <= datetime('now');
```

### LRU eviction

```sql
-- Delete oldest accessed entries when over limit
DELETE FROM cache WHERE key IN (
  SELECT key FROM cache
  ORDER BY last_accessed ASC
  LIMIT ?
);
```

### Namespaces

```pascal
// Different namespaces can have same keys
CacheSet('token', 'abc123', 1800, 'session');
CacheSet('token', 'xyz789', 300, 'api');

// Clear only one namespace
CacheClear('session');
```

### Statistics tracking

```sql
CREATE TABLE cache_stats (
  namespace TEXT PRIMARY KEY,
  hits INTEGER DEFAULT 0,
  misses INTEGER DEFAULT 0,
  sets INTEGER DEFAULT 0,
  evictions INTEGER DEFAULT 0
);

-- Calculate hit rate
SELECT namespace,
       ROUND(100.0 * hits / NULLIF(hits + misses, 0), 1) as hit_rate
FROM cache_stats;
```

## Cache strategies

| Strategy | Description | Use Case |
|----------|-------------|----------|
| TTL | Expire after fixed time | Session data |
| LRU | Remove least recently used | Memory-constrained |
| LFU | Remove least frequently used | Hot data optimization |
| Manual | Explicit invalidation | On data change |

## Best practices

1. **Set appropriate TTL**: Balance freshness vs. performance
2. **Clean regularly**: Schedule periodic cleanup of expired entries
3. **Monitor hit rate**: Low hit rate means cache is ineffective
4. **Size limits**: Prevent unbounded cache growth
5. **Use namespaces**: Organize cache by function or tenant

## Advantages over in-memory cache

- **Persistence**: Survives application restart
- **Sharing**: Multiple processes can share cache
- **Size**: Can be larger than available RAM (with memory-mapped I/O)
- **Queryable**: SQL for analysis and debugging

## Building

```bash
lazbuild CacheDatabase.lpi
```

## Running

```bash
./CacheDatabase      # Linux/macOS
CacheDatabase.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
