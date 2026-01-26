# Example 30: Upsert Operations

This example demonstrates upsert (insert or update) patterns in SQLite.

## What you'll learn

- INSERT OR REPLACE
- INSERT OR IGNORE
- ON CONFLICT DO UPDATE (true upsert)
- ON CONFLICT DO NOTHING
- Conflict targets for composite keys
- The EXCLUDED table reference
- Conditional updates with WHERE
- RETURNING clause

## Key concepts

### INSERT OR REPLACE

```pascal
// Deletes existing row and inserts new one
Connection.ExecuteNonQuery(
  'INSERT OR REPLACE INTO products (id, name, price) ' +
  'VALUES (1, ''Updated Product'', 99.99)');

// WARNING: This DELETES then INSERTS
// - ON DELETE triggers fire
// - Unspecified columns get defaults, not old values
```

### INSERT OR IGNORE

```pascal
// Silently ignores duplicate key errors
Connection.ExecuteNonQuery(
  'INSERT OR IGNORE INTO users (email, name) ' +
  'VALUES (''existing@test.com'', ''Name'')');
// No error raised, no row inserted if email exists
```

### ON CONFLICT DO UPDATE (recommended)

```pascal
// True upsert - updates if exists, inserts if not
Connection.ExecuteNonQuery(
  'INSERT INTO inventory (sku, name, quantity) ' +
  'VALUES (''SKU001'', ''Widget'', 25) ' +
  'ON CONFLICT(sku) DO UPDATE SET ' +
  '  quantity = quantity + excluded.quantity');

// excluded.column = the value that would have been inserted
```

### ON CONFLICT DO NOTHING

```pascal
// Same as INSERT OR IGNORE but more explicit
Connection.ExecuteNonQuery(
  'INSERT INTO settings (key, value) VALUES (''theme'', ''dark'') ' +
  'ON CONFLICT DO NOTHING');
```

### Composite key conflict

```pascal
// Handle conflict on multiple columns
Connection.ExecuteNonQuery(
  'INSERT INTO order_items (order_id, product_id, quantity) ' +
  'VALUES (100, 1, 5) ' +
  'ON CONFLICT(order_id, product_id) DO UPDATE SET ' +
  '  quantity = quantity + excluded.quantity');
```

### Conditional update

```pascal
// Only update if condition is met
Connection.ExecuteNonQuery(
  'INSERT INTO prices (id, price) VALUES (1, 50.00) ' +
  'ON CONFLICT(id) DO UPDATE SET price = excluded.price ' +
  'WHERE excluded.price >= prices.min_price');
```

### RETURNING clause

```pascal
// Get the inserted/updated row back
DS := Connection.ExecuteQuery(
  'INSERT INTO tokens (user_id, token) VALUES (1, ''abc'') ' +
  'ON CONFLICT(user_id) DO UPDATE SET token = excluded.token ' +
  'RETURNING user_id, token');
```

## Comparison

| Method | On Conflict | Behavior |
|--------|-------------|----------|
| `INSERT OR REPLACE` | Delete + Insert | Triggers fire, defaults applied |
| `INSERT OR IGNORE` | Skip silently | No error, no insert |
| `ON CONFLICT DO UPDATE` | Update in place | Preserves row, updates columns |
| `ON CONFLICT DO NOTHING` | Skip silently | Same as OR IGNORE |

## The EXCLUDED table

In ON CONFLICT DO UPDATE, `excluded` refers to the values that would have been inserted:

```sql
INSERT INTO t (id, value) VALUES (1, 'new')
ON CONFLICT(id) DO UPDATE SET
  value = excluded.value;  -- 'new'

-- You can also reference the existing row:
ON CONFLICT(id) DO UPDATE SET
  value = t.value || excluded.value;  -- concatenate old + new
```

## Best practices

1. **Prefer ON CONFLICT DO UPDATE** over INSERT OR REPLACE
2. **Use specific conflict targets** for clarity
3. **Use RETURNING** to get the final row state
4. **Wrap bulk upserts in transactions** for performance

## Building

```bash
lazbuild Upsert.lpi
```

## Running

```bash
./Upsert      # Linux/macOS
Upsert.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
