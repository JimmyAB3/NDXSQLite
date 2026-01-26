# Example 53: Soft Deletes

This example demonstrates logical deletion (soft delete) instead of physical deletion, allowing data recovery and audit trails with NDXSQLite.

## Features Demonstrated

- **Soft Delete**: Mark records as deleted without removing them
- **Restore (Undelete)**: Recover deleted records
- **Cascade Soft Delete**: Delete related records together
- **Trash Management**: View and manage deleted items
- **Auto-Purge**: Permanently delete old trash items
- **Active Record Views**: Simplified queries for active data
- **Trigger-Based Soft Delete**: Transparent deletion

## How It Works

Instead of `DELETE FROM table`, soft delete uses:
```sql
UPDATE table SET deleted_at = datetime('now'), deleted_by = 'user' WHERE id = ?
```

## Database Schema

```sql
CREATE TABLE products (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  price REAL NOT NULL,
  deleted_at TEXT DEFAULT NULL,   -- NULL = active, timestamp = deleted
  deleted_by TEXT DEFAULT NULL    -- Who deleted it
);

-- View for active records only
CREATE VIEW active_products AS
SELECT * FROM products WHERE deleted_at IS NULL;

-- View for trash
CREATE VIEW trash_items AS
SELECT 'product' as type, id, name, deleted_at, deleted_by
FROM products WHERE deleted_at IS NOT NULL;
```

## Key Operations

### Soft Delete
```pascal
procedure SoftDeleteProduct(AProductId: Integer; ADeletedBy: string);
begin
  Connection.ExecuteNonQuery(
    'UPDATE products SET deleted_at = datetime(''now''), deleted_by = ? ' +
    'WHERE id = ? AND deleted_at IS NULL',
    [ADeletedBy, AProductId]);
end;
```

### Restore
```pascal
procedure RestoreProduct(AProductId: Integer);
begin
  Connection.ExecuteNonQuery(
    'UPDATE products SET deleted_at = NULL, deleted_by = NULL WHERE id = ?',
    [AProductId]);
end;
```

### Cascade Soft Delete
```pascal
procedure SoftDeleteCategory(ACategoryId: Integer; ADeletedBy: string);
begin
  Connection.BeginTransaction;
  try
    // Soft delete category
    Connection.ExecuteNonQuery(
      'UPDATE categories SET deleted_at = datetime(''now''), deleted_by = ? WHERE id = ?',
      [ADeletedBy, ACategoryId]);

    // Cascade to products
    Connection.ExecuteNonQuery(
      'UPDATE products SET deleted_at = datetime(''now''), deleted_by = ? ' +
      'WHERE category_id = ? AND deleted_at IS NULL',
      [ADeletedBy + ' (cascade)', ACategoryId]);

    Connection.Commit;
  except
    Connection.Rollback;
  end;
end;
```

### Query Active Records
```sql
-- Using view (recommended)
SELECT * FROM active_products;

-- Direct query
SELECT * FROM products WHERE deleted_at IS NULL;
```

### Auto-Purge Old Trash
```sql
-- Permanently delete items in trash for > 30 days
DELETE FROM products
WHERE deleted_at IS NOT NULL
  AND deleted_at < datetime('now', '-30 days');
```

### Trigger-Based Soft Delete
```sql
CREATE TRIGGER soft_delete_document
BEFORE DELETE ON documents
WHEN OLD.deleted_at IS NULL
BEGIN
  UPDATE documents SET deleted_at = datetime('now') WHERE id = OLD.id;
  SELECT RAISE(IGNORE);  -- Prevent actual deletion
END;
```

## Query Patterns

```sql
-- Count active vs deleted
SELECT
  SUM(CASE WHEN deleted_at IS NULL THEN 1 ELSE 0 END) as active,
  SUM(CASE WHEN deleted_at IS NOT NULL THEN 1 ELSE 0 END) as deleted
FROM products;

-- Recently deleted (last 24 hours)
SELECT * FROM trash_items
WHERE deleted_at >= datetime('now', '-1 day')
ORDER BY deleted_at DESC;
```

## Build and Run

```bash
cd 53_SoftDeletes
fpc SoftDeletes.lpr
./SoftDeletes
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use `deleted_at` (timestamp) instead of `is_deleted` (boolean)
- Track who deleted: `deleted_by` column
- Create views for active records (cleaner queries)
- Consider cascade behavior for related records
- Implement auto-purge for old deleted records
- Index `deleted_at` column for performance
