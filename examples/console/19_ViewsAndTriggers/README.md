# Example 19: Views and Triggers

This example demonstrates creating and using views and triggers in SQLite.

## What you'll learn

- Creating simple and complex views
- Views with JOINs and aggregations
- BEFORE/AFTER triggers
- Audit logging with triggers
- Data validation with triggers
- Auto-timestamp triggers (created_at/updated_at)
- Listing views and triggers

## Key concepts

### Creating a simple view

```pascal
Connection.ExecuteNonQuery(
  'CREATE VIEW IF NOT EXISTS expensive_products AS ' +
  'SELECT id, name, price FROM products WHERE price > 100');

// Use the view like a table
DS := Connection.ExecuteQuery('SELECT * FROM expensive_products');
```

### View with JOIN

```pascal
Connection.ExecuteNonQuery(
  'CREATE VIEW IF NOT EXISTS product_details AS ' +
  'SELECT p.id, p.name, p.price, p.stock, c.name as category_name ' +
  'FROM products p ' +
  'LEFT JOIN categories c ON p.category = c.name');
```

### View with aggregation

```pascal
Connection.ExecuteNonQuery(
  'CREATE VIEW IF NOT EXISTS category_stats AS ' +
  'SELECT category, COUNT(*) as product_count, ' +
  '       AVG(price) as avg_price, SUM(stock) as total_stock ' +
  'FROM products GROUP BY category');
```

### Audit trigger (AFTER UPDATE)

```pascal
Connection.ExecuteNonQuery(
  'CREATE TRIGGER IF NOT EXISTS audit_price_change ' +
  'AFTER UPDATE OF price ON products ' +
  'WHEN OLD.price <> NEW.price ' +
  'BEGIN ' +
  '  INSERT INTO audit_log (table_name, operation, record_id, old_value, new_value) ' +
  '  VALUES (''products'', ''UPDATE'', NEW.id, OLD.price, NEW.price); ' +
  'END');
```

### Validation trigger (BEFORE INSERT)

```pascal
Connection.ExecuteNonQuery(
  'CREATE TRIGGER IF NOT EXISTS validate_stock ' +
  'BEFORE INSERT ON products ' +
  'BEGIN ' +
  '  SELECT CASE ' +
  '    WHEN NEW.stock < 0 THEN RAISE(ABORT, ''Stock cannot be negative'') ' +
  '    WHEN NEW.price < 0 THEN RAISE(ABORT, ''Price cannot be negative'') ' +
  '  END; ' +
  'END');
```

### Auto-timestamp triggers

```pascal
// Auto-set created_at and updated_at on insert
Connection.ExecuteNonQuery(
  'CREATE TRIGGER IF NOT EXISTS set_created_at ' +
  'AFTER INSERT ON articles ' +
  'BEGIN ' +
  '  UPDATE articles SET created_at = datetime(''now''), updated_at = datetime(''now'') WHERE id = NEW.id; ' +
  'END');

// Auto-update updated_at on any update
Connection.ExecuteNonQuery(
  'CREATE TRIGGER IF NOT EXISTS set_updated_at ' +
  'AFTER UPDATE ON articles ' +
  'WHEN OLD.updated_at = NEW.updated_at ' +
  'BEGIN ' +
  '  UPDATE articles SET updated_at = datetime(''now'') WHERE id = NEW.id; ' +
  'END');
```

### Listing views and triggers

```pascal
// List all views
DS := Connection.ExecuteQuery('SELECT name FROM sqlite_master WHERE type = ''view''');

// List all triggers
DS := Connection.ExecuteQuery('SELECT name, tbl_name FROM sqlite_master WHERE type = ''trigger''');
```

## Trigger types

| Type | When | Use Case |
|------|------|----------|
| BEFORE INSERT | Before row inserted | Validation, auto-fill |
| AFTER INSERT | After row inserted | Audit, cascading, timestamps |
| BEFORE UPDATE | Before row updated | Validation |
| AFTER UPDATE | After row updated | Audit, sync, timestamps |
| BEFORE DELETE | Before row deleted | Protection |
| AFTER DELETE | After row deleted | Cleanup, audit |

## Building

```bash
lazbuild ViewsAndTriggers.lpi
```

## Running

```bash
./ViewsAndTriggers      # Linux/macOS
ViewsAndTriggers.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
