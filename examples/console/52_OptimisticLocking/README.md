# Example 52: Optimistic Locking

This example demonstrates version-based concurrency control to handle concurrent updates safely with NDXSQLite.

## Features Demonstrated

- **Version-Based Locking**: Detect concurrent modifications
- **Conflict Detection**: Identify when data has changed
- **Retry Strategies**: Handle conflicts gracefully
- **Multi-Table Transactions**: Versioned transfers
- **Version History**: Track all changes via triggers
- **Best Practices**: Patterns for optimistic locking

## How It Works

Optimistic locking assumes conflicts are rare. Instead of locking rows during read, it:
1. Reads data with current version number
2. Makes modifications locally
3. Updates only if version hasn't changed
4. Detects conflict if version mismatch

## Database Schema

```sql
CREATE TABLE products (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  price REAL NOT NULL,
  stock INTEGER NOT NULL DEFAULT 0,
  version INTEGER NOT NULL DEFAULT 1,  -- Version column
  updated_at TEXT DEFAULT CURRENT_TIMESTAMP
);
```

## Key Operations

### Read-Modify-Write Pattern
```pascal
// 1. Read with version
DS := Connection.ExecuteQuery('SELECT id, price, version FROM products WHERE id = ?', [ProductId]);
CurrentVersion := DS.FieldByName('version').AsInteger;

// 2. Update with version check
RowsAffected := Connection.ExecuteNonQuery(
  'UPDATE products SET price = ?, version = version + 1 ' +
  'WHERE id = ? AND version = ?',
  [NewPrice, ProductId, CurrentVersion]);

// 3. Check for conflict
if RowsAffected = 0 then
  // CONFLICT: Another process modified the row
else
  // SUCCESS: Update applied
```

### Simulated Concurrent Updates
```pascal
// User A and User B read same version
VersionA := GetVersion(ProductId);
VersionB := GetVersion(ProductId);  // Same as VersionA

// User A updates first - succeeds
UpdateWithVersion(ProductId, NewPriceA, VersionA);  // OK

// User B tries with stale version - fails
UpdateWithVersion(ProductId, NewPriceB, VersionB);  // CONFLICT!
```

### Retry Strategy
```pascal
for Attempt := 1 to MaxRetries do
begin
  // Re-read current state
  CurrentVersion := GetCurrentVersion(ProductId);

  // Try update
  if UpdateWithVersion(ProductId, NewValue, CurrentVersion) then
  begin
    WriteLn('SUCCESS');
    Break;
  end
  else
  begin
    WriteLn('CONFLICT: Retrying...');
    Sleep(10);  // Brief delay before retry
  end;
end;
```

### Versioned Money Transfer
```pascal
Connection.BeginTransaction;
try
  // Debit source with version check
  if ExecuteNonQuery('UPDATE accounts SET balance = balance - ?, version = version + 1 ' +
     'WHERE id = ? AND version = ? AND balance >= ?',
     [Amount, FromId, FromVersion, Amount]) = 0 then
  begin
    Connection.Rollback;
    Exit;  // Conflict or insufficient funds
  end;

  // Credit destination with version check
  if ExecuteNonQuery('UPDATE accounts SET balance = balance + ?, version = version + 1 ' +
     'WHERE id = ? AND version = ?',
     [Amount, ToId, ToVersion]) = 0 then
  begin
    Connection.Rollback;
    Exit;  // Conflict
  end;

  Connection.Commit;
except
  Connection.Rollback;
end;
```

### Version History Trigger
```sql
CREATE TRIGGER log_product_changes
AFTER UPDATE ON products
WHEN OLD.version <> NEW.version
BEGIN
  INSERT INTO product_history (product_id, old_price, new_price, old_version, new_version)
  VALUES (OLD.id, OLD.price, NEW.price, OLD.version, NEW.version);
END;
```

## Build and Run

```bash
cd 52_OptimisticLocking
fpc OptimisticLocking.lpr
./OptimisticLocking
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Always include version column in tables with concurrent updates
- Check affected rows count after UPDATE
- Implement retry logic for transient conflicts
- Use transactions for multi-table updates
- Keep read-modify-write cycles short
- Log conflicts for monitoring
