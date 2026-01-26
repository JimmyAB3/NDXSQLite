# Example 97: RETURNING Clause

## Overview

This example demonstrates the **RETURNING clause** in SQLite (available since 3.35.0), which allows INSERT, UPDATE, and DELETE statements to return the affected rows. This eliminates the need for separate SELECT queries to retrieve auto-generated IDs, verify modifications, or capture data before deletion.

## Features Demonstrated

### 1. INSERT ... RETURNING
- Get auto-generated IDs immediately after insert
- Retrieve default values assigned by the database
- Return specific columns or all columns (*)

### 2. RETURNING with Expressions
- Computed values (price with tax, inventory value)
- Function calls (UPPER, ROUND)
- Arithmetic expressions on returned data

### 3. UPDATE ... RETURNING
- See modified rows after bulk updates
- Compare old/new values (price adjustments)
- Verify which rows were actually affected

### 4. DELETE ... RETURNING
- Capture deleted data for logging
- Archive records before removal
- Count and identify removed rows

### 5. Audit Trail Pattern
- INSERT + RETURNING for audit logging
- Track changes with record IDs
- Build complete modification history

### 6. Stock Management
- Atomic stock deduction with verification
- Insufficient stock detection (empty result = rejected)
- Order creation with returned product IDs

### 7. RETURNING *
- Return all columns including server-side defaults
- Useful for seeing computed columns (timestamps, etc.)

### 8. Bulk Operations
- Mass status updates with affected row reporting
- Batch reactivation with new values

### 9. Delete-Archive Pattern
- Delete from source table
- Capture all fields via RETURNING
- Insert into archive table

### 10. Conditional Expressions in RETURNING
- CASE expressions for classification
- Tier assignment based on computed scores
- Stock status categorization

## Database Schema

```
+------------------+     +------------------+     +------------------+
| users            |     | products         |     | orders           |
+------------------+     +------------------+     +------------------+
| id (PK, AUTO)   |     | id (PK, AUTO)    |     | id (PK, AUTO)    |
| username         |     | name             |     | user_id          |
| email            |     | category         |     | product_id       |
| role (def:user)  |     | price            |     | quantity         |
| status (def:act) |     | stock (def:0)    |     | unit_price       |
| score (def:0)    |     | is_active (def:1)|     | total            |
| created_at       |     | updated_at       |     | status           |
+------------------+     +------------------+     | created_at       |
                                                  +------------------+
+------------------+     +------------------+
| audit_log        |     | users_archive    |
+------------------+     +------------------+
| id (PK, AUTO)   |     | id               |
| table_name       |     | username         |
| operation        |     | email            |
| record_id        |     | role             |
| details          |     | archived_at      |
| created_at       |     +------------------+
+------------------+
```

## RETURNING Syntax

```sql
-- With INSERT
INSERT INTO table (col1, col2) VALUES (val1, val2)
RETURNING id, col1, col2, expression AS alias;

-- With UPDATE
UPDATE table SET col1 = new_val WHERE condition
RETURNING id, col1, old_expression, new_expression;

-- With DELETE
DELETE FROM table WHERE condition
RETURNING *;
```

## Key Patterns

### Get Auto-Generated ID
```pascal
DS := Conn.ExecuteQuery(
  'INSERT INTO users (username, email) VALUES (''alice'', ''a@b.com'') ' +
  'RETURNING id, username');
// DS.FieldByName('id').AsInteger contains the new ID
```

### Verify Update Affected Rows
```pascal
DS := Conn.ExecuteQuery(
  'UPDATE products SET stock = stock - 5 WHERE id = 1 AND stock >= 5 ' +
  'RETURNING id, stock');
if DS.EOF then
  WriteLn('Insufficient stock!')  // No rows matched
else
  WriteLn('New stock: ', DS.FieldByName('stock').AsInteger);
```

### Archive Before Delete
```pascal
DS := Conn.ExecuteQuery(
  'DELETE FROM users WHERE status = ''inactive'' ' +
  'RETURNING id, username, email');
while not DS.EOF do begin
  Conn.ExecuteNonQuery('INSERT INTO archive ...', [DS values]);
  DS.Next;
end;
```

### Computed Return Values
```pascal
DS := Conn.ExecuteQuery(
  'INSERT INTO products (name, price, stock) VALUES (''Widget'', 29.99, 100) ' +
  'RETURNING id, name, price * stock as inventory_value, ROUND(price * 1.2, 2) as with_tax');
```

### Conditional Classification
```pascal
DS := Conn.ExecuteQuery(
  'UPDATE users SET score = score + 10 ' +
  'RETURNING username, score, ' +
  '  CASE WHEN score >= 100 THEN ''Gold'' ELSE ''Silver'' END as tier');
```

## Compilation

```bash
cd 97_ReturningClause
lazbuild ReturningClause.lpi
./ReturningClause
```

## Sample Output

```
SQLite version: 3.45.1

1. INSERT ... RETURNING - Get Generated IDs
   -> Inserted: id=1, user=alice, email=alice@example.com, role=admin
   -> Inserted: id=2, user=bob, role=user

3. UPDATE ... RETURNING - See Modified Rows
   Applying 10% price increase to Electronics:
   -> id=1 Laptop Pro: was $1299.99 -> now $1429.99
   -> id=4 Headphones: was $89.99 -> now $98.99

6. RETURNING for Stock Management
   -> Order for Laptop Pro (qty: 2), Remaining stock: 23
   -> Order REJECTED: insufficient stock (no rows returned)

10. RETURNING with Conditional Expressions
   alice    175   Gold
   bob      105   Silver
   diana     70   Bronze
```

## Related Examples

- **95_WindowFunctions** - Advanced SELECT queries
- **96_CTEAdvanced** - CTEs with INSERT/UPDATE
- **94_CQRS** - Event sourcing patterns

## Best Practices

1. **Use ExecuteQuery**: RETURNING makes DML return rows, so use ExecuteQuery instead of ExecuteNonQuery
2. **Check EOF for validation**: Empty result means no rows matched (e.g., insufficient stock)
3. **Return only needed columns**: RETURNING specific columns is more efficient than RETURNING *
4. **Atomic operations**: RETURNING avoids race conditions between INSERT and subsequent SELECT
5. **Audit patterns**: Combine RETURNING with audit log inserts for change tracking
6. **Archive before delete**: Use DELETE...RETURNING to capture data before removal
7. **Version check**: RETURNING requires SQLite 3.35.0+ (2021-03-12)
8. **Expression support**: Use ROUND, UPPER, CASE, arithmetic in RETURNING for computed values
