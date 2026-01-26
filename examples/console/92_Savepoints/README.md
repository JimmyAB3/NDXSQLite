# Example 92: Savepoints (Nested Transactions)

## Overview

This example demonstrates **nested transactions using SAVEPOINTs** in SQLite. It covers basic savepoint creation and release, partial rollbacks, multi-level nesting, order processing with stock validation, batch processing with error isolation, audit trail preservation, full transaction rollbacks, and the retry pattern.

## Features Demonstrated

### 1. Basic SAVEPOINT / RELEASE
- Create named savepoints within a transaction
- RELEASE to commit the nested work
- Outer COMMIT finalizes everything

### 2. ROLLBACK TO SAVEPOINT
- Undo specific operations while keeping others
- Process multiple transfers, roll back only failed ones
- RELEASE after ROLLBACK TO removes the savepoint

### 3. Nested SAVEPOINTs (Multi-Level)
- 3 levels of savepoint nesting
- Rollback innermost level while keeping outer levels
- Demonstrates scope hierarchy

### 4. Order Processing
- Stock validation before confirming orders
- Rollback individual orders on insufficient stock
- Other orders in same transaction remain unaffected

### 5. Batch with Error Isolation
- Process N operations in a single transaction
- Failed operations are individually rolled back
- Successful operations are preserved

### 6. Guaranteed Audit Trail
- Audit entries written outside savepoint scope
- Risky operation in savepoint can be rolled back
- Audit trail survives regardless of operation outcome

### 7. Full Transaction Rollback
- ROLLBACK discards all savepoints (even released ones)
- Demonstrates that RELEASE doesn't truly commit to disk

### 8. Retry Pattern
- Reusable savepoint for retry attempts
- ROLLBACK TO preserves the savepoint for reuse
- RELEASE only on final success

## SQLite Savepoint Rules

```
BEGIN TRANSACTION
  |
  +-- SAVEPOINT name         (creates named point)
  |     |
  |     +-- ... operations ...
  |     |
  |     +-- RELEASE SAVEPOINT name       (success: merge into parent)
  |     |       OR
  |     +-- ROLLBACK TO SAVEPOINT name   (undo to savepoint, keep SP)
  |           +-- RELEASE SAVEPOINT name (remove SP after rollback)
  |
  +-- COMMIT                  (persist everything)
        OR
  +-- ROLLBACK                (discard everything, including released SPs)
```

## Key Patterns

### Basic Savepoint
```pascal
Conn.ExecuteNonQuery('BEGIN TRANSACTION');
Conn.ExecuteNonQuery('SAVEPOINT sp_name');
// ... operations ...
Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_name');  // success
Conn.ExecuteNonQuery('COMMIT');
```

### Partial Rollback
```pascal
Conn.ExecuteNonQuery('SAVEPOINT sp_risky');
// ... risky operation ...
if Failed then
begin
  Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_risky');
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_risky');
end
else
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_risky');
```

### Batch Error Isolation
```pascal
Conn.ExecuteNonQuery('BEGIN TRANSACTION');
for I := 1 to N do
begin
  Conn.ExecuteNonQuery('SAVEPOINT sp_' + IntToStr(I));
  if OperationSucceeds then
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_' + IntToStr(I))
  else begin
    Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_' + IntToStr(I));
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_' + IntToStr(I));
  end;
end;
Conn.ExecuteNonQuery('COMMIT');
```

### Retry Pattern
```pascal
for Attempt := 1 to MaxRetries do
begin
  Conn.ExecuteNonQuery('SAVEPOINT sp_retry');
  if TryOperation then
  begin
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_retry');
    Break;
  end
  else
    Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_retry');
    // sp_retry still exists, can be reused
end;
```

## Compilation

```bash
cd 92_Savepoints
lazbuild Savepoints.lpi
./Savepoints
```

## Sample Output

```
4. ROLLBACK TO SAVEPOINT (Partial Rollback)
   Transfer 1: Bob -> Charlie $50 (released)
   Transfer 2: Charlie -> Diana $200 (in savepoint)
   Validation failed! Rolling back Transfer 2...
   Transfer 3: Alice -> Bob $25 (released)
   COMMIT - Transfers 1 and 3 committed, Transfer 2 was rolled back

5. Nested SAVEPOINTs (3 Levels Deep)
   Audit entries with level1: 1
   Audit entries with level2: 1
   Audit entries with level3: 0 (rolled back)

6. Order Processing with Savepoints
   Order 1: Alice buys 10 Widgets - CONFIRMED
   Order 2: Bob buys 10 Doohickeys - INSUFFICIENT STOCK (only 5)
   Order 3: Charlie buys 5 Gadgets - CONFIRMED

9. Full Transaction Rollback (Savepoints Discarded)
   Transactions after rollback: 3 (unchanged)
   Both released savepoints were discarded by ROLLBACK
```

## Related Examples

- **93_StateMachine** - State transitions with validation
- **94_CQRS** - Command/Query separation pattern
- **86_WebhookStorage** - Webhook delivery with retry

## Best Practices

1. **Always RELEASE after ROLLBACK TO**: The savepoint persists after rollback - release it to clean up
2. **Use unique names**: Name savepoints descriptively (`sp_order_1`, `sp_transfer_alice`)
3. **Error isolation**: Wrap each independent operation in its own savepoint
4. **Audit outside savepoints**: Keep audit/logging outside the rollback scope
5. **Don't rely on RELEASE alone**: Only COMMIT persists to disk; RELEASE just merges into parent scope
6. **Retry pattern**: ROLLBACK TO preserves the savepoint for reuse without recreating it
7. **Batch processing**: Savepoints enable all-or-nothing per item within a single transaction
8. **Keep transactions short**: Even with savepoints, long-held transactions block other writers
