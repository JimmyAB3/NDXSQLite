# Example 109: Unit of Work - Tracked Changes, Batch Commit, Rollback

## Overview

This example demonstrates the **Unit of Work pattern** with change tracking, atomic batch commits, rollback on failure, nested scopes via savepoints, pre-commit validation, and a full audit trail.

## Features Demonstrated

### 1. Basic Unit of Work
- Begin/Commit transaction lifecycle
- Track INSERT, UPDATE, DELETE operations
- Atomic commit of multiple changes
- Change count per UoW session

### 2. Rollback on Failure
- Simulate validation error mid-transaction
- ROLLBACK reverts all changes atomically
- Data remains unchanged after rollback
- Demonstrates all-or-nothing semantics

### 3. Nested Scopes (Savepoints)
- SAVEPOINT for partial rollback within a transaction
- Independent scopes: one succeeds, one fails
- RELEASE SAVEPOINT to keep nested changes
- ROLLBACK TO SAVEPOINT for partial undo
- Outer transaction still commits successfully

### 4. Batch Commit with Validation
- Multiple inserts in a single UoW
- Pre-commit validation (total salary check)
- Conditional commit or rollback based on rules

### 5. Change Log (Audit Trail)
- Every tracked change recorded with details
- UoW ID, entity, change type, field changes
- Persists after commit for audit purposes

### 6. Session History
- UoW session lifecycle tracking
- Status: active, committed, rolled_back
- Timestamps and change counts
- Note: rolled-back sessions are also undone (atomicity)

### 7. Idempotent Operations
- UPSERT pattern (INSERT OR UPDATE ON CONFLICT)
- Safely handle duplicate entries
- Track as INSERT or UPDATE accordingly

### 8. Statistics
- Session counts by status
- Change type distribution (INSERT/UPDATE/DELETE)
- Entity-level change counts

## Architecture

```
+------------------+
| Application      |
+------------------+
        |
        v
+------------------+     +------------------+
| Unit of Work     |---->| Change Log       |
+------------------+     +------------------+
| UoWBegin()       |     | uow_id           |
| UoWCommit()      |     | entity_table     |
| UoWRollback()    |     | entity_id        |
| UoWSavepoint()   |     | change_type      |
| TrackChange()    |     | field_changes    |
+------------------+     +------------------+
        |
        v
+------------------+     +------------------+
| SQLite           |     | UoW Sessions     |
| Transactions     |     +------------------+
+------------------+     | started_at       |
| BEGIN            |     | finished_at      |
| COMMIT           |     | status           |
| ROLLBACK         |     | change_count     |
| SAVEPOINT        |     +------------------+
| RELEASE          |
+------------------+
```

## Database Schema

```
+------------------+     +------------------+
| uow_sessions     |     | change_log       |
+------------------+     +------------------+
| id (PK, AUTO)    |<--->| id (PK, AUTO)    |
| started_at       |     | uow_id (FK)      |
| finished_at      |     | entity_table     |
| status           |     | entity_id        |
| change_count     |     | change_type      |
+------------------+     | field_changes    |
                          | tracked_at       |
Domain tables:            +------------------+
+------------------+     +------------------+
| departments      |     | employees        |
+------------------+     +------------------+
| id (PK, AUTO)    |<--->| id (PK, AUTO)    |
| name (UNIQUE)    |     | name             |
| budget           |     | department_id(FK)|
+------------------+     | salary           |
                          | hired_at         |
                          +------------------+
```

## Compilation

```bash
cd 109_UnitOfWork
lazbuild UnitOfWork.lpi
./UnitOfWork
```

## Sample Output

```
1. Basic Unit of Work
   UoW #1 started
   Tracked: INSERT employee #5
   Tracked: UPDATE employee #1 salary
   UoW #1 committed (4 changes)

2. Rollback on Failure
   ERROR: Salary 200000 exceeds budget!
   UoW #2 rolled back
   Alice salary after rollback: $98000 (unchanged)

3. Nested Scopes with Savepoints
   CONSTRAINT VIOLATION: salaries exceed budget!
   Rolled back to savepoint "salary_changes_2"
   UoW committed (budget changes kept, salary changes reverted)
```

## Related Examples

- **108_RepositoryPattern** - CRUD and specification patterns
- **110_EventSourcing** - Event-based state management

## Best Practices

1. **Atomic commits**: All changes succeed or none do
2. **Track before execute**: Log the intent before making the change
3. **Pre-commit validation**: Check business rules before COMMIT
4. **Savepoints for nesting**: Partial rollback without losing outer changes
5. **Audit persistence**: Change log survives committed transactions
6. **Rollback atomicity**: ROLLBACK undoes everything including audit entries in same transaction
7. **Upsert for idempotency**: ON CONFLICT DO UPDATE prevents duplicate key errors
8. **Session tracking**: Monitor UoW lifecycle for debugging and metrics
