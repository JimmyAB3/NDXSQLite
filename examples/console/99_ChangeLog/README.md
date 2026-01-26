# Example 99: ChangeLog - Modification Journal with Replay

## Overview

This example demonstrates a **WAL-style changelog** (Write-Ahead Log) pattern for tracking all modifications to database records. It implements an append-only log with old/new data capture, point-in-time recovery, snapshots, undo operations, and log compaction.

## Features Demonstrated

### 1. Recording Changes to the Changelog
- All INSERT, UPDATE, DELETE operations logged
- Old and new data captured as key=value strings
- Actor (who), timestamp (when), transaction ID (grouping)

### 2. Full Changelog (Append-Only Log)
- Sequential entries with monotonic sequence numbers
- Immutable log (append-only, no modifications)
- Complete audit trail of all changes

### 3. Point-in-Time Recovery
- Rebuild database state at any past timestamp
- Replay INSERT/UPDATE/DELETE from changelog
- Consistent state reconstruction

### 4. Snapshots (Checkpoints)
- Named snapshots at specific sequence numbers
- Reduce replay cost for recovery
- Snapshot metadata (label, description, timestamp)

### 5. Record History (Audit Trail)
- Complete history for any individual record
- Shows who changed what, when, and how
- Tracks record from creation to deletion

### 6. Undo Last Change
- Reverse the most recent change using old_data
- Logged as a new changelog entry (preserves history)
- Supports undoing UPDATE and DELETE operations

### 7. Transaction Grouping
- Changes grouped by transaction ID (tx_id)
- Shows all operations in a single logical unit
- Tracks actors and operation types per transaction

### 8. Change Statistics
- Operations by type (INSERT, UPDATE, DELETE)
- Changes by actor (who made the most changes)
- Most frequently changed records

### 9. Log Compaction
- Remove redundant entries before a snapshot
- Keep only the last state per record for recovery
- Prevents unbounded log growth

### 10. Diff Between Two Points in Time
- Show all changes between two timestamps
- Count of affected records
- Useful for change review and auditing

## Database Schema

```
+---------------------+     +---------------------+
| accounts            |     | changelog           |
+---------------------+     +---------------------+
| id (PK, text)       |     | seq (PK, AUTO)      |
| owner               |     | table_name          |
| balance (real)      |     | operation           |
| status              |     | record_id           |
+---------------------+     | old_data            |
                             | new_data            |
+---------------------+      | actor               |
| snapshots           |     | changed_at          |
+---------------------+     | tx_id               |
| name (PK, text)     |     +---------------------+
| at_seq              |
| created_at          |
| description         |
+---------------------+
```

## Key Patterns

### Append-Only Log Entry
```pascal
procedure LogChange(const TableName, Operation, RecordId,
  OldData, NewData, Actor, Timestamp: string; TxId: Integer);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO changelog (table_name, operation, record_id, ' +
    'old_data, new_data, actor, changed_at, tx_id) VALUES (...)');
end;
```

### Point-in-Time Recovery
```pascal
// Replay log entries up to target timestamp
DS := Conn.ExecuteQuery(
  'SELECT * FROM changelog WHERE changed_at <= :target ORDER BY seq');
// For each entry: apply INSERT/UPDATE/DELETE to rebuild state
```

### Undo Operation
```pascal
// Find last change for record, reverse it using old_data
DS := Conn.ExecuteQuery(
  'SELECT * FROM changelog WHERE record_id = :id ORDER BY seq DESC LIMIT 1');
// Apply old_data as new state, log as new changelog entry
```

### Log Compaction
```pascal
// Keep only the latest entry per record before snapshot
Conn.ExecuteNonQuery(
  'DELETE FROM changelog WHERE seq <= :snap_seq ' +
  'AND seq < (SELECT MAX(seq) FROM changelog c2 ' +
  '           WHERE c2.record_id = changelog.record_id AND c2.seq <= :snap_seq)');
```

## Compilation

```bash
cd 99_ChangeLog
lazbuild ChangeLog.lpi
./ChangeLog
```

## Sample Output

```
1. Recording Changes to the Changelog
   Created 4 accounts (tx=1)
   Updated balances (tx=2)
   Final updates (tx=5)

2. Full Changelog (Append-Only Log)
   [  1]  INSERT  acc-001 tx=1 new={owner=Alice Johnson;balance=1000;status=active}
   [  5]  UPDATE  acc-001 tx=2 new={balance=1500} old={balance=1000}
   [ 11]  DELETE  acc-002 tx=4 old={owner=Bob Smith;balance=0;status=active}

3. Point-in-Time Recovery
   State at 2024-01-15T10:05:00:
    acc-001  Alice Johnson $   1500.00  active
    acc-002      Bob Smith $   2300.00  active

6. Undo Last Change
   Reverting seq=14: UPDATE old={status=frozen}
   acc-004 status reverted from active to frozen

9. Log Compaction
   Before: 15 entries, After: 11 entries
   Removed: 4 redundant entries
```

## Related Examples

- **98_DataSync** - Bidirectional synchronization between databases
- **100_MergeReplication** - Vector clocks and merge strategies
- **94_CQRS** - Event sourcing patterns

## Best Practices

1. **Append-only**: Never modify or delete changelog entries during normal operation
2. **Old + New data**: Always capture both for reversibility
3. **Transaction grouping**: Use tx_id to group related changes
4. **Monotonic sequence**: Use auto-increment for ordering guarantee
5. **Snapshots before compaction**: Always create a snapshot before compacting
6. **Actor tracking**: Record who made each change for audit purposes
7. **ISO timestamps**: Use ISO 8601 format for reliable ordering
8. **Compaction safety**: Keep at least the last entry per record
