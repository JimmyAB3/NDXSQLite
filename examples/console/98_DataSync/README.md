# Example 98: Data Sync - Bidirectional Synchronization

## Overview

This example demonstrates **bidirectional data synchronization** between two SQLite databases. It implements timestamp-based change detection, last-writer-wins conflict resolution, tombstone deletions, sync state tracking, and consistency verification.

## Features Demonstrated

### 1. Two-Node Setup
- Independent databases with identical schema
- Each node can operate offline
- Node origin tracking per record

### 2. Initial Sync
- Full bidirectional exchange on first sync
- New records pushed in both directions
- Sync state established

### 3. Independent Changes
- Both nodes add and modify records independently
- Changes are detected via `updated_at` timestamps
- Non-conflicting changes merge cleanly

### 4. Conflict Resolution (Last-Writer-Wins)
- Both nodes modify the same record
- Later timestamp wins
- Conflict count tracked in sync log

### 5. Tombstone Deletions
- Soft delete (is_deleted flag) instead of physical delete
- Tombstones propagate via sync
- Deleted records invisible to queries

### 6. Delete vs Update Conflict
- One node updates, other deletes same record
- Later operation wins (timestamp comparison)
- Tombstone overwrites update if newer

### 7. Sync History
- Every sync logged with direction, counts, conflicts
- Sync state tracks last sync time per remote node
- Full audit trail

### 8. Tombstone Cleanup
- Periodic purge of old tombstones
- Configurable retention period
- Prevents unbounded growth

### 9. Consistency Verification
- Compare active records across nodes
- Detect mismatches after sync
- Validate convergence

## Database Schema

```
+------------------+     +------------------+     +------------------+
| contacts         |     | sync_log         |     | sync_state       |
+------------------+     +------------------+     +------------------+
| id (PK, text)    |     | id (PK, AUTO)    |     | remote_node (PK) |
| name             |     | sync_direction   |     | last_sync_at     |
| email            |     | remote_node      |     +------------------+
| phone            |     | records_pushed   |
| updated_at       |     | conflicts        |
| is_deleted       |     | sync_at          |
| node_origin      |     +------------------+
+------------------+
```

## Sync Protocol

```
1. Get last_sync_at for remote node
2. Query source: SELECT * WHERE updated_at > last_sync_at
3. For each changed record:
   a. If not in target -> INSERT (new record)
   b. If in target:
      - Compare timestamps
      - If source newer -> UPDATE (last-writer-wins)
      - If target newer -> SKIP
4. Update sync_state with max timestamp
5. Log sync operation
```

## Key Patterns

### Bidirectional Sync
```pascal
function BidirectionalSync(A, B): Integer;
begin
  PushedAtoB := SyncNodes(A, B);  // Push A's changes to B
  PushedBtoA := SyncNodes(B, A);  // Push B's changes to A
end;
```

### Change Detection
```pascal
DS := Source.ExecuteQuery(
  'SELECT * FROM contacts WHERE updated_at > :last_sync ORDER BY updated_at');
```

### Last-Writer-Wins
```pascal
if RemoteTime > LocalTime then
  // Source is newer - overwrite target
  Target.ExecuteNonQuery('UPDATE contacts SET ... WHERE id = ...')
// else: target is newer, skip
```

### Soft Delete (Tombstone)
```pascal
Conn.ExecuteNonQuery(
  'UPDATE contacts SET is_deleted = 1, updated_at = :timestamp WHERE id = :id');
```

### Active Records Query
```pascal
DS := Conn.ExecuteQuery('SELECT * FROM contacts WHERE is_deleted = 0');
```

## Compilation

```bash
cd 98_DataSync
lazbuild DataSync.lpi
./DataSync
```

## Sample Output

```
2. Initial Bidirectional Sync
   Sync complete: A->B: 3 records, B->A: 2 records

4. Conflict Resolution (Last-Writer-Wins)
   Node A: Updated Carol at 11:00
   Node B: Updated Carol at 11:30
   Sync complete: A->B: 0 records, B->A: 1 records
   -> Carol W. Johnson (Node B wins)

6. Delete vs Update Conflict
   Node A: Updated Alice at 14:00
   Node B: Deleted Alice at 14:30
   -> Delete wins (later timestamp)

9. Final Consistency Check
   OK: c-002 = Bob Jones Jr
   OK: c-003 = Carol W. Johnson
   OK: c-004 = Dave Brown
   OK: c-007 = Grace Kim
```

## Related Examples

- **99_ChangeLog** - WAL-style changelog with replay
- **100_MergeReplication** - Vector clocks and merge strategies
- **94_CQRS** - Event sourcing patterns

## Best Practices

1. **Timestamp precision**: Use ISO 8601 format for reliable string comparison
2. **Tombstones over DELETE**: Never physically delete - use is_deleted flag
3. **Node origin**: Track which node created/modified each record
4. **Sync watermark**: Track max(updated_at) from source, not wall clock time
5. **Conflict logging**: Record all conflicts for debugging and analysis
6. **Idempotent sync**: Running sync twice should produce the same result
7. **Tombstone purge**: Periodically clean up old tombstones to save space
8. **Consistency checks**: Verify convergence after sync operations
