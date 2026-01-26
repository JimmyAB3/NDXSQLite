# Example 100: Merge Replication - Vector Clocks & Merge Strategies

## Overview

This example demonstrates **merge replication** using vector clocks for causality tracking and multiple conflict resolution strategies. It implements a three-node replication topology with Last-Writer-Wins (LWW) and field-level merge strategies, tombstone deletions, anti-entropy sync, and convergence verification.

## Features Demonstrated

### 1. Three-Node Setup
- Independent databases with identical schema
- Each node manages different product categories
- Vector clocks track causal history per record

### 2. Vector Clock Basics
- Clock comparison: BEFORE, AFTER, CONCURRENT, EQUAL
- Clock increment on write (per-node counter)
- Clock merge (component-wise max)

### 3. Initial Full Replication
- All-pairs bidirectional sync
- New records inserted with source vector clock
- Replication log tracks all operations

### 4. Causal Ordering (No Conflict)
- Sequential updates across nodes
- Vector clock naturally orders causally related writes
- No conflict when one clock dominates another

### 5. Concurrent Writes - Last-Writer-Wins (LWW)
- Two nodes modify same record without syncing
- Vector clocks detect concurrency
- Timestamp comparison breaks the tie

### 6. Concurrent Writes - Field-Level Merge
- Custom merge strategy for concurrent modifications
- max(price), sum(stock), longer name, merged categories
- Both sides contribute to final state

### 7. Delete Replication (Tombstones)
- Soft delete propagates via replication
- Tombstone carries vector clock
- Deleted records excluded from active queries

### 8. Delete vs Update Conflict
- One node updates, another deletes concurrently
- LWW by timestamp determines winner
- Delete wins if its timestamp is later

### 9. Anti-Entropy (Full State Sync)
- Complete all-pairs replication round
- Ensures all nodes reach consistent state
- Handles any missed updates

### 10. Convergence Verification
- Compare active record counts across nodes
- Field-by-field comparison of shared records
- Detect and report mismatches

### 11. Replication History
- Full log of all replication operations
- Tracks direction, action, clock comparison, resolution
- Useful for debugging and auditing

## Database Schema

```
+---------------------+     +---------------------+     +------------------+
| products            |     | replication_log      |     | node_state       |
+---------------------+     +---------------------+     +------------------+
| id (PK, text)       |     | id (PK, AUTO)        |     | node_name (PK)   |
| name                |     | direction            |     | last_clock       |
| price (real)        |     | remote_node          |     +------------------+
| stock (integer)     |     | record_id            |
| category            |     | action               |
| vclock              |     | local_clock          |
| updated_at          |     | remote_clock         |
| origin_node         |     | resolution           |
| is_deleted          |     | replicated_at        |
+---------------------+     +---------------------+
```

## Vector Clock Format

Clocks are stored as `A:B:C` strings where each component represents the logical counter for that node:
- `1:0:0` = Node A wrote once
- `2:1:0` = Node A wrote twice, Node B wrote once
- `3:2:1` = All three nodes contributed

## Clock Comparison Rules

```
A <= B (all components):  A happened BEFORE B
A >= B (all components):  A happened AFTER B
Neither dominates:        CONCURRENT (conflict!)
All equal:                EQUAL (same state)
```

## Conflict Resolution Strategies

### Last-Writer-Wins (LWW)
```pascal
if SrcUpdated >= TgtUpdated then
  // Source timestamp is later - overwrite target
else
  // Target timestamp is later - keep target
```

### Field-Level Merge
```pascal
MergedPrice := Max(SrcPrice, TgtPrice);     // Take higher price
MergedStock := SrcStock + TgtStock;          // Sum quantities
MergedCategory := TgtCategory + '+' + SrcCategory;  // Combine
```

## Key Patterns

### Vector Clock Increment
```pascal
function IncrementClock(const VC: TVectorClock; const NodeName: string): TVectorClock;
begin
  Result := VC;
  if NodeName = NODE_A then Inc(Result.NodeA)
  // ...
end;
```

### Clock Merge (Component-wise Max)
```pascal
function MergeClock(const A, B: TVectorClock): TVectorClock;
begin
  Result.NodeA := Max(A.NodeA, B.NodeA);
  Result.NodeB := Max(A.NodeB, B.NodeB);
  Result.NodeC := Max(A.NodeC, B.NodeC);
end;
```

### Replication Decision
```pascal
case CompareClocks(VCSrc, VCTgt) of
  crBefore:     // Source older -> skip
  crAfter:      // Source newer -> overwrite
  crConcurrent: // Conflict -> apply merge strategy
  crEqual:      // Same state -> skip
end;
```

## Compilation

```bash
cd 100_MergeReplication
lazbuild MergeReplication.lpi
./MergeReplication
```

## Sample Output

```
3. Initial Full Replication
   A <-> B: 3 + 2 records replicated
   A <-> C: 5 + 1 records replicated

4. Causal Ordering (No Conflict)
   Node A: Updated p-001 price to $1199.99
   Node B: Updated p-001 stock to 45 (after seeing A's change)
   -> No conflict, B's clock dominates A's

5. Concurrent Writes - Last-Writer-Wins
   Node A: price=$24.99 (clock=4:0:0, @09:00)
   Node B: price=$34.99 (clock=2:2:0, @09:30)
   -> CONCURRENT: Node B wins (later timestamp)

6. Field-Level Merge
   Node A: price=$59.99, stock=100
   Node C: stock=80, category=peripherals
   -> Merged: price=$59.99, stock=180, category=peripherals+accessories

10. Convergence Verification
   Active records: A=4, B=4, C=4
   All nodes converged successfully!
```

## Related Examples

- **98_DataSync** - Timestamp-based bidirectional sync
- **99_ChangeLog** - WAL-style changelog with replay
- **94_CQRS** - Event sourcing patterns

## Best Practices

1. **Vector clocks over timestamps**: Clocks detect true concurrency; timestamps only approximate
2. **Merge on conflict**: Always increment clock after merge to establish new causal point
3. **Strategy selection**: LWW for simplicity, field-merge for data preservation
4. **Anti-entropy rounds**: Periodic full sync catches any missed updates
5. **Tombstones**: Never physically delete - use soft delete for propagation
6. **Replication log**: Track all operations for debugging and compliance
7. **Convergence checks**: Verify all nodes agree after sync operations
8. **Clock format**: Use simple serializable format (colon-separated integers)
