# Example 143: Distributed Lock Manager

## Overview

This example demonstrates a **distributed lock manager** implemented with SQLite tables. It simulates named locks with TTL expiry, lock queuing with priority, reentrant lock semantics, wait-for graph construction, and deadlock detection using recursive CTEs for cycle finding.

## Features Demonstrated

### 1. Lock Registry
- 8 named locks (resource: and mutex: prefixes)
- States: free, held, expired
- Owner tracking and reentry counts
- Expiry timestamps (TTL)

### 2. Lock Acquisition and Release
- Atomic acquire on free locks (UPDATE WHERE status='free')
- TTL assignment on acquisition
- Release resets owner and status
- History logging for all operations

### 3. TTL Expiry Detection
- Time-based expiry check against current timestamp
- Overdue calculation (seconds past TTL)
- Force-expiry of stale locks
- Previously-expired lock tracking

### 4. Lock Queue (Waiters)
- Priority-based queue ordering (higher priority first)
- FIFO within same priority level
- Position calculation per lock
- Timeout tracking for queued requests

### 5. Reentrant Locks
- Same owner can re-acquire (reentry_count increments)
- Partial release (decrement count, lock still held)
- Full release only when count reaches 0
- Non-owner reentry denied

### 6. Wait-For Graph
- Edge construction: waiter -> holder via lock
- Multi-edge support (one node waits for multiple locks)
- Dependency degree analysis (out-degree per node)
- Visual relationship display

### 7. Deadlock Detection (Cycle Finding)
- Recursive CTE traversal of wait-for graph
- Cycle detection: path returns to start node
- Depth-2 cycles: binary deadlocks (A<->B)
- Depth-3 cycles: three-way deadlocks (A->B->C->A)
- Resolution: abort youngest waiter

### 8. Lock Contention Analysis
- Queue depth per lock (most contested)
- Total acquires and waits from history
- Contention ratio: waits/acquires percentage
- Identifies hot locks

### 9. Lock History (Audit Trail)
- Complete timeline of lock operations
- Actions: acquire, release, reenter, enqueue, expired
- Owner and details for each event
- Chronological ordering

### 10. Lock System Health
- Overall metrics (total, held, free, expired)
- Per-node lock holdings and reentries
- Completed lock durations
- Queue depth and waiting node count

## Architecture

```
+------------------+     +------------------+     +------------------+
| Node-A           |     | Lock Manager     |     | Deadlock         |
| Node-B           |---->| (locks table)    |---->| Detector         |
| Node-C           |     |                  |     | (recursive CTE)  |
+------------------+     +------------------+     +------------------+
        |                         |                         |
        v                         v                         v
+------------------+     +------------------+     +------------------+
| lock_queue       |     | wait_for_graph   |     | lock_history     |
| (waiters)        |     | (dependencies)   |     | (audit trail)    |
+------------------+     +------------------+     +------------------+
```

## Deadlock Detection Algorithm

```
Recursive CTE: detect_cycle(start_node, current_node, path, depth)

Base:    For each edge (waiter -> holder), start a path
Step:    Follow edges from current_node, avoid revisits
         Stop expanding when current_node = start_node (cycle found)
Filter:  SELECT paths WHERE current_node = start_node AND depth >= 2

Example cycle:
  node-A -> node-B -> node-A  (depth 2)
  Meaning: A waits for B, B waits for A => DEADLOCK
```

## Wait-For Graph

```
node-A -----> node-B -----> node-A     (cycle: A<->B)
  |              |
  +-----> node-C -----> node-A         (cycle: A<->C)
              |
              +---------> node-B       (3-way: A->C->B->A)
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| locks            |     | lock_queue       |     | wait_for_graph   |
+------------------+     +------------------+     +------------------+
| lock_name (PK)   |     | id (PK)          |     | waiter (PK)      |
| owner            |     | lock_name        |     | holder (PK)      |
| acquired_at      |     | requester        |     | lock_name (PK)   |
| expires_at       |     | requested_at     |     +------------------+
| reentry_count    |     | priority         |
| status           |     | timeout_at       |     +------------------+
+------------------+     | status           |     | lock_history     |
                          +------------------+     +------------------+
                                                    | id (PK)          |
                                                    | lock_name        |
                                                    | owner            |
                                                    | action           |
                                                    | details          |
                                                    | timestamp        |
                                                    +------------------+
```

## Compilation

```bash
cd 143_DistributedLock
lazbuild DistributedLock.lpi
./DistributedLock
```

## Sample Output

```
1. Registry: 8 locks (5 held, 2 free, 1 expired)
2. Acquire/Release: node-A acquires file-store (TTL=300s)
3. TTL: mutex:user-update overdue by 29s, force-expired
4. Queue: 6 waiters, db-primary has 2 (priority-ordered)
5. Reentrant: cache-1 count=2, increment/decrement demo
6. Wait-for: 6 edges, A waits for 2 nodes
7. Deadlocks: 4 depth-2 cycles, 3 depth-3 cycles
8. Contention: 66.7% ratio (6 waits / 9 acquires)
9. History: 20 entries (9 acquire, 6 enqueue, 2 release, 2 reenter, 1 expired)
10. Health: 3 nodes, A holds 3 locks, B holds 2
```

## Related Examples

- **141_OptimisticConcurrencyAdvanced** - MVCC with version vectors
- **142_PubSubDatabase** - Publish/subscribe messaging

## Best Practices

1. **TTL on all locks**: Prevents indefinite lock holding from crashed nodes
2. **Priority queuing**: Critical operations can jump the queue
3. **Reentrant semantics**: Prevents self-deadlock in recursive code paths
4. **Wait-for graph**: Enables real-time deadlock detection without timeouts
5. **Cycle detection via CTE**: Database-native solution, no external tooling
6. **Youngest-first abort**: Minimizes wasted work when breaking deadlocks
7. **Audit trail**: Full history enables post-mortem analysis
8. **Contention metrics**: Identifies locks that need redesign (too hot)
