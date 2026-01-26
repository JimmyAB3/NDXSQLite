# Example 141: Optimistic Concurrency - MVCC with Version Vectors

## Overview

This example demonstrates **Multi-Version Concurrency Control (MVCC)** using version vectors for conflict detection and resolution in a distributed document store. It simulates multiple nodes writing concurrently, detects conflicts using vector clock comparison, and applies configurable merge strategies per document type.

## Features Demonstrated

### 1. Document Store
- Field-level versioned documents (key-value with version vectors)
- 4 document types: user, config, counter, profile
- Initial state with A:1 version vector (single-node origin)

### 2. Version Vectors
- Multi-node causality tracking (node-A, node-B, node-C)
- Dominance comparison rules
- Conflict detection: A:2,B:0 vs A:1,B:1 = CONFLICT

### 3. Concurrent Writes
- Timeline of user:alice updates across 3 nodes
- Node-B writes based on stale version (A:1, missing A:2)
- Clear demonstration of concurrent modification

### 4. Last-Writer-Wins (LWW)
- Timestamp-based resolution for user documents
- Node-B wins (wrote at 10:06 > 10:05)
- Simple but may lose important updates

### 5. First-Writer-Wins (FWW)
- Stability-focused resolution for config documents
- First write preserved, later conflicting writes rejected
- Prevents configuration flickering

### 6. Custom Merge Strategies
- 6 strategies configured per document/field pattern
- Counter merge-add: 100 + (115-100) + (120-100) = 135
- Profile field-level: name=LWW, bio=longest-wins, score=max-wins

### 7. Read Snapshots
- Point-in-time consistent views
- "initial" vs "post-conflict" comparison
- Shows how values evolved through conflict resolution

### 8. Version History
- Full audit trail of 18 document changes
- Operation tracking: create, update
- Node attribution and timestamps

### 9. Multi-Node Vectors
- 3-way conflict: A:2,B:0,C:0 vs A:1,B:0,C:1
- Neither dominates: A wins in node-A, C wins in node-C
- Pending conflict requiring manual resolution

### 10. Conflict Statistics
- 7 total conflicts (6 resolved, 1 pending)
- 5 different strategies applied
- Node-A vs Node-B: 6 conflicts (most active pair)

## Architecture

```
+------------------+     +------------------+     +------------------+
| Node-A           |     | Conflict         |     | Resolution       |
| Node-B           |---->| Detection        |---->| Strategies       |
| Node-C           |     | (vector compare) |     | (per doc type)   |
+------------------+     +------------------+     +------------------+
        |                         |                         |
        v                         v                         v
+------------------+     +------------------+     +------------------+
| doc_history      |     | conflicts        |     | snapshots        |
| (audit trail)    |     | (pending/resolved)|    | (point-in-time)  |
+------------------+     +------------------+     +------------------+
```

## Version Vector Comparison

```
Vector A: A:2, B:0          Vector B: A:1, B:1
           |                            |
Compare each component:
  A: 2 > 1  (A dominates)
  B: 0 < 1  (B dominates)
           |
Result: CONFLICT (neither fully dominates)
```

## Merge Strategies

| Strategy | Logic | Use Case |
|----------|-------|----------|
| last-writer-wins | Latest timestamp | User profiles, general data |
| first-writer-wins | Earliest write kept | Configuration, settings |
| merge-add | Sum of deltas from base | Counters, accumulators |
| longest-wins | Longer string value | Text fields, bios |
| max-wins | Higher numeric value | Scores, high-water marks |

## Database Schema

```
+------------------+     +------------------+     +------------------+
| documents        |     | version_entries  |     | doc_history      |
+------------------+     +------------------+     +------------------+
| doc_key (PK)     |     | doc_key (PK)     |     | id (PK)          |
| field_name (PK)  |     | node_id (PK)     |     | doc_key          |
| field_value      |     | version           |     | field_name       |
| version_vector   |     +------------------+     | field_value      |
| updated_by       |                               | version_vector   |
| updated_at       |     +------------------+     | node_id          |
+------------------+     | conflicts        |     | operation        |
                          +------------------+     | timestamp        |
+------------------+     | doc_key          |     +------------------+
| merge_strategies |     | field_name       |
+------------------+     | value_a/b        |
| doc_pattern (PK) |     | vector_a/b       |
| field_pattern(PK)|     | strategy         |
| strategy         |     | resolved_value   |
+------------------+     | status           |
                          +------------------+
```

## Compilation

```bash
cd 141_OptimisticConcurrencyAdvanced
lazbuild OptimisticConcurrencyAdvanced.lpi
./OptimisticConcurrencyAdvanced
```

## Sample Output

```
1. Documents: 10 fields across 4 document types
2. Vectors: 3 nodes tracking causality
3. Concurrent: A:2,B:0 vs A:1,B:1 detected as conflict
4. LWW: Node-B wins (later timestamp)
5. FWW: Node-A wins (first writer preserved)
6. Merge: counter=135, bio=longest, score=max
7. Snapshots: initial vs post-conflict comparison
8. History: 18 audit entries across 3 nodes
9. 3-way: A:2,B:0,C:0 vs A:1,B:0,C:1 = pending
10. Stats: 6 resolved, 1 pending, 5 strategies
```

## Related Examples

- **142_PubSubDatabase** - Publish/subscribe messaging
- **143_DistributedLock** - Lock manager with deadlock detection

## Best Practices

1. **Version vectors over timestamps**: Vectors capture causality, timestamps can drift
2. **Field-level granularity**: Reduces conflicts (different fields rarely conflict)
3. **Strategy per document type**: Counter merging != text merging
4. **Preserve history**: Never delete old versions for auditability
5. **Snapshot isolation**: Read snapshots provide consistent point-in-time views
6. **Pending conflicts**: Some conflicts require human resolution (3-way merges)
7. **Base value tracking**: Merge-add requires knowing the common ancestor value
8. **Monotonic vectors**: Node versions only increase, never decrease
