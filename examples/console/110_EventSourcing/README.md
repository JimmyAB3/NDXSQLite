# Example 110: Event Sourcing - Snapshot, Aggregate Rebuild, Temporal Queries

## Overview

This example demonstrates **Event Sourcing** with an append-only event store, aggregate state rebuilt from events, snapshots for performance, temporal queries (state at any point in time), materialized projections, and event replay.

## Features Demonstrated

### 1. Event Store (Append-Only Log)
- Events are immutable, never updated or deleted
- Each event has: sequence_id, aggregate_id, event_type, event_data, timestamp, version
- Per-aggregate versioning for optimistic concurrency
- Domain: Bank accounts with AccountOpened, MoneyDeposited, MoneyWithdrawn, AccountClosed events

### 2. Aggregate Rebuild from Events
- Current state derived by replaying all events in order
- No mutable "current state" table needed
- ApplyEvent function folds events into state
- Full rebuild from event_store ORDER BY version ASC

### 3. Temporal Queries (State at Any Point in Time)
- Query state at any version: WHERE version <= N
- Query state at any timestamp: WHERE event_timestamp <= T
- Trace balance history over time
- No special history table needed (events ARE the history)

### 4. Snapshots (Performance Optimization)
- Save aggregate state at a version for fast loading
- Rebuild = load snapshot + replay events after snapshot version
- Reduces events replayed from N to (N - snapshot_version)
- Verified: snapshot rebuild matches full rebuild

### 5. Business Rules (Invariant Enforcement)
- Check current state before appending events
- Insufficient funds: reject withdrawal if balance < amount
- Account closure: reject if balance != 0
- Invariants checked on rebuilt aggregate, not projections

### 6. Projections (Materialized Views)
- account_projection: current balance, status, owner per account
- transaction_projection: deposit/withdrawal history with running balance
- Updated after each command (event + projection update)
- Separate from event store (can be rebuilt at any time)

### 7. Event Replay (Rebuild Projections)
- Clear all projections
- Replay events to rebuild materialized views
- Result matches original projections exactly
- Enables schema changes, new projections, bug fixes

### 8. Event Store Statistics
- Total event count
- Events by type distribution
- Events per aggregate
- Timeline (events per month)
- Snapshot count

## Architecture

```
+------------------+     +------------------+
| Domain Commands  |     | Event Store      |
+------------------+     +------------------+
| OpenAccount()    |---->| sequence_id (PK) |
| Deposit()        |     | aggregate_id     |
| Withdraw()       |     | aggregate_type   |
| CloseAccount()   |     | event_type       |
+------------------+     | event_data       |
        |                 | event_timestamp  |
        v                 | version          |
+------------------+     +------------------+
| Aggregate        |             |
+------------------+             v
| RebuildAggregate |     +------------------+
| ApplyEvent()     |     | Snapshots        |
| RebuildAtVersion |     +------------------+
| RebuildAtTime    |     | aggregate_id     |
| RebuildFromSnap  |     | version          |
+------------------+     | state_data       |
        |                 +------------------+
        v
+------------------+     +------------------+
| Projections      |     | Transaction Log  |
+------------------+     +------------------+
| account_id (PK)  |     | account_id       |
| owner, balance   |     | type, amount     |
| status, version  |     | balance_after    |
+------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| event_store      |     | snapshots        |
+------------------+     +------------------+
| sequence_id (PK) |     | id (PK, AUTO)    |
| aggregate_id     |     | aggregate_id     |
| aggregate_type   |     | aggregate_type   |
| event_type       |     | version          |
| event_data       |     | state_data       |
| event_timestamp  |     | created_at       |
| version          |     +------------------+
+------------------+

+------------------+     +---------------------+
| account_projection|    | transaction_projection|
+------------------+     +---------------------+
| account_id (PK)  |     | id (PK, AUTO)       |
| owner            |     | account_id          |
| balance          |     | transaction_type    |
| status           |     | amount              |
| last_version     |     | balance_after       |
| updated_at       |     | description         |
+------------------+     | event_timestamp     |
                          +---------------------+
```

## Compilation

```bash
cd 110_EventSourcing
lazbuild EventSourcing.lpi
./EventSourcing
```

## Sample Output

```
1. Event Store (Append-Only Log)
   [1] ACC-001 v1: AccountOpened (2024-01-15 09:00:00)
   [3] ACC-001 v2: MoneyDeposited (2024-01-15 10:00:00)

2. Aggregate Rebuild from Events
   ACC-001: Owner=Alice Johnson, Balance=$6300.00, Version=4

3. Temporal Queries
   After v2 (deposit):  $5000.00
   After v3 (salary):   $7500.00
   Feb 01, 2024 noon:   $7500.00

4. Snapshots
   Full rebuild:  $8500.00 (v6)
   From snapshot: $8500.00 (v6) - Match: YES
   Events skipped: 4, Replayed: 2

5. Business Rules
   ERROR: Insufficient funds (balance: $8500.00, requested: $50000.00)
   ERROR: Cannot close account with balance $8500.00
```

## Related Examples

- **109_UnitOfWork** - Transaction management with tracked changes
- **108_RepositoryPattern** - CRUD and specification patterns

## Best Practices

1. **Append-only**: Never update or delete events; they are the source of truth
2. **Aggregate rebuild**: Derive current state by replaying events, not from mutable tables
3. **Snapshots**: Cache state at intervals to avoid replaying thousands of events
4. **Temporal queries**: Filter events by version or timestamp for point-in-time state
5. **Projections**: Materialized views for read performance, rebuildable from events
6. **Business rules on aggregates**: Validate invariants against rebuilt state, not projections
7. **Event replay**: Ability to rebuild all projections enables schema evolution
8. **Per-aggregate versioning**: Enables optimistic concurrency control
