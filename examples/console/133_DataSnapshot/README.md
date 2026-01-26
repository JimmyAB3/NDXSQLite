# Example 133: Data Snapshot - Point-in-Time Snapshots, Diff, Restore

## Overview

This example demonstrates a **Point-in-Time Snapshot System** with shadow tables, snapshot creation, data restoration, diff comparison, and history navigation. It simulates a version control system for database records, enabling rollback, auditing, and change tracking.

## Features Demonstrated

### 1. Initial Product Inventory
- 8 products across electronics, accessories, and furniture
- Full attribute tracking: name, category, price, stock, status
- Baseline state for snapshot comparison

### 2. Snapshot Creation (Baseline)
- Point-in-time copy of all records into shadow table
- Metadata recording: name, description, timestamp, record count
- Unique snapshot naming for identification

### 3. Data Modifications (Round 1)
- Price increase (Laptop Pro 15: $1299.99 -> $1399.99)
- Stock reduction (Wireless Mouse: 200 -> 180)
- Status change (Desk Lamp: active -> discontinued)
- New product insertion (Bluetooth Speaker)
- Change log entries for each modification

### 4. Second Snapshot
- Captures state after 4 modifications
- Records 9 products (1 new addition)

### 5. Data Modifications (Round 2)
- Product deletion (Desk Lamp removed)
- Price reduction (Mechanical Keyboard: $149.99 -> $129.99)
- Stock increase (USB-C Cable: 500 -> 750)
- New product (Phone Stand)
- Category reclassification (Monitor Stand: accessories -> ergonomics)

### 6. Third Snapshot
- Captures state after deletions and restocking
- Records 9 products (1 deleted, 1 added since round 1)

### 7. Snapshot Timeline
- Chronological listing of all snapshots
- Metadata display: ID, name, description, timestamp, row count

### 8. Diff Between Snapshots
- Deleted records detection (baseline vs round2)
- Added records detection
- Modified records with field-level comparison
- Price, stock, category, and status change tracking

### 9. Snapshot Restore
- Full data restoration from any previous snapshot
- DELETE + INSERT approach for clean restore
- Verification of restored state
- Non-destructive (shadow data preserved)

### 10. History Navigation
- Per-product price timeline across all snapshots
- Products appearing/disappearing from timeline
- GROUP_CONCAT for compact history display
- Change log summary by type

### 11. Storage Analysis
- Per-snapshot row and field counts
- Estimated byte usage
- Total shadow record count
- Storage overhead metrics

### 12. Comparison Matrix
- Pairwise differences between consecutive snapshots
- Added/Removed/Modified counts
- Full-span comparison (first to last)
- Change velocity tracking

## Architecture

```
+------------------+     +------------------+     +------------------+
| Live Data        |     | Snapshot Engine   |     | Diff Engine      |
+------------------+     +------------------+     +------------------+
| products table   |---->| Shadow copy      |---->| Record compare   |
| Current state    |     | Metadata record  |     | Field-level diff |
| CRUD operations  |     | Change logging   |     | History timeline |
+------------------+     +------------------+     +------------------+
        |                         |                         |
        v                         v                         v
+------------------+     +------------------+     +------------------+
| Change Log       |     | Shadow Tables    |     | Restore Engine   |
+------------------+     +------------------+     +------------------+
| INSERT/UPDATE/DEL|     | products_shadow  |     | DELETE + INSERT   |
| Field tracking   |     | snapshot_id FK   |     | Any snapshot      |
| Timestamps       |     | Full row copies  |     | State verification|
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| products         |     | snapshots        |     | products_shadow  |
+------------------+     +------------------+     +------------------+
| id (PK)          |     | id (PK)          |     | shadow_id (PK)   |
| name             |     | snapshot_name (UQ)|     | snapshot_id (FK) |
| category         |     | description      |     | product_id       |
| price            |     | created_at       |     | name             |
| stock            |     | record_count     |     | category         |
| status           |     +------------------+     | price            |
+------------------+                               | stock            |
                          +------------------+     | status           |
                          | change_log       |     +------------------+
                          +------------------+
                          | id (PK)          |
                          | product_id       |
                          | change_type      |
                          | field_name       |
                          | old_value        |
                          | new_value        |
                          | changed_at       |
                          +------------------+
```

## Snapshot Lifecycle

```
Time: ------[Baseline]-------[Round1]-------[Round2]------>

Products:    8 records         9 records      9 records
Changes:     (initial)         +1 new         -1 del, +1 new
                               3 modified     3 modified

Full Span:   Baseline -> Round2 = +2 added, -1 removed, 5 modified
```

## Compilation

```bash
cd 133_DataSnapshot
lazbuild DataSnapshot.lpi
./DataSnapshot
```

## Sample Output

```
1. Initial: 8 products in inventory
2. Baseline snapshot: 8 records captured
3. Round 1: 4 changes (price, stock, status, insert)
4. Round 1 snapshot: 9 records
5. Round 2: 5 changes (delete, price, stock, insert, category)
6. Round 2 snapshot: 9 records
8. Diff: 1 deleted, 2 added, 5 modified fields
9. Restore: 9 -> 8 products (baseline state verified)
12. Matrix: baseline->round2 = +2 added, -1 removed, 5 modified
```

## Related Examples

- **132_DatabaseDiff** - Schema comparison between databases
- **134_TestFixtures** - Test data management

## Best Practices

1. **Shadow table design**: Mirror the source table structure with a snapshot_id FK
2. **Atomic snapshots**: Copy all records in a single operation for consistency
3. **Non-destructive restore**: Keep shadow data intact after restoring
4. **Change logging**: Track individual field changes for detailed auditing
5. **Metadata tracking**: Record timestamp, description, and record count per snapshot
6. **Comparison queries**: Use JOINs and NOT IN for efficient diff computation
7. **Storage awareness**: Monitor shadow table growth and implement cleanup policies
8. **History navigation**: Use GROUP_CONCAT for compact timeline views
