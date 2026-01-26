# Example 113: Data Archiving - Hot/Cold Storage, Partition Rotation, Compressed Archives

## Overview

This example demonstrates **Data Archiving** with tiered hot/cold storage, time-based partition rotation, compressed aggregate archives, configurable retention policies, restore operations, and a full audit trail of archive activities.

## Features Demonstrated

### 1. Hot/Cold Storage
- Active data in `orders_hot` (fast access, indexed)
- Archived data in `orders_cold` (historical, less accessed)
- Atomic move: INSERT INTO cold + DELETE FROM hot in transaction
- Timestamp tracking (archived_at) for lifecycle management

### 2. Partition Rotation (Monthly Tables)
- Time-based table partitioning (orders_2024_01, orders_2024_02, etc.)
- Distribute cold data into monthly partitions
- Drop oldest partition for instant bulk deletion
- No index rebuild or WHERE-clause scanning needed

### 3. Compressed Archives (Aggregated Summaries)
- Aggregate individual records into period/region summaries
- Store: order_count, customer_count, total/avg/min/max amounts
- 100:1 compression ratio (1000 records -> 10 summaries)
- Retain analytical value after raw data is purged

### 4. Archive Policies
- Configurable retention periods per data type
- Hot retention: days before moving to cold
- Cold retention: days before compressing
- Purge threshold: days before permanent deletion
- Active/inactive policy toggle

### 5. Restore from Cold Storage
- Move data back from cold to hot when needed (e.g., audit, reprocessing)
- Atomic restore operation in transaction
- Logged in archive_log for traceability

### 6. Data Purge (Permanent Deletion)
- Remove data beyond retention period
- Compressed archive retains aggregated summaries
- Individual records permanently deleted
- Logged in archive_log

### 7. Archive Log (Audit Trail)
- Every archive operation tracked
- Operation types: hot_to_cold, cold_to_archive, restore, purge
- Records affected count
- Timestamps for compliance

### 8. Archiving Statistics
- Storage tier row counts and totals
- Amount totals per tier
- Space analysis estimates
- Archive efficiency metrics
- Operations summary by type

## Architecture

```
+------------------+     +------------------+     +------------------+
| HOT Storage      |---->| COLD Storage     |---->| COMPRESSED       |
| (orders_hot)     |     | (orders_cold)    |     | (orders_archive) |
+------------------+     +------------------+     +------------------+
| Active data      |     | Archived data    |     | Aggregated       |
| Full access      |     | Queryable        |     | summaries        |
| Indexed          |     | + archived_at    |     | Period + Region  |
| < 30 days        |     | 30-90 days       |     | > 90 days        |
+------------------+     +------------------+     +------------------+
        ^                         |                         |
        |   RESTORE               v                         v
        +-------------------------+                  +------+------+
                                                     | PURGE       |
                                                     | > 365 days  |
                                                     +-------------+

+------------------+     +------------------+
| Partition Tables |     | Archive Log      |
+------------------+     +------------------+
| orders_2024_01   |     | operation        |
| orders_2024_02   |     | source/target    |
| orders_2024_03   |     | records_affected |
| (monthly rotate) |     | executed_at      |
+------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| orders_hot       |     | orders_cold      |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK)          |
| customer         |     | customer         |
| product          |     | product          |
| quantity, amount |     | quantity, amount  |
| status           |     | status           |
| order_date       |     | order_date       |
| region           |     | region           |
+------------------+     | archived_at      |
                          +------------------+

+------------------+     +------------------+
| orders_archive   |     | archive_policies |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| period (YYYY-MM) |     | policy_name      |
| region           |     | source_table     |
| customer_count   |     | hot_retention_d  |
| order_count      |     | cold_retention_d |
| total_quantity   |     | purge_after_days |
| total/avg/min/max|     | is_active        |
| compressed_at    |     +------------------+
+------------------+

+------------------+
| archive_log      |
+------------------+
| id (PK, AUTO)    |
| operation        |
| source_table     |
| target_table     |
| records_affected |
| executed_at      |
+------------------+
```

## Compilation

```bash
cd 113_DataArchiving
lazbuild DataArchiving.lpi
./DataArchiving
```

## Sample Output

```
1. Hot/Cold Storage
   Initial: Hot=2000, Cold=0
   After archival: Hot=1000, Cold=1000

2. Partition Rotation
   orders_2024_01: 500 rows (dropped)
   orders_2024_02: 500 rows (retained)

3. Compressed Archives
   1000 records -> 10 summaries (100:1 ratio)

4. Archive Policies
   orders_monthly: 30d hot, 90d cold, 365d purge

6. Data Purge
   Purged 500 records permanently
   Archive still retains 5 summary entries

8. Statistics
   Hot: 1500 records, Cold: 0, Archive: 10 summaries
```

## Related Examples

- **111_QueryProfiler** - Query performance measurement
- **112_DatabaseMonitoring** - Database health and performance metrics

## Best Practices

1. **Tiered storage**: Use hot/cold/archive tiers to balance access speed vs storage cost
2. **Atomic moves**: Always wrap archive operations in transactions for consistency
3. **Partition by time**: Monthly/quarterly tables enable instant bulk drops without scanning
4. **Compress, don't delete**: Aggregate old data into summaries before purging raw records
5. **Retention policies**: Define clear policies per data type for compliance
6. **Audit trail**: Log every archive/restore/purge operation for compliance and debugging
7. **Restore capability**: Keep cold storage queryable so data can be restored if needed
8. **Archive summaries survive purge**: Compressed archives retain analytical value permanently
