# Example 85: Table Partitioning

## Overview

This example demonstrates **table partitioning patterns** in SQLite, which doesn't have native partitioning support. It implements date-based, range-based, and list-based partitioning using separate tables with UNION ALL views, insert routing, partition pruning, lifecycle management, and performance comparison.

## Features Demonstrated

### 1. Date-Based Partitioning
- Monthly partition tables (events_2024_01 to events_2024_12)
- CHECK constraints for date range validation
- Per-partition indexes
- UNION ALL view across all months

### 2. Range-Based Partitioning
- Amount range partitions (small, medium, large, xlarge, premium)
- CHECK constraints enforce range boundaries
- Insert routing by value

### 3. List Partitioning
- Region-based partitions (europe, americas, asia, africa)
- Category-based routing
- Per-region indexes and views

### 4. Partition Pruning
- Direct partition queries vs view queries
- Performance comparison (1 table vs 12 tables)
- Targeted multi-partition queries

### 5. Partition Lifecycle
- Archive old partitions (RENAME)
- Create new period partitions
- Drop archived data
- Update UNION ALL views

### 6. Cross-Partition Aggregation
- Monthly totals via view
- Regional summaries
- Range distribution statistics

### 7. Performance Comparison
- Single table vs partitioned queries
- Pruning advantage measurement
- Full scan vs targeted access

### 8. Partition Metadata
- Registry table for tracking partitions
- Type, range, status tracking
- Row count maintenance

## Architecture

```
                    +-------------------+
                    |   events_all      |  (VIEW)
                    |   UNION ALL       |
                    +---+-------+---+---+
                        |       |       |
          +-------------+       |       +-------------+
          |                     |                     |
+---------v-------+  +----------v------+  +-----------v-----+
| events_2024_01  |  | events_2024_02  |  | events_2024_12  |
| (Jan partition) |  | (Feb partition) |  | (Dec partition) |
+-----------------+  +-----------------+  +-----------------+
| id (PK)         |  | id (PK)         |  | id (PK)         |
| event_type      |  | event_type      |  | event_type      |
| event_date      |  | event_date      |  | event_date      |
| user_id         |  | user_id         |  | user_id         |
| payload         |  | payload         |  | payload         |
| CHECK(date)     |  | CHECK(date)     |  | CHECK(date)     |
+-----------------+  +-----------------+  +-----------------+
```

## Partitioning Strategies

| Strategy | Partition Key | Use Case | Example |
|----------|--------------|----------|---------|
| Date | Time period | Logs, events, timeseries | events_2024_01 |
| Range | Numeric value | Orders by amount | orders_large |
| List | Category value | Regional data | sales_europe |

## Key Patterns

### Creating Date Partitions
```pascal
for Month := 1 to 12 do
begin
  TableName := Format('events_2024_%s', [Format('%.2d', [Month])]);
  Conn.ExecuteNonQuery(
    'CREATE TABLE ' + TableName + ' (' +
    '  id INTEGER PRIMARY KEY,' +
    '  event_date TEXT NOT NULL,' +
    '  CHECK (event_date >= ''2024-01-01'' AND event_date < ''2024-02-01'')' +
    ')');
  // Create per-partition indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_' + TableName + '_date ON ' + TableName + '(event_date)');
end;
```

### UNION ALL View
```pascal
Conn.ExecuteNonQuery(
  'CREATE VIEW events_all AS ' +
  'SELECT * FROM events_2024_01 UNION ALL ' +
  'SELECT * FROM events_2024_02 UNION ALL ' +
  '... ' +
  'SELECT * FROM events_2024_12');
```

### Insert Routing
```pascal
// Route to correct partition based on date
Month := MonthOf(EventDate);
TableName := Format('events_2024_%s', [Format('%.2d', [Month])]);
Conn.ExecuteNonQuery(
  'INSERT INTO ' + TableName + ' (event_type, event_date) VALUES (?, ?)',
  [EventType, DateStr]);
```

### Partition Pruning
```pascal
// SLOW: Query all 12 partitions via view
DS := Conn.ExecuteQuery('SELECT * FROM events_all WHERE event_date LIKE ''2024-03%''');

// FAST: Query only the relevant partition
DS := Conn.ExecuteQuery('SELECT * FROM events_2024_03');
```

### Partition Lifecycle
```pascal
// Archive old partition
Conn.ExecuteNonQuery('ALTER TABLE events_2024_01 RENAME TO events_2024_01_archived');

// Create new partition
Conn.ExecuteNonQuery('CREATE TABLE events_2025_01 (...)');

// Drop archived data
Conn.ExecuteNonQuery('DROP TABLE events_2024_01_archived');

// Update view (DROP + CREATE)
Conn.ExecuteNonQuery('DROP VIEW events_all');
Conn.ExecuteNonQuery('CREATE VIEW events_all AS ...');
```

## Performance Results

| Test | Single Table | Partitioned | Advantage |
|------|-------------|-------------|-----------|
| Count all | 0.10 ms | 0.10 ms | Equal |
| Single month | 0.10 ms | 0.05 ms | 2x faster (pruning) |
| Group by type | 0.50 ms | 2.45 ms | Single faster (no UNION overhead) |
| Q1 range | 0.15 ms | 0.05 ms | 3x faster (2 partitions) |

**Key insight**: Partitioning excels when queries can be pruned to few partitions. Full-scan aggregations are slower due to UNION ALL overhead.

## Demonstration Sections

1. **Date Partitioning** - 12 monthly tables, 500 events each
2. **Partition Pruning** - View vs direct query performance
3. **Range Partitioning** - 5 amount range tables
4. **List Partitioning** - 4 regional sales tables
5. **Lifecycle** - Archive, create, drop, update view
6. **Aggregation** - Cross-partition queries
7. **Performance** - Single table vs partitioned comparison
8. **Metadata** - Partition registry table
9. **Summary** - All tables and views listed

## When to Partition

### Good Candidates
- Time-series data with date-based retention
- Multi-tenant data with tenant isolation
- Large tables where queries target specific ranges
- Tables requiring periodic archival/deletion

### Poor Candidates
- Small tables (< 10K rows)
- Tables with frequent cross-partition queries
- Randomly accessed data with no partition key
- Tables requiring foreign key references

## Compilation

```bash
cd 85_TablePartitioning
lazbuild TablePartitioning.lpi
./TablePartitioning
```

## Sample Output

```
1. Date-Based Partitioning (Monthly)
   Created 12 monthly tables (events_2024_01 to events_2024_12)
   Inserted 6000 events across 12 partitions

2. Partition Pruning
   View query:     0.80 ms (scans all 12 tables)
   Direct query:   0.10 ms (scans 1 table)

3. Range-Based Partitioning
   orders_small   | 900 | 10-98
   orders_medium  | 900 | 100-498
   orders_large   | 600 | 500-998
   orders_xlarge  | 300 | 1008-3998
   orders_premium | 300 | 5009-7999

5. Partition Lifecycle
   Archived partition has 500 rows
   Created events_2025_01
   Dropped events_2024_01_archived

9. Summary
   Total partition tables: 22
   Union views:            3
   Partition indexes:      29
```

## Related Examples

- **83_QueryPlanAnalysis** - EXPLAIN QUERY PLAN
- **84_DatabaseStatistics** - ANALYZE statistics
- **78_NotificationSystem** - Event-driven patterns

## Considerations

1. **View maintenance**: UNION ALL views must be recreated when partitions change
2. **ID uniqueness**: Partition tables have independent ID sequences
3. **INSERT routing**: Application must determine correct partition
4. **Foreign keys**: Cannot reference partitioned tables via view
5. **VACUUM**: Run per-table after dropping partitions
6. **Transactions**: Cross-partition transactions work normally
7. **Schema changes**: Must ALTER each partition table individually
