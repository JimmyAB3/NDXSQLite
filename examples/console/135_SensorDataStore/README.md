# Example 135: Sensor Data Store - High-Frequency Ingestion, Aggregation, Retention

## Overview

This example demonstrates a **High-Frequency Sensor Data Ingestion System** with batch inserts, time-bucketed aggregation tables, auto-aggregation triggers, retention policies, and multi-tier storage. It simulates an IoT telemetry platform that handles thousands of readings per second while maintaining queryable historical data.

## Features Demonstrated

### 1. Sensor Registry
- 5 sensors: 2 temperature, 1 humidity, 1 pressure, 1 light
- Metadata: type, location, unit, range, sample rate
- Across 3 locations (Server Room A/B, Outdoor, Office)

### 2. High-Frequency Batch Ingestion
- 888 raw records ingested across 4 batches
- Transaction-wrapped batch inserts for performance
- 222 records per hourly batch (5 sensors at varying rates)
- 5-second sampling interval

### 3. Raw Data Browsing
- Per-sensor reading display with timestamps
- Quality indicator per reading (95-100)
- Chronological ordering

### 4. Hourly Auto-Aggregation
- 20 hourly buckets computed from raw data
- MIN/MAX/AVG per sensor per hour
- Sample count tracking
- INSERT OR REPLACE for idempotent aggregation

### 5. Daily Aggregation
- Rollup from hourly to daily granularity
- 5 daily summary records (one per sensor)
- Preserves extreme values across hours

### 6. Retention Policy Execution
- 3-tier retention: 24h raw, 30d hourly, 1y daily
- Simulated expiry deletes 444 expired raw records
- Aggregations preserved (longer retention window)

### 7. Time-Range Queries
- Latest reading per sensor
- Hourly trend visualization with ASCII bars
- Temperature progression tracking

### 8. Ingestion Performance
- Batch log with record counts and timing
- Total throughput: ~4500 records/sec
- 195ms total ingestion time for 888 records

### 9. Storage Tier Analysis
- Per-table record counts and estimated size
- 22.2x compression ratio (raw -> hourly)
- Granularity comparison across tiers

### 10. Summary Statistics
- Per-sensor daily min/max/avg
- Data quality overview (min/max/avg quality scores)
- Cross-sensor comparison

## Architecture

```
+------------------+     +------------------+     +------------------+
| Ingestion Layer  |     | Aggregation      |     | Query Layer      |
+------------------+     +------------------+     +------------------+
| Batch inserts    |---->| Hourly rollup    |---->| Time-range       |
| Transaction wrap |     | Daily rollup     |     | Latest readings  |
| Quality tagging  |     | MIN/MAX/AVG      |     | Trend display    |
| Ingestion log    |     | Sample counting  |     | Cross-sensor     |
+------------------+     +------------------+     +------------------+
        |                         |                         |
        v                         v                         v
+------------------+     +------------------+     +------------------+
| readings_raw     |     | readings_hourly  |     | readings_daily   |
+------------------+     +------------------+     +------------------+
| 5-sec granularity|     | 1-hour buckets   |     | 1-day buckets    |
| 24-hour retention|     | 30-day retention |     | 1-year retention |
| Full precision   |     | Statistical      |     | Summary only     |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| sensors          |     | retention_policies|
+------------------+     +------------------+
| sensor_id (UQ)   |     | table_name (UQ)  |
| sensor_type      |     | retention_hours  |
| location         |     | description      |
| unit             |     +------------------+
| min/max_range    |
| sample_rate_hz   |     +------------------+
+------------------+     | ingestion_log    |
        |                 +------------------+
        |                 | batch_id         |
   +----+----+            | sensor_id        |
   |    |    |            | record_count     |
   v    v    v            | ingested_at      |
+------+------+------+   | duration_ms      |
|readings_raw |       |   +------------------+
+-------------+       |
| sensor_id   |       |
| timestamp   |  +----v---------+  +--------v------+
| value       |  |readings_hourly|  |readings_daily |
| quality     |  +---------------+  +---------------+
+-------------+  | sensor_id     |  | sensor_id     |
                 | bucket_hour   |  | bucket_date   |
                 | min/max/avg   |  | min/max/avg   |
                 | sample_count  |  | sample_count  |
                 +---------------+  +---------------+
```

## Storage Tiers

| Tier | Table | Retention | Granularity | Use Case |
|------|-------|-----------|-------------|----------|
| Hot | readings_raw | 24 hours | 5 seconds | Real-time monitoring, alerts |
| Warm | readings_hourly | 30 days | 1 hour | Trend analysis, dashboards |
| Cold | readings_daily | 1 year | 1 day | Capacity planning, reports |

## Compilation

```bash
cd 135_SensorDataStore
lazbuild SensorDataStore.lpi
./SensorDataStore
```

## Sample Output

```
1. Registry: 5 sensors, 3 locations
2. Ingestion: 888 records in 4 batches (transaction-wrapped)
4. Hourly: 20 aggregation records (MIN/MAX/AVG per sensor-hour)
5. Daily: 5 summary records
6. Retention: 888 -> 444 raw records (expired 444)
7. Trend: TEMP-001 rising 22.4 -> 25.0 C over 4 hours
9. Compression: 22.2x reduction (raw -> hourly)
10. Quality: 95-100 across all sensors
```

## Related Examples

- **136_Downsampling** - Data reduction and moving averages
- **137_AlertingRules** - Threshold monitoring and alerts

## Best Practices

1. **Batch inserts**: Wrap high-frequency inserts in transactions for performance
2. **Time bucketing**: Group readings by hour/day for efficient aggregation
3. **Tiered retention**: Keep raw data short-term, aggregations long-term
4. **Idempotent aggregation**: Use INSERT OR REPLACE for re-runnable rollups
5. **Quality tracking**: Record data quality alongside values for filtering
6. **Index strategy**: Index on (sensor_id, timestamp) for time-range queries
7. **Compression ratio**: Monitor raw-to-aggregate compression for capacity planning
8. **Ingestion logging**: Track batch performance for throughput monitoring
