# Example 48: Time Series Database

This example demonstrates how to implement time-series data storage and analysis using SQLite with NDXSQLite.

## Features Demonstrated

- **Time-Series Storage**: Efficient sensor/metrics data storage
- **Time-Range Queries**: Fast queries over time periods
- **Downsampling**: Hourly and daily aggregation
- **Moving Averages**: Statistical smoothing
- **Anomaly Detection**: Identify outliers using standard deviation
- **Retention Policies**: Automatic data lifecycle management
- **Sensor Comparison**: Cross-sensor analysis

## Database Schema

### Tables

1. **metrics**: High-resolution raw data
   - Sensor ID and metric name
   - Value with unit
   - Timestamp
   - Optional JSON tags

2. **metrics_hourly**: Hourly rollups
   - Min, max, avg, sum, count per hour

3. **metrics_daily**: Daily rollups
   - Min, max, avg, sum, count per day

4. **retention_policies**: Data lifecycle rules
   - Different retention per table
   - Last cleanup timestamp

5. **alert_rules**: Threshold definitions

6. **alert_events**: Triggered alerts

## Key Operations

### Record Metric
```pascal
RecordMetric('sensor_001', 'temperature', 23.5, 'celsius');
RecordMetric('sensor_001', 'humidity', 45.0, 'percent');
```

### Time-Range Query
```sql
SELECT sensor_id, AVG(value) as avg_temp
FROM metrics
WHERE metric_name = 'temperature'
  AND timestamp >= datetime('now', '-1 hour')
GROUP BY sensor_id
```

### Downsampling (Hourly Aggregation)
```sql
INSERT INTO metrics_hourly
SELECT sensor_id, metric_name,
       strftime('%Y-%m-%d %H:00:00', timestamp) as hour,
       MIN(value), MAX(value), AVG(value), SUM(value), COUNT(*)
FROM metrics
GROUP BY sensor_id, metric_name, hour
```

### Moving Average
```sql
SELECT timestamp, value,
       AVG(value) OVER (
         ORDER BY timestamp
         ROWS BETWEEN 2 PRECEDING AND 2 FOLLOWING
       ) as moving_avg_5
FROM metrics
```

### Anomaly Detection
Identifies values outside 2 standard deviations:
```sql
WITH stats AS (
  SELECT AVG(value) as avg_val,
         SQRT(AVG(value*value) - AVG(value)*AVG(value)) as std_dev
  FROM metrics WHERE ...
)
SELECT * FROM metrics, stats
WHERE value > avg_val + 2*std_dev
   OR value < avg_val - 2*std_dev
```

## Retention Policies

Default policies:
- Raw metrics: 7 days
- Hourly aggregates: 30 days
- Daily aggregates: 365 days

## Sample Data Generation

The example generates realistic sensor data:
- 3 sensors
- Temperature, humidity, pressure
- 24 hours of data at 5-minute intervals
- Daily variation patterns
- Random noise

## Build and Run

```bash
cd 48_TimeSeries
fpc TimeSeries.lpr
./TimeSeries
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Use Cases

- IoT sensor data
- Server metrics monitoring
- Application performance tracking
- Environmental monitoring
- Energy consumption tracking
- Financial tick data
- Industrial process monitoring
