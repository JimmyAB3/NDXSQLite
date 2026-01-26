# Example 136: Downsampling - Moving Averages, Bucketing, Progressive Detail Loss

## Overview

This example demonstrates **Data Reduction Techniques** for time-series data including moving averages, min/max/avg bucketing at configurable granularities, progressive detail loss analysis, and peak detection across resolution levels. It simulates a monitoring system that trades temporal precision for storage efficiency.

## Features Demonstrated

### 1. Raw Time Series
- 360 data points at 1-second intervals (6 minutes)
- 4 metrics: CPU load, memory %, disk IOPS, network Mbps
- Simulated CPU spike pattern (30% baseline, 87% peak, recovery)
- Network burst pattern (every 60 seconds)

### 2. Moving Averages
- 3-point, 5-point, and 10-point window sizes
- Comparison during spike period shows smoothing effect
- Larger windows delay spike detection
- Subquery-based calculation (no window functions required)

### 3. 10-Second Buckets
- 36 buckets from 360 raw records (10x compression)
- MIN/MAX/AVG preserved per bucket
- Spike clearly visible in bucket averages
- Full sample count per bucket

### 4. 1-Minute Buckets
- 6 buckets covering the full 6-minute window
- All 4 metrics aggregated simultaneously
- Spike concentrated in minute 10:02 (avg=66.4%)
- 60x compression ratio

### 5. 5-Minute Buckets
- 2 buckets (0-5 min, 5-6 min)
- 180x compression ratio
- Spike diluted to avg=39.3% in first bucket
- Maximum granularity loss

### 6. Progressive Detail Loss
- Peak preserved at all levels (86.6%)
- Variance (stddev) decreases: 15.85 -> 15.58 -> 13.33 -> 4.66
- Compression ratios: 10x, 60x, 180x
- Smoothing effect quantified per level

### 7. Peak Detection
- Raw: 50 seconds above 60% threshold
- 10-sec: 5 buckets above threshold (avg-based)
- 1-min: 1 bucket above threshold (avg diluted)
- MAX-based detection preserves 2 spike buckets at 1-min level

### 8. Granularity Configuration
- 4 tiers: 10sec, 1min, 5min, 15min
- Retention: 6h, 24h, 168h (1 week), 720h (30 days)
- Progressive detail vs. storage trade-off

### 9. Compression Statistics
- Raw: 21600 bytes estimated
- 10-sec: 83% savings
- 1-min: 97% savings
- 5-min: 99% savings
- Moving average record counts (near-raw, no compression)

### 10. Visual Comparison
- ASCII bar chart of CPU load at 1-minute granularity
- Min-max range shown with dots, average with hashes
- Spike clearly visible in minute 10:02

## Architecture

```
+------------------+     +------------------+     +------------------+
| Raw Ingestion    |     | Downsampling     |     | Analysis         |
+------------------+     +------------------+     +------------------+
| 1-second metrics |---->| Moving averages  |---->| Detail loss      |
| CPU/Mem/IO/Net   |     | Time bucketing   |     | Peak detection   |
| 360 records      |     | MIN/MAX/AVG      |     | Compression stats|
+------------------+     +------------------+     +------------------+
        |                         |
        v                         v
+------------------+     +------------------+
| raw_metrics      |     | downsampled      |
+------------------+     +------------------+
| 1-sec precision  |     | 10s/1m/5m/15m    |
| Full resolution  |     | Configurable     |
| Short retention  |     | Long retention   |
+------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| raw_metrics      |     | moving_averages  |     | downsampled      |
+------------------+     +------------------+     +------------------+
| timestamp        |     | timestamp        |     | bucket_start     |
| cpu_load         |     | window_size      |     | granularity_sec  |
| memory_pct       |     | cpu_avg          |     | cpu_min/max/avg  |
| disk_iops        |     | memory_avg       |     | mem_min/max/avg  |
| network_mbps     |     +------------------+     | iops_min/max/avg |
+------------------+                               | net_min/max/avg  |
                          +------------------+     | sample_count     |
                          | granularity_config|    +------------------+
                          +------------------+
                          | name (UQ)        |     +------------------+
                          | seconds          |     | detail_loss      |
                          | retention_hours  |     +------------------+
                          | description      |     | granularity_sec  |
                          +------------------+     | metric_name      |
                                                    | raw/bucket_stddev|
                                                    | peak_raw/bucket  |
                                                    | loss_pct         |
                                                    +------------------+
```

## Downsampling Strategy

| Level | Interval | Compression | Peak Loss | Stddev | Use Case |
|-------|----------|-------------|-----------|--------|----------|
| Raw | 1 sec | 1:1 | 0% | 15.85 | Real-time alerts |
| 10-sec | 10 sec | 10:1 | 0% | 15.58 | Short-term dashboard |
| 1-min | 60 sec | 60:1 | 0% | 13.33 | Hourly monitoring |
| 5-min | 300 sec | 180:1 | 0% | 4.66 | Weekly trends |

## Moving Average Windows

```
Window Size   Lag        Smoothing    Best For
---------    -----      ----------   --------
MA-3         1.5 sec    Minimal      Noise reduction
MA-5         2.5 sec    Moderate     Trend following
MA-10        5.0 sec    Aggressive   Baseline tracking
```

## Compilation

```bash
cd 136_Downsampling
lazbuild Downsampling.lpi
./Downsampling
```

## Sample Output

```
1. Raw: 360 points (6 min at 1-sec intervals)
2. Moving Avg: MA-3/5/10 during spike (smooth vs. lag trade-off)
3. 10-sec: 36 buckets, spike visible at 73.9-82.3% avg
4. 1-min: 6 buckets, spike at 66.4% avg
6. Detail Loss: peak preserved, stddev drops 15.85->4.66
7. Peak Detection: 50s raw vs 5 buckets(10s) vs 1 bucket(1m)
9. Compression: 83%/97%/99% savings at 10s/1m/5m
10. Visual: ASCII bars show spike profile
```

## Related Examples

- **135_SensorDataStore** - High-frequency sensor ingestion
- **137_AlertingRules** - Threshold monitoring and alerts

## Best Practices

1. **Preserve extremes**: Always store MIN/MAX alongside AVG in buckets
2. **Configurable granularity**: Let users choose detail vs. storage trade-off
3. **Peak detection**: Use MAX-based thresholds, not AVG, for alerting
4. **Progressive aggregation**: Aggregate from finest to coarsest (raw -> 10s -> 1m -> 5m)
5. **Moving average lag**: Larger windows smooth more but detect changes later
6. **Retention tiers**: Keep fine-grained data shorter, coarse data longer
7. **Variance tracking**: Monitor stddev loss to ensure useful signal preservation
8. **Visual verification**: Use charts to validate downsampled data quality
