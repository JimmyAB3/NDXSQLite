# Example 125: Anomaly Detection - Z-Score, Moving Average, Thresholds, Trends

## Overview

This example demonstrates a **Statistical Anomaly Detection** system that analyzes time-series metrics (response time, CPU usage, request count) using multiple detection methods: z-score analysis, moving average deviation, static threshold alerts, and trend detection (consecutive patterns, spikes, drops).

## Features Demonstrated

### 1. Time Series Data
- Three metrics: response_time, cpu_usage, request_count
- Summary statistics per metric (count, min, max, avg)
- Visual bar chart of response time series
- Data spans 2 days with hourly granularity

### 2. Z-Score Detection
- Calculates mean and standard deviation per metric
- Identifies values with |z-score| > 2.0 (configurable)
- Automatic severity: z > 3.0 = critical, z > 2.0 = high
- Detects both positive spikes and negative drops

### 3. Moving Average Deviation
- Configurable window size (default 5 data points)
- Calculates rolling average and standard deviation
- Deviation = current value - window average
- Flags values exceeding 2x window stddev

### 4. Threshold Alerts
- Rule-based upper and lower thresholds per metric
- response_time > 200ms = critical
- cpu_usage > 90% = critical
- request_count < 50 = critical
- Stores detected violations in anomalies table

### 5. Trend Detection
- Consecutive increasing/decreasing sequences (3+ points)
- Change percentage calculation
- Spike detection: value > 2x previous value
- Drop detection: value < 0.5x previous value
- Start/end timestamps and values recorded

### 6. Anomaly History
- Comprehensive log of all detected anomalies
- Method, z-score, severity, and timestamp per entry
- Cross-reference between detection methods
- Same value can be flagged by multiple methods

### 7. Multi-Metric Correlation
- Identifies time periods with multiple metric anomalies
- Co-occurrence analysis (e.g., CPU spike + response time spike)
- Per-metric anomaly count and average deviation
- Highlights systemic issues vs isolated events

### 8. Severity Classification
- Four levels: critical, high, medium, low
- Breakdown by severity with detection methods used
- Detail view of critical anomalies
- Expected vs actual value comparison

### 9. Alert Dashboard
- Acknowledgement tracking for alerts
- Pending vs acknowledged counts
- Grouped view by severity and metric
- Operational status overview

### 10. Statistics
- Total data points and anomaly rate
- Detection method comparison (alerts, avg deviation)
- Trend summary (type, count, avg change)
- Overall system health metrics

## Architecture

```
+------------------+     +------------------+     +------------------+
| Time Series      |     | Detection Engine  |     | Results          |
+------------------+     +------------------+     +------------------+
| metrics table    |---->| Z-Score          |---->| anomalies        |
| 42 data points   |     | Moving Average   |     | moving_averages  |
| 3 metrics        |     | Thresholds       |     | trends           |
+------------------+     | Trend Analysis   |     +------------------+
                          +------------------+
                                  |
                  +---------------+---------------+
                  |               |               |
                  v               v               v
          +-------------+  +-------------+  +-------------+
          | Statistical |  | Rule-Based  |  | Pattern     |
          +-------------+  +-------------+  +-------------+
          | mean, stddev|  | upper/lower |  | consecutive |
          | z-score     |  | configurable|  | spike/drop  |
          | window avg  |  | per-metric  |  | 2x threshold|
          +-------------+  +-------------+  +-------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| metrics          |     | anomalies        |     | anomaly_rules    |
+------------------+     +------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |     | id (PK, AUTO)    |
| metric_name      |     | metric_name      |     | metric_name      |
| metric_value     |     | metric_value     |     | rule_type        |
| recorded_at      |     | detection_method |     | threshold_upper  |
| source           |     | expected_value   |     | threshold_lower  |
+------------------+     | deviation        |     | zscore_limit     |
                          | zscore           |     | window_size      |
+------------------+     | severity         |     | severity         |
| moving_averages  |     | acknowledged     |     | enabled          |
+------------------+     | recorded_at      |     +------------------+
| id (PK, AUTO)    |     | detected_at      |
| metric_name      |     +------------------+
| window_size      |
| avg_value        |     +------------------+
| stddev_value     |     | trends           |
| period_end       |     +------------------+
+------------------+     | id (PK, AUTO)    |
                          | metric_name      |
                          | trend_type       |
                          | start_time       |
                          | end_time         |
                          | start_value      |
                          | end_value        |
                          | change_pct       |
                          | data_points      |
                          +------------------+
```

## Detection Methods

| Method | Formula | Use Case |
|--------|---------|----------|
| Z-Score | z = (x - mean) / stddev | Statistical outliers from overall distribution |
| Moving Average | dev = x - window_avg | Deviations from recent behavior |
| Threshold | x > upper or x < lower | Known operational limits |
| Trend | 3+ consecutive increase/decrease | Gradual degradation |
| Spike/Drop | ratio > 2x or < 0.5x | Sudden jumps |

## Severity Levels

| Level | Trigger | Action |
|-------|---------|--------|
| critical | z > 3.0 or threshold breach | Immediate investigation |
| high | z > 2.0 | Review within hour |
| medium | moving avg deviation | Monitor closely |
| low | minor threshold proximity | Log for analysis |

## Sample Metrics

| Metric | Normal Range | Anomaly Examples |
|--------|-------------|-----------------|
| response_time | 50-150 ms | 245.8ms (spike), 350.2ms (spike) |
| cpu_usage | 20-60% | 92.7% (spike), 97.5% (spike) |
| request_count | 100-500 | 42 (drop), 15 (drop) |

## Compilation

```bash
cd 125_AnomalyDetection
lazbuild AnomalyDetection.lpi
./AnomalyDetection
```

## Sample Output

```
2. Z-Score: 5 anomalies (response_time z=3.22, cpu 2.05/2.25, requests -2.01/-2.22)
4. Threshold: 6 critical alerts (response>200, cpu>90%, requests<50)
5. Trends: increasing 172.7% over 7 points, 2 spikes (4.2x, 6.5x)
7. Correlation: 13:00 had 5 concurrent anomalies across all 3 metrics
10. Anomaly rate: 26.2%, 6 trends detected
```

## Related Examples

- **123_DataProfiling** - Column-level statistics, type inference, pattern detection
- **124_DataReconciliation** - Cross-source comparison and discrepancy reporting

## Best Practices

1. **Multiple methods**: Use z-score + threshold + moving average for robust detection
2. **Baseline period**: Establish normal behavior before setting thresholds
3. **Window sizing**: Smaller windows catch fast changes, larger windows reduce noise
4. **Severity tuning**: Align severity levels with operational impact
5. **Correlation**: Multi-metric anomalies often indicate systemic issues
6. **Acknowledgement**: Track alert handling to measure response effectiveness
7. **Trend monitoring**: Gradual trends predict future threshold breaches
8. **False positives**: Tune z-score limits and window sizes to reduce noise
