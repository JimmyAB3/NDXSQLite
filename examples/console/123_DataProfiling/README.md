# Example 123: Data Profiling - Statistics, Histograms, Type Inference, Patterns

## Overview

This example demonstrates a **Data Profiling** system that analyzes column-level statistics for any table. It computes null ratios, cardinality, min/max/avg values, standard deviation, string lengths, distribution histograms, automatic type inference (integer, real, date, email, phone, text), and pattern detection (email format, phone format, date format, codes).

## Features Demonstrated

### 1. Column-Level Profiling
- Profiles all columns of a table automatically
- Computes: null count, distinct count, min/max, avg, stddev
- String length statistics (min, max, avg)
- Stores results for later querying

### 2. Null Analysis
- Null count and null ratio per column
- Ranks columns by data completeness
- Identifies columns with missing data
- Percentage-based reporting

### 3. Cardinality Analysis
- Distinct value count per column
- Cardinality ratio (distinct / non-null)
- Identifies high-cardinality (unique) columns
- Identifies low-cardinality (categorical) columns

### 4. Numeric Column Statistics
- Min, Max, Average for numeric columns
- Standard deviation calculation
- Automatic numeric type detection
- Skips non-numeric columns

### 5. String Length Analysis
- Minimum, maximum, average string lengths
- Identifies fixed-length fields (e.g., country codes)
- Detects variable-length text columns
- Useful for schema optimization

### 6. Type Inference
- Automatically detects: integer, real, date, email, phone, text
- Uses SQLite's typeof() for numeric detection
- GLOB patterns for date detection
- LIKE patterns for email/phone detection

### 7. Pattern Detection
- Email format (xxx@yyy.zzz)
- Phone format (+XX-...)
- Date format (yyyy-mm-dd)
- Code format (REF-NNN)
- ISO code format (XX uppercase)
- Match count and percentage

### 8. Distribution Histograms
- Top-N value frequency for categorical columns
- Visual bar charts using # characters
- Percentage of total for each value
- Sorted by frequency descending

### 9. Data Quality Score
- Completeness score (based on null ratio)
- Columns with quality issues flagged
- High null ratio warnings (>10%)
- Low cardinality info (<20%)

### 10. Full Profile Summary
- Combined view of all profiling results
- One row per column with key metrics
- Min/Max truncated for readability
- Inferred types displayed

## Architecture

```
+------------------+     +------------------+     +------------------+
| Sample Data      |     | Profiling Engine  |     | Results Storage  |
+------------------+     +------------------+     +------------------+
| customers table  |---->| ProfileColumn()  |---->| profile_results  |
| 20 rows          |     | InferType()      |     | profile_histogram|
| 11 columns       |     | DetectPatterns() |     | profile_patterns |
+------------------+     | GenerateHisto()  |     +------------------+
                          +------------------+
                                  |
                  +---------------+---------------+
                  |               |               |
                  v               v               v
          +-------------+  +-------------+  +-------------+
          | Null/Card   |  | Type Infer  |  | Patterns    |
          +-------------+  +-------------+  +-------------+
          | COUNT/DISTINCT| | typeof()   |  | GLOB/LIKE   |
          | MIN/MAX/AVG |  | GLOB       |  | Match count |
          | StdDev      |  | LIKE       |  | Percentage  |
          +-------------+  +-------------+  +-------------+
```

## Database Schema

```
+------------------+     +------------------+
| profile_results  |     | profile_histogram|
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| table_name       |     | table_name       |
| column_name      |     | column_name      |
| total_rows       |     | bucket_value     |
| null_count       |     | frequency        |
| null_ratio       |     | percentage       |
| distinct_count   |     +------------------+
| cardinality      |
| min_value        |     +------------------+
| max_value        |     | profile_patterns |
| avg_value        |     +------------------+
| stddev_value     |     | id (PK, AUTO)    |
| min_length       |     | table_name       |
| max_length       |     | column_name      |
| avg_length       |     | pattern          |
| inferred_type    |     | example_value    |
| detected_pattern |     | match_count      |
| profiled_at      |     | match_percentage |
+------------------+     +------------------+
```

## Inferred Types

| Type | Detection Method | Example |
|------|-----------------|---------|
| integer | typeof() = 'integer' | 32, 45, 28 |
| real | typeof() = 'real' | 75000.00 |
| date | GLOB yyyy-mm-dd | 2024-01-15 |
| email | LIKE %@%._%  | alice@email.com |
| phone | LIKE '+%' | +1-555-0101 |
| text | default fallback | New York |

## Compilation

```bash
cd 123_DataProfiling
lazbuild DataProfiling.lpi
./DataProfiling
```

## Sample Output

```
2. Null Analysis: referral_code 45%, email/phone/age 10%, others <5%
3. Cardinality: status 15% (low), country 30%, city 60%, others 100%
4. Numeric: age avg=36.6 std=8.9, salary avg=77947 std=22397
6. Type Inference: email, phone, integer, real, date detected correctly
8. Histograms: US 70%, active 75%, Chicago/NYC 15% each
9. Quality Score: 91.8/100, referral_code flagged (45% null)
```

## Related Examples

- **124_DataReconciliation** - Cross-source comparison and discrepancy reporting
- **125_AnomalyDetection** - Statistical outliers and trend detection

## Best Practices

1. **Profile before ETL**: Run profiling on source data before loading into production
2. **Track over time**: Compare profiles across loads to detect drift
3. **Null thresholds**: Set acceptable null ratios per column based on business rules
4. **Cardinality for indexing**: High-cardinality columns benefit from indexes
5. **Type inference for validation**: Detected types should match expected schema
6. **Pattern detection for quality**: Columns should match expected patterns (emails, phones)
7. **Histograms for outliers**: Unexpected distribution shapes indicate data issues
8. **Quality scoring**: Aggregate metrics into a single score for dashboards
