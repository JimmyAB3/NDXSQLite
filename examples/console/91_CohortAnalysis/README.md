# Example 91: Cohort Analysis

## Overview

This example demonstrates a comprehensive **cohort analysis system** using SQLite. It implements user cohorts by signup week, multi-period retention analysis, weekly retention tables, revenue cohorts with cumulative LTV, behavioral cohorts based on feature adoption, acquisition source comparison, churn risk scoring, and lifetime value calculations.

## Features Demonstrated

### 1. Signup Cohorts
- Weekly cohort grouping by signup date
- Cohort size tracking
- Date range per cohort

### 2. Retention Analysis
- Day 1, Day 7, Day 14, Day 30 retention rates
- Per-cohort retention breakdown
- Retention comparison across cohorts

### 3. Weekly Retention Table
- Retention by weeks since signup (W0-W4)
- Classic retention triangle/table format
- Identifies retention curve shape

### 4. Revenue Cohorts
- Total revenue per cohort
- ARPU (Average Revenue Per User)
- Cumulative revenue growth by week
- Paying user identification

### 5. Behavioral Cohorts
- Retention by feature adopted
- Impact of feature count on retention
- Feature adoption as retention predictor

### 6. Acquisition Source Cohorts
- D1/D7 retention by traffic source
- ARPU by acquisition channel
- Best-performing sources identification

### 7. Plan-Based Cohorts
- Retention comparison across plan tiers
- ARPU and feature usage by plan
- Free vs Pro vs Enterprise comparison

### 8. Geographic Cohorts
- Retention and ARPU by country
- Geographic performance comparison

### 9. Churn Risk Analysis
- Activity-based churn status classification
- Active / At Risk / Churning / Churned segments
- Churn rate by plan tier

### 10. Lifetime Value
- LTV per user by cohort
- Transaction frequency analysis
- Cohort revenue maturity tracking

## Database Schema

```
+------------------+     +------------------+
| users            |     | user_activity    |
+------------------+     +------------------+
| id (PK)          |---->| user_id (FK)     |
| username         |     | activity_date    |
| signup_date      |     | activity_type    |
| signup_week      |     +------------------+
| plan             |
| acquisition_src  |     +------------------+
| country          |     | revenue_events   |
+------------------+     +------------------+
        |            ---->| user_id (FK)     |
        |                 | amount           |
        |                 | event_type       |
        |                 | event_date       |
        |                 +------------------+
        |
        +----------->+------------------+
                     | feature_adoption |
                     +------------------+
                     | user_id (FK)     |
                     | feature_name     |
                     | first_used_date  |
                     +------------------+
```

## Retention Table Format

```
Cohort         W0    W1    W2    W3    W4
2026-W48      100%   91%   91%   91%   73%
2026-W49      100%   97%   94%   94%   84%
2026-W50      100%  100%   92%   88%   88%
2026-W51      100%   92%   84%   92%   64%
2026-W52      100%   95%   97%   95%   11%
```

## Key Patterns

### Define Weekly Cohorts
```pascal
Conn.ExecuteNonQuery(
  'INSERT INTO users (username, signup_date, signup_week) ' +
  'VALUES (?, date(''now'', ? || '' days''), ' +
  ' strftime(''%Y-W%W'', date(''now'', ? || '' days'')))',
  [Username, DaysAgo, DaysAgo]);
```

### Multi-Period Retention Query
```pascal
DS := Conn.ExecuteQuery(
  'SELECT signup_week, COUNT(DISTINCT u.id) as cohort_size, ' +
  '  ROUND(COUNT(DISTINCT CASE ' +
  '    WHEN julianday(ua.activity_date) - julianday(u.signup_date) ' +
  '    BETWEEN 6 AND 8 THEN u.id END) * 100.0 / ' +
  '    COUNT(DISTINCT u.id), 1) as d7_retention ' +
  'FROM users u LEFT JOIN user_activity ua ON ua.user_id = u.id ' +
  'GROUP BY signup_week');
```

### Weekly Retention Table
```pascal
// W0, W1, W2... = weeks since signup
'CAST((julianday(activity_date) - julianday(signup_date)) / 7 AS INTEGER) = 1'
// Maps activity to "week 1 after signup"
```

### Cumulative Revenue by Period
```pascal
'SUM(CASE WHEN week_offset <= 2 THEN amount END) as w2_cumulative'
```

### Churn Risk Classification
```pascal
'CASE WHEN days_since_last <= 3 THEN ''Active'' ' +
'     WHEN days_since_last <= 7 THEN ''At Risk'' ' +
'     WHEN days_since_last <= 14 THEN ''Churning'' ' +
'     ELSE ''Churned'' END as status'
```

## Compilation

```bash
cd 91_CohortAnalysis
lazbuild CohortAnalysis.lpi
./CohortAnalysis
```

## Sample Output

```
4. Retention Analysis (by cohort week)
   Cohort         Size     D1     D7     D14     D30
   2026-W48         11  90.9%  72.7%  72.7%  72.7%
   2026-W49         31  67.7%  96.8%  64.5%  80.6%
   2026-W50         25  92.0%  92.0%  76.0%  76.0%

7. Behavioral Cohorts (Feature Adoption)
   Retention by features adopted count:
     0 features: 26 users, 26.9% retained at D14
     1 features: 65 users, 63.1% retained at D14
     2 features: 77 users, 81.8% retained at D14
     4 features: 12 users, 91.7% retained at D14

11. Churn Risk Analysis
     Active (0-3d): 157 users (68.9%)
     At Risk (4-7d): 45 users (19.7%)
     Churning (8-14d): 21 users (9.2%)
     Churned (14d+): 5 users (2.2%)
   Churn rate by plan:
     enterprise: 1.1%
     pro: 2.7%
     free: 6.0%

13. Overall Cohort Metrics
   Total users: 228
   Active (7d): 195 (85.5%)
   Paying users: 139 (61.0%)
   Total revenue: $8335.00
   Overall ARPU: $36.56
```

## Related Examples

- **89_UserActivityTracking** - User activity and engagement
- **90_FunnelAnalysis** - Conversion funnel analysis
- **88_APIRateLimiting** - Rate limiting patterns

## Best Practices

1. **Weekly cohorts**: Group users by signup week for meaningful comparison
2. **Retention windows**: Use ranges (e.g., Day 6-8) rather than exact days for reliability
3. **Behavioral cohorts**: Feature adoption count strongly correlates with retention
4. **Revenue maturity**: Older cohorts generate more cumulative revenue - compare at same age
5. **Churn signals**: "Days since last activity" is the simplest churn predictor
6. **Source analysis**: Compare acquisition channels by retention, not just volume
7. **Plan comparison**: Higher-tier plans typically show better retention
8. **LTV tracking**: Track both total revenue and per-user to normalize for cohort size
