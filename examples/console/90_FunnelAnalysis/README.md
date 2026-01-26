# Example 90: Conversion Funnel Analysis

## Overview

This example demonstrates a comprehensive **conversion funnel analysis system**. It implements multi-step funnel definitions, user event tracking, step-by-step conversion rates, drop-off analysis, segmentation by source/device, time-to-convert metrics, A/B testing comparison, and trend analysis.

## Features Demonstrated

### 1. Funnel Definitions
- Named funnels with ordered steps
- Event-based step matching
- Multiple funnels (signup, purchase, onboarding)

### 2. Step-by-Step Conversion
- User count at each funnel step
- Conversion rate between consecutive steps
- Drop-off percentage per step

### 3. Drop-Off Analysis
- Distribution of where users abandon
- Per-funnel drop-off breakdown
- Identification of highest-friction steps

### 4. Source Segmentation
- Conversion rates by traffic source (organic, paid, social, referral)
- Average step reached per source
- Best-performing acquisition channels

### 5. Device Segmentation
- Conversion rates by device (desktop, mobile, tablet)
- Cross-funnel device comparison

### 6. Time to Convert
- Average, min, max conversion time
- Time distribution buckets (< 5 min, 5-10 min, etc.)
- Per-funnel timing analysis

### 7. A/B Testing
- Variant assignment tracking
- Conversion rate per variant
- Lift calculation (variant vs control)
- Average step and time comparison

### 8. Trend Analysis
- Weekly funnel performance over time
- Period-over-period comparison

## Database Schema

```
+------------------+     +------------------+
| funnels          |     | funnel_steps     |
+------------------+     +------------------+
| id (PK)          |---->| funnel_id (FK)   |
| name (UNIQUE)    |     | step_order       |
| description      |     | step_name        |
| created_at       |     | event_name       |
+------------------+     +------------------+

+------------------+     +---------------------+
| user_events      |     | funnel_conversions  |
+------------------+     +---------------------+
| id (PK)          |     | funnel_id (FK)      |
| user_id          |     | user_id             |
| event_name       |     | step_reached        |
| event_data       |     | completed           |
| source           |     | started_at          |
| device           |     | completed_at        |
| created_at       |     | time_to_convert_sec |
+------------------+     | source              |
                         | device              |
                         +---------------------+

+------------------+
| ab_variants      |
+------------------+
| funnel_id (FK)   |
| variant_name     |
| user_id          |
| assigned_at      |
+------------------+
```

## Funnel Flow

```
Step 1 ──> Step 2 ──> Step 3 ──> Step 4 ──> Step 5
 200        150        81         58         44
         (25% drop) (46% drop) (28% drop) (24% drop)

Overall: 200 → 44 = 22% conversion
```

## Key Patterns

### Define Funnel Steps
```pascal
Conn.ExecuteNonQuery(
  'INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) ' +
  'VALUES (?, ?, ?, ?)',
  [FunnelId, StepOrder, 'Visit Landing', 'page_landing']);
```

### Calculate Step Conversion
```pascal
DS := Conn.ExecuteQuery(
  'SELECT fs.step_order, fs.step_name, ' +
  '  COUNT(DISTINCT ue.user_id) as user_count ' +
  'FROM funnel_steps fs ' +
  'LEFT JOIN user_events ue ON ue.event_name = fs.event_name ' +
  'WHERE fs.funnel_id = ? ' +
  'GROUP BY fs.step_order ORDER BY fs.step_order');
```

### Drop-Off Distribution
```pascal
DS := Conn.ExecuteQuery(
  'SELECT step_reached, COUNT(*) as user_count, ' +
  '  ROUND(COUNT(*) * 100.0 / total, 1) as pct ' +
  'FROM funnel_conversions ' +
  'WHERE completed = 0 GROUP BY step_reached');
```

### A/B Test Comparison
```pascal
DS := Conn.ExecuteQuery(
  'SELECT ab.variant_name, COUNT(*) as total, ' +
  '  SUM(fc.completed) as converted, ' +
  '  ROUND(SUM(fc.completed) * 100.0 / COUNT(*), 1) as conv_rate ' +
  'FROM ab_variants ab JOIN funnel_conversions fc ... ' +
  'GROUP BY ab.variant_name');
```

## Compilation

```bash
cd 90_FunnelAnalysis
lazbuild FunnelAnalysis.lpi
./FunnelAnalysis
```

## Sample Output

```
4. Signup Funnel Analysis
   Step-by-step conversion:
     Step 1: Visit Landing   200 users | conv: 100.0% | drop:   0.0%
     Step 2: Click Signup    150 users | conv:  75.0% | drop:  25.0%
     Step 3: Fill Form        81 users | conv:  54.0% | drop:  46.0%
     Step 4: Submit Form      58 users | conv:  71.6% | drop:  28.4%
     Step 5: Verify Email     44 users | conv:  75.9% | drop:  24.1%
   Overall conversion: 44 / 200 = 22.0%

9. Time to Convert
   Signup funnel (44 conversions):
     Avg time: 678 seconds (11.3 minutes)
     Min time: 435 seconds
     Max time: 950 seconds

10. A/B Test Results (Signup Funnel)
   Variant: control
     Users: 101, Converted: 25 (24.8%)
   Variant: variant_b
     Users: 99, Converted: 19 (19.2%)
   Lift: -5.56% (variant_b vs control)

12. Funnel Summary
   signup: 5 steps, 200 users, 44 converted (22.0%)
   purchase: 5 steps, 150 users, 25 converted (16.7%)
```

## Related Examples

- **89_UserActivityTracking** - User activity and engagement
- **91_CohortAnalysis** - Cohort retention analysis
- **88_APIRateLimiting** - Rate limiting patterns

## Best Practices

1. **Step ordering**: Use ordered integers for deterministic funnel progression
2. **Event matching**: Map funnel steps to specific event names for precise tracking
3. **Drop-off focus**: Highest drop-off steps are the best optimization targets
4. **Source attribution**: Track acquisition source to identify best channels
5. **A/B testing**: Always compare against a control group with sufficient sample size
6. **Time windows**: Analyze funnels in weekly cohorts to detect trend changes
7. **Device analysis**: Mobile vs desktop conversion gaps reveal UX issues
8. **Statistical significance**: Large sample sizes needed for reliable A/B results
