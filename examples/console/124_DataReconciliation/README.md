# Example 124: Data Reconciliation - Cross-Source Comparison, Discrepancies

## Overview

This example demonstrates a **Data Reconciliation** system that compares data between a source (ERP) and target (Warehouse) to identify discrepancies. It performs key-based matching, column-by-column comparison, severity classification, discrepancy resolution tracking, amount reconciliation, and match score distribution analysis.

## Features Demonstrated

### 1. Data Sources
- Source table (ERP) with 12 order records
- Target table (Warehouse) with 12 order records
- Differences include quantity, price, status, date, and name mismatches
- Source-only and target-only records for missing/extra detection

### 2. Run Reconciliation
- Creates a reconciliation run with metadata
- Compares source vs target by primary key (order_id)
- Identifies exact matches, partial matches, and no-matches
- Records totals: matched, mismatched, source_only, target_only

### 3. Match Results
- Per-record match status: exact_match, partial_match, no_match
- Match score: percentage of matching columns (0-100%)
- Lists mismatched columns for partial matches
- Sorted by score descending for quick identification

### 4. Discrepancy Report
- Field-level mismatch details (source value vs target value)
- Severity classification per discrepancy
- Missing record detection (source_only, target_only)
- Comprehensive cross-reference view

### 5. Severity Analysis
- Four severity levels: critical, high, medium, low
- Critical: quantity and total_amount differences
- High: unit_price differences and missing records
- Medium: status and date differences
- Low: name/label differences

### 6. Resolve Discrepancies
- Resolution statuses: open, resolved, ignored
- Resolution tracking: who resolved and when
- Notes for resolution context
- Resolution rate calculation

### 7. Amount Reconciliation
- Total amount comparison (source vs target)
- Per-record amount differences for matched records
- Aggregate difference calculation
- Identifies largest financial discrepancies

### 8. Match Score Distribution
- Average score by match status category
- Score histogram with visual bars
- Buckets: 100%, 80-99%, 50-79%, 1-49%, 0%
- Quick overview of data alignment quality

### 9. Reconciliation Trend (Simulated)
- Historical reconciliation run comparison
- Match rate tracking over time
- Source/target counts, matched/mismatched counts
- Trend analysis for data quality improvement

### 10. Statistics
- Overall match rate percentage
- Total discrepancies and resolution rate
- Open discrepancies by severity
- Key metrics for reporting

## Architecture

```
+------------------+     +------------------+     +------------------+
| Source (ERP)     |     | Reconciliation   |     | Results          |
+------------------+     +------------------+     +------------------+
| source_orders    |---->| RunReconciliation|---->| match_results    |
| 12 records       |     | Key-based match  |     | discrepancies    |
| 8 columns        |     | Column compare   |     | recon_runs       |
+------------------+     | Severity assign  |     +------------------+
                          +------------------+
+------------------+              |
| Target (Whse)    |              v
+------------------+     +------------------+
| target_orders    |     | Analysis         |
| 12 records       |     +------------------+
| 8 columns        |     | Match scores     |
+------------------+     | Amount recon     |
                          | Trend tracking   |
                          +------------------+
```

## Database Schema

```
+---------------------+     +-------------------+
| reconciliation_runs |     | match_results     |
+---------------------+     +-------------------+
| id (PK, AUTO)       |     | id (PK, AUTO)     |
| run_id (UNIQUE)     |     | run_id            |
| source_table        |     | record_key        |
| target_table        |     | match_status      |
| match_key           |     | match_score       |
| compare_columns     |     | matched_columns   |
| total_source        |     | total_columns     |
| total_target        |     | mismatched_columns|
| matched             |     +-------------------+
| mismatched          |
| source_only         |     +-------------------+
| target_only         |     | discrepancies     |
| status              |     +-------------------+
| started_at          |     | id (PK, AUTO)     |
| completed_at        |     | run_id            |
+---------------------+     | record_key        |
                             | discrepancy_type  |
+---------------------+     | column_name       |
| source_orders       |     | source_value      |
+---------------------+     | target_value      |
| order_id (PK)       |     | severity          |
| customer_name       |     | resolution_status |
| product             |     | resolved_by       |
| quantity            |     | resolved_at       |
| unit_price          |     | notes             |
| total_amount        |     | created_at        |
| order_date          |     +-------------------+
| status              |
+---------------------+
```

## Discrepancy Types

| Type | Description | Example |
|------|-------------|---------|
| mismatch | Column value differs between source and target | quantity: 50 vs 48 |
| source_only | Record exists in source but not target | ORD-011 missing from warehouse |
| target_only | Record exists in target but not source | ORD-013 extra in warehouse |

## Severity Levels

| Level | Trigger Columns | Impact |
|-------|----------------|--------|
| critical | quantity, total_amount | Financial/inventory impact |
| high | unit_price, missing records | Pricing or data gap |
| medium | status, order_date | Operational timing |
| low | customer_name | Cosmetic/label only |

## Compilation

```bash
cd 124_DataReconciliation
lazbuild DataReconciliation.lpi
./DataReconciliation
```

## Sample Output

```
2. Reconciliation: 5 exact matches, 6 mismatches, 1 source-only, 1 target-only
3. Match scores: exact=100%, partial=71-86%, no_match=0%
4. Discrepancies: quantity, amount (critical), price (high), status (medium)
7. Amount difference: 309.00 across matched records
10. Resolution rate: 30%, 7 open discrepancies remaining
```

## Related Examples

- **123_DataProfiling** - Column-level statistics, type inference, pattern detection
- **125_AnomalyDetection** - Statistical outliers, z-score, trend detection

## Best Practices

1. **Key selection**: Choose a unique, immutable key for matching (e.g., order_id)
2. **Severity classification**: Align severity with business impact (financial = critical)
3. **Regular scheduling**: Run reconciliation daily/weekly to catch drift early
4. **Resolution tracking**: Document why discrepancies are resolved or ignored
5. **Amount tolerance**: Consider adding tolerance thresholds for rounding differences
6. **Trend monitoring**: Track match rates over time to detect systemic issues
7. **Source of truth**: Define which system is authoritative for each data element
8. **Audit trail**: Keep historical runs for compliance and debugging
