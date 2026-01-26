# Example 106: Data Lineage - Origin Tracking, Transformation Chain, Impact Analysis

## Overview

This example demonstrates a complete **data lineage system** with data source registration, column-level lineage tracking, transformation chains, recursive forward/backward traversal, impact analysis, and data freshness monitoring.

## Features Demonstrated

### 1. Data Sources & Datasets
- Multiple source types: database, API, CSV, manual
- Dataset types: raw, cleaned, derived, aggregated
- Row counts and timestamps per dataset

### 2. Transformation Chain
- Named transformations with types (clean, join, aggregate, derive)
- Creator tracking and descriptions
- Edge counts per transformation

### 3. Column-Level Lineage
- Track data flow at the column granularity
- Edge types: direct (pass-through), derived (computed), aggregated (grouped)
- Source dataset and transformation references

### 4. Forward Lineage (Downstream)
- Recursive CTE to traverse downstream dependencies
- Multi-hop traversal with depth tracking
- Answer: "What datasets use this source column?"

### 5. Backward Lineage (Origin Tracking)
- Recursive CTE to trace data origins
- From any derived column back to raw sources
- Answer: "Where does this data come from?"

### 6. Impact Analysis
- Source-level impact: "If this system goes down, what breaks?"
- Transitive dependency resolution
- Dataset-level impact summary with row counts

### 7. Lineage Events (Audit Trail)
- Event types: ingested, transformed, validated, published
- Chronological pipeline execution log
- Details per event for debugging

### 8. Data Freshness
- Hours since last update per dataset
- Staleness detection across the pipeline
- Identifies bottlenecks in refresh schedules

### 9. Statistics
- Counts: sources, datasets, columns, transformations, edges, events
- Datasets grouped by type with row totals
- Edge type distribution

## Database Schema

```
+------------------+     +------------------+     +------------------+
| data_sources     |     | datasets         |     | dataset_columns  |
+------------------+     +------------------+     +------------------+
| id (PK)          |<--->| id (PK)          |<--->| id (PK)          |
| name             |     | name             |     | dataset_id (FK)  |
| source_type      |     | source_id (FK)   |     | column_name      |
| uri              |     | dataset_type     |     | data_type        |
| description      |     | row_count        |     | description      |
+------------------+     | created_at       |     +------------------+
                          | updated_at       |
+------------------+     +------------------+     +------------------+
| transformations  |                               | lineage_edges    |
+------------------+                               +------------------+
| id (PK)          |                               | id (PK, AUTO)    |
| name             |<----------------------------->| transformation_id|
| transform_type   |                               | source_column_id |
| description      |                               | target_column_id |
| created_at       |                               | edge_type        |
| created_by       |                               +------------------+
+------------------+
                          +------------------+
                          | lineage_events   |
                          +------------------+
                          | id (PK, AUTO)    |
                          | dataset_id (FK)  |
                          | event_type       |
                          | details          |
                          | occurred_at      |
                          +------------------+
```

## Compilation

```bash
cd 106_DataLineage
lazbuild DataLineage.lpi
./DataLineage
```

## Sample Output

```
3. Column-Level Lineage
   customer_id     <- customer_id     [Cleaned Customers] via "Join Customer 360" (direct)
   total_revenue   <- total_amount    [Cleaned Orders] via "Join Customer 360" (aggregated)

4. Forward Lineage: What depends on "Raw Customers"?
   [depth 1] Cleaned Customers.customer_id
   [depth 2] Customer 360 View.customer_id

6. Impact Analysis: If "CRM Database" goes down, what is affected?
   AFFECTED: Cleaned Customers      [cleaned]  48500 rows
   AFFECTED: Customer 360 View      [derived]  48500 rows
```

## Related Examples

- **105_DataDeduplication** - Data quality and deduplication
- **107_SchemaEvolution** - Database schema versioning

## Best Practices

1. **Column-level tracking**: More precise than dataset-level for impact analysis
2. **Recursive CTEs**: SQLite supports WITH RECURSIVE for multi-hop traversal
3. **Edge types**: Distinguish direct pass-through from derived/aggregated transformations
4. **Freshness monitoring**: Track updated_at to detect stale data in pipelines
5. **Event logging**: Record every pipeline step for debugging and compliance
6. **Impact analysis**: Run before making source changes to understand blast radius
7. **Bidirectional traversal**: Support both forward (downstream) and backward (origin) queries
