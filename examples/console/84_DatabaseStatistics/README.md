# Example 84: Database Statistics

## Overview

This example demonstrates how to use the **ANALYZE** command and **sqlite_stat** tables to understand index effectiveness and guide query optimization. It covers running ANALYZE, reading statistics, interpreting selectivity, composite index analysis, cardinality measurement, and partial re-analysis after data changes.

## Features Demonstrated

### 1. ANALYZE Command
- Full database analysis
- Per-table analysis (ANALYZE tablename)
- Statistics table creation

### 2. sqlite_stat1 Format
- `stat` column interpretation
- Total rows, average per key
- Composite index multi-value format

### 3. Index Selectivity
- Selectivity percentage calculation
- Ranking indexes by effectiveness
- Identifying low-value indexes

### 4. Composite Index Statistics
- Prefix selectivity breakdown
- Column combination analysis
- Multi-level distinct counts

### 5. Query Plan Impact
- Before/after ANALYZE comparison
- Optimizer index selection
- JOIN order optimization

### 6. Cardinality Analysis
- Distinct value counting
- Selectivity ratio computation
- Index candidate identification

### 7. Database Size Metrics
- Page size and count
- Free page tracking
- Total size calculation

## Database Schema

```
+-------------------+     +------------------+
| employees (500)   |     | projects (100)   |
+-------------------+     +------------------+
| id (PK)           |     | id (PK)          |
| name              |     | name             |
| department        |     | department       |
| position          |     | budget           |
| salary            |     | status           |
| hire_date         |     | start_date       |
| city              |     | priority         |
| country           |     +------------------+
| is_active         |
+-------------------+
        |                         |
        v                         v
+--------------------+    +-------------------+
| assignments (1000) |    | timesheets (10000)|
+--------------------+    +-------------------+
| id (PK)            |    | id (PK)           |
| employee_id (FK)   |    | employee_id (FK)  |
| project_id (FK)    |    | project_id (FK)   |
| role               |    | work_date         |
| hours_allocated    |    | hours             |
| start_date         |    | category          |
+--------------------+    +-------------------+
```

## sqlite_stat1 Format

The `stat` column contains space-separated integers:

```
"total_rows avg_per_first_col [avg_per_first_two_cols] [...]"
```

### Single-Column Index
```
stat = "500 100"
  → 500 rows in table
  → 100 rows per distinct key value
  → ~5 distinct values (500/100)
  → Selectivity: 1/100 = 1%
```

### Composite Index (a, b)
```
stat = "500 100 25"
  → 500 rows in table
  → 100 rows per distinct value of column a
  → 25 rows per distinct (a, b) combination
  → ~5 distinct a values, ~20 distinct (a,b) pairs
```

### Composite Index (a, b, c)
```
stat = "10000 20 20 1"
  → 10000 rows in table
  → 20 rows per distinct a
  → 20 rows per distinct (a, b) - same as a alone (b adds no selectivity)
  → 1 row per distinct (a, b, c) - unique combinations
```

## Key Patterns

### Running ANALYZE
```pascal
// Full database
Conn.ExecuteNonQuery('ANALYZE');

// Specific table only
Conn.ExecuteNonQuery('ANALYZE timesheets');
```

### Reading Statistics
```pascal
DS := Conn.ExecuteQuery('SELECT tbl, idx, stat FROM sqlite_stat1');
while not DS.EOF do
begin
  WriteLn(Format('%s.%s: %s',
    [DS.FieldByName('tbl').AsString,
     DS.FieldByName('idx').AsString,
     DS.FieldByName('stat').AsString]));
  DS.Next;
end;
```

### Calculating Selectivity
```pascal
// Parse stat string "500 100"
TotalRows := 500;
AvgPerKey := 100;
Selectivity := 1.0 / AvgPerKey * 100.0; // 1.0%
DistinctKeys := TotalRows div AvgPerKey; // 5
```

### Database Size
```pascal
PageSize := Conn.ExecuteScalar('PRAGMA page_size');
PageCount := Conn.ExecuteScalar('PRAGMA page_count');
FreePages := Conn.ExecuteScalar('PRAGMA freelist_count');
DbSizeKB := (PageSize * PageCount) div 1024;
```

## Selectivity Ranking (Sample Output)

| Rank | Index | Avg/Key | Selectivity |
|------|-------|---------|-------------|
| 1 | idx_assign_emp | 2 | 50.00% |
| 2 | idx_emp_salary | 10 | 10.00% |
| 3 | idx_ts_emp | 20 | 5.00% |
| 4 | idx_proj_dept | 20 | 5.00% |
| 5 | idx_emp_city | 50 | 2.00% |
| ... | idx_ts_category | 2500 | 0.04% |

Higher selectivity = more useful for filtering.

## Demonstration Sections

1. **Schema Creation** - 4 tables with relationships
2. **Test Data** - 500 employees, 100 projects, 10000 timesheets
3. **Index Creation** - 17 indexes (single + composite)
4. **Before ANALYZE** - No statistics available
5. **Run ANALYZE** - Gather and display statistics
6. **Interpret Stats** - Format explanation with examples
7. **After ANALYZE** - Improved query plans
8. **Selectivity Ranking** - All indexes ranked
9. **Database Size** - Page and size metrics
10. **Composite Stats** - Multi-column analysis
11. **Partial ANALYZE** - Re-analyze after data changes
12. **Cardinality** - Distinct value analysis
13. **Summary** - Overall statistics overview

## Compilation

```bash
cd 84_DatabaseStatistics
lazbuild DatabaseStatistics.lpi
./DatabaseStatistics
```

## Sample Output

```
5. Running ANALYZE
   sqlite_stat1 rows: 17
   employees | idx_emp_department | 500 100
   employees | idx_emp_dept_pos   | 500 100 25
   timesheets| idx_ts_emp_date    | 10000 20 1

6. Interpreting Statistics
   employees.idx_emp_department:
     stat = "500 100"
     Total rows: 500, Avg/key: 100, Distinct keys: ~5, Selectivity: 1.0%

   timesheets.idx_ts_emp_proj_date:
     stat = "10000 20 20 1"
     Total rows: 10000, Avg/key: 20, Distinct keys: ~500, Selectivity: 5.0%

9. Database Size Statistics
   Page size:       4096 bytes
   Total pages:     379
   Database size:   1516 KB

12. Cardinality Analysis
    employees | department  | 5 distinct / 500 total = 1.0%
    timesheets| employee_id | 500 distinct / 10500 total = 4.8%
```

## Related Examples

- **83_QueryPlanAnalysis** - EXPLAIN QUERY PLAN
- **85_TablePartitioning** - Partitioning strategies
- **55_Indexes** - Basic index creation

## Best Practices

1. **Run ANALYZE after bulk inserts** - Statistics become stale
2. **Re-analyze specific tables** when only they changed
3. **Check low-selectivity indexes** - They may not help queries
4. **Monitor composite index stats** - Column order matters
5. **Use cardinality** to decide which columns to index
6. **Compare before/after** to verify ANALYZE improves plans
7. **sqlite_stat4** provides sample data (requires compile-time flag)
