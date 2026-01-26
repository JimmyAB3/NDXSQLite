# Example 95: Window Functions

## Overview

This example demonstrates **SQLite window functions** - powerful analytical operations that compute values across a set of rows related to the current row, without collapsing the result set like GROUP BY. Covers ROW_NUMBER, RANK, DENSE_RANK, LAG/LEAD, running totals, moving averages, NTILE, FIRST_VALUE/LAST_VALUE, PERCENT_RANK, and CUME_DIST.

## Features Demonstrated

### 1. ROW_NUMBER
- Sequential numbering within partitions
- Partitioned by department, ordered by amount
- No ties - each row gets a unique number

### 2. RANK vs DENSE_RANK
- RANK: Skips numbers after ties (1, 2, 2, 4)
- DENSE_RANK: No gaps after ties (1, 2, 2, 3)
- Multi-subject ranking with PARTITION BY

### 3. LAG / LEAD
- LAG: Access previous row's value
- LEAD: Access next row's value
- Custom offsets (LAG with offset 2)
- NULL handling for boundary rows

### 4. Running Totals (Cumulative SUM)
- SUM() OVER with UNBOUNDED PRECEDING
- Partitioned running totals by department
- Frame: ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW

### 5. Moving Averages
- 3-day moving average (ROWS BETWEEN 2 PRECEDING AND CURRENT ROW)
- 5-day moving average volume
- Sliding window calculations

### 6. NTILE
- Equal-sized bucket distribution
- Performance tiers (top/middle/bottom)
- Quartile analysis of sale amounts

### 7. FIRST_VALUE / LAST_VALUE
- First and last values in the window
- Comparison against period boundaries
- Per-employee first/last sale dates
- Frame: ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING

### 8. Aggregate Window Functions
- SUM/AVG/COUNT/MIN/MAX with OVER clause
- Per-row access to group-level aggregates
- Percentage of department total without subquery
- Running counts with partition totals

### 9. PERCENT_RANK / CUME_DIST
- Percentile ranking (0.0 to 1.0 scale)
- Cumulative distribution function
- Statistical positioning within groups

### 10. Complex Dashboard Example
- CTE combined with window functions
- Week-over-week growth percentages
- Employee performance vs department average
- Multi-function analytical queries

## Database Schema

```
+------------------+     +------------------+     +------------------+
| sales            |     | students         |     | stock_prices     |
+------------------+     +------------------+     +------------------+
| id (PK)          |     | id (PK)          |     | id (PK)          |
| employee         |     | name             |     | symbol           |
| department       |     | subject          |     | trade_date       |
| region           |     | score            |     | close_price      |
| sale_date        |     +------------------+     | volume           |
| amount           |                               +------------------+
| quantity         |
+------------------+
```

## Window Function Syntax

```sql
function_name() OVER (
  [PARTITION BY column1, column2, ...]
  [ORDER BY column3, column4, ...]
  [frame_specification]
)
```

### Frame Specifications Used
```
ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW     -- Running total
ROWS BETWEEN 2 PRECEDING AND CURRENT ROW             -- 3-row moving window
ROWS BETWEEN 4 PRECEDING AND CURRENT ROW             -- 5-row moving window
ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING  -- Entire partition
```

## Key Patterns

### ROW_NUMBER with Partition
```pascal
DS := Conn.ExecuteQuery(
  'SELECT ROW_NUMBER() OVER (PARTITION BY department ORDER BY amount DESC) as row_num,' +
  '  employee, department, amount FROM sales');
```

### Running Total
```pascal
DS := Conn.ExecuteQuery(
  'SELECT sale_date, amount,' +
  '  SUM(amount) OVER (ORDER BY sale_date ' +
  '    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as running_total ' +
  'FROM sales WHERE employee = ''Alice''');
```

### Moving Average
```pascal
DS := Conn.ExecuteQuery(
  'SELECT trade_date, close_price,' +
  '  ROUND(AVG(close_price) OVER (ORDER BY trade_date ' +
  '    ROWS BETWEEN 2 PRECEDING AND CURRENT ROW), 2) as ma_3day ' +
  'FROM stock_prices WHERE symbol = ''ACME''');
```

### LAG / LEAD
```pascal
DS := Conn.ExecuteQuery(
  'SELECT trade_date, close_price,' +
  '  LAG(close_price, 1) OVER (ORDER BY trade_date) as prev_price,' +
  '  LEAD(close_price, 1) OVER (ORDER BY trade_date) as next_price ' +
  'FROM stock_prices');
```

### Percentage of Group Total
```pascal
DS := Conn.ExecuteQuery(
  'SELECT employee, department, amount,' +
  '  ROUND(amount * 100.0 / SUM(amount) OVER (PARTITION BY department), 1) as pct ' +
  'FROM sales');
```

## Compilation

```bash
cd 95_WindowFunctions
lazbuild WindowFunctions.lpi
./WindowFunctions
```

## Sample Output

```
1. ROW_NUMBER - Sequential Numbering
   #1     Clothing    Diana 2024-01-12  $ 1400.00
   #2     Clothing  Charlie 2024-01-19  $ 1100.00
   ...

2. RANK vs DENSE_RANK - Handling Ties
   Name       Score   RANK   DENSE_RANK
      Alice     95      1      1
      Diana     92      2      2
        Bob     87      3      3
    Charlie     87      3      3
        Eve     78      5      4

3. LAG/LEAD - Previous/Next Row Access
   2024-01-09  $152.50  $150.25   2.25  $148.75
   2024-01-10  $148.75  $152.50  -3.75  $153.00

5. Moving Averages
   ACME 3-day moving average price:
   2024-01-10  $148.75  $150.50
   2024-01-11  $153.00  $151.42

10. Complex Example: Sales Performance Dashboard
   Alice    Electronics  $13000.00  #1   1950.00   Above
   Bob      Electronics  $ 9100.00  #2  -1950.00   Below
```

## Related Examples

- **96_CTEAdvanced** - Advanced CTEs (used with window functions)
- **90_FunnelAnalysis** - Analytics with window functions
- **91_CohortAnalysis** - Retention analysis

## Best Practices

1. **PARTITION BY**: Divides rows into groups for independent window calculations
2. **ORDER BY in OVER**: Required for ranking and cumulative functions
3. **Frame clause**: Explicitly specify ROWS BETWEEN for predictable behavior
4. **UNBOUNDED FOLLOWING**: Required for LAST_VALUE to see the entire partition
5. **NULL handling**: LAG/LEAD return NULL at boundaries - use COALESCE if needed
6. **Performance**: Window functions avoid self-joins and correlated subqueries
7. **CAST for AVG**: Use CAST(AVG(...) AS INTEGER) when integer results are needed
8. **Combine with CTE**: Use CTEs to pre-aggregate before applying window functions
