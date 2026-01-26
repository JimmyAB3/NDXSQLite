# Example 57: Reporting Queries

This example demonstrates OLAP-style analytics and business intelligence queries with NDXSQLite.

## Features Demonstrated

- **Basic Aggregations**: COUNT, SUM, AVG, GROUP BY
- **Pivot Tables**: Cross-tab queries with CASE
- **Window Functions**: RANK, ROW_NUMBER, NTILE
- **Running Totals**: Cumulative calculations
- **Year-over-Year**: Period comparisons
- **Rolling Averages**: Moving window calculations
- **Dashboard Summaries**: KPI queries

## Database Schema

```sql
CREATE TABLE products (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  category TEXT NOT NULL,
  price REAL NOT NULL
);

CREATE TABLE regions (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  country TEXT NOT NULL
);

CREATE TABLE sales (
  id INTEGER PRIMARY KEY,
  product_id INTEGER NOT NULL,
  region_id INTEGER NOT NULL,
  sale_date TEXT NOT NULL,
  quantity INTEGER NOT NULL,
  unit_price REAL NOT NULL,
  discount REAL DEFAULT 0
);
```

## Query Examples

### Basic Aggregation
```sql
SELECT p.category,
       COUNT(*) as num_sales,
       SUM(s.quantity) as total_units,
       ROUND(SUM(s.quantity * s.unit_price * (1 - s.discount/100)), 2) as revenue
FROM sales s
JOIN products p ON s.product_id = p.id
GROUP BY p.category
ORDER BY revenue DESC;
```

### Pivot Table (Cross-Tab)
```sql
SELECT r.name as region,
       SUM(CASE WHEN p.category = 'Electronics'
           THEN s.quantity * s.unit_price ELSE 0 END) as Electronics,
       SUM(CASE WHEN p.category = 'Furniture'
           THEN s.quantity * s.unit_price ELSE 0 END) as Furniture,
       SUM(s.quantity * s.unit_price) as Total
FROM sales s
JOIN products p ON s.product_id = p.id
JOIN regions r ON s.region_id = r.id
GROUP BY r.name;
```

### Running Total (Window Function)
```sql
SELECT sale_date,
       quantity * unit_price as daily_revenue,
       SUM(quantity * unit_price) OVER (ORDER BY sale_date) as running_total
FROM sales
ORDER BY sale_date;
```

### Product Ranking
```sql
SELECT p.name,
       SUM(s.quantity * s.unit_price) as revenue,
       RANK() OVER (ORDER BY SUM(s.quantity * s.unit_price) DESC) as rank
FROM sales s
JOIN products p ON s.product_id = p.id
GROUP BY p.id
ORDER BY rank;
```

### Year-over-Year Comparison
```sql
WITH yearly_sales AS (
  SELECT strftime('%Y', sale_date) as year,
         strftime('%m', sale_date) as month,
         SUM(quantity * unit_price) as revenue
  FROM sales
  GROUP BY year, month
)
SELECT y24.month,
       y23.revenue as "2023",
       y24.revenue as "2024",
       ROUND((y24.revenue - y23.revenue) / y23.revenue * 100, 1) as growth_pct
FROM yearly_sales y24
LEFT JOIN yearly_sales y23 ON y24.month = y23.month AND y23.year = '2023'
WHERE y24.year = '2024';
```

### Rolling 3-Month Average
```sql
WITH monthly AS (
  SELECT strftime('%Y-%m', sale_date) as month,
         SUM(quantity * unit_price) as revenue
  FROM sales
  GROUP BY month
)
SELECT month, revenue,
       AVG(revenue) OVER (ORDER BY month ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) as moving_avg
FROM monthly;
```

### Quartile Distribution
```sql
WITH sale_values AS (
  SELECT quantity * unit_price as sale_value,
         NTILE(4) OVER (ORDER BY quantity * unit_price) as quartile
  FROM sales
)
SELECT quartile,
       COUNT(*) as num_sales,
       MIN(sale_value) as min_value,
       MAX(sale_value) as max_value,
       AVG(sale_value) as avg_value
FROM sale_values
GROUP BY quartile;
```

### Dashboard KPIs
```sql
SELECT
  (SELECT COUNT(*) FROM sales) as total_sales,
  (SELECT SUM(quantity) FROM sales) as total_units,
  (SELECT SUM(quantity * unit_price) FROM sales) as total_revenue,
  (SELECT AVG(quantity * unit_price) FROM sales) as avg_order_value,
  (SELECT name FROM products p JOIN sales s ON p.id = s.product_id
   GROUP BY p.id ORDER BY SUM(s.quantity) DESC LIMIT 1) as top_product;
```

### Top-N per Region
```sql
WITH ranked AS (
  SELECT r.name as region, p.name as product,
         SUM(s.quantity * s.unit_price) as revenue,
         ROW_NUMBER() OVER (PARTITION BY r.id ORDER BY SUM(s.quantity * s.unit_price) DESC) as rn
  FROM sales s
  JOIN products p ON s.product_id = p.id
  JOIN regions r ON s.region_id = r.id
  GROUP BY r.id, p.id
)
SELECT region, product, revenue
FROM ranked
WHERE rn <= 3;
```

## Build and Run

```bash
cd 57_ReportingQueries
fpc ReportingQueries.lpr
./ReportingQueries
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use CTEs (WITH clause) for complex, readable queries
- Create indexes on GROUP BY and ORDER BY columns
- Use window functions for running calculations
- Pre-aggregate data for large datasets (summary tables)
- Use EXPLAIN QUERY PLAN to optimize slow queries
- Consider materialized views for dashboards
- Cache frequently-used reports
- Partition data by date for time-series analysis
