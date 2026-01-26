{===============================================================================
  NDXSQLite Example 57 - Reporting Queries
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - OLAP-style analytics and aggregations
  - Pivot tables (cross-tab queries)
  - Rolling aggregates and running totals
  - Window functions (RANK, ROW_NUMBER, etc.)
  - Year-over-year comparisons
  - Dashboard summary queries

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ReportingQueries;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates products, regions, and sales tables, then inserts sample data spanning 2023-2024 across multiple categories and regions. }
procedure SetupSampleData;
begin
  // Products
  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  price REAL NOT NULL' +
    ')');

  // Regions
  Connection.ExecuteNonQuery(
    'CREATE TABLE regions (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  country TEXT NOT NULL' +
    ')');

  // Sales transactions
  Connection.ExecuteNonQuery(
    'CREATE TABLE sales (' +
    '  id INTEGER PRIMARY KEY,' +
    '  product_id INTEGER NOT NULL,' +
    '  region_id INTEGER NOT NULL,' +
    '  sale_date TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  unit_price REAL NOT NULL,' +
    '  discount REAL DEFAULT 0,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id),' +
    '  FOREIGN KEY (region_id) REFERENCES regions(id)' +
    ')');

  // Insert products
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (1, ''Laptop Pro'', ''Electronics'', 1299.99)');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (2, ''Wireless Mouse'', ''Electronics'', 49.99)');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (3, ''Office Chair'', ''Furniture'', 299.99)');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (4, ''Standing Desk'', ''Furniture'', 599.99)');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (5, ''Monitor 27"'', ''Electronics'', 449.99)');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (6, ''Keyboard'', ''Electronics'', 79.99)');

  // Insert regions
  Connection.ExecuteNonQuery('INSERT INTO regions VALUES (1, ''North America'', ''USA'')');
  Connection.ExecuteNonQuery('INSERT INTO regions VALUES (2, ''Europe'', ''UK'')');
  Connection.ExecuteNonQuery('INSERT INTO regions VALUES (3, ''Asia Pacific'', ''Japan'')');

  // Insert sales data (2023)
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (1, 1, 1, ''2023-01-15'', 5, 1299.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (2, 2, 1, ''2023-01-20'', 20, 49.99, 5)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (3, 3, 2, ''2023-02-10'', 8, 299.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (4, 1, 2, ''2023-02-28'', 3, 1299.99, 10)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (5, 4, 1, ''2023-03-05'', 10, 599.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (6, 5, 3, ''2023-03-15'', 15, 449.99, 5)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (7, 2, 2, ''2023-04-01'', 30, 49.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (8, 6, 1, ''2023-04-20'', 25, 79.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (9, 1, 3, ''2023-05-10'', 7, 1299.99, 15)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (10, 3, 1, ''2023-06-15'', 12, 299.99, 0)');

  // Insert sales data (2024)
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (11, 1, 1, ''2024-01-10'', 8, 1299.99, 5)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (12, 2, 1, ''2024-01-25'', 35, 49.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (13, 3, 2, ''2024-02-05'', 10, 299.99, 5)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (14, 1, 2, ''2024-02-20'', 6, 1299.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (15, 4, 1, ''2024-03-01'', 15, 599.99, 10)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (16, 5, 3, ''2024-03-20'', 20, 449.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (17, 2, 3, ''2024-04-05'', 40, 49.99, 5)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (18, 6, 2, ''2024-04-15'', 30, 79.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (19, 1, 1, ''2024-05-20'', 10, 1299.99, 0)');
  Connection.ExecuteNonQuery('INSERT INTO sales VALUES (20, 4, 3, ''2024-06-01'', 8, 599.99, 0)');
end;

{ Runs a GROUP BY query on sales joined with products to display total sales count, units sold, and revenue per product category. }
procedure DemoBasicAggregations;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic Aggregations');
  WriteLn('   --------------------');

  // Total sales by category
  WriteLn('   Sales by Category:');
  DS := Connection.ExecuteQuery(
    'SELECT p.category, ' +
    '       COUNT(*) as num_sales, ' +
    '       SUM(s.quantity) as total_units, ' +
    '       ROUND(SUM(s.quantity * s.unit_price * (1 - s.discount/100)), 2) as revenue ' +
    'FROM sales s ' +
    'JOIN products p ON s.product_id = p.id ' +
    'GROUP BY p.category ' +
    'ORDER BY revenue DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d sales, %d units, $%.2f revenue',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('num_sales').AsInteger,
         DS.FieldByName('total_units').AsInteger,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Runs a cross-tab query using CASE expressions to pivot revenue by region as rows and product category as columns. }
procedure DemoPivotTable;
var
  DS: TDataSet;
begin
  WriteLn('2. Pivot Table (Cross-Tab)');
  WriteLn('   -------------------------');

  // Revenue by region and category (pivot)
  WriteLn('   Revenue by Region and Category:');
  DS := Connection.ExecuteQuery(
    'SELECT r.name as region, ' +
    '       ROUND(SUM(CASE WHEN p.category = ''Electronics'' THEN s.quantity * s.unit_price * (1 - s.discount/100) ELSE 0 END), 2) as Electronics, ' +
    '       ROUND(SUM(CASE WHEN p.category = ''Furniture'' THEN s.quantity * s.unit_price * (1 - s.discount/100) ELSE 0 END), 2) as Furniture, ' +
    '       ROUND(SUM(s.quantity * s.unit_price * (1 - s.discount/100)), 2) as Total ' +
    'FROM sales s ' +
    'JOIN products p ON s.product_id = p.id ' +
    'JOIN regions r ON s.region_id = r.id ' +
    'GROUP BY r.name ' +
    'ORDER BY Total DESC');
  try
    WriteLn('     Region           | Electronics |  Furniture |      Total');
    WriteLn('     -----------------|-------------|------------|------------');
    while not DS.EOF do
    begin
      WriteLn(Format('     %-16s | %11.2f | %10.2f | %10.2f',
        [DS.FieldByName('region').AsString,
         DS.FieldByName('Electronics').AsFloat,
         DS.FieldByName('Furniture').AsFloat,
         DS.FieldByName('Total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Computes a cumulative running total of revenue ordered by date using SUM OVER, and ranks products by total revenue using RANK OVER. }
procedure DemoWindowFunctions;
var
  DS: TDataSet;
begin
  WriteLn('3. Window Functions');
  WriteLn('   ------------------');

  // Running total by date
  WriteLn('   Running Total (Cumulative Revenue by Date):');
  DS := Connection.ExecuteQuery(
    'SELECT sale_date, ' +
    '       ROUND(quantity * unit_price * (1 - discount/100), 2) as daily_revenue, ' +
    '       ROUND(SUM(quantity * unit_price * (1 - discount/100)) OVER (ORDER BY sale_date), 2) as running_total ' +
    'FROM sales ' +
    'ORDER BY sale_date ' +
    'LIMIT 10');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: $%.2f (Running: $%.2f)',
        [DS.FieldByName('sale_date').AsString,
         DS.FieldByName('daily_revenue').AsFloat,
         DS.FieldByName('running_total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Ranking products by revenue
  WriteLn('   Product Revenue Ranking:');
  DS := Connection.ExecuteQuery(
    'SELECT p.name, ' +
    '       ROUND(SUM(s.quantity * s.unit_price * (1 - s.discount/100)), 2) as revenue, ' +
    '       RANK() OVER (ORDER BY SUM(s.quantity * s.unit_price * (1 - s.discount/100)) DESC) as rank ' +
    'FROM sales s ' +
    'JOIN products p ON s.product_id = p.id ' +
    'GROUP BY p.id ' +
    'ORDER BY rank');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     #%d: %s - $%.2f',
        [DS.FieldByName('rank').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Compares monthly revenue between 2023 and 2024 using a CTE with self-join, calculating the percentage growth for each month. }
procedure DemoYearOverYear;
var
  DS: TDataSet;
begin
  WriteLn('4. Year-over-Year Comparison');
  WriteLn('   ---------------------------');

  DS := Connection.ExecuteQuery(
    'WITH yearly_sales AS (' +
    '  SELECT strftime(''%Y'', sale_date) as year, ' +
    '         strftime(''%m'', sale_date) as month, ' +
    '         ROUND(SUM(quantity * unit_price * (1 - discount/100)), 2) as revenue ' +
    '  FROM sales ' +
    '  GROUP BY strftime(''%Y'', sale_date), strftime(''%m'', sale_date) ' +
    ')' +
    'SELECT y24.month, ' +
    '       COALESCE(y23.revenue, 0) as "2023", ' +
    '       COALESCE(y24.revenue, 0) as "2024", ' +
    '       ROUND((y24.revenue - COALESCE(y23.revenue, 0)) / NULLIF(y23.revenue, 0) * 100, 1) as growth_pct ' +
    'FROM yearly_sales y24 ' +
    'LEFT JOIN yearly_sales y23 ON y24.month = y23.month AND y23.year = ''2023'' ' +
    'WHERE y24.year = ''2024'' ' +
    'ORDER BY y24.month');
  try
    WriteLn('     Month |    2023    |    2024    | Growth %');
    WriteLn('     ------|------------|------------|----------');
    while not DS.EOF do
    begin
      WriteLn(Format('        %s | %10.2f | %10.2f | %8s',
        [DS.FieldByName('month').AsString,
         DS.FieldByName('2023').AsFloat,
         DS.FieldByName('2024').AsFloat,
         DS.FieldByName('growth_pct').AsString + '%']));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Calculates a 3-month moving average of monthly revenue using AVG with a ROWS BETWEEN window frame. }
procedure DemoRollingAverages;
var
  DS: TDataSet;
begin
  WriteLn('5. Rolling Averages');
  WriteLn('   ------------------');

  // 3-month moving average
  WriteLn('   3-Month Moving Average Revenue:');
  DS := Connection.ExecuteQuery(
    'WITH monthly AS (' +
    '  SELECT strftime(''%Y-%m'', sale_date) as month, ' +
    '         ROUND(SUM(quantity * unit_price * (1 - discount/100)), 2) as revenue ' +
    '  FROM sales ' +
    '  GROUP BY strftime(''%Y-%m'', sale_date) ' +
    ')' +
    'SELECT month, revenue, ' +
    '       ROUND(AVG(revenue) OVER (ORDER BY month ROWS BETWEEN 2 PRECEDING AND CURRENT ROW), 2) as moving_avg_3m ' +
    'FROM monthly ' +
    'ORDER BY month');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: $%.2f (3-mo avg: $%.2f)',
        [DS.FieldByName('month').AsString,
         DS.FieldByName('revenue').AsFloat,
         DS.FieldByName('moving_avg_3m').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Demonstrates percentile and distribution calculations on sales data. }
procedure DemoPercentiles;
var
  DS: TDataSet;
begin
  WriteLn('6. Percentiles and Distribution');
  WriteLn('   ------------------------------');

  // Revenue percentile per sale
  WriteLn('   Sale Size Distribution:');
  DS := Connection.ExecuteQuery(
    'WITH sale_values AS (' +
    '  SELECT id, ' +
    '         ROUND(quantity * unit_price * (1 - discount/100), 2) as sale_value, ' +
    '         NTILE(4) OVER (ORDER BY quantity * unit_price * (1 - discount/100)) as quartile ' +
    '  FROM sales ' +
    ')' +
    'SELECT ' +
    '  CASE quartile ' +
    '    WHEN 1 THEN ''Q1 (Bottom 25%)'' ' +
    '    WHEN 2 THEN ''Q2 (25-50%)'' ' +
    '    WHEN 3 THEN ''Q3 (50-75%)'' ' +
    '    WHEN 4 THEN ''Q4 (Top 25%)'' ' +
    '  END as quartile_name, ' +
    '  COUNT(*) as num_sales, ' +
    '  ROUND(MIN(sale_value), 2) as min_value, ' +
    '  ROUND(MAX(sale_value), 2) as max_value, ' +
    '  ROUND(AVG(sale_value), 2) as avg_value ' +
    'FROM sale_values ' +
    'GROUP BY quartile ' +
    'ORDER BY quartile');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d sales, $%.2f - $%.2f (avg: $%.2f)',
        [DS.FieldByName('quartile_name').AsString,
         DS.FieldByName('num_sales').AsInteger,
         DS.FieldByName('min_value').AsFloat,
         DS.FieldByName('max_value').AsFloat,
         DS.FieldByName('avg_value').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries key performance indicators in a single statement: total sales, units, revenue, average order value, top product, and top region. }
procedure DemoDashboardSummary;
var
  DS: TDataSet;
begin
  WriteLn('7. Dashboard Summary');
  WriteLn('   -------------------');

  // KPI summary
  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM sales) as total_sales, ' +
    '  (SELECT SUM(quantity) FROM sales) as total_units, ' +
    '  (SELECT ROUND(SUM(quantity * unit_price * (1 - discount/100)), 2) FROM sales) as total_revenue, ' +
    '  (SELECT ROUND(AVG(quantity * unit_price * (1 - discount/100)), 2) FROM sales) as avg_order_value, ' +
    '  (SELECT name FROM products p JOIN sales s ON p.id = s.product_id ' +
    '   GROUP BY p.id ORDER BY SUM(s.quantity) DESC LIMIT 1) as top_product, ' +
    '  (SELECT name FROM regions r JOIN sales s ON r.id = s.region_id ' +
    '   GROUP BY r.id ORDER BY SUM(s.quantity * s.unit_price) DESC LIMIT 1) as top_region');
  try
    WriteLn('   Key Performance Indicators:');
    WriteLn(Format('     Total Sales: %d transactions', [DS.FieldByName('total_sales').AsInteger]));
    WriteLn(Format('     Total Units Sold: %d', [DS.FieldByName('total_units').AsInteger]));
    WriteLn(Format('     Total Revenue: $%.2f', [DS.FieldByName('total_revenue').AsFloat]));
    WriteLn(Format('     Average Order Value: $%.2f', [DS.FieldByName('avg_order_value').AsFloat]));
    WriteLn(Format('     Top Product: %s', [DS.FieldByName('top_product').AsString]));
    WriteLn(Format('     Top Region: %s', [DS.FieldByName('top_region').AsString]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Uses ROW_NUMBER partitioned by region to find the top 3 revenue-generating products within each sales region. }
procedure DemoTopNAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('8. Top-N Analysis');
  WriteLn('   ----------------');

  // Top 3 products by region
  WriteLn('   Top 3 Products per Region:');
  DS := Connection.ExecuteQuery(
    'WITH ranked AS (' +
    '  SELECT r.name as region, p.name as product, ' +
    '         ROUND(SUM(s.quantity * s.unit_price * (1 - s.discount/100)), 2) as revenue, ' +
    '         ROW_NUMBER() OVER (PARTITION BY r.id ORDER BY SUM(s.quantity * s.unit_price * (1 - s.discount/100)) DESC) as rn ' +
    '  FROM sales s ' +
    '  JOIN products p ON s.product_id = p.id ' +
    '  JOIN regions r ON s.region_id = r.id ' +
    '  GROUP BY r.id, p.id ' +
    ')' +
    'SELECT region, product, revenue ' +
    'FROM ranked ' +
    'WHERE rn <= 3 ' +
    'ORDER BY region, revenue DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %s - $%.2f',
        [DS.FieldByName('region').AsString,
         DS.FieldByName('product').AsString,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints a summary of reporting query best practices including CTEs, indexing, window functions, pre-aggregation, and caching strategies. }
procedure DemoBestPractices;
begin
  WriteLn('9. Reporting Query Best Practices');
  WriteLn('   --------------------------------');
  WriteLn('   - Use CTEs (WITH clause) for complex queries');
  WriteLn('   - Create indexes on GROUP BY and ORDER BY columns');
  WriteLn('   - Use window functions for running calculations');
  WriteLn('   - Pre-aggregate data for large datasets');
  WriteLn('   - Use EXPLAIN QUERY PLAN to optimize');
  WriteLn('   - Consider materialized views for dashboards');
  WriteLn('   - Cache frequently-used reports');
  WriteLn('   - Partition data by date for time-series analysis');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 57: Reporting Queries ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example57.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupSampleData;
      DemoBasicAggregations;
      DemoPivotTable;
      DemoWindowFunctions;
      DemoYearOverYear;
      DemoRollingAverages;
      DemoPercentiles;
      DemoDashboardSummary;
      DemoTopNAnalysis;
      DemoBestPractices;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
