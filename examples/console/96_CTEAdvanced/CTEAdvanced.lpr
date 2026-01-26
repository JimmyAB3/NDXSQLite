{===============================================================================
  NDXSQLite Example 96 - CTE Advanced
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Non-recursive and chained CTEs
  - Recursive CTEs for hierarchies and path finding
  - Data generation with recursive CTEs
  - CTEs with INSERT, UPDATE, DELETE statements
  - Multiple CTE joins

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CTEAdvanced;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // Employees with hierarchy
  Conn.ExecuteNonQuery(
    'CREATE TABLE employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  manager_id INTEGER,' +
    '  department TEXT NOT NULL,' +
    '  salary REAL NOT NULL,' +
    '  hire_date TEXT NOT NULL' +
    ')');

  // Orders
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  customer_id INTEGER NOT NULL,' +
    '  order_date TEXT NOT NULL,' +
    '  total REAL NOT NULL,' +
    '  status TEXT NOT NULL' +
    ')');

  // Categories (tree structure)
  Conn.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  parent_id INTEGER' +
    ')');

  // Products
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category_id INTEGER NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL DEFAULT 0' +
    ')');

  // Summary table for CTE with INSERT
  Conn.ExecuteNonQuery(
    'CREATE TABLE monthly_summary (' +
    '  month TEXT NOT NULL,' +
    '  total_orders INTEGER,' +
    '  total_revenue REAL,' +
    '  avg_order REAL' +
    ')');

  // Employee hierarchy
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''CEO Alice'', NULL, ''Executive'', 150000, ''2018-01-15'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (2, ''VP Bob'', 1, ''Engineering'', 120000, ''2018-06-01'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (3, ''VP Carol'', 1, ''Sales'', 115000, ''2019-03-10'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (4, ''Dir Dave'', 2, ''Engineering'', 100000, ''2019-08-20'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (5, ''Dir Eve'', 2, ''Engineering'', 98000, ''2020-01-05'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (6, ''Mgr Frank'', 4, ''Engineering'', 85000, ''2020-06-15'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (7, ''Mgr Grace'', 3, ''Sales'', 82000, ''2020-09-01'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (8, ''Dev Henry'', 6, ''Engineering'', 75000, ''2021-02-10'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (9, ''Dev Ivy'', 6, ''Engineering'', 72000, ''2021-05-20'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (10, ''Sales Jack'', 7, ''Sales'', 65000, ''2021-08-01'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (11, ''Sales Kate'', 7, ''Sales'', 63000, ''2022-01-10'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (12, ''Dev Leo'', 5, ''Engineering'', 70000, ''2022-03-15'')');

  // Categories tree
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (1, ''All Products'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (2, ''Electronics'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (3, ''Clothing'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (4, ''Computers'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (5, ''Phones'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (6, ''Laptops'', 4)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (7, ''Desktops'', 4)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (8, ''Men'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (9, ''Women'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (10, ''Accessories'', 2)');

  // Products
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''MacBook Pro'', 6, 2499.99, 15)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''ThinkPad X1'', 6, 1899.99, 22)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''Gaming Desktop'', 7, 1599.99, 8)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''iPhone 15'', 5, 999.99, 50)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''Galaxy S24'', 5, 899.99, 35)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''USB-C Hub'', 10, 49.99, 200)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''T-Shirt'', 8, 29.99, 150)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock) VALUES (''Dress'', 9, 79.99, 45)');

  // Orders (multiple months)
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (1, ''2024-01-05'', 299.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (2, ''2024-01-12'', 1499.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (1, ''2024-01-18'', 89.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (3, ''2024-01-25'', 549.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (2, ''2024-02-02'', 199.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (4, ''2024-02-10'', 2499.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (1, ''2024-02-15'', 79.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (3, ''2024-02-22'', 649.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (5, ''2024-03-01'', 999.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (2, ''2024-03-08'', 149.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (4, ''2024-03-15'', 349.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (1, ''2024-03-22'', 1899.99, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (5, ''2024-03-28'', 449.99, ''pending'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (3, ''2024-03-30'', 129.99, ''pending'')');
end;

{ Shows department salary statistics and employees above their department average using non-recursive CTEs as named subqueries. }
procedure DemoNonRecursiveCTE;
begin
  WriteLn('=== 1. Non-Recursive CTEs - Simplifying Complex Queries ===');
  WriteLn('');
  WriteLn('   Department salary statistics (CTE as named subquery):');
  WriteLn('   Department     Employees   Avg Salary   Min        Max');
  WriteLn('   ' + StringOfChar('-', 60));

  DS := Conn.ExecuteQuery(
    'WITH dept_stats AS (' +
    '  SELECT department,' +
    '    COUNT(*) as emp_count,' +
    '    CAST(ROUND(AVG(salary), 0) AS INTEGER) as avg_sal,' +
    '    CAST(MIN(salary) AS INTEGER) as min_sal,' +
    '    CAST(MAX(salary) AS INTEGER) as max_sal ' +
    '  FROM employees ' +
    '  GROUP BY department' +
    ')' +
    'SELECT * FROM dept_stats ORDER BY avg_sal DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('department').AsString:14,
        '  ', DS.FieldByName('emp_count').AsInteger:3,
        '       $', DS.FieldByName('avg_sal').AsInteger,
        '  $', DS.FieldByName('min_sal').AsInteger,
        '  $', DS.FieldByName('max_sal').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Employees earning above department average:');

  DS := Conn.ExecuteQuery(
    'WITH dept_avg AS (' +
    '  SELECT department, AVG(salary) as avg_salary ' +
    '  FROM employees GROUP BY department' +
    ')' +
    'SELECT e.name, e.department, CAST(e.salary AS INTEGER) as salary,' +
    '  CAST(ROUND(d.avg_salary, 0) AS INTEGER) as dept_avg,' +
    '  CAST(ROUND(e.salary - d.avg_salary, 0) AS INTEGER) as above_avg ' +
    'FROM employees e ' +
    'JOIN dept_avg d ON e.department = d.department ' +
    'WHERE e.salary > d.avg_salary ' +
    'ORDER BY above_avg DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:12,
        '  ', DS.FieldByName('department').AsString:12,
        '  $', DS.FieldByName('salary').AsInteger,
        '  (avg: $', DS.FieldByName('dept_avg').AsInteger,
        ', +$', DS.FieldByName('above_avg').AsInteger, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Uses three chained CTEs to compute customer order stats, segment customers by spending, and summarize segments. }
procedure DemoChainedCTEs;
begin
  WriteLn('=== 2. Chained CTEs - Multiple Named Subqueries ===');
  WriteLn('');
  WriteLn('   Customer analysis with 3 chained CTEs:');

  DS := Conn.ExecuteQuery(
    'WITH ' +
    '  customer_orders AS (' +
    '    SELECT customer_id, COUNT(*) as order_count, ' +
    '      SUM(total) as total_spent,' +
    '      MIN(order_date) as first_order,' +
    '      MAX(order_date) as last_order ' +
    '    FROM orders GROUP BY customer_id' +
    '  ),' +
    '  customer_segments AS (' +
    '    SELECT customer_id, order_count, total_spent, first_order, last_order,' +
    '      CASE ' +
    '        WHEN total_spent >= 2000 THEN ''VIP'' ' +
    '        WHEN total_spent >= 500 THEN ''Regular'' ' +
    '        ELSE ''New'' ' +
    '      END as segment ' +
    '    FROM customer_orders' +
    '  ),' +
    '  segment_summary AS (' +
    '    SELECT segment, COUNT(*) as customers,' +
    '      CAST(ROUND(AVG(total_spent), 0) AS INTEGER) as avg_spent,' +
    '      CAST(ROUND(SUM(total_spent), 0) AS INTEGER) as segment_total ' +
    '    FROM customer_segments GROUP BY segment' +
    '  )' +
    'SELECT * FROM segment_summary ORDER BY avg_spent DESC');
  try
    WriteLn('   Segment    Customers   Avg Spent   Total');
    WriteLn('   ' + StringOfChar('-', 45));
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('segment').AsString:8,
        '   ', DS.FieldByName('customers').AsInteger:3,
        '        $', DS.FieldByName('avg_spent').AsInteger,
        '  $', DS.FieldByName('segment_total').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Detailed customer segments:');

  DS := Conn.ExecuteQuery(
    'WITH ' +
    '  customer_orders AS (' +
    '    SELECT customer_id, COUNT(*) as order_count, ' +
    '      ROUND(SUM(total), 2) as total_spent,' +
    '      MIN(order_date) as first_order,' +
    '      MAX(order_date) as last_order ' +
    '    FROM orders GROUP BY customer_id' +
    '  ),' +
    '  customer_segments AS (' +
    '    SELECT customer_id, order_count, total_spent, first_order, last_order,' +
    '      CASE ' +
    '        WHEN total_spent >= 2000 THEN ''VIP'' ' +
    '        WHEN total_spent >= 500 THEN ''Regular'' ' +
    '        ELSE ''New'' ' +
    '      END as segment ' +
    '    FROM customer_orders' +
    '  )' +
    'SELECT * FROM customer_segments ORDER BY total_spent DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   Cust #', DS.FieldByName('customer_id').AsInteger,
        ': ', DS.FieldByName('segment').AsString:8,
        ' | orders: ', DS.FieldByName('order_count').AsInteger,
        ' | spent: $', DS.FieldByName('total_spent').AsFloat:0:2,
        ' | ', DS.FieldByName('first_order').AsString, ' to ', DS.FieldByName('last_order').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Traverses the employee hierarchy as an org chart, builds a category tree with depth, and counts products in each category subtree. }
procedure DemoRecursiveCTE;
begin
  WriteLn('=== 3. Recursive CTEs - Hierarchy Traversal ===');
  WriteLn('');
  WriteLn('   Organization chart (recursive tree traversal):');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE org_chart AS (' +
    '  SELECT id, name, manager_id, department, salary, 0 as level,' +
    '    name as path ' +
    '  FROM employees WHERE manager_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT e.id, e.name, e.manager_id, e.department, e.salary, oc.level + 1,' +
    '    oc.path || '' > '' || e.name ' +
    '  FROM employees e ' +
    '  JOIN org_chart oc ON e.manager_id = oc.id' +
    ')' +
    'SELECT * FROM org_chart ORDER BY path');
  try
    while not DS.EOF do
    begin
      Write('   ');
      Write(StringOfChar(' ', DS.FieldByName('level').AsInteger * 3));
      WriteLn(DS.FieldByName('name').AsString,
        ' (', DS.FieldByName('department').AsString,
        ', $', DS.FieldByName('salary').AsFloat:0:0, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Category tree with depth:');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE cat_tree AS (' +
    '  SELECT id, name, parent_id, 0 as depth,' +
    '    CAST(name AS TEXT) as full_path ' +
    '  FROM categories WHERE parent_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT c.id, c.name, c.parent_id, ct.depth + 1,' +
    '    ct.full_path || '' / '' || c.name ' +
    '  FROM categories c ' +
    '  JOIN cat_tree ct ON c.parent_id = ct.id' +
    ')' +
    'SELECT * FROM cat_tree ORDER BY full_path');
  try
    while not DS.EOF do
    begin
      Write('   ');
      Write(StringOfChar(' ', DS.FieldByName('depth').AsInteger * 2));
      WriteLn('[', DS.FieldByName('id').AsInteger, '] ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Subtree product counts:');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE cat_subtree AS (' +
    '  SELECT id, name, parent_id FROM categories WHERE parent_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT c.id, c.name, c.parent_id ' +
    '  FROM categories c JOIN cat_subtree cs ON c.parent_id = cs.id' +
    '),' +
    'cat_products AS (' +
    '  SELECT cs.id as cat_id, cs.name as cat_name, COUNT(p.id) as product_count ' +
    '  FROM cat_subtree cs ' +
    '  LEFT JOIN products p ON p.category_id = cs.id ' +
    '  GROUP BY cs.id, cs.name' +
    ')' +
    'SELECT * FROM cat_products WHERE product_count > 0 ORDER BY product_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('cat_name').AsString:15,
        ': ', DS.FieldByName('product_count').AsInteger, ' products');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Generates number sequences, date series, Fibonacci numbers, and powers of 2 using recursive CTEs. }
procedure DemoDataGeneration;
begin
  WriteLn('=== 4. Data Generation with Recursive CTEs ===');
  WriteLn('');
  WriteLn('   Generate number sequence (1-10):');
  Write('   ');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE nums AS (' +
    '  SELECT 1 as n ' +
    '  UNION ALL ' +
    '  SELECT n + 1 FROM nums WHERE n < 10' +
    ')' +
    'SELECT n FROM nums');
  try
    while not DS.EOF do
    begin
      Write(DS.FieldByName('n').AsInteger:3);
      DS.Next;
    end;
    WriteLn('');
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Generate date series (first 10 days of January 2024):');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE dates AS (' +
    '  SELECT date(''2024-01-01'') as d ' +
    '  UNION ALL ' +
    '  SELECT date(d, ''+1 day'') FROM dates WHERE d < ''2024-01-10''' +
    ')' +
    'SELECT d, ' +
    '  CASE CAST(strftime(''%w'', d) AS INTEGER) ' +
    '    WHEN 0 THEN ''Sun'' WHEN 1 THEN ''Mon'' WHEN 2 THEN ''Tue'' ' +
    '    WHEN 3 THEN ''Wed'' WHEN 4 THEN ''Thu'' WHEN 5 THEN ''Fri'' ' +
    '    WHEN 6 THEN ''Sat'' END as day_name ' +
    'FROM dates');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('d').AsString, ' (', DS.FieldByName('day_name').AsString, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Fibonacci sequence (first 12 numbers):');
  Write('   ');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE fib AS (' +
    '  SELECT 1 as n, 0 as a, 1 as b ' +
    '  UNION ALL ' +
    '  SELECT n + 1, b, a + b FROM fib WHERE n < 12' +
    ')' +
    'SELECT a as fib_num FROM fib');
  try
    while not DS.EOF do
    begin
      Write(DS.FieldByName('fib_num').AsInteger, ' ');
      DS.Next;
    end;
    WriteLn('');
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Powers of 2 (2^0 to 2^10):');
  Write('   ');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE powers AS (' +
    '  SELECT 0 as exp, 1 as val ' +
    '  UNION ALL ' +
    '  SELECT exp + 1, val * 2 FROM powers WHERE exp < 10' +
    ')' +
    'SELECT exp, val FROM powers');
  try
    while not DS.EOF do
    begin
      Write('2^', DS.FieldByName('exp').AsInteger, '=', DS.FieldByName('val').AsInteger, ' ');
      DS.Next;
    end;
    WriteLn('');
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Inserts computed monthly order summaries into the monthly_summary table using a CTE with INSERT. }
procedure DemoCTEWithInsert;
begin
  WriteLn('=== 5. CTEs with INSERT - Computed Data Insertion ===');
  WriteLn('');
  WriteLn('   Inserting monthly summaries using CTE:');

  Conn.ExecuteNonQuery(
    'INSERT INTO monthly_summary (month, total_orders, total_revenue, avg_order) ' +
    'WITH monthly AS (' +
    '  SELECT ' +
    '    strftime(''%Y-%m'', order_date) as month,' +
    '    COUNT(*) as total_orders,' +
    '    ROUND(SUM(total), 2) as total_revenue,' +
    '    ROUND(AVG(total), 2) as avg_order ' +
    '  FROM orders ' +
    '  WHERE status = ''completed'' ' +
    '  GROUP BY strftime(''%Y-%m'', order_date)' +
    ')' +
    'SELECT month, total_orders, total_revenue, avg_order FROM monthly');

  DS := Conn.ExecuteQuery('SELECT * FROM monthly_summary ORDER BY month');
  try
    WriteLn('   Month       Orders   Revenue      Avg Order');
    WriteLn('   ' + StringOfChar('-', 48));
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('month').AsString,
        '     ', DS.FieldByName('total_orders').AsInteger:3,
        '    $', DS.FieldByName('total_revenue').AsFloat:9:2,
        '  $', DS.FieldByName('avg_order').AsFloat:8:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Increases stock by 20% for products with low inventory (stock under 30) using a subquery pattern. }
procedure DemoCTEWithUpdate;
begin
  WriteLn('=== 6. CTEs with UPDATE - Conditional Updates ===');
  WriteLn('');
  WriteLn('   Before update - stock levels:');

  DS := Conn.ExecuteQuery('SELECT name, stock FROM products ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:16, ': ', DS.FieldByName('stock').AsInteger, ' units');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // SQLite doesn't support CTE directly with UPDATE in older versions,
  // so we use a subquery approach that achieves the same logical pattern
  WriteLn('');
  WriteLn('   Applying 20% stock increase for low-stock items (stock < 30):');

  Conn.ExecuteNonQuery(
    'UPDATE products SET stock = stock + CAST(stock * 0.2 AS INTEGER) ' +
    'WHERE id IN (SELECT id FROM products WHERE stock < 30)');

  WriteLn('');
  WriteLn('   After update - stock levels:');

  DS := Conn.ExecuteQuery('SELECT name, stock FROM products ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:16, ': ', DS.FieldByName('stock').AsInteger, ' units');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deletes orders from 2023 (old records) using a CTE within a DELETE subquery to identify target rows. }
procedure DemoCTEWithDelete;
begin
  WriteLn('=== 7. CTEs with DELETE - Targeted Removal ===');
  WriteLn('');

  // First add some test data
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (6, ''2023-06-01'', 25.00, ''completed'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (customer_id, order_date, total, status) VALUES (7, ''2023-07-15'', 30.00, ''completed'')');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM orders');
  try
    WriteLn('   Orders before cleanup: ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('   Deleting orders from 2023 (old records):');

  Conn.ExecuteNonQuery(
    'DELETE FROM orders WHERE id IN (' +
    '  WITH old_orders AS (' +
    '    SELECT id FROM orders WHERE order_date < ''2024-01-01''' +
    '  ) SELECT id FROM old_orders' +
    ')');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM orders');
  try
    WriteLn('   Orders after cleanup: ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Joins four CTEs (managers, team sizes, salary ranks, department budgets) to produce a comprehensive employee report. }
procedure DemoMultipleCTEJoins;
begin
  WriteLn('=== 8. Multiple CTEs with Joins ===');
  WriteLn('');
  WriteLn('   Employees with manager info and team size (4 CTEs joined):');

  DS := Conn.ExecuteQuery(
    'WITH ' +
    '  managers AS (' +
    '    SELECT id, name as mgr_name FROM employees' +
    '  ),' +
    '  team_size AS (' +
    '    SELECT manager_id, COUNT(*) as direct_reports ' +
    '    FROM employees WHERE manager_id IS NOT NULL ' +
    '    GROUP BY manager_id' +
    '  ),' +
    '  salary_rank AS (' +
    '    SELECT id, name, salary, department,' +
    '      RANK() OVER (PARTITION BY department ORDER BY salary DESC) as dept_rank ' +
    '    FROM employees' +
    '  ),' +
    '  dept_budget AS (' +
    '    SELECT department, CAST(SUM(salary) AS INTEGER) as total_budget ' +
    '    FROM employees GROUP BY department' +
    '  )' +
    'SELECT sr.name, sr.department, CAST(sr.salary AS INTEGER) as salary,' +
    '  sr.dept_rank,' +
    '  COALESCE(m.mgr_name, ''(none)'') as reports_to,' +
    '  COALESCE(ts.direct_reports, 0) as team_size,' +
    '  db.total_budget ' +
    'FROM salary_rank sr ' +
    'LEFT JOIN employees e ON sr.id = e.id ' +
    'LEFT JOIN managers m ON e.manager_id = m.id ' +
    'LEFT JOIN team_size ts ON sr.id = ts.manager_id ' +
    'LEFT JOIN dept_budget db ON sr.department = db.department ' +
    'ORDER BY sr.department, sr.dept_rank');
  try
    WriteLn('   Name          Dept          Salary    Rank  Reports To     Team  Dept Budget');
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:12,
        '  ', DS.FieldByName('department').AsString:12,
        '  $', DS.FieldByName('salary').AsInteger:6,
        '   #', DS.FieldByName('dept_rank').AsInteger,
        '  ', DS.FieldByName('reports_to').AsString:14,
        '  ', DS.FieldByName('team_size').AsInteger:2,
        '    $', DS.FieldByName('total_budget').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Builds the complete management chain path from each employee up to the CEO using a recursive CTE. }
procedure DemoRecursivePathFinding;
begin
  WriteLn('=== 9. Recursive CTE - Management Chain ===');
  WriteLn('');
  WriteLn('   Full management chain for each employee:');

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE chain AS (' +
    '  SELECT id, name, manager_id, name as chain_path, 0 as chain_length ' +
    '  FROM employees ' +
    '  UNION ALL ' +
    '  SELECT c.id, c.name, e.manager_id, e.name || '' > '' || c.chain_path, c.chain_length + 1 ' +
    '  FROM chain c ' +
    '  JOIN employees e ON c.manager_id = e.id' +
    ')' +
    'SELECT name, chain_path, chain_length ' +
    'FROM chain ' +
    'WHERE manager_id IS NULL ' +
    'ORDER BY chain_length DESC, name');
  try
    while not DS.EOF do
    begin
      WriteLn('   [depth ', DS.FieldByName('chain_length').AsInteger, '] ',
        DS.FieldByName('chain_path').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Simulates a pivot table showing monthly order counts per customer and calculates month-over-month revenue. }
procedure DemoCTEPivotSimulation;
begin
  WriteLn('=== 10. CTE for Pivot-like Operations ===');
  WriteLn('');
  WriteLn('   Monthly order counts per customer (pivot simulation):');

  DS := Conn.ExecuteQuery(
    'WITH ' +
    '  months AS (' +
    '    SELECT DISTINCT strftime(''%Y-%m'', order_date) as month FROM orders' +
    '  ),' +
    '  customers AS (' +
    '    SELECT DISTINCT customer_id FROM orders' +
    '  ),' +
    '  cross_join AS (' +
    '    SELECT c.customer_id, m.month FROM customers c, months m' +
    '  ),' +
    '  order_counts AS (' +
    '    SELECT customer_id, strftime(''%Y-%m'', order_date) as month, COUNT(*) as cnt ' +
    '    FROM orders GROUP BY customer_id, strftime(''%Y-%m'', order_date)' +
    '  )' +
    'SELECT cj.customer_id,' +
    '  SUM(CASE WHEN cj.month = ''2024-01'' THEN COALESCE(oc.cnt, 0) ELSE 0 END) as jan,' +
    '  SUM(CASE WHEN cj.month = ''2024-02'' THEN COALESCE(oc.cnt, 0) ELSE 0 END) as feb,' +
    '  SUM(CASE WHEN cj.month = ''2024-03'' THEN COALESCE(oc.cnt, 0) ELSE 0 END) as mar ' +
    'FROM cross_join cj ' +
    'LEFT JOIN order_counts oc ON cj.customer_id = oc.customer_id AND cj.month = oc.month ' +
    'GROUP BY cj.customer_id ' +
    'ORDER BY cj.customer_id');
  try
    WriteLn('   Customer   Jan   Feb   Mar');
    WriteLn('   ' + StringOfChar('-', 30));
    while not DS.EOF do
    begin
      WriteLn('   Cust #', DS.FieldByName('customer_id').AsInteger,
        '    ', DS.FieldByName('jan').AsInteger:3,
        '   ', DS.FieldByName('feb').AsInteger:3,
        '   ', DS.FieldByName('mar').AsInteger:3);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Revenue growth analysis (month-over-month):');

  DS := Conn.ExecuteQuery(
    'WITH monthly_rev AS (' +
    '  SELECT strftime(''%Y-%m'', order_date) as month,' +
    '    ROUND(SUM(total), 2) as revenue,' +
    '    COUNT(*) as orders ' +
    '  FROM orders ' +
    '  WHERE status = ''completed'' ' +
    '  GROUP BY strftime(''%Y-%m'', order_date)' +
    ')' +
    'SELECT month, revenue, orders FROM monthly_rev ORDER BY month');
  try
    WriteLn('   Month      Revenue      Orders');
    WriteLn('   ' + StringOfChar('-', 32));
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('month').AsString,
        '  $', DS.FieldByName('revenue').AsFloat:9:2,
        '   ', DS.FieldByName('orders').AsInteger:3);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Prints table row counts and lists all CTE types demonstrated in this example. }
procedure PrintSummary;
var
  EmpCount, CatCount, ProdCount, OrdCount, SumCount: Integer;
begin
  WriteLn('=== CTE Advanced Summary ===');
  WriteLn('');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM employees');
  try EmpCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM categories');
  try CatCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try ProdCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM orders');
  try OrdCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM monthly_summary');
  try SumCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  WriteLn('   Tables used: employees (', EmpCount, '), categories (', CatCount,
    '), products (', ProdCount, '), orders (', OrdCount, '), monthly_summary (', SumCount, ')');
  WriteLn('');
  WriteLn('   CTE types demonstrated:');
  WriteLn('   - Non-recursive: Named subqueries for readability');
  WriteLn('   - Chained: Multiple CTEs in one WITH clause');
  WriteLn('   - Recursive: Hierarchy/tree traversal (org chart, categories)');
  WriteLn('   - Data generation: Number series, date ranges, Fibonacci, powers');
  WriteLn('   - With INSERT: Populate summary tables from computed data');
  WriteLn('   - With UPDATE/DELETE: Conditional modifications');
  WriteLn('   - Multi-join: Complex analytics from multiple CTEs');
  WriteLn('   - Pivot simulation: Cross-tab reporting');
  WriteLn('');
end;

begin
  WriteLn('Example 96: Advanced CTEs (Common Table Expressions)');
  WriteLn(StringOfChar('=', 55));
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    WriteLn('Setting up sample data...');
    SetupDatabase;
    WriteLn('Done.');
    WriteLn('');

    DemoNonRecursiveCTE;
    DemoChainedCTEs;
    DemoRecursiveCTE;
    DemoDataGeneration;
    DemoCTEWithInsert;
    DemoCTEWithUpdate;
    DemoCTEWithDelete;
    DemoMultipleCTEJoins;
    DemoRecursivePathFinding;
    DemoCTEPivotSimulation;
    PrintSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('Done.');
end.
