{===============================================================================
  NDXSQLite Example 38 - Aggregate Functions and Extensions
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - GROUP_CONCAT for string aggregation
  - JSON aggregate functions (json_group_array, json_group_object)
  - Window functions (OVER clause)
  - Aggregate with FILTER
  - Custom ordering in aggregates
  - Subtotals with ROLLUP simulation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program AggregateExtensions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // Create sample data
  Connection.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer TEXT,' +
    '  product TEXT,' +
    '  category TEXT,' +
    '  quantity INTEGER,' +
    '  price REAL,' +
    '  order_date TEXT' +
    ')');

  // Insert sample orders
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (1, ''Alice'', ''Laptop'', ''Electronics'', 1, 999.99, ''2024-01-15'')');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (2, ''Bob'', ''Phone'', ''Electronics'', 2, 599.99, ''2024-01-16'')');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (3, ''Alice'', ''Headphones'', ''Electronics'', 1, 149.99, ''2024-01-17'')');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (4, ''Carol'', ''Book'', ''Books'', 3, 29.99, ''2024-01-18'')');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (5, ''Bob'', ''Mouse'', ''Electronics'', 1, 49.99, ''2024-01-19'')');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (6, ''Alice'', ''Novel'', ''Books'', 2, 19.99, ''2024-01-20'')');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (7, ''Carol'', ''Tablet'', ''Electronics'', 1, 449.99, ''2024-01-21'')');
  Connection.ExecuteNonQuery('INSERT INTO orders VALUES (8, ''Bob'', ''Magazine'', ''Books'', 5, 9.99, ''2024-01-22'')');

  // Create employees table for window functions demo
  Connection.ExecuteNonQuery(
    'CREATE TABLE employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  department TEXT,' +
    '  salary INTEGER,' +
    '  hire_date TEXT' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Alice'', ''Engineering'', 95000, ''2020-01-15'')');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (2, ''Bob'', ''Engineering'', 85000, ''2021-03-01'')');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (3, ''Carol'', ''Sales'', 75000, ''2019-06-10'')');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (4, ''Dave'', ''Engineering'', 90000, ''2020-09-20'')');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (5, ''Eve'', ''Sales'', 80000, ''2021-01-05'')');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (6, ''Frank'', ''Marketing'', 70000, ''2022-02-14'')');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (7, ''Grace'', ''Sales'', 72000, ''2022-04-01'')');
end;

{ Aggregates product names per customer using GROUP_CONCAT with both default comma and custom pipe separators. }
procedure DemoGroupConcat;
var
  DS: TDataSet;
begin
  WriteLn('1. GROUP_CONCAT - String aggregation');
  WriteLn('   ----------------------------------');

  // Basic GROUP_CONCAT
  WriteLn('   Products by customer:');
  DS := Connection.ExecuteQuery(
    'SELECT customer, GROUP_CONCAT(product) AS products ' +
    'FROM orders GROUP BY customer');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('customer').AsString:8, ': ',
              DS.FieldByName('products').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // GROUP_CONCAT with custom separator
  WriteLn('   Products with custom separator ( | ):');
  DS := Connection.ExecuteQuery(
    'SELECT customer, GROUP_CONCAT(product, '' | '') AS products ' +
    'FROM orders GROUP BY customer');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('customer').AsString:8, ': ',
              DS.FieldByName('products').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Uses GROUP_CONCAT with DISTINCT to list unique categories per customer along with their order counts. }
procedure DemoGroupConcatDistinct;
var
  DS: TDataSet;
begin
  WriteLn('2. GROUP_CONCAT with DISTINCT');
  WriteLn('   --------------------------');

  WriteLn('   Unique categories per customer:');
  DS := Connection.ExecuteQuery(
    'SELECT customer, ' +
    '       GROUP_CONCAT(DISTINCT category) AS categories, ' +
    '       COUNT(*) AS order_count ' +
    'FROM orders GROUP BY customer');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('customer').AsString:8, ': ',
              DS.FieldByName('categories').AsString:20,
              ' (', DS.FieldByName('order_count').AsInteger, ' orders)');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Concatenates products per customer ordered by price descending using a pre-sorted subquery. }
procedure DemoGroupConcatOrdered;
var
  DS: TDataSet;
begin
  WriteLn('3. GROUP_CONCAT with ORDER BY (subquery)');
  WriteLn('   -------------------------------------');

  WriteLn('   Products ordered by price (high to low):');
  DS := Connection.ExecuteQuery(
    'SELECT customer, ' +
    '       GROUP_CONCAT(product) AS products_by_price ' +
    'FROM (SELECT * FROM orders ORDER BY customer, price DESC) ' +
    'GROUP BY customer');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('customer').AsString:8, ': ',
              DS.FieldByName('products_by_price').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Aggregates products into JSON arrays per customer, both as simple string arrays and as arrays of JSON objects with product details. }
procedure DemoJsonGroupArray;
var
  DS: TDataSet;
begin
  WriteLn('4. JSON_GROUP_ARRAY - JSON array aggregation');
  WriteLn('   -----------------------------------------');

  WriteLn('   Products as JSON array:');
  DS := Connection.ExecuteQuery(
    'SELECT customer, JSON_GROUP_ARRAY(product) AS products_json ' +
    'FROM orders GROUP BY customer');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('customer').AsString:8, ': ',
              DS.FieldByName('products_json').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Complex JSON array with objects
  WriteLn('   Order details as JSON array of objects:');
  DS := Connection.ExecuteQuery(
    'SELECT customer, ' +
    '       JSON_GROUP_ARRAY(' +
    '         JSON_OBJECT(''product'', product, ''qty'', quantity, ''price'', price)' +
    '       ) AS orders_json ' +
    'FROM orders GROUP BY customer');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('customer').AsString, ':');
      WriteLn('     ', DS.FieldByName('orders_json').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Builds a single JSON object mapping customer names to their total order amounts using JSON_GROUP_OBJECT. }
procedure DemoJsonGroupObject;
var
  DS: TDataSet;
begin
  WriteLn('5. JSON_GROUP_OBJECT - JSON object aggregation');
  WriteLn('   -------------------------------------------');

  WriteLn('   Customer totals as JSON object:');
  DS := Connection.ExecuteQuery(
    'SELECT JSON_GROUP_OBJECT(customer, total) AS totals_json ' +
    'FROM (SELECT customer, ROUND(SUM(quantity * price), 2) AS total ' +
    '      FROM orders GROUP BY customer)');
  try
    if not DS.EOF then
      WriteLn('   ', DS.FieldByName('totals_json').AsString);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Uses the FILTER clause with COUNT and SUM to compute per-customer totals conditionally split by Electronics and Books categories. }
procedure DemoAggregateFilter;
var
  DS: TDataSet;
begin
  WriteLn('6. Aggregate with FILTER clause');
  WriteLn('   ----------------------------');

  WriteLn('   Conditional aggregation:');
  DS := Connection.ExecuteQuery(
    'SELECT customer, ' +
    '       COUNT(*) AS total_orders, ' +
    '       COUNT(*) FILTER (WHERE category = ''Electronics'') AS electronics_orders, ' +
    '       COUNT(*) FILTER (WHERE category = ''Books'') AS book_orders, ' +
    '       ROUND(SUM(quantity * price), 2) AS total_spent, ' +
    '       ROUND(SUM(quantity * price) FILTER (WHERE category = ''Electronics''), 2) AS electronics_spent ' +
    'FROM orders GROUP BY customer');
  try
    WriteLn('   Customer   Total  Elec  Books  Total$    Elec$');
    WriteLn('   --------   -----  ----  -----  ------    -----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('customer').AsString:8,
              DS.FieldByName('total_orders').AsInteger:6,
              DS.FieldByName('electronics_orders').AsInteger:6,
              DS.FieldByName('book_orders').AsInteger:6,
              DS.FieldByName('total_spent').AsFloat:9:2,
              DS.FieldByName('electronics_spent').AsFloat:9:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Queries employees with a running salary total and RANK using window functions over the entire result set ordered by salary descending. }
procedure DemoWindowFunctions;
var
  DS: TDataSet;
begin
  WriteLn('7. Window functions (OVER clause)');
  WriteLn('   ------------------------------');

  WriteLn('   Employees with running total and rank:');
  DS := Connection.ExecuteQuery(
    'SELECT name, department, salary, ' +
    '       SUM(salary) OVER (ORDER BY salary DESC) AS running_total, ' +
    '       RANK() OVER (ORDER BY salary DESC) AS salary_rank ' +
    'FROM employees ORDER BY salary DESC');
  try
    WriteLn('   Name      Dept         Salary    Running    Rank');
    WriteLn('   ----      ----         ------    -------    ----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:8,
              DS.FieldByName('department').AsString:12,
              DS.FieldByName('salary').AsInteger:7,
              DS.FieldByName('running_total').AsInteger:10,
              DS.FieldByName('salary_rank').AsInteger:7);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Computes per-department AVG, MAX salary, and each employee's difference from their department average using PARTITION BY. }
procedure DemoWindowPartition;
var
  DS: TDataSet;
begin
  WriteLn('8. Window functions with PARTITION BY');
  WriteLn('   -----------------------------------');

  WriteLn('   Department statistics:');
  DS := Connection.ExecuteQuery(
    'SELECT name, department, salary, ' +
    '       AVG(salary) OVER (PARTITION BY department) AS dept_avg, ' +
    '       MAX(salary) OVER (PARTITION BY department) AS dept_max, ' +
    '       salary - AVG(salary) OVER (PARTITION BY department) AS diff_from_avg ' +
    'FROM employees ORDER BY department, salary DESC');
  try
    WriteLn('   Name      Dept         Salary  DeptAvg  DeptMax  Diff');
    WriteLn('   ----      ----         ------  -------  -------  ----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:8,
              DS.FieldByName('department').AsString:12,
              DS.FieldByName('salary').AsInteger:7,
              DS.FieldByName('dept_avg').AsFloat:8:0,
              DS.FieldByName('dept_max').AsInteger:8,
              DS.FieldByName('diff_from_avg').AsFloat:7:0);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Ranks employees within each department by salary using ROW_NUMBER, RANK, and DENSE_RANK window functions. }
procedure DemoRowNumber;
var
  DS: TDataSet;
begin
  WriteLn('9. ROW_NUMBER, RANK, DENSE_RANK');
  WriteLn('   ----------------------------');

  WriteLn('   Ranking within departments:');
  DS := Connection.ExecuteQuery(
    'SELECT name, department, salary, ' +
    '       ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) AS row_num, ' +
    '       RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS rank, ' +
    '       DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank ' +
    'FROM employees ORDER BY department, salary DESC');
  try
    WriteLn('   Name      Dept         Salary  Row#  Rank  Dense');
    WriteLn('   ----      ----         ------  ----  ----  -----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:8,
              DS.FieldByName('department').AsString:12,
              DS.FieldByName('salary').AsInteger:7,
              DS.FieldByName('row_num').AsInteger:5,
              DS.FieldByName('rank').AsInteger:5,
              DS.FieldByName('dense_rank').AsInteger:6);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays each order alongside the previous and next product using LAG and LEAD window functions ordered by id. }
procedure DemoLagLead;
var
  DS: TDataSet;
begin
  WriteLn('10. LAG and LEAD functions');
  WriteLn('    ----------------------');

  WriteLn('    Orders with previous and next:');
  DS := Connection.ExecuteQuery(
    'SELECT id, customer, product, ' +
    '       LAG(product, 1, ''(first)'') OVER (ORDER BY id) AS prev_product, ' +
    '       LEAD(product, 1, ''(last)'') OVER (ORDER BY id) AS next_product ' +
    'FROM orders ORDER BY id');
  try
    WriteLn('    ID  Customer  Product       Previous      Next');
    WriteLn('    --  --------  -------       --------      ----');
    while not DS.EOF do
    begin
      WriteLn('    ', DS.FieldByName('id').AsInteger:2,
              '  ', DS.FieldByName('customer').AsString:8,
              '  ', DS.FieldByName('product').AsString:12,
              '  ', DS.FieldByName('prev_product').AsString:12,
              '  ', DS.FieldByName('next_product').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Shows the highest and lowest earner in each department using FIRST_VALUE and LAST_VALUE with an unbounded window frame. }
procedure DemoFirstLastValue;
var
  DS: TDataSet;
begin
  WriteLn('11. FIRST_VALUE and LAST_VALUE');
  WriteLn('    --------------------------');

  WriteLn('    Department salary extremes:');
  DS := Connection.ExecuteQuery(
    'SELECT name, department, salary, ' +
    '       FIRST_VALUE(name) OVER (PARTITION BY department ORDER BY salary DESC) AS top_earner, ' +
    '       LAST_VALUE(name) OVER (PARTITION BY department ORDER BY salary DESC ' +
    '         RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS lowest_earner ' +
    'FROM employees ORDER BY department, salary DESC');
  try
    WriteLn('    Name      Dept         Salary  Top        Lowest');
    WriteLn('    ----      ----         ------  ---        ------');
    while not DS.EOF do
    begin
      WriteLn('    ', DS.FieldByName('name').AsString:8,
              DS.FieldByName('department').AsString:12,
              DS.FieldByName('salary').AsInteger:7,
              '  ', DS.FieldByName('top_earner').AsString:9,
              '  ', DS.FieldByName('lowest_earner').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Simulates ROLLUP behavior using UNION ALL to produce per-category subtotals and a grand total alongside individual rows. }
procedure DemoSubtotalsSimulation;
var
  DS: TDataSet;
begin
  WriteLn('12. Subtotals simulation (ROLLUP-like)');
  WriteLn('    ----------------------------------');

  WriteLn('    Category and customer totals with subtotals:');
  DS := Connection.ExecuteQuery(
    'SELECT category, customer, ROUND(SUM(quantity * price), 2) AS total ' +
    'FROM orders ' +
    'GROUP BY category, customer ' +
    'UNION ALL ' +
    'SELECT category, ''** Subtotal **'', ROUND(SUM(quantity * price), 2) ' +
    'FROM orders ' +
    'GROUP BY category ' +
    'UNION ALL ' +
    'SELECT ''** GRAND TOTAL **'', '''', ROUND(SUM(quantity * price), 2) ' +
    'FROM orders ' +
    'ORDER BY category, customer');
  try
    WriteLn('    Category       Customer        Total');
    WriteLn('    --------       --------        -----');
    while not DS.EOF do
    begin
      WriteLn('    ', DS.FieldByName('category').AsString:15,
              DS.FieldByName('customer').AsString:15,
              DS.FieldByName('total').AsFloat:10:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 38: Aggregate Extensions ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example38.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupDatabase;
      WriteLn('Database setup complete');
      WriteLn('');

      DemoGroupConcat;
      DemoGroupConcatDistinct;
      DemoGroupConcatOrdered;
      DemoJsonGroupArray;
      DemoJsonGroupObject;
      DemoAggregateFilter;
      DemoWindowFunctions;
      DemoWindowPartition;
      DemoRowNumber;
      DemoLagLead;
      DemoFirstLastValue;
      DemoSubtotalsSimulation;

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
