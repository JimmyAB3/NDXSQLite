{===============================================================================
  NDXSQLite Example 22 - Advanced SQL
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Common Table Expressions (CTE)
  - Recursive CTEs
  - Window functions
  - Set operations (UNION, INTERSECT, EXCEPT)

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program AdvancedSQL;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  // Employees with hierarchy
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  department TEXT,' +
    '  salary REAL,' +
    '  manager_id INTEGER REFERENCES employees(id)' +
    ')');

  // Insert sample data with hierarchy
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (1, ''CEO Alice'', ''Executive'', 200000, NULL)');
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (2, ''VP Bob'', ''Engineering'', 150000, 1)');
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (3, ''VP Carol'', ''Sales'', 140000, 1)');
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (4, ''Manager Dave'', ''Engineering'', 100000, 2)');
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (5, ''Manager Eve'', ''Engineering'', 95000, 2)');
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (6, ''Dev Frank'', ''Engineering'', 80000, 4)');
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (7, ''Dev Grace'', ''Engineering'', 75000, 4)');
  Connection.ExecuteNonQuery('INSERT INTO employees (id, name, department, salary, manager_id) VALUES (8, ''Sales Henry'', ''Sales'', 70000, 3)');

  // Sales data for window functions
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS sales (' +
    '  id INTEGER PRIMARY KEY,' +
    '  employee_id INTEGER,' +
    '  amount REAL,' +
    '  sale_date TEXT' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO sales (employee_id, amount, sale_date) VALUES (8, 5000, ''2024-01-15'')');
  Connection.ExecuteNonQuery('INSERT INTO sales (employee_id, amount, sale_date) VALUES (8, 7500, ''2024-01-20'')');
  Connection.ExecuteNonQuery('INSERT INTO sales (employee_id, amount, sale_date) VALUES (8, 3000, ''2024-02-10'')');
  Connection.ExecuteNonQuery('INSERT INTO sales (employee_id, amount, sale_date) VALUES (3, 12000, ''2024-01-25'')');
  Connection.ExecuteNonQuery('INSERT INTO sales (employee_id, amount, sale_date) VALUES (3, 8000, ''2024-02-15'')');
end;

{ Uses a WITH clause to select and display employees earning more than $90,000 sorted by salary. }
procedure DemoSimpleCTE;
var
  DS: TDataSet;
begin
  WriteLn('1. Simple CTE (WITH clause)');
  WriteLn('   ------------------------');

  DS := Connection.ExecuteQuery(
    'WITH high_earners AS (' +
    '  SELECT name, salary, department FROM employees WHERE salary > 90000' +
    ')' +
    'SELECT * FROM high_earners ORDER BY salary DESC');
  try
    WriteLn('   High earners (salary > $90,000):');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%s): $%.0f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('department').AsString,
         DS.FieldByName('salary').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Builds an organization hierarchy tree using a recursive CTE that walks the manager_id chain. }
procedure DemoRecursiveCTE;
var
  DS: TDataSet;
begin
  WriteLn('2. Recursive CTE (Org Chart)');
  WriteLn('   -------------------------');

  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE org_chart(id, name, level, path) AS (' +
    '  SELECT id, name, 0, name ' +
    '  FROM employees WHERE manager_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT e.id, e.name, oc.level + 1, oc.path || '' > '' || e.name ' +
    '  FROM employees e ' +
    '  JOIN org_chart oc ON e.manager_id = oc.id' +
    ')' +
    'SELECT * FROM org_chart ORDER BY path');
  try
    WriteLn('   Organization hierarchy:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s%s (Level %d)',
        [StringOfChar(' ', DS.FieldByName('level').AsInteger * 2),
         DS.FieldByName('name').AsString,
         DS.FieldByName('level').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Generates numbers 1 through 10 with their squares using a recursive CTE. }
procedure DemoRecursiveCTENumbers;
var
  DS: TDataSet;
begin
  WriteLn('3. Recursive CTE (Number Sequence)');
  WriteLn('   -------------------------------');

  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE numbers(n) AS (' +
    '  SELECT 1 ' +
    '  UNION ALL ' +
    '  SELECT n + 1 FROM numbers WHERE n < 10' +
    ')' +
    'SELECT n, n * n as square FROM numbers');
  try
    WriteLn('   Numbers 1-10 with squares:');
    while not DS.EOF do
    begin
      Write(Format('   %d^2=%d', [DS.FieldByName('n').AsInteger, DS.FieldByName('square').AsInteger]));
      DS.Next;
      if not DS.EOF then Write(', ');
    end;
    WriteLn('');
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Ranks employees by salary using ROW_NUMBER, RANK, and DENSE_RANK window functions. }
procedure DemoWindowFunctions;
var
  DS: TDataSet;
begin
  WriteLn('4. Window Functions');
  WriteLn('   ----------------');

  // ROW_NUMBER, RANK, DENSE_RANK
  DS := Connection.ExecuteQuery(
    'SELECT name, department, salary, ' +
    '  ROW_NUMBER() OVER (ORDER BY salary DESC) as row_num, ' +
    '  RANK() OVER (ORDER BY salary DESC) as rank, ' +
    '  DENSE_RANK() OVER (ORDER BY salary DESC) as dense_rank ' +
    'FROM employees ORDER BY salary DESC LIMIT 5');
  try
    WriteLn('   Salary ranking (top 5):');
    WriteLn('   Name                  Salary   Row#  Rank  Dense');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s $%-8.0f %-5d %-5d %d',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('salary').AsFloat,
         DS.FieldByName('row_num').AsInteger,
         DS.FieldByName('rank').AsInteger,
         DS.FieldByName('dense_rank').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Computes each employee's salary difference from their department average using PARTITION BY. }
procedure DemoWindowPartition;
var
  DS: TDataSet;
begin
  WriteLn('5. Window Functions with PARTITION BY');
  WriteLn('   ----------------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT name, department, salary, ' +
    '  AVG(salary) OVER (PARTITION BY department) as dept_avg, ' +
    '  salary - AVG(salary) OVER (PARTITION BY department) as diff_from_avg ' +
    'FROM employees ORDER BY department, salary DESC');
  try
    WriteLn('   Salary vs department average:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s (%s): $%.0f (avg: $%.0f, diff: %+.0f)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('department').AsString,
         DS.FieldByName('salary').AsFloat,
         DS.FieldByName('dept_avg').AsFloat,
         DS.FieldByName('diff_from_avg').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Calculates a cumulative running total of sales amounts ordered by date using SUM OVER. }
procedure DemoRunningTotal;
var
  DS: TDataSet;
begin
  WriteLn('6. Running Total (SUM OVER)');
  WriteLn('   ------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT sale_date, amount, ' +
    '  SUM(amount) OVER (ORDER BY sale_date ROWS UNBOUNDED PRECEDING) as running_total ' +
    'FROM sales ORDER BY sale_date');
  try
    WriteLn('   Sales running total:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: $%.0f (total: $%.0f)',
        [DS.FieldByName('sale_date').AsString,
         DS.FieldByName('amount').AsFloat,
         DS.FieldByName('running_total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Displays each sale alongside its previous and next amounts using LAG and LEAD window functions. }
procedure DemoLagLead;
var
  DS: TDataSet;
begin
  WriteLn('7. LAG and LEAD Functions');
  WriteLn('   ----------------------');

  DS := Connection.ExecuteQuery(
    'SELECT sale_date, amount, ' +
    '  LAG(amount, 1, 0) OVER (ORDER BY sale_date) as prev_amount, ' +
    '  LEAD(amount, 1, 0) OVER (ORDER BY sale_date) as next_amount, ' +
    '  amount - LAG(amount, 1, 0) OVER (ORDER BY sale_date) as change ' +
    'FROM sales ORDER BY sale_date');
  try
    WriteLn('   Sales with previous/next:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: $%.0f (prev: $%.0f, next: $%.0f, change: %+.0f)',
        [DS.FieldByName('sale_date').AsString,
         DS.FieldByName('amount').AsFloat,
         DS.FieldByName('prev_amount').AsFloat,
         DS.FieldByName('next_amount').AsFloat,
         DS.FieldByName('change').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Combines employee name queries using UNION, INTERSECT, and EXCEPT to show set logic results. }
procedure DemoSetOperations;
var
  DS: TDataSet;
begin
  WriteLn('8. Set Operations (UNION, INTERSECT, EXCEPT)');
  WriteLn('   -----------------------------------------');

  // UNION
  DS := Connection.ExecuteQuery(
    'SELECT name FROM employees WHERE department = ''Engineering'' ' +
    'UNION ' +
    'SELECT name FROM employees WHERE salary > 100000');
  try
    WriteLn('   Engineering OR salary > $100k (UNION):');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // INTERSECT
  DS := Connection.ExecuteQuery(
    'SELECT name FROM employees WHERE department = ''Engineering'' ' +
    'INTERSECT ' +
    'SELECT name FROM employees WHERE salary > 100000');
  try
    WriteLn('   Engineering AND salary > $100k (INTERSECT):');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // EXCEPT
  DS := Connection.ExecuteQuery(
    'SELECT name FROM employees WHERE department = ''Engineering'' ' +
    'EXCEPT ' +
    'SELECT name FROM employees WHERE salary > 100000');
  try
    WriteLn('   Engineering BUT NOT salary > $100k (EXCEPT):');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Joins two CTEs (dept_stats and high_salary) to display department averages alongside high earner counts. }
procedure DemoMultipleCTEs;
var
  DS: TDataSet;
begin
  WriteLn('9. Multiple CTEs');
  WriteLn('   -------------');

  DS := Connection.ExecuteQuery(
    'WITH ' +
    '  dept_stats AS (' +
    '    SELECT department, AVG(salary) as avg_sal, COUNT(*) as cnt ' +
    '    FROM employees GROUP BY department' +
    '  ), ' +
    '  high_salary AS (' +
    '    SELECT * FROM employees WHERE salary > 100000' +
    '  ) ' +
    'SELECT d.department, d.avg_sal, d.cnt, COUNT(h.id) as high_earners ' +
    'FROM dept_stats d ' +
    'LEFT JOIN high_salary h ON d.department = h.department ' +
    'GROUP BY d.department');
  try
    WriteLn('   Department statistics with high earners:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: avg $%.0f, %d employees, %d high earners',
        [DS.FieldByName('department').AsString,
         DS.FieldByName('avg_sal').AsFloat,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('high_earners').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 22: Advanced SQL ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example22.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      DemoSimpleCTE;
      DemoRecursiveCTE;
      DemoRecursiveCTENumbers;
      DemoWindowFunctions;
      DemoWindowPartition;
      DemoRunningTotal;
      DemoLagLead;
      DemoSetOperations;
      DemoMultipleCTEs;

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
