{===============================================================================
  NDXSQLite Example 37 - Recursive Queries with CTEs
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Common Table Expressions (WITH clause)
  - Recursive CTEs for hierarchical data
  - Tree traversal (employees, categories)
  - Graph traversal (paths, connections)
  - Bill of Materials pattern
  - Generating sequences

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program RecursiveQueries;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Runs a non-recursive CTE that builds a quarterly sales summary and computes the total sales across all quarters. }
procedure DemoBasicCTE;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic CTE (Common Table Expression)');
  WriteLn('   ------------------------------------');

  // Simple CTE - not recursive
  DS := Connection.ExecuteQuery(
    'WITH sales_summary AS (' +
    '  SELECT ''Q1'' AS quarter, 100 AS sales UNION ALL ' +
    '  SELECT ''Q2'', 150 UNION ALL ' +
    '  SELECT ''Q3'', 200 UNION ALL ' +
    '  SELECT ''Q4'', 250' +
    ')' +
    'SELECT quarter, sales, ' +
    '       (SELECT SUM(sales) FROM sales_summary) AS total ' +
    'FROM sales_summary');
  try
    WriteLn('   Quarter  Sales   Total');
    WriteLn('   -------  -----   -----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('quarter').AsString:7,
              DS.FieldByName('sales').AsInteger:7,
              DS.FieldByName('total').AsInteger:8);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   CTEs create named temporary result sets');
  WriteLn('');
end;

{ Generates numbers 1 through 10 with their squares and cubes using a recursive CTE. }
procedure DemoRecursiveNumbers;
var
  DS: TDataSet;
begin
  WriteLn('2. Recursive CTE - Generating numbers');
  WriteLn('   -----------------------------------');

  // Generate numbers 1-10
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE numbers(n) AS (' +
    '  SELECT 1 ' +                    // Base case
    '  UNION ALL ' +
    '  SELECT n + 1 FROM numbers WHERE n < 10' +  // Recursive case
    ')' +
    'SELECT n, n * n AS square, n * n * n AS cube FROM numbers');
  try
    WriteLn('   N    Square   Cube');
    WriteLn('   --   ------   ----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('n').AsInteger:2,
              DS.FieldByName('square').AsInteger:8,
              DS.FieldByName('cube').AsInteger:7);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Pattern: Base case UNION ALL Recursive case with termination');
  WriteLn('');
end;

{ Demonstrates generating Fibonacci numbers using a recursive CTE. }
procedure DemoFibonacci;
var
  DS: TDataSet;
begin
  WriteLn('3. Recursive CTE - Fibonacci sequence');
  WriteLn('   -----------------------------------');

  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE fib(n, a, b) AS (' +
    '  SELECT 1, 0, 1 ' +              // Initial: position 1, fib(0)=0, fib(1)=1
    '  UNION ALL ' +
    '  SELECT n + 1, b, a + b FROM fib WHERE n < 15' +
    ')' +
    'SELECT n, a AS fib_value FROM fib');
  try
    WriteLn('   N    Fibonacci');
    WriteLn('   --   ---------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('n').AsInteger:2,
              '   ', DS.FieldByName('fib_value').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates an employees table with manager references and uses a recursive CTE to display the organizational chart with indented levels. }
procedure DemoEmployeeHierarchy;
var
  DS: TDataSet;
begin
  WriteLn('4. Employee hierarchy (tree structure)');
  WriteLn('   -----------------------------------');

  // Create employee table
  Connection.ExecuteNonQuery('DROP TABLE IF EXISTS employees');
  Connection.ExecuteNonQuery(
    'CREATE TABLE employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  manager_id INTEGER REFERENCES employees(id)' +
    ')');

  // Insert organizational structure
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Alice (CEO)'', NULL)');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (2, ''Bob (CTO)'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (3, ''Carol (CFO)'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (4, ''Dave (Dev Lead)'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (5, ''Eve (Dev)'', 4)');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (6, ''Frank (Dev)'', 4)');
  Connection.ExecuteNonQuery('INSERT INTO employees VALUES (7, ''Grace (Accountant)'', 3)');

  WriteLn('   Organizational chart with levels:');
  WriteLn('');

  // Recursive query to show hierarchy
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE org_chart(id, name, manager_id, level, path) AS (' +
    '  SELECT id, name, manager_id, 0, name ' +
    '  FROM employees WHERE manager_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT e.id, e.name, e.manager_id, oc.level + 1, oc.path || '' > '' || e.name ' +
    '  FROM employees e ' +
    '  JOIN org_chart oc ON e.manager_id = oc.id' +
    ')' +
    'SELECT id, name, level, ' +
    '       printf(''%.*c'', level * 2, '' '') || name AS display ' +
    'FROM org_chart ' +
    'ORDER BY path');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('display').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Recursive CTE walks parent-child relationships');
  WriteLn('');
end;

{ Creates a hierarchical categories table and uses a recursive CTE to build and display full slash-separated category paths. }
procedure DemoCategoryTree;
var
  DS: TDataSet;
begin
  WriteLn('5. Category tree with full paths');
  WriteLn('   -----------------------------');

  // Create category table
  Connection.ExecuteNonQuery('DROP TABLE IF EXISTS categories');
  Connection.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  parent_id INTEGER REFERENCES categories(id)' +
    ')');

  // Insert category hierarchy
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (1, ''Electronics'', NULL)');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (2, ''Computers'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (3, ''Phones'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (4, ''Laptops'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (5, ''Desktops'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (6, ''Smartphones'', 3)');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (7, ''Tablets'', 3)');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (8, ''Gaming Laptops'', 4)');

  WriteLn('   Full category paths:');
  WriteLn('');

  // Build full paths
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE category_path(id, name, path, depth) AS (' +
    '  SELECT id, name, name, 0 ' +
    '  FROM categories WHERE parent_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT c.id, c.name, cp.path || '' / '' || c.name, cp.depth + 1 ' +
    '  FROM categories c ' +
    '  JOIN category_path cp ON c.parent_id = cp.id' +
    ')' +
    'SELECT id, path, depth FROM category_path ORDER BY path');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('id').AsInteger:2, '] ',
              DS.FieldByName('path').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates bottom-up traversal to find all ancestors of a node. }
procedure DemoAncestors;
var
  DS: TDataSet;
begin
  WriteLn('6. Finding all ancestors (bottom-up)');
  WriteLn('   ---------------------------------');

  WriteLn('   Ancestors of "Gaming Laptops" (id=8):');
  WriteLn('');

  // Find all ancestors of a specific node
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE ancestors(id, name, parent_id, level) AS (' +
    '  SELECT id, name, parent_id, 0 ' +
    '  FROM categories WHERE id = 8 ' +    // Start from Gaming Laptops
    '  UNION ALL ' +
    '  SELECT c.id, c.name, c.parent_id, a.level + 1 ' +
    '  FROM categories c ' +
    '  JOIN ancestors a ON c.id = a.parent_id' +
    ')' +
    'SELECT id, name, level FROM ancestors ORDER BY level');
  try
    while not DS.EOF do
    begin
      WriteLn('   Level ', DS.FieldByName('level').AsInteger, ': ',
              DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Bottom-up traversal: start from leaf, walk up to root');
  WriteLn('');
end;

{ Demonstrates top-down traversal to find all descendants of a node. }
procedure DemoDescendants;
var
  DS: TDataSet;
begin
  WriteLn('7. Finding all descendants (top-down)');
  WriteLn('   ----------------------------------');

  WriteLn('   Descendants of "Computers" (id=2):');
  WriteLn('');

  // Find all descendants of a specific node
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE descendants(id, name, parent_id, level) AS (' +
    '  SELECT id, name, parent_id, 0 ' +
    '  FROM categories WHERE id = 2 ' +    // Start from Computers
    '  UNION ALL ' +
    '  SELECT c.id, c.name, c.parent_id, d.level + 1 ' +
    '  FROM categories c ' +
    '  JOIN descendants d ON c.parent_id = d.id' +
    ')' +
    'SELECT id, name, level FROM descendants ORDER BY level, name');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', StringOfChar(' ', DS.FieldByName('level').AsInteger * 2),
              DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates a bidirectional city routes graph and uses a recursive CTE with cycle detection to find all paths from Paris to Marseille. }
procedure DemoGraphPaths;
var
  DS: TDataSet;
begin
  WriteLn('8. Graph traversal - finding paths');
  WriteLn('   --------------------------------');

  // Create a graph (cities and routes)
  Connection.ExecuteNonQuery('DROP TABLE IF EXISTS routes');
  Connection.ExecuteNonQuery(
    'CREATE TABLE routes (' +
    '  from_city TEXT,' +
    '  to_city TEXT,' +
    '  distance INTEGER,' +
    '  PRIMARY KEY (from_city, to_city)' +
    ')');

  // Insert routes (bidirectional by adding both ways)
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Paris'', ''Lyon'', 470)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Lyon'', ''Paris'', 470)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Lyon'', ''Marseille'', 315)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Marseille'', ''Lyon'', 315)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Paris'', ''Bordeaux'', 585)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Bordeaux'', ''Paris'', 585)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Bordeaux'', ''Toulouse'', 245)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Toulouse'', ''Bordeaux'', 245)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Toulouse'', ''Marseille'', 405)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (''Marseille'', ''Toulouse'', 405)');

  WriteLn('   All paths from Paris to Marseille:');
  WriteLn('');

  // Find all paths (avoiding cycles)
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, visited) AS (' +
    '  SELECT ''Paris'', ''Paris'', 0, ''|Paris|'' ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance, ' +
    '         p.visited || r.to_city || ''|'' ' +
    '  FROM routes r ' +
    '  JOIN paths p ON r.from_city = p.current_city ' +
    '  WHERE p.visited NOT LIKE ''%|'' || r.to_city || ''|%'' ' +  // Avoid cycles
    '    AND length(p.path) < 100' +  // Limit path length
    ')' +
    'SELECT path, total_distance ' +
    'FROM paths ' +
    'WHERE current_city = ''Marseille'' ' +
    'ORDER BY total_distance');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('path').AsString);
      WriteLn('     Distance: ', DS.FieldByName('total_distance').AsInteger, ' km');
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Graph traversal with cycle detection');
  WriteLn('');
end;

{ Creates a BOM table for a bicycle and recursively explodes the parts list, multiplying quantities down the tree. }
procedure DemoBillOfMaterials;
var
  DS: TDataSet;
begin
  WriteLn('9. Bill of Materials (BOM) explosion');
  WriteLn('   ----------------------------------');

  // Create BOM table
  Connection.ExecuteNonQuery('DROP TABLE IF EXISTS bom');
  Connection.ExecuteNonQuery(
    'CREATE TABLE bom (' +
    '  parent TEXT,' +
    '  component TEXT,' +
    '  quantity INTEGER,' +
    '  PRIMARY KEY (parent, component)' +
    ')');

  // Insert BOM structure
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Bicycle'', ''Frame'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Bicycle'', ''Wheel'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Bicycle'', ''Handlebar'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Wheel'', ''Rim'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Wheel'', ''Spoke'', 36)');
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Wheel'', ''Tire'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Frame'', ''Tube'', 3)');
  Connection.ExecuteNonQuery('INSERT INTO bom VALUES (''Frame'', ''Seat Post'', 1)');

  WriteLn('   Complete parts list for a Bicycle:');
  WriteLn('');

  // Explode BOM with quantities
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE exploded_bom(component, quantity, level, path) AS (' +
    '  SELECT ''Bicycle'', 1, 0, ''Bicycle'' ' +
    '  UNION ALL ' +
    '  SELECT b.component, ' +
    '         b.quantity * eb.quantity, ' +
    '         eb.level + 1, ' +
    '         eb.path || '' > '' || b.component ' +
    '  FROM bom b ' +
    '  JOIN exploded_bom eb ON b.parent = eb.component' +
    ')' +
    'SELECT component, quantity, level, ' +
    '       printf(''%.*c'', level * 2, '' '') || component AS display ' +
    'FROM exploded_bom ' +
    'ORDER BY path');
  try
    WriteLn('   Component           Qty');
    WriteLn('   ---------           ---');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('display').AsString:20,
              DS.FieldByName('quantity').AsInteger:4);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Note: Quantities multiply down the tree');
  WriteLn('');
end;

{ Generates the next 7 days starting from today using a recursive CTE and displays each date with its day name. }
procedure DemoDateSeries;
var
  DS: TDataSet;
begin
  WriteLn('10. Generating date series');
  WriteLn('    ----------------------');

  WriteLn('    Next 7 days:');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE dates(d) AS (' +
    '  SELECT date(''now'') ' +
    '  UNION ALL ' +
    '  SELECT date(d, ''+1 day'') FROM dates WHERE d < date(''now'', ''+6 days'')' +
    ')' +
    'SELECT d AS date, ' +
    '       strftime(''%w'', d) AS day_of_week, ' +
    '       CASE CAST(strftime(''%w'', d) AS INTEGER) ' +
    '         WHEN 0 THEN ''Sunday'' ' +
    '         WHEN 1 THEN ''Monday'' ' +
    '         WHEN 2 THEN ''Tuesday'' ' +
    '         WHEN 3 THEN ''Wednesday'' ' +
    '         WHEN 4 THEN ''Thursday'' ' +
    '         WHEN 5 THEN ''Friday'' ' +
    '         WHEN 6 THEN ''Saturday'' ' +
    '       END AS day_name ' +
    'FROM dates');
  try
    WriteLn('    Date         Day');
    WriteLn('    ----------   ---------');
    while not DS.EOF do
    begin
      WriteLn('    ', DS.FieldByName('date').AsString, '   ',
              DS.FieldByName('day_name').AsString);
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
  WriteLn('=== NDXSQLite Example 37: Recursive Queries ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example37.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      DemoBasicCTE;
      DemoRecursiveNumbers;
      DemoFibonacci;
      DemoEmployeeHierarchy;
      DemoCategoryTree;
      DemoAncestors;
      DemoDescendants;
      DemoGraphPaths;
      DemoBillOfMaterials;
      DemoDateSeries;

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
