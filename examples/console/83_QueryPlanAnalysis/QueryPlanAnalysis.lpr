{===============================================================================
  NDXSQLite Example 83 - Query Plan Analysis
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates EXPLAIN QUERY PLAN for query optimization:
  - Reading and interpreting query plans
  - Full table scan vs. index scan detection
  - Covering index identification
  - JOIN order analysis
  - Subquery optimization
  - Index impact measurement
  - Before/after index comparison
  - Composite index effectiveness
  - Query performance timing

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program QueryPlanAnalysis;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants, DateUtils,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Helper Functions
// =============================================================================
{ Executes EXPLAIN QUERY PLAN on the given SQL and prints each line of the query execution plan. }
procedure ShowQueryPlan(const SQL: string);
var
  DS: TDataSet;
begin
  DS := Conn.ExecuteQuery('EXPLAIN QUERY PLAN ' + SQL);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('       %s', [DS.FieldByName('detail').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Executes a SQL query multiple times and returns the average duration in milliseconds. }
function TimeQuery(const SQL: string; Iterations: Integer = 1): Double;
var
  DS: TDataSet;
  Start: TDateTime;
  I: Integer;
begin
  Start := Now;
  for I := 1 to Iterations do
  begin
    DS := Conn.ExecuteQuery(SQL);
    try
      while not DS.EOF do
        DS.Next;
    finally
      DS.Free;
    end;
  end;
  Result := MilliSecondsBetween(Now, Start) / Iterations;
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Test Schema');
  WriteLn('   ======================');

  // Orders table with various columns
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER NOT NULL,' +
    '  product_id INTEGER NOT NULL,' +
    '  order_date TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  region TEXT,' +
    '  notes TEXT' +
    ')');

  // Customers table
  Conn.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT UNIQUE,' +
    '  city TEXT,' +
    '  country TEXT,' +
    '  tier TEXT DEFAULT ''standard''' +
    ')');

  // Products table
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT,' +
    '  price REAL,' +
    '  stock INTEGER DEFAULT 0' +
    ')');

  // Order items (for JOIN analysis)
  Conn.ExecuteNonQuery(
    'CREATE TABLE order_items (' +
    '  id INTEGER PRIMARY KEY,' +
    '  order_id INTEGER NOT NULL REFERENCES orders(id),' +
    '  product_id INTEGER NOT NULL REFERENCES products(id),' +
    '  quantity INTEGER NOT NULL,' +
    '  unit_price REAL NOT NULL' +
    ')');

  WriteLn('   Created tables: orders, customers, products, order_items');
  WriteLn('');
end;

{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

// =============================================================================
// Generate Test Data
// =============================================================================
{ Populates the database with 100 customers, 50 products, 5000 orders with varying statuses/regions, and 5000 order items for query plan analysis. }
procedure GenerateTestData;
var
  I, CustId, ProdId, OrderId: Integer;
  Regions: array[0..4] of string;
  Statuses: array[0..3] of string;
  Categories: array[0..4] of string;
begin
  WriteLn('2. Generating Test Data');
  WriteLn('   ======================');

  Regions[0] := 'North'; Regions[1] := 'South';
  Regions[2] := 'East'; Regions[3] := 'West'; Regions[4] := 'Central';

  Statuses[0] := 'pending'; Statuses[1] := 'confirmed';
  Statuses[2] := 'shipped'; Statuses[3] := 'delivered';

  Categories[0] := 'Electronics'; Categories[1] := 'Books';
  Categories[2] := 'Clothing'; Categories[3] := 'Food'; Categories[4] := 'Tools';

  // Create customers
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 100 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO customers (name, email, city, country, tier) VALUES (?, ?, ?, ?, ?)',
      [Format('Customer_%d', [I]),
       Format('cust%d@email.com', [I]),
       Format('City_%d', [I mod 20]),
       Format('Country_%d', [I mod 5]),
       IfThen(I mod 10 = 0, 'premium', 'standard')]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 100 customers');

  // Create products
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 50 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO products (name, category, price, stock) VALUES (?, ?, ?, ?)',
      [Format('Product_%d', [I]),
       Categories[I mod 5],
       10.0 + (I * 5.99),
       50 + (I * 3)]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 50 products');

  // Create orders (5000 for meaningful query plans)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 5000 do
  begin
    CustId := (I mod 100) + 1;
    ProdId := (I mod 50) + 1;
    Conn.ExecuteNonQuery(
      'INSERT INTO orders (customer_id, product_id, order_date, amount, status, region) VALUES (?, ?, ?, ?, ?, ?)',
      [CustId, ProdId,
       Format('2024-%s-%s', [Format('%.2d', [(I mod 12) + 1]), Format('%.2d', [(I mod 28) + 1])]),
       25.0 + (I mod 500) * 1.5,
       Statuses[I mod 4],
       Regions[I mod 5]]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 5000 orders');

  // Create order items
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 5000 do
  begin
    OrderId := I;
    ProdId := (I mod 50) + 1;
    Conn.ExecuteNonQuery(
      'INSERT INTO order_items (order_id, product_id, quantity, unit_price) VALUES (?, ?, ?, ?)',
      [OrderId, ProdId, (I mod 5) + 1, 10.0 + (ProdId * 3.5)]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 5000 order items');
  WriteLn('');
end;

// =============================================================================
// Demo: Full Table Scan
// =============================================================================
{ Shows query plans for queries without indexes (full table scans) versus a primary key lookup, explaining the O(n) vs O(log n) performance difference. }
procedure DemoFullTableScan;
begin
  WriteLn('3. Full Table Scan Analysis');
  WriteLn('   ==========================');
  WriteLn('');

  WriteLn('   Query: SELECT * FROM orders WHERE status = ''shipped''');
  WriteLn('   Plan (no index on status):');
  ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped''');
  WriteLn('   -> SCAN TABLE = reads every row (O(n))');

  WriteLn('');
  WriteLn('   Query: SELECT * FROM orders WHERE amount > 500');
  WriteLn('   Plan (no index on amount):');
  ShowQueryPlan('SELECT * FROM orders WHERE amount > 500');
  WriteLn('   -> SCAN TABLE = full scan required');

  WriteLn('');
  WriteLn('   Query: SELECT * FROM orders WHERE id = 42');
  WriteLn('   Plan (PRIMARY KEY lookup):');
  ShowQueryPlan('SELECT * FROM orders WHERE id = 42');
  WriteLn('   -> SEARCH using INTEGER PRIMARY KEY = O(log n)');

  WriteLn('');
end;

// =============================================================================
// Demo: Index Impact
// =============================================================================
{ Compares query plans and timing before and after adding an index on orders(status), then creates additional indexes for subsequent demonstrations. }
procedure DemoIndexImpact;
var
  TimeNoIndex, TimeWithIndex: Double;
begin
  WriteLn('4. Index Impact Analysis');
  WriteLn('   ========================');
  WriteLn('');

  // Before index
  WriteLn('   BEFORE adding index on orders(status):');
  WriteLn('   Plan:');
  ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped''');
  TimeNoIndex := TimeQuery('SELECT * FROM orders WHERE status = ''shipped''', 10);
  WriteLn(Format('   Avg time: %.2f ms', [TimeNoIndex]));

  // Add index
  WriteLn('');
  WriteLn('   Adding index: CREATE INDEX idx_orders_status ON orders(status)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_status ON orders(status)');

  // After index
  WriteLn('');
  WriteLn('   AFTER adding index on orders(status):');
  WriteLn('   Plan:');
  ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped''');
  TimeWithIndex := TimeQuery('SELECT * FROM orders WHERE status = ''shipped''', 10);
  WriteLn(Format('   Avg time: %.2f ms', [TimeWithIndex]));
  WriteLn('   Improvement: SCAN -> SEARCH using index');

  // Additional indexes
  WriteLn('');
  WriteLn('   Adding more indexes for next demos:');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_customer ON orders(customer_id)');
  WriteLn('     + idx_orders_customer ON orders(customer_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_date ON orders(order_date)');
  WriteLn('     + idx_orders_date ON orders(order_date)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_region ON orders(region)');
  WriteLn('     + idx_orders_region ON orders(region)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_order_items_order ON order_items(order_id)');
  WriteLn('     + idx_order_items_order ON order_items(order_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_order_items_product ON order_items(product_id)');
  WriteLn('     + idx_order_items_product ON order_items(product_id)');

  WriteLn('');
end;

// =============================================================================
// Demo: Composite Index
// =============================================================================
{ Shows how a composite index on (status, region) improves multi-column filters, and illustrates that composite indexes only help when querying the leftmost prefix. }
procedure DemoCompositeIndex;
begin
  WriteLn('5. Composite Index Analysis');
  WriteLn('   ===========================');
  WriteLn('');

  // Single column queries
  WriteLn('   Query: WHERE status = ''shipped'' AND region = ''North''');
  WriteLn('   Plan (separate indexes):');
  ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped'' AND region = ''North''');

  // Add composite index
  WriteLn('');
  WriteLn('   Adding composite: CREATE INDEX idx_status_region ON orders(status, region)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_status_region ON orders(status, region)');

  WriteLn('');
  WriteLn('   Plan (with composite index):');
  ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped'' AND region = ''North''');
  WriteLn('   -> Uses composite index (more selective)');

  // Prefix usage
  WriteLn('');
  WriteLn('   Composite index prefix usage:');
  WriteLn('   Query: WHERE status = ''shipped'' (uses prefix):');
  ShowQueryPlan('SELECT * FROM orders WHERE status = ''shipped''');

  WriteLn('');
  WriteLn('   Query: WHERE region = ''North'' (cannot use composite):');
  ShowQueryPlan('SELECT * FROM orders WHERE region = ''North''');
  WriteLn('   -> Falls back to idx_orders_region (second column alone not usable in composite)');

  WriteLn('');
end;

// =============================================================================
// Demo: Covering Index
// =============================================================================
{ Creates a covering index on (status, amount) and shows how queries can be satisfied entirely from the index without accessing the table data. }
procedure DemoCoveringIndex;
begin
  WriteLn('6. Covering Index Analysis');
  WriteLn('   ==========================');
  WriteLn('');

  WriteLn('   Query: SELECT status, COUNT(*) FROM orders GROUP BY status');
  WriteLn('   Plan (non-covering):');
  ShowQueryPlan('SELECT status, COUNT(*) FROM orders GROUP BY status');

  // Create covering index
  WriteLn('');
  WriteLn('   Adding covering index: CREATE INDEX idx_cover_status_amount ON orders(status, amount)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_cover_status_amount ON orders(status, amount)');

  WriteLn('');
  WriteLn('   Query: SELECT status, SUM(amount) FROM orders GROUP BY status');
  WriteLn('   Plan (with covering index):');
  ShowQueryPlan('SELECT status, SUM(amount) FROM orders GROUP BY status');
  WriteLn('   -> May use covering index (all needed columns in index)');

  WriteLn('');
end;

// =============================================================================
// Demo: JOIN Order
// =============================================================================
{ Displays query plans for 2-table, 3-table, and 4-table JOINs, showing how SQLite chooses scan order based on filter selectivity and index availability. }
procedure DemoJoinOrder;
begin
  WriteLn('7. JOIN Order Analysis');
  WriteLn('   =====================');
  WriteLn('');

  // Simple two-table join
  WriteLn('   Two-table JOIN (orders + customers):');
  ShowQueryPlan(
    'SELECT o.id, c.name, o.amount FROM orders o ' +
    'JOIN customers c ON o.customer_id = c.id ' +
    'WHERE o.status = ''shipped''');
  WriteLn('   -> SQLite chooses scan order based on selectivity');

  // Three-table join
  WriteLn('');
  WriteLn('   Three-table JOIN (orders + customers + products):');
  ShowQueryPlan(
    'SELECT o.id, c.name, p.name, o.amount FROM orders o ' +
    'JOIN customers c ON o.customer_id = c.id ' +
    'JOIN products p ON o.product_id = p.id ' +
    'WHERE o.status = ''confirmed'' AND o.region = ''East''');

  // Four-table join
  WriteLn('');
  WriteLn('   Four-table JOIN (orders + customers + order_items + products):');
  ShowQueryPlan(
    'SELECT o.id, c.name, oi.quantity, p.name FROM orders o ' +
    'JOIN customers c ON o.customer_id = c.id ' +
    'JOIN order_items oi ON o.id = oi.order_id ' +
    'JOIN products p ON oi.product_id = p.id ' +
    'WHERE c.tier = ''premium'' AND o.amount > 200');

  WriteLn('');
end;

// =============================================================================
// Demo: Subquery Plans
// =============================================================================
{ Compares query plans for correlated subqueries, IN subqueries, EXISTS subqueries, and their equivalent JOIN rewrites to identify more efficient patterns. }
procedure DemoSubqueryPlans;
begin
  WriteLn('8. Subquery Analysis');
  WriteLn('   ====================');
  WriteLn('');

  // Correlated subquery
  WriteLn('   Correlated subquery (expensive):');
  ShowQueryPlan(
    'SELECT c.name, (SELECT COUNT(*) FROM orders o WHERE o.customer_id = c.id) AS order_count ' +
    'FROM customers c WHERE c.tier = ''premium''');

  // IN subquery
  WriteLn('');
  WriteLn('   IN subquery:');
  ShowQueryPlan(
    'SELECT * FROM customers WHERE id IN ' +
    '(SELECT customer_id FROM orders WHERE amount > 700)');

  // EXISTS subquery
  WriteLn('');
  WriteLn('   EXISTS subquery:');
  ShowQueryPlan(
    'SELECT * FROM customers c WHERE EXISTS ' +
    '(SELECT 1 FROM orders o WHERE o.customer_id = c.id AND o.status = ''delivered'')');

  // Rewritten as JOIN (usually better)
  WriteLn('');
  WriteLn('   Same as JOIN (often more efficient):');
  ShowQueryPlan(
    'SELECT DISTINCT c.* FROM customers c ' +
    'JOIN orders o ON o.customer_id = c.id ' +
    'WHERE o.status = ''delivered''');

  WriteLn('');
end;

// =============================================================================
// Demo: ORDER BY and GROUP BY
// =============================================================================
{ Shows query plans for ORDER BY and GROUP BY clauses, contrasting cases that require temporary B-trees for sorting versus those that can use existing indexes. }
procedure DemoSortingPlans;
begin
  WriteLn('9. ORDER BY / GROUP BY Analysis');
  WriteLn('   ===============================');
  WriteLn('');

  // ORDER BY without index
  WriteLn('   ORDER BY without matching index:');
  ShowQueryPlan('SELECT * FROM orders ORDER BY amount DESC LIMIT 10');
  WriteLn('   -> Requires temp B-tree for sorting');

  // ORDER BY with matching index
  WriteLn('');
  WriteLn('   ORDER BY with matching index (order_date):');
  ShowQueryPlan('SELECT * FROM orders ORDER BY order_date LIMIT 10');
  WriteLn('   -> Can use index to avoid sort');

  // GROUP BY
  WriteLn('');
  WriteLn('   GROUP BY region (with index):');
  ShowQueryPlan('SELECT region, COUNT(*), SUM(amount) FROM orders GROUP BY region');

  // GROUP BY without index
  WriteLn('');
  WriteLn('   GROUP BY category (no direct index on products):');
  ShowQueryPlan('SELECT category, AVG(price) FROM products GROUP BY category');
  WriteLn('   -> Uses temp B-tree for grouping');

  WriteLn('');
end;

// =============================================================================
// Demo: Query Optimization Tips
// =============================================================================
{ Times and compares common query patterns: SELECT * vs specific columns, COUNT(*) vs COUNT(column), and queries with vs without LIMIT. }
procedure DemoOptimizationTips;
var
  Time1, Time2: Double;
begin
  WriteLn('10. Optimization Comparisons');
  WriteLn('    ===========================');
  WriteLn('');

  // SELECT * vs SELECT specific columns
  WriteLn('    SELECT * vs specific columns:');
  Time1 := TimeQuery('SELECT * FROM orders WHERE status = ''shipped''', 10);
  Time2 := TimeQuery('SELECT id, amount FROM orders WHERE status = ''shipped''', 10);
  WriteLn(Format('      SELECT *:            %.2f ms', [Time1]));
  WriteLn(Format('      SELECT id, amount:   %.2f ms', [Time2]));

  // COUNT(*) vs COUNT(column)
  WriteLn('');
  WriteLn('    COUNT(*) vs COUNT(column):');
  Time1 := TimeQuery('SELECT COUNT(*) FROM orders', 10);
  Time2 := TimeQuery('SELECT COUNT(notes) FROM orders', 10);
  WriteLn(Format('      COUNT(*):         %.2f ms', [Time1]));
  WriteLn(Format('      COUNT(notes):     %.2f ms (skips NULLs)', [Time2]));

  // LIMIT impact
  WriteLn('');
  WriteLn('    LIMIT impact on performance:');
  Time1 := TimeQuery('SELECT * FROM orders WHERE status = ''shipped''', 10);
  Time2 := TimeQuery('SELECT * FROM orders WHERE status = ''shipped'' LIMIT 10', 10);
  WriteLn(Format('      Without LIMIT:  %.2f ms', [Time1]));
  WriteLn(Format('      With LIMIT 10:  %.2f ms', [Time2]));

  WriteLn('');
end;

// =============================================================================
// Demo: Index List Summary
// =============================================================================
{ Lists all user-created indexes from sqlite_master showing index name, target table, column definitions, and total index count. }
procedure DemoIndexSummary;
var
  DS: TDataSet;
begin
  WriteLn('11. Index Summary');
  WriteLn('    ================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT name, tbl_name, sql FROM sqlite_master ' +
    'WHERE type = ''index'' AND sql IS NOT NULL ' +
    'ORDER BY tbl_name, name');
  try
    WriteLn(Format('    %-30s | %-12s | %s', ['Index Name', 'Table', 'Definition']));
    WriteLn('    ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('    %-30s | %-12s | %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('tbl_name').AsString,
         DS.FieldByName('sql').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('    Total indexes: ' + IntToStr(
    Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_master WHERE type = ''index'''))));
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 83: Query Plan Analysis ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    GenerateTestData;
    DemoFullTableScan;
    DemoIndexImpact;
    DemoCompositeIndex;
    DemoCoveringIndex;
    DemoJoinOrder;
    DemoSubqueryPlans;
    DemoSortingPlans;
    DemoOptimizationTips;
    DemoIndexSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
