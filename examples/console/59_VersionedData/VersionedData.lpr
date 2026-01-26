{===============================================================================
  NDXSQLite Example 59 - Versioned Data (Temporal Tables)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Temporal tables with valid_from/valid_to timestamps
  - Data versioning with automatic history tracking
  - Point-in-time queries
  - Current vs historical data views
  - Bi-temporal modeling (system time + business time)
  - Time-travel queries

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program VersionedData;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates temporal history tables for products and employees with valid_from/valid_to columns, current-data views, a bi-temporal contracts table, and performance indexes. }
procedure SetupTemporalTables;
begin
  // Main products table with temporal columns
  Connection.ExecuteNonQuery(
    'CREATE TABLE products_history (' +
    '  id INTEGER NOT NULL,' +
    '  version INTEGER NOT NULL,' +
    '  name TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  category TEXT,' +
    '  valid_from TEXT NOT NULL,' +
    '  valid_to TEXT,' +
    '  is_current INTEGER DEFAULT 1,' +
    '  modified_by TEXT,' +
    '  PRIMARY KEY (id, version)' +
    ')');

  // Current products view
  Connection.ExecuteNonQuery(
    'CREATE VIEW products AS ' +
    'SELECT id, version, name, price, category, valid_from, modified_by ' +
    'FROM products_history ' +
    'WHERE is_current = 1');

  // Employees with temporal tracking
  Connection.ExecuteNonQuery(
    'CREATE TABLE employees_history (' +
    '  id INTEGER NOT NULL,' +
    '  version INTEGER NOT NULL,' +
    '  name TEXT NOT NULL,' +
    '  department TEXT NOT NULL,' +
    '  salary REAL NOT NULL,' +
    '  manager_id INTEGER,' +
    '  valid_from TEXT NOT NULL,' +
    '  valid_to TEXT,' +
    '  is_current INTEGER DEFAULT 1,' +
    '  PRIMARY KEY (id, version)' +
    ')');

  // Current employees view
  Connection.ExecuteNonQuery(
    'CREATE VIEW employees AS ' +
    'SELECT id, version, name, department, salary, manager_id, valid_from ' +
    'FROM employees_history ' +
    'WHERE is_current = 1');

  // Bi-temporal table: contracts with system time and business time
  Connection.ExecuteNonQuery(
    'CREATE TABLE contracts (' +
    '  id INTEGER NOT NULL,' +
    '  version INTEGER NOT NULL,' +
    '  customer_name TEXT NOT NULL,' +
    '  contract_value REAL NOT NULL,' +
    '  business_start TEXT NOT NULL,' +
    '  business_end TEXT,' +
    '  system_start TEXT NOT NULL,' +
    '  system_end TEXT,' +
    '  is_current INTEGER DEFAULT 1,' +
    '  PRIMARY KEY (id, version)' +
    ')');

  // Indexes for temporal queries
  Connection.ExecuteNonQuery('CREATE INDEX idx_products_current ON products_history(is_current)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_products_valid ON products_history(valid_from, valid_to)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_employees_current ON employees_history(is_current)');
end;

{ Inserts a new product version, marking the previous version as non-current with a valid_to timestamp, and assigns an incremented version number. }
procedure InsertVersionedProduct(AId: Integer; const AName: string; APrice: Real;
  const ACategory, AValidFrom, AModifiedBy: string);
var
  CurrentVersion, NewVersion: Integer;
  ScalarResult: Variant;
begin
  // Get current version
  ScalarResult := Connection.ExecuteScalar(
    'SELECT MAX(version) FROM products_history WHERE id = ?', [AId]);
  if VarIsNull(ScalarResult) then
    CurrentVersion := 0
  else
    CurrentVersion := ScalarResult;

  NewVersion := CurrentVersion + 1;

  // If updating, mark old version as not current
  if CurrentVersion > 0 then
  begin
    Connection.ExecuteNonQuery(
      'UPDATE products_history SET is_current = 0, valid_to = ? ' +
      'WHERE id = ? AND is_current = 1', [AValidFrom, AId]);
  end;

  // Insert new version
  Connection.ExecuteNonQuery(
    'INSERT INTO products_history (id, version, name, price, category, valid_from, is_current, modified_by) ' +
    'VALUES (?, ?, ?, ?, ?, ?, 1, ?)',
    [AId, NewVersion, AName, APrice, ACategory, AValidFrom, AModifiedBy]);
end;

{ Inserts a new employee version with department, salary, and manager, marking the previous version as non-current with a valid_to timestamp. }
procedure InsertVersionedEmployee(AId: Integer; const AName, ADepartment: string;
  ASalary: Real; AManagerId: Integer; const AValidFrom: string);
var
  CurrentVersion, NewVersion: Integer;
  ManagerVal, ScalarResult: Variant;
begin
  ScalarResult := Connection.ExecuteScalar(
    'SELECT MAX(version) FROM employees_history WHERE id = ?', [AId]);
  if VarIsNull(ScalarResult) then
    CurrentVersion := 0
  else
    CurrentVersion := ScalarResult;

  NewVersion := CurrentVersion + 1;

  if CurrentVersion > 0 then
  begin
    Connection.ExecuteNonQuery(
      'UPDATE employees_history SET is_current = 0, valid_to = ? ' +
      'WHERE id = ? AND is_current = 1', [AValidFrom, AId]);
  end;

  if AManagerId = 0 then
    ManagerVal := Null
  else
    ManagerVal := AManagerId;

  Connection.ExecuteNonQuery(
    'INSERT INTO employees_history (id, version, name, department, salary, manager_id, valid_from, is_current) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?, 1)',
    [AId, NewVersion, AName, ADepartment, ASalary, ManagerVal, AValidFrom]);
end;

{ Populates product and employee history with multiple versions simulating price and salary changes over 2022-2024, plus a bi-temporal contract with a renegotiation. }
procedure SetupSampleData;
begin
  // Insert product history (simulating changes over time)
  InsertVersionedProduct(1, 'Laptop Basic', 799.99, 'Electronics', '2023-01-01', 'admin');
  InsertVersionedProduct(1, 'Laptop Basic', 749.99, 'Electronics', '2023-06-01', 'sales_mgr');
  InsertVersionedProduct(1, 'Laptop Pro', 899.99, 'Electronics', '2024-01-01', 'product_mgr');

  InsertVersionedProduct(2, 'Wireless Mouse', 29.99, 'Accessories', '2023-01-15', 'admin');
  InsertVersionedProduct(2, 'Wireless Mouse', 34.99, 'Accessories', '2023-09-01', 'sales_mgr');

  InsertVersionedProduct(3, 'USB-C Cable', 9.99, 'Accessories', '2023-02-01', 'admin');

  // Insert employee history
  InsertVersionedEmployee(1, 'John Smith', 'Engineering', 75000, 0, '2022-01-15');
  InsertVersionedEmployee(1, 'John Smith', 'Engineering', 82000, 0, '2023-01-01');
  InsertVersionedEmployee(1, 'John Smith', 'Engineering', 90000, 0, '2024-01-01');

  InsertVersionedEmployee(2, 'Jane Doe', 'Sales', 65000, 1, '2022-06-01');
  InsertVersionedEmployee(2, 'Jane Doe', 'Marketing', 70000, 1, '2023-07-01');

  InsertVersionedEmployee(3, 'Bob Wilson', 'Engineering', 60000, 1, '2023-03-01');

  // Insert bi-temporal contract
  Connection.ExecuteNonQuery(
    'INSERT INTO contracts VALUES (1, 1, ''Acme Corp'', 50000, ''2023-01-01'', ''2023-12-31'', ''2023-01-01'', NULL, 0)');
  Connection.ExecuteNonQuery(
    'INSERT INTO contracts VALUES (1, 2, ''Acme Corp'', 75000, ''2023-01-01'', ''2024-06-30'', ''2023-06-15'', NULL, 1)');
end;

{ Queries the current-data views to display the latest version of each product and employee record. }
procedure DemoCurrentData;
var
  DS: TDataSet;
begin
  WriteLn('1. Current Data (Latest Versions)');
  WriteLn('   --------------------------------');

  WriteLn('   Current Products:');
  DS := Connection.ExecuteQuery('SELECT * FROM products ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d v%d] %s - $%.2f (%s) valid since %s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('version').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('category').AsString,
         DS.FieldByName('valid_from').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  WriteLn('   Current Employees:');
  DS := Connection.ExecuteQuery('SELECT * FROM employees ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d v%d] %s - %s - $%.0f',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('version').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('department').AsString,
         DS.FieldByName('salary').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays the complete version history for a product and an employee, showing name, price/salary, and validity periods for each version. }
procedure DemoFullHistory;
var
  DS: TDataSet;
begin
  WriteLn('2. Full Version History');
  WriteLn('   ----------------------');

  WriteLn('   Product 1 History (Laptop):');
  DS := Connection.ExecuteQuery(
    'SELECT version, name, price, valid_from, ' +
    '       COALESCE(valid_to, ''(current)'') as valid_to, modified_by ' +
    'FROM products_history WHERE id = 1 ORDER BY version');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     v%d: %s ($%.2f) | %s to %s | by %s',
        [DS.FieldByName('version').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('valid_from').AsString,
         DS.FieldByName('valid_to').AsString,
         DS.FieldByName('modified_by').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  WriteLn('   Employee 1 Salary History:');
  DS := Connection.ExecuteQuery(
    'SELECT version, salary, department, valid_from, ' +
    '       COALESCE(valid_to, ''(current)'') as valid_to ' +
    'FROM employees_history WHERE id = 1 ORDER BY version');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     v%d: $%.0f (%s) | %s to %s',
        [DS.FieldByName('version').AsInteger,
         DS.FieldByName('salary').AsFloat,
         DS.FieldByName('department').AsString,
         DS.FieldByName('valid_from').AsString,
         DS.FieldByName('valid_to').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries the product history table at two specific dates to show which product versions were valid at each point in time. }
procedure DemoPointInTimeQuery;
var
  DS: TDataSet;
  QueryDate: string;
begin
  WriteLn('3. Point-in-Time Queries');
  WriteLn('   -----------------------');

  // Query products as they were on a specific date
  QueryDate := '2023-07-01';
  WriteLn(Format('   Products as of %s:', [QueryDate]));
  DS := Connection.ExecuteQuery(
    'SELECT id, version, name, price, category ' +
    'FROM products_history ' +
    'WHERE valid_from <= ? AND (valid_to IS NULL OR valid_to > ?) ' +
    'ORDER BY id', [QueryDate, QueryDate]);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s - $%.2f',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  QueryDate := '2024-01-15';
  WriteLn(Format('   Products as of %s:', [QueryDate]));
  DS := Connection.ExecuteQuery(
    'SELECT id, version, name, price, category ' +
    'FROM products_history ' +
    'WHERE valid_from <= ? AND (valid_to IS NULL OR valid_to > ?) ' +
    'ORDER BY id', [QueryDate, QueryDate]);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s - $%.2f',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Uses CTEs to query employee salaries at mid-2023 and mid-2024, then calculates the percentage increase for each employee. }
procedure DemoSalaryComparison;
var
  DS: TDataSet;
begin
  WriteLn('4. Year-over-Year Salary Comparison');
  WriteLn('   ----------------------------------');

  DS := Connection.ExecuteQuery(
    'WITH salary_2023 AS (' +
    '  SELECT id, name, salary ' +
    '  FROM employees_history ' +
    '  WHERE valid_from <= ''2023-06-01'' AND (valid_to IS NULL OR valid_to > ''2023-06-01'')' +
    '),' +
    'salary_2024 AS (' +
    '  SELECT id, name, salary ' +
    '  FROM employees_history ' +
    '  WHERE valid_from <= ''2024-06-01'' AND (valid_to IS NULL OR valid_to > ''2024-06-01'')' +
    ')' +
    'SELECT s24.name, s23.salary as salary_2023, s24.salary as salary_2024, ' +
    '       ROUND((s24.salary - s23.salary) / s23.salary * 100, 1) as increase_pct ' +
    'FROM salary_2024 s24 ' +
    'LEFT JOIN salary_2023 s23 ON s24.id = s23.id ' +
    'WHERE s23.salary IS NOT NULL');
  try
    WriteLn('     Employee       | 2023     | 2024     | Change');
    WriteLn('     ----------------|----------|----------|--------');
    while not DS.EOF do
    begin
      WriteLn(Format('     %-15s | $%7.0f | $%7.0f | %5.1f%%',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('salary_2023').AsFloat,
         DS.FieldByName('salary_2024').AsFloat,
         DS.FieldByName('increase_pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Demonstrates viewing the version history as a change log. }
procedure DemoChangelog;
var
  DS: TDataSet;
begin
  WriteLn('5. Change Log / Audit Trail');
  WriteLn('   --------------------------');

  WriteLn('   All product changes:');
  DS := Connection.ExecuteQuery(
    'SELECT p2.id, p2.version, ' +
    '       COALESCE(p1.name, ''(new)'') as old_name, p2.name as new_name, ' +
    '       COALESCE(p1.price, 0) as old_price, p2.price as new_price, ' +
    '       p2.valid_from as changed_at, p2.modified_by ' +
    'FROM products_history p2 ' +
    'LEFT JOIN products_history p1 ON p2.id = p1.id AND p2.version = p1.version + 1 ' +
    'ORDER BY p2.valid_from, p2.id');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('version').AsInteger = 1 then
        WriteLn(Format('     [%s] Product %d CREATED: "%s" at $%.2f by %s',
          [DS.FieldByName('changed_at').AsString,
           DS.FieldByName('id').AsInteger,
           DS.FieldByName('new_name').AsString,
           DS.FieldByName('new_price').AsFloat,
           DS.FieldByName('modified_by').AsString]))
      else
      begin
        if DS.FieldByName('old_name').AsString <> DS.FieldByName('new_name').AsString then
          WriteLn(Format('     [%s] Product %d RENAMED: "%s" -> "%s" by %s',
            [DS.FieldByName('changed_at').AsString,
             DS.FieldByName('id').AsInteger,
             DS.FieldByName('old_name').AsString,
             DS.FieldByName('new_name').AsString,
             DS.FieldByName('modified_by').AsString]))
        else if DS.FieldByName('old_price').AsFloat <> DS.FieldByName('new_price').AsFloat then
          WriteLn(Format('     [%s] Product %d PRICE CHANGED: $%.2f -> $%.2f by %s',
            [DS.FieldByName('changed_at').AsString,
             DS.FieldByName('id').AsInteger,
             DS.FieldByName('old_price').AsFloat,
             DS.FieldByName('new_price').AsFloat,
             DS.FieldByName('modified_by').AsString]));
      end;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays a contract's bi-temporal history showing both the business validity period and the system recording period for each version. }
procedure DemoBiTemporal;
var
  DS: TDataSet;
begin
  WriteLn('6. Bi-Temporal Data (System + Business Time)');
  WriteLn('   -------------------------------------------');

  WriteLn('   Contract 1 history (Acme Corp):');
  WriteLn('   - Business time: when the contract is effective');
  WriteLn('   - System time: when we recorded this information');
  WriteLn;

  DS := Connection.ExecuteQuery(
    'SELECT version, customer_name, contract_value, ' +
    '       business_start, COALESCE(business_end, ''ongoing'') as business_end, ' +
    '       system_start, COALESCE(system_end, ''(current)'') as system_end ' +
    'FROM contracts WHERE id = 1 ORDER BY version');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     v%d: $%.0f',
        [DS.FieldByName('version').AsInteger,
         DS.FieldByName('contract_value').AsFloat]));
      WriteLn(Format('         Business: %s to %s',
        [DS.FieldByName('business_start').AsString,
         DS.FieldByName('business_end').AsString]));
      WriteLn(Format('         Recorded: %s to %s',
        [DS.FieldByName('system_start').AsString,
         DS.FieldByName('system_end').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   The contract was originally valued at $50K (recorded 2023-01-01).');
  WriteLn('   On 2023-06-15, it was renegotiated to $75K and extended to 2024-06-30.');
  WriteLn;
end;

{ Aggregates version counts per product and displays creation date, last modification date, and current name in a summary table. }
procedure DemoVersionStatistics;
var
  DS: TDataSet;
begin
  WriteLn('7. Version Statistics');
  WriteLn('   --------------------');

  DS := Connection.ExecuteQuery(
    'SELECT id, ' +
    '       COUNT(*) as total_versions, ' +
    '       MIN(valid_from) as first_created, ' +
    '       MAX(valid_from) as last_modified, ' +
    '       (SELECT name FROM products_history p2 WHERE p2.id = p1.id AND p2.is_current = 1) as current_name ' +
    'FROM products_history p1 ' +
    'GROUP BY id ' +
    'ORDER BY total_versions DESC');
  try
    WriteLn('     Product | Versions | Created    | Last Modified | Current Name');
    WriteLn('     --------|----------|------------|---------------|-------------');
    while not DS.EOF do
    begin
      WriteLn(Format('     %7d | %8d | %s | %s | %s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('total_versions').AsInteger,
         DS.FieldByName('first_created').AsString,
         DS.FieldByName('last_modified').AsString,
         DS.FieldByName('current_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints a summary of temporal data best practices including validity columns, current flags, views, indexing, bi-temporal modeling, and retention policies. }
procedure DemoBestPractices;
begin
  WriteLn('8. Temporal Data Best Practices');
  WriteLn('   ------------------------------');
  WriteLn('   - Use valid_from/valid_to for temporal validity');
  WriteLn('   - Keep is_current flag for efficient current data queries');
  WriteLn('   - Create views for current data access');
  WriteLn('   - Index temporal columns (valid_from, valid_to, is_current)');
  WriteLn('   - Use bi-temporal for compliance (system + business time)');
  WriteLn('   - Consider retention policies for old versions');
  WriteLn('   - Implement versioned insert/update procedures');
  WriteLn('   - Use CTEs for complex point-in-time queries');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 59: Versioned Data (Temporal Tables) ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example59.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTemporalTables;
      SetupSampleData;
      DemoCurrentData;
      DemoFullHistory;
      DemoPointInTimeQuery;
      DemoSalaryComparison;
      DemoChangelog;
      DemoBiTemporal;
      DemoVersionStatistics;
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
