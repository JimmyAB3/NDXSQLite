{===============================================================================
  NDXSQLite Example 84 - Database Statistics
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates ANALYZE command and sqlite_stat tables:
  - Running ANALYZE to gather statistics
  - Reading sqlite_stat1 table format
  - Interpreting index selectivity
  - Statistics impact on query plans
  - Table page count analysis
  - Index cardinality estimation
  - Statistics for composite indexes
  - Before/after ANALYZE comparison
  - Database size metrics

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program DatabaseStatistics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

{ Displays query plan information. }
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

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Test Schema');
  WriteLn('   ======================');

  // Employees table
  Conn.ExecuteNonQuery(
    'CREATE TABLE employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  department TEXT NOT NULL,' +
    '  position TEXT,' +
    '  salary REAL,' +
    '  hire_date TEXT,' +
    '  city TEXT,' +
    '  country TEXT,' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Projects table
  Conn.ExecuteNonQuery(
    'CREATE TABLE projects (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  department TEXT,' +
    '  budget REAL,' +
    '  status TEXT DEFAULT ''active'',' +
    '  start_date TEXT,' +
    '  priority INTEGER DEFAULT 3' +
    ')');

  // Assignments table
  Conn.ExecuteNonQuery(
    'CREATE TABLE assignments (' +
    '  id INTEGER PRIMARY KEY,' +
    '  employee_id INTEGER NOT NULL REFERENCES employees(id),' +
    '  project_id INTEGER NOT NULL REFERENCES projects(id),' +
    '  role TEXT,' +
    '  hours_allocated INTEGER DEFAULT 40,' +
    '  start_date TEXT' +
    ')');

  // Timesheets table (high volume)
  Conn.ExecuteNonQuery(
    'CREATE TABLE timesheets (' +
    '  id INTEGER PRIMARY KEY,' +
    '  employee_id INTEGER NOT NULL REFERENCES employees(id),' +
    '  project_id INTEGER NOT NULL REFERENCES projects(id),' +
    '  work_date TEXT NOT NULL,' +
    '  hours REAL NOT NULL,' +
    '  category TEXT DEFAULT ''regular''' +
    ')');

  WriteLn('   Created tables: employees, projects, assignments, timesheets');
  WriteLn('');
end;

// =============================================================================
// Generate Test Data
// =============================================================================
{ Populates the employees, projects, assignments, and timesheets tables with sample data using varied departments, positions, cities, and categories. }
procedure GenerateTestData;
var
  I, EmpId, ProjId: Integer;
  Departments: array[0..4] of string;
  Positions: array[0..3] of string;
  Cities: array[0..9] of string;
  Statuses: array[0..2] of string;
  Categories: array[0..3] of string;
begin
  WriteLn('2. Generating Test Data');
  WriteLn('   ======================');

  Departments[0] := 'Engineering'; Departments[1] := 'Marketing';
  Departments[2] := 'Sales'; Departments[3] := 'HR'; Departments[4] := 'Finance';

  Positions[0] := 'Junior'; Positions[1] := 'Mid';
  Positions[2] := 'Senior'; Positions[3] := 'Lead';

  Cities[0] := 'Paris'; Cities[1] := 'London'; Cities[2] := 'Berlin';
  Cities[3] := 'Madrid'; Cities[4] := 'Rome'; Cities[5] := 'Amsterdam';
  Cities[6] := 'Brussels'; Cities[7] := 'Vienna'; Cities[8] := 'Lisbon';
  Cities[9] := 'Dublin';

  Statuses[0] := 'active'; Statuses[1] := 'completed'; Statuses[2] := 'paused';

  Categories[0] := 'regular'; Categories[1] := 'overtime';
  Categories[2] := 'training'; Categories[3] := 'meeting';

  // Create employees (500)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 500 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO employees (name, department, position, salary, hire_date, city, country, is_active) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
      [Format('Employee_%d', [I]),
       Departments[I mod 5],
       Positions[I mod 4],
       30000.0 + (I mod 50) * 1500.0,
       Format('20%s-%s-01', [Format('%.2d', [15 + (I mod 10)]), Format('%.2d', [(I mod 12) + 1])]),
       Cities[I mod 10],
       IfThen(I mod 10 < 5, 'France', 'Germany'),
       IfThen(I mod 20 = 0, '0', '1')]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 500 employees');

  // Create projects (100)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 100 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO projects (name, department, budget, status, start_date, priority) VALUES (?, ?, ?, ?, ?, ?)',
      [Format('Project_%d', [I]),
       Departments[I mod 5],
       50000.0 + (I mod 20) * 25000.0,
       Statuses[I mod 3],
       Format('2024-%s-15', [Format('%.2d', [(I mod 12) + 1])]),
       (I mod 5) + 1]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 100 projects');

  // Create assignments (1000)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 1000 do
  begin
    EmpId := (I mod 500) + 1;
    ProjId := (I mod 100) + 1;
    Conn.ExecuteNonQuery(
      'INSERT INTO assignments (employee_id, project_id, role, hours_allocated, start_date) VALUES (?, ?, ?, ?, ?)',
      [EmpId, ProjId,
       Positions[I mod 4],
       20 + (I mod 30),
       Format('2024-%s-01', [Format('%.2d', [(I mod 12) + 1])])]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 1000 assignments');

  // Create timesheets (10000)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 10000 do
  begin
    EmpId := (I mod 500) + 1;
    ProjId := (I mod 100) + 1;
    Conn.ExecuteNonQuery(
      'INSERT INTO timesheets (employee_id, project_id, work_date, hours, category) VALUES (?, ?, ?, ?, ?)',
      [EmpId, ProjId,
       Format('2024-%s-%s', [Format('%.2d', [(I mod 12) + 1]), Format('%.2d', [(I mod 28) + 1])]),
       1.0 + (I mod 8),
       Categories[I mod 4]]);
  end;
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Created 10000 timesheets');
  WriteLn('');
end;

// =============================================================================
// Demo: Create Indexes
// =============================================================================
{ Creates indexes on the sample tables for statistics analysis. }
procedure CreateIndexes;
begin
  WriteLn('3. Creating Indexes');
  WriteLn('   ===================');

  // Single column indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_emp_department ON employees(department)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_emp_city ON employees(city)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_emp_salary ON employees(salary)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_emp_active ON employees(is_active)');

  // Composite indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_emp_dept_pos ON employees(department, position)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_emp_country_city ON employees(country, city)');

  // Project indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_proj_dept ON projects(department)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_proj_status ON projects(status)');

  // Assignment indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_assign_emp ON assignments(employee_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_assign_proj ON assignments(project_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_assign_emp_proj ON assignments(employee_id, project_id)');

  // Timesheet indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_ts_emp ON timesheets(employee_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_ts_proj ON timesheets(project_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_ts_date ON timesheets(work_date)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_ts_category ON timesheets(category)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_ts_emp_date ON timesheets(employee_id, work_date)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_ts_emp_proj_date ON timesheets(employee_id, project_id, work_date)');

  WriteLn('   Created 17 indexes across all tables');
  WriteLn('');
end;

// =============================================================================
// Demo: Before ANALYZE
// =============================================================================
{ Checks whether sqlite_stat1 exists and displays query plans before ANALYZE has been run. }
procedure DemoBeforeAnalyze;
var
  DS: TDataSet;
  StatExists: Boolean;
begin
  WriteLn('4. Before ANALYZE');
  WriteLn('   =================');
  WriteLn('');

  // Check if sqlite_stat1 exists
  StatExists := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE name = ''sqlite_stat1''')) > 0;
  WriteLn('   sqlite_stat1 exists: ' + IfThen(StatExists, 'Yes', 'No'));

  if StatExists then
  begin
    DS := Conn.ExecuteQuery('SELECT COUNT(*) AS cnt FROM sqlite_stat1');
    try
      WriteLn('   sqlite_stat1 rows: ' + DS.FieldByName('cnt').AsString);
    finally
      DS.Free;
    end;
  end;

  WriteLn('');
  WriteLn('   Query plans WITHOUT statistics:');
  WriteLn('   SELECT * FROM employees WHERE department = ''Engineering'':');
  ShowQueryPlan('SELECT * FROM employees WHERE department = ''Engineering''');

  WriteLn('   SELECT * FROM timesheets WHERE category = ''overtime'':');
  ShowQueryPlan('SELECT * FROM timesheets WHERE category = ''overtime''');

  WriteLn('');
end;

// =============================================================================
// Demo: Running ANALYZE
// =============================================================================
{ Executes the ANALYZE command and displays the resulting sqlite_stat1 table entries with table, index, and stat columns. }
procedure DemoRunAnalyze;
var
  DS: TDataSet;
  RowCount: Integer;
begin
  WriteLn('5. Running ANALYZE');
  WriteLn('   ==================');
  WriteLn('');

  WriteLn('   Executing: ANALYZE');
  Conn.ExecuteNonQuery('ANALYZE');
  WriteLn('   ANALYZE completed - statistics gathered');

  // Check sqlite_stat1
  WriteLn('');
  RowCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_stat1'));
  WriteLn(Format('   sqlite_stat1 rows: %d', [RowCount]));

  // Show first few entries
  WriteLn('');
  WriteLn(Format('   %-22s | %-30s | %s', ['Table', 'Index', 'Stat']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery('SELECT tbl, idx, stat FROM sqlite_stat1 ORDER BY tbl, idx');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s | %-30s | %s',
        [DS.FieldByName('tbl').AsString,
         DS.FieldByName('idx').AsString,
         DS.FieldByName('stat').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Interpreting Statistics
// =============================================================================
{ Parses sqlite_stat1 entries for selected indexes and computes total rows, average rows per key, distinct key count, and selectivity percentage. }
procedure DemoInterpretStats;
var
  DS: TDataSet;
  StatStr, Token: string;
  TotalRows, AvgPerKey: Integer;
  Selectivity: Double;
  P: Integer;
begin
  WriteLn('6. Interpreting Statistics');
  WriteLn('   =========================');
  WriteLn('');

  WriteLn('   Format: "total_rows avg_rows_per_key [avg_rows_per_prefix...]"');
  WriteLn('');
  WriteLn('   For single-column index:');
  WriteLn('     stat = "500 100" means:');
  WriteLn('       - 500 rows in table');
  WriteLn('       - Average 100 rows per distinct key value');
  WriteLn('       - ~5 distinct values (500/100)');
  WriteLn('');
  WriteLn('   For composite index (a, b):');
  WriteLn('     stat = "500 25 5" means:');
  WriteLn('       - 500 rows in table');
  WriteLn('       - Average 25 rows per distinct (a) prefix');
  WriteLn('       - Average 5 rows per distinct (a, b) combination');
  WriteLn('');

  // Analyze specific indexes
  WriteLn('   Detailed analysis of key indexes:');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT tbl, idx, stat FROM sqlite_stat1 ' +
    'WHERE idx IN (''idx_emp_department'', ''idx_emp_dept_pos'', ' +
    '''idx_ts_category'', ''idx_ts_emp_proj_date'', ''idx_emp_active'') ' +
    'ORDER BY tbl, idx');
  try
    while not DS.EOF do
    begin
      StatStr := DS.FieldByName('stat').AsString;
      WriteLn(Format('   %s.%s:', [DS.FieldByName('tbl').AsString, DS.FieldByName('idx').AsString]));
      WriteLn(Format('     stat = "%s"', [StatStr]));

      // Parse first two numbers
      P := Pos(' ', StatStr);
      if P > 0 then
      begin
        TotalRows := StrToIntDef(Copy(StatStr, 1, P - 1), 0);
        Token := Copy(StatStr, P + 1, Length(StatStr));
        P := Pos(' ', Token);
        if P > 0 then
          AvgPerKey := StrToIntDef(Copy(Token, 1, P - 1), 1)
        else
          AvgPerKey := StrToIntDef(Token, 1);

        if AvgPerKey > 0 then
        begin
          Selectivity := 1.0 / AvgPerKey * 100.0;
          WriteLn(Format('     Total rows: %d, Avg/key: %d, Distinct keys: ~%d, Selectivity: %.1f%%',
            [TotalRows, AvgPerKey, TotalRows div AvgPerKey, Selectivity]));
        end;
      end;

      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// =============================================================================
// Demo: After ANALYZE Query Plans
// =============================================================================
{ Displays query plans for single-condition, multi-condition, and JOIN queries after ANALYZE has populated statistics. }
procedure DemoAfterAnalyze;
begin
  WriteLn('7. Query Plans AFTER ANALYZE');
  WriteLn('   ============================');
  WriteLn('');

  WriteLn('   SQLite now uses statistics to choose better plans:');
  WriteLn('');

  WriteLn('   SELECT * FROM employees WHERE department = ''Engineering'':');
  ShowQueryPlan('SELECT * FROM employees WHERE department = ''Engineering''');

  WriteLn('');
  WriteLn('   SELECT * FROM timesheets WHERE category = ''overtime'':');
  ShowQueryPlan('SELECT * FROM timesheets WHERE category = ''overtime''');

  // Multi-condition query - optimizer uses stats to pick best index
  WriteLn('');
  WriteLn('   Multi-condition (optimizer picks most selective index):');
  WriteLn('   WHERE department = ''Engineering'' AND is_active = 1:');
  ShowQueryPlan('SELECT * FROM employees WHERE department = ''Engineering'' AND is_active = 1');

  WriteLn('');
  WriteLn('   JOIN with statistics (better join order):');
  ShowQueryPlan(
    'SELECT e.name, t.hours FROM employees e ' +
    'JOIN timesheets t ON e.id = t.employee_id ' +
    'WHERE e.department = ''Sales'' AND t.category = ''overtime''');

  WriteLn('');
end;

// =============================================================================
// Demo: Index Selectivity Ranking
// =============================================================================
{ Queries sqlite_stat1 and ranks all indexes by their average rows per key, displaying selectivity percentages from most to least selective. }
procedure DemoSelectivityRanking;
var
  DS: TDataSet;
  StatStr: string;
  TotalRows, AvgPerKey, Rank: Integer;
  P: Integer;
begin
  WriteLn('8. Index Selectivity Ranking');
  WriteLn('   ============================');
  WriteLn('');

  WriteLn('   Lower avg/key = more selective = better for filtering');
  WriteLn('');
  WriteLn(Format('   %-5s | %-30s | %-8s | %-8s | %s',
    ['Rank', 'Index', 'Rows', 'Avg/Key', 'Selectivity']));
  WriteLn('   ' + StringOfChar('-', 75));

  Rank := 0;
  DS := Conn.ExecuteQuery(
    'SELECT tbl, idx, stat FROM sqlite_stat1 ' +
    'WHERE idx NOT LIKE ''sqlite%'' ' +
    'ORDER BY CAST(substr(stat, instr(stat, '' '') + 1, ' +
    'CASE WHEN instr(substr(stat, instr(stat, '' '') + 1), '' '') > 0 ' +
    'THEN instr(substr(stat, instr(stat, '' '') + 1), '' '') - 1 ' +
    'ELSE length(stat) END) AS INTEGER)');
  try
    while not DS.EOF do
    begin
      StatStr := DS.FieldByName('stat').AsString;
      P := Pos(' ', StatStr);
      if P > 0 then
      begin
        TotalRows := StrToIntDef(Copy(StatStr, 1, P - 1), 0);
        AvgPerKey := StrToIntDef(Copy(StatStr, P + 1, Pos(' ', StatStr + ' ', P + 1) - P - 1), 1);
        if AvgPerKey < 1 then AvgPerKey := 1;
        Inc(Rank);
        WriteLn(Format('   %-5d | %-30s | %-8d | %-8d | %.2f%%',
          [Rank, DS.FieldByName('idx').AsString, TotalRows, AvgPerKey, 1.0 / AvgPerKey * 100.0]));
      end;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Table Size Statistics
// =============================================================================
{ Retrieves database page size, page count, and freelist count via PRAGMAs and displays row counts for each table. }
procedure DemoTableStats;
var
  DS: TDataSet;
  PageSize, PageCount, FreePages: Integer;
begin
  WriteLn('9. Database Size Statistics');
  WriteLn('   ===========================');
  WriteLn('');

  // Database-level stats
  PageSize := Integer(Conn.ExecuteScalar('PRAGMA page_size'));
  PageCount := Integer(Conn.ExecuteScalar('PRAGMA page_count'));
  FreePages := Integer(Conn.ExecuteScalar('PRAGMA freelist_count'));

  WriteLn(Format('   Page size:       %d bytes', [PageSize]));
  WriteLn(Format('   Total pages:     %d', [PageCount]));
  WriteLn(Format('   Free pages:      %d', [FreePages]));
  WriteLn(Format('   Database size:   %d KB', [(PageSize * PageCount) div 1024]));
  WriteLn(Format('   Used space:      %d KB', [(PageSize * (PageCount - FreePages)) div 1024]));
  WriteLn('');

  // Row counts per table
  WriteLn('   Table row counts:');
  WriteLn(Format('   %-20s | %s', ['Table', 'Rows']));
  WriteLn('   ' + StringOfChar('-', 35));

  DS := Conn.ExecuteQuery(
    'SELECT ''employees'' AS tbl, COUNT(*) AS cnt FROM employees UNION ALL ' +
    'SELECT ''projects'', COUNT(*) FROM projects UNION ALL ' +
    'SELECT ''assignments'', COUNT(*) FROM assignments UNION ALL ' +
    'SELECT ''timesheets'', COUNT(*) FROM timesheets');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s | %s', [DS.FieldByName('tbl').AsString, DS.FieldByName('cnt').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Composite Index Statistics
// =============================================================================
{ Parses multi-value stat strings for composite indexes and displays prefix selectivity at each column level. }
procedure DemoCompositeStats;
var
  DS: TDataSet;
  StatStr, Token: string;
  P1, P2, P3: Integer;
  TotalRows, Avg1, Avg2, Avg3: Integer;
begin
  WriteLn('10. Composite Index Statistics');
  WriteLn('    ==============================');
  WriteLn('');

  WriteLn('    Multi-column statistics show prefix selectivity:');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT tbl, idx, stat FROM sqlite_stat1 ' +
    'WHERE idx IN (''idx_emp_dept_pos'', ''idx_emp_country_city'', ' +
    '''idx_assign_emp_proj'', ''idx_ts_emp_date'', ''idx_ts_emp_proj_date'')');
  try
    while not DS.EOF do
    begin
      StatStr := DS.FieldByName('stat').AsString;
      WriteLn(Format('    %s (%s):', [DS.FieldByName('idx').AsString, DS.FieldByName('tbl').AsString]));
      WriteLn(Format('      stat = "%s"', [StatStr]));

      // Parse stat values
      P1 := Pos(' ', StatStr);
      if P1 > 0 then
      begin
        TotalRows := StrToIntDef(Copy(StatStr, 1, P1 - 1), 0);
        Token := Copy(StatStr, P1 + 1, Length(StatStr));

        P2 := Pos(' ', Token);
        if P2 > 0 then
        begin
          Avg1 := StrToIntDef(Copy(Token, 1, P2 - 1), 1);
          Token := Copy(Token, P2 + 1, Length(Token));

          P3 := Pos(' ', Token);
          if P3 > 0 then
          begin
            Avg2 := StrToIntDef(Copy(Token, 1, P3 - 1), 1);
            Avg3 := StrToIntDef(Copy(Token, P3 + 1, Length(Token)), 1);
            WriteLn(Format('      Rows: %d | Col1 avg: %d (~%d distinct) | Col1+2 avg: %d | Col1+2+3 avg: %d',
              [TotalRows, Avg1, TotalRows div Avg1, Avg2, Avg3]));
          end
          else
          begin
            Avg2 := StrToIntDef(Token, 1);
            WriteLn(Format('      Rows: %d | Col1 avg: %d (~%d distinct) | Col1+2 avg: %d (~%d distinct)',
              [TotalRows, Avg1, TotalRows div Avg1, Avg2, TotalRows div Avg2]));
          end;
        end
        else
        begin
          Avg1 := StrToIntDef(Token, 1);
          WriteLn(Format('      Rows: %d | Avg/key: %d (~%d distinct)',
            [TotalRows, Avg1, TotalRows div Avg1]));
        end;
      end;

      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// =============================================================================
// Demo: ANALYZE on Specific Tables
// =============================================================================
{ Inserts additional timesheet rows, then runs ANALYZE on only the timesheets table and compares stat values before and after. }
procedure DemoPartialAnalyze;
var
  DS: TDataSet;
  CountBefore, CountAfter: Integer;
begin
  WriteLn('11. Partial ANALYZE');
  WriteLn('    ===================');
  WriteLn('');

  // Add new data
  WriteLn('    Adding 500 new timesheets...');
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for CountBefore := 10001 to 10500 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO timesheets (employee_id, project_id, work_date, hours, category) VALUES (?, ?, ?, ?, ?)',
      [(CountBefore mod 500) + 1, (CountBefore mod 100) + 1,
       '2024-12-15', 4.0, 'overtime']);
  end;
  Conn.ExecuteNonQuery('COMMIT');

  // Show current stats
  DS := Conn.ExecuteQuery('SELECT stat FROM sqlite_stat1 WHERE idx = ''idx_ts_category''');
  try
    WriteLn('    idx_ts_category stat BEFORE re-analyze: ' + DS.FieldByName('stat').AsString);
  finally
    DS.Free;
  end;

  // Analyze just timesheets table
  WriteLn('    Running: ANALYZE timesheets');
  Conn.ExecuteNonQuery('ANALYZE timesheets');

  DS := Conn.ExecuteQuery('SELECT stat FROM sqlite_stat1 WHERE idx = ''idx_ts_category''');
  try
    WriteLn('    idx_ts_category stat AFTER re-analyze:  ' + DS.FieldByName('stat').AsString);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('    Total timesheets now: ' +
    IntToStr(Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM timesheets'))));

  WriteLn('');
end;

// =============================================================================
// Demo: Cardinality Analysis
// =============================================================================
{ Computes distinct value counts and cardinality ratios for each indexed column across all tables. }
procedure DemoCardinalityAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('12. Cardinality Analysis');
  WriteLn('    ========================');
  WriteLn('');

  WriteLn('    Distinct value counts per indexed column:');
  WriteLn('');
  WriteLn(Format('    %-20s | %-20s | %-10s | %-10s | %s',
    ['Table', 'Column', 'Distinct', 'Total', 'Ratio']));
  WriteLn('    ' + StringOfChar('-', 80));

  DS := Conn.ExecuteQuery(
    'SELECT ''employees'' AS tbl, ''department'' AS col, ' +
    'COUNT(DISTINCT department) AS dist, COUNT(*) AS total FROM employees UNION ALL ' +
    'SELECT ''employees'', ''position'', COUNT(DISTINCT position), COUNT(*) FROM employees UNION ALL ' +
    'SELECT ''employees'', ''city'', COUNT(DISTINCT city), COUNT(*) FROM employees UNION ALL ' +
    'SELECT ''employees'', ''country'', COUNT(DISTINCT country), COUNT(*) FROM employees UNION ALL ' +
    'SELECT ''employees'', ''is_active'', COUNT(DISTINCT is_active), COUNT(*) FROM employees UNION ALL ' +
    'SELECT ''timesheets'', ''category'', COUNT(DISTINCT category), COUNT(*) FROM timesheets UNION ALL ' +
    'SELECT ''timesheets'', ''employee_id'', COUNT(DISTINCT employee_id), COUNT(*) FROM timesheets UNION ALL ' +
    'SELECT ''timesheets'', ''project_id'', COUNT(DISTINCT project_id), COUNT(*) FROM timesheets UNION ALL ' +
    'SELECT ''projects'', ''status'', COUNT(DISTINCT status), COUNT(*) FROM projects UNION ALL ' +
    'SELECT ''projects'', ''department'', COUNT(DISTINCT department), COUNT(*) FROM projects');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %-20s | %-20s | %-10s | %-10s | %.1f%%',
        [DS.FieldByName('tbl').AsString,
         DS.FieldByName('col').AsString,
         DS.FieldByName('dist').AsString,
         DS.FieldByName('total').AsString,
         DS.FieldByName('dist').AsFloat / DS.FieldByName('total').AsFloat * 100.0]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('    Higher ratio = more selective = better index candidate');
  WriteLn('');
end;

// =============================================================================
// Demo: Statistics Summary
// =============================================================================
{ Displays total index count, sqlite_stat1 entry count, sqlite_stat4 presence, and the number of indexes per table. }
procedure DemoStatsSummary;
var
  DS: TDataSet;
  IdxCount, StatCount: Integer;
begin
  WriteLn('13. Statistics Summary');
  WriteLn('    ======================');
  WriteLn('');

  IdxCount := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE type = ''index'''));
  StatCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_stat1'));

  WriteLn(Format('    Total indexes:        %d', [IdxCount]));
  WriteLn(Format('    Statistics entries:    %d', [StatCount]));

  // Check for sqlite_stat4
  if Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE name = ''sqlite_stat4''')) > 0 then
  begin
    WriteLn(Format('    sqlite_stat4 entries: %d',
      [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_stat4'))]));
  end
  else
    WriteLn('    sqlite_stat4:         Not present (compile-time option)');

  WriteLn('');

  // Index count per table
  WriteLn('    Indexes per table:');
  DS := Conn.ExecuteQuery(
    'SELECT tbl_name, COUNT(*) AS cnt FROM sqlite_master ' +
    'WHERE type = ''index'' AND sql IS NOT NULL ' +
    'GROUP BY tbl_name ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('      %-20s: %s indexes',
        [DS.FieldByName('tbl_name').AsString,
         DS.FieldByName('cnt').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 84: Database Statistics ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    GenerateTestData;
    CreateIndexes;
    DemoBeforeAnalyze;
    DemoRunAnalyze;
    DemoInterpretStats;
    DemoAfterAnalyze;
    DemoSelectivityRanking;
    DemoTableStats;
    DemoCompositeStats;
    DemoPartialAnalyze;
    DemoCardinalityAnalysis;
    DemoStatsSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
