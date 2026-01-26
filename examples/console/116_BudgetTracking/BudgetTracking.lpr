{===============================================================================
  NDXSQLite Example 116 - Budget Tracking
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Budget category and allocation management
  - Expense tracking with budget validation
  - Threshold-based budget alerts
  - Period-based budget rollover
  - Budget vs actual reporting

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program BudgetTracking;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Budget categories
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS budget_categories (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL UNIQUE,' +
    '  description TEXT,' +
    '  is_active INTEGER DEFAULT 1' +
    ')'
  );

  // Budget allocations per category per period
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS budget_allocations (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  category_id INTEGER NOT NULL,' +
    '  period TEXT NOT NULL,' +
    '  allocated_amount REAL NOT NULL DEFAULT 0,' +
    '  rollover_amount REAL NOT NULL DEFAULT 0,' +
    '  notes TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  UNIQUE(category_id, period),' +
    '  FOREIGN KEY (category_id) REFERENCES budget_categories(id)' +
    ')'
  );

  // Individual expenses
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS expenses (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  category_id INTEGER NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  description TEXT,' +
    '  expense_date TEXT NOT NULL,' +
    '  period TEXT NOT NULL,' +
    '  approved_by TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  FOREIGN KEY (category_id) REFERENCES budget_categories(id)' +
    ')'
  );

  // Budget alerts
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS budget_alerts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  category_id INTEGER NOT NULL,' +
    '  period TEXT NOT NULL,' +
    '  alert_type TEXT NOT NULL,' +
    '  threshold_pct REAL NOT NULL,' +
    '  current_pct REAL NOT NULL,' +
    '  message TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  FOREIGN KEY (category_id) REFERENCES budget_categories(id)' +
    ')'
  );

  // Alert thresholds configuration
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS alert_thresholds (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  threshold_pct REAL NOT NULL UNIQUE,' +
    '  alert_type TEXT NOT NULL,' +
    '  is_active INTEGER DEFAULT 1' +
    ')'
  );

  // Default alert thresholds: 75% warning, 90% critical, 100% overspend
  Conn.ExecuteNonQuery(
    'INSERT INTO alert_thresholds (threshold_pct, alert_type) VALUES (75, ''warning'')'
  );
  Conn.ExecuteNonQuery(
    'INSERT INTO alert_thresholds (threshold_pct, alert_type) VALUES (90, ''critical'')'
  );
  Conn.ExecuteNonQuery(
    'INSERT INTO alert_thresholds (threshold_pct, alert_type) VALUES (100, ''overspend'')'
  );
end;

{ Inserts a new budget category with the given name and description, and returns
  the auto-generated category ID. }
function CreateCategory(const Name, Description: string): Integer;
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO budget_categories (name, description) VALUES (?, ?)',
    [Name, Description]
  );
  Result := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
end;

{ Looks up a budget category by name and returns its ID, or -1 if not found. }
function GetCategoryId(const Name: string): Integer;
var
  V: Variant;
begin
  V := Conn.ExecuteScalar(
    'SELECT id FROM budget_categories WHERE name = ?', [Name]
  );
  if VarIsNull(V) then
    Result := -1
  else
    Result := Integer(V);
end;

{ Inserts or replaces a budget allocation for a category and period, preserving
  any existing rollover amount. }
procedure AllocateBudget(CategoryId: Integer; const Period: string;
  Amount: Double; const Notes: string);
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO budget_allocations (category_id, period, allocated_amount, rollover_amount, notes, created_at) ' +
    'VALUES (?, ?, ?, COALESCE((SELECT rollover_amount FROM budget_allocations WHERE category_id = ? AND period = ?), 0), ?, ?)',
    [CategoryId, Period, Amount, CategoryId, Period, Notes, Now_]
  );
end;

{ Adds an expense record and returns its ID. }
function AddExpense(CategoryId: Integer; Amount: Double;
  const Description, ExpenseDate, Period, ApprovedBy: string): Integer;
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'INSERT INTO expenses (category_id, amount, description, expense_date, period, approved_by, created_at) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?)',
    [CategoryId, Amount, Description, ExpenseDate, Period, ApprovedBy, Now_]
  );
  Result := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
end;

{ Calculates the spending percentage for a category in a period and generates
  budget alerts (warning, critical, overspend) for each threshold exceeded. }
procedure CheckBudgetAlerts(CategoryId: Integer; const Period: string);
var
  DS: TDataSet;
  Allocated, Spent, Pct: Double;
  ThresholdDS: TDataSet;
  ThresholdPct: Double;
  AlertType, CatName: string;
  Now_: string;
begin
  // Get total budget (allocated + rollover)
  DS := Conn.ExecuteQuery(
    'SELECT allocated_amount + rollover_amount as total_budget ' +
    'FROM budget_allocations WHERE category_id = ? AND period = ?',
    [CategoryId, Period]
  );
  try
    if DS.EOF then Exit;
    Allocated := DS.FieldByName('total_budget').AsFloat;
  finally
    DS.Free;
  end;

  if Allocated <= 0 then Exit;

  // Get total spent
  DS := Conn.ExecuteQuery(
    'SELECT COALESCE(SUM(amount), 0) as total_spent FROM expenses ' +
    'WHERE category_id = ? AND period = ?',
    [CategoryId, Period]
  );
  try
    Spent := DS.FieldByName('total_spent').AsFloat;
  finally
    DS.Free;
  end;

  Pct := (Spent / Allocated) * 100.0;

  // Get category name
  CatName := VarToStr(Conn.ExecuteScalar(
    'SELECT name FROM budget_categories WHERE id = ?', [CategoryId]
  ));

  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Check each threshold
  ThresholdDS := Conn.ExecuteQuery(
    'SELECT threshold_pct, alert_type FROM alert_thresholds ' +
    'WHERE is_active = 1 ORDER BY threshold_pct',
    []
  );
  try
    while not ThresholdDS.EOF do
    begin
      ThresholdPct := ThresholdDS.FieldByName('threshold_pct').AsFloat;
      AlertType := ThresholdDS.FieldByName('alert_type').AsString;

      if Pct >= ThresholdPct then
      begin
        // Check if alert already exists for this category/period/type
        DS := Conn.ExecuteQuery(
          'SELECT id FROM budget_alerts WHERE category_id = ? AND period = ? AND alert_type = ?',
          [CategoryId, Period, AlertType]
        );
        try
          if DS.EOF then
          begin
            Conn.ExecuteNonQuery(
              'INSERT INTO budget_alerts (category_id, period, alert_type, threshold_pct, current_pct, message, created_at) ' +
              'VALUES (?, ?, ?, ?, ?, ?, ?)',
              [CategoryId, Period, AlertType, ThresholdPct, Pct,
               Format('%s: %.1f%% of budget used ($%.2f / $%.2f)', [CatName, Pct, Spent, Allocated]),
               Now_]
            );
          end;
        finally
          DS.Free;
        end;
      end;

      ThresholdDS.Next;
    end;
  finally
    ThresholdDS.Free;
  end;
end;

{ Calculates unused budget (allocated minus spent) for each category in the source
  period and carries the remaining amounts forward as rollover into the target period. }
procedure PerformRollover(const FromPeriod, ToPeriod: string);
var
  DS: TDataSet;
  CatId: Integer;
  Allocated, Spent, Remaining: Double;
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  DS := Conn.ExecuteQuery(
    'SELECT ba.category_id, ba.allocated_amount + ba.rollover_amount as total_budget, ' +
    'COALESCE((SELECT SUM(amount) FROM expenses WHERE category_id = ba.category_id AND period = ?), 0) as spent ' +
    'FROM budget_allocations ba WHERE ba.period = ?',
    [FromPeriod, FromPeriod]
  );
  try
    while not DS.EOF do
    begin
      CatId := DS.FieldByName('category_id').AsInteger;
      Allocated := DS.FieldByName('total_budget').AsFloat;
      Spent := DS.FieldByName('spent').AsFloat;
      Remaining := Allocated - Spent;

      if Remaining > 0 then
      begin
        // Update rollover for next period (create allocation if not exists)
        Conn.ExecuteNonQuery(
          'INSERT INTO budget_allocations (category_id, period, allocated_amount, rollover_amount, notes, created_at) ' +
          'VALUES (?, ?, 0, ?, ?, ?) ' +
          'ON CONFLICT(category_id, period) DO UPDATE SET rollover_amount = ?',
          [CatId, ToPeriod, Remaining,
           Format('Rollover from %s: $%.2f', [FromPeriod, Remaining]),
           Now_, Remaining]
        );
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Creates five budget categories (Marketing, Engineering, Operations, Travel, HR)
  and prints their names and descriptions. }
procedure DemoBudgetCategories;
begin
  WriteLn('=== 1. Budget Categories ===');
  WriteLn;

  CreateCategory('Marketing', 'Advertising, promotions, events');
  CreateCategory('Engineering', 'Software tools, infrastructure, training');
  CreateCategory('Operations', 'Office supplies, facilities, logistics');
  CreateCategory('Travel', 'Business travel, conferences, lodging');
  CreateCategory('HR', 'Recruitment, benefits, team events');

  WriteLn('   Created 5 budget categories:');
  WriteLn('     - Marketing (Advertising, promotions, events)');
  WriteLn('     - Engineering (Software tools, infrastructure, training)');
  WriteLn('     - Operations (Office supplies, facilities, logistics)');
  WriteLn('     - Travel (Business travel, conferences, lodging)');
  WriteLn('     - HR (Recruitment, benefits, team events)');
  WriteLn;
end;

{ Allocates monthly budgets for all five categories across January through March
  2024 and prints the per-category and total monthly allocation amounts. }
procedure DemoBudgetAllocations;
var
  CatMarketing, CatEngineering, CatOperations, CatTravel, CatHR: Integer;
begin
  WriteLn('=== 2. Budget Allocations (Q1 2024) ===');
  WriteLn;

  CatMarketing := GetCategoryId('Marketing');
  CatEngineering := GetCategoryId('Engineering');
  CatOperations := GetCategoryId('Operations');
  CatTravel := GetCategoryId('Travel');
  CatHR := GetCategoryId('HR');

  // January allocations
  AllocateBudget(CatMarketing, '2024-01', 15000, 'Q1 Marketing budget');
  AllocateBudget(CatEngineering, '2024-01', 25000, 'Q1 Engineering budget');
  AllocateBudget(CatOperations, '2024-01', 8000, 'Q1 Operations budget');
  AllocateBudget(CatTravel, '2024-01', 5000, 'Q1 Travel budget');
  AllocateBudget(CatHR, '2024-01', 10000, 'Q1 HR budget');

  // February allocations
  AllocateBudget(CatMarketing, '2024-02', 15000, 'Feb Marketing');
  AllocateBudget(CatEngineering, '2024-02', 25000, 'Feb Engineering');
  AllocateBudget(CatOperations, '2024-02', 8000, 'Feb Operations');
  AllocateBudget(CatTravel, '2024-02', 5000, 'Feb Travel');
  AllocateBudget(CatHR, '2024-02', 10000, 'Feb HR');

  // March allocations
  AllocateBudget(CatMarketing, '2024-03', 15000, 'Mar Marketing');
  AllocateBudget(CatEngineering, '2024-03', 25000, 'Mar Engineering');
  AllocateBudget(CatOperations, '2024-03', 8000, 'Mar Operations');
  AllocateBudget(CatTravel, '2024-03', 5000, 'Mar Travel');
  AllocateBudget(CatHR, '2024-03', 10000, 'Mar HR');

  WriteLn('   Monthly allocations (Jan-Mar 2024):');
  WriteLn(Format('     Marketing:   $%s/month', [FormatFloat('#,##0', 15000)]));
  WriteLn(Format('     Engineering: $%s/month', [FormatFloat('#,##0', 25000)]));
  WriteLn(Format('     Operations:  $%s/month', [FormatFloat('#,##0', 8000)]));
  WriteLn(Format('     Travel:      $%s/month', [FormatFloat('#,##0', 5000)]));
  WriteLn(Format('     HR:          $%s/month', [FormatFloat('#,##0', 10000)]));
  WriteLn(Format('     Total monthly: $%s', [FormatFloat('#,##0', 63000)]));
  WriteLn;
end;

{ Records detailed expense entries across all categories for January and February
  2024, including amounts, descriptions, dates, and approvers. }
procedure DemoExpenseTracking;
var
  CatMarketing, CatEngineering, CatOperations, CatTravel, CatHR: Integer;
begin
  WriteLn('=== 3. Expense Tracking (January 2024) ===');
  WriteLn;

  CatMarketing := GetCategoryId('Marketing');
  CatEngineering := GetCategoryId('Engineering');
  CatOperations := GetCategoryId('Operations');
  CatTravel := GetCategoryId('Travel');
  CatHR := GetCategoryId('HR');

  // Marketing expenses (January) - near budget
  AddExpense(CatMarketing, 5000, 'Google Ads campaign', '2024-01-05', '2024-01', 'J. Smith');
  AddExpense(CatMarketing, 3500, 'Trade show booth', '2024-01-12', '2024-01', 'J. Smith');
  AddExpense(CatMarketing, 2800, 'Social media ads', '2024-01-18', '2024-01', 'A. Jones');
  AddExpense(CatMarketing, 2200, 'Email platform subscription', '2024-01-22', '2024-01', 'J. Smith');

  // Engineering expenses (January) - under budget
  AddExpense(CatEngineering, 8000, 'Cloud infrastructure (AWS)', '2024-01-01', '2024-01', 'M. Lee');
  AddExpense(CatEngineering, 3500, 'JetBrains licenses', '2024-01-10', '2024-01', 'M. Lee');
  AddExpense(CatEngineering, 4200, 'CI/CD pipeline upgrade', '2024-01-15', '2024-01', 'R. Chen');
  AddExpense(CatEngineering, 2000, 'Security audit tools', '2024-01-25', '2024-01', 'M. Lee');

  // Operations (January) - over budget
  AddExpense(CatOperations, 3000, 'Office furniture', '2024-01-03', '2024-01', 'P. Wilson');
  AddExpense(CatOperations, 2500, 'Cleaning service', '2024-01-10', '2024-01', 'P. Wilson');
  AddExpense(CatOperations, 1800, 'Printer supplies', '2024-01-17', '2024-01', 'P. Wilson');
  AddExpense(CatOperations, 1500, 'Emergency HVAC repair', '2024-01-28', '2024-01', 'P. Wilson');

  // Travel (January) - well under budget
  AddExpense(CatTravel, 1200, 'NYC client meeting - flights', '2024-01-08', '2024-01', 'S. Park');
  AddExpense(CatTravel, 800, 'NYC client meeting - hotel', '2024-01-09', '2024-01', 'S. Park');

  // HR (January)
  AddExpense(CatHR, 4000, 'Recruiter fees', '2024-01-05', '2024-01', 'L. Brown');
  AddExpense(CatHR, 2500, 'Team building event', '2024-01-20', '2024-01', 'L. Brown');
  AddExpense(CatHR, 1500, 'Training platform', '2024-01-25', '2024-01', 'L. Brown');

  // Show January summary
  WriteLn('   January 2024 expenses recorded:');
  WriteLn(Format('     Marketing:   $%s (4 expenses)', [FormatFloat('#,##0', 13500.0)]));
  WriteLn(Format('     Engineering: $%s (4 expenses)', [FormatFloat('#,##0', 17700.0)]));
  WriteLn(Format('     Operations:  $%s (4 expenses)', [FormatFloat('#,##0', 8800.0)]));
  WriteLn(Format('     Travel:      $%s (2 expenses)', [FormatFloat('#,##0', 2000.0)]));
  WriteLn(Format('     HR:          $%s (3 expenses)', [FormatFloat('#,##0', 8000.0)]));

  // February expenses (partial)
  AddExpense(CatMarketing, 4500, 'Valentine campaign', '2024-02-01', '2024-02', 'J. Smith');
  AddExpense(CatMarketing, 6000, 'Influencer partnership', '2024-02-10', '2024-02', 'A. Jones');
  AddExpense(CatMarketing, 3000, 'Print ads', '2024-02-15', '2024-02', 'J. Smith');
  AddExpense(CatMarketing, 4000, 'Conference sponsorship', '2024-02-20', '2024-02', 'J. Smith');

  AddExpense(CatEngineering, 8000, 'Cloud infrastructure (AWS)', '2024-02-01', '2024-02', 'M. Lee');
  AddExpense(CatEngineering, 5500, 'New dev laptops (2x)', '2024-02-08', '2024-02', 'M. Lee');
  AddExpense(CatEngineering, 3000, 'Conference tickets', '2024-02-15', '2024-02', 'R. Chen');

  AddExpense(CatOperations, 2500, 'Cleaning service', '2024-02-10', '2024-02', 'P. Wilson');
  AddExpense(CatOperations, 1200, 'Office supplies', '2024-02-20', '2024-02', 'P. Wilson');

  AddExpense(CatTravel, 3500, 'SF conference - flights + hotel', '2024-02-12', '2024-02', 'R. Chen');
  AddExpense(CatTravel, 1800, 'Chicago client visit', '2024-02-22', '2024-02', 'S. Park');

  AddExpense(CatHR, 3000, 'Recruiter fees', '2024-02-05', '2024-02', 'L. Brown');
  AddExpense(CatHR, 5500, 'Annual benefits enrollment', '2024-02-15', '2024-02', 'L. Brown');

  WriteLn;
  WriteLn('   February 2024 expenses also recorded');
  WriteLn;
end;

{ Queries each category's budget vs. actual spending for January 2024 and prints
  a table with allocated, spent, remaining, percentage used, and status flags. }
procedure DemoBudgetStatus;
var
  DS: TDataSet;
  Allocated, Spent, Remaining, Pct: Double;
begin
  WriteLn('=== 4. Budget Status (January 2024) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT bc.name, ' +
    'ba.allocated_amount + ba.rollover_amount as total_budget, ' +
    'COALESCE((SELECT SUM(amount) FROM expenses WHERE category_id = bc.id AND period = ''2024-01''), 0) as spent ' +
    'FROM budget_categories bc ' +
    'JOIN budget_allocations ba ON bc.id = ba.category_id AND ba.period = ''2024-01'' ' +
    'ORDER BY bc.name',
    []
  );
  try
    WriteLn(Format('   %-14s %10s %10s %10s %6s  %s',
      ['Category', 'Budget', 'Spent', 'Remaining', 'Used', 'Status']));
    WriteLn('   ' + StringOfChar('-', 72));

    while not DS.EOF do
    begin
      Allocated := DS.FieldByName('total_budget').AsFloat;
      Spent := DS.FieldByName('spent').AsFloat;
      Remaining := Allocated - Spent;
      if Allocated > 0 then
        Pct := (Spent / Allocated) * 100
      else
        Pct := 0;

      WriteLn(Format('   %-14s $%9s $%9s $%9s %5.1f%%  %s',
        [DS.FieldByName('name').AsString,
         FormatFloat('#,##0', Allocated),
         FormatFloat('#,##0', Spent),
         FormatFloat('#,##0', Remaining),
         Pct,
         BoolToStr(Spent > Allocated, 'OVER BUDGET', BoolToStr(Pct >= 90, 'CRITICAL', BoolToStr(Pct >= 75, 'WARNING', 'OK')))]));

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Runs budget threshold checks for all active categories in January 2024 and
  prints any triggered alerts (warning at 75%, critical at 90%, overspend at 100%). }
procedure DemoOverspendAlerts;
var
  DS: TDataSet;
  CatId: Integer;
begin
  WriteLn('=== 5. Overspend Alerts ===');
  WriteLn;

  // Check alerts for all categories in January
  DS := Conn.ExecuteQuery('SELECT id FROM budget_categories WHERE is_active = 1', []);
  try
    while not DS.EOF do
    begin
      CatId := DS.FieldByName('id').AsInteger;
      CheckBudgetAlerts(CatId, '2024-01');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Display alerts
  DS := Conn.ExecuteQuery(
    'SELECT ba.alert_type, ba.threshold_pct, ba.current_pct, ba.message ' +
    'FROM budget_alerts ba ' +
    'WHERE ba.period = ''2024-01'' ' +
    'ORDER BY ba.current_pct DESC',
    []
  );
  try
    WriteLn('   January 2024 alerts:');
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s',
        [UpperCase(DS.FieldByName('alert_type').AsString),
         DS.FieldByName('message').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  if Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM budget_alerts WHERE period = ''2024-01''')) = 0 then
    WriteLn('   No alerts triggered for January 2024');

  WriteLn;
end;

{ Demonstrates budget rollover of unused amounts between periods. }
procedure DemoRollover;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Budget Rollover (January -> February) ===');
  WriteLn;

  PerformRollover('2024-01', '2024-02');

  WriteLn('   Unused January budget rolled over to February:');

  DS := Conn.ExecuteQuery(
    'SELECT bc.name, ba.rollover_amount ' +
    'FROM budget_allocations ba ' +
    'JOIN budget_categories bc ON bc.id = ba.category_id ' +
    'WHERE ba.period = ''2024-02'' AND ba.rollover_amount > 0 ' +
    'ORDER BY ba.rollover_amount DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s +$%s rollover',
        [DS.FieldByName('name').AsString,
         FormatFloat('#,##0', DS.FieldByName('rollover_amount').AsFloat)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Show updated February budget
  WriteLn('   February 2024 budget (allocated + rollover):');
  DS := Conn.ExecuteQuery(
    'SELECT bc.name, ba.allocated_amount, ba.rollover_amount, ' +
    'ba.allocated_amount + ba.rollover_amount as total ' +
    'FROM budget_allocations ba ' +
    'JOIN budget_categories bc ON bc.id = ba.category_id ' +
    'WHERE ba.period = ''2024-02'' ' +
    'ORDER BY bc.name',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s $%s + $%s rollover = $%s',
        [DS.FieldByName('name').AsString,
         FormatFloat('#,##0', DS.FieldByName('allocated_amount').AsFloat),
         FormatFloat('#,##0', DS.FieldByName('rollover_amount').AsFloat),
         FormatFloat('#,##0', DS.FieldByName('total').AsFloat)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Formats a percentage value with a sign prefix. }
function FormatPct(Value: Double): string;
begin
  if Value >= 0 then
    Result := Format('+%.1f%%', [Value])
  else
    Result := Format('%.1f%%', [Value]);
end;

{ Compares January and February spending by category, printing each category's
  total for both months along with the dollar change and percentage change. }
procedure DemoPeriodComparison;
var
  DS: TDataSet;
  JanSpent, FebSpent, Change: Double;
begin
  WriteLn('=== 7. Period Comparison (January vs February) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT bc.name, ' +
    'COALESCE((SELECT SUM(amount) FROM expenses WHERE category_id = bc.id AND period = ''2024-01''), 0) as jan_spent, ' +
    'COALESCE((SELECT SUM(amount) FROM expenses WHERE category_id = bc.id AND period = ''2024-02''), 0) as feb_spent ' +
    'FROM budget_categories bc ' +
    'WHERE bc.is_active = 1 ' +
    'ORDER BY bc.name',
    []
  );
  try
    WriteLn(Format('   %-14s %10s %10s %10s %8s',
      ['Category', 'January', 'February', 'Change', '% Change']));
    WriteLn('   ' + StringOfChar('-', 60));

    while not DS.EOF do
    begin
      JanSpent := DS.FieldByName('jan_spent').AsFloat;
      FebSpent := DS.FieldByName('feb_spent').AsFloat;
      Change := FebSpent - JanSpent;

      if JanSpent > 0 then
        WriteLn(Format('   %-14s $%9s $%9s $%9s %8s',
          [DS.FieldByName('name').AsString,
           FormatFloat('#,##0', JanSpent),
           FormatFloat('#,##0', FebSpent),
           FormatFloat('#,##0', Change),
           FormatPct((Change / JanSpent) * 100)]))
      else
        WriteLn(Format('   %-14s $%9s $%9s $%9s      N/A',
          [DS.FieldByName('name').AsString,
           FormatFloat('#,##0', JanSpent),
           FormatFloat('#,##0', FebSpent),
           FormatFloat('#,##0', Change)]));

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Totals
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    'COALESCE((SELECT SUM(amount) FROM expenses WHERE period = ''2024-01''), 0) as jan_total, ' +
    'COALESCE((SELECT SUM(amount) FROM expenses WHERE period = ''2024-02''), 0) as feb_total',
    []
  );
  try
    JanSpent := DS.FieldByName('jan_total').AsFloat;
    FebSpent := DS.FieldByName('feb_total').AsFloat;
    Change := FebSpent - JanSpent;
    WriteLn(Format('   TOTAL:        $%9s $%9s $%9s %8s',
      [FormatFloat('#,##0', JanSpent),
       FormatFloat('#,##0', FebSpent),
       FormatFloat('#,##0', Change),
       FormatPct((Change / JanSpent) * 100)]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Lists all individual January 2024 expenses grouped by category, showing the
  amount, description, date, and approver for each expense item. }
procedure DemoCategoryBreakdown;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Category Expense Breakdown (January 2024) ===');
  WriteLn;

  WriteLn('   Top expenses by category:');
  DS := Conn.ExecuteQuery(
    'SELECT bc.name as category, e.description, e.amount, e.expense_date, e.approved_by ' +
    'FROM expenses e ' +
    'JOIN budget_categories bc ON bc.id = e.category_id ' +
    'WHERE e.period = ''2024-01'' ' +
    'ORDER BY bc.name, e.amount DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-12s $%8s  %s (%s, approved: %s)',
        [DS.FieldByName('category').AsString,
         FormatFloat('#,##0', DS.FieldByName('amount').AsFloat),
         DS.FieldByName('description').AsString,
         DS.FieldByName('expense_date').AsString,
         DS.FieldByName('approved_by').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints aggregate statistics including total/average/min/max expenses, January
  budget utilization, top approvers by total amount, and alert count. }
procedure DemoSummaryStatistics;
var
  DS: TDataSet;
  TotalBudget, TotalSpent: Double;
begin
  WriteLn('=== 9. Summary Statistics ===');
  WriteLn;

  // Overall Q1 stats
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT period) as periods, ' +
    'COUNT(*) as total_expenses, ' +
    'SUM(amount) as total_spent, ' +
    'AVG(amount) as avg_expense, ' +
    'MAX(amount) as max_expense, ' +
    'MIN(amount) as min_expense ' +
    'FROM expenses',
    []
  );
  try
    WriteLn(Format('   Periods tracked: %d', [DS.FieldByName('periods').AsInteger]));
    WriteLn(Format('   Total expenses: %d', [DS.FieldByName('total_expenses').AsInteger]));
    WriteLn(Format('   Total spent: $%s', [FormatFloat('#,##0.00', DS.FieldByName('total_spent').AsFloat)]));
    WriteLn(Format('   Average expense: $%s', [FormatFloat('#,##0.00', DS.FieldByName('avg_expense').AsFloat)]));
    WriteLn(Format('   Largest expense: $%s', [FormatFloat('#,##0.00', DS.FieldByName('max_expense').AsFloat)]));
    WriteLn(Format('   Smallest expense: $%s', [FormatFloat('#,##0.00', DS.FieldByName('min_expense').AsFloat)]));
  finally
    DS.Free;
  end;

  WriteLn;

  // Budget utilization
  TotalBudget := Double(Conn.ExecuteScalar(
    'SELECT SUM(allocated_amount + rollover_amount) FROM budget_allocations WHERE period = ''2024-01'''));
  TotalSpent := Double(Conn.ExecuteScalar(
    'SELECT COALESCE(SUM(amount), 0) FROM expenses WHERE period = ''2024-01'''));

  WriteLn('   January 2024 Budget Utilization:');
  WriteLn(Format('     Total budget: $%s', [FormatFloat('#,##0', TotalBudget)]));
  WriteLn(Format('     Total spent:  $%s', [FormatFloat('#,##0', TotalSpent)]));
  WriteLn(Format('     Remaining:    $%s', [FormatFloat('#,##0', TotalBudget - TotalSpent)]));
  WriteLn(Format('     Utilization:  %.1f%%', [(TotalSpent / TotalBudget) * 100]));

  WriteLn;

  // Top spenders
  WriteLn('   Top approvers by total amount:');
  DS := Conn.ExecuteQuery(
    'SELECT approved_by, COUNT(*) as cnt, SUM(amount) as total ' +
    'FROM expenses GROUP BY approved_by ORDER BY total DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-12s %d expenses, $%s total',
        [DS.FieldByName('approved_by').AsString,
         DS.FieldByName('cnt').AsInteger,
         FormatFloat('#,##0', DS.FieldByName('total').AsFloat)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Alerts summary
  WriteLn(Format('   Alerts generated: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM budget_alerts'))]));

  WriteLn;
end;

begin
  WriteLn('=== Example 116: Budget Tracking ===');
  WriteLn('    Budget allocation, expense tracking, rollover, alerts');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    DemoBudgetCategories;
    DemoBudgetAllocations;
    DemoExpenseTracking;
    DemoBudgetStatus;
    DemoOverspendAlerts;
    DemoRollover;
    DemoPeriodComparison;
    DemoCategoryBreakdown;
    DemoSummaryStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example 116 Complete ===');
end.
