{===============================================================================
  NDXSQLite Example 91 - Cohort Analysis
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - User cohort creation by signup date
  - Weekly and monthly retention analysis
  - Revenue cohort tracking
  - Behavioral cohorts and segmentation
  - Analysis by source, plan, and country

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CohortAnalysis;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DateUtils, Classes, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('=== Cohort Analysis ===');
  WriteLn;
  WriteLn('1. Creating Schema');

  // Users with signup date
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT NOT NULL,' +
    '  signup_date TEXT NOT NULL,' +
    '  signup_week TEXT NOT NULL,' +
    '  plan TEXT DEFAULT ''free'',' +
    '  acquisition_source TEXT,' +
    '  country TEXT' +
    ')');

  // User activity (daily login events)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS user_activity (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  activity_date TEXT NOT NULL,' +
    '  activity_type TEXT DEFAULT ''login'',' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Revenue events
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS revenue_events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  event_type TEXT NOT NULL,' +
    '  event_date TEXT NOT NULL,' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Feature adoption tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS feature_adoption (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  feature_name TEXT NOT NULL,' +
    '  first_used_date TEXT NOT NULL,' +
    '  UNIQUE(user_id, feature_name),' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_users_signup ON users(signup_date)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_users_week ON users(signup_week)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_activity_user ON user_activity(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_activity_date ON user_activity(activity_date)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_revenue_user ON revenue_events(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_revenue_date ON revenue_events(event_date)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_feature_user ON feature_adoption(user_id)');

  WriteLn('   Tables: users, user_activity, revenue_events, feature_adoption');
  WriteLn('   Indexes: 7');
end;

{ Populates users, activity, revenue, and feature adoption tables with randomized data across 8 weekly cohorts. }
procedure GenerateTestData;
var
  I, J, K, DaysActive: Integer;
  Sources: array[0..3] of string;
  Countries: array[0..4] of string;
  Plans: array[0..2] of string;
  Features: array[0..4] of string;
  WeekNum, SignupDaysAgo: Integer;
  RetentionProb: Double;
  UserCount, ActivityCount, RevenueCount, FeatureCount: Integer;
begin
  WriteLn;
  WriteLn('2. Generating Test Data');

  Sources[0] := 'organic'; Sources[1] := 'paid';
  Sources[2] := 'referral'; Sources[3] := 'social';
  Countries[0] := 'US'; Countries[1] := 'UK';
  Countries[2] := 'DE'; Countries[3] := 'FR'; Countries[4] := 'JP';
  Plans[0] := 'free'; Plans[1] := 'pro'; Plans[2] := 'enterprise';
  Features[0] := 'dashboard'; Features[1] := 'export';
  Features[2] := 'api'; Features[3] := 'team'; Features[4] := 'analytics';

  UserCount := 0;
  ActivityCount := 0;
  RevenueCount := 0;
  FeatureCount := 0;

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Create users across 8 weeks (cohorts)
  for WeekNum := 0 to 7 do
  begin
    // 20-35 users per weekly cohort
    for I := 0 to 19 + Random(16) do
    begin
      SignupDaysAgo := (7 - WeekNum) * 7 + Random(7); // spread within week
      if SignupDaysAgo < 0 then SignupDaysAgo := 0;

      Inc(UserCount);
      Conn.ExecuteNonQuery(
        'INSERT INTO users (username, signup_date, signup_week, plan, acquisition_source, country) ' +
        'VALUES (?, date(''now'', ? || '' days''), ' +
        'strftime(''%Y-W'' || printf(''%02d'', (strftime(''%j'', date(''now'', ? || '' days'')) - 1) / 7 + 1)), ' +
        '?, ?, ?)',
        ['user_' + IntToStr(UserCount),
         IntToStr(-SignupDaysAgo),
         IntToStr(-SignupDaysAgo),
         Plans[Random(3)],
         Sources[Random(4)],
         Countries[Random(5)]]);

      // Generate activity for this user (retention decreases over time)
      // Higher retention for paid users
      RetentionProb := 0.7; // Base day-1 retention
      if Plans[Random(3)] = 'enterprise' then RetentionProb := 0.85;

      for J := 0 to SignupDaysAgo - 1 do
      begin
        // Probability decreases with days since signup
        if Random(100) < Round(RetentionProb * 100 * (1.0 / (1.0 + J * 0.08))) then
        begin
          Conn.ExecuteNonQuery(
            'INSERT INTO user_activity (user_id, activity_date, activity_type) ' +
            'VALUES (?, date(''now'', ? || '' days''), ?)',
            [IntToStr(UserCount), IntToStr(-(SignupDaysAgo - J - 1)),
             'login']);
          Inc(ActivityCount);

          // Revenue events (some users purchase)
          if (Random(100) < 15) and (J > 2) then
          begin
            Conn.ExecuteNonQuery(
              'INSERT INTO revenue_events (user_id, amount, event_type, event_date) ' +
              'VALUES (?, ?, ?, date(''now'', ? || '' days''))',
              [IntToStr(UserCount),
               FloatToStr(5.0 + Random(50)),
               'purchase',
               IntToStr(-(SignupDaysAgo - J - 1))]);
            Inc(RevenueCount);
          end;
        end;
      end;

      // Feature adoption (users discover features over time)
      DaysActive := SignupDaysAgo;
      for K := 0 to 4 do
      begin
        // Features are adopted progressively
        if (DaysActive > K * 3) and (Random(100) < 60 - K * 10) then
        begin
          Conn.ExecuteNonQuery(
            'INSERT OR IGNORE INTO feature_adoption (user_id, feature_name, first_used_date) ' +
            'VALUES (?, ?, date(''now'', ? || '' days''))',
            [IntToStr(UserCount), Features[K],
             IntToStr(-(SignupDaysAgo - K * 3 - Random(5)))]);
          Inc(FeatureCount);
        end;
      end;
    end;
  end;

  Conn.ExecuteNonQuery('COMMIT');

  WriteLn(Format('   Users created: %d (8 weekly cohorts)', [UserCount]));
  WriteLn(Format('   Activity events: %d', [ActivityCount]));
  WriteLn(Format('   Revenue events: %d', [RevenueCount]));
  WriteLn(Format('   Feature adoptions: %d', [FeatureCount]));
end;

{ Queries and displays the number of users, first signup, and last signup date for each weekly cohort. }
procedure AnalyzeCohortSizes;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('3. Cohort Sizes (by signup week)');

  DS := Conn.ExecuteQuery(
    'SELECT signup_week, COUNT(*) as cohort_size, ' +
    '  MIN(signup_date) as first_signup, MAX(signup_date) as last_signup ' +
    'FROM users GROUP BY signup_week ' +
    'ORDER BY signup_week');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d users (%s to %s)',
        [DS.FieldByName('signup_week').AsString,
         DS.FieldByName('cohort_size').AsInteger,
         DS.FieldByName('first_signup').AsString,
         DS.FieldByName('last_signup').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Computes and displays Day 1, Day 7, Day 14, and Day 30 retention rates for each weekly signup cohort. }
procedure AnalyzeRetention;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('4. Retention Analysis (by cohort week)');

  // Day 1, Day 7, Day 14, Day 30 retention per cohort
  WriteLn('   Retention rates:');
  WriteLn(Format('   %-12s %6s %6s %6s %7s %7s', ['Cohort', 'Size', 'D1', 'D7', 'D14', 'D30']));

  DS := Conn.ExecuteQuery(
    'SELECT u.signup_week, ' +
    '  COUNT(DISTINCT u.id) as cohort_size, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 1 AND 1 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d1_retention, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 6 AND 8 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d7_retention, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 13 AND 15 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d14_retention, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 28 AND 32 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d30_retention ' +
    'FROM users u ' +
    'LEFT JOIN user_activity ua ON ua.user_id = u.id ' +
    'GROUP BY u.signup_week ' +
    'ORDER BY u.signup_week');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %6d %5.1f%% %5.1f%% %5.1f%% %5.1f%%',
        [DS.FieldByName('signup_week').AsString,
         DS.FieldByName('cohort_size').AsInteger,
         DS.FieldByName('d1_retention').AsFloat,
         DS.FieldByName('d7_retention').AsFloat,
         DS.FieldByName('d14_retention').AsFloat,
         DS.FieldByName('d30_retention').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Builds and displays a retention matrix showing the percentage of users active in weeks 0 through 4 for each cohort. }
procedure AnalyzeWeeklyRetentionTable;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('5. Weekly Retention Table');

  // Show retention as weeks since signup
  WriteLn(Format('   %-12s %5s %5s %5s %5s %5s', ['Cohort', 'W0', 'W1', 'W2', 'W3', 'W4']));

  DS := Conn.ExecuteQuery(
    'SELECT u.signup_week, ' +
    '  COUNT(DISTINCT u.id) as cohort_size, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN CAST((julianday(ua.activity_date) - julianday(u.signup_date)) / 7 AS INTEGER) = 0 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 0) as w0, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN CAST((julianday(ua.activity_date) - julianday(u.signup_date)) / 7 AS INTEGER) = 1 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 0) as w1, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN CAST((julianday(ua.activity_date) - julianday(u.signup_date)) / 7 AS INTEGER) = 2 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 0) as w2, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN CAST((julianday(ua.activity_date) - julianday(u.signup_date)) / 7 AS INTEGER) = 3 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 0) as w3, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN CAST((julianday(ua.activity_date) - julianday(u.signup_date)) / 7 AS INTEGER) = 4 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 0) as w4 ' +
    'FROM users u ' +
    'LEFT JOIN user_activity ua ON ua.user_id = u.id ' +
    'GROUP BY u.signup_week ' +
    'ORDER BY u.signup_week');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %4.0f%% %4.0f%% %4.0f%% %4.0f%% %4.0f%%',
        [DS.FieldByName('signup_week').AsString,
         DS.FieldByName('w0').AsFloat,
         DS.FieldByName('w1').AsFloat,
         DS.FieldByName('w2').AsFloat,
         DS.FieldByName('w3').AsFloat,
         DS.FieldByName('w4').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Computes total revenue, paying user count, ARPU per cohort, and displays cumulative revenue by week since signup. }
procedure AnalyzeRevenueCohorts;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('6. Revenue Cohort Analysis');

  // Revenue per cohort
  WriteLn('   Revenue by signup cohort:');
  DS := Conn.ExecuteQuery(
    'SELECT u.signup_week, ' +
    '  COUNT(DISTINCT u.id) as cohort_size, ' +
    '  COUNT(DISTINCT r.user_id) as paying_users, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0), 2) as total_revenue, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0) / COUNT(DISTINCT u.id), 2) as arpu, ' +
    '  ROUND(COALESCE(AVG(r.amount), 0), 2) as avg_order ' +
    'FROM users u ' +
    'LEFT JOIN revenue_events r ON r.user_id = u.id ' +
    'GROUP BY u.signup_week ' +
    'ORDER BY u.signup_week');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d users, %d paying, $%.2f total, ARPU $%.2f',
        [DS.FieldByName('signup_week').AsString,
         DS.FieldByName('cohort_size').AsInteger,
         DS.FieldByName('paying_users').AsInteger,
         DS.FieldByName('total_revenue').AsFloat,
         DS.FieldByName('arpu').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Cumulative revenue over time per cohort
  WriteLn;
  WriteLn('   Cumulative revenue by week since signup:');
  DS := Conn.ExecuteQuery(
    'SELECT u.signup_week, ' +
    '  ROUND(COALESCE(SUM(CASE WHEN CAST((julianday(r.event_date) - julianday(u.signup_date)) / 7 AS INTEGER) = 0 THEN r.amount END), 0), 0) as w0_rev, ' +
    '  ROUND(COALESCE(SUM(CASE WHEN CAST((julianday(r.event_date) - julianday(u.signup_date)) / 7 AS INTEGER) <= 1 THEN r.amount END), 0), 0) as w1_cum, ' +
    '  ROUND(COALESCE(SUM(CASE WHEN CAST((julianday(r.event_date) - julianday(u.signup_date)) / 7 AS INTEGER) <= 2 THEN r.amount END), 0), 0) as w2_cum, ' +
    '  ROUND(COALESCE(SUM(CASE WHEN CAST((julianday(r.event_date) - julianday(u.signup_date)) / 7 AS INTEGER) <= 3 THEN r.amount END), 0), 0) as w3_cum ' +
    'FROM users u ' +
    'LEFT JOIN revenue_events r ON r.user_id = u.id ' +
    'GROUP BY u.signup_week ' +
    'HAVING COALESCE(SUM(r.amount), 0) > 0 ' +
    'ORDER BY u.signup_week');
  try
    WriteLn(Format('   %-12s %8s %8s %8s %8s', ['Cohort', 'W0', 'W0+W1', 'W0-W2', 'W0-W3']));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s $%6.0f $%6.0f $%6.0f $%6.0f',
        [DS.FieldByName('signup_week').AsString,
         DS.FieldByName('w0_rev').AsFloat,
         DS.FieldByName('w1_cum').AsFloat,
         DS.FieldByName('w2_cum').AsFloat,
         DS.FieldByName('w3_cum').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Displays retention and ARPU segmented by feature adoption, and shows how the number of features adopted correlates with D14 retention. }
procedure AnalyzeBehavioralCohorts;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('7. Behavioral Cohorts (Feature Adoption)');

  // Retention by feature adoption
  WriteLn('   Retention by first feature adopted:');
  DS := Conn.ExecuteQuery(
    'SELECT fa.feature_name, ' +
    '  COUNT(DISTINCT fa.user_id) as adopters, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) >= 14 THEN fa.user_id END) * 100.0 / COUNT(DISTINCT fa.user_id), 1) as d14_retention, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0) / COUNT(DISTINCT fa.user_id), 2) as arpu ' +
    'FROM feature_adoption fa ' +
    'JOIN users u ON u.id = fa.user_id ' +
    'LEFT JOIN user_activity ua ON ua.user_id = fa.user_id ' +
    'LEFT JOIN revenue_events r ON r.user_id = fa.user_id ' +
    'GROUP BY fa.feature_name ' +
    'ORDER BY d14_retention DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d adopters, D14 retention: %.1f%%, ARPU: $%.2f',
        [DS.FieldByName('feature_name').AsString,
         DS.FieldByName('adopters').AsInteger,
         DS.FieldByName('d14_retention').AsFloat,
         DS.FieldByName('arpu').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Users segmented by number of features adopted
  WriteLn('   Retention by features adopted count:');
  DS := Conn.ExecuteQuery(
    'SELECT feature_count, COUNT(*) as user_count, ' +
    '  ROUND(AVG(retained) * 100.0, 1) as retention_pct ' +
    'FROM (' +
    '  SELECT u.id, ' +
    '    COALESCE((SELECT COUNT(*) FROM feature_adoption WHERE user_id = u.id), 0) as feature_count, ' +
    '    CASE WHEN EXISTS(SELECT 1 FROM user_activity WHERE user_id = u.id AND julianday(activity_date) - julianday(u.signup_date) >= 14) THEN 1 ELSE 0 END as retained ' +
    '  FROM users u ' +
    ') GROUP BY feature_count ORDER BY feature_count');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %d features: %d users, %.1f%% retained at D14',
        [DS.FieldByName('feature_count').AsInteger,
         DS.FieldByName('user_count').AsInteger,
         DS.FieldByName('retention_pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Compares user count, D1 and D7 retention rates, and ARPU across acquisition sources (organic, paid, referral, social). }
procedure AnalyzeBySource;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('8. Cohorts by Acquisition Source');

  DS := Conn.ExecuteQuery(
    'SELECT u.acquisition_source, ' +
    '  COUNT(DISTINCT u.id) as users, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 1 AND 1 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d1_ret, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 6 AND 8 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d7_ret, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0) / COUNT(DISTINCT u.id), 2) as arpu ' +
    'FROM users u ' +
    'LEFT JOIN user_activity ua ON ua.user_id = u.id ' +
    'LEFT JOIN revenue_events r ON r.user_id = u.id ' +
    'GROUP BY u.acquisition_source ' +
    'ORDER BY d7_ret DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d users, D1: %.1f%%, D7: %.1f%%, ARPU: $%.2f',
        [DS.FieldByName('acquisition_source').AsString,
         DS.FieldByName('users').AsInteger,
         DS.FieldByName('d1_ret').AsFloat,
         DS.FieldByName('d7_ret').AsFloat,
         DS.FieldByName('arpu').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Compares retention (D1, D7, D14), ARPU, and average feature adoption across user plan tiers (free, pro, enterprise). }
procedure AnalyzeByPlan;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('9. Cohorts by Plan');

  DS := Conn.ExecuteQuery(
    'SELECT u.plan, ' +
    '  COUNT(DISTINCT u.id) as users, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 1 AND 1 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d1_ret, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 6 AND 8 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d7_ret, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 13 AND 15 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d14_ret, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0) / COUNT(DISTINCT u.id), 2) as arpu, ' +
    '  ROUND(COALESCE(AVG((SELECT COUNT(DISTINCT feature_name) FROM feature_adoption WHERE user_id = u.id)), 0), 1) as avg_features ' +
    'FROM users u ' +
    'LEFT JOIN user_activity ua ON ua.user_id = u.id ' +
    'LEFT JOIN revenue_events r ON r.user_id = u.id ' +
    'GROUP BY u.plan ' +
    'ORDER BY d14_ret DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d users, D1: %.1f%%, D7: %.1f%%, D14: %.1f%%, ARPU: $%.2f, avg features: %.1f',
        [DS.FieldByName('plan').AsString,
         DS.FieldByName('users').AsInteger,
         DS.FieldByName('d1_ret').AsFloat,
         DS.FieldByName('d7_ret').AsFloat,
         DS.FieldByName('d14_ret').AsFloat,
         DS.FieldByName('arpu').AsFloat,
         DS.FieldByName('avg_features').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Compares D7 retention rates and ARPU across user countries (US, UK, DE, FR, JP). }
procedure AnalyzeByCountry;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('10. Cohorts by Country');

  DS := Conn.ExecuteQuery(
    'SELECT u.country, ' +
    '  COUNT(DISTINCT u.id) as users, ' +
    '  ROUND(COUNT(DISTINCT CASE WHEN julianday(ua.activity_date) - julianday(u.signup_date) BETWEEN 6 AND 8 THEN u.id END) * 100.0 / COUNT(DISTINCT u.id), 1) as d7_ret, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0) / COUNT(DISTINCT u.id), 2) as arpu ' +
    'FROM users u ' +
    'LEFT JOIN user_activity ua ON ua.user_id = u.id ' +
    'LEFT JOIN revenue_events r ON r.user_id = u.id ' +
    'GROUP BY u.country ' +
    'ORDER BY d7_ret DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d users, D7 retention: %.1f%%, ARPU: $%.2f',
        [DS.FieldByName('country').AsString,
         DS.FieldByName('users').AsInteger,
         DS.FieldByName('d7_ret').AsFloat,
         DS.FieldByName('arpu').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Segments users by days since last activity into risk categories (Active, At Risk, Churning, Churned) and shows churn rates by plan. }
procedure AnalyzeChurnRisk;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('11. Churn Risk Analysis');

  // Users with declining activity
  WriteLn('   Activity trend (last active vs signup):');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE ' +
    '    WHEN days_since_last <= 3 THEN ''Active (0-3d)'' ' +
    '    WHEN days_since_last <= 7 THEN ''At Risk (4-7d)'' ' +
    '    WHEN days_since_last <= 14 THEN ''Churning (8-14d)'' ' +
    '    ELSE ''Churned (14d+)'' ' +
    '  END as status, ' +
    '  COUNT(*) as user_count, ' +
    '  ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM users), 1) as pct ' +
    'FROM (' +
    '  SELECT u.id, ' +
    '    CAST(julianday(''now'') - julianday(COALESCE(MAX(ua.activity_date), u.signup_date)) AS INTEGER) as days_since_last ' +
    '  FROM users u LEFT JOIN user_activity ua ON ua.user_id = u.id ' +
    '  GROUP BY u.id ' +
    ') GROUP BY status ' +
    'ORDER BY MIN(days_since_last)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d users (%.1f%%)',
        [DS.FieldByName('status').AsString,
         DS.FieldByName('user_count').AsInteger,
         DS.FieldByName('pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Churn by plan
  WriteLn('   Churn rate by plan (inactive > 14 days):');
  DS := Conn.ExecuteQuery(
    'SELECT u.plan, COUNT(*) as total, ' +
    '  SUM(CASE WHEN julianday(''now'') - julianday(COALESCE(last_active, u.signup_date)) > 14 THEN 1 ELSE 0 END) as churned, ' +
    '  ROUND(SUM(CASE WHEN julianday(''now'') - julianday(COALESCE(last_active, u.signup_date)) > 14 THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 1) as churn_pct ' +
    'FROM users u ' +
    'LEFT JOIN (SELECT user_id, MAX(activity_date) as last_active FROM user_activity GROUP BY user_id) la ON la.user_id = u.id ' +
    'GROUP BY u.plan ORDER BY churn_pct');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d/%d churned (%.1f%%)',
        [DS.FieldByName('plan').AsString,
         DS.FieldByName('churned').AsInteger,
         DS.FieldByName('total').AsInteger,
         DS.FieldByName('churn_pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Computes total lifetime value, LTV per user, and transactions per user for each weekly signup cohort. }
procedure AnalyzeLifetimeValue;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('12. Lifetime Value by Cohort');

  DS := Conn.ExecuteQuery(
    'SELECT u.signup_week, ' +
    '  COUNT(DISTINCT u.id) as cohort_size, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0), 2) as total_ltv, ' +
    '  ROUND(COALESCE(SUM(r.amount), 0) / COUNT(DISTINCT u.id), 2) as ltv_per_user, ' +
    '  COUNT(r.id) as transactions, ' +
    '  ROUND(CAST(COUNT(r.id) AS REAL) / COUNT(DISTINCT u.id), 2) as txns_per_user ' +
    'FROM users u ' +
    'LEFT JOIN revenue_events r ON r.user_id = u.id ' +
    'GROUP BY u.signup_week ' +
    'ORDER BY u.signup_week');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d users, LTV/user: $%.2f, %.2f txns/user',
        [DS.FieldByName('signup_week').AsString,
         DS.FieldByName('cohort_size').AsInteger,
         DS.FieldByName('ltv_per_user').AsFloat,
         DS.FieldByName('txns_per_user').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Queries and displays aggregate metrics: total users, 7-day active users, paying users, total revenue, overall ARPU, and cohort count. }
procedure ShowOverallMetrics;
var
  TotalUsers, TotalActive, TotalPaying: Integer;
  TotalRevenue, OverallARPU: Double;
begin
  WriteLn;
  WriteLn('13. Overall Cohort Metrics');

  TotalUsers := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');
  TotalActive := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM user_activity ' +
    'WHERE activity_date >= date(''now'', ''-7 days'')');
  TotalPaying := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM revenue_events');

  TotalRevenue := Conn.ExecuteScalar(
    'SELECT COALESCE(SUM(amount), 0) FROM revenue_events');

  if TotalUsers > 0 then
    OverallARPU := TotalRevenue / TotalUsers
  else
    OverallARPU := 0;

  WriteLn(Format('   Total users: %d', [TotalUsers]));
  WriteLn(Format('   Active (7d): %d (%.1f%%)', [TotalActive, TotalActive * 100.0 / TotalUsers]));
  WriteLn(Format('   Paying users: %d (%.1f%%)', [TotalPaying, TotalPaying * 100.0 / TotalUsers]));
  WriteLn(Format('   Total revenue: $%.2f', [TotalRevenue]));
  WriteLn(Format('   Overall ARPU: $%.2f', [OverallARPU]));
  WriteLn(Format('   Total cohorts: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(DISTINCT signup_week) FROM users'))]));
end;

begin
  Randomize;
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');
    Conn.ExecuteNonQuery('PRAGMA foreign_keys=ON');

    CreateSchema;
    GenerateTestData;
    AnalyzeCohortSizes;
    AnalyzeRetention;
    AnalyzeWeeklyRetentionTable;
    AnalyzeRevenueCohorts;
    AnalyzeBehavioralCohorts;
    AnalyzeBySource;
    AnalyzeByPlan;
    AnalyzeByCountry;
    AnalyzeChurnRisk;
    AnalyzeLifetimeValue;
    ShowOverallMetrics;

    WriteLn;
    WriteLn('=== Example Complete ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
