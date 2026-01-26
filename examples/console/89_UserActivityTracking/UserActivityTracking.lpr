{===============================================================================
  NDXSQLite Example 89 - User Activity Tracking
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Session and page view tracking
  - Event analytics and feature usage monitoring
  - DAU/WAU/MAU calculations
  - User engagement scoring
  - User segmentation analysis

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program UserActivityTracking;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DateUtils, Classes, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;

{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('=== User Activity Tracking ===');
  WriteLn;
  WriteLn('1. Creating Schema');

  // Users table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT NOT NULL UNIQUE,' +
    '  email TEXT NOT NULL,' +
    '  plan TEXT DEFAULT ''free'',' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Sessions table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS sessions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  session_token TEXT NOT NULL UNIQUE,' +
    '  started_at TEXT NOT NULL,' +
    '  ended_at TEXT,' +
    '  duration_seconds INTEGER,' +
    '  ip_address TEXT,' +
    '  user_agent TEXT,' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Activity events table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS activity_events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  session_id INTEGER,' +
    '  event_type TEXT NOT NULL,' +
    '  event_name TEXT NOT NULL,' +
    '  event_data TEXT,' +
    '  page_url TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  FOREIGN KEY (user_id) REFERENCES users(id),' +
    '  FOREIGN KEY (session_id) REFERENCES sessions(id)' +
    ')');

  // Page views table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS page_views (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  session_id INTEGER,' +
    '  page_url TEXT NOT NULL,' +
    '  page_title TEXT,' +
    '  referrer TEXT,' +
    '  duration_seconds INTEGER,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Feature usage table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS feature_usage (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  feature_name TEXT NOT NULL,' +
    '  action TEXT NOT NULL,' +
    '  metadata TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Daily activity summary (materialized)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS daily_activity_summary (' +
    '  user_id INTEGER NOT NULL,' +
    '  activity_date TEXT NOT NULL,' +
    '  sessions_count INTEGER DEFAULT 0,' +
    '  page_views_count INTEGER DEFAULT 0,' +
    '  events_count INTEGER DEFAULT 0,' +
    '  total_duration_seconds INTEGER DEFAULT 0,' +
    '  engagement_score REAL DEFAULT 0.0,' +
    '  PRIMARY KEY (user_id, activity_date),' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_sessions_user ON sessions(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_sessions_started ON sessions(started_at)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_user ON activity_events(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_type ON activity_events(event_type, event_name)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_created ON activity_events(created_at)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_page_views_user ON page_views(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_feature_usage_feature ON feature_usage(feature_name, user_id)');

  WriteLn('   Tables created: users, sessions, activity_events, page_views, feature_usage, daily_activity_summary');
  WriteLn('   Indexes created: 7');
end;

{ Generates test data for demonstrations. }
procedure GenerateTestData;
var
  I, J, SessionCount, EventCount, PageCount, FeatureCount: Integer;
  UserNames: array[0..9] of string;
  Plans: array[0..2] of string;
  Pages: array[0..7] of string;
  Features: array[0..5] of string;
  EventTypes: array[0..3] of string;
  UserAgents: array[0..2] of string;
  DaysAgo, DurationSec: Integer;
  SessionId: Int64;
begin
  WriteLn;
  WriteLn('2. Generating Test Data');

  UserNames[0] := 'alice'; UserNames[1] := 'bob'; UserNames[2] := 'charlie';
  UserNames[3] := 'diana'; UserNames[4] := 'evan'; UserNames[5] := 'fiona';
  UserNames[6] := 'george'; UserNames[7] := 'hannah'; UserNames[8] := 'ivan';
  UserNames[9] := 'julia';

  Plans[0] := 'free'; Plans[1] := 'pro'; Plans[2] := 'enterprise';

  Pages[0] := '/dashboard'; Pages[1] := '/settings'; Pages[2] := '/projects';
  Pages[3] := '/reports'; Pages[4] := '/team'; Pages[5] := '/billing';
  Pages[6] := '/help'; Pages[7] := '/profile';

  Features[0] := 'export'; Features[1] := 'search'; Features[2] := 'filter';
  Features[3] := 'share'; Features[4] := 'import'; Features[5] := 'notify';

  EventTypes[0] := 'click'; EventTypes[1] := 'navigation';
  EventTypes[2] := 'form_submit'; EventTypes[3] := 'api_call';

  UserAgents[0] := 'Chrome/120 Windows';
  UserAgents[1] := 'Firefox/119 macOS';
  UserAgents[2] := 'Safari/17 iOS';

  // Create users
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 0 to 9 do
  begin
    DaysAgo := 30 + Random(60); // Created 30-90 days ago
    Conn.ExecuteNonQuery(
      'INSERT INTO users (username, email, plan, created_at) VALUES (?, ?, ?, datetime(''now'', ? || '' days''))',
      [UserNames[I], UserNames[I] + '@example.com', Plans[I mod 3], IntToStr(-DaysAgo)]);
  end;

  // Create sessions and events for each user
  SessionCount := 0;
  EventCount := 0;
  PageCount := 0;
  FeatureCount := 0;

  for I := 1 to 10 do // For each user
  begin
    // Create 5-15 sessions per user over last 30 days
    for J := 0 to 4 + Random(11) do
    begin
      DaysAgo := Random(30);
      DurationSec := 120 + Random(3600); // 2min to 1h

      Conn.ExecuteNonQuery(
        'INSERT INTO sessions (user_id, session_token, started_at, ended_at, duration_seconds, ip_address, user_agent) ' +
        'VALUES (?, ?, datetime(''now'', ? || '' days'', ''-'' || ? || '' hours''), ' +
        'datetime(''now'', ? || '' days'', ''-'' || ? || '' hours'', ''+'' || ? || '' seconds''), ?, ?, ?)',
        [IntToStr(I), 'sess_' + IntToStr(I) + '_' + IntToStr(J),
         IntToStr(-DaysAgo), IntToStr(Random(12)),
         IntToStr(-DaysAgo), IntToStr(Random(12)), IntToStr(DurationSec),
         IntToStr(DurationSec),
         '192.168.1.' + IntToStr(100 + I),
         UserAgents[Random(3)]]);
      Inc(SessionCount);

      // Get session ID
      SessionId := Conn.ExecuteScalar(
        'SELECT MAX(id) FROM sessions WHERE user_id = ?', [IntToStr(I)]);

      // 3-10 page views per session
      PageCount := PageCount + 3 + Random(8);
      Conn.ExecuteNonQuery(
        'INSERT INTO page_views (user_id, session_id, page_url, page_title, duration_seconds, created_at) ' +
        'SELECT ?, ?, ?, ''Page Title'', ?, datetime(''now'', ? || '' days'') ' +
        'FROM (SELECT 1)',
        [IntToStr(I), IntToStr(SessionId), Pages[Random(8)],
         IntToStr(10 + Random(120)), IntToStr(-DaysAgo)]);

      // 2-5 events per session
      Conn.ExecuteNonQuery(
        'INSERT INTO activity_events (user_id, session_id, event_type, event_name, page_url, created_at) ' +
        'VALUES (?, ?, ?, ?, ?, datetime(''now'', ? || '' days''))',
        [IntToStr(I), IntToStr(SessionId), EventTypes[Random(4)],
         'action_' + IntToStr(Random(20)), Pages[Random(8)], IntToStr(-DaysAgo)]);
      Inc(EventCount);
    end;

    // Feature usage (3-8 per user)
    for J := 0 to 2 + Random(6) do
    begin
      DaysAgo := Random(30);
      Conn.ExecuteNonQuery(
        'INSERT INTO feature_usage (user_id, feature_name, action, created_at) ' +
        'VALUES (?, ?, ?, datetime(''now'', ? || '' days''))',
        [IntToStr(I), Features[Random(6)], 'used', IntToStr(-DaysAgo)]);
      Inc(FeatureCount);
    end;
  end;

  Conn.ExecuteNonQuery('COMMIT');

  WriteLn(Format('   Users created: %d', [10]));
  WriteLn(Format('   Sessions: %d', [SessionCount]));
  WriteLn(Format('   Activity events: %d', [EventCount]));
  WriteLn(Format('   Feature usages: %d', [FeatureCount]));
end;

{ Queries active users in the last 7 days, average session duration, and lists the top 5 users by session count with total duration. }
procedure TrackSessionActivity;
var
  DS: TDataSet;
  ActiveSessions, AvgDuration: Integer;
begin
  WriteLn;
  WriteLn('3. Session Activity Analysis');

  // Active sessions in last 7 days
  ActiveSessions := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
    'WHERE started_at >= datetime(''now'', ''-7 days'')');
  WriteLn(Format('   Active users (last 7 days): %d', [ActiveSessions]));

  // Average session duration
  AvgDuration := Conn.ExecuteScalar(
    'SELECT COALESCE(AVG(duration_seconds), 0) FROM sessions ' +
    'WHERE duration_seconds IS NOT NULL');
  WriteLn(Format('   Avg session duration: %d seconds', [AvgDuration]));

  // Sessions per user
  WriteLn('   Sessions per user (top 5):');
  DS := Conn.ExecuteQuery(
    'SELECT u.username, COUNT(s.id) as session_count, ' +
    'COALESCE(SUM(s.duration_seconds), 0) as total_seconds ' +
    'FROM users u LEFT JOIN sessions s ON u.id = s.user_id ' +
    'GROUP BY u.id ORDER BY session_count DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d sessions, %d sec total',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('session_count').AsInteger,
         DS.FieldByName('total_seconds').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Counts total page views and lists the most visited pages with view count, unique users, and average duration. }
procedure AnalyzePageViews;
var
  DS: TDataSet;
  TotalViews: Integer;
begin
  WriteLn;
  WriteLn('4. Page View Analysis');

  TotalViews := Conn.ExecuteScalar('SELECT COUNT(*) FROM page_views');
  WriteLn(Format('   Total page views: %d', [TotalViews]));

  // Most visited pages
  WriteLn('   Most visited pages:');
  DS := Conn.ExecuteQuery(
    'SELECT page_url, COUNT(*) as view_count, ' +
    'COUNT(DISTINCT user_id) as unique_users, ' +
    'CAST(COALESCE(AVG(duration_seconds), 0) AS INTEGER) as avg_duration ' +
    'FROM page_views GROUP BY page_url ' +
    'ORDER BY view_count DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d views, %d users, avg %ds',
        [DS.FieldByName('page_url').AsString,
         DS.FieldByName('view_count').AsInteger,
         DS.FieldByName('unique_users').AsInteger,
         DS.FieldByName('avg_duration').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Counts total activity events, breaks them down by event type with unique user counts, and shows daily event trends for the last 7 days. }
procedure AnalyzeEvents;
var
  DS: TDataSet;
  TotalEvents: Integer;
begin
  WriteLn;
  WriteLn('5. Event Analysis');

  TotalEvents := Conn.ExecuteScalar('SELECT COUNT(*) FROM activity_events');
  WriteLn(Format('   Total events: %d', [TotalEvents]));

  // Events by type
  WriteLn('   Events by type:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, COUNT(*) as event_count, ' +
    'COUNT(DISTINCT user_id) as unique_users ' +
    'FROM activity_events GROUP BY event_type ' +
    'ORDER BY event_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d events (%d users)',
        [DS.FieldByName('event_type').AsString,
         DS.FieldByName('event_count').AsInteger,
         DS.FieldByName('unique_users').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Events per day trend
  WriteLn('   Daily event trend (last 7 days):');
  DS := Conn.ExecuteQuery(
    'SELECT date(created_at) as event_date, COUNT(*) as daily_count ' +
    'FROM activity_events ' +
    'WHERE created_at >= datetime(''now'', ''-7 days'') ' +
    'GROUP BY date(created_at) ORDER BY event_date');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d events',
        [DS.FieldByName('event_date').AsString,
         DS.FieldByName('daily_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Ranks features by usage count and adoption percentage, then breaks down feature usage by subscription plan. }
procedure AnalyzeFeatureUsage;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('6. Feature Usage Analysis');

  // Feature popularity
  WriteLn('   Feature popularity:');
  DS := Conn.ExecuteQuery(
    'SELECT feature_name, COUNT(*) as usage_count, ' +
    'COUNT(DISTINCT user_id) as unique_users, ' +
    'ROUND(COUNT(DISTINCT user_id) * 100.0 / (SELECT COUNT(*) FROM users), 1) as adoption_pct ' +
    'FROM feature_usage GROUP BY feature_name ' +
    'ORDER BY usage_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d uses, %d users (%.1f%% adoption)',
        [DS.FieldByName('feature_name').AsString,
         DS.FieldByName('usage_count').AsInteger,
         DS.FieldByName('unique_users').AsInteger,
         DS.FieldByName('adoption_pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Feature usage by plan
  WriteLn('   Feature usage by plan:');
  DS := Conn.ExecuteQuery(
    'SELECT u.plan, COUNT(f.id) as total_usage, ' +
    'COUNT(DISTINCT f.feature_name) as features_used ' +
    'FROM users u LEFT JOIN feature_usage f ON u.id = f.user_id ' +
    'GROUP BY u.plan ORDER BY total_usage DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d uses, %d features',
        [DS.FieldByName('plan').AsString,
         DS.FieldByName('total_usage').AsInteger,
         DS.FieldByName('features_used').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Computes daily, weekly, and monthly active users from session data and calculates the stickiness ratio (DAU/MAU). }
procedure CalculateDAUWAUMAU;
var
  DAU, WAU, MAU: Integer;
  Stickiness: Double;
begin
  WriteLn;
  WriteLn('7. DAU / WAU / MAU Metrics');

  // Daily Active Users (today)
  DAU := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
    'WHERE date(started_at) = date(''now'')');

  // Weekly Active Users (last 7 days)
  WAU := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
    'WHERE started_at >= datetime(''now'', ''-7 days'')');

  // Monthly Active Users (last 30 days)
  MAU := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
    'WHERE started_at >= datetime(''now'', ''-30 days'')');

  WriteLn(Format('   DAU (today): %d', [DAU]));
  WriteLn(Format('   WAU (7 days): %d', [WAU]));
  WriteLn(Format('   MAU (30 days): %d', [MAU]));

  // Stickiness ratio
  if MAU > 0 then
    Stickiness := (DAU * 100.0) / MAU
  else
    Stickiness := 0;
  WriteLn(Format('   Stickiness (DAU/MAU): %.1f%%', [Stickiness]));
end;

{ Builds the daily_activity_summary table with weighted engagement scores and displays average scores per user and by plan. }
procedure CalculateEngagementScore;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('8. User Engagement Scoring');

  // Build daily activity summary
  Conn.ExecuteNonQuery('DELETE FROM daily_activity_summary');
  Conn.ExecuteNonQuery(
    'INSERT INTO daily_activity_summary (user_id, activity_date, sessions_count, page_views_count, events_count, total_duration_seconds, engagement_score) ' +
    'SELECT s.user_id, date(s.started_at) as activity_date, ' +
    '  COUNT(DISTINCT s.id) as sessions_count, ' +
    '  (SELECT COUNT(*) FROM page_views pv WHERE pv.user_id = s.user_id AND date(pv.created_at) = date(s.started_at)) as page_views_count, ' +
    '  (SELECT COUNT(*) FROM activity_events ae WHERE ae.user_id = s.user_id AND date(ae.created_at) = date(s.started_at)) as events_count, ' +
    '  COALESCE(SUM(s.duration_seconds), 0) as total_duration, ' +
    '  ROUND(' +
    '    COUNT(DISTINCT s.id) * 10.0 + ' +  // 10 pts per session
    '    (SELECT COUNT(*) FROM page_views pv WHERE pv.user_id = s.user_id AND date(pv.created_at) = date(s.started_at)) * 2.0 + ' + // 2 pts per page view
    '    (SELECT COUNT(*) FROM activity_events ae WHERE ae.user_id = s.user_id AND date(ae.created_at) = date(s.started_at)) * 5.0 + ' + // 5 pts per event
    '    MIN(COALESCE(SUM(s.duration_seconds), 0) / 60.0, 100.0)' + // 1 pt per minute, max 100
    '  , 1) as engagement_score ' +
    'FROM sessions s ' +
    'GROUP BY s.user_id, date(s.started_at)');

  // Show engagement scores per user
  WriteLn('   User engagement (avg daily score):');
  DS := Conn.ExecuteQuery(
    'SELECT u.username, u.plan, ' +
    'ROUND(AVG(d.engagement_score), 1) as avg_score, ' +
    'COUNT(d.activity_date) as active_days, ' +
    'SUM(d.sessions_count) as total_sessions ' +
    'FROM users u LEFT JOIN daily_activity_summary d ON u.id = d.user_id ' +
    'GROUP BY u.id ORDER BY avg_score DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%s): score=%.1f, %d active days, %d sessions',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('plan').AsString,
         DS.FieldByName('avg_score').AsFloat,
         DS.FieldByName('active_days').AsInteger,
         DS.FieldByName('total_sessions').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Engagement by plan
  WriteLn('   Engagement by plan:');
  DS := Conn.ExecuteQuery(
    'SELECT u.plan, ' +
    'ROUND(AVG(d.engagement_score), 1) as avg_score, ' +
    'COUNT(DISTINCT u.id) as user_count ' +
    'FROM users u LEFT JOIN daily_activity_summary d ON u.id = d.user_id ' +
    'GROUP BY u.plan ORDER BY avg_score DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: avg score=%.1f (%d users)',
        [DS.FieldByName('plan').AsString,
         DS.FieldByName('avg_score').AsFloat,
         DS.FieldByName('user_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Segments users into Power User, Active, Casual, and Dormant categories by session count, and shows platform distribution by user agent. }
procedure AnalyzeUserSegments;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('9. User Segmentation');

  // Segment by activity level
  WriteLn('   Activity segments:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE ' +
    '    WHEN total_sessions >= 10 THEN ''Power User'' ' +
    '    WHEN total_sessions >= 5 THEN ''Active'' ' +
    '    WHEN total_sessions >= 2 THEN ''Casual'' ' +
    '    ELSE ''Dormant'' ' +
    '  END as segment, ' +
    '  COUNT(*) as user_count, ' +
    '  ROUND(AVG(total_sessions), 1) as avg_sessions, ' +
    '  CAST(ROUND(AVG(total_duration), 0) AS INTEGER) as avg_duration_sec ' +
    'FROM (' +
    '  SELECT user_id, COUNT(*) as total_sessions, ' +
    '  COALESCE(SUM(duration_seconds), 0) as total_duration ' +
    '  FROM sessions ' +
    '  WHERE started_at >= datetime(''now'', ''-30 days'') ' +
    '  GROUP BY user_id' +
    ') GROUP BY segment ORDER BY avg_sessions DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d users, avg %.1f sessions, avg %d sec',
        [DS.FieldByName('segment').AsString,
         DS.FieldByName('user_count').AsInteger,
         DS.FieldByName('avg_sessions').AsFloat,
         DS.FieldByName('avg_duration_sec').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Segment by user agent
  WriteLn('   Platform distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT user_agent as platform, COUNT(DISTINCT user_id) as users, ' +
    'COUNT(*) as sessions ' +
    'FROM sessions GROUP BY user_agent ORDER BY sessions DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d users, %d sessions',
        [DS.FieldByName('platform').AsString,
         DS.FieldByName('users').AsInteger,
         DS.FieldByName('sessions').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Calculates Day 1, Day 7, and Day 30 retention percentages and displays a weekly activity heatmap with sessions per day of week. }
procedure AnalyzeRetention;
var
  DS: TDataSet;
  Day1, Day7, Day30: Integer;
  TotalUsers: Integer;
begin
  WriteLn;
  WriteLn('10. Retention Analysis');

  TotalUsers := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');

  // Day 1 retention: users who came back the day after their first session
  Day1 := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT s1.user_id) FROM sessions s1 ' +
    'WHERE EXISTS (' +
    '  SELECT 1 FROM sessions s2 WHERE s2.user_id = s1.user_id ' +
    '  AND date(s2.started_at) = date(s1.started_at, ''+1 day'')' +
    ')');

  // Day 7 retention
  Day7 := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
    'WHERE started_at >= datetime(''now'', ''-7 days'') ' +
    'AND user_id IN (SELECT DISTINCT user_id FROM sessions WHERE started_at < datetime(''now'', ''-7 days''))');

  // Day 30 retention
  Day30 := Conn.ExecuteScalar(
    'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
    'WHERE started_at >= datetime(''now'', ''-7 days'') ' +
    'AND user_id IN (SELECT DISTINCT user_id FROM sessions WHERE started_at < datetime(''now'', ''-21 days''))');

  WriteLn(Format('   Total users: %d', [TotalUsers]));
  WriteLn(Format('   Day 1 retention: %d users (%.1f%%)', [Day1, Day1 * 100.0 / TotalUsers]));
  WriteLn(Format('   Day 7 retention: %d users (%.1f%%)', [Day7, Day7 * 100.0 / TotalUsers]));
  WriteLn(Format('   Day 30 retention: %d users (%.1f%%)', [Day30, Day30 * 100.0 / TotalUsers]));

  // Weekly activity heatmap
  WriteLn('   Weekly activity (sessions per day):');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE CAST(strftime(''%w'', started_at) AS INTEGER) ' +
    '    WHEN 0 THEN ''Sun'' WHEN 1 THEN ''Mon'' WHEN 2 THEN ''Tue'' ' +
    '    WHEN 3 THEN ''Wed'' WHEN 4 THEN ''Thu'' WHEN 5 THEN ''Fri'' ' +
    '    WHEN 6 THEN ''Sat'' END as day_name, ' +
    '  COUNT(*) as session_count, ' +
    '  COUNT(DISTINCT user_id) as unique_users ' +
    'FROM sessions GROUP BY strftime(''%w'', started_at) ' +
    'ORDER BY CAST(strftime(''%w'', started_at) AS INTEGER)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d sessions (%d users)',
        [DS.FieldByName('day_name').AsString,
         DS.FieldByName('session_count').AsInteger,
         DS.FieldByName('unique_users').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Displays activity timeline information. }
procedure ShowActivityTimeline;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('11. User Activity Timeline (alice)');

  DS := Conn.ExecuteQuery(
    'SELECT ''session'' as type, started_at as timestamp, ' +
    '  ''Session ('' || duration_seconds || ''s)'' as description ' +
    'FROM sessions WHERE user_id = 1 ' +
    'UNION ALL ' +
    'SELECT ''page_view'', created_at, ''Viewed '' || page_url ' +
    'FROM page_views WHERE user_id = 1 ' +
    'UNION ALL ' +
    'SELECT ''event'', created_at, event_type || '': '' || event_name ' +
    'FROM activity_events WHERE user_id = 1 ' +
    'UNION ALL ' +
    'SELECT ''feature'', created_at, ''Used '' || feature_name ' +
    'FROM feature_usage WHERE user_id = 1 ' +
    'ORDER BY timestamp DESC LIMIT 10');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s - %s',
        [DS.FieldByName('type').AsString,
         DS.FieldByName('timestamp').AsString,
         DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Displays overall stats information. }
procedure ShowOverallStats;
var
  TotalUsers, TotalSessions, TotalEvents, TotalPages: Integer;
  AvgSessionsPerUser: Double;
begin
  WriteLn;
  WriteLn('12. Overall Statistics');

  TotalUsers := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');
  TotalSessions := Conn.ExecuteScalar('SELECT COUNT(*) FROM sessions');
  TotalEvents := Conn.ExecuteScalar('SELECT COUNT(*) FROM activity_events');
  TotalPages := Conn.ExecuteScalar('SELECT COUNT(*) FROM page_views');

  if TotalUsers > 0 then
    AvgSessionsPerUser := TotalSessions / TotalUsers
  else
    AvgSessionsPerUser := 0;

  WriteLn(Format('   Total users: %d', [TotalUsers]));
  WriteLn(Format('   Total sessions: %d', [TotalSessions]));
  WriteLn(Format('   Total events: %d', [TotalEvents]));
  WriteLn(Format('   Total page views: %d', [TotalPages]));
  WriteLn(Format('   Avg sessions/user: %.1f', [AvgSessionsPerUser]));
  WriteLn(Format('   Engagement summaries: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM daily_activity_summary'))]));
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
    TrackSessionActivity;
    AnalyzePageViews;
    AnalyzeEvents;
    AnalyzeFeatureUsage;
    CalculateDAUWAUMAU;
    CalculateEngagementScore;
    AnalyzeUserSegments;
    AnalyzeRetention;
    ShowActivityTimeline;
    ShowOverallStats;

    WriteLn;
    WriteLn('=== Example Complete ===');
    Conn.Close;
  finally
    Conn.Free;
  end;
end.
