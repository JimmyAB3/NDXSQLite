{===============================================================================
  NDXSQLite Example 90 - Funnel Analysis
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Conversion funnel definition and tracking
  - Signup and purchase funnel analysis
  - Drop-off point identification
  - Source and device segmentation
  - Time-to-convert metrics and A/B test comparison

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program FunnelAnalysis;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DateUtils, Classes, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('=== Conversion Funnel Analysis ===');
  WriteLn;
  WriteLn('1. Creating Schema');

  // Funnels definition
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS funnels (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL UNIQUE,' +
    '  description TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Funnel steps
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS funnel_steps (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  funnel_id INTEGER NOT NULL,' +
    '  step_order INTEGER NOT NULL,' +
    '  step_name TEXT NOT NULL,' +
    '  event_name TEXT NOT NULL,' +
    '  UNIQUE(funnel_id, step_order),' +
    '  FOREIGN KEY (funnel_id) REFERENCES funnels(id)' +
    ')');

  // User events for tracking funnel progression
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS user_events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER NOT NULL,' +
    '  event_name TEXT NOT NULL,' +
    '  event_data TEXT,' +
    '  source TEXT,' +
    '  device TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Funnel conversions tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS funnel_conversions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  funnel_id INTEGER NOT NULL,' +
    '  user_id INTEGER NOT NULL,' +
    '  step_reached INTEGER NOT NULL,' +
    '  completed INTEGER DEFAULT 0,' +
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  time_to_convert_seconds INTEGER,' +
    '  source TEXT,' +
    '  device TEXT,' +
    '  FOREIGN KEY (funnel_id) REFERENCES funnels(id)' +
    ')');

  // A/B test variants
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS ab_variants (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  funnel_id INTEGER NOT NULL,' +
    '  variant_name TEXT NOT NULL,' +
    '  user_id INTEGER NOT NULL,' +
    '  assigned_at TEXT DEFAULT (datetime(''now'')),' +
    '  FOREIGN KEY (funnel_id) REFERENCES funnels(id)' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_user ON user_events(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_name ON user_events(event_name)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_events_created ON user_events(created_at)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_conversions_funnel ON funnel_conversions(funnel_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_conversions_user ON funnel_conversions(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_ab_funnel ON ab_variants(funnel_id, variant_name)');

  WriteLn('   Tables: funnels, funnel_steps, user_events, funnel_conversions, ab_variants');
  WriteLn('   Indexes: 6');
end;

{ Inserts signup, purchase, and onboarding funnel definitions with ordered step names and corresponding event names. }
procedure DefineFunnels;
begin
  WriteLn;
  WriteLn('2. Defining Funnels');

  // Signup funnel
  Conn.ExecuteNonQuery(
    'INSERT INTO funnels (name, description) VALUES (?, ?)',
    ['signup', 'User registration funnel']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (1, 1, ?, ?)', ['Visit Landing', 'page_landing']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (1, 2, ?, ?)', ['Click Signup', 'click_signup']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (1, 3, ?, ?)', ['Fill Form', 'form_start']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (1, 4, ?, ?)', ['Submit Form', 'form_submit']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (1, 5, ?, ?)', ['Verify Email', 'email_verified']);

  // Purchase funnel
  Conn.ExecuteNonQuery(
    'INSERT INTO funnels (name, description) VALUES (?, ?)',
    ['purchase', 'E-commerce purchase funnel']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (2, 1, ?, ?)', ['View Product', 'product_view']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (2, 2, ?, ?)', ['Add to Cart', 'cart_add']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (2, 3, ?, ?)', ['Start Checkout', 'checkout_start']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (2, 4, ?, ?)', ['Enter Payment', 'payment_enter']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (2, 5, ?, ?)', ['Confirm Purchase', 'purchase_confirm']);

  // Onboarding funnel
  Conn.ExecuteNonQuery(
    'INSERT INTO funnels (name, description) VALUES (?, ?)',
    ['onboarding', 'User onboarding funnel']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (3, 1, ?, ?)', ['First Login', 'first_login']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (3, 2, ?, ?)', ['Profile Setup', 'profile_setup']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (3, 3, ?, ?)', ['First Project', 'project_create']);
  Conn.ExecuteNonQuery('INSERT INTO funnel_steps (funnel_id, step_order, step_name, event_name) VALUES (3, 4, ?, ?)', ['Invite Team', 'team_invite']);

  WriteLn('   Signup funnel: 5 steps (Landing -> Email Verified)');
  WriteLn('   Purchase funnel: 5 steps (Product View -> Purchase Confirm)');
  WriteLn('   Onboarding funnel: 4 steps (First Login -> Invite Team)');
end;

{ Simulates signup and purchase events for 200+ users with probabilistic drop-off at each step, recording conversions and A/B assignments. }
procedure GenerateUserEvents;
var
  I, J, StepReached, TimeDelta: Integer;
  Sources: array[0..3] of string;
  Devices: array[0..2] of string;
  UserCount, TotalEvents: Integer;
  DaysAgo: Integer;
  DropProbability: Double;
begin
  WriteLn;
  WriteLn('3. Generating User Events');

  Sources[0] := 'organic'; Sources[1] := 'paid_search';
  Sources[2] := 'social'; Sources[3] := 'referral';
  Devices[0] := 'desktop'; Devices[1] := 'mobile'; Devices[2] := 'tablet';

  UserCount := 200;
  TotalEvents := 0;

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Signup funnel events (200 users start)
  for I := 1 to UserCount do
  begin
    DaysAgo := Random(30);
    // Each step has a drop-off probability
    // Step 1: landing (all users)
    Conn.ExecuteNonQuery(
      'INSERT INTO user_events (user_id, event_name, source, device, created_at) ' +
      'VALUES (?, ?, ?, ?, datetime(''now'', ? || '' days'', ? || '' minutes''))',
      [IntToStr(I), 'page_landing', Sources[Random(4)], Devices[Random(3)],
       IntToStr(-DaysAgo), IntToStr(0)]);
    Inc(TotalEvents);
    StepReached := 1;

    // Step 2: click signup (70% proceed)
    if Random(100) < 70 then
    begin
      Conn.ExecuteNonQuery(
        'INSERT INTO user_events (user_id, event_name, source, device, created_at) ' +
        'VALUES (?, ?, ?, ?, datetime(''now'', ? || '' days'', ? || '' minutes''))',
        [IntToStr(I), 'click_signup', Sources[I mod 4], Devices[I mod 3],
         IntToStr(-DaysAgo), IntToStr(2)]);
      Inc(TotalEvents);
      StepReached := 2;

      // Step 3: fill form (60% proceed)
      if Random(100) < 60 then
      begin
        Conn.ExecuteNonQuery(
          'INSERT INTO user_events (user_id, event_name, source, device, created_at) ' +
          'VALUES (?, ?, ?, ?, datetime(''now'', ? || '' days'', ? || '' minutes''))',
          [IntToStr(I), 'form_start', Sources[I mod 4], Devices[I mod 3],
           IntToStr(-DaysAgo), IntToStr(5)]);
        Inc(TotalEvents);
        StepReached := 3;

        // Step 4: submit form (75% proceed)
        if Random(100) < 75 then
        begin
          Conn.ExecuteNonQuery(
            'INSERT INTO user_events (user_id, event_name, source, device, created_at) ' +
            'VALUES (?, ?, ?, ?, datetime(''now'', ? || '' days'', ? || '' minutes''))',
            [IntToStr(I), 'form_submit', Sources[I mod 4], Devices[I mod 3],
             IntToStr(-DaysAgo), IntToStr(10)]);
          Inc(TotalEvents);
          StepReached := 4;

          // Step 5: verify email (80% proceed)
          if Random(100) < 80 then
          begin
            Conn.ExecuteNonQuery(
              'INSERT INTO user_events (user_id, event_name, source, device, created_at) ' +
              'VALUES (?, ?, ?, ?, datetime(''now'', ? || '' days'', ? || '' minutes''))',
              [IntToStr(I), 'email_verified', Sources[I mod 4], Devices[I mod 3],
               IntToStr(-DaysAgo), IntToStr(60)]);
            Inc(TotalEvents);
            StepReached := 5;
          end;
        end;
      end;
    end;

    // Record conversion progress
    TimeDelta := 60 + Random(600) + (StepReached * 60); // variable time per step
    if StepReached = 5 then
      Conn.ExecuteNonQuery(
        'INSERT INTO funnel_conversions (funnel_id, user_id, step_reached, completed, ' +
        'started_at, completed_at, time_to_convert_seconds, source, device) ' +
        'VALUES (1, ?, ?, 1, datetime(''now'', ? || '' days''), ' +
        'datetime(''now'', ? || '' days'', ''+'' || ? || '' seconds''), ?, ?, ?)',
        [IntToStr(I), IntToStr(StepReached),
         IntToStr(-DaysAgo), IntToStr(-DaysAgo), IntToStr(TimeDelta),
         IntToStr(TimeDelta), Sources[I mod 4], Devices[I mod 3]])
    else
      Conn.ExecuteNonQuery(
        'INSERT INTO funnel_conversions (funnel_id, user_id, step_reached, completed, ' +
        'started_at, time_to_convert_seconds, source, device) ' +
        'VALUES (1, ?, ?, 0, datetime(''now'', ? || '' days''), NULL, ?, ?)',
        [IntToStr(I), IntToStr(StepReached),
         IntToStr(-DaysAgo), Sources[I mod 4], Devices[I mod 3]]);
  end;

  // Purchase funnel events (150 users)
  for I := 1 to 150 do
  begin
    DaysAgo := Random(30);
    // Product view
    Conn.ExecuteNonQuery(
      'INSERT INTO user_events (user_id, event_name, source, device, created_at) ' +
      'VALUES (?, ?, ?, ?, datetime(''now'', ? || '' days''))',
      [IntToStr(I), 'product_view', Sources[Random(4)], Devices[Random(3)], IntToStr(-DaysAgo)]);
    Inc(TotalEvents);
    StepReached := 1;

    // Add to cart (40%)
    if Random(100) < 40 then
    begin
      Conn.ExecuteNonQuery(
        'INSERT INTO user_events (user_id, event_name, created_at) VALUES (?, ?, datetime(''now'', ? || '' days'', ''+5 minutes''))',
        [IntToStr(I), 'cart_add', IntToStr(-DaysAgo)]);
      Inc(TotalEvents);
      StepReached := 2;

      // Start checkout (65%)
      if Random(100) < 65 then
      begin
        Conn.ExecuteNonQuery(
          'INSERT INTO user_events (user_id, event_name, created_at) VALUES (?, ?, datetime(''now'', ? || '' days'', ''+10 minutes''))',
          [IntToStr(I), 'checkout_start', IntToStr(-DaysAgo)]);
        Inc(TotalEvents);
        StepReached := 3;

        // Enter payment (70%)
        if Random(100) < 70 then
        begin
          Conn.ExecuteNonQuery(
            'INSERT INTO user_events (user_id, event_name, created_at) VALUES (?, ?, datetime(''now'', ? || '' days'', ''+15 minutes''))',
            [IntToStr(I), 'payment_enter', IntToStr(-DaysAgo)]);
          Inc(TotalEvents);
          StepReached := 4;

          // Confirm purchase (85%)
          if Random(100) < 85 then
          begin
            Conn.ExecuteNonQuery(
              'INSERT INTO user_events (user_id, event_name, created_at) VALUES (?, ?, datetime(''now'', ? || '' days'', ''+20 minutes''))',
              [IntToStr(I), 'purchase_confirm', IntToStr(-DaysAgo)]);
            Inc(TotalEvents);
            StepReached := 5;
          end;
        end;
      end;
    end;

    Conn.ExecuteNonQuery(
      'INSERT INTO funnel_conversions (funnel_id, user_id, step_reached, completed, source, device) ' +
      'VALUES (2, ?, ?, ?, ?, ?)',
      [IntToStr(I), IntToStr(StepReached), IntToStr(Ord(StepReached = 5)),
       Sources[I mod 4], Devices[I mod 3]]);
  end;

  // A/B test assignment (signup funnel)
  for I := 1 to UserCount do
  begin
    if Random(2) = 0 then
      Conn.ExecuteNonQuery('INSERT INTO ab_variants (funnel_id, variant_name, user_id) VALUES (1, ?, ?)', ['control', IntToStr(I)])
    else
      Conn.ExecuteNonQuery('INSERT INTO ab_variants (funnel_id, variant_name, user_id) VALUES (1, ?, ?)', ['variant_b', IntToStr(I)]);
  end;

  Conn.ExecuteNonQuery('COMMIT');

  WriteLn(Format('   Users: %d', [UserCount]));
  WriteLn(Format('   Total events: %d', [TotalEvents]));
  WriteLn('   Funnels tracked: signup (200 users), purchase (150 users)');
  WriteLn('   A/B variants assigned: 200 users');
end;

{ Counts distinct users at each signup step, calculates step-to-step conversion and drop-off percentages, and shows overall conversion rate. }
procedure AnalyzeSignupFunnel;
var
  DS: TDataSet;
  StepCount, PrevCount: Integer;
  StepName: string;
  DropOff, ConvRate: Double;
begin
  WriteLn;
  WriteLn('4. Signup Funnel Analysis');

  // Count users at each step
  WriteLn('   Step-by-step conversion:');
  PrevCount := 0;
  DS := Conn.ExecuteQuery(
    'SELECT fs.step_order, fs.step_name, fs.event_name, ' +
    '  COUNT(DISTINCT ue.user_id) as user_count ' +
    'FROM funnel_steps fs ' +
    'LEFT JOIN user_events ue ON ue.event_name = fs.event_name ' +
    'WHERE fs.funnel_id = 1 ' +
    'GROUP BY fs.step_order, fs.step_name ' +
    'ORDER BY fs.step_order');
  try
    while not DS.EOF do
    begin
      StepCount := DS.FieldByName('user_count').AsInteger;
      StepName := DS.FieldByName('step_name').AsString;

      if PrevCount > 0 then
      begin
        DropOff := (1.0 - StepCount / PrevCount) * 100;
        ConvRate := (StepCount * 100.0) / PrevCount;
      end
      else
      begin
        DropOff := 0;
        ConvRate := 100;
      end;

      WriteLn(Format('     Step %d: %-15s %3d users | conv: %5.1f%% | drop: %5.1f%%',
        [DS.FieldByName('step_order').AsInteger, StepName, StepCount, ConvRate, DropOff]));
      PrevCount := StepCount;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Overall conversion rate
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(DISTINCT user_id) FROM user_events WHERE event_name = ''page_landing'') as top_count, ' +
    '  (SELECT COUNT(DISTINCT user_id) FROM user_events WHERE event_name = ''email_verified'') as bottom_count');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Overall conversion: %d / %d = %.1f%%',
        [DS.FieldByName('bottom_count').AsInteger,
         DS.FieldByName('top_count').AsInteger,
         DS.FieldByName('bottom_count').AsInteger * 100.0 / DS.FieldByName('top_count').AsInteger]));
    end;
  finally
    DS.Free;
  end;
end;

{ Counts distinct users at each purchase step from product view to confirmation and calculates step-to-step conversion rates. }
procedure AnalyzePurchaseFunnel;
var
  DS: TDataSet;
  StepCount, PrevCount: Integer;
  ConvRate, DropOff: Double;
begin
  WriteLn;
  WriteLn('5. Purchase Funnel Analysis');

  WriteLn('   Step-by-step conversion:');
  PrevCount := 0;
  DS := Conn.ExecuteQuery(
    'SELECT fs.step_order, fs.step_name, ' +
    '  COUNT(DISTINCT ue.user_id) as user_count ' +
    'FROM funnel_steps fs ' +
    'LEFT JOIN user_events ue ON ue.event_name = fs.event_name ' +
    'WHERE fs.funnel_id = 2 ' +
    'GROUP BY fs.step_order ' +
    'ORDER BY fs.step_order');
  try
    while not DS.EOF do
    begin
      StepCount := DS.FieldByName('user_count').AsInteger;

      if PrevCount > 0 then
      begin
        ConvRate := (StepCount * 100.0) / PrevCount;
        DropOff := (1.0 - StepCount / PrevCount) * 100;
      end
      else
      begin
        ConvRate := 100;
        DropOff := 0;
      end;

      WriteLn(Format('     Step %d: %-16s %3d users | conv: %5.1f%% | drop: %5.1f%%',
        [DS.FieldByName('step_order').AsInteger,
         DS.FieldByName('step_name').AsString,
         StepCount, ConvRate, DropOff]));
      PrevCount := StepCount;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Identifies which step users dropped off at for both signup and purchase funnels and displays counts with percentages. }
procedure AnalyzeDropOff;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('6. Drop-Off Analysis');

  // Where do users drop off in signup funnel?
  WriteLn('   Signup funnel - drop-off distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT fc.step_reached, fs.step_name as dropped_at, ' +
    '  COUNT(*) as user_count, ' +
    '  ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM funnel_conversions WHERE funnel_id = 1), 1) as pct ' +
    'FROM funnel_conversions fc ' +
    'JOIN funnel_steps fs ON fs.funnel_id = fc.funnel_id AND fs.step_order = fc.step_reached ' +
    'WHERE fc.funnel_id = 1 AND fc.completed = 0 ' +
    'GROUP BY fc.step_reached ' +
    'ORDER BY fc.step_reached');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     Dropped at "%s": %d users (%.1f%%)',
        [DS.FieldByName('dropped_at').AsString,
         DS.FieldByName('user_count').AsInteger,
         DS.FieldByName('pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Purchase funnel drop-off
  WriteLn('   Purchase funnel - drop-off distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT fc.step_reached, fs.step_name as dropped_at, ' +
    '  COUNT(*) as user_count, ' +
    '  ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM funnel_conversions WHERE funnel_id = 2), 1) as pct ' +
    'FROM funnel_conversions fc ' +
    'JOIN funnel_steps fs ON fs.funnel_id = fc.funnel_id AND fs.step_order = fc.step_reached ' +
    'WHERE fc.funnel_id = 2 AND fc.completed = 0 ' +
    'GROUP BY fc.step_reached ' +
    'ORDER BY fc.step_reached');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     Dropped at "%s": %d users (%.1f%%)',
        [DS.FieldByName('dropped_at').AsString,
         DS.FieldByName('user_count').AsInteger,
         DS.FieldByName('pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Breaks down signup conversion by traffic source (organic, paid, social, referral) with conversion rate and average step reached. }
procedure AnalyzeBySource;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('7. Funnel by Traffic Source');

  WriteLn('   Signup conversion by source:');
  DS := Conn.ExecuteQuery(
    'SELECT source, ' +
    '  COUNT(*) as total_users, ' +
    '  SUM(completed) as converted, ' +
    '  ROUND(SUM(completed) * 100.0 / COUNT(*), 1) as conv_rate, ' +
    '  ROUND(AVG(step_reached), 1) as avg_step ' +
    'FROM funnel_conversions WHERE funnel_id = 1 ' +
    'GROUP BY source ORDER BY conv_rate DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d users, %d converted (%.1f%%), avg step: %.1f',
        [DS.FieldByName('source').AsString,
         DS.FieldByName('total_users').AsInteger,
         DS.FieldByName('converted').AsInteger,
         DS.FieldByName('conv_rate').AsFloat,
         DS.FieldByName('avg_step').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Breaks down signup and purchase conversion rates by device type (desktop, mobile, tablet) with average step reached. }
procedure AnalyzeByDevice;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('8. Funnel by Device');

  WriteLn('   Signup conversion by device:');
  DS := Conn.ExecuteQuery(
    'SELECT device, ' +
    '  COUNT(*) as total_users, ' +
    '  SUM(completed) as converted, ' +
    '  ROUND(SUM(completed) * 100.0 / COUNT(*), 1) as conv_rate, ' +
    '  ROUND(AVG(step_reached), 1) as avg_step ' +
    'FROM funnel_conversions WHERE funnel_id = 1 ' +
    'GROUP BY device ORDER BY conv_rate DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d users, %d converted (%.1f%%), avg step: %.1f',
        [DS.FieldByName('device').AsString,
         DS.FieldByName('total_users').AsInteger,
         DS.FieldByName('converted').AsInteger,
         DS.FieldByName('conv_rate').AsFloat,
         DS.FieldByName('avg_step').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Purchase funnel by device
  WriteLn('   Purchase conversion by device:');
  DS := Conn.ExecuteQuery(
    'SELECT device, ' +
    '  COUNT(*) as total_users, ' +
    '  SUM(completed) as converted, ' +
    '  ROUND(SUM(completed) * 100.0 / COUNT(*), 1) as conv_rate ' +
    'FROM funnel_conversions WHERE funnel_id = 2 ' +
    'GROUP BY device ORDER BY conv_rate DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d users, %d converted (%.1f%%)',
        [DS.FieldByName('device').AsString,
         DS.FieldByName('total_users').AsInteger,
         DS.FieldByName('converted').AsInteger,
         DS.FieldByName('conv_rate').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Computes average, min, and max time-to-convert for the signup funnel and buckets conversions by duration ranges. }
procedure AnalyzeTimeToConvert;
var
  DS: TDataSet;
  AvgTime, MinTime, MaxTime: Integer;
begin
  WriteLn;
  WriteLn('9. Time to Convert');

  // Signup funnel timing
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CAST(COALESCE(AVG(time_to_convert_seconds), 0) AS INTEGER) as avg_time, ' +
    '  COALESCE(MIN(time_to_convert_seconds), 0) as min_time, ' +
    '  COALESCE(MAX(time_to_convert_seconds), 0) as max_time, ' +
    '  COUNT(*) as converted_count ' +
    'FROM funnel_conversions WHERE funnel_id = 1 AND completed = 1');
  try
    if not DS.EOF then
    begin
      AvgTime := DS.FieldByName('avg_time').AsInteger;
      MinTime := DS.FieldByName('min_time').AsInteger;
      MaxTime := DS.FieldByName('max_time').AsInteger;
      WriteLn(Format('   Signup funnel (%d conversions):', [DS.FieldByName('converted_count').AsInteger]));
      WriteLn(Format('     Avg time: %d seconds (%.1f minutes)', [AvgTime, AvgTime / 60.0]));
      WriteLn(Format('     Min time: %d seconds', [MinTime]));
      WriteLn(Format('     Max time: %d seconds', [MaxTime]));
    end;
  finally
    DS.Free;
  end;

  // Time distribution
  WriteLn('   Conversion time distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE ' +
    '    WHEN time_to_convert_seconds < 300 THEN ''< 5 min'' ' +
    '    WHEN time_to_convert_seconds < 600 THEN ''5-10 min'' ' +
    '    WHEN time_to_convert_seconds < 1800 THEN ''10-30 min'' ' +
    '    ELSE ''> 30 min'' ' +
    '  END as time_bucket, ' +
    '  COUNT(*) as user_count ' +
    'FROM funnel_conversions ' +
    'WHERE funnel_id = 1 AND completed = 1 AND time_to_convert_seconds IS NOT NULL ' +
    'GROUP BY time_bucket ' +
    'ORDER BY MIN(time_to_convert_seconds)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d users',
        [DS.FieldByName('time_bucket').AsString,
         DS.FieldByName('user_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Compares control and variant_b conversion rates, average steps reached, and average convert times, then calculates the lift. }
procedure AnalyzeABTest;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('10. A/B Test Results (Signup Funnel)');

  DS := Conn.ExecuteQuery(
    'SELECT ab.variant_name, ' +
    '  COUNT(*) as total_users, ' +
    '  SUM(fc.completed) as converted, ' +
    '  ROUND(SUM(fc.completed) * 100.0 / COUNT(*), 1) as conv_rate, ' +
    '  ROUND(AVG(fc.step_reached), 2) as avg_step, ' +
    '  CAST(COALESCE(AVG(CASE WHEN fc.completed = 1 THEN fc.time_to_convert_seconds END), 0) AS INTEGER) as avg_convert_time ' +
    'FROM ab_variants ab ' +
    'JOIN funnel_conversions fc ON fc.funnel_id = ab.funnel_id AND fc.user_id = ab.user_id ' +
    'WHERE ab.funnel_id = 1 ' +
    'GROUP BY ab.variant_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Variant: %s', [DS.FieldByName('variant_name').AsString]));
      WriteLn(Format('     Users: %d, Converted: %d (%.1f%%)',
        [DS.FieldByName('total_users').AsInteger,
         DS.FieldByName('converted').AsInteger,
         DS.FieldByName('conv_rate').AsFloat]));
      WriteLn(Format('     Avg step reached: %.2f, Avg convert time: %d sec',
        [DS.FieldByName('avg_step').AsFloat,
         DS.FieldByName('avg_convert_time').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Statistical comparison
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT ROUND(SUM(fc.completed) * 100.0 / COUNT(*), 2) ' +
    '   FROM ab_variants ab JOIN funnel_conversions fc ON fc.user_id = ab.user_id AND fc.funnel_id = ab.funnel_id ' +
    '   WHERE ab.variant_name = ''variant_b'' AND ab.funnel_id = 1) as variant_rate, ' +
    '  (SELECT ROUND(SUM(fc.completed) * 100.0 / COUNT(*), 2) ' +
    '   FROM ab_variants ab JOIN funnel_conversions fc ON fc.user_id = ab.user_id AND fc.funnel_id = ab.funnel_id ' +
    '   WHERE ab.variant_name = ''control'' AND ab.funnel_id = 1) as control_rate');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Lift: %.2f%% (variant_b vs control)',
        [DS.FieldByName('variant_rate').AsFloat - DS.FieldByName('control_rate').AsFloat]));
    end;
  finally
    DS.Free;
  end;
end;

{ Groups landing and verified events by weekly period and shows converted users over time. }
procedure AnalyzeFunnelTrend;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('11. Weekly Funnel Trend');

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE ' +
    '    WHEN julianday(''now'') - julianday(created_at) <= 7 THEN ''This week'' ' +
    '    WHEN julianday(''now'') - julianday(created_at) <= 14 THEN ''Last week'' ' +
    '    WHEN julianday(''now'') - julianday(created_at) <= 21 THEN ''2 weeks ago'' ' +
    '    ELSE ''3+ weeks ago'' ' +
    '  END as period, ' +
    '  COUNT(DISTINCT CASE WHEN event_name = ''page_landing'' THEN user_id END) as landed, ' +
    '  COUNT(DISTINCT CASE WHEN event_name = ''email_verified'' THEN user_id END) as converted ' +
    'FROM user_events ' +
    'WHERE event_name IN (''page_landing'', ''email_verified'') ' +
    'GROUP BY period ' +
    'ORDER BY MIN(julianday(''now'') - julianday(created_at))');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d landed, %d converted',
        [DS.FieldByName('period').AsString,
         DS.FieldByName('landed').AsInteger,
         DS.FieldByName('converted').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Displays funnel summary information. }
procedure ShowFunnelSummary;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('12. Funnel Summary');

  DS := Conn.ExecuteQuery(
    'SELECT f.name, ' +
    '  (SELECT COUNT(*) FROM funnel_steps WHERE funnel_id = f.id) as steps, ' +
    '  COUNT(fc.id) as total_users, ' +
    '  SUM(fc.completed) as converted, ' +
    '  ROUND(SUM(fc.completed) * 100.0 / COUNT(fc.id), 1) as overall_rate ' +
    'FROM funnels f ' +
    'JOIN funnel_conversions fc ON fc.funnel_id = f.id ' +
    'GROUP BY f.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d steps, %d users, %d converted (%.1f%%)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('steps').AsInteger,
         DS.FieldByName('total_users').AsInteger,
         DS.FieldByName('converted').AsInteger,
         DS.FieldByName('overall_rate').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total events tracked: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM user_events'))]));
  WriteLn(Format('   Total conversions: %d',
    [Integer(Conn.ExecuteScalar('SELECT SUM(completed) FROM funnel_conversions'))]));
end;

begin
  Randomize;
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');
    Conn.ExecuteNonQuery('PRAGMA foreign_keys=ON');

    CreateSchema;
    DefineFunnels;
    GenerateUserEvents;
    AnalyzeSignupFunnel;
    AnalyzePurchaseFunnel;
    AnalyzeDropOff;
    AnalyzeBySource;
    AnalyzeByDevice;
    AnalyzeTimeToConvert;
    AnalyzeABTest;
    AnalyzeFunnelTrend;
    ShowFunnelSummary;

    WriteLn;
    WriteLn('=== Example Complete ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
