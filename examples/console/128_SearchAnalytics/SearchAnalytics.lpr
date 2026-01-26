{===============================================================================
  NDXSQLite Example 128 - Search Analytics
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Query logging and analysis
  - Zero-result query identification
  - Click-through rate tracking
  - Query refinement pattern analysis
  - Search session and performance metrics

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SearchAnalytics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Query logs
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS query_logs (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  session_id TEXT NOT NULL,' +
    '  user_id TEXT,' +
    '  query_text TEXT NOT NULL,' +
    '  result_count INTEGER DEFAULT 0,' +
    '  response_time_ms INTEGER DEFAULT 0,' +
    '  page_number INTEGER DEFAULT 1,' +
    '  searched_at TEXT NOT NULL' +
    ')'
  );

  // Click events
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS click_events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  query_log_id INTEGER NOT NULL,' +
    '  result_position INTEGER NOT NULL,' +
    '  result_title TEXT NOT NULL,' +
    '  clicked_at TEXT NOT NULL,' +
    '  dwell_time_sec INTEGER DEFAULT 0,' +
    '  FOREIGN KEY (query_log_id) REFERENCES query_logs(id)' +
    ')'
  );

  // Search sessions (group of related searches)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS search_sessions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  session_id TEXT NOT NULL UNIQUE,' +
    '  user_id TEXT,' +
    '  query_count INTEGER DEFAULT 0,' +
    '  click_count INTEGER DEFAULT 0,' +
    '  started_at TEXT NOT NULL,' +
    '  ended_at TEXT,' +
    '  converted INTEGER DEFAULT 0' +
    ')'
  );

  // Refinement patterns
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS refinements (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  session_id TEXT NOT NULL,' +
    '  original_query TEXT NOT NULL,' +
    '  refined_query TEXT NOT NULL,' +
    '  refinement_type TEXT NOT NULL,' +  // narrow, broaden, rephrase, correct
    '  sequence_num INTEGER NOT NULL' +
    ')'
  );
end;

{ Inserts sample data for demonstrations. }
procedure InsertSampleData;
begin
  // === Sessions ===
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-001'', ''alice'', 3, 2, ''2024-01-20 10:00:00'', ''2024-01-20 10:08:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-002'', ''bob'', 4, 3, ''2024-01-20 11:00:00'', ''2024-01-20 11:12:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-003'', ''carol'', 2, 0, ''2024-01-20 14:00:00'', ''2024-01-20 14:03:00'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-004'', ''dave'', 1, 1, ''2024-01-20 15:30:00'', ''2024-01-20 15:32:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-005'', ''alice'', 2, 1, ''2024-01-21 09:00:00'', ''2024-01-21 09:05:00'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-006'', ''eve'', 5, 2, ''2024-01-21 13:00:00'', ''2024-01-21 13:20:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-007'', ''bob'', 1, 1, ''2024-01-21 16:00:00'', ''2024-01-21 16:02:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-008'', ''frank'', 3, 0, ''2024-01-22 08:00:00'', ''2024-01-22 08:06:00'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-009'', ''carol'', 2, 2, ''2024-01-22 10:00:00'', ''2024-01-22 10:07:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO search_sessions (session_id, user_id, query_count, click_count, started_at, ended_at, converted) VALUES (''sess-010'', ''alice'', 1, 0, ''2024-01-22 14:00:00'', ''2024-01-22 14:01:00'', 0)');

  // === Query Logs ===
  // Session 1: alice searches for headphones (refinement: narrow)
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-001'', ''alice'', ''headphones'', 45, 120, ''2024-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-001'', ''alice'', ''wireless headphones'', 23, 95, ''2024-01-20 10:02:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-001'', ''alice'', ''wireless headphones noise cancelling'', 8, 85, ''2024-01-20 10:05:00'')');

  // Session 2: bob searches for keyboard (refinement: narrow then rephrase)
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-002'', ''bob'', ''keyboard'', 60, 130, ''2024-01-20 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-002'', ''bob'', ''mechanical keyboard'', 28, 105, ''2024-01-20 11:03:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-002'', ''bob'', ''mechanical keyboard blue switches'', 5, 78, ''2024-01-20 11:06:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-002'', ''bob'', ''clicky mechanical keyboard'', 12, 92, ''2024-01-20 11:09:00'')');

  // Session 3: carol searches for something not found
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-003'', ''carol'', ''holographic display'', 0, 110, ''2024-01-20 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-003'', ''carol'', ''3d display'', 0, 98, ''2024-01-20 14:02:00'')');

  // Session 4: dave direct search
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-004'', ''dave'', ''usb-c cable 2m'', 6, 75, ''2024-01-20 15:30:00'')');

  // Session 5: alice searches for monitor
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-005'', ''alice'', ''4k monitor'', 15, 88, ''2024-01-21 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-005'', ''alice'', ''4k monitor 27 inch'', 4, 72, ''2024-01-21 09:03:00'')');

  // Session 6: eve long search journey
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-006'', ''eve'', ''laptop'', 120, 150, ''2024-01-21 13:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-006'', ''eve'', ''laptop stand'', 18, 90, ''2024-01-21 13:04:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-006'', ''eve'', ''laptop cooling'', 9, 82, ''2024-01-21 13:08:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-006'', ''eve'', ''laptop accessories'', 35, 115, ''2024-01-21 13:12:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-006'', ''eve'', ''ergonomic laptop stand'', 3, 68, ''2024-01-21 13:16:00'')');

  // Session 7: bob quick search
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-007'', ''bob'', ''logitech mx master'', 3, 65, ''2024-01-21 16:00:00'')');

  // Session 8: frank zero-result searches
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-008'', ''frank'', ''quantum computer kit'', 0, 105, ''2024-01-22 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-008'', ''frank'', ''ai chip'', 0, 92, ''2024-01-22 08:03:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-008'', ''frank'', ''neural processor'', 0, 88, ''2024-01-22 08:05:00'')');

  // Session 9: carol successful search
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-009'', ''carol'', ''power bank'', 22, 95, ''2024-01-22 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-009'', ''carol'', ''power bank 20000mah'', 5, 70, ''2024-01-22 10:04:00'')');

  // Session 10: alice abandoned search
  Conn.ExecuteNonQuery('INSERT INTO query_logs (session_id, user_id, query_text, result_count, response_time_ms, searched_at) VALUES (''sess-010'', ''alice'', ''bluetooth speaker waterproof'', 2, 78, ''2024-01-22 14:00:00'')');

  // === Click Events ===
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (2, 1, ''Sony WH-1000XM5'', ''2024-01-20 10:02:30'', 45)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (3, 2, ''Bose QuietComfort 45'', ''2024-01-20 10:05:30'', 120)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (5, 1, ''Keychron K8 Pro'', ''2024-01-20 11:03:30'', 60)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (6, 3, ''Gateron Blue Switch'', ''2024-01-20 11:07:00'', 30)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (7, 1, ''Corsair K70 RGB'', ''2024-01-20 11:09:30'', 90)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (10, 2, ''Anker USB-C Cable'', ''2024-01-20 15:30:30'', 25)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (12, 3, ''Dell U2723QE'', ''2024-01-21 09:01:00'', 55)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (16, 1, ''Rain mStand'', ''2024-01-21 13:05:00'', 40)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (17, 1, ''Ergonomic Laptop Riser'', ''2024-01-21 13:17:00'', 95)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (18, 1, ''Logitech MX Master 3S'', ''2024-01-21 16:00:30'', 80)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (22, 1, ''Anker PowerCore'', ''2024-01-22 10:01:00'', 50)');
  Conn.ExecuteNonQuery('INSERT INTO click_events (query_log_id, result_position, result_title, clicked_at, dwell_time_sec) VALUES (23, 1, ''Anker 737 Power Bank'', ''2024-01-22 10:04:30'', 110)');

  // === Refinements ===
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-001'', ''headphones'', ''wireless headphones'', ''narrow'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-001'', ''wireless headphones'', ''wireless headphones noise cancelling'', ''narrow'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-002'', ''keyboard'', ''mechanical keyboard'', ''narrow'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-002'', ''mechanical keyboard'', ''mechanical keyboard blue switches'', ''narrow'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-002'', ''mechanical keyboard blue switches'', ''clicky mechanical keyboard'', ''rephrase'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-003'', ''holographic display'', ''3d display'', ''rephrase'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-005'', ''4k monitor'', ''4k monitor 27 inch'', ''narrow'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-006'', ''laptop'', ''laptop stand'', ''narrow'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-006'', ''laptop stand'', ''laptop cooling'', ''rephrase'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-006'', ''laptop cooling'', ''laptop accessories'', ''broaden'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-006'', ''laptop accessories'', ''ergonomic laptop stand'', ''narrow'', 4)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-008'', ''quantum computer kit'', ''ai chip'', ''rephrase'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-008'', ''ai chip'', ''neural processor'', ''rephrase'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO refinements (session_id, original_query, refined_query, refinement_type, sequence_num) VALUES (''sess-009'', ''power bank'', ''power bank 20000mah'', ''narrow'', 1)');
end;

// ============================================================
// Demo Sections
// ============================================================

{ Displays total queries, unique users, session count, and the 8 most recent queries with result counts and response times. }
procedure Demo1_QueryLogs;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Query Logs ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM query_logs');
  try
    WriteLn(Format('   Total queries logged: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(DISTINCT user_id) as cnt FROM query_logs');
  try
    WriteLn(Format('   Unique users: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(DISTINCT session_id) as cnt FROM query_logs');
  try
    WriteLn(Format('   Sessions: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Recent queries:');
  DS := Conn.ExecuteQuery(
    'SELECT user_id, query_text, result_count, response_time_ms, searched_at ' +
    'FROM query_logs ORDER BY searched_at DESC LIMIT 8');
  try
    WriteLn(Format('   %-8s %-35s %-8s %-6s %s',
      ['User', 'Query', 'Results', 'Ms', 'Time']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-35s %-8d %-6d %s',
        [DS.FieldByName('user_id').AsString,
         DS.FieldByName('query_text').AsString,
         DS.FieldByName('result_count').AsInteger,
         DS.FieldByName('response_time_ms').AsInteger,
         DS.FieldByName('searched_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Identifies queries that returned no results, lists failed searches with session info, and finds sessions with only zero-result queries. }
procedure Demo2_ZeroResultQueries;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Zero-Result Queries ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM query_logs WHERE result_count = 0');
  try
    WriteLn(Format('   Zero-result queries: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as total FROM query_logs');
  try
    WriteLn(Format('   Total queries: %d', [DS.FieldByName('total').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Failed searches:');
  DS := Conn.ExecuteQuery(
    'SELECT user_id, query_text, session_id, searched_at ' +
    'FROM query_logs WHERE result_count = 0 ORDER BY searched_at');
  try
    WriteLn(Format('   %-8s %-30s %-12s %s',
      ['User', 'Query', 'Session', 'Time']));
    WriteLn('   ' + StringOfChar('-', 65));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-30s %-12s %s',
        [DS.FieldByName('user_id').AsString,
         DS.FieldByName('query_text').AsString,
         DS.FieldByName('session_id').AsString,
         DS.FieldByName('searched_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Sessions with only zero-result queries:');
  DS := Conn.ExecuteQuery(
    'SELECT session_id, user_id, COUNT(*) as attempts ' +
    'FROM query_logs WHERE session_id IN ' +
    '(SELECT session_id FROM query_logs GROUP BY session_id ' +
    'HAVING MAX(result_count) = 0) ' +
    'GROUP BY session_id ORDER BY attempts DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s (%s): %d failed attempts',
        [DS.FieldByName('session_id').AsString,
         DS.FieldByName('user_id').AsString,
         DS.FieldByName('attempts').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Calculates overall click-through rate, shows click distribution by result position with average dwell time, and lists top clicked results. }
procedure Demo3_ClickThroughRates;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Click-Through Rates ===');
  WriteLn;

  // Overall CTR
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT ql.id) as queries_with_clicks, ' +
    '(SELECT COUNT(*) FROM query_logs WHERE result_count > 0) as queries_with_results ' +
    'FROM query_logs ql JOIN click_events ce ON ql.id = ce.query_log_id');
  try
    WriteLn(Format('   Queries with results: %d', [DS.FieldByName('queries_with_results').AsInteger]));
    WriteLn(Format('   Queries with clicks: %d', [DS.FieldByName('queries_with_clicks').AsInteger]));
    if DS.FieldByName('queries_with_results').AsInteger > 0 then
      WriteLn(Format('   Click-through rate: %.1f%%',
        [DS.FieldByName('queries_with_clicks').AsInteger * 100.0 /
         DS.FieldByName('queries_with_results').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Clicks by position:');
  DS := Conn.ExecuteQuery(
    'SELECT result_position, COUNT(*) as clicks, ' +
    'ROUND(AVG(dwell_time_sec), 0) as avg_dwell ' +
    'FROM click_events GROUP BY result_position ORDER BY result_position');
  try
    WriteLn(Format('   %-10s %-8s %s', ['Position', 'Clicks', 'Avg Dwell(s)']));
    WriteLn('   ' + StringOfChar('-', 32));
    while not DS.EOF do
    begin
      WriteLn(Format('   #%-9d %-8d %s',
        [DS.FieldByName('result_position').AsInteger,
         DS.FieldByName('clicks').AsInteger,
         DS.FieldByName('avg_dwell').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Top clicked results:');
  DS := Conn.ExecuteQuery(
    'SELECT result_title, COUNT(*) as clicks, ' +
    'ROUND(AVG(dwell_time_sec), 0) as avg_dwell ' +
    'FROM click_events GROUP BY result_title ORDER BY clicks DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %d clicks, %ss avg dwell',
        [DS.FieldByName('result_title').AsString,
         DS.FieldByName('clicks').AsInteger,
         DS.FieldByName('avg_dwell').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Aggregates refinement types (narrow, broaden, rephrase) with counts and displays full refinement sequences per session. }
procedure Demo4_RefinementPatterns;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Search Refinement Patterns ===');
  WriteLn;

  WriteLn('   Refinement types:');
  DS := Conn.ExecuteQuery(
    'SELECT refinement_type, COUNT(*) as cnt FROM refinements ' +
    'GROUP BY refinement_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d occurrences',
        [DS.FieldByName('refinement_type').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Refinement sequences:');
  DS := Conn.ExecuteQuery(
    'SELECT session_id, original_query, refined_query, refinement_type, sequence_num ' +
    'FROM refinements ORDER BY session_id, sequence_num');
  try
    WriteLn(Format('   %-10s %-4s %-30s -> %-30s %s',
      ['Session', 'Seq', 'From', 'To', 'Type']));
    WriteLn('   ' + StringOfChar('-', 90));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s #%-3d %-30s -> %-30s %s',
        [DS.FieldByName('session_id').AsString,
         DS.FieldByName('sequence_num').AsInteger,
         DS.FieldByName('original_query').AsString,
         DS.FieldByName('refined_query').AsString,
         DS.FieldByName('refinement_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Groups queries by result count ranges with histogram bars, and analyzes query length (word count) versus average results returned. }
procedure Demo5_QueryDistribution;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Query Distribution ===');
  WriteLn;

  WriteLn('   Queries by result count range:');
  DS := Conn.ExecuteQuery(
    'SELECT CASE ' +
    '  WHEN result_count = 0 THEN ''0 results'' ' +
    '  WHEN result_count <= 5 THEN ''1-5 results'' ' +
    '  WHEN result_count <= 20 THEN ''6-20 results'' ' +
    '  WHEN result_count <= 50 THEN ''21-50 results'' ' +
    '  ELSE ''50+ results'' END as range_bucket, ' +
    'COUNT(*) as cnt FROM query_logs GROUP BY range_bucket ' +
    'ORDER BY MIN(result_count)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %d queries  %s',
        [DS.FieldByName('range_bucket').AsString,
         DS.FieldByName('cnt').AsInteger,
         StringOfChar('#', DS.FieldByName('cnt').AsInteger * 2)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Queries by word count:');
  DS := Conn.ExecuteQuery(
    'SELECT LENGTH(query_text) - LENGTH(REPLACE(query_text, '' '', '''')) + 1 as word_count, ' +
    'COUNT(*) as cnt, ROUND(AVG(result_count), 0) as avg_results ' +
    'FROM query_logs GROUP BY word_count ORDER BY word_count');
  try
    WriteLn(Format('   %-8s %-8s %s', ['Words', 'Queries', 'Avg Results']));
    WriteLn('   ' + StringOfChar('-', 30));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8d %-8d %s',
        [DS.FieldByName('word_count').AsInteger,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('avg_results').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Reports average, min, and max response times; correlates response time with result count; and lists slow queries exceeding 100ms. }
procedure Demo6_ResponseTimeAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Response Time Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT ROUND(AVG(response_time_ms), 0) as avg_ms, ' +
    'MIN(response_time_ms) as min_ms, MAX(response_time_ms) as max_ms ' +
    'FROM query_logs');
  try
    WriteLn(Format('   Average response time: %s ms', [DS.FieldByName('avg_ms').AsString]));
    WriteLn(Format('   Min: %d ms, Max: %d ms',
      [DS.FieldByName('min_ms').AsInteger, DS.FieldByName('max_ms').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Response time by result count:');
  DS := Conn.ExecuteQuery(
    'SELECT CASE ' +
    '  WHEN result_count = 0 THEN ''0 results'' ' +
    '  WHEN result_count <= 10 THEN ''1-10'' ' +
    '  WHEN result_count <= 50 THEN ''11-50'' ' +
    '  ELSE ''50+'' END as bucket, ' +
    'COUNT(*) as queries, ROUND(AVG(response_time_ms), 0) as avg_ms ' +
    'FROM query_logs GROUP BY bucket ORDER BY avg_ms');
  try
    WriteLn(Format('   %-12s %-8s %s', ['Results', 'Queries', 'Avg Ms']));
    WriteLn('   ' + StringOfChar('-', 30));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-8d %s',
        [DS.FieldByName('bucket').AsString,
         DS.FieldByName('queries').AsInteger,
         DS.FieldByName('avg_ms').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Slow queries (>100ms):');
  DS := Conn.ExecuteQuery(
    'SELECT query_text, result_count, response_time_ms FROM query_logs ' +
    'WHERE response_time_ms > 100 ORDER BY response_time_ms DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-35s %3d results  %d ms',
        [DS.FieldByName('query_text').AsString,
         DS.FieldByName('result_count').AsInteger,
         DS.FieldByName('response_time_ms').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Lists all search sessions with query/click counts and conversion status; calculates average queries, clicks per session, and conversion rate. }
procedure Demo7_SearchSessions;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Search Sessions ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT session_id, user_id, query_count, click_count, ' +
    'converted, started_at ' +
    'FROM search_sessions ORDER BY started_at');
  try
    WriteLn(Format('   %-10s %-8s %-8s %-7s %-9s %s',
      ['Session', 'User', 'Queries', 'Clicks', 'Converted', 'Started']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-8s %-8d %-7d %-9s %s',
        [DS.FieldByName('session_id').AsString,
         DS.FieldByName('user_id').AsString,
         DS.FieldByName('query_count').AsInteger,
         DS.FieldByName('click_count').AsInteger,
         BoolToStr(DS.FieldByName('converted').AsInteger = 1, 'yes', 'no'),
         DS.FieldByName('started_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Session metrics:');
  DS := Conn.ExecuteQuery(
    'SELECT ROUND(AVG(query_count), 1) as avg_queries, ' +
    'ROUND(AVG(click_count), 1) as avg_clicks, ' +
    'SUM(converted) as conversions, COUNT(*) as total ' +
    'FROM search_sessions');
  try
    WriteLn(Format('   Avg queries/session: %s', [DS.FieldByName('avg_queries').AsString]));
    WriteLn(Format('   Avg clicks/session: %s', [DS.FieldByName('avg_clicks').AsString]));
    WriteLn(Format('   Conversion rate: %.1f%% (%d/%d)',
      [DS.FieldByName('conversions').AsInteger * 100.0 / DS.FieldByName('total').AsInteger,
       DS.FieldByName('conversions').AsInteger,
       DS.FieldByName('total').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Ranks queries by click-through rate percentage with dwell times, and lists queries from sessions that resulted in conversions. }
procedure Demo8_TopPerformers;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Top Performing Queries ===');
  WriteLn;

  WriteLn('   Queries with highest click-through:');
  DS := Conn.ExecuteQuery(
    'SELECT ql.query_text, ql.result_count, COUNT(ce.id) as clicks, ' +
    'ROUND(COUNT(ce.id) * 100.0 / ql.result_count, 1) as ctr_pct, ' +
    'ROUND(AVG(ce.dwell_time_sec), 0) as avg_dwell ' +
    'FROM query_logs ql JOIN click_events ce ON ql.id = ce.query_log_id ' +
    'WHERE ql.result_count > 0 ' +
    'GROUP BY ql.id ORDER BY ctr_pct DESC LIMIT 8');
  try
    WriteLn(Format('   %-35s %-8s %-6s %-8s %s',
      ['Query', 'Results', 'Clicks', 'CTR', 'Dwell']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-35s %-8d %-6d %-8s %ss',
        [DS.FieldByName('query_text').AsString,
         DS.FieldByName('result_count').AsInteger,
         DS.FieldByName('clicks').AsInteger,
         DS.FieldByName('ctr_pct').AsString + '%',
         DS.FieldByName('avg_dwell').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Queries leading to conversions:');
  DS := Conn.ExecuteQuery(
    'SELECT ql.query_text, ss.session_id FROM query_logs ql ' +
    'JOIN search_sessions ss ON ql.session_id = ss.session_id ' +
    'WHERE ss.converted = 1 ' +
    'GROUP BY ql.query_text ORDER BY ql.searched_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   -> %-35s [%s]',
        [DS.FieldByName('query_text').AsString,
         DS.FieldByName('session_id').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Builds a search funnel showing sessions to results to clicks to conversions, with percentages and drop-off analysis at each stage. }
procedure Demo9_ConversionFunnel;
var
  DS: TDataSet;
  TotalSessions, WithResults, WithClicks, Converted: Integer;
begin
  WriteLn('=== 9. Conversion Funnel ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM search_sessions');
  try
    TotalSessions := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT session_id) as cnt FROM query_logs WHERE result_count > 0');
  try
    WithResults := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM search_sessions WHERE click_count > 0');
  try
    WithClicks := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM search_sessions WHERE converted = 1');
  try
    Converted := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn('   Search -> Results -> Click -> Conversion funnel:');
  WriteLn;
  WriteLn(Format('   Sessions:     %d  (100%%)', [TotalSessions]));
  WriteLn(Format('   Got results:  %d  (%.0f%%)',
    [WithResults, WithResults * 100.0 / TotalSessions]));
  WriteLn(Format('   Clicked:      %d  (%.0f%%)',
    [WithClicks, WithClicks * 100.0 / TotalSessions]));
  WriteLn(Format('   Converted:    %d  (%.0f%%)',
    [Converted, Converted * 100.0 / TotalSessions]));

  WriteLn;
  WriteLn('   Funnel visualization:');
  WriteLn(Format('   Sessions     %s %d', [StringOfChar('#', TotalSessions * 3), TotalSessions]));
  WriteLn(Format('   Results      %s %d', [StringOfChar('#', WithResults * 3), WithResults]));
  WriteLn(Format('   Clicks       %s %d', [StringOfChar('#', WithClicks * 3), WithClicks]));
  WriteLn(Format('   Conversions  %s %d', [StringOfChar('#', Converted * 3), Converted]));

  WriteLn;
  WriteLn('   Drop-off analysis:');
  WriteLn(Format('   Sessions -> Results: %.0f%% drop-off',
    [(TotalSessions - WithResults) * 100.0 / TotalSessions]));
  WriteLn(Format('   Results -> Clicks:   %.0f%% drop-off',
    [(WithResults - WithClicks) * 100.0 / WithResults]));
  WriteLn(Format('   Clicks -> Convert:   %.0f%% drop-off',
    [(WithClicks - Converted) * 100.0 / WithClicks]));

  WriteLn;
end;

{ Demonstrates search analytics statistics and query performance metrics. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM query_logs');
  try
    WriteLn(Format('   Total queries: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM click_events');
  try
    WriteLn(Format('   Total clicks: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM search_sessions');
  try
    WriteLn(Format('   Total sessions: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM refinements');
  try
    WriteLn(Format('   Total refinements: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Per-user summary:');
  DS := Conn.ExecuteQuery(
    'SELECT user_id, COUNT(*) as queries, ' +
    'SUM(CASE WHEN result_count = 0 THEN 1 ELSE 0 END) as zero_results, ' +
    'ROUND(AVG(result_count), 0) as avg_results ' +
    'FROM query_logs GROUP BY user_id ORDER BY queries DESC');
  try
    WriteLn(Format('   %-8s %-8s %-12s %s',
      ['User', 'Queries', 'Zero-Result', 'Avg Results']));
    WriteLn('   ' + StringOfChar('-', 42));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-8d %-12d %s',
        [DS.FieldByName('user_id').AsString,
         DS.FieldByName('queries').AsInteger,
         DS.FieldByName('zero_results').AsInteger,
         DS.FieldByName('avg_results').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Search quality metrics:');
  DS := Conn.ExecuteQuery(
    'SELECT ROUND(AVG(result_count), 1) as avg_results, ' +
    'ROUND(AVG(response_time_ms), 0) as avg_response, ' +
    'SUM(CASE WHEN result_count = 0 THEN 1 ELSE 0 END) * 100.0 / COUNT(*) as zero_pct ' +
    'FROM query_logs');
  try
    WriteLn(Format('   Avg results per query: %s', [DS.FieldByName('avg_results').AsString]));
    WriteLn(Format('   Avg response time: %s ms', [DS.FieldByName('avg_response').AsString]));
    WriteLn(Format('   Zero-result rate: %.1f%%', [DS.FieldByName('zero_pct').AsFloat]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

// ============================================================
// Main
// ============================================================
begin
  WriteLn('Example 128: Search Analytics - Query Logs, CTR, Refinements, Funnels');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertSampleData;

    Demo1_QueryLogs;
    Demo2_ZeroResultQueries;
    Demo3_ClickThroughRates;
    Demo4_RefinementPatterns;
    Demo5_QueryDistribution;
    Demo6_ResponseTimeAnalysis;
    Demo7_SearchSessions;
    Demo8_TopPerformers;
    Demo9_ConversionFunnel;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
