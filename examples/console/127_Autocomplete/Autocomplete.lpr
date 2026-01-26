{===============================================================================
  NDXSQLite Example 127 - Autocomplete
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Prefix matching with vocabulary index
  - Frequency-ranked suggestions
  - Recent search history integration
  - Personalized suggestions per user
  - Multi-word typeahead simulation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Autocomplete;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Vocabulary (terms with global frequency)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS vocabulary (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  term TEXT NOT NULL UNIQUE,' +
    '  category TEXT,' +
    '  frequency INTEGER DEFAULT 0,' +
    '  last_used TEXT' +
    ')'
  );
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_vocab_term ON vocabulary(term)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_vocab_freq ON vocabulary(frequency DESC)');

  // Search history (per-user)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS search_history (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id TEXT NOT NULL,' +
    '  query_text TEXT NOT NULL,' +
    '  result_count INTEGER DEFAULT 0,' +
    '  selected_result TEXT,' +
    '  searched_at TEXT NOT NULL' +
    ')'
  );
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_history_user ON search_history(user_id, searched_at DESC)');

  // User preferences (personalization)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS user_preferences (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  weight REAL DEFAULT 1.0,' +
    '  UNIQUE(user_id, category)' +
    ')'
  );

  // Trending terms
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS trending (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  term TEXT NOT NULL,' +
    '  search_count INTEGER DEFAULT 0,' +
    '  period TEXT NOT NULL,' +
    '  trend_score REAL DEFAULT 0' +
    ')'
  );
end;

{ Inserts vocabulary entries for autocomplete. }
procedure InsertVocabulary;
begin
  // Electronics / tech terms
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''bluetooth headphones'', ''electronics'', 850)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''bluetooth speaker'', ''electronics'', 720)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''bluetooth adapter'', ''electronics'', 340)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''bluetooth earbuds'', ''electronics'', 680)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''wireless mouse'', ''peripherals'', 920)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''wireless keyboard'', ''peripherals'', 780)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''wireless charger'', ''accessories'', 650)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''wireless headset'', ''electronics'', 540)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''usb cable'', ''accessories'', 1200)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''usb hub'', ''accessories'', 480)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''usb adapter'', ''accessories'', 390)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''usb drive'', ''storage'', 560)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''laptop stand'', ''accessories'', 420)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''laptop bag'', ''accessories'', 380)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''laptop charger'', ''accessories'', 510)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''laptop cooling pad'', ''accessories'', 290)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''monitor 4k'', ''displays'', 610)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''monitor arm'', ''accessories'', 350)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''monitor cable'', ''accessories'', 280)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''mechanical keyboard'', ''peripherals'', 890)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''gaming mouse'', ''peripherals'', 760)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''gaming headset'', ''electronics'', 580)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''gaming keyboard'', ''peripherals'', 670)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''power bank'', ''accessories'', 830)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''power strip'', ''accessories'', 310)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''external ssd'', ''storage'', 720)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''external hard drive'', ''storage'', 540)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''webcam hd'', ''peripherals'', 450)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''webcam 4k'', ''peripherals'', 320)');
  Conn.ExecuteNonQuery('INSERT INTO vocabulary (term, category, frequency) VALUES (''desk lamp'', ''lighting'', 260)');
end;

{ Inserts search history entries. }
procedure InsertSearchHistory;
begin
  // User alice - prefers electronics
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''alice'', ''bluetooth headphones'', 15, ''Sony WH-1000XM5'', ''2024-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''alice'', ''bluetooth earbuds'', 12, ''AirPods Pro'', ''2024-01-20 10:05:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''alice'', ''wireless headset'', 8, ''Jabra Evolve2'', ''2024-01-21 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''alice'', ''bluetooth speaker'', 10, ''JBL Flip 6'', ''2024-01-22 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''alice'', ''gaming headset'', 9, ''SteelSeries Arctis'', ''2024-01-23 16:00:00'')');

  // User bob - prefers peripherals/gaming
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''bob'', ''mechanical keyboard'', 20, ''Keychron K8'', ''2024-01-20 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''bob'', ''gaming mouse'', 18, ''Logitech G502'', ''2024-01-20 11:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''bob'', ''gaming keyboard'', 14, ''Corsair K70'', ''2024-01-21 15:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''bob'', ''wireless mouse'', 16, ''Logitech MX Master'', ''2024-01-22 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''bob'', ''monitor 4k'', 11, ''Dell U2723QE'', ''2024-01-23 10:00:00'')');

  // User carol - prefers accessories
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''carol'', ''usb cable'', 25, ''Anker USB-C'', ''2024-01-20 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''carol'', ''laptop stand'', 10, ''Rain mStand'', ''2024-01-21 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''carol'', ''power bank'', 13, ''Anker 737'', ''2024-01-22 17:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''carol'', ''wireless charger'', 8, ''Belkin MagSafe'', ''2024-01-23 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO search_history (user_id, query_text, result_count, selected_result, searched_at) VALUES (''carol'', ''laptop bag'', 7, ''Thule Subterra'', ''2024-01-23 14:00:00'')');

  // Setup user preferences based on history
  Conn.ExecuteNonQuery('INSERT INTO user_preferences (user_id, category, weight) VALUES (''alice'', ''electronics'', 2.5)');
  Conn.ExecuteNonQuery('INSERT INTO user_preferences (user_id, category, weight) VALUES (''alice'', ''peripherals'', 1.0)');
  Conn.ExecuteNonQuery('INSERT INTO user_preferences (user_id, category, weight) VALUES (''bob'', ''peripherals'', 2.5)');
  Conn.ExecuteNonQuery('INSERT INTO user_preferences (user_id, category, weight) VALUES (''bob'', ''displays'', 1.5)');
  Conn.ExecuteNonQuery('INSERT INTO user_preferences (user_id, category, weight) VALUES (''carol'', ''accessories'', 2.5)');
  Conn.ExecuteNonQuery('INSERT INTO user_preferences (user_id, category, weight) VALUES (''carol'', ''storage'', 1.2)');

  // Trending data
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''wireless mouse'', 245, ''2024-01-W4'', 8.5)');
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''mechanical keyboard'', 198, ''2024-01-W4'', 7.2)');
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''usb cable'', 312, ''2024-01-W4'', 6.8)');
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''bluetooth headphones'', 187, ''2024-01-W4'', 6.5)');
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''power bank'', 156, ''2024-01-W4'', 5.9)');
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''external ssd'', 143, ''2024-01-W4'', 5.4)');
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''gaming mouse'', 134, ''2024-01-W4'', 5.1)');
  Conn.ExecuteNonQuery('INSERT INTO trending (term, search_count, period, trend_score) VALUES (''monitor 4k'', 112, ''2024-01-W4'', 4.8)');
end;

// ============================================================
// Demo Sections
// ============================================================

{ Displays total vocabulary term count and lists the top 10 terms sorted by frequency in descending order. }
procedure Demo1_VocabularyIndex;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Vocabulary Index ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM vocabulary');
  try
    WriteLn(Format('   Total terms: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Top 10 by frequency:');
  DS := Conn.ExecuteQuery(
    'SELECT term, category, frequency FROM vocabulary ORDER BY frequency DESC LIMIT 10');
  try
    WriteLn(Format('   %-25s %-15s %s', ['Term', 'Category', 'Frequency']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-15s %d',
        [DS.FieldByName('term').AsString,
         DS.FieldByName('category').AsString,
         DS.FieldByName('frequency').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Tests multiple prefixes (blue, wire, usb, gam) and returns matching vocabulary terms sorted by frequency. }
procedure Demo2_PrefixMatching;
var
  DS: TDataSet;
  Prefixes: array[0..3] of string;
  I: Integer;
begin
  WriteLn('=== 2. Prefix Matching ===');
  WriteLn;

  Prefixes[0] := 'blue';
  Prefixes[1] := 'wire';
  Prefixes[2] := 'usb';
  Prefixes[3] := 'gam';

  for I := 0 to 3 do
  begin
    WriteLn(Format('   Prefix: "%s"', [Prefixes[I]]));
    DS := Conn.ExecuteQuery(Format(
      'SELECT term, frequency FROM vocabulary WHERE term LIKE ''%s%%'' ORDER BY frequency DESC',
      [Prefixes[I]]));
    try
      while not DS.EOF do
      begin
        WriteLn(Format('   -> %-30s (freq: %d)',
          [DS.FieldByName('term').AsString,
           DS.FieldByName('frequency').AsInteger]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;
  end;
end;

{ Displays ranked autocomplete suggestions for prefixes "lap" and "mon", ordering results by frequency descending. }
procedure Demo3_FrequencyRanking;
var
  DS: TDataSet;
  Prefix: string;
  Rank: Integer;
begin
  WriteLn('=== 3. Frequency-Weighted Ranking ===');
  WriteLn;

  Prefix := 'lap';
  WriteLn(Format('   Prefix: "%s" (sorted by frequency)', [Prefix]));
  WriteLn;

  Rank := 0;
  DS := Conn.ExecuteQuery(Format(
    'SELECT term, category, frequency FROM vocabulary ' +
    'WHERE term LIKE ''%s%%'' ORDER BY frequency DESC',
    [Prefix]));
  try
    WriteLn(Format('   %-5s %-25s %-15s %s', ['Rank', 'Suggestion', 'Category', 'Freq']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      Inc(Rank);
      WriteLn(Format('   #%-4d %-25s %-15s %d',
        [Rank,
         DS.FieldByName('term').AsString,
         DS.FieldByName('category').AsString,
         DS.FieldByName('frequency').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  Prefix := 'mon';
  WriteLn(Format('   Prefix: "%s" (sorted by frequency)', [Prefix]));
  WriteLn;

  Rank := 0;
  DS := Conn.ExecuteQuery(Format(
    'SELECT term, category, frequency FROM vocabulary ' +
    'WHERE term LIKE ''%s%%'' ORDER BY frequency DESC',
    [Prefix]));
  try
    WriteLn(Format('   %-5s %-25s %-15s %s', ['Rank', 'Suggestion', 'Category', 'Freq']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      Inc(Rank);
      WriteLn(Format('   #%-4d %-25s %-15s %d',
        [Rank,
         DS.FieldByName('term').AsString,
         DS.FieldByName('category').AsString,
         DS.FieldByName('frequency').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Retrieves the last 3 search history entries for each user (alice, bob, carol) showing query text, selected result, and timestamp. }
procedure Demo4_RecentSearches;
var
  DS: TDataSet;
  Users: array[0..2] of string;
  I: Integer;
begin
  WriteLn('=== 4. Recent Searches ===');
  WriteLn;

  Users[0] := 'alice';
  Users[1] := 'bob';
  Users[2] := 'carol';

  for I := 0 to 2 do
  begin
    WriteLn(Format('   User: %s (last 3 searches)', [Users[I]]));
    DS := Conn.ExecuteQuery(Format(
      'SELECT query_text, selected_result, searched_at FROM search_history ' +
      'WHERE user_id = ''%s'' ORDER BY searched_at DESC LIMIT 3',
      [Users[I]]));
    try
      while not DS.EOF do
      begin
        WriteLn(Format('   -> %-25s selected: %-20s %s',
          [DS.FieldByName('query_text').AsString,
           DS.FieldByName('selected_result').AsString,
           DS.FieldByName('searched_at').AsString]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;
  end;
end;

{ Demonstrates personalized autocomplete suggestions based on user history. }
procedure Demo5_Personalization;
var
  DS: TDataSet;
  Prefix, UserId: string;
  Users: array[0..2] of string;
  I: Integer;
begin
  WriteLn('=== 5. Per-User Personalization ===');
  WriteLn;

  Prefix := 'wire';
  Users[0] := 'alice';
  Users[1] := 'bob';
  Users[2] := 'carol';

  WriteLn(Format('   Prefix: "%s" - personalized for each user', [Prefix]));
  WriteLn;

  for I := 0 to 2 do
  begin
    UserId := Users[I];
    WriteLn(Format('   User: %s', [UserId]));

    // Get suggestions with user preference boost
    DS := Conn.ExecuteQuery(Format(
      'SELECT v.term, v.category, v.frequency, ' +
      'COALESCE(up.weight, 1.0) as pref_weight, ' +
      'ROUND(v.frequency * COALESCE(up.weight, 1.0), 0) as adjusted_score ' +
      'FROM vocabulary v ' +
      'LEFT JOIN user_preferences up ON v.category = up.category AND up.user_id = ''%s'' ' +
      'WHERE v.term LIKE ''%s%%'' ' +
      'ORDER BY adjusted_score DESC',
      [UserId, Prefix]));
    try
      while not DS.EOF do
      begin
        WriteLn(Format('   -> %-25s [%s] freq:%d x%.1f = %.0f',
          [DS.FieldByName('term').AsString,
           DS.FieldByName('category').AsString,
           DS.FieldByName('frequency').AsInteger,
           DS.FieldByName('pref_weight').AsFloat,
           DS.FieldByName('adjusted_score').AsFloat]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;
  end;
end;

{ Simulates character-by-character typing of "bluetooth" and shows top 3 suggestions after each character (starting at 2 characters). }
procedure Demo6_TypeaheadSimulation;
var
  DS: TDataSet;
  FullQuery: string;
  I: Integer;
  Prefix: string;
begin
  WriteLn('=== 6. Typeahead Simulation ===');
  WriteLn;

  FullQuery := 'bluetooth';
  WriteLn(Format('   Simulating typing: "%s"', [FullQuery]));
  WriteLn;

  for I := 1 to Length(FullQuery) do
  begin
    Prefix := Copy(FullQuery, 1, I);
    Write(Format('   "%s"', [Prefix]));

    if I >= 2 then
    begin
      DS := Conn.ExecuteQuery(Format(
        'SELECT term FROM vocabulary WHERE term LIKE ''%s%%'' ' +
        'ORDER BY frequency DESC LIMIT 3',
        [Prefix]));
      try
        Write(' -> ');
        while not DS.EOF do
        begin
          Write(Format('%s', [DS.FieldByName('term').AsString]));
          DS.Next;
          if not DS.EOF then
            Write(', ');
        end;
      finally
        DS.Free;
      end;
    end
    else
      Write(' -> (waiting for 2+ chars)');

    WriteLn;
  end;

  WriteLn;
end;

{ Shows second-word completions for "external" and "wireless", plus partial second-word matching for "laptop c". }
procedure Demo7_MultiWordSuggestions;
var
  DS: TDataSet;
  Prefix: string;
begin
  WriteLn('=== 7. Multi-Word Suggestions ===');
  WriteLn;

  // First word completed, suggest second word
  Prefix := 'external';
  WriteLn(Format('   First word: "%s"', [Prefix]));
  WriteLn('   Completing second word:');
  DS := Conn.ExecuteQuery(Format(
    'SELECT term, frequency FROM vocabulary WHERE term LIKE ''%s %%'' ORDER BY frequency DESC',
    [Prefix]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   -> %s (freq: %d)',
        [DS.FieldByName('term').AsString,
         DS.FieldByName('frequency').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  Prefix := 'wireless';
  WriteLn(Format('   First word: "%s"', [Prefix]));
  WriteLn('   Completing second word:');
  DS := Conn.ExecuteQuery(Format(
    'SELECT term, frequency FROM vocabulary WHERE term LIKE ''%s %%'' ORDER BY frequency DESC',
    [Prefix]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   -> %s (freq: %d)',
        [DS.FieldByName('term').AsString,
         DS.FieldByName('frequency').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  // Partial second word
  Prefix := 'laptop c';
  WriteLn(Format('   Partial: "%s"', [Prefix]));
  DS := Conn.ExecuteQuery(Format(
    'SELECT term, frequency FROM vocabulary WHERE term LIKE ''%s%%'' ORDER BY frequency DESC',
    [Prefix]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   -> %s (freq: %d)',
        [DS.FieldByName('term').AsString,
         DS.FieldByName('frequency').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Aggregates search history to show most-searched queries with user counts, plus category distribution across all searches. }
procedure Demo8_PopularCompletions;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Popular Completions ===');
  WriteLn;

  WriteLn('   Most searched terms (from history):');
  DS := Conn.ExecuteQuery(
    'SELECT query_text, COUNT(*) as search_count, ' +
    'COUNT(DISTINCT user_id) as unique_users ' +
    'FROM search_history GROUP BY query_text ' +
    'ORDER BY search_count DESC LIMIT 10');
  try
    WriteLn(Format('   %-25s %-10s %s', ['Query', 'Searches', 'Users']));
    WriteLn('   ' + StringOfChar('-', 45));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-10d %d',
        [DS.FieldByName('query_text').AsString,
         DS.FieldByName('search_count').AsInteger,
         DS.FieldByName('unique_users').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Category distribution in searches:');
  DS := Conn.ExecuteQuery(
    'SELECT v.category, COUNT(*) as cnt ' +
    'FROM search_history sh JOIN vocabulary v ON sh.query_text = v.term ' +
    'GROUP BY v.category ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %d searches',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Lists trending terms for the current period ranked by trend score, and filters trending terms starting with "m". }
procedure Demo9_TrendingSuggestions;
var
  DS: TDataSet;
  Rank: Integer;
begin
  WriteLn('=== 9. Trending Suggestions ===');
  WriteLn;

  WriteLn('   Current trending terms (week 2024-01-W4):');
  WriteLn;

  Rank := 0;
  DS := Conn.ExecuteQuery(
    'SELECT term, search_count, ROUND(trend_score, 1) as score ' +
    'FROM trending WHERE period = ''2024-01-W4'' ' +
    'ORDER BY trend_score DESC');
  try
    WriteLn(Format('   %-5s %-25s %-10s %s', ['Rank', 'Term', 'Searches', 'Score']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      Inc(Rank);
      WriteLn(Format('   #%-4d %-25s %-10d %s',
        [Rank,
         DS.FieldByName('term').AsString,
         DS.FieldByName('search_count').AsInteger,
         DS.FieldByName('score').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Trending + prefix filter "m":');
  DS := Conn.ExecuteQuery(
    'SELECT t.term, t.trend_score, v.frequency ' +
    'FROM trending t JOIN vocabulary v ON t.term = v.term ' +
    'WHERE t.term LIKE ''m%'' ORDER BY t.trend_score DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   -> %-25s trend:%.1f  freq:%d',
        [DS.FieldByName('term').AsString,
         DS.FieldByName('trend_score').AsFloat,
         DS.FieldByName('frequency').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays vocabulary size, category count, history entries, unique users, total search volume, category breakdowns, and per-user activity. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM vocabulary');
  try
    WriteLn(Format('   Vocabulary size: %d terms', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(DISTINCT category) as cnt FROM vocabulary');
  try
    WriteLn(Format('   Categories: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM search_history');
  try
    WriteLn(Format('   Search history entries: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(DISTINCT user_id) as cnt FROM search_history');
  try
    WriteLn(Format('   Unique users: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT SUM(frequency) as total FROM vocabulary');
  try
    WriteLn(Format('   Total search volume: %d', [DS.FieldByName('total').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Category breakdown:');
  DS := Conn.ExecuteQuery(
    'SELECT category, COUNT(*) as terms, SUM(frequency) as total_freq, ' +
    'ROUND(AVG(frequency), 0) as avg_freq ' +
    'FROM vocabulary GROUP BY category ORDER BY total_freq DESC');
  try
    WriteLn(Format('   %-15s %-6s %-10s %s', ['Category', 'Terms', 'TotalFreq', 'AvgFreq']));
    WriteLn('   ' + StringOfChar('-', 45));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %-6d %-10d %s',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('terms').AsInteger,
         DS.FieldByName('total_freq').AsInteger,
         DS.FieldByName('avg_freq').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   User activity:');
  DS := Conn.ExecuteQuery(
    'SELECT user_id, COUNT(*) as searches, ' +
    'COUNT(DISTINCT query_text) as unique_queries ' +
    'FROM search_history GROUP BY user_id ORDER BY searches DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %d searches, %d unique queries',
        [DS.FieldByName('user_id').AsString,
         DS.FieldByName('searches').AsInteger,
         DS.FieldByName('unique_queries').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// ============================================================
// Main
// ============================================================
begin
  WriteLn('Example 127: Autocomplete - Prefix Matching, Frequency, Personalization');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertVocabulary;
    InsertSearchHistory;

    Demo1_VocabularyIndex;
    Demo2_PrefixMatching;
    Demo3_FrequencyRanking;
    Demo4_RecentSearches;
    Demo5_Personalization;
    Demo6_TypeaheadSimulation;
    Demo7_MultiWordSuggestions;
    Demo8_PopularCompletions;
    Demo9_TrendingSuggestions;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
