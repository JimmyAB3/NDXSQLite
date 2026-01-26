{===============================================================================
  NDXSQLite Example 24 - Collations and Unicode
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Built-in collations (BINARY, NOCASE, RTRIM)
  - Case-sensitive vs case-insensitive comparisons
  - Unicode string handling
  - Sorting with collations

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CollationsUnicode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Compares BINARY, NOCASE, and RTRIM collations by querying the same text with different case and trailing spaces. }
procedure DemoBuiltInCollations;
var
  DS: TDataSet;
begin
  WriteLn('1. Built-in Collations');
  WriteLn('   -------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS collation_test (' +
    '  id INTEGER PRIMARY KEY,' +
    '  text_binary TEXT COLLATE BINARY,' +
    '  text_nocase TEXT COLLATE NOCASE,' +
    '  text_rtrim TEXT COLLATE RTRIM' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO collation_test VALUES (1, ''Apple'', ''Apple'', ''Apple  '')');
  Connection.ExecuteNonQuery('INSERT INTO collation_test VALUES (2, ''apple'', ''apple'', ''apple'')');
  Connection.ExecuteNonQuery('INSERT INTO collation_test VALUES (3, ''APPLE'', ''APPLE'', ''APPLE   '')');
  Connection.ExecuteNonQuery('INSERT INTO collation_test VALUES (4, ''Banana'', ''Banana'', ''Banana'')');

  WriteLn('   BINARY: Case-sensitive, exact comparison');
  WriteLn('   NOCASE: Case-insensitive for ASCII');
  WriteLn('   RTRIM: Ignores trailing spaces');
  WriteLn('');

  // BINARY comparison
  DS := Connection.ExecuteQuery('SELECT text_binary FROM collation_test WHERE text_binary = ''apple'' ORDER BY text_binary');
  try
    Write('   BINARY = ''apple'': ');
    while not DS.EOF do
    begin
      Write(DS.FieldByName('text_binary').AsString, ' ');
      DS.Next;
    end;
    WriteLn('(exact match only)');
  finally
    DS.Free;
  end;

  // NOCASE comparison
  DS := Connection.ExecuteQuery('SELECT text_nocase FROM collation_test WHERE text_nocase = ''apple'' ORDER BY text_nocase');
  try
    Write('   NOCASE = ''apple'': ');
    while not DS.EOF do
    begin
      Write(DS.FieldByName('text_nocase').AsString, ' ');
      DS.Next;
    end;
    WriteLn('(case-insensitive)');
  finally
    DS.Free;
  end;

  // RTRIM comparison
  DS := Connection.ExecuteQuery('SELECT text_rtrim FROM collation_test WHERE text_rtrim = ''Apple'' ORDER BY text_rtrim');
  try
    Write('   RTRIM = ''Apple'': ');
    while not DS.EOF do
    begin
      Write('[', DS.FieldByName('text_rtrim').AsString, '] ');
      DS.Next;
    end;
    WriteLn('(trailing spaces ignored)');
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Sorts mixed-case fruit names using both BINARY and NOCASE collations to contrast ordering behavior. }
procedure DemoSortingWithCollations;
var
  DS: TDataSet;
begin
  WriteLn('2. Sorting with Collations');
  WriteLn('   -----------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS fruits (' +
    '  name TEXT' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO fruits VALUES (''apple'')');
  Connection.ExecuteNonQuery('INSERT INTO fruits VALUES (''Banana'')');
  Connection.ExecuteNonQuery('INSERT INTO fruits VALUES (''CHERRY'')');
  Connection.ExecuteNonQuery('INSERT INTO fruits VALUES (''date'')');
  Connection.ExecuteNonQuery('INSERT INTO fruits VALUES (''Elderberry'')');

  // Default (BINARY) sorting
  DS := Connection.ExecuteQuery('SELECT name FROM fruits ORDER BY name');
  try
    Write('   BINARY sort: ');
    while not DS.EOF do
    begin
      Write(DS.FieldByName('name').AsString);
      DS.Next;
      if not DS.EOF then Write(', ');
    end;
    WriteLn(' (uppercase first)');
  finally
    DS.Free;
  end;

  // NOCASE sorting
  DS := Connection.ExecuteQuery('SELECT name FROM fruits ORDER BY name COLLATE NOCASE');
  try
    Write('   NOCASE sort: ');
    while not DS.EOF do
    begin
      Write(DS.FieldByName('name').AsString);
      DS.Next;
      if not DS.EOF then Write(', ');
    end;
    WriteLn(' (alphabetical)');
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Inserts and retrieves strings in French, German, Spanish, Japanese, Chinese, Russian, Arabic, and emoji. }
procedure DemoUnicodeStrings;
var
  DS: TDataSet;
begin
  WriteLn('3. Unicode String Handling');
  WriteLn('   -----------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS unicode_test (' +
    '  id INTEGER PRIMARY KEY,' +
    '  description TEXT,' +
    '  content TEXT' +
    ')');

  // Various Unicode strings
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''French'', ''Café résumé naïve'')');
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''German'', ''Größe Übung Äpfel'')');
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''Spanish'', ''Niño año mañana'')');
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''Japanese'', ''こんにちは世界'')');
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''Chinese'', ''你好世界'')');
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''Russian'', ''Привет мир'')');
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''Arabic'', ''مرحبا بالعالم'')');
  Connection.ExecuteNonQuery('INSERT INTO unicode_test (description, content) VALUES (''Emoji'', ''Hello 👋🌍🎉'')');

  DS := Connection.ExecuteQuery('SELECT description, content, LENGTH(content) as len FROM unicode_test');
  try
    WriteLn('   Unicode content:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %s (length: %d chars)',
        [DS.FieldByName('description').AsString,
         DS.FieldByName('content').AsString,
         DS.FieldByName('len').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Applies UPPER and LOWER to French and German text, showing that SQLite only converts ASCII characters. }
procedure DemoCaseConversion;
var
  DS: TDataSet;
begin
  WriteLn('4. Case Conversion (ASCII only)');
  WriteLn('   ----------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT content, ' +
    '  UPPER(content) as upper_text, ' +
    '  LOWER(content) as lower_text ' +
    'FROM unicode_test WHERE description IN (''French'', ''German'')');
  try
    WriteLn('   Note: SQLite UPPER/LOWER only works for ASCII');
    while not DS.EOF do
    begin
      WriteLn(Format('   Original: %s', [DS.FieldByName('content').AsString]));
      WriteLn(Format('   UPPER:    %s', [DS.FieldByName('upper_text').AsString]));
      WriteLn(Format('   LOWER:    %s', [DS.FieldByName('lower_text').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Searches for Unicode substrings using LIKE with accented characters and CJK ideographs. }
procedure DemoLikeWithUnicode;
var
  DS: TDataSet;
begin
  WriteLn('5. LIKE with Unicode');
  WriteLn('   -----------------');

  // LIKE is case-insensitive for ASCII only
  DS := Connection.ExecuteQuery('SELECT description, content FROM unicode_test WHERE content LIKE ''%é%''');
  try
    WriteLn('   LIKE ''%é%'' (accented e):');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('description').AsString, ': ', DS.FieldByName('content').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Connection.ExecuteQuery('SELECT description, content FROM unicode_test WHERE content LIKE ''%世界%''');
  try
    WriteLn('   LIKE ''%世界%'' (Chinese "world"):');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('description').AsString, ': ', DS.FieldByName('content').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates a NOCASE UNIQUE username column and verifies that duplicate names with different casing are rejected. }
procedure DemoIndexWithCollation;
var
  DS: TDataSet;
begin
  WriteLn('6. Index with Collation');
  WriteLn('   --------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS users_ci (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT COLLATE NOCASE UNIQUE,' +
    '  email TEXT COLLATE NOCASE' +
    ')');

  WriteLn('   Created table with NOCASE columns');

  Connection.ExecuteNonQuery('INSERT INTO users_ci (username, email) VALUES (''JohnDoe'', ''john@example.com'')');

  // Try to insert same username with different case
  try
    Connection.ExecuteNonQuery('INSERT INTO users_ci (username, email) VALUES (''johndoe'', ''other@example.com'')');
    WriteLn('   ERROR: Should have failed!');
  except
    on E: Exception do
      WriteLn('   Correctly rejected: ''johndoe'' conflicts with ''JohnDoe'' (case-insensitive)');
  end;

  // Case-insensitive lookup
  DS := Connection.ExecuteQuery('SELECT * FROM users_ci WHERE username = ''JOHNDOE''');
  try
    WriteLn('   Lookup ''JOHNDOE'' found: ', DS.FieldByName('username').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Evaluates direct string equality using BINARY, NOCASE, and RTRIM collations, including a non-ASCII pair. }
procedure DemoCompareStrings;
var
  V: Variant;
begin
  WriteLn('7. String Comparisons');
  WriteLn('   ------------------');

  // Direct comparisons
  V := Connection.ExecuteScalar('SELECT ''abc'' = ''ABC''');
  WriteLn('   ''abc'' = ''ABC'' (BINARY): ', VarToStr(V), ' (0=false, 1=true)');

  V := Connection.ExecuteScalar('SELECT ''abc'' = ''ABC'' COLLATE NOCASE');
  WriteLn('   ''abc'' = ''ABC'' COLLATE NOCASE: ', VarToStr(V));

  V := Connection.ExecuteScalar('SELECT ''abc '' = ''abc'' COLLATE RTRIM');
  WriteLn('   ''abc '' = ''abc'' COLLATE RTRIM: ', VarToStr(V));

  V := Connection.ExecuteScalar('SELECT ''Ä'' = ''ä''');
  WriteLn('   ''Ä'' = ''ä'' (BINARY): ', VarToStr(V), ' (no Unicode case folding)');

  WriteLn('');
end;

{ Outputs guidelines for collation selection, UTF-8 storage, and Unicode handling limitations. }
procedure DemoBestPractices;
begin
  WriteLn('8. Best Practices');
  WriteLn('   --------------');
  WriteLn('   - Use COLLATE NOCASE for case-insensitive text (ASCII)');
  WriteLn('   - Define collation in column definition for indexes');
  WriteLn('   - SQLite stores all text as UTF-8');
  WriteLn('   - LENGTH() returns character count, not bytes');
  WriteLn('   - LIKE is case-insensitive for ASCII only');
  WriteLn('   - For full Unicode case folding, use application-level logic');
  WriteLn('   - Consider ICU extension for full Unicode support');
  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 24: Collations and Unicode ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example24.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      DemoBuiltInCollations;
      DemoSortingWithCollations;
      DemoUnicodeStrings;
      DemoCaseConversion;
      DemoLikeWithUnicode;
      DemoIndexWithCollation;
      DemoCompareStrings;
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
