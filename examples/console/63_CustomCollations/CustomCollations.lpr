{===============================================================================
  NDXSQLite Example 63 - Custom Collations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Creating custom text comparison collations
  - Case-insensitive sorting
  - Natural sorting (file1, file2, file10 instead of file1, file10, file2)
  - Reverse sorting
  - Numeric string sorting
  - Using collations in ORDER BY and indexes

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CustomCollations;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqlitecollation;

type
  { Helper class for collation callbacks }
  TMyCollations = class
  public
    // Case-insensitive comparison
    function CompareCaseInsensitive(const A, B: string): Integer;

    // Reverse alphabetical order
    function CompareReverse(const A, B: string): Integer;

    // Natural sort (handles numbers in strings)
    function CompareNatural(const A, B: string): Integer;

    // Numeric comparison (treats strings as numbers)
    function CompareNumeric(const A, B: string): Integer;

    // Length-based comparison (shorter strings first)
    function CompareByLength(const A, B: string): Integer;
  end;

{ TMyCollations }

function TMyCollations.CompareCaseInsensitive(const A, B: string): Integer;
begin
  Result := CompareText(A, B);
end;

{ Compares strings in reverse alphabetical order. }
function TMyCollations.CompareReverse(const A, B: string): Integer;
begin
  // Reverse of normal comparison
  Result := CompareStr(B, A);
end;

{ Compares strings using natural sort order with numeric awareness. }
function TMyCollations.CompareNatural(const A, B: string): Integer;
var
  I, J: Integer;
  NumA, NumB: Int64;
  StartA, StartB: Integer;
begin
  I := 1;
  J := 1;

  while (I <= Length(A)) and (J <= Length(B)) do
  begin
    // Both are digits - compare numerically
    if (A[I] >= '0') and (A[I] <= '9') and
       (B[J] >= '0') and (B[J] <= '9') then
    begin
      // Extract number from A
      NumA := 0;
      StartA := I;
      while (I <= Length(A)) and (A[I] >= '0') and (A[I] <= '9') do
        Inc(I);
      if I > StartA then
        NumA := StrToInt64(Copy(A, StartA, I - StartA));

      // Extract number from B
      NumB := 0;
      StartB := J;
      while (J <= Length(B)) and (B[J] >= '0') and (B[J] <= '9') do
        Inc(J);
      if J > StartB then
        NumB := StrToInt64(Copy(B, StartB, J - StartB));

      if NumA < NumB then
      begin
        Result := -1;
        Exit;
      end
      else if NumA > NumB then
      begin
        Result := 1;
        Exit;
      end;
      // Continue if equal
    end
    else
    begin
      // Compare characters case-insensitively
      Result := Ord(UpCase(A[I])) - Ord(UpCase(B[J]));
      if Result <> 0 then
        Exit;
      Inc(I);
      Inc(J);
    end;
  end;

  // Shorter string comes first
  Result := (Length(A) - I + 1) - (Length(B) - J + 1);
end;

{ Compares strings as numeric values, with non-numbers sorted after. }
function TMyCollations.CompareNumeric(const A, B: string): Integer;
var
  NumA, NumB: Double;
  CodeA, CodeB: Integer;
begin
  Val(A, NumA, CodeA);
  Val(B, NumB, CodeB);

  // Both are valid numbers
  if (CodeA = 0) and (CodeB = 0) then
  begin
    if NumA < NumB then
      Result := -1
    else if NumA > NumB then
      Result := 1
    else
      Result := 0;
  end
  // A is number, B is not - numbers first
  else if CodeA = 0 then
    Result := -1
  // B is number, A is not
  else if CodeB = 0 then
    Result := 1
  // Neither is a number - compare as strings
  else
    Result := CompareStr(A, B);
end;

{ Compares strings by length first, then alphabetically if equal. }
function TMyCollations.CompareByLength(const A, B: string): Integer;
begin
  Result := Length(A) - Length(B);
  // If same length, use alphabetical order
  if Result = 0 then
    Result := CompareStr(A, B);
end;

{ Executes a SQL query and prints all first-column values as a comma-separated list with the given label. }
procedure ShowQueryResults(Conn: INDXSQLiteConnection; const SQL, Label_: string);
var
  DS: TDataSet;
  Values: string;
begin
  DS := Conn.ExecuteQuery(SQL);
  try
    Values := '';
    while not DS.EOF do
    begin
      if Values <> '' then
        Values := Values + ', ';
      Values := Values + DS.Fields[0].AsString;
      DS.Next;
    end;
    WriteLn('   ', Label_, ': ', Values);
  finally
    DS.Free;
  end;
end;

var
  Conn: INDXSQLiteConnection;
  Coll: TNDXSQLiteCollation;
  MyColls: TMyCollations;
  DBPath: string;
  CollList: TStringList;
  I: Integer;

begin
  WriteLn('=== NDXSQLite Example 63: Custom Collations ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example63.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Conn := TNDXSQLiteConnection.Create(DBPath);
  Conn.Open;

  MyColls := TMyCollations.Create;
  Coll := TNDXSQLiteCollation.Create(Conn);
  try
    // Register all collations
    Coll.RegisterCollation('NOCASE_CUSTOM', @MyColls.CompareCaseInsensitive);
    Coll.RegisterCollation('REVERSE_ORDER', @MyColls.CompareReverse);
    Coll.RegisterCollation('NATURAL_SORT', @MyColls.CompareNatural);
    Coll.RegisterCollation('NUMERIC_SORT', @MyColls.CompareNumeric);
    Coll.RegisterCollation('LENGTH_SORT', @MyColls.CompareByLength);

    // 1. Case-insensitive collation
    WriteLn('1. Case-Insensitive Collation:');
    Conn.ExecuteNonQuery('CREATE TABLE names (name TEXT)');
    Conn.ExecuteNonQuery('INSERT INTO names VALUES (''Zebra''), (''apple''), (''BANANA''), (''cherry'')');

    ShowQueryResults(Conn, 'SELECT name FROM names ORDER BY name', 'Default (case-sensitive)');
    ShowQueryResults(Conn, 'SELECT name FROM names ORDER BY name COLLATE NOCASE_CUSTOM', 'Custom NOCASE');
    WriteLn;

    // 2. Reverse order collation
    WriteLn('2. Reverse Order Collation:');
    Conn.ExecuteNonQuery('CREATE TABLE letters (letter TEXT)');
    Conn.ExecuteNonQuery('INSERT INTO letters VALUES (''A''), (''B''), (''C''), (''D''), (''E'')');

    ShowQueryResults(Conn, 'SELECT letter FROM letters ORDER BY letter', 'Normal order');
    ShowQueryResults(Conn, 'SELECT letter FROM letters ORDER BY letter COLLATE REVERSE_ORDER', 'Reverse order');
    WriteLn;

    // 3. Natural sort collation
    WriteLn('3. Natural Sort Collation (files with numbers):');
    Conn.ExecuteNonQuery('CREATE TABLE files (filename TEXT)');
    Conn.ExecuteNonQuery('INSERT INTO files VALUES (''file1.txt''), (''file10.txt''), (''file2.txt''), (''file20.txt''), (''file3.txt'')');

    ShowQueryResults(Conn, 'SELECT filename FROM files ORDER BY filename', 'Default (lexicographic)');
    ShowQueryResults(Conn, 'SELECT filename FROM files ORDER BY filename COLLATE NATURAL_SORT', 'Natural sort');
    WriteLn;

    // 4. Numeric sort collation
    WriteLn('4. Numeric Sort Collation:');
    Conn.ExecuteNonQuery('CREATE TABLE values_table (val TEXT)');
    Conn.ExecuteNonQuery('INSERT INTO values_table VALUES (''100''), (''20''), (''3''), (''abc''), (''1000''), (''50'')');

    ShowQueryResults(Conn, 'SELECT val FROM values_table ORDER BY val', 'Default (string)');
    ShowQueryResults(Conn, 'SELECT val FROM values_table ORDER BY val COLLATE NUMERIC_SORT', 'Numeric sort');
    WriteLn;

    // 5. Length-based sort collation
    WriteLn('5. Length-Based Sort Collation:');
    Conn.ExecuteNonQuery('CREATE TABLE words (word TEXT)');
    Conn.ExecuteNonQuery('INSERT INTO words VALUES (''cat''), (''elephant''), (''dog''), (''a''), (''butterfly'')');

    ShowQueryResults(Conn, 'SELECT word FROM words ORDER BY word', 'Default (alphabetical)');
    ShowQueryResults(Conn, 'SELECT word FROM words ORDER BY word COLLATE LENGTH_SORT', 'By length');
    WriteLn;

    // 6. Using collation in table definition
    WriteLn('6. Collation in Column Definition:');
    Conn.ExecuteNonQuery(
      'CREATE TABLE products (' +
      '  id INTEGER PRIMARY KEY,' +
      '  name TEXT COLLATE NOCASE_CUSTOM' +
      ')');
    Conn.ExecuteNonQuery('INSERT INTO products (name) VALUES (''Apple''), (''APPLE''), (''apple'')');

    ShowQueryResults(Conn, 'SELECT DISTINCT name FROM products', 'Distinct with NOCASE column');
    WriteLn('   (Case variations treated as same)');
    WriteLn;

    // 7. Collation with indexes
    WriteLn('7. Collation with Indexes:');
    Conn.ExecuteNonQuery(
      'CREATE INDEX idx_files_natural ON files(filename COLLATE NATURAL_SORT)');
    WriteLn('   Created index with NATURAL_SORT collation');
    WriteLn('   Queries using this collation can utilize the index');
    WriteLn;

    // 8. List registered collations
    WriteLn('8. Registered Collations:');
    CollList := Coll.GetRegisteredCollations;
    try
      for I := 0 to CollList.Count - 1 do
        WriteLn('   - ', CollList[I]);
    finally
      CollList.Free;
    end;
    WriteLn;

    // 9. Built-in helper collations
    WriteLn('9. Built-in Static Collation Helpers:');
    WriteLn('   TNDXSQLiteCollation provides static helper methods:');
    WriteLn('   - CompareCaseInsensitive: Case-insensitive comparison');
    WriteLn('   - CompareNatural: Natural number sorting');
    WriteLn('   - CompareReverse: Reverse order');
    WriteLn('   - CompareNumeric: Numeric string comparison');
    WriteLn;

    // 10. Best practices
    WriteLn('10. Collation Best Practices:');
    WriteLn('    - Register collations before creating tables that use them');
    WriteLn('    - Use collations consistently in queries and indexes');
    WriteLn('    - Consider performance: collations are called for each comparison');
    WriteLn('    - For Unicode, consider using ICU collation (if available)');
    WriteLn('    - Collations must be re-registered after reconnection');
    WriteLn;

  finally
    Coll.Free;
    MyColls.Free;
  end;

  Conn.Close;
  Conn := nil;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
