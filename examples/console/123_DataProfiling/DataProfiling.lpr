{===============================================================================
  NDXSQLite Example 123 - Data Profiling
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Column statistics and data type inference
  - Null analysis and cardinality metrics
  - Pattern detection in string data
  - Histogram generation for numeric columns
  - Data quality assessment

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataProfiling;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, Math, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Sample data table to profile
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS customers (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  first_name TEXT,' +
    '  last_name TEXT,' +
    '  email TEXT,' +
    '  phone TEXT,' +
    '  age INTEGER,' +
    '  salary REAL,' +
    '  city TEXT,' +
    '  country TEXT,' +
    '  registration_date TEXT,' +
    '  status TEXT,' +
    '  referral_code TEXT' +
    ')'
  );

  // Profile results storage
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS profile_results (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  table_name TEXT NOT NULL,' +
    '  column_name TEXT NOT NULL,' +
    '  total_rows INTEGER,' +
    '  null_count INTEGER,' +
    '  null_ratio REAL,' +
    '  distinct_count INTEGER,' +
    '  cardinality REAL,' +  // distinct/total ratio
    '  min_value TEXT,' +
    '  max_value TEXT,' +
    '  avg_value REAL,' +
    '  stddev_value REAL,' +
    '  min_length INTEGER,' +
    '  max_length INTEGER,' +
    '  avg_length REAL,' +
    '  inferred_type TEXT,' +  // integer, real, date, email, phone, text
    '  detected_pattern TEXT,' +
    '  profiled_at TEXT DEFAULT (datetime(''now''))' +
    ')'
  );

  // Distribution histogram
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS profile_histogram (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  table_name TEXT NOT NULL,' +
    '  column_name TEXT NOT NULL,' +
    '  bucket_value TEXT NOT NULL,' +
    '  frequency INTEGER NOT NULL,' +
    '  percentage REAL' +
    ')'
  );

  // Pattern detection results
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS profile_patterns (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  table_name TEXT NOT NULL,' +
    '  column_name TEXT NOT NULL,' +
    '  pattern TEXT NOT NULL,' +
    '  example_value TEXT,' +
    '  match_count INTEGER,' +
    '  match_percentage REAL' +
    ')'
  );
end;

{ Inserts sample data for demonstrations. }
procedure InsertSampleData;
begin
  // Mix of good and problematic data for profiling
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Alice'', ''Smith'', ''alice.smith@email.com'', ''+1-555-0101'', 32, 75000.00, ''New York'', ''US'', ''2024-01-15'', ''active'', ''REF-001'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Bob'', ''Johnson'', ''bob.j@company.org'', ''+1-555-0102'', 45, 92000.50, ''Chicago'', ''US'', ''2024-02-20'', ''active'', ''REF-002'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Carol'', ''Williams'', ''carol@web.net'', ''+44-20-7946-0958'', 28, 45000.00, ''London'', ''UK'', ''2024-03-10'', ''active'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Dave'', ''Brown'', NULL, ''+1-555-0104'', 55, 120000.00, ''San Francisco'', ''US'', ''2024-01-05'', ''inactive'', ''REF-003'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Eve'', ''Davis'', ''eve.davis@email.com'', NULL, 38, 68000.00, ''Boston'', ''US'', ''2024-04-12'', ''active'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Frank'', ''Miller'', ''frank.m@email.com'', ''+49-30-1234567'', NULL, 85000.00, ''Berlin'', ''DE'', ''2024-05-01'', ''active'', ''REF-004'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Grace'', ''Wilson'', ''grace@email.com'', ''+1-555-0107'', 29, NULL, ''Seattle'', ''US'', ''2024-03-22'', ''pending'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Henry'', ''Moore'', ''h.moore@bigcorp.com'', ''+1-555-0108'', 41, 95000.00, ''New York'', ''US'', ''2024-06-15'', ''active'', ''REF-005'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Iris'', ''Taylor'', ''iris.t@email.com'', ''+33-1-23456789'', 35, 72000.00, ''Paris'', ''FR'', ''2024-07-01'', ''active'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Jack'', ''Anderson'', ''jack@email.com'', ''+1-555-0110'', 50, 110000.00, ''Chicago'', ''US'', ''2024-02-28'', ''active'', ''REF-006'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Kate'', ''Thomas'', NULL, ''+1-555-0111'', 27, 52000.00, ''Boston'', ''US'', ''2024-08-10'', ''active'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Leo'', ''Jackson'', ''leo.jackson@email.com'', NULL, 33, 78000.00, ''London'', ''UK'', ''2024-04-05'', ''inactive'', ''REF-007'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(NULL, ''White'', ''unknown@temp.com'', ''+1-555-0113'', 22, 35000.00, ''Miami'', ''US'', ''2024-09-01'', ''pending'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Nancy'', NULL, ''nancy@email.com'', ''+1-555-0114'', 44, 88000.00, ''Denver'', ''US'', ''2024-05-20'', ''active'', ''REF-008'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Oscar'', ''Harris'', ''oscar.h@email.com'', ''+81-3-1234-5678'', 39, 98000.00, ''Tokyo'', ''JP'', ''2024-10-15'', ''active'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Pam'', ''Martin'', ''pam123@email.com'', ''+1-555-0116'', 31, 62000.00, ''San Francisco'', ''US'', ''2024-06-30'', ''active'', ''REF-009'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Quinn'', ''Garcia'', ''quinn.g@email.com'', ''+1-555-0117'', NULL, 71000.00, ''Seattle'', ''US'', ''2024-11-01'', ''active'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Rita'', ''Martinez'', ''rita.m@company.org'', ''+34-91-1234567'', 48, 105000.00, ''Madrid'', ''ES'', ''2024-07-22'', ''active'', ''REF-010'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Sam'', ''Robinson'', ''sam@email.com'', ''+1-555-0119'', 36, 82000.00, ''New York'', ''US'', ''2024-08-15'', ''inactive'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO customers (first_name, last_name, email, phone, age, salary, city, country, registration_date, status, referral_code) VALUES ' +
    '(''Tina'', ''Clark'', ''tina.clark@email.com'', ''+1-555-0120'', 26, 48000.00, ''Chicago'', ''US'', ''2024-12-01'', ''active'', ''REF-011'')');
end;

// ============================================================
// Profiling Functions
// ============================================================

{ Computes and stores statistics for a single column including null count, distinct count, cardinality, min/max values, average, standard deviation, and string lengths. }
procedure ProfileColumn(const TableName, ColumnName: string);
var
  DS: TDataSet;
  TotalRows, NullCount, DistinctCount: Integer;
  NullRatio, Cardinality: Double;
  MinVal, MaxVal: string;
  AvgVal, StdDevVal: Double;
  MinLen, MaxLen: Integer;
  AvgLen: Double;
  HasAvg: Boolean;
begin
  // Total rows
  DS := Conn.ExecuteQuery(Format('SELECT COUNT(*) as cnt FROM %s', [TableName]));
  try
    TotalRows := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Null count
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NULL', [TableName, ColumnName]));
  try
    NullCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  NullRatio := NullCount / TotalRows;

  // Distinct count
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(DISTINCT %s) as cnt FROM %s WHERE %s IS NOT NULL',
    [ColumnName, TableName, ColumnName]));
  try
    DistinctCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  if (TotalRows - NullCount) > 0 then
    Cardinality := DistinctCount / (TotalRows - NullCount)
  else
    Cardinality := 0;

  // Min/Max
  DS := Conn.ExecuteQuery(Format(
    'SELECT MIN(%s) as min_val, MAX(%s) as max_val FROM %s WHERE %s IS NOT NULL',
    [ColumnName, ColumnName, TableName, ColumnName]));
  try
    MinVal := VarToStr(DS.FieldByName('min_val').Value);
    MaxVal := VarToStr(DS.FieldByName('max_val').Value);
  finally
    DS.Free;
  end;

  // Avg and StdDev (only for numeric columns)
  AvgVal := 0;
  StdDevVal := 0;
  HasAvg := False;
  DS := Conn.ExecuteQuery(Format(
    'SELECT AVG(CAST(%s AS REAL)) as avg_val FROM %s WHERE %s IS NOT NULL ' +
    'AND typeof(%s) IN (''integer'', ''real'')',
    [ColumnName, TableName, ColumnName, ColumnName]));
  try
    if not DS.FieldByName('avg_val').IsNull then
    begin
      AvgVal := DS.FieldByName('avg_val').AsFloat;
      HasAvg := True;
    end;
  finally
    DS.Free;
  end;

  if HasAvg then
  begin
    // Calculate standard deviation manually using SQL
    DS := Conn.ExecuteQuery(Format(
      'SELECT AVG((%s - %.4f) * (%s - %.4f)) as variance FROM %s ' +
      'WHERE %s IS NOT NULL AND typeof(%s) IN (''integer'', ''real'')',
      [ColumnName, AvgVal, ColumnName, AvgVal, TableName, ColumnName, ColumnName]));
    try
      if not DS.FieldByName('variance').IsNull then
        StdDevVal := Sqrt(DS.FieldByName('variance').AsFloat);
    finally
      DS.Free;
    end;
  end;

  // String lengths
  MinLen := 0;
  MaxLen := 0;
  AvgLen := 0;
  DS := Conn.ExecuteQuery(Format(
    'SELECT MIN(LENGTH(%s)) as min_len, MAX(LENGTH(%s)) as max_len, ' +
    'AVG(LENGTH(%s)) as avg_len FROM %s WHERE %s IS NOT NULL',
    [ColumnName, ColumnName, ColumnName, TableName, ColumnName]));
  try
    if not DS.FieldByName('min_len').IsNull then
    begin
      MinLen := DS.FieldByName('min_len').AsInteger;
      MaxLen := DS.FieldByName('max_len').AsInteger;
      AvgLen := DS.FieldByName('avg_len').AsFloat;
    end;
  finally
    DS.Free;
  end;

  // Store results
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO profile_results (table_name, column_name, total_rows, null_count, ' +
    'null_ratio, distinct_count, cardinality, min_value, max_value, avg_value, ' +
    'stddev_value, min_length, max_length, avg_length) VALUES ' +
    '(''%s'', ''%s'', %d, %d, %.4f, %d, %.4f, ''%s'', ''%s'', %.2f, %.2f, %d, %d, %.1f)',
    [TableName, ColumnName, TotalRows, NullCount, NullRatio,
     DistinctCount, Cardinality, MinVal, MaxVal, AvgVal, StdDevVal,
     MinLen, MaxLen, AvgLen]));
end;

{ Analyzes column values against type patterns (integer, real, date, email, phone) and updates the profile with the inferred data type. }
procedure InferColumnType(const TableName, ColumnName: string);
var
  DS: TDataSet;
  IntCount, RealCount, DateCount, EmailCount, PhoneCount, TextCount: Integer;
  TotalNonNull: Integer;
  InferredType: string;
begin
  // Count values matching each type pattern
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL',
    [TableName, ColumnName]));
  try
    TotalNonNull := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  if TotalNonNull = 0 then
  begin
    Conn.ExecuteNonQuery(Format(
      'UPDATE profile_results SET inferred_type = ''unknown'' ' +
      'WHERE table_name = ''%s'' AND column_name = ''%s''',
      [TableName, ColumnName]));
    Exit;
  end;

  // Integer check
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL ' +
    'AND typeof(%s) = ''integer''',
    [TableName, ColumnName, ColumnName]));
  try
    IntCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Real check
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL ' +
    'AND typeof(%s) = ''real''',
    [TableName, ColumnName, ColumnName]));
  try
    RealCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Date pattern (yyyy-mm-dd)
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL ' +
    'AND %s GLOB ''[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]''',
    [TableName, ColumnName, ColumnName]));
  try
    DateCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Email pattern
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL ' +
    'AND %s LIKE ''%%@%%._%%''',
    [TableName, ColumnName, ColumnName]));
  try
    EmailCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Phone pattern (starts with +)
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL ' +
    'AND %s LIKE ''+%%''',
    [TableName, ColumnName, ColumnName]));
  try
    PhoneCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  TextCount := TotalNonNull - IntCount - RealCount;

  // Determine type based on majority
  if IntCount = TotalNonNull then
    InferredType := 'integer'
  else if RealCount = TotalNonNull then
    InferredType := 'real'
  else if (IntCount + RealCount) = TotalNonNull then
    InferredType := 'numeric'
  else if DateCount = TotalNonNull then
    InferredType := 'date'
  else if EmailCount = TotalNonNull then
    InferredType := 'email'
  else if (EmailCount > TotalNonNull div 2) then
    InferredType := 'email (mixed)'
  else if PhoneCount = TotalNonNull then
    InferredType := 'phone'
  else if (PhoneCount > TotalNonNull div 2) then
    InferredType := 'phone (mixed)'
  else
    InferredType := 'text';

  Conn.ExecuteNonQuery(Format(
    'UPDATE profile_results SET inferred_type = ''%s'' ' +
    'WHERE table_name = ''%s'' AND column_name = ''%s''',
    [InferredType, TableName, ColumnName]));
end;

{ Scans column values for known patterns (email, phone, date, referral code, ISO country code) and records match counts and percentages. }
procedure DetectPatterns(const TableName, ColumnName: string);
var
  DS: TDataSet;
  TotalNonNull: Integer;
  PatternCount: Integer;
begin
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL',
    [TableName, ColumnName]));
  try
    TotalNonNull := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  if TotalNonNull = 0 then Exit;

  // Check for email pattern
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt, MIN(%s) as example FROM %s ' +
    'WHERE %s IS NOT NULL AND %s LIKE ''%%@%%._%%''',
    [ColumnName, TableName, ColumnName, ColumnName]));
  try
    PatternCount := DS.FieldByName('cnt').AsInteger;
    if PatternCount > 0 then
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO profile_patterns (table_name, column_name, pattern, example_value, match_count, match_percentage) ' +
        'VALUES (''%s'', ''%s'', ''email (xxx@yyy.zzz)'', ''%s'', %d, %.1f)',
        [TableName, ColumnName, DS.FieldByName('example').AsString,
         PatternCount, PatternCount * 100.0 / TotalNonNull]));
  finally
    DS.Free;
  end;

  // Check for phone pattern (+XX-...)
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt, MIN(%s) as example FROM %s ' +
    'WHERE %s IS NOT NULL AND %s LIKE ''+%%''',
    [ColumnName, TableName, ColumnName, ColumnName]));
  try
    PatternCount := DS.FieldByName('cnt').AsInteger;
    if PatternCount > 0 then
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO profile_patterns (table_name, column_name, pattern, example_value, match_count, match_percentage) ' +
        'VALUES (''%s'', ''%s'', ''phone (+XX-...)'', ''%s'', %d, %.1f)',
        [TableName, ColumnName, DS.FieldByName('example').AsString,
         PatternCount, PatternCount * 100.0 / TotalNonNull]));
  finally
    DS.Free;
  end;

  // Check for date pattern (yyyy-mm-dd)
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt, MIN(%s) as example FROM %s ' +
    'WHERE %s IS NOT NULL AND %s GLOB ''[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]''',
    [ColumnName, TableName, ColumnName, ColumnName]));
  try
    PatternCount := DS.FieldByName('cnt').AsInteger;
    if PatternCount > 0 then
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO profile_patterns (table_name, column_name, pattern, example_value, match_count, match_percentage) ' +
        'VALUES (''%s'', ''%s'', ''date (yyyy-mm-dd)'', ''%s'', %d, %.1f)',
        [TableName, ColumnName, DS.FieldByName('example').AsString,
         PatternCount, PatternCount * 100.0 / TotalNonNull]));
  finally
    DS.Free;
  end;

  // Check for referral code pattern (REF-NNN)
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt, MIN(%s) as example FROM %s ' +
    'WHERE %s IS NOT NULL AND %s LIKE ''REF-%%''',
    [ColumnName, TableName, ColumnName, ColumnName]));
  try
    PatternCount := DS.FieldByName('cnt').AsInteger;
    if PatternCount > 0 then
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO profile_patterns (table_name, column_name, pattern, example_value, match_count, match_percentage) ' +
        'VALUES (''%s'', ''%s'', ''code (REF-NNN)'', ''%s'', %d, %.1f)',
        [TableName, ColumnName, DS.FieldByName('example').AsString,
         PatternCount, PatternCount * 100.0 / TotalNonNull]));
  finally
    DS.Free;
  end;

  // Check for uppercase-only
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s ' +
    'WHERE %s IS NOT NULL AND %s = UPPER(%s) AND LENGTH(%s) = 2',
    [TableName, ColumnName, ColumnName, ColumnName, ColumnName]));
  try
    PatternCount := DS.FieldByName('cnt').AsInteger;
    if PatternCount > 0 then
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO profile_patterns (table_name, column_name, pattern, example_value, match_count, match_percentage) ' +
        'VALUES (''%s'', ''%s'', ''iso_code (XX)'', ''US'', %d, %.1f)',
        [TableName, ColumnName, PatternCount, PatternCount * 100.0 / TotalNonNull]));
  finally
    DS.Free;
  end;
end;

{ Builds a frequency distribution of the top N distinct values in a column, storing each value's count and percentage in the histogram table. }
procedure GenerateHistogram(const TableName, ColumnName: string; MaxBuckets: Integer);
var
  DS: TDataSet;
  TotalNonNull: Integer;
begin
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM %s WHERE %s IS NOT NULL',
    [TableName, ColumnName]));
  try
    TotalNonNull := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  if TotalNonNull = 0 then Exit;

  // Generate frequency distribution (top N values)
  DS := Conn.ExecuteQuery(Format(
    'SELECT %s as val, COUNT(*) as freq FROM %s ' +
    'WHERE %s IS NOT NULL GROUP BY %s ORDER BY freq DESC, val LIMIT %d',
    [ColumnName, TableName, ColumnName, ColumnName, MaxBuckets]));
  try
    while not DS.EOF do
    begin
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO profile_histogram (table_name, column_name, bucket_value, frequency, percentage) ' +
        'VALUES (''%s'', ''%s'', ''%s'', %d, %.1f)',
        [TableName, ColumnName,
         DS.FieldByName('val').AsString,
         DS.FieldByName('freq').AsInteger,
         DS.FieldByName('freq').AsInteger * 100.0 / TotalNonNull]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// ============================================================
// Demo Sections
// ============================================================

{ Runs ProfileColumn on all 11 columns of the customers table to compute their statistics. }
procedure Demo1_BasicProfile;
var
  Columns: array[0..10] of string;
  I: Integer;
begin
  WriteLn('=== 1. Column-Level Profiling ===');
  WriteLn;

  Columns[0] := 'first_name';
  Columns[1] := 'last_name';
  Columns[2] := 'email';
  Columns[3] := 'phone';
  Columns[4] := 'age';
  Columns[5] := 'salary';
  Columns[6] := 'city';
  Columns[7] := 'country';
  Columns[8] := 'registration_date';
  Columns[9] := 'status';
  Columns[10] := 'referral_code';

  for I := 0 to 10 do
    ProfileColumn('customers', Columns[I]);

  WriteLn('   Profiled 11 columns from customers table (20 rows)');
  WriteLn;
end;

{ Displays the null count and null percentage for each column, sorted by highest null ratio first. }
procedure Demo2_NullAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Null Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT column_name, total_rows, null_count, ' +
    'ROUND(null_ratio * 100, 1) as null_pct ' +
    'FROM profile_results WHERE table_name = ''customers'' ' +
    'ORDER BY null_ratio DESC');
  try
    WriteLn(Format('   %-20s %-8s %-8s %s', ['Column', 'Total', 'Nulls', 'Null%']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-8d %-8d %s%%',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('total_rows').AsInteger,
         DS.FieldByName('null_count').AsInteger,
         DS.FieldByName('null_pct').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays the distinct value count and cardinality ratio (distinct/non-null) for each column, sorted by highest cardinality first. }
procedure Demo3_CardinalityAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Cardinality Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT column_name, distinct_count, ' +
    '(total_rows - null_count) as non_null, ' +
    'ROUND(cardinality * 100, 1) as card_pct ' +
    'FROM profile_results WHERE table_name = ''customers'' ' +
    'ORDER BY cardinality DESC');
  try
    WriteLn(Format('   %-20s %-10s %-10s %s', ['Column', 'Distinct', 'Non-Null', 'Cardinality%']));
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-10d %-10d %s%%',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('distinct_count').AsInteger,
         DS.FieldByName('non_null').AsInteger,
         DS.FieldByName('card_pct').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays min, max, average, and standard deviation for columns that have numeric values (age, salary). }
procedure Demo4_NumericStats;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Numeric Column Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT column_name, min_value, max_value, ' +
    'ROUND(avg_value, 2) as avg_val, ROUND(stddev_value, 2) as std_val ' +
    'FROM profile_results WHERE table_name = ''customers'' ' +
    'AND avg_value > 0 ORDER BY column_name');
  try
    WriteLn(Format('   %-20s %-12s %-12s %-12s %s', ['Column', 'Min', 'Max', 'Avg', 'StdDev']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-12s %-12s %-12s %s',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('min_value').AsString,
         DS.FieldByName('max_value').AsString,
         DS.FieldByName('avg_val').AsString,
         DS.FieldByName('std_val').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays the minimum, maximum, and average string lengths for all columns that have non-zero length values. }
procedure Demo5_StringLengths;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. String Length Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT column_name, min_length, max_length, ROUND(avg_length, 1) as avg_len ' +
    'FROM profile_results WHERE table_name = ''customers'' ' +
    'AND min_length > 0 ORDER BY avg_length DESC');
  try
    WriteLn(Format('   %-20s %-8s %-8s %s', ['Column', 'MinLen', 'MaxLen', 'AvgLen']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-8d %-8d %s',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('min_length').AsInteger,
         DS.FieldByName('max_length').AsInteger,
         DS.FieldByName('avg_len').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Runs type inference on all columns and displays the inferred data type for each (integer, real, date, email, phone, or text). }
procedure Demo6_TypeInference;
var
  Columns: array[0..10] of string;
  I: Integer;
  DS: TDataSet;
begin
  WriteLn('=== 6. Type Inference ===');
  WriteLn;

  Columns[0] := 'first_name';
  Columns[1] := 'last_name';
  Columns[2] := 'email';
  Columns[3] := 'phone';
  Columns[4] := 'age';
  Columns[5] := 'salary';
  Columns[6] := 'city';
  Columns[7] := 'country';
  Columns[8] := 'registration_date';
  Columns[9] := 'status';
  Columns[10] := 'referral_code';

  for I := 0 to 10 do
    InferColumnType('customers', Columns[I]);

  DS := Conn.ExecuteQuery(
    'SELECT column_name, inferred_type FROM profile_results ' +
    'WHERE table_name = ''customers'' ORDER BY id');
  try
    WriteLn(Format('   %-20s %s', ['Column', 'Inferred Type']));
    WriteLn('   ' + StringOfChar('-', 40));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %s',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('inferred_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Runs pattern detection on all columns and displays matched patterns with example values, counts, and match percentages. }
procedure Demo7_PatternDetection;
var
  Columns: array[0..10] of string;
  I: Integer;
  DS: TDataSet;
begin
  WriteLn('=== 7. Pattern Detection ===');
  WriteLn;

  Columns[0] := 'first_name';
  Columns[1] := 'last_name';
  Columns[2] := 'email';
  Columns[3] := 'phone';
  Columns[4] := 'age';
  Columns[5] := 'salary';
  Columns[6] := 'city';
  Columns[7] := 'country';
  Columns[8] := 'registration_date';
  Columns[9] := 'status';
  Columns[10] := 'referral_code';

  for I := 0 to 10 do
    DetectPatterns('customers', Columns[I]);

  DS := Conn.ExecuteQuery(
    'SELECT column_name, pattern, example_value, match_count, ' +
    'ROUND(match_percentage, 1) as match_pct ' +
    'FROM profile_patterns WHERE table_name = ''customers'' ' +
    'ORDER BY column_name, match_count DESC');
  try
    WriteLn(Format('   %-20s %-22s %-20s %-6s %s',
      ['Column', 'Pattern', 'Example', 'Count', 'Match%']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-22s %-20s %-6d %s%%',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('pattern').AsString,
         DS.FieldByName('example_value').AsString,
         DS.FieldByName('match_count').AsInteger,
         DS.FieldByName('match_pct').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Generates frequency histograms for city, country, and status columns, then displays them with visual bar charts and percentages. }
procedure Demo8_DistributionHistograms;
var
  DS: TDataSet;
  CurrentCol, PrevCol: string;
begin
  WriteLn('=== 8. Distribution Histograms ===');
  WriteLn;

  // Generate histograms for selected columns
  GenerateHistogram('customers', 'city', 10);
  GenerateHistogram('customers', 'country', 10);
  GenerateHistogram('customers', 'status', 5);

  PrevCol := '';
  DS := Conn.ExecuteQuery(
    'SELECT column_name, bucket_value, frequency, ' +
    'ROUND(percentage, 1) as pct ' +
    'FROM profile_histogram WHERE table_name = ''customers'' ' +
    'ORDER BY column_name, frequency DESC');
  try
    while not DS.EOF do
    begin
      CurrentCol := DS.FieldByName('column_name').AsString;
      if CurrentCol <> PrevCol then
      begin
        if PrevCol <> '' then WriteLn;
        WriteLn(Format('   Distribution: %s', [CurrentCol]));
        PrevCol := CurrentCol;
      end;

      WriteLn(Format('   %-20s %3d  %s %s%%',
        [DS.FieldByName('bucket_value').AsString,
         DS.FieldByName('frequency').AsInteger,
         StringOfChar('#', DS.FieldByName('frequency').AsInteger * 2),
         DS.FieldByName('pct').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn;
end;

{ Computes a completeness score from average null ratios, identifies columns with high null ratios or low cardinality, and flags them as quality issues. }
procedure Demo9_DataQualityScore;
var
  DS: TDataSet;
  TotalColumns, ColumnsWithNulls: Integer;
  AvgNullRatio, AvgCardinality: Double;
  Score: Double;
begin
  WriteLn('=== 9. Data Quality Score ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as total_cols, ' +
    'SUM(CASE WHEN null_count > 0 THEN 1 ELSE 0 END) as cols_with_nulls, ' +
    'AVG(null_ratio) as avg_null_ratio, ' +
    'AVG(cardinality) as avg_cardinality ' +
    'FROM profile_results WHERE table_name = ''customers''');
  try
    TotalColumns := DS.FieldByName('total_cols').AsInteger;
    ColumnsWithNulls := DS.FieldByName('cols_with_nulls').AsInteger;
    AvgNullRatio := DS.FieldByName('avg_null_ratio').AsFloat;
    AvgCardinality := DS.FieldByName('avg_cardinality').AsFloat;
  finally
    DS.Free;
  end;

  // Simple quality score: completeness * uniqueness
  Score := (1.0 - AvgNullRatio) * 100.0;

  WriteLn(Format('   Total columns profiled: %d', [TotalColumns]));
  WriteLn(Format('   Columns with nulls: %d / %d', [ColumnsWithNulls, TotalColumns]));
  WriteLn(Format('   Average null ratio: %.1f%%', [AvgNullRatio * 100]));
  WriteLn(Format('   Average cardinality: %.1f%%', [AvgCardinality * 100]));
  WriteLn(Format('   Completeness score: %.1f / 100', [Score]));

  WriteLn;
  WriteLn('   Quality issues:');
  DS := Conn.ExecuteQuery(
    'SELECT column_name, null_ratio, cardinality FROM profile_results ' +
    'WHERE table_name = ''customers'' AND (null_ratio > 0.1 OR cardinality < 0.2) ' +
    'ORDER BY null_ratio DESC');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('null_ratio').AsFloat > 0.1 then
        WriteLn(Format('   [WARN] %s: %.0f%% null values',
          [DS.FieldByName('column_name').AsString,
           DS.FieldByName('null_ratio').AsFloat * 100]))
      else
        WriteLn(Format('   [INFO] %s: low cardinality (%.0f%%)',
          [DS.FieldByName('column_name').AsString,
           DS.FieldByName('cardinality').AsFloat * 100]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays a consolidated profile report showing null count, distinct count, cardinality, min/max values, and inferred type for every column. }
procedure Demo10_ProfileSummary;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Full Profile Summary ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT column_name, null_count, distinct_count, ' +
    'ROUND(cardinality * 100, 0) as card_pct, ' +
    'min_value, max_value, inferred_type ' +
    'FROM profile_results WHERE table_name = ''customers'' ORDER BY id');
  try
    WriteLn(Format('   %-18s %-6s %-8s %-6s %-14s %-14s %s',
      ['Column', 'Nulls', 'Distinct', 'Card%', 'Min', 'Max', 'Type']));
    WriteLn('   ' + StringOfChar('-', 90));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-6d %-8d %-6s %-14s %-14s %s',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('null_count').AsInteger,
         DS.FieldByName('distinct_count').AsInteger,
         DS.FieldByName('card_pct').AsString,
         Copy(DS.FieldByName('min_value').AsString, 1, 13),
         Copy(DS.FieldByName('max_value').AsString, 1, 13),
         DS.FieldByName('inferred_type').AsString]));
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
  WriteLn('Example 123: Data Profiling - Statistics, Histograms, Type Inference, Patterns');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertSampleData;

    Demo1_BasicProfile;
    Demo2_NullAnalysis;
    Demo3_CardinalityAnalysis;
    Demo4_NumericStats;
    Demo5_StringLengths;
    Demo6_TypeInference;
    Demo7_PatternDetection;
    Demo8_DistributionHistograms;
    Demo9_DataQualityScore;
    Demo10_ProfileSummary;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
