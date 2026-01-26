{===============================================================================
  NDXSQLite Example 129 - Data Anonymization
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Email and phone masking techniques
  - Age and salary generalization (k-anonymity)
  - ZIP code truncation for privacy
  - Pseudonymization with consistent mapping
  - Anonymized view generation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataAnonymization;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Original PII data
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS persons (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  first_name TEXT NOT NULL,' +
    '  last_name TEXT NOT NULL,' +
    '  email TEXT NOT NULL,' +
    '  phone TEXT,' +
    '  age INTEGER,' +
    '  gender TEXT,' +
    '  zip_code TEXT,' +
    '  city TEXT,' +
    '  salary REAL,' +
    '  medical_condition TEXT' +
    ')'
  );

  // Anonymized output table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS persons_anon (' +
    '  id INTEGER PRIMARY KEY,' +
    '  first_name TEXT,' +
    '  last_name TEXT,' +
    '  email TEXT,' +
    '  phone TEXT,' +
    '  age_range TEXT,' +
    '  gender TEXT,' +
    '  zip_prefix TEXT,' +
    '  city TEXT,' +
    '  salary_band TEXT,' +
    '  medical_condition TEXT' +
    ')'
  );

  // Pseudonym mapping (for consistent pseudonymization)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS pseudonym_map (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  field_name TEXT NOT NULL,' +
    '  original_value TEXT NOT NULL,' +
    '  pseudonym TEXT NOT NULL,' +
    '  UNIQUE(field_name, original_value)' +
    ')'
  );

  // Anonymization policies per field
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS anon_policies (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  field_name TEXT NOT NULL UNIQUE,' +
    '  policy_type TEXT NOT NULL,' +  // mask, pseudonymize, generalize, suppress, keep
    '  parameters TEXT' +
    ')'
  );

  // K-anonymity equivalence classes
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS k_anon_classes (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  age_range TEXT,' +
    '  gender TEXT,' +
    '  zip_prefix TEXT,' +
    '  record_count INTEGER' +
    ')'
  );
end;

{ Inserts sample data for demonstrations. }
procedure InsertSampleData;
begin
  // 20 persons with realistic PII
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Alice'', ''Johnson'', ''alice.johnson@email.com'', ''555-0101'', 34, ''F'', ''90210'', ''Beverly Hills'', 85000, ''Diabetes'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Bob'', ''Smith'', ''bob.smith@work.org'', ''555-0102'', 45, ''M'', ''90211'', ''Beverly Hills'', 92000, ''Hypertension'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Carol'', ''Williams'', ''carol.w@provider.net'', ''555-0103'', 28, ''F'', ''90212'', ''Beverly Hills'', 67000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Dave'', ''Brown'', ''dave.brown@company.com'', ''555-0104'', 52, ''M'', ''10001'', ''New York'', 120000, ''Asthma'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Eve'', ''Davis'', ''eve.davis@mail.com'', ''555-0105'', 31, ''F'', ''10002'', ''New York'', 78000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Frank'', ''Miller'', ''frank.m@email.org'', ''555-0106'', 47, ''M'', ''10003'', ''New York'', 95000, ''Diabetes'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Grace'', ''Wilson'', ''grace.wilson@web.com'', ''555-0107'', 39, ''F'', ''60601'', ''Chicago'', 88000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Henry'', ''Moore'', ''henry.moore@corp.com'', ''555-0108'', 55, ''M'', ''60602'', ''Chicago'', 110000, ''Heart Disease'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Iris'', ''Taylor'', ''iris.t@service.net'', ''555-0109'', 26, ''F'', ''60603'', ''Chicago'', 62000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Jack'', ''Anderson'', ''jack.a@email.com'', ''555-0110'', 41, ''M'', ''30301'', ''Atlanta'', 79000, ''Asthma'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Karen'', ''Thomas'', ''karen.t@work.com'', ''555-0111'', 33, ''F'', ''30302'', ''Atlanta'', 71000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Leo'', ''Jackson'', ''leo.j@provider.org'', ''555-0112'', 48, ''M'', ''30303'', ''Atlanta'', 98000, ''Hypertension'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Mia'', ''White'', ''mia.white@mail.net'', ''555-0113'', 29, ''F'', ''90213'', ''Beverly Hills'', 69000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Nick'', ''Harris'', ''nick.h@company.org'', ''555-0114'', 36, ''M'', ''10004'', ''New York'', 83000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Olivia'', ''Martin'', ''olivia.m@web.org'', ''555-0115'', 43, ''F'', ''60604'', ''Chicago'', 91000, ''Diabetes'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Paul'', ''Garcia'', ''paul.g@email.net'', ''555-0116'', 57, ''M'', ''90214'', ''Beverly Hills'', 105000, ''Heart Disease'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Quinn'', ''Martinez'', ''quinn.m@corp.net'', ''555-0117'', 25, ''F'', ''10005'', ''New York'', 58000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Ryan'', ''Robinson'', ''ryan.r@service.com'', ''555-0118'', 50, ''M'', ''30304'', ''Atlanta'', 96000, ''Asthma'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Sara'', ''Clark'', ''sara.c@mail.org'', ''555-0119'', 38, ''F'', ''60605'', ''Chicago'', 84000, ''None'')');
  Conn.ExecuteNonQuery('INSERT INTO persons (first_name, last_name, email, phone, age, gender, zip_code, city, salary, medical_condition) VALUES (''Tom'', ''Lewis'', ''tom.lewis@work.net'', ''555-0120'', 44, ''M'', ''30305'', ''Atlanta'', 87000, ''Hypertension'')');

  // Anonymization policies
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''first_name'', ''pseudonymize'', ''prefix=Person'')');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''last_name'', ''suppress'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''email'', ''mask'', ''show=3'')');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''phone'', ''mask'', ''show_last=4'')');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''age'', ''generalize'', ''range=10'')');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''gender'', ''keep'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''zip_code'', ''generalize'', ''prefix=3'')');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''city'', ''keep'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''salary'', ''generalize'', ''band=20000'')');
  Conn.ExecuteNonQuery('INSERT INTO anon_policies (field_name, policy_type, parameters) VALUES (''medical_condition'', ''keep'', NULL)');
end;

// Helper: mask an email (show first 3 chars)
{ Returns an email with the local part masked, showing only the first 1-3 characters before '***@domain'. }
function MaskEmail(const AEmail: string): string;
var
  AtPos: Integer;
begin
  AtPos := Pos('@', AEmail);
  if AtPos > 4 then
    Result := Copy(AEmail, 1, 3) + '***@' + Copy(AEmail, AtPos + 1, Length(AEmail))
  else if AtPos > 1 then
    Result := Copy(AEmail, 1, 1) + '***@' + Copy(AEmail, AtPos + 1, Length(AEmail))
  else
    Result := '***';
end;

// Helper: mask a phone (show last 4)
{ Returns a phone number masked to show only the last 4 digits, prefixed with '***-'. }
function MaskPhone(const APhone: string): string;
begin
  if Length(APhone) > 4 then
    Result := '***-' + Copy(APhone, Length(APhone) - 3, 4)
  else
    Result := '****';
end;

// Helper: generalize age to range
{ Generalizes an age value into a decade range string. }
function GeneralizeAge(Age: Integer): string;
var
  LowBound: Integer;
begin
  LowBound := (Age div 10) * 10;
  Result := Format('%d-%d', [LowBound, LowBound + 9]);
end;

// Helper: generalize zip to prefix
{ Generalizes a zip code by keeping only the first three digits. }
function GeneralizeZip(const AZip: string): string;
begin
  if Length(AZip) >= 3 then
    Result := Copy(AZip, 1, 3) + '**'
  else
    Result := AZip;
end;

// Helper: generalize salary to band
{ Generalizes a salary value into a 20k band range string. }
function GeneralizeSalary(Salary: Double): string;
var
  LowBound: Integer;
begin
  LowBound := (Trunc(Salary) div 20000) * 20000;
  Result := Format('%dk-%dk', [LowBound div 1000, (LowBound + 20000) div 1000]);
end;

// Helper: simple hash-based pseudonym
{ Generates a pseudonym by combining a prefix with a sequential index (e.g., 'Person_1'). }
function Pseudonymize(const AValue, APrefix: string; AIndex: Integer): string;
begin
  Result := Format('%s_%d', [APrefix, AIndex]);
end;

// ============================================================
// Demo Sections
// ============================================================

{ Displays total record count and a sample of 5 original PII records with names, emails, phones, ages, and zip codes. }
procedure Demo1_OriginalData;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Original PII Data (Sample) ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM persons');
  try
    WriteLn(Format('   Total records: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Sample records (first 5):');
  WriteLn(Format('   %-8s %-10s %-25s %-12s %-4s %-6s', ['Name', 'Last', 'Email', 'Phone', 'Age', 'Zip']));
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery('SELECT * FROM persons LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-10s %-25s %-12s %-4d %-6s', [
        DS.FieldByName('first_name').AsString,
        DS.FieldByName('last_name').AsString,
        DS.FieldByName('email').AsString,
        DS.FieldByName('phone').AsString,
        DS.FieldByName('age').AsInteger,
        DS.FieldByName('zip_code').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   WARNING: This data contains identifiable PII!');
  WriteLn;
end;

{ Shows email and phone masking transformations, displaying original values alongside their masked versions. }
procedure Demo2_DirectMasking;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Direct Masking ===');
  WriteLn;
  WriteLn('   Applying masks to direct identifiers:');
  WriteLn(Format('   %-25s -> %-25s', ['Original Email', 'Masked Email']));
  WriteLn('   ' + StringOfChar('-', 55));

  DS := Conn.ExecuteQuery('SELECT email, phone FROM persons LIMIT 6');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s -> %-25s', [
        DS.FieldByName('email').AsString,
        MaskEmail(DS.FieldByName('email').AsString)
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   %-12s -> %-12s', ['Original Phone', 'Masked Phone']));
  WriteLn('   ' + StringOfChar('-', 30));

  DS := Conn.ExecuteQuery('SELECT phone FROM persons LIMIT 6');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s    -> %-12s', [
        DS.FieldByName('phone').AsString,
        MaskPhone(DS.FieldByName('phone').AsString)
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates consistent pseudonymization of personal identifiers. }
procedure Demo3_Pseudonymization;
var
  DS: TDataSet;
  Counter: Integer;
  OrigName, PseudoName: string;
begin
  WriteLn('=== 3. Consistent Pseudonymization ===');
  WriteLn;

  // Build pseudonym mapping
  DS := Conn.ExecuteQuery('SELECT DISTINCT first_name FROM persons ORDER BY first_name');
  try
    Counter := 0;
    while not DS.EOF do
    begin
      Inc(Counter);
      OrigName := DS.FieldByName('first_name').AsString;
      PseudoName := Pseudonymize(OrigName, 'Person', Counter);
      Conn.ExecuteNonQuery(Format(
        'INSERT OR IGNORE INTO pseudonym_map (field_name, original_value, pseudonym) VALUES (''first_name'', ''%s'', ''%s'')',
        [OrigName, PseudoName]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Pseudonym mapping (consistent across uses):');
  WriteLn(Format('   %-12s -> %-15s', ['Original', 'Pseudonym']));
  WriteLn('   ' + StringOfChar('-', 30));

  DS := Conn.ExecuteQuery('SELECT original_value, pseudonym FROM pseudonym_map WHERE field_name = ''first_name'' LIMIT 10');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s -> %-15s', [
        DS.FieldByName('original_value').AsString,
        DS.FieldByName('pseudonym').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Key property: Same name always maps to same pseudonym');
  WriteLn('   "Alice" -> always "Person_1" (reversible with mapping table)');
  WriteLn;
end;

{ Demonstrates generalization hierarchies for quasi-identifiers. }
procedure Demo4_Generalization;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Generalization Hierarchies ===');
  WriteLn;
  WriteLn('   Age -> Age Range (10-year bands):');
  WriteLn(Format('   %-6s -> %-10s', ['Age', 'Range']));
  WriteLn('   ' + StringOfChar('-', 20));

  DS := Conn.ExecuteQuery('SELECT DISTINCT age FROM persons ORDER BY age LIMIT 10');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-6d -> %-10s', [
        DS.FieldByName('age').AsInteger,
        GeneralizeAge(DS.FieldByName('age').AsInteger)
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Zip Code -> Zip Prefix (3 digits):');
  WriteLn(Format('   %-8s -> %-8s', ['Zip', 'Prefix']));
  WriteLn('   ' + StringOfChar('-', 20));

  DS := Conn.ExecuteQuery('SELECT DISTINCT zip_code FROM persons ORDER BY zip_code LIMIT 8');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s -> %-8s', [
        DS.FieldByName('zip_code').AsString,
        GeneralizeZip(DS.FieldByName('zip_code').AsString)
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Salary -> Salary Band (20k bands):');
  WriteLn(Format('   %-10s -> %-10s', ['Salary', 'Band']));
  WriteLn('   ' + StringOfChar('-', 25));

  DS := Conn.ExecuteQuery('SELECT DISTINCT salary FROM persons ORDER BY salary LIMIT 8');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   $%-9.0f -> %-10s', [
        DS.FieldByName('salary').AsFloat,
        GeneralizeSalary(DS.FieldByName('salary').AsFloat)
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Builds equivalence classes from quasi-identifiers (age range, gender, zip prefix), calculates minimum k, and flags k=1 violations. }
procedure Demo5_KAnonymity;
var
  DS: TDataSet;
  AgeRange, Gender, ZipPrefix: string;
  RecCount, MinK, TotalClasses, ViolationCount: Integer;
begin
  WriteLn('=== 5. K-Anonymity Analysis ===');
  WriteLn;

  // Build equivalence classes using quasi-identifiers: age_range, gender, zip_prefix
  Conn.ExecuteNonQuery('DELETE FROM k_anon_classes');

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE WHEN age < 30 THEN ''20-29'' WHEN age < 40 THEN ''30-39'' WHEN age < 50 THEN ''40-49'' ELSE ''50-59'' END as age_range,' +
    '  gender,' +
    '  SUBSTR(zip_code, 1, 3) as zip_prefix,' +
    '  COUNT(*) as cnt ' +
    'FROM persons ' +
    'GROUP BY age_range, gender, zip_prefix ' +
    'ORDER BY cnt');
  try
    while not DS.EOF do
    begin
      AgeRange := DS.FieldByName('age_range').AsString;
      Gender := DS.FieldByName('gender').AsString;
      ZipPrefix := DS.FieldByName('zip_prefix').AsString;
      RecCount := DS.FieldByName('cnt').AsInteger;
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO k_anon_classes (age_range, gender, zip_prefix, record_count) VALUES (''%s'', ''%s'', ''%s'', %d)',
        [AgeRange, Gender, ZipPrefix, RecCount]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Equivalence classes (quasi-identifiers: age_range, gender, zip_prefix):');
  WriteLn(Format('   %-10s %-8s %-10s %s', ['Age Range', 'Gender', 'Zip Prefix', 'Count']));
  WriteLn('   ' + StringOfChar('-', 45));

  DS := Conn.ExecuteQuery('SELECT * FROM k_anon_classes ORDER BY record_count, age_range');
  try
    MinK := MaxInt;
    TotalClasses := 0;
    ViolationCount := 0;
    while not DS.EOF do
    begin
      RecCount := DS.FieldByName('record_count').AsInteger;
      Inc(TotalClasses);
      if RecCount < MinK then
        MinK := RecCount;
      if RecCount < 2 then
        Inc(ViolationCount);
      WriteLn(Format('   %-10s %-8s %-10s %d%s', [
        DS.FieldByName('age_range').AsString,
        DS.FieldByName('gender').AsString,
        DS.FieldByName('zip_prefix').AsString,
        RecCount,
        BoolToStr(RecCount < 2, '  <-- k=1 violation!', '')
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Minimum k: %d', [MinK]));
  WriteLn(Format('   Total equivalence classes: %d', [TotalClasses]));
  WriteLn(Format('   Classes with k<2: %d (violations for 2-anonymity)', [ViolationCount]));
  WriteLn;
end;

{ Identifies records in singleton equivalence classes that must be suppressed to achieve 2-anonymity, with utility percentage. }
procedure Demo6_DataSuppression;
var
  DS: TDataSet;
  SuppressedCount: Integer;
begin
  WriteLn('=== 6. Data Suppression for K-Anonymity ===');
  WriteLn;
  WriteLn('   To achieve 2-anonymity, suppress records in singleton classes:');
  WriteLn;

  // Find records that would be suppressed (in classes with count < 2)
  DS := Conn.ExecuteQuery(
    'SELECT p.first_name, p.age, p.gender, p.zip_code, ' +
    '  CASE WHEN p.age < 30 THEN ''20-29'' WHEN p.age < 40 THEN ''30-39'' WHEN p.age < 50 THEN ''40-49'' ELSE ''50-59'' END as age_range,' +
    '  SUBSTR(p.zip_code, 1, 3) as zip_prefix ' +
    'FROM persons p ' +
    'WHERE (' +
    '  CASE WHEN p.age < 30 THEN ''20-29'' WHEN p.age < 40 THEN ''30-39'' WHEN p.age < 50 THEN ''40-49'' ELSE ''50-59'' END || ''+'' || ' +
    '  p.gender || ''+'' || SUBSTR(p.zip_code, 1, 3)' +
    ') IN (' +
    '  SELECT k.age_range || ''+'' || k.gender || ''+'' || k.zip_prefix ' +
    '  FROM k_anon_classes k WHERE k.record_count < 2' +
    ')');
  try
    SuppressedCount := 0;
    WriteLn(Format('   %-10s %-4s %-6s %-6s %-10s %-6s', ['Name', 'Age', 'Gender', 'Zip', 'Age Range', 'Prefix']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      Inc(SuppressedCount);
      WriteLn(Format('   %-10s %-4d %-6s %-6s %-10s %-6s  [SUPPRESS]', [
        DS.FieldByName('first_name').AsString,
        DS.FieldByName('age').AsInteger,
        DS.FieldByName('gender').AsString,
        DS.FieldByName('zip_code').AsString,
        DS.FieldByName('age_range').AsString,
        DS.FieldByName('zip_prefix').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Records to suppress: %d out of 20', [SuppressedCount]));
  WriteLn(Format('   Remaining after suppression: %d', [20 - SuppressedCount]));
  WriteLn(Format('   Data utility preserved: %.1f%%', [(20 - SuppressedCount) / 20.0 * 100]));
  WriteLn;
end;

{ Creates token-based pseudonyms for email addresses using sequential numbering and stores mappings for reversibility. }
procedure Demo7_TokenMapping;
var
  DS: TDataSet;
  Counter: Integer;
  OrigEmail, Token: string;
begin
  WriteLn('=== 7. Token-Based Pseudonymization ===');
  WriteLn;

  // Create token mappings for emails
  DS := Conn.ExecuteQuery('SELECT DISTINCT email FROM persons ORDER BY email');
  try
    Counter := 0;
    while not DS.EOF do
    begin
      Inc(Counter);
      OrigEmail := DS.FieldByName('email').AsString;
      Token := 'user_' + Format('%.4d', [Counter]) + '@anon.local';
      Conn.ExecuteNonQuery(Format(
        'INSERT OR IGNORE INTO pseudonym_map (field_name, original_value, pseudonym) VALUES (''email'', ''%s'', ''%s'')',
        [OrigEmail, Token]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Token mapping (reversible with secure lookup):');
  WriteLn(Format('   %-30s -> %-25s', ['Original Email', 'Token']));
  WriteLn('   ' + StringOfChar('-', 60));

  DS := Conn.ExecuteQuery('SELECT original_value, pseudonym FROM pseudonym_map WHERE field_name = ''email'' LIMIT 8');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-30s -> %-25s', [
        DS.FieldByName('original_value').AsString,
        DS.FieldByName('pseudonym').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Token properties:');
  WriteLn('   - Consistent: same email always gets same token');
  WriteLn('   - Reversible: mapping table allows re-identification');
  WriteLn('   - Secure: mapping table must be stored separately');
  WriteLn;
end;

{ Compares original salary and age statistics with generalized distributions to show that aggregate patterns are preserved. }
procedure Demo8_UtilityPreservation;
var
  DS: TDataSet;
  OrigAvg, OrigMin, OrigMax: Double;
begin
  WriteLn('=== 8. Utility Preservation ===');
  WriteLn;
  WriteLn('   Comparing statistics before and after generalization:');
  WriteLn;

  // Original salary statistics
  DS := Conn.ExecuteQuery('SELECT AVG(salary) as avg_s, MIN(salary) as min_s, MAX(salary) as max_s, COUNT(*) as cnt FROM persons');
  try
    OrigAvg := DS.FieldByName('avg_s').AsFloat;
    OrigMin := DS.FieldByName('min_s').AsFloat;
    OrigMax := DS.FieldByName('max_s').AsFloat;
    WriteLn('   Original salary data:');
    WriteLn(Format('     Count: %d', [DS.FieldByName('cnt').AsInteger]));
    WriteLn(Format('     Average: $%.0f', [OrigAvg]));
    WriteLn(Format('     Min: $%.0f, Max: $%.0f', [OrigMin, OrigMax]));
    WriteLn(Format('     Range: $%.0f', [OrigMax - OrigMin]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   After generalization (20k bands):');

  // Show distribution in bands
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (CAST(salary AS INTEGER) / 20000) * 20000 as band_low,' +
    '  COUNT(*) as cnt ' +
    'FROM persons GROUP BY band_low ORDER BY band_low');
  try
    WriteLn(Format('   %-15s %s', ['Salary Band', 'Count']));
    WriteLn('   ' + StringOfChar('-', 25));
    while not DS.EOF do
    begin
      WriteLn(Format('   $%dk-$%dk %5d', [
        DS.FieldByName('band_low').AsInteger div 1000,
        (DS.FieldByName('band_low').AsInteger + 20000) div 1000,
        DS.FieldByName('cnt').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Age distribution (original vs generalized):');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE WHEN age < 30 THEN ''20-29'' WHEN age < 40 THEN ''30-39'' WHEN age < 50 THEN ''40-49'' ELSE ''50-59'' END as age_range,' +
    '  COUNT(*) as cnt, AVG(age) as avg_age ' +
    'FROM persons GROUP BY age_range ORDER BY age_range');
  try
    WriteLn(Format('   %-10s %-6s %-8s', ['Range', 'Count', 'Avg Age']));
    WriteLn('   ' + StringOfChar('-', 28));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-6d %-8.1f', [
        DS.FieldByName('age_range').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('avg_age').AsFloat
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Utility: Distribution shape is preserved, exact values are hidden');
  WriteLn;
end;

{ Lists configured anonymization policies per field and applies all transformations (mask, pseudonymize, generalize, suppress) to sample records. }
procedure Demo9_FieldLevelPolicies;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Field-Level Anonymization Policies ===');
  WriteLn;
  WriteLn('   Configured policies:');
  WriteLn(Format('   %-20s %-15s %-20s', ['Field', 'Policy', 'Parameters']));
  WriteLn('   ' + StringOfChar('-', 60));

  DS := Conn.ExecuteQuery('SELECT * FROM anon_policies ORDER BY field_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-15s %-20s', [
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('policy_type').AsString,
        VarToStr(DS.FieldByName('parameters').Value)
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Applying all policies to first 5 records:');
  WriteLn(Format('   %-12s %-10s %-20s %-10s %-8s %-6s %-10s', [
    'Name', 'Last', 'Email', 'Phone', 'Age', 'Zip', 'Salary']));
  WriteLn('   ' + StringOfChar('-', 80));

  DS := Conn.ExecuteQuery('SELECT * FROM persons LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-10s %-20s %-10s %-8s %-6s %-10s', [
        'Person_' + IntToStr(DS.FieldByName('id').AsInteger),       // pseudonymize
        '[REMOVED]',                                                  // suppress
        MaskEmail(DS.FieldByName('email').AsString),                 // mask
        MaskPhone(DS.FieldByName('phone').AsString),                 // mask
        GeneralizeAge(DS.FieldByName('age').AsInteger),              // generalize
        GeneralizeZip(DS.FieldByName('zip_code').AsString),          // generalize
        GeneralizeSalary(DS.FieldByName('salary').AsFloat)           // generalize
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Policy types: mask, pseudonymize, generalize, suppress, keep');
  WriteLn;
end;

{ Demonstrates data anonymization statistics and privacy metrics. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Anonymization Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM persons');
  try
    WriteLn(Format('   Total records: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM pseudonym_map');
  try
    WriteLn(Format('   Pseudonym mappings: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM anon_policies');
  try
    WriteLn(Format('   Field policies: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM k_anon_classes');
  try
    WriteLn(Format('   Equivalence classes: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   PII field analysis:');
  WriteLn(Format('   %-20s %-15s %-12s', ['Field', 'Type', 'Risk Level']));
  WriteLn('   ' + StringOfChar('-', 50));
  WriteLn(Format('   %-20s %-15s %-12s', ['first_name', 'Direct ID', 'HIGH']));
  WriteLn(Format('   %-20s %-15s %-12s', ['last_name', 'Direct ID', 'HIGH']));
  WriteLn(Format('   %-20s %-15s %-12s', ['email', 'Direct ID', 'HIGH']));
  WriteLn(Format('   %-20s %-15s %-12s', ['phone', 'Direct ID', 'HIGH']));
  WriteLn(Format('   %-20s %-15s %-12s', ['age', 'Quasi-ID', 'MEDIUM']));
  WriteLn(Format('   %-20s %-15s %-12s', ['gender', 'Quasi-ID', 'LOW']));
  WriteLn(Format('   %-20s %-15s %-12s', ['zip_code', 'Quasi-ID', 'MEDIUM']));
  WriteLn(Format('   %-20s %-15s %-12s', ['city', 'Quasi-ID', 'LOW']));
  WriteLn(Format('   %-20s %-15s %-12s', ['salary', 'Sensitive', 'HIGH']));
  WriteLn(Format('   %-20s %-15s %-12s', ['medical_condition', 'Sensitive', 'HIGH']));

  WriteLn;
  WriteLn('   K-anonymity summary:');
  DS := Conn.ExecuteQuery('SELECT MIN(record_count) as min_k, MAX(record_count) as max_k, AVG(record_count) as avg_k FROM k_anon_classes');
  try
    WriteLn(Format('     Min k: %d', [DS.FieldByName('min_k').AsInteger]));
    WriteLn(Format('     Max k: %d', [DS.FieldByName('max_k').AsInteger]));
    WriteLn(Format('     Avg class size: %.1f', [DS.FieldByName('avg_k').AsFloat]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

// ============================================================
// Main
// ============================================================

begin
  WriteLn('Example 129: Data Anonymization - PII Protection, K-Anonymity, Pseudonymization');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;

    Demo1_OriginalData;
    Demo2_DirectMasking;
    Demo3_Pseudonymization;
    Demo4_Generalization;
    Demo5_KAnonymity;
    Demo6_DataSuppression;
    Demo7_TokenMapping;
    Demo8_UtilityPreservation;
    Demo9_FieldLevelPolicies;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
