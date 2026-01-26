{===============================================================================
  NDXSQLite Example 81 - Data Masking
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates data masking techniques for Personally Identifiable Information:
  - Email masking (j***@example.com)
  - Phone number masking (***-***-1234)
  - Credit card masking (****-****-****-5678)
  - SSN/National ID masking (***-**-6789)
  - Name masking (partial or full)
  - Address masking
  - Role-based masking levels
  - Masking policies and rules
  - Audit trail for unmasked access

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program DataMasking;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;
  CurrentUserId: Integer;
  CurrentRole: string;

// =============================================================================
// Masking Functions
// =============================================================================
{ Returns an email address with the local part obscured, showing only the first and last characters separated by asterisks while preserving the domain. }
function MaskEmail(const Email: string): string;
var
  AtPos: Integer;
  LocalPart, Domain: string;
begin
  AtPos := Pos('@', Email);
  if AtPos <= 0 then
  begin
    Result := '***@***.***';
    Exit;
  end;

  LocalPart := Copy(Email, 1, AtPos - 1);
  Domain := Copy(Email, AtPos, Length(Email) - AtPos + 1);

  if Length(LocalPart) <= 2 then
    Result := LocalPart[1] + '***' + Domain
  else
    Result := LocalPart[1] + StringOfChar('*', Length(LocalPart) - 2) + LocalPart[Length(LocalPart)] + Domain;
end;

{ Returns a phone number with only the last four digits visible, replacing earlier digits with asterisks in the format ***-***-NNNN. }
function MaskPhone(const Phone: string): string;
var
  I, DigitCount: Integer;
  Digits: string;
begin
  // Extract digits
  Digits := '';
  for I := 1 to Length(Phone) do
    if Phone[I] in ['0'..'9'] then
      Digits := Digits + Phone[I];

  DigitCount := Length(Digits);
  if DigitCount <= 4 then
  begin
    Result := '***-****';
    Exit;
  end;

  // Show only last 4 digits
  Result := StringOfChar('*', DigitCount - 4);
  // Format with dashes
  Result := '***-***-' + Copy(Digits, DigitCount - 3, 4);
end;

{ Returns a credit card number with only the last four digits visible, replacing earlier digits with asterisks in the format ****-****-****-NNNN. }
function MaskCreditCard(const CardNumber: string): string;
var
  I: Integer;
  Digits: string;
begin
  // Extract digits
  Digits := '';
  for I := 1 to Length(CardNumber) do
    if CardNumber[I] in ['0'..'9'] then
      Digits := Digits + CardNumber[I];

  if Length(Digits) < 4 then
  begin
    Result := '****-****-****-****';
    Exit;
  end;

  // Show only last 4 digits
  Result := '****-****-****-' + Copy(Digits, Length(Digits) - 3, 4);
end;

{ Returns a Social Security Number with only the last four digits visible, replacing the area and group numbers with asterisks in the format ***-**-NNNN. }
function MaskSSN(const SSN: string): string;
var
  I: Integer;
  Digits: string;
begin
  Digits := '';
  for I := 1 to Length(SSN) do
    if SSN[I] in ['0'..'9'] then
      Digits := Digits + SSN[I];

  if Length(Digits) < 4 then
  begin
    Result := '***-**-****';
    Exit;
  end;

  Result := '***-**-' + Copy(Digits, Length(Digits) - 3, 4);
end;

{ Returns a name reduced to initials with periods, extracting first letters from first and last name components. }
function MaskName(const Name: string): string;
var
  SpacePos: Integer;
  FirstName, LastName: string;
begin
  SpacePos := Pos(' ', Name);
  if SpacePos > 0 then
  begin
    FirstName := Copy(Name, 1, SpacePos - 1);
    LastName := Copy(Name, SpacePos + 1, Length(Name) - SpacePos);
    Result := FirstName[1] + '. ' + LastName[1] + '.';
  end
  else if Length(Name) > 0 then
    Result := Name[1] + '.'
  else
    Result := '***';
end;

{ Returns an address with the street portion replaced by asterisks while preserving the city and state portion after the first comma. }
function MaskAddress(const Address: string): string;
var
  Parts: Integer;
  I: Integer;
begin
  // Count words, mask street but keep city/state
  Parts := 0;
  for I := 1 to Length(Address) do
    if Address[I] = ',' then
      Inc(Parts);

  if Parts >= 1 then
    // Show only from first comma (city/state)
    Result := '*** ' + Copy(Address, Pos(',', Address), Length(Address))
  else
    Result := '*** ***';
end;

{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Data Masking Schema');
  WriteLn('   ==============================');

  // Users with roles
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  display_name TEXT,' +
    '  role TEXT CHECK(role IN (''admin'', ''data_officer'', ''analyst'', ''support'', ''viewer''))' +
    ')');

  // Customer PII data
  Conn.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  first_name TEXT NOT NULL,' +
    '  last_name TEXT NOT NULL,' +
    '  email TEXT NOT NULL,' +
    '  phone TEXT,' +
    '  ssn TEXT,' +
    '  date_of_birth TEXT,' +
    '  address TEXT,' +
    '  city TEXT,' +
    '  state TEXT,' +
    '  zip_code TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Payment information
  Conn.ExecuteNonQuery(
    'CREATE TABLE payment_methods (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER NOT NULL REFERENCES customers(id),' +
    '  card_type TEXT,' +
    '  card_number TEXT NOT NULL,' +
    '  expiry_date TEXT,' +
    '  cardholder_name TEXT,' +
    '  billing_zip TEXT' +
    ')');

  // Medical records (highly sensitive)
  Conn.ExecuteNonQuery(
    'CREATE TABLE medical_records (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER NOT NULL REFERENCES customers(id),' +
    '  record_type TEXT,' +
    '  diagnosis TEXT,' +
    '  medication TEXT,' +
    '  doctor_name TEXT,' +
    '  visit_date TEXT,' +
    '  notes TEXT' +
    ')');

  // Masking policies
  Conn.ExecuteNonQuery(
    'CREATE TABLE masking_policies (' +
    '  id INTEGER PRIMARY KEY,' +
    '  policy_name TEXT NOT NULL,' +
    '  target_table TEXT NOT NULL,' +
    '  target_column TEXT NOT NULL,' +
    '  data_type TEXT CHECK(data_type IN (''email'', ''phone'', ''ssn'', ''credit_card'', ''name'', ''address'', ''date'', ''text'')),' +
    '  min_role_to_unmask TEXT,' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Unmask access log
  Conn.ExecuteNonQuery(
    'CREATE TABLE unmask_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL,' +
    '  target_table TEXT NOT NULL,' +
    '  target_column TEXT NOT NULL,' +
    '  record_id INTEGER,' +
    '  reason TEXT,' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  WriteLn('   Created tables: users, customers, payment_methods,');
  WriteLn('                   medical_records, masking_policies, unmask_log');
  WriteLn('');
end;

// =============================================================================
// Sample Data
// =============================================================================
{ Populates the database with users across five role levels, customers with full PII (email, phone, SSN, address), payment methods with card numbers, medical records, and masking policy configurations. }
procedure InsertSampleData;
begin
  WriteLn('2. Creating Sample Data');
  WriteLn('   ======================');

  // Users
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (1, ''admin'', ''System Admin'', ''admin'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (2, ''dpo'', ''Data Protection Officer'', ''data_officer'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (3, ''analyst1'', ''Jane Analyst'', ''analyst'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (4, ''support1'', ''Support Agent'', ''support'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (5, ''viewer1'', ''External Auditor'', ''viewer'')');

  WriteLn('   Created 5 users with different roles');

  // Customers with PII
  Conn.ExecuteNonQuery(
    'INSERT INTO customers (first_name, last_name, email, phone, ssn, date_of_birth, address, city, state, zip_code) ' +
    'VALUES (''John'', ''Doe'', ''john.doe@email.com'', ''555-123-4567'', ''123-45-6789'', ''1985-03-15'', ''123 Main Street'', ''Springfield'', ''IL'', ''62701'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO customers (first_name, last_name, email, phone, ssn, date_of_birth, address, city, state, zip_code) ' +
    'VALUES (''Jane'', ''Smith'', ''jane.smith@company.org'', ''555-987-6543'', ''987-65-4321'', ''1990-07-22'', ''456 Oak Avenue'', ''Portland'', ''OR'', ''97201'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO customers (first_name, last_name, email, phone, ssn, date_of_birth, address, city, state, zip_code) ' +
    'VALUES (''Robert'', ''Johnson'', ''rjohnson@webmail.net'', ''555-456-7890'', ''456-78-9012'', ''1978-11-30'', ''789 Pine Road'', ''Austin'', ''TX'', ''73301'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO customers (first_name, last_name, email, phone, ssn, date_of_birth, address, city, state, zip_code) ' +
    'VALUES (''Maria'', ''Garcia'', ''m.garcia@domain.io'', ''555-321-0987'', ''321-09-8765'', ''1995-01-08'', ''321 Elm Court'', ''Denver'', ''CO'', ''80201'')');

  WriteLn('   Created 4 customers with PII data');

  // Payment methods
  Conn.ExecuteNonQuery('INSERT INTO payment_methods (customer_id, card_type, card_number, expiry_date, cardholder_name, billing_zip) VALUES (1, ''Visa'', ''4111222233334444'', ''12/26'', ''John Doe'', ''62701'')');
  Conn.ExecuteNonQuery('INSERT INTO payment_methods (customer_id, card_type, card_number, expiry_date, cardholder_name, billing_zip) VALUES (2, ''MasterCard'', ''5500666677778888'', ''03/27'', ''Jane Smith'', ''97201'')');
  Conn.ExecuteNonQuery('INSERT INTO payment_methods (customer_id, card_type, card_number, expiry_date, cardholder_name, billing_zip) VALUES (3, ''Amex'', ''378912345678901'', ''09/25'', ''Robert Johnson'', ''73301'')');
  Conn.ExecuteNonQuery('INSERT INTO payment_methods (customer_id, card_type, card_number, expiry_date, cardholder_name, billing_zip) VALUES (4, ''Visa'', ''4222333344445555'', ''06/28'', ''Maria Garcia'', ''80201'')');

  WriteLn('   Created 4 payment methods');

  // Medical records
  Conn.ExecuteNonQuery('INSERT INTO medical_records (customer_id, record_type, diagnosis, medication, doctor_name, visit_date) VALUES (1, ''visit'', ''Hypertension'', ''Lisinopril 10mg'', ''Dr. Wilson'', ''2024-06-15'')');
  Conn.ExecuteNonQuery('INSERT INTO medical_records (customer_id, record_type, diagnosis, medication, doctor_name, visit_date) VALUES (2, ''lab'', ''Normal cholesterol'', ''None'', ''Dr. Chen'', ''2024-08-20'')');
  Conn.ExecuteNonQuery('INSERT INTO medical_records (customer_id, record_type, diagnosis, medication, doctor_name, visit_date) VALUES (3, ''visit'', ''Type 2 Diabetes'', ''Metformin 500mg'', ''Dr. Patel'', ''2024-09-10'')');

  WriteLn('   Created 3 medical records');

  // Masking policies
  Conn.ExecuteNonQuery('INSERT INTO masking_policies (policy_name, target_table, target_column, data_type, min_role_to_unmask) VALUES (''mask_customer_email'', ''customers'', ''email'', ''email'', ''data_officer'')');
  Conn.ExecuteNonQuery('INSERT INTO masking_policies (policy_name, target_table, target_column, data_type, min_role_to_unmask) VALUES (''mask_customer_phone'', ''customers'', ''phone'', ''phone'', ''support'')');
  Conn.ExecuteNonQuery('INSERT INTO masking_policies (policy_name, target_table, target_column, data_type, min_role_to_unmask) VALUES (''mask_customer_ssn'', ''customers'', ''ssn'', ''ssn'', ''admin'')');
  Conn.ExecuteNonQuery('INSERT INTO masking_policies (policy_name, target_table, target_column, data_type, min_role_to_unmask) VALUES (''mask_payment_card'', ''payment_methods'', ''card_number'', ''credit_card'', ''data_officer'')');
  Conn.ExecuteNonQuery('INSERT INTO masking_policies (policy_name, target_table, target_column, data_type, min_role_to_unmask) VALUES (''mask_customer_dob'', ''customers'', ''date_of_birth'', ''date'', ''analyst'')');
  Conn.ExecuteNonQuery('INSERT INTO masking_policies (policy_name, target_table, target_column, data_type, min_role_to_unmask) VALUES (''mask_medical_diag'', ''medical_records'', ''diagnosis'', ''text'', ''admin'')');

  WriteLn('   Created 6 masking policies');
  WriteLn('');
end;

// =============================================================================
// Role Hierarchy Check
// =============================================================================
{ Returns the numeric privilege level for a given role name. }
function RoleLevel(const Role: string): Integer;
begin
  if Role = 'admin' then Result := 5
  else if Role = 'data_officer' then Result := 4
  else if Role = 'analyst' then Result := 3
  else if Role = 'support' then Result := 2
  else if Role = 'viewer' then Result := 1
  else Result := 0;
end;

{ Checks whether the user role has sufficient privilege to unmask data. }
function CanUnmask(const UserRole, MinRole: string): Boolean;
begin
  Result := RoleLevel(UserRole) >= RoleLevel(MinRole);
end;

{ Sets the global CurrentUserId and CurrentRole variables by looking up the user's role from the database. }
procedure SetCurrentUser(UserId: Integer);
begin
  CurrentUserId := UserId;
  CurrentRole := VarToStr(Conn.ExecuteScalar('SELECT role FROM users WHERE id = ?', [UserId]));
end;

// =============================================================================
// Demo: Basic Masking Functions
// =============================================================================
{ Displays examples of each masking function (email, phone, credit card, SSN, name, address) with sample inputs and their masked outputs. }
procedure DemoMaskingFunctions;
begin
  WriteLn('3. Masking Functions Demo');
  WriteLn('   ========================');
  WriteLn('');

  WriteLn('   Email masking:');
  WriteLn(Format('     john.doe@email.com     -> %s', [MaskEmail('john.doe@email.com')]));
  WriteLn(Format('     jane.smith@company.org -> %s', [MaskEmail('jane.smith@company.org')]));
  WriteLn(Format('     rjohnson@webmail.net   -> %s', [MaskEmail('rjohnson@webmail.net')]));
  WriteLn(Format('     m.garcia@domain.io     -> %s', [MaskEmail('m.garcia@domain.io')]));

  WriteLn('');
  WriteLn('   Phone masking:');
  WriteLn(Format('     555-123-4567 -> %s', [MaskPhone('555-123-4567')]));
  WriteLn(Format('     555-987-6543 -> %s', [MaskPhone('555-987-6543')]));

  WriteLn('');
  WriteLn('   Credit card masking:');
  WriteLn(Format('     4111222233334444  -> %s', [MaskCreditCard('4111222233334444')]));
  WriteLn(Format('     5500666677778888  -> %s', [MaskCreditCard('5500666677778888')]));

  WriteLn('');
  WriteLn('   SSN masking:');
  WriteLn(Format('     123-45-6789 -> %s', [MaskSSN('123-45-6789')]));
  WriteLn(Format('     987-65-4321 -> %s', [MaskSSN('987-65-4321')]));

  WriteLn('');
  WriteLn('   Name masking:');
  WriteLn(Format('     John Doe      -> %s', [MaskName('John Doe')]));
  WriteLn(Format('     Jane Smith    -> %s', [MaskName('Jane Smith')]));

  WriteLn('');
  WriteLn('   Address masking:');
  WriteLn(Format('     123 Main Street, Springfield -> %s', [MaskAddress('123 Main Street, Springfield')]));

  WriteLn('');
end;

// =============================================================================
// Demo: Role-Based Data View
// =============================================================================
{ Shows the same customer records viewed by admin (full access), support (phone visible, email/SSN masked), and viewer (all PII masked) to illustrate role-based data visibility. }
procedure DemoRoleBasedView;
var
  DS: TDataSet;
  FullName, Email, Phone, SSN: string;
begin
  WriteLn('4. Role-Based Customer Data View');
  WriteLn('   ===============================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT * FROM customers ORDER BY id');
  try
    // Admin view - fully unmasked
    SetCurrentUser(1);
    WriteLn(Format('   As %s (role: %s) - FULL ACCESS:',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));
    WriteLn(Format('   %-15s | %-25s | %-14s | %s', ['Name', 'Email', 'Phone', 'SSN']));
    WriteLn('   ' + StringOfChar('-', 70));

    DS.First;
    while not DS.EOF do
    begin
      FullName := DS.FieldByName('first_name').AsString + ' ' + DS.FieldByName('last_name').AsString;
      Email := DS.FieldByName('email').AsString;
      Phone := DS.FieldByName('phone').AsString;
      SSN := DS.FieldByName('ssn').AsString;
      WriteLn(Format('   %-15s | %-25s | %-14s | %s', [FullName, Email, Phone, SSN]));
      DS.Next;
    end;

    // Support view - can see phone, masked email/SSN
    WriteLn('');
    SetCurrentUser(4);
    WriteLn(Format('   As %s (role: %s) - phone visible, email/SSN masked:',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));
    WriteLn(Format('   %-15s | %-25s | %-14s | %s', ['Name', 'Email', 'Phone', 'SSN']));
    WriteLn('   ' + StringOfChar('-', 70));

    DS.First;
    while not DS.EOF do
    begin
      FullName := DS.FieldByName('first_name').AsString + ' ' + DS.FieldByName('last_name').AsString;
      Email := MaskEmail(DS.FieldByName('email').AsString);
      Phone := DS.FieldByName('phone').AsString;
      SSN := MaskSSN(DS.FieldByName('ssn').AsString);
      WriteLn(Format('   %-15s | %-25s | %-14s | %s', [FullName, Email, Phone, SSN]));
      DS.Next;
    end;

    // Viewer view - everything masked
    WriteLn('');
    SetCurrentUser(5);
    WriteLn(Format('   As %s (role: %s) - ALL PII masked:',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));
    WriteLn(Format('   %-15s | %-25s | %-14s | %s', ['Name', 'Email', 'Phone', 'SSN']));
    WriteLn('   ' + StringOfChar('-', 70));

    DS.First;
    while not DS.EOF do
    begin
      FullName := MaskName(DS.FieldByName('first_name').AsString + ' ' + DS.FieldByName('last_name').AsString);
      Email := MaskEmail(DS.FieldByName('email').AsString);
      Phone := MaskPhone(DS.FieldByName('phone').AsString);
      SSN := MaskSSN(DS.FieldByName('ssn').AsString);
      WriteLn(Format('   %-15s | %-25s | %-14s | %s', [FullName, Email, Phone, SSN]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Payment Data Masking
// =============================================================================
{ Displays payment method records as seen by data_officer (full card numbers visible) versus support agent (card numbers and cardholder names masked). }
procedure DemoPaymentMasking;
var
  DS: TDataSet;
  CardNum, Holder: string;
begin
  WriteLn('5. Payment Data Masking');
  WriteLn('   ======================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT pm.*, c.first_name, c.last_name FROM payment_methods pm ' +
    'JOIN customers c ON pm.customer_id = c.id ORDER BY pm.id');
  try
    // Data officer can see cards
    SetCurrentUser(2);
    WriteLn(Format('   As %s (role: %s) - card numbers visible:',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));
    WriteLn(Format('   %-8s | %-19s | %-7s | %s', ['Type', 'Card Number', 'Expiry', 'Holder']));
    WriteLn('   ' + StringOfChar('-', 55));

    DS.First;
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s | %-19s | %-7s | %s',
        [DS.FieldByName('card_type').AsString,
         DS.FieldByName('card_number').AsString,
         DS.FieldByName('expiry_date').AsString,
         DS.FieldByName('cardholder_name').AsString]));
      DS.Next;
    end;

    // Support agent - cards masked
    WriteLn('');
    SetCurrentUser(4);
    WriteLn(Format('   As %s (role: %s) - card numbers MASKED:',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));
    WriteLn(Format('   %-8s | %-19s | %-7s | %s', ['Type', 'Card Number', 'Expiry', 'Holder']));
    WriteLn('   ' + StringOfChar('-', 55));

    DS.First;
    while not DS.EOF do
    begin
      CardNum := MaskCreditCard(DS.FieldByName('card_number').AsString);
      Holder := MaskName(DS.FieldByName('cardholder_name').AsString);
      WriteLn(Format('   %-8s | %-19s | %-7s | %s',
        [DS.FieldByName('card_type').AsString,
         CardNum,
         DS.FieldByName('expiry_date').AsString,
         Holder]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Medical Records (Highest Sensitivity)
// =============================================================================
{ Displays medical records with full visibility for admin, redacted diagnosis/medication for analyst, and complete access denial for viewer role. }
procedure DemoMedicalMasking;
var
  DS: TDataSet;
  PatientName, Diagnosis, Medication: string;
begin
  WriteLn('6. Medical Records - Maximum Protection');
  WriteLn('   ======================================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT mr.*, c.first_name, c.last_name FROM medical_records mr ' +
    'JOIN customers c ON mr.customer_id = c.id ORDER BY mr.visit_date');
  try
    // Admin - full access
    SetCurrentUser(1);
    WriteLn(Format('   As %s (role: %s) - FULL medical data:',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));

    DS.First;
    while not DS.EOF do
    begin
      PatientName := DS.FieldByName('first_name').AsString + ' ' + DS.FieldByName('last_name').AsString;
      WriteLn(Format('     Patient: %s | Diagnosis: %s | Medication: %s',
        [PatientName,
         DS.FieldByName('diagnosis').AsString,
         DS.FieldByName('medication').AsString]));
      DS.Next;
    end;

    // Analyst - masked medical data
    WriteLn('');
    SetCurrentUser(3);
    WriteLn(Format('   As %s (role: %s) - masked medical data:',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));

    DS.First;
    while not DS.EOF do
    begin
      PatientName := MaskName(DS.FieldByName('first_name').AsString + ' ' + DS.FieldByName('last_name').AsString);
      Diagnosis := '[REDACTED]';
      Medication := '[REDACTED]';
      WriteLn(Format('     Patient: %s | Diagnosis: %s | Medication: %s',
        [PatientName, Diagnosis, Medication]));
      DS.Next;
    end;

    // Viewer - completely blocked
    WriteLn('');
    SetCurrentUser(5);
    WriteLn(Format('   As %s (role: %s):',
      [VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId])), CurrentRole]));
    WriteLn('     ACCESS DENIED - Medical records require admin role');
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Masking Policies
// =============================================================================
{ Demonstrates viewing and managing active data masking policies. }
procedure DemoPolicies;
var
  DS: TDataSet;
begin
  WriteLn('7. Active Masking Policies');
  WriteLn('   =========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT * FROM masking_policies WHERE is_active = 1 ORDER BY target_table, target_column');
  try
    WriteLn(Format('   %-22s | %-16s | %-12s | %-11s | %s',
      ['Policy', 'Table', 'Column', 'Data Type', 'Min Role']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s | %-16s | %-12s | %-11s | %s',
        [DS.FieldByName('policy_name').AsString,
         DS.FieldByName('target_table').AsString,
         DS.FieldByName('target_column').AsString,
         DS.FieldByName('data_type').AsString,
         DS.FieldByName('min_role_to_unmask').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Dynamic Masking Level
// =============================================================================
{ Shows a single customer record viewed by each of the five roles, demonstrating how masking is applied dynamically based on the minimum role requirement for each field. }
procedure DemoDynamicMasking;
var
  DS: TDataSet;
  Email, Phone, SSN, DOB: string;
  UserName: string;
  I: Integer;
  UserIds: array[1..5] of Integer = (1, 2, 3, 4, 5);
begin
  WriteLn('8. Dynamic Masking by Access Level');
  WriteLn('   =================================');
  WriteLn('');

  // Show first customer through different access levels
  WriteLn('   Customer #1 (John Doe) viewed by each role:');
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery('SELECT * FROM customers WHERE id = 1');
  try
    for I := 1 to 5 do
    begin
      SetCurrentUser(UserIds[I]);
      UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));

      DS.First;

      // Apply masking based on role
      if CanUnmask(CurrentRole, 'data_officer') then
        Email := DS.FieldByName('email').AsString
      else
        Email := MaskEmail(DS.FieldByName('email').AsString);

      if CanUnmask(CurrentRole, 'support') then
        Phone := DS.FieldByName('phone').AsString
      else
        Phone := MaskPhone(DS.FieldByName('phone').AsString);

      if CanUnmask(CurrentRole, 'admin') then
        SSN := DS.FieldByName('ssn').AsString
      else
        SSN := MaskSSN(DS.FieldByName('ssn').AsString);

      if CanUnmask(CurrentRole, 'analyst') then
        DOB := DS.FieldByName('date_of_birth').AsString
      else
        DOB := '****-**-**';

      WriteLn(Format('   [%-12s] %-20s | %-14s | %-11s | %s',
        [CurrentRole, Email, Phone, SSN, DOB]));
    end;
  finally
    DS.Free;
  end;

  WriteLn('   ' + StringOfChar('-', 75));
  WriteLn('   Fields: Email | Phone | SSN | DOB');
  WriteLn('');
end;

// =============================================================================
// Demo: Unmask Audit
// =============================================================================
{ Inserts sample unmask requests into the audit log and displays the trail showing who accessed which unmasked field, for which record, and the stated reason. }
procedure DemoUnmaskAudit;
var
  DS: TDataSet;
begin
  WriteLn('9. Unmask Audit Trail');
  WriteLn('   ====================');
  WriteLn('');

  // Simulate unmask requests
  Conn.ExecuteNonQuery(
    'INSERT INTO unmask_log (user_id, target_table, target_column, record_id, reason) ' +
    'VALUES (1, ''customers'', ''ssn'', 1, ''Customer verification for account recovery'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO unmask_log (user_id, target_table, target_column, record_id, reason) ' +
    'VALUES (2, ''payment_methods'', ''card_number'', 2, ''Payment dispute investigation'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO unmask_log (user_id, target_table, target_column, record_id, reason) ' +
    'VALUES (4, ''customers'', ''phone'', 3, ''Customer callback request'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO unmask_log (user_id, target_table, target_column, record_id, reason) ' +
    'VALUES (1, ''medical_records'', ''diagnosis'', 1, ''Insurance claim verification'')');

  WriteLn('   Recent unmask access:');
  DS := Conn.ExecuteQuery(
    'SELECT ul.*, u.display_name, u.role FROM unmask_log ul ' +
    'JOIN users u ON ul.user_id = u.id ' +
    'ORDER BY ul.timestamp DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s (%s)',
        [DS.FieldByName('role').AsString,
         DS.FieldByName('display_name').AsString,
         DS.FieldByName('timestamp').AsString]));
      WriteLn(Format('       Unmasked: %s.%s (record #%d)',
        [DS.FieldByName('target_table').AsString,
         DS.FieldByName('target_column').AsString,
         DS.FieldByName('record_id').AsInteger]));
      WriteLn(Format('       Reason: %s', [DS.FieldByName('reason').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Masking Statistics
// =============================================================================
{ Demonstrates data masking usage statistics and coverage metrics. }
procedure DemoStatistics;
begin
  WriteLn('10. Data Masking Statistics');
  WriteLn('    =========================');
  WriteLn('');

  WriteLn('    PII field inventory:');
  WriteLn(Format('      Customer email addresses: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM customers WHERE email IS NOT NULL'))]));
  WriteLn(Format('      Phone numbers: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM customers WHERE phone IS NOT NULL'))]));
  WriteLn(Format('      SSN records: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM customers WHERE ssn IS NOT NULL'))]));
  WriteLn(Format('      Credit cards on file: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM payment_methods'))]));
  WriteLn(Format('      Medical records: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM medical_records'))]));

  WriteLn('');
  WriteLn('    Masking coverage:');
  WriteLn(Format('      Active masking policies: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM masking_policies WHERE is_active = 1'))]));
  WriteLn(Format('      Unmask audit entries: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM unmask_log'))]));

  WriteLn('');
  WriteLn('    Role hierarchy (lowest to highest access):');
  WriteLn('      viewer -> support -> analyst -> data_officer -> admin');
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 81: Data Masking (PII Protection) ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;
    DemoMaskingFunctions;
    DemoRoleBasedView;
    DemoPaymentMasking;
    DemoMedicalMasking;
    DemoPolicies;
    DemoDynamicMasking;
    DemoUnmaskAudit;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
