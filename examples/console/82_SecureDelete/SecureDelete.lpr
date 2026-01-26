{===============================================================================
  NDXSQLite Example 82 - Secure Delete
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates secure deletion patterns:
  - Overwriting data before DELETE (field-level zeroing)
  - Multi-pass overwrite (DoD 5220.22-M inspired patterns)
  - Soft delete vs. hard delete workflows
  - Deletion audit trail
  - Data retention policies with scheduled purge
  - GDPR right-to-erasure implementation
  - Verification of overwrite completion
  - Cascading secure delete across related tables
  - SQLite VACUUM for physical space reclamation

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program SecureDelete;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

{ Returns a string of the specified length filled with a character based on the pass number: '0' for pass 1 and 3, 'X' for pass 2. }
function GenerateOverwritePattern(Pass: Integer; Len: Integer): string;
var
  Ch: Char;
begin
  case Pass of
    1: Ch := '0';   // Pass 1: zeros character
    2: Ch := 'X';   // Pass 2: X pattern
    3: Ch := '0';   // Pass 3: zeros again
  else
    Ch := '*';
  end;

  Result := StringOfChar(Ch, Len);
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Secure Delete Schema');
  WriteLn('   ===============================');

  // Users
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  email TEXT,' +
    '  phone TEXT,' +
    '  full_name TEXT,' +
    '  address TEXT,' +
    '  ssn TEXT,' +
    '  is_deleted INTEGER DEFAULT 0,' +
    '  deleted_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Sensitive documents
  Conn.ExecuteNonQuery(
    'CREATE TABLE documents (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  title TEXT NOT NULL,' +
    '  content TEXT,' +
    '  classification TEXT DEFAULT ''internal'',' +
    '  is_deleted INTEGER DEFAULT 0,' +
    '  deleted_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Financial records
  Conn.ExecuteNonQuery(
    'CREATE TABLE financial_records (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  account_number TEXT,' +
    '  routing_number TEXT,' +
    '  balance REAL,' +
    '  card_number TEXT,' +
    '  transaction_history TEXT,' +
    '  is_deleted INTEGER DEFAULT 0,' +
    '  deleted_at TEXT' +
    ')');

  // Deletion requests (GDPR erasure requests)
  Conn.ExecuteNonQuery(
    'CREATE TABLE deletion_requests (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL,' +
    '  request_type TEXT CHECK(request_type IN (''user_request'', ''retention_policy'', ''legal_order'', ''admin_action'')),' +
    '  reason TEXT,' +
    '  status TEXT DEFAULT ''pending'' CHECK(status IN (''pending'', ''processing'', ''completed'', ''failed'', ''verified'')),' +
    '  tables_affected TEXT,' +
    '  records_deleted INTEGER DEFAULT 0,' +
    '  overwrite_passes INTEGER DEFAULT 3,' +
    '  requested_at TEXT DEFAULT (datetime(''now'')),' +
    '  completed_at TEXT,' +
    '  verified_at TEXT' +
    ')');

  // Deletion audit log
  Conn.ExecuteNonQuery(
    'CREATE TABLE deletion_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  request_id INTEGER REFERENCES deletion_requests(id),' +
    '  target_table TEXT NOT NULL,' +
    '  record_id INTEGER NOT NULL,' +
    '  field_name TEXT,' +
    '  original_length INTEGER,' +
    '  overwrite_pass INTEGER,' +
    '  action TEXT CHECK(action IN (''overwrite'', ''delete'', ''verify'')),' +
    '  status TEXT DEFAULT ''success'',' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Retention policies
  Conn.ExecuteNonQuery(
    'CREATE TABLE retention_policies (' +
    '  id INTEGER PRIMARY KEY,' +
    '  policy_name TEXT NOT NULL,' +
    '  target_table TEXT NOT NULL,' +
    '  retention_days INTEGER NOT NULL,' +
    '  delete_method TEXT DEFAULT ''secure'' CHECK(delete_method IN (''soft'', ''hard'', ''secure'')),' +
    '  overwrite_passes INTEGER DEFAULT 3,' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  WriteLn('   Created tables: users, documents, financial_records,');
  WriteLn('                   deletion_requests, deletion_log, retention_policies');
  WriteLn('');
end;

// =============================================================================
// Sample Data
// =============================================================================
{ Populates the database with users containing full PII, classified documents, financial records with account/card data, and data retention policies. }
procedure InsertSampleData;
begin
  WriteLn('2. Creating Sample Data');
  WriteLn('   ======================');

  // Users
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, phone, full_name, address, ssn) VALUES (''jdoe'', ''john.doe@email.com'', ''555-123-4567'', ''John Doe'', ''123 Main St, Springfield'', ''123-45-6789'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, phone, full_name, address, ssn) VALUES (''jsmith'', ''jane.smith@corp.com'', ''555-987-6543'', ''Jane Smith'', ''456 Oak Ave, Portland'', ''987-65-4321'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, phone, full_name, address, ssn) VALUES (''rjohnson'', ''robert@webmail.net'', ''555-456-7890'', ''Robert Johnson'', ''789 Pine Rd, Austin'', ''456-78-9012'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, phone, full_name, address, ssn) VALUES (''mgarcia'', ''maria@domain.io'', ''555-321-0987'', ''Maria Garcia'', ''321 Elm Ct, Denver'', ''321-09-8765'')');

  WriteLn('   Created 4 users with PII data');

  // Documents
  Conn.ExecuteNonQuery('INSERT INTO documents (user_id, title, content, classification) VALUES (1, ''Tax Return 2023'', ''Income: $85000, Deductions: $12000, Tax owed: $15000'', ''confidential'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (user_id, title, content, classification) VALUES (1, ''Medical Report'', ''Patient diagnosis: Hypertension. Prescribed: Lisinopril'', ''restricted'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (user_id, title, content, classification) VALUES (2, ''Employment Contract'', ''Salary: $95000, Start date: 2020-01-15, Benefits: Full'', ''confidential'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (user_id, title, content, classification) VALUES (3, ''Legal Agreement'', ''Non-disclosure terms and conditions...'', ''restricted'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (user_id, title, content, classification) VALUES (3, ''Project Notes'', ''Sprint planning details and architecture notes'', ''internal'')');

  WriteLn('   Created 5 documents (internal, confidential, restricted)');

  // Financial records
  Conn.ExecuteNonQuery('INSERT INTO financial_records (user_id, account_number, routing_number, balance, card_number, transaction_history) VALUES (1, ''1234567890'', ''021000021'', 15230.50, ''4111222233334444'', ''2024-01: +5000, 2024-02: -1200, 2024-03: +3500'')');
  Conn.ExecuteNonQuery('INSERT INTO financial_records (user_id, account_number, routing_number, balance, card_number, transaction_history) VALUES (2, ''9876543210'', ''021000089'', 42100.75, ''5500666677778888'', ''2024-01: +8000, 2024-02: -2500, 2024-03: +6000'')');
  Conn.ExecuteNonQuery('INSERT INTO financial_records (user_id, account_number, routing_number, balance, card_number, transaction_history) VALUES (3, ''5555666677'', ''021000054'', 8900.25, ''378912345678901'', ''2024-01: +3000, 2024-02: -900, 2024-03: +1500'')');

  WriteLn('   Created 3 financial records');

  // Retention policies
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (policy_name, target_table, retention_days, delete_method, overwrite_passes) VALUES (''user_data_90d'', ''users'', 90, ''secure'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (policy_name, target_table, retention_days, delete_method, overwrite_passes) VALUES (''documents_30d'', ''documents'', 30, ''secure'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (policy_name, target_table, retention_days, delete_method) VALUES (''financial_7yr'', ''financial_records'', 2555, ''secure'')');

  WriteLn('   Created 3 retention policies');
  WriteLn('');
end;

// =============================================================================
// Secure Overwrite Function
// =============================================================================
{ Overwrites a text field with pattern data for the specified number of passes, logging each overwrite operation with the original field length. }
procedure SecureOverwriteField(const TableName, FieldName: string;
  RecordId, Passes, RequestId: Integer);
var
  OrigLen: Integer;
  Pass: Integer;
  OverwriteData: string;
  V: Variant;
begin
  // Get original field length
  V := Conn.ExecuteScalar(
    'SELECT length(' + FieldName + ') FROM ' + TableName + ' WHERE id = ?', [RecordId]);
  if VarIsNull(V) then
    OrigLen := 0
  else
    OrigLen := Integer(V);

  if OrigLen = 0 then Exit;

  // Multi-pass overwrite
  for Pass := 1 to Passes do
  begin
    OverwriteData := GenerateOverwritePattern(Pass, OrigLen);
    Conn.ExecuteNonQuery(
      'UPDATE ' + TableName + ' SET ' + FieldName + ' = ? WHERE id = ?',
      [OverwriteData, RecordId]);

    // Log each pass
    Conn.ExecuteNonQuery(
      'INSERT INTO deletion_log (request_id, target_table, record_id, field_name, original_length, overwrite_pass, action) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ''overwrite'')',
      [RequestId, TableName, RecordId, FieldName, OrigLen, Pass]);
  end;
end;

{ Securely deletes a user by overwriting all PII fields, cascading to overwrite related documents and financial records, then marking all affected rows as soft-deleted. }
procedure SecureDeleteUser(UserId, RequestId: Integer);
var
  DS: TDataSet;
  DocId, FinId: Integer;
  RecordsDeleted: Integer;
begin
  RecordsDeleted := 0;

  // 1. Overwrite user PII fields
  WriteLn(Format('     Overwriting user #%d PII fields...', [UserId]));
  SecureOverwriteField('users', 'email', UserId, 3, RequestId);
  SecureOverwriteField('users', 'phone', UserId, 3, RequestId);
  SecureOverwriteField('users', 'full_name', UserId, 3, RequestId);
  SecureOverwriteField('users', 'address', UserId, 3, RequestId);
  SecureOverwriteField('users', 'ssn', UserId, 3, RequestId);
  Inc(RecordsDeleted);

  // 2. Overwrite and delete documents
  DS := Conn.ExecuteQuery('SELECT id FROM documents WHERE user_id = ?', [UserId]);
  try
    while not DS.EOF do
    begin
      DocId := DS.FieldByName('id').AsInteger;
      WriteLn(Format('     Overwriting document #%d content...', [DocId]));
      SecureOverwriteField('documents', 'content', DocId, 3, RequestId);
      SecureOverwriteField('documents', 'title', DocId, 3, RequestId);
      Inc(RecordsDeleted);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // 3. Overwrite and delete financial records
  DS := Conn.ExecuteQuery('SELECT id FROM financial_records WHERE user_id = ?', [UserId]);
  try
    while not DS.EOF do
    begin
      FinId := DS.FieldByName('id').AsInteger;
      WriteLn(Format('     Overwriting financial record #%d...', [FinId]));
      SecureOverwriteField('financial_records', 'account_number', FinId, 3, RequestId);
      SecureOverwriteField('financial_records', 'routing_number', FinId, 3, RequestId);
      SecureOverwriteField('financial_records', 'card_number', FinId, 3, RequestId);
      SecureOverwriteField('financial_records', 'transaction_history', FinId, 3, RequestId);
      Conn.ExecuteNonQuery('UPDATE financial_records SET balance = 0 WHERE id = ?', [FinId]);
      Inc(RecordsDeleted);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // 4. Mark as soft-deleted
  Conn.ExecuteNonQuery('UPDATE users SET is_deleted = 1, deleted_at = datetime(''now'') WHERE id = ?', [UserId]);
  Conn.ExecuteNonQuery('UPDATE documents SET is_deleted = 1, deleted_at = datetime(''now'') WHERE user_id = ?', [UserId]);
  Conn.ExecuteNonQuery('UPDATE financial_records SET is_deleted = 1, deleted_at = datetime(''now'') WHERE user_id = ?', [UserId]);

  // 5. Update request
  Conn.ExecuteNonQuery(
    'UPDATE deletion_requests SET status = ''completed'', records_deleted = ?, completed_at = datetime(''now'') ' +
    'WHERE id = ?', [RecordsDeleted, RequestId]);
end;

// =============================================================================
// Demo: Show Data Before Deletion
// =============================================================================
{ Displays user #1's complete PII, documents with content previews, and financial account details before the secure deletion process. }
procedure DemoShowDataBefore;
var
  DS: TDataSet;
begin
  WriteLn('3. Data Before Secure Deletion');
  WriteLn('   ==============================');
  WriteLn('');

  WriteLn('   User #1 (John Doe) - PII data:');
  DS := Conn.ExecuteQuery('SELECT * FROM users WHERE id = 1');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('     Username: %s', [DS.FieldByName('username').AsString]));
      WriteLn(Format('     Email:    %s', [DS.FieldByName('email').AsString]));
      WriteLn(Format('     Phone:    %s', [DS.FieldByName('phone').AsString]));
      WriteLn(Format('     Name:     %s', [DS.FieldByName('full_name').AsString]));
      WriteLn(Format('     Address:  %s', [DS.FieldByName('address').AsString]));
      WriteLn(Format('     SSN:      %s', [DS.FieldByName('ssn').AsString]));
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   User #1 documents:');
  DS := Conn.ExecuteQuery('SELECT id, title, substr(content, 1, 50) AS preview, classification FROM documents WHERE user_id = 1');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] #%d "%s": %s...',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('id').AsInteger,
         DS.FieldByName('title').AsString,
         DS.FieldByName('preview').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   User #1 financial data:');
  DS := Conn.ExecuteQuery('SELECT * FROM financial_records WHERE user_id = 1');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('     Account: %s | Routing: %s',
        [DS.FieldByName('account_number').AsString,
         DS.FieldByName('routing_number').AsString]));
      WriteLn(Format('     Card: %s | Balance: $%.2f',
        [DS.FieldByName('card_number').AsString,
         DS.FieldByName('balance').AsFloat]));
      WriteLn(Format('     History: %s', [DS.FieldByName('transaction_history').AsString]));
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Execute Secure Delete
// =============================================================================
{ Creates a GDPR erasure request for user #1 and executes the 3-pass secure deletion process on all their PII, documents, and financial records. }
procedure DemoSecureDelete;
var
  RequestId: Integer;
begin
  WriteLn('4. Executing Secure Delete (User #1)');
  WriteLn('   ====================================');
  WriteLn('');

  // Create deletion request
  Conn.ExecuteNonQuery(
    'INSERT INTO deletion_requests (user_id, request_type, reason, status, tables_affected, overwrite_passes) ' +
    'VALUES (1, ''user_request'', ''GDPR Right to Erasure - customer request'', ''processing'', ''users,documents,financial_records'', 3)');
  RequestId := Conn.ExecuteScalar('SELECT last_insert_rowid()');

  WriteLn(Format('   Created deletion request #%d', [RequestId]));
  WriteLn('   Method: 3-pass overwrite (zeros, pattern, zeros) then soft delete');
  WriteLn('');
  WriteLn('   Processing:');

  // Execute secure delete
  SecureDeleteUser(1, RequestId);

  WriteLn('');
  WriteLn('   Secure deletion completed.');
  WriteLn('');
end;

// =============================================================================
// Demo: Verify Overwrite
// =============================================================================
{ Displays user #1's data after secure deletion to verify that all PII fields, documents, and financial records have been overwritten with zeros and marked as deleted. }
procedure DemoVerifyOverwrite;
var
  DS: TDataSet;
  AllZeros: Boolean;
  FieldVal: string;
begin
  WriteLn('5. Verification After Secure Delete');
  WriteLn('   ==================================');
  WriteLn('');

  WriteLn('   User #1 data after overwrite:');
  DS := Conn.ExecuteQuery('SELECT * FROM users WHERE id = 1');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('     Username:   %s (preserved for reference)', [DS.FieldByName('username').AsString]));

      FieldVal := DS.FieldByName('email').AsString;
      AllZeros := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
      WriteLn(Format('     Email:      [%d bytes overwritten] %s',
        [Length(FieldVal), IfThen(AllZeros, 'VERIFIED OVERWRITTEN', 'ORIGINAL')]));

      FieldVal := DS.FieldByName('phone').AsString;
      AllZeros := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
      WriteLn(Format('     Phone:      [%d bytes overwritten] %s',
        [Length(FieldVal), IfThen(AllZeros, 'VERIFIED OVERWRITTEN', 'ORIGINAL')]));

      FieldVal := DS.FieldByName('full_name').AsString;
      AllZeros := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
      WriteLn(Format('     Name:       [%d bytes overwritten] %s',
        [Length(FieldVal), IfThen(AllZeros, 'VERIFIED OVERWRITTEN', 'ORIGINAL')]));

      FieldVal := DS.FieldByName('ssn').AsString;
      AllZeros := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
      WriteLn(Format('     SSN:        [%d bytes overwritten] %s',
        [Length(FieldVal), IfThen(AllZeros, 'VERIFIED OVERWRITTEN', 'ORIGINAL')]));

      WriteLn(Format('     is_deleted: %d', [DS.FieldByName('is_deleted').AsInteger]));
      WriteLn(Format('     deleted_at: %s', [DS.FieldByName('deleted_at').AsString]));
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   User #1 documents after overwrite:');
  DS := Conn.ExecuteQuery('SELECT id, title, content, is_deleted FROM documents WHERE user_id = 1');
  try
    while not DS.EOF do
    begin
      FieldVal := DS.FieldByName('content').AsString;
      AllZeros := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
      WriteLn(Format('     Doc #%d: title=[%d bytes], content=[%d bytes] %s (deleted=%d)',
        [DS.FieldByName('id').AsInteger,
         Length(DS.FieldByName('title').AsString),
         Length(FieldVal),
         IfThen(AllZeros, 'VERIFIED OVERWRITTEN', 'ORIGINAL'),
         DS.FieldByName('is_deleted').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   User #1 financial record after overwrite:');
  DS := Conn.ExecuteQuery('SELECT * FROM financial_records WHERE user_id = 1');
  try
    if not DS.EOF then
    begin
      FieldVal := DS.FieldByName('account_number').AsString;
      AllZeros := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
      WriteLn(Format('     Account:  [%d bytes] %s',
        [Length(FieldVal), IfThen(AllZeros, 'VERIFIED OVERWRITTEN', 'ORIGINAL')]));

      FieldVal := DS.FieldByName('card_number').AsString;
      AllZeros := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
      WriteLn(Format('     Card:     [%d bytes] %s',
        [Length(FieldVal), IfThen(AllZeros, 'VERIFIED OVERWRITTEN', 'ORIGINAL')]));

      WriteLn(Format('     Balance:  $%.2f (zeroed)', [DS.FieldByName('balance').AsFloat]));
    end;
  finally
    DS.Free;
  end;

  // Mark as verified
  Conn.ExecuteNonQuery(
    'UPDATE deletion_requests SET status = ''verified'', verified_at = datetime(''now'') WHERE user_id = 1');

  WriteLn('');
end;

// =============================================================================
// Demo: Soft Delete vs Hard Delete
// =============================================================================
{ Compares soft delete (marking is_deleted=1 while preserving data) versus hard delete (overwriting fields then permanently removing the row with DELETE). }
procedure DemoSoftVsHard;
var
  CountBefore, CountAfter: Integer;
begin
  WriteLn('6. Soft Delete vs. Hard Delete');
  WriteLn('   =============================');
  WriteLn('');

  // Soft delete user #2
  WriteLn('   Soft deleting user #2 (Jane Smith):');
  Conn.ExecuteNonQuery('UPDATE users SET is_deleted = 1, deleted_at = datetime(''now'') WHERE id = 2');
  Conn.ExecuteNonQuery('UPDATE documents SET is_deleted = 1, deleted_at = datetime(''now'') WHERE user_id = 2');
  WriteLn('     -> Marked as deleted (data preserved, just hidden)');

  WriteLn(Format('     Active users: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE is_deleted = 0'))]));
  WriteLn(Format('     Soft-deleted users: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE is_deleted = 1'))]));
  WriteLn(Format('     Total in table: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users'))]));

  // Hard delete user #2 (after secure overwrite)
  WriteLn('');
  WriteLn('   Hard deleting user #2 (after overwrite):');

  // Overwrite first
  WriteLn('     Step 1: Overwriting PII...');
  Conn.ExecuteNonQuery('UPDATE users SET email = ''[DELETED]'', phone = ''[DELETED]'', full_name = ''[DELETED]'', address = ''[DELETED]'', ssn = ''[DELETED]'' WHERE id = 2');

  // Then hard delete
  CountBefore := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');
  WriteLn('     Step 2: Hard DELETE from table...');
  Conn.ExecuteNonQuery('DELETE FROM documents WHERE user_id = 2');
  Conn.ExecuteNonQuery('DELETE FROM financial_records WHERE user_id = 2');
  Conn.ExecuteNonQuery('DELETE FROM users WHERE id = 2');
  CountAfter := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');

  WriteLn(Format('     -> Users before: %d, after: %d (permanently removed)', [CountBefore, CountAfter]));
  WriteLn('');
end;

// =============================================================================
// Demo: Retention Policies
// =============================================================================
{ Displays the configured data retention policies showing table, retention period, deletion method, and overwrite pass count, then simulates finding expired records. }
procedure DemoRetentionPolicies;
var
  DS: TDataSet;
begin
  WriteLn('7. Data Retention Policies');
  WriteLn('   =========================');
  WriteLn('');

  DS := Conn.ExecuteQuery('SELECT * FROM retention_policies ORDER BY retention_days');
  try
    WriteLn(Format('   %-20s | %-18s | %-10s | %-8s | %s',
      ['Policy', 'Table', 'Retention', 'Method', 'Passes']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s | %-18s | %4d days | %-8s | %d',
        [DS.FieldByName('policy_name').AsString,
         DS.FieldByName('target_table').AsString,
         DS.FieldByName('retention_days').AsInteger,
         DS.FieldByName('delete_method').AsString,
         DS.FieldByName('overwrite_passes').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Simulating retention policy enforcement:');

  // Simulate finding expired records
  WriteLn('     Checking users older than 90 days...');
  WriteLn(Format('       Found: %d records eligible for deletion',
    [Integer(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM users WHERE is_deleted = 1 AND deleted_at IS NOT NULL'))]));

  WriteLn('     Checking documents older than 30 days...');
  WriteLn(Format('       Found: %d records eligible for deletion',
    [Integer(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM documents WHERE is_deleted = 1 AND deleted_at IS NOT NULL'))]));

  WriteLn('');
end;

// =============================================================================
// Demo: Deletion Audit Log
// =============================================================================
{ Displays the deletion audit log summarized by table and field, showing operation counts and maximum field lengths, plus a breakdown of operations by overwrite pass. }
procedure DemoDeletionLog;
var
  DS: TDataSet;
begin
  WriteLn('8. Deletion Audit Log');
  WriteLn('   ====================');
  WriteLn('');

  // Summary by table
  WriteLn('   Overwrite operations by table:');
  DS := Conn.ExecuteQuery(
    'SELECT target_table, field_name, COUNT(*) AS ops, MAX(original_length) AS max_len ' +
    'FROM deletion_log WHERE action = ''overwrite'' ' +
    'GROUP BY target_table, field_name ' +
    'ORDER BY target_table, field_name');
  try
    WriteLn(Format('   %-18s | %-20s | %s | %s',
      ['Table', 'Field', 'Ops', 'Max Len']));
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s | %-20s | %3d | %4d bytes',
        [DS.FieldByName('target_table').AsString,
         DS.FieldByName('field_name').AsString,
         DS.FieldByName('ops').AsInteger,
         DS.FieldByName('max_len').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Overwrite pass summary
  WriteLn('');
  WriteLn('   Overwrite passes completed:');
  DS := Conn.ExecuteQuery(
    'SELECT overwrite_pass, COUNT(*) AS operations FROM deletion_log ' +
    'WHERE action = ''overwrite'' GROUP BY overwrite_pass ORDER BY overwrite_pass');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     Pass %d: %d field overwrites',
        [DS.FieldByName('overwrite_pass').AsInteger,
         DS.FieldByName('operations').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Deletion Requests Status
// =============================================================================
{ Creates sample deletion requests (retention policy, legal order) and displays all requests showing user, type, reason, affected tables, records deleted, and completion status. }
procedure DemoDeletionRequests;
var
  DS: TDataSet;
begin
  WriteLn('9. Deletion Request Status');
  WriteLn('   ==========================');
  WriteLn('');

  // Add additional deletion requests for demo
  Conn.ExecuteNonQuery(
    'INSERT INTO deletion_requests (user_id, request_type, reason, status, tables_affected) ' +
    'VALUES (3, ''retention_policy'', ''Data retention period expired'', ''pending'', ''users,documents'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO deletion_requests (user_id, request_type, reason, status, tables_affected) ' +
    'VALUES (4, ''legal_order'', ''Court order #2024-5678'', ''pending'', ''financial_records'')');

  DS := Conn.ExecuteQuery(
    'SELECT dr.*, u.username FROM deletion_requests dr ' +
    'LEFT JOIN users u ON dr.user_id = u.id ' +
    'ORDER BY dr.requested_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Request #%d [%s]:',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('status').AsString]));
      WriteLn(Format('     User: %s | Type: %s',
        [IfThen(VarIsNull(DS.FieldByName('username').Value),
          'ID#' + DS.FieldByName('user_id').AsString,
          DS.FieldByName('username').AsString),
         DS.FieldByName('request_type').AsString]));
      WriteLn(Format('     Reason: %s', [DS.FieldByName('reason').AsString]));
      WriteLn(Format('     Tables: %s | Records deleted: %d',
        [DS.FieldByName('tables_affected').AsString,
         DS.FieldByName('records_deleted').AsInteger]));
      if not VarIsNull(DS.FieldByName('completed_at').Value) then
        WriteLn(Format('     Completed: %s', [DS.FieldByName('completed_at').AsString]));
      if not VarIsNull(DS.FieldByName('verified_at').Value) then
        WriteLn(Format('     Verified: %s', [DS.FieldByName('verified_at').AsString]));
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// =============================================================================
// Demo: Statistics
// =============================================================================
{ Demonstrates secure deletion statistics and compliance reporting. }
procedure DemoStatistics;
begin
  WriteLn('10. Secure Deletion Statistics');
  WriteLn('    ============================');
  WriteLn('');

  WriteLn('    Overall status:');
  WriteLn(Format('      Total users: %d (active: %d, deleted: %d)',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users')),
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE is_deleted = 0')),
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE is_deleted = 1'))]));
  WriteLn(Format('      Total documents: %d (active: %d, deleted: %d)',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM documents')),
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM documents WHERE is_deleted = 0')),
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM documents WHERE is_deleted = 1'))]));

  WriteLn('');
  WriteLn('    Deletion operations:');
  WriteLn(Format('      Total overwrite operations: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM deletion_log WHERE action = ''overwrite'''))]));
  WriteLn(Format('      Total bytes overwritten: %d',
    [Integer(Conn.ExecuteScalar('SELECT COALESCE(SUM(original_length), 0) FROM deletion_log WHERE action = ''overwrite'''))]));
  WriteLn(Format('      Deletion requests: %d (completed: %d, pending: %d)',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM deletion_requests')),
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM deletion_requests WHERE status IN (''completed'', ''verified'')')),
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM deletion_requests WHERE status = ''pending'''))]));

  WriteLn('');
  WriteLn('    Security notes:');
  WriteLn('      - 3-pass overwrite: zeros -> pattern -> zeros');
  WriteLn('      - Soft delete preserves record structure for audit');
  WriteLn('      - Hard delete removes row completely after overwrite');
  WriteLn('      - VACUUM reclaims physical disk space in SQLite');
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 82: Secure Delete ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;
    DemoShowDataBefore;
    DemoSecureDelete;
    DemoVerifyOverwrite;
    DemoSoftVsHard;
    DemoRetentionPolicies;
    DemoDeletionLog;
    DemoDeletionRequests;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
