{===============================================================================
  NDXSQLite Example 130 - Consent Management
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Data subject registration and tracking
  - Purpose-based consent grants
  - Consent withdrawal and history
  - Data Subject Access Requests (DSAR)
  - Processing activity records

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ConsentManagement;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Data subjects (users)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS data_subjects (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  subject_id TEXT NOT NULL UNIQUE,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT NOT NULL,' +
    '  registered_at TEXT NOT NULL' +
    ')'
  );

  // Processing purposes
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS purposes (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  purpose_code TEXT NOT NULL UNIQUE,' +
    '  purpose_name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  legal_basis TEXT NOT NULL,' +
    '  mandatory INTEGER DEFAULT 0' +
    ')'
  );

  // Current consent state
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS consents (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  subject_id TEXT NOT NULL,' +
    '  purpose_code TEXT NOT NULL,' +
    '  status TEXT NOT NULL,' +        // granted, withdrawn, expired
    '  granted_at TEXT,' +
    '  withdrawn_at TEXT,' +
    '  expires_at TEXT,' +
    '  version INTEGER DEFAULT 1,' +
    '  UNIQUE(subject_id, purpose_code)' +
    ')'
  );

  // Consent change history (audit trail)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS consent_history (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  subject_id TEXT NOT NULL,' +
    '  purpose_code TEXT NOT NULL,' +
    '  action TEXT NOT NULL,' +          // grant, withdraw, renew, expire
    '  previous_status TEXT,' +
    '  new_status TEXT NOT NULL,' +
    '  reason TEXT,' +
    '  changed_at TEXT NOT NULL,' +
    '  ip_address TEXT,' +
    '  user_agent TEXT' +
    ')'
  );

  // Processing activity records
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS processing_records (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  subject_id TEXT NOT NULL,' +
    '  purpose_code TEXT NOT NULL,' +
    '  activity TEXT NOT NULL,' +
    '  data_categories TEXT,' +
    '  processor TEXT,' +
    '  processed_at TEXT NOT NULL,' +
    '  consent_verified INTEGER DEFAULT 0' +
    ')'
  );

  // Data subject access requests
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS dsar_requests (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  subject_id TEXT NOT NULL,' +
    '  request_type TEXT NOT NULL,' +    // access, rectification, erasure, portability
    '  status TEXT NOT NULL,' +           // submitted, in_progress, completed, rejected
    '  submitted_at TEXT NOT NULL,' +
    '  completed_at TEXT,' +
    '  response_details TEXT' +
    ')'
  );
end;

{ Inserts sample data for demonstrations. }
procedure InsertSampleData;
begin
  // Data subjects
  Conn.ExecuteNonQuery('INSERT INTO data_subjects (subject_id, name, email, registered_at) VALUES (''SUB-001'', ''Alice Martin'', ''alice@example.com'', ''2024-01-10 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO data_subjects (subject_id, name, email, registered_at) VALUES (''SUB-002'', ''Bob Wilson'', ''bob@example.com'', ''2024-01-12 14:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO data_subjects (subject_id, name, email, registered_at) VALUES (''SUB-003'', ''Carol Davis'', ''carol@example.com'', ''2024-01-15 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO data_subjects (subject_id, name, email, registered_at) VALUES (''SUB-004'', ''Dave Brown'', ''dave@example.com'', ''2024-02-01 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO data_subjects (subject_id, name, email, registered_at) VALUES (''SUB-005'', ''Eve Taylor'', ''eve@example.com'', ''2024-02-10 16:00:00'')');

  // Processing purposes
  Conn.ExecuteNonQuery('INSERT INTO purposes (purpose_code, purpose_name, description, legal_basis, mandatory) VALUES (''MARKETING'', ''Marketing Communications'', ''Send promotional emails and offers'', ''consent'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO purposes (purpose_code, purpose_name, description, legal_basis, mandatory) VALUES (''ANALYTICS'', ''Usage Analytics'', ''Track usage patterns to improve service'', ''legitimate_interest'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO purposes (purpose_code, purpose_name, description, legal_basis, mandatory) VALUES (''PROFILING'', ''User Profiling'', ''Build user profiles for personalization'', ''consent'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO purposes (purpose_code, purpose_name, description, legal_basis, mandatory) VALUES (''THIRD_PARTY'', ''Third-Party Sharing'', ''Share data with partner companies'', ''consent'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO purposes (purpose_code, purpose_name, description, legal_basis, mandatory) VALUES (''SERVICE'', ''Service Delivery'', ''Process data to deliver core service'', ''contract'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO purposes (purpose_code, purpose_name, description, legal_basis, mandatory) VALUES (''PERSONALIZE'', ''Content Personalization'', ''Customize content based on preferences'', ''consent'', 0)');

  // Consents (current state)
  // Alice: consented to marketing, analytics, profiling; not third-party
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-001'', ''MARKETING'', ''granted'', ''2024-01-10 09:00:00'', ''2025-01-10 09:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-001'', ''ANALYTICS'', ''granted'', ''2024-01-10 09:00:00'', ''2025-01-10 09:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-001'', ''PROFILING'', ''granted'', ''2024-01-10 09:00:00'', ''2025-01-10 09:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, withdrawn_at, version) VALUES (''SUB-001'', ''THIRD_PARTY'', ''withdrawn'', ''2024-01-10 09:00:00'', ''2024-03-15 14:00:00'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-001'', ''PERSONALIZE'', ''granted'', ''2024-01-10 09:00:00'', ''2025-01-10 09:00:00'', 1)');

  // Bob: minimal consent (analytics only, withdrew marketing)
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, withdrawn_at, version) VALUES (''SUB-002'', ''MARKETING'', ''withdrawn'', ''2024-01-12 14:30:00'', ''2024-02-20 10:00:00'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-002'', ''ANALYTICS'', ''granted'', ''2024-01-12 14:30:00'', ''2025-01-12 14:30:00'', 1)');

  // Carol: all consents granted
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-003'', ''MARKETING'', ''granted'', ''2024-01-15 11:00:00'', ''2025-01-15 11:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-003'', ''ANALYTICS'', ''granted'', ''2024-01-15 11:00:00'', ''2025-01-15 11:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-003'', ''PROFILING'', ''granted'', ''2024-01-15 11:00:00'', ''2025-01-15 11:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-003'', ''THIRD_PARTY'', ''granted'', ''2024-01-15 11:00:00'', ''2025-01-15 11:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-003'', ''PERSONALIZE'', ''granted'', ''2024-01-15 11:00:00'', ''2025-01-15 11:00:00'', 1)');

  // Dave: withdrew all optional consents
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, withdrawn_at, version) VALUES (''SUB-004'', ''MARKETING'', ''withdrawn'', ''2024-02-01 08:00:00'', ''2024-03-01 09:00:00'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, withdrawn_at, version) VALUES (''SUB-004'', ''ANALYTICS'', ''withdrawn'', ''2024-02-01 08:00:00'', ''2024-03-01 09:00:00'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, withdrawn_at, version) VALUES (''SUB-004'', ''PROFILING'', ''withdrawn'', ''2024-02-01 08:00:00'', ''2024-03-01 09:00:00'', 2)');

  // Eve: marketing and personalization only
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-005'', ''MARKETING'', ''granted'', ''2024-02-10 16:00:00'', ''2025-02-10 16:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO consents (subject_id, purpose_code, status, granted_at, expires_at, version) VALUES (''SUB-005'', ''PERSONALIZE'', ''granted'', ''2024-02-10 16:00:00'', ''2025-02-10 16:00:00'', 1)');

  // Consent history (audit trail)
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-001'', ''MARKETING'', ''grant'', NULL, ''granted'', ''Initial registration'', ''2024-01-10 09:00:00'', ''192.168.1.10'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-001'', ''THIRD_PARTY'', ''grant'', NULL, ''granted'', ''Initial registration'', ''2024-01-10 09:00:00'', ''192.168.1.10'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-001'', ''THIRD_PARTY'', ''withdraw'', ''granted'', ''withdrawn'', ''Privacy concerns'', ''2024-03-15 14:00:00'', ''192.168.1.10'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-002'', ''MARKETING'', ''grant'', NULL, ''granted'', ''Initial registration'', ''2024-01-12 14:30:00'', ''10.0.0.5'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-002'', ''MARKETING'', ''withdraw'', ''granted'', ''withdrawn'', ''Too many emails'', ''2024-02-20 10:00:00'', ''10.0.0.5'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-004'', ''MARKETING'', ''grant'', NULL, ''granted'', ''Initial registration'', ''2024-02-01 08:00:00'', ''172.16.0.20'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-004'', ''MARKETING'', ''withdraw'', ''granted'', ''withdrawn'', ''Data minimization request'', ''2024-03-01 09:00:00'', ''172.16.0.20'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-004'', ''ANALYTICS'', ''withdraw'', ''granted'', ''withdrawn'', ''Data minimization request'', ''2024-03-01 09:00:00'', ''172.16.0.20'')');
  Conn.ExecuteNonQuery('INSERT INTO consent_history (subject_id, purpose_code, action, previous_status, new_status, reason, changed_at, ip_address) VALUES (''SUB-004'', ''PROFILING'', ''withdraw'', ''granted'', ''withdrawn'', ''Data minimization request'', ''2024-03-01 09:00:00'', ''172.16.0.20'')');

  // Processing records
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-001'', ''MARKETING'', ''send_newsletter'', ''email,name'', ''MailService'', ''2024-02-01 10:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-001'', ''ANALYTICS'', ''page_tracking'', ''browsing,clicks'', ''AnalyticsEngine'', ''2024-02-05 12:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-001'', ''PROFILING'', ''build_profile'', ''browsing,purchases'', ''ProfileBuilder'', ''2024-02-10 08:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-002'', ''ANALYTICS'', ''page_tracking'', ''browsing,clicks'', ''AnalyticsEngine'', ''2024-02-15 09:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-003'', ''MARKETING'', ''send_newsletter'', ''email,name'', ''MailService'', ''2024-02-01 10:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-003'', ''THIRD_PARTY'', ''share_profile'', ''name,email,preferences'', ''PartnerAPI'', ''2024-02-20 14:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-003'', ''PERSONALIZE'', ''recommend_content'', ''browsing,preferences'', ''RecommendEngine'', ''2024-03-01 11:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-005'', ''MARKETING'', ''send_promo'', ''email,name'', ''MailService'', ''2024-03-10 09:00:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO processing_records (subject_id, purpose_code, activity, data_categories, processor, processed_at, consent_verified) VALUES (''SUB-005'', ''PERSONALIZE'', ''recommend_content'', ''browsing,preferences'', ''RecommendEngine'', ''2024-03-15 15:00:00'', 1)');

  // DSAR requests
  Conn.ExecuteNonQuery('INSERT INTO dsar_requests (subject_id, request_type, status, submitted_at, completed_at, response_details) VALUES (''SUB-001'', ''access'', ''completed'', ''2024-03-01 10:00:00'', ''2024-03-05 14:00:00'', ''Full data export provided (JSON)'')');
  Conn.ExecuteNonQuery('INSERT INTO dsar_requests (subject_id, request_type, status, submitted_at, completed_at, response_details) VALUES (''SUB-002'', ''access'', ''completed'', ''2024-03-10 09:00:00'', ''2024-03-12 11:00:00'', ''Data summary provided'')');
  Conn.ExecuteNonQuery('INSERT INTO dsar_requests (subject_id, request_type, status, submitted_at, response_details) VALUES (''SUB-004'', ''erasure'', ''in_progress'', ''2024-03-20 08:00:00'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO dsar_requests (subject_id, request_type, status, submitted_at, completed_at, response_details) VALUES (''SUB-004'', ''portability'', ''completed'', ''2024-03-05 16:00:00'', ''2024-03-08 10:00:00'', ''Exported to CSV format'')');
  Conn.ExecuteNonQuery('INSERT INTO dsar_requests (subject_id, request_type, status, submitted_at, response_details) VALUES (''SUB-005'', ''rectification'', ''submitted'', ''2024-03-25 14:00:00'', NULL)');
end;

// ============================================================
// Demo Sections
// ============================================================

{ Lists all registered data subjects with their IDs, names, emails, and registration timestamps. }
procedure Demo1_DataSubjects;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Data Subjects ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM data_subjects');
  try
    WriteLn(Format('   Registered data subjects: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   %-10s %-15s %-22s %s', ['ID', 'Name', 'Email', 'Registered']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery('SELECT * FROM data_subjects ORDER BY registered_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-15s %-22s %s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('name').AsString,
        DS.FieldByName('email').AsString,
        DS.FieldByName('registered_at').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates viewing processing purposes with their legal basis and settings. }
procedure Demo2_Purposes;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Processing Purposes ===');
  WriteLn;
  WriteLn(Format('   %-14s %-25s %-20s %-10s', ['Code', 'Name', 'Legal Basis', 'Mandatory']));
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery('SELECT * FROM purposes ORDER BY purpose_code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %-25s %-20s %-10s', [
        DS.FieldByName('purpose_code').AsString,
        DS.FieldByName('purpose_name').AsString,
        DS.FieldByName('legal_basis').AsString,
        BoolToStr(DS.FieldByName('mandatory').AsInteger = 1, 'YES', 'no')
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Legal bases: consent, legitimate_interest, contract');
  WriteLn;
end;

{ Displays all active (granted) consents per subject with purpose codes, grant dates, and expiration dates. }
procedure Demo3_ConsentGrants;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Consent Grants ===');
  WriteLn;
  WriteLn('   Active consents per subject:');
  WriteLn(Format('   %-10s %-14s %-10s %-22s %-22s', ['Subject', 'Purpose', 'Status', 'Granted', 'Expires']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery(
    'SELECT subject_id, purpose_code, status, granted_at, ' +
    '  COALESCE(expires_at, ''N/A'') as expires ' +
    'FROM consents WHERE status = ''granted'' ' +
    'ORDER BY subject_id, purpose_code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %-10s %-22s %-22s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('purpose_code').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('granted_at').AsString,
        DS.FieldByName('expires').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Builds a matrix showing each subject's consent status (g/w/-) for all processing purposes. }
procedure Demo4_ConsentStatus;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Consent Status Matrix ===');
  WriteLn;
  WriteLn('   Subject consent overview (G=granted, W=withdrawn, -=not set):');
  WriteLn;

  // Build a matrix of subjects vs purposes
  WriteLn(Format('   %-10s %-10s %-10s %-10s %-12s %-12s', [
    'Subject', 'MARKET', 'ANALYTIC', 'PROFILE', 'THIRD_PARTY', 'PERSONALIZE']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT ds.subject_id,' +
    '  COALESCE((SELECT SUBSTR(c.status,1,1) FROM consents c WHERE c.subject_id = ds.subject_id AND c.purpose_code = ''MARKETING''), ''-'') as marketing,' +
    '  COALESCE((SELECT SUBSTR(c.status,1,1) FROM consents c WHERE c.subject_id = ds.subject_id AND c.purpose_code = ''ANALYTICS''), ''-'') as analytics,' +
    '  COALESCE((SELECT SUBSTR(c.status,1,1) FROM consents c WHERE c.subject_id = ds.subject_id AND c.purpose_code = ''PROFILING''), ''-'') as profiling,' +
    '  COALESCE((SELECT SUBSTR(c.status,1,1) FROM consents c WHERE c.subject_id = ds.subject_id AND c.purpose_code = ''THIRD_PARTY''), ''-'') as third_party,' +
    '  COALESCE((SELECT SUBSTR(c.status,1,1) FROM consents c WHERE c.subject_id = ds.subject_id AND c.purpose_code = ''PERSONALIZE''), ''-'') as personalize ' +
    'FROM data_subjects ds ORDER BY ds.subject_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-10s %-10s %-10s %-12s %-12s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('marketing').AsString,
        DS.FieldByName('analytics').AsString,
        DS.FieldByName('profiling').AsString,
        DS.FieldByName('third_party').AsString,
        DS.FieldByName('personalize').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Legend: g=granted, w=withdrawn, -=never set');
  WriteLn;
end;

{ Demonstrates consent withdrawal tracking with reasons and timestamps. }
procedure Demo5_Withdrawals;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Consent Withdrawals ===');
  WriteLn;
  WriteLn('   Withdrawn consents with reasons:');
  WriteLn(Format('   %-10s %-14s %-22s %-25s', ['Subject', 'Purpose', 'Withdrawn At', 'Reason']));
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery(
    'SELECT ch.subject_id, ch.purpose_code, ch.changed_at, ch.reason ' +
    'FROM consent_history ch ' +
    'WHERE ch.action = ''withdraw'' ' +
    'ORDER BY ch.changed_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %-22s %-25s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('purpose_code').AsString,
        DS.FieldByName('changed_at').AsString,
        DS.FieldByName('reason').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Withdrawal rate by purpose:');

  DS := Conn.ExecuteQuery(
    'SELECT purpose_code, ' +
    '  COUNT(*) as total, ' +
    '  SUM(CASE WHEN status = ''withdrawn'' THEN 1 ELSE 0 END) as withdrawn ' +
    'FROM consents ' +
    'GROUP BY purpose_code ORDER BY purpose_code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %d/%d withdrawn', [
        DS.FieldByName('purpose_code').AsString,
        DS.FieldByName('withdrawn').AsInteger,
        DS.FieldByName('total').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists all DSAR requests (access, erasure, portability, rectification) with status and checks 30-day compliance deadline. }
procedure Demo6_DSARRequests;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Data Subject Access Requests (DSAR) ===');
  WriteLn;
  WriteLn(Format('   %-10s %-14s %-12s %-22s %-22s', ['Subject', 'Type', 'Status', 'Submitted', 'Completed']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery('SELECT * FROM dsar_requests ORDER BY submitted_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %-12s %-22s %-22s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('request_type').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('submitted_at').AsString,
        VarToStr(DS.FieldByName('completed_at').Value)
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   DSAR compliance check (30-day deadline):');

  DS := Conn.ExecuteQuery(
    'SELECT subject_id, request_type, status, submitted_at, completed_at,' +
    '  CASE ' +
    '    WHEN status = ''completed'' THEN ' +
    '      CAST(julianday(completed_at) - julianday(submitted_at) AS INTEGER) ' +
    '    ELSE ' +
    '      CAST(julianday(''2024-04-01'') - julianday(submitted_at) AS INTEGER) ' +
    '  END as days_elapsed ' +
    'FROM dsar_requests ORDER BY submitted_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %d days %s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('request_type').AsString,
        DS.FieldByName('days_elapsed').AsInteger,
        BoolToStr(DS.FieldByName('days_elapsed').AsInteger <= 30, '[OK]', '[OVERDUE!]')
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows processing activity records with subject, purpose, activity type, processor name, and consent verification status. }
procedure Demo7_ProcessingRecords;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Processing Activity Records ===');
  WriteLn;
  WriteLn(Format('   %-10s %-14s %-20s %-18s %-8s', ['Subject', 'Purpose', 'Activity', 'Processor', 'Consent']));
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery('SELECT * FROM processing_records ORDER BY processed_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %-20s %-18s %-8s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('purpose_code').AsString,
        DS.FieldByName('activity').AsString,
        DS.FieldByName('processor').AsString,
        BoolToStr(DS.FieldByName('consent_verified').AsInteger = 1, 'YES', 'NO')
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Processing by data category:');
  // Show distinct data categories processed
  DS := Conn.ExecuteQuery(
    'SELECT data_categories, COUNT(*) as cnt ' +
    'FROM processing_records GROUP BY data_categories ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-30s %d records', [
        DS.FieldByName('data_categories').AsString,
        DS.FieldByName('cnt').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays full consent change audit trail showing actions, status transitions, timestamps, and withdrawal reason summary. }
procedure Demo8_ConsentHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Consent History (Audit Trail) ===');
  WriteLn;
  WriteLn(Format('   %-10s %-14s %-10s %-10s %-10s %-22s', ['Subject', 'Purpose', 'Action', 'From', 'To', 'Changed At']));
  WriteLn('   ' + StringOfChar('-', 80));

  DS := Conn.ExecuteQuery('SELECT * FROM consent_history ORDER BY changed_at, subject_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %-10s %-10s %-10s %-22s', [
        DS.FieldByName('subject_id').AsString,
        DS.FieldByName('purpose_code').AsString,
        DS.FieldByName('action').AsString,
        VarToStr(DS.FieldByName('previous_status').Value),
        DS.FieldByName('new_status').AsString,
        DS.FieldByName('changed_at').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Withdrawal reasons summary:');
  DS := Conn.ExecuteQuery(
    'SELECT reason, COUNT(*) as cnt FROM consent_history ' +
    'WHERE action = ''withdraw'' GROUP BY reason ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-30s %d occurrences', [
        DS.FieldByName('reason').AsString,
        DS.FieldByName('cnt').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Evaluates processing eligibility for each subject+purpose based on legal basis (consent, legitimate interest, or contract). }
procedure Demo9_PurposeBasedAccess;
var
  DS: TDataSet;
  SubjectID, Purpose, Status: string;
  HasConsent: Boolean;
begin
  WriteLn('=== 9. Purpose-Based Access Control ===');
  WriteLn;
  WriteLn('   Checking if processing is allowed for each subject+purpose:');
  WriteLn;

  // Check processing eligibility
  DS := Conn.ExecuteQuery(
    'SELECT ds.subject_id, p.purpose_code, p.legal_basis, p.mandatory, ' +
    '  COALESCE(c.status, ''none'') as consent_status ' +
    'FROM data_subjects ds ' +
    'CROSS JOIN purposes p ' +
    'LEFT JOIN consents c ON c.subject_id = ds.subject_id AND c.purpose_code = p.purpose_code ' +
    'WHERE p.purpose_code IN (''MARKETING'', ''ANALYTICS'', ''THIRD_PARTY'') ' +
    'ORDER BY ds.subject_id, p.purpose_code');
  try
    WriteLn(Format('   %-10s %-14s %-20s %-10s %-10s', ['Subject', 'Purpose', 'Legal Basis', 'Consent', 'Allowed?']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      SubjectID := DS.FieldByName('subject_id').AsString;
      Purpose := DS.FieldByName('purpose_code').AsString;
      Status := DS.FieldByName('consent_status').AsString;

      // Determine if processing is allowed
      if DS.FieldByName('mandatory').AsInteger = 1 then
        HasConsent := True  // contract-based, always allowed
      else if DS.FieldByName('legal_basis').AsString = 'legitimate_interest' then
        HasConsent := (Status <> 'withdrawn')  // allowed unless explicitly opted out
      else
        HasConsent := (Status = 'granted');  // consent-based, must be explicitly granted

      WriteLn(Format('   %-10s %-14s %-20s %-10s %-10s', [
        SubjectID, Purpose,
        DS.FieldByName('legal_basis').AsString,
        Status,
        BoolToStr(HasConsent, 'YES', 'NO')
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Rules: consent->must be granted, legitimate_interest->unless withdrawn, contract->always');
  WriteLn;
end;

{ Demonstrates consent management compliance statistics and coverage metrics. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Compliance Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM data_subjects');
  try
    WriteLn(Format('   Data subjects: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM purposes');
  try
    WriteLn(Format('   Processing purposes: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM consents');
  try
    WriteLn(Format('   Total consent records: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM consents WHERE status = ''granted''');
  try
    WriteLn(Format('   Active consents: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM consents WHERE status = ''withdrawn''');
  try
    WriteLn(Format('   Withdrawn consents: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM consent_history');
  try
    WriteLn(Format('   Audit trail entries: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM processing_records');
  try
    WriteLn(Format('   Processing records: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM dsar_requests');
  try
    WriteLn(Format('   DSAR requests: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Consent rate by purpose:');
  DS := Conn.ExecuteQuery(
    'SELECT p.purpose_code, ' +
    '  (SELECT COUNT(*) FROM consents c WHERE c.purpose_code = p.purpose_code AND c.status = ''granted'') as granted, ' +
    '  (SELECT COUNT(*) FROM data_subjects) as total ' +
    'FROM purposes p WHERE p.mandatory = 0 ORDER BY p.purpose_code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %d/%d (%.0f%%)', [
        DS.FieldByName('purpose_code').AsString,
        DS.FieldByName('granted').AsInteger,
        DS.FieldByName('total').AsInteger,
        DS.FieldByName('granted').AsInteger / DS.FieldByName('total').AsInteger * 100
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   DSAR response times:');
  DS := Conn.ExecuteQuery(
    'SELECT request_type, status, ' +
    '  CASE WHEN completed_at IS NOT NULL THEN ' +
    '    CAST(julianday(completed_at) - julianday(submitted_at) AS INTEGER) ' +
    '  ELSE -1 END as days ' +
    'FROM dsar_requests ORDER BY submitted_at');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('days').AsInteger >= 0 then
        WriteLn(Format('   %-14s %s in %d days', [
          DS.FieldByName('request_type').AsString,
          DS.FieldByName('status').AsString,
          DS.FieldByName('days').AsInteger
        ]))
      else
        WriteLn(Format('   %-14s %s (pending)', [
          DS.FieldByName('request_type').AsString,
          DS.FieldByName('status').AsString
        ]));
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
  WriteLn('Example 130: Consent Management - GDPR Compliance, DSAR, Processing Records');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;

    Demo1_DataSubjects;
    Demo2_Purposes;
    Demo3_ConsentGrants;
    Demo4_ConsentStatus;
    Demo5_Withdrawals;
    Demo6_DSARRequests;
    Demo7_ProcessingRecords;
    Demo8_ConsentHistory;
    Demo9_PurposeBasedAccess;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
