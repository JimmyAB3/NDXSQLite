{===============================================================================
  NDXSQLite Example 131 - Retention Policies
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Data retention policy definitions
  - Expiry detection and enforcement
  - Legal hold management
  - Cascading cleanup operations
  - Deletion audit trail

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program RetentionPolicies;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Retention policies per data category
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS retention_policies (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  category TEXT NOT NULL UNIQUE,' +
    '  retention_days INTEGER NOT NULL,' +
    '  description TEXT,' +
    '  action TEXT NOT NULL DEFAULT ''delete'',' +   // delete, archive, anonymize
    '  cascade_children INTEGER DEFAULT 0' +
    ')'
  );

  // Data records with lifecycle tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS data_records (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  record_ref TEXT NOT NULL UNIQUE,' +
    '  category TEXT NOT NULL,' +
    '  owner TEXT,' +
    '  description TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  expires_at TEXT,' +
    '  status TEXT NOT NULL DEFAULT ''active'',' +   // active, expired, deleted, archived, held
    '  parent_ref TEXT' +
    ')'
  );

  // Legal holds
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS legal_holds (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  hold_id TEXT NOT NULL UNIQUE,' +
    '  record_ref TEXT NOT NULL,' +
    '  reason TEXT NOT NULL,' +
    '  placed_by TEXT NOT NULL,' +
    '  placed_at TEXT NOT NULL,' +
    '  released_at TEXT,' +
    '  status TEXT NOT NULL DEFAULT ''active''' +    // active, released
    ')'
  );

  // Deletion/action log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS deletion_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  record_ref TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  action_taken TEXT NOT NULL,' +               // deleted, archived, anonymized, held
    '  reason TEXT NOT NULL,' +                     // policy_expiry, cascade, manual, legal_hold_block
    '  executed_at TEXT NOT NULL,' +
    '  policy_id INTEGER' +
    ')'
  );

  // Cascade rules (parent-child relationships)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS cascade_rules (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  parent_category TEXT NOT NULL,' +
    '  child_category TEXT NOT NULL,' +
    '  cascade_action TEXT NOT NULL DEFAULT ''delete'',' +
    '  UNIQUE(parent_category, child_category)' +
    ')'
  );
end;

{ Inserts sample data for demonstrations. }
procedure InsertSampleData;
begin
  // Retention policies
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''session_log'', 30, ''User session logs'', ''delete'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''analytics_event'', 90, ''Analytics and tracking events'', ''anonymize'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''user_account'', 730, ''User account data (2 years inactive)'', ''archive'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''transaction'', 2555, ''Financial transactions (7 years)'', ''archive'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''support_ticket'', 365, ''Customer support tickets (1 year)'', ''delete'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''temp_upload'', 7, ''Temporary file uploads'', ''delete'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''audit_trail'', 1825, ''System audit logs (5 years)'', ''archive'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (category, retention_days, description, action, cascade_children) VALUES (''marketing_pref'', 365, ''Marketing preferences'', ''delete'', 0)');

  // Cascade rules
  Conn.ExecuteNonQuery('INSERT INTO cascade_rules (parent_category, child_category, cascade_action) VALUES (''user_account'', ''session_log'', ''delete'')');
  Conn.ExecuteNonQuery('INSERT INTO cascade_rules (parent_category, child_category, cascade_action) VALUES (''user_account'', ''analytics_event'', ''anonymize'')');
  Conn.ExecuteNonQuery('INSERT INTO cascade_rules (parent_category, child_category, cascade_action) VALUES (''user_account'', ''marketing_pref'', ''delete'')');
  Conn.ExecuteNonQuery('INSERT INTO cascade_rules (parent_category, child_category, cascade_action) VALUES (''support_ticket'', ''temp_upload'', ''delete'')');

  // Data records (using relative dates from a reference point of 2024-06-01)
  // Session logs (some expired at 30 days)
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''SESS-001'', ''session_log'', ''alice'', ''Login session'', ''2024-04-01 10:00:00'', ''2024-05-01 10:00:00'', ''expired'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''SESS-002'', ''session_log'', ''bob'', ''Login session'', ''2024-04-15 14:00:00'', ''2024-05-15 14:00:00'', ''expired'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''SESS-003'', ''session_log'', ''alice'', ''Login session'', ''2024-05-20 09:00:00'', ''2024-06-19 09:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''SESS-004'', ''session_log'', ''carol'', ''Login session'', ''2024-05-25 11:00:00'', ''2024-06-24 11:00:00'', ''active'')');

  // Analytics events (some expired at 90 days)
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''EVT-001'', ''analytics_event'', ''alice'', ''Page view event'', ''2024-02-01 08:00:00'', ''2024-05-01 08:00:00'', ''expired'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''EVT-002'', ''analytics_event'', ''bob'', ''Click event'', ''2024-03-15 12:00:00'', ''2024-06-13 12:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''EVT-003'', ''analytics_event'', ''carol'', ''Purchase event'', ''2024-04-10 16:00:00'', ''2024-07-09 16:00:00'', ''active'')');

  // User accounts
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''USR-001'', ''user_account'', ''alice'', ''Active user account'', ''2022-01-15 09:00:00'', ''2024-01-15 09:00:00'', ''expired'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''USR-002'', ''user_account'', ''bob'', ''Active user account'', ''2023-06-01 10:00:00'', ''2025-06-01 10:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''USR-003'', ''user_account'', ''carol'', ''Active user account'', ''2024-01-10 14:00:00'', ''2026-01-10 14:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''USR-004'', ''user_account'', ''dave'', ''Inactive account'', ''2021-03-01 08:00:00'', ''2023-03-01 08:00:00'', ''expired'')');

  // Transactions (7 year retention)
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''TXN-001'', ''transaction'', ''alice'', ''Purchase $49.99'', ''2020-06-15 10:00:00'', ''2027-06-15 10:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''TXN-002'', ''transaction'', ''bob'', ''Purchase $129.00'', ''2023-11-20 14:00:00'', ''2030-11-20 14:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''TXN-003'', ''transaction'', ''dave'', ''Refund $25.00'', ''2017-01-10 09:00:00'', ''2024-01-10 09:00:00'', ''expired'')');

  // Support tickets
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''TKT-001'', ''support_ticket'', ''alice'', ''Password reset help'', ''2023-08-01 11:00:00'', ''2024-08-01 11:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''TKT-002'', ''support_ticket'', ''carol'', ''Billing question'', ''2023-03-15 09:00:00'', ''2024-03-15 09:00:00'', ''expired'')');

  // Temp uploads (7 day retention)
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status, parent_ref) VALUES (''TMP-001'', ''temp_upload'', ''alice'', ''Profile photo draft'', ''2024-05-20 10:00:00'', ''2024-05-27 10:00:00'', ''expired'', ''TKT-001'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status, parent_ref) VALUES (''TMP-002'', ''temp_upload'', ''bob'', ''Document scan'', ''2024-05-28 15:00:00'', ''2024-06-04 15:00:00'', ''active'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status, parent_ref) VALUES (''TMP-003'', ''temp_upload'', ''carol'', ''Screenshot'', ''2024-05-29 08:00:00'', ''2024-06-05 08:00:00'', ''active'', ''TKT-002'')');

  // Marketing preferences
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status, parent_ref) VALUES (''MKT-001'', ''marketing_pref'', ''alice'', ''Email preferences'', ''2023-06-01 09:00:00'', ''2024-06-01 09:00:00'', ''expired'', ''USR-001'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status, parent_ref) VALUES (''MKT-002'', ''marketing_pref'', ''bob'', ''Email preferences'', ''2024-01-15 10:00:00'', ''2025-01-15 10:00:00'', ''active'', ''USR-002'')');

  // Audit trail
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''AUD-001'', ''audit_trail'', ''system'', ''Login audit log'', ''2020-01-01 00:00:00'', ''2025-01-01 00:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO data_records (record_ref, category, owner, description, created_at, expires_at, status) VALUES (''AUD-002'', ''audit_trail'', ''system'', ''Data access log'', ''2019-01-01 00:00:00'', ''2024-01-01 00:00:00'', ''expired'')');

  // Legal holds
  Conn.ExecuteNonQuery('INSERT INTO legal_holds (hold_id, record_ref, reason, placed_by, placed_at, status) VALUES (''HOLD-001'', ''USR-001'', ''Ongoing litigation - employment dispute'', ''legal_dept'', ''2024-02-01 09:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO legal_holds (hold_id, record_ref, reason, placed_by, placed_at, status) VALUES (''HOLD-002'', ''TXN-003'', ''Tax audit 2017'', ''finance_dept'', ''2024-01-15 14:00:00'', ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO legal_holds (hold_id, record_ref, reason, placed_by, placed_at, released_at, status) VALUES (''HOLD-003'', ''SESS-001'', ''Security investigation'', ''security_team'', ''2024-04-10 08:00:00'', ''2024-05-10 08:00:00'', ''released'')');
  Conn.ExecuteNonQuery('INSERT INTO legal_holds (hold_id, record_ref, reason, placed_by, placed_at, status) VALUES (''HOLD-004'', ''TKT-002'', ''Regulatory inquiry'', ''compliance_team'', ''2024-03-20 10:00:00'', ''active'')');
end;

// ============================================================
// Demo Sections
// ============================================================

{ Lists all retention policies showing category, retention period in days, action type, cascade setting, and description. }
procedure Demo1_RetentionPolicies;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Retention Policies ===');
  WriteLn;
  WriteLn(Format('   %-18s %-6s %-12s %-8s %s', ['Category', 'Days', 'Action', 'Cascade', 'Description']));
  WriteLn('   ' + StringOfChar('-', 80));

  DS := Conn.ExecuteQuery('SELECT * FROM retention_policies ORDER BY retention_days');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-6d %-12s %-8s %s', [
        DS.FieldByName('category').AsString,
        DS.FieldByName('retention_days').AsInteger,
        DS.FieldByName('action').AsString,
        BoolToStr(DS.FieldByName('cascade_children').AsInteger = 1, 'yes', 'no'),
        DS.FieldByName('description').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Actions: delete (permanent), archive (cold storage), anonymize (remove PII)');
  WriteLn;
end;

{ Displays total record count, records grouped by category and status, and per-owner record counts with expiration tallies. }
procedure Demo2_DataInventory;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Data Inventory ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM data_records');
  try
    WriteLn(Format('   Total data records: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Records by category and status:');
  DS := Conn.ExecuteQuery(
    'SELECT category, status, COUNT(*) as cnt ' +
    'FROM data_records GROUP BY category, status ORDER BY category, status');
  try
    WriteLn(Format('   %-18s %-10s %s', ['Category', 'Status', 'Count']));
    WriteLn('   ' + StringOfChar('-', 35));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-10s %d', [
        DS.FieldByName('category').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Records by owner:');
  DS := Conn.ExecuteQuery(
    'SELECT owner, COUNT(*) as cnt, ' +
    '  SUM(CASE WHEN status = ''expired'' THEN 1 ELSE 0 END) as expired_cnt ' +
    'FROM data_records GROUP BY owner ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %d records (%d expired)', [
        DS.FieldByName('owner').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('expired_cnt').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists all records that have exceeded their retention period and are awaiting processing action. }
procedure Demo3_ExpiryCheck;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Policy-Driven Expiry Check ===');
  WriteLn;
  WriteLn('   Reference date: 2024-06-01');
  WriteLn('   Records past their retention period:');
  WriteLn;
  WriteLn(Format('   %-10s %-18s %-8s %-22s %-22s', ['Ref', 'Category', 'Owner', 'Created', 'Expired At']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery(
    'SELECT record_ref, category, owner, created_at, expires_at ' +
    'FROM data_records ' +
    'WHERE status = ''expired'' ' +
    'ORDER BY expires_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-18s %-8s %-22s %-22s', [
        DS.FieldByName('record_ref').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('created_at').AsString,
        DS.FieldByName('expires_at').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM data_records WHERE status = ''expired''');
  try
    WriteLn(Format('   Total expired records awaiting action: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays all legal holds with their reasons and status, and identifies expired records blocked from deletion by active holds. }
procedure Demo4_LegalHolds;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Legal Holds ===');
  WriteLn;
  WriteLn(Format('   %-10s %-10s %-35s %-15s %-8s', ['Hold ID', 'Record', 'Reason', 'Placed By', 'Status']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery('SELECT * FROM legal_holds ORDER BY placed_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-10s %-35s %-15s %-8s', [
        DS.FieldByName('hold_id').AsString,
        DS.FieldByName('record_ref').AsString,
        DS.FieldByName('reason').AsString,
        DS.FieldByName('placed_by').AsString,
        DS.FieldByName('status').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Held records that are past expiry (cannot be deleted):');
  DS := Conn.ExecuteQuery(
    'SELECT dr.record_ref, dr.category, dr.expires_at, lh.reason ' +
    'FROM data_records dr ' +
    'JOIN legal_holds lh ON lh.record_ref = dr.record_ref ' +
    'WHERE dr.status = ''expired'' AND lh.status = ''active'' ' +
    'ORDER BY dr.record_ref');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-18s expired %-22s HELD: %s', [
        DS.FieldByName('record_ref').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('expires_at').AsString,
        DS.FieldByName('reason').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Processes expired records by executing policy actions (delete/archive/anonymize) while respecting legal holds, and logs all actions. }
procedure Demo5_ExpiryExecution;
var
  DS: TDataSet;
  RecRef, Category, Action: string;
  PolicyAction: string;
  IsHeld: Boolean;
  ExecutedCount, HeldCount: Integer;
begin
  WriteLn('=== 5. Expiry Execution (Simulated) ===');
  WriteLn;
  WriteLn('   Processing expired records (respecting legal holds):');
  WriteLn;

  ExecutedCount := 0;
  HeldCount := 0;

  DS := Conn.ExecuteQuery(
    'SELECT dr.record_ref, dr.category, ' +
    '  rp.action as policy_action, ' +
    '  CASE WHEN EXISTS(SELECT 1 FROM legal_holds lh WHERE lh.record_ref = dr.record_ref AND lh.status = ''active'') ' +
    '    THEN 1 ELSE 0 END as is_held ' +
    'FROM data_records dr ' +
    'JOIN retention_policies rp ON rp.category = dr.category ' +
    'WHERE dr.status = ''expired'' ' +
    'ORDER BY dr.record_ref');
  try
    while not DS.EOF do
    begin
      RecRef := DS.FieldByName('record_ref').AsString;
      Category := DS.FieldByName('category').AsString;
      PolicyAction := DS.FieldByName('policy_action').AsString;
      IsHeld := DS.FieldByName('is_held').AsInteger = 1;

      if IsHeld then
      begin
        Action := 'BLOCKED (legal hold)';
        Inc(HeldCount);
        // Log the hold block
        Conn.ExecuteNonQuery(Format(
          'INSERT INTO deletion_log (record_ref, category, action_taken, reason, executed_at) VALUES (''%s'', ''%s'', ''held'', ''legal_hold_block'', ''2024-06-01 00:00:00'')',
          [RecRef, Category]));
      end
      else
      begin
        Action := UpperCase(PolicyAction);
        Inc(ExecutedCount);
        // Log the action
        Conn.ExecuteNonQuery(Format(
          'INSERT INTO deletion_log (record_ref, category, action_taken, reason, executed_at) VALUES (''%s'', ''%s'', ''%s'', ''policy_expiry'', ''2024-06-01 00:00:00'')',
          [RecRef, Category, PolicyAction]));
        // Update status
        if PolicyAction = 'delete' then
          Conn.ExecuteNonQuery(Format('UPDATE data_records SET status = ''deleted'' WHERE record_ref = ''%s''', [RecRef]))
        else if PolicyAction = 'archive' then
          Conn.ExecuteNonQuery(Format('UPDATE data_records SET status = ''archived'' WHERE record_ref = ''%s''', [RecRef]))
        else
          Conn.ExecuteNonQuery(Format('UPDATE data_records SET status = ''archived'' WHERE record_ref = ''%s''', [RecRef]));
      end;

      WriteLn(Format('   %-10s %-18s -> %s', [RecRef, Category, Action]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Executed: %d records processed', [ExecutedCount]));
  WriteLn(Format('   Held: %d records blocked by legal holds', [HeldCount]));
  WriteLn;
end;

{ Shows cascade rules and applies cascading deletions/anonymizations to child records when parent records are deleted or archived. }
procedure Demo6_CascadingCleanup;
var
  DS: TDataSet;
  ParentRef, ChildRef, ChildCategory, CascadeAction: string;
  CascadeCount: Integer;
begin
  WriteLn('=== 6. Cascading Cleanup ===');
  WriteLn;
  WriteLn('   Cascade rules:');
  WriteLn(Format('   %-18s -> %-18s %-10s', ['Parent Category', 'Child Category', 'Action']));
  WriteLn('   ' + StringOfChar('-', 55));

  DS := Conn.ExecuteQuery('SELECT * FROM cascade_rules ORDER BY parent_category');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s -> %-18s %-10s', [
        DS.FieldByName('parent_category').AsString,
        DS.FieldByName('child_category').AsString,
        DS.FieldByName('cascade_action').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Applying cascades for deleted/archived parent records:');
  WriteLn;

  CascadeCount := 0;

  // Find children of deleted/archived parents
  DS := Conn.ExecuteQuery(
    'SELECT dr_child.record_ref as child_ref, dr_child.category as child_cat, ' +
    '  dr_child.parent_ref, dr_parent.status as parent_status, ' +
    '  cr.cascade_action ' +
    'FROM data_records dr_child ' +
    'JOIN data_records dr_parent ON dr_parent.record_ref = dr_child.parent_ref ' +
    'JOIN cascade_rules cr ON cr.parent_category = dr_parent.category AND cr.child_category = dr_child.category ' +
    'WHERE dr_parent.status IN (''deleted'', ''archived'') ' +
    'AND dr_child.status NOT IN (''deleted'', ''archived'') ' +
    'ORDER BY dr_child.record_ref');
  try
    while not DS.EOF do
    begin
      ChildRef := DS.FieldByName('child_ref').AsString;
      ChildCategory := DS.FieldByName('child_cat').AsString;
      ParentRef := DS.FieldByName('parent_ref').AsString;
      CascadeAction := DS.FieldByName('cascade_action').AsString;

      WriteLn(Format('   %-10s (%s) -> cascade %s (parent %s %s)', [
        ChildRef, ChildCategory, CascadeAction, ParentRef,
        DS.FieldByName('parent_status').AsString]));

      // Execute cascade
      Conn.ExecuteNonQuery(Format(
        'UPDATE data_records SET status = ''deleted'' WHERE record_ref = ''%s''',
        [ChildRef]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO deletion_log (record_ref, category, action_taken, reason, executed_at) VALUES (''%s'', ''%s'', ''%s'', ''cascade'', ''2024-06-01 00:01:00'')',
        [ChildRef, ChildCategory, CascadeAction]));

      Inc(CascadeCount);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  if CascadeCount = 0 then
    WriteLn('   No cascading actions needed at this time.');

  WriteLn;
  WriteLn(Format('   Cascade actions executed: %d', [CascadeCount]));
  WriteLn;
end;

{ Displays the deletion audit log with record references, actions taken, reasons, and timestamps, plus a summary by action type. }
procedure Demo7_DeletionAudit;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Deletion Audit Log ===');
  WriteLn;
  WriteLn(Format('   %-10s %-18s %-12s %-18s %s', ['Record', 'Category', 'Action', 'Reason', 'Executed']));
  WriteLn('   ' + StringOfChar('-', 80));

  DS := Conn.ExecuteQuery('SELECT * FROM deletion_log ORDER BY executed_at, record_ref');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-18s %-12s %-18s %s', [
        DS.FieldByName('record_ref').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('action_taken').AsString,
        DS.FieldByName('reason').AsString,
        DS.FieldByName('executed_at').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Audit summary:');
  DS := Conn.ExecuteQuery(
    'SELECT action_taken, reason, COUNT(*) as cnt ' +
    'FROM deletion_log GROUP BY action_taken, reason ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-18s %d records', [
        DS.FieldByName('action_taken').AsString,
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

{ Lists active and released legal holds with details, and simulates releasing a hold to allow subsequent policy processing. }
procedure Demo8_HoldManagement;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Hold Management ===');
  WriteLn;

  // Show active vs released holds
  WriteLn('   Active holds:');
  DS := Conn.ExecuteQuery(
    'SELECT lh.hold_id, lh.record_ref, dr.category, lh.placed_by, lh.placed_at ' +
    'FROM legal_holds lh ' +
    'JOIN data_records dr ON dr.record_ref = lh.record_ref ' +
    'WHERE lh.status = ''active'' ORDER BY lh.placed_at');
  try
    WriteLn(Format('   %-10s %-10s %-18s %-15s %s', ['Hold', 'Record', 'Category', 'Placed By', 'Since']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-10s %-18s %-15s %s', [
        DS.FieldByName('hold_id').AsString,
        DS.FieldByName('record_ref').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('placed_by').AsString,
        DS.FieldByName('placed_at').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Released holds:');
  DS := Conn.ExecuteQuery(
    'SELECT hold_id, record_ref, placed_at, released_at, reason ' +
    'FROM legal_holds WHERE status = ''released'' ORDER BY released_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s on %-10s (held %s to %s): %s', [
        DS.FieldByName('hold_id').AsString,
        DS.FieldByName('record_ref').AsString,
        DS.FieldByName('placed_at').AsString,
        DS.FieldByName('released_at').AsString,
        DS.FieldByName('reason').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Simulating hold release for HOLD-004...');
  Conn.ExecuteNonQuery('UPDATE legal_holds SET status = ''released'', released_at = ''2024-06-01 12:00:00'' WHERE hold_id = ''HOLD-004''');
  WriteLn('   HOLD-004 released. TKT-002 can now be processed by retention policy.');
  WriteLn;
end;

{ Generates a compliance report showing overdue records and per-category breakdown of processed, within-policy, and overdue records. }
procedure Demo9_ComplianceReport;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Retention Compliance Report ===');
  WriteLn;

  // Check for records that exceeded their retention without action
  WriteLn('   Records exceeding retention period without processing:');
  WriteLn(Format('   %-10s %-18s %-22s %-6s %s', ['Record', 'Category', 'Expired At', 'Days', 'Status']));
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery(
    'SELECT dr.record_ref, dr.category, dr.expires_at, dr.status, ' +
    '  CAST(julianday(''2024-06-01'') - julianday(dr.expires_at) AS INTEGER) as days_overdue ' +
    'FROM data_records dr ' +
    'WHERE dr.expires_at < ''2024-06-01'' ' +
    'AND dr.status NOT IN (''deleted'', ''archived'') ' +
    'ORDER BY days_overdue DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-18s %-22s %-6d %s', [
        DS.FieldByName('record_ref').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('expires_at').AsString,
        DS.FieldByName('days_overdue').AsInteger,
        DS.FieldByName('status').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Policy compliance by category:');
  DS := Conn.ExecuteQuery(
    'SELECT dr.category, ' +
    '  COUNT(*) as total, ' +
    '  SUM(CASE WHEN dr.status IN (''deleted'', ''archived'') THEN 1 ELSE 0 END) as processed, ' +
    '  SUM(CASE WHEN dr.status = ''active'' AND dr.expires_at > ''2024-06-01'' THEN 1 ELSE 0 END) as within_policy, ' +
    '  SUM(CASE WHEN dr.status NOT IN (''deleted'', ''archived'') AND dr.expires_at <= ''2024-06-01'' THEN 1 ELSE 0 END) as overdue ' +
    'FROM data_records dr ' +
    'GROUP BY dr.category ORDER BY dr.category');
  try
    WriteLn(Format('   %-18s %-6s %-10s %-12s %-8s', ['Category', 'Total', 'Processed', 'Within Policy', 'Overdue']));
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-6d %-10d %-12d %-8d', [
        DS.FieldByName('category').AsString,
        DS.FieldByName('total').AsInteger,
        DS.FieldByName('processed').AsInteger,
        DS.FieldByName('within_policy').AsInteger,
        DS.FieldByName('overdue').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates retention policy statistics and data lifecycle metrics. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM retention_policies');
  try
    WriteLn(Format('   Retention policies: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM data_records');
  try
    WriteLn(Format('   Total data records: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM legal_holds WHERE status = ''active''');
  try
    WriteLn(Format('   Active legal holds: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM deletion_log');
  try
    WriteLn(Format('   Deletion log entries: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM cascade_rules');
  try
    WriteLn(Format('   Cascade rules: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Record status distribution:');
  DS := Conn.ExecuteQuery('SELECT status, COUNT(*) as cnt FROM data_records GROUP BY status ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d records', [
        DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Retention period summary:');
  DS := Conn.ExecuteQuery(
    'SELECT category, retention_days, ' +
    '  CASE ' +
    '    WHEN retention_days <= 30 THEN ''short-term'' ' +
    '    WHEN retention_days <= 365 THEN ''medium-term'' ' +
    '    ELSE ''long-term'' ' +
    '  END as tier ' +
    'FROM retention_policies ORDER BY retention_days');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %4d days  (%s)', [
        DS.FieldByName('category').AsString,
        DS.FieldByName('retention_days').AsInteger,
        DS.FieldByName('tier').AsString
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
  WriteLn('Example 131: Retention Policies - Policy-Driven Expiry, Legal Hold, Cascade');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;

    Demo1_RetentionPolicies;
    Demo2_DataInventory;
    Demo3_ExpiryCheck;
    Demo4_LegalHolds;
    Demo5_ExpiryExecution;
    Demo6_CascadingCleanup;
    Demo7_DeletionAudit;
    Demo8_HoldManagement;
    Demo9_ComplianceReport;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
