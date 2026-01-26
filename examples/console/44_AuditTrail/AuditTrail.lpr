{===============================================================================
  NDXSQLite Example 44 - Audit Trail Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Complete audit logging with before/after values
  - Automatic change tracking with triggers
  - User and session tracking
  - Tamper detection with checksums
  - Audit history queries
  - Compliance reporting (GDPR, SOX)

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program AuditTrail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

// Current session context (would come from application context)
var
  CurrentUserId: string = 'system';
  CurrentSessionId: string = '';
  CurrentIpAddress: string = '127.0.0.1';

// ============================================================================
// Simple checksum for tamper detection (use SHA-256 in production)
// ============================================================================

{ Computes an 8-character hex checksum of the given string using a shift-and-XOR hash for tamper detection. }
function CalculateChecksum(const AData: string): string;
var
  I: Integer;
  Hash: Cardinal;
begin
  Hash := 0;
  for I := 1 to Length(AData) do
    Hash := ((Hash shl 5) + Hash) xor Ord(AData[I]);
  Result := IntToHex(Hash, 8);
end;

// ============================================================================
// Database setup
// ============================================================================

{ Creates the audit_trail table, audit_context session table, customers and orders business tables,
  and INSERT/UPDATE/DELETE triggers on each business table that log changes with old/new JSON values. }
procedure SetupAuditSchema;
begin
  // Main audit trail table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS audit_trail (' +
    '  id INTEGER PRIMARY KEY,' +
    '  table_name TEXT NOT NULL,' +
    '  record_id TEXT NOT NULL,' +           // ID of the affected record
    '  action TEXT NOT NULL,' +              // INSERT, UPDATE, DELETE
    '  old_values TEXT,' +                   // JSON of old values (NULL for INSERT)
    '  new_values TEXT,' +                   // JSON of new values (NULL for DELETE)
    '  changed_fields TEXT,' +               // JSON array of changed field names
    '  user_id TEXT,' +
    '  session_id TEXT,' +
    '  ip_address TEXT,' +
    '  user_agent TEXT,' +
    '  timestamp TEXT DEFAULT (datetime(''now'')),' +
    '  checksum TEXT' +                      // For tamper detection
    ')');

  // Index for efficient queries
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_audit_table ON audit_trail(table_name)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_audit_record ON audit_trail(table_name, record_id)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_audit_user ON audit_trail(user_id)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_audit_timestamp ON audit_trail(timestamp)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_audit_action ON audit_trail(action)');

  // Session context table (for trigger access)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS audit_context (' +
    '  key TEXT PRIMARY KEY,' +
    '  value TEXT' +
    ')');

  // Sample business tables
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT UNIQUE,' +
    '  phone TEXT,' +
    '  address TEXT,' +
    '  credit_limit REAL DEFAULT 0,' +
    '  status TEXT DEFAULT ''active'',' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER REFERENCES customers(id),' +
    '  order_date TEXT DEFAULT (datetime(''now'')),' +
    '  total_amount REAL,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  notes TEXT' +
    ')');

  // Create audit triggers for customers table
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS audit_customers_insert ' +
    'AFTER INSERT ON customers ' +
    'BEGIN ' +
    '  INSERT INTO audit_trail (table_name, record_id, action, new_values, user_id, session_id, ip_address) ' +
    '  VALUES (' +
    '    ''customers'', ' +
    '    NEW.id, ' +
    '    ''INSERT'', ' +
    '    json_object(' +
    '      ''id'', NEW.id, ' +
    '      ''name'', NEW.name, ' +
    '      ''email'', NEW.email, ' +
    '      ''phone'', NEW.phone, ' +
    '      ''address'', NEW.address, ' +
    '      ''credit_limit'', NEW.credit_limit, ' +
    '      ''status'', NEW.status' +
    '    ), ' +
    '    (SELECT value FROM audit_context WHERE key = ''user_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''session_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''ip_address'') ' +
    '  ); ' +
    'END');

  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS audit_customers_update ' +
    'AFTER UPDATE ON customers ' +
    'BEGIN ' +
    '  INSERT INTO audit_trail (table_name, record_id, action, old_values, new_values, changed_fields, user_id, session_id, ip_address) ' +
    '  VALUES (' +
    '    ''customers'', ' +
    '    NEW.id, ' +
    '    ''UPDATE'', ' +
    '    json_object(' +
    '      ''id'', OLD.id, ' +
    '      ''name'', OLD.name, ' +
    '      ''email'', OLD.email, ' +
    '      ''phone'', OLD.phone, ' +
    '      ''address'', OLD.address, ' +
    '      ''credit_limit'', OLD.credit_limit, ' +
    '      ''status'', OLD.status' +
    '    ), ' +
    '    json_object(' +
    '      ''id'', NEW.id, ' +
    '      ''name'', NEW.name, ' +
    '      ''email'', NEW.email, ' +
    '      ''phone'', NEW.phone, ' +
    '      ''address'', NEW.address, ' +
    '      ''credit_limit'', NEW.credit_limit, ' +
    '      ''status'', NEW.status' +
    '    ), ' +
    '    json_array(' +
    '      CASE WHEN OLD.name != NEW.name THEN ''name'' ELSE NULL END,' +
    '      CASE WHEN OLD.email != NEW.email OR (OLD.email IS NULL) != (NEW.email IS NULL) THEN ''email'' ELSE NULL END,' +
    '      CASE WHEN OLD.phone != NEW.phone OR (OLD.phone IS NULL) != (NEW.phone IS NULL) THEN ''phone'' ELSE NULL END,' +
    '      CASE WHEN OLD.address != NEW.address OR (OLD.address IS NULL) != (NEW.address IS NULL) THEN ''address'' ELSE NULL END,' +
    '      CASE WHEN OLD.credit_limit != NEW.credit_limit THEN ''credit_limit'' ELSE NULL END,' +
    '      CASE WHEN OLD.status != NEW.status THEN ''status'' ELSE NULL END' +
    '    ), ' +
    '    (SELECT value FROM audit_context WHERE key = ''user_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''session_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''ip_address'') ' +
    '  ); ' +
    '  UPDATE customers SET updated_at = datetime(''now'') WHERE id = NEW.id; ' +
    'END');

  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS audit_customers_delete ' +
    'AFTER DELETE ON customers ' +
    'BEGIN ' +
    '  INSERT INTO audit_trail (table_name, record_id, action, old_values, user_id, session_id, ip_address) ' +
    '  VALUES (' +
    '    ''customers'', ' +
    '    OLD.id, ' +
    '    ''DELETE'', ' +
    '    json_object(' +
    '      ''id'', OLD.id, ' +
    '      ''name'', OLD.name, ' +
    '      ''email'', OLD.email, ' +
    '      ''phone'', OLD.phone, ' +
    '      ''address'', OLD.address, ' +
    '      ''credit_limit'', OLD.credit_limit, ' +
    '      ''status'', OLD.status' +
    '    ), ' +
    '    (SELECT value FROM audit_context WHERE key = ''user_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''session_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''ip_address'') ' +
    '  ); ' +
    'END');

  // Similar triggers for orders table
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS audit_orders_insert ' +
    'AFTER INSERT ON orders ' +
    'BEGIN ' +
    '  INSERT INTO audit_trail (table_name, record_id, action, new_values, user_id, session_id, ip_address) ' +
    '  VALUES (' +
    '    ''orders'', ' +
    '    NEW.id, ' +
    '    ''INSERT'', ' +
    '    json_object(' +
    '      ''id'', NEW.id, ' +
    '      ''customer_id'', NEW.customer_id, ' +
    '      ''total_amount'', NEW.total_amount, ' +
    '      ''status'', NEW.status, ' +
    '      ''notes'', NEW.notes' +
    '    ), ' +
    '    (SELECT value FROM audit_context WHERE key = ''user_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''session_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''ip_address'') ' +
    '  ); ' +
    'END');

  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS audit_orders_update ' +
    'AFTER UPDATE ON orders ' +
    'BEGIN ' +
    '  INSERT INTO audit_trail (table_name, record_id, action, old_values, new_values, user_id, session_id, ip_address) ' +
    '  VALUES (' +
    '    ''orders'', ' +
    '    NEW.id, ' +
    '    ''UPDATE'', ' +
    '    json_object(' +
    '      ''id'', OLD.id, ' +
    '      ''customer_id'', OLD.customer_id, ' +
    '      ''total_amount'', OLD.total_amount, ' +
    '      ''status'', OLD.status, ' +
    '      ''notes'', OLD.notes' +
    '    ), ' +
    '    json_object(' +
    '      ''id'', NEW.id, ' +
    '      ''customer_id'', NEW.customer_id, ' +
    '      ''total_amount'', NEW.total_amount, ' +
    '      ''status'', NEW.status, ' +
    '      ''notes'', NEW.notes' +
    '    ), ' +
    '    (SELECT value FROM audit_context WHERE key = ''user_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''session_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''ip_address'') ' +
    '  ); ' +
    'END');

  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS audit_orders_delete ' +
    'AFTER DELETE ON orders ' +
    'BEGIN ' +
    '  INSERT INTO audit_trail (table_name, record_id, action, old_values, user_id, session_id, ip_address) ' +
    '  VALUES (' +
    '    ''orders'', ' +
    '    OLD.id, ' +
    '    ''DELETE'', ' +
    '    json_object(' +
    '      ''id'', OLD.id, ' +
    '      ''customer_id'', OLD.customer_id, ' +
    '      ''total_amount'', OLD.total_amount, ' +
    '      ''status'', OLD.status, ' +
    '      ''notes'', OLD.notes' +
    '    ), ' +
    '    (SELECT value FROM audit_context WHERE key = ''user_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''session_id''), ' +
    '    (SELECT value FROM audit_context WHERE key = ''ip_address'') ' +
    '  ); ' +
    'END');
end;

{ Stores the current user, session, and IP address in both global variables and the audit_context table for use by triggers. }
procedure SetAuditContext(const AUserId, ASessionId, AIpAddress: string);
begin
  CurrentUserId := AUserId;
  CurrentSessionId := ASessionId;
  CurrentIpAddress := AIpAddress;

  Connection.ExecuteNonQuery('INSERT OR REPLACE INTO audit_context (key, value) VALUES (''user_id'', ?)', [AUserId]);
  Connection.ExecuteNonQuery('INSERT OR REPLACE INTO audit_context (key, value) VALUES (''session_id'', ?)', [ASessionId]);
  Connection.ExecuteNonQuery('INSERT OR REPLACE INTO audit_context (key, value) VALUES (''ip_address'', ?)', [AIpAddress]);
end;

// ============================================================================
// Demo procedures
// ============================================================================

{ Sets audit context, inserts three customers, and displays the resulting trigger-generated audit trail entries. }
procedure DemoBasicAuditTrail;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic audit trail with automatic triggers');
  WriteLn('   -----------------------------------------');
  WriteLn('');

  // Set audit context
  SetAuditContext('admin', 'sess_001', '192.168.1.100');
  WriteLn('   Audit context: user=admin, session=sess_001, ip=192.168.1.100');
  WriteLn('');

  // Create customers
  WriteLn('   Creating customers...');
  Connection.ExecuteNonQuery(
    'INSERT INTO customers (name, email, phone, credit_limit) VALUES (?, ?, ?, ?)',
    ['Acme Corp', 'contact@acme.com', '555-0100', 50000.0]);
  Connection.ExecuteNonQuery(
    'INSERT INTO customers (name, email, phone, credit_limit) VALUES (?, ?, ?, ?)',
    ['Globex Inc', 'info@globex.com', '555-0200', 75000.0]);
  Connection.ExecuteNonQuery(
    'INSERT INTO customers (name, email, phone, credit_limit) VALUES (?, ?, ?, ?)',
    ['Initech LLC', 'sales@initech.com', '555-0300', 25000.0]);

  WriteLn('   Created 3 customers');
  WriteLn('');

  // Show audit entries
  WriteLn('   Audit trail entries:');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, action, table_name, record_id, user_id ' +
    'FROM audit_trail ORDER BY id DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('timestamp').AsString, '] ',
              DS.FieldByName('action').AsString:8, ' ',
              DS.FieldByName('table_name').AsString, ' #',
              DS.FieldByName('record_id').AsString,
              ' by ', DS.FieldByName('user_id').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Updates customer fields under a different user context and displays the audit entries showing old values, new values, and changed fields. }
procedure DemoUpdateTracking;
var
  DS: TDataSet;
begin
  WriteLn('2. Tracking updates with before/after values');
  WriteLn('   -----------------------------------------');
  WriteLn('');

  // Change user context
  SetAuditContext('sales_mgr', 'sess_002', '192.168.1.105');
  WriteLn('   Audit context: user=sales_mgr, session=sess_002');
  WriteLn('');

  // Update customer
  WriteLn('   Updating Acme Corp credit limit: 50000 -> 75000');
  Connection.ExecuteNonQuery(
    'UPDATE customers SET credit_limit = ?, status = ? WHERE name = ?',
    [75000.0, 'premium', 'Acme Corp']);

  WriteLn('   Updating Globex Inc phone number');
  Connection.ExecuteNonQuery(
    'UPDATE customers SET phone = ? WHERE name = ?',
    ['555-0201', 'Globex Inc']);

  WriteLn('');

  // Show the update audit entry with details
  WriteLn('   Recent UPDATE audit entries:');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, record_id, old_values, new_values, changed_fields, user_id ' +
    'FROM audit_trail WHERE action = ''UPDATE'' ORDER BY id DESC LIMIT 2');
  try
    while not DS.EOF do
    begin
      WriteLn('   Record #', DS.FieldByName('record_id').AsString,
              ' by ', DS.FieldByName('user_id').AsString);
      WriteLn('     Changed: ', DS.FieldByName('changed_fields').AsString);
      WriteLn('     Old: ', Copy(DS.FieldByName('old_values').AsString, 1, 60), '...');
      WriteLn('     New: ', Copy(DS.FieldByName('new_values').AsString, 1, 60), '...');
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Deletes a customer and displays the audit entry that preserves the deleted record's data as JSON old_values. }
procedure DemoDeleteTracking;
var
  DS: TDataSet;
begin
  WriteLn('3. Tracking deletions');
  WriteLn('   ------------------');
  WriteLn('');

  SetAuditContext('admin', 'sess_003', '192.168.1.100');

  WriteLn('   Deleting Initech LLC...');
  Connection.ExecuteNonQuery('DELETE FROM customers WHERE name = ?', ['Initech LLC']);

  WriteLn('');

  // Show delete audit
  WriteLn('   DELETE audit entry:');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, record_id, old_values, user_id ' +
    'FROM audit_trail WHERE action = ''DELETE'' ORDER BY id DESC LIMIT 1');
  try
    if not DS.EOF then
    begin
      WriteLn('   Deleted record #', DS.FieldByName('record_id').AsString);
      WriteLn('   By: ', DS.FieldByName('user_id').AsString);
      WriteLn('   Data preserved: ', DS.FieldByName('old_values').AsString);
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates orders and updates their status under different user contexts, then displays audit counts grouped by table and action. }
procedure DemoOrdersAudit;
var
  DS: TDataSet;
begin
  WriteLn('4. Multi-table audit trail');
  WriteLn('   -----------------------');
  WriteLn('');

  SetAuditContext('order_clerk', 'sess_004', '192.168.1.110');

  // Create orders
  WriteLn('   Creating orders...');
  Connection.ExecuteNonQuery(
    'INSERT INTO orders (customer_id, total_amount, status, notes) VALUES (?, ?, ?, ?)',
    [1, 1500.00, 'pending', 'Rush delivery requested']);
  Connection.ExecuteNonQuery(
    'INSERT INTO orders (customer_id, total_amount, status) VALUES (?, ?, ?)',
    [1, 2500.00, 'pending']);
  Connection.ExecuteNonQuery(
    'INSERT INTO orders (customer_id, total_amount, status) VALUES (?, ?, ?)',
    [2, 3200.00, 'pending']);

  // Update order status
  SetAuditContext('warehouse', 'sess_005', '192.168.1.120');
  Connection.ExecuteNonQuery(
    'UPDATE orders SET status = ? WHERE id = ?',
    ['shipped', 1]);

  SetAuditContext('accounting', 'sess_006', '192.168.1.130');
  Connection.ExecuteNonQuery(
    'UPDATE orders SET status = ? WHERE id = ?',
    ['paid', 2]);

  WriteLn('');

  // Show all audit by table
  WriteLn('   Audit summary by table:');
  DS := Connection.ExecuteQuery(
    'SELECT table_name, action, COUNT(*) as count ' +
    'FROM audit_trail GROUP BY table_name, action ORDER BY table_name, action');
  try
    WriteLn('   Table       Action    Count');
    WriteLn('   -----       ------    -----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('table_name').AsString:12,
              DS.FieldByName('action').AsString:10,
              DS.FieldByName('count').AsInteger:5);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Queries the audit trail for a specific record's full history, per-user activity summaries, and sensitive field (credit_limit) changes. }
procedure DemoQueryAuditHistory;
var
  DS: TDataSet;
begin
  WriteLn('5. Querying audit history');
  WriteLn('   ----------------------');
  WriteLn('');

  // History for specific record
  WriteLn('   a) Complete history for customer #1 (Acme Corp):');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, action, ' +
    '       COALESCE(JSON_EXTRACT(new_values, ''$.credit_limit''), ' +
    '                JSON_EXTRACT(old_values, ''$.credit_limit'')) as credit_limit, ' +
    '       COALESCE(JSON_EXTRACT(new_values, ''$.status''), ' +
    '                JSON_EXTRACT(old_values, ''$.status'')) as status, ' +
    '       user_id ' +
    'FROM audit_trail ' +
    'WHERE table_name = ''customers'' AND record_id = ''1'' ' +
    'ORDER BY timestamp');
  try
    while not DS.EOF do
    begin
      WriteLn('      [', DS.FieldByName('timestamp').AsString, '] ',
              DS.FieldByName('action').AsString:8,
              ' credit_limit=', DS.FieldByName('credit_limit').AsString,
              ' status=', DS.FieldByName('status').AsString,
              ' by ', DS.FieldByName('user_id').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Activity by user
  WriteLn('   b) Activity by user:');
  DS := Connection.ExecuteQuery(
    'SELECT user_id, COUNT(*) as actions, ' +
    '       COUNT(DISTINCT table_name) as tables_modified, ' +
    '       MIN(timestamp) as first_action, ' +
    '       MAX(timestamp) as last_action ' +
    'FROM audit_trail GROUP BY user_id ORDER BY actions DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('      ', DS.FieldByName('user_id').AsString:15,
              ' - ', DS.FieldByName('actions').AsInteger, ' actions on ',
              DS.FieldByName('tables_modified').AsInteger, ' tables');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Recent changes to sensitive fields
  WriteLn('   c) Credit limit changes (sensitive field):');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, record_id, ' +
    '       JSON_EXTRACT(old_values, ''$.credit_limit'') as old_limit, ' +
    '       JSON_EXTRACT(new_values, ''$.credit_limit'') as new_limit, ' +
    '       user_id ' +
    'FROM audit_trail ' +
    'WHERE table_name = ''customers'' ' +
    '  AND action = ''UPDATE'' ' +
    '  AND JSON_EXTRACT(old_values, ''$.credit_limit'') != JSON_EXTRACT(new_values, ''$.credit_limit'') ' +
    'ORDER BY timestamp DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('      Customer #', DS.FieldByName('record_id').AsString,
              ': ', DS.FieldByName('old_limit').AsString,
              ' -> ', DS.FieldByName('new_limit').AsString,
              ' by ', DS.FieldByName('user_id').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Computes and stores checksums for all audit records, verifies integrity, then simulates tampering and detects the mismatch. }
procedure DemoTamperDetection;
type
  TAuditRecord = record
    Id: Integer;
    Data: string;
    Checksum: string;
  end;
var
  DS: TDataSet;
  StoredChecksum, ComputedChecksum: string;
  AuditId: Integer;
  AuditData: string;
  Records: array of TAuditRecord;
  I, RecordCount: Integer;
begin
  WriteLn('6. Tamper detection with checksums');
  WriteLn('   --------------------------------');
  WriteLn('');

  // Add checksums to existing audit records
  WriteLn('   Adding checksums to audit records...');

  // First, collect all data
  SetLength(Records, 0);
  RecordCount := 0;
  DS := Connection.ExecuteQuery(
    'SELECT id, table_name, record_id, action, old_values, new_values, timestamp ' +
    'FROM audit_trail WHERE checksum IS NULL');
  try
    while not DS.EOF do
    begin
      SetLength(Records, RecordCount + 1);
      Records[RecordCount].Id := DS.FieldByName('id').AsInteger;
      Records[RecordCount].Data := DS.FieldByName('table_name').AsString +
                   DS.FieldByName('record_id').AsString +
                   DS.FieldByName('action').AsString +
                   DS.FieldByName('old_values').AsString +
                   DS.FieldByName('new_values').AsString +
                   DS.FieldByName('timestamp').AsString;
      Records[RecordCount].Checksum := CalculateChecksum(Records[RecordCount].Data);
      Inc(RecordCount);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Now update checksums
  for I := 0 to RecordCount - 1 do
    Connection.ExecuteNonQuery(
      'UPDATE audit_trail SET checksum = ? WHERE id = ?',
      [Records[I].Checksum, Records[I].Id]);

  WriteLn('   Checksums added to all records');
  WriteLn('');

  // Verify integrity
  WriteLn('   Verifying audit trail integrity...');
  DS := Connection.ExecuteQuery(
    'SELECT id, table_name, record_id, action, old_values, new_values, timestamp, checksum ' +
    'FROM audit_trail');
  try
    while not DS.EOF do
    begin
      AuditData := DS.FieldByName('table_name').AsString +
                   DS.FieldByName('record_id').AsString +
                   DS.FieldByName('action').AsString +
                   DS.FieldByName('old_values').AsString +
                   DS.FieldByName('new_values').AsString +
                   DS.FieldByName('timestamp').AsString;
      StoredChecksum := DS.FieldByName('checksum').AsString;
      ComputedChecksum := CalculateChecksum(AuditData);

      if StoredChecksum <> ComputedChecksum then
        WriteLn('   WARNING: Tampering detected in record #', DS.FieldByName('id').AsInteger);

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   All records verified - no tampering detected');
  WriteLn('');

  // Demonstrate tampering detection
  WriteLn('   Simulating tampering...');
  Connection.ExecuteNonQuery(
    'UPDATE audit_trail SET new_values = REPLACE(new_values, ''75000'', ''175000'') WHERE id = 4');

  WriteLn('   Re-verifying...');
  DS := Connection.ExecuteQuery(
    'SELECT id, table_name, record_id, action, old_values, new_values, timestamp, checksum ' +
    'FROM audit_trail WHERE id = 4');
  try
    if not DS.EOF then
    begin
      AuditData := DS.FieldByName('table_name').AsString +
                   DS.FieldByName('record_id').AsString +
                   DS.FieldByName('action').AsString +
                   DS.FieldByName('old_values').AsString +
                   DS.FieldByName('new_values').AsString +
                   DS.FieldByName('timestamp').AsString;
      StoredChecksum := DS.FieldByName('checksum').AsString;
      ComputedChecksum := CalculateChecksum(AuditData);

      if StoredChecksum <> ComputedChecksum then
      begin
        WriteLn('   ALERT: Tampering detected in audit record #4!');
        WriteLn('   Stored checksum:   ', StoredChecksum);
        WriteLn('   Computed checksum: ', ComputedChecksum);
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Generates GDPR-style data access reports, SOX-style financial change audits, and a chronological activity timeline from the audit trail. }
procedure DemoComplianceReports;
var
  DS: TDataSet;
begin
  WriteLn('7. Compliance reporting');
  WriteLn('   ---------------------');
  WriteLn('');

  // GDPR-style data access report
  WriteLn('   a) Data access report (GDPR Article 15):');
  DS := Connection.ExecuteQuery(
    'SELECT table_name, action, COUNT(*) as count, ' +
    '       GROUP_CONCAT(DISTINCT user_id) as users ' +
    'FROM audit_trail ' +
    'WHERE record_id = ''1'' ' +  // For specific customer
    'GROUP BY table_name, action');
  try
    WriteLn('      Customer #1 data was:');
    while not DS.EOF do
    begin
      WriteLn('      - ', DS.FieldByName('action').AsString, ' ',
              DS.FieldByName('count').AsInteger, ' time(s) in ',
              DS.FieldByName('table_name').AsString,
              ' by: ', DS.FieldByName('users').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // SOX-style financial changes audit
  WriteLn('   b) Financial changes audit (SOX):');
  DS := Connection.ExecuteQuery(
    'SELECT a.timestamp, a.table_name, a.record_id, ' +
    '       JSON_EXTRACT(a.old_values, ''$.credit_limit'') as old_limit, ' +
    '       JSON_EXTRACT(a.new_values, ''$.credit_limit'') as new_limit, ' +
    '       JSON_EXTRACT(a.old_values, ''$.total_amount'') as old_amount, ' +
    '       JSON_EXTRACT(a.new_values, ''$.total_amount'') as new_amount, ' +
    '       a.user_id, a.ip_address ' +
    'FROM audit_trail a ' +
    'WHERE (JSON_EXTRACT(a.old_values, ''$.credit_limit'') IS NOT NULL ' +
    '       OR JSON_EXTRACT(a.new_values, ''$.total_amount'') IS NOT NULL) ' +
    'ORDER BY a.timestamp');
  try
    WriteLn('      Timestamp            User          Table       Change');
    WriteLn('      ---------            ----          -----       ------');
    while not DS.EOF do
    begin
      Write('      ', DS.FieldByName('timestamp').AsString, '  ');
      Write(DS.FieldByName('user_id').AsString:12, '  ');
      Write(DS.FieldByName('table_name').AsString:10, '  ');
      if not DS.FieldByName('old_limit').IsNull then
        WriteLn('credit: ', DS.FieldByName('old_limit').AsString, ' -> ', DS.FieldByName('new_limit').AsString)
      else if not DS.FieldByName('new_amount').IsNull then
        WriteLn('amount: $', DS.FieldByName('new_amount').AsString)
      else
        WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Activity timeline
  WriteLn('   c) Activity timeline:');
  DS := Connection.ExecuteQuery(
    'SELECT strftime(''%H:%M:%S'', timestamp) as time, ' +
    '       user_id, action, table_name, record_id ' +
    'FROM audit_trail ORDER BY timestamp');
  try
    while not DS.EOF do
    begin
      WriteLn('      ', DS.FieldByName('time').AsString, ' ',
              DS.FieldByName('user_id').AsString:12, ' ',
              DS.FieldByName('action').AsString:8, ' ',
              DS.FieldByName('table_name').AsString, ' #',
              DS.FieldByName('record_id').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Reconstructs the historical states of a customer record over time by replaying audit trail entries in chronological order. }
procedure DemoPointInTimeRecovery;
var
  DS: TDataSet;
begin
  WriteLn('8. Point-in-time state reconstruction');
  WriteLn('   -----------------------------------');
  WriteLn('');

  WriteLn('   Reconstructing customer #1 state over time:');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, action, ' +
    '       COALESCE(new_values, old_values) as state ' +
    'FROM audit_trail ' +
    'WHERE table_name = ''customers'' AND record_id = ''1'' ' +
    'ORDER BY timestamp');
  try
    while not DS.EOF do
    begin
      WriteLn('');
      WriteLn('   At ', DS.FieldByName('timestamp').AsString, ' (', DS.FieldByName('action').AsString, '):');
      WriteLn('   ', DS.FieldByName('state').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the example database file and its WAL and SHM companions if they exist. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  WriteLn('=== NDXSQLite Example 44: Audit Trail Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example44.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupAuditSchema;
      WriteLn('Audit trail database initialized');
      WriteLn('');

      DemoBasicAuditTrail;
      DemoUpdateTracking;
      DemoDeleteTracking;
      DemoOrdersAudit;
      DemoQueryAuditHistory;
      DemoTamperDetection;
      DemoComplianceReports;
      DemoPointInTimeRecovery;

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
