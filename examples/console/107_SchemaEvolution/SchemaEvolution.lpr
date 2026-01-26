{===============================================================================
  NDXSQLite Example 107 - Schema Evolution
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Version-controlled database migrations
  - Forward and rollback migration support
  - Migration registry and history
  - Schema versioning best practices
  - Safe schema change patterns

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SchemaEvolution;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Creates the migration registry table for version tracking. }
procedure CreateMigrationRegistry;
begin
  // Schema versions table (the migration registry itself)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS schema_versions (' +
    '  version INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  applied_at TEXT,' +
    '  rolled_back_at TEXT,' +
    '  status TEXT DEFAULT ''pending''' +  // pending, applied, rolled_back
    ')');

  // Migration log (audit trail)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS migration_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  version INTEGER,' +
    '  action TEXT NOT NULL,' +  // apply, rollback, verify
    '  details TEXT,' +
    '  occurred_at TEXT NOT NULL' +
    ')');
end;

{ Queries schema_versions for the highest applied migration version number. }
function GetCurrentVersion: Integer;
var
  V: Variant;
begin
  V := Conn.ExecuteScalar('SELECT COALESCE(MAX(version), 0) FROM schema_versions WHERE status = ''applied''');
  Result := Integer(V);
end;

{ Inserts an entry into the migration_log table recording the version, action
  type, details, and a computed timestamp. }
procedure LogMigration(Version: Integer; const Action, Details: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO migration_log (version, action, details, occurred_at) ' +
    'VALUES (%d, ''%s'', ''%s'', datetime(''2024-03-01 10:00:00'', ''+%d minutes''))',
    [Version, Action, Details, Version * 5]));
end;

// === Migration v1: Initial users table ===
{ Creates the users table with username and email columns, registers migration
  v1 as applied in schema_versions, and logs the action. }
procedure ApplyV1;
begin
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  email TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery(
    'INSERT INTO schema_versions (version, name, description, applied_at, status) ' +
    'VALUES (1, ''create_users'', ''Initial users table with username and email'', ' +
    'datetime(''2024-03-01 10:05:00''), ''applied'')');
  LogMigration(1, 'apply', 'Created users table');
end;

{ Drops the users table, marks migration v1 as rolled back in schema_versions,
  and logs the rollback action. }
procedure RollbackV1;
begin
  Conn.ExecuteNonQuery('DROP TABLE IF EXISTS users');
  Conn.ExecuteNonQuery('UPDATE schema_versions SET status = ''rolled_back'', rolled_back_at = datetime(''now'') WHERE version = 1');
  LogMigration(1, 'rollback', 'Dropped users table');
end;

// === Migration v2: Add created_at column with default ===
{ Adds the created_at column to users, populates existing rows with a default
  timestamp, registers migration v2, and logs the action. }
procedure ApplyV2;
begin
  Conn.ExecuteNonQuery(
    'ALTER TABLE users ADD COLUMN created_at TEXT');

  // Populate existing rows with current timestamp
  Conn.ExecuteNonQuery('UPDATE users SET created_at = ''2024-03-01 10:00:00'' WHERE created_at IS NULL');

  Conn.ExecuteNonQuery(
    'INSERT INTO schema_versions (version, name, description, applied_at, status) ' +
    'VALUES (2, ''add_created_at'', ''Add created_at column (backward compatible)'', ' +
    'datetime(''2024-03-01 10:10:00''), ''applied'')');
  LogMigration(2, 'apply', 'Added users.created_at, populated existing rows');
end;

{ Removes the created_at column by rebuilding the users table from a backup
  copy, marks migration v2 as rolled back, and logs the action. }
procedure RollbackV2;
begin
  Conn.ExecuteNonQuery('PRAGMA foreign_keys = OFF');
  // SQLite doesn't support DROP COLUMN directly in older versions
  // Recreate table without the column
  Conn.ExecuteNonQuery('CREATE TABLE users_backup AS SELECT id, username, email FROM users');
  Conn.ExecuteNonQuery('DROP TABLE users');
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  email TEXT NOT NULL' +
    ')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email) SELECT id, username, email FROM users_backup');
  Conn.ExecuteNonQuery('DROP TABLE users_backup');
  Conn.ExecuteNonQuery('PRAGMA foreign_keys = ON');
  Conn.ExecuteNonQuery('UPDATE schema_versions SET status = ''rolled_back'', rolled_back_at = datetime(''now'') WHERE version = 2');
  LogMigration(2, 'rollback', 'Removed users.created_at');
end;

// === Migration v3: Create orders table ===
{ Creates the orders table with a foreign key to users and an index on user_id,
  registers migration v3, and logs the action. }
procedure ApplyV3;
begin
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  amount REAL NOT NULL,' +
    '  ordered_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_user ON orders(user_id)');

  Conn.ExecuteNonQuery(
    'INSERT INTO schema_versions (version, name, description, applied_at, status) ' +
    'VALUES (3, ''create_orders'', ''Create orders table with user FK (backward compatible)'', ' +
    'datetime(''2024-03-01 10:15:00''), ''applied'')');
  LogMigration(3, 'apply', 'Created orders table with index');
end;

{ Drops the orders table and its index, marks migration v3 as rolled back, and
  logs the action. }
procedure RollbackV3;
begin
  Conn.ExecuteNonQuery('DROP INDEX IF EXISTS idx_orders_user');
  Conn.ExecuteNonQuery('DROP TABLE IF EXISTS orders');
  Conn.ExecuteNonQuery('UPDATE schema_versions SET status = ''rolled_back'', rolled_back_at = datetime(''now'') WHERE version = 3');
  LogMigration(3, 'rollback', 'Dropped orders table');
end;

// === Migration v4: Add display_name, populate from username ===
{ Adds the display_name column to users and populates it from existing username
  values, registers migration v4, and logs the action. }
procedure ApplyV4;
begin
  Conn.ExecuteNonQuery('ALTER TABLE users ADD COLUMN display_name TEXT');

  // Populate from existing data
  Conn.ExecuteNonQuery('UPDATE users SET display_name = username WHERE display_name IS NULL');

  Conn.ExecuteNonQuery(
    'INSERT INTO schema_versions (version, name, description, applied_at, status) ' +
    'VALUES (4, ''add_display_name'', ''Add display_name column, populated from username'', ' +
    'datetime(''2024-03-01 10:20:00''), ''applied'')');
  LogMigration(4, 'apply', 'Added users.display_name, populated from username');
end;

{ Removes the display_name column by rebuilding the users table from a backup
  copy, marks migration v4 as rolled back, and logs the action. }
procedure RollbackV4;
begin
  Conn.ExecuteNonQuery('PRAGMA foreign_keys = OFF');
  // Recreate without display_name
  Conn.ExecuteNonQuery('CREATE TABLE users_backup AS SELECT id, username, email, created_at FROM users');
  Conn.ExecuteNonQuery('DROP TABLE users');
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  email TEXT NOT NULL,' +
    '  created_at TEXT' +
    ')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, created_at) SELECT id, username, email, created_at FROM users_backup');
  Conn.ExecuteNonQuery('DROP TABLE users_backup');
  Conn.ExecuteNonQuery('PRAGMA foreign_keys = ON');
  Conn.ExecuteNonQuery('UPDATE schema_versions SET status = ''rolled_back'', rolled_back_at = datetime(''now'') WHERE version = 4');
  LogMigration(4, 'rollback', 'Removed users.display_name');
end;

// === Migration v5: Rename email -> contact_email + backward compat view ===
{ Adds contact_email column, copies data from email, and creates a backward-
  compatible view mapping contact_email back to email for legacy code. }
procedure ApplyV5;
begin
  // SQLite: rename = add new column, copy, drop old via table rebuild
  Conn.ExecuteNonQuery('ALTER TABLE users ADD COLUMN contact_email TEXT');
  Conn.ExecuteNonQuery('UPDATE users SET contact_email = email');

  // Create backward-compatible view (old code can still use 'email')
  Conn.ExecuteNonQuery(
    'CREATE VIEW users_compat AS ' +
    'SELECT id, username, contact_email as email, created_at, display_name FROM users');

  Conn.ExecuteNonQuery(
    'INSERT INTO schema_versions (version, name, description, applied_at, status) ' +
    'VALUES (5, ''rename_email'', ''Rename email to contact_email + backward-compat view'', ' +
    'datetime(''2024-03-01 10:25:00''), ''applied'')');
  LogMigration(5, 'apply', 'Renamed email->contact_email, created users_compat view');
end;

{ Drops the compatibility view, rebuilds users without the contact_email column
  to restore the original email column, and marks migration v5 as rolled back. }
procedure RollbackV5;
begin
  Conn.ExecuteNonQuery('DROP VIEW IF EXISTS users_compat');
  Conn.ExecuteNonQuery('PRAGMA foreign_keys = OFF');
  // Remove contact_email column
  Conn.ExecuteNonQuery('CREATE TABLE users_backup AS SELECT id, username, email, created_at, display_name FROM users');
  Conn.ExecuteNonQuery('DROP TABLE users');
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  email TEXT NOT NULL,' +
    '  created_at TEXT,' +
    '  display_name TEXT' +
    ')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, created_at, display_name) ' +
    'SELECT id, username, email, created_at, display_name FROM users_backup');
  Conn.ExecuteNonQuery('DROP TABLE users_backup');
  Conn.ExecuteNonQuery('PRAGMA foreign_keys = ON');
  Conn.ExecuteNonQuery('UPDATE schema_versions SET status = ''rolled_back'', rolled_back_at = datetime(''now'') WHERE version = 5');
  LogMigration(5, 'rollback', 'Reverted email rename, dropped compat view');
end;

// === Feature 1: Apply All Migrations ===
{ Sequentially applies migrations v1 through v5, inserting seed data for users
  and orders between steps, and reports the schema version before and after. }
procedure DemoApplyMigrations;
begin
  WriteLn('=== 1. Apply Migrations (v1 through v5) ===');
  WriteLn;

  WriteLn(Format('   Current schema version: %d', [GetCurrentVersion]));
  WriteLn;

  WriteLn('   Applying v1: create_users...');
  ApplyV1;
  WriteLn('   OK - users table created');

  // Insert seed data after v1
  Conn.ExecuteNonQuery('INSERT INTO users (username, email) VALUES (''alice'', ''alice@example.com'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email) VALUES (''bob'', ''bob@example.com'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email) VALUES (''carol'', ''carol@example.com'')');
  WriteLn('   (inserted 3 seed users)');

  WriteLn;
  WriteLn('   Applying v2: add_created_at...');
  ApplyV2;
  WriteLn('   OK - created_at column added (existing rows get default)');

  WriteLn;
  WriteLn('   Applying v3: create_orders...');
  ApplyV3;
  WriteLn('   OK - orders table created');

  // Insert seed orders
  Conn.ExecuteNonQuery('INSERT INTO orders (user_id, amount, ordered_at) VALUES (1, 99.99, ''2024-01-15'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (user_id, amount, ordered_at) VALUES (1, 49.50, ''2024-02-01'')');
  Conn.ExecuteNonQuery('INSERT INTO orders (user_id, amount, ordered_at) VALUES (2, 199.00, ''2024-02-15'')');
  WriteLn('   (inserted 3 seed orders)');

  WriteLn;
  WriteLn('   Applying v4: add_display_name...');
  ApplyV4;
  WriteLn('   OK - display_name added, populated from username');

  WriteLn;
  WriteLn('   Applying v5: rename_email...');
  ApplyV5;
  WriteLn('   OK - email renamed to contact_email, compat view created');

  WriteLn;
  WriteLn(Format('   Current schema version: %d', [GetCurrentVersion]));
  WriteLn;
end;

// === Feature 2: Verify Data Integrity ===
{ Queries and prints users with the current schema columns, then the backward-
  compatible view using the old email alias, and finally the orders table. }
procedure DemoVerifyData;
begin
  WriteLn('=== 2. Verify Data After Migrations ===');
  WriteLn;

  WriteLn('   Users table (current schema):');
  DS := Conn.ExecuteQuery('SELECT id, username, contact_email, display_name FROM users ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d %-8s %-22s display=%s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('username').AsString,
         DS.FieldByName('contact_email').AsString,
         DS.FieldByName('display_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Backward-compat view (uses "email" alias):');
  DS := Conn.ExecuteQuery('SELECT id, username, email FROM users_compat ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d %-8s email=%s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('username').AsString,
         DS.FieldByName('email').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Orders (unchanged by migrations):');
  DS := Conn.ExecuteQuery(
    'SELECT o.id, u.username, o.amount, o.ordered_at ' +
    'FROM orders o JOIN users u ON u.id = o.user_id ORDER BY o.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d user=%s amount=$%.2f date=%s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('username').AsString,
         DS.FieldByName('amount').AsFloat,
         DS.FieldByName('ordered_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 3: Migration History ===
{ Lists all registered schema versions showing their version number, status,
  name, description, and application timestamp. }
procedure DemoMigrationHistory;
begin
  WriteLn('=== 3. Migration History ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT version, name, description, status, applied_at, rolled_back_at ' +
    'FROM schema_versions ORDER BY version');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   v%d [%-10s] %-20s %s',
        [DS.FieldByName('version').AsInteger,
         DS.FieldByName('status').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('description').AsString]));
      if DS.FieldByName('applied_at').AsString <> '' then
        WriteLn('      applied: ' + DS.FieldByName('applied_at').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 4: Rollback ===
{ Demonstrates rolling back a migration and verifying data preservation. }
procedure DemoRollback;
begin
  WriteLn('=== 4. Rollback Migration v5 ===');
  WriteLn;

  WriteLn(Format('   Current version before rollback: %d', [GetCurrentVersion]));
  WriteLn('   Rolling back v5 (rename_email)...');
  RollbackV5;
  WriteLn('   OK - contact_email removed, email column restored, view dropped');
  WriteLn(Format('   Current version after rollback: %d', [GetCurrentVersion]));

  WriteLn;
  WriteLn('   Users after rollback (email column is back):');
  DS := Conn.ExecuteQuery('SELECT id, username, email, display_name FROM users ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d %-8s %-22s display=%s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('username').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('display_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 5: Re-apply After Fix ===
{ Demonstrates re-applying a previously rolled-back migration with fixes. }
procedure DemoReapply;
begin
  WriteLn('=== 5. Re-apply Migration v5 (fixed version) ===');
  WriteLn;

  // Reset the version entry
  Conn.ExecuteNonQuery('DELETE FROM schema_versions WHERE version = 5');

  WriteLn('   Re-applying v5 with improved migration...');
  ApplyV5;
  WriteLn('   OK - contact_email + compat view recreated');
  WriteLn(Format('   Current version: %d', [GetCurrentVersion]));

  WriteLn;
  WriteLn('   Verify compat view works:');
  DS := Conn.ExecuteQuery('SELECT username, email FROM users_compat WHERE username = ''alice''');
  try
    if not DS.EOF then
      WriteLn(Format('   alice via compat view: email=%s', [DS.FieldByName('email').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 6: Schema Inspection ===
{ Queries sqlite_master to list all tables and views, shows column metadata for
  the users table via PRAGMA table_info, and lists all user-defined indexes. }
procedure DemoSchemaInspection;
begin
  WriteLn('=== 6. Schema Inspection ===');
  WriteLn;

  WriteLn('   Tables in database:');
  DS := Conn.ExecuteQuery(
    'SELECT name, type FROM sqlite_master ' +
    'WHERE type IN (''table'', ''view'') AND name NOT LIKE ''sqlite_%'' ' +
    'ORDER BY type, name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%s] %s',
        [DS.FieldByName('type').AsString,
         DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Users table columns:');
  DS := Conn.ExecuteQuery('PRAGMA table_info(users)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-8s nullable=%d default=%s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('type').AsString,
         1 - DS.FieldByName('notnull').AsInteger,
         DS.FieldByName('dflt_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Indexes:');
  DS := Conn.ExecuteQuery(
    'SELECT name, tbl_name FROM sqlite_master WHERE type = ''index'' AND name NOT LIKE ''sqlite_%'' ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s on %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('tbl_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 7: Audit Log ===
{ Prints the full migration_log table showing each apply and rollback action
  with its version, details, and timestamp in chronological order. }
procedure DemoAuditLog;
begin
  WriteLn('=== 7. Migration Audit Log ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT version, action, details, occurred_at FROM migration_log ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   v%d [%-8s] %s  (%s)',
        [DS.FieldByName('version').AsInteger,
         DS.FieldByName('action').AsString,
         DS.FieldByName('details').AsString,
         DS.FieldByName('occurred_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 8: Pending Migrations Check ===
{ Compares the current schema version against the target version, reports
  whether migrations are up to date, and shows per-version status breakdown. }
procedure DemoPendingCheck;
var
  CurrentVer: Integer;
  TargetVer: Integer;
begin
  WriteLn('=== 8. Migration Status Summary ===');
  WriteLn;

  CurrentVer := GetCurrentVersion;
  TargetVer := 5;

  WriteLn(Format('   Current schema version: %d', [CurrentVer]));
  WriteLn(Format('   Target schema version:  %d', [TargetVer]));

  if CurrentVer >= TargetVer then
    WriteLn('   Status: UP TO DATE')
  else
    WriteLn(Format('   Status: %d migrations pending', [TargetVer - CurrentVer]));

  WriteLn;
  WriteLn('   Version status breakdown:');
  DS := Conn.ExecuteQuery(
    'SELECT version, name, status FROM schema_versions ORDER BY version');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   v%d %-20s [%s]',
        [DS.FieldByName('version').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total migrations applied: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM schema_versions WHERE status = ''applied'''))]));
  WriteLn(Format('   Total rollbacks: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM migration_log WHERE action = ''rollback'''))]));
  WriteLn;
end;

// === Main Program ===
begin
  WriteLn('=== Example 107: Schema Evolution ===');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateMigrationRegistry;

    DemoApplyMigrations;
    DemoVerifyData;
    DemoMigrationHistory;
    DemoRollback;
    DemoReapply;
    DemoSchemaInspection;
    DemoAuditLog;
    DemoPendingCheck;

    WriteLn('=== All schema evolution features demonstrated successfully ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
