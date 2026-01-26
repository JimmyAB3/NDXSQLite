{===============================================================================
  NDXSQLite Example 11 - Backup and Restore
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Hot backup (while database is in use)
  - Complete database with tables, indexes, views, triggers, BLOBs
  - Backup verification
  - Full restore verification (schema + data + triggers working)

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program BackupRestore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqlitebackup, ndxsqliteverify;

var
  Conn: INDXSQLiteConnection;
  Backup: TNDXSQLiteBackup;
  BackupResult: TNDXBackupResult;
  BackupOptions: TNDXBackupOptions;
  DBPath, BackupPath, RestorePath: string;

{ Creates a complete database. }
procedure CreateCompleteDatabase;
var
  I: Integer;
begin
  WriteLn('1. Creating complete test database');
  WriteLn('   --------------------------------');

  // Tables with FK relationships
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  avatar BLOB' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE documents (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  category_id INTEGER REFERENCES categories(id),' +
    '  title TEXT,' +
    '  content BLOB' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE audit_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  table_name TEXT,' +
    '  action TEXT,' +
    '  record_id INTEGER,' +
    '  ts TEXT' +
    ')');

  // Index
  Conn.ExecuteNonQuery('CREATE INDEX idx_users_email ON users(email)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_documents_user ON documents(user_id)');

  // View
  Conn.ExecuteNonQuery(
    'CREATE VIEW v_user_documents AS ' +
    'SELECT u.name as user_name, d.title, c.name as category ' +
    'FROM documents d ' +
    'JOIN users u ON d.user_id = u.id ' +
    'LEFT JOIN categories c ON d.category_id = c.id');

  // Triggers
  Conn.ExecuteNonQuery(
    'CREATE TRIGGER trg_doc_insert AFTER INSERT ON documents BEGIN ' +
    '  INSERT INTO audit_log (table_name, action, record_id, ts) ' +
    '  VALUES (''documents'', ''INSERT'', NEW.id, datetime(''now'')); ' +
    'END');

  Conn.ExecuteNonQuery(
    'CREATE TRIGGER trg_doc_delete AFTER DELETE ON documents BEGIN ' +
    '  INSERT INTO audit_log (table_name, action, record_id, ts) ' +
    '  VALUES (''documents'', ''DELETE'', OLD.id, datetime(''now'')); ' +
    'END');

  // Insert data with BLOBs
  Conn.BeginTransaction;

  // Users with BLOB avatars
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (1, ''Alice'', ''alice@example.com'', X''DEADBEEF01020304'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (2, ''Bob'', ''bob@test.org'', X''CAFEBABE05060708'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (3, ''Charlie'', ''charlie@demo.net'', NULL)');

  // Categories
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (1, ''Work'')');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (2, ''Personal'')');
  Conn.ExecuteNonQuery('INSERT INTO categories VALUES (3, ''Archive'')');

  // Documents (triggers will fire and populate audit_log)
  for I := 1 to 50 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO documents (user_id, category_id, title, content) VALUES (?, ?, ?, ?)',
      [((I - 1) mod 3) + 1,
       ((I - 1) mod 3) + 1,
       Format('Document %d', [I]),
       Format('Content for document %d with some data...', [I])]);
  end;

  Conn.Commit;

  WriteLn('   Tables: users, categories, documents, audit_log');
  WriteLn('   Indexes: idx_users_email, idx_documents_user');
  WriteLn('   Views: v_user_documents');
  WriteLn('   Triggers: trg_doc_insert, trg_doc_delete');
  WriteLn('   Users: 3 (with BLOB avatars)');
  WriteLn('   Categories: 3');
  WriteLn('   Documents: 50');
  WriteLn('   Audit log entries: ', Conn.ExecuteScalar('SELECT COUNT(*) FROM audit_log'), ' (from INSERT triggers)');
  WriteLn('');
end;

{ Validates restored database. }
procedure VerifyRestoredDatabase;
var
  Verify: TNDXSQLiteVerify;
  Schema: TNDXSchemaCount;
  VerifyResult: TNDXVerifyResult;
  DS: TDataSet;
  UserCount, DocCount, AuditCount, AuditCountAfterInsert: Integer;
  BlobHex: string;
  AllOK: Boolean;
begin
  WriteLn('5. Verifying restored database using TNDXSQLiteVerify');
  WriteLn('   --------------------------------------------------');
  AllOK := True;

  Verify := TNDXSQLiteVerify.Create(Conn as TNDXSQLiteConnection);
  try
    // Use TNDXSQLiteVerify to get schema counts
    Schema := Verify.GetSchema;

    // Verify schema
    Write('   Tables: ', Schema.TableCount);
    if Schema.TableCount = 4 then WriteLn(' [OK]') else begin WriteLn(' [FAIL - expected 4]'); AllOK := False; end;

    Write('   Indexes: ', Schema.IndexCount);
    if Schema.IndexCount >= 2 then WriteLn(' [OK]') else begin WriteLn(' [FAIL - expected >= 2]'); AllOK := False; end;

    Write('   Views: ', Schema.ViewCount);
    if Schema.ViewCount = 1 then WriteLn(' [OK]') else begin WriteLn(' [FAIL - expected 1]'); AllOK := False; end;

    Write('   Triggers: ', Schema.TriggerCount);
    if Schema.TriggerCount = 2 then WriteLn(' [OK]') else begin WriteLn(' [FAIL - expected 2]'); AllOK := False; end;

    // Verify data counts
    UserCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');
    DocCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM documents');
    AuditCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM audit_log');

    Write('   Users: ', UserCount);
    if UserCount = 3 then WriteLn(' [OK]') else begin WriteLn(' [FAIL - expected 3]'); AllOK := False; end;

    Write('   Documents: ', DocCount);
    if DocCount = 50 then WriteLn(' [OK]') else begin WriteLn(' [FAIL - expected 50]'); AllOK := False; end;

    Write('   Audit log: ', AuditCount);
    if AuditCount = 50 then WriteLn(' [OK]') else begin WriteLn(' [FAIL - expected 50]'); AllOK := False; end;

    // Verify BLOB using TNDXSQLiteVerify
    Write('   BLOBs preserved: ');
    if Verify.VerifyBlobsExist('users', 'avatar') then WriteLn('YES [OK]') else begin WriteLn('NO [FAIL]'); AllOK := False; end;

    // Verify actual BLOB value
    DS := (Conn as TNDXSQLiteConnection).ExecuteQuery('SELECT hex(avatar) as h FROM users WHERE id=1');
    try
      BlobHex := DS.FieldByName('h').AsString;
    finally
      DS.Free;
    end;

    Write('   BLOB value: ', BlobHex);
    if BlobHex = 'DEADBEEF01020304' then WriteLn(' [OK]') else begin WriteLn(' [FAIL]'); AllOK := False; end;

    // Verify views work using TNDXSQLiteVerify
    Write('   Views working: ');
    if Verify.VerifyViewsWork then WriteLn('YES [OK]') else begin WriteLn('NO [FAIL]'); AllOK := False; end;

    // Verify triggers WORK (not just exist)
    WriteLn('');
    WriteLn('   Testing triggers actually work after restore:');

    // Insert a new document - trigger should fire
    Conn.ExecuteNonQuery('INSERT INTO documents (user_id, category_id, title) VALUES (1, 1, ''Test After Restore'')');
    AuditCountAfterInsert := Conn.ExecuteScalar('SELECT COUNT(*) FROM audit_log');

    Write('   INSERT trigger fired: ');
    if AuditCountAfterInsert = AuditCount + 1 then
      WriteLn('YES [OK]')
    else
    begin
      WriteLn('NO [FAIL - audit count unchanged]');
      AllOK := False;
    end;

    // Delete the test document - trigger should fire
    Conn.ExecuteNonQuery('DELETE FROM documents WHERE title=''Test After Restore''');
    AuditCountAfterInsert := Conn.ExecuteScalar('SELECT COUNT(*) FROM audit_log');

    Write('   DELETE trigger fired: ');
    if AuditCountAfterInsert = AuditCount + 2 then
      WriteLn('YES [OK]')
    else
    begin
      WriteLn('NO [FAIL]');
      AllOK := False;
    end;

  finally
    Verify.Free;
  end;

  WriteLn('');
  if AllOK then
    WriteLn('   >>> ALL VERIFICATIONS PASSED <<<')
  else
    WriteLn('   >>> SOME VERIFICATIONS FAILED <<<');
  WriteLn('');
end;

begin
  WriteLn('=== NDXSQLite Example 11: Backup and Restore ===');
  WriteLn('    Complete database backup with full verification');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example11.db';
  BackupPath := ExtractFilePath(ParamStr(0)) + 'example11_backup.db';
  RestorePath := ExtractFilePath(ParamStr(0)) + 'example11_restored.db';

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(BackupPath) then DeleteFile(BackupPath);
  if FileExists(RestorePath) then DeleteFile(RestorePath);

  // 1. Create complete database
  Conn := TNDXSQLiteConnection.Create(DBPath, True);
  Conn.Open;
  CreateCompleteDatabase;

  // 2. Backup with verification
  WriteLn('2. Creating backup');
  WriteLn('   ---------------');
  Backup := TNDXSQLiteBackup.Create(Conn);
  try
    BackupOptions.PagesPerStep := 100;
    BackupOptions.StepDelayMs := 0;
    BackupOptions.OverwriteExisting := True;
    BackupOptions.VerifyAfterBackup := True;

    BackupResult := Backup.BackupTo(BackupPath, BackupOptions);

    if BackupResult.Success then
    begin
      WriteLn('   Backup successful!');
      WriteLn('   File: ', BackupPath);
      WriteLn('   Size: ', BackupResult.DestinationSizeBytes, ' bytes');
      WriteLn('   Pages: ', BackupResult.TotalPages);
      WriteLn('   Time: ', BackupResult.ElapsedTimeMs, ' ms');
      WriteLn('   Verified: ', BackupResult.Verified);
    end
    else
    begin
      WriteLn('   Backup FAILED: ', BackupResult.ErrorMessage);
      Halt(1);
    end;
  finally
    Backup.Free;
  end;
  WriteLn('');

  // 3. Verify backup file integrity
  WriteLn('3. Verifying backup file integrity');
  WriteLn('   --------------------------------');
  Backup := TNDXSQLiteBackup.Create(Conn);
  try
    if Backup.VerifyBackup(BackupPath) then
      WriteLn('   Integrity check: PASSED')
    else
    begin
      WriteLn('   Integrity check: FAILED');
      Halt(1);
    end;
  finally
    Backup.Free;
  end;
  WriteLn('');

  Conn.Close;
  Conn := nil;

  // 4. Restore to new database
  WriteLn('4. Restoring from backup');
  WriteLn('   ----------------------');
  Conn := TNDXSQLiteConnection.Create(RestorePath, True);
  Conn.Open;

  Backup := TNDXSQLiteBackup.Create(Conn);
  try
    BackupResult := Backup.RestoreFrom(BackupPath);
    if BackupResult.Success then
    begin
      WriteLn('   Restore successful!');
      WriteLn('   Time: ', BackupResult.ElapsedTimeMs, ' ms');
    end
    else
    begin
      WriteLn('   Restore FAILED: ', BackupResult.ErrorMessage);
      Halt(1);
    end;
  finally
    Backup.Free;
  end;
  WriteLn('');

  // 5. Full verification of restored database
  VerifyRestoredDatabase;

  Conn.Close;
  Conn := nil;

  WriteLn('=== Example completed successfully! ===');

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(BackupPath) then DeleteFile(BackupPath);
  if FileExists(RestorePath) then DeleteFile(RestorePath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end.
