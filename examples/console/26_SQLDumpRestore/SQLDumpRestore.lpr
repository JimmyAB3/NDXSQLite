{===============================================================================
  NDXSQLite Example 26 - SQL Dump and Restore
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates the TNDXSQLiteDump class:
  - Complete database export to SQL format
  - BLOB columns exported as X'hexadecimal'
  - Foreign key dependency ordering (topological sort)
  - Full schema export (tables, indexes, triggers, views)
  - SQL file import/restore

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SQLDumpRestore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions, ndxsqlitedump, ndxsqliteverify;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath, DumpPath, RestoredDBPath: string;

{ Creates a sample database. }
procedure CreateSampleDatabase;
begin
  WriteLn('1. Creating sample database with complex FK relationships');
  WriteLn('   ------------------------------------------------------');

  // Create tables with FK dependencies
  // Dependency chain: comments -> documents -> users
  //                   audit_log -> users
  //                   categories (no FK)
  //                   doc_categories -> documents, categories

  Connection.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  avatar BLOB' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE documents (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  title TEXT,' +
    '  content BLOB' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE comments (' +
    '  id INTEGER PRIMARY KEY,' +
    '  document_id INTEGER REFERENCES documents(id),' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  text TEXT' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE doc_categories (' +
    '  doc_id INTEGER REFERENCES documents(id),' +
    '  cat_id INTEGER REFERENCES categories(id),' +
    '  PRIMARY KEY (doc_id, cat_id)' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE audit_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  action TEXT' +
    ')');

  // Create index, view, and trigger
  Connection.ExecuteNonQuery('CREATE INDEX idx_users_email ON users(email)');
  Connection.ExecuteNonQuery(
    'CREATE VIEW user_summary AS ' +
    'SELECT id, name, (SELECT COUNT(*) FROM documents WHERE user_id = users.id) as doc_count FROM users');
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER trg_audit_insert AFTER INSERT ON documents ' +
    'BEGIN ' +
    '  INSERT INTO audit_log (user_id, action) VALUES (NEW.user_id, ''document_created''); ' +
    'END');

  // Insert data
  Connection.ExecuteNonQuery('INSERT INTO users VALUES (1, ''Alice'', ''alice@example.com'', X''DEADBEEF'')');
  Connection.ExecuteNonQuery('INSERT INTO users VALUES (2, ''Bob'', ''bob@test.org'', X''CAFEBABE'')');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (1, ''Work'')');
  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (2, ''Personal'')');
  Connection.ExecuteNonQuery('INSERT INTO documents VALUES (1, 1, ''Report'', X''48656C6C6F'')');
  Connection.ExecuteNonQuery('INSERT INTO documents VALUES (2, 2, ''Notes'', NULL)');
  Connection.ExecuteNonQuery('INSERT INTO comments VALUES (1, 1, 2, ''Great report!'')');
  Connection.ExecuteNonQuery('INSERT INTO comments VALUES (2, 1, 1, ''Thanks!'')');
  Connection.ExecuteNonQuery('INSERT INTO doc_categories VALUES (1, 1)');
  Connection.ExecuteNonQuery('INSERT INTO doc_categories VALUES (1, 2)');
  // Note: audit_log already has entries from the trigger (trg_audit_insert)
  // Add a manual entry with explicit id to avoid conflict
  Connection.ExecuteNonQuery('INSERT INTO audit_log VALUES (100, 1, ''login'')');

  WriteLn('   Tables with FK dependencies created:');
  WriteLn('     - users, categories (no FK)');
  WriteLn('     - documents -> users');
  WriteLn('     - audit_log -> users');
  WriteLn('     - comments -> documents, users');
  WriteLn('     - doc_categories -> documents, categories');
  WriteLn('   Plus: 1 index, 1 view, 1 trigger');
  WriteLn('');
end;

{ Exports the database to a SQL file using TNDXSQLiteDump, displaying the FK
  dependency order and reporting export statistics (tables, indexes, views, triggers). }
procedure DemoExportWithUnit;
var
  Dump: TNDXSQLiteDump;
  DumpResult: TNDXDumpResult;
  TableOrder: TStringList;
  I: Integer;
begin
  WriteLn('2. Using TNDXSQLiteDump to export database');
  WriteLn('   ---------------------------------------');

  Dump := TNDXSQLiteDump.Create(Connection);
  try
    // Show FK dependency order
    TableOrder := Dump.GetTableExportOrder;
    try
      WriteLn('   FK dependency order (parent tables first):');
      for I := 0 to TableOrder.Count - 1 do
        WriteLn('     ', I+1, '. ', TableOrder[I]);
    finally
      TableOrder.Free;
    end;
    WriteLn('');

    // Export to SQL file
    DumpResult := Dump.ExportToSQL(DumpPath);

    if DumpResult.Success then
    begin
      WriteLn('   Export successful!');
      WriteLn('     File: ', DumpResult.FilePath);
      WriteLn('     Size: ', DumpResult.FileSize, ' bytes');
      WriteLn('     Tables: ', DumpResult.TableCount);
      WriteLn('     Indexes: ', DumpResult.IndexCount);
      WriteLn('     Views: ', DumpResult.ViewCount);
      WriteLn('     Triggers: ', DumpResult.TriggerCount);
    end
    else
      WriteLn('   Export failed: ', DumpResult.ErrorMessage);
  finally
    Dump.Free;
  end;
  WriteLn('');
end;

{ Displays dump preview information. }
procedure ShowDumpPreview;
var
  Lines: TStringList;
  I, MaxLines: Integer;
begin
  WriteLn('3. SQL dump content preview');
  WriteLn('   ------------------------');

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(DumpPath);
    MaxLines := 60;
    if Lines.Count < MaxLines then
      MaxLines := Lines.Count;

    for I := 0 to MaxLines - 1 do
      WriteLn('   ', Lines[I]);

    if Lines.Count > MaxLines then
      WriteLn('   ... (', Lines.Count - MaxLines, ' more lines)');
  finally
    Lines.Free;
  end;
  WriteLn('');
end;

{ Restores a database from the SQL dump file into a new database using
  TNDXSQLiteDump.ImportFromSQL with foreign key constraints enabled. }
procedure DemoRestoreWithUnit;
var
  RestoreConn: TNDXSQLiteConnection;
  RestoreOpts: TNDXSQLiteConnectionOptions;
  Dump: TNDXSQLiteDump;
  ImportResult: TNDXImportResult;
  ImportOpts: TNDXImportOptions;
begin
  WriteLn('4. Using TNDXSQLiteDump to restore database');
  WriteLn('   ----------------------------------------');

  if FileExists(RestoredDBPath) then
    DeleteFile(RestoredDBPath);

  RestoreOpts := TNDXSQLiteConnectionOptions.Create;
  try
    RestoreOpts.DatabasePath := RestoredDBPath;
    RestoreConn := TNDXSQLiteConnection.Create(RestoreOpts);
    try
      RestoreConn.Open;

      Dump := TNDXSQLiteDump.Create(RestoreConn);
      try
        // Import with FK constraints ENABLED to prove correct ordering
        ImportOpts := TNDXSQLiteDump.DefaultImportOptions;
        ImportOpts.EnableForeignKeys := True;

        WriteLn('   Importing with PRAGMA foreign_keys=ON...');
        ImportResult := Dump.ImportFromSQL(DumpPath, ImportOpts);
        try
          if ImportResult.Success then
          begin
            WriteLn('   SUCCESS! Import completed without FK violations.');
            WriteLn('   Statements executed: ', ImportResult.StatementsExecuted);
          end
          else
          begin
            WriteLn('   Import failed: ', ImportResult.ErrorMessage);
            WriteLn('   Errors: ', ImportResult.ErrorCount);
          end;
        finally
          ImportResult.Errors.Free;
        end;
      finally
        Dump.Free;
      end;

      RestoreConn.Close;
    finally
      RestoreConn.Free;
    end;
  finally
    RestoreOpts.Free;
  end;
  WriteLn('');
end;

{ Compares the original and restored databases using TNDXSQLiteVerify, reporting
  schema counts, data match status, any differences, and BLOB data preservation. }
procedure DemoVerifyIntegrity;
var
  RestoreConn: TNDXSQLiteConnection;
  RestoreOpts: TNDXSQLiteConnectionOptions;
  Verify: TNDXSQLiteVerify;
  CompareResult: TNDXCompareResult;
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('5. Verifying data integrity using TNDXSQLiteVerify');
  WriteLn('   -----------------------------------------------');

  RestoreOpts := TNDXSQLiteConnectionOptions.Create;
  try
    RestoreOpts.DatabasePath := RestoredDBPath;
    RestoreConn := TNDXSQLiteConnection.Create(RestoreOpts);
    try
      RestoreConn.Open;

      // Use TNDXSQLiteVerify to compare databases
      Verify := TNDXSQLiteVerify.Create(Connection);
      try
        CompareResult := Verify.CompareWith(RestoreConn);
        try
          WriteLn('   Schema comparison:');
          WriteLn('     Tables: source=', CompareResult.SourceSchema.TableCount,
                  ', target=', CompareResult.TargetSchema.TableCount);
          WriteLn('     Indexes: source=', CompareResult.SourceSchema.IndexCount,
                  ', target=', CompareResult.TargetSchema.IndexCount);
          WriteLn('     Views: source=', CompareResult.SourceSchema.ViewCount,
                  ', target=', CompareResult.TargetSchema.ViewCount);
          WriteLn('     Triggers: source=', CompareResult.SourceSchema.TriggerCount,
                  ', target=', CompareResult.TargetSchema.TriggerCount);
          WriteLn('');
          WriteLn('   Schemas match: ', CompareResult.SchemasMatch);
          WriteLn('   Data matches: ', CompareResult.DataMatches);
          WriteLn('   Overall success: ', CompareResult.Success);

          if CompareResult.Differences.Count > 0 then
          begin
            WriteLn('');
            WriteLn('   Differences found:');
            for I := 0 to CompareResult.Differences.Count - 1 do
              WriteLn('     - ', CompareResult.Differences[I]);
          end;
        finally
          CompareResult.Differences.Free;
        end;
      finally
        Verify.Free;
      end;

      // Verify BLOB data still preserved
      WriteLn('');
      DS := Connection.ExecuteQuery('SELECT hex(avatar) as h FROM users WHERE id=1');
      try
        WriteLn('   BLOB preserved: ', DS.FieldByName('h').AsString);
      finally
        DS.Free;
      end;

      RestoreConn.Close;
    finally
      RestoreConn.Free;
    end;
  finally
    RestoreOpts.Free;
  end;
  WriteLn('');
end;

{ Deletes the source database, SQL dump, and restored database files. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DumpPath) then DeleteFile(DumpPath);
  if FileExists(RestoredDBPath) then DeleteFile(RestoredDBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 26: SQL Dump and Restore ===');
  WriteLn('    Using TNDXSQLiteDump from ndxsqlitedump unit');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example26.db';
  DumpPath := ExtractFilePath(ParamStr(0)) + 'example26_dump.sql';
  RestoredDBPath := ExtractFilePath(ParamStr(0)) + 'example26_restored.db';

  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;
    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      CreateSampleDatabase;
      DemoExportWithUnit;
      ShowDumpPreview;
      DemoRestoreWithUnit;
      DemoVerifyIntegrity;

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
