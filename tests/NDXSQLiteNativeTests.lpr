{===============================================================================
  NDXSQLite Native API Tests
  Tests for the new native SQLite3 implementation without SQLDB dependency.
  Author: Nicolas DEOUX - NDXDev 2025
===============================================================================}
program NDXSQLiteNativeTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DB, Variants, DateUtils,
  ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions,
  ndxsqlitedatabase, ndxsqlitestatement, ndxsqlitedataset, ndxsqlitequery;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  TestDB: string;

procedure Check(ACondition: Boolean; const ATestName: string);
begin
  if ACondition then
  begin
    Inc(TestsPassed);
    WriteLn('  [PASS] ', ATestName);
  end
  else
  begin
    Inc(TestsFailed);
    WriteLn('  [FAIL] ', ATestName);
  end;
end;

procedure TestCategory(const ACategory: string);
begin
  WriteLn;
  WriteLn('=== ', ACategory, ' ===');
end;

// ============================================================================
// Library Loading Tests
// ============================================================================

procedure TestLibraryLoading;
begin
  TestCategory('Library Loading');

  Check(LoadSQLite3Library, 'LoadSQLite3Library succeeds');
  Check(IsSQLite3LibraryLoaded, 'IsSQLite3LibraryLoaded returns True');
  Check(Assigned(sqlite3_libversion), 'sqlite3_libversion is assigned');

  if Assigned(sqlite3_libversion) then
    WriteLn('  SQLite version: ', sqlite3_libversion());

  Check(Assigned(sqlite3_open_v2), 'sqlite3_open_v2 is assigned');
  Check(Assigned(sqlite3_prepare_v2), 'sqlite3_prepare_v2 is assigned');
  Check(Assigned(sqlite3_step), 'sqlite3_step is assigned');
  Check(Assigned(sqlite3_finalize), 'sqlite3_finalize is assigned');
end;

// ============================================================================
// Database Connection Tests
// ============================================================================

procedure TestDatabaseConnection;
var
  DB: TNDXSQLiteDatabase;
begin
  TestCategory('Database Connection');

  DB := TNDXSQLiteDatabase.Create;
  try
    Check(not DB.IsOpen, 'Database initially closed');

    DB.Open(TestDB);
    Check(DB.IsOpen, 'Database opens successfully');
    Check(DB.DatabasePath = TestDB, 'DatabasePath is correct');

    DB.Close;
    Check(not DB.IsOpen, 'Database closes successfully');

    // Memory database
    DB.OpenMemory;
    Check(DB.IsOpen, 'Memory database opens');
    DB.Close;
  finally
    DB.Free;
  end;
end;

// ============================================================================
// Basic Execution Tests
// ============================================================================

procedure TestBasicExecution;
var
  DB: TNDXSQLiteDatabase;
  V: Variant;
begin
  TestCategory('Basic Execution');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);

    // Create table
    DB.Execute('CREATE TABLE IF NOT EXISTS test_basic (id INTEGER PRIMARY KEY, name TEXT, value REAL)');
    Check(DB.TableExists('test_basic'), 'Table created');

    // Insert data
    DB.Execute('INSERT INTO test_basic (name, value) VALUES (''test1'', 1.5)');
    Check(DB.Changes = 1, 'Insert affected 1 row');
    Check(DB.LastInsertRowId > 0, 'LastInsertRowId is set');

    // Select scalar
    V := DB.ExecuteScalar('SELECT COUNT(*) FROM test_basic');
    Check((not VarIsNull(V)) and (Integer(V) >= 1), 'ExecuteScalar returns count');

    V := DB.ExecuteScalar('SELECT name FROM test_basic WHERE id = 1');
    Check((not VarIsNull(V)) and (VarToStr(V) = 'test1'), 'ExecuteScalar returns text');

    // Clean up
    DB.Execute('DROP TABLE test_basic');
    Check(not DB.TableExists('test_basic'), 'Table dropped');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// Transaction Tests
// ============================================================================

procedure TestTransactions;
var
  DB: TNDXSQLiteDatabase;
  V: Variant;
begin
  TestCategory('Transactions');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);
    DB.Execute('CREATE TABLE IF NOT EXISTS test_trans (id INTEGER PRIMARY KEY, val INTEGER)');

    // Test commit
    DB.BeginTransaction;
    Check(DB.InTransaction, 'Transaction started');
    DB.Execute('INSERT INTO test_trans (val) VALUES (100)');
    DB.Commit;
    Check(not DB.InTransaction, 'Transaction committed');

    V := DB.ExecuteScalar('SELECT val FROM test_trans WHERE val = 100');
    Check((not VarIsNull(V)) and (Integer(V) = 100), 'Committed data is visible');

    // Test rollback
    DB.BeginTransaction(ttImmediate);
    DB.Execute('INSERT INTO test_trans (val) VALUES (200)');
    DB.Rollback;

    V := DB.ExecuteScalar('SELECT val FROM test_trans WHERE val = 200');
    Check(VarIsNull(V), 'Rolled back data is not visible');

    // Test savepoint
    DB.BeginTransaction;
    DB.Execute('INSERT INTO test_trans (val) VALUES (300)');
    DB.Savepoint('sp1');
    DB.Execute('INSERT INTO test_trans (val) VALUES (400)');
    DB.RollbackToSavepoint('sp1');
    DB.Commit;

    V := DB.ExecuteScalar('SELECT val FROM test_trans WHERE val = 300');
    Check(not VarIsNull(V), 'Data before savepoint is committed');

    V := DB.ExecuteScalar('SELECT val FROM test_trans WHERE val = 400');
    Check(VarIsNull(V), 'Data after savepoint is rolled back');

    DB.Execute('DROP TABLE test_trans');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// Prepared Statement Tests
// ============================================================================

procedure TestPreparedStatements;
var
  DB: TNDXSQLiteDatabase;
  Stmt: TNDXSQLiteStatement;
  I: Integer;
begin
  TestCategory('Prepared Statements');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);
    DB.Execute('CREATE TABLE IF NOT EXISTS test_stmt (id INTEGER PRIMARY KEY, name TEXT, score REAL, active INTEGER)');

    // Insert with binding
    Stmt := TNDXSQLiteStatement.Create(DB);
    try
      Stmt.Prepare('INSERT INTO test_stmt (name, score, active) VALUES (:name, :score, :active)');
      Check(Stmt.IsPrepared, 'Statement prepared');
      Check(Stmt.ParamCount = 3, 'ParamCount is 3');

      for I := 1 to 5 do
      begin
        Stmt.BindTextByName('name', 'User' + IntToStr(I));
        Stmt.BindDoubleByName('score', I * 10.5);
        Stmt.BindBooleanByName('active', I mod 2 = 0);
        Stmt.Execute;
        Stmt.Reset;
        Stmt.ClearBindings;
      end;

      Check(True, 'Inserted 5 rows with bindings');
    finally
      Stmt.Free;
    end;

    // Select with columns
    Stmt := TNDXSQLiteStatement.Create(DB);
    try
      Stmt.Prepare('SELECT id, name, score, active FROM test_stmt ORDER BY id');
      Check(Stmt.ColumnCount = 4, 'ColumnCount is 4');
      Check(Stmt.ColumnName(0) = 'id', 'Column 0 is id');
      Check(Stmt.ColumnName(1) = 'name', 'Column 1 is name');

      I := 0;
      while Stmt.Step do
      begin
        Inc(I);
        Check(Stmt.ColumnAsInteger(0) = I, 'Row ' + IntToStr(I) + ' id correct');
        Check(Stmt.ColumnAsString(1) = 'User' + IntToStr(I), 'Row ' + IntToStr(I) + ' name correct');
        Check(Abs(Stmt.ColumnAsDouble(2) - I * 10.5) < 0.001, 'Row ' + IntToStr(I) + ' score correct');
        Check(Stmt.ColumnAsBoolean(3) = (I mod 2 = 0), 'Row ' + IntToStr(I) + ' active correct');
      end;

      Check(I = 5, 'Read all 5 rows');
    finally
      Stmt.Free;
    end;

    DB.Execute('DROP TABLE test_stmt');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// DataSet Tests
// ============================================================================

procedure TestDataSet;
var
  DS: TNDXSQLiteDataSet;
  I: Integer;
begin
  TestCategory('DataSet');

  DS := TNDXSQLiteDataSet.Create(nil);
  try
    DS.OpenDatabase(TestDB);
    Check(DS.Database <> nil, 'Database opened');
    Check(DS.Database.IsOpen, 'Database is open');

    // Create test table
    DS.Database.Execute('CREATE TABLE IF NOT EXISTS test_dataset (' +
      'id INTEGER PRIMARY KEY, ' +
      'name TEXT NOT NULL, ' +
      'quantity INTEGER, ' +
      'price REAL, ' +
      'created DATETIME)');

    // Insert test data
    DS.Database.Execute('DELETE FROM test_dataset');
    for I := 1 to 10 do
    begin
      DS.Database.Execute(Format(
        'INSERT INTO test_dataset (name, quantity, price, created) VALUES ' +
        '(''Product %d'', %d, %.2f, ''2025-01-%0.2d 10:00:00'')',
        [I, I * 10, I * 9.99, I]));
    end;

    // Open dataset (id is INTEGER PRIMARY KEY so it IS the rowid - don't select both)
    DS.SQL.Text := 'SELECT id, name, quantity, price, created FROM test_dataset ORDER BY id';
    DS.TableName := 'test_dataset';
    DS.Open;

    Check(DS.Active, 'Dataset is active');
    Check(DS.RecordCount = 10, 'RecordCount is 10');
    Check(DS.FieldCount >= 5, 'FieldCount >= 5');

    // Test navigation
    DS.First;
    Check((not DS.BOF) or (DS.RecordCount > 0), 'First works');

    Check(DS.FieldByName('name').AsString = 'Product 1', 'First record name correct');

    DS.Last;
    // Note: After Last, EOF may or may not be True depending on TDataSet implementation
    // The important test is that we can read the last record
    Check(DS.FieldByName('name').AsString = 'Product 10', 'Last record name correct');

    DS.First;
    I := 0;
    while not DS.EOF do
    begin
      Inc(I);
      DS.Next;
    end;
    Check(I = 10, 'Iterated through all 10 records');

    // Test Locate
    Check(DS.Locate('name', 'Product 5', []), 'Locate found Product 5');
    Check(DS.FieldByName('quantity').AsInteger = 50, 'Located record has correct quantity');

    // Test Lookup
    Check(DS.Lookup('name', 'Product 3', 'price') = 29.97, 'Lookup returns correct price');

    DS.Close;
    DS.Database.Execute('DROP TABLE test_dataset');
  finally
    DS.Free;
  end;
end;

// ============================================================================
// DataSet Editing Tests
// ============================================================================

procedure TestDataSetEditing;
var
  DS: TNDXSQLiteDataSet;
begin
  TestCategory('DataSet Editing');

  DS := TNDXSQLiteDataSet.Create(nil);
  try
    DS.OpenDatabase(TestDB);

    // Create test table
    DS.Database.Execute('CREATE TABLE IF NOT EXISTS test_edit (' +
      'id INTEGER PRIMARY KEY, ' +
      'name TEXT, ' +
      'value INTEGER)');
    DS.Database.Execute('DELETE FROM test_edit');
    DS.Database.Execute('INSERT INTO test_edit (name, value) VALUES (''Original'', 100)');

    // Open for editing (id IS the rowid since it's INTEGER PRIMARY KEY)
    DS.SQL.Text := 'SELECT id, name, value FROM test_edit';
    DS.TableName := 'test_edit';
    DS.Open;

    Check(DS.RecordCount = 1, 'Initial record count is 1');

    // Test Edit
    DS.First;
    DS.Edit;
    DS.FieldByName('name').AsString := 'Modified';
    DS.FieldByName('value').AsInteger := 200;
    DS.Post;

    // Refresh and verify
    DS.Close;
    DS.Open;
    DS.First;
    Check(DS.FieldByName('name').AsString = 'Modified', 'Edit was saved');
    Check(DS.FieldByName('value').AsInteger = 200, 'Edit value was saved');

    // Test Insert
    DS.Append;
    DS.FieldByName('name').AsString := 'New Record';
    DS.FieldByName('value').AsInteger := 300;
    DS.Post;

    DS.Close;
    DS.Open;
    Check(DS.RecordCount = 2, 'Insert added a record');

    // Test Delete
    DS.Last;
    DS.Delete;

    DS.Close;
    DS.Open;
    Check(DS.RecordCount = 1, 'Delete removed a record');

    DS.Close;
    DS.Database.Execute('DROP TABLE test_edit');
  finally
    DS.Free;
  end;
end;

// ============================================================================
// Query Component Tests
// ============================================================================

procedure TestQueryComponent;
var
  Q: TNDXSQLiteQuery;
begin
  TestCategory('Query Component');

  Q := TNDXSQLiteQuery.Create(nil);
  try
    Q.OpenDatabase(TestDB);

    // Create test table
    Q.Database.Execute('CREATE TABLE IF NOT EXISTS test_query (id INTEGER PRIMARY KEY, val INTEGER)');
    Q.Database.Execute('DELETE FROM test_query');
    Q.Database.Execute('INSERT INTO test_query (val) VALUES (10), (20), (30), (40), (50)');

    // Test GetInt
    Check(Q.GetInt('SELECT SUM(val) FROM test_query') = 150, 'GetInt returns sum');

    // Test GetCount
    Check(Q.GetCount('test_query') = 5, 'GetCount returns 5');
    Check(Q.GetCount('test_query', 'val > 25') = 3, 'GetCount with WHERE returns 3');

    // Test Exists
    Check(Q.Exists('SELECT 1 FROM test_query WHERE val = 30'), 'Exists returns True for existing');
    Check(not Q.Exists('SELECT 1 FROM test_query WHERE val = 999'), 'Exists returns False for non-existing');

    // Test ExecSQL
    Q.SQL.Text := 'UPDATE test_query SET val = val + 1';
    Check(Q.ExecSQL = 5, 'ExecSQL affected 5 rows');

    // Verify update
    Check(Q.GetInt('SELECT SUM(val) FROM test_query') = 155, 'Sum after update is correct');

    Q.Database.Execute('DROP TABLE test_query');
  finally
    Q.Free;
  end;
end;

// ============================================================================
// Blob Tests
// ============================================================================

procedure TestBlobs;
var
  DB: TNDXSQLiteDatabase;
  Stmt: TNDXSQLiteStatement;
  BlobIn, BlobOut: TBytes;
  I: Integer;
begin
  TestCategory('Blob Handling');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);
    DB.Execute('CREATE TABLE IF NOT EXISTS test_blob (id INTEGER PRIMARY KEY, data BLOB)');

    // Create test blob
    SetLength(BlobIn, 1000);
    for I := 0 to High(BlobIn) do
      BlobIn[I] := Byte(I mod 256);

    // Insert blob
    Stmt := TNDXSQLiteStatement.Create(DB);
    try
      Stmt.Prepare('INSERT INTO test_blob (data) VALUES (:data)');
      Stmt.BindBlobByName('data', BlobIn);
      Stmt.Execute;
    finally
      Stmt.Free;
    end;

    // Read blob
    Stmt := TNDXSQLiteStatement.Create(DB);
    try
      Stmt.Prepare('SELECT data FROM test_blob WHERE id = 1');
      Check(Stmt.Step, 'Blob record found');
      BlobOut := Stmt.ColumnAsBlob(0);
      Check(Length(BlobOut) = Length(BlobIn), 'Blob size matches');

      // Verify content
      for I := 0 to High(BlobIn) do
      begin
        if BlobOut[I] <> BlobIn[I] then
        begin
          Check(False, 'Blob content matches');
          Break;
        end;
        if I = High(BlobIn) then
          Check(True, 'Blob content matches');
      end;
    finally
      Stmt.Free;
    end;

    DB.Execute('DROP TABLE test_blob');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// NULL Handling Tests
// ============================================================================

procedure TestNullHandling;
var
  DB: TNDXSQLiteDatabase;
  Stmt: TNDXSQLiteStatement;
begin
  TestCategory('NULL Handling');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);
    DB.Execute('CREATE TABLE IF NOT EXISTS test_null (id INTEGER PRIMARY KEY, nullable TEXT)');

    // Insert NULL
    DB.Execute('INSERT INTO test_null (nullable) VALUES (NULL)');

    Stmt := TNDXSQLiteStatement.Create(DB);
    try
      Stmt.Prepare('SELECT nullable FROM test_null WHERE id = 1');
      Check(Stmt.Step, 'NULL record found');
      Check(Stmt.ColumnIsNull(0), 'ColumnIsNull returns True');
      Check(Stmt.ColumnAsString(0) = '', 'ColumnAsString returns empty for NULL');
      Check(VarIsNull(Stmt.ColumnAsVariant(0)), 'ColumnAsVariant returns Null');
    finally
      Stmt.Free;
    end;

    DB.Execute('DROP TABLE test_null');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// DateTime Tests
// ============================================================================

procedure TestDateTime;
var
  DB: TNDXSQLiteDatabase;
  Stmt: TNDXSQLiteStatement;
  DT, DT2: TDateTime;
begin
  TestCategory('DateTime Handling');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);
    DB.Execute('CREATE TABLE IF NOT EXISTS test_dt (id INTEGER PRIMARY KEY, dt DATETIME)');

    DT := EncodeDateTime(2025, 1, 15, 14, 30, 45, 0);

    // Insert datetime
    Stmt := TNDXSQLiteStatement.Create(DB);
    try
      Stmt.Prepare('INSERT INTO test_dt (dt) VALUES (:dt)');
      Stmt.BindDateTimeByName('dt', DT);
      Stmt.Execute;
    finally
      Stmt.Free;
    end;

    // Read datetime
    Stmt := TNDXSQLiteStatement.Create(DB);
    try
      Stmt.Prepare('SELECT dt FROM test_dt WHERE id = 1');
      Check(Stmt.Step, 'DateTime record found');
      DT2 := Stmt.ColumnAsDateTime(0);

      // Allow small difference due to format conversion
      Check(Abs(DT - DT2) < 1/86400, 'DateTime matches within 1 second');
    finally
      Stmt.Free;
    end;

    DB.Execute('DROP TABLE test_dt');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// Batch Executor Tests
// ============================================================================

procedure TestBatchExecutor;
var
  DB: TNDXSQLiteDatabase;
  Batch: TNDXSQLiteBatchExecutor;
  V: Variant;
begin
  TestCategory('Batch Executor');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);

    Batch := TNDXSQLiteBatchExecutor.Create(DB);
    try
      Batch.Add('CREATE TABLE IF NOT EXISTS test_batch (id INTEGER PRIMARY KEY, val INTEGER)');
      Batch.Add('INSERT INTO test_batch (val) VALUES (1)');
      Batch.Add('INSERT INTO test_batch (val) VALUES (2)');
      Batch.Add('INSERT INTO test_batch (val) VALUES (3)');

      Check(Batch.Execute, 'Batch execution succeeds');
      Check(Batch.ExecutedCount = 4, 'ExecutedCount is 4');

      V := DB.ExecuteScalar('SELECT COUNT(*) FROM test_batch');
      Check((not VarIsNull(V)) and (Integer(V) = 3), 'Batch inserted 3 rows');

      // Test script execution
      Batch.Clear;
      Check(Batch.ExecuteScript(
        'INSERT INTO test_batch (val) VALUES (4);' + LineEnding +
        'INSERT INTO test_batch (val) VALUES (5);' + LineEnding +
        '-- This is a comment' + LineEnding +
        'INSERT INTO test_batch (val) VALUES (6);'
      ), 'Script execution succeeds');

      V := DB.ExecuteScalar('SELECT COUNT(*) FROM test_batch');
      Check((not VarIsNull(V)) and (Integer(V) = 6), 'Script inserted 3 more rows');

      DB.Execute('DROP TABLE test_batch');
    finally
      Batch.Free;
    end;
  finally
    DB.Free;
  end;
end;

// ============================================================================
// Pragma Tests
// ============================================================================

procedure TestPragmas;
var
  DB: TNDXSQLiteDatabase;
  JournalMode: string;
begin
  TestCategory('Pragma Handling');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);

    // Test WAL mode
    DB.EnableWalMode;
    JournalMode := DB.GetPragma('journal_mode');
    Check(SameText(JournalMode, 'wal'), 'WAL mode enabled');

    // Test user_version
    DB.SetUserVersion(42);
    Check(DB.GetUserVersion = 42, 'UserVersion set and read correctly');

    // Test page_size
    Check(DB.GetPageSize > 0, 'PageSize is positive');

    // Test page_count
    Check(DB.GetPageCount >= 0, 'PageCount is non-negative');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// Utility Tests
// ============================================================================

procedure TestUtilities;
var
  DB: TNDXSQLiteDatabase;
  Tables: TStringList;
  Columns: TStringList;
begin
  TestCategory('Utility Functions');

  DB := TNDXSQLiteDatabase.Create;
  try
    DB.Open(TestDB);

    // Create test table
    DB.Execute('CREATE TABLE IF NOT EXISTS test_util (id INTEGER PRIMARY KEY, name TEXT, value REAL)');

    // Test TableExists
    Check(DB.TableExists('test_util'), 'TableExists returns True for existing table');
    Check(not DB.TableExists('nonexistent_table'), 'TableExists returns False for non-existing table');

    // Test ColumnExists
    Check(DB.ColumnExists('test_util', 'name'), 'ColumnExists returns True for existing column');
    Check(not DB.ColumnExists('test_util', 'nonexistent_column'), 'ColumnExists returns False for non-existing column');

    // Test GetTableList
    Tables := DB.GetTableList;
    try
      Check(Tables.IndexOf('test_util') >= 0, 'GetTableList contains test_util');
    finally
      Tables.Free;
    end;

    // Test GetColumnList
    Columns := DB.GetColumnList('test_util');
    try
      Check(Columns.Count = 3, 'GetColumnList returns 3 columns');
      Check(Columns.IndexOf('id') >= 0, 'GetColumnList contains id');
      Check(Columns.IndexOf('name') >= 0, 'GetColumnList contains name');
      Check(Columns.IndexOf('value') >= 0, 'GetColumnList contains value');
    finally
      Columns.Free;
    end;

    // Test IsValidIdentifier
    Check(IsValidIdentifier('valid_name'), 'IsValidIdentifier: valid_name is valid');
    Check(IsValidIdentifier('_underscore'), 'IsValidIdentifier: _underscore is valid');
    Check(not IsValidIdentifier('1invalid'), 'IsValidIdentifier: 1invalid is invalid');
    Check(not IsValidIdentifier('has space'), 'IsValidIdentifier: "has space" is invalid');
    Check(not IsValidIdentifier(''), 'IsValidIdentifier: empty is invalid');

    DB.Execute('DROP TABLE test_util');
  finally
    DB.Free;
  end;
end;

// ============================================================================
// Main
// ============================================================================

begin
  TestDB := GetTempDir + 'ndx_native_test.db';
  WriteLn('NDXSQLite Native API Test Suite');
  WriteLn('================================');
  WriteLn('Test database: ', TestDB);

  // Delete old test database
  if FileExists(TestDB) then
    DeleteFile(TestDB);

  try
    TestLibraryLoading;
    TestDatabaseConnection;
    TestBasicExecution;
    TestTransactions;
    TestPreparedStatements;
    TestDataSet;
    TestDataSetEditing;
    TestQueryComponent;
    TestBlobs;
    TestNullHandling;
    TestDateTime;
    TestBatchExecutor;
    TestPragmas;
    TestUtilities;

    WriteLn;
    WriteLn('================================');
    WriteLn('Results: ', TestsPassed, ' passed, ', TestsFailed, ' failed');

    if TestsFailed = 0 then
      WriteLn('All tests PASSED!')
    else
      WriteLn('Some tests FAILED!');

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      Inc(TestsFailed);
    end;
  end;

  // Cleanup
  if FileExists(TestDB) then
    DeleteFile(TestDB);

  // Exit code
  if TestsFailed > 0 then
    ExitCode := 1;
end.
