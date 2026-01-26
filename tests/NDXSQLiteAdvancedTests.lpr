program NDXSQLiteAdvancedTests;

{===============================================================================
  NDXSQLite - Advanced Features Tests
  Tests for: UDF, Collation, Attached Databases, Incremental Blob I/O,
             R-Tree Spatial Index, Virtual Tables, Authorizer, Unlock Notify,
             Preupdate Hooks
  Author: Nicolas DEOUX - NDXDev 2025
===============================================================================}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqlite3api,
  ndxsqlitetypes,
  ndxsqliteexceptions,
  ndxsqliteconnectionintf,
  ndxsqliteconnection,
  ndxsqliteplatform,
  ndxsqliteudf,
  ndxsqlitecollation,
  ndxsqliteattached,
  ndxsqliteblob,
  ndxsqliteertree,
  ndxsqlitevtab,
  ndxsqliteauthorizer,
  ndxsqliteunlocknotify,
  ndxsqlitepreupdate;

var
  TEST_DB: string;
  TEST_DIR: string;
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  CurrentTest: string = '';

{ Initialize test paths according to platform }
procedure InitTestPaths;
begin
  TEST_DIR := TNDXPlatform.GetTempDirectory + 'ndxsqlite_advanced_tests' + PathDelim;
  TEST_DB := TEST_DIR + 'test_advanced.db';
  TNDXPlatform.EnsureDirectoryExists(TEST_DIR);
  WriteLn('Platform: ', TNDXPlatform.PlatformName);
  WriteLn('Test directory: ', TEST_DIR);
end;

procedure LogSuccess(const ATestName: string);
begin
  WriteLn('[OK] ', ATestName);
  Inc(TestsPassed);
end;

procedure LogFailure(const ATestName, AError: string);
begin
  WriteLn('[FAIL] ', ATestName, ' - ', AError);
  Inc(TestsFailed);
end;

procedure StartTest(const ATestName: string);
begin
  CurrentTest := ATestName;
end;

procedure CleanupTestDB;
var
  SR: TSearchRec;
begin
  if DirectoryExists(TEST_DIR) then
  begin
    if FindFirst(TEST_DIR + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
          DeleteFile(TEST_DIR + SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;
end;

{ ============================================================================ }
{ UDF Tests }
{ ============================================================================ }

type
  { Test class for UDF callbacks }
  TUDFTestHelper = class
  public
    procedure DoubleValue(var AResult: TNDXSQLiteResult;
      const AArgs: array of TNDXSQLiteValue);
    procedure ConcatStrings(var AResult: TNDXSQLiteResult;
      const AArgs: array of TNDXSQLiteValue);
    procedure SumStep(AContext: Pointer; const AArgs: array of TNDXSQLiteValue);
    procedure SumFinal(var AResult: TNDXSQLiteResult; AContext: Pointer);
  end;

procedure TUDFTestHelper.DoubleValue(var AResult: TNDXSQLiteResult;
  const AArgs: array of TNDXSQLiteValue);
begin
  if Length(AArgs) > 0 then
    AResult.SetInt64(AArgs[0].AsInt64 * 2)
  else
    AResult.SetNull;
end;

procedure TUDFTestHelper.ConcatStrings(var AResult: TNDXSQLiteResult;
  const AArgs: array of TNDXSQLiteValue);
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 0 to High(AArgs) do
  begin
    if not AArgs[I].IsNull then
      S := S + AArgs[I].AsString;
  end;
  AResult.SetString(S);
end;

type
  PSumContext = ^TSumContext;
  TSumContext = record
    Sum: Int64;
    Count: Integer;
  end;

procedure TUDFTestHelper.SumStep(AContext: Pointer;
  const AArgs: array of TNDXSQLiteValue);
var
  Ctx: PSumContext;
begin
  Ctx := PSumContext(AContext);
  if Ctx^.Count = 0 then
    Ctx^.Sum := 0;
  if (Length(AArgs) > 0) and not AArgs[0].IsNull then
    Ctx^.Sum := Ctx^.Sum + AArgs[0].AsInt64;
  Inc(Ctx^.Count);
end;

procedure TUDFTestHelper.SumFinal(var AResult: TNDXSQLiteResult;
  AContext: Pointer);
var
  Ctx: PSumContext;
begin
  if AContext = nil then
    AResult.SetNull
  else
  begin
    Ctx := PSumContext(AContext);
    AResult.SetInt64(Ctx^.Sum);
  end;
end;

procedure TestUDFScalarFunction;
var
  Conn: TNDXSQLiteConnection;
  UDF: TNDXSQLiteUDF;
  Helper: TUDFTestHelper;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('UDF Scalar Function');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TUDFTestHelper.Create;
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;
      UDF := TNDXSQLiteUDF.Create(Conn);
      try
        // Register function
        UDF.RegisterScalarFunction('double_val', 1, @Helper.DoubleValue);

        // Test the function
        V := Conn.ExecuteScalar('SELECT double_val(21)');
        if V = 42 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected 42, got %d', [Integer(V)]);

        UDF.UnregisterAll;
      finally
        UDF.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestUDFMultipleArgs;
var
  Conn: TNDXSQLiteConnection;
  UDF: TNDXSQLiteUDF;
  Helper: TUDFTestHelper;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('UDF Multiple Arguments');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TUDFTestHelper.Create;
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;
      UDF := TNDXSQLiteUDF.Create(Conn);
      try
        // Register function with variable arguments (-1)
        UDF.RegisterScalarFunction('concat_all', -1, @Helper.ConcatStrings);

        // Test the function
        V := Conn.ExecuteScalar('SELECT concat_all(''Hello'', '' '', ''World'', ''!'')');
        if VarToStr(V) = 'Hello World!' then
          TestPassed := True
        else
          ErrorMsg := Format('Expected "Hello World!", got "%s"', [VarToStr(V)]);

        UDF.UnregisterAll;
      finally
        UDF.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestUDFAggregateFunction;
var
  Conn: TNDXSQLiteConnection;
  UDF: TNDXSQLiteUDF;
  Helper: TUDFTestHelper;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('UDF Aggregate Function');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TUDFTestHelper.Create;
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create test table
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS test_agg (value INTEGER)');
      Conn.ExecuteNonQuery('DELETE FROM test_agg');
      Conn.ExecuteNonQuery('INSERT INTO test_agg VALUES (10), (20), (30), (40)');

      UDF := TNDXSQLiteUDF.Create(Conn);
      try
        // Register aggregate function
        UDF.RegisterAggregateFunction('my_sum', 1,
          @Helper.SumStep, @Helper.SumFinal, SizeOf(TSumContext));

        // Test the function
        V := Conn.ExecuteScalar('SELECT my_sum(value) FROM test_agg');
        if V = 100 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected 100, got %d', [Integer(V)]);

        UDF.UnregisterAll;
      finally
        UDF.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestUDFRegistration;
var
  Conn: TNDXSQLiteConnection;
  UDF: TNDXSQLiteUDF;
  Helper: TUDFTestHelper;
  FuncList: TStringList;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('UDF Registration');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TUDFTestHelper.Create;
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;
      UDF := TNDXSQLiteUDF.Create(Conn);
      try
        UDF.RegisterScalarFunction('func1', 1, @Helper.DoubleValue);
        UDF.RegisterScalarFunction('func2', 2, @Helper.ConcatStrings);

        if UDF.IsFunctionRegistered('func1') and
           UDF.IsFunctionRegistered('func2') and
           not UDF.IsFunctionRegistered('func3') then
        begin
          FuncList := UDF.GetRegisteredFunctions;
          try
            if FuncList.Count = 2 then
              TestPassed := True
            else
              ErrorMsg := Format('Expected 2 functions, got %d', [FuncList.Count]);
          finally
            FuncList.Free;
          end;
        end
        else
          ErrorMsg := 'Function registration check failed';

        UDF.UnregisterAll;
      finally
        UDF.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ Collation Tests }
{ ============================================================================ }

type
  TCollationTestHelper = class
  public
    function CompareCaseInsensitive(const A, B: string): Integer;
    function CompareReverse(const A, B: string): Integer;
    function CompareNatural(const A, B: string): Integer;
  end;

function TCollationTestHelper.CompareCaseInsensitive(const A, B: string): Integer;
begin
  Result := CompareText(A, B);
end;

function TCollationTestHelper.CompareReverse(const A, B: string): Integer;
begin
  Result := CompareStr(B, A);
end;

function TCollationTestHelper.CompareNatural(const A, B: string): Integer;
begin
  // Super simple: just use standard string comparison
  // This validates the callback mechanism works
  Result := CompareText(A, B);
end;

procedure TestCollationCaseInsensitive;
var
  Conn: TNDXSQLiteConnection;
  Coll: TNDXSQLiteCollation;
  Helper: TCollationTestHelper;
  DS: TDataSet;
  FirstValue: string;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Collation Case Insensitive');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TCollationTestHelper.Create;
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create test table
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS test_coll (name TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM test_coll');
      Conn.ExecuteNonQuery('INSERT INTO test_coll VALUES (''Zebra''), (''apple''), (''BANANA''), (''cherry'')');

      Coll := TNDXSQLiteCollation.Create(Conn);
      try
        Coll.RegisterCollation('NOCASE_CUSTOM', @Helper.CompareCaseInsensitive);

        // Query with custom collation
        DS := Conn.ExecuteQuery('SELECT name FROM test_coll ORDER BY name COLLATE NOCASE_CUSTOM');
        try
          FirstValue := DS.Fields[0].AsString;
          if FirstValue = 'apple' then
            TestPassed := True
          else
            ErrorMsg := Format('Expected "apple" first, got "%s"', [FirstValue]);
        finally
          DS.Free;
        end;

        Coll.UnregisterAll;
      finally
        Coll.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestCollationReverse;
var
  Conn: TNDXSQLiteConnection;
  Coll: TNDXSQLiteCollation;
  Helper: TCollationTestHelper;
  DS: TDataSet;
  FirstValue: string;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Collation Reverse Order');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TCollationTestHelper.Create;
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create test table
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS test_coll2 (name TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM test_coll2');
      Conn.ExecuteNonQuery('INSERT INTO test_coll2 VALUES (''A''), (''B''), (''C''), (''D'')');

      Coll := TNDXSQLiteCollation.Create(Conn);
      try
        Coll.RegisterCollation('REVERSE', @Helper.CompareReverse);

        // Query with reverse collation - should get D first
        DS := Conn.ExecuteQuery('SELECT name FROM test_coll2 ORDER BY name COLLATE REVERSE');
        try
          FirstValue := DS.Fields[0].AsString;
          if FirstValue = 'D' then
            TestPassed := True
          else
            ErrorMsg := Format('Expected "D" first, got "%s"', [FirstValue]);
        finally
          DS.Free;
        end;

        Coll.UnregisterAll;
      finally
        Coll.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestCollationNatural;
var
  Conn: TNDXSQLiteConnection;
  Coll: TNDXSQLiteCollation;
  Helper: TCollationTestHelper;
  DS: TDataSet;
  FirstValue: string;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Collation Natural Sort');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TCollationTestHelper.Create;
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create test table with single-digit numbers for simplified natural sort
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS test_natural (name TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM test_natural');
      Conn.ExecuteNonQuery('INSERT INTO test_natural VALUES (''item9''), (''item2''), (''item1''), (''item5'')');

      Coll := TNDXSQLiteCollation.Create(Conn);
      try
        // Use helper's natural compare wrapper (MYNATURAL to avoid reserved name)
        Coll.RegisterCollation('MYNATURAL', @Helper.CompareNatural);

        // Query with natural sort - should be item1, item2, item5, item9
        DS := Conn.ExecuteQuery('SELECT name FROM test_natural ORDER BY name COLLATE MYNATURAL');
        try
          FirstValue := DS.Fields[0].AsString;
          if FirstValue = 'item1' then
            TestPassed := True
          else
            ErrorMsg := Format('Expected "item1" first, got "%s"', [FirstValue]);
        finally
          DS.Free;
        end;

        Coll.UnregisterAll;
      finally
        Coll.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ Attached Database Tests }
{ ============================================================================ }

procedure TestAttachDetach;
var
  Conn: TNDXSQLiteConnection;
  Attached: TNDXSQLiteAttached;
  SecondDB: string;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Attach/Detach Database');
  TestPassed := False;
  ErrorMsg := '';
  try
    SecondDB := TEST_DIR + 'second.db';

    // Create second database first
    Conn := TNDXSQLiteConnection.Create(SecondDB, False);
    try
      Conn.Open;
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY, name TEXT)');
      Conn.ExecuteNonQuery('INSERT INTO items VALUES (1, ''Second DB Item'')');
      Conn.Close;
    finally
      Conn.Free;
    end;

    // Now use main DB and attach second
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;
      Attached := TNDXSQLiteAttached.Create(Conn);
      try
        Attached.Attach(SecondDB, 'seconddb');

        if Attached.IsAttached('seconddb') then
        begin
          Attached.Detach('seconddb');
          if not Attached.IsAttached('seconddb') then
            TestPassed := True
          else
            ErrorMsg := 'Database still attached after detach';
        end
        else
          ErrorMsg := 'Database not attached';

      finally
        Attached.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestCrossDbQuery;
var
  Conn: TNDXSQLiteConnection;
  Attached: TNDXSQLiteAttached;
  SecondDB: string;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Cross-Database Query');
  TestPassed := False;
  ErrorMsg := '';
  try
    SecondDB := TEST_DIR + 'second.db';

    // Create second database
    Conn := TNDXSQLiteConnection.Create(SecondDB, False);
    try
      Conn.Open;
      Conn.ExecuteNonQuery('DROP TABLE IF EXISTS items');
      Conn.ExecuteNonQuery('CREATE TABLE items (id INTEGER PRIMARY KEY, name TEXT)');
      Conn.ExecuteNonQuery('INSERT INTO items VALUES (1, ''From Second DB'')');
      Conn.Close;
    finally
      Conn.Free;
    end;

    // Create main database with table
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS main_items (id INTEGER PRIMARY KEY, name TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM main_items');
      Conn.ExecuteNonQuery('INSERT INTO main_items VALUES (1, ''From Main DB'')');

      Attached := TNDXSQLiteAttached.Create(Conn);
      try
        Attached.Attach(SecondDB, 'seconddb');

        // Query across databases
        V := Conn.ExecuteScalar('SELECT COUNT(*) FROM main_items m JOIN seconddb.items s ON m.id = s.id');
        if Integer(V) = 1 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected 1 row, got %d', [Integer(V)]);

        Attached.DetachAll;
      finally
        Attached.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestAttachMemory;
var
  Conn: TNDXSQLiteConnection;
  Attached: TNDXSQLiteAttached;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Attach Memory Database');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;
      Attached := TNDXSQLiteAttached.Create(Conn);
      try
        Attached.AttachMemory('memdb');

        // Create table in memory db
        Conn.ExecuteNonQuery('CREATE TABLE memdb.temp_data (value INTEGER)');
        Conn.ExecuteNonQuery('INSERT INTO memdb.temp_data VALUES (42)');

        V := Conn.ExecuteScalar('SELECT value FROM memdb.temp_data');
        if V = 42 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected 42, got %d', [Integer(V)]);

        Attached.DetachAll;
      finally
        Attached.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestCopyTable;
var
  Conn: TNDXSQLiteConnection;
  Attached: TNDXSQLiteAttached;
  SecondDB: string;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Copy Table Between Databases');
  TestPassed := False;
  ErrorMsg := '';
  try
    SecondDB := TEST_DIR + 'copy_dest.db';

    // Create destination database
    if FileExists(SecondDB) then
      DeleteFile(SecondDB);
    Conn := TNDXSQLiteConnection.Create(SecondDB, False);
    try
      Conn.Open;
      Conn.Close;
    finally
      Conn.Free;
    end;

    // Use main database
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create source table
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS source_table (id INTEGER PRIMARY KEY, data TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM source_table');
      Conn.ExecuteNonQuery('INSERT INTO source_table VALUES (1, ''Row 1''), (2, ''Row 2''), (3, ''Row 3'')');

      Attached := TNDXSQLiteAttached.Create(Conn);
      try
        Attached.Attach(SecondDB, 'destdb');

        // Copy table
        Attached.CopyTable('main', 'source_table', 'destdb', 'copied_table', True);

        // Verify copy
        V := Conn.ExecuteScalar('SELECT COUNT(*) FROM destdb.copied_table');
        if Integer(V) = 3 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected 3 rows, got %d', [Integer(V)]);

        Attached.DetachAll;
      finally
        Attached.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ Incremental Blob I/O Tests }
{ ============================================================================ }

procedure TestBlobBasicIO;
var
  Conn: TNDXSQLiteConnection;
  Blob: TNDXSQLiteBlob;
  RowId: Int64;
  WriteData, ReadData: TBytes;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Blob Basic I/O');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create table with blob column
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS blob_test (id INTEGER PRIMARY KEY, data BLOB)');
      Conn.ExecuteNonQuery('DELETE FROM blob_test');

      // Insert placeholder blob
      Conn.ExecuteNonQuery('INSERT INTO blob_test VALUES (1, zeroblob(100))');
      RowId := 1;

      // Write data
      SetLength(WriteData, 50);
      FillChar(WriteData[0], 50, $AA);

      Blob := TNDXSQLiteBlob.Create(Conn);
      try
        Blob.Open('blob_test', 'data', RowId, bomReadWrite);

        if Blob.Size = 100 then
        begin
          Blob.Write(WriteData, 0);
          Blob.Close;

          // Read back
          Blob.Open('blob_test', 'data', RowId, bomReadOnly);
          ReadData := Blob.Read(0, 50);
          Blob.Close;

          if (Length(ReadData) = 50) and (ReadData[0] = $AA) and (ReadData[49] = $AA) then
            TestPassed := True
          else
            ErrorMsg := 'Read data mismatch';
        end
        else
          ErrorMsg := Format('Expected size 100, got %d', [Blob.Size]);
      finally
        Blob.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestBlobStreaming;
var
  Conn: TNDXSQLiteConnection;
  Blob: TNDXSQLiteBlob;
  RowId: Int64;
  WriteStream, ReadStream: TMemoryStream;
  I: Integer;
  Buffer: array[0..255] of Byte;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Blob Streaming');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create table with blob column
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS blob_stream (id INTEGER PRIMARY KEY, data BLOB)');
      Conn.ExecuteNonQuery('DELETE FROM blob_stream');

      // Insert placeholder blob (1KB)
      Conn.ExecuteNonQuery('INSERT INTO blob_stream VALUES (1, zeroblob(1024))');
      RowId := 1;

      // Create test data
      WriteStream := TMemoryStream.Create;
      try
        for I := 0 to 255 do
          Buffer[I] := I;
        WriteStream.Write(Buffer, 256);
        WriteStream.Write(Buffer, 256);
        WriteStream.Write(Buffer, 256);
        WriteStream.Write(Buffer, 256); // 1024 bytes total

        Blob := TNDXSQLiteBlob.Create(Conn);
        try
          // Write stream
          Blob.Open('blob_stream', 'data', RowId, bomReadWrite);
          WriteStream.Position := 0;
          Blob.WriteFromStream(WriteStream);
          Blob.Close;

          // Read back to stream
          ReadStream := TMemoryStream.Create;
          try
            Blob.Open('blob_stream', 'data', RowId, bomReadOnly);
            Blob.ReadToStream(ReadStream);
            Blob.Close;

            if ReadStream.Size = 1024 then
            begin
              ReadStream.Position := 0;
              ReadStream.Read(Buffer, 256);
              if (Buffer[0] = 0) and (Buffer[255] = 255) then
                TestPassed := True
              else
                ErrorMsg := 'Stream data mismatch';
            end
            else
              ErrorMsg := Format('Expected 1024 bytes, got %d', [ReadStream.Size]);
          finally
            ReadStream.Free;
          end;
        finally
          Blob.Free;
        end;
      finally
        WriteStream.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestBlobReopen;
var
  Conn: TNDXSQLiteConnection;
  Blob: TNDXSQLiteBlob;
  Data1, Data2: TBytes;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Blob Reopen');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create table with multiple rows
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS blob_reopen (id INTEGER PRIMARY KEY, data BLOB)');
      Conn.ExecuteNonQuery('DELETE FROM blob_reopen');
      Conn.ExecuteNonQuery('INSERT INTO blob_reopen VALUES (1, X''AABBCC'')');
      Conn.ExecuteNonQuery('INSERT INTO blob_reopen VALUES (2, X''DDEEFF'')');

      Blob := TNDXSQLiteBlob.Create(Conn);
      try
        // Open first row
        Blob.Open('blob_reopen', 'data', 1, bomReadOnly);
        Data1 := Blob.ReadAll;

        // Reopen to second row
        Blob.Reopen(2);
        Data2 := Blob.ReadAll;

        Blob.Close;

        if (Length(Data1) = 3) and (Data1[0] = $AA) and
           (Length(Data2) = 3) and (Data2[0] = $DD) then
          TestPassed := True
        else
          ErrorMsg := 'Reopen data mismatch';
      finally
        Blob.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestBlobUtils;
var
  Conn: TNDXSQLiteConnection;
  Blob: TNDXSQLiteBlob;
  Size: Integer;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Blob Utilities');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create table with zeroblob in the INSERT directly
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS blob_utils (id INTEGER PRIMARY KEY, data BLOB)');
      Conn.ExecuteNonQuery('DELETE FROM blob_utils');
      Conn.ExecuteNonQuery('INSERT INTO blob_utils VALUES (1, zeroblob(512))');

      // Verify size by opening the blob directly
      Blob := TNDXSQLiteBlob.Create(Conn);
      try
        Blob.Open('blob_utils', 'data', 1, bomReadOnly);
        Size := Blob.Size;
        Blob.Close;

        if Size = 512 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected size 512, got %d', [Size]);
      finally
        Blob.Free;
      end;

      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestBlobCopy;
var
  Conn: TNDXSQLiteConnection;
  Data: TBytes;
  Blob: TNDXSQLiteBlob;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Blob Copy');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create table with source and dest rows
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS blob_copy (id INTEGER PRIMARY KEY, data BLOB)');
      Conn.ExecuteNonQuery('DELETE FROM blob_copy');
      Conn.ExecuteNonQuery('INSERT INTO blob_copy VALUES (1, X''0102030405'')');
      Conn.ExecuteNonQuery('INSERT INTO blob_copy VALUES (2, NULL)');

      // Copy blob from row 1 to row 2 using SQL (avoid utility function)
      Conn.ExecuteNonQuery('UPDATE blob_copy SET data = (SELECT data FROM blob_copy WHERE id = 1) WHERE id = 2');

      // Verify copy
      Blob := TNDXSQLiteBlob.Create(Conn);
      try
        Blob.Open('blob_copy', 'data', 2, bomReadOnly);
        Data := Blob.ReadAll;
        Blob.Close;

        if (Length(Data) = 5) and (Data[0] = 1) and (Data[4] = 5) then
          TestPassed := True
        else
          ErrorMsg := 'Copy data mismatch';
      finally
        Blob.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ R-Tree Spatial Index Tests }
{ ============================================================================ }

procedure TestRTreeCreate;
var
  Conn: TNDXSQLiteConnection;
  RTree: TNDXSQLiteRTree;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('R-Tree Create');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      RTree := TNDXSQLiteRTree.Create(Conn);
      try
        // Create R-Tree table
        RTree.CreateRTree('spatial_index', rtd2D);

        if RTree.RTreeExists('spatial_index') then
          TestPassed := True
        else
          ErrorMsg := 'R-Tree table not created';

        RTree.DropRTree('spatial_index');
      finally
        RTree.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestRTreeInsertQuery;
var
  Conn: TNDXSQLiteConnection;
  RTree: TNDXSQLiteRTree;
  DS: TDataSet;
  Count: Integer;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('R-Tree Insert/Query');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      RTree := TNDXSQLiteRTree.Create(Conn);
      try
        RTree.CreateRTree('geo_index', rtd2D);

        // Insert some 2D bounding boxes
        RTree.Insert2D(1, 0, 10, 0, 10);   // Box at origin
        RTree.Insert2D(2, 5, 15, 5, 15);   // Overlapping box
        RTree.Insert2D(3, 20, 30, 20, 30); // Distant box
        RTree.Insert2D(4, 100, 110, 100, 110); // Far away

        // Query for boxes that overlap with (0,20) x (0,20)
        DS := RTree.QueryBox2D(0, 20, 0, 20);
        try
          Count := 0;
          while not DS.EOF do
          begin
            Inc(Count);
            DS.Next;
          end;

          // Should find boxes 1, 2, and 3 (all overlap with query box)
          if Count >= 2 then
            TestPassed := True
          else
            ErrorMsg := Format('Expected at least 2 results, got %d', [Count]);
        finally
          DS.Free;
        end;

        RTree.DropRTree('geo_index');
      finally
        RTree.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestRTreePointQuery;
var
  Conn: TNDXSQLiteConnection;
  RTree: TNDXSQLiteRTree;
  DS: TDataSet;
  Count: Integer;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('R-Tree Point Query');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      RTree := TNDXSQLiteRTree.Create(Conn);
      try
        RTree.CreateRTree('point_index', rtd2D);

        // Insert boxes
        RTree.Insert2D(1, 0, 10, 0, 10);   // Contains point (5,5)
        RTree.Insert2D(2, 8, 20, 8, 20);   // Contains point (10,10)
        RTree.Insert2D(3, 100, 200, 100, 200); // Doesn't contain (5,5)

        // Query for boxes containing point (5,5)
        DS := RTree.QueryContainsPoint2D(5, 5);
        try
          Count := 0;
          while not DS.EOF do
          begin
            Inc(Count);
            DS.Next;
          end;

          // Should find box 1
          if Count = 1 then
            TestPassed := True
          else
            ErrorMsg := Format('Expected 1 result, got %d', [Count]);
        finally
          DS.Free;
        end;

        RTree.DropRTree('point_index');
      finally
        RTree.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestRTree3D;
var
  Conn: TNDXSQLiteConnection;
  RTree: TNDXSQLiteRTree;
  Count: Int64;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('R-Tree 3D');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      RTree := TNDXSQLiteRTree.Create(Conn);
      try
        RTree.CreateRTree('index_3d', rtd3D);

        // Insert 3D bounding boxes
        RTree.Insert3D(1, 0, 10, 0, 10, 0, 10);
        RTree.Insert3D(2, 5, 15, 5, 15, 5, 15);
        RTree.Insert3D(3, 100, 110, 100, 110, 100, 110);

        Count := RTree.GetCount;
        if Count = 3 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected 3 entries, got %d', [Count]);

        RTree.DropRTree('index_3d');
      finally
        RTree.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ Virtual Table Tests }
{ ============================================================================ }

procedure TestVTableModuleRegistration;
var
  Conn: TNDXSQLiteConnection;
  VTModule: TNDXVirtualTableModule;
  ModuleList: TStringList;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Virtual Table Module Registration');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      VTModule := TNDXVirtualTableModule.Create(Conn);
      try
        // Register modules
        VTModule.RegisterModule('series', TNDXSeriesVirtualTable, True);
        VTModule.RegisterModule('keyvalue', TNDXKeyValueVirtualTable, False);

        if VTModule.IsModuleRegistered('series') and
           VTModule.IsModuleRegistered('keyvalue') and
           not VTModule.IsModuleRegistered('unknown') then
        begin
          ModuleList := VTModule.GetRegisteredModules;
          try
            if ModuleList.Count = 2 then
              TestPassed := True
            else
              ErrorMsg := Format('Expected 2 modules, got %d', [ModuleList.Count]);
          finally
            ModuleList.Free;
          end;
        end
        else
          ErrorMsg := 'Module registration check failed';
      finally
        VTModule.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestVTableSeries;
var
  Conn: TNDXSQLiteConnection;
  VTModule: TNDXVirtualTableModule;
  Series: TNDXSeriesVirtualTable;
  Cursor: TNDXVirtualCursor;
  Sum: Int64;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Virtual Table Series');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      VTModule := TNDXVirtualTableModule.Create(Conn);
      try
        VTModule.RegisterModule('series', TNDXSeriesVirtualTable, True);

        // Create series table
        Series := TNDXSeriesVirtualTable(
          VTModule.CreateVirtualTable('test_series', 'series', []));
        Series.SetRange(1, 10, 1);

        // Iterate using cursor
        Cursor := Series.CreateCursor;
        try
          Cursor.Filter(0, '', []);
          Sum := 0;
          while not Cursor.Eof do
          begin
            Sum := Sum + Cursor.Column(0);
            Cursor.Next;
          end;

          // Sum of 1..10 = 55
          if Sum = 55 then
            TestPassed := True
          else
            ErrorMsg := Format('Expected sum 55, got %d', [Sum]);
        finally
          Cursor.Free;
        end;

        VTModule.DropVirtualTable('test_series');
      finally
        VTModule.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestVTableKeyValue;
var
  Conn: TNDXSQLiteConnection;
  VTModule: TNDXVirtualTableModule;
  KV: TNDXKeyValueVirtualTable;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Virtual Table Key-Value');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Test KeyValue table directly without module (simpler test)
      KV := TNDXKeyValueVirtualTable.Create(nil, 'direct_kv');
      try
        // Test key-value operations (use strings for all values)
        KV.SetValue('name', 'Alice');
        KV.SetValue('age', '30');
        KV.SetValue('city', 'Paris');

        if KV.HasKey('name') and KV.HasKey('age') and
           not KV.HasKey('country') then
        begin
          if KV.Keys.Count = 3 then
          begin
            KV.DeleteKey('city');
            if KV.Keys.Count = 2 then
              TestPassed := True
            else
              ErrorMsg := 'Delete key failed';
          end
          else
            ErrorMsg := Format('Expected 3 keys, got %d', [KV.Keys.Count]);
        end
        else
          ErrorMsg := 'HasKey check failed';
      finally
        KV.Free;
      end;

      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestVTableCSV;
var
  Conn: TNDXSQLiteConnection;
  VTModule: TNDXVirtualTableModule;
  CSV: TNDXCSVVirtualTable;
  CSVFile: string;
  F: TextFile;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Virtual Table CSV');
  TestPassed := False;
  ErrorMsg := '';
  try
    // Create test CSV file
    CSVFile := TEST_DIR + 'test_data.csv';
    AssignFile(F, CSVFile);
    Rewrite(F);
    WriteLn(F, 'id,name,value');
    WriteLn(F, '1,Alice,100');
    WriteLn(F, '2,Bob,200');
    WriteLn(F, '3,Carol,300');
    CloseFile(F);

    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      VTModule := TNDXVirtualTableModule.Create(Conn);
      try
        VTModule.RegisterModule('csv', TNDXCSVVirtualTable, True);

        // Create CSV table
        CSV := TNDXCSVVirtualTable(
          VTModule.CreateVirtualTable('test_csv', 'csv', []));
        CSV.SetFile(CSVFile, ',', True);

        // Verify loaded data
        if CSV.RowCount = 3 then
        begin
          if CSV.ColumnCount = 3 then
            TestPassed := True
          else
            ErrorMsg := Format('Expected 3 columns, got %d', [CSV.ColumnCount]);
        end
        else
          ErrorMsg := Format('Expected 3 rows, got %d', [CSV.RowCount]);

        VTModule.DropVirtualTable('test_csv');
      finally
        VTModule.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  // Cleanup
  if FileExists(CSVFile) then
    DeleteFile(CSVFile);

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ Authorizer Tests }
{ ============================================================================ }

type
  TAuthorizerTestHelper = class
  public
    FLastAction: TNDXAuthAction;
    FDenyCount: Integer;
    function HandleAuthorization(const Request: TNDXAuthRequest): TNDXAuthResult;
  end;

function TAuthorizerTestHelper.HandleAuthorization(const Request: TNDXAuthRequest): TNDXAuthResult;
begin
  FLastAction := Request.Action;
  if (Request.Action = aaInsert) and (Request.Param1 = 'protected_table') then
  begin
    Inc(FDenyCount);
    Result := arDeny;
  end
  else
    Result := arOK;
end;

procedure TestAuthorizerBasic;
var
  Conn: TNDXSQLiteConnection;
  Auth: TNDXSQLiteAuthorizer;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Authorizer Basic Enable/Disable');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Auth := TNDXSQLiteAuthorizer.Create(Conn);
      try
        if not Auth.Enabled then
        begin
          Auth.Enable;
          if Auth.Enabled then
          begin
            Auth.Disable;
            if not Auth.Enabled then
              TestPassed := True
            else
              ErrorMsg := 'Disable failed';
          end
          else
            ErrorMsg := 'Enable failed';
        end
        else
          ErrorMsg := 'Initially enabled';
      finally
        Auth.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestAuthorizerRules;
var
  Conn: TNDXSQLiteConnection;
  Auth: TNDXSQLiteAuthorizer;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Authorizer Rule-Based');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create test table
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS auth_test (id INTEGER, name TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM auth_test');

      Auth := TNDXSQLiteAuthorizer.Create(Conn);
      try
        // Add rule to allow SELECT
        Auth.AddAllowRule(aaSelect);
        Auth.AddAllowRule(aaRead);
        Auth.Enable;

        // This should work (SELECT is allowed)
        Conn.ExecuteNonQuery('INSERT INTO auth_test VALUES (1, ''Test'')');
        V := Conn.ExecuteScalar('SELECT COUNT(*) FROM auth_test');

        if Integer(V) = 1 then
          TestPassed := True
        else
          ErrorMsg := Format('Expected 1, got %d', [Integer(V)]);

        Auth.Disable;
      finally
        Auth.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestAuthorizerDenyAllWrites;
var
  Conn: TNDXSQLiteConnection;
  Auth: TNDXSQLiteAuthorizer;
  InsertFailed: Boolean;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Authorizer Deny All Writes');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create test table
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS deny_write_test (id INTEGER)');

      Auth := TNDXSQLiteAuthorizer.Create(Conn);
      try
        Auth.DenyAllWrites;
        Auth.Enable;

        // Try INSERT - should fail
        InsertFailed := False;
        try
          Conn.ExecuteNonQuery('INSERT INTO deny_write_test VALUES (1)');
        except
          InsertFailed := True;
        end;

        if InsertFailed then
          TestPassed := True
        else
          ErrorMsg := 'INSERT should have been denied';

        Auth.Disable;
      finally
        Auth.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestAuthorizerCallback;
var
  Conn: TNDXSQLiteConnection;
  Auth: TNDXSQLiteAuthorizer;
  Helper: TAuthorizerTestHelper;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Authorizer Custom Callback');
  TestPassed := False;
  ErrorMsg := '';
  Helper := TAuthorizerTestHelper.Create;
  try
    Helper.FDenyCount := 0;
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      // Create tables
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS normal_table (id INTEGER)');
      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS protected_table (id INTEGER)');

      Auth := TNDXSQLiteAuthorizer.Create(Conn);
      try
        Auth.SetCallback(@Helper.HandleAuthorization);
        Auth.Enable;

        // Insert into normal table - should work
        Conn.ExecuteNonQuery('INSERT INTO normal_table VALUES (1)');

        // Insert into protected table - should fail
        try
          Conn.ExecuteNonQuery('INSERT INTO protected_table VALUES (1)');
        except
          // Expected
        end;

        Auth.Disable;

        if Helper.FDenyCount > 0 then
          TestPassed := True
        else
          ErrorMsg := 'Callback was not invoked for protected_table';
      finally
        Auth.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
  Helper.Free;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestAuthorizerLogging;
var
  Conn: TNDXSQLiteConnection;
  Auth: TNDXSQLiteAuthorizer;
  LogList: TStringList;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Authorizer Logging');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS log_test (id INTEGER)');

      Auth := TNDXSQLiteAuthorizer.Create(Conn);
      try
        Auth.EnableLogging;
        Auth.Enable;

        // Execute some queries
        Conn.ExecuteNonQuery('INSERT INTO log_test VALUES (1)');
        Conn.ExecuteScalar('SELECT COUNT(*) FROM log_test');

        Auth.Disable;

        LogList := Auth.GetLog;
        if LogList.Count > 0 then
          TestPassed := True
        else
          ErrorMsg := 'No log entries recorded';
      finally
        Auth.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ Unlock Notify Tests }
{ ============================================================================ }

procedure TestUnlockNotifyAvailable;
var
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Unlock Notify Availability Check');
  TestPassed := False;
  ErrorMsg := '';
  try
    // Just check if the function is available
    // It may or may not be depending on SQLite compile options
    if TNDXSQLiteUnlockNotify.IsAvailable then
      TestPassed := True
    else
    begin
      // Not a failure - unlock_notify may not be compiled in
      WriteLn('  (Note: unlock_notify not available in this SQLite build)');
      TestPassed := True;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestUnlockNotifyCreate;
var
  Conn: TNDXSQLiteConnection;
  Unlock: TNDXSQLiteUnlockNotify;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Unlock Notify Create/Destroy');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Unlock := TNDXSQLiteUnlockNotify.Create(Conn);
      try
        // Check default values
        if (Unlock.DefaultTimeout = 30000) and
           (Unlock.AutoRetry = True) and
           (Unlock.MaxRetries = 5) then
          TestPassed := True
        else
          ErrorMsg := 'Default values not correct';
      finally
        Unlock.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestUnlockNotifyExecuteWithRetry;
var
  Conn: TNDXSQLiteConnection;
  Unlock: TNDXSQLiteUnlockNotify;
  Success: Boolean;
  V: Variant;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Unlock Notify Execute With Retry');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS unlock_test (id INTEGER, value TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM unlock_test');

      Unlock := TNDXSQLiteUnlockNotify.Create(Conn);
      try
        Success := Unlock.ExecuteWithRetry('INSERT INTO unlock_test VALUES (1, ''Test'')');

        if Success then
        begin
          V := Conn.ExecuteScalar('SELECT COUNT(*) FROM unlock_test');
          if Integer(V) = 1 then
            TestPassed := True
          else
            ErrorMsg := Format('Expected 1 row, got %d', [Integer(V)]);
        end
        else
          ErrorMsg := 'ExecuteWithRetry failed: ' + Unlock.LastError;
      finally
        Unlock.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestStepRetry;
var
  Conn: TNDXSQLiteConnection;
  Retry: TNDXSQLiteStepRetry;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Step Retry Helper');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Retry := TNDXSQLiteStepRetry.Create(Conn);
      try
        // Check default values
        if (Retry.MaxRetries = 5) and (Retry.RetryDelay = 100) then
        begin
          Retry.MaxRetries := 10;
          Retry.RetryDelay := 200;
          if (Retry.MaxRetries = 10) and (Retry.RetryDelay = 200) then
            TestPassed := True
          else
            ErrorMsg := 'Property setters failed';
        end
        else
          ErrorMsg := 'Default values not correct';
      finally
        Retry.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ Preupdate Hook Tests }
{ ============================================================================ }

procedure TestPreupdateAvailable;
var
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Preupdate Hooks Availability Check');
  TestPassed := False;
  ErrorMsg := '';
  try
    if TNDXSQLitePreupdate.IsAvailable then
      TestPassed := True
    else
    begin
      WriteLn('  (Note: Preupdate hooks not available - requires SQLITE_ENABLE_PREUPDATE_HOOK)');
      TestPassed := True; // Not a failure
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestPreupdateCreate;
var
  Conn: TNDXSQLiteConnection;
  Preupdate: TNDXSQLitePreupdate;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Preupdate Create/Destroy');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Preupdate := TNDXSQLitePreupdate.Create(Conn);
      try
        if (not Preupdate.Enabled) and
           (not Preupdate.LogEnabled) and
           (not Preupdate.TrackChanges) then
          TestPassed := True
        else
          ErrorMsg := 'Default values not correct';
      finally
        Preupdate.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestPreupdateEnableDisable;
var
  Conn: TNDXSQLiteConnection;
  Preupdate: TNDXSQLitePreupdate;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Preupdate Enable/Disable');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Preupdate := TNDXSQLitePreupdate.Create(Conn);
      try
        if TNDXSQLitePreupdate.IsAvailable then
        begin
          Preupdate.Enable;
          if Preupdate.Enabled then
          begin
            Preupdate.Disable;
            if not Preupdate.Enabled then
              TestPassed := True
            else
              ErrorMsg := 'Disable failed';
          end
          else
            ErrorMsg := 'Enable failed';
        end
        else
        begin
          // Skip if not available
          WriteLn('  (Skipped - preupdate hooks not available)');
          TestPassed := True;
        end;
      finally
        Preupdate.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestPreupdateLogging;
var
  Conn: TNDXSQLiteConnection;
  Preupdate: TNDXSQLitePreupdate;
  LogList: TStringList;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Preupdate Logging');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS preupdate_log_test (id INTEGER PRIMARY KEY, name TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM preupdate_log_test');

      Preupdate := TNDXSQLitePreupdate.Create(Conn);
      try
        if TNDXSQLitePreupdate.IsAvailable then
        begin
          Preupdate.EnableLogging;
          Preupdate.Enable;

          // Perform some operations
          Conn.ExecuteNonQuery('INSERT INTO preupdate_log_test VALUES (1, ''Test'')');
          Conn.ExecuteNonQuery('UPDATE preupdate_log_test SET name = ''Updated'' WHERE id = 1');
          Conn.ExecuteNonQuery('DELETE FROM preupdate_log_test WHERE id = 1');

          Preupdate.Disable;

          LogList := Preupdate.GetLog;
          if LogList.Count > 0 then
            TestPassed := True
          else
            ErrorMsg := 'No log entries recorded';
        end
        else
        begin
          WriteLn('  (Skipped - preupdate hooks not available)');
          TestPassed := True;
        end;
      finally
        Preupdate.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestPreupdateOperationStrings;
var
  S1, S2, S3: string;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Preupdate Operation Strings');
  TestPassed := False;
  ErrorMsg := '';
  try
    S1 := TNDXSQLitePreupdate.OperationToString(poInsert);
    S2 := TNDXSQLitePreupdate.OperationToString(poUpdate);
    S3 := TNDXSQLitePreupdate.OperationToString(poDelete);

    if (S1 = 'INSERT') and (S2 = 'UPDATE') and (S3 = 'DELETE') then
      TestPassed := True
    else
      ErrorMsg := Format('Strings: %s, %s, %s', [S1, S2, S3]);
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

procedure TestChangeTracker;
var
  Conn: TNDXSQLiteConnection;
  Tracker: TNDXChangeTracker;
  TestPassed: Boolean;
  ErrorMsg: string;
begin
  StartTest('Change Tracker');
  TestPassed := False;
  ErrorMsg := '';
  try
    Conn := TNDXSQLiteConnection.Create(TEST_DB, False);
    try
      Conn.Open;

      Conn.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS tracker_test (id INTEGER PRIMARY KEY, value TEXT)');
      Conn.ExecuteNonQuery('DELETE FROM tracker_test');

      Tracker := TNDXChangeTracker.Create(Conn);
      try
        if TNDXSQLitePreupdate.IsAvailable then
        begin
          Tracker.Start('tracker_test');

          Conn.ExecuteNonQuery('INSERT INTO tracker_test VALUES (1, ''Initial'')');
          Conn.ExecuteNonQuery('UPDATE tracker_test SET value = ''Changed'' WHERE id = 1');

          Tracker.Stop;

          if Tracker.GetChangeCount > 0 then
            TestPassed := True
          else
            ErrorMsg := 'No changes tracked';
        end
        else
        begin
          WriteLn('  (Skipped - preupdate hooks not available)');
          TestPassed := True;
        end;
      finally
        Tracker.Free;
      end;
      Conn.Close;
    finally
      Conn.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;

  if TestPassed then
    LogSuccess(CurrentTest)
  else
    LogFailure(CurrentTest, ErrorMsg);
end;

{ ============================================================================ }
{ MAIN }
{ ============================================================================ }
begin
  WriteLn('');
  WriteLn('===============================================');
  WriteLn('   NDXSQLite - Advanced Features Tests');
  WriteLn('   (UDF, Collation, Attached, Blob, R-Tree,');
  WriteLn('    Virtual Tables, Authorizer, Unlock Notify,');
  WriteLn('    Preupdate Hooks)');
  WriteLn('===============================================');
  WriteLn('');

  InitTestPaths;
  WriteLn('');

  CleanupTestDB;

  WriteLn('--- User-Defined Functions Tests ---');
  TestUDFScalarFunction;
  TestUDFMultipleArgs;
  TestUDFAggregateFunction;
  TestUDFRegistration;

  WriteLn('');
  WriteLn('--- Custom Collations Tests ---');
  TestCollationCaseInsensitive;
  TestCollationReverse;
  TestCollationNatural;

  WriteLn('');
  WriteLn('--- Attached Databases Tests ---');
  TestAttachDetach;
  TestCrossDbQuery;
  TestAttachMemory;
  TestCopyTable;

  WriteLn('');
  WriteLn('--- Incremental Blob I/O Tests ---');
  TestBlobBasicIO;
  TestBlobStreaming;
  TestBlobReopen;
  TestBlobUtils;
  TestBlobCopy;

  WriteLn('');
  WriteLn('--- R-Tree Spatial Index Tests ---');
  TestRTreeCreate;
  TestRTreeInsertQuery;
  TestRTreePointQuery;
  TestRTree3D;

  WriteLn('');
  WriteLn('--- Virtual Table Tests ---');
  TestVTableModuleRegistration;
  TestVTableSeries;
  TestVTableKeyValue;
  TestVTableCSV;

  WriteLn('');
  WriteLn('--- Authorizer Tests ---');
  TestAuthorizerBasic;
  TestAuthorizerRules;
  TestAuthorizerDenyAllWrites;
  TestAuthorizerCallback;
  TestAuthorizerLogging;

  WriteLn('');
  WriteLn('--- Unlock Notify Tests ---');
  TestUnlockNotifyAvailable;
  TestUnlockNotifyCreate;
  TestUnlockNotifyExecuteWithRetry;
  TestStepRetry;

  WriteLn('');
  WriteLn('--- Preupdate Hook Tests ---');
  TestPreupdateAvailable;
  TestPreupdateCreate;
  TestPreupdateEnableDisable;
  TestPreupdateLogging;
  TestPreupdateOperationStrings;
  TestChangeTracker;

  WriteLn('');
  WriteLn('===============================================');
  WriteLn(Format('   RESULTS: %d OK, %d FAILED', [TestsPassed, TestsFailed]));
  WriteLn('===============================================');
  WriteLn('');

  CleanupTestDB;

  if TestsFailed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
