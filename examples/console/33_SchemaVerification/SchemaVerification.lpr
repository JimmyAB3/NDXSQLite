{===============================================================================
  NDXSQLite Example 33 - Schema Verification
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates TNDXSQLiteVerify:
  - Counting schema objects (tables, indexes, views, triggers)
  - Listing schema objects
  - Verifying schema against expectations
  - Comparing two databases
  - Verifying data integrity
  - Testing triggers and views

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SchemaVerification;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions, ndxsqliteverify;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath, DB2Path: string;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // Create tables
  Connection.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  avatar BLOB' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE posts (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  title TEXT,' +
    '  content TEXT' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE audit_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  action TEXT,' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Create indexes
  Connection.ExecuteNonQuery('CREATE INDEX idx_users_email ON users(email)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_posts_user ON posts(user_id)');

  // Create view
  Connection.ExecuteNonQuery(
    'CREATE VIEW user_posts AS ' +
    'SELECT u.name, p.title FROM users u JOIN posts p ON u.id = p.user_id');

  // Create trigger
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER trg_post_insert AFTER INSERT ON posts ' +
    'BEGIN INSERT INTO audit_log (action) VALUES (''post_created''); END');

  // Insert data
  Connection.ExecuteNonQuery('INSERT INTO users VALUES (1, ''Alice'', ''alice@test.com'', X''DEADBEEF'')');
  Connection.ExecuteNonQuery('INSERT INTO users VALUES (2, ''Bob'', ''bob@test.com'', NULL)');
  Connection.ExecuteNonQuery('INSERT INTO posts VALUES (1, 1, ''Hello World'', ''First post'')');
  Connection.ExecuteNonQuery('INSERT INTO posts VALUES (2, 1, ''Second Post'', ''More content'')');
end;

{ Counts and prints the number of tables, indexes, views, and triggers using both individual count methods and GetSchema. }
procedure DemoCountSchema;
var
  Verify: TNDXSQLiteVerify;
  Schema: TNDXSchemaCount;
begin
  WriteLn('1. Counting schema objects');
  WriteLn('   ------------------------');

  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    // Individual counts
    WriteLn('   Tables:   ', Verify.CountTables);
    WriteLn('   Indexes:  ', Verify.CountIndexes);
    WriteLn('   Views:    ', Verify.CountViews);
    WriteLn('   Triggers: ', Verify.CountTriggers);

    WriteLn('');

    // All at once
    Schema := Verify.GetSchema;
    WriteLn('   Using GetSchema:');
    WriteLn('     TableCount:   ', Schema.TableCount);
    WriteLn('     IndexCount:   ', Schema.IndexCount);
    WriteLn('     ViewCount:    ', Schema.ViewCount);
    WriteLn('     TriggerCount: ', Schema.TriggerCount);
  finally
    Verify.Free;
  end;

  WriteLn('');
end;

{ Retrieves and prints the names of all tables, indexes, views, and triggers in the database. }
procedure DemoListSchemaObjects;
var
  Verify: TNDXSQLiteVerify;
  List: TStringList;
  I: Integer;
begin
  WriteLn('2. Listing schema objects');
  WriteLn('   -----------------------');

  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    // List tables
    List := Verify.GetTableList;
    try
      WriteLn('   Tables:');
      for I := 0 to List.Count - 1 do
        WriteLn('     - ', List[I]);
    finally
      List.Free;
    end;

    // List indexes
    List := Verify.GetIndexList;
    try
      WriteLn('   Indexes:');
      for I := 0 to List.Count - 1 do
        WriteLn('     - ', List[I]);
    finally
      List.Free;
    end;

    // List views
    List := Verify.GetViewList;
    try
      WriteLn('   Views:');
      for I := 0 to List.Count - 1 do
        WriteLn('     - ', List[I]);
    finally
      List.Free;
    end;

    // List triggers
    List := Verify.GetTriggerList;
    try
      WriteLn('   Triggers:');
      for I := 0 to List.Count - 1 do
        WriteLn('     - ', List[I]);
    finally
      List.Free;
    end;
  finally
    Verify.Free;
  end;

  WriteLn('');
end;

{ Verifies the database schema against expected object counts (3 tables, 2 indexes, 1 view, 1 trigger) and reports success or errors. }
procedure DemoVerifySchema;
var
  Verify: TNDXSQLiteVerify;
  Expected: TNDXExpectedSchema;
  Result: TNDXVerifyResult;
  I: Integer;
begin
  WriteLn('3. Verifying schema against expectations');
  WriteLn('   --------------------------------------');

  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    // Define expected schema
    Expected := TNDXSQLiteVerify.CreateExpectedSchema(
      3,   // 3 tables
      2,   // 2 indexes
      1,   // 1 view
      1,   // 1 trigger
      True // Test triggers
    );
    Expected.TestViews := True;
    Expected.TestBlobs := False;

    Result := Verify.VerifySchema(Expected);
    try
      WriteLn('   Expected: 3 tables, 2 indexes, 1 view, 1 trigger');
      WriteLn('   Actual:   ', Result.Schema.TableCount, ' tables, ',
              Result.Schema.IndexCount, ' indexes, ',
              Result.Schema.ViewCount, ' views, ',
              Result.Schema.TriggerCount, ' triggers');
      WriteLn('');
      WriteLn('   Verification result:');
      WriteLn('     Success: ', Result.Success);
      WriteLn('     Views working: ', Result.ViewsWorking);
      WriteLn('     Triggers working: ', Result.TriggersWorking);

      if Result.Errors.Count > 0 then
      begin
        WriteLn('   Errors:');
        for I := 0 to Result.Errors.Count - 1 do
          WriteLn('     - ', Result.Errors[I]);
      end;
    finally
      Result.Errors.Free;
    end;
  finally
    Verify.Free;
  end;

  WriteLn('');
end;

{ Checks whether BLOB data exists in the users.avatar and posts.content columns using VerifyBlobsExist. }
procedure DemoVerifyBlobs;
var
  Verify: TNDXSQLiteVerify;
begin
  WriteLn('4. Verifying BLOBs exist');
  WriteLn('   ----------------------');

  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    WriteLn('   users.avatar has BLOBs: ',
            Verify.VerifyBlobsExist('users', 'avatar'));
    WriteLn('   posts.content has BLOBs: ',
            Verify.VerifyBlobsExist('posts', 'content'));
  finally
    Verify.Free;
  end;

  WriteLn('');
end;

{ Tests whether all views in the database are queryable by calling VerifyViewsWork. }
procedure DemoVerifyViews;
var
  Verify: TNDXSQLiteVerify;
begin
  WriteLn('5. Verifying views work');
  WriteLn('   ---------------------');

  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    if Verify.VerifyViewsWork then
      WriteLn('   All views are queryable: YES')
    else
      WriteLn('   All views are queryable: NO');
  finally
    Verify.Free;
  end;

  WriteLn('');
end;

{ Creates a second database with a missing index and trigger, then compares both schemas and data, reporting differences. }
procedure DemoCompareWithOther;
var
  Conn2: TNDXSQLiteConnection;
  Opts2: TNDXSQLiteConnectionOptions;
  Verify: TNDXSQLiteVerify;
  CompResult: TNDXCompareResult;
  I: Integer;
begin
  WriteLn('6. Comparing two databases');
  WriteLn('   ------------------------');

  // Create second database with slightly different schema
  Opts2 := TNDXSQLiteConnectionOptions.Create;
  try
    Opts2.DatabasePath := DB2Path;
    Conn2 := TNDXSQLiteConnection.Create(Opts2);
    try
      Conn2.Open;

      // Same tables but missing an index
      Conn2.ExecuteNonQuery(
        'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, email TEXT, avatar BLOB)');
      Conn2.ExecuteNonQuery(
        'CREATE TABLE posts (id INTEGER PRIMARY KEY, user_id INTEGER, title TEXT, content TEXT)');
      Conn2.ExecuteNonQuery(
        'CREATE TABLE audit_log (id INTEGER PRIMARY KEY, action TEXT, timestamp TEXT)');
      Conn2.ExecuteNonQuery('CREATE INDEX idx_users_email ON users(email)');
      // Missing idx_posts_user!
      Conn2.ExecuteNonQuery(
        'CREATE VIEW user_posts AS SELECT u.name, p.title FROM users u JOIN posts p ON u.id = p.user_id');
      // Missing trigger!

      // Insert same data
      Conn2.ExecuteNonQuery('INSERT INTO users VALUES (1, ''Alice'', ''alice@test.com'', X''DEADBEEF'')');
      Conn2.ExecuteNonQuery('INSERT INTO users VALUES (2, ''Bob'', ''bob@test.com'', NULL)');
      Conn2.ExecuteNonQuery('INSERT INTO posts VALUES (1, 1, ''Hello World'', ''First post'')');
      Conn2.ExecuteNonQuery('INSERT INTO posts VALUES (2, 1, ''Second Post'', ''More content'')');

      // Compare
      Verify := TNDXSQLiteVerify.Create(Connection);
      try
        CompResult := Verify.CompareWith(Conn2);
        try
          WriteLn('   Source: ', CompResult.SourceSchema.TableCount, ' tables, ',
                  CompResult.SourceSchema.IndexCount, ' indexes, ',
                  CompResult.SourceSchema.TriggerCount, ' triggers');
          WriteLn('   Target: ', CompResult.TargetSchema.TableCount, ' tables, ',
                  CompResult.TargetSchema.IndexCount, ' indexes, ',
                  CompResult.TargetSchema.TriggerCount, ' triggers');
          WriteLn('');
          WriteLn('   Schemas match: ', CompResult.SchemasMatch);
          WriteLn('   Data matches: ', CompResult.DataMatches);

          if CompResult.Differences.Count > 0 then
          begin
            WriteLn('');
            WriteLn('   Differences:');
            for I := 0 to CompResult.Differences.Count - 1 do
              WriteLn('     - ', CompResult.Differences[I]);
          end;
        finally
          CompResult.Differences.Free;
        end;
      finally
        Verify.Free;
      end;

      Conn2.Close;
    finally
      Conn2.Free;
    end;
  finally
    Opts2.Free;
  end;

  WriteLn('');
end;

{ Compares the users table data between two databases before and after modifying a row to show match/mismatch detection. }
procedure DemoCompareTableData;
var
  Conn2: TNDXSQLiteConnection;
  Opts2: TNDXSQLiteConnectionOptions;
  Verify: TNDXSQLiteVerify;
begin
  WriteLn('7. Comparing specific table data');
  WriteLn('   ------------------------------');

  // Create identical database
  if FileExists(DB2Path) then DeleteFile(DB2Path);

  Opts2 := TNDXSQLiteConnectionOptions.Create;
  try
    Opts2.DatabasePath := DB2Path;
    Conn2 := TNDXSQLiteConnection.Create(Opts2);
    try
      Conn2.Open;

      Conn2.ExecuteNonQuery(
        'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, email TEXT, avatar BLOB)');

      // Same data as source
      Conn2.ExecuteNonQuery('INSERT INTO users VALUES (1, ''Alice'', ''alice@test.com'', X''DEADBEEF'')');
      Conn2.ExecuteNonQuery('INSERT INTO users VALUES (2, ''Bob'', ''bob@test.com'', NULL)');

      Verify := TNDXSQLiteVerify.Create(Connection);
      try
        WriteLn('   users table matches: ', Verify.CompareTableData(Conn2, 'users'));
      finally
        Verify.Free;
      end;

      // Modify data
      Conn2.ExecuteNonQuery('UPDATE users SET name = ''Alice Modified'' WHERE id = 1');

      Verify := TNDXSQLiteVerify.Create(Connection);
      try
        WriteLn('   After modification: ', Verify.CompareTableData(Conn2, 'users'));
      finally
        Verify.Free;
      end;

      Conn2.Close;
    finally
      Conn2.Free;
    end;
  finally
    Opts2.Free;
  end;

  WriteLn('');
end;

{ Tests each trigger by exercising its associated table operation and reports whether each trigger fired correctly. }
procedure DemoVerifyTriggers;
var
  Verify: TNDXSQLiteVerify;
  Result: TNDXVerifyResult;
  I: Integer;
begin
  WriteLn('8. Verifying triggers work');
  WriteLn('   ------------------------');

  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    Result := Verify.VerifyTriggersWork;
    try
      WriteLn('   Triggers tested: ', Length(Result.TriggerTests));
      for I := 0 to High(Result.TriggerTests) do
      begin
        WriteLn('     ', Result.TriggerTests[I].TriggerName, ' on ',
                Result.TriggerTests[I].TableName, ' (', Result.TriggerTests[I].TriggerType, '):');
        if Result.TriggerTests[I].Tested then
        begin
          if Result.TriggerTests[I].Working then
            WriteLn('       Status: WORKING')
          else
            WriteLn('       Status: FAILED - ', Result.TriggerTests[I].ErrorMessage);
        end
        else
          WriteLn('       Status: NOT TESTED');
      end;
    finally
      Result.Errors.Free;
    end;
  finally
    Verify.Free;
  end;

  WriteLn('');
end;

{ Deletes the primary and comparison database files from disk if they exist. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DB2Path) then DeleteFile(DB2Path);
end;

begin
  WriteLn('=== NDXSQLite Example 33: Schema Verification ===');
  WriteLn('    Using TNDXSQLiteVerify');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example33.db';
  DB2Path := ExtractFilePath(ParamStr(0)) + 'example33_compare.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupDatabase;

      DemoCountSchema;
      DemoListSchemaObjects;
      DemoVerifySchema;
      DemoVerifyBlobs;
      DemoVerifyViews;
      DemoCompareWithOther;
      DemoCompareTableData;
      DemoVerifyTriggers;

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
