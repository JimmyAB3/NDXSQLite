{===============================================================================
  NDXSQLite Example 05 - Error Handling
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Catching SQLite errors
  - Handling constraint violations
  - Transaction error recovery
  - Error codes and messages

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ErrorHandling;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteexceptions;

var
  Connection: TNDXSQLiteConnection;
  DBPath: string;

begin
  WriteLn('=== NDXSQLite Example 05: Error Handling ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example05.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Connection := TNDXSQLiteConnection.Create(DBPath, True);
  try
    Connection.Open;

    // Setup test table
    Connection.ExecuteNonQuery(
      'CREATE TABLE users (' +
      '  id INTEGER PRIMARY KEY,' +
      '  email TEXT UNIQUE NOT NULL,' +
      '  age INTEGER CHECK(age >= 0)' +
      ')'
    );
    Connection.ExecuteNonQuery(
      'INSERT INTO users (id, email, age) VALUES (1, ''alice@example.com'', 30)');

    // 1. Query exception - Syntax error
    WriteLn('1. SQL Syntax Error:');
    try
      Connection.ExecuteQuery('SELCT * FROM users'); // Typo in SELECT
    except
      on E: Exception do
      begin
        WriteLn('   Exception: ', E.ClassName);
        WriteLn('   Message: ', E.Message);
      end;
    end;
    WriteLn;

    // 2. Constraint exception - UNIQUE violation
    WriteLn('2. UNIQUE Constraint Violation:');
    try
      Connection.ExecuteNonQuery(
        'INSERT INTO users (id, email, age) VALUES (2, ''alice@example.com'', 25)');
    except
      on E: Exception do
      begin
        WriteLn('   Exception: ', E.ClassName);
        WriteLn('   Message: ', E.Message);
        if Pos('UNIQUE', UpperCase(E.Message)) > 0 then
          WriteLn('   -> Detected as UNIQUE constraint violation');
      end;
    end;
    WriteLn;

    // 3. Constraint exception - CHECK violation
    WriteLn('3. CHECK Constraint Violation:');
    try
      Connection.ExecuteNonQuery(
        'INSERT INTO users (email, age) VALUES (''bob@example.com'', -5)');
    except
      on E: Exception do
      begin
        WriteLn('   Exception: ', E.ClassName);
        WriteLn('   Message: ', E.Message);
        if Pos('CHECK', UpperCase(E.Message)) > 0 then
          WriteLn('   -> Detected as CHECK constraint violation');
      end;
    end;
    WriteLn;

    // 4. Constraint exception - NOT NULL violation
    WriteLn('4. NOT NULL Constraint Violation:');
    try
      Connection.ExecuteNonQuery(
        'INSERT INTO users (age) VALUES (40)'); // email is NOT NULL
    except
      on E: Exception do
      begin
        WriteLn('   Exception: ', E.ClassName);
        WriteLn('   Message: ', E.Message);
        if Pos('NOT NULL', UpperCase(E.Message)) > 0 then
          WriteLn('   -> Detected as NOT NULL constraint violation');
      end;
    end;
    WriteLn;

    // 5. Table not found error
    WriteLn('5. Table Not Found Error:');
    try
      Connection.ExecuteNonQuery('SELECT * FROM nonexistent_table');
    except
      on E: Exception do
      begin
        WriteLn('   Exception: ', E.ClassName);
        WriteLn('   Message: ', E.Message);
        if Pos('no such table', LowerCase(E.Message)) > 0 then
          WriteLn('   -> Detected as table not found error');
      end;
    end;
    WriteLn;

    // 6. Transaction commit without begin - no error expected
    WriteLn('6. Transaction state check:');
    WriteLn('   IsTransactionActive before begin: ', Connection.IsTransactionActive);
    Connection.BeginTransaction;
    WriteLn('   IsTransactionActive after begin: ', Connection.IsTransactionActive);
    Connection.Commit;
    WriteLn('   IsTransactionActive after commit: ', Connection.IsTransactionActive);
    WriteLn;

    // 7. Graceful error recovery
    WriteLn('7. Graceful Error Recovery Pattern:');
    Connection.BeginTransaction;
    try
      Connection.ExecuteNonQuery(
        'INSERT INTO users (email, age) VALUES (''charlie@example.com'', 35)');
      // This will fail - duplicate email
      Connection.ExecuteNonQuery(
        'INSERT INTO users (email, age) VALUES (''charlie@example.com'', 36)');
      Connection.Commit;
      WriteLn('   Transaction committed.');
    except
      on E: Exception do
      begin
        Connection.Rollback;
        WriteLn('   Error occurred: ', E.Message);
        WriteLn('   Transaction rolled back - database state preserved.');
      end;
    end;

    // Verify rollback worked
    WriteLn('   Users after rollback: ', Connection.ExecuteScalar(
      'SELECT COUNT(*) FROM users'));
    WriteLn;

    // 8. Error detection patterns
    WriteLn('8. Error Detection Patterns:');
    WriteLn('   Check for UNIQUE: Pos(''UNIQUE'', Message) > 0');
    WriteLn('   Check for CHECK: Pos(''CHECK'', Message) > 0');
    WriteLn('   Check for NOT NULL: Pos(''NOT NULL'', Message) > 0');
    WriteLn('   Check for no table: Pos(''no such table'', Message) > 0');
    WriteLn('   Check for syntax: Pos(''syntax error'', Message) > 0');
    WriteLn;

    // 9. Best practices
    WriteLn('9. Error Handling Best Practices:');
    WriteLn('   - Always use try-except around database operations');
    WriteLn('   - Check IsTransactionActive before commit/rollback');
    WriteLn('   - Use transactions for multi-statement operations');
    WriteLn('   - Log errors with full message for debugging');
    WriteLn('   - Roll back on any error within a transaction');
    WriteLn;

    Connection.Close;

  finally
    Connection.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
