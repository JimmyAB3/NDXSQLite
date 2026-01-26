{===============================================================================
  NDXSQLite Example 68 - Custom Authorizers
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Basic authorizer enable/disable
  - Rule-based authorization (allow/deny patterns)
  - Custom callback authorization
  - Logging authorization decisions
  - Convenience methods (DenyAllWrites, AllowReadOnly, etc.)

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CustomAuthorizers;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Variants, Math,
  ndxsqlite3api,
  ndxsqliteconnection, ndxsqliteconnectionintf,
  ndxsqliteauthorizer;

const
  DB_FILE = 'authorizer_demo.db';

type
  { Custom authorizer handler class }
  TMyAuthorizerHandler = class
  public
    function HandleAuth(const Request: TNDXAuthRequest): TNDXAuthResult;
  end;

{ Evaluates authorization requests and allows, denies, or ignores operations. }
function TMyAuthorizerHandler.HandleAuth(const Request: TNDXAuthRequest): TNDXAuthResult;
begin
  // Example: Deny all access to 'secret_table'
  if (Request.Param1 = 'secret_table') then
  begin
    WriteLn('      [DENIED] Access to secret_table blocked!');
    Result := arDeny;
  end
  // Example: Ignore (return NULL) for salary column reads
  else if (Request.Action = aaRead) and (Request.Param2 = 'salary') then
  begin
    WriteLn('      [IGNORED] Salary column hidden');
    Result := arIgnore;
  end
  else
    Result := arOK;
end;

var
  Conn: INDXSQLiteConnection;
  Auth: TNDXSQLiteAuthorizer;
  Handler: TMyAuthorizerHandler;
  LogList: TStringList;
  I: Integer;
  V: Variant;

begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 68: Custom Authorizers ===');
  WriteLn('');

  // Cleanup
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TNDXSQLiteConnection.Create(DB_FILE, False);
  Conn.Open;

  // Create test tables
  Conn.ExecuteNonQuery('CREATE TABLE employees (id INTEGER PRIMARY KEY, name TEXT, salary REAL)');
  Conn.ExecuteNonQuery('CREATE TABLE departments (id INTEGER PRIMARY KEY, name TEXT)');
  Conn.ExecuteNonQuery('CREATE TABLE secret_table (id INTEGER PRIMARY KEY, data TEXT)');

  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Alice'', 50000)');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (2, ''Bob'', 60000)');
  Conn.ExecuteNonQuery('INSERT INTO departments VALUES (1, ''Engineering'')');
  Conn.ExecuteNonQuery('INSERT INTO secret_table VALUES (1, ''Top Secret Data'')');

    // =====================================================================
    // 1. Basic Authorizer - Enable/Disable
    // =====================================================================
    WriteLn('1. Basic Authorizer Enable/Disable:');
    Auth := TNDXSQLiteAuthorizer.Create(Conn);
    try
      WriteLn('   Enabled before: ', Auth.Enabled);
      Auth.Enable;
      WriteLn('   Enabled after Enable(): ', Auth.Enabled);
      Auth.Disable;
      WriteLn('   Enabled after Disable(): ', Auth.Enabled);
    finally
      Auth.Free;
    end;
    WriteLn('');

    // =====================================================================
    // 2. Rule-Based Authorization - Deny All Writes
    // =====================================================================
    WriteLn('2. Rule-Based Authorization - Deny All Writes:');
    Auth := TNDXSQLiteAuthorizer.Create(Conn);
    try
      Auth.DenyAllWrites;
      WriteLn('   Rules added, enabling authorizer...');
      Auth.Enable;
      WriteLn('   Authorizer enabled');

      // INSERT should fail
      Write('   INSERT (denied): ');
      try
        Conn.ExecuteNonQuery('INSERT INTO employees VALUES (3, ''Carol'', 70000)');
        WriteLn('OK (unexpected!)');
      except
        on E: Exception do WriteLn('BLOCKED as expected - ', E.Message);
      end;

      // UPDATE should fail
      Write('   UPDATE (denied): ');
      try
        Conn.ExecuteNonQuery('UPDATE employees SET salary = 55000 WHERE id = 1');
        WriteLn('OK (unexpected!)');
      except
        on E: Exception do WriteLn('BLOCKED as expected');
      end;

      // SELECT should work
      Write('   SELECT (allowed): ');
      try
        V := Conn.ExecuteScalar('SELECT COUNT(*) FROM employees');
        WriteLn('OK - ', Integer(V), ' employees');
      except
        on E: Exception do WriteLn('FAILED: ', E.Message);
      end;

      Auth.Disable;
    finally
      Auth.Free;
    end;
    WriteLn('');

    // =====================================================================
    // 3. Rule-Based Authorization - Allow Read Only
    // =====================================================================
    WriteLn('3. Rule-Based Authorization - Allow Read Only:');
    Auth := TNDXSQLiteAuthorizer.Create(Conn);
    try
      Auth.AllowReadOnly; // Sets DenyByDefault = True and allows SELECT/READ

      Auth.Enable;

      Write('   SELECT (allowed): ');
      try
        V := Conn.ExecuteScalar('SELECT name FROM departments WHERE id = 1');
        WriteLn('OK - ', VarToStr(V));
      except
        on E: Exception do WriteLn('FAILED: ', E.Message);
      end;

      Write('   DROP TABLE (denied): ');
      try
        Conn.ExecuteNonQuery('DROP TABLE departments');
        WriteLn('OK (unexpected!)');
      except
        on E: Exception do WriteLn('BLOCKED as expected');
      end;

      Auth.Disable;
    finally
      Auth.Free;
    end;
    WriteLn('');

    // =====================================================================
    // 4. Custom Callback Authorization
    // =====================================================================
    WriteLn('4. Custom Callback Authorization:');
    Handler := TMyAuthorizerHandler.Create;
    try
      Auth := TNDXSQLiteAuthorizer.Create(Conn);
      try
        Auth.SetCallback(@Handler.HandleAuth);
        Auth.Enable;

        Write('   SELECT from employees: ');
        try
          V := Conn.ExecuteScalar('SELECT name FROM employees WHERE id = 1');
          WriteLn('OK - ', VarToStr(V));
        except
          on E: Exception do WriteLn('FAILED: ', E.Message);
        end;

        Write('   SELECT from secret_table: ');
        try
          V := Conn.ExecuteScalar('SELECT data FROM secret_table WHERE id = 1');
          WriteLn('OK - ', VarToStr(V), ' (unexpected!)');
        except
          on E: Exception do WriteLn('BLOCKED as expected');
        end;

        Auth.Disable;
      finally
        Auth.Free;
      end;
    finally
      Handler.Free;
    end;
    WriteLn('');

    // =====================================================================
    // 5. Authorization Logging
    // =====================================================================
    WriteLn('5. Authorization Logging:');
    Auth := TNDXSQLiteAuthorizer.Create(Conn);
    try
      Auth.EnableLogging;
      Auth.Enable;

      // Execute some queries
      Conn.ExecuteScalar('SELECT COUNT(*) FROM employees');
      Conn.ExecuteScalar('SELECT name FROM departments WHERE id = 1');

      Auth.Disable;

      WriteLn('   Log entries recorded: ', Auth.GetLog.Count);
      LogList := Auth.GetLog;
      for I := 0 to Min(4, LogList.Count - 1) do
        WriteLn('   ', LogList[I]);
      if LogList.Count > 5 then
        WriteLn('   ... (', LogList.Count - 5, ' more entries)');
    finally
      Auth.Free;
    end;
    WriteLn('');

    // =====================================================================
    // 6. Deny DDL Operations
    // =====================================================================
    WriteLn('6. Deny DDL Operations:');
    Auth := TNDXSQLiteAuthorizer.Create(Conn);
    try
      Auth.DenyDDL;
      Auth.Enable;

      Write('   CREATE TABLE (denied): ');
      try
        Conn.ExecuteNonQuery('CREATE TABLE new_table (id INTEGER)');
        WriteLn('OK (unexpected!)');
      except
        on E: Exception do WriteLn('BLOCKED as expected');
      end;

      Write('   SELECT (allowed): ');
      try
        V := Conn.ExecuteScalar('SELECT COUNT(*) FROM employees');
        WriteLn('OK - ', Integer(V), ' rows');
      except
        on E: Exception do WriteLn('FAILED: ', E.Message);
      end;

      Auth.Disable;
    finally
      Auth.Free;
    end;
    WriteLn('');

    // =====================================================================
    // 7. Deny Specific Table Access
    // =====================================================================
    WriteLn('7. Deny Specific Table Access:');
    Auth := TNDXSQLiteAuthorizer.Create(Conn);
    try
      Auth.DenyTableAccess('secret_table');
      Auth.Enable;

      Write('   SELECT from employees (allowed): ');
      try
        V := Conn.ExecuteScalar('SELECT COUNT(*) FROM employees');
        WriteLn('OK - ', Integer(V), ' rows');
      except
        on E: Exception do WriteLn('FAILED: ', E.Message);
      end;

      Write('   SELECT from secret_table (denied): ');
      try
        V := Conn.ExecuteScalar('SELECT COUNT(*) FROM secret_table');
        WriteLn('OK (unexpected!)');
      except
        on E: Exception do WriteLn('BLOCKED as expected');
      end;

      Auth.Disable;
    finally
      Auth.Free;
    end;
    WriteLn('');

    // =====================================================================
    // 8. Helper Functions
    // =====================================================================
    WriteLn('8. Helper Functions:');
    WriteLn('   ActionToString(aaSelect) = ', TNDXSQLiteAuthorizer.ActionToString(aaSelect));
    WriteLn('   ActionToString(aaInsert) = ', TNDXSQLiteAuthorizer.ActionToString(aaInsert));
    WriteLn('   ActionToString(aaUpdate) = ', TNDXSQLiteAuthorizer.ActionToString(aaUpdate));
    WriteLn('   ActionToString(aaDelete) = ', TNDXSQLiteAuthorizer.ActionToString(aaDelete));
    WriteLn('   ResultToString(arOK) = ', TNDXSQLiteAuthorizer.ResultToString(arOK));
    WriteLn('   ResultToString(arDeny) = ', TNDXSQLiteAuthorizer.ResultToString(arDeny));
    WriteLn('   ResultToString(arIgnore) = ', TNDXSQLiteAuthorizer.ResultToString(arIgnore));
    WriteLn('');

  Conn.Close;
  Conn := nil; // Release interface reference

  // Cleanup
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
