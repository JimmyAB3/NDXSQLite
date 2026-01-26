{===============================================================================
  NDXSQLite Example 70 - Preupdate Hooks
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Setting up preupdate callbacks for change tracking
  - Inspecting old/new values before INSERT, UPDATE, DELETE
  - Logging data modifications with timestamps
  - Building an audit trail system
  - Change tracker helper for JSON export
  - Trigger depth detection
  - Undo/Redo support pattern

  Note: Preupdate hooks require SQLite compiled with SQLITE_ENABLE_PREUPDATE_HOOK.
  If not available, the example still runs and shows the API structure.

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program PreupdateHooks;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Variants, DateUtils,
  ndxsqlite3api,
  ndxsqliteconnection, ndxsqliteconnectionintf,
  ndxsqlitepreupdate;

const
  DB_FILE = 'preupdate_demo.db';

type
  { Audit record structure }
  TAuditRecord = record
    Timestamp: TDateTime;
    Operation: string;
    TableName: string;
    RowId: Int64;
    OldValues: string;
    NewValues: string;
  end;

  { Custom preupdate handler for audit trail }
  TAuditTrailHandler = class
  private
    FAuditLog: array of TAuditRecord;
    FChangeCount: Integer;
  public
    InsertCount: Integer;
    UpdateCount: Integer;
    DeleteCount: Integer;
    procedure HandlePreupdate(Sender: TNDXSQLitePreupdate;
      const Info: TNDXPreupdateInfo);
    procedure Clear;
    procedure PrintSummary;
    procedure PrintAuditLog;
    property ChangeCount: Integer read FChangeCount;
  end;

{ Handles pre-update hook events and records changes to the audit log. }
procedure TAuditTrailHandler.HandlePreupdate(Sender: TNDXSQLitePreupdate;
  const Info: TNDXPreupdateInfo);
var
  I: Integer;
  OldVal, NewVal: Variant;
  OldValues, NewValues: string;
  AuditRec: TAuditRecord;
begin
  OldValues := '';
  NewValues := '';

  case Info.Operation of
    poInsert:
      begin
        Inc(InsertCount);
        for I := 0 to Info.ColumnCount - 1 do
        begin
          NewVal := Sender.GetNewValue(I);
          if I > 0 then NewValues := NewValues + ', ';
          NewValues := NewValues + VarToStr(NewVal);
        end;
      end;

    poUpdate:
      begin
        Inc(UpdateCount);
        for I := 0 to Info.ColumnCount - 1 do
        begin
          OldVal := Sender.GetOldValue(I);
          NewVal := Sender.GetNewValue(I);
          if I > 0 then
          begin
            OldValues := OldValues + ', ';
            NewValues := NewValues + ', ';
          end;
          OldValues := OldValues + VarToStr(OldVal);
          NewValues := NewValues + VarToStr(NewVal);
        end;
      end;

    poDelete:
      begin
        Inc(DeleteCount);
        for I := 0 to Info.ColumnCount - 1 do
        begin
          OldVal := Sender.GetOldValue(I);
          if I > 0 then OldValues := OldValues + ', ';
          OldValues := OldValues + VarToStr(OldVal);
        end;
      end;
  end;

  // Store in audit log
  AuditRec.Timestamp := Now;
  AuditRec.Operation := TNDXSQLitePreupdate.OperationToString(Info.Operation);
  AuditRec.TableName := Info.TableName;
  AuditRec.RowId := Info.NewRowId;
  if Info.Operation = poDelete then
    AuditRec.RowId := Info.OldRowId;
  AuditRec.OldValues := OldValues;
  AuditRec.NewValues := NewValues;

  SetLength(FAuditLog, Length(FAuditLog) + 1);
  FAuditLog[High(FAuditLog)] := AuditRec;
  Inc(FChangeCount);
end;

{ Resets all audit counters and clears the audit log. }
procedure TAuditTrailHandler.Clear;
begin
  InsertCount := 0;
  UpdateCount := 0;
  DeleteCount := 0;
  FChangeCount := 0;
  SetLength(FAuditLog, 0);
end;

{ Prints a summary of insert, update, and delete counts. }
procedure TAuditTrailHandler.PrintSummary;
begin
  WriteLn('   Inserts: ', InsertCount);
  WriteLn('   Updates: ', UpdateCount);
  WriteLn('   Deletes: ', DeleteCount);
  WriteLn('   Total changes: ', FChangeCount);
end;

{ Prints all entries in the audit log with timestamps and details. }
procedure TAuditTrailHandler.PrintAuditLog;
var
  I: Integer;
begin
  WriteLn('   Audit Log (', Length(FAuditLog), ' entries):');
  for I := 0 to High(FAuditLog) do
  begin
    WriteLn('   [', FormatDateTime('hh:nn:ss.zzz', FAuditLog[I].Timestamp), '] ',
            FAuditLog[I].Operation, ' on ', FAuditLog[I].TableName,
            ' (RowId: ', FAuditLog[I].RowId, ')');
    if FAuditLog[I].OldValues <> '' then
      WriteLn('      Old: ', FAuditLog[I].OldValues);
    if FAuditLog[I].NewValues <> '' then
      WriteLn('      New: ', FAuditLog[I].NewValues);
  end;
end;

var
  Conn: INDXSQLiteConnection;
  Preupdate: TNDXSQLitePreupdate;
  Tracker: TNDXChangeTracker;
  Handler: TAuditTrailHandler;
  LogList: TStringList;
  I: Integer;
  JSON: string;
  HooksAvailable: Boolean;

begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 70: Preupdate Hooks ===');
  WriteLn('');

  // Cleanup
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  // =====================================================================
  // 1. Check Availability
  // =====================================================================
  WriteLn('1. Checking Preupdate Hooks Availability:');
  HooksAvailable := TNDXSQLitePreupdate.IsAvailable;
  if HooksAvailable then
    WriteLn('   Preupdate hooks are AVAILABLE')
  else
  begin
    WriteLn('   Preupdate hooks are NOT available');
    WriteLn('   (SQLite was not compiled with SQLITE_ENABLE_PREUPDATE_HOOK)');
    WriteLn('   Note: The example will still demonstrate the API structure.');
  end;
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(DB_FILE, False);
  Conn.Open;

  // Create test tables
  Conn.ExecuteNonQuery('CREATE TABLE employees (id INTEGER PRIMARY KEY, name TEXT, salary REAL, department TEXT)');
  Conn.ExecuteNonQuery('CREATE TABLE departments (id INTEGER PRIMARY KEY, name TEXT, budget REAL)');

  // =====================================================================
  // 2. Basic Preupdate Enable/Disable
  // =====================================================================
  WriteLn('2. Basic Preupdate Enable/Disable:');
  Preupdate := TNDXSQLitePreupdate.Create(Conn);
  try
    WriteLn('   Initial state - Enabled: ', Preupdate.Enabled);
    Preupdate.Enable;
    WriteLn('   After Enable - Enabled: ', Preupdate.Enabled);
    Preupdate.Disable;
    WriteLn('   After Disable - Enabled: ', Preupdate.Enabled);
  finally
    Preupdate.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 3. Audit Trail with Custom Handler
  // =====================================================================
  WriteLn('3. Audit Trail with Custom Handler:');
  Handler := TAuditTrailHandler.Create;
  try
    Handler.Clear;

    Preupdate := TNDXSQLitePreupdate.Create(Conn);
    try
      Preupdate.SetCallback(@Handler.HandlePreupdate);
      Preupdate.Enable;

      // Perform operations - these will be tracked
      WriteLn('   Performing database operations...');

      WriteLn('   - INSERT employee Alice');
      Conn.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Alice'', 50000, ''Engineering'')');

      WriteLn('   - INSERT employee Bob');
      Conn.ExecuteNonQuery('INSERT INTO employees VALUES (2, ''Bob'', 60000, ''Sales'')');

      WriteLn('   - INSERT employee Carol');
      Conn.ExecuteNonQuery('INSERT INTO employees VALUES (3, ''Carol'', 55000, ''Engineering'')');

      WriteLn('   - UPDATE Alice salary');
      Conn.ExecuteNonQuery('UPDATE employees SET salary = 55000 WHERE id = 1');

      WriteLn('   - UPDATE all Engineering salaries');
      Conn.ExecuteNonQuery('UPDATE employees SET salary = salary * 1.1 WHERE department = ''Engineering''');

      WriteLn('   - DELETE Bob');
      Conn.ExecuteNonQuery('DELETE FROM employees WHERE id = 2');

      Preupdate.Disable;

      WriteLn('');
      WriteLn('   === Change Summary ===');
      Handler.PrintSummary;

      if HooksAvailable then
      begin
        WriteLn('');
        Handler.PrintAuditLog;
      end
      else
        WriteLn('   (Detailed log not available - hooks not compiled in)');
    finally
      Preupdate.Free;
    end;
  finally
    Handler.Free;
  end;
  WriteLn('');

  // Reset table for next demos
  Conn.ExecuteNonQuery('DELETE FROM employees');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Alice'', 50000, ''Engineering'')');
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (2, ''Bob'', 60000, ''Sales'')');

  // =====================================================================
  // 4. Automatic Logging Mode
  // =====================================================================
  WriteLn('4. Automatic Logging Mode:');
  Preupdate := TNDXSQLitePreupdate.Create(Conn);
  try
    Preupdate.EnableLogging;
    Preupdate.TrackChanges := True;
    Preupdate.Enable;

    // Perform operations
    Conn.ExecuteNonQuery('UPDATE employees SET salary = 52000 WHERE id = 1');
    Conn.ExecuteNonQuery('UPDATE employees SET department = ''Marketing'' WHERE id = 2');
    Conn.ExecuteNonQuery('INSERT INTO employees VALUES (3, ''David'', 45000, ''HR'')');

    Preupdate.Disable;

    LogList := Preupdate.GetLog;
    WriteLn('   Log entries captured: ', LogList.Count);
    for I := 0 to LogList.Count - 1 do
      WriteLn('   ', LogList[I]);

    if LogList.Count = 0 then
      WriteLn('   (No entries captured - hooks not available in this build)');
  finally
    Preupdate.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 5. Change Tracker with JSON Export
  // =====================================================================
  WriteLn('5. Change Tracker with JSON Export:');
  Tracker := TNDXChangeTracker.Create(Conn);
  try
    Tracker.Start('employees');  // Track only employees table

    Conn.ExecuteNonQuery('INSERT INTO employees VALUES (4, ''Eve'', 70000, ''Finance'')');
    Conn.ExecuteNonQuery('UPDATE employees SET salary = 72000 WHERE id = 4');
    Conn.ExecuteNonQuery('DELETE FROM employees WHERE id = 4');

    Tracker.Stop;

    WriteLn('   Changes tracked: ', Tracker.GetChangeCount);
    JSON := Tracker.GetChangesAsJSON;
    WriteLn('   JSON output:');
    if Length(JSON) > 2 then
      WriteLn('   ', JSON)
    else
      WriteLn('   [] (empty - hooks not available or no changes captured)');
  finally
    Tracker.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 6. Multi-Table Tracking
  // =====================================================================
  WriteLn('6. Multi-Table Tracking:');
  Handler := TAuditTrailHandler.Create;
  try
    Handler.Clear;

    Preupdate := TNDXSQLitePreupdate.Create(Conn);
    try
      Preupdate.SetCallback(@Handler.HandlePreupdate);
      Preupdate.Enable;

      // Operations on multiple tables
      Conn.ExecuteNonQuery('INSERT INTO departments VALUES (1, ''Engineering'', 500000)');
      Conn.ExecuteNonQuery('INSERT INTO departments VALUES (2, ''Sales'', 300000)');
      Conn.ExecuteNonQuery('UPDATE employees SET department = ''Sales'' WHERE name = ''Alice''');
      Conn.ExecuteNonQuery('UPDATE departments SET budget = 550000 WHERE id = 1');

      Preupdate.Disable;

      WriteLn('   Operations tracked across tables:');
      Handler.PrintSummary;
    finally
      Preupdate.Free;
    end;
  finally
    Handler.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 7. Trigger Depth Detection
  // =====================================================================
  WriteLn('7. Trigger Depth Detection:');
  WriteLn('   Creating trigger for cascade updates...');

  // Create a trigger that updates related data
  Conn.ExecuteNonQuery(
    'CREATE TRIGGER update_timestamp AFTER UPDATE ON employees ' +
    'BEGIN ' +
    '  UPDATE departments SET budget = budget WHERE id = 1; ' +
    'END');

  Handler := TAuditTrailHandler.Create;
  try
    Handler.Clear;

    Preupdate := TNDXSQLitePreupdate.Create(Conn);
    try
      Preupdate.SetCallback(@Handler.HandlePreupdate);
      Preupdate.Enable;

      WriteLn('   Updating employee (will fire trigger)...');
      Conn.ExecuteNonQuery('UPDATE employees SET salary = 53000 WHERE id = 1');

      Preupdate.Disable;

      WriteLn('   Changes detected:');
      Handler.PrintSummary;
      WriteLn('   (If hooks available: 1 direct update + 1 trigger-caused update)');
    finally
      Preupdate.Free;
    end;
  finally
    Handler.Free;
  end;

  // Cleanup trigger
  Conn.ExecuteNonQuery('DROP TRIGGER update_timestamp');
  WriteLn('');

  // =====================================================================
  // 8. Operation String Helpers
  // =====================================================================
  WriteLn('8. Operation String Helpers:');
  WriteLn('   OperationToString(poInsert) = "', TNDXSQLitePreupdate.OperationToString(poInsert), '"');
  WriteLn('   OperationToString(poUpdate) = "', TNDXSQLitePreupdate.OperationToString(poUpdate), '"');
  WriteLn('   OperationToString(poDelete) = "', TNDXSQLitePreupdate.OperationToString(poDelete), '"');
  WriteLn('');

  // =====================================================================
  // 9. Batch Operations Tracking
  // =====================================================================
  WriteLn('9. Batch Operations Tracking:');
  Handler := TAuditTrailHandler.Create;
  try
    Handler.Clear;

    Preupdate := TNDXSQLitePreupdate.Create(Conn);
    try
      Preupdate.SetCallback(@Handler.HandlePreupdate);
      Preupdate.Enable;

      // Batch insert
      Conn.BeginTransaction;
      try
        for I := 10 to 19 do
          Conn.ExecuteNonQuery('INSERT INTO employees VALUES (?, ?, ?, ?)',
            [I, 'Employee' + IntToStr(I), 40000 + I * 1000, 'Batch']);
        Conn.Commit;
      except
        Conn.Rollback;
        raise;
      end;

      // Batch update
      Conn.ExecuteNonQuery('UPDATE employees SET salary = salary + 1000 WHERE department = ''Batch''');

      // Batch delete
      Conn.ExecuteNonQuery('DELETE FROM employees WHERE department = ''Batch''');

      Preupdate.Disable;

      WriteLn('   Batch operations summary:');
      Handler.PrintSummary;
      WriteLn('   Expected: 10 inserts, 10 updates, 10 deletes = 30 total');
    finally
      Preupdate.Free;
    end;
  finally
    Handler.Free;
  end;
  WriteLn('');

  // =====================================================================
  // 10. Final State
  // =====================================================================
  WriteLn('10. Final Database State:');
  WriteLn('    Employees: ', Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM employees')));
  WriteLn('    Departments: ', Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM departments')));
  WriteLn('');

  Conn.Close;
  Conn := nil;

  // Cleanup
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
