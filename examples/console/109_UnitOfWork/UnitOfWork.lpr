{===============================================================================
  NDXSQLite Example 109 - Unit of Work
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Unit of Work pattern for transaction management
  - Change tracking across multiple entities
  - Savepoint support within unit of work
  - Commit and rollback semantics
  - Domain entity lifecycle management

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program UnitOfWork;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

// ============================================================
// Unit of Work Infrastructure
// ============================================================

type
  TChangeType = (ctInsert, ctUpdate, ctDelete);

const
  ChangeTypeNames: array[TChangeType] of string = ('INSERT', 'UPDATE', 'DELETE');

var
  UoWActive: Boolean = False;
  UoWSavepointCounter: Integer = 0;

{ Creates infrastructure tables for the pattern. }
procedure CreateInfrastructure;
begin
  // Change tracking table (records all operations within a UoW)
  Conn.ExecuteNonQuery(
    'CREATE TABLE change_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  uow_id INTEGER NOT NULL,' +
    '  entity_table TEXT NOT NULL,' +
    '  entity_id INTEGER,' +
    '  change_type TEXT NOT NULL,' +
    '  field_changes TEXT,' +
    '  tracked_at TEXT NOT NULL' +
    ')');

  // UoW sessions
  Conn.ExecuteNonQuery(
    'CREATE TABLE uow_sessions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  started_at TEXT NOT NULL,' +
    '  finished_at TEXT,' +
    '  status TEXT DEFAULT ''active'',' +  // active, committed, rolled_back
    '  change_count INTEGER DEFAULT 0' +
    ')');
end;

// --- UoW Operations ---

var
  CurrentUoWId: Integer = 0;

{ Begins a new unit of work transaction and returns its session ID. }
function UoWBegin(const Timestamp: string): Integer;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO uow_sessions (started_at, status) VALUES (''%s'', ''active'')',
    [Timestamp]));
  CurrentUoWId := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
  UoWActive := True;
  Result := CurrentUoWId;
end;

{ Counts tracked changes for the current session, updates the session record
  with committed status and change count, then commits the transaction. }
procedure UoWCommit(const Timestamp: string);
var
  Changes: Integer;
begin
  Changes := Integer(Conn.ExecuteScalar(Format(
    'SELECT COUNT(*) FROM change_log WHERE uow_id = %d', [CurrentUoWId])));
  Conn.ExecuteNonQuery(Format(
    'UPDATE uow_sessions SET status = ''committed'', finished_at = ''%s'', change_count = %d WHERE id = %d',
    [Timestamp, Changes, CurrentUoWId]));
  Conn.ExecuteNonQuery('COMMIT');
  UoWActive := False;
end;

{ Marks the current session as rolled back with a timestamp and issues a
  ROLLBACK to discard all changes made within the unit of work. }
procedure UoWRollback(const Timestamp: string);
begin
  Conn.ExecuteNonQuery(Format(
    'UPDATE uow_sessions SET status = ''rolled_back'', finished_at = ''%s'' WHERE id = %d',
    [Timestamp, CurrentUoWId]));
  Conn.ExecuteNonQuery('ROLLBACK');
  UoWActive := False;
end;

{ Creates a named savepoint within the current unit of work. }
function UoWSavepoint(const Name: string): string;
begin
  Inc(UoWSavepointCounter);
  Result := Name + '_' + IntToStr(UoWSavepointCounter);
  Conn.ExecuteNonQuery('SAVEPOINT ' + Result);
end;

{ Releases the named savepoint, making its changes permanent within the
  enclosing transaction. }
procedure UoWReleaseSavepoint(const Name: string);
begin
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT ' + Name);
end;

{ Rolls back all changes made since the named savepoint was created, reverting
  to the state at the time the savepoint was set. }
procedure UoWRollbackToSavepoint(const Name: string);
begin
  Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT ' + Name);
end;

{ Inserts a change_log entry recording the entity table, ID, change type, field
  details, and timestamp for the current unit of work session. }
procedure TrackChange(const EntityTable: string; EntityId: Integer;
  ChangeType: TChangeType; const FieldChanges, Timestamp: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO change_log (uow_id, entity_table, entity_id, change_type, field_changes, tracked_at) ' +
    'VALUES (%d, ''%s'', %d, ''%s'', ''%s'', ''%s'')',
    [CurrentUoWId, EntityTable, EntityId, ChangeTypeNames[ChangeType], FieldChanges, Timestamp]));
end;

// ============================================================
// Domain Schema
// ============================================================

{ Creates the departments and employees tables with their columns and foreign
  key relationship. }
procedure CreateDomainSchema;
begin
  Conn.ExecuteNonQuery(
    'CREATE TABLE departments (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT UNIQUE NOT NULL,' +
    '  budget REAL DEFAULT 0' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE employees (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  department_id INTEGER REFERENCES departments(id),' +
    '  salary REAL NOT NULL,' +
    '  hired_at TEXT' +
    ')');
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  // Departments
  Conn.ExecuteNonQuery('INSERT INTO departments (name, budget) VALUES (''Engineering'', 500000)');
  Conn.ExecuteNonQuery('INSERT INTO departments (name, budget) VALUES (''Marketing'', 200000)');
  Conn.ExecuteNonQuery('INSERT INTO departments (name, budget) VALUES (''Sales'', 300000)');

  // Employees
  Conn.ExecuteNonQuery('INSERT INTO employees (name, department_id, salary, hired_at) VALUES (''Alice Chen'', 1, 95000, ''2022-03-15'')');
  Conn.ExecuteNonQuery('INSERT INTO employees (name, department_id, salary, hired_at) VALUES (''Bob Kumar'', 1, 88000, ''2023-01-10'')');
  Conn.ExecuteNonQuery('INSERT INTO employees (name, department_id, salary, hired_at) VALUES (''Carol Davis'', 2, 75000, ''2022-08-20'')');
  Conn.ExecuteNonQuery('INSERT INTO employees (name, department_id, salary, hired_at) VALUES (''Dave Wilson'', 3, 82000, ''2023-06-01'')');
end;

// ============================================================
// Feature Demonstrations
// ============================================================

{ Displays all employees with their departments and salaries. }
procedure ShowEmployees(const Title: string);
begin
  WriteLn('   ' + Title);
  DS := Conn.ExecuteQuery(
    'SELECT e.id, e.name, d.name as dept, e.salary ' +
    'FROM employees e ' +
    'JOIN departments d ON d.id = e.department_id ' +
    'ORDER BY e.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d %-15s %-12s $%.0f',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('dept').AsString,
         DS.FieldByName('salary').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 1: Basic UoW - Track and Commit ===
{ Opens a unit of work, performs an insert, salary update, department transfer,
  and delete with change tracking, then commits all changes together. }
procedure DemoBasicUoW;
var
  UoWId, NewId: Integer;
begin
  WriteLn('=== 1. Basic Unit of Work: Track Changes & Commit ===');
  WriteLn;

  ShowEmployees('Before UoW:');

  UoWId := UoWBegin('2024-03-01 10:00:00');
  WriteLn(Format('   UoW #%d started', [UoWId]));

  // INSERT: new employee
  Conn.ExecuteNonQuery(
    'INSERT INTO employees (name, department_id, salary, hired_at) ' +
    'VALUES (''Eve Taylor'', 1, 92000, ''2024-03-01'')');
  NewId := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
  TrackChange('employees', NewId, ctInsert, 'name=Eve Taylor,dept=1,salary=92000', '2024-03-01 10:00:01');
  WriteLn(Format('   Tracked: INSERT employee #%d (Eve Taylor)', [NewId]));

  // UPDATE: raise salary
  Conn.ExecuteNonQuery('UPDATE employees SET salary = 98000 WHERE id = 1');
  TrackChange('employees', 1, ctUpdate, 'salary: 95000->98000', '2024-03-01 10:00:02');
  WriteLn('   Tracked: UPDATE employee #1 salary 95000->98000');

  // UPDATE: transfer department
  Conn.ExecuteNonQuery('UPDATE employees SET department_id = 2 WHERE id = 4');
  TrackChange('employees', 4, ctUpdate, 'department_id: 3->2', '2024-03-01 10:00:03');
  WriteLn('   Tracked: UPDATE employee #4 dept Sales->Marketing');

  // DELETE
  Conn.ExecuteNonQuery('DELETE FROM employees WHERE id = 3');
  TrackChange('employees', 3, ctDelete, 'name=Carol Davis', '2024-03-01 10:00:04');
  WriteLn('   Tracked: DELETE employee #3 (Carol Davis)');

  // Commit
  UoWCommit('2024-03-01 10:00:05');
  WriteLn;
  WriteLn(Format('   UoW #%d committed (4 changes)', [UoWId]));

  ShowEmployees('After UoW commit:');
end;

// === Feature 2: UoW Rollback ===
{ Demonstrates rolling back a unit of work on failure. }
procedure DemoRollback;
var
  UoWId: Integer;
  EmpCount: Integer;
begin
  WriteLn('=== 2. Unit of Work: Rollback on Failure ===');
  WriteLn;

  EmpCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM employees'));
  WriteLn(Format('   Employees before: %d', [EmpCount]));

  UoWId := UoWBegin('2024-03-01 11:00:00');
  WriteLn(Format('   UoW #%d started', [UoWId]));

  // Make some changes
  Conn.ExecuteNonQuery(
    'INSERT INTO employees (name, department_id, salary, hired_at) ' +
    'VALUES (''Frank Moore'', 2, 70000, ''2024-03-01'')');
  TrackChange('employees', 6, ctInsert, 'name=Frank Moore', '2024-03-01 11:00:01');
  WriteLn('   Tracked: INSERT Frank Moore');

  Conn.ExecuteNonQuery('UPDATE employees SET salary = 200000 WHERE id = 1');
  TrackChange('employees', 1, ctUpdate, 'salary: 98000->200000', '2024-03-01 11:00:02');
  WriteLn('   Tracked: UPDATE Alice salary to 200000 (invalid!)');

  // Simulate failure: salary exceeds budget
  WriteLn('   ERROR: Salary 200000 exceeds department budget!');
  WriteLn('   Rolling back all changes...');

  UoWRollback('2024-03-01 11:00:03');
  WriteLn(Format('   UoW #%d rolled back', [UoWId]));

  EmpCount := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM employees'));
  WriteLn(Format('   Employees after rollback: %d (unchanged)', [EmpCount]));

  DS := Conn.ExecuteQuery('SELECT salary FROM employees WHERE id = 1');
  try
    WriteLn(Format('   Alice salary after rollback: $%.0f (unchanged)', [DS.FieldByName('salary').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 3: Savepoints (Nested UoW) ===
{ Demonstrates nested scopes using savepoints within a unit of work. }
procedure DemoSavepoints;
var
  UoWId: Integer;
  SP1, SP2: string;
begin
  WriteLn('=== 3. Nested Scopes with Savepoints ===');
  WriteLn;

  UoWId := UoWBegin('2024-03-01 12:00:00');
  WriteLn(Format('   UoW #%d started', [UoWId]));

  // Scope 1: Department budget changes (will succeed)
  SP1 := UoWSavepoint('dept_changes');
  WriteLn(Format('   Savepoint "%s" created', [SP1]));

  Conn.ExecuteNonQuery('UPDATE departments SET budget = 550000 WHERE id = 1');
  TrackChange('departments', 1, ctUpdate, 'budget: 500000->550000', '2024-03-01 12:00:01');
  WriteLn('   Updated Engineering budget to 550000');

  Conn.ExecuteNonQuery('UPDATE departments SET budget = 250000 WHERE id = 2');
  TrackChange('departments', 2, ctUpdate, 'budget: 200000->250000', '2024-03-01 12:00:02');
  WriteLn('   Updated Marketing budget to 250000');

  UoWReleaseSavepoint(SP1);
  WriteLn(Format('   Savepoint "%s" released (changes kept)', [SP1]));

  // Scope 2: Risky salary changes (will be rolled back)
  WriteLn;
  SP2 := UoWSavepoint('salary_changes');
  WriteLn(Format('   Savepoint "%s" created', [SP2]));

  Conn.ExecuteNonQuery('UPDATE employees SET salary = 300000 WHERE id = 1');
  WriteLn('   Updated Alice salary to 300000');
  Conn.ExecuteNonQuery('UPDATE employees SET salary = 280000 WHERE id = 2');
  WriteLn('   Updated Bob salary to 280000');

  // Check: total salaries exceed budget
  WriteLn('   Checking budget constraint...');
  DS := Conn.ExecuteQuery(
    'SELECT d.budget, SUM(e.salary) as total_salaries ' +
    'FROM departments d JOIN employees e ON e.department_id = d.id ' +
    'WHERE d.id = 1 GROUP BY d.id');
  try
    WriteLn(Format('   Engineering: budget=$%.0f, salaries=$%.0f',
      [DS.FieldByName('budget').AsFloat, DS.FieldByName('total_salaries').AsFloat]));
    if DS.FieldByName('total_salaries').AsFloat > DS.FieldByName('budget').AsFloat then
    begin
      WriteLn('   CONSTRAINT VIOLATION: salaries exceed budget!');
      UoWRollbackToSavepoint(SP2);
      WriteLn(Format('   Rolled back to savepoint "%s"', [SP2]));
    end;
  finally
    DS.Free;
  end;

  // Commit the outer UoW (only dept budget changes persist)
  WriteLn;
  UoWCommit('2024-03-01 12:00:05');
  WriteLn(Format('   UoW #%d committed', [UoWId]));

  // Verify
  WriteLn;
  WriteLn('   After commit:');
  DS := Conn.ExecuteQuery('SELECT name, budget FROM departments ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s budget=$%.0f',
        [DS.FieldByName('name').AsString, DS.FieldByName('budget').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT name, salary FROM employees WHERE department_id = 1 ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s salary=$%.0f (unchanged)',
        [DS.FieldByName('name').AsString, DS.FieldByName('salary').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 4: Batch Commit with Validation ===
{ Batch-inserts 3 new employees in a unit of work, validates the total salary
  bill against a budget limit, and commits or rolls back accordingly. }
procedure DemoBatchCommit;
var
  UoWId, I: Integer;
  TotalSalary: Double;
begin
  WriteLn('=== 4. Batch Commit with Pre-Commit Validation ===');
  WriteLn;

  UoWId := UoWBegin('2024-03-01 13:00:00');
  WriteLn(Format('   UoW #%d started', [UoWId]));

  // Batch hire 3 employees
  for I := 1 to 3 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO employees (name, department_id, salary, hired_at) ' +
      'VALUES (''New Hire %d'', %d, %.0f, ''2024-03-01'')',
      [I, ((I - 1) mod 3) + 1, 65000.0 + I * 5000.0]));
    TrackChange('employees', 0, ctInsert,
      Format('name=New Hire %d, salary=%.0f', [I, 65000.0 + I * 5000.0]),
      '2024-03-01 13:00:0' + IntToStr(I));
    WriteLn(Format('   Batch INSERT: New Hire %d (salary=$%.0f)', [I, 65000.0 + I * 5000.0]));
  end;

  // Pre-commit validation
  WriteLn;
  WriteLn('   Pre-commit validation:');
  TotalSalary := Double(Conn.ExecuteScalar('SELECT SUM(salary) FROM employees'));
  WriteLn(Format('   Total salary bill: $%.0f', [TotalSalary]));

  if TotalSalary > 1000000 then
  begin
    WriteLn('   VALIDATION FAILED: Total salaries exceed $1M limit!');
    UoWRollback('2024-03-01 13:00:05');
    WriteLn(Format('   UoW #%d rolled back', [UoWId]));
  end
  else
  begin
    WriteLn('   Validation passed: within budget');
    UoWCommit('2024-03-01 13:00:05');
    WriteLn(Format('   UoW #%d committed (%d changes)',
      [UoWId, Integer(Conn.ExecuteScalar(Format(
        'SELECT COUNT(*) FROM change_log WHERE uow_id = %d', [UoWId])))]));
  end;

  WriteLn(Format('   Employees after: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM employees'))]));
  WriteLn;
end;

// === Feature 5: Change Log Review ===
{ Prints the full change_log table showing each tracked operation's UoW ID,
  entity, change type, entity ID, and field-level change details. }
procedure DemoChangeLog;
begin
  WriteLn('=== 5. Change Log (Audit Trail) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT cl.uow_id, cl.entity_table, cl.entity_id, cl.change_type, ' +
    '  cl.field_changes, cl.tracked_at ' +
    'FROM change_log cl ' +
    'ORDER BY cl.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [UoW %d] %-10s %-7s #%d  %s',
        [DS.FieldByName('uow_id').AsInteger,
         DS.FieldByName('entity_table').AsString,
         DS.FieldByName('change_type').AsString,
         DS.FieldByName('entity_id').AsInteger,
         DS.FieldByName('field_changes').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 6: UoW Session History ===
{ Lists all unit of work sessions showing their ID, status (committed or rolled
  back), start and finish timestamps, and total change count. }
procedure DemoSessionHistory;
begin
  WriteLn('=== 6. UoW Session History ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT id, started_at, finished_at, status, change_count ' +
    'FROM uow_sessions ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   UoW #%d [%-11s] %s -> %s  changes=%d',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('status').AsString,
         DS.FieldByName('started_at').AsString,
         DS.FieldByName('finished_at').AsString,
         DS.FieldByName('change_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 7: Idempotent Operations ===
{ Demonstrates idempotent operations using the upsert pattern. }
procedure DemoIdempotent;
var
  UoWId: Integer;
  Before, After: Integer;
begin
  WriteLn('=== 7. Idempotent Operations (Upsert Pattern) ===');
  WriteLn;

  UoWId := UoWBegin('2024-03-01 14:00:00');

  // Upsert: insert or update if exists
  Before := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM departments'));

  Conn.ExecuteNonQuery(
    'INSERT INTO departments (name, budget) VALUES (''Research'', 400000) ' +
    'ON CONFLICT(name) DO UPDATE SET budget = 400000');
  TrackChange('departments', 4, ctInsert, 'name=Research,budget=400000', '2024-03-01 14:00:01');
  WriteLn('   Upsert: Research department (new)');

  Conn.ExecuteNonQuery(
    'INSERT INTO departments (name, budget) VALUES (''Engineering'', 600000) ' +
    'ON CONFLICT(name) DO UPDATE SET budget = 600000');
  TrackChange('departments', 1, ctUpdate, 'budget: 550000->600000', '2024-03-01 14:00:02');
  WriteLn('   Upsert: Engineering department (update budget)');

  UoWCommit('2024-03-01 14:00:03');
  After := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM departments'));

  WriteLn(Format('   Departments: %d -> %d', [Before, After]));
  WriteLn(Format('   UoW #%d committed', [UoWId]));
  WriteLn;
end;

// === Feature 8: Statistics ===
{ Demonstrates unit of work session statistics and change tracking. }
procedure DemoStatistics;
begin
  WriteLn('=== 8. Unit of Work Statistics ===');
  WriteLn;

  WriteLn(Format('   Total UoW sessions:   %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM uow_sessions'))]));
  WriteLn(Format('   Committed:            %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM uow_sessions WHERE status = ''committed'''))]));
  WriteLn(Format('   Rolled back:          %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM uow_sessions WHERE status = ''rolled_back'''))]));
  WriteLn(Format('   Total tracked changes: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM change_log'))]));

  WriteLn;
  WriteLn('   Changes by type:');
  DS := Conn.ExecuteQuery(
    'SELECT change_type, COUNT(*) as cnt FROM change_log GROUP BY change_type ORDER BY change_type');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %d',
        [DS.FieldByName('change_type').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Changes by entity:');
  DS := Conn.ExecuteQuery(
    'SELECT entity_table, COUNT(*) as cnt FROM change_log GROUP BY entity_table ORDER BY entity_table');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d changes',
        [DS.FieldByName('entity_table').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Final state:');
  WriteLn(Format('   Departments: %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM departments'))]));
  WriteLn(Format('   Employees:   %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM employees'))]));
  WriteLn;
end;

// === Main Program ===
begin
  WriteLn('=== Example 109: Unit of Work ===');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateInfrastructure;
    CreateDomainSchema;
    SeedData;

    DemoBasicUoW;
    DemoRollback;
    DemoSavepoints;
    DemoBatchCommit;
    DemoChangeLog;
    DemoSessionHistory;
    DemoIdempotent;
    DemoStatistics;

    WriteLn('=== All unit of work features demonstrated successfully ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
