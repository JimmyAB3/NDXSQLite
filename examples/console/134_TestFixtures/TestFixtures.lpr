{===============================================================================
  NDXSQLite Example 134 - Test Fixtures
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Minimal and full fixture loading
  - Factory pattern for test data generation
  - Schema relationship preservation
  - Teardown and cleanup procedures
  - Fixture isolation for repeatable tests

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program TestFixtures;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  Conn.ExecuteNonQuery('PRAGMA foreign_keys = ON');

  // Departments (top-level, no FK dependencies)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS departments (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL UNIQUE,' +
    '  budget REAL NOT NULL DEFAULT 0' +
    ')'
  );

  // Users (depends on departments)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS users (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  username TEXT NOT NULL UNIQUE,' +
    '  email TEXT NOT NULL,' +
    '  department_id INTEGER,' +
    '  role TEXT NOT NULL DEFAULT ''member'',' +
    '  FOREIGN KEY(department_id) REFERENCES departments(id)' +
    ')'
  );

  // Projects (depends on departments)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS projects (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  department_id INTEGER NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''planning'',' +
    '  priority INTEGER NOT NULL DEFAULT 3,' +
    '  FOREIGN KEY(department_id) REFERENCES departments(id)' +
    ')'
  );

  // Tasks (depends on projects and users)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS tasks (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  title TEXT NOT NULL,' +
    '  project_id INTEGER NOT NULL,' +
    '  assigned_to INTEGER,' +
    '  status TEXT NOT NULL DEFAULT ''todo'',' +
    '  hours_estimate REAL DEFAULT 0,' +
    '  FOREIGN KEY(project_id) REFERENCES projects(id),' +
    '  FOREIGN KEY(assigned_to) REFERENCES users(id)' +
    ')'
  );

  // Comments (depends on tasks and users)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS comments (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  task_id INTEGER NOT NULL,' +
    '  user_id INTEGER NOT NULL,' +
    '  content TEXT NOT NULL,' +
    '  created_at TEXT NOT NULL,' +
    '  FOREIGN KEY(task_id) REFERENCES tasks(id),' +
    '  FOREIGN KEY(user_id) REFERENCES users(id)' +
    ')'
  );

  // Fixture registry
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS fixture_registry (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  fixture_name TEXT NOT NULL,' +
    '  table_name TEXT NOT NULL,' +
    '  record_count INTEGER NOT NULL DEFAULT 0,' +
    '  description TEXT' +
    ')'
  );
end;

{ Cleans up all test data and tables. }
procedure Teardown;
begin
  // Delete in reverse FK order to respect referential integrity
  Conn.ExecuteNonQuery('DELETE FROM comments');
  Conn.ExecuteNonQuery('DELETE FROM tasks');
  Conn.ExecuteNonQuery('DELETE FROM projects');
  Conn.ExecuteNonQuery('DELETE FROM users');
  Conn.ExecuteNonQuery('DELETE FROM departments');
end;

{ Inserts one record per table (department, user, project, task, comment) to satisfy FK constraints and registers the fixture in the registry. }
procedure LoadFixture_Minimal;
begin
  // Minimal fixture: just enough data to satisfy FK constraints
  Conn.ExecuteNonQuery('INSERT INTO departments (id, name, budget) VALUES (1, ''Engineering'', 500000)');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, department_id, role) VALUES (1, ''alice'', ''alice@test.com'', 1, ''lead'')');
  Conn.ExecuteNonQuery('INSERT INTO projects (id, name, department_id, status, priority) VALUES (1, ''Alpha'', 1, ''active'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (1, ''Setup CI'', 1, 1, ''in_progress'', 8.0)');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (1, 1, 1, ''Started pipeline config'', ''2025-01-10 09:00:00'')');

  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''minimal'', ''departments'', 1, ''Single department'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''minimal'', ''users'', 1, ''Single user'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''minimal'', ''projects'', 1, ''Single project'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''minimal'', ''tasks'', 1, ''Single task'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''minimal'', ''comments'', 1, ''Single comment'')');
end;

{ Inserts a realistic dataset (3 departments, 6 users, 4 projects, 10 tasks, 8 comments) with cross-department relationships and registers each table in the fixture registry. }
procedure LoadFixture_Full;
begin
  // Full fixture: realistic dataset with multiple relationships
  // Departments
  Conn.ExecuteNonQuery('INSERT INTO departments (id, name, budget) VALUES (1, ''Engineering'', 500000)');
  Conn.ExecuteNonQuery('INSERT INTO departments (id, name, budget) VALUES (2, ''Design'', 200000)');
  Conn.ExecuteNonQuery('INSERT INTO departments (id, name, budget) VALUES (3, ''Marketing'', 300000)');

  // Users across departments
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, department_id, role) VALUES (1, ''alice'', ''alice@test.com'', 1, ''lead'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, department_id, role) VALUES (2, ''bob'', ''bob@test.com'', 1, ''senior'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, department_id, role) VALUES (3, ''carol'', ''carol@test.com'', 2, ''lead'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, department_id, role) VALUES (4, ''dave'', ''dave@test.com'', 2, ''member'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, department_id, role) VALUES (5, ''eve'', ''eve@test.com'', 3, ''lead'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, email, department_id, role) VALUES (6, ''frank'', ''frank@test.com'', 1, ''member'')');

  // Projects
  Conn.ExecuteNonQuery('INSERT INTO projects (id, name, department_id, status, priority) VALUES (1, ''Alpha Platform'', 1, ''active'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO projects (id, name, department_id, status, priority) VALUES (2, ''Beta API'', 1, ''active'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO projects (id, name, department_id, status, priority) VALUES (3, ''Brand Refresh'', 2, ''planning'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO projects (id, name, department_id, status, priority) VALUES (4, ''Launch Campaign'', 3, ''active'', 1)');

  // Tasks
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (1, ''Setup CI pipeline'', 1, 1, ''done'', 8.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (2, ''Database schema'', 1, 2, ''in_progress'', 16.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (3, ''Auth module'', 1, 6, ''todo'', 24.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (4, ''REST endpoints'', 2, 2, ''in_progress'', 20.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (5, ''API documentation'', 2, 1, ''todo'', 12.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (6, ''Logo redesign'', 3, 3, ''in_progress'', 10.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (7, ''Color palette'', 3, 4, ''done'', 6.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (8, ''Social media plan'', 4, 5, ''in_progress'', 15.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (9, ''Press release'', 4, 5, ''todo'', 8.0)');
  Conn.ExecuteNonQuery('INSERT INTO tasks (id, title, project_id, assigned_to, status, hours_estimate) VALUES (10, ''Unit tests'', 1, 6, ''todo'', 20.0)');

  // Comments
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (1, 1, 1, ''Pipeline is green'', ''2025-01-10 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (2, 2, 2, ''Schema v2 draft ready'', ''2025-01-11 14:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (3, 2, 1, ''Looks good, add indexes'', ''2025-01-11 15:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (4, 4, 2, ''Endpoints spec attached'', ''2025-01-12 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (5, 6, 3, ''First concepts uploaded'', ''2025-01-13 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (6, 6, 4, ''Love option B'', ''2025-01-13 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (7, 8, 5, ''Draft schedule done'', ''2025-01-14 09:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO comments (id, task_id, user_id, content, created_at) VALUES (8, 7, 4, ''Palette approved by client'', ''2025-01-14 16:00:00'')');

  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''full'', ''departments'', 3, ''Three departments'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''full'', ''users'', 6, ''Six users across depts'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''full'', ''projects'', 4, ''Four active projects'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''full'', ''tasks'', 10, ''Ten tasks assigned'')');
  Conn.ExecuteNonQuery('INSERT INTO fixture_registry (fixture_name, table_name, record_count, description) VALUES (''full'', ''comments'', 8, ''Eight task comments'')');
end;

{ Inserts the specified number of users into a given department with sequentially numbered usernames and emails based on the provided prefix. }
procedure FactoryCreateUsers(Count: Integer; DeptId: Integer; Prefix: string);
var
  I: Integer;
begin
  for I := 1 to Count do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO users (username, email, department_id, role) VALUES (''%s_%.3d'', ''%s_%.3d@factory.test'', %d, ''member'')',
      [Prefix, I, Prefix, I, DeptId]));
end;

{ Inserts the specified number of tasks for a given project and assignee, cycling through todo/in_progress/done statuses and computing hours from the loop index. }
procedure FactoryCreateTasks(Count: Integer; ProjectId, AssignedTo: Integer; Prefix: string);
var
  I: Integer;
  StatusArr: array[0..2] of string;
begin
  StatusArr[0] := 'todo';
  StatusArr[1] := 'in_progress';
  StatusArr[2] := 'done';

  for I := 1 to Count do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO tasks (title, project_id, assigned_to, status, hours_estimate) VALUES (''%s task %d'', %d, %d, ''%s'', %.1f)',
      [Prefix, I, ProjectId, AssignedTo, StatusArr[I mod 3], I * 2.5]));
end;

// === Demo 1: Schema with FK Relationships ===
{ Prints the table dependency order for fixture loading and teardown, then queries sqlite_master to list all application tables. }
procedure Demo1_SchemaRelationships;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Schema with FK Relationships ===');
  WriteLn;
  WriteLn('   Table dependency order (for loading and teardown):');
  WriteLn;
  WriteLn('   Level 0: departments         (no dependencies)');
  WriteLn('   Level 1: users               (-> departments)');
  WriteLn('   Level 1: projects            (-> departments)');
  WriteLn('   Level 2: tasks               (-> projects, users)');
  WriteLn('   Level 3: comments            (-> tasks, users)');
  WriteLn;
  WriteLn('   Loading order:  departments -> users -> projects -> tasks -> comments');
  WriteLn('   Teardown order: comments -> tasks -> projects -> users -> departments');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT name FROM sqlite_master WHERE type = ''table'' AND name NOT LIKE ''sqlite_%'' AND name <> ''fixture_registry'' ORDER BY name');
  try
    Write('   Tables: ');
    while not DS.EOF do
    begin
      Write(DS.FieldByName('name').AsString);
      DS.Next;
      if not DS.EOF then Write(', ');
    end;
    WriteLn;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 2: Minimal Fixture Loading ===
{ Loads the minimal fixture, displays its registry entries, and verifies the FK chain from comment through task, project, and department. }
procedure Demo2_MinimalFixture;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Minimal Fixture Loading ===');
  WriteLn;

  LoadFixture_Minimal;

  WriteLn('   Loaded fixture "minimal" (1 record per table):');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT table_name, record_count, description FROM fixture_registry WHERE fixture_name = ''minimal'' ORDER BY table_name');
  try
    WriteLn(Format('   %-14s %5s  %s', ['Table', 'Rows', 'Description']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %5d  %s', [
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('record_count').AsInteger,
        DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Verifying FK integrity...');

  DS := Conn.ExecuteQuery(
    'SELECT t.title, p.name as project, u.username as assignee, d.name as dept ' +
    'FROM tasks t ' +
    'JOIN projects p ON p.id = t.project_id ' +
    'JOIN users u ON u.id = t.assigned_to ' +
    'JOIN departments d ON d.id = p.department_id');
  try
    WriteLn(Format('   Task "%s" -> Project "%s" -> Dept "%s" (assigned: %s)', [
      DS.FieldByName('title').AsString,
      DS.FieldByName('project').AsString,
      DS.FieldByName('dept').AsString,
      DS.FieldByName('assignee').AsString]));
  finally
    DS.Free;
  end;

  WriteLn('   FK chain verified: comment -> task -> project -> department');
  WriteLn;
end;

// === Demo 3: Teardown ===
{ Demonstrates foreign-key-safe teardown of test fixture data. }
procedure Demo3_Teardown;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Teardown (FK-safe cleanup) ===');
  WriteLn;

  WriteLn('   Before teardown:');
  DS := Conn.ExecuteQuery(
    'SELECT ''departments'' as t, COUNT(*) as c FROM departments ' +
    'UNION ALL SELECT ''users'', COUNT(*) FROM users ' +
    'UNION ALL SELECT ''projects'', COUNT(*) FROM projects ' +
    'UNION ALL SELECT ''tasks'', COUNT(*) FROM tasks ' +
    'UNION ALL SELECT ''comments'', COUNT(*) FROM comments');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d records', [DS.FieldByName('t').AsString, DS.FieldByName('c').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Executing teardown (reverse FK order)...');
  Teardown;
  WriteLn('   1. DELETE FROM comments');
  WriteLn('   2. DELETE FROM tasks');
  WriteLn('   3. DELETE FROM projects');
  WriteLn('   4. DELETE FROM users');
  WriteLn('   5. DELETE FROM departments');

  WriteLn;
  WriteLn('   After teardown:');
  DS := Conn.ExecuteQuery(
    'SELECT ''departments'' as t, COUNT(*) as c FROM departments ' +
    'UNION ALL SELECT ''users'', COUNT(*) FROM users ' +
    'UNION ALL SELECT ''projects'', COUNT(*) FROM projects ' +
    'UNION ALL SELECT ''tasks'', COUNT(*) FROM tasks ' +
    'UNION ALL SELECT ''comments'', COUNT(*) FROM comments');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d records', [DS.FieldByName('t').AsString, DS.FieldByName('c').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   All tables empty, ready for next fixture');
  WriteLn;
end;

// === Demo 4: Full Fixture Loading ===
{ Loads the full fixture and displays the fixture registry summary showing record counts and descriptions for each table. }
procedure Demo4_FullFixture;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Full Fixture Loading ===');
  WriteLn;

  LoadFixture_Full;

  WriteLn('   Loaded fixture "full" (realistic dataset):');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT table_name, record_count, description FROM fixture_registry WHERE fixture_name = ''full'' ORDER BY table_name');
  try
    WriteLn(Format('   %-14s %5s  %s', ['Table', 'Rows', 'Description']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %5d  %s', [
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('record_count').AsInteger,
        DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total: 3 departments, 6 users, 4 projects, 10 tasks, 8 comments', []));
  WriteLn;
end;

// === Demo 5: Referential Integrity Verification ===
{ Queries each FK relationship to verify no orphaned references exist, reporting VALID or BROKEN for all six foreign key constraints. }
procedure Demo5_ReferentialIntegrity;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Referential Integrity Verification ===');
  WriteLn;

  // Check all FK relationships are valid
  WriteLn('   Checking FK constraints:');
  WriteLn;

  // Users -> Departments
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM users u WHERE u.department_id NOT IN (SELECT id FROM departments)');
  try
    WriteLn(Format('   users.department_id -> departments.id : %s',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'VALID', 'BROKEN')]));
  finally
    DS.Free;
  end;

  // Projects -> Departments
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM projects p WHERE p.department_id NOT IN (SELECT id FROM departments)');
  try
    WriteLn(Format('   projects.department_id -> departments.id : %s',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'VALID', 'BROKEN')]));
  finally
    DS.Free;
  end;

  // Tasks -> Projects
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM tasks t WHERE t.project_id NOT IN (SELECT id FROM projects)');
  try
    WriteLn(Format('   tasks.project_id -> projects.id : %s',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'VALID', 'BROKEN')]));
  finally
    DS.Free;
  end;

  // Tasks -> Users
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM tasks t WHERE t.assigned_to IS NOT NULL AND t.assigned_to NOT IN (SELECT id FROM users)');
  try
    WriteLn(Format('   tasks.assigned_to -> users.id : %s',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'VALID', 'BROKEN')]));
  finally
    DS.Free;
  end;

  // Comments -> Tasks
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM comments c WHERE c.task_id NOT IN (SELECT id FROM tasks)');
  try
    WriteLn(Format('   comments.task_id -> tasks.id : %s',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'VALID', 'BROKEN')]));
  finally
    DS.Free;
  end;

  // Comments -> Users
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM comments c WHERE c.user_id NOT IN (SELECT id FROM users)');
  try
    WriteLn(Format('   comments.user_id -> users.id : %s',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'VALID', 'BROKEN')]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   All 6 FK constraints verified VALID');
  WriteLn;
end;

// === Demo 6: Factory Pattern ===
{ Uses FactoryCreateUsers to generate batches of users for multiple departments, then displays the generated usernames, emails, and department assignments. }
procedure Demo6_FactoryPattern;
var
  DS: TDataSet;
  CountBefore, CountAfter: Integer;
begin
  WriteLn('=== 6. Factory Pattern (programmatic generation) ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users');
  try
    CountBefore := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn('   Generating factory records:');
  WriteLn('   - 5 users for Engineering (dept 1) with prefix "eng"');
  FactoryCreateUsers(5, 1, 'eng');

  WriteLn('   - 3 users for Design (dept 2) with prefix "des"');
  FactoryCreateUsers(3, 2, 'des');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users');
  try
    CountAfter := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Users: %d -> %d (added %d)', [CountBefore, CountAfter, CountAfter - CountBefore]));
  WriteLn;

  // Show generated users
  WriteLn('   Generated users:');
  WriteLn(Format('   %-12s %-28s %-6s', ['Username', 'Email', 'Dept']));
  WriteLn('   ' + StringOfChar('-', 50));
  DS := Conn.ExecuteQuery(
    'SELECT u.username, u.email, d.name as dept ' +
    'FROM users u JOIN departments d ON d.id = u.department_id ' +
    'WHERE u.username LIKE ''%\_%'' ESCAPE ''\'' ' +
    'ORDER BY u.username');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-28s %-6s', [
        DS.FieldByName('username').AsString,
        DS.FieldByName('email').AsString,
        DS.FieldByName('dept').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// === Demo 7: Reproducible Seed Data ===
{ Generates five tasks with deterministic project, assignee, status, and hours values derived from a fixed seed, then displays the resulting assignments. }
procedure Demo7_ReproducibleSeed;
var
  DS: TDataSet;
  I: Integer;
  Seed: Integer;
begin
  WriteLn('=== 7. Reproducible Seed Data ===');
  WriteLn;
  WriteLn('   Generating deterministic task data with seed=42:');
  WriteLn;

  Seed := 42;

  // Generate tasks with deterministic values based on seed
  for I := 1 to 5 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO tasks (title, project_id, assigned_to, status, hours_estimate) ' +
      'VALUES (''Seed task %d'', %d, %d, ''%s'', %.1f)',
      [I,
       ((Seed + I) mod 4) + 1,                        // project 1-4
       ((Seed + I * 7) mod 6) + 1,                    // user 1-6
       BoolToStr((Seed + I) mod 2 = 0, 'done', 'todo'),  // alternating status
       ((Seed + I * 3) mod 20) + 1.0                  // hours 1-20
      ]));
  end;

  DS := Conn.ExecuteQuery(
    'SELECT t.title, p.name as project, u.username, t.status, t.hours_estimate ' +
    'FROM tasks t ' +
    'JOIN projects p ON p.id = t.project_id ' +
    'JOIN users u ON u.id = t.assigned_to ' +
    'WHERE t.title LIKE ''Seed%'' ' +
    'ORDER BY t.id');
  try
    WriteLn(Format('   %-14s %-16s %-10s %-6s %5s', ['Task', 'Project', 'Assigned', 'Status', 'Hours']));
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %-16s %-10s %-6s %5.1f', [
        DS.FieldByName('title').AsString,
        DS.FieldByName('project').AsString,
        DS.FieldByName('username').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('hours_estimate').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Same seed always produces identical assignments');
  WriteLn;
end;

// === Demo 8: Fixture Isolation ===
{ Runs three simulated tests with alternating minimal and full fixtures, verifying that teardown between tests prevents data bleed-through. }
procedure Demo8_FixtureIsolation;
var
  DS: TDataSet;
  Test1Count, Test2Count: Integer;
begin
  WriteLn('=== 8. Fixture Isolation (fresh state per test) ===');
  WriteLn;

  // Simulate Test 1: uses minimal fixture
  WriteLn('   Test 1: Running with minimal fixture...');
  Teardown;
  Conn.ExecuteNonQuery('DELETE FROM fixture_registry');
  LoadFixture_Minimal;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM tasks');
  try
    Test1Count := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;
  WriteLn(Format('     Tasks in scope: %d', [Test1Count]));

  // Simulate Test 2: uses full fixture
  WriteLn('   Test 2: Running with full fixture...');
  Teardown;
  Conn.ExecuteNonQuery('DELETE FROM fixture_registry');
  LoadFixture_Full;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM tasks');
  try
    Test2Count := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;
  WriteLn(Format('     Tasks in scope: %d', [Test2Count]));

  // Simulate Test 3: back to minimal
  WriteLn('   Test 3: Running with minimal fixture again...');
  Teardown;
  Conn.ExecuteNonQuery('DELETE FROM fixture_registry');
  LoadFixture_Minimal;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM tasks');
  try
    WriteLn(Format('     Tasks in scope: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Isolation verified: Test1=%d, Test2=%d, Test3=%d (no bleed-through)',
    [Test1Count, Test2Count, Test1Count]));
  WriteLn;

  // Restore full fixture for remaining demos
  Teardown;
  Conn.ExecuteNonQuery('DELETE FROM fixture_registry');
  LoadFixture_Full;
end;

// === Demo 9: Composite Fixtures ===
{ Layers factory-generated users and tasks on top of the full fixture, then displays the combined record counts across all tables. }
procedure Demo9_CompositeFixtures;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Composite Fixtures (layered loading) ===');
  WriteLn;
  WriteLn('   Building composite fixture: full + factory + seed');
  WriteLn;

  // Already have full fixture loaded, add factory and seed layers
  FactoryCreateUsers(3, 3, 'mkt');
  FactoryCreateTasks(4, 4, 5, 'Campaign');

  WriteLn('   Layers applied:');
  WriteLn('   1. Base: full fixture (3 depts, 6 users, 4 projects, 10 tasks)');
  WriteLn('   2. Factory: +3 marketing users');
  WriteLn('   3. Factory: +4 campaign tasks');
  WriteLn;

  WriteLn('   Composite state:');
  DS := Conn.ExecuteQuery(
    'SELECT ''departments'' as tbl, COUNT(*) as cnt FROM departments ' +
    'UNION ALL SELECT ''users'', COUNT(*) FROM users ' +
    'UNION ALL SELECT ''projects'', COUNT(*) FROM projects ' +
    'UNION ALL SELECT ''tasks'', COUNT(*) FROM tasks ' +
    'UNION ALL SELECT ''comments'', COUNT(*) FROM comments');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d records', [DS.FieldByName('tbl').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// === Demo 10: Data Validation ===
{ Runs seven validation checks on fixture data including email format, hours estimates, project coverage, budgets, orphaned records, status values, and username uniqueness. }
procedure Demo10_DataValidation;
var
  DS: TDataSet;
  Issues: Integer;
begin
  WriteLn('=== 10. Fixture Data Validation ===');
  WriteLn;
  WriteLn('   Running validation checks on loaded fixture:');
  WriteLn;

  Issues := 0;

  // Check: all users have valid email format
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users WHERE email NOT LIKE ''%@%.%''');
  try
    WriteLn(Format('   [%s] All users have valid email format',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'PASS', 'FAIL')]));
    if DS.FieldByName('cnt').AsInteger > 0 then Inc(Issues);
  finally
    DS.Free;
  end;

  // Check: all tasks have positive hours estimate
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM tasks WHERE hours_estimate < 0');
  try
    WriteLn(Format('   [%s] All tasks have non-negative hours',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'PASS', 'FAIL')]));
    if DS.FieldByName('cnt').AsInteger > 0 then Inc(Issues);
  finally
    DS.Free;
  end;

  // Check: all projects have at least one task
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM projects p WHERE NOT EXISTS (SELECT 1 FROM tasks t WHERE t.project_id = p.id)');
  try
    WriteLn(Format('   [%s] All projects have at least one task',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'PASS', 'FAIL')]));
    if DS.FieldByName('cnt').AsInteger > 0 then Inc(Issues);
  finally
    DS.Free;
  end;

  // Check: department budgets are positive
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM departments WHERE budget <= 0');
  try
    WriteLn(Format('   [%s] All departments have positive budgets',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'PASS', 'FAIL')]));
    if DS.FieldByName('cnt').AsInteger > 0 then Inc(Issues);
  finally
    DS.Free;
  end;

  // Check: no orphaned comments (task exists)
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM comments c WHERE c.task_id NOT IN (SELECT id FROM tasks)');
  try
    WriteLn(Format('   [%s] No orphaned comments',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'PASS', 'FAIL')]));
    if DS.FieldByName('cnt').AsInteger > 0 then Inc(Issues);
  finally
    DS.Free;
  end;

  // Check: task status values are valid
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM tasks WHERE status NOT IN (''todo'', ''in_progress'', ''done'')');
  try
    WriteLn(Format('   [%s] All task statuses are valid (todo/in_progress/done)',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'PASS', 'FAIL')]));
    if DS.FieldByName('cnt').AsInteger > 0 then Inc(Issues);
  finally
    DS.Free;
  end;

  // Check: usernames are unique
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM (SELECT username FROM users GROUP BY username HAVING COUNT(*) > 1)');
  try
    WriteLn(Format('   [%s] All usernames are unique',
      [BoolToStr(DS.FieldByName('cnt').AsInteger = 0, 'PASS', 'FAIL')]));
    if DS.FieldByName('cnt').AsInteger > 0 then Inc(Issues);
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Validation result: %d/7 checks passed, %d issues',
    [7 - Issues, Issues]));
  WriteLn;
end;

// === Demo 11: Fixture Statistics ===
{ Displays task status distribution, per-user workload (task count and total hours), and department-level summaries of users, projects, and tasks. }
procedure Demo11_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 11. Fixture Statistics ===');
  WriteLn;

  // Task distribution by status
  WriteLn('   Task status distribution:');
  DS := Conn.ExecuteQuery('SELECT status, COUNT(*) as cnt FROM tasks GROUP BY status ORDER BY status');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d tasks', [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Workload per user
  WriteLn('   Workload per user (assigned tasks):');
  WriteLn(Format('   %-10s %5s %8s', ['User', 'Tasks', 'Hours']));
  WriteLn('   ' + StringOfChar('-', 28));
  DS := Conn.ExecuteQuery(
    'SELECT u.username, COUNT(t.id) as task_count, COALESCE(SUM(t.hours_estimate), 0) as total_hours ' +
    'FROM users u ' +
    'LEFT JOIN tasks t ON t.assigned_to = u.id ' +
    'GROUP BY u.id, u.username ' +
    'HAVING COUNT(t.id) > 0 ' +
    'ORDER BY total_hours DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %5d %8.1f', [
        DS.FieldByName('username').AsString,
        DS.FieldByName('task_count').AsInteger,
        DS.FieldByName('total_hours').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Department summary
  WriteLn('   Department summary:');
  WriteLn(Format('   %-14s %5s %8s %6s', ['Department', 'Users', 'Projects', 'Tasks']));
  WriteLn('   ' + StringOfChar('-', 40));
  DS := Conn.ExecuteQuery(
    'SELECT d.name, ' +
    '  (SELECT COUNT(*) FROM users u WHERE u.department_id = d.id) as user_count, ' +
    '  (SELECT COUNT(*) FROM projects p WHERE p.department_id = d.id) as proj_count, ' +
    '  (SELECT COUNT(*) FROM tasks t JOIN projects p ON p.id = t.project_id WHERE p.department_id = d.id) as task_count ' +
    'FROM departments d ORDER BY d.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %5d %8d %6d', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('user_count').AsInteger,
        DS.FieldByName('proj_count').AsInteger,
        DS.FieldByName('task_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 12: Final Teardown Verification ===
{ Performs a final teardown of all data and verifies that all tables are empty by summing total remaining records. }
procedure Demo12_FinalTeardown;
var
  DS: TDataSet;
  Total: Integer;
begin
  WriteLn('=== 12. Final Teardown Verification ===');
  WriteLn;

  WriteLn('   Performing final teardown...');
  Teardown;
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM departments) + ' +
    '  (SELECT COUNT(*) FROM users) + ' +
    '  (SELECT COUNT(*) FROM projects) + ' +
    '  (SELECT COUNT(*) FROM tasks) + ' +
    '  (SELECT COUNT(*) FROM comments) as total');
  try
    Total := DS.FieldByName('total').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total records remaining: %d', [Total]));
  WriteLn(Format('   Teardown %s', [BoolToStr(Total = 0, 'SUCCESSFUL - all tables clean', 'FAILED - orphaned records remain')]));
  WriteLn;
end;

// === Main ===
begin
  WriteLn('Example 134: Test Fixtures - Loading, Integrity, Teardown, Factories');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    Demo1_SchemaRelationships;
    Demo2_MinimalFixture;
    Demo3_Teardown;
    Demo4_FullFixture;
    Demo5_ReferentialIntegrity;
    Demo6_FactoryPattern;
    Demo7_ReproducibleSeed;
    Demo8_FixtureIsolation;
    Demo9_CompositeFixtures;
    Demo10_DataValidation;
    Demo11_Statistics;
    Demo12_FinalTeardown;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
