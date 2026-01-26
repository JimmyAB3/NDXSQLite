{===============================================================================
  NDXSQLite Example 28 - Foreign Keys
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Enabling foreign key enforcement
  - Creating tables with FK constraints
  - ON DELETE CASCADE/SET NULL/RESTRICT
  - ON UPDATE CASCADE
  - Checking FK violations
  - Deferrable foreign keys

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ForeignKeys;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Checks the current foreign_keys PRAGMA status, enables it, and verifies the
  new setting. }
procedure DemoEnableForeignKeys;
var
  DS: TDataSet;
begin
  WriteLn('1. Enabling foreign keys');
  WriteLn('   ----------------------');

  // Check current FK status
  DS := Connection.ExecuteQuery('PRAGMA foreign_keys');
  try
    WriteLn('   Before: foreign_keys = ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  // Enable foreign keys (MUST be done before any tables are created/used)
  Connection.ExecuteNonQuery('PRAGMA foreign_keys = ON');

  DS := Connection.ExecuteQuery('PRAGMA foreign_keys');
  try
    WriteLn('   After:  foreign_keys = ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('   NOTE: FK enforcement is per-connection, not stored in DB!');
  WriteLn('');
end;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  // Parent table
  Connection.ExecuteNonQuery(
    'CREATE TABLE departments (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL UNIQUE' +
    ')');

  // Child table with basic FK
  Connection.ExecuteNonQuery(
    'CREATE TABLE employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  dept_id INTEGER,' +
    '  FOREIGN KEY (dept_id) REFERENCES departments(id)' +
    ')');

  // Insert test data
  Connection.ExecuteNonQuery('INSERT INTO departments (id, name) VALUES (1, ''Engineering'')');
  Connection.ExecuteNonQuery('INSERT INTO departments (id, name) VALUES (2, ''Sales'')');
  Connection.ExecuteNonQuery('INSERT INTO departments (id, name) VALUES (3, ''HR'')');
end;

{ Tests basic FK enforcement: a valid insert, a rejected insert with a
  non-existent dept_id, and a permitted NULL FK value. }
procedure DemoBasicFK;
begin
  WriteLn('2. Basic foreign key constraint');
  WriteLn('   -----------------------------');

  // Valid insert
  Connection.ExecuteNonQuery(
    'INSERT INTO employees (name, dept_id) VALUES (''Alice'', 1)');
  WriteLn('   Inserted Alice in dept 1 (Engineering) - OK');

  // Try to insert with non-existent FK
  try
    Connection.ExecuteNonQuery(
      'INSERT INTO employees (name, dept_id) VALUES (''Bob'', 999)');
    WriteLn('   ERROR: Should have failed!');
  except
    on E: Exception do
      WriteLn('   Insert with dept_id=999 rejected: FK violation');
  end;

  // NULL FK is allowed (unless NOT NULL constraint)
  Connection.ExecuteNonQuery(
    'INSERT INTO employees (name, dept_id) VALUES (''Charlie'', NULL)');
  WriteLn('   Inserted Charlie with NULL dept_id - OK (no department)');

  WriteLn('');
end;

{ Tests ON DELETE RESTRICT: attempts to delete a referenced parent row (blocked)
  and deletes an unreferenced parent row (succeeds). }
procedure DemoOnDeleteRestrict;
begin
  WriteLn('3. ON DELETE RESTRICT (default)');
  WriteLn('   -----------------------------');

  // Try to delete department that has employees
  try
    Connection.ExecuteNonQuery('DELETE FROM departments WHERE id = 1');
    WriteLn('   ERROR: Should have failed!');
  except
    on E: Exception do
      WriteLn('   Cannot delete Engineering: has employees (RESTRICT)');
  end;

  // Can delete department with no employees
  Connection.ExecuteNonQuery('DELETE FROM departments WHERE id = 3');
  WriteLn('   Deleted HR (no employees) - OK');

  // Re-add for next demos
  Connection.ExecuteNonQuery('INSERT INTO departments (id, name) VALUES (3, ''HR'')');

  WriteLn('');
end;

{ Creates projects and tasks with ON DELETE CASCADE, deletes a project, and
  shows that all related tasks are automatically deleted. }
procedure DemoOnDeleteCascade;
var
  DS: TDataSet;
begin
  WriteLn('4. ON DELETE CASCADE');
  WriteLn('   ------------------');

  // Create table with CASCADE
  Connection.ExecuteNonQuery(
    'CREATE TABLE projects (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE tasks (' +
    '  id INTEGER PRIMARY KEY,' +
    '  project_id INTEGER,' +
    '  title TEXT,' +
    '  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE' +
    ')');

  // Insert data
  Connection.ExecuteNonQuery('INSERT INTO projects VALUES (1, ''Website Redesign'')');
  Connection.ExecuteNonQuery('INSERT INTO tasks VALUES (1, 1, ''Design mockups'')');
  Connection.ExecuteNonQuery('INSERT INTO tasks VALUES (2, 1, ''Implement frontend'')');
  Connection.ExecuteNonQuery('INSERT INTO tasks VALUES (3, 1, ''Testing'')');

  DS := Connection.ExecuteQuery('SELECT COUNT(*) as cnt FROM tasks');
  try
    WriteLn('   Tasks before delete: ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  // Delete project - tasks will be automatically deleted
  Connection.ExecuteNonQuery('DELETE FROM projects WHERE id = 1');
  WriteLn('   Deleted project "Website Redesign"');

  DS := Connection.ExecuteQuery('SELECT COUNT(*) as cnt FROM tasks');
  try
    WriteLn('   Tasks after delete: ', DS.FieldByName('cnt').AsInteger, ' (cascaded!)');
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates managers and teams with ON DELETE SET NULL, deletes a manager, and
  shows that child rows have their FK column set to NULL. }
procedure DemoOnDeleteSetNull;
var
  DS: TDataSet;
begin
  WriteLn('5. ON DELETE SET NULL');
  WriteLn('   -------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE managers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE teams (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  manager_id INTEGER,' +
    '  FOREIGN KEY (manager_id) REFERENCES managers(id) ON DELETE SET NULL' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO managers VALUES (1, ''John Boss'')');
  Connection.ExecuteNonQuery('INSERT INTO teams VALUES (1, ''Alpha Team'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO teams VALUES (2, ''Beta Team'', 1)');

  DS := Connection.ExecuteQuery('SELECT name, manager_id FROM teams ORDER BY id');
  try
    WriteLn('   Before deleting manager:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('name').AsString,
              ' - manager_id: ', DS.FieldByName('manager_id').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  Connection.ExecuteNonQuery('DELETE FROM managers WHERE id = 1');
  WriteLn('   Deleted manager "John Boss"');

  DS := Connection.ExecuteQuery('SELECT name, manager_id FROM teams ORDER BY id');
  try
    WriteLn('   After deleting manager:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('name').AsString,
              ' - manager_id: ', DS.FieldByName('manager_id').AsString, ' (set to NULL!)');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates categories and products with ON UPDATE CASCADE, updates a parent
  primary key, and shows that child FK values are automatically updated. }
procedure DemoOnUpdateCascade;
var
  DS: TDataSet;
begin
  WriteLn('6. ON UPDATE CASCADE');
  WriteLn('   ------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  code TEXT PRIMARY KEY,' +
    '  name TEXT' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  category_code TEXT,' +
    '  FOREIGN KEY (category_code) REFERENCES categories(code) ON UPDATE CASCADE' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO categories VALUES (''ELEC'', ''Electronics'')');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (1, ''Laptop'', ''ELEC'')');
  Connection.ExecuteNonQuery('INSERT INTO products VALUES (2, ''Phone'', ''ELEC'')');

  DS := Connection.ExecuteQuery('SELECT name, category_code FROM products ORDER BY id');
  try
    WriteLn('   Before update:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('name').AsString,
              ' - category: ', DS.FieldByName('category_code').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update primary key - children will follow
  Connection.ExecuteNonQuery('UPDATE categories SET code = ''TECH'' WHERE code = ''ELEC''');
  WriteLn('   Updated category code ELEC -> TECH');

  DS := Connection.ExecuteQuery('SELECT name, category_code FROM products ORDER BY id');
  try
    WriteLn('   After update:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('name').AsString,
              ' - category: ', DS.FieldByName('category_code').AsString, ' (cascaded!)');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Temporarily disables FK enforcement to insert an orphan row, re-enables FK,
  and runs PRAGMA foreign_key_check to detect the violation. }
procedure DemoCheckFKViolations;
var
  DS: TDataSet;
begin
  WriteLn('7. Check FK violations (PRAGMA foreign_key_check)');
  WriteLn('   -----------------------------------------------');

  // Temporarily disable FK to create violation
  Connection.ExecuteNonQuery('PRAGMA foreign_keys = OFF');

  Connection.ExecuteNonQuery(
    'CREATE TABLE authors (id INTEGER PRIMARY KEY, name TEXT)');
  Connection.ExecuteNonQuery(
    'CREATE TABLE books (id INTEGER PRIMARY KEY, title TEXT, ' +
    'author_id INTEGER REFERENCES authors(id))');

  // Insert book with non-existent author (FK disabled!)
  Connection.ExecuteNonQuery('INSERT INTO authors VALUES (1, ''Known Author'')');
  Connection.ExecuteNonQuery('INSERT INTO books VALUES (1, ''Good Book'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO books VALUES (2, ''Orphan Book'', 999)');

  WriteLn('   Created violation: book with author_id=999 (author doesn''t exist)');

  // Re-enable FK
  Connection.ExecuteNonQuery('PRAGMA foreign_keys = ON');

  // Check for violations
  DS := Connection.ExecuteQuery('PRAGMA foreign_key_check');
  try
    if DS.IsEmpty then
      WriteLn('   No FK violations found')
    else
    begin
      WriteLn('   FK violations found:');
      while not DS.EOF do
      begin
        WriteLn(Format('     Table: %s, RowID: %s, Parent: %s, FK Index: %s', [
          DS.Fields[0].AsString,
          DS.Fields[1].AsString,
          DS.Fields[2].AsString,
          DS.Fields[3].AsString
        ]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Lists the foreign key constraints on the books table using PRAGMA
  foreign_key_list, showing column mappings and action clauses. }
procedure DemoFKInfo;
var
  DS: TDataSet;
begin
  WriteLn('8. List foreign keys (PRAGMA foreign_key_list)');
  WriteLn('   --------------------------------------------');

  DS := Connection.ExecuteQuery('PRAGMA foreign_key_list(books)');
  try
    WriteLn('   Foreign keys on "books" table:');
    while not DS.EOF do
    begin
      WriteLn(Format('     FK #%d: %s(%s) -> %s(%s) ON UPDATE %s ON DELETE %s', [
        DS.FieldByName('id').AsInteger,
        'books',
        DS.FieldByName('from').AsString,
        DS.FieldByName('table').AsString,
        DS.FieldByName('to').AsString,
        DS.FieldByName('on_update').AsString,
        DS.FieldByName('on_delete').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates a self-referential org_chart table with a manager_id FK pointing to
  the same table, inserts a hierarchy, and displays it with a LEFT JOIN. }
procedure DemoSelfReferentialFK;
var
  DS: TDataSet;
begin
  WriteLn('9. Self-referential FK (hierarchical data)');
  WriteLn('   ----------------------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE org_chart (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  manager_id INTEGER,' +
    '  FOREIGN KEY (manager_id) REFERENCES org_chart(id)' +
    ')');

  // CEO has no manager
  Connection.ExecuteNonQuery('INSERT INTO org_chart VALUES (1, ''CEO'', NULL)');
  Connection.ExecuteNonQuery('INSERT INTO org_chart VALUES (2, ''VP Engineering'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO org_chart VALUES (3, ''VP Sales'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO org_chart VALUES (4, ''Senior Dev'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO org_chart VALUES (5, ''Junior Dev'', 4)');

  DS := Connection.ExecuteQuery(
    'SELECT e.name, COALESCE(m.name, ''(none)'') as manager ' +
    'FROM org_chart e ' +
    'LEFT JOIN org_chart m ON e.manager_id = m.id ' +
    'ORDER BY e.id');
  try
    WriteLn('   Org chart:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('name').AsString,
              ' reports to: ', DS.FieldByName('manager').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 28: Foreign Keys ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example28.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      DemoEnableForeignKeys;
      SetupTables;
      DemoBasicFK;
      DemoOnDeleteRestrict;
      DemoOnDeleteCascade;
      DemoOnDeleteSetNull;
      DemoOnUpdateCascade;
      DemoCheckFKViolations;
      DemoFKInfo;
      DemoSelfReferentialFK;

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
