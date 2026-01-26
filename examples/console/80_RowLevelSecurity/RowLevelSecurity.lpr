{===============================================================================
  NDXSQLite Example 80 - Row-Level Security
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates row-level security (RLS) patterns:
  - Multi-tenant data isolation
  - User/role-based data filtering
  - Security policies simulation
  - Department-level access control
  - Data visibility rules
  - Secure views with built-in filters
  - Hierarchical access (manager sees team data)
  - Audit of access attempts

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program RowLevelSecurity;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;
  CurrentUserId: Integer;
  CurrentRole: string;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Row-Level Security Schema');
  WriteLn('   =====================================');

  // Users and roles
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  display_name TEXT,' +
    '  role TEXT NOT NULL CHECK(role IN (''admin'', ''manager'', ''employee'', ''viewer'')),' +
    '  department TEXT,' +
    '  manager_id INTEGER REFERENCES users(id),' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Security policies define what each role/user can access
  Conn.ExecuteNonQuery(
    'CREATE TABLE security_policies (' +
    '  id INTEGER PRIMARY KEY,' +
    '  policy_name TEXT NOT NULL,' +
    '  target_table TEXT NOT NULL,' +
    '  role TEXT,' +
    '  department TEXT,' +
    '  filter_column TEXT,' +
    '  filter_type TEXT CHECK(filter_type IN (''own'', ''department'', ''team'', ''all'')),' +
    '  can_read INTEGER DEFAULT 1,' +
    '  can_write INTEGER DEFAULT 0,' +
    '  can_delete INTEGER DEFAULT 0,' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Employee records (sensitive data)
  Conn.ExecuteNonQuery(
    'CREATE TABLE employee_records (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  department TEXT NOT NULL,' +
    '  salary REAL,' +
    '  performance_rating INTEGER CHECK(performance_rating BETWEEN 1 AND 5),' +
    '  notes TEXT,' +
    '  hire_date TEXT,' +
    '  last_review_date TEXT' +
    ')');

  // Projects (department-scoped)
  Conn.ExecuteNonQuery(
    'CREATE TABLE projects (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  department TEXT NOT NULL,' +
    '  budget REAL,' +
    '  status TEXT DEFAULT ''active'',' +
    '  owner_id INTEGER REFERENCES users(id),' +
    '  classification TEXT DEFAULT ''internal'' CHECK(classification IN (''public'', ''internal'', ''confidential'', ''restricted''))' +
    ')');

  // Documents (classified)
  Conn.ExecuteNonQuery(
    'CREATE TABLE documents (' +
    '  id INTEGER PRIMARY KEY,' +
    '  title TEXT NOT NULL,' +
    '  content TEXT,' +
    '  department TEXT,' +
    '  owner_id INTEGER NOT NULL REFERENCES users(id),' +
    '  classification TEXT DEFAULT ''internal'' CHECK(classification IN (''public'', ''internal'', ''confidential'', ''restricted'')),' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Multi-tenant customer data
  Conn.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  tenant_id INTEGER NOT NULL,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  phone TEXT,' +
    '  revenue REAL DEFAULT 0,' +
    '  assigned_to INTEGER REFERENCES users(id)' +
    ')');

  // Access audit log
  Conn.ExecuteNonQuery(
    'CREATE TABLE access_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL,' +
    '  target_table TEXT NOT NULL,' +
    '  action TEXT NOT NULL,' +
    '  row_count INTEGER,' +
    '  filter_applied TEXT,' +
    '  was_denied INTEGER DEFAULT 0,' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  WriteLn('   Created tables: users, security_policies, employee_records,');
  WriteLn('                   projects, documents, customers, access_log');
  WriteLn('');
end;

// =============================================================================
// Sample Data
// =============================================================================
{ Populates the database with users across roles/departments, employee salary records, classified projects and documents, multi-tenant customer data, and role-based security policies. }
procedure InsertSampleData;
begin
  WriteLn('2. Creating Sample Data');
  WriteLn('   ======================');

  // Users with different roles and departments
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, display_name, role, department) VALUES (1, ''admin'', ''System Admin'', ''admin'', ''IT'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, display_name, role, department) VALUES (2, ''jsmith'', ''John Smith'', ''manager'', ''Engineering'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, display_name, role, department, manager_id) VALUES (3, ''alice'', ''Alice Johnson'', ''employee'', ''Engineering'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, display_name, role, department, manager_id) VALUES (4, ''bob'', ''Bob Williams'', ''employee'', ''Engineering'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, display_name, role, department) VALUES (5, ''mgarcia'', ''Maria Garcia'', ''manager'', ''Sales'')');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, display_name, role, department, manager_id) VALUES (6, ''carol'', ''Carol Davis'', ''employee'', ''Sales'', 5)');
  Conn.ExecuteNonQuery('INSERT INTO users (id, username, display_name, role, department) VALUES (7, ''viewer1'', ''External Viewer'', ''viewer'', ''External'')');

  WriteLn('   Created 7 users (1 admin, 2 managers, 3 employees, 1 viewer)');

  // Employee records
  Conn.ExecuteNonQuery('INSERT INTO employee_records (user_id, department, salary, performance_rating, hire_date) VALUES (2, ''Engineering'', 120000, 4, ''2018-03-15'')');
  Conn.ExecuteNonQuery('INSERT INTO employee_records (user_id, department, salary, performance_rating, hire_date) VALUES (3, ''Engineering'', 95000, 5, ''2020-06-01'')');
  Conn.ExecuteNonQuery('INSERT INTO employee_records (user_id, department, salary, performance_rating, hire_date) VALUES (4, ''Engineering'', 88000, 3, ''2021-01-10'')');
  Conn.ExecuteNonQuery('INSERT INTO employee_records (user_id, department, salary, performance_rating, hire_date) VALUES (5, ''Sales'', 110000, 4, ''2019-09-20'')');
  Conn.ExecuteNonQuery('INSERT INTO employee_records (user_id, department, salary, performance_rating, hire_date) VALUES (6, ''Sales'', 75000, 4, ''2022-03-01'')');

  WriteLn('   Created 5 employee records');

  // Projects
  Conn.ExecuteNonQuery('INSERT INTO projects (name, department, budget, owner_id, classification) VALUES (''Project Alpha'', ''Engineering'', 500000, 2, ''confidential'')');
  Conn.ExecuteNonQuery('INSERT INTO projects (name, department, budget, owner_id, classification) VALUES (''Website Redesign'', ''Engineering'', 150000, 3, ''internal'')');
  Conn.ExecuteNonQuery('INSERT INTO projects (name, department, budget, owner_id, classification) VALUES (''Sales Campaign Q1'', ''Sales'', 80000, 5, ''internal'')');
  Conn.ExecuteNonQuery('INSERT INTO projects (name, department, budget, owner_id, classification) VALUES (''Company Handbook'', ''HR'', 20000, 1, ''public'')');
  Conn.ExecuteNonQuery('INSERT INTO projects (name, department, budget, owner_id, classification) VALUES (''Secret R&D'', ''Engineering'', 1000000, 2, ''restricted'')');

  WriteLn('   Created 5 projects (public, internal, confidential, restricted)');

  // Documents
  Conn.ExecuteNonQuery('INSERT INTO documents (title, content, department, owner_id, classification) VALUES (''Team Guidelines'', ''Engineering team practices...'', ''Engineering'', 2, ''internal'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (title, content, department, owner_id, classification) VALUES (''Salary Report 2024'', ''Compensation data...'', ''HR'', 1, ''restricted'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (title, content, department, owner_id, classification) VALUES (''Sales Playbook'', ''Sales strategies...'', ''Sales'', 5, ''confidential'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (title, content, department, owner_id, classification) VALUES (''Holiday Calendar'', ''Company holidays...'', NULL, 1, ''public'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (title, content, department, owner_id, classification) VALUES (''Client Contracts'', ''Legal documents...'', ''Sales'', 5, ''restricted'')');
  Conn.ExecuteNonQuery('INSERT INTO documents (title, content, department, owner_id, classification) VALUES (''Architecture Docs'', ''System design...'', ''Engineering'', 3, ''confidential'')');

  WriteLn('   Created 6 documents with various classifications');

  // Customers (multi-tenant: tenant 1 = Engineering, tenant 2 = Sales)
  Conn.ExecuteNonQuery('INSERT INTO customers (tenant_id, name, email, revenue, assigned_to) VALUES (1, ''TechCorp'', ''contact@techcorp.com'', 50000, 3)');
  Conn.ExecuteNonQuery('INSERT INTO customers (tenant_id, name, email, revenue, assigned_to) VALUES (1, ''DevHouse'', ''info@devhouse.com'', 35000, 4)');
  Conn.ExecuteNonQuery('INSERT INTO customers (tenant_id, name, email, revenue, assigned_to) VALUES (2, ''RetailPlus'', ''sales@retailplus.com'', 120000, 6)');
  Conn.ExecuteNonQuery('INSERT INTO customers (tenant_id, name, email, revenue, assigned_to) VALUES (2, ''MarketPro'', ''hello@marketpro.com'', 85000, 6)');
  Conn.ExecuteNonQuery('INSERT INTO customers (tenant_id, name, email, revenue, assigned_to) VALUES (2, ''BrandNew Inc'', ''info@brandnew.com'', 200000, 5)');

  WriteLn('   Created 5 customers across 2 tenants');

  // Security policies
  Conn.ExecuteNonQuery(
    'INSERT INTO security_policies (policy_name, target_table, role, filter_type, can_read, can_write, can_delete) ' +
    'VALUES (''admin_full_access'', ''*'', ''admin'', ''all'', 1, 1, 1)');
  Conn.ExecuteNonQuery(
    'INSERT INTO security_policies (policy_name, target_table, role, filter_type, can_read, can_write) ' +
    'VALUES (''manager_team_records'', ''employee_records'', ''manager'', ''team'', 1, 1)');
  Conn.ExecuteNonQuery(
    'INSERT INTO security_policies (policy_name, target_table, role, filter_type, can_read) ' +
    'VALUES (''employee_own_records'', ''employee_records'', ''employee'', ''own'', 1)');
  Conn.ExecuteNonQuery(
    'INSERT INTO security_policies (policy_name, target_table, role, filter_type, can_read) ' +
    'VALUES (''manager_dept_projects'', ''projects'', ''manager'', ''department'', 1)');
  Conn.ExecuteNonQuery(
    'INSERT INTO security_policies (policy_name, target_table, role, filter_type, can_read) ' +
    'VALUES (''viewer_public_only'', ''documents'', ''viewer'', ''own'', 1)');

  WriteLn('   Created 5 security policies');
  WriteLn('');
end;

// =============================================================================
// Simulated RLS: Query with filters applied based on current user
// =============================================================================
{ Sets the global CurrentUserId and CurrentRole variables by looking up the user's role from the database. }
procedure SetCurrentUser(UserId: Integer);
begin
  CurrentUserId := UserId;
  CurrentRole := VarToStr(Conn.ExecuteScalar(
    'SELECT role FROM users WHERE id = ?', [UserId]));
end;

{ Builds a SQL query for employee_records filtered by the current user's role: admins see all, managers see their department, employees see only their own record, others see nothing. }
function GetEmployeeRecordsSQL: string;
var
  Dept: string;
begin
  case CurrentRole of
    'admin':
      Result := 'SELECT er.*, u.display_name FROM employee_records er JOIN users u ON er.user_id = u.id';
    'manager':
    begin
      Dept := VarToStr(Conn.ExecuteScalar('SELECT department FROM users WHERE id = ?', [CurrentUserId]));
      Result := 'SELECT er.*, u.display_name FROM employee_records er JOIN users u ON er.user_id = u.id ' +
        'WHERE er.department = ''' + Dept + '''';
    end;
    'employee':
      Result := 'SELECT er.*, u.display_name FROM employee_records er JOIN users u ON er.user_id = u.id ' +
        'WHERE er.user_id = ' + IntToStr(CurrentUserId);
  else
    Result := 'SELECT er.*, u.display_name FROM employee_records er JOIN users u ON er.user_id = u.id WHERE 1=0';
  end;
end;

{ Builds a SQL query for documents filtered by the current user's role and department, restricting access based on classification levels (public/internal/confidential/restricted). }
function GetDocumentsSQL: string;
var
  Dept: string;
begin
  case CurrentRole of
    'admin':
      Result := 'SELECT * FROM documents';
    'manager':
    begin
      Dept := VarToStr(Conn.ExecuteScalar('SELECT department FROM users WHERE id = ?', [CurrentUserId]));
      Result := 'SELECT * FROM documents WHERE classification = ''public'' ' +
        'OR (department = ''' + Dept + ''' AND classification IN (''internal'', ''confidential'')) ' +
        'OR owner_id = ' + IntToStr(CurrentUserId);
    end;
    'employee':
    begin
      Dept := VarToStr(Conn.ExecuteScalar('SELECT department FROM users WHERE id = ?', [CurrentUserId]));
      Result := 'SELECT * FROM documents WHERE classification = ''public'' ' +
        'OR (department = ''' + Dept + ''' AND classification = ''internal'') ' +
        'OR owner_id = ' + IntToStr(CurrentUserId);
    end;
    'viewer':
      Result := 'SELECT * FROM documents WHERE classification = ''public''';
  else
    Result := 'SELECT * FROM documents WHERE 1=0';
  end;
end;

{ Builds a SQL query for projects filtered by the current user's role, department, and project classification level. }
function GetProjectsSQL: string;
var
  Dept: string;
begin
  case CurrentRole of
    'admin':
      Result := 'SELECT * FROM projects';
    'manager':
    begin
      Dept := VarToStr(Conn.ExecuteScalar('SELECT department FROM users WHERE id = ?', [CurrentUserId]));
      Result := 'SELECT * FROM projects WHERE department = ''' + Dept + ''' ' +
        'OR classification = ''public''';
    end;
    'employee':
    begin
      Dept := VarToStr(Conn.ExecuteScalar('SELECT department FROM users WHERE id = ?', [CurrentUserId]));
      Result := 'SELECT * FROM projects WHERE (department = ''' + Dept + ''' ' +
        'AND classification IN (''public'', ''internal'')) ' +
        'OR owner_id = ' + IntToStr(CurrentUserId);
    end;
    'viewer':
      Result := 'SELECT * FROM projects WHERE classification = ''public''';
  else
    Result := 'SELECT * FROM projects WHERE 1=0';
  end;
end;

{ Builds a SQL query for customers applying tenant isolation: admins see all, managers see their department's tenant, employees see only their assigned customers. }
function GetCustomersSQL: string;
var
  TenantId: Integer;
begin
  case CurrentRole of
    'admin':
      Result := 'SELECT * FROM customers';
    'manager':
    begin
      TenantId := Conn.ExecuteScalar(
        'SELECT CASE department WHEN ''Engineering'' THEN 1 WHEN ''Sales'' THEN 2 ELSE 0 END FROM users WHERE id = ?',
        [CurrentUserId]);
      Result := 'SELECT * FROM customers WHERE tenant_id = ' + IntToStr(TenantId);
    end;
    'employee':
      Result := 'SELECT * FROM customers WHERE assigned_to = ' + IntToStr(CurrentUserId);
  else
    Result := 'SELECT * FROM customers WHERE 1=0';
  end;
end;

{ Inserts an entry into the access_log table recording the current user, target table, action performed, rows returned, filter applied, and whether access was denied. }
procedure LogAccess(const TargetTable, Action, Filter: string; RowCount: Integer; Denied: Boolean);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO access_log (user_id, target_table, action, row_count, filter_applied, was_denied) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [CurrentUserId, TargetTable, Action, RowCount, Filter, Ord(Denied)]);
end;

// =============================================================================
// Demo: Employee Records Access
// =============================================================================
{ Queries employee_records as four different users (admin, manager, employee, viewer) to show how each role sees different subsets of salary and performance data based on RLS filters. }
procedure DemoEmployeeAccess;
var
  DS: TDataSet;
  UserName: string;
  Count: Integer;
begin
  WriteLn('3. Employee Records - Role-Based Access');
  WriteLn('   ======================================');
  WriteLn('');

  // Admin view - sees everything
  SetCurrentUser(1);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s):', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetEmployeeRecordsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     %s | Dept: %s | Salary: $%.0f | Rating: %d/5',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('department').AsString,
         DS.FieldByName('salary').AsFloat,
         DS.FieldByName('performance_rating').AsInteger]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d records visible', [Count]));
    LogAccess('employee_records', 'SELECT', 'all', Count, False);
  finally
    DS.Free;
  end;

  // Manager view - sees only their department
  WriteLn('');
  SetCurrentUser(2);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s, dept: Engineering):', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetEmployeeRecordsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     %s | Salary: $%.0f | Rating: %d/5',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('salary').AsFloat,
         DS.FieldByName('performance_rating').AsInteger]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d records visible (department filter)', [Count]));
    LogAccess('employee_records', 'SELECT', 'department=Engineering', Count, False);
  finally
    DS.Free;
  end;

  // Employee view - sees only their own
  WriteLn('');
  SetCurrentUser(3);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s):', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetEmployeeRecordsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     %s | Salary: $%.0f | Rating: %d/5',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('salary').AsFloat,
         DS.FieldByName('performance_rating').AsInteger]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d record visible (own data only)', [Count]));
    LogAccess('employee_records', 'SELECT', 'user_id=3', Count, False);
  finally
    DS.Free;
  end;

  // Viewer - sees nothing
  WriteLn('');
  SetCurrentUser(7);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s):', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetEmployeeRecordsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      DS.Next;
    end;
    WriteLn(Format('     -> %d records visible (ACCESS DENIED)', [Count]));
    LogAccess('employee_records', 'SELECT', 'denied', Count, True);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Document Classification Access
// =============================================================================
{ Queries documents as four different users to demonstrate classification-based access control, where visibility depends on role, department, and document classification level. }
procedure DemoDocumentAccess;
var
  DS: TDataSet;
  UserName: string;
  Count: Integer;
begin
  WriteLn('4. Document Access - Classification-Based');
  WriteLn('   =========================================');
  WriteLn('');

  // Admin sees all
  SetCurrentUser(1);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s) - sees ALL documents:', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetDocumentsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     [%s] %s (dept: %s)',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('title').AsString,
         IfThen(VarIsNull(DS.FieldByName('department').Value), 'All', DS.FieldByName('department').AsString)]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d documents visible', [Count]));
  finally
    DS.Free;
  end;

  // Manager sees public + own department internal/confidential
  WriteLn('');
  SetCurrentUser(5);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s, dept: Sales):', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetDocumentsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     [%s] %s',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('title').AsString]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d documents visible (public + Sales dept)', [Count]));
  finally
    DS.Free;
  end;

  // Employee sees public + own department internal + own docs
  WriteLn('');
  SetCurrentUser(3);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s, dept: Engineering):', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetDocumentsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     [%s] %s',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('title').AsString]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d documents visible (public + Engineering internal + owned)', [Count]));
  finally
    DS.Free;
  end;

  // Viewer sees only public
  WriteLn('');
  SetCurrentUser(7);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (role: %s):', [UserName, CurrentRole]));
  DS := Conn.ExecuteQuery(GetDocumentsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     [%s] %s',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('title').AsString]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d document visible (public only)', [Count]));
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Multi-Tenant Isolation
// =============================================================================
{ Queries customers as admin, manager, and employee to demonstrate multi-tenant data isolation where each user only sees customers within their tenant or assignment scope. }
procedure DemoTenantIsolation;
var
  DS: TDataSet;
  UserName: string;
  Count: Integer;
begin
  WriteLn('5. Multi-Tenant Customer Isolation');
  WriteLn('   =================================');
  WriteLn('');

  // Admin sees all customers
  SetCurrentUser(1);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (admin) - ALL customers:', [UserName]));
  DS := Conn.ExecuteQuery(GetCustomersSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     Tenant %d | %s | Revenue: $%.0f',
        [DS.FieldByName('tenant_id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d customers visible', [Count]));
  finally
    DS.Free;
  end;

  // Engineering manager sees tenant 1
  WriteLn('');
  SetCurrentUser(2);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (manager, Engineering = Tenant 1):', [UserName]));
  DS := Conn.ExecuteQuery(GetCustomersSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     %s | Revenue: $%.0f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d customers visible (tenant isolation)', [Count]));
  finally
    DS.Free;
  end;

  // Employee sees only assigned customers
  WriteLn('');
  SetCurrentUser(6);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (employee) - assigned customers only:', [UserName]));
  DS := Conn.ExecuteQuery(GetCustomersSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     %s | Revenue: $%.0f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d customers visible (assigned only)', [Count]));
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Project Access by Classification
// =============================================================================
{ Queries projects as admin, employee, and viewer to show how project visibility varies by role, department membership, and classification level (public/internal/confidential/restricted). }
procedure DemoProjectAccess;
var
  DS: TDataSet;
  UserName: string;
  Count: Integer;
begin
  WriteLn('6. Project Access - Classification Levels');
  WriteLn('   =========================================');
  WriteLn('');

  // Admin sees everything
  SetCurrentUser(1);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (admin):', [UserName]));
  DS := Conn.ExecuteQuery(GetProjectsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     [%s] %s (dept: %s, budget: $%.0f)',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('department').AsString,
         DS.FieldByName('budget').AsFloat]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d projects visible', [Count]));
  finally
    DS.Free;
  end;

  // Engineering employee (sees public + internal in dept + own)
  WriteLn('');
  SetCurrentUser(3);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (employee, Engineering):', [UserName]));
  DS := Conn.ExecuteQuery(GetProjectsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     [%s] %s (budget: $%.0f)',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('budget').AsFloat]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d projects visible (no confidential/restricted)', [Count]));
  finally
    DS.Free;
  end;

  // Viewer sees only public
  WriteLn('');
  SetCurrentUser(7);
  UserName := VarToStr(Conn.ExecuteScalar('SELECT display_name FROM users WHERE id = ?', [CurrentUserId]));
  WriteLn(Format('   As %s (viewer):', [UserName]));
  DS := Conn.ExecuteQuery(GetProjectsSQL);
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      WriteLn(Format('     [%s] %s',
        [DS.FieldByName('classification').AsString,
         DS.FieldByName('name').AsString]));
      DS.Next;
    end;
    WriteLn(Format('     -> %d project visible (public only)', [Count]));
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Write Access Control
// =============================================================================
{ Displays a table showing write and delete permissions for each role by querying the security_policies table for employee_records access. }
procedure DemoWriteAccess;
var
  CanWrite: Integer;
begin
  WriteLn('7. Write Access Control');
  WriteLn('   ======================');
  WriteLn('');

  WriteLn('   Checking write permissions per role:');
  WriteLn('   ' + StringOfChar('-', 55));
  WriteLn(Format('   %-20s | %-10s | %-10s | %s', ['User', 'Table', 'Can Write', 'Can Delete']));
  WriteLn('   ' + StringOfChar('-', 55));

  // Admin
  SetCurrentUser(1);
  CanWrite := Conn.ExecuteScalar(
    'SELECT can_write FROM security_policies WHERE role = ''admin'' AND target_table = ''*'' AND is_active = 1');
  WriteLn(Format('   %-20s | %-10s | %-10s | %s',
    ['System Admin', '*', IfThen(CanWrite = 1, 'YES', 'NO'), 'YES']));

  // Manager on employee_records
  SetCurrentUser(2);
  CanWrite := Conn.ExecuteScalar(
    'SELECT can_write FROM security_policies WHERE role = ''manager'' AND target_table = ''employee_records'' AND is_active = 1');
  WriteLn(Format('   %-20s | %-10s | %-10s | %s',
    ['John Smith (mgr)', 'emp_records', IfThen(CanWrite = 1, 'YES', 'NO'), 'NO']));

  // Employee on employee_records
  SetCurrentUser(3);
  CanWrite := Conn.ExecuteScalar(
    'SELECT COALESCE(can_write, 0) FROM security_policies WHERE role = ''employee'' AND target_table = ''employee_records'' AND is_active = 1');
  WriteLn(Format('   %-20s | %-10s | %-10s | %s',
    ['Alice (employee)', 'emp_records', IfThen(CanWrite = 1, 'YES', 'NO'), 'NO']));

  // Viewer
  WriteLn(Format('   %-20s | %-10s | %-10s | %s',
    ['External Viewer', 'documents', 'NO', 'NO']));

  WriteLn('   ' + StringOfChar('-', 55));
  WriteLn('');
end;

// =============================================================================
// Demo: Security Policy Summary
// =============================================================================
{ Lists all active security policies showing policy name, target table, role, filter type, and read/write/delete permissions. }
procedure DemoPolicySummary;
var
  DS: TDataSet;
begin
  WriteLn('8. Security Policy Summary');
  WriteLn('   ==========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT policy_name, target_table, role, filter_type, can_read, can_write, can_delete ' +
    'FROM security_policies WHERE is_active = 1 ORDER BY role, target_table');
  try
    WriteLn(Format('   %-25s | %-15s | %-10s | %-6s | R | W | D',
      ['Policy', 'Table', 'Role', 'Filter']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s | %-15s | %-10s | %-6s | %s | %s | %s',
        [DS.FieldByName('policy_name').AsString,
         DS.FieldByName('target_table').AsString,
         DS.FieldByName('role').AsString,
         DS.FieldByName('filter_type').AsString,
         IfThen(DS.FieldByName('can_read').AsInteger = 1, 'Y', 'N'),
         IfThen(DS.FieldByName('can_write').AsInteger = 1, 'Y', 'N'),
         IfThen(DS.FieldByName('can_delete').AsInteger = 1, 'Y', 'N')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Access Audit Log
// =============================================================================
{ Displays the access_log table showing user, role, target table, row count, filter applied, and whether access was denied for each logged query. }
procedure DemoAccessLog;
var
  DS: TDataSet;
begin
  WriteLn('9. Access Audit Log');
  WriteLn('   ==================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT al.*, u.display_name, u.role ' +
    'FROM access_log al ' +
    'JOIN users u ON al.user_id = u.id ' +
    'ORDER BY al.timestamp DESC');
  try
    WriteLn(Format('   %-15s | %-10s | %-15s | %s | %s',
      ['User', 'Role', 'Table', 'Rows', 'Filter']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s | %-10s | %-15s | %4d | %s %s',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('role').AsString,
         DS.FieldByName('target_table').AsString,
         DS.FieldByName('row_count').AsInteger,
         DS.FieldByName('filter_applied').AsString,
         IfThen(DS.FieldByName('was_denied').AsInteger = 1, '[DENIED]', '')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Visibility Comparison
// =============================================================================
{ Generates a comparison matrix showing how many rows each user can see across employee_records, documents, projects, and customers tables versus total rows. }
procedure DemoVisibilityMatrix;
var
  DS: TDataSet;
  UserId, EmpCount, DocCount, ProjCount, CustCount: Integer;
  UserName, Role: string;
begin
  WriteLn('10. Visibility Comparison Matrix');
  WriteLn('    ==============================');
  WriteLn('');

  WriteLn(Format('    %-15s | %-10s | %-8s | %-8s | %-8s | %s',
    ['User', 'Role', 'EmpRecs', 'Docs', 'Projects', 'Customers']));
  WriteLn('    ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery('SELECT id, display_name, role FROM users ORDER BY id');
  try
    while not DS.EOF do
    begin
      UserId := DS.FieldByName('id').AsInteger;
      UserName := DS.FieldByName('display_name').AsString;
      Role := DS.FieldByName('role').AsString;

      SetCurrentUser(UserId);

      EmpCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM (' + GetEmployeeRecordsSQL + ')');
      DocCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM (' + GetDocumentsSQL + ')');
      ProjCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM (' + GetProjectsSQL + ')');
      CustCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM (' + GetCustomersSQL + ')');

      WriteLn(Format('    %-15s | %-10s | %4d/5  | %4d/6  | %4d/5  | %4d/5',
        [UserName, Role, EmpCount, DocCount, ProjCount, CustCount]));

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('    ' + StringOfChar('-', 70));
  WriteLn('    (numerator = visible rows / denominator = total rows)');
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 80: Row-Level Security ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;
    DemoEmployeeAccess;
    DemoDocumentAccess;
    DemoTenantIsolation;
    DemoProjectAccess;
    DemoWriteAccess;
    DemoPolicySummary;
    DemoAccessLog;
    DemoVisibilityMatrix;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
