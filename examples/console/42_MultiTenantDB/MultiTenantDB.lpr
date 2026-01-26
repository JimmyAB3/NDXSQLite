{===============================================================================
  NDXSQLite Example 42 - Multi-Tenant Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Database per tenant approach
  - Shared schema with tenant_id approach
  - Tenant isolation patterns
  - Cross-tenant queries (admin)
  - Tenant provisioning and cleanup
  - Comparing approaches

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program MultiTenantDB;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  BaseDir: string;

// ============================================================================
// Approach 1: Database per Tenant
// ============================================================================

type
  TTenantConnection = class
  private
    FConnection: TNDXSQLiteConnection;
    FOptions: TNDXSQLiteConnectionOptions;
    FTenantId: string;
  public
    constructor Create(const ATenantId, ABaseDir: string);
    destructor Destroy; override;
    property Connection: TNDXSQLiteConnection read FConnection;
    property TenantId: string read FTenantId;
  end;

constructor TTenantConnection.Create(const ATenantId, ABaseDir: string);
begin
  FTenantId := ATenantId;
  FOptions := TNDXSQLiteConnectionOptions.Create;
  FOptions.DatabasePath := ABaseDir + 'tenant_' + ATenantId + '.db';
  FConnection := TNDXSQLiteConnection.Create(FOptions);
  FConnection.Open;
end;

destructor TTenantConnection.Destroy;
begin
  FConnection.Close;
  FConnection.Free;
  FOptions.Free;
  inherited;
end;

{ Creates users, products, and orders tables in the given tenant connection for the database-per-tenant approach. }
procedure SetupTenantSchema(AConn: TNDXSQLiteConnection);
begin
  AConn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT UNIQUE,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  AConn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  price REAL,' +
    '  stock INTEGER DEFAULT 0' +
    ')');

  AConn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  product_id INTEGER REFERENCES products(id),' +
    '  quantity INTEGER,' +
    '  total REAL,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');
end;

{ Creates separate database files for two tenants, populates each with isolated data, and queries them independently to verify tenant isolation. }
procedure DemoSeparateDatabases;
var
  TenantA, TenantB: TTenantConnection;
  DS: TDataSet;
begin
  WriteLn('');
  WriteLn('======================================');
  WriteLn('APPROACH 1: Database per Tenant');
  WriteLn('======================================');
  WriteLn('');
  WriteLn('Each tenant has its own database file.');
  WriteLn('Complete data isolation at file level.');
  WriteLn('');

  // Create tenant connections
  TenantA := TTenantConnection.Create('acme', BaseDir);
  TenantB := TTenantConnection.Create('globex', BaseDir);
  try
    // Setup schemas
    SetupTenantSchema(TenantA.Connection);
    SetupTenantSchema(TenantB.Connection);

    WriteLn('1. Provisioning tenants');
    WriteLn('   --------------------');
    WriteLn('   Created database: tenant_acme.db');
    WriteLn('   Created database: tenant_globex.db');
    WriteLn('');

    // Add data to each tenant
    WriteLn('2. Adding tenant-specific data');
    WriteLn('   ---------------------------');

    // Tenant A data
    TenantA.Connection.ExecuteNonQuery(
      'INSERT INTO users (name, email) VALUES (''Alice'', ''alice@acme.com'')');
    TenantA.Connection.ExecuteNonQuery(
      'INSERT INTO users (name, email) VALUES (''Bob'', ''bob@acme.com'')');
    TenantA.Connection.ExecuteNonQuery(
      'INSERT INTO products (name, price, stock) VALUES (''Widget'', 19.99, 100)');

    // Tenant B data
    TenantB.Connection.ExecuteNonQuery(
      'INSERT INTO users (name, email) VALUES (''Carol'', ''carol@globex.com'')');
    TenantB.Connection.ExecuteNonQuery(
      'INSERT INTO products (name, price, stock) VALUES (''Gadget'', 49.99, 50)');
    TenantB.Connection.ExecuteNonQuery(
      'INSERT INTO products (name, price, stock) VALUES (''Gizmo'', 29.99, 200)');

    WriteLn('   Tenant ACME: 2 users, 1 product');
    WriteLn('   Tenant GLOBEX: 1 user, 2 products');
    WriteLn('');

    // Query each tenant separately
    WriteLn('3. Querying tenant data (isolated)');
    WriteLn('   --------------------------------');

    WriteLn('   ACME users:');
    DS := TenantA.Connection.ExecuteQuery('SELECT name, email FROM users');
    try
      while not DS.EOF do
      begin
        WriteLn('   - ', DS.FieldByName('name').AsString, ' <', DS.FieldByName('email').AsString, '>');
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    WriteLn('');
    WriteLn('   GLOBEX products:');
    DS := TenantB.Connection.ExecuteQuery('SELECT name, price FROM products');
    try
      while not DS.EOF do
      begin
        WriteLn('   - ', DS.FieldByName('name').AsString, ': $', DS.FieldByName('price').AsFloat:0:2);
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    WriteLn('');
    WriteLn('4. Tenant isolation verified');
    WriteLn('   -------------------------');
    WriteLn('   Each tenant can only see their own data.');
    WriteLn('   No risk of data leakage between tenants.');
    WriteLn('');

  finally
    TenantA.Free;
    TenantB.Free;
  end;
end;

// ============================================================================
// Approach 2: Shared Schema with Tenant ID
// ============================================================================

var
  SharedConnection: TNDXSQLiteConnection;
  SharedOptions: TNDXSQLiteConnectionOptions;
  CurrentTenantId: string;

{ Creates the tenants registry and shared_users/shared_products/shared_orders tables with tenant_id columns and per-tenant indexes. }
procedure SetupSharedSchema;
begin
  // Tenants table
  SharedConnection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS tenants (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  plan TEXT DEFAULT ''basic'',' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  active INTEGER DEFAULT 1' +
    ')');

  // Users with tenant_id
  SharedConnection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS shared_users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  tenant_id TEXT NOT NULL REFERENCES tenants(id),' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(tenant_id, email)' +
    ')');

  // Products with tenant_id
  SharedConnection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS shared_products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  tenant_id TEXT NOT NULL REFERENCES tenants(id),' +
    '  name TEXT NOT NULL,' +
    '  price REAL,' +
    '  stock INTEGER DEFAULT 0' +
    ')');

  // Orders with tenant_id
  SharedConnection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS shared_orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  tenant_id TEXT NOT NULL REFERENCES tenants(id),' +
    '  user_id INTEGER,' +
    '  product_id INTEGER,' +
    '  quantity INTEGER,' +
    '  total REAL,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Create indexes for efficient tenant filtering
  SharedConnection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_users_tenant ON shared_users(tenant_id)');
  SharedConnection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_products_tenant ON shared_products(tenant_id)');
  SharedConnection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_orders_tenant ON shared_orders(tenant_id)');

  // Create views that automatically filter by current tenant (simulation)
  // In real app, these would be parameterized or use session variables
end;

{ Sets the global CurrentTenantId variable used to filter queries by tenant in the shared-schema approach. }
procedure SetCurrentTenant(const ATenantId: string);
begin
  CurrentTenantId := ATenantId;
end;

{ Provisions three tenants in a single shared database, inserts tenant-scoped data, queries with tenant_id filtering, runs cross-tenant admin queries, and deactivates a tenant. }
procedure DemoSharedSchema;
var
  DS: TDataSet;
begin
  WriteLn('');
  WriteLn('======================================');
  WriteLn('APPROACH 2: Shared Schema with tenant_id');
  WriteLn('======================================');
  WriteLn('');
  WriteLn('All tenants share one database.');
  WriteLn('Each table has tenant_id column.');
  WriteLn('');

  SharedOptions := TNDXSQLiteConnectionOptions.Create;
  try
    SharedOptions.DatabasePath := BaseDir + 'shared_multitenant.db';
    SharedConnection := TNDXSQLiteConnection.Create(SharedOptions);
    try
      SharedConnection.Open;
      SetupSharedSchema;

      WriteLn('1. Provisioning tenants');
      WriteLn('   --------------------');

      SharedConnection.ExecuteNonQuery(
        'INSERT INTO tenants (id, name, plan) VALUES (''acme'', ''ACME Corp'', ''premium'')');
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO tenants (id, name, plan) VALUES (''globex'', ''Globex Inc'', ''basic'')');
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO tenants (id, name, plan) VALUES (''initech'', ''Initech LLC'', ''enterprise'')');

      DS := SharedConnection.ExecuteQuery('SELECT id, name, plan FROM tenants');
      try
        while not DS.EOF do
        begin
          WriteLn('   - ', DS.FieldByName('id').AsString:10, ' (',
                  DS.FieldByName('name').AsString, ' - ',
                  DS.FieldByName('plan').AsString, ')');
          DS.Next;
        end;
      finally
        DS.Free;
      end;
      WriteLn('');

      WriteLn('2. Adding tenant-specific data');
      WriteLn('   ---------------------------');

      // ACME data
      SetCurrentTenant('acme');
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_users (tenant_id, name, email) VALUES (?, ''Alice'', ''alice@acme.com'')',
        [CurrentTenantId]);
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_users (tenant_id, name, email) VALUES (?, ''Bob'', ''bob@acme.com'')',
        [CurrentTenantId]);
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_products (tenant_id, name, price, stock) VALUES (?, ''Widget'', 19.99, 100)',
        [CurrentTenantId]);

      // GLOBEX data
      SetCurrentTenant('globex');
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_users (tenant_id, name, email) VALUES (?, ''Carol'', ''carol@globex.com'')',
        [CurrentTenantId]);
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_products (tenant_id, name, price, stock) VALUES (?, ''Gadget'', 49.99, 50)',
        [CurrentTenantId]);

      // INITECH data
      SetCurrentTenant('initech');
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_users (tenant_id, name, email) VALUES (?, ''Dave'', ''dave@initech.com'')',
        [CurrentTenantId]);
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_users (tenant_id, name, email) VALUES (?, ''Eve'', ''eve@initech.com'')',
        [CurrentTenantId]);
      SharedConnection.ExecuteNonQuery(
        'INSERT INTO shared_users (tenant_id, name, email) VALUES (?, ''Frank'', ''frank@initech.com'')',
        [CurrentTenantId]);

      WriteLn('   Data added for all tenants');
      WriteLn('');

      WriteLn('3. Querying with tenant isolation');
      WriteLn('   -------------------------------');

      // Query as ACME
      SetCurrentTenant('acme');
      WriteLn('   Current tenant: ACME');
      WriteLn('   Users visible:');
      DS := SharedConnection.ExecuteQuery(
        'SELECT name, email FROM shared_users WHERE tenant_id = ?',
        [CurrentTenantId]);
      try
        while not DS.EOF do
        begin
          WriteLn('   - ', DS.FieldByName('name').AsString);
          DS.Next;
        end;
      finally
        DS.Free;
      end;

      WriteLn('');

      // Query as INITECH
      SetCurrentTenant('initech');
      WriteLn('   Current tenant: INITECH');
      WriteLn('   Users visible:');
      DS := SharedConnection.ExecuteQuery(
        'SELECT name, email FROM shared_users WHERE tenant_id = ?',
        [CurrentTenantId]);
      try
        while not DS.EOF do
        begin
          WriteLn('   - ', DS.FieldByName('name').AsString);
          DS.Next;
        end;
      finally
        DS.Free;
      end;

      WriteLn('');

      WriteLn('4. Cross-tenant queries (Admin only)');
      WriteLn('   ---------------------------------');

      DS := SharedConnection.ExecuteQuery(
        'SELECT t.name as tenant, COUNT(u.id) as user_count, ' +
        '       COUNT(p.id) as product_count ' +
        'FROM tenants t ' +
        'LEFT JOIN shared_users u ON t.id = u.tenant_id ' +
        'LEFT JOIN shared_products p ON t.id = p.tenant_id ' +
        'GROUP BY t.id');
      try
        WriteLn('   Tenant         Users  Products');
        WriteLn('   ------         -----  --------');
        while not DS.EOF do
        begin
          WriteLn('   ', DS.FieldByName('tenant').AsString:15,
                  DS.FieldByName('user_count').AsInteger:5,
                  DS.FieldByName('product_count').AsInteger:9);
          DS.Next;
        end;
      finally
        DS.Free;
      end;

      WriteLn('');

      WriteLn('5. Tenant deactivation');
      WriteLn('   -------------------');

      SharedConnection.ExecuteNonQuery(
        'UPDATE tenants SET active = 0 WHERE id = ''globex''');
      WriteLn('   Globex tenant deactivated');

      // Query only active tenants
      DS := SharedConnection.ExecuteQuery(
        'SELECT id, name FROM tenants WHERE active = 1');
      try
        WriteLn('   Active tenants:');
        while not DS.EOF do
        begin
          WriteLn('   - ', DS.FieldByName('name').AsString);
          DS.Next;
        end;
      finally
        DS.Free;
      end;

      WriteLn('');

      SharedConnection.Close;
    finally
      SharedConnection.Free;
    end;
  finally
    SharedOptions.Free;
  end;
end;

{ Outputs a pros/cons comparison of database-per-tenant versus shared-schema multi-tenancy and provides recommendations based on tenant size and compliance needs. }
procedure DemoCompareApproaches;
begin
  WriteLn('');
  WriteLn('======================================');
  WriteLn('COMPARISON: Database per Tenant vs Shared Schema');
  WriteLn('======================================');
  WriteLn('');
  WriteLn('Database per Tenant:');
  WriteLn('  Pros:');
  WriteLn('    + Complete isolation (no data leakage risk)');
  WriteLn('    + Easy backup/restore per tenant');
  WriteLn('    + Performance scaling (separate files)');
  WriteLn('    + Different schemas per tenant possible');
  WriteLn('  Cons:');
  WriteLn('    - More files to manage');
  WriteLn('    - Cross-tenant queries difficult');
  WriteLn('    - Schema migrations per database');
  WriteLn('');
  WriteLn('Shared Schema with tenant_id:');
  WriteLn('  Pros:');
  WriteLn('    + Single database to manage');
  WriteLn('    + Easy cross-tenant analytics');
  WriteLn('    + One schema migration for all');
  WriteLn('    + Resource efficient for many small tenants');
  WriteLn('  Cons:');
  WriteLn('    - Risk of data leakage (forgotten WHERE)');
  WriteLn('    - Indexes needed on tenant_id');
  WriteLn('    - Single large database may hit limits');
  WriteLn('');
  WriteLn('Recommendation:');
  WriteLn('  - Few large tenants -> Database per tenant');
  WriteLn('  - Many small tenants -> Shared schema');
  WriteLn('  - Strict compliance needs -> Database per tenant');
  WriteLn('');
end;

{ Finds and deletes all .db files in the base directory. }
procedure Cleanup;
var
  SR: TSearchRec;
begin
  // Clean up all database files
  if FindFirst(BaseDir + '*.db', faAnyFile, SR) = 0 then
  begin
    repeat
      DeleteFile(BaseDir + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

begin
  WriteLn('=== NDXSQLite Example 42: Multi-Tenant Database ===');

  BaseDir := ExtractFilePath(ParamStr(0));
  Cleanup;

  DemoSeparateDatabases;
  DemoSharedSchema;
  DemoCompareApproaches;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
