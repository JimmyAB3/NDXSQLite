# Example 42: Multi-Tenant Database

This example demonstrates two approaches to multi-tenancy with SQLite.

## What you'll learn

- Database per tenant approach
- Shared schema with tenant_id approach
- Tenant isolation patterns
- Cross-tenant queries (admin)
- Trade-offs between approaches

## Approach 1: Database per Tenant

Each tenant has their own SQLite database file.

### Structure

```
data/
  tenant_acme.db
  tenant_globex.db
  tenant_initech.db
```

### Implementation

```pascal
type
  TTenantConnection = class
  private
    FConnection: TNDXSQLiteConnection;
    FTenantId: string;
  public
    constructor Create(const ATenantId, ABaseDir: string);
    property Connection: TNDXSQLiteConnection read FConnection;
  end;

constructor TTenantConnection.Create(const ATenantId, ABaseDir: string);
begin
  FTenantId := ATenantId;
  // Each tenant gets their own database file
  FOptions.DatabasePath := ABaseDir + 'tenant_' + ATenantId + '.db';
  FConnection := TNDXSQLiteConnection.Create(FOptions);
  FConnection.Open;
end;
```

### Advantages

- Complete data isolation
- Easy per-tenant backup/restore
- No risk of data leakage
- Independent scaling
- Different schemas possible

### Disadvantages

- More files to manage
- Schema migrations per database
- Cross-tenant queries difficult
- Connection management overhead

## Approach 2: Shared Schema with tenant_id

All tenants share one database with tenant_id column.

### Schema

```sql
CREATE TABLE tenants (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  plan TEXT DEFAULT 'basic',
  active INTEGER DEFAULT 1
);

CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  tenant_id TEXT NOT NULL REFERENCES tenants(id),
  name TEXT NOT NULL,
  email TEXT,
  UNIQUE(tenant_id, email)
);

CREATE INDEX idx_users_tenant ON users(tenant_id);
```

### Implementation

```pascal
var
  CurrentTenantId: string;

procedure SetCurrentTenant(const ATenantId: string);
begin
  CurrentTenantId := ATenantId;
end;

// Always filter by tenant
DS := Connection.ExecuteQuery(
  'SELECT * FROM users WHERE tenant_id = ?',
  [CurrentTenantId]);
```

### Advantages

- Single database to manage
- Easy cross-tenant analytics
- One schema migration for all
- Resource efficient

### Disadvantages

- Risk of data leakage
- Need tenant_id on all queries
- Larger single database
- Shared resources

## Comparison

| Aspect | DB per Tenant | Shared Schema |
|--------|---------------|---------------|
| Isolation | Complete | Application-level |
| Backup | Per tenant | All or nothing |
| Scaling | Independent | Shared |
| Migrations | Per database | Single |
| Analytics | Difficult | Easy |
| Files | Many | One |

## When to use which

**Database per Tenant:**
- Few large tenants
- Strict compliance requirements
- Custom schemas per tenant
- Tenant-specific backup needs

**Shared Schema:**
- Many small tenants
- Cross-tenant analytics needed
- Simple, uniform schema
- Resource constraints

## Security considerations

### Database per Tenant
```pascal
// Connection is already isolated
DS := TenantConn.ExecuteQuery('SELECT * FROM users');
```

### Shared Schema
```pascal
// ALWAYS filter by tenant_id
DS := Connection.ExecuteQuery(
  'SELECT * FROM users WHERE tenant_id = ?',
  [CurrentTenantId]);

// Use views for safety
Connection.ExecuteNonQuery(
  'CREATE VIEW tenant_users AS ' +
  'SELECT * FROM users WHERE tenant_id = current_tenant()');
```

## Building

```bash
lazbuild MultiTenantDB.lpi
```

## Running

```bash
./MultiTenantDB      # Linux/macOS
MultiTenantDB.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
