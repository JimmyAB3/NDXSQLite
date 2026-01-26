# Example 80: Row-Level Security

## Overview

This example demonstrates **row-level security (RLS)** patterns for filtering data based on user identity and role. It implements multi-tenant isolation, department-scoped access, classification-based document control, and comprehensive audit logging — patterns essential for enterprise applications.

## Features Demonstrated

### 1. Role-Based Access Control (RBAC)
- Admin: full access to all data
- Manager: department-scoped access
- Employee: own data only
- Viewer: public data only

### 2. Classification Levels
- Public: visible to all users
- Internal: visible within department
- Confidential: visible to managers in department
- Restricted: admin-only access

### 3. Multi-Tenant Isolation
- Tenant-based customer data separation
- No cross-tenant data leakage
- Employee sees only assigned records

### 4. Security Policies
- Declarative policy definitions
- Per-table, per-role configurations
- Read/Write/Delete granularity
- Filter type specification (own, department, team, all)

### 5. Access Audit Trail
- All access attempts logged
- Denied access flagged
- Filter applied recorded
- Row count tracking

## Database Schema

```
+--------+     +-------------------+     +-----------------+
| users  |     | security_policies |     | employee_records|
+--------+     +-------------------+     +-----------------+
| id     |     | policy_name       |     | user_id         |
| role   |     | target_table      |     | department      |
| dept   |     | role              |     | salary          |
| manager|     | filter_type       |     | rating          |
+--------+     | can_read/write    |     +-----------------+
    |          +-------------------+
    |                                    +-----------+
    +------------------------------------>| documents |
    |                                    +-----------+
    |                                    | owner_id  |
    |                                    | dept      |
    |                                    | classif.  |
    |                                    +-----------+
    |
    +---> customers (tenant_id)
    +---> projects (classification)
    +---> access_log (audit)
```

## Key Patterns

### Dynamic SQL Filter Based on Role
```pascal
function GetEmployeeRecordsSQL: string;
begin
  case CurrentRole of
    'admin':
      Result := 'SELECT * FROM employee_records';
    'manager':
      Result := 'SELECT * FROM employee_records WHERE department = ''' + Dept + '''';
    'employee':
      Result := 'SELECT * FROM employee_records WHERE user_id = ' + IntToStr(CurrentUserId);
    else
      Result := 'SELECT * FROM employee_records WHERE 1=0'; // deny
  end;
end;
```

### Classification-Based Document Access
```pascal
// Manager sees: public + own department internal/confidential
Result := 'SELECT * FROM documents WHERE classification = ''public'' ' +
  'OR (department = ''' + Dept + ''' AND classification IN (''internal'', ''confidential''))';

// Viewer sees: public only
Result := 'SELECT * FROM documents WHERE classification = ''public''';
```

### Access Audit Logging
```pascal
procedure LogAccess(const TargetTable, Action, Filter: string;
  RowCount: Integer; Denied: Boolean);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO access_log (user_id, target_table, action, row_count, filter_applied, was_denied) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [CurrentUserId, TargetTable, Action, RowCount, Filter, Ord(Denied)]);
end;
```

## Demonstration Sections

1. **Schema Creation** - Security tables and policies
2. **Sample Data** - Users, records, documents, customers
3. **Employee Access** - Role-based record visibility
4. **Document Access** - Classification-based filtering
5. **Tenant Isolation** - Multi-tenant customer separation
6. **Project Access** - Classification level enforcement
7. **Write Control** - Read/write/delete permissions
8. **Policy Summary** - All active policies
9. **Audit Log** - Access attempt history
10. **Visibility Matrix** - Comparison across all users

## Access Matrix

| Role | Employee Records | Documents | Projects | Customers |
|------|-----------------|-----------|----------|-----------|
| Admin | All (5/5) | All (6/6) | All (5/5) | All (5/5) |
| Manager | Department (2-3/5) | Dept+Public (3/6) | Dept+Public (2-4/5) | Tenant (2-3/5) |
| Employee | Own only (1/5) | Internal+Own (1-3/6) | Internal+Own (1/5) | Assigned (1-2/5) |
| Viewer | None (0/5) | Public (1/6) | Public (1/5) | None (0/5) |

## Compilation

```bash
cd 80_RowLevelSecurity
lazbuild RowLevelSecurity.lpi
./RowLevelSecurity
```

## Sample Output

```
3. Employee Records - Role-Based Access
   ======================================

   As System Admin (role: admin):
     John Smith | Dept: Engineering | Salary: $120000 | Rating: 4/5
     Alice Johnson | Dept: Engineering | Salary: $95000 | Rating: 5/5
     ...
     -> 5 records visible

   As John Smith (role: manager, dept: Engineering):
     -> 3 records visible (department filter)

   As Alice Johnson (role: employee):
     -> 1 record visible (own data only)

   As External Viewer (role: viewer):
     -> 0 records visible (ACCESS DENIED)

10. Visibility Comparison Matrix
    ==============================

    User            | Role       | EmpRecs  | Docs     | Projects | Customers
    System Admin    | admin      |    5/5  |    6/6  |    5/5  |    5/5
    Alice Johnson   | employee   |    1/5  |    3/6  |    1/5  |    1/5
    External Viewer | viewer     |    0/5  |    1/6  |    1/5  |    0/5
```

## Related Examples

- **75_UserAuthentication** - User identity management
- **81_DataMasking** - Sensitive data masking
- **82_SecureDelete** - Secure data removal

## Production Considerations

1. **SQL Injection**: Use parameterized queries for user-supplied values
2. **Performance**: Index filter columns for RLS predicates
3. **Caching**: Cache policy lookups per session
4. **Views**: Create database views with built-in RLS filters
5. **Column-Level**: Extend to column-level security for field masking
6. **Hierarchical Roles**: Support role inheritance
7. **Temporal Policies**: Time-based access windows
