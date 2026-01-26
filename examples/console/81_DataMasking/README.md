# Example 81: Data Masking (PII Protection)

## Overview

This example demonstrates **data masking techniques** for protecting Personally Identifiable Information (PII). It implements email, phone, credit card, SSN, name, and address masking with role-based access levels and unmask audit trails — essential patterns for GDPR, HIPAA, and PCI DSS compliance.

## Features Demonstrated

### 1. PII Masking Functions
- Email: `j******e@email.com`
- Phone: `***-***-4567`
- Credit card: `****-****-****-4444`
- SSN: `***-**-6789`
- Name: `J. D.`
- Address: `***, Springfield`

### 2. Role-Based Masking Levels
- Admin: full access to all data
- Data Officer: see emails, cards; SSN masked
- Analyst: see DOB; emails, SSN masked
- Support: see phone; emails, cards masked
- Viewer: all PII masked

### 3. Classification Sensitivity
- Public fields: always visible
- Internal: masked for external users
- Confidential: requires data officer+
- Restricted (medical, SSN): admin only

### 4. Masking Policies
- Declarative policy definitions
- Per-table, per-column configuration
- Minimum role to unmask specification
- Active/inactive toggle

### 5. Unmask Audit Trail
- Every unmask action logged
- Reason required for audit
- User and timestamp tracking
- Record-level granularity

## Database Schema

```
+--------+     +------------------+
| users  |     | masking_policies |
+--------+     +------------------+
| id     |     | target_table     |
| role   |     | target_column    |
+--------+     | data_type        |
    |          | min_role_to_     |
    |          | unmask           |
    |          +------------------+
    |
    +---> customers (email, phone, ssn, dob, address)
    +---> payment_methods (card_number, holder)
    +---> medical_records (diagnosis, medication)
    +---> unmask_log (audit trail)
```

## Key Patterns

### Email Masking
```pascal
function MaskEmail(const Email: string): string;
begin
  // john.doe@email.com -> j******e@email.com
  Result := LocalPart[1] + StringOfChar('*', Len - 2) + LocalPart[Len] + Domain;
end;
```

### Role Hierarchy for Unmask
```pascal
function RoleLevel(const Role: string): Integer;
begin
  if Role = 'admin' then Result := 5
  else if Role = 'data_officer' then Result := 4
  else if Role = 'analyst' then Result := 3
  else if Role = 'support' then Result := 2
  else Result := 1; // viewer
end;

function CanUnmask(const UserRole, MinRole: string): Boolean;
begin
  Result := RoleLevel(UserRole) >= RoleLevel(MinRole);
end;
```

### Dynamic Masking Application
```pascal
if CanUnmask(CurrentRole, 'data_officer') then
  Email := DS.FieldByName('email').AsString    // show real data
else
  Email := MaskEmail(DS.FieldByName('email').AsString);  // mask it
```

### Unmask Audit
```sql
INSERT INTO unmask_log (user_id, target_table, target_column, record_id, reason)
VALUES (?, ?, ?, ?, ?);
```

## Demonstration Sections

1. **Schema Creation** - PII tables and policies
2. **Sample Data** - Customers, payments, medical records
3. **Masking Functions** - Each masking type demonstrated
4. **Role-Based View** - Same data through different roles
5. **Payment Masking** - Credit card protection
6. **Medical Records** - Maximum sensitivity handling
7. **Policies** - Active masking policy summary
8. **Dynamic Masking** - Progressive reveal by role
9. **Unmask Audit** - Access audit trail
10. **Statistics** - PII inventory and coverage

## Masking Types

| Type | Original | Masked | Last N Visible |
|------|----------|--------|----------------|
| Email | john.doe@email.com | j******e@email.com | First + Last char |
| Phone | 555-123-4567 | ***-***-4567 | Last 4 digits |
| Credit Card | 4111222233334444 | ****-****-****-4444 | Last 4 digits |
| SSN | 123-45-6789 | ***-**-6789 | Last 4 digits |
| Name | John Doe | J. D. | Initials only |
| Date | 1985-03-15 | ****-**-** | Fully masked |

## Role Access Matrix

| Field | Admin | Data Officer | Analyst | Support | Viewer |
|-------|-------|-------------|---------|---------|--------|
| Email | Clear | Clear | Masked | Masked | Masked |
| Phone | Clear | Clear | Clear | Clear | Masked |
| SSN | Clear | Masked | Masked | Masked | Masked |
| DOB | Clear | Clear | Clear | Masked | Masked |
| Credit Card | Clear | Clear | Masked | Masked | Masked |
| Medical | Clear | Masked | Masked | Masked | Denied |

## Compilation

```bash
cd 81_DataMasking
lazbuild DataMasking.lpi
./DataMasking
```

## Sample Output

```
4. Role-Based Customer Data View
   ===============================

   As System Admin (role: admin) - FULL ACCESS:
   Name            | Email                     | Phone          | SSN
   John Doe        | john.doe@email.com        | 555-123-4567   | 123-45-6789

   As Support Agent (role: support) - phone visible, email/SSN masked:
   John Doe        | j******e@email.com        | 555-123-4567   | ***-**-6789

   As External Auditor (role: viewer) - ALL PII masked:
   J. D.           | j******e@email.com        | ***-***-4567   | ***-**-6789

8. Dynamic Masking by Access Level
   Customer #1 (John Doe) viewed by each role:
   [admin       ] john.doe@email.com   | 555-123-4567   | 123-45-6789 | 1985-03-15
   [data_officer] john.doe@email.com   | 555-123-4567   | ***-**-6789 | 1985-03-15
   [viewer      ] j******e@email.com   | ***-***-4567   | ***-**-6789 | ****-**-**
```

## Related Examples

- **80_RowLevelSecurity** - Row-level access control
- **75_UserAuthentication** - Identity and authentication
- **82_SecureDelete** - Secure data removal

## Compliance Considerations

1. **GDPR**: Right to erasure, data minimization, purpose limitation
2. **HIPAA**: Protected Health Information (PHI) de-identification
3. **PCI DSS**: Card data masking requirements (show max last 4)
4. **CCPA**: Consumer data access and deletion rights
5. **SOX**: Financial data access controls
6. **Data Residency**: Location-based masking rules
7. **Tokenization**: Replace sensitive data with tokens for storage
