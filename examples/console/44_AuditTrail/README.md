# Example 44: Audit Trail Database

This example demonstrates complete audit logging with SQLite, including before/after values, user tracking, and tamper detection.

## What you'll learn

- Automatic change tracking with triggers
- Before/after value capture
- User and session context tracking
- Tamper detection with checksums
- Compliance reporting (GDPR, SOX)
- Point-in-time state reconstruction

## Schema design

### Audit trail table

```sql
CREATE TABLE audit_trail (
  id INTEGER PRIMARY KEY,
  table_name TEXT NOT NULL,
  record_id TEXT NOT NULL,
  action TEXT NOT NULL,          -- INSERT, UPDATE, DELETE
  old_values TEXT,               -- JSON (NULL for INSERT)
  new_values TEXT,               -- JSON (NULL for DELETE)
  changed_fields TEXT,           -- JSON array of changed fields
  user_id TEXT,
  session_id TEXT,
  ip_address TEXT,
  user_agent TEXT,
  timestamp TEXT DEFAULT (datetime('now')),
  checksum TEXT                  -- For tamper detection
);

-- Indexes for efficient queries
CREATE INDEX idx_audit_table ON audit_trail(table_name);
CREATE INDEX idx_audit_record ON audit_trail(table_name, record_id);
CREATE INDEX idx_audit_user ON audit_trail(user_id);
CREATE INDEX idx_audit_timestamp ON audit_trail(timestamp);
```

### Audit context (for triggers)

```sql
CREATE TABLE audit_context (
  key TEXT PRIMARY KEY,
  value TEXT
);
```

## Automatic triggers

### INSERT trigger

```sql
CREATE TRIGGER audit_customers_insert
AFTER INSERT ON customers
BEGIN
  INSERT INTO audit_trail (table_name, record_id, action, new_values, user_id)
  VALUES (
    'customers',
    NEW.id,
    'INSERT',
    json_object('id', NEW.id, 'name', NEW.name, 'email', NEW.email),
    (SELECT value FROM audit_context WHERE key = 'user_id')
  );
END;
```

### UPDATE trigger with change detection

```sql
CREATE TRIGGER audit_customers_update
AFTER UPDATE ON customers
BEGIN
  INSERT INTO audit_trail (table_name, record_id, action, old_values, new_values, changed_fields, user_id)
  VALUES (
    'customers',
    NEW.id,
    'UPDATE',
    json_object('id', OLD.id, 'name', OLD.name, 'email', OLD.email),
    json_object('id', NEW.id, 'name', NEW.name, 'email', NEW.email),
    json_array(
      CASE WHEN OLD.name != NEW.name THEN 'name' ELSE NULL END,
      CASE WHEN OLD.email != NEW.email THEN 'email' ELSE NULL END
    ),
    (SELECT value FROM audit_context WHERE key = 'user_id')
  );
END;
```

### DELETE trigger

```sql
CREATE TRIGGER audit_customers_delete
AFTER DELETE ON customers
BEGIN
  INSERT INTO audit_trail (table_name, record_id, action, old_values, user_id)
  VALUES (
    'customers',
    OLD.id,
    'DELETE',
    json_object('id', OLD.id, 'name', OLD.name, 'email', OLD.email),
    (SELECT value FROM audit_context WHERE key = 'user_id')
  );
END;
```

## Setting audit context

```pascal
procedure SetAuditContext(const AUserId, ASessionId, AIpAddress: string);
begin
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO audit_context (key, value) VALUES (''user_id'', ?)',
    [AUserId]);
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO audit_context (key, value) VALUES (''session_id'', ?)',
    [ASessionId]);
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO audit_context (key, value) VALUES (''ip_address'', ?)',
    [AIpAddress]);
end;
```

## Query examples

### Record history

```sql
SELECT timestamp, action, old_values, new_values, user_id
FROM audit_trail
WHERE table_name = 'customers' AND record_id = '1'
ORDER BY timestamp;
```

### User activity

```sql
SELECT user_id, COUNT(*) as actions,
       COUNT(DISTINCT table_name) as tables_modified
FROM audit_trail
GROUP BY user_id
ORDER BY actions DESC;
```

### Sensitive field changes

```sql
SELECT timestamp, record_id,
       JSON_EXTRACT(old_values, '$.credit_limit') as old_limit,
       JSON_EXTRACT(new_values, '$.credit_limit') as new_limit,
       user_id
FROM audit_trail
WHERE table_name = 'customers'
  AND action = 'UPDATE'
  AND JSON_EXTRACT(old_values, '$.credit_limit') !=
      JSON_EXTRACT(new_values, '$.credit_limit');
```

## Tamper detection

Add checksums to audit records:

```pascal
function CalculateChecksum(const AData: string): string;
begin
  // Use SHA-256 in production
  Result := SHA256Hash(AData);
end;

// Verify integrity
AuditData := TableName + RecordId + Action + OldValues + NewValues + Timestamp;
StoredChecksum := GetStoredChecksum(AuditId);
ComputedChecksum := CalculateChecksum(AuditData);

if StoredChecksum <> ComputedChecksum then
  RaiseAlert('Tampering detected!');
```

## Compliance reporting

### GDPR Article 15 (Data Access)

```sql
SELECT table_name, action, COUNT(*) as count,
       GROUP_CONCAT(DISTINCT user_id) as users
FROM audit_trail
WHERE record_id = ?  -- Customer ID
GROUP BY table_name, action;
```

### SOX Financial Changes

```sql
SELECT timestamp, user_id, table_name,
       JSON_EXTRACT(old_values, '$.amount') as old_amount,
       JSON_EXTRACT(new_values, '$.amount') as new_amount
FROM audit_trail
WHERE table_name IN ('invoices', 'payments', 'credits')
ORDER BY timestamp;
```

## Point-in-time reconstruction

```sql
-- Get state of record at specific time
SELECT new_values
FROM audit_trail
WHERE table_name = 'customers'
  AND record_id = '1'
  AND timestamp <= '2024-01-15 12:00:00'
ORDER BY timestamp DESC
LIMIT 1;
```

## Best practices

1. **Always set context** before operations
2. **Index heavily** - audit tables grow large
3. **Archive old records** periodically
4. **Use checksums** for tamper detection
5. **Never delete** audit records (soft-archive instead)
6. **Separate storage** for audit tables in high-volume systems

## Building

```bash
lazbuild AuditTrail.lpi
```

## Running

```bash
./AuditTrail      # Linux/macOS
AuditTrail.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
