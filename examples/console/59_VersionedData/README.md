# Example 59: Versioned Data (Temporal Tables)

This example demonstrates temporal data patterns for tracking data history and enabling time-travel queries with NDXSQLite.

## Features Demonstrated

- **Temporal Tables**: valid_from/valid_to timestamps
- **Version Tracking**: Automatic version numbering
- **Point-in-Time Queries**: Query data as it was at any date
- **Current Views**: Simplified access to latest versions
- **Change History**: Full audit trail of modifications
- **Bi-Temporal Data**: System time + business time tracking

## Database Schema

### Temporal History Table
```sql
CREATE TABLE products_history (
  id INTEGER NOT NULL,
  version INTEGER NOT NULL,
  name TEXT NOT NULL,
  price REAL NOT NULL,
  category TEXT,
  valid_from TEXT NOT NULL,    -- When this version became active
  valid_to TEXT,               -- When this version was superseded (NULL = current)
  is_current INTEGER DEFAULT 1,
  modified_by TEXT,
  PRIMARY KEY (id, version)
);

-- View for current data only
CREATE VIEW products AS
SELECT id, version, name, price, category, valid_from, modified_by
FROM products_history
WHERE is_current = 1;
```

### Bi-Temporal Table (System + Business Time)
```sql
CREATE TABLE contracts (
  id INTEGER NOT NULL,
  version INTEGER NOT NULL,
  customer_name TEXT NOT NULL,
  contract_value REAL NOT NULL,
  business_start TEXT NOT NULL,  -- When contract is effective
  business_end TEXT,
  system_start TEXT NOT NULL,    -- When we recorded this
  system_end TEXT,
  is_current INTEGER DEFAULT 1,
  PRIMARY KEY (id, version)
);
```

## Key Operations

### Insert or Update Versioned Record
```pascal
procedure InsertVersionedProduct(AId: Integer; const AName: string; APrice: Real;
  const ACategory, AValidFrom, AModifiedBy: string);
var
  CurrentVersion, NewVersion: Integer;
begin
  // Get current version
  CurrentVersion := Connection.ExecuteScalar(
    'SELECT COALESCE(MAX(version), 0) FROM products_history WHERE id = ?', [AId]);
  NewVersion := CurrentVersion + 1;

  // Mark old version as not current
  if CurrentVersion > 0 then
  begin
    Connection.ExecuteNonQuery(
      'UPDATE products_history SET is_current = 0, valid_to = ? ' +
      'WHERE id = ? AND is_current = 1', [AValidFrom, AId]);
  end;

  // Insert new version
  Connection.ExecuteNonQuery(
    'INSERT INTO products_history (id, version, name, price, category, valid_from, is_current, modified_by) ' +
    'VALUES (?, ?, ?, ?, ?, ?, 1, ?)',
    [AId, NewVersion, AName, APrice, ACategory, AValidFrom, AModifiedBy]);
end;
```

### Query Current Data
```sql
-- Using view (recommended)
SELECT * FROM products WHERE id = 1;

-- Direct query
SELECT * FROM products_history WHERE id = 1 AND is_current = 1;
```

### Point-in-Time Query
```sql
-- Products as they were on 2023-07-01
SELECT id, version, name, price, category
FROM products_history
WHERE valid_from <= '2023-07-01'
  AND (valid_to IS NULL OR valid_to > '2023-07-01')
ORDER BY id;
```

### Full Version History
```sql
SELECT version, name, price, valid_from,
       COALESCE(valid_to, '(current)') as valid_to,
       modified_by
FROM products_history
WHERE id = 1
ORDER BY version;
```

### Year-over-Year Comparison
```sql
WITH salary_2023 AS (
  SELECT id, name, salary FROM employees_history
  WHERE valid_from <= '2023-06-01' AND (valid_to IS NULL OR valid_to > '2023-06-01')
),
salary_2024 AS (
  SELECT id, name, salary FROM employees_history
  WHERE valid_from <= '2024-06-01' AND (valid_to IS NULL OR valid_to > '2024-06-01')
)
SELECT s24.name,
       s23.salary as salary_2023,
       s24.salary as salary_2024,
       ROUND((s24.salary - s23.salary) / s23.salary * 100, 1) as increase_pct
FROM salary_2024 s24
JOIN salary_2023 s23 ON s24.id = s23.id;
```

### Change Log / Audit Trail
```sql
SELECT p2.id, p2.version,
       COALESCE(p1.name, '(new)') as old_name, p2.name as new_name,
       COALESCE(p1.price, 0) as old_price, p2.price as new_price,
       p2.valid_from as changed_at, p2.modified_by
FROM products_history p2
LEFT JOIN products_history p1 ON p2.id = p1.id AND p2.version = p1.version + 1
ORDER BY p2.valid_from;
```

## Bi-Temporal Example

Bi-temporal tracking captures two timelines:
- **Business time**: When the fact is true in the real world
- **System time**: When we recorded the information

```
Contract v1: $50K (recorded 2023-01-01)
  Business: 2023-01-01 to 2023-12-31

Contract v2: $75K (recorded 2023-06-15, renegotiated)
  Business: 2023-01-01 to 2024-06-30
```

This allows answering:
- "What did we believe on 2023-03-01?" (system time query)
- "What was the contract value for Q2 2023?" (business time query)

## Build and Run

```bash
cd 59_VersionedData
fpc VersionedData.lpr
./VersionedData
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use `valid_from`/`valid_to` for temporal validity
- Keep `is_current` flag for efficient current data queries
- Create views for transparent current data access
- Index temporal columns (`valid_from`, `valid_to`, `is_current`)
- Use bi-temporal for compliance and audit requirements
- Consider retention policies for old versions
- Implement versioned insert/update as stored procedures
- Use CTEs for complex point-in-time queries
