# Example 55: Data Validation

This example demonstrates comprehensive data validation techniques using SQLite constraints and triggers with NDXSQLite.

## Features Demonstrated

- **CHECK Constraints**: Column-level validation
- **UNIQUE Constraints**: Prevent duplicates
- **Foreign Key Constraints**: Referential integrity
- **Trigger Validation**: Complex business rules
- **Status Transitions**: State machine validation
- **Dynamic Validation Rules**: Table-stored rules
- **Custom Error Messages**: RAISE(ABORT, message)

## Validation Types

### CHECK Constraints
```sql
CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  username TEXT NOT NULL,
  email TEXT NOT NULL,
  age INTEGER,
  status TEXT DEFAULT 'active',
  -- Length validation
  CHECK (length(username) >= 3),
  CHECK (length(username) <= 50),
  -- Character validation
  CHECK (username NOT GLOB '*[^a-zA-Z0-9_]*'),
  -- Format validation
  CHECK (email LIKE '%@%.%'),
  -- Range validation
  CHECK (age IS NULL OR (age >= 0 AND age <= 150)),
  -- Enum validation
  CHECK (status IN ('active', 'inactive', 'banned'))
);
```

### UNIQUE Constraints
```sql
CREATE TABLE products (
  id INTEGER PRIMARY KEY,
  sku TEXT NOT NULL UNIQUE,        -- Must be unique
  name TEXT NOT NULL,
  CHECK (length(sku) = 8)          -- Fixed length
);
```

### Foreign Key Constraints
```sql
PRAGMA foreign_keys = ON;

CREATE TABLE orders (
  id INTEGER PRIMARY KEY,
  user_id INTEGER NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users(id)
);
```

### Trigger Validation

#### Reserved Username Check
```sql
CREATE TRIGGER validate_username_reserved
BEFORE INSERT ON users
BEGIN
  SELECT CASE
    WHEN lower(NEW.username) IN ('admin', 'root', 'system') THEN
      RAISE(ABORT, 'This username is reserved')
  END;
END;
```

#### Email Domain Validation
```sql
CREATE TRIGGER validate_email_domain
BEFORE INSERT ON users
BEGIN
  SELECT CASE
    WHEN NEW.email LIKE '%@temp.%' OR NEW.email LIKE '%@fake.%' THEN
      RAISE(ABORT, 'Temporary or fake email domains are not allowed')
  END;
END;
```

#### Price Change Limit
```sql
CREATE TRIGGER validate_price_change
BEFORE UPDATE OF price ON products
WHEN NEW.price < OLD.price * 0.5
BEGIN
  SELECT RAISE(ABORT, 'Price cannot be reduced by more than 50%');
END;
```

#### Status Transition Validation
```sql
CREATE TRIGGER validate_order_status_transition
BEFORE UPDATE OF status ON orders
BEGIN
  SELECT CASE
    WHEN OLD.status = 'cancelled' AND NEW.status <> 'cancelled' THEN
      RAISE(ABORT, 'Cannot change status of cancelled order')
    WHEN OLD.status = 'delivered' AND NEW.status NOT IN ('delivered', 'cancelled') THEN
      RAISE(ABORT, 'Delivered order can only be cancelled')
  END;
END;
```

## Error Handling

```pascal
function TryInsert(const ASQL: string; const AParams: array of Variant): Boolean;
begin
  Result := True;
  try
    Connection.ExecuteNonQuery(ASQL, AParams);
  except
    on E: Exception do
    begin
      Result := False;
      WriteLn('Validation Error: ', E.Message);
    end;
  end;
end;
```

## Dynamic Validation Rules

Store validation rules in a table for runtime configuration:

```sql
CREATE TABLE validation_rules (
  id INTEGER PRIMARY KEY,
  table_name TEXT NOT NULL,
  field_name TEXT NOT NULL,
  rule_type TEXT NOT NULL,      -- 'regex', 'range', 'list'
  rule_value TEXT NOT NULL,
  error_message TEXT NOT NULL,
  is_active INTEGER DEFAULT 1
);

INSERT INTO validation_rules VALUES
  (1, 'users', 'username', 'regex', '^[a-z][a-z0-9_]{2,29}$', 'Invalid username format');
```

## Build and Run

```bash
cd 55_DataValidation
fpc DataValidation.lpr
./DataValidation
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use CHECK constraints for simple value validation
- Use UNIQUE constraints for business keys
- Enable `PRAGMA foreign_keys = ON` for referential integrity
- Use triggers for complex multi-field validation
- Use RAISE(ABORT, message) for custom error messages
- Validate on both INSERT and UPDATE
- Test edge cases: NULL, empty strings, boundary values
