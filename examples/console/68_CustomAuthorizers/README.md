# Example 68: Custom Authorizers

This example demonstrates SQLite authorizer callback mechanism for fine-grained SQL access control.

## What you'll learn

- How to enable/disable the authorizer
- How to use rule-based authorization (allow/deny patterns)
- How to create custom authorization callbacks
- How to log authorization decisions
- How to use convenience methods (DenyAllWrites, AllowReadOnly, etc.)

## Key concepts

### Basic Authorizer

```pascal
Auth := TNDXSQLiteAuthorizer.Create(Connection);
Auth.Enable;
// ... perform operations
Auth.Disable;
```

### Rule-Based Authorization

```pascal
// Deny all write operations
Auth.DenyAllWrites;

// Allow only read operations (deny by default)
Auth.AllowReadOnly;

// Deny DDL (CREATE, DROP, ALTER)
Auth.DenyDDL;

// Deny specific table access
Auth.DenyTableAccess('secret_table');

// Deny specific column access
Auth.DenyColumnAccess('employees', 'salary');

// Custom rules
Auth.AddDenyRule(aaInsert, 'audit_log');
Auth.AddAllowRule(aaSelect, '*');
Auth.AddIgnoreRule(aaRead, 'employees', 'ssn');
```

### Custom Callback

```pascal
type
  TMyHandler = class
    function HandleAuth(const Request: TNDXAuthRequest): TNDXAuthResult;
  end;

function TMyHandler.HandleAuth(const Request: TNDXAuthRequest): TNDXAuthResult;
begin
  // Custom logic
  if Request.Param1 = 'secret_table' then
    Result := arDeny
  else
    Result := arOK;
end;

// Use callback
Auth.SetCallback(@MyHandler.HandleAuth);
```

### Authorization Results

- `arOK` - Allow the action
- `arDeny` - Deny with error (operation aborts)
- `arIgnore` - Ignore (returns NULL for reads, no-op for writes)

### Logging

```pascal
Auth.EnableLogging;
Auth.Enable;
// ... operations
Auth.Disable;
LogList := Auth.GetLog;
for I := 0 to LogList.Count - 1 do
  WriteLn(LogList[I]);
```

## Authorization Actions

| Action | Description |
|--------|-------------|
| aaSelect | SELECT statement |
| aaInsert | INSERT statement |
| aaUpdate | UPDATE statement |
| aaDelete | DELETE statement |
| aaRead | Column read |
| aaCreateTable | CREATE TABLE |
| aaDropTable | DROP TABLE |
| aaCreateIndex | CREATE INDEX |
| aaDropIndex | DROP INDEX |
| aaCreateView | CREATE VIEW |
| aaDropView | DROP VIEW |
| aaCreateTrigger | CREATE TRIGGER |
| aaDropTrigger | DROP TRIGGER |
| aaAlterTable | ALTER TABLE |
| aaAttach | ATTACH DATABASE |
| aaDetach | DETACH DATABASE |
| aaPragma | PRAGMA statement |
| aaTransaction | Transaction control |
| aaFunction | Function call |

## Building

```bash
lazbuild CustomAuthorizers.lpi
```

## Running

```bash
./CustomAuthorizers      # Linux/macOS
CustomAuthorizers.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 68: Custom Authorizers ===

1. Basic Authorizer Enable/Disable:
   Enabled before: FALSE
   Enabled after Enable(): TRUE
   Enabled after Disable(): FALSE

2. Rule-Based Authorization - Deny All Writes:
   SELECT (allowed): OK - 2 employees
   INSERT (denied): BLOCKED as expected
   UPDATE (denied): BLOCKED as expected

3. Rule-Based Authorization - Allow Read Only:
   SELECT (allowed): OK - Engineering
   DROP TABLE (denied): BLOCKED as expected

4. Custom Callback Authorization:
   SELECT from employees: OK - Alice
   SELECT from secret_table: BLOCKED as expected

5. Authorization Logging:
   Log entries recorded: 15
   [10:30:45.123] SELECT: employees. -> OK
   ...

=== Example completed successfully! ===
```

## Use cases

- Multi-tenant applications (isolate tenant data)
- Read-only connections
- Audit logging
- Column-level security
- Preventing DDL in production
- Sandboxed SQL execution

## Cross-Platform

This example works on Windows, Linux, and macOS.
