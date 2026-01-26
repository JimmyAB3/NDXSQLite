# NDXSQLite Security Guide

This document covers security best practices for NDXSQLite, with a focus on SQLCipher encryption, key management, and threat mitigation.

---

## Table of Contents

1. [Overview](#overview)
2. [SQLCipher Encryption](#sqlcipher-encryption)
3. [Key Management](#key-management)
4. [Threat Model](#threat-model)
5. [SQL Injection Prevention](#sql-injection-prevention)
6. [Access Control](#access-control)
7. [Secure Configuration](#secure-configuration)
8. [Audit and Logging](#audit-and-logging)

---

## Overview

NDXSQLite provides multiple security layers:

| Layer | Feature | Description |
|-------|---------|-------------|
| Encryption | SQLCipher | AES-256 database encryption |
| Access Control | Authorizer | Fine-grained SQL permission control |
| Input Validation | Parameters | SQL injection prevention |
| Audit | Hooks | Change tracking and logging |

### Security Architecture

```
┌───────────────────────────────────────────────────────────────────┐
│                        APPLICATION                                │
├───────────────────────────────────────────────────────────────────┤
│                    NDXSQLite Security L ayer                      │
│  ┌───────────────┐ ┌─────────────┐ ┌─────────────┐ ┌────────────┐ │
│  │ Parameterized │ │  Authorizer │ │   Audit     │ │   Access   │ │
│  │   Queries     │ │  Callbacks  │ │   Hooks     │ │   Control  │ │
│  └───────────────┘ └─────────────┘ └─────────────┘ └────────────┘ │
├───────────────────────────────────────────────────────────────────┤
│                    SQLCipher Encryption                           │
│                      (AES-256-CBC)                                │
├───────────────────────────────────────────────────────────────────┤
│                    Operating System                               │
│                  (File Permissions)                               │
└───────────────────────────────────────────────────────────────────┘
```

---

## SQLCipher Encryption

### What is SQLCipher?

SQLCipher is an open-source extension to SQLite that provides transparent 256-bit AES encryption of database files.

**Features:**
- AES-256 encryption in CBC mode
- PBKDF2 key derivation
- Per-page HMAC authentication
- Automatic encryption/decryption

### Enabling Encryption

```pascal
var
  Options: TNDXSQLiteConnectionOptions;
  Conn: TNDXSQLiteConnection;
begin
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := 'secure.db';
    Options.EncryptionKey := 'your-secret-key-here';  // MUST set before Open
    Options.IsPrimaryConnection := True;

    Conn := TNDXSQLiteConnection.Create(Options);
    try
      Conn.Open;  // Database is now encrypted

      // All operations are encrypted transparently
      Conn.ExecuteNonQuery('CREATE TABLE secrets (data TEXT)');

      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Options.Free;
  end;
end;
```

### Checking SQLCipher Availability

```pascal
if TNDXSQLiteConnection.IsSQLCipherAvailable then
  WriteLn('SQLCipher is available')
else
  WriteLn('WARNING: SQLCipher not found - encryption unavailable');
```

### Changing Encryption Key

```pascal
// Change to a new key
Conn.ChangeEncryptionKey('new-secret-key');

// Remove encryption (convert to plain SQLite)
Conn.ChangeEncryptionKey('');
```

### Key Rotation Strategy

Rotate encryption keys periodically:

```pascal
procedure RotateEncryptionKey(const DatabasePath, OldKey, NewKey: String);
var
  Options: TNDXSQLiteConnectionOptions;
  Conn: TNDXSQLiteConnection;
begin
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DatabasePath;
    Options.EncryptionKey := OldKey;

    Conn := TNDXSQLiteConnection.Create(Options);
    try
      Conn.Open;

      // Verify we can access the database
      Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_master');

      // Change to new key
      Conn.ChangeEncryptionKey(NewKey);

      WriteLn('Key rotation successful');
    finally
      Conn.Free;
    end;
  finally
    Options.Free;
  end;
end;
```

---

## Key Management

### Best Practices

| Practice | Description |
|----------|-------------|
| Never hardcode keys | Store keys externally |
| Use strong keys | Minimum 32 characters, random |
| Rotate regularly | Monthly or quarterly |
| Secure transmission | Never log or display keys |
| Backup keys separately | Don't store key with database |

### Key Storage Options

#### 1. Environment Variables

```pascal
var
  Key: String;
begin
  Key := GetEnvironmentVariable('MYAPP_DB_KEY');
  if Key = '' then
    raise Exception.Create('Database encryption key not configured');

  Options.EncryptionKey := Key;
end;
```

#### 2. Configuration File (Encrypted)

```pascal
// Store key in encrypted config file
// Use OS keychain or separate encryption for config
Options.EncryptionKey := LoadKeyFromSecureConfig('database.key');
```

#### 3. Hardware Security Module (HSM)

For enterprise deployments, use HSM or key management service.

#### 4. User-Provided at Runtime

```pascal
procedure OpenSecureDatabase(const Password: String);
begin
  // Derive key from password using PBKDF2 or similar
  Options.EncryptionKey := DeriveKey(Password, Salt, Iterations);
end;
```

### Key Derivation

For password-based keys, use proper key derivation:

```pascal
function DeriveKey(const Password, Salt: String; Iterations: Integer): String;
var
  Hash: TSHA256;
  Key: array[0..31] of Byte;
begin
  // Use PBKDF2 or Argon2 for production
  // This is a simplified example
  PBKDF2_SHA256(Password, Salt, Iterations, Key, SizeOf(Key));
  Result := BytesToHex(Key);
end;
```

### What NOT to Do

```pascal
// WRONG: Hardcoded key
Options.EncryptionKey := 'my-secret-key';

// WRONG: Weak key
Options.EncryptionKey := '123456';

// WRONG: Logging key
WriteLn('Using key: ' + Options.EncryptionKey);

// WRONG: Storing key in database
Conn.ExecuteNonQuery('INSERT INTO config VALUES (''key'', ?)' , [Key]);
```

---

## Threat Model

### Assets to Protect

| Asset | Description | Impact |
|-------|-------------|--------|
| Database file | SQLite database on disk | Data breach |
| Encryption key | SQLCipher key | Complete compromise |
| Connection strings | Database path, credentials | Unauthorized access |
| Query logs | SQL statements | Information leakage |

### Threats and Mitigations

#### 1. Disk Theft / Lost Device

**Threat:** Physical access to database file

**Mitigation:**
- Enable SQLCipher encryption
- Use strong encryption key
- Secure key storage separate from device

```pascal
// Always use encryption for sensitive data
Options.EncryptionKey := GetSecureKey;
```

#### 2. SQL Injection

**Threat:** Malicious SQL through user input

**Mitigation:**
- Always use parameterized queries
- Never concatenate user input into SQL

```pascal
// SECURE: Parameterized query
Conn.ExecuteNonQuery('SELECT * FROM users WHERE id = ?', [UserId]);

// VULNERABLE: String concatenation
Conn.ExecuteNonQuery('SELECT * FROM users WHERE id = ' + UserId);  // NEVER DO THIS
```

#### 3. Unauthorized Database Access

**Threat:** Unauthorized users accessing database

**Mitigation:**
- Use authorizer callbacks
- Implement application-level access control
- Set proper file permissions

```pascal
// Restrict to read-only for certain operations
procedure AuthorizerCallback(Action: Integer; var Allow: Boolean);
begin
  case Action of
    SQLITE_DELETE, SQLITE_UPDATE, SQLITE_INSERT:
      Allow := CurrentUserHasWritePermission;
    else
      Allow := True;
  end;
end;
```

#### 4. Information Leakage

**Threat:** Sensitive data in logs or error messages

**Mitigation:**
- Disable action logging for sensitive operations
- Sanitize error messages
- Secure log storage

```pascal
// Disable logging for sensitive operations
Options.EnableActionLog := False;
```

#### 5. Memory Attacks

**Threat:** Keys or data in memory

**Mitigation:**
- Clear sensitive variables after use
- Use secure memory allocation where available

```pascal
try
  // Use key
  Conn.Open;
finally
  // Clear key from memory
  FillChar(Key[1], Length(Key), 0);
  Key := '';
end;
```

#### 6. Backup Exposure

**Threat:** Unencrypted backups

**Mitigation:**
- Backup encrypted database (not decrypted)
- Encrypt backup files separately
- Secure backup storage

```pascal
// Backup preserves encryption
Backup := TNDXSQLiteBackup.Create(Conn, 'backup.db');
try
  Backup.Execute;  // Backup is also encrypted
finally
  Backup.Free;
end;
```

---

## SQL Injection Prevention

### Always Use Parameters

```pascal
// CORRECT: Parameters
Conn.ExecuteNonQuery(
  'INSERT INTO users (name, email) VALUES (?, ?)',
  [UserName, Email]);

// CORRECT: Named parameters (if supported)
Conn.ExecuteNonQuery(
  'INSERT INTO users (name, email) VALUES (:name, :email)',
  [UserName, Email]);
```

### Never Concatenate User Input

```pascal
// DANGEROUS - SQL INJECTION VULNERABILITY
Conn.ExecuteNonQuery('SELECT * FROM users WHERE name = ''' + UserInput + '''');

// If UserInput = "'; DROP TABLE users; --" the query becomes:
// SELECT * FROM users WHERE name = ''; DROP TABLE users; --'
```

### Validate Input When Necessary

```pascal
// For dynamic table/column names that can't be parameterized
function SafeIdentifier(const Name: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Name) do
    if Name[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
      Result := Result + Name[I];

  if Result = '' then
    raise Exception.Create('Invalid identifier');
end;

// Usage
TableName := SafeIdentifier(UserProvidedTableName);
Conn.ExecuteQuery('SELECT * FROM ' + TableName);
```

---

## Access Control

### Using the Authorizer

The authorizer allows fine-grained SQL access control:

```pascal
uses ndxsqliteauthorizer;

var
  Auth: TNDXSQLiteAuthorizer;
begin
  Auth := TNDXSQLiteAuthorizer.Create(Conn);
  try
    Auth.OnAuthorize := @MyAuthorizerCallback;
    Auth.Enable;

    // Now all SQL is filtered through the authorizer
    Conn.ExecuteQuery('SELECT * FROM users');  // Checked by authorizer

  finally
    Auth.Free;
  end;
end;

function MyAuthorizerCallback(Action: TNDXAuthorizerAction;
  const Arg1, Arg2, Arg3, Arg4: String): TNDXAuthorizerResult;
begin
  // Example: Block all DELETE operations
  if Action = aaDelete then
    Result := arDeny
  else
    Result := arOK;
end;
```

### Authorizer Actions

| Action | Description |
|--------|-------------|
| `aaSelect` | SELECT statement |
| `aaInsert` | INSERT statement |
| `aaUpdate` | UPDATE statement |
| `aaDelete` | DELETE statement |
| `aaCreateTable` | CREATE TABLE |
| `aaDropTable` | DROP TABLE |
| `aaCreateIndex` | CREATE INDEX |
| `aaDropIndex` | DROP INDEX |
| `aaPragma` | PRAGMA statement |
| `aaAttach` | ATTACH DATABASE |
| `aaDetach` | DETACH DATABASE |

### Row-Level Security

Implement row-level security in your application:

```pascal
// Filter queries by user context
function GetUserData(UserId: Integer): TNDXSQLiteDataSet;
begin
  // Always include user filter
  Result := Conn.ExecuteQuery(
    'SELECT * FROM data WHERE owner_id = ?',
    [UserId]);
end;
```

---

## Secure Configuration

### Recommended Settings

```pascal
Options := TNDXSQLiteConnectionOptions.Create;

// Enable encryption
Options.EncryptionKey := GetSecureKey;

// Use WAL for better concurrency (also more secure against corruption)
Options.JournalMode := jmWAL;

// Enable foreign keys for data integrity
Options.ForeignKeys := True;

// Set busy timeout to prevent denial of service
Options.BusyTimeout := 5000;  // 5 seconds

// Disable if not needed
Options.EnableActionLog := False;  // Don't log sensitive queries
```

### File Permissions

```bash
# Linux/macOS: Restrict database file permissions
chmod 600 myapp.db
chown myapp:myapp myapp.db

# Directory permissions
chmod 700 /path/to/database/directory
```

### Read-Only Connections

Use read-only connections when writes aren't needed:

```pascal
Options.ReadOnly := True;  // Prevents accidental writes
```

---

## Audit and Logging

### Pre-Update Hooks

Track all changes to sensitive tables:

```pascal
uses ndxsqlitepreupdate;

var
  PreUpdate: TNDXSQLitePreUpdate;
begin
  PreUpdate := TNDXSQLitePreUpdate.Create(Conn);
  try
    PreUpdate.OnChange := @AuditChange;
    PreUpdate.Enable;

    // Changes are now tracked
  finally
    PreUpdate.Free;
  end;
end;

procedure AuditChange(const TableName: String; Action: TNDXPreUpdateAction;
  const OldValues, NewValues: array of Variant);
begin
  // Log to audit table or external system
  LogAudit(TableName, Action, OldValues, NewValues);
end;
```

### Action Logging

Built-in action logging (use with caution for sensitive data):

```pascal
Options.EnableActionLog := True;

// After operations
for I := 0 to Conn.ActionLog.Count - 1 do
  WriteLn(Conn.ActionLog[I]);
```

### Secure Audit Storage

```pascal
// Store audit logs in separate encrypted database
AuditOptions := TNDXSQLiteConnectionOptions.Create;
AuditOptions.DatabasePath := 'audit.db';
AuditOptions.EncryptionKey := GetAuditKey;

AuditConn := TNDXSQLiteConnection.Create(AuditOptions);
AuditConn.ExecuteNonQuery(
  'INSERT INTO audit_log (timestamp, action, details) VALUES (?, ?, ?)',
  [Now, ActionType, Details]);
```

---

## Security Checklist

### Development

- [ ] Use parameterized queries for all user input
- [ ] Enable SQLCipher for sensitive data
- [ ] Implement proper key management
- [ ] Use authorizer for access control
- [ ] Set appropriate file permissions
- [ ] Disable unnecessary logging

### Deployment

- [ ] Verify SQLCipher is installed and working
- [ ] Secure key storage configured
- [ ] File permissions set correctly
- [ ] Backup encryption enabled
- [ ] Audit logging configured
- [ ] Error messages sanitized

### Monitoring

- [ ] Monitor for unauthorized access attempts
- [ ] Track encryption key usage
- [ ] Review audit logs regularly
- [ ] Test backup restoration periodically

---

## Further Reading

- [SQLCipher Documentation](https://www.zetetic.net/sqlcipher/documentation/)
- [OWASP SQL Injection Prevention](https://cheatsheetseries.owasp.org/cheatsheets/SQL_Injection_Prevention_Cheat_Sheet.html)
- [SQLite Security](https://www.sqlite.org/security.html)

---

*NDXSQLite Security Guide - Version 1.0.0*
