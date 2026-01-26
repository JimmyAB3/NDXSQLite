# NDXSQLite Troubleshooting Guide

This guide helps diagnose and resolve common issues with NDXSQLite.

---

## Table of Contents

1. [Connection Issues](#connection-issues)
2. [Library Loading](#library-loading)
3. [Database Errors](#database-errors)
4. [Performance Issues](#performance-issues)
5. [Encryption Issues](#encryption-issues)
6. [Platform-Specific Issues](#platform-specific-issues)
7. [Debugging Techniques](#debugging-techniques)

---

## Connection Issues

### "Database file not found"

**Symptoms:** Exception when opening connection

**Causes:**
- Incorrect path
- Missing directory
- Permission issues

**Solutions:**

```pascal
// Check path exists before connecting
if not FileExists(Options.DatabasePath) then
  WriteLn('Database does not exist: ', Options.DatabasePath);

// Use absolute path
Options.DatabasePath := ExpandFileName('myapp.db');

// Create directory if needed
ForceDirectories(ExtractFilePath(Options.DatabasePath));
```

### "Database is locked"

**Symptoms:** `SQLITE_BUSY` error, operations timeout

**Causes:**
- Another connection has exclusive lock
- Transaction left open
- WAL mode not enabled for concurrent access

**Solutions:**

```pascal
// Enable WAL mode for concurrent access
Options.JournalMode := jmWAL;

// Increase busy timeout
Options.BusyTimeout := 10000;  // 10 seconds

// Ensure transactions are closed
try
  Conn.BeginTransaction;
  // ... operations ...
  Conn.Commit;
except
  Conn.Rollback;  // Always rollback on error
  raise;
end;
```

### "Connection is not open"

**Symptoms:** Operations fail with closed connection

**Solutions:**

```pascal
// Check state before operations
if not Conn.IsOpen then
  Conn.Open;

// Use reconnect for dropped connections
Conn.Reconnect;

// Check connection state
case Conn.State of
  csDisconnected: WriteLn('Not connected');
  csConnecting: WriteLn('Connecting...');
  csConnected: WriteLn('Connected');
  csExecuting: WriteLn('Executing query');
end;
```

### "Too many open connections"

**Symptoms:** Resource exhaustion, memory issues

**Solutions:**

```pascal
// Always free connections
Conn := TNDXSQLiteConnection.Create(Options);
try
  Conn.Open;
  // ... operations ...
finally
  Conn.Free;  // Essential!
end;

// Use connection pooling
Pool := TNDXSQLiteConnectionPool.Create(Options, 2, 10);
try
  Conn := Pool.Acquire;
  try
    // ... operations ...
  finally
    Pool.Release(Conn);  // Return to pool, don't free
  end;
finally
  Pool.Free;
end;
```

---

## Library Loading

### "SQLite library not found"

**Symptoms:** Exception on first connection attempt

**Diagnosis:**

```pascal
// Check library availability
WriteLn('SQLite available: ', TNDXSQLiteConnection.IsSQLiteAvailable);
WriteLn('SQLCipher available: ', TNDXSQLiteConnection.IsSQLCipherAvailable);
```

**Solutions by Platform:**

**Linux:**
```bash
# Check if installed
ldconfig -p | grep sqlite

# Install
sudo apt-get install libsqlite3-0 libsqlite3-dev  # Debian/Ubuntu
sudo dnf install sqlite sqlite-devel               # Fedora/RHEL

# Check library path
ls -la /usr/lib/x86_64-linux-gnu/libsqlite3.so*
```

**Windows:**
```bash
# Download from sqlite.org
# Place sqlite3.dll in:
# - Application directory (recommended)
# - C:\Windows\System32
# - Directory in PATH
```

**macOS:**
```bash
# SQLite included with macOS
ls -la /usr/lib/libsqlite3.dylib

# Or install via Homebrew
brew install sqlite
```

### "Wrong library version"

**Symptoms:** Missing functions, unexpected behavior

**Diagnosis:**

```pascal
// Check version
WriteLn('SQLite version: ', TNDXSQLiteConnection.GetSQLiteVersion);
```

**Solutions:**
- Update SQLite to 3.7.0+ for basic features
- Update to 3.24.0+ for UPSERT
- Update to 3.35.0+ for RETURNING clause

### Snap/Flatpak Library Issues

**Symptoms:** Library not found in sandboxed environments

**Solutions:**

```pascal
// NDXSQLite auto-detects Snap/Flatpak environments
// If issues persist, check paths:

// Snap
WriteLn('Snap: ', GetEnvironmentVariable('SNAP'));
// Library at: $SNAP/usr/lib/x86_64-linux-gnu/libsqlite3.so

// Flatpak
WriteLn('Flatpak: ', GetEnvironmentVariable('FLATPAK_ID'));
// Library at: /app/lib/libsqlite3.so or /usr/lib/libsqlite3.so
```

---

## Database Errors

### "File is not a database"

**Symptoms:** `SQLITE_NOTADB` error

**Causes:**
- File is corrupted
- File is encrypted (wrong or missing key)
- Not a SQLite database

**Solutions:**

```pascal
// For encrypted databases, ensure key is set
Options.EncryptionKey := 'your-key';  // Before Open!

// Verify file is SQLite
var
  F: File;
  Header: array[0..15] of Byte;
begin
  AssignFile(F, DatabasePath);
  Reset(F, 1);
  BlockRead(F, Header, 16);
  CloseFile(F);

  // SQLite header: "SQLite format 3"
  if (Header[0] = $53) and (Header[1] = $51) and (Header[2] = $4C) then
    WriteLn('Valid SQLite database')
  else
    WriteLn('Not a SQLite database or encrypted');
end;
```

### "Database disk image is malformed"

**Symptoms:** `SQLITE_CORRUPT` error

**Solutions:**

```pascal
// Run integrity check
var
  Result: String;
begin
  Result := Conn.ExecuteScalar('PRAGMA integrity_check');
  WriteLn('Integrity: ', Result);
end;

// Attempt recovery
// 1. Export data to SQL
Dump := TNDXSQLiteDump.Create(Conn);
try
  Dump.SaveToFile('backup.sql');
finally
  Dump.Free;
end;

// 2. Create new database and import
// sqlite3 new.db < backup.sql
```

### "SQL syntax error"

**Symptoms:** `SQLITE_ERROR` with syntax message

**Diagnosis:**

```pascal
try
  Conn.ExecuteNonQuery(SQL);
except
  on E: ENDXSQLiteException do
  begin
    WriteLn('Error: ', E.Message);
    WriteLn('SQL: ', SQL);
    WriteLn('Error code: ', E.ErrorCode);
  end;
end;
```

**Common Syntax Issues:**
- Missing quotes around strings
- Reserved word used as identifier
- Mismatched parentheses
- Missing commas in INSERT values

```pascal
// WRONG
Conn.ExecuteNonQuery('INSERT INTO users (name) VALUES (John)');

// CORRECT
Conn.ExecuteNonQuery('INSERT INTO users (name) VALUES (?)', ['John']);
```

### "Constraint violation"

**Symptoms:** `SQLITE_CONSTRAINT` error

**Types:**

| Constraint | Error | Solution |
|------------|-------|----------|
| UNIQUE | Duplicate value | Check before insert or use UPSERT |
| NOT NULL | NULL in required column | Provide value |
| FOREIGN KEY | Invalid reference | Ensure parent exists |
| CHECK | Value fails check | Validate input |
| PRIMARY KEY | Duplicate key | Use auto-increment or check |

```pascal
// Handle constraint violations
try
  Conn.ExecuteNonQuery('INSERT INTO users (email) VALUES (?)', [Email]);
except
  on E: ENDXSQLiteException do
    if Pos('UNIQUE constraint', E.Message) > 0 then
      WriteLn('Email already exists')
    else
      raise;
end;

// Or use UPSERT
Conn.ExecuteNonQuery(
  'INSERT INTO users (email, name) VALUES (?, ?) ' +
  'ON CONFLICT(email) DO UPDATE SET name = excluded.name',
  [Email, Name]);
```

---

## Performance Issues

### Slow Inserts

**Diagnosis:**

```pascal
var
  StartTime: TDateTime;
begin
  StartTime := Now;
  // Insert operations
  WriteLn('Time: ', MilliSecondsBetween(Now, StartTime), 'ms');
end;
```

**Solutions:**

```pascal
// 1. Use transactions
Conn.BeginTransaction;
try
  for I := 1 to 10000 do
    Conn.ExecuteNonQuery('INSERT INTO data (value) VALUES (?)', [I]);
  Conn.Commit;
except
  Conn.Rollback;
  raise;
end;

// 2. Enable WAL
Options.JournalMode := jmWAL;

// 3. Reduce sync
Options.SyncMode := smNormal;
```

### Slow Queries

**Diagnosis:**

```pascal
// Check query plan
var
  Plan: String;
begin
  Plan := Conn.ExecuteScalar('EXPLAIN QUERY PLAN ' + YourQuery);
  WriteLn(Plan);
  // Look for "SCAN" (slow) vs "SEARCH" (indexed)
end;
```

**Solutions:**

```pascal
// 1. Add index
Conn.ExecuteNonQuery('CREATE INDEX idx_users_email ON users(email)');

// 2. Analyze tables
Conn.ExecuteNonQuery('ANALYZE');

// 3. Increase cache
Options.CacheSize := 10000;

// 4. Optimize query
// Before: SELECT * FROM users WHERE UPPER(name) = 'JOHN'
// After:  SELECT * FROM users WHERE name = 'John' COLLATE NOCASE
```

### Memory Issues

**Diagnosis:**

```pascal
// Check memory usage
WriteLn('SQLite memory used: ', sqlite3_memory_used);
WriteLn('SQLite memory highwater: ', sqlite3_memory_highwater(0));
```

**Solutions:**

```pascal
// 1. Reduce cache size
Options.CacheSize := 1000;

// 2. Free results promptly
DataSet := Conn.ExecuteQuery('SELECT * FROM large_table');
try
  // Process
finally
  DataSet.Free;
end;

// 3. Use pagination
Conn.ExecuteQuery('SELECT * FROM large_table LIMIT ? OFFSET ?', [100, 0]);

// 4. Close unused connections
Conn.Close;
```

---

## Encryption Issues

### SQLCipher Not Available

**Symptoms:** Encryption key ignored, database not encrypted

**Diagnosis:**

```pascal
if not TNDXSQLiteConnection.IsSQLCipherAvailable then
begin
  WriteLn('SQLCipher not installed');
  WriteLn('Database will NOT be encrypted');
end;
```

**Solutions:**

**Linux:**
```bash
sudo apt-get install libsqlcipher1 libsqlcipher-dev
```

**macOS:**
```bash
brew install sqlcipher
```

**Windows:**
Download from SQLCipher releases or build from source.

### Wrong Encryption Key

**Symptoms:** "File is not a database" error

**Solutions:**

```pascal
// Key must be set BEFORE opening
Options.EncryptionKey := 'correct-key';
Conn := TNDXSQLiteConnection.Create(Options);
Conn.Open;  // Key applied here

// Verify key works
try
  Conn.ExecuteScalar('SELECT COUNT(*) FROM sqlite_master');
  WriteLn('Key is correct');
except
  WriteLn('Wrong key or not encrypted');
end;
```

### Key Change Failed

**Solutions:**

```pascal
// Ensure database is open with correct current key
Options.EncryptionKey := 'current-key';
Conn.Open;

// Then change key
Conn.ChangeEncryptionKey('new-key');

// Verify
Conn.Close;
Options.EncryptionKey := 'new-key';
Conn.Open;
Conn.ExecuteScalar('SELECT 1');  // Test access
```

---

## Platform-Specific Issues

### Linux

**Library Path Issues:**
```bash
# Check library paths
ldconfig -p | grep sqlite

# Add custom path
export LD_LIBRARY_PATH=/custom/path:$LD_LIBRARY_PATH
```

**Permission Issues:**
```bash
# Check file permissions
ls -la myapp.db

# Fix permissions
chmod 644 myapp.db
chown myuser:mygroup myapp.db
```

### Windows

**DLL Not Found:**
- Place `sqlite3.dll` in application directory
- Or add to `C:\Windows\System32`
- Or add directory to PATH

**32/64-bit Mismatch:**
- Use 32-bit DLL for 32-bit app
- Use 64-bit DLL for 64-bit app

### macOS

**Code Signing Issues:**
```bash
# Remove quarantine attribute
xattr -d com.apple.quarantine libsqlite3.dylib
```

**Homebrew Library Path:**
```bash
# Intel Mac
/usr/local/opt/sqlite/lib/libsqlite3.dylib

# Apple Silicon
/opt/homebrew/opt/sqlite/lib/libsqlite3.dylib
```

---

## Debugging Techniques

### Enable Action Logging

```pascal
Options.EnableActionLog := True;

// After operations
for I := 0 to Conn.ActionLog.Count - 1 do
  WriteLn(Conn.ActionLog[I]);
```

### Verbose Error Information

```pascal
try
  Conn.ExecuteNonQuery(SQL);
except
  on E: ENDXSQLiteException do
  begin
    WriteLn('Message: ', E.Message);
    WriteLn('SQL: ', SQL);
    WriteLn('Error code: ', E.ErrorCode);
    WriteLn('Extended code: ', E.ExtendedErrorCode);
    WriteLn('Stack trace: ', E.StackTrace);
  end;
end;
```

### Database State Inspection

```pascal
// List all tables
var
  Tables: TNDXSQLiteDataSet;
begin
  Tables := Conn.ExecuteQuery(
    'SELECT name FROM sqlite_master WHERE type = ''table''');
  try
    while not Tables.EOF do
    begin
      WriteLn('Table: ', Tables.FieldByName('name').AsString);
      Tables.Next;
    end;
  finally
    Tables.Free;
  end;
end;

// Check database info
WriteLn('Page size: ', Conn.ExecuteScalar('PRAGMA page_size'));
WriteLn('Page count: ', Conn.ExecuteScalar('PRAGMA page_count'));
WriteLn('Journal mode: ', Conn.ExecuteScalar('PRAGMA journal_mode'));
WriteLn('Foreign keys: ', Conn.ExecuteScalar('PRAGMA foreign_keys'));
```

### Health Check

```pascal
uses ndxsqlitehealthcheck;

var
  Health: TNDXSQLiteHealthCheck;
begin
  Health := TNDXSQLiteHealthCheck.Create(Conn);
  try
    if Health.IntegrityCheck then
      WriteLn('Database is healthy')
    else
      WriteLn('Database has issues');

    if Health.QuickCheck then
      WriteLn('Quick check passed');
  finally
    Health.Free;
  end;
end;
```

### Test Minimal Connection

```pascal
// Minimal test to isolate issues
program TestConnection;

{$mode objfpc}{$H+}

uses
  SysUtils, ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Options: TNDXSQLiteConnectionOptions;
  Conn: TNDXSQLiteConnection;
begin
  WriteLn('Testing NDXSQLite...');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := ':memory:';

    Conn := TNDXSQLiteConnection.Create(Options);
    try
      Conn.Open;
      WriteLn('Connected successfully');

      Conn.ExecuteNonQuery('CREATE TABLE test (id INTEGER)');
      WriteLn('Table created');

      Conn.ExecuteNonQuery('INSERT INTO test VALUES (1)');
      WriteLn('Insert successful');

      WriteLn('Count: ', Conn.ExecuteScalar('SELECT COUNT(*) FROM test'));

      Conn.Close;
      WriteLn('Test passed!');
    finally
      Conn.Free;
    end;
  finally
    Options.Free;
  end;
end.
```

---

## Getting Help

If issues persist:

1. Check existing issues on GitHub
2. Prepare minimal reproduction case
3. Include:
   - NDXSQLite version
   - Free Pascal version
   - Operating system and version
   - Error messages and stack traces
   - Minimal code to reproduce

---

*NDXSQLite Troubleshooting Guide - Version 1.0.0*
