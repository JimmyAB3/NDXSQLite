# NDXSQLite Quick Start Guide

This guide will help you get started with NDXSQLite in your Free Pascal/Lazarus projects.

---

## Table of Contents

1. [Requirements](#requirements)
2. [Installation](#installation)
3. [First Program](#first-program)
4. [SQLCipher Encryption (Optional)](#sqlcipher-encryption-optional)
5. [Common Operations](#common-operations)
6. [Troubleshooting](#troubleshooting)

---

## Requirements

### Compiler
- Free Pascal 3.2.0 or later
- Lazarus 2.0 or later (optional, for IDE integration)

### Operating Systems
- **Linux** (x86_64, aarch64)
- **Windows** (32-bit, 64-bit)
- **macOS** (Intel, Apple Silicon)

### SQLite Library
The SQLite library is automatically detected. NDXSQLite searches standard system paths:

| Platform | Library Locations |
|----------|------------------|
| Linux | `/usr/lib/x86_64-linux-gnu/`, `/usr/lib64/`, Snap, Flatpak |
| Windows | Application directory, System32, PATH |
| macOS | `/usr/lib/`, Homebrew paths |

---

## Installation

### Option 1: Package Installation (Lazarus IDE)

1. Open Lazarus IDE
2. Go to **Package** → **Open Package File (.lpk)**
3. Select `NDXSQLite.lpk` from the project root
4. Click **Compile**, then **Use** → **Add to Project**

### Option 2: Manual Installation

Add the source paths to your project's compiler options:

```
-Fu/path/to/src-NDXSQLite/src
-Fu/path/to/src-NDXSQLite/src/core
-Fu/path/to/src-NDXSQLite/src/api
-Fu/path/to/src-NDXSQLite/src/database
-Fu/path/to/src-NDXSQLite/src/advanced
```

Or add to your `.lpi` file:
```xml
<SearchPaths>
  <OtherUnitFiles Value="path/to/src-NDXSQLite/src;path/to/src-NDXSQLite/src/core"/>
</SearchPaths>
```

### Option 3: Command Line Compilation

```bash
fpc -Fu./src -Fu./src/core -Fu./src/api -Fu./src/database yourprogram.pas
```

---

## First Program

Create a file named `HelloSQLite.lpr`:

```pascal
program HelloSQLite;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ndxsqliteconnection,
  ndxsqliteconnectionoptions;

var
  Options: TNDXSQLiteConnectionOptions;
  Conn: TNDXSQLiteConnection;
begin
  WriteLn('NDXSQLite Quick Start');
  WriteLn('=====================');
  WriteLn;

  // Create connection options
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := 'hello.db';
    Options.IsPrimaryConnection := True;

    // Create and open connection
    Conn := TNDXSQLiteConnection.Create(Options);
    try
      Conn.Open;
      WriteLn('Connected to: ', Conn.DatabasePath);

      // Create a table
      Conn.ExecuteNonQuery(
        'CREATE TABLE IF NOT EXISTS greetings (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  message TEXT NOT NULL,' +
        '  created_at TEXT DEFAULT CURRENT_TIMESTAMP' +
        ')');
      WriteLn('Table created.');

      // Insert data
      Conn.ExecuteNonQuery(
        'INSERT INTO greetings (message) VALUES (?)',
        ['Hello, NDXSQLite!']);
      WriteLn('Row inserted. Last ID: ', Conn.GetLastInsertRowId);

      // Query data
      WriteLn;
      WriteLn('Messages:');
      WriteLn('---------');
      with Conn.ExecuteQuery('SELECT id, message, created_at FROM greetings') do
      try
        while not EOF do
        begin
          WriteLn(Format('  [%d] %s (%s)', [
            FieldByName('id').AsInteger,
            FieldByName('message').AsString,
            FieldByName('created_at').AsString
          ]));
          Next;
        end;
      finally
        Free;
      end;

      Conn.Close;
      WriteLn;
      WriteLn('Done!');

    finally
      Conn.Free;
    end;
  finally
    Options.Free;
  end;
end.
```

### Compile and Run

```bash
# Compile
fpc -Fu../src -Fu../src/core -Fu../src/api -Fu../src/database HelloSQLite.lpr

# Run
./HelloSQLite
```

### Expected Output

```
NDXSQLite Quick Start
=====================

Connected to: hello.db
Table created.
Row inserted. Last ID: 1

Messages:
---------
  [1] Hello, NDXSQLite! (2026-01-26 12:00:00)

Done!
```

---

## SQLCipher Encryption (Optional)

NDXSQLite supports transparent database encryption via SQLCipher.

### Installing SQLCipher

**Ubuntu/Debian:**
```bash
sudo apt-get install libsqlcipher1 libsqlcipher-dev
```

**Fedora/RHEL:**
```bash
sudo dnf install sqlcipher sqlcipher-devel
```

**macOS (Homebrew):**
```bash
brew install sqlcipher
```

**Windows:**
Download prebuilt binaries from [SQLCipher releases](https://github.com/nicholasdeoux/sqlcipher/releases) and place `sqlcipher.dll` in your application directory.

### Using Encryption

```pascal
Options := TNDXSQLiteConnectionOptions.Create;
try
  Options.DatabasePath := 'secure.db';
  Options.EncryptionKey := 'my-secret-key-256bit';  // Set before opening
  Options.IsPrimaryConnection := True;

  Conn := TNDXSQLiteConnection.Create(Options);
  try
    Conn.Open;
    // Database is now encrypted with AES-256

    // Your code here...

    Conn.Close;
  finally
    Conn.Free;
  end;
finally
  Options.Free;
end;
```

### Checking SQLCipher Availability

```pascal
if TNDXSQLiteConnection.IsSQLCipherAvailable then
  WriteLn('SQLCipher is available - encryption supported')
else
  WriteLn('SQLCipher not found - using standard SQLite');
```

### Changing Encryption Key

```pascal
// Change to a new key
Conn.ChangeEncryptionKey('new-secret-key');

// Remove encryption entirely
Conn.ChangeEncryptionKey('');
```

---

## Common Operations

### Transactions

```pascal
Conn.BeginTransaction;
try
  Conn.ExecuteNonQuery('INSERT INTO accounts (name) VALUES (?)', ['Alice']);
  Conn.ExecuteNonQuery('INSERT INTO accounts (name) VALUES (?)', ['Bob']);
  Conn.Commit;
except
  Conn.Rollback;
  raise;
end;
```

### Parameterized Queries

```pascal
// By position
Conn.ExecuteNonQuery(
  'INSERT INTO users (name, email) VALUES (?, ?)',
  ['John', 'john@example.com']);

// Scalar result
var Count: Integer;
Count := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');
WriteLn('Total users: ', Count);
```

### Connection Pool

```pascal
uses ndxsqliteconnectionpool;

var
  Pool: TNDXSQLiteConnectionPool;
  Conn: INDXSQLiteConnection;
begin
  Pool := TNDXSQLiteConnectionPool.Create(Options, 2, 10);
  try
    // Acquire a connection
    Conn := Pool.Acquire;
    try
      Conn.ExecuteNonQuery('INSERT INTO logs (msg) VALUES (?)', ['Event']);
    finally
      Pool.Release(Conn);
    end;
  finally
    Pool.Free;
  end;
end;
```

### WAL Mode (Recommended)

```pascal
Options.JournalMode := jmWAL;  // Write-Ahead Logging
Options.SyncMode := smNormal;  // Balanced durability/performance
```

---

## Troubleshooting

### "Library not found" Error

**Linux:**
```bash
# Check if SQLite is installed
ldconfig -p | grep sqlite

# Install if missing
sudo apt-get install libsqlite3-0
```

**Windows:**
Ensure `sqlite3.dll` is in your application directory or system PATH.

**macOS:**
```bash
# SQLite is included with macOS, but you can install via Homebrew
brew install sqlite
```

### "Database is locked" Error

This occurs when multiple connections try to write simultaneously.

**Solutions:**
1. Use WAL mode: `Options.JournalMode := jmWAL`
2. Increase busy timeout: `Options.BusyTimeout := 10000` (10 seconds)
3. Use a connection pool for multi-threaded applications

### "File is not a database" Error

This typically means:
1. Wrong encryption key
2. Database file is corrupted
3. Trying to open an encrypted database without SQLCipher

**For encrypted databases:**
- Ensure SQLCipher is installed
- Verify the encryption key is correct
- The key must be set BEFORE opening the connection

### Performance Issues

1. **Enable WAL mode** for concurrent read/write
2. **Use transactions** for bulk inserts (10x+ faster)
3. **Increase cache size**: `Options.CacheSize := 4000` (4MB)
4. **Use prepared statements** for repeated queries

---

## Next Steps

- Read the [API Reference](API.md) for complete documentation
- Explore [examples](../examples/console/) for working code
- Check the [Changelog](CHANGELOG.md) for version history

---

*NDXSQLite - Professional SQLite for Free Pascal*
