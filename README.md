# NDXSQLite

**Professional SQLite Library for Free Pascal/Lazarus**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2%2B-orange.svg)](https://www.freepascal.org/)
[![Platform](https://img.shields.io/badge/Platform-Linux%20%7C%20Windows%20%7C%20macOS-lightgrey.svg)]()

---

## Overview

NDXSQLite is a modern, thread-safe SQLite wrapper for Free Pascal and Lazarus. It provides a clean, object-oriented API for database operations with support for advanced features like connection pooling, schema migrations, and transparent encryption via SQLCipher.

### Key Features

- **Native SQLite API** - Direct bindings without SQLDB dependency
- **Thread-Safe** - Full thread safety with connection pooling
- **SQLCipher Support** - Transparent AES-256 database encryption
- **Schema Migrations** - Version-controlled database schema evolution
- **Cross-Platform** - Linux, Windows, and macOS support
- **TDataSet Compatible** - Works with Lazarus DB-aware controls
- **Comprehensive** - 148 working examples covering all features

---

## Quick Start

### Installation

1. Clone or download the repository
2. Add the source paths to your project:
   ```
   -Fu/path/to/src-NDXSQLite/src
   -Fu/path/to/src-NDXSQLite/src/core
   -Fu/path/to/src-NDXSQLite/src/api
   ```

### Basic Usage

```pascal
program QuickDemo;

{$mode objfpc}{$H+}

uses
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Options: TNDXSQLiteConnectionOptions;
  Conn: TNDXSQLiteConnection;
begin
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := 'myapp.db';
    Options.JournalMode := jmWAL;

    Conn := TNDXSQLiteConnection.Create(Options);
    try
      Conn.Open;

      // Create table
      Conn.ExecuteNonQuery(
        'CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)');

      // Insert with parameters
      Conn.ExecuteNonQuery(
        'INSERT INTO users (name) VALUES (?)', ['Alice']);

      // Query
      WriteLn('Total: ', Conn.ExecuteScalar('SELECT COUNT(*) FROM users'));

      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Options.Free;
  end;
end.
```

### With Encryption (SQLCipher)

```pascal
Options.DatabasePath := 'secure.db';
Options.EncryptionKey := 'my-secret-key';  // AES-256 encryption
```

---

## Features

### Core
| Feature | Description |
|---------|-------------|
| Connection Management | Auto-close timeout, connection states, action logging |
| Transactions | Deferred, Immediate, Exclusive modes with savepoints |
| Query Execution | ExecuteNonQuery, ExecuteScalar, ExecuteQuery with parameters |
| PRAGMA Support | Journal mode, sync mode, cache size, foreign keys, etc. |

### Advanced
| Feature | Description |
|---------|-------------|
| Connection Pool | Thread-safe pooling with statistics |
| Schema Migrations | Version tracking, up/down migrations |
| Online Backup | Progress reporting, integrity verification |
| Incremental BLOB I/O | Stream-based large binary handling |
| Full-Text Search | FTS5 helper functions |
| JSON Support | JSON extraction and manipulation |
| Virtual Tables | Custom virtual table framework |

### Platform Support
| Platform | Architecture | Status |
|----------|--------------|--------|
| Linux | x86_64, aarch64 | Supported |
| Windows | x86, x64 | Supported |
| macOS | Intel, Apple Silicon | Supported |
| Snap | x86_64 | Supported |
| Flatpak | x86_64 | Supported |

---

## Documentation

| Document | Description |
|----------|-------------|
| [Quick Start Guide](docs/QUICKSTART.md) | Installation and first program |
| [API Reference](docs/API.md) | Complete class and method documentation |
| [Changelog](docs/CHANGELOG.md) | Version history |
| [Building](BUILDING.md) | Compilation instructions |
| [Table of Contents](TOC.md) | Full documentation index |

---

## Examples

The `examples/console/` directory contains 148 working examples:

```
01_BasicConnection     - Simple connection and queries
02_Transactions        - Transaction handling
08_ConnectionPool      - Thread-safe connection pooling
11_BackupRestore       - Online database backup
14_Migrations          - Schema version control
43_EncryptedDatabase   - SQLCipher encryption
148_SQLCipherEncryption - Advanced encryption scenarios
```

Run an example:
```bash
cd examples/console/01_BasicConnection
fpc -Fu../../../src BasicConnection.lpr
./BasicConnection
```

---

## Requirements

- **Compiler:** Free Pascal 3.2.0 or later
- **IDE:** Lazarus 2.0+ (optional)
- **SQLite:** Automatically detected from system paths
- **SQLCipher:** Optional, for encryption support

---

## Project Structure

```
src-NDXSQLite/
├── src/                    # Source code
│   ├── core/               # Connection, options, types
│   ├── api/                # SQLite3 API bindings
│   ├── advanced/           # Backup, migrations, BLOB, etc.
│   ├── pool/               # Connection pooling
│   └── ...
├── tests/                  # Test suites
├── examples/               # Working examples
├── docs/                   # Documentation
└── .github/workflows/      # CI/CD configuration
```

---

## Contributing

Contributions are welcome. Please ensure:

1. Code compiles without warnings
2. Tests pass
3. Examples work correctly
4. Documentation is updated

---

## License

MIT License - See [LICENSE](LICENSE) for details.

---

## Author

**Nicolas DEOUX**   

- 📧 [NDXDev@gmail.com](mailto:NDXDev@gmail.com)  
- 💼 [LinkedIn](https://www.linkedin.com/in/nicolas-deoux-ab295980/)  
- 🐙 [GitHub](https://github.com/NDXDeveloper)  


---

<div align="center">

[![Star on GitHub](https://img.shields.io/github/stars/NDXDeveloper/NDXSQLite?style=social)](https://github.com/NDXDeveloper/NDXSQLite)
[![Follow](https://img.shields.io/github/followers/NDXDeveloper?style=social)](https://github.com/NDXDeveloper)

**[⬆ Back to top](#ndxsqlite)**

*Last updated: January 2026 | SQLite 3.45.0+*

</div>
