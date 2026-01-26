# Changelog

All notable changes to NDXSQLite will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [1.0.0] - 2026-01-26

### Initial Release

First stable release of NDXSQLite, a professional SQLite library for Free Pascal/Lazarus.

### Added

#### Core Features
- **TNDXSQLiteConnection** - Full-featured database connection class
  - Connection lifecycle management with auto-close timeout
  - Thread-safe operations with internal locking
  - Action history and logging
  - Primary/secondary connection modes

- **TNDXSQLiteConnectionOptions** - Comprehensive configuration
  - All SQLite PRAGMA settings (journal mode, sync mode, cache size, etc.)
  - SQLCipher encryption key support
  - Connection timeout and busy timeout settings
  - Memory database support

- **Query Execution**
  - `ExecuteNonQuery` - INSERT/UPDATE/DELETE with affected row count
  - `ExecuteScalar` - Single value retrieval
  - `ExecuteQuery` - Full dataset results
  - `ExecuteScalarInt64` - Native Int64 support (avoids Variant truncation)
  - Parameterized queries with positional binding

#### Transaction Support
- `BeginTransaction` with Deferred, Immediate, and Exclusive modes
- `Commit` and `Rollback`
- Savepoints for nested transactions (`Savepoint`, `ReleaseSavepoint`, `RollbackToSavepoint`)

#### SQLCipher Integration
- Transparent AES-256 database encryption
- Automatic SQLCipher library detection
- `EncryptionKey` option for encrypted databases
- `ChangeEncryptionKey` for key rotation
- `IsSQLCipherAvailable` class method
- Graceful fallback to standard SQLite when SQLCipher unavailable

#### Connection Pooling
- **TNDXSQLiteConnectionPool** - Thread-safe connection pool
  - Configurable min/max pool size
  - Automatic connection validation
  - Statistics tracking (peak usage, wait times, etc.)
  - Timeout-based acquisition
- **TNDXPooledConnection** - RAII helper for automatic release

#### Advanced Features
- **TNDXSQLiteBackup** - Online backup API
  - Progress reporting with cancellation
  - Integrity verification after backup
  - Configurable pages per step

- **TNDXMigrationManager** - Schema migration framework
  - Version tracking in database table
  - Up/down migration support
  - SQL-based and custom migrations
  - Rollback capability

- **TNDXSQLiteBlob** - Incremental BLOB I/O
  - Stream-based read/write
  - File I/O support
  - Memory-efficient for large BLOBs

- **TNDXSQLiteDataSet** - TDataSet implementation
  - Compatible with Lazarus DB-aware controls
  - Bidirectional navigation
  - Parameterized queries

#### Utility Modules
- CSV import/export (`ndxsqlitecsv`)
- SQL dump generation (`ndxsqlitedump`)
- Full-text search helpers (`ndxsqlitefts`)
- JSON support (`ndxsqlitejson`)
- Schema verification (`ndxsqliteverify`)
- User-defined functions (`ndxsqliteudf`)
- Custom collations (`ndxsqlitecollation`)
- Virtual tables (`ndxsqlitevtab`)
- R-Tree spatial indexes (`ndxsqliteertree`)
- Attached databases (`ndxsqliteattached`)
- Pre-update hooks (`ndxsqlitepreupdate`)
- Unlock notifications (`ndxsqliteunlocknotify`)
- Custom authorizers (`ndxsqliteauthorizer`)

#### Cross-Platform Support
- **Linux** - x86_64, aarch64, Snap, Flatpak
- **Windows** - 32-bit and 64-bit
- **macOS** - Intel and Apple Silicon
- Automatic library path detection per platform

#### Quality Assurance
- Comprehensive test suite (190+ tests)
- CI/CD pipeline with GitHub Actions
- Multi-platform automated testing
- SQLCipher encryption tests

#### Documentation
- 148 working console examples
- Quick start guide
- API reference
- Building instructions

### Known Limitations

The following features are planned for future releases:

- Base64 encoding for BLOB export in CSV module
- Column mapping for CSV import

See [TODO.md](../TODO.md) for the complete list.

---

## Version History Summary

| Version | Date | Description |
|---------|------|-------------|
| 1.0.0 | 2026-01-26 | Initial stable release |

---

*For the latest updates, visit the project repository.*
