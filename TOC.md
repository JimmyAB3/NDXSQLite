# NDXSQLite Documentation

**Version 1.0.0** | **Author:** Nicolas DEOUX - NDX Software | **License:** MIT

---

## Table of Contents

### Getting Started

- [README](README.md) - Project overview and quick start
- [Quick Start Guide](docs/QUICKSTART.md) - Installation and first program
- [Building from Source](BUILDING.md) - Compilation instructions
- [Contributing](CONTRIBUTING.md) - Contribution guidelines

### Reference

- [API Reference](docs/API.md) - Complete API documentation
- [Architecture](docs/ARCHITECTURE.md) - Internal architecture and design patterns
- [Changelog](docs/CHANGELOG.md) - Version history

### Guides

- [Performance](docs/PERFORMANCE.md) - Performance optimization and benchmarks
- [Security](docs/SECURITY.md) - SQLCipher encryption and security best practices
- [Testing](docs/TESTING.md) - Test suite documentation
- [Troubleshooting](docs/TROUBLESHOOTING.md) - Common issues and solutions

### CI/CD

- [CI/CD Pipeline](.github/workflows/README.md) - GitHub Actions workflow documentation

### Examples

149 console examples and 10 GUI examples covering all features:

| Category | Examples |
|----------|----------|
| **Basic** | 01-10: Connection, Transactions, Statements, Types, Errors |
| **Configuration** | 11-20: Options, Factory, Pool, Async, Health Check |
| **Features** | 21-40: Backup, JSON, FTS, Migrations, CSV, Memory DB |
| **Advanced** | 41-70: WAL, Triggers, Views, BLOB, Indexes, Upsert |
| **Patterns** | 71-100: EAV, Polymorphism, Shopping Cart, Auth, Tags |
| **Enterprise** | 101-148: Workflow, Replication, Analytics, SQLCipher |

Browse examples: [Examples Index](examples/README.md) | [Console Examples](examples/console/) | [GUI Examples](examples/gui/)

---

## Quick Links

| Task | Documentation |
|------|---------------|
| Install and run | [Quick Start](docs/QUICKSTART.md) |
| Connect to database | [API - TNDXSQLiteConnection](docs/API.md#tndxsqliteconnection) |
| Execute queries | [API - Query Methods](docs/API.md#query-methods) |
| Use transactions | [API - Transactions](docs/API.md#transactions) |
| Enable encryption | [Security - SQLCipher](docs/SECURITY.md#sqlcipher-encryption) |
| Optimize performance | [Performance Guide](docs/PERFORMANCE.md) |
| Connection pooling | [API - TNDXSQLiteConnectionPool](docs/API.md#tndxsqliteconnectionpool) |
| Schema migrations | [API - TNDXMigrationManager](docs/API.md#tndxmigrationmanager) |
| Backup database | [API - TNDXSQLiteBackup](docs/API.md#tndxsqlitebackup) |
| Troubleshoot issues | [Troubleshooting](docs/TROUBLESHOOTING.md) |
| Contribute | [Contributing](CONTRIBUTING.md) |

---

## Project Structure

```
src-NDXSQLite/
├── src/
│   ├── core/           # Connection, options, types
│   ├── api/            # SQLite3 API bindings
│   ├── database/       # Low-level database wrapper
│   ├── pool/           # Connection pooling
│   ├── async/          # Asynchronous operations
│   ├── factory/        # Connection factory
│   ├── health/         # Health check utilities
│   ├── query/          # Query execution
│   ├── dataset/        # TDataSet implementation
│   └── advanced/       # Backup, migrations, BLOB, CSV, etc.
├── tests/              # Unit and integration tests
├── examples/           # Working code examples
├── docs/               # Documentation
└── .github/workflows/  # CI/CD configuration
```

---

*For support or to report issues, visit the project repository.*
