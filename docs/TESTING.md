# NDXSQLite Testing Guide

This document describes the testing infrastructure, how to run tests, write new tests, and contribute to test coverage.

---

## Table of Contents

1. [Test Suite Overview](#test-suite-overview)
2. [Running Tests](#running-tests)
3. [Test Framework](#test-framework)
4. [Writing Tests](#writing-tests)
5. [Test Categories](#test-categories)
6. [CI/CD Integration](#cicd-integration)
7. [Coverage Goals](#coverage-goals)

---

## Test Suite Overview

NDXSQLite uses a custom console-based testing framework. The test suites are:

| Test Suite | Lines | Tests | Purpose |
|------------|-------|-------|---------|
| `NDXSQLiteTests.lpr` | 18,219 | 60+ | Core functionality |
| `NDXSQLiteAdvancedTests.lpr` | 2,479 | 30+ | Advanced features |
| `SQLCipherTests.lpr` | 616 | 15+ | SQLCipher encryption |
| `DateTimeUtilsTests.lpr` | 50 | 5+ | DateTime utilities |
| `DateTimeUtilsFunctionalTests.lpr` | 52 | 5+ | DateTime functional |
| `NDXSQLiteNativeTests.lpr` | 788 | 20+ | Native API |

**Total: 135+ test procedures across 6 test programs**

---

## Running Tests

### Quick Start

```bash
cd tests

# Build and run core tests
fpc NDXSQLiteTests.lpr && ./NDXSQLiteTests

# Build and run advanced tests
fpc NDXSQLiteAdvancedTests.lpr && ./NDXSQLiteAdvancedTests

# Build and run all tests
fpc NDXSQLiteTests.lpr && ./NDXSQLiteTests && \
fpc NDXSQLiteAdvancedTests.lpr && ./NDXSQLiteAdvancedTests && \
fpc DateTimeUtilsTests.lpr && ./DateTimeUtilsTests && \
fpc DateTimeUtilsFunctionalTests.lpr && ./DateTimeUtilsFunctionalTests
```

### With Lazarus

```bash
cd tests

# Build using lazbuild
lazbuild NDXSQLiteTests.lpi
lazbuild NDXSQLiteAdvancedTests.lpi

# Run
./NDXSQLiteTests
./NDXSQLiteAdvancedTests
```

### SQLCipher Tests

SQLCipher tests require SQLCipher library installed:

```bash
# Ubuntu/Debian
sudo apt-get install libsqlcipher1 libsqlcipher-dev

# Build and run
cd tests
fpc SQLCipherTests.lpr && ./SQLCipherTests
```

### Platform-Specific Paths

The tests automatically detect platform and use appropriate temporary directories:

| Platform | Temp Directory |
|----------|---------------|
| Linux | `/tmp/ndxsqlite_tests/` |
| Windows | `%TEMP%\ndxsqlite_tests\` |
| macOS | `/tmp/ndxsqlite_tests/` |
| Snap | `$SNAP_USER_DATA/ndxsqlite_tests/` |
| Flatpak | `$XDG_CACHE_HOME/ndxsqlite_tests/` |

---

## Test Framework

### Architecture

The framework uses simple procedural design:

```pascal
// Global counters
var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

// Logging procedures
procedure Log(const Msg: String);
procedure LogSuccess(const Msg: String);
procedure LogFailure(const Msg: String);

// Test helpers
function CreateTestConnection: TNDXSQLiteConnection;
procedure CleanupTestDatabase;
```

### Test Structure

Each test is a standalone procedure:

```pascal
procedure TestBasicConnection;
var
  Options: TNDXSQLiteConnectionOptions;
  Conn: TNDXSQLiteConnection;
begin
  Log('Testing basic connection...');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := GetTestDatabasePath;
    Options.IsPrimaryConnection := True;

    Conn := TNDXSQLiteConnection.Create(Options);
    try
      Conn.Open;

      if Conn.IsOpen then
        LogSuccess('Connection opened successfully')
      else
        LogFailure('Failed to open connection');

      Conn.Close;

      if not Conn.IsOpen then
        LogSuccess('Connection closed successfully')
      else
        LogFailure('Failed to close connection');

    finally
      Conn.Free;
    end;
  finally
    Options.Free;
  end;
end;
```

### Assertions

The framework uses explicit pass/fail logging:

```pascal
// Success
if Result = Expected then
  LogSuccess('Test passed: ' + Description)
else
  LogFailure('Test failed: expected ' + Expected + ', got ' + Result);

// Exception expected
try
  SomeOperationThatShouldFail;
  LogFailure('Expected exception not raised');
except
  on E: ENDXSQLiteException do
    LogSuccess('Correct exception raised: ' + E.Message);
end;
```

### Test Lifecycle

```pascal
program NDXSQLiteTests;

begin
  // Initialize
  Log('NDXSQLite Test Suite');
  Log('====================');
  InitializeTestEnvironment;

  // Run tests
  TestBasicConnection;
  TestInsertSelect;
  TestTransactions;
  // ... more tests ...

  // Summary
  Log('');
  Log('Results: ' + IntToStr(TestsPassed) + ' passed, ' +
      IntToStr(TestsFailed) + ' failed');

  // Cleanup
  CleanupTestEnvironment;

  // Exit code
  if TestsFailed > 0 then
    Halt(1);
end.
```

---

## Writing Tests

### Test Template

```pascal
procedure TestFeatureName;
var
  Conn: TNDXSQLiteConnection;
  // Other variables
begin
  Log('Testing feature name...');

  Conn := CreateTestConnection;
  try
    // Setup
    Conn.ExecuteNonQuery('CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)');

    // Execute
    Conn.ExecuteNonQuery('INSERT INTO test (value) VALUES (?)', ['test']);

    // Verify
    if Conn.ExecuteScalar('SELECT COUNT(*) FROM test') = 1 then
      LogSuccess('Feature works correctly')
    else
      LogFailure('Feature did not work as expected');

    // Cleanup (optional - database is deleted after tests)
    Conn.ExecuteNonQuery('DROP TABLE test');

  finally
    Conn.Free;
  end;
end;
```

### Testing Exceptions

```pascal
procedure TestInvalidSQL;
var
  Conn: TNDXSQLiteConnection;
begin
  Log('Testing invalid SQL handling...');

  Conn := CreateTestConnection;
  try
    try
      Conn.ExecuteNonQuery('INVALID SQL SYNTAX');
      LogFailure('Expected exception for invalid SQL');
    except
      on E: ENDXSQLiteException do
        LogSuccess('Correct exception for invalid SQL: ' + E.Message);
      on E: Exception do
        LogFailure('Wrong exception type: ' + E.ClassName);
    end;
  finally
    Conn.Free;
  end;
end;
```

### Testing Transactions

```pascal
procedure TestTransactionRollback;
var
  Conn: TNDXSQLiteConnection;
  CountBefore, CountAfter: Integer;
begin
  Log('Testing transaction rollback...');

  Conn := CreateTestConnection;
  try
    // Setup
    Conn.ExecuteNonQuery('CREATE TABLE test (id INTEGER PRIMARY KEY)');
    Conn.ExecuteNonQuery('INSERT INTO test DEFAULT VALUES');
    CountBefore := Conn.ExecuteScalar('SELECT COUNT(*) FROM test');

    // Begin transaction
    Conn.BeginTransaction;
    Conn.ExecuteNonQuery('INSERT INTO test DEFAULT VALUES');
    Conn.ExecuteNonQuery('INSERT INTO test DEFAULT VALUES');

    // Rollback
    Conn.Rollback;

    // Verify
    CountAfter := Conn.ExecuteScalar('SELECT COUNT(*) FROM test');

    if CountAfter = CountBefore then
      LogSuccess('Rollback restored original state')
    else
      LogFailure('Rollback failed: count changed from ' +
                 IntToStr(CountBefore) + ' to ' + IntToStr(CountAfter));

  finally
    Conn.Free;
  end;
end;
```

### Testing with Multiple Connections

```pascal
procedure TestConcurrentAccess;
var
  Conn1, Conn2: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  Log('Testing concurrent access...');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := GetTestDatabasePath;
    Options.JournalMode := jmWAL;  // Required for concurrent access

    Conn1 := TNDXSQLiteConnection.Create(Options);
    Conn2 := TNDXSQLiteConnection.Create(Options);
    try
      Conn1.Open;
      Conn2.Open;

      // Conn1 creates table
      Conn1.ExecuteNonQuery('CREATE TABLE IF NOT EXISTS test (id INTEGER)');

      // Conn1 writes
      Conn1.ExecuteNonQuery('INSERT INTO test VALUES (1)');

      // Conn2 reads (should see the data in WAL mode)
      if Conn2.ExecuteScalar('SELECT COUNT(*) FROM test') >= 1 then
        LogSuccess('Concurrent access works correctly')
      else
        LogFailure('Concurrent access failed');

    finally
      Conn2.Free;
      Conn1.Free;
    end;
  finally
    Options.Free;
  end;
end;
```

### Test Helpers

Create reusable helpers for common patterns:

```pascal
type
  TTestHelper = class
  public
    class function CreateTestConnection: TNDXSQLiteConnection;
    class function GetTestDatabasePath: String;
    class procedure CleanupTestDatabase;
    class procedure CreateSampleTable(Conn: TNDXSQLiteConnection);
    class procedure InsertSampleData(Conn: TNDXSQLiteConnection; Count: Integer);
  end;

class function TTestHelper.CreateTestConnection: TNDXSQLiteConnection;
var
  Options: TNDXSQLiteConnectionOptions;
begin
  Options := TNDXSQLiteConnectionOptions.Create;
  Options.DatabasePath := GetTestDatabasePath;
  Options.IsPrimaryConnection := True;
  Options.JournalMode := jmWAL;

  Result := TNDXSQLiteConnection.Create(Options);
  Result.Open;

  // Note: Options ownership transferred to connection
end;
```

---

## Test Categories

### Core Tests (NDXSQLiteTests.lpr)

| Category | Tests | Description |
|----------|-------|-------------|
| Connection | 10+ | Open, close, reconnect, states |
| CRUD | 15+ | Insert, select, update, delete |
| Transactions | 8+ | Begin, commit, rollback, savepoints |
| Parameters | 6+ | Positional binding, types |
| Scalar | 5+ | COUNT, SUM, single values |
| Types | 8+ | Integer, Real, Text, Blob, Null |
| Errors | 5+ | Invalid SQL, constraints |
| PRAGMA | 4+ | Journal mode, foreign keys |

### Advanced Tests (NDXSQLiteAdvancedTests.lpr)

| Category | Tests | Description |
|----------|-------|-------------|
| UDF | 6+ | Scalar functions, aggregates |
| Collation | 4+ | Custom sort orders |
| Attached | 3+ | Multi-database queries |
| BLOB | 5+ | Incremental I/O, streaming |
| R-Tree | 3+ | Spatial queries |
| Virtual Tables | 4+ | Series, CSV, key-value |
| Authorizer | 3+ | Access control |
| Hooks | 4+ | Pre-update, unlock notify |

### SQLCipher Tests (SQLCipherTests.lpr)

| Category | Tests | Description |
|----------|-------|-------------|
| Encryption | 5+ | Create encrypted DB |
| Key Change | 3+ | Rotate encryption key |
| Compatibility | 3+ | Open without SQLCipher |
| Performance | 2+ | Encrypted operations |

---

## CI/CD Integration

### GitHub Actions Workflow

The CI pipeline runs tests on every push and pull request:

```yaml
jobs:
  build-and-test:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]

    steps:
      - name: Build tests
        run: |
          lazbuild tests/NDXSQLiteTests.lpi
          lazbuild tests/NDXSQLiteAdvancedTests.lpi

      - name: Run tests
        run: |
          ./tests/NDXSQLiteTests
          ./tests/NDXSQLiteAdvancedTests

  test-sqlcipher:
    needs: build-and-test
    runs-on: ubuntu-latest
    steps:
      - name: Install SQLCipher
        run: sudo apt-get install libsqlcipher1 libsqlcipher-dev

      - name: Run SQLCipher tests
        run: ./tests/SQLCipherTests
```

### Test Artifacts

On failure, the CI uploads test artifacts:

- `tests/*.db` - Test databases
- `tests/*.log` - Test output logs

Retention: 7 days

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All tests passed |
| 1 | One or more tests failed |

---

## Coverage Goals

### Current Coverage

| Module | Coverage | Goal |
|--------|----------|------|
| Core Connection | High | 90%+ |
| Transactions | High | 90%+ |
| Query Execution | High | 90%+ |
| Pool | Medium | 80%+ |
| Async | Medium | 80%+ |
| Advanced (UDF, etc.) | Medium | 75%+ |
| Platform | Low | 60%+ |

### Priority Test Areas

1. **Critical Path** - Connection, query, transactions
2. **Error Handling** - All exception paths
3. **Edge Cases** - Null values, empty results, large data
4. **Thread Safety** - Pool, async operations
5. **Platform Specific** - Path handling, library loading

### Adding Coverage

When adding new features:

1. Write tests for success cases
2. Write tests for error cases
3. Write tests for edge cases
4. Update existing tests if behavior changes

### Test Naming Convention

```
Test<Feature><Scenario>

Examples:
  TestConnectionOpen
  TestConnectionOpenInvalidPath
  TestTransactionCommit
  TestTransactionRollbackOnError
  TestPoolAcquireTimeout
  TestUDFScalarFunction
```

---

## Troubleshooting Tests

### Common Issues

**"Library not found" Error**

```bash
# Linux: Install SQLite
sudo apt-get install libsqlite3-0 libsqlite3-dev

# Verify
ldconfig -p | grep sqlite
```

**"Database is locked" Error**

- Ensure WAL mode for concurrent tests
- Check for orphaned test databases
- Clean temp directory

**Test Database Cleanup**

```bash
# Remove test databases
rm -rf /tmp/ndxsqlite_tests/
```

**Permission Issues**

```bash
# Check temp directory permissions
ls -la /tmp/ndxsqlite_tests/
chmod 755 /tmp/ndxsqlite_tests/
```

### Debugging Tests

```pascal
// Add verbose logging
procedure TestSomething;
begin
  Log('DEBUG: Starting test');
  Log('DEBUG: DatabasePath = ' + Conn.DatabasePath);
  Log('DEBUG: State = ' + IntToStr(Ord(Conn.State)));

  // ... test code ...

  Log('DEBUG: Test completed');
end;
```

### Running Single Test

Modify the test program to run only specific tests:

```pascal
begin
  // Comment out other tests
  // TestConnectionOpen;
  // TestTransactions;

  // Run only the test you're debugging
  TestSpecificFeature;
end.
```

---

*NDXSQLite Testing Guide - Version 1.0.0*
