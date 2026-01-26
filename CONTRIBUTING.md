# Contributing to NDXSQLite

Thank you for your interest in contributing to NDXSQLite. This document provides guidelines for contributing to the project.

---

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Environment](#development-environment)
4. [Coding Standards](#coding-standards)
5. [Commit Conventions](#commit-conventions)
6. [Pull Request Process](#pull-request-process)
7. [Testing Requirements](#testing-requirements)
8. [Documentation](#documentation)

---

## Code of Conduct

- Be respectful and constructive in all interactions
- Focus on the technical merits of contributions
- Help newcomers learn and contribute effectively

---

## Getting Started

### Prerequisites

- **Free Pascal 3.2.0** or later
- **Lazarus 2.0** or later (optional, for IDE and GUI examples)
- **Git** for version control
- **SQLite 3** development libraries (for building)
- **SQLCipher** (optional, for encryption features)

### Fork and Clone

```bash
# Fork the repository on GitHub, then clone your fork
git clone https://github.com/YOUR_USERNAME/NDXSQLite.git
cd NDXSQLite

# Add upstream remote
git remote add upstream https://github.com/ORIGINAL_OWNER/NDXSQLite.git
```

### Build Verification

```bash
# Build the package
lazbuild --add-package-link=src/NDXSQLite.lpk

# Run tests
cd tests
fpc NDXSQLiteTests.lpr && ./NDXSQLiteTests
fpc NDXSQLiteAdvancedTests.lpr && ./NDXSQLiteAdvancedTests
```

---

## Development Environment

### Recommended Setup

1. **Lazarus IDE** - For package management and GUI examples
2. **VS Code** or **Vim** - For lightweight editing with Pascal extensions
3. **Git** - Command line or GUI client

### Directory Structure

```
src-NDXSQLite/
├── src/                    # Source code (modify here)
│   ├── core/               # Connection, options, types
│   ├── api/                # SQLite3 API bindings
│   ├── database/           # Low-level database wrapper
│   ├── pool/               # Connection pooling
│   ├── async/              # Asynchronous operations
│   └── advanced/           # Backup, migrations, BLOB, etc.
├── tests/                  # Test suites (add tests here)
├── examples/               # Working examples (update as needed)
│   ├── console/            # 149 console examples
│   └── gui/                # 10 GUI examples
└── docs/                   # Documentation
```

### Building Individual Components

```bash
# Build a specific test
cd tests
fpc -Fu../src -Fu../src/core -Fu../src/api NDXSQLiteTests.lpr

# Build an example
cd examples/console/01_BasicConnection
fpc -Fu../../../src BasicConnection.lpr
```

---

## Coding Standards

### Pascal Style Guide

#### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Units | `ndxsqlite` prefix, lowercase | `ndxsqliteconnection.pas` |
| Classes | `TNDX` prefix, PascalCase | `TNDXSQLiteConnection` |
| Interfaces | `INDX` prefix, PascalCase | `INDXSQLiteConnection` |
| Methods | PascalCase | `ExecuteNonQuery` |
| Properties | PascalCase | `DatabasePath` |
| Private fields | `F` prefix | `FDatabasePath` |
| Parameters | PascalCase, `A` prefix for ambiguous | `AValue`, `Options` |
| Constants | PascalCase or UPPER_CASE | `DefaultTimeout`, `MAX_CONNECTIONS` |
| Enumerations | `T` prefix, `jm`/`sm` item prefix | `TNDXJournalMode`, `jmWAL` |

#### Code Formatting

```pascal
// Unit header
unit ndxsqliteexample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TNDXSQLiteExample - Brief description }
  TNDXSQLiteExample = class(TObject)
  private
    FValue: Integer;
    procedure SetValue(AValue: Integer);
  protected
    procedure DoSomething; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
    function GetResult: String;

    property Value: Integer read FValue write SetValue;
  end;

implementation

{ TNDXSQLiteExample }

constructor TNDXSQLiteExample.Create;
begin
  inherited Create;
  FValue := 0;
end;

destructor TNDXSQLiteExample.Destroy;
begin
  // Cleanup
  inherited Destroy;
end;

procedure TNDXSQLiteExample.SetValue(AValue: Integer);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
end;

procedure TNDXSQLiteExample.DoSomething;
begin
  // Implementation
end;

procedure TNDXSQLiteExample.Execute;
var
  I: Integer;
begin
  for I := 0 to FValue - 1 do
  begin
    DoSomething;
  end;
end;

function TNDXSQLiteExample.GetResult: String;
begin
  Result := IntToStr(FValue);
end;

end.
```

#### Formatting Rules

- **Indentation:** 2 spaces (no tabs)
- **Line length:** Maximum 100 characters
- **Braces:** `begin`/`end` on separate lines
- **Spacing:** Space after keywords (`if`, `for`, `while`), no space before parentheses
- **Comments:** Use `//` for single-line, `{ }` for multi-line, `///` for documentation

#### Error Handling

```pascal
// Use try-finally for resource cleanup
Conn := TNDXSQLiteConnection.Create(Options);
try
  Conn.Open;
  // Work with connection
finally
  Conn.Free;
end;

// Use try-except for error handling
try
  Conn.ExecuteNonQuery(SQL);
except
  on E: ENDXSQLiteException do
  begin
    LogError('Database error: ' + E.Message);
    raise;
  end;
end;

// Raise meaningful exceptions
if not FileExists(DatabasePath) then
  raise ENDXSQLiteException.Create('Database file not found: ' + DatabasePath);
```

#### Thread Safety

```pascal
// Use critical sections for shared resources
FCriticalSection.Enter;
try
  // Thread-safe operations
finally
  FCriticalSection.Leave;
end;

// Document thread safety in comments
{ Thread-safe method - uses internal locking }
procedure TNDXSQLitePool.Acquire;
```

---

## Commit Conventions

### Commit Message Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Types

| Type | Description |
|------|-------------|
| `feat` | New feature |
| `fix` | Bug fix |
| `docs` | Documentation only |
| `style` | Formatting, no code change |
| `refactor` | Code restructuring |
| `perf` | Performance improvement |
| `test` | Adding or updating tests |
| `chore` | Maintenance tasks |

### Scopes

| Scope | Description |
|-------|-------------|
| `core` | Connection, options, types |
| `api` | SQLite3 API bindings |
| `pool` | Connection pooling |
| `async` | Asynchronous operations |
| `backup` | Backup functionality |
| `migration` | Schema migrations |
| `fts` | Full-text search |
| `blob` | BLOB handling |
| `json` | JSON support |
| `csv` | CSV import/export |
| `vtab` | Virtual tables |
| `udf` | User-defined functions |
| `cipher` | SQLCipher encryption |
| `examples` | Example code |
| `tests` | Test suites |
| `docs` | Documentation |

### Examples

```bash
# Feature
feat(pool): add connection validation on acquire

# Bug fix
fix(core): resolve memory leak in statement finalization

# Documentation
docs(api): add examples for ExecuteScalar method

# Performance
perf(query): optimize parameter binding for large batches

# Tests
test(migration): add rollback edge case tests
```

### Commit Best Practices

- Keep commits focused and atomic
- Write clear, descriptive messages
- Reference issues when applicable: `Fixes #123`
- Avoid commits that break the build

---

## Pull Request Process

### Before Submitting

1. **Sync with upstream:**
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

2. **Run all tests:**
   ```bash
   cd tests
   ./NDXSQLiteTests && ./NDXSQLiteAdvancedTests
   ```

3. **Build examples:**
   ```bash
   # Verify affected examples compile
   cd examples/console/01_BasicConnection
   fpc -Fu../../../src BasicConnection.lpr
   ```

4. **Update documentation** if adding/changing public API

### PR Requirements

- [ ] Code compiles without warnings (`-vw` flag)
- [ ] All existing tests pass
- [ ] New features have tests
- [ ] Documentation updated for API changes
- [ ] Examples updated if applicable
- [ ] Commit messages follow conventions

### PR Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Testing
- [ ] Existing tests pass
- [ ] New tests added for new features
- [ ] Manual testing performed

## Checklist
- [ ] Code follows style guidelines
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] No warnings on compilation
```

### Review Process

1. Submit PR with clear description
2. CI pipeline runs automatically
3. Maintainer reviews code
4. Address feedback if any
5. Squash and merge when approved

---

## Testing Requirements

### Test Coverage

All new features must include tests:

| Feature Type | Required Tests |
|--------------|----------------|
| New class | Constructor, destructor, all public methods |
| New method | Success case, error cases, edge cases |
| Bug fix | Test that reproduces the bug |
| API change | Updated existing tests |

### Writing Tests

Tests go in `/tests/` directory. Follow existing patterns:

```pascal
procedure TestNewFeature;
var
  Conn: TNDXSQLiteConnection;
begin
  Log('Testing new feature...');

  Conn := CreateTestConnection;
  try
    // Test implementation
    if Conn.NewMethod = ExpectedValue then
      LogSuccess('New feature works correctly')
    else
      LogFailure('New feature returned unexpected value');
  finally
    Conn.Free;
  end;
end;
```

### Running Tests

```bash
# Run core tests
cd tests
fpc NDXSQLiteTests.lpr && ./NDXSQLiteTests

# Run advanced tests
fpc NDXSQLiteAdvancedTests.lpr && ./NDXSQLiteAdvancedTests

# Run SQLCipher tests (requires SQLCipher)
fpc SQLCipherTests.lpr && ./SQLCipherTests
```

### CI Pipeline

The GitHub Actions CI runs:
1. Build on Linux, Windows, macOS
2. Run all test suites
3. Build all 149 console examples
4. Build all 10 GUI examples
5. Run SQLCipher tests (Linux only)

All checks must pass before merge.

---

## Documentation

### When to Update Docs

- Adding new public class or method
- Changing method signatures
- Adding new example
- Fixing incorrect documentation

### Documentation Files

| File | Content |
|------|---------|
| `docs/API.md` | Complete API reference |
| `docs/QUICKSTART.md` | Getting started guide |
| `docs/CHANGELOG.md` | Version history |
| `README.md` | Project overview |
| `examples/README.md` | Example index |

### Documentation Style

- Use clear, concise language
- Include code examples
- Document parameters and return values
- Note exceptions that may be raised
- Indicate thread safety

### Example Documentation

```pascal
{ Executes a SQL query and returns a single value.

  @param SQL The SQL query to execute
  @param Params Optional array of parameter values
  @returns The first column of the first row as Variant
  @raises ENDXSQLiteException on query error

  @example
    Count := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');

  @note Thread-safe when used with connection pooling }
function ExecuteScalar(const SQL: String;
  const Params: array of const): Variant;
```

---

## Questions?

- Open an issue for questions or discussion
- Tag maintainers for urgent matters
- Check existing issues before creating new ones

---

*Thank you for contributing to NDXSQLite!*
