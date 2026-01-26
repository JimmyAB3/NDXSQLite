# Building NDXSQLite

This document describes how to build NDXSQLite from source on different platforms.

## Requirements

### Compiler

- **Free Pascal Compiler (FPC)** 3.2.2 or later
- **Lazarus IDE** 2.2 or later (for GUI examples and IDE integration)

### SQLite Library

NDXSQLite dynamically loads the SQLite library at runtime. You need the SQLite shared library installed on your system.

| Platform | Library Name | Installation |
|----------|--------------|--------------|
| Linux | `libsqlite3.so.0` | `sudo apt install libsqlite3-0` |
| Windows | `sqlite3.dll` | Download from [sqlite.org](https://www.sqlite.org/download.html) |
| macOS | `libsqlite3.dylib` | Included with macOS |

### Platform-Specific Dependencies

#### Linux (Debian/Ubuntu)

```bash
# Runtime dependencies
sudo apt-get install libsqlite3-0

# Development dependencies (for compiling)
sudo apt-get install libsqlite3-dev

# GUI dependencies (for GUI examples only)
sudo apt-get install libgtk2.0-0 libgtk2.0-dev
```

#### Linux (Fedora/RHEL)

```bash
# Runtime dependencies
sudo dnf install sqlite-libs

# Development dependencies
sudo dnf install sqlite-devel

# GUI dependencies (for GUI examples only)
sudo dnf install gtk2 gtk2-devel
```

#### Linux (Arch)

```bash
sudo pacman -S sqlite gtk2
```

#### Windows

1. Download the SQLite DLL from https://www.sqlite.org/download.html
   - For 64-bit: `sqlite-dll-win-x64-*.zip`
   - For 32-bit: `sqlite-dll-win-x86-*.zip`

2. Extract `sqlite3.dll` to one of:
   - Your application directory (recommended)
   - `C:\Windows\System32` (64-bit)
   - `C:\Windows\SysWOW64` (32-bit on 64-bit Windows)

#### macOS

SQLite is included with macOS. No additional installation required.

For a newer version via Homebrew:
```bash
brew install sqlite
```

### SQLCipher (Optional - for database encryption)

SQLCipher is required for full database encryption support. If not installed, encryption features will be disabled but the library will work normally with standard SQLite.

#### Linux (Debian/Ubuntu)

```bash
# SQLCipher runtime and development
sudo apt-get install libsqlcipher0 libsqlcipher-dev
```

#### Linux (Fedora/RHEL)

```bash
sudo dnf install sqlcipher sqlcipher-devel
```

#### Linux (Arch)

```bash
sudo pacman -S sqlcipher
```

#### Windows

Download SQLCipher from https://www.zetetic.net/sqlcipher/ or build from source.
Replace `sqlite3.dll` with the SQLCipher version.

#### macOS

```bash
brew install sqlcipher
```

## Building

### Using Makefile (Linux/macOS)

Run `make help` to see all available targets:

```
BUILD:
  make build            - Compile core tests
  make build-sqlcipher  - Compile SQLCipher tests
  make build-examples   - Compile all examples (149 console + 10 GUI)
  make build-all        - Compile everything
  make rebuild          - Clean and rebuild core tests

TEST:
  make test             - Run core tests
  make test-sqlcipher   - Run SQLCipher tests (requires libsqlcipher)
  make test-all         - Run all tests including SQLCipher

VERIFY:
  make check            - Full verification (clean, build, test)
  make info             - Show project information

INSTALL:
  make install          - Install NDXSQLite package in Lazarus
  make uninstall        - Remove NDXSQLite package from Lazarus
  make package          - Build package without installing

RELEASE:
  make tag VERSION=x.y.z - Update version and create git tag

CLEANUP:
  make clean            - Clean src/ and tests/
  make clean-lib        - Clean only lib/ directories
  make clean-tests      - Clean tests/ (compiled + test DBs)
  make clean-examples   - Clean examples/
  make clean-all        - Clean everything
```

Common workflows:

```bash
# Quick build and test
make build && make test

# Full verification before commit
make check

# Install in Lazarus IDE
make install
```

### Using Lazarus IDE

1. Open `src/NDXSQLite.lpk`
2. Click "Compile" to build the package
3. Click "Use > Install" to install in the IDE (optional)

### Using lazbuild (Command Line)

```bash
# Register the package
lazbuild --add-package-link=src/NDXSQLite.lpk

# Build tests
cd tests
lazbuild NDXSQLiteTests.lpi
lazbuild NDXSQLiteAdvancedTests.lpi

# Build an example
cd examples/console/01_BasicConnection
lazbuild BasicConnection.lpi
```

## Running Tests

```bash
# From the tests directory
cd tests

# Run all tests
./NDXSQLiteTests
./NDXSQLiteAdvancedTests
./DateTimeUtilsTests --all --format=plain
./DateTimeUtilsFunctionalTests --all --format=plain
```

## CI/CD

The project uses GitHub Actions for continuous integration. See `.github/workflows/ci.yml`.

Tests run automatically on:
- Ubuntu (latest)
- Windows (latest)
- macOS (latest)

## Troubleshooting

### "libsqlite3.so.0: cannot open shared object file"

SQLite library not found. Install it:
```bash
sudo apt install libsqlite3-0
```

### "sqlite3.dll not found" (Windows)

Download `sqlite3.dll` from sqlite.org and place it in your application directory or System32.

### "Undefined symbol: sqlite3_*"

SQLite library version is too old. Update to SQLite 3.7.0 or later:
```bash
sudo apt install --upgrade libsqlite3-0
```

### GUI examples fail to compile on Linux

Install GTK2 development packages:
```bash
sudo apt install libgtk2.0-dev
```

## Version Information

After building, you can check version information:

```pascal
uses ndxsqliteversion, ndxsqliteplatform;

begin
  // Load SQLite
  TNDXPlatform.LoadSQLiteLibrary;

  // Check versions
  WriteLn('NDXSQLite: ', GetLibraryVersion);
  WriteLn('SQLite Runtime: ', GetRuntimeSQLiteVersion);
end.
```
