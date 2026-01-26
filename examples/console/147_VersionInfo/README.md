# Example 147: Version Information

This example demonstrates how to retrieve version information for both the NDXSQLite library and the SQLite runtime library.

## Features Demonstrated

- Getting NDXSQLite library version (static, from build)
- Getting SQLite runtime version (dynamic, from loaded library)
- Checking SQLite minimum version requirements for features
- Comparing built-for version vs actual runtime version

## Key Functions

### Library Version (from `ndxsqliteversion` unit)

| Function | Description |
|----------|-------------|
| `GetLibraryVersion` | Returns version string (e.g., '1.0.0') |
| `GetLibraryVersionFull` | Returns version with date (e.g., '1.0.0 (2026-01-25)') |
| `GetLibraryVersionMajor` | Returns major version number |
| `GetLibraryVersionMinor` | Returns minor version number |
| `GetLibraryVersionPatch` | Returns patch version number |
| `GetLibraryVersionDate` | Returns release date |

### SQLite Version

| Function | Description |
|----------|-------------|
| `GetBuiltForSQLiteVersion` | Static SQLite version the library was built for |
| `GetRuntimeSQLiteVersion` | Actual SQLite version loaded at runtime |
| `GetRuntimeSQLiteVersionNumber` | Runtime version as integer (e.g., 3045001) |
| `CheckSQLiteMinVersion(N)` | Returns True if runtime version >= N |

### Version Constants (from `ndxsqlite_version.inc`)

```pascal
NDXSQLITE_VERSION_MAJOR   // Major version (Integer)
NDXSQLITE_VERSION_MINOR   // Minor version (Integer)
NDXSQLITE_VERSION_PATCH   // Patch version (Integer)
NDXSQLITE_VERSION_STRING  // Version string (e.g., '1.0.0')
NDXSQLITE_VERSION_DATE    // Release date (e.g., '2026-01-25')
NDXSQLITE_VERSION_FULL    // Full version string
NDXSQLITE_SQLITE_VERSION  // Built-for SQLite version
```

## SQLite Feature Version Requirements

| Feature | Minimum Version |
|---------|-----------------|
| FTS5 (Full-Text Search) | 3.9.0 (3009000) |
| RETURNING clause | 3.35.0 (3035000) |
| Math functions | 3.35.0 (3035000) |
| STRICT tables | 3.37.0 (3037000) |
| JSON functions | 3.38.0 (3038000) |

## Usage Example

```pascal
uses
  ndxsqliteversion, ndxsqliteplatform;

begin
  // Initialize SQLite
  TNDXPlatform.LoadSQLiteLibrary;

  // Display versions
  WriteLn('NDXSQLite: ', GetLibraryVersion);
  WriteLn('SQLite Runtime: ', GetRuntimeSQLiteVersion);

  // Check feature support
  if CheckSQLiteMinVersion(3038000) then
    WriteLn('JSON functions are available')
  else
    WriteLn('JSON functions require SQLite 3.38+');
end.
```

## Version Tagging (for maintainers)

The version constants are automatically updated when creating a release tag:

```bash
make tag VERSION=1.1.0
git push && git push --tags
```

This updates `src/ndxsqlite_version.inc`, commits the change, and creates a git tag.

## Building

```bash
cd examples/console/147_VersionInfo
lazbuild VersionInfo.lpi
./VersionInfo
```

## Cross-Platform

This example works on:
- Windows
- Linux
- macOS
