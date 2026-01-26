# GUI Example 09: Version Information

This GUI example demonstrates how to retrieve and display version information for both the NDXSQLite library and the SQLite runtime.

## Features Demonstrated

- NDXSQLite library version display
- SQLite runtime version detection
- Feature compatibility checking with visual feedback
- Feature support grid showing all SQLite features
- Cross-platform GUI (Windows, Linux, macOS)

## Screenshot Description

The application window contains:

1. **NDXSQLite Library Panel** - Shows library version, components, and release date
2. **SQLite Runtime Panel** - Shows built-for version vs actual runtime version
3. **Feature Check Panel** - Dropdown to test specific feature support
4. **Feature Grid** - Complete list of SQLite features with support status
5. **Log Panel** - Detailed logging of version checks

## Key Functions Used

From `ndxsqliteversion` unit:

| Function | Description |
|----------|-------------|
| `GetLibraryVersion` | Library version string |
| `GetLibraryVersionFull` | Version with date |
| `GetLibraryVersionMajor/Minor/Patch` | Version components |
| `GetBuiltForSQLiteVersion` | Static SQLite version |
| `GetRuntimeSQLiteVersion` | Actual SQLite version |
| `CheckSQLiteMinVersion(N)` | Feature support check |

## SQLite Features Tracked

| Feature | Minimum Version |
|---------|-----------------|
| FTS3/FTS4 | 3.7.0 |
| FTS5 | 3.9.0 |
| JSON1 Extension | 3.9.0 |
| Common Table Expressions | 3.8.3 |
| UPSERT | 3.24.0 |
| Window Functions | 3.25.0 |
| RETURNING clause | 3.35.0 |
| Math Functions | 3.35.0 |
| STRICT Tables | 3.37.0 |
| JSON Functions | 3.38.0 |

## Building

```bash
cd examples/gui/09_VersionInfo
lazbuild VersionInfoDemo.lpi
./VersionInfoDemo
```

## Usage

1. Launch the application
2. View library and SQLite version information in the top panels
3. Use the Feature Check dropdown to test specific features
4. Review the Feature Grid for complete compatibility overview
5. Check the Log panel for detailed information

## Code Example

```pascal
uses
  ndxsqliteversion, ndxsqliteplatform;

procedure CheckVersions;
begin
  // Initialize SQLite
  TNDXPlatform.LoadSQLiteLibrary;

  // Display library info
  ShowMessage('NDXSQLite: ' + GetLibraryVersion);
  ShowMessage('SQLite Runtime: ' + GetRuntimeSQLiteVersion);

  // Check feature support
  if CheckSQLiteMinVersion(3038000) then
    ShowMessage('JSON functions available!')
  else
    ShowMessage('JSON functions require SQLite 3.38+');
end;
```

## Cross-Platform

This example works on:
- Windows
- Linux
- macOS
