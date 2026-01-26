# Example 15: Cross-Platform Paths

This example demonstrates platform-aware path handling using TNDXPlatform.

## What you'll learn

- Platform detection (Windows, Linux, macOS)
- CPU architecture detection
- Standard directory locations
- XDG compliance on Linux
- Snap confinement handling
- SQLite library management
- Safe path operations

## Key concepts

### Platform detection

```pascal
if TNDXPlatform.IsWindows then
  // Windows-specific code
else if TNDXPlatform.IsLinux then
  // Linux-specific code
else if TNDXPlatform.IsMacOS then
  // macOS-specific code;

// Platform name and architecture
WriteLn('Platform: ', TNDXPlatform.PlatformName);
WriteLn('Architecture: ', TNDXPlatform.ArchitectureName);
```

### Standard paths

```pascal
// User data directory
DataDir := TNDXPlatform.GetDataDirectory;
// Windows: C:\Users\X\AppData\Local
// Linux: ~/.local/share
// macOS: ~/Library/Application Support

// Configuration directory
ConfigDir := TNDXPlatform.GetConfigDirectory;

// Temp directory
TempDir := TNDXPlatform.GetTempDirectory;

// Default database path
DBPath := TNDXPlatform.GetDefaultDatabasePath('myapp.db');
```

### Confinement detection

```pascal
if TNDXPlatform.IsSnap then
begin
  // Snap-specific paths
  WriteLn('Snap data: ', TNDXPlatform.GetSnapUserData);
  WriteLn('Snap common: ', TNDXPlatform.GetSnapUserCommon);
end;
```

### Safe operations

```pascal
// Ensure directory exists
TNDXPlatform.EnsureDirectoryExists(DataDir);

// Check writability
if TNDXPlatform.IsPathWritable(DataDir) then
  // Safe to write

// Normalize path (fix separators)
Path := TNDXPlatform.NormalizePath('/path/to//database.db');
```

### SQLite library management

```pascal
if TNDXPlatform.LoadSQLiteLibrary then
begin
  WriteLn('Version: ', TNDXPlatform.GetSQLiteVersion);
  WriteLn('Path: ', TNDXPlatform.GetLoadedLibraryPath);

  // Check minimum version
  if TNDXPlatform.CheckSQLiteVersion(3027000) then
    WriteLn('Supports VACUUM INTO');
end;
```

## Platform Paths

| Platform | User Data | Config | Temp |
|----------|-----------|--------|------|
| Windows | %LOCALAPPDATA% | %APPDATA% | %TEMP% |
| Linux | ~/.local/share | ~/.config | /tmp |
| macOS | ~/Library/Application Support | ~/Library/Preferences | /tmp |

## Building

```bash
lazbuild CrossPlatformPaths.lpi
```

## Running

```bash
./CrossPlatformPaths      # Linux/macOS
CrossPlatformPaths.exe    # Windows
```
