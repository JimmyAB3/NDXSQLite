{===============================================================================
  NDXSQLite Example 15 - Cross-Platform Paths
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Platform detection using TNDXPlatform
  - Standard path locations
  - XDG compliance on Linux
  - Snap confinement handling
  - macOS sandbox paths
  - SQLite library management

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CrossPlatformPaths;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils,
  ndxsqliteplatform;

begin
  WriteLn('=== NDXSQLite Example 15: Cross-Platform Paths ===');
  WriteLn;

  // 1. Platform detection
  WriteLn('1. Platform Detection:');
  WriteLn('   Platform name: ', TNDXPlatform.PlatformName);
  WriteLn('   Architecture: ', TNDXPlatform.ArchitectureName);
  WriteLn('   Is Windows: ', TNDXPlatform.IsWindows);
  WriteLn('   Is Linux: ', TNDXPlatform.IsLinux);
  WriteLn('   Is macOS: ', TNDXPlatform.IsMacOS);
  WriteLn('   Is Snap: ', TNDXPlatform.IsSnap);
  {$IFDEF DARWIN}
  WriteLn('   Is macOS Sandboxed: ', TNDXPlatform.IsMacOSSandboxed);
  {$ENDIF}
  WriteLn;

  // 2. Platform type enumeration
  WriteLn('2. Platform Type:');
  case TNDXPlatform.GetPlatformType of
    ptLinux: WriteLn('   Type: Standard Linux');
    ptLinuxSnap: WriteLn('   Type: Linux (Snap confined)');
    ptWindows: WriteLn('   Type: Windows');
    ptMacOS: WriteLn('   Type: macOS');
    ptUnknown: WriteLn('   Type: Unknown');
  end;
  WriteLn;

  // 3. CPU Architecture
  WriteLn('3. CPU Architecture:');
  case TNDXPlatform.GetCPUArchitecture of
    cpuX86_64: WriteLn('   Architecture: x86_64 (AMD64/Intel 64-bit)');
    cpuX86: WriteLn('   Architecture: x86 (Intel 32-bit)');
    cpuARM64: WriteLn('   Architecture: ARM64 (aarch64)');
    cpuARM32: WriteLn('   Architecture: ARM32 (armhf)');
    cpuUnknown: WriteLn('   Architecture: Unknown');
  end;
  WriteLn;

  // 4. Standard directories
  WriteLn('4. Standard Directories:');
  WriteLn('   Data directory: ', TNDXPlatform.GetDataDirectory);
  WriteLn('   Config directory: ', TNDXPlatform.GetConfigDirectory);
  WriteLn('   Temp directory: ', TNDXPlatform.GetTempDirectory);
  WriteLn('   Executable directory: ', TNDXPlatform.GetExecutableDirectory);
  WriteLn;

  // 5. Database path handling
  WriteLn('5. Database Path Handling:');
  WriteLn('   Default path for "myapp.db":');
  WriteLn('     ', TNDXPlatform.GetDefaultDatabasePath('myapp.db'));
  WriteLn('   Normalized path example:');
  WriteLn('     ', TNDXPlatform.NormalizePath('/path/to//database.db'));
  WriteLn;

  // 6. Path validation
  WriteLn('6. Path Validation:');
  WriteLn('   Temp dir writable: ', TNDXPlatform.IsPathWritable(TNDXPlatform.GetTempDirectory));
  WriteLn('   Data dir writable: ', TNDXPlatform.IsPathWritable(TNDXPlatform.GetDataDirectory));
  WriteLn;

  // 7. Snap-specific information (if running in Snap)
  WriteLn('7. Snap Environment (if applicable):');
  if TNDXPlatform.IsSnap then
  begin
    WriteLn('   SNAP_USER_DATA: ', TNDXPlatform.GetSnapUserData);
    WriteLn('   SNAP_USER_COMMON: ', TNDXPlatform.GetSnapUserCommon);
    WriteLn('   SNAP_NAME: ', TNDXPlatform.GetSnapName);
    WriteLn('   SNAP path: ', TNDXPlatform.GetSnapPath);
  end
  else
    WriteLn('   Not running in Snap confinement.');
  WriteLn;

  // 8. SQLite library information
  WriteLn('8. SQLite Library:');
  if TNDXPlatform.LoadSQLiteLibrary then
  begin
    WriteLn('   Library loaded: Yes');
    WriteLn('   Version: ', TNDXPlatform.GetSQLiteVersion);
    WriteLn('   Version number: ', TNDXPlatform.GetSQLiteVersionNumber);
    WriteLn('   Loaded from: ', TNDXPlatform.GetLoadedLibraryPath);
    WriteLn('   Supports JSON1 (3.9+): ', TNDXPlatform.CheckSQLiteVersion(3009000));
    WriteLn('   Supports FTS5 (3.9+): ', TNDXPlatform.CheckSQLiteVersion(3009000));
    WriteLn('   Supports VACUUM INTO (3.27+): ', TNDXPlatform.CheckSQLiteVersion(3027000));
  end
  else
    WriteLn('   Could not load SQLite library!');
  WriteLn;

  // 9. Path utility examples
  WriteLn('9. Path Utilities:');
  WriteLn('   Path to URI: ', TNDXPlatform.PathToURI(TNDXPlatform.GetDataDirectory + 'test.db'));
  WriteLn('   SQL escaped path: ', TNDXPlatform.EscapeSQLPath('/path/with''quotes.db'));
  WriteLn;

  // 10. Platform-specific considerations
  WriteLn('10. Platform-Specific Recommendations:');
  WriteLn;

  {$IFDEF WINDOWS}
  WriteLn('   Windows:');
  WriteLn('   - Use AppData/Local for user data');
  WriteLn('   - Handle long paths (> 260 chars) with HandleLongPath()');
  WriteLn('   - Consider UAC for Program Files locations');
  WriteLn('   - sqlite3.dll typically in app directory or System32');
  WriteLn('   Example LOCALAPPDATA: ', GetEnvironmentVariable('LOCALAPPDATA'));
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('   Linux:');
  WriteLn('   - Follow XDG Base Directory specification');
  WriteLn('   - Data: $XDG_DATA_HOME or ~/.local/share');
  WriteLn('   - Config: $XDG_CONFIG_HOME or ~/.config');
  WriteLn('   - Handle Snap confinement with TNDXPlatform.IsSnap');
  WriteLn('   - Use ValidateDatabasePath() for Snap path remapping');
  WriteLn('   $HOME: ', GetEnvironmentVariable('HOME'));
  WriteLn('   $XDG_DATA_HOME: ', GetEnvironmentVariable('XDG_DATA_HOME'));
  {$ENDIF}

  {$IFDEF DARWIN}
  WriteLn('   macOS:');
  WriteLn('   - Use ~/Library/Application Support for data');
  WriteLn('   - Use ~/Library/Preferences for config');
  WriteLn('   - Handle App Sandbox with IsMacOSSandboxed');
  WriteLn('   - SQLite included in system (/usr/lib/libsqlite3.dylib)');
  WriteLn('   $HOME: ', GetEnvironmentVariable('HOME'));
  {$ENDIF}

  WriteLn;

  // 11. Best practices summary
  WriteLn('11. Cross-Platform Best Practices:');
  WriteLn('    - Use TNDXPlatform.GetDataDirectory for user data');
  WriteLn('    - Use TNDXPlatform.GetDefaultDatabasePath for DB files');
  WriteLn('    - Always call ValidateDatabasePath() before opening');
  WriteLn('    - Check IsPathWritable() before writing');
  WriteLn('    - Use NormalizePath() for consistent separators');
  WriteLn('    - Handle Snap/Sandbox path restrictions');
  WriteLn('    - Use PathDelim constant for path separators');
  WriteLn('    - Test on all target platforms');
  WriteLn;

  // 12. Creating directories safely
  WriteLn('12. Safe Directory Creation:');
  WriteLn('   Creating test directory...');
  if TNDXPlatform.EnsureDirectoryExists(TNDXPlatform.GetDataDirectory + 'NDXSQLiteExample') then
    WriteLn('   Directory created/verified: OK')
  else
    WriteLn('   Directory creation: FAILED (check permissions)');
  WriteLn;

  WriteLn('=== Example completed successfully! ===');
end.
