{===============================================================================
  NDXSQLite Example 147 - Version Information
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Getting NDXSQLite library version
  - Getting SQLite runtime version
  - Checking SQLite minimum version requirements
  - Comparing static vs runtime version information

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program VersionInfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  ndxsqliteversion, ndxsqliteplatform;

const
  { SQLite version requirements for various features }
  SQLITE_JSON_MIN = 3038000;      { 3.38.0 - JSON functions }
  SQLITE_RETURNING_MIN = 3035000; { 3.35.0 - RETURNING clause }
  SQLITE_MATH_MIN = 3035000;      { 3.35.0 - Math functions }
  SQLITE_STRICT_MIN = 3037000;    { 3.37.0 - STRICT tables }
  SQLITE_FTS5_MIN = 3009000;      { 3.9.0 - FTS5 }

procedure PrintSeparator;
begin
  WriteLn(StringOfChar('-', 60));
end;

procedure CheckFeature(const AFeatureName: string; AMinVersion: Integer);
var
  Supported: Boolean;
  VersionStr: string;
begin
  Supported := CheckSQLiteMinVersion(AMinVersion);
  VersionStr := Format('%d.%d.%d', [
    AMinVersion div 1000000,
    (AMinVersion div 1000) mod 1000,
    AMinVersion mod 1000
  ]);
  if Supported then
    WriteLn(Format('   [OK] %-25s (requires %s)', [AFeatureName, VersionStr]))
  else
    WriteLn(Format('   [--] %-25s (requires %s)', [AFeatureName, VersionStr]));
end;

begin
  WriteLn('=== NDXSQLite Example 147: Version Information ===');
  WriteLn;

  { Initialize SQLite library }
  TNDXPlatform.LoadSQLiteLibrary;

  { Section 1: NDXSQLite Library Version }
  PrintSeparator;
  WriteLn('1. NDXSQLite Library Version');
  PrintSeparator;
  WriteLn('   Version:       ', GetLibraryVersion);
  WriteLn('   Full Version:  ', GetLibraryVersionFull);
  WriteLn('   Major:         ', GetLibraryVersionMajor);
  WriteLn('   Minor:         ', GetLibraryVersionMinor);
  WriteLn('   Patch:         ', GetLibraryVersionPatch);
  WriteLn('   Release Date:  ', GetLibraryVersionDate);
  WriteLn;

  { Section 2: SQLite Version Comparison }
  PrintSeparator;
  WriteLn('2. SQLite Version Information');
  PrintSeparator;
  WriteLn('   Built for:     ', GetBuiltForSQLiteVersion, ' (static reference)');
  WriteLn('   Runtime:       ', GetRuntimeSQLiteVersion, ' (actual loaded library)');
  WriteLn('   Version Number: ', GetRuntimeSQLiteVersionNumber);
  WriteLn;

  { Section 3: Feature Support Check }
  PrintSeparator;
  WriteLn('3. SQLite Feature Support (based on runtime version)');
  PrintSeparator;
  CheckFeature('Full-Text Search (FTS5)', SQLITE_FTS5_MIN);
  CheckFeature('RETURNING clause', SQLITE_RETURNING_MIN);
  CheckFeature('Math functions', SQLITE_MATH_MIN);
  CheckFeature('STRICT tables', SQLITE_STRICT_MIN);
  CheckFeature('JSON functions', SQLITE_JSON_MIN);
  WriteLn;

  { Section 4: Platform Information }
  PrintSeparator;
  WriteLn('4. Platform Information');
  PrintSeparator;
  if TNDXPlatform.IsSQLiteLoaded then
    WriteLn('   Library Loaded: Yes')
  else
    WriteLn('   Library Loaded: No');
  {$IFDEF WINDOWS}
  WriteLn('   Platform:       Windows');
  {$ENDIF}
  {$IFDEF LINUX}
  WriteLn('   Platform:       Linux');
  {$ENDIF}
  {$IFDEF DARWIN}
  WriteLn('   Platform:       macOS');
  {$ENDIF}
  WriteLn;

  PrintSeparator;
  WriteLn('=== Example completed successfully! ===');
end.
