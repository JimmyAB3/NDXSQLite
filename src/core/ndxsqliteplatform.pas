{
  NDXSQLite - Cross-Platform Utilities
  =====================================

  Utilities for cross-platform compatibility:
  - Standard Linux (x64, ARM64, ARM32, i386)
  - Linux Snap (sandbox)
  - Linux Flatpak (sandbox)
  - Windows (x64, x86, ARM64)
  - macOS (Intel, Apple Silicon)

  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
}
unit ndxsqliteplatform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ndxsqlite3api;

type
  { Detected platform type }
  TNDXPlatformType = (
    ptLinux,        // Standard Linux
    ptLinuxSnap,    // Linux under Snap confinement
    ptLinuxFlatpak, // Linux under Flatpak confinement
    ptWindows,      // Windows
    ptMacOS,        // macOS (Intel or Apple Silicon)
    ptUnknown       // Unrecognized platform
  );

  { CPU Architecture }
  TNDXCPUArchitecture = (
    cpuX86_64,      // AMD64 / Intel 64-bit
    cpuX86,         // Intel 32-bit (i386/i686)
    cpuARM64,       // ARM 64-bit (aarch64)
    cpuARM32,       // ARM 32-bit (armhf/armel)
    cpuUnknown      // Unrecognized architecture
  );

  { Static class for platform utilities }
  TNDXPlatform = class
  private
    class var FPlatformType: TNDXPlatformType;
    class var FCPUArchitecture: TNDXCPUArchitecture;
    class var FSnapUserData: string;
    class var FSnapUserCommon: string;
    class var FSnapName: string;
    class var FSnapPath: string;
    class var FFlatpakId: string;
    class var FFlatpakAppDir: string;
    class var FInitialized: Boolean;
    class var FIsMacOSSandboxed: Boolean;
    class var FSQLiteLoaded: Boolean;
    class var FSQLiteLoadedPath: string;
    class var FSQLiteVersion: string;
    class var FSQLiteVersionNumber: Integer;
    class procedure Initialize;
    class procedure DetectCPUArchitecture;
    class procedure DetectMacOSSandbox;
    class function GetLinuxLibPaths: TStringArray;
    class function GetLinuxSnapLibPaths: TStringArray;
    class function GetLinuxFlatpakLibPaths: TStringArray;
    class function GetWindowsLibPaths: TStringArray;
    class function GetMacOSLibPaths: TStringArray;
  public
    { Platform detection }

    { Returns the detected platform type for the current system. }
    class function GetPlatformType: TNDXPlatformType;
    { Returns the detected CPU architecture. }
    class function GetCPUArchitecture: TNDXCPUArchitecture;
    { Returns True if running inside a Linux Snap confinement. }
    class function IsSnap: Boolean;
    { Returns True if running inside a Linux Flatpak confinement. }
    class function IsFlatpak: Boolean;
    { Returns True if running on Windows. }
    class function IsWindows: Boolean;
    { Returns True if running on Linux (standard, Snap or Flatpak). }
    class function IsLinux: Boolean;
    { Returns True if running on macOS. }
    class function IsMacOS: Boolean;
    { Returns True if running inside a macOS App Sandbox. }
    class function IsMacOSSandboxed: Boolean;
    { Returns a human-readable name for the detected platform. }
    class function PlatformName: string;
    { Returns a human-readable name for the CPU architecture. }
    class function ArchitectureName: string;

    { Path management }

    { Normalizes path separators and removes duplicates. }
    class function NormalizePath(const APath: string): string;
    { Adds long path prefix on Windows (\\?\); no-op on Unix. }
    class function HandleLongPath(const APath: string): string;
    { Converts a file path to a SQLite URI (file:// scheme). }
    class function PathToURI(const APath: string): string;
    { Escapes single quotes for safe use in SQL statements. }
    class function EscapeSQLString(const AValue: string): string;
    { Normalizes and escapes a path for SQL statements. }
    class function EscapeSQLPath(const APath: string): string;

    { Platform directories }

    { Returns the platform-appropriate application data directory. }
    class function GetDataDirectory: string;
    { Returns the platform-appropriate temporary directory. }
    class function GetTempDirectory: string;
    { Returns the platform-appropriate configuration directory. }
    class function GetConfigDirectory: string;
    { Returns the default full path for a database file name. }
    class function GetDefaultDatabasePath(const ADBName: string): string;
    { Returns the directory containing the current executable. }
    class function GetExecutableDirectory: string;

    { Snap specific }

    { Returns the SNAP_USER_DATA path, or empty if not in Snap. }
    class function GetSnapUserData: string;
    { Returns the SNAP_USER_COMMON path, or empty if not in Snap. }
    class function GetSnapUserCommon: string;
    { Returns the SNAP_NAME value, or empty if not in Snap. }
    class function GetSnapName: string;
    { Returns the SNAP base path, or empty if not in Snap. }
    class function GetSnapPath: string;

    { Flatpak specific }

    { Returns the FLATPAK_ID value, or empty if not in Flatpak. }
    class function GetFlatpakId: string;
    { Returns the Flatpak app data directory (~/.var/app/<FLATPAK_ID>), or empty if not in Flatpak. }
    class function GetFlatpakAppDir: string;
    { Returns the Flatpak data directory (~/.var/app/<FLATPAK_ID>/data), or empty if not in Flatpak. }
    class function GetFlatpakDataDir: string;
    { Returns the Flatpak config directory (~/.var/app/<FLATPAK_ID>/config), or empty if not in Flatpak. }
    class function GetFlatpakConfigDir: string;
    { Returns the Flatpak cache directory (~/.var/app/<FLATPAK_ID>/cache), or empty if not in Flatpak. }
    class function GetFlatpakCacheDir: string;

    { Path validation }

    { Returns True if the directory containing APath is writable. }
    class function IsPathWritable(const APath: string): Boolean;
    { Creates the directory if it does not exist. Returns True on success. }
    class function EnsureDirectoryExists(const APath: string): Boolean;
    { Validates and adjusts a database path for platform constraints (Snap, sandbox). }
    class function ValidateDatabasePath(const APath: string): string;

    { UTF-8 encoding }

    { Converts a native path string to UTF-8. }
    class function PathToUTF8(const APath: string): UTF8String;
    { Converts a UTF-8 string to a native path string. }
    class function UTF8ToPath(const AUTF8: UTF8String): string;

    { SQLite library }

    { Returns candidate file paths for the SQLite library on this platform. }
    class function GetSQLiteLibraryPaths: TStringArray;
    { Loads the SQLite shared library from ACustomPath or default locations. }
    class function LoadSQLiteLibrary(const ACustomPath: string = ''): Boolean;
    { Returns True if the SQLite library has been loaded. }
    class function IsSQLiteLoaded: Boolean;
    { Returns the loaded SQLite version string (e.g. '3.45.1'). }
    class function GetSQLiteVersion: string;
    { Returns the loaded SQLite version number (e.g. 3045001). }
    class function GetSQLiteVersionNumber: Integer;
    { Returns True if the loaded SQLite version is at least AMinVersion. }
    class function CheckSQLiteVersion(AMinVersion: Integer): Boolean;
    { Returns the file path from which the SQLite library was loaded. }
    class function GetLoadedLibraryPath: string;
  end;

implementation

{ TNDXPlatform }

class procedure TNDXPlatform.DetectCPUArchitecture;
begin
  // Detection based on Free Pascal compilation directives
  {$IF DEFINED(CPUX86_64) OR DEFINED(CPUAMD64)}
  FCPUArchitecture := cpuX86_64;
  {$ELSEIF DEFINED(CPUI386) OR DEFINED(CPU386) OR DEFINED(CPUX86)}
  FCPUArchitecture := cpuX86;
  {$ELSEIF DEFINED(CPUAARCH64) OR DEFINED(CPUARM64)}
  FCPUArchitecture := cpuARM64;
  {$ELSEIF DEFINED(CPUARM)}
  FCPUArchitecture := cpuARM32;
  {$ELSE}
  FCPUArchitecture := cpuUnknown;
  {$ENDIF}
end;

class procedure TNDXPlatform.DetectMacOSSandbox;
{$IFDEF DARWIN}
var
  SandboxContainer: string;
{$ENDIF}
begin
  FIsMacOSSandboxed := False;

  {$IFDEF DARWIN}
  // Check if the app is sandboxed via App Sandbox container
  SandboxContainer := GetEnvironmentVariable('APP_SANDBOX_CONTAINER_ID');
  if SandboxContainer <> '' then
    FIsMacOSSandboxed := True
  else
  begin
    // Also check HOME which is redirected in sandbox
    SandboxContainer := GetEnvironmentVariable('HOME');
    if Pos('/Library/Containers/', SandboxContainer) > 0 then
      FIsMacOSSandboxed := True;
  end;
  {$ENDIF}
end;

class procedure TNDXPlatform.Initialize;
var
  ContainerEnv: string;
begin
  if FInitialized then Exit;

  // Detect CPU architecture
  DetectCPUArchitecture;

  // Platform detection
  {$IFDEF MSWINDOWS}
  FPlatformType := ptWindows;
  {$ELSE}
    {$IFDEF DARWIN}
    FPlatformType := ptMacOS;
    DetectMacOSSandbox;
    {$ELSE}
      {$IFDEF UNIX}
      // Check for Flatpak environment first (FLATPAK_ID or container=flatpak)
      FFlatpakId := GetEnvironmentVariable('FLATPAK_ID');
      ContainerEnv := GetEnvironmentVariable('container');

      if (FFlatpakId <> '') or (ContainerEnv = 'flatpak') then
      begin
        FPlatformType := ptLinuxFlatpak;
        // Set Flatpak app directory (~/.var/app/<FLATPAK_ID>)
        if FFlatpakId <> '' then
          FFlatpakAppDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
                           '.var/app/' + FFlatpakId
        else
          FFlatpakAppDir := '';
      end
      else
      begin
        // Check if we are in a Snap environment
        // Use multiple variables for robust detection
        FSnapUserData := GetEnvironmentVariable('SNAP_USER_DATA');
        FSnapUserCommon := GetEnvironmentVariable('SNAP_USER_COMMON');
        FSnapName := GetEnvironmentVariable('SNAP_NAME');
        FSnapPath := GetEnvironmentVariable('SNAP');

        // Snap detection: SNAP_USER_DATA, SNAP_NAME or SNAP (fallback)
        if (FSnapUserData <> '') or (FSnapName <> '') or (FSnapPath <> '') then
          FPlatformType := ptLinuxSnap
        else
          FPlatformType := ptLinux;
      end;
      {$ELSE}
      FPlatformType := ptUnknown;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  FInitialized := True;
end;

class function TNDXPlatform.GetPlatformType: TNDXPlatformType;
begin
  Initialize;
  Result := FPlatformType;
end;

class function TNDXPlatform.GetCPUArchitecture: TNDXCPUArchitecture;
begin
  Initialize;
  Result := FCPUArchitecture;
end;

class function TNDXPlatform.IsSnap: Boolean;
begin
  Initialize;
  Result := FPlatformType = ptLinuxSnap;
end;

class function TNDXPlatform.IsFlatpak: Boolean;
begin
  Initialize;
  Result := FPlatformType = ptLinuxFlatpak;
end;

class function TNDXPlatform.IsWindows: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TNDXPlatform.IsLinux: Boolean;
begin
  Initialize;
  Result := FPlatformType in [ptLinux, ptLinuxSnap, ptLinuxFlatpak];
end;

class function TNDXPlatform.IsMacOS: Boolean;
begin
  {$IFDEF DARWIN}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TNDXPlatform.IsMacOSSandboxed: Boolean;
begin
  Initialize;
  Result := FIsMacOSSandboxed;
end;

class function TNDXPlatform.PlatformName: string;
begin
  Initialize;
  case FPlatformType of
    ptLinux: Result := 'Linux';
    ptLinuxSnap: Result := 'Linux (Snap: ' + FSnapName + ')';
    ptLinuxFlatpak: Result := 'Linux (Flatpak: ' + FFlatpakId + ')';
    ptWindows: Result := 'Windows';
    ptMacOS:
      begin
        if FIsMacOSSandboxed then
          Result := 'macOS (Sandboxed)'
        else
          Result := 'macOS';
      end;
  else
    Result := 'Unknown';
  end;
end;

class function TNDXPlatform.ArchitectureName: string;
begin
  Initialize;
  case FCPUArchitecture of
    cpuX86_64: Result := 'x86_64';
    cpuX86: Result := 'x86';
    cpuARM64: Result := 'ARM64';
    cpuARM32: Result := 'ARM32';
  else
    Result := 'Unknown';
  end;
end;

class function TNDXPlatform.NormalizePath(const APath: string): string;
begin
  if APath = '' then
  begin
    Result := '';
    Exit;
  end;

  // Use the native platform separator
  {$IFDEF MSWINDOWS}
  Result := StringReplace(APath, '/', '\', [rfReplaceAll]);

  // Remove double separators but preserve UNC
  // First, save if it's a UNC path
  if (Length(Result) >= 2) and (Result[1] = '\') and (Result[2] = '\') then
  begin
    // UNC path: \\server\share
    Result := '\\' + StringReplace(Copy(Result, 3, MaxInt), '\\', '\', [rfReplaceAll]);
  end
  else
  begin
    while Pos('\\', Result) > 0 do
      Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
  end;
  {$ELSE}
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
  while Pos('//', Result) > 0 do
    Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
  {$ENDIF}
end;

class function TNDXPlatform.HandleLongPath(const APath: string): string;
{$IFDEF MSWINDOWS}
const
  MAX_PATH_LIMIT = 260;
  LONG_PATH_PREFIX = '\\?\';
  UNC_LONG_PREFIX = '\\?\UNC\';
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  // Windows: add \\?\ prefix for paths > 260 characters
  if Length(APath) >= MAX_PATH_LIMIT then
  begin
    // Check if prefix is already present
    if Copy(APath, 1, 4) = LONG_PATH_PREFIX then
    begin
      Result := APath;
      Exit;
    end;

    // UNC path: \\server\share -> \\?\UNC\server\share
    if (Length(APath) >= 2) and (APath[1] = '\') and (APath[2] = '\') then
      Result := UNC_LONG_PREFIX + Copy(APath, 3, MaxInt)
    else
      Result := LONG_PATH_PREFIX + APath;
  end
  else
    Result := APath;
  {$ELSE}
  // Linux/macOS: no practical limit, return as-is
  Result := APath;
  {$ENDIF}
end;

class function TNDXPlatform.PathToURI(const APath: string): string;
var
  NormalizedPath: string;
  I: Integer;
  C: Char;
  ResultPath: string;
begin
  // SQLite URI always uses forward slashes
  NormalizedPath := StringReplace(APath, '\', '/', [rfReplaceAll]);

  // Escape special characters according to RFC 3986
  // Unreserved characters (safe): A-Z a-z 0-9 - . _ ~
  // Reserved characters to escape: everything else except / and :
  ResultPath := '';
  for I := 1 to Length(NormalizedPath) do
  begin
    C := NormalizedPath[I];
    case C of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~', '/', ':':
        ResultPath := ResultPath + C;
      ' ':
        ResultPath := ResultPath + '%20';
      '#':
        ResultPath := ResultPath + '%23';
      '%':
        ResultPath := ResultPath + '%25';
      '&':
        ResultPath := ResultPath + '%26';
      '''':
        ResultPath := ResultPath + '%27';
      '(':
        ResultPath := ResultPath + '%28';
      ')':
        ResultPath := ResultPath + '%29';
      '*':
        ResultPath := ResultPath + '%2A';
      '+':
        ResultPath := ResultPath + '%2B';
      ',':
        ResultPath := ResultPath + '%2C';
      ';':
        ResultPath := ResultPath + '%3B';
      '=':
        ResultPath := ResultPath + '%3D';
      '?':
        ResultPath := ResultPath + '%3F';
      '@':
        ResultPath := ResultPath + '%40';
      '[':
        ResultPath := ResultPath + '%5B';
      ']':
        ResultPath := ResultPath + '%5D';
      '{':
        ResultPath := ResultPath + '%7B';
      '|':
        ResultPath := ResultPath + '%7C';
      '}':
        ResultPath := ResultPath + '%7D';
    else
      // For characters > 127 (UTF-8), encode in hex
      if Ord(C) > 127 then
        ResultPath := ResultPath + '%' + IntToHex(Ord(C), 2)
      else
        ResultPath := ResultPath + C;
    end;
  end;

  NormalizedPath := ResultPath;

  {$IFDEF MSWINDOWS}
  // Windows: file:///C:/path/to/db.sqlite
  if (Length(NormalizedPath) >= 2) and (NormalizedPath[2] = ':') then
    Result := 'file:///' + NormalizedPath
  else if (Length(NormalizedPath) >= 2) and (NormalizedPath[1] = '/') and (NormalizedPath[2] = '/') then
    // UNC path: //server/share -> file://server/share
    Result := 'file:' + NormalizedPath
  else
    Result := 'file://' + NormalizedPath;
  {$ELSE}
  // Unix: file:///path/to/db.sqlite
  if (Length(NormalizedPath) > 0) and (NormalizedPath[1] = '/') then
    Result := 'file://' + NormalizedPath
  else
    Result := 'file:///' + NormalizedPath;
  {$ENDIF}
end;

class function TNDXPlatform.EscapeSQLString(const AValue: string): string;
begin
  // Double apostrophes to escape in SQL
  Result := StringReplace(AValue, '''', '''''', [rfReplaceAll]);
end;

class function TNDXPlatform.EscapeSQLPath(const APath: string): string;
begin
  // Normalize then escape for SQL
  Result := EscapeSQLString(NormalizePath(APath));
end;

class function TNDXPlatform.GetDataDirectory: string;
begin
  Initialize;

  case FPlatformType of
    ptLinuxSnap:
      begin
        // Snap: use SNAP_USER_DATA or SNAP_USER_COMMON
        if FSnapUserData <> '' then
          Result := FSnapUserData
        else if FSnapUserCommon <> '' then
          Result := FSnapUserCommon
        else
          Result := GetEnvironmentVariable('HOME');
      end;
    ptLinuxFlatpak:
      begin
        // Flatpak: use XDG_DATA_HOME which is redirected to ~/.var/app/<ID>/data
        Result := GetEnvironmentVariable('XDG_DATA_HOME');
        if Result = '' then
        begin
          // Fallback to constructed path
          if FFlatpakAppDir <> '' then
            Result := IncludeTrailingPathDelimiter(FFlatpakAppDir) + 'data'
          else
            Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.local/share';
        end;
      end;
    ptLinux:
      begin
        // Standard Linux: XDG_DATA_HOME or ~/.local/share
        Result := GetEnvironmentVariable('XDG_DATA_HOME');
        if Result = '' then
          Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.local/share';
      end;
    ptWindows:
      begin
        // Windows: LOCALAPPDATA or APPDATA
        Result := GetEnvironmentVariable('LOCALAPPDATA');
        if Result = '' then
          Result := GetEnvironmentVariable('APPDATA');
        if Result = '' then
          Result := GetEnvironmentVariable('USERPROFILE');
      end;
    ptMacOS:
      begin
        if FIsMacOSSandboxed then
          // Sandboxed: HOME already points to the container
          Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + 'Library/Application Support'
        else
          Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + 'Library/Application Support';
      end;
  else
    Result := GetCurrentDir;
  end;

  Result := IncludeTrailingPathDelimiter(Result);
end;

class function TNDXPlatform.GetTempDirectory: string;
var
  TempCreated: Boolean;
begin
  Initialize;
  TempCreated := False;

  case FPlatformType of
    ptLinuxSnap:
      begin
        // Snap: use SNAP_USER_DATA/tmp or SNAP_USER_COMMON/tmp
        if FSnapUserData <> '' then
        begin
          Result := IncludeTrailingPathDelimiter(FSnapUserData) + 'tmp';
          TempCreated := EnsureDirectoryExists(Result);
        end;
        if (not TempCreated) and (FSnapUserCommon <> '') then
        begin
          Result := IncludeTrailingPathDelimiter(FSnapUserCommon) + 'tmp';
          TempCreated := EnsureDirectoryExists(Result);
        end;
        if not TempCreated then
          Result := '/tmp';
      end;
    ptLinuxFlatpak:
      begin
        // Flatpak: use XDG_CACHE_HOME/tmp or fallback to app cache directory
        Result := GetEnvironmentVariable('XDG_CACHE_HOME');
        if Result <> '' then
        begin
          Result := IncludeTrailingPathDelimiter(Result) + 'tmp';
          TempCreated := EnsureDirectoryExists(Result);
        end;
        if (not TempCreated) and (FFlatpakAppDir <> '') then
        begin
          Result := IncludeTrailingPathDelimiter(FFlatpakAppDir) + 'cache/tmp';
          TempCreated := EnsureDirectoryExists(Result);
        end;
        if not TempCreated then
        begin
          // Flatpak has access to /tmp but it's isolated
          Result := GetEnvironmentVariable('TMPDIR');
          if Result = '' then
            Result := '/tmp';
        end;
      end;
    ptLinux, ptMacOS:
      begin
        Result := GetEnvironmentVariable('TMPDIR');
        if Result = '' then
          Result := '/tmp';
      end;
    ptWindows:
      begin
        Result := GetEnvironmentVariable('TEMP');
        if Result = '' then
          Result := GetEnvironmentVariable('TMP');
        if Result = '' then
        begin
          Result := GetEnvironmentVariable('LOCALAPPDATA');
          if Result <> '' then
            Result := IncludeTrailingPathDelimiter(Result) + 'Temp';
        end;
        if Result = '' then
          Result := GetEnvironmentVariable('USERPROFILE');
      end;
  else
    Result := GetCurrentDir;
  end;

  Result := IncludeTrailingPathDelimiter(Result);

  // Verify the directory exists and is accessible
  if not DirectoryExists(ExcludeTrailingPathDelimiter(Result)) then
    EnsureDirectoryExists(Result);
end;

class function TNDXPlatform.GetConfigDirectory: string;
begin
  Initialize;

  case FPlatformType of
    ptLinuxSnap:
      begin
        if FSnapUserData <> '' then
          Result := FSnapUserData
        else
          Result := GetEnvironmentVariable('HOME');
      end;
    ptLinuxFlatpak:
      begin
        // Flatpak: use XDG_CONFIG_HOME which is redirected to ~/.var/app/<ID>/config
        Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
        if Result = '' then
        begin
          // Fallback to constructed path
          if FFlatpakAppDir <> '' then
            Result := IncludeTrailingPathDelimiter(FFlatpakAppDir) + 'config'
          else
            Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config';
        end;
      end;
    ptLinux:
      begin
        Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
        if Result = '' then
          Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.config';
      end;
    ptWindows:
      begin
        Result := GetEnvironmentVariable('APPDATA');
        if Result = '' then
          Result := GetEnvironmentVariable('USERPROFILE');
      end;
    ptMacOS:
      begin
        Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
                  'Library/Preferences';
      end;
  else
    Result := GetCurrentDir;
  end;

  Result := IncludeTrailingPathDelimiter(Result);
end;

class function TNDXPlatform.GetExecutableDirectory: string;
begin
  Result := ExtractFilePath(ParamStr(0));
  if Result = '' then
    Result := GetCurrentDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

class function TNDXPlatform.GetDefaultDatabasePath(const ADBName: string): string;
var
  BaseDir: string;
begin
  Initialize;

  // For Snap and Flatpak, force use of sandboxed directory
  if FPlatformType in [ptLinuxSnap, ptLinuxFlatpak] then
  begin
    BaseDir := GetDataDirectory;
    Result := BaseDir + ADBName;
  end
  else
  begin
    // For other platforms, use the path as-is if it's absolute
    if (Length(ADBName) > 0) and
       ((ADBName[1] = '/') or
        ((Length(ADBName) > 1) and (ADBName[2] = ':')) or
        ((Length(ADBName) >= 2) and (ADBName[1] = '\') and (ADBName[2] = '\'))) then
      Result := ADBName
    else
      Result := GetDataDirectory + ADBName;
  end;

  Result := NormalizePath(Result);
end;

class function TNDXPlatform.GetSnapUserData: string;
begin
  Initialize;
  Result := FSnapUserData;
end;

class function TNDXPlatform.GetSnapUserCommon: string;
begin
  Initialize;
  Result := FSnapUserCommon;
end;

class function TNDXPlatform.GetSnapName: string;
begin
  Initialize;
  Result := FSnapName;
end;

class function TNDXPlatform.GetSnapPath: string;
begin
  Initialize;
  Result := FSnapPath;
end;

class function TNDXPlatform.GetFlatpakId: string;
begin
  Initialize;
  Result := FFlatpakId;
end;

class function TNDXPlatform.GetFlatpakAppDir: string;
begin
  Initialize;
  Result := FFlatpakAppDir;
end;

class function TNDXPlatform.GetFlatpakDataDir: string;
begin
  Initialize;
  if FFlatpakAppDir <> '' then
    Result := IncludeTrailingPathDelimiter(FFlatpakAppDir) + 'data'
  else
    Result := '';
end;

class function TNDXPlatform.GetFlatpakConfigDir: string;
begin
  Initialize;
  if FFlatpakAppDir <> '' then
    Result := IncludeTrailingPathDelimiter(FFlatpakAppDir) + 'config'
  else
    Result := '';
end;

class function TNDXPlatform.GetFlatpakCacheDir: string;
begin
  Initialize;
  if FFlatpakAppDir <> '' then
    Result := IncludeTrailingPathDelimiter(FFlatpakAppDir) + 'cache'
  else
    Result := '';
end;

class function TNDXPlatform.IsPathWritable(const APath: string): Boolean;
var
  TestFile: string;
  F: TextFile;
  Dir: string;
begin
  Result := False;

  // Extract the directory
  Dir := ExtractFilePath(APath);
  if Dir = '' then
    Dir := GetCurrentDir;

  // Check if the directory exists
  if not DirectoryExists(Dir) then
    Exit;

  // macOS sandbox: check if we can write
  {$IFDEF DARWIN}
  if FIsMacOSSandboxed then
  begin
    // In sandbox, only certain directories are accessible
    // Check if the path is in an allowed directory
    if (Pos('/Library/Containers/', Dir) = 0) and
       (Pos(GetEnvironmentVariable('HOME'), Dir) = 0) then
    begin
      Result := False;
      Exit;
    end;
  end;
  {$ENDIF}

  // Test writing with a temporary file
  TestFile := IncludeTrailingPathDelimiter(Dir) + '.ndx_write_test_' +
              IntToStr(Random(100000)) + '.tmp';
  try
    AssignFile(F, TestFile);
    {$I-}
    Rewrite(F);
    {$I+}
    if IOResult = 0 then
    begin
      CloseFile(F);
      DeleteFile(TestFile);
      Result := True;
    end;
  except
    Result := False;
  end;
end;

class function TNDXPlatform.EnsureDirectoryExists(const APath: string): Boolean;
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(APath);
  if Dir = '' then
  begin
    Result := False;
    Exit;
  end;

  if DirectoryExists(Dir) then
    Result := True
  else
  begin
    Result := ForceDirectories(Dir);
    // Verify that creation succeeded
    if Result then
      Result := DirectoryExists(Dir);
  end;
end;

class function TNDXPlatform.ValidateDatabasePath(const APath: string): string;
var
  Dir, RelPath, TempDir, ParentDir, GrandParentDir: string;
  Level: Integer;
begin
  Initialize;

  if APath = '' then
  begin
    Result := '';
    Exit;
  end;

  Result := NormalizePath(APath);

  // For Snap, verify the path is in an allowed directory
  if FPlatformType = ptLinuxSnap then
  begin
    // If path is not in SNAP_USER_DATA or SNAP_USER_COMMON, redirect it
    if (FSnapUserData <> '') and (Pos(FSnapUserData, Result) <> 1) and
       ((FSnapUserCommon = '') or (Pos(FSnapUserCommon, Result) <> 1)) then
    begin
      // Preserve up to 3 directory levels to keep the structure
      RelPath := ExtractFileName(APath);
      Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(APath));
      Level := 0;

      // Level 1: parent directory
      if (Dir <> '') and (Dir <> '/') and (Dir <> '\') then
      begin
        ParentDir := ExtractFileName(Dir);
        if ParentDir <> '' then
        begin
          RelPath := ParentDir + PathDelim + RelPath;
          Inc(Level);
          TempDir := ExcludeTrailingPathDelimiter(ExtractFilePath(Dir));

          // Level 2: grandparent directory
          if (Level < 3) and (TempDir <> '') and (TempDir <> '/') and (TempDir <> '\') then
          begin
            GrandParentDir := ExtractFileName(TempDir);
            if GrandParentDir <> '' then
            begin
              RelPath := GrandParentDir + PathDelim + RelPath;
              Inc(Level);
              TempDir := ExcludeTrailingPathDelimiter(ExtractFilePath(TempDir));

              // Level 3: great-grandparent directory
              if (Level < 3) and (TempDir <> '') and (TempDir <> '/') and (TempDir <> '\') then
              begin
                ParentDir := ExtractFileName(TempDir);
                if ParentDir <> '' then
                  RelPath := ParentDir + PathDelim + RelPath;
              end;
            end;
          end;
        end;
      end;

      Result := IncludeTrailingPathDelimiter(FSnapUserData) + RelPath;
    end;
  end;

  // For Flatpak, verify the path is in the app directory
  if FPlatformType = ptLinuxFlatpak then
  begin
    // If path is not in the Flatpak app directory, redirect it
    if (FFlatpakAppDir <> '') and (Pos(FFlatpakAppDir, Result) <> 1) then
    begin
      // Preserve up to 3 directory levels to keep the structure
      RelPath := ExtractFileName(APath);
      Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(APath));
      Level := 0;

      // Level 1: parent directory
      if (Dir <> '') and (Dir <> '/') and (Dir <> '\') then
      begin
        ParentDir := ExtractFileName(Dir);
        if ParentDir <> '' then
        begin
          RelPath := ParentDir + PathDelim + RelPath;
          Inc(Level);
          TempDir := ExcludeTrailingPathDelimiter(ExtractFilePath(Dir));

          // Level 2: grandparent directory
          if (Level < 3) and (TempDir <> '') and (TempDir <> '/') and (TempDir <> '\') then
          begin
            GrandParentDir := ExtractFileName(TempDir);
            if GrandParentDir <> '' then
            begin
              RelPath := GrandParentDir + PathDelim + RelPath;
              Inc(Level);
              TempDir := ExcludeTrailingPathDelimiter(ExtractFilePath(TempDir));

              // Level 3: great-grandparent directory
              if (Level < 3) and (TempDir <> '') and (TempDir <> '/') and (TempDir <> '\') then
              begin
                ParentDir := ExtractFileName(TempDir);
                if ParentDir <> '' then
                  RelPath := ParentDir + PathDelim + RelPath;
              end;
            end;
          end;
        end;
      end;

      // Redirect to Flatpak data directory
      Result := IncludeTrailingPathDelimiter(FFlatpakAppDir) + 'data/' + RelPath;
    end;
  end;

  // For sandboxed macOS, verify the path
  {$IFDEF DARWIN}
  if FIsMacOSSandboxed then
  begin
    // If path is not in the container, redirect to Application Support
    // preserving the relative structure
    if Pos(GetEnvironmentVariable('HOME'), Result) <> 1 then
    begin
      // Preserve filename and up to 2 directory levels
      RelPath := ExtractFileName(APath);
      Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(APath));
      if (Dir <> '') and (Dir <> '/') then
      begin
        ParentDir := ExtractFileName(Dir);
        if ParentDir <> '' then
          RelPath := ParentDir + PathDelim + RelPath;
      end;
      Result := GetDataDirectory + RelPath;
    end;
  end;
  {$ENDIF}

  // Ensure parent directory exists
  Dir := ExtractFilePath(Result);
  if (Dir <> '') and (not DirectoryExists(Dir)) then
  begin
    if not EnsureDirectoryExists(Dir) then
    begin
      // If we cannot create the directory, use the temp directory
      Result := GetTempDirectory + ExtractFileName(APath);
    end;
  end;
end;

class function TNDXPlatform.PathToUTF8(const APath: string): UTF8String;
begin
  // In Free Pascal with {$H+}, string is already UTF-8 on modern platforms
  // But we ensure explicit conversion
  {$IFDEF FPC}
  Result := UTF8Encode(APath);
  {$ELSE}
  Result := UTF8String(APath);
  {$ENDIF}
end;

class function TNDXPlatform.UTF8ToPath(const AUTF8: UTF8String): string;
begin
  {$IFDEF FPC}
  Result := UTF8Decode(AUTF8);
  {$ELSE}
  Result := string(AUTF8);
  {$ENDIF}
end;

class function TNDXPlatform.GetLinuxLibPaths: TStringArray;
var
  ArchDir: string;
begin
  // Determine architecture directory
  case FCPUArchitecture of
    cpuX86_64: ArchDir := 'x86_64-linux-gnu';
    cpuX86: ArchDir := 'i386-linux-gnu';
    cpuARM64: ArchDir := 'aarch64-linux-gnu';
    cpuARM32: ArchDir := 'arm-linux-gnueabihf';
  else
    ArchDir := '';
  end;

  if ArchDir <> '' then
  begin
    SetLength(Result, 16);
    // SQLCipher first (for encryption support)
    Result[0] := GetExecutableDirectory + 'libsqlcipher.so.1';
    Result[1] := '/usr/lib/' + ArchDir + '/libsqlcipher.so.1';
    Result[2] := '/lib/' + ArchDir + '/libsqlcipher.so.1';
    Result[3] := '/usr/lib/libsqlcipher.so.1';
    Result[4] := '/usr/local/lib/libsqlcipher.so.1';
    Result[5] := 'libsqlcipher.so.1';
    // Standard SQLite fallback
    Result[6] := GetExecutableDirectory + 'libsqlite3.so.0';
    Result[7] := GetExecutableDirectory + 'libsqlite3.so';
    Result[8] := '/usr/lib/' + ArchDir + '/libsqlite3.so.0';
    Result[9] := '/lib/' + ArchDir + '/libsqlite3.so.0';
    Result[10] := '/usr/lib/libsqlite3.so.0';
    Result[11] := '/usr/local/lib/libsqlite3.so.0';
    Result[12] := '/lib/libsqlite3.so.0';
    Result[13] := '/usr/lib/libsqlite3.so';
    Result[14] := '/usr/local/lib/libsqlite3.so';
    Result[15] := 'libsqlite3.so.0';
  end
  else
  begin
    SetLength(Result, 10);
    // SQLCipher first (for encryption support)
    Result[0] := GetExecutableDirectory + 'libsqlcipher.so.1';
    Result[1] := '/usr/lib/libsqlcipher.so.1';
    Result[2] := '/usr/local/lib/libsqlcipher.so.1';
    Result[3] := 'libsqlcipher.so.1';
    // Standard SQLite fallback
    Result[4] := GetExecutableDirectory + 'libsqlite3.so.0';
    Result[5] := '/usr/lib/libsqlite3.so.0';
    Result[6] := '/usr/local/lib/libsqlite3.so.0';
    Result[7] := '/lib/libsqlite3.so.0';
    Result[8] := '/usr/lib/libsqlite3.so';
    Result[9] := 'libsqlite3.so.0';
  end;
end;

class function TNDXPlatform.GetLinuxSnapLibPaths: TStringArray;
var
  ArchDir: string;
  SnapBase: string;
  I: Integer;
begin
  // Determine architecture directory
  case FCPUArchitecture of
    cpuX86_64: ArchDir := 'x86_64-linux-gnu';
    cpuX86: ArchDir := 'i386-linux-gnu';
    cpuARM64: ArchDir := 'aarch64-linux-gnu';
    cpuARM32: ArchDir := 'arm-linux-gnueabihf';
  else
    ArchDir := '';
  end;

  // Get SNAP path safely
  SnapBase := FSnapPath;

  SetLength(Result, 24);
  I := 0;

  // SQLCipher first (for encryption support)
  Result[I] := GetExecutableDirectory + 'libsqlcipher.so.1';
  Inc(I);
  if ArchDir <> '' then
  begin
    Result[I] := '/usr/lib/' + ArchDir + '/libsqlcipher.so.1';
    Inc(I);
    Result[I] := '/lib/' + ArchDir + '/libsqlcipher.so.1';
    Inc(I);
  end;
  Result[I] := '/usr/lib/libsqlcipher.so.1';
  Inc(I);
  Result[I] := '/usr/local/lib/libsqlcipher.so.1';
  Inc(I);
  Result[I] := 'libsqlcipher.so.1';
  Inc(I);

  // Standard SQLite paths
  // First, paths relative to executable
  Result[I] := GetExecutableDirectory + 'libsqlite3.so.0';
  Inc(I);

  // Then Snap paths (if SNAP is defined)
  if SnapBase <> '' then
  begin
    Result[I] := SnapBase + '/usr/lib/libsqlite3.so.0';
    Inc(I);
    Result[I] := SnapBase + '/lib/libsqlite3.so.0';
    Inc(I);
    if ArchDir <> '' then
    begin
      Result[I] := SnapBase + '/usr/lib/' + ArchDir + '/libsqlite3.so.0';
      Inc(I);
    end;
  end;

  // Then system paths (if interface allows)
  if ArchDir <> '' then
  begin
    Result[I] := '/usr/lib/' + ArchDir + '/libsqlite3.so.0';
    Inc(I);
    Result[I] := '/lib/' + ArchDir + '/libsqlite3.so.0';
    Inc(I);
  end;

  Result[I] := '/usr/lib/libsqlite3.so.0';
  Inc(I);
  Result[I] := '/lib/libsqlite3.so.0';
  Inc(I);
  Result[I] := 'libsqlite3.so.0';
  Inc(I);

  // Adjust size
  SetLength(Result, I);
end;

class function TNDXPlatform.GetLinuxFlatpakLibPaths: TStringArray;
var
  ArchDir: string;
  I: Integer;
begin
  // Determine architecture directory
  case FCPUArchitecture of
    cpuX86_64: ArchDir := 'x86_64-linux-gnu';
    cpuX86: ArchDir := 'i386-linux-gnu';
    cpuARM64: ArchDir := 'aarch64-linux-gnu';
    cpuARM32: ArchDir := 'arm-linux-gnueabihf';
  else
    ArchDir := '';
  end;

  SetLength(Result, 24);
  I := 0;

  // SQLCipher first (for encryption support)
  Result[I] := GetExecutableDirectory + 'libsqlcipher.so.1';
  Inc(I);
  Result[I] := '/app/lib/libsqlcipher.so.1';
  Inc(I);
  if ArchDir <> '' then
  begin
    Result[I] := '/app/lib/' + ArchDir + '/libsqlcipher.so.1';
    Inc(I);
    Result[I] := '/usr/lib/' + ArchDir + '/libsqlcipher.so.1';
    Inc(I);
  end;
  Result[I] := '/usr/lib/libsqlcipher.so.1';
  Inc(I);
  Result[I] := '/usr/local/lib/libsqlcipher.so.1';
  Inc(I);
  Result[I] := 'libsqlcipher.so.1';
  Inc(I);

  // Standard SQLite paths
  // First, paths relative to executable
  Result[I] := GetExecutableDirectory + 'libsqlite3.so.0';
  Inc(I);

  // Flatpak app-bundled libraries
  Result[I] := '/app/lib/libsqlite3.so.0';
  Inc(I);
  Result[I] := '/app/lib/libsqlite3.so';
  Inc(I);

  // Flatpak runtime libraries
  if ArchDir <> '' then
  begin
    Result[I] := '/app/lib/' + ArchDir + '/libsqlite3.so.0';
    Inc(I);
    Result[I] := '/usr/lib/' + ArchDir + '/libsqlite3.so.0';
    Inc(I);
    Result[I] := '/lib/' + ArchDir + '/libsqlite3.so.0';
    Inc(I);
  end;

  // Standard paths (within the Flatpak container)
  Result[I] := '/usr/lib/libsqlite3.so.0';
  Inc(I);
  Result[I] := '/lib/libsqlite3.so.0';
  Inc(I);
  Result[I] := '/usr/lib/libsqlite3.so';
  Inc(I);
  Result[I] := 'libsqlite3.so.0';
  Inc(I);

  // Adjust size
  SetLength(Result, I);
end;

class function TNDXPlatform.GetWindowsLibPaths: TStringArray;
var
  SystemRoot, ProgramFiles, ProgramFilesX86: string;
  ExeDir: string;
  PathEnv, PathDir: string;
  PathList: TStringList;
  I, J: Integer;
begin
  SystemRoot := GetEnvironmentVariable('SYSTEMROOT');
  ProgramFiles := GetEnvironmentVariable('PROGRAMFILES');
  ProgramFilesX86 := GetEnvironmentVariable('PROGRAMFILES(X86)');
  ExeDir := GetExecutableDirectory;
  PathEnv := GetEnvironmentVariable('PATH');

  // Start with base paths
  SetLength(Result, 30);
  I := 0;

  // SQLCipher first (for encryption support)
  Result[I] := ExeDir + 'sqlcipher.dll';
  Inc(I);
  Result[I] := 'sqlcipher.dll';
  Inc(I);
  if SystemRoot <> '' then
  begin
    {$IF DEFINED(CPUX86_64) OR DEFINED(CPUAMD64)}
    Result[I] := SystemRoot + '\System32\sqlcipher.dll';
    Inc(I);
    {$ELSE}
    Result[I] := SystemRoot + '\SysWOW64\sqlcipher.dll';
    Inc(I);
    Result[I] := SystemRoot + '\System32\sqlcipher.dll';
    Inc(I);
    {$ENDIF}
  end;

  // Standard SQLite paths
  // 1. Executable directory (highest priority)
  Result[I] := ExeDir + 'sqlite3.dll';
  Inc(I);

  // 2. Simple name (standard Windows search)
  Result[I] := 'sqlite3.dll';
  Inc(I);

  // 3. System32 and SysWOW64 depending on architecture
  if SystemRoot <> '' then
  begin
    {$IF DEFINED(CPUX86_64) OR DEFINED(CPUAMD64)}
    // 64-bit application
    Result[I] := SystemRoot + '\System32\sqlite3.dll';
    Inc(I);
    {$ELSE}
    // 32-bit application - may be on 64-bit Windows (SysWOW64) or 32-bit
    Result[I] := SystemRoot + '\SysWOW64\sqlite3.dll';
    Inc(I);
    Result[I] := SystemRoot + '\System32\sqlite3.dll';
    Inc(I);
    {$ENDIF}
  end;

  // 4. Program Files
  if ProgramFiles <> '' then
  begin
    Result[I] := ProgramFiles + '\SQLite\sqlite3.dll';
    Inc(I);
  end;

  if ProgramFilesX86 <> '' then
  begin
    Result[I] := ProgramFilesX86 + '\SQLite\sqlite3.dll';
    Inc(I);
  end;

  // 5. Browse the PATH
  if PathEnv <> '' then
  begin
    PathList := TStringList.Create;
    try
      PathList.Delimiter := ';';
      PathList.StrictDelimiter := True;
      PathList.DelimitedText := PathEnv;

      for J := 0 to PathList.Count - 1 do
      begin
        PathDir := Trim(PathList[J]);
        if (PathDir <> '') and (I < Length(Result) - 1) then
        begin
          Result[I] := IncludeTrailingPathDelimiter(PathDir) + 'sqlite3.dll';
          Inc(I);
        end;
      end;
    finally
      PathList.Free;
    end;
  end;

  // Adjust size
  SetLength(Result, I);
end;

class function TNDXPlatform.GetMacOSLibPaths: TStringArray;
begin
  SetLength(Result, 20);

  // SQLCipher first (for encryption support)
  Result[0] := GetExecutableDirectory + 'libsqlcipher.dylib';
  Result[1] := GetExecutableDirectory + '../Frameworks/libsqlcipher.dylib';
  Result[2] := '/usr/local/lib/libsqlcipher.dylib';
  Result[3] := '/usr/local/opt/sqlcipher/lib/libsqlcipher.dylib';
  Result[4] := '/opt/homebrew/lib/libsqlcipher.dylib';
  Result[5] := '/opt/homebrew/opt/sqlcipher/lib/libsqlcipher.dylib';
  Result[6] := '/opt/local/lib/libsqlcipher.dylib';
  Result[7] := 'libsqlcipher.dylib';

  // Standard SQLite paths
  // 1. Executable directory (for bundled apps)
  Result[8] := GetExecutableDirectory + 'libsqlite3.dylib';
  Result[9] := GetExecutableDirectory + '../Frameworks/libsqlite3.dylib';

  // 2. macOS system (SQLite is included)
  Result[10] := '/usr/lib/libsqlite3.dylib';

  // 3. Homebrew (Intel Mac)
  Result[11] := '/usr/local/lib/libsqlite3.dylib';
  Result[12] := '/usr/local/opt/sqlite/lib/libsqlite3.dylib';

  // 4. Homebrew (Apple Silicon M1/M2/M3)
  Result[13] := '/opt/homebrew/lib/libsqlite3.dylib';
  Result[14] := '/opt/homebrew/opt/sqlite/lib/libsqlite3.dylib';

  // 5. MacPorts
  Result[15] := '/opt/local/lib/libsqlite3.dylib';

  // 6. Fink
  Result[16] := '/sw/lib/libsqlite3.dylib';

  // 7. Generic paths
  Result[17] := '/Library/Frameworks/SQLite3.framework/SQLite3';
  Result[18] := 'libsqlite3.dylib';
  Result[19] := 'libsqlite3.0.dylib';
end;

class function TNDXPlatform.GetSQLiteLibraryPaths: TStringArray;
begin
  Initialize;

  case FPlatformType of
    ptWindows:
      Result := GetWindowsLibPaths;
    ptLinuxSnap:
      Result := GetLinuxSnapLibPaths;
    ptLinuxFlatpak:
      Result := GetLinuxFlatpakLibPaths;
    ptLinux:
      Result := GetLinuxLibPaths;
    ptMacOS:
      Result := GetMacOSLibPaths;
  else
    SetLength(Result, 1);
    Result[0] := 'sqlite3';
  end;
end;

class function TNDXPlatform.LoadSQLiteLibrary(const ACustomPath: string): Boolean;
var
  Paths: TStringArray;
  I: Integer;
  LibPath: string;
  VerStr: PAnsiChar;
begin
  // If already marked as loaded, return True
  if FSQLiteLoaded then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

  // Check if SQLite is already loaded by ndxsqlite3api
  if IsSQLite3LibraryLoaded then
  begin
    // Already loaded
    FSQLiteLoaded := True;
    FSQLiteLoadedPath := '(loaded by ndxsqlite3api)';
    VerStr := sqlite3_libversion();
    if VerStr <> nil then
      FSQLiteVersion := string(VerStr)
    else
      FSQLiteVersion := 'unknown';
    FSQLiteVersionNumber := sqlite3_libversion_number();
    Result := True;
    Exit;
  end;

  // If a custom path is provided, try it first
  if ACustomPath <> '' then
  begin
    LibPath := NormalizePath(ACustomPath);
    if FileExists(LibPath) then
    begin
      if ndxsqlite3api.LoadSQLite3Library(LibPath) then
      begin
        FSQLiteLoaded := True;
        FSQLiteLoadedPath := LibPath;
        Result := True;
      end;
    end;
  end;

  // If not yet loaded, try default paths
  if not FSQLiteLoaded then
  begin
    Paths := GetSQLiteLibraryPaths;
    for I := 0 to High(Paths) do
    begin
      LibPath := Paths[I];
      if ndxsqlite3api.LoadSQLite3Library(LibPath) then
      begin
        FSQLiteLoaded := True;
        FSQLiteLoadedPath := LibPath;
        Result := True;
        Break;
      end;
    end;
  end;

  // If loaded, get the version
  if FSQLiteLoaded and (FSQLiteVersion = '') then
  begin
    try
      VerStr := sqlite3_libversion();
      if VerStr <> nil then
        FSQLiteVersion := string(VerStr)
      else
        FSQLiteVersion := 'unknown';

      FSQLiteVersionNumber := sqlite3_libversion_number();
    except
      FSQLiteVersion := 'unknown';
      FSQLiteVersionNumber := 0;
    end;
  end;
end;

class function TNDXPlatform.IsSQLiteLoaded: Boolean;
begin
  Result := FSQLiteLoaded;
end;

class function TNDXPlatform.GetSQLiteVersion: string;
begin
  if not FSQLiteLoaded then
    LoadSQLiteLibrary;
  Result := FSQLiteVersion;
end;

class function TNDXPlatform.GetSQLiteVersionNumber: Integer;
begin
  if not FSQLiteLoaded then
    LoadSQLiteLibrary;
  Result := FSQLiteVersionNumber;
end;

class function TNDXPlatform.CheckSQLiteVersion(AMinVersion: Integer): Boolean;
begin
  if not FSQLiteLoaded then
    LoadSQLiteLibrary;
  Result := FSQLiteVersionNumber >= AMinVersion;
end;

class function TNDXPlatform.GetLoadedLibraryPath: string;
begin
  Result := FSQLiteLoadedPath;
end;

initialization
  TNDXPlatform.FInitialized := False;
  TNDXPlatform.FPlatformType := ptUnknown;
  TNDXPlatform.FCPUArchitecture := cpuUnknown;
  TNDXPlatform.FSnapUserData := '';
  TNDXPlatform.FSnapUserCommon := '';
  TNDXPlatform.FSnapName := '';
  TNDXPlatform.FSnapPath := '';
  TNDXPlatform.FFlatpakId := '';
  TNDXPlatform.FFlatpakAppDir := '';
  TNDXPlatform.FIsMacOSSandboxed := False;
  TNDXPlatform.FSQLiteLoaded := False;
  TNDXPlatform.FSQLiteLoadedPath := '';
  TNDXPlatform.FSQLiteVersion := '';
  TNDXPlatform.FSQLiteVersionNumber := 0;
  Randomize;

end.
