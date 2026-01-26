{===============================================================================
  NDXSQLite - Version Information Unit
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Provides version information for the NDXSQLite library.
  Version constants are defined in ndxsqlite_version.inc and updated
  automatically when creating version tags via 'make tag VERSION=x.y.z'.
===============================================================================}
unit ndxsqliteversion;

{$mode objfpc}{$H+}

interface

const
  { Include version constants }
  {$I ../ndxsqlite_version.inc}

{ Returns the library version as a string (e.g., '1.0.0') }
function GetLibraryVersion: string;

{ Returns the library version with date (e.g., '1.0.0 (2026-01-25)') }
function GetLibraryVersionFull: string;

{ Returns the major version number }
function GetLibraryVersionMajor: Integer;

{ Returns the minor version number }
function GetLibraryVersionMinor: Integer;

{ Returns the patch version number }
function GetLibraryVersionPatch: Integer;

{ Returns the release date }
function GetLibraryVersionDate: string;

{ Returns the SQLite version this library was built for (static) }
function GetBuiltForSQLiteVersion: string;

{ Returns the actual SQLite library version loaded at runtime (dynamic) }
function GetRuntimeSQLiteVersion: string;

{ Returns the actual SQLite library version number at runtime (e.g., 3045000 for 3.45.0) }
function GetRuntimeSQLiteVersionNumber: Integer;

{ Checks if the loaded SQLite version meets the minimum requirement }
function CheckSQLiteMinVersion(AMinVersion: Integer): Boolean;

implementation

uses
  SysUtils, ndxsqliteplatform;

function GetLibraryVersion: string;
begin
  Result := NDXSQLITE_VERSION_STRING;
end;

function GetLibraryVersionFull: string;
begin
  Result := Format('%s (%s)', [NDXSQLITE_VERSION_STRING, NDXSQLITE_VERSION_DATE]);
end;

function GetLibraryVersionMajor: Integer;
begin
  Result := NDXSQLITE_VERSION_MAJOR;
end;

function GetLibraryVersionMinor: Integer;
begin
  Result := NDXSQLITE_VERSION_MINOR;
end;

function GetLibraryVersionPatch: Integer;
begin
  Result := NDXSQLITE_VERSION_PATCH;
end;

function GetLibraryVersionDate: string;
begin
  Result := NDXSQLITE_VERSION_DATE;
end;

function GetBuiltForSQLiteVersion: string;
begin
  Result := NDXSQLITE_SQLITE_VERSION;
end;

function GetRuntimeSQLiteVersion: string;
begin
  Result := TNDXPlatform.GetSQLiteVersion;
end;

function GetRuntimeSQLiteVersionNumber: Integer;
begin
  Result := TNDXPlatform.GetSQLiteVersionNumber;
end;

function CheckSQLiteMinVersion(AMinVersion: Integer): Boolean;
begin
  Result := TNDXPlatform.CheckSQLiteVersion(AMinVersion);
end;

end.
