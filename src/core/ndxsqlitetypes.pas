{===============================================================================
  NDXSQLite - Types and Enumerations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 0 (no internal dependencies)
===============================================================================}
unit ndxsqlitetypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, DateUtils;

const
  { Default constants }
  NDX_DEFAULT_AUTO_CLOSE_TIMEOUT = 28000;   // 28 seconds
  NDX_DEFAULT_BUSY_TIMEOUT = 5000;          // 5 seconds
  NDX_DEFAULT_CACHE_SIZE = 2000;            // ~2MB
  NDX_DEFAULT_PAGE_SIZE = 4096;             // 4KB
  NDX_DEFAULT_CONNECTION_TIMEOUT = 30000;   // 30 seconds
  NDX_MAX_ACTION_HISTORY = 5;

type
  { SQLite transaction mode }
  TNDXSQLiteTransactionMode = (
    tmDeferred,    // Lock acquired on first write (default)
    tmImmediate,   // RESERVED lock acquired immediately
    tmExclusive    // EXCLUSIVE lock acquired immediately
  );

  { SQLite journal mode }
  TNDXSQLiteJournalMode = (
    jmDelete,      // Delete journal after commit (legacy default)
    jmTruncate,    // Truncate journal to zero
    jmPersist,     // Keep journal, zero header
    jmMemory,      // Journal in memory only
    jmWAL,         // Write-Ahead Logging (recommended)
    jmOff          // No journal (dangerous!)
  );

  { SQLite synchronization mode }
  TNDXSQLiteSyncMode = (
    smOff,         // No sync (fast but risky)
    smNormal,      // Sync at critical moments (recommended)
    smFull,        // Sync after each write
    smExtra        // Extra sync for WAL
  );

  { Connection state }
  TNDXSQLiteConnectionState = (
    scsDisconnected,   // Disconnected
    scsConnecting,     // Connecting
    scsConnected,      // Connected and ready
    scsExecuting,      // Executing
    scsError           // Error state
  );

  { Open mode }
  TNDXSQLiteOpenMode = (
    omReadWrite,       // Read/write (default)
    omReadOnly,        // Read only
    omReadWriteCreate, // Read/write, create if not exists
    omMemory           // In-memory database
  );

  { Locking mode }
  TNDXSQLiteLockingMode = (
    lmNormal,          // Standard locks
    lmExclusive        // Permanent exclusive lock
  );

  { Generic callbacks }
  TDataSetNotifyEvent = procedure(Sender: TObject; ADataSet: TDataSet) of object;
  TBackupProgressEvent = procedure(Sender: TObject; ARemaining, ATotal: Integer) of object;
  TErrorNotifyEvent = procedure(Sender: TObject; const AErrorMessage: string; AErrorCode: Integer) of object;

  { Helper for mode conversion }
  TNDXSQLiteHelper = class
  public
    { Converts a journal mode enumeration value to its SQL keyword string. }
    class function JournalModeToString(AMode: TNDXSQLiteJournalMode): string;
    { Parses a SQL keyword string into its journal mode enumeration value. Defaults to jmDelete. }
    class function StringToJournalMode(const AValue: string): TNDXSQLiteJournalMode;
    { Converts a synchronization mode enumeration value to its SQL keyword string. }
    class function SyncModeToString(AMode: TNDXSQLiteSyncMode): string;
    { Parses a SQL keyword string into its synchronization mode value. Defaults to smNormal. }
    class function StringToSyncMode(const AValue: string): TNDXSQLiteSyncMode;
    { Converts a transaction mode enumeration value to its SQL keyword string. }
    class function TransactionModeToString(AMode: TNDXSQLiteTransactionMode): string;
    { Converts a connection state enumeration value to a human-readable label. }
    class function ConnectionStateToString(AState: TNDXSQLiteConnectionState): string;
    { Returns the current time as milliseconds since the Unix epoch. }
    class function GetTimeMs: Int64;
  end;

{ Utility functions }

{ Returns True if the name is a valid SQL identifier (no reserved words, valid characters). }
function IsValidIdentifier(const AName: string): Boolean;
{ Wraps the name in double quotes, escaping any embedded double quotes. }
function QuoteIdentifier(const AName: string): string;
{ Escapes single quotes in the value by doubling them for use in SQL literals. }
function EscapeString(const AValue: string): string;

implementation

{ TNDXSQLiteHelper }

class function TNDXSQLiteHelper.JournalModeToString(AMode: TNDXSQLiteJournalMode): string;
begin
  case AMode of
    jmDelete:   Result := 'DELETE';
    jmTruncate: Result := 'TRUNCATE';
    jmPersist:  Result := 'PERSIST';
    jmMemory:   Result := 'MEMORY';
    jmWAL:      Result := 'WAL';
    jmOff:      Result := 'OFF';
  else
    Result := 'DELETE';
  end;
end;

class function TNDXSQLiteHelper.StringToJournalMode(const AValue: string): TNDXSQLiteJournalMode;
var
  S: string;
begin
  S := UpperCase(Trim(AValue));
  if S = 'DELETE' then Result := jmDelete
  else if S = 'TRUNCATE' then Result := jmTruncate
  else if S = 'PERSIST' then Result := jmPersist
  else if S = 'MEMORY' then Result := jmMemory
  else if S = 'WAL' then Result := jmWAL
  else if S = 'OFF' then Result := jmOff
  else Result := jmDelete;
end;

class function TNDXSQLiteHelper.SyncModeToString(AMode: TNDXSQLiteSyncMode): string;
begin
  case AMode of
    smOff:    Result := 'OFF';
    smNormal: Result := 'NORMAL';
    smFull:   Result := 'FULL';
    smExtra:  Result := 'EXTRA';
  else
    Result := 'NORMAL';
  end;
end;

class function TNDXSQLiteHelper.StringToSyncMode(const AValue: string): TNDXSQLiteSyncMode;
var
  S: string;
begin
  S := UpperCase(Trim(AValue));
  if S = 'OFF' then Result := smOff
  else if S = 'NORMAL' then Result := smNormal
  else if S = 'FULL' then Result := smFull
  else if S = 'EXTRA' then Result := smExtra
  else Result := smNormal;
end;

class function TNDXSQLiteHelper.TransactionModeToString(AMode: TNDXSQLiteTransactionMode): string;
begin
  case AMode of
    tmDeferred:  Result := 'DEFERRED';
    tmImmediate: Result := 'IMMEDIATE';
    tmExclusive: Result := 'EXCLUSIVE';
  else
    Result := 'DEFERRED';
  end;
end;

class function TNDXSQLiteHelper.ConnectionStateToString(AState: TNDXSQLiteConnectionState): string;
begin
  case AState of
    scsDisconnected: Result := 'Disconnected';
    scsConnecting:   Result := 'Connecting';
    scsConnected:    Result := 'Connected';
    scsExecuting:    Result := 'Executing';
    scsError:        Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

class function TNDXSQLiteHelper.GetTimeMs: Int64;
begin
  Result := MilliSecondsBetween(Now, EncodeDate(1970, 1, 1));
end;

{ Utility functions }

function IsValidIdentifier(const AName: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;

  // Empty name is not valid
  if AName = '' then
    Exit;

  // Check first character: must be letter or underscore
  C := AName[1];
  if not (C in ['A'..'Z', 'a'..'z', '_']) then
    Exit;

  // Check remaining characters: letter, digit, or underscore
  for I := 2 to Length(AName) do
  begin
    C := AName[I];
    if not (C in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Exit;
  end;

  // Check for SQL reserved words (basic list)
  // Note: SQLite is more permissive, but we're being safe
  if SameText(AName, 'SELECT') or SameText(AName, 'FROM') or
     SameText(AName, 'WHERE') or SameText(AName, 'INSERT') or
     SameText(AName, 'UPDATE') or SameText(AName, 'DELETE') or
     SameText(AName, 'DROP') or SameText(AName, 'CREATE') or
     SameText(AName, 'TABLE') or SameText(AName, 'INDEX') or
     SameText(AName, 'ALTER') or SameText(AName, 'BEGIN') or
     SameText(AName, 'COMMIT') or SameText(AName, 'ROLLBACK') then
    Exit;

  Result := True;
end;

function QuoteIdentifier(const AName: string): string;
begin
  // Use double quotes for identifiers (SQL standard)
  // Escape any existing double quotes by doubling them
  Result := '"' + StringReplace(AName, '"', '""', [rfReplaceAll]) + '"';
end;

function EscapeString(const AValue: string): string;
begin
  // Escape single quotes by doubling them
  Result := StringReplace(AValue, '''', '''''', [rfReplaceAll]);
end;

end.
