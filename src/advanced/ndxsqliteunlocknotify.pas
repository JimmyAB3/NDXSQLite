{===============================================================================
  NDXSQLite - Unlock Notify Support
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for SQLite unlock_notify mechanism.
  Allows waiting for database unlock when SQLITE_LOCKED is returned.
  Useful for WAL mode and shared-cache mode deadlock resolution.
===============================================================================}
unit ndxsqliteunlocknotify;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Unlock notification event }
  TNDXUnlockNotifyEvent = procedure(Sender: TObject) of object;

  { Unlock wait result }
  TNDXUnlockWaitResult = (
    uwrSuccess,      // Database was unlocked
    uwrTimeout,      // Wait timed out
    uwrError,        // Error occurred
    uwrNotSupported  // unlock_notify not available
  );

  { Unlock Notify Manager }
  TNDXSQLiteUnlockNotify = class
  private
    FConnection: INDXSQLiteConnection;
    FOnUnlock: TNDXUnlockNotifyEvent;
    FEvent: TEvent;
    FLastError: string;
    FDefaultTimeout: Cardinal;
    FAutoRetry: Boolean;
    FMaxRetries: Integer;

    function GetDBHandle: Psqlite3;
    procedure DoUnlockNotify;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Blocks until the database lock is released or the timeout expires. }
    function WaitForUnlock(ATimeout: Cardinal = INFINITE): TNDXUnlockWaitResult;

    { Executes a query with automatic retry when SQLITE_LOCKED is returned. Returns True on success. }
    function ExecuteWithRetry(const ASQL: string): Boolean;
    { Executes a non-query statement with automatic retry on SQLITE_LOCKED. Returns rows affected. }
    function ExecuteWithRetryNonQuery(const ASQL: string): Integer;

    { Returns True if the SQLite library supports the unlock_notify mechanism. }
    class function IsAvailable: Boolean;

    { Properties }
    property Connection: INDXSQLiteConnection read FConnection;
    property OnUnlock: TNDXUnlockNotifyEvent read FOnUnlock write FOnUnlock;
    property LastError: string read FLastError;
    property DefaultTimeout: Cardinal read FDefaultTimeout write FDefaultTimeout;
    property AutoRetry: Boolean read FAutoRetry write FAutoRetry;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
  end;

  { Helper class for step-retry pattern }
  TNDXSQLiteStepRetry = class
  private
    FConnection: INDXSQLiteConnection;
    FUnlockNotify: TNDXSQLiteUnlockNotify;
    FMaxRetries: Integer;
    FRetryDelay: Cardinal;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Steps a prepared statement, automatically retrying when SQLITE_LOCKED is returned. }
    function StepWithRetry(AStmt: Psqlite3_stmt): Integer;

    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property RetryDelay: Cardinal read FRetryDelay write FRetryDelay;
  end;

implementation

var
  UnlockNotifyInstance: TNDXSQLiteUnlockNotify = nil;

{ Unlock notify callback - called by SQLite }
procedure UnlockNotifyCallback(apArg: PPointer; nArg: Integer); cdecl;
var
  I: Integer;
  Notify: TNDXSQLiteUnlockNotify;
begin
  for I := 0 to nArg - 1 do
  begin
    Notify := TNDXSQLiteUnlockNotify(apArg[I]);
    if Notify <> nil then
      Notify.DoUnlockNotify;
  end;
end;

{ TNDXSQLiteUnlockNotify }

constructor TNDXSQLiteUnlockNotify.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FOnUnlock := nil;
  FEvent := TEvent.Create(nil, True, False, '');
  FLastError := '';
  FDefaultTimeout := 30000; // 30 seconds default
  FAutoRetry := True;
  FMaxRetries := 5;
end;

destructor TNDXSQLiteUnlockNotify.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

function TNDXSQLiteUnlockNotify.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

procedure TNDXSQLiteUnlockNotify.DoUnlockNotify;
begin
  FEvent.SetEvent;
  if Assigned(FOnUnlock) then
    FOnUnlock(Self);
end;

function TNDXSQLiteUnlockNotify.WaitForUnlock(ATimeout: Cardinal): TNDXUnlockWaitResult;
var
  RC: Integer;
  WaitResult: TWaitResult;
begin
  if not IsAvailable then
  begin
    FLastError := 'sqlite3_unlock_notify is not available';
    Result := uwrNotSupported;
    Exit;
  end;

  // Reset event
  FEvent.ResetEvent;

  // Register for unlock notification
  RC := sqlite3_unlock_notify(GetDBHandle, @UnlockNotifyCallback, Self);

  if RC = SQLITE_LOCKED then
  begin
    // Already locked, wait for notification
    if ATimeout = 0 then
      ATimeout := FDefaultTimeout;

    WaitResult := FEvent.WaitFor(ATimeout);

    case WaitResult of
      wrSignaled:
        Result := uwrSuccess;
      wrTimeout:
        begin
          FLastError := 'Timeout waiting for database unlock';
          Result := uwrTimeout;
        end;
    else
      begin
        FLastError := 'Error waiting for database unlock';
        Result := uwrError;
      end;
    end;
  end
  else if RC = SQLITE_OK then
  begin
    // Not currently locked
    Result := uwrSuccess;
  end
  else
  begin
    FLastError := Format('sqlite3_unlock_notify failed: %d', [RC]);
    Result := uwrError;
  end;
end;

function TNDXSQLiteUnlockNotify.ExecuteWithRetry(const ASQL: string): Boolean;
var
  Retries: Integer;
  RC: Integer;
  Stmt: Psqlite3_stmt;
  WaitResult: TNDXUnlockWaitResult;
begin
  Result := False;
  Retries := 0;

  while Retries < FMaxRetries do
  begin
    RC := sqlite3_prepare_v2(GetDBHandle, PAnsiChar(AnsiString(ASQL)), -1, Stmt, nil);

    if RC = SQLITE_OK then
    begin
      try
        repeat
          RC := sqlite3_step(Stmt);

          if RC = SQLITE_LOCKED then
          begin
            if FAutoRetry then
            begin
              WaitResult := WaitForUnlock(FDefaultTimeout);
              if WaitResult <> uwrSuccess then
              begin
                Inc(Retries);
                Break;
              end;
              // Reset statement and retry
              sqlite3_reset(Stmt);
            end
            else
            begin
              FLastError := 'Database is locked';
              Exit;
            end;
          end;
        until (RC <> SQLITE_ROW) and (RC <> SQLITE_LOCKED);

        if (RC = SQLITE_DONE) or (RC = SQLITE_OK) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          FLastError := string(sqlite3_errmsg(GetDBHandle));
          Inc(Retries);
        end;
      finally
        sqlite3_finalize(Stmt);
      end;
    end
    else if RC = SQLITE_LOCKED then
    begin
      if FAutoRetry then
      begin
        WaitResult := WaitForUnlock(FDefaultTimeout);
        if WaitResult <> uwrSuccess then
          Inc(Retries)
        else
          Continue; // Retry prepare
      end
      else
      begin
        FLastError := 'Database is locked during prepare';
        Exit;
      end;
    end
    else
    begin
      FLastError := string(sqlite3_errmsg(GetDBHandle));
      Exit;
    end;
  end;

  FLastError := Format('Max retries (%d) exceeded', [FMaxRetries]);
end;

function TNDXSQLiteUnlockNotify.ExecuteWithRetryNonQuery(const ASQL: string): Integer;
begin
  if ExecuteWithRetry(ASQL) then
    Result := sqlite3_changes(GetDBHandle)
  else
    Result := -1;
end;

class function TNDXSQLiteUnlockNotify.IsAvailable: Boolean;
begin
  Result := Assigned(sqlite3_unlock_notify);
end;

{ TNDXSQLiteStepRetry }

constructor TNDXSQLiteStepRetry.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FUnlockNotify := TNDXSQLiteUnlockNotify.Create(AConnection);
  FMaxRetries := 5;
  FRetryDelay := 100; // 100ms between retries
end;

destructor TNDXSQLiteStepRetry.Destroy;
begin
  FUnlockNotify.Free;
  inherited Destroy;
end;

function TNDXSQLiteStepRetry.StepWithRetry(AStmt: Psqlite3_stmt): Integer;
var
  Retries: Integer;
  WaitResult: TNDXUnlockWaitResult;
begin
  Retries := 0;

  repeat
    Result := sqlite3_step(AStmt);

    if Result = SQLITE_LOCKED then
    begin
      Inc(Retries);
      if Retries > FMaxRetries then
        Exit;

      // Try unlock notify first
      if TNDXSQLiteUnlockNotify.IsAvailable then
      begin
        WaitResult := FUnlockNotify.WaitForUnlock(FRetryDelay);
        if WaitResult = uwrSuccess then
          sqlite3_reset(AStmt);
      end
      else
      begin
        // Fallback: simple sleep
        Sleep(FRetryDelay);
        sqlite3_reset(AStmt);
      end;
    end;
  until Result <> SQLITE_LOCKED;
end;

end.
