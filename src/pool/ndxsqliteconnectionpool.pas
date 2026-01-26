{===============================================================================
  NDXSQLite - Connection Pool
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 5 (depends on ndxsqliteconnectionintf, ndxsqliteconnection,
             ndxsqliteconnectionoptions, ndxsqlitetypes, ndxsqliteexceptions)
===============================================================================}
unit ndxsqliteconnectionpool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  ndxsqliteconnectionintf, ndxsqliteconnection, ndxsqliteconnectionoptions,
  ndxsqlitetypes, ndxsqliteexceptions;

type
  { Pool statistics }
  TNDXPoolStatistics = record
    TotalCreated: Integer;
    TotalDestroyed: Integer;
    CurrentActive: Integer;
    CurrentIdle: Integer;
    PeakActive: Integer;
    TotalAcquisitions: Integer;
    TotalReleases: Integer;
    AverageWaitTimeMs: Double;
    TotalWaitTimeMs: Int64;
  end;

  { Forward declaration }
  TNDXSQLiteConnectionPool = class;

  { SQLite connection pool }
  TNDXSQLiteConnectionPool = class
  private
    FOptions: TNDXSQLiteConnectionOptions;
    FMinSize: Integer;
    FMaxSize: Integer;
    FIdlePool: TList;
    FActivePool: TList;
    FLock: TCriticalSection;
    FAvailableEvent: TEvent;
    FAcquireTimeoutMs: Integer;
    FStatistics: TNDXPoolStatistics;
    FValidateOnAcquire: Boolean;
    FValidationQuery: string;
    FDisposed: Boolean;

    function CreateNewConnection: INDXSQLiteConnection;
    procedure DestroyConnection(AConnection: INDXSQLiteConnection);
    function ValidateConnection(AConnection: INDXSQLiteConnection): Boolean;
    procedure EnsureMinConnections;
    function GetActiveCount: Integer;
    function GetIdleCount: Integer;

  public
    constructor Create(AOptions: TNDXSQLiteConnectionOptions;
      AMinSize: Integer = 2; AMaxSize: Integer = 10);
    destructor Destroy; override;

    { Acquires an idle connection from the pool, blocking until one is available. }
    function Acquire: INDXSQLiteConnection;
    { Attempts to acquire a connection within the timeout. Returns True on success. }
    function TryAcquire(out AConnection: INDXSQLiteConnection;
      ATimeoutMs: Integer = -1): Boolean;
    { Returns a connection to the pool for reuse. }
    procedure Release(AConnection: INDXSQLiteConnection);
    { Closes and removes all idle and active connections from the pool. }
    procedure Clear;
    { Changes the minimum and maximum pool size, adjusting connections as needed. }
    procedure Resize(ANewMinSize, ANewMaxSize: Integer);
    { Checks all idle connections with the validation query, removing invalid ones. }
    procedure Validate;

    property MinSize: Integer read FMinSize;
    property MaxSize: Integer read FMaxSize;
    property ActiveCount: Integer read GetActiveCount;
    property IdleCount: Integer read GetIdleCount;
    property Statistics: TNDXPoolStatistics read FStatistics;
    property AcquireTimeoutMs: Integer read FAcquireTimeoutMs write FAcquireTimeoutMs;
    property ValidateOnAcquire: Boolean read FValidateOnAcquire write FValidateOnAcquire;
    property ValidationQuery: string read FValidationQuery write FValidationQuery;
  end;

  { Wrapper for automatic use with try/finally }
  TNDXPooledConnection = class
  private
    FPool: TNDXSQLiteConnectionPool;
    FConnection: INDXSQLiteConnection;
  public
    { Acquires a connection from the pool. Released automatically on Destroy. }
    constructor Create(APool: TNDXSQLiteConnectionPool);
    destructor Destroy; override;
    property Connection: INDXSQLiteConnection read FConnection;
  end;

implementation

{ TNDXSQLiteConnectionPool }

constructor TNDXSQLiteConnectionPool.Create(AOptions: TNDXSQLiteConnectionOptions;
  AMinSize: Integer; AMaxSize: Integer);
begin
  inherited Create;

  if AMinSize < 0 then
    raise ENDXSQLitePoolException.Create('MinSize cannot be negative');
  if AMaxSize < 1 then
    raise ENDXSQLitePoolException.Create('MaxSize must be at least 1');
  if AMinSize > AMaxSize then
    raise ENDXSQLitePoolException.Create('MinSize cannot exceed MaxSize');

  FOptions := AOptions.Clone;
  FMinSize := AMinSize;
  FMaxSize := AMaxSize;
  FIdlePool := TList.Create;
  FActivePool := TList.Create;
  FLock := TCriticalSection.Create;
  FAvailableEvent := TEvent.Create(nil, True, True, '');
  FAcquireTimeoutMs := 30000;
  FValidateOnAcquire := True;
  FValidationQuery := 'SELECT 1';
  FDisposed := False;

  FillChar(FStatistics, SizeOf(FStatistics), 0);
  EnsureMinConnections;
end;

destructor TNDXSQLiteConnectionPool.Destroy;
begin
  // Set FDisposed inside the lock to prevent race conditions
  FLock.Enter;
  try
    FDisposed := True;
    // Signal any waiting threads to wake up and exit
    FAvailableEvent.SetEvent;
  finally
    FLock.Leave;
  end;

  Clear;
  FreeAndNil(FIdlePool);
  FreeAndNil(FActivePool);
  FreeAndNil(FLock);
  FreeAndNil(FAvailableEvent);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TNDXSQLiteConnectionPool.CreateNewConnection: INDXSQLiteConnection;
var
  Opts: TNDXSQLiteConnectionOptions;
begin
  Opts := FOptions.Clone;
  try
    Opts.IsPrimaryConnection := False;
    Opts.DisableAutoClose := True; // The pool manages the lifecycle
    Result := TNDXSQLiteConnection.Create(Opts);
    Result.Open;
    Inc(FStatistics.TotalCreated);
  finally
    Opts.Free;
  end;
end;

procedure TNDXSQLiteConnectionPool.DestroyConnection(AConnection: INDXSQLiteConnection);
begin
  if AConnection <> nil then
  begin
    try
      if AConnection.IsOpen then
        AConnection.Close;
    except
      // Ignore errors on close
    end;
    Inc(FStatistics.TotalDestroyed);
  end;
end;

function TNDXSQLiteConnectionPool.ValidateConnection(
  AConnection: INDXSQLiteConnection): Boolean;
begin
  Result := False;
  try
    if not AConnection.IsOpen then
      Exit;
    if FValidationQuery <> '' then
      AConnection.ExecuteScalar(FValidationQuery);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TNDXSQLiteConnectionPool.EnsureMinConnections;
var
  Conn: INDXSQLiteConnection;
  ConnPtr: Pointer;
begin
  FLock.Enter;
  try
    while FIdlePool.Count < FMinSize do
    begin
      Conn := CreateNewConnection;
      ConnPtr := Pointer(Conn);
      Conn._AddRef; // Keep the reference
      FIdlePool.Add(ConnPtr);
      Inc(FStatistics.CurrentIdle);
    end;
  finally
    FLock.Leave;
  end;
end;

function TNDXSQLiteConnectionPool.Acquire: INDXSQLiteConnection;
begin
  if not TryAcquire(Result, FAcquireTimeoutMs) then
    raise ENDXSQLiteTimeoutException.Create('Acquire connection', FAcquireTimeoutMs);
end;

function TNDXSQLiteConnectionPool.TryAcquire(out AConnection: INDXSQLiteConnection;
  ATimeoutMs: Integer): Boolean;
var
  StartTime: TDateTime;
  Conn: INDXSQLiteConnection;
  ConnPtr: Pointer;
  ValidConn: Boolean;
  WaitResult: TWaitResult;
  Disposed: Boolean;
begin
  Result := False;
  AConnection := nil;
  StartTime := Now;

  repeat
    FLock.Enter;
    try
      // Check disposed inside the lock for thread-safety
      if FDisposed then
        Exit;

      // Try to get a connection from the idle pool
      while FIdlePool.Count > 0 do
      begin
        ConnPtr := FIdlePool[FIdlePool.Count - 1];
        FIdlePool.Delete(FIdlePool.Count - 1);
        Dec(FStatistics.CurrentIdle);

        Conn := INDXSQLiteConnection(ConnPtr);
        ValidConn := not FValidateOnAcquire or ValidateConnection(Conn);

        if ValidConn then
        begin
          FActivePool.Add(ConnPtr);
          AConnection := Conn;
          Inc(FStatistics.TotalAcquisitions);
          Inc(FStatistics.CurrentActive);

          if FStatistics.CurrentActive > FStatistics.PeakActive then
            FStatistics.PeakActive := FStatistics.CurrentActive;

          // Calculate wait time
          FStatistics.TotalWaitTimeMs := FStatistics.TotalWaitTimeMs +
            MilliSecondsBetween(Now, StartTime);
          if FStatistics.TotalAcquisitions > 0 then
            FStatistics.AverageWaitTimeMs :=
              FStatistics.TotalWaitTimeMs / FStatistics.TotalAcquisitions;

          Result := True;
          Exit;
        end
        else
        begin
          // Invalid connection, destroy it
          DestroyConnection(Conn);
          Conn._Release;
        end;
      end;

      // If no idle connection, create a new one if possible
      if FActivePool.Count < FMaxSize then
      begin
        Conn := CreateNewConnection;
        ConnPtr := Pointer(Conn);
        Conn._AddRef;
        FActivePool.Add(ConnPtr);
        AConnection := Conn;
        Inc(FStatistics.TotalAcquisitions);
        Inc(FStatistics.CurrentActive);

        if FStatistics.CurrentActive > FStatistics.PeakActive then
          FStatistics.PeakActive := FStatistics.CurrentActive;

        Result := True;
        Exit;
      end;

      FAvailableEvent.ResetEvent;
      Disposed := FDisposed;  // Capture before leaving lock
    finally
      FLock.Leave;
    end;

    // Check disposed after releasing lock
    if Disposed then
      Exit;

    // Wait for a connection to become available
    if ATimeoutMs < 0 then
      WaitResult := FAvailableEvent.WaitFor($7FFFFFFF)
    else
    begin
      WaitResult := FAvailableEvent.WaitFor(100);
      if WaitResult = wrTimeout then
      begin
        if MilliSecondsBetween(Now, StartTime) >= ATimeoutMs then
          Exit;
      end;
    end;

    // Re-check disposed after waking from wait
    FLock.Enter;
    try
      Disposed := FDisposed;
    finally
      FLock.Leave;
    end;

  until Disposed;
end;

procedure TNDXSQLiteConnectionPool.Release(AConnection: INDXSQLiteConnection);
var
  Idx: Integer;
  ConnPtr: Pointer;
begin
  if AConnection = nil then
    Exit;

  FLock.Enter;
  try
    // Check disposed inside the lock for thread-safety
    if FDisposed then
    begin
      // Pool is being destroyed, just close and release the connection
      DestroyConnection(AConnection);
      Exit;
    end;

    ConnPtr := Pointer(AConnection);
    Idx := FActivePool.IndexOf(ConnPtr);

    if Idx < 0 then
      raise ENDXSQLitePoolException.Create('Connection not found in active pool');

    FActivePool.Delete(Idx);
    Dec(FStatistics.CurrentActive);
    Inc(FStatistics.TotalReleases);

    // Rollback if a transaction is in progress
    if AConnection.IsTransactionActive then
    begin
      try
        AConnection.Rollback;
      except
        // Ignore errors
      end;
    end;

    // Return to idle pool if possible
    if FIdlePool.Count < FMaxSize then
    begin
      FIdlePool.Add(ConnPtr);
      Inc(FStatistics.CurrentIdle);
    end
    else
    begin
      DestroyConnection(AConnection);
      AConnection._Release;
    end;

    FAvailableEvent.SetEvent;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnectionPool.Clear;
var
  I: Integer;
  Conn: INDXSQLiteConnection;
  ConnPtr: Pointer;
begin
  FLock.Enter;
  try
    // Clean up idle pool
    for I := FIdlePool.Count - 1 downto 0 do
    begin
      ConnPtr := FIdlePool[I];
      Conn := INDXSQLiteConnection(ConnPtr);
      DestroyConnection(Conn);
      Conn._Release;
    end;
    FIdlePool.Clear;

    // Clean up active pool
    for I := FActivePool.Count - 1 downto 0 do
    begin
      ConnPtr := FActivePool[I];
      Conn := INDXSQLiteConnection(ConnPtr);
      DestroyConnection(Conn);
      Conn._Release;
    end;
    FActivePool.Clear;

    FStatistics.CurrentActive := 0;
    FStatistics.CurrentIdle := 0;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXSQLiteConnectionPool.Resize(ANewMinSize, ANewMaxSize: Integer);
begin
  if ANewMinSize < 0 then
    raise ENDXSQLitePoolException.Create('MinSize cannot be negative');
  if ANewMaxSize < 1 then
    raise ENDXSQLitePoolException.Create('MaxSize must be at least 1');
  if ANewMinSize > ANewMaxSize then
    raise ENDXSQLitePoolException.Create('MinSize cannot exceed MaxSize');

  FLock.Enter;
  try
    FMinSize := ANewMinSize;
    FMaxSize := ANewMaxSize;
  finally
    FLock.Leave;
  end;

  EnsureMinConnections;
end;

procedure TNDXSQLiteConnectionPool.Validate;
var
  I: Integer;
  Conn: INDXSQLiteConnection;
  ConnPtr: Pointer;
begin
  FLock.Enter;
  try
    for I := FIdlePool.Count - 1 downto 0 do
    begin
      ConnPtr := FIdlePool[I];
      Conn := INDXSQLiteConnection(ConnPtr);

      if not ValidateConnection(Conn) then
      begin
        FIdlePool.Delete(I);
        DestroyConnection(Conn);
        Conn._Release;
        Dec(FStatistics.CurrentIdle);
      end;
    end;
  finally
    FLock.Leave;
  end;

  EnsureMinConnections;
end;

function TNDXSQLiteConnectionPool.GetActiveCount: Integer;
begin
  FLock.Enter;
  try
    Result := FActivePool.Count;
  finally
    FLock.Leave;
  end;
end;

function TNDXSQLiteConnectionPool.GetIdleCount: Integer;
begin
  FLock.Enter;
  try
    Result := FIdlePool.Count;
  finally
    FLock.Leave;
  end;
end;

{ TNDXPooledConnection }

constructor TNDXPooledConnection.Create(APool: TNDXSQLiteConnectionPool);
begin
  inherited Create;
  FPool := APool;
  FConnection := FPool.Acquire;
end;

destructor TNDXPooledConnection.Destroy;
begin
  if (FPool <> nil) and (FConnection <> nil) then
    FPool.Release(FConnection);
  FConnection := nil;
  inherited Destroy;
end;

end.
