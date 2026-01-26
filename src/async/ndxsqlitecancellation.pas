{===============================================================================
  NDXSQLite - CancellationToken
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 2 (no internal dependencies)
===============================================================================}
unit ndxsqlitecancellation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { Forward declaration }
  TNDXCancellationTokenSource = class;

  { Cancellation exception }
  ENDXOperationCanceledException = class(Exception)
  public
    constructor Create; overload;
    constructor Create(const AMessage: string); overload;
  end;

  { Cancellation token interface }
  INDXCancellationToken = interface
    ['{B1C2D3E4-F5A6-7890-1234-567890ABCDEF}']
    function IsCancellationRequested: Boolean;
    procedure ThrowIfCancellationRequested;
    function GetWaitHandle: TEvent;
    property WaitHandle: TEvent read GetWaitHandle;
  end;

  { Internal token implementation backed by a shared boolean flag and event. }
  TNDXCancellationTokenImpl = class(TInterfacedObject, INDXCancellationToken)
  private
    FCanceledPtr: PBoolean;
    FEvent: TEvent;
    FOwnsEvent: Boolean;
  public
    constructor Create(ACanceledPtr: PBoolean; AEvent: TEvent; AOwnsEvent: Boolean = False);
    destructor Destroy; override;
    { Returns True if cancellation has been requested via the source. }
    function IsCancellationRequested: Boolean;
    { Raises ENDXOperationCanceledException if cancellation has been requested. }
    procedure ThrowIfCancellationRequested;
    { Returns the event handle that is signaled when cancellation is requested. }
    function GetWaitHandle: TEvent;
  end;

  { Internal thread for CancelAfter }
  TNDXCancelAfterThread = class(TThread)
  private
    FSource: TNDXCancellationTokenSource;
    FDelayMs: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ASource: TNDXCancellationTokenSource; ADelayMs: Integer);
  end;

  { Controls cancellation by signaling associated tokens. }
  TNDXCancellationTokenSource = class
  private
    FToken: INDXCancellationToken;
    FCanceled: Boolean;
    FEvent: TEvent;
    FLock: TCriticalSection;
    FTimer: TNDXCancelAfterThread;
    FDisposed: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    { Signals cancellation to all tokens obtained from this source. }
    procedure Cancel;
    { Schedules automatic cancellation after the specified delay in milliseconds. }
    procedure CancelAfter(ADelayMs: Integer);
    { Resets the source to a non-canceled state for reuse. }
    procedure Reset;

    { Returns the cancellation token associated with this source. }
    function GetToken: INDXCancellationToken;
    property Token: INDXCancellationToken read GetToken;
    property IsCancellationRequested: Boolean read FCanceled;

    { Creates a token source that is canceled when any of the provided tokens is canceled. }
    class function CreateLinkedTokenSource(
      const ATokens: array of INDXCancellationToken): TNDXCancellationTokenSource;
  end;

  { Static token for non-cancelable operations }
  TNDXCancellationToken = class
  private
    class var FNoneToken: INDXCancellationToken;
    class var FNeverCanceled: Boolean;
  public
    class function None: INDXCancellationToken;
    class destructor Destroy;
  end;

implementation

{ ENDXOperationCanceledException }

constructor ENDXOperationCanceledException.Create;
begin
  inherited Create('The operation was canceled.');
end;

constructor ENDXOperationCanceledException.Create(const AMessage: string);
begin
  inherited Create(AMessage);
end;

{ TNDXCancellationTokenImpl }

constructor TNDXCancellationTokenImpl.Create(ACanceledPtr: PBoolean;
  AEvent: TEvent; AOwnsEvent: Boolean);
begin
  inherited Create;
  FCanceledPtr := ACanceledPtr;
  FEvent := AEvent;
  FOwnsEvent := AOwnsEvent;
end;

destructor TNDXCancellationTokenImpl.Destroy;
begin
  if FOwnsEvent and Assigned(FEvent) then
    FreeAndNil(FEvent);
  inherited Destroy;
end;

function TNDXCancellationTokenImpl.IsCancellationRequested: Boolean;
begin
  if FCanceledPtr <> nil then
    Result := FCanceledPtr^
  else
    Result := False;
end;

procedure TNDXCancellationTokenImpl.ThrowIfCancellationRequested;
begin
  if (FCanceledPtr <> nil) and FCanceledPtr^ then
    raise ENDXOperationCanceledException.Create;
end;

function TNDXCancellationTokenImpl.GetWaitHandle: TEvent;
begin
  Result := FEvent;
end;

{ TNDXCancelAfterThread }

constructor TNDXCancelAfterThread.Create(ASource: TNDXCancellationTokenSource;
  ADelayMs: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSource := ASource;
  FDelayMs := ADelayMs;
end;

procedure TNDXCancelAfterThread.Execute;
var
  ElapsedMs: Integer;
  StepMs: Integer;
begin
  ElapsedMs := 0;
  StepMs := 100;

  while (not Terminated) and (ElapsedMs < FDelayMs) do
  begin
    Sleep(StepMs);
    Inc(ElapsedMs, StepMs);
  end;

  if not Terminated and Assigned(FSource) and not FSource.FCanceled then
    FSource.Cancel;
end;

{ TNDXCancellationTokenSource }

constructor TNDXCancellationTokenSource.Create;
begin
  inherited Create;
  FCanceled := False;
  FDisposed := False;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, True, False, '');
  FToken := TNDXCancellationTokenImpl.Create(@FCanceled, FEvent, False);
end;

destructor TNDXCancellationTokenSource.Destroy;
begin
  FDisposed := True;

  if Assigned(FTimer) then
  begin
    FTimer.Terminate;
    FTimer := nil;
  end;

  FToken := nil;
  FreeAndNil(FEvent);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TNDXCancellationTokenSource.Cancel;
begin
  if FDisposed then Exit;

  FLock.Enter;
  try
    if not FCanceled then
    begin
      FCanceled := True;
      if Assigned(FEvent) then
        FEvent.SetEvent;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXCancellationTokenSource.CancelAfter(ADelayMs: Integer);
begin
  if FDisposed or FCanceled then Exit;

  FLock.Enter;
  try
    if Assigned(FTimer) then
    begin
      FTimer.Terminate;
      FTimer := nil;
    end;

    FTimer := TNDXCancelAfterThread.Create(Self, ADelayMs);
    FTimer.Start;
  finally
    FLock.Leave;
  end;
end;

procedure TNDXCancellationTokenSource.Reset;
begin
  if FDisposed then Exit;

  FLock.Enter;
  try
    if Assigned(FTimer) then
    begin
      FTimer.Terminate;
      FTimer := nil;
    end;

    FCanceled := False;
    if Assigned(FEvent) then
      FEvent.ResetEvent;
  finally
    FLock.Leave;
  end;
end;

function TNDXCancellationTokenSource.GetToken: INDXCancellationToken;
begin
  Result := FToken;
end;

class function TNDXCancellationTokenSource.CreateLinkedTokenSource(
  const ATokens: array of INDXCancellationToken): TNDXCancellationTokenSource;
var
  I: Integer;
begin
  Result := TNDXCancellationTokenSource.Create;

  // Check if one of the linked tokens is already canceled
  for I := Low(ATokens) to High(ATokens) do
  begin
    if Assigned(ATokens[I]) and ATokens[I].IsCancellationRequested then
    begin
      Result.Cancel;
      Break;
    end;
  end;
end;

{ TNDXCancellationToken }

class function TNDXCancellationToken.None: INDXCancellationToken;
begin
  if FNoneToken = nil then
  begin
    FNeverCanceled := False;
    FNoneToken := TNDXCancellationTokenImpl.Create(@FNeverCanceled, nil, False);
  end;
  Result := FNoneToken;
end;

class destructor TNDXCancellationToken.Destroy;
begin
  FNoneToken := nil;
end;

end.
