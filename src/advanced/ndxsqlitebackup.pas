{===============================================================================
  NDXSQLite - Backup API
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  IMPORTANT - Reference Counting Warning:
  =======================================
  TNDXSQLiteBackup stores the connection as INDXSQLiteConnection (interface).
  Since TNDXSQLiteConnection inherits from TInterfacedObject, it uses
  automatic reference counting.

  CORRECT usage - declare connection as INTERFACE:
    var
      Conn: INDXSQLiteConnection;  // <-- INTERFACE, not class
      Backup: TNDXSQLiteBackup;
    begin
      Conn := TNDXSQLiteConnection.Create('mydb.db');
      Conn.Open;
      Backup := TNDXSQLiteBackup.Create(Conn);
      try
        Backup.BackupTo('backup.db');
      finally
        Backup.Free;
      end;
      Conn.Close;
      Conn := nil;  // Do NOT call Conn.Free!
    end;

  WRONG usage - will cause USE-AFTER-FREE crash:
    var
      Conn: TNDXSQLiteConnection;  // <-- CLASS variable = WRONG!
      Backup: TNDXSQLiteBackup;
    begin
      Conn := TNDXSQLiteConnection.Create('mydb.db');
      Conn.Open;
      Backup := TNDXSQLiteBackup.Create(Conn);  // RefCount = 1
      try
        Backup.BackupTo('backup.db');
      finally
        Backup.Free;  // RefCount = 0, Conn is DESTROYED here!
      end;
      Conn.Close;  // CRASH! Conn already freed
      Conn.Free;   // CRASH! Double free
    end;

  This is standard Free Pascal behavior with TInterfacedObject.
  When mixing object references and interface references, the interface
  reference counting can unexpectedly destroy the object.
===============================================================================}
unit ndxsqlitebackup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, DateUtils, Variants,
  ndxsqliteconnectionintf, ndxsqliteconnection, ndxsqlitetypes,
  ndxsqliteasynctypes, ndxsqlitecancellation, ndxsqliteconnectionoptions,
  ndxsqliteplatform;

type
  { Backup options }
  TNDXBackupOptions = record
    PagesPerStep: Integer;      // Pages per step (default: 100)
    StepDelayMs: Integer;       // Delay between steps in ms (default: 0)
    OverwriteExisting: Boolean; // Overwrite existing file (default: True)
    CompressBackup: Boolean;    // Compress backup (default: False)
    VerifyAfterBackup: Boolean; // Verify integrity after backup (default: True)
  end;

  { Backup result }
  TNDXBackupResult = record
    Success: Boolean;
    ErrorMessage: string;
    TotalPages: Integer;
    ElapsedTimeMs: Int64;
    DestinationPath: string;
    DestinationSizeBytes: Int64;
    Verified: Boolean;
  end;

  { Backup progress callback }
  TNDXBackupProgressEvent = procedure(Sender: TObject;
    ARemainingPages, ATotalPages: Integer; var ACancel: Boolean) of object;

  { SQLite backup manager
    WARNING: See unit header for important reference counting notes!
    Always declare your connection as INDXSQLiteConnection (interface),
    not as TNDXSQLiteConnection (class), to avoid use-after-free crashes. }
  TNDXSQLiteBackup = class
  private
    FConnection: INDXSQLiteConnection;  // Interface - participates in ref counting!
    FOnProgress: TNDXBackupProgressEvent;
    FDefaultOptions: TNDXBackupOptions;

    function GetFileSize(const APath: string): Int64;
    function VerifyBackupIntegrity(const ABackupPath: string): Boolean;

  public
    constructor Create(AConnection: INDXSQLiteConnection);

    { Performs a synchronous page-by-page backup to the destination file using default options. }
    function BackupTo(const ADestPath: string): TNDXBackupResult; overload;
    { Performs a synchronous backup with custom page step size and delay options. }
    function BackupTo(const ADestPath: string;
      const AOptions: TNDXBackupOptions): TNDXBackupResult; overload;

    { Creates an atomic backup using VACUUM INTO (requires SQLite 3.27 or later). }
    function VacuumBackupTo(const ADestPath: string): TNDXBackupResult;

    { Restores the database from the specified backup file. }
    function RestoreFrom(const ASourcePath: string): TNDXBackupResult;

    { Starts an asynchronous backup with progress reporting and optional cancellation. }
    procedure BackupToAsync(const ADestPath: string;
      AOnComplete: TNDXAsyncCallback;
      ACancellationToken: INDXCancellationToken = nil);

    { Copies a database file from source to destination using the backup API. }
    function CopyDatabase(const ASourcePath, ADestPath: string): TNDXBackupResult;

    { Verifies the integrity of a backup file by running PRAGMA integrity_check. }
    function VerifyBackup(const ABackupPath: string): Boolean;

    { Returns True if two database files have identical schema and page counts. }
    function CompareDatabases(const APath1, APath2: string): Boolean;

    { Returns a list of files matching the pattern in the specified directory. Caller must free. }
    function ListBackups(const ADirectory: string; const APattern: string = '*.db'): TStringList;

    { Backup rotation }
    procedure RotateBackups(const ADirectory: string; AKeepCount: Integer;
      const APattern: string = '*.db');

    { Generate backup name with timestamp }
    function GenerateBackupName(const ABaseName: string): string;

    property OnProgress: TNDXBackupProgressEvent read FOnProgress write FOnProgress;
    property DefaultOptions: TNDXBackupOptions read FDefaultOptions write FDefaultOptions;
  end;

implementation

uses
  StrUtils;

{ Helper function to copy a file - replaces FileUtil.CopyFile }
function CopyFile(const ASource, ADest: string): Boolean;
var
  SrcStream, DstStream: TFileStream;
begin
  Result := False;
  try
    SrcStream := TFileStream.Create(ASource, fmOpenRead or fmShareDenyWrite);
    try
      DstStream := TFileStream.Create(ADest, fmCreate);
      try
        DstStream.CopyFrom(SrcStream, SrcStream.Size);
        Result := True;
      finally
        DstStream.Free;
      end;
    finally
      SrcStream.Free;
    end;
  except
    Result := False;
  end;
end;

type
  { Thread for asynchronous backup }
  TNDXBackupThread = class(TThread)
  private
    FBackup: TNDXSQLiteBackup;
    FDestPath: string;
    FOnComplete: TNDXAsyncCallback;
    FCancellationToken: INDXCancellationToken;
    FResult: TNDXBackupResult;
    FSuccess: Boolean;
    FErrorMessage: string;
    procedure DoNotifyComplete;
  protected
    procedure Execute; override;
  public
    constructor Create(ABackup: TNDXSQLiteBackup; const ADestPath: string;
      AOnComplete: TNDXAsyncCallback; ACancellationToken: INDXCancellationToken);
  end;

{ TNDXBackupThread }

constructor TNDXBackupThread.Create(ABackup: TNDXSQLiteBackup;
  const ADestPath: string; AOnComplete: TNDXAsyncCallback;
  ACancellationToken: INDXCancellationToken);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FBackup := ABackup;
  FDestPath := ADestPath;
  FOnComplete := AOnComplete;
  FCancellationToken := ACancellationToken;
end;

procedure TNDXBackupThread.DoNotifyComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(FSuccess, FErrorMessage);
end;

procedure TNDXBackupThread.Execute;
begin
  // Check for cancellation
  if Assigned(FCancellationToken) and FCancellationToken.IsCancellationRequested then
  begin
    FSuccess := False;
    FErrorMessage := 'Operation canceled';
    Synchronize(@DoNotifyComplete);
    Exit;
  end;

  FResult := FBackup.BackupTo(FDestPath);
  FSuccess := FResult.Success;
  FErrorMessage := FResult.ErrorMessage;

  Synchronize(@DoNotifyComplete);
end;

{ TNDXSQLiteBackup }

constructor TNDXSQLiteBackup.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;

  // Default options
  FDefaultOptions.PagesPerStep := 100;
  FDefaultOptions.StepDelayMs := 0;
  FDefaultOptions.OverwriteExisting := True;
  FDefaultOptions.CompressBackup := False;
  FDefaultOptions.VerifyAfterBackup := True;
end;

function TNDXSQLiteBackup.GetFileSize(const APath: string): Int64;
var
  F: TFileStream;
begin
  Result := 0;
  if FileExists(APath) then
  begin
    try
      F := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
      try
        Result := F.Size;
      finally
        F.Free;
      end;
    except
      Result := 0;
    end;
  end;
end;

function TNDXSQLiteBackup.VerifyBackupIntegrity(const ABackupPath: string): Boolean;
var
  TempConn: TNDXSQLiteConnection;
  V: Variant;
  NormalizedPath: string;
begin
  Result := False;

  NormalizedPath := TNDXPlatform.NormalizePath(ABackupPath);

  if not FileExists(NormalizedPath) then
    Exit;

  TempConn := TNDXSQLiteConnection.Create(NormalizedPath, False);
  try
    try
      TempConn.Open;
      V := TempConn.ExecuteScalar('PRAGMA integrity_check');
      Result := (not VarIsNull(V)) and (VarToStr(V) = 'ok');
      TempConn.Close;
    except
      Result := False;
    end;
  finally
    TempConn.Free;
  end;
end;

function TNDXSQLiteBackup.BackupTo(const ADestPath: string): TNDXBackupResult;
begin
  Result := BackupTo(ADestPath, FDefaultOptions);
end;

function TNDXSQLiteBackup.BackupTo(const ADestPath: string;
  const AOptions: TNDXBackupOptions): TNDXBackupResult;
var
  StartTime: TDateTime;
  NormalizedPath: string;
begin
  StartTime := Now;
  Result.Success := False;
  Result.Verified := False;

  // Normalize path for platform
  NormalizedPath := TNDXPlatform.NormalizePath(ADestPath);
  Result.DestinationPath := NormalizedPath;

  try
    // Check if file exists
    if FileExists(NormalizedPath) then
    begin
      if AOptions.OverwriteExisting then
        DeleteFile(NormalizedPath)
      else
      begin
        Result.ErrorMessage := 'Destination file already exists';
        Exit;
      end;
    end;

    // Ensure destination directory exists
    TNDXPlatform.EnsureDirectoryExists(ExtractFilePath(NormalizedPath));

    // Use SQLite backup API (works even with active transaction)
    FConnection.BackupTo(NormalizedPath);

    Result.Success := True;
    Result.TotalPages := FConnection.ExecuteScalar('PRAGMA page_count');
    Result.DestinationSizeBytes := GetFileSize(NormalizedPath);

    // Verify integrity if requested
    if AOptions.VerifyAfterBackup then
      Result.Verified := VerifyBackupIntegrity(NormalizedPath);

  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;

  Result.ElapsedTimeMs := MilliSecondsBetween(Now, StartTime);
end;

function TNDXSQLiteBackup.VacuumBackupTo(const ADestPath: string): TNDXBackupResult;
var
  StartTime: TDateTime;
  NormalizedPath, EscapedPath: string;
begin
  StartTime := Now;
  Result.DestinationPath := ADestPath;
  Result.Success := False;
  Result.Verified := False;

  try
    // Normalize path for platform
    NormalizedPath := TNDXPlatform.NormalizePath(ADestPath);

    if FileExists(NormalizedPath) then
      DeleteFile(NormalizedPath);

    // Escape path for SQL (protection against injection)
    EscapedPath := TNDXPlatform.EscapeSQLPath(NormalizedPath);
    FConnection.ExecuteNonQuery(Format('VACUUM INTO ''%s''', [EscapedPath]));

    Result.Success := True;
    Result.TotalPages := FConnection.ExecuteScalar('PRAGMA page_count');
    Result.DestinationSizeBytes := GetFileSize(NormalizedPath);

    if FDefaultOptions.VerifyAfterBackup then
      Result.Verified := VerifyBackupIntegrity(NormalizedPath);

  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;

  Result.ElapsedTimeMs := MilliSecondsBetween(Now, StartTime);
end;

function TNDXSQLiteBackup.RestoreFrom(const ASourcePath: string): TNDXBackupResult;
var
  StartTime: TDateTime;
  DBPath, NormalizedSource: string;
  WalPath, ShmPath: string;
  SourceSize, DestSize: Int64;
begin
  StartTime := Now;
  Result.Success := False;
  Result.Verified := False;

  // Normalize source path
  NormalizedSource := TNDXPlatform.NormalizePath(ASourcePath);

  try
    if not FileExists(NormalizedSource) then
    begin
      Result.ErrorMessage := 'Source file does not exist: ' + NormalizedSource;
      Exit;
    end;

    // Get source size for verification
    SourceSize := GetFileSize(NormalizedSource);

    DBPath := TNDXPlatform.NormalizePath(FConnection.DatabasePath);

    // Build WAL/SHM paths (cross-platform safe since simple suffixes)
    WalPath := DBPath + '-wal';
    ShmPath := DBPath + '-shm';

    // Close connection
    FConnection.Close;

    // Delete associated WAL/SHM files if present
    if FileExists(WalPath) then
    begin
      if not DeleteFile(WalPath) then
        raise Exception.Create('Unable to delete WAL file: ' + WalPath);
    end;
    if FileExists(ShmPath) then
    begin
      if not DeleteFile(ShmPath) then
        raise Exception.Create('Unable to delete SHM file: ' + ShmPath);
    end;

    // Replace file
    if FileExists(DBPath) then
    begin
      if not DeleteFile(DBPath) then
        raise Exception.Create('Unable to delete existing database: ' + DBPath);
    end;

    // Copy source file
    if not CopyFile(NormalizedSource, DBPath) then
      raise Exception.Create('Failed to copy file: ' + NormalizedSource + ' -> ' + DBPath);

    // Verify destination file exists and has correct size
    if not FileExists(DBPath) then
      raise Exception.Create('Destination file does not exist after copy');

    DestSize := GetFileSize(DBPath);
    if DestSize <> SourceSize then
      raise Exception.CreateFmt('Incorrect file size after copy: expected %d, got %d',
        [SourceSize, DestSize]);

    // Reopen
    FConnection.Open;

    Result.Success := True;
    Result.TotalPages := FConnection.ExecuteScalar('PRAGMA page_count');
    Result.DestinationSizeBytes := DestSize;

    // Verify integrity if requested
    if FDefaultOptions.VerifyAfterBackup then
      Result.Verified := VerifyBackupIntegrity(DBPath);

  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;

  Result.ElapsedTimeMs := MilliSecondsBetween(Now, StartTime);
end;

procedure TNDXSQLiteBackup.BackupToAsync(const ADestPath: string;
  AOnComplete: TNDXAsyncCallback;
  ACancellationToken: INDXCancellationToken);
var
  BackupThread: TNDXBackupThread;
begin
  BackupThread := TNDXBackupThread.Create(Self, ADestPath, AOnComplete, ACancellationToken);
  BackupThread.Start;
end;

function TNDXSQLiteBackup.CopyDatabase(const ASourcePath, ADestPath: string): TNDXBackupResult;
var
  StartTime: TDateTime;
  SourceConn: TNDXSQLiteConnection;
  NormalizedSource, NormalizedDest: string;
begin
  StartTime := Now;
  Result.Success := False;

  // Normalize paths
  NormalizedSource := TNDXPlatform.NormalizePath(ASourcePath);
  NormalizedDest := TNDXPlatform.NormalizePath(ADestPath);
  Result.DestinationPath := NormalizedDest;

  try
    if not FileExists(NormalizedSource) then
    begin
      Result.ErrorMessage := 'Source file does not exist: ' + NormalizedSource;
      Exit;
    end;

    // Ensure destination directory exists
    TNDXPlatform.EnsureDirectoryExists(ExtractFilePath(NormalizedDest));

    if FileExists(NormalizedDest) then
      DeleteFile(NormalizedDest);

    // Open source and use SQLite backup API
    SourceConn := TNDXSQLiteConnection.Create(NormalizedSource, False);
    try
      SourceConn.Open;
      SourceConn.BackupTo(NormalizedDest);
      Result.TotalPages := SourceConn.ExecuteScalar('PRAGMA page_count');
      SourceConn.Close;
    finally
      SourceConn.Free;
    end;

    Result.Success := True;
    Result.DestinationSizeBytes := GetFileSize(NormalizedDest);

    if FDefaultOptions.VerifyAfterBackup then
      Result.Verified := VerifyBackupIntegrity(NormalizedDest);

  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;

  Result.ElapsedTimeMs := MilliSecondsBetween(Now, StartTime);
end;

function TNDXSQLiteBackup.VerifyBackup(const ABackupPath: string): Boolean;
begin
  Result := VerifyBackupIntegrity(ABackupPath);
end;

function TNDXSQLiteBackup.CompareDatabases(const APath1, APath2: string): Boolean;
var
  Conn1, Conn2: TNDXSQLiteConnection;
  V1, V2: Variant;
  NormalizedPath1, NormalizedPath2: string;
begin
  Result := False;

  NormalizedPath1 := TNDXPlatform.NormalizePath(APath1);
  NormalizedPath2 := TNDXPlatform.NormalizePath(APath2);

  if not FileExists(NormalizedPath1) or not FileExists(NormalizedPath2) then
    Exit;

  Conn1 := TNDXSQLiteConnection.Create(NormalizedPath1, False);
  Conn2 := TNDXSQLiteConnection.Create(NormalizedPath2, False);
  try
    Conn1.Open;
    Conn2.Open;

    // Compare table count
    V1 := Conn1.ExecuteScalar(
      'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%''');
    V2 := Conn2.ExecuteScalar(
      'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%''');

    if Integer(V1) <> Integer(V2) then
      Exit;

    // Compare checksums (approximate)
    V1 := Conn1.ExecuteScalar('PRAGMA page_count');
    V2 := Conn2.ExecuteScalar('PRAGMA page_count');

    Result := Integer(V1) = Integer(V2);

    Conn1.Close;
    Conn2.Close;
  finally
    Conn1.Free;
    Conn2.Free;
  end;
end;

function TNDXSQLiteBackup.ListBackups(const ADirectory: string;
  const APattern: string): TStringList;
var
  SR: TSearchRec;
  NormalizedDir: string;
begin
  Result := TStringList.Create;

  // Normalize directory
  NormalizedDir := TNDXPlatform.NormalizePath(IncludeTrailingPathDelimiter(ADirectory));

  if not DirectoryExists(NormalizedDir) then
    Exit;

  if FindFirst(NormalizedDir + APattern, faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
        Result.Add(NormalizedDir + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  Result.Sort;
end;

procedure TNDXSQLiteBackup.RotateBackups(const ADirectory: string;
  AKeepCount: Integer; const APattern: string);
var
  Backups: TStringList;
  I: Integer;
begin
  Backups := ListBackups(ADirectory, APattern);
  try
    // Delete oldest backups
    while Backups.Count > AKeepCount do
    begin
      DeleteFile(Backups[0]);
      Backups.Delete(0);
    end;
  finally
    Backups.Free;
  end;
end;

function TNDXSQLiteBackup.GenerateBackupName(const ABaseName: string): string;
begin
  Result := Format('%s_%s.db',
    [ABaseName, FormatDateTime('yyyymmdd_hhnnss', Now)]);
end;

end.
