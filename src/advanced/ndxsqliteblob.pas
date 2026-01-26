{===============================================================================
  NDXSQLite - Incremental Blob I/O
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for incremental BLOB I/O operations.
  Allows streaming read/write of large BLOBs without loading them entirely
  into memory.
===============================================================================}
unit ndxsqliteblob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Blob open mode }
  TNDXBlobOpenMode = (
    bomReadOnly,   // Open for reading only
    bomReadWrite   // Open for reading and writing
  );

  { Incremental Blob I/O Handle }
  TNDXSQLiteBlob = class
  private
    FConnection: INDXSQLiteConnection;
    FBlobHandle: Psqlite3_blob;
    FDatabase: string;
    FTable: string;
    FColumn: string;
    FRowId: Int64;
    FOpenMode: TNDXBlobOpenMode;
    FIsOpen: Boolean;

    function GetDBHandle: Psqlite3;
    function GetSize: Integer;
    procedure CheckOpen;
    procedure CheckResult(AResult: Integer; const AOperation: string);

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Opens a BLOB field for incremental I/O on the specified table, column, and row. }
    procedure Open(const ATable, AColumn: string; ARowId: Int64;
      AMode: TNDXBlobOpenMode = bomReadOnly; const ADatabase: string = 'main');

    { Closes the current BLOB handle and releases associated resources. }
    procedure Close;

    { Repositions the open BLOB handle to a different row without closing and reopening. }
    procedure Reopen(ARowId: Int64);

    { Reads ACount bytes starting at AOffset from the BLOB and returns them as a byte array. }
    function Read(AOffset, ACount: Integer): TBytes;

    { Reads ACount bytes at AOffset into the provided buffer. Returns the number of bytes read. }
    function ReadBuffer(ABuffer: Pointer; AOffset, ACount: Integer): Integer;

    { Reads the entire BLOB content and returns it as a byte array. }
    function ReadAll: TBytes;

    { Writes the byte array to the BLOB starting at the specified offset. }
    procedure Write(const AData: TBytes; AOffset: Integer = 0);

    { Writes ACount bytes from the buffer to the BLOB at the specified offset. }
    procedure WriteBuffer(ABuffer: Pointer; AOffset, ACount: Integer);

    { Reads BLOB data into a stream, optionally specifying offset and byte count. }
    procedure ReadToStream(AStream: TStream; AOffset: Integer = 0;
      ACount: Integer = -1);

    { Writes stream content to the BLOB, optionally specifying offset and byte count. }
    procedure WriteFromStream(AStream: TStream; AOffset: Integer = 0;
      ACount: Integer = -1);

    { Reads the entire BLOB and saves it to a file. }
    procedure ReadToFile(const AFilePath: string);

    { Reads a file and writes its content to the BLOB. }
    procedure WriteFromFile(const AFilePath: string);

    { Properties }
    property Connection: INDXSQLiteConnection read FConnection;
    property IsOpen: Boolean read FIsOpen;
    property Size: Integer read GetSize;
    property Database: string read FDatabase;
    property Table: string read FTable;
    property Column: string read FColumn;
    property RowId: Int64 read FRowId;
    property OpenMode: TNDXBlobOpenMode read FOpenMode;
  end;

  { Blob utilities }
  TNDXSQLiteBlobUtils = class
  public
    { Allocates a zero-filled BLOB of the specified size for later incremental writing. }
    class procedure CreateZeroBlob(AConnection: INDXSQLiteConnection;
      const ATable, AColumn: string; ARowId: Int64; ASize: Integer;
      const ADatabase: string = 'main');

    { Returns the size in bytes of the BLOB at the specified row without opening it. }
    class function GetBlobSize(AConnection: INDXSQLiteConnection;
      const ATable, AColumn: string; ARowId: Int64;
      const ADatabase: string = 'main'): Int64;

    { Copies the entire BLOB value from one row to another within the same table. }
    class procedure CopyBlob(AConnection: INDXSQLiteConnection;
      const ATable, AColumn: string; ASourceRowId, ADestRowId: Int64;
      const ADatabase: string = 'main');

    { Copies a BLOB between rows using chunked streaming to limit memory usage. }
    class procedure CopyBlobStreaming(AConnection: INDXSQLiteConnection;
      const ATable, AColumn: string; ASourceRowId, ADestRowId: Int64;
      AChunkSize: Integer = 65536; const ADatabase: string = 'main');
  end;

implementation

const
  DEFAULT_CHUNK_SIZE = 65536; // 64KB chunks for streaming

{ TNDXSQLiteBlob }

constructor TNDXSQLiteBlob.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FBlobHandle := nil;
  FIsOpen := False;
end;

destructor TNDXSQLiteBlob.Destroy;
begin
  if FIsOpen then
    Close;
  inherited Destroy;
end;

function TNDXSQLiteBlob.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

function TNDXSQLiteBlob.GetSize: Integer;
begin
  CheckOpen;
  Result := sqlite3_blob_bytes(FBlobHandle);
end;

procedure TNDXSQLiteBlob.CheckOpen;
begin
  if not FIsOpen then
    raise ENDXSQLiteException.Create('BLOB is not open');
end;

procedure TNDXSQLiteBlob.CheckResult(AResult: Integer; const AOperation: string);
begin
  if AResult <> SQLITE_OK then
    raise ENDXSQLiteException.CreateFmt('BLOB %s failed: %s',
      [AOperation, string(sqlite3_errmsg(GetDBHandle))]);
end;

procedure TNDXSQLiteBlob.Open(const ATable, AColumn: string; ARowId: Int64;
  AMode: TNDXBlobOpenMode; const ADatabase: string);
var
  Flags: Integer;
  RC: Integer;
  DbAnsi, TableAnsi, ColumnAnsi: AnsiString;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to access BLOBs');

  // Close existing if open
  if FIsOpen then
    Close;

  // Determine flags
  if AMode = bomReadWrite then
    Flags := 1
  else
    Flags := 0;

  // Open the BLOB
  DbAnsi := AnsiString(ADatabase);
  TableAnsi := AnsiString(ATable);
  ColumnAnsi := AnsiString(AColumn);

  RC := sqlite3_blob_open(
    GetDBHandle,
    PAnsiChar(DbAnsi),
    PAnsiChar(TableAnsi),
    PAnsiChar(ColumnAnsi),
    ARowId,
    Flags,
    FBlobHandle
  );

  CheckResult(RC, 'open');

  FDatabase := ADatabase;
  FTable := ATable;
  FColumn := AColumn;
  FRowId := ARowId;
  FOpenMode := AMode;
  FIsOpen := True;
end;

procedure TNDXSQLiteBlob.Close;
var
  RC: Integer;
begin
  if not FIsOpen then
    Exit;

  RC := sqlite3_blob_close(FBlobHandle);
  FBlobHandle := nil;
  FIsOpen := False;

  // Don't throw on close errors, just log
  if RC <> SQLITE_OK then
    ; // Could log warning here
end;

procedure TNDXSQLiteBlob.Reopen(ARowId: Int64);
var
  RC: Integer;
begin
  CheckOpen;

  RC := sqlite3_blob_reopen(FBlobHandle, ARowId);
  CheckResult(RC, 'reopen');

  FRowId := ARowId;
end;

function TNDXSQLiteBlob.Read(AOffset, ACount: Integer): TBytes;
var
  RC: Integer;
  BlobSize: Integer;
begin
  CheckOpen;

  BlobSize := Size;

  // Validate offset
  if AOffset < 0 then
    AOffset := 0;
  if AOffset >= BlobSize then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // Adjust count if needed
  if ACount < 0 then
    ACount := BlobSize - AOffset;
  if AOffset + ACount > BlobSize then
    ACount := BlobSize - AOffset;

  SetLength(Result, ACount);
  if ACount > 0 then
  begin
    RC := sqlite3_blob_read(FBlobHandle, @Result[0], ACount, AOffset);
    CheckResult(RC, 'read');
  end;
end;

function TNDXSQLiteBlob.ReadBuffer(ABuffer: Pointer; AOffset, ACount: Integer): Integer;
var
  RC: Integer;
  BlobSize: Integer;
begin
  CheckOpen;

  BlobSize := Size;

  // Validate offset
  if (AOffset < 0) or (AOffset >= BlobSize) then
  begin
    Result := 0;
    Exit;
  end;

  // Adjust count if needed
  if AOffset + ACount > BlobSize then
    ACount := BlobSize - AOffset;

  Result := ACount;
  if ACount > 0 then
  begin
    RC := sqlite3_blob_read(FBlobHandle, ABuffer, ACount, AOffset);
    CheckResult(RC, 'read');
  end;
end;

function TNDXSQLiteBlob.ReadAll: TBytes;
begin
  Result := Read(0, -1);
end;

procedure TNDXSQLiteBlob.Write(const AData: TBytes; AOffset: Integer);
var
  RC: Integer;
begin
  CheckOpen;

  if FOpenMode = bomReadOnly then
    raise ENDXSQLiteException.Create('BLOB is opened in read-only mode');

  if Length(AData) = 0 then
    Exit;

  RC := sqlite3_blob_write(FBlobHandle, @AData[0], Length(AData), AOffset);
  CheckResult(RC, 'write');
end;

procedure TNDXSQLiteBlob.WriteBuffer(ABuffer: Pointer; AOffset, ACount: Integer);
var
  RC: Integer;
begin
  CheckOpen;

  if FOpenMode = bomReadOnly then
    raise ENDXSQLiteException.Create('BLOB is opened in read-only mode');

  if ACount <= 0 then
    Exit;

  RC := sqlite3_blob_write(FBlobHandle, ABuffer, ACount, AOffset);
  CheckResult(RC, 'write');
end;

procedure TNDXSQLiteBlob.ReadToStream(AStream: TStream; AOffset: Integer;
  ACount: Integer);
var
  Buffer: TBytes;
  BlobSize, BytesToRead, BytesRead: Integer;
  CurrentOffset: Integer;
begin
  CheckOpen;

  BlobSize := Size;
  if ACount < 0 then
    ACount := BlobSize - AOffset;
  if AOffset + ACount > BlobSize then
    ACount := BlobSize - AOffset;

  SetLength(Buffer, DEFAULT_CHUNK_SIZE);
  CurrentOffset := AOffset;

  while ACount > 0 do
  begin
    BytesToRead := ACount;
    if BytesToRead > DEFAULT_CHUNK_SIZE then
      BytesToRead := DEFAULT_CHUNK_SIZE;

    BytesRead := ReadBuffer(@Buffer[0], CurrentOffset, BytesToRead);
    if BytesRead > 0 then
      AStream.WriteBuffer(Buffer[0], BytesRead);

    Inc(CurrentOffset, BytesRead);
    Dec(ACount, BytesRead);

    if BytesRead < BytesToRead then
      Break;
  end;
end;

procedure TNDXSQLiteBlob.WriteFromStream(AStream: TStream; AOffset: Integer;
  ACount: Integer);
var
  Buffer: TBytes;
  BytesToWrite, BytesRead: Integer;
  CurrentOffset: Integer;
begin
  CheckOpen;

  if FOpenMode = bomReadOnly then
    raise ENDXSQLiteException.Create('BLOB is opened in read-only mode');

  if ACount < 0 then
    ACount := AStream.Size - AStream.Position;

  SetLength(Buffer, DEFAULT_CHUNK_SIZE);
  CurrentOffset := AOffset;

  while ACount > 0 do
  begin
    BytesToWrite := ACount;
    if BytesToWrite > DEFAULT_CHUNK_SIZE then
      BytesToWrite := DEFAULT_CHUNK_SIZE;

    BytesRead := AStream.Read(Buffer[0], BytesToWrite);
    if BytesRead <= 0 then
      Break;

    WriteBuffer(@Buffer[0], CurrentOffset, BytesRead);

    Inc(CurrentOffset, BytesRead);
    Dec(ACount, BytesRead);
  end;
end;

procedure TNDXSQLiteBlob.ReadToFile(const AFilePath: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFilePath, fmCreate);
  try
    ReadToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TNDXSQLiteBlob.WriteFromFile(const AFilePath: string);
var
  FS: TFileStream;
begin
  if not FileExists(AFilePath) then
    raise ENDXSQLiteException.CreateFmt('File not found: %s', [AFilePath]);

  FS := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    WriteFromStream(FS);
  finally
    FS.Free;
  end;
end;

{ TNDXSQLiteBlobUtils }

class procedure TNDXSQLiteBlobUtils.CreateZeroBlob(AConnection: INDXSQLiteConnection;
  const ATable, AColumn: string; ARowId: Int64; ASize: Integer;
  const ADatabase: string);
begin
  AConnection.ExecuteNonQuery(Format(
    'UPDATE %s.%s SET %s = zeroblob(%d) WHERE rowid = %d',
    [ADatabase, ATable, AColumn, ASize, ARowId]));
end;

class function TNDXSQLiteBlobUtils.GetBlobSize(AConnection: INDXSQLiteConnection;
  const ATable, AColumn: string; ARowId: Int64;
  const ADatabase: string): Int64;
var
  V: Variant;
begin
  V := AConnection.ExecuteScalar(Format('SELECT length(%s) FROM %s.%s WHERE rowid = %d',
    [AColumn, ADatabase, ATable, ARowId]));

  if VarIsNull(V) then
    Result := 0
  else
    Result := V;
end;

class procedure TNDXSQLiteBlobUtils.CopyBlob(AConnection: INDXSQLiteConnection;
  const ATable, AColumn: string; ASourceRowId, ADestRowId: Int64;
  const ADatabase: string);
begin
  AConnection.ExecuteNonQuery(Format(
    'UPDATE %s.%s SET %s = (SELECT %s FROM %s.%s WHERE rowid = %d) WHERE rowid = %d',
    [ADatabase, ATable, AColumn, AColumn, ADatabase, ATable, ASourceRowId, ADestRowId]));
end;

class procedure TNDXSQLiteBlobUtils.CopyBlobStreaming(AConnection: INDXSQLiteConnection;
  const ATable, AColumn: string; ASourceRowId, ADestRowId: Int64;
  AChunkSize: Integer; const ADatabase: string);
var
  SourceBlob, DestBlob: TNDXSQLiteBlob;
  BlobSize: Integer;
  Buffer: TBytes;
  Offset, BytesToCopy: Integer;
begin
  // First, create a zeroblob of the same size at destination
  SourceBlob := TNDXSQLiteBlob.Create(AConnection);
  try
    SourceBlob.Open(ATable, AColumn, ASourceRowId, bomReadOnly, ADatabase);
    BlobSize := SourceBlob.Size;
    SourceBlob.Close;
  finally
    SourceBlob.Free;
  end;

  // Create zeroblob placeholder
  CreateZeroBlob(AConnection, ATable, AColumn, ADestRowId, BlobSize, ADatabase);

  // Now copy in chunks
  SourceBlob := TNDXSQLiteBlob.Create(AConnection);
  DestBlob := TNDXSQLiteBlob.Create(AConnection);
  try
    SourceBlob.Open(ATable, AColumn, ASourceRowId, bomReadOnly, ADatabase);
    DestBlob.Open(ATable, AColumn, ADestRowId, bomReadWrite, ADatabase);

    SetLength(Buffer, AChunkSize);
    Offset := 0;

    while Offset < BlobSize do
    begin
      BytesToCopy := BlobSize - Offset;
      if BytesToCopy > AChunkSize then
        BytesToCopy := AChunkSize;

      SourceBlob.ReadBuffer(@Buffer[0], Offset, BytesToCopy);
      DestBlob.WriteBuffer(@Buffer[0], Offset, BytesToCopy);

      Inc(Offset, BytesToCopy);
    end;
  finally
    SourceBlob.Free;
    DestBlob.Free;
  end;
end;

end.
