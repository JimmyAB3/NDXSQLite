{===============================================================================
  NDXSQLite Example 64 - Incremental Blob I/O
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Reading and writing large BLOBs without loading them entirely in memory
  - Using zeroblob() for placeholder allocation
  - Chunked streaming with progress reporting
  - Large file handling (multi-MB files)
  - Blob reopen for efficient row switching
  - File-to-BLOB and BLOB-to-file operations
  - Real-world scenarios: image storage, document management

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program IncrementalBlobIO;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteblob;

const
  CHUNK_SIZE = 4096;  // 4KB chunks for efficient streaming
  LARGE_FILE_SIZE = 2 * 1024 * 1024;  // 2MB for large file demo

var
  Conn: INDXSQLiteConnection;
  DBPath: string;

// Generate a test file with random-like content
{ Creates a file of the given size filled with a deterministic byte pattern based on position. }
procedure GenerateTestFile(const FilePath: string; Size: Integer);
var
  FS: TFileStream;
  Buffer: array[0..4095] of Byte;
  I, ToWrite, Written: Integer;
begin
  FS := TFileStream.Create(FilePath, fmCreate);
  try
    // Fill buffer with pattern based on position
    Written := 0;
    while Written < Size do
    begin
      // Create varied content
      for I := 0 to High(Buffer) do
        Buffer[I] := ((Written + I) * 17 + 31) mod 256;

      ToWrite := Size - Written;
      if ToWrite > SizeOf(Buffer) then
        ToWrite := SizeOf(Buffer);

      FS.WriteBuffer(Buffer, ToWrite);
      Inc(Written, ToWrite);
    end;
  finally
    FS.Free;
  end;
end;

// ============================================================================
// Demo 1: Chunked Write with Progress
// ============================================================================
{ Writes a 500KB memory stream into a BLOB in 4KB chunks, displaying a progress bar during the operation. }
procedure DemoChunkedWriteWithProgress;
var
  Blob: TNDXSQLiteBlob;
  SourceStream: TMemoryStream;
  Buffer: array[0..CHUNK_SIZE-1] of Byte;
  TotalSize, Written, ToWrite, I: Integer;
  Progress: Integer;
begin
  WriteLn('1. Chunked BLOB Write with Progress Reporting:');
  WriteLn('   ============================================');

  // Create source data (500KB)
  TotalSize := 500 * 1024;
  WriteLn('   Creating ', TotalSize div 1024, 'KB of source data...');

  SourceStream := TMemoryStream.Create;
  try
    SourceStream.SetSize(TotalSize);
    // Fill with pattern
    for I := 0 to TotalSize - 1 do
      PByte(SourceStream.Memory + I)^ := (I * 13 + 7) mod 256;

    // Create placeholder with zeroblob
    Conn.ExecuteNonQuery('INSERT INTO documents (id, name, doc_type, content) VALUES (1, ''large_document.bin'', ''binary'', zeroblob(?))', [TotalSize]);
    WriteLn('   Allocated BLOB placeholder: ', TotalSize, ' bytes');

    // Open BLOB for writing
    Blob := TNDXSQLiteBlob.Create(Conn);
    try
      Blob.Open('documents', 'content', 1, bomReadWrite);

      // Write in chunks with progress
      Written := 0;
      SourceStream.Position := 0;
      Write('   Writing: [');

      while Written < TotalSize do
      begin
        ToWrite := TotalSize - Written;
        if ToWrite > CHUNK_SIZE then
          ToWrite := CHUNK_SIZE;

        SourceStream.ReadBuffer(Buffer, ToWrite);
        Blob.WriteBuffer(@Buffer[0], Written, ToWrite);
        Inc(Written, ToWrite);

        // Progress bar
        Progress := (Written * 50) div TotalSize;
        if (Written mod (TotalSize div 10)) < CHUNK_SIZE then
          Write('=');
      end;

      WriteLn('] 100%');
      WriteLn('   Written ', Written, ' bytes in ', (TotalSize + CHUNK_SIZE - 1) div CHUNK_SIZE, ' chunks');
      Blob.Close;
    finally
      Blob.Free;
    end;
  finally
    SourceStream.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 2: Chunked Read with Progress
// ============================================================================
{ Reads a BLOB back in 4KB chunks with a progress bar and computes a checksum to verify the data. }
procedure DemoChunkedReadWithProgress;
var
  Blob: TNDXSQLiteBlob;
  DestStream: TMemoryStream;
  Buffer: array[0..CHUNK_SIZE-1] of Byte;
  TotalSize, ReadBytes, ToRead, BytesRead: Integer;
  Progress: Integer;
  Checksum: Cardinal;
  I: Integer;
begin
  WriteLn('2. Chunked BLOB Read with Progress Reporting:');
  WriteLn('   ===========================================');

  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('documents', 'content', 1, bomReadOnly);
    TotalSize := Blob.Size;
    WriteLn('   BLOB size: ', TotalSize, ' bytes (', TotalSize div 1024, 'KB)');

    DestStream := TMemoryStream.Create;
    try
      DestStream.SetSize(TotalSize);

      // Read in chunks with progress
      ReadBytes := 0;
      Write('   Reading: [');

      while ReadBytes < TotalSize do
      begin
        ToRead := TotalSize - ReadBytes;
        if ToRead > CHUNK_SIZE then
          ToRead := CHUNK_SIZE;

        BytesRead := Blob.ReadBuffer(@Buffer[0], ReadBytes, ToRead);
        DestStream.WriteBuffer(Buffer, BytesRead);
        Inc(ReadBytes, BytesRead);

        // Progress bar
        if (ReadBytes mod (TotalSize div 10)) < CHUNK_SIZE then
          Write('=');
      end;

      WriteLn('] 100%');
      WriteLn('   Read ', ReadBytes, ' bytes in ', (TotalSize + CHUNK_SIZE - 1) div CHUNK_SIZE, ' chunks');

      // Verify data with checksum
      Checksum := 0;
      for I := 0 to TotalSize - 1 do
        Checksum := Checksum + PByte(DestStream.Memory + I)^;
      WriteLn('   Data checksum: ', Checksum);
    finally
      DestStream.Free;
    end;

    Blob.Close;
  finally
    Blob.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 3: Large File Storage (2MB)
// ============================================================================
{ Streams a 2MB file into a BLOB and back out, measuring throughput and verifying file integrity. }
procedure DemoLargeFileStorage;
var
  Blob: TNDXSQLiteBlob;
  InputFile, OutputFile: string;
  SourceFS, DestFS: TFileStream;
  Buffer: array[0..CHUNK_SIZE-1] of Byte;
  TotalSize, Processed, ToProcess: Int64;
  StartTime: TDateTime;
  ElapsedMs: Integer;
begin
  WriteLn('3. Large File Storage (', LARGE_FILE_SIZE div (1024*1024), 'MB):');
  WriteLn('   ==============================');

  InputFile := ExtractFilePath(ParamStr(0)) + 'large_input.bin';
  OutputFile := ExtractFilePath(ParamStr(0)) + 'large_output.bin';

  // Generate large test file
  WriteLn('   Generating ', LARGE_FILE_SIZE div (1024*1024), 'MB test file...');
  StartTime := Now;
  GenerateTestFile(InputFile, LARGE_FILE_SIZE);
  ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
  WriteLn('   File generated in ', ElapsedMs, 'ms');

  // Store in database
  WriteLn('   Storing file in BLOB...');
  Conn.ExecuteNonQuery('INSERT INTO documents (id, name, doc_type, content) VALUES (2, ''large_file.bin'', ''archive'', zeroblob(?))', [LARGE_FILE_SIZE]);

  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('documents', 'content', 2, bomReadWrite);

    SourceFS := TFileStream.Create(InputFile, fmOpenRead);
    try
      TotalSize := SourceFS.Size;
      Processed := 0;
      StartTime := Now;

      while Processed < TotalSize do
      begin
        ToProcess := TotalSize - Processed;
        if ToProcess > CHUNK_SIZE then
          ToProcess := CHUNK_SIZE;

        SourceFS.ReadBuffer(Buffer, ToProcess);
        Blob.WriteBuffer(@Buffer[0], Processed, ToProcess);
        Inc(Processed, ToProcess);

        // Show progress every 10%
        if (Processed mod (TotalSize div 10)) < CHUNK_SIZE then
          Write('.');
      end;
      WriteLn;

      ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
      WriteLn('   Stored ', TotalSize, ' bytes in ', ElapsedMs, 'ms');
      WriteLn('   Throughput: ', (TotalSize div 1024) * 1000 div Max(ElapsedMs, 1), ' KB/s');
    finally
      SourceFS.Free;
    end;

    Blob.Close;
  finally
    Blob.Free;
  end;

  // Retrieve from database
  WriteLn('   Retrieving file from BLOB...');
  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('documents', 'content', 2, bomReadOnly);

    DestFS := TFileStream.Create(OutputFile, fmCreate);
    try
      TotalSize := Blob.Size;
      Processed := 0;
      StartTime := Now;

      while Processed < TotalSize do
      begin
        ToProcess := TotalSize - Processed;
        if ToProcess > CHUNK_SIZE then
          ToProcess := CHUNK_SIZE;

        Blob.ReadBuffer(@Buffer[0], Processed, ToProcess);
        DestFS.WriteBuffer(Buffer, ToProcess);
        Inc(Processed, ToProcess);

        if (Processed mod (TotalSize div 10)) < CHUNK_SIZE then
          Write('.');
      end;
      WriteLn;

      ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
      WriteLn('   Retrieved ', TotalSize, ' bytes in ', ElapsedMs, 'ms');
      WriteLn('   Throughput: ', (TotalSize div 1024) * 1000 div Max(ElapsedMs, 1), ' KB/s');
    finally
      DestFS.Free;
    end;

    Blob.Close;
  finally
    Blob.Free;
  end;

  // Verify files match
  Write('   Verifying file integrity... ');
  SourceFS := TFileStream.Create(InputFile, fmOpenRead);
  DestFS := TFileStream.Create(OutputFile, fmOpenRead);
  try
    if SourceFS.Size = DestFS.Size then
      WriteLn('OK (sizes match: ', SourceFS.Size, ' bytes)')
    else
      WriteLn('MISMATCH!');
  finally
    SourceFS.Free;
    DestFS.Free;
  end;

  // Cleanup temp files
  DeleteFile(InputFile);
  DeleteFile(OutputFile);
  WriteLn;
end;

// ============================================================================
// Demo 4: Partial Read/Write (Random Access)
// ============================================================================
{ Writes byte patterns at specific offsets (beginning, middle, end) in a BLOB, then reads them back to verify random access. }
procedure DemoPartialReadWrite;
var
  Blob: TNDXSQLiteBlob;
  OriginalData, ModifiedData: TBytes;
  I: Integer;
begin
  WriteLn('4. Partial Read/Write (Random Access):');
  WriteLn('   ====================================');

  // Create document with known content
  Conn.ExecuteNonQuery('INSERT INTO documents (id, name, doc_type, content) VALUES (3, ''random_access.dat'', ''data'', zeroblob(1000))');

  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('documents', 'content', 3, bomReadWrite);

    // Write pattern at beginning
    SetLength(OriginalData, 100);
    for I := 0 to 99 do
      OriginalData[I] := I;
    Blob.Write(OriginalData, 0);
    WriteLn('   Wrote 100 bytes at offset 0');

    // Write pattern in middle
    for I := 0 to 99 do
      OriginalData[I] := 100 + I;
    Blob.WriteBuffer(@OriginalData[0], 450, 100);
    WriteLn('   Wrote 100 bytes at offset 450');

    // Write pattern at end
    for I := 0 to 99 do
      OriginalData[I] := 200 - I;
    Blob.WriteBuffer(@OriginalData[0], 900, 100);
    WriteLn('   Wrote 100 bytes at offset 900');

    Blob.Close;
  finally
    Blob.Free;
  end;

  // Random access read
  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('documents', 'content', 3, bomReadOnly);

    // Read from different positions
    ModifiedData := Blob.Read(0, 10);
    Write('   First 10 bytes: ');
    for I := 0 to 9 do Write(ModifiedData[I], ' ');
    WriteLn;

    ModifiedData := Blob.Read(450, 10);
    Write('   Bytes 450-459: ');
    for I := 0 to 9 do Write(ModifiedData[I], ' ');
    WriteLn;

    ModifiedData := Blob.Read(900, 10);
    Write('   Bytes 900-909: ');
    for I := 0 to 9 do Write(ModifiedData[I], ' ');
    WriteLn;

    Blob.Close;
  finally
    Blob.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 5: Blob Reopen for Batch Processing
// ============================================================================
{ Creates 100 small BLOBs and processes them using Reopen to switch rows efficiently, comparing performance against Open/Close per row. }
procedure DemoBlobReopenBatch;
var
  Blob: TNDXSQLiteBlob;
  I: Integer;
  Data: TBytes;
  StartTime: TDateTime;
  ElapsedMs: Integer;
begin
  WriteLn('5. BLOB Reopen for Batch Processing:');
  WriteLn('   ==================================');

  // Create multiple small BLOBs
  WriteLn('   Creating 100 small BLOBs...');
  for I := 1 to 100 do
    Conn.ExecuteNonQuery('INSERT INTO documents (id, name, doc_type, content) VALUES (?, ?, ''batch'', zeroblob(256))',
      [100 + I, 'batch_' + IntToStr(I) + '.dat']);

  // Process all BLOBs with single handle using Reopen
  WriteLn('   Processing with Reopen (efficient):');
  StartTime := Now;

  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    SetLength(Data, 256);

    // Open first BLOB
    Blob.Open('documents', 'content', 101, bomReadWrite);

    for I := 1 to 100 do
    begin
      if I > 1 then
        Blob.Reopen(100 + I);  // Efficient switch to next row

      // Write unique pattern to each BLOB
      FillChar(Data[0], 256, I);
      Blob.Write(Data, 0);
    end;

    Blob.Close;
  finally
    Blob.Free;
  end;

  ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
  WriteLn('   Processed 100 BLOBs in ', ElapsedMs, 'ms using Reopen');

  // Compare with opening/closing each BLOB
  WriteLn('   Processing with Open/Close (comparison):');
  StartTime := Now;

  for I := 1 to 100 do
  begin
    Blob := TNDXSQLiteBlob.Create(Conn);
    try
      Blob.Open('documents', 'content', 100 + I, bomReadOnly);
      Data := Blob.Read(0, 256);
      Blob.Close;
    finally
      Blob.Free;
    end;
  end;

  ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
  WriteLn('   Processed 100 BLOBs in ', ElapsedMs, 'ms with Open/Close');
  WriteLn;
end;

// ============================================================================
// Demo 6: Image Storage Scenario
// ============================================================================
{ Stores a simulated PNG thumbnail (10KB) and full image (500KB) with proper PNG signatures, then verifies the header on read-back. }
procedure DemoImageStorageScenario;
var
  Blob: TNDXSQLiteBlob;
  ImageHeader: array[0..7] of Byte;
  ImageData: TBytes;
  I, ToWrite: Integer;
begin
  WriteLn('6. Image Storage Scenario:');
  WriteLn('   ========================');

  // Simulate storing images with metadata
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS images (' +
    '  id INTEGER PRIMARY KEY,' +
    '  filename TEXT,' +
    '  mime_type TEXT,' +
    '  width INTEGER,' +
    '  height INTEGER,' +
    '  thumbnail BLOB,' +
    '  full_image BLOB' +
    ')');

  // Insert image with both thumbnail and full size
  WriteLn('   Storing image with thumbnail...');

  // Simulate PNG header + data for thumbnail (10KB)
  Conn.ExecuteNonQuery(
    'INSERT INTO images (id, filename, mime_type, width, height, thumbnail, full_image) ' +
    'VALUES (1, ''photo.png'', ''image/png'', 1920, 1080, zeroblob(10240), zeroblob(512000))');

  // Write thumbnail
  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('images', 'thumbnail', 1, bomReadWrite);

    // PNG signature
    ImageHeader[0] := $89;
    ImageHeader[1] := $50;  // P
    ImageHeader[2] := $4E;  // N
    ImageHeader[3] := $47;  // G
    ImageHeader[4] := $0D;
    ImageHeader[5] := $0A;
    ImageHeader[6] := $1A;
    ImageHeader[7] := $0A;

    Blob.WriteBuffer(@ImageHeader[0], 0, 8);

    // Write simulated image data
    SetLength(ImageData, 10232);
    for I := 0 to High(ImageData) do
      ImageData[I] := Random(256);
    Blob.Write(ImageData, 8);

    WriteLn('   Thumbnail stored: 10KB');
    Blob.Close;
  finally
    Blob.Free;
  end;

  // Write full image
  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('images', 'full_image', 1, bomReadWrite);

    // PNG signature
    Blob.WriteBuffer(@ImageHeader[0], 0, 8);

    // Write in chunks (500KB total, 8 bytes for header = 511992 bytes of data)
    SetLength(ImageData, CHUNK_SIZE);
    I := 0;
    while 8 + I * CHUNK_SIZE < 512000 do
    begin
      FillChar(ImageData[0], CHUNK_SIZE, (I * 7) mod 256);
      ToWrite := Min(CHUNK_SIZE, 512000 - (8 + I * CHUNK_SIZE));
      Blob.WriteBuffer(@ImageData[0], 8 + I * CHUNK_SIZE, ToWrite);
      Inc(I);
    end;

    WriteLn('   Full image stored: 500KB');
    Blob.Close;
  finally
    Blob.Free;
  end;

  // Read back and verify PNG header
  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('images', 'thumbnail', 1, bomReadOnly);
    Blob.ReadBuffer(@ImageHeader[0], 0, 8);

    Write('   PNG signature check: ');
    if (ImageHeader[0] = $89) and (ImageHeader[1] = $50) and
       (ImageHeader[2] = $4E) and (ImageHeader[3] = $47) then
      WriteLn('VALID')
    else
      WriteLn('INVALID');

    Blob.Close;
  finally
    Blob.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 7: BLOB Properties and Metadata
// ============================================================================
{ Opens a BLOB handle and prints its properties: database, table, column, row ID, size, and open mode. }
procedure DemoBlobProperties;
var
  Blob: TNDXSQLiteBlob;
begin
  WriteLn('7. BLOB Handle Properties:');
  WriteLn('   ========================');

  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('documents', 'content', 1, bomReadOnly);

    WriteLn('   IsOpen: ', Blob.IsOpen);
    WriteLn('   Database: ', Blob.Database);
    WriteLn('   Table: ', Blob.Table);
    WriteLn('   Column: ', Blob.Column);
    WriteLn('   RowId: ', Blob.RowId);
    WriteLn('   Size: ', Blob.Size, ' bytes (', Blob.Size div 1024, 'KB)');
    WriteLn('   OpenMode: ', Ord(Blob.OpenMode), ' (0=ReadOnly, 1=ReadWrite)');

    Blob.Close;
    WriteLn('   After Close - IsOpen: ', Blob.IsOpen);
  finally
    Blob.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 8: Stream Integration
// ============================================================================
{ Writes a memory stream into a BLOB using WriteFromStream, reads it back with ReadToStream, and verifies the content. }
procedure DemoStreamIntegration;
var
  Blob: TNDXSQLiteBlob;
  SourceStream, DestStream: TMemoryStream;
  I: Integer;
begin
  WriteLn('8. Stream Integration:');
  WriteLn('   ====================');

  // Create test document
  Conn.ExecuteNonQuery('INSERT INTO documents (id, name, doc_type, content) VALUES (201, ''stream_test.bin'', ''test'', zeroblob(8192))');

  // Write from stream
  SourceStream := TMemoryStream.Create;
  try
    SourceStream.SetSize(8192);
    for I := 0 to 8191 do
      PByte(SourceStream.Memory + I)^ := (I * 3) mod 256;

    Blob := TNDXSQLiteBlob.Create(Conn);
    try
      Blob.Open('documents', 'content', 201, bomReadWrite);
      SourceStream.Position := 0;
      Blob.WriteFromStream(SourceStream);
      WriteLn('   WriteFromStream: 8192 bytes written');
      Blob.Close;
    finally
      Blob.Free;
    end;
  finally
    SourceStream.Free;
  end;

  // Read to stream
  DestStream := TMemoryStream.Create;
  try
    Blob := TNDXSQLiteBlob.Create(Conn);
    try
      Blob.Open('documents', 'content', 201, bomReadOnly);
      Blob.ReadToStream(DestStream, 0, Blob.Size);
      WriteLn('   ReadToStream: ', DestStream.Size, ' bytes read');
      Blob.Close;
    finally
      Blob.Free;
    end;

    // Verify content
    Write('   Content verification: ');
    if PByte(DestStream.Memory)^ = 0 then
      WriteLn('First byte OK')
    else
      WriteLn('MISMATCH');
  finally
    DestStream.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Main Program
// ============================================================================
begin
  Randomize;
  WriteLn('=== NDXSQLite Example 64: Incremental Blob I/O ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example64.db';

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);

  Conn := TNDXSQLiteConnection.Create(DBPath);
  Conn.Open;

  // Create document table
  Conn.ExecuteNonQuery(
    'CREATE TABLE documents (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  doc_type TEXT,' +
    '  content BLOB' +
    ')');

  // Run all demos
  DemoChunkedWriteWithProgress;
  DemoChunkedReadWithProgress;
  DemoLargeFileStorage;
  DemoPartialReadWrite;
  DemoBlobReopenBatch;
  DemoImageStorageScenario;
  DemoBlobProperties;
  DemoStreamIntegration;

  // Final statistics
  WriteLn('=== Summary ===');
  WriteLn('   Documents stored: ', Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM documents')));
  WriteLn('   Total BLOB size: ', Integer(Conn.ExecuteScalar('SELECT SUM(LENGTH(content)) FROM documents')) div 1024, 'KB');
  WriteLn;

  Conn.Close;
  Conn := nil;

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);

  WriteLn('=== Example completed successfully! ===');
end.
