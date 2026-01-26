{===============================================================================
  NDXSQLite Example 27 - BLOB Handling
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Storing binary data (BLOBs) in SQLite
  - Reading and writing files as BLOBs
  - Partial BLOB reading with substr()
  - BLOB size with length()
  - Hex encoding/decoding
  - NULL vs empty BLOB handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program BlobHandling;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Math,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS files (' +
    '  id INTEGER PRIMARY KEY,' +
    '  filename TEXT NOT NULL,' +
    '  content BLOB,' +
    '  mime_type TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS images (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  thumbnail BLOB,' +
    '  full_image BLOB' +
    ')');
end;

{ Inserts a BLOB using X'...' hex notation and queries it back, displaying the
  content as text, as hex, and its byte size. }
procedure DemoInsertBlobHex;
var
  DS: TDataSet;
begin
  WriteLn('1. Insert BLOB using hex notation');
  WriteLn('   -------------------------------');

  // X'...' syntax for hex-encoded binary data
  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content, mime_type) ' +
    'VALUES (''hello.txt'', X''48656C6C6F20576F726C6421'', ''text/plain'')');

  // X'48656C6C6F20576F726C6421' = "Hello World!" in hex

  DS := Connection.ExecuteQuery(
    'SELECT filename, content, hex(content) as hex_content, length(content) as size ' +
    'FROM files WHERE filename = ''hello.txt''');
  try
    WriteLn('   Filename: ', DS.FieldByName('filename').AsString);
    WriteLn('   Content as text: ', DS.FieldByName('content').AsString);
    WriteLn('   Content as hex: ', DS.FieldByName('hex_content').AsString);
    WriteLn('   Size: ', DS.FieldByName('size').AsInteger, ' bytes');
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Inserts a 256-byte binary array as a BLOB using a parameterized query and
  queries back its size and first 16 bytes in hex. }
procedure DemoInsertBlobParam;
var
  BinaryData: TBytes;
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('2. Insert BLOB using parameters');
  WriteLn('   -----------------------------');

  // Create binary data (could be image, PDF, etc.)
  SetLength(BinaryData, 256);
  for I := 0 to 255 do
    BinaryData[I] := Byte(I);

  // Insert using parameterized query
  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content, mime_type) VALUES (?, ?, ?)',
    ['binary_data.bin', BinaryData, 'application/octet-stream']);

  DS := Connection.ExecuteQuery(
    'SELECT filename, length(content) as size, hex(substr(content, 1, 16)) as first_16_bytes ' +
    'FROM files WHERE filename = ''binary_data.bin''');
  try
    WriteLn('   Filename: ', DS.FieldByName('filename').AsString);
    WriteLn('   Size: ', DS.FieldByName('size').AsInteger, ' bytes');
    WriteLn('   First 16 bytes (hex): ', DS.FieldByName('first_16_bytes').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Reads a BLOB field into a TBytes array via CreateBlobStream and displays
  the total byte count and first 16 bytes in hex. }
procedure DemoReadBlobToBytes;
var
  DS: TDataSet;
  BlobField: TField;
  Stream: TStream;
  Data: TBytes;
  I: Integer;
  HexStr: string;
begin
  WriteLn('3. Read BLOB to TBytes');
  WriteLn('   --------------------');

  DS := Connection.ExecuteQuery(
    'SELECT content FROM files WHERE filename = ''binary_data.bin''');
  try
    BlobField := DS.FieldByName('content');

    if not BlobField.IsNull then
    begin
      Stream := DS.CreateBlobStream(BlobField, bmRead);
      try
        SetLength(Data, Stream.Size);
        Stream.ReadBuffer(Data[0], Stream.Size);
        WriteLn('   Read ', Length(Data), ' bytes from BLOB');

        // Show first 16 bytes
        HexStr := '';
        for I := 0 to Min(15, High(Data)) do
          HexStr := HexStr + IntToHex(Data[I], 2) + ' ';
        WriteLn('   First bytes: ', HexStr);
      finally
        Stream.Free;
      end;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Inserts a 1000-byte random BLOB and reads specific portions using substr(),
  displaying the first, middle, and last 8 bytes in hex. }
procedure DemoPartialBlobRead;
var
  DS: TDataSet;
begin
  WriteLn('4. Partial BLOB reading (substr)');
  WriteLn('   ------------------------------');

  // Insert a larger blob
  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content, mime_type) VALUES ' +
    '(''large.dat'', randomblob(1000), ''application/octet-stream'')');

  // Read only portions using substr(blob, start, length)
  // Note: SQLite substr is 1-indexed
  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  length(content) as total_size,' +
    '  hex(substr(content, 1, 8)) as first_8,' +
    '  hex(substr(content, 500, 8)) as middle_8,' +
    '  hex(substr(content, -8)) as last_8 ' +
    'FROM files WHERE filename = ''large.dat''');
  try
    WriteLn('   Total size: ', DS.FieldByName('total_size').AsInteger, ' bytes');
    WriteLn('   First 8 bytes: ', DS.FieldByName('first_8').AsString);
    WriteLn('   Middle 8 bytes (from 500): ', DS.FieldByName('middle_8').AsString);
    WriteLn('   Last 8 bytes: ', DS.FieldByName('last_8').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Queries SQLite BLOB functions (zeroblob, randomblob, typeof) and displays
  their hex output, lengths, and type information. }
procedure DemoBlobFunctions;
var
  DS: TDataSet;
begin
  WriteLn('5. BLOB functions');
  WriteLn('   ---------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  zeroblob(10) as zero_blob,' +
    '  hex(zeroblob(10)) as zero_hex,' +
    '  length(zeroblob(10)) as zero_len,' +
    '  hex(randomblob(8)) as random_blob,' +
    '  typeof(X''CAFE'') as blob_type');
  try
    WriteLn('   zeroblob(10) hex: ', DS.FieldByName('zero_hex').AsString);
    WriteLn('   zeroblob(10) length: ', DS.FieldByName('zero_len').AsInteger);
    WriteLn('   randomblob(8) hex: ', DS.FieldByName('random_blob').AsString);
    WriteLn('   typeof(X''CAFE''): ', DS.FieldByName('blob_type').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Inserts NULL, empty (X''), and zeroblob(0) BLOBs, then compares their
  IS NULL status, length, and typeof values in a formatted table. }
procedure DemoNullVsEmptyBlob;
var
  DS: TDataSet;
begin
  WriteLn('6. NULL vs empty BLOB');
  WriteLn('   -------------------');

  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content) VALUES (''null_blob.dat'', NULL)');
  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content) VALUES (''empty_blob.dat'', X'''')');
  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content) VALUES (''zero_blob.dat'', zeroblob(0))');

  DS := Connection.ExecuteQuery(
    'SELECT filename, ' +
    '  content IS NULL as is_null,' +
    '  length(content) as len,' +
    '  typeof(content) as type ' +
    'FROM files WHERE filename LIKE ''%_blob.dat'' ORDER BY filename');
  try
    WriteLn('   Filename            | IS NULL | Length | Type');
    WriteLn('   --------------------|---------|--------|------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-19s | %-7s | %-6s | %s', [
        DS.FieldByName('filename').AsString,
        BoolToStr(DS.FieldByName('is_null').AsBoolean, 'true', 'false'),
        DS.FieldByName('len').AsString,
        DS.FieldByName('type').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Inserts simulated image records with thumbnail and full-size BLOBs, then
  queries their sizes and computes total image storage used. }
procedure DemoImageStorage;
var
  DS: TDataSet;
begin
  WriteLn('7. Image storage pattern (thumbnail + full)');
  WriteLn('   -----------------------------------------');

  // Simulate storing an image with thumbnail
  // In real app, you'd resize the image to create thumbnail
  Connection.ExecuteNonQuery(
    'INSERT INTO images (name, thumbnail, full_image) VALUES ' +
    '(''photo1.jpg'', randomblob(5000), randomblob(500000))');

  Connection.ExecuteNonQuery(
    'INSERT INTO images (name, thumbnail, full_image) VALUES ' +
    '(''photo2.jpg'', randomblob(4500), randomblob(750000))');

  // Query that only fetches thumbnails (efficient for listing)
  DS := Connection.ExecuteQuery(
    'SELECT name, length(thumbnail) as thumb_size, length(full_image) as full_size ' +
    'FROM images');
  try
    WriteLn('   Image listings (without loading full images):');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: thumbnail=%d bytes, full=%d bytes', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('thumb_size').AsInteger,
        DS.FieldByName('full_size').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Total storage used
  DS := Connection.ExecuteQuery(
    'SELECT SUM(length(thumbnail) + length(full_image)) as total_bytes FROM images');
  try
    WriteLn('   Total image storage: ', DS.FieldByName('total_bytes').AsInteger, ' bytes');
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Inserts BLOB values and tests equality comparisons between identical and
  differing BLOB contents using SQL subquery expressions. }
procedure DemoBlobComparison;
var
  DS: TDataSet;
begin
  WriteLn('8. BLOB comparison');
  WriteLn('   ----------------');

  Connection.ExecuteNonQuery('DELETE FROM files WHERE filename LIKE ''compare%''');

  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content) VALUES (''compare1.dat'', X''AABBCCDD'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content) VALUES (''compare2.dat'', X''AABBCCDD'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content) VALUES (''compare3.dat'', X''AABBCCEE'')');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  (SELECT content FROM files WHERE filename=''compare1.dat'') = ' +
    '  (SELECT content FROM files WHERE filename=''compare2.dat'') as same_12,' +
    '  (SELECT content FROM files WHERE filename=''compare1.dat'') = ' +
    '  (SELECT content FROM files WHERE filename=''compare3.dat'') as same_13');
  try
    WriteLn('   compare1 = compare2 (same content): ', DS.FieldByName('same_12').AsBoolean);
    WriteLn('   compare1 = compare3 (different): ', DS.FieldByName('same_13').AsBoolean);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 27: BLOB Handling ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example27.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      DemoInsertBlobHex;
      DemoInsertBlobParam;
      DemoReadBlobToBytes;
      DemoPartialBlobRead;
      DemoBlobFunctions;
      DemoNullVsEmptyBlob;
      DemoImageStorage;
      DemoBlobComparison;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
