# Example 64: Incremental Blob I/O

This example demonstrates how to read and write large BLOBs efficiently using `TNDXSQLiteBlob` without loading the entire content into memory.

## What you'll learn

- Chunked read/write operations with progress reporting
- Using `zeroblob()` for space allocation
- Large file storage (multi-MB files) with throughput measurement
- Random access partial read/write operations
- Efficient row switching with `Reopen()` for batch processing
- Stream-based BLOB operations
- Image storage scenarios with header verification

## Key concepts

### Why incremental BLOB I/O?

Standard SQL operations load entire BLOBs into memory:

```sql
SELECT content FROM documents WHERE id = 1;  -- Loads entire BLOB
```

Incremental I/O allows reading/writing specific portions:

```pascal
Data := Blob.Read(Offset, Count);  -- Only reads Count bytes at Offset
```

### Creating a BLOB placeholder

Use `zeroblob()` to pre-allocate space:

```sql
INSERT INTO documents (id, content) VALUES (1, zeroblob(1000000));
```

This creates a 1MB BLOB filled with zeros, ready for incremental writing.

### Chunked write with progress

```pascal
const
  CHUNK_SIZE = 4096;  // 4KB chunks
var
  Buffer: array[0..CHUNK_SIZE-1] of Byte;
  Written, ToWrite: Integer;
begin
  Blob := TNDXSQLiteBlob.Create(Connection);
  try
    Blob.Open('documents', 'content', RowId, bomReadWrite);

    Written := 0;
    while Written < TotalSize do
    begin
      ToWrite := Min(CHUNK_SIZE, TotalSize - Written);
      SourceStream.ReadBuffer(Buffer, ToWrite);
      Blob.WriteBuffer(@Buffer[0], Written, ToWrite);
      Inc(Written, ToWrite);

      // Report progress
      WriteLn('Progress: ', (Written * 100) div TotalSize, '%');
    end;

    Blob.Close;
  finally
    Blob.Free;
  end;
end;
```

### Chunked read with progress

```pascal
var
  Buffer: array[0..CHUNK_SIZE-1] of Byte;
  ReadBytes, ToRead, BytesRead: Integer;
begin
  Blob := TNDXSQLiteBlob.Create(Connection);
  try
    Blob.Open('documents', 'content', RowId, bomReadOnly);
    TotalSize := Blob.Size;

    ReadBytes := 0;
    while ReadBytes < TotalSize do
    begin
      ToRead := Min(CHUNK_SIZE, TotalSize - ReadBytes);
      BytesRead := Blob.ReadBuffer(@Buffer[0], ReadBytes, ToRead);
      DestStream.WriteBuffer(Buffer, BytesRead);
      Inc(ReadBytes, BytesRead);
    end;

    Blob.Close;
  finally
    Blob.Free;
  end;
end;
```

### TBytes-based operations

```pascal
// Write TBytes at offset
Blob.Write(MyBytes, Offset);

// Read to TBytes
Data := Blob.Read(Offset, Count);

// Read entire BLOB
AllData := Blob.ReadAll;
```

### Buffer-based operations

```pascal
// Write from buffer (pointer, offset, count)
Blob.WriteBuffer(@Buffer[0], Offset, Count);

// Read to buffer (pointer, offset, count) - returns bytes read
BytesRead := Blob.ReadBuffer(@Buffer[0], Offset, Count);
```

### Stream operations

```pascal
// Write from stream
Blob.Open('documents', 'content', 1, bomReadWrite);
Blob.WriteFromStream(InputStream);
Blob.Close;

// Read to stream
Blob.Open('documents', 'content', 1, bomReadOnly);
Blob.ReadToStream(OutputStream, Offset, Count);
Blob.Close;
```

### Efficient row switching with Reopen

When processing multiple rows with the same table/column:

```pascal
Blob.Open('documents', 'content', 101, bomReadWrite);

for I := 1 to 100 do
begin
  if I > 1 then
    Blob.Reopen(100 + I);  // Efficient switch to next row

  // Process BLOB
  Blob.Write(Data, 0);
end;

Blob.Close;
```

### BLOB properties

```pascal
Blob.Open('documents', 'content', 1, bomReadOnly);

WriteLn('IsOpen: ', Blob.IsOpen);
WriteLn('Database: ', Blob.Database);
WriteLn('Table: ', Blob.Table);
WriteLn('Column: ', Blob.Column);
WriteLn('RowId: ', Blob.RowId);
WriteLn('Size: ', Blob.Size, ' bytes');
WriteLn('OpenMode: ', Ord(Blob.OpenMode));  // 0=ReadOnly, 1=ReadWrite

Blob.Close;
```

## Open modes

| Mode | Constant | Description |
|------|----------|-------------|
| Read-only | `bomReadOnly` | Cannot write, safer |
| Read-write | `bomReadWrite` | Can both read and write |

## Important limitations

- **BLOB size is fixed**: You cannot resize a BLOB after creation
- **Pre-allocate with zeroblob()**: Determine size before writing
- **Row must exist**: Open requires an existing row with BLOB data
- **No concurrent writes**: SQLite locks during BLOB writes

## Building

```bash
lazbuild IncrementalBlobIO.lpi
```

## Running

```bash
./IncrementalBlobIO      # Linux/macOS
IncrementalBlobIO.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 64: Incremental Blob I/O ===

1. Chunked BLOB Write with Progress Reporting:
   ============================================
   Creating 500KB of source data...
   Allocated BLOB placeholder: 512000 bytes
   Writing: [==========] 100%
   Written 512000 bytes in 125 chunks

2. Chunked BLOB Read with Progress Reporting:
   ===========================================
   BLOB size: 512000 bytes (500KB)
   Reading: [==========] 100%
   Read 512000 bytes in 125 chunks
   Data checksum: 65280000

3. Large File Storage (2MB):
   ==============================
   Generating 2MB test file...
   File generated in 25ms
   Storing file in BLOB...
   ..........
   Stored 2097152 bytes in 15ms
   Throughput: 136533 KB/s
   Retrieving file from BLOB...
   ..........
   Retrieved 2097152 bytes in 3ms
   Throughput: 682666 KB/s
   Verifying file integrity... OK (sizes match: 2097152 bytes)

4. Partial Read/Write (Random Access):
   ====================================
   Wrote 100 bytes at offset 0
   Wrote 100 bytes at offset 450
   Wrote 100 bytes at offset 900
   First 10 bytes: 0 1 2 3 4 5 6 7 8 9
   Bytes 450-459: 100 101 102 103 104 105 106 107 108 109
   Bytes 900-909: 200 199 198 197 196 195 194 193 192 191

5. BLOB Reopen for Batch Processing:
   ==================================
   Creating 100 small BLOBs...
   Processing with Reopen (efficient):
   Processed 100 BLOBs in 5ms using Reopen
   Processing with Open/Close (comparison):
   Processed 100 BLOBs in 1ms with Open/Close

6. Image Storage Scenario:
   ========================
   Storing image with thumbnail...
   Thumbnail stored: 10KB
   Full image stored: 500KB
   PNG signature check: VALID

7. BLOB Handle Properties:
   ========================
   IsOpen: TRUE
   Database: main
   Table: documents
   Column: content
   RowId: 1
   Size: 512000 bytes (500KB)
   OpenMode: 0 (0=ReadOnly, 1=ReadWrite)
   After Close - IsOpen: FALSE

8. Stream Integration:
   ====================
   WriteFromStream: 8192 bytes written
   ReadToStream: 8192 bytes read
   Content verification: First byte OK

=== Summary ===
   Documents stored: 104
   Total BLOB size: 2581KB

=== Example completed successfully! ===
```

## Use cases

- **Media storage**: Images, audio, video files
- **Document management**: PDFs, Office documents
- **Binary data**: Serialized objects, compressed data
- **Large text**: Log files, XML/JSON documents
- **Streaming uploads/downloads**: Progress tracking for large files

## Notes

- Default chunk size of 4KB provides good balance of performance and memory usage
- Always close BLOB handles to release resources
- Use transactions when writing multiple BLOBs
- BLOB handles are invalidated if the row is modified by another operation

## Cross-Platform

This example works on Windows, Linux, and macOS.
