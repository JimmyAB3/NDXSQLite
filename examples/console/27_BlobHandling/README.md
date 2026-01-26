# Example 27: BLOB Handling

This example demonstrates working with binary large objects (BLOBs) in SQLite.

## What you'll learn

- Storing binary data (BLOBs) in SQLite
- Hex notation for BLOB literals (X'...')
- Inserting BLOBs using parameters
- Reading BLOBs to TBytes using streams
- Partial BLOB reading with substr()
- BLOB functions: length(), hex(), zeroblob(), randomblob()
- NULL vs empty BLOB handling
- BLOB comparison

## Key concepts

### Insert BLOB using hex notation

```pascal
// X'...' syntax for hex-encoded binary data
Connection.ExecuteNonQuery(
  'INSERT INTO files (filename, content) ' +
  'VALUES (''hello.txt'', X''48656C6C6F20576F726C6421'')');
// 48656C6C6F20576F726C6421 = "Hello World!" in hex
```

### Insert BLOB using parameters

```pascal
var
  BinaryData: TBytes;
begin
  SetLength(BinaryData, 256);
  // Fill BinaryData...

  Connection.ExecuteNonQuery(
    'INSERT INTO files (filename, content) VALUES (?, ?)',
    ['data.bin', BinaryData]);
end;
```

### Read BLOB to TBytes

```pascal
var
  DS: TDataSet;
  Stream: TStream;
  Data: TBytes;
begin
  DS := Connection.ExecuteQuery('SELECT content FROM files WHERE id = 1');
  try
    Stream := DS.CreateBlobStream(DS.FieldByName('content'), bmRead);
    try
      SetLength(Data, Stream.Size);
      Stream.ReadBuffer(Data[0], Stream.Size);
    finally
      Stream.Free;
    end;
  finally
    DS.Free;
  end;
end;
```

### Partial BLOB reading

```pascal
// Read only portions of a BLOB (efficient for large files)
// substr(blob, start, length) - start is 1-indexed
DS := Connection.ExecuteQuery(
  'SELECT ' +
  '  hex(substr(content, 1, 8)) as first_8_bytes,' +
  '  hex(substr(content, -8)) as last_8_bytes ' +
  'FROM files WHERE id = 1');
```

### BLOB functions

```pascal
// Create zero-filled BLOB
Connection.ExecuteNonQuery('INSERT INTO t (data) VALUES (zeroblob(1000))');

// Create random BLOB
Connection.ExecuteNonQuery('INSERT INTO t (data) VALUES (randomblob(16))');

// Get BLOB size without loading content
DS := Connection.ExecuteQuery('SELECT length(content) as size FROM files');

// Convert BLOB to hex string
DS := Connection.ExecuteQuery('SELECT hex(content) as hex_data FROM files');
```

### NULL vs empty BLOB

```pascal
-- NULL BLOB (no data)
INSERT INTO files (content) VALUES (NULL);

-- Empty BLOB (zero-length data)
INSERT INTO files (content) VALUES (X'');

-- Zero-filled BLOB of specific size
INSERT INTO files (content) VALUES (zeroblob(100));
```

## BLOB functions reference

| Function | Description |
|----------|-------------|
| `length(blob)` | Returns size in bytes |
| `hex(blob)` | Returns hexadecimal string |
| `substr(blob, start, len)` | Extracts portion (1-indexed) |
| `zeroblob(n)` | Creates n zero-filled bytes |
| `randomblob(n)` | Creates n random bytes |
| `typeof(blob)` | Returns 'blob' |

## Building

```bash
lazbuild BlobHandling.lpi
```

## Running

```bash
./BlobHandling      # Linux/macOS
BlobHandling.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
