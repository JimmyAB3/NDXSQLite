# Example 04: Data Types

This example demonstrates working with various data types in SQLite.

## What you'll learn

- SQLite's five storage classes: NULL, INTEGER, REAL, TEXT, BLOB
- How to handle NULL values
- Boolean storage (as INTEGER)
- Date/time storage (as TEXT in ISO 8601 format)
- BLOB handling
- Type affinity in SQLite

## SQLite Data Types

SQLite uses dynamic typing with five storage classes:

| Storage Class | Description |
|--------------|-------------|
| NULL | Null value |
| INTEGER | Signed integer (1-8 bytes) |
| REAL | 8-byte IEEE floating point |
| TEXT | UTF-8/UTF-16 string |
| BLOB | Binary data |

## Key concepts

### Boolean values

SQLite has no native BOOLEAN type. Use INTEGER:
- `0` = False
- `1` = True

```pascal
Connection.ExecuteNonQuery(
  'INSERT INTO items (active) VALUES (?)', [1]); // True
```

### Date/Time handling

Store as ISO 8601 TEXT for portability:

```pascal
Connection.ExecuteNonQuery(
  'INSERT INTO events (created_at) VALUES (?)',
  [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
```

### NULL handling

```pascal
if DataSet.FieldByName('column').IsNull then
  WriteLn('Value is NULL')
else
  WriteLn(DataSet.FieldByName('column').AsString);
```

### BLOB data

```pascal
var BlobData: TBytes;
SetLength(BlobData, 100);
// Fill BlobData...
Connection.ExecuteNonQuery(
  'INSERT INTO files (data) VALUES (?)', [BlobData]);
```

## Building

```bash
lazbuild DataTypes.lpi
```

## Running

```bash
./DataTypes      # Linux/macOS
DataTypes.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
