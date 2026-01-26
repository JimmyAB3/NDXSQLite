# NDXSQLite GUI Example 08 - BLOB and Image Management

A demonstration of storing, displaying, and retrieving binary large objects (BLOBs) including images and documents in SQLite.

## Overview

This example shows how to work with binary data in SQLite using `TNDXSQLiteDataSet`. BLOBs are commonly used for storing images, documents, audio files, and other binary content directly in the database. The example includes automatic image display using `TDBImage` and manual file import/export functionality.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| BLOB Storage | Store binary data up to 2GB in SQLite |
| TDBImage Integration | Automatic image display from BLOB fields |
| File Import | Load images and files from disk into database |
| File Export | Save BLOB content back to files |
| File Type Detection | Identify file types from binary headers |
| Metadata Tracking | Store file name, type, and size with BLOB |

## Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                         TfrmMain                                    │
├──────────────────────────┬─────────────────────────────────────────┤
│  Image Preview           │              Data Grid                   │
│  ┌────────────────────┐  │  ┌─────────────────────────────────────┐ │
│  │                    │  │  │           TDBGrid                   │ │
│  │     TDBImage       │  │  │  ID │ Title    │ File Name │ Size  │ │
│  │  (displays BLOB    │  │  │  1  │ Photo 1  │ cat.jpg   │ 125KB │ │
│  │   automatically)   │  │  │  2  │ Document │ report.pdf│ 2.1MB │ │
│  │                    │  │  │  ...                                │ │
│  └────────────────────┘  │  └─────────────────────────────────────┘ │
│                          │                                         │
│  File Info               │  ┌─────────────────────────────────────┐ │
│  ┌────────────────────┐  │  │         TDBNavigator                │ │
│  │ File: cat.jpg      │  │  │    [<] [>] [+] [-] [Post] [Cancel] │ │
│  │ Type: .jpg         │  │  └─────────────────────────────────────┘ │
│  │ Size: 125 KB       │  │                                         │
│  └────────────────────┘  │  ┌─────────────────────────────────────┐ │
│                          │  │         Activity Log                │ │
│  [Load Image] [Save]     │  │ Loaded: photo.png (256 KB)          │ │
│  [Load File] [Export]    │  │ Saved to: export.png                │ │
│  └────────────────────┘  │  └─────────────────────────────────────┘ │
└──────────────────────────┴─────────────────────────────────────────┘
```

## Database Schema

```sql
CREATE TABLE media_files (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    description TEXT,
    file_name TEXT,           -- Original file name
    file_type TEXT,           -- File extension (.jpg, .png, etc.)
    file_size INTEGER,        -- Size in bytes
    image_data BLOB,          -- Binary data storage
    created_at TEXT DEFAULT (datetime('now', 'localtime')),
    modified_at TEXT
);
```

## Key Implementation Details

### Loading a File into BLOB

```pascal
procedure TfrmMain.LoadFileToBlob(const AFilePath: string);
var
  FileStream: TFileStream;
  BlobStream: TStream;
begin
  FileStream := TFileStream.Create(AFilePath, fmOpenRead);
  try
    FDataSet.Edit;

    // Get a write stream to the BLOB field
    BlobStream := FDataSet.CreateBlobStream(
      FDataSet.FieldByName('image_data'),
      bmWrite  // Write mode
    );
    try
      BlobStream.CopyFrom(FileStream, 0);  // 0 = copy entire stream
    finally
      BlobStream.Free;
    end;

    // Update metadata
    FDataSet.FieldByName('file_name').AsString := ExtractFileName(AFilePath);
    FDataSet.FieldByName('file_size').AsInteger := FileStream.Size;

    FDataSet.Post;
  finally
    FileStream.Free;
  end;
end;
```

### Saving BLOB to File

```pascal
procedure TfrmMain.SaveBlobToFile(const AFilePath: string);
var
  FileStream: TFileStream;
  BlobStream: TStream;
begin
  FileStream := TFileStream.Create(AFilePath, fmCreate);
  try
    // Get a read stream from the BLOB field
    BlobStream := FDataSet.CreateBlobStream(
      FDataSet.FieldByName('image_data'),
      bmRead  // Read mode
    );
    try
      FileStream.CopyFrom(BlobStream, 0);  // 0 = copy entire stream
    finally
      BlobStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

### TDBImage Configuration

```pascal
// In FormCreate:
DBImage1.DataSource := DataSource1;
DBImage1.DataField := 'image_data';   // BLOB field name
DBImage1.Stretch := True;              // Scale to fit
DBImage1.Proportional := True;         // Maintain aspect ratio
```

### File Type Detection

```pascal
function TfrmMain.DetectFileType(AStream: TStream): string;
var
  Header: array[0..7] of Byte;
begin
  AStream.Position := 0;
  AStream.Read(Header, 8);

  // Check magic bytes (file signatures)
  if (Header[0] = $89) and (Header[1] = $50) then Result := '.png'   // PNG
  else if (Header[0] = $FF) and (Header[1] = $D8) then Result := '.jpg'   // JPEG
  else if (Header[0] = $47) and (Header[1] = $49) then Result := '.gif'   // GIF
  else if (Header[0] = $42) and (Header[1] = $4D) then Result := '.bmp'   // BMP
  else if (Header[0] = $25) and (Header[1] = $50) then Result := '.pdf';  // PDF
end;
```

## Supported File Types

### Images (displayed in TDBImage)
- **PNG** - Portable Network Graphics
- **JPEG/JPG** - Joint Photographic Experts Group
- **BMP** - Windows Bitmap
- **GIF** - Graphics Interchange Format

### Other Files (stored but not previewed)
- **PDF** - Portable Document Format
- **ZIP** - Archive files
- Any binary file can be stored

## Performance Considerations

### BLOB Size Guidelines

| Size | Recommendation |
|------|----------------|
| < 100 KB | Excellent - store freely |
| 100 KB - 1 MB | Good - watch database size |
| 1 MB - 10 MB | Moderate - consider external storage |
| > 10 MB | Consider storing path only, files on disk |

### Best Practices

1. **Store metadata separately** - File name, size, type in regular columns
2. **Use incremental BLOBs** - For very large files, consider SQLite's incremental I/O
3. **Compress when appropriate** - PNG and JPEG already compressed; text can be gzip'd
4. **Consider external storage** - For very large files, store path instead of content
5. **Index metadata columns** - But never try to index BLOB columns

## Usage

### Loading an Image

1. Navigate to a record using DBNavigator
2. Click **Load Image**
3. Select an image file (PNG, JPEG, BMP, GIF)
4. Image appears in preview and is stored in database

### Saving an Image

1. Navigate to a record with an image
2. Click **Save Image**
3. Choose destination and filename
4. File is exported from database to disk

### Loading Any File

1. Click **Load Any File**
2. Select any file type
3. File is stored in BLOB field
4. If it's an image, preview updates automatically

## Building and Running

### Build
```bash
cd examples/gui/08_BlobImages
lazbuild BlobImagesDemo.lpi
```

### Run
- **Linux/macOS**: `./BlobImagesDemo`
- **Windows**: `BlobImagesDemo.exe`

## Project Files

| File | Description |
|------|-------------|
| `BlobImagesDemo.lpi` | Lazarus project configuration |
| `BlobImagesDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form with BLOB handling logic |
| `MainForm.lfm` | Visual form design |

## TDBImage Notes

`TDBImage` is a data-aware image control that automatically:
- Loads image data from BLOB fields
- Displays common image formats
- Updates when navigating records
- Handles NULL (empty) fields gracefully

Supported formats depend on the LCL and graphics libraries:
- **All platforms**: PNG, JPEG, BMP
- **With additional libraries**: GIF, TIFF, and more

## Related Examples

- **01_DBGrid**: Basic data display without BLOBs
- **02_MasterDetail**: Linking records (e.g., products with images)
- **06_SearchFilter**: Finding records by metadata

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
