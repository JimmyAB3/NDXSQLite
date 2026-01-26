# NDXSQLite GUI Example 00 - DateTime Utilities

Demonstrates the TNDXDateTimeHelper class for date/time conversions in SQLite applications with an interactive graphical interface.

## Overview

This example provides an interactive interface for exploring the `TNDXDateTimeHelper` static class, which handles date/time conversions essential for SQLite applications. It includes ISO 8601 UTC formatting, Unix timestamp conversions, local/UTC transformations, and format validation.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| Real-time Clock | Live display of local, UTC, ISO 8601, and Unix timestamp |
| Local to ISO UTC | Convert local datetime to ISO 8601 UTC string |
| ISO UTC to Local | Convert ISO 8601 UTC string to local datetime |
| Unix Timestamp | Bidirectional conversion between ISO 8601 and Unix timestamps |
| Format Validation | Verify if a string matches ISO 8601 format |
| ISO 8601 Parsing | Parse ISO 8601 strings with UTC detection |
| Timezone Display | Show current timezone offset from UTC |

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                           TfrmMain                                  │
├─────────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ Current Time (auto-refresh every second)                    │    │
│  │   Local: 2026-01-25 14:30:45.123                            │    │
│  │   UTC:   2026-01-25 13:30:45.123                            │    │
│  │   ISO:   2026-01-25T13:30:45.123Z                           │    │
│  │   Unix:  1769425845                                         │    │
│  │   Timezone: UTC+1.0                                         │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ Date/Time Conversions                                       │    │
│  │   Local DateTime: [2026-01-25 14:30:45] [To ISO UTC]        │    │
│  │   ISO 8601 UTC:   [2026-01-25T13:30:45Z] [To Local]         │    │
│  │   Unix Timestamp: [1769425845] [ISO→Unix] [Unix→ISO]        │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ Format Validation                                           │    │
│  │   [2024-06-15T14:30:45.123Z] [Validate] → Valid ISO 8601    │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ Parse ISO 8601                                              │    │
│  │   [2024-12-25T10:30:00Z] [Parse] → Parsed: ... (UTC: Yes)   │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
         │                                    │
         ▼                                    ▼
┌─────────────────────┐              ┌─────────────────────┐
│ TNDXDateTimeHelper  │              │ Log Memo (right)    │
│ (Static Class)      │              │ All operations are  │
│ - NowUTC            │              │ logged with         │
│ - NowToIsoUTC       │              │ timestamps          │
│ - LocalToIsoUTC     │              │                     │
│ - IsoUTCToLocal     │              │                     │
│ - Unix conversions  │              │                     │
│ - IsIso8601Format   │              │                     │
│ - ParseIso8601      │              │                     │
└─────────────────────┘              └─────────────────────┘
```

## Key Implementation Details

### Getting Current Times

```pascal
// Get current UTC time
UtcNow := TNDXDateTimeHelper.NowUTC;

// Get current time as ISO 8601 UTC string
IsoStr := TNDXDateTimeHelper.NowToIsoUTC;

// Get current Unix timestamp
UnixTS := TNDXDateTimeHelper.NowToUnixTimestamp;

// Format any TDateTime to string
FormattedStr := TNDXDateTimeHelper.DateTimeToStr(ADateTime);
```

### Converting Local to ISO UTC

```pascal
procedure TfrmMain.btnConvertLocalToIsoClick(Sender: TObject);
var
  LocalDT: TDateTime;
  IsoStr: string;
begin
  // Parse the local datetime from user input
  LocalDT := StrToDateTime(edtLocalDateTime.Text);

  // Convert to ISO 8601 UTC string
  IsoStr := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(LocalDT);

  // Display result
  edtIsoUtc.Text := IsoStr;
end;
```

### Converting ISO UTC to Local

```pascal
procedure TfrmMain.btnConvertIsoToLocalClick(Sender: TObject);
var
  LocalDT: TDateTime;
  Success: Boolean;
begin
  // Safe parsing with TryXxx pattern
  Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime(edtIsoUtc.Text, LocalDT);

  if Success then
    edtLocalDateTime.Text := TNDXDateTimeHelper.DateTimeToStr(LocalDT)
  else
    ShowMessage('Invalid ISO 8601 format');
end;
```

### Unix Timestamp Conversions

```pascal
// ISO 8601 to Unix timestamp
UnixTS := TNDXDateTimeHelper.IsoUTCToUnixTimestamp(IsoString);

// Unix timestamp to ISO 8601 UTC
IsoStr := TNDXDateTimeHelper.UnixTimestampToIsoUTC(UnixTS);
```

### Format Validation

```pascal
// Check if string is valid ISO 8601 format
if TNDXDateTimeHelper.IsIso8601Format(InputStr) then
  ShowMessage('Valid ISO 8601')
else
  ShowMessage('Not valid ISO 8601');
```

### Parsing with UTC Detection

```pascal
procedure TfrmMain.btnParseIsoClick(Sender: TObject);
var
  DT: TDateTime;
  IsUTC: Boolean;
begin
  // Parse and detect if input is UTC (ends with 'Z')
  DT := TNDXDateTimeHelper.ParseIso8601(edtParseInput.Text, IsUTC);

  lblParseResult.Caption := Format('Parsed: %s (UTC: %s)',
    [TNDXDateTimeHelper.DateTimeToStr(DT), BoolToStr(IsUTC, 'Yes', 'No')]);
end;
```

## Available Format Constants

| Constant | Value | Example |
|----------|-------|---------|
| `NDX_ISO8601_FORMAT` | `yyyy-mm-dd"T"hh:nn:ss.zzz"Z"` | 2024-06-15T14:30:45.123Z |
| `NDX_ISO8601_FORMAT_NO_MS` | `yyyy-mm-dd"T"hh:nn:ss"Z"` | 2024-06-15T14:30:45Z |
| `NDX_ISO8601_DATE_ONLY` | `yyyy-mm-dd` | 2024-06-15 |
| `NDX_ISO8601_TIME_ONLY` | `hh:nn:ss.zzz` | 14:30:45.123 |

## TNDXDateTimeHelper Methods Summary

| Method | Return Type | Description |
|--------|-------------|-------------|
| `NowUTC` | TDateTime | Current UTC time |
| `NowToIsoUTC` | string | Current time as ISO 8601 UTC |
| `NowLocalToStr` | string | Current local time as string |
| `NowToUnixTimestamp` | Int64 | Current Unix timestamp |
| `LocalDateTimeToIsoUTC` | string | Convert local to ISO UTC |
| `IsoUTCToLocalDateTime` | TDateTime | Convert ISO UTC to local |
| `TryIsoUTCToLocalDateTime` | Boolean | Safe ISO UTC to local conversion |
| `IsoUTCToUnixTimestamp` | Int64 | Convert ISO UTC to Unix |
| `UnixTimestampToIsoUTC` | string | Convert Unix to ISO UTC |
| `IsIso8601Format` | Boolean | Validate ISO 8601 format |
| `ParseIso8601` | TDateTime | Parse with UTC detection |
| `DateTimeToStr` | string | Format TDateTime as string |

## Building and Running

### Build
```bash
cd examples/gui/00_DateTimeUtils
lazbuild DateTimeUtilsDemo.lpi
```

### Run
- **Linux/macOS**: `./DateTimeUtilsDemo`
- **Windows**: `DateTimeUtilsDemo.exe`

## Project Files

| File | Description |
|------|-------------|
| `DateTimeUtilsDemo.lpi` | Lazarus project configuration |
| `DateTimeUtilsDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form with date/time conversion logic |
| `MainForm.lfm` | Visual form design |

## Testing the Features

1. **Real-time Clock**: Observe the live updating clock showing all time formats
   - Local time in your timezone
   - UTC time
   - ISO 8601 UTC format
   - Unix timestamp
   - Your timezone offset

2. **Local to ISO UTC**: Enter a local datetime and convert to ISO 8601 UTC
   - Input: `2024-06-15 14:30:45`
   - Output: `2024-06-15T12:30:45.000Z` (assuming UTC+2 timezone)

3. **ISO UTC to Local**: Enter an ISO 8601 string and convert to local
   - Input: `2024-06-15T14:30:45.123Z`
   - Output: `2024-06-15 16:30:45` (assuming UTC+2 timezone)

4. **Unix Timestamp**: Convert between ISO 8601 and Unix timestamps
   - Try: `1718454645` converts to `2024-06-15T14:30:45.000Z`

5. **Validation**: Test various strings for ISO 8601 compliance
   - Valid: `2024-06-15T14:30:45.123Z`, `2024-06-15T14:30:45Z`
   - Invalid: `15/06/2024`, `June 15, 2024`

6. **Parsing**: Parse ISO 8601 strings and detect UTC marker
   - `2024-06-15T14:30:45Z` → UTC: Yes
   - `2024-06-15 14:30:45` → UTC: No

## Common Use Cases

| Scenario | Recommended Method |
|----------|-------------------|
| Store timestamp in SQLite | `NowToIsoUTC` |
| Display database timestamp | `IsoUTCToLocalDateTime` |
| Interop with REST APIs | `LocalDateTimeToIsoUTC` |
| Unix timestamp from JS | `UnixTimestampToIsoUTC` |
| Validate user input | `IsIso8601Format` |
| Import from CSV | `TryIsoUTCToLocalDateTime` |

## Requirements

- **IDE**: Lazarus 2.2+ with Free Pascal 3.2+
- **Library**: NDXSQLite (parent project)
- **Platform**: Windows, Linux, or macOS

## Cross-Platform Notes

This example works identically on all platforms. The `TNDXDateTimeHelper` class uses platform-independent date/time handling. Timezone calculations are performed using the local system's timezone settings.

## Related Examples

- **Console 00_DateTimeUtils**: Console version with more detailed demonstrations
- **01_DBGrid**: Basic single-table CRUD operations
- **02_MasterDetail**: Two linked datasets with automatic filtering
- **03_CachedUpdates**: Batch editing with Apply/Cancel
- **04_AsyncProgress**: Non-blocking database operations

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
