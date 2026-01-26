# Example 00: DateTime Utilities

This example demonstrates the `TNDXDateTimeHelper` class for handling date/time conversions in SQLite applications.

## What you'll learn

- How to convert local date/time to ISO 8601 UTC strings
- How to parse ISO 8601 strings back to local TDateTime
- How to work with Unix timestamps
- How to validate ISO 8601 format strings
- How to integrate date/time handling with SQLite TEXT columns
- Best practices for timezone-independent date storage

## Key concepts

### ISO 8601 UTC conversions

```pascal
// Get current time as ISO 8601 UTC string
IsoString := TNDXDateTimeHelper.NowToIsoUTC;
// Result: "2024-06-15T14:30:45.123Z"

// Convert local date/time to ISO 8601 UTC
IsoString := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(Now);

// Convert ISO 8601 UTC back to local time
LocalDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime('2024-06-15T14:30:45.123Z');
```

### Safe parsing with error handling

```pascal
var
  LocalDT: TDateTime;
begin
  if TNDXDateTimeHelper.TryIsoUTCToLocalDateTime(UserInput, LocalDT) then
    WriteLn('Valid date: ', DateTimeToStr(LocalDT))
  else
    WriteLn('Invalid date format');
end;
```

### Unix timestamp conversions

```pascal
// Get current Unix timestamp
UnixTS := TNDXDateTimeHelper.NowToUnixTimestamp;

// Convert Unix timestamp to TDateTime
DT := TNDXDateTimeHelper.UnixTimestampToDateTime(UnixTS);

// Convert Unix timestamp to ISO 8601
IsoString := TNDXDateTimeHelper.UnixTimestampToIsoUTC(UnixTS);

// Convert ISO 8601 to Unix timestamp
UnixTS := TNDXDateTimeHelper.IsoUTCToUnixTimestamp('2024-06-15T12:00:00Z');
```

### SQLite integration

```pascal
// Store dates as TEXT in ISO 8601 UTC format
Connection.ExecuteNonQuery(Format(
  'INSERT INTO events (name, event_date) VALUES (''Meeting'', ''%s'')',
  [TNDXDateTimeHelper.NowToIsoUTC]));

// Retrieve and convert to local time for display
DS := Connection.ExecuteQuery('SELECT event_date FROM events');
LocalDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime(
  DS.FieldByName('event_date').AsString);
```

### Format validation

```pascal
if TNDXDateTimeHelper.IsIso8601Format(UserInput) then
  // Safe to parse
else
  // Invalid format
```

## Supported ISO 8601 formats

The parser handles multiple variations:

- `YYYY-MM-DDTHH:mm:ss.sssZ` (full with milliseconds)
- `YYYY-MM-DDTHH:mm:ssZ` (without milliseconds)
- `YYYY-MM-DD HH:mm:ss.sss` (space separator)
- `YYYY-MM-DD HH:mm:ss` (space separator, no ms)
- `YYYY-MM-DD` (date only)

## Available format constants

```pascal
NDX_ISO8601_FORMAT       = 'yyyy-mm-dd hh:nn:ss.zzz';
NDX_ISO8601_FORMAT_NO_MS = 'yyyy-mm-dd hh:nn:ss';
NDX_ISO8601_DATE_ONLY    = 'yyyy-mm-dd';
NDX_ISO8601_TIME_ONLY    = 'hh:nn:ss.zzz';
```

## Building

```bash
lazbuild DateTimeUtils.lpi
```

## Running

```bash
./DateTimeUtils      # Linux/macOS
DateTimeUtils.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 00: DateTime Utilities ===

============================================================
1. Basic ISO 8601 UTC Conversions
============================================================
Current time as ISO 8601 UTC: 2024-06-15T14:30:45.123Z
Converted back to local:      2024-06-15 16:30:45.123
...

============================================================
6. SQLite Integration
============================================================
Storing events with ISO 8601 UTC dates in SQLite:

Events sorted by date (ISO 8601 sorts correctly as TEXT):

  Christmas Morning
    Stored (UTC):  2024-12-25T08:00:00.000Z
    Local display: 2024-12-25 09:00:00.000
    Unix timestamp: 1735113600
...

=== Example completed successfully! ===
```

## Best practices

1. **Store dates as TEXT in ISO 8601 UTC format** - Sorts correctly, timezone-independent, human-readable
2. **Convert to local time only for display** - Keep storage format consistent
3. **Use TryIsoUTCToLocalDateTime for user input** - Safe parsing with error handling
4. **Use Unix timestamps for system interoperability** - Common format for APIs
5. **All methods are thread-safe** - Static class functions with no shared state

## Cross-Platform

This example works on Windows, Linux, and macOS.
