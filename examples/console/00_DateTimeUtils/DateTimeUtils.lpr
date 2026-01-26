{===============================================================================
  NDXSQLite Example 00 - DateTime Utilities
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates the TNDXDateTimeHelper class for handling
  date/time conversions in SQLite applications:
  - ISO 8601 UTC string conversions
  - Unix timestamp conversions
  - Local/UTC time conversions
  - Parsing various ISO 8601 formats
  - Integration with SQLite TEXT date storage

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DateTimeUtils;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DateUtils, DB,
  ndxsqliteconnection, ndxsqlitedatetimeutils;

var
  Connection: TNDXSQLiteConnection;
  DBPath: string;

{ Creates the events table for storing date/time examples. }
procedure SetupDatabase;
begin
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS events (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  event_date TEXT NOT NULL,' +    // ISO 8601 UTC format
    '  created_at TEXT NOT NULL,' +    // ISO 8601 UTC format
    '  unix_timestamp INTEGER' +        // Unix timestamp
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS logs (' +
    '  id INTEGER PRIMARY KEY,' +
    '  message TEXT NOT NULL,' +
    '  logged_at TEXT NOT NULL' +      // ISO 8601 UTC format
    ')');
end;

{ Prints a section header with consistent formatting. }
procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn(StringOfChar('=', 60));
  WriteLn(ATitle);
  WriteLn(StringOfChar('=', 60));
end;

{ Demonstrates basic ISO 8601 UTC conversions for current time. }
procedure DemoBasicIsoConversions;
var
  IsoNow: string;
  LocalNow: TDateTime;
begin
  PrintHeader('1. Basic ISO 8601 UTC Conversions');

  // Get current time as ISO 8601 UTC string
  IsoNow := TNDXDateTimeHelper.NowToIsoUTC;
  WriteLn('Current time as ISO 8601 UTC: ', IsoNow);

  // Convert back to local TDateTime
  LocalNow := TNDXDateTimeHelper.IsoUTCToLocalDateTime(IsoNow);
  WriteLn('Converted back to local:      ', TNDXDateTimeHelper.DateTimeToStr(LocalNow));

  // Show the difference (should be minimal, just formatting)
  WriteLn;
  WriteLn('Local Now formatted:          ', TNDXDateTimeHelper.NowLocalToStr);
  WriteLn('UTC Now:                      ', TNDXDateTimeHelper.DateTimeToStr(TNDXDateTimeHelper.NowUTC));
end;

{ Demonstrates converting specific local dates to ISO 8601 UTC. }
procedure DemoLocalToIsoConversion;
var
  LocalDate: TDateTime;
  IsoString: string;
begin
  PrintHeader('2. Local DateTime to ISO 8601 UTC');

  // Create a specific local date/time
  LocalDate := EncodeDateTime(2024, 12, 25, 10, 30, 0, 0);
  WriteLn('Original local date/time: ', TNDXDateTimeHelper.DateTimeToStr(LocalDate));

  // Convert to ISO 8601 UTC
  IsoString := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(LocalDate);
  WriteLn('As ISO 8601 UTC string:   ', IsoString);

  // Show it can be stored directly in SQLite
  WriteLn;
  WriteLn('This format sorts correctly in SQLite TEXT columns');
  WriteLn('and is timezone-independent for data exchange.');
end;

{ Demonstrates parsing various ISO 8601 format variations. }
procedure DemoParsingFormats;
var
  DT: TDateTime;
  IsUTC: Boolean;
  Success: Boolean;
begin
  PrintHeader('3. Parsing Various ISO 8601 Formats');

  WriteLn('The parser handles multiple ISO 8601 variations:');
  WriteLn;

  // Full format with milliseconds and Z
  WriteLn('Format: YYYY-MM-DDTHH:mm:ss.sssZ');
  DT := TNDXDateTimeHelper.ParseIso8601('2024-06-15T14:30:45.123Z', IsUTC);
  WriteLn('  Input:  2024-06-15T14:30:45.123Z');
  WriteLn('  Parsed: ', TNDXDateTimeHelper.DateTimeToStr(DT), ' (UTC: ', IsUTC, ')');
  WriteLn;

  // Without milliseconds
  WriteLn('Format: YYYY-MM-DDTHH:mm:ssZ');
  DT := TNDXDateTimeHelper.ParseIso8601('2024-06-15T14:30:45Z', IsUTC);
  WriteLn('  Input:  2024-06-15T14:30:45Z');
  WriteLn('  Parsed: ', TNDXDateTimeHelper.DateTimeToStr(DT), ' (UTC: ', IsUTC, ')');
  WriteLn;

  // Space separator instead of T
  WriteLn('Format: YYYY-MM-DD HH:mm:ss.sss');
  DT := TNDXDateTimeHelper.ParseIso8601('2024-06-15 14:30:45.500', IsUTC);
  WriteLn('  Input:  2024-06-15 14:30:45.500');
  WriteLn('  Parsed: ', TNDXDateTimeHelper.DateTimeToStr(DT), ' (UTC: ', IsUTC, ')');
  WriteLn;

  // Date only
  WriteLn('Format: YYYY-MM-DD (date only)');
  DT := TNDXDateTimeHelper.ParseIso8601('2024-06-15', IsUTC);
  WriteLn('  Input:  2024-06-15');
  WriteLn('  Parsed: ', TNDXDateTimeHelper.DateTimeToStr(DT), ' (UTC: ', IsUTC, ')');
  WriteLn;

  // TryParse for safe parsing
  WriteLn('Safe parsing with TryIsoUTCToLocalDateTime:');
  Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime('invalid-date', DT);
  WriteLn('  "invalid-date" -> Success: ', Success);
  Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime('2024-06-15T12:00:00Z', DT);
  WriteLn('  "2024-06-15T12:00:00Z" -> Success: ', Success);
end;

{ Demonstrates Unix timestamp conversions and their use cases. }
procedure DemoUnixTimestamps;
var
  CurrentTimestamp: Int64;
  DT: TDateTime;
  IsoString: string;
begin
  PrintHeader('4. Unix Timestamp Conversions');

  // Get current Unix timestamp
  CurrentTimestamp := TNDXDateTimeHelper.NowToUnixTimestamp;
  WriteLn('Current Unix timestamp: ', CurrentTimestamp);

  // Convert Unix timestamp to DateTime
  DT := TNDXDateTimeHelper.UnixTimestampToDateTime(CurrentTimestamp);
  WriteLn('As TDateTime (UTC):     ', TNDXDateTimeHelper.DateTimeToStr(DT));

  // Convert Unix timestamp directly to ISO string
  IsoString := TNDXDateTimeHelper.UnixTimestampToIsoUTC(CurrentTimestamp);
  WriteLn('As ISO 8601 UTC:        ', IsoString);

  WriteLn;
  WriteLn('Common Unix timestamps:');

  // Unix epoch
  WriteLn('  Epoch (0):            ', TNDXDateTimeHelper.UnixTimestampToIsoUTC(0));

  // Y2K
  WriteLn('  Y2K (946684800):      ', TNDXDateTimeHelper.UnixTimestampToIsoUTC(946684800));

  // 2024-01-01
  WriteLn('  2024-01-01 (1704067200): ', TNDXDateTimeHelper.UnixTimestampToIsoUTC(1704067200));

  WriteLn;
  WriteLn('Convert ISO to Unix timestamp:');
  WriteLn('  "2024-06-15T12:00:00Z" -> ',
    TNDXDateTimeHelper.IsoUTCToUnixTimestamp('2024-06-15T12:00:00Z'));
end;

{ Demonstrates format validation using IsIso8601Format. }
procedure DemoFormatValidation;
begin
  PrintHeader('5. ISO 8601 Format Validation');

  WriteLn('IsIso8601Format checks if a string looks like ISO 8601:');
  WriteLn;

  WriteLn('  "2024-06-15T14:30:45.123Z" -> ',
    TNDXDateTimeHelper.IsIso8601Format('2024-06-15T14:30:45.123Z'));
  WriteLn('  "2024-06-15"               -> ',
    TNDXDateTimeHelper.IsIso8601Format('2024-06-15'));
  WriteLn('  "2024-06-15 14:30:45"      -> ',
    TNDXDateTimeHelper.IsIso8601Format('2024-06-15 14:30:45'));
  WriteLn('  "15/06/2024"               -> ',
    TNDXDateTimeHelper.IsIso8601Format('15/06/2024'));
  WriteLn('  "June 15, 2024"            -> ',
    TNDXDateTimeHelper.IsIso8601Format('June 15, 2024'));
  WriteLn('  ""                         -> ',
    TNDXDateTimeHelper.IsIso8601Format(''));

  WriteLn;
  WriteLn('Use this to validate user input before parsing.');
end;

{ Demonstrates integration with SQLite for storing events. }
procedure DemoSQLiteIntegration;
var
  DS: TDataSet;
  EventDate, CreatedAt: string;
  UnixTS: Int64;
  LocalDT: TDateTime;
begin
  PrintHeader('6. SQLite Integration');

  WriteLn('Storing events with ISO 8601 UTC dates in SQLite:');
  WriteLn;

  // Insert events with ISO 8601 UTC dates
  EventDate := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(
    EncodeDateTime(2024, 12, 25, 9, 0, 0, 0));
  CreatedAt := TNDXDateTimeHelper.NowToIsoUTC;
  UnixTS := TNDXDateTimeHelper.NowToUnixTimestamp;

  Connection.ExecuteNonQuery(Format(
    'INSERT INTO events (name, event_date, created_at, unix_timestamp) ' +
    'VALUES (''Christmas Morning'', ''%s'', ''%s'', %d)',
    [EventDate, CreatedAt, UnixTS]));

  EventDate := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(
    EncodeDateTime(2024, 12, 31, 23, 59, 0, 0));
  CreatedAt := TNDXDateTimeHelper.NowToIsoUTC;
  UnixTS := TNDXDateTimeHelper.NowToUnixTimestamp;

  Connection.ExecuteNonQuery(Format(
    'INSERT INTO events (name, event_date, created_at, unix_timestamp) ' +
    'VALUES (''New Year Eve'', ''%s'', ''%s'', %d)',
    [EventDate, CreatedAt, UnixTS]));

  EventDate := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(
    EncodeDateTime(2025, 1, 1, 0, 0, 0, 0));
  CreatedAt := TNDXDateTimeHelper.NowToIsoUTC;
  UnixTS := TNDXDateTimeHelper.NowToUnixTimestamp;

  Connection.ExecuteNonQuery(Format(
    'INSERT INTO events (name, event_date, created_at, unix_timestamp) ' +
    'VALUES (''New Year 2025'', ''%s'', ''%s'', %d)',
    [EventDate, CreatedAt, UnixTS]));

  // Query and display events (note: ORDER BY works correctly with ISO 8601)
  WriteLn('Events sorted by date (ISO 8601 sorts correctly as TEXT):');
  WriteLn;

  DS := Connection.ExecuteQuery(
    'SELECT name, event_date, unix_timestamp FROM events ORDER BY event_date');
  try
    while not DS.EOF do
    begin
      // Convert ISO UTC back to local for display
      LocalDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime(
        DS.FieldByName('event_date').AsString);

      WriteLn(Format('  %s', [DS.FieldByName('name').AsString]));
      WriteLn(Format('    Stored (UTC):  %s', [DS.FieldByName('event_date').AsString]));
      WriteLn(Format('    Local display: %s', [TNDXDateTimeHelper.DateTimeToStr(LocalDT)]));
      WriteLn(Format('    Unix timestamp: %d', [DS.FieldByName('unix_timestamp').AsInteger]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Demonstrates logging with automatic timestamps. }
procedure DemoLogging;
var
  I: Integer;
  DS: TDataSet;
  LoggedAt: string;
  LocalDT: TDateTime;
begin
  PrintHeader('7. Logging with Automatic Timestamps');

  WriteLn('Inserting log entries with automatic UTC timestamps:');
  WriteLn;

  // Insert several log entries
  for I := 1 to 5 do
  begin
    Connection.ExecuteNonQuery(Format(
      'INSERT INTO logs (message, logged_at) VALUES (''Log entry %d'', ''%s'')',
      [I, TNDXDateTimeHelper.NowToIsoUTC]));
    Sleep(100); // Small delay to show different timestamps
  end;

  // Display logs
  DS := Connection.ExecuteQuery('SELECT * FROM logs ORDER BY logged_at');
  try
    while not DS.EOF do
    begin
      LoggedAt := DS.FieldByName('logged_at').AsString;
      LocalDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime(LoggedAt);

      WriteLn(Format('  [%s] %s',
        [TNDXDateTimeHelper.DateTimeToStr(LocalDT, 'hh:nn:ss.zzz'),
         DS.FieldByName('message').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Demonstrates UTC conversion helpers for timezone handling. }
procedure DemoUTCHelpers;
var
  LocalNow, UTCNow: TDateTime;
  TimezoneOffset: Double;
begin
  PrintHeader('8. UTC Conversion Helpers');

  LocalNow := Now;
  UTCNow := TNDXDateTimeHelper.NowUTC;

  WriteLn('Local now:    ', TNDXDateTimeHelper.DateTimeToStr(LocalNow));
  WriteLn('UTC now:      ', TNDXDateTimeHelper.DateTimeToStr(UTCNow));

  // Calculate timezone offset
  TimezoneOffset := (LocalNow - UTCNow) * 24;
  WriteLn;
  WriteLn(Format('Timezone offset: %.1f hours', [TimezoneOffset]));

  WriteLn;
  WriteLn('Conversion functions:');
  WriteLn('  TNDXDateTimeHelper.LocalToUTC(LocalDateTime) -> UTC');
  WriteLn('  TNDXDateTimeHelper.UTCToLocal(UTCDateTime)   -> Local');
  WriteLn('  TNDXDateTimeHelper.NowUTC                    -> Current UTC');
end;

{ Demonstrates custom formatting options. }
procedure DemoCustomFormatting;
var
  DT: TDateTime;
begin
  PrintHeader('9. Custom Formatting Options');

  DT := Now;

  WriteLn('Same date/time with different formats:');
  WriteLn;

  WriteLn('  Default (ISO 8601):    ',
    TNDXDateTimeHelper.DateTimeToStr(DT));
  WriteLn('  Date only:             ',
    TNDXDateTimeHelper.DateTimeToStr(DT, NDX_ISO8601_DATE_ONLY));
  WriteLn('  Time only:             ',
    TNDXDateTimeHelper.DateTimeToStr(DT, NDX_ISO8601_TIME_ONLY));
  WriteLn('  No milliseconds:       ',
    TNDXDateTimeHelper.DateTimeToStr(DT, NDX_ISO8601_FORMAT_NO_MS));
  WriteLn('  European format:       ',
    TNDXDateTimeHelper.DateTimeToStr(DT, 'dd/mm/yyyy hh:nn:ss'));
  WriteLn('  US format:             ',
    TNDXDateTimeHelper.DateTimeToStr(DT, 'mm/dd/yyyy hh:nn:ss AM/PM'));
  WriteLn('  Compact:               ',
    TNDXDateTimeHelper.DateTimeToStr(DT, 'yyyymmdd_hhnnss'));

  WriteLn;
  WriteLn('Available format constants:');
  WriteLn('  NDX_ISO8601_FORMAT      = ''', NDX_ISO8601_FORMAT, '''');
  WriteLn('  NDX_ISO8601_FORMAT_NO_MS = ''', NDX_ISO8601_FORMAT_NO_MS, '''');
  WriteLn('  NDX_ISO8601_DATE_ONLY   = ''', NDX_ISO8601_DATE_ONLY, '''');
  WriteLn('  NDX_ISO8601_TIME_ONLY   = ''', NDX_ISO8601_TIME_ONLY, '''');
end;

{ Prints a summary of best practices for date/time handling. }
procedure PrintBestPractices;
begin
  PrintHeader('10. Best Practices Summary');

  WriteLn('1. Store dates as TEXT in ISO 8601 UTC format (YYYY-MM-DDTHH:mm:ss.sssZ)');
  WriteLn('   - Sorts correctly as text');
  WriteLn('   - Timezone independent');
  WriteLn('   - Human readable');
  WriteLn;
  WriteLn('2. Convert to local time only for display');
  WriteLn('   IsoDate := TNDXDateTimeHelper.NowToIsoUTC;');
  WriteLn('   LocalDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime(IsoDate);');
  WriteLn;
  WriteLn('3. Use TryIsoUTCToLocalDateTime for user input validation');
  WriteLn('   if TNDXDateTimeHelper.TryIsoUTCToLocalDateTime(Input, DT) then ...');
  WriteLn;
  WriteLn('4. Use Unix timestamps when interoperating with other systems');
  WriteLn('   UnixTS := TNDXDateTimeHelper.NowToUnixTimestamp;');
  WriteLn;
  WriteLn('5. All methods are thread-safe (static class functions)');
end;

{ Deletes the example database file. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 00: DateTime Utilities ===');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example00.db';
  Cleanup;

  Connection := TNDXSQLiteConnection.Create(DBPath);
  try
    Connection.Open;
    SetupDatabase;

    DemoBasicIsoConversions;
    DemoLocalToIsoConversion;
    DemoParsingFormats;
    DemoUnixTimestamps;
    DemoFormatValidation;
    DemoSQLiteIntegration;
    DemoLogging;
    DemoUTCHelpers;
    DemoCustomFormatting;
    PrintBestPractices;

    Connection.Close;
  finally
    Connection.Free;
  end;

  Cleanup;

  WriteLn;
  WriteLn('=== Example completed successfully! ===');
end.
