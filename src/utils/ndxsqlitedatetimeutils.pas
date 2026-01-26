{===============================================================================
  NDXSQLite - DateTime Utilities
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Thread-safe helper class for ISO 8601 UTC date/time conversions.
  Designed for SQLite applications where dates are stored as TEXT in
  ISO 8601 format for portability and sorting.

  Key features:
  - Local <-> UTC conversions
  - ISO 8601 parsing (full and partial formats)
  - Unix timestamp conversions
  - Thread-safe static methods

  Cross-platform: Windows, Linux, macOS
===============================================================================}
unit ndxsqlitedatetimeutils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils;

const
  { Default ISO 8601 format with milliseconds for international use. }
  NDX_ISO8601_FORMAT = 'yyyy-mm-dd hh:nn:ss.zzz';

  { ISO 8601 format without milliseconds. }
  NDX_ISO8601_FORMAT_NO_MS = 'yyyy-mm-dd hh:nn:ss';

  { ISO 8601 date-only format. }
  NDX_ISO8601_DATE_ONLY = 'yyyy-mm-dd';

  { ISO 8601 time-only format with milliseconds. }
  NDX_ISO8601_TIME_ONLY = 'hh:nn:ss.zzz';

  { Unix epoch: January 1, 1970 00:00:00 UTC. }
  UNIX_EPOCH: TDateTime = 25569.0;

type
  { Thread-safe datetime helper providing ISO 8601 UTC conversions.

    All methods are static and thread-safe. This class is designed to
    simplify date/time handling in SQLite applications where dates are
    stored as TEXT in ISO 8601 format.

    Usage example:
      IsoDate := TNDXDateTimeHelper.NowToIsoUTC;
      SQLiteQuery.ParamByName('created_at').AsString := IsoDate;
  }
  TNDXDateTimeHelper = class
  public
    { -------------------------------------------------------------------------
      Local DateTime <-> ISO 8601 UTC Conversions
      ------------------------------------------------------------------------- }

    { Returns the current local date/time converted to ISO 8601 UTC format.
      Output format: YYYY-MM-DDTHH:mm:ss.sssZ
      Thread-safe. }
    class function NowToIsoUTC: string; static;

    { Converts a local TDateTime value to ISO 8601 UTC format.
      @param ALocalDateTime The local date/time value to convert.
      @returns ISO 8601 UTC string in format YYYY-MM-DDTHH:mm:ss.sssZ
      Thread-safe. }
    class function LocalDateTimeToIsoUTC(const ALocalDateTime: TDateTime): string; static;

    { Converts an ISO 8601 UTC date/time string to a local TDateTime value.
      Supports multiple ISO 8601 formats:
      - Full with milliseconds: YYYY-MM-DDTHH:mm:ss.sssZ
      - Without milliseconds: YYYY-MM-DDTHH:mm:ssZ
      - With space separator: YYYY-MM-DD HH:mm:ss.sssZ
      - Date only: YYYY-MM-DD (assumes 00:00:00 UTC)
      @param AIsoString The ISO 8601 UTC string to parse.
      @returns Local TDateTime value.
      @raises EConvertError if the string cannot be parsed.
      Thread-safe. }
    class function IsoUTCToLocalDateTime(const AIsoString: string): TDateTime; static;

    { Attempts to convert an ISO 8601 UTC string to a local TDateTime value.
      @param AIsoString The ISO 8601 UTC string to parse.
      @param ALocalDateTime Output parameter for the converted local date/time.
      @returns True if conversion succeeded, False otherwise.
      Thread-safe. }
    class function TryIsoUTCToLocalDateTime(const AIsoString: string;
      out ALocalDateTime: TDateTime): Boolean; static;

    { -------------------------------------------------------------------------
      Formatting Helpers
      ------------------------------------------------------------------------- }

    { Returns the current local date/time as a formatted string.
      @param AFormat Optional format string (default: ISO 8601 with milliseconds).
      @returns Formatted date/time string.
      Thread-safe. }
    class function NowLocalToStr(const AFormat: string = NDX_ISO8601_FORMAT): string; static;

    { Formats a TDateTime value as a string.
      @param ADateTime The date/time value to format.
      @param AFormat Optional format string (default: ISO 8601 with milliseconds).
      @returns Formatted date/time string.
      Thread-safe. }
    class function DateTimeToStr(const ADateTime: TDateTime;
      const AFormat: string = NDX_ISO8601_FORMAT): string; static;

    { -------------------------------------------------------------------------
      Unix Timestamp Conversions
      ------------------------------------------------------------------------- }

    { Converts a TDateTime value to Unix timestamp (seconds since epoch).
      @param ADateTime The date/time value (assumed UTC).
      @returns Unix timestamp as Int64.
      Thread-safe. }
    class function DateTimeToUnixTimestamp(const ADateTime: TDateTime): Int64; static;

    { Converts a Unix timestamp to TDateTime value.
      @param AUnixTimestamp Seconds since Unix epoch (1970-01-01 00:00:00 UTC).
      @returns TDateTime value (UTC).
      Thread-safe. }
    class function UnixTimestampToDateTime(const AUnixTimestamp: Int64): TDateTime; static;

    { Returns the current UTC time as Unix timestamp.
      @returns Current Unix timestamp as Int64.
      Thread-safe. }
    class function NowToUnixTimestamp: Int64; static;

    { Converts a Unix timestamp to ISO 8601 UTC string.
      @param AUnixTimestamp Seconds since Unix epoch.
      @returns ISO 8601 UTC string in format YYYY-MM-DDTHH:mm:ss.sssZ
      Thread-safe. }
    class function UnixTimestampToIsoUTC(const AUnixTimestamp: Int64): string; static;

    { Converts an ISO 8601 UTC string to Unix timestamp.
      @param AIsoString The ISO 8601 UTC string to parse.
      @returns Unix timestamp as Int64.
      @raises EConvertError if the string cannot be parsed.
      Thread-safe. }
    class function IsoUTCToUnixTimestamp(const AIsoString: string): Int64; static;

    { -------------------------------------------------------------------------
      ISO 8601 Parsing Helpers
      ------------------------------------------------------------------------- }

    { Parses an ISO 8601 string (with or without timezone indicator).
      Handles multiple formats:
      - YYYY-MM-DDTHH:mm:ss.sssZ (full UTC)
      - YYYY-MM-DDTHH:mm:ssZ (no milliseconds)
      - YYYY-MM-DD HH:mm:ss.sss (space separator)
      - YYYY-MM-DD HH:mm:ss (space separator, no ms)
      - YYYY-MM-DD (date only)
      @param AIsoString The ISO 8601 string to parse.
      @param AIsUTC Output parameter: True if string had Z suffix.
      @returns Parsed TDateTime value.
      @raises EConvertError if the string cannot be parsed.
      Thread-safe. }
    class function ParseIso8601(const AIsoString: string;
      out AIsUTC: Boolean): TDateTime; static;

    { Attempts to parse an ISO 8601 string.
      @param AIsoString The ISO 8601 string to parse.
      @param ADateTime Output parameter for the parsed date/time.
      @param AIsUTC Output parameter: True if string had Z suffix.
      @returns True if parsing succeeded, False otherwise.
      Thread-safe. }
    class function TryParseIso8601(const AIsoString: string;
      out ADateTime: TDateTime; out AIsUTC: Boolean): Boolean; static;

    { Checks if a string appears to be a valid ISO 8601 date/time format.
      Performs basic format validation without full parsing.
      @param AValue The string to check.
      @returns True if the string looks like ISO 8601 format.
      Thread-safe. }
    class function IsIso8601Format(const AValue: string): Boolean; static;

    { -------------------------------------------------------------------------
      UTC Helpers
      ------------------------------------------------------------------------- }

    { Returns the current UTC date/time.
      @returns Current UTC TDateTime value.
      Thread-safe. }
    class function NowUTC: TDateTime; static;

    { Converts local TDateTime to UTC TDateTime.
      @param ALocalDateTime The local date/time value.
      @returns UTC TDateTime value.
      Thread-safe. }
    class function LocalToUTC(const ALocalDateTime: TDateTime): TDateTime; static;

    { Converts UTC TDateTime to local TDateTime.
      @param AUTCDateTime The UTC date/time value.
      @returns Local TDateTime value.
      Thread-safe. }
    class function UTCToLocal(const AUTCDateTime: TDateTime): TDateTime; static;
  end;

implementation

{ ----------------------------------------------------------------------------- }
{ Local DateTime <-> ISO 8601 UTC Conversions                                   }
{ ----------------------------------------------------------------------------- }

class function TNDXDateTimeHelper.NowToIsoUTC: string;
begin
  Result := LocalDateTimeToIsoUTC(Now);
end;

class function TNDXDateTimeHelper.LocalDateTimeToIsoUTC(
  const ALocalDateTime: TDateTime): string;
var
  UTCDateTime: TDateTime;
begin
  // Convert local time to UTC
  UTCDateTime := LocalTimeToUniversal(ALocalDateTime);

  // Format as ISO 8601 with milliseconds and Z suffix
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', UTCDateTime);
end;

class function TNDXDateTimeHelper.IsoUTCToLocalDateTime(
  const AIsoString: string): TDateTime;
var
  IsUTC: Boolean;
  ParsedDateTime: TDateTime;
begin
  ParsedDateTime := ParseIso8601(AIsoString, IsUTC);

  // If the string was marked as UTC (had Z suffix), convert to local
  if IsUTC then
    Result := UniversalTimeToLocal(ParsedDateTime)
  else
    Result := ParsedDateTime;
end;

class function TNDXDateTimeHelper.TryIsoUTCToLocalDateTime(
  const AIsoString: string; out ALocalDateTime: TDateTime): Boolean;
var
  IsUTC: Boolean;
  ParsedDateTime: TDateTime;
begin
  Result := TryParseIso8601(AIsoString, ParsedDateTime, IsUTC);
  if Result then
  begin
    if IsUTC then
      ALocalDateTime := UniversalTimeToLocal(ParsedDateTime)
    else
      ALocalDateTime := ParsedDateTime;
  end
  else
    ALocalDateTime := 0;
end;

{ ----------------------------------------------------------------------------- }
{ Formatting Helpers                                                            }
{ ----------------------------------------------------------------------------- }

class function TNDXDateTimeHelper.NowLocalToStr(const AFormat: string): string;
begin
  Result := FormatDateTime(AFormat, Now);
end;

class function TNDXDateTimeHelper.DateTimeToStr(const ADateTime: TDateTime;
  const AFormat: string): string;
begin
  Result := FormatDateTime(AFormat, ADateTime);
end;

{ ----------------------------------------------------------------------------- }
{ Unix Timestamp Conversions                                                    }
{ ----------------------------------------------------------------------------- }

class function TNDXDateTimeHelper.DateTimeToUnixTimestamp(
  const ADateTime: TDateTime): Int64;
begin
  // Calculate seconds since Unix epoch
  Result := Round((ADateTime - UNIX_EPOCH) * SecsPerDay);
end;

class function TNDXDateTimeHelper.UnixTimestampToDateTime(
  const AUnixTimestamp: Int64): TDateTime;
begin
  // Convert seconds since epoch to TDateTime
  Result := UNIX_EPOCH + (AUnixTimestamp / SecsPerDay);
end;

class function TNDXDateTimeHelper.NowToUnixTimestamp: Int64;
begin
  Result := DateTimeToUnixTimestamp(NowUTC);
end;

class function TNDXDateTimeHelper.UnixTimestampToIsoUTC(
  const AUnixTimestamp: Int64): string;
var
  UTCDateTime: TDateTime;
begin
  UTCDateTime := UnixTimestampToDateTime(AUnixTimestamp);
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', UTCDateTime);
end;

class function TNDXDateTimeHelper.IsoUTCToUnixTimestamp(
  const AIsoString: string): Int64;
var
  IsUTC: Boolean;
  ParsedDateTime: TDateTime;
begin
  ParsedDateTime := ParseIso8601(AIsoString, IsUTC);

  // If not marked as UTC, assume it is UTC anyway for this function
  Result := DateTimeToUnixTimestamp(ParsedDateTime);
end;

{ ----------------------------------------------------------------------------- }
{ ISO 8601 Parsing Helpers                                                      }
{ ----------------------------------------------------------------------------- }

class function TNDXDateTimeHelper.ParseIso8601(const AIsoString: string;
  out AIsUTC: Boolean): TDateTime;
var
  S: string;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  DatePart, TimePart: string;
  SepPos: Integer;
begin
  S := Trim(AIsoString);
  if S = '' then
    raise EConvertError.Create('Cannot parse empty ISO 8601 string');

  // Check for UTC indicator
  AIsUTC := (Length(S) > 0) and (S[Length(S)] = 'Z');
  if AIsUTC then
    S := Copy(S, 1, Length(S) - 1);

  // Replace 'T' separator with space for uniform parsing
  S := StringReplace(S, 'T', ' ', []);

  // Find the separator between date and time
  SepPos := Pos(' ', S);

  if SepPos > 0 then
  begin
    DatePart := Copy(S, 1, SepPos - 1);
    TimePart := Copy(S, SepPos + 1, Length(S));
  end
  else
  begin
    // Date only format
    DatePart := S;
    TimePart := '';
  end;

  // Parse date part (YYYY-MM-DD)
  if Length(DatePart) >= 10 then
  begin
    Year := StrToInt(Copy(DatePart, 1, 4));
    Month := StrToInt(Copy(DatePart, 6, 2));
    Day := StrToInt(Copy(DatePart, 9, 2));
  end
  else
    raise EConvertError.CreateFmt('Invalid ISO 8601 date format: %s', [AIsoString]);

  // Parse time part if present
  Hour := 0;
  Min := 0;
  Sec := 0;
  MSec := 0;

  if TimePart <> '' then
  begin
    // At minimum we need HH:MM:SS (8 characters)
    if Length(TimePart) >= 8 then
    begin
      Hour := StrToInt(Copy(TimePart, 1, 2));
      Min := StrToInt(Copy(TimePart, 4, 2));
      Sec := StrToInt(Copy(TimePart, 7, 2));

      // Check for milliseconds (.sss)
      if (Length(TimePart) >= 12) and (TimePart[9] = '.') then
        MSec := StrToInt(Copy(TimePart, 10, 3));
    end
    else
      raise EConvertError.CreateFmt('Invalid ISO 8601 time format: %s', [AIsoString]);
  end;

  Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec);
end;

class function TNDXDateTimeHelper.TryParseIso8601(const AIsoString: string;
  out ADateTime: TDateTime; out AIsUTC: Boolean): Boolean;
begin
  Result := True;
  try
    ADateTime := ParseIso8601(AIsoString, AIsUTC);
  except
    on E: Exception do
    begin
      Result := False;
      ADateTime := 0;
      AIsUTC := False;
    end;
  end;
end;

class function TNDXDateTimeHelper.IsIso8601Format(const AValue: string): Boolean;
var
  S: string;
begin
  Result := False;
  S := Trim(AValue);

  if Length(S) < 10 then
    Exit;

  // Check basic date pattern: YYYY-MM-DD
  if (S[5] <> '-') or (S[8] <> '-') then
    Exit;

  // Check if first 4 characters are digits (year)
  if not (S[1] in ['0'..'9']) or not (S[2] in ['0'..'9']) or
     not (S[3] in ['0'..'9']) or not (S[4] in ['0'..'9']) then
    Exit;

  // Check month digits
  if not (S[6] in ['0'..'9']) or not (S[7] in ['0'..'9']) then
    Exit;

  // Check day digits
  if not (S[9] in ['0'..'9']) or not (S[10] in ['0'..'9']) then
    Exit;

  // Basic date format is valid
  Result := True;

  // If there's more, check for time part
  if Length(S) > 10 then
  begin
    // Must have T or space separator
    if not (S[11] in ['T', ' ']) then
      Result := False;
  end;
end;

{ ----------------------------------------------------------------------------- }
{ UTC Helpers                                                                   }
{ ----------------------------------------------------------------------------- }

class function TNDXDateTimeHelper.NowUTC: TDateTime;
begin
  Result := LocalTimeToUniversal(Now);
end;

class function TNDXDateTimeHelper.LocalToUTC(
  const ALocalDateTime: TDateTime): TDateTime;
begin
  Result := LocalTimeToUniversal(ALocalDateTime);
end;

class function TNDXDateTimeHelper.UTCToLocal(
  const AUTCDateTime: TDateTime): TDateTime;
begin
  Result := UniversalTimeToLocal(AUTCDateTime);
end;

end.
