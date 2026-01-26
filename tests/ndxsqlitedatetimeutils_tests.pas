{===============================================================================
  NDXSQLite - DateTime Utilities Unit Tests
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Comprehensive test suite for TNDXDateTimeHelper class.
  Tests cover:
  - ISO 8601 UTC conversions
  - Unix timestamp conversions
  - Parsing of various ISO 8601 formats
  - Edge cases and error handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}
unit ndxsqlitedatetimeutils_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils,
  ndxsqlitedatetimeutils;

type
  { Test suite for TNDXDateTimeHelper ISO 8601 UTC conversions. }
  TDateTimeUtilsIsoTests = class(TTestCase)
  published
    { Verifies NowToIsoUTC returns a properly formatted ISO 8601 string. }
    procedure TestNowToIsoUTC_Format;

    { Verifies NowToIsoUTC string contains T separator and Z suffix. }
    procedure TestNowToIsoUTC_ContainsTAndZ;

    { Verifies LocalDateTimeToIsoUTC produces correct output for a known date. }
    procedure TestLocalDateTimeToIsoUTC_KnownDate;

    { Verifies IsoUTCToLocalDateTime correctly parses a full ISO 8601 string. }
    procedure TestIsoUTCToLocalDateTime_FullFormat;

    { Verifies IsoUTCToLocalDateTime handles format without milliseconds. }
    procedure TestIsoUTCToLocalDateTime_NoMilliseconds;

    { Verifies IsoUTCToLocalDateTime handles date-only format. }
    procedure TestIsoUTCToLocalDateTime_DateOnly;

    { Verifies IsoUTCToLocalDateTime handles space separator instead of T. }
    procedure TestIsoUTCToLocalDateTime_SpaceSeparator;

    { Verifies round-trip conversion preserves the original value. }
    procedure TestRoundTrip_LocalToIsoAndBack;

    { Verifies TryIsoUTCToLocalDateTime returns True for valid input. }
    procedure TestTryIsoUTCToLocalDateTime_ValidInput;

    { Verifies TryIsoUTCToLocalDateTime returns False for invalid input. }
    procedure TestTryIsoUTCToLocalDateTime_InvalidInput;

    { Verifies IsoUTCToLocalDateTime raises exception for empty string. }
    procedure TestIsoUTCToLocalDateTime_EmptyString_RaisesException;

    { Verifies IsoUTCToLocalDateTime raises exception for malformed input. }
    procedure TestIsoUTCToLocalDateTime_MalformedInput_RaisesException;
  end;

  { Test suite for TNDXDateTimeHelper Unix timestamp conversions. }
  TDateTimeUtilsUnixTests = class(TTestCase)
  published
    { Verifies DateTimeToUnixTimestamp returns correct value for Unix epoch. }
    procedure TestDateTimeToUnixTimestamp_Epoch;

    { Verifies DateTimeToUnixTimestamp returns correct value for known date. }
    procedure TestDateTimeToUnixTimestamp_KnownDate;

    { Verifies UnixTimestampToDateTime correctly converts epoch. }
    procedure TestUnixTimestampToDateTime_Epoch;

    { Verifies UnixTimestampToDateTime correctly converts known timestamp. }
    procedure TestUnixTimestampToDateTime_KnownTimestamp;

    { Verifies round-trip Unix timestamp conversion preserves value. }
    procedure TestRoundTrip_UnixTimestamp;

    { Verifies NowToUnixTimestamp returns a reasonable current timestamp. }
    procedure TestNowToUnixTimestamp_ReasonableValue;

    { Verifies UnixTimestampToIsoUTC produces correct ISO string. }
    procedure TestUnixTimestampToIsoUTC;

    { Verifies IsoUTCToUnixTimestamp parses correctly. }
    procedure TestIsoUTCToUnixTimestamp;

    { Verifies round-trip between ISO and Unix timestamp. }
    procedure TestRoundTrip_IsoToUnixAndBack;
  end;

  { Test suite for TNDXDateTimeHelper ISO 8601 parsing. }
  TDateTimeUtilsParsingTests = class(TTestCase)
  published
    { Verifies ParseIso8601 correctly identifies UTC indicator. }
    procedure TestParseIso8601_DetectsUTC;

    { Verifies ParseIso8601 handles non-UTC strings. }
    procedure TestParseIso8601_NonUTC;

    { Verifies TryParseIso8601 returns True for valid formats. }
    procedure TestTryParseIso8601_ValidFormats;

    { Verifies TryParseIso8601 returns False for invalid formats. }
    procedure TestTryParseIso8601_InvalidFormats;

    { Verifies IsIso8601Format correctly identifies valid ISO strings. }
    procedure TestIsIso8601Format_ValidFormats;

    { Verifies IsIso8601Format correctly rejects invalid formats. }
    procedure TestIsIso8601Format_InvalidFormats;

    { Verifies parsing of ISO 8601 with various millisecond values. }
    procedure TestParseIso8601_Milliseconds;
  end;

  { Test suite for TNDXDateTimeHelper UTC conversion helpers. }
  TDateTimeUtilsUTCTests = class(TTestCase)
  published
    { Verifies NowUTC returns a value close to converted Now. }
    procedure TestNowUTC_ConsistentWithNow;

    { Verifies LocalToUTC and UTCToLocal are inverse operations. }
    procedure TestLocalToUTC_UTCToLocal_Inverse;

    { Verifies UTC conversion handles midnight correctly. }
    procedure TestUTCConversion_Midnight;

    { Verifies UTC conversion handles noon correctly. }
    procedure TestUTCConversion_Noon;
  end;

  { Test suite for TNDXDateTimeHelper formatting functions. }
  TDateTimeUtilsFormattingTests = class(TTestCase)
  published
    { Verifies NowLocalToStr returns non-empty string. }
    procedure TestNowLocalToStr_NotEmpty;

    { Verifies NowLocalToStr respects custom format. }
    procedure TestNowLocalToStr_CustomFormat;

    { Verifies DateTimeToStr formats correctly with default format. }
    procedure TestDateTimeToStr_DefaultFormat;

    { Verifies DateTimeToStr respects custom format. }
    procedure TestDateTimeToStr_CustomFormat;

    { Verifies constant NDX_ISO8601_FORMAT produces expected output. }
    procedure TestConstant_ISO8601Format;

    { Verifies constant NDX_ISO8601_DATE_ONLY produces expected output. }
    procedure TestConstant_ISO8601DateOnly;
  end;

implementation

{ ----------------------------------------------------------------------------- }
{ TDateTimeUtilsIsoTests                                                        }
{ ----------------------------------------------------------------------------- }

procedure TDateTimeUtilsIsoTests.TestNowToIsoUTC_Format;
var
  Iso: string;
begin
  Iso := TNDXDateTimeHelper.NowToIsoUTC;

  // Should be 24 characters: YYYY-MM-DDTHH:MM:SS.sssZ
  AssertEquals('ISO string length', 24, Length(Iso));
end;

procedure TDateTimeUtilsIsoTests.TestNowToIsoUTC_ContainsTAndZ;
var
  Iso: string;
begin
  Iso := TNDXDateTimeHelper.NowToIsoUTC;

  AssertTrue('ISO string must contain T separator', Pos('T', Iso) > 0);
  AssertTrue('ISO string must end with Z', Iso[Length(Iso)] = 'Z');
end;

procedure TDateTimeUtilsIsoTests.TestLocalDateTimeToIsoUTC_KnownDate;
var
  DT: TDateTime;
  Iso: string;
begin
  // Create a known UTC date: 2024-06-15 14:30:45.123
  DT := EncodeDateTime(2024, 6, 15, 14, 30, 45, 123);

  // Convert from "local" (which we treat as the input) to ISO UTC
  Iso := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(DT);

  // The output should end with Z
  AssertTrue('Output must end with Z', Iso[Length(Iso)] = 'Z');
  AssertTrue('Output must contain T', Pos('T', Iso) > 0);
end;

procedure TDateTimeUtilsIsoTests.TestIsoUTCToLocalDateTime_FullFormat;
var
  DT: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DT := TNDXDateTimeHelper.IsoUTCToLocalDateTime('2024-06-15T14:30:45.123Z');

  // Decode and verify (note: result is local time, so we can't check exact values
  // without knowing the timezone, but we can verify it parsed without error)
  DecodeDateTime(DT, Year, Month, Day, Hour, Min, Sec, MSec);

  AssertEquals('Year', 2024, Year);
  AssertEquals('Month', 6, Month);
  AssertEquals('Day', 15, Day);
  // Hour will vary by timezone, so we just check it's valid
  AssertTrue('Hour in valid range', Hour <= 23);
end;

procedure TDateTimeUtilsIsoTests.TestIsoUTCToLocalDateTime_NoMilliseconds;
var
  DT: TDateTime;
begin
  // Should parse without milliseconds
  DT := TNDXDateTimeHelper.IsoUTCToLocalDateTime('2024-06-15T14:30:45Z');

  AssertTrue('Should parse successfully', DT > 0);
end;

procedure TDateTimeUtilsIsoTests.TestIsoUTCToLocalDateTime_DateOnly;
var
  DT: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DT := TNDXDateTimeHelper.IsoUTCToLocalDateTime('2024-06-15');

  DecodeDateTime(DT, Year, Month, Day, Hour, Min, Sec, MSec);

  AssertEquals('Year', 2024, Year);
  AssertEquals('Month', 6, Month);
  AssertEquals('Day', 15, Day);
end;

procedure TDateTimeUtilsIsoTests.TestIsoUTCToLocalDateTime_SpaceSeparator;
var
  DT: TDateTime;
begin
  // Should accept space instead of T
  DT := TNDXDateTimeHelper.IsoUTCToLocalDateTime('2024-06-15 14:30:45.123Z');

  AssertTrue('Should parse successfully', DT > 0);
end;

procedure TDateTimeUtilsIsoTests.TestRoundTrip_LocalToIsoAndBack;
var
  Original, Converted: TDateTime;
  Iso: string;
begin
  Original := Now;
  Iso := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(Original);
  Converted := TNDXDateTimeHelper.IsoUTCToLocalDateTime(Iso);

  // Allow 1 second tolerance due to millisecond rounding
  AssertTrue('Round-trip should preserve value within 1 second',
    Abs(Original - Converted) < (1 / SecsPerDay));
end;

procedure TDateTimeUtilsIsoTests.TestTryIsoUTCToLocalDateTime_ValidInput;
var
  DT: TDateTime;
  Success: Boolean;
begin
  Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime('2024-06-15T14:30:45.123Z', DT);

  AssertTrue('Should return True for valid input', Success);
  AssertTrue('DateTime should be non-zero', DT > 0);
end;

procedure TDateTimeUtilsIsoTests.TestTryIsoUTCToLocalDateTime_InvalidInput;
var
  DT: TDateTime;
  Success: Boolean;
begin
  Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime('not-a-date', DT);

  AssertFalse('Should return False for invalid input', Success);
end;

procedure TDateTimeUtilsIsoTests.TestIsoUTCToLocalDateTime_EmptyString_RaisesException;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    TNDXDateTimeHelper.IsoUTCToLocalDateTime('');
  except
    on E: EConvertError do
      ExceptionRaised := True;
  end;

  AssertTrue('Should raise EConvertError for empty string', ExceptionRaised);
end;

procedure TDateTimeUtilsIsoTests.TestIsoUTCToLocalDateTime_MalformedInput_RaisesException;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    TNDXDateTimeHelper.IsoUTCToLocalDateTime('invalid-date-format');
  except
    on E: Exception do
      ExceptionRaised := True;
  end;

  AssertTrue('Should raise exception for malformed input', ExceptionRaised);
end;

{ ----------------------------------------------------------------------------- }
{ TDateTimeUtilsUnixTests                                                       }
{ ----------------------------------------------------------------------------- }

procedure TDateTimeUtilsUnixTests.TestDateTimeToUnixTimestamp_Epoch;
var
  Timestamp: Int64;
begin
  // Unix epoch is 1970-01-01 00:00:00 UTC
  Timestamp := TNDXDateTimeHelper.DateTimeToUnixTimestamp(UNIX_EPOCH);

  AssertEquals('Unix epoch should return 0', 0, Timestamp);
end;

procedure TDateTimeUtilsUnixTests.TestDateTimeToUnixTimestamp_KnownDate;
var
  DT: TDateTime;
  Timestamp: Int64;
begin
  // 2024-01-01 00:00:00 UTC = 1704067200
  DT := EncodeDateTime(2024, 1, 1, 0, 0, 0, 0);
  Timestamp := TNDXDateTimeHelper.DateTimeToUnixTimestamp(DT);

  AssertEquals('Known date timestamp', 1704067200, Timestamp);
end;

procedure TDateTimeUtilsUnixTests.TestUnixTimestampToDateTime_Epoch;
var
  DT: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DT := TNDXDateTimeHelper.UnixTimestampToDateTime(0);

  DecodeDateTime(DT, Year, Month, Day, Hour, Min, Sec, MSec);

  AssertEquals('Year', 1970, Year);
  AssertEquals('Month', 1, Month);
  AssertEquals('Day', 1, Day);
  AssertEquals('Hour', 0, Hour);
  AssertEquals('Minute', 0, Min);
  AssertEquals('Second', 0, Sec);
end;

procedure TDateTimeUtilsUnixTests.TestUnixTimestampToDateTime_KnownTimestamp;
var
  DT: TDateTime;
  Year, Month, Day: Word;
begin
  // 1704067200 = 2024-01-01 00:00:00 UTC
  DT := TNDXDateTimeHelper.UnixTimestampToDateTime(1704067200);

  DecodeDate(DT, Year, Month, Day);

  AssertEquals('Year', 2024, Year);
  AssertEquals('Month', 1, Month);
  AssertEquals('Day', 1, Day);
end;

procedure TDateTimeUtilsUnixTests.TestRoundTrip_UnixTimestamp;
var
  Original, Converted: Int64;
  DT: TDateTime;
begin
  Original := 1704067200; // 2024-01-01 00:00:00 UTC
  DT := TNDXDateTimeHelper.UnixTimestampToDateTime(Original);
  Converted := TNDXDateTimeHelper.DateTimeToUnixTimestamp(DT);

  AssertEquals('Round-trip should preserve Unix timestamp', Original, Converted);
end;

procedure TDateTimeUtilsUnixTests.TestNowToUnixTimestamp_ReasonableValue;
var
  Timestamp: Int64;
begin
  Timestamp := TNDXDateTimeHelper.NowToUnixTimestamp;

  // Should be after 2024-01-01 (1704067200) and before 2100-01-01 (4102444800)
  AssertTrue('Timestamp should be after 2024', Timestamp > 1704067200);
  AssertTrue('Timestamp should be before 2100', Timestamp < 4102444800);
end;

procedure TDateTimeUtilsUnixTests.TestUnixTimestampToIsoUTC;
var
  Iso: string;
begin
  // 1704067200 = 2024-01-01 00:00:00 UTC
  Iso := TNDXDateTimeHelper.UnixTimestampToIsoUTC(1704067200);

  AssertEquals('ISO string', '2024-01-01T00:00:00.000Z', Iso);
end;

procedure TDateTimeUtilsUnixTests.TestIsoUTCToUnixTimestamp;
var
  Timestamp: Int64;
begin
  Timestamp := TNDXDateTimeHelper.IsoUTCToUnixTimestamp('2024-01-01T00:00:00.000Z');

  AssertEquals('Unix timestamp', 1704067200, Timestamp);
end;

procedure TDateTimeUtilsUnixTests.TestRoundTrip_IsoToUnixAndBack;
var
  OriginalIso, ConvertedIso: string;
  Timestamp: Int64;
begin
  OriginalIso := '2024-06-15T14:30:45.000Z';
  Timestamp := TNDXDateTimeHelper.IsoUTCToUnixTimestamp(OriginalIso);
  ConvertedIso := TNDXDateTimeHelper.UnixTimestampToIsoUTC(Timestamp);

  AssertEquals('Round-trip should preserve ISO string', OriginalIso, ConvertedIso);
end;

{ ----------------------------------------------------------------------------- }
{ TDateTimeUtilsParsingTests                                                    }
{ ----------------------------------------------------------------------------- }

procedure TDateTimeUtilsParsingTests.TestParseIso8601_DetectsUTC;
var
  DT: TDateTime;
  IsUTC: Boolean;
begin
  DT := TNDXDateTimeHelper.ParseIso8601('2024-06-15T14:30:45.123Z', IsUTC);

  AssertTrue('Should detect UTC indicator', IsUTC);
  AssertTrue('Should parse successfully', DT > 0);
end;

procedure TDateTimeUtilsParsingTests.TestParseIso8601_NonUTC;
var
  DT: TDateTime;
  IsUTC: Boolean;
begin
  DT := TNDXDateTimeHelper.ParseIso8601('2024-06-15T14:30:45.123', IsUTC);

  AssertFalse('Should not detect UTC indicator', IsUTC);
  AssertTrue('Should parse successfully', DT > 0);
end;

procedure TDateTimeUtilsParsingTests.TestTryParseIso8601_ValidFormats;
var
  DT: TDateTime;
  IsUTC: Boolean;
begin
  // Full format with Z
  AssertTrue('Full format with Z',
    TNDXDateTimeHelper.TryParseIso8601('2024-06-15T14:30:45.123Z', DT, IsUTC));

  // No milliseconds
  AssertTrue('No milliseconds',
    TNDXDateTimeHelper.TryParseIso8601('2024-06-15T14:30:45Z', DT, IsUTC));

  // Date only
  AssertTrue('Date only',
    TNDXDateTimeHelper.TryParseIso8601('2024-06-15', DT, IsUTC));

  // Space separator
  AssertTrue('Space separator',
    TNDXDateTimeHelper.TryParseIso8601('2024-06-15 14:30:45.123', DT, IsUTC));
end;

procedure TDateTimeUtilsParsingTests.TestTryParseIso8601_InvalidFormats;
var
  DT: TDateTime;
  IsUTC: Boolean;
begin
  AssertFalse('Empty string',
    TNDXDateTimeHelper.TryParseIso8601('', DT, IsUTC));

  AssertFalse('Random text',
    TNDXDateTimeHelper.TryParseIso8601('not-a-date', DT, IsUTC));

  AssertFalse('Invalid date',
    TNDXDateTimeHelper.TryParseIso8601('2024-13-45', DT, IsUTC));
end;

procedure TDateTimeUtilsParsingTests.TestIsIso8601Format_ValidFormats;
begin
  AssertTrue('Full format', TNDXDateTimeHelper.IsIso8601Format('2024-06-15T14:30:45.123Z'));
  AssertTrue('Date only', TNDXDateTimeHelper.IsIso8601Format('2024-06-15'));
  AssertTrue('Space separator', TNDXDateTimeHelper.IsIso8601Format('2024-06-15 14:30:45'));
  AssertTrue('No milliseconds', TNDXDateTimeHelper.IsIso8601Format('2024-06-15T14:30:45Z'));
end;

procedure TDateTimeUtilsParsingTests.TestIsIso8601Format_InvalidFormats;
begin
  AssertFalse('Empty string', TNDXDateTimeHelper.IsIso8601Format(''));
  AssertFalse('Too short', TNDXDateTimeHelper.IsIso8601Format('2024'));
  AssertFalse('Wrong separator', TNDXDateTimeHelper.IsIso8601Format('2024/06/15'));
  AssertFalse('Random text', TNDXDateTimeHelper.IsIso8601Format('hello world'));
  AssertFalse('Missing dashes', TNDXDateTimeHelper.IsIso8601Format('20240615'));
end;

procedure TDateTimeUtilsParsingTests.TestParseIso8601_Milliseconds;
var
  DT: TDateTime;
  IsUTC: Boolean;
  MSec: Word;
  Year, Month, Day, Hour, Min, Sec: Word;
begin
  DT := TNDXDateTimeHelper.ParseIso8601('2024-06-15T14:30:45.999Z', IsUTC);
  DecodeDateTime(DT, Year, Month, Day, Hour, Min, Sec, MSec);

  AssertEquals('Milliseconds', 999, MSec);
end;

{ ----------------------------------------------------------------------------- }
{ TDateTimeUtilsUTCTests                                                        }
{ ----------------------------------------------------------------------------- }

procedure TDateTimeUtilsUTCTests.TestNowUTC_ConsistentWithNow;
var
  LocalNow, UTCNow, ConvertedUTC: TDateTime;
begin
  LocalNow := Now;
  UTCNow := TNDXDateTimeHelper.NowUTC;
  ConvertedUTC := TNDXDateTimeHelper.LocalToUTC(LocalNow);

  // Should be very close (within 1 second)
  AssertTrue('NowUTC should be consistent with LocalToUTC(Now)',
    Abs(UTCNow - ConvertedUTC) < (1 / SecsPerDay));
end;

procedure TDateTimeUtilsUTCTests.TestLocalToUTC_UTCToLocal_Inverse;
var
  Original, Converted: TDateTime;
begin
  Original := Now;
  Converted := TNDXDateTimeHelper.UTCToLocal(TNDXDateTimeHelper.LocalToUTC(Original));

  // Should be identical (within millisecond precision)
  AssertTrue('LocalToUTC and UTCToLocal should be inverse operations',
    Abs(Original - Converted) < (1 / MSecsPerDay));
end;

procedure TDateTimeUtilsUTCTests.TestUTCConversion_Midnight;
var
  LocalMidnight, UTCMidnight, BackToLocal: TDateTime;
begin
  LocalMidnight := Trunc(Now); // Midnight today
  UTCMidnight := TNDXDateTimeHelper.LocalToUTC(LocalMidnight);
  BackToLocal := TNDXDateTimeHelper.UTCToLocal(UTCMidnight);

  AssertTrue('Midnight conversion should round-trip',
    Abs(LocalMidnight - BackToLocal) < (1 / MSecsPerDay));
end;

procedure TDateTimeUtilsUTCTests.TestUTCConversion_Noon;
var
  LocalNoon, UTCNoon, BackToLocal: TDateTime;
begin
  LocalNoon := Trunc(Now) + 0.5; // Noon today
  UTCNoon := TNDXDateTimeHelper.LocalToUTC(LocalNoon);
  BackToLocal := TNDXDateTimeHelper.UTCToLocal(UTCNoon);

  AssertTrue('Noon conversion should round-trip',
    Abs(LocalNoon - BackToLocal) < (1 / MSecsPerDay));
end;

{ ----------------------------------------------------------------------------- }
{ TDateTimeUtilsFormattingTests                                                 }
{ ----------------------------------------------------------------------------- }

procedure TDateTimeUtilsFormattingTests.TestNowLocalToStr_NotEmpty;
var
  S: string;
begin
  S := TNDXDateTimeHelper.NowLocalToStr;

  AssertTrue('NowLocalToStr should return non-empty string', Length(S) > 0);
end;

procedure TDateTimeUtilsFormattingTests.TestNowLocalToStr_CustomFormat;
var
  S: string;
begin
  S := TNDXDateTimeHelper.NowLocalToStr('yyyy');

  // Should be a 4-digit year
  AssertEquals('Custom format year length', 4, Length(S));
  AssertTrue('Year should be numeric', StrToIntDef(S, 0) > 2000);
end;

procedure TDateTimeUtilsFormattingTests.TestDateTimeToStr_DefaultFormat;
var
  DT: TDateTime;
  S: string;
begin
  DT := EncodeDateTime(2024, 6, 15, 14, 30, 45, 123);
  S := TNDXDateTimeHelper.DateTimeToStr(DT);

  AssertEquals('Default format output', '2024-06-15 14:30:45.123', S);
end;

procedure TDateTimeUtilsFormattingTests.TestDateTimeToStr_CustomFormat;
var
  DT: TDateTime;
  S: string;
begin
  DT := EncodeDateTime(2024, 6, 15, 14, 30, 45, 0);
  // Use quoted slashes to avoid locale-specific DateSeparator replacement
  S := TNDXDateTimeHelper.DateTimeToStr(DT, 'dd"/"mm"/"yyyy');

  AssertEquals('Custom format output', '15/06/2024', S);
end;

procedure TDateTimeUtilsFormattingTests.TestConstant_ISO8601Format;
var
  DT: TDateTime;
  S: string;
begin
  DT := EncodeDateTime(2024, 6, 15, 14, 30, 45, 123);
  S := FormatDateTime(NDX_ISO8601_FORMAT, DT);

  AssertEquals('ISO 8601 format constant', '2024-06-15 14:30:45.123', S);
end;

procedure TDateTimeUtilsFormattingTests.TestConstant_ISO8601DateOnly;
var
  DT: TDateTime;
  S: string;
begin
  DT := EncodeDateTime(2024, 6, 15, 14, 30, 45, 123);
  S := FormatDateTime(NDX_ISO8601_DATE_ONLY, DT);

  AssertEquals('ISO 8601 date only constant', '2024-06-15', S);
end;

initialization
  RegisterTest(TDateTimeUtilsIsoTests);
  RegisterTest(TDateTimeUtilsUnixTests);
  RegisterTest(TDateTimeUtilsParsingTests);
  RegisterTest(TDateTimeUtilsUTCTests);
  RegisterTest(TDateTimeUtilsFormattingTests);

end.
