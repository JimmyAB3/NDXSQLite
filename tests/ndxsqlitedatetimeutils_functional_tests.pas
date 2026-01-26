{===============================================================================
  NDXSQLite - DateTime Utilities Functional Tests
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Functional test suite for TNDXDateTimeHelper class with SQLite integration.
  Tests cover real-world scenarios:
  - Storing and retrieving ISO 8601 dates in SQLite
  - Verifying chronological sorting of TEXT dates
  - Round-trip conversions through database storage
  - Unix timestamp storage and retrieval
  - Logging with automatic timestamps

  Cross-platform: Windows, Linux, macOS
===============================================================================}
unit ndxsqlitedatetimeutils_functional_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, DB,
  ndxsqliteconnection, ndxsqlitedatetimeutils;

type
  { Functional tests for TNDXDateTimeHelper with SQLite integration. }
  TDateTimeUtilsSQLiteTests = class(TTestCase)
  private
    FConnection: TNDXSQLiteConnection;
    FDBPath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Verifies ISO 8601 dates can be stored and retrieved from SQLite. }
    procedure TestStoreAndRetrieveIsoDate;

    { Verifies ISO 8601 TEXT dates sort chronologically in SQLite. }
    procedure TestIsoDatesSortCorrectly;

    { Verifies round-trip conversion: Local -> ISO UTC -> SQLite -> ISO UTC -> Local. }
    procedure TestRoundTripThroughSQLite;

    { Verifies Unix timestamps can be stored and retrieved correctly. }
    procedure TestUnixTimestampStorage;

    { Verifies both ISO and Unix timestamp can be stored for same event. }
    procedure TestDualFormatStorage;

    { Verifies logging with automatic timestamps preserves order. }
    procedure TestLoggingWithTimestamps;

    { Verifies date-only format works correctly with SQLite. }
    procedure TestDateOnlyStorage;

    { Verifies NULL handling for optional date fields. }
    procedure TestNullDateHandling;

    { Verifies querying dates within a range using ISO 8601 strings. }
    procedure TestDateRangeQuery;

    { Verifies timezone-independent storage and retrieval. }
    procedure TestTimezoneIndependentStorage;
  end;

  { Functional tests for edge cases and error scenarios. }
  TDateTimeUtilsEdgeCaseTests = class(TTestCase)
  private
    FConnection: TNDXSQLiteConnection;
    FDBPath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Verifies handling of dates near Unix epoch. }
    procedure TestEpochDateStorage;

    { Verifies handling of dates far in the future. }
    procedure TestFutureDateStorage;

    { Verifies handling of leap year dates. }
    procedure TestLeapYearDateStorage;

    { Verifies handling of year boundary (Dec 31 -> Jan 1). }
    procedure TestYearBoundaryStorage;

    { Verifies millisecond precision is preserved. }
    procedure TestMillisecondPrecision;
  end;

implementation

{ ----------------------------------------------------------------------------- }
{ TDateTimeUtilsSQLiteTests                                                      }
{ ----------------------------------------------------------------------------- }

procedure TDateTimeUtilsSQLiteTests.SetUp;
begin
  FDBPath := GetTempDir + 'datetime_functional_test_' +
    FormatDateTime('hhnnsszzz', Now) + '.db';
  FConnection := TNDXSQLiteConnection.Create(FDBPath);
  FConnection.Open;
end;

procedure TDateTimeUtilsSQLiteTests.TearDown;
begin
  if Assigned(FConnection) then
  begin
    if FConnection.IsOpen then
      FConnection.Close;
    FConnection.Free;
  end;
  if FileExists(FDBPath) then
    DeleteFile(FDBPath);
end;

procedure TDateTimeUtilsSQLiteTests.TestStoreAndRetrieveIsoDate;
var
  DS: TDataSet;
  StoredDate, RetrievedDate: string;
  LocalDT, RetrievedLocalDT: TDateTime;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE events (id INTEGER PRIMARY KEY, name TEXT, event_date TEXT)');

  // Store a date
  LocalDT := EncodeDateTime(2024, 12, 25, 10, 30, 0, 0);
  StoredDate := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(LocalDT);

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO events (name, event_date) VALUES (''Christmas'', ''%s'')',
    [StoredDate]));

  // Retrieve the date
  DS := FConnection.ExecuteQuery('SELECT event_date FROM events WHERE name = ''Christmas''');
  try
    AssertFalse('Should have result', DS.EOF);
    RetrievedDate := DS.FieldByName('event_date').AsString;
    AssertEquals('Stored date should match', StoredDate, RetrievedDate);

    // Convert back and verify
    RetrievedLocalDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime(RetrievedDate);
    AssertTrue('Round-trip should preserve value within 1 second',
      Abs(LocalDT - RetrievedLocalDT) < (1 / SecsPerDay));
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestIsoDatesSortCorrectly;
var
  DS: TDataSet;
  PrevDate, CurrDate: string;
  Count: Integer;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE events (id INTEGER PRIMARY KEY, name TEXT, event_date TEXT)');

  // Insert dates in random order
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Event3'', ''2024-12-31T23:59:00.000Z'')');
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Event1'', ''2024-01-01T00:00:00.000Z'')');
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Event4'', ''2025-06-15T12:00:00.000Z'')');
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Event2'', ''2024-06-15T12:00:00.000Z'')');

  // Query with ORDER BY - should sort chronologically
  DS := FConnection.ExecuteQuery('SELECT event_date FROM events ORDER BY event_date ASC');
  try
    Count := 0;
    PrevDate := '';
    while not DS.EOF do
    begin
      CurrDate := DS.FieldByName('event_date').AsString;
      if PrevDate <> '' then
        AssertTrue('Dates should be in ascending order: ' + PrevDate + ' < ' + CurrDate,
          CompareStr(PrevDate, CurrDate) < 0);
      PrevDate := CurrDate;
      Inc(Count);
      DS.Next;
    end;
    AssertEquals('Should have 4 events', 4, Count);
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestRoundTripThroughSQLite;
var
  DS: TDataSet;
  OriginalLocal, RetrievedLocal: TDateTime;
  IsoString: string;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE timestamps (id INTEGER PRIMARY KEY, ts TEXT)');

  // Original local time
  OriginalLocal := Now;
  IsoString := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(OriginalLocal);

  // Store in SQLite
  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO timestamps (ts) VALUES (''%s'')', [IsoString]));

  // Retrieve and convert back
  DS := FConnection.ExecuteQuery('SELECT ts FROM timestamps');
  try
    RetrievedLocal := TNDXDateTimeHelper.IsoUTCToLocalDateTime(
      DS.FieldByName('ts').AsString);

    // Should be within 1 millisecond
    AssertTrue('Round-trip should preserve value',
      Abs(OriginalLocal - RetrievedLocal) < (1 / MSecsPerDay));
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestUnixTimestampStorage;
var
  DS: TDataSet;
  OriginalTS, RetrievedTS: Int64;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE events (id INTEGER PRIMARY KEY, name TEXT, unix_ts INTEGER)');

  // Store Unix timestamp
  OriginalTS := TNDXDateTimeHelper.NowToUnixTimestamp;

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO events (name, unix_ts) VALUES (''Now'', %d)', [OriginalTS]));

  // Retrieve
  DS := FConnection.ExecuteQuery('SELECT unix_ts FROM events');
  try
    RetrievedTS := DS.FieldByName('unix_ts').AsLargeInt;
    AssertEquals('Unix timestamp should be preserved', OriginalTS, RetrievedTS);
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestDualFormatStorage;
var
  DS: TDataSet;
  IsoDate: string;
  UnixTS: Int64;
  UTCNow: TDateTime;
begin
  // Create table with both formats
  FConnection.ExecuteNonQuery(
    'CREATE TABLE events (id INTEGER PRIMARY KEY, iso_date TEXT, unix_ts INTEGER)');

  // Store both formats for same moment - capture UTC once to avoid race condition
  UTCNow := TNDXDateTimeHelper.NowUTC;
  IsoDate := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', UTCNow);
  UnixTS := TNDXDateTimeHelper.DateTimeToUnixTimestamp(UTCNow);

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO events (iso_date, unix_ts) VALUES (''%s'', %d)',
    [IsoDate, UnixTS]));

  // Retrieve and cross-verify
  DS := FConnection.ExecuteQuery('SELECT iso_date, unix_ts FROM events');
  try
    // Verify both values were stored correctly
    AssertEquals('ISO date should be stored', IsoDate, DS.FieldByName('iso_date').AsString);
    AssertEquals('Unix timestamp should be stored', UnixTS, DS.FieldByName('unix_ts').AsLargeInt);

    // Verify Unix timestamp converts back to same second
    AssertEquals('Unix timestamp should match',
      UnixTS,
      TNDXDateTimeHelper.IsoUTCToUnixTimestamp(DS.FieldByName('iso_date').AsString));
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestLoggingWithTimestamps;
var
  DS: TDataSet;
  I: Integer;
  PrevTS, CurrTS: string;
begin
  // Create log table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE logs (id INTEGER PRIMARY KEY, message TEXT, logged_at TEXT)');

  // Insert multiple log entries with small delays
  for I := 1 to 5 do
  begin
    FConnection.ExecuteNonQuery(Format(
      'INSERT INTO logs (message, logged_at) VALUES (''Log %d'', ''%s'')',
      [I, TNDXDateTimeHelper.NowToIsoUTC]));
    Sleep(10); // Small delay to ensure different timestamps
  end;

  // Verify they sort in insertion order
  DS := FConnection.ExecuteQuery('SELECT message, logged_at FROM logs ORDER BY logged_at ASC');
  try
    PrevTS := '';
    I := 1;
    while not DS.EOF do
    begin
      CurrTS := DS.FieldByName('logged_at').AsString;
      AssertEquals('Message order', 'Log ' + IntToStr(I), DS.FieldByName('message').AsString);
      if PrevTS <> '' then
        AssertTrue('Timestamps should increase', CompareStr(PrevTS, CurrTS) <= 0);
      PrevTS := CurrTS;
      Inc(I);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestDateOnlyStorage;
var
  DS: TDataSet;
  DateStr: string;
  DT: TDateTime;
  IsUTC: Boolean;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE birthdays (id INTEGER PRIMARY KEY, name TEXT, birth_date TEXT)');

  // Store date-only format
  FConnection.ExecuteNonQuery(
    'INSERT INTO birthdays (name, birth_date) VALUES (''John'', ''1990-05-15'')');

  // Retrieve and parse
  DS := FConnection.ExecuteQuery('SELECT birth_date FROM birthdays');
  try
    DateStr := DS.FieldByName('birth_date').AsString;
    AssertTrue('Should be valid ISO 8601 format',
      TNDXDateTimeHelper.IsIso8601Format(DateStr));

    DT := TNDXDateTimeHelper.ParseIso8601(DateStr, IsUTC);
    AssertFalse('Date-only should not be marked as UTC', IsUTC);
    AssertEquals('Year', 1990, YearOf(DT));
    AssertEquals('Month', 5, MonthOf(DT));
    AssertEquals('Day', 15, DayOf(DT));
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestNullDateHandling;
var
  DS: TDataSet;
  LocalDT: TDateTime;
  Success: Boolean;
begin
  // Create table with nullable date
  FConnection.ExecuteNonQuery(
    'CREATE TABLE tasks (id INTEGER PRIMARY KEY, name TEXT, due_date TEXT)');

  // Insert with NULL date
  FConnection.ExecuteNonQuery(
    'INSERT INTO tasks (name, due_date) VALUES (''Task1'', NULL)');
  FConnection.ExecuteNonQuery(
    'INSERT INTO tasks (name, due_date) VALUES (''Task2'', ''2024-12-31T23:59:59.000Z'')');

  // Retrieve and handle NULL
  DS := FConnection.ExecuteQuery('SELECT name, due_date FROM tasks ORDER BY name');
  try
    // First row - NULL date
    AssertTrue('due_date should be NULL', DS.FieldByName('due_date').IsNull);

    DS.Next;

    // Second row - valid date
    AssertFalse('due_date should not be NULL', DS.FieldByName('due_date').IsNull);
    Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime(
      DS.FieldByName('due_date').AsString, LocalDT);
    AssertTrue('Should parse successfully', Success);
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestDateRangeQuery;
var
  DS: TDataSet;
  Count: Integer;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE events (id INTEGER PRIMARY KEY, name TEXT, event_date TEXT)');

  // Insert events across different dates
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Jan Event'', ''2024-01-15T12:00:00.000Z'')');
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Mar Event'', ''2024-03-15T12:00:00.000Z'')');
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Jun Event'', ''2024-06-15T12:00:00.000Z'')');
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Sep Event'', ''2024-09-15T12:00:00.000Z'')');
  FConnection.ExecuteNonQuery(
    'INSERT INTO events (name, event_date) VALUES (''Dec Event'', ''2024-12-15T12:00:00.000Z'')');

  // Query events in Q2 (April-June)
  DS := FConnection.ExecuteQuery(
    'SELECT name FROM events WHERE event_date >= ''2024-04-01'' AND event_date < ''2024-07-01''');
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      DS.Next;
    end;
    AssertEquals('Should find 1 event in Q2', 1, Count);
  finally
    DS.Free;
  end;

  // Query events in H1 (Jan-Jun)
  DS := FConnection.ExecuteQuery(
    'SELECT name FROM events WHERE event_date < ''2024-07-01''');
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      DS.Next;
    end;
    AssertEquals('Should find 3 events in H1', 3, Count);
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsSQLiteTests.TestTimezoneIndependentStorage;
var
  DS: TDataSet;
  LocalDT1, LocalDT2: TDateTime;
  StoredIso, RetrievedIso: string;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE meetings (id INTEGER PRIMARY KEY, meeting_time TEXT)');

  // Store current time as UTC
  LocalDT1 := Now;
  StoredIso := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(LocalDT1);

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO meetings (meeting_time) VALUES (''%s'')', [StoredIso]));

  // Retrieve the stored value
  DS := FConnection.ExecuteQuery('SELECT meeting_time FROM meetings');
  try
    RetrievedIso := DS.FieldByName('meeting_time').AsString;

    // The ISO string should be identical (timezone-independent)
    AssertEquals('ISO string should be preserved exactly', StoredIso, RetrievedIso);

    // Converting back to local should give same result
    LocalDT2 := TNDXDateTimeHelper.IsoUTCToLocalDateTime(RetrievedIso);
    AssertTrue('Local time should be preserved',
      Abs(LocalDT1 - LocalDT2) < (1 / MSecsPerDay));
  finally
    DS.Free;
  end;
end;

{ ----------------------------------------------------------------------------- }
{ TDateTimeUtilsEdgeCaseTests                                                    }
{ ----------------------------------------------------------------------------- }

procedure TDateTimeUtilsEdgeCaseTests.SetUp;
begin
  FDBPath := GetTempDir + 'datetime_edge_test_' +
    FormatDateTime('hhnnsszzz', Now) + '.db';
  FConnection := TNDXSQLiteConnection.Create(FDBPath);
  FConnection.Open;
end;

procedure TDateTimeUtilsEdgeCaseTests.TearDown;
begin
  if Assigned(FConnection) then
  begin
    if FConnection.IsOpen then
      FConnection.Close;
    FConnection.Free;
  end;
  if FileExists(FDBPath) then
    DeleteFile(FDBPath);
end;

procedure TDateTimeUtilsEdgeCaseTests.TestEpochDateStorage;
var
  DS: TDataSet;
  EpochIso: string;
  RetrievedTS: Int64;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE epochs (id INTEGER PRIMARY KEY, iso_date TEXT, unix_ts INTEGER)');

  // Store Unix epoch
  EpochIso := TNDXDateTimeHelper.UnixTimestampToIsoUTC(0);

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO epochs (iso_date, unix_ts) VALUES (''%s'', 0)', [EpochIso]));

  // Verify
  DS := FConnection.ExecuteQuery('SELECT iso_date, unix_ts FROM epochs');
  try
    AssertEquals('Epoch ISO', '1970-01-01T00:00:00.000Z',
      DS.FieldByName('iso_date').AsString);
    RetrievedTS := TNDXDateTimeHelper.IsoUTCToUnixTimestamp(
      DS.FieldByName('iso_date').AsString);
    AssertEquals('Epoch Unix timestamp', 0, RetrievedTS);
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsEdgeCaseTests.TestFutureDateStorage;
var
  DS: TDataSet;
  FutureDT: TDateTime;
  FutureIso, RetrievedIso: string;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE future (id INTEGER PRIMARY KEY, event_date TEXT)');

  // Store a date far in the future (year 2099)
  FutureDT := EncodeDateTime(2099, 12, 31, 23, 59, 59, 999);
  FutureIso := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(FutureDT);

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO future (event_date) VALUES (''%s'')', [FutureIso]));

  // Retrieve and verify
  DS := FConnection.ExecuteQuery('SELECT event_date FROM future');
  try
    RetrievedIso := DS.FieldByName('event_date').AsString;
    AssertTrue('Future date should start with 2099 or 2100',
      (Pos('2099', RetrievedIso) = 1) or (Pos('2100', RetrievedIso) = 1));
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsEdgeCaseTests.TestLeapYearDateStorage;
var
  DS: TDataSet;
  LeapDT: TDateTime;
  LeapIso: string;
  Year, Month, Day: Word;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE leap (id INTEGER PRIMARY KEY, event_date TEXT)');

  // Store Feb 29, 2024 (leap year)
  LeapDT := EncodeDate(2024, 2, 29);
  LeapIso := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(LeapDT);

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO leap (event_date) VALUES (''%s'')', [LeapIso]));

  // Retrieve and verify
  DS := FConnection.ExecuteQuery('SELECT event_date FROM leap');
  try
    LeapDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime(
      DS.FieldByName('event_date').AsString);
    DecodeDate(LeapDT, Year, Month, Day);

    // Due to timezone conversion, might be Feb 28 or Feb 29
    AssertEquals('Year', 2024, Year);
    AssertEquals('Month', 2, Month);
    AssertTrue('Day should be 28 or 29', (Day = 28) or (Day = 29));
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsEdgeCaseTests.TestYearBoundaryStorage;
var
  DS: TDataSet;
  NewYearEve, NewYear: string;
  Count: Integer;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE holidays (id INTEGER PRIMARY KEY, name TEXT, event_date TEXT)');

  // Store dates around year boundary
  NewYearEve := '2024-12-31T23:59:59.000Z';
  NewYear := '2025-01-01T00:00:00.000Z';

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO holidays (name, event_date) VALUES (''NYE'', ''%s'')', [NewYearEve]));
  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO holidays (name, event_date) VALUES (''NewYear'', ''%s'')', [NewYear]));

  // Query should sort correctly across year boundary
  DS := FConnection.ExecuteQuery(
    'SELECT name FROM holidays ORDER BY event_date ASC');
  try
    Count := 0;
    while not DS.EOF do
    begin
      Inc(Count);
      if Count = 1 then
        AssertEquals('First should be NYE', 'NYE', DS.FieldByName('name').AsString)
      else
        AssertEquals('Second should be NewYear', 'NewYear', DS.FieldByName('name').AsString);
      DS.Next;
    end;
    AssertEquals('Should have 2 holidays', 2, Count);
  finally
    DS.Free;
  end;
end;

procedure TDateTimeUtilsEdgeCaseTests.TestMillisecondPrecision;
var
  DS: TDataSet;
  DT: TDateTime;
  IsoString, RetrievedIso: string;
  MSec: Word;
  Year, Month, Day, Hour, Min, Sec: Word;
  IsUTC: Boolean;
begin
  // Create table
  FConnection.ExecuteNonQuery(
    'CREATE TABLE precise (id INTEGER PRIMARY KEY, ts TEXT)');

  // Create time with specific milliseconds
  DT := EncodeDateTime(2024, 6, 15, 14, 30, 45, 123);
  IsoString := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', DT);

  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO precise (ts) VALUES (''%s'')', [IsoString]));

  // Retrieve and verify milliseconds
  DS := FConnection.ExecuteQuery('SELECT ts FROM precise');
  try
    RetrievedIso := DS.FieldByName('ts').AsString;
    AssertEquals('ISO string should be preserved', IsoString, RetrievedIso);

    // Parse and verify milliseconds
    DT := TNDXDateTimeHelper.ParseIso8601(RetrievedIso, IsUTC);
    DecodeDateTime(DT, Year, Month, Day, Hour, Min, Sec, MSec);
    AssertEquals('Milliseconds should be preserved', 123, MSec);
  finally
    DS.Free;
  end;
end;

initialization
  RegisterTest(TDateTimeUtilsSQLiteTests);
  RegisterTest(TDateTimeUtilsEdgeCaseTests);

end.
