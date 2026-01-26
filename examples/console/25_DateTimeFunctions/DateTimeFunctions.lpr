{===============================================================================
  NDXSQLite Example 25 - Date/Time Functions
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - SQLite date/time functions
  - Date arithmetic
  - Formatting dates
  - Time zone handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DateTimeFunctions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Queries and displays the current date, time, UTC datetime, local datetime, and Julian day number. }
procedure DemoCurrentDateTime;
var
  DS: TDataSet;
begin
  WriteLn('1. Current Date/Time Functions');
  WriteLn('   ---------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  date(''now'') as today, ' +
    '  time(''now'') as current_time, ' +
    '  datetime(''now'') as now_utc, ' +
    '  datetime(''now'', ''localtime'') as now_local, ' +
    '  julianday(''now'') as julian_day');
  try
    WriteLn('   today (date):        ', DS.FieldByName('today').AsString);
    WriteLn('   current_time (time): ', DS.FieldByName('current_time').AsString);
    WriteLn('   now (UTC):           ', DS.FieldByName('now_utc').AsString);
    WriteLn('   now (local):         ', DS.FieldByName('now_local').AsString);
    WriteLn('   Julian day:          ', FormatFloat('0.00000', DS.FieldByName('julian_day').AsFloat));
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Computes tomorrow, yesterday, next week/month/year, and start-of-month/year relative to today. }
procedure DemoDateArithmetic;
var
  DS: TDataSet;
begin
  WriteLn('2. Date Arithmetic');
  WriteLn('   ---------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  date(''now'') as today, ' +
    '  date(''now'', ''+1 day'') as tomorrow, ' +
    '  date(''now'', ''-1 day'') as yesterday, ' +
    '  date(''now'', ''+1 week'') as next_week, ' +
    '  date(''now'', ''+1 month'') as next_month, ' +
    '  date(''now'', ''+1 year'') as next_year, ' +
    '  date(''now'', ''start of month'') as month_start, ' +
    '  date(''now'', ''start of year'') as year_start');
  try
    WriteLn('   Today:           ', DS.FieldByName('today').AsString);
    WriteLn('   Tomorrow:        ', DS.FieldByName('tomorrow').AsString);
    WriteLn('   Yesterday:       ', DS.FieldByName('yesterday').AsString);
    WriteLn('   Next week:       ', DS.FieldByName('next_week').AsString);
    WriteLn('   Next month:      ', DS.FieldByName('next_month').AsString);
    WriteLn('   Next year:       ', DS.FieldByName('next_year').AsString);
    WriteLn('   Start of month:  ', DS.FieldByName('month_start').AsString);
    WriteLn('   Start of year:   ', DS.FieldByName('year_start').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Adds and subtracts hours, minutes, and seconds from the current time using SQLite modifiers. }
procedure DemoTimeArithmetic;
var
  DS: TDataSet;
begin
  WriteLn('3. Time Arithmetic');
  WriteLn('   ---------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  time(''now'') as now_time, ' +
    '  time(''now'', ''+1 hour'') as plus_1h, ' +
    '  time(''now'', ''-30 minutes'') as minus_30m, ' +
    '  time(''now'', ''+90 seconds'') as plus_90s, ' +
    '  datetime(''now'', ''+2 hours'', ''+30 minutes'') as combined');
  try
    WriteLn('   Now:              ', DS.FieldByName('now_time').AsString);
    WriteLn('   +1 hour:          ', DS.FieldByName('plus_1h').AsString);
    WriteLn('   -30 minutes:      ', DS.FieldByName('minus_30m').AsString);
    WriteLn('   +90 seconds:      ', DS.FieldByName('plus_90s').AsString);
    WriteLn('   +2h30m combined:  ', DS.FieldByName('combined').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Demonstrates date/time formatting with the strftime() function. }
procedure DemoStrftime;
var
  DS: TDataSet;
begin
  WriteLn('4. strftime() Formatting');
  WriteLn('   ---------------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  strftime(''%Y'', ''now'') as year, ' +
    '  strftime(''%m'', ''now'') as month, ' +
    '  strftime(''%d'', ''now'') as day, ' +
    '  strftime(''%H:%M:%S'', ''now'') as time_fmt, ' +
    '  strftime(''%Y-%m-%d %H:%M'', ''now'') as datetime_fmt, ' +
    '  strftime(''%w'', ''now'') as weekday, ' +
    '  strftime(''%W'', ''now'') as week_num, ' +
    '  strftime(''%j'', ''now'') as day_of_year, ' +
    '  strftime(''%s'', ''now'') as unix_timestamp');
  try
    WriteLn('   Year (%Y):            ', DS.FieldByName('year').AsString);
    WriteLn('   Month (%m):           ', DS.FieldByName('month').AsString);
    WriteLn('   Day (%d):             ', DS.FieldByName('day').AsString);
    WriteLn('   Time (%H:%M:%S):      ', DS.FieldByName('time_fmt').AsString);
    WriteLn('   DateTime formatted:   ', DS.FieldByName('datetime_fmt').AsString);
    WriteLn('   Weekday (%w, 0=Sun):  ', DS.FieldByName('weekday').AsString);
    WriteLn('   Week number (%W):     ', DS.FieldByName('week_num').AsString);
    WriteLn('   Day of year (%j):     ', DS.FieldByName('day_of_year').AsString);
    WriteLn('   Unix timestamp (%s):  ', DS.FieldByName('unix_timestamp').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Calculates differences between dates in days and weeks using julianday() and strftime(). }
procedure DemoDateDifference;
var
  DS: TDataSet;
begin
  WriteLn('5. Date Differences');
  WriteLn('   ----------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  julianday(''2025-12-31'') - julianday(''2025-01-01'') as days_in_year, ' +
    '  julianday(''now'') - julianday(''2000-01-01'') as days_since_2000, ' +
    '  CAST((julianday(''2025-06-15'') - julianday(''2025-01-01'')) / 7 AS INTEGER) as weeks_diff, ' +
    '  (strftime(''%s'', ''now'') - strftime(''%s'', ''2024-01-01'')) / 86400 as days_since_2024');
  try
    WriteLn('   Days in 2025:         ', DS.FieldByName('days_in_year').AsInteger);
    WriteLn('   Days since 2000-01-01:', FormatFloat('0', DS.FieldByName('days_since_2000').AsFloat));
    WriteLn('   Weeks from Jan to Jun:', DS.FieldByName('weeks_diff').AsInteger);
    WriteLn('   Days since 2024-01-01:', DS.FieldByName('days_since_2024').AsInteger);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Converts between Unix epoch timestamps and ISO datetime strings in both UTC and local time. }
procedure DemoUnixTimestamp;
var
  DS: TDataSet;
begin
  WriteLn('6. Unix Timestamp Conversion');
  WriteLn('   -------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  strftime(''%s'', ''now'') as current_unix, ' +
    '  datetime(1704067200, ''unixepoch'') as from_unix, ' +
    '  datetime(1704067200, ''unixepoch'', ''localtime'') as from_unix_local, ' +
    '  strftime(''%s'', ''2024-06-15 12:30:00'') as to_unix');
  try
    WriteLn('   Current Unix timestamp:     ', DS.FieldByName('current_unix').AsString);
    WriteLn('   1704067200 to datetime:     ', DS.FieldByName('from_unix').AsString, ' (UTC)');
    WriteLn('   1704067200 to local:        ', DS.FieldByName('from_unix_local').AsString);
    WriteLn('   2024-06-15 12:30 to Unix:   ', DS.FieldByName('to_unix').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Creates an events table, queries upcoming events with days-until, and groups events by month. }
procedure DemoTableWithDates;
var
  DS: TDataSet;
begin
  WriteLn('7. Working with Date Columns');
  WriteLn('   -------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS events (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  event_date TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO events (name, event_date) VALUES (''Conference'', ''2025-06-15'')');
  Connection.ExecuteNonQuery('INSERT INTO events (name, event_date) VALUES (''Workshop'', ''2025-03-20'')');
  Connection.ExecuteNonQuery('INSERT INTO events (name, event_date) VALUES (''Meetup'', ''2025-01-10'')');
  Connection.ExecuteNonQuery('INSERT INTO events (name, event_date) VALUES (''Hackathon'', ''2025-09-05'')');

  // Query with date filtering
  DS := Connection.ExecuteQuery(
    'SELECT name, event_date, ' +
    '  CAST(julianday(event_date) - julianday(''now'') AS INTEGER) as days_until ' +
    'FROM events ' +
    'WHERE event_date >= date(''now'') ' +
    'ORDER BY event_date');
  try
    WriteLn('   Upcoming events:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s on %s (in %d days)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('event_date').AsString,
         DS.FieldByName('days_until').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Group by month
  DS := Connection.ExecuteQuery(
    'SELECT strftime(''%Y-%m'', event_date) as month, COUNT(*) as count ' +
    'FROM events GROUP BY month ORDER BY month');
  try
    WriteLn('   Events by month:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d event(s)',
        [DS.FieldByName('month').AsString,
         DS.FieldByName('count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Maps the numeric weekday and month from strftime() to human-readable names using CASE expressions. }
procedure DemoWeekdayNames;
var
  DS: TDataSet;
begin
  WriteLn('8. Weekday and Month Names');
  WriteLn('   -----------------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  date(''now'') as today, ' +
    '  CASE CAST(strftime(''%w'', ''now'') AS INTEGER) ' +
    '    WHEN 0 THEN ''Sunday'' ' +
    '    WHEN 1 THEN ''Monday'' ' +
    '    WHEN 2 THEN ''Tuesday'' ' +
    '    WHEN 3 THEN ''Wednesday'' ' +
    '    WHEN 4 THEN ''Thursday'' ' +
    '    WHEN 5 THEN ''Friday'' ' +
    '    WHEN 6 THEN ''Saturday'' ' +
    '  END as weekday_name, ' +
    '  CASE CAST(strftime(''%m'', ''now'') AS INTEGER) ' +
    '    WHEN 1 THEN ''January'' WHEN 2 THEN ''February'' ' +
    '    WHEN 3 THEN ''March'' WHEN 4 THEN ''April'' ' +
    '    WHEN 5 THEN ''May'' WHEN 6 THEN ''June'' ' +
    '    WHEN 7 THEN ''July'' WHEN 8 THEN ''August'' ' +
    '    WHEN 9 THEN ''September'' WHEN 10 THEN ''October'' ' +
    '    WHEN 11 THEN ''November'' WHEN 12 THEN ''December'' ' +
    '  END as month_name');
  try
    WriteLn('   Today: ', DS.FieldByName('today').AsString);
    WriteLn('   Weekday: ', DS.FieldByName('weekday_name').AsString);
    WriteLn('   Month: ', DS.FieldByName('month_name').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Outputs recommended date/time storage practices, format specifiers, and conversion tips. }
procedure DemoBestPractices;
begin
  WriteLn('9. Date/Time Best Practices');
  WriteLn('   ------------------------');
  WriteLn('   - Store dates as TEXT in ISO 8601 format (YYYY-MM-DD HH:MM:SS)');
  WriteLn('   - Store in UTC, convert to local time on display');
  WriteLn('   - Use datetime(''now'') for automatic timestamps');
  WriteLn('   - Use julianday() for date arithmetic');
  WriteLn('   - Index date columns for range queries');
  WriteLn('   - Use strftime() for custom formatting');
  WriteLn('   - For Unix timestamps, use strftime(''%s'', ...)');
  WriteLn('');
  WriteLn('   Format specifiers:');
  WriteLn('   %Y = Year (4 digits)    %m = Month (01-12)');
  WriteLn('   %d = Day (01-31)        %H = Hour (00-23)');
  WriteLn('   %M = Minute (00-59)     %S = Second (00-59)');
  WriteLn('   %w = Weekday (0-6)      %W = Week number');
  WriteLn('   %j = Day of year        %s = Unix timestamp');
  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 25: Date/Time Functions ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example25.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      DemoCurrentDateTime;
      DemoDateArithmetic;
      DemoTimeArithmetic;
      DemoStrftime;
      DemoDateDifference;
      DemoUnixTimestamp;
      DemoTableWithDates;
      DemoWeekdayNames;
      DemoBestPractices;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
