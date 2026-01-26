{===============================================================================
  NDXSQLite Example 121 - Calendar Events
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Event creation with recurrence rules
  - Occurrence generation from patterns
  - Conflict detection for overlapping events
  - Exception handling for recurring events
  - Daily, weekly, and monthly recurrence

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CalendarEvents;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Parses an ISO date string (yyyy-mm-dd) into a TDateTime value. }
function ISOToDate(const S: string): TDateTime;
var
  Y, M, D: Word;
begin
  // Parse yyyy-mm-dd format
  Y := StrToInt(Copy(S, 1, 4));
  M := StrToInt(Copy(S, 6, 2));
  D := StrToInt(Copy(S, 9, 2));
  Result := EncodeDate(Y, M, D);
end;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Events table - defines calendar events with optional recurrence
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  event_id TEXT NOT NULL,' +
    '  title TEXT NOT NULL,' +
    '  description TEXT DEFAULT '''',' +
    '  location TEXT DEFAULT '''',' +
    '  start_date TEXT NOT NULL,' +       // yyyy-mm-dd
    '  start_time TEXT NOT NULL,' +        // hh:mm
    '  end_time TEXT NOT NULL,' +          // hh:mm
    '  duration_minutes INTEGER DEFAULT 60,' +
    '  is_recurring INTEGER DEFAULT 0,' +
    '  recurrence_type TEXT,' +            // daily, weekly, monthly, yearly
    '  recurrence_interval INTEGER DEFAULT 1,' +  // every N days/weeks/months
    '  recurrence_days TEXT,' +            // for weekly: comma-separated day numbers (1=Mon..7=Sun)
    '  recurrence_day_of_month INTEGER,' + // for monthly: day of month
    '  recurrence_end_date TEXT,' +        // when recurrence stops (NULL = no end)
    '  recurrence_count INTEGER,' +        // max occurrences (NULL = unlimited)
    '  calendar_id TEXT DEFAULT ''default'',' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(event_id)' +
    ')'
  );

  // Event exceptions - skip or modify specific occurrences
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_exceptions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  event_id TEXT NOT NULL,' +
    '  exception_date TEXT NOT NULL,' +     // the date being modified/skipped
    '  exception_type TEXT NOT NULL,' +     // cancelled, modified
    '  new_title TEXT,' +
    '  new_start_time TEXT,' +
    '  new_end_time TEXT,' +
    '  new_location TEXT,' +
    '  reason TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(event_id, exception_date)' +
    ')'
  );

  // Generated occurrences - materialized view of expanded events
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_occurrences (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  event_id TEXT NOT NULL,' +
    '  occurrence_date TEXT NOT NULL,' +
    '  start_time TEXT NOT NULL,' +
    '  end_time TEXT NOT NULL,' +
    '  title TEXT NOT NULL,' +
    '  location TEXT,' +
    '  is_exception INTEGER DEFAULT 0,' +
    '  is_cancelled INTEGER DEFAULT 0,' +
    '  calendar_id TEXT,' +
    '  UNIQUE(event_id, occurrence_date)' +
    ')'
  );

  // Conflicts log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_conflicts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  event_id_1 TEXT NOT NULL,' +
    '  event_id_2 TEXT NOT NULL,' +
    '  conflict_date TEXT NOT NULL,' +
    '  overlap_start TEXT NOT NULL,' +
    '  overlap_end TEXT NOT NULL,' +
    '  detected_at TEXT DEFAULT (datetime(''now''))' +
    ')'
  );
end;

{ Creates an event with optional recurrence settings. }
procedure CreateEvent(const EventId, Title, Description, Location: string;
  const StartDate, StartTime, EndTime: string; DurationMinutes: Integer;
  IsRecurring: Boolean; const RecurrenceType: string;
  RecurrenceInterval: Integer; const RecurrenceDays: string;
  RecurrenceDayOfMonth: Integer; const RecurrenceEndDate: string;
  RecurrenceCount: Integer; const CalendarId: string);
var
  RecurInt: Integer;
  EndDateVal, CountVal: string;
begin
  if IsRecurring then
    RecurInt := 1
  else
    RecurInt := 0;

  if RecurrenceEndDate <> '' then
    EndDateVal := '''' + RecurrenceEndDate + ''''
  else
    EndDateVal := 'NULL';

  if RecurrenceCount > 0 then
    CountVal := IntToStr(RecurrenceCount)
  else
    CountVal := 'NULL';

  Conn.ExecuteNonQuery(Format(
    'INSERT INTO events (event_id, title, description, location, ' +
    'start_date, start_time, end_time, duration_minutes, ' +
    'is_recurring, recurrence_type, recurrence_interval, ' +
    'recurrence_days, recurrence_day_of_month, recurrence_end_date, ' +
    'recurrence_count, calendar_id) VALUES ' +
    '(''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %d, ' +
    '%d, ''%s'', %d, ''%s'', %d, %s, %s, ''%s'')',
    [EventId, Title, Description, Location,
     StartDate, StartTime, EndTime, DurationMinutes,
     RecurInt, RecurrenceType, RecurrenceInterval,
     RecurrenceDays, RecurrenceDayOfMonth, EndDateVal,
     CountVal, CalendarId]));
end;

{ Returns the ISO day of week (Monday=1 through Sunday=7). }
function DayOfWeekISO(D: TDateTime): Integer;
var
  DOW: Integer;
begin
  DOW := DayOfWeek(D);  // 1=Sun, 2=Mon, ..., 7=Sat
  if DOW = 1 then
    Result := 7  // Sunday = 7
  else
    Result := DOW - 1;  // Mon=1, Tue=2, ..., Sat=6
end;

{ Checks whether a day list string contains the specified day number. }
function ContainsDay(const DaysList: string; Day: Integer): Boolean;
var
  DayStr: string;
begin
  DayStr := IntToStr(Day);
  Result := (Pos(DayStr, DaysList) > 0);
end;

{ Expands a recurring or single event into individual occurrence rows within the specified date range, applying any exceptions (cancellations or modifications). }
procedure GenerateOccurrences(const EventId: string; const FromDate, ToDate: string);
var
  DS: TDataSet;
  StartDate: TDateTime;
  CurrentDate: TDateTime;
  EndDate: TDateTime;
  RangeStart, RangeEnd: TDateTime;
  RecType, RecDays, StartTime, EndTime, Title, Location, CalendarId: string;
  RecInterval, RecDayOfMonth, RecCount: Integer;
  IsRecurring: Boolean;
  Count: Integer;
  DateStr: string;
  ExDS: TDataSet;
  ExType, NewTitle, NewStartTime, NewEndTime, NewLocation: string;
begin
  // Get event details
  DS := Conn.ExecuteQuery(Format(
    'SELECT * FROM events WHERE event_id = ''%s''', [EventId]));
  try
    if DS.IsEmpty then Exit;
    Title := DS.FieldByName('title').AsString;
    Location := DS.FieldByName('location').AsString;
    StartTime := DS.FieldByName('start_time').AsString;
    EndTime := DS.FieldByName('end_time').AsString;
    CalendarId := DS.FieldByName('calendar_id').AsString;
    IsRecurring := DS.FieldByName('is_recurring').AsInteger = 1;
    RecType := DS.FieldByName('recurrence_type').AsString;
    RecInterval := DS.FieldByName('recurrence_interval').AsInteger;
    RecDays := DS.FieldByName('recurrence_days').AsString;
    RecDayOfMonth := DS.FieldByName('recurrence_day_of_month').AsInteger;

    if not DS.FieldByName('recurrence_end_date').IsNull then
      EndDate := ISOToDate(DS.FieldByName('recurrence_end_date').AsString)
    else
      EndDate := ISOToDate(ToDate);

    if not DS.FieldByName('recurrence_count').IsNull then
      RecCount := DS.FieldByName('recurrence_count').AsInteger
    else
      RecCount := 0;  // unlimited

    StartDate := ISOToDate(DS.FieldByName('start_date').AsString);
  finally
    DS.Free;
  end;

  RangeStart := ISOToDate(FromDate);
  RangeEnd := ISOToDate(ToDate);
  if EndDate < RangeEnd then
    RangeEnd := EndDate;

  Count := 0;

  if not IsRecurring then
  begin
    // Single event - just add if in range
    if (StartDate >= RangeStart) and (StartDate <= RangeEnd) then
    begin
      DateStr := FormatDateTime('yyyy-mm-dd', StartDate);
      Conn.ExecuteNonQuery(Format(
        'INSERT OR IGNORE INTO event_occurrences ' +
        '(event_id, occurrence_date, start_time, end_time, title, location, calendar_id) ' +
        'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'')',
        [EventId, DateStr, StartTime, EndTime, Title, Location, CalendarId]));
    end;
    Exit;
  end;

  // Generate recurring occurrences
  CurrentDate := StartDate;
  while CurrentDate <= RangeEnd do
  begin
    if (RecCount > 0) and (Count >= RecCount) then
      Break;

    if CurrentDate >= RangeStart then
    begin
      // Check if this day matches the recurrence pattern
      if (RecType = 'daily') then
      begin
        DateStr := FormatDateTime('yyyy-mm-dd', CurrentDate);
        // Check for exceptions
        ExDS := Conn.ExecuteQuery(Format(
          'SELECT * FROM event_exceptions WHERE event_id = ''%s'' AND exception_date = ''%s''',
          [EventId, DateStr]));
        try
          if not ExDS.IsEmpty then
          begin
            ExType := ExDS.FieldByName('exception_type').AsString;
            if ExType = 'cancelled' then
            begin
              Conn.ExecuteNonQuery(Format(
                'INSERT OR IGNORE INTO event_occurrences ' +
                '(event_id, occurrence_date, start_time, end_time, title, location, is_exception, is_cancelled, calendar_id) ' +
                'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', 1, 1, ''%s'')',
                [EventId, DateStr, StartTime, EndTime, Title, Location, CalendarId]));
            end
            else if ExType = 'modified' then
            begin
              NewTitle := ExDS.FieldByName('new_title').AsString;
              if NewTitle = '' then NewTitle := Title;
              NewStartTime := ExDS.FieldByName('new_start_time').AsString;
              if NewStartTime = '' then NewStartTime := StartTime;
              NewEndTime := ExDS.FieldByName('new_end_time').AsString;
              if NewEndTime = '' then NewEndTime := EndTime;
              NewLocation := ExDS.FieldByName('new_location').AsString;
              if NewLocation = '' then NewLocation := Location;
              Conn.ExecuteNonQuery(Format(
                'INSERT OR IGNORE INTO event_occurrences ' +
                '(event_id, occurrence_date, start_time, end_time, title, location, is_exception, calendar_id) ' +
                'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', 1, ''%s'')',
                [EventId, DateStr, NewStartTime, NewEndTime, NewTitle, NewLocation, CalendarId]));
            end;
          end
          else
          begin
            Conn.ExecuteNonQuery(Format(
              'INSERT OR IGNORE INTO event_occurrences ' +
              '(event_id, occurrence_date, start_time, end_time, title, location, calendar_id) ' +
              'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'')',
              [EventId, DateStr, StartTime, EndTime, Title, Location, CalendarId]));
          end;
        finally
          ExDS.Free;
        end;
        Inc(Count);
      end
      else if (RecType = 'weekly') then
      begin
        if ContainsDay(RecDays, DayOfWeekISO(CurrentDate)) then
        begin
          DateStr := FormatDateTime('yyyy-mm-dd', CurrentDate);
          // Check exceptions
          ExDS := Conn.ExecuteQuery(Format(
            'SELECT * FROM event_exceptions WHERE event_id = ''%s'' AND exception_date = ''%s''',
            [EventId, DateStr]));
          try
            if not ExDS.IsEmpty then
            begin
              ExType := ExDS.FieldByName('exception_type').AsString;
              if ExType = 'cancelled' then
              begin
                Conn.ExecuteNonQuery(Format(
                  'INSERT OR IGNORE INTO event_occurrences ' +
                  '(event_id, occurrence_date, start_time, end_time, title, location, is_exception, is_cancelled, calendar_id) ' +
                  'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', 1, 1, ''%s'')',
                  [EventId, DateStr, StartTime, EndTime, Title, Location, CalendarId]));
              end
              else if ExType = 'modified' then
              begin
                NewTitle := ExDS.FieldByName('new_title').AsString;
                if NewTitle = '' then NewTitle := Title;
                NewStartTime := ExDS.FieldByName('new_start_time').AsString;
                if NewStartTime = '' then NewStartTime := StartTime;
                NewEndTime := ExDS.FieldByName('new_end_time').AsString;
                if NewEndTime = '' then NewEndTime := EndTime;
                NewLocation := ExDS.FieldByName('new_location').AsString;
                if NewLocation = '' then NewLocation := Location;
                Conn.ExecuteNonQuery(Format(
                  'INSERT OR IGNORE INTO event_occurrences ' +
                  '(event_id, occurrence_date, start_time, end_time, title, location, is_exception, calendar_id) ' +
                  'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', 1, ''%s'')',
                  [EventId, DateStr, NewStartTime, NewEndTime, NewTitle, NewLocation, CalendarId]));
              end;
            end
            else
            begin
              Conn.ExecuteNonQuery(Format(
                'INSERT OR IGNORE INTO event_occurrences ' +
                '(event_id, occurrence_date, start_time, end_time, title, location, calendar_id) ' +
                'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'')',
                [EventId, DateStr, StartTime, EndTime, Title, Location, CalendarId]));
            end;
          finally
            ExDS.Free;
          end;
          Inc(Count);
        end;
      end
      else if (RecType = 'monthly') then
      begin
        if RecDayOfMonth = StrToInt(FormatDateTime('d', CurrentDate)) then
        begin
          DateStr := FormatDateTime('yyyy-mm-dd', CurrentDate);
          Conn.ExecuteNonQuery(Format(
            'INSERT OR IGNORE INTO event_occurrences ' +
            '(event_id, occurrence_date, start_time, end_time, title, location, calendar_id) ' +
            'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'')',
            [EventId, DateStr, StartTime, EndTime, Title, Location, CalendarId]));
          Inc(Count);
        end;
      end;
    end;

    // Advance to next day
    CurrentDate := CurrentDate + 1;
  end;
end;

{ Finds overlapping non-cancelled occurrences on a given date within the same calendar and inserts them into the event_conflicts table. }
procedure DetectConflicts(const ForDate: string);
var
  DS: TDataSet;
begin
  // Find overlapping events on the same date (not cancelled)
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO event_conflicts (event_id_1, event_id_2, conflict_date, overlap_start, overlap_end) ' +
    'SELECT a.event_id, b.event_id, a.occurrence_date, ' +
    '  CASE WHEN a.start_time > b.start_time THEN a.start_time ELSE b.start_time END, ' +
    '  CASE WHEN a.end_time < b.end_time THEN a.end_time ELSE b.end_time END ' +
    'FROM event_occurrences a ' +
    'JOIN event_occurrences b ON a.occurrence_date = b.occurrence_date ' +
    '  AND a.event_id < b.event_id ' +
    '  AND a.start_time < b.end_time ' +
    '  AND a.end_time > b.start_time ' +
    '  AND a.calendar_id = b.calendar_id ' +
    'WHERE a.occurrence_date = ''%s'' ' +
    '  AND a.is_cancelled = 0 AND b.is_cancelled = 0 ' +
    '  AND NOT EXISTS (SELECT 1 FROM event_conflicts ec ' +
    '    WHERE ec.event_id_1 = a.event_id AND ec.event_id_2 = b.event_id ' +
    '    AND ec.conflict_date = a.occurrence_date)',
    [ForDate]));
end;

// ============================================================
// Demo Sections
// ============================================================

{ Creates sample calendar events including single, daily, weekly, and monthly recurring events across work and personal calendars, then lists them all. }
procedure Demo1_CreateEvents;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Create Calendar Events ===');
  WriteLn;

  // Single event
  CreateEvent('evt-single-001', 'Project Kickoff', 'Initial meeting',
    'Conference Room A', '2026-02-02', '09:00', '10:30', 90,
    False, '', 0, '', 0, '', 0, 'work');
  WriteLn('   Created: Project Kickoff (single, 2026-02-02 09:00-10:30)');

  // Daily standup (weekdays, Mon-Fri = 1-5)
  CreateEvent('evt-daily-001', 'Daily Standup', 'Team sync',
    'Huddle Room', '2026-02-02', '09:30', '09:45', 15,
    True, 'daily', 1, '', 0, '2026-02-28', 0, 'work');
  WriteLn('   Created: Daily Standup (daily, 2026-02-02 to 2026-02-28)');

  // Weekly team meeting (every Monday and Thursday)
  CreateEvent('evt-weekly-001', 'Team Meeting', 'Sprint review',
    'Main Hall', '2026-02-02', '14:00', '15:00', 60,
    True, 'weekly', 1, '1,4', 0, '2026-03-31', 0, 'work');
  WriteLn('   Created: Team Meeting (weekly Mon+Thu, 14:00-15:00)');

  // Monthly report (15th of each month)
  CreateEvent('evt-monthly-001', 'Monthly Report', 'Financial review',
    'Board Room', '2026-02-15', '10:00', '11:00', 60,
    True, 'monthly', 1, '', 15, '2026-06-30', 0, 'work');
  WriteLn('   Created: Monthly Report (monthly on 15th, 10:00-11:00)');

  // Gym session (weekly Tue+Thu+Sat = 2,4,6)
  CreateEvent('evt-weekly-002', 'Gym Session', 'Workout',
    'City Gym', '2026-02-03', '07:00', '08:00', 60,
    True, 'weekly', 1, '2,4,6', 0, '2026-03-31', 0, 'personal');
  WriteLn('   Created: Gym Session (weekly Tue+Thu+Sat, 07:00-08:00, personal)');

  // Limited recurrence (5 occurrences)
  CreateEvent('evt-limited-001', 'Training Course', 'Python basics',
    'Lab B', '2026-02-03', '13:00', '15:00', 120,
    True, 'weekly', 1, '1', 0, '', 5, 'work');
  WriteLn('   Created: Training Course (weekly Mon, max 5 occurrences)');

  WriteLn;
  WriteLn('   All events:');
  DS := Conn.ExecuteQuery(
    'SELECT event_id, title, start_date, start_time, end_time, ' +
    'is_recurring, recurrence_type, calendar_id FROM events ORDER BY start_date');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-18s %s %s-%s  %s  cal:%s',
        [DS.FieldByName('event_id').AsString,
         DS.FieldByName('title').AsString,
         DS.FieldByName('start_date').AsString,
         DS.FieldByName('start_time').AsString,
         DS.FieldByName('end_time').AsString,
         BoolToStr(DS.FieldByName('is_recurring').AsInteger = 1, 'recurring', 'single'),
         DS.FieldByName('calendar_id').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Generates all event occurrences for February 2026 and displays the total count along with the first 15 chronological occurrences. }
procedure Demo2_GenerateOccurrences;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Generate Occurrences (Feb 2026) ===');
  WriteLn;

  // Generate occurrences for February 2026
  GenerateOccurrences('evt-single-001', '2026-02-01', '2026-02-28');
  GenerateOccurrences('evt-daily-001', '2026-02-01', '2026-02-28');
  GenerateOccurrences('evt-weekly-001', '2026-02-01', '2026-02-28');
  GenerateOccurrences('evt-monthly-001', '2026-02-01', '2026-02-28');
  GenerateOccurrences('evt-weekly-002', '2026-02-01', '2026-02-28');
  GenerateOccurrences('evt-limited-001', '2026-02-01', '2026-02-28');

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM event_occurrences WHERE is_cancelled = 0');
  try
    WriteLn(Format('   Total occurrences generated: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  // Show first 10
  DS := Conn.ExecuteQuery(
    'SELECT occurrence_date, start_time, end_time, title, event_id ' +
    'FROM event_occurrences WHERE is_cancelled = 0 ' +
    'ORDER BY occurrence_date, start_time LIMIT 15');
  try
    WriteLn;
    WriteLn('   First 15 occurrences:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %s-%s  %-20s (%s)',
        [DS.FieldByName('occurrence_date').AsString,
         DS.FieldByName('start_time').AsString,
         DS.FieldByName('end_time').AsString,
         DS.FieldByName('title').AsString,
         DS.FieldByName('event_id').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Adds cancellation and modification exceptions to recurring events, regenerates their occurrences, and displays the resulting exception entries. }
procedure Demo3_Exceptions;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Exceptions and Overrides ===');
  WriteLn;

  // Cancel Daily Standup on Feb 10
  Conn.ExecuteNonQuery(
    'INSERT INTO event_exceptions (event_id, exception_date, exception_type, reason) ' +
    'VALUES (''evt-daily-001'', ''2026-02-10'', ''cancelled'', ''Public holiday'')');
  WriteLn('   Exception: Daily Standup cancelled on 2026-02-10 (holiday)');

  // Modify Team Meeting on Feb 12 (different time and room)
  Conn.ExecuteNonQuery(
    'INSERT INTO event_exceptions (event_id, exception_date, exception_type, ' +
    'new_start_time, new_end_time, new_location, reason) ' +
    'VALUES (''evt-weekly-001'', ''2026-02-12'', ''modified'', ' +
    '''16:00'', ''17:00'', ''Video Call'', ''Room maintenance'')');
  WriteLn('   Exception: Team Meeting on 2026-02-12 moved to 16:00-17:00 (Video Call)');

  // Cancel standup on Feb 17
  Conn.ExecuteNonQuery(
    'INSERT INTO event_exceptions (event_id, exception_date, exception_type, reason) ' +
    'VALUES (''evt-daily-001'', ''2026-02-17'', ''cancelled'', ''Team offsite'')');
  WriteLn('   Exception: Daily Standup cancelled on 2026-02-17 (team offsite)');

  // Regenerate affected events
  Conn.ExecuteNonQuery('DELETE FROM event_occurrences WHERE event_id = ''evt-daily-001''');
  Conn.ExecuteNonQuery('DELETE FROM event_occurrences WHERE event_id = ''evt-weekly-001''');
  GenerateOccurrences('evt-daily-001', '2026-02-01', '2026-02-28');
  GenerateOccurrences('evt-weekly-001', '2026-02-01', '2026-02-28');

  WriteLn;
  WriteLn('   Exceptions in occurrences:');
  DS := Conn.ExecuteQuery(
    'SELECT occurrence_date, start_time, end_time, title, location, is_cancelled ' +
    'FROM event_occurrences WHERE is_exception = 1 ORDER BY occurrence_date');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('is_cancelled').AsInteger = 1 then
        WriteLn(Format('   %s  [CANCELLED] %s',
          [DS.FieldByName('occurrence_date').AsString,
           DS.FieldByName('title').AsString]))
      else
        WriteLn(Format('   %s  [MODIFIED] %s %s-%s at %s',
          [DS.FieldByName('occurrence_date').AsString,
           DS.FieldByName('title').AsString,
           DS.FieldByName('start_time').AsString,
           DS.FieldByName('end_time').AsString,
           DS.FieldByName('location').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays all scheduled events for a single day (2026-02-05) in chronological order, indicating any cancelled items. }
procedure Demo4_DayView;
var
  DS: TDataSet;
  ViewDate: string;
begin
  WriteLn('=== 4. Day View ===');
  WriteLn;

  ViewDate := '2026-02-05';  // Thursday
  WriteLn(Format('   Schedule for %s (Thursday):', [ViewDate]));
  WriteLn;

  DS := Conn.ExecuteQuery(Format(
    'SELECT start_time, end_time, title, location, event_id, is_cancelled ' +
    'FROM event_occurrences ' +
    'WHERE occurrence_date = ''%s'' ' +
    'ORDER BY start_time', [ViewDate]));
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('is_cancelled').AsInteger = 1 then
        WriteLn(Format('   %s-%s  [CANCELLED] %s',
          [DS.FieldByName('start_time').AsString,
           DS.FieldByName('end_time').AsString,
           DS.FieldByName('title').AsString]))
      else
        WriteLn(Format('   %s-%s  %-20s  %s',
          [DS.FieldByName('start_time').AsString,
           DS.FieldByName('end_time').AsString,
           DS.FieldByName('title').AsString,
           DS.FieldByName('location').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays active (non-cancelled) events for each day in the week of Feb 2-8, 2026, grouped by date. }
procedure Demo5_WeekView;
var
  DS: TDataSet;
  CurrentDate, PrevDate: string;
  I: Integer;
  WeekStart: string;
begin
  WriteLn('=== 5. Week View (Feb 2-8, 2026) ===');
  WriteLn;

  WeekStart := '2026-02-02';  // Monday
  PrevDate := '';

  for I := 0 to 6 do
  begin
    CurrentDate := FormatDateTime('yyyy-mm-dd', ISOToDate('2026-02-02') + I);

    DS := Conn.ExecuteQuery(Format(
      'SELECT start_time, end_time, title, is_cancelled ' +
      'FROM event_occurrences ' +
      'WHERE occurrence_date = ''%s'' AND is_cancelled = 0 ' +
      'ORDER BY start_time', [CurrentDate]));
    try
      if not DS.IsEmpty then
      begin
        WriteLn(Format('   %s (%s):', [CurrentDate,
          FormatDateTime('ddd', ISOToDate('2026-02-02') + I)]));
        while not DS.EOF do
        begin
          WriteLn(Format('     %s-%s  %s',
            [DS.FieldByName('start_time').AsString,
             DS.FieldByName('end_time').AsString,
             DS.FieldByName('title').AsString]));
          DS.Next;
        end;
      end;
    finally
      DS.Free;
    end;
  end;

  WriteLn;
end;

{ Displays a summary of active event counts per day for the entire month of February 2026. }
procedure Demo6_MonthView;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Month View Summary (Feb 2026) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT occurrence_date, COUNT(*) as event_count ' +
    'FROM event_occurrences ' +
    'WHERE occurrence_date >= ''2026-02-01'' AND occurrence_date <= ''2026-02-28'' ' +
    'AND is_cancelled = 0 ' +
    'GROUP BY occurrence_date ' +
    'ORDER BY occurrence_date');
  try
    WriteLn('   Date         Events');
    WriteLn('   ' + StringOfChar('-', 30));
    while not DS.EOF do
    begin
      WriteLn(Format('   %s   %d event(s)',
        [DS.FieldByName('occurrence_date').AsString,
         DS.FieldByName('event_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Creates overlapping events on 2026-02-05, runs conflict detection, and displays all detected scheduling conflicts with their overlap times. }
procedure Demo7_ConflictDetection;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Conflict Detection ===');
  WriteLn;

  // Add an event that overlaps with Daily Standup on same calendar
  CreateEvent('evt-conflict-001', 'Client Call', 'Urgent call',
    'Phone Booth', '2026-02-05', '09:30', '10:00', 30,
    False, '', 0, '', 0, '', 0, 'work');
  GenerateOccurrences('evt-conflict-001', '2026-02-01', '2026-02-28');
  WriteLn('   Added: Client Call on 2026-02-05 09:30-10:00 (overlaps Standup 09:30-09:45)');

  // Add another overlapping event
  CreateEvent('evt-conflict-002', 'Lunch Workshop', 'Team building',
    'Cafeteria', '2026-02-05', '14:30', '15:30', 60,
    False, '', 0, '', 0, '', 0, 'work');
  GenerateOccurrences('evt-conflict-002', '2026-02-01', '2026-02-28');
  WriteLn('   Added: Lunch Workshop on 2026-02-05 14:30-15:30 (overlaps Team Meeting 14:00-15:00)');

  // Detect conflicts
  DetectConflicts('2026-02-05');

  WriteLn;
  WriteLn('   Detected conflicts:');
  DS := Conn.ExecuteQuery(
    'SELECT c.event_id_1, c.event_id_2, c.conflict_date, c.overlap_start, c.overlap_end, ' +
    'e1.title as title1, e2.title as title2 ' +
    'FROM event_conflicts c ' +
    'JOIN events e1 ON e1.event_id = c.event_id_1 ' +
    'JOIN events e2 ON e2.event_id = c.event_id_2 ' +
    'ORDER BY c.conflict_date, c.overlap_start');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   CONFLICT on %s: "%s" vs "%s" (overlap %s-%s)',
        [DS.FieldByName('conflict_date').AsString,
         DS.FieldByName('title1').AsString,
         DS.FieldByName('title2').AsString,
         DS.FieldByName('overlap_start').AsString,
         DS.FieldByName('overlap_end').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays events for a specific date grouped by calendar (work/personal) and shows the total occurrence count per calendar for February 2026. }
procedure Demo8_MultiCalendar;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Multi-Calendar View ===');
  WriteLn;

  WriteLn('   Events by calendar on 2026-02-05:');

  DS := Conn.ExecuteQuery(
    'SELECT calendar_id, start_time, end_time, title, is_cancelled ' +
    'FROM event_occurrences ' +
    'WHERE occurrence_date = ''2026-02-05'' ' +
    'ORDER BY calendar_id, start_time');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('is_cancelled').AsInteger = 0 then
        WriteLn(Format('   [%s] %s-%s  %s',
          [DS.FieldByName('calendar_id').AsString,
           DS.FieldByName('start_time').AsString,
           DS.FieldByName('end_time').AsString,
           DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Events per calendar (Feb 2026):');
  DS := Conn.ExecuteQuery(
    'SELECT calendar_id, COUNT(*) as cnt FROM event_occurrences ' +
    'WHERE is_cancelled = 0 AND occurrence_date >= ''2026-02-01'' ' +
    'AND occurrence_date <= ''2026-02-28'' ' +
    'GROUP BY calendar_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d occurrences',
        [DS.FieldByName('calendar_id').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Lists all occurrences of the count-limited Training Course event and verifies the total does not exceed the configured maximum of 5. }
procedure Demo9_RecurrenceCount;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Limited Recurrence (Count-based) ===');
  WriteLn;

  WriteLn('   Training Course (max 5 occurrences, weekly Monday):');
  DS := Conn.ExecuteQuery(
    'SELECT occurrence_date, start_time, end_time, title ' +
    'FROM event_occurrences WHERE event_id = ''evt-limited-001'' ' +
    'ORDER BY occurrence_date');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %s-%s  %s',
        [DS.FieldByName('occurrence_date').AsString,
         DS.FieldByName('start_time').AsString,
         DS.FieldByName('end_time').AsString,
         DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM event_occurrences WHERE event_id = ''evt-limited-001''');
  try
    WriteLn(Format('   Total occurrences: %d (max: 5)', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries and displays aggregate statistics including total events, recurring vs single counts, active occurrences, exceptions, conflicts, the busiest day, and number of calendars. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
  TotalEvents, RecurringEvents, TotalOccurrences: Integer;
  CancelledCount, ModifiedCount, ConflictCount: Integer;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM events');
  try
    TotalEvents := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM events WHERE is_recurring = 1');
  try
    RecurringEvents := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM event_occurrences WHERE is_cancelled = 0');
  try
    TotalOccurrences := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM event_occurrences WHERE is_cancelled = 1');
  try
    CancelledCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM event_occurrences WHERE is_exception = 1 AND is_cancelled = 0');
  try
    ModifiedCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM event_conflicts');
  try
    ConflictCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total events defined: %d (%d recurring, %d single)',
    [TotalEvents, RecurringEvents, TotalEvents - RecurringEvents]));
  WriteLn(Format('   Total occurrences: %d (active)', [TotalOccurrences]));
  WriteLn(Format('   Exceptions: %d cancelled, %d modified', [CancelledCount, ModifiedCount]));
  WriteLn(Format('   Conflicts detected: %d', [ConflictCount]));

  // Busiest day
  DS := Conn.ExecuteQuery(
    'SELECT occurrence_date, COUNT(*) as cnt FROM event_occurrences ' +
    'WHERE is_cancelled = 0 GROUP BY occurrence_date ORDER BY cnt DESC LIMIT 1');
  try
    if not DS.IsEmpty then
      WriteLn(Format('   Busiest day: %s (%d events)',
        [DS.FieldByName('occurrence_date').AsString,
         DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  // Calendars
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT calendar_id) as cnt FROM events');
  try
    WriteLn(Format('   Calendars: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

// ============================================================
// Main
// ============================================================
begin
  WriteLn('Example 121: Calendar Events - Recurring Patterns, Exceptions, Views, Conflicts');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    Demo1_CreateEvents;
    Demo2_GenerateOccurrences;
    Demo3_Exceptions;
    Demo4_DayView;
    Demo5_WeekView;
    Demo6_MonthView;
    Demo7_ConflictDetection;
    Demo8_MultiCalendar;
    Demo9_RecurrenceCount;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
