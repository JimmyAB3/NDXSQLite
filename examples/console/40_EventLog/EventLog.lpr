{===============================================================================
  NDXSQLite Example 40 - Event Log Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Event logging with SQLite
  - Log levels and categories
  - Time-based partitioning simulation
  - Log rotation and archiving
  - Log analysis and statistics
  - Efficient querying with indexes

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program EventLog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

const
  LOG_DEBUG = 0;
  LOG_INFO = 1;
  LOG_WARNING = 2;
  LOG_ERROR = 3;
  LOG_CRITICAL = 4;

{ Returns the display name for a given log level constant. }
function LevelName(ALevel: Integer): string;
begin
  case ALevel of
    LOG_DEBUG: Result := 'DEBUG';
    LOG_INFO: Result := 'INFO';
    LOG_WARNING: Result := 'WARNING';
    LOG_ERROR: Result := 'ERROR';
    LOG_CRITICAL: Result := 'CRITICAL';
  else
    Result := 'UNKNOWN';
  end;
end;

{ Creates the event_log and event_log_archive tables with fields for level, category,
  source, context, and user tracking, plus indexes on timestamp, level, category, and user_id. }
procedure SetupEventLog;
begin
  // Main event log table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  timestamp TEXT DEFAULT (datetime(''now'')),' +
    '  level INTEGER NOT NULL,' +      // 0=debug, 1=info, 2=warning, 3=error, 4=critical
    '  category TEXT,' +                // e.g., "auth", "database", "api"
    '  source TEXT,' +                  // e.g., module or function name
    '  message TEXT NOT NULL,' +
    '  context TEXT,' +                 // JSON additional data
    '  user_id TEXT,' +
    '  session_id TEXT,' +
    '  ip_address TEXT' +
    ')');

  // Archive table for old logs
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_log_archive (' +
    '  id INTEGER PRIMARY KEY,' +
    '  timestamp TEXT,' +
    '  level INTEGER,' +
    '  category TEXT,' +
    '  source TEXT,' +
    '  message TEXT,' +
    '  context TEXT,' +
    '  user_id TEXT,' +
    '  session_id TEXT,' +
    '  ip_address TEXT,' +
    '  archived_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Create indexes for efficient querying
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_log_timestamp ON event_log(timestamp)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_log_level ON event_log(level)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_log_category ON event_log(category)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_log_user ON event_log(user_id)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_log_level_time ON event_log(level, timestamp)');
end;

{ Inserts a log entry into the event_log table with the specified level, category, source, message, and optional context/user fields. }
procedure LogEvent(ALevel: Integer; const ACategory, ASource, AMessage: string;
  const AContext: string = ''; const AUserId: string = '';
  const ASessionId: string = ''; const AIpAddress: string = '');
begin
  Connection.ExecuteNonQuery(
    'INSERT INTO event_log (level, category, source, message, context, user_id, session_id, ip_address) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
    [ALevel, ACategory, ASource, AMessage, AContext, AUserId, ASessionId, AIpAddress]);
end;

{ Inserts a DEBUG-level log entry with the given category, source, and message. }
procedure LogDebug(const ACategory, ASource, AMessage: string);
begin
  LogEvent(LOG_DEBUG, ACategory, ASource, AMessage);
end;

{ Inserts an INFO-level log entry with the given category, source, and message. }
procedure LogInfo(const ACategory, ASource, AMessage: string);
begin
  LogEvent(LOG_INFO, ACategory, ASource, AMessage);
end;

{ Inserts a WARNING-level log entry with the given category, source, and message. }
procedure LogWarning(const ACategory, ASource, AMessage: string);
begin
  LogEvent(LOG_WARNING, ACategory, ASource, AMessage);
end;

{ Inserts an ERROR-level log entry with the given category, source, message, and optional JSON context. }
procedure LogError(const ACategory, ASource, AMessage: string;
  const AContext: string = '');
begin
  LogEvent(LOG_ERROR, ACategory, ASource, AMessage, AContext);
end;

{ Inserts a CRITICAL-level log entry with the given category, source, message, and optional JSON context. }
procedure LogCritical(const ACategory, ASource, AMessage: string;
  const AContext: string = '');
begin
  LogEvent(LOG_CRITICAL, ACategory, ASource, AMessage, AContext);
end;

{ Populates the event_log table with 102 randomized entries across multiple categories,
  sources, and severity levels within a transaction, including user and IP context on some entries. }
procedure GenerateSampleLogs;
var
  I: Integer;
  Categories: array[0..4] of string = ('auth', 'database', 'api', 'system', 'user');
  Sources: array[0..3] of string = ('main', 'worker', 'scheduler', 'handler');
  Users: array[0..2] of string = ('user1', 'user2', 'admin');
begin
  WriteLn('   Generating sample log entries...');

  Connection.BeginTransaction;
  try
    // Generate varied log entries
    for I := 1 to 100 do
    begin
      case Random(10) of
        0..4: LogInfo(Categories[Random(5)], Sources[Random(4)],
                'Operation completed successfully #' + IntToStr(I));
        5..6: LogDebug(Categories[Random(5)], Sources[Random(4)],
                'Debug information for operation #' + IntToStr(I));
        7..8: LogWarning(Categories[Random(5)], Sources[Random(4)],
                'Warning: potential issue detected #' + IntToStr(I));
        9: LogError(Categories[Random(5)], Sources[Random(4)],
             'Error occurred during operation #' + IntToStr(I),
             '{"error_code": ' + IntToStr(Random(100)) + '}');
      end;

      // Add user context to some entries
      if Random(3) = 0 then
        Connection.ExecuteNonQuery(
          'UPDATE event_log SET user_id = ?, ip_address = ? WHERE id = last_insert_rowid()',
          [Users[Random(3)], '192.168.1.' + IntToStr(Random(255))]);
    end;

    // Add some critical errors
    LogCritical('system', 'main', 'System resource exhausted',
      '{"memory_used": "95%", "disk_free": "2GB"}');
    LogCritical('database', 'handler', 'Database connection lost',
      '{"retry_count": 3, "last_error": "timeout"}');

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;

  WriteLn('   Generated 102 log entries');
end;

{ Generates sample log entries and displays the five most recent events with their timestamp, level, category, and message. }
procedure DemoBasicLogging;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic event logging');
  WriteLn('   -------------------');

  GenerateSampleLogs;

  WriteLn('');
  WriteLn('   Recent log entries:');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, level, category, message ' +
    'FROM event_log ORDER BY id DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('timestamp').AsString, '] ',
              LevelName(DS.FieldByName('level').AsInteger):8, ' ',
              DS.FieldByName('category').AsString:10, ' ',
              DS.FieldByName('message').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays log entry counts grouped by severity level and lists the most recent errors and critical events. }
procedure DemoLogLevelFiltering;
var
  DS: TDataSet;
begin
  WriteLn('2. Filter by log level');
  WriteLn('   -------------------');

  WriteLn('   Log count by level:');
  DS := Connection.ExecuteQuery(
    'SELECT level, COUNT(*) as count FROM event_log GROUP BY level ORDER BY level');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', LevelName(DS.FieldByName('level').AsInteger):10, ': ',
              DS.FieldByName('count').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  WriteLn('   Errors and critical (level >= 3):');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, category, message FROM event_log ' +
    'WHERE level >= ? ORDER BY timestamp DESC LIMIT 5',
    [LOG_ERROR]);
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('timestamp').AsString, '] ',
              DS.FieldByName('category').AsString:10, ' ',
              DS.FieldByName('message').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays per-category event counts along with the number of error-level entries in each category. }
procedure DemoCategoryFiltering;
var
  DS: TDataSet;
begin
  WriteLn('3. Filter by category');
  WriteLn('   ------------------');

  WriteLn('   Log count by category:');
  DS := Connection.ExecuteQuery(
    'SELECT category, COUNT(*) as count, ' +
    '       SUM(CASE WHEN level >= 3 THEN 1 ELSE 0 END) as errors ' +
    'FROM event_log GROUP BY category ORDER BY count DESC');
  try
    WriteLn('   Category    Count  Errors');
    WriteLn('   --------    -----  ------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('category').AsString:10,
              DS.FieldByName('count').AsInteger:6,
              DS.FieldByName('errors').AsInteger:7);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Counts events from the last hour and groups all events by hour using strftime for a time-distribution view. }
procedure DemoTimeBasedQueries;
var
  DS: TDataSet;
begin
  WriteLn('4. Time-based queries');
  WriteLn('   ------------------');

  WriteLn('   Events in the last hour:');
  DS := Connection.ExecuteQuery(
    'SELECT COUNT(*) as count FROM event_log ' +
    'WHERE timestamp >= datetime(''now'', ''-1 hour'')');
  try
    WriteLn('   Count: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');

  WriteLn('   Events by hour (last 24h simulation):');
  DS := Connection.ExecuteQuery(
    'SELECT strftime(''%H:00'', timestamp) as hour, ' +
    '       COUNT(*) as count ' +
    'FROM event_log ' +
    'GROUP BY hour ORDER BY hour');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('hour').AsString, ' - ',
              DS.FieldByName('count').AsInteger, ' events');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Moves DEBUG and INFO entries from event_log into event_log_archive within a transaction, then reports the remaining and archived entry counts. }
procedure DemoLogRotation;
var
  DS: TDataSet;
  ArchivedCount: Integer;
begin
  WriteLn('5. Log rotation and archiving');
  WriteLn('   --------------------------');

  // Show current counts
  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM event_log');
  try
    WriteLn('   Current log entries: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM event_log_archive');
  try
    WriteLn('   Archived entries: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  // Archive old logs (simulate: archive DEBUG and INFO older than "now")
  WriteLn('');
  WriteLn('   Archiving DEBUG and INFO logs...');

  Connection.BeginTransaction;
  try
    // Move to archive
    Connection.ExecuteNonQuery(
      'INSERT INTO event_log_archive ' +
      '  (id, timestamp, level, category, source, message, context, user_id, session_id, ip_address) ' +
      'SELECT id, timestamp, level, category, source, message, context, user_id, session_id, ip_address ' +
      'FROM event_log WHERE level <= ?',
      [LOG_INFO]);

    DS := Connection.ExecuteQuery('SELECT changes() as count');
    try
      ArchivedCount := DS.Fields[0].AsInteger;
    finally
      DS.Free;
    end;

    // Delete from main table
    Connection.ExecuteNonQuery(
      'DELETE FROM event_log WHERE level <= ?',
      [LOG_INFO]);

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;

  WriteLn('   Archived ', ArchivedCount, ' entries');

  // Show new counts
  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM event_log');
  try
    WriteLn('   Remaining log entries: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM event_log_archive');
  try
    WriteLn('   Total archived: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Computes error rates per category across both active and archived logs using a CTE union, and lists the most active sources by event count. }
procedure DemoLogStatistics;
var
  DS: TDataSet;
begin
  WriteLn('6. Log statistics and analysis');
  WriteLn('   ---------------------------');

  WriteLn('   Error rate by category:');
  DS := Connection.ExecuteQuery(
    'WITH all_logs AS (' +
    '  SELECT * FROM event_log ' +
    '  UNION ALL ' +
    '  SELECT id, timestamp, level, category, source, message, context, user_id, session_id, ip_address ' +
    '  FROM event_log_archive' +
    ')' +
    'SELECT category, ' +
    '       COUNT(*) as total, ' +
    '       SUM(CASE WHEN level >= 3 THEN 1 ELSE 0 END) as errors, ' +
    '       ROUND(100.0 * SUM(CASE WHEN level >= 3 THEN 1 ELSE 0 END) / COUNT(*), 1) as error_rate ' +
    'FROM all_logs GROUP BY category ORDER BY error_rate DESC');
  try
    WriteLn('   Category    Total  Errors  Rate%');
    WriteLn('   --------    -----  ------  -----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('category').AsString:10,
              DS.FieldByName('total').AsInteger:6,
              DS.FieldByName('errors').AsInteger:7,
              DS.FieldByName('error_rate').AsFloat:7:1);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  WriteLn('   Most active sources:');
  DS := Connection.ExecuteQuery(
    'SELECT source, COUNT(*) as count FROM (' +
    '  SELECT source FROM event_log ' +
    '  UNION ALL ' +
    '  SELECT source FROM event_log_archive' +
    ') GROUP BY source ORDER BY count DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('source').AsString:15, ' - ',
              DS.FieldByName('count').AsInteger, ' events');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Searches log messages and context for a substring ('timeout') and extracts error_code values from JSON context fields. }
procedure DemoSearchLogs;
var
  DS: TDataSet;
begin
  WriteLn('7. Search logs');
  WriteLn('   -----------');

  WriteLn('   Searching for "timeout" in messages:');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, level, message FROM event_log ' +
    'WHERE message LIKE ? OR context LIKE ? ' +
    'ORDER BY timestamp DESC LIMIT 5',
    ['%timeout%', '%timeout%']);
  try
    while not DS.EOF do
    begin
      WriteLn('   [', LevelName(DS.FieldByName('level').AsInteger), '] ',
              DS.FieldByName('message').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  WriteLn('   Logs with JSON context containing error_code:');
  DS := Connection.ExecuteQuery(
    'SELECT timestamp, message, JSON_EXTRACT(context, ''$.error_code'') as error_code ' +
    'FROM event_log ' +
    'WHERE context IS NOT NULL AND context != '''' AND JSON_VALID(context) ' +
    '  AND JSON_EXTRACT(context, ''$.error_code'') IS NOT NULL ' +
    'LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn('   Error code ', DS.FieldByName('error_code').AsString, ': ',
              DS.FieldByName('message').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays per-user event counts and the number of distinct IP addresses associated with each user. }
procedure DemoUserActivityLog;
var
  DS: TDataSet;
begin
  WriteLn('8. User activity tracking');
  WriteLn('   ----------------------');

  WriteLn('   Events per user:');
  DS := Connection.ExecuteQuery(
    'SELECT COALESCE(user_id, ''(anonymous)'') as user_id, ' +
    '       COUNT(*) as event_count, ' +
    '       COUNT(DISTINCT ip_address) as ip_count ' +
    'FROM event_log ' +
    'GROUP BY user_id ORDER BY event_count DESC');
  try
    WriteLn('   User         Events  IPs');
    WriteLn('   ----         ------  ---');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('user_id').AsString:12,
              DS.FieldByName('event_count').AsInteger:6,
              DS.FieldByName('ip_count').AsInteger:5);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  Randomize;
  WriteLn('=== NDXSQLite Example 40: Event Log Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example40.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupEventLog;
      WriteLn('Event log database initialized');
      WriteLn('');

      DemoBasicLogging;
      DemoLogLevelFiltering;
      DemoCategoryFiltering;
      DemoTimeBasedQueries;
      DemoLogRotation;
      DemoLogStatistics;
      DemoSearchLogs;
      DemoUserActivityLog;

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
