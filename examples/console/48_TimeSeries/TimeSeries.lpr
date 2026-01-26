{===============================================================================
  NDXSQLite Example 48 - Time Series Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Time-series data storage (IoT sensors, metrics)
  - Efficient time-range queries
  - Downsampling and aggregation
  - Data retention policies
  - Moving averages and statistics
  - Anomaly detection patterns

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program TimeSeries;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Math,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates metrics, hourly/daily aggregation, retention policy, and alert tables with time-based indexes. }
procedure SetupTimeSeries;
begin
  // Raw metrics table (high-resolution data)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS metrics (' +
    '  id INTEGER PRIMARY KEY,' +
    '  sensor_id TEXT NOT NULL,' +
    '  metric_name TEXT NOT NULL,' +
    '  value REAL NOT NULL,' +
    '  unit TEXT,' +
    '  timestamp TEXT NOT NULL,' +
    '  tags TEXT' +                              // JSON tags for filtering
    ')');

  // Aggregated data (hourly rollups)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS metrics_hourly (' +
    '  sensor_id TEXT NOT NULL,' +
    '  metric_name TEXT NOT NULL,' +
    '  hour TEXT NOT NULL,' +                    // YYYY-MM-DD HH:00:00
    '  min_value REAL,' +
    '  max_value REAL,' +
    '  avg_value REAL,' +
    '  sum_value REAL,' +
    '  count INTEGER,' +
    '  PRIMARY KEY (sensor_id, metric_name, hour)' +
    ')');

  // Aggregated data (daily rollups)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS metrics_daily (' +
    '  sensor_id TEXT NOT NULL,' +
    '  metric_name TEXT NOT NULL,' +
    '  day TEXT NOT NULL,' +                     // YYYY-MM-DD
    '  min_value REAL,' +
    '  max_value REAL,' +
    '  avg_value REAL,' +
    '  sum_value REAL,' +
    '  count INTEGER,' +
    '  PRIMARY KEY (sensor_id, metric_name, day)' +
    ')');

  // Retention policies
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS retention_policies (' +
    '  id INTEGER PRIMARY KEY,' +
    '  table_name TEXT NOT NULL,' +
    '  retention_days INTEGER NOT NULL,' +
    '  last_cleanup TEXT' +
    ')');

  // Alerts/thresholds
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS alert_rules (' +
    '  id INTEGER PRIMARY KEY,' +
    '  sensor_id TEXT,' +                        // NULL = all sensors
    '  metric_name TEXT NOT NULL,' +
    '  condition TEXT NOT NULL,' +               // gt, lt, eq, ne
    '  threshold REAL NOT NULL,' +
    '  enabled INTEGER DEFAULT 1' +
    ')');

  // Alert events
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS alert_events (' +
    '  id INTEGER PRIMARY KEY,' +
    '  rule_id INTEGER REFERENCES alert_rules(id),' +
    '  sensor_id TEXT,' +
    '  value REAL,' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes for time-series queries
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_metrics_sensor_time ON metrics(sensor_id, timestamp)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_metrics_name_time ON metrics(metric_name, timestamp)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_metrics_time ON metrics(timestamp)');

  // Default retention policies
  Connection.ExecuteNonQuery('INSERT OR IGNORE INTO retention_policies (table_name, retention_days) VALUES (''metrics'', 7)');
  Connection.ExecuteNonQuery('INSERT OR IGNORE INTO retention_policies (table_name, retention_days) VALUES (''metrics_hourly'', 30)');
  Connection.ExecuteNonQuery('INSERT OR IGNORE INTO retention_policies (table_name, retention_days) VALUES (''metrics_daily'', 365)');
end;

// ============================================================================
// Time-series operations
// ============================================================================

{ Inserts a timestamped metric value for a sensor into the metrics table. }
procedure RecordMetric(const ASensorId, AMetricName: string; AValue: Real;
  const AUnit: string = ''; const ATags: string = '');
var
  Timestamp: string;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Connection.ExecuteNonQuery(
    'INSERT INTO metrics (sensor_id, metric_name, value, unit, timestamp, tags) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [ASensorId, AMetricName, AValue, AUnit, Timestamp, ATags]);
end;

{ Populates the metrics table with 24 hours of simulated temperature, humidity, and pressure readings for three sensors. }
procedure GenerateSampleData;
var
  I, J: Integer;
  Sensors: array[0..2] of string = ('sensor_001', 'sensor_002', 'sensor_003');
  BaseTemp, BaseHumidity, BasePressure: Real;
  Timestamp: TDateTime;
begin
  WriteLn('   Generating sample sensor data...');

  Connection.BeginTransaction;
  try
    BaseTemp := 22.0;
    BaseHumidity := 45.0;
    BasePressure := 1013.25;

    // Generate data points (simulating 24 hours of data, every 5 minutes)
    for I := 0 to 287 do  // 288 points = 24 hours at 5-min intervals
    begin
      Timestamp := IncMinute(Now - 1, I * 5);  // Start from yesterday

      for J := 0 to 2 do
      begin
        // Temperature with daily variation and noise
        Connection.ExecuteNonQuery(
          'INSERT INTO metrics (sensor_id, metric_name, value, unit, timestamp) VALUES (?, ?, ?, ?, ?)',
          [Sensors[J], 'temperature',
           BaseTemp + 5 * Sin(I * Pi / 144) + (Random - 0.5) * 2 + J,
           'celsius',
           FormatDateTime('yyyy-mm-dd hh:nn:ss', Timestamp)]);

        // Humidity with inverse correlation to temperature
        Connection.ExecuteNonQuery(
          'INSERT INTO metrics (sensor_id, metric_name, value, unit, timestamp) VALUES (?, ?, ?, ?, ?)',
          [Sensors[J], 'humidity',
           BaseHumidity - 10 * Sin(I * Pi / 144) + (Random - 0.5) * 5 + J * 2,
           'percent',
           FormatDateTime('yyyy-mm-dd hh:nn:ss', Timestamp)]);

        // Pressure with slow variation
        Connection.ExecuteNonQuery(
          'INSERT INTO metrics (sensor_id, metric_name, value, unit, timestamp) VALUES (?, ?, ?, ?, ?)',
          [Sensors[J], 'pressure',
           BasePressure + 5 * Sin(I * Pi / 288) + (Random - 0.5),
           'hPa',
           FormatDateTime('yyyy-mm-dd hh:nn:ss', Timestamp)]);
      end;
    end;

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;

  WriteLn('   Generated ', 288 * 3 * 3, ' data points');
end;

{ Generates sample sensor data and displays the latest reading for each sensor and metric. }
procedure DemoBasicQueries;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic time-series queries');
  WriteLn('   -------------------------');
  WriteLn('');

  GenerateSampleData;
  WriteLn('');

  // Latest readings
  WriteLn('   Latest readings per sensor:');
  DS := Connection.ExecuteQuery(
    'SELECT sensor_id, metric_name, value, unit, timestamp ' +
    'FROM metrics ' +
    'WHERE (sensor_id, metric_name, timestamp) IN (' +
    '  SELECT sensor_id, metric_name, MAX(timestamp) ' +
    '  FROM metrics GROUP BY sensor_id, metric_name' +
    ') ORDER BY sensor_id, metric_name');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('sensor_id').AsString:12,
              DS.FieldByName('metric_name').AsString:12,
              DS.FieldByName('value').AsFloat:8:2, ' ',
              DS.FieldByName('unit').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Queries the last hour of temperature data and displays min, max, avg, and reading count per sensor. }
procedure DemoTimeRangeQueries;
var
  DS: TDataSet;
begin
  WriteLn('2. Time-range queries');
  WriteLn('   ------------------');
  WriteLn('');

  // Last hour stats
  WriteLn('   Temperature stats (last hour):');
  DS := Connection.ExecuteQuery(
    'SELECT sensor_id, ' +
    '       MIN(value) as min_temp, ' +
    '       MAX(value) as max_temp, ' +
    '       AVG(value) as avg_temp, ' +
    '       COUNT(*) as readings ' +
    'FROM metrics ' +
    'WHERE metric_name = ''temperature'' ' +
    '  AND timestamp >= datetime(''now'', ''-1 hour'') ' +
    'GROUP BY sensor_id');
  try
    WriteLn('   Sensor        Min     Max     Avg   Readings');
    WriteLn('   ------        ---     ---     ---   --------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('sensor_id').AsString:12,
              DS.FieldByName('min_temp').AsFloat:7:2,
              DS.FieldByName('max_temp').AsFloat:8:2,
              DS.FieldByName('avg_temp').AsFloat:8:2,
              DS.FieldByName('readings').AsInteger:8);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates time series downsampling through hourly aggregation. }
procedure DemoDownsampling;
var
  DS: TDataSet;
begin
  WriteLn('3. Downsampling (aggregation)');
  WriteLn('   --------------------------');
  WriteLn('');

  // Create hourly rollup
  WriteLn('   Creating hourly aggregates...');
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO metrics_hourly ' +
    '  (sensor_id, metric_name, hour, min_value, max_value, avg_value, sum_value, count) ' +
    'SELECT sensor_id, metric_name, ' +
    '       strftime(''%Y-%m-%d %H:00:00'', timestamp) as hour, ' +
    '       MIN(value), MAX(value), AVG(value), SUM(value), COUNT(*) ' +
    'FROM metrics ' +
    'GROUP BY sensor_id, metric_name, hour');

  // Show hourly data
  DS := Connection.ExecuteQuery(
    'SELECT hour, sensor_id, avg_value, count ' +
    'FROM metrics_hourly ' +
    'WHERE metric_name = ''temperature'' AND sensor_id = ''sensor_001'' ' +
    'ORDER BY hour DESC LIMIT 6');
  try
    WriteLn('   Hourly temperature averages (sensor_001):');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('hour').AsString,
              '  avg: ', DS.FieldByName('avg_value').AsFloat:6:2,
              '  (', DS.FieldByName('count').AsInteger, ' readings)');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Create daily rollup
  WriteLn('   Creating daily aggregates...');
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO metrics_daily ' +
    '  (sensor_id, metric_name, day, min_value, max_value, avg_value, sum_value, count) ' +
    'SELECT sensor_id, metric_name, ' +
    '       date(timestamp) as day, ' +
    '       MIN(value), MAX(value), AVG(value), SUM(value), COUNT(*) ' +
    'FROM metrics ' +
    'GROUP BY sensor_id, metric_name, day');

  DS := Connection.ExecuteQuery(
    'SELECT day, min_value, max_value, avg_value ' +
    'FROM metrics_daily ' +
    'WHERE metric_name = ''temperature'' AND sensor_id = ''sensor_001'' ' +
    'ORDER BY day DESC LIMIT 3');
  try
    WriteLn('   Daily temperature summary (sensor_001):');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('day').AsString,
              '  min: ', DS.FieldByName('min_value').AsFloat:5:1,
              '  max: ', DS.FieldByName('max_value').AsFloat:5:1,
              '  avg: ', DS.FieldByName('avg_value').AsFloat:5:1);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Calculates and displays a 5-point moving average for sensor_001 temperature readings. }
procedure DemoMovingAverages;
var
  DS: TDataSet;
begin
  WriteLn('4. Moving averages');
  WriteLn('   ---------------');
  WriteLn('');

  // Calculate 5-point moving average
  DS := Connection.ExecuteQuery(
    'WITH numbered AS (' +
    '  SELECT timestamp, value, ' +
    '         ROW_NUMBER() OVER (ORDER BY timestamp) as rn ' +
    '  FROM metrics ' +
    '  WHERE sensor_id = ''sensor_001'' AND metric_name = ''temperature'' ' +
    '  ORDER BY timestamp DESC LIMIT 20' +
    ')' +
    'SELECT n1.timestamp, n1.value as current, ' +
    '       (SELECT AVG(n2.value) FROM numbered n2 ' +
    '        WHERE n2.rn BETWEEN n1.rn - 2 AND n1.rn + 2) as moving_avg_5 ' +
    'FROM numbered n1 ' +
    'ORDER BY n1.timestamp DESC LIMIT 10');
  try
    WriteLn('   Temperature with 5-point moving average:');
    WriteLn('   Timestamp             Current  Moving Avg');
    WriteLn('   ---------             -------  ----------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('timestamp').AsString,
              DS.FieldByName('current').AsFloat:9:2,
              DS.FieldByName('moving_avg_5').AsFloat:11:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Computes mean and standard deviation for sensor_001 temperature, then finds values outside 2 sigma. }
procedure DemoAnomalyDetection;
var
  DS: TDataSet;
  AvgVal, StdDev: Real;
begin
  WriteLn('5. Anomaly detection');
  WriteLn('   -----------------');
  WriteLn('');

  // Calculate mean and standard deviation
  DS := Connection.ExecuteQuery(
    'SELECT AVG(value) as avg_val, ' +
    '       SQRT(AVG(value * value) - AVG(value) * AVG(value)) as std_dev ' +
    'FROM metrics ' +
    'WHERE sensor_id = ''sensor_001'' AND metric_name = ''temperature''');
  try
    AvgVal := DS.FieldByName('avg_val').AsFloat;
    StdDev := DS.FieldByName('std_dev').AsFloat;
    WriteLn('   Temperature stats (sensor_001):');
    WriteLn('   Mean: ', AvgVal:0:2, ', StdDev: ', StdDev:0:2);
    WriteLn('   Anomaly threshold: > ', (AvgVal + 2 * StdDev):0:2, ' or < ', (AvgVal - 2 * StdDev):0:2);
  finally
    DS.Free;
  end;

  WriteLn('');

  // Find anomalies (values outside 2 standard deviations)
  DS := Connection.ExecuteQuery(
    'WITH stats AS (' +
    '  SELECT AVG(value) as avg_val, ' +
    '         SQRT(AVG(value * value) - AVG(value) * AVG(value)) as std_dev ' +
    '  FROM metrics ' +
    '  WHERE sensor_id = ''sensor_001'' AND metric_name = ''temperature''' +
    ')' +
    'SELECT m.timestamp, m.value, ' +
    '       CASE WHEN m.value > s.avg_val + 2 * s.std_dev THEN ''HIGH'' ' +
    '            WHEN m.value < s.avg_val - 2 * s.std_dev THEN ''LOW'' ' +
    '       END as anomaly_type ' +
    'FROM metrics m, stats s ' +
    'WHERE m.sensor_id = ''sensor_001'' AND m.metric_name = ''temperature'' ' +
    '  AND (m.value > s.avg_val + 2 * s.std_dev OR m.value < s.avg_val - 2 * s.std_dev) ' +
    'ORDER BY m.timestamp DESC LIMIT 5');
  try
    WriteLn('   Detected anomalies:');
    if DS.EOF then
      WriteLn('   (No anomalies found - data is within normal range)')
    else
      while not DS.EOF do
      begin
        WriteLn('   ', DS.FieldByName('timestamp').AsString,
                ': ', DS.FieldByName('value').AsFloat:6:2,
                ' (', DS.FieldByName('anomaly_type').AsString, ')');
        DS.Next;
      end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays configured retention policies and shows the cleanup SQL that would purge old data. }
procedure DemoRetentionPolicy;
var
  DS: TDataSet;
begin
  WriteLn('6. Retention policies');
  WriteLn('   ------------------');
  WriteLn('');

  DS := Connection.ExecuteQuery('SELECT table_name, retention_days FROM retention_policies');
  try
    WriteLn('   Current retention policies:');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('table_name').AsString:20,
              ': ', DS.FieldByName('retention_days').AsInteger, ' days');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Simulate cleanup
  WriteLn('   Running cleanup (would delete data older than policy):');
  WriteLn('   DELETE FROM metrics WHERE timestamp < datetime(''now'', ''-7 days'')');
  WriteLn('   DELETE FROM metrics_hourly WHERE hour < datetime(''now'', ''-30 days'')');
  WriteLn('   DELETE FROM metrics_daily WHERE day < date(''now'', ''-365 days'')');

  WriteLn('');
end;

{ Compares temperature statistics (avg, min, max, range) across all sensors. }
procedure DemoSensorComparison;
var
  DS: TDataSet;
begin
  WriteLn('7. Sensor comparison');
  WriteLn('   -----------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT sensor_id, ' +
    '       AVG(value) as avg_temp, ' +
    '       MIN(value) as min_temp, ' +
    '       MAX(value) as max_temp, ' +
    '       MAX(value) - MIN(value) as temp_range ' +
    'FROM metrics ' +
    'WHERE metric_name = ''temperature'' ' +
    'GROUP BY sensor_id ' +
    'ORDER BY avg_temp DESC');
  try
    WriteLn('   Temperature comparison across sensors:');
    WriteLn('   Sensor        Avg     Min     Max   Range');
    WriteLn('   ------        ---     ---     ---   -----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('sensor_id').AsString:12,
              DS.FieldByName('avg_temp').AsFloat:7:2,
              DS.FieldByName('min_temp').AsFloat:8:2,
              DS.FieldByName('max_temp').AsFloat:8:2,
              DS.FieldByName('temp_range').AsFloat:7:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the database file and its WAL/SHM journal files from disk. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  Randomize;
  WriteLn('=== NDXSQLite Example 48: Time Series Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example48.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTimeSeries;
      WriteLn('Time series database initialized');
      WriteLn('');

      DemoBasicQueries;
      DemoTimeRangeQueries;
      DemoDownsampling;
      DemoMovingAverages;
      DemoAnomalyDetection;
      DemoRetentionPolicy;
      DemoSensorComparison;

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
