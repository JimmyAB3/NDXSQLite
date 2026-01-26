{===============================================================================
  NDXSQLite Example 125 - Anomaly Detection
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Z-score based anomaly detection
  - Moving average deviation analysis
  - Threshold-based alerting rules
  - Trend detection and analysis
  - Multi-metric severity classification

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program AnomalyDetection;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Time-series metrics
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS metrics (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  metric_name TEXT NOT NULL,' +
    '  metric_value REAL NOT NULL,' +
    '  recorded_at TEXT NOT NULL,' +
    '  source TEXT DEFAULT ''system''' +
    ')'
  );

  // Anomaly detection rules
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS anomaly_rules (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  metric_name TEXT NOT NULL,' +
    '  rule_type TEXT NOT NULL,' +       // zscore, threshold, moving_avg
    '  threshold_upper REAL,' +
    '  threshold_lower REAL,' +
    '  zscore_limit REAL DEFAULT 2.0,' +
    '  window_size INTEGER DEFAULT 5,' +
    '  severity TEXT DEFAULT ''medium'',' +
    '  enabled INTEGER DEFAULT 1' +
    ')'
  );

  // Detected anomalies
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS anomalies (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  metric_name TEXT NOT NULL,' +
    '  metric_value REAL NOT NULL,' +
    '  detection_method TEXT NOT NULL,' +  // zscore, threshold, moving_avg, trend
    '  expected_value REAL,' +
    '  deviation REAL,' +
    '  zscore REAL,' +
    '  severity TEXT DEFAULT ''medium'',' + // low, medium, high, critical
    '  acknowledged INTEGER DEFAULT 0,' +
    '  recorded_at TEXT NOT NULL,' +
    '  detected_at TEXT DEFAULT (datetime(''now''))' +
    ')'
  );

  // Moving averages storage
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS moving_averages (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  metric_name TEXT NOT NULL,' +
    '  window_size INTEGER NOT NULL,' +
    '  avg_value REAL NOT NULL,' +
    '  stddev_value REAL,' +
    '  period_end TEXT NOT NULL' +
    ')'
  );

  // Trend analysis
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS trends (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  metric_name TEXT NOT NULL,' +
    '  trend_type TEXT NOT NULL,' +       // increasing, decreasing, stable, spike, drop
    '  start_time TEXT NOT NULL,' +
    '  end_time TEXT NOT NULL,' +
    '  start_value REAL,' +
    '  end_value REAL,' +
    '  change_pct REAL,' +
    '  data_points INTEGER' +
    ')'
  );
end;

{ Inserts sample data for demonstrations. }
procedure InsertSampleData;
begin
  // Response time metrics (ms) - normal range ~50-150ms with some anomalies
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 52.3, ''2024-01-01 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 48.7, ''2024-01-01 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 55.1, ''2024-01-01 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 61.2, ''2024-01-01 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 58.9, ''2024-01-01 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 245.8, ''2024-01-01 13:00:00'')');  // SPIKE anomaly
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 62.4, ''2024-01-01 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 57.3, ''2024-01-01 15:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 53.8, ''2024-01-01 16:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 350.2, ''2024-01-01 17:00:00'')');  // SPIKE anomaly
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 49.5, ''2024-01-01 18:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 54.6, ''2024-01-01 19:00:00'')');
  // Gradual increase (trend)
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 70.1, ''2024-01-02 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 85.3, ''2024-01-02 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 98.7, ''2024-01-02 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 112.4, ''2024-01-02 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 135.6, ''2024-01-02 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''response_time'', 148.9, ''2024-01-02 13:00:00'')');

  // CPU usage (%) - normal range ~20-60% with anomalies
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 25.4, ''2024-01-01 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 32.1, ''2024-01-01 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 28.7, ''2024-01-01 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 45.3, ''2024-01-01 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 38.9, ''2024-01-01 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 92.7, ''2024-01-01 13:00:00'')');  // SPIKE
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 41.2, ''2024-01-01 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 35.8, ''2024-01-01 15:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 97.5, ''2024-01-01 16:00:00'')');  // SPIKE
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 30.4, ''2024-01-01 17:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 27.6, ''2024-01-01 18:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''cpu_usage'', 22.3, ''2024-01-01 19:00:00'')');

  // Request count - normal ~100-500, with a drop anomaly
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 320, ''2024-01-01 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 385, ''2024-01-01 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 412, ''2024-01-01 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 398, ''2024-01-01 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 445, ''2024-01-01 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 42, ''2024-01-01 13:00:00'')');   // DROP anomaly
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 380, ''2024-01-01 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 356, ''2024-01-01 15:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 290, ''2024-01-01 16:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 15, ''2024-01-01 17:00:00'')');   // DROP anomaly
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 335, ''2024-01-01 18:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, metric_value, recorded_at) VALUES (''request_count'', 310, ''2024-01-01 19:00:00'')');

  // Setup detection rules
  Conn.ExecuteNonQuery('INSERT INTO anomaly_rules (metric_name, rule_type, zscore_limit, severity) VALUES (''response_time'', ''zscore'', 2.0, ''high'')');
  Conn.ExecuteNonQuery('INSERT INTO anomaly_rules (metric_name, rule_type, threshold_upper, threshold_lower, severity) VALUES (''response_time'', ''threshold'', 200.0, 10.0, ''critical'')');
  Conn.ExecuteNonQuery('INSERT INTO anomaly_rules (metric_name, rule_type, window_size, severity) VALUES (''response_time'', ''moving_avg'', 5, ''medium'')');
  Conn.ExecuteNonQuery('INSERT INTO anomaly_rules (metric_name, rule_type, zscore_limit, severity) VALUES (''cpu_usage'', ''zscore'', 2.0, ''high'')');
  Conn.ExecuteNonQuery('INSERT INTO anomaly_rules (metric_name, rule_type, threshold_upper, severity) VALUES (''cpu_usage'', ''threshold'', 90.0, ''critical'')');
  Conn.ExecuteNonQuery('INSERT INTO anomaly_rules (metric_name, rule_type, zscore_limit, severity) VALUES (''request_count'', ''zscore'', 2.0, ''high'')');
  Conn.ExecuteNonQuery('INSERT INTO anomaly_rules (metric_name, rule_type, threshold_lower, severity) VALUES (''request_count'', ''threshold'', 50.0, ''critical'')');
end;

// ============================================================
// Demo Sections
// ============================================================

{ Displays summary statistics (count, min, max, avg) for each metric and shows the response time series with a visual bar chart. }
procedure Demo1_TimeSeriesData;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Time Series Data ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT metric_name, COUNT(*) as cnt, ROUND(MIN(metric_value), 1) as min_val, ' +
    'ROUND(MAX(metric_value), 1) as max_val, ROUND(AVG(metric_value), 1) as avg_val ' +
    'FROM metrics GROUP BY metric_name ORDER BY metric_name');
  try
    WriteLn(Format('   %-15s %-6s %-8s %-8s %s', ['Metric', 'Count', 'Min', 'Max', 'Avg']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %-6d %-8s %-8s %s',
        [DS.FieldByName('metric_name').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('min_val').AsString,
         DS.FieldByName('max_val').AsString,
         DS.FieldByName('avg_val').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Response time series (first 12 hours):');
  DS := Conn.ExecuteQuery(
    'SELECT recorded_at, metric_value FROM metrics ' +
    'WHERE metric_name = ''response_time'' AND recorded_at LIKE ''2024-01-01%'' ' +
    'ORDER BY recorded_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %6.1f ms  %s',
        [DS.FieldByName('recorded_at').AsString,
         DS.FieldByName('metric_value').AsFloat,
         StringOfChar('|', Round(DS.FieldByName('metric_value').AsFloat / 10))]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Calculates mean and standard deviation for each metric, identifies values with z-score magnitude greater than 2, and records them as anomalies. }
procedure Demo2_ZScoreDetection;
var
  DS, DS2: TDataSet;
  Mean, StdDev, ZScore, Val: Double;
  MetricName, RecordedAt: string;
  Severity: string;
  AnomalyCount: Integer;
begin
  WriteLn('=== 2. Z-Score Detection ===');
  WriteLn;

  AnomalyCount := 0;

  // For each metric, calculate z-scores
  DS := Conn.ExecuteQuery('SELECT DISTINCT metric_name FROM metrics ORDER BY metric_name');
  try
    while not DS.EOF do
    begin
      MetricName := DS.FieldByName('metric_name').AsString;

      // Get mean and stddev
      DS2 := Conn.ExecuteQuery(Format(
        'SELECT AVG(metric_value) as mean_val, ' +
        'SQRT(AVG((metric_value - (SELECT AVG(metric_value) FROM metrics WHERE metric_name = ''%s'')) * ' +
        '(metric_value - (SELECT AVG(metric_value) FROM metrics WHERE metric_name = ''%s'')))) as std_val ' +
        'FROM metrics WHERE metric_name = ''%s''',
        [MetricName, MetricName, MetricName]));
      try
        Mean := DS2.FieldByName('mean_val').AsFloat;
        StdDev := DS2.FieldByName('std_val').AsFloat;
      finally
        DS2.Free;
      end;

      WriteLn(Format('   %s: mean=%.1f, stddev=%.1f', [MetricName, Mean, StdDev]));

      if StdDev > 0 then
      begin
        // Find values with |z-score| > 2
        DS2 := Conn.ExecuteQuery(Format(
          'SELECT metric_value, recorded_at FROM metrics ' +
          'WHERE metric_name = ''%s'' ORDER BY recorded_at',
          [MetricName]));
        try
          while not DS2.EOF do
          begin
            Val := DS2.FieldByName('metric_value').AsFloat;
            RecordedAt := DS2.FieldByName('recorded_at').AsString;
            ZScore := (Val - Mean) / StdDev;

            if Abs(ZScore) > 2.0 then
            begin
              if Abs(ZScore) > 3.0 then
                Severity := 'critical'
              else
                Severity := 'high';

              WriteLn(Format('   -> ANOMALY: value=%.1f z-score=%.2f at %s [%s]',
                [Val, ZScore, RecordedAt, Severity]));

              Conn.ExecuteNonQuery(Format(
                'INSERT INTO anomalies (metric_name, metric_value, detection_method, ' +
                'expected_value, deviation, zscore, severity, recorded_at) VALUES ' +
                '(''%s'', %.2f, ''zscore'', %.2f, %.2f, %.2f, ''%s'', ''%s'')',
                [MetricName, Val, Mean, Val - Mean, ZScore, Severity, RecordedAt]));

              Inc(AnomalyCount);
            end;
            DS2.Next;
          end;
        finally
          DS2.Free;
        end;
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total z-score anomalies detected: %d', [AnomalyCount]));
  WriteLn;
end;

{ Computes a rolling 5-point moving average for response time, stores each window's average and standard deviation, and flags values deviating more than 2 standard deviations. }
procedure Demo3_MovingAverage;
var
  DS: TDataSet;
  MetricName: string;
  Values: array[0..49] of Double;
  Times: array[0..49] of string;
  Count, I, J, WindowSize: Integer;
  WindowAvg, WindowStd, Diff, SumVal, SumSq: Double;
begin
  WriteLn('=== 3. Moving Average Deviation ===');
  WriteLn;

  WindowSize := 5;
  WriteLn(Format('   Window size: %d', [WindowSize]));
  WriteLn;

  // Process response_time
  MetricName := 'response_time';
  DS := Conn.ExecuteQuery(Format(
    'SELECT metric_value, recorded_at FROM metrics ' +
    'WHERE metric_name = ''%s'' AND recorded_at LIKE ''2024-01-01%%'' ORDER BY recorded_at',
    [MetricName]));
  try
    Count := 0;
    while not DS.EOF do
    begin
      Values[Count] := DS.FieldByName('metric_value').AsFloat;
      Times[Count] := DS.FieldByName('recorded_at').AsString;
      Inc(Count);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   %s moving averages:', [MetricName]));
  for I := WindowSize - 1 to Count - 1 do
  begin
    // Calculate window average
    SumVal := 0;
    for J := I - WindowSize + 1 to I do
      SumVal := SumVal + Values[J];
    WindowAvg := SumVal / WindowSize;

    // Calculate window stddev
    SumSq := 0;
    for J := I - WindowSize + 1 to I do
      SumSq := SumSq + (Values[J] - WindowAvg) * (Values[J] - WindowAvg);
    WindowStd := Sqrt(SumSq / WindowSize);

    Diff := Values[I] - WindowAvg;

    // Store moving average
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO moving_averages (metric_name, window_size, avg_value, stddev_value, period_end) ' +
      'VALUES (''%s'', %d, %.2f, %.2f, ''%s'')',
      [MetricName, WindowSize, WindowAvg, WindowStd, Times[I]]));

    if Abs(Diff) > WindowStd * 2 then
      WriteLn(Format('   %s  val:%6.1f  mavg:%6.1f  dev:%7.1f  ** ANOMALY **',
        [Times[I], Values[I], WindowAvg, Diff]))
    else
      WriteLn(Format('   %s  val:%6.1f  mavg:%6.1f  dev:%7.1f',
        [Times[I], Values[I], WindowAvg, Diff]));
  end;

  WriteLn;
end;

{ Displays active threshold rules, then checks each metric against its upper/lower thresholds and logs critical violations as anomalies. }
procedure Demo4_ThresholdAlerts;
var
  DS: TDataSet;
  AlertCount: Integer;
  Val: Double;
  MetricName, RecordedAt: string;
begin
  WriteLn('=== 4. Threshold Alerts ===');
  WriteLn;

  WriteLn('   Active rules:');
  DS := Conn.ExecuteQuery(
    'SELECT metric_name, rule_type, threshold_upper, threshold_lower, severity ' +
    'FROM anomaly_rules WHERE rule_type = ''threshold'' AND enabled = 1 ' +
    'ORDER BY metric_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s upper: %-8s lower: %-8s [%s]',
        [DS.FieldByName('metric_name').AsString,
         VarToStr(DS.FieldByName('threshold_upper').Value),
         VarToStr(DS.FieldByName('threshold_lower').Value),
         DS.FieldByName('severity').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Threshold violations:');
  AlertCount := 0;

  // Check response_time upper threshold (200ms)
  DS := Conn.ExecuteQuery(
    'SELECT metric_name, metric_value, recorded_at FROM metrics ' +
    'WHERE metric_name = ''response_time'' AND metric_value > 200.0 ' +
    'ORDER BY recorded_at');
  try
    while not DS.EOF do
    begin
      Inc(AlertCount);
      MetricName := DS.FieldByName('metric_name').AsString;
      Val := DS.FieldByName('metric_value').AsFloat;
      RecordedAt := DS.FieldByName('recorded_at').AsString;
      WriteLn(Format('   [CRITICAL] %s = %.1f (> 200.0) at %s',
        [MetricName, Val, RecordedAt]));

      Conn.ExecuteNonQuery(Format(
        'INSERT INTO anomalies (metric_name, metric_value, detection_method, ' +
        'expected_value, deviation, severity, recorded_at) VALUES ' +
        '(''%s'', %.2f, ''threshold'', 200.0, %.2f, ''critical'', ''%s'')',
        [MetricName, Val, Val - 200.0, RecordedAt]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Check cpu_usage upper threshold (90%)
  DS := Conn.ExecuteQuery(
    'SELECT metric_name, metric_value, recorded_at FROM metrics ' +
    'WHERE metric_name = ''cpu_usage'' AND metric_value > 90.0 ' +
    'ORDER BY recorded_at');
  try
    while not DS.EOF do
    begin
      Inc(AlertCount);
      MetricName := DS.FieldByName('metric_name').AsString;
      Val := DS.FieldByName('metric_value').AsFloat;
      RecordedAt := DS.FieldByName('recorded_at').AsString;
      WriteLn(Format('   [CRITICAL] %s = %.1f%% (> 90.0%%) at %s',
        [MetricName, Val, RecordedAt]));

      Conn.ExecuteNonQuery(Format(
        'INSERT INTO anomalies (metric_name, metric_value, detection_method, ' +
        'expected_value, deviation, severity, recorded_at) VALUES ' +
        '(''%s'', %.2f, ''threshold'', 90.0, %.2f, ''critical'', ''%s'')',
        [MetricName, Val, Val - 90.0, RecordedAt]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Check request_count lower threshold (50)
  DS := Conn.ExecuteQuery(
    'SELECT metric_name, metric_value, recorded_at FROM metrics ' +
    'WHERE metric_name = ''request_count'' AND metric_value < 50.0 ' +
    'ORDER BY recorded_at');
  try
    while not DS.EOF do
    begin
      Inc(AlertCount);
      MetricName := DS.FieldByName('metric_name').AsString;
      Val := DS.FieldByName('metric_value').AsFloat;
      RecordedAt := DS.FieldByName('recorded_at').AsString;
      WriteLn(Format('   [CRITICAL] %s = %.0f (< 50) at %s',
        [MetricName, Val, RecordedAt]));

      Conn.ExecuteNonQuery(Format(
        'INSERT INTO anomalies (metric_name, metric_value, detection_method, ' +
        'expected_value, deviation, severity, recorded_at) VALUES ' +
        '(''%s'', %.2f, ''threshold'', 50.0, %.2f, ''critical'', ''%s'')',
        [MetricName, Val, Val - 50.0, RecordedAt]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total threshold alerts: %d', [AlertCount]));
  WriteLn;
end;

{ Scans response time data for consecutive increasing or decreasing sequences of 3+ points, and detects spikes/drops where value changes by more than 2x. }
procedure Demo5_TrendDetection;
var
  DS: TDataSet;
  Values: array[0..49] of Double;
  Times: array[0..49] of string;
  Count, I, ConsecInc, ConsecDec, StartIdx: Integer;
  ChangePct: Double;
  TrendType: string;
begin
  WriteLn('=== 5. Trend Detection ===');
  WriteLn;

  // Analyze response_time for trends (using Jan 2 data which has an upward trend)
  DS := Conn.ExecuteQuery(
    'SELECT metric_value, recorded_at FROM metrics ' +
    'WHERE metric_name = ''response_time'' ORDER BY recorded_at');
  try
    Count := 0;
    while not DS.EOF do
    begin
      Values[Count] := DS.FieldByName('metric_value').AsFloat;
      Times[Count] := DS.FieldByName('recorded_at').AsString;
      Inc(Count);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Detecting consecutive trends (3+ points):');
  WriteLn;

  // Find consecutive increasing/decreasing sequences
  I := 1;
  while I < Count do
  begin
    // Check for increasing trend
    ConsecInc := 0;
    StartIdx := I - 1;
    while (I < Count) and (Values[I] > Values[I-1]) do
    begin
      Inc(ConsecInc);
      Inc(I);
    end;
    if ConsecInc >= 3 then
    begin
      ChangePct := (Values[I-1] - Values[StartIdx]) / Values[StartIdx] * 100.0;
      TrendType := 'increasing';
      WriteLn(Format('   TREND: %s from %.1f to %.1f (%d points, %.1f%%)',
        [TrendType, Values[StartIdx], Values[I-1], ConsecInc + 1, ChangePct]));
      WriteLn(Format('          %s -> %s', [Times[StartIdx], Times[I-1]]));

      Conn.ExecuteNonQuery(Format(
        'INSERT INTO trends (metric_name, trend_type, start_time, end_time, ' +
        'start_value, end_value, change_pct, data_points) VALUES ' +
        '(''response_time'', ''%s'', ''%s'', ''%s'', %.2f, %.2f, %.1f, %d)',
        [TrendType, Times[StartIdx], Times[I-1],
         Values[StartIdx], Values[I-1], ChangePct, ConsecInc + 1]));
    end;

    // Check for decreasing trend
    ConsecDec := 0;
    StartIdx := I - 1;
    while (I < Count) and (Values[I] < Values[I-1]) do
    begin
      Inc(ConsecDec);
      Inc(I);
    end;
    if ConsecDec >= 3 then
    begin
      ChangePct := (Values[I-1] - Values[StartIdx]) / Values[StartIdx] * 100.0;
      TrendType := 'decreasing';
      WriteLn(Format('   TREND: %s from %.1f to %.1f (%d points, %.1f%%)',
        [TrendType, Values[StartIdx], Values[I-1], ConsecDec + 1, ChangePct]));
      WriteLn(Format('          %s -> %s', [Times[StartIdx], Times[I-1]]));

      Conn.ExecuteNonQuery(Format(
        'INSERT INTO trends (metric_name, trend_type, start_time, end_time, ' +
        'start_value, end_value, change_pct, data_points) VALUES ' +
        '(''response_time'', ''%s'', ''%s'', ''%s'', %.2f, %.2f, %.1f, %d)',
        [TrendType, Times[StartIdx], Times[I-1],
         Values[StartIdx], Values[I-1], ChangePct, ConsecDec + 1]));
    end;

    if (ConsecInc < 3) and (ConsecDec < 3) then
      Inc(I);
  end;

  // Also detect spikes (value > 2x previous)
  WriteLn;
  WriteLn('   Spike/Drop detection (>2x change from previous):');
  for I := 1 to Count - 1 do
  begin
    if (Values[I-1] > 0) and (Values[I] / Values[I-1] > 2.0) then
    begin
      WriteLn(Format('   SPIKE: %.1f -> %.1f (%.1fx) at %s',
        [Values[I-1], Values[I], Values[I] / Values[I-1], Times[I]]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO trends (metric_name, trend_type, start_time, end_time, ' +
        'start_value, end_value, change_pct, data_points) VALUES ' +
        '(''response_time'', ''spike'', ''%s'', ''%s'', %.2f, %.2f, %.1f, 2)',
        [Times[I-1], Times[I], Values[I-1], Values[I],
         (Values[I] - Values[I-1]) / Values[I-1] * 100.0]));
    end
    else if (Values[I] > 0) and (Values[I-1] / Values[I] > 2.0) then
    begin
      WriteLn(Format('   DROP:  %.1f -> %.1f (%.2fx) at %s',
        [Values[I-1], Values[I], Values[I] / Values[I-1], Times[I]]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO trends (metric_name, trend_type, start_time, end_time, ' +
        'start_value, end_value, change_pct, data_points) VALUES ' +
        '(''response_time'', ''drop'', ''%s'', ''%s'', %.2f, %.2f, %.1f, 2)',
        [Times[I-1], Times[I], Values[I-1], Values[I],
         (Values[I] - Values[I-1]) / Values[I-1] * 100.0]));
    end;
  end;

  WriteLn;
end;

{ Lists all detected anomalies with their metric, value, detection method, z-score, severity, and timestamp. }
procedure Demo6_AnomalyHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Anomaly History ===');
  WriteLn;

  WriteLn('   All detected anomalies:');
  DS := Conn.ExecuteQuery(
    'SELECT metric_name, ROUND(metric_value, 1) as val, detection_method, ' +
    'ROUND(zscore, 2) as z, severity, recorded_at ' +
    'FROM anomalies ORDER BY recorded_at, metric_name');
  try
    WriteLn(Format('   %-15s %-8s %-12s %-7s %-8s %s',
      ['Metric', 'Value', 'Method', 'Z-Score', 'Severity', 'Time']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %-8s %-12s %-7s %-8s %s',
        [DS.FieldByName('metric_name').AsString,
         DS.FieldByName('val').AsString,
         DS.FieldByName('detection_method').AsString,
         VarToStr(DS.FieldByName('z').Value),
         DS.FieldByName('severity').AsString,
         DS.FieldByName('recorded_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Identifies time periods with anomalies across multiple metrics and shows anomaly counts per metric with average deviation. }
procedure Demo7_MultiMetricAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Multi-Metric Correlation ===');
  WriteLn;

  // Find time periods where multiple metrics had anomalies
  WriteLn('   Anomaly co-occurrence by hour:');
  DS := Conn.ExecuteQuery(
    'SELECT recorded_at, COUNT(*) as anomaly_count, ' +
    'GROUP_CONCAT(DISTINCT metric_name) as affected_metrics ' +
    'FROM anomalies GROUP BY recorded_at ' +
    'HAVING COUNT(*) > 1 ORDER BY anomaly_count DESC');
  try
    if DS.IsEmpty then
      WriteLn('   No multi-metric anomalies in same time period.')
    else
    begin
      while not DS.EOF do
      begin
        WriteLn(Format('   %s: %d anomalies (%s)',
          [DS.FieldByName('recorded_at').AsString,
           DS.FieldByName('anomaly_count').AsInteger,
           DS.FieldByName('affected_metrics').AsString]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Anomalies per metric:');
  DS := Conn.ExecuteQuery(
    'SELECT metric_name, COUNT(*) as cnt, ' +
    'ROUND(AVG(ABS(COALESCE(zscore, deviation / NULLIF(expected_value, 0)))), 2) as avg_severity ' +
    'FROM anomalies GROUP BY metric_name ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %d anomalies  avg_deviation: %s',
        [DS.FieldByName('metric_name').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('avg_severity').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Groups anomalies by severity level with detection methods used, and lists detailed information for all critical-severity anomalies. }
procedure Demo8_SeverityClassification;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Severity Classification ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT severity, COUNT(*) as cnt, ' +
    'GROUP_CONCAT(DISTINCT detection_method) as methods ' +
    'FROM anomalies GROUP BY severity ' +
    'ORDER BY CASE severity WHEN ''critical'' THEN 1 WHEN ''high'' THEN 2 ' +
    'WHEN ''medium'' THEN 3 WHEN ''low'' THEN 4 END');
  try
    WriteLn(Format('   %-10s %-6s %s', ['Severity', 'Count', 'Methods']));
    WriteLn('   ' + StringOfChar('-', 40));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-6d %s',
        [DS.FieldByName('severity').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('methods').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Critical anomalies detail:');
  DS := Conn.ExecuteQuery(
    'SELECT metric_name, ROUND(metric_value, 1) as val, detection_method, ' +
    'ROUND(COALESCE(expected_value, 0), 1) as expected, recorded_at ' +
    'FROM anomalies WHERE severity = ''critical'' ORDER BY recorded_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %s=%.1f (expected ~%.1f) via %s',
        [DS.FieldByName('recorded_at').AsString,
         DS.FieldByName('metric_name').AsString,
         DS.FieldByName('val').AsFloat,
         DS.FieldByName('expected').AsFloat,
         DS.FieldByName('detection_method').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Marks some anomalies as acknowledged, then displays total, acknowledged, and pending alert counts along with pending alerts grouped by severity and metric. }
procedure Demo9_AlertDashboard;
var
  DS: TDataSet;
  Total, Acknowledged: Integer;
begin
  WriteLn('=== 9. Alert Dashboard ===');
  WriteLn;

  // Acknowledge some anomalies
  Conn.ExecuteNonQuery(
    'UPDATE anomalies SET acknowledged = 1 WHERE severity = ''high'' AND metric_name = ''response_time''');
  Conn.ExecuteNonQuery(
    'UPDATE anomalies SET acknowledged = 1 WHERE severity = ''critical'' AND metric_name = ''cpu_usage'' ' +
    'AND recorded_at = ''2024-01-01 13:00:00''');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM anomalies');
  try
    Total := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM anomalies WHERE acknowledged = 1');
  try
    Acknowledged := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total alerts: %d', [Total]));
  WriteLn(Format('   Acknowledged: %d', [Acknowledged]));
  WriteLn(Format('   Pending: %d', [Total - Acknowledged]));
  WriteLn;

  WriteLn('   Pending alerts by severity:');
  DS := Conn.ExecuteQuery(
    'SELECT severity, metric_name, COUNT(*) as cnt ' +
    'FROM anomalies WHERE acknowledged = 0 ' +
    'GROUP BY severity, metric_name ' +
    'ORDER BY CASE severity WHEN ''critical'' THEN 1 WHEN ''high'' THEN 2 ' +
    'WHEN ''medium'' THEN 3 WHEN ''low'' THEN 4 END, metric_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%-8s] %-15s x%d',
        [DS.FieldByName('severity').AsString,
         DS.FieldByName('metric_name').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Shows overall statistics including data points, anomaly rate, detection method effectiveness with average deviations, and trend type summary. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
  TotalMetrics, TotalAnomalies, TotalTrends: Integer;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM metrics');
  try
    TotalMetrics := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM anomalies');
  try
    TotalAnomalies := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM trends');
  try
    TotalTrends := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total data points: %d', [TotalMetrics]));
  WriteLn(Format('   Total anomalies detected: %d', [TotalAnomalies]));
  WriteLn(Format('   Anomaly rate: %.1f%%', [TotalAnomalies * 100.0 / TotalMetrics]));
  WriteLn(Format('   Trends detected: %d', [TotalTrends]));

  WriteLn;
  WriteLn('   Detection method effectiveness:');
  DS := Conn.ExecuteQuery(
    'SELECT detection_method, COUNT(*) as cnt, ' +
    'ROUND(AVG(ABS(deviation)), 1) as avg_dev ' +
    'FROM anomalies GROUP BY detection_method ORDER BY cnt DESC');
  try
    WriteLn(Format('   %-12s %-8s %s', ['Method', 'Alerts', 'Avg Dev']));
    WriteLn('   ' + StringOfChar('-', 35));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-8d %s',
        [DS.FieldByName('detection_method').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('avg_dev').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Trend summary:');
  DS := Conn.ExecuteQuery(
    'SELECT trend_type, COUNT(*) as cnt, ' +
    'ROUND(AVG(ABS(change_pct)), 1) as avg_change ' +
    'FROM trends GROUP BY trend_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d occurrences  avg_change: %s%%',
        [DS.FieldByName('trend_type').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('avg_change').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// ============================================================
// Main
// ============================================================
begin
  WriteLn('Example 125: Anomaly Detection - Z-Score, Moving Average, Thresholds, Trends');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertSampleData;

    Demo1_TimeSeriesData;
    Demo2_ZScoreDetection;
    Demo3_MovingAverage;
    Demo4_ThresholdAlerts;
    Demo5_TrendDetection;
    Demo6_AnomalyHistory;
    Demo7_MultiMetricAnalysis;
    Demo8_SeverityClassification;
    Demo9_AlertDashboard;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
