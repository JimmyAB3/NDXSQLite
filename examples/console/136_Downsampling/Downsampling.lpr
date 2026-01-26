{===============================================================================
  NDXSQLite Example 136 - Downsampling
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Time-series data downsampling
  - Moving average calculations
  - Configurable bucket sizes (10s, 1m, 5m)
  - Detail loss analysis at each granularity
  - Peak detection in downsampled data

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Downsampling;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Raw time series (1-second CPU metrics)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS raw_metrics (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  timestamp TEXT NOT NULL,' +
    '  cpu_load REAL NOT NULL,' +
    '  memory_pct REAL NOT NULL,' +
    '  disk_iops INTEGER NOT NULL,' +
    '  network_mbps REAL NOT NULL' +
    ')'
  );

  // Moving averages table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS moving_averages (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  timestamp TEXT NOT NULL,' +
    '  window_size INTEGER NOT NULL,' +
    '  cpu_avg REAL NOT NULL,' +
    '  memory_avg REAL NOT NULL' +
    ')'
  );

  // Downsampled buckets (configurable granularity)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS downsampled (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  bucket_start TEXT NOT NULL,' +
    '  granularity_sec INTEGER NOT NULL,' +
    '  cpu_min REAL, cpu_max REAL, cpu_avg REAL,' +
    '  mem_min REAL, mem_max REAL, mem_avg REAL,' +
    '  iops_min INTEGER, iops_max INTEGER, iops_avg REAL,' +
    '  net_min REAL, net_max REAL, net_avg REAL,' +
    '  sample_count INTEGER NOT NULL,' +
    '  UNIQUE(bucket_start, granularity_sec)' +
    ')'
  );

  // Granularity configuration
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS granularity_config (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL UNIQUE,' +
    '  seconds INTEGER NOT NULL,' +
    '  retention_hours INTEGER NOT NULL,' +
    '  description TEXT' +
    ')'
  );

  // Detail loss tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS detail_loss (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  granularity_sec INTEGER NOT NULL,' +
    '  metric_name TEXT NOT NULL,' +
    '  raw_stddev REAL,' +
    '  bucket_stddev REAL,' +
    '  peak_raw REAL,' +
    '  peak_bucket REAL,' +
    '  loss_pct REAL' +
    ')'
  );
end;

{ Generates sample data for demonstrations. }
procedure GenerateData;
var
  I: Integer;
  CpuBase, MemBase, NetBase: Real;
  CpuVal, MemVal, NetVal: Real;
  Iops: Integer;
  TS: string;
begin
  // Generate 360 seconds (6 minutes) of 1-second metrics
  // Simulate a CPU spike pattern: normal -> spike -> recovery -> normal
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  for I := 0 to 359 do
  begin
    // CPU: base 30%, spike at 120-180s to 85%, recovery
    if (I >= 120) and (I < 150) then
      CpuBase := 30.0 + (I - 120) * 1.8  // ramp up
    else if (I >= 150) and (I < 180) then
      CpuBase := 84.0 - (I - 150) * 0.5  // plateau/slight decline
    else if (I >= 180) and (I < 210) then
      CpuBase := 69.0 - (I - 180) * 1.3  // ramp down
    else
      CpuBase := 30.0;

    // Add noise
    CpuVal := CpuBase + ((I * 17 + 11) mod 100) / 100.0 * 8.0 - 4.0;
    if CpuVal < 5.0 then CpuVal := 5.0;
    if CpuVal > 98.0 then CpuVal := 98.0;

    // Memory: gradual increase over time
    MemBase := 55.0 + I * 0.02;
    MemVal := MemBase + ((I * 13 + 7) mod 100) / 100.0 * 4.0 - 2.0;

    // Disk IOPS: correlated with CPU
    Iops := Round(CpuVal * 10 + ((I * 23 + 3) mod 50));

    // Network: bursty pattern
    NetBase := 50.0;
    if (I mod 60) < 10 then
      NetBase := 150.0;  // burst every minute
    NetVal := NetBase + ((I * 19 + 5) mod 100) / 100.0 * 30.0 - 15.0;
    if NetVal < 1.0 then NetVal := 1.0;

    TS := Format('2025-01-20 10:%.2d:%.2d', [I div 60, I mod 60]);

    Conn.ExecuteNonQuery(Format(
      'INSERT INTO raw_metrics (timestamp, cpu_load, memory_pct, disk_iops, network_mbps) ' +
      'VALUES (''%s'', %.2f, %.2f, %d, %.2f)',
      [TS, CpuVal, MemVal, Iops, NetVal]));
  end;

  Conn.ExecuteNonQuery('COMMIT');

  // Configure granularities
  Conn.ExecuteNonQuery('INSERT INTO granularity_config (name, seconds, retention_hours, description) VALUES (''10sec'', 10, 6, ''10-second buckets for short-term detail'')');
  Conn.ExecuteNonQuery('INSERT INTO granularity_config (name, seconds, retention_hours, description) VALUES (''1min'', 60, 24, ''1-minute buckets for hourly dashboards'')');
  Conn.ExecuteNonQuery('INSERT INTO granularity_config (name, seconds, retention_hours, description) VALUES (''5min'', 300, 168, ''5-minute buckets for weekly trends'')');
  Conn.ExecuteNonQuery('INSERT INTO granularity_config (name, seconds, retention_hours, description) VALUES (''15min'', 900, 720, ''15-minute buckets for monthly reports'')');
end;

// === Demo 1: Raw Data Overview ===
{ Displays the total raw data point count and the first 12 seconds of CPU, memory, IOPS, and network metrics. }
procedure Demo1_RawDataOverview;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Raw Time Series Data ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM raw_metrics');
  try
    WriteLn(Format('   Total raw data points: %d (1-second intervals, 6 minutes)', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;

  WriteLn('   Sample (first 12 seconds):');
  WriteLn(Format('   %-20s %6s %6s %6s %8s', ['Timestamp', 'CPU%', 'Mem%', 'IOPS', 'Net Mbps']));
  WriteLn('   ' + StringOfChar('-', 54));

  DS := Conn.ExecuteQuery('SELECT * FROM raw_metrics ORDER BY id LIMIT 12');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %6.1f %6.1f %6d %8.1f', [
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('cpu_load').AsFloat,
        DS.FieldByName('memory_pct').AsFloat,
        DS.FieldByName('disk_iops').AsInteger,
        DS.FieldByName('network_mbps').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('   ...');
  WriteLn;
end;

// === Demo 2: Moving Averages ===
{ Computes 3-point, 5-point, and 10-point moving averages for CPU and memory, then displays the spike period comparing raw values against each window size. }
procedure Demo2_MovingAverages;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Moving Averages (CPU Load) ===');
  WriteLn;

  // Compute 3-point, 5-point, and 10-point moving averages using window functions
  // Since SQLite window functions may not be available, use subquery approach
  Conn.ExecuteNonQuery(
    'INSERT INTO moving_averages (timestamp, window_size, cpu_avg, memory_avg) ' +
    'SELECT r.timestamp, 3, ' +
    '  (SELECT AVG(r2.cpu_load) FROM raw_metrics r2 WHERE r2.id BETWEEN r.id - 1 AND r.id + 1), ' +
    '  (SELECT AVG(r2.memory_pct) FROM raw_metrics r2 WHERE r2.id BETWEEN r.id - 1 AND r.id + 1) ' +
    'FROM raw_metrics r WHERE r.id >= 2 AND r.id <= (SELECT MAX(id) - 1 FROM raw_metrics)');

  Conn.ExecuteNonQuery(
    'INSERT INTO moving_averages (timestamp, window_size, cpu_avg, memory_avg) ' +
    'SELECT r.timestamp, 5, ' +
    '  (SELECT AVG(r2.cpu_load) FROM raw_metrics r2 WHERE r2.id BETWEEN r.id - 2 AND r.id + 2), ' +
    '  (SELECT AVG(r2.memory_pct) FROM raw_metrics r2 WHERE r2.id BETWEEN r.id - 2 AND r.id + 2) ' +
    'FROM raw_metrics r WHERE r.id >= 3 AND r.id <= (SELECT MAX(id) - 2 FROM raw_metrics)');

  Conn.ExecuteNonQuery(
    'INSERT INTO moving_averages (timestamp, window_size, cpu_avg, memory_avg) ' +
    'SELECT r.timestamp, 10, ' +
    '  (SELECT AVG(r2.cpu_load) FROM raw_metrics r2 WHERE r2.id BETWEEN r.id - 5 AND r.id + 4), ' +
    '  (SELECT AVG(r2.memory_pct) FROM raw_metrics r2 WHERE r2.id BETWEEN r.id - 5 AND r.id + 4) ' +
    'FROM raw_metrics r WHERE r.id >= 6 AND r.id <= (SELECT MAX(id) - 4 FROM raw_metrics)');

  // Show comparison during the spike period (around t=130-160)
  WriteLn('   CPU during spike (t=120-170s), comparing window sizes:');
  WriteLn(Format('   %-20s %6s %6s %6s %6s', ['Timestamp', 'Raw', 'MA-3', 'MA-5', 'MA-10']));
  WriteLn('   ' + StringOfChar('-', 50));

  DS := Conn.ExecuteQuery(
    'SELECT r.timestamp, r.cpu_load as raw_val, ' +
    '  (SELECT ma.cpu_avg FROM moving_averages ma WHERE ma.timestamp = r.timestamp AND ma.window_size = 3) as ma3, ' +
    '  (SELECT ma.cpu_avg FROM moving_averages ma WHERE ma.timestamp = r.timestamp AND ma.window_size = 5) as ma5, ' +
    '  (SELECT ma.cpu_avg FROM moving_averages ma WHERE ma.timestamp = r.timestamp AND ma.window_size = 10) as ma10 ' +
    'FROM raw_metrics r ' +
    'WHERE r.id >= 121 AND r.id <= 171 AND (r.id - 121) % 5 = 0 ' +
    'ORDER BY r.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %6.1f %6.1f %6.1f %6.1f', [
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('raw_val').AsFloat,
        DS.FieldByName('ma3').AsFloat,
        DS.FieldByName('ma5').AsFloat,
        DS.FieldByName('ma10').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Larger windows smooth more aggressively (MA-10 delays spike detection)');
  WriteLn;
end;

// === Demo 3: 10-Second Buckets ===
{ Aggregates raw metrics into 10-second buckets with min/max/avg for all metrics, then displays CPU load buckets during the spike period. }
procedure Demo3_10SecBuckets;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Downsampling: 10-Second Buckets ===');
  WriteLn;

  Conn.ExecuteNonQuery(
    'INSERT INTO downsampled (bucket_start, granularity_sec, cpu_min, cpu_max, cpu_avg, ' +
    '  mem_min, mem_max, mem_avg, iops_min, iops_max, iops_avg, net_min, net_max, net_avg, sample_count) ' +
    'SELECT ' +
    '  substr(timestamp, 1, 18) || ''0'' as bucket_start, 10, ' +
    '  MIN(cpu_load), MAX(cpu_load), AVG(cpu_load), ' +
    '  MIN(memory_pct), MAX(memory_pct), AVG(memory_pct), ' +
    '  MIN(disk_iops), MAX(disk_iops), AVG(disk_iops), ' +
    '  MIN(network_mbps), MAX(network_mbps), AVG(network_mbps), ' +
    '  COUNT(*) ' +
    'FROM raw_metrics ' +
    'GROUP BY substr(timestamp, 1, 18) || ''0''');

  WriteLn('   CPU Load in 10-second buckets (showing spike period):');
  WriteLn(Format('   %-20s %6s %6s %6s %5s', ['Bucket', 'Min', 'Avg', 'Max', 'N']));
  WriteLn('   ' + StringOfChar('-', 48));

  DS := Conn.ExecuteQuery(
    'SELECT bucket_start, cpu_min, cpu_avg, cpu_max, sample_count ' +
    'FROM downsampled WHERE granularity_sec = 10 ' +
    'AND bucket_start >= ''2025-01-20 10:02:00'' AND bucket_start < ''2025-01-20 10:03:10'' ' +
    'ORDER BY bucket_start');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %6.1f %6.1f %6.1f %5d', [
        DS.FieldByName('bucket_start').AsString,
        DS.FieldByName('cpu_min').AsFloat,
        DS.FieldByName('cpu_avg').AsFloat,
        DS.FieldByName('cpu_max').AsFloat,
        DS.FieldByName('sample_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM downsampled WHERE granularity_sec = 10');
  try
    WriteLn(Format('   Total 10-sec buckets: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 4: 1-Minute Buckets ===
{ Aggregates raw metrics into 1-minute buckets and displays CPU min/avg/max, memory, IOPS, and network averages for each minute. }
procedure Demo4_1MinBuckets;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Downsampling: 1-Minute Buckets ===');
  WriteLn;

  Conn.ExecuteNonQuery(
    'INSERT INTO downsampled (bucket_start, granularity_sec, cpu_min, cpu_max, cpu_avg, ' +
    '  mem_min, mem_max, mem_avg, iops_min, iops_max, iops_avg, net_min, net_max, net_avg, sample_count) ' +
    'SELECT ' +
    '  substr(timestamp, 1, 16) || '':00'' as bucket_start, 60, ' +
    '  MIN(cpu_load), MAX(cpu_load), AVG(cpu_load), ' +
    '  MIN(memory_pct), MAX(memory_pct), AVG(memory_pct), ' +
    '  MIN(disk_iops), MAX(disk_iops), AVG(disk_iops), ' +
    '  MIN(network_mbps), MAX(network_mbps), AVG(network_mbps), ' +
    '  COUNT(*) ' +
    'FROM raw_metrics ' +
    'GROUP BY substr(timestamp, 1, 16)');

  WriteLn('   All metrics at 1-minute granularity:');
  WriteLn(Format('   %-14s %6s %6s %6s %6s %6s %6s %5s', ['Minute', 'CPUmin', 'CPUavg', 'CPUmax', 'Mem%', 'IOPS', 'Net', 'N']));
  WriteLn('   ' + StringOfChar('-', 60));

  DS := Conn.ExecuteQuery(
    'SELECT bucket_start, cpu_min, cpu_avg, cpu_max, mem_avg, iops_avg, net_avg, sample_count ' +
    'FROM downsampled WHERE granularity_sec = 60 ORDER BY bucket_start');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %6.1f %6.1f %6.1f %6.1f %6.0f %6.1f %5d', [
        Copy(DS.FieldByName('bucket_start').AsString, 12, 5),
        DS.FieldByName('cpu_min').AsFloat,
        DS.FieldByName('cpu_avg').AsFloat,
        DS.FieldByName('cpu_max').AsFloat,
        DS.FieldByName('mem_avg').AsFloat,
        DS.FieldByName('iops_avg').AsFloat,
        DS.FieldByName('net_avg').AsFloat,
        DS.FieldByName('sample_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 5: 5-Minute Buckets ===
{ Aggregates raw metrics into 5-minute buckets aligned to 0 and 5 minute boundaries and displays the full summary. }
procedure Demo5_5MinBuckets;
var
  DS: TDataSet;
  BucketMinute: Integer;
begin
  WriteLn('=== 5. Downsampling: 5-Minute Buckets ===');
  WriteLn;

  // Group by 5-minute boundaries
  Conn.ExecuteNonQuery(
    'INSERT INTO downsampled (bucket_start, granularity_sec, cpu_min, cpu_max, cpu_avg, ' +
    '  mem_min, mem_max, mem_avg, iops_min, iops_max, iops_avg, net_min, net_max, net_avg, sample_count) ' +
    'SELECT ' +
    '  substr(timestamp, 1, 15) || CASE ' +
    '    WHEN CAST(substr(timestamp, 16, 1) AS INTEGER) < 5 THEN ''0:00'' ' +
    '    ELSE ''5:00'' END as bucket_start, 300, ' +
    '  MIN(cpu_load), MAX(cpu_load), AVG(cpu_load), ' +
    '  MIN(memory_pct), MAX(memory_pct), AVG(memory_pct), ' +
    '  MIN(disk_iops), MAX(disk_iops), AVG(disk_iops), ' +
    '  MIN(network_mbps), MAX(network_mbps), AVG(network_mbps), ' +
    '  COUNT(*) ' +
    'FROM raw_metrics ' +
    'GROUP BY substr(timestamp, 1, 15) || CASE ' +
    '    WHEN CAST(substr(timestamp, 16, 1) AS INTEGER) < 5 THEN ''0:00'' ' +
    '    ELSE ''5:00'' END');

  WriteLn('   5-minute summary (full 6-minute window):');
  WriteLn(Format('   %-14s %6s %6s %6s %6s %6s %5s', ['Bucket', 'CPUmin', 'CPUavg', 'CPUmax', 'Mem%', 'Net', 'N']));
  WriteLn('   ' + StringOfChar('-', 54));

  DS := Conn.ExecuteQuery(
    'SELECT bucket_start, cpu_min, cpu_avg, cpu_max, mem_avg, net_avg, sample_count ' +
    'FROM downsampled WHERE granularity_sec = 300 ORDER BY bucket_start');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %6.1f %6.1f %6.1f %6.1f %6.1f %5d', [
        Copy(DS.FieldByName('bucket_start').AsString, 12, 5),
        DS.FieldByName('cpu_min').AsFloat,
        DS.FieldByName('cpu_avg').AsFloat,
        DS.FieldByName('cpu_max').AsFloat,
        DS.FieldByName('mem_avg').AsFloat,
        DS.FieldByName('net_avg').AsFloat,
        DS.FieldByName('sample_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 6: Progressive Detail Loss ===
{ Compares raw peak and standard deviation against each granularity level, showing peak loss percentage, smoothing effect, and compression ratio. }
procedure Demo6_DetailLoss;
var
  DS: TDataSet;
  RawPeak, RawStddev: Real;
begin
  WriteLn('=== 6. Progressive Detail Loss Analysis ===');
  WriteLn;

  // Get raw statistics for comparison
  DS := Conn.ExecuteQuery(
    'SELECT MAX(cpu_load) as peak, ' +
    '  AVG(cpu_load) as mean, ' +
    '  AVG(cpu_load * cpu_load) - AVG(cpu_load) * AVG(cpu_load) as variance ' +
    'FROM raw_metrics');
  try
    RawPeak := DS.FieldByName('peak').AsFloat;
    RawStddev := Sqrt(DS.FieldByName('variance').AsFloat);
  finally
    DS.Free;
  end;

  WriteLn(Format('   Raw data: peak=%.2f%%, stddev=%.2f', [RawPeak, RawStddev]));
  WriteLn;
  WriteLn(Format('   %-12s %8s %8s %10s %10s %8s', ['Granularity', 'Buckets', 'Peak', 'PeakLoss%', 'Smoothing', 'Compress']));
  WriteLn('   ' + StringOfChar('-', 64));

  // 10-second analysis
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt, MAX(cpu_max) as peak, ' +
    '  AVG(cpu_avg * cpu_avg) - AVG(cpu_avg) * AVG(cpu_avg) as variance ' +
    'FROM downsampled WHERE granularity_sec = 10');
  try
    WriteLn(Format('   %-12s %8d %8.2f %9.1f%% %10.2f %7.0fx', [
      '10 seconds',
      DS.FieldByName('cnt').AsInteger,
      DS.FieldByName('peak').AsFloat,
      (RawPeak - DS.FieldByName('peak').AsFloat) / RawPeak * 100,
      Sqrt(DS.FieldByName('variance').AsFloat),
      360.0 / DS.FieldByName('cnt').AsFloat]));
  finally
    DS.Free;
  end;

  // 1-minute analysis
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt, MAX(cpu_max) as peak, ' +
    '  AVG(cpu_avg * cpu_avg) - AVG(cpu_avg) * AVG(cpu_avg) as variance ' +
    'FROM downsampled WHERE granularity_sec = 60');
  try
    WriteLn(Format('   %-12s %8d %8.2f %9.1f%% %10.2f %7.0fx', [
      '1 minute',
      DS.FieldByName('cnt').AsInteger,
      DS.FieldByName('peak').AsFloat,
      (RawPeak - DS.FieldByName('peak').AsFloat) / RawPeak * 100,
      Sqrt(DS.FieldByName('variance').AsFloat),
      360.0 / DS.FieldByName('cnt').AsFloat]));
  finally
    DS.Free;
  end;

  // 5-minute analysis
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt, MAX(cpu_max) as peak, ' +
    '  AVG(cpu_avg * cpu_avg) - AVG(cpu_avg) * AVG(cpu_avg) as variance ' +
    'FROM downsampled WHERE granularity_sec = 300');
  try
    WriteLn(Format('   %-12s %8d %8.2f %9.1f%% %10.2f %7.0fx', [
      '5 minutes',
      DS.FieldByName('cnt').AsInteger,
      DS.FieldByName('peak').AsFloat,
      (RawPeak - DS.FieldByName('peak').AsFloat) / RawPeak * 100,
      Sqrt(DS.FieldByName('variance').AsFloat),
      360.0 / DS.FieldByName('cnt').AsFloat]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Note: Higher granularity preserves peaks but loses temporal precision');
  WriteLn('   Note: Avg-based smoothing progressively reduces apparent variance');
  WriteLn;
end;

// === Demo 7: Peak Detection at Each Level ===
{ Detects CPU spikes exceeding 60% at raw, 10-second, and 1-minute levels, comparing spike counts, timing, and whether avg-based or max-based detection succeeds. }
procedure Demo7_PeakDetection;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Peak Detection Across Granularities ===');
  WriteLn;
  WriteLn('   Detecting CPU spikes (threshold > 60%) at each level:');
  WriteLn;

  // Raw peaks
  WriteLn('   Raw (1-sec): spikes detected by timestamp');
  DS := Conn.ExecuteQuery(
    'SELECT MIN(timestamp) as first_spike, MAX(timestamp) as last_spike, ' +
    '  COUNT(*) as spike_count, MAX(cpu_load) as max_val ' +
    'FROM raw_metrics WHERE cpu_load > 60.0');
  try
    WriteLn(Format('     First: %s, Last: %s', [DS.FieldByName('first_spike').AsString, DS.FieldByName('last_spike').AsString]));
    WriteLn(Format('     Count: %d seconds above threshold, Peak: %.1f%%', [DS.FieldByName('spike_count').AsInteger, DS.FieldByName('max_val').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;

  // 10-sec peaks
  WriteLn('   10-sec buckets: spikes where avg > 60%');
  DS := Conn.ExecuteQuery(
    'SELECT MIN(bucket_start) as first_spike, MAX(bucket_start) as last_spike, ' +
    '  COUNT(*) as spike_count, MAX(cpu_max) as max_val ' +
    'FROM downsampled WHERE granularity_sec = 10 AND cpu_avg > 60.0');
  try
    WriteLn(Format('     First: %s, Last: %s', [DS.FieldByName('first_spike').AsString, DS.FieldByName('last_spike').AsString]));
    WriteLn(Format('     Count: %d buckets above threshold, Peak: %.1f%%', [DS.FieldByName('spike_count').AsInteger, DS.FieldByName('max_val').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;

  // 1-min peaks
  WriteLn('   1-min buckets: spikes where avg > 60%');
  DS := Conn.ExecuteQuery(
    'SELECT MIN(bucket_start) as first_spike, MAX(bucket_start) as last_spike, ' +
    '  COUNT(*) as spike_count, MAX(cpu_max) as max_val ' +
    'FROM downsampled WHERE granularity_sec = 60 AND cpu_avg > 60.0');
  try
    if DS.FieldByName('spike_count').AsInteger > 0 then
    begin
      WriteLn(Format('     First: %s, Last: %s', [DS.FieldByName('first_spike').AsString, DS.FieldByName('last_spike').AsString]));
      WriteLn(Format('     Count: %d buckets above threshold, Peak: %.1f%%', [DS.FieldByName('spike_count').AsInteger, DS.FieldByName('max_val').AsFloat]));
    end
    else
      WriteLn('     No 1-minute buckets with avg > 60% (spike diluted by averaging)');
  finally
    DS.Free;
  end;
  WriteLn;

  // Show that max detection still works
  WriteLn('   1-min buckets: spikes where MAX > 60% (preserved in bucket max)');
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as spike_count, MAX(cpu_max) as max_val ' +
    'FROM downsampled WHERE granularity_sec = 60 AND cpu_max > 60.0');
  try
    WriteLn(Format('     Count: %d buckets with max > 60%%, Peak: %.1f%%', [DS.FieldByName('spike_count').AsInteger, DS.FieldByName('max_val').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 8: Granularity Configuration ===
{ Queries and displays the granularity_config table showing each tier's bucket size, retention period, and description. }
procedure Demo8_GranularityConfig;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Granularity Configuration ===');
  WriteLn;
  WriteLn(Format('   %-8s %8s %12s  %s', ['Name', 'Seconds', 'Retention', 'Description']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery('SELECT * FROM granularity_config ORDER BY seconds');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %8d %10d h  %s', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('seconds').AsInteger,
        DS.FieldByName('retention_hours').AsInteger,
        DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Configured tiers progressively trade detail for storage efficiency');
  WriteLn;
end;

// === Demo 9: Compression Statistics ===
{ Displays record counts, compression ratios, estimated byte sizes, and storage savings for each downsampling level plus moving average record counts. }
procedure Demo9_CompressionStats;
var
  DS: TDataSet;
  RawCount: Integer;
begin
  WriteLn('=== 9. Compression Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM raw_metrics');
  try
    RawCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   %-14s %8s %10s %12s %10s', ['Level', 'Records', 'Ratio', 'Est. Bytes', 'Savings']));
  WriteLn('   ' + StringOfChar('-', 58));
  WriteLn(Format('   %-14s %8d %10s %12d %10s', ['Raw (1s)', RawCount, '1:1', RawCount * 60, '-']));

  DS := Conn.ExecuteQuery(
    'SELECT granularity_sec, COUNT(*) as cnt FROM downsampled GROUP BY granularity_sec ORDER BY granularity_sec');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %8d %8d:1 %12d %9.0f%%', [
        BoolToStr(DS.FieldByName('granularity_sec').AsInteger = 10, '10-sec',
          BoolToStr(DS.FieldByName('granularity_sec').AsInteger = 60, '1-min',
            BoolToStr(DS.FieldByName('granularity_sec').AsInteger = 300, '5-min', '15-min'))),
        DS.FieldByName('cnt').AsInteger,
        RawCount div DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('cnt').AsInteger * 100,
        (1.0 - DS.FieldByName('cnt').AsFloat * 100.0 / (RawCount * 60.0)) * 100.0]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Moving average record counts
  DS := Conn.ExecuteQuery(
    'SELECT window_size, COUNT(*) as cnt FROM moving_averages GROUP BY window_size ORDER BY window_size');
  try
    WriteLn('   Moving average records:');
    while not DS.EOF do
    begin
      WriteLn(Format('     MA-%d: %d records', [DS.FieldByName('window_size').AsInteger, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 10: Visual Comparison ===
{ Renders a text-based bar chart of 1-minute CPU load buckets with hash marks for averages and dots for min-max ranges. }
procedure Demo10_VisualComparison;
var
  DS: TDataSet;
  BarLen: Integer;
begin
  WriteLn('=== 10. Visual: CPU Load at Different Granularities ===');
  WriteLn;

  // 1-minute view with bars
  WriteLn('   1-minute view (avg with min-max range):');
  WriteLn(Format('   %-6s %s', ['Time', '0%       25%       50%       75%      100%']));
  WriteLn(Format('   %-6s %s', ['', '|---------|---------|---------|---------|']));

  DS := Conn.ExecuteQuery(
    'SELECT bucket_start, cpu_min, cpu_avg, cpu_max ' +
    'FROM downsampled WHERE granularity_sec = 60 ORDER BY bucket_start');
  try
    while not DS.EOF do
    begin
      BarLen := Round(DS.FieldByName('cpu_avg').AsFloat / 100.0 * 40);
      if BarLen < 1 then BarLen := 1;
      Write(Format('   %-6s ', [Copy(DS.FieldByName('bucket_start').AsString, 12, 5)]));
      Write(StringOfChar('.', Round(DS.FieldByName('cpu_min').AsFloat / 100.0 * 40)));
      Write(StringOfChar('#', BarLen - Round(DS.FieldByName('cpu_min').AsFloat / 100.0 * 40)));
      if Round(DS.FieldByName('cpu_max').AsFloat / 100.0 * 40) > BarLen then
        Write(StringOfChar('.', Round(DS.FieldByName('cpu_max').AsFloat / 100.0 * 40) - BarLen));
      WriteLn(Format(' [%.0f-%.0f%%]', [DS.FieldByName('cpu_min').AsFloat, DS.FieldByName('cpu_max').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Main ===
begin
  WriteLn('Example 136: Downsampling - Moving Averages, Bucketing, Progressive Detail Loss');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    GenerateData;

    Demo1_RawDataOverview;
    Demo2_MovingAverages;
    Demo3_10SecBuckets;
    Demo4_1MinBuckets;
    Demo5_5MinBuckets;
    Demo6_DetailLoss;
    Demo7_PeakDetection;
    Demo8_GranularityConfig;
    Demo9_CompressionStats;
    Demo10_VisualComparison;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
