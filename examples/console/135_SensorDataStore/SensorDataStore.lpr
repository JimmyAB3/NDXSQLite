{===============================================================================
  NDXSQLite Example 135 - Sensor Data Store
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Sensor registration and metadata
  - Batch reading ingestion
  - Hourly and daily aggregation
  - Retention policy enforcement
  - Time range queries on sensor data

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SensorDataStore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Sensor registry
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS sensors (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sensor_id TEXT NOT NULL UNIQUE,' +
    '  sensor_type TEXT NOT NULL,' +
    '  location TEXT NOT NULL,' +
    '  unit TEXT NOT NULL,' +
    '  min_range REAL,' +
    '  max_range REAL,' +
    '  sample_rate_hz REAL NOT NULL DEFAULT 1.0' +
    ')'
  );

  // Raw readings (high-frequency)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS readings_raw (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sensor_id TEXT NOT NULL,' +
    '  timestamp TEXT NOT NULL,' +
    '  value REAL NOT NULL,' +
    '  quality INTEGER NOT NULL DEFAULT 100,' +
    '  FOREIGN KEY(sensor_id) REFERENCES sensors(sensor_id)' +
    ')'
  );

  // Hourly aggregations
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS readings_hourly (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sensor_id TEXT NOT NULL,' +
    '  bucket_hour TEXT NOT NULL,' +
    '  min_value REAL NOT NULL,' +
    '  max_value REAL NOT NULL,' +
    '  avg_value REAL NOT NULL,' +
    '  sample_count INTEGER NOT NULL,' +
    '  UNIQUE(sensor_id, bucket_hour),' +
    '  FOREIGN KEY(sensor_id) REFERENCES sensors(sensor_id)' +
    ')'
  );

  // Daily aggregations
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS readings_daily (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sensor_id TEXT NOT NULL,' +
    '  bucket_date TEXT NOT NULL,' +
    '  min_value REAL NOT NULL,' +
    '  max_value REAL NOT NULL,' +
    '  avg_value REAL NOT NULL,' +
    '  sample_count INTEGER NOT NULL,' +
    '  UNIQUE(sensor_id, bucket_date),' +
    '  FOREIGN KEY(sensor_id) REFERENCES sensors(sensor_id)' +
    ')'
  );

  // Retention policies
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS retention_policies (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  table_name TEXT NOT NULL UNIQUE,' +
    '  retention_hours INTEGER NOT NULL,' +
    '  description TEXT' +
    ')'
  );

  // Ingestion log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS ingestion_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  batch_id TEXT NOT NULL,' +
    '  sensor_id TEXT NOT NULL,' +
    '  record_count INTEGER NOT NULL,' +
    '  ingested_at TEXT NOT NULL,' +
    '  duration_ms INTEGER NOT NULL DEFAULT 0' +
    ')'
  );

  // Indexes for performance
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_raw_sensor_ts ON readings_raw(sensor_id, timestamp)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_raw_ts ON readings_raw(timestamp)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_hourly_sensor ON readings_hourly(sensor_id, bucket_hour)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_daily_sensor ON readings_daily(sensor_id, bucket_date)');
end;

{ Registers sensor devices in the database. }
procedure RegisterSensors;
begin
  Conn.ExecuteNonQuery('INSERT INTO sensors (sensor_id, sensor_type, location, unit, min_range, max_range, sample_rate_hz) VALUES (''TEMP-001'', ''temperature'', ''Server Room A'', ''C'', -10.0, 60.0, 1.0)');
  Conn.ExecuteNonQuery('INSERT INTO sensors (sensor_id, sensor_type, location, unit, min_range, max_range, sample_rate_hz) VALUES (''TEMP-002'', ''temperature'', ''Server Room B'', ''C'', -10.0, 60.0, 1.0)');
  Conn.ExecuteNonQuery('INSERT INTO sensors (sensor_id, sensor_type, location, unit, min_range, max_range, sample_rate_hz) VALUES (''HUM-001'', ''humidity'', ''Server Room A'', ''%'', 0.0, 100.0, 0.5)');
  Conn.ExecuteNonQuery('INSERT INTO sensors (sensor_id, sensor_type, location, unit, min_range, max_range, sample_rate_hz) VALUES (''PRES-001'', ''pressure'', ''Outdoor'', ''hPa'', 900.0, 1100.0, 0.1)');
  Conn.ExecuteNonQuery('INSERT INTO sensors (sensor_id, sensor_type, location, unit, min_range, max_range, sample_rate_hz) VALUES (''LIGHT-001'', ''light'', ''Office Floor 2'', ''lux'', 0.0, 10000.0, 2.0)');

  // Retention policies
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (table_name, retention_hours, description) VALUES (''readings_raw'', 24, ''Keep raw data for 24 hours'')');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (table_name, retention_hours, description) VALUES (''readings_hourly'', 720, ''Keep hourly data for 30 days'')');
  Conn.ExecuteNonQuery('INSERT INTO retention_policies (table_name, retention_hours, description) VALUES (''readings_daily'', 8760, ''Keep daily data for 1 year'')');
end;

{ Inserts Count raw readings for the given sensor, computing timestamps at 5-second intervals and values with variance-based noise around the base value. }
procedure GenerateBatchReadings(ASensorId: string; BaseTime: string; BaseValue, Variance: Real; Count: Integer; HourOffset: Integer);
var
  I, Minute, Second: Integer;
  Value: Real;
  TS: string;
begin
  for I := 0 to Count - 1 do
  begin
    Minute := (I * 5) div 60;
    Second := (I * 5) mod 60;
    Value := BaseValue + (((I * 7 + HourOffset * 13) mod 100) / 100.0 - 0.5) * Variance * 2;
    TS := Format('%s %.2d:%.2d:%.2d', [BaseTime, HourOffset, Minute, Second]);
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO readings_raw (sensor_id, timestamp, value, quality) VALUES (''%s'', ''%s'', %.4f, %d)',
      [ASensorId, TS, Value, 95 + (I mod 6)]));
  end;
end;

// === Demo 1: Sensor Registry ===
{ Queries and displays all registered sensors with their type, location, unit, measurement range, and sample rate. }
procedure Demo1_SensorRegistry;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Sensor Registry ===');
  WriteLn;
  WriteLn(Format('   %-10s %-14s %-16s %-5s %8s %8s %5s', ['ID', 'Type', 'Location', 'Unit', 'Min', 'Max', 'Hz']));
  WriteLn('   ' + StringOfChar('-', 74));

  DS := Conn.ExecuteQuery('SELECT * FROM sensors ORDER BY sensor_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %-16s %-5s %8.1f %8.1f %5.1f', [
        DS.FieldByName('sensor_id').AsString,
        DS.FieldByName('sensor_type').AsString,
        DS.FieldByName('location').AsString,
        DS.FieldByName('unit').AsString,
        DS.FieldByName('min_range').AsFloat,
        DS.FieldByName('max_range').AsFloat,
        DS.FieldByName('sample_rate_hz').AsFloat
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   5 sensors registered across 3 locations');
  WriteLn;
end;

// === Demo 2: Batch Ingestion ===
{ Ingests four hourly batches of sensor readings across five sensors using transactions and logs each batch with record count and duration. }
procedure Demo2_BatchIngestion;
var
  DS: TDataSet;
  TotalRecords: Integer;
begin
  WriteLn('=== 2. High-Frequency Batch Ingestion ===');
  WriteLn;
  WriteLn('   Ingesting sensor data using batch transactions...');
  WriteLn;

  TotalRecords := 0;

  // Hour 0: 08:00 - readings every 5 seconds (12 per minute)
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  GenerateBatchReadings('TEMP-001', '2025-01-20', 22.5, 2.0, 60, 8);
  GenerateBatchReadings('TEMP-002', '2025-01-20', 24.0, 1.5, 60, 8);
  GenerateBatchReadings('HUM-001', '2025-01-20', 45.0, 5.0, 30, 8);
  GenerateBatchReadings('PRES-001', '2025-01-20', 1013.0, 3.0, 12, 8);
  GenerateBatchReadings('LIGHT-001', '2025-01-20', 350.0, 100.0, 60, 8);
  Conn.ExecuteNonQuery('COMMIT');
  Conn.ExecuteNonQuery(Format('INSERT INTO ingestion_log (batch_id, sensor_id, record_count, ingested_at, duration_ms) VALUES (''batch_h08'', ''ALL'', %d, ''2025-01-20 08:05:00'', 45)', [222]));
  TotalRecords := TotalRecords + 222;
  WriteLn(Format('   Batch h08: 222 records (5 sensors, hour 08:00)', []));

  // Hour 1: 09:00
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  GenerateBatchReadings('TEMP-001', '2025-01-20', 23.0, 2.5, 60, 9);
  GenerateBatchReadings('TEMP-002', '2025-01-20', 25.5, 2.0, 60, 9);
  GenerateBatchReadings('HUM-001', '2025-01-20', 43.0, 6.0, 30, 9);
  GenerateBatchReadings('PRES-001', '2025-01-20', 1012.5, 2.5, 12, 9);
  GenerateBatchReadings('LIGHT-001', '2025-01-20', 500.0, 150.0, 60, 9);
  Conn.ExecuteNonQuery('COMMIT');
  Conn.ExecuteNonQuery(Format('INSERT INTO ingestion_log (batch_id, sensor_id, record_count, ingested_at, duration_ms) VALUES (''batch_h09'', ''ALL'', %d, ''2025-01-20 09:05:00'', 52)', [222]));
  TotalRecords := TotalRecords + 222;
  WriteLn(Format('   Batch h09: 222 records (5 sensors, hour 09:00)', []));

  // Hour 2: 10:00
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  GenerateBatchReadings('TEMP-001', '2025-01-20', 24.5, 3.0, 60, 10);
  GenerateBatchReadings('TEMP-002', '2025-01-20', 26.0, 2.0, 60, 10);
  GenerateBatchReadings('HUM-001', '2025-01-20', 40.0, 8.0, 30, 10);
  GenerateBatchReadings('PRES-001', '2025-01-20', 1011.0, 4.0, 12, 10);
  GenerateBatchReadings('LIGHT-001', '2025-01-20', 650.0, 200.0, 60, 10);
  Conn.ExecuteNonQuery('COMMIT');
  Conn.ExecuteNonQuery(Format('INSERT INTO ingestion_log (batch_id, sensor_id, record_count, ingested_at, duration_ms) VALUES (''batch_h10'', ''ALL'', %d, ''2025-01-20 10:05:00'', 48)', [222]));
  TotalRecords := TotalRecords + 222;
  WriteLn(Format('   Batch h10: 222 records (5 sensors, hour 10:00)', []));

  // Hour 3: 11:00
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  GenerateBatchReadings('TEMP-001', '2025-01-20', 25.0, 2.0, 60, 11);
  GenerateBatchReadings('TEMP-002', '2025-01-20', 27.0, 1.5, 60, 11);
  GenerateBatchReadings('HUM-001', '2025-01-20', 38.0, 7.0, 30, 11);
  GenerateBatchReadings('PRES-001', '2025-01-20', 1010.0, 3.5, 12, 11);
  GenerateBatchReadings('LIGHT-001', '2025-01-20', 800.0, 250.0, 60, 11);
  Conn.ExecuteNonQuery('COMMIT');
  Conn.ExecuteNonQuery(Format('INSERT INTO ingestion_log (batch_id, sensor_id, record_count, ingested_at, duration_ms) VALUES (''batch_h11'', ''ALL'', %d, ''2025-01-20 11:05:00'', 50)', [222]));
  TotalRecords := TotalRecords + 222;
  WriteLn(Format('   Batch h11: 222 records (5 sensors, hour 11:00)', []));

  WriteLn;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM readings_raw');
  try
    WriteLn(Format('   Total raw records ingested: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 3: Raw Data Sample ===
{ Queries and displays the first 10 raw readings for sensor TEMP-001 showing timestamp, value, and quality. }
procedure Demo3_RawDataSample;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Raw Data Sample (TEMP-001, first 10 readings) ===');
  WriteLn;
  WriteLn(Format('   %-22s %8s %4s', ['Timestamp', 'Value', 'Qual']));
  WriteLn('   ' + StringOfChar('-', 38));

  DS := Conn.ExecuteQuery(
    'SELECT timestamp, value, quality FROM readings_raw ' +
    'WHERE sensor_id = ''TEMP-001'' ORDER BY timestamp LIMIT 10');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %8.4f %4d', [
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('value').AsFloat,
        DS.FieldByName('quality').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   ...');
  WriteLn;
end;

// === Demo 4: Hourly Aggregation ===
{ Computes and inserts hourly min/max/avg/count aggregations from raw readings, then displays all hourly bucket records. }
procedure Demo4_HourlyAggregation;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Auto-Aggregation (Hourly Buckets) ===');
  WriteLn;
  WriteLn('   Computing hourly min/max/avg from raw readings...');
  WriteLn;

  // Perform hourly aggregation
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO readings_hourly (sensor_id, bucket_hour, min_value, max_value, avg_value, sample_count) ' +
    'SELECT sensor_id, ' +
    '  substr(timestamp, 1, 13) || '':00:00'' as bucket_hour, ' +
    '  MIN(value) as min_value, ' +
    '  MAX(value) as max_value, ' +
    '  AVG(value) as avg_value, ' +
    '  COUNT(*) as sample_count ' +
    'FROM readings_raw ' +
    'GROUP BY sensor_id, substr(timestamp, 1, 13)');

  WriteLn(Format('   %-10s %-22s %8s %8s %8s %6s', ['Sensor', 'Hour', 'Min', 'Max', 'Avg', 'Count']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT * FROM readings_hourly ORDER BY sensor_id, bucket_hour');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-22s %8.2f %8.2f %8.2f %6d', [
        DS.FieldByName('sensor_id').AsString,
        DS.FieldByName('bucket_hour').AsString,
        DS.FieldByName('min_value').AsFloat,
        DS.FieldByName('max_value').AsFloat,
        DS.FieldByName('avg_value').AsFloat,
        DS.FieldByName('sample_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM readings_hourly');
  try
    WriteLn(Format('   %d hourly aggregation records created', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 5: Daily Aggregation ===
{ Rolls up hourly aggregations into daily min/max/avg/count records per sensor and displays the daily summaries. }
procedure Demo5_DailyAggregation;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Daily Aggregation ===');
  WriteLn;
  WriteLn('   Computing daily rollup from hourly data...');
  WriteLn;

  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO readings_daily (sensor_id, bucket_date, min_value, max_value, avg_value, sample_count) ' +
    'SELECT sensor_id, ' +
    '  substr(bucket_hour, 1, 10) as bucket_date, ' +
    '  MIN(min_value) as min_value, ' +
    '  MAX(max_value) as max_value, ' +
    '  AVG(avg_value) as avg_value, ' +
    '  SUM(sample_count) as sample_count ' +
    'FROM readings_hourly ' +
    'GROUP BY sensor_id, substr(bucket_hour, 1, 10)');

  WriteLn(Format('   %-10s %-12s %8s %8s %8s %6s', ['Sensor', 'Date', 'Min', 'Max', 'Avg', 'Samples']));
  WriteLn('   ' + StringOfChar('-', 58));

  DS := Conn.ExecuteQuery(
    'SELECT * FROM readings_daily ORDER BY sensor_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-12s %8.2f %8.2f %8.2f %6d', [
        DS.FieldByName('sensor_id').AsString,
        DS.FieldByName('bucket_date').AsString,
        DS.FieldByName('min_value').AsFloat,
        DS.FieldByName('max_value').AsFloat,
        DS.FieldByName('avg_value').AsFloat,
        DS.FieldByName('sample_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// === Demo 6: Retention Policy Execution ===
{ Displays configured retention policies, then simulates enforcement by deleting raw readings older than a reference time and reporting before/after counts. }
procedure Demo6_RetentionPolicy;
var
  DS: TDataSet;
  CountBefore, CountAfter: Integer;
begin
  WriteLn('=== 6. Retention Policy Execution ===');
  WriteLn;

  WriteLn('   Configured policies:');
  DS := Conn.ExecuteQuery('SELECT * FROM retention_policies ORDER BY retention_hours');
  try
    WriteLn(Format('   %-18s %8s  %s', ['Table', 'Hours', 'Description']));
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %8d  %s', [
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('retention_hours').AsInteger,
        DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Simulating retention for raw data (keeping only hours 10-11):');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM readings_raw');
  try
    CountBefore := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Delete readings from hours 08-09 (simulating 24h retention with reference time at hour 10)
  Conn.ExecuteNonQuery(
    'DELETE FROM readings_raw WHERE timestamp < ''2025-01-20 10:00:00''');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM readings_raw');
  try
    CountAfter := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Before: %d raw records', [CountBefore]));
  WriteLn(Format('   After:  %d raw records (deleted %d expired)', [CountAfter, CountBefore - CountAfter]));
  WriteLn;
  WriteLn('   Note: Hourly and daily aggregations preserved (longer retention)');
  WriteLn;
end;

// === Demo 7: Time-Range Queries ===
{ Queries the latest reading per sensor and displays the TEMP-001 hourly trend with min/avg/max values and a visual bar chart. }
procedure Demo7_TimeRangeQueries;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Time-Range Queries ===');
  WriteLn;

  // Query 1: Latest readings per sensor
  WriteLn('   Latest reading per sensor:');
  WriteLn(Format('   %-10s %-22s %10s %-4s', ['Sensor', 'Timestamp', 'Value', 'Unit']));
  WriteLn('   ' + StringOfChar('-', 52));

  DS := Conn.ExecuteQuery(
    'SELECT r.sensor_id, r.timestamp, r.value, s.unit ' +
    'FROM readings_raw r ' +
    'JOIN sensors s ON s.sensor_id = r.sensor_id ' +
    'WHERE r.id IN (SELECT MAX(id) FROM readings_raw GROUP BY sensor_id) ' +
    'ORDER BY r.sensor_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-22s %10.2f %-4s', [
        DS.FieldByName('sensor_id').AsString,
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('value').AsFloat,
        DS.FieldByName('unit').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Query 2: Hourly trend for TEMP-001
  WriteLn('   TEMP-001 hourly trend:');
  WriteLn(Format('   %-14s %6s %6s %6s  %s', ['Hour', 'Min', 'Avg', 'Max', 'Trend']));
  WriteLn('   ' + StringOfChar('-', 52));

  DS := Conn.ExecuteQuery(
    'SELECT bucket_hour, min_value, avg_value, max_value ' +
    'FROM readings_hourly WHERE sensor_id = ''TEMP-001'' ORDER BY bucket_hour');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %6.1f %6.1f %6.1f  %s', [
        Copy(DS.FieldByName('bucket_hour').AsString, 12, 5),
        DS.FieldByName('min_value').AsFloat,
        DS.FieldByName('avg_value').AsFloat,
        DS.FieldByName('max_value').AsFloat,
        StringOfChar('#', Round((DS.FieldByName('avg_value').AsFloat - 20.0) * 3))]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 8: Ingestion Performance ===
{ Displays ingestion log entries with batch IDs, record counts, and timing, then computes total records, batch count, and throughput in records per second. }
procedure Demo8_IngestionPerformance;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Ingestion Performance Metrics ===');
  WriteLn;
  WriteLn(Format('   %-12s %-8s %8s %-22s %6s', ['Batch', 'Sensor', 'Records', 'Time', 'ms']));
  WriteLn('   ' + StringOfChar('-', 62));

  DS := Conn.ExecuteQuery('SELECT * FROM ingestion_log ORDER BY ingested_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-8s %8d %-22s %6d', [
        DS.FieldByName('batch_id').AsString,
        DS.FieldByName('sensor_id').AsString,
        DS.FieldByName('record_count').AsInteger,
        DS.FieldByName('ingested_at').AsString,
        DS.FieldByName('duration_ms').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT SUM(record_count) as total_records, ' +
    '  SUM(duration_ms) as total_ms, ' +
    '  COUNT(*) as batch_count ' +
    'FROM ingestion_log');
  try
    WriteLn(Format('   Total: %d records in %d batches, %d ms total ingestion time',
      [DS.FieldByName('total_records').AsInteger,
       DS.FieldByName('batch_count').AsInteger,
       DS.FieldByName('total_ms').AsInteger]));
    if DS.FieldByName('total_ms').AsInteger > 0 then
      WriteLn(Format('   Throughput: %.0f records/sec',
        [DS.FieldByName('total_records').AsFloat / DS.FieldByName('total_ms').AsFloat * 1000]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 9: Storage Analysis ===
{ Displays record counts, retention periods, granularity, and estimated storage size for each tier (raw, hourly, daily) and computes the compression ratio from raw to hourly. }
procedure Demo9_StorageAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Storage Tier Analysis ===');
  WriteLn;
  WriteLn(Format('   %-18s %8s %10s %12s %10s', ['Table', 'Records', 'Retention', 'Granularity', 'Est. KB']));
  WriteLn('   ' + StringOfChar('-', 64));

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM readings_raw');
  try
    WriteLn(Format('   %-18s %8d %10s %12s %10.1f', ['readings_raw', DS.FieldByName('cnt').AsInteger, '24 hours', '5 seconds', DS.FieldByName('cnt').AsInteger * 0.08]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM readings_hourly');
  try
    WriteLn(Format('   %-18s %8d %10s %12s %10.1f', ['readings_hourly', DS.FieldByName('cnt').AsInteger, '30 days', '1 hour', DS.FieldByName('cnt').AsInteger * 0.06]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM readings_daily');
  try
    WriteLn(Format('   %-18s %8d %10s %12s %10.1f', ['readings_daily', DS.FieldByName('cnt').AsInteger, '1 year', '1 day', DS.FieldByName('cnt').AsInteger * 0.06]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Data compression ratio (raw -> hourly):');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM readings_raw) as raw_count, ' +
    '  (SELECT COUNT(*) FROM readings_hourly) as hourly_count');
  try
    if DS.FieldByName('hourly_count').AsInteger > 0 then
      WriteLn(Format('     %d raw records -> %d hourly records (%.1fx reduction)',
        [DS.FieldByName('raw_count').AsInteger,
         DS.FieldByName('hourly_count').AsInteger,
         DS.FieldByName('raw_count').AsFloat / DS.FieldByName('hourly_count').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 10: Summary Statistics ===
{ Displays daily min/max/avg and sample counts per sensor, then shows per-sensor data quality metrics (min, max, and average quality scores). }
procedure Demo10_SummaryStats;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Sensor Summary Statistics ===');
  WriteLn;
  WriteLn(Format('   %-10s %-14s %8s %8s %8s %6s', ['Sensor', 'Type', 'Day Min', 'Day Max', 'Day Avg', 'Samp.']));
  WriteLn('   ' + StringOfChar('-', 62));

  DS := Conn.ExecuteQuery(
    'SELECT d.sensor_id, s.sensor_type, d.min_value, d.max_value, d.avg_value, d.sample_count ' +
    'FROM readings_daily d ' +
    'JOIN sensors s ON s.sensor_id = d.sensor_id ' +
    'ORDER BY d.sensor_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-14s %8.2f %8.2f %8.2f %6d', [
        DS.FieldByName('sensor_id').AsString,
        DS.FieldByName('sensor_type').AsString,
        DS.FieldByName('min_value').AsFloat,
        DS.FieldByName('max_value').AsFloat,
        DS.FieldByName('avg_value').AsFloat,
        DS.FieldByName('sample_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Data quality
  WriteLn('   Data quality overview:');
  DS := Conn.ExecuteQuery(
    'SELECT sensor_id, ' +
    '  MIN(quality) as min_q, MAX(quality) as max_q, ' +
    '  ROUND(AVG(quality), 1) as avg_q ' +
    'FROM readings_raw GROUP BY sensor_id ORDER BY sensor_id');
  try
    WriteLn(Format('   %-10s %6s %6s %6s', ['Sensor', 'MinQ', 'MaxQ', 'AvgQ']));
    WriteLn('   ' + StringOfChar('-', 34));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %6d %6d %6.1f', [
        DS.FieldByName('sensor_id').AsString,
        DS.FieldByName('min_q').AsInteger,
        DS.FieldByName('max_q').AsInteger,
        DS.FieldByName('avg_q').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Main ===
begin
  WriteLn('Example 135: Sensor Data Store - High-Frequency Ingestion, Aggregation, Retention');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    RegisterSensors;

    Demo1_SensorRegistry;
    Demo2_BatchIngestion;
    Demo3_RawDataSample;
    Demo4_HourlyAggregation;
    Demo5_DailyAggregation;
    Demo6_RetentionPolicy;
    Demo7_TimeRangeQueries;
    Demo8_IngestionPerformance;
    Demo9_StorageAnalysis;
    Demo10_SummaryStats;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
