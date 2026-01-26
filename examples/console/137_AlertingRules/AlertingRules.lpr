{===============================================================================
  NDXSQLite Example 137 - Alerting Rules
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Alert rule definition and configuration
  - Metric-based alert generation
  - Alert lifecycle (fire, acknowledge, resolve)
  - Escalation rules and notification
  - Suppression windows for maintenance

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program AlertingRules;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Alert rules
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS alert_rules (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  rule_name TEXT NOT NULL UNIQUE,' +
    '  metric_name TEXT NOT NULL,' +
    '  condition TEXT NOT NULL,' +       // gt, lt, gte, lte, eq
    '  threshold REAL NOT NULL,' +
    '  severity TEXT NOT NULL,' +         // info, warning, critical, emergency
    '  target TEXT NOT NULL,' +           // which system/service
    '  cooldown_sec INTEGER NOT NULL DEFAULT 300,' +
    '  enabled INTEGER NOT NULL DEFAULT 1,' +
    '  description TEXT' +
    ')'
  );

  // Metric readings (incoming data)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS metrics (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  metric_name TEXT NOT NULL,' +
    '  target TEXT NOT NULL,' +
    '  value REAL NOT NULL,' +
    '  recorded_at TEXT NOT NULL' +
    ')'
  );

  // Alerts (generated when rules trigger)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS alerts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  rule_id INTEGER NOT NULL,' +
    '  target TEXT NOT NULL,' +
    '  severity TEXT NOT NULL,' +
    '  metric_value REAL NOT NULL,' +
    '  threshold REAL NOT NULL,' +
    '  state TEXT NOT NULL DEFAULT ''open'',' +    // open, acknowledged, resolved, suppressed
    '  opened_at TEXT NOT NULL,' +
    '  acknowledged_at TEXT,' +
    '  acknowledged_by TEXT,' +
    '  resolved_at TEXT,' +
    '  resolved_by TEXT,' +
    '  escalated INTEGER NOT NULL DEFAULT 0,' +
    '  FOREIGN KEY(rule_id) REFERENCES alert_rules(id)' +
    ')'
  );

  // Escalation rules
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS escalation_rules (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  from_severity TEXT NOT NULL,' +
    '  to_severity TEXT NOT NULL,' +
    '  after_minutes INTEGER NOT NULL,' +
    '  notify_channel TEXT NOT NULL,' +
    '  description TEXT' +
    ')'
  );

  // Suppression windows (maintenance)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS suppression_windows (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  window_name TEXT NOT NULL,' +
    '  target_pattern TEXT NOT NULL,' +   // target to suppress (supports %)
    '  start_time TEXT NOT NULL,' +
    '  end_time TEXT NOT NULL,' +
    '  reason TEXT,' +
    '  created_by TEXT NOT NULL' +
    ')'
  );

  // Alert history log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS alert_history (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  alert_id INTEGER NOT NULL,' +
    '  action TEXT NOT NULL,' +          // opened, acknowledged, escalated, resolved, suppressed
    '  performed_by TEXT,' +
    '  performed_at TEXT NOT NULL,' +
    '  notes TEXT,' +
    '  FOREIGN KEY(alert_id) REFERENCES alerts(id)' +
    ')'
  );
end;

{ Inserts alert rules for CPU, memory, disk, latency, and error rate thresholds, plus escalation rules and suppression maintenance windows. }
procedure SeedRules;
begin
  // Alert rules for different metrics and severities
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''cpu_warning'', ''cpu_load'', ''gt'', 70.0, ''warning'', ''server-prod-01'', 300, ''CPU load above 70%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''cpu_critical'', ''cpu_load'', ''gt'', 90.0, ''critical'', ''server-prod-01'', 60, ''CPU load above 90%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''memory_warning'', ''memory_pct'', ''gt'', 80.0, ''warning'', ''server-prod-01'', 300, ''Memory usage above 80%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''memory_critical'', ''memory_pct'', ''gt'', 95.0, ''critical'', ''server-prod-01'', 60, ''Memory usage above 95%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''disk_warning'', ''disk_usage_pct'', ''gt'', 85.0, ''warning'', ''server-prod-01'', 600, ''Disk usage above 85%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''latency_warning'', ''response_ms'', ''gt'', 500.0, ''warning'', ''api-gateway'', 120, ''API response above 500ms'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''latency_critical'', ''response_ms'', ''gt'', 2000.0, ''critical'', ''api-gateway'', 30, ''API response above 2000ms'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''error_rate'', ''error_rate_pct'', ''gt'', 5.0, ''critical'', ''api-gateway'', 60, ''Error rate above 5%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''disk_space_low'', ''disk_free_gb'', ''lt'', 10.0, ''emergency'', ''db-primary'', 30, ''Disk space below 10GB'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_rules (rule_name, metric_name, condition, threshold, severity, target, cooldown_sec, description) ' +
    'VALUES (''conn_pool'', ''connections'', ''gt'', 90.0, ''warning'', ''db-primary'', 120, ''Connection pool above 90%'')');

  // Escalation rules
  Conn.ExecuteNonQuery('INSERT INTO escalation_rules (from_severity, to_severity, after_minutes, notify_channel, description) ' +
    'VALUES (''warning'', ''critical'', 30, ''pager'', ''Warning unacknowledged for 30 min'')');
  Conn.ExecuteNonQuery('INSERT INTO escalation_rules (from_severity, to_severity, after_minutes, notify_channel, description) ' +
    'VALUES (''critical'', ''emergency'', 15, ''phone'', ''Critical unacknowledged for 15 min'')');
  Conn.ExecuteNonQuery('INSERT INTO escalation_rules (from_severity, to_severity, after_minutes, notify_channel, description) ' +
    'VALUES (''emergency'', ''emergency'', 5, ''all_hands'', ''Emergency unresolved for 5 min'')');

  // Suppression windows
  Conn.ExecuteNonQuery('INSERT INTO suppression_windows (window_name, target_pattern, start_time, end_time, reason, created_by) ' +
    'VALUES (''Prod Maintenance'', ''server-prod%'', ''2025-01-20 02:00:00'', ''2025-01-20 04:00:00'', ''Scheduled kernel update'', ''ops-team'')');
  Conn.ExecuteNonQuery('INSERT INTO suppression_windows (window_name, target_pattern, start_time, end_time, reason, created_by) ' +
    'VALUES (''DB Migration'', ''db-%'', ''2025-01-20 03:00:00'', ''2025-01-20 03:30:00'', ''Schema migration v2.5'', ''dba-team'')');
end;

{ Inserts metric readings that trigger threshold violations, creates corresponding alert records with various states, and populates the alert history log. }
procedure GenerateMetricsAndAlerts;
begin
  // Simulate metrics that trigger various rules
  // Time period: 2025-01-20 10:00 - 11:00

  // Normal readings (no alerts)
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''cpu_load'', ''server-prod-01'', 45.0, ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''memory_pct'', ''server-prod-01'', 62.0, ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''response_ms'', ''api-gateway'', 120.0, ''2025-01-20 10:00:00'')');

  // CPU spike (triggers warning at 10:15, critical at 10:20)
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''cpu_load'', ''server-prod-01'', 75.0, ''2025-01-20 10:15:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''cpu_load'', ''server-prod-01'', 82.0, ''2025-01-20 10:18:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''cpu_load'', ''server-prod-01'', 93.0, ''2025-01-20 10:20:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''cpu_load'', ''server-prod-01'', 88.0, ''2025-01-20 10:25:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''cpu_load'', ''server-prod-01'', 55.0, ''2025-01-20 10:35:00'')');

  // Memory creep (triggers warning at 10:30)
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''memory_pct'', ''server-prod-01'', 78.0, ''2025-01-20 10:20:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''memory_pct'', ''server-prod-01'', 83.0, ''2025-01-20 10:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''memory_pct'', ''server-prod-01'', 96.0, ''2025-01-20 10:40:00'')');

  // API latency spike (triggers warning, then critical)
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''response_ms'', ''api-gateway'', 650.0, ''2025-01-20 10:22:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''response_ms'', ''api-gateway'', 2500.0, ''2025-01-20 10:25:00'')');
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''response_ms'', ''api-gateway'', 180.0, ''2025-01-20 10:40:00'')');

  // Disk space on DB (triggers emergency)
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''disk_free_gb'', ''db-primary'', 8.5, ''2025-01-20 10:45:00'')');

  // Error rate spike
  Conn.ExecuteNonQuery('INSERT INTO metrics (metric_name, target, value, recorded_at) VALUES (''error_rate_pct'', ''api-gateway'', 7.2, ''2025-01-20 10:25:00'')');

  // Generate alerts based on threshold crossings
  // Alert 1: CPU warning at 10:15
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at) ' +
    'VALUES (1, ''server-prod-01'', ''warning'', 75.0, 70.0, ''resolved'', ''2025-01-20 10:15:00'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (1, ''opened'', ''2025-01-20 10:15:00'', ''CPU load 75% > threshold 70%'')');

  // Alert 2: CPU critical at 10:20
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at, acknowledged_at, acknowledged_by, resolved_at, resolved_by) ' +
    'VALUES (2, ''server-prod-01'', ''critical'', 93.0, 90.0, ''resolved'', ''2025-01-20 10:20:00'', ''2025-01-20 10:22:00'', ''oncall-engineer'', ''2025-01-20 10:35:00'', ''oncall-engineer'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (2, ''opened'', NULL, ''2025-01-20 10:20:00'', ''CPU load 93% > threshold 90%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (2, ''acknowledged'', ''oncall-engineer'', ''2025-01-20 10:22:00'', ''Investigating high CPU'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (2, ''resolved'', ''oncall-engineer'', ''2025-01-20 10:35:00'', ''Killed runaway process'')');

  // Alert 3: Memory warning at 10:30
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at, acknowledged_at, acknowledged_by) ' +
    'VALUES (3, ''server-prod-01'', ''warning'', 83.0, 80.0, ''acknowledged'', ''2025-01-20 10:30:00'', ''2025-01-20 10:45:00'', ''sre-team'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (3, ''opened'', ''2025-01-20 10:30:00'', ''Memory 83% > threshold 80%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (3, ''acknowledged'', ''sre-team'', ''2025-01-20 10:45:00'', ''Monitoring memory trend'')');

  // Alert 4: Memory critical at 10:40
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at, escalated) ' +
    'VALUES (4, ''server-prod-01'', ''critical'', 96.0, 95.0, ''open'', ''2025-01-20 10:40:00'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (4, ''opened'', ''2025-01-20 10:40:00'', ''Memory 96% > threshold 95%'')');

  // Alert 5: Latency warning at 10:22
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at, resolved_at, resolved_by) ' +
    'VALUES (6, ''api-gateway'', ''warning'', 650.0, 500.0, ''resolved'', ''2025-01-20 10:22:00'', ''2025-01-20 10:40:00'', ''auto-resolve'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (5, ''opened'', ''2025-01-20 10:22:00'', ''Response 650ms > threshold 500ms'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (5, ''resolved'', ''auto-resolve'', ''2025-01-20 10:40:00'', ''Latency returned to normal'')');

  // Alert 6: Latency critical at 10:25
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at, acknowledged_at, acknowledged_by, resolved_at, resolved_by, escalated) ' +
    'VALUES (7, ''api-gateway'', ''critical'', 2500.0, 2000.0, ''resolved'', ''2025-01-20 10:25:00'', ''2025-01-20 10:26:00'', ''oncall-engineer'', ''2025-01-20 10:40:00'', ''oncall-engineer'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (6, ''opened'', ''2025-01-20 10:25:00'', ''Response 2500ms > threshold 2000ms'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (6, ''acknowledged'', ''oncall-engineer'', ''2025-01-20 10:26:00'', ''Checking API gateway'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (6, ''resolved'', ''oncall-engineer'', ''2025-01-20 10:40:00'', ''Gateway restarted'')');

  // Alert 7: Error rate at 10:25
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at, resolved_at, resolved_by) ' +
    'VALUES (8, ''api-gateway'', ''critical'', 7.2, 5.0, ''resolved'', ''2025-01-20 10:25:00'', ''2025-01-20 10:38:00'', ''auto-resolve'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (7, ''opened'', ''2025-01-20 10:25:00'', ''Error rate 7.2% > threshold 5%'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (7, ''resolved'', ''auto-resolve'', ''2025-01-20 10:38:00'', ''Error rate normalized'')');

  // Alert 8: Disk emergency at 10:45
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at, escalated) ' +
    'VALUES (9, ''db-primary'', ''emergency'', 8.5, 10.0, ''open'', ''2025-01-20 10:45:00'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (8, ''opened'', ''2025-01-20 10:45:00'', ''Disk free 8.5GB < threshold 10GB'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (8, ''escalated'', ''2025-01-20 10:50:00'', ''Emergency unresolved for 5 min, notifying all_hands'')');

  // Alert 1 resolved via auto (CPU returned to normal)
  Conn.ExecuteNonQuery('UPDATE alerts SET resolved_at = ''2025-01-20 10:35:00'', resolved_by = ''auto-resolve'' WHERE id = 1');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_by, performed_at, notes) VALUES (1, ''resolved'', ''auto-resolve'', ''2025-01-20 10:35:00'', ''CPU returned below threshold'')');

  // Simulate suppressed alert during maintenance window (CPU spike at 03:00)
  Conn.ExecuteNonQuery('INSERT INTO alerts (rule_id, target, severity, metric_value, threshold, state, opened_at) ' +
    'VALUES (1, ''server-prod-01'', ''warning'', 78.0, 70.0, ''suppressed'', ''2025-01-20 03:15:00'')');
  Conn.ExecuteNonQuery('INSERT INTO alert_history (alert_id, action, performed_at, notes) VALUES (9, ''suppressed'', ''2025-01-20 03:15:00'', ''Suppressed by window: Prod Maintenance'')');
end;

// === Demo 1: Alert Rules Configuration ===
{ Queries and displays all enabled alert rules showing rule name, metric, condition operator, threshold, severity, and target. }
procedure Demo1_AlertRules;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Alert Rules Configuration ===');
  WriteLn;
  WriteLn(Format('   %-18s %-16s %-4s %8s %-10s %-16s', ['Rule', 'Metric', 'Op', 'Thresh', 'Severity', 'Target']));
  WriteLn('   ' + StringOfChar('-', 80));

  DS := Conn.ExecuteQuery('SELECT * FROM alert_rules WHERE enabled = 1 ORDER BY target, severity');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-16s %-4s %8.1f %-10s %-16s', [
        DS.FieldByName('rule_name').AsString,
        DS.FieldByName('metric_name').AsString,
        DS.FieldByName('condition').AsString,
        DS.FieldByName('threshold').AsFloat,
        DS.FieldByName('severity').AsString,
        DS.FieldByName('target').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM alert_rules WHERE enabled = 1');
  try
    WriteLn;
    WriteLn(Format('   %d active alert rules configured', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 2: Metric Readings ===
{ Displays all ingested metric readings ordered by time, showing metric name, target, value, and timestamp. }
procedure Demo2_MetricReadings;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Incoming Metric Readings ===');
  WriteLn;
  WriteLn(Format('   %-16s %-16s %10s %-22s', ['Metric', 'Target', 'Value', 'Time']));
  WriteLn('   ' + StringOfChar('-', 68));

  DS := Conn.ExecuteQuery('SELECT * FROM metrics ORDER BY recorded_at, metric_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-16s %10.1f %-22s', [
        DS.FieldByName('metric_name').AsString,
        DS.FieldByName('target').AsString,
        DS.FieldByName('value').AsFloat,
        DS.FieldByName('recorded_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM metrics');
  try
    WriteLn;
    WriteLn(Format('   %d metric readings ingested', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 3: Alert Generation ===
{ Lists all generated alerts with their rule name, target, severity, metric value versus threshold, and current state. }
procedure Demo3_AlertGeneration;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Generated Alerts ===');
  WriteLn;
  WriteLn(Format('   %-3s %-18s %-16s %-10s %8s %8s %-12s', ['ID', 'Rule', 'Target', 'Severity', 'Value', 'Thresh', 'State']));
  WriteLn('   ' + StringOfChar('-', 82));

  DS := Conn.ExecuteQuery(
    'SELECT a.id, r.rule_name, a.target, a.severity, a.metric_value, a.threshold, a.state ' +
    'FROM alerts a JOIN alert_rules r ON r.id = a.rule_id ' +
    'ORDER BY a.opened_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-3d %-18s %-16s %-10s %8.1f %8.1f %-12s', [
        DS.FieldByName('id').AsInteger,
        DS.FieldByName('rule_name').AsString,
        DS.FieldByName('target').AsString,
        DS.FieldByName('severity').AsString,
        DS.FieldByName('metric_value').AsFloat,
        DS.FieldByName('threshold').AsFloat,
        DS.FieldByName('state').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM alerts');
  try
    WriteLn;
    WriteLn(Format('   %d alerts generated from threshold violations', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 4: Alert Lifecycle ===
{ Displays the full history of alert #2 (CPU critical) showing its progression through open, acknowledged, and resolved states with timestamps and notes. }
procedure Demo4_AlertLifecycle;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Alert State Lifecycle ===');
  WriteLn;
  WriteLn('   Alert #2 (CPU critical) lifecycle:');
  WriteLn;
  WriteLn(Format('   %-14s %-16s %-22s %s', ['Action', 'By', 'Time', 'Notes']));
  WriteLn('   ' + StringOfChar('-', 80));

  DS := Conn.ExecuteQuery(
    'SELECT action, COALESCE(performed_by, ''system'') as by_whom, performed_at, notes ' +
    'FROM alert_history WHERE alert_id = 2 ORDER BY performed_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %-16s %-22s %s', [
        DS.FieldByName('action').AsString,
        DS.FieldByName('by_whom').AsString,
        DS.FieldByName('performed_at').AsString,
        DS.FieldByName('notes').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   State flow: open -> acknowledged -> resolved');
  WriteLn('   Time to acknowledge: 2 min | Time to resolve: 15 min');
  WriteLn;
end;

// === Demo 5: Escalation Rules ===
{ Displays configured escalation rules (severity transitions, timeouts, notification channels) and lists all alerts that have been escalated. }
procedure Demo5_EscalationRules;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Escalation Rules ===');
  WriteLn;
  WriteLn(Format('   %-12s -> %-12s  After %5s  Channel    Description', ['From', 'To', 'Min']));
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery('SELECT * FROM escalation_rules ORDER BY after_minutes');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s -> %-12s  After %3d m  %-10s %s', [
        DS.FieldByName('from_severity').AsString,
        DS.FieldByName('to_severity').AsString,
        DS.FieldByName('after_minutes').AsInteger,
        DS.FieldByName('notify_channel').AsString,
        DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Show escalated alerts
  WriteLn('   Escalated alerts:');
  DS := Conn.ExecuteQuery(
    'SELECT a.id, r.rule_name, a.severity, a.state, a.opened_at ' +
    'FROM alerts a JOIN alert_rules r ON r.id = a.rule_id ' +
    'WHERE a.escalated = 1');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - Alert #%d (%s): %s, state=%s, opened %s', [
        DS.FieldByName('id').AsInteger,
        DS.FieldByName('rule_name').AsString,
        DS.FieldByName('severity').AsString,
        DS.FieldByName('state').AsString,
        DS.FieldByName('opened_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 6: Suppression Windows ===
{ Displays configured maintenance suppression windows with target patterns and time ranges, then lists alerts that were suppressed during those windows. }
procedure Demo6_SuppressionWindows;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Suppression Windows (Maintenance) ===');
  WriteLn;
  WriteLn(Format('   %-18s %-14s %-22s %-22s %s', ['Window', 'Pattern', 'Start', 'End', 'Reason']));
  WriteLn('   ' + StringOfChar('-', 92));

  DS := Conn.ExecuteQuery('SELECT * FROM suppression_windows ORDER BY start_time');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-14s %-22s %-22s %s', [
        DS.FieldByName('window_name').AsString,
        DS.FieldByName('target_pattern').AsString,
        DS.FieldByName('start_time').AsString,
        DS.FieldByName('end_time').AsString,
        DS.FieldByName('reason').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Suppressed alerts (fired during maintenance):');
  DS := Conn.ExecuteQuery(
    'SELECT a.id, r.rule_name, a.target, a.opened_at ' +
    'FROM alerts a JOIN alert_rules r ON r.id = a.rule_id ' +
    'WHERE a.state = ''suppressed''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - Alert #%d (%s) on %s at %s -> SUPPRESSED', [
        DS.FieldByName('id').AsInteger,
        DS.FieldByName('rule_name').AsString,
        DS.FieldByName('target').AsString,
        DS.FieldByName('opened_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 7: Active Alerts Dashboard ===
{ Displays currently open or acknowledged alerts sorted by severity, showing target, state, metric value, and escalation status. }
procedure Demo7_ActiveDashboard;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Active Alerts Dashboard ===');
  WriteLn;
  WriteLn('   Currently open/acknowledged alerts (requires action):');
  WriteLn;
  WriteLn(Format('   %-3s %-10s %-16s %-10s %8s  %-22s %s', ['ID', 'Severity', 'Target', 'State', 'Value', 'Opened', 'Escalated']));
  WriteLn('   ' + StringOfChar('-', 88));

  DS := Conn.ExecuteQuery(
    'SELECT a.id, a.severity, a.target, a.state, a.metric_value, a.opened_at, a.escalated ' +
    'FROM alerts a ' +
    'WHERE a.state IN (''open'', ''acknowledged'') ' +
    'ORDER BY CASE a.severity ' +
    '  WHEN ''emergency'' THEN 1 WHEN ''critical'' THEN 2 ' +
    '  WHEN ''warning'' THEN 3 ELSE 4 END, a.opened_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-3d %-10s %-16s %-10s %8.1f  %-22s %s', [
        DS.FieldByName('id').AsInteger,
        DS.FieldByName('severity').AsString,
        DS.FieldByName('target').AsString,
        DS.FieldByName('state').AsString,
        DS.FieldByName('metric_value').AsFloat,
        DS.FieldByName('opened_at').AsString,
        BoolToStr(DS.FieldByName('escalated').AsInteger = 1, 'YES', 'no')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 8: Alert History Timeline ===
{ Demonstrates the alert history timeline with actions and performers. }
procedure Demo8_Timeline;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Alert History Timeline ===');
  WriteLn;
  WriteLn(Format('   %-22s %-3s %-14s %-16s %s', ['Time', '#', 'Action', 'By', 'Notes']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery(
    'SELECT performed_at, alert_id, action, COALESCE(performed_by, ''system'') as by_whom, ' +
    '  SUBSTR(notes, 1, 35) as short_notes ' +
    'FROM alert_history ORDER BY performed_at, alert_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-3d %-14s %-16s %s', [
        DS.FieldByName('performed_at').AsString,
        DS.FieldByName('alert_id').AsInteger,
        DS.FieldByName('action').AsString,
        DS.FieldByName('by_whom').AsString,
        DS.FieldByName('short_notes').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 9: Statistics ===
{ Demonstrates alerting rule statistics and trigger frequency metrics. }
procedure Demo9_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Alert Statistics ===');
  WriteLn;

  // By state
  WriteLn('   By state:');
  DS := Conn.ExecuteQuery('SELECT state, COUNT(*) as cnt FROM alerts GROUP BY state ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d', [DS.FieldByName('state').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // By severity
  WriteLn('   By severity:');
  DS := Conn.ExecuteQuery(
    'SELECT severity, COUNT(*) as cnt FROM alerts GROUP BY severity ' +
    'ORDER BY CASE severity WHEN ''emergency'' THEN 1 WHEN ''critical'' THEN 2 WHEN ''warning'' THEN 3 ELSE 4 END');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d', [DS.FieldByName('severity').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // By target
  WriteLn('   By target:');
  DS := Conn.ExecuteQuery('SELECT target, COUNT(*) as cnt FROM alerts GROUP BY target ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-16s %d', [DS.FieldByName('target').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Resolution metrics
  WriteLn('   Resolution metrics (resolved alerts):');
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt, ' +
    '  SUM(CASE WHEN resolved_by = ''auto-resolve'' THEN 1 ELSE 0 END) as auto_resolved, ' +
    '  SUM(CASE WHEN resolved_by <> ''auto-resolve'' THEN 1 ELSE 0 END) as manual_resolved ' +
    'FROM alerts WHERE state = ''resolved''');
  try
    WriteLn(Format('     Total resolved: %d', [DS.FieldByName('cnt').AsInteger]));
    WriteLn(Format('     Auto-resolved: %d', [DS.FieldByName('auto_resolved').AsInteger]));
    WriteLn(Format('     Manual-resolved: %d', [DS.FieldByName('manual_resolved').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Demo 10: Rule Effectiveness ===
{ Displays per-rule statistics (fire count, resolved, escalated, suppressed) and lists rules that have never triggered. }
procedure Demo10_RuleEffectiveness;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Rule Effectiveness ===');
  WriteLn;
  WriteLn(Format('   %-18s %6s %8s %10s %12s', ['Rule', 'Fires', 'Resolved', 'Escalated', 'Suppressed']));
  WriteLn('   ' + StringOfChar('-', 60));

  DS := Conn.ExecuteQuery(
    'SELECT r.rule_name, ' +
    '  COUNT(a.id) as fires, ' +
    '  SUM(CASE WHEN a.state = ''resolved'' THEN 1 ELSE 0 END) as resolved, ' +
    '  SUM(a.escalated) as escalated, ' +
    '  SUM(CASE WHEN a.state = ''suppressed'' THEN 1 ELSE 0 END) as suppressed ' +
    'FROM alert_rules r ' +
    'LEFT JOIN alerts a ON a.rule_id = r.id ' +
    'GROUP BY r.id, r.rule_name ' +
    'HAVING COUNT(a.id) > 0 ' +
    'ORDER BY fires DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %6d %8d %10d %12d', [
        DS.FieldByName('rule_name').AsString,
        DS.FieldByName('fires').AsInteger,
        DS.FieldByName('resolved').AsInteger,
        DS.FieldByName('escalated').AsInteger,
        DS.FieldByName('suppressed').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Rules with 0 fires (no violations detected):');
  DS := Conn.ExecuteQuery(
    'SELECT r.rule_name, r.metric_name, r.threshold ' +
    'FROM alert_rules r ' +
    'LEFT JOIN alerts a ON a.rule_id = r.id ' +
    'GROUP BY r.id ' +
    'HAVING COUNT(a.id) = 0 ' +
    'ORDER BY r.rule_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%s > %.1f)', [
        DS.FieldByName('rule_name').AsString,
        DS.FieldByName('metric_name').AsString,
        DS.FieldByName('threshold').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Main ===
begin
  WriteLn('Example 137: Alerting Rules - Threshold Monitoring, Escalation, Suppression');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    SeedRules;
    GenerateMetricsAndAlerts;

    Demo1_AlertRules;
    Demo2_MetricReadings;
    Demo3_AlertGeneration;
    Demo4_AlertLifecycle;
    Demo5_EscalationRules;
    Demo6_SuppressionWindows;
    Demo7_ActiveDashboard;
    Demo8_Timeline;
    Demo9_Statistics;
    Demo10_RuleEffectiveness;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
