{===============================================================================
  NDXSQLite Example 120 - Job Scheduler
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Cron-like job scheduling with intervals
  - Job execution with concurrency control
  - Failed job retry with backoff
  - Job dependency management
  - Execution history and monitoring

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program JobScheduler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Jobs table - defines scheduled jobs
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS jobs (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  job_name TEXT NOT NULL,' +
    '  job_type TEXT NOT NULL,' +
    '  schedule_type TEXT NOT NULL,' +  // interval, daily, hourly, weekly
    '  interval_seconds INTEGER DEFAULT 0,' +
    '  schedule_hour INTEGER DEFAULT -1,' +  // for daily: hour to run (0-23)
    '  schedule_minute INTEGER DEFAULT 0,' +
    '  schedule_day_of_week INTEGER DEFAULT -1,' +  // for weekly: 0=Sun..6=Sat
    '  payload TEXT DEFAULT '''',' +
    '  max_retries INTEGER DEFAULT 3,' +
    '  retry_backoff_base INTEGER DEFAULT 2,' +  // exponential backoff base in seconds
    '  concurrency_group TEXT DEFAULT ''default'',' +
    '  max_concurrent INTEGER DEFAULT 2,' +
    '  status TEXT DEFAULT ''active'',' +  // active, paused, disabled
    '  next_run_at TEXT,' +
    '  last_run_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(job_name)' +
    ')'
  );

  // Job executions - history of each run
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS job_executions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  job_id INTEGER NOT NULL,' +
    '  job_name TEXT NOT NULL,' +
    '  status TEXT NOT NULL,' +  // running, completed, failed, retrying
    '  attempt INTEGER DEFAULT 1,' +
    '  started_at TEXT DEFAULT (datetime(''now'')),' +
    '  completed_at TEXT,' +
    '  duration_ms INTEGER,' +
    '  result_data TEXT,' +
    '  error_message TEXT,' +
    '  next_retry_at TEXT' +
    ')'
  );

  // Dead letter queue - permanently failed jobs
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS dead_letter_queue (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  job_id INTEGER NOT NULL,' +
    '  job_name TEXT NOT NULL,' +
    '  job_type TEXT NOT NULL,' +
    '  payload TEXT,' +
    '  attempts INTEGER,' +
    '  last_error TEXT,' +
    '  failed_at TEXT DEFAULT (datetime(''now'')),' +
    '  acknowledged INTEGER DEFAULT 0,' +
    '  acknowledged_at TEXT' +
    ')'
  );

  // Concurrency tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS concurrency_slots (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  concurrency_group TEXT NOT NULL,' +
    '  job_id INTEGER NOT NULL,' +
    '  execution_id INTEGER NOT NULL,' +
    '  acquired_at TEXT DEFAULT (datetime(''now'')),' +
    '  released_at TEXT' +
    ')'
  );
end;

{ Computes the next scheduled run time based on schedule type (interval, hourly,
  daily, or weekly) and the corresponding time parameters. }
function CalculateNextRun(const ScheduleType: string; IntervalSeconds: Integer;
  ScheduleHour, ScheduleMinute, ScheduleDayOfWeek: Integer;
  const FromTime: string): string;
var
  BaseTime: TDateTime;
  NextTime: TDateTime;
  CurrentHour, CurrentMinute: Word;
  DayOffset: Integer;
  CurrentDOW: Integer;
begin
  if FromTime <> '' then
    BaseTime := StrToDateTime(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
  else
    BaseTime := Now;

  // Use Now as the base for all calculations
  BaseTime := Now;

  if ScheduleType = 'interval' then
  begin
    NextTime := BaseTime + (IntervalSeconds / 86400.0);
  end
  else if ScheduleType = 'hourly' then
  begin
    // Next occurrence at ScheduleMinute past the hour
    DecodeTime(BaseTime, CurrentHour, CurrentMinute, PWord(@CurrentHour)^, PWord(@CurrentHour)^);
    if CurrentMinute >= ScheduleMinute then
      NextTime := Trunc(BaseTime) + (CurrentHour + 1) / 24.0 + ScheduleMinute / 1440.0
    else
      NextTime := Trunc(BaseTime) + CurrentHour / 24.0 + ScheduleMinute / 1440.0;
  end
  else if ScheduleType = 'daily' then
  begin
    // Next occurrence at ScheduleHour:ScheduleMinute
    NextTime := Trunc(BaseTime) + ScheduleHour / 24.0 + ScheduleMinute / 1440.0;
    if NextTime <= BaseTime then
      NextTime := NextTime + 1.0;  // tomorrow
  end
  else if ScheduleType = 'weekly' then
  begin
    // Next occurrence on ScheduleDayOfWeek at ScheduleHour:ScheduleMinute
    CurrentDOW := DayOfWeek(BaseTime) - 1; // 0=Sun..6=Sat
    DayOffset := ScheduleDayOfWeek - CurrentDOW;
    if DayOffset <= 0 then
      DayOffset := DayOffset + 7;
    NextTime := Trunc(BaseTime) + DayOffset + ScheduleHour / 24.0 + ScheduleMinute / 1440.0;
  end
  else
  begin
    // Default: 1 hour from now
    NextTime := BaseTime + (3600 / 86400.0);
  end;

  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', NextTime);
end;

{ Inserts a new job definition with schedule type, timing parameters, retry settings,
  concurrency group, and calculates the initial next_run_at timestamp. }
procedure RegisterJob(const JobName, JobType, ScheduleType: string;
  IntervalSeconds, ScheduleHour, ScheduleMinute, ScheduleDayOfWeek: Integer;
  const Payload, ConcurrencyGroup: string; MaxRetries, MaxConcurrent: Integer);
var
  NextRun: string;
begin
  NextRun := CalculateNextRun(ScheduleType, IntervalSeconds,
    ScheduleHour, ScheduleMinute, ScheduleDayOfWeek, '');

  Conn.ExecuteNonQuery(Format(
    'INSERT INTO jobs (job_name, job_type, schedule_type, interval_seconds, ' +
    'schedule_hour, schedule_minute, schedule_day_of_week, payload, ' +
    'max_retries, concurrency_group, max_concurrent, next_run_at) ' +
    'VALUES (''%s'', ''%s'', ''%s'', %d, %d, %d, %d, ''%s'', %d, ''%s'', %d, ''%s'')',
    [JobName, JobType, ScheduleType, IntervalSeconds,
     ScheduleHour, ScheduleMinute, ScheduleDayOfWeek, Payload,
     MaxRetries, ConcurrencyGroup, MaxConcurrent, NextRun]));
end;

{ Checks if a concurrency slot is available in the job's group; if so, creates
  an execution record and acquires the slot, returning the execution ID. }
function AcquireConcurrencySlot(const ConcurrencyGroup: string;
  JobId, MaxConcurrent: Integer): Integer;
var
  DS: TDataSet;
  ActiveCount: Integer;
  ExecutionId: Integer;
begin
  Result := -1;

  // Count active slots in this group
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM concurrency_slots ' +
    'WHERE concurrency_group = ''%s'' AND released_at IS NULL',
    [ConcurrencyGroup]));
  try
    ActiveCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  if ActiveCount >= MaxConcurrent then
    Exit;

  // Create execution record first
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO job_executions (job_id, job_name, status, attempt) ' +
    'VALUES (%d, (SELECT job_name FROM jobs WHERE id = %d), ''running'', 1)',
    [JobId, JobId]));

  // Get the execution ID
  DS := Conn.ExecuteQuery('SELECT last_insert_rowid() as eid');
  try
    ExecutionId := DS.FieldByName('eid').AsInteger;
  finally
    DS.Free;
  end;

  // Acquire slot
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO concurrency_slots (concurrency_group, job_id, execution_id) ' +
    'VALUES (''%s'', %d, %d)',
    [ConcurrencyGroup, JobId, ExecutionId]));

  Result := ExecutionId;
end;

{ Marks a concurrency slot as released by setting its released_at timestamp. }
procedure ReleaseConcurrencySlot(ExecutionId: Integer);
begin
  Conn.ExecuteNonQuery(Format(
    'UPDATE concurrency_slots SET released_at = datetime(''now'') ' +
    'WHERE execution_id = %d AND released_at IS NULL',
    [ExecutionId]));
end;

{ Simulates running a job based on its type and returns a JSON result with
  type-specific data (emails sent, reports generated, records cleaned, etc.). }
function SimulateJobExecution(const JobName, JobType: string; ShouldFail: Boolean): string;
begin
  // Simulate job execution
  if ShouldFail then
    Result := ''
  else
  begin
    if JobType = 'email' then
      Result := Format('{"sent": true, "job": "%s", "recipients": 150}', [JobName])
    else if JobType = 'report' then
      Result := Format('{"generated": true, "job": "%s", "pages": 42}', [JobName])
    else if JobType = 'cleanup' then
      Result := Format('{"cleaned": true, "job": "%s", "records": 1024}', [JobName])
    else if JobType = 'sync' then
      Result := Format('{"synced": true, "job": "%s", "items": 256}', [JobName])
    else if JobType = 'backup' then
      Result := Format('{"backed_up": true, "job": "%s", "size_mb": 512}', [JobName])
    else
      Result := Format('{"completed": true, "job": "%s"}', [JobName]);
  end;
end;

{ Executes a scheduled job after acquiring a concurrency slot, records the
  execution result, updates the job's last_run and next_run timestamps, and
  releases the slot. }
procedure ExecuteJob(JobId: Integer; ShouldFail: Boolean);
var
  DS: TDataSet;
  JobName, JobType, ConcurrencyGroup, ScheduleType: string;
  MaxRetries, MaxConcurrent, IntervalSeconds: Integer;
  ScheduleHour, ScheduleMinute, ScheduleDayOfWeek: Integer;
  ExecutionId: Integer;
  ResultData, NextRun: string;
begin
  // Get job details
  DS := Conn.ExecuteQuery(Format(
    'SELECT * FROM jobs WHERE id = %d', [JobId]));
  try
    if DS.IsEmpty then Exit;
    JobName := DS.FieldByName('job_name').AsString;
    JobType := DS.FieldByName('job_type').AsString;
    ConcurrencyGroup := DS.FieldByName('concurrency_group').AsString;
    MaxRetries := DS.FieldByName('max_retries').AsInteger;
    MaxConcurrent := DS.FieldByName('max_concurrent').AsInteger;
    ScheduleType := DS.FieldByName('schedule_type').AsString;
    IntervalSeconds := DS.FieldByName('interval_seconds').AsInteger;
    ScheduleHour := DS.FieldByName('schedule_hour').AsInteger;
    ScheduleMinute := DS.FieldByName('schedule_minute').AsInteger;
    ScheduleDayOfWeek := DS.FieldByName('schedule_day_of_week').AsInteger;
  finally
    DS.Free;
  end;

  // Try to acquire concurrency slot
  ExecutionId := AcquireConcurrencySlot(ConcurrencyGroup, JobId, MaxConcurrent);
  if ExecutionId < 0 then
  begin
    WriteLn(Format('   [BLOCKED] Job "%s" - concurrency limit reached for group "%s"',
      [JobName, ConcurrencyGroup]));
    Exit;
  end;

  // Execute the job
  ResultData := SimulateJobExecution(JobName, JobType, ShouldFail);

  if ResultData <> '' then
  begin
    // Success
    Conn.ExecuteNonQuery(Format(
      'UPDATE job_executions SET status = ''completed'', ' +
      'completed_at = datetime(''now''), duration_ms = %d, ' +
      'result_data = ''%s'' WHERE id = %d',
      [50 + Random(200), ResultData, ExecutionId]));

    // Update job next_run and last_run
    NextRun := CalculateNextRun(ScheduleType, IntervalSeconds,
      ScheduleHour, ScheduleMinute, ScheduleDayOfWeek, '');
    Conn.ExecuteNonQuery(Format(
      'UPDATE jobs SET last_run_at = datetime(''now''), next_run_at = ''%s'' WHERE id = %d',
      [NextRun, JobId]));

    WriteLn(Format('   [OK] Job "%s" completed. Result: %s', [JobName, ResultData]));
  end
  else
  begin
    // Failure
    Conn.ExecuteNonQuery(Format(
      'UPDATE job_executions SET status = ''failed'', ' +
      'completed_at = datetime(''now''), duration_ms = %d, ' +
      'error_message = ''Simulated failure for %s'' WHERE id = %d',
      [20 + Random(50), JobName, ExecutionId]));

    WriteLn(Format('   [FAIL] Job "%s" failed.', [JobName]));
  end;

  // Release concurrency slot
  ReleaseConcurrencySlot(ExecutionId);
end;

{ Retries a failed job with exponential backoff; on success records completion,
  on failure either schedules another retry or moves the job to the dead letter
  queue if max retries are exhausted. }
procedure RetryFailedJob(JobId: Integer; Attempt: Integer; ShouldSucceed: Boolean);
var
  DS: TDataSet;
  JobName, JobType, ConcurrencyGroup: string;
  MaxRetries, MaxConcurrent: Integer;
  BackoffBase, BackoffSeconds: Integer;
  ExecutionId: Integer;
  ResultData, NextRetryAt: string;
begin
  // Get job details
  DS := Conn.ExecuteQuery(Format(
    'SELECT * FROM jobs WHERE id = %d', [JobId]));
  try
    if DS.IsEmpty then Exit;
    JobName := DS.FieldByName('job_name').AsString;
    JobType := DS.FieldByName('job_type').AsString;
    ConcurrencyGroup := DS.FieldByName('concurrency_group').AsString;
    MaxRetries := DS.FieldByName('max_retries').AsInteger;
    MaxConcurrent := DS.FieldByName('max_concurrent').AsInteger;
    BackoffBase := DS.FieldByName('retry_backoff_base').AsInteger;
  finally
    DS.Free;
  end;

  // Calculate exponential backoff
  BackoffSeconds := BackoffBase;
  if Attempt > 1 then
    BackoffSeconds := BackoffBase * (1 shl (Attempt - 1));  // 2^(attempt-1) * base

  WriteLn(Format('   Attempt %d/%d (backoff: %ds)...', [Attempt, MaxRetries, BackoffSeconds]));

  // Create execution record
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO job_executions (job_id, job_name, status, attempt) ' +
    'VALUES (%d, ''%s'', ''retrying'', %d)',
    [JobId, JobName, Attempt]));

  DS := Conn.ExecuteQuery('SELECT last_insert_rowid() as eid');
  try
    ExecutionId := DS.FieldByName('eid').AsInteger;
  finally
    DS.Free;
  end;

  ResultData := SimulateJobExecution(JobName, JobType, not ShouldSucceed);

  if ResultData <> '' then
  begin
    // Retry succeeded
    Conn.ExecuteNonQuery(Format(
      'UPDATE job_executions SET status = ''completed'', ' +
      'completed_at = datetime(''now''), duration_ms = %d, ' +
      'result_data = ''%s'' WHERE id = %d',
      [50 + Random(200), ResultData, ExecutionId]));

    WriteLn(Format('   [OK] Retry succeeded on attempt %d!', [Attempt]));
  end
  else
  begin
    // Retry failed
    if Attempt >= MaxRetries then
    begin
      // Move to dead letter queue
      Conn.ExecuteNonQuery(Format(
        'UPDATE job_executions SET status = ''failed'', ' +
        'completed_at = datetime(''now''), duration_ms = %d, ' +
        'error_message = ''Max retries exhausted'' WHERE id = %d',
        [20 + Random(50), ExecutionId]));

      Conn.ExecuteNonQuery(Format(
        'INSERT INTO dead_letter_queue (job_id, job_name, job_type, payload, attempts, last_error) ' +
        'VALUES (%d, ''%s'', ''%s'', ' +
        '(SELECT payload FROM jobs WHERE id = %d), %d, ''Max retries (%d) exhausted'')',
        [JobId, JobName, JobType, JobId, Attempt, MaxRetries]));

      WriteLn(Format('   [DLQ] Job "%s" moved to dead letter queue after %d attempts.',
        [JobName, Attempt]));
    end
    else
    begin
      // Schedule next retry
      NextRetryAt := FormatDateTime('yyyy-mm-dd hh:nn:ss',
        Now + (BackoffSeconds / 86400.0));
      Conn.ExecuteNonQuery(Format(
        'UPDATE job_executions SET status = ''failed'', ' +
        'completed_at = datetime(''now''), duration_ms = %d, ' +
        'error_message = ''Failed attempt %d'', next_retry_at = ''%s'' WHERE id = %d',
        [20 + Random(50), Attempt, NextRetryAt, ExecutionId]));

      WriteLn(Format('   [RETRY] Attempt %d failed, next retry at %s',
        [Attempt, NextRetryAt]));
    end;
  end;
end;

// ============================================================
// Demo Sections
// ============================================================

{ Registers five jobs with various schedule types (daily, hourly, interval, weekly)
  and different concurrency groups, then prints the scheduled job list with next
  run times. }
procedure Demo1_RegisterAndScheduleJobs;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Register and Schedule Jobs ===');
  WriteLn;

  // Register various jobs with different schedules
  RegisterJob('daily-report', 'report', 'daily',
    0, 6, 0, -1,   // daily at 06:00
    '{"report_type": "sales"}', 'reports', 3, 2);
  WriteLn('   Registered: daily-report (daily at 06:00)');

  RegisterJob('hourly-sync', 'sync', 'hourly',
    0, -1, 30, -1,  // every hour at :30
    '{"source": "crm"}', 'sync', 3, 1);
  WriteLn('   Registered: hourly-sync (hourly at :30)');

  RegisterJob('cleanup-task', 'cleanup', 'interval',
    1800, -1, -1, -1,  // every 30 minutes
    '{"older_than_days": 30}', 'maintenance', 2, 2);
  WriteLn('   Registered: cleanup-task (every 30 min)');

  RegisterJob('weekly-backup', 'backup', 'weekly',
    0, 2, 0, 0,   // Sunday at 02:00
    '{"full_backup": true}', 'backups', 5, 1);
  WriteLn('   Registered: weekly-backup (Sunday at 02:00)');

  RegisterJob('email-digest', 'email', 'daily',
    0, 8, 0, -1,   // daily at 08:00
    '{"template": "digest"}', 'emails', 3, 2);
  WriteLn('   Registered: email-digest (daily at 08:00)');

  // Show scheduled jobs
  WriteLn;
  WriteLn('   Scheduled jobs:');
  DS := Conn.ExecuteQuery(
    'SELECT job_name, schedule_type, next_run_at, concurrency_group FROM jobs ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-10s next: %s  group: %s',
        [DS.FieldByName('job_name').AsString,
         DS.FieldByName('schedule_type').AsString,
         DS.FieldByName('next_run_at').AsString,
         DS.FieldByName('concurrency_group').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Executes all five registered jobs successfully, acquiring concurrency slots
  and recording completed execution results. }
procedure Demo2_ExecuteJobs;
begin
  WriteLn('=== 2. Execute Jobs (First Run) ===');
  WriteLn;

  ExecuteJob(1, False);  // daily-report: success
  ExecuteJob(2, False);  // hourly-sync: success
  ExecuteJob(3, False);  // cleanup-task: success
  ExecuteJob(4, False);  // weekly-backup: success
  ExecuteJob(5, False);  // email-digest: success

  WriteLn;
end;

{ Shows the hourly-sync job failing twice with exponential backoff (2s, 4s) then
  succeeding on the third attempt (8s backoff). }
procedure Demo3_RetryWithBackoff;
begin
  WriteLn('=== 3. Retry with Exponential Backoff ===');
  WriteLn;

  WriteLn('   Job "hourly-sync" fails and retries with backoff:');
  // Simulate job failing, then retries
  RetryFailedJob(2, 1, False);  // Attempt 1: fail (backoff 2s)
  RetryFailedJob(2, 2, False);  // Attempt 2: fail (backoff 4s)
  RetryFailedJob(2, 3, True);   // Attempt 3: succeed (backoff 8s)

  WriteLn;
end;

{ Shows the cleanup-task failing all retry attempts (max_retries=2) and being
  moved to the dead letter queue, then displays the DLQ contents. }
procedure Demo4_DeadLetterQueue;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Dead Letter Queue (Max Retries Exhausted) ===');
  WriteLn;

  WriteLn('   Job "cleanup-task" fails all retries (max_retries=2):');
  RetryFailedJob(3, 1, False);  // Attempt 1: fail
  RetryFailedJob(3, 2, False);  // Attempt 2: fail -> DLQ

  WriteLn;
  WriteLn('   Dead letter queue contents:');
  DS := Conn.ExecuteQuery(
    'SELECT job_name, job_type, attempts, last_error, failed_at FROM dead_letter_queue');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Job: %s (%s), Attempts: %d, Error: %s',
        [DS.FieldByName('job_name').AsString,
         DS.FieldByName('job_type').AsString,
         DS.FieldByName('attempts').AsInteger,
         DS.FieldByName('last_error').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Adds two more report jobs to the reports group (max_concurrent=2), runs two
  that fill both slots, then shows the third being blocked until slots free up. }
procedure Demo5_ConcurrencyLimit;
begin
  WriteLn('=== 5. Concurrency Limit ===');
  WriteLn;

  WriteLn('   Group "reports" has max_concurrent=2:');

  // Register two more report jobs in same concurrency group
  RegisterJob('monthly-report', 'report', 'interval',
    3600, -1, -1, -1,
    '{"report_type": "monthly"}', 'reports', 3, 2);
  RegisterJob('adhoc-report', 'report', 'interval',
    7200, -1, -1, -1,
    '{"report_type": "adhoc"}', 'reports', 3, 2);

  // Execute - first two should succeed, third should be blocked
  ExecuteJob(1, False);  // daily-report: slot 1 of 2
  // Simulate slot not released yet by not releasing
  Conn.ExecuteNonQuery(
    'UPDATE concurrency_slots SET released_at = NULL ' +
    'WHERE job_id = 1 AND id = (SELECT MAX(id) FROM concurrency_slots WHERE job_id = 1)');

  ExecuteJob(6, False);  // monthly-report: slot 2 of 2
  Conn.ExecuteNonQuery(
    'UPDATE concurrency_slots SET released_at = NULL ' +
    'WHERE job_id = 6 AND id = (SELECT MAX(id) FROM concurrency_slots WHERE job_id = 6)');

  ExecuteJob(7, False);  // adhoc-report: BLOCKED (2/2 slots used)

  // Release all slots
  Conn.ExecuteNonQuery('UPDATE concurrency_slots SET released_at = datetime(''now'') WHERE released_at IS NULL');

  WriteLn;
end;

{ Queries and prints the full job execution history table showing job name,
  status, attempt number, duration in milliseconds, and any error messages. }
procedure Demo6_ExecutionHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Execution History ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT job_name, status, attempt, duration_ms, ' +
    'COALESCE(error_message, ''-'') as err ' +
    'FROM job_executions ORDER BY id');
  try
    WriteLn(Format('   %-18s %-12s %-8s %-8s %s',
      ['Job', 'Status', 'Attempt', 'Duration', 'Error']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-12s %-8d %-8s %s',
        [DS.FieldByName('job_name').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('attempt').AsInteger,
         VarToStr(DS.FieldByName('duration_ms').Value) + 'ms',
         DS.FieldByName('err').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Shows how each job's next_run_at is recalculated after execution based on its
  schedule type, displaying last_run and next_run timestamps for executed jobs. }
procedure Demo7_NextRunCalculation;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Next Run Calculation ===');
  WriteLn;

  WriteLn('   After execution, next_run is recalculated:');
  DS := Conn.ExecuteQuery(
    'SELECT job_name, schedule_type, interval_seconds, ' +
    'last_run_at, next_run_at FROM jobs WHERE last_run_at IS NOT NULL ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-10s last: %s  next: %s',
        [DS.FieldByName('job_name').AsString,
         DS.FieldByName('schedule_type').AsString,
         DS.FieldByName('last_run_at').AsString,
         DS.FieldByName('next_run_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Pauses the hourly-sync job and disables the cleanup-task, displays all job
  statuses, then resumes hourly-sync to show job lifecycle management. }
procedure Demo8_PauseAndResumeJobs;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Pause and Resume Jobs ===');
  WriteLn;

  // Pause a job
  Conn.ExecuteNonQuery('UPDATE jobs SET status = ''paused'' WHERE job_name = ''hourly-sync''');
  WriteLn('   Paused: hourly-sync');

  // Disable a job
  Conn.ExecuteNonQuery('UPDATE jobs SET status = ''disabled'' WHERE job_name = ''cleanup-task''');
  WriteLn('   Disabled: cleanup-task');

  // Show job statuses
  DS := Conn.ExecuteQuery('SELECT job_name, status FROM jobs ORDER BY id');
  try
    WriteLn;
    WriteLn('   Job statuses:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %s', [DS.FieldByName('job_name').AsString,
        DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Resume job
  Conn.ExecuteNonQuery('UPDATE jobs SET status = ''active'' WHERE job_name = ''hourly-sync''');
  WriteLn;
  WriteLn('   Resumed: hourly-sync');

  WriteLn;
end;

{ Shows the dead letter queue before and after acknowledging an entry, marking
  the cleanup-task failure as reviewed with an acknowledgement timestamp. }
procedure Demo9_AcknowledgeDLQ;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Acknowledge Dead Letter Queue ===');
  WriteLn;

  WriteLn('   Before acknowledgement:');
  DS := Conn.ExecuteQuery(
    'SELECT job_name, attempts, acknowledged FROM dead_letter_queue');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Job: %s, Attempts: %d, Acknowledged: %s',
        [DS.FieldByName('job_name').AsString,
         DS.FieldByName('attempts').AsInteger,
         BoolToStr(DS.FieldByName('acknowledged').AsInteger = 1, 'Yes', 'No')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Acknowledge DLQ entry
  Conn.ExecuteNonQuery(
    'UPDATE dead_letter_queue SET acknowledged = 1, ' +
    'acknowledged_at = datetime(''now'') WHERE job_name = ''cleanup-task''');

  WriteLn;
  WriteLn('   After acknowledging cleanup-task:');
  DS := Conn.ExecuteQuery(
    'SELECT job_name, attempts, acknowledged, acknowledged_at FROM dead_letter_queue');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Job: %s, Acknowledged: %s at %s',
        [DS.FieldByName('job_name').AsString,
         BoolToStr(DS.FieldByName('acknowledged').AsInteger = 1, 'Yes', 'No'),
         VarToStr(DS.FieldByName('acknowledged_at').Value)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Demonstrates job scheduler statistics and execution metrics. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
  TotalExec, CompletedExec, FailedExec, RetryingExec: Integer;
  DLQCount, ActiveJobs, PausedJobs: Integer;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  // Job stats
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM jobs GROUP BY status ORDER BY status');
  try
    WriteLn('   Jobs by status:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d', [DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Execution stats
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM job_executions GROUP BY status ORDER BY status');
  try
    WriteLn;
    WriteLn('   Executions by status:');
    TotalExec := 0;
    CompletedExec := 0;
    FailedExec := 0;
    RetryingExec := 0;
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d', [DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger]));
      if DS.FieldByName('status').AsString = 'completed' then
        CompletedExec := DS.FieldByName('cnt').AsInteger
      else if DS.FieldByName('status').AsString = 'failed' then
        FailedExec := DS.FieldByName('cnt').AsInteger
      else if DS.FieldByName('status').AsString = 'retrying' then
        RetryingExec := DS.FieldByName('cnt').AsInteger;
      TotalExec := TotalExec + DS.FieldByName('cnt').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // DLQ stats
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM dead_letter_queue');
  try
    DLQCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Active/Paused
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM jobs WHERE status = ''active''');
  try
    ActiveJobs := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM jobs WHERE status = ''paused''');
  try
    PausedJobs := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Summary:');
  WriteLn(Format('   Total jobs registered: %d (active: %d, paused: %d)',
    [ActiveJobs + PausedJobs + 1, ActiveJobs, PausedJobs]));
  WriteLn(Format('   Total executions: %d (completed: %d, failed: %d)',
    [TotalExec, CompletedExec, FailedExec]));
  if TotalExec > 0 then
    WriteLn(Format('   Success rate: %.1f%%', [CompletedExec * 100.0 / TotalExec]));
  WriteLn(Format('   Dead letter queue: %d entries', [DLQCount]));

  // Average duration
  DS := Conn.ExecuteQuery(
    'SELECT AVG(duration_ms) as avg_dur FROM job_executions WHERE status = ''completed''');
  try
    if not DS.FieldByName('avg_dur').IsNull then
      WriteLn(Format('   Avg execution duration: %.0fms', [DS.FieldByName('avg_dur').AsFloat]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

// ============================================================
// Main
// ============================================================
begin
  WriteLn('Example 120: Job Scheduler - Cron-like Scheduling, Retry, DLQ, Concurrency');
  WriteLn(StringOfChar('=', 75));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    Randomize;

    Demo1_RegisterAndScheduleJobs;
    Demo2_ExecuteJobs;
    Demo3_RetryWithBackoff;
    Demo4_DeadLetterQueue;
    Demo5_ConcurrencyLimit;
    Demo6_ExecutionHistory;
    Demo7_NextRunCalculation;
    Demo8_PauseAndResumeJobs;
    Demo9_AcknowledgeDLQ;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
