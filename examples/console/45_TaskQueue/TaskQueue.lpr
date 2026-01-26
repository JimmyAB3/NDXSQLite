{===============================================================================
  NDXSQLite Example 45 - Task Queue Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - SQLite as a job/task queue
  - Enqueue and dequeue operations
  - Priority-based processing
  - Retry logic with exponential backoff
  - Dead-letter queue for failed tasks
  - Worker claiming and heartbeats

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program TaskQueue;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Math,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

const
  STATUS_PENDING = 'pending';
  STATUS_PROCESSING = 'processing';
  STATUS_COMPLETED = 'completed';
  STATUS_FAILED = 'failed';
  STATUS_DEAD = 'dead';

  PRIORITY_LOW = 1;
  PRIORITY_NORMAL = 5;
  PRIORITY_HIGH = 10;
  PRIORITY_CRITICAL = 100;

  MAX_RETRIES = 3;

{ Creates the tasks, dead_letter_queue, and task_history tables with indexes on status, queue/priority, scheduled time, and worker. }
procedure SetupTaskQueue;
begin
  // Main task queue table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS tasks (' +
    '  id INTEGER PRIMARY KEY,' +
    '  queue_name TEXT NOT NULL DEFAULT ''default'',' +
    '  task_type TEXT NOT NULL,' +
    '  payload TEXT,' +                      // JSON payload
    '  priority INTEGER DEFAULT 5,' +        // Higher = more important
    '  status TEXT DEFAULT ''pending'',' +
    '  retry_count INTEGER DEFAULT 0,' +
    '  max_retries INTEGER DEFAULT 3,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  scheduled_at TEXT DEFAULT (datetime(''now'')),' +  // For delayed tasks
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  worker_id TEXT,' +                    // Which worker claimed it
    '  last_heartbeat TEXT,' +
    '  error_message TEXT,' +
    '  result TEXT' +                        // JSON result
    ')');

  // Dead-letter queue for permanently failed tasks
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS dead_letter_queue (' +
    '  id INTEGER PRIMARY KEY,' +
    '  original_task_id INTEGER,' +
    '  queue_name TEXT,' +
    '  task_type TEXT,' +
    '  payload TEXT,' +
    '  error_message TEXT,' +
    '  retry_count INTEGER,' +
    '  created_at TEXT,' +
    '  failed_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Task execution history
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS task_history (' +
    '  id INTEGER PRIMARY KEY,' +
    '  task_id INTEGER,' +
    '  status TEXT,' +
    '  worker_id TEXT,' +
    '  message TEXT,' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes for efficient queue operations
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_tasks_queue ON tasks(queue_name, status, priority DESC, scheduled_at)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_tasks_scheduled ON tasks(scheduled_at)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_tasks_worker ON tasks(worker_id, status)');
end;

// ============================================================================
// Queue operations
// ============================================================================

{ Inserts a new task into the specified queue with the given type, payload, priority, and optional delay, returning its ID. }
function Enqueue(const AQueueName, ATaskType, APayload: string;
  APriority: Integer = PRIORITY_NORMAL; ADelaySeconds: Integer = 0): Int64;
var
  DS: TDataSet;
  ScheduledAt: string;
begin
  if ADelaySeconds > 0 then
    ScheduledAt := FormatDateTime('yyyy-mm-dd hh:nn:ss',
      IncSecond(Now, ADelaySeconds))
  else
    ScheduledAt := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Connection.ExecuteNonQuery(
    'INSERT INTO tasks (queue_name, task_type, payload, priority, scheduled_at) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [AQueueName, ATaskType, APayload, APriority, ScheduledAt]);

  DS := Connection.ExecuteQuery('SELECT last_insert_rowid()');
  try
    Result := DS.Fields[0].AsLargeInt;
  finally
    DS.Free;
  end;
end;

{ Dequeues the next available task by priority and returns its ID. }
function Dequeue(const AQueueName, AWorkerId: string): Int64;
var
  DS: TDataSet;
begin
  Result := -1;

  Connection.BeginTransaction;
  try
    // Get next available task (highest priority, oldest scheduled)
    DS := Connection.ExecuteQuery(
      'SELECT id FROM tasks ' +
      'WHERE queue_name = ? AND status = ? AND scheduled_at <= datetime(''now'') ' +
      'ORDER BY priority DESC, scheduled_at ASC LIMIT 1',
      [AQueueName, STATUS_PENDING]);
    try
      if not DS.EOF then
        Result := DS.FieldByName('id').AsLargeInt;
    finally
      DS.Free;
    end;

    if Result > 0 then
    begin
      // Claim the task
      Connection.ExecuteNonQuery(
        'UPDATE tasks SET status = ?, worker_id = ?, started_at = datetime(''now''), ' +
        'last_heartbeat = datetime(''now'') WHERE id = ?',
        [STATUS_PROCESSING, AWorkerId, Result]);

      // Log history
      Connection.ExecuteNonQuery(
        'INSERT INTO task_history (task_id, status, worker_id, message) VALUES (?, ?, ?, ?)',
        [Result, STATUS_PROCESSING, AWorkerId, 'Task claimed by worker']);
    end;

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;
end;

{ Marks a task as completed with a timestamp and result payload, and logs the completion in task_history. }
procedure CompleteTask(ATaskId: Int64; const AResult: string);
begin
  Connection.ExecuteNonQuery(
    'UPDATE tasks SET status = ?, completed_at = datetime(''now''), result = ? WHERE id = ?',
    [STATUS_COMPLETED, AResult, ATaskId]);

  Connection.ExecuteNonQuery(
    'INSERT INTO task_history (task_id, status, message) VALUES (?, ?, ?)',
    [ATaskId, STATUS_COMPLETED, 'Task completed successfully']);
end;

{ Handles task failure by either rescheduling with exponential backoff if retries remain, or moving the task to the dead-letter queue. }
procedure FailTask(ATaskId: Int64; const AErrorMessage: string);
var
  DS: TDataSet;
  RetryCount, MaxRetries: Integer;
  NextRetryDelay: Integer;
begin
  // Get current retry info
  DS := Connection.ExecuteQuery(
    'SELECT retry_count, max_retries, queue_name, task_type, payload, created_at FROM tasks WHERE id = ?',
    [ATaskId]);
  try
    RetryCount := DS.FieldByName('retry_count').AsInteger;
    MaxRetries := DS.FieldByName('max_retries').AsInteger;

    if RetryCount < MaxRetries then
    begin
      // Retry with exponential backoff
      NextRetryDelay := Round(Power(2, RetryCount) * 10);  // 10, 20, 40 seconds...
      Connection.ExecuteNonQuery(
        'UPDATE tasks SET status = ?, retry_count = retry_count + 1, ' +
        'error_message = ?, scheduled_at = datetime(''now'', ''+' + IntToStr(NextRetryDelay) + ' seconds''), ' +
        'worker_id = NULL, started_at = NULL WHERE id = ?',
        [STATUS_PENDING, AErrorMessage, ATaskId]);

      Connection.ExecuteNonQuery(
        'INSERT INTO task_history (task_id, status, message) VALUES (?, ?, ?)',
        [ATaskId, STATUS_FAILED, 'Retry ' + IntToStr(RetryCount + 1) + ': ' + AErrorMessage]);
    end
    else
    begin
      // Move to dead-letter queue
      Connection.ExecuteNonQuery(
        'INSERT INTO dead_letter_queue (original_task_id, queue_name, task_type, payload, ' +
        'error_message, retry_count, created_at) ' +
        'SELECT id, queue_name, task_type, payload, ?, retry_count, created_at FROM tasks WHERE id = ?',
        [AErrorMessage, ATaskId]);

      Connection.ExecuteNonQuery(
        'UPDATE tasks SET status = ?, error_message = ? WHERE id = ?',
        [STATUS_DEAD, AErrorMessage, ATaskId]);

      Connection.ExecuteNonQuery(
        'INSERT INTO task_history (task_id, status, message) VALUES (?, ?, ?)',
        [ATaskId, STATUS_DEAD, 'Moved to dead-letter queue: ' + AErrorMessage]);
    end;
  finally
    DS.Free;
  end;
end;

{ Updates the last_heartbeat timestamp for a task to indicate the worker is still actively processing it. }
procedure Heartbeat(ATaskId: Int64);
begin
  Connection.ExecuteNonQuery(
    'UPDATE tasks SET last_heartbeat = datetime(''now'') WHERE id = ?',
    [ATaskId]);
end;

// ============================================================================
// Demo procedures
// ============================================================================

{ Enqueues several tasks, then dequeues and completes them one by one using a single worker, displaying progress counts. }
procedure DemoBasicQueue;
var
  TaskId: Int64;
  DS: TDataSet;
begin
  WriteLn('1. Basic queue operations');
  WriteLn('   ----------------------');
  WriteLn('');

  // Enqueue some tasks
  WriteLn('   Enqueueing tasks...');
  Enqueue('default', 'send_email', '{"to": "user1@example.com", "subject": "Welcome"}');
  Enqueue('default', 'send_email', '{"to": "user2@example.com", "subject": "Newsletter"}');
  Enqueue('default', 'process_image', '{"file": "photo.jpg", "resize": "800x600"}');
  Enqueue('default', 'generate_report', '{"type": "monthly", "format": "pdf"}');

  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM tasks WHERE status = ?', [STATUS_PENDING]);
  try
    WriteLn('   Pending tasks: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');

  // Dequeue and process
  WriteLn('   Processing tasks (worker-1)...');
  TaskId := Dequeue('default', 'worker-1');
  while TaskId > 0 do
  begin
    DS := Connection.ExecuteQuery('SELECT task_type, payload FROM tasks WHERE id = ?', [TaskId]);
    try
      WriteLn('     Processing: ', DS.FieldByName('task_type').AsString);
    finally
      DS.Free;
    end;

    // Simulate work
    CompleteTask(TaskId, '{"status": "success"}');

    TaskId := Dequeue('default', 'worker-1');
  end;

  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM tasks WHERE status = ?', [STATUS_COMPLETED]);
  try
    WriteLn('   Completed tasks: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Enqueues tasks with LOW, NORMAL, HIGH, and CRITICAL priorities, then dequeues them to show highest-priority-first processing order. }
procedure DemoPriorityQueue;
var
  TaskId: Int64;
  DS: TDataSet;
begin
  WriteLn('2. Priority-based processing');
  WriteLn('   -------------------------');
  WriteLn('');

  // Enqueue tasks with different priorities
  WriteLn('   Enqueueing tasks with priorities...');
  Enqueue('priority_queue', 'low_task', '{"data": "low"}', PRIORITY_LOW);
  Enqueue('priority_queue', 'normal_task', '{"data": "normal"}', PRIORITY_NORMAL);
  Enqueue('priority_queue', 'high_task', '{"data": "high"}', PRIORITY_HIGH);
  Enqueue('priority_queue', 'critical_task', '{"data": "critical"}', PRIORITY_CRITICAL);
  Enqueue('priority_queue', 'another_normal', '{"data": "normal2"}', PRIORITY_NORMAL);

  WriteLn('');

  // Show queue order
  WriteLn('   Processing order (by priority):');
  TaskId := Dequeue('priority_queue', 'worker-2');
  while TaskId > 0 do
  begin
    DS := Connection.ExecuteQuery('SELECT task_type, priority FROM tasks WHERE id = ?', [TaskId]);
    try
      WriteLn('     ', DS.FieldByName('task_type').AsString,
              ' (priority: ', DS.FieldByName('priority').AsInteger, ')');
    finally
      DS.Free;
    end;
    CompleteTask(TaskId, '{}');
    TaskId := Dequeue('priority_queue', 'worker-2');
  end;

  WriteLn('');
end;

{ Enqueues tasks with future scheduled_at times and displays which are immediately ready versus still waiting. }
procedure DemoDelayedTasks;
var
  DS: TDataSet;
begin
  WriteLn('3. Delayed/scheduled tasks');
  WriteLn('   -----------------------');
  WriteLn('');

  // Enqueue delayed tasks
  WriteLn('   Scheduling tasks for future execution...');
  Enqueue('scheduled', 'immediate_task', '{}', PRIORITY_NORMAL, 0);
  Enqueue('scheduled', 'delayed_5s', '{}', PRIORITY_NORMAL, 5);
  Enqueue('scheduled', 'delayed_10s', '{}', PRIORITY_NORMAL, 10);

  DS := Connection.ExecuteQuery(
    'SELECT task_type, scheduled_at, ' +
    '       CASE WHEN scheduled_at <= datetime(''now'') THEN ''ready'' ELSE ''waiting'' END as status ' +
    'FROM tasks WHERE queue_name = ''scheduled'' ORDER BY scheduled_at');
  try
    WriteLn('   Task             Scheduled At          Status');
    WriteLn('   ----             ------------          ------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('task_type').AsString:16,
              DS.FieldByName('scheduled_at').AsString:22,
              DS.FieldByName('status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Simulates repeated task failures to show exponential backoff rescheduling and eventual movement to the dead-letter queue after max retries. }
procedure DemoRetryLogic;
var
  TaskId: Int64;
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('4. Retry logic with exponential backoff');
  WriteLn('   ------------------------------------');
  WriteLn('');

  // Enqueue a task that will fail
  TaskId := Enqueue('retry_queue', 'flaky_task', '{"will_fail": true}');
  WriteLn('   Created task #', TaskId, ' with max_retries = 3');
  WriteLn('');

  // Simulate multiple failures
  for I := 1 to 4 do
  begin
    // Claim the task
    Connection.ExecuteNonQuery(
      'UPDATE tasks SET status = ?, worker_id = ?, started_at = datetime(''now''), ' +
      'scheduled_at = datetime(''now'') WHERE id = ?',
      [STATUS_PROCESSING, 'worker-3', TaskId]);

    WriteLn('   Attempt ', I, ': Processing task...');
    FailTask(TaskId, 'Simulated failure #' + IntToStr(I));

    DS := Connection.ExecuteQuery('SELECT status, retry_count, scheduled_at FROM tasks WHERE id = ?', [TaskId]);
    try
      WriteLn('     Status: ', DS.FieldByName('status').AsString,
              ', Retries: ', DS.FieldByName('retry_count').AsInteger);
      if DS.FieldByName('status').AsString = STATUS_PENDING then
        WriteLn('     Next attempt at: ', DS.FieldByName('scheduled_at').AsString);
    finally
      DS.Free;
    end;
    WriteLn('');
  end;

  // Check dead-letter queue
  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM dead_letter_queue');
  try
    WriteLn('   Tasks in dead-letter queue: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Enqueues tasks into separate named queues (emails, notifications, reports) and displays a per-queue summary of total, pending, and completed counts. }
procedure DemoMultipleQueues;
var
  DS: TDataSet;
begin
  WriteLn('5. Multiple named queues');
  WriteLn('   ---------------------');
  WriteLn('');

  // Enqueue to different queues
  Enqueue('emails', 'send_welcome', '{}');
  Enqueue('emails', 'send_newsletter', '{}');
  Enqueue('notifications', 'push_notification', '{}');
  Enqueue('notifications', 'sms_alert', '{}');
  Enqueue('reports', 'daily_report', '{}');

  WriteLn('   Queue summary:');
  DS := Connection.ExecuteQuery(
    'SELECT queue_name, COUNT(*) as count, ' +
    '       SUM(CASE WHEN status = ''pending'' THEN 1 ELSE 0 END) as pending, ' +
    '       SUM(CASE WHEN status = ''completed'' THEN 1 ELSE 0 END) as completed ' +
    'FROM tasks GROUP BY queue_name ORDER BY queue_name');
  try
    WriteLn('   Queue           Total  Pending  Completed');
    WriteLn('   -----           -----  -------  ---------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('queue_name').AsString:15,
              DS.FieldByName('count').AsInteger:6,
              DS.FieldByName('pending').AsInteger:8,
              DS.FieldByName('completed').AsInteger:10);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays the most recent task_history entries showing task ID, type, status transitions, and timestamps. }
procedure DemoTaskHistory;
var
  DS: TDataSet;
begin
  WriteLn('6. Task execution history');
  WriteLn('   ----------------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT h.task_id, t.task_type, h.status, h.worker_id, h.message, h.timestamp ' +
    'FROM task_history h ' +
    'JOIN tasks t ON h.task_id = t.id ' +
    'ORDER BY h.timestamp DESC LIMIT 10');
  try
    WriteLn('   Recent task history:');
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('timestamp').AsString, '] ',
              'Task #', DS.FieldByName('task_id').AsString, ' (',
              DS.FieldByName('task_type').AsString, '): ',
              DS.FieldByName('status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays overall task counts by status (pending, processing, completed, failed, dead) and per-type completion counts. }
procedure DemoQueueStats;
var
  DS: TDataSet;
begin
  WriteLn('7. Queue statistics');
  WriteLn('   ----------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  COUNT(*) as total, ' +
    '  SUM(CASE WHEN status = ''pending'' THEN 1 ELSE 0 END) as pending, ' +
    '  SUM(CASE WHEN status = ''processing'' THEN 1 ELSE 0 END) as processing, ' +
    '  SUM(CASE WHEN status = ''completed'' THEN 1 ELSE 0 END) as completed, ' +
    '  SUM(CASE WHEN status = ''failed'' THEN 1 ELSE 0 END) as failed, ' +
    '  SUM(CASE WHEN status = ''dead'' THEN 1 ELSE 0 END) as dead ' +
    'FROM tasks');
  try
    WriteLn('   Overall statistics:');
    WriteLn('     Total tasks:     ', DS.FieldByName('total').AsInteger);
    WriteLn('     Pending:         ', DS.FieldByName('pending').AsInteger);
    WriteLn('     Processing:      ', DS.FieldByName('processing').AsInteger);
    WriteLn('     Completed:       ', DS.FieldByName('completed').AsInteger);
    WriteLn('     Failed (retry):  ', DS.FieldByName('failed').AsInteger);
    WriteLn('     Dead:            ', DS.FieldByName('dead').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT task_type, COUNT(*) as count, ' +
    '       AVG(CASE WHEN completed_at IS NOT NULL THEN ' +
    '           (julianday(completed_at) - julianday(started_at)) * 86400 END) as avg_duration ' +
    'FROM tasks WHERE status = ''completed'' GROUP BY task_type');
  try
    WriteLn('   Completed tasks by type:');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('task_type').AsString:20,
              ' - ', DS.FieldByName('count').AsInteger, ' completed');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Lists all entries in the dead_letter_queue with their original task ID, type, error message, and failure timestamp. }
procedure DemoDeadLetterRecovery;
var
  DS: TDataSet;
begin
  WriteLn('8. Dead-letter queue management');
  WriteLn('   ----------------------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT id, original_task_id, task_type, error_message, failed_at ' +
    'FROM dead_letter_queue ORDER BY failed_at DESC');
  try
    WriteLn('   Dead-letter queue contents:');
    if DS.EOF then
      WriteLn('     (empty)')
    else
      while not DS.EOF do
      begin
        WriteLn('     ID: ', DS.FieldByName('id').AsInteger,
                ' (original #', DS.FieldByName('original_task_id').AsString, ')');
        WriteLn('       Type: ', DS.FieldByName('task_type').AsString);
        WriteLn('       Error: ', DS.FieldByName('error_message').AsString);
        WriteLn('       Failed: ', DS.FieldByName('failed_at').AsString);
        DS.Next;
      end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   To retry a dead-letter task:');
  WriteLn('   INSERT INTO tasks (queue_name, task_type, payload)');
  WriteLn('   SELECT queue_name, task_type, payload FROM dead_letter_queue WHERE id = ?;');
  WriteLn('');
end;

{ Deletes the example database file and its WAL and SHM companions if they exist. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  WriteLn('=== NDXSQLite Example 45: Task Queue Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example45.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTaskQueue;
      WriteLn('Task queue database initialized');
      WriteLn('');

      DemoBasicQueue;
      DemoPriorityQueue;
      DemoDelayedTasks;
      DemoRetryLogic;
      DemoMultipleQueues;
      DemoTaskHistory;
      DemoQueueStats;
      DemoDeadLetterRecovery;

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
