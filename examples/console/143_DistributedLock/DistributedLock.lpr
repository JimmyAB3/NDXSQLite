{===============================================================================
  NDXSQLite Example 143 - Distributed Lock
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Lock acquisition with TTL expiry
  - Lock queue and fair ordering
  - Reentrant lock support
  - Wait-for graph construction
  - Deadlock detection algorithms

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DistributedLock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Named locks registry
  Conn.ExecuteNonQuery(
    'CREATE TABLE locks (' +
    '  lock_name TEXT PRIMARY KEY, ' +
    '  owner TEXT, ' +
    '  acquired_at TEXT, ' +
    '  expires_at TEXT, ' +
    '  reentry_count INTEGER NOT NULL DEFAULT 1, ' +
    '  status TEXT NOT NULL DEFAULT ''free'')');

  // Lock wait queue
  Conn.ExecuteNonQuery(
    'CREATE TABLE lock_queue (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  lock_name TEXT NOT NULL, ' +
    '  requester TEXT NOT NULL, ' +
    '  requested_at TEXT NOT NULL, ' +
    '  priority INTEGER NOT NULL DEFAULT 0, ' +
    '  timeout_at TEXT, ' +
    '  status TEXT NOT NULL DEFAULT ''waiting'', ' +
    '  UNIQUE(lock_name, requester))');

  // Lock operation history
  Conn.ExecuteNonQuery(
    'CREATE TABLE lock_history (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  lock_name TEXT NOT NULL, ' +
    '  owner TEXT NOT NULL, ' +
    '  action TEXT NOT NULL, ' +
    '  details TEXT, ' +
    '  timestamp TEXT NOT NULL)');

  // Wait-for graph edges (for deadlock detection)
  Conn.ExecuteNonQuery(
    'CREATE TABLE wait_for_graph (' +
    '  waiter TEXT NOT NULL, ' +
    '  holder TEXT NOT NULL, ' +
    '  lock_name TEXT NOT NULL, ' +
    '  PRIMARY KEY(waiter, holder, lock_name))');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Register named locks
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''resource:db-primary'', ''node-A'', ''2025-01-20 10:00:00'', ''2025-01-20 10:05:00'', 1, ''held'')');
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''resource:cache-1'', ''node-B'', ''2025-01-20 10:01:00'', ''2025-01-20 10:04:00'', 2, ''held'')');
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''resource:queue-worker'', ''node-A'', ''2025-01-20 10:02:00'', ''2025-01-20 10:07:00'', 1, ''held'')');
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''mutex:user-update'', ''node-C'', ''2025-01-20 10:00:30'', ''2025-01-20 10:02:30'', 1, ''held'')');
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''mutex:order-process'', ''node-B'', ''2025-01-20 10:01:15'', ''2025-01-20 10:06:15'', 1, ''held'')');
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''resource:file-store'', NULL, NULL, NULL, 0, ''free'')');
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''mutex:config-write'', NULL, NULL, NULL, 0, ''free'')');
  Conn.ExecuteNonQuery('INSERT INTO locks (lock_name, owner, acquired_at, expires_at, reentry_count, status) VALUES ' +
    '(''resource:log-rotate'', ''node-C'', ''2025-01-20 09:55:00'', ''2025-01-20 09:58:00'', 1, ''expired'')');

  // Lock queue entries (waiters)
  Conn.ExecuteNonQuery('INSERT INTO lock_queue (lock_name, requester, requested_at, priority, timeout_at, status) VALUES ' +
    '(''resource:db-primary'', ''node-B'', ''2025-01-20 10:00:30'', 0, ''2025-01-20 10:05:30'', ''waiting'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_queue (lock_name, requester, requested_at, priority, timeout_at, status) VALUES ' +
    '(''resource:db-primary'', ''node-C'', ''2025-01-20 10:01:00'', 1, ''2025-01-20 10:06:00'', ''waiting'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_queue (lock_name, requester, requested_at, priority, timeout_at, status) VALUES ' +
    '(''resource:cache-1'', ''node-A'', ''2025-01-20 10:01:30'', 0, ''2025-01-20 10:04:30'', ''waiting'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_queue (lock_name, requester, requested_at, priority, timeout_at, status) VALUES ' +
    '(''mutex:user-update'', ''node-A'', ''2025-01-20 10:01:00'', 0, ''2025-01-20 10:03:00'', ''waiting'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_queue (lock_name, requester, requested_at, priority, timeout_at, status) VALUES ' +
    '(''mutex:order-process'', ''node-C'', ''2025-01-20 10:02:00'', 0, ''2025-01-20 10:07:00'', ''waiting'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_queue (lock_name, requester, requested_at, priority, timeout_at, status) VALUES ' +
    '(''resource:queue-worker'', ''node-B'', ''2025-01-20 10:02:30'', 0, ''2025-01-20 10:07:30'', ''waiting'')');

  // Build wait-for graph from current state
  // node-B waits for node-A (db-primary)
  Conn.ExecuteNonQuery('INSERT INTO wait_for_graph VALUES (''node-B'', ''node-A'', ''resource:db-primary'')');
  // node-C waits for node-A (db-primary)
  Conn.ExecuteNonQuery('INSERT INTO wait_for_graph VALUES (''node-C'', ''node-A'', ''resource:db-primary'')');
  // node-A waits for node-B (cache-1)
  Conn.ExecuteNonQuery('INSERT INTO wait_for_graph VALUES (''node-A'', ''node-B'', ''resource:cache-1'')');
  // node-A waits for node-C (user-update)
  Conn.ExecuteNonQuery('INSERT INTO wait_for_graph VALUES (''node-A'', ''node-C'', ''mutex:user-update'')');
  // node-C waits for node-B (order-process)
  Conn.ExecuteNonQuery('INSERT INTO wait_for_graph VALUES (''node-C'', ''node-B'', ''mutex:order-process'')');
  // node-B waits for node-A (queue-worker)
  Conn.ExecuteNonQuery('INSERT INTO wait_for_graph VALUES (''node-B'', ''node-A'', ''resource:queue-worker'')');

  // Lock history
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:db-primary'', ''node-A'', ''acquire'', ''TTL=300s'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:cache-1'', ''node-B'', ''acquire'', ''TTL=180s'', ''2025-01-20 10:01:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:cache-1'', ''node-B'', ''reenter'', ''count=2'', ''2025-01-20 10:01:30'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:queue-worker'', ''node-A'', ''acquire'', ''TTL=300s'', ''2025-01-20 10:02:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''mutex:user-update'', ''node-C'', ''acquire'', ''TTL=120s'', ''2025-01-20 10:00:30'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''mutex:order-process'', ''node-B'', ''acquire'', ''TTL=300s'', ''2025-01-20 10:01:15'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:log-rotate'', ''node-C'', ''acquire'', ''TTL=180s'', ''2025-01-20 09:55:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:log-rotate'', ''node-C'', ''expired'', ''TTL exceeded'', ''2025-01-20 09:58:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:db-primary'', ''node-B'', ''enqueue'', ''position=1'', ''2025-01-20 10:00:30'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:db-primary'', ''node-C'', ''enqueue'', ''position=2, priority=1'', ''2025-01-20 10:01:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:cache-1'', ''node-A'', ''enqueue'', ''position=1'', ''2025-01-20 10:01:30'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''mutex:user-update'', ''node-A'', ''enqueue'', ''position=1'', ''2025-01-20 10:01:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''mutex:order-process'', ''node-C'', ''enqueue'', ''position=1'', ''2025-01-20 10:02:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:queue-worker'', ''node-B'', ''enqueue'', ''position=1'', ''2025-01-20 10:02:30'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:file-store'', ''node-A'', ''acquire'', ''TTL=60s'', ''2025-01-20 09:50:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:file-store'', ''node-A'', ''release'', ''held 45s'', ''2025-01-20 09:50:45'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''mutex:config-write'', ''node-B'', ''acquire'', ''TTL=30s'', ''2025-01-20 09:45:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''mutex:config-write'', ''node-B'', ''release'', ''held 12s'', ''2025-01-20 09:45:12'')');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Lists all registered locks with owner, status, reentry count, and timestamps, then summarizes counts by status. }
procedure Demo1_LockRegistry;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Lock Registry ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT lock_name, ' +
    '  COALESCE(owner, ''-'') AS owner, ' +
    '  status, ' +
    '  reentry_count, ' +
    '  COALESCE(acquired_at, ''-'') AS acquired, ' +
    '  COALESCE(expires_at, ''-'') AS expires ' +
    'FROM locks ORDER BY status, lock_name');
  try
    WriteLn(Format('   %-25s %-8s %-8s %-6s %-22s %s',
      ['Lock Name', 'Owner', 'Status', 'Reent', 'Acquired', 'Expires']));
    WriteLn('   ' + StringOfChar('-', 100));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-8s %-8s %-6d %-22s %s', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('reentry_count').AsInteger,
        DS.FieldByName('acquired').AsString,
        DS.FieldByName('expires').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) AS cnt FROM locks GROUP BY status ORDER BY status');
  try
    Write('   Summary: ');
    while not DS.EOF do
    begin
      Write(Format('%s=%d ', [DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
    WriteLn;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Acquires a free lock by setting owner and TTL, simulates a release, then lists all currently held locks with their TTL values. }
procedure Demo2_LockAcquisition;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Lock Acquisition and Release ===');
  WriteLn;

  // Simulate acquiring a free lock
  Conn.ExecuteNonQuery(
    'UPDATE locks SET owner = ''node-A'', status = ''held'', ' +
    '  acquired_at = ''2025-01-20 10:03:00'', ' +
    '  expires_at = ''2025-01-20 10:08:00'', ' +
    '  reentry_count = 1 ' +
    'WHERE lock_name = ''resource:file-store'' AND status = ''free''');

  Conn.ExecuteNonQuery(
    'INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:file-store'', ''node-A'', ''acquire'', ''TTL=300s'', ''2025-01-20 10:03:00'')');

  WriteLn('   Acquired: resource:file-store by node-A (TTL=300s)');

  // Simulate releasing a lock
  Conn.ExecuteNonQuery(
    'UPDATE locks SET owner = NULL, status = ''free'', ' +
    '  acquired_at = NULL, expires_at = NULL, reentry_count = 0 ' +
    'WHERE lock_name = ''mutex:config-write''');

  WriteLn('   Released: mutex:config-write (was free, no-op)');
  WriteLn;

  // Show current held locks
  DS := Conn.ExecuteQuery(
    'SELECT lock_name, owner, ' +
    '  CAST((julianday(expires_at) - julianday(acquired_at)) * 86400 AS INTEGER) AS ttl_sec ' +
    'FROM locks WHERE status = ''held'' ORDER BY acquired_at');
  try
    WriteLn('   Currently held locks:');
    WriteLn(Format('   %-25s %-8s %s', ['Lock', 'Owner', 'TTL (sec)']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-8s %d', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('ttl_sec').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Finds held locks past their expiration time, shows previously expired locks, and force-expires overdue ones. }
procedure Demo3_TTLExpiry;
var
  DS: TDataSet;
  ExpiredCount: Integer;
begin
  WriteLn('=== 3. TTL Expiry Detection ===');
  WriteLn;

  // Check for expired locks (current time = 10:03:00, user-update expired at 10:02:30)
  WriteLn('   Current time: 2025-01-20 10:03:00');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT lock_name, owner, expires_at, ' +
    '  CAST((julianday(''2025-01-20 10:03:00'') - julianday(expires_at)) * 86400 AS INTEGER) AS expired_sec_ago ' +
    'FROM locks ' +
    'WHERE status = ''held'' AND expires_at < ''2025-01-20 10:03:00'' ' +
    'ORDER BY expires_at');
  try
    WriteLn('   Expired locks (held past TTL):');
    WriteLn(Format('   %-25s %-8s %-22s %s', ['Lock', 'Owner', 'Expired At', 'Overdue (sec)']));
    WriteLn('   ' + StringOfChar('-', 75));
    ExpiredCount := 0;
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-8s %-22s %d', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('expires_at').AsString,
        DS.FieldByName('expired_sec_ago').AsInteger]));
      Inc(ExpiredCount);
      DS.Next;
    end;
    if ExpiredCount = 0 then
      WriteLn('   (no expired locks)');
  finally
    DS.Free;
  end;
  WriteLn;

  // Also show already-expired status locks
  DS := Conn.ExecuteQuery(
    'SELECT lock_name, owner, expires_at FROM locks WHERE status = ''expired''');
  try
    WriteLn('   Previously expired (already marked):');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (was held by %s, expired %s)', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('expires_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Force-expire the overdue lock
  Conn.ExecuteNonQuery(
    'UPDATE locks SET status = ''expired'' ' +
    'WHERE status = ''held'' AND expires_at < ''2025-01-20 10:03:00''');
  ExpiredCount := Conn.GetChangesCount;
  WriteLn(Format('   Force-expired %d overdue lock(s)', [ExpiredCount]));
  WriteLn;
end;

{ Lists waiting lock requests with priority-based queue position and timeout, then shows queue depth per lock. }
procedure Demo4_LockQueue;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Lock Queue (Waiters) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT q.lock_name, q.requester, q.priority, q.requested_at, q.timeout_at, ' +
    '  l.owner AS current_holder, ' +
    '  (SELECT COUNT(*) FROM lock_queue q2 ' +
    '   WHERE q2.lock_name = q.lock_name AND q2.status = ''waiting'' ' +
    '   AND (q2.priority > q.priority OR (q2.priority = q.priority AND q2.requested_at < q.requested_at))) + 1 AS position ' +
    'FROM lock_queue q ' +
    'JOIN locks l ON l.lock_name = q.lock_name ' +
    'WHERE q.status = ''waiting'' ' +
    'ORDER BY q.lock_name, q.priority DESC, q.requested_at');
  try
    WriteLn(Format('   %-25s %-8s %-4s %-4s %-8s %s',
      ['Lock', 'Waiter', 'Prio', 'Pos', 'Holder', 'Timeout']));
    WriteLn('   ' + StringOfChar('-', 90));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-8s %-4d %-4d %-8s %s', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('requester').AsString,
        DS.FieldByName('priority').AsInteger,
        DS.FieldByName('position').AsInteger,
        DS.FieldByName('current_holder').AsString,
        DS.FieldByName('timeout_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Queue depth per lock
  DS := Conn.ExecuteQuery(
    'SELECT lock_name, COUNT(*) AS queue_depth, ' +
    '  MAX(priority) AS max_priority ' +
    'FROM lock_queue WHERE status = ''waiting'' ' +
    'GROUP BY lock_name ORDER BY queue_depth DESC');
  try
    WriteLn('   Queue depth per lock:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d waiter(s), max priority=%d', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('queue_depth').AsInteger,
        DS.FieldByName('max_priority').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows locks with reentry count greater than one, simulates incrementing and decrementing reentry, and denies non-owner reentry attempts. }
procedure Demo5_ReentrantLocks;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Reentrant Locks ===');
  WriteLn;

  WriteLn('   Reentrant lock allows same owner to acquire again (incrementing count)');
  WriteLn;

  // Show current reentrant locks
  DS := Conn.ExecuteQuery(
    'SELECT lock_name, owner, reentry_count FROM locks ' +
    'WHERE status = ''held'' AND reentry_count > 1');
  try
    WriteLn('   Currently reentrant (count > 1):');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: owner=%s, count=%d', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('reentry_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Simulate reentry: node-A re-acquires queue-worker
  WriteLn('   Simulating: node-A re-acquires resource:queue-worker...');
  Conn.ExecuteNonQuery(
    'UPDATE locks SET reentry_count = reentry_count + 1 ' +
    'WHERE lock_name = ''resource:queue-worker'' AND owner = ''node-A'' AND status = ''held''');
  Conn.ExecuteNonQuery(
    'INSERT INTO lock_history (lock_name, owner, action, details, timestamp) VALUES ' +
    '(''resource:queue-worker'', ''node-A'', ''reenter'', ''count=2'', ''2025-01-20 10:03:00'')');
  WriteLn('   OK: reentry_count incremented to 2');
  WriteLn;

  // Simulate partial release (decrement)
  WriteLn('   Simulating: node-A releases one level of resource:queue-worker...');
  Conn.ExecuteNonQuery(
    'UPDATE locks SET reentry_count = reentry_count - 1 ' +
    'WHERE lock_name = ''resource:queue-worker'' AND owner = ''node-A'' AND reentry_count > 1');
  WriteLn('   OK: reentry_count decremented to 1 (lock still held)');
  WriteLn;

  // Show reentry attempts by non-owner
  WriteLn('   Non-owner reentry attempt: node-C tries resource:queue-worker');
  DS := Conn.ExecuteQuery(
    'SELECT lock_name, owner FROM locks ' +
    'WHERE lock_name = ''resource:queue-worker'' AND owner != ''node-C'' AND status = ''held''');
  try
    if not DS.EOF then
      WriteLn(Format('   DENIED: lock held by %s (not node-C)', [
        DS.FieldByName('owner').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays direct waiter-to-holder edges from the wait-for graph and summarizes how many nodes each waiter depends on. }
procedure Demo6_WaitForGraph;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Wait-For Graph ===');
  WriteLn;

  WriteLn('   Direct wait relationships (waiter -> holder via lock):');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT waiter, holder, lock_name FROM wait_for_graph ORDER BY waiter, holder');
  try
    WriteLn(Format('   %-8s %-10s %s', ['Waiter', 'Holder', 'Lock']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s -> %-7s via %s', [
        DS.FieldByName('waiter').AsString,
        DS.FieldByName('holder').AsString,
        DS.FieldByName('lock_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Show out-degree (how many nodes each waiter depends on)
  DS := Conn.ExecuteQuery(
    'SELECT waiter, COUNT(DISTINCT holder) AS depends_on, ' +
    '  GROUP_CONCAT(DISTINCT holder) AS holders ' +
    'FROM wait_for_graph GROUP BY waiter ORDER BY depends_on DESC');
  try
    WriteLn('   Wait dependencies:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s waits for %d node(s): %s', [
        DS.FieldByName('waiter').AsString,
        DS.FieldByName('depends_on').AsInteger,
        DS.FieldByName('holders').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Uses recursive CTE to find cycles in the wait-for graph, explains the circular dependency, and suggests aborting the youngest waiter. }
procedure Demo7_DeadlockDetection;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Deadlock Detection (Cycle Finding) ===');
  WriteLn;

  // Use recursive CTE to find cycles in wait-for graph
  // A cycle means: starting from node X, following edges, we return to X
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE detect_cycle(start_node, current_node, path, depth) AS (' +
    '  SELECT waiter, holder, waiter || '' -> '' || holder, 1 ' +
    '  FROM wait_for_graph ' +
    '  UNION ALL ' +
    '  SELECT dc.start_node, wfg.holder, ' +
    '    dc.path || '' -> '' || wfg.holder, dc.depth + 1 ' +
    '  FROM detect_cycle dc ' +
    '  JOIN wait_for_graph wfg ON wfg.waiter = dc.current_node ' +
    '  WHERE dc.depth < 6 ' +
    '    AND dc.current_node != dc.start_node ' +
    '    AND (dc.path NOT LIKE ''%'' || wfg.holder || '' -> %'' ' +
    '         OR wfg.holder = dc.start_node)' +
    ') ' +
    'SELECT DISTINCT start_node, path, depth ' +
    'FROM detect_cycle ' +
    'WHERE current_node = start_node AND depth >= 2 ' +
    'ORDER BY depth, start_node');
  try
    WriteLn('   Detected cycles (deadlocks):');
    WriteLn;
    if DS.EOF then
      WriteLn('   (no deadlocks found)')
    else
    begin
      while not DS.EOF do
      begin
        WriteLn(Format('   Cycle (depth %d): %s', [
          DS.FieldByName('depth').AsInteger,
          DS.FieldByName('path').AsString]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Show the simplest deadlock explanation
  WriteLn('   Deadlock explanation:');
  WriteLn('   - node-A holds resource:db-primary, waits for resource:cache-1 (node-B)');
  WriteLn('   - node-B holds resource:cache-1, waits for resource:db-primary (node-A)');
  WriteLn('   => Circular dependency: neither can proceed');
  WriteLn;

  // Resolution: abort the youngest transaction (most recent waiter)
  DS := Conn.ExecuteQuery(
    'SELECT q.requester, q.lock_name, q.requested_at ' +
    'FROM lock_queue q ' +
    'WHERE q.requester IN (''node-A'', ''node-B'') ' +
    '  AND q.status = ''waiting'' ' +
    'ORDER BY q.requested_at DESC LIMIT 1');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Resolution: abort youngest waiter (%s, queued at %s)', [
        DS.FieldByName('requester').AsString,
        DS.FieldByName('requested_at').AsString]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows each lock's queue depth, total acquires, and wait counts from history, then calculates the overall contention ratio. }
procedure Demo8_LockContention;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Lock Contention Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT l.lock_name, ' +
    '  COALESCE(l.owner, ''-'') AS holder, ' +
    '  (SELECT COUNT(*) FROM lock_queue q WHERE q.lock_name = l.lock_name AND q.status = ''waiting'') AS queue_depth, ' +
    '  (SELECT COUNT(*) FROM lock_history h WHERE h.lock_name = l.lock_name AND h.action = ''acquire'') AS total_acquires, ' +
    '  (SELECT COUNT(*) FROM lock_history h WHERE h.lock_name = l.lock_name AND h.action = ''enqueue'') AS total_waits ' +
    'FROM locks l ' +
    'ORDER BY queue_depth DESC, total_acquires DESC');
  try
    WriteLn(Format('   %-25s %-8s %-6s %-8s %s',
      ['Lock', 'Holder', 'Queue', 'Acquires', 'Waits']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-8s %-6d %-8d %d', [
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('holder').AsString,
        DS.FieldByName('queue_depth').AsInteger,
        DS.FieldByName('total_acquires').AsInteger,
        DS.FieldByName('total_waits').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Contention ratio
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM lock_history WHERE action = ''enqueue'') AS total_waits, ' +
    '  (SELECT COUNT(*) FROM lock_history WHERE action = ''acquire'') AS total_acquires');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Contention ratio: %d waits / %d acquires = %.1f%%', [
        DS.FieldByName('total_waits').AsInteger,
        DS.FieldByName('total_acquires').AsInteger,
        DS.FieldByName('total_waits').AsFloat /
          DS.FieldByName('total_acquires').AsFloat * 100]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists the full lock operation history as an audit trail and summarizes action counts. }
procedure Demo9_LockHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Lock History (Audit Trail) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT timestamp, lock_name, owner, action, COALESCE(details, ''-'') AS details ' +
    'FROM lock_history ORDER BY timestamp, id');
  try
    WriteLn(Format('   %-22s %-25s %-8s %-8s %s',
      ['Timestamp', 'Lock', 'Owner', 'Action', 'Details']));
    WriteLn('   ' + StringOfChar('-', 95));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-25s %-8s %-8s %s', [
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('lock_name').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('action').AsString,
        DS.FieldByName('details').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT action, COUNT(*) AS cnt FROM lock_history GROUP BY action ORDER BY cnt DESC');
  try
    Write('   Actions: ');
    while not DS.EOF do
    begin
      Write(Format('%s=%d ', [DS.FieldByName('action').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
    WriteLn;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Reports overall lock system metrics, per-node holdings with reentries, and completed lock durations from release history. }
procedure Demo10_HealthMetrics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Lock System Health ===');
  WriteLn;

  // Overall metrics
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM locks) AS total_locks, ' +
    '  (SELECT COUNT(*) FROM locks WHERE status = ''held'') AS held, ' +
    '  (SELECT COUNT(*) FROM locks WHERE status = ''free'') AS free, ' +
    '  (SELECT COUNT(*) FROM locks WHERE status = ''expired'') AS expired, ' +
    '  (SELECT COUNT(*) FROM lock_queue WHERE status = ''waiting'') AS waiters, ' +
    '  (SELECT COUNT(DISTINCT waiter) FROM wait_for_graph) AS nodes_waiting, ' +
    '  (SELECT COUNT(*) FROM lock_history) AS history_entries');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Total locks:      %d', [DS.FieldByName('total_locks').AsInteger]));
      WriteLn(Format('   Held:             %d', [DS.FieldByName('held').AsInteger]));
      WriteLn(Format('   Free:             %d', [DS.FieldByName('free').AsInteger]));
      WriteLn(Format('   Expired:          %d', [DS.FieldByName('expired').AsInteger]));
      WriteLn(Format('   Queued waiters:   %d', [DS.FieldByName('waiters').AsInteger]));
      WriteLn(Format('   Nodes waiting:    %d', [DS.FieldByName('nodes_waiting').AsInteger]));
      WriteLn(Format('   History entries:  %d', [DS.FieldByName('history_entries').AsInteger]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Per-node summary
  DS := Conn.ExecuteQuery(
    'SELECT owner AS node, ' +
    '  COUNT(*) AS locks_held, ' +
    '  SUM(reentry_count) AS total_reentries ' +
    'FROM locks WHERE status = ''held'' AND owner IS NOT NULL ' +
    'GROUP BY owner ORDER BY locks_held DESC');
  try
    WriteLn('   Per-node lock holdings:');
    WriteLn(Format('   %-8s %-10s %s', ['Node', 'Held', 'Reentries']));
    WriteLn('   ' + StringOfChar('-', 35));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-10d %d', [
        DS.FieldByName('node').AsString,
        DS.FieldByName('locks_held').AsInteger,
        DS.FieldByName('total_reentries').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Average hold time from history (for released locks)
  DS := Conn.ExecuteQuery(
    'SELECT owner, details FROM lock_history ' +
    'WHERE action = ''release'' AND details LIKE ''held %s'' ' +
    'ORDER BY timestamp');
  try
    WriteLn('   Completed lock durations:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %s', [
        DS.FieldByName('owner').AsString,
        DS.FieldByName('details').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

begin
  WriteLn('Example 143: Distributed Lock Manager');
  WriteLn('======================================================================');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_LockRegistry;
    Demo2_LockAcquisition;
    Demo3_TTLExpiry;
    Demo4_LockQueue;
    Demo5_ReentrantLocks;
    Demo6_WaitForGraph;
    Demo7_DeadlockDetection;
    Demo8_LockContention;
    Demo9_LockHistory;
    Demo10_HealthMetrics;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
