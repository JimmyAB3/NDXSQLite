{===============================================================================
  NDXSQLite Example 98 - Data Sync
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Multi-node data synchronization
  - Last-writer-wins conflict resolution
  - Soft delete propagation across nodes
  - Bidirectional sync with change tracking
  - Timestamp-based change detection

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataSync;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, NDXSQLiteConnection, DB;

var
  NodeA, NodeB: TNDXSQLiteConnection;
  DS, DS2: TDataSet;
  SyncCount: Integer;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema(Conn: TNDXSQLiteConnection);
begin
  Conn.ExecuteNonQuery(
    'CREATE TABLE contacts (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  phone TEXT,' +
    '  updated_at TEXT NOT NULL,' +
    '  is_deleted INTEGER NOT NULL DEFAULT 0,' +
    '  node_origin TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE sync_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sync_direction TEXT NOT NULL,' +
    '  remote_node TEXT NOT NULL,' +
    '  records_pushed INTEGER DEFAULT 0,' +
    '  records_pulled INTEGER DEFAULT 0,' +
    '  conflicts INTEGER DEFAULT 0,' +
    '  sync_at TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE sync_state (' +
    '  remote_node TEXT PRIMARY KEY,' +
    '  last_sync_at TEXT NOT NULL' +
    ')');
end;

{ Inserts or replaces a contact record in the specified node's database. }
procedure InsertContact(Conn: TNDXSQLiteConnection; const Id, Name, Email, Phone, Timestamp, NodeOrigin: string);
begin
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO contacts (id, name, email, phone, updated_at, is_deleted, node_origin) ' +
    'VALUES (''' + Id + ''', ''' + Name + ''', ''' + Email + ''', ''' + Phone + ''', ''' + Timestamp + ''', 0, ''' + NodeOrigin + ''')');
end;

{ Marks a contact as deleted by setting is_deleted flag with a new timestamp and origin node. }
procedure SoftDelete(Conn: TNDXSQLiteConnection; const Id, Timestamp, NodeOrigin: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE contacts SET is_deleted = 1, updated_at = ''' + Timestamp + ''', node_origin = ''' + NodeOrigin + ''' ' +
    'WHERE id = ''' + Id + '''');
end;

{ Prints all contacts (or only active contacts) from a node showing id, name, email, timestamp, and origin. }
procedure ShowContacts(Conn: TNDXSQLiteConnection; const NodeName: string; ShowDeleted: Boolean);
begin
  if ShowDeleted then
    DS := Conn.ExecuteQuery('SELECT id, name, email, phone, updated_at, is_deleted, node_origin FROM contacts ORDER BY id')
  else
    DS := Conn.ExecuteQuery('SELECT id, name, email, phone, updated_at, is_deleted, node_origin FROM contacts WHERE is_deleted = 0 ORDER BY id');
  try
    while not DS.EOF do
    begin
      Write('   [', NodeName, '] ', DS.FieldByName('id').AsString:6,
        ' ', DS.FieldByName('name').AsString:12);
      if DS.FieldByName('is_deleted').AsInteger = 1 then
        Write(' [DELETED]')
      else
        Write(' ', DS.FieldByName('email').AsString:20);
      WriteLn(' (', DS.FieldByName('updated_at').AsString, ', origin:', DS.FieldByName('node_origin').AsString, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Pulls changes from source to target since last sync, applying last-writer-wins conflict resolution, and logs the sync. }
function SyncNodes(Source, Target: TNDXSQLiteConnection; const SourceName, TargetName: string): Integer;
var
  LastSync, MaxUpdated: string;
  Pushed, Conflicts: Integer;
  LocalTime, RemoteTime: string;
begin
  Pushed := 0;
  Conflicts := 0;

  // Get last sync time for this source
  DS := Target.ExecuteQuery(
    'SELECT last_sync_at FROM sync_state WHERE remote_node = ''' + SourceName + '''');
  try
    if DS.EOF then
      LastSync := '1970-01-01T00:00:00'
    else
      LastSync := DS.FieldByName('last_sync_at').AsString;
  finally
    DS.Free;
  end;

  // Get max updated_at from source to use as new watermark
  MaxUpdated := LastSync;
  DS := Source.ExecuteQuery('SELECT MAX(updated_at) as max_ts FROM contacts');
  try
    if (not DS.EOF) and (not DS.FieldByName('max_ts').IsNull) then
      if DS.FieldByName('max_ts').AsString > MaxUpdated then
        MaxUpdated := DS.FieldByName('max_ts').AsString;
  finally
    DS.Free;
  end;

  // Get changes from source since last sync
  DS := Source.ExecuteQuery(
    'SELECT id, name, email, phone, updated_at, is_deleted, node_origin ' +
    'FROM contacts WHERE updated_at > ''' + LastSync + ''' ORDER BY updated_at');
  try
    while not DS.EOF do
    begin
      // Check if target has this record
      DS2 := Target.ExecuteQuery(
        'SELECT updated_at FROM contacts WHERE id = ''' + DS.FieldByName('id').AsString + '''');
      try
        if DS2.EOF then
        begin
          // New record - just insert
          Target.ExecuteNonQuery(
            'INSERT INTO contacts (id, name, email, phone, updated_at, is_deleted, node_origin) VALUES (' +
            '''' + DS.FieldByName('id').AsString + ''', ' +
            '''' + DS.FieldByName('name').AsString + ''', ' +
            '''' + DS.FieldByName('email').AsString + ''', ' +
            '''' + DS.FieldByName('phone').AsString + ''', ' +
            '''' + DS.FieldByName('updated_at').AsString + ''', ' +
            IntToStr(DS.FieldByName('is_deleted').AsInteger) + ', ' +
            '''' + DS.FieldByName('node_origin').AsString + ''')');
          Inc(Pushed);
        end
        else
        begin
          // Exists - conflict resolution (last-writer-wins)
          RemoteTime := DS.FieldByName('updated_at').AsString;
          LocalTime := DS2.FieldByName('updated_at').AsString;
          if RemoteTime > LocalTime then
          begin
            // Source is newer - overwrite target
            Target.ExecuteNonQuery(
              'UPDATE contacts SET name = ''' + DS.FieldByName('name').AsString + ''', ' +
              'email = ''' + DS.FieldByName('email').AsString + ''', ' +
              'phone = ''' + DS.FieldByName('phone').AsString + ''', ' +
              'updated_at = ''' + RemoteTime + ''', ' +
              'is_deleted = ' + IntToStr(DS.FieldByName('is_deleted').AsInteger) + ', ' +
              'node_origin = ''' + DS.FieldByName('node_origin').AsString + ''' ' +
              'WHERE id = ''' + DS.FieldByName('id').AsString + '''');
            Inc(Pushed);
            Inc(Conflicts);
          end;
          // else: target is newer or same, skip
        end;
      finally
        DS2.Free;
      end;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update sync state with the max timestamp from source
  Target.ExecuteNonQuery(
    'INSERT OR REPLACE INTO sync_state (remote_node, last_sync_at) VALUES (' +
    '''' + SourceName + ''', ''' + MaxUpdated + ''')');

  // Log the sync
  Target.ExecuteNonQuery(
    'INSERT INTO sync_log (sync_direction, remote_node, records_pushed, conflicts, sync_at) VALUES (' +
    '''pull_from'', ''' + SourceName + ''', ' + IntToStr(Pushed) + ', ' + IntToStr(Conflicts) + ', ''' + MaxUpdated + ''')');

  Result := Pushed;
end;

{ Performs bidirectional synchronization between two nodes and returns the total records synced. }
function BidirectionalSync(ConnA, ConnB: TNDXSQLiteConnection; const NameA, NameB: string): Integer;
var
  PushedAtoB, PushedBtoA: Integer;
begin
  // A -> B (push A's changes to B)
  PushedAtoB := SyncNodes(ConnA, ConnB, NameA, NameB);
  // B -> A (push B's changes to A)
  PushedBtoA := SyncNodes(ConnB, ConnA, NameB, NameA);
  Result := PushedAtoB + PushedBtoA;
  WriteLn('   Sync complete: A->B: ', PushedAtoB, ' records, B->A: ', PushedBtoA, ' records');
end;

{ Demonstrates the initial setup of two nodes with shared schema and sample data. }
procedure DemoSetup;
begin
  WriteLn('=== 1. Setup - Two Nodes with Shared Schema ===');
  WriteLn('');

  // Initial data on Node A
  InsertContact(NodeA, 'c-001', 'Alice Smith', 'alice@company.com', '555-0101', '2024-01-10T09:00:00', 'NodeA');
  InsertContact(NodeA, 'c-002', 'Bob Jones', 'bob@company.com', '555-0102', '2024-01-10T09:05:00', 'NodeA');
  InsertContact(NodeA, 'c-003', 'Carol White', 'carol@company.com', '555-0103', '2024-01-10T09:10:00', 'NodeA');

  // Initial data on Node B
  InsertContact(NodeB, 'c-004', 'Dave Brown', 'dave@partner.com', '555-0201', '2024-01-10T10:00:00', 'NodeB');
  InsertContact(NodeB, 'c-005', 'Eve Green', 'eve@partner.com', '555-0202', '2024-01-10T10:05:00', 'NodeB');

  WriteLn('   Node A contacts (3):');
  ShowContacts(NodeA, 'A', False);
  WriteLn('');
  WriteLn('   Node B contacts (2):');
  ShowContacts(NodeB, 'B', False);
  WriteLn('');
end;

{ Performs the first bidirectional sync, merging Node A's 3 contacts and Node B's 2 contacts into both nodes. }
procedure DemoInitialSync;
begin
  WriteLn('=== 2. Initial Bidirectional Sync ===');
  WriteLn('');
  WriteLn('   Performing first sync between Node A and Node B...');
  BidirectionalSync(NodeA, NodeB, 'NodeA', 'NodeB');
  WriteLn('');

  WriteLn('   Node A after sync (5):');
  ShowContacts(NodeA, 'A', False);
  WriteLn('');
  WriteLn('   Node B after sync (5):');
  ShowContacts(NodeB, 'B', False);
  WriteLn('');
end;

{ Adds and updates contacts independently on each node, then syncs to propagate all changes to both. }
procedure DemoIndependentChanges;
begin
  WriteLn('=== 3. Independent Changes on Both Nodes ===');
  WriteLn('');

  // Node A adds a new contact
  InsertContact(NodeA, 'c-006', 'Frank Lee', 'frank@company.com', '555-0106', '2024-01-11T08:00:00', 'NodeA');
  WriteLn('   Node A: Added Frank Lee (c-006)');

  // Node B adds a new contact
  InsertContact(NodeB, 'c-007', 'Grace Kim', 'grace@partner.com', '555-0207', '2024-01-11T09:00:00', 'NodeB');
  WriteLn('   Node B: Added Grace Kim (c-007)');

  // Node A updates an existing contact
  InsertContact(NodeA, 'c-002', 'Bob Jones Jr', 'bob.jr@company.com', '555-0102', '2024-01-11T10:00:00', 'NodeA');
  WriteLn('   Node A: Updated Bob Jones -> Bob Jones Jr (c-002)');

  // Node B updates a different existing contact
  InsertContact(NodeB, 'c-004', 'Dave Brown', 'dave.b@newdomain.com', '555-0201-ext', '2024-01-11T10:30:00', 'NodeB');
  WriteLn('   Node B: Updated Dave Brown email/phone (c-004)');

  WriteLn('');
  WriteLn('   Syncing after independent changes...');
  BidirectionalSync(NodeA, NodeB, 'NodeA', 'NodeB');
  WriteLn('');

  WriteLn('   Node A after sync (7):');
  ShowContacts(NodeA, 'A', False);
  WriteLn('');
  WriteLn('   Node B after sync (7):');
  ShowContacts(NodeB, 'B', False);
  WriteLn('');
end;

{ Modifies the same contact on both nodes with different timestamps, showing last-writer-wins resolution. }
procedure DemoConflictResolution;
begin
  WriteLn('=== 4. Conflict Resolution (Last-Writer-Wins) ===');
  WriteLn('');

  // Both nodes modify the same contact
  // Node A updates at 11:00
  InsertContact(NodeA, 'c-003', 'Carol White-Smith', 'carol.ws@company.com', '555-0103', '2024-01-12T11:00:00', 'NodeA');
  WriteLn('   Node A: Updated Carol (c-003) at 11:00 -> "Carol White-Smith"');

  // Node B updates same contact at 11:30 (later = wins)
  InsertContact(NodeB, 'c-003', 'Carol W. Johnson', 'carol.j@partner.com', '555-9999', '2024-01-12T11:30:00', 'NodeB');
  WriteLn('   Node B: Updated Carol (c-003) at 11:30 -> "Carol W. Johnson"');

  WriteLn('');
  WriteLn('   Conflict: Both nodes modified c-003');
  WriteLn('   Resolution: Last-writer-wins (Node B at 11:30 > Node A at 11:00)');
  WriteLn('');
  WriteLn('   Syncing...');
  BidirectionalSync(NodeA, NodeB, 'NodeA', 'NodeB');
  WriteLn('');

  // Show the resolved record
  WriteLn('   c-003 on Node A after resolution:');
  DS := NodeA.ExecuteQuery('SELECT name, email, phone, node_origin FROM contacts WHERE id = ''c-003''');
  try
    if not DS.EOF then
      WriteLn('   -> ', DS.FieldByName('name').AsString,
        ', ', DS.FieldByName('email').AsString,
        ', origin: ', DS.FieldByName('node_origin').AsString);
  finally
    DS.Free;
  end;

  WriteLn('   c-003 on Node B after resolution:');
  DS := NodeB.ExecuteQuery('SELECT name, email, phone, node_origin FROM contacts WHERE id = ''c-003''');
  try
    if not DS.EOF then
      WriteLn('   -> ', DS.FieldByName('name').AsString,
        ', ', DS.FieldByName('email').AsString,
        ', origin: ', DS.FieldByName('node_origin').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Demonstrates tombstone-based deletion and its propagation across nodes. }
procedure DemoTombstones;
begin
  WriteLn('=== 5. Tombstone Deletions ===');
  WriteLn('');

  // Node A soft-deletes a contact
  SoftDelete(NodeA, 'c-006', '2024-01-13T08:00:00', 'NodeA');
  WriteLn('   Node A: Soft-deleted Frank Lee (c-006)');

  // Node B soft-deletes a different contact
  SoftDelete(NodeB, 'c-005', '2024-01-13T09:00:00', 'NodeB');
  WriteLn('   Node B: Soft-deleted Eve Green (c-005)');

  WriteLn('');
  WriteLn('   Syncing tombstones...');
  BidirectionalSync(NodeA, NodeB, 'NodeA', 'NodeB');
  WriteLn('');

  WriteLn('   Node A - all records (including tombstones):');
  ShowContacts(NodeA, 'A', True);
  WriteLn('');
  WriteLn('   Node A - active records only:');
  ShowContacts(NodeA, 'A', False);
  WriteLn('');

  // Count active vs deleted
  DS := NodeA.ExecuteQuery('SELECT COUNT(*) as total, SUM(CASE WHEN is_deleted = 0 THEN 1 ELSE 0 END) as active, SUM(is_deleted) as deleted FROM contacts');
  try
    WriteLn('   Node A stats: total=', DS.FieldByName('total').AsInteger,
      ', active=', DS.FieldByName('active').AsInteger,
      ', tombstones=', DS.FieldByName('deleted').AsInteger);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Shows how a delete on one node overrides an update on another when the delete has a later timestamp. }
procedure DemoDeleteConflict;
begin
  WriteLn('=== 6. Delete vs Update Conflict ===');
  WriteLn('');

  // Node A updates c-001 at 14:00
  InsertContact(NodeA, 'c-001', 'Alice Smith-Updated', 'alice.new@company.com', '555-0101', '2024-01-14T14:00:00', 'NodeA');
  WriteLn('   Node A: Updated Alice (c-001) at 14:00');

  // Node B deletes c-001 at 14:30 (later)
  SoftDelete(NodeB, 'c-001', '2024-01-14T14:30:00', 'NodeB');
  WriteLn('   Node B: Deleted Alice (c-001) at 14:30');

  WriteLn('');
  WriteLn('   Conflict: Node A updated, Node B deleted same record');
  WriteLn('   Resolution: Delete wins (later timestamp)');
  WriteLn('');
  WriteLn('   Syncing...');
  BidirectionalSync(NodeA, NodeB, 'NodeA', 'NodeB');
  WriteLn('');

  // Check final state
  DS := NodeA.ExecuteQuery('SELECT name, is_deleted, node_origin, updated_at FROM contacts WHERE id = ''c-001''');
  try
    WriteLn('   c-001 on Node A: name=', DS.FieldByName('name').AsString,
      ', deleted=', DS.FieldByName('is_deleted').AsInteger,
      ', origin=', DS.FieldByName('node_origin').AsString);
  finally
    DS.Free;
  end;

  DS := NodeB.ExecuteQuery('SELECT name, is_deleted, node_origin, updated_at FROM contacts WHERE id = ''c-001''');
  try
    WriteLn('   c-001 on Node B: name=', DS.FieldByName('name').AsString,
      ', deleted=', DS.FieldByName('is_deleted').AsInteger,
      ', origin=', DS.FieldByName('node_origin').AsString);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Displays the sync_log table on both nodes showing the history of synchronization operations. }
procedure DemoSyncLog;
begin
  WriteLn('=== 7. Sync History ===');
  WriteLn('');
  WriteLn('   Node A sync log:');

  DS := NodeA.ExecuteQuery('SELECT id, sync_direction, remote_node, records_pushed, conflicts, sync_at FROM sync_log ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('id').AsInteger, '] ',
        DS.FieldByName('sync_direction').AsString, ' ',
        DS.FieldByName('remote_node').AsString,
        ': ', DS.FieldByName('records_pushed').AsInteger, ' records',
        ', ', DS.FieldByName('conflicts').AsInteger, ' conflicts');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Node B sync log:');

  DS := NodeB.ExecuteQuery('SELECT id, sync_direction, remote_node, records_pushed, conflicts, sync_at FROM sync_log ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('id').AsInteger, '] ',
        DS.FieldByName('sync_direction').AsString, ' ',
        DS.FieldByName('remote_node').AsString,
        ': ', DS.FieldByName('records_pushed').AsInteger, ' records',
        ', ', DS.FieldByName('conflicts').AsInteger, ' conflicts');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Purges tombstone records older than a threshold date and shows the remaining records. }
procedure DemoTombstoneCleanup;
begin
  WriteLn('=== 8. Tombstone Cleanup (Purge Old Deletions) ===');
  WriteLn('');

  DS := NodeA.ExecuteQuery('SELECT COUNT(*) as cnt FROM contacts WHERE is_deleted = 1');
  try
    WriteLn('   Tombstones before purge: ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  // Purge tombstones older than a threshold (simulated)
  WriteLn('   Purging tombstones older than 2024-01-14...');
  NodeA.ExecuteNonQuery('DELETE FROM contacts WHERE is_deleted = 1 AND updated_at < ''2024-01-14T00:00:00''');

  DS := NodeA.ExecuteQuery('SELECT COUNT(*) as cnt FROM contacts WHERE is_deleted = 1');
  try
    WriteLn('   Tombstones after purge: ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Remaining records on Node A:');
  ShowContacts(NodeA, 'A', True);
  WriteLn('');
end;

{ Compares active record counts on both nodes and verifies data consistency by checking each contact matches. }
procedure DemoFinalState;
var
  CountA, CountB, ActiveA, ActiveB: Integer;
begin
  WriteLn('=== 9. Final Consistency Check ===');
  WriteLn('');

  // Compare active records on both nodes
  DS := NodeA.ExecuteQuery('SELECT COUNT(*) as cnt FROM contacts WHERE is_deleted = 0');
  try ActiveA := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := NodeB.ExecuteQuery('SELECT COUNT(*) as cnt FROM contacts WHERE is_deleted = 0');
  try ActiveB := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := NodeA.ExecuteQuery('SELECT COUNT(*) as cnt FROM contacts');
  try CountA := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := NodeB.ExecuteQuery('SELECT COUNT(*) as cnt FROM contacts');
  try CountB := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  WriteLn('   Node A: ', CountA, ' total records, ', ActiveA, ' active');
  WriteLn('   Node B: ', CountB, ' total records, ', ActiveB, ' active');
  WriteLn('');

  // Verify data consistency for active records
  WriteLn('   Checking data consistency (active records matching):');
  DS := NodeA.ExecuteQuery(
    'SELECT id, name, email FROM contacts WHERE is_deleted = 0 ORDER BY id');
  try
    while not DS.EOF do
    begin
      DS2 := NodeB.ExecuteQuery(
        'SELECT name, email, is_deleted FROM contacts WHERE id = ''' + DS.FieldByName('id').AsString + '''');
      try
        if DS2.EOF then
          WriteLn('   MISMATCH: ', DS.FieldByName('id').AsString, ' missing on Node B')
        else if DS2.FieldByName('is_deleted').AsInteger = 1 then
          WriteLn('   MISMATCH: ', DS.FieldByName('id').AsString, ' deleted on Node B but active on A')
        else if DS.FieldByName('name').AsString = DS2.FieldByName('name').AsString then
          WriteLn('   OK: ', DS.FieldByName('id').AsString, ' = ', DS.FieldByName('name').AsString)
        else
          WriteLn('   MISMATCH: ', DS.FieldByName('id').AsString,
            ' A="', DS.FieldByName('name').AsString, '" B="', DS2.FieldByName('name').AsString, '"');
      finally
        DS2.Free;
      end;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Prints a summary of all data synchronization concepts demonstrated in this example. }
procedure PrintSummary;
begin
  WriteLn('=== Data Sync Summary ===');
  WriteLn('');
  WriteLn('   Concepts demonstrated:');
  WriteLn('   - Bidirectional sync between two SQLite databases');
  WriteLn('   - Timestamp-based change detection (updated_at)');
  WriteLn('   - Last-writer-wins conflict resolution');
  WriteLn('   - Tombstone pattern for soft deletes');
  WriteLn('   - Delete vs Update conflict handling');
  WriteLn('   - Sync state tracking (last_sync_at per remote)');
  WriteLn('   - Sync history logging');
  WriteLn('   - Tombstone cleanup/purging');
  WriteLn('   - Consistency verification');
  WriteLn('');
end;

begin
  WriteLn('Example 98: Data Sync - Bidirectional Synchronization');
  WriteLn(StringOfChar('=', 55));
  WriteLn('');

  NodeA := TNDXSQLiteConnection.Create(':memory:');
  NodeB := TNDXSQLiteConnection.Create(':memory:');
  try
    NodeA.Open;
    NodeB.Open;

    WriteLn('Setting up two nodes with identical schema...');
    CreateSchema(NodeA);
    CreateSchema(NodeB);
    WriteLn('Done. Node A and Node B initialized.');
    WriteLn('');

    DemoSetup;
    DemoInitialSync;
    DemoIndependentChanges;
    DemoConflictResolution;
    DemoTombstones;
    DemoDeleteConflict;
    DemoSyncLog;
    DemoTombstoneCleanup;
    DemoFinalState;
    PrintSummary;

    NodeA.Close;
    NodeB.Close;
  finally
    NodeA.Free;
    NodeB.Free;
  end;

  WriteLn('Done.');
end.
