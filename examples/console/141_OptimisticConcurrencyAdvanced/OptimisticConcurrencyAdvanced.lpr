{===============================================================================
  NDXSQLite Example 141 - Optimistic Concurrency Advanced
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Version vectors for conflict detection
  - Last-writer-wins and first-writer-wins strategies
  - Merge conflict resolution
  - Read snapshots at specific versions
  - Document version history

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program OptimisticConcurrencyAdvanced;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Documents with version vectors
  Conn.ExecuteNonQuery(
    'CREATE TABLE documents (' +
    '  doc_key TEXT NOT NULL, ' +
    '  field_name TEXT NOT NULL, ' +
    '  field_value TEXT, ' +
    '  version_vector TEXT NOT NULL, ' +
    '  updated_by TEXT NOT NULL, ' +
    '  updated_at TEXT NOT NULL, ' +
    '  PRIMARY KEY(doc_key, field_name))');

  // Version vector entries (parsed form for comparison)
  Conn.ExecuteNonQuery(
    'CREATE TABLE version_entries (' +
    '  doc_key TEXT NOT NULL, ' +
    '  node_id TEXT NOT NULL, ' +
    '  version INTEGER NOT NULL DEFAULT 0, ' +
    '  PRIMARY KEY(doc_key, node_id))');

  // Document history (all versions)
  Conn.ExecuteNonQuery(
    'CREATE TABLE doc_history (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  doc_key TEXT NOT NULL, ' +
    '  field_name TEXT NOT NULL, ' +
    '  field_value TEXT, ' +
    '  version_vector TEXT NOT NULL, ' +
    '  node_id TEXT NOT NULL, ' +
    '  operation TEXT NOT NULL, ' +
    '  timestamp TEXT NOT NULL)');

  // Detected conflicts
  Conn.ExecuteNonQuery(
    'CREATE TABLE conflicts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  doc_key TEXT NOT NULL, ' +
    '  field_name TEXT NOT NULL, ' +
    '  value_a TEXT, ' +
    '  value_b TEXT, ' +
    '  vector_a TEXT NOT NULL, ' +
    '  vector_b TEXT NOT NULL, ' +
    '  node_a TEXT NOT NULL, ' +
    '  node_b TEXT NOT NULL, ' +
    '  strategy TEXT, ' +
    '  resolved_value TEXT, ' +
    '  resolved_at TEXT, ' +
    '  status TEXT NOT NULL DEFAULT ''pending'')');

  // Merge strategies per document pattern
  Conn.ExecuteNonQuery(
    'CREATE TABLE merge_strategies (' +
    '  doc_pattern TEXT NOT NULL, ' +
    '  field_pattern TEXT NOT NULL DEFAULT ''*'', ' +
    '  strategy TEXT NOT NULL, ' +
    '  PRIMARY KEY(doc_pattern, field_pattern))');

  // Read snapshots
  Conn.ExecuteNonQuery(
    'CREATE TABLE snapshots (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  snapshot_name TEXT NOT NULL UNIQUE, ' +
    '  created_at TEXT NOT NULL, ' +
    '  description TEXT)');

  Conn.ExecuteNonQuery(
    'CREATE TABLE snapshot_data (' +
    '  snapshot_id INTEGER NOT NULL, ' +
    '  doc_key TEXT NOT NULL, ' +
    '  field_name TEXT NOT NULL, ' +
    '  field_value TEXT, ' +
    '  version_vector TEXT NOT NULL, ' +
    '  PRIMARY KEY(snapshot_id, doc_key, field_name))');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Merge strategies
  Conn.ExecuteNonQuery('INSERT INTO merge_strategies VALUES (''user:%'', ''*'', ''last-writer-wins'')');
  Conn.ExecuteNonQuery('INSERT INTO merge_strategies VALUES (''config:%'', ''*'', ''first-writer-wins'')');
  Conn.ExecuteNonQuery('INSERT INTO merge_strategies VALUES (''counter:%'', ''*'', ''merge-add'')');
  Conn.ExecuteNonQuery('INSERT INTO merge_strategies VALUES (''profile:%'', ''name'', ''last-writer-wins'')');
  Conn.ExecuteNonQuery('INSERT INTO merge_strategies VALUES (''profile:%'', ''bio'', ''longest-wins'')');
  Conn.ExecuteNonQuery('INSERT INTO merge_strategies VALUES (''profile:%'', ''score'', ''max-wins'')');

  // Initial documents (written by node-A)
  // User document
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''user:alice'', ''name'', ''Alice Smith'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''user:alice'', ''email'', ''alice@example.com'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''user:alice'', ''role'', ''developer'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');

  // Config document
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''config:app'', ''theme'', ''light'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''config:app'', ''language'', ''en'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''config:app'', ''timeout'', ''30'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');

  // Counter document
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''counter:visits'', ''count'', ''100'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');

  // Profile document
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''profile:bob'', ''name'', ''Bob Jones'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''profile:bob'', ''bio'', ''Developer'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO documents VALUES (''profile:bob'', ''score'', ''50'', ''A:1'', ''node-A'', ''2025-01-20 10:00:00'')');

  // Version entries for initial state
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''user:alice'', ''node-A'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''user:alice'', ''node-B'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''user:alice'', ''node-C'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''config:app'', ''node-A'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''config:app'', ''node-B'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''counter:visits'', ''node-A'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''counter:visits'', ''node-B'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''counter:visits'', ''node-C'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''profile:bob'', ''node-A'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO version_entries VALUES (''profile:bob'', ''node-B'', 0)');

  // History for initial writes
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''name'', ''Alice Smith'', ''A:1'', ''node-A'', ''create'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''email'', ''alice@example.com'', ''A:1'', ''node-A'', ''create'', ''2025-01-20 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''role'', ''developer'', ''A:1'', ''node-A'', ''create'', ''2025-01-20 10:00:00'')');

  // Simulate updates from different nodes

  // Node-A updates user:alice at A:2
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''email'', ''alice.smith@company.com'', ''A:2,B:0'', ''node-A'', ''update'', ''2025-01-20 10:05:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''role'', ''senior developer'', ''A:2,B:0'', ''node-A'', ''update'', ''2025-01-20 10:05:00'')');

  // Node-B updates user:alice CONCURRENTLY at A:1,B:1 (based on A:1, didn't see A:2)
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''email'', ''alice@newdomain.io'', ''A:1,B:1'', ''node-B'', ''update'', ''2025-01-20 10:06:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''name'', ''Alice S. Smith'', ''A:1,B:1'', ''node-B'', ''update'', ''2025-01-20 10:06:00'')');

  // Node-A updates config:app at A:2
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''config:app'', ''theme'', ''dark'', ''A:2'', ''node-A'', ''update'', ''2025-01-20 10:10:00'')');

  // Node-B updates config:app CONCURRENTLY at A:1,B:1
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''config:app'', ''theme'', ''blue'', ''A:1,B:1'', ''node-B'', ''update'', ''2025-01-20 10:11:00'')');

  // Counter: both nodes increment
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''counter:visits'', ''count'', ''115'', ''A:2,B:0'', ''node-A'', ''update'', ''2025-01-20 10:15:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''counter:visits'', ''count'', ''120'', ''A:1,B:1'', ''node-B'', ''update'', ''2025-01-20 10:16:00'')');

  // Profile: concurrent updates to different fields
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''profile:bob'', ''name'', ''Robert Jones'', ''A:2,B:0'', ''node-A'', ''update'', ''2025-01-20 10:20:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''profile:bob'', ''bio'', ''Senior Developer at TechCorp'', ''A:2,B:0'', ''node-A'', ''update'', ''2025-01-20 10:20:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''profile:bob'', ''score'', ''75'', ''A:2,B:0'', ''node-A'', ''update'', ''2025-01-20 10:20:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''profile:bob'', ''name'', ''Bob J.'', ''A:1,B:1'', ''node-B'', ''update'', ''2025-01-20 10:21:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''profile:bob'', ''bio'', ''Dev'', ''A:1,B:1'', ''node-B'', ''update'', ''2025-01-20 10:21:00'')');
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''profile:bob'', ''score'', ''80'', ''A:1,B:1'', ''node-B'', ''update'', ''2025-01-20 10:21:00'')');

  // Record conflicts
  Conn.ExecuteNonQuery('INSERT INTO conflicts (doc_key, field_name, value_a, value_b, vector_a, vector_b, node_a, node_b, strategy, resolved_value, resolved_at, status) VALUES ' +
    '(''user:alice'', ''email'', ''alice.smith@company.com'', ''alice@newdomain.io'', ''A:2,B:0'', ''A:1,B:1'', ''node-A'', ''node-B'', ''last-writer-wins'', ''alice@newdomain.io'', ''2025-01-20 10:07:00'', ''resolved'')');
  Conn.ExecuteNonQuery('INSERT INTO conflicts (doc_key, field_name, value_a, value_b, vector_a, vector_b, node_a, node_b, strategy, resolved_value, resolved_at, status) VALUES ' +
    '(''config:app'', ''theme'', ''dark'', ''blue'', ''A:2'', ''A:1,B:1'', ''node-A'', ''node-B'', ''first-writer-wins'', ''dark'', ''2025-01-20 10:12:00'', ''resolved'')');
  Conn.ExecuteNonQuery('INSERT INTO conflicts (doc_key, field_name, value_a, value_b, vector_a, vector_b, node_a, node_b, strategy, resolved_value, resolved_at, status) VALUES ' +
    '(''counter:visits'', ''count'', ''115'', ''120'', ''A:2,B:0'', ''A:1,B:1'', ''node-A'', ''node-B'', ''merge-add'', ''135'', ''2025-01-20 10:17:00'', ''resolved'')');
  Conn.ExecuteNonQuery('INSERT INTO conflicts (doc_key, field_name, value_a, value_b, vector_a, vector_b, node_a, node_b, strategy, resolved_value, resolved_at, status) VALUES ' +
    '(''profile:bob'', ''name'', ''Robert Jones'', ''Bob J.'', ''A:2,B:0'', ''A:1,B:1'', ''node-A'', ''node-B'', ''last-writer-wins'', ''Bob J.'', ''2025-01-20 10:22:00'', ''resolved'')');
  Conn.ExecuteNonQuery('INSERT INTO conflicts (doc_key, field_name, value_a, value_b, vector_a, vector_b, node_a, node_b, strategy, resolved_value, resolved_at, status) VALUES ' +
    '(''profile:bob'', ''bio'', ''Senior Developer at TechCorp'', ''Dev'', ''A:2,B:0'', ''A:1,B:1'', ''node-A'', ''node-B'', ''longest-wins'', ''Senior Developer at TechCorp'', ''2025-01-20 10:22:00'', ''resolved'')');
  Conn.ExecuteNonQuery('INSERT INTO conflicts (doc_key, field_name, value_a, value_b, vector_a, vector_b, node_a, node_b, strategy, resolved_value, resolved_at, status) VALUES ' +
    '(''profile:bob'', ''score'', ''75'', ''80'', ''A:2,B:0'', ''A:1,B:1'', ''node-A'', ''node-B'', ''max-wins'', ''80'', ''2025-01-20 10:22:00'', ''resolved'')');

  // Node-C writes (causing 3-way version vector)
  Conn.ExecuteNonQuery('INSERT INTO doc_history (doc_key, field_name, field_value, version_vector, node_id, operation, timestamp) VALUES ' +
    '(''user:alice'', ''role'', ''staff engineer'', ''A:1,B:0,C:1'', ''node-C'', ''update'', ''2025-01-20 10:30:00'')');

  Conn.ExecuteNonQuery('INSERT INTO conflicts (doc_key, field_name, value_a, value_b, vector_a, vector_b, node_a, node_b, status) VALUES ' +
    '(''user:alice'', ''role'', ''senior developer'', ''staff engineer'', ''A:2,B:0,C:0'', ''A:1,B:0,C:1'', ''node-A'', ''node-C'', ''pending'')');

  // Snapshots
  Conn.ExecuteNonQuery('INSERT INTO snapshots (snapshot_name, created_at, description) VALUES ' +
    '(''initial'', ''2025-01-20 10:00:00'', ''Initial state before any updates'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshots (snapshot_name, created_at, description) VALUES ' +
    '(''post-conflict'', ''2025-01-20 10:25:00'', ''After conflict resolution'')');

  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(1, ''user:alice'', ''name'', ''Alice Smith'', ''A:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(1, ''user:alice'', ''email'', ''alice@example.com'', ''A:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(1, ''user:alice'', ''role'', ''developer'', ''A:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(1, ''config:app'', ''theme'', ''light'', ''A:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(1, ''counter:visits'', ''count'', ''100'', ''A:1'')');

  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(2, ''user:alice'', ''name'', ''Alice S. Smith'', ''A:2,B:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(2, ''user:alice'', ''email'', ''alice@newdomain.io'', ''A:2,B:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(2, ''user:alice'', ''role'', ''senior developer'', ''A:2,B:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(2, ''config:app'', ''theme'', ''dark'', ''A:2,B:1'')');
  Conn.ExecuteNonQuery('INSERT INTO snapshot_data (snapshot_id, doc_key, field_name, field_value, version_vector) VALUES ' +
    '(2, ''counter:visits'', ''count'', ''135'', ''A:2,B:1'')');

  // Update version entries to final state
  Conn.ExecuteNonQuery('UPDATE version_entries SET version = 2 WHERE doc_key = ''user:alice'' AND node_id = ''node-A''');
  Conn.ExecuteNonQuery('UPDATE version_entries SET version = 1 WHERE doc_key = ''user:alice'' AND node_id = ''node-B''');
  Conn.ExecuteNonQuery('UPDATE version_entries SET version = 1 WHERE doc_key = ''user:alice'' AND node_id = ''node-C''');
  Conn.ExecuteNonQuery('UPDATE version_entries SET version = 2 WHERE doc_key = ''config:app'' AND node_id = ''node-A''');
  Conn.ExecuteNonQuery('UPDATE version_entries SET version = 1 WHERE doc_key = ''counter:visits'' AND node_id = ''node-A''');
  Conn.ExecuteNonQuery('UPDATE version_entries SET version = 1 WHERE doc_key = ''counter:visits'' AND node_id = ''node-B''');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Displays all documents with their fields, values, and version vectors ordered by key and field name. }
procedure Demo1_DocumentStore;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Document Store with Version Vectors ===');
  WriteLn;
  WriteLn('   Doc Key          Field        Value                    Version Vector');
  WriteLn('   ' + StringOfChar('-', 78));
  DS := Conn.ExecuteQuery(
    'SELECT doc_key, field_name, field_value, version_vector FROM documents ' +
    'ORDER BY doc_key, field_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-12s %-24s %s', [
        DS.FieldByName('doc_key').AsString,
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('field_value').AsString,
        DS.FieldByName('version_vector').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows the parsed version vector entries per document and node, and explains vector comparison rules for conflict detection. }
procedure Demo2_VersionVectors;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Version Vector State ===');
  WriteLn;
  WriteLn('   A version vector tracks causality across nodes.');
  WriteLn('   Format: node_id:version (higher = more recent on that node)');
  WriteLn;
  WriteLn('   Doc Key          Node     Version');
  WriteLn('   ' + StringOfChar('-', 45));
  DS := Conn.ExecuteQuery(
    'SELECT doc_key, node_id, version FROM version_entries ' +
    'ORDER BY doc_key, node_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-8s %d', [
        DS.FieldByName('doc_key').AsString,
        DS.FieldByName('node_id').AsString,
        DS.FieldByName('version').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   Comparison rules:');
  WriteLn('   - V1 dominates V2: all entries V1 >= V2, at least one V1 > V2');
  WriteLn('   - Conflict: neither dominates the other');
  WriteLn('   - Example: A:2,B:0 vs A:1,B:1 -> CONFLICT (A wins in A, B wins in B)');
  WriteLn;
end;

{ Displays the timeline of concurrent updates to user:alice showing how version vectors diverge and create conflicts. }
procedure Demo3_ConcurrentWrites;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Concurrent Writes (Conflict Detection) ===');
  WriteLn;
  WriteLn('   Timeline of user:alice updates:');
  WriteLn;
  WriteLn('   Time         Node     Field        Value                    Vector');
  WriteLn('   ' + StringOfChar('-', 80));
  DS := Conn.ExecuteQuery(
    'SELECT timestamp, node_id, field_name, field_value, version_vector, operation ' +
    'FROM doc_history WHERE doc_key = ''user:alice'' ORDER BY timestamp');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %-8s %-12s %-24s %s', [
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('node_id').AsString,
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('field_value').AsString,
        DS.FieldByName('version_vector').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   Conflict: A:2,B:0 vs A:1,B:1 (neither dominates)');
  WriteLn('   Node-B wrote based on A:1 without seeing A:2 -> concurrent!');
  WriteLn;
end;

{ Shows conflicts resolved using the last-writer-wins strategy where the later timestamp determines the winning value. }
procedure Demo4_LastWriterWins;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Last-Writer-Wins (LWW) Strategy ===');
  WriteLn;
  WriteLn('   Strategy: Accept the write with the latest timestamp.');
  WriteLn('   Used for: user:* documents');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT doc_key, field_name, value_a, value_b, ' +
    '  vector_a, vector_b, node_a, node_b, resolved_value ' +
    'FROM conflicts ' +
    'WHERE strategy = ''last-writer-wins'' ORDER BY doc_key, field_name');
  try
    WriteLn('   Field        Node-A Value            Node-B Value            Winner');
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-20s %-20s -> %s', [
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('value_a').AsString,
        DS.FieldByName('value_b').AsString,
        DS.FieldByName('resolved_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   LWW: Node-B wrote later (10:06 > 10:05) -> Node-B wins');
  WriteLn('   Trade-off: Simple but may lose important updates');
  WriteLn;
end;

{ Shows the config:app.theme conflict resolved with first-writer-wins strategy where the earlier write is preserved. }
procedure Demo5_FirstWriterWins;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. First-Writer-Wins (FWW) Strategy ===');
  WriteLn;
  WriteLn('   Strategy: Keep the first write, reject later conflicting writes.');
  WriteLn('   Used for: config:* documents (stable configuration)');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT doc_key, field_name, value_a, value_b, ' +
    '  vector_a, vector_b, node_a, node_b, resolved_value ' +
    'FROM conflicts ' +
    'WHERE strategy = ''first-writer-wins''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   config:app.theme conflict:', []));
      WriteLn(Format('   - Node-A wrote ''%s'' at 10:10 (vector %s)', [
        DS.FieldByName('value_a').AsString,
        DS.FieldByName('vector_a').AsString]));
      WriteLn(Format('   - Node-B wrote ''%s'' at 10:11 (vector %s)', [
        DS.FieldByName('value_b').AsString,
        DS.FieldByName('vector_b').AsString]));
      WriteLn(Format('   - Resolved: ''%s'' (first writer kept)', [
        DS.FieldByName('resolved_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   FWW: Prevents config flickering, ensures stability');
  WriteLn;
end;

{ Lists configured merge strategies per document/field pattern and shows counter merge-add and profile field-level merge examples. }
procedure Demo6_MergeStrategies;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Custom Merge Strategies ===');
  WriteLn;
  WriteLn('   Configured strategies:');
  WriteLn('   Pattern          Field     Strategy');
  WriteLn('   ' + StringOfChar('-', 50));
  DS := Conn.ExecuteQuery('SELECT * FROM merge_strategies ORDER BY doc_pattern, field_pattern');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-10s %s', [
        DS.FieldByName('doc_pattern').AsString,
        DS.FieldByName('field_pattern').AsString,
        DS.FieldByName('strategy').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Counter merge-add example:');
  WriteLn('   - Base value: 100');
  WriteLn('   - Node-A: 100 + 15 = 115 (A:2,B:0)');
  WriteLn('   - Node-B: 100 + 20 = 120 (A:1,B:1)');
  WriteLn('   - Merge: 100 + (115-100) + (120-100) = 100 + 15 + 20 = 135');

  WriteLn;
  WriteLn('   Profile field-level merge:');
  DS := Conn.ExecuteQuery(
    'SELECT field_name, value_a, value_b, strategy, resolved_value ' +
    'FROM conflicts WHERE doc_key = ''profile:bob'' ORDER BY field_name');
  try
    WriteLn('   Field     Value-A                  Value-B    Strategy       Resolved');
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-9s %-24s %-10s %-14s %s', [
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('value_a').AsString,
        DS.FieldByName('value_b').AsString,
        DS.FieldByName('strategy').AsString,
        DS.FieldByName('resolved_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists available snapshots and compares document field values between the initial snapshot and post-conflict resolution snapshot. }
procedure Demo7_ReadSnapshots;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Read Snapshots (Point-in-Time Views) ===');
  WriteLn;
  DS := Conn.ExecuteQuery('SELECT * FROM snapshots ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Snapshot: "%s" (%s)', [
        DS.FieldByName('snapshot_name').AsString,
        DS.FieldByName('created_at').AsString]));
      WriteLn(Format('   %s', [DS.FieldByName('description').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   Comparing snapshots:');
  WriteLn('   Doc Key          Field        Initial              Post-Conflict');
  WriteLn('   ' + StringOfChar('-', 72));
  DS := Conn.ExecuteQuery(
    'SELECT s1.doc_key, s1.field_name, ' +
    '  s1.field_value as initial_val, ' +
    '  s2.field_value as resolved_val, ' +
    '  s1.version_vector as v1, s2.version_vector as v2 ' +
    'FROM snapshot_data s1 ' +
    'JOIN snapshot_data s2 ON s1.doc_key = s2.doc_key AND s1.field_name = s2.field_name ' +
    '  AND s2.snapshot_id = 2 ' +
    'WHERE s1.snapshot_id = 1 ' +
    'ORDER BY s1.doc_key, s1.field_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-12s %-20s %-20s', [
        DS.FieldByName('doc_key').AsString,
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('initial_val').AsString,
        DS.FieldByName('resolved_val').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays the complete audit trail of all document changes with timestamp, node, operation type, key, field, and value. }
procedure Demo8_VersionHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Version History (Audit Trail) ===');
  WriteLn;
  WriteLn('   Full history of all document changes:');
  WriteLn;
  WriteLn('   Time         Node     Op       Doc Key          Field        Value');
  WriteLn('   ' + StringOfChar('-', 85));
  DS := Conn.ExecuteQuery(
    'SELECT timestamp, node_id, operation, doc_key, field_name, field_value ' +
    'FROM doc_history ORDER BY timestamp, doc_key, field_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %-8s %-8s %-16s %-12s %s', [
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('node_id').AsString,
        DS.FieldByName('operation').AsString,
        DS.FieldByName('doc_key').AsString,
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('field_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM doc_history');
  try
    DS.First;
    WriteLn(Format('   Total history entries: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows the 3-way version vector conflict for user:alice.role with detailed vector comparison and lists pending unresolved conflicts. }
procedure Demo9_MultiNodeVectors;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Multi-Node Version Vectors (3-way conflict) ===');
  WriteLn;
  WriteLn('   user:alice.role was updated by 3 nodes:');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT node_id, field_value, version_vector, timestamp ' +
    'FROM doc_history WHERE doc_key = ''user:alice'' AND field_name = ''role'' ' +
    'ORDER BY timestamp');
  try
    WriteLn('   Node     Value                Vector          Time');
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-20s %-15s %s', [
        DS.FieldByName('node_id').AsString,
        DS.FieldByName('field_value').AsString,
        DS.FieldByName('version_vector').AsString,
        DS.FieldByName('timestamp').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Version vector comparison:');
  WriteLn('   A:2,B:0,C:0 vs A:1,B:0,C:1:');
  WriteLn('     Node-A: 2 vs 1 -> A wins');
  WriteLn('     Node-C: 0 vs 1 -> C wins');
  WriteLn('     Result: CONFLICT (neither dominates)');
  WriteLn;
  WriteLn('   Pending conflicts:');
  DS := Conn.ExecuteQuery(
    'SELECT doc_key, field_name, value_a, value_b, ' +
    '  vector_a, vector_b, node_a, node_b ' +
    'FROM conflicts WHERE status = ''pending''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s.%s: "%s" (%s) vs "%s" (%s)', [
        DS.FieldByName('doc_key').AsString,
        DS.FieldByName('field_name').AsString,
        DS.FieldByName('value_a').AsString,
        DS.FieldByName('vector_a').AsString,
        DS.FieldByName('value_b').AsString,
        DS.FieldByName('vector_b').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays conflict statistics grouped by status, resolution strategy, node pair, and document key, plus a summary of each strategy's behavior. }
procedure Demo10_ConflictStats;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Conflict Resolution Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM conflicts GROUP BY status ORDER BY status');
  try
    WriteLn('   By status:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %-12s %d', [
        DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT strategy, COUNT(*) as cnt FROM conflicts ' +
    'WHERE strategy IS NOT NULL GROUP BY strategy ORDER BY cnt DESC');
  try
    WriteLn('   By strategy:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %-18s %d conflicts', [
        DS.FieldByName('strategy').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT node_a, node_b, COUNT(*) as cnt FROM conflicts ' +
    'GROUP BY node_a, node_b ORDER BY cnt DESC');
  try
    WriteLn('   By node pair:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s vs %s: %d conflicts', [
        DS.FieldByName('node_a').AsString,
        DS.FieldByName('node_b').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT doc_key, COUNT(*) as cnt FROM conflicts ' +
    'GROUP BY doc_key ORDER BY cnt DESC');
  try
    WriteLn('   By document:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %-20s %d conflicts', [
        DS.FieldByName('doc_key').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Resolution summary:');
  WriteLn('   - LWW: Latest timestamp wins (simple, may lose data)');
  WriteLn('   - FWW: First write preserved (stable, rejects later writes)');
  WriteLn('   - Merge-add: Combine increments from base (counters)');
  WriteLn('   - Longest-wins: Keep longer text (bio fields)');
  WriteLn('   - Max-wins: Keep higher numeric value (scores)');
end;

begin
  WriteLn('Example 141: Optimistic Concurrency - MVCC with Version Vectors');
  WriteLn(StringOfChar('=', 70));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_DocumentStore;
    Demo2_VersionVectors;
    Demo3_ConcurrentWrites;
    Demo4_LastWriterWins;
    Demo5_FirstWriterWins;
    Demo6_MergeStrategies;
    Demo7_ReadSnapshots;
    Demo8_VersionHistory;
    Demo9_MultiNodeVectors;
    Demo10_ConflictStats;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
