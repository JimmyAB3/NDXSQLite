{===============================================================================
  NDXSQLite Example 99 - Change Log
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Database change tracking with old/new values
  - Transaction-grouped change recording
  - Point-in-time data reconstruction
  - Change history queries and reporting
  - Account lifecycle tracking

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ChangeLog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, StrUtils, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // The changelog (WAL-style append-only log)
  Conn.ExecuteNonQuery(
    'CREATE TABLE changelog (' +
    '  seq INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  table_name TEXT NOT NULL,' +
    '  operation TEXT NOT NULL,' +  // INSERT, UPDATE, DELETE
    '  record_id TEXT NOT NULL,' +
    '  old_data TEXT,' +  // JSON-like representation of old values
    '  new_data TEXT,' +  // JSON-like representation of new values
    '  changed_by TEXT NOT NULL,' +
    '  changed_at TEXT NOT NULL,' +
    '  tx_id INTEGER NOT NULL DEFAULT 0' +
    ')');

  // Snapshots for point-in-time recovery
  Conn.ExecuteNonQuery(
    'CREATE TABLE snapshots (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  last_seq INTEGER NOT NULL,' +
    '  created_at TEXT NOT NULL,' +
    '  description TEXT' +
    ')');

  // The actual data table
  Conn.ExecuteNonQuery(
    'CREATE TABLE accounts (' +
    '  id TEXT PRIMARY KEY,' +
    '  owner TEXT NOT NULL,' +
    '  balance REAL NOT NULL DEFAULT 0,' +
    '  status TEXT NOT NULL DEFAULT ''active'',' +
    '  created_at TEXT NOT NULL' +
    ')');

  // Rebuilt state table (for replay demo)
  Conn.ExecuteNonQuery(
    'CREATE TABLE accounts_rebuilt (' +
    '  id TEXT PRIMARY KEY,' +
    '  owner TEXT NOT NULL,' +
    '  balance REAL NOT NULL DEFAULT 0,' +
    '  status TEXT NOT NULL DEFAULT ''active'',' +
    '  created_at TEXT NOT NULL' +
    ')');
end;

{ Returns the next tx id. }
function GetNextTxId: Integer;
begin
  DS := Conn.ExecuteQuery('SELECT COALESCE(MAX(tx_id), 0) + 1 as next_tx FROM changelog');
  try
    Result := DS.FieldByName('next_tx').AsInteger;
  finally
    DS.Free;
  end;
end;

{ Inserts a changelog entry with table name, operation type, record ID, old/new data values, actor, timestamp, and transaction ID. }
procedure LogChange(const TableName, Operation, RecordId, OldData, NewData, ChangedBy, ChangedAt: string; TxId: Integer);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO changelog (table_name, operation, record_id, old_data, new_data, changed_by, changed_at, tx_id) VALUES (' +
    '''' + TableName + ''', ' +
    '''' + Operation + ''', ' +
    '''' + RecordId + ''', ' +
    (IfThen(OldData = '', 'NULL', '''' + OldData + '''')) + ', ' +
    (IfThen(NewData = '', 'NULL', '''' + NewData + '''')) + ', ' +
    '''' + ChangedBy + ''', ' +
    '''' + ChangedAt + ''', ' +
    IntToStr(TxId) + ')');
end;

{ Creates an account with the specified details and logs the change. }
procedure CreateAccount(const Id, Owner, CreatedAt, ChangedBy: string; Balance: Double; TxId: Integer);
var
  NewData: string;
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO accounts (id, owner, balance, status, created_at) VALUES (' +
    '''' + Id + ''', ''' + Owner + ''', ' + FloatToStr(Balance) + ', ''active'', ''' + CreatedAt + ''')');

  NewData := 'owner=' + Owner + ';balance=' + FloatToStr(Balance) + ';status=active';
  LogChange('accounts', 'INSERT', Id, '', NewData, ChangedBy, CreatedAt, TxId);
end;

{ Reads the current balance for the given account, updates it to the new value, and logs the old and new balance to the changelog. }
procedure UpdateBalance(const Id, ChangedAt, ChangedBy: string; NewBalance: Double; TxId: Integer);
var
  OldBalance: Double;
  OldData, NewData: string;
begin
  DS := Conn.ExecuteQuery('SELECT balance FROM accounts WHERE id = ''' + Id + '''');
  try
    OldBalance := DS.FieldByName('balance').AsFloat;
  finally
    DS.Free;
  end;

  Conn.ExecuteNonQuery(
    'UPDATE accounts SET balance = ' + FloatToStr(NewBalance) + ' WHERE id = ''' + Id + '''');

  OldData := 'balance=' + FloatToStr(OldBalance);
  NewData := 'balance=' + FloatToStr(NewBalance);
  LogChange('accounts', 'UPDATE', Id, OldData, NewData, ChangedBy, ChangedAt, TxId);
end;

{ Reads the current status for the given account, updates it to the new status value, and logs the old and new status to the changelog. }
procedure UpdateStatus(const Id, NewStatus, ChangedAt, ChangedBy: string; TxId: Integer);
var
  OldStatus, OldData, NewData: string;
begin
  DS := Conn.ExecuteQuery('SELECT status FROM accounts WHERE id = ''' + Id + '''');
  try
    OldStatus := DS.FieldByName('status').AsString;
  finally
    DS.Free;
  end;

  Conn.ExecuteNonQuery(
    'UPDATE accounts SET status = ''' + NewStatus + ''' WHERE id = ''' + Id + '''');

  OldData := 'status=' + OldStatus;
  NewData := 'status=' + NewStatus;
  LogChange('accounts', 'UPDATE', Id, OldData, NewData, ChangedBy, ChangedAt, TxId);
end;

{ Captures the full account state (owner, balance, status), deletes the account record, and logs the deletion with the old data to the changelog. }
procedure CloseAccount(const Id, ChangedAt, ChangedBy: string; TxId: Integer);
var
  OldData: string;
begin
  DS := Conn.ExecuteQuery('SELECT owner, balance, status FROM accounts WHERE id = ''' + Id + '''');
  try
    OldData := 'owner=' + DS.FieldByName('owner').AsString +
      ';balance=' + FloatToStr(DS.FieldByName('balance').AsFloat) +
      ';status=' + DS.FieldByName('status').AsString;
  finally
    DS.Free;
  end;

  Conn.ExecuteNonQuery('DELETE FROM accounts WHERE id = ''' + Id + '''');
  LogChange('accounts', 'DELETE', Id, OldData, '', ChangedBy, ChangedAt, TxId);
end;

{ Queries and prints all accounts showing their ID, owner name, balance, and status in a formatted table with the given label. }
procedure ShowAccounts(const ALabel: string);
begin
  WriteLn('   ', ALabel, ':');
  DS := Conn.ExecuteQuery('SELECT id, owner, balance, status FROM accounts ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('id').AsString:8,
        ' ', DS.FieldByName('owner').AsString:14,
        ' $', DS.FieldByName('balance').AsFloat:10:2,
        '  ', DS.FieldByName('status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Queries and prints changelog entries (optionally limited) showing sequence number, operation, record ID, transaction ID, new/old data, actor, and timestamp. }
procedure ShowChangeLog(const ALabel: string; Limit: Integer);
begin
  WriteLn('   ', ALabel, ':');
  if Limit > 0 then
    DS := Conn.ExecuteQuery('SELECT seq, table_name, operation, record_id, old_data, new_data, changed_by, changed_at, tx_id FROM changelog ORDER BY seq LIMIT ' + IntToStr(Limit))
  else
    DS := Conn.ExecuteQuery('SELECT seq, table_name, operation, record_id, old_data, new_data, changed_by, changed_at, tx_id FROM changelog ORDER BY seq');
  try
    while not DS.EOF do
    begin
      Write('   [', DS.FieldByName('seq').AsInteger:3, '] ',
        DS.FieldByName('operation').AsString:7,
        ' ', DS.FieldByName('record_id').AsString:8,
        ' tx=', DS.FieldByName('tx_id').AsInteger);
      if not DS.FieldByName('new_data').IsNull then
        Write(' new={', DS.FieldByName('new_data').AsString, '}');
      if not DS.FieldByName('old_data').IsNull then
        Write(' old={', DS.FieldByName('old_data').AsString, '}');
      WriteLn(' by:', DS.FieldByName('changed_by').AsString,
        ' @', DS.FieldByName('changed_at').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Creates four accounts, then performs multiple transactions updating balances, freezing an account, closing an account, and applying final updates, printing the current state afterward. }
procedure DemoBasicChanges;
var
  TxId: Integer;
begin
  WriteLn('=== 1. Recording Changes to the Changelog ===');
  WriteLn('');

  // Transaction 1: Create accounts
  TxId := GetNextTxId;
  CreateAccount('acc-001', 'Alice Johnson', '2024-01-10T09:00:00', 'system', 1000.00, TxId);
  CreateAccount('acc-002', 'Bob Smith', '2024-01-10T09:00:00', 'system', 2500.00, TxId);
  CreateAccount('acc-003', 'Carol Williams', '2024-01-10T09:00:00', 'system', 750.00, TxId);
  CreateAccount('acc-004', 'Dave Brown', '2024-01-10T09:00:00', 'system', 3200.00, TxId);
  WriteLn('   Created 4 accounts (tx=', TxId, ')');

  // Transaction 2: Balance updates
  TxId := GetNextTxId;
  UpdateBalance('acc-001', '2024-01-15T10:00:00', 'teller_1', 1500.00, TxId);
  UpdateBalance('acc-002', '2024-01-15T10:05:00', 'teller_1', 2300.00, TxId);
  WriteLn('   Updated balances (tx=', TxId, ')');

  // Transaction 3: More updates
  TxId := GetNextTxId;
  UpdateBalance('acc-001', '2024-01-20T14:00:00', 'teller_2', 1800.00, TxId);
  UpdateBalance('acc-003', '2024-01-20T14:05:00', 'teller_2', 900.00, TxId);
  UpdateStatus('acc-004', 'frozen', '2024-01-20T15:00:00', 'compliance', TxId);
  WriteLn('   Updated balances and froze acc-004 (tx=', TxId, ')');

  // Transaction 4: Account closure
  TxId := GetNextTxId;
  UpdateBalance('acc-002', '2024-01-25T09:00:00', 'teller_1', 0.00, TxId);
  CloseAccount('acc-002', '2024-01-25T09:05:00', 'manager', TxId);
  WriteLn('   Closed acc-002 (tx=', TxId, ')');

  // Transaction 5: Final updates
  TxId := GetNextTxId;
  UpdateBalance('acc-001', '2024-01-30T11:00:00', 'online', 2100.00, TxId);
  UpdateBalance('acc-003', '2024-01-30T11:30:00', 'online', 1200.00, TxId);
  UpdateStatus('acc-004', 'active', '2024-01-30T16:00:00', 'compliance', TxId);
  WriteLn('   Final updates (tx=', TxId, ')');

  WriteLn('');
  ShowAccounts('Current state');
  WriteLn('');
end;

{ Prints the complete changelog ordered by sequence number and displays summary statistics including total entries, max sequence, and transaction count. }
procedure DemoFullChangelog;
begin
  WriteLn('=== 2. Full Changelog (Append-Only Log) ===');
  WriteLn('');
  ShowChangeLog('Complete changelog', 0);

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt, MAX(seq) as max_seq, COUNT(DISTINCT tx_id) as txs FROM changelog');
  try
    WriteLn('');
    WriteLn('   Total entries: ', DS.FieldByName('cnt').AsInteger,
      ', Max seq: ', DS.FieldByName('max_seq').AsInteger,
      ', Transactions: ', DS.FieldByName('txs').AsInteger);
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Replays changelog entries up to specified cutoff timestamps to reconstruct the accounts_rebuilt table, showing the account state at two different points in time (Jan 15 and Jan 20). }
procedure DemoPointInTimeRecovery;
var
  CutoffTime: string;
begin
  WriteLn('=== 3. Point-in-Time Recovery ===');
  WriteLn('');
  CutoffTime := '2024-01-15T10:05:00';
  WriteLn('   Rebuilding state as of ', CutoffTime, ':');
  WriteLn('');

  // Clear rebuilt table
  Conn.ExecuteNonQuery('DELETE FROM accounts_rebuilt');

  // Replay changelog up to the cutoff time
  DS := Conn.ExecuteQuery(
    'SELECT seq, operation, record_id, old_data, new_data, changed_at ' +
    'FROM changelog WHERE changed_at <= ''' + CutoffTime + ''' ORDER BY seq');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('operation').AsString = 'INSERT' then
      begin
        // Parse new_data to extract fields
        // Format: owner=X;balance=Y;status=Z
        Conn.ExecuteNonQuery(
          'INSERT OR REPLACE INTO accounts_rebuilt (id, owner, balance, status, created_at) ' +
          'SELECT ''' + DS.FieldByName('record_id').AsString + ''', ' +
          '  SUBSTR(new_data, INSTR(new_data, ''owner='') + 6, ' +
          '    INSTR(new_data, '';balance='') - INSTR(new_data, ''owner='') - 6), ' +
          '  CAST(SUBSTR(new_data, INSTR(new_data, '';balance='') + 9, ' +
          '    INSTR(new_data, '';status='') - INSTR(new_data, '';balance='') - 9) AS REAL), ' +
          '  SUBSTR(new_data, INSTR(new_data, '';status='') + 8), ' +
          '  ''' + DS.FieldByName('changed_at').AsString + '''' +
          ' FROM changelog WHERE seq = ' + DS.FieldByName('seq').AsString);
      end
      else if DS.FieldByName('operation').AsString = 'UPDATE' then
      begin
        // Apply update based on new_data content
        if Pos('balance=', DS.FieldByName('new_data').AsString) = 1 then
        begin
          Conn.ExecuteNonQuery(
            'UPDATE accounts_rebuilt SET balance = ' +
            Copy(DS.FieldByName('new_data').AsString, 9, Length(DS.FieldByName('new_data').AsString)) +
            ' WHERE id = ''' + DS.FieldByName('record_id').AsString + '''');
        end
        else if Pos('status=', DS.FieldByName('new_data').AsString) = 1 then
        begin
          Conn.ExecuteNonQuery(
            'UPDATE accounts_rebuilt SET status = ''' +
            Copy(DS.FieldByName('new_data').AsString, 8, Length(DS.FieldByName('new_data').AsString)) +
            ''' WHERE id = ''' + DS.FieldByName('record_id').AsString + '''');
        end;
      end
      else if DS.FieldByName('operation').AsString = 'DELETE' then
      begin
        Conn.ExecuteNonQuery(
          'DELETE FROM accounts_rebuilt WHERE id = ''' + DS.FieldByName('record_id').AsString + '''');
      end;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   State at ', CutoffTime, ':');
  DS := Conn.ExecuteQuery('SELECT id, owner, balance, status FROM accounts_rebuilt ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('id').AsString:8,
        ' ', DS.FieldByName('owner').AsString:14,
        ' $', DS.FieldByName('balance').AsFloat:10:2,
        '  ', DS.FieldByName('status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Also show state at a different time
  CutoffTime := '2024-01-20T15:00:00';
  WriteLn('');
  WriteLn('   Rebuilding state as of ', CutoffTime, ':');

  Conn.ExecuteNonQuery('DELETE FROM accounts_rebuilt');
  DS := Conn.ExecuteQuery(
    'SELECT seq, operation, record_id, new_data, changed_at ' +
    'FROM changelog WHERE changed_at <= ''' + CutoffTime + ''' ORDER BY seq');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('operation').AsString = 'INSERT' then
      begin
        Conn.ExecuteNonQuery(
          'INSERT OR REPLACE INTO accounts_rebuilt (id, owner, balance, status, created_at) ' +
          'SELECT ''' + DS.FieldByName('record_id').AsString + ''', ' +
          '  SUBSTR(new_data, INSTR(new_data, ''owner='') + 6, ' +
          '    INSTR(new_data, '';balance='') - INSTR(new_data, ''owner='') - 6), ' +
          '  CAST(SUBSTR(new_data, INSTR(new_data, '';balance='') + 9, ' +
          '    INSTR(new_data, '';status='') - INSTR(new_data, '';balance='') - 9) AS REAL), ' +
          '  SUBSTR(new_data, INSTR(new_data, '';status='') + 8), ' +
          '  ''' + DS.FieldByName('changed_at').AsString + '''' +
          ' FROM changelog WHERE seq = ' + DS.FieldByName('seq').AsString);
      end
      else if DS.FieldByName('operation').AsString = 'UPDATE' then
      begin
        if Pos('balance=', DS.FieldByName('new_data').AsString) = 1 then
          Conn.ExecuteNonQuery(
            'UPDATE accounts_rebuilt SET balance = ' +
            Copy(DS.FieldByName('new_data').AsString, 9, Length(DS.FieldByName('new_data').AsString)) +
            ' WHERE id = ''' + DS.FieldByName('record_id').AsString + '''')
        else if Pos('status=', DS.FieldByName('new_data').AsString) = 1 then
          Conn.ExecuteNonQuery(
            'UPDATE accounts_rebuilt SET status = ''' +
            Copy(DS.FieldByName('new_data').AsString, 8, Length(DS.FieldByName('new_data').AsString)) +
            ''' WHERE id = ''' + DS.FieldByName('record_id').AsString + '''');
      end
      else if DS.FieldByName('operation').AsString = 'DELETE' then
        Conn.ExecuteNonQuery(
          'DELETE FROM accounts_rebuilt WHERE id = ''' + DS.FieldByName('record_id').AsString + '''');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   State at ', CutoffTime, ':');
  DS := Conn.ExecuteQuery('SELECT id, owner, balance, status FROM accounts_rebuilt ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('id').AsString:8,
        ' ', DS.FieldByName('owner').AsString:14,
        ' $', DS.FieldByName('balance').AsFloat:10:2,
        '  ', DS.FieldByName('status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Demonstrates creating and comparing database snapshots at different points in time. }
procedure DemoSnapshots;
begin
  WriteLn('=== 4. Snapshots (Checkpoints) ===');
  WriteLn('');

  // Create snapshots at different points
  DS := Conn.ExecuteQuery('SELECT MAX(seq) as max_seq FROM changelog WHERE changed_at <= ''2024-01-15T23:59:59''');
  try
    Conn.ExecuteNonQuery(
      'INSERT INTO snapshots (name, last_seq, created_at, description) VALUES (' +
      '''snap_jan15'', ' + DS.FieldByName('max_seq').AsString + ', ''2024-01-15T23:59:59'', ''End of day Jan 15'')');
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT MAX(seq) as max_seq FROM changelog WHERE changed_at <= ''2024-01-20T23:59:59''');
  try
    Conn.ExecuteNonQuery(
      'INSERT INTO snapshots (name, last_seq, created_at, description) VALUES (' +
      '''snap_jan20'', ' + DS.FieldByName('max_seq').AsString + ', ''2024-01-20T23:59:59'', ''End of day Jan 20 - compliance freeze'')');
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT MAX(seq) as max_seq FROM changelog');
  try
    Conn.ExecuteNonQuery(
      'INSERT INTO snapshots (name, last_seq, created_at, description) VALUES (' +
      '''snap_current'', ' + DS.FieldByName('max_seq').AsString + ', ''2024-01-30T23:59:59'', ''Current state'')');
  finally
    DS.Free;
  end;

  WriteLn('   Snapshots:');
  DS := Conn.ExecuteQuery('SELECT name, last_seq, created_at, description FROM snapshots ORDER BY last_seq');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:14,
        ' seq<=', DS.FieldByName('last_seq').AsInteger:3,
        '  ', DS.FieldByName('created_at').AsString,
        '  ', DS.FieldByName('description').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Queries and prints the complete audit trail for specific accounts (acc-001 and acc-002), showing every operation, actor, timestamp, and data changes in chronological order. }
procedure DemoRecordHistory;
begin
  WriteLn('=== 5. Record History (Audit Trail) ===');
  WriteLn('');
  WriteLn('   Complete history for acc-001 (Alice):');

  DS := Conn.ExecuteQuery(
    'SELECT seq, operation, old_data, new_data, changed_by, changed_at ' +
    'FROM changelog WHERE record_id = ''acc-001'' ORDER BY seq');
  try
    while not DS.EOF do
    begin
      Write('   [', DS.FieldByName('seq').AsInteger:3, '] ',
        DS.FieldByName('operation').AsString:7, ' by ', DS.FieldByName('changed_by').AsString:10,
        ' @ ', DS.FieldByName('changed_at').AsString);
      if not DS.FieldByName('new_data').IsNull then
        Write('  -> ', DS.FieldByName('new_data').AsString);
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Complete history for acc-002 (Bob - closed):');

  DS := Conn.ExecuteQuery(
    'SELECT seq, operation, old_data, new_data, changed_by, changed_at ' +
    'FROM changelog WHERE record_id = ''acc-002'' ORDER BY seq');
  try
    while not DS.EOF do
    begin
      Write('   [', DS.FieldByName('seq').AsInteger:3, '] ',
        DS.FieldByName('operation').AsString:7, ' by ', DS.FieldByName('changed_by').AsString:10,
        ' @ ', DS.FieldByName('changed_at').AsString);
      if not DS.FieldByName('new_data').IsNull then
        Write('  -> ', DS.FieldByName('new_data').AsString);
      if (DS.FieldByName('operation').AsString = 'DELETE') and (not DS.FieldByName('old_data').IsNull) then
        Write('  was: {', DS.FieldByName('old_data').AsString, '}');
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Finds the last changelog entry for acc-004, reverts the account status to its previous value using old_data, and logs the reversal as a new changelog entry. }
procedure DemoUndoOperation;
var
  UndoSeq: Integer;
  UndoOp, UndoOld, UndoNew: string;
begin
  WriteLn('=== 6. Undo Last Change ===');
  WriteLn('');

  ShowAccounts('Before undo');
  WriteLn('');

  // Find the last change for acc-004 (status change to active)
  WriteLn('   Undoing last status change for acc-004...');
  DS := Conn.ExecuteQuery(
    'SELECT seq, operation, old_data, new_data FROM changelog ' +
    'WHERE record_id = ''acc-004'' ORDER BY seq DESC LIMIT 1');
  try
    if not DS.EOF then
    begin
      UndoSeq := DS.FieldByName('seq').AsInteger;
      UndoOp := DS.FieldByName('operation').AsString;
      UndoOld := DS.FieldByName('old_data').AsString;
      UndoNew := DS.FieldByName('new_data').AsString;
    end
    else
      UndoOld := '';
  finally
    DS.Free;
  end;

  if UndoOld <> '' then
  begin
    WriteLn('   Reverting seq=', UndoSeq, ': ', UndoOp, ' old={', UndoOld, '}');
    if Pos('status=', UndoOld) = 1 then
    begin
      Conn.ExecuteNonQuery(
        'UPDATE accounts SET status = ''' +
        Copy(UndoOld, 8, Length(UndoOld)) +
        ''' WHERE id = ''acc-004''');
      // Log the undo as a new change
      LogChange('accounts', 'UPDATE', 'acc-004',
        UndoNew, UndoOld,
        'undo_system', '2024-01-31T08:00:00', GetNextTxId);
    end;
  end;

  WriteLn('');
  ShowAccounts('After undo');
  WriteLn('');
end;

{ Queries the changelog grouped by tx_id and prints each transaction's operation count, operation types, actors, and start timestamp. }
procedure DemoTransactionGrouping;
begin
  WriteLn('=== 7. Transaction Grouping ===');
  WriteLn('');
  WriteLn('   Changes grouped by transaction:');

  DS := Conn.ExecuteQuery(
    'SELECT tx_id, COUNT(*) as ops, ' +
    '  MIN(changed_at) as start_at, MAX(changed_at) as end_at,' +
    '  GROUP_CONCAT(DISTINCT operation) as op_types,' +
    '  GROUP_CONCAT(DISTINCT changed_by) as actors ' +
    'FROM changelog GROUP BY tx_id ORDER BY tx_id');
  try
    while not DS.EOF do
    begin
      WriteLn('   TX #', DS.FieldByName('tx_id').AsInteger,
        ': ', DS.FieldByName('ops').AsInteger, ' ops',
        ' [', DS.FieldByName('op_types').AsString, ']',
        ' by ', DS.FieldByName('actors').AsString,
        ' (', DS.FieldByName('start_at').AsString, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Queries and prints changelog analytics: operation counts by type, change counts by actor with records affected, and most-changed records with their first and last change timestamps. }
procedure DemoChangeStatistics;
begin
  WriteLn('=== 8. Change Statistics ===');
  WriteLn('');

  WriteLn('   Operations by type:');
  DS := Conn.ExecuteQuery(
    'SELECT operation, COUNT(*) as cnt FROM changelog GROUP BY operation ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('operation').AsString:8, ': ', DS.FieldByName('cnt').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Changes by actor:');
  DS := Conn.ExecuteQuery(
    'SELECT changed_by, COUNT(*) as cnt, ' +
    '  COUNT(DISTINCT record_id) as records_affected ' +
    'FROM changelog GROUP BY changed_by ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('changed_by').AsString:12,
        ': ', DS.FieldByName('cnt').AsInteger, ' changes',
        ', ', DS.FieldByName('records_affected').AsInteger, ' records');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Most changed records:');
  DS := Conn.ExecuteQuery(
    'SELECT record_id, COUNT(*) as change_count, ' +
    '  MIN(changed_at) as first_change, MAX(changed_at) as last_change ' +
    'FROM changelog GROUP BY record_id ORDER BY change_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('record_id').AsString:8,
        ': ', DS.FieldByName('change_count').AsInteger, ' changes',
        ' (', DS.FieldByName('first_change').AsString, ' to ', DS.FieldByName('last_change').AsString, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Removes redundant early changelog entries (seq <= 4) for records that have newer entries, then reports how many entries were removed by the compaction. }
procedure DemoLogCompaction;
var
  Before, After: Integer;
begin
  WriteLn('=== 9. Log Compaction ===');
  WriteLn('');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM changelog');
  try Before := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  WriteLn('   Changelog entries before compaction: ', Before);

  // Compact: remove entries older than snap_jan20 (keep recent entries)
  WriteLn('   Compacting entries before snapshot snap_jan20 (seq <= 6)...');
  WriteLn('   (Keeping last state per record for recovery)');

  // In practice, you'd keep the most recent entry per record before the cutoff
  // Here we just demonstrate removing very old entries before first snapshot
  Conn.ExecuteNonQuery(
    'DELETE FROM changelog WHERE seq <= 4 AND ' +
    'record_id IN (SELECT record_id FROM changelog WHERE seq > 4)');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM changelog');
  try After := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  WriteLn('   Changelog entries after compaction: ', After);
  WriteLn('   Removed: ', Before - After, ' redundant entries');
  WriteLn('');
end;

{ Queries and prints all changelog entries between Jan 15 and Jan 25, then shows the total number of changes and affected records in that time range. }
procedure DemoDiffBetweenPoints;
begin
  WriteLn('=== 10. Diff Between Two Points in Time ===');
  WriteLn('');
  WriteLn('   Changes between Jan 15 and Jan 25:');

  DS := Conn.ExecuteQuery(
    'SELECT seq, operation, record_id, old_data, new_data, changed_by, changed_at ' +
    'FROM changelog ' +
    'WHERE changed_at > ''2024-01-15T23:59:59'' AND changed_at <= ''2024-01-25T23:59:59'' ' +
    'ORDER BY seq');
  try
    while not DS.EOF do
    begin
      Write('   [', DS.FieldByName('seq').AsInteger:3, '] ',
        DS.FieldByName('operation').AsString:7, ' ',
        DS.FieldByName('record_id').AsString:8,
        ' by ', DS.FieldByName('changed_by').AsString);
      if not DS.FieldByName('new_data').IsNull then
        Write(' -> {', DS.FieldByName('new_data').AsString, '}');
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as changes, COUNT(DISTINCT record_id) as affected ' +
    'FROM changelog ' +
    'WHERE changed_at > ''2024-01-15T23:59:59'' AND changed_at <= ''2024-01-25T23:59:59''');
  try
    WriteLn('');
    WriteLn('   Total: ', DS.FieldByName('changes').AsInteger, ' changes affecting ',
      DS.FieldByName('affected').AsInteger, ' records');
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Prints final summary with total changelog entries, snapshot count, and transaction count, followed by a list of all changelog patterns exercised in this example. }
procedure PrintSummary;
var
  LogCount, SnapCount, TxCount: Integer;
begin
  WriteLn('=== ChangeLog Summary ===');
  WriteLn('');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM changelog');
  try LogCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM snapshots');
  try SnapCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(DISTINCT tx_id) as cnt FROM changelog');
  try TxCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  WriteLn('   Changelog entries: ', LogCount, ', Snapshots: ', SnapCount, ', Transactions: ', TxCount);
  WriteLn('');
  WriteLn('   Patterns demonstrated:');
  WriteLn('   - Append-only changelog (WAL-style)');
  WriteLn('   - Operation types: INSERT, UPDATE, DELETE');
  WriteLn('   - Old/new data capture for each change');
  WriteLn('   - Transaction grouping (tx_id)');
  WriteLn('   - Point-in-time recovery (replay to timestamp)');
  WriteLn('   - Snapshots/checkpoints');
  WriteLn('   - Record history (audit trail)');
  WriteLn('   - Undo operations (reverse from old_data)');
  WriteLn('   - Change statistics and analytics');
  WriteLn('   - Log compaction');
  WriteLn('   - Diff between time points');
  WriteLn('');
end;

begin
  WriteLn('Example 99: ChangeLog - Modification Journal with Replay');
  WriteLn(StringOfChar('=', 58));
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    SetupDatabase;
    WriteLn('Database setup complete.');
    WriteLn('');

    DemoBasicChanges;
    DemoFullChangelog;
    DemoPointInTimeRecovery;
    DemoSnapshots;
    DemoRecordHistory;
    DemoUndoOperation;
    DemoTransactionGrouping;
    DemoChangeStatistics;
    DemoLogCompaction;
    DemoDiffBetweenPoints;
    PrintSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('Done.');
end.
