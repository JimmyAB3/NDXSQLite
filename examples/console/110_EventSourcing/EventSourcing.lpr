{===============================================================================
  NDXSQLite Example 110 - Event Sourcing
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Append-only event store
  - Aggregate state rebuild from events
  - Point-in-time and version-based snapshots
  - Event projection and replay
  - Account lifecycle with event sourcing

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program EventSourcing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

{
  Example 110: Event Sourcing
  - Append-only event store
  - Aggregate rebuild from events
  - Snapshots for performance optimization
  - Temporal queries (state at any point in time)
  - Event projections (materialized views)

  Domain: Bank Accounts
  Events: AccountOpened, MoneyDeposited, MoneyWithdrawn, AccountClosed
}

var
  Conn: TNDXSQLiteConnection;

// ============================================================================
// Schema Setup
// ============================================================================

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Event store: append-only log of all events
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_store (' +
    '  sequence_id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  aggregate_id TEXT NOT NULL,' +
    '  aggregate_type TEXT NOT NULL,' +
    '  event_type TEXT NOT NULL,' +
    '  event_data TEXT NOT NULL,' +  // JSON-like key=value pairs
    '  event_timestamp TEXT NOT NULL,' +
    '  version INTEGER NOT NULL' +   // per-aggregate version
    ')');

  // Snapshots: cached aggregate state at a point in time
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS snapshots (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  aggregate_id TEXT NOT NULL,' +
    '  aggregate_type TEXT NOT NULL,' +
    '  version INTEGER NOT NULL,' +
    '  state_data TEXT NOT NULL,' +    // serialized state
    '  created_at TEXT NOT NULL' +
    ')');

  // Projection: materialized view of current account balances
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS account_projection (' +
    '  account_id TEXT PRIMARY KEY,' +
    '  owner TEXT NOT NULL,' +
    '  balance REAL NOT NULL DEFAULT 0,' +
    '  status TEXT NOT NULL DEFAULT ''open'',' +
    '  last_version INTEGER NOT NULL DEFAULT 0,' +
    '  updated_at TEXT NOT NULL' +
    ')');

  // Projection: transaction history (deposits/withdrawals)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS transaction_projection (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  account_id TEXT NOT NULL,' +
    '  transaction_type TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  balance_after REAL NOT NULL,' +
    '  description TEXT,' +
    '  event_timestamp TEXT NOT NULL' +
    ')');
end;

// ============================================================================
// Event Store Operations
// ============================================================================

{ Queries the highest version number for the given aggregate and returns the
  next sequential version value. }
function GetNextVersion(const AggregateId: string): Integer;
var
  V: Variant;
begin
  V := Conn.ExecuteScalar(Format(
    'SELECT COALESCE(MAX(version), 0) FROM event_store WHERE aggregate_id = ''%s''',
    [AggregateId]));
  Result := Integer(V) + 1;
end;

{ Inserts a new event into the event_store with the aggregate info, event type,
  serialized data, timestamp, and auto-incremented version number. }
procedure AppendEvent(const AggregateId, AggregateType, EventType, EventData, Timestamp: string);
var
  Version: Integer;
begin
  Version := GetNextVersion(AggregateId);
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO event_store (aggregate_id, aggregate_type, event_type, event_data, event_timestamp, version) ' +
    'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %d)',
    [AggregateId, AggregateType, EventType, EventData, Timestamp, Version]));
end;

// ============================================================================
// Aggregate: BankAccount
// ============================================================================

type
  TAccountState = record
    AccountId: string;
    Owner: string;
    Balance: Double;
    Status: string;  // open, closed
    Version: Integer;
  end;

{ Initializes and returns an empty account state record. }
function NewAccountState: TAccountState;
begin
  Result.AccountId := '';
  Result.Owner := '';
  Result.Balance := 0;
  Result.Status := '';
  Result.Version := 0;
end;

// Parse simple key=value format: "key1=val1;key2=val2"
{ Extracts a value from a key=value formatted string. }
function GetField(const Data, Key: string): string;
var
  P, PEnd: Integer;
  Search: string;
begin
  Result := '';
  Search := Key + '=';
  P := Pos(Search, Data);
  if P > 0 then
  begin
    P := P + Length(Search);
    PEnd := Pos(';', Copy(Data, P, Length(Data)));
    if PEnd > 0 then
      Result := Copy(Data, P, PEnd - 1)
    else
      Result := Copy(Data, P, Length(Data));
  end;
end;

// Apply a single event to the account state
{ Updates the account state by processing a single event: AccountOpened sets
  owner and status, deposits add to balance, withdrawals subtract, and
  AccountClosed changes status to closed. }
function ApplyEvent(State: TAccountState; const EventType, EventData: string; Version: Integer): TAccountState;
var
  Amount: Double;
begin
  Result := State;
  Result.Version := Version;

  if EventType = 'AccountOpened' then
  begin
    Result.AccountId := GetField(EventData, 'account_id');
    Result.Owner := GetField(EventData, 'owner');
    Result.Balance := 0;
    Result.Status := 'open';
  end
  else if EventType = 'MoneyDeposited' then
  begin
    Amount := StrToFloat(GetField(EventData, 'amount'));
    Result.Balance := Result.Balance + Amount;
  end
  else if EventType = 'MoneyWithdrawn' then
  begin
    Amount := StrToFloat(GetField(EventData, 'amount'));
    Result.Balance := Result.Balance - Amount;
  end
  else if EventType = 'AccountClosed' then
  begin
    Result.Status := 'closed';
  end;
end;

// Rebuild aggregate from all events
{ Queries all events for the aggregate in version order and applies each one
  sequentially to reconstruct the current account state. }
function RebuildAggregate(const AggregateId: string): TAccountState;
var
  DS: TDataSet;
begin
  Result := NewAccountState;
  DS := Conn.ExecuteQuery(Format(
    'SELECT event_type, event_data, version FROM event_store ' +
    'WHERE aggregate_id = ''%s'' ORDER BY version ASC', [AggregateId]));
  try
    while not DS.EOF do
    begin
      Result := ApplyEvent(Result,
        DS.FieldByName('event_type').AsString,
        DS.FieldByName('event_data').AsString,
        DS.FieldByName('version').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// Rebuild aggregate up to a specific version (temporal query)
{ Reconstructs the account state as it existed at a specific version by
  applying only events up to and including that version number. }
function RebuildAggregateAtVersion(const AggregateId: string; MaxVersion: Integer): TAccountState;
var
  DS: TDataSet;
begin
  Result := NewAccountState;
  DS := Conn.ExecuteQuery(Format(
    'SELECT event_type, event_data, version FROM event_store ' +
    'WHERE aggregate_id = ''%s'' AND version <= %d ORDER BY version ASC',
    [AggregateId, MaxVersion]));
  try
    while not DS.EOF do
    begin
      Result := ApplyEvent(Result,
        DS.FieldByName('event_type').AsString,
        DS.FieldByName('event_data').AsString,
        DS.FieldByName('version').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// Rebuild aggregate up to a specific timestamp
{ Reconstructs the account state as it existed at a specific point in time by
  applying only events with timestamps on or before the given timestamp. }
function RebuildAggregateAtTime(const AggregateId, Timestamp: string): TAccountState;
var
  DS: TDataSet;
begin
  Result := NewAccountState;
  DS := Conn.ExecuteQuery(Format(
    'SELECT event_type, event_data, version FROM event_store ' +
    'WHERE aggregate_id = ''%s'' AND event_timestamp <= ''%s'' ORDER BY version ASC',
    [AggregateId, Timestamp]));
  try
    while not DS.EOF do
    begin
      Result := ApplyEvent(Result,
        DS.FieldByName('event_type').AsString,
        DS.FieldByName('event_data').AsString,
        DS.FieldByName('version').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// ============================================================================
// Snapshot Operations
// ============================================================================

{ Serializes the current account state and inserts it into the snapshots table
  with the version number for later fast recovery. }
procedure SaveSnapshot(const State: TAccountState);
var
  StateData: string;
begin
  StateData := Format('owner=%s;balance=%.2f;status=%s',
    [State.Owner, State.Balance, State.Status]);
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO snapshots (aggregate_id, aggregate_type, version, state_data, created_at) ' +
    'VALUES (''%s'', ''BankAccount'', %d, ''%s'', datetime(''now''))',
    [State.AccountId, State.Version, StateData]));
end;

{ Retrieves the most recent snapshot for the aggregate and deserializes it into
  an account state record, or returns an empty state if none exists. }
function LoadSnapshot(const AggregateId: string): TAccountState;
var
  DS: TDataSet;
  StateData: string;
  BalStr: string;
begin
  Result := NewAccountState;
  DS := Conn.ExecuteQuery(Format(
    'SELECT version, state_data FROM snapshots ' +
    'WHERE aggregate_id = ''%s'' ORDER BY version DESC LIMIT 1', [AggregateId]));
  try
    if not DS.EOF then
    begin
      Result.AccountId := AggregateId;
      Result.Version := DS.FieldByName('version').AsInteger;
      StateData := DS.FieldByName('state_data').AsString;
      Result.Owner := GetField(StateData, 'owner');
      BalStr := GetField(StateData, 'balance');
      if BalStr <> '' then
        Result.Balance := StrToFloat(BalStr)
      else
        Result.Balance := 0;
      Result.Status := GetField(StateData, 'status');
    end;
  finally
    DS.Free;
  end;
end;

// Rebuild from snapshot + remaining events
{ Loads the latest snapshot and applies only events with versions higher than
  the snapshot version, avoiding full replay of the entire event history. }
function RebuildFromSnapshot(const AggregateId: string): TAccountState;
var
  DS: TDataSet;
begin
  Result := LoadSnapshot(AggregateId);
  if Result.Version = 0 then
  begin
    // No snapshot, rebuild from scratch
    Result := RebuildAggregate(AggregateId);
    Exit;
  end;

  // Apply events after snapshot version
  DS := Conn.ExecuteQuery(Format(
    'SELECT event_type, event_data, version FROM event_store ' +
    'WHERE aggregate_id = ''%s'' AND version > %d ORDER BY version ASC',
    [AggregateId, Result.Version]));
  try
    while not DS.EOF do
    begin
      Result := ApplyEvent(Result,
        DS.FieldByName('event_type').AsString,
        DS.FieldByName('event_data').AsString,
        DS.FieldByName('version').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// ============================================================================
// Projection Updates
// ============================================================================

{ Inserts or replaces the account_projection record with the current aggregate
  state including owner, balance, status, and version. }
procedure UpdateAccountProjection(const State: TAccountState; const Timestamp: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT OR REPLACE INTO account_projection (account_id, owner, balance, status, last_version, updated_at) ' +
    'VALUES (''%s'', ''%s'', %.2f, ''%s'', %d, ''%s'')',
    [State.AccountId, State.Owner, State.Balance, State.Status, State.Version, Timestamp]));
end;

{ Inserts a transaction_projection record capturing the transaction type, amount,
  resulting balance, description, and timestamp. }
procedure AddTransactionProjection(const AccountId, TxType: string; Amount, BalanceAfter: Double;
  const Description, Timestamp: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO transaction_projection (account_id, transaction_type, amount, balance_after, description, event_timestamp) ' +
    'VALUES (''%s'', ''%s'', %.2f, %.2f, ''%s'', ''%s'')',
    [AccountId, TxType, Amount, BalanceAfter, Description, Timestamp]));
end;

// ============================================================================
// Domain Commands (emit events)
// ============================================================================

{ Appends an AccountOpened event with the account ID and owner, then rebuilds
  the aggregate and updates the account projection. }
procedure OpenAccount(const AccountId, Owner, Timestamp: string);
var
  EventData: string;
  State: TAccountState;
begin
  EventData := Format('account_id=%s;owner=%s', [AccountId, Owner]);
  AppendEvent(AccountId, 'BankAccount', 'AccountOpened', EventData, Timestamp);
  State := RebuildAggregate(AccountId);
  UpdateAccountProjection(State, Timestamp);
end;

{ Appends a MoneyDeposited event, rebuilds the aggregate, and updates both the
  account and transaction projections with the new balance. }
procedure Deposit(const AccountId: string; Amount: Double; const Description, Timestamp: string);
var
  EventData: string;
  State: TAccountState;
begin
  EventData := Format('amount=%.2f;description=%s', [Amount, Description]);
  AppendEvent(AccountId, 'BankAccount', 'MoneyDeposited', EventData, Timestamp);
  State := RebuildAggregate(AccountId);
  UpdateAccountProjection(State, Timestamp);
  AddTransactionProjection(AccountId, 'deposit', Amount, State.Balance, Description, Timestamp);
end;

{ Validates sufficient funds, then appends a MoneyWithdrawn event, rebuilds
  the aggregate, and updates the projections. Prints error if balance too low. }
procedure Withdraw(const AccountId: string; Amount: Double; const Description, Timestamp: string);
var
  EventData: string;
  State: TAccountState;
begin
  // Business rule: check sufficient funds
  State := RebuildAggregate(AccountId);
  if State.Balance < Amount then
  begin
    WriteLn(Format('   ERROR: Insufficient funds (balance: $%.2f, requested: $%.2f)', [State.Balance, Amount]));
    Exit;
  end;

  EventData := Format('amount=%.2f;description=%s', [Amount, Description]);
  AppendEvent(AccountId, 'BankAccount', 'MoneyWithdrawn', EventData, Timestamp);
  State := RebuildAggregate(AccountId);
  UpdateAccountProjection(State, Timestamp);
  AddTransactionProjection(AccountId, 'withdrawal', Amount, State.Balance, Description, Timestamp);
end;

{ Validates balance is zero, then appends an AccountClosed event and updates
  the projection. Prints error if account has remaining balance. }
procedure CloseAccount(const AccountId, Timestamp: string);
var
  State: TAccountState;
begin
  State := RebuildAggregate(AccountId);
  if State.Balance <> 0 then
  begin
    WriteLn(Format('   ERROR: Cannot close account with balance $%.2f', [State.Balance]));
    Exit;
  end;
  AppendEvent(AccountId, 'BankAccount', 'AccountClosed', 'reason=customer_request', Timestamp);
  State := RebuildAggregate(AccountId);
  UpdateAccountProjection(State, Timestamp);
end;

// ============================================================================
// Demo 1: Event Store Basics
// ============================================================================

{ Opens accounts, performs deposits and withdrawals, then prints the full
  contents of the append-only event store with sequence IDs and versions. }
procedure DemoEventStore;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Event Store (Append-Only Log) ===');
  WriteLn;

  // Open accounts
  OpenAccount('ACC-001', 'Alice Johnson', '2024-01-15 09:00:00');
  WriteLn('   Opened account ACC-001 for Alice Johnson');

  OpenAccount('ACC-002', 'Bob Smith', '2024-01-15 09:30:00');
  WriteLn('   Opened account ACC-002 for Bob Smith');

  // Perform transactions
  Deposit('ACC-001', 5000, 'Initial deposit', '2024-01-15 10:00:00');
  WriteLn('   ACC-001: Deposited $5000.00');

  Deposit('ACC-001', 2500, 'Salary', '2024-02-01 09:00:00');
  WriteLn('   ACC-001: Deposited $2500.00');

  Withdraw('ACC-001', 1200, 'Rent payment', '2024-02-05 14:00:00');
  WriteLn('   ACC-001: Withdrew $1200.00');

  Deposit('ACC-002', 10000, 'Initial deposit', '2024-01-16 11:00:00');
  WriteLn('   ACC-002: Deposited $10000.00');

  Withdraw('ACC-002', 3500, 'Car payment', '2024-02-10 16:00:00');
  WriteLn('   ACC-002: Withdrew $3500.00');

  // Show event store contents
  WriteLn;
  WriteLn('   Event Store Contents:');
  DS := Conn.ExecuteQuery(
    'SELECT sequence_id, aggregate_id, event_type, version, event_timestamp ' +
    'FROM event_store ORDER BY sequence_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%d] %s v%d: %s (%s)', [
        DS.FieldByName('sequence_id').AsInteger,
        DS.FieldByName('aggregate_id').AsString,
        DS.FieldByName('version').AsInteger,
        DS.FieldByName('event_type').AsString,
        DS.FieldByName('event_timestamp').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 2: Aggregate Rebuild
// ============================================================================

{ Rebuilds account states from their event histories and prints the owner,
  balance, status, and version for each account. }
procedure DemoAggregateRebuild;
var
  State: TAccountState;
begin
  WriteLn('=== 2. Aggregate Rebuild from Events ===');
  WriteLn;

  // Rebuild ACC-001
  State := RebuildAggregate('ACC-001');
  WriteLn('   ACC-001 (rebuilt from events):');
  WriteLn(Format('     Owner:   %s', [State.Owner]));
  WriteLn(Format('     Balance: $%.2f', [State.Balance]));
  WriteLn(Format('     Status:  %s', [State.Status]));
  WriteLn(Format('     Version: %d', [State.Version]));

  // Rebuild ACC-002
  State := RebuildAggregate('ACC-002');
  WriteLn;
  WriteLn('   ACC-002 (rebuilt from events):');
  WriteLn(Format('     Owner:   %s', [State.Owner]));
  WriteLn(Format('     Balance: $%.2f', [State.Balance]));
  WriteLn(Format('     Status:  %s', [State.Status]));
  WriteLn(Format('     Version: %d', [State.Version]));
  WriteLn;
end;

// ============================================================================
// Demo 3: Temporal Queries
// ============================================================================

{ Shows account balance at various version numbers and at specific timestamps,
  demonstrating how to query historical state at any point in time. }
procedure DemoTemporalQueries;
var
  State: TAccountState;
begin
  WriteLn('=== 3. Temporal Queries (State at Any Point in Time) ===');
  WriteLn;

  // ACC-001 at different points in time
  WriteLn('   ACC-001 balance over time:');

  State := RebuildAggregateAtVersion('ACC-001', 1);
  WriteLn(Format('     After v1 (opened):     $%.2f', [State.Balance]));

  State := RebuildAggregateAtVersion('ACC-001', 2);
  WriteLn(Format('     After v2 (deposit):    $%.2f', [State.Balance]));

  State := RebuildAggregateAtVersion('ACC-001', 3);
  WriteLn(Format('     After v3 (salary):     $%.2f', [State.Balance]));

  State := RebuildAggregateAtVersion('ACC-001', 4);
  WriteLn(Format('     After v4 (rent):       $%.2f', [State.Balance]));

  // Time-based query
  WriteLn;
  WriteLn('   ACC-001 balance at specific dates:');

  State := RebuildAggregateAtTime('ACC-001', '2024-01-15 12:00:00');
  WriteLn(Format('     Jan 15, 2024 noon:     $%.2f', [State.Balance]));

  State := RebuildAggregateAtTime('ACC-001', '2024-02-01 12:00:00');
  WriteLn(Format('     Feb 01, 2024 noon:     $%.2f', [State.Balance]));

  State := RebuildAggregateAtTime('ACC-001', '2024-02-28 23:59:59');
  WriteLn(Format('     Feb 28, 2024 end:      $%.2f', [State.Balance]));
  WriteLn;
end;

// ============================================================================
// Demo 4: Snapshots
// ============================================================================

{ Demonstrates event sourcing snapshots for performance optimization. }
procedure DemoSnapshots;
var
  State, FromSnap: TAccountState;
  EventsBefore, EventsAfter: Integer;
begin
  WriteLn('=== 4. Snapshots (Performance Optimization) ===');
  WriteLn;

  // Take snapshot of ACC-001 current state
  State := RebuildAggregate('ACC-001');
  SaveSnapshot(State);
  WriteLn(Format('   Snapshot saved for ACC-001 at version %d (balance: $%.2f)', [State.Version, State.Balance]));

  // Add more events after snapshot
  Deposit('ACC-001', 3000, 'Bonus', '2024-03-01 09:00:00');
  WriteLn('   ACC-001: Deposited $3000.00 (after snapshot)');

  Withdraw('ACC-001', 800, 'Utilities', '2024-03-05 10:00:00');
  WriteLn('   ACC-001: Withdrew $800.00 (after snapshot)');

  // Rebuild from snapshot (only applies events after snapshot version)
  FromSnap := RebuildFromSnapshot('ACC-001');
  WriteLn;
  WriteLn('   Rebuild from snapshot:');
  WriteLn(Format('     Balance: $%.2f', [FromSnap.Balance]));
  WriteLn(Format('     Version: %d', [FromSnap.Version]));

  // Compare: full rebuild vs snapshot rebuild
  State := RebuildAggregate('ACC-001');
  WriteLn;
  WriteLn('   Full rebuild vs Snapshot rebuild:');
  WriteLn(Format('     Full rebuild:     $%.2f (v%d)', [State.Balance, State.Version]));
  WriteLn(Format('     From snapshot:    $%.2f (v%d)', [FromSnap.Balance, FromSnap.Version]));
  WriteLn(Format('     Match: %s', [BoolToStr(Abs(State.Balance - FromSnap.Balance) < 0.01, 'YES', 'NO')]));

  // Show event count saved by snapshot
  EventsBefore := Integer(Conn.ExecuteScalar(Format(
    'SELECT COUNT(*) FROM event_store WHERE aggregate_id = ''ACC-001'' AND version <= %d',
    [FromSnap.Version - 2])));  // events skipped by snapshot
  EventsAfter := Integer(Conn.ExecuteScalar(Format(
    'SELECT COUNT(*) FROM event_store WHERE aggregate_id = ''ACC-001'' AND version > %d',
    [FromSnap.Version - 2])));
  WriteLn(Format('     Events skipped by snapshot: %d, Events replayed: %d', [EventsBefore, EventsAfter]));
  WriteLn;
end;

// ============================================================================
// Demo 5: Business Rules (Invariant Enforcement)
// ============================================================================

{ Attempts invalid operations (overdraw and closing with balance) to show how
  business rules are enforced, then successfully closes a zero-balance account. }
procedure DemoBusinessRules;
var
  State: TAccountState;
begin
  WriteLn('=== 5. Business Rules (Invariant Enforcement) ===');
  WriteLn;

  // Try to overdraw
  State := RebuildAggregate('ACC-001');
  WriteLn(Format('   ACC-001 current balance: $%.2f', [State.Balance]));
  WriteLn('   Attempting to withdraw $50000...');
  Withdraw('ACC-001', 50000, 'Large withdrawal', '2024-03-10 09:00:00');

  // Try to close account with balance
  WriteLn;
  WriteLn(Format('   Attempting to close ACC-001 (balance: $%.2f)...', [State.Balance]));
  CloseAccount('ACC-001', '2024-03-10 10:00:00');

  // Successfully close an empty account
  WriteLn;
  OpenAccount('ACC-003', 'Charlie Brown', '2024-03-10 11:00:00');
  WriteLn('   Opened account ACC-003 for Charlie Brown');
  WriteLn('   Attempting to close ACC-003 (balance: $0.00)...');
  CloseAccount('ACC-003', '2024-03-10 12:00:00');
  State := RebuildAggregate('ACC-003');
  WriteLn(Format('   ACC-003 status: %s', [State.Status]));
  WriteLn;
end;

// ============================================================================
// Demo 6: Projections (Materialized Views)
// ============================================================================

{ Demonstrates materialized view projections from the event stream. }
procedure DemoProjections;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Projections (Materialized Views) ===');
  WriteLn;

  // Account balances projection
  WriteLn('   Current Account Balances:');
  DS := Conn.ExecuteQuery(
    'SELECT account_id, owner, balance, status, last_version FROM account_projection ORDER BY account_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s | %-15s | $%9.2f | %-6s | v%d', [
        DS.FieldByName('account_id').AsString,
        DS.FieldByName('owner').AsString,
        DS.FieldByName('balance').AsFloat,
        DS.FieldByName('status').AsString,
        DS.FieldByName('last_version').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Transaction history projection
  WriteLn;
  WriteLn('   Transaction History (ACC-001):');
  DS := Conn.ExecuteQuery(
    'SELECT transaction_type, amount, balance_after, description, event_timestamp ' +
    'FROM transaction_projection WHERE account_id = ''ACC-001'' ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s | $%8.2f -> $%8.2f | %s', [
        DS.FieldByName('event_timestamp').AsString,
        DS.FieldByName('amount').AsFloat,
        DS.FieldByName('balance_after').AsFloat,
        DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 7: Event Replay (Rebuild Projections)
// ============================================================================

{ Clears all projection tables, then replays events to rebuild the account
  projections from scratch, verifying the reconstructed balances. }
procedure DemoEventReplay;
var
  DS: TDataSet;
  State: TAccountState;
  AccId: string;
begin
  WriteLn('=== 7. Event Replay (Rebuild Projections from Scratch) ===');
  WriteLn;

  // Clear projections
  Conn.ExecuteNonQuery('DELETE FROM account_projection');
  Conn.ExecuteNonQuery('DELETE FROM transaction_projection');
  WriteLn('   Projections cleared');

  // Replay all events to rebuild projections
  DS := Conn.ExecuteQuery(
    'SELECT DISTINCT aggregate_id FROM event_store WHERE aggregate_type = ''BankAccount''');
  try
    while not DS.EOF do
    begin
      AccId := DS.FieldByName('aggregate_id').AsString;
      State := RebuildAggregate(AccId);
      UpdateAccountProjection(State, '2024-03-15 00:00:00');
      WriteLn(Format('   Rebuilt projection for %s (balance: $%.2f, status: %s)',
        [AccId, State.Balance, State.Status]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Verify rebuilt projections
  WriteLn;
  WriteLn('   Rebuilt Account Balances:');
  DS := Conn.ExecuteQuery('SELECT account_id, balance, status FROM account_projection ORDER BY account_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: $%.2f (%s)', [
        DS.FieldByName('account_id').AsString,
        DS.FieldByName('balance').AsFloat,
        DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 8: Statistics
// ============================================================================

{ Demonstrates event store statistics and aggregate counts. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Event Store Statistics ===');
  WriteLn;

  // Total events
  WriteLn(Format('   Total events: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM event_store'))]));

  // Events by type
  WriteLn;
  WriteLn('   Events by type:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, COUNT(*) as cnt FROM event_store GROUP BY event_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-20s %d', [
        DS.FieldByName('event_type').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Events by aggregate
  WriteLn;
  WriteLn('   Events by account:');
  DS := Conn.ExecuteQuery(
    'SELECT aggregate_id, COUNT(*) as cnt, MAX(version) as max_ver ' +
    'FROM event_store GROUP BY aggregate_id ORDER BY aggregate_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d events (latest version: %d)', [
        DS.FieldByName('aggregate_id').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('max_ver').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Snapshots
  WriteLn;
  WriteLn(Format('   Snapshots stored: %s',
    [VarToStr(Conn.ExecuteScalar('SELECT COUNT(*) FROM snapshots'))]));

  // Events per second (timeline)
  WriteLn;
  WriteLn('   Event timeline:');
  DS := Conn.ExecuteQuery(
    'SELECT substr(event_timestamp, 1, 7) as month, COUNT(*) as cnt ' +
    'FROM event_store GROUP BY month ORDER BY month');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d events', [
        DS.FieldByName('month').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Main Program
// ============================================================================

begin
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    WriteLn('Example 110: Event Sourcing - Snapshot, Aggregate Rebuild, Temporal Queries');
    WriteLn(StringOfChar('=', 75));
    WriteLn;

    DemoEventStore;
    DemoAggregateRebuild;
    DemoTemporalQueries;
    DemoSnapshots;
    DemoBusinessRules;
    DemoProjections;
    DemoEventReplay;
    DemoStatistics;

    WriteLn('Done.');
    Conn.Close;
  finally
    Conn.Free;
  end;
end.
