{===============================================================================
  NDXSQLite Example 93 - State Machine
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - State machine definition with transitions
  - Event-driven state changes and validation
  - Transition guards and history tracking
  - Order and ticket lifecycle management
  - Invalid transition detection

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program StateMachine;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('=== State Machine with Validated Transitions ===');
  WriteLn;
  WriteLn('1. Creating Schema');

  // State definitions
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS states (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  machine_type TEXT NOT NULL,' +
    '  state_name TEXT NOT NULL,' +
    '  is_initial INTEGER DEFAULT 0,' +
    '  is_final INTEGER DEFAULT 0,' +
    '  description TEXT,' +
    '  UNIQUE(machine_type, state_name)' +
    ')');

  // Transition rules
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS transitions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  machine_type TEXT NOT NULL,' +
    '  from_state TEXT NOT NULL,' +
    '  to_state TEXT NOT NULL,' +
    '  event_name TEXT NOT NULL,' +
    '  guard_condition TEXT,' +
    '  description TEXT,' +
    '  UNIQUE(machine_type, from_state, event_name)' +
    ')');

  // Entity instances (orders, tickets, etc.)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS entities (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  machine_type TEXT NOT NULL,' +
    '  entity_ref TEXT NOT NULL,' +
    '  current_state TEXT NOT NULL,' +
    '  data TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // State history / transition log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS state_history (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  entity_id INTEGER NOT NULL,' +
    '  from_state TEXT,' +
    '  to_state TEXT NOT NULL,' +
    '  event_name TEXT NOT NULL,' +
    '  actor TEXT,' +
    '  metadata TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  FOREIGN KEY (entity_id) REFERENCES entities(id)' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_entities_type ON entities(machine_type)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_entities_state ON entities(current_state)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_history_entity ON state_history(entity_id)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_transitions_lookup ON transitions(machine_type, from_state, event_name)');

  WriteLn('   Tables: states, transitions, entities, state_history');
  WriteLn('   Indexes: 4');
end;

{ Inserts the 8 order states and 9 transition rules defining the order lifecycle (draft through delivered/cancelled/refunded). }
procedure DefineOrderMachine;
begin
  WriteLn;
  WriteLn('2. Defining Order State Machine');

  // States
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, is_initial, description) VALUES (?, ?, 1, ?)', ['order', 'draft', 'Order created but not submitted']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['order', 'pending', 'Awaiting payment']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['order', 'paid', 'Payment received']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['order', 'processing', 'Being prepared']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['order', 'shipped', 'In transit']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, is_final, description) VALUES (?, ?, 1, ?)', ['order', 'delivered', 'Received by customer']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, is_final, description) VALUES (?, ?, 1, ?)', ['order', 'cancelled', 'Order cancelled']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['order', 'refunded', 'Payment returned']);

  // Transitions
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)',
    ['order', 'draft', 'pending', 'submit', 'Customer submits order']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)',
    ['order', 'draft', 'cancelled', 'cancel', 'Customer cancels draft']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, guard_condition, description) VALUES (?, ?, ?, ?, ?, ?)',
    ['order', 'pending', 'paid', 'pay', 'amount > 0', 'Payment processed']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)',
    ['order', 'pending', 'cancelled', 'cancel', 'Customer cancels before payment']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)',
    ['order', 'paid', 'processing', 'process', 'Start fulfillment']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)',
    ['order', 'paid', 'refunded', 'refund', 'Refund before processing']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, guard_condition, description) VALUES (?, ?, ?, ?, ?, ?)',
    ['order', 'processing', 'shipped', 'ship', 'tracking_number IS NOT NULL', 'Package shipped']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)',
    ['order', 'shipped', 'delivered', 'deliver', 'Customer confirms receipt']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)',
    ['order', 'delivered', 'refunded', 'refund', 'Post-delivery refund']);

  WriteLn('   States: draft, pending, paid, processing, shipped, delivered, cancelled, refunded');
  WriteLn('   Transitions: 9 rules defined');

  // Show transition diagram
  WriteLn('   Flow: draft -> pending -> paid -> processing -> shipped -> delivered');
  WriteLn('         draft -> cancelled | pending -> cancelled');
  WriteLn('         paid -> refunded | delivered -> refunded');
end;

{ Inserts the 7 ticket states and 9 transition rules defining the support ticket lifecycle (open through closed with reopen support). }
procedure DefineTicketMachine;
begin
  WriteLn;
  WriteLn('3. Defining Ticket State Machine');

  // States
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, is_initial, description) VALUES (?, ?, 1, ?)', ['ticket', 'open', 'New ticket created']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['ticket', 'assigned', 'Assigned to agent']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['ticket', 'in_progress', 'Being worked on']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['ticket', 'waiting', 'Waiting for customer']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['ticket', 'resolved', 'Solution provided']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, is_final, description) VALUES (?, ?, 1, ?)', ['ticket', 'closed', 'Ticket closed']);
  Conn.ExecuteNonQuery('INSERT INTO states (machine_type, state_name, description) VALUES (?, ?, ?)', ['ticket', 'reopened', 'Reopened by customer']);

  // Transitions
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'open', 'assigned', 'assign', 'Assign to agent']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'assigned', 'in_progress', 'start_work', 'Agent starts working']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'in_progress', 'waiting', 'ask_customer', 'Need customer info']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'waiting', 'in_progress', 'customer_reply', 'Customer responds']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'in_progress', 'resolved', 'resolve', 'Solution found']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'resolved', 'closed', 'close', 'Customer confirms']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'resolved', 'reopened', 'reopen', 'Issue persists']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'reopened', 'in_progress', 'start_work', 'Resume work']);
  Conn.ExecuteNonQuery('INSERT INTO transitions (machine_type, from_state, to_state, event_name, description) VALUES (?, ?, ?, ?, ?)', ['ticket', 'open', 'closed', 'close', 'Close without action']);

  WriteLn('   States: open, assigned, in_progress, waiting, resolved, closed, reopened');
  WriteLn('   Transitions: 9 rules defined');
end;

{ Creates an entity in the state machine and sets it to the initial state. }
function CreateEntity(const MachineType, EntityRef, Data: string): Integer;
var
  InitialState: string;
begin
  InitialState := Conn.ExecuteScalar(
    'SELECT state_name FROM states WHERE machine_type = ? AND is_initial = 1',
    [MachineType]);

  Conn.ExecuteNonQuery(
    'INSERT INTO entities (machine_type, entity_ref, current_state, data) VALUES (?, ?, ?, ?)',
    [MachineType, EntityRef, InitialState, Data]);

  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');

  // Log initial state
  Conn.ExecuteNonQuery(
    'INSERT INTO state_history (entity_id, to_state, event_name, actor) VALUES (?, ?, ?, ?)',
    [IntToStr(Result), InitialState, 'create', 'system']);
end;

{ Triggers a state machine event and transitions the entity if valid. }
function TriggerEvent(EntityId: Integer; const EventName, Actor, Metadata: string): Boolean;
var
  CurrentState, MachineType, ToState, Guard: string;
  DS: TDataSet;
begin
  Result := False;

  // Get current state
  DS := Conn.ExecuteQuery(
    'SELECT current_state, machine_type FROM entities WHERE id = ?',
    [IntToStr(EntityId)]);
  try
    if DS.EOF then Exit;
    CurrentState := DS.FieldByName('current_state').AsString;
    MachineType := DS.FieldByName('machine_type').AsString;
  finally
    DS.Free;
  end;

  // Find valid transition
  DS := Conn.ExecuteQuery(
    'SELECT to_state, guard_condition FROM transitions ' +
    'WHERE machine_type = ? AND from_state = ? AND event_name = ?',
    [MachineType, CurrentState, EventName]);
  try
    if DS.EOF then
    begin
      WriteLn(Format('     REJECTED: No transition from "%s" on event "%s"',
        [CurrentState, EventName]));
      Exit;
    end;
    ToState := DS.FieldByName('to_state').AsString;
    Guard := DS.FieldByName('guard_condition').AsString;
  finally
    DS.Free;
  end;

  // Check guard condition (simplified - just log it)
  if Guard <> '' then
    WriteLn(Format('     Guard: %s (assumed OK)', [Guard]));

  // Perform transition
  Conn.ExecuteNonQuery(
    'UPDATE entities SET current_state = ?, updated_at = datetime(''now'') WHERE id = ?',
    [ToState, IntToStr(EntityId)]);

  // Log transition
  Conn.ExecuteNonQuery(
    'INSERT INTO state_history (entity_id, from_state, to_state, event_name, actor, metadata) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [IntToStr(EntityId), CurrentState, ToState, EventName, Actor, Metadata]);

  WriteLn(Format('     %s -> %s (event: %s, actor: %s)',
    [CurrentState, ToState, EventName, Actor]));
  Result := True;
end;

{ Walks order ORD-001 through the full happy path: draft, submit, pay, process, ship, deliver. }
procedure DemoOrderLifecycle;
var
  OrderId: Integer;
begin
  WriteLn;
  WriteLn('4. Order Lifecycle (Happy Path)');

  OrderId := CreateEntity('order', 'ORD-001', '{"product":"Widget","qty":5}');
  WriteLn(Format('   Created order ORD-001 (id=%d)', [OrderId]));

  TriggerEvent(OrderId, 'submit', 'customer', '');
  TriggerEvent(OrderId, 'pay', 'payment_gateway', '{"amount":49.95}');
  TriggerEvent(OrderId, 'process', 'warehouse', '');
  TriggerEvent(OrderId, 'ship', 'logistics', '{"tracking":"TRK123"}');
  TriggerEvent(OrderId, 'deliver', 'courier', '{"signature":"John"}');

  WriteLn(Format('   Final state: %s',
    [string(Conn.ExecuteScalar('SELECT current_state FROM entities WHERE id = ?', [IntToStr(OrderId)]))]));
end;

{ Attempts illegal transitions (ship from draft, deliver from draft, process without payment) to verify rejection by the state machine. }
procedure DemoInvalidTransitions;
var
  OrderId: Integer;
begin
  WriteLn;
  WriteLn('5. Invalid Transition Attempts');

  OrderId := CreateEntity('order', 'ORD-002', '{"product":"Gadget","qty":1}');
  WriteLn(Format('   Created order ORD-002 (id=%d) in draft state', [OrderId]));

  // Try to ship a draft order (invalid)
  WriteLn('   Attempting to ship a draft order:');
  TriggerEvent(OrderId, 'ship', 'warehouse', '');

  // Try to deliver a draft order (invalid)
  WriteLn('   Attempting to deliver a draft order:');
  TriggerEvent(OrderId, 'deliver', 'courier', '');

  // Try to process without paying (invalid)
  TriggerEvent(OrderId, 'submit', 'customer', '');
  WriteLn('   Attempting to process without payment:');
  TriggerEvent(OrderId, 'process', 'warehouse', '');

  WriteLn(Format('   State remains: %s',
    [string(Conn.ExecuteScalar('SELECT current_state FROM entities WHERE id = ?', [IntToStr(OrderId)]))]));
end;

{ Demonstrates cancellation paths through the state machine. }
procedure DemoCancellation;
var
  OrderId1, OrderId2: Integer;
begin
  WriteLn;
  WriteLn('6. Cancellation Paths');

  // Cancel from draft
  OrderId1 := CreateEntity('order', 'ORD-003', '{}');
  WriteLn('   ORD-003: Cancel from draft');
  TriggerEvent(OrderId1, 'cancel', 'customer', '{"reason":"changed mind"}');

  // Cancel from pending
  OrderId2 := CreateEntity('order', 'ORD-004', '{}');
  WriteLn('   ORD-004: Submit then cancel');
  TriggerEvent(OrderId2, 'submit', 'customer', '');
  TriggerEvent(OrderId2, 'cancel', 'customer', '{"reason":"found cheaper"}');

  // Try to cancel after payment (invalid - no transition)
  WriteLn('   Attempting to cancel a paid order:');
  OrderId1 := CreateEntity('order', 'ORD-005', '{}');
  TriggerEvent(OrderId1, 'submit', 'customer', '');
  TriggerEvent(OrderId1, 'pay', 'gateway', '');
  TriggerEvent(OrderId1, 'cancel', 'customer', '');
  WriteLn(Format('   ORD-005 state: %s (cancel not allowed after payment)',
    [string(Conn.ExecuteScalar('SELECT current_state FROM entities WHERE id = ?', [IntToStr(OrderId1)]))]));
end;

{ Walks ticket TKT-001 through assign, work, ask customer, reply, resolve, reopen, re-resolve, and close. }
procedure DemoTicketLifecycle;
var
  TicketId: Integer;
begin
  WriteLn;
  WriteLn('7. Ticket Lifecycle (with Reopen)');

  TicketId := CreateEntity('ticket', 'TKT-001', '{"issue":"Login broken"}');
  WriteLn(Format('   Created ticket TKT-001 (id=%d)', [TicketId]));

  TriggerEvent(TicketId, 'assign', 'manager', '{"agent":"alice"}');
  TriggerEvent(TicketId, 'start_work', 'alice', '');
  TriggerEvent(TicketId, 'ask_customer', 'alice', '{"question":"Browser version?"}');
  TriggerEvent(TicketId, 'customer_reply', 'customer', '{"answer":"Chrome 120"}');
  TriggerEvent(TicketId, 'resolve', 'alice', '{"fix":"Cleared cache"}');

  // Customer reopens
  WriteLn('   Customer reopens:');
  TriggerEvent(TicketId, 'reopen', 'customer', '{"reason":"Still broken"}');
  TriggerEvent(TicketId, 'start_work', 'alice', '');
  TriggerEvent(TicketId, 'resolve', 'alice', '{"fix":"Reset password"}');
  TriggerEvent(TicketId, 'close', 'customer', '{"satisfied":true}');

  WriteLn(Format('   Final state: %s',
    [string(Conn.ExecuteScalar('SELECT current_state FROM entities WHERE id = ?', [IntToStr(TicketId)]))]));
end;

{ Prints the full sequence of state transitions for order ORD-001, including actor and event at each step. }
procedure ShowTransitionHistory;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('8. Transition History (ORD-001)');

  DS := Conn.ExecuteQuery(
    'SELECT sh.from_state, sh.to_state, sh.event_name, sh.actor, sh.created_at ' +
    'FROM state_history sh ' +
    'JOIN entities e ON e.id = sh.entity_id ' +
    'WHERE e.entity_ref = ''ORD-001'' ' +
    'ORDER BY sh.id');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('from_state').AsString = '' then
        WriteLn(Format('   [%s] -> %s (event: %s)',
          [DS.FieldByName('actor').AsString,
           DS.FieldByName('to_state').AsString,
           DS.FieldByName('event_name').AsString]))
      else
        WriteLn(Format('   [%s] %s -> %s (event: %s)',
          [DS.FieldByName('actor').AsString,
           DS.FieldByName('from_state').AsString,
           DS.FieldByName('to_state').AsString,
           DS.FieldByName('event_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Lists all valid events and their target states for each state in the order machine, identifying terminal states. }
procedure ShowAvailableTransitions;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('9. Available Transitions per State (Order Machine)');

  DS := Conn.ExecuteQuery(
    'SELECT s.state_name, ' +
    '  GROUP_CONCAT(t.event_name || '' -> '' || t.to_state, '', '') as available_events ' +
    'FROM states s ' +
    'LEFT JOIN transitions t ON t.machine_type = s.machine_type AND t.from_state = s.state_name ' +
    'WHERE s.machine_type = ''order'' ' +
    'GROUP BY s.state_name ' +
    'ORDER BY s.is_initial DESC, s.state_name');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('available_events').AsString <> '' then
        WriteLn(Format('   %s: %s',
          [DS.FieldByName('state_name').AsString,
           DS.FieldByName('available_events').AsString]))
      else
        WriteLn(Format('   %s: (terminal state)',
          [DS.FieldByName('state_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Lists all entities with their machine type, reference, current state, and total number of transitions recorded. }
procedure ShowEntityStates;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('10. Current Entity States');

  DS := Conn.ExecuteQuery(
    'SELECT machine_type, entity_ref, current_state, ' +
    '  (SELECT COUNT(*) FROM state_history WHERE entity_id = e.id) as transitions ' +
    'FROM entities e ORDER BY machine_type, id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%s] %s: %s (%d transitions)',
        [DS.FieldByName('machine_type').AsString,
         DS.FieldByName('entity_ref').AsString,
         DS.FieldByName('current_state').AsString,
         DS.FieldByName('transitions').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Prints order counts by current state, average transitions per entity type, and the top 5 most common order transitions. }
procedure ShowStatistics;
var
  DS: TDataSet;
begin
  WriteLn;
  WriteLn('11. State Machine Statistics');

  // Orders by state
  WriteLn('   Orders by current state:');
  DS := Conn.ExecuteQuery(
    'SELECT current_state, COUNT(*) as cnt FROM entities ' +
    'WHERE machine_type = ''order'' GROUP BY current_state ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d',
        [DS.FieldByName('current_state').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Avg transitions per entity
  WriteLn('   Average transitions:');
  DS := Conn.ExecuteQuery(
    'SELECT e.machine_type, ROUND(AVG(cnt), 1) as avg_transitions ' +
    'FROM entities e ' +
    'JOIN (SELECT entity_id, COUNT(*) as cnt FROM state_history GROUP BY entity_id) sh ' +
    '  ON sh.entity_id = e.id ' +
    'GROUP BY e.machine_type');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %.1f transitions/entity',
        [DS.FieldByName('machine_type').AsString,
         DS.FieldByName('avg_transitions').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Most common transitions
  WriteLn('   Most common transitions (order):');
  DS := Conn.ExecuteQuery(
    'SELECT sh.from_state || '' -> '' || sh.to_state as transition, COUNT(*) as cnt ' +
    'FROM state_history sh ' +
    'JOIN entities e ON e.id = sh.entity_id ' +
    'WHERE e.machine_type = ''order'' AND sh.from_state IS NOT NULL AND sh.from_state <> '''' ' +
    'GROUP BY transition ORDER BY cnt DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d times',
        [DS.FieldByName('transition').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

begin
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');
    Conn.ExecuteNonQuery('PRAGMA foreign_keys=ON');

    CreateSchema;
    DefineOrderMachine;
    DefineTicketMachine;
    DemoOrderLifecycle;
    DemoInvalidTransitions;
    DemoCancellation;
    DemoTicketLifecycle;
    ShowTransitionHistory;
    ShowAvailableTransitions;
    ShowEntityStates;
    ShowStatistics;

    WriteLn;
    WriteLn('=== Example Complete ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
