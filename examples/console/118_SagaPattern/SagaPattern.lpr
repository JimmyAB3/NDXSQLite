{===============================================================================
  NDXSQLite Example 118 - Saga Pattern
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Distributed transaction saga orchestration
  - Compensating actions for rollback
  - Step-by-step execution and failure handling
  - Saga state persistence and recovery
  - Multi-step business process coordination

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SagaPattern;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Saga instances
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS sagas (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  saga_id TEXT NOT NULL UNIQUE,' +
    '  saga_type TEXT NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  payload TEXT,' +
    '  current_step INTEGER DEFAULT 0,' +
    '  total_steps INTEGER DEFAULT 0,' +
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  error_message TEXT' +
    ')'
  );

  // Saga steps
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS saga_steps (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  saga_id TEXT NOT NULL,' +
    '  step_name TEXT NOT NULL,' +
    '  step_order INTEGER NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  action_data TEXT,' +
    '  compensation_data TEXT,' +
    '  result_data TEXT,' +
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  error_message TEXT,' +
    '  FOREIGN KEY (saga_id) REFERENCES sagas(saga_id),' +
    '  UNIQUE(saga_id, step_order)' +
    ')'
  );

  // Saga execution log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS saga_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  saga_id TEXT NOT NULL,' +
    '  step_name TEXT,' +
    '  action TEXT NOT NULL,' +
    '  status TEXT NOT NULL,' +
    '  message TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  FOREIGN KEY (saga_id) REFERENCES sagas(saga_id)' +
    ')'
  );
end;

{ Inserts an entry into the saga_log table recording a saga action with its step
  name, action type, status, and descriptive message. }
procedure LogSagaAction(const SagaId, StepName, Action, Status, Message: string);
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'INSERT INTO saga_log (saga_id, step_name, action, status, message, created_at) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [SagaId, StepName, Action, Status, Message, Now_]
  );
end;

{ Inserts a new saga instance with the given ID, type, and JSON payload, setting
  its initial status to pending and logging the creation event. }
function CreateSaga(const SagaId, SagaType, Payload: string): Boolean;
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'INSERT INTO sagas (saga_id, saga_type, status, payload, started_at) ' +
    'VALUES (?, ?, ''pending'', ?, ?)',
    [SagaId, SagaType, Payload, Now_]
  );
  LogSagaAction(SagaId, '', 'saga_created', 'success', Format('Saga %s created', [SagaType]));
  Result := True;
end;

{ Adds a step definition to a saga with the step name, execution order, action
  data, and compensation data, then updates the saga's total_steps count. }
procedure AddSagaStep(const SagaId, StepName, ActionData, CompensationData: string; StepOrder: Integer);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO saga_steps (saga_id, step_name, step_order, action_data, compensation_data) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [SagaId, StepName, StepOrder, ActionData, CompensationData]
  );
  Conn.ExecuteNonQuery(
    'UPDATE sagas SET total_steps = (SELECT COUNT(*) FROM saga_steps WHERE saga_id = ?) WHERE saga_id = ?',
    [SagaId, SagaId]
  );
end;

// Simulate step execution (returns True for success, False for failure)
{ Simulates executing a saga step, marking it running then completed or failed
  based on the SimulateFailure flag, and logging the result. }
function ExecuteStep(const SagaId, StepName, ActionData: string; SimulateFailure: Boolean): Boolean;
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Conn.ExecuteNonQuery(
    'UPDATE saga_steps SET status = ''running'', started_at = ? ' +
    'WHERE saga_id = ? AND step_name = ?',
    [Now_, SagaId, StepName]
  );

  if SimulateFailure then
  begin
    Conn.ExecuteNonQuery(
      'UPDATE saga_steps SET status = ''failed'', completed_at = ?, ' +
      'error_message = ''Service unavailable'' WHERE saga_id = ? AND step_name = ?',
      [Now_, SagaId, StepName]
    );
    LogSagaAction(SagaId, StepName, 'execute', 'failed', 'Service unavailable');
    Result := False;
  end
  else
  begin
    Conn.ExecuteNonQuery(
      'UPDATE saga_steps SET status = ''completed'', completed_at = ?, ' +
      'result_data = ? WHERE saga_id = ? AND step_name = ?',
      [Now_, Format('{"confirmation":"%s_OK"}', [StepName]), SagaId, StepName]
    );
    LogSagaAction(SagaId, StepName, 'execute', 'success', Format('Step completed: %s', [ActionData]));
    Result := True;
  end;
end;

// Execute compensation for a step
{ Marks a step as compensated and logs the compensation action with the step's
  compensation data. }
procedure CompensateStep(const SagaId, StepName, CompensationData: string);
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'UPDATE saga_steps SET status = ''compensated'', completed_at = ? ' +
    'WHERE saga_id = ? AND step_name = ?',
    [Now_, SagaId, StepName]
  );
  LogSagaAction(SagaId, StepName, 'compensate', 'success', Format('Compensated: %s', [CompensationData]));
end;

// Forward declaration
{ Executes compensating actions for all completed steps in reverse order when a
  saga step fails, then marks the saga as compensated. }
procedure CompensateSaga(const SagaId: string; FailedStep: Integer); forward;

// Run the saga orchestration
{ Orchestrates a saga by executing steps sequentially; if a step fails at the
  specified FailAtStep position, triggers compensation for all completed steps. }
procedure RunSaga(const SagaId: string; FailAtStep: Integer);
var
  DS: TDataSet;
  StepName, ActionData, CompensationData: string;
  StepOrder: Integer;
  StepSuccess: Boolean;
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Start saga
  Conn.ExecuteNonQuery(
    'UPDATE sagas SET status = ''running'', started_at = ? WHERE saga_id = ?',
    [Now_, SagaId]
  );
  LogSagaAction(SagaId, '', 'saga_started', 'success', 'Executing steps');

  // Execute steps in order
  DS := Conn.ExecuteQuery(
    'SELECT step_name, step_order, action_data, compensation_data ' +
    'FROM saga_steps WHERE saga_id = ? ORDER BY step_order',
    [SagaId]
  );
  try
    while not DS.EOF do
    begin
      StepName := DS.FieldByName('step_name').AsString;
      StepOrder := DS.FieldByName('step_order').AsInteger;
      ActionData := DS.FieldByName('action_data').AsString;
      CompensationData := DS.FieldByName('compensation_data').AsString;

      // Update current step
      Conn.ExecuteNonQuery(
        'UPDATE sagas SET current_step = ? WHERE saga_id = ?',
        [StepOrder, SagaId]
      );

      // Execute step (simulate failure at specified step)
      StepSuccess := ExecuteStep(SagaId, StepName, ActionData, StepOrder = FailAtStep);

      if not StepSuccess then
      begin
        // Step failed - begin compensation
        Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
        Conn.ExecuteNonQuery(
          'UPDATE sagas SET status = ''compensating'', error_message = ? WHERE saga_id = ?',
          [Format('Step %d (%s) failed', [StepOrder, StepName]), SagaId]
        );
        LogSagaAction(SagaId, StepName, 'saga_compensating', 'info',
          Format('Starting compensation from step %d', [StepOrder - 1]));

        // Compensate completed steps in reverse order
        CompensateSaga(SagaId, StepOrder);
        Exit;
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // All steps completed successfully
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'UPDATE sagas SET status = ''completed'', completed_at = ? WHERE saga_id = ?',
    [Now_, SagaId]
  );
  LogSagaAction(SagaId, '', 'saga_completed', 'success', 'All steps completed successfully');
end;

// Compensate completed steps in reverse order
{ Iterates through all completed steps before the failed step in reverse order,
  executing their compensating actions, then marks the saga as compensated. }
procedure CompensateSaga(const SagaId: string; FailedStep: Integer);
var
  DS: TDataSet;
  StepName, CompensationData: string;
  Now_: string;
begin
  DS := Conn.ExecuteQuery(
    'SELECT step_name, compensation_data FROM saga_steps ' +
    'WHERE saga_id = ? AND step_order < ? AND status = ''completed'' ' +
    'ORDER BY step_order DESC',
    [SagaId, FailedStep]
  );
  try
    while not DS.EOF do
    begin
      StepName := DS.FieldByName('step_name').AsString;
      CompensationData := DS.FieldByName('compensation_data').AsString;
      CompensateStep(SagaId, StepName, CompensationData);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Mark saga as compensated
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'UPDATE sagas SET status = ''compensated'', completed_at = ? WHERE saga_id = ?',
    [Now_, SagaId]
  );
  LogSagaAction(SagaId, '', 'saga_compensated', 'success', 'All completed steps compensated');
end;

{ Creates and executes a 4-step BookTrip saga that completes successfully,
  printing each step's result and confirmation data. }
procedure DemoSuccessfulSaga;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Successful Saga (Book Trip) ===');
  WriteLn;

  CreateSaga('saga-trip-001', 'BookTrip',
    '{"customer":"John","destination":"Paris","dates":"2024-06-01 to 2024-06-07"}');

  AddSagaStep('saga-trip-001', 'ReserveFlight', 'flight:NYC-PAR,date:2024-06-01', 'CancelFlightReservation', 1);
  AddSagaStep('saga-trip-001', 'ReserveHotel', 'hotel:Paris-Marriott,nights:6', 'CancelHotelReservation', 2);
  AddSagaStep('saga-trip-001', 'ReserveCarRental', 'car:sedan,days:6', 'CancelCarRental', 3);
  AddSagaStep('saga-trip-001', 'ChargePayment', 'amount:2450.00,card:****1234', 'RefundPayment', 4);

  WriteLn('   Saga: BookTrip (4 steps)');
  WriteLn('   Steps: ReserveFlight -> ReserveHotel -> ReserveCarRental -> ChargePayment');
  WriteLn;

  // Execute all steps successfully (FailAtStep=0 means no failure)
  RunSaga('saga-trip-001', 0);

  // Show result
  DS := Conn.ExecuteQuery('SELECT status, current_step, total_steps FROM sagas WHERE saga_id = ?',
    ['saga-trip-001']);
  try
    WriteLn(Format('   Result: status=%s, steps completed: %d/%d',
      [DS.FieldByName('status').AsString,
       DS.FieldByName('current_step').AsInteger,
       DS.FieldByName('total_steps').AsInteger]));
  finally
    DS.Free;
  end;

  // Show step statuses
  WriteLn('   Step results:');
  DS := Conn.ExecuteQuery(
    'SELECT step_name, status, result_data FROM saga_steps ' +
    'WHERE saga_id = ? ORDER BY step_order',
    ['saga-trip-001']
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %-20s %s  %s',
        [DS.RecNo,
         DS.FieldByName('step_name').AsString,
         DS.FieldByName('status').AsString,
         VarToStr(DS.FieldByName('result_data').Value)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Creates a BookTrip saga that fails at step 3 (ReserveCarRental), triggering
  compensating actions for the previously completed flight and hotel reservations. }
procedure DemoFailedSagaWithCompensation;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Failed Saga with Compensation ===');
  WriteLn;

  CreateSaga('saga-trip-002', 'BookTrip',
    '{"customer":"Alice","destination":"Tokyo","dates":"2024-07-15 to 2024-07-22"}');

  AddSagaStep('saga-trip-002', 'ReserveFlight', 'flight:LAX-TYO,date:2024-07-15', 'CancelFlightReservation', 1);
  AddSagaStep('saga-trip-002', 'ReserveHotel', 'hotel:Tokyo-Hilton,nights:7', 'CancelHotelReservation', 2);
  AddSagaStep('saga-trip-002', 'ReserveCarRental', 'car:compact,days:7', 'CancelCarRental', 3);
  AddSagaStep('saga-trip-002', 'ChargePayment', 'amount:3200.00,card:****5678', 'RefundPayment', 4);

  WriteLn('   Saga: BookTrip (4 steps, will fail at step 3: ReserveCarRental)');
  WriteLn;

  // Fail at step 3 (ReserveCarRental)
  RunSaga('saga-trip-002', 3);

  // Show result
  DS := Conn.ExecuteQuery('SELECT status, error_message FROM sagas WHERE saga_id = ?',
    ['saga-trip-002']);
  try
    WriteLn(Format('   Result: status=%s', [DS.FieldByName('status').AsString]));
    WriteLn(Format('   Error: %s', [DS.FieldByName('error_message').AsString]));
  finally
    DS.Free;
  end;

  // Show step statuses
  WriteLn('   Step results:');
  DS := Conn.ExecuteQuery(
    'SELECT step_order, step_name, status, error_message FROM saga_steps ' +
    'WHERE saga_id = ? ORDER BY step_order',
    ['saga-trip-002']
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %-20s %s  %s',
        [DS.FieldByName('step_order').AsInteger,
         DS.FieldByName('step_name').AsString,
         DS.FieldByName('status').AsString,
         VarToStr(DS.FieldByName('error_message').Value)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Creates a BookTrip saga that fails immediately at step 1, showing that no
  compensation is needed when no steps have completed successfully. }
procedure DemoEarlyFailure;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Early Failure (Step 1 Fails) ===');
  WriteLn;

  CreateSaga('saga-trip-003', 'BookTrip',
    '{"customer":"Bob","destination":"London","dates":"2024-08-01 to 2024-08-05"}');

  AddSagaStep('saga-trip-003', 'ReserveFlight', 'flight:SFO-LHR,date:2024-08-01', 'CancelFlightReservation', 1);
  AddSagaStep('saga-trip-003', 'ReserveHotel', 'hotel:London-Ritz,nights:4', 'CancelHotelReservation', 2);
  AddSagaStep('saga-trip-003', 'ChargePayment', 'amount:1800.00,card:****9012', 'RefundPayment', 3);

  WriteLn('   Saga: BookTrip (3 steps, will fail at step 1: ReserveFlight)');

  // Fail at step 1
  RunSaga('saga-trip-003', 1);

  DS := Conn.ExecuteQuery('SELECT status, error_message FROM sagas WHERE saga_id = ?',
    ['saga-trip-003']);
  try
    WriteLn(Format('   Result: status=%s', [DS.FieldByName('status').AsString]));
    WriteLn(Format('   Error: %s', [DS.FieldByName('error_message').AsString]));
  finally
    DS.Free;
  end;

  WriteLn('   No compensation needed (no steps completed before failure)');

  // Show step statuses
  DS := Conn.ExecuteQuery(
    'SELECT step_order, step_name, status FROM saga_steps ' +
    'WHERE saga_id = ? ORDER BY step_order',
    ['saga-trip-003']
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %-20s %s',
        [DS.FieldByName('step_order').AsInteger,
         DS.FieldByName('step_name').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Runs two OrderFulfillment sagas: one that completes all 5 steps successfully,
  and another that fails at payment and compensates the inventory reservations. }
procedure DemoOrderFulfillmentSaga;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Order Fulfillment Saga ===');
  WriteLn;

  CreateSaga('saga-order-001', 'OrderFulfillment',
    '{"order_id":"ORD-5001","items":3,"total":599.99}');

  AddSagaStep('saga-order-001', 'ValidateInventory', 'check:3 items available', 'ReleaseInventoryHold', 1);
  AddSagaStep('saga-order-001', 'ReserveInventory', 'reserve:3 items', 'ReleaseInventoryReservation', 2);
  AddSagaStep('saga-order-001', 'ProcessPayment', 'charge:599.99', 'RefundPayment', 3);
  AddSagaStep('saga-order-001', 'CreateShipment', 'ship:3 items to address', 'CancelShipment', 4);
  AddSagaStep('saga-order-001', 'SendConfirmation', 'email:order confirmation', 'SendCancellationEmail', 5);

  WriteLn('   Saga: OrderFulfillment (5 steps)');
  WriteLn('   Steps: ValidateInventory -> ReserveInventory -> ProcessPayment -> CreateShipment -> SendConfirmation');

  // Execute successfully
  RunSaga('saga-order-001', 0);

  DS := Conn.ExecuteQuery('SELECT status, current_step, total_steps FROM sagas WHERE saga_id = ?',
    ['saga-order-001']);
  try
    WriteLn(Format('   Result: status=%s (%d/%d steps)',
      [DS.FieldByName('status').AsString,
       DS.FieldByName('current_step').AsInteger,
       DS.FieldByName('total_steps').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;

  // Now one that fails at payment
  CreateSaga('saga-order-002', 'OrderFulfillment',
    '{"order_id":"ORD-5002","items":1,"total":1299.99}');

  AddSagaStep('saga-order-002', 'ValidateInventory', 'check:1 item available', 'ReleaseInventoryHold', 1);
  AddSagaStep('saga-order-002', 'ReserveInventory', 'reserve:1 item', 'ReleaseInventoryReservation', 2);
  AddSagaStep('saga-order-002', 'ProcessPayment', 'charge:1299.99 (insufficient funds)', 'RefundPayment', 3);
  AddSagaStep('saga-order-002', 'CreateShipment', 'ship:1 item', 'CancelShipment', 4);
  AddSagaStep('saga-order-002', 'SendConfirmation', 'email:confirmation', 'SendCancellationEmail', 5);

  WriteLn('   Saga: OrderFulfillment (fails at step 3: ProcessPayment)');

  RunSaga('saga-order-002', 3);

  DS := Conn.ExecuteQuery('SELECT status, error_message FROM sagas WHERE saga_id = ?',
    ['saga-order-002']);
  try
    WriteLn(Format('   Result: status=%s', [DS.FieldByName('status').AsString]));
    WriteLn(Format('   Error: %s', [DS.FieldByName('error_message').AsString]));
  finally
    DS.Free;
  end;

  WriteLn('   Compensation: ReleaseInventoryReservation, ReleaseInventoryHold');

  WriteLn;
end;

{ Queries and prints the complete execution log for the failed trip saga,
  showing each action with its step name, status, and message in chronological order. }
procedure DemoSagaLog;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Saga Execution Log ===');
  WriteLn;

  // Show log for the failed saga (saga-trip-002)
  WriteLn('   Execution log for saga-trip-002 (failed + compensated):');
  DS := Conn.ExecuteQuery(
    'SELECT step_name, action, status, message FROM saga_log ' +
    'WHERE saga_id = ''saga-trip-002'' ORDER BY id',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%-12s] %-22s %s: %s',
        [DS.FieldByName('status').AsString,
         VarToStr(DS.FieldByName('step_name').Value),
         DS.FieldByName('action').AsString,
         DS.FieldByName('message').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints an overview table of all sagas showing ID, type, status, step progress,
  and any error messages. }
procedure DemoSagaStateTracking;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Saga State Overview ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT saga_id, saga_type, status, current_step, total_steps, error_message ' +
    'FROM sagas ORDER BY id',
    []
  );
  try
    WriteLn(Format('   %-16s %-18s %-14s %s',
      ['Saga ID', 'Type', 'Status', 'Progress']));
    WriteLn('   ' + StringOfChar('-', 70));

    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-18s %-14s %d/%d  %s',
        [DS.FieldByName('saga_id').AsString,
         DS.FieldByName('saga_type').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('current_step').AsInteger,
         DS.FieldByName('total_steps').AsInteger,
         VarToStr(DS.FieldByName('error_message').Value)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Demonstrates saga execution statistics and compensation metrics. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Saga Statistics ===');
  WriteLn;

  // By status
  WriteLn('   Sagas by status:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM sagas GROUP BY status ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d sagas',
        [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Steps by status
  WriteLn('   Steps by status:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM saga_steps GROUP BY status ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-14s %d steps',
        [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Overall
  WriteLn('   Overall:');
  WriteLn(Format('     Total sagas: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM sagas'))]));
  WriteLn(Format('     Total steps defined: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM saga_steps'))]));
  WriteLn(Format('     Total log entries: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM saga_log'))]));
  WriteLn(Format('     Compensation rate: %d/%d sagas required compensation',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM sagas WHERE status = ''compensated''')),
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM sagas'))]));

  WriteLn;
end;

begin
  WriteLn('=== Example 118: Saga Pattern ===');
  WriteLn('    Long-running transactions with compensating actions');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    DemoSuccessfulSaga;
    DemoFailedSagaWithCompensation;
    DemoEarlyFailure;
    DemoOrderFulfillmentSaga;
    DemoSagaLog;
    DemoSagaStateTracking;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example 118 Complete ===');
end.
