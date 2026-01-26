{===============================================================================
  NDXSQLite Example 117 - Outbox Pattern
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Transactional outbox for reliable messaging
  - Atomic write of business data and events
  - Event polling and delivery tracking
  - Consumer checkpoint management
  - Failed delivery retry and dead-letter handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program OutboxPattern;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Business table: orders
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  order_id TEXT NOT NULL UNIQUE,' +
    '  customer_id TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''created'',' +
    '  created_at TEXT NOT NULL' +
    ')'
  );

  // Transactional outbox
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS outbox_events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  event_id TEXT NOT NULL UNIQUE,' +
    '  aggregate_type TEXT NOT NULL,' +
    '  aggregate_id TEXT NOT NULL,' +
    '  event_type TEXT NOT NULL,' +
    '  payload TEXT NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  retry_count INTEGER DEFAULT 0,' +
    '  max_retries INTEGER DEFAULT 3,' +
    '  created_at TEXT NOT NULL,' +
    '  processed_at TEXT,' +
    '  delivered_at TEXT,' +
    '  error_message TEXT' +
    ')'
  );

  Conn.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_outbox_status ON outbox_events(status)'
  );

  Conn.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_outbox_created ON outbox_events(created_at)'
  );

  // Delivery log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS event_deliveries (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  event_id TEXT NOT NULL,' +
    '  destination TEXT NOT NULL,' +
    '  status TEXT NOT NULL,' +
    '  attempt INTEGER NOT NULL,' +
    '  response TEXT,' +
    '  attempted_at TEXT NOT NULL,' +
    '  FOREIGN KEY (event_id) REFERENCES outbox_events(event_id)' +
    ')'
  );

  // Consumer checkpoints
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS consumer_checkpoints (' +
    '  consumer_id TEXT PRIMARY KEY,' +
    '  last_event_id INTEGER DEFAULT 0,' +
    '  last_poll_at TEXT,' +
    '  events_processed INTEGER DEFAULT 0' +
    ')'
  );
end;

{ Builds a unique event ID string from the current timestamp and a sequence number. }
function GenerateEventId(Seq: Integer): string;
begin
  Result := Format('evt_%s_%4.4d', [FormatDateTime('yyyymmddhhnnss', Now), Seq]);
end;

// Atomic operation: create order + outbox event in same transaction
{ Creates an order atomically with its outbox event in a single transaction. }
function CreateOrderWithEvent(const OrderId, CustomerId: string;
  Amount: Double; EventSeq: Integer): Boolean;
var
  Now_: string;
  EventId, Payload: string;
begin
  Result := False;
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  EventId := GenerateEventId(EventSeq);
  Payload := Format('{"order_id":"%s","customer_id":"%s","amount":%.2f,"status":"created"}',
    [OrderId, CustomerId, Amount]);

  try
    Conn.ExecuteNonQuery('BEGIN TRANSACTION');

    // Business data
    Conn.ExecuteNonQuery(
      'INSERT INTO orders (order_id, customer_id, amount, status, created_at) ' +
      'VALUES (?, ?, ?, ''created'', ?)',
      [OrderId, CustomerId, Amount, Now_]
    );

    // Outbox event (same transaction)
    Conn.ExecuteNonQuery(
      'INSERT INTO outbox_events (event_id, aggregate_type, aggregate_id, event_type, payload, status, created_at) ' +
      'VALUES (?, ''Order'', ?, ''OrderCreated'', ?, ''pending'', ?)',
      [EventId, OrderId, Payload, Now_]
    );

    Conn.ExecuteNonQuery('COMMIT');
    Result := True;
  except
    on E: Exception do
    begin
      Conn.ExecuteNonQuery('ROLLBACK');
      WriteLn(Format('   ERROR: %s', [E.Message]));
    end;
  end;
end;

// Update order status + emit event atomically
{ Updates an order's status and atomically inserts a corresponding outbox event
  within a single transaction, rolling back both on failure. }
function UpdateOrderWithEvent(const OrderId, NewStatus: string; EventSeq: Integer): Boolean;
var
  Now_: string;
  EventId, Payload, EventType: string;
begin
  Result := False;
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  EventId := GenerateEventId(EventSeq);
  EventType := 'Order' + UpperCase(Copy(NewStatus, 1, 1)) + Copy(NewStatus, 2, Length(NewStatus) - 1);
  Payload := Format('{"order_id":"%s","status":"%s","updated_at":"%s"}',
    [OrderId, NewStatus, Now_]);

  try
    Conn.ExecuteNonQuery('BEGIN TRANSACTION');

    Conn.ExecuteNonQuery(
      'UPDATE orders SET status = ? WHERE order_id = ?',
      [NewStatus, OrderId]
    );

    Conn.ExecuteNonQuery(
      'INSERT INTO outbox_events (event_id, aggregate_type, aggregate_id, event_type, payload, status, created_at) ' +
      'VALUES (?, ''Order'', ?, ?, ?, ''pending'', ?)',
      [EventId, OrderId, EventType, Payload, Now_]
    );

    Conn.ExecuteNonQuery('COMMIT');
    Result := True;
  except
    on E: Exception do
    begin
      Conn.ExecuteNonQuery('ROLLBACK');
      WriteLn(Format('   ERROR: %s', [E.Message]));
    end;
  end;
end;

// Polling consumer: fetch pending events
{ Retrieves up to BatchSize pending or retry-eligible outbox events ordered by ID
  for processing by a consumer. }
function PollOutboxEvents(BatchSize: Integer): TDataSet;
begin
  Result := Conn.ExecuteQuery(
    'SELECT id, event_id, aggregate_type, aggregate_id, event_type, payload, retry_count ' +
    'FROM outbox_events ' +
    'WHERE status IN (''pending'', ''retry'') ' +
    'ORDER BY id ASC LIMIT ?',
    [BatchSize]
  );
end;

// Simulate delivery to a destination
{ Simulates delivering an event to a destination, recording the delivery attempt
  with either success or a simulated timeout failure in the event_deliveries table. }
function DeliverEvent(const EventId, Destination: string; Attempt: Integer;
  SimulateFailure: Boolean): Boolean;
var
  Now_: string;
  Response: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  if SimulateFailure then
  begin
    Response := 'Connection timeout';
    Conn.ExecuteNonQuery(
      'INSERT INTO event_deliveries (event_id, destination, status, attempt, response, attempted_at) ' +
      'VALUES (?, ?, ''failed'', ?, ?, ?)',
      [EventId, Destination, Attempt, Response, Now_]
    );
    Result := False;
  end
  else
  begin
    Response := '200 OK';
    Conn.ExecuteNonQuery(
      'INSERT INTO event_deliveries (event_id, destination, status, attempt, response, attempted_at) ' +
      'VALUES (?, ?, ''delivered'', ?, ?, ?)',
      [EventId, Destination, Attempt, Response, Now_]
    );
    Result := True;
  end;
end;

// Mark event as delivered
{ Sets an outbox event's status to 'delivered' and records the delivery timestamp. }
procedure MarkEventDelivered(const EventId: string);
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'UPDATE outbox_events SET status = ''delivered'', delivered_at = ? WHERE event_id = ?',
    [Now_, EventId]
  );
end;

// Mark event as failed (increment retry or mark permanently failed)
{ Increments an event's retry count and transitions it to 'retry' status, or marks
  it as permanently 'failed' with the error message if max retries are exhausted. }
procedure MarkEventFailed(const EventId, ErrorMsg: string);
var
  DS: TDataSet;
  RetryCount, MaxRetries: Integer;
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  DS := Conn.ExecuteQuery(
    'SELECT retry_count, max_retries FROM outbox_events WHERE event_id = ?',
    [EventId]
  );
  try
    if DS.EOF then Exit;
    RetryCount := DS.FieldByName('retry_count').AsInteger;
    MaxRetries := DS.FieldByName('max_retries').AsInteger;
  finally
    DS.Free;
  end;

  if RetryCount + 1 >= MaxRetries then
  begin
    Conn.ExecuteNonQuery(
      'UPDATE outbox_events SET status = ''failed'', retry_count = retry_count + 1, ' +
      'error_message = ?, processed_at = ? WHERE event_id = ?',
      [ErrorMsg, Now_, EventId]
    );
  end
  else
  begin
    Conn.ExecuteNonQuery(
      'UPDATE outbox_events SET status = ''retry'', retry_count = retry_count + 1, ' +
      'error_message = ? WHERE event_id = ?',
      [ErrorMsg, EventId]
    );
  end;
end;

{ Records or updates a consumer's processing checkpoint, tracking the last processed
  event ID, poll timestamp, and cumulative events processed count. }
procedure UpdateConsumerCheckpoint(const ConsumerId: string; LastEventId: Integer);
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'INSERT INTO consumer_checkpoints (consumer_id, last_event_id, last_poll_at, events_processed) ' +
    'VALUES (?, ?, ?, 1) ' +
    'ON CONFLICT(consumer_id) DO UPDATE SET last_event_id = ?, last_poll_at = ?, events_processed = events_processed + 1',
    [ConsumerId, LastEventId, Now_, LastEventId, Now_]
  );
end;

{ Creates orders and status updates with their corresponding outbox events in atomic
  transactions, then prints all orders and their pending outbox events. }
procedure DemoAtomicWrite;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Atomic Write (Order + Outbox Event) ===');
  WriteLn;

  // Create orders with events atomically
  CreateOrderWithEvent('ORD-001', 'CUST-100', 250.00, 1);
  CreateOrderWithEvent('ORD-002', 'CUST-101', 175.50, 2);
  CreateOrderWithEvent('ORD-003', 'CUST-102', 89.99, 3);
  CreateOrderWithEvent('ORD-004', 'CUST-100', 432.00, 4);
  CreateOrderWithEvent('ORD-005', 'CUST-103', 67.25, 5);

  WriteLn('   Created 5 orders with outbox events (atomic transactions)');

  // Update some orders (generating more events)
  UpdateOrderWithEvent('ORD-001', 'confirmed', 6);
  UpdateOrderWithEvent('ORD-002', 'confirmed', 7);
  UpdateOrderWithEvent('ORD-001', 'shipped', 8);
  UpdateOrderWithEvent('ORD-003', 'cancelled', 9);

  WriteLn('   Updated 4 order statuses (4 more outbox events)');

  // Show orders
  WriteLn;
  WriteLn('   Orders:');
  DS := Conn.ExecuteQuery('SELECT order_id, customer_id, amount, status FROM orders ORDER BY id', []);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s  customer: %s  $%.2f  status: %s',
        [DS.FieldByName('order_id').AsString,
         DS.FieldByName('customer_id').AsString,
         DS.FieldByName('amount').AsFloat,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show outbox
  WriteLn;
  WriteLn('   Outbox events (all pending):');
  DS := Conn.ExecuteQuery(
    'SELECT event_id, event_type, aggregate_id, status FROM outbox_events ORDER BY id', []);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s  %s [%s]  status: %s',
        [DS.FieldByName('event_id').AsString,
         DS.FieldByName('event_type').AsString,
         DS.FieldByName('aggregate_id').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Polls the outbox in batches, delivers each event to the notification service,
  marks them as delivered, and updates the consumer checkpoint after each batch. }
procedure DemoPollingConsumer;
var
  DS: TDataSet;
  EventId: string;
  EventDbId: Integer;
  Delivered: Boolean;
  ProcessedCount: Integer;
begin
  WriteLn('=== 2. Polling Consumer (Batch Processing) ===');
  WriteLn;

  // Poll first batch (5 events)
  WriteLn('   Poll batch 1 (size=5):');
  ProcessedCount := 0;
  DS := PollOutboxEvents(5);
  try
    while not DS.EOF do
    begin
      EventId := DS.FieldByName('event_id').AsString;
      EventDbId := DS.FieldByName('id').AsInteger;

      // Deliver to notification service
      Delivered := DeliverEvent(EventId, 'notification-service', 1, False);
      if Delivered then
      begin
        MarkEventDelivered(EventId);
        UpdateConsumerCheckpoint('consumer-1', EventDbId);
        Inc(ProcessedCount);
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('     Processed: %d events -> all delivered', [ProcessedCount]));

  // Poll second batch
  WriteLn('   Poll batch 2 (size=5):');
  ProcessedCount := 0;
  DS := PollOutboxEvents(5);
  try
    while not DS.EOF do
    begin
      EventId := DS.FieldByName('event_id').AsString;
      EventDbId := DS.FieldByName('id').AsInteger;

      Delivered := DeliverEvent(EventId, 'notification-service', 1, False);
      if Delivered then
      begin
        MarkEventDelivered(EventId);
        UpdateConsumerCheckpoint('consumer-1', EventDbId);
        Inc(ProcessedCount);
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('     Processed: %d events -> all delivered', [ProcessedCount]));

  // Poll again (should be empty)
  DS := PollOutboxEvents(5);
  try
    WriteLn(Format('   Poll batch 3: %d events remaining', [Integer(not DS.EOF)]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Simulates delivery failures with retries for one order that eventually succeeds,
  and another order that exhausts all retries and enters permanently failed state. }
procedure DemoFailureAndRetry;
var
  Now_: string;
  EventId: string;
  DS: TDataSet;
  RetryCount: Integer;
  Delivered: Boolean;
begin
  WriteLn('=== 3. Failure Handling and Retries ===');
  WriteLn;

  // Create a new order that will have delivery failures
  CreateOrderWithEvent('ORD-006', 'CUST-104', 199.99, 10);
  WriteLn('   Created order ORD-006 with outbox event');

  // Get the event ID
  EventId := VarToStr(Conn.ExecuteScalar(
    'SELECT event_id FROM outbox_events WHERE aggregate_id = ''ORD-006'' AND event_type = ''OrderCreated'''));

  // First attempt: fails
  Delivered := DeliverEvent(EventId, 'notification-service', 1, True);
  MarkEventFailed(EventId, 'Connection timeout');
  WriteLn(Format('   Attempt 1: delivered=%s (Connection timeout)', [BoolToStr(Delivered, 'YES', 'NO')]));

  // Show current state
  DS := Conn.ExecuteQuery(
    'SELECT status, retry_count, max_retries FROM outbox_events WHERE event_id = ?',
    [EventId]
  );
  try
    WriteLn(Format('   Event status: %s (retry %d/%d)',
      [DS.FieldByName('status').AsString,
       DS.FieldByName('retry_count').AsInteger,
       DS.FieldByName('max_retries').AsInteger]));
  finally
    DS.Free;
  end;

  // Second attempt: fails again
  Delivered := DeliverEvent(EventId, 'notification-service', 2, True);
  MarkEventFailed(EventId, 'Service unavailable');
  WriteLn(Format('   Attempt 2: delivered=%s (Service unavailable)', [BoolToStr(Delivered, 'YES', 'NO')]));

  // Third attempt: succeeds
  Delivered := DeliverEvent(EventId, 'notification-service', 3, False);
  if Delivered then
    MarkEventDelivered(EventId);
  WriteLn(Format('   Attempt 3: delivered=%s', [BoolToStr(Delivered, 'YES', 'NO')]));

  DS := Conn.ExecuteQuery(
    'SELECT status, retry_count FROM outbox_events WHERE event_id = ?',
    [EventId]
  );
  try
    WriteLn(Format('   Final status: %s (after %d retries)',
      [DS.FieldByName('status').AsString,
       DS.FieldByName('retry_count').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;

  // Create another order that will permanently fail
  CreateOrderWithEvent('ORD-007', 'CUST-105', 55.00, 11);
  EventId := VarToStr(Conn.ExecuteScalar(
    'SELECT event_id FROM outbox_events WHERE aggregate_id = ''ORD-007'' AND event_type = ''OrderCreated'''));

  WriteLn('   Created order ORD-007 (will exhaust retries):');
  DeliverEvent(EventId, 'notification-service', 1, True);
  MarkEventFailed(EventId, 'Timeout');
  DeliverEvent(EventId, 'notification-service', 2, True);
  MarkEventFailed(EventId, 'Timeout');
  DeliverEvent(EventId, 'notification-service', 3, True);
  MarkEventFailed(EventId, 'Timeout');

  DS := Conn.ExecuteQuery(
    'SELECT status, retry_count, max_retries, error_message FROM outbox_events WHERE event_id = ?',
    [EventId]
  );
  try
    WriteLn(Format('   Status: %s (retries: %d/%d, error: %s)',
      [DS.FieldByName('status').AsString,
       DS.FieldByName('retry_count').AsInteger,
       DS.FieldByName('max_retries').AsInteger,
       DS.FieldByName('error_message').AsString]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries and prints the full delivery attempt history for a specific order's
  event, plus a summary count of all delivery attempts grouped by status. }
procedure DemoDeliveryTracking;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Delivery Tracking ===');
  WriteLn;

  WriteLn('   Delivery attempts for ORD-006 event:');
  DS := Conn.ExecuteQuery(
    'SELECT ed.attempt, ed.destination, ed.status, ed.response, ed.attempted_at ' +
    'FROM event_deliveries ed ' +
    'JOIN outbox_events oe ON ed.event_id = oe.event_id ' +
    'WHERE oe.aggregate_id = ''ORD-006'' AND oe.event_type = ''OrderCreated'' ' +
    'ORDER BY ed.attempt',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     Attempt %d: %s -> %s (%s)',
        [DS.FieldByName('attempt').AsInteger,
         DS.FieldByName('destination').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('response').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Summary by status
  WriteLn('   Delivery summary:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM event_deliveries GROUP BY status ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-12s %d attempts',
        [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Creates an order and delivers its event to four different services, showing how
  one destination can fail and retry independently while others succeed immediately. }
procedure DemoMultipleDestinations;
var
  EventId: string;
  DS: TDataSet;
begin
  WriteLn('=== 5. Multiple Destinations ===');
  WriteLn;

  // Create an order and deliver to multiple services
  CreateOrderWithEvent('ORD-008', 'CUST-106', 320.00, 12);
  EventId := VarToStr(Conn.ExecuteScalar(
    'SELECT event_id FROM outbox_events WHERE aggregate_id = ''ORD-008'' AND event_type = ''OrderCreated'''));

  WriteLn(Format('   Delivering event %s to multiple destinations:', [EventId]));

  DeliverEvent(EventId, 'notification-service', 1, False);
  WriteLn('     notification-service: delivered');

  DeliverEvent(EventId, 'analytics-service', 1, False);
  WriteLn('     analytics-service: delivered');

  DeliverEvent(EventId, 'inventory-service', 1, False);
  WriteLn('     inventory-service: delivered');

  DeliverEvent(EventId, 'billing-service', 1, True);
  WriteLn('     billing-service: FAILED (will retry)');

  // Retry billing
  DeliverEvent(EventId, 'billing-service', 2, False);
  WriteLn('     billing-service (retry): delivered');

  MarkEventDelivered(EventId);

  // Show all deliveries for this event
  WriteLn;
  WriteLn('   All delivery records:');
  DS := Conn.ExecuteQuery(
    'SELECT destination, attempt, status, response FROM event_deliveries ' +
    'WHERE event_id = ? ORDER BY destination, attempt',
    [EventId]
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-24s attempt %d: %s (%s)',
        [DS.FieldByName('destination').AsString,
         DS.FieldByName('attempt').AsInteger,
         DS.FieldByName('status').AsString,
         DS.FieldByName('response').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints each consumer's checkpoint state including last processed event ID,
  total events processed, and last poll timestamp. }
procedure DemoConsumerCheckpoints;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Consumer Checkpoints ===');
  WriteLn;

  // Show consumer state
  DS := Conn.ExecuteQuery(
    'SELECT consumer_id, last_event_id, events_processed, last_poll_at ' +
    'FROM consumer_checkpoints',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Consumer: %s', [DS.FieldByName('consumer_id').AsString]));
      WriteLn(Format('     Last event ID: %d', [DS.FieldByName('last_event_id').AsInteger]));
      WriteLn(Format('     Events processed: %d', [DS.FieldByName('events_processed').AsInteger]));
      WriteLn(Format('     Last poll: %s', [DS.FieldByName('last_poll_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Demonstrates cleanup of delivered outbox events. }
procedure DemoCleanup;
var
  DeliveredCount, CleanedCount: Integer;
begin
  WriteLn('=== 7. Outbox Cleanup ===');
  WriteLn;

  // Count delivered events
  DeliveredCount := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM outbox_events WHERE status = ''delivered'''));
  WriteLn(Format('   Delivered events before cleanup: %d', [DeliveredCount]));

  // Simulate cleanup of "old" delivered events (in practice, based on age)
  // Here we clean up events that are delivered and older than now (all of them for demo)
  Conn.ExecuteNonQuery(
    'DELETE FROM event_deliveries WHERE event_id IN ' +
    '(SELECT event_id FROM outbox_events WHERE status = ''delivered'' AND aggregate_id < ''ORD-005'')'
  );
  Conn.ExecuteNonQuery(
    'DELETE FROM outbox_events WHERE status = ''delivered'' AND aggregate_id < ''ORD-005'''
  );

  CleanedCount := DeliveredCount - Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM outbox_events WHERE status = ''delivered'''));
  WriteLn(Format('   Cleaned up: %d old delivered events', [CleanedCount]));
  WriteLn(Format('   Remaining delivered: %d', [DeliveredCount - CleanedCount]));

  WriteLn;
end;

{ Demonstrates outbox pattern statistics and delivery metrics. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Outbox Statistics ===');
  WriteLn;

  // Events by status
  WriteLn('   Events by status:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM outbox_events GROUP BY status ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-12s %d events',
        [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Events by type
  WriteLn('   Events by type:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, COUNT(*) as cnt FROM outbox_events GROUP BY event_type ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-20s %d events',
        [DS.FieldByName('event_type').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Overall stats
  WriteLn('   Overall:');
  WriteLn(Format('     Total orders: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders'))]));
  WriteLn(Format('     Total outbox events: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM outbox_events'))]));
  WriteLn(Format('     Total delivery attempts: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM event_deliveries'))]));
  WriteLn(Format('     Failed events (exhausted retries): %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM outbox_events WHERE status = ''failed'''))]));

  WriteLn;
end;

begin
  WriteLn('=== Example 117: Outbox Pattern ===');
  WriteLn('    Transactional outbox for reliable event publishing');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    DemoAtomicWrite;
    DemoPollingConsumer;
    DemoFailureAndRetry;
    DemoDeliveryTracking;
    DemoMultipleDestinations;
    DemoConsumerCheckpoints;
    DemoCleanup;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example 117 Complete ===');
end.
