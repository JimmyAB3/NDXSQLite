{===============================================================================
  NDXSQLite Example 86 - Webhook Storage
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates webhook storage and replay patterns:
  - Receiving and storing incoming webhooks
  - Webhook endpoint configuration
  - Delivery tracking with attempts/status
  - Replay mechanism for failed deliveries
  - Dead letter queue for permanent failures
  - Signature verification (simulated)
  - Payload inspection and filtering
  - Retention policy and cleanup

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program WebhookStorage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

{ Computes a simple string hash and returns it as a hexadecimal string. }
function SimpleHash(const S: string): string;
var
  I: Integer;
  Hash: LongWord;
begin
  Hash := 0;
  for I := 1 to Length(S) do
    Hash := Hash * 31 + Ord(S[I]);
  Result := IntToHex(Hash, 8);
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Webhook Schema');
  WriteLn('   ==========================');

  // Webhook endpoints (subscribers)
  Conn.ExecuteNonQuery(
    'CREATE TABLE webhook_endpoints (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  url TEXT NOT NULL,' +
    '  secret TEXT,' +
    '  event_types TEXT NOT NULL,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  max_retries INTEGER DEFAULT 5,' +
    '  timeout_ms INTEGER DEFAULT 5000' +
    ')');

  // Incoming webhook events
  Conn.ExecuteNonQuery(
    'CREATE TABLE webhook_events (' +
    '  id INTEGER PRIMARY KEY,' +
    '  event_type TEXT NOT NULL,' +
    '  source TEXT NOT NULL,' +
    '  payload TEXT NOT NULL,' +
    '  signature TEXT,' +
    '  received_at TEXT DEFAULT (datetime(''now'')),' +
    '  processed INTEGER DEFAULT 0,' +
    '  processed_at TEXT' +
    ')');

  // Delivery attempts
  Conn.ExecuteNonQuery(
    'CREATE TABLE webhook_deliveries (' +
    '  id INTEGER PRIMARY KEY,' +
    '  event_id INTEGER NOT NULL REFERENCES webhook_events(id),' +
    '  endpoint_id INTEGER NOT NULL REFERENCES webhook_endpoints(id),' +
    '  status TEXT DEFAULT ''pending'',' +
    '  attempt_count INTEGER DEFAULT 0,' +
    '  max_attempts INTEGER DEFAULT 5,' +
    '  last_attempt_at TEXT,' +
    '  next_retry_at TEXT,' +
    '  response_code INTEGER,' +
    '  response_body TEXT,' +
    '  error_message TEXT,' +
    '  delivered_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Dead letter queue
  Conn.ExecuteNonQuery(
    'CREATE TABLE webhook_dead_letters (' +
    '  id INTEGER PRIMARY KEY,' +
    '  delivery_id INTEGER NOT NULL REFERENCES webhook_deliveries(id),' +
    '  event_id INTEGER NOT NULL,' +
    '  endpoint_id INTEGER NOT NULL,' +
    '  event_type TEXT NOT NULL,' +
    '  payload TEXT NOT NULL,' +
    '  failure_reason TEXT,' +
    '  total_attempts INTEGER,' +
    '  moved_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Delivery log
  Conn.ExecuteNonQuery(
    'CREATE TABLE webhook_delivery_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  delivery_id INTEGER NOT NULL,' +
    '  attempt_number INTEGER NOT NULL,' +
    '  status TEXT NOT NULL,' +
    '  response_code INTEGER,' +
    '  error_message TEXT,' +
    '  duration_ms INTEGER,' +
    '  attempted_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_events_type ON webhook_events(event_type)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_events_source ON webhook_events(source)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_events_processed ON webhook_events(processed)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_deliveries_status ON webhook_deliveries(status)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_deliveries_event ON webhook_deliveries(event_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_deliveries_endpoint ON webhook_deliveries(endpoint_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_deliveries_retry ON webhook_deliveries(next_retry_at)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_dead_letters_event ON webhook_dead_letters(event_type)');

  WriteLn('   Created tables: webhook_endpoints, webhook_events, webhook_deliveries,');
  WriteLn('                    webhook_dead_letters, webhook_delivery_log');
  WriteLn('');
end;

// =============================================================================
// Setup Endpoints
// =============================================================================
{ Inserts five webhook endpoint configurations with URLs, secrets, subscribed event types, retry limits, and timeout settings. }
procedure SetupEndpoints;
begin
  WriteLn('2. Configuring Webhook Endpoints');
  WriteLn('   ================================');

  Conn.ExecuteNonQuery(
    'INSERT INTO webhook_endpoints (name, url, secret, event_types, max_retries, timeout_ms) VALUES ' +
    '(?, ?, ?, ?, ?, ?)',
    ['Order Service', 'https://orders.example.com/webhooks', 'secret_orders_key',
     'order.created,order.updated,order.cancelled', 5, 3000]);

  Conn.ExecuteNonQuery(
    'INSERT INTO webhook_endpoints (name, url, secret, event_types, max_retries, timeout_ms) VALUES ' +
    '(?, ?, ?, ?, ?, ?)',
    ['Payment Gateway', 'https://payments.example.com/hooks', 'secret_payments_key',
     'payment.received,payment.failed,order.created', 3, 5000]);

  Conn.ExecuteNonQuery(
    'INSERT INTO webhook_endpoints (name, url, secret, event_types, max_retries, timeout_ms) VALUES ' +
    '(?, ?, ?, ?, ?, ?)',
    ['Notification Service', 'https://notify.example.com/webhook', 'secret_notify_key',
     'order.created,order.cancelled,user.registered', 5, 2000]);

  Conn.ExecuteNonQuery(
    'INSERT INTO webhook_endpoints (name, url, secret, event_types, max_retries, timeout_ms) VALUES ' +
    '(?, ?, ?, ?, ?, ?)',
    ['Analytics Service', 'https://analytics.example.com/ingest', 'secret_analytics_key',
     'order.created,order.updated,payment.received,user.registered', 2, 10000]);

  Conn.ExecuteNonQuery(
    'INSERT INTO webhook_endpoints (name, url, secret, event_types, max_retries, is_active) VALUES ' +
    '(?, ?, ?, ?, ?, ?)',
    ['Legacy System', 'https://legacy.example.com/api/hooks', 'old_key',
     'order.created', 1, 0]);

  WriteLn('   Configured 5 endpoints (4 active, 1 inactive):');
  WriteLn('     - Order Service (order.*)');
  WriteLn('     - Payment Gateway (payment.*, order.created)');
  WriteLn('     - Notification Service (order.created/cancelled, user.*)');
  WriteLn('     - Analytics Service (order.*, payment.*, user.*)');
  WriteLn('     - Legacy System (order.created) [INACTIVE]');
  WriteLn('');
end;

// =============================================================================
// Receive Webhooks
// =============================================================================
{ Inserts six incoming webhook events of different types with JSON payloads and computed signatures into the webhook_events table. }
procedure ReceiveWebhooks;
var
  I: Integer;
  EventTypes: array[0..5] of string;
  Sources: array[0..2] of string;
  Payloads: array[0..5] of string;
begin
  WriteLn('3. Receiving Incoming Webhooks');
  WriteLn('   ==============================');

  EventTypes[0] := 'order.created'; EventTypes[1] := 'order.updated';
  EventTypes[2] := 'order.cancelled'; EventTypes[3] := 'payment.received';
  EventTypes[4] := 'payment.failed'; EventTypes[5] := 'user.registered';

  Sources[0] := 'shop-api'; Sources[1] := 'mobile-app'; Sources[2] := 'admin-portal';

  Payloads[0] := '{"order_id":1001,"customer":"John","amount":99.99,"items":3}';
  Payloads[1] := '{"order_id":1001,"status":"shipped","tracking":"TRK123"}';
  Payloads[2] := '{"order_id":1002,"reason":"customer_request"}';
  Payloads[3] := '{"payment_id":"PAY-001","amount":99.99,"method":"card"}';
  Payloads[4] := '{"payment_id":"PAY-002","amount":150.00,"error":"insufficient_funds"}';
  Payloads[5] := '{"user_id":42,"email":"new@example.com","plan":"premium"}';

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 0 to 5 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO webhook_events (event_type, source, payload, signature, received_at) VALUES (?, ?, ?, ?, ?)',
      [EventTypes[I], Sources[I mod 3], Payloads[I],
       SimpleHash(Payloads[I] + 'secret'),
       Format('2024-06-15 10:%.2d:00', [I * 5])]);
  end;
  Conn.ExecuteNonQuery('COMMIT');

  WriteLn(Format('   Received %d webhook events:', [6]));
  WriteLn('     1. order.created    (shop-api)');
  WriteLn('     2. order.updated    (mobile-app)');
  WriteLn('     3. order.cancelled  (admin-portal)');
  WriteLn('     4. payment.received (shop-api)');
  WriteLn('     5. payment.failed   (mobile-app)');
  WriteLn('     6. user.registered  (admin-portal)');
  WriteLn('');
end;

// =============================================================================
// Route Events to Endpoints
// =============================================================================
{ Matches each webhook event to active endpoints by event type subscription and creates pending delivery records. }
procedure RouteEvents;
var
  DS, DSEndpoints: TDataSet;
  EventId: Integer;
  EventType, EndpointEvents: string;
  MatchCount: Integer;
begin
  WriteLn('4. Routing Events to Endpoints');
  WriteLn('   ==============================');

  MatchCount := 0;
  DS := Conn.ExecuteQuery('SELECT id, event_type FROM webhook_events ORDER BY id');
  try
    while not DS.EOF do
    begin
      EventId := DS.FieldByName('id').AsInteger;
      EventType := DS.FieldByName('event_type').AsString;

      // Find matching active endpoints
      DSEndpoints := Conn.ExecuteQuery(
        'SELECT id, name, event_types, max_retries FROM webhook_endpoints WHERE is_active = 1');
      try
        while not DSEndpoints.EOF do
        begin
          EndpointEvents := DSEndpoints.FieldByName('event_types').AsString;
          if Pos(EventType, EndpointEvents) > 0 then
          begin
            Inc(MatchCount);
            Conn.ExecuteNonQuery(
              'INSERT INTO webhook_deliveries (event_id, endpoint_id, status, max_attempts, created_at) VALUES (?, ?, ?, ?, ?)',
              [EventId, DSEndpoints.FieldByName('id').AsInteger, 'pending',
               DSEndpoints.FieldByName('max_retries').AsInteger,
               '2024-06-15 10:30:00']);
          end;
          DSEndpoints.Next;
        end;
      finally
        DSEndpoints.Free;
      end;

      // Mark event as processed
      Conn.ExecuteNonQuery(
        'UPDATE webhook_events SET processed = 1, processed_at = ? WHERE id = ?',
        ['2024-06-15 10:30:00', EventId]);

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Created %d delivery tasks:', [MatchCount]));

  // Show routing summary
  DS := Conn.ExecuteQuery(
    'SELECT e.name AS endpoint, COUNT(*) AS deliveries ' +
    'FROM webhook_deliveries d ' +
    'JOIN webhook_endpoints e ON d.endpoint_id = e.id ' +
    'GROUP BY e.name ORDER BY deliveries DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %s deliveries',
        [DS.FieldByName('endpoint').AsString, DS.FieldByName('deliveries').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Simulate Delivery Attempts
// =============================================================================
{ Processes pending deliveries with simulated HTTP outcomes, logs each attempt, and updates delivery status to delivered or retry_pending. }
procedure SimulateDeliveries;
var
  DS: TDataSet;
  DeliveryId, AttemptNum, ResponseCode, DurationMs: Integer;
  Status, ErrorMsg: string;
begin
  WriteLn('5. Simulating Delivery Attempts');
  WriteLn('   ================================');
  WriteLn('');

  // Process pending deliveries with simulated outcomes
  DS := Conn.ExecuteQuery(
    'SELECT d.id, d.event_id, d.endpoint_id, d.max_attempts, e.name AS endpoint_name ' +
    'FROM webhook_deliveries d ' +
    'JOIN webhook_endpoints e ON d.endpoint_id = e.id ' +
    'WHERE d.status = ''pending'' ORDER BY d.id');
  try
    while not DS.EOF do
    begin
      DeliveryId := DS.FieldByName('id').AsInteger;

      // Simulate different outcomes based on delivery ID
      case DeliveryId mod 5 of
        0: begin Status := 'delivered'; ResponseCode := 200; ErrorMsg := ''; DurationMs := 150; end;
        1: begin Status := 'delivered'; ResponseCode := 200; ErrorMsg := ''; DurationMs := 320; end;
        2: begin Status := 'failed'; ResponseCode := 500; ErrorMsg := 'Internal Server Error'; DurationMs := 2100; end;
        3: begin Status := 'delivered'; ResponseCode := 202; ErrorMsg := ''; DurationMs := 85; end;
        4: begin Status := 'failed'; ResponseCode := 0; ErrorMsg := 'Connection timeout'; DurationMs := 5000; end;
      else
        begin Status := 'delivered'; ResponseCode := 200; ErrorMsg := ''; DurationMs := 200; end;
      end;

      AttemptNum := 1;

      // Log the attempt
      Conn.ExecuteNonQuery(
        'INSERT INTO webhook_delivery_log (delivery_id, attempt_number, status, response_code, error_message, duration_ms, attempted_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        [DeliveryId, AttemptNum, Status, ResponseCode,
         IfThen(ErrorMsg = '', 'OK', ErrorMsg), DurationMs, '2024-06-15 10:31:00']);

      // Update delivery status
      if Status = 'delivered' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE webhook_deliveries SET status = ''delivered'', attempt_count = ?, ' +
          'response_code = ?, last_attempt_at = ?, delivered_at = ? WHERE id = ?',
          [AttemptNum, ResponseCode, '2024-06-15 10:31:00', '2024-06-15 10:31:00', DeliveryId]);
      end
      else
      begin
        Conn.ExecuteNonQuery(
          'UPDATE webhook_deliveries SET status = ''retry_pending'', attempt_count = ?, ' +
          'response_code = ?, error_message = ?, last_attempt_at = ?, ' +
          'next_retry_at = ? WHERE id = ?',
          [AttemptNum, ResponseCode, ErrorMsg, '2024-06-15 10:31:00',
           '2024-06-15 10:32:00', DeliveryId]);
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show results
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) AS cnt FROM webhook_deliveries GROUP BY status ORDER BY status');
  try
    WriteLn('   Delivery results:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %s', [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Retry Failed Deliveries
// =============================================================================
{ Retries deliveries in retry_pending status with additional attempts, moving exhausted ones to the dead letter queue. }
procedure RetryDeliveries;
var
  DS: TDataSet;
  DeliveryId, AttemptCount, MaxAttempts, ResponseCode: Integer;
  Status: string;
  RetryCount, SuccessCount, DeadCount: Integer;
begin
  WriteLn('6. Retrying Failed Deliveries');
  WriteLn('   ============================');
  WriteLn('');

  RetryCount := 0;
  SuccessCount := 0;
  DeadCount := 0;

  // Get deliveries pending retry
  DS := Conn.ExecuteQuery(
    'SELECT d.id, d.attempt_count, d.max_attempts, d.event_id, d.endpoint_id ' +
    'FROM webhook_deliveries d WHERE d.status = ''retry_pending''');
  try
    while not DS.EOF do
    begin
      DeliveryId := DS.FieldByName('id').AsInteger;
      AttemptCount := DS.FieldByName('attempt_count').AsInteger;
      MaxAttempts := DS.FieldByName('max_attempts').AsInteger;

      Inc(RetryCount);

      // Simulate retry attempts (2nd and 3rd attempts)
      // Attempt 2: still fails
      Inc(AttemptCount);
      Conn.ExecuteNonQuery(
        'INSERT INTO webhook_delivery_log (delivery_id, attempt_number, status, response_code, error_message, duration_ms, attempted_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        [DeliveryId, AttemptCount, 'failed', 503, 'Service Unavailable', 3200, '2024-06-15 10:32:00']);

      // Attempt 3: succeeds for some
      Inc(AttemptCount);
      if DeliveryId mod 3 = 0 then
      begin
        Status := 'delivered';
        ResponseCode := 200;
        Inc(SuccessCount);
      end
      else
      begin
        Status := 'failed';
        ResponseCode := 500;
      end;

      Conn.ExecuteNonQuery(
        'INSERT INTO webhook_delivery_log (delivery_id, attempt_number, status, response_code, error_message, duration_ms, attempted_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        [DeliveryId, AttemptCount, Status, ResponseCode,
         IfThen(Status = 'delivered', 'OK', 'Still failing'), 250, '2024-06-15 10:35:00']);

      if Status = 'delivered' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE webhook_deliveries SET status = ''delivered'', attempt_count = ?, ' +
          'response_code = 200, delivered_at = ? WHERE id = ?',
          [AttemptCount, '2024-06-15 10:35:00', DeliveryId]);
      end
      else if AttemptCount >= MaxAttempts then
      begin
        // Move to dead letter queue
        Conn.ExecuteNonQuery(
          'UPDATE webhook_deliveries SET status = ''dead_letter'', attempt_count = ? WHERE id = ?',
          [AttemptCount, DeliveryId]);

        Conn.ExecuteNonQuery(
          'INSERT INTO webhook_dead_letters (delivery_id, event_id, endpoint_id, event_type, payload, failure_reason, total_attempts) ' +
          'SELECT d.id, d.event_id, d.endpoint_id, e.event_type, e.payload, d.error_message, d.attempt_count ' +
          'FROM webhook_deliveries d ' +
          'JOIN webhook_events e ON d.event_id = e.id ' +
          'WHERE d.id = ?', [DeliveryId]);
        Inc(DeadCount);
      end
      else
      begin
        Conn.ExecuteNonQuery(
          'UPDATE webhook_deliveries SET status = ''retry_pending'', attempt_count = ?, ' +
          'next_retry_at = ? WHERE id = ?',
          [AttemptCount, '2024-06-15 10:40:00', DeliveryId]);
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Retried %d deliveries:', [RetryCount]));
  WriteLn(Format('     Recovered (now delivered): %d', [SuccessCount]));
  WriteLn(Format('     Moved to dead letter:      %d', [DeadCount]));
  WriteLn(Format('     Still retrying:            %d', [RetryCount - SuccessCount - DeadCount]));
  WriteLn('');
end;

// =============================================================================
// Replay Webhooks
// =============================================================================
{ Re-queues all order.created events by creating new pending delivery records for each matching active endpoint. }
procedure ReplayWebhooks;
var
  DS: TDataSet;
  ReplayCount: Integer;
begin
  WriteLn('7. Webhook Replay');
  WriteLn('   =================');
  WriteLn('');

  WriteLn('   Replaying all order.created events to all subscribers:');

  // Find all order.created events
  ReplayCount := 0;
  DS := Conn.ExecuteQuery(
    'SELECT id, event_type, payload FROM webhook_events WHERE event_type = ''order.created''');
  try
    while not DS.EOF do
    begin
      // Create new delivery for each active endpoint that subscribes
      Conn.ExecuteNonQuery(
        'INSERT INTO webhook_deliveries (event_id, endpoint_id, status, max_attempts, created_at) ' +
        'SELECT ?, e.id, ''pending'', e.max_retries, datetime(''now'') ' +
        'FROM webhook_endpoints e ' +
        'WHERE e.is_active = 1 AND e.event_types LIKE ''%order.created%''',
        [DS.FieldByName('id').AsInteger]);
      Inc(ReplayCount);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Replayed %d events', [ReplayCount]));

  // Show new pending deliveries
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) AS cnt FROM webhook_deliveries GROUP BY status ORDER BY status');
  try
    WriteLn('');
    WriteLn('   Updated delivery status:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %s', [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Signature Verification
// =============================================================================
{ Recomputes payload signatures for each stored event and compares them against stored signatures to verify integrity. }
procedure DemoSignatureVerification;
var
  DS: TDataSet;
  Payload, StoredSig, ComputedSig, VerifyResult: string;
begin
  WriteLn('8. Signature Verification');
  WriteLn('   =========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT id, event_type, payload, signature FROM webhook_events ORDER BY id');
  try
    WriteLn(Format('   %-4s | %-18s | %-10s | %-10s | %s',
      ['ID', 'Event', 'Stored Sig', 'Computed', 'Valid']));
    WriteLn('   ' + StringOfChar('-', 70));

    while not DS.EOF do
    begin
      Payload := DS.FieldByName('payload').AsString;
      StoredSig := DS.FieldByName('signature').AsString;
      ComputedSig := SimpleHash(Payload + 'secret');

      if StoredSig = ComputedSig then
        VerifyResult := 'VALID'
      else
        VerifyResult := 'INVALID';

      WriteLn(Format('   %-4s | %-18s | %-10s | %-10s | %s',
        [DS.FieldByName('id').AsString,
         DS.FieldByName('event_type').AsString,
         Copy(StoredSig, 1, 8),
         Copy(ComputedSig, 1, 8),
         VerifyResult]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Dead Letter Queue
// =============================================================================
{ Queries and displays all entries in the dead letter queue with their event type, endpoint, attempt count, and failure reason. }
procedure DemoDeadLetterQueue;
var
  DS: TDataSet;
begin
  WriteLn('9. Dead Letter Queue');
  WriteLn('   ====================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT dl.id, dl.event_type, dl.failure_reason, dl.total_attempts, ' +
    'e.name AS endpoint ' +
    'FROM webhook_dead_letters dl ' +
    'JOIN webhook_endpoints e ON dl.endpoint_id = e.id ' +
    'ORDER BY dl.id');
  try
    if DS.EOF then
      WriteLn('   No dead letters (all deliveries succeeded or still retrying)')
    else
    begin
      WriteLn(Format('   %-4s | %-18s | %-22s | %-8s | %s',
        ['ID', 'Event Type', 'Endpoint', 'Attempts', 'Failure Reason']));
      WriteLn('   ' + StringOfChar('-', 85));
      while not DS.EOF do
      begin
        WriteLn(Format('   %-4s | %-18s | %-22s | %-8s | %s',
          [DS.FieldByName('id').AsString,
           DS.FieldByName('event_type').AsString,
           DS.FieldByName('endpoint').AsString,
           DS.FieldByName('total_attempts').AsString,
           DS.FieldByName('failure_reason').AsString]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Delivery Log
// =============================================================================
{ Displays all delivery attempt log entries with delivery ID, attempt number, status, response code, duration, and error message. }
procedure DemoDeliveryLog;
var
  DS: TDataSet;
begin
  WriteLn('10. Delivery Attempt Log');
  WriteLn('    ========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT dl.delivery_id, dl.attempt_number, dl.status, dl.response_code, ' +
    'dl.duration_ms, dl.error_message ' +
    'FROM webhook_delivery_log dl ' +
    'ORDER BY dl.delivery_id, dl.attempt_number');
  try
    WriteLn(Format('    %-5s | %-7s | %-10s | %-6s | %-8s | %s',
      ['Deliv', 'Attempt', 'Status', 'Code', 'Duration', 'Message']));
    WriteLn('    ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('    %-5s | %-7s | %-10s | %-6s | %-8s | %s',
        [DS.FieldByName('delivery_id').AsString,
         DS.FieldByName('attempt_number').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('response_code').AsString,
         DS.FieldByName('duration_ms').AsString + 'ms',
         DS.FieldByName('error_message').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Retention and Cleanup
// =============================================================================
{ Demonstrates retention policies and cleanup of old webhook data. }
procedure DemoRetention;
var
  TotalEvents, TotalDeliveries, TotalLogs: Integer;
  OldEvents: Integer;
begin
  WriteLn('11. Retention Policy');
  WriteLn('    ====================');
  WriteLn('');

  TotalEvents := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM webhook_events'));
  TotalDeliveries := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM webhook_deliveries'));
  TotalLogs := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM webhook_delivery_log'));

  WriteLn('    Before cleanup:');
  WriteLn(Format('      Events:     %d', [TotalEvents]));
  WriteLn(Format('      Deliveries: %d', [TotalDeliveries]));
  WriteLn(Format('      Log entries: %d', [TotalLogs]));

  // Simulate cleanup: remove delivered events older than 30 days
  WriteLn('');
  WriteLn('    Retention rules:');
  WriteLn('      - Delivered events: 30 days');
  WriteLn('      - Failed events: 90 days');
  WriteLn('      - Dead letters: 180 days');
  WriteLn('      - Delivery logs: 7 days');

  // Count what would be removed (simulated - all events are "old" in our test)
  OldEvents := Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM webhook_events e ' +
    'WHERE e.processed = 1 AND e.received_at < ''2024-07-15'''));
  WriteLn('');
  WriteLn(Format('    Events eligible for cleanup (>30 days): %d', [OldEvents]));

  // Actually clean delivered logs
  Conn.ExecuteNonQuery(
    'DELETE FROM webhook_delivery_log WHERE delivery_id IN ' +
    '(SELECT id FROM webhook_deliveries WHERE status = ''delivered'' AND delivered_at < ''2024-06-15'')');

  WriteLn('    (Cleanup would be scheduled, not executed in demo)');
  WriteLn('');
end;

// =============================================================================
// Statistics Summary
// =============================================================================
{ Demonstrates webhook delivery statistics and success rates. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('12. Webhook Statistics');
  WriteLn('    ======================');
  WriteLn('');

  // Event type distribution
  WriteLn('    Events by type:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, COUNT(*) AS cnt FROM webhook_events GROUP BY event_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('      %-20s: %s', [DS.FieldByName('event_type').AsString, DS.FieldByName('cnt').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Delivery success rate
  WriteLn('');
  WriteLn('    Delivery statistics:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) AS cnt, ' +
    'ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM webhook_deliveries), 1) AS pct ' +
    'FROM webhook_deliveries GROUP BY status ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('      %-15s: %s (%s%%)',
        [DS.FieldByName('status').AsString,
         DS.FieldByName('cnt').AsString,
         DS.FieldByName('pct').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Endpoint performance
  WriteLn('');
  WriteLn('    Endpoint delivery summary:');
  DS := Conn.ExecuteQuery(
    'SELECT e.name, ' +
    'COUNT(*) AS total, ' +
    'SUM(CASE WHEN d.status = ''delivered'' THEN 1 ELSE 0 END) AS delivered, ' +
    'SUM(CASE WHEN d.status = ''dead_letter'' THEN 1 ELSE 0 END) AS dead ' +
    'FROM webhook_deliveries d ' +
    'JOIN webhook_endpoints e ON d.endpoint_id = e.id ' +
    'GROUP BY e.name ORDER BY total DESC');
  try
    WriteLn(Format('    %-22s | %-6s | %-10s | %s', ['Endpoint', 'Total', 'Delivered', 'Dead']));
    WriteLn('    ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('    %-22s | %-6s | %-10s | %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('total').AsString,
         DS.FieldByName('delivered').AsString,
         DS.FieldByName('dead').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Average delivery time
  WriteLn('');
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) AS attempts, AVG(duration_ms) AS avg_ms, MAX(duration_ms) AS max_ms ' +
    'FROM webhook_delivery_log');
  try
    WriteLn(Format('    Total attempts: %s', [DS.FieldByName('attempts').AsString]));
    WriteLn(Format('    Avg duration:   %.0f ms', [DS.FieldByName('avg_ms').AsFloat]));
    WriteLn(Format('    Max duration:   %s ms', [DS.FieldByName('max_ms').AsString]));
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 86: Webhook Storage ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    SetupEndpoints;
    ReceiveWebhooks;
    RouteEvents;
    SimulateDeliveries;
    RetryDeliveries;
    ReplayWebhooks;
    DemoSignatureVerification;
    DemoDeadLetterQueue;
    DemoDeliveryLog;
    DemoRetention;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
