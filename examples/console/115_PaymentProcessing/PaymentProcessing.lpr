{===============================================================================
  NDXSQLite Example 115 - Payment Processing
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Payment lifecycle (authorize, capture, refund, void)
  - Partial capture and refund support
  - Idempotency key handling
  - Payment gateway simulation
  - Transaction history and reconciliation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program PaymentProcessing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Payments table - core payment records
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS payments (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  payment_id TEXT NOT NULL UNIQUE,' +
    '  idempotency_key TEXT,' +
    '  customer_id TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  currency TEXT NOT NULL DEFAULT ''USD'',' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  description TEXT,' +
    '  authorized_amount REAL DEFAULT 0,' +
    '  captured_amount REAL DEFAULT 0,' +
    '  refunded_amount REAL DEFAULT 0,' +
    '  created_at TEXT NOT NULL,' +
    '  updated_at TEXT NOT NULL' +
    ')'
  );

  Conn.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_payments_idempotency ON payments(idempotency_key)'
  );

  Conn.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_payments_customer ON payments(customer_id)'
  );

  Conn.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_payments_status ON payments(status)'
  );

  // Payment events table - state transition log
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS payment_events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  payment_id TEXT NOT NULL,' +
    '  event_type TEXT NOT NULL,' +
    '  status_from TEXT,' +
    '  status_to TEXT,' +
    '  amount REAL,' +
    '  metadata TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  FOREIGN KEY (payment_id) REFERENCES payments(payment_id)' +
    ')'
  );

  // Disputes table
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS disputes (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  dispute_id TEXT NOT NULL UNIQUE,' +
    '  payment_id TEXT NOT NULL,' +
    '  reason TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  status TEXT NOT NULL DEFAULT ''open'',' +
    '  evidence TEXT,' +
    '  opened_at TEXT NOT NULL,' +
    '  resolved_at TEXT,' +
    '  resolution TEXT,' +
    '  FOREIGN KEY (payment_id) REFERENCES payments(payment_id)' +
    ')'
  );

  // Reconciliation batches
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS reconciliation_batches (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  batch_id TEXT NOT NULL UNIQUE,' +
    '  batch_date TEXT NOT NULL,' +
    '  total_payments INTEGER DEFAULT 0,' +
    '  total_amount REAL DEFAULT 0,' +
    '  matched_count INTEGER DEFAULT 0,' +
    '  unmatched_count INTEGER DEFAULT 0,' +
    '  discrepancy_amount REAL DEFAULT 0,' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  created_at TEXT NOT NULL' +
    ')'
  );

  // External transactions (for reconciliation)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS external_transactions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  external_id TEXT NOT NULL,' +
    '  payment_id TEXT,' +
    '  amount REAL NOT NULL,' +
    '  transaction_date TEXT NOT NULL,' +
    '  source TEXT NOT NULL,' +
    '  matched INTEGER DEFAULT 0,' +
    '  batch_id TEXT,' +
    '  FOREIGN KEY (payment_id) REFERENCES payments(payment_id)' +
    ')'
  );
end;

{ Builds a unique payment ID string using the current date and a sequence number. }
function GeneratePaymentId(Seq: Integer): string;
begin
  Result := Format('pay_%s_%d', [FormatDateTime('yyyymmdd', Now), Seq]);
end;

{ Inserts a new payment record in pending status after checking the idempotency key;
  if a payment with the same idempotency key already exists, returns its ID instead
  of creating a duplicate. Logs a 'created' event in the payment_events table. }
function CreatePayment(const PaymentId, CustomerId: string; Amount: Double;
  const Currency, Description, IdempotencyKey: string): Integer;
var
  DS: TDataSet;
  ExistingId: Integer;
  Now_: string;
begin
  // Check idempotency key
  if IdempotencyKey <> '' then
  begin
    DS := Conn.ExecuteQuery(
      'SELECT id FROM payments WHERE idempotency_key = ?',
      [IdempotencyKey]
    );
    try
      if not DS.EOF then
      begin
        ExistingId := DS.FieldByName('id').AsInteger;
        Result := ExistingId;
        Exit;
      end;
    finally
      DS.Free;
    end;
  end;

  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Conn.ExecuteNonQuery(
    'INSERT INTO payments (payment_id, idempotency_key, customer_id, amount, currency, ' +
    'status, description, created_at, updated_at) ' +
    'VALUES (?, ?, ?, ?, ?, ''pending'', ?, ?, ?)',
    [PaymentId, IdempotencyKey, CustomerId, Amount, Currency, Description, Now_, Now_]
  );

  Result := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));

  // Log event
  Conn.ExecuteNonQuery(
    'INSERT INTO payment_events (payment_id, event_type, status_from, status_to, amount, created_at) ' +
    'VALUES (?, ''created'', NULL, ''pending'', ?, ?)',
    [PaymentId, Amount, Now_]
  );
end;

{ Transitions a pending payment to authorized status, sets the authorized amount,
  and logs an 'authorized' event. Returns False if the payment is not in pending status. }
function AuthorizePayment(const PaymentId: string; Amount: Double): Boolean;
var
  DS: TDataSet;
  CurrentStatus: string;
  Now_: string;
begin
  Result := False;
  DS := Conn.ExecuteQuery('SELECT status, amount FROM payments WHERE payment_id = ?', [PaymentId]);
  try
    if DS.EOF then Exit;
    CurrentStatus := DS.FieldByName('status').AsString;
    if CurrentStatus <> 'pending' then Exit;
  finally
    DS.Free;
  end;

  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Conn.ExecuteNonQuery(
    'UPDATE payments SET status = ''authorized'', authorized_amount = ?, updated_at = ? ' +
    'WHERE payment_id = ?',
    [Amount, Now_, PaymentId]
  );

  Conn.ExecuteNonQuery(
    'INSERT INTO payment_events (payment_id, event_type, status_from, status_to, amount, created_at) ' +
    'VALUES (?, ''authorized'', ''pending'', ''authorized'', ?, ?)',
    [PaymentId, Amount, Now_]
  );

  Result := True;
end;

{ Captures a specified amount from an authorized payment, supporting partial captures;
  rejects captures exceeding the authorized amount and transitions the status to
  'captured' or 'partially_captured' accordingly. }
function CapturePayment(const PaymentId: string; Amount: Double): Boolean;
var
  DS: TDataSet;
  CurrentStatus: string;
  AuthAmount, AlreadyCaptured: Double;
  NewStatus: string;
  Now_: string;
begin
  Result := False;
  DS := Conn.ExecuteQuery(
    'SELECT status, authorized_amount, captured_amount FROM payments WHERE payment_id = ?',
    [PaymentId]
  );
  try
    if DS.EOF then Exit;
    CurrentStatus := DS.FieldByName('status').AsString;
    if (CurrentStatus <> 'authorized') and (CurrentStatus <> 'partially_captured') then Exit;
    AuthAmount := DS.FieldByName('authorized_amount').AsFloat;
    AlreadyCaptured := DS.FieldByName('captured_amount').AsFloat;
  finally
    DS.Free;
  end;

  // Cannot capture more than authorized
  if (AlreadyCaptured + Amount) > AuthAmount + 0.001 then Exit;

  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  if Abs((AlreadyCaptured + Amount) - AuthAmount) < 0.01 then
    NewStatus := 'captured'
  else
    NewStatus := 'partially_captured';

  Conn.ExecuteNonQuery(
    'UPDATE payments SET status = ?, captured_amount = captured_amount + ?, updated_at = ? ' +
    'WHERE payment_id = ?',
    [NewStatus, Amount, Now_, PaymentId]
  );

  Conn.ExecuteNonQuery(
    'INSERT INTO payment_events (payment_id, event_type, status_from, status_to, amount, created_at) ' +
    'VALUES (?, ''captured'', ?, ?, ?, ?)',
    [PaymentId, CurrentStatus, NewStatus, Amount, Now_]
  );

  Result := True;
end;

{ Refunds a specified amount from a captured payment, supporting partial refunds;
  rejects refunds exceeding the captured amount and transitions the status to
  'refunded' or 'partially_refunded' accordingly. }
function RefundPayment(const PaymentId: string; Amount: Double): Boolean;
var
  DS: TDataSet;
  CurrentStatus: string;
  CapturedAmount, AlreadyRefunded: Double;
  NewStatus: string;
  Now_: string;
begin
  Result := False;
  DS := Conn.ExecuteQuery(
    'SELECT status, captured_amount, refunded_amount FROM payments WHERE payment_id = ?',
    [PaymentId]
  );
  try
    if DS.EOF then Exit;
    CurrentStatus := DS.FieldByName('status').AsString;
    if (CurrentStatus <> 'captured') and (CurrentStatus <> 'partially_refunded') then Exit;
    CapturedAmount := DS.FieldByName('captured_amount').AsFloat;
    AlreadyRefunded := DS.FieldByName('refunded_amount').AsFloat;
  finally
    DS.Free;
  end;

  // Cannot refund more than captured
  if (AlreadyRefunded + Amount) > CapturedAmount + 0.001 then Exit;

  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  if Abs((AlreadyRefunded + Amount) - CapturedAmount) < 0.01 then
    NewStatus := 'refunded'
  else
    NewStatus := 'partially_refunded';

  Conn.ExecuteNonQuery(
    'UPDATE payments SET status = ?, refunded_amount = refunded_amount + ?, updated_at = ? ' +
    'WHERE payment_id = ?',
    [NewStatus, Amount, Now_, PaymentId]
  );

  Conn.ExecuteNonQuery(
    'INSERT INTO payment_events (payment_id, event_type, status_from, status_to, amount, created_at) ' +
    'VALUES (?, ''refunded'', ?, ?, ?, ?)',
    [PaymentId, CurrentStatus, NewStatus, Amount, Now_]
  );

  Result := True;
end;

{ Voids an authorized payment that has not yet been captured, transitioning it to
  'voided' status and logging the event. Returns False if the payment is not authorized. }
function VoidPayment(const PaymentId: string): Boolean;
var
  DS: TDataSet;
  CurrentStatus: string;
  Now_: string;
begin
  Result := False;
  DS := Conn.ExecuteQuery('SELECT status FROM payments WHERE payment_id = ?', [PaymentId]);
  try
    if DS.EOF then Exit;
    CurrentStatus := DS.FieldByName('status').AsString;
    // Can only void authorized (not yet captured) payments
    if CurrentStatus <> 'authorized' then Exit;
  finally
    DS.Free;
  end;

  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Conn.ExecuteNonQuery(
    'UPDATE payments SET status = ''voided'', updated_at = ? WHERE payment_id = ?',
    [Now_, PaymentId]
  );

  Conn.ExecuteNonQuery(
    'INSERT INTO payment_events (payment_id, event_type, status_from, status_to, amount, created_at) ' +
    'VALUES (?, ''voided'', ''authorized'', ''voided'', 0, ?)',
    [PaymentId, Now_]
  );

  Result := True;
end;

{ Creates a payment and runs it through the full lifecycle: pending to authorized to
  captured, printing the result of each state transition and the final status. }
procedure DemoPaymentLifecycle;
var
  PayId: string;
  Success: Boolean;
  DS: TDataSet;
begin
  WriteLn('=== 1. Payment Lifecycle (Pending -> Authorized -> Captured) ===');
  WriteLn;

  PayId := 'pay_lifecycle_001';
  CreatePayment(PayId, 'cust_100', 250.00, 'USD', 'Order #1001 - Electronics', '');

  WriteLn('   Created payment: ', PayId, ' for $250.00');

  // Authorize
  Success := AuthorizePayment(PayId, 250.00);
  WriteLn(Format('   Authorized: %s (amount: $%.2f)', [BoolToStr(Success, 'YES', 'NO'), 250.0]));

  // Capture full amount
  Success := CapturePayment(PayId, 250.00);
  WriteLn(Format('   Captured: %s (amount: $%.2f)', [BoolToStr(Success, 'YES', 'NO'), 250.0]));

  // Show final status
  DS := Conn.ExecuteQuery('SELECT status, captured_amount FROM payments WHERE payment_id = ?', [PayId]);
  try
    WriteLn(Format('   Final status: %s, captured: $%.2f',
      [DS.FieldByName('status').AsString, DS.FieldByName('captured_amount').AsFloat]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Authorizes a $500 payment and captures it in two partial amounts ($300 then $200),
  showing status transitions between partially_captured and captured, then verifies
  that over-capture is correctly rejected. }
procedure DemoPartialCapture;
var
  PayId: string;
  Success: Boolean;
  DS: TDataSet;
begin
  WriteLn('=== 2. Partial Capture ===');
  WriteLn;

  PayId := 'pay_partial_001';
  CreatePayment(PayId, 'cust_101', 500.00, 'USD', 'Order #1002 - Furniture', '');
  AuthorizePayment(PayId, 500.00);
  WriteLn('   Authorized: $500.00');

  // Capture only $300 (partial shipment)
  Success := CapturePayment(PayId, 300.00);
  DS := Conn.ExecuteQuery('SELECT status, captured_amount FROM payments WHERE payment_id = ?', [PayId]);
  try
    WriteLn(Format('   Partial capture $300: %s -> status: %s',
      [BoolToStr(Success, 'YES', 'NO'), DS.FieldByName('status').AsString]));
  finally
    DS.Free;
  end;

  // Capture remaining $200
  Success := CapturePayment(PayId, 200.00);
  DS := Conn.ExecuteQuery('SELECT status, captured_amount FROM payments WHERE payment_id = ?', [PayId]);
  try
    WriteLn(Format('   Remaining capture $200: %s -> status: %s, total captured: $%.2f',
      [BoolToStr(Success, 'YES', 'NO'), DS.FieldByName('status').AsString,
       DS.FieldByName('captured_amount').AsFloat]));
  finally
    DS.Free;
  end;

  // Try to over-capture (should fail)
  Success := CapturePayment(PayId, 50.00);
  WriteLn(Format('   Over-capture $50: %s (correctly rejected)', [BoolToStr(Success, 'YES', 'NO')]));

  WriteLn;
end;

{ Creates payments with idempotency keys and verifies that duplicate requests with
  the same key return the existing payment, while a different key creates a new one. }
procedure DemoIdempotencyKeys;
var
  Id1, Id2, Id3: Integer;
begin
  WriteLn('=== 3. Idempotency Keys ===');
  WriteLn;

  // First request with idempotency key
  Id1 := CreatePayment('pay_idemp_001', 'cust_102', 75.00, 'USD',
    'Subscription renewal', 'idem_key_abc123');
  WriteLn(Format('   First request (key=idem_key_abc123): payment id=%d', [Id1]));

  // Same idempotency key - should return same payment
  Id2 := CreatePayment('pay_idemp_002', 'cust_102', 75.00, 'USD',
    'Subscription renewal', 'idem_key_abc123');
  WriteLn(Format('   Duplicate request (same key): payment id=%d', [Id2]));
  WriteLn(Format('   Same payment returned: %s', [BoolToStr(Id1 = Id2, 'YES', 'NO')]));

  // Different idempotency key - should create new payment
  Id3 := CreatePayment('pay_idemp_003', 'cust_102', 75.00, 'USD',
    'Subscription renewal', 'idem_key_def456');
  WriteLn(Format('   Different key (idem_key_def456): payment id=%d', [Id3]));
  WriteLn(Format('   New payment created: %s', [BoolToStr(Id3 <> Id1, 'YES', 'NO')]));

  WriteLn;
end;

{ Demonstrates payment refund workflows including partial and full refunds. }
procedure DemoRefunds;
var
  PayId: string;
  Success: Boolean;
  DS: TDataSet;
begin
  WriteLn('=== 4. Refunds (Full and Partial) ===');
  WriteLn;

  // Full refund
  PayId := 'pay_refund_001';
  CreatePayment(PayId, 'cust_103', 150.00, 'USD', 'Order #1003 - Returned item', '');
  AuthorizePayment(PayId, 150.00);
  CapturePayment(PayId, 150.00);
  WriteLn('   Payment captured: $150.00');

  Success := RefundPayment(PayId, 150.00);
  DS := Conn.ExecuteQuery('SELECT status, refunded_amount FROM payments WHERE payment_id = ?', [PayId]);
  try
    WriteLn(Format('   Full refund $150: %s -> status: %s',
      [BoolToStr(Success, 'YES', 'NO'), DS.FieldByName('status').AsString]));
  finally
    DS.Free;
  end;

  // Partial refund
  PayId := 'pay_refund_002';
  CreatePayment(PayId, 'cust_104', 200.00, 'USD', 'Order #1004 - Partial return', '');
  AuthorizePayment(PayId, 200.00);
  CapturePayment(PayId, 200.00);
  WriteLn('   Payment captured: $200.00');

  Success := RefundPayment(PayId, 80.00);
  DS := Conn.ExecuteQuery('SELECT status, refunded_amount FROM payments WHERE payment_id = ?', [PayId]);
  try
    WriteLn(Format('   Partial refund $80: %s -> status: %s, refunded: $%.2f',
      [BoolToStr(Success, 'YES', 'NO'), DS.FieldByName('status').AsString,
       DS.FieldByName('refunded_amount').AsFloat]));
  finally
    DS.Free;
  end;

  // Try refund on non-captured payment (should fail)
  PayId := 'pay_refund_003';
  CreatePayment(PayId, 'cust_105', 100.00, 'USD', 'Order #1005', '');
  AuthorizePayment(PayId, 100.00);
  Success := RefundPayment(PayId, 100.00);
  WriteLn(Format('   Refund on authorized-only: %s (correctly rejected)', [BoolToStr(Success, 'YES', 'NO')]));

  WriteLn;
end;

{ Creates and authorizes a payment, then voids the authorization and verifies that
  subsequent capture attempts on the voided payment are correctly rejected. }
procedure DemoVoidPayment;
var
  PayId: string;
  Success: Boolean;
  DS: TDataSet;
begin
  WriteLn('=== 5. Void Authorization ===');
  WriteLn;

  PayId := 'pay_void_001';
  CreatePayment(PayId, 'cust_106', 350.00, 'USD', 'Order #1006 - Cancelled', '');
  AuthorizePayment(PayId, 350.00);
  WriteLn('   Authorized: $350.00');

  Success := VoidPayment(PayId);
  DS := Conn.ExecuteQuery('SELECT status FROM payments WHERE payment_id = ?', [PayId]);
  try
    WriteLn(Format('   Void authorization: %s -> status: %s',
      [BoolToStr(Success, 'YES', 'NO'), DS.FieldByName('status').AsString]));
  finally
    DS.Free;
  end;

  // Try to capture voided payment (should fail)
  Success := CapturePayment(PayId, 350.00);
  WriteLn(Format('   Capture after void: %s (correctly rejected)', [BoolToStr(Success, 'YES', 'NO')]));

  WriteLn;
end;

{ Simulates a full dispute lifecycle on a captured payment: opens a dispute for
  unauthorized charge, submits shipping evidence, resolves the dispute in favor
  of the merchant, and restores the payment to captured status. }
procedure DemoDisputeHandling;
var
  PayId, DisputeId: string;
  DS: TDataSet;
  Now_: string;
begin
  WriteLn('=== 6. Dispute Handling ===');
  WriteLn;

  // Create a captured payment that gets disputed
  PayId := 'pay_dispute_001';
  CreatePayment(PayId, 'cust_107', 450.00, 'USD', 'Order #1007 - Disputed charge', '');
  AuthorizePayment(PayId, 450.00);
  CapturePayment(PayId, 450.00);
  WriteLn('   Payment captured: $450.00');

  // Open dispute
  DisputeId := 'disp_001';
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Conn.ExecuteNonQuery(
    'INSERT INTO disputes (dispute_id, payment_id, reason, amount, status, opened_at) ' +
    'VALUES (?, ?, ?, ?, ''open'', ?)',
    [DisputeId, PayId, 'unauthorized_charge', 450.00, Now_]
  );

  // Update payment status to disputed
  Conn.ExecuteNonQuery(
    'UPDATE payments SET status = ''disputed'', updated_at = ? WHERE payment_id = ?',
    [Now_, PayId]
  );

  Conn.ExecuteNonQuery(
    'INSERT INTO payment_events (payment_id, event_type, status_from, status_to, amount, metadata, created_at) ' +
    'VALUES (?, ''dispute_opened'', ''captured'', ''disputed'', ?, ?, ?)',
    [PayId, 450.00, 'reason: unauthorized_charge', Now_]
  );

  WriteLn(Format('   Dispute opened: %s (reason: unauthorized_charge, amount: $%.2f)',
    [DisputeId, 450.0]));

  // Submit evidence
  Conn.ExecuteNonQuery(
    'UPDATE disputes SET evidence = ?, status = ''under_review'' WHERE dispute_id = ?',
    ['Shipping confirmation #TRACK123, signed delivery receipt', DisputeId]
  );
  WriteLn('   Evidence submitted: shipping confirmation + delivery receipt');

  // Resolve dispute (won by merchant)
  Conn.ExecuteNonQuery(
    'UPDATE disputes SET status = ''resolved'', resolution = ''won'', resolved_at = ? WHERE dispute_id = ?',
    [Now_, DisputeId]
  );

  Conn.ExecuteNonQuery(
    'UPDATE payments SET status = ''captured'', updated_at = ? WHERE payment_id = ?',
    [Now_, PayId]
  );

  Conn.ExecuteNonQuery(
    'INSERT INTO payment_events (payment_id, event_type, status_from, status_to, amount, metadata, created_at) ' +
    'VALUES (?, ''dispute_resolved'', ''disputed'', ''captured'', ?, ?, ?)',
    [PayId, 450.00, 'resolution: won', Now_]
  );

  DS := Conn.ExecuteQuery(
    'SELECT d.status, d.resolution, p.status as payment_status ' +
    'FROM disputes d JOIN payments p ON d.payment_id = p.payment_id ' +
    'WHERE d.dispute_id = ?',
    [DisputeId]
  );
  try
    WriteLn(Format('   Dispute resolved: %s (resolution: %s, payment restored to: %s)',
      [DS.FieldByName('status').AsString, DS.FieldByName('resolution').AsString,
       DS.FieldByName('payment_status').AsString]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Demonstrates payment batch reconciliation with settlement processing. }
procedure DemoReconciliation;
var
  BatchId: string;
  Now_: string;
  DS: TDataSet;
  I: Integer;
  MatchedCount, UnmatchedCount: Integer;
  TotalAmount, DiscrepancyAmount: Double;
  ExtPayId: string;
  ExtAmount, IntAmount: Double;
begin
  WriteLn('=== 7. Reconciliation ===');
  WriteLn;

  // Create some captured payments for reconciliation
  for I := 1 to 5 do
  begin
    CreatePayment(Format('pay_recon_%3.3d', [I]), Format('cust_2%2.2d', [I]),
      100.00 + (I * 25.0), 'USD', Format('Recon order #%d', [I]), '');
    AuthorizePayment(Format('pay_recon_%3.3d', [I]), 100.00 + (I * 25.0));
    CapturePayment(Format('pay_recon_%3.3d', [I]), 100.00 + (I * 25.0));
  end;
  WriteLn('   Created 5 captured payments for reconciliation');

  // Simulate external transaction records (from payment processor)
  Now_ := FormatDateTime('yyyy-mm-dd', Now);
  BatchId := 'batch_' + FormatDateTime('yyyymmdd', Now);

  // 4 matching + 1 with amount discrepancy + 1 unmatched external
  for I := 1 to 4 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO external_transactions (external_id, payment_id, amount, transaction_date, source) ' +
      'VALUES (?, ?, ?, ?, ?)',
      [Format('ext_%3.3d', [I]), Format('pay_recon_%3.3d', [I]),
       100.00 + (I * 25.0), Now_, 'stripe']
    );
  end;

  // Payment 5 with discrepancy ($0.50 difference)
  Conn.ExecuteNonQuery(
    'INSERT INTO external_transactions (external_id, payment_id, amount, transaction_date, source) ' +
    'VALUES (?, ?, ?, ?, ?)',
    ['ext_005', 'pay_recon_005', 225.50, Now_, 'stripe']
  );

  // Unmatched external transaction (no payment_id - NULL)
  Conn.ExecuteNonQuery(
    'INSERT INTO external_transactions (external_id, amount, transaction_date, source) ' +
    'VALUES (?, ?, ?, ?)',
    ['ext_006', 99.99, Now_, 'stripe']
  );

  WriteLn('   Loaded 6 external transactions (4 match, 1 discrepancy, 1 unmatched)');

  // Create reconciliation batch
  Conn.ExecuteNonQuery(
    'INSERT INTO reconciliation_batches (batch_id, batch_date, status, created_at) ' +
    'VALUES (?, ?, ''processing'', ?)',
    [BatchId, Now_, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]
  );

  // Perform reconciliation
  MatchedCount := 0;
  UnmatchedCount := 0;
  TotalAmount := 0;
  DiscrepancyAmount := 0;

  DS := Conn.ExecuteQuery(
    'SELECT et.external_id, et.payment_id, et.amount as ext_amount, ' +
    'p.captured_amount as int_amount ' +
    'FROM external_transactions et ' +
    'LEFT JOIN payments p ON et.payment_id = p.payment_id ' +
    'WHERE et.batch_id IS NULL',
    []
  );
  try
    while not DS.EOF do
    begin
      ExtPayId := DS.FieldByName('payment_id').AsString;
      ExtAmount := DS.FieldByName('ext_amount').AsFloat;
      TotalAmount := TotalAmount + ExtAmount;

      if (ExtPayId <> '') and (not DS.FieldByName('int_amount').IsNull) then
      begin
        IntAmount := DS.FieldByName('int_amount').AsFloat;
        if Abs(ExtAmount - IntAmount) < 0.01 then
        begin
          Inc(MatchedCount);
          Conn.ExecuteNonQuery(
            'UPDATE external_transactions SET matched = 1, batch_id = ? WHERE external_id = ?',
            [BatchId, DS.FieldByName('external_id').AsString]
          );
        end
        else
        begin
          Inc(UnmatchedCount);
          DiscrepancyAmount := DiscrepancyAmount + Abs(ExtAmount - IntAmount);
          Conn.ExecuteNonQuery(
            'UPDATE external_transactions SET matched = 0, batch_id = ? WHERE external_id = ?',
            [BatchId, DS.FieldByName('external_id').AsString]
          );
        end;
      end
      else
      begin
        Inc(UnmatchedCount);
        DiscrepancyAmount := DiscrepancyAmount + ExtAmount;
        Conn.ExecuteNonQuery(
          'UPDATE external_transactions SET matched = 0, batch_id = ? WHERE external_id = ?',
          [BatchId, DS.FieldByName('external_id').AsString]
        );
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update batch
  Conn.ExecuteNonQuery(
    'UPDATE reconciliation_batches SET total_payments = ?, total_amount = ?, ' +
    'matched_count = ?, unmatched_count = ?, discrepancy_amount = ?, status = ''completed'' ' +
    'WHERE batch_id = ?',
    [MatchedCount + UnmatchedCount, TotalAmount, MatchedCount, UnmatchedCount,
     DiscrepancyAmount, BatchId]
  );

  WriteLn(Format('   Batch: %s', [BatchId]));
  WriteLn(Format('   Total transactions: %d, Amount: $%.2f', [MatchedCount + UnmatchedCount, TotalAmount]));
  WriteLn(Format('   Matched: %d, Unmatched: %d', [MatchedCount, UnmatchedCount]));
  WriteLn(Format('   Discrepancy amount: $%.2f', [DiscrepancyAmount]));
  WriteLn('   Status: completed');

  WriteLn;
end;

{ Queries and prints the full event log for the disputed payment, showing each
  state transition with event type, status change, amount, and metadata. }
procedure DemoPaymentHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Payment Event History ===');
  WriteLn;

  WriteLn('   History for pay_dispute_001:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, status_from, status_to, amount, metadata, created_at ' +
    'FROM payment_events WHERE payment_id = ? ORDER BY id',
    ['pay_dispute_001']
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%s] %s -> %s (amount: $%.2f) %s',
        [DS.FieldByName('event_type').AsString,
         VarToStr(DS.FieldByName('status_from').Value),
         DS.FieldByName('status_to').AsString,
         DS.FieldByName('amount').AsFloat,
         VarToStr(DS.FieldByName('metadata').Value)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Demonstrates payment processing statistics and settlement summary. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Payment Statistics ===');
  WriteLn;

  // By status
  WriteLn('   Payments by status:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt, SUM(amount) as total ' +
    'FROM payments GROUP BY status ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-20s count: %d  total: $%.2f',
        [DS.FieldByName('status').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Summary
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as total_payments, ' +
    'SUM(captured_amount) as total_captured, ' +
    'SUM(refunded_amount) as total_refunded, ' +
    'COUNT(CASE WHEN status = ''disputed'' THEN 1 END) as disputes_open ' +
    'FROM payments',
    []
  );
  try
    WriteLn(Format('   Total payments: %d', [DS.FieldByName('total_payments').AsInteger]));
    WriteLn(Format('   Total captured: $%.2f', [DS.FieldByName('total_captured').AsFloat]));
    WriteLn(Format('   Total refunded: $%.2f', [DS.FieldByName('total_refunded').AsFloat]));
    WriteLn(Format('   Open disputes: %d', [DS.FieldByName('disputes_open').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;

  // Event types
  WriteLn('   Event log summary:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, COUNT(*) as cnt FROM payment_events GROUP BY event_type ORDER BY cnt DESC',
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
end;

begin
  WriteLn('=== Example 115: Payment Processing ===');
  WriteLn('    Payment lifecycle, idempotency, disputes, reconciliation');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    DemoPaymentLifecycle;
    DemoPartialCapture;
    DemoIdempotencyKeys;
    DemoRefunds;
    DemoVoidPayment;
    DemoDisputeHandling;
    DemoReconciliation;
    DemoPaymentHistory;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example 115 Complete ===');
end.
