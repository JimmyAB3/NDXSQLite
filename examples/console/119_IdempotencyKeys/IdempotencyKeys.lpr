{===============================================================================
  NDXSQLite Example 119 - Idempotency Keys
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Request deduplication with idempotency keys
  - Request hash computation and verification
  - TTL-based key expiration
  - Concurrent request handling
  - Response replay for duplicate requests

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program IdempotencyKeys;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

const
  DEFAULT_TTL_SECONDS = 86400; // 24 hours

type
  TRequestResult = record
    IsNew: Boolean;       // True if this is a new request, False if replay
    IsConflict: Boolean;  // True if same key but different request hash
    ResponseData: string;
    HttpStatus: Integer;
    Status: string;       // pending, completed, expired
  end;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Idempotency key store
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS idempotency_keys (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  idempotency_key TEXT NOT NULL UNIQUE,' +
    '  request_path TEXT NOT NULL,' +
    '  request_method TEXT NOT NULL,' +
    '  request_hash TEXT NOT NULL,' +
    '  response_data TEXT,' +
    '  http_status INTEGER DEFAULT 0,' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +
    '  ttl_seconds INTEGER NOT NULL,' +
    '  created_at TEXT NOT NULL,' +
    '  expires_at TEXT NOT NULL,' +
    '  completed_at TEXT' +
    ')'
  );

  Conn.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_idem_expires ON idempotency_keys(expires_at)'
  );

  Conn.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_idem_status ON idempotency_keys(status)'
  );

  // Request log (for tracking all attempts - no FK for flexibility with cleanup)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS request_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  idempotency_key TEXT NOT NULL,' +
    '  request_path TEXT NOT NULL,' +
    '  request_method TEXT NOT NULL,' +
    '  request_body TEXT,' +
    '  is_replay INTEGER DEFAULT 0,' +
    '  is_conflict INTEGER DEFAULT 0,' +
    '  result_source TEXT,' +
    '  created_at TEXT NOT NULL' +
    ')'
  );

  // Simulated business table (orders processed)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS processed_orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  order_id TEXT NOT NULL,' +
    '  customer_id TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  idempotency_key TEXT NOT NULL,' +
    '  created_at TEXT NOT NULL' +
    ')'
  );
end;

// Simple hash function for request fingerprinting
{ Calculates a fingerprint hash from the HTTP method, path, and request body
  to detect whether two requests with the same idempotency key have identical content. }
function ComputeRequestHash(const Method, Path, Body: string): string;
var
  Hash: Cardinal;
  I: Integer;
  S: string;
begin
  S := Method + '|' + Path + '|' + Body;
  Hash := 0;
  for I := 1 to Length(S) do
    Hash := Hash * 31 + Ord(S[I]);
  Result := IntToHex(Hash, 8);
end;

// Check if an idempotency key exists and is valid
{ Looks up an idempotency key and checks for expiration or hash conflicts;
  returns the stored response if found and valid, or marks it as a new request. }
function CheckIdempotencyKey(const Key, Method, Path, Body: string;
  out Res: TRequestResult): Boolean;
var
  DS: TDataSet;
  StoredHash, CurrentHash: string;
  ExpiresAtStr, NowStr: string;
begin
  Result := False;
  Res.IsNew := True;
  Res.IsConflict := False;
  Res.ResponseData := '';
  Res.HttpStatus := 0;
  Res.Status := '';

  CurrentHash := ComputeRequestHash(Method, Path, Body);

  DS := Conn.ExecuteQuery(
    'SELECT request_hash, response_data, http_status, status, expires_at ' +
    'FROM idempotency_keys WHERE idempotency_key = ?',
    [Key]
  );
  try
    if DS.EOF then
      Exit; // Key doesn't exist, it's new

    Result := True; // Key exists
    Res.IsNew := False;
    StoredHash := DS.FieldByName('request_hash').AsString;
    Res.ResponseData := VarToStr(DS.FieldByName('response_data').Value);
    Res.HttpStatus := DS.FieldByName('http_status').AsInteger;
    Res.Status := DS.FieldByName('status').AsString;

    // Check if expired (string comparison works for ISO format dates)
    ExpiresAtStr := DS.FieldByName('expires_at').AsString;
    NowStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    if NowStr > ExpiresAtStr then
    begin
      // Expired - mark as expired, treat as new
      Conn.ExecuteNonQuery(
        'UPDATE idempotency_keys SET status = ''expired'' WHERE idempotency_key = ?',
        [Key]
      );
      Res.Status := 'expired';
      Res.IsNew := True;
      Result := False;
      Exit;
    end;

    // Check hash conflict (same key, different request)
    if StoredHash <> CurrentHash then
    begin
      Res.IsConflict := True;
      Exit;
    end;
  finally
    DS.Free;
  end;
end;

// Store a new idempotency key (pending state)
{ Inserts a new idempotency key record in pending status with the request hash,
  method, path, and calculated expiration timestamp based on the TTL. }
procedure StoreIdempotencyKey(const Key, Method, Path, Body: string; TTL: Integer);
var
  Now_, ExpiresAt: string;
  Hash: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  ExpiresAt := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now + (TTL / 86400));
  Hash := ComputeRequestHash(Method, Path, Body);

  Conn.ExecuteNonQuery(
    'INSERT INTO idempotency_keys (idempotency_key, request_path, request_method, ' +
    'request_hash, status, ttl_seconds, created_at, expires_at) ' +
    'VALUES (?, ?, ?, ?, ''pending'', ?, ?, ?)',
    [Key, Path, Method, Hash, TTL, Now_, ExpiresAt]
  );
end;

// Complete a pending idempotency key with response
{ Updates an idempotency key to completed status, storing the response data and
  HTTP status code for replay on subsequent duplicate requests. }
procedure CompleteIdempotencyKey(const Key, ResponseData: string; HttpStatus: Integer);
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'UPDATE idempotency_keys SET status = ''completed'', response_data = ?, ' +
    'http_status = ?, completed_at = ? WHERE idempotency_key = ?',
    [ResponseData, HttpStatus, Now_, Key]
  );
end;

// Log a request attempt
{ Inserts an entry into the request_log table recording the idempotency key,
  request details, whether it was a replay or conflict, and the result source. }
procedure LogRequest(const Key, Path, Method, Body, ResultSource: string;
  IsReplay, IsConflict: Boolean);
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'INSERT INTO request_log (idempotency_key, request_path, request_method, ' +
    'request_body, is_replay, is_conflict, result_source, created_at) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
    [Key, Path, Method, Body, Ord(IsReplay), Ord(IsConflict), ResultSource, Now_]
  );
end;

// Simulate processing an order (the business logic)
{ Inserts an order record into processed_orders and returns a JSON response
  containing the order ID, status, and amount. }
function ProcessOrder(const Key, OrderId, CustomerId: string; Amount: Double): string;
var
  Now_: string;
begin
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  Conn.ExecuteNonQuery(
    'INSERT INTO processed_orders (order_id, customer_id, amount, idempotency_key, created_at) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [OrderId, CustomerId, Amount, Key, Now_]
  );
  Result := Format('{"order_id":"%s","status":"created","amount":%.2f}', [OrderId, Amount]);
end;

// High-level idempotent request handler
{ Coordinates idempotency checking, key storage, business logic execution, and
  response caching; returns cached responses for duplicates or rejects conflicts. }
function HandleRequest(const Key, Method, Path, Body: string;
  TTL: Integer; out Response: string; out HttpStatus: Integer): string;
var
  Res: TRequestResult;
  KeyExists: Boolean;
  OrderId, CustomerId: string;
  Amount: Double;
begin
  // Check existing key
  KeyExists := CheckIdempotencyKey(Key, Method, Path, Body, Res);

  if KeyExists then
  begin
    if Res.IsConflict then
    begin
      // Same key, different request body
      Response := '{"error":"Idempotency key conflict - different request body"}';
      HttpStatus := 409;
      LogRequest(Key, Path, Method, Body, 'conflict', False, True);
      Result := 'conflict';
      Exit;
    end;

    if Res.Status = 'pending' then
    begin
      // Request still processing (concurrent duplicate)
      Response := '{"error":"Request is still being processed"}';
      HttpStatus := 409;
      LogRequest(Key, Path, Method, Body, 'pending_duplicate', True, False);
      Result := 'pending_duplicate';
      Exit;
    end;

    // Completed - return stored response
    Response := Res.ResponseData;
    HttpStatus := Res.HttpStatus;
    LogRequest(Key, Path, Method, Body, 'cache', True, False);
    Result := 'replay';
    Exit;
  end;

  // New request - store key and process
  // If expired key exists, remove it first
  if Res.Status = 'expired' then
  begin
    Conn.ExecuteNonQuery(
      'DELETE FROM idempotency_keys WHERE idempotency_key = ?', [Key]
    );
  end;

  StoreIdempotencyKey(Key, Method, Path, Body, TTL);

  // Simulate business logic (parse order from body)
  // Simple parsing for demo
  OrderId := 'ORD-' + Copy(Key, Length(Key) - 3, 4);
  CustomerId := 'CUST-' + Copy(Key, 5, 3);
  Amount := 99.99;

  // Extract amount from body if present
  if Pos('amount', Body) > 0 then
  begin
    // Simple extraction for demo
    Amount := StrToFloatDef(Copy(Body, Pos('amount":', Body) + 8, 6), 99.99);
  end;

  try
    Response := ProcessOrder(Key, OrderId, CustomerId, Amount);
    HttpStatus := 201;
    CompleteIdempotencyKey(Key, Response, HttpStatus);
    LogRequest(Key, Path, Method, Body, 'new', False, False);
    Result := 'new';
  except
    on E: Exception do
    begin
      Response := Format('{"error":"%s"}', [E.Message]);
      HttpStatus := 500;
      CompleteIdempotencyKey(Key, Response, HttpStatus);
      LogRequest(Key, Path, Method, Body, 'error', False, False);
      Result := 'error';
    end;
  end;
end;

{ Sends a new order request with an idempotency key, which gets processed and
  stored for potential replay detection on future duplicate requests. }
procedure DemoFirstRequest;
var
  Response, ResultType: string;
  HttpStatus: Integer;
begin
  WriteLn('=== 1. First Request (New Processing) ===');
  WriteLn;

  ResultType := HandleRequest('idem-key-001', 'POST', '/api/orders',
    '{"order":"A","amount":150.00}', DEFAULT_TTL_SECONDS, Response, HttpStatus);

  WriteLn('   Key: idem-key-001');
  WriteLn(Format('   Result type: %s', [ResultType]));
  WriteLn(Format('   HTTP status: %d', [HttpStatus]));
  WriteLn(Format('   Response: %s', [Response]));
  WriteLn;
end;

{ Sends the same request with the same idempotency key, verifying that the cached
  response is returned without re-executing the business logic. }
procedure DemoDuplicateRequest;
var
  Response, ResultType: string;
  HttpStatus: Integer;
begin
  WriteLn('=== 2. Duplicate Request (Replay Detection) ===');
  WriteLn;

  // Same key, same body - should return cached response
  ResultType := HandleRequest('idem-key-001', 'POST', '/api/orders',
    '{"order":"A","amount":150.00}', DEFAULT_TTL_SECONDS, Response, HttpStatus);

  WriteLn('   Key: idem-key-001 (same as request 1)');
  WriteLn(Format('   Result type: %s (no re-execution)', [ResultType]));
  WriteLn(Format('   HTTP status: %d', [HttpStatus]));
  WriteLn(Format('   Response: %s', [Response]));

  // Verify order was NOT created again
  WriteLn(Format('   Orders with this key: %d (should be 1)',
    [Integer(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM processed_orders WHERE idempotency_key = ''idem-key-001'''))]));
  WriteLn;
end;

{ Sends a request with the same payload but a different idempotency key, showing
  that a new order is created because idempotency is per-key, not per-payload. }
procedure DemoDifferentKey;
var
  Response, ResultType: string;
  HttpStatus: Integer;
begin
  WriteLn('=== 3. Different Key, Same Payload ===');
  WriteLn;

  // Different key - creates new request even with same body
  ResultType := HandleRequest('idem-key-002', 'POST', '/api/orders',
    '{"order":"A","amount":150.00}', DEFAULT_TTL_SECONDS, Response, HttpStatus);

  WriteLn('   Key: idem-key-002 (different key, same body)');
  WriteLn(Format('   Result type: %s (new processing)', [ResultType]));
  WriteLn(Format('   HTTP status: %d', [HttpStatus]));
  WriteLn(Format('   Response: %s', [Response]));
  WriteLn(Format('   Total orders now: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM processed_orders'))]));
  WriteLn;
end;

{ Attempts to reuse an existing idempotency key with a different request body,
  which is rejected with a 409 Conflict to prevent misuse. }
procedure DemoHashConflict;
var
  Response, ResultType: string;
  HttpStatus: Integer;
begin
  WriteLn('=== 4. Key Conflict (Same Key, Different Body) ===');
  WriteLn;

  // Same key but different request body - should be rejected
  ResultType := HandleRequest('idem-key-001', 'POST', '/api/orders',
    '{"order":"B","amount":299.99}', DEFAULT_TTL_SECONDS, Response, HttpStatus);

  WriteLn('   Key: idem-key-001 (same key, DIFFERENT body)');
  WriteLn(Format('   Result type: %s', [ResultType]));
  WriteLn(Format('   HTTP status: %d (Conflict)', [HttpStatus]));
  WriteLn(Format('   Response: %s', [Response]));
  WriteLn;
end;

{ Creates a key with short TTL, simulates expiration by backdating it, then shows
  that the same key can be reused for a new request after expiration. }
procedure DemoTTLExpiration;
var
  Response, ResultType: string;
  HttpStatus: Integer;
  Now_: string;
begin
  WriteLn('=== 5. TTL Expiration ===');
  WriteLn;

  // Create a key with very short TTL (already expired)
  ResultType := HandleRequest('idem-key-ttl', 'POST', '/api/orders',
    '{"order":"C","amount":50.00}', 1, Response, HttpStatus);
  WriteLn(Format('   Created key with 1s TTL: %s (status: %d)', [ResultType, HttpStatus]));

  // Manually expire it (simulate time passing)
  Now_ := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now - (1/24)); // 1 hour ago
  Conn.ExecuteNonQuery(
    'UPDATE idempotency_keys SET expires_at = ? WHERE idempotency_key = ''idem-key-ttl''',
    [Now_]
  );
  WriteLn('   (Simulated TTL expiration by setting expires_at to past)');

  // Try again with same key - should process as new (expired key)
  ResultType := HandleRequest('idem-key-ttl', 'POST', '/api/orders',
    '{"order":"C","amount":50.00}', DEFAULT_TTL_SECONDS, Response, HttpStatus);
  WriteLn(Format('   Retry after expiration: %s (status: %d)', [ResultType, HttpStatus]));
  WriteLn(Format('   Response: %s', [Response]));
  WriteLn(Format('   Orders with this key: %d',
    [Integer(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM processed_orders WHERE idempotency_key = ''idem-key-ttl'''))]));
  WriteLn;
end;

{ Creates a key in pending status to simulate an in-progress request, then shows
  that concurrent duplicate attempts are rejected with 409 Conflict. }
procedure DemoPendingDuplicate;
var
  Response, ResultType: string;
  HttpStatus: Integer;
begin
  WriteLn('=== 6. Concurrent Request Detection ===');
  WriteLn;

  // Create a key in pending state (simulating in-progress request)
  StoreIdempotencyKey('idem-key-pending', 'POST', '/api/orders',
    '{"order":"D","amount":75.00}', DEFAULT_TTL_SECONDS);
  WriteLn('   Created pending key: idem-key-pending (simulating in-progress request)');

  // Try duplicate while still pending
  ResultType := HandleRequest('idem-key-pending', 'POST', '/api/orders',
    '{"order":"D","amount":75.00}', DEFAULT_TTL_SECONDS, Response, HttpStatus);

  WriteLn(Format('   Duplicate attempt: %s', [ResultType]));
  WriteLn(Format('   HTTP status: %d (Conflict - still processing)', [HttpStatus]));
  WriteLn(Format('   Response: %s', [Response]));
  WriteLn;
end;

{ Processes five unique requests then replays all five with the same keys, showing
  that only five orders are created despite ten total request attempts. }
procedure DemoBatchProcessing;
var
  Response, ResultType: string;
  HttpStatus, I: Integer;
  Key: string;
begin
  WriteLn('=== 7. Batch Processing with Idempotency ===');
  WriteLn;

  // Process 5 unique requests
  WriteLn('   Processing 5 unique requests:');
  for I := 1 to 5 do
  begin
    Key := Format('batch-key-%3.3d', [I]);
    ResultType := HandleRequest(Key, 'POST', '/api/orders',
      Format('{"order":"BATCH-%d","amount":%.2f}', [I, I * 100.0]),
      DEFAULT_TTL_SECONDS, Response, HttpStatus);
    WriteLn(Format('     %s -> %s (HTTP %d)', [Key, ResultType, HttpStatus]));
  end;

  // Replay all 5 (simulating retry storm)
  WriteLn('   Replaying all 5 (retry storm):');
  for I := 1 to 5 do
  begin
    Key := Format('batch-key-%3.3d', [I]);
    ResultType := HandleRequest(Key, 'POST', '/api/orders',
      Format('{"order":"BATCH-%d","amount":%.2f}', [I, I * 100.0]),
      DEFAULT_TTL_SECONDS, Response, HttpStatus);
    WriteLn(Format('     %s -> %s (HTTP %d)', [Key, ResultType, HttpStatus]));
  end;

  WriteLn(Format('   Total orders created: %d (should be 5, not 10)',
    [Integer(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM processed_orders WHERE idempotency_key LIKE ''batch-key-%'''))]));
  WriteLn;
end;

{ Queries and prints recent request log entries showing idempotency key, method,
  path, replay/conflict flags, and result source for each attempt. }
procedure DemoRequestLog;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Request Log ===');
  WriteLn;

  WriteLn('   Recent requests:');
  DS := Conn.ExecuteQuery(
    'SELECT idempotency_key, request_method, request_path, is_replay, is_conflict, result_source ' +
    'FROM request_log ORDER BY id LIMIT 15',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-18s %s %-12s replay:%s conflict:%s source:%s',
        [DS.FieldByName('idempotency_key').AsString,
         DS.FieldByName('request_method').AsString,
         DS.FieldByName('request_path').AsString,
         BoolToStr(DS.FieldByName('is_replay').AsInteger = 1, 'Y', 'N'),
         BoolToStr(DS.FieldByName('is_conflict').AsInteger = 1, 'Y', 'N'),
         DS.FieldByName('result_source').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Manually expires some keys by backdating them, then runs cleanup to delete
  expired non-pending keys and reports how many were removed. }
procedure DemoCleanupExpired;
var
  TotalKeys, ExpiredCount: Integer;
begin
  WriteLn('=== 9. Cleanup Expired Keys ===');
  WriteLn;

  TotalKeys := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM idempotency_keys'));
  WriteLn(Format('   Total keys before cleanup: %d', [TotalKeys]));

  // Expire some keys for demo (set expires_at to past)
  Conn.ExecuteNonQuery(
    'UPDATE idempotency_keys SET expires_at = ''2020-01-01 00:00:00'' ' +
    'WHERE idempotency_key IN (''batch-key-001'', ''batch-key-002'')'
  );

  // Cleanup expired
  Conn.ExecuteNonQuery(
    'DELETE FROM idempotency_keys WHERE expires_at < ? AND status != ''pending''',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]
  );

  ExpiredCount := TotalKeys - Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM idempotency_keys'));
  WriteLn(Format('   Expired keys cleaned up: %d', [ExpiredCount]));
  WriteLn(Format('   Remaining keys: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM idempotency_keys'))]));

  WriteLn;
end;

{ Demonstrates idempotency key usage statistics and deduplication rates. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  // Keys by status
  WriteLn('   Idempotency keys by status:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM idempotency_keys GROUP BY status ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-12s %d keys',
        [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Request outcomes
  WriteLn('   Request outcomes:');
  DS := Conn.ExecuteQuery(
    'SELECT result_source, COUNT(*) as cnt FROM request_log GROUP BY result_source ORDER BY cnt DESC',
    []
  );
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-18s %d requests',
        [DS.FieldByName('result_source').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Overall
  WriteLn('   Overall:');
  WriteLn(Format('     Total idempotency keys: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM idempotency_keys'))]));
  WriteLn(Format('     Total request attempts: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM request_log'))]));
  WriteLn(Format('     Total orders processed: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM processed_orders'))]));
  WriteLn(Format('     Replay rate: %.1f%% of requests were replays',
    [Double(Conn.ExecuteScalar(
      'SELECT CAST(SUM(is_replay) AS REAL) / COUNT(*) * 100 FROM request_log'))]));
  WriteLn(Format('     Conflict rate: %.1f%% of requests had conflicts',
    [Double(Conn.ExecuteScalar(
      'SELECT CAST(SUM(is_conflict) AS REAL) / COUNT(*) * 100 FROM request_log'))]));

  WriteLn;
end;

begin
  WriteLn('=== Example 119: Idempotency Keys ===');
  WriteLn('    Exactly-once processing, deduplication, TTL, replay detection');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    DemoFirstRequest;
    DemoDuplicateRequest;
    DemoDifferentKey;
    DemoHashConflict;
    DemoTTLExpiration;
    DemoPendingDuplicate;
    DemoBatchProcessing;
    DemoRequestLog;
    DemoCleanupExpired;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example 119 Complete ===');
end.
