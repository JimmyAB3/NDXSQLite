{===============================================================================
  NDXSQLite Example 88 - API Rate Limiting
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates rate limiting patterns by user/IP:
  - Fixed window rate limiting
  - Sliding window counter
  - Token bucket algorithm
  - Per-user and per-IP limits
  - API key tiered limits (free/pro/enterprise)
  - Endpoint-specific limits
  - Burst handling
  - Rate limit response headers
  - Abuse detection and blocking
  - Quota management and monitoring

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program APIRateLimiting;

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

{ Returns the smaller of two integer values. }
function MinInt(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Rate Limiting Schema');
  WriteLn('   ================================');

  // API keys with tier-based limits
  Conn.ExecuteNonQuery(
    'CREATE TABLE api_keys (' +
    '  id INTEGER PRIMARY KEY,' +
    '  key_value TEXT NOT NULL UNIQUE,' +
    '  owner TEXT NOT NULL,' +
    '  tier TEXT NOT NULL DEFAULT ''free'',' +
    '  requests_per_minute INTEGER DEFAULT 60,' +
    '  requests_per_hour INTEGER DEFAULT 1000,' +
    '  requests_per_day INTEGER DEFAULT 10000,' +
    '  burst_limit INTEGER DEFAULT 10,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Rate limit rules per endpoint
  Conn.ExecuteNonQuery(
    'CREATE TABLE rate_limit_rules (' +
    '  id INTEGER PRIMARY KEY,' +
    '  endpoint TEXT NOT NULL,' +
    '  method TEXT DEFAULT ''*'',' +
    '  window_seconds INTEGER NOT NULL,' +
    '  max_requests INTEGER NOT NULL,' +
    '  tier TEXT DEFAULT ''*'',' +
    '  description TEXT' +
    ')');

  // Fixed window counters
  Conn.ExecuteNonQuery(
    'CREATE TABLE rate_limit_counters (' +
    '  id INTEGER PRIMARY KEY,' +
    '  identifier TEXT NOT NULL,' +
    '  identifier_type TEXT NOT NULL,' +
    '  endpoint TEXT NOT NULL,' +
    '  window_start TEXT NOT NULL,' +
    '  window_seconds INTEGER NOT NULL,' +
    '  request_count INTEGER DEFAULT 0,' +
    '  limit_value INTEGER NOT NULL,' +
    '  UNIQUE(identifier, endpoint, window_start)' +
    ')');

  // Token bucket state
  Conn.ExecuteNonQuery(
    'CREATE TABLE token_buckets (' +
    '  id INTEGER PRIMARY KEY,' +
    '  identifier TEXT NOT NULL UNIQUE,' +
    '  tokens REAL NOT NULL,' +
    '  max_tokens INTEGER NOT NULL,' +
    '  refill_rate REAL NOT NULL,' +
    '  last_refill TEXT NOT NULL' +
    ')');

  // Request log
  Conn.ExecuteNonQuery(
    'CREATE TABLE request_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  api_key TEXT,' +
    '  ip_address TEXT NOT NULL,' +
    '  endpoint TEXT NOT NULL,' +
    '  method TEXT NOT NULL,' +
    '  status_code INTEGER,' +
    '  rate_limited INTEGER DEFAULT 0,' +
    '  response_time_ms INTEGER,' +
    '  requested_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Blocked IPs/keys
  Conn.ExecuteNonQuery(
    'CREATE TABLE blocked_clients (' +
    '  id INTEGER PRIMARY KEY,' +
    '  identifier TEXT NOT NULL,' +
    '  identifier_type TEXT NOT NULL,' +
    '  reason TEXT NOT NULL,' +
    '  blocked_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT,' +
    '  violation_count INTEGER DEFAULT 1' +
    ')');

  // Daily quotas
  Conn.ExecuteNonQuery(
    'CREATE TABLE daily_quotas (' +
    '  id INTEGER PRIMARY KEY,' +
    '  api_key TEXT NOT NULL,' +
    '  quota_date TEXT NOT NULL,' +
    '  requests_used INTEGER DEFAULT 0,' +
    '  requests_limit INTEGER NOT NULL,' +
    '  UNIQUE(api_key, quota_date)' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_counters_id ON rate_limit_counters(identifier, endpoint)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_request_log_key ON request_log(api_key, requested_at)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_request_log_ip ON request_log(ip_address, requested_at)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_blocked_id ON blocked_clients(identifier)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_quotas_key ON daily_quotas(api_key, quota_date)');

  WriteLn('   Created tables: api_keys, rate_limit_rules, rate_limit_counters,');
  WriteLn('                    token_buckets, request_log, blocked_clients, daily_quotas');
  WriteLn('');
end;

// =============================================================================
// Setup API Keys and Rules
// =============================================================================
{ Inserts API keys for free, pro, and enterprise tiers with per-minute/hour/day limits, and adds endpoint-specific rate limit rules. }
procedure SetupKeysAndRules;
begin
  WriteLn('2. Configuring API Keys and Rules');
  WriteLn('   =================================');

  // API Keys with different tiers
  Conn.ExecuteNonQuery(
    'INSERT INTO api_keys (key_value, owner, tier, requests_per_minute, requests_per_hour, requests_per_day, burst_limit) VALUES ' +
    '(''key_free_001'', ''Free User 1'', ''free'', 30, 500, 5000, 5)');
  Conn.ExecuteNonQuery(
    'INSERT INTO api_keys (key_value, owner, tier, requests_per_minute, requests_per_hour, requests_per_day, burst_limit) VALUES ' +
    '(''key_free_002'', ''Free User 2'', ''free'', 30, 500, 5000, 5)');
  Conn.ExecuteNonQuery(
    'INSERT INTO api_keys (key_value, owner, tier, requests_per_minute, requests_per_hour, requests_per_day, burst_limit) VALUES ' +
    '(''key_pro_001'', ''Pro User 1'', ''pro'', 120, 5000, 50000, 20)');
  Conn.ExecuteNonQuery(
    'INSERT INTO api_keys (key_value, owner, tier, requests_per_minute, requests_per_hour, requests_per_day, burst_limit) VALUES ' +
    '(''key_enterprise_001'', ''Enterprise Corp'', ''enterprise'', 600, 30000, 500000, 100)');

  WriteLn('   API Keys configured:');
  WriteLn('     Free tier:       30 req/min,  500 req/hr,   5K/day (burst: 5)');
  WriteLn('     Pro tier:        120 req/min, 5K req/hr,    50K/day (burst: 20)');
  WriteLn('     Enterprise tier: 600 req/min, 30K req/hr,   500K/day (burst: 100)');

  // Rate limit rules per endpoint
  Conn.ExecuteNonQuery(
    'INSERT INTO rate_limit_rules (endpoint, method, window_seconds, max_requests, tier, description) VALUES ' +
    '(''/api/search'', ''GET'', 60, 20, ''free'', ''Search limit for free tier'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO rate_limit_rules (endpoint, method, window_seconds, max_requests, tier, description) VALUES ' +
    '(''/api/search'', ''GET'', 60, 100, ''pro'', ''Search limit for pro tier'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO rate_limit_rules (endpoint, method, window_seconds, max_requests, tier, description) VALUES ' +
    '(''/api/upload'', ''POST'', 3600, 10, ''free'', ''Upload limit for free tier'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO rate_limit_rules (endpoint, method, window_seconds, max_requests, tier, description) VALUES ' +
    '(''/api/upload'', ''POST'', 3600, 100, ''pro'', ''Upload limit for pro tier'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO rate_limit_rules (endpoint, method, window_seconds, max_requests, tier, description) VALUES ' +
    '(''/api/data'', ''*'', 60, 60, ''*'', ''General data endpoint limit'')');

  WriteLn('');
  WriteLn('   Endpoint rules:');
  WriteLn('     /api/search GET: 20/min (free), 100/min (pro)');
  WriteLn('     /api/upload POST: 10/hr (free), 100/hr (pro)');
  WriteLn('     /api/data *: 60/min (all tiers)');
  WriteLn('');
end;

// =============================================================================
// Demo: Fixed Window Rate Limiting
// =============================================================================
{ Simulates 40 requests against a 30/min counter, allowing within the limit and returning 429 for excess, then displays rate limit headers. }
procedure DemoFixedWindow;
var
  I: Integer;
  Allowed, Denied: Integer;
  CurrentCount, LimitValue: Integer;
  WindowStart: string;
begin
  WriteLn('3. Fixed Window Rate Limiting');
  WriteLn('   ============================');
  WriteLn('');

  WindowStart := '2024-06-15 14:00:00';
  LimitValue := 30; // free tier: 30 req/min

  // Initialize counter
  Conn.ExecuteNonQuery(
    'INSERT INTO rate_limit_counters (identifier, identifier_type, endpoint, window_start, window_seconds, request_count, limit_value) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?)',
    ['key_free_001', 'api_key', '/api/data', WindowStart, 60, 0, LimitValue]);

  // Simulate 40 requests in one window
  Allowed := 0;
  Denied := 0;

  WriteLn(Format('   Simulating 40 requests (limit: %d/min):', [LimitValue]));
  for I := 1 to 40 do
  begin
    CurrentCount := Integer(Conn.ExecuteScalar(
      'SELECT request_count FROM rate_limit_counters WHERE identifier = ? AND endpoint = ? AND window_start = ?',
      ['key_free_001', '/api/data', WindowStart]));

    if CurrentCount < LimitValue then
    begin
      // Allow request
      Conn.ExecuteNonQuery(
        'UPDATE rate_limit_counters SET request_count = request_count + 1 ' +
        'WHERE identifier = ? AND endpoint = ? AND window_start = ?',
        ['key_free_001', '/api/data', WindowStart]);

      Conn.ExecuteNonQuery(
        'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, response_time_ms, requested_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
        ['key_free_001', '192.168.1.10', '/api/data', 'GET', 200, 0, 50 + (I mod 30), WindowStart]);
      Inc(Allowed);
    end
    else
    begin
      // Deny request (429)
      Conn.ExecuteNonQuery(
        'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, response_time_ms, requested_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
        ['key_free_001', '192.168.1.10', '/api/data', 'GET', 429, 1, 5, WindowStart]);
      Inc(Denied);
    end;
  end;

  WriteLn(Format('     Allowed: %d (status 200)', [Allowed]));
  WriteLn(Format('     Denied:  %d (status 429 - Too Many Requests)', [Denied]));

  // Show rate limit headers
  CurrentCount := Integer(Conn.ExecuteScalar(
    'SELECT request_count FROM rate_limit_counters WHERE identifier = ? AND endpoint = ?',
    ['key_free_001', '/api/data']));
  WriteLn('');
  WriteLn('   Response headers (last request):');
  WriteLn(Format('     X-RateLimit-Limit:     %d', [LimitValue]));
  WriteLn(Format('     X-RateLimit-Remaining: %d', [LimitValue - MinInt(CurrentCount, LimitValue)]));
  WriteLn(Format('     X-RateLimit-Reset:     %s', ['2024-06-15 14:01:00']));

  WriteLn('');
end;

// =============================================================================
// Demo: Sliding Window Counter
// =============================================================================
{ Creates six 10-second sub-windows to form a 60-second sliding window and simulates requests, summing counts across sub-windows. }
procedure DemoSlidingWindow;
var
  I, TotalInWindow, LimitValue: Integer;
  Allowed, Denied: Integer;
begin
  WriteLn('4. Sliding Window Counter');
  WriteLn('   =========================');
  WriteLn('');

  LimitValue := 20;

  // Create multiple sub-windows (simulate sliding by using 6x10sec windows instead of 1x60sec)
  WriteLn('   Sliding window approach (6 x 10-sec sub-windows for 60-sec total):');
  for I := 0 to 5 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO rate_limit_counters (identifier, identifier_type, endpoint, window_start, window_seconds, request_count, limit_value) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?)',
      ['key_free_002', 'api_key', '/api/search',
       Format('2024-06-15 14:00:%s', [Format('%.2d', [I * 10])]),
       10, 3 + (I mod 4), LimitValue]);
  end;

  // Calculate total across sub-windows
  TotalInWindow := Integer(Conn.ExecuteScalar(
    'SELECT SUM(request_count) FROM rate_limit_counters ' +
    'WHERE identifier = ''key_free_002'' AND endpoint = ''/api/search'''));

  WriteLn(Format('   Current window total: %d / %d', [TotalInWindow, LimitValue]));
  WriteLn('');

  // Simulate more requests
  Allowed := 0;
  Denied := 0;
  WriteLn(Format('   Simulating 10 more requests (remaining: %d):', [LimitValue - TotalInWindow]));

  for I := 1 to 10 do
  begin
    TotalInWindow := Integer(Conn.ExecuteScalar(
      'SELECT SUM(request_count) FROM rate_limit_counters ' +
      'WHERE identifier = ''key_free_002'' AND endpoint = ''/api/search'''));

    if TotalInWindow < LimitValue then
    begin
      // Add to current sub-window
      Conn.ExecuteNonQuery(
        'UPDATE rate_limit_counters SET request_count = request_count + 1 ' +
        'WHERE identifier = ''key_free_002'' AND endpoint = ''/api/search'' ' +
        'AND window_start = ''2024-06-15 14:00:50''');
      Inc(Allowed);

      Conn.ExecuteNonQuery(
        'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, requested_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        ['key_free_002', '10.0.0.5', '/api/search', 'GET', 200, 0, '2024-06-15 14:00:55']);
    end
    else
    begin
      Inc(Denied);
      Conn.ExecuteNonQuery(
        'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, requested_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        ['key_free_002', '10.0.0.5', '/api/search', 'GET', 429, 1, '2024-06-15 14:00:55']);
    end;
  end;

  WriteLn(Format('     Allowed: %d', [Allowed]));
  WriteLn(Format('     Denied:  %d', [Denied]));
  WriteLn('');
end;

// =============================================================================
// Demo: Token Bucket
// =============================================================================
{ Initializes a token bucket with max tokens and refill rate, then simulates a burst of 15 requests consuming and refilling tokens. }
procedure DemoTokenBucket;
var
  I: Integer;
  Tokens: Double;
  MaxTokens: Integer;
  RefillRate: Double;
  Allowed, Denied: Integer;
begin
  WriteLn('5. Token Bucket Algorithm');
  WriteLn('   =========================');
  WriteLn('');

  MaxTokens := 10;
  RefillRate := 2.0; // 2 tokens per second

  // Initialize bucket for pro user
  Conn.ExecuteNonQuery(
    'INSERT INTO token_buckets (identifier, tokens, max_tokens, refill_rate, last_refill) VALUES (?, ?, ?, ?, ?)',
    ['key_pro_001', MaxTokens, MaxTokens, RefillRate, '2024-06-15 14:00:00']);

  WriteLn(Format('   Bucket config: max=%d tokens, refill=%.0f tokens/sec', [MaxTokens, RefillRate]));
  WriteLn('');

  // Simulate burst of 15 requests
  Allowed := 0;
  Denied := 0;
  Tokens := MaxTokens;

  WriteLn('   Simulating burst of 15 requests:');
  for I := 1 to 15 do
  begin
    // Refill tokens (simulate 0.2 seconds between requests)
    Tokens := Tokens + RefillRate * 0.2;
    if Tokens > MaxTokens then
      Tokens := MaxTokens;

    if Tokens >= 1.0 then
    begin
      Tokens := Tokens - 1.0;
      Inc(Allowed);
      Conn.ExecuteNonQuery(
        'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, requested_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        ['key_pro_001', '172.16.0.20', '/api/data', 'GET', 200, 0,
         Format('2024-06-15 14:00:%.2d', [I])]);
    end
    else
    begin
      Inc(Denied);
      Conn.ExecuteNonQuery(
        'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, requested_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        ['key_pro_001', '172.16.0.20', '/api/data', 'GET', 429, 1,
         Format('2024-06-15 14:00:%.2d', [I])]);
    end;

    if I <= 15 then
      WriteLn(Format('     Request %2d: tokens=%.1f -> %s',
        [I, Tokens, IfThen(Tokens >= 0, 'ALLOWED', 'DENIED')]));
  end;

  // Update bucket state
  Conn.ExecuteNonQuery(
    'UPDATE token_buckets SET tokens = ?, last_refill = ? WHERE identifier = ?',
    [Tokens, '2024-06-15 14:00:15', 'key_pro_001']);

  WriteLn('');
  WriteLn(Format('   Results: %d allowed, %d denied', [Allowed, Denied]));
  WriteLn(Format('   Tokens remaining: %.1f', [Tokens]));
  WriteLn('');
end;

// =============================================================================
// Demo: Per-IP Rate Limiting
// =============================================================================
{ Simulates different request volumes from four IP addresses against a per-IP counter and reports which ones exceed the limit. }
procedure DemoPerIPLimiting;
var
  I: Integer;
  IPs: array[0..3] of string;
  Counts: array[0..3] of Integer;
  LimitPerIP: Integer;
begin
  WriteLn('6. Per-IP Rate Limiting');
  WriteLn('   =======================');
  WriteLn('');

  IPs[0] := '203.0.113.1';   // Normal user
  IPs[1] := '203.0.113.2';   // Normal user
  IPs[2] := '198.51.100.50'; // Suspicious (many requests)
  IPs[3] := '198.51.100.99'; // Abusive

  LimitPerIP := 15;
  Counts[0] := 0; Counts[1] := 0; Counts[2] := 0; Counts[3] := 0;

  WriteLn(Format('   IP limit: %d requests/min (unauthenticated)', [LimitPerIP]));
  WriteLn('');

  // Initialize IP counters
  for I := 0 to 3 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO rate_limit_counters (identifier, identifier_type, endpoint, window_start, window_seconds, request_count, limit_value) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?)',
      [IPs[I], 'ip', '/api/public', '2024-06-15 14:02:00', 60, 0, LimitPerIP]);
  end;

  // Simulate varied request patterns
  // IP[0]: 8 requests (normal)
  for I := 1 to 8 do
  begin
    Conn.ExecuteNonQuery(
      'UPDATE rate_limit_counters SET request_count = request_count + 1 WHERE identifier = ? AND endpoint = ?',
      [IPs[0], '/api/public']);
    Counts[0] := I;
  end;

  // IP[1]: 5 requests (light)
  for I := 1 to 5 do
  begin
    Conn.ExecuteNonQuery(
      'UPDATE rate_limit_counters SET request_count = request_count + 1 WHERE identifier = ? AND endpoint = ?',
      [IPs[1], '/api/public']);
    Counts[1] := I;
  end;

  // IP[2]: 15 requests (at limit)
  for I := 1 to 15 do
  begin
    Conn.ExecuteNonQuery(
      'UPDATE rate_limit_counters SET request_count = request_count + 1 WHERE identifier = ? AND endpoint = ?',
      [IPs[2], '/api/public']);
    Counts[2] := I;
  end;

  // IP[3]: 25 requests (exceeds limit, 10 denied)
  for I := 1 to 25 do
  begin
    Conn.ExecuteNonQuery(
      'UPDATE rate_limit_counters SET request_count = request_count + 1 WHERE identifier = ? AND endpoint = ?',
      [IPs[3], '/api/public']);
    Counts[3] := I;
  end;

  // Show per-IP status
  WriteLn(Format('   %-18s | %-8s | %-6s | %s', ['IP Address', 'Requests', 'Limit', 'Status']));
  WriteLn('   ' + StringOfChar('-', 55));
  for I := 0 to 3 do
  begin
    WriteLn(Format('   %-18s | %-8d | %-6d | %s',
      [IPs[I], Counts[I], LimitPerIP,
       IfThen(Counts[I] <= LimitPerIP, 'OK',
         Format('EXCEEDED (+%d denied)', [Counts[I] - LimitPerIP]))]));
  end;

  WriteLn('');
end;

// =============================================================================
// Demo: Tiered Limits
// =============================================================================
{ Displays API key rate limits by tier (free/pro/enterprise) and endpoint-specific rules with window sizes and max requests. }
procedure DemoTieredLimits;
var
  DS: TDataSet;
begin
  WriteLn('7. Tiered Rate Limits');
  WriteLn('   =====================');
  WriteLn('');

  WriteLn('   Rate limits by tier:');
  DS := Conn.ExecuteQuery(
    'SELECT tier, owner, key_value, requests_per_minute, requests_per_hour, requests_per_day, burst_limit ' +
    'FROM api_keys WHERE is_active = 1 ORDER BY ' +
    'CASE tier WHEN ''free'' THEN 1 WHEN ''pro'' THEN 2 WHEN ''enterprise'' THEN 3 END');
  try
    WriteLn(Format('   %-12s | %-18s | %-8s | %-8s | %-8s | %s',
      ['Tier', 'Owner', 'Req/Min', 'Req/Hr', 'Req/Day', 'Burst']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s | %-18s | %-8s | %-8s | %-8s | %s',
        [DS.FieldByName('tier').AsString,
         DS.FieldByName('owner').AsString,
         DS.FieldByName('requests_per_minute').AsString,
         DS.FieldByName('requests_per_hour').AsString,
         DS.FieldByName('requests_per_day').AsString,
         DS.FieldByName('burst_limit').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show endpoint-specific rules
  WriteLn('');
  WriteLn('   Endpoint-specific rules:');
  DS := Conn.ExecuteQuery(
    'SELECT endpoint, method, tier, max_requests, window_seconds, description ' +
    'FROM rate_limit_rules ORDER BY endpoint, tier');
  try
    WriteLn(Format('   %-15s | %-6s | %-12s | %-6s | %-8s | %s',
      ['Endpoint', 'Method', 'Tier', 'Limit', 'Window', 'Description']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s | %-6s | %-12s | %-6s | %-8s | %s',
        [DS.FieldByName('endpoint').AsString,
         DS.FieldByName('method').AsString,
         DS.FieldByName('tier').AsString,
         DS.FieldByName('max_requests').AsString,
         DS.FieldByName('window_seconds').AsString + 's',
         DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Demo: Abuse Detection
// =============================================================================
{ Detects IPs with excessive 429 responses in the request log and inserts them into the blocked_clients table with a timed ban. }
procedure DemoAbuseDetection;
var
  DS: TDataSet;
  RateLimitedCount: Integer;
begin
  WriteLn('8. Abuse Detection and Blocking');
  WriteLn('   ================================');
  WriteLn('');

  // Check for IPs with too many 429s
  WriteLn('   Checking for abusive clients (>10 rate-limited requests):');

  // Simulate some abusive requests for IP 198.51.100.99
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  Conn.ExecuteNonQuery(
    'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, requested_at) VALUES ' +
    '(NULL, ''198.51.100.99'', ''/api/public'', ''GET'', 429, 1, ''2024-06-15 14:02:01'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, requested_at) VALUES ' +
    '(NULL, ''198.51.100.99'', ''/api/public'', ''GET'', 429, 1, ''2024-06-15 14:02:02'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO request_log (api_key, ip_address, endpoint, method, status_code, rate_limited, requested_at) VALUES ' +
    '(NULL, ''198.51.100.99'', ''/api/public'', ''GET'', 429, 1, ''2024-06-15 14:02:03'')');
  Conn.ExecuteNonQuery('COMMIT');

  // Detect abuse
  DS := Conn.ExecuteQuery(
    'SELECT ip_address, COUNT(*) AS violations ' +
    'FROM request_log WHERE rate_limited = 1 ' +
    'GROUP BY ip_address HAVING violations >= 3 ' +
    'ORDER BY violations DESC');
  try
    while not DS.EOF do
    begin
      RateLimitedCount := DS.FieldByName('violations').AsInteger;
      WriteLn(Format('   DETECTED: %s - %d rate-limited requests',
        [DS.FieldByName('ip_address').AsString, RateLimitedCount]));

      // Block the IP
      Conn.ExecuteNonQuery(
        'INSERT INTO blocked_clients (identifier, identifier_type, reason, blocked_at, expires_at, violation_count) ' +
        'VALUES (?, ?, ?, ?, ?, ?)',
        [DS.FieldByName('ip_address').AsString, 'ip',
         Format('Excessive rate limiting violations (%d)', [RateLimitedCount]),
         '2024-06-15 14:03:00',
         '2024-06-15 15:03:00', // 1 hour ban
         RateLimitedCount]);

      WriteLn(Format('   BLOCKED: %s for 1 hour (until 15:03:00)',
        [DS.FieldByName('ip_address').AsString]));

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show blocked list
  WriteLn('');
  WriteLn('   Blocked clients:');
  DS := Conn.ExecuteQuery('SELECT identifier, identifier_type, reason, expires_at, violation_count FROM blocked_clients');
  try
    WriteLn(Format('   %-18s | %-5s | %-40s | %-20s | %s',
      ['Identifier', 'Type', 'Reason', 'Expires', 'Violations']));
    WriteLn('   ' + StringOfChar('-', 100));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s | %-5s | %-40s | %-20s | %s',
        [DS.FieldByName('identifier').AsString,
         DS.FieldByName('identifier_type').AsString,
         DS.FieldByName('reason').AsString,
         DS.FieldByName('expires_at').AsString,
         DS.FieldByName('violation_count').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Demo: Daily Quotas
// =============================================================================
{ Populates daily quota records for each API key and displays usage percentage with warning/high/ok status indicators. }
procedure DemoDailyQuotas;
var
  DS: TDataSet;
begin
  WriteLn('9. Daily Quota Management');
  WriteLn('   =========================');
  WriteLn('');

  // Initialize daily quotas
  Conn.ExecuteNonQuery(
    'INSERT INTO daily_quotas (api_key, quota_date, requests_used, requests_limit) VALUES ' +
    '(''key_free_001'', ''2024-06-15'', 3847, 5000)');
  Conn.ExecuteNonQuery(
    'INSERT INTO daily_quotas (api_key, quota_date, requests_used, requests_limit) VALUES ' +
    '(''key_free_002'', ''2024-06-15'', 4920, 5000)');
  Conn.ExecuteNonQuery(
    'INSERT INTO daily_quotas (api_key, quota_date, requests_used, requests_limit) VALUES ' +
    '(''key_pro_001'', ''2024-06-15'', 12500, 50000)');
  Conn.ExecuteNonQuery(
    'INSERT INTO daily_quotas (api_key, quota_date, requests_used, requests_limit) VALUES ' +
    '(''key_enterprise_001'', ''2024-06-15'', 45000, 500000)');

  // Show quota status
  DS := Conn.ExecuteQuery(
    'SELECT q.api_key, a.owner, a.tier, q.requests_used, q.requests_limit, ' +
    'ROUND((q.requests_used * 100.0 / q.requests_limit), 1) AS pct_used ' +
    'FROM daily_quotas q ' +
    'JOIN api_keys a ON q.api_key = a.key_value ' +
    'WHERE q.quota_date = ''2024-06-15'' ' +
    'ORDER BY pct_used DESC');
  try
    WriteLn(Format('   %-18s | %-12s | %-8s | %-8s | %-6s | %s',
      ['Owner', 'Tier', 'Used', 'Limit', '% Used', 'Status']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s | %-12s | %-8s | %-8s | %-6s | %s',
        [DS.FieldByName('owner').AsString,
         DS.FieldByName('tier').AsString,
         DS.FieldByName('requests_used').AsString,
         DS.FieldByName('requests_limit').AsString,
         DS.FieldByName('pct_used').AsString + '%',
         IfThen(DS.FieldByName('pct_used').AsFloat >= 90, 'WARNING',
           IfThen(DS.FieldByName('pct_used').AsFloat >= 75, 'HIGH', 'OK'))]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Demo: Statistics
// =============================================================================
{ Demonstrates rate limiting statistics and violation reporting. }
procedure DemoStatistics;
var
  DS: TDataSet;
  TotalRequests, RateLimited: Integer;
begin
  WriteLn('10. Rate Limiting Statistics');
  WriteLn('    ===========================');
  WriteLn('');

  TotalRequests := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM request_log'));
  RateLimited := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM request_log WHERE rate_limited = 1'));

  WriteLn(Format('    Total requests logged:   %d', [TotalRequests]));
  WriteLn(Format('    Rate-limited (429):      %d (%.1f%%)',
    [RateLimited, RateLimited * 100.0 / TotalRequests]));
  WriteLn(Format('    Successful (200):        %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM request_log WHERE status_code = 200'))]));
  WriteLn(Format('    Blocked clients:         %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM blocked_clients'))]));

  // Per-endpoint stats
  WriteLn('');
  WriteLn('    Requests by endpoint:');
  DS := Conn.ExecuteQuery(
    'SELECT endpoint, COUNT(*) AS total, ' +
    'SUM(CASE WHEN rate_limited = 1 THEN 1 ELSE 0 END) AS limited ' +
    'FROM request_log GROUP BY endpoint ORDER BY total DESC');
  try
    WriteLn(Format('    %-15s | %-8s | %-8s | %s', ['Endpoint', 'Total', 'Limited', 'Rate']));
    WriteLn('    ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('    %-15s | %-8s | %-8s | %.1f%%',
        [DS.FieldByName('endpoint').AsString,
         DS.FieldByName('total').AsString,
         DS.FieldByName('limited').AsString,
         DS.FieldByName('limited').AsFloat / DS.FieldByName('total').AsFloat * 100.0]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Per-key stats
  WriteLn('');
  WriteLn('    Requests by API key:');
  DS := Conn.ExecuteQuery(
    'SELECT COALESCE(api_key, ''(no key)'') AS key_id, COUNT(*) AS total, ' +
    'SUM(CASE WHEN rate_limited = 1 THEN 1 ELSE 0 END) AS limited ' +
    'FROM request_log GROUP BY api_key ORDER BY total DESC');
  try
    WriteLn(Format('    %-20s | %-8s | %-8s | %s', ['API Key', 'Total', 'Limited', 'Rate']));
    WriteLn('    ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('    %-20s | %-8s | %-8s | %.1f%%',
        [DS.FieldByName('key_id').AsString,
         DS.FieldByName('total').AsString,
         DS.FieldByName('limited').AsString,
         DS.FieldByName('limited').AsFloat / DS.FieldByName('total').AsFloat * 100.0]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Token bucket state
  WriteLn('');
  WriteLn('    Token bucket states:');
  DS := Conn.ExecuteQuery('SELECT identifier, tokens, max_tokens, refill_rate FROM token_buckets');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('      %s: %.1f/%s tokens (refill: %s/sec)',
        [DS.FieldByName('identifier').AsString,
         DS.FieldByName('tokens').AsFloat,
         DS.FieldByName('max_tokens').AsString,
         DS.FieldByName('refill_rate').AsString]));
      DS.Next;
    end;
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
  WriteLn('=== NDXSQLite Example 88: API Rate Limiting ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    SetupKeysAndRules;
    DemoFixedWindow;
    DemoSlidingWindow;
    DemoTokenBucket;
    DemoPerIPLimiting;
    DemoTieredLimits;
    DemoAbuseDetection;
    DemoDailyQuotas;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
