{===============================================================================
  NDXSQLite Example 46 - Session Store Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Web session management with SQLite
  - Session creation and validation
  - Session data storage (key-value)
  - Expiration and cleanup
  - Session activity tracking
  - Security features (IP binding, user agent)

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SessionStore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

const
  DEFAULT_SESSION_LIFETIME = 3600;  // 1 hour in seconds
  CLEANUP_THRESHOLD = 100;           // Cleanup after this many expired sessions

{ Generates a unique session identifier by extracting hex characters from a GUID. }
function GenerateSessionId: string;
var
  I: Integer;
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := '';
  for I := 1 to Length(GUIDToString(GUID)) do
    if GUIDToString(GUID)[I] in ['0'..'9', 'A'..'F', 'a'..'f'] then
      Result := Result + GUIDToString(GUID)[I];
  Result := LowerCase(Result);
end;

{ Creates the sessions, session_data, and session_activity tables with supporting indexes. }
procedure SetupSessionStore;
begin
  // Sessions table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS sessions (' +
    '  id TEXT PRIMARY KEY,' +
    '  user_id TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT NOT NULL,' +
    '  last_activity TEXT DEFAULT (datetime(''now'')),' +
    '  ip_address TEXT,' +
    '  user_agent TEXT,' +
    '  is_valid INTEGER DEFAULT 1' +
    ')');

  // Session data (key-value storage)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS session_data (' +
    '  session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,' +
    '  key TEXT NOT NULL,' +
    '  value TEXT,' +
    '  PRIMARY KEY (session_id, key)' +
    ')');

  // Session activity log
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS session_activity (' +
    '  id INTEGER PRIMARY KEY,' +
    '  session_id TEXT,' +
    '  action TEXT,' +               // create, access, invalidate, expire
    '  ip_address TEXT,' +
    '  user_agent TEXT,' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_sessions_user ON sessions(user_id)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_sessions_expires ON sessions(expires_at)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_sessions_valid ON sessions(is_valid, expires_at)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_activity_session ON session_activity(session_id)');
end;

// ============================================================================
// Session operations
// ============================================================================

{ Creates a new session record with an expiration time and logs the creation in the activity table. }
function CreateSession(const AUserId, AIpAddress, AUserAgent: string;
  ALifetimeSeconds: Integer = DEFAULT_SESSION_LIFETIME): string;
var
  ExpiresAt: string;
begin
  Result := GenerateSessionId;
  ExpiresAt := FormatDateTime('yyyy-mm-dd hh:nn:ss',
    IncSecond(Now, ALifetimeSeconds));

  Connection.ExecuteNonQuery(
    'INSERT INTO sessions (id, user_id, expires_at, ip_address, user_agent) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [Result, AUserId, ExpiresAt, AIpAddress, AUserAgent]);

  Connection.ExecuteNonQuery(
    'INSERT INTO session_activity (session_id, action, ip_address, user_agent) ' +
    'VALUES (?, ?, ?, ?)',
    [Result, 'create', AIpAddress, AUserAgent]);
end;

{ Checks whether a session is valid and not expired, optionally verifying IP binding and updating last activity. }
function ValidateSession(const ASessionId: string; const AIpAddress: string = '';
  AUpdateActivity: Boolean = True): Boolean;
var
  DS: TDataSet;
begin
  Result := False;

  DS := Connection.ExecuteQuery(
    'SELECT id, ip_address, is_valid FROM sessions ' +
    'WHERE id = ? AND is_valid = 1 AND expires_at > datetime(''now'')',
    [ASessionId]);
  try
    if not DS.EOF then
    begin
      // Optional: Check IP binding
      if (AIpAddress <> '') and (DS.FieldByName('ip_address').AsString <> AIpAddress) then
      begin
        // IP mismatch - potential session hijacking
        Connection.ExecuteNonQuery(
          'INSERT INTO session_activity (session_id, action, ip_address) VALUES (?, ?, ?)',
          [ASessionId, 'ip_mismatch', AIpAddress]);
        Exit;
      end;

      Result := True;

      if AUpdateActivity then
      begin
        Connection.ExecuteNonQuery(
          'UPDATE sessions SET last_activity = datetime(''now'') WHERE id = ?',
          [ASessionId]);
      end;
    end;
  finally
    DS.Free;
  end;
end;

{ Sets the session data. }
procedure SetSessionData(const ASessionId, AKey, AValue: string);
begin
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO session_data (session_id, key, value) VALUES (?, ?, ?)',
    [ASessionId, AKey, AValue]);
end;

{ Returns the session data. }
function GetSessionData(const ASessionId, AKey: string; const ADefault: string = ''): string;
var
  DS: TDataSet;
begin
  Result := ADefault;
  DS := Connection.ExecuteQuery(
    'SELECT value FROM session_data WHERE session_id = ? AND key = ?',
    [ASessionId, AKey]);
  try
    if not DS.EOF then
      Result := DS.FieldByName('value').AsString;
  finally
    DS.Free;
  end;
end;

{ Marks a session as invalid and logs the invalidation in the activity table. }
procedure InvalidateSession(const ASessionId: string);
begin
  Connection.ExecuteNonQuery(
    'UPDATE sessions SET is_valid = 0 WHERE id = ?',
    [ASessionId]);

  Connection.ExecuteNonQuery(
    'INSERT INTO session_activity (session_id, action) VALUES (?, ?)',
    [ASessionId, 'invalidate']);
end;

{ Invalidates all active sessions belonging to the specified user. }
procedure InvalidateUserSessions(const AUserId: string);
begin
  Connection.ExecuteNonQuery(
    'UPDATE sessions SET is_valid = 0 WHERE user_id = ? AND is_valid = 1',
    [AUserId]);
end;

{ Invalidates expired sessions and returns the number affected. }
function CleanupExpiredSessions: Integer;
var
  DS: TDataSet;
begin
  // Log expired sessions
  Connection.ExecuteNonQuery(
    'INSERT INTO session_activity (session_id, action) ' +
    'SELECT id, ''expire'' FROM sessions WHERE expires_at <= datetime(''now'') AND is_valid = 1');

  // Mark as invalid
  Connection.ExecuteNonQuery(
    'UPDATE sessions SET is_valid = 0 WHERE expires_at <= datetime(''now'')');

  // Delete old session data (keep sessions for audit)
  Connection.ExecuteNonQuery(
    'DELETE FROM session_data WHERE session_id IN ' +
    '(SELECT id FROM sessions WHERE is_valid = 0 AND expires_at < datetime(''now'', ''-7 days''))');

  DS := Connection.ExecuteQuery('SELECT changes()');
  try
    Result := DS.Fields[0].AsInteger;
  finally
    DS.Free;
  end;
end;

{ Extends the expiration time of a valid session by the specified number of seconds. }
procedure ExtendSession(const ASessionId: string; AExtendSeconds: Integer);
begin
  Connection.ExecuteNonQuery(
    'UPDATE sessions SET expires_at = datetime(expires_at, ''+' + IntToStr(AExtendSeconds) + ' seconds'') ' +
    'WHERE id = ? AND is_valid = 1',
    [ASessionId]);
end;

// ============================================================================
// Demo procedures
// ============================================================================

{ Creates sessions for multiple users, validates one, and displays all active sessions. }
procedure DemoBasicSessions;
var
  SessionId: string;
  DS: TDataSet;
begin
  WriteLn('1. Basic session management');
  WriteLn('   ------------------------');
  WriteLn('');

  // Create sessions
  WriteLn('   Creating sessions for users...');
  SessionId := CreateSession('user_alice', '192.168.1.100', 'Mozilla/5.0 Chrome');
  WriteLn('   Alice session: ', Copy(SessionId, 1, 16), '...');

  CreateSession('user_bob', '192.168.1.101', 'Mozilla/5.0 Firefox');
  CreateSession('user_carol', '192.168.1.102', 'Mozilla/5.0 Safari');

  WriteLn('');

  // Validate session
  WriteLn('   Validating Alice''s session...');
  if ValidateSession(SessionId) then
    WriteLn('   Session is VALID')
  else
    WriteLn('   Session is INVALID');

  WriteLn('');

  // Show active sessions
  DS := Connection.ExecuteQuery(
    'SELECT user_id, created_at, expires_at, ip_address ' +
    'FROM sessions WHERE is_valid = 1 ORDER BY created_at');
  try
    WriteLn('   Active sessions:');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('user_id').AsString,
              ' from ', DS.FieldByName('ip_address').AsString,
              ' (expires: ', DS.FieldByName('expires_at').AsString, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates a session and stores key-value data pairs, then retrieves and displays them. }
procedure DemoSessionData;
var
  SessionId: string;
begin
  WriteLn('2. Session data storage');
  WriteLn('   --------------------');
  WriteLn('');

  // Create session with data
  SessionId := CreateSession('user_dave', '10.0.0.50', 'Mozilla/5.0');

  WriteLn('   Storing session data...');
  SetSessionData(SessionId, 'cart_items', '["item1", "item2", "item3"]');
  SetSessionData(SessionId, 'language', 'en-US');
  SetSessionData(SessionId, 'theme', 'dark');
  SetSessionData(SessionId, 'last_page', '/products/123');

  WriteLn('');

  // Retrieve data
  WriteLn('   Retrieving session data:');
  WriteLn('   - cart_items: ', GetSessionData(SessionId, 'cart_items'));
  WriteLn('   - language: ', GetSessionData(SessionId, 'language'));
  WriteLn('   - theme: ', GetSessionData(SessionId, 'theme'));
  WriteLn('   - last_page: ', GetSessionData(SessionId, 'last_page'));
  WriteLn('   - missing_key: "', GetSessionData(SessionId, 'missing', 'default_value'), '"');

  WriteLn('');
end;

{ Creates an already-expired session, validates it to show failure, runs cleanup, and displays session counts. }
procedure DemoSessionExpiration;
var
  ShortSession: string;
  DS: TDataSet;
begin
  WriteLn('3. Session expiration');
  WriteLn('   ------------------');
  WriteLn('');

  // Create a short-lived session (already expired for demo)
  ShortSession := GenerateSessionId;
  Connection.ExecuteNonQuery(
    'INSERT INTO sessions (id, user_id, expires_at, ip_address, is_valid) ' +
    'VALUES (?, ?, datetime(''now'', ''-1 hour''), ?, 1)',
    [ShortSession, 'expired_user', '192.168.1.200']);

  WriteLn('   Created expired session for demo');
  WriteLn('');

  // Validate expired session
  WriteLn('   Validating expired session...');
  if ValidateSession(ShortSession) then
    WriteLn('   Session is VALID')
  else
    WriteLn('   Session is INVALID (expired)');

  WriteLn('');

  // Cleanup
  WriteLn('   Running cleanup...');
  CleanupExpiredSessions;

  DS := Connection.ExecuteQuery(
    'SELECT COUNT(*) as total, ' +
    '       SUM(CASE WHEN is_valid = 1 THEN 1 ELSE 0 END) as valid, ' +
    '       SUM(CASE WHEN is_valid = 0 THEN 1 ELSE 0 END) as invalid ' +
    'FROM sessions');
  try
    WriteLn('   Total sessions: ', DS.FieldByName('total').AsInteger);
    WriteLn('   Valid: ', DS.FieldByName('valid').AsInteger);
    WriteLn('   Invalid/Expired: ', DS.FieldByName('invalid').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Tests IP-bound session validation and invalidates all sessions for a multi-device user. }
procedure DemoSessionSecurity;
var
  SessionId: string;
begin
  WriteLn('4. Session security features');
  WriteLn('   -------------------------');
  WriteLn('');

  // Create session with IP binding
  SessionId := CreateSession('secure_user', '192.168.1.50', 'SecureBrowser/1.0');
  WriteLn('   Created session bound to IP 192.168.1.50');

  // Validate from same IP
  WriteLn('   Validating from same IP (192.168.1.50)...');
  if ValidateSession(SessionId, '192.168.1.50') then
    WriteLn('   Result: VALID')
  else
    WriteLn('   Result: INVALID');

  // Validate from different IP
  WriteLn('   Validating from different IP (10.0.0.99)...');
  if ValidateSession(SessionId, '10.0.0.99') then
    WriteLn('   Result: VALID')
  else
    WriteLn('   Result: INVALID (IP mismatch - possible hijacking)');

  WriteLn('');

  // Invalidate all user sessions (logout everywhere)
  WriteLn('   Logging out user from all sessions...');
  CreateSession('multi_device_user', '192.168.1.1', 'Desktop');
  CreateSession('multi_device_user', '192.168.1.2', 'Mobile');
  CreateSession('multi_device_user', '192.168.1.3', 'Tablet');

  InvalidateUserSessions('multi_device_user');
  WriteLn('   All sessions for multi_device_user invalidated');

  WriteLn('');
end;

{ Creates a session with a 30-minute lifetime, extends it by another 30 minutes, and displays both expiration times. }
procedure DemoSessionExtension;
var
  SessionId: string;
  DS: TDataSet;
begin
  WriteLn('5. Session extension (sliding expiration)');
  WriteLn('   --------------------------------------');
  WriteLn('');

  SessionId := CreateSession('active_user', '192.168.1.77', 'Browser', 1800);  // 30 min

  DS := Connection.ExecuteQuery('SELECT expires_at FROM sessions WHERE id = ?', [SessionId]);
  try
    WriteLn('   Initial expiration: ', DS.FieldByName('expires_at').AsString);
  finally
    DS.Free;
  end;

  // Extend by 30 minutes
  ExtendSession(SessionId, 1800);

  DS := Connection.ExecuteQuery('SELECT expires_at FROM sessions WHERE id = ?', [SessionId]);
  try
    WriteLn('   After extension:    ', DS.FieldByName('expires_at').AsString);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Queries and displays the 10 most recent entries from the session activity log. }
procedure DemoSessionActivity;
var
  DS: TDataSet;
begin
  WriteLn('6. Session activity tracking');
  WriteLn('   -------------------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT session_id, action, ip_address, timestamp ' +
    'FROM session_activity ORDER BY timestamp DESC LIMIT 10');
  try
    WriteLn('   Recent activity:');
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('timestamp').AsString, '] ',
              DS.FieldByName('action').AsString:12, ' ',
              'session: ', Copy(DS.FieldByName('session_id').AsString, 1, 8), '...',
              ' from ', DS.FieldByName('ip_address').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays aggregate session statistics including total, active, unique users, unique IPs, and per-user counts. }
procedure DemoSessionStats;
var
  DS: TDataSet;
begin
  WriteLn('7. Session statistics');
  WriteLn('   ------------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  COUNT(*) as total_sessions, ' +
    '  SUM(CASE WHEN is_valid = 1 AND expires_at > datetime(''now'') THEN 1 ELSE 0 END) as active, ' +
    '  COUNT(DISTINCT user_id) as unique_users, ' +
    '  COUNT(DISTINCT ip_address) as unique_ips ' +
    'FROM sessions');
  try
    WriteLn('   Total sessions:  ', DS.FieldByName('total_sessions').AsInteger);
    WriteLn('   Active sessions: ', DS.FieldByName('active').AsInteger);
    WriteLn('   Unique users:    ', DS.FieldByName('unique_users').AsInteger);
    WriteLn('   Unique IPs:      ', DS.FieldByName('unique_ips').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT user_id, COUNT(*) as session_count ' +
    'FROM sessions GROUP BY user_id ORDER BY session_count DESC LIMIT 5');
  try
    WriteLn('   Sessions per user:');
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('user_id').AsString:20,
              ': ', DS.FieldByName('session_count').AsInteger, ' sessions');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the database file and its WAL/SHM journal files from disk. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  WriteLn('=== NDXSQLite Example 46: Session Store Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example46.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupSessionStore;
      WriteLn('Session store database initialized');
      WriteLn('');

      DemoBasicSessions;
      DemoSessionData;
      DemoSessionExpiration;
      DemoSessionSecurity;
      DemoSessionExtension;
      DemoSessionActivity;
      DemoSessionStats;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
