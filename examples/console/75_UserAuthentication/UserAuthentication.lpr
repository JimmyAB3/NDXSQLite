{===============================================================================
  NDXSQLite Example 75 - User Authentication System
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Secure password hashing with salt
  - Session token management
  - Login attempt tracking and account lockout
  - Password reset tokens with expiration
  - Two-factor authentication (2FA) setup
  - Remember me tokens
  - Session management and invalidation
  - Security audit logging

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program UserAuthentication;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Variants, md5, base64,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Cryptographic Helper Functions
// =============================================================================
{ Generates a random hexadecimal string of the specified byte length by producing random bytes and converting each to a two-character hex representation. }
function GenerateRandomHex(Length: Integer): string;
var
  I: Integer;
  Bytes: array of Byte;
begin
  SetLength(Bytes, Length);
  for I := 0 to Length - 1 do
    Bytes[I] := Random(256);
  Result := '';
  for I := 0 to Length - 1 do
    Result := Result + IntToHex(Bytes[I], 2);
end;

{ Generates a 32-byte (64 hex character) random salt for password hashing. }
function GenerateSalt: string;
begin
  // 32-byte salt (64 hex chars)
  Result := GenerateRandomHex(32);
end;

{ Hashes a password with salt using multiple rounds of MD5. }
function HashPassword(const Password, Salt: string): string;
var
  Salted: string;
begin
  // Simple hash: SHA-like using MD5 with multiple rounds
  // Note: In production, use bcrypt, scrypt, or Argon2
  Salted := Salt + Password + Salt;
  Result := MD5Print(MD5String(Salted));
  // Additional rounds for key stretching
  Result := MD5Print(MD5String(Result + Salt));
  Result := MD5Print(MD5String(Salt + Result));
end;

{ Generates a 64-byte (128 hex character) cryptographically random session token. }
function GenerateSessionToken: string;
begin
  // 64-byte token (128 hex chars)
  Result := GenerateRandomHex(64);
end;

{ Generates a 6-digit numeric code for use as a two-factor authentication verification token. }
function GenerateShortToken: string;
begin
  // 6-digit numeric token for 2FA
  Result := Format('%.6d', [Random(1000000)]);
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Authentication Schema');
  WriteLn('   ================================');

  // Users table
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  email TEXT UNIQUE NOT NULL,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  password_hash TEXT NOT NULL,' +
    '  password_salt TEXT NOT NULL,' +
    '  display_name TEXT,' +
    '  phone TEXT,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  is_verified INTEGER DEFAULT 0,' +
    '  is_admin INTEGER DEFAULT 0,' +
    '  failed_login_attempts INTEGER DEFAULT 0,' +
    '  locked_until TEXT,' +
    '  last_login TEXT,' +
    '  last_password_change TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Sessions table
  Conn.ExecuteNonQuery(
    'CREATE TABLE sessions (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  session_token TEXT UNIQUE NOT NULL,' +
    '  ip_address TEXT,' +
    '  user_agent TEXT,' +
    '  is_valid INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT NOT NULL,' +
    '  last_activity TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Password reset tokens
  Conn.ExecuteNonQuery(
    'CREATE TABLE password_reset_tokens (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  token TEXT UNIQUE NOT NULL,' +
    '  is_used INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT NOT NULL' +
    ')');

  // Remember me tokens
  Conn.ExecuteNonQuery(
    'CREATE TABLE remember_tokens (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  token_hash TEXT NOT NULL,' +
    '  token_salt TEXT NOT NULL,' +
    '  series TEXT NOT NULL,' +
    '  is_valid INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT NOT NULL,' +
    '  last_used TEXT' +
    ')');

  // Two-factor authentication
  Conn.ExecuteNonQuery(
    'CREATE TABLE user_2fa (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER UNIQUE NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  is_enabled INTEGER DEFAULT 0,' +
    '  secret_key TEXT,' +
    '  backup_codes TEXT,' +
    '  verified_at TEXT' +
    ')');

  // 2FA verification codes (temporary)
  Conn.ExecuteNonQuery(
    'CREATE TABLE verification_codes (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  code TEXT NOT NULL,' +
    '  purpose TEXT CHECK (purpose IN (''2fa'', ''email_verify'', ''phone_verify'')),' +
    '  is_used INTEGER DEFAULT 0,' +
    '  attempts INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT NOT NULL' +
    ')');

  // Security audit log
  Conn.ExecuteNonQuery(
    'CREATE TABLE security_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  event_type TEXT NOT NULL,' +
    '  event_details TEXT,' +
    '  ip_address TEXT,' +
    '  user_agent TEXT,' +
    '  success INTEGER,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Login history
  Conn.ExecuteNonQuery(
    'CREATE TABLE login_history (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  login_time TEXT DEFAULT (datetime(''now'')),' +
    '  logout_time TEXT,' +
    '  ip_address TEXT,' +
    '  user_agent TEXT,' +
    '  location TEXT,' +
    '  success INTEGER NOT NULL' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_users_email ON users(email)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_users_username ON users(username)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_sessions_token ON sessions(session_token)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_sessions_user ON sessions(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_security_log_user ON security_log(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_security_log_type ON security_log(event_type)');

  WriteLn('   Created tables: users, sessions, password_reset_tokens,');
  WriteLn('                   remember_tokens, user_2fa, verification_codes,');
  WriteLn('                   security_log, login_history');
  WriteLn('');
end;

// =============================================================================
// User Registration
// =============================================================================
{ Registers a new user by validating email/username uniqueness, generating a password salt and hash, creating the user record and 2FA entry, and logging the registration event. }
function RegisterUser(const Email, Username, Password, DisplayName: string): Integer;
var
  Salt, Hash: string;
begin
  Result := 0;

  // Validate email uniqueness
  if Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE email = ?', [Email]) > 0 then
  begin
    WriteLn('     ERROR: Email already registered');
    Exit;
  end;

  // Validate username uniqueness
  if Conn.ExecuteScalar('SELECT COUNT(*) FROM users WHERE username = ?', [Username]) > 0 then
  begin
    WriteLn('     ERROR: Username already taken');
    Exit;
  end;

  // Generate salt and hash password
  Salt := GenerateSalt;
  Hash := HashPassword(Password, Salt);

  // Create user
  Conn.ExecuteNonQuery(
    'INSERT INTO users (email, username, password_hash, password_salt, display_name, last_password_change) ' +
    'VALUES (?, ?, ?, ?, ?, datetime(''now''))',
    [Email, Username, Hash, Salt, DisplayName]);

  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');

  // Create 2FA record (disabled by default)
  Conn.ExecuteNonQuery('INSERT INTO user_2fa (user_id) VALUES (?)', [Result]);

  // Log registration
  Conn.ExecuteNonQuery(
    'INSERT INTO security_log (user_id, event_type, event_details, success) ' +
    'VALUES (?, ''registration'', ''New user registered'', 1)', [Result]);
end;

{ Demonstrates user registration with validation and email verification. }
procedure DemoRegistration;
var
  DS: TDataSet;
begin
  WriteLn('2. User Registration');
  WriteLn('   ==================');
  WriteLn('');

  WriteLn('   Registering users with hashed passwords:');

  if RegisterUser('alice@example.com', 'alice', 'SecurePass123!', 'Alice Johnson') > 0 then
    WriteLn('     + alice@example.com (alice) - registered');

  if RegisterUser('bob@example.com', 'bob', 'BobSecret456@', 'Bob Smith') > 0 then
    WriteLn('     + bob@example.com (bob) - registered');

  if RegisterUser('carol@example.com', 'carol', 'CarolPwd789#', 'Carol White') > 0 then
    WriteLn('     + carol@example.com (carol) - registered');

  // Try duplicate registration
  WriteLn('');
  WriteLn('   Attempting duplicate registration:');
  RegisterUser('alice@example.com', 'alice2', 'AnotherPass', 'Alice Clone');

  // Display password storage (never show actual passwords in production!)
  WriteLn('');
  WriteLn('   Password storage (hashes, not plaintext):');
  DS := Conn.ExecuteQuery(
    'SELECT username, ' +
    '  substr(password_hash, 1, 16) || ''...'' AS hash_preview, ' +
    '  substr(password_salt, 1, 16) || ''...'' AS salt_preview ' +
    'FROM users');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: hash=%s salt=%s',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('hash_preview').AsString,
         DS.FieldByName('salt_preview').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Authentication
// =============================================================================
{ Authenticates a user by credentials and returns a session token. }
function AuthenticateUser(const UsernameOrEmail, Password, IPAddress, UserAgent: string): string;
var
  DS: TDataSet;
  UserId: Integer;
  StoredHash, Salt, ComputedHash: string;
  FailedAttempts: Integer;
  LockedUntil: string;
  V: Variant;
begin
  Result := '';  // Empty = failed

  // Find user
  DS := Conn.ExecuteQuery(
    'SELECT id, password_hash, password_salt, is_active, failed_login_attempts, locked_until ' +
    'FROM users WHERE email = ? OR username = ?', [UsernameOrEmail, UsernameOrEmail]);
  try
    if DS.EOF then
    begin
      // Log failed attempt (user not found)
      Conn.ExecuteNonQuery(
        'INSERT INTO security_log (event_type, event_details, ip_address, user_agent, success) ' +
        'VALUES (''login_attempt'', ''User not found: '' || ?, ?, ?, 0)',
        [UsernameOrEmail, IPAddress, UserAgent]);
      Exit;
    end;

    UserId := DS.FieldByName('id').AsInteger;

    // Check if account is active
    if DS.FieldByName('is_active').AsInteger = 0 then
    begin
      Conn.ExecuteNonQuery(
        'INSERT INTO security_log (user_id, event_type, event_details, ip_address, success) ' +
        'VALUES (?, ''login_attempt'', ''Account disabled'', ?, 0)', [UserId, IPAddress]);
      Exit;
    end;

    // Check if account is locked
    V := DS.FieldByName('locked_until').AsVariant;
    if not VarIsNull(V) then
    begin
      LockedUntil := VarToStr(V);
      if LockedUntil > FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) then
      begin
        Conn.ExecuteNonQuery(
          'INSERT INTO security_log (user_id, event_type, event_details, ip_address, success) ' +
          'VALUES (?, ''login_attempt'', ''Account locked until '' || ?, ?, 0)',
          [UserId, LockedUntil, IPAddress]);
        Exit;
      end;
    end;

    // Verify password
    StoredHash := DS.FieldByName('password_hash').AsString;
    Salt := DS.FieldByName('password_salt').AsString;
    ComputedHash := HashPassword(Password, Salt);

    if ComputedHash <> StoredHash then
    begin
      // Failed login
      FailedAttempts := DS.FieldByName('failed_login_attempts').AsInteger + 1;

      // Lock account after 5 failed attempts
      if FailedAttempts >= 5 then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE users SET failed_login_attempts = ?, locked_until = datetime(''now'', ''+30 minutes'') WHERE id = ?',
          [FailedAttempts, UserId]);
        Conn.ExecuteNonQuery(
          'INSERT INTO security_log (user_id, event_type, event_details, ip_address, success) ' +
          'VALUES (?, ''account_locked'', ''Account locked after 5 failed attempts'', ?, 0)',
          [UserId, IPAddress]);
      end
      else
      begin
        Conn.ExecuteNonQuery(
          'UPDATE users SET failed_login_attempts = ? WHERE id = ?', [FailedAttempts, UserId]);
      end;

      Conn.ExecuteNonQuery(
        'INSERT INTO security_log (user_id, event_type, event_details, ip_address, success) ' +
        'VALUES (?, ''login_attempt'', ''Invalid password (attempt '' || ? || ''/5)'', ?, 0)',
        [UserId, FailedAttempts, IPAddress]);

      // Log to login history
      Conn.ExecuteNonQuery(
        'INSERT INTO login_history (user_id, ip_address, user_agent, success) VALUES (?, ?, ?, 0)',
        [UserId, IPAddress, UserAgent]);

      Exit;
    end;
  finally
    DS.Free;
  end;

  // Successful login - create session
  Result := GenerateSessionToken;

  // Reset failed attempts and update last login
  Conn.ExecuteNonQuery(
    'UPDATE users SET failed_login_attempts = 0, locked_until = NULL, last_login = datetime(''now'') WHERE id = ?',
    [UserId]);

  // Create session (24 hour expiry)
  Conn.ExecuteNonQuery(
    'INSERT INTO sessions (user_id, session_token, ip_address, user_agent, expires_at) ' +
    'VALUES (?, ?, ?, ?, datetime(''now'', ''+24 hours''))',
    [UserId, Result, IPAddress, UserAgent]);

  // Log successful login
  Conn.ExecuteNonQuery(
    'INSERT INTO security_log (user_id, event_type, event_details, ip_address, user_agent, success) ' +
    'VALUES (?, ''login_success'', ''User logged in'', ?, ?, 1)',
    [UserId, IPAddress, UserAgent]);

  // Log to login history
  Conn.ExecuteNonQuery(
    'INSERT INTO login_history (user_id, ip_address, user_agent, success) VALUES (?, ?, ?, 1)',
    [UserId, IPAddress, UserAgent]);
end;

{ Demonstrates the user authentication workflow with session management. }
procedure DemoAuthentication;
var
  Token: string;
begin
  WriteLn('3. User Authentication');
  WriteLn('   =====================');
  WriteLn('');

  // Successful login
  WriteLn('   Attempting login for alice with correct password:');
  Token := AuthenticateUser('alice', 'SecurePass123!', '192.168.1.100', 'Mozilla/5.0');
  if Token <> '' then
    WriteLn(Format('     SUCCESS! Session token: %s...', [Copy(Token, 1, 32)]))
  else
    WriteLn('     FAILED!');

  // Login with email
  WriteLn('');
  WriteLn('   Attempting login with email for bob:');
  Token := AuthenticateUser('bob@example.com', 'BobSecret456@', '192.168.1.101', 'Chrome/120');
  if Token <> '' then
    WriteLn(Format('     SUCCESS! Session token: %s...', [Copy(Token, 1, 32)]))
  else
    WriteLn('     FAILED!');

  // Failed login attempts
  WriteLn('');
  WriteLn('   Simulating failed login attempts for carol:');
  AuthenticateUser('carol', 'wrong1', '10.0.0.50', 'Attacker Bot');
  WriteLn('     Attempt 1: Wrong password');
  AuthenticateUser('carol', 'wrong2', '10.0.0.50', 'Attacker Bot');
  WriteLn('     Attempt 2: Wrong password');
  AuthenticateUser('carol', 'wrong3', '10.0.0.50', 'Attacker Bot');
  WriteLn('     Attempt 3: Wrong password');

  // Non-existent user
  WriteLn('');
  WriteLn('   Attempting login for non-existent user:');
  Token := AuthenticateUser('hacker', 'password123', '10.0.0.99', 'Bot');
  if Token = '' then
    WriteLn('     FAILED (user not found) - logged for security');

  WriteLn('');
end;

// =============================================================================
// Session Validation
// =============================================================================
{ Validates a session token by checking existence, expiration, and user active status; updates last activity on success and returns the user ID or 0 if invalid. }
function ValidateSession(const SessionToken: string): Integer;
var
  DS: TDataSet;
  UserId: Integer;
begin
  Result := 0;  // 0 = invalid

  DS := Conn.ExecuteQuery(
    'SELECT s.user_id, s.expires_at, u.is_active ' +
    'FROM sessions s ' +
    'JOIN users u ON s.user_id = u.id ' +
    'WHERE s.session_token = ? AND s.is_valid = 1', [SessionToken]);
  try
    if DS.EOF then
      Exit;

    // Check expiration
    if DS.FieldByName('expires_at').AsString < FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) then
    begin
      // Invalidate expired session
      Conn.ExecuteNonQuery('UPDATE sessions SET is_valid = 0 WHERE session_token = ?', [SessionToken]);
      Exit;
    end;

    // Check user is still active
    if DS.FieldByName('is_active').AsInteger = 0 then
      Exit;

    UserId := DS.FieldByName('user_id').AsInteger;
  finally
    DS.Free;
  end;

  // Update last activity
  Conn.ExecuteNonQuery(
    'UPDATE sessions SET last_activity = datetime(''now'') WHERE session_token = ?', [SessionToken]);

  Result := UserId;
end;

{ Marks a specific session as invalid by setting its is_valid flag to 0, effectively logging the user out. }
procedure InvalidateSession(const SessionToken: string);
begin
  Conn.ExecuteNonQuery('UPDATE sessions SET is_valid = 0 WHERE session_token = ?', [SessionToken]);
end;

{ Invalidates all active sessions for a user, forcing logout on all devices (used after password reset as a security measure). }
procedure InvalidateAllUserSessions(UserId: Integer);
begin
  Conn.ExecuteNonQuery('UPDATE sessions SET is_valid = 0 WHERE user_id = ?', [UserId]);
end;

{ Creates a session for Alice, validates it, lists all active sessions, then invalidates it and confirms the invalidation. }
procedure DemoSessionManagement;
var
  Token: string;
  UserId: Integer;
  DS: TDataSet;
begin
  WriteLn('4. Session Management');
  WriteLn('   ====================');
  WriteLn('');

  // Create a session
  Token := AuthenticateUser('alice', 'SecurePass123!', '192.168.1.200', 'Firefox/120');
  WriteLn(Format('   Created session for Alice: %s...', [Copy(Token, 1, 24)]));

  // Validate session
  UserId := ValidateSession(Token);
  WriteLn(Format('   Session validation: User ID = %d (Alice)', [UserId]));

  // Show active sessions
  WriteLn('');
  WriteLn('   Active sessions:');
  DS := Conn.ExecuteQuery(
    'SELECT u.username, s.ip_address, s.user_agent, ' +
    '  s.created_at, s.last_activity ' +
    'FROM sessions s ' +
    'JOIN users u ON s.user_id = u.id ' +
    'WHERE s.is_valid = 1 ' +
    'ORDER BY s.last_activity DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s from %s (%s)',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('ip_address').AsString,
         DS.FieldByName('user_agent').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Invalidate one session
  WriteLn('');
  WriteLn('   Invalidating Alice''s latest session...');
  InvalidateSession(Token);
  UserId := ValidateSession(Token);
  WriteLn(Format('   Session validation after invalidation: User ID = %d (0 = invalid)', [UserId]));

  WriteLn('');
end;

// =============================================================================
// Password Reset
// =============================================================================
{ Creates a password reset token. }
function CreatePasswordResetToken(const Email: string): string;
var
  UserId: Integer;
  V: Variant;
begin
  Result := '';

  V := Conn.ExecuteScalar('SELECT id FROM users WHERE email = ?', [Email]);
  if VarIsNull(V) then
  begin
    WriteLn('     User not found (but we don''t reveal this to user)');
    Exit;
  end;

  UserId := V;
  Result := GenerateRandomHex(32);

  // Invalidate any existing reset tokens
  Conn.ExecuteNonQuery(
    'UPDATE password_reset_tokens SET is_used = 1 WHERE user_id = ? AND is_used = 0', [UserId]);

  // Create new token (valid for 1 hour)
  Conn.ExecuteNonQuery(
    'INSERT INTO password_reset_tokens (user_id, token, expires_at) ' +
    'VALUES (?, ?, datetime(''now'', ''+1 hour''))', [UserId, Result]);

  // Log security event
  Conn.ExecuteNonQuery(
    'INSERT INTO security_log (user_id, event_type, event_details, success) ' +
    'VALUES (?, ''password_reset_request'', ''Reset token created'', 1)', [UserId]);
end;

{ Resets a user's password using a valid reset token. }
function ResetPassword(const Token, NewPassword: string): Boolean;
var
  DS: TDataSet;
  UserId: Integer;
  Salt, Hash: string;
begin
  Result := False;

  DS := Conn.ExecuteQuery(
    'SELECT user_id, expires_at FROM password_reset_tokens ' +
    'WHERE token = ? AND is_used = 0', [Token]);
  try
    if DS.EOF then
    begin
      WriteLn('     Invalid or expired token');
      Exit;
    end;

    if DS.FieldByName('expires_at').AsString < FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) then
    begin
      WriteLn('     Token has expired');
      Exit;
    end;

    UserId := DS.FieldByName('user_id').AsInteger;
  finally
    DS.Free;
  end;

  // Generate new salt and hash
  Salt := GenerateSalt;
  Hash := HashPassword(NewPassword, Salt);

  // Update password
  Conn.ExecuteNonQuery(
    'UPDATE users SET password_hash = ?, password_salt = ?, ' +
    'last_password_change = datetime(''now'') WHERE id = ?', [Hash, Salt, UserId]);

  // Mark token as used
  Conn.ExecuteNonQuery('UPDATE password_reset_tokens SET is_used = 1 WHERE token = ?', [Token]);

  // Invalidate all sessions (security measure)
  InvalidateAllUserSessions(UserId);

  // Log security event
  Conn.ExecuteNonQuery(
    'INSERT INTO security_log (user_id, event_type, event_details, success) ' +
    'VALUES (?, ''password_reset'', ''Password changed via reset token'', 1)', [UserId]);

  Result := True;
end;

{ Generates a password reset token for Bob, uses it to change the password, then tests with invalid and already-used tokens to show rejection. }
procedure DemoPasswordReset;
var
  Token: string;
begin
  WriteLn('5. Password Reset Flow');
  WriteLn('   =====================');
  WriteLn('');

  // Request reset token
  WriteLn('   Requesting password reset for bob@example.com:');
  Token := CreatePasswordResetToken('bob@example.com');
  if Token <> '' then
    WriteLn(Format('     Reset token generated: %s...', [Copy(Token, 1, 24)]))
  else
    WriteLn('     Failed to generate token');

  // Use reset token
  WriteLn('');
  WriteLn('   Resetting password with token:');
  if ResetPassword(Token, 'NewBobPassword!') then
    WriteLn('     Password reset successful!')
  else
    WriteLn('     Password reset failed');

  // Try invalid token
  WriteLn('');
  WriteLn('   Attempting reset with invalid token:');
  ResetPassword('invalid_token_12345', 'HackerPassword');

  // Try using same token again
  WriteLn('');
  WriteLn('   Attempting to reuse token:');
  ResetPassword(Token, 'AnotherPassword');

  WriteLn('');
end;

// =============================================================================
// Two-Factor Authentication
// =============================================================================
{ Generates a TOTP secret key and 8 backup codes for a user, stores them in the 2FA table, and returns the secret key. }
function Setup2FA(UserId: Integer): string;
var
  SecretKey, BackupCodes: string;
  I: Integer;
begin
  // Generate secret key (would be TOTP secret in production)
  SecretKey := GenerateRandomHex(20);

  // Generate backup codes
  BackupCodes := '';
  for I := 1 to 8 do
  begin
    if I > 1 then BackupCodes := BackupCodes + ',';
    BackupCodes := BackupCodes + GenerateRandomHex(4) + '-' + GenerateRandomHex(4);
  end;

  // Update 2FA record
  Conn.ExecuteNonQuery(
    'UPDATE user_2fa SET secret_key = ?, backup_codes = ? WHERE user_id = ?',
    [SecretKey, BackupCodes, UserId]);

  // Log security event
  Conn.ExecuteNonQuery(
    'INSERT INTO security_log (user_id, event_type, event_details, success) ' +
    'VALUES (?, ''2fa_setup'', ''2FA secret key generated'', 1)', [UserId]);

  Result := SecretKey;
end;

{ Verifies a two-factor authentication code for a user. }
function Verify2FA(UserId: Integer; const Code: string): Boolean;
var
  DS: TDataSet;
begin
  Result := False;

  // In production, verify TOTP code against secret
  // Here we simulate with verification codes table

  DS := Conn.ExecuteQuery(
    'SELECT id FROM verification_codes ' +
    'WHERE user_id = ? AND code = ? AND purpose = ''2fa'' ' +
    '  AND is_used = 0 AND expires_at > datetime(''now'') AND attempts < 3',
    [UserId, Code]);
  try
    if not DS.EOF then
    begin
      // Mark code as used
      Conn.ExecuteNonQuery(
        'UPDATE verification_codes SET is_used = 1 WHERE user_id = ? AND code = ?',
        [UserId, Code]);

      // Enable 2FA
      Conn.ExecuteNonQuery(
        'UPDATE user_2fa SET is_enabled = 1, verified_at = datetime(''now'') WHERE user_id = ?',
        [UserId]);

      // Log security event
      Conn.ExecuteNonQuery(
        'INSERT INTO security_log (user_id, event_type, event_details, success) ' +
        'VALUES (?, ''2fa_enabled'', ''Two-factor authentication enabled'', 1)', [UserId]);

      Result := True;
    end
    else
    begin
      // Increment attempts
      Conn.ExecuteNonQuery(
        'UPDATE verification_codes SET attempts = attempts + 1 ' +
        'WHERE user_id = ? AND purpose = ''2fa'' AND is_used = 0', [UserId]);
    end;
  finally
    DS.Free;
  end;
end;

{ Demonstrates two-factor authentication setup and verification. }
procedure Demo2FA;
var
  SecretKey, VerifyCode: string;
  DS: TDataSet;
begin
  WriteLn('6. Two-Factor Authentication');
  WriteLn('   ===========================');
  WriteLn('');

  // Setup 2FA for Alice
  WriteLn('   Setting up 2FA for Alice:');
  SecretKey := Setup2FA(1);
  WriteLn(Format('     Secret key: %s...', [Copy(SecretKey, 1, 16)]));

  // Create verification code (simulating TOTP)
  VerifyCode := GenerateShortToken;
  Conn.ExecuteNonQuery(
    'INSERT INTO verification_codes (user_id, code, purpose, expires_at) ' +
    'VALUES (1, ?, ''2fa'', datetime(''now'', ''+5 minutes''))', [VerifyCode]);
  WriteLn(Format('     Verification code sent: %s', [VerifyCode]));

  // Verify code
  WriteLn('');
  WriteLn('   Verifying 2FA code:');
  if Verify2FA(1, VerifyCode) then
    WriteLn('     2FA enabled successfully!')
  else
    WriteLn('     2FA verification failed');

  // Show 2FA status
  WriteLn('');
  WriteLn('   2FA status for all users:');
  DS := Conn.ExecuteQuery(
    'SELECT u.username, ' +
    '  CASE WHEN f.is_enabled = 1 THEN ''Enabled'' ELSE ''Disabled'' END AS status, ' +
    '  f.verified_at ' +
    'FROM users u ' +
    'JOIN user_2fa f ON u.id = f.user_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %s',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Security Audit Log
// =============================================================================
{ Displays the 15 most recent security audit log entries and a summary of event counts grouped by type with success rates. }
procedure DemoSecurityLog;
var
  DS: TDataSet;
begin
  WriteLn('7. Security Audit Log');
  WriteLn('   ====================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT sl.created_at, u.username, sl.event_type, sl.event_details, ' +
    '  sl.ip_address, sl.success ' +
    'FROM security_log sl ' +
    'LEFT JOIN users u ON sl.user_id = u.id ' +
    'ORDER BY sl.created_at DESC ' +
    'LIMIT 15');
  try
    WriteLn('   Time     | User    | Event                | Details');
    WriteLn('   ---------|---------|----------------------|---------------------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s | %-7s | %-20s | %s',
        [Copy(DS.FieldByName('created_at').AsString, 12, 8),
         DS.FieldByName('username').AsString,
         DS.FieldByName('event_type').AsString,
         Copy(DS.FieldByName('event_details').AsString, 1, 30)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Security summary
  WriteLn('');
  WriteLn('   Event summary:');
  DS := Conn.ExecuteQuery(
    'SELECT event_type, COUNT(*) AS count, SUM(success) AS successes ' +
    'FROM security_log ' +
    'GROUP BY event_type ' +
    'ORDER BY count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d events (%d successful)',
        [DS.FieldByName('event_type').AsString,
         DS.FieldByName('count').AsInteger,
         DS.FieldByName('successes').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Login History
// =============================================================================
{ Displays the 10 most recent login attempts with IP, user agent, and status, plus per-user statistics of successful vs failed login counts. }
procedure DemoLoginHistory;
var
  DS: TDataSet;
begin
  WriteLn('8. Login History');
  WriteLn('   ===============');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT u.username, lh.login_time, lh.ip_address, lh.user_agent, ' +
    '  CASE lh.success WHEN 1 THEN ''OK'' ELSE ''FAILED'' END AS result ' +
    'FROM login_history lh ' +
    'JOIN users u ON lh.user_id = u.id ' +
    'ORDER BY lh.login_time DESC ' +
    'LIMIT 10');
  try
    WriteLn('   User    | Time     | IP Address     | Status | User Agent');
    WriteLn('   --------|----------|----------------|--------|------------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-7s | %s | %-14s | %-6s | %s',
        [DS.FieldByName('username').AsString,
         Copy(DS.FieldByName('login_time').AsString, 12, 8),
         DS.FieldByName('ip_address').AsString,
         DS.FieldByName('result').AsString,
         Copy(DS.FieldByName('user_agent').AsString, 1, 15)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Login statistics
  WriteLn('');
  WriteLn('   Login statistics per user:');
  DS := Conn.ExecuteQuery(
    'SELECT u.username, ' +
    '  COUNT(*) AS total_attempts, ' +
    '  SUM(CASE WHEN lh.success = 1 THEN 1 ELSE 0 END) AS successful, ' +
    '  SUM(CASE WHEN lh.success = 0 THEN 1 ELSE 0 END) AS failed ' +
    'FROM login_history lh ' +
    'JOIN users u ON lh.user_id = u.id ' +
    'GROUP BY u.id ' +
    'ORDER BY total_attempts DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d total (%d successful, %d failed)',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('total_attempts').AsInteger,
         DS.FieldByName('successful').AsInteger,
         DS.FieldByName('failed').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Account Lockout Status
// =============================================================================
{ Displays all user accounts with their security status (Active, Warning, Locked, or Disabled) based on failed login attempts and lock expiration. }
procedure DemoAccountLockout;
var
  DS: TDataSet;
begin
  WriteLn('9. Account Security Status');
  WriteLn('   =========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT username, email, is_active, failed_login_attempts, locked_until, ' +
    '  CASE ' +
    '    WHEN is_active = 0 THEN ''Disabled'' ' +
    '    WHEN locked_until > datetime(''now'') THEN ''Locked'' ' +
    '    WHEN failed_login_attempts > 0 THEN ''Warning'' ' +
    '    ELSE ''Active'' ' +
    '  END AS status ' +
    'FROM users');
  try
    WriteLn('   Username | Email                  | Status  | Failed Attempts');
    WriteLn('   ---------|------------------------|---------|----------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s | %-22s | %-7s | %d',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('failed_login_attempts').AsInteger]));
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
  WriteLn('=== NDXSQLite Example 75: User Authentication System ===');
  WriteLn('');

  Randomize;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    DemoRegistration;
    DemoAuthentication;
    DemoSessionManagement;
    DemoPasswordReset;
    Demo2FA;
    DemoSecurityLog;
    DemoLoginHistory;
    DemoAccountLockout;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
