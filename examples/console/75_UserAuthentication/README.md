# Example 75: User Authentication System

## Overview

This example demonstrates a **complete user authentication system** with secure password storage, session management, account lockout protection, password reset flows, and two-factor authentication. It showcases security best practices essential for production applications.

## Features Demonstrated

### 1. Secure Password Storage
- Password hashing with salt (using MD5 with key stretching)
- Unique salt per user prevents rainbow table attacks
- Hash verification without storing plaintext passwords

### 2. Session Management
- Session token generation (128-character hex tokens)
- Session validation with expiration checking
- Session invalidation (logout)
- Multiple active sessions per user
- Session activity tracking

### 3. Account Security
- Failed login attempt tracking
- Automatic account lockout after 5 failed attempts
- 30-minute lockout duration
- Account activation/deactivation

### 4. Password Reset Flow
- Secure token generation for reset links
- Token expiration (1 hour)
- Single-use tokens (marked used after reset)
- All sessions invalidated after password change

### 5. Two-Factor Authentication (2FA)
- Secret key generation
- Verification code management
- Backup codes for account recovery
- Enable/disable 2FA per user

### 6. Security Audit Logging
- All security events logged
- IP address and user agent tracking
- Success/failure status for each event
- Comprehensive login history

## Database Schema

```
+------------+     +-------------+     +------------------+
| users      |<--->| sessions    |     | password_reset   |
+------------+     +-------------+     | _tokens          |
| id         |     | user_id     |     +------------------+
| email      |     | session_token|    | user_id          |
| username   |     | ip_address  |     | token            |
| pass_hash  |     | user_agent  |     | is_used          |
| pass_salt  |     | expires_at  |     | expires_at       |
| locked_until|    | last_activity|    +------------------+
+------------+     +-------------+
      |                  |
      |    +-------------+-----------+
      |    |                         |
      v    v                         v
+-----------+     +------------------+     +---------------+
| user_2fa  |     | verification_    |     | login_history |
+-----------+     | codes            |     +---------------+
| user_id   |     +------------------+     | user_id       |
| is_enabled|     | user_id          |     | login_time    |
| secret_key|     | code             |     | ip_address    |
| backup_codes|   | purpose          |     | user_agent    |
+-----------+     | expires_at       |     | success       |
                  +------------------+     +---------------+
                         |
                         v
                  +------------------+
                  | security_log     |
                  +------------------+
                  | user_id          |
                  | event_type       |
                  | event_details    |
                  | ip_address       |
                  | success          |
                  +------------------+
```

## Key Patterns

### Password Hashing with Salt
```pascal
function HashPassword(const Password, Salt: string): string;
var
  Salted: string;
begin
  Salted := Salt + Password + Salt;
  Result := MD5Print(MD5String(Salted));
  // Key stretching with multiple rounds
  Result := MD5Print(MD5String(Result + Salt));
  Result := MD5Print(MD5String(Salt + Result));
end;
```

### Account Lockout
```sql
-- After 5 failed attempts, lock for 30 minutes
UPDATE users SET
  failed_login_attempts = ?,
  locked_until = datetime('now', '+30 minutes')
WHERE id = ?;

-- Check if account is locked
SELECT * FROM users
WHERE locked_until > datetime('now');
```

### Session Validation
```sql
-- Validate session token
SELECT s.user_id, s.expires_at, u.is_active
FROM sessions s
JOIN users u ON s.user_id = u.id
WHERE s.session_token = ?
  AND s.is_valid = 1
  AND s.expires_at > datetime('now');
```

### Security Audit Trail
```sql
-- Log all security events
INSERT INTO security_log
  (user_id, event_type, event_details, ip_address, success)
VALUES (?, ?, ?, ?, ?);
```

## Demonstration Sections

1. **Schema Creation** - Authentication database structure
2. **User Registration** - Secure user creation with hashed passwords
3. **Authentication** - Login with password verification
4. **Session Management** - Token-based sessions
5. **Password Reset** - Secure reset token flow
6. **Two-Factor Authentication** - 2FA setup and verification
7. **Security Audit Log** - Review security events
8. **Login History** - Track login attempts
9. **Account Security Status** - View lockout states

## Security Features Implemented

| Feature | Implementation |
|---------|----------------|
| Password storage | Salted hash, never plaintext |
| Brute force protection | Account lockout after 5 attempts |
| Session security | Long random tokens, expiration |
| Password reset | Single-use, time-limited tokens |
| 2FA support | TOTP-compatible secret keys |
| Audit trail | All events logged with details |

## Compilation

```bash
cd 75_UserAuthentication
lazbuild UserAuthentication.lpi
./UserAuthentication
```

## Sample Output

```
2. User Registration
   ==================

   Registering users with hashed passwords:
     + alice@example.com (alice) - registered
     + bob@example.com (bob) - registered

   Password storage (hashes, not plaintext):
     alice: hash=929d9362be041e98... salt=B0E5537988AFFE76...
     bob: hash=53338062e76d139f... salt=8D902B93D9825950...

3. User Authentication
   =====================

   Attempting login for alice with correct password:
     SUCCESS! Session token: AFD92EAE7A26DF0D1B8A22FB42064951...

   Simulating failed login attempts for carol:
     Attempt 1: Wrong password
     Attempt 2: Wrong password
     Attempt 3: Wrong password

9. Account Security Status
   =========================

   Username | Email                  | Status  | Failed Attempts
   ---------|------------------------|---------|----------------
   alice    | alice@example.com      | Active  | 0
   carol    | carol@example.com      | Warning | 3
```

## Related Examples

- **74_ShoppingCart** - Session-based cart management
- **76_TaggingSystem** - User-generated content organization
- **78_NotificationSystem** - User notification tracking
- **13_Transactions** - Transaction handling

## Production Considerations

1. **Password Hashing**: Use bcrypt, scrypt, or Argon2 instead of MD5
2. **Token Storage**: Store session tokens hashed, not plaintext
3. **HTTPS**: Always use TLS for authentication endpoints
4. **Rate Limiting**: Add request rate limiting at the API level
5. **CSRF Protection**: Include CSRF tokens for form submissions
6. **Secure Cookies**: Use HttpOnly, Secure, SameSite attributes
7. **Password Policy**: Enforce minimum length and complexity

## Event Types Logged

| Event Type | Description |
|------------|-------------|
| registration | New user account created |
| login_success | Successful authentication |
| login_attempt | Failed login attempt |
| account_locked | Account locked due to failures |
| password_reset_request | Reset token generated |
| password_reset | Password successfully changed |
| 2fa_setup | 2FA secret key generated |
| 2fa_enabled | 2FA successfully enabled |
