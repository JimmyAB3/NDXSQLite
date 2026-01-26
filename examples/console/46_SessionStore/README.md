# Example 46: Session Store Database

This example demonstrates how to implement web session management using SQLite with NDXSQLite.

## Features Demonstrated

- **Session Creation**: Generate secure session IDs
- **Session Validation**: Check session validity and expiration
- **Key-Value Storage**: Store arbitrary session data
- **Expiration Handling**: Automatic session timeout
- **Security Features**: IP binding and user agent tracking
- **Activity Tracking**: Log all session events
- **Session Extension**: Sliding expiration support
- **Bulk Operations**: Invalidate all user sessions

## Database Schema

### Tables

1. **sessions**: Core session storage
   - Unique session ID (GUID-based)
   - User association
   - Expiration timestamp
   - IP address and user agent binding
   - Validity flag

2. **session_data**: Key-value storage per session
   - Arbitrary data storage
   - Cascade delete with session

3. **session_activity**: Audit log
   - Create, access, invalidate, expire events
   - IP address tracking

## Key Operations

### Create Session
```pascal
SessionId := CreateSession('user123', '192.168.1.100',
  'Mozilla/5.0...', 3600);  // 1 hour lifetime
```

### Validate Session
```pascal
if ValidateSession(SessionId, ClientIP) then
  // Session is valid
else
  // Session expired, invalid, or IP mismatch
```

### Store/Retrieve Session Data
```pascal
SetSessionData(SessionId, 'cart', '["item1", "item2"]');
SetSessionData(SessionId, 'language', 'en-US');

CartData := GetSessionData(SessionId, 'cart', '[]');
```

### Session Security
```pascal
// Invalidate single session (logout)
InvalidateSession(SessionId);

// Invalidate all user sessions (logout everywhere)
InvalidateUserSessions('user123');
```

### Extend Session (Sliding Expiration)
```pascal
ExtendSession(SessionId, 1800);  // Add 30 minutes
```

## Security Features

### IP Binding
Sessions can be bound to an IP address. If a validation request comes from a different IP, it's flagged as a potential hijacking attempt.

### User Agent Tracking
The original user agent is stored for forensic analysis.

### Activity Logging
All session events are logged with timestamps and IP addresses for security audits.

## Build and Run

```bash
cd 46_SessionStore
fpc SessionStore.lpr
./SessionStore
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Use Cases

- Web application sessions
- API token management
- Shopping cart persistence
- User preference storage
- Authentication state management
- Multi-device session control
