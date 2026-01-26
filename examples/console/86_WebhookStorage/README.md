# Example 86: Webhook Storage

## Overview

This example demonstrates **webhook storage and replay** patterns for reliable event delivery. It implements endpoint configuration, event routing, delivery tracking with retry logic, dead letter queues, signature verification, and retention policies — essential for building reliable webhook infrastructure.

## Features Demonstrated

### 1. Endpoint Configuration
- Named webhook subscribers with URLs
- Secret keys for signature verification
- Event type filtering per endpoint
- Active/inactive status control
- Configurable max retries and timeout

### 2. Event Reception
- Incoming webhook storage
- Source tracking
- Payload preservation
- Signature recording
- Timestamp management

### 3. Event Routing
- Match events to subscribed endpoints
- Skip inactive endpoints
- Create delivery tasks per match
- Mark events as processed

### 4. Delivery Tracking
- Attempt counting with max limit
- Status progression (pending → delivered / retry_pending → dead_letter)
- Response code recording
- Error message preservation
- Next retry scheduling

### 5. Retry Mechanism
- Exponential backoff (simulated)
- Max attempts enforcement
- Recovery detection
- Dead letter escalation

### 6. Webhook Replay
- Re-deliver specific event types
- Target all matching endpoints
- Create new delivery tasks
- Historical event access

### 7. Signature Verification
- Hash-based signature computation
- Stored vs computed comparison
- Valid/invalid detection

### 8. Dead Letter Queue
- Permanently failed deliveries
- Full context preservation (event + endpoint + attempts)
- Failure reason documentation
- Manual inspection and replay

## Database Schema

```
+--------------------+     +------------------+
| webhook_endpoints  |     | webhook_events   |
+--------------------+     +------------------+
| id (PK)            |     | id (PK)          |
| name               |     | event_type       |
| url                |     | source           |
| secret             |     | payload          |
| event_types        |     | signature        |
| is_active          |     | received_at      |
| max_retries        |     | processed        |
| timeout_ms         |     | processed_at     |
+--------------------+     +------------------+
         |                          |
         v                          v
+------------------------+
| webhook_deliveries     |
+------------------------+
| id (PK)                |
| event_id (FK)          |-------> webhook_events
| endpoint_id (FK)       |-------> webhook_endpoints
| status                 |
| attempt_count          |
| max_attempts           |
| last_attempt_at        |
| next_retry_at          |
| response_code          |
| error_message          |
| delivered_at           |
+------------------------+
         |
    +----+----+
    |         |
    v         v
+------------------+    +---------------------+
| delivery_log     |    | webhook_dead_letters|
+------------------+    +---------------------+
| delivery_id      |    | delivery_id         |
| attempt_number   |    | event_id            |
| status           |    | endpoint_id         |
| response_code    |    | event_type          |
| error_message    |    | payload             |
| duration_ms      |    | failure_reason      |
| attempted_at     |    | total_attempts      |
+------------------+    +---------------------+
```

## Delivery Status Flow

```
  [pending]
      |
      v
  Attempt delivery
      |
  +---+---+
  |       |
  v       v
[delivered]  [retry_pending]
              |
              v
         Wait for next_retry_at
              |
              v
         Retry delivery
              |
          +---+---+
          |       |
          v       v
    [delivered]  attempts >= max?
                      |
                  +---+---+
                  |       |
                  v       v
            [retry_pending] [dead_letter]
            (continue...)   (permanent failure)
```

## Key Patterns

### Endpoint Registration
```pascal
Conn.ExecuteNonQuery(
  'INSERT INTO webhook_endpoints (name, url, secret, event_types, max_retries) ' +
  'VALUES (?, ?, ?, ?, ?)',
  ['Order Service', 'https://orders.example.com/webhooks',
   'secret_key', 'order.created,order.updated', 5]);
```

### Event Routing
```pascal
// Find matching endpoints for event type
DS := Conn.ExecuteQuery(
  'SELECT id FROM webhook_endpoints ' +
  'WHERE is_active = 1 AND event_types LIKE ?',
  ['%' + EventType + '%']);
// Create delivery task for each
```

### Retry with Dead Letter
```pascal
if AttemptCount >= MaxAttempts then
begin
  // Move to dead letter queue
  UPDATE deliveries SET status = 'dead_letter';
  INSERT INTO dead_letters (delivery_id, event_type, payload, failure_reason, ...)
end
else
begin
  // Schedule retry
  UPDATE deliveries SET status = 'retry_pending',
    next_retry_at = datetime('now', '+' || (attempt * 60) || ' seconds');
end;
```

### Webhook Replay
```pascal
// Replay all events of a type
Conn.ExecuteNonQuery(
  'INSERT INTO webhook_deliveries (event_id, endpoint_id, status, max_attempts) ' +
  'SELECT ?, e.id, ''pending'', e.max_retries ' +
  'FROM webhook_endpoints e ' +
  'WHERE e.is_active = 1 AND e.event_types LIKE ?',
  [EventId, '%order.created%']);
```

## Compilation

```bash
cd 86_WebhookStorage
lazbuild WebhookStorage.lpi
./WebhookStorage
```

## Sample Output

```
4. Routing Events to Endpoints
   Created 13 delivery tasks:
     Analytics Service: 4 deliveries
     Payment Gateway: 3 deliveries
     Order Service: 3 deliveries
     Notification Service: 3 deliveries

5. Simulating Delivery Attempts
   Delivery results:
     delivered: 8
     retry_pending: 5

6. Retrying Failed Deliveries
   Retried 5 deliveries:
     Recovered (now delivered): 2
     Moved to dead letter:      2

9. Dead Letter Queue
   1 | order.created | Payment Gateway  | 3 | Internal Server Error
   2 | order.created | Analytics Service| 3 | Connection timeout

12. Webhook Statistics
    Delivery statistics:
      delivered:   10 (58.8%)
      pending:      4 (23.5%)
      dead_letter:  2 (11.8%)
```

## Related Examples

- **87_EmailQueue** - Email queue with retry
- **88_APIRateLimiting** - Rate limiting
- **78_NotificationSystem** - Event notifications

## Best Practices

1. **Idempotency**: Store event IDs to prevent duplicate processing
2. **Exponential backoff**: Increase delay between retries (1s, 2s, 4s, 8s...)
3. **Dead letter monitoring**: Alert on dead letter queue growth
4. **Signature verification**: Always verify before processing
5. **Payload size limits**: Enforce max payload size
6. **Retention**: Clean old delivered events regularly
7. **Replay safety**: Ensure replay handlers are idempotent
