# Example 87: Email Queue

## Overview

This example demonstrates a **priority-based email queue with retry** mechanism. It implements template management, priority queuing, batch processing, exponential backoff retries, bounce handling with automatic suppression, rate limiting, and comprehensive delivery tracking.

## Features Demonstrated

### 1. Email Templates
- Named templates with placeholders
- Subject and body templates
- Category classification (transactional, marketing, system)

### 2. Priority Queue
- Priority levels (1=high, 5=normal, 9=low)
- FIFO within same priority
- Scheduled delivery times

### 3. Batch Processing
- Process N emails per batch
- Priority-ordered processing
- Suppression list checking
- Status tracking per email

### 4. Retry with Exponential Backoff
- Configurable max attempts per email
- Increasing delay between retries
- Permanent failure after max attempts

### 5. Bounce Handling
- Hard bounce (5xx) → immediate suppression
- Soft bounce (4xx) → retry
- Automatic suppression list management
- Bounce reason tracking

### 6. Suppression List
- Hard-bounced addresses auto-added
- User unsubscribe support
- Pre-send checking
- Source tracking

### 7. Rate Limiting
- Per-category limits
- Hourly window enforcement
- Global and per-type limits

### 8. Delivery Logging
- Per-attempt SMTP response
- Duration tracking
- Error message preservation

## Database Schema

```
+-------------------+     +------------------+
| email_templates   |     | email_queue      |
+-------------------+     +------------------+
| id (PK)           |     | id (PK)          |
| name (UNIQUE)     |     | template_id (FK) |
| subject_template  |     | from_address     |
| body_template     |     | to_address       |
| category          |     | subject          |
+-------------------+     | body             |
                          | priority (1-9)   |
                          | status           |
                          | attempt_count    |
                          | max_attempts     |
                          | scheduled_at     |
                          | sent_at          |
                          | next_retry_at    |
                          | error_message    |
                          | category         |
                          +------------------+
                                  |
                    +-------------+-------------+
                    |                           |
                    v                           v
          +------------------+       +-------------------+
          | email_send_log   |       | email_bounces     |
          +------------------+       +-------------------+
          | email_id (FK)    |       | email_id (FK)     |
          | attempt_number   |       | to_address        |
          | status           |       | bounce_type       |
          | smtp_response    |       | bounce_reason     |
          | duration_ms      |       +-------------------+
          +------------------+               |
                                             v
                                   +-------------------+
                                   | email_suppression |
                                   +-------------------+
                                   | email_address     |
                                   | reason            |
                                   | source            |
                                   +-------------------+

+-------------------+
| email_rate_limits |
+-------------------+
| window_start      |
| window_end        |
| emails_sent       |
| max_per_window    |
| category          |
+-------------------+
```

## Email Status Flow

```
[queued] ──────────────────────────────────────────┐
    |                                              |
    v                                              v
Check suppression list                      [suppressed]
    |                                       (skip send)
    v
[sending]
    |
    +──── Success (250) ──────> [sent]
    |
    +──── Hard bounce (5xx) ──> [bounced]
    |                               |
    |                               v
    |                         Add to suppression
    |
    +──── Soft bounce (4xx) ──> [retry_pending]
                                    |
                                    v
                              Wait (backoff)
                                    |
                              +─────+─────+
                              |           |
                              v           v
                          [sent]    attempts >= max?
                                        |
                                        v
                                    [failed]
```

## Key Patterns

### Queue with Priority
```pascal
Conn.ExecuteNonQuery(
  'INSERT INTO email_queue (from_address, to_address, subject, body, priority, scheduled_at) ' +
  'VALUES (?, ?, ?, ?, ?, ?)',
  ['noreply@app.com', 'user@email.com', 'Reset Password',
   'Click here...', 1, datetime('now')]);  // priority 1 = high
```

### Process by Priority
```pascal
DS := Conn.ExecuteQuery(
  'SELECT * FROM email_queue ' +
  'WHERE status = ''queued'' AND scheduled_at <= datetime(''now'') ' +
  'ORDER BY priority ASC, created_at ASC ' +
  'LIMIT 50');  // batch size
```

### Exponential Backoff
```pascal
// Retry delay: 2^attempt minutes
next_retry := datetime('now', '+' || (power(2, attempt_count)) || ' minutes');
UPDATE email_queue SET status = 'retry_pending',
  next_retry_at = next_retry WHERE id = ?;
```

### Bounce → Suppression
```pascal
if ResponseCode >= 500 then  // Hard bounce
begin
  UPDATE email_queue SET status = 'bounced';
  INSERT INTO email_bounces (to_address, bounce_type, bounce_reason) ...;
  INSERT INTO email_suppression (email_address, reason, source)
    VALUES (to_address, 'hard_bounce', 'automatic');
end;
```

### Pre-Send Suppression Check
```pascal
if Conn.ExecuteScalar(
  'SELECT COUNT(*) FROM email_suppression WHERE email_address = ?',
  [ToAddress]) > 0 then
begin
  UPDATE email_queue SET status = 'suppressed';
  Skip;  // Don't send
end;
```

## Compilation

```bash
cd 87_EmailQueue
lazbuild EmailQueue.lpi
./EmailQueue
```

## Sample Output

```
3. Queuing Emails
   Queued 19 emails:
     High priority (1):   5
     Normal priority (5): 6
     Low priority (9):    8

4. Processing Email Queue
   Batch processed: 19 emails
     Sent:       12
     Failed:     5
     Suppressed: 2

5. Retrying Failed Emails
   Retried: 2 emails
     Now sent:     1
     Still failing: 1

8. Queue Status Overview
   sent:          13 (68.4%)
   bounced:        3 (15.8%)
   suppressed:     2 (10.5%)

10. Email Queue Statistics
    Bounce rate:  15.8%
    Avg duration: 573 ms
```

## Related Examples

- **86_WebhookStorage** - Webhook delivery with retry
- **88_APIRateLimiting** - Rate limiting patterns
- **78_NotificationSystem** - Event notifications

## Best Practices

1. **Priority**: Use 1 for password resets/alerts, 5 for orders, 9 for newsletters
2. **Backoff**: Double delay each retry (1min, 2min, 4min, 8min...)
3. **Hard bounces**: Immediately suppress — never retry
4. **Soft bounces**: Retry up to max, then suppress
5. **Rate limiting**: Separate limits for transactional vs marketing
6. **Batch size**: Process 50-100 per batch to manage memory
7. **Monitoring**: Alert on high bounce rates (>5%)
8. **Retention**: Keep sent logs for 30 days, failed for 90 days
