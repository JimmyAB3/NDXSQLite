# Example 117: Outbox Pattern - Transactional Outbox for Reliable Event Publishing

## Overview

This example demonstrates the **Transactional Outbox Pattern** for reliable event publishing. Business data and events are written atomically in the same database transaction, ensuring no events are lost even if the messaging system is unavailable. A polling consumer reads pending events and delivers them to downstream services with retry logic, delivery tracking, and consumer checkpoints.

## Features Demonstrated

### 1. Atomic Write (Business Data + Event)
- Order creation and outbox event in the same transaction
- If either fails, both are rolled back (no orphan events)
- Events capture the full state change as JSON payload

### 2. Polling Consumer
- Batch polling of pending/retry events
- Ordered processing (FIFO by event ID)
- Consumer checkpoints for resumable processing
- Empty poll detection (no busy-waiting)

### 3. Failure Handling and Retries
- Configurable max retries per event (default: 3)
- Retry status for recoverable failures
- Permanent failure after exhausting retries
- Error message tracking for debugging

### 4. Delivery Tracking
- Per-attempt delivery records
- Status tracking (delivered/failed)
- Response logging for diagnostics
- Complete audit trail of delivery attempts

### 5. Multiple Destinations
- Fan-out delivery to multiple services
- Independent retry per destination
- All-or-nothing event completion

### 6. Consumer Checkpoints
- Track last processed event ID
- Event count per consumer
- Enables consumer restart without reprocessing

### 7. Outbox Cleanup
- Remove old delivered events and delivery records
- Configurable retention (by age or aggregate)
- Maintains table performance over time

### 8. Statistics
- Events by status and type breakdown
- Total delivery attempts
- Failed event count

## Architecture

```
+------------------+     +------------------+     +------------------+
| Business Logic   |     | Outbox Table     |     | Polling Consumer |
+------------------+     +------------------+     +------------------+
| CreateOrder()    |---->| event_id         |<----| PollOutboxEvents |
| UpdateOrder()    |     | event_type       |     | DeliverEvent()   |
| (in transaction) |     | payload (JSON)   |     | MarkDelivered()  |
+------------------+     | status: pending  |     | MarkFailed()     |
        |                 +------------------+     +------------------+
        |                         |                         |
        v                         v                         v
+------------------+     +------------------+     +------------------+
| orders table     |     | Event States     |     | Destinations     |
+------------------+     +------------------+     +------------------+
| order_id         |     | pending          |     | notification-svc |
| customer_id      |     | retry            |     | analytics-svc    |
| amount, status   |     | delivered        |     | inventory-svc    |
+------------------+     | failed           |     | billing-svc      |
                          +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| orders           |     | outbox_events    |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| order_id (UNI)   |     | event_id (UNI)   |
| customer_id      |     | aggregate_type   |
| amount           |     | aggregate_id     |
| status           |     | event_type       |
| created_at       |     | payload (JSON)   |
+------------------+     | status           |
                          | retry_count      |
+------------------+      | max_retries      |
| event_deliveries |      | created_at       |
+------------------+      | processed_at     |
| id (PK, AUTO)    |      | delivered_at     |
| event_id (FK)    |      | error_message    |
| destination      |      +------------------+
| status           |
| attempt          |     +------------------+
| response         |     | consumer         |
| attempted_at     |     | _checkpoints     |
+------------------+     +------------------+
                          | consumer_id (PK) |
                          | last_event_id    |
                          | last_poll_at     |
                          | events_processed |
                          +------------------+
```

## Event Lifecycle

```
pending --> (poll) --> delivering --> delivered
    |                      |
    v                      v
  retry  <------------- failed
    |                      |
    v                      v
  pending (re-poll)    failed (permanent)
                       (max retries exceeded)
```

## Compilation

```bash
cd 117_OutboxPattern
lazbuild OutboxPattern.lpi
./OutboxPattern
```

## Sample Output

```
1. Atomic Write
   Created 5 orders with outbox events (atomic transactions)
   Updated 4 order statuses (4 more outbox events)

2. Polling Consumer
   Poll batch 1 (size=5): Processed 5 events
   Poll batch 2 (size=5): Processed 4 events
   Poll batch 3: 0 events remaining

3. Failure Handling
   Attempt 1: NO (Connection timeout) -> retry 1/3
   Attempt 2: NO (Service unavailable) -> retry 2/3
   Attempt 3: YES -> delivered

7. Cleanup
   Delivered events before: 11
   Cleaned up: 8 old events
```

## Related Examples

- **118_SagaPattern** - Long-running transactions with compensating actions
- **119_IdempotencyKeys** - Exactly-once processing with deduplication

## Best Practices

1. **Atomic writes**: Always write business data and outbox event in the same transaction
2. **Idempotent consumers**: Downstream services should handle duplicate deliveries gracefully
3. **Ordered processing**: Process events in order (by ID) to maintain causality
4. **Bounded retries**: Set max retries to prevent infinite retry loops
5. **Cleanup policy**: Regularly purge delivered events to maintain table performance
6. **Consumer checkpoints**: Track progress for resumable processing after restarts
7. **Fan-out carefully**: When delivering to multiple destinations, track each independently
8. **Error context**: Store error messages for debugging failed deliveries
