# Example 142: Pub/Sub Database - Publish/Subscribe via Polling

## Overview

This example demonstrates a **Publish/Subscribe messaging system** implemented entirely with SQLite tables. It simulates channels, subscriptions, message ordering, acknowledgment tracking, dead letter queuing, content-based filtering, and subscription lag monitoring - all using database polling rather than real-time push.

## Features Demonstrated

### 1. Channels and Subscriptions
- 4 channels: orders, payments, notifications, logs
- 5 subscribers with varying subscription patterns
- Content-based filter support (LIKE patterns)
- Configurable retention per channel

### 2. Message Publishing
- Sequence-numbered messages per channel
- JSON payloads with structured content
- Priority levels (0=normal, 1=urgent)
- Publisher attribution and expiry timestamps

### 3. Message Ordering (FIFO with Priority)
- Priority-first ordering within a channel
- Sequence-based FIFO within same priority level
- Urgent messages (priority 1) delivered before normal

### 4. Subscriber Polling
- Polling via last_ack_seq comparison
- Unread message detection per subscriber
- Gap-free delivery through sequence tracking

### 5. Message Acknowledgment
- Delivery status tracking: pending, delivered, acknowledged
- Per-subscriber delivery timestamps
- Acknowledgment timestamp recording

### 6. Dead Letter Queue
- Failed messages moved to dead_letters table
- Reasons: max_retries_exceeded, message_expired
- Retry count tracking
- Audit trail for failed deliveries

### 7. Fan-Out Delivery
- Single message delivered to all channel subscribers
- Delivery ratio tracking (messages x subscribers)
- Independent acknowledgment per subscriber

### 8. Content-Based Filtering
- LIKE-based payload pattern matching
- Subscriber-specific filters (e.g., "%urgent%")
- Reduction statistics (filtered vs eligible)

### 9. Subscription Lag Monitoring
- Per-subscriber lag calculation (total - acknowledged)
- Lag-sorted view for identifying slow consumers
- Attention indicators for subscribers falling behind

### 10. Channel Statistics
- Overall message/delivery/dead-letter counts
- Throughput by publisher
- Success rate by subscriber (acked/total)

## Architecture

```
+------------------+     +------------------+     +------------------+
| Publishers       |     | Channels         |     | Subscribers      |
| (api-server,     |---->| (orders,         |<----| (order-processor,|
|  payment-svc,    |     |  payments,       |     |  analytics,      |
|  sys, ...)       |     |  notifications,  |     |  email-service)  |
+------------------+     |  logs)           |     +------------------+
                          +------------------+              |
                                   |                        v
                          +------------------+     +------------------+
                          | messages         |     | deliveries       |
                          | (seq, payload,   |     | (status, ack'd,  |
                          |  priority, ttl)  |     |  retry_count)    |
                          +------------------+     +------------------+
                                                            |
                                                            v
                                                   +------------------+
                                                   | dead_letters     |
                                                   | (reason, retries)|
                                                   +------------------+
```

## Polling vs Push

```
Publisher                Channel Table              Subscriber
   |                         |                         |
   |-- INSERT message ------>|                         |
   |                         |                         |
   |                         |<-- SELECT WHERE         |
   |                         |    seq > last_ack ------|
   |                         |                         |
   |                         |--- Return rows -------->|
   |                         |                         |
   |                         |<-- UPDATE last_ack -----|
   |                         |    (acknowledge)        |
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| channels         |     | messages         |     | subscribers      |
+------------------+     +------------------+     +------------------+
| id (PK)          |     | id (PK)          |     | id (PK)          |
| name (UNIQUE)    |     | channel_id (FK)  |     | name (UNIQUE)    |
| description      |     | sequence_num     |     | filter_pattern   |
| max_retention_sec|     | payload          |     | created_at       |
| created_at       |     | priority         |     +------------------+
+------------------+     | published_at     |
                          | expires_at       |     +------------------+
+------------------+     | publisher        |     | subscriptions    |
| deliveries       |     +------------------+     +------------------+
+------------------+                               | subscriber_id(PK)|
| id (PK)          |     +------------------+     | channel_id (PK)  |
| message_id (FK)  |     | dead_letters     |     | last_ack_seq     |
| subscriber_id(FK)|     +------------------+     | subscribed_at    |
| status           |     | id (PK)          |     | status           |
| delivered_at     |     | message_id       |     +------------------+
| acknowledged_at  |     | subscriber_id    |
| retry_count      |     | channel_name     |
+------------------+     | reason           |
                          | retry_count      |
                          | original_payload |
                          | dead_at          |
                          +------------------+
```

## Compilation

```bash
cd 142_PubSubDatabase
lazbuild PubSubDatabase.lpi
./PubSubDatabase
```

## Sample Output

```
1. Channels: 4 channels with 2-4 subscribers each
2. Published: 22 messages across 4 channels
3. Ordering: Priority 1 messages delivered first
4. Polling: 2 undelivered messages for order-processor
5. Acknowledgment: delivered/acknowledged status tracking
6. Dead letters: 3 messages (expired + max retries)
7. Fan-out: 4 subscribers receive message #3
8. Filtering: 2/11 messages match "%urgent%" (82% reduction)
9. Lag: subscribers ordered by processing delay
10. Stats: 100% to 50% success rate by subscriber
```

## Related Examples

- **141_OptimisticConcurrencyAdvanced** - MVCC with version vectors
- **143_DistributedLock** - Lock manager with deadlock detection

## Best Practices

1. **Sequence numbers over IDs**: Sequences guarantee ordering within a channel
2. **Priority queuing**: Use priority + sequence for delivery order
3. **Acknowledgment tracking**: Never delete messages until all subscribers ack
4. **Dead letter queue**: Preserve failed messages for debugging/replay
5. **Content filtering**: Push filtering to SQL to reduce polling overhead
6. **Lag monitoring**: Alert when subscribers fall too far behind
7. **Message expiry**: TTL prevents unbounded storage growth
8. **Fan-out tracking**: Independent delivery status per subscriber
