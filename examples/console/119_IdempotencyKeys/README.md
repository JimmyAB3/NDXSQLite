# Example 119: Idempotency Keys - Exactly-Once Processing, Deduplication, TTL, Replay Detection

## Overview

This example demonstrates an **Idempotency Key** system for exactly-once request processing. It prevents duplicate operations from network retries or client bugs by storing a fingerprint of each unique request. Duplicate requests return the cached response without re-executing business logic. The system includes TTL expiration, hash-based conflict detection, concurrent request blocking, and comprehensive request logging.

## Features Demonstrated

### 1. First Request (New Processing)
- Idempotency key stored with request hash
- Business logic executes (order created)
- Response cached for future duplicates
- HTTP 201 returned

### 2. Duplicate Request (Replay Detection)
- Same key + same body = replay
- Returns cached response (no re-execution)
- Order NOT created again (exactly-once guarantee)
- Same HTTP status as original

### 3. Different Key, Same Payload
- Different idempotency key = new request
- Business logic executes again (new order)
- Keys are independent of payload content

### 4. Key Conflict (Same Key, Different Body)
- Same key but different request hash
- Returns HTTP 409 Conflict
- Prevents accidental reuse of keys for different operations
- No business logic executed

### 5. TTL Expiration
- Keys expire after configurable TTL (default: 24h)
- Expired keys can be reused for new requests
- String comparison of ISO timestamps for expiration check
- Old key cleaned up before re-insertion

### 6. Concurrent Request Detection
- Pending key blocks duplicate attempts
- Returns HTTP 409 while first request is processing
- Prevents double-execution in concurrent scenarios

### 7. Batch Processing with Idempotency
- Multiple unique requests processed normally
- Retry storm (replaying all requests) returns cached responses
- Total orders = number of unique requests, not total attempts

### 8. Request Log
- Every attempt logged (new, replay, conflict)
- Tracks: key, method, path, replay flag, conflict flag, result source
- Provides audit trail and metrics

### 9. Cleanup Expired Keys
- Remove keys past their TTL
- Maintains table performance
- Preserves pending keys (still processing)

### 10. Statistics
- Keys by status (completed, pending, expired)
- Request outcomes (new, cache, conflict, pending_duplicate)
- Replay and conflict rates

## Architecture

```
+------------------+     +------------------+     +------------------+
| Client Request   |     | Idempotency Check|     | Business Logic   |
+------------------+     +------------------+     +------------------+
| Key + Method     |---->| Key exists?      |     | ProcessOrder()   |
| Path + Body      |     | Hash match?      |     | (only if new)    |
|                  |     | Expired?         |     |                  |
+------------------+     | Pending?         |     +------------------+
                          +------------------+
                                  |
              +-------------------+-------------------+
              |                   |                   |
              v                   v                   v
      +-------------+    +-------------+    +-------------+
      | New Request |    | Replay      |    | Conflict    |
      +-------------+    +-------------+    +-------------+
      | Store key   |    | Return cache|    | HTTP 409    |
      | Execute biz |    | No execution|    | No execution|
      | Cache result|    | Log replay  |    | Log conflict|
      | HTTP 201    |    | Same HTTP   |    |             |
      +-------------+    +-------------+    +-------------+
```

## Database Schema

```
+------------------+     +------------------+
| idempotency_keys |     | request_log      |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| idempotency_key  |     | idempotency_key  |
| request_path     |     | request_path     |
| request_method   |     | request_method   |
| request_hash     |     | request_body     |
| response_data    |     | is_replay        |
| http_status      |     | is_conflict      |
| status           |     | result_source    |
| ttl_seconds      |     | created_at       |
| created_at       |     +------------------+
| expires_at       |
| completed_at     |     +------------------+
+------------------+     | processed_orders |
                          +------------------+
                          | id (PK, AUTO)    |
                          | order_id         |
                          | customer_id      |
                          | amount           |
                          | idempotency_key  |
                          | created_at       |
                          +------------------+
```

## Key States

```
(new request) --> pending --> completed --> expired (after TTL)
                                              |
                                              v
                                         (deleted/reusable)
```

## Request Outcomes

| Scenario | Key Exists | Hash Match | Status | Result |
|----------|-----------|------------|--------|--------|
| New request | No | N/A | N/A | Process + cache |
| Replay | Yes | Yes | completed | Return cache |
| Conflict | Yes | No | any | HTTP 409 |
| Concurrent | Yes | Yes | pending | HTTP 409 |
| Expired | Yes | Yes | expired | Re-process |

## Compilation

```bash
cd 119_IdempotencyKeys
lazbuild IdempotencyKeys.lpi
./IdempotencyKeys
```

## Sample Output

```
1. First Request: new, HTTP 201
2. Duplicate: replay (no re-execution), HTTP 201, orders=1
3. Different Key: new, HTTP 201, orders=2
4. Conflict: HTTP 409 (different body)
5. TTL Expiration: re-processes after expiry
6. Concurrent: HTTP 409 (still processing)
7. Batch: 5 new + 5 replays = 5 orders total
10. Statistics: Replay rate 41.2%, Conflict rate 5.9%
```

## Related Examples

- **117_OutboxPattern** - Transactional outbox for reliable event publishing
- **118_SagaPattern** - Long-running transactions with compensating actions

## Best Practices

1. **Client-generated keys**: Let clients generate idempotency keys (UUID v4) for full control
2. **Hash the full request**: Include method, path, and body in the fingerprint
3. **Detect conflicts**: Same key with different body = error, not silent override
4. **Block concurrency**: Pending status prevents double-execution from parallel retries
5. **TTL policy**: Keys should expire (24h default) to prevent unbounded growth
6. **Cleanup regularly**: Purge expired keys to maintain query performance
7. **Log all attempts**: Track replays and conflicts for debugging and metrics
8. **Idempotent by design**: Even without keys, business logic should be as idempotent as possible
