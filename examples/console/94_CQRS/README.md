# Example 94: CQRS - Command Query Responsibility Segregation

## Overview

This example demonstrates the **CQRS pattern** with event sourcing using SQLite. It separates the write side (commands producing events) from the read side (projections built from events). Features include an append-only event store, command logging, multiple read projections, event replay for rebuilding models, aggregate history, and failed command handling.

## Features Demonstrated

### 1. Event Store (Write Side)
- Append-only event log
- Aggregate versioning (optimistic concurrency)
- Event types with structured data
- Unique constraint on (aggregate_id, version)

### 2. Command Handling
- Command validation and logging
- Command-to-event translation
- Success/failure tracking
- Command types: CreateProduct, UpdatePrice, AddStock, CreateOrder, AddItem, ConfirmOrder

### 3. Read Projections
- Product catalog (denormalized for fast queries)
- Order summary (pre-computed item counts)
- Customer summary (aggregated stats)
- Version tracking per projection

### 4. Event Replay
- Clear and rebuild all read models from events
- Projection status tracking (last processed event)
- Idempotent projection builders

### 5. Aggregate History
- Full event history per aggregate
- Version-ordered event timeline
- Complete audit trail

### 6. Failed Commands
- Error message preservation
- Separate success/failure counts
- No events emitted for failed commands

## Architecture

```
                    WRITE SIDE                    READ SIDE

   [Command] ─────> [Command Handler] ─────> [Event Store]
       │                                           │
       v                                           v
   [Command Log]                          [Projection Builder]
                                                   │
                                          ┌────────┼────────┐
                                          v        v        v
                                    [Products] [Orders] [Customers]
                                     (read)    (read)    (read)
                                          ^        ^        ^
                                          │        │        │
                                    [Query Handler] ← ── [Client]
```

## Database Schema

### Write Side
```
+------------------+     +------------------+
| event_store      |     | command_log      |
+------------------+     +------------------+
| id (PK)          |     | id (PK)          |
| aggregate_id     |     | command_type     |
| aggregate_type   |     | command_data     |
| event_type       |     | status           |
| event_data       |     | error_message    |
| version          |     | created_at       |
| created_at       |     | processed_at     |
+------------------+     +------------------+
```

### Read Side
```
+------------------+  +------------------+  +------------------+
| read_products    |  | read_orders      |  | read_customers   |
+------------------+  +------------------+  +------------------+
| id (PK)          |  | id (PK)          |  | id (PK)          |
| name             |  | customer_id      |  | name             |
| description      |  | status           |  | email            |
| price            |  | total_amount     |  | total_orders     |
| stock            |  | item_count       |  | total_spent      |
| category         |  | items            |  | last_order_at    |
| is_active        |  | version          |  | version          |
| version          |  +------------------+  +------------------+
+------------------+
```

## Event Flow

```
Command: CreateProduct("Widget Pro", $29.99)
    │
    v
Event: ProductCreated {id, name, price, category}
    │
    v
Projection: INSERT INTO read_products ...

Command: UpdatePrice("prod-1", $34.99)
    │
    v
Event: PriceChanged {product_id, new_price}
    │
    v
Projection: UPDATE read_products SET price = 34.99 ...
```

## Key Patterns

### Append Event to Store
```pascal
procedure AppendEvent(AggregateId, EventType, EventData: string);
begin
  Version := GetNextVersion(AggregateId);
  Conn.ExecuteNonQuery(
    'INSERT INTO event_store (aggregate_id, aggregate_type, event_type, event_data, version) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [AggregateId, AggType, EventType, EventData, Version]);
end;
```

### Project Events to Read Model
```pascal
procedure ProjectEvents;
begin
  LastProcessed := GetLastEventId;
  DS := Conn.ExecuteQuery(
    'SELECT * FROM event_store WHERE id > ? ORDER BY id', [LastProcessed]);
  while not DS.EOF do
  begin
    case EventType of
      'ProductCreated': UpdateReadProducts(...);
      'OrderConfirmed': UpdateReadOrders(...);
    end;
    DS.Next;
  end;
end;
```

### Replay (Rebuild Projections)
```pascal
Conn.ExecuteNonQuery('DELETE FROM read_products');
Conn.ExecuteNonQuery('DELETE FROM projection_status');
ProjectEvents;  // Replays all events from beginning
```

### Aggregate History
```pascal
DS := Conn.ExecuteQuery(
  'SELECT event_type, event_data, version FROM event_store ' +
  'WHERE aggregate_id = ? ORDER BY version', [AggId]);
```

## Compilation

```bash
cd 94_CQRS
lazbuild CQRS.lpi
./CQRS
```

## Sample Output

```
2. Executing Commands (Write Side)
   Total events emitted: 19
   Total commands logged: 19

4. Query: Product Catalog (Read Side)
   prod-1: Widget Pro (electronics) $34.99, stock: 100 [v3]
   prod-2: Gadget X (electronics) $44.99, stock: 50 [v3]
   prod-3: Book ABC (books) $19.99, stock: 200 [v2]

5. Query: Order Summary (Read Side)
   ord-1: customer=cust-1 (Alice Smith), status=confirmed, items=2
   ord-2: customer=cust-2 (Bob Jones), status=confirmed, items=1
   ord-3: customer=cust-1 (Alice Smith), status=created, items=1

9. Aggregate History (prod-1)
   v1: ProductCreated {"id":"prod-1","name":"Widget Pro",...}
   v2: StockAdded {"product_id":"prod-1","quantity":100}
   v3: PriceChanged {"product_id":"prod-1","new_price":34.99}

12. CQRS Summary
   Write side: 19 events, 21 commands
   Read side: 3 products, 3 orders, 2 customers
```

## Related Examples

- **92_Savepoints** - Transaction isolation patterns
- **93_StateMachine** - State transitions with history
- **86_WebhookStorage** - Event-driven delivery

## Best Practices

1. **Append-only**: Never modify or delete events in the event store
2. **Versioning**: Use aggregate versions for optimistic concurrency control
3. **Idempotent projections**: Projections should handle replayed events correctly
4. **Command validation**: Validate commands before emitting events
5. **Separate reads/writes**: Query models are optimized for read patterns, not write patterns
6. **Event replay**: The ability to rebuild read models from events is essential for schema changes
7. **Command logging**: Track all commands (success and failure) for debugging
8. **Projection tracking**: Record which events have been processed to enable incremental updates
