# Example 93: State Machine with Validated Transitions

## Overview

This example demonstrates a **database-driven state machine** with validated transitions. It implements state definitions, transition rules with guard conditions, multiple machine types (order, ticket), transition history logging, invalid transition rejection, and state analytics.

## Features Demonstrated

### 1. State Definitions
- Named states per machine type
- Initial and final state markers
- State descriptions

### 2. Transition Rules
- From/to state pairs with event triggers
- Guard conditions (pre-conditions for transitions)
- Unique constraint: one transition per (state, event) pair

### 3. Transition Validation
- Only allowed transitions are executed
- Invalid events are rejected with clear messages
- Guard conditions are checked before transition

### 4. Transition History
- Full audit trail of state changes
- Actor tracking (who triggered the transition)
- Metadata per transition

### 5. Multiple Machine Types
- Order machine: draft -> pending -> paid -> shipped -> delivered
- Ticket machine: open -> assigned -> in_progress -> resolved -> closed
- Same infrastructure, different rules

### 6. Cancellation & Refund Paths
- Multiple paths to terminal states
- State-dependent availability (can't cancel after payment)
- Refund available from paid or delivered states

### 7. Reopen Pattern (Ticket)
- Non-linear flow: resolved -> reopened -> in_progress
- Cycle handling without infinite loops

## Database Schema

```
+------------------+     +------------------+
| states           |     | transitions      |
+------------------+     +------------------+
| machine_type     |     | machine_type     |
| state_name       |     | from_state       |
| is_initial       |     | to_state         |
| is_final         |     | event_name       |
| description      |     | guard_condition  |
+------------------+     | description      |
                         +------------------+

+------------------+     +------------------+
| entities         |     | state_history    |
+------------------+     +------------------+
| machine_type     |     | entity_id (FK)   |
| entity_ref       |     | from_state       |
| current_state    |     | to_state         |
| data             |     | event_name       |
| created_at       |     | actor            |
| updated_at       |     | metadata         |
+------------------+     | created_at       |
                         +------------------+
```

## State Diagrams

### Order Machine
```
[draft] --submit--> [pending] --pay--> [paid] --process--> [processing]
  |                    |                  |                      |
  +--cancel-->     +--cancel-->       +--refund-->          +--ship-->
  |                    |                  |                      |
  v                    v                  v                      v
[cancelled]      [cancelled]        [refunded]            [shipped]
                                        ^                      |
                                        |                 +--deliver-->
                                        |                      |
                                        +---refund---  [delivered]
```

### Ticket Machine
```
[open] --assign--> [assigned] --start--> [in_progress]
  |                                           |    ^
  +--close-->                          ask_customer |
  |                                           |    customer_reply
  v                                           v    |
[closed] <--close-- [resolved] <--resolve-- [waiting]
                        |
                    +--reopen-->
                        |
                        v
                   [reopened] --start_work--> [in_progress]
```

## Key Patterns

### Create Entity in Initial State
```pascal
function CreateEntity(MachineType, Ref, Data: string): Integer;
begin
  InitialState := Conn.ExecuteScalar(
    'SELECT state_name FROM states WHERE machine_type = ? AND is_initial = 1',
    [MachineType]);
  Conn.ExecuteNonQuery(
    'INSERT INTO entities (machine_type, entity_ref, current_state, data) ' +
    'VALUES (?, ?, ?, ?)', [MachineType, Ref, InitialState, Data]);
end;
```

### Validate and Execute Transition
```pascal
function TriggerEvent(EntityId: Integer; EventName, Actor: string): Boolean;
begin
  // 1. Get current state
  CurrentState := GetCurrentState(EntityId);
  // 2. Find transition rule
  DS := Conn.ExecuteQuery(
    'SELECT to_state, guard_condition FROM transitions ' +
    'WHERE machine_type = ? AND from_state = ? AND event_name = ?', ...);
  // 3. Reject if no valid transition
  if DS.EOF then Exit(False);
  // 4. Update state
  Conn.ExecuteNonQuery('UPDATE entities SET current_state = ? ...', [ToState]);
  // 5. Log transition
  Conn.ExecuteNonQuery('INSERT INTO state_history ...', [...]);
end;
```

### Available Transitions Query
```pascal
DS := Conn.ExecuteQuery(
  'SELECT event_name, to_state FROM transitions ' +
  'WHERE machine_type = ? AND from_state = ?',
  [MachineType, CurrentState]);
```

## Compilation

```bash
cd 93_StateMachine
lazbuild StateMachine.lpi
./StateMachine
```

## Sample Output

```
4. Order Lifecycle (Happy Path)
     draft -> pending (event: submit, actor: customer)
     pending -> paid (event: pay, actor: payment_gateway)
     paid -> processing (event: process, actor: warehouse)
     processing -> shipped (event: ship, actor: logistics)
     shipped -> delivered (event: deliver, actor: courier)

5. Invalid Transition Attempts
     REJECTED: No transition from "draft" on event "ship"
     REJECTED: No transition from "pending" on event "process"

7. Ticket Lifecycle (with Reopen)
     resolved -> reopened (event: reopen, actor: customer)
     reopened -> in_progress (event: start_work, actor: alice)
     resolved -> closed (event: close, actor: customer)
```

## Related Examples

- **92_Savepoints** - Nested transactions for atomic state changes
- **94_CQRS** - Command/Query separation
- **86_WebhookStorage** - Event-driven architecture

## Best Practices

1. **Single source of truth**: Store transition rules in the database, not in code
2. **Unique constraint**: (machine_type, from_state, event_name) prevents ambiguous transitions
3. **History logging**: Every state change is recorded with actor and timestamp
4. **Guard conditions**: Add pre-conditions for transitions that need validation
5. **Initial/Final markers**: Explicitly mark entry and terminal states
6. **Reject loudly**: Invalid transitions should be clearly rejected, never silently ignored
7. **Idempotent checks**: Always verify current state before transitioning
8. **Separate machines**: Use `machine_type` to define multiple FSMs in the same tables
