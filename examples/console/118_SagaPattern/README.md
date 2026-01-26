# Example 118: Saga Pattern - Long-Running Transactions with Compensating Actions

## Overview

This example demonstrates the **Saga Pattern** for managing long-running distributed transactions. Each saga consists of multiple ordered steps, where each step has both an action and a compensation (rollback) operation. If any step fails, previously completed steps are compensated in reverse order, maintaining data consistency without distributed locks.

## Features Demonstrated

### 1. Successful Saga Execution
- Multi-step orchestration (BookTrip: flight + hotel + car + payment)
- Sequential step execution in defined order
- Result tracking per step (confirmation data)
- Final status: completed

### 2. Failed Saga with Compensation
- Failure detection mid-saga (step 3 fails)
- Automatic compensation of completed steps in reverse order
- Steps 1,2 compensated; step 4 never executed
- Final status: compensated

### 3. Early Failure (First Step)
- When step 1 fails, no compensation needed
- Saga immediately transitions to compensated state
- Remaining steps stay in pending status

### 4. Different Saga Types
- BookTrip: ReserveFlight -> ReserveHotel -> ReserveCarRental -> ChargePayment
- OrderFulfillment: ValidateInventory -> ReserveInventory -> ProcessPayment -> CreateShipment -> SendConfirmation
- Each step has a specific compensation action

### 5. Saga Execution Log
- Complete audit trail of all saga actions
- Tracks: creation, start, step execution, failures, compensation
- Chronological ordering for debugging

### 6. Saga State Overview
- Dashboard of all sagas with their current status
- Progress tracking (current step / total steps)
- Error messages for failed sagas

### 7. Statistics
- Sagas by status (completed, compensated, failed)
- Steps by status distribution
- Compensation rate (how often sagas need rollback)

## Architecture

```
+------------------+     +------------------+     +------------------+
| Saga Definition  |     | Saga Orchestrator|     | Step Execution   |
+------------------+     +------------------+     +------------------+
| CreateSaga()     |---->| RunSaga()        |---->| ExecuteStep()    |
| AddSagaStep()    |     | For each step:   |     | Success/Failure  |
| (action + comp)  |     |   Execute        |     +------------------+
+------------------+     |   If fail:       |              |
                          |     Compensate   |              v
                          +------------------+     +------------------+
                                  |                | CompensateStep() |
                                  v                +------------------+
                          +------------------+     | Reverse order    |
                          | Saga States      |     | Undo completed   |
                          +------------------+     | steps            |
                          | pending          |     +------------------+
                          | running          |
                          | completed        |
                          | compensating     |
                          | compensated      |
                          +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| sagas            |     | saga_steps       |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| saga_id (UNI)    |<--->| saga_id (FK)     |
| saga_type        |     | step_name        |
| status           |     | step_order       |
| payload (JSON)   |     | status           |
| current_step     |     | action_data      |
| total_steps      |     | compensation_data|
| started_at       |     | result_data      |
| completed_at     |     | started_at       |
| error_message    |     | completed_at     |
+------------------+     | error_message    |
                          | UNI(saga,order)  |
+------------------+      +------------------+
| saga_log         |
+------------------+
| id (PK, AUTO)    |
| saga_id (FK)     |
| step_name        |
| action           |
| status           |
| message          |
| created_at       |
+------------------+
```

## Saga States

```
pending --> running --> completed (all steps OK)
                |
                v
           compensating (step failed)
                |
                v
           compensated (rollback complete)
```

## Step States

```
pending --> running --> completed --> compensated (if saga fails later)
                |
                v
              failed (triggers saga compensation)
```

## Compilation

```bash
cd 118_SagaPattern
lazbuild SagaPattern.lpi
./SagaPattern
```

## Sample Output

```
1. Successful Saga (BookTrip)
   Steps: ReserveFlight -> ReserveHotel -> ReserveCarRental -> ChargePayment
   Result: completed (4/4 steps)

2. Failed Saga with Compensation
   Step 3 (ReserveCarRental) failed: Service unavailable
   Compensated: CancelHotelReservation, CancelFlightReservation
   Result: compensated

6. Saga State Overview
   saga-trip-001    BookTrip         completed    4/4
   saga-trip-002    BookTrip         compensated  3/4  Step 3 failed
   saga-order-001   OrderFulfillment completed    5/5

7. Statistics
   Compensation rate: 3/5 sagas required compensation
```

## Related Examples

- **117_OutboxPattern** - Transactional outbox for reliable event publishing
- **119_IdempotencyKeys** - Exactly-once processing with deduplication

## Best Practices

1. **Each step needs compensation**: Define undo logic for every action before implementing
2. **Compensate in reverse**: Roll back from the last completed step to the first
3. **Idempotent compensations**: Compensation actions must be safe to retry
4. **Log everything**: Maintain a detailed execution log for debugging and auditing
5. **State tracking**: Always know the current state of each saga and step
6. **Fail fast**: Detect failures early to minimize the number of compensations needed
7. **Payload preservation**: Store enough context to perform compensations later
8. **Timeouts**: In production, add timeout detection for steps that hang
