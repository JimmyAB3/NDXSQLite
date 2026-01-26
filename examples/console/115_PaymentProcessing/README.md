# Example 115: Payment Processing - Lifecycle, Idempotency, Disputes, Reconciliation

## Overview

This example demonstrates a **Payment Processing** system with full payment lifecycle management (pending/authorized/captured/refunded), idempotency keys for safe retries, partial captures, void authorization, dispute handling with evidence submission, batch reconciliation against external transaction records, and comprehensive event logging.

## Features Demonstrated

### 1. Payment Lifecycle
- Full flow: Pending -> Authorized -> Captured
- State transitions with validation (can't skip states)
- Amount tracking at each stage (authorized, captured, refunded)
- Timestamp tracking for audit

### 2. Partial Capture
- Authorize full amount, capture in parts (e.g., partial shipment)
- Track partially_captured vs fully captured states
- Prevent over-capture (cannot exceed authorized amount)

### 3. Idempotency Keys
- Same idempotency key returns existing payment (no duplicate charges)
- Different key creates a new payment
- Safe retry mechanism for network failures

### 4. Refunds (Full and Partial)
- Full refund: captured -> refunded
- Partial refund: captured -> partially_refunded
- Cannot refund more than captured amount
- Cannot refund non-captured payments (authorization only)

### 5. Void Authorization
- Cancel authorized (not yet captured) payments
- Voided payments cannot be subsequently captured
- Releases the authorization hold

### 6. Dispute Handling
- Open disputes on captured payments
- Submit evidence (shipping confirmations, receipts)
- Resolution tracking (won/lost)
- Payment status restored after winning dispute

### 7. Reconciliation
- Match internal payments against external processor records
- Detect amount discrepancies
- Identify unmatched external transactions
- Batch processing with summary statistics

### 8. Payment Event History
- Complete audit trail of all state transitions
- Chronological event log per payment
- Metadata for context (dispute reasons, resolution details)

### 9. Payment Statistics
- Breakdown by payment status
- Total captured and refunded amounts
- Event type summary counts

## Architecture

```
+------------------+     +------------------+     +------------------+
| Payment Creation |     | Authorization    |     | Capture          |
+------------------+     +------------------+     +------------------+
| CreatePayment()  |---->| AuthorizePayment |---->| CapturePayment   |
| Idempotency key  |     | Amount hold      |     | Full or partial  |
| Pending status   |     | Authorized status|     | Captured status  |
+------------------+     +------------------+     +------------------+
                                  |                         |
                                  v                         v
                          +------------------+     +------------------+
                          | VoidPayment()    |     | RefundPayment()  |
                          +------------------+     +------------------+
                          | Cancel auth hold |     | Full or partial  |
                          | Voided status    |     | Refunded status  |
                          +------------------+     +------------------+

+------------------+     +------------------+
| Dispute Handling |     | Reconciliation   |
+------------------+     +------------------+
| Open dispute     |     | External records |
| Submit evidence  |     | Amount matching  |
| Resolve (won/lost)     | Discrepancy detect|
+------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| payments         |     | payment_events   |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| payment_id (UNI) |<--->| payment_id (FK)  |
| idempotency_key  |     | event_type       |
| customer_id      |     | status_from      |
| amount           |     | status_to        |
| currency         |     | amount           |
| status           |     | metadata         |
| authorized_amount|     | created_at       |
| captured_amount  |     +------------------+
| refunded_amount  |
| created_at       |     +------------------+
| updated_at       |     | disputes         |
+------------------+     +------------------+
        ^                 | id (PK, AUTO)    |
        |                 | dispute_id (UNI) |
        +-----------------| payment_id (FK)  |
                          | reason           |
+------------------+      | amount           |
| reconciliation   |      | status           |
| _batches         |      | evidence         |
+------------------+      | resolution       |
| id (PK, AUTO)    |      | opened_at        |
| batch_id (UNI)   |      | resolved_at      |
| batch_date       |      +------------------+
| total_payments   |
| total_amount     |      +------------------+
| matched_count    |      | external         |
| unmatched_count  |      | _transactions    |
| discrepancy_amt  |      +------------------+
| status           |      | id (PK, AUTO)    |
| created_at       |      | external_id      |
+------------------+      | payment_id (FK)  |
                           | amount           |
                           | transaction_date |
                           | source           |
                           | matched          |
                           | batch_id         |
                           +------------------+
```

## Payment States

```
pending --> authorized --> captured --> refunded
                |              |
                v              v
             voided     partially_refunded
                               |
                               v
                           refunded

captured --> disputed --> captured (won)
                     \--> refunded (lost)
```

## Compilation

```bash
cd 115_PaymentProcessing
lazbuild PaymentProcessing.lpi
./PaymentProcessing
```

## Sample Output

```
1. Payment Lifecycle
   Created: pay_lifecycle_001 for $250.00
   Authorized: YES -> Captured: YES
   Final status: captured, captured: $250.00

2. Partial Capture
   Authorized: $500.00
   Partial capture $300 -> partially_captured
   Remaining $200 -> captured, total: $500.00
   Over-capture $50: NO (correctly rejected)

3. Idempotency Keys
   Same key returns same payment: YES
   Different key creates new payment: YES

4. Refunds
   Full refund $150 -> refunded
   Partial refund $80 -> partially_refunded
   Refund on authorized-only: NO (correctly rejected)

7. Reconciliation
   Matched: 4, Unmatched: 2
   Discrepancy amount: $100.49

9. Statistics
   Total payments: 14
   Total captured: $2,425.00
   Total refunded: $230.00
```

## Related Examples

- **114_DoubleEntryLedger** - Double-entry bookkeeping with debits/credits
- **116_BudgetTracking** - Budget allocation and expense tracking

## Best Practices

1. **State machine**: Enforce valid transitions (can't capture without authorization)
2. **Idempotency**: Use unique keys to prevent duplicate charges on retry
3. **Partial operations**: Support partial captures and refunds for flexible fulfillment
4. **Event sourcing**: Log every state change for complete audit trail
5. **Dispute flow**: Track evidence and resolution for chargeback management
6. **Reconciliation**: Regularly match internal records against external processor
7. **Amount guards**: Never capture > authorized or refund > captured
8. **Void vs Refund**: Void releases auth hold (no capture); refund returns captured funds
