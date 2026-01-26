# Example 103: Invoice System - Line Items, Taxes, Payments

## Overview

This example demonstrates a complete **invoicing system** with line items, per-line tax rates, percentage and fixed discounts, partial payment tracking, PDF-ready formatted queries, aging reports, and revenue analytics.

## Features Demonstrated

### 1. Invoice Creation
- Invoice header with customer, dates, status
- Multiple line items per invoice
- Products/services catalog with default tax rates
- Discount types: percentage, fixed, per-line

### 2. Line Items
- Product reference or custom description
- Quantity, unit price, tax rate per line
- Line-level discounts
- Subtotal and tax calculation per line

### 3. Tax & Discount Calculation
- Per-line tax rates (e.g., services taxed, support exempt)
- Invoice-level discounts (percentage or fixed)
- Proportional tax adjustment after discount
- Grand total: (subtotal - discount) + adjusted tax

### 4. Payment Tracking
- Multiple payments per invoice
- Payment methods: bank transfer, credit card, check, cash
- Partial payment support
- Invoice status: draft, sent, paid, partial, overdue, cancelled

### 5. PDF-Ready Invoice Query
- Formatted invoice suitable for PDF rendering
- Header, line items, totals, payment history
- Balance due calculation

### 6. Aging Report
- Outstanding invoices with days overdue
- Days calculated via SQLite julianday()
- Amount and outstanding per invoice

### 7. Revenue Report
- Revenue by customer (billed vs paid)
- Revenue by product/service
- Payment method distribution

## Database Schema

```
+------------------+     +------------------+     +------------------+
| customers        |     | invoices         |     | invoice_lines    |
+------------------+     +------------------+     +------------------+
| id (PK)          |<--->| id (PK)          |<--->| id (PK, AUTO)    |
| name             |     | invoice_number   |     | invoice_id (FK)  |
| email            |     | customer_id (FK) |     | product_id (FK)  |
| address          |     | status           |     | description      |
| tax_id           |     | issued_at        |     | quantity         |
+------------------+     | due_at           |     | unit_price       |
                          | discount_type    |     | tax_rate         |
+------------------+     | discount_value   |     | line_discount    |
| products         |     +------------------+     +------------------+
+------------------+
| id (PK)          |     +------------------+
| name             |     | payments         |
| unit_price       |     +------------------+
| tax_rate         |     | id (PK, AUTO)    |
+------------------+     | invoice_id (FK)  |
                          | amount           |
                          | method           |
                          | reference        |
                          | paid_at          |
                          +------------------+
```

## Compilation

```bash
cd 103_InvoiceSystem
lazbuild InvoiceSystem.lpi
./InvoiceSystem
```

## Sample Output

```
3. Tax & Discount Calculation
   Lines subtotal:    $  19200.00
   Discount (10%):   -$   1920.00
   Tax (adjusted):    $   2376.00
   GRAND TOTAL:       $  19656.00

5. PDF-Ready Invoice
   | Enterprise License 2024        1   5000.00   20%   5000.00 |
   | Production Server              2   3500.00   20%   7000.00 |
   |                                   TOTAL:  19656.00 |
   |                                    Paid:  18000.00 |
   |                                 Balance:   1656.00 |

6. Aging Report
   INV-2023-099 Acme Corp  2023-12-01  91 days  outstanding=$2000.00
```

## Related Examples

- **101_WorkflowEngine** - Approval workflows
- **102_InventoryManagement** - Stock and product management
- **104_ChatMessaging** - Real-time messaging patterns

## Best Practices

1. **Per-line tax rates**: Different products may have different tax rules
2. **Discount before tax**: Apply discount to subtotal, then calculate tax
3. **Immutable payments**: Never modify payment records, only add new ones
4. **Status machine**: draft->sent->partial/paid/overdue->cancelled
5. **Aging calculation**: Use julianday() for accurate day counting
6. **Line discount**: Support both invoice-level and line-level discounts
7. **PDF queries**: Use single queries with JOINs for efficient rendering
