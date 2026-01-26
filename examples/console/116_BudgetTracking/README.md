# Example 116: Budget Tracking - Allocation, Expenses, Rollover, Alerts

## Overview

This example demonstrates a **Budget Tracking** system with category-based budget allocation, individual expense tracking with approval workflow, configurable overspend alerts at multiple thresholds, unused budget rollover between periods, month-over-month period comparisons, and comprehensive spending analytics.

## Features Demonstrated

### 1. Budget Categories
- Named categories with descriptions (Marketing, Engineering, Operations, Travel, HR)
- Active/inactive toggle for category management

### 2. Budget Allocations
- Monthly budget allocation per category
- Separate tracking of base allocation and rollover amounts
- Total available budget = allocated + rollover

### 3. Expense Tracking
- Individual expense records with descriptions
- Date and period tracking
- Approval workflow (approved_by field)
- Link to budget category for utilization tracking

### 4. Budget Status Dashboard
- Per-category budget vs spent comparison
- Remaining balance calculation
- Utilization percentage
- Status indicators: OK, WARNING (75%+), CRITICAL (90%+), OVER BUDGET (100%+)

### 5. Overspend Alerts
- Configurable alert thresholds (75% warning, 90% critical, 100% overspend)
- Automatic alert generation when thresholds are crossed
- De-duplication (same alert type not generated twice per category/period)
- Alert message with context (percentage, amounts)

### 6. Budget Rollover
- Unused budget from one period carries to the next
- Only positive remaining amounts roll over (overspend does not carry)
- Rollover tracked separately from base allocation
- Notes field documents the source period

### 7. Period Comparison
- Month-over-month spending comparison
- Absolute change and percentage change
- Category-level and total-level comparisons
- Identifies spending trends (increases/decreases)

### 8. Category Expense Breakdown
- Detailed listing of all expenses per category
- Sorted by amount (largest first within category)
- Shows approval chain for audit

### 9. Summary Statistics
- Total expenses count and amount
- Average, min, max expense amounts
- Budget utilization rate
- Top approvers by total amount approved

## Architecture

```
+------------------+     +------------------+     +------------------+
| Budget Categories|     | Budget Allocations|    | Expenses         |
+------------------+     +------------------+     +------------------+
| Marketing        |<--->| Period: 2024-01  |     | Amount, Date     |
| Engineering      |     | Allocated: $15K  |<--->| Description      |
| Operations       |     | Rollover: $1.5K  |     | Approved by      |
| Travel, HR       |     | Total: $16.5K    |     | Category link    |
+------------------+     +------------------+     +------------------+
                                                          |
                                                          v
+------------------+     +------------------+     +------------------+
| Alert Thresholds |     | Budget Alerts    |     | Reports          |
+------------------+     +------------------+     +------------------+
| 75% -> warning   |---->| Category/Period  |     | Budget Status    |
| 90% -> critical  |     | Current %        |     | Period Compare   |
| 100% -> overspend|     | Message          |     | Statistics       |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| budget_categories|     | budget_allocations|
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| name (UNIQUE)    |<--->| category_id (FK) |
| description      |     | period (YYYY-MM) |
| is_active        |     | allocated_amount |
+------------------+     | rollover_amount  |
                          | notes            |
+------------------+      | UNIQUE(cat,period)|
| expenses         |      +------------------+
+------------------+
| id (PK, AUTO)    |     +------------------+
| category_id (FK) |     | budget_alerts    |
| amount           |     +------------------+
| description      |     | id (PK, AUTO)    |
| expense_date     |     | category_id (FK) |
| period           |     | period           |
| approved_by      |     | alert_type       |
| created_at       |     | threshold_pct    |
+------------------+     | current_pct      |
                          | message          |
+------------------+      +------------------+
| alert_thresholds |
+------------------+
| id (PK, AUTO)    |
| threshold_pct    |
| alert_type       |
| is_active        |
+------------------+
```

## Compilation

```bash
cd 116_BudgetTracking
lazbuild BudgetTracking.lpi
./BudgetTracking
```

## Sample Output

```
4. Budget Status (January 2024)
   Engineering    $25,000  $17,700  $7,300   70.8%  OK
   HR             $10,000  $8,000   $2,000   80.0%  WARNING
   Marketing      $15,000  $13,500  $1,500   90.0%  CRITICAL
   Operations     $8,000   $8,800   -$800   110.0%  OVER BUDGET
   Travel         $5,000   $2,000   $3,000   40.0%  OK

5. Overspend Alerts
   [OVERSPEND] Operations: 110.0% of budget used
   [CRITICAL] Marketing: 90.0% of budget used
   [WARNING] HR: 80.0% of budget used

6. Budget Rollover (January -> February)
   Engineering +$7,300, Travel +$3,000, HR +$2,000, Marketing +$1,500

7. Period Comparison
   Marketing:   +29.6%  (Jan $13,500 -> Feb $17,500)
   Travel:     +165.0%  (Jan $2,000 -> Feb $5,300)
   Operations:  -58.0%  (Jan $8,800 -> Feb $3,700)

9. Summary
   Total expenses: 30, Total spent: $101,500
   Utilization: 79.4%
```

## Related Examples

- **114_DoubleEntryLedger** - Double-entry bookkeeping with debits/credits
- **115_PaymentProcessing** - Payment lifecycle and reconciliation

## Best Practices

1. **Category-based budgets**: Organize spending by department/function for visibility
2. **Threshold alerts**: Configure multiple warning levels (75%, 90%, 100%) for early intervention
3. **Rollover policy**: Carry unused budget forward to incentivize accuracy over year-end spending rushes
4. **Period comparison**: Track trends to identify systematic over/under-spending
5. **Approval tracking**: Record who approved each expense for accountability
6. **Separate allocation vs rollover**: Distinguish base budget from carried-over amounts
7. **De-duplicate alerts**: Only generate each alert type once per category/period
8. **Atomic operations**: Use transactions for rollover calculations to prevent inconsistencies
