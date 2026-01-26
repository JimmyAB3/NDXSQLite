# Example 114: Double-Entry Ledger - Debits/Credits, Trial Balance, Period Closing

## Overview

This example demonstrates a **Double-Entry Bookkeeping** system with a chart of accounts, balanced journal entries, trial balance verification, income statement, balance sheet with accounting equation validation, period closing, and account ledger history.

## Features Demonstrated

### 1. Chart of Accounts
- Five account types: Asset, Liability, Equity, Revenue, Expense
- Normal balance designation (Debit or Credit)
- Standard account numbering (1xxx Assets, 2xxx Liabilities, 3xxx Equity, 4xxx Revenue, 5xxx Expenses)

### 2. Journal Entries (Balanced Debits/Credits)
- Multi-line entries (compound journal entries)
- Every entry must balance: total debits = total credits
- Posted flag for draft vs final entries
- Reference numbers for external document linking

### 3. Unbalanced Entry Detection
- Balance validation before posting
- Reject entries where debits != credits
- Cleanup of failed entries

### 4. Trial Balance
- Sum all account balances for a period
- Debit-normal accounts in debit column, credit-normal in credit column
- Total debits must equal total credits
- Verification of overall ledger integrity

### 5. Income Statement
- Revenue accounts (credit-normal): Sales, Service
- Expense accounts (debit-normal): COGS, Salary, Rent, Utilities
- Net Income = Revenue - Expenses

### 6. Balance Sheet
- Assets = Liabilities + Equity (accounting equation)
- Current Period Income included in equity before closing
- Validates the fundamental accounting equation

### 7. Period Closing
- Close temporary accounts (Revenue, Expense) to Retained Earnings
- Debit all revenue accounts to zero
- Credit all expense accounts to zero
- Net difference to Retained Earnings
- Closing entry is itself a balanced journal entry

### 8. Account Ledger
- Transaction history for a specific account
- Running balance calculation
- Chronological ordering by date and entry ID

## Architecture

```
+------------------+     +------------------+
| Domain Commands  |     | Validation       |
+------------------+     +------------------+
| CreateJournalEntry|    | IsEntryBalanced  |
| AddDebit()       |---->| PostEntry()      |
| AddCredit()      |     | Trial Balance    |
+------------------+     | Equation Check   |
        |                 +------------------+
        v
+------------------+     +------------------+
| Journal Entries  |     | Journal Lines    |
+------------------+     +------------------+
| id, date         |<--->| entry_id (FK)    |
| description      |     | account_id (FK)  |
| period, posted   |     | debit, credit    |
+------------------+     +------------------+
                                  |
                                  v
+------------------+     +------------------+
| Accounts (COA)   |     | Reports          |
+------------------+     +------------------+
| code, name       |     | Trial Balance    |
| account_type     |     | Income Statement |
| normal_balance   |     | Balance Sheet    |
+------------------+     | Account Ledger   |
                          +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| accounts         |     | journal_entries  |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| code (UNIQUE)    |     | entry_date       |
| name             |     | description      |
| account_type     |     | reference        |
| normal_balance   |     | period (YYYY-MM) |
| is_active        |     | is_posted        |
+------------------+     +------------------+
        ^                         ^
        |                         |
+------------------+     +------------------+
| journal_lines    |     | period_closings  |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| entry_id (FK)    |     | period (UNIQUE)  |
| account_id (FK)  |     | closed_at        |
| debit            |     | net_income       |
| credit           |     | closing_entry_id |
| memo             |     +------------------+
+------------------+
```

## Compilation

```bash
cd 114_DoubleEntryLedger
lazbuild DoubleEntryLedger.lpi
./DoubleEntryLedger
```

## Sample Output

```
4. Trial Balance
   TOTALS: Debit=$82,000  Credit=$82,000  Balanced: YES

5. Income Statement
   Revenue: $17,000  Expenses: $15,500  Net Income: $1,500

6. Balance Sheet
   Assets=$66,500 = Liabilities=$15,000 + Equity=$51,500
   Accounting equation holds: YES

7. Period Closing
   Close Revenue -> Retained Earnings
   Close Expenses -> Retained Earnings
   Net Income $1,500 to Retained Earnings
```

## Related Examples

- **115_PaymentProcessing** - Payment lifecycle and reconciliation
- **116_BudgetTracking** - Budget allocation and expense tracking

## Best Practices

1. **Always balance**: Every journal entry must have equal debits and credits
2. **Post after validation**: Only post entries that pass balance check
3. **Period closing**: Close temporary accounts monthly/quarterly to Retained Earnings
4. **Accounting equation**: Assets = Liabilities + Equity must always hold
5. **Audit trail**: Never delete posted entries; use correcting entries instead
6. **Account numbering**: Use systematic codes (1xxx, 2xxx...) for easy sorting
7. **Compound entries**: Multiple debits/credits in one entry for complex transactions
8. **Running balances**: Track account history with chronological ledger view
