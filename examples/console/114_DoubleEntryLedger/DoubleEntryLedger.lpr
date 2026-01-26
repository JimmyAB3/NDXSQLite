{===============================================================================
  NDXSQLite Example 114 - Double Entry Ledger
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Chart of accounts management
  - Journal entries with debits and credits
  - Entry balance validation
  - Trial balance and account summaries
  - Financial transaction posting

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DoubleEntryLedger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

{
  Example 114: Double-Entry Ledger
  - Chart of accounts (Asset, Liability, Equity, Revenue, Expense)
  - Journal entries with balanced debits/credits
  - Trial balance verification
  - Income statement and balance sheet
  - Period closing (revenue/expense to retained earnings)
  - Ledger queries and account history
}

var
  Conn: TNDXSQLiteConnection;

// ============================================================================
// Schema Setup
// ============================================================================

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Chart of accounts
  Conn.ExecuteNonQuery(
    'CREATE TABLE accounts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  code TEXT NOT NULL UNIQUE,' +
    '  name TEXT NOT NULL,' +
    '  account_type TEXT NOT NULL,' +  // Asset, Liability, Equity, Revenue, Expense
    '  normal_balance TEXT NOT NULL,' + // Debit or Credit
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Journal entry headers
  Conn.ExecuteNonQuery(
    'CREATE TABLE journal_entries (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  entry_date TEXT NOT NULL,' +
    '  description TEXT NOT NULL,' +
    '  reference TEXT,' +
    '  period TEXT NOT NULL,' +        // YYYY-MM
    '  is_posted INTEGER DEFAULT 0,' +
    '  created_at TEXT NOT NULL DEFAULT (datetime(''now''))' +
    ')');

  // Journal entry line items
  Conn.ExecuteNonQuery(
    'CREATE TABLE journal_lines (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  entry_id INTEGER NOT NULL REFERENCES journal_entries(id),' +
    '  account_id INTEGER NOT NULL REFERENCES accounts(id),' +
    '  debit REAL NOT NULL DEFAULT 0,' +
    '  credit REAL NOT NULL DEFAULT 0,' +
    '  memo TEXT' +
    ')');

  // Period closing records
  Conn.ExecuteNonQuery(
    'CREATE TABLE period_closings (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  period TEXT NOT NULL UNIQUE,' +
    '  closed_at TEXT NOT NULL,' +
    '  net_income REAL NOT NULL,' +
    '  closing_entry_id INTEGER REFERENCES journal_entries(id)' +
    ')');

  Conn.ExecuteNonQuery('CREATE INDEX idx_journal_lines_entry ON journal_lines(entry_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_journal_lines_account ON journal_lines(account_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_journal_entries_period ON journal_entries(period)');
end;

// ============================================================================
// Chart of Accounts Setup
// ============================================================================

{ Inserts the full chart of accounts into the database, covering Asset, Liability,
  Equity, Revenue, and Expense account types with their normal balance directions. }
procedure SetupChartOfAccounts;
begin
  // Assets (normal balance: Debit)
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''1000'', ''Cash'', ''Asset'', ''Debit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''1100'', ''Accounts Receivable'', ''Asset'', ''Debit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''1200'', ''Inventory'', ''Asset'', ''Debit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''1500'', ''Equipment'', ''Asset'', ''Debit'')');

  // Liabilities (normal balance: Credit)
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''2000'', ''Accounts Payable'', ''Liability'', ''Credit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''2100'', ''Salaries Payable'', ''Liability'', ''Credit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''2500'', ''Bank Loan'', ''Liability'', ''Credit'')');

  // Equity (normal balance: Credit)
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''3000'', ''Owner Capital'', ''Equity'', ''Credit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''3100'', ''Retained Earnings'', ''Equity'', ''Credit'')');

  // Revenue (normal balance: Credit)
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''4000'', ''Sales Revenue'', ''Revenue'', ''Credit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''4100'', ''Service Revenue'', ''Revenue'', ''Credit'')');

  // Expenses (normal balance: Debit)
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''5000'', ''Cost of Goods Sold'', ''Expense'', ''Debit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''5100'', ''Salary Expense'', ''Expense'', ''Debit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''5200'', ''Rent Expense'', ''Expense'', ''Debit'')');
  Conn.ExecuteNonQuery('INSERT INTO accounts (code, name, account_type, normal_balance) VALUES (''5300'', ''Utilities Expense'', ''Expense'', ''Debit'')');
end;

// ============================================================================
// Journal Entry Functions
// ============================================================================

{ Inserts a new journal entry header record with the given date, description,
  reference, and period, and returns the auto-generated entry ID. }
function CreateJournalEntry(const EntryDate, Description, Reference, Period: string): Integer;
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO journal_entries (entry_date, description, reference, period) ' +
    'VALUES (''%s'', ''%s'', ''%s'', ''%s'')',
    [EntryDate, Description, Reference, Period]));
  Result := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
end;

{ Adds a debit line to a journal entry. }
procedure AddDebit(EntryId, AccountId: Integer; Amount: Double; const Memo: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO journal_lines (entry_id, account_id, debit, credit, memo) ' +
    'VALUES (%d, %d, %.2f, 0, ''%s'')', [EntryId, AccountId, Amount, Memo]));
end;

{ Adds a credit line to a journal entry. }
procedure AddCredit(EntryId, AccountId: Integer; Amount: Double; const Memo: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO journal_lines (entry_id, account_id, debit, credit, memo) ' +
    'VALUES (%d, %d, 0, %.2f, ''%s'')', [EntryId, AccountId, Amount, Memo]));
end;

{ Checks whether a journal entry's debits and credits are balanced. }
function IsEntryBalanced(EntryId: Integer): Boolean;
var
  TotalDebit, TotalCredit: Double;
begin
  TotalDebit := Double(Conn.ExecuteScalar(Format(
    'SELECT COALESCE(SUM(debit), 0) FROM journal_lines WHERE entry_id = %d', [EntryId])));
  TotalCredit := Double(Conn.ExecuteScalar(Format(
    'SELECT COALESCE(SUM(credit), 0) FROM journal_lines WHERE entry_id = %d', [EntryId])));
  Result := Abs(TotalDebit - TotalCredit) < 0.01;
end;

{ Validates that the journal entry is balanced, then marks it as posted;
  returns False and prints an error if debits and credits do not match. }
function PostEntry(EntryId: Integer): Boolean;
begin
  if not IsEntryBalanced(EntryId) then
  begin
    WriteLn(Format('   ERROR: Entry #%d is not balanced!', [EntryId]));
    Result := False;
    Exit;
  end;
  Conn.ExecuteNonQuery(Format(
    'UPDATE journal_entries SET is_posted = 1 WHERE id = %d', [EntryId]));
  Result := True;
end;

{ Looks up an account by its code string and returns the corresponding integer ID. }
function GetAccountId(const Code: string): Integer;
begin
  Result := Integer(Conn.ExecuteScalar(Format(
    'SELECT id FROM accounts WHERE code = ''%s''', [Code])));
end;

// ============================================================================
// Demo 1: Chart of Accounts
// ============================================================================

{ Queries all accounts and prints a formatted table showing each account's
  code, name, type, and normal balance direction. }
procedure DemoChartOfAccounts;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Chart of Accounts ===');
  WriteLn;

  WriteLn(Format('   %-6s %-22s %-12s %-8s', ['Code', 'Name', 'Type', 'Normal']));
  WriteLn('   ' + StringOfChar('-', 52));

  DS := Conn.ExecuteQuery('SELECT code, name, account_type, normal_balance FROM accounts ORDER BY code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-6s %-22s %-12s %-8s', [
        DS.FieldByName('code').AsString,
        DS.FieldByName('name').AsString,
        DS.FieldByName('account_type').AsString,
        DS.FieldByName('normal_balance').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// ============================================================================
// Demo 2: Journal Entries
// ============================================================================

{ Creates and posts a series of balanced journal entries including owner investment,
  equipment purchase, inventory, sales, rent, salaries, service revenue, utilities,
  and cost of goods sold, printing the balance and post status of each. }
procedure DemoJournalEntries;
var
  EntryId: Integer;
begin
  WriteLn('=== 2. Journal Entries (Balanced Debits/Credits) ===');
  WriteLn;

  // Entry 1: Owner invests $50,000 cash
  EntryId := CreateJournalEntry('2024-01-01', 'Owner initial investment', 'INV-001', '2024-01');
  AddDebit(EntryId, GetAccountId('1000'), 50000, 'Cash invested');
  AddCredit(EntryId, GetAccountId('3000'), 50000, 'Owner capital contribution');
  WriteLn(Format('   Entry #%d: Owner investment $50,000', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 2: Purchase equipment for $15,000 (bank loan)
  EntryId := CreateJournalEntry('2024-01-05', 'Equipment purchase via loan', 'PO-001', '2024-01');
  AddDebit(EntryId, GetAccountId('1500'), 15000, 'Office equipment');
  AddCredit(EntryId, GetAccountId('2500'), 15000, 'Bank loan for equipment');
  WriteLn(Format('   Entry #%d: Equipment purchase $15,000 (loan)', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 3: Purchase inventory $10,000
  EntryId := CreateJournalEntry('2024-01-10', 'Inventory purchase', 'PO-002', '2024-01');
  AddDebit(EntryId, GetAccountId('1200'), 10000, 'Merchandise inventory');
  AddCredit(EntryId, GetAccountId('1000'), 10000, 'Cash for inventory');
  WriteLn(Format('   Entry #%d: Inventory purchase $10,000', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 4: Sales revenue $12,000 (cash $8,000 + receivable $4,000)
  EntryId := CreateJournalEntry('2024-01-15', 'Sales to customers', 'INV-101', '2024-01');
  AddDebit(EntryId, GetAccountId('1000'), 8000, 'Cash sales');
  AddDebit(EntryId, GetAccountId('1100'), 4000, 'Credit sales');
  AddCredit(EntryId, GetAccountId('4000'), 12000, 'January sales');
  WriteLn(Format('   Entry #%d: Sales $12,000 (cash $8K + AR $4K)', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 4: Pay rent $3,000
  EntryId := CreateJournalEntry('2024-01-20', 'Monthly rent payment', 'CHK-201', '2024-01');
  AddDebit(EntryId, GetAccountId('5200'), 3000, 'January rent');
  AddCredit(EntryId, GetAccountId('1000'), 3000, 'Rent check');
  WriteLn(Format('   Entry #%d: Rent expense $3,000', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 5: Salary expense $8,000
  EntryId := CreateJournalEntry('2024-01-31', 'January salaries', 'PAY-001', '2024-01');
  AddDebit(EntryId, GetAccountId('5100'), 8000, 'January salaries');
  AddCredit(EntryId, GetAccountId('1000'), 8000, 'Salary payments');
  WriteLn(Format('   Entry #%d: Salary expense $8,000', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 6: Service revenue $5,000
  EntryId := CreateJournalEntry('2024-01-28', 'Consulting services', 'INV-102', '2024-01');
  AddDebit(EntryId, GetAccountId('1100'), 5000, 'Consulting receivable');
  AddCredit(EntryId, GetAccountId('4100'), 5000, 'Consulting fee');
  WriteLn(Format('   Entry #%d: Service revenue $5,000', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 7: Utilities $500
  EntryId := CreateJournalEntry('2024-01-31', 'Utility bills', 'UTIL-001', '2024-01');
  AddDebit(EntryId, GetAccountId('5300'), 500, 'Electric and water');
  AddCredit(EntryId, GetAccountId('1000'), 500, 'Utility payment');
  WriteLn(Format('   Entry #%d: Utilities $500', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  // Entry 8: COGS $4,000
  EntryId := CreateJournalEntry('2024-01-31', 'Cost of goods sold', 'COGS-001', '2024-01');
  AddDebit(EntryId, GetAccountId('5000'), 4000, 'Product costs');
  AddCredit(EntryId, GetAccountId('1200'), 4000, 'Inventory reduction');
  WriteLn(Format('   Entry #%d: COGS $4,000', [EntryId]));
  WriteLn(Format('     Balanced: %s, Posted: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO'), BoolToStr(PostEntry(EntryId), 'YES', 'NO')]));

  WriteLn;
end;

// ============================================================================
// Demo 3: Unbalanced Entry Detection
// ============================================================================

{ Creates an intentionally unbalanced journal entry to show that posting is
  rejected when debits and credits do not match, then removes the invalid entry. }
procedure DemoUnbalancedEntry;
var
  EntryId: Integer;
  TotalD, TotalC: Double;
begin
  WriteLn('=== 3. Unbalanced Entry Detection ===');
  WriteLn;

  // Try to create an unbalanced entry
  EntryId := CreateJournalEntry('2024-01-31', 'Intentionally unbalanced', 'ERR-001', '2024-01');
  AddDebit(EntryId, GetAccountId('1000'), 1000, 'Debit side');
  AddCredit(EntryId, GetAccountId('4000'), 750, 'Credit side - short!');

  TotalD := Double(Conn.ExecuteScalar(Format(
    'SELECT SUM(debit) FROM journal_lines WHERE entry_id = %d', [EntryId])));
  TotalC := Double(Conn.ExecuteScalar(Format(
    'SELECT SUM(credit) FROM journal_lines WHERE entry_id = %d', [EntryId])));

  WriteLn(Format('   Entry #%d: Debit=$%.2f, Credit=$%.2f', [EntryId, TotalD, TotalC]));
  WriteLn(Format('   Balanced: %s', [BoolToStr(IsEntryBalanced(EntryId), 'YES', 'NO')]));
  WriteLn('   Attempting to post...');
  PostEntry(EntryId); // Will print error

  // Clean up - delete the unbalanced entry
  Conn.ExecuteNonQuery(Format('DELETE FROM journal_lines WHERE entry_id = %d', [EntryId]));
  Conn.ExecuteNonQuery(Format('DELETE FROM journal_entries WHERE id = %d', [EntryId]));
  WriteLn('   Unbalanced entry rejected and removed');
  WriteLn;
end;

// ============================================================================
// Demo 4: Trial Balance
// ============================================================================

{ Computes and prints the trial balance for period 2024-01, showing each account's
  net debit or credit balance and verifying that total debits equal total credits. }
procedure DemoTrialBalance;
var
  DS: TDataSet;
  TotalDebits, TotalCredits: Double;
begin
  WriteLn('=== 4. Trial Balance (Period: 2024-01) ===');
  WriteLn;

  TotalDebits := 0;
  TotalCredits := 0;

  WriteLn(Format('   %-6s %-22s %12s %12s', ['Code', 'Account', 'Debit', 'Credit']));
  WriteLn('   ' + StringOfChar('-', 56));

  DS := Conn.ExecuteQuery(
    'SELECT a.code, a.name, a.normal_balance, ' +
    'COALESCE(SUM(jl.debit), 0) as total_debit, ' +
    'COALESCE(SUM(jl.credit), 0) as total_credit ' +
    'FROM accounts a ' +
    'LEFT JOIN journal_lines jl ON jl.account_id = a.id ' +
    'LEFT JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 ' +
    'GROUP BY a.id ' +
    'HAVING total_debit > 0 OR total_credit > 0 ' +
    'ORDER BY a.code');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('normal_balance').AsString = 'Debit' then
      begin
        // Debit-normal: show net in debit column
        WriteLn(Format('   %-6s %-22s %12.2f %12s', [
          DS.FieldByName('code').AsString,
          DS.FieldByName('name').AsString,
          DS.FieldByName('total_debit').AsFloat - DS.FieldByName('total_credit').AsFloat,
          '']));
        TotalDebits := TotalDebits + DS.FieldByName('total_debit').AsFloat - DS.FieldByName('total_credit').AsFloat;
      end
      else
      begin
        // Credit-normal: show net in credit column
        WriteLn(Format('   %-6s %-22s %12s %12.2f', [
          DS.FieldByName('code').AsString,
          DS.FieldByName('name').AsString,
          '',
          DS.FieldByName('total_credit').AsFloat - DS.FieldByName('total_debit').AsFloat]));
        TotalCredits := TotalCredits + DS.FieldByName('total_credit').AsFloat - DS.FieldByName('total_debit').AsFloat;
      end;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   ' + StringOfChar('-', 56));
  WriteLn(Format('   %-6s %-22s %12.2f %12.2f', ['', 'TOTALS', TotalDebits, TotalCredits]));
  WriteLn(Format('   Balanced: %s', [BoolToStr(Abs(TotalDebits - TotalCredits) < 0.01, 'YES', 'NO')]));
  WriteLn;
end;

// ============================================================================
// Demo 5: Income Statement
// ============================================================================

{ Queries posted revenue and expense accounts for period 2024-01, prints itemized
  totals for each category, and calculates the net income (revenue minus expenses). }
procedure DemoIncomeStatement;
var
  DS: TDataSet;
  TotalRevenue, TotalExpenses, NetIncome: Double;
begin
  WriteLn('=== 5. Income Statement (Period: 2024-01) ===');
  WriteLn;

  TotalRevenue := 0;
  TotalExpenses := 0;

  // Revenue
  WriteLn('   REVENUE:');
  DS := Conn.ExecuteQuery(
    'SELECT a.code, a.name, ' +
    'COALESCE(SUM(jl.credit), 0) - COALESCE(SUM(jl.debit), 0) as balance ' +
    'FROM accounts a ' +
    'JOIN journal_lines jl ON jl.account_id = a.id ' +
    'JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 AND je.period = ''2024-01'' ' +
    'WHERE a.account_type = ''Revenue'' ' +
    'GROUP BY a.id ORDER BY a.code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-22s %12.2f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('balance').AsFloat]));
      TotalRevenue := TotalRevenue + DS.FieldByName('balance').AsFloat;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('     %-22s %12.2f', ['Total Revenue', TotalRevenue]));

  // Expenses
  WriteLn;
  WriteLn('   EXPENSES:');
  DS := Conn.ExecuteQuery(
    'SELECT a.code, a.name, ' +
    'COALESCE(SUM(jl.debit), 0) - COALESCE(SUM(jl.credit), 0) as balance ' +
    'FROM accounts a ' +
    'JOIN journal_lines jl ON jl.account_id = a.id ' +
    'JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 AND je.period = ''2024-01'' ' +
    'WHERE a.account_type = ''Expense'' ' +
    'GROUP BY a.id ORDER BY a.code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-22s %12.2f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('balance').AsFloat]));
      TotalExpenses := TotalExpenses + DS.FieldByName('balance').AsFloat;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('     %-22s %12.2f', ['Total Expenses', TotalExpenses]));

  NetIncome := TotalRevenue - TotalExpenses;
  WriteLn;
  WriteLn(Format('   NET INCOME:            %12.2f', [NetIncome]));
  WriteLn;
end;

// ============================================================================
// Demo 6: Balance Sheet
// ============================================================================

{ Computes and prints asset, liability, and equity balances from posted entries,
  includes current-period net income, and verifies the accounting equation
  (Assets = Liabilities + Equity). }
procedure DemoBalanceSheet;
var
  DS: TDataSet;
  TotalAssets, TotalLiabilities, TotalEquity, NetIncome: Double;
begin
  WriteLn('=== 6. Balance Sheet (as of 2024-01-31) ===');
  WriteLn;

  TotalAssets := 0;
  TotalLiabilities := 0;
  TotalEquity := 0;

  // Assets
  WriteLn('   ASSETS:');
  DS := Conn.ExecuteQuery(
    'SELECT a.code, a.name, ' +
    'COALESCE(SUM(jl.debit), 0) - COALESCE(SUM(jl.credit), 0) as balance ' +
    'FROM accounts a ' +
    'LEFT JOIN journal_lines jl ON jl.account_id = a.id ' +
    'LEFT JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 ' +
    'WHERE a.account_type = ''Asset'' ' +
    'GROUP BY a.id HAVING balance <> 0 ORDER BY a.code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-22s %12.2f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('balance').AsFloat]));
      TotalAssets := TotalAssets + DS.FieldByName('balance').AsFloat;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('     %-22s %12.2f', ['Total Assets', TotalAssets]));

  // Liabilities
  WriteLn;
  WriteLn('   LIABILITIES:');
  DS := Conn.ExecuteQuery(
    'SELECT a.code, a.name, ' +
    'COALESCE(SUM(jl.credit), 0) - COALESCE(SUM(jl.debit), 0) as balance ' +
    'FROM accounts a ' +
    'LEFT JOIN journal_lines jl ON jl.account_id = a.id ' +
    'LEFT JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 ' +
    'WHERE a.account_type = ''Liability'' ' +
    'GROUP BY a.id HAVING balance <> 0 ORDER BY a.code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-22s %12.2f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('balance').AsFloat]));
      TotalLiabilities := TotalLiabilities + DS.FieldByName('balance').AsFloat;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('     %-22s %12.2f', ['Total Liabilities', TotalLiabilities]));

  // Equity
  WriteLn;
  WriteLn('   EQUITY:');
  DS := Conn.ExecuteQuery(
    'SELECT a.code, a.name, ' +
    'COALESCE(SUM(jl.credit), 0) - COALESCE(SUM(jl.debit), 0) as balance ' +
    'FROM accounts a ' +
    'LEFT JOIN journal_lines jl ON jl.account_id = a.id ' +
    'LEFT JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 ' +
    'WHERE a.account_type = ''Equity'' ' +
    'GROUP BY a.id HAVING balance <> 0 ORDER BY a.code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-22s %12.2f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('balance').AsFloat]));
      TotalEquity := TotalEquity + DS.FieldByName('balance').AsFloat;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Add current period net income (revenue - expenses not yet closed)
  NetIncome := Double(Conn.ExecuteScalar(
    'SELECT COALESCE(' +
    '(SELECT SUM(jl.credit) - SUM(jl.debit) FROM journal_lines jl ' +
    ' JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 ' +
    ' JOIN accounts a ON a.id = jl.account_id WHERE a.account_type = ''Revenue''), 0) - ' +
    'COALESCE(' +
    '(SELECT SUM(jl.debit) - SUM(jl.credit) FROM journal_lines jl ' +
    ' JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 ' +
    ' JOIN accounts a ON a.id = jl.account_id WHERE a.account_type = ''Expense''), 0)'));
  if Abs(NetIncome) > 0.01 then
  begin
    WriteLn(Format('     %-22s %12.2f', ['Current Period Income', NetIncome]));
    TotalEquity := TotalEquity + NetIncome;
  end;
  WriteLn(Format('     %-22s %12.2f', ['Total Equity', TotalEquity]));

  WriteLn;
  WriteLn(Format('   Assets = Liabilities + Equity: %.2f = %.2f + %.2f',
    [TotalAssets, TotalLiabilities, TotalEquity]));
  WriteLn(Format('   Accounting equation holds: %s',
    [BoolToStr(Abs(TotalAssets - (TotalLiabilities + TotalEquity)) < 0.01, 'YES', 'NO')]));
  WriteLn;
end;

// ============================================================================
// Demo 7: Period Closing
// ============================================================================

{ Closes period 2024-01 by creating a closing journal entry that zeroes out all
  revenue and expense accounts and transfers the net income to Retained Earnings,
  then records the closing in the period_closings table. }
procedure DemoPeriodClosing;
var
  NetIncome: Double;
  ClosingEntryId: Integer;
  DS: TDataSet;
begin
  WriteLn('=== 7. Period Closing (2024-01) ===');
  WriteLn;

  // Calculate net income
  NetIncome := Double(Conn.ExecuteScalar(
    'SELECT COALESCE(' +
    '(SELECT SUM(jl.credit) - SUM(jl.debit) FROM journal_lines jl ' +
    ' JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 AND je.period = ''2024-01'' ' +
    ' JOIN accounts a ON a.id = jl.account_id WHERE a.account_type = ''Revenue''), 0) - ' +
    'COALESCE(' +
    '(SELECT SUM(jl.debit) - SUM(jl.credit) FROM journal_lines jl ' +
    ' JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 AND je.period = ''2024-01'' ' +
    ' JOIN accounts a ON a.id = jl.account_id WHERE a.account_type = ''Expense''), 0)'));

  WriteLn(Format('   Net income for 2024-01: $%.2f', [NetIncome]));

  // Create closing entry: close revenue and expense accounts to Retained Earnings
  WriteLn('   Creating closing entry...');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  ClosingEntryId := CreateJournalEntry('2024-01-31', 'Period closing - January 2024', 'CLOSE-2024-01', '2024-01');

  // Close revenue accounts (debit revenue, credit retained earnings)
  DS := Conn.ExecuteQuery(
    'SELECT a.id, a.name, ' +
    'SUM(jl.credit) - SUM(jl.debit) as balance ' +
    'FROM accounts a ' +
    'JOIN journal_lines jl ON jl.account_id = a.id ' +
    'JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 AND je.period = ''2024-01'' ' +
    'WHERE a.account_type = ''Revenue'' GROUP BY a.id');
  try
    while not DS.EOF do
    begin
      AddDebit(ClosingEntryId, DS.FieldByName('id').AsInteger,
        DS.FieldByName('balance').AsFloat, 'Close ' + DS.FieldByName('name').AsString);
      WriteLn(Format('     Close %s: Debit $%.2f', [DS.FieldByName('name').AsString, DS.FieldByName('balance').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Close expense accounts (credit expense, debit retained earnings)
  DS := Conn.ExecuteQuery(
    'SELECT a.id, a.name, ' +
    'SUM(jl.debit) - SUM(jl.credit) as balance ' +
    'FROM accounts a ' +
    'JOIN journal_lines jl ON jl.account_id = a.id ' +
    'JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 AND je.period = ''2024-01'' ' +
    'WHERE a.account_type = ''Expense'' GROUP BY a.id');
  try
    while not DS.EOF do
    begin
      AddCredit(ClosingEntryId, DS.FieldByName('id').AsInteger,
        DS.FieldByName('balance').AsFloat, 'Close ' + DS.FieldByName('name').AsString);
      WriteLn(Format('     Close %s: Credit $%.2f', [DS.FieldByName('name').AsString, DS.FieldByName('balance').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Net income to Retained Earnings
  if NetIncome > 0 then
    AddCredit(ClosingEntryId, GetAccountId('3100'), NetIncome, 'Net income to retained earnings')
  else
    AddDebit(ClosingEntryId, GetAccountId('3100'), Abs(NetIncome), 'Net loss to retained earnings');

  WriteLn(Format('     Retained Earnings: Credit $%.2f', [NetIncome]));

  PostEntry(ClosingEntryId);

  // Record period closing
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO period_closings (period, closed_at, net_income, closing_entry_id) ' +
    'VALUES (''2024-01'', datetime(''now''), %.2f, %d)', [NetIncome, ClosingEntryId]));

  Conn.ExecuteNonQuery('COMMIT');

  WriteLn;
  WriteLn(Format('   Closing entry #%d posted successfully', [ClosingEntryId]));
  WriteLn(Format('   Balanced: %s', [BoolToStr(IsEntryBalanced(ClosingEntryId), 'YES', 'NO')]));
  WriteLn;
end;

// ============================================================================
// Demo 8: Account Ledger
// ============================================================================

{ Prints the full transaction history for the Cash account (code 1000), showing
  each posted entry's date, description, debit, credit, and running balance. }
procedure DemoAccountLedger;
var
  DS: TDataSet;
  RunningBalance: Double;
begin
  WriteLn('=== 8. Account Ledger (Cash - 1000) ===');
  WriteLn;

  RunningBalance := 0;

  WriteLn(Format('   %-10s %-25s %10s %10s %12s', ['Date', 'Description', 'Debit', 'Credit', 'Balance']));
  WriteLn('   ' + StringOfChar('-', 72));

  DS := Conn.ExecuteQuery(
    'SELECT je.entry_date, je.description, jl.debit, jl.credit, jl.memo ' +
    'FROM journal_lines jl ' +
    'JOIN journal_entries je ON je.id = jl.entry_id AND je.is_posted = 1 ' +
    'WHERE jl.account_id = (SELECT id FROM accounts WHERE code = ''1000'') ' +
    'ORDER BY je.entry_date, je.id');
  try
    while not DS.EOF do
    begin
      RunningBalance := RunningBalance + DS.FieldByName('debit').AsFloat - DS.FieldByName('credit').AsFloat;
      WriteLn(Format('   %-10s %-25s %10.2f %10.2f %12.2f', [
        DS.FieldByName('entry_date').AsString,
        Copy(DS.FieldByName('description').AsString, 1, 25),
        DS.FieldByName('debit').AsFloat,
        DS.FieldByName('credit').AsFloat,
        RunningBalance]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   ' + StringOfChar('-', 72));
  WriteLn(Format('   Final Cash Balance: $%.2f', [RunningBalance]));
  WriteLn;
end;

// ============================================================================
// Main Program
// ============================================================================

begin
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    SetupChartOfAccounts;

    WriteLn('Example 114: Double-Entry Ledger - Debits/Credits, Trial Balance, Period Closing');
    WriteLn(StringOfChar('=', 78));
    WriteLn;

    DemoChartOfAccounts;
    DemoJournalEntries;
    DemoUnbalancedEntry;
    DemoTrialBalance;
    DemoIncomeStatement;
    DemoBalanceSheet;
    DemoPeriodClosing;
    DemoAccountLedger;

    WriteLn('Done.');
    Conn.Close;
  finally
    Conn.Free;
  end;
end.
