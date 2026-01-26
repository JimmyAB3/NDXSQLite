{===============================================================================
  NDXSQLite Example 92 - Savepoints
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Basic savepoint creation and release
  - Rollback to savepoint on error
  - Nested savepoints for complex workflows
  - Order processing with partial rollback
  - Batch operations with error handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Savepoints;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;

{ Returns the number of rows in the specified table. }
function GetCount(const TableName: string): Integer;
begin
  Result := Conn.ExecuteScalar('SELECT COUNT(*) FROM ' + TableName);
end;

{ Queries and prints each account name and balance, ordered alphabetically. }
procedure ShowAccounts;
var
  DS: TDataSet;
begin
  DS := Conn.ExecuteQuery(
    'SELECT name, balance FROM accounts ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('balance').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('=== Nested Transactions with SAVEPOINT ===');
  WriteLn;
  WriteLn('1. Creating Schema');

  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS accounts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL UNIQUE,' +
    '  balance REAL NOT NULL DEFAULT 0.0,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS transactions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  from_account TEXT,' +
    '  to_account TEXT,' +
    '  amount REAL NOT NULL,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS audit_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  action TEXT NOT NULL,' +
    '  details TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS orders (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  customer TEXT NOT NULL,' +
    '  product TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  total REAL NOT NULL,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS inventory (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  product TEXT NOT NULL UNIQUE,' +
    '  stock INTEGER NOT NULL DEFAULT 0,' +
    '  price REAL NOT NULL DEFAULT 0.0' +
    ')');

  WriteLn('   Tables: accounts, transactions, audit_log, orders, inventory');
end;

{ Inserts four accounts (Alice, Bob, Charlie, Diana) and three inventory products (Widget, Gadget, Doohickey) with initial balances and stock. }
procedure SetupData;
begin
  WriteLn;
  WriteLn('2. Setup Initial Data');

  Conn.ExecuteNonQuery('INSERT INTO accounts (name, balance) VALUES (?, ?)', ['Alice', '1000.00']);
  Conn.ExecuteNonQuery('INSERT INTO accounts (name, balance) VALUES (?, ?)', ['Bob', '500.00']);
  Conn.ExecuteNonQuery('INSERT INTO accounts (name, balance) VALUES (?, ?)', ['Charlie', '750.00']);
  Conn.ExecuteNonQuery('INSERT INTO accounts (name, balance) VALUES (?, ?)', ['Diana', '2000.00']);

  Conn.ExecuteNonQuery('INSERT INTO inventory (product, stock, price) VALUES (?, ?, ?)', ['Widget', '100', '9.99']);
  Conn.ExecuteNonQuery('INSERT INTO inventory (product, stock, price) VALUES (?, ?, ?)', ['Gadget', '50', '24.99']);
  Conn.ExecuteNonQuery('INSERT INTO inventory (product, stock, price) VALUES (?, ?, ?)', ['Doohickey', '5', '99.99']);

  WriteLn('   Accounts: Alice=$1000, Bob=$500, Charlie=$750, Diana=$2000');
  WriteLn('   Inventory: Widget(100), Gadget(50), Doohickey(5)');
end;

{ Transfers $100 from Alice to Bob within a savepoint, releases it, and commits the outer transaction. }
procedure DemoBasicSavepoint;
var
  CountBefore, CountAfter: Integer;
begin
  WriteLn;
  WriteLn('3. Basic SAVEPOINT / RELEASE');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Insert within transaction
  Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
    ['transfer_start', 'Starting batch transfer']);

  // Create savepoint
  Conn.ExecuteNonQuery('SAVEPOINT sp_transfer');
  WriteLn('   SAVEPOINT sp_transfer created');

  // Transfer $100 from Alice to Bob
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance - 100 WHERE name = ?', ['Alice']);
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance + 100 WHERE name = ?', ['Bob']);
  Conn.ExecuteNonQuery('INSERT INTO transactions (from_account, to_account, amount, status) VALUES (?, ?, ?, ?)',
    ['Alice', 'Bob', '100.00', 'completed']);

  // Release savepoint (commit nested transaction)
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_transfer');
  WriteLn('   RELEASE sp_transfer - Transfer committed within transaction');

  Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
    ['transfer_complete', 'Batch transfer done']);

  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   COMMIT - Outer transaction committed');

  WriteLn('   Balances after:');
  ShowAccounts;
end;

{ Processes three transfers in separate savepoints, rolling back the second on validation failure while keeping the first and third. }
procedure DemoRollbackToSavepoint;
begin
  WriteLn;
  WriteLn('4. ROLLBACK TO SAVEPOINT (Partial Rollback)');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // First transfer - will be kept
  Conn.ExecuteNonQuery('SAVEPOINT sp_first');
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance - 50 WHERE name = ?', ['Bob']);
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance + 50 WHERE name = ?', ['Charlie']);
  Conn.ExecuteNonQuery('INSERT INTO transactions (from_account, to_account, amount, status) VALUES (?, ?, ?, ?)',
    ['Bob', 'Charlie', '50.00', 'completed']);
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_first');
  WriteLn('   Transfer 1: Bob -> Charlie $50 (released)');

  // Second transfer - will be rolled back
  Conn.ExecuteNonQuery('SAVEPOINT sp_second');
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance - 200 WHERE name = ?', ['Charlie']);
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance + 200 WHERE name = ?', ['Diana']);
  WriteLn('   Transfer 2: Charlie -> Diana $200 (in savepoint)');

  // Simulate validation failure - rollback this transfer only
  WriteLn('   Validation failed! Rolling back Transfer 2...');
  Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_second');
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_second');
  WriteLn('   ROLLBACK TO sp_second - Transfer 2 undone');

  // Third transfer - will be kept
  Conn.ExecuteNonQuery('SAVEPOINT sp_third');
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance - 25 WHERE name = ?', ['Alice']);
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance + 25 WHERE name = ?', ['Bob']);
  Conn.ExecuteNonQuery('INSERT INTO transactions (from_account, to_account, amount, status) VALUES (?, ?, ?, ?)',
    ['Alice', 'Bob', '25.00', 'completed']);
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_third');
  WriteLn('   Transfer 3: Alice -> Bob $25 (released)');

  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   COMMIT - Transfers 1 and 3 committed, Transfer 2 was rolled back');
  WriteLn('   Balances after:');
  ShowAccounts;
end;

{ Creates three levels of nested savepoints, rolls back only the innermost level, and commits the outer two levels with their audit entries. }
procedure DemoNestedSavepoints;
begin
  WriteLn;
  WriteLn('5. Nested SAVEPOINTs (3 Levels Deep)');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  WriteLn('   BEGIN TRANSACTION');

  // Level 1
  Conn.ExecuteNonQuery('SAVEPOINT sp_level1');
  Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
    ['level1', 'Level 1 operation']);
  WriteLn('   SAVEPOINT sp_level1 - Inserted audit entry');

  // Level 2
  Conn.ExecuteNonQuery('SAVEPOINT sp_level2');
  Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
    ['level2', 'Level 2 operation']);
  WriteLn('     SAVEPOINT sp_level2 - Inserted audit entry');

  // Level 3
  Conn.ExecuteNonQuery('SAVEPOINT sp_level3');
  Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
    ['level3', 'Level 3 operation - will be rolled back']);
  WriteLn('       SAVEPOINT sp_level3 - Inserted audit entry');

  // Rollback level 3 only
  Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_level3');
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_level3');
  WriteLn('       ROLLBACK TO sp_level3 - Level 3 undone');

  // Release level 2 (keeps its changes)
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_level2');
  WriteLn('     RELEASE sp_level2 - Level 2 kept');

  // Release level 1 (keeps its changes)
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_level1');
  WriteLn('   RELEASE sp_level1 - Level 1 kept');

  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   COMMIT');

  // Show what was kept
  WriteLn(Format('   Audit entries with level1: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM audit_log WHERE action = ''level1'''))]));
  WriteLn(Format('   Audit entries with level2: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM audit_log WHERE action = ''level2'''))]));
  WriteLn(Format('   Audit entries with level3: %d (rolled back)',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM audit_log WHERE action = ''level3'''))]));
end;

{ Processes three product orders in individual savepoints, confirming orders with sufficient stock and rolling back those without. }
procedure DemoOrderProcessing;
var
  Stock: Integer;
begin
  WriteLn;
  WriteLn('6. Order Processing with Savepoints');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Process order 1 - Widget (enough stock)
  Conn.ExecuteNonQuery('SAVEPOINT sp_order1');
  Stock := Conn.ExecuteScalar('SELECT stock FROM inventory WHERE product = ?', ['Widget']);
  if Stock >= 10 then
  begin
    Conn.ExecuteNonQuery('UPDATE inventory SET stock = stock - 10 WHERE product = ?', ['Widget']);
    Conn.ExecuteNonQuery('INSERT INTO orders (customer, product, quantity, total, status) VALUES (?, ?, ?, ?, ?)',
      ['Alice', 'Widget', '10', '99.90', 'confirmed']);
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_order1');
    WriteLn('   Order 1: Alice buys 10 Widgets - CONFIRMED');
  end
  else
  begin
    Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_order1');
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_order1');
    WriteLn('   Order 1: Alice buys 10 Widgets - INSUFFICIENT STOCK');
  end;

  // Process order 2 - Doohickey (not enough stock for 10)
  Conn.ExecuteNonQuery('SAVEPOINT sp_order2');
  Stock := Conn.ExecuteScalar('SELECT stock FROM inventory WHERE product = ?', ['Doohickey']);
  if Stock >= 10 then
  begin
    Conn.ExecuteNonQuery('UPDATE inventory SET stock = stock - 10 WHERE product = ?', ['Doohickey']);
    Conn.ExecuteNonQuery('INSERT INTO orders (customer, product, quantity, total, status) VALUES (?, ?, ?, ?, ?)',
      ['Bob', 'Doohickey', '10', '999.90', 'confirmed']);
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_order2');
    WriteLn('   Order 2: Bob buys 10 Doohickeys - CONFIRMED');
  end
  else
  begin
    Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_order2');
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_order2');
    WriteLn(Format('   Order 2: Bob buys 10 Doohickeys - INSUFFICIENT STOCK (only %d)', [Stock]));
  end;

  // Process order 3 - Gadget (enough stock)
  Conn.ExecuteNonQuery('SAVEPOINT sp_order3');
  Stock := Conn.ExecuteScalar('SELECT stock FROM inventory WHERE product = ?', ['Gadget']);
  if Stock >= 5 then
  begin
    Conn.ExecuteNonQuery('UPDATE inventory SET stock = stock - 5 WHERE product = ?', ['Gadget']);
    Conn.ExecuteNonQuery('INSERT INTO orders (customer, product, quantity, total, status) VALUES (?, ?, ?, ?, ?)',
      ['Charlie', 'Gadget', '5', '124.95', 'confirmed']);
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_order3');
    WriteLn('   Order 3: Charlie buys 5 Gadgets - CONFIRMED');
  end
  else
  begin
    Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_order3');
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_order3');
    WriteLn('   Order 3: Charlie buys 5 Gadgets - INSUFFICIENT STOCK');
  end;

  Conn.ExecuteNonQuery('COMMIT');

  WriteLn('   Inventory after:');
  WriteLn(Format('     Widget: %d (was 100)', [Integer(Conn.ExecuteScalar('SELECT stock FROM inventory WHERE product = ''Widget'''))]));
  WriteLn(Format('     Gadget: %d (was 50)', [Integer(Conn.ExecuteScalar('SELECT stock FROM inventory WHERE product = ''Gadget'''))]));
  WriteLn(Format('     Doohickey: %d (unchanged)', [Integer(Conn.ExecuteScalar('SELECT stock FROM inventory WHERE product = ''Doohickey'''))]));
  WriteLn(Format('   Orders confirmed: %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM orders WHERE status = ''confirmed'''))]));
end;

{ Runs 10 random-amount transfers from Alice to Bob, isolating each in a savepoint and rolling back those with insufficient funds. }
procedure DemoBatchWithErrorHandling;
var
  I, Succeeded, RolledBack: Integer;
  Amount: Double;
begin
  WriteLn;
  WriteLn('7. Batch Processing with Error Isolation');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  Succeeded := 0;
  RolledBack := 0;

  // Process 10 transfers, some will fail (insufficient funds)
  for I := 1 to 10 do
  begin
    Conn.ExecuteNonQuery('SAVEPOINT sp_batch_' + IntToStr(I));

    Amount := 50 + Random(200); // Random amount $50-$250

    // Try to debit Alice
    if Conn.ExecuteScalar('SELECT balance FROM accounts WHERE name = ''Alice''') >= Amount then
    begin
      Conn.ExecuteNonQuery(Format(
        'UPDATE accounts SET balance = balance - %.2f WHERE name = ''Alice''', [Amount]));
      Conn.ExecuteNonQuery(Format(
        'UPDATE accounts SET balance = balance + %.2f WHERE name = ''Bob''', [Amount]));
      Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_batch_' + IntToStr(I));
      Inc(Succeeded);
    end
    else
    begin
      // Insufficient funds - rollback this transfer only
      Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_batch_' + IntToStr(I));
      Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_batch_' + IntToStr(I));
      Inc(RolledBack);
    end;
  end;

  Conn.ExecuteNonQuery('COMMIT');

  WriteLn(Format('   Batch of 10 transfers: %d succeeded, %d rolled back', [Succeeded, RolledBack]));
  WriteLn('   Balances after batch:');
  ShowAccounts;
end;

{ Attempts a large debit in a savepoint, rolls back if balance goes negative, while preserving the audit trail outside the savepoint. }
procedure DemoSavepointWithAudit;
begin
  WriteLn;
  WriteLn('8. Savepoint with Guaranteed Audit Trail');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Audit log is always written (outside savepoint)
  Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
    ['operation_attempt', 'Attempting risky operation']);

  // Risky operation in savepoint
  Conn.ExecuteNonQuery('SAVEPOINT sp_risky');
  Conn.ExecuteNonQuery('UPDATE accounts SET balance = balance - 9999 WHERE name = ?', ['Alice']);
  WriteLn('   Attempted: Debit $9999 from Alice');

  // Check if balance went negative
  if Conn.ExecuteScalar('SELECT balance FROM accounts WHERE name = ''Alice''') < 0 then
  begin
    WriteLn('   Balance would be negative! Rolling back operation...');
    Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_risky');
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_risky');
    Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
      ['operation_failed', 'Insufficient funds - rolled back']);
  end
  else
  begin
    Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_risky');
    Conn.ExecuteNonQuery('INSERT INTO audit_log (action, details) VALUES (?, ?)',
      ['operation_success', 'Operation completed']);
  end;

  Conn.ExecuteNonQuery('COMMIT');
  WriteLn('   Audit trail preserved even though operation was rolled back');
  WriteLn(Format('   Audit entries: %d', [GetCount('audit_log')]));
  WriteLn(Format('   Alice balance unchanged: $%.2f',
    [Double(Conn.ExecuteScalar('SELECT balance FROM accounts WHERE name = ''Alice'''))]));
end;

{ Inserts two transfers in released savepoints then rolls back the entire transaction, discarding all changes including released savepoints. }
procedure DemoRollbackEntireTransaction;
begin
  WriteLn;
  WriteLn('9. Full Transaction Rollback (Savepoints Discarded)');

  WriteLn(Format('   Transactions before: %d', [GetCount('transactions')]));

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  Conn.ExecuteNonQuery('SAVEPOINT sp_a');
  Conn.ExecuteNonQuery('INSERT INTO transactions (from_account, to_account, amount, status) VALUES (?, ?, ?, ?)',
    ['Diana', 'Alice', '500.00', 'pending']);
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_a');
  WriteLn('   Savepoint sp_a released (Diana -> Alice $500)');

  Conn.ExecuteNonQuery('SAVEPOINT sp_b');
  Conn.ExecuteNonQuery('INSERT INTO transactions (from_account, to_account, amount, status) VALUES (?, ?, ?, ?)',
    ['Alice', 'Bob', '300.00', 'pending']);
  Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_b');
  WriteLn('   Savepoint sp_b released (Alice -> Bob $300)');

  // Simulate critical error - rollback everything
  WriteLn('   Critical error detected! Rolling back entire transaction...');
  Conn.ExecuteNonQuery('ROLLBACK');

  WriteLn(Format('   Transactions after rollback: %d (unchanged)', [GetCount('transactions')]));
  WriteLn('   Both released savepoints were discarded by ROLLBACK');
end;

{ Retries an order insertion up to 3 times using the same savepoint name, rolling back failed attempts until the operation succeeds. }
procedure DemoReusableSavepoint;
var
  I, Attempt: Integer;
  Success: Boolean;
begin
  WriteLn;
  WriteLn('10. Reusable Savepoint (Retry Pattern)');

  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Retry pattern: attempt operation up to 3 times
  Success := False;
  for Attempt := 1 to 3 do
  begin
    Conn.ExecuteNonQuery('SAVEPOINT sp_retry');

    // Simulate an operation that might fail
    Conn.ExecuteNonQuery('INSERT INTO orders (customer, product, quantity, total, status) VALUES (?, ?, ?, ?, ?)',
      ['Diana', 'Widget', '1', '9.99', 'pending']);

    // Simulate failure on first 2 attempts
    if Attempt < 3 then
    begin
      WriteLn(Format('   Attempt %d: Failed (simulated error)', [Attempt]));
      Conn.ExecuteNonQuery('ROLLBACK TO SAVEPOINT sp_retry');
      // Note: savepoint still exists after ROLLBACK TO, can be reused
    end
    else
    begin
      WriteLn(Format('   Attempt %d: Succeeded!', [Attempt]));
      Conn.ExecuteNonQuery('RELEASE SAVEPOINT sp_retry');
      Success := True;
    end;
  end;

  if Success then
    Conn.ExecuteNonQuery('COMMIT')
  else
    Conn.ExecuteNonQuery('ROLLBACK');

  WriteLn(Format('   Total orders: %d', [GetCount('orders')]));
end;

{ Prints the final row counts for accounts, transactions, orders, and audit_log tables, along with all account balances. }
procedure ShowSummary;
begin
  WriteLn;
  WriteLn('11. Final Summary');
  WriteLn(Format('   Accounts: %d', [GetCount('accounts')]));
  WriteLn(Format('   Transactions: %d', [GetCount('transactions')]));
  WriteLn(Format('   Orders: %d', [GetCount('orders')]));
  WriteLn(Format('   Audit entries: %d', [GetCount('audit_log')]));
  WriteLn('   Final balances:');
  ShowAccounts;
end;

begin
  Randomize;
  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    Conn.ExecuteNonQuery('PRAGMA journal_mode=WAL');
    Conn.ExecuteNonQuery('PRAGMA foreign_keys=ON');

    CreateSchema;
    SetupData;
    DemoBasicSavepoint;
    DemoRollbackToSavepoint;
    DemoNestedSavepoints;
    DemoOrderProcessing;
    DemoBatchWithErrorHandling;
    DemoSavepointWithAudit;
    DemoRollbackEntireTransaction;
    DemoReusableSavepoint;
    ShowSummary;

    WriteLn;
    WriteLn('=== Example Complete ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
