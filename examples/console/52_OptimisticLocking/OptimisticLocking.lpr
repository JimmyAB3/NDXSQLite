{===============================================================================
  NDXSQLite Example 52 - Optimistic Locking
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Version-based concurrency control
  - Conflict detection on concurrent updates
  - Automatic version increment
  - Retry strategies for conflicts
  - Row-level versioning patterns

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program OptimisticLocking;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  // Products table with version column for optimistic locking
  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL DEFAULT 0,' +
    '  version INTEGER NOT NULL DEFAULT 1,' +
    '  updated_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  // Accounts table with version for balance updates
  Connection.ExecuteNonQuery(
    'CREATE TABLE accounts (' +
    '  id INTEGER PRIMARY KEY,' +
    '  owner TEXT NOT NULL,' +
    '  balance REAL NOT NULL DEFAULT 0,' +
    '  version INTEGER NOT NULL DEFAULT 1' +
    ')');

  // Insert sample data
  Connection.ExecuteNonQuery('INSERT INTO products (name, price, stock) VALUES (''Laptop'', 999.99, 50)');
  Connection.ExecuteNonQuery('INSERT INTO products (name, price, stock) VALUES (''Phone'', 599.99, 100)');
  Connection.ExecuteNonQuery('INSERT INTO products (name, price, stock) VALUES (''Tablet'', 399.99, 75)');

  Connection.ExecuteNonQuery('INSERT INTO accounts (owner, balance) VALUES (''Alice'', 1000.00)');
  Connection.ExecuteNonQuery('INSERT INTO accounts (owner, balance) VALUES (''Bob'', 500.00)');
end;

{ Queries and prints all products with their id, name, price, stock, and version number. }
procedure ShowProducts;
var
  DS: TDataSet;
begin
  WriteLn('   Current products:');
  DS := Connection.ExecuteQuery('SELECT id, name, price, stock, version FROM products ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s - $%.2f, Stock: %d (v%d)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('stock').AsInteger,
         DS.FieldByName('version').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Queries and prints all accounts with their id, owner name, balance, and version number. }
procedure ShowAccounts;
var
  DS: TDataSet;
begin
  WriteLn('   Current accounts:');
  DS := Connection.ExecuteQuery('SELECT id, owner, balance, version FROM accounts ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s - $%.2f (v%d)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('owner').AsString,
         DS.FieldByName('balance').AsFloat,
         DS.FieldByName('version').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Updates a product's price only if the row version matches the expected version, incrementing the version on success. }
function UpdateProductWithVersion(AProductId: Integer; ANewPrice: Double;
  AExpectedVersion: Integer): Boolean;
var
  RowsAffected: Integer;
begin
  // Only update if version matches (optimistic locking)
  RowsAffected := Connection.ExecuteNonQuery(
    'UPDATE products SET price = ?, version = version + 1, updated_at = CURRENT_TIMESTAMP ' +
    'WHERE id = ? AND version = ?',
    [ANewPrice, AProductId, AExpectedVersion]);

  Result := RowsAffected > 0;
end;

{ Adjusts a product's stock by the given quantity if the version matches and the resulting stock is non-negative. }
function UpdateStockWithVersion(AProductId: Integer; AQuantityChange: Integer;
  AExpectedVersion: Integer): Boolean;
var
  RowsAffected: Integer;
begin
  // Update stock with version check
  RowsAffected := Connection.ExecuteNonQuery(
    'UPDATE products SET stock = stock + ?, version = version + 1 ' +
    'WHERE id = ? AND version = ? AND stock + ? >= 0',
    [AQuantityChange, AProductId, AExpectedVersion, AQuantityChange]);

  Result := RowsAffected > 0;
end;

{ Transfers an amount between two accounts within a transaction, checking both account versions before committing. }
function TransferWithVersion(AFromId, AToId: Integer; AAmount: Double;
  AFromVersion, AToVersion: Integer): Boolean;
var
  Rows1, Rows2: Integer;
begin
  Result := False;

  Connection.BeginTransaction;
  try
    // Debit source account with version check
    Rows1 := Connection.ExecuteNonQuery(
      'UPDATE accounts SET balance = balance - ?, version = version + 1 ' +
      'WHERE id = ? AND version = ? AND balance >= ?',
      [AAmount, AFromId, AFromVersion, AAmount]);

    if Rows1 = 0 then
    begin
      Connection.Rollback;
      Exit;
    end;

    // Credit destination account with version check
    Rows2 := Connection.ExecuteNonQuery(
      'UPDATE accounts SET balance = balance + ?, version = version + 1 ' +
      'WHERE id = ? AND version = ?',
      [AAmount, AToId, AToVersion]);

    if Rows2 = 0 then
    begin
      Connection.Rollback;
      Exit;
    end;

    Connection.Commit;
    Result := True;
  except
    Connection.Rollback;
    raise;
  end;
end;

{ Reads a product's current version, updates its price using version-checked update, and shows the result. }
procedure DemoBasicOptimisticLocking;
var
  DS: TDataSet;
  ProductId, CurrentVersion: Integer;
  Success: Boolean;
begin
  WriteLn('1. Basic Optimistic Locking');
  WriteLn('   -------------------------');

  ShowProducts;

  // Read current state
  DS := Connection.ExecuteQuery('SELECT id, price, version FROM products WHERE name = ''Laptop''');
  try
    ProductId := DS.FieldByName('id').AsInteger;
    CurrentVersion := DS.FieldByName('version').AsInteger;
    WriteLn(Format('   Read Laptop: id=%d, version=%d', [ProductId, CurrentVersion]));
  finally
    DS.Free;
  end;

  // Update with correct version
  WriteLn('   Attempting update with correct version...');
  Success := UpdateProductWithVersion(ProductId, 899.99, CurrentVersion);
  if Success then
    WriteLn('   SUCCESS: Price updated to $899.99')
  else
    WriteLn('   CONFLICT: Update failed (version mismatch)');

  ShowProducts;
end;

{ Simulates two users reading the same version, where the first update succeeds and the second detects a conflict. }
procedure DemoConflictDetection;
var
  DS: TDataSet;
  ProductId, Version1, Version2: Integer;
  Success: Boolean;
begin
  WriteLn('2. Conflict Detection (Simulated Concurrent Updates)');
  WriteLn('   --------------------------------------------------');

  // Simulate two users reading the same product
  DS := Connection.ExecuteQuery('SELECT id, price, version FROM products WHERE name = ''Phone''');
  try
    ProductId := DS.FieldByName('id').AsInteger;
    Version1 := DS.FieldByName('version').AsInteger;
    Version2 := DS.FieldByName('version').AsInteger; // Same version for both "users"
    WriteLn(Format('   User A reads Phone: version=%d', [Version1]));
    WriteLn(Format('   User B reads Phone: version=%d', [Version2]));
  finally
    DS.Free;
  end;

  // User A updates first (succeeds)
  WriteLn;
  WriteLn('   User A updates price to $549.99...');
  Success := UpdateProductWithVersion(ProductId, 549.99, Version1);
  if Success then
    WriteLn('   User A: SUCCESS')
  else
    WriteLn('   User A: CONFLICT');

  // User B tries to update with stale version (fails)
  WriteLn('   User B updates price to $579.99...');
  Success := UpdateProductWithVersion(ProductId, 579.99, Version2);
  if Success then
    WriteLn('   User B: SUCCESS')
  else
    WriteLn('   User B: CONFLICT DETECTED - version changed by another user!');

  ShowProducts;
end;

{ Attempts a stock update with re-read-and-retry logic after a simulated concurrent modification causes a conflict. }
procedure DemoRetryStrategy;
var
  DS: TDataSet;
  ProductId, CurrentVersion: Integer;
  MaxRetries, Attempt: Integer;
  Success: Boolean;
  QuantityToDeduct: Integer;
begin
  WriteLn('3. Retry Strategy for Conflicts');
  WriteLn('   -----------------------------');

  MaxRetries := 3;
  QuantityToDeduct := -5; // Sell 5 units

  for Attempt := 1 to MaxRetries do
  begin
    WriteLn(Format('   Attempt %d:', [Attempt]));

    // Re-read current state
    DS := Connection.ExecuteQuery('SELECT id, stock, version FROM products WHERE name = ''Tablet''');
    try
      ProductId := DS.FieldByName('id').AsInteger;
      CurrentVersion := DS.FieldByName('version').AsInteger;
      WriteLn(Format('     Read: stock=%d, version=%d', [DS.FieldByName('stock').AsInteger, CurrentVersion]));
    finally
      DS.Free;
    end;

    // Simulate another user updating between read and write (on first attempt only)
    if Attempt = 1 then
    begin
      WriteLn('     (Simulating concurrent update by another user...)');
      Connection.ExecuteNonQuery(
        'UPDATE products SET stock = stock - 2, version = version + 1 WHERE id = ?',
        [ProductId]);
    end;

    // Try update
    Success := UpdateStockWithVersion(ProductId, QuantityToDeduct, CurrentVersion);
    if Success then
    begin
      WriteLn('     SUCCESS: Stock updated');
      Break;
    end
    else
    begin
      WriteLn('     CONFLICT: Retrying...');
      Sleep(10); // Small delay before retry
    end;
  end;

  if not Success then
    WriteLn('   FAILED: Max retries exceeded');

  ShowProducts;
end;

{ Performs a successful version-checked transfer between accounts, then shows that a stale-version transfer fails. }
procedure DemoTransferWithOptimisticLocking;
var
  DS: TDataSet;
  AliceVersion, BobVersion: Integer;
  Success: Boolean;
begin
  WriteLn('4. Money Transfer with Optimistic Locking');
  WriteLn('   ---------------------------------------');

  ShowAccounts;

  // Read current versions
  DS := Connection.ExecuteQuery('SELECT version FROM accounts WHERE owner = ''Alice''');
  try
    AliceVersion := DS.FieldByName('version').AsInteger;
  finally
    DS.Free;
  end;

  DS := Connection.ExecuteQuery('SELECT version FROM accounts WHERE owner = ''Bob''');
  try
    BobVersion := DS.FieldByName('version').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Alice version: %d, Bob version: %d', [AliceVersion, BobVersion]));
  WriteLn('   Transferring $200 from Alice to Bob...');

  Success := TransferWithVersion(1, 2, 200.00, AliceVersion, BobVersion);
  if Success then
    WriteLn('   SUCCESS: Transfer completed')
  else
    WriteLn('   FAILED: Version conflict or insufficient funds');

  ShowAccounts;

  // Try transfer with stale version (should fail)
  WriteLn('   Attempting transfer with stale version...');
  Success := TransferWithVersion(1, 2, 100.00, AliceVersion, BobVersion); // Old versions
  if Success then
    WriteLn('   SUCCESS: Transfer completed')
  else
    WriteLn('   CONFLICT: Versions have changed since last read');

  WriteLn;
end;

{ Creates a product_history table and trigger, performs price updates, and prints the logged version change history. }
procedure DemoVersionHistoryTrigger;
var
  DS: TDataSet;
begin
  WriteLn('5. Automatic Version History with Trigger');
  WriteLn('   ---------------------------------------');

  // Create version history table
  Connection.ExecuteNonQuery(
    'CREATE TABLE product_history (' +
    '  history_id INTEGER PRIMARY KEY,' +
    '  product_id INTEGER,' +
    '  old_price REAL,' +
    '  new_price REAL,' +
    '  old_version INTEGER,' +
    '  new_version INTEGER,' +
    '  changed_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  // Create trigger to log version changes
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER log_product_changes ' +
    'AFTER UPDATE ON products ' +
    'WHEN OLD.version <> NEW.version ' +
    'BEGIN ' +
    '  INSERT INTO product_history (product_id, old_price, new_price, old_version, new_version) ' +
    '  VALUES (OLD.id, OLD.price, NEW.price, OLD.version, NEW.version); ' +
    'END');

  WriteLn('   Trigger created for version history');

  // Make some updates
  Connection.ExecuteNonQuery(
    'UPDATE products SET price = 879.99, version = version + 1 WHERE name = ''Laptop''');
  Connection.ExecuteNonQuery(
    'UPDATE products SET price = 869.99, version = version + 1 WHERE name = ''Laptop''');

  // Show history
  WriteLn('   Version history for Laptop:');
  DS := Connection.ExecuteQuery(
    'SELECT old_price, new_price, old_version, new_version, changed_at ' +
    'FROM product_history WHERE product_id = 1 ORDER BY history_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     v%d -> v%d: $%.2f -> $%.2f (%s)',
        [DS.FieldByName('old_version').AsInteger,
         DS.FieldByName('new_version').AsInteger,
         DS.FieldByName('old_price').AsFloat,
         DS.FieldByName('new_price').AsFloat,
         DS.FieldByName('changed_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints a list of optimistic locking best practices and guidelines for concurrency handling. }
procedure DemoBestPractices;
begin
  WriteLn('6. Optimistic Locking Best Practices');
  WriteLn('   ----------------------------------');
  WriteLn('   - Always include version column in tables with concurrent updates');
  WriteLn('   - Check affected rows count after UPDATE');
  WriteLn('   - Implement retry logic for transient conflicts');
  WriteLn('   - Use transactions for multi-table updates');
  WriteLn('   - Consider timestamp-based versioning for audit trails');
  WriteLn('   - Keep read-modify-write cycles short');
  WriteLn('   - Use SELECT ... FOR UPDATE in databases that support it');
  WriteLn('   - Log conflicts for monitoring and debugging');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 52: Optimistic Locking ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example52.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      DemoBasicOptimisticLocking;
      DemoConflictDetection;
      DemoRetryStrategy;
      DemoTransferWithOptimisticLocking;
      DemoVersionHistoryTrigger;
      DemoBestPractices;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
