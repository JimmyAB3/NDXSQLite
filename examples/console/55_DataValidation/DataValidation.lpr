{===============================================================================
  NDXSQLite Example 55 - Data Validation
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - CHECK constraints for column-level validation
  - Triggers for complex validation rules
  - Custom validators with RAISE(ABORT)
  - Foreign key constraints
  - Unique constraints and indexes
  - Validation error handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataValidation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  // Enable foreign keys
  Connection.ExecuteNonQuery('PRAGMA foreign_keys = ON');

  // Users table with various CHECK constraints
  Connection.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT NOT NULL UNIQUE,' +
    '  email TEXT NOT NULL,' +
    '  age INTEGER,' +
    '  status TEXT DEFAULT ''active'',' +
    '  created_at TEXT DEFAULT CURRENT_TIMESTAMP,' +
    '  CHECK (length(username) >= 3),' +
    '  CHECK (length(username) <= 50),' +
    '  CHECK (username NOT GLOB ''*[^a-zA-Z0-9_]*''),' +
    '  CHECK (email LIKE ''%@%.%''),' +
    '  CHECK (age IS NULL OR (age >= 0 AND age <= 150)),' +
    '  CHECK (status IN (''active'', ''inactive'', ''banned''))' +
    ')');

  // Products table with price and quantity validation
  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  sku TEXT NOT NULL UNIQUE,' +
    '  name TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  quantity INTEGER NOT NULL DEFAULT 0,' +
    '  category_id INTEGER,' +
    '  CHECK (length(sku) = 8),' +
    '  CHECK (price > 0),' +
    '  CHECK (quantity >= 0),' +
    '  CHECK (price <= 999999.99),' +
    '  FOREIGN KEY (category_id) REFERENCES categories(id)' +
    ')');

  // Categories for FK validation
  Connection.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL UNIQUE' +
    ')');

  // Orders with date validation
  Connection.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL,' +
    '  order_date TEXT NOT NULL,' +
    '  total REAL NOT NULL,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  CHECK (order_date GLOB ''[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*''),' +
    '  CHECK (total >= 0),' +
    '  CHECK (status IN (''pending'', ''processing'', ''shipped'', ''delivered'', ''cancelled'')),' +
    '  FOREIGN KEY (user_id) REFERENCES users(id)' +
    ')');

  // Insert sample categories
  Connection.ExecuteNonQuery('INSERT INTO categories (name) VALUES (''Electronics'')');
  Connection.ExecuteNonQuery('INSERT INTO categories (name) VALUES (''Clothing'')');
  Connection.ExecuteNonQuery('INSERT INTO categories (name) VALUES (''Books'')');
end;

{ Creates BEFORE INSERT/UPDATE triggers that enforce email domain rules, price change limits, stock-level alerts, order status transitions, and reserved username checks. }
procedure SetupValidationTriggers;
begin
  // Trigger: Validate email domain (complex validation)
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER validate_email_domain ' +
    'BEFORE INSERT ON users ' +
    'BEGIN ' +
    '  SELECT CASE ' +
    '    WHEN NEW.email LIKE ''%@temp.%'' OR NEW.email LIKE ''%@fake.%'' THEN ' +
    '      RAISE(ABORT, ''Temporary or fake email domains are not allowed'') ' +
    '  END; ' +
    'END');

  // Trigger: Validate email on update too
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER validate_email_domain_update ' +
    'BEFORE UPDATE OF email ON users ' +
    'BEGIN ' +
    '  SELECT CASE ' +
    '    WHEN NEW.email LIKE ''%@temp.%'' OR NEW.email LIKE ''%@fake.%'' THEN ' +
    '      RAISE(ABORT, ''Temporary or fake email domains are not allowed'') ' +
    '  END; ' +
    'END');

  // Trigger: Prevent price decrease of more than 50%
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER validate_price_change ' +
    'BEFORE UPDATE OF price ON products ' +
    'WHEN NEW.price < OLD.price * 0.5 ' +
    'BEGIN ' +
    '  SELECT RAISE(ABORT, ''Price cannot be reduced by more than 50%''); ' +
    'END');

  // Trigger: Auto-update status when quantity becomes 0
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER check_stock_level ' +
    'AFTER UPDATE OF quantity ON products ' +
    'WHEN NEW.quantity = 0 ' +
    'BEGIN ' +
    '  UPDATE products SET name = name || '' [OUT OF STOCK]'' WHERE id = NEW.id ' +
    '    AND name NOT LIKE ''%[OUT OF STOCK]%''; ' +
    'END');

  // Trigger: Validate order total matches calculation
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER validate_order_status_transition ' +
    'BEFORE UPDATE OF status ON orders ' +
    'BEGIN ' +
    '  SELECT CASE ' +
    '    WHEN OLD.status = ''cancelled'' AND NEW.status <> ''cancelled'' THEN ' +
    '      RAISE(ABORT, ''Cannot change status of cancelled order'') ' +
    '    WHEN OLD.status = ''delivered'' AND NEW.status NOT IN (''delivered'', ''cancelled'') THEN ' +
    '      RAISE(ABORT, ''Delivered order can only be cancelled'') ' +
    '  END; ' +
    'END');

  // Trigger: Validate username not reserved
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER validate_username_reserved ' +
    'BEFORE INSERT ON users ' +
    'BEGIN ' +
    '  SELECT CASE ' +
    '    WHEN lower(NEW.username) IN (''admin'', ''root'', ''system'', ''null'', ''undefined'') THEN ' +
    '      RAISE(ABORT, ''This username is reserved'') ' +
    '  END; ' +
    'END');
end;

{ Attempts a parameterized insert and returns whether it succeeded. }
function TryInsert(const ASQL: string; const AParams: array of Variant): Boolean;
begin
  Result := True;
  try
    Connection.ExecuteNonQuery(ASQL, AParams);
  except
    on E: Exception do
    begin
      Result := False;
      WriteLn('     ERROR: ', E.Message);
    end;
  end;
end;

{ Attempts to execute a SQL statement and returns whether it succeeded. }
function TryExecute(const ASQL: string): Boolean;
begin
  Result := True;
  try
    Connection.ExecuteNonQuery(ASQL);
  except
    on E: Exception do
    begin
      Result := False;
      WriteLn('     ERROR: ', E.Message);
    end;
  end;
end;

{ Tests CHECK constraints by inserting valid and invalid users (short username, bad email, negative age, invalid status). }
procedure DemoCheckConstraints;
begin
  WriteLn('1. CHECK Constraints');
  WriteLn('   ------------------');

  // Valid user
  WriteLn('   Inserting valid user (john_doe)...');
  if TryInsert('INSERT INTO users (username, email, age, status) VALUES (?, ?, ?, ?)',
    ['john_doe', 'john@example.com', 25, 'active']) then
    WriteLn('     SUCCESS');

  // Username too short
  WriteLn('   Inserting user with short username (ab)...');
  TryInsert('INSERT INTO users (username, email) VALUES (?, ?)',
    ['ab', 'ab@example.com']);

  // Invalid email format
  WriteLn('   Inserting user with invalid email...');
  TryInsert('INSERT INTO users (username, email) VALUES (?, ?)',
    ['validuser', 'not-an-email']);

  // Invalid age
  WriteLn('   Inserting user with negative age...');
  TryInsert('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',
    ['testuser1', 'test@example.com', -5]);

  // Invalid status
  WriteLn('   Inserting user with invalid status...');
  TryInsert('INSERT INTO users (username, email, status) VALUES (?, ?, ?)',
    ['testuser2', 'test2@example.com', 'invalid_status']);

  // Username with special characters
  WriteLn('   Inserting user with special chars (user@name)...');
  TryInsert('INSERT INTO users (username, email) VALUES (?, ?)',
    ['user@name', 'special@example.com']);

  WriteLn;
end;

{ Tests UNIQUE constraints by attempting duplicate username and duplicate SKU inserts. }
procedure DemoUniqueConstraints;
begin
  WriteLn('2. UNIQUE Constraints');
  WriteLn('   -------------------');

  // Insert first user
  WriteLn('   Inserting user (alice_smith)...');
  if TryInsert('INSERT INTO users (username, email) VALUES (?, ?)',
    ['alice_smith', 'alice@example.com']) then
    WriteLn('     SUCCESS');

  // Try duplicate username
  WriteLn('   Inserting duplicate username (alice_smith)...');
  TryInsert('INSERT INTO users (username, email) VALUES (?, ?)',
    ['alice_smith', 'alice2@example.com']);

  // Insert product with unique SKU
  WriteLn('   Inserting product (SKU: PROD0001)...');
  if TryInsert('INSERT INTO products (sku, name, price, category_id) VALUES (?, ?, ?, ?)',
    ['PROD0001', 'Laptop', 999.99, 1]) then
    WriteLn('     SUCCESS');

  // Try duplicate SKU
  WriteLn('   Inserting duplicate SKU (PROD0001)...');
  TryInsert('INSERT INTO products (sku, name, price, category_id) VALUES (?, ?, ?, ?)',
    ['PROD0001', 'Another Laptop', 899.99, 1]);

  WriteLn;
end;

{ Tests FOREIGN KEY enforcement by inserting products and orders with both valid and invalid reference IDs. }
procedure DemoForeignKeyConstraints;
begin
  WriteLn('3. Foreign Key Constraints');
  WriteLn('   ------------------------');

  // Valid FK reference
  WriteLn('   Inserting product with valid category (Electronics)...');
  if TryInsert('INSERT INTO products (sku, name, price, category_id) VALUES (?, ?, ?, ?)',
    ['PROD0002', 'Phone', 599.99, 1]) then
    WriteLn('     SUCCESS');

  // Invalid FK reference
  WriteLn('   Inserting product with invalid category (ID=999)...');
  TryInsert('INSERT INTO products (sku, name, price, category_id) VALUES (?, ?, ?, ?)',
    ['PROD0003', 'Mystery Item', 99.99, 999]);

  // Insert valid user for order test
  Connection.ExecuteNonQuery('INSERT INTO users (username, email) VALUES (?, ?)',
    ['bob_jones', 'bob@example.com']);

  // Valid order
  WriteLn('   Inserting order with valid user...');
  if TryInsert('INSERT INTO orders (user_id, order_date, total) VALUES (?, ?, ?)',
    [1, '2024-01-15', 150.00]) then
    WriteLn('     SUCCESS');

  // Invalid user reference
  WriteLn('   Inserting order with invalid user (ID=999)...');
  TryInsert('INSERT INTO orders (user_id, order_date, total) VALUES (?, ?, ?)',
    [999, '2024-01-15', 100.00]);

  WriteLn;
end;

{ Tests trigger-based validation: reserved usernames, temporary email domains, and excessive price reductions. }
procedure DemoTriggerValidation;
begin
  WriteLn('4. Trigger-Based Validation');
  WriteLn('   -------------------------');

  // Reserved username
  WriteLn('   Inserting reserved username (admin)...');
  TryExecute('INSERT INTO users (username, email) VALUES (''admin'', ''admin@example.com'')');

  // Temporary email domain
  WriteLn('   Inserting user with temp email...');
  TryExecute('INSERT INTO users (username, email) VALUES (''tempuser'', ''user@temp.mail'')');

  // Valid user for price test
  WriteLn('   Inserting valid user for tests...');
  Connection.ExecuteNonQuery('INSERT INTO users (username, email) VALUES (''validuser'', ''valid@example.com'')');

  // Insert product for price test
  WriteLn('   Inserting product for price validation test...');
  Connection.ExecuteNonQuery(
    'INSERT INTO products (sku, name, price, category_id) VALUES (''PROD0004'', ''TestItem'', 100.00, 1)');

  // Try to reduce price by more than 50%
  WriteLn('   Reducing price by 60% (from $100 to $40)...');
  TryExecute('UPDATE products SET price = 40.00 WHERE sku = ''PROD0004''');

  // Valid price reduction (40%)
  WriteLn('   Reducing price by 40% (from $100 to $60)...');
  if TryExecute('UPDATE products SET price = 60.00 WHERE sku = ''PROD0004''') then
    WriteLn('     SUCCESS');

  WriteLn;
end;

{ Tests order status transition rules, verifying valid progressions succeed and invalid ones are rejected by triggers. }
procedure DemoStatusTransitionValidation;
begin
  WriteLn('5. Status Transition Validation');
  WriteLn('   -----------------------------');

  // Create an order
  WriteLn('   Creating test order...');
  Connection.ExecuteNonQuery(
    'INSERT INTO orders (user_id, order_date, total, status) VALUES (1, ''2024-01-20'', 200.00, ''pending'')');

  // Valid transition: pending -> processing
  WriteLn('   Transitioning: pending -> processing...');
  if TryExecute('UPDATE orders SET status = ''processing'' WHERE id = 2') then
    WriteLn('     SUCCESS');

  // Valid transition: processing -> shipped
  WriteLn('   Transitioning: processing -> shipped...');
  if TryExecute('UPDATE orders SET status = ''shipped'' WHERE id = 2') then
    WriteLn('     SUCCESS');

  // Valid transition: shipped -> delivered
  WriteLn('   Transitioning: shipped -> delivered...');
  if TryExecute('UPDATE orders SET status = ''delivered'' WHERE id = 2') then
    WriteLn('     SUCCESS');

  // Invalid: delivered -> pending
  WriteLn('   Transitioning: delivered -> pending (invalid)...');
  TryExecute('UPDATE orders SET status = ''pending'' WHERE id = 2');

  // Create another order and cancel it
  Connection.ExecuteNonQuery(
    'INSERT INTO orders (user_id, order_date, total, status) VALUES (1, ''2024-01-21'', 50.00, ''cancelled'')');

  // Invalid: cancelled -> processing
  WriteLn('   Transitioning: cancelled -> processing (invalid)...');
  TryExecute('UPDATE orders SET status = ''processing'' WHERE id = 3');

  WriteLn;
end;

{ Creates a validation_rules table with regex, range, and format rules, then displays them as a dynamic validation catalog. }
procedure DemoComplexValidation;
var
  DS: TDataSet;
begin
  WriteLn('6. Complex Validation Patterns');
  WriteLn('   ----------------------------');

  // Create a validation rules table
  Connection.ExecuteNonQuery(
    'CREATE TABLE validation_rules (' +
    '  id INTEGER PRIMARY KEY,' +
    '  table_name TEXT NOT NULL,' +
    '  field_name TEXT NOT NULL,' +
    '  rule_type TEXT NOT NULL,' +      // 'regex', 'range', 'list', 'custom'
    '  rule_value TEXT NOT NULL,' +
    '  error_message TEXT NOT NULL,' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Insert sample validation rules
  Connection.ExecuteNonQuery(
    'INSERT INTO validation_rules (table_name, field_name, rule_type, rule_value, error_message) VALUES ' +
    '(''users'', ''username'', ''regex'', ''^[a-z][a-z0-9_]{2,29}$'', ''Username must start with letter, 3-30 chars'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO validation_rules (table_name, field_name, rule_type, rule_value, error_message) VALUES ' +
    '(''users'', ''email'', ''regex'', ''^[^@]+@[^@]+\.[^@]+$'', ''Invalid email format'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO validation_rules (table_name, field_name, rule_type, rule_value, error_message) VALUES ' +
    '(''products'', ''price'', ''range'', ''0.01-999999.99'', ''Price must be between 0.01 and 999999.99'')');

  WriteLn('   Validation rules stored in database:');
  DS := Connection.ExecuteQuery('SELECT table_name, field_name, rule_type, rule_value FROM validation_rules');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s.%s: %s = %s',
        [DS.FieldByName('table_name').AsString,
         DS.FieldByName('field_name').AsString,
         DS.FieldByName('rule_type').AsString,
         DS.FieldByName('rule_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   These rules can be queried at runtime for dynamic validation.');
  WriteLn;
end;

{ Reduces product stock to zero triggering the auto-label trigger, then verifies the negative stock CHECK constraint. }
procedure DemoStockValidation;
var
  DS: TDataSet;
begin
  WriteLn('7. Automatic Stock Level Validation');
  WriteLn('   ----------------------------------');

  // Insert product with stock
  Connection.ExecuteNonQuery(
    'INSERT INTO products (sku, name, price, quantity, category_id) VALUES ' +
    '(''PROD0005'', ''Limited Edition'', 199.99, 5, 1)');

  WriteLn('   Initial product:');
  DS := Connection.ExecuteQuery('SELECT name, quantity FROM products WHERE sku = ''PROD0005''');
  try
    WriteLn(Format('     %s - Qty: %d', [DS.FieldByName('name').AsString, DS.FieldByName('quantity').AsInteger]));
  finally
    DS.Free;
  end;

  // Reduce stock to 0
  WriteLn('   Reducing stock to 0...');
  Connection.ExecuteNonQuery('UPDATE products SET quantity = 0 WHERE sku = ''PROD0005''');

  WriteLn('   After stock depletion (trigger auto-updates name):');
  DS := Connection.ExecuteQuery('SELECT name, quantity FROM products WHERE sku = ''PROD0005''');
  try
    WriteLn(Format('     %s - Qty: %d', [DS.FieldByName('name').AsString, DS.FieldByName('quantity').AsInteger]));
  finally
    DS.Free;
  end;

  // Try negative stock
  WriteLn('   Trying to set negative stock...');
  TryExecute('UPDATE products SET quantity = -1 WHERE sku = ''PROD0005''');

  WriteLn;
end;

{ Prints a summary of data validation best practices covering constraints, triggers, and testing strategies. }
procedure DemoBestPractices;
begin
  WriteLn('8. Data Validation Best Practices');
  WriteLn('   --------------------------------');
  WriteLn('   - Use CHECK constraints for simple value validation');
  WriteLn('   - Use UNIQUE constraints for business keys');
  WriteLn('   - Enable PRAGMA foreign_keys = ON for referential integrity');
  WriteLn('   - Use triggers for complex multi-field validation');
  WriteLn('   - Store validation rules in tables for dynamic validation');
  WriteLn('   - Use RAISE(ABORT, message) for custom error messages');
  WriteLn('   - Validate on both INSERT and UPDATE (separate triggers)');
  WriteLn('   - Test edge cases: NULL, empty strings, boundary values');
  WriteLn('   - Log validation failures for debugging');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 55: Data Validation ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example55.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      SetupValidationTriggers;
      DemoCheckConstraints;
      DemoUniqueConstraints;
      DemoForeignKeyConstraints;
      DemoTriggerValidation;
      DemoStatusTransitionValidation;
      DemoComplexValidation;
      DemoStockValidation;
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
