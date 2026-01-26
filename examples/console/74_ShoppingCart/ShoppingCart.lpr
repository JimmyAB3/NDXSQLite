{===============================================================================
  NDXSQLite Example 74 - Shopping Cart with Stock Management
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  A complete e-commerce shopping cart implementation demonstrating:
  - Product catalog with inventory tracking
  - Cart management (add, update, remove items)
  - Stock reservation and validation
  - Price calculations with discounts
  - Order processing with transaction safety
  - Inventory updates on checkout
  - Cart abandonment handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program ShoppingCart;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating E-Commerce Schema');
  WriteLn('   ===========================');

  // Products table
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  sku TEXT UNIQUE NOT NULL,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  price REAL NOT NULL,' +
    '  cost REAL,' +
    '  category TEXT,' +
    '  weight REAL,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Inventory table (stock levels)
  Conn.ExecuteNonQuery(
    'CREATE TABLE inventory (' +
    '  id INTEGER PRIMARY KEY,' +
    '  product_id INTEGER UNIQUE NOT NULL REFERENCES products(id),' +
    '  quantity_available INTEGER DEFAULT 0,' +
    '  quantity_reserved INTEGER DEFAULT 0,' +
    '  reorder_level INTEGER DEFAULT 10,' +
    '  max_stock INTEGER DEFAULT 100,' +
    '  last_restocked TEXT' +
    ')');

  // Customers table
  Conn.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  email TEXT UNIQUE NOT NULL,' +
    '  name TEXT NOT NULL,' +
    '  phone TEXT,' +
    '  shipping_address TEXT,' +
    '  billing_address TEXT,' +
    '  loyalty_points INTEGER DEFAULT 0,' +
    '  tier TEXT DEFAULT ''standard'' CHECK (tier IN (''standard'', ''silver'', ''gold'', ''platinum'')),' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Shopping carts table
  Conn.ExecuteNonQuery(
    'CREATE TABLE carts (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER REFERENCES customers(id),' +
    '  session_id TEXT,' +
    '  status TEXT DEFAULT ''active'' CHECK (status IN (''active'', ''abandoned'', ''converted'', ''merged'')),' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT' +
    ')');

  // Cart items table
  Conn.ExecuteNonQuery(
    'CREATE TABLE cart_items (' +
    '  id INTEGER PRIMARY KEY,' +
    '  cart_id INTEGER NOT NULL REFERENCES carts(id) ON DELETE CASCADE,' +
    '  product_id INTEGER NOT NULL REFERENCES products(id),' +
    '  quantity INTEGER NOT NULL DEFAULT 1,' +
    '  unit_price REAL NOT NULL,' +
    '  discount_percent REAL DEFAULT 0,' +
    '  added_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(cart_id, product_id)' +
    ')');

  // Discount codes table
  Conn.ExecuteNonQuery(
    'CREATE TABLE discount_codes (' +
    '  id INTEGER PRIMARY KEY,' +
    '  code TEXT UNIQUE NOT NULL,' +
    '  discount_type TEXT CHECK (discount_type IN (''percent'', ''fixed'', ''shipping'')),' +
    '  discount_value REAL NOT NULL,' +
    '  min_order_amount REAL DEFAULT 0,' +
    '  max_uses INTEGER,' +
    '  used_count INTEGER DEFAULT 0,' +
    '  valid_from TEXT,' +
    '  valid_until TEXT,' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Orders table
  Conn.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_id INTEGER NOT NULL REFERENCES customers(id),' +
    '  cart_id INTEGER REFERENCES carts(id),' +
    '  order_number TEXT UNIQUE NOT NULL,' +
    '  status TEXT DEFAULT ''pending'' CHECK (status IN (''pending'', ''paid'', ''processing'', ''shipped'', ''delivered'', ''cancelled'', ''refunded'')),' +
    '  subtotal REAL NOT NULL,' +
    '  discount_amount REAL DEFAULT 0,' +
    '  tax_amount REAL DEFAULT 0,' +
    '  shipping_amount REAL DEFAULT 0,' +
    '  total_amount REAL NOT NULL,' +
    '  discount_code TEXT,' +
    '  shipping_address TEXT,' +
    '  payment_method TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  paid_at TEXT,' +
    '  shipped_at TEXT,' +
    '  delivered_at TEXT' +
    ')');

  // Order items table
  Conn.ExecuteNonQuery(
    'CREATE TABLE order_items (' +
    '  id INTEGER PRIMARY KEY,' +
    '  order_id INTEGER NOT NULL REFERENCES orders(id) ON DELETE CASCADE,' +
    '  product_id INTEGER NOT NULL REFERENCES products(id),' +
    '  product_name TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  unit_price REAL NOT NULL,' +
    '  discount_percent REAL DEFAULT 0,' +
    '  line_total REAL NOT NULL' +
    ')');

  // Inventory transactions log
  Conn.ExecuteNonQuery(
    'CREATE TABLE inventory_transactions (' +
    '  id INTEGER PRIMARY KEY,' +
    '  product_id INTEGER NOT NULL REFERENCES products(id),' +
    '  transaction_type TEXT CHECK (transaction_type IN (''restock'', ''sale'', ''reserve'', ''release'', ''adjustment'', ''return'')),' +
    '  quantity INTEGER NOT NULL,' +
    '  reference_type TEXT,' +
    '  reference_id INTEGER,' +
    '  notes TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_products_category ON products(category)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_products_active ON products(is_active)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_carts_customer ON carts(customer_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_carts_status ON carts(status)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_cart_items_cart ON cart_items(cart_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_customer ON orders(customer_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_orders_status ON orders(status)');

  WriteLn('   Created tables: products, inventory, customers, carts, cart_items,');
  WriteLn('                   discount_codes, orders, order_items, inventory_transactions');
  WriteLn('');
end;

// =============================================================================
// Populate Sample Data
// =============================================================================
{ Inserts products with inventory levels, customers with loyalty tiers, and discount codes into the database for e-commerce testing. }
procedure PopulateSampleData;
begin
  WriteLn('2. Populating Sample Data');
  WriteLn('   =======================');

  // Products
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''LAPTOP-001'', ''Pro Laptop 15"'', ''High-performance laptop with 16GB RAM'', 1299.99, 850.00, ''Electronics'', 2.1)');
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''PHONE-001'', ''SmartPhone X'', ''Latest smartphone with 128GB storage'', 899.99, 550.00, ''Electronics'', 0.2)');
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''HEADPHONES-001'', ''Wireless Headphones'', ''Noise-cancelling bluetooth headphones'', 249.99, 120.00, ''Electronics'', 0.3)');
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''TSHIRT-001'', ''Classic T-Shirt'', ''100% cotton comfortable t-shirt'', 24.99, 8.00, ''Clothing'', 0.2)');
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''JEANS-001'', ''Slim Fit Jeans'', ''Modern slim fit denim jeans'', 79.99, 25.00, ''Clothing'', 0.5)');
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''BOOK-001'', ''Programming Guide'', ''Complete guide to modern programming'', 49.99, 15.00, ''Books'', 0.8)');
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''WATCH-001'', ''Smart Watch'', ''Fitness tracking smartwatch'', 299.99, 150.00, ''Electronics'', 0.1)');
  Conn.ExecuteNonQuery('INSERT INTO products (sku, name, description, price, cost, category, weight) VALUES ' +
    '(''CHARGER-001'', ''USB-C Charger'', ''Fast charging USB-C adapter'', 29.99, 10.00, ''Accessories'', 0.1)');

  // Inventory
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (1, 25, 5, 50)');
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (2, 50, 10, 100)');
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (3, 30, 5, 60)');
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (4, 100, 20, 200)');
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (5, 75, 15, 150)');
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (6, 40, 10, 80)');
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (7, 3, 5, 30)');  // Low stock
  Conn.ExecuteNonQuery('INSERT INTO inventory (product_id, quantity_available, reorder_level, max_stock) VALUES (8, 200, 50, 500)');

  // Customers
  Conn.ExecuteNonQuery('INSERT INTO customers (email, name, phone, shipping_address, tier, loyalty_points) VALUES ' +
    '(''alice@example.com'', ''Alice Johnson'', ''555-0101'', ''123 Main St, City, ST 12345'', ''gold'', 2500)');
  Conn.ExecuteNonQuery('INSERT INTO customers (email, name, phone, shipping_address, tier, loyalty_points) VALUES ' +
    '(''bob@example.com'', ''Bob Smith'', ''555-0102'', ''456 Oak Ave, Town, ST 67890'', ''silver'', 1200)');
  Conn.ExecuteNonQuery('INSERT INTO customers (email, name, phone, shipping_address, tier, loyalty_points) VALUES ' +
    '(''carol@example.com'', ''Carol White'', ''555-0103'', ''789 Pine Rd, Village, ST 11223'', ''standard'', 300)');

  // Discount codes
  Conn.ExecuteNonQuery('INSERT INTO discount_codes (code, discount_type, discount_value, min_order_amount, max_uses, valid_until) VALUES ' +
    '(''SAVE10'', ''percent'', 10, 50, 100, ''2027-12-31'')');
  Conn.ExecuteNonQuery('INSERT INTO discount_codes (code, discount_type, discount_value, min_order_amount, max_uses, valid_until) VALUES ' +
    '(''FLAT20'', ''fixed'', 20, 100, 50, ''2027-12-31'')');
  Conn.ExecuteNonQuery('INSERT INTO discount_codes (code, discount_type, discount_value, min_order_amount, valid_until) VALUES ' +
    '(''FREESHIP'', ''shipping'', 100, 75, ''2027-12-31'')');
  Conn.ExecuteNonQuery('INSERT INTO discount_codes (code, discount_type, discount_value, min_order_amount, is_active) VALUES ' +
    '(''EXPIRED'', ''percent'', 25, 0, 0)');  // Inactive code

  WriteLn('   Created 8 products with inventory');
  WriteLn('   Created 3 customers (gold, silver, standard tiers)');
  WriteLn('   Created 4 discount codes');
  WriteLn('');
end;

// =============================================================================
// Cart Management Functions
// =============================================================================
{ Creates a new shopping cart for the given customer and session, setting a 24-hour expiration, and returns the cart ID. }
function CreateCart(CustomerId: Integer; const SessionId: string): Integer;
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO carts (customer_id, session_id, expires_at) VALUES (?, ?, datetime(''now'', ''+24 hours''))',
    [CustomerId, SessionId]);
  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');
end;

{ Adds a product to the cart after verifying stock availability, reserves the inventory, and logs the transaction. Returns True on success. }
function AddToCart(CartId, ProductId, Quantity: Integer): Boolean;
var
  Available, CurrentQty: Integer;
  Price: Double;
  V: Variant;
begin
  Result := False;

  // Check stock availability
  V := Conn.ExecuteScalar(
    'SELECT COALESCE(quantity_available - quantity_reserved, 0) FROM inventory WHERE product_id = ?', [ProductId]);
  if VarIsNull(V) then
    Available := 0
  else
    Available := V;

  if Available < Quantity then
  begin
    WriteLn(Format('     ERROR: Insufficient stock. Available: %d, Requested: %d', [Available, Quantity]));
    Exit;
  end;

  // Get product price
  Price := Conn.ExecuteScalar('SELECT price FROM products WHERE id = ?', [ProductId]);

  // Check if item already in cart
  V := Conn.ExecuteScalar(
    'SELECT quantity FROM cart_items WHERE cart_id = ? AND product_id = ?', [CartId, ProductId]);
  if VarIsNull(V) then
    CurrentQty := 0
  else
    CurrentQty := V;

  if CurrentQty > 0 then
  begin
    // Update existing item
    Conn.ExecuteNonQuery(
      'UPDATE cart_items SET quantity = quantity + ?, updated_at = datetime(''now'') WHERE cart_id = ? AND product_id = ?',
      [Quantity, CartId, ProductId]);
  end
  else
  begin
    // Add new item
    Conn.ExecuteNonQuery(
      'INSERT INTO cart_items (cart_id, product_id, quantity, unit_price) VALUES (?, ?, ?, ?)',
      [CartId, ProductId, Quantity, Price]);
  end;

  // Reserve stock
  Conn.ExecuteNonQuery(
    'UPDATE inventory SET quantity_reserved = quantity_reserved + ? WHERE product_id = ?',
    [Quantity, ProductId]);

  // Log inventory transaction
  Conn.ExecuteNonQuery(
    'INSERT INTO inventory_transactions (product_id, transaction_type, quantity, reference_type, reference_id) ' +
    'VALUES (?, ''reserve'', ?, ''cart'', ?)', [ProductId, Quantity, CartId]);

  Result := True;
end;

{ Changes the quantity of an existing cart item, checking stock availability for increases and adjusting the reserved inventory accordingly. }
procedure UpdateCartItem(CartId, ProductId, NewQuantity: Integer);
var
  CurrentQty, Available, Diff: Integer;
  V: Variant;
begin
  V := Conn.ExecuteScalar(
    'SELECT quantity FROM cart_items WHERE cart_id = ? AND product_id = ?', [CartId, ProductId]);
  if VarIsNull(V) then
    Exit;
  CurrentQty := V;

  Diff := NewQuantity - CurrentQty;

  if Diff > 0 then
  begin
    // Increasing quantity - check availability
    V := Conn.ExecuteScalar(
      'SELECT quantity_available - quantity_reserved FROM inventory WHERE product_id = ?', [ProductId]);
    if VarIsNull(V) then
      Available := 0
    else
      Available := V;

    if Available < Diff then
    begin
      WriteLn(Format('     ERROR: Cannot increase quantity. Only %d more available.', [Available]));
      Exit;
    end;
  end;

  // Update cart item
  Conn.ExecuteNonQuery(
    'UPDATE cart_items SET quantity = ? WHERE cart_id = ? AND product_id = ?',
    [NewQuantity, CartId, ProductId]);

  // Update reserved stock
  Conn.ExecuteNonQuery(
    'UPDATE inventory SET quantity_reserved = quantity_reserved + ? WHERE product_id = ?',
    [Diff, ProductId]);
end;

{ Removes a product from the cart, releases its reserved inventory, and logs the release transaction. }
procedure RemoveFromCart(CartId, ProductId: Integer);
var
  Quantity: Integer;
begin
  // Get current quantity
  Quantity := Conn.ExecuteScalar(
    'SELECT quantity FROM cart_items WHERE cart_id = ? AND product_id = ?', [CartId, ProductId]);

  // Remove item
  Conn.ExecuteNonQuery(
    'DELETE FROM cart_items WHERE cart_id = ? AND product_id = ?', [CartId, ProductId]);

  // Release reserved stock
  Conn.ExecuteNonQuery(
    'UPDATE inventory SET quantity_reserved = quantity_reserved - ? WHERE product_id = ?',
    [Quantity, ProductId]);

  // Log inventory transaction
  Conn.ExecuteNonQuery(
    'INSERT INTO inventory_transactions (product_id, transaction_type, quantity, reference_type, reference_id) ' +
    'VALUES (?, ''release'', ?, ''cart'', ?)', [ProductId, Quantity, CartId]);
end;

// =============================================================================
// Demonstrate Cart Operations
// =============================================================================
{ Creates a cart for Alice, adds items including a stock-limited product, and displays the cart contents with subtotals. }
procedure DemoCartOperations;
var
  CartId: Integer;
  DS: TDataSet;
begin
  WriteLn('3. Shopping Cart Operations');
  WriteLn('   =========================');
  WriteLn('');

  // Create cart for Alice
  CartId := CreateCart(1, 'session_alice_001');
  WriteLn(Format('   Created cart #%d for Alice', [CartId]));
  WriteLn('');

  // Add items to cart
  WriteLn('   Adding items to cart:');
  if AddToCart(CartId, 1, 1) then
    WriteLn('     + Pro Laptop 15" (qty: 1)');
  if AddToCart(CartId, 3, 2) then
    WriteLn('     + Wireless Headphones (qty: 2)');
  if AddToCart(CartId, 8, 3) then
    WriteLn('     + USB-C Charger (qty: 3)');

  // Try to add low-stock item in excessive quantity
  WriteLn('');
  WriteLn('   Attempting to add Smart Watch (qty: 5) - only 3 in stock:');
  AddToCart(CartId, 7, 5);  // Should fail

  // Add available quantity
  if AddToCart(CartId, 7, 2) then
    WriteLn('     + Smart Watch (qty: 2) - added available quantity');

  // Display cart
  WriteLn('');
  WriteLn('   Current cart contents:');
  WriteLn('   ----------------------');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, ci.quantity, ci.unit_price, ' +
    '  (ci.quantity * ci.unit_price * (1 - ci.discount_percent/100)) AS line_total ' +
    'FROM cart_items ci ' +
    'JOIN products p ON ci.product_id = p.id ' +
    'WHERE ci.cart_id = ? ' +
    'ORDER BY ci.added_at', [CartId]);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s x%d @ $%.2f = $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('unit_price').AsFloat,
         DS.FieldByName('line_total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Cart total
  DS := Conn.ExecuteQuery(
    'SELECT SUM(quantity * unit_price * (1 - discount_percent/100)) AS subtotal, ' +
    '  COUNT(*) AS item_count, SUM(quantity) AS total_items ' +
    'FROM cart_items WHERE cart_id = ?', [CartId]);
  try
    if not DS.EOF then
      WriteLn(Format('   Subtotal: $%.2f (%d items, %d units)',
        [DS.FieldByName('subtotal').AsFloat,
         DS.FieldByName('item_count').AsInteger,
         DS.FieldByName('total_items').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Stock Reservation Display
// =============================================================================
{ Displays all products with their available, reserved, and free stock quantities along with a low-stock warning status. }
procedure DemoStockReservation;
var
  DS: TDataSet;
begin
  WriteLn('4. Stock Reservation Status');
  WriteLn('   =========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT p.name, i.quantity_available, i.quantity_reserved, ' +
    '  (i.quantity_available - i.quantity_reserved) AS actually_available, ' +
    '  i.reorder_level, ' +
    '  CASE WHEN (i.quantity_available - i.quantity_reserved) <= i.reorder_level ' +
    '    THEN ''LOW STOCK'' ELSE ''OK'' END AS stock_status ' +
    'FROM inventory i ' +
    'JOIN products p ON i.product_id = p.id ' +
    'ORDER BY actually_available');
  try
    WriteLn('   Product              | Available | Reserved | Free  | Status');
    WriteLn('   ---------------------|-----------|----------|-------|----------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s | %9d | %8d | %5d | %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('quantity_available').AsInteger,
         DS.FieldByName('quantity_reserved').AsInteger,
         DS.FieldByName('actually_available').AsInteger,
         DS.FieldByName('stock_status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Discount Code Validation
// =============================================================================
{ Checks a discount code for validity (active, not expired, within usage limits, meets minimum order) and returns the calculated discount amount. }
function ValidateDiscountCode(const Code: string; OrderAmount: Double): Double;
var
  DS: TDataSet;
  DiscountType: string;
  DiscountValue, MinAmount: Double;
  IsActive, MaxUses, UsedCount: Integer;
begin
  Result := 0;

  DS := Conn.ExecuteQuery(
    'SELECT discount_type, discount_value, min_order_amount, is_active, ' +
    '  COALESCE(max_uses, 999999) AS max_uses, used_count, ' +
    '  CASE WHEN valid_until IS NULL OR valid_until >= date(''now'') THEN 1 ELSE 0 END AS valid ' +
    'FROM discount_codes WHERE code = ?', [Code]);
  try
    if DS.EOF then
    begin
      WriteLn(Format('     Code "%s" not found', [Code]));
      Exit;
    end;

    IsActive := DS.FieldByName('is_active').AsInteger;
    if IsActive = 0 then
    begin
      WriteLn(Format('     Code "%s" is not active', [Code]));
      Exit;
    end;

    if DS.FieldByName('valid').AsInteger = 0 then
    begin
      WriteLn(Format('     Code "%s" has expired', [Code]));
      Exit;
    end;

    MaxUses := DS.FieldByName('max_uses').AsInteger;
    UsedCount := DS.FieldByName('used_count').AsInteger;
    if UsedCount >= MaxUses then
    begin
      WriteLn(Format('     Code "%s" has reached maximum uses', [Code]));
      Exit;
    end;

    MinAmount := DS.FieldByName('min_order_amount').AsFloat;
    if OrderAmount < MinAmount then
    begin
      WriteLn(Format('     Code "%s" requires minimum order of $%.2f', [Code, MinAmount]));
      Exit;
    end;

    DiscountType := DS.FieldByName('discount_type').AsString;
    DiscountValue := DS.FieldByName('discount_value').AsFloat;

    case DiscountType of
      'percent': Result := OrderAmount * (DiscountValue / 100);
      'fixed': Result := DiscountValue;
      'shipping': Result := 0;  // Applied to shipping, not subtotal
    end;

    WriteLn(Format('     Code "%s" valid! Discount: $%.2f', [Code, Result]));
  finally
    DS.Free;
  end;
end;

{ Tests various discount codes against an order subtotal, showing valid codes, expired codes, minimum-amount failures, and non-existent codes. }
procedure DemoDiscountCodes;
var
  Subtotal: Double;
begin
  WriteLn('5. Discount Code Validation');
  WriteLn('   =========================');
  WriteLn('');

  Subtotal := 150.00;
  WriteLn(Format('   Testing codes on order subtotal: $%.2f', [Subtotal]));
  WriteLn('');

  WriteLn('   Testing "SAVE10" (10% off, min $50):');
  ValidateDiscountCode('SAVE10', Subtotal);

  WriteLn('   Testing "FLAT20" ($20 off, min $100):');
  ValidateDiscountCode('FLAT20', Subtotal);

  WriteLn('   Testing "FLAT20" on $50 order:');
  ValidateDiscountCode('FLAT20', 50.00);

  WriteLn('   Testing "EXPIRED" (inactive code):');
  ValidateDiscountCode('EXPIRED', Subtotal);

  WriteLn('   Testing "INVALID" (non-existent):');
  ValidateDiscountCode('INVALID', Subtotal);

  WriteLn('');
end;

// =============================================================================
// Checkout Process
// =============================================================================
{ Generates a unique order number in the format ORD-YYYYMMDD-NNNN using the current date and a random 4-digit suffix. }
function GenerateOrderNumber: string;
begin
  Result := 'ORD-' + FormatDateTime('yyyymmdd', Now) + '-' +
            IntToStr(Random(9000) + 1000);
end;

{ Converts a cart into an order within a transaction: applies discounts, calculates tax and shipping, creates order records, updates inventory, and awards loyalty points. }
function ProcessCheckout(CartId: Integer; const DiscountCode: string): Integer;
var
  DS: TDataSet;
  CustomerId: Integer;
  Subtotal, DiscountAmount, TaxAmount, ShippingAmount, TotalAmount: Double;
  OrderNumber, ShippingAddress: string;
  TaxRate: Double;
begin
  Result := 0;
  TaxRate := 0.08;  // 8% tax

  WriteLn('   Processing checkout for cart #' + IntToStr(CartId) + '...');

  // Get cart info
  DS := Conn.ExecuteQuery('SELECT customer_id FROM carts WHERE id = ?', [CartId]);
  try
    if DS.EOF then
    begin
      WriteLn('     ERROR: Cart not found');
      Exit;
    end;
    CustomerId := DS.FieldByName('customer_id').AsInteger;
  finally
    DS.Free;
  end;

  // Calculate subtotal
  Subtotal := Conn.ExecuteScalar(
    'SELECT SUM(quantity * unit_price * (1 - discount_percent/100)) FROM cart_items WHERE cart_id = ?',
    [CartId]);

  // Apply discount
  DiscountAmount := 0;
  if DiscountCode <> '' then
    DiscountAmount := ValidateDiscountCode(DiscountCode, Subtotal);

  // Calculate tax and shipping
  TaxAmount := (Subtotal - DiscountAmount) * TaxRate;
  ShippingAmount := 9.99;  // Flat rate shipping

  // Check for free shipping
  if DiscountCode = 'FREESHIP' then
    ShippingAmount := 0;

  TotalAmount := Subtotal - DiscountAmount + TaxAmount + ShippingAmount;

  // Get shipping address
  ShippingAddress := Conn.ExecuteScalar(
    'SELECT shipping_address FROM customers WHERE id = ?', [CustomerId]);

  // Generate order number
  OrderNumber := GenerateOrderNumber;

  // Begin transaction
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  try
    // Create order
    Conn.ExecuteNonQuery(
      'INSERT INTO orders (customer_id, cart_id, order_number, subtotal, discount_amount, ' +
      'tax_amount, shipping_amount, total_amount, discount_code, shipping_address, status) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ''pending'')',
      [CustomerId, CartId, OrderNumber, Subtotal, DiscountAmount,
       TaxAmount, ShippingAmount, TotalAmount, DiscountCode, ShippingAddress]);

    Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');

    // Copy cart items to order items
    Conn.ExecuteNonQuery(
      'INSERT INTO order_items (order_id, product_id, product_name, quantity, unit_price, discount_percent, line_total) ' +
      'SELECT ?, ci.product_id, p.name, ci.quantity, ci.unit_price, ci.discount_percent, ' +
      '  (ci.quantity * ci.unit_price * (1 - ci.discount_percent/100)) ' +
      'FROM cart_items ci ' +
      'JOIN products p ON ci.product_id = p.id ' +
      'WHERE ci.cart_id = ?', [Result, CartId]);

    // Update inventory (move from reserved to sold)
    Conn.ExecuteNonQuery(
      'UPDATE inventory SET ' +
      '  quantity_available = quantity_available - (SELECT quantity FROM cart_items WHERE cart_id = ? AND product_id = inventory.product_id), ' +
      '  quantity_reserved = quantity_reserved - (SELECT quantity FROM cart_items WHERE cart_id = ? AND product_id = inventory.product_id) ' +
      'WHERE product_id IN (SELECT product_id FROM cart_items WHERE cart_id = ?)',
      [CartId, CartId, CartId]);

    // Log inventory transactions
    DS := Conn.ExecuteQuery('SELECT product_id, quantity FROM cart_items WHERE cart_id = ?', [CartId]);
    try
      while not DS.EOF do
      begin
        Conn.ExecuteNonQuery(
          'INSERT INTO inventory_transactions (product_id, transaction_type, quantity, reference_type, reference_id) ' +
          'VALUES (?, ''sale'', ?, ''order'', ?)',
          [DS.FieldByName('product_id').AsInteger, DS.FieldByName('quantity').AsInteger, Result]);
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    // Update discount code usage
    if DiscountCode <> '' then
      Conn.ExecuteNonQuery('UPDATE discount_codes SET used_count = used_count + 1 WHERE code = ?', [DiscountCode]);

    // Mark cart as converted
    Conn.ExecuteNonQuery('UPDATE carts SET status = ''converted'', updated_at = datetime(''now'') WHERE id = ?', [CartId]);

    // Add loyalty points (1 point per $1)
    Conn.ExecuteNonQuery(
      'UPDATE customers SET loyalty_points = loyalty_points + ? WHERE id = ?',
      [Round(TotalAmount), CustomerId]);

    Conn.ExecuteNonQuery('COMMIT');

    WriteLn('');
    WriteLn(Format('   Order #%s created successfully!', [OrderNumber]));
    WriteLn(Format('     Subtotal:     $%8.2f', [Subtotal]));
    if DiscountAmount > 0 then
      WriteLn(Format('     Discount:    -$%8.2f', [DiscountAmount]));
    WriteLn(Format('     Tax (8%%):     $%8.2f', [TaxAmount]));
    WriteLn(Format('     Shipping:     $%8.2f', [ShippingAmount]));
    WriteLn(Format('     TOTAL:        $%8.2f', [TotalAmount]));

  except
    on E: Exception do
    begin
      Conn.ExecuteNonQuery('ROLLBACK');
      WriteLn('     ERROR: Checkout failed - ' + E.Message);
      Result := 0;
    end;
  end;
end;

{ Demonstrates the checkout process converting a cart to an order. }
procedure DemoCheckout;
var
  CartId, OrderId: Integer;
begin
  WriteLn('6. Checkout Process');
  WriteLn('   ==================');
  WriteLn('');

  // Create a new cart for Bob and process checkout
  CartId := CreateCart(2, 'session_bob_001');
  AddToCart(CartId, 2, 1);  // SmartPhone
  AddToCart(CartId, 4, 3);  // T-Shirts
  AddToCart(CartId, 6, 1);  // Programming Guide

  WriteLn(Format('   Created cart #%d for Bob with 3 products', [CartId]));
  WriteLn('');

  OrderId := ProcessCheckout(CartId, 'SAVE10');

  WriteLn('');
end;

// =============================================================================
// Inventory Status After Orders
// =============================================================================
{ Displays the current inventory levels after checkout processing and identifies products that have fallen below their reorder threshold. }
procedure DemoInventoryAfterOrders;
var
  DS: TDataSet;
begin
  WriteLn('7. Inventory After Orders');
  WriteLn('   =======================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT p.name, i.quantity_available, i.quantity_reserved, i.reorder_level, ' +
    '  CASE ' +
    '    WHEN i.quantity_available <= 0 THEN ''OUT OF STOCK'' ' +
    '    WHEN i.quantity_available <= i.reorder_level THEN ''REORDER'' ' +
    '    ELSE ''IN STOCK'' ' +
    '  END AS status ' +
    'FROM inventory i ' +
    'JOIN products p ON i.product_id = p.id ' +
    'ORDER BY i.quantity_available');
  try
    WriteLn('   Product              | Available | Reserved | Reorder | Status');
    WriteLn('   ---------------------|-----------|----------|---------|------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s | %9d | %8d | %7d | %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('quantity_available').AsInteger,
         DS.FieldByName('quantity_reserved').AsInteger,
         DS.FieldByName('reorder_level').AsInteger,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Products needing reorder
  WriteLn('');
  WriteLn('   Products needing reorder:');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, i.quantity_available, i.reorder_level, ' +
    '  (i.max_stock - i.quantity_available) AS suggested_order ' +
    'FROM inventory i ' +
    'JOIN products p ON i.product_id = p.id ' +
    'WHERE i.quantity_available <= i.reorder_level');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d in stock (reorder level: %d) - suggest ordering %d units',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('quantity_available').AsInteger,
         DS.FieldByName('reorder_level').AsInteger,
         DS.FieldByName('suggested_order').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Cart Abandonment
// =============================================================================
{ Creates an expired cart, finds all abandoned carts past their expiration, releases their reserved stock, and marks them as abandoned. }
procedure DemoCartAbandonment;
var
  DS: TDataSet;
  AbandonedCount: Integer;
begin
  WriteLn('8. Cart Abandonment Handling');
  WriteLn('   ==========================');
  WriteLn('');

  // Create some abandoned carts
  Conn.ExecuteNonQuery(
    'INSERT INTO carts (customer_id, session_id, status, created_at, expires_at) VALUES ' +
    '(3, ''session_carol_old'', ''active'', datetime(''now'', ''-48 hours''), datetime(''now'', ''-24 hours''))');

  // Add items to abandoned cart
  Conn.ExecuteNonQuery(
    'INSERT INTO cart_items (cart_id, product_id, quantity, unit_price) VALUES ' +
    '((SELECT id FROM carts WHERE session_id = ''session_carol_old''), 1, 1, 1299.99)');
  Conn.ExecuteNonQuery(
    'UPDATE inventory SET quantity_reserved = quantity_reserved + 1 WHERE product_id = 1');

  WriteLn('   Created test abandoned cart (48 hours old)');
  WriteLn('');

  // Find abandoned carts
  WriteLn('   Abandoned carts (expired and still active):');
  DS := Conn.ExecuteQuery(
    'SELECT c.id, c.customer_id, cu.name, cu.email, c.created_at, ' +
    '  (SELECT SUM(quantity * unit_price) FROM cart_items WHERE cart_id = c.id) AS cart_value, ' +
    '  (SELECT COUNT(*) FROM cart_items WHERE cart_id = c.id) AS item_count ' +
    'FROM carts c ' +
    'LEFT JOIN customers cu ON c.customer_id = cu.id ' +
    'WHERE c.status = ''active'' AND c.expires_at < datetime(''now'')');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     Cart #%d - %s (%s)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('email').AsString]));
      WriteLn(Format('       Created: %s | Value: $%.2f | Items: %d',
        [DS.FieldByName('created_at').AsString,
         DS.FieldByName('cart_value').AsFloat,
         DS.FieldByName('item_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Process abandoned carts (release stock and mark as abandoned)
  WriteLn('');
  WriteLn('   Processing abandoned carts...');

  // Release reserved stock
  Conn.ExecuteNonQuery(
    'UPDATE inventory SET quantity_reserved = quantity_reserved - ' +
    '  (SELECT COALESCE(SUM(ci.quantity), 0) FROM cart_items ci ' +
    '   JOIN carts c ON ci.cart_id = c.id ' +
    '   WHERE c.status = ''active'' AND c.expires_at < datetime(''now'') ' +
    '   AND ci.product_id = inventory.product_id) ' +
    'WHERE product_id IN ' +
    '  (SELECT DISTINCT ci.product_id FROM cart_items ci ' +
    '   JOIN carts c ON ci.cart_id = c.id ' +
    '   WHERE c.status = ''active'' AND c.expires_at < datetime(''now''))');

  // Mark carts as abandoned
  AbandonedCount := Conn.ExecuteNonQuery(
    'UPDATE carts SET status = ''abandoned'', updated_at = datetime(''now'') ' +
    'WHERE status = ''active'' AND expires_at < datetime(''now'')');

  WriteLn(Format('     Marked %d cart(s) as abandoned and released stock', [AbandonedCount]));
  WriteLn('');
end;

// =============================================================================
// Order History and Analytics
// =============================================================================
{ Displays recent orders, revenue breakdown by product category, and customer loyalty information including tier, points, and total spending. }
procedure DemoOrderAnalytics;
var
  DS: TDataSet;
begin
  WriteLn('9. Order Analytics');
  WriteLn('   =================');
  WriteLn('');

  // Recent orders
  WriteLn('   Recent orders:');
  DS := Conn.ExecuteQuery(
    'SELECT o.order_number, c.name AS customer, o.total_amount, o.status, o.created_at ' +
    'FROM orders o ' +
    'JOIN customers c ON o.customer_id = c.id ' +
    'ORDER BY o.created_at DESC ' +
    'LIMIT 5');
  try
    WriteLn('   Order #       | Customer      | Total     | Status    | Date');
    WriteLn('   --------------|---------------|-----------|-----------|--------------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-13s | %-13s | $%8.2f | %-9s | %s',
        [DS.FieldByName('order_number').AsString,
         DS.FieldByName('customer').AsString,
         DS.FieldByName('total_amount').AsFloat,
         DS.FieldByName('status').AsString,
         DS.FieldByName('created_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Sales by category
  WriteLn('');
  WriteLn('   Sales by category:');
  DS := Conn.ExecuteQuery(
    'SELECT p.category, SUM(oi.line_total) AS revenue, SUM(oi.quantity) AS units_sold ' +
    'FROM order_items oi ' +
    'JOIN products p ON oi.product_id = p.id ' +
    'GROUP BY p.category ' +
    'ORDER BY revenue DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: $%.2f (%d units)',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('revenue').AsFloat,
         DS.FieldByName('units_sold').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Customer loyalty status
  WriteLn('');
  WriteLn('   Customer loyalty status:');
  DS := Conn.ExecuteQuery(
    'SELECT name, email, tier, loyalty_points, ' +
    '  (SELECT COUNT(*) FROM orders WHERE customer_id = customers.id) AS order_count, ' +
    '  (SELECT COALESCE(SUM(total_amount), 0) FROM orders WHERE customer_id = customers.id) AS total_spent ' +
    'FROM customers ' +
    'ORDER BY total_spent DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%s) - %s tier, %d points',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('tier').AsString,
         DS.FieldByName('loyalty_points').AsInteger]));
      WriteLn(Format('       Orders: %d | Total spent: $%.2f',
        [DS.FieldByName('order_count').AsInteger,
         DS.FieldByName('total_spent').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Inventory Transaction Log
// =============================================================================
{ Displays the most recent inventory transactions showing product, transaction type (reserve/sale/release), quantity, and reference details. }
procedure DemoInventoryLog;
var
  DS: TDataSet;
begin
  WriteLn('10. Inventory Transaction Log');
  WriteLn('    ==========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT p.name, it.transaction_type, it.quantity, it.reference_type, it.reference_id, it.created_at ' +
    'FROM inventory_transactions it ' +
    'JOIN products p ON it.product_id = p.id ' +
    'ORDER BY it.created_at DESC ' +
    'LIMIT 10');
  try
    WriteLn('   Product              | Type     | Qty  | Reference     | Time');
    WriteLn('   ---------------------|----------|------|---------------|--------------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s | %-8s | %4d | %-13s | %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('transaction_type').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('reference_type').AsString + ' #' + DS.FieldByName('reference_id').AsString,
         Copy(DS.FieldByName('created_at').AsString, 12, 8)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 74: Shopping Cart with Stock Management ===');
  WriteLn('');

  Randomize;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    PopulateSampleData;
    DemoCartOperations;
    DemoStockReservation;
    DemoDiscountCodes;
    DemoCheckout;
    DemoInventoryAfterOrders;
    DemoCartAbandonment;
    DemoOrderAnalytics;
    DemoInventoryLog;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
