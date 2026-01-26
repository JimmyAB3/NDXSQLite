{===============================================================================
  NDXSQLite Example 103 - Invoice System
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Invoice creation with line items
  - Tax calculation and totals
  - Payment tracking and status updates
  - Aging report and overdue detection
  - Multi-currency support patterns

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program InvoiceSystem;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  Conn.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  address TEXT,' +
    '  tax_id TEXT' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  unit_price REAL NOT NULL,' +
    '  tax_rate REAL DEFAULT 0.20' +  // 20% default tax
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE invoices (' +
    '  id TEXT PRIMARY KEY,' +
    '  invoice_number TEXT UNIQUE,' +
    '  customer_id TEXT NOT NULL,' +
    '  status TEXT DEFAULT ''draft'',' +  // draft, sent, paid, partial, overdue, cancelled
    '  issued_at TEXT,' +
    '  due_at TEXT,' +
    '  notes TEXT,' +
    '  discount_type TEXT,' +  // percentage, fixed
    '  discount_value REAL DEFAULT 0,' +
    '  FOREIGN KEY (customer_id) REFERENCES customers(id)' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE invoice_lines (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  invoice_id TEXT NOT NULL,' +
    '  product_id TEXT,' +
    '  description TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL DEFAULT 1,' +
    '  unit_price REAL NOT NULL,' +
    '  tax_rate REAL DEFAULT 0.20,' +
    '  line_discount REAL DEFAULT 0,' +
    '  FOREIGN KEY (invoice_id) REFERENCES invoices(id),' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE payments (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  invoice_id TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  method TEXT NOT NULL,' +  // bank_transfer, credit_card, check, cash
    '  reference TEXT,' +
    '  paid_at TEXT NOT NULL,' +
    '  FOREIGN KEY (invoice_id) REFERENCES invoices(id)' +
    ')');
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  // Customers
  Conn.ExecuteNonQuery('INSERT INTO customers (id, name, email, address, tax_id) VALUES ' +
    '(''cust-001'', ''Acme Corp'', ''billing@acme.com'', ''123 Business Ave, Suite 100'', ''US-12-3456789'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (id, name, email, address, tax_id) VALUES ' +
    '(''cust-002'', ''TechStart Inc'', ''invoices@techstart.io'', ''456 Innovation Dr'', ''US-98-7654321'')');
  Conn.ExecuteNonQuery('INSERT INTO customers (id, name, email, address, tax_id) VALUES ' +
    '(''cust-003'', ''Global Services Ltd'', ''ap@globalservices.com'', ''789 Enterprise Blvd'', ''UK-VAT-123456'')');

  // Products/Services
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, unit_price, tax_rate) VALUES ' +
    '(''svc-consult'', ''Consulting (hourly)'', 150.00, 0.20)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, unit_price, tax_rate) VALUES ' +
    '(''svc-dev'', ''Development (hourly)'', 120.00, 0.20)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, unit_price, tax_rate) VALUES ' +
    '(''lic-enterprise'', ''Enterprise License (annual)'', 5000.00, 0.20)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, unit_price, tax_rate) VALUES ' +
    '(''lic-basic'', ''Basic License (annual)'', 1200.00, 0.20)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, unit_price, tax_rate) VALUES ' +
    '(''hw-server'', ''Server Hardware'', 3500.00, 0.20)');
  Conn.ExecuteNonQuery('INSERT INTO products (id, name, unit_price, tax_rate) VALUES ' +
    '(''svc-support'', ''Premium Support (monthly)'', 500.00, 0.00)');  // Tax-exempt
end;

{ Creates an invoice with the specified discount settings. }
procedure CreateInvoice(const InvId, InvNum, CustId, IssuedAt, DueAt: string;
  const DiscountType: string; DiscountValue: Double);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO invoices (id, invoice_number, customer_id, status, issued_at, due_at, discount_type, discount_value) VALUES (' +
    '''' + InvId + ''', ''' + InvNum + ''', ''' + CustId + ''', ''draft'', ' +
    '''' + IssuedAt + ''', ''' + DueAt + ''', ' +
    '''' + DiscountType + ''', ' + FloatToStr(DiscountValue) + ')');
end;

{ Adds a line item to an invoice with quantity, price, and tax details. }
procedure AddLine(const InvId, ProductId, Description: string;
  Qty: Integer; UnitPrice, TaxRate, LineDiscount: Double);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO invoice_lines (invoice_id, product_id, description, quantity, unit_price, tax_rate, line_discount) VALUES (' +
    '''' + InvId + ''', ''' + ProductId + ''', ''' + Description + ''', ' +
    IntToStr(Qty) + ', ' + FloatToStr(UnitPrice) + ', ' + FloatToStr(TaxRate) + ', ' +
    FloatToStr(LineDiscount) + ')');
end;

{ Inserts a payment record for an invoice with amount, payment method, reference number, and payment timestamp. }
procedure RecordPayment(const InvId, Method, Reference, PaidAt: string; Amount: Double);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO payments (invoice_id, amount, method, reference, paid_at) VALUES (' +
    '''' + InvId + ''', ' + FloatToStr(Amount) + ', ''' + Method + ''', ' +
    '''' + Reference + ''', ''' + PaidAt + ''')');
end;

{ Sets the status field of an invoice to the specified value (draft, sent, paid, partial, overdue, or cancelled). }
procedure UpdateInvoiceStatus(const InvId, Status: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE invoices SET status = ''' + Status + ''' WHERE id = ''' + InvId + '''');
end;

{ Demo procedures }

procedure DemoCreateInvoices;
begin
  WriteLn('=== 1. Create Invoices ===');
  WriteLn;

  // Invoice 1: Large enterprise deal with percentage discount
  CreateInvoice('inv-001', 'INV-2024-001', 'cust-001', '2024-01-15', '2024-02-14', 'percentage', 10);
  AddLine('inv-001', 'lic-enterprise', 'Enterprise License 2024', 1, 5000.00, 0.20, 0);
  AddLine('inv-001', 'svc-consult', 'Initial Setup Consulting', 8, 150.00, 0.20, 0);
  AddLine('inv-001', 'hw-server', 'Production Server', 2, 3500.00, 0.20, 0);
  AddLine('inv-001', 'svc-support', 'Premium Support (12 months)', 12, 500.00, 0.00, 0);
  WriteLn('   Created INV-2024-001 for Acme Corp (10% discount)');

  // Invoice 2: Small project with fixed discount
  CreateInvoice('inv-002', 'INV-2024-002', 'cust-002', '2024-01-20', '2024-02-19', 'fixed', 200);
  AddLine('inv-002', 'svc-dev', 'Web App Development', 40, 120.00, 0.20, 0);
  AddLine('inv-002', 'lic-basic', 'Basic License', 1, 1200.00, 0.20, 0);
  AddLine('inv-002', 'svc-consult', 'Architecture Review', 4, 150.00, 0.20, 100);  // line discount
  WriteLn('   Created INV-2024-002 for TechStart Inc ($200 discount + $100 line discount)');

  // Invoice 3: Services only, no discount
  CreateInvoice('inv-003', 'INV-2024-003', 'cust-003', '2024-02-01', '2024-03-02', '', 0);
  AddLine('inv-003', 'svc-consult', 'Strategy Consulting', 16, 150.00, 0.20, 0);
  AddLine('inv-003', 'svc-dev', 'Custom Integration', 24, 120.00, 0.20, 0);
  WriteLn('   Created INV-2024-003 for Global Services Ltd (no discount)');

  // Invoice 4: Overdue invoice
  CreateInvoice('inv-004', 'INV-2023-099', 'cust-001', '2023-11-01', '2023-12-01', '', 0);
  AddLine('inv-004', 'svc-consult', 'Q4 Advisory Services', 20, 150.00, 0.20, 0);
  UpdateInvoiceStatus('inv-004', 'overdue');
  WriteLn('   Created INV-2023-099 for Acme Corp (overdue)');

  WriteLn;
end;

{ Prints all line items for INV-2024-001 showing description, quantity, unit price, tax rate, subtotal, and calculated tax amount. }
procedure DemoLineItems;
begin
  WriteLn('=== 2. Line Items Detail ===');
  WriteLn;

  WriteLn('   INV-2024-001 line items:');
  DS := Conn.ExecuteQuery(
    'SELECT il.description, il.quantity, il.unit_price, il.tax_rate, il.line_discount, ' +
    '(il.quantity * il.unit_price - il.line_discount) as subtotal, ' +
    'CAST((il.quantity * il.unit_price - il.line_discount) * il.tax_rate AS REAL) as tax_amount ' +
    'FROM invoice_lines il WHERE il.invoice_id = ''inv-001'' ORDER BY il.id');
  try
    WriteLn(Format('   %-30s %3s %10s %5s %10s %10s',
      ['Description', 'Qty', 'Unit $', 'Tax%', 'Subtotal', 'Tax']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-30s %3d %10.2f %4.0f%% %10.2f %10.2f',
        [DS.FieldByName('description').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('unit_price').AsFloat,
         DS.FieldByName('tax_rate').AsFloat * 100,
         DS.FieldByName('subtotal').AsFloat,
         DS.FieldByName('tax_amount').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Calculates and prints subtotal, discount amount, proportionally-adjusted tax, and grand total for INV-2024-001, then shows totals for all invoices. }
procedure DemoTaxCalculation;
var
  Subtotal, TaxTotal, DiscountAmt, GrandTotal: Double;
  DiscountType: string;
  DiscountValue: Double;
begin
  WriteLn('=== 3. Tax & Discount Calculation ===');
  WriteLn;

  WriteLn('   INV-2024-001 totals:');

  // Calculate subtotal (before discount)
  DS := Conn.ExecuteQuery(
    'SELECT CAST(SUM(quantity * unit_price - line_discount) AS REAL) as subtotal ' +
    'FROM invoice_lines WHERE invoice_id = ''inv-001''');
  try
    Subtotal := DS.FieldByName('subtotal').AsFloat;
  finally
    DS.Free;
  end;
  WriteLn(Format('    Lines subtotal:    $%10.2f', [Subtotal]));

  // Get discount
  DS := Conn.ExecuteQuery('SELECT discount_type, discount_value FROM invoices WHERE id = ''inv-001''');
  try
    DiscountType := DS.FieldByName('discount_type').AsString;
    DiscountValue := DS.FieldByName('discount_value').AsFloat;
  finally
    DS.Free;
  end;

  if DiscountType = 'percentage' then
    DiscountAmt := Subtotal * DiscountValue / 100
  else if DiscountType = 'fixed' then
    DiscountAmt := DiscountValue
  else
    DiscountAmt := 0;

  WriteLn(Format('    Discount (%s %.0f): -$%9.2f', [DiscountType, DiscountValue, DiscountAmt]));

  // Calculate tax on discounted amount
  DS := Conn.ExecuteQuery(
    'SELECT CAST(SUM((quantity * unit_price - line_discount) * tax_rate) AS REAL) as tax_total ' +
    'FROM invoice_lines WHERE invoice_id = ''inv-001''');
  try
    TaxTotal := DS.FieldByName('tax_total').AsFloat;
  finally
    DS.Free;
  end;

  // Adjust tax for discount (proportional)
  TaxTotal := TaxTotal * (1 - DiscountAmt / Subtotal);
  WriteLn(Format('    Tax (adjusted):    $%10.2f', [TaxTotal]));

  GrandTotal := (Subtotal - DiscountAmt) + TaxTotal;
  WriteLn(Format('    GRAND TOTAL:       $%10.2f', [GrandTotal]));

  WriteLn;

  // Show all invoices with calculated totals
  WriteLn('   All invoice totals:');
  DS := Conn.ExecuteQuery(
    'SELECT i.invoice_number, c.name as customer, i.status, i.discount_type, i.discount_value, ' +
    'CAST(SUM(il.quantity * il.unit_price - il.line_discount) AS REAL) as subtotal, ' +
    'CAST(SUM((il.quantity * il.unit_price - il.line_discount) * il.tax_rate) AS REAL) as tax ' +
    'FROM invoices i ' +
    'JOIN customers c ON i.customer_id = c.id ' +
    'JOIN invoice_lines il ON il.invoice_id = i.id ' +
    'GROUP BY i.id ORDER BY i.invoice_number');
  try
    while not DS.EOF do
    begin
      Subtotal := DS.FieldByName('subtotal').AsFloat;
      TaxTotal := DS.FieldByName('tax').AsFloat;
      DiscountType := DS.FieldByName('discount_type').AsString;
      DiscountValue := DS.FieldByName('discount_value').AsFloat;

      if DiscountType = 'percentage' then
        DiscountAmt := Subtotal * DiscountValue / 100
      else if DiscountType = 'fixed' then
        DiscountAmt := DiscountValue
      else
        DiscountAmt := 0;

      TaxTotal := TaxTotal * (1 - DiscountAmt / Subtotal);
      GrandTotal := (Subtotal - DiscountAmt) + TaxTotal;

      WriteLn(Format('    %s  %-20s  %-8s  $%10.2f',
        [DS.FieldByName('invoice_number').AsString,
         DS.FieldByName('customer').AsString,
         DS.FieldByName('status').AsString,
         GrandTotal]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Sends invoices, records partial and full payments via different methods, updates invoice statuses, and prints payment summaries with outstanding amounts. }
procedure DemoPaymentTracking;
var
  TotalPaid, Outstanding: Double;
begin
  WriteLn('=== 4. Payment Tracking ===');
  WriteLn;

  // Send invoices
  UpdateInvoiceStatus('inv-001', 'sent');
  UpdateInvoiceStatus('inv-002', 'sent');
  UpdateInvoiceStatus('inv-003', 'sent');
  WriteLn('   Sent invoices: INV-2024-001, INV-2024-002, INV-2024-003');

  // Payments for inv-001 (partial payments)
  RecordPayment('inv-001', 'bank_transfer', 'TRF-20240201-001', '2024-02-01', 10000.00);
  WriteLn('   Payment: $10,000 on INV-2024-001 (bank transfer)');

  RecordPayment('inv-001', 'bank_transfer', 'TRF-20240210-002', '2024-02-10', 8000.00);
  WriteLn('   Payment: $8,000 on INV-2024-001 (bank transfer)');
  UpdateInvoiceStatus('inv-001', 'partial');

  // Full payment for inv-002
  RecordPayment('inv-002', 'credit_card', 'CC-4532-XXXX-1234', '2024-02-05', 6940.00);
  WriteLn('   Payment: $6,940 on INV-2024-002 (credit card - full)');
  UpdateInvoiceStatus('inv-002', 'paid');

  // Partial for inv-004
  RecordPayment('inv-004', 'check', 'CHK-88901', '2024-02-15', 1000.00);
  WriteLn('   Payment: $1,000 on INV-2023-099 (check - partial)');

  WriteLn;
  WriteLn('   Payment summary per invoice:');
  DS := Conn.ExecuteQuery(
    'SELECT i.invoice_number, i.status, ' +
    'CAST(SUM(il.quantity * il.unit_price - il.line_discount) AS REAL) as subtotal, ' +
    'COALESCE((SELECT SUM(p.amount) FROM payments p WHERE p.invoice_id = i.id), 0) as paid ' +
    'FROM invoices i ' +
    'JOIN invoice_lines il ON il.invoice_id = i.id ' +
    'GROUP BY i.id ORDER BY i.invoice_number');
  try
    while not DS.EOF do
    begin
      TotalPaid := DS.FieldByName('paid').AsFloat;
      Outstanding := DS.FieldByName('subtotal').AsFloat - TotalPaid;
      if Outstanding < 0 then Outstanding := 0;
      WriteLn(Format('    %s  %-8s  paid=$%9.2f  outstanding=$%9.2f',
        [DS.FieldByName('invoice_number').AsString,
         DS.FieldByName('status').AsString,
         TotalPaid,
         Outstanding]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Builds and prints a formatted invoice display for INV-2024-001 with header, customer info, line items, totals, payments, and balance due. }
procedure DemoPDFReadyQuery;
var
  Subtotal, DiscountAmt, TaxTotal, GrandTotal, TotalPaid: Double;
  DiscountType: string;
  DiscountValue: Double;
begin
  WriteLn('=== 5. PDF-Ready Invoice Query ===');
  WriteLn;

  // Full invoice data for rendering
  WriteLn('   +' + StringOfChar('-', 60) + '+');
  WriteLn('   |                        INVOICE                              |');
  WriteLn('   +' + StringOfChar('-', 60) + '+');

  // Header
  DS := Conn.ExecuteQuery(
    'SELECT i.invoice_number, i.issued_at, i.due_at, i.status, ' +
    'c.name, c.address, c.email, c.tax_id, ' +
    'i.discount_type, i.discount_value ' +
    'FROM invoices i JOIN customers c ON i.customer_id = c.id ' +
    'WHERE i.id = ''inv-001''');
  try
    WriteLn(Format('   | Invoice: %-20s  Date: %-16s |',
      [DS.FieldByName('invoice_number').AsString,
       DS.FieldByName('issued_at').AsString]));
    WriteLn(Format('   | Due:     %-20s  Status: %-14s |',
      [DS.FieldByName('due_at').AsString,
       DS.FieldByName('status').AsString]));
    WriteLn('   |' + StringOfChar('-', 60) + '|');
    WriteLn(Format('   | Bill To: %-49s |', [DS.FieldByName('name').AsString]));
    WriteLn(Format('   |          %-49s |', [DS.FieldByName('address').AsString]));
    WriteLn(Format('   |          %-49s |', [DS.FieldByName('email').AsString]));
    WriteLn(Format('   | Tax ID:  %-49s |', [DS.FieldByName('tax_id').AsString]));
    DiscountType := DS.FieldByName('discount_type').AsString;
    DiscountValue := DS.FieldByName('discount_value').AsFloat;
  finally
    DS.Free;
  end;

  WriteLn('   |' + StringOfChar('-', 60) + '|');
  WriteLn(Format('   | %-28s %3s %9s %5s %9s |',
    ['Description', 'Qty', 'Price', 'Tax', 'Amount']));
  WriteLn('   |' + StringOfChar('-', 60) + '|');

  // Lines
  Subtotal := 0;
  TaxTotal := 0;
  DS := Conn.ExecuteQuery(
    'SELECT description, quantity, unit_price, tax_rate, line_discount, ' +
    '(quantity * unit_price - line_discount) as line_total, ' +
    'CAST((quantity * unit_price - line_discount) * tax_rate AS REAL) as tax_amt ' +
    'FROM invoice_lines WHERE invoice_id = ''inv-001'' ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   | %-28s %3d %9.2f %4.0f%% %9.2f |',
        [DS.FieldByName('description').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('unit_price').AsFloat,
         DS.FieldByName('tax_rate').AsFloat * 100,
         DS.FieldByName('line_total').AsFloat]));
      Subtotal := Subtotal + DS.FieldByName('line_total').AsFloat;
      TaxTotal := TaxTotal + DS.FieldByName('tax_amt').AsFloat;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Calculate discount
  if DiscountType = 'percentage' then
    DiscountAmt := Subtotal * DiscountValue / 100
  else if DiscountType = 'fixed' then
    DiscountAmt := DiscountValue
  else
    DiscountAmt := 0;

  TaxTotal := TaxTotal * (1 - DiscountAmt / Subtotal);
  GrandTotal := (Subtotal - DiscountAmt) + TaxTotal;

  WriteLn('   |' + StringOfChar('-', 60) + '|');
  WriteLn(Format('   |%40s  Subtotal: %9.2f |', ['', Subtotal]));
  if DiscountAmt > 0 then
    WriteLn(Format('   |%40s  Discount: -%8.2f |', ['', DiscountAmt]));
  WriteLn(Format('   |%40s       Tax: %9.2f |', ['', TaxTotal]));
  WriteLn('   |' + StringOfChar(' ', 40) + StringOfChar('-', 20) + '|');
  WriteLn(Format('   |%40s     TOTAL: %9.2f |', ['', GrandTotal]));

  // Payments
  WriteLn('   |' + StringOfChar('-', 60) + '|');
  WriteLn('   | Payments:                                                    |');
  TotalPaid := 0;
  DS := Conn.ExecuteQuery(
    'SELECT amount, method, reference, paid_at FROM payments WHERE invoice_id = ''inv-001'' ORDER BY paid_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   |   %s  %-15s  %9.2f  ref: %-12s |',
        [DS.FieldByName('paid_at').AsString,
         DS.FieldByName('method').AsString,
         DS.FieldByName('amount').AsFloat,
         DS.FieldByName('reference').AsString]));
      TotalPaid := TotalPaid + DS.FieldByName('amount').AsFloat;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn(Format('   |%40s      Paid: %9.2f |', ['', TotalPaid]));
  WriteLn(Format('   |%40s   Balance: %9.2f |', ['', GrandTotal - TotalPaid]));
  WriteLn('   +' + StringOfChar('-', 60) + '+');
  WriteLn;
end;

{ Prints unpaid/partial invoices showing customer, due date, days overdue, billed amount, and outstanding balance as of a reference date. }
procedure DemoAgingReport;
begin
  WriteLn('=== 6. Aging Report ===');
  WriteLn;

  WriteLn('   Outstanding invoices by age (as of 2024-03-01):');
  DS := Conn.ExecuteQuery(
    'SELECT i.invoice_number, c.name, i.due_at, i.status, ' +
    'CAST(julianday(''2024-03-01'') - julianday(i.due_at) AS INTEGER) as days_overdue, ' +
    'CAST(SUM(il.quantity * il.unit_price - il.line_discount) AS REAL) as amount, ' +
    'COALESCE((SELECT SUM(p.amount) FROM payments p WHERE p.invoice_id = i.id), 0) as paid ' +
    'FROM invoices i ' +
    'JOIN customers c ON i.customer_id = c.id ' +
    'JOIN invoice_lines il ON il.invoice_id = i.id ' +
    'WHERE i.status NOT IN (''paid'', ''cancelled'') ' +
    'GROUP BY i.id ORDER BY i.due_at');
  try
    WriteLn(Format('   %-12s %-20s %-11s %5s %12s %12s',
      ['Invoice', 'Customer', 'Due', 'Days', 'Amount', 'Outstanding']));
    WriteLn('   ' + StringOfChar('-', 78));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-20s %-11s %5d %12.2f %12.2f',
        [DS.FieldByName('invoice_number').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('due_at').AsString,
         DS.FieldByName('days_overdue').AsInteger,
         DS.FieldByName('amount').AsFloat,
         DS.FieldByName('amount').AsFloat - DS.FieldByName('paid').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Prints revenue summaries grouped by customer, by product/service, and by payment method with invoice counts, quantities, and dollar totals. }
procedure DemoRevenueReport;
begin
  WriteLn('=== 7. Revenue Report ===');
  WriteLn;

  // Revenue by customer
  WriteLn('   Revenue by customer:');
  DS := Conn.ExecuteQuery(
    'SELECT c.name, COUNT(DISTINCT i.id) as invoice_count, ' +
    'CAST(SUM(il.quantity * il.unit_price - il.line_discount) AS REAL) as total_billed, ' +
    'COALESCE((SELECT SUM(p.amount) FROM payments p ' +
    '  JOIN invoices i2 ON p.invoice_id = i2.id WHERE i2.customer_id = c.id), 0) as total_paid ' +
    'FROM customers c ' +
    'JOIN invoices i ON i.customer_id = c.id ' +
    'JOIN invoice_lines il ON il.invoice_id = i.id ' +
    'GROUP BY c.id ORDER BY total_billed DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %-20s  %d invoices  billed=$%10.2f  paid=$%10.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('invoice_count').AsInteger,
         DS.FieldByName('total_billed').AsFloat,
         DS.FieldByName('total_paid').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Revenue by product/service
  WriteLn;
  WriteLn('   Revenue by product/service:');
  DS := Conn.ExecuteQuery(
    'SELECT COALESCE(p.name, il.description) as product, ' +
    'SUM(il.quantity) as total_qty, ' +
    'CAST(SUM(il.quantity * il.unit_price - il.line_discount) AS REAL) as revenue ' +
    'FROM invoice_lines il ' +
    'LEFT JOIN products p ON il.product_id = p.id ' +
    'GROUP BY COALESCE(p.id, il.description) ' +
    'ORDER BY revenue DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %-30s  qty=%3d  revenue=$%10.2f',
        [DS.FieldByName('product').AsString,
         DS.FieldByName('total_qty').AsInteger,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Payment method breakdown
  WriteLn;
  WriteLn('   Payments by method:');
  DS := Conn.ExecuteQuery(
    'SELECT method, COUNT(*) as cnt, CAST(SUM(amount) AS REAL) as total ' +
    'FROM payments GROUP BY method ORDER BY total DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %-15s  %d payments  total=$%10.2f',
        [DS.FieldByName('method').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays a summary of invoice counts, totals, and payment statistics. }
procedure DemoSummary;
var
  InvCount, LineCount, PayCount: Integer;
  TotalBilled, TotalPaid: Double;
begin
  WriteLn('=== Invoice System Summary ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM invoices');
  try InvCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM invoice_lines');
  try LineCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM payments');
  try PayCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT CAST(SUM(quantity * unit_price - line_discount) AS REAL) as total FROM invoice_lines');
  try TotalBilled := DS.FieldByName('total').AsFloat; finally DS.Free; end;
  DS := Conn.ExecuteQuery('SELECT CAST(COALESCE(SUM(amount), 0) AS REAL) as total FROM payments');
  try TotalPaid := DS.FieldByName('total').AsFloat; finally DS.Free; end;

  WriteLn(Format('   Invoices: %d, Line items: %d, Payments: %d', [InvCount, LineCount, PayCount]));
  WriteLn(Format('   Total billed: $%.2f, Total paid: $%.2f', [TotalBilled, TotalPaid]));
  WriteLn;
  WriteLn('   Patterns demonstrated:');
  WriteLn('   - Invoice with line items (products/services)');
  WriteLn('   - Tax calculation (per-line tax rates)');
  WriteLn('   - Discounts (percentage, fixed, per-line)');
  WriteLn('   - Payment tracking (partial, multiple methods)');
  WriteLn('   - PDF-ready formatted invoice query');
  WriteLn('   - Aging report (days overdue)');
  WriteLn('   - Revenue reporting (by customer, product, payment method)');
end;

begin
  WriteLn('Example 103: Invoice System - Line Items, Taxes, Payments');
  WriteLn('==========================================================');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    SetupDatabase;
    SeedData;

    WriteLn('Database setup complete.');
    WriteLn;

    DemoCreateInvoices;
    DemoLineItems;
    DemoTaxCalculation;
    DemoPaymentTracking;
    DemoPDFReadyQuery;
    DemoAgingReport;
    DemoRevenueReport;
    DemoSummary;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
