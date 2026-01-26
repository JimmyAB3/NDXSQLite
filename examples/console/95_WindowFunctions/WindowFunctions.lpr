{===============================================================================
  NDXSQLite Example 95 - Window Functions
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - ROW_NUMBER, RANK, DENSE_RANK
  - LAG and LEAD for row comparisons
  - Running totals and moving averages
  - NTILE for data distribution
  - FIRST_VALUE, LAST_VALUE, PERCENT_RANK

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program WindowFunctions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, NDXSQLiteConnection, DB;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // Create sales table
  Conn.ExecuteNonQuery(
    'CREATE TABLE sales (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  employee TEXT NOT NULL,' +
    '  department TEXT NOT NULL,' +
    '  region TEXT NOT NULL,' +
    '  sale_date TEXT NOT NULL,' +
    '  amount REAL NOT NULL,' +
    '  quantity INTEGER NOT NULL' +
    ')');

  // Create students table for ranking demo
  Conn.ExecuteNonQuery(
    'CREATE TABLE students (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  subject TEXT NOT NULL,' +
    '  score INTEGER NOT NULL' +
    ')');

  // Create stock_prices table for LAG/LEAD demo
  Conn.ExecuteNonQuery(
    'CREATE TABLE stock_prices (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  symbol TEXT NOT NULL,' +
    '  trade_date TEXT NOT NULL,' +
    '  close_price REAL NOT NULL,' +
    '  volume INTEGER NOT NULL' +
    ')');

  // Insert sales data
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Alice'', ''Electronics'', ''North'', ''2024-01-05'', 1500.00, 3)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Alice'', ''Electronics'', ''North'', ''2024-01-12'', 2200.00, 5)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Alice'', ''Electronics'', ''North'', ''2024-01-19'', 1800.00, 4)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Alice'', ''Electronics'', ''North'', ''2024-01-26'', 3100.00, 7)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Alice'', ''Electronics'', ''North'', ''2024-02-02'', 2500.00, 5)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Alice'', ''Electronics'', ''North'', ''2024-02-09'', 1900.00, 4)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Bob'', ''Electronics'', ''South'', ''2024-01-05'', 1200.00, 2)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Bob'', ''Electronics'', ''South'', ''2024-01-12'', 1800.00, 4)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Bob'', ''Electronics'', ''South'', ''2024-01-19'', 2400.00, 5)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Bob'', ''Electronics'', ''South'', ''2024-01-26'', 1600.00, 3)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Bob'', ''Electronics'', ''South'', ''2024-02-02'', 2100.00, 4)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Charlie'', ''Clothing'', ''North'', ''2024-01-05'', 800.00, 4)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Charlie'', ''Clothing'', ''North'', ''2024-01-12'', 950.00, 5)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Charlie'', ''Clothing'', ''North'', ''2024-01-19'', 1100.00, 6)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Charlie'', ''Clothing'', ''North'', ''2024-01-26'', 750.00, 3)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Charlie'', ''Clothing'', ''North'', ''2024-02-02'', 1200.00, 6)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Diana'', ''Clothing'', ''South'', ''2024-01-05'', 600.00, 3)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Diana'', ''Clothing'', ''South'', ''2024-01-12'', 1400.00, 7)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Diana'', ''Clothing'', ''South'', ''2024-01-19'', 900.00, 4)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Diana'', ''Clothing'', ''South'', ''2024-01-26'', 1100.00, 5)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Diana'', ''Clothing'', ''South'', ''2024-02-02'', 1300.00, 6)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Eve'', ''Food'', ''North'', ''2024-01-05'', 400.00, 10)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Eve'', ''Food'', ''North'', ''2024-01-12'', 550.00, 14)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Eve'', ''Food'', ''North'', ''2024-01-19'', 480.00, 12)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Eve'', ''Food'', ''North'', ''2024-01-26'', 620.00, 15)');
  Conn.ExecuteNonQuery('INSERT INTO sales (employee, department, region, sale_date, amount, quantity) VALUES (''Eve'', ''Food'', ''North'', ''2024-02-02'', 510.00, 13)');

  // Insert student scores
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Alice'', ''Math'', 95)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Bob'', ''Math'', 87)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Charlie'', ''Math'', 87)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Diana'', ''Math'', 92)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Eve'', ''Math'', 78)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Alice'', ''Science'', 88)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Bob'', ''Science'', 92)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Charlie'', ''Science'', 85)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Diana'', ''Science'', 92)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Eve'', ''Science'', 90)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Alice'', ''English'', 91)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Bob'', ''English'', 84)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Charlie'', ''English'', 91)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Diana'', ''English'', 88)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, subject, score) VALUES (''Eve'', ''English'', 95)');

  // Insert stock prices (2 weeks of data for 2 symbols)
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-08'', 150.25, 1200000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-09'', 152.50, 1350000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-10'', 148.75, 1500000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-11'', 153.00, 1100000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-12'', 155.50, 1400000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-15'', 154.00, 1250000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-16'', 157.25, 1600000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-17'', 156.00, 1300000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-18'', 159.50, 1700000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''ACME'', ''2024-01-19'', 158.25, 1450000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-08'', 280.00, 2500000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-09'', 275.50, 2800000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-10'', 282.25, 2200000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-11'', 278.00, 2400000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-12'', 285.75, 2600000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-15'', 283.50, 2300000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-16'', 290.00, 3000000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-17'', 287.25, 2700000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-18'', 292.50, 3200000)');
  Conn.ExecuteNonQuery('INSERT INTO stock_prices (symbol, trade_date, close_price, volume) VALUES (''TECH'', ''2024-01-19'', 295.00, 2900000)');
end;

{ Displays sales ranked by amount within each department using ROW_NUMBER with PARTITION BY. }
procedure DemoRowNumber;
begin
  WriteLn('=== 1. ROW_NUMBER - Sequential Numbering ===');
  WriteLn('');
  WriteLn('   Sales ranked by amount within each department:');

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  ROW_NUMBER() OVER (PARTITION BY department ORDER BY amount DESC) as row_num,' +
    '  employee, department, sale_date, amount ' +
    'FROM sales ' +
    'WHERE sale_date >= ''2024-01-01'' AND sale_date < ''2024-02-01'' ' +
    'ORDER BY department, row_num');
  try
    while not DS.EOF do
    begin
      WriteLn('   #', DS.FieldByName('row_num').AsInteger,
        ' ', DS.FieldByName('department').AsString:12,
        ' ', DS.FieldByName('employee').AsString:8,
        ' ', DS.FieldByName('sale_date').AsString,
        '  $', DS.FieldByName('amount').AsFloat:8:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Compares RANK and DENSE_RANK on Math scores showing how ties are handled differently, and shows per-subject rankings. }
procedure DemoRankDenseRank;
begin
  WriteLn('=== 2. RANK vs DENSE_RANK - Handling Ties ===');
  WriteLn('');
  WriteLn('   Student scores in Math (notice tie handling):');
  WriteLn('   Name       Score   RANK   DENSE_RANK');
  WriteLn('   ' + StringOfChar('-', 45));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  name, score,' +
    '  RANK() OVER (ORDER BY score DESC) as rank_num,' +
    '  DENSE_RANK() OVER (ORDER BY score DESC) as dense_rank_num ' +
    'FROM students ' +
    'WHERE subject = ''Math'' ' +
    'ORDER BY score DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:8,
        '  ', DS.FieldByName('score').AsInteger:5,
        '  ', DS.FieldByName('rank_num').AsInteger:5,
        '  ', DS.FieldByName('dense_rank_num').AsInteger:5);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Multi-subject ranking (DENSE_RANK per subject):');

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  subject, name, score,' +
    '  DENSE_RANK() OVER (PARTITION BY subject ORDER BY score DESC) as subject_rank ' +
    'FROM students ' +
    'ORDER BY subject, subject_rank');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('subject').AsString:8,
        ' ', DS.FieldByName('name').AsString:8,
        '  ', DS.FieldByName('score').AsInteger:3,
        '  rank=', DS.FieldByName('subject_rank').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Shows ACME stock prices with LAG for daily and 2-day price changes, and LEAD for next-day price. }
procedure DemoLagLead;
begin
  WriteLn('=== 3. LAG/LEAD - Previous/Next Row Access ===');
  WriteLn('');
  WriteLn('   ACME stock: daily price change and next-day price:');
  WriteLn('   Date        Close    Prev     Change   Next');
  WriteLn('   ' + StringOfChar('-', 55));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  trade_date, close_price,' +
    '  LAG(close_price, 1) OVER (ORDER BY trade_date) as prev_price,' +
    '  ROUND(close_price - LAG(close_price, 1) OVER (ORDER BY trade_date), 2) as daily_change,' +
    '  LEAD(close_price, 1) OVER (ORDER BY trade_date) as next_price ' +
    'FROM stock_prices ' +
    'WHERE symbol = ''ACME'' ' +
    'ORDER BY trade_date');
  try
    while not DS.EOF do
    begin
      Write('   ', DS.FieldByName('trade_date').AsString);
      Write('  $', DS.FieldByName('close_price').AsFloat:7:2);
      if DS.FieldByName('prev_price').IsNull then
        Write('      -  ')
      else
        Write('  $', DS.FieldByName('prev_price').AsFloat:7:2);
      if DS.FieldByName('daily_change').IsNull then
        Write('      -  ')
      else
        Write('  ', DS.FieldByName('daily_change').AsFloat:+7:2);
      if DS.FieldByName('next_price').IsNull then
        WriteLn('      -  ')
      else
        WriteLn('  $', DS.FieldByName('next_price').AsFloat:7:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   LAG with offset 2 (compare to 2 days ago):');

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  trade_date, close_price,' +
    '  LAG(close_price, 2) OVER (ORDER BY trade_date) as two_days_ago,' +
    '  ROUND(close_price - LAG(close_price, 2) OVER (ORDER BY trade_date), 2) as two_day_change ' +
    'FROM stock_prices ' +
    'WHERE symbol = ''ACME'' ' +
    'ORDER BY trade_date');
  try
    while not DS.EOF do
    begin
      Write('   ', DS.FieldByName('trade_date').AsString);
      Write('  $', DS.FieldByName('close_price').AsFloat:7:2);
      if DS.FieldByName('two_days_ago').IsNull then
        Write('      -     ')
      else
        Write('  $', DS.FieldByName('two_days_ago').AsFloat:7:2);
      if DS.FieldByName('two_day_change').IsNull then
        WriteLn('      -')
      else
        WriteLn('  ', DS.FieldByName('two_day_change').AsFloat:+7:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Computes cumulative sales totals for Alice and running totals partitioned by department using SUM OVER with ROWS frame. }
procedure DemoRunningTotals;
begin
  WriteLn('=== 4. Running Totals (Cumulative SUM) ===');
  WriteLn('');
  WriteLn('   Alice''s cumulative sales over time:');
  WriteLn('   Date         Amount    Running Total');
  WriteLn('   ' + StringOfChar('-', 40));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  sale_date, amount,' +
    '  SUM(amount) OVER (ORDER BY sale_date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as running_total ' +
    'FROM sales ' +
    'WHERE employee = ''Alice'' ' +
    'ORDER BY sale_date');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('sale_date').AsString,
        '  $', DS.FieldByName('amount').AsFloat:8:2,
        '  $', DS.FieldByName('running_total').AsFloat:9:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Running totals by department:');
  WriteLn('   Dept          Date         Amount    Dept Running Total');
  WriteLn('   ' + StringOfChar('-', 58));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  department, sale_date, amount,' +
    '  SUM(amount) OVER (PARTITION BY department ORDER BY sale_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as dept_running ' +
    'FROM sales ' +
    'ORDER BY department, sale_date');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('department').AsString:12,
        '  ', DS.FieldByName('sale_date').AsString,
        '  $', DS.FieldByName('amount').AsFloat:8:2,
        '  $', DS.FieldByName('dept_running').AsFloat:10:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Calculates 3-day moving average for ACME close price and 5-day moving average for TECH trading volume. }
procedure DemoMovingAverages;
begin
  WriteLn('=== 5. Moving Averages ===');
  WriteLn('');
  WriteLn('   ACME 3-day moving average price:');
  WriteLn('   Date         Close     3-Day MA');
  WriteLn('   ' + StringOfChar('-', 38));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  trade_date, close_price,' +
    '  ROUND(AVG(close_price) OVER (ORDER BY trade_date ' +
    '    ROWS BETWEEN 2 PRECEDING AND CURRENT ROW), 2) as ma_3day ' +
    'FROM stock_prices ' +
    'WHERE symbol = ''ACME'' ' +
    'ORDER BY trade_date');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('trade_date').AsString,
        '  $', DS.FieldByName('close_price').AsFloat:7:2,
        '  $', DS.FieldByName('ma_3day').AsFloat:7:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   TECH 5-day moving average volume:');
  WriteLn('   Date         Volume       5-Day MA Vol');
  WriteLn('   ' + StringOfChar('-', 42));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  trade_date, volume,' +
    '  CAST(AVG(volume) OVER (ORDER BY trade_date ' +
    '    ROWS BETWEEN 4 PRECEDING AND CURRENT ROW) AS INTEGER) as ma_5day_vol ' +
    'FROM stock_prices ' +
    'WHERE symbol = ''TECH'' ' +
    'ORDER BY trade_date');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('trade_date').AsString,
        '  ', DS.FieldByName('volume').AsInteger:10,
        '  ', DS.FieldByName('ma_5day_vol').AsInteger:10);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Demonstrates the NTILE window function for distributing rows into buckets. }
procedure DemoNtile;
begin
  WriteLn('=== 6. NTILE - Distribution into Buckets ===');
  WriteLn('');
  WriteLn('   Employees divided into 3 performance tiers (by total sales):');
  WriteLn('   Employee   Total Sales   Tier');
  WriteLn('   ' + StringOfChar('-', 35));

  DS := Conn.ExecuteQuery(
    'SELECT employee, total_sales, tier FROM (' +
    '  SELECT ' +
    '    employee,' +
    '    SUM(amount) as total_sales,' +
    '    NTILE(3) OVER (ORDER BY SUM(amount) DESC) as tier ' +
    '  FROM sales ' +
    '  GROUP BY employee' +
    ') ORDER BY tier, total_sales DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('employee').AsString:8,
        '  $', DS.FieldByName('total_sales').AsFloat:9:2,
        '   Tier ', DS.FieldByName('tier').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Quartile distribution of individual sale amounts:');
  WriteLn('   Quartile   Min        Max        Count');
  WriteLn('   ' + StringOfChar('-', 42));

  DS := Conn.ExecuteQuery(
    'SELECT quartile, ' +
    '  MIN(amount) as min_amt, MAX(amount) as max_amt, COUNT(*) as cnt ' +
    'FROM (' +
    '  SELECT amount, NTILE(4) OVER (ORDER BY amount) as quartile FROM sales' +
    ') GROUP BY quartile ORDER BY quartile');
  try
    while not DS.EOF do
    begin
      WriteLn('   Q', DS.FieldByName('quartile').AsInteger,
        '       $', DS.FieldByName('min_amt').AsFloat:8:2,
        '  $', DS.FieldByName('max_amt').AsFloat:8:2,
        '  ', DS.FieldByName('cnt').AsInteger:5);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Compares each ACME daily close to the period's first and last prices, and shows first/last sale dates per employee. }
procedure DemoFirstLastValue;
begin
  WriteLn('=== 7. FIRST_VALUE / LAST_VALUE ===');
  WriteLn('');
  WriteLn('   Each ACME trade vs first and last price of the period:');
  WriteLn('   Date         Close     First     Last      vs First   vs Last');
  WriteLn('   ' + StringOfChar('-', 65));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  trade_date, close_price,' +
    '  FIRST_VALUE(close_price) OVER (ORDER BY trade_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) as first_price,' +
    '  LAST_VALUE(close_price) OVER (ORDER BY trade_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) as last_price,' +
    '  ROUND(close_price - FIRST_VALUE(close_price) OVER (ORDER BY trade_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING), 2) as vs_first,' +
    '  ROUND(close_price - LAST_VALUE(close_price) OVER (ORDER BY trade_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING), 2) as vs_last ' +
    'FROM stock_prices ' +
    'WHERE symbol = ''ACME'' ' +
    'ORDER BY trade_date');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('trade_date').AsString,
        '  $', DS.FieldByName('close_price').AsFloat:7:2,
        '  $', DS.FieldByName('first_price').AsFloat:7:2,
        '  $', DS.FieldByName('last_price').AsFloat:7:2,
        '  ', DS.FieldByName('vs_first').AsFloat:+7:2,
        '  ', DS.FieldByName('vs_last').AsFloat:+7:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   First and last sale per employee:');

  DS := Conn.ExecuteQuery(
    'SELECT DISTINCT ' +
    '  employee,' +
    '  FIRST_VALUE(sale_date) OVER (PARTITION BY employee ORDER BY sale_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) as first_sale,' +
    '  LAST_VALUE(sale_date) OVER (PARTITION BY employee ORDER BY sale_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) as last_sale,' +
    '  FIRST_VALUE(amount) OVER (PARTITION BY employee ORDER BY sale_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) as first_amount,' +
    '  LAST_VALUE(amount) OVER (PARTITION BY employee ORDER BY sale_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) as last_amount ' +
    'FROM sales ' +
    'ORDER BY employee');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('employee').AsString:8,
        '  first: ', DS.FieldByName('first_sale').AsString, ' ($', DS.FieldByName('first_amount').AsFloat:0:2, ')',
        '  last: ', DS.FieldByName('last_sale').AsString, ' ($', DS.FieldByName('last_amount').AsFloat:0:2, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Shows sales with department averages, totals, and percentage contribution without GROUP BY, plus running counts per region. }
procedure DemoAggregateWindows;
begin
  WriteLn('=== 8. Aggregate Window Functions (SUM, AVG, COUNT, MIN, MAX) ===');
  WriteLn('');
  WriteLn('   Each sale with department aggregates (without GROUP BY):');
  WriteLn('   Employee   Dept          Amount    Dept Avg    Dept Total   % of Dept');
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  employee, department, amount,' +
    '  ROUND(AVG(amount) OVER (PARTITION BY department), 2) as dept_avg,' +
    '  SUM(amount) OVER (PARTITION BY department) as dept_total,' +
    '  ROUND(amount * 100.0 / SUM(amount) OVER (PARTITION BY department), 1) as pct_of_dept ' +
    'FROM sales ' +
    'ORDER BY department, amount DESC ' +
    'LIMIT 15');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('employee').AsString:8,
        '  ', DS.FieldByName('department').AsString:12,
        '  $', DS.FieldByName('amount').AsFloat:8:2,
        '  $', DS.FieldByName('dept_avg').AsFloat:8:2,
        '  $', DS.FieldByName('dept_total').AsFloat:9:2,
        '  ', DS.FieldByName('pct_of_dept').AsFloat:5:1, '%');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('   ...');

  WriteLn('');
  WriteLn('   Count of sales per region with running count:');

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  employee, region, sale_date,' +
    '  COUNT(*) OVER (PARTITION BY region ORDER BY sale_date ' +
    '    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as running_count,' +
    '  COUNT(*) OVER (PARTITION BY region) as region_total ' +
    'FROM sales ' +
    'WHERE employee IN (''Alice'', ''Charlie'', ''Eve'') ' +
    'ORDER BY region, sale_date ' +
    'LIMIT 10');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('employee').AsString:8,
        '  ', DS.FieldByName('region').AsString:6,
        '  ', DS.FieldByName('sale_date').AsString,
        '  count: ', DS.FieldByName('running_count').AsInteger,
        '/', DS.FieldByName('region_total').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Calculates PERCENT_RANK and CUME_DIST for Math student scores to show percentile positioning. }
procedure DemoPercentRank;
begin
  WriteLn('=== 9. PERCENT_RANK and CUME_DIST ===');
  WriteLn('');
  WriteLn('   Student percentile rankings in Math:');
  WriteLn('   Name       Score   PERCENT_RANK   CUME_DIST');
  WriteLn('   ' + StringOfChar('-', 48));

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  name, score,' +
    '  ROUND(PERCENT_RANK() OVER (ORDER BY score), 4) as pct_rank,' +
    '  ROUND(CUME_DIST() OVER (ORDER BY score), 4) as cume_dist ' +
    'FROM students ' +
    'WHERE subject = ''Math'' ' +
    'ORDER BY score DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:8,
        '  ', DS.FieldByName('score').AsInteger:5,
        '  ', DS.FieldByName('pct_rank').AsFloat:12:4,
        '  ', DS.FieldByName('cume_dist').AsFloat:9:4);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

{ Combines CTE with window functions to show employee rankings, department comparisons, week-over-week growth, and summary statistics. }
procedure DemoComplexExample;
begin
  WriteLn('=== 10. Complex Example: Sales Performance Dashboard ===');
  WriteLn('');
  WriteLn('   Employee performance with multiple window functions:');
  WriteLn('   Employee    Dept          Total     Rank  vs Dept Avg   Trend');
  WriteLn('   ' + StringOfChar('-', 65));

  DS := Conn.ExecuteQuery(
    'WITH employee_totals AS (' +
    '  SELECT ' +
    '    employee, department,' +
    '    SUM(amount) as total_sales,' +
    '    COUNT(*) as num_sales ' +
    '  FROM sales ' +
    '  GROUP BY employee, department' +
    ')' +
    'SELECT ' +
    '  employee, department, total_sales,' +
    '  RANK() OVER (ORDER BY total_sales DESC) as overall_rank,' +
    '  ROUND(total_sales - AVG(total_sales) OVER (PARTITION BY department), 2) as vs_dept_avg,' +
    '  CASE ' +
    '    WHEN total_sales > AVG(total_sales) OVER (PARTITION BY department) THEN ''Above'' ' +
    '    ELSE ''Below'' ' +
    '  END as trend ' +
    'FROM employee_totals ' +
    'ORDER BY overall_rank');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('employee').AsString:8,
        '   ', DS.FieldByName('department').AsString:12,
        '  $', DS.FieldByName('total_sales').AsFloat:9:2,
        '  #', DS.FieldByName('overall_rank').AsInteger,
        '  ', DS.FieldByName('vs_dept_avg').AsFloat:+9:2,
        '   ', DS.FieldByName('trend').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Week-over-week growth per employee:');

  DS := Conn.ExecuteQuery(
    'WITH weekly_sales AS (' +
    '  SELECT ' +
    '    employee, sale_date, amount,' +
    '    LAG(amount) OVER (PARTITION BY employee ORDER BY sale_date) as prev_amount ' +
    '  FROM sales' +
    ')' +
    'SELECT ' +
    '  employee, sale_date, amount, prev_amount,' +
    '  CASE WHEN prev_amount IS NOT NULL AND prev_amount > 0 THEN ' +
    '    ROUND((amount - prev_amount) * 100.0 / prev_amount, 1) ' +
    '  ELSE NULL END as growth_pct ' +
    'FROM weekly_sales ' +
    'WHERE employee IN (''Alice'', ''Bob'') ' +
    'ORDER BY employee, sale_date');
  try
    while not DS.EOF do
    begin
      Write('   ', DS.FieldByName('employee').AsString:8,
        '  ', DS.FieldByName('sale_date').AsString,
        '  $', DS.FieldByName('amount').AsFloat:8:2);
      if DS.FieldByName('growth_pct').IsNull then
        WriteLn('      -')
      else
        WriteLn('  ', DS.FieldByName('growth_pct').AsFloat:+6:1, '%');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Summary statistics:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  COUNT(DISTINCT employee) as employees,' +
    '  COUNT(*) as total_sales,' +
    '  CAST(SUM(amount) AS INTEGER) as total_revenue,' +
    '  CAST(ROUND(AVG(amount), 0) AS INTEGER) as avg_sale ' +
    'FROM sales');
  try
    if not DS.EOF then
    begin
      WriteLn('   Employees: ', DS.FieldByName('employees').AsInteger,
        ', Sales: ', DS.FieldByName('total_sales').AsInteger,
        ', Revenue: $', DS.FieldByName('total_revenue').AsInteger,
        ', Avg: $', DS.FieldByName('avg_sale').AsInteger);
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

begin
  WriteLn('Example 95: Window Functions');
  WriteLn(StringOfChar('=', 50));
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    WriteLn('Setting up sample data...');
    SetupDatabase;
    WriteLn('Done. Tables: sales (26 rows), students (15 rows), stock_prices (20 rows)');
    WriteLn('');

    DemoRowNumber;
    DemoRankDenseRank;
    DemoLagLead;
    DemoRunningTotals;
    DemoMovingAverages;
    DemoNtile;
    DemoFirstLastValue;
    DemoAggregateWindows;
    DemoPercentRank;
    DemoComplexExample;

    WriteLn('=== Window Functions Summary ===');
    WriteLn('');
    WriteLn('   Functions demonstrated:');
    WriteLn('   - ROW_NUMBER(): Sequential numbering within partitions');
    WriteLn('   - RANK()/DENSE_RANK(): Ranking with tie handling');
    WriteLn('   - LAG()/LEAD(): Access previous/next row values');
    WriteLn('   - SUM() OVER: Running totals (cumulative sums)');
    WriteLn('   - AVG() OVER: Moving averages (sliding windows)');
    WriteLn('   - NTILE(): Equal-sized bucket distribution');
    WriteLn('   - FIRST_VALUE()/LAST_VALUE(): First/last in window');
    WriteLn('   - COUNT()/MIN()/MAX() OVER: Aggregate windows');
    WriteLn('   - PERCENT_RANK()/CUME_DIST(): Percentile calculations');
    WriteLn('');
    WriteLn('   Frame specifications used:');
    WriteLn('   - ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW');
    WriteLn('   - ROWS BETWEEN 2 PRECEDING AND CURRENT ROW');
    WriteLn('   - ROWS BETWEEN 4 PRECEDING AND CURRENT ROW');
    WriteLn('   - ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING');
    WriteLn('');

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('Done.');
end.
