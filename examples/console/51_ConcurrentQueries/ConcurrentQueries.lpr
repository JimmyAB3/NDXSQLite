{===============================================================================
  NDXSQLite Example 51 - Concurrent Queries
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:

  - Multiple ExecuteNonQuery (INSERT, UPDATE, DELETE)
  - Multiple ExecuteQuery (SELECT returning datasets)
  - Multiple ExecuteScalar (SELECT returning single values)
  - All operations mixed while datasets remain open

  This showcases the fix that allows ExecuteNonQuery to run without
  invalidating open datasets from ExecuteQuery.

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ConcurrentQueries;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection;

var
  Connection: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('Creating database schema...');
  WriteLn('');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER DEFAULT 0' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS sales (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  product_id INTEGER NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  sale_date TEXT DEFAULT CURRENT_TIMESTAMP,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')');
end;

{ Inserts multiple product records sequentially using ExecuteNonQuery and reports affected rows. }
procedure DemoMultipleExecuteNonQuery;
var
  Rows1, Rows2, Rows3, Rows4, Rows5: Integer;
begin
  WriteLn('1. Multiple ExecuteNonQuery on same connection');
  WriteLn('   -------------------------------------------');
  WriteLn('');

  // Execute multiple INSERT statements in sequence
  Rows1 := Connection.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, stock) VALUES (?, ?, ?, ?)',
    ['Laptop', 'Electronics', 999.99, 50]);
  WriteLn('   INSERT Laptop: ', Rows1, ' row affected');

  Rows2 := Connection.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, stock) VALUES (?, ?, ?, ?)',
    ['Mouse', 'Electronics', 29.99, 200]);
  WriteLn('   INSERT Mouse: ', Rows2, ' row affected');

  Rows3 := Connection.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, stock) VALUES (?, ?, ?, ?)',
    ['Keyboard', 'Electronics', 79.99, 150]);
  WriteLn('   INSERT Keyboard: ', Rows3, ' row affected');

  Rows4 := Connection.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, stock) VALUES (?, ?, ?, ?)',
    ['Desk Chair', 'Furniture', 299.99, 30]);
  WriteLn('   INSERT Desk Chair: ', Rows4, ' row affected');

  Rows5 := Connection.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, stock) VALUES (?, ?, ?, ?)',
    ['Monitor', 'Electronics', 399.99, 75]);
  WriteLn('   INSERT Monitor: ', Rows5, ' row affected');

  WriteLn('');
  WriteLn('   Total: ', Rows1 + Rows2 + Rows3 + Rows4 + Rows5, ' products inserted');
  WriteLn('');
end;

{ Runs multiple aggregate scalar queries (COUNT, SUM, AVG, MAX, MIN) and prints inventory statistics. }
procedure DemoMultipleExecuteScalar;
var
  TotalProducts, TotalStock, ElectronicsCount: Integer;
  AvgPrice, MaxPrice, MinPrice, TotalValue: Double;
begin
  WriteLn('2. Multiple ExecuteScalar on same connection');
  WriteLn('   -----------------------------------------');
  WriteLn('');

  // Execute multiple scalar queries in sequence
  TotalProducts := Connection.ExecuteScalar('SELECT COUNT(*) FROM products');
  WriteLn('   Total products: ', TotalProducts);

  TotalStock := Connection.ExecuteScalar('SELECT SUM(stock) FROM products');
  WriteLn('   Total stock: ', TotalStock, ' units');

  AvgPrice := Connection.ExecuteScalar('SELECT AVG(price) FROM products');
  WriteLn('   Average price: $', AvgPrice:0:2);

  MaxPrice := Connection.ExecuteScalar('SELECT MAX(price) FROM products');
  WriteLn('   Highest price: $', MaxPrice:0:2);

  MinPrice := Connection.ExecuteScalar('SELECT MIN(price) FROM products');
  WriteLn('   Lowest price: $', MinPrice:0:2);

  ElectronicsCount := Connection.ExecuteScalar(
    'SELECT COUNT(*) FROM products WHERE category = ?', ['Electronics']);
  WriteLn('   Electronics count: ', ElectronicsCount);

  TotalValue := Connection.ExecuteScalar('SELECT SUM(price * stock) FROM products');
  WriteLn('   Total inventory value: $', TotalValue:0:2);

  WriteLn('');
end;

{ Opens and iterates two separate ExecuteQuery datasets (Electronics and Furniture) in sequence. }
procedure DemoMultipleExecuteQuery;
var
  DS1, DS2: TDataSet;
  Count: Integer;
begin
  WriteLn('3. Multiple ExecuteQuery on same connection');
  WriteLn('   ----------------------------------------');
  WriteLn('');

  // First query - Electronics
  WriteLn('   Query 1: Electronics products');
  DS1 := Connection.ExecuteQuery(
    'SELECT name, price, stock FROM products WHERE category = ? ORDER BY price DESC',
    ['Electronics']);
  try
    Count := 0;
    while not DS1.EOF do
    begin
      Inc(Count);
      WriteLn('   ', Count, '. ', DS1.FieldByName('name').AsString,
              ' - $', DS1.FieldByName('price').AsFloat:0:2,
              ' (', DS1.FieldByName('stock').AsInteger, ' in stock)');
      DS1.Next;
    end;
  finally
    DS1.Free;
  end;

  WriteLn('');

  // Second query - Furniture
  WriteLn('   Query 2: Furniture products');
  DS2 := Connection.ExecuteQuery(
    'SELECT name, price, stock FROM products WHERE category = ? ORDER BY price DESC',
    ['Furniture']);
  try
    Count := 0;
    while not DS2.EOF do
    begin
      Inc(Count);
      WriteLn('   ', Count, '. ', DS2.FieldByName('name').AsString,
              ' - $', DS2.FieldByName('price').AsFloat:0:2,
              ' (', DS2.FieldByName('stock').AsInteger, ' in stock)');
      DS2.Next;
    end;
  finally
    DS2.Free;
  end;

  WriteLn('');
end;

{ Opens three datasets simultaneously (by category and by price) and reads from all while they remain open. }
procedure DemoMultipleOpenDatasets;
var
  DSElectronics, DSFurniture, DSExpensive: TDataSet;
begin
  WriteLn('4. Multiple DataSets open simultaneously');
  WriteLn('   -------------------------------------');
  WriteLn('');

  // Open three datasets at the same time
  DSElectronics := Connection.ExecuteQuery(
    'SELECT name, price FROM products WHERE category = ?', ['Electronics']);
  DSFurniture := Connection.ExecuteQuery(
    'SELECT name, price FROM products WHERE category = ?', ['Furniture']);
  DSExpensive := Connection.ExecuteQuery(
    'SELECT name, price FROM products WHERE price > ? ORDER BY price DESC', [100]);

  try
    // Read from all three while they're all open
    WriteLn('   Electronics (first): ', DSElectronics.FieldByName('name').AsString,
            ' - $', DSElectronics.FieldByName('price').AsFloat:0:2);

    WriteLn('   Furniture (first): ', DSFurniture.FieldByName('name').AsString,
            ' - $', DSFurniture.FieldByName('price').AsFloat:0:2);

    WriteLn('   Most expensive: ', DSExpensive.FieldByName('name').AsString,
            ' - $', DSExpensive.FieldByName('price').AsFloat:0:2);

    // Navigate in one dataset while others are open
    WriteLn('');
    WriteLn('   Iterating expensive products while other datasets are open:');
    while not DSExpensive.EOF do
    begin
      WriteLn('   - ', DSExpensive.FieldByName('name').AsString,
              ': $', DSExpensive.FieldByName('price').AsFloat:0:2);
      DSExpensive.Next;
    end;

  finally
    DSElectronics.Free;
    DSFurniture.Free;
    DSExpensive.Free;
  end;

  WriteLn('');
end;

{ Executes UPDATE and INSERT statements while a dataset remains open, then verifies the dataset is still valid. }
procedure DemoExecuteNonQueryWithOpenDataset;
var
  DS: TDataSet;
  ProductName: string;
  OriginalStock, NewStock: Integer;
  RowsAffected: Integer;
begin
  WriteLn('5. ExecuteNonQuery while DataSet is open');
  WriteLn('   -------------------------------------');
  WriteLn('');

  // Open a dataset
  DS := Connection.ExecuteQuery(
    'SELECT id, name, stock FROM products WHERE name = ?', ['Laptop']);
  try
    ProductName := DS.FieldByName('name').AsString;
    OriginalStock := DS.FieldByName('stock').AsInteger;
    WriteLn('   Opened dataset for: ', ProductName);
    WriteLn('   Original stock: ', OriginalStock);

    // Execute UPDATE while the dataset is still open
    WriteLn('');
    WriteLn('   Executing UPDATE while dataset is open...');
    RowsAffected := Connection.ExecuteNonQuery(
      'UPDATE products SET stock = stock + 10 WHERE name = ?', ['Mouse']);
    WriteLn('   Updated Mouse stock: ', RowsAffected, ' row affected');

    // Execute INSERT while the dataset is still open
    RowsAffected := Connection.ExecuteNonQuery(
      'INSERT INTO products (name, category, price, stock) VALUES (?, ?, ?, ?)',
      ['Webcam', 'Electronics', 89.99, 100]);
    WriteLn('   Inserted Webcam: ', RowsAffected, ' row affected');

    // The original dataset should still be valid
    WriteLn('');
    WriteLn('   Checking original dataset is still valid...');
    WriteLn('   Product name: ', DS.FieldByName('name').AsString);
    WriteLn('   Stock value: ', DS.FieldByName('stock').AsInteger);

    if DS.FieldByName('name').AsString = ProductName then
      WriteLn('   SUCCESS: Dataset remained valid after ExecuteNonQuery!')
    else
      WriteLn('   ERROR: Dataset was invalidated!');

  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Iterates a products dataset while inserting sales and updating stock, then opens a second dataset to compute revenue. }
procedure DemoAllOperationsCombined;
var
  DSProducts, DSSales: TDataSet;
  ProductId, TotalSales, SaleCount: Integer;
  ProductName: string;
  Revenue: Double;
begin
  WriteLn('6. All operations combined (real-world scenario)');
  WriteLn('   ----------------------------------------------');
  WriteLn('');
  WriteLn('   Scenario: Process sales while browsing products');
  WriteLn('');

  // Open products dataset
  DSProducts := Connection.ExecuteQuery(
    'SELECT id, name, price, stock FROM products WHERE stock > 0 ORDER BY name');
  try
    WriteLn('   Browsing available products:');
    while not DSProducts.EOF do
    begin
      ProductId := DSProducts.FieldByName('id').AsInteger;
      ProductName := DSProducts.FieldByName('name').AsString;

      WriteLn('   - ', ProductName, ' (ID: ', ProductId, ', Stock: ',
              DSProducts.FieldByName('stock').AsInteger, ')');

      // Record a sale while iterating (ExecuteNonQuery with open dataset)
      if DSProducts.FieldByName('stock').AsInteger > 100 then
      begin
        Connection.ExecuteNonQuery(
          'INSERT INTO sales (product_id, quantity) VALUES (?, ?)',
          [ProductId, 5]);
        Connection.ExecuteNonQuery(
          'UPDATE products SET stock = stock - 5 WHERE id = ?', [ProductId]);
        WriteLn('     ^ Sold 5 units');
      end;

      DSProducts.Next;
    end;

    WriteLn('');

    // Execute scalars while products dataset might still be referenced
    TotalSales := Connection.ExecuteScalar('SELECT COUNT(*) FROM sales');
    WriteLn('   Total sales transactions: ', TotalSales);

    SaleCount := Connection.ExecuteScalar('SELECT COALESCE(SUM(quantity), 0) FROM sales');
    WriteLn('   Total units sold: ', SaleCount);

    // Open another dataset while first one exists
    DSSales := Connection.ExecuteQuery(
      'SELECT p.name, s.quantity, p.price, (s.quantity * p.price) as total ' +
      'FROM sales s JOIN products p ON s.product_id = p.id');
    try
      WriteLn('');
      WriteLn('   Sales details:');
      Revenue := 0;
      while not DSSales.EOF do
      begin
        WriteLn('   - ', DSSales.FieldByName('name').AsString,
                ': ', DSSales.FieldByName('quantity').AsInteger,
                ' units = $', DSSales.FieldByName('total').AsFloat:0:2);
        Revenue := Revenue + DSSales.FieldByName('total').AsFloat;
        DSSales.Next;
      end;
      WriteLn('');
      WriteLn('   Total revenue: $', Revenue:0:2);

    finally
      DSSales.Free;
    end;

  finally
    DSProducts.Free;
  end;

  WriteLn('');
end;

{ Opens five datasets simultaneously, performs inserts and scalar queries, then verifies all datasets remain valid. }
procedure DemoStressTest;
var
  DS1, DS2, DS3, DS4, DS5: TDataSet;
  I, Count, Total: Integer;
begin
  WriteLn('7. Stress test: 5 open datasets + multiple operations');
  WriteLn('   ---------------------------------------------------');
  WriteLn('');

  // Open 5 datasets simultaneously
  DS1 := Connection.ExecuteQuery('SELECT * FROM products WHERE id = 1');
  DS2 := Connection.ExecuteQuery('SELECT * FROM products WHERE id = 2');
  DS3 := Connection.ExecuteQuery('SELECT * FROM products WHERE id = 3');
  DS4 := Connection.ExecuteQuery('SELECT * FROM products WHERE category = ?', ['Electronics']);
  DS5 := Connection.ExecuteQuery('SELECT * FROM sales');

  try
    WriteLn('   Opened 5 datasets simultaneously');
    WriteLn('');

    // Perform multiple operations while all datasets are open
    WriteLn('   Performing operations while all datasets are open:');

    for I := 1 to 5 do
    begin
      // ExecuteNonQuery
      Connection.ExecuteNonQuery(
        'INSERT INTO sales (product_id, quantity) VALUES (?, ?)', [1, I]);

      // ExecuteScalar
      Count := Connection.ExecuteScalar('SELECT COUNT(*) FROM sales');

      WriteLn('   Iteration ', I, ': Inserted sale, total sales = ', Count);
    end;

    WriteLn('');

    // Verify all datasets are still valid
    WriteLn('   Verifying all datasets are still valid:');
    WriteLn('   DS1 (product 1): ', DS1.FieldByName('name').AsString);
    WriteLn('   DS2 (product 2): ', DS2.FieldByName('name').AsString);
    WriteLn('   DS3 (product 3): ', DS3.FieldByName('name').AsString);

    Total := 0;
    DS4.First;
    while not DS4.EOF do
    begin
      Inc(Total);
      DS4.Next;
    end;
    WriteLn('   DS4 (electronics): ', Total, ' products');

    Total := 0;
    DS5.First;
    while not DS5.EOF do
    begin
      Inc(Total);
      DS5.Next;
    end;
    WriteLn('   DS5 (sales): ', Total, ' records');

    WriteLn('');
    WriteLn('   SUCCESS: All datasets remained valid!');

  finally
    DS1.Free;
    DS2.Free;
    DS3.Free;
    DS4.Free;
    DS5.Free;
  end;

  WriteLn('');
end;

{ Queries and prints final product count, sales transactions, total units sold, and total revenue. }
procedure PrintFinalStats;
var
  Products, Sales, TotalUnits: Integer;
  TotalRevenue: Double;
begin
  WriteLn('8. Final statistics');
  WriteLn('   -----------------');
  WriteLn('');

  Products := Connection.ExecuteScalar('SELECT COUNT(*) FROM products');
  Sales := Connection.ExecuteScalar('SELECT COUNT(*) FROM sales');
  TotalUnits := Connection.ExecuteScalar('SELECT COALESCE(SUM(quantity), 0) FROM sales');
  TotalRevenue := Connection.ExecuteScalar(
    'SELECT COALESCE(SUM(s.quantity * p.price), 0) FROM sales s JOIN products p ON s.product_id = p.id');

  WriteLn('   Products in catalog: ', Products);
  WriteLn('   Sales transactions: ', Sales);
  WriteLn('   Total units sold: ', TotalUnits);
  WriteLn('   Total revenue: $', TotalRevenue:0:2);
  WriteLn('');
end;

begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 51: Concurrent Queries ===');
  WriteLn('');

  Connection := TNDXSQLiteConnection.Create(':memory:', False);
  try
    Connection.Open;

    CreateSchema;

    DemoMultipleExecuteNonQuery;
    DemoMultipleExecuteScalar;
    DemoMultipleExecuteQuery;
    DemoMultipleOpenDatasets;
    DemoExecuteNonQueryWithOpenDataset;
    DemoAllOperationsCombined;
    DemoStressTest;
    PrintFinalStats;

    Connection.Close;
  finally
    Connection.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
