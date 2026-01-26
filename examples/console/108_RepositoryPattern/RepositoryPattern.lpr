{===============================================================================
  NDXSQLite Example 108 - Repository Pattern
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Generic repository with specification pattern
  - Query builder for composable queries
  - Specification combinators (AND, OR, NOT)
  - CRUD operations through repository interface
  - Category and product domain examples

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program RepositoryPattern;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

// ============================================================
// Repository Infrastructure
// ============================================================

type
  // Specification: encapsulates a WHERE clause
  TSpecification = record
    WhereClause: string;
  end;

  // Query Builder: builds SELECT statements fluently
  TQueryBuilder = record
    FTable: string;
    FColumns: string;
    FWhere: string;
    FOrderBy: string;
    FLimit: Integer;
    FOffset: Integer;
  end;

// --- Specification Constructors ---

{ Creates a specification that matches all records. }
function SpecAll: TSpecification;
begin
  Result.WhereClause := '1=1';
end;

{ Creates a specification filtering products by category ID. }
function SpecByCategory(CategoryId: Integer): TSpecification;
begin
  Result.WhereClause := Format('category_id = %d', [CategoryId]);
end;

{ Creates a specification filtering products within a price range. }
function SpecPriceRange(MinPrice, MaxPrice: Double): TSpecification;
begin
  Result.WhereClause := Format('price >= %.2f AND price <= %.2f', [MinPrice, MaxPrice]);
end;

{ Creates a specification filtering products that are in stock. }
function SpecInStock: TSpecification;
begin
  Result.WhereClause := 'stock > 0';
end;

{ Creates a specification filtering active products only. }
function SpecActive: TSpecification;
begin
  Result.WhereClause := 'is_active = 1';
end;

{ Creates a specification filtering products with stock above a minimum. }
function SpecMinStock(MinQty: Integer): TSpecification;
begin
  Result.WhereClause := Format('stock >= %d', [MinQty]);
end;

// Combine specifications with AND
{ Combines two specifications with a logical AND. }
function SpecAnd(A, B: TSpecification): TSpecification;
begin
  Result.WhereClause := '(' + A.WhereClause + ') AND (' + B.WhereClause + ')';
end;

// Combine specifications with OR
{ Combines two specifications with a logical OR. }
function SpecOr(A, B: TSpecification): TSpecification;
begin
  Result.WhereClause := '(' + A.WhereClause + ') OR (' + B.WhereClause + ')';
end;

// Negate a specification
{ Negates a specification with a logical NOT. }
function SpecNot(A: TSpecification): TSpecification;
begin
  Result.WhereClause := 'NOT (' + A.WhereClause + ')';
end;

// --- Query Builder Functions ---

{ Creates a new query builder for the specified table. }
function NewQuery(const Table: string): TQueryBuilder;
begin
  Result.FTable := Table;
  Result.FColumns := '*';
  Result.FWhere := '';
  Result.FOrderBy := '';
  Result.FLimit := 0;
  Result.FOffset := 0;
end;

{ Sets the columns to select in the query builder. }
function QSelect(Q: TQueryBuilder; const Columns: string): TQueryBuilder;
begin
  Result := Q;
  Result.FColumns := Columns;
end;

{ Adds a WHERE condition to the query builder. }
function QWhere(Q: TQueryBuilder; const Condition: string): TQueryBuilder;
begin
  Result := Q;
  if Result.FWhere = '' then
    Result.FWhere := Condition
  else
    Result.FWhere := Result.FWhere + ' AND ' + Condition;
end;

{ Adds a specification's condition to the query builder. }
function QWhereSpec(Q: TQueryBuilder; Spec: TSpecification): TQueryBuilder;
begin
  Result := QWhere(Q, Spec.WhereClause);
end;

{ Sets the ORDER BY clause in the query builder. }
function QOrderBy(Q: TQueryBuilder; const OrderClause: string): TQueryBuilder;
begin
  Result := Q;
  Result.FOrderBy := OrderClause;
end;

{ Sets the LIMIT clause in the query builder. }
function QLimit(Q: TQueryBuilder; Limit: Integer): TQueryBuilder;
begin
  Result := Q;
  Result.FLimit := Limit;
end;

{ Sets the OFFSET clause in the query builder. }
function QOffset(Q: TQueryBuilder; OffsetVal: Integer): TQueryBuilder;
begin
  Result := Q;
  Result.FOffset := OffsetVal;
end;

{ Builds and returns the final SQL query string. }
function QBuild(Q: TQueryBuilder): string;
begin
  Result := 'SELECT ' + Q.FColumns + ' FROM ' + Q.FTable;
  if Q.FWhere <> '' then
    Result := Result + ' WHERE ' + Q.FWhere;
  if Q.FOrderBy <> '' then
    Result := Result + ' ORDER BY ' + Q.FOrderBy;
  if Q.FLimit > 0 then
    Result := Result + Format(' LIMIT %d', [Q.FLimit]);
  if Q.FOffset > 0 then
    Result := Result + Format(' OFFSET %d', [Q.FOffset]);
end;

// --- Generic Repository Operations ---

{ Returns the count of records matching the specification. }
function RepoCount(const Table: string; Spec: TSpecification): Integer;
begin
  Result := Integer(Conn.ExecuteScalar(
    Format('SELECT COUNT(*) FROM %s WHERE %s', [Table, Spec.WhereClause])));
end;

{ Checks whether a record with the given ID exists in the table. }
function RepoExists(const Table: string; Id: Integer): Boolean;
begin
  Result := Integer(Conn.ExecuteScalar(
    Format('SELECT COUNT(*) FROM %s WHERE id = %d', [Table, Id]))) > 0;
end;

{ Deletes the record with the given ID from the specified table. }
procedure RepoDelete(const Table: string; Id: Integer);
begin
  Conn.ExecuteNonQuery(Format('DELETE FROM %s WHERE id = %d', [Table, Id]));
end;

{ Marks the record with the given ID as inactive by setting is_active to 0. }
procedure RepoSoftDelete(const Table: string; Id: Integer);
begin
  Conn.ExecuteNonQuery(Format('UPDATE %s SET is_active = 0 WHERE id = %d', [Table, Id]));
end;

// ============================================================
// Domain Setup
// ============================================================

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  Conn.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT UNIQUE NOT NULL,' +
    '  description TEXT' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category_id INTEGER REFERENCES categories(id),' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER DEFAULT 0,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  created_at TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery('CREATE INDEX idx_products_category ON products(category_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_products_price ON products(price)');
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  // Categories
  Conn.ExecuteNonQuery('INSERT INTO categories (name, description) VALUES (''Electronics'', ''Electronic devices and gadgets'')');
  Conn.ExecuteNonQuery('INSERT INTO categories (name, description) VALUES (''Books'', ''Physical and digital books'')');
  Conn.ExecuteNonQuery('INSERT INTO categories (name, description) VALUES (''Clothing'', ''Apparel and accessories'')');

  // Products - Electronics (category 1)
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''Laptop Pro 15'', 1, 1299.99, 25, ''2024-01-15'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''Wireless Mouse'', 1, 29.99, 150, ''2024-01-20'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''USB-C Hub'', 1, 49.99, 80, ''2024-02-01'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''4K Monitor'', 1, 599.99, 0, ''2024-02-10'')');

  // Products - Books (category 2)
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''Clean Code'', 2, 39.99, 45, ''2024-01-05'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''Design Patterns'', 2, 44.99, 30, ''2024-01-10'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''Refactoring'', 2, 42.50, 0, ''2024-02-15'')');

  // Products - Clothing (category 3)
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''Dev T-Shirt'', 3, 24.99, 200, ''2024-01-25'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, created_at) VALUES (''Hoodie XL'', 3, 59.99, 35, ''2024-02-05'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category_id, price, stock, is_active, created_at) VALUES (''Vintage Cap'', 3, 19.99, 5, 0, ''2024-01-01'')');
end;

// ============================================================
// Feature Demonstrations
// ============================================================

// === Feature 1: CRUD Operations ===
{ Demonstrates basic CRUD operations through the repository pattern. }
procedure DemoCRUD;
var
  NewId: Integer;
begin
  WriteLn('=== 1. Repository CRUD Operations ===');
  WriteLn;

  // CREATE
  Conn.ExecuteNonQuery(
    'INSERT INTO products (name, category_id, price, stock, created_at) ' +
    'VALUES (''Mechanical Keyboard'', 1, 89.99, 50, ''2024-03-01'')');
  NewId := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
  WriteLn(Format('   CREATE: Added "Mechanical Keyboard" with id=%d', [NewId]));

  // READ (GetById)
  DS := Conn.ExecuteQuery(Format('SELECT * FROM products WHERE id = %d', [NewId]));
  try
    WriteLn(Format('   READ:   id=%d name="%s" price=$%.2f stock=%d',
      [DS.FieldByName('id').AsInteger,
       DS.FieldByName('name').AsString,
       DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger]));
  finally
    DS.Free;
  end;

  // UPDATE
  Conn.ExecuteNonQuery(Format('UPDATE products SET price = 79.99, stock = 45 WHERE id = %d', [NewId]));
  DS := Conn.ExecuteQuery(Format('SELECT price, stock FROM products WHERE id = %d', [NewId]));
  try
    WriteLn(Format('   UPDATE: price=$%.2f stock=%d',
      [DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger]));
  finally
    DS.Free;
  end;

  // DELETE
  RepoDelete('products', NewId);
  WriteLn(Format('   DELETE: id=%d exists=%s', [NewId, BoolToStr(RepoExists('products', NewId), 'true', 'false')]));

  WriteLn;
end;

// === Feature 2: Specifications ===
{ Demonstrates the specification pattern for composable query filters. }
procedure DemoSpecifications;
var
  Spec: TSpecification;
begin
  WriteLn('=== 2. Specification Pattern ===');
  WriteLn;

  // Simple specifications
  WriteLn(Format('   All products:           %d', [RepoCount('products', SpecAll)]));
  WriteLn(Format('   Electronics:            %d', [RepoCount('products', SpecByCategory(1))]));
  WriteLn(Format('   Books:                  %d', [RepoCount('products', SpecByCategory(2))]));
  WriteLn(Format('   In stock:               %d', [RepoCount('products', SpecInStock)]));
  WriteLn(Format('   Active:                 %d', [RepoCount('products', SpecActive)]));
  WriteLn(Format('   Price $30-$50:          %d', [RepoCount('products', SpecPriceRange(30, 50))]));

  // Combined specifications
  WriteLn;
  WriteLn('   Combined specifications:');

  Spec := SpecAnd(SpecByCategory(1), SpecInStock);
  WriteLn(Format('   Electronics AND InStock: %d', [RepoCount('products', Spec)]));

  Spec := SpecAnd(SpecActive, SpecPriceRange(0, 50));
  WriteLn(Format('   Active AND under $50:   %d', [RepoCount('products', Spec)]));

  Spec := SpecAnd(SpecAnd(SpecByCategory(2), SpecInStock), SpecActive);
  WriteLn(Format('   Books AND InStock AND Active: %d', [RepoCount('products', Spec)]));

  Spec := SpecOr(SpecByCategory(1), SpecByCategory(2));
  WriteLn(Format('   Electronics OR Books:   %d', [RepoCount('products', Spec)]));

  Spec := SpecNot(SpecInStock);
  WriteLn(Format('   NOT InStock (out):      %d', [RepoCount('products', Spec)]));

  WriteLn;

  // Show matching products for a complex spec
  Spec := SpecAnd(SpecActive, SpecAnd(SpecInStock, SpecPriceRange(20, 60)));
  WriteLn('   Products matching: Active AND InStock AND $20-$60:');
  DS := Conn.ExecuteQuery(
    'SELECT name, price, stock FROM products WHERE ' + Spec.WhereClause + ' ORDER BY price');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %-20s $%6.2f  stock=%d',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('stock').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 3: Query Builder ===
{ Builds and executes three queries using the fluent query builder: top 3 active
  products by price, active in-stock products with category join, and a
  paginated query with offset. Prints the generated SQL and results. }
procedure DemoQueryBuilder;
var
  Q: TQueryBuilder;
  SQL: string;
begin
  WriteLn('=== 3. Query Builder ===');
  WriteLn;

  // Simple query
  Q := NewQuery('products');
  Q := QSelect(Q, 'name, price');
  Q := QWhere(Q, 'is_active = 1');
  Q := QOrderBy(Q, 'price DESC');
  Q := QLimit(Q, 3);
  SQL := QBuild(Q);
  WriteLn('   Query 1: Top 3 active products by price');
  WriteLn('   SQL: ' + SQL);
  DS := Conn.ExecuteQuery(SQL);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %-20s $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Query with specification
  WriteLn;
  Q := NewQuery('products');
  Q := QSelect(Q, 'p.name, c.name as category, p.price, p.stock');
  Q.FTable := 'products p JOIN categories c ON c.id = p.category_id';
  Q := QWhereSpec(Q, SpecAnd(SpecInStock, SpecActive));
  Q := QOrderBy(Q, 'c.name, p.name');
  SQL := QBuild(Q);
  WriteLn('   Query 2: Active in-stock products with category');
  WriteLn('   SQL: ' + SQL);
  DS := Conn.ExecuteQuery(SQL);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - [%-11s] %-20s $%6.2f  stock=%d',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('stock').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Paginated query
  WriteLn;
  Q := NewQuery('products');
  Q := QSelect(Q, 'name, price');
  Q := QWhereSpec(Q, SpecActive);
  Q := QOrderBy(Q, 'name');
  Q := QLimit(Q, 3);
  Q := QOffset(Q, 3);
  SQL := QBuild(Q);
  WriteLn('   Query 3: Page 2 (offset 3, limit 3) of active products');
  WriteLn('   SQL: ' + SQL);
  DS := Conn.ExecuteQuery(SQL);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %-20s $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 4: Aggregate Queries ===
{ Demonstrates aggregate queries through the repository. }
procedure DemoAggregates;
begin
  WriteLn('=== 4. Repository Aggregate Queries ===');
  WriteLn;

  // Count per category
  WriteLn('   Product count by category:');
  DS := Conn.ExecuteQuery(
    'SELECT c.name, COUNT(p.id) as cnt, ' +
    '  COALESCE(SUM(p.stock), 0) as total_stock, ' +
    '  COALESCE(CAST(AVG(p.price) AS REAL), 0) as avg_price ' +
    'FROM categories c ' +
    'LEFT JOIN products p ON p.category_id = c.id AND p.is_active = 1 ' +
    'GROUP BY c.id ORDER BY c.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d products, stock=%d, avg=$%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('total_stock').AsInteger,
         DS.FieldByName('avg_price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Inventory value
  DS := Conn.ExecuteQuery(
    'SELECT SUM(price * stock) as total_value, ' +
    '  SUM(stock) as total_units, ' +
    '  COUNT(*) as product_count ' +
    'FROM products WHERE is_active = 1 AND stock > 0');
  try
    WriteLn(Format('   Total inventory value: $%.2f',
      [DS.FieldByName('total_value').AsFloat]));
    WriteLn(Format('   Total units in stock:  %d',
      [DS.FieldByName('total_units').AsInteger]));
    WriteLn(Format('   Products with stock:   %d',
      [DS.FieldByName('product_count').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 5: Soft Delete & Restore ===
{ Soft-deletes a product by marking it inactive, verifies the active count
  decreases, then restores it and confirms the count is back to normal. }
procedure DemoSoftDelete;
begin
  WriteLn('=== 5. Soft Delete & Restore ===');
  WriteLn;

  WriteLn(Format('   Active products before: %d', [RepoCount('products', SpecActive)]));

  // Soft delete
  RepoSoftDelete('products', 2); // Wireless Mouse
  WriteLn('   Soft-deleted "Wireless Mouse" (id=2)');
  WriteLn(Format('   Active products after:  %d', [RepoCount('products', SpecActive)]));

  // Restore
  Conn.ExecuteNonQuery('UPDATE products SET is_active = 1 WHERE id = 2');
  WriteLn('   Restored "Wireless Mouse" (id=2)');
  WriteLn(Format('   Active products now:    %d', [RepoCount('products', SpecActive)]));
  WriteLn;
end;

// === Feature 6: Batch Operations ===
{ Inserts 5 products in a transaction, applies a 10% price increase to all
  Electronics via batch update, then batch-deletes the inserted items. }
procedure DemoBatchOperations;
var
  I: Integer;
begin
  WriteLn('=== 6. Batch Operations ===');
  WriteLn;

  // Batch insert
  WriteLn('   Batch inserting 5 products...');
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');
  for I := 1 to 5 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO products (name, category_id, price, stock, created_at) ' +
      'VALUES (''Batch Item %d'', %d, %.2f, %d, ''2024-03-01'')',
      [I, (I mod 3) + 1, 10.0 + I * 5.0, I * 10]));
  Conn.ExecuteNonQuery('COMMIT');
  WriteLn(Format('   Total products after batch: %d', [RepoCount('products', SpecAll)]));

  // Batch update with specification
  WriteLn('   Batch update: 10%% price increase for Electronics...');
  Conn.ExecuteNonQuery('UPDATE products SET price = price * 1.10 WHERE category_id = 1');
  DS := Conn.ExecuteQuery('SELECT name, price FROM products WHERE category_id = 1 ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %-20s $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Batch delete
  WriteLn;
  WriteLn('   Batch delete: removing batch items...');
  Conn.ExecuteNonQuery('DELETE FROM products WHERE name LIKE ''Batch Item%''');
  WriteLn(Format('   Total products after cleanup: %d', [RepoCount('products', SpecAll)]));
  WriteLn;
end;

// === Feature 7: Existence and Find ===
{ Checks whether specific product IDs exist, then finds the cheapest active
  product and the most expensive in-stock product using specifications. }
procedure DemoExistenceFind;
begin
  WriteLn('=== 7. Existence Checks & Find ===');
  WriteLn;

  WriteLn(Format('   Product id=1 exists: %s', [BoolToStr(RepoExists('products', 1), 'true', 'false')]));
  WriteLn(Format('   Product id=99 exists: %s', [BoolToStr(RepoExists('products', 99), 'true', 'false')]));

  // FindFirst by specification
  WriteLn;
  WriteLn('   FindFirst: cheapest active product:');
  DS := Conn.ExecuteQuery(
    'SELECT name, price FROM products WHERE ' + SpecActive.WhereClause +
    ' ORDER BY price ASC LIMIT 1');
  try
    if not DS.EOF then
      WriteLn(Format('   -> %s at $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat]));
  finally
    DS.Free;
  end;

  WriteLn('   FindFirst: most expensive in-stock:');
  DS := Conn.ExecuteQuery(
    'SELECT name, price FROM products WHERE ' +
    SpecAnd(SpecInStock, SpecActive).WhereClause +
    ' ORDER BY price DESC LIMIT 1');
  try
    if not DS.EOF then
      WriteLn(Format('   -> %s at $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 8: Statistics ===
{ Demonstrates repository statistics and data counts. }
procedure DemoStatistics;
begin
  WriteLn('=== 8. Repository Statistics ===');
  WriteLn;

  WriteLn(Format('   Categories:       %d', [RepoCount('categories', SpecAll)]));
  WriteLn(Format('   Total products:   %d', [RepoCount('products', SpecAll)]));
  WriteLn(Format('   Active products:  %d', [RepoCount('products', SpecActive)]));
  WriteLn(Format('   In stock:         %d', [RepoCount('products', SpecInStock)]));
  WriteLn(Format('   Out of stock:     %d', [RepoCount('products', SpecNot(SpecInStock))]));

  WriteLn;
  WriteLn('   Specification SQL examples:');
  WriteLn('   SpecActive:           ' + SpecActive.WhereClause);
  WriteLn('   SpecByCategory(1):    ' + SpecByCategory(1).WhereClause);
  WriteLn('   SpecPriceRange(20,50):' + SpecPriceRange(20, 50).WhereClause);
  WriteLn('   Active AND InStock:   ' + SpecAnd(SpecActive, SpecInStock).WhereClause);
  WriteLn('   Cat1 OR Cat2:         ' + SpecOr(SpecByCategory(1), SpecByCategory(2)).WhereClause);
  WriteLn;
end;

// === Main Program ===
begin
  WriteLn('=== Example 108: Repository Pattern ===');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    SeedData;

    DemoCRUD;
    DemoSpecifications;
    DemoQueryBuilder;
    DemoAggregates;
    DemoSoftDelete;
    DemoBatchOperations;
    DemoExistenceFind;
    DemoStatistics;

    WriteLn('=== All repository pattern features demonstrated successfully ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
