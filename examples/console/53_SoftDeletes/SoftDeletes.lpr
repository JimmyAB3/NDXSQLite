{===============================================================================
  NDXSQLite Example 53 - Soft Deletes
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Logical deletion (soft delete) instead of physical deletion
  - Restore functionality (undelete)
  - Trash management with auto-purge
  - Cascade soft-delete for related records
  - Querying active vs deleted records

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SoftDeletes;

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
  // Categories with soft delete support
  Connection.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  deleted_at TEXT DEFAULT NULL,' +  // NULL = active, timestamp = soft deleted
    '  deleted_by TEXT DEFAULT NULL' +
    ')');

  // Products with soft delete and cascade support
  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  category_id INTEGER,' +
    '  name TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  deleted_at TEXT DEFAULT NULL,' +
    '  deleted_by TEXT DEFAULT NULL,' +
    '  FOREIGN KEY (category_id) REFERENCES categories(id)' +
    ')');

  // Orders with soft delete
  Connection.ExecuteNonQuery(
    'CREATE TABLE orders (' +
    '  id INTEGER PRIMARY KEY,' +
    '  customer_name TEXT NOT NULL,' +
    '  total REAL NOT NULL,' +
    '  created_at TEXT DEFAULT CURRENT_TIMESTAMP,' +
    '  deleted_at TEXT DEFAULT NULL,' +
    '  deleted_by TEXT DEFAULT NULL' +
    ')');

  // Order items with cascade soft delete
  Connection.ExecuteNonQuery(
    'CREATE TABLE order_items (' +
    '  id INTEGER PRIMARY KEY,' +
    '  order_id INTEGER,' +
    '  product_name TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  deleted_at TEXT DEFAULT NULL,' +
    '  FOREIGN KEY (order_id) REFERENCES orders(id)' +
    ')');

  // Create views for active records only
  Connection.ExecuteNonQuery(
    'CREATE VIEW active_categories AS ' +
    'SELECT * FROM categories WHERE deleted_at IS NULL');

  Connection.ExecuteNonQuery(
    'CREATE VIEW active_products AS ' +
    'SELECT * FROM products WHERE deleted_at IS NULL');

  Connection.ExecuteNonQuery(
    'CREATE VIEW active_orders AS ' +
    'SELECT * FROM orders WHERE deleted_at IS NULL');

  // Create view for trash (deleted items)
  Connection.ExecuteNonQuery(
    'CREATE VIEW trash_items AS ' +
    'SELECT ''category'' as type, id, name as description, deleted_at, deleted_by FROM categories WHERE deleted_at IS NOT NULL ' +
    'UNION ALL ' +
    'SELECT ''product'' as type, id, name as description, deleted_at, deleted_by FROM products WHERE deleted_at IS NOT NULL ' +
    'UNION ALL ' +
    'SELECT ''order'' as type, id, customer_name as description, deleted_at, deleted_by FROM orders WHERE deleted_at IS NOT NULL');

  // Insert sample data
  Connection.ExecuteNonQuery('INSERT INTO categories (name) VALUES (''Electronics'')');
  Connection.ExecuteNonQuery('INSERT INTO categories (name) VALUES (''Clothing'')');
  Connection.ExecuteNonQuery('INSERT INTO categories (name) VALUES (''Books'')');

  Connection.ExecuteNonQuery('INSERT INTO products (category_id, name, price) VALUES (1, ''Laptop'', 999.99)');
  Connection.ExecuteNonQuery('INSERT INTO products (category_id, name, price) VALUES (1, ''Phone'', 599.99)');
  Connection.ExecuteNonQuery('INSERT INTO products (category_id, name, price) VALUES (1, ''Tablet'', 399.99)');
  Connection.ExecuteNonQuery('INSERT INTO products (category_id, name, price) VALUES (2, ''T-Shirt'', 29.99)');
  Connection.ExecuteNonQuery('INSERT INTO products (category_id, name, price) VALUES (3, ''Novel'', 14.99)');

  Connection.ExecuteNonQuery('INSERT INTO orders (customer_name, total) VALUES (''Alice'', 1599.98)');
  Connection.ExecuteNonQuery('INSERT INTO order_items (order_id, product_name, quantity, price) VALUES (1, ''Laptop'', 1, 999.99)');
  Connection.ExecuteNonQuery('INSERT INTO order_items (order_id, product_name, quantity, price) VALUES (1, ''Phone'', 1, 599.99)');

  Connection.ExecuteNonQuery('INSERT INTO orders (customer_name, total) VALUES (''Bob'', 44.98)');
  Connection.ExecuteNonQuery('INSERT INTO order_items (order_id, product_name, quantity, price) VALUES (2, ''T-Shirt'', 1, 29.99)');
  Connection.ExecuteNonQuery('INSERT INTO order_items (order_id, product_name, quantity, price) VALUES (2, ''Novel'', 1, 14.99)');
end;

{ Queries the active_categories view and prints each non-deleted category with its id and name. }
procedure ShowActiveCategories;
var
  DS: TDataSet;
begin
  WriteLn('   Active categories:');
  DS := Connection.ExecuteQuery('SELECT * FROM active_categories ORDER BY id');
  try
    if DS.IsEmpty then
      WriteLn('     (none)')
    else
      while not DS.EOF do
      begin
        WriteLn(Format('     [%d] %s',
          [DS.FieldByName('id').AsInteger,
           DS.FieldByName('name').AsString]));
        DS.Next;
      end;
  finally
    DS.Free;
  end;
end;

{ Queries active products joined with active categories and prints each product's name, price, and category. }
procedure ShowActiveProducts;
var
  DS: TDataSet;
begin
  WriteLn('   Active products:');
  DS := Connection.ExecuteQuery(
    'SELECT p.id, p.name, p.price, c.name as category ' +
    'FROM active_products p ' +
    'LEFT JOIN active_categories c ON p.category_id = c.id ' +
    'ORDER BY p.id');
  try
    if DS.IsEmpty then
      WriteLn('     (none)')
    else
      while not DS.EOF do
      begin
        WriteLn(Format('     [%d] %s - $%.2f (Category: %s)',
          [DS.FieldByName('id').AsInteger,
           DS.FieldByName('name').AsString,
           DS.FieldByName('price').AsFloat,
           DS.FieldByName('category').AsString]));
        DS.Next;
      end;
  finally
    DS.Free;
  end;
end;

{ Queries the trash_items view and prints each soft-deleted record's type, id, description, timestamp, and who deleted it. }
procedure ShowTrash;
var
  DS: TDataSet;
begin
  WriteLn('   Trash contents:');
  DS := Connection.ExecuteQuery('SELECT * FROM trash_items ORDER BY deleted_at DESC');
  try
    if DS.IsEmpty then
      WriteLn('     (empty)')
    else
      while not DS.EOF do
      begin
        WriteLn(Format('     [%s #%d] %s - deleted at %s by %s',
          [DS.FieldByName('type').AsString,
           DS.FieldByName('id').AsInteger,
           DS.FieldByName('description').AsString,
           DS.FieldByName('deleted_at').AsString,
           DS.FieldByName('deleted_by').AsString]));
        DS.Next;
      end;
  finally
    DS.Free;
  end;
end;

{ Marks a product as deleted by setting its deleted_at timestamp and deleted_by user without removing the row. }
procedure SoftDeleteProduct(AProductId: Integer; ADeletedBy: string);
begin
  Connection.ExecuteNonQuery(
    'UPDATE products SET deleted_at = datetime(''now''), deleted_by = ? WHERE id = ? AND deleted_at IS NULL',
    [ADeletedBy, AProductId]);
end;

{ Restores a soft-deleted product by clearing its deleted_at and deleted_by fields back to NULL. }
procedure RestoreProduct(AProductId: Integer);
begin
  Connection.ExecuteNonQuery(
    'UPDATE products SET deleted_at = NULL, deleted_by = NULL WHERE id = ?',
    [AProductId]);
end;

{ Soft-deletes a category and optionally cascades the soft-delete to all its products within a transaction. }
procedure SoftDeleteCategory(ACategoryId: Integer; ADeletedBy: string; ACascade: Boolean);
begin
  Connection.BeginTransaction;
  try
    // Soft delete the category
    Connection.ExecuteNonQuery(
      'UPDATE categories SET deleted_at = datetime(''now''), deleted_by = ? WHERE id = ? AND deleted_at IS NULL',
      [ADeletedBy, ACategoryId]);

    // Optionally cascade to products
    if ACascade then
    begin
      Connection.ExecuteNonQuery(
        'UPDATE products SET deleted_at = datetime(''now''), deleted_by = ? ' +
        'WHERE category_id = ? AND deleted_at IS NULL',
        [ADeletedBy + ' (cascade)', ACategoryId]);
    end;

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;
end;

{ Restores a soft-deleted category and optionally restores its cascade-deleted products within a transaction. }
procedure RestoreCategory(ACategoryId: Integer; ARestoreProducts: Boolean);
begin
  Connection.BeginTransaction;
  try
    // Restore the category
    Connection.ExecuteNonQuery(
      'UPDATE categories SET deleted_at = NULL, deleted_by = NULL WHERE id = ?',
      [ACategoryId]);

    // Optionally restore products
    if ARestoreProducts then
    begin
      Connection.ExecuteNonQuery(
        'UPDATE products SET deleted_at = NULL, deleted_by = NULL ' +
        'WHERE category_id = ? AND deleted_by LIKE ''% (cascade)''',
        [ACategoryId]);
    end;

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;
end;

{ Soft-deletes an order and cascades the soft-delete to its order_items within a transaction. }
procedure SoftDeleteOrder(AOrderId: Integer; ADeletedBy: string);
begin
  Connection.BeginTransaction;
  try
    // Soft delete the order
    Connection.ExecuteNonQuery(
      'UPDATE orders SET deleted_at = datetime(''now''), deleted_by = ? WHERE id = ? AND deleted_at IS NULL',
      [ADeletedBy, AOrderId]);

    // Cascade to order items
    Connection.ExecuteNonQuery(
      'UPDATE order_items SET deleted_at = datetime(''now'') WHERE order_id = ? AND deleted_at IS NULL',
      [AOrderId]);

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;
end;

{ Soft-deletes a product, then shows the updated active products list and trash contents. }
procedure DemoBasicSoftDelete;
begin
  WriteLn('1. Basic Soft Delete');
  WriteLn('   ------------------');

  ShowActiveProducts;
  WriteLn;

  WriteLn('   Soft deleting "Tablet"...');
  SoftDeleteProduct(3, 'admin');
  WriteLn;

  ShowActiveProducts;
  WriteLn;

  ShowTrash;
  WriteLn;
end;

{ Demonstrates restoring soft-deleted records. }
procedure DemoRestore;
begin
  WriteLn('2. Restore (Undelete)');
  WriteLn('   -------------------');

  WriteLn('   Restoring "Tablet"...');
  RestoreProduct(3);
  WriteLn;

  ShowActiveProducts;
  WriteLn;

  ShowTrash;
  WriteLn;
end;

{ Soft-deletes the Electronics category with cascade to its products, then displays resulting state. }
procedure DemoCascadeSoftDelete;
begin
  WriteLn('3. Cascade Soft Delete');
  WriteLn('   --------------------');

  ShowActiveCategories;
  WriteLn;
  ShowActiveProducts;
  WriteLn;

  WriteLn('   Soft deleting "Electronics" category with cascade...');
  SoftDeleteCategory(1, 'manager', True);
  WriteLn;

  ShowActiveCategories;
  WriteLn;
  ShowActiveProducts;
  WriteLn;

  ShowTrash;
  WriteLn;
end;

{ Restores the Electronics category along with its cascade-deleted products and displays the result. }
procedure DemoRestoreWithCascade;
begin
  WriteLn('4. Restore Category with Products');
  WriteLn('   --------------------------------');

  WriteLn('   Restoring "Electronics" category and cascade-deleted products...');
  RestoreCategory(1, True);
  WriteLn;

  ShowActiveCategories;
  WriteLn;
  ShowActiveProducts;
  WriteLn;

  ShowTrash;
  WriteLn;
end;

{ Populates the trash with several soft-deleted items, then permanently purges them with time-based DELETE statements. }
procedure DemoTrashManagement;
var
  DS: TDataSet;
  DeletedCount, PurgedCount: Integer;
begin
  WriteLn('5. Trash Management');
  WriteLn('   -----------------');

  // Delete some items to populate trash
  SoftDeleteProduct(4, 'user1');  // T-Shirt
  SoftDeleteProduct(5, 'user2');  // Novel
  SoftDeleteOrder(2, 'user1');    // Bob's order

  ShowTrash;
  WriteLn;

  // Count items in trash
  DS := Connection.ExecuteQuery('SELECT COUNT(*) as cnt FROM trash_items');
  try
    DeletedCount := DS.FieldByName('cnt').AsInteger;
    WriteLn(Format('   Total items in trash: %d', [DeletedCount]));
  finally
    DS.Free;
  end;

  // Simulate auto-purge: permanently delete items older than X days
  WriteLn;
  WriteLn('   Auto-purge simulation (items deleted > 0 seconds ago):');

  // For demo, we'll purge items immediately
  PurgedCount := Connection.ExecuteNonQuery(
    'DELETE FROM order_items WHERE deleted_at IS NOT NULL AND deleted_at <= datetime(''now'')');
  WriteLn(Format('     Purged %d order items', [PurgedCount]));

  PurgedCount := Connection.ExecuteNonQuery(
    'DELETE FROM orders WHERE deleted_at IS NOT NULL AND deleted_at <= datetime(''now'')');
  WriteLn(Format('     Purged %d orders', [PurgedCount]));

  PurgedCount := Connection.ExecuteNonQuery(
    'DELETE FROM products WHERE deleted_at IS NOT NULL AND deleted_at <= datetime(''now'')');
  WriteLn(Format('     Purged %d products', [PurgedCount]));

  WriteLn;
  ShowTrash;
  WriteLn;
end;

{ Shows various query patterns: active-only via views, all-records with status, active vs deleted counts, and recent deletions. }
procedure DemoQueryPatterns;
var
  DS: TDataSet;
begin
  WriteLn('6. Query Patterns for Soft Delete');
  WriteLn('   --------------------------------');

  // Re-add some data for queries
  Connection.ExecuteNonQuery('INSERT INTO products (category_id, name, price) VALUES (2, ''Jeans'', 49.99)');
  Connection.ExecuteNonQuery('INSERT INTO products (category_id, name, price) VALUES (3, ''Magazine'', 9.99)');
  SoftDeleteProduct(6, 'test');

  WriteLn('   Query 1: All active products (using view)');
  DS := Connection.ExecuteQuery('SELECT name FROM active_products');
  try
    Write('     ');
    while not DS.EOF do
    begin
      Write(DS.Fields[0].AsString);
      DS.Next;
      if not DS.EOF then Write(', ');
    end;
    WriteLn;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Query 2: All products including deleted (for admin)');
  DS := Connection.ExecuteQuery(
    'SELECT name, CASE WHEN deleted_at IS NULL THEN ''active'' ELSE ''deleted'' END as status FROM products');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%s)', [DS.Fields[0].AsString, DS.Fields[1].AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Query 3: Count active vs deleted');
  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  SUM(CASE WHEN deleted_at IS NULL THEN 1 ELSE 0 END) as active, ' +
    '  SUM(CASE WHEN deleted_at IS NOT NULL THEN 1 ELSE 0 END) as deleted ' +
    'FROM products');
  try
    WriteLn(Format('     Active: %d, Deleted: %d',
      [DS.FieldByName('active').AsInteger,
       DS.FieldByName('deleted').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Query 4: Recently deleted items (last 24 hours)');
  DS := Connection.ExecuteQuery(
    'SELECT type, description, deleted_by FROM trash_items ' +
    'WHERE deleted_at >= datetime(''now'', ''-1 day'') ORDER BY deleted_at DESC');
  try
    if DS.IsEmpty then
      WriteLn('     (none)')
    else
      while not DS.EOF do
      begin
        WriteLn(Format('     %s: %s (by %s)',
          [DS.FieldByName('type').AsString,
           DS.FieldByName('description').AsString,
           DS.FieldByName('deleted_by').AsString]));
        DS.Next;
      end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Creates a documents table with a BEFORE DELETE trigger that converts DELETE into a soft-delete timestamp update. }
procedure DemoTriggerBasedSoftDelete;
var
  DS: TDataSet;
begin
  WriteLn('7. Trigger-Based Soft Delete');
  WriteLn('   --------------------------');

  // Create a table with trigger-based soft delete
  Connection.ExecuteNonQuery(
    'CREATE TABLE documents (' +
    '  id INTEGER PRIMARY KEY,' +
    '  title TEXT NOT NULL,' +
    '  content TEXT,' +
    '  deleted_at TEXT DEFAULT NULL' +
    ')');

  // Create INSTEAD OF DELETE trigger
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER soft_delete_document ' +
    'BEFORE DELETE ON documents ' +
    'WHEN OLD.deleted_at IS NULL ' +
    'BEGIN ' +
    '  UPDATE documents SET deleted_at = datetime(''now'') WHERE id = OLD.id; ' +
    '  SELECT RAISE(IGNORE); ' +
    'END');

  // Insert test data
  Connection.ExecuteNonQuery('INSERT INTO documents (title, content) VALUES (''Report Q1'', ''Content...'')');
  Connection.ExecuteNonQuery('INSERT INTO documents (title, content) VALUES (''Report Q2'', ''Content...'')');

  WriteLn('   Documents before DELETE:');
  DS := Connection.ExecuteQuery('SELECT id, title, deleted_at FROM documents');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('deleted_at').IsNull then
        WriteLn(Format('     [%d] %s (deleted: no)',
          [DS.FieldByName('id').AsInteger, DS.FieldByName('title').AsString]))
      else
        WriteLn(Format('     [%d] %s (deleted: yes)',
          [DS.FieldByName('id').AsInteger, DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Executing: DELETE FROM documents WHERE id = 1');
  Connection.ExecuteNonQuery('DELETE FROM documents WHERE id = 1');

  WriteLn;
  WriteLn('   Documents after DELETE (soft deleted via trigger):');
  DS := Connection.ExecuteQuery('SELECT id, title, deleted_at FROM documents');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('deleted_at').IsNull then
        WriteLn(Format('     [%d] %s (deleted: no)',
          [DS.FieldByName('id').AsInteger, DS.FieldByName('title').AsString]))
      else
        WriteLn(Format('     [%d] %s (deleted: yes)',
          [DS.FieldByName('id').AsInteger, DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints a summary of soft-delete best practices including column choice, views, cascading, and indexing tips. }
procedure DemoBestPractices;
begin
  WriteLn('8. Soft Delete Best Practices');
  WriteLn('   ---------------------------');
  WriteLn('   - Use deleted_at (timestamp) instead of is_deleted (boolean)');
  WriteLn('   - Track who deleted: deleted_by column');
  WriteLn('   - Create views for active records (cleaner queries)');
  WriteLn('   - Consider cascade behavior for related records');
  WriteLn('   - Implement auto-purge for old deleted records');
  WriteLn('   - Index deleted_at column for performance');
  WriteLn('   - Use triggers for transparent soft delete');
  WriteLn('   - Document soft delete behavior for your team');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 53: Soft Deletes ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example53.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      DemoBasicSoftDelete;
      DemoRestore;
      DemoCascadeSoftDelete;
      DemoRestoreWithCascade;
      DemoTrashManagement;
      DemoQueryPatterns;
      DemoTriggerBasedSoftDelete;
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
