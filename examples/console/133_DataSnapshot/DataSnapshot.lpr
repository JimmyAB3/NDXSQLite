{===============================================================================
  NDXSQLite Example 133 - Data Snapshot
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Point-in-time database snapshots
  - Snapshot comparison and diff
  - Data restore from snapshots
  - Snapshot metadata and versioning
  - Change tracking between snapshots

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataSnapshot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  SnapshotCounter: Integer;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Main data table: product inventory
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL DEFAULT 0,' +
    '  status TEXT NOT NULL DEFAULT ''active''' +
    ')'
  );

  // Snapshot metadata
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS snapshots (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  snapshot_name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  record_count INTEGER NOT NULL DEFAULT 0,' +
    '  UNIQUE(snapshot_name)' +
    ')'
  );

  // Shadow table: stores point-in-time copies of products
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products_shadow (' +
    '  shadow_id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  snapshot_id INTEGER NOT NULL,' +
    '  product_id INTEGER NOT NULL,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL,' +
    '  status TEXT NOT NULL,' +
    '  FOREIGN KEY(snapshot_id) REFERENCES snapshots(id)' +
    ')'
  );

  // Change log: tracks modifications between snapshots
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS change_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  product_id INTEGER,' +
    '  change_type TEXT NOT NULL,' +  // INSERT, UPDATE, DELETE
    '  field_name TEXT,' +
    '  old_value TEXT,' +
    '  new_value TEXT,' +
    '  changed_at TEXT NOT NULL' +
    ')'
  );

  SnapshotCounter := 0;
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Laptop Pro 15'', ''electronics'', 1299.99, 45, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Wireless Mouse'', ''electronics'', 29.99, 200, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''USB-C Cable'', ''accessories'', 12.99, 500, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Mechanical Keyboard'', ''electronics'', 149.99, 75, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Monitor Stand'', ''accessories'', 49.99, 120, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Webcam HD'', ''electronics'', 79.99, 60, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Desk Lamp'', ''furniture'', 39.99, 85, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Ergonomic Chair'', ''furniture'', 599.99, 20, ''active'')');
end;

{ Captures current product table state into the shadow table with metadata (name, description, timestamp, record count). }
function CreateSnapshot(AName, ADescription: string): Integer;
var
  DS: TDataSet;
  SnapId, RecCount: Integer;
begin
  // Count current records
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try
    RecCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Insert snapshot metadata
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO snapshots (snapshot_name, description, created_at, record_count) ' +
    'VALUES (''%s'', ''%s'', ''2025-01-%s 10:%.2d:00'', %d)',
    [AName, ADescription, Format('%.2d', [15 + SnapshotCounter]), SnapshotCounter * 15, RecCount]));

  // Get snapshot ID
  DS := Conn.ExecuteQuery('SELECT last_insert_rowid() as id');
  try
    SnapId := DS.FieldByName('id').AsInteger;
  finally
    DS.Free;
  end;

  // Copy current products into shadow table
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO products_shadow (snapshot_id, product_id, name, category, price, stock, status) ' +
    'SELECT %d, id, name, category, price, stock, status FROM products',
    [SnapId]));

  Inc(SnapshotCounter);
  Result := SnapId;
end;

// === Demo 1: Initial Data ===
{ Displays the initial product inventory with ID, name, category, price, stock, and status. }
procedure Demo1_InitialData;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Initial Product Inventory ===');
  WriteLn;
  WriteLn(Format('   %-4s %-22s %-14s %8s %6s %-8s', ['ID', 'Name', 'Category', 'Price', 'Stock', 'Status']));
  WriteLn('   ' + StringOfChar('-', 72));

  DS := Conn.ExecuteQuery('SELECT * FROM products ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-4d %-22s %-14s %8.2f %6d %-8s', [
        DS.FieldByName('id').AsInteger,
        DS.FieldByName('name').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('price').AsFloat,
        DS.FieldByName('stock').AsInteger,
        DS.FieldByName('status').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   8 products in initial inventory');
  WriteLn;
end;

// === Demo 2: First Snapshot ===
{ Creates the baseline snapshot capturing all 8 initial products for point-in-time recovery. }
procedure Demo2_FirstSnapshot;
var
  SnapId: Integer;
begin
  WriteLn('=== 2. Create First Snapshot (baseline) ===');
  WriteLn;

  SnapId := CreateSnapshot('snap_baseline', 'Initial inventory baseline');

  WriteLn(Format('   Snapshot created: ID=%d, Name=snap_baseline', [SnapId]));
  WriteLn('   Description: Initial inventory baseline');
  WriteLn('   Records captured: 8');
  WriteLn;
end;

// === Demo 3: Modify Data (Round 1) ===
{ Applies round 1 changes: price update, stock reduction, status change to discontinued, and new product insert. }
procedure Demo3_ModifyRound1;
begin
  WriteLn('=== 3. Data Modifications (Round 1) ===');
  WriteLn;

  // Price increase for Laptop
  Conn.ExecuteNonQuery('UPDATE products SET price = 1399.99 WHERE id = 1');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (1, ''UPDATE'', ''price'', ''1299.99'', ''1399.99'', ''2025-01-15 14:00:00'')');
  WriteLn('   [UPDATE] Laptop Pro 15: price 1299.99 -> 1399.99');

  // Stock reduction for Mouse
  Conn.ExecuteNonQuery('UPDATE products SET stock = 180 WHERE id = 2');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (2, ''UPDATE'', ''stock'', ''200'', ''180'', ''2025-01-15 14:05:00'')');
  WriteLn('   [UPDATE] Wireless Mouse: stock 200 -> 180');

  // Discontinue Desk Lamp
  Conn.ExecuteNonQuery('UPDATE products SET status = ''discontinued'' WHERE id = 7');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (7, ''UPDATE'', ''status'', ''active'', ''discontinued'', ''2025-01-15 15:00:00'')');
  WriteLn('   [UPDATE] Desk Lamp: status active -> discontinued');

  // Add new product
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Bluetooth Speaker'', ''electronics'', 59.99, 100, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (9, ''INSERT'', NULL, NULL, ''Bluetooth Speaker'', ''2025-01-15 16:00:00'')');
  WriteLn('   [INSERT] Bluetooth Speaker (electronics, $59.99, stock=100)');

  WriteLn;
  WriteLn('   4 changes applied');
  WriteLn;
end;

// === Demo 4: Second Snapshot ===
{ Creates the second snapshot capturing state after round 1 modifications (9 products). }
procedure Demo4_SecondSnapshot;
var
  SnapId: Integer;
begin
  WriteLn('=== 4. Create Second Snapshot (after round 1) ===');
  WriteLn;

  SnapId := CreateSnapshot('snap_round1', 'After price updates and new product');

  WriteLn(Format('   Snapshot created: ID=%d, Name=snap_round1', [SnapId]));
  WriteLn('   Description: After price updates and new product');
  WriteLn('   Records captured: 9');
  WriteLn;
end;

// === Demo 5: Modify Data (Round 2) ===
{ Applies round 2 changes: product deletion, price drop, restock, new product, and category change. }
procedure Demo5_ModifyRound2;
begin
  WriteLn('=== 5. Data Modifications (Round 2) ===');
  WriteLn;

  // Delete discontinued product
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (7, ''DELETE'', NULL, ''Desk Lamp'', NULL, ''2025-01-16 09:00:00'')');
  Conn.ExecuteNonQuery('DELETE FROM products WHERE id = 7');
  WriteLn('   [DELETE] Desk Lamp (was discontinued)');

  // Price drop for Keyboard
  Conn.ExecuteNonQuery('UPDATE products SET price = 129.99 WHERE id = 4');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (4, ''UPDATE'', ''price'', ''149.99'', ''129.99'', ''2025-01-16 10:00:00'')');
  WriteLn('   [UPDATE] Mechanical Keyboard: price 149.99 -> 129.99');

  // Restock USB cables
  Conn.ExecuteNonQuery('UPDATE products SET stock = 750 WHERE id = 3');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (3, ''UPDATE'', ''stock'', ''500'', ''750'', ''2025-01-16 11:00:00'')');
  WriteLn('   [UPDATE] USB-C Cable: stock 500 -> 750');

  // Add another product
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, price, stock, status) VALUES (''Phone Stand'', ''accessories'', 19.99, 200, ''active'')');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (10, ''INSERT'', NULL, NULL, ''Phone Stand'', ''2025-01-16 12:00:00'')');
  WriteLn('   [INSERT] Phone Stand (accessories, $19.99, stock=200)');

  // Category change for Monitor Stand
  Conn.ExecuteNonQuery('UPDATE products SET category = ''ergonomics'' WHERE id = 5');
  Conn.ExecuteNonQuery('INSERT INTO change_log (product_id, change_type, field_name, old_value, new_value, changed_at) ' +
    'VALUES (5, ''UPDATE'', ''category'', ''accessories'', ''ergonomics'', ''2025-01-16 13:00:00'')');
  WriteLn('   [UPDATE] Monitor Stand: category accessories -> ergonomics');

  WriteLn;
  WriteLn('   5 changes applied');
  WriteLn;
end;

// === Demo 6: Third Snapshot ===
{ Creates the third snapshot capturing state after round 2 modifications (9 products). }
procedure Demo6_ThirdSnapshot;
var
  SnapId: Integer;
begin
  WriteLn('=== 6. Create Third Snapshot (after round 2) ===');
  WriteLn;

  SnapId := CreateSnapshot('snap_round2', 'After deletions and restocking');

  WriteLn(Format('   Snapshot created: ID=%d, Name=snap_round2', [SnapId]));
  WriteLn('   Description: After deletions and restocking');
  WriteLn('   Records captured: 9');
  WriteLn;
end;

// === Demo 7: List All Snapshots ===
{ Lists all snapshots with ID, name, description, creation timestamp, and row count. }
procedure Demo7_ListSnapshots;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Snapshot Timeline ===');
  WriteLn;
  WriteLn(Format('   %-4s %-16s %-42s %-20s %5s', ['ID', 'Name', 'Description', 'Created', 'Rows']));
  WriteLn('   ' + StringOfChar('-', 92));

  DS := Conn.ExecuteQuery('SELECT * FROM snapshots ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-4d %-16s %-42s %-20s %5d', [
        DS.FieldByName('id').AsInteger,
        DS.FieldByName('snapshot_name').AsString,
        DS.FieldByName('description').AsString,
        DS.FieldByName('created_at').AsString,
        DS.FieldByName('record_count').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// === Demo 8: Diff Between Snapshots ===
{ Compares baseline and round2 snapshots showing deleted records, added records, and field-level modifications. }
procedure Demo8_SnapshotDiff;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Diff Between Snapshots (baseline vs round2) ===');
  WriteLn;

  // Records in baseline but not in round2 (deleted)
  WriteLn('   DELETED records (in baseline, not in round2):');
  DS := Conn.ExecuteQuery(
    'SELECT s1.product_id, s1.name, s1.category, s1.price ' +
    'FROM products_shadow s1 ' +
    'WHERE s1.snapshot_id = 1 ' +
    'AND s1.product_id NOT IN (SELECT s2.product_id FROM products_shadow s2 WHERE s2.snapshot_id = 3)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - [%d] %s (%s, $%.2f)', [
        DS.FieldByName('product_id').AsInteger,
        DS.FieldByName('name').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('price').AsFloat
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Records in round2 but not in baseline (added)
  WriteLn('   ADDED records (in round2, not in baseline):');
  DS := Conn.ExecuteQuery(
    'SELECT s2.product_id, s2.name, s2.category, s2.price ' +
    'FROM products_shadow s2 ' +
    'WHERE s2.snapshot_id = 3 ' +
    'AND s2.product_id NOT IN (SELECT s1.product_id FROM products_shadow s1 WHERE s1.snapshot_id = 1)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   + [%d] %s (%s, $%.2f)', [
        DS.FieldByName('product_id').AsInteger,
        DS.FieldByName('name').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('price').AsFloat
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Records that changed values
  WriteLn('   MODIFIED records (value differences):');
  WriteLn(Format('   %-22s %-12s %-16s %-16s', ['Product', 'Field', 'Baseline', 'Round2']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT s1.product_id, s1.name, ' +
    '  s1.price as old_price, s2.price as new_price, ' +
    '  s1.stock as old_stock, s2.stock as new_stock, ' +
    '  s1.category as old_cat, s2.category as new_cat, ' +
    '  s1.status as old_status, s2.status as new_status ' +
    'FROM products_shadow s1 ' +
    'JOIN products_shadow s2 ON s2.snapshot_id = 3 AND s2.product_id = s1.product_id ' +
    'WHERE s1.snapshot_id = 1 ' +
    'AND (s1.price <> s2.price OR s1.stock <> s2.stock OR s1.category <> s2.category OR s1.status <> s2.status) ' +
    'ORDER BY s1.product_id');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('old_price').AsFloat <> DS.FieldByName('new_price').AsFloat then
        WriteLn(Format('   %-22s %-12s %-16s %-16s', [
          DS.FieldByName('name').AsString, 'price',
          Format('$%.2f', [DS.FieldByName('old_price').AsFloat]),
          Format('$%.2f', [DS.FieldByName('new_price').AsFloat])]));
      if DS.FieldByName('old_stock').AsInteger <> DS.FieldByName('new_stock').AsInteger then
        WriteLn(Format('   %-22s %-12s %-16s %-16s', [
          DS.FieldByName('name').AsString, 'stock',
          DS.FieldByName('old_stock').AsString,
          DS.FieldByName('new_stock').AsString]));
      if DS.FieldByName('old_cat').AsString <> DS.FieldByName('new_cat').AsString then
        WriteLn(Format('   %-22s %-12s %-16s %-16s', [
          DS.FieldByName('name').AsString, 'category',
          DS.FieldByName('old_cat').AsString,
          DS.FieldByName('new_cat').AsString]));
      if DS.FieldByName('old_status').AsString <> DS.FieldByName('new_status').AsString then
        WriteLn(Format('   %-22s %-12s %-16s %-16s', [
          DS.FieldByName('name').AsString, 'status',
          DS.FieldByName('old_status').AsString,
          DS.FieldByName('new_status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// === Demo 9: Restore From Snapshot ===
{ Restores the product table to baseline snapshot state by replacing all current data with shadow table contents. }
procedure Demo9_RestoreSnapshot;
var
  DS: TDataSet;
  CountBefore, CountAfter: Integer;
begin
  WriteLn('=== 9. Restore From Snapshot (baseline) ===');
  WriteLn;

  // Show current state
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try
    CountBefore := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;
  WriteLn(Format('   Current state: %d products', [CountBefore]));

  // Restore from baseline snapshot (id=1)
  WriteLn('   Restoring from snapshot: snap_baseline...');
  WriteLn;

  Conn.ExecuteNonQuery('DELETE FROM products');
  Conn.ExecuteNonQuery(
    'INSERT INTO products (id, name, category, price, stock, status) ' +
    'SELECT product_id, name, category, price, stock, status ' +
    'FROM products_shadow WHERE snapshot_id = 1');

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try
    CountAfter := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Restored state: %d products', [CountAfter]));
  WriteLn;

  // Show restored data
  WriteLn(Format('   %-4s %-22s %-14s %8s %6s %-8s', ['ID', 'Name', 'Category', 'Price', 'Stock', 'Status']));
  WriteLn('   ' + StringOfChar('-', 72));

  DS := Conn.ExecuteQuery('SELECT * FROM products ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-4d %-22s %-14s %8.2f %6d %-8s', [
        DS.FieldByName('id').AsInteger,
        DS.FieldByName('name').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('price').AsFloat,
        DS.FieldByName('stock').AsInteger,
        DS.FieldByName('status').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   All data restored to baseline state (original prices, stock, status)');
  WriteLn;

  // Restore back to round2 for further demos
  Conn.ExecuteNonQuery('DELETE FROM products');
  Conn.ExecuteNonQuery(
    'INSERT INTO products (id, name, category, price, stock, status) ' +
    'SELECT product_id, name, category, price, stock, status ' +
    'FROM products_shadow WHERE snapshot_id = 3');
end;

// === Demo 10: History Navigation ===
{ Shows price timeline for each product across snapshots and summarizes change log by operation type. }
procedure Demo10_HistoryNavigation;
var
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('=== 10. History Navigation (product price timeline) ===');
  WriteLn;
  WriteLn('   Tracking price changes for each product across snapshots:');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT p.product_id, p.name, ' +
    '  GROUP_CONCAT(s.snapshot_name || '':$'' || printf(''%.2f'', p.price), '' | '') as price_history ' +
    'FROM products_shadow p ' +
    'JOIN snapshots s ON s.id = p.snapshot_id ' +
    'GROUP BY p.product_id, p.name ' +
    'ORDER BY p.product_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%d] %s', [
        DS.FieldByName('product_id').AsInteger,
        DS.FieldByName('name').AsString]));
      WriteLn(Format('       %s', [DS.FieldByName('price_history').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Show change log summary
  WriteLn('   Change log summary:');
  WriteLn(Format('   %-10s %5s', ['Type', 'Count']));
  WriteLn('   ' + StringOfChar('-', 18));

  DS := Conn.ExecuteQuery(
    'SELECT change_type, COUNT(*) as cnt FROM change_log GROUP BY change_type ORDER BY change_type');
  try
    I := 0;
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %5d', [
        DS.FieldByName('change_type').AsString,
        DS.FieldByName('cnt').AsInteger]));
      I := I + DS.FieldByName('cnt').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('   ' + StringOfChar('-', 18));
  WriteLn(Format('   %-10s %5d', ['Total', I]));
  WriteLn;
end;

// === Demo 11: Snapshot Size Analysis ===
{ Reports storage metrics per snapshot: row count, field count, estimated bytes, and total shadow records. }
procedure Demo11_SizeAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 11. Snapshot Storage Analysis ===');
  WriteLn;
  WriteLn(Format('   %-16s %6s %8s %10s', ['Snapshot', 'Rows', 'Fields', 'Est. Bytes']));
  WriteLn('   ' + StringOfChar('-', 46));

  DS := Conn.ExecuteQuery(
    'SELECT s.snapshot_name, s.record_count, ' +
    '  s.record_count * 6 as field_count, ' +
    '  s.record_count * 120 as est_bytes ' +
    'FROM snapshots s ORDER BY s.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %6d %8d %10d', [
        DS.FieldByName('snapshot_name').AsString,
        DS.FieldByName('record_count').AsInteger,
        DS.FieldByName('field_count').AsInteger,
        DS.FieldByName('est_bytes').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Total shadow records
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products_shadow');
  try
    WriteLn(Format('   Total shadow records: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM snapshots');
  try
    WriteLn(Format('   Total snapshots: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

// === Demo 12: Snapshot Comparison Matrix ===
{ Calculates pairwise snapshot differences (added, removed, modified counts) for consecutive and full-span comparisons. }
procedure Demo12_ComparisonMatrix;
var
  DS: TDataSet;
begin
  WriteLn('=== 12. Snapshot Comparison Matrix ===');
  WriteLn;
  WriteLn('   Pairwise differences between consecutive snapshots:');
  WriteLn;

  // Baseline vs Round1
  WriteLn('   snap_baseline -> snap_round1:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM products_shadow WHERE snapshot_id = 2 AND product_id NOT IN (SELECT product_id FROM products_shadow WHERE snapshot_id = 1)) as added, ' +
    '  (SELECT COUNT(*) FROM products_shadow WHERE snapshot_id = 1 AND product_id NOT IN (SELECT product_id FROM products_shadow WHERE snapshot_id = 2)) as removed, ' +
    '  (SELECT COUNT(*) FROM products_shadow s1 JOIN products_shadow s2 ON s1.product_id = s2.product_id ' +
    '   WHERE s1.snapshot_id = 1 AND s2.snapshot_id = 2 ' +
    '   AND (s1.price <> s2.price OR s1.stock <> s2.stock OR s1.category <> s2.category OR s1.status <> s2.status)) as modified');
  try
    WriteLn(Format('     Added: %d, Removed: %d, Modified: %d', [
      DS.FieldByName('added').AsInteger,
      DS.FieldByName('removed').AsInteger,
      DS.FieldByName('modified').AsInteger]));
  finally
    DS.Free;
  end;

  // Round1 vs Round2
  WriteLn('   snap_round1 -> snap_round2:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM products_shadow WHERE snapshot_id = 3 AND product_id NOT IN (SELECT product_id FROM products_shadow WHERE snapshot_id = 2)) as added, ' +
    '  (SELECT COUNT(*) FROM products_shadow WHERE snapshot_id = 2 AND product_id NOT IN (SELECT product_id FROM products_shadow WHERE snapshot_id = 3)) as removed, ' +
    '  (SELECT COUNT(*) FROM products_shadow s1 JOIN products_shadow s2 ON s1.product_id = s2.product_id ' +
    '   WHERE s1.snapshot_id = 2 AND s2.snapshot_id = 3 ' +
    '   AND (s1.price <> s2.price OR s1.stock <> s2.stock OR s1.category <> s2.category OR s1.status <> s2.status)) as modified');
  try
    WriteLn(Format('     Added: %d, Removed: %d, Modified: %d', [
      DS.FieldByName('added').AsInteger,
      DS.FieldByName('removed').AsInteger,
      DS.FieldByName('modified').AsInteger]));
  finally
    DS.Free;
  end;

  // Baseline vs Round2 (full span)
  WriteLn('   snap_baseline -> snap_round2 (full span):');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM products_shadow WHERE snapshot_id = 3 AND product_id NOT IN (SELECT product_id FROM products_shadow WHERE snapshot_id = 1)) as added, ' +
    '  (SELECT COUNT(*) FROM products_shadow WHERE snapshot_id = 1 AND product_id NOT IN (SELECT product_id FROM products_shadow WHERE snapshot_id = 3)) as removed, ' +
    '  (SELECT COUNT(*) FROM products_shadow s1 JOIN products_shadow s2 ON s1.product_id = s2.product_id ' +
    '   WHERE s1.snapshot_id = 1 AND s2.snapshot_id = 3 ' +
    '   AND (s1.price <> s2.price OR s1.stock <> s2.stock OR s1.category <> s2.category OR s1.status <> s2.status)) as modified');
  try
    WriteLn(Format('     Added: %d, Removed: %d, Modified: %d', [
      DS.FieldByName('added').AsInteger,
      DS.FieldByName('removed').AsInteger,
      DS.FieldByName('modified').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

// === Main ===
begin
  WriteLn('Example 133: Data Snapshot - Point-in-Time Snapshots, Diff, Restore');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    SeedData;

    Demo1_InitialData;
    Demo2_FirstSnapshot;
    Demo3_ModifyRound1;
    Demo4_SecondSnapshot;
    Demo5_ModifyRound2;
    Demo6_ThirdSnapshot;
    Demo7_ListSnapshots;
    Demo8_SnapshotDiff;
    Demo9_RestoreSnapshot;
    Demo10_HistoryNavigation;
    Demo11_SizeAnalysis;
    Demo12_ComparisonMatrix;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
