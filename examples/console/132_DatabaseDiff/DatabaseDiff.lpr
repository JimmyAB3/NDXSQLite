{===============================================================================
  NDXSQLite Example 132 - Database Diff
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Schema comparison between databases
  - Table and column difference detection
  - Type change identification
  - Index difference analysis
  - Migration script generation hints

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DatabaseDiff;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Formats an integer with a sign prefix for display. }
function SignedInt(Value: Integer): string;
begin
  if Value > 0 then
    Result := '+' + IntToStr(Value)
  else
    Result := IntToStr(Value);
end;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Schema registry: tables
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS schema_tables (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  db_name TEXT NOT NULL,' +       // source, target
    '  table_name TEXT NOT NULL,' +
    '  UNIQUE(db_name, table_name)' +
    ')'
  );

  // Schema registry: columns
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS schema_columns (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  db_name TEXT NOT NULL,' +
    '  table_name TEXT NOT NULL,' +
    '  column_name TEXT NOT NULL,' +
    '  data_type TEXT NOT NULL,' +
    '  is_nullable INTEGER DEFAULT 1,' +
    '  default_value TEXT,' +
    '  is_primary_key INTEGER DEFAULT 0,' +
    '  column_order INTEGER,' +
    '  UNIQUE(db_name, table_name, column_name)' +
    ')'
  );

  // Schema registry: indexes
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS schema_indexes (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  db_name TEXT NOT NULL,' +
    '  table_name TEXT NOT NULL,' +
    '  index_name TEXT NOT NULL,' +
    '  columns TEXT NOT NULL,' +
    '  is_unique INTEGER DEFAULT 0,' +
    '  UNIQUE(db_name, index_name)' +
    ')'
  );

  // Diff results
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS diff_results (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  diff_type TEXT NOT NULL,' +      // table, column, index
    '  object_name TEXT NOT NULL,' +
    '  change_type TEXT NOT NULL,' +    // added, removed, modified
    '  details TEXT,' +
    '  migration_sql TEXT' +
    ')'
  );
end;

{ Inserts source database schema definition. }
procedure InsertSourceSchema;
begin
  // Source database: e-commerce v1.0
  // Tables
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''source'', ''users'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''source'', ''products'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''source'', ''orders'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''source'', ''order_items'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''source'', ''categories'')');

  // users columns
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''source'', ''users'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''source'', ''users'', ''username'', ''TEXT'', 0, NULL, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''users'', ''email'', ''TEXT'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''users'', ''password_hash'', ''TEXT'', 0, 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''source'', ''users'', ''created_at'', ''TEXT'', 0, ''CURRENT_TIMESTAMP'', 5)');

  // products columns
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''source'', ''products'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''products'', ''name'', ''TEXT'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''products'', ''description'', ''TEXT'', 1, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''products'', ''price'', ''REAL'', 0, 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''products'', ''category_id'', ''INTEGER'', 1, 5)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''source'', ''products'', ''stock'', ''INTEGER'', 0, ''0'', 6)');

  // orders columns
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''source'', ''orders'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''orders'', ''user_id'', ''INTEGER'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''orders'', ''total_amount'', ''REAL'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''source'', ''orders'', ''status'', ''TEXT'', 0, ''pending'', 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''orders'', ''created_at'', ''TEXT'', 0, 5)');

  // order_items columns
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''source'', ''order_items'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''order_items'', ''order_id'', ''INTEGER'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''order_items'', ''product_id'', ''INTEGER'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''order_items'', ''quantity'', ''INTEGER'', 0, 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''order_items'', ''unit_price'', ''REAL'', 0, 5)');

  // categories columns
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''source'', ''categories'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''categories'', ''name'', ''TEXT'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''source'', ''categories'', ''parent_id'', ''INTEGER'', 1, 3)');

  // Source indexes
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''source'', ''users'', ''idx_users_email'', ''email'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''source'', ''users'', ''idx_users_username'', ''username'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''source'', ''products'', ''idx_products_category'', ''category_id'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''source'', ''orders'', ''idx_orders_user'', ''user_id'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''source'', ''orders'', ''idx_orders_status'', ''status'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''source'', ''order_items'', ''idx_items_order'', ''order_id'', 0)');
end;

{ Inserts target database schema definition. }
procedure InsertTargetSchema;
begin
  // Target database: e-commerce v2.0 (evolved schema)
  // Changes:
  //   - users: added phone, last_login; removed password_hash (moved to auth table)
  //   - products: price changed to DECIMAL, added sku, added weight
  //   - orders: added shipping_address, delivery_date
  //   - order_items: added discount_pct
  //   - categories: removed (replaced by tags)
  //   - NEW: user_auth table
  //   - NEW: tags table
  //   - NEW: product_tags table

  // Tables
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''target'', ''users'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''target'', ''products'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''target'', ''orders'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''target'', ''order_items'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''target'', ''user_auth'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''target'', ''tags'')');
  Conn.ExecuteNonQuery('INSERT INTO schema_tables (db_name, table_name) VALUES (''target'', ''product_tags'')');

  // users columns (modified)
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''target'', ''users'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''users'', ''username'', ''TEXT'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''users'', ''email'', ''VARCHAR(255)'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''users'', ''phone'', ''TEXT'', 1, 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''target'', ''users'', ''created_at'', ''TEXT'', 0, ''CURRENT_TIMESTAMP'', 5)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''users'', ''last_login'', ''TEXT'', 1, 6)');

  // products columns (modified)
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''target'', ''products'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''products'', ''sku'', ''TEXT'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''products'', ''name'', ''TEXT'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''products'', ''description'', ''TEXT'', 0, 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''products'', ''price'', ''DECIMAL(10,2)'', 0, 5)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''target'', ''products'', ''stock'', ''INTEGER'', 0, ''1'', 6)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''products'', ''weight_kg'', ''REAL'', 1, 7)');

  // orders columns (modified)
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''target'', ''orders'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''orders'', ''user_id'', ''INTEGER'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''orders'', ''total_amount'', ''DECIMAL(10,2)'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''target'', ''orders'', ''status'', ''TEXT'', 0, ''received'', 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''orders'', ''shipping_address'', ''TEXT'', 1, 5)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''orders'', ''created_at'', ''TEXT'', 0, 6)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''orders'', ''delivery_date'', ''TEXT'', 1, 7)');

  // order_items columns (modified)
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''target'', ''order_items'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''order_items'', ''order_id'', ''INTEGER'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''order_items'', ''product_id'', ''INTEGER'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''order_items'', ''quantity'', ''INTEGER'', 0, 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''order_items'', ''unit_price'', ''DECIMAL(10,2)'', 0, 5)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, default_value, column_order) VALUES (''target'', ''order_items'', ''discount_pct'', ''REAL'', 1, ''0'', 6)');

  // user_auth (new table)
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''target'', ''user_auth'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''user_auth'', ''user_id'', ''INTEGER'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''user_auth'', ''password_hash'', ''TEXT'', 0, 3)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''user_auth'', ''auth_provider'', ''TEXT'', 1, 4)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''user_auth'', ''mfa_enabled'', ''INTEGER'', 0, 5)');

  // tags (new table, replaces categories)
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, is_primary_key, column_order) VALUES (''target'', ''tags'', ''id'', ''INTEGER'', 0, 1, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''tags'', ''name'', ''TEXT'', 0, 2)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''tags'', ''slug'', ''TEXT'', 0, 3)');

  // product_tags (new junction table)
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''product_tags'', ''product_id'', ''INTEGER'', 0, 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_columns (db_name, table_name, column_name, data_type, is_nullable, column_order) VALUES (''target'', ''product_tags'', ''tag_id'', ''INTEGER'', 0, 2)');

  // Target indexes
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''users'', ''idx_users_email'', ''email'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''users'', ''idx_users_username'', ''username'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''users'', ''idx_users_phone'', ''phone'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''products'', ''idx_products_sku'', ''sku'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''orders'', ''idx_orders_user'', ''user_id'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''orders'', ''idx_orders_status'', ''status'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''orders'', ''idx_orders_delivery'', ''delivery_date'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''order_items'', ''idx_items_order'', ''order_id'', 0)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''user_auth'', ''idx_auth_user'', ''user_id'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''tags'', ''idx_tags_slug'', ''slug'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO schema_indexes (db_name, table_name, index_name, columns, is_unique) VALUES (''target'', ''product_tags'', ''idx_ptags_product'', ''product_id'', 0)');
end;

// ============================================================
// Demo Sections
// ============================================================

{ Displays the source (v1.0) schema with table names, column counts, and index counts per table. }
procedure Demo1_SourceSchema;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Source Schema (v1.0) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT t.table_name, ' +
    '  (SELECT COUNT(*) FROM schema_columns c WHERE c.db_name = ''source'' AND c.table_name = t.table_name) as col_count, ' +
    '  (SELECT COUNT(*) FROM schema_indexes i WHERE i.db_name = ''source'' AND i.table_name = t.table_name) as idx_count ' +
    'FROM schema_tables t WHERE t.db_name = ''source'' ORDER BY t.table_name');
  try
    WriteLn(Format('   %-15s %-8s %-8s', ['Table', 'Columns', 'Indexes']));
    WriteLn('   ' + StringOfChar('-', 35));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %-8d %-8d', [
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('col_count').AsInteger,
        DS.FieldByName('idx_count').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT table_name) as t_cnt, COUNT(*) as c_cnt FROM schema_columns WHERE db_name = ''source''');
  try
    WriteLn(Format('   Source: %d tables, %d columns total', [
      DS.FieldByName('t_cnt').AsInteger,
      DS.FieldByName('c_cnt').AsInteger
    ]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays the target (v2.0) schema with table names, column counts, and index counts per table. }
procedure Demo2_TargetSchema;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Target Schema (v2.0) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT t.table_name, ' +
    '  (SELECT COUNT(*) FROM schema_columns c WHERE c.db_name = ''target'' AND c.table_name = t.table_name) as col_count, ' +
    '  (SELECT COUNT(*) FROM schema_indexes i WHERE i.db_name = ''target'' AND i.table_name = t.table_name) as idx_count ' +
    'FROM schema_tables t WHERE t.db_name = ''target'' ORDER BY t.table_name');
  try
    WriteLn(Format('   %-15s %-8s %-8s', ['Table', 'Columns', 'Indexes']));
    WriteLn('   ' + StringOfChar('-', 35));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %-8d %-8d', [
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('col_count').AsInteger,
        DS.FieldByName('idx_count').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT table_name) as t_cnt, COUNT(*) as c_cnt FROM schema_columns WHERE db_name = ''target''');
  try
    WriteLn(Format('   Target: %d tables, %d columns total', [
      DS.FieldByName('t_cnt').AsInteger,
      DS.FieldByName('c_cnt').AsInteger
    ]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Compares tables between source and target, listing added, removed, and common tables with diff records logged. }
procedure Demo3_TableDiff;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Table-Level Diff ===');
  WriteLn;

  // Tables added in target
  WriteLn('   Tables ADDED in target:');
  DS := Conn.ExecuteQuery(
    'SELECT table_name FROM schema_tables WHERE db_name = ''target'' ' +
    'AND table_name NOT IN (SELECT table_name FROM schema_tables WHERE db_name = ''source'') ORDER BY table_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   + %s', [DS.FieldByName('table_name').AsString]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO diff_results (diff_type, object_name, change_type, details, migration_sql) VALUES (''table'', ''%s'', ''added'', ''New table in v2.0'', ''CREATE TABLE %s (...)'')',
        [DS.FieldByName('table_name').AsString, DS.FieldByName('table_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Tables REMOVED from source:');
  DS := Conn.ExecuteQuery(
    'SELECT table_name FROM schema_tables WHERE db_name = ''source'' ' +
    'AND table_name NOT IN (SELECT table_name FROM schema_tables WHERE db_name = ''target'') ORDER BY table_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s', [DS.FieldByName('table_name').AsString]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO diff_results (diff_type, object_name, change_type, details, migration_sql) VALUES (''table'', ''%s'', ''removed'', ''Table dropped in v2.0'', ''DROP TABLE %s'')',
        [DS.FieldByName('table_name').AsString, DS.FieldByName('table_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Tables COMMON (may have column changes):');
  DS := Conn.ExecuteQuery(
    'SELECT table_name FROM schema_tables WHERE db_name = ''source'' ' +
    'AND table_name IN (SELECT table_name FROM schema_tables WHERE db_name = ''target'') ORDER BY table_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   = %s', [DS.FieldByName('table_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Identifies columns added and removed in common tables, recording diffs with suggested ALTER TABLE statements. }
procedure Demo4_ColumnDiff;
var
  DS: TDataSet;
  TableName: string;
begin
  WriteLn('=== 4. Column-Level Diff ===');
  WriteLn;

  // Columns added
  WriteLn('   Columns ADDED:');
  DS := Conn.ExecuteQuery(
    'SELECT tc.table_name, tc.column_name, tc.data_type, tc.is_nullable ' +
    'FROM schema_columns tc ' +
    'WHERE tc.db_name = ''target'' ' +
    'AND tc.table_name IN (SELECT table_name FROM schema_tables WHERE db_name = ''source'') ' +
    'AND NOT EXISTS (SELECT 1 FROM schema_columns sc WHERE sc.db_name = ''source'' AND sc.table_name = tc.table_name AND sc.column_name = tc.column_name) ' +
    'ORDER BY tc.table_name, tc.column_order');
  try
    while not DS.EOF do
    begin
      TableName := DS.FieldByName('table_name').AsString;
      WriteLn(Format('   + %s.%s (%s, nullable=%s)', [
        TableName,
        DS.FieldByName('column_name').AsString,
        DS.FieldByName('data_type').AsString,
        BoolToStr(DS.FieldByName('is_nullable').AsInteger = 1, 'yes', 'no')
      ]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO diff_results (diff_type, object_name, change_type, details, migration_sql) VALUES (''column'', ''%s.%s'', ''added'', ''New column: %s'', ''ALTER TABLE %s ADD COLUMN %s %s'')',
        [TableName, DS.FieldByName('column_name').AsString,
         DS.FieldByName('data_type').AsString,
         TableName, DS.FieldByName('column_name').AsString, DS.FieldByName('data_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Columns REMOVED:');
  DS := Conn.ExecuteQuery(
    'SELECT sc.table_name, sc.column_name, sc.data_type ' +
    'FROM schema_columns sc ' +
    'WHERE sc.db_name = ''source'' ' +
    'AND sc.table_name IN (SELECT table_name FROM schema_tables WHERE db_name = ''target'') ' +
    'AND NOT EXISTS (SELECT 1 FROM schema_columns tc WHERE tc.db_name = ''target'' AND tc.table_name = sc.table_name AND tc.column_name = sc.column_name) ' +
    'ORDER BY sc.table_name, sc.column_order');
  try
    while not DS.EOF do
    begin
      TableName := DS.FieldByName('table_name').AsString;
      WriteLn(Format('   - %s.%s (%s)', [
        TableName,
        DS.FieldByName('column_name').AsString,
        DS.FieldByName('data_type').AsString
      ]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO diff_results (diff_type, object_name, change_type, details, migration_sql) VALUES (''column'', ''%s.%s'', ''removed'', ''Column dropped'', ''ALTER TABLE %s DROP COLUMN %s'')',
        [TableName, DS.FieldByName('column_name').AsString,
         TableName, DS.FieldByName('column_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Detects data type changes for columns present in both schemas, showing source type, target type, and compatibility notes. }
procedure Demo5_TypeChanges;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Data Type Changes ===');
  WriteLn;
  WriteLn(Format('   %-20s %-15s %-15s %s', ['Column', 'Source Type', 'Target Type', 'Compatible?']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT sc.table_name, sc.column_name, sc.data_type as src_type, tc.data_type as tgt_type ' +
    'FROM schema_columns sc ' +
    'JOIN schema_columns tc ON tc.db_name = ''target'' AND tc.table_name = sc.table_name AND tc.column_name = sc.column_name ' +
    'WHERE sc.db_name = ''source'' AND sc.data_type <> tc.data_type ' +
    'ORDER BY sc.table_name, sc.column_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-15s %-15s %s', [
        DS.FieldByName('table_name').AsString + '.' + DS.FieldByName('column_name').AsString,
        DS.FieldByName('src_type').AsString,
        DS.FieldByName('tgt_type').AsString,
        'widening'  // In SQLite all are compatible, but semantically they're widenings
      ]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO diff_results (diff_type, object_name, change_type, details, migration_sql) VALUES (''column'', ''%s.%s'', ''modified'', ''Type: %s -> %s'', ''-- Type change (SQLite flexible)'')',
        [DS.FieldByName('table_name').AsString, DS.FieldByName('column_name').AsString,
         DS.FieldByName('src_type').AsString, DS.FieldByName('tgt_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Note: SQLite uses dynamic typing, type changes are metadata-only');
  WriteLn;
end;

{ Compares indexes between source and target, listing added, removed, and unchanged indexes with CREATE/DROP statements. }
procedure Demo6_IndexDiff;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Index Diff ===');
  WriteLn;

  WriteLn('   Indexes ADDED:');
  DS := Conn.ExecuteQuery(
    'SELECT index_name, table_name, columns, is_unique FROM schema_indexes ' +
    'WHERE db_name = ''target'' AND index_name NOT IN ' +
    '  (SELECT index_name FROM schema_indexes WHERE db_name = ''source'') ' +
    'ORDER BY table_name, index_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   + %-25s on %-15s (%s) %s', [
        DS.FieldByName('index_name').AsString,
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('columns').AsString,
        BoolToStr(DS.FieldByName('is_unique').AsInteger = 1, 'UNIQUE', '')
      ]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO diff_results (diff_type, object_name, change_type, details, migration_sql) VALUES (''index'', ''%s'', ''added'', ''New index on %s(%s)'', ''CREATE %sINDEX %s ON %s(%s)'')',
        [DS.FieldByName('index_name').AsString,
         DS.FieldByName('table_name').AsString,
         DS.FieldByName('columns').AsString,
         BoolToStr(DS.FieldByName('is_unique').AsInteger = 1, 'UNIQUE ', ''),
         DS.FieldByName('index_name').AsString,
         DS.FieldByName('table_name').AsString,
         DS.FieldByName('columns').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Indexes REMOVED:');
  DS := Conn.ExecuteQuery(
    'SELECT index_name, table_name, columns FROM schema_indexes ' +
    'WHERE db_name = ''source'' AND index_name NOT IN ' +
    '  (SELECT index_name FROM schema_indexes WHERE db_name = ''target'') ' +
    'ORDER BY table_name, index_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %-25s on %-15s (%s)', [
        DS.FieldByName('index_name').AsString,
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('columns').AsString
      ]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO diff_results (diff_type, object_name, change_type, details, migration_sql) VALUES (''index'', ''%s'', ''removed'', ''Index dropped'', ''DROP INDEX %s'')',
        [DS.FieldByName('index_name').AsString, DS.FieldByName('index_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Indexes UNCHANGED:');
  DS := Conn.ExecuteQuery(
    'SELECT si.index_name, si.table_name, si.columns FROM schema_indexes si ' +
    'WHERE si.db_name = ''source'' AND EXISTS ' +
    '  (SELECT 1 FROM schema_indexes ti WHERE ti.db_name = ''target'' AND ti.index_name = si.index_name AND ti.columns = si.columns) ' +
    'ORDER BY si.table_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   = %-25s on %-15s (%s)', [
        DS.FieldByName('index_name').AsString,
        DS.FieldByName('table_name').AsString,
        DS.FieldByName('columns').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Generates an ordered migration script from diff results: drops, adds, creates, and index changes with step numbers. }
procedure Demo7_MigrationSQL;
var
  DS: TDataSet;
  StepNum: Integer;
begin
  WriteLn('=== 7. Migration SQL Generation ===');
  WriteLn;
  WriteLn('   Generated migration script (source -> target):');
  WriteLn('   ' + StringOfChar('-', 60));
  WriteLn;

  StepNum := 0;

  // 1. Drop removed tables
  DS := Conn.ExecuteQuery('SELECT object_name, migration_sql FROM diff_results WHERE diff_type = ''table'' AND change_type = ''removed'' ORDER BY object_name');
  try
    while not DS.EOF do
    begin
      Inc(StepNum);
      WriteLn(Format('   -- Step %d: Drop table', [StepNum]));
      WriteLn(Format('   %s;', [DS.FieldByName('migration_sql').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // 2. Drop removed columns
  DS := Conn.ExecuteQuery('SELECT object_name, migration_sql FROM diff_results WHERE diff_type = ''column'' AND change_type = ''removed'' ORDER BY object_name');
  try
    while not DS.EOF do
    begin
      Inc(StepNum);
      WriteLn(Format('   -- Step %d: Drop column %s', [StepNum, DS.FieldByName('object_name').AsString]));
      WriteLn(Format('   %s;', [DS.FieldByName('migration_sql').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // 3. Add new columns
  DS := Conn.ExecuteQuery('SELECT object_name, migration_sql FROM diff_results WHERE diff_type = ''column'' AND change_type = ''added'' ORDER BY object_name');
  try
    while not DS.EOF do
    begin
      Inc(StepNum);
      WriteLn(Format('   -- Step %d: Add column %s', [StepNum, DS.FieldByName('object_name').AsString]));
      WriteLn(Format('   %s;', [DS.FieldByName('migration_sql').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // 4. Create new tables
  DS := Conn.ExecuteQuery('SELECT object_name, migration_sql FROM diff_results WHERE diff_type = ''table'' AND change_type = ''added'' ORDER BY object_name');
  try
    while not DS.EOF do
    begin
      Inc(StepNum);
      WriteLn(Format('   -- Step %d: Create table %s', [StepNum, DS.FieldByName('object_name').AsString]));
      WriteLn(Format('   %s;', [DS.FieldByName('migration_sql').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // 5. Index changes
  DS := Conn.ExecuteQuery('SELECT object_name, migration_sql, change_type FROM diff_results WHERE diff_type = ''index'' ORDER BY change_type DESC, object_name');
  try
    while not DS.EOF do
    begin
      Inc(StepNum);
      WriteLn(Format('   -- Step %d: %s index %s', [StepNum,
        BoolToStr(DS.FieldByName('change_type').AsString = 'added', 'Create', 'Drop'),
        DS.FieldByName('object_name').AsString]));
      WriteLn(Format('   %s;', [DS.FieldByName('migration_sql').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total migration steps: %d', [StepNum]));
  WriteLn;
end;

{ Summarizes architectural changes, data model improvements, type precision updates, and breaking changes between schema versions. }
procedure Demo8_SemanticDiff;
begin
  WriteLn('=== 8. Semantic Diff Summary ===');
  WriteLn;
  WriteLn('   Schema evolution from v1.0 to v2.0:');
  WriteLn;
  WriteLn('   ARCHITECTURAL CHANGES:');
  WriteLn('   - Authentication separated: password_hash moved from users to user_auth');
  WriteLn('   - Categorization refactored: categories table replaced by tags + product_tags');
  WriteLn('   - Multi-factor auth support added (user_auth.mfa_enabled)');
  WriteLn;
  WriteLn('   DATA MODEL IMPROVEMENTS:');
  WriteLn('   - Products: Added SKU for inventory tracking');
  WriteLn('   - Products: Added weight for shipping calculations');
  WriteLn('   - Products: Removed category_id FK (replaced by tag system)');
  WriteLn('   - Orders: Added shipping_address and delivery_date');
  WriteLn('   - Order items: Added discount_pct for promotional pricing');
  WriteLn('   - Users: Added phone and last_login tracking');
  WriteLn;
  WriteLn('   TYPE PRECISION:');
  WriteLn('   - Monetary fields: REAL -> DECIMAL(10,2) (precision guarantee)');
  WriteLn('   - Email field: TEXT -> VARCHAR(255) (length constraint)');
  WriteLn;
  WriteLn('   BREAKING CHANGES:');
  WriteLn('   - categories table dropped (requires data migration to tags)');
  WriteLn('   - products.category_id removed (requires FK update)');
  WriteLn('   - users.password_hash removed (requires auth table population)');
  WriteLn;
end;

{ Compares nullability and default value constraints for common columns, highlighting changes that may require data backfill. }
procedure Demo9_NullabilityChanges;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Nullability and Default Changes ===');
  WriteLn;
  WriteLn('   Comparing nullable and default values for common columns:');
  WriteLn;
  WriteLn(Format('   %-25s %-12s %-12s %-15s %-15s', ['Column', 'Src Null', 'Tgt Null', 'Src Default', 'Tgt Default']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery(
    'SELECT sc.table_name, sc.column_name, ' +
    '  sc.is_nullable as src_null, tc.is_nullable as tgt_null, ' +
    '  COALESCE(sc.default_value, ''(none)'') as src_default, ' +
    '  COALESCE(tc.default_value, ''(none)'') as tgt_default ' +
    'FROM schema_columns sc ' +
    'JOIN schema_columns tc ON tc.db_name = ''target'' AND tc.table_name = sc.table_name AND tc.column_name = sc.column_name ' +
    'WHERE sc.db_name = ''source'' ' +
    'AND (sc.is_nullable <> tc.is_nullable OR COALESCE(sc.default_value, '''') <> COALESCE(tc.default_value, '''')) ' +
    'ORDER BY sc.table_name, sc.column_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-12s %-12s %-15s %-15s', [
        DS.FieldByName('table_name').AsString + '.' + DS.FieldByName('column_name').AsString,
        BoolToStr(DS.FieldByName('src_null').AsInteger = 1, 'nullable', 'NOT NULL'),
        BoolToStr(DS.FieldByName('tgt_null').AsInteger = 1, 'nullable', 'NOT NULL'),
        DS.FieldByName('src_default').AsString,
        DS.FieldByName('tgt_default').AsString
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Note: Nullability changes may require data backfill for NOT NULL additions');
  WriteLn;
end;

{ Demonstrates database diff statistics and schema change metrics. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Diff Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM diff_results');
  try
    WriteLn(Format('   Total differences found: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   By type:');
  DS := Conn.ExecuteQuery('SELECT diff_type, change_type, COUNT(*) as cnt FROM diff_results GROUP BY diff_type, change_type ORDER BY diff_type, change_type');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-10s %d', [
        DS.FieldByName('diff_type').AsString,
        DS.FieldByName('change_type').AsString,
        DS.FieldByName('cnt').AsInteger
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Schema size comparison:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM schema_tables WHERE db_name = ''source'') as src_tables, ' +
    '  (SELECT COUNT(*) FROM schema_tables WHERE db_name = ''target'') as tgt_tables, ' +
    '  (SELECT COUNT(*) FROM schema_columns WHERE db_name = ''source'') as src_cols, ' +
    '  (SELECT COUNT(*) FROM schema_columns WHERE db_name = ''target'') as tgt_cols, ' +
    '  (SELECT COUNT(*) FROM schema_indexes WHERE db_name = ''source'') as src_idx, ' +
    '  (SELECT COUNT(*) FROM schema_indexes WHERE db_name = ''target'') as tgt_idx');
  try
    WriteLn(Format('   %-12s %-8s %-8s %-8s', ['', 'Tables', 'Columns', 'Indexes']));
    WriteLn('   ' + StringOfChar('-', 40));
    WriteLn(Format('   %-12s %-8d %-8d %-8d', ['Source v1.0',
      DS.FieldByName('src_tables').AsInteger,
      DS.FieldByName('src_cols').AsInteger,
      DS.FieldByName('src_idx').AsInteger]));
    WriteLn(Format('   %-12s %-8d %-8d %-8d', ['Target v2.0',
      DS.FieldByName('tgt_tables').AsInteger,
      DS.FieldByName('tgt_cols').AsInteger,
      DS.FieldByName('tgt_idx').AsInteger]));
    WriteLn(Format('   %-12s %-8s %-8s %-8s', ['Delta',
      SignedInt(DS.FieldByName('tgt_tables').AsInteger - DS.FieldByName('src_tables').AsInteger),
      SignedInt(DS.FieldByName('tgt_cols').AsInteger - DS.FieldByName('src_cols').AsInteger),
      SignedInt(DS.FieldByName('tgt_idx').AsInteger - DS.FieldByName('src_idx').AsInteger)]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Migration complexity: MEDIUM (breaking changes require data migration)');
  WriteLn;
end;

// ============================================================
// Main
// ============================================================

begin
  WriteLn('Example 132: Database Diff - Schema Comparison, Migration SQL, Semantic Diff');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSourceSchema;
    InsertTargetSchema;

    Demo1_SourceSchema;
    Demo2_TargetSchema;
    Demo3_TableDiff;
    Demo4_ColumnDiff;
    Demo5_TypeChanges;
    Demo6_IndexDiff;
    Demo7_MigrationSQL;
    Demo8_SemanticDiff;
    Demo9_NullabilityChanges;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
