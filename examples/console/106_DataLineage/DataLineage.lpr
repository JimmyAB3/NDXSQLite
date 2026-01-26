{===============================================================================
  NDXSQLite Example 106 - Data Lineage
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Data source and transformation tracking
  - Column-level lineage mapping
  - Forward and backward lineage traversal
  - Impact analysis for schema changes
  - Data freshness monitoring

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataLineage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Data sources (origin systems)
  Conn.ExecuteNonQuery(
    'CREATE TABLE data_sources (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  source_type TEXT NOT NULL,' +  // csv, api, database, manual
    '  uri TEXT,' +
    '  description TEXT' +
    ')');

  // Datasets (tables/files produced)
  Conn.ExecuteNonQuery(
    'CREATE TABLE datasets (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  source_id TEXT REFERENCES data_sources(id),' +
    '  dataset_type TEXT NOT NULL,' +  // raw, cleaned, derived, aggregated
    '  row_count INTEGER DEFAULT 0,' +
    '  created_at TEXT NOT NULL,' +
    '  updated_at TEXT' +
    ')');

  // Dataset columns
  Conn.ExecuteNonQuery(
    'CREATE TABLE dataset_columns (' +
    '  id TEXT PRIMARY KEY,' +
    '  dataset_id TEXT REFERENCES datasets(id),' +
    '  column_name TEXT NOT NULL,' +
    '  data_type TEXT NOT NULL,' +
    '  description TEXT' +
    ')');

  // Transformations (ETL steps)
  Conn.ExecuteNonQuery(
    'CREATE TABLE transformations (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  transform_type TEXT NOT NULL,' +  // filter, map, join, aggregate, derive, clean
    '  description TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  created_by TEXT' +
    ')');

  // Lineage edges (column-level data flow)
  Conn.ExecuteNonQuery(
    'CREATE TABLE lineage_edges (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  source_column_id TEXT REFERENCES dataset_columns(id),' +
    '  target_column_id TEXT REFERENCES dataset_columns(id),' +
    '  transformation_id TEXT REFERENCES transformations(id),' +
    '  edge_type TEXT DEFAULT ''direct''' +  // direct, derived, aggregated
    ')');

  // Lineage events (audit trail)
  Conn.ExecuteNonQuery(
    'CREATE TABLE lineage_events (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  dataset_id TEXT REFERENCES datasets(id),' +
    '  event_type TEXT NOT NULL,' +  // ingested, transformed, validated, published
    '  details TEXT,' +
    '  occurred_at TEXT NOT NULL' +
    ')');

  Conn.ExecuteNonQuery('CREATE INDEX idx_edges_source ON lineage_edges(source_column_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_edges_target ON lineage_edges(target_column_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_events_dataset ON lineage_events(dataset_id, occurred_at)');
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  // === Data Sources ===
  Conn.ExecuteNonQuery('INSERT INTO data_sources VALUES (''src_crm'', ''CRM Database'', ''database'', ''postgres://crm-prod:5432/crm'', ''Customer relationship management'')');
  Conn.ExecuteNonQuery('INSERT INTO data_sources VALUES (''src_erp'', ''ERP System'', ''api'', ''https://erp.company.com/api/v2'', ''Enterprise resource planning'')');
  Conn.ExecuteNonQuery('INSERT INTO data_sources VALUES (''src_csv'', ''Sales CSV Export'', ''csv'', ''/data/imports/sales_2024.csv'', ''Monthly sales data export'')');
  Conn.ExecuteNonQuery('INSERT INTO data_sources VALUES (''src_web'', ''Web Analytics'', ''api'', ''https://analytics.company.com/api'', ''Website traffic and events'')');

  // === Raw Datasets ===
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_customers_raw'', ''Raw Customers'', ''src_crm'', ''raw'', 50000, ''2024-01-15 08:00:00'', ''2024-03-01 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_orders_raw'', ''Raw Orders'', ''src_erp'', ''raw'', 120000, ''2024-01-15 08:30:00'', ''2024-03-01 08:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_sales_raw'', ''Raw Sales'', ''src_csv'', ''raw'', 85000, ''2024-02-01 09:00:00'', ''2024-03-01 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_traffic_raw'', ''Raw Traffic'', ''src_web'', ''raw'', 500000, ''2024-01-20 07:00:00'', ''2024-03-01 07:00:00'')');

  // === Cleaned Datasets ===
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_customers_clean'', ''Cleaned Customers'', ''src_crm'', ''cleaned'', 48500, ''2024-01-15 10:00:00'', ''2024-03-01 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_orders_clean'', ''Cleaned Orders'', ''src_erp'', ''cleaned'', 118000, ''2024-01-15 10:30:00'', ''2024-03-01 10:30:00'')');

  // === Derived/Aggregated Datasets ===
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_customer_360'', ''Customer 360 View'', NULL, ''derived'', 48500, ''2024-01-16 06:00:00'', ''2024-03-01 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_revenue_monthly'', ''Monthly Revenue'', NULL, ''aggregated'', 36, ''2024-01-16 07:00:00'', ''2024-03-01 13:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO datasets VALUES (''ds_conversion_funnel'', ''Conversion Funnel'', NULL, ''derived'', 1200, ''2024-02-01 12:00:00'', ''2024-03-01 14:00:00'')');

  // === Dataset Columns ===
  // Raw Customers columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_cust_id'', ''ds_customers_raw'', ''customer_id'', ''INTEGER'', ''Primary key'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_cust_name'', ''ds_customers_raw'', ''full_name'', ''TEXT'', ''Customer full name'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_cust_email'', ''ds_customers_raw'', ''email'', ''TEXT'', ''Contact email'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_cust_created'', ''ds_customers_raw'', ''created_at'', ''DATETIME'', ''Account creation date'')');

  // Raw Orders columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_ord_id'', ''ds_orders_raw'', ''order_id'', ''INTEGER'', ''Order primary key'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_ord_cust'', ''ds_orders_raw'', ''customer_id'', ''INTEGER'', ''FK to customer'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_ord_total'', ''ds_orders_raw'', ''total_amount'', ''REAL'', ''Order total'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_ord_date'', ''ds_orders_raw'', ''order_date'', ''DATE'', ''Order placement date'')');

  // Raw Sales columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_sales_id'', ''ds_sales_raw'', ''sale_id'', ''INTEGER'', ''Sale identifier'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_sales_amount'', ''ds_sales_raw'', ''amount'', ''REAL'', ''Sale amount'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_sales_date'', ''ds_sales_raw'', ''sale_date'', ''DATE'', ''Sale date'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_sales_region'', ''ds_sales_raw'', ''region'', ''TEXT'', ''Sales region'')');

  // Raw Traffic columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_traf_session'', ''ds_traffic_raw'', ''session_id'', ''TEXT'', ''Session identifier'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_traf_user'', ''ds_traffic_raw'', ''user_id'', ''INTEGER'', ''Matched user ID'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_traf_page'', ''ds_traffic_raw'', ''page_url'', ''TEXT'', ''Page visited'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_traf_ts'', ''ds_traffic_raw'', ''event_ts'', ''DATETIME'', ''Event timestamp'')');

  // Cleaned Customers columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_cc_id'', ''ds_customers_clean'', ''customer_id'', ''INTEGER'', ''Validated customer ID'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_cc_name'', ''ds_customers_clean'', ''full_name'', ''TEXT'', ''Trimmed and normalized name'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_cc_email'', ''ds_customers_clean'', ''email'', ''TEXT'', ''Validated email'')');

  // Cleaned Orders columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_co_id'', ''ds_orders_clean'', ''order_id'', ''INTEGER'', ''Validated order ID'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_co_cust'', ''ds_orders_clean'', ''customer_id'', ''INTEGER'', ''Validated FK'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_co_total'', ''ds_orders_clean'', ''total_amount'', ''REAL'', ''Validated amount'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_co_date'', ''ds_orders_clean'', ''order_date'', ''DATE'', ''Validated date'')');

  // Customer 360 columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_360_id'', ''ds_customer_360'', ''customer_id'', ''INTEGER'', ''Customer ID'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_360_name'', ''ds_customer_360'', ''full_name'', ''TEXT'', ''Customer name'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_360_orders'', ''ds_customer_360'', ''total_orders'', ''INTEGER'', ''Order count'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_360_revenue'', ''ds_customer_360'', ''total_revenue'', ''REAL'', ''Lifetime revenue'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_360_visits'', ''ds_customer_360'', ''web_visits'', ''INTEGER'', ''Total web visits'')');

  // Monthly Revenue columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_rev_month'', ''ds_revenue_monthly'', ''month'', ''TEXT'', ''Year-month'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_rev_total'', ''ds_revenue_monthly'', ''total_revenue'', ''REAL'', ''Monthly total'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_rev_orders'', ''ds_revenue_monthly'', ''order_count'', ''INTEGER'', ''Monthly orders'')');

  // Conversion Funnel columns
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_fun_stage'', ''ds_conversion_funnel'', ''funnel_stage'', ''TEXT'', ''Stage name'')');
  Conn.ExecuteNonQuery('INSERT INTO dataset_columns VALUES (''col_fun_count'', ''ds_conversion_funnel'', ''user_count'', ''INTEGER'', ''Users at stage'')');

  // === Transformations ===
  Conn.ExecuteNonQuery('INSERT INTO transformations VALUES (''t_clean_cust'', ''Clean Customers'', ''clean'', ''Remove nulls, trim names, validate emails'', ''2024-01-15 09:00:00'', ''data_engineer'')');
  Conn.ExecuteNonQuery('INSERT INTO transformations VALUES (''t_clean_ord'', ''Clean Orders'', ''clean'', ''Remove cancelled, validate amounts > 0'', ''2024-01-15 09:30:00'', ''data_engineer'')');
  Conn.ExecuteNonQuery('INSERT INTO transformations VALUES (''t_join_360'', ''Join Customer 360'', ''join'', ''Join customers + orders + traffic'', ''2024-01-16 05:00:00'', ''data_engineer'')');
  Conn.ExecuteNonQuery('INSERT INTO transformations VALUES (''t_agg_revenue'', ''Aggregate Revenue'', ''aggregate'', ''SUM orders by month'', ''2024-01-16 06:00:00'', ''analyst'')');
  Conn.ExecuteNonQuery('INSERT INTO transformations VALUES (''t_derive_funnel'', ''Derive Funnel'', ''derive'', ''Match traffic sessions to orders'', ''2024-02-01 11:00:00'', ''analyst'')');
  Conn.ExecuteNonQuery('INSERT INTO transformations VALUES (''t_agg_orders'', ''Count Orders'', ''aggregate'', ''COUNT orders per customer'', ''2024-01-16 05:30:00'', ''data_engineer'')');

  // === Lineage Edges ===
  // Raw -> Cleaned (customers)
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_cust_id'', ''col_cc_id'', ''t_clean_cust'', ''direct'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_cust_name'', ''col_cc_name'', ''t_clean_cust'', ''direct'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_cust_email'', ''col_cc_email'', ''t_clean_cust'', ''direct'')');

  // Raw -> Cleaned (orders)
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_ord_id'', ''col_co_id'', ''t_clean_ord'', ''direct'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_ord_cust'', ''col_co_cust'', ''t_clean_ord'', ''direct'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_ord_total'', ''col_co_total'', ''t_clean_ord'', ''direct'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_ord_date'', ''col_co_date'', ''t_clean_ord'', ''direct'')');

  // Cleaned -> Customer 360 (join)
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_cc_id'', ''col_360_id'', ''t_join_360'', ''direct'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_cc_name'', ''col_360_name'', ''t_join_360'', ''direct'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_co_id'', ''col_360_orders'', ''t_agg_orders'', ''aggregated'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_co_total'', ''col_360_revenue'', ''t_join_360'', ''aggregated'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_traf_user'', ''col_360_visits'', ''t_join_360'', ''aggregated'')');

  // Cleaned Orders -> Monthly Revenue (aggregate)
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_co_date'', ''col_rev_month'', ''t_agg_revenue'', ''derived'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_co_total'', ''col_rev_total'', ''t_agg_revenue'', ''aggregated'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_co_id'', ''col_rev_orders'', ''t_agg_revenue'', ''aggregated'')');

  // Traffic + Orders -> Conversion Funnel (derive)
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_traf_session'', ''col_fun_stage'', ''t_derive_funnel'', ''derived'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_edges (source_column_id, target_column_id, transformation_id, edge_type) VALUES (''col_traf_user'', ''col_fun_count'', ''t_derive_funnel'', ''aggregated'')');

  // === Lineage Events ===
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_customers_raw'', ''ingested'', ''Full load from CRM, 50000 rows'', ''2024-03-01 08:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_orders_raw'', ''ingested'', ''Incremental load, 2500 new orders'', ''2024-03-01 08:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_customers_clean'', ''transformed'', ''1500 invalid emails removed'', ''2024-03-01 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_orders_clean'', ''transformed'', ''2000 cancelled orders filtered'', ''2024-03-01 10:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_customer_360'', ''transformed'', ''Join completed, 48500 profiles'', ''2024-03-01 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_customer_360'', ''validated'', ''All customer_ids present in source'', ''2024-03-01 12:05:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_revenue_monthly'', ''transformed'', ''36 months aggregated'', ''2024-03-01 13:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO lineage_events (dataset_id, event_type, details, occurred_at) VALUES (''ds_revenue_monthly'', ''published'', ''Dashboard refreshed'', ''2024-03-01 13:05:00'')');
end;

// === Feature 1: Data Sources & Datasets ===
{ Queries and prints all registered data sources with their type and URI, then
  lists all datasets with their type, row count, and originating source. }
procedure DemoDataSources;
begin
  WriteLn('=== 1. Data Sources & Datasets ===');
  WriteLn;

  WriteLn('   Data Sources:');
  DS := Conn.ExecuteQuery(
    'SELECT id, name, source_type, uri FROM data_sources ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%s] %-18s (%s)',
        [DS.FieldByName('source_type').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('uri').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Datasets:');
  DS := Conn.ExecuteQuery(
    'SELECT d.name, d.dataset_type, d.row_count, ' +
    '  COALESCE(s.name, ''(derived)'') as source_name ' +
    'FROM datasets d ' +
    'LEFT JOIN data_sources s ON s.id = d.source_id ' +
    'ORDER BY d.dataset_type, d.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s [%-10s] %6d rows  <- %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('dataset_type').AsString,
         DS.FieldByName('row_count').AsInteger,
         DS.FieldByName('source_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 2: Transformation Chain ===
{ Lists all ETL transformations in chronological order, showing their type,
  description, edge count, and the user who created them. }
procedure DemoTransformationChain;
begin
  WriteLn('=== 2. Transformation Chain ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT t.name, t.transform_type, t.description, t.created_by, ' +
    '  (SELECT COUNT(*) FROM lineage_edges WHERE transformation_id = t.id) as edge_count ' +
    'FROM transformations t ' +
    'ORDER BY t.created_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%s] %s (%d edges)',
        [DS.FieldByName('transform_type').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('edge_count').AsInteger]));
      WriteLn(Format('      %s (by %s)',
        [DS.FieldByName('description').AsString,
         DS.FieldByName('created_by').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 3: Column-Level Lineage ===
{ Traces column-level lineage for the Customer 360 dataset, showing each target
  column's source column, source dataset, transformation, and edge type. }
procedure DemoColumnLineage;
begin
  WriteLn('=== 3. Column-Level Lineage ===');
  WriteLn;

  WriteLn('   Lineage for "Customer 360 View" columns:');
  DS := Conn.ExecuteQuery(
    'SELECT tc.column_name as target_col, ' +
    '  sc.column_name as source_col, ' +
    '  sd.name as source_dataset, ' +
    '  t.name as transform_name, ' +
    '  le.edge_type ' +
    'FROM lineage_edges le ' +
    'JOIN dataset_columns tc ON tc.id = le.target_column_id ' +
    'JOIN dataset_columns sc ON sc.id = le.source_column_id ' +
    'JOIN datasets sd ON sd.id = sc.dataset_id ' +
    'JOIN transformations t ON t.id = le.transformation_id ' +
    'WHERE tc.dataset_id = ''ds_customer_360'' ' +
    'ORDER BY tc.column_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s <- %-15s [%s] via "%s" (%s)',
        [DS.FieldByName('target_col').AsString,
         DS.FieldByName('source_col').AsString,
         DS.FieldByName('source_dataset').AsString,
         DS.FieldByName('transform_name').AsString,
         DS.FieldByName('edge_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 4: Forward Lineage (Downstream Impact) ===
{ Uses a recursive CTE to find all downstream columns that depend on the Raw
  Customers dataset, showing multi-hop dependencies with their depth level. }
procedure DemoForwardLineage;
begin
  WriteLn('=== 4. Forward Lineage: What depends on "Raw Customers"? ===');
  WriteLn;

  // Find all downstream columns from ds_customers_raw (multi-hop via recursive CTE)
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE downstream AS (' +
    '  SELECT le.target_column_id as col_id, 1 as depth ' +
    '  FROM lineage_edges le ' +
    '  JOIN dataset_columns sc ON sc.id = le.source_column_id ' +
    '  WHERE sc.dataset_id = ''ds_customers_raw'' ' +
    '  UNION ' +
    '  SELECT le2.target_column_id, d.depth + 1 ' +
    '  FROM downstream d ' +
    '  JOIN lineage_edges le2 ON le2.source_column_id = d.col_id ' +
    '  WHERE d.depth < 5' +
    ') ' +
    'SELECT DISTINCT ds.name as dataset_name, dc.column_name, d.depth ' +
    'FROM downstream d ' +
    'JOIN dataset_columns dc ON dc.id = d.col_id ' +
    'JOIN datasets ds ON ds.id = dc.dataset_id ' +
    'ORDER BY d.depth, ds.name, dc.column_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [depth %d] %s.%s',
        [DS.FieldByName('depth').AsInteger,
         DS.FieldByName('dataset_name').AsString,
         DS.FieldByName('column_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 5: Backward Lineage (Origin Tracking) ===
{ Uses a recursive CTE to trace the origin of the total_revenue column in the
  Customer 360 dataset back through all upstream transformations and sources. }
procedure DemoBackwardLineage;
begin
  WriteLn('=== 5. Backward Lineage: Where does "total_revenue" in Customer 360 come from? ===');
  WriteLn;

  // Trace back from col_360_revenue
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE upstream AS (' +
    '  SELECT le.source_column_id as col_id, le.transformation_id as t_id, 1 as depth ' +
    '  FROM lineage_edges le ' +
    '  WHERE le.target_column_id = ''col_360_revenue'' ' +
    '  UNION ' +
    '  SELECT le2.source_column_id, le2.transformation_id, u.depth + 1 ' +
    '  FROM upstream u ' +
    '  JOIN lineage_edges le2 ON le2.target_column_id = u.col_id ' +
    '  WHERE u.depth < 5' +
    ') ' +
    'SELECT ds.name as dataset_name, dc.column_name, t.name as transform_name, u.depth ' +
    'FROM upstream u ' +
    'JOIN dataset_columns dc ON dc.id = u.col_id ' +
    'JOIN datasets ds ON ds.id = dc.dataset_id ' +
    'LEFT JOIN transformations t ON t.id = u.t_id ' +
    'ORDER BY u.depth DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [depth %d] %s.%s  (via "%s")',
        [DS.FieldByName('depth').AsInteger,
         DS.FieldByName('dataset_name').AsString,
         DS.FieldByName('column_name').AsString,
         DS.FieldByName('transform_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 6: Impact Analysis ===
{ Determines which datasets would be affected if the CRM data source becomes
  unavailable, using a recursive CTE to find all transitively dependent datasets. }
procedure DemoImpactAnalysis;
begin
  WriteLn('=== 6. Impact Analysis: If "CRM Database" goes down, what is affected? ===');
  WriteLn;

  // All datasets that depend on src_crm (directly or transitively)
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE impacted AS (' +
    '  SELECT DISTINCT dc.dataset_id ' +
    '  FROM lineage_edges le ' +
    '  JOIN dataset_columns sc ON sc.id = le.source_column_id ' +
    '  JOIN dataset_columns dc ON dc.id = le.target_column_id ' +
    '  WHERE sc.dataset_id IN (SELECT id FROM datasets WHERE source_id = ''src_crm'') ' +
    '  UNION ' +
    '  SELECT DISTINCT dc2.dataset_id ' +
    '  FROM impacted i ' +
    '  JOIN dataset_columns sc2 ON sc2.dataset_id = i.dataset_id ' +
    '  JOIN lineage_edges le2 ON le2.source_column_id = sc2.id ' +
    '  JOIN dataset_columns dc2 ON dc2.id = le2.target_column_id ' +
    ') ' +
    'SELECT d.name, d.dataset_type, d.row_count ' +
    'FROM impacted i ' +
    'JOIN datasets d ON d.id = i.dataset_id ' +
    'ORDER BY d.dataset_type, d.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   AFFECTED: %-22s [%-10s] %6d rows',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('dataset_type').AsString,
         DS.FieldByName('row_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 7: Lineage Events (Audit Trail) ===
{ Prints the chronological audit trail of lineage events, showing when each
  dataset was ingested, transformed, validated, or published along with details. }
procedure DemoLineageEvents;
begin
  WriteLn('=== 7. Lineage Events (Audit Trail) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT le.occurred_at, d.name as dataset, le.event_type, le.details ' +
    'FROM lineage_events le ' +
    'JOIN datasets d ON d.id = le.dataset_id ' +
    'ORDER BY le.occurred_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s [%-11s] %-22s %s',
        [DS.FieldByName('occurred_at').AsString,
         DS.FieldByName('event_type').AsString,
         DS.FieldByName('dataset').AsString,
         DS.FieldByName('details').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 8: Data Freshness ===
{ Calculates and displays how many hours ago each dataset was last updated,
  ordered by staleness to highlight datasets most in need of refresh. }
procedure DemoDataFreshness;
begin
  WriteLn('=== 8. Data Freshness Report ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT d.name, d.dataset_type, d.updated_at, ' +
    '  CAST(julianday(''2024-03-01 15:00:00'') - julianday(d.updated_at) AS REAL) * 24 as hours_stale ' +
    'FROM datasets d ' +
    'ORDER BY hours_stale DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s [%-10s] updated: %s  (%.1fh ago)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('dataset_type').AsString,
         DS.FieldByName('updated_at').AsString,
         DS.FieldByName('hours_stale').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 9: Statistics ===
{ Prints aggregate counts for data sources, datasets, columns, transformations,
  edges, and events, then breaks down datasets by type and edges by edge type. }
procedure DemoStatistics;
begin
  WriteLn('=== 9. Lineage Statistics ===');
  WriteLn;

  WriteLn(Format('   Data sources:       %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM data_sources'))]));
  WriteLn(Format('   Datasets:           %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM datasets'))]));
  WriteLn(Format('   Columns tracked:    %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM dataset_columns'))]));
  WriteLn(Format('   Transformations:    %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM transformations'))]));
  WriteLn(Format('   Lineage edges:      %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM lineage_edges'))]));
  WriteLn(Format('   Lineage events:     %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM lineage_events'))]));

  WriteLn;
  WriteLn('   Datasets by type:');
  DS := Conn.ExecuteQuery(
    'SELECT dataset_type, COUNT(*) as cnt, SUM(row_count) as total_rows ' +
    'FROM datasets GROUP BY dataset_type ORDER BY dataset_type');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d datasets, %d total rows',
        [DS.FieldByName('dataset_type').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('total_rows').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Edge types:');
  DS := Conn.ExecuteQuery(
    'SELECT edge_type, COUNT(*) as cnt FROM lineage_edges GROUP BY edge_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d edges',
        [DS.FieldByName('edge_type').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Main Program ===
begin
  WriteLn('=== Example 106: Data Lineage ===');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    SeedData;

    DemoDataSources;
    DemoTransformationChain;
    DemoColumnLineage;
    DemoForwardLineage;
    DemoBackwardLineage;
    DemoImpactAnalysis;
    DemoLineageEvents;
    DemoDataFreshness;
    DemoStatistics;

    WriteLn('=== All data lineage features demonstrated successfully ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
