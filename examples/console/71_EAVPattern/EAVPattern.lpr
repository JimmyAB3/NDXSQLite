{===============================================================================
  NDXSQLite Example 71 - Entity-Attribute-Value (EAV) Pattern
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Basic EAV schema design
  - Typed attribute values (string, integer, float, boolean, date, JSON)
  - Pivot queries to convert EAV to relational format
  - Efficient querying with indexes
  - Attribute metadata and validation
  - Performance considerations

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program EAVPattern;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Variants, StrUtils,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the Entity-Attribute-Value schema with required tables. }
procedure CreateEAVSchema;
begin
  WriteLn('1. Creating EAV Schema');
  WriteLn('   ===================');

  // Entities table - the "things" we're describing
  Conn.ExecuteNonQuery(
    'CREATE TABLE entities (' +
    '  id INTEGER PRIMARY KEY,' +
    '  entity_type TEXT NOT NULL,' +  // e.g., 'product', 'user', 'config'
    '  entity_name TEXT NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Attributes table - defines what attributes are possible
  Conn.ExecuteNonQuery(
    'CREATE TABLE attributes (' +
    '  id INTEGER PRIMARY KEY,' +
    '  attribute_name TEXT NOT NULL UNIQUE,' +
    '  data_type TEXT NOT NULL,' +      // 'string', 'integer', 'float', 'boolean', 'date', 'json'
    '  entity_type TEXT,' +              // NULL = applies to all types
    '  is_required BOOLEAN DEFAULT 0,' +
    '  default_value TEXT,' +
    '  description TEXT' +
    ')');

  // Values table - the actual EAV data
  Conn.ExecuteNonQuery(
    'CREATE TABLE entity_values (' +
    '  id INTEGER PRIMARY KEY,' +
    '  entity_id INTEGER NOT NULL REFERENCES entities(id) ON DELETE CASCADE,' +
    '  attribute_id INTEGER NOT NULL REFERENCES attributes(id),' +
    '  value_string TEXT,' +
    '  value_integer INTEGER,' +
    '  value_float REAL,' +
    '  value_boolean INTEGER,' +
    '  value_date TEXT,' +
    '  value_json TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(entity_id, attribute_id)' +
    ')');

  // Indexes for efficient querying
  Conn.ExecuteNonQuery('CREATE INDEX idx_entities_type ON entities(entity_type)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_values_entity ON entity_values(entity_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_values_attribute ON entity_values(attribute_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_values_string ON entity_values(value_string) WHERE value_string IS NOT NULL');
  Conn.ExecuteNonQuery('CREATE INDEX idx_values_integer ON entity_values(value_integer) WHERE value_integer IS NOT NULL');

  WriteLn('   Created tables: entities, attributes, entity_values');
  WriteLn('   Created indexes for efficient querying');
  WriteLn('');
end;

// =============================================================================
// Define Attributes for Product Catalog
// =============================================================================
{ Inserts attribute definitions for products including common, electronics, clothing, and JSON attributes into the attributes table. }
procedure DefineProductAttributes;
begin
  WriteLn('2. Defining Product Attributes');
  WriteLn('   ============================');

  // Common attributes for all products
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, is_required, description) VALUES ' +
    '(''brand'', ''string'', ''product'', 1, ''Product brand name'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, is_required, description) VALUES ' +
    '(''price'', ''float'', ''product'', 1, ''Price in USD'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, is_required, description) VALUES ' +
    '(''in_stock'', ''boolean'', ''product'', 1, ''Is product in stock'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, is_required, description) VALUES ' +
    '(''release_date'', ''date'', ''product'', 0, ''Product release date'')');

  // Electronics-specific attributes
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''screen_size'', ''float'', ''product'', ''Screen size in inches'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''ram_gb'', ''integer'', ''product'', ''RAM in gigabytes'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''storage_gb'', ''integer'', ''product'', ''Storage in gigabytes'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''cpu_cores'', ''integer'', ''product'', ''Number of CPU cores'')');

  // Clothing-specific attributes
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''size'', ''string'', ''product'', ''Clothing size (S, M, L, XL)'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''color'', ''string'', ''product'', ''Product color'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''material'', ''string'', ''product'', ''Material composition'')');

  // JSON attribute for complex data
  Conn.ExecuteNonQuery(
    'INSERT INTO attributes (attribute_name, data_type, entity_type, description) VALUES ' +
    '(''specifications'', ''json'', ''product'', ''Additional specifications as JSON'')');

  WriteLn('   Defined 12 product attributes');
  WriteLn('   - Common: brand, price, in_stock, release_date');
  WriteLn('   - Electronics: screen_size, ram_gb, storage_gb, cpu_cores');
  WriteLn('   - Clothing: size, color, material');
  WriteLn('   - Complex: specifications (JSON)');
  WriteLn('');
end;

// =============================================================================
// Helper Functions
// =============================================================================
{ Looks up and returns the ID of an attribute given its name from the attributes table. }
function GetAttributeId(const AttrName: string): Integer;
begin
  Result := Conn.ExecuteScalar(
    'SELECT id FROM attributes WHERE attribute_name = ?', [AttrName]);
end;

{ Stores a typed attribute value for an entity by deleting any existing value and inserting into the appropriate type column (string, integer, float, boolean, date, or json). }
procedure SetEntityValue(EntityId: Integer; const AttrName: string; Value: Variant);
var
  AttrId: Integer;
  DataType: string;
begin
  AttrId := GetAttributeId(AttrName);
  DataType := VarToStr(Conn.ExecuteScalar(
    'SELECT data_type FROM attributes WHERE id = ?', [AttrId]));

  // Delete existing value if any
  Conn.ExecuteNonQuery(
    'DELETE FROM entity_values WHERE entity_id = ? AND attribute_id = ?',
    [EntityId, AttrId]);

  // Insert new value based on data type
  case DataType of
    'string':
      Conn.ExecuteNonQuery(
        'INSERT INTO entity_values (entity_id, attribute_id, value_string) VALUES (?, ?, ?)',
        [EntityId, AttrId, VarToStr(Value)]);
    'integer':
      Conn.ExecuteNonQuery(
        'INSERT INTO entity_values (entity_id, attribute_id, value_integer) VALUES (?, ?, ?)',
        [EntityId, AttrId, Integer(Value)]);
    'float':
      Conn.ExecuteNonQuery(
        'INSERT INTO entity_values (entity_id, attribute_id, value_float) VALUES (?, ?, ?)',
        [EntityId, AttrId, Double(Value)]);
    'boolean':
      Conn.ExecuteNonQuery(
        'INSERT INTO entity_values (entity_id, attribute_id, value_boolean) VALUES (?, ?, ?)',
        [EntityId, AttrId, Integer(Boolean(Value))]);
    'date':
      Conn.ExecuteNonQuery(
        'INSERT INTO entity_values (entity_id, attribute_id, value_date) VALUES (?, ?, ?)',
        [EntityId, AttrId, VarToStr(Value)]);
    'json':
      Conn.ExecuteNonQuery(
        'INSERT INTO entity_values (entity_id, attribute_id, value_json) VALUES (?, ?, ?)',
        [EntityId, AttrId, VarToStr(Value)]);
  end;
end;

{ Creates an entity record and returns its ID. }
function CreateEntity(const EntityType, EntityName: string): Integer;
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO entities (entity_type, entity_name) VALUES (?, ?)',
    [EntityType, EntityName]);
  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');
end;

// =============================================================================
// Create Sample Products
// =============================================================================
{ Creates sample product entities with attributes for demonstration. }
procedure CreateSampleProducts;
var
  ProductId: Integer;
begin
  WriteLn('3. Creating Sample Products');
  WriteLn('   =========================');

  // Laptop
  ProductId := CreateEntity('product', 'MacBook Pro 16"');
  SetEntityValue(ProductId, 'brand', 'Apple');
  SetEntityValue(ProductId, 'price', 2499.99);
  SetEntityValue(ProductId, 'in_stock', True);
  SetEntityValue(ProductId, 'release_date', '2024-01-15');
  SetEntityValue(ProductId, 'screen_size', 16.2);
  SetEntityValue(ProductId, 'ram_gb', 32);
  SetEntityValue(ProductId, 'storage_gb', 1024);
  SetEntityValue(ProductId, 'cpu_cores', 12);
  SetEntityValue(ProductId, 'specifications', '{"ports": ["USB-C", "HDMI", "MagSafe"], "battery_hours": 22}');
  WriteLn('   Created: MacBook Pro 16" (laptop with 9 attributes)');

  // Smartphone
  ProductId := CreateEntity('product', 'iPhone 15 Pro');
  SetEntityValue(ProductId, 'brand', 'Apple');
  SetEntityValue(ProductId, 'price', 1199.00);
  SetEntityValue(ProductId, 'in_stock', True);
  SetEntityValue(ProductId, 'release_date', '2023-09-22');
  SetEntityValue(ProductId, 'screen_size', 6.1);
  SetEntityValue(ProductId, 'ram_gb', 8);
  SetEntityValue(ProductId, 'storage_gb', 256);
  SetEntityValue(ProductId, 'specifications', '{"camera_mp": 48, "5g": true, "face_id": true}');
  WriteLn('   Created: iPhone 15 Pro (smartphone with 8 attributes)');

  // T-Shirt
  ProductId := CreateEntity('product', 'Classic Cotton T-Shirt');
  SetEntityValue(ProductId, 'brand', 'BasicWear');
  SetEntityValue(ProductId, 'price', 24.99);
  SetEntityValue(ProductId, 'in_stock', True);
  SetEntityValue(ProductId, 'size', 'M');
  SetEntityValue(ProductId, 'color', 'Navy Blue');
  SetEntityValue(ProductId, 'material', '100% Cotton');
  WriteLn('   Created: Classic Cotton T-Shirt (clothing with 6 attributes)');

  // Jeans
  ProductId := CreateEntity('product', 'Slim Fit Jeans');
  SetEntityValue(ProductId, 'brand', 'DenimCo');
  SetEntityValue(ProductId, 'price', 79.99);
  SetEntityValue(ProductId, 'in_stock', False);
  SetEntityValue(ProductId, 'size', 'L');
  SetEntityValue(ProductId, 'color', 'Dark Indigo');
  SetEntityValue(ProductId, 'material', '98% Cotton, 2% Elastane');
  WriteLn('   Created: Slim Fit Jeans (clothing with 6 attributes)');

  // Monitor
  ProductId := CreateEntity('product', 'UltraWide Monitor 34"');
  SetEntityValue(ProductId, 'brand', 'LG');
  SetEntityValue(ProductId, 'price', 699.99);
  SetEntityValue(ProductId, 'in_stock', True);
  SetEntityValue(ProductId, 'screen_size', 34.0);
  SetEntityValue(ProductId, 'specifications', '{"resolution": "3440x1440", "refresh_rate": 144, "panel": "IPS"}');
  WriteLn('   Created: UltraWide Monitor 34" (monitor with 5 attributes)');

  WriteLn('');
  WriteLn('   Total: 5 products with varying attributes');
  WriteLn('');
end;

// =============================================================================
// Query EAV Data
// =============================================================================
{ Queries the EAV tables to display all attributes of a specific product, find products by RAM threshold, and filter products by price range. }
procedure QueryEAVData;
var
  DS: TDataSet;
begin
  WriteLn('4. Querying EAV Data');
  WriteLn('   ==================');

  // Get all attributes for a specific product
  WriteLn('');
  WriteLn('   All attributes for "MacBook Pro 16":');
  WriteLn('   ------------------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT a.attribute_name, a.data_type, ' +
    '  COALESCE(v.value_string, ' +
    '    CAST(v.value_integer AS TEXT), ' +
    '    CAST(v.value_float AS TEXT), ' +
    '    CASE WHEN v.value_boolean IS NOT NULL THEN ' +
    '      CASE v.value_boolean WHEN 1 THEN ''true'' ELSE ''false'' END ' +
    '    END, ' +
    '    v.value_date, ' +
    '    v.value_json) AS value ' +
    'FROM entities e ' +
    'JOIN entity_values v ON e.id = v.entity_id ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE e.entity_name = ''MacBook Pro 16"'' ' +
    'ORDER BY a.attribute_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%s): %s',
        [DS.FieldByName('attribute_name').AsString,
         DS.FieldByName('data_type').AsString,
         DS.FieldByName('value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Find products by attribute value
  WriteLn('');
  WriteLn('   Products with RAM >= 16GB:');
  WriteLn('   --------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT e.entity_name, v.value_integer AS ram_gb ' +
    'FROM entities e ' +
    'JOIN entity_values v ON e.id = v.entity_id ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE a.attribute_name = ''ram_gb'' AND v.value_integer >= 16');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d GB',
        [DS.FieldByName('entity_name').AsString,
         DS.FieldByName('ram_gb').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Find products by price range
  WriteLn('');
  WriteLn('   Products priced $50-$100:');
  WriteLn('   -------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT e.entity_name, v.value_float AS price ' +
    'FROM entities e ' +
    'JOIN entity_values v ON e.id = v.entity_id ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE a.attribute_name = ''price'' ' +
    '  AND v.value_float BETWEEN 50 AND 100');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: $%.2f',
        [DS.FieldByName('entity_name').AsString,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Pivot Query - Convert EAV to Relational Format
// =============================================================================
{ Executes a pivot query that transforms EAV rows into a relational table format showing electronics products with brand, price, screen size, RAM, storage, and stock status as columns. }
procedure DemoPivotQuery;
var
  DS: TDataSet;
  RamStr, StorageStr, StockStr: string;
begin
  WriteLn('5. Pivot Query - EAV to Relational');
  WriteLn('   =================================');
  WriteLn('');
  WriteLn('   Converting EAV data to traditional table format:');
  WriteLn('');

  // Create a pivot view for electronics
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  e.entity_name AS product, ' +
    '  MAX(CASE WHEN a.attribute_name = ''brand'' THEN v.value_string END) AS brand, ' +
    '  MAX(CASE WHEN a.attribute_name = ''price'' THEN v.value_float END) AS price, ' +
    '  MAX(CASE WHEN a.attribute_name = ''screen_size'' THEN v.value_float END) AS screen, ' +
    '  MAX(CASE WHEN a.attribute_name = ''ram_gb'' THEN v.value_integer END) AS ram, ' +
    '  MAX(CASE WHEN a.attribute_name = ''storage_gb'' THEN v.value_integer END) AS storage, ' +
    '  MAX(CASE WHEN a.attribute_name = ''in_stock'' THEN v.value_boolean END) AS in_stock ' +
    'FROM entities e ' +
    'JOIN entity_values v ON e.id = v.entity_id ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE e.entity_type = ''product'' ' +
    'GROUP BY e.id, e.entity_name ' +
    'HAVING screen IS NOT NULL ' +  // Only electronics with screen
    'ORDER BY price DESC');
  try
    WriteLn('   Product              | Brand  | Price    | Screen | RAM  | Storage | Stock');
    WriteLn('   ---------------------|--------|----------|--------|------|---------|------');
    while not DS.EOF do
    begin
      // Get RAM value safely
      if DS.FieldByName('ram').IsNull or (DS.FieldByName('ram').AsString = '') then
        RamStr := 'N/A'
      else
        RamStr := IntToStr(DS.FieldByName('ram').AsInteger);

      // Get Storage value safely
      if DS.FieldByName('storage').IsNull or (DS.FieldByName('storage').AsString = '') then
        StorageStr := 'N/A'
      else
        StorageStr := IntToStr(DS.FieldByName('storage').AsInteger);

      // Get in_stock value safely
      if DS.FieldByName('in_stock').IsNull or (DS.FieldByName('in_stock').AsString = '') then
        StockStr := 'N/A'
      else if DS.FieldByName('in_stock').AsInteger = 1 then
        StockStr := 'Yes'
      else
        StockStr := 'No';

      WriteLn(Format('   %-20s | %-6s | $%7.2f | %4.1f" | %3s | %4s GB | %s',
        [Copy(DS.FieldByName('product').AsString, 1, 20),
         DS.FieldByName('brand').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('screen').AsFloat,
         RamStr,
         StorageStr,
         StockStr]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Search with Multiple Criteria
// =============================================================================
{ Searches for products matching multiple attribute criteria (brand=Apple, price<2000, in_stock=true) using HAVING clauses on pivoted EAV data. }
procedure DemoMultiCriteriaSearch;
var
  DS: TDataSet;
begin
  WriteLn('6. Multi-Criteria Search');
  WriteLn('   ======================');
  WriteLn('');
  WriteLn('   Finding products: brand=Apple AND price<2000 AND in_stock=true');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT e.entity_name, ' +
    '  MAX(CASE WHEN a.attribute_name = ''brand'' THEN v.value_string END) AS brand, ' +
    '  MAX(CASE WHEN a.attribute_name = ''price'' THEN v.value_float END) AS price ' +
    'FROM entities e ' +
    'JOIN entity_values v ON e.id = v.entity_id ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE e.entity_type = ''product'' ' +
    'GROUP BY e.id ' +
    'HAVING ' +
    '  MAX(CASE WHEN a.attribute_name = ''brand'' THEN v.value_string END) = ''Apple'' ' +
    '  AND MAX(CASE WHEN a.attribute_name = ''price'' THEN v.value_float END) < 2000 ' +
    '  AND MAX(CASE WHEN a.attribute_name = ''in_stock'' THEN v.value_boolean END) = 1');
  try
    WriteLn('   Results:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s - %s - $%.2f',
        [DS.FieldByName('entity_name').AsString,
         DS.FieldByName('brand').AsString,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// JSON Attribute Queries
// =============================================================================
{ Extracts and displays nested fields (battery, camera, resolution, refresh rate) from JSON-typed specification attributes using json_extract. }
procedure DemoJSONQueries;
var
  DS: TDataSet;
begin
  WriteLn('7. JSON Attribute Queries');
  WriteLn('   =======================');
  WriteLn('');

  // Extract JSON fields using json_extract
  WriteLn('   Extracting data from JSON specifications:');
  WriteLn('');
  DS := Conn.ExecuteQuery(
    'SELECT e.entity_name, ' +
    '  json_extract(v.value_json, ''$.battery_hours'') AS battery, ' +
    '  json_extract(v.value_json, ''$.camera_mp'') AS camera, ' +
    '  json_extract(v.value_json, ''$.resolution'') AS resolution, ' +
    '  json_extract(v.value_json, ''$.refresh_rate'') AS refresh_rate ' +
    'FROM entities e ' +
    'JOIN entity_values v ON e.id = v.entity_id ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE a.attribute_name = ''specifications'' ' +
    '  AND v.value_json IS NOT NULL');
  try
    while not DS.EOF do
    begin
      Write(Format('     %s: ', [DS.FieldByName('entity_name').AsString]));
      if not DS.FieldByName('battery').IsNull then
        Write(Format('Battery=%sh ', [DS.FieldByName('battery').AsString]));
      if not DS.FieldByName('camera').IsNull then
        Write(Format('Camera=%sMP ', [DS.FieldByName('camera').AsString]));
      if not DS.FieldByName('resolution').IsNull then
        Write(Format('Resolution=%s ', [DS.FieldByName('resolution').AsString]));
      if not DS.FieldByName('refresh_rate').IsNull then
        Write(Format('Refresh=%sHz ', [DS.FieldByName('refresh_rate').AsString]));
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Attribute Statistics
// =============================================================================
{ Displays attribute usage frequency across all entities and computes price statistics (count, min, max, average) from the EAV value table. }
procedure DemoAttributeStats;
var
  DS: TDataSet;
begin
  WriteLn('8. Attribute Statistics');
  WriteLn('   =====================');
  WriteLn('');

  // Count usage of each attribute
  WriteLn('   Attribute usage frequency:');
  WriteLn('');
  DS := Conn.ExecuteQuery(
    'SELECT attribute_name, data_type, ' +
    '  (SELECT COUNT(*) FROM entity_values v WHERE v.attribute_id = a.id) AS usage_count ' +
    'FROM attributes a ' +
    'ORDER BY usage_count DESC, attribute_name');
  try
    WriteLn('   Attribute         | Type    | Used');
    WriteLn('   ------------------|---------|-----');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-17s | %-7s | %d',
        [DS.FieldByName('attribute_name').AsString,
         DS.FieldByName('data_type').AsString,
         DS.FieldByName('usage_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Price statistics
  WriteLn('');
  WriteLn('   Price statistics:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  COUNT(*) AS count, ' +
    '  MIN(v.value_float) AS min_price, ' +
    '  MAX(v.value_float) AS max_price, ' +
    '  AVG(v.value_float) AS avg_price ' +
    'FROM entity_values v ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE a.attribute_name = ''price''');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('     Products: %d', [DS.FieldByName('count').AsInteger]));
      WriteLn(Format('     Min: $%.2f', [DS.FieldByName('min_price').AsFloat]));
      WriteLn(Format('     Max: $%.2f', [DS.FieldByName('max_price').AsFloat]));
      WriteLn(Format('     Avg: $%.2f', [DS.FieldByName('avg_price').AsFloat]));
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Create Materialized View for Performance
// =============================================================================
{ Creates a denormalized products table by pivoting EAV data into fixed columns, adds indexes, and queries it to show faster access compared to EAV joins. }
procedure DemoMaterializedView;
var
  DS: TDataSet;
begin
  WriteLn('9. Materialized View for Performance');
  WriteLn('   ===================================');
  WriteLn('');

  // Create a denormalized table for fast product queries
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products_denormalized AS ' +
    'SELECT ' +
    '  e.id, ' +
    '  e.entity_name AS name, ' +
    '  MAX(CASE WHEN a.attribute_name = ''brand'' THEN v.value_string END) AS brand, ' +
    '  MAX(CASE WHEN a.attribute_name = ''price'' THEN v.value_float END) AS price, ' +
    '  MAX(CASE WHEN a.attribute_name = ''in_stock'' THEN v.value_boolean END) AS in_stock, ' +
    '  MAX(CASE WHEN a.attribute_name = ''screen_size'' THEN v.value_float END) AS screen_size, ' +
    '  MAX(CASE WHEN a.attribute_name = ''ram_gb'' THEN v.value_integer END) AS ram_gb, ' +
    '  MAX(CASE WHEN a.attribute_name = ''size'' THEN v.value_string END) AS size, ' +
    '  MAX(CASE WHEN a.attribute_name = ''color'' THEN v.value_string END) AS color ' +
    'FROM entities e ' +
    'JOIN entity_values v ON e.id = v.entity_id ' +
    'JOIN attributes a ON v.attribute_id = a.id ' +
    'WHERE e.entity_type = ''product'' ' +
    'GROUP BY e.id');

  // Create indexes on the denormalized table
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_prod_brand ON products_denormalized(brand)');
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_prod_price ON products_denormalized(price)');

  WriteLn('   Created denormalized table: products_denormalized');
  WriteLn('   This table can be refreshed periodically for fast queries.');
  WriteLn('');

  // Query the denormalized table (much faster for large datasets)
  WriteLn('   Fast query on denormalized data:');
  DS := Conn.ExecuteQuery(
    'SELECT name, brand, price, ' +
    '  CASE WHEN in_stock = 1 THEN ''Yes'' ELSE ''No'' END AS stock ' +
    'FROM products_denormalized ' +
    'ORDER BY price DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s | %s | $%.2f | Stock: %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('brand').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('stock').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Summary
// =============================================================================
{ Outputs a summary of EAV pattern advantages, disadvantages, best practices, and recommended use cases. }
procedure PrintSummary;
begin
  WriteLn('10. EAV Pattern Summary');
  WriteLn('    ====================');
  WriteLn('');
  WriteLn('    Advantages:');
  WriteLn('      + Highly flexible schema - add attributes without ALTER TABLE');
  WriteLn('      + Different entities can have different attributes');
  WriteLn('      + Easy to add/remove attributes at runtime');
  WriteLn('      + Good for sparse data (not all entities have all attributes)');
  WriteLn('');
  WriteLn('    Disadvantages:');
  WriteLn('      - Complex queries (lots of JOINs)');
  WriteLn('      - No database-level type safety');
  WriteLn('      - Difficult to enforce constraints');
  WriteLn('      - Performance can suffer with large datasets');
  WriteLn('');
  WriteLn('    Best Practices:');
  WriteLn('      1. Use typed value columns (value_string, value_integer, etc.)');
  WriteLn('      2. Create indexes on frequently queried attributes');
  WriteLn('      3. Use materialized views for complex/frequent queries');
  WriteLn('      4. Consider JSON columns for complex nested data');
  WriteLn('      5. Validate data types in application layer');
  WriteLn('');
  WriteLn('    When to use EAV:');
  WriteLn('      - Product catalogs with varying specifications');
  WriteLn('      - User preferences/settings');
  WriteLn('      - Custom fields (CRM, CMS, forms)');
  WriteLn('      - Configuration management');
  WriteLn('      - Medical/scientific data with varying measurements');
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 71: Entity-Attribute-Value (EAV) Pattern ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateEAVSchema;
    DefineProductAttributes;
    CreateSampleProducts;
    QueryEAVData;
    DemoPivotQuery;
    DemoMultiCriteriaSearch;
    DemoJSONQueries;
    DemoAttributeStats;
    DemoMaterializedView;
    PrintSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
