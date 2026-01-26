{===============================================================================
  NDXSQLite Example 49 - Document Store Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - NoSQL-style document storage with JSON
  - Schema-less collections
  - Document CRUD operations
  - Querying JSON documents
  - Indexing JSON fields
  - Document validation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DocumentStore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the collections and documents tables with triggers that maintain document counts. }
procedure SetupDocumentStore;
begin
  // Collections metadata
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS collections (' +
    '  name TEXT PRIMARY KEY,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  document_count INTEGER DEFAULT 0,' +
    '  schema TEXT' +                            // Optional JSON schema
    ')');

  // Documents table (generic)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS documents (' +
    '  id TEXT PRIMARY KEY,' +                   // UUID or custom ID
    '  collection TEXT NOT NULL,' +
    '  data TEXT NOT NULL,' +                    // JSON document
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now'')),' +
    '  version INTEGER DEFAULT 1' +
    ')');

  // Indexes on common JSON paths (can add more dynamically)
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_documents_collection ON documents(collection)');

  // Trigger to update collection count
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS update_collection_count_insert ' +
    'AFTER INSERT ON documents BEGIN ' +
    '  UPDATE collections SET document_count = document_count + 1 ' +
    '  WHERE name = NEW.collection; ' +
    'END');

  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS update_collection_count_delete ' +
    'AFTER DELETE ON documents BEGIN ' +
    '  UPDATE collections SET document_count = document_count - 1 ' +
    '  WHERE name = OLD.collection; ' +
    'END');
end;

// ============================================================================
// Document operations
// ============================================================================

{ Generates a unique document identifier from a GUID, formatted as a lowercase 36-character string. }
function GenerateId: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := LowerCase(Copy(GUIDToString(GUID), 2, 36));
end;

{ Registers a new collection name in the collections table, optionally with a JSON schema. }
procedure CreateCollection(const AName: string; const ASchema: string = '');
begin
  Connection.ExecuteNonQuery(
    'INSERT OR IGNORE INTO collections (name, schema) VALUES (?, ?)',
    [AName, ASchema]);
end;

{ Inserts a document into a collection and returns its ID. }
function InsertDocument(const ACollection, AData: string; const AId: string = ''): string;
var
  DocId: string;
begin
  if AId = '' then
    DocId := GenerateId
  else
    DocId := AId;

  // Ensure collection exists
  CreateCollection(ACollection);

  Connection.ExecuteNonQuery(
    'INSERT INTO documents (id, collection, data) VALUES (?, ?, ?)',
    [DocId, ACollection, AData]);

  Result := DocId;
end;

{ Retrieves a document's JSON data by collection and ID. }
function GetDocument(const ACollection, AId: string): string;
var
  DS: TDataSet;
begin
  Result := '';
  DS := Connection.ExecuteQuery(
    'SELECT data FROM documents WHERE collection = ? AND id = ?',
    [ACollection, AId]);
  try
    if not DS.EOF then
      Result := DS.FieldByName('data').AsString;
  finally
    DS.Free;
  end;
end;

{ Replaces the JSON data of a document, increments its version, and updates the timestamp. }
procedure UpdateDocument(const ACollection, AId, AData: string);
begin
  Connection.ExecuteNonQuery(
    'UPDATE documents SET data = ?, updated_at = datetime(''now''), version = version + 1 ' +
    'WHERE collection = ? AND id = ?',
    [AData, ACollection, AId]);
end;

{ Deletes a document from its collection by ID. }
procedure DeleteDocument(const ACollection, AId: string);
begin
  Connection.ExecuteNonQuery(
    'DELETE FROM documents WHERE collection = ? AND id = ?',
    [ACollection, AId]);
end;

// ============================================================================
// Demo procedures
// ============================================================================

{ Inserts user documents into a collection, reads one back, and updates it with new data. }
procedure DemoBasicOperations;
var
  DocId: string;
  Doc: string;
begin
  WriteLn('1. Basic document operations');
  WriteLn('   -------------------------');
  WriteLn('');

  // Insert documents
  WriteLn('   Inserting documents into "users" collection...');
  DocId := InsertDocument('users',
    '{"name": "Alice Smith", "email": "alice@example.com", "age": 30, "city": "New York"}');
  WriteLn('   Created document: ', DocId);

  InsertDocument('users',
    '{"name": "Bob Jones", "email": "bob@example.com", "age": 25, "city": "Los Angeles"}');
  InsertDocument('users',
    '{"name": "Carol White", "email": "carol@example.com", "age": 35, "city": "Chicago"}');

  WriteLn('');

  // Read document
  WriteLn('   Reading document...');
  Doc := GetDocument('users', DocId);
  WriteLn('   Document: ', Doc);
  WriteLn('');

  // Update document
  WriteLn('   Updating document...');
  UpdateDocument('users', DocId,
    '{"name": "Alice Smith", "email": "alice@example.com", "age": 31, "city": "Boston"}');
  Doc := GetDocument('users', DocId);
  WriteLn('   Updated: ', Doc);

  WriteLn('');
end;

{ Inserts product documents and queries them by JSON field values including category and price filters. }
procedure DemoJsonQueries;
var
  DS: TDataSet;
begin
  WriteLn('2. Querying JSON documents');
  WriteLn('   -----------------------');
  WriteLn('');

  // Add more sample data
  InsertDocument('products',
    '{"name": "Laptop", "price": 999.99, "category": "electronics", "stock": 50, "tags": ["computer", "portable"]}');
  InsertDocument('products',
    '{"name": "Mouse", "price": 29.99, "category": "electronics", "stock": 200, "tags": ["peripheral"]}');
  InsertDocument('products',
    '{"name": "Desk", "price": 299.99, "category": "furniture", "stock": 30, "tags": ["office"]}');
  InsertDocument('products',
    '{"name": "Chair", "price": 199.99, "category": "furniture", "stock": 45, "tags": ["office", "ergonomic"]}');
  InsertDocument('products',
    '{"name": "Monitor", "price": 349.99, "category": "electronics", "stock": 75, "tags": ["display", "computer"]}');

  // Query by JSON field
  WriteLn('   Products in "electronics" category:');
  DS := Connection.ExecuteQuery(
    'SELECT id, JSON_EXTRACT(data, ''$.name'') as name, ' +
    '       JSON_EXTRACT(data, ''$.price'') as price ' +
    'FROM documents ' +
    'WHERE collection = ''products'' ' +
    '  AND JSON_EXTRACT(data, ''$.category'') = ''electronics''');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString,
              ': $', DS.FieldByName('price').AsFloat:0:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Query with comparison
  WriteLn('   Products with price > $200:');
  DS := Connection.ExecuteQuery(
    'SELECT JSON_EXTRACT(data, ''$.name'') as name, ' +
    '       JSON_EXTRACT(data, ''$.price'') as price ' +
    'FROM documents ' +
    'WHERE collection = ''products'' ' +
    '  AND CAST(JSON_EXTRACT(data, ''$.price'') AS REAL) > 200 ' +
    'ORDER BY price DESC');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString,
              ': $', DS.FieldByName('price').AsFloat:0:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Inserts an order with nested customer/address/items structure and extracts fields at multiple depths. }
procedure DemoNestedDocuments;
var
  DocId: string;
  DS: TDataSet;
begin
  WriteLn('3. Nested documents');
  WriteLn('   ----------------');
  WriteLn('');

  // Insert document with nested structure
  DocId := InsertDocument('orders',
    '{' +
    '  "order_number": "ORD-001",' +
    '  "customer": {' +
    '    "name": "John Doe",' +
    '    "email": "john@example.com",' +
    '    "address": {' +
    '      "street": "123 Main St",' +
    '      "city": "Springfield",' +
    '      "zip": "12345"' +
    '    }' +
    '  },' +
    '  "items": [' +
    '    {"product": "Laptop", "quantity": 1, "price": 999.99},' +
    '    {"product": "Mouse", "quantity": 2, "price": 29.99}' +
    '  ],' +
    '  "total": 1059.97' +
    '}');

  WriteLn('   Created nested order document');
  WriteLn('');

  // Query nested fields
  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  JSON_EXTRACT(data, ''$.order_number'') as order_num, ' +
    '  JSON_EXTRACT(data, ''$.customer.name'') as customer_name, ' +
    '  JSON_EXTRACT(data, ''$.customer.address.city'') as city, ' +
    '  JSON_EXTRACT(data, ''$.total'') as total, ' +
    '  JSON_ARRAY_LENGTH(JSON_EXTRACT(data, ''$.items'')) as item_count ' +
    'FROM documents WHERE id = ?',
    [DocId]);
  try
    WriteLn('   Order: ', DS.FieldByName('order_num').AsString);
    WriteLn('   Customer: ', DS.FieldByName('customer_name').AsString);
    WriteLn('   City: ', DS.FieldByName('city').AsString);
    WriteLn('   Total: $', DS.FieldByName('total').AsFloat:0:2);
    WriteLn('   Items: ', DS.FieldByName('item_count').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Queries products by array tag values using JSON_EACH and expands order line items from a JSON array. }
procedure DemoArrayOperations;
var
  DS: TDataSet;
begin
  WriteLn('4. Array operations');
  WriteLn('   ----------------');
  WriteLn('');

  // Query products with specific tags
  WriteLn('   Products with "computer" tag:');
  DS := Connection.ExecuteQuery(
    'SELECT JSON_EXTRACT(data, ''$.name'') as name, ' +
    '       JSON_EXTRACT(data, ''$.tags'') as tags ' +
    'FROM documents ' +
    'WHERE collection = ''products'' ' +
    '  AND EXISTS (SELECT 1 FROM JSON_EACH(JSON_EXTRACT(data, ''$.tags'')) WHERE value = ''computer'')');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString,
              ' ', DS.FieldByName('tags').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Expand array items
  WriteLn('   Expanding order items:');
  DS := Connection.ExecuteQuery(
    'SELECT JSON_EXTRACT(data, ''$.order_number'') as order_num, ' +
    '       JSON_EXTRACT(value, ''$.product'') as product, ' +
    '       JSON_EXTRACT(value, ''$.quantity'') as qty, ' +
    '       JSON_EXTRACT(value, ''$.price'') as price ' +
    'FROM documents, JSON_EACH(JSON_EXTRACT(data, ''$.items'')) ' +
    'WHERE collection = ''orders''');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('order_num').AsString, ': ',
              DS.FieldByName('product').AsString, ' x',
              DS.FieldByName('qty').AsString, ' @ $',
              DS.FieldByName('price').AsFloat:0:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates document aggregation queries across collections. }
procedure DemoAggregations;
var
  DS: TDataSet;
begin
  WriteLn('5. Document aggregations');
  WriteLn('   ---------------------');
  WriteLn('');

  // Aggregate by category
  WriteLn('   Products by category:');
  DS := Connection.ExecuteQuery(
    'SELECT JSON_EXTRACT(data, ''$.category'') as category, ' +
    '       COUNT(*) as count, ' +
    '       SUM(CAST(JSON_EXTRACT(data, ''$.price'') AS REAL)) as total_value, ' +
    '       AVG(CAST(JSON_EXTRACT(data, ''$.price'') AS REAL)) as avg_price ' +
    'FROM documents ' +
    'WHERE collection = ''products'' ' +
    'GROUP BY category');
  try
    WriteLn('   Category      Count  Total Value  Avg Price');
    WriteLn('   --------      -----  -----------  ---------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('category').AsString:12,
              DS.FieldByName('count').AsInteger:6,
              DS.FieldByName('total_value').AsFloat:12:2,
              DS.FieldByName('avg_price').AsFloat:10:2);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Inventory value
  DS := Connection.ExecuteQuery(
    'SELECT SUM(CAST(JSON_EXTRACT(data, ''$.price'') AS REAL) * ' +
    '           CAST(JSON_EXTRACT(data, ''$.stock'') AS INTEGER)) as inventory_value ' +
    'FROM documents WHERE collection = ''products''');
  try
    WriteLn('   Total inventory value: $', DS.FieldByName('inventory_value').AsFloat:0:2);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays per-collection statistics including document count, average document size, and creation time. }
procedure DemoCollectionStats;
var
  DS: TDataSet;
begin
  WriteLn('6. Collection statistics');
  WriteLn('   ---------------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT c.name, c.document_count, c.created_at, ' +
    '       AVG(LENGTH(d.data)) as avg_doc_size ' +
    'FROM collections c ' +
    'LEFT JOIN documents d ON c.name = d.collection ' +
    'GROUP BY c.name ORDER BY c.document_count DESC');
  try
    WriteLn('   Collection       Docs  Avg Size  Created');
    WriteLn('   ----------       ----  --------  -------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:15,
              DS.FieldByName('document_count').AsInteger:5,
              DS.FieldByName('avg_doc_size').AsFloat:9:0, ' bytes  ',
              DS.FieldByName('created_at').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates a document and updates it multiple times, then displays the current version number and data. }
procedure DemoDocumentVersioning;
var
  DocId: string;
  DS: TDataSet;
  I: Integer;
begin
  WriteLn('7. Document versioning');
  WriteLn('   -------------------');
  WriteLn('');

  DocId := InsertDocument('versioned', '{"content": "Version 1", "author": "Alice"}');
  WriteLn('   Created document with version 1');

  for I := 2 to 4 do
  begin
    UpdateDocument('versioned', DocId,
      '{"content": "Version ' + IntToStr(I) + '", "author": "Alice"}');
    WriteLn('   Updated to version ', I);
  end;

  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT id, version, data, updated_at FROM documents WHERE id = ?',
    [DocId]);
  try
    WriteLn('   Current document state:');
    WriteLn('   Version: ', DS.FieldByName('version').AsInteger);
    WriteLn('   Data: ', DS.FieldByName('data').AsString);
    WriteLn('   Updated: ', DS.FieldByName('updated_at').AsString);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the database file and its WAL/SHM journal files from disk. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  WriteLn('=== NDXSQLite Example 49: Document Store Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example49.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupDocumentStore;
      WriteLn('Document store database initialized');
      WriteLn('');

      DemoBasicOperations;
      DemoJsonQueries;
      DemoNestedDocuments;
      DemoArrayOperations;
      DemoAggregations;
      DemoCollectionStats;
      DemoDocumentVersioning;

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
