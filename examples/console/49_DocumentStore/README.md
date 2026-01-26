# Example 49: Document Store Database

This example demonstrates how to implement a NoSQL-style document store using SQLite's JSON capabilities with NDXSQLite.

## Features Demonstrated

- **Schema-less Collections**: Store any JSON structure
- **Document CRUD**: Create, read, update, delete operations
- **JSON Queries**: Query by JSON field values
- **Nested Documents**: Access deeply nested fields
- **Array Operations**: Query and expand JSON arrays
- **Document Aggregations**: Aggregate across documents
- **Versioning**: Track document changes

## Database Schema

### Tables

1. **collections**: Collection metadata
   - Name and creation time
   - Document count (auto-updated)
   - Optional JSON schema

2. **documents**: Document storage
   - UUID-based document IDs
   - Collection association
   - JSON data payload
   - Version tracking
   - Timestamps

## Key Operations

### Insert Document
```pascal
DocId := InsertDocument('users',
  '{"name": "Alice", "email": "alice@example.com", "age": 30}');
```

### Get Document
```pascal
JsonData := GetDocument('users', DocId);
```

### Update Document
```pascal
UpdateDocument('users', DocId,
  '{"name": "Alice", "email": "new@example.com", "age": 31}');
```

### Query by JSON Field
```sql
SELECT JSON_EXTRACT(data, '$.name') as name
FROM documents
WHERE collection = 'products'
  AND JSON_EXTRACT(data, '$.category') = 'electronics'
```

### Query Nested Fields
```sql
SELECT
  JSON_EXTRACT(data, '$.customer.name') as customer,
  JSON_EXTRACT(data, '$.customer.address.city') as city
FROM documents
```

### Array Queries
```sql
-- Find documents with specific tag
SELECT * FROM documents
WHERE EXISTS (
  SELECT 1 FROM JSON_EACH(JSON_EXTRACT(data, '$.tags'))
  WHERE value = 'computer'
)

-- Expand array items
SELECT JSON_EXTRACT(value, '$.product') as product
FROM documents, JSON_EACH(JSON_EXTRACT(data, '$.items'))
```

### Aggregations
```sql
SELECT JSON_EXTRACT(data, '$.category') as category,
       COUNT(*) as count,
       AVG(CAST(JSON_EXTRACT(data, '$.price') AS REAL)) as avg_price
FROM documents
WHERE collection = 'products'
GROUP BY category
```

## JSON Functions Used

- `JSON_EXTRACT(json, path)`: Extract value at path
- `JSON_ARRAY_LENGTH(json)`: Get array length
- `JSON_EACH(json)`: Expand array/object to rows
- `JSON_TYPE(json)`: Get JSON value type

## Document Versioning

Each update increments the version number:
```pascal
// Version 1
DocId := InsertDocument('docs', '{"content": "v1"}');

// Version 2
UpdateDocument('docs', DocId, '{"content": "v2"}');
```

## Collection Statistics

Automatic document counting via triggers:
```sql
SELECT name, document_count,
       AVG(LENGTH(data)) as avg_doc_size
FROM collections
```

## Build and Run

```bash
cd 49_DocumentStore
fpc DocumentStore.lpr
./DocumentStore
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Use Cases

- Configuration storage
- User profiles with varying fields
- Product catalogs
- Order management
- Content management systems
- Logging and event storage
- Flexible data models
