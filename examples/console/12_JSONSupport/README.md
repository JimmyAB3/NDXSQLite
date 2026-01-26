# Example 12: JSON Support

This example demonstrates SQLite's JSON functions (SQLite 3.38+).

## What you'll learn

- JSON validation and type checking
- Extracting values with JSON path syntax
- Manipulating JSON data
- Storing JSON in tables
- Querying JSON columns

## Prerequisites

Requires SQLite 3.38 or newer for native JSON functions.

## Key concepts

### JSON validation

```pascal
JSON := TNDXSQLiteJSON.Create(Connection);
if JSON.IsValidJSON('{"key": "value"}') then
  WriteLn('Valid JSON');
```

### JSON extraction

```pascal
// Extract value by path
Value := JSON.JSONExtract('{"name": "Alice"}', '$.name');

// Extract nested values
CPU := JSON.JSONExtract(metadata, '$.specs.cpu');

// Extract array element
First := JSON.JSONExtract(data, '$.items[0]');
```

### JSON manipulation

```pascal
// Set value
NewJSON := JSON.JSONSet(original, '$.age', 31);

// Insert new key
NewJSON := JSON.JSONInsert(original, '$.country', 'France');

// Remove key
NewJSON := JSON.JSONRemove(original, ['$.temp']);
```

### SQL queries with JSON

```sql
-- Extract JSON values in queries
SELECT name, json_extract(metadata, '$.brand') as brand
FROM products;

-- Filter by JSON values
SELECT * FROM products
WHERE json_extract(metadata, '$.ram') >= 8;

-- Aggregate to JSON array
SELECT json_group_array(name) FROM products;
```

## Building

```bash
lazbuild JSONSupport.lpi
```

## Running

```bash
./JSONSupport      # Linux/macOS
JSONSupport.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
