# Example 63: Custom Collations

This example demonstrates how to create custom text comparison rules using `TNDXSQLiteCollation`.

## What you'll learn

- Creating custom collation functions
- Case-insensitive sorting
- Natural sorting for strings with numbers
- Using collations in ORDER BY clauses
- Applying collations to columns and indexes

## Key concepts

### What is a collation?

A collation defines how text values are compared and sorted. SQLite includes built-in collations like `BINARY`, `NOCASE`, and `RTRIM`, but you can create custom ones for specific needs.

### Creating a custom collation

```pascal
type
  TMyCollations = class
    function CompareCaseInsensitive(const A, B: string): Integer;
  end;

function TMyCollations.CompareCaseInsensitive(const A, B: string): Integer;
begin
  Result := CompareText(A, B);  // Returns <0, 0, or >0
end;

// Registration
Coll := TNDXSQLiteCollation.Create(Connection);
Coll.RegisterCollation('MY_NOCASE', @MyColls.CompareCaseInsensitive);
```

### Return value convention

- **Negative**: A comes before B
- **Zero**: A equals B
- **Positive**: A comes after B

### Using collations in SQL

```sql
-- In ORDER BY
SELECT name FROM products ORDER BY name COLLATE MY_NOCASE;

-- In column definition
CREATE TABLE products (
  name TEXT COLLATE MY_NOCASE
);

-- In index creation
CREATE INDEX idx_name ON products(name COLLATE MY_NOCASE);

-- In WHERE comparisons
SELECT * FROM products WHERE name = 'apple' COLLATE MY_NOCASE;
```

### Natural sort example

Natural sorting handles embedded numbers intelligently:

| Default Sort | Natural Sort |
|-------------|--------------|
| file1.txt   | file1.txt    |
| file10.txt  | file2.txt    |
| file2.txt   | file3.txt    |
| file20.txt  | file10.txt   |
| file3.txt   | file20.txt   |

```pascal
function TMyCollations.CompareNatural(const A, B: string): Integer;
// Compares strings, treating embedded numbers numerically
// "file2" < "file10" (unlike lexicographic "file10" < "file2")
```

## Common collation types

| Collation | Description | Use Case |
|-----------|-------------|----------|
| Case-insensitive | Ignores case | User searches |
| Natural | Numbers in strings | File names, versions |
| Numeric | Treats as numbers | Sorting "1", "10", "2" |
| Reverse | Z to A | Descending without DESC |
| Length | By string length | Short-first display |

## Building

```bash
lazbuild CustomCollations.lpi
```

## Running

```bash
./CustomCollations      # Linux/macOS
CustomCollations.exe    # Windows
```

## Notes

- Collations are connection-specific
- Must be registered before use in tables/indexes
- Re-register after reconnection
- Performance: Keep comparison logic simple
- For full Unicode support, consider ICU integration

## Cross-Platform

This example works on Windows, Linux, and macOS.
