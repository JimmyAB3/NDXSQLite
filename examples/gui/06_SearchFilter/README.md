# NDXSQLite GUI Example 06 - Search, Filter, and Sort

A comprehensive demonstration of data searching, filtering, and sorting techniques with `TNDXSQLiteDataSet`.

## Overview

This example showcases various methods for finding, filtering, and organizing data in a dataset. It covers both client-side operations (Locate, Filter) and server-side operations (SQL ORDER BY), helping developers choose the right approach for their specific needs.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| Locate | Find and position on matching records |
| Locate Next | Continue searching for additional matches |
| Filter Property | Display only records matching criteria |
| Dynamic Sorting | Change sort order via SQL ORDER BY |
| Column Click Sort | Sort by clicking column headers |
| Partial Matching | Find records containing search text |
| Case-Insensitive Search | Ignore case when searching |

## Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                         TfrmMain                                    │
├──────────────────────────┬─────────────────────────────────────────┤
│  Search Controls         │              Data View                   │
│  ┌────────────────────┐  │  ┌─────────────────────────────────────┐ │
│  │ Field: [name    ▼] │  │  │           TDBGrid                   │ │
│  │ Value: [laptop___] │  │  │  ID │ Name      │ Category │ Price │ │
│  │ [x] Partial match  │  │  │  1  │ Laptop Pro│ Electron.│ 1299  │ │
│  │ [x] Case insensitive│  │  │  2  │ Laptop Ba.│ Electron.│  599  │ │
│  │ [Find] [Find Next] │  │  │  ...                                │ │
│  └────────────────────┘  │  └─────────────────────────────────────┘ │
│  Filter Controls         │                                         │
│  ┌────────────────────┐  │  Click column headers to sort!          │
│  │ Field: [category▼] │  │                                         │
│  │ Op: [Contains   ▼] │  │  ┌─────────────────────────────────────┐ │
│  │ Value: [Audio____] │  │  │         Activity Log                │ │
│  │ [Apply] [Clear]    │  │  │ LOCATE: Found "laptop" at record 1  │ │
│  └────────────────────┘  │  │ FILTER: Applied "category='Audio'"  │ │
│  Sort Controls           │  │ SORT: price DESC                    │ │
│  ┌────────────────────┐  │  └─────────────────────────────────────┘ │
│  │ Field: [price   ▼] │  │                                         │
│  │ Dir: [Ascending ▼] │  │                                         │
│  └────────────────────┘  │                                         │
└──────────────────────────┴─────────────────────────────────────────┘
```

## Search Methods Comparison

| Method | Scope | Performance | Use Case |
|--------|-------|-------------|----------|
| **Locate** | Client-side | O(n) scan | Find specific record in small dataset |
| **Filter** | Client-side | O(n) scan | Show subset of loaded records |
| **SQL WHERE** | Server-side | Uses indexes | Large datasets, initial load |
| **SQL ORDER BY** | Server-side | Uses indexes | Efficient sorting of large datasets |

### When to Use Each Method

```
Small Dataset (<1000 records):
  - Locate/Filter work well
  - Quick, no round-trip to database

Large Dataset (>1000 records):
  - Use SQL WHERE for filtering
  - Use SQL ORDER BY for sorting
  - Create indexes on searched columns
```

## Database Schema

```sql
CREATE TABLE products (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    category TEXT,
    price REAL,
    stock INTEGER,
    supplier TEXT,
    created_at TEXT DEFAULT (datetime('now', 'localtime'))
);

-- Indexes for better search performance
CREATE INDEX idx_products_name ON products(name);
CREATE INDEX idx_products_category ON products(category);
```

## Key Implementation Details

### Using Locate

```pascal
procedure TfrmMain.btnLocateClick(Sender: TObject);
var
  Options: TLocateOptions;
begin
  Options := [];
  if chkPartialMatch.Checked then
    Include(Options, loPartialKey);
  if chkCaseInsensitive.Checked then
    Include(Options, loCaseInsensitive);

  if FDataSet.Locate('name', 'laptop', Options) then
    ShowMessage('Found at record ' + IntToStr(FDataSet.RecNo))
  else
    ShowMessage('Not found');
end;
```

### Using Filter Property

```pascal
procedure TfrmMain.btnApplyFilterClick(Sender: TObject);
begin
  // Filter uses SQL-like syntax with wildcards
  FDataSet.Filter := 'category = ''Electronics''';
  FDataSet.Filtered := True;

  // Other filter examples:
  // FDataSet.Filter := 'name LIKE ''*laptop*''';     // Contains
  // FDataSet.Filter := 'price > 100';                // Numeric comparison
  // FDataSet.Filter := 'stock >= 10 AND stock <= 50'; // Range
end;
```

### Dynamic SQL Sorting

```pascal
procedure TfrmMain.ApplySorting;
var
  SQL: string;
begin
  SQL := 'SELECT * FROM products';

  if cboSortField.ItemIndex > 0 then
  begin
    SQL := SQL + ' ORDER BY ' + cboSortField.Text;
    if cboSortDirection.ItemIndex = 0 then
      SQL := SQL + ' ASC'
    else
      SQL := SQL + ' DESC';
  end;

  FDataSet.Close;
  FDataSet.SQL.Text := SQL;
  FDataSet.Open;
end;
```

### Column Header Click Sorting

```pascal
procedure TfrmMain.DBGrid1TitleClick(Column: TColumn);
begin
  if FSortColumn = Column.FieldName then
    // Toggle direction
    FSortDirection := not FSortDirection
  else
  begin
    // New column, ascending
    FSortColumn := Column.FieldName;
    FSortDirection := sdAscending;
  end;

  ApplySorting;
end;
```

## Filter Operators

| Operator | Example | Description |
|----------|---------|-------------|
| `=` | `category = 'Audio'` | Exact match |
| `LIKE '*x*'` | `name LIKE '*laptop*'` | Contains |
| `LIKE 'x*'` | `name LIKE 'Pro*'` | Starts with |
| `LIKE '*x'` | `name LIKE '*HD'` | Ends with |
| `>`, `<`, `>=`, `<=` | `price > 100` | Numeric comparison |
| `AND`, `OR` | `price > 50 AND stock > 0` | Compound conditions |

## Usage Scenarios

### Scenario 1: Quick Search

1. Select **name** in search field
2. Type **laptop** in search value
3. Check **Partial match** and **Case insensitive**
4. Click **Find** to locate first match
5. Click **Find Next** for additional matches

### Scenario 2: Filter by Category

1. Select **category** in filter field
2. Select **Equals** operator
3. Type **Electronics** in filter value
4. Click **Apply Filter**
5. Grid shows only electronics products

### Scenario 3: Sort by Price

1. Click the **Price** column header
2. Click again to toggle ascending/descending
3. Or use the Sort controls for precise selection

## Building and Running

### Build
```bash
cd examples/gui/06_SearchFilter
lazbuild SearchFilterDemo.lpi
```

### Run
- **Linux/macOS**: `./SearchFilterDemo`
- **Windows**: `SearchFilterDemo.exe`

## Project Files

| File | Description |
|------|-------------|
| `SearchFilterDemo.lpi` | Lazarus project configuration |
| `SearchFilterDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form with search/filter/sort logic |
| `MainForm.lfm` | Visual form design |

## Performance Tips

1. **Create indexes** on columns you frequently search or sort
2. **Use SQL WHERE** for initial data loading with large tables
3. **Use Filter property** for temporary client-side subsetting
4. **Avoid Locate** on very large datasets - use SQL instead
5. **Limit result sets** with SQL LIMIT when possible

## Related Examples

- **01_DBGrid**: Basic grid operations
- **05_Transactions**: Data modification with transactions
- **07_MultiThread**: Searching in background threads

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
