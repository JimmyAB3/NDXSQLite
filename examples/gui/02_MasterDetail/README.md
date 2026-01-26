# NDXSQLite GUI Example 02 - Master/Detail Relationship

Demonstrates linked datasets where selecting a master record automatically filters the detail records.

## Overview

This example implements the classic master/detail (parent/child) pattern commonly used in database applications. Selecting a category in the master grid automatically displays only the products belonging to that category in the detail grid.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| Shared Database Connection | Multiple datasets using the same `TNDXSQLiteDatabase` instance |
| Parameterized Queries | Detail dataset filtered via `:param` syntax |
| Foreign Key Relationships | `ON DELETE CASCADE` for referential integrity |
| Automatic Detail Refresh | `OnDataChange` event triggers detail reload |
| OnNewRecord Event | Automatic foreign key assignment for new detail records |

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                           TfrmMain                                  │
├─────────────────────────────────────────────────────────────────────┤
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ Master Grid (Categories)                                     │   │
│  │ ┌────┬────────────────┬──────────────────────────────────┐   │   │
│  │ │ ID │ Category Name  │ Description                      │   │   │
│  │ ├────┼────────────────┼──────────────────────────────────┤   │   │
│  │ │ 1  │ Electronics    │ Electronic devices and...        │◄──┼───┼── User selects
│  │ │ 2  │ Books          │ Physical and digital books       │   │   │
│  │ └────┴────────────────┴──────────────────────────────────┘   │   │
│  └──────────────────────────────────────────────────────────────┘   │
│                                │                                    │
│                                ▼ OnDataChange                       │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ Detail Grid (Products WHERE category_id = 1)                 │   │
│  │ ┌────┬────────────────────┬─────────┬───────┐                │   │
│  │ │ ID │ Product Name       │ Price   │ Stock │                │   │
│  │ ├────┼────────────────────┼─────────┼───────┤                │   │
│  │ │ 1  │ Smartphone X12     │ 699.99  │ 50    │◄───────────────┼───┼── Filtered
│  │ │ 2  │ Wireless Headphones│ 149.99  │ 120   │                │   │
│  │ │ 3  │ USB-C Cable        │ 12.99   │ 500   │                │   │
│  │ └────┴────────────────────┴─────────┴───────┘                │   │
│  └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

## Database Schema

```sql
-- Master table
CREATE TABLE categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    description TEXT
);

-- Detail table with foreign key
CREATE TABLE products (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    category_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    price REAL DEFAULT 0.0,
    stock INTEGER DEFAULT 0,
    FOREIGN KEY (category_id) REFERENCES categories(id) ON DELETE CASCADE
);
```

## Key Implementation Details

### Shared Database Connection

Both datasets share the same database connection for consistency:

```pascal
FDatabase := TNDXSQLiteDatabase.Create;
FDatabase.Open(DBPath);
FDatabase.Execute('PRAGMA foreign_keys = ON');  // Enable FK enforcement

FMasterDataSet.Database := FDatabase;
FDetailDataSet.Database := FDatabase;
```

### Parameterized Detail Query

The detail dataset uses a parameter to filter by the selected category:

```pascal
FDetailDataSet.SQL.Text :=
  'SELECT id, category_id, name, price, stock FROM products ' +
  'WHERE category_id = :cat_id ORDER BY name';
FDetailDataSet.Params.ParamByName('cat_id').AsInteger := CategoryId;
FDetailDataSet.Open;
```

### Automatic Detail Refresh

The `OnDataChange` event of the master DataSource triggers a detail refresh:

```pascal
procedure TfrmMain.DataSourceMasterDataChange(Sender: TObject; Field: TField);
begin
  // Field = nil means record navigation (not field edit)
  if (Field = nil) and Assigned(FDatabase) and FDatabase.IsOpen then
    RefreshDetailDataSet;
end;
```

### Automatic Foreign Key Assignment

When inserting a new product, the `OnNewRecord` event sets the foreign key:

```pascal
procedure TfrmMain.DetailDataSetNewRecord(DataSet: TDataSet);
begin
  if FMasterDataSet.Active and not FMasterDataSet.IsEmpty then
    DataSet.FieldByName('category_id').AsInteger :=
      FMasterDataSet.FieldByName('id').AsInteger;
end;
```

## Common Use Cases

| Domain | Master | Detail |
|--------|--------|--------|
| E-Commerce | Categories | Products |
| Order Management | Orders | Order Items |
| Project Management | Projects | Tasks |
| Document Management | Folders | Documents |
| HR Systems | Departments | Employees |

## Building and Running

### Build
```bash
cd examples/gui/02_MasterDetail
lazbuild MasterDetailDemo.lpi
```

### Run
- **Linux/macOS**: `./MasterDetailDemo`
- **Windows**: `MasterDetailDemo.exe`

The database file `masterdetail_demo.db` is created automatically with sample data.

## Project Files

| File | Description |
|------|-------------|
| `MasterDetailDemo.lpi` | Lazarus project configuration |
| `MasterDetailDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form with master/detail logic |
| `MainForm.lfm` | Visual form design |

## Testing the Relationship

1. **Navigate Master**: Click on different categories
   - Detail grid updates automatically
   - Product count shown in detail panel header

2. **Add Product**: Click `+` in detail navigator
   - New product automatically assigned to current category
   - `category_id` is pre-filled via `OnNewRecord`

3. **Delete Category**: Delete a category with products
   - `ON DELETE CASCADE` automatically removes related products

## Requirements

- **IDE**: Lazarus 2.2+ with Free Pascal 3.2+
- **Library**: NDXSQLite (parent project)
- **Platform**: Windows, Linux, or macOS

## Cross-Platform Notes

This example works identically on all platforms. Foreign key constraints require SQLite 3.6.19+ which is included in NDXSQLite.

## Related Examples

- **01_DBGrid**: Basic single-table CRUD operations
- **03_CachedUpdates**: Batch editing with Apply/Cancel
- **04_AsyncProgress**: Non-blocking database operations

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
