# NDXSQLite GUI Example 01 - DBGrid with Navigator

A complete CRUD (Create, Read, Update, Delete) interface using `TNDXSQLiteDataSet` with standard Lazarus data-aware controls.

## Overview

This example demonstrates the fundamental pattern for building database applications with NDXSQLite. It showcases how `TNDXSQLiteDataSet` integrates seamlessly with Lazarus visual components, providing a fully functional data entry interface with minimal code.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| TDataSet Compatibility | `TNDXSQLiteDataSet` works with all standard data-aware controls |
| DBGrid Integration | Display and inline editing of tabular data |
| DBNavigator Support | Full navigation and CRUD operations via standard buttons |
| Auto-Increment Keys | Automatic handling of `INTEGER PRIMARY KEY AUTOINCREMENT` fields |
| Database Auto-Creation | Database and tables created automatically on first run |

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      TfrmMain                           │
├─────────────────────────────────────────────────────────┤
│  ┌─────────────────┐    ┌─────────────────────────────┐ │
│  │  TDBNavigator   │    │         TDBGrid             │ │
│  │  [<][>][+][-]   │    │  ID │ Name │ Email │ City  │ │
│  └────────┬────────┘    └──────────────┬──────────────┘ │
│           │                            │                │
│           └──────────┬─────────────────┘                │
│                      ▼                                  │
│              ┌───────────────┐                          │
│              │  TDataSource  │                          │
│              └───────┬───────┘                          │
│                      ▼                                  │
│           ┌─────────────────────┐                       │
│           │ TNDXSQLiteDataSet   │                       │
│           │  TableName='...'    │                       │
│           │  PrimaryKey='id'    │                       │
│           └──────────┬──────────┘                       │
│                      ▼                                  │
│              ┌───────────────┐                          │
│              │ SQLite Database│                         │
│              │ (dbgrid_demo.db)│                        │
│              └───────────────┘                          │
└─────────────────────────────────────────────────────────┘
```

## Database Schema

```sql
CREATE TABLE customers (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT,
    phone TEXT,
    city TEXT,
    created_at TEXT DEFAULT (datetime('now', 'localtime'))
);
```

## Key Implementation Details

### Dataset Configuration for Editing

For `TDBNavigator` to perform Insert, Update, and Delete operations, you must configure:

```pascal
FDataSet.TableName := 'customers';  // Required: table name for SQL generation
FDataSet.PrimaryKey := 'id';        // Required: primary key for WHERE clauses
FDataSet.SQL.Text := 'SELECT id, name, email, phone, city, created_at
                      FROM customers ORDER BY name';
FDataSet.Open;
```

### DataSource as Mediator

The `TDataSource` component acts as a bridge between the dataset and visual controls:

```pascal
// In FormCreate
FDataSet := TNDXSQLiteDataSet.Create(Self);
DataSource1.DataSet := FDataSet;
// DBGrid1.DataSource and DBNavigator1.DataSource are set to DataSource1 in the IDE
```

### Grid Column Configuration

Columns are configured programmatically for precise control:

```pascal
with DBGrid1.Columns.Add do
begin
  FieldName := 'id';
  Title.Caption := 'ID';
  Width := 50;
  ReadOnly := True;  // Auto-increment field should not be editable
end;
```

## CRUD Operations

| Operation | User Action | What Happens |
|-----------|-------------|--------------|
| **Create** | Click `+` button | New empty row added, user enters data, Post saves to DB |
| **Read** | Open application | Data loaded from database and displayed in grid |
| **Update** | Edit cell, press Enter | Changes saved to database immediately |
| **Delete** | Click `-` button | Current record deleted from database |

## Building and Running

### Build
```bash
cd examples/gui/01_DBGrid
lazbuild DBGridDemo.lpi
```

### Run
- **Linux/macOS**: `./DBGridDemo`
- **Windows**: `DBGridDemo.exe`

The database file `dbgrid_demo.db` is created automatically in the executable directory.

## Project Files

| File | Description |
|------|-------------|
| `DBGridDemo.lpi` | Lazarus project configuration |
| `DBGridDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form implementation with all database logic |
| `MainForm.lfm` | Visual form design (IDE-generated) |

## Requirements

- **IDE**: Lazarus 2.2+ with Free Pascal 3.2+
- **Library**: NDXSQLite (parent project)
- **Platform**: Windows, Linux, or macOS

## Cross-Platform Notes

This example is fully cross-platform:

| Platform | Widgetset | Database Location |
|----------|-----------|-------------------|
| Windows | Native Win32/Win64 | Executable directory |
| Linux | GTK2 or Qt5 | Executable directory |
| macOS | Cocoa | Executable directory |

> **Production Note**: For production applications, consider using platform-specific paths:
> - Windows: `%APPDATA%\YourApp\`
> - Linux: `~/.local/share/YourApp/`
> - macOS: `~/Library/Application Support/YourApp/`

## Related Examples

- **02_MasterDetail**: Two linked datasets with automatic filtering
- **03_CachedUpdates**: Batch editing with Apply/Cancel functionality
- **04_AsyncProgress**: Non-blocking operations with progress reporting

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
