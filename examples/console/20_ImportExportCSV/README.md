# Example 20: Import/Export CSV, JSON, SQL

This example demonstrates importing and exporting data using the NDXSQLite library units.

## What you'll learn

- Export tables to CSV using TNDXSQLiteCSV
- Import CSV data into tables
- Handle special characters (quotes, commas)
- Export SQL dump using TNDXSQLiteDump
- Export to JSON using TNDXSQLiteJSON
- CSV utility functions (EscapeField, ParseLine)

## Key concepts

### Export to CSV using TNDXSQLiteCSV

```pascal
uses ndxsqlitecsv;

var
  CSV: TNDXSQLiteCSV;
  ExportResult: TNDXCSVExportResult;
begin
  CSV := TNDXSQLiteCSV.Create(Connection);
  try
    ExportResult := CSV.ExportTableToCSV('employees', 'employees.csv');

    if ExportResult.Success then
    begin
      WriteLn('Rows: ', ExportResult.RowCount);
      WriteLn('Columns: ', ExportResult.ColumnCount);
      WriteLn('Size: ', ExportResult.FileSize, ' bytes');
    end;
  finally
    CSV.Free;
  end;
end;
```

### Import from CSV using TNDXSQLiteCSV

```pascal
var
  CSV: TNDXSQLiteCSV;
  ImportResult: TNDXCSVImportResult;
begin
  CSV := TNDXSQLiteCSV.Create(Connection);
  try
    ImportResult := CSV.ImportFromCSV('data.csv', 'target_table');
    try
      if ImportResult.Success then
      begin
        WriteLn('Rows imported: ', ImportResult.RowsImported);
        WriteLn('Rows skipped: ', ImportResult.RowsSkipped);
      end;
    finally
      ImportResult.Errors.Free;
    end;
  finally
    CSV.Free;
  end;
end;
```

### CSV utility functions

```pascal
// Escape a field for CSV output
WriteLn(TNDXSQLiteCSV.EscapeField('Hello'));         // Hello
WriteLn(TNDXSQLiteCSV.EscapeField('Hello, World'));  // "Hello, World"
WriteLn(TNDXSQLiteCSV.EscapeField('Say "Hi"'));      // "Say ""Hi"""

// Parse a CSV line into fields
var
  Fields: TStringList;
begin
  Fields := TNDXSQLiteCSV.ParseLine('John,"Doe, Jr.",35,"New York"');
  try
    // Fields[0] = 'John'
    // Fields[1] = 'Doe, Jr.'
    // Fields[2] = '35'
    // Fields[3] = 'New York'
  finally
    Fields.Free;
  end;
end;
```

### Export SQL dump using TNDXSQLiteDump

```pascal
uses ndxsqlitedump;

var
  Dump: TNDXSQLiteDump;
  DumpResult: TNDXDumpResult;
begin
  Dump := TNDXSQLiteDump.Create(Connection);
  try
    DumpResult := Dump.ExportToSQL('dump.sql');

    if DumpResult.Success then
    begin
      WriteLn('Tables: ', DumpResult.TableCount);
      WriteLn('Indexes: ', DumpResult.IndexCount);
      WriteLn('Views: ', DumpResult.ViewCount);
    end;
  finally
    Dump.Free;
  end;
end;
```

### Export to JSON using TNDXSQLiteJSON

```pascal
uses ndxsqlitejson;

var
  JSON: TNDXSQLiteJSON;
begin
  JSON := TNDXSQLiteJSON.Create(Connection);
  try
    // Export table to JSON file (with pretty-print)
    if JSON.ExportTableToFile('employees', 'employees.json', '', True) then
      WriteLn('Export successful!');
  finally
    JSON.Free;
  end;
end;
```

## Export formats

| Format | Class | Use Case |
|--------|-------|----------|
| CSV | TNDXSQLiteCSV | Spreadsheets, data exchange |
| SQL Dump | TNDXSQLiteDump | Backup, migration |
| JSON | TNDXSQLiteJSON | Web APIs, modern apps |

## Building

```bash
lazbuild ImportExportCSV.lpi
```

## Running

```bash
./ImportExportCSV      # Linux/macOS
ImportExportCSV.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
