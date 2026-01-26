# Example 67: Virtual Tables

This example demonstrates SQLite virtual tables with NDXSQLite:

## What you'll learn

- How to create and register virtual table modules
- How to use the Series virtual table (like generate_series)
- How to use the KeyValue virtual table (in-memory store)
- How to use the CSV virtual table (file reader)
- How to create custom virtual tables
- How to iterate data using cursors

## Key concepts

### Virtual Table Module

```pascal
VTModule := TNDXVirtualTableModule.Create(Connection);
VTModule.RegisterModule('series', TNDXSeriesVirtualTable, True);
VTModule.RegisterModule('keyvalue', TNDXKeyValueVirtualTable, False);
VTModule.RegisterModule('csv', TNDXCSVVirtualTable, True);
```

### Series Virtual Table

```pascal
Series := TNDXSeriesVirtualTable.Create(nil, 'my_series');
Series.SetRange(1, 10, 1);  // Start, Stop, Step

// Iterate with cursor
Cursor := Series.CreateCursor;
Cursor.Filter(0, '', []);
while not Cursor.Eof do
begin
  WriteLn(Cursor.Column(0));
  Cursor.Next;
end;
```

### KeyValue Virtual Table

```pascal
KeyValue := TNDXKeyValueVirtualTable.Create(nil, 'settings');

// Set values
KeyValue.SetValue('app_name', 'MyApp');
KeyValue.SetValue('version', '1.0.0');

// Get values
Value := KeyValue.GetValue('app_name');

// Check existence
if KeyValue.HasKey('debug_mode') then ...

// Delete
KeyValue.DeleteKey('old_setting');
```

### CSV Virtual Table

```pascal
CSV := TNDXCSVVirtualTable.Create(nil, 'data');
CSV.SetFile('data.csv', ',', True);  // Path, Delimiter, HasHeader

// Iterate rows
Cursor := CSV.CreateCursor;
Cursor.Filter(0, '', []);
while not Cursor.Eof do
begin
  WriteLn(Cursor.Column(0), ', ', Cursor.Column(1));
  Cursor.Next;
end;
```

### Custom Virtual Table

```pascal
Custom := TNDXVirtualTable.Create(nil, 'players');

// Define columns
Custom.AddColumn('id', 'INTEGER', True, True);
Custom.AddColumn('name', 'TEXT', True);
Custom.AddColumn('score', 'REAL');

// Add rows
Custom.AddRow([1, 'Player1', 95.5]);
Custom.AddRow([2, 'Player2', 87.3]);
```

## Building

```bash
lazbuild VirtualTables.lpi
```

## Running

```bash
./VirtualTables      # Linux/macOS
VirtualTables.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 67: Virtual Tables ===

1. Creating Virtual Table Module:
   Registered modules: series, keyvalue, csv
   IsModuleRegistered(series): TRUE

2. Series Virtual Table (like generate_series):
   Created series from 1 to 10 (step 1)
   Row count: 10
   Values:
     1 2 3 4 5 6 7 8 9 10

3. KeyValue Virtual Table (in-memory store):
   Stored 5 key-value pairs
   Retrieving values:
     app_name: MyApp
     version: 1.0.0

4. CSV Virtual Table (file reader):
   Loaded CSV file
   Row count: 5
   Data:
     Alice, 30 years, from Paris
     Bob, 25 years, from London
     ...

5. Custom Virtual Table (in-memory):
   Created custom table with 4 rows
   Data:
     ID=1, Name=Player1, Score=95.5
     ...

=== Example completed successfully! ===
```

## Use cases

- **Series**: Generate test data, sequences, ranges
- **KeyValue**: Configuration storage, caching, session data
- **CSV**: Import CSV files without SQL COPY
- **Custom**: Any computed or dynamic data source

## Cross-Platform

This example works on Windows, Linux, and macOS.
