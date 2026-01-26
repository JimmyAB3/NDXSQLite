{===============================================================================
  NDXSQLite Example 20 - Import/Export CSV, JSON, SQL
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates the data export/import units:
  - TNDXSQLiteCSV for CSV export/import
  - TNDXSQLiteJSON for JSON export
  - TNDXSQLiteDump for SQL dump export

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ImportExportCSV;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Math,
  ndxsqliteconnection, ndxsqliteconnectionoptions,
  ndxsqlitecsv, ndxsqlitejson, ndxsqlitedump;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  department TEXT,' +
    '  salary REAL,' +
    '  hire_date TEXT' +
    ')');

  Connection.ExecuteNonQuery('INSERT INTO employees (name, department, salary, hire_date) VALUES (''Alice Smith'', ''Engineering'', 75000, ''2020-01-15'')');
  Connection.ExecuteNonQuery('INSERT INTO employees (name, department, salary, hire_date) VALUES (''Bob Johnson'', ''Sales'', 65000, ''2019-06-20'')');
  Connection.ExecuteNonQuery('INSERT INTO employees (name, department, salary, hire_date) VALUES (''Charlie Brown'', ''Engineering'', 80000, ''2018-03-10'')');
  Connection.ExecuteNonQuery('INSERT INTO employees (name, department, salary, hire_date) VALUES (''Diana Ross'', ''HR'', 60000, ''2021-09-01'')');
  // Employee with special characters (tests CSV escaping)
  Connection.ExecuteNonQuery('INSERT INTO employees (name, department, salary, hire_date) VALUES (''Eve "The Great" Wilson'', ''Marketing'', 70000, ''2022-01-15'')');
end;

{ Exports the employees table to a CSV file, displays export statistics and file content, then deletes the file. }
procedure DemoCSVExport;
var
  CSV: TNDXSQLiteCSV;
  ExportResult: TNDXCSVExportResult;
  CSVPath: string;
  Lines: TStringList;
  I: Integer;
begin
  WriteLn('1. Export to CSV using TNDXSQLiteCSV');
  WriteLn('   ----------------------------------');

  CSVPath := ExtractFilePath(ParamStr(0)) + 'employees.csv';

  CSV := TNDXSQLiteCSV.Create(Connection);
  try
    ExportResult := CSV.ExportTableToCSV('employees', CSVPath);

    if ExportResult.Success then
    begin
      WriteLn('   Export successful!');
      WriteLn('   File: ', ExportResult.FilePath);
      WriteLn('   Rows: ', ExportResult.RowCount);
      WriteLn('   Columns: ', ExportResult.ColumnCount);
      WriteLn('   Size: ', ExportResult.FileSize, ' bytes');
      WriteLn('');

      // Show content
      WriteLn('   CSV content:');
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(CSVPath);
        for I := 0 to Lines.Count - 1 do
          WriteLn('   ', Lines[I]);
      finally
        Lines.Free;
      end;
    end
    else
      WriteLn('   Export failed: ', ExportResult.ErrorMessage);
  finally
    CSV.Free;
  end;

  // Cleanup
  if FileExists(CSVPath) then
    DeleteFile(CSVPath);

  WriteLn('');
end;

{ Creates a sample CSV file, imports it into the imported_employees table, and displays the imported rows. }
procedure DemoCSVImport;
var
  CSV: TNDXSQLiteCSV;
  ImportResult: TNDXCSVImportResult;
  CSVPath: string;
  CSVFile: TextFile;
  DS: TDataSet;
begin
  WriteLn('2. Import from CSV using TNDXSQLiteCSV');
  WriteLn('   -----------------------------------');

  // Create sample CSV to import
  CSVPath := ExtractFilePath(ParamStr(0)) + 'import_data.csv';
  AssignFile(CSVFile, CSVPath);
  Rewrite(CSVFile);
  try
    WriteLn(CSVFile, 'name,department,salary,hire_date');
    WriteLn(CSVFile, 'Frank Miller,Engineering,72000,2023-01-10');
    WriteLn(CSVFile, 'Grace Lee,Sales,58000,2023-02-15');
    WriteLn(CSVFile, '"Henry, Jr. O''Brien",HR,55000,2023-03-20');
  finally
    CloseFile(CSVFile);
  end;

  WriteLn('   Created sample CSV with 3 rows');

  // Create import table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS imported_employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  department TEXT,' +
    '  salary REAL,' +
    '  hire_date TEXT' +
    ')');

  CSV := TNDXSQLiteCSV.Create(Connection);
  try
    ImportResult := CSV.ImportFromCSV(CSVPath, 'imported_employees');
    try
      if ImportResult.Success then
      begin
        WriteLn('   Import successful!');
        WriteLn('   Rows imported: ', ImportResult.RowsImported);
        WriteLn('   Rows skipped: ', ImportResult.RowsSkipped);
        WriteLn('   Errors: ', ImportResult.ErrorCount);
        WriteLn('');

        // Verify import
        WriteLn('   Imported data:');
        DS := Connection.ExecuteQuery('SELECT * FROM imported_employees');
        try
          while not DS.EOF do
          begin
            WriteLn(Format('   - %s, %s, $%.0f',
              [DS.FieldByName('name').AsString,
               DS.FieldByName('department').AsString,
               DS.FieldByName('salary').AsFloat]));
            DS.Next;
          end;
        finally
          DS.Free;
        end;
      end
      else
        WriteLn('   Import failed: ', ImportResult.ErrorMessage);
    finally
      ImportResult.Errors.Free;
    end;
  finally
    CSV.Free;
  end;

  // Cleanup
  if FileExists(CSVPath) then
    DeleteFile(CSVPath);

  WriteLn('');
end;

{ Exports the entire database to a SQL dump file, displays statistics and the first lines of the dump. }
procedure DemoSQLDump;
var
  Dump: TNDXSQLiteDump;
  DumpResult: TNDXDumpResult;
  DumpPath: string;
  Lines: TStringList;
  I: Integer;
begin
  WriteLn('3. Export SQL Dump using TNDXSQLiteDump');
  WriteLn('   ------------------------------------');

  DumpPath := ExtractFilePath(ParamStr(0)) + 'dump.sql';

  Dump := TNDXSQLiteDump.Create(Connection);
  try
    DumpResult := Dump.ExportToSQL(DumpPath);

    if DumpResult.Success then
    begin
      WriteLn('   Export successful!');
      WriteLn('   File: ', DumpResult.FilePath);
      WriteLn('   Size: ', DumpResult.FileSize, ' bytes');
      WriteLn('   Tables: ', DumpResult.TableCount);
      WriteLn('   Indexes: ', DumpResult.IndexCount);
      WriteLn('   Views: ', DumpResult.ViewCount);
      WriteLn('');

      // Show first lines
      WriteLn('   First lines of dump:');
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(DumpPath);
        for I := 0 to Min(12, Lines.Count - 1) do
          WriteLn('   ', Lines[I]);
        if Lines.Count > 12 then
          WriteLn('   ... (', Lines.Count - 12, ' more lines)');
      finally
        Lines.Free;
      end;
    end
    else
      WriteLn('   Export failed: ', DumpResult.ErrorMessage);
  finally
    Dump.Free;
  end;

  // Cleanup
  if FileExists(DumpPath) then
    DeleteFile(DumpPath);

  WriteLn('');
end;

{ Exports the employees table to a JSON file with pretty-printing and displays the first 20 lines. }
procedure DemoJSONExport;
var
  JSON: TNDXSQLiteJSON;
  JSONPath: string;
  Lines: TStringList;
  I: Integer;
begin
  WriteLn('4. Export to JSON using TNDXSQLiteJSON');
  WriteLn('   -----------------------------------');

  JSONPath := ExtractFilePath(ParamStr(0)) + 'employees.json';

  JSON := TNDXSQLiteJSON.Create(Connection);
  try
    if JSON.ExportTableToFile('employees', JSONPath, '', True) then
    begin
      WriteLn('   Export successful!');
      WriteLn('   File: ', JSONPath);
      WriteLn('');

      // Show content
      WriteLn('   JSON content (first 20 lines):');
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(JSONPath);
        for I := 0 to Min(19, Lines.Count - 1) do
          WriteLn('   ', Lines[I]);
        if Lines.Count > 20 then
          WriteLn('   ... (', Lines.Count - 20, ' more lines)');
      finally
        Lines.Free;
      end;
    end
    else
      WriteLn('   Export failed');
  finally
    JSON.Free;
  end;

  // Cleanup
  if FileExists(JSONPath) then
    DeleteFile(JSONPath);

  WriteLn('');
end;

{ Tests CSV field escaping with special characters and parses a comma-separated line into individual fields. }
procedure DemoCSVUtilities;
var
  TestLine: string;
  Fields: TStringList;
  I: Integer;
begin
  WriteLn('5. CSV Utility Functions');
  WriteLn('   ---------------------');

  // Test escaping
  WriteLn('   EscapeField examples:');
  WriteLn('     "Hello" -> ', TNDXSQLiteCSV.EscapeField('Hello'));
  WriteLn('     "Hello, World" -> ', TNDXSQLiteCSV.EscapeField('Hello, World'));
  WriteLn('     ''Say "Hi"'' -> ', TNDXSQLiteCSV.EscapeField('Say "Hi"'));
  WriteLn('');

  // Test parsing
  WriteLn('   ParseLine example:');
  TestLine := 'John,"Doe, Jr.",35,"New York"';
  WriteLn('     Input: ', TestLine);
  Fields := TNDXSQLiteCSV.ParseLine(TestLine);
  try
    WriteLn('     Parsed fields:');
    for I := 0 to Fields.Count - 1 do
      WriteLn('       [', I, ']: ', Fields[I]);
  finally
    Fields.Free;
  end;
  WriteLn('');
end;

{ Deletes the example database file if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 20: Import/Export ===');
  WriteLn('    Using TNDXSQLiteCSV, TNDXSQLiteJSON, TNDXSQLiteDump');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example20.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      DemoCSVExport;
      DemoCSVImport;
      DemoSQLDump;
      DemoJSONExport;
      DemoCSVUtilities;

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
