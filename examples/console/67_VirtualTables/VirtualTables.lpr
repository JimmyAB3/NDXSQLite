{===============================================================================
  NDXSQLite Example 67 - Virtual Tables
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Creating custom virtual table implementations
  - Using pre-built virtual tables (Series, CSV, KeyValue)
  - Virtual table module registration
  - Cursor-based data iteration
  - In-memory key-value store

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program VirtualTables;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Variants,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqlitevtab;

var
  Conn: INDXSQLiteConnection;
  VTModule: TNDXVirtualTableModule;
  Series: TNDXSeriesVirtualTable;
  KeyValue: TNDXKeyValueVirtualTable;
  CSV: TNDXCSVVirtualTable;
  Custom: TNDXVirtualTable;
  Cursor: TNDXVirtualCursor;
  DBPath, CSVPath: string;
  I: Integer;
  CSVFile: TextFile;

begin
  WriteLn('=== NDXSQLite Example 67: Virtual Tables ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example67.db';
  CSVPath := ExtractFilePath(ParamStr(0)) + 'test_data.csv';

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(CSVPath) then DeleteFile(CSVPath);

  Conn := TNDXSQLiteConnection.Create(DBPath);
  Conn.Open;

  // 1. Virtual Table Module
  WriteLn('1. Creating Virtual Table Module:');
  VTModule := TNDXVirtualTableModule.Create(Conn);
  try
    // Register modules
    VTModule.RegisterModule('series', TNDXSeriesVirtualTable, True);
    VTModule.RegisterModule('keyvalue', TNDXKeyValueVirtualTable, False);
    VTModule.RegisterModule('csv', TNDXCSVVirtualTable, True);

    WriteLn('   Registered modules: series, keyvalue, csv');
    WriteLn('   IsModuleRegistered(series): ', VTModule.IsModuleRegistered('series'));
    WriteLn('   IsModuleRegistered(unknown): ', VTModule.IsModuleRegistered('unknown'));
    WriteLn;

    // 2. Series Virtual Table (generate_series equivalent)
    WriteLn('2. Series Virtual Table (like generate_series):');
    Series := TNDXSeriesVirtualTable.Create(nil, 'my_series');
    try
      Series.SetRange(1, 10, 1);
      WriteLn('   Created series from 1 to 10 (step 1)');
      WriteLn('   Row count: ', Series.RowCount);
      WriteLn('   Column count: ', Series.ColumnCount);

      // Iterate using cursor
      WriteLn('   Values:');
      Cursor := Series.CreateCursor;
      try
        Cursor.Filter(0, '', []);
        Write('     ');
        while not Cursor.Eof do
        begin
          Write(Cursor.Column(0), ' ');
          Cursor.Next;
        end;
        WriteLn;
      finally
        Cursor.Free;
      end;
      WriteLn;

      // Series with different step
      WriteLn('   Series from 0 to 100 (step 10):');
      Series.SetRange(0, 100, 10);
      Cursor := Series.CreateCursor;
      try
        Cursor.Filter(0, '', []);
        Write('     ');
        while not Cursor.Eof do
        begin
          Write(Cursor.Column(0), ' ');
          Cursor.Next;
        end;
        WriteLn;
      finally
        Cursor.Free;
      end;
      WriteLn;

      // Descending series
      WriteLn('   Descending series from 5 to 1 (step -1):');
      Series.SetRange(5, 1, -1);
      Cursor := Series.CreateCursor;
      try
        Cursor.Filter(0, '', []);
        Write('     ');
        while not Cursor.Eof do
        begin
          Write(Cursor.Column(0), ' ');
          Cursor.Next;
        end;
        WriteLn;
      finally
        Cursor.Free;
      end;
      WriteLn;
    finally
      Series.Free;
    end;

    // 3. KeyValue Virtual Table
    WriteLn('3. KeyValue Virtual Table (in-memory store):');
    KeyValue := TNDXKeyValueVirtualTable.Create(nil, 'settings');
    try
      // Set some values
      KeyValue.SetValue('app_name', 'MyApp');
      KeyValue.SetValue('version', '1.0.0');
      KeyValue.SetValue('debug_mode', 'true');
      KeyValue.SetValue('max_connections', '100');
      KeyValue.SetValue('timeout', '30');

      WriteLn('   Stored 5 key-value pairs');
      WriteLn('   Row count: ', KeyValue.RowCount);
      WriteLn;

      // Retrieve values
      WriteLn('   Retrieving values:');
      WriteLn('     app_name: ', KeyValue.GetValue('app_name'));
      WriteLn('     version: ', KeyValue.GetValue('version'));
      WriteLn('     debug_mode: ', KeyValue.GetValue('debug_mode'));
      WriteLn;

      // Check key existence
      WriteLn('   HasKey(app_name): ', KeyValue.HasKey('app_name'));
      WriteLn('   HasKey(unknown): ', KeyValue.HasKey('unknown'));
      WriteLn;

      // Iterate all key-value pairs
      WriteLn('   All entries:');
      Cursor := KeyValue.CreateCursor;
      try
        Cursor.Filter(0, '', []);
        while not Cursor.Eof do
        begin
          WriteLn('     ', Cursor.Column(0), ' = ', Cursor.Column(1));
          Cursor.Next;
        end;
      finally
        Cursor.Free;
      end;
      WriteLn;

      // Update a value
      WriteLn('   Updating version to 2.0.0...');
      KeyValue.SetValue('version', '2.0.0');
      WriteLn('     New version: ', KeyValue.GetValue('version'));
      WriteLn;

      // Delete a key
      WriteLn('   Deleting debug_mode...');
      KeyValue.DeleteKey('debug_mode');
      WriteLn('     HasKey(debug_mode): ', KeyValue.HasKey('debug_mode'));
      WriteLn('     Row count: ', KeyValue.RowCount);
      WriteLn;

      // Clear all
      WriteLn('   Clearing all entries...');
      KeyValue.Clear;
      WriteLn('     Row count: ', KeyValue.RowCount);
      WriteLn;
    finally
      KeyValue.Free;
    end;

    // 4. CSV Virtual Table
    WriteLn('4. CSV Virtual Table (file reader):');

    // Create a test CSV file
    AssignFile(CSVFile, CSVPath);
    Rewrite(CSVFile);
    WriteLn(CSVFile, 'name,age,city');
    WriteLn(CSVFile, 'Alice,30,Paris');
    WriteLn(CSVFile, 'Bob,25,London');
    WriteLn(CSVFile, 'Charlie,35,Berlin');
    WriteLn(CSVFile, 'Diana,28,Madrid');
    WriteLn(CSVFile, 'Eve,32,Rome');
    CloseFile(CSVFile);
    WriteLn('   Created test CSV file with 5 records');

    CSV := TNDXCSVVirtualTable.Create(nil, 'people');
    try
      CSV.SetFile(CSVPath, ',', True);
      WriteLn('   Loaded CSV file');
      WriteLn('   Row count: ', CSV.RowCount);
      WriteLn('   Column count: ', CSV.ColumnCount);
      WriteLn;

      // Show column definitions
      WriteLn('   Columns:');
      for I := 0 to CSV.ColumnCount - 1 do
        WriteLn('     ', I, ': ', CSV.GetColumnDef(I).Name, ' (', CSV.GetColumnDef(I).DataType, ')');
      WriteLn;

      // Iterate data
      WriteLn('   Data:');
      Cursor := CSV.CreateCursor;
      try
        Cursor.Filter(0, '', []);
        while not Cursor.Eof do
        begin
          WriteLn(Format('     %s, %s years, from %s', [
            VarToStr(Cursor.Column(0)),
            VarToStr(Cursor.Column(1)),
            VarToStr(Cursor.Column(2))
          ]));
          Cursor.Next;
        end;
      finally
        Cursor.Free;
      end;
      WriteLn;
    finally
      CSV.Free;
    end;

    // 5. Custom Virtual Table (in-memory data)
    WriteLn('5. Custom Virtual Table (in-memory):');
    Custom := TNDXVirtualTable.Create(nil, 'custom_data');
    try
      // Define columns
      Custom.AddColumn('id', 'INTEGER', True, True);
      Custom.AddColumn('name', 'TEXT', True);
      Custom.AddColumn('score', 'REAL');

      // Add some rows
      Custom.AddRow([1, 'Player1', 95.5]);
      Custom.AddRow([2, 'Player2', 87.3]);
      Custom.AddRow([3, 'Player3', 92.1]);
      Custom.AddRow([4, 'Player4', 78.9]);

      WriteLn('   Created custom table with 4 rows');
      WriteLn('   Create statement: ', Custom.GetCreateStatement);
      WriteLn('   Row count: ', Custom.RowCount);
      WriteLn;

      // Iterate
      WriteLn('   Data:');
      Cursor := Custom.CreateCursor;
      try
        Cursor.Filter(0, '', []);
        while not Cursor.Eof do
        begin
          WriteLn(Format('     ID=%s, Name=%s, Score=%s', [
            VarToStr(Cursor.Column(0)),
            VarToStr(Cursor.Column(1)),
            VarToStr(Cursor.Column(2))
          ]));
          Cursor.Next;
        end;
      finally
        Cursor.Free;
      end;
      WriteLn;
    finally
      Custom.Free;
    end;

    // 6. Module management
    WriteLn('6. Module Management:');
    WriteLn('   Unregistering csv module...');
    VTModule.UnregisterModule('csv');
    WriteLn('   IsModuleRegistered(csv): ', VTModule.IsModuleRegistered('csv'));
    WriteLn;

  finally
    VTModule.Free;
  end;

  // 7. Best practices
  WriteLn('7. Virtual Table Best Practices:');
  WriteLn('   - Use virtual tables for computed/dynamic data');
  WriteLn('   - Series: great for generating test data');
  WriteLn('   - KeyValue: fast in-memory configuration');
  WriteLn('   - CSV: easy file import without SQL');
  WriteLn('   - Cursors: memory-efficient iteration');
  WriteLn('   - Custom tables: extend TNDXVirtualTable');
  WriteLn;

  Conn.Close;
  Conn := nil;

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(CSVPath) then DeleteFile(CSVPath);

  WriteLn('=== Example completed successfully! ===');
end.
