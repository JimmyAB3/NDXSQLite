{===============================================================================
  NDXSQLite Example 04 - Data Types
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Working with SQLite data types (TEXT, INTEGER, REAL, BLOB, NULL)
  - Date/Time handling
  - Boolean values
  - BLOB storage and retrieval
  - NULL handling

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataTypes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

var
  Connection: TNDXSQLiteConnection;
  DBPath: string;
  DataSet: TDataSet;
  BlobData: TBytes;
  BlobStream: TStream;
  I: Integer;

begin
  WriteLn('=== NDXSQLite Example 04: Data Types ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example04.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Connection := TNDXSQLiteConnection.Create(DBPath);
  try
    Connection.Open;

    // Create table with various data types
    WriteLn('1. Creating table with various data types...');
    Connection.ExecuteNonQuery(
      'CREATE TABLE data_types_demo (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  text_col TEXT,' +
      '  int_col INTEGER,' +
      '  real_col REAL,' +
      '  blob_col BLOB,' +
      '  nullable_col TEXT,' +
      '  bool_col INTEGER,' +          // SQLite has no native BOOLEAN
      '  date_col TEXT,' +             // Store as ISO 8601
      '  datetime_col TEXT' +
      ')'
    );
    WriteLn('   Table created.');
    WriteLn;

    // Insert various data types
    WriteLn('2. Inserting data with different types...');

    // Create sample BLOB data
    SetLength(BlobData, 16);
    for I := 0 to 15 do
      BlobData[I] := I * 16;

    // Insert with all types
    Connection.ExecuteNonQuery(
      'INSERT INTO data_types_demo ' +
      '(text_col, int_col, real_col, blob_col, nullable_col, bool_col, date_col, datetime_col) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
      ['Hello World',                           // TEXT
       42,                                       // INTEGER
       3.14159265359,                            // REAL
       BlobData,                                 // BLOB (as TBytes variant)
       'Not null',                               // TEXT (will be null in next row)
       1,                                        // BOOLEAN as INTEGER (True)
       FormatDateTime('yyyy-mm-dd', Date),       // DATE as TEXT
       FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) // DATETIME as TEXT
      ]);

    // Insert with NULL values
    Connection.ExecuteNonQuery(
      'INSERT INTO data_types_demo ' +
      '(text_col, int_col, real_col, nullable_col, bool_col) ' +
      'VALUES (?, ?, ?, ?, ?)',
      ['Second row', 100, 2.718, Null, 0]); // NULL and False

    // Insert with empty string vs NULL
    Connection.ExecuteNonQuery(
      'INSERT INTO data_types_demo (text_col, nullable_col) VALUES (?, ?)',
      ['Empty vs NULL demo', '']);  // Empty string, not NULL

    WriteLn('   3 rows inserted.');
    WriteLn;

    // Read and display all data
    WriteLn('3. Reading data types:');
    WriteLn('   --------------------------------------------------------');

    DataSet := Connection.ExecuteQuery('SELECT * FROM data_types_demo ORDER BY id');
    try
      while not DataSet.EOF do
      begin
        WriteLn(Format('   Row ID: %d', [DataSet.FieldByName('id').AsInteger]));

        // TEXT
        WriteLn(Format('   - TEXT: %s', [DataSet.FieldByName('text_col').AsString]));

        // INTEGER
        if DataSet.FieldByName('int_col').IsNull then
          WriteLn('   - INTEGER: NULL')
        else
          WriteLn(Format('   - INTEGER: %d', [DataSet.FieldByName('int_col').AsInteger]));

        // REAL
        if DataSet.FieldByName('real_col').IsNull then
          WriteLn('   - REAL: NULL')
        else
          WriteLn(Format('   - REAL: %.10f', [DataSet.FieldByName('real_col').AsFloat]));

        // BLOB
        if DataSet.FieldByName('blob_col').IsNull then
          WriteLn('   - BLOB: NULL')
        else
        begin
          BlobStream := DataSet.CreateBlobStream(DataSet.FieldByName('blob_col'), bmRead);
          try
            WriteLn(Format('   - BLOB: %d bytes', [BlobStream.Size]));
          finally
            BlobStream.Free;
          end;
        end;

        // NULL handling
        if DataSet.FieldByName('nullable_col').IsNull then
          WriteLn('   - NULLABLE: NULL')
        else if DataSet.FieldByName('nullable_col').AsString = '' then
          WriteLn('   - NULLABLE: (empty string)')
        else
          WriteLn(Format('   - NULLABLE: %s', [DataSet.FieldByName('nullable_col').AsString]));

        // BOOLEAN (stored as INTEGER)
        if DataSet.FieldByName('bool_col').IsNull then
          WriteLn('   - BOOLEAN: NULL')
        else if DataSet.FieldByName('bool_col').AsInteger = 1 then
          WriteLn('   - BOOLEAN: True')
        else
          WriteLn('   - BOOLEAN: False');

        // DATE
        if DataSet.FieldByName('date_col').IsNull then
          WriteLn('   - DATE: NULL')
        else
          WriteLn(Format('   - DATE: %s', [DataSet.FieldByName('date_col').AsString]));

        // DATETIME
        if DataSet.FieldByName('datetime_col').IsNull then
          WriteLn('   - DATETIME: NULL')
        else
          WriteLn(Format('   - DATETIME: %s', [DataSet.FieldByName('datetime_col').AsString]));

        WriteLn('   --------------------------------------------------------');
        DataSet.Next;
      end;
    finally
      DataSet.Free;
    end;
    WriteLn;

    // Type affinity demonstration
    WriteLn('4. SQLite type affinity demonstration:');
    Connection.ExecuteNonQuery(
      'CREATE TABLE affinity_demo (val)'); // No type specified

    Connection.ExecuteNonQuery('INSERT INTO affinity_demo VALUES (?)', [123]);
    Connection.ExecuteNonQuery('INSERT INTO affinity_demo VALUES (?)', ['456']);
    Connection.ExecuteNonQuery('INSERT INTO affinity_demo VALUES (?)', [78.9]);
    Connection.ExecuteNonQuery('INSERT INTO affinity_demo VALUES (?)', ['text']);
    Connection.ExecuteNonQuery('INSERT INTO affinity_demo VALUES (?)', [Null]);

    DataSet := Connection.ExecuteQuery(
      'SELECT val, typeof(val) as type FROM affinity_demo');
    try
      while not DataSet.EOF do
      begin
        WriteLn(Format('   Value: %-10s | Type: %s', [
          DataSet.FieldByName('val').AsString,
          DataSet.FieldByName('type').AsString
        ]));
        DataSet.Next;
      end;
    finally
      DataSet.Free;
    end;
    WriteLn;

    // Large integer handling
    WriteLn('5. Large integer handling (Int64):');
    Connection.ExecuteNonQuery(
      'CREATE TABLE large_int (big_value INTEGER)');
    Connection.ExecuteNonQuery(
      'INSERT INTO large_int VALUES (?)', [Int64(9223372036854775807)]); // Max Int64

    DataSet := Connection.ExecuteQuery('SELECT big_value FROM large_int');
    try
      WriteLn(Format('   Max Int64 value: %d', [DataSet.Fields[0].AsLargeInt]));
    finally
      DataSet.Free;
    end;
    WriteLn;

    // Date functions in SQLite
    WriteLn('6. SQLite date functions:');
    DataSet := Connection.ExecuteQuery(
      'SELECT ' +
      '  date(''now'') as today,' +
      '  datetime(''now'') as now_utc,' +
      '  datetime(''now'', ''localtime'') as now_local,' +
      '  date(''now'', ''+7 days'') as next_week,' +
      '  strftime(''%Y-%m-%d %H:%M:%S'', ''now'') as formatted');
    try
      WriteLn(Format('   Today: %s', [DataSet.FieldByName('today').AsString]));
      WriteLn(Format('   Now (UTC): %s', [DataSet.FieldByName('now_utc').AsString]));
      WriteLn(Format('   Now (Local): %s', [DataSet.FieldByName('now_local').AsString]));
      WriteLn(Format('   Next week: %s', [DataSet.FieldByName('next_week').AsString]));
      WriteLn(Format('   Formatted: %s', [DataSet.FieldByName('formatted').AsString]));
    finally
      DataSet.Free;
    end;
    WriteLn;

    Connection.Close;

  finally
    Connection.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
