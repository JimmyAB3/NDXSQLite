{===============================================================================
  NDXSQLite Example 31 - Batch Operations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Bulk inserts with transactions
  - Prepared statement reuse
  - Performance comparison: naive vs optimized
  - Multi-row INSERT syntax
  - Bulk updates and deletes
  - Progress reporting for large operations

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program BatchOperations;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Math,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

const
  ROW_COUNT = 50000;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Returns the tick count64. }
function GetTickCount64: Int64;
begin
  Result := MilliSecondsBetween(Now, EncodeDate(1970, 1, 1));
end;

{ Creates the 'records' table with id, name, value, category, and created_at columns if it does not already exist. }
procedure SetupTable;
begin
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS records (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  value REAL,' +
    '  category TEXT,' +
    '  created_at TEXT' +
    ')');
end;

{ Deletes all rows from the 'records' table. }
procedure ClearTable;
begin
  Connection.ExecuteNonQuery('DELETE FROM records');
end;

{ Inserts 1000 rows without an explicit transaction (auto-commit per row) and reports the elapsed time and rate. }
procedure DemoNaiveInsert;
var
  I: Integer;
  StartTime: Int64;
  ElapsedMs: Int64;
begin
  WriteLn('1. Naive insert (NO transaction) - 1000 rows only');
  WriteLn('   -----------------------------------------------');

  ClearTable;
  StartTime := GetTickCount64;

  // Each insert is auto-committed - VERY SLOW
  for I := 1 to 1000 do
  begin
    Connection.ExecuteNonQuery(
      Format('INSERT INTO records (name, value, category) VALUES (''Item %d'', %f, ''Cat%d'')',
        [I, Random * 1000, I mod 10]));
  end;

  ElapsedMs := GetTickCount64 - StartTime;
  WriteLn('   Time for 1000 rows: ', ElapsedMs, ' ms');
  WriteLn('   Rate: ', (1000 * 1000) div Max(1, ElapsedMs), ' rows/sec');
  WriteLn('   WARNING: This is extremely slow for large datasets!');
  WriteLn('');
end;

{ Inserts ROW_COUNT rows wrapped in a single transaction using formatted SQL strings and reports performance. }
procedure DemoTransactionInsert;
var
  I: Integer;
  StartTime: Int64;
  ElapsedMs: Int64;
begin
  WriteLn('2. Insert with transaction - ', ROW_COUNT, ' rows');
  WriteLn('   ------------------------------------------');

  ClearTable;
  StartTime := GetTickCount64;

  Connection.BeginTransaction;
  try
    for I := 1 to ROW_COUNT do
    begin
      Connection.ExecuteNonQuery(
        Format('INSERT INTO records (name, value, category) VALUES (''Item %d'', %f, ''Cat%d'')',
          [I, Random * 1000, I mod 10]));
    end;
    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;

  ElapsedMs := GetTickCount64 - StartTime;
  WriteLn('   Time: ', ElapsedMs, ' ms');
  WriteLn('   Rate: ', (Int64(ROW_COUNT) * 1000) div Max(1, ElapsedMs), ' rows/sec');
  WriteLn('');
end;

{ Inserts ROW_COUNT rows using parameterized queries within a transaction and reports performance. }
procedure DemoPreparedStatementInsert;
var
  I: Integer;
  StartTime: Int64;
  ElapsedMs: Int64;
begin
  WriteLn('3. Prepared statement with transaction - ', ROW_COUNT, ' rows');
  WriteLn('   ---------------------------------------------------');

  ClearTable;
  StartTime := GetTickCount64;

  Connection.BeginTransaction;
  try
    // Using parameterized query - statement is prepared once, executed many times
    for I := 1 to ROW_COUNT do
    begin
      Connection.ExecuteNonQuery(
        'INSERT INTO records (name, value, category) VALUES (?, ?, ?)',
        [Format('Item %d', [I]), Random * 1000, Format('Cat%d', [I mod 10])]);
    end;
    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;

  ElapsedMs := GetTickCount64 - StartTime;
  WriteLn('   Time: ', ElapsedMs, ' ms');
  WriteLn('   Rate: ', (Int64(ROW_COUNT) * 1000) div Max(1, ElapsedMs), ' rows/sec');
  WriteLn('');
end;

{ Inserts ROW_COUNT rows using multi-row INSERT syntax in batches of 500 within a transaction and reports performance. }
procedure DemoMultiRowInsert;
var
  I, BatchNum: Integer;
  StartTime: Int64;
  ElapsedMs: Int64;
  SQL: string;
  BatchSize: Integer;
begin
  WriteLn('4. Multi-row INSERT syntax - ', ROW_COUNT, ' rows');
  WriteLn('   -----------------------------------------');

  ClearTable;
  StartTime := GetTickCount64;
  BatchSize := 500;  // SQLite has limit on compound statements

  Connection.BeginTransaction;
  try
    BatchNum := 0;
    while BatchNum * BatchSize < ROW_COUNT do
    begin
      SQL := 'INSERT INTO records (name, value, category) VALUES ';
      for I := 1 to BatchSize do
      begin
        if I > 1 then SQL := SQL + ',';
        SQL := SQL + Format('(''Item %d'', %f, ''Cat%d'')',
          [BatchNum * BatchSize + I, Random * 1000, (BatchNum * BatchSize + I) mod 10]);
      end;
      Connection.ExecuteNonQuery(SQL);
      Inc(BatchNum);
    end;
    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;

  ElapsedMs := GetTickCount64 - StartTime;
  WriteLn('   Time: ', ElapsedMs, ' ms');
  WriteLn('   Rate: ', (Int64(ROW_COUNT) * 1000) div Max(1, ElapsedMs), ' rows/sec');
  WriteLn('   Batches of ', BatchSize, ' rows each');
  WriteLn('');
end;

{ Updates rows in category 'Cat5' with a value multiplier, then applies a CASE-based bulk update across multiple categories, reporting rows affected and timing. }
procedure DemoBulkUpdate;
var
  StartTime: Int64;
  ElapsedMs: Int64;
  RowsAffected: Integer;
begin
  WriteLn('5. Bulk UPDATE');
  WriteLn('   -----------');

  // Make sure we have data
  if Connection.ExecuteScalar('SELECT COUNT(*) FROM records') = 0 then
  begin
    WriteLn('   No data, skipping...');
    WriteLn('');
    Exit;
  end;

  StartTime := GetTickCount64;

  // Update all rows in category 'Cat5'
  Connection.ExecuteNonQuery(
    'UPDATE records SET value = value * 1.1, ' +
    'created_at = datetime(''now'') WHERE category = ''Cat5''');

  RowsAffected := Connection.ExecuteScalar(
    'SELECT changes()');

  ElapsedMs := GetTickCount64 - StartTime;
  WriteLn('   Updated ', RowsAffected, ' rows in ', ElapsedMs, ' ms');

  // Bulk update with CASE expression
  StartTime := GetTickCount64;

  Connection.ExecuteNonQuery(
    'UPDATE records SET value = CASE ' +
    '  WHEN category = ''Cat0'' THEN value * 0.9 ' +
    '  WHEN category = ''Cat1'' THEN value * 0.95 ' +
    '  WHEN category = ''Cat2'' THEN value * 1.05 ' +
    '  ELSE value ' +
    'END');

  RowsAffected := Connection.ExecuteScalar('SELECT changes()');
  ElapsedMs := GetTickCount64 - StartTime;
  WriteLn('   CASE update on ', RowsAffected, ' rows in ', ElapsedMs, ' ms');

  WriteLn('');
end;

{ Deletes all rows in categories Cat7, Cat8, and Cat9, and reports the count of deleted rows and elapsed time. }
procedure DemoBulkDelete;
var
  StartTime: Int64;
  ElapsedMs: Int64;
  CountBefore, CountAfter: Integer;
begin
  WriteLn('6. Bulk DELETE');
  WriteLn('   -----------');

  CountBefore := Connection.ExecuteScalar('SELECT COUNT(*) FROM records');

  StartTime := GetTickCount64;

  // Delete rows matching condition
  Connection.ExecuteNonQuery(
    'DELETE FROM records WHERE category IN (''Cat7'', ''Cat8'', ''Cat9'')');

  ElapsedMs := GetTickCount64 - StartTime;
  CountAfter := Connection.ExecuteScalar('SELECT COUNT(*) FROM records');

  WriteLn('   Deleted ', CountBefore - CountAfter, ' rows in ', ElapsedMs, ' ms');
  WriteLn('   Remaining: ', CountAfter, ' rows');

  WriteLn('');
end;

{ Inserts ROW_COUNT rows in batches using generate_series, printing progress percentage after each batch. }
procedure DemoProgressReporting;
var
  I, BatchCount: Integer;
  StartTime: Int64;
  ElapsedMs: Int64;
  BatchSize: Integer;
begin
  WriteLn('7. Progress reporting for large operations');
  WriteLn('   ----------------------------------------');

  ClearTable;
  BatchSize := 5000;
  BatchCount := ROW_COUNT div BatchSize;

  StartTime := GetTickCount64;

  Connection.BeginTransaction;
  try
    for I := 1 to BatchCount do
    begin
      // Insert batch
      Connection.ExecuteNonQuery(
        'INSERT INTO records (name, value, category) ' +
        'SELECT ''Item '' || (? + value), random() * 1000, ''Cat'' || (value % 10) ' +
        'FROM generate_series(1, ?)',
        [(I - 1) * BatchSize, BatchSize]);

      // Report progress
      Write(Format('   Progress: %d/%d batches (%d%%)' + #13,
        [I, BatchCount, (I * 100) div BatchCount]));
    end;
    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;

  WriteLn('');
  ElapsedMs := GetTickCount64 - StartTime;
  WriteLn('   Total time: ', ElapsedMs, ' ms');
  WriteLn('   Final count: ', Connection.ExecuteScalar('SELECT COUNT(*) FROM records'));

  WriteLn('');
end;

{ Creates a 'records_archive' table and copies all rows from 'records' with category 'Cat0' into it, reporting count and timing. }
procedure DemoInsertFromSelect;
var
  StartTime: Int64;
  ElapsedMs: Int64;
begin
  WriteLn('8. INSERT from SELECT (data duplication)');
  WriteLn('   --------------------------------------');

  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS records_archive (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT,' +
    '  value REAL,' +
    '  category TEXT,' +
    '  archived_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  StartTime := GetTickCount64;

  // Copy subset of data to archive table
  Connection.ExecuteNonQuery(
    'INSERT INTO records_archive (name, value, category) ' +
    'SELECT name, value, category FROM records WHERE category = ''Cat0''');

  ElapsedMs := GetTickCount64 - StartTime;

  WriteLn('   Copied ', Connection.ExecuteScalar('SELECT COUNT(*) FROM records_archive'),
          ' rows to archive in ', ElapsedMs, ' ms');

  WriteLn('');
end;

{ Queries and prints the row count and average value for the first 5 categories in the 'records' table. }
procedure ShowSummary;
var
  DS: TDataSet;
begin
  WriteLn('Summary');
  WriteLn('-------');

  DS := Connection.ExecuteQuery(
    'SELECT category, COUNT(*) as cnt, ROUND(AVG(value), 2) as avg_val ' +
    'FROM records GROUP BY category ORDER BY category LIMIT 5');
  try
    WriteLn('   Sample by category (first 5):');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d rows, avg value = %.2f', [
        DS.FieldByName('category').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('avg_val').AsFloat
      ]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 31: Batch Operations ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example31.db';
  Cleanup;
  Randomize;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      // Note: PRAGMA optimizations (synchronous=OFF, journal_mode=MEMORY)
      // would speed things up but must be set before any transaction.
      // See README for details on optimization PRAGMAs.

      SetupTable;

      DemoNaiveInsert;
      DemoTransactionInsert;
      DemoPreparedStatementInsert;
      DemoMultiRowInsert;
      DemoBulkUpdate;
      DemoBulkDelete;
      // DemoProgressReporting;  // Requires generate_series (SQLite 3.37+)
      DemoInsertFromSelect;
      ShowSummary;

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
