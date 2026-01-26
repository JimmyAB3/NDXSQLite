{===============================================================================
  NDXSQLite Example 01 - Basic Connection
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Opening a connection to a SQLite database
  - Creating a table
  - Inserting data
  - Querying data
  - Closing the connection

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program BasicConnection;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection;

var
  Connection: TNDXSQLiteConnection;
  DataSet: TDataSet;
  DBPath: string;

begin
  WriteLn('=== NDXSQLite Example 01: Basic Connection ===');
  WriteLn;

  // Database path (in current directory)
  DBPath := ExtractFilePath(ParamStr(0)) + 'example01.db';

  // Delete existing database for a fresh start
  if FileExists(DBPath) then
    DeleteFile(DBPath);

  // Create connection
  Connection := TNDXSQLiteConnection.Create(DBPath);
  try
    // Open the connection
    WriteLn('1. Opening database: ', DBPath);
    Connection.Open;
    WriteLn('   Database opened successfully.');
    WriteLn;

    // Create a table
    WriteLn('2. Creating table "users"...');
    Connection.ExecuteNonQuery(
      'CREATE TABLE users (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name TEXT NOT NULL,' +
      '  email TEXT UNIQUE,' +
      '  age INTEGER' +
      ')'
    );
    WriteLn('   Table created successfully.');
    WriteLn;

    // Insert data
    WriteLn('3. Inserting data...');
    Connection.ExecuteNonQuery(
      'INSERT INTO users (name, email, age) VALUES (''Alice'', ''alice@example.com'', 30)');
    Connection.ExecuteNonQuery(
      'INSERT INTO users (name, email, age) VALUES (''Bob'', ''bob@example.com'', 25)');
    Connection.ExecuteNonQuery(
      'INSERT INTO users (name, email, age) VALUES (''Charlie'', ''charlie@example.com'', 35)');
    WriteLn('   3 records inserted.');
    WriteLn;

    // Query data
    WriteLn('4. Querying all users:');
    WriteLn('   -----------------------------------------');
    DataSet := Connection.ExecuteQuery('SELECT id, name, email, age FROM users ORDER BY name');
    try
      while not DataSet.EOF do
      begin
        WriteLn(Format('   ID: %d | Name: %-10s | Email: %-25s | Age: %d', [
          DataSet.FieldByName('id').AsInteger,
          DataSet.FieldByName('name').AsString,
          DataSet.FieldByName('email').AsString,
          DataSet.FieldByName('age').AsInteger
        ]));
        DataSet.Next;
      end;
    finally
      DataSet.Free;
    end;
    WriteLn('   -----------------------------------------');
    WriteLn;

    // Close the connection
    WriteLn('5. Closing database...');
    Connection.Close;
    WriteLn('   Database closed successfully.');

  finally
    Connection.Free;
  end;

  WriteLn;
  WriteLn('=== Example completed successfully! ===');

  // Clean up test database
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
