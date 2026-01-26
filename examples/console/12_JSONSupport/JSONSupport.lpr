{===============================================================================
  NDXSQLite Example 12 - JSON Support
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - JSON validation and parsing
  - JSON path queries
  - JSON creation and manipulation
  - JSON aggregation functions
  - Storing JSON in SQLite

  Cross-platform: Windows, Linux, macOS
  Note: Requires SQLite 3.38+ for native JSON functions
===============================================================================}
program JSONSupport;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqlitejson;

var
  Conn: TNDXSQLiteConnection;
  JSON: TNDXSQLiteJSON;
  DataSet: TDataSet;
  DBPath: string;
  TestJSON, ResultJSON: string;
  Keys: TStringList;
  I: Integer;

begin
  WriteLn('=== NDXSQLite Example 12: JSON Support ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example12.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Conn := TNDXSQLiteConnection.Create(DBPath);
  Conn.Open;

  JSON := TNDXSQLiteJSON.Create(Conn);
    try
      // 1. Check JSON support
      WriteLn('1. Checking JSON support:');
      if JSON.IsJSONSupported then
        WriteLn('   JSON functions are available!')
      else
      begin
        WriteLn('   JSON functions not available (SQLite < 3.38)');
        WriteLn('   Skipping example.');
        Exit;
      end;
      WriteLn;

      // Sample JSON for testing
      TestJSON := '{"name": "Alice", "age": 30, "city": "Paris", "tags": ["developer", "pascal"]}';

      // 2. JSON validation
      WriteLn('2. JSON validation:');
      WriteLn('   Valid JSON: ', JSON.IsValidJSON(TestJSON));
      WriteLn('   Invalid JSON: ', JSON.IsValidJSON('not json'));
      WriteLn('   JSON type: ', Ord(JSON.GetJSONType(TestJSON)), ' (Object)');
      WriteLn;

      // 3. JSON extraction
      WriteLn('3. JSON extraction ($.path syntax):');
      WriteLn('   $.name: ', JSON.JSONExtract(TestJSON, '$.name'));
      WriteLn('   $.age: ', JSON.JSONExtract(TestJSON, '$.age'));
      WriteLn('   $.city: ', JSON.JSONExtract(TestJSON, '$.city'));
      WriteLn('   $.tags[0]: ', JSON.JSONExtract(TestJSON, '$.tags[0]'));
      WriteLn('   $.tags[1]: ', JSON.JSONExtract(TestJSON, '$.tags[1]'));
      WriteLn;

      // 4. JSON manipulation
      WriteLn('4. JSON manipulation:');
      ResultJSON := JSON.JSONSet(TestJSON, '$.age', 31);
      WriteLn('   After JSONSet(age=31): ', ResultJSON);

      ResultJSON := JSON.JSONInsert(TestJSON, '$.country', 'France');
      WriteLn('   After JSONInsert(country): ', ResultJSON);

      ResultJSON := JSON.JSONRemove(TestJSON, ['$.tags']);
      WriteLn('   After JSONRemove(tags): ', ResultJSON);
      WriteLn;

      // 5. JSON creation
      WriteLn('5. JSON creation:');
      ResultJSON := JSON.JSONObject(
        ['name', 'email', 'active'],
        ['Bob', 'bob@example.com', True]);
      WriteLn('   Created object: ', ResultJSON);

      ResultJSON := JSON.JSONArray([1, 2, 3, 'four', True]);
      WriteLn('   Created array: ', ResultJSON);
      WriteLn;

      // 6. JSON array length and keys
      WriteLn('6. JSON utilities:');
      WriteLn('   Array length (tags): ', JSON.JSONArrayLength(TestJSON, '$.tags'));

      Keys := JSON.JSONKeys(TestJSON);
      try
        Write('   Object keys: ');
        for I := 0 to Keys.Count - 1 do
        begin
          if I > 0 then Write(', ');
          Write(Keys[I]);
        end;
        WriteLn;
      finally
        Keys.Free;
      end;
      WriteLn;

      // 7. Store and query JSON in table
      WriteLn('7. Storing JSON in table:');
      Conn.ExecuteNonQuery(
        'CREATE TABLE products (' +
        '  id INTEGER PRIMARY KEY,' +
        '  name TEXT,' +
        '  metadata TEXT' +  // JSON stored as TEXT
        ')');

      Conn.ExecuteNonQuery(
        'INSERT INTO products (name, metadata) VALUES (?, ?)',
        ['Laptop', '{"brand": "Dell", "ram": 16, "specs": {"cpu": "i7", "storage": "512GB"}}']);
      Conn.ExecuteNonQuery(
        'INSERT INTO products (name, metadata) VALUES (?, ?)',
        ['Phone', '{"brand": "Apple", "ram": 8, "specs": {"cpu": "A15", "storage": "256GB"}}']);
      Conn.ExecuteNonQuery(
        'INSERT INTO products (name, metadata) VALUES (?, ?)',
        ['Tablet', '{"brand": "Samsung", "ram": 6, "specs": {"cpu": "Snapdragon", "storage": "128GB"}}']);
      WriteLn('   Inserted 3 products with JSON metadata.');
      WriteLn;

      // 8. Query JSON columns
      WriteLn('8. Querying JSON columns:');
      DataSet := Conn.ExecuteQuery(
        'SELECT name, ' +
        '  json_extract(metadata, ''$.brand'') as brand, ' +
        '  json_extract(metadata, ''$.ram'') as ram, ' +
        '  json_extract(metadata, ''$.specs.cpu'') as cpu ' +
        'FROM products');
      try
        while not DataSet.EOF do
        begin
          WriteLn(Format('   %s: %s, %sGB RAM, %s', [
            DataSet.FieldByName('name').AsString,
            DataSet.FieldByName('brand').AsString,
            DataSet.FieldByName('ram').AsString,
            DataSet.FieldByName('cpu').AsString
          ]));
          DataSet.Next;
        end;
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 9. JSON aggregation
      WriteLn('9. JSON aggregation (group functions):');
      DataSet := Conn.ExecuteQuery(
        'SELECT json_group_array(name) as names FROM products');
      try
        WriteLn('   All names: ', DataSet.Fields[0].AsString);
      finally
        DataSet.Free;
      end;

      DataSet := Conn.ExecuteQuery(
        'SELECT json_group_object(name, json_extract(metadata, ''$.brand'')) as mapping FROM products');
      try
        WriteLn('   Name->Brand: ', DataSet.Fields[0].AsString);
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 10. Filter by JSON values
      WriteLn('10. Filtering by JSON values:');
      DataSet := Conn.ExecuteQuery(
        'SELECT name FROM products WHERE json_extract(metadata, ''$.ram'') >= 8');
      try
        Write('    Products with >= 8GB RAM: ');
        while not DataSet.EOF do
        begin
          Write(DataSet.Fields[0].AsString);
          DataSet.Next;
          if not DataSet.EOF then Write(', ');
        end;
        WriteLn;
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 11. Best practices
      WriteLn('11. JSON Best Practices:');
      WriteLn('    - Use for flexible/sparse attributes');
      WriteLn('    - Index extracted values for queries:');
      WriteLn('      CREATE INDEX idx_brand ON products(json_extract(metadata, ''$.brand''))');
      WriteLn('    - Validate JSON before inserting');
      WriteLn('    - Use generated columns for frequently queried paths');
      WriteLn;

  finally
    JSON.Free;
  end;

  Conn.Close;
  Conn.Free;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
