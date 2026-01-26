{===============================================================================
  NDXSQLite - JSON Support (JSON1 Extension)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Provides a high-level wrapper around SQLite's JSON1 extension functions.
  Supports validation, path-based extraction, in-place manipulation (set,
  insert, replace, remove, patch), creation of JSON objects and arrays,
  aggregation from table data, tree/each iteration, and export to files.

  All JSON path expressions use the SQLite JSON path syntax:
    '$'          - Root element
    '$.key'      - Object member
    '$[0]'       - Array element by index
    '$.a.b[2].c' - Nested path

  Note: Requires SQLite 3.38+ for native JSON functions (built-in since 3.38).
===============================================================================}
unit ndxsqlitejson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

type
  { JSON value type }
  TNDXJSONValueType = (
    jvtNull,
    jvtTrue,
    jvtFalse,
    jvtInteger,
    jvtReal,
    jvtText,
    jvtBlob,
    jvtArray,
    jvtObject
  );

  { High-level wrapper for SQLite JSON1 extension functions.
    Provides validation, extraction, manipulation, creation, aggregation,
    querying, transformation, and file export of JSON data stored in SQLite. }
  TNDXSQLiteJSON = class
  private
    FConnection: TNDXSQLiteConnection;

    function ValueTypeToString(AType: TNDXJSONValueType): string;

  public
    { Creates a JSON manager bound to the specified connection. }
    constructor Create(AConnection: TNDXSQLiteConnection);

    { Availability }

    { Returns True if the SQLite JSON1 extension is available on this connection. }
    function IsJSONSupported: Boolean;

    { Validation }

    { Returns True if the string contains well-formed JSON. }
    function IsValidJSON(const AJSON: string): Boolean;
    { Returns the JSON value type of the root element (object, array, text, etc.). }
    function GetJSONType(const AJSON: string): TNDXJSONValueType;

    { Value extraction }

    { Extracts a value at the given JSON path. Returns Null if path not found. }
    function JSONExtract(const AJSON, APath: string): Variant;
    { Extracts a value at the given path as a string (empty string if not found). }
    function JSONExtractText(const AJSON, APath: string): string;
    { Extracts a JSON array at the given path into a string list. Caller must free. }
    function JSONExtractArray(const AJSON, APath: string): TStringList;

    { Manipulation }

    { Sets a value at the given path (creates or replaces). Returns modified JSON. }
    function JSONSet(const AJSON, APath: string; const AValue: Variant): string;
    { Inserts a value at the given path (only if path does not exist). Returns modified JSON. }
    function JSONInsert(const AJSON, APath: string; const AValue: Variant): string;
    { Replaces a value at the given path (only if path exists). Returns modified JSON. }
    function JSONReplace(const AJSON, APath: string; const AValue: Variant): string;
    { Removes one or more paths from the JSON. Returns modified JSON. }
    function JSONRemove(const AJSON: string; const APaths: array of string): string;
    { Applies a RFC 7396 JSON Merge Patch to the target JSON. Returns patched JSON. }
    function JSONPatch(const AJSON, APatch: string): string;

    { Creation }

    { Creates a JSON object from parallel arrays of keys and values. }
    function JSONObject(const AKeys: array of string;
      const AValues: array of Variant): string;
    { Creates a JSON array from an array of values. }
    function JSONArray(const AValues: array of Variant): string;
    { Wraps a string value as a properly quoted JSON string. }
    function JSONQuote(const AValue: string): string;

    { Aggregation }

    { Aggregates column values from a table into a JSON array. }
    function JSONGroupArray(const ATableName, AColumn: string;
      const AWhereClause: string = ''): string;
    { Aggregates key-value pairs from a table into a JSON object. }
    function JSONGroupObject(const ATableName, AKeyColumn, AValueColumn: string;
      const AWhereClause: string = ''): string;

    { Queries on JSON columns }

    { Queries a JSON column extracting values at a path. Returns a TDataSet. Caller must free. }
    function QueryJSONPath(const ATableName, AJSONColumn, APath: string): TDataSet;
    { Queries rows where a JSON path equals a value. Returns a TDataSet. Caller must free. }
    function QueryJSONWhere(const ATableName, AJSONColumn, APath: string;
      const AValue: Variant): TDataSet;
    { Queries rows where a JSON column contains a value. Returns a TDataSet. Caller must free. }
    function QueryJSONContains(const ATableName, AJSONColumn: string;
      const AValue: Variant): TDataSet;

    { Transformation }

    { Converts a JSON array of objects to a TDataSet (one row per object). Caller must free. }
    function JSONToTable(const AJSON: string): TDataSet;
    { Converts table rows to a JSON array of objects. }
    function TableToJSON(const ATableName: string;
      const AWhereClause: string = ''): string;
    { Executes a SQL query and returns the result as a JSON array of objects. }
    function QueryToJSON(const ASQL: string): string;

    { File export }

    { Exports a table to a JSON file. Returns True on success.
      @param APrettyPrint  If True, formats the JSON with indentation. }
    function ExportTableToFile(const ATableName, AFilePath: string;
      const AWhereClause: string = ''; APrettyPrint: Boolean = False): Boolean;
    { Exports a SQL query result to a JSON file. Returns True on success.
      @param APrettyPrint  If True, formats the JSON with indentation. }
    function ExportQueryToFile(const ASQL, AFilePath: string;
      APrettyPrint: Boolean = False): Boolean;

    { Utility functions }

    { Returns the number of elements in a JSON array at the given path. }
    function JSONArrayLength(const AJSON: string; const APath: string = '$'): Integer;
    { Returns a list of all top-level keys in a JSON object. Caller must free. }
    function JSONKeys(const AJSON: string): TStringList;
    { Iterates over top-level elements of a JSON value. Returns a TDataSet. Caller must free. }
    function JSONEach(const AJSON: string): TDataSet;
    { Recursively iterates all elements of a JSON value. Returns a TDataSet. Caller must free. }
    function JSONTree(const AJSON: string): TDataSet;
  end;

implementation

{ Helper function for pretty printing JSON }
function FormatJSONPretty(const AJSON: string): string; forward;

{ TNDXSQLiteJSON }

constructor TNDXSQLiteJSON.Create(AConnection: TNDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

function TNDXSQLiteJSON.ValueTypeToString(AType: TNDXJSONValueType): string;
begin
  case AType of
    jvtNull:    Result := 'null';
    jvtTrue:    Result := 'true';
    jvtFalse:   Result := 'false';
    jvtInteger: Result := 'integer';
    jvtReal:    Result := 'real';
    jvtText:    Result := 'text';
    jvtBlob:    Result := 'blob';
    jvtArray:   Result := 'array';
    jvtObject:  Result := 'object';
  else
    Result := 'unknown';
  end;
end;

function TNDXSQLiteJSON.IsJSONSupported: Boolean;
var
  V: Variant;
begin
  try
    V := FConnection.ExecuteScalar('SELECT json(''{"test":1}'')');
    Result := not VarIsNull(V);
  except
    Result := False;
  end;
end;

function TNDXSQLiteJSON.IsValidJSON(const AJSON: string): Boolean;
var
  V: Variant;
begin
  try
    V := FConnection.ExecuteScalar(Format('SELECT json_valid(''%s'')',
      [StringReplace(AJSON, '''', '''''', [rfReplaceAll])]));
    Result := (not VarIsNull(V)) and (Integer(V) = 1);
  except
    Result := False;
  end;
end;

function TNDXSQLiteJSON.GetJSONType(const AJSON: string): TNDXJSONValueType;
var
  V: Variant;
  TypeStr: string;
begin
  Result := jvtNull;
  try
    V := FConnection.ExecuteScalar(Format('SELECT json_type(''%s'')',
      [StringReplace(AJSON, '''', '''''', [rfReplaceAll])]));
    if not VarIsNull(V) then
    begin
      TypeStr := LowerCase(VarToStr(V));
      if TypeStr = 'null' then Result := jvtNull
      else if TypeStr = 'true' then Result := jvtTrue
      else if TypeStr = 'false' then Result := jvtFalse
      else if TypeStr = 'integer' then Result := jvtInteger
      else if TypeStr = 'real' then Result := jvtReal
      else if TypeStr = 'text' then Result := jvtText
      else if TypeStr = 'blob' then Result := jvtBlob
      else if TypeStr = 'array' then Result := jvtArray
      else if TypeStr = 'object' then Result := jvtObject;
    end;
  except
    Result := jvtNull;
  end;
end;

function TNDXSQLiteJSON.JSONExtract(const AJSON, APath: string): Variant;
begin
  Result := FConnection.ExecuteScalar(Format(
    'SELECT json_extract(''%s'', ''%s'')',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll]), APath]));
end;

function TNDXSQLiteJSON.JSONExtractText(const AJSON, APath: string): string;
var
  V: Variant;
begin
  V := JSONExtract(AJSON, APath);
  if VarIsNull(V) then
    Result := ''
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONExtractArray(const AJSON, APath: string): TStringList;
var
  DS: TDataSet;
begin
  Result := TStringList.Create;
  try
    DS := FConnection.ExecuteQuery(Format(
      'SELECT value FROM json_each(json_extract(''%s'', ''%s''))',
      [StringReplace(AJSON, '''', '''''', [rfReplaceAll]), APath]));
    try
      while not DS.EOF do
      begin
        Result.Add(DS.Fields[0].AsString);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
  except
    // Return empty list on error
  end;
end;

function TNDXSQLiteJSON.JSONSet(const AJSON, APath: string;
  const AValue: Variant): string;
var
  V: Variant;
  ValueStr: string;
begin
  if VarIsNull(AValue) then
    ValueStr := 'null'
  else if VarType(AValue) = varString then
    ValueStr := Format('"%s"', [StringReplace(VarToStr(AValue), '"', '\"', [rfReplaceAll])])
  else if VarType(AValue) = varBoolean then
  begin
    if Boolean(AValue) then
      ValueStr := 'true'
    else
      ValueStr := 'false';
  end
  else
    ValueStr := VarToStr(AValue);

  V := FConnection.ExecuteScalar(Format(
    'SELECT json_set(''%s'', ''%s'', json(''%s''))',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll]), APath, ValueStr]));

  if VarIsNull(V) then
    Result := AJSON
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONInsert(const AJSON, APath: string;
  const AValue: Variant): string;
var
  V: Variant;
  ValueStr: string;
begin
  if VarIsNull(AValue) then
    ValueStr := 'null'
  else if VarType(AValue) = varString then
    ValueStr := Format('"%s"', [StringReplace(VarToStr(AValue), '"', '\"', [rfReplaceAll])])
  else
    ValueStr := VarToStr(AValue);

  V := FConnection.ExecuteScalar(Format(
    'SELECT json_insert(''%s'', ''%s'', json(''%s''))',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll]), APath, ValueStr]));

  if VarIsNull(V) then
    Result := AJSON
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONReplace(const AJSON, APath: string;
  const AValue: Variant): string;
var
  V: Variant;
  ValueStr: string;
begin
  if VarIsNull(AValue) then
    ValueStr := 'null'
  else if VarType(AValue) = varString then
    ValueStr := Format('"%s"', [StringReplace(VarToStr(AValue), '"', '\"', [rfReplaceAll])])
  else
    ValueStr := VarToStr(AValue);

  V := FConnection.ExecuteScalar(Format(
    'SELECT json_replace(''%s'', ''%s'', json(''%s''))',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll]), APath, ValueStr]));

  if VarIsNull(V) then
    Result := AJSON
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONRemove(const AJSON: string;
  const APaths: array of string): string;
var
  V: Variant;
  PathList: string;
  I: Integer;
begin
  PathList := '';
  for I := Low(APaths) to High(APaths) do
  begin
    if PathList <> '' then
      PathList := PathList + ', ';
    PathList := PathList + Format('''%s''', [APaths[I]]);
  end;

  V := FConnection.ExecuteScalar(Format(
    'SELECT json_remove(''%s'', %s)',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll]), PathList]));

  if VarIsNull(V) then
    Result := AJSON
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONPatch(const AJSON, APatch: string): string;
var
  V: Variant;
begin
  V := FConnection.ExecuteScalar(Format(
    'SELECT json_patch(''%s'', ''%s'')',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll]),
     StringReplace(APatch, '''', '''''', [rfReplaceAll])]));

  if VarIsNull(V) then
    Result := AJSON
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONObject(const AKeys: array of string;
  const AValues: array of Variant): string;
var
  V: Variant;
  Args: string;
  I: Integer;
  ValueStr: string;
begin
  Args := '';
  for I := Low(AKeys) to High(AKeys) do
  begin
    if Args <> '' then
      Args := Args + ', ';

    if VarIsNull(AValues[I]) then
      ValueStr := 'null'
    else if VarType(AValues[I]) = varString then
      ValueStr := Format('''%s''', [StringReplace(VarToStr(AValues[I]), '''', '''''', [rfReplaceAll])])
    else
      ValueStr := VarToStr(AValues[I]);

    Args := Args + Format('''%s'', %s', [AKeys[I], ValueStr]);
  end;

  V := FConnection.ExecuteScalar(Format('SELECT json_object(%s)', [Args]));

  if VarIsNull(V) then
    Result := '{}'
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONArray(const AValues: array of Variant): string;
var
  V: Variant;
  Args: string;
  I: Integer;
  ValueStr: string;
begin
  Args := '';
  for I := Low(AValues) to High(AValues) do
  begin
    if Args <> '' then
      Args := Args + ', ';

    if VarIsNull(AValues[I]) then
      ValueStr := 'null'
    else if VarType(AValues[I]) = varString then
      ValueStr := Format('''%s''', [StringReplace(VarToStr(AValues[I]), '''', '''''', [rfReplaceAll])])
    else
      ValueStr := VarToStr(AValues[I]);

    Args := Args + ValueStr;
  end;

  V := FConnection.ExecuteScalar(Format('SELECT json_array(%s)', [Args]));

  if VarIsNull(V) then
    Result := '[]'
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONQuote(const AValue: string): string;
var
  V: Variant;
begin
  V := FConnection.ExecuteScalar(Format('SELECT json_quote(''%s'')',
    [StringReplace(AValue, '''', '''''', [rfReplaceAll])]));

  if VarIsNull(V) then
    Result := '""'
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONGroupArray(const ATableName, AColumn: string;
  const AWhereClause: string): string;
var
  SQL: string;
  V: Variant;
begin
  SQL := Format('SELECT json_group_array(%s) FROM %s', [AColumn, ATableName]);
  if AWhereClause <> '' then
    SQL := SQL + ' WHERE ' + AWhereClause;

  V := FConnection.ExecuteScalar(SQL);

  if VarIsNull(V) then
    Result := '[]'
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.JSONGroupObject(const ATableName, AKeyColumn, AValueColumn: string;
  const AWhereClause: string): string;
var
  SQL: string;
  V: Variant;
begin
  SQL := Format('SELECT json_group_object(%s, %s) FROM %s',
    [AKeyColumn, AValueColumn, ATableName]);
  if AWhereClause <> '' then
    SQL := SQL + ' WHERE ' + AWhereClause;

  V := FConnection.ExecuteScalar(SQL);

  if VarIsNull(V) then
    Result := '{}'
  else
    Result := VarToStr(V);
end;

function TNDXSQLiteJSON.QueryJSONPath(const ATableName, AJSONColumn, APath: string): TDataSet;
begin
  Result := FConnection.ExecuteQuery(Format(
    'SELECT *, json_extract(%s, ''%s'') AS json_value FROM %s',
    [AJSONColumn, APath, ATableName]));
end;

function TNDXSQLiteJSON.QueryJSONWhere(const ATableName, AJSONColumn, APath: string;
  const AValue: Variant): TDataSet;
var
  ValueStr: string;
begin
  if VarIsNull(AValue) then
    ValueStr := 'NULL'
  else if VarType(AValue) = varString then
    ValueStr := Format('''%s''', [StringReplace(VarToStr(AValue), '''', '''''', [rfReplaceAll])])
  else
    ValueStr := VarToStr(AValue);

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s WHERE json_extract(%s, ''%s'') = %s',
    [ATableName, AJSONColumn, APath, ValueStr]));
end;

function TNDXSQLiteJSON.QueryJSONContains(const ATableName, AJSONColumn: string;
  const AValue: Variant): TDataSet;
var
  ValueStr: string;
begin
  if VarIsNull(AValue) then
    ValueStr := 'null'
  else if VarType(AValue) = varString then
    ValueStr := Format('"%s"', [StringReplace(VarToStr(AValue), '"', '\"', [rfReplaceAll])])
  else
    ValueStr := VarToStr(AValue);

  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM %s, json_each(%s) WHERE json_each.value = json(''%s'')',
    [ATableName, AJSONColumn, ValueStr]));
end;

function TNDXSQLiteJSON.JSONToTable(const AJSON: string): TDataSet;
begin
  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM json_tree(''%s'')',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll])]));
end;

function TNDXSQLiteJSON.TableToJSON(const ATableName: string;
  const AWhereClause: string): string;
var
  SQL, Columns, JSONFields: string;
  DS: TDataSet;
  I: Integer;
begin
  // First get column names
  DS := FConnection.ExecuteQuery(Format(
    'PRAGMA table_info(%s)', [ATableName]));
  try
    Columns := '';
    JSONFields := '';
    while not DS.EOF do
    begin
      if Columns <> '' then
      begin
        Columns := Columns + ', ';
        JSONFields := JSONFields + ', ';
      end;
      Columns := Columns + DS.FieldByName('name').AsString;
      JSONFields := JSONFields + Format('''%s'', %s',
        [DS.FieldByName('name').AsString, DS.FieldByName('name').AsString]);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  if Columns = '' then
  begin
    Result := '[]';
    Exit;
  end;

  // Build JSON query
  SQL := Format('SELECT json_group_array(json_object(%s)) FROM %s',
    [JSONFields, ATableName]);
  if AWhereClause <> '' then
    SQL := SQL + ' WHERE ' + AWhereClause;

  Result := VarToStr(FConnection.ExecuteScalar(SQL));
  if Result = '' then
    Result := '[]';
end;

function TNDXSQLiteJSON.QueryToJSON(const ASQL: string): string;
var
  DS: TDataSet;
  JSONFields: string;
  I: Integer;
  FieldName: string;
begin
  // Execute query to get field names
  DS := FConnection.ExecuteQuery(ASQL);
  try
    if DS.FieldCount = 0 then
    begin
      Result := '[]';
      Exit;
    end;

    // Build JSON fields list
    JSONFields := '';
    for I := 0 to DS.FieldCount - 1 do
    begin
      if JSONFields <> '' then
        JSONFields := JSONFields + ', ';
      FieldName := DS.Fields[I].FieldName;
      JSONFields := JSONFields + Format('''%s'', %s', [FieldName, FieldName]);
    end;
  finally
    DS.Free;
  end;

  // Now execute with json_group_array
  Result := VarToStr(FConnection.ExecuteScalar(Format(
    'SELECT json_group_array(json_object(%s)) FROM (%s)',
    [JSONFields, ASQL])));

  if Result = '' then
    Result := '[]';
end;

function TNDXSQLiteJSON.ExportTableToFile(const ATableName, AFilePath: string;
  const AWhereClause: string; APrettyPrint: Boolean): Boolean;
var
  JSONStr: string;
  F: TextFile;
begin
  Result := False;
  try
    JSONStr := TableToJSON(ATableName, AWhereClause);

    if APrettyPrint then
      JSONStr := FormatJSONPretty(JSONStr);

    AssignFile(F, AFilePath);
    Rewrite(F);
    try
      Write(F, JSONStr);
    finally
      CloseFile(F);
    end;
    Result := True;
  except
    // Return False on error
  end;
end;

function TNDXSQLiteJSON.ExportQueryToFile(const ASQL, AFilePath: string;
  APrettyPrint: Boolean): Boolean;
var
  JSONStr: string;
  F: TextFile;
begin
  Result := False;
  try
    JSONStr := QueryToJSON(ASQL);

    if APrettyPrint then
      JSONStr := FormatJSONPretty(JSONStr);

    AssignFile(F, AFilePath);
    Rewrite(F);
    try
      Write(F, JSONStr);
    finally
      CloseFile(F);
    end;
    Result := True;
  except
    // Return False on error
  end;
end;

function FormatJSONPretty(const AJSON: string): string;
var
  I, Indent: Integer;
  InString: Boolean;
  Ch: Char;
begin
  Result := '';
  Indent := 0;
  InString := False;

  for I := 1 to Length(AJSON) do
  begin
    Ch := AJSON[I];

    if Ch = '"' then
    begin
      if (I = 1) or (AJSON[I-1] <> '\') then
        InString := not InString;
      Result := Result + Ch;
    end
    else if not InString then
    begin
      case Ch of
        '{', '[':
        begin
          Result := Result + Ch + sLineBreak;
          Inc(Indent);
          Result := Result + StringOfChar(' ', Indent * 2);
        end;
        '}', ']':
        begin
          Result := Result + sLineBreak;
          Dec(Indent);
          Result := Result + StringOfChar(' ', Indent * 2) + Ch;
        end;
        ',':
        begin
          Result := Result + Ch + sLineBreak;
          Result := Result + StringOfChar(' ', Indent * 2);
        end;
        ':':
          Result := Result + ': ';
        ' ', #9, #10, #13:
          ; // Skip whitespace
        else
          Result := Result + Ch;
      end;
    end
    else
      Result := Result + Ch;
  end;
end;

function TNDXSQLiteJSON.JSONArrayLength(const AJSON: string; const APath: string): Integer;
var
  V: Variant;
begin
  V := FConnection.ExecuteScalar(Format(
    'SELECT json_array_length(''%s'', ''%s'')',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll]), APath]));

  if VarIsNull(V) then
    Result := 0
  else
    Result := V;
end;

function TNDXSQLiteJSON.JSONKeys(const AJSON: string): TStringList;
var
  DS: TDataSet;
begin
  Result := TStringList.Create;
  try
    DS := FConnection.ExecuteQuery(Format(
      'SELECT key FROM json_each(''%s'') WHERE type != ''null''',
      [StringReplace(AJSON, '''', '''''', [rfReplaceAll])]));
    try
      while not DS.EOF do
      begin
        Result.Add(DS.Fields[0].AsString);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
  except
    // Return empty list on error
  end;
end;

function TNDXSQLiteJSON.JSONEach(const AJSON: string): TDataSet;
begin
  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM json_each(''%s'')',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll])]));
end;

function TNDXSQLiteJSON.JSONTree(const AJSON: string): TDataSet;
begin
  Result := FConnection.ExecuteQuery(Format(
    'SELECT * FROM json_tree(''%s'')',
    [StringReplace(AJSON, '''', '''''', [rfReplaceAll])]));
end;

end.
