{===============================================================================
  NDXSQLite Example 39 - Configuration Store
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Using SQLite as a configuration/settings store
  - Key-value storage with typed values
  - Hierarchical configuration (sections)
  - Default values and validation
  - Configuration versioning
  - Import/Export configuration

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ConfigurationStore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the config table with typed key-value entries, a config_history table
  for auditing changes, an update trigger that logs old/new values, and a section index. }
procedure SetupConfigStore;
begin
  // Main configuration table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS config (' +
    '  key TEXT PRIMARY KEY,' +
    '  value TEXT,' +
    '  value_type TEXT DEFAULT ''string'',' +  // string, integer, float, boolean, json
    '  section TEXT DEFAULT ''general'',' +
    '  description TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Configuration history for auditing
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS config_history (' +
    '  id INTEGER PRIMARY KEY,' +
    '  key TEXT,' +
    '  old_value TEXT,' +
    '  new_value TEXT,' +
    '  changed_at TEXT DEFAULT (datetime(''now'')),' +
    '  changed_by TEXT' +
    ')');

  // Trigger to track changes
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER IF NOT EXISTS config_update_trigger ' +
    'AFTER UPDATE ON config ' +
    'BEGIN ' +
    '  INSERT INTO config_history (key, old_value, new_value, changed_by) ' +
    '  VALUES (OLD.key, OLD.value, NEW.value, ''system''); ' +
    '  UPDATE config SET updated_at = datetime(''now'') WHERE key = NEW.key; ' +
    'END');

  // Create indexes
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_config_section ON config(section)');
end;

// Helper functions to get/set config values
{ Retrieves a string configuration value for the given key, returning ADefault if the key does not exist. }
function GetConfigString(const AKey: string; const ADefault: string = ''): string;
var
  DS: TDataSet;
begin
  Result := ADefault;
  DS := Connection.ExecuteQuery(
    'SELECT value FROM config WHERE key = ?', [AKey]);
  try
    if not DS.EOF then
      Result := DS.Fields[0].AsString;
  finally
    DS.Free;
  end;
end;

{ Retrieves an integer configuration value for the given key, returning ADefault if the key is missing or not a valid integer. }
function GetConfigInteger(const AKey: string; const ADefault: Integer = 0): Integer;
var
  S: string;
begin
  S := GetConfigString(AKey, '');
  if S = '' then
    Result := ADefault
  else
    Result := StrToIntDef(S, ADefault);
end;

{ Retrieves a boolean configuration value for the given key, interpreting 'true', '1', or 'yes' as True. }
function GetConfigBoolean(const AKey: string; const ADefault: Boolean = False): Boolean;
var
  S: string;
begin
  S := LowerCase(GetConfigString(AKey, ''));
  if S = '' then
    Result := ADefault
  else
    Result := (S = 'true') or (S = '1') or (S = 'yes');
end;

{ Retrieves a floating-point configuration value for the given key, returning ADefault if the key is missing or not a valid number. }
function GetConfigFloat(const AKey: string; const ADefault: Double = 0.0): Double;
var
  S: string;
begin
  S := GetConfigString(AKey, '');
  if S = '' then
    Result := ADefault
  else
    Result := StrToFloatDef(S, ADefault);
end;

{ Inserts or updates a configuration entry with the given key, value, type, section, and description using upsert. }
procedure SetConfig(const AKey, AValue: string; const AType: string = 'string';
  const ASection: string = 'general'; const ADescription: string = '');
begin
  Connection.ExecuteNonQuery(
    'INSERT INTO config (key, value, value_type, section, description) ' +
    'VALUES (?, ?, ?, ?, ?) ' +
    'ON CONFLICT(key) DO UPDATE SET value = excluded.value',
    [AKey, AValue, AType, ASection, ADescription]);
end;

{ Stores an integer configuration value by converting it to a string and delegating to SetConfig with type 'integer'. }
procedure SetConfigInteger(const AKey: string; AValue: Integer;
  const ASection: string = 'general');
begin
  SetConfig(AKey, IntToStr(AValue), 'integer', ASection);
end;

{ Stores a boolean configuration value as 'true' or 'false' string with type 'boolean'. }
procedure SetConfigBoolean(const AKey: string; AValue: Boolean;
  const ASection: string = 'general');
begin
  if AValue then
    SetConfig(AKey, 'true', 'boolean', ASection)
  else
    SetConfig(AKey, 'false', 'boolean', ASection);
end;

{ Stores a floating-point configuration value by converting it to a string and delegating to SetConfig with type 'float'. }
procedure SetConfigFloat(const AKey: string; AValue: Double;
  const ASection: string = 'general');
begin
  SetConfig(AKey, FloatToStr(AValue), 'float', ASection);
end;

{ Stores and retrieves configuration values of various types (string, integer, boolean, float) and shows default value fallback for missing keys. }
procedure DemoBasicKeyValue;
begin
  WriteLn('1. Basic key-value storage');
  WriteLn('   -----------------------');

  // Set some configuration values
  SetConfig('app.name', 'My Application', 'string', 'general', 'Application name');
  SetConfig('app.version', '1.0.0', 'string', 'general', 'Current version');
  SetConfigInteger('app.max_connections', 100, 'general');
  SetConfigBoolean('app.debug_mode', True, 'general');
  SetConfigFloat('app.timeout', 30.5, 'general');

  // Read them back
  WriteLn('   app.name: ', GetConfigString('app.name'));
  WriteLn('   app.version: ', GetConfigString('app.version'));
  WriteLn('   app.max_connections: ', GetConfigInteger('app.max_connections'));
  WriteLn('   app.debug_mode: ', GetConfigBoolean('app.debug_mode'));
  WriteLn('   app.timeout: ', GetConfigFloat('app.timeout'):0:1);

  // Default values for missing keys
  WriteLn('   missing.key (default): ', GetConfigString('missing.key', 'default_value'));

  WriteLn('');
end;

{ Demonstrates organizing configuration entries by logical sections. }
procedure DemoSections;
var
  DS: TDataSet;
begin
  WriteLn('2. Configuration sections');
  WriteLn('   ----------------------');

  // Database settings
  SetConfig('db.host', 'localhost', 'string', 'database');
  SetConfigInteger('db.port', 5432, 'database');
  SetConfig('db.name', 'myapp', 'string', 'database');
  SetConfigBoolean('db.ssl', True, 'database');

  // UI settings
  SetConfig('ui.theme', 'dark', 'string', 'ui');
  SetConfigInteger('ui.font_size', 14, 'ui');
  SetConfig('ui.language', 'en', 'string', 'ui');

  // List all sections
  WriteLn('   Available sections:');
  DS := Connection.ExecuteQuery(
    'SELECT section, COUNT(*) as count FROM config GROUP BY section ORDER BY section');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('section').AsString,
              ' (', DS.FieldByName('count').AsInteger, ' keys)');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Get all keys in a section
  WriteLn('   Database section settings:');
  DS := Connection.ExecuteQuery(
    'SELECT key, value, value_type FROM config WHERE section = ? ORDER BY key',
    ['database']);
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('key').AsString:15, ' = ',
              DS.FieldByName('value').AsString,
              ' (', DS.FieldByName('value_type').AsString, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Updates a configuration key multiple times and queries the config_history table to display the recorded old and new values with timestamps. }
procedure DemoConfigHistory;
var
  DS: TDataSet;
begin
  WriteLn('3. Configuration change history');
  WriteLn('   ----------------------------');

  // Make some changes
  WriteLn('   Making changes to app.version...');
  SetConfig('app.version', '1.0.1', 'string', 'general');
  SetConfig('app.version', '1.1.0', 'string', 'general');
  SetConfig('app.version', '2.0.0', 'string', 'general');

  // View history
  WriteLn('');
  WriteLn('   Change history for app.version:');
  DS := Connection.ExecuteQuery(
    'SELECT old_value, new_value, changed_at FROM config_history ' +
    'WHERE key = ? ORDER BY changed_at DESC LIMIT 5',
    ['app.version']);
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('changed_at').AsString,
              ': ', DS.FieldByName('old_value').AsString,
              ' -> ', DS.FieldByName('new_value').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Stores dot-separated hierarchical keys (e.g., server.http.port) and queries them using LIKE prefix matching. }
procedure DemoHierarchicalKeys;
var
  DS: TDataSet;
begin
  WriteLn('4. Hierarchical key patterns');
  WriteLn('   -------------------------');

  // Store hierarchical settings
  SetConfig('server.http.port', '8080', 'integer', 'server');
  SetConfig('server.http.host', '0.0.0.0', 'string', 'server');
  SetConfig('server.http.timeout', '30', 'integer', 'server');
  SetConfig('server.https.port', '8443', 'integer', 'server');
  SetConfig('server.https.cert', '/path/to/cert.pem', 'string', 'server');
  SetConfig('server.https.key', '/path/to/key.pem', 'string', 'server');

  // Get all keys starting with a prefix
  WriteLn('   All server.http.* settings:');
  DS := Connection.ExecuteQuery(
    'SELECT key, value FROM config WHERE key LIKE ? ORDER BY key',
    ['server.http.%']);
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('key').AsString:25, ' = ',
              DS.FieldByName('value').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Stores JSON arrays and objects as configuration values, then extracts individual fields using JSON_EXTRACT and JSON_ARRAY_LENGTH. }
procedure DemoJsonConfig;
var
  DS: TDataSet;
begin
  WriteLn('5. JSON configuration values');
  WriteLn('   -------------------------');

  // Store complex configuration as JSON
  SetConfig('features.enabled',
    '["feature_a", "feature_b", "feature_c"]',
    'json', 'features', 'List of enabled features');

  SetConfig('email.smtp',
    '{"host": "smtp.example.com", "port": 587, "tls": true, "user": "sender@example.com"}',
    'json', 'email', 'SMTP configuration');

  // Read and parse JSON
  WriteLn('   Features enabled:');
  DS := Connection.ExecuteQuery(
    'SELECT value, ' +
    '       JSON_ARRAY_LENGTH(value) as feature_count ' +
    'FROM config WHERE key = ?',
    ['features.enabled']);
  try
    if not DS.EOF then
    begin
      WriteLn('   Raw: ', DS.FieldByName('value').AsString);
      WriteLn('   Count: ', DS.FieldByName('feature_count').AsInteger, ' features');
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Extract JSON fields
  WriteLn('   SMTP settings:');
  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  JSON_EXTRACT(value, ''$.host'') as host, ' +
    '  JSON_EXTRACT(value, ''$.port'') as port, ' +
    '  JSON_EXTRACT(value, ''$.tls'') as tls ' +
    'FROM config WHERE key = ?',
    ['email.smtp']);
  try
    if not DS.EOF then
    begin
      WriteLn('   Host: ', DS.FieldByName('host').AsString);
      WriteLn('   Port: ', DS.FieldByName('port').AsString);
      WriteLn('   TLS: ', DS.FieldByName('tls').AsString);
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Exports configuration entries from a section as a JSON object using JSON_GROUP_OBJECT with type-aware casting, and displays a per-section key count summary. }
procedure DemoExportImport;
var
  DS: TDataSet;
begin
  WriteLn('6. Export/Import configuration');
  WriteLn('   ---------------------------');

  // Export to JSON format
  WriteLn('   Exporting configuration as JSON:');
  DS := Connection.ExecuteQuery(
    'SELECT JSON_GROUP_OBJECT(key, ' +
    '  CASE value_type ' +
    '    WHEN ''integer'' THEN CAST(value AS INTEGER) ' +
    '    WHEN ''float'' THEN CAST(value AS REAL) ' +
    '    WHEN ''boolean'' THEN CASE WHEN value = ''true'' THEN JSON(''true'') ELSE JSON(''false'') END ' +
    '    WHEN ''json'' THEN JSON(value) ' +
    '    ELSE value ' +
    '  END' +
    ') as config_json ' +
    'FROM config WHERE section = ?',
    ['general']);
  try
    if not DS.EOF then
      WriteLn('   ', DS.FieldByName('config_json').AsString);
  finally
    DS.Free;
  end;

  WriteLn('');

  // Count configurations per section
  WriteLn('   Configuration summary:');
  DS := Connection.ExecuteQuery(
    'SELECT section, ' +
    '       COUNT(*) as key_count, ' +
    '       GROUP_CONCAT(key) as keys ' +
    'FROM config GROUP BY section ORDER BY section');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('section').AsString, '] ',
              DS.FieldByName('key_count').AsInteger, ' keys');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates a config_defaults table with min/max/valid value constraints and shows how to retrieve effective values using COALESCE fallback to defaults. }
procedure DemoDefaultsAndValidation;
var
  DS: TDataSet;
begin
  WriteLn('7. Defaults and validation');
  WriteLn('   -----------------------');

  // Create a defaults table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS config_defaults (' +
    '  key TEXT PRIMARY KEY,' +
    '  default_value TEXT,' +
    '  min_value TEXT,' +
    '  max_value TEXT,' +
    '  valid_values TEXT' +  // JSON array of valid values
    ')');

  // Define some defaults with validation
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO config_defaults VALUES ' +
    '(''app.log_level'', ''info'', NULL, NULL, ''["debug", "info", "warn", "error"]'')');
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO config_defaults VALUES ' +
    '(''app.workers'', ''4'', ''1'', ''32'', NULL)');

  // Get config with default fallback
  WriteLn('   Config with defaults:');
  DS := Connection.ExecuteQuery(
    'SELECT d.key, ' +
    '       COALESCE(c.value, d.default_value) as effective_value, ' +
    '       d.default_value, ' +
    '       CASE WHEN c.value IS NULL THEN ''(default)'' ELSE ''(custom)'' END as source ' +
    'FROM config_defaults d ' +
    'LEFT JOIN config c ON d.key = c.key');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('key').AsString:20, ' = ',
              DS.FieldByName('effective_value').AsString:10,
              ' ', DS.FieldByName('source').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Searches configuration entries by key or value substring match and lists the most recently modified settings ordered by update timestamp. }
procedure DemoSearchConfig;
var
  DS: TDataSet;
begin
  WriteLn('8. Search configuration');
  WriteLn('   --------------------');

  WriteLn('   All keys containing "port":');
  DS := Connection.ExecuteQuery(
    'SELECT key, value, section FROM config ' +
    'WHERE key LIKE ''%port%'' OR value LIKE ''%port%'' ' +
    'ORDER BY section, key');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('section').AsString, '] ',
              DS.FieldByName('key').AsString, ' = ',
              DS.FieldByName('value').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  WriteLn('   Recently modified settings:');
  DS := Connection.ExecuteQuery(
    'SELECT key, value, updated_at FROM config ' +
    'ORDER BY updated_at DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('updated_at').AsString, ' - ',
              DS.FieldByName('key').AsString);
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
  WriteLn('=== NDXSQLite Example 39: Configuration Store ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example39.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupConfigStore;
      WriteLn('Configuration store initialized');
      WriteLn('');

      DemoBasicKeyValue;
      DemoSections;
      DemoConfigHistory;
      DemoHierarchicalKeys;
      DemoJsonConfig;
      DemoExportImport;
      DemoDefaultsAndValidation;
      DemoSearchConfig;

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
