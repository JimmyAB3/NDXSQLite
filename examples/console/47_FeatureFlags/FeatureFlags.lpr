{===============================================================================
  NDXSQLite Example 47 - Feature Flags Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Feature toggle management
  - Percentage-based rollouts
  - User/group targeting
  - A/B testing support
  - Environment-based flags
  - Feature flag history

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program FeatureFlags;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, StrUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the feature_flags, targeting rules, user_groups, flag_history, and A/B test tables with indexes. }
procedure SetupFeatureFlags;
begin
  // Feature flags table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS feature_flags (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT UNIQUE NOT NULL,' +
    '  description TEXT,' +
    '  enabled INTEGER DEFAULT 0,' +
    '  rollout_percentage INTEGER DEFAULT 100,' +  // 0-100
    '  environment TEXT DEFAULT ''all'',' +        // all, dev, staging, prod
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now'')),' +
    '  created_by TEXT,' +
    '  expires_at TEXT' +                          // Optional expiration
    ')');

  // User targeting rules
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS flag_user_rules (' +
    '  id INTEGER PRIMARY KEY,' +
    '  flag_id INTEGER REFERENCES feature_flags(id) ON DELETE CASCADE,' +
    '  user_id TEXT NOT NULL,' +
    '  enabled INTEGER NOT NULL,' +               // Override: 1=force on, 0=force off
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Group targeting rules
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS flag_group_rules (' +
    '  id INTEGER PRIMARY KEY,' +
    '  flag_id INTEGER REFERENCES feature_flags(id) ON DELETE CASCADE,' +
    '  group_name TEXT NOT NULL,' +               // e.g., "beta_testers", "premium"
    '  enabled INTEGER NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // User groups
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS user_groups (' +
    '  user_id TEXT NOT NULL,' +
    '  group_name TEXT NOT NULL,' +
    '  PRIMARY KEY (user_id, group_name)' +
    ')');

  // Flag change history
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS flag_history (' +
    '  id INTEGER PRIMARY KEY,' +
    '  flag_id INTEGER,' +
    '  flag_name TEXT,' +
    '  action TEXT,' +                            // created, enabled, disabled, rollout_changed, deleted
    '  old_value TEXT,' +
    '  new_value TEXT,' +
    '  changed_by TEXT,' +
    '  timestamp TEXT DEFAULT (datetime(''now''))' +
    ')');

  // A/B test variants
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS ab_test_variants (' +
    '  id INTEGER PRIMARY KEY,' +
    '  flag_id INTEGER REFERENCES feature_flags(id) ON DELETE CASCADE,' +
    '  variant_name TEXT NOT NULL,' +
    '  weight INTEGER DEFAULT 50,' +              // Weight for distribution
    '  config TEXT' +                             // JSON configuration for variant
    ')');

  // User variant assignments (sticky)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS user_variants (' +
    '  user_id TEXT NOT NULL,' +
    '  flag_id INTEGER NOT NULL,' +
    '  variant_name TEXT NOT NULL,' +
    '  assigned_at TEXT DEFAULT (datetime(''now'')),' +
    '  PRIMARY KEY (user_id, flag_id)' +
    ')');

  // Indexes
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_flags_name ON feature_flags(name)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_flags_env ON feature_flags(environment)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_user_rules ON flag_user_rules(flag_id, user_id)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_group_rules ON flag_group_rules(flag_id, group_name)');
end;

// ============================================================================
// Feature flag operations
// ============================================================================

{ Inserts a new feature flag with rollout and environment settings and records the creation in history. }
function CreateFlag(const AName, ADescription: string; AEnabled: Boolean = False;
  ARolloutPercentage: Integer = 100; const AEnvironment: string = 'all'): Integer;
var
  DS: TDataSet;
begin
  Connection.ExecuteNonQuery(
    'INSERT INTO feature_flags (name, description, enabled, rollout_percentage, environment, created_by) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [AName, ADescription, Ord(AEnabled), ARolloutPercentage, AEnvironment, 'admin']);

  DS := Connection.ExecuteQuery('SELECT last_insert_rowid()');
  try
    Result := DS.Fields[0].AsInteger;
  finally
    DS.Free;
  end;

  Connection.ExecuteNonQuery(
    'INSERT INTO flag_history (flag_id, flag_name, action, new_value, changed_by) VALUES (?, ?, ?, ?, ?)',
    [Result, AName, 'created', BoolToStr(AEnabled, 'enabled', 'disabled'), 'admin']);
end;

{ Toggles a feature flag's enabled state and records the change in the history table. }
procedure SetFlagEnabled(const AName: string; AEnabled: Boolean);
var
  DS: TDataSet;
  FlagId: Integer;
  OldValue: string;
begin
  DS := Connection.ExecuteQuery('SELECT id, enabled FROM feature_flags WHERE name = ?', [AName]);
  try
    if DS.EOF then Exit;
    FlagId := DS.FieldByName('id').AsInteger;
    OldValue := BoolToStr(DS.FieldByName('enabled').AsInteger = 1, 'enabled', 'disabled');
  finally
    DS.Free;
  end;

  Connection.ExecuteNonQuery(
    'UPDATE feature_flags SET enabled = ?, updated_at = datetime(''now'') WHERE name = ?',
    [Ord(AEnabled), AName]);

  Connection.ExecuteNonQuery(
    'INSERT INTO flag_history (flag_id, flag_name, action, old_value, new_value, changed_by) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [FlagId, AName, IfThen(AEnabled, 'enabled', 'disabled'), OldValue,
     BoolToStr(AEnabled, 'enabled', 'disabled'), 'admin']);
end;

{ Updates a flag's rollout percentage and logs the old and new values in the history table. }
procedure SetRolloutPercentage(const AName: string; APercentage: Integer);
var
  DS: TDataSet;
  FlagId: Integer;
  OldPercentage: string;
begin
  DS := Connection.ExecuteQuery('SELECT id, rollout_percentage FROM feature_flags WHERE name = ?', [AName]);
  try
    if DS.EOF then Exit;
    FlagId := DS.FieldByName('id').AsInteger;
    OldPercentage := DS.FieldByName('rollout_percentage').AsString + '%';
  finally
    DS.Free;
  end;

  Connection.ExecuteNonQuery(
    'UPDATE feature_flags SET rollout_percentage = ?, updated_at = datetime(''now'') WHERE name = ?',
    [APercentage, AName]);

  Connection.ExecuteNonQuery(
    'INSERT INTO flag_history (flag_id, flag_name, action, old_value, new_value, changed_by) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    [FlagId, AName, 'rollout_changed', OldPercentage,
     IntToStr(APercentage) + '%', 'admin']);
end;

{ Inserts or replaces a per-user override rule for a specific feature flag. }
procedure AddUserRule(const AFlagName, AUserId: string; AEnabled: Boolean);
var
  DS: TDataSet;
  FlagId: Integer;
begin
  DS := Connection.ExecuteQuery('SELECT id FROM feature_flags WHERE name = ?', [AFlagName]);
  try
    if DS.EOF then Exit;
    FlagId := DS.FieldByName('id').AsInteger;
  finally
    DS.Free;
  end;
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO flag_user_rules (flag_id, user_id, enabled) VALUES (?, ?, ?)',
    [FlagId, AUserId, Ord(AEnabled)]);
end;

{ Inserts or replaces a group-level override rule for a specific feature flag. }
procedure AddGroupRule(const AFlagName, AGroupName: string; AEnabled: Boolean);
var
  DS: TDataSet;
  FlagId: Integer;
begin
  DS := Connection.ExecuteQuery('SELECT id FROM feature_flags WHERE name = ?', [AFlagName]);
  try
    if DS.EOF then Exit;
    FlagId := DS.FieldByName('id').AsInteger;
  finally
    DS.Free;
  end;
  Connection.ExecuteNonQuery(
    'INSERT OR REPLACE INTO flag_group_rules (flag_id, group_name, enabled) VALUES (?, ?, ?)',
    [FlagId, AGroupName, Ord(AEnabled)]);
end;

{ Associates a user with a targeting group, ignoring duplicates. }
procedure AddUserToGroup(const AUserId, AGroupName: string);
begin
  Connection.ExecuteNonQuery(
    'INSERT OR IGNORE INTO user_groups (user_id, group_name) VALUES (?, ?)',
    [AUserId, AGroupName]);
end;

{ Checks whether a feature flag is enabled for a given user and environment. }
function IsFeatureEnabled(const AFlagName, AUserId, AEnvironment: string): Boolean;
var
  DS: TDataSet;
  FlagId: Integer;
  BaseEnabled: Boolean;
  RolloutPercentage: Integer;
  UserHash: Cardinal;
  I: Integer;
begin
  Result := False;

  // Get flag info
  DS := Connection.ExecuteQuery(
    'SELECT id, enabled, rollout_percentage, environment, expires_at FROM feature_flags WHERE name = ?',
    [AFlagName]);
  try
    if DS.EOF then Exit;

    FlagId := DS.FieldByName('id').AsInteger;
    BaseEnabled := DS.FieldByName('enabled').AsInteger = 1;
    RolloutPercentage := DS.FieldByName('rollout_percentage').AsInteger;

    // Check environment
    if (DS.FieldByName('environment').AsString <> 'all') and
       (DS.FieldByName('environment').AsString <> AEnvironment) then
      Exit;

    // Check expiration
    if not DS.FieldByName('expires_at').IsNull then
      if DS.FieldByName('expires_at').AsString < FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) then
        Exit;
  finally
    DS.Free;
  end;

  if not BaseEnabled then Exit;

  // Check user-specific rule (highest priority)
  DS := Connection.ExecuteQuery(
    'SELECT enabled FROM flag_user_rules WHERE flag_id = ? AND user_id = ?',
    [FlagId, AUserId]);
  try
    if not DS.EOF then
    begin
      Result := DS.FieldByName('enabled').AsInteger = 1;
      Exit;
    end;
  finally
    DS.Free;
  end;

  // Check group rules
  DS := Connection.ExecuteQuery(
    'SELECT gr.enabled FROM flag_group_rules gr ' +
    'JOIN user_groups ug ON gr.group_name = ug.group_name ' +
    'WHERE gr.flag_id = ? AND ug.user_id = ? LIMIT 1',
    [FlagId, AUserId]);
  try
    if not DS.EOF then
    begin
      Result := DS.FieldByName('enabled').AsInteger = 1;
      Exit;
    end;
  finally
    DS.Free;
  end;

  // Check percentage rollout (deterministic based on user ID)
  if RolloutPercentage < 100 then
  begin
    UserHash := 0;
    for I := 1 to Length(AUserId) do
      UserHash := ((UserHash shl 5) + UserHash) + Ord(AUserId[I]);
    Result := (UserHash mod 100) < Cardinal(RolloutPercentage);
  end
  else
    Result := True;
end;

// ============================================================================
// Demo procedures
// ============================================================================

{ Creates several feature flags and checks their enabled state for a sample user. }
procedure DemoBasicFlags;
begin
  WriteLn('1. Basic feature flags');
  WriteLn('   -------------------');
  WriteLn('');

  // Create some flags
  WriteLn('   Creating feature flags...');
  CreateFlag('dark_mode', 'Enable dark mode UI', True);
  CreateFlag('new_checkout', 'New checkout flow', False);
  CreateFlag('ai_recommendations', 'AI-powered recommendations', True);
  CreateFlag('beta_features', 'Beta feature access', False);

  WriteLn('');

  // Check flags
  WriteLn('   Checking flags for user "alice":');
  WriteLn('   - dark_mode: ', IsFeatureEnabled('dark_mode', 'alice', 'prod'));
  WriteLn('   - new_checkout: ', IsFeatureEnabled('new_checkout', 'alice', 'prod'));
  WriteLn('   - ai_recommendations: ', IsFeatureEnabled('ai_recommendations', 'alice', 'prod'));

  WriteLn('');
end;

{ Demonstrates gradual percentage-based feature rollouts. }
procedure DemoRollouts;
var
  I, EnabledCount: Integer;
  UserId: string;
begin
  WriteLn('2. Percentage-based rollouts');
  WriteLn('   -------------------------');
  WriteLn('');

  // Create a flag with 30% rollout
  CreateFlag('new_ui', 'New user interface', True, 30);
  WriteLn('   Created "new_ui" flag with 30% rollout');
  WriteLn('');

  // Test with multiple users
  WriteLn('   Testing with 20 users:');
  EnabledCount := 0;
  for I := 1 to 20 do
  begin
    UserId := 'user_' + IntToStr(I);
    if IsFeatureEnabled('new_ui', UserId, 'prod') then
    begin
      Inc(EnabledCount);
      Write('   ', UserId, ': ON  ');
    end
    else
      Write('   ', UserId, ': OFF ');
    if I mod 4 = 0 then WriteLn('');
  end;
  WriteLn('');
  WriteLn('   Enabled for ', EnabledCount, '/20 users (~', (EnabledCount * 100) div 20, '%)');

  WriteLn('');

  // Increase rollout
  WriteLn('   Increasing rollout to 70%...');
  SetRolloutPercentage('new_ui', 70);
  EnabledCount := 0;
  for I := 1 to 20 do
    if IsFeatureEnabled('new_ui', 'user_' + IntToStr(I), 'prod') then
      Inc(EnabledCount);
  WriteLn('   Now enabled for ', EnabledCount, '/20 users (~', (EnabledCount * 100) div 20, '%)');

  WriteLn('');
end;

{ Creates a flag with per-user override rules and verifies targeting for individual users. }
procedure DemoUserTargeting;
begin
  WriteLn('3. User targeting');
  WriteLn('   --------------');
  WriteLn('');

  // Create flag that's disabled by default
  CreateFlag('premium_feature', 'Premium only feature', False);
  WriteLn('   Created "premium_feature" (disabled by default)');

  // Add specific user rules
  AddUserRule('premium_feature', 'premium_user_1', True);
  AddUserRule('premium_feature', 'premium_user_2', True);
  AddUserRule('premium_feature', 'blocked_user', False);

  WriteLn('   Added user rules:');
  WriteLn('   - premium_user_1: enabled');
  WriteLn('   - premium_user_2: enabled');
  WriteLn('   - blocked_user: disabled');
  WriteLn('');

  // Also enable the flag globally so rules take effect
  SetFlagEnabled('premium_feature', True);

  // Test
  WriteLn('   Testing:');
  WriteLn('   - premium_user_1: ', IsFeatureEnabled('premium_feature', 'premium_user_1', 'prod'));
  WriteLn('   - premium_user_2: ', IsFeatureEnabled('premium_feature', 'premium_user_2', 'prod'));
  WriteLn('   - regular_user: ', IsFeatureEnabled('premium_feature', 'regular_user', 'prod'));
  WriteLn('   - blocked_user: ', IsFeatureEnabled('premium_feature', 'blocked_user', 'prod'));

  WriteLn('');
end;

{ Creates a flag with a group rule, assigns users to the group, and tests group-based access. }
procedure DemoGroupTargeting;
begin
  WriteLn('4. Group targeting');
  WriteLn('   ---------------');
  WriteLn('');

  // Create flag for beta testers
  CreateFlag('experimental_feature', 'Experimental feature', True, 0);  // 0% rollout by default
  WriteLn('   Created "experimental_feature" (0% rollout)');

  // Add group rule
  AddGroupRule('experimental_feature', 'beta_testers', True);
  WriteLn('   Added group rule: beta_testers = enabled');
  WriteLn('');

  // Add users to groups
  AddUserToGroup('tester_alice', 'beta_testers');
  AddUserToGroup('tester_bob', 'beta_testers');
  WriteLn('   Added tester_alice, tester_bob to beta_testers group');
  WriteLn('');

  // Test
  WriteLn('   Testing:');
  WriteLn('   - tester_alice (beta_tester): ', IsFeatureEnabled('experimental_feature', 'tester_alice', 'prod'));
  WriteLn('   - tester_bob (beta_tester): ', IsFeatureEnabled('experimental_feature', 'tester_bob', 'prod'));
  WriteLn('   - regular_user (no group): ', IsFeatureEnabled('experimental_feature', 'regular_user', 'prod'));

  WriteLn('');
end;

{ Demonstrates environment-based feature flag configuration. }
procedure DemoEnvironments;
begin
  WriteLn('5. Environment-based flags');
  WriteLn('   -----------------------');
  WriteLn('');

  // Create environment-specific flags
  CreateFlag('debug_mode', 'Debug mode logging', True, 100, 'dev');
  CreateFlag('staging_banner', 'Show staging banner', True, 100, 'staging');
  CreateFlag('prod_analytics', 'Production analytics', True, 100, 'prod');

  WriteLn('   Created environment-specific flags');
  WriteLn('');

  WriteLn('   Testing debug_mode:');
  WriteLn('   - in dev: ', IsFeatureEnabled('debug_mode', 'user1', 'dev'));
  WriteLn('   - in staging: ', IsFeatureEnabled('debug_mode', 'user1', 'staging'));
  WriteLn('   - in prod: ', IsFeatureEnabled('debug_mode', 'user1', 'prod'));

  WriteLn('');

  WriteLn('   Testing prod_analytics:');
  WriteLn('   - in dev: ', IsFeatureEnabled('prod_analytics', 'user1', 'dev'));
  WriteLn('   - in prod: ', IsFeatureEnabled('prod_analytics', 'user1', 'prod'));

  WriteLn('');
end;

{ Toggles flags and rollout percentages, then queries and displays the change history log. }
procedure DemoFlagHistory;
var
  DS: TDataSet;
begin
  WriteLn('6. Flag change history');
  WriteLn('   -------------------');
  WriteLn('');

  // Make some changes
  SetFlagEnabled('new_checkout', True);
  SetRolloutPercentage('new_checkout', 50);
  SetFlagEnabled('new_checkout', False);

  DS := Connection.ExecuteQuery(
    'SELECT flag_name, action, old_value, new_value, timestamp ' +
    'FROM flag_history ORDER BY timestamp DESC LIMIT 10');
  try
    WriteLn('   Recent flag changes:');
    while not DS.EOF do
    begin
      Write('   [', DS.FieldByName('timestamp').AsString, '] ');
      Write(DS.FieldByName('flag_name').AsString:20, ' ');
      Write(DS.FieldByName('action').AsString:15);
      if not DS.FieldByName('old_value').IsNull then
        Write(' (', DS.FieldByName('old_value').AsString, ' -> ',
              DS.FieldByName('new_value').AsString, ')');
      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Queries all feature flags and displays them in a dashboard table with status, rollout, and environment. }
procedure DemoFlagListing;
var
  DS: TDataSet;
begin
  WriteLn('7. Feature flag dashboard');
  WriteLn('   ----------------------');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT name, description, enabled, rollout_percentage, environment ' +
    'FROM feature_flags ORDER BY name');
  try
    WriteLn('   Flag Name              Enabled  Rollout  Environment');
    WriteLn('   ---------              -------  -------  -----------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:22,
              IfThen(DS.FieldByName('enabled').AsInteger = 1, 'YES', 'NO'):8,
              DS.FieldByName('rollout_percentage').AsString + '%':8,
              '  ', DS.FieldByName('environment').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the database file and its WAL/SHM journal files from disk. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  Randomize;
  WriteLn('=== NDXSQLite Example 47: Feature Flags Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example47.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupFeatureFlags;
      WriteLn('Feature flags database initialized');
      WriteLn('');

      DemoBasicFlags;
      DemoRollouts;
      DemoUserTargeting;
      DemoGroupTargeting;
      DemoEnvironments;
      DemoFlagHistory;
      DemoFlagListing;

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
