{===============================================================================
  NDXSQLite Example 41 - Cache Database with TTL
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Using SQLite as a cache store
  - Time-to-live (TTL) expiration
  - Cache statistics and hit rates
  - Automatic cleanup of expired entries
  - Cache size limits (LRU eviction)
  - Namespace/prefix support

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CacheDatabase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

const
  DEFAULT_TTL_SECONDS = 3600;  // 1 hour
  MAX_CACHE_SIZE = 1000;       // Maximum entries

{ Creates the cache table with TTL and access tracking columns, a cache_stats table
  for hit/miss/eviction counters, indexes on expires_at/namespace/last_accessed, and initializes default namespace stats. }
procedure SetupCache;
begin
  // Main cache table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS cache (' +
    '  key TEXT PRIMARY KEY,' +
    '  value TEXT,' +
    '  namespace TEXT DEFAULT ''default'',' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT,' +                    // NULL = never expires
    '  last_accessed TEXT DEFAULT (datetime(''now'')),' +
    '  access_count INTEGER DEFAULT 0,' +
    '  size_bytes INTEGER DEFAULT 0' +
    ')');

  // Cache statistics table
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS cache_stats (' +
    '  namespace TEXT PRIMARY KEY,' +
    '  hits INTEGER DEFAULT 0,' +
    '  misses INTEGER DEFAULT 0,' +
    '  sets INTEGER DEFAULT 0,' +
    '  evictions INTEGER DEFAULT 0' +
    ')');

  // Create indexes
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_cache_expires ON cache(expires_at)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_cache_namespace ON cache(namespace)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_cache_lru ON cache(last_accessed)');

  // Initialize default namespace stats
  Connection.ExecuteNonQuery(
    'INSERT OR IGNORE INTO cache_stats (namespace) VALUES (''default'')');
end;

{ Inserts or updates a cache entry with the given key, value, namespace, and TTL in seconds, and increments the namespace set counter. }
procedure CacheSet(const AKey, AValue: string; ATtlSeconds: Integer = DEFAULT_TTL_SECONDS;
  const ANamespace: string = 'default');
var
  ExpiresAt: string;
begin
  if ATtlSeconds > 0 then
    ExpiresAt := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now + ATtlSeconds / 86400)
  else
    ExpiresAt := '';

  Connection.ExecuteNonQuery(
    'INSERT INTO cache (key, value, namespace, expires_at, size_bytes) ' +
    'VALUES (?, ?, ?, NULLIF(?, ''''), ?) ' +
    'ON CONFLICT(key) DO UPDATE SET ' +
    '  value = excluded.value, ' +
    '  expires_at = excluded.expires_at, ' +
    '  last_accessed = datetime(''now''), ' +
    '  size_bytes = excluded.size_bytes',
    [AKey, AValue, ANamespace, ExpiresAt, Length(AValue)]);

  // Update stats
  Connection.ExecuteNonQuery(
    'INSERT INTO cache_stats (namespace, sets) VALUES (?, 1) ' +
    'ON CONFLICT(namespace) DO UPDATE SET sets = sets + 1',
    [ANamespace]);
end;

{ Retrieves a cached value by key, updating access statistics. }
function CacheGet(const AKey: string; const ANamespace: string = 'default'): string;
var
  DS: TDataSet;
  Found: Boolean;
begin
  Result := '';
  Found := False;

  // Get value if not expired
  DS := Connection.ExecuteQuery(
    'SELECT value FROM cache ' +
    'WHERE key = ? AND namespace = ? ' +
    '  AND (expires_at IS NULL OR expires_at > datetime(''now''))',
    [AKey, ANamespace]);
  try
    if not DS.EOF then
    begin
      Result := DS.Fields[0].AsString;
      Found := True;

      // Update access time and count
      Connection.ExecuteNonQuery(
        'UPDATE cache SET last_accessed = datetime(''now''), access_count = access_count + 1 ' +
        'WHERE key = ? AND namespace = ?',
        [AKey, ANamespace]);
    end;
  finally
    DS.Free;
  end;

  // Update stats
  if Found then
    Connection.ExecuteNonQuery(
      'UPDATE cache_stats SET hits = hits + 1 WHERE namespace = ?', [ANamespace])
  else
    Connection.ExecuteNonQuery(
      'UPDATE cache_stats SET misses = misses + 1 WHERE namespace = ?', [ANamespace]);
end;

{ Checks whether a cache entry exists and has not expired. }
function CacheExists(const AKey: string; const ANamespace: string = 'default'): Boolean;
var
  DS: TDataSet;
begin
  DS := Connection.ExecuteQuery(
    'SELECT 1 FROM cache ' +
    'WHERE key = ? AND namespace = ? ' +
    '  AND (expires_at IS NULL OR expires_at > datetime(''now''))',
    [AKey, ANamespace]);
  try
    Result := not DS.EOF;
  finally
    DS.Free;
  end;
end;

{ Removes a single cache entry identified by key and namespace from the cache table. }
procedure CacheDelete(const AKey: string; const ANamespace: string = 'default');
begin
  Connection.ExecuteNonQuery(
    'DELETE FROM cache WHERE key = ? AND namespace = ?',
    [AKey, ANamespace]);
end;

{ Deletes all cache entries, or only those in the specified namespace if one is provided. }
procedure CacheClear(const ANamespace: string = '');
begin
  if ANamespace = '' then
    Connection.ExecuteNonQuery('DELETE FROM cache')
  else
    Connection.ExecuteNonQuery('DELETE FROM cache WHERE namespace = ?', [ANamespace]);
end;

{ Removes expired cache entries and returns the number deleted. }
function CacheCleanupExpired: Integer;
var
  DS: TDataSet;
begin
  Connection.ExecuteNonQuery(
    'DELETE FROM cache WHERE expires_at IS NOT NULL AND expires_at <= datetime(''now'')');

  DS := Connection.ExecuteQuery('SELECT changes()');
  try
    Result := DS.Fields[0].AsInteger;
  finally
    DS.Free;
  end;
end;

{ Evicts least-recently-used entries when cache exceeds the maximum size. }
function CacheEvictLRU(AMaxEntries: Integer = MAX_CACHE_SIZE): Integer;
var
  DS: TDataSet;
  CurrentCount: Integer;
  ToEvict: Integer;
begin
  Result := 0;

  // Get current count
  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM cache');
  try
    CurrentCount := DS.Fields[0].AsInteger;
  finally
    DS.Free;
  end;

  if CurrentCount > AMaxEntries then
  begin
    ToEvict := CurrentCount - AMaxEntries;

    // Delete oldest accessed entries
    Connection.ExecuteNonQuery(
      'DELETE FROM cache WHERE key IN (' +
      '  SELECT key FROM cache ORDER BY last_accessed ASC LIMIT ?' +
      ')', [ToEvict]);

    DS := Connection.ExecuteQuery('SELECT changes()');
    try
      Result := DS.Fields[0].AsInteger;
    finally
      DS.Free;
    end;

    // Update eviction stats
    Connection.ExecuteNonQuery(
      'UPDATE cache_stats SET evictions = evictions + ?', [Result]);
  end;
end;

{ Stores several key-value pairs in the cache, retrieves them, checks existence, and shows that a missing key returns an empty string. }
procedure DemoBasicCaching;
begin
  WriteLn('1. Basic cache operations');
  WriteLn('   ----------------------');

  // Set some values
  CacheSet('user:1:name', 'Alice', 3600);
  CacheSet('user:1:email', 'alice@example.com', 3600);
  CacheSet('user:2:name', 'Bob', 3600);

  WriteLn('   Cached user:1:name = ', CacheGet('user:1:name'));
  WriteLn('   Cached user:1:email = ', CacheGet('user:1:email'));
  WriteLn('   Cached user:2:name = ', CacheGet('user:2:name'));

  // Check existence
  WriteLn('   user:1:name exists: ', CacheExists('user:1:name'));
  WriteLn('   user:3:name exists: ', CacheExists('user:3:name'));

  // Missing key returns empty
  WriteLn('   Missing key returns: "', CacheGet('nonexistent'), '"');

  WriteLn('');
end;

{ Inserts an already-expired entry and a valid entry, then shows that expired entries return empty on retrieval and lists expiration statuses. }
procedure DemoTtlExpiration;
var
  DS: TDataSet;
begin
  WriteLn('2. TTL expiration');
  WriteLn('   --------------');

  // Set with very short TTL (already expired)
  Connection.ExecuteNonQuery(
    'INSERT INTO cache (key, value, namespace, expires_at) ' +
    'VALUES (''temp:key1'', ''value1'', ''default'', datetime(''now'', ''-1 second''))');

  // Set with future expiration
  CacheSet('temp:key2', 'value2', 3600);  // 1 hour

  WriteLn('   temp:key1 (expired): "', CacheGet('temp:key1'), '"');
  WriteLn('   temp:key2 (valid): "', CacheGet('temp:key2'), '"');

  // Show expiration times
  WriteLn('');
  WriteLn('   Cache entries with expiration:');
  DS := Connection.ExecuteQuery(
    'SELECT key, expires_at, ' +
    '       CASE WHEN expires_at <= datetime(''now'') THEN ''EXPIRED'' ELSE ''VALID'' END as status ' +
    'FROM cache WHERE expires_at IS NOT NULL');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('key').AsString:15, ' ',
              DS.FieldByName('expires_at').AsString, ' ',
              DS.FieldByName('status').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates cache isolation using separate namespaces. }
procedure DemoNamespaces;
var
  DS: TDataSet;
begin
  WriteLn('3. Cache namespaces');
  WriteLn('   ----------------');

  // Initialize namespace stats
  Connection.ExecuteNonQuery('INSERT OR IGNORE INTO cache_stats (namespace) VALUES (''session'')');
  Connection.ExecuteNonQuery('INSERT OR IGNORE INTO cache_stats (namespace) VALUES (''api'')');

  // Set values in different namespaces
  CacheSet('token', 'abc123', 1800, 'session');
  CacheSet('token', 'xyz789', 300, 'api');
  CacheSet('user_id', '42', 1800, 'session');

  WriteLn('   session:token = ', CacheGet('token', 'session'));
  WriteLn('   api:token = ', CacheGet('token', 'api'));
  WriteLn('   session:user_id = ', CacheGet('user_id', 'session'));

  // Count by namespace
  WriteLn('');
  WriteLn('   Entries per namespace:');
  DS := Connection.ExecuteQuery(
    'SELECT namespace, COUNT(*) as count, SUM(size_bytes) as total_size ' +
    'FROM cache GROUP BY namespace');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('namespace').AsString:10, ': ',
              DS.FieldByName('count').AsInteger, ' entries, ',
              DS.FieldByName('total_size').AsInteger, ' bytes');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Generates cache hits and misses, then displays per-namespace statistics including hit count, miss count, sets, evictions, and hit rate percentage. }
procedure DemoCacheStats;
var
  DS: TDataSet;
begin
  WriteLn('4. Cache statistics');
  WriteLn('   ----------------');

  // Generate some activity
  CacheGet('user:1:name');
  CacheGet('user:1:name');
  CacheGet('user:1:name');
  CacheGet('nonexistent1');
  CacheGet('nonexistent2');

  DS := Connection.ExecuteQuery(
    'SELECT namespace, hits, misses, sets, evictions, ' +
    '       CASE WHEN hits + misses > 0 ' +
    '         THEN ROUND(100.0 * hits / (hits + misses), 1) ' +
    '         ELSE 0 END as hit_rate ' +
    'FROM cache_stats');
  try
    WriteLn('   Namespace   Hits  Miss  Sets  Evict  Hit%');
    WriteLn('   ---------   ----  ----  ----  -----  ----');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('namespace').AsString:10,
              DS.FieldByName('hits').AsInteger:5,
              DS.FieldByName('misses').AsInteger:6,
              DS.FieldByName('sets').AsInteger:6,
              DS.FieldByName('evictions').AsInteger:6,
              DS.FieldByName('hit_rate').AsFloat:6:1);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates cache cleanup with expiration and LRU eviction. }
procedure DemoCleanup;
var
  Expired, Evicted: Integer;
  DS: TDataSet;
begin
  WriteLn('5. Cache cleanup');
  WriteLn('   -------------');

  // Add some entries to test cleanup
  CacheSet('cleanup:1', 'data1', 3600);
  CacheSet('cleanup:2', 'data2', 3600);
  CacheSet('cleanup:3', 'data3', 3600);

  // Add expired entries
  Connection.ExecuteNonQuery(
    'INSERT INTO cache (key, value, expires_at) VALUES ' +
    '(''expired:1'', ''old1'', datetime(''now'', ''-1 hour'')), ' +
    '(''expired:2'', ''old2'', datetime(''now'', ''-2 hours''))');

  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM cache');
  try
    WriteLn('   Total entries before cleanup: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  // Cleanup expired
  Expired := CacheCleanupExpired;
  WriteLn('   Expired entries removed: ', Expired);

  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM cache');
  try
    WriteLn('   Total entries after cleanup: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');

  // Test LRU eviction with small limit
  WriteLn('   Testing LRU eviction (max 5 entries):');
  Evicted := CacheEvictLRU(5);
  WriteLn('   Entries evicted: ', Evicted);

  DS := Connection.ExecuteQuery('SELECT COUNT(*) FROM cache');
  try
    WriteLn('   Total entries after eviction: ', DS.Fields[0].AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Accesses cache entries multiple times and displays the most frequently accessed keys sorted by access count. }
procedure DemoAccessPatterns;
var
  DS: TDataSet;
begin
  WriteLn('6. Access patterns (most accessed)');
  WriteLn('   ------------------------------');

  // Access some entries multiple times
  CacheGet('user:1:name');
  CacheGet('user:1:name');
  CacheGet('user:1:name');
  CacheGet('user:1:email');
  CacheGet('user:1:email');
  CacheGet('user:2:name');

  DS := Connection.ExecuteQuery(
    'SELECT key, access_count, last_accessed ' +
    'FROM cache ' +
    'WHERE access_count > 0 ' +
    'ORDER BY access_count DESC LIMIT 5');
  try
    WriteLn('   Key                 Accesses  Last Access');
    WriteLn('   ---                 --------  -----------');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('key').AsString:20,
              DS.FieldByName('access_count').AsInteger:6, '    ',
              DS.FieldByName('last_accessed').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Stores JSON objects as cache values and extracts individual fields from them using JSON_EXTRACT queries. }
procedure DemoJsonCaching;
var
  DS: TDataSet;
  JsonValue: string;
begin
  WriteLn('7. Caching JSON data');
  WriteLn('   -----------------');

  // Cache complex objects as JSON
  CacheSet('product:1',
    '{"id": 1, "name": "Laptop", "price": 999.99, "stock": 50}',
    7200);
  CacheSet('product:2',
    '{"id": 2, "name": "Phone", "price": 599.99, "stock": 100}',
    7200);

  JsonValue := CacheGet('product:1');
  WriteLn('   Raw cached value: ', JsonValue);

  // Extract JSON fields
  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  JSON_EXTRACT(value, ''$.name'') as name, ' +
    '  JSON_EXTRACT(value, ''$.price'') as price, ' +
    '  JSON_EXTRACT(value, ''$.stock'') as stock ' +
    'FROM cache WHERE key = ?',
    ['product:1']);
  try
    if not DS.EOF then
    begin
      WriteLn('   Extracted: name=', DS.FieldByName('name').AsString,
              ', price=', DS.FieldByName('price').AsString,
              ', stock=', DS.FieldByName('stock').AsString);
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays overall cache metrics including total entries, total size in bytes, average entry size, and number of expired entries pending cleanup. }
procedure DemoCacheInfo;
var
  DS: TDataSet;
begin
  WriteLn('8. Cache information');
  WriteLn('   -----------------');

  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  COUNT(*) as total_entries, ' +
    '  SUM(size_bytes) as total_bytes, ' +
    '  AVG(size_bytes) as avg_size, ' +
    '  SUM(CASE WHEN expires_at IS NOT NULL AND expires_at <= datetime(''now'') THEN 1 ELSE 0 END) as expired ' +
    'FROM cache');
  try
    WriteLn('   Total entries: ', DS.FieldByName('total_entries').AsInteger);
    WriteLn('   Total size: ', DS.FieldByName('total_bytes').AsInteger, ' bytes');
    WriteLn('   Average entry size: ', DS.FieldByName('avg_size').AsFloat:0:1, ' bytes');
    WriteLn('   Expired (pending cleanup): ', DS.FieldByName('expired').AsInteger);
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
  WriteLn('=== NDXSQLite Example 41: Cache Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example41.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupCache;
      WriteLn('Cache database initialized');
      WriteLn('');

      DemoBasicCaching;
      DemoTtlExpiration;
      DemoNamespaces;
      DemoCacheStats;
      DemoCleanup;
      DemoAccessPatterns;
      DemoJsonCaching;
      DemoCacheInfo;

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
