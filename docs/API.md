# NDXSQLite API Reference

Complete API documentation for NDXSQLite 1.0.0.

---

## Table of Contents

- [Core Classes](#core-classes)
  - [TNDXSQLiteConnection](#tndxsqliteconnection)
  - [TNDXSQLiteConnectionOptions](#tndxsqliteconnectionoptions)
  - [INDXSQLiteConnection](#indxsqliteconnection)
- [Connection Management](#connection-management)
  - [TNDXSQLiteConnectionPool](#tndxsqliteconnectionpool)
  - [TNDXSQLiteConnectionFactory](#tndxsqliteconnectionfactory)
- [Asynchronous Operations](#asynchronous-operations)
  - [TNDXSQLiteAsyncConnection](#tndxsqliteasyncconnection)
  - [TNDXAsyncResult](#tndxasyncresult)
  - [TNDXCancellationTokenSource](#tndxcancellationtokensource)
- [Query Components](#query-components)
  - [TNDXSQLiteQuery](#tndxsqlitequery)
  - [TNDXSQLiteTable](#tndxsqlitetable)
  - [TNDXSQLiteBatchExecutor](#tndxsqlitebatchexecutor)
- [Health and Maintenance](#health-and-maintenance)
  - [TNDXSQLiteHealthCheck](#tndxsqlitehealthcheck)
- [Advanced Classes](#advanced-classes)
  - [TNDXSQLiteBackup](#tndxsqlitebackup)
  - [TNDXMigrationManager](#tndxmigrationmanager)
  - [TNDXSQLiteBlob](#tndxsqliteblob)
- [Data Import/Export](#data-importexport)
  - [TNDXSQLiteCSV](#tndxsqlitecsv)
  - [TNDXSQLiteDump](#tndxsqlitedump)
  - [TNDXSQLiteJSON](#tndxsqlitejson)
- [Full-Text Search](#full-text-search)
  - [TNDXSQLiteFTS](#tndxsqlitefts)
- [Spatial Indexing](#spatial-indexing)
  - [TNDXSQLiteRTree](#tndxsqliteertree)
- [Extensibility](#extensibility)
  - [TNDXSQLiteUDF](#tndxsqliteudf)
  - [TNDXSQLiteCollation](#tndxsqlitecollation)
  - [TNDXVirtualTableModule](#tndxvirtualtablemodule)
- [Multi-Database Support](#multi-database-support)
  - [TNDXSQLiteAttached](#tndxsqliteattached)
- [Security and Access Control](#security-and-access-control)
  - [TNDXSQLiteAuthorizer](#tndxsqliteauthorizer)
- [Hooks and Notifications](#hooks-and-notifications)
  - [TNDXSQLitePreUpdate](#tndxsqlitepreupdate)
  - [TNDXSQLiteUnlockNotify](#tndxsqliteunlocknotify)
- [Schema Tools](#schema-tools)
  - [TNDXSQLiteVerify](#tndxsqliteverify)
- [Enumerations](#enumerations)
- [Exceptions](#exceptions)
- [Utility Functions](#utility-functions)

---

## Core Classes

### TNDXSQLiteConnection

The main database connection class. Thread-safe with automatic connection management.

**Unit:** `ndxsqliteconnection`

#### Constructors

```pascal
constructor Create(AOptions: TNDXSQLiteConnectionOptions);
constructor Create(const ADatabasePath: string; AIsPrimary: Boolean = False);
```

#### Connection Methods

| Method | Description |
|--------|-------------|
| `Open` | Opens the database connection and applies PRAGMA settings |
| `Close` | Closes the connection and releases resources |

#### Query Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `ExecuteNonQuery(SQL)` | `Integer` | Executes INSERT/UPDATE/DELETE, returns affected rows |
| `ExecuteNonQuery(SQL, Params)` | `Integer` | Parameterized non-query execution |
| `ExecuteScalar(SQL)` | `Variant` | Returns first column of first row |
| `ExecuteScalar(SQL, Params)` | `Variant` | Parameterized scalar query |
| `ExecuteQuery(SQL)` | `TDataSet` | Returns a dataset (caller must free) |
| `ExecuteQuery(SQL, Params)` | `TDataSet` | Parameterized query returning dataset |
| `ExecuteScalarInt64(SQL)` | `Int64` | Scalar query avoiding Variant truncation |
| `CreateQuery(SQL)` | `TDataSet` | Creates a reusable query object |

#### Transactions

| Method | Description |
|--------|-------------|
| `BeginTransaction(Mode)` | Starts transaction (Deferred, Immediate, Exclusive) |
| `Commit` | Commits the active transaction |
| `Rollback` | Rolls back the active transaction |
| `Savepoint(Name)` | Creates a named savepoint |
| `ReleaseSavepoint(Name)` | Releases (commits) a savepoint |
| `RollbackToSavepoint(Name)` | Rolls back to a savepoint |

#### PRAGMA Methods

| Method | Description |
|--------|-------------|
| `SetJournalMode(Mode)` | Sets journal mode (DELETE, WAL, etc.) |
| `GetJournalMode` | Returns current journal mode |
| `SetSyncMode(Mode)` | Sets synchronous mode |
| `GetSyncMode` | Returns current synchronous mode |
| `SetCacheSize(KB)` | Sets page cache size in kilobytes |
| `GetCacheSize` | Returns current cache size |
| `SetBusyTimeout(Ms)` | Sets busy timeout in milliseconds |
| `EnableForeignKeys(Bool)` | Enables/disables foreign key enforcement |
| `IsForeignKeysEnabled` | Returns foreign key status |
| `GetPragmaValue(Name)` | Reads any PRAGMA value |
| `SetPragmaValue(Name, Value)` | Sets any PRAGMA value |

#### Encryption Methods (SQLCipher)

| Method | Description |
|--------|-------------|
| `ChangeEncryptionKey(NewKey)` | Changes or removes encryption key |
| `IsEncrypted` | Returns True if database is encrypted |
| `IsSQLCipherAvailable` | Class method: checks SQLCipher availability |

#### Backup Methods

| Method | Description |
|--------|-------------|
| `BackupTo(DestPath)` | Creates online backup |
| `RestoreFrom(SourcePath)` | Restores from backup file |

#### Utility Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `GetLastInsertRowId` | `Int64` | Last auto-increment ID |
| `GetChangesCount` | `Integer` | Rows affected by last statement |
| `GetTotalChangesCount` | `Integer` | Total rows changed since connection opened |
| `ResetAutoCloseTimer` | - | Prevents inactivity timeout |

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `Id` | `Integer` | Unique connection identifier |
| `CreatedAt` | `TDateTime` | Connection creation time |
| `State` | `TNDXSQLiteConnectionState` | Current connection state |
| `IsOpen` | `Boolean` | Connection is open |
| `IsTransactionActive` | `Boolean` | Transaction in progress |
| `IsPrimaryConnection` | `Boolean` | Primary connection flag |
| `DatabasePath` | `string` | Database file path |
| `LastAction` | `string` | Most recent action description |
| `ActionHistory` | `TStrings` | Recent action log |
| `ConnectionHandle` | `Pointer` | Raw sqlite3 handle |

#### Example

```pascal
var
  Options: TNDXSQLiteConnectionOptions;
  Conn: TNDXSQLiteConnection;
begin
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := 'myapp.db';
    Options.JournalMode := jmWAL;
    Options.ForeignKeys := True;

    Conn := TNDXSQLiteConnection.Create(Options);
    try
      Conn.Open;

      // Transaction example
      Conn.BeginTransaction;
      try
        Conn.ExecuteNonQuery('INSERT INTO users (name) VALUES (?)', ['Alice']);
        Conn.ExecuteNonQuery('INSERT INTO users (name) VALUES (?)', ['Bob']);
        Conn.Commit;
      except
        Conn.Rollback;
        raise;
      end;

      // Query example
      var Count := Conn.ExecuteScalar('SELECT COUNT(*) FROM users');
      WriteLn('Total users: ', Count);

      Conn.Close;
    finally
      Conn.Free;
    end;
  finally
    Options.Free;
  end;
end;
```

---

### TNDXSQLiteConnectionOptions

Configuration options for database connections.

**Unit:** `ndxsqliteconnectionoptions`

#### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `DatabasePath` | `string` | `''` | Path to database file |
| `CreateIfNotExists` | `Boolean` | `True` | Create database if missing |
| `ReadOnly` | `Boolean` | `False` | Open in read-only mode |
| `IsPrimaryConnection` | `Boolean` | `False` | Mark as primary (no auto-close) |
| `AutoCloseTimeoutMs` | `Integer` | `60000` | Inactivity timeout (ms) |
| `DisableAutoClose` | `Boolean` | `False` | Disable auto-close |
| `JournalMode` | `TNDXSQLiteJournalMode` | `jmDelete` | Journal mode |
| `SyncMode` | `TNDXSQLiteSyncMode` | `smNormal` | Synchronous mode |
| `CacheSize` | `Integer` | `2000` | Cache size in KB |
| `BusyTimeout` | `Integer` | `5000` | Lock timeout (ms) |
| `ForeignKeys` | `Boolean` | `True` | Enable foreign keys |
| `SharedCache` | `Boolean` | `False` | Shared cache mode |
| `MemoryDatabase` | `Boolean` | `False` | In-memory database |
| `EncryptionKey` | `string` | `''` | SQLCipher encryption key |
| `ConnectionTimeoutMs` | `Integer` | `30000` | Connection timeout (ms) |
| `PageSize` | `Integer` | `4096` | Page size in bytes |
| `LockingMode` | `TNDXSQLiteLockingMode` | `lmNormal` | Locking mode |
| `TempStore` | `Integer` | `2` | Temp storage (0=DEFAULT, 1=FILE, 2=MEMORY) |
| `MaxPageCount` | `Integer` | `0` | Maximum pages (0=unlimited) |
| `WALAutoCheckpoint` | `Integer` | `1000` | WAL checkpoint threshold |

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Clone` | `TNDXSQLiteConnectionOptions` | Creates independent copy |
| `Validate` | `Boolean` | Validates configuration |
| `ValidateWithMessage(out Msg)` | `Boolean` | Validates with error message |
| `BuildConnectionString` | `string` | Returns connection string |

#### Example

```pascal
Options := TNDXSQLiteConnectionOptions.Create;
Options.DatabasePath := '/data/myapp.db';
Options.JournalMode := jmWAL;
Options.SyncMode := smNormal;
Options.CacheSize := 4000;  // 4MB cache
Options.BusyTimeout := 10000;  // 10 second timeout
Options.ForeignKeys := True;
Options.EncryptionKey := 'secret-key';  // Optional encryption
```

---

### INDXSQLiteConnection

Interface for database connections. Use this type for variables to enable reference counting.

**Unit:** `ndxsqliteconnectionintf`

#### Key Interface Members

```pascal
INDXSQLiteConnection = interface
  procedure Open;
  procedure Close;
  function ExecuteNonQuery(const ASQL: string): Integer;
  function ExecuteScalar(const ASQL: string): Variant;
  function ExecuteQuery(const ASQL: string): TDataSet;
  procedure BeginTransaction(AMode: TNDXSQLiteTransactionMode = tmDeferred);
  procedure Commit;
  procedure Rollback;
  function IsOpen: Boolean;
  function IsTransactionActive: Boolean;
  function GetLastInsertRowId: Int64;
  function ConnectionHandle: Pointer;
end;
```

**Important:** Always use `INDXSQLiteConnection` for variables when working with components like backup, migration, or health check to avoid reference counting issues.

---

## Connection Management

### TNDXSQLiteConnectionPool

Thread-safe connection pool for multi-threaded applications.

**Unit:** `ndxsqliteconnectionpool`

#### Constructor

```pascal
constructor Create(AOptions: TNDXSQLiteConnectionOptions;
  AMinSize: Integer = 2; AMaxSize: Integer = 10);
```

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Acquire` | `INDXSQLiteConnection` | Gets connection (blocks until available) |
| `TryAcquire(out Conn, TimeoutMs)` | `Boolean` | Tries to acquire with timeout |
| `Release(Conn)` | - | Returns connection to pool |
| `Clear` | - | Closes all connections |
| `Resize(NewMin, NewMax)` | - | Adjusts pool size |
| `Validate` | - | Validates idle connections |

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `MinSize` | `Integer` | Minimum pool size |
| `MaxSize` | `Integer` | Maximum pool size |
| `ActiveCount` | `Integer` | Currently active connections |
| `IdleCount` | `Integer` | Currently idle connections |
| `Statistics` | `TNDXPoolStatistics` | Pool statistics |
| `AcquireTimeoutMs` | `Integer` | Acquire timeout |
| `ValidateOnAcquire` | `Boolean` | Validate before returning |
| `ValidationQuery` | `string` | Query for validation |

#### TNDXPoolStatistics Record

| Field | Type | Description |
|-------|------|-------------|
| `TotalCreated` | `Integer` | Total connections created |
| `TotalDestroyed` | `Integer` | Total connections destroyed |
| `CurrentActive` | `Integer` | Currently in use |
| `CurrentIdle` | `Integer` | Currently idle |
| `PeakActive` | `Integer` | Peak concurrent usage |
| `TotalAcquisitions` | `Integer` | Total acquire calls |
| `TotalReleases` | `Integer` | Total release calls |
| `AverageWaitTimeMs` | `Double` | Average wait time |

#### Example

```pascal
var
  Pool: TNDXSQLiteConnectionPool;
  Conn: INDXSQLiteConnection;
begin
  Pool := TNDXSQLiteConnectionPool.Create(Options, 2, 10);
  try
    Conn := Pool.Acquire;
    try
      Conn.ExecuteNonQuery('INSERT INTO logs (msg) VALUES (?)', ['Event']);
    finally
      Pool.Release(Conn);
    end;

    WriteLn('Active: ', Pool.Statistics.CurrentActive);
    WriteLn('Peak: ', Pool.Statistics.PeakActive);
  finally
    Pool.Free;
  end;
end;
```

#### TNDXPooledConnection Helper

Automatic acquire/release using try-finally pattern:

```pascal
var
  Pooled: TNDXPooledConnection;
begin
  Pooled := TNDXPooledConnection.Create(Pool);
  try
    Pooled.Connection.ExecuteNonQuery('INSERT INTO logs (msg) VALUES (?)', ['Auto']);
  finally
    Pooled.Free;  // Automatically releases connection
  end;
end;
```

---

### TNDXSQLiteConnectionFactory

Factory pattern for creating various connection types.

**Unit:** `ndxsqliteconnectionfactory`

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `CreateConnection(Path)` | `INDXSQLiteConnection` | Creates standard connection |
| `CreateConnectionWithOptions(Options)` | `INDXSQLiteConnection` | Creates with custom options |
| `CreatePrimaryConnection(Path)` | `INDXSQLiteConnection` | Creates primary connection |
| `CreateMemoryConnection` | `INDXSQLiteConnection` | Creates in-memory database |
| `CreateReadOnlyConnection(Path)` | `INDXSQLiteConnection` | Creates read-only connection |
| `CreateAsyncConnection(Path)` | `TNDXSQLiteAsyncConnection` | Creates async connection |

#### Example

```pascal
var
  Factory: TNDXSQLiteConnectionFactory;
  Conn: INDXSQLiteConnection;
begin
  Factory := TNDXSQLiteConnectionFactory.Create;
  try
    // Create in-memory database
    Conn := Factory.CreateMemoryConnection;
    Conn.Open;

    // Create read-only connection
    Conn := Factory.CreateReadOnlyConnection('data.db');
    Conn.Open;
  finally
    Factory.Free;
  end;
end;
```

---

## Asynchronous Operations

### TNDXSQLiteAsyncConnection

Asynchronous connection wrapper with callback-based operations.

**Unit:** `ndxsqliteasyncconnection`

#### Constructor

```pascal
constructor Create(AOptions: TNDXSQLiteConnectionOptions);
constructor Create(const ADatabasePath: string);
```

#### Async Methods

| Method | Description |
|--------|-------------|
| `OpenAsync(Callback)` | Opens connection asynchronously |
| `CloseAsync(Callback)` | Closes connection asynchronously |
| `ExecuteNonQueryAsync(SQL, Callback)` | Executes non-query asynchronously |
| `ExecuteNonQueryAsync(SQL, Params, Callback)` | Parameterized async non-query |
| `ExecuteScalarAsync(SQL, Callback)` | Executes scalar query asynchronously |
| `ExecuteQueryAsync(SQL, Callback)` | Executes query asynchronously |
| `BeginTransactionAsync(Mode, Callback)` | Begins transaction asynchronously |
| `CommitAsync(Callback)` | Commits asynchronously |
| `RollbackAsync(Callback)` | Rolls back asynchronously |
| `BackupToAsync(Path, Options, Callback)` | Performs async backup |

#### Operation Management

| Method | Description |
|--------|-------------|
| `CancelAllPendingOperations` | Cancels all pending async operations |
| `WaitForAllOperations(TimeoutMs)` | Waits for all operations to complete |
| `GetPendingOperationsCount` | Returns number of pending operations |
| `InterruptCurrentOperation` | Interrupts currently executing operation |

#### Example

```pascal
var
  AsyncConn: TNDXSQLiteAsyncConnection;
begin
  AsyncConn := TNDXSQLiteAsyncConnection.Create('mydb.db');
  try
    AsyncConn.OpenAsync(procedure(Result: TNDXAsyncResultBoolean)
    begin
      if Result.Success then
        WriteLn('Connected!')
      else
        WriteLn('Error: ', Result.ErrorMessage);
    end);

    AsyncConn.ExecuteNonQueryAsync(
      'INSERT INTO users (name) VALUES (?)',
      ['Alice'],
      procedure(Result: TNDXAsyncResultInt)
      begin
        if Result.Success then
          WriteLn('Inserted ', Result.Data, ' row(s)')
        else
          WriteLn('Error: ', Result.ErrorMessage);
      end);

    AsyncConn.WaitForAllOperations(30000);
  finally
    AsyncConn.Free;
  end;
end;
```

---

### TNDXAsyncResult

Generic record for async operation results.

**Unit:** `ndxsqliteasynctypes`

#### TNDXAsyncResult<T> Fields

| Field | Type | Description |
|-------|------|-------------|
| `Success` | `Boolean` | Operation succeeded |
| `Data` | `T` | Result data |
| `Status` | `TNDXAsyncStatus` | Operation status |
| `ErrorMessage` | `string` | Error description |
| `ErrorCode` | `Integer` | SQLite error code |
| `ExecutionTimeMs` | `Int64` | Execution duration |

#### TNDXAsyncStatus Enumeration

```pascal
TNDXAsyncStatus = (
  asPending,    // Not started
  asRunning,    // In progress
  asCompleted,  // Completed successfully
  asFaulted,    // Failed with error
  asCanceled    // Canceled
);
```

#### Type Specializations

| Type | Description |
|------|-------------|
| `TNDXAsyncResultInt` | Result with Integer data |
| `TNDXAsyncResultInt64` | Result with Int64 data |
| `TNDXAsyncResultVariant` | Result with Variant data |
| `TNDXAsyncResultString` | Result with string data |
| `TNDXAsyncResultBoolean` | Result with Boolean data |
| `TNDXAsyncResultDataSet` | Result with TDataSet data |

---

### TNDXCancellationTokenSource

.NET-style cancellation token pattern for async operations.

**Unit:** `ndxsqlitecancellation`

#### Methods

| Method | Description |
|--------|-------------|
| `Cancel` | Signals cancellation |
| `CancelAfter(DelayMs)` | Cancels after specified delay |
| `Reset` | Resets to non-canceled state |
| `GetToken` | Returns INDXCancellationToken |
| `CreateLinkedTokenSource(Tokens)` | Creates linked source |

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `IsCancellationRequested` | `Boolean` | Cancellation requested |

#### INDXCancellationToken Interface

```pascal
INDXCancellationToken = interface
  function IsCancellationRequested: Boolean;
  procedure ThrowIfCancellationRequested;
  function WaitHandle: TEvent;
end;
```

#### Example

```pascal
var
  CTS: TNDXCancellationTokenSource;
  Token: INDXCancellationToken;
begin
  CTS := TNDXCancellationTokenSource.Create;
  try
    Token := CTS.GetToken;

    // Cancel after 5 seconds
    CTS.CancelAfter(5000);

    // Check cancellation in long operation
    while not Token.IsCancellationRequested do
    begin
      // Do work...
    end;
  finally
    CTS.Free;
  end;
end;
```

---

## Query Components

### TNDXSQLiteQuery

Specialized query component with convenience methods.

**Unit:** `ndxsqlitequery`

#### Key Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `ExecSQL` | `Integer` | Executes SQL, returns affected rows |
| `ExecSQLScalar` | `Variant` | Executes and returns scalar |
| `SQL_Clear` | - | Clears SQL text |
| `SQL_Add(Line)` | - | Appends line to SQL |
| `SQL_Text` | `string` | Returns current SQL |
| `GetInt(Field)` | `Integer` | Gets field as integer |
| `GetStr(Field)` | `string` | Gets field as string |
| `GetFloat(Field)` | `Double` | Gets field as float |
| `GetDateTime(Field)` | `TDateTime` | Gets field as datetime |
| `GetBool(Field)` | `Boolean` | Gets field as boolean |
| `Exists` | `Boolean` | Returns True if query has results |
| `GetCount` | `Integer` | Returns record count |

#### Example

```pascal
var
  Query: TNDXSQLiteQuery;
begin
  Query := TNDXSQLiteQuery.Create(nil);
  try
    Query.Connection := Conn;
    Query.SQL_Clear;
    Query.SQL_Add('SELECT * FROM users WHERE active = 1');
    Query.Open;

    while not Query.EOF do
    begin
      WriteLn(Query.GetInt('id'), ': ', Query.GetStr('name'));
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;
```

---

### TNDXSQLiteTable

Table-aware component with master-detail support.

**Unit:** `ndxsqlitequery`

#### Key Methods

| Method | Description |
|--------|-------------|
| `CreateTable(SQL)` | Creates table from SQL |
| `DropTable(TableName)` | Drops table |
| `EmptyTable(TableName)` | Deletes all rows |
| `RenameTable(OldName, NewName)` | Renames table |
| `CreateIndex(Name, Table, Columns, Unique)` | Creates index |
| `DropIndex(Name)` | Drops index |
| `OpenTable(TableName)` | Opens table for browsing |
| `SetMasterSource(Source, Fields)` | Sets master-detail link |

#### Example

```pascal
var
  Table: TNDXSQLiteTable;
begin
  Table := TNDXSQLiteTable.Create(nil);
  try
    Table.Connection := Conn;
    Table.OpenTable('users');

    while not Table.EOF do
    begin
      WriteLn(Table.FieldByName('name').AsString);
      Table.Next;
    end;
  finally
    Table.Free;
  end;
end;
```

---

### TNDXSQLiteBatchExecutor

Batch SQL statement execution with transaction support.

**Unit:** `ndxsqlitequery`

#### Methods

| Method | Description |
|--------|-------------|
| `Clear` | Clears statement list |
| `Add(SQL)` | Adds SQL statement |
| `AddFormat(SQL, Args)` | Adds formatted SQL |
| `Execute` | Executes all statements |
| `ExecuteScript(Script)` | Executes multi-statement script |

#### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `UseTransaction` | `Boolean` | `True` | Wrap in transaction |
| `StopOnError` | `Boolean` | `True` | Stop on first error |
| `LastError` | `string` | - | Last error message |
| `ExecutedCount` | `Integer` | - | Statements executed |

#### Example

```pascal
var
  Batch: TNDXSQLiteBatchExecutor;
begin
  Batch := TNDXSQLiteBatchExecutor.Create(Conn);
  try
    Batch.UseTransaction := True;
    Batch.Add('INSERT INTO users (name) VALUES (''Alice'')');
    Batch.Add('INSERT INTO users (name) VALUES (''Bob'')');
    Batch.AddFormat('INSERT INTO users (name) VALUES (''%s'')', ['Charlie']);

    if Batch.Execute then
      WriteLn('Executed ', Batch.ExecutedCount, ' statements')
    else
      WriteLn('Error: ', Batch.LastError);
  finally
    Batch.Free;
  end;
end;
```

---

## Health and Maintenance

### TNDXSQLiteHealthCheck

Database health monitoring and maintenance utilities.

**Unit:** `ndxsqlitehealthcheck`

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `CheckHealth` | `TNDXSQLiteHealthCheckResult` | Performs health check |
| `GetDatabaseInfo` | `TNDXSQLiteDatabaseInfo` | Gets database information |
| `GetDatabaseStats` | `TNDXSQLiteDatabaseStats` | Gets database statistics |
| `CheckIntegrity` | `Boolean` | Runs integrity_check |
| `CheckForeignKeys` | `Boolean` | Validates foreign keys |
| `Vacuum` | `Boolean` | Compacts database |
| `Analyze` | `Boolean` | Updates query planner stats |
| `Optimize` | `Boolean` | Runs optimizer |
| `Checkpoint` | `Boolean` | WAL checkpoint |
| `GetTableCount` | `Integer` | Returns number of tables |
| `GetIndexCount` | `Integer` | Returns number of indexes |
| `GetTriggerCount` | `Integer` | Returns number of triggers |
| `GetViewCount` | `Integer` | Returns number of views |
| `GetTables` | `TStringList` | Returns table names |
| `GetTableRowCount(Table)` | `Int64` | Returns row count |

#### TNDXSQLiteHealthCheckResult Record

| Field | Type | Description |
|-------|------|-------------|
| `IsHealthy` | `Boolean` | Database is healthy |
| `Message` | `string` | Status message |
| `ResponseTimeMs` | `Int64` | Response time |
| `ExceptionMessage` | `string` | Exception if any |

#### TNDXSQLiteDatabaseInfo Record

| Field | Type | Description |
|-------|------|-------------|
| `SQLiteVersion` | `string` | SQLite version |
| `DatabasePath` | `string` | Database path |
| `DatabaseSizeBytes` | `Int64` | File size |
| `PageSize` | `Integer` | Page size |
| `PageCount` | `Integer` | Total pages |
| `FreePageCount` | `Integer` | Unused pages |
| `JournalMode` | `string` | Journal mode |
| `Encoding` | `string` | Text encoding |
| `AutoVacuum` | `Integer` | Auto-vacuum mode |
| `IsEncrypted` | `Boolean` | Encryption status |

#### TNDXSQLiteDatabaseStats Record

| Field | Type | Description |
|-------|------|-------------|
| `TableCount` | `Integer` | Number of tables |
| `IndexCount` | `Integer` | Number of indexes |
| `TriggerCount` | `Integer` | Number of triggers |
| `ViewCount` | `Integer` | Number of views |

#### Example

```pascal
var
  Health: TNDXSQLiteHealthCheck;
  Result: TNDXSQLiteHealthCheckResult;
  Info: TNDXSQLiteDatabaseInfo;
begin
  Health := TNDXSQLiteHealthCheck.Create(Conn);
  try
    Result := Health.CheckHealth;
    if Result.IsHealthy then
    begin
      Info := Health.GetDatabaseInfo;
      WriteLn('SQLite version: ', Info.SQLiteVersion);
      WriteLn('Database size: ', Info.DatabaseSizeBytes, ' bytes');
      WriteLn('Tables: ', Health.GetTableCount);

      // Maintenance
      Health.Analyze;
      Health.Vacuum;
    end;
  finally
    Health.Free;
  end;
end;
```

---

## Advanced Classes

### TNDXSQLiteBackup

Online database backup with progress reporting.

**Unit:** `ndxsqlitebackup`

#### Constructor

```pascal
constructor Create(AConnection: INDXSQLiteConnection);
```

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `BackupTo(DestPath)` | `TNDXBackupResult` | Backup with default options |
| `BackupTo(DestPath, Options)` | `TNDXBackupResult` | Backup with custom options |
| `BackupToAsync(DestPath, Options, Cancel)` | `TNDXBackupResult` | Cancellable async backup |

#### TNDXBackupOptions Record

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `PagesPerStep` | `Integer` | `100` | Pages per backup step |
| `StepDelayMs` | `Integer` | `0` | Delay between steps |
| `OverwriteExisting` | `Boolean` | `True` | Overwrite existing file |
| `CompressBackup` | `Boolean` | `False` | Compress backup |
| `VerifyAfterBackup` | `Boolean` | `True` | Verify integrity after |

#### TNDXBackupResult Record

| Field | Type | Description |
|-------|------|-------------|
| `Success` | `Boolean` | Backup succeeded |
| `ErrorMessage` | `string` | Error message if failed |
| `TotalPages` | `Integer` | Total pages backed up |
| `ElapsedTimeMs` | `Int64` | Duration in milliseconds |
| `DestinationPath` | `string` | Destination file path |
| `DestinationSizeBytes` | `Int64` | Backup file size |
| `Verified` | `Boolean` | Integrity verified |

#### Events

| Event | Type | Description |
|-------|------|-------------|
| `OnProgress` | `TNDXBackupProgressEvent` | Progress callback |

#### Example

```pascal
var
  Conn: INDXSQLiteConnection;
  Backup: TNDXSQLiteBackup;
  Result: TNDXBackupResult;
begin
  Conn := TNDXSQLiteConnection.Create('mydb.db');
  Conn.Open;

  Backup := TNDXSQLiteBackup.Create(Conn);
  try
    Backup.OnProgress := @MyProgressHandler;
    Result := Backup.BackupTo('/backups/mydb_backup.db');

    if Result.Success then
      WriteLn('Backup completed: ', Result.ElapsedTimeMs, ' ms')
    else
      WriteLn('Backup failed: ', Result.ErrorMessage);
  finally
    Backup.Free;
  end;
end;
```

---

### TNDXMigrationManager

Schema migration framework with version tracking.

**Unit:** `ndxsqlitemigration`

#### Constructor

```pascal
constructor Create(AConnection: INDXSQLiteConnection;
  const ATableName: string = '_migrations');
```

#### Methods

| Method | Description |
|--------|-------------|
| `RegisterMigration(Migration)` | Registers a migration instance |
| `RegisterSQLMigration(Version, Name, UpSQL, DownSQL)` | Registers SQL migration |
| `MigrateUp` | Applies all pending migrations |
| `MigrateUp(TargetVersion)` | Migrates to specific version |
| `MigrateDown` | Rolls back one migration |
| `MigrateDown(TargetVersion)` | Rolls back to specific version |
| `GetCurrentVersion` | Returns current schema version |
| `GetPendingMigrations` | Returns list of pending migrations |
| `GetMigrationHistory` | Returns applied migration history |

#### Example

```pascal
var
  Manager: TNDXMigrationManager;
begin
  Manager := TNDXMigrationManager.Create(Conn);
  try
    Manager.RegisterSQLMigration(1, 'CreateUsers',
      'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)',
      'DROP TABLE users');

    Manager.RegisterSQLMigration(2, 'AddEmail',
      'ALTER TABLE users ADD COLUMN email TEXT',
      '');

    Manager.MigrateUp;
    WriteLn('Current version: ', Manager.GetCurrentVersion);
  finally
    Manager.Free;
  end;
end;
```

---

### TNDXSQLiteBlob

Incremental BLOB I/O for large binary data.

**Unit:** `ndxsqliteblob`

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Open(Table, Column, RowId, Mode, Database)` | - | Opens BLOB handle |
| `Close` | - | Closes BLOB handle |
| `Reopen(RowId)` | - | Moves to different row |
| `Read(Offset, Count)` | `TBytes` | Reads bytes from offset |
| `ReadAll` | `TBytes` | Reads entire BLOB |
| `Write(Data, Offset)` | - | Writes bytes at offset |
| `ReadToStream(Stream, Offset, Count)` | - | Reads to stream |
| `WriteFromStream(Stream, Offset, Count)` | - | Writes from stream |
| `ReadToFile(FilePath)` | - | Saves BLOB to file |
| `WriteFromFile(FilePath)` | - | Loads file into BLOB |

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `IsOpen` | `Boolean` | Handle is open |
| `Size` | `Integer` | BLOB size in bytes |
| `Table` | `string` | Table name |
| `Column` | `string` | Column name |
| `RowId` | `Int64` | Row identifier |
| `OpenMode` | `TNDXBlobOpenMode` | Read-only or read-write |

#### Example

```pascal
var
  Blob: TNDXSQLiteBlob;
begin
  Blob := TNDXSQLiteBlob.Create(Conn);
  try
    Blob.Open('files', 'content', 123, bomReadOnly);
    Blob.ReadToFile('/tmp/output.bin');
    Blob.Close;
  finally
    Blob.Free;
  end;
end;
```

---

## Data Import/Export

### TNDXSQLiteCSV

CSV import and export functionality with RFC 4180 compliance.

**Unit:** `ndxsqlitecsv`

#### Export Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `ExportQueryToCSV(SQL, FilePath, Options)` | `TNDXCSVExportResult` | Exports query to CSV file |
| `ExportTableToCSV(Table, FilePath, Options)` | `TNDXCSVExportResult` | Exports table to CSV file |
| `ExportToStringList(SQL, Options)` | `TStringList` | Exports to string list |

#### Import Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `ImportFromCSV(FilePath, Table, Options)` | `TNDXCSVImportResult` | Imports CSV to table |
| `ImportFromStringList(List, Table, Options)` | `TNDXCSVImportResult` | Imports from string list |

#### TNDXCSVExportOptions Record

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `Delimiter` | `Char` | `,` | Field separator |
| `IncludeHeader` | `Boolean` | `True` | Include column headers |
| `QuoteAllFields` | `Boolean` | `False` | Quote all fields |
| `NullValue` | `string` | `''` | NULL representation |
| `BlobFormat` | `TNDXCSVBlobFormat` | `cbfSkip` | BLOB handling |
| `LineEnding` | `string` | `CRLF` | Line ending |

#### TNDXCSVImportOptions Record

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `Delimiter` | `Char` | `,` | Field separator |
| `HasHeader` | `Boolean` | `True` | First row is header |
| `SkipEmptyLines` | `Boolean` | `True` | Skip empty lines |
| `TrimFields` | `Boolean` | `True` | Trim whitespace |
| `NullValue` | `string` | `''` | NULL marker |
| `UseTransaction` | `Boolean` | `True` | Wrap in transaction |
| `StopOnError` | `Boolean` | `False` | Stop on first error |

#### Example

```pascal
var
  CSV: TNDXSQLiteCSV;
  ExportOpts: TNDXCSVExportOptions;
  Result: TNDXCSVExportResult;
begin
  CSV := TNDXSQLiteCSV.Create(Conn);
  try
    ExportOpts := Default(TNDXCSVExportOptions);
    ExportOpts.Delimiter := ';';
    ExportOpts.IncludeHeader := True;

    Result := CSV.ExportTableToCSV('users', '/tmp/users.csv', ExportOpts);
    WriteLn('Exported ', Result.RowCount, ' rows');
  finally
    CSV.Free;
  end;
end;
```

---

### TNDXSQLiteDump

SQL dump generation for database backup and migration.

**Unit:** `ndxsqlitedump`

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `DumpDatabase(FilePath, Options)` | `TNDXDumpResult` | Dumps entire database |
| `DumpTable(Table, FilePath, Options)` | `TNDXDumpResult` | Dumps single table |
| `DumpSchema(FilePath)` | `TNDXDumpResult` | Dumps schema only |
| `DumpToStringList(Options)` | `TStringList` | Dumps to string list |
| `RestoreFromDump(FilePath)` | `TNDXRestoreResult` | Restores from SQL dump |

#### TNDXDumpOptions Record

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `IncludeSchema` | `Boolean` | `True` | Include CREATE statements |
| `IncludeData` | `Boolean` | `True` | Include INSERT statements |
| `IncludeIndexes` | `Boolean` | `True` | Include indexes |
| `IncludeTriggers` | `Boolean` | `True` | Include triggers |
| `IncludeViews` | `Boolean` | `True` | Include views |
| `UseTransaction` | `Boolean` | `True` | Wrap in transaction |
| `InsertMode` | `TNDXInsertMode` | `imInsert` | INSERT style |

#### Example

```pascal
var
  Dump: TNDXSQLiteDump;
  Options: TNDXDumpOptions;
begin
  Dump := TNDXSQLiteDump.Create(Conn);
  try
    Options := Default(TNDXDumpOptions);
    Options.IncludeSchema := True;
    Options.IncludeData := True;

    Dump.DumpDatabase('/backup/dump.sql', Options);
  finally
    Dump.Free;
  end;
end;
```

---

### TNDXSQLiteJSON

JSON1 extension wrapper for JSON operations.

**Unit:** `ndxsqlitejson`

#### Query Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `IsJSONSupported` | `Boolean` | Checks JSON1 extension |
| `IsValidJSON(Text)` | `Boolean` | Validates JSON string |
| `GetJSONType(Text)` | `TNDXJSONValueType` | Returns JSON value type |
| `JSONExtract(JSON, Path)` | `Variant` | Extracts value by path |
| `JSONExtractText(JSON, Path)` | `string` | Extracts as text |
| `JSONExtractArray(JSON, Path)` | `TStringList` | Extracts array elements |

#### Modification Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `JSONSet(JSON, Path, Value)` | `string` | Sets value at path |
| `JSONInsert(JSON, Path, Value)` | `string` | Inserts value |
| `JSONReplace(JSON, Path, Value)` | `string` | Replaces value |
| `JSONRemove(JSON, Path)` | `string` | Removes path |
| `JSONPatch(JSON, Patch)` | `string` | Applies JSON patch |

#### Construction Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `JSONObject(Pairs)` | `string` | Builds JSON object |
| `JSONArray(Values)` | `string` | Builds JSON array |
| `JSONQuote(Value)` | `string` | Quotes value for JSON |

#### Aggregation Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `JSONGroupArray(SQL)` | `string` | Aggregates as JSON array |
| `JSONGroupObject(KeySQL, ValueSQL)` | `string` | Aggregates as JSON object |

#### Table Operations

| Method | Returns | Description |
|--------|---------|-------------|
| `QueryJSONPath(Table, Column, Path)` | `TDataSet` | Queries by JSON path |
| `QueryJSONWhere(Table, Column, Path, Value)` | `TDataSet` | Filters by JSON value |
| `QueryJSONContains(Table, Column, Value)` | `TDataSet` | Contains search |
| `JSONToTable(JSON, TableName)` | `Boolean` | Creates table from JSON |
| `TableToJSON(Table)` | `string` | Exports table as JSON |
| `QueryToJSON(SQL)` | `string` | Exports query as JSON |
| `ExportTableToFile(Table, FilePath)` | `Boolean` | Exports to JSON file |
| `ExportQueryToFile(SQL, FilePath)` | `Boolean` | Exports query to file |

#### TNDXJSONValueType Enumeration

```pascal
TNDXJSONValueType = (
  jvtNull,     // JSON null
  jvtTrue,     // JSON true
  jvtFalse,    // JSON false
  jvtInteger,  // Integer number
  jvtReal,     // Floating point
  jvtText,     // String
  jvtBlob,     // Binary data
  jvtArray,    // JSON array
  jvtObject    // JSON object
);
```

#### Example

```pascal
var
  JSON: TNDXSQLiteJSON;
  Value: string;
begin
  JSON := TNDXSQLiteJSON.Create(Conn);
  try
    if JSON.IsJSONSupported then
    begin
      // Extract value
      Value := JSON.JSONExtractText('{"name":"Alice","age":30}', '$.name');
      WriteLn('Name: ', Value);

      // Modify JSON
      Value := JSON.JSONSet('{"name":"Alice"}', '$.age', '30');
      WriteLn('Updated: ', Value);

      // Export table as JSON
      Value := JSON.TableToJSON('users');
      WriteLn(Value);
    end;
  finally
    JSON.Free;
  end;
end;
```

---

## Full-Text Search

### TNDXSQLiteFTS

Full-text search wrapper for FTS5.

**Unit:** `ndxsqlitefts`

#### Table Management

| Method | Returns | Description |
|--------|---------|-------------|
| `CreateFTSTable(Name, Columns, Options)` | `Boolean` | Creates FTS5 table |
| `DropFTSTable(Name)` | `Boolean` | Drops FTS table |
| `FTSTableExists(Name)` | `Boolean` | Checks if FTS table exists |

#### Search Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Search(Table, Query)` | `TDataSet` | Basic search |
| `Search(Table, Query, Options)` | `TDataSet` | Search with options |
| `SearchWithSnippet(Table, Query, Column)` | `TDataSet` | Search with highlighted snippets |
| `SearchWithRank(Table, Query)` | `TDataSet` | Search with relevance ranking |
| `SearchBoolean(Table, Query)` | `TDataSet` | Boolean query search |
| `SearchPhrase(Table, Phrase)` | `TDataSet` | Exact phrase search |
| `SearchPrefix(Table, Prefix)` | `TDataSet` | Prefix matching |
| `SearchNear(Table, Word1, Word2, Distance)` | `TDataSet` | NEAR query |

#### Maintenance Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `RebuildIndex(Table)` | `Boolean` | Rebuilds FTS index |
| `OptimizeIndex(Table)` | `Boolean` | Optimizes index |
| `IntegrityCheck(Table)` | `Boolean` | Checks index integrity |
| `SyncFromContentTable(FTSTable, ContentTable)` | `Boolean` | Syncs external content |
| `SetTokenizer(Table, Tokenizer, Options)` | `Boolean` | Sets tokenizer |

#### TNDXFTSSearchOptions Record

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `HighlightStart` | `string` | `<b>` | Highlight start tag |
| `HighlightEnd` | `string` | `</b>` | Highlight end tag |
| `SnippetSize` | `Integer` | `64` | Snippet length |
| `MaxResults` | `Integer` | `0` | Max results (0=unlimited) |
| `OrderByRank` | `Boolean` | `True` | Sort by relevance |

#### Example

```pascal
var
  FTS: TNDXSQLiteFTS;
  Results: TDataSet;
begin
  FTS := TNDXSQLiteFTS.Create(Conn);
  try
    // Create FTS table
    FTS.CreateFTSTable('articles_fts', ['title', 'content'], '');

    // Search with snippets
    Results := FTS.SearchWithSnippet('articles_fts', 'SQLite database', 'content');
    try
      while not Results.EOF do
      begin
        WriteLn('Title: ', Results.FieldByName('title').AsString);
        WriteLn('Snippet: ', Results.FieldByName('snippet').AsString);
        Results.Next;
      end;
    finally
      Results.Free;
    end;
  finally
    FTS.Free;
  end;
end;
```

---

## Spatial Indexing

### TNDXSQLiteRTree

R-Tree spatial index support for 2D/3D queries.

**Unit:** `ndxsqliteertree`

#### Table Management

| Method | Description |
|--------|-------------|
| `CreateRTree(Name, Dimension)` | Creates R-Tree (1D to 5D) |
| `CreateRTreeWithAux(Name, Dim, AuxCols)` | Creates with auxiliary columns |
| `DropRTree(Name)` | Drops R-Tree table |
| `RTreeExists(Name)` | Checks existence |
| `OpenRTree(Name)` | Opens existing R-Tree |

#### Insert/Update Methods

| Method | Description |
|--------|-------------|
| `Insert2D(ID, MinX, MaxX, MinY, MaxY)` | Inserts 2D entry |
| `Insert3D(ID, MinX, MaxX, MinY, MaxY, MinZ, MaxZ)` | Inserts 3D entry |
| `InsertPoint2D(ID, X, Y)` | Inserts 2D point |
| `InsertPoint3D(ID, X, Y, Z)` | Inserts 3D point |
| `Insert2DWithAux(ID, Box, AuxValues)` | Inserts with aux data |
| `Update2D(ID, MinX, MaxX, MinY, MaxY)` | Updates 2D entry |
| `Update3D(ID, Box)` | Updates 3D entry |
| `Delete(ID)` | Deletes entry |

#### Query Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `QueryBox2D(MinX, MaxX, MinY, MaxY)` | `TDataSet` | Box overlap query |
| `QueryBox3D(MinX, MaxX, MinY, MaxY, MinZ, MaxZ)` | `TDataSet` | 3D box query |
| `QueryBox(BoundingBox)` | `TDataSet` | Query by bounding box |
| `QueryContainsPoint2D(X, Y)` | `TDataSet` | Point containment |
| `QueryContainsPoint3D(X, Y, Z)` | `TDataSet` | 3D point containment |
| `QueryWithinRadius2D(CX, CY, R)` | `TDataSet` | Circle/radius query |
| `QueryWithinRadius3D(CX, CY, CZ, R)` | `TDataSet` | Sphere query |
| `QueryCustom(WhereClause)` | `TDataSet` | Custom WHERE |
| `CountInBox2D(Box)` | `Int64` | Count in region |
| `GetAll` | `TDataSet` | All entries |
| `GetByID(ID)` | `TDataSet` | Single entry |

#### Utility Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `GetTotalBounds2D` | `TNDXBoundingBox2D` | Overall bounds |
| `GetTotalBounds3D` | `TNDXBoundingBox3D` | Overall 3D bounds |
| `GetCount` | `Int64` | Entry count |
| `BeginBulkInsert` | - | Starts bulk transaction |
| `EndBulkInsert` | - | Commits bulk transaction |
| `Clear` | - | Removes all entries |
| `Rebuild` | - | Rebuilds index |

#### TNDXRTreeDimension Enumeration

```pascal
TNDXRTreeDimension = (
  rtd1D,  // 1D (min/max)
  rtd2D,  // 2D (x, y)
  rtd3D,  // 3D (x, y, z)
  rtd4D,  // 4D
  rtd5D   // 5D
);
```

#### Example

```pascal
var
  RTree: TNDXSQLiteRTree;
  Results: TDataSet;
begin
  RTree := TNDXSQLiteRTree.Create(Conn);
  try
    RTree.CreateRTree('locations', rtd2D);

    // Bulk insert
    RTree.BeginBulkInsert;
    try
      RTree.InsertPoint2D(1, 10.5, 20.3);
      RTree.InsertPoint2D(2, 15.2, 25.7);
      RTree.Insert2D(3, 5.0, 12.0, 18.0, 30.0);
    finally
      RTree.EndBulkInsert;
    end;

    // Query points within radius
    Results := RTree.QueryWithinRadius2D(12.0, 22.0, 10.0);
    try
      WriteLn('Found ', Results.RecordCount, ' entries');
    finally
      Results.Free;
    end;
  finally
    RTree.Free;
  end;
end;
```

#### Helper Functions

```pascal
function BoundingBox2D(MinX, MaxX, MinY, MaxY: Double): TNDXBoundingBox2D;
function BoundingBox3D(MinX, MaxX, MinY, MaxY, MinZ, MaxZ: Double): TNDXBoundingBox3D;
function PointToBox2D(X, Y: Double): TNDXBoundingBox2D;
function PointToBox3D(X, Y, Z: Double): TNDXBoundingBox3D;
function ExpandBox2D(Box: TNDXBoundingBox2D; Margin: Double): TNDXBoundingBox2D;
function BoxesOverlap2D(A, B: TNDXBoundingBox2D): Boolean;
function BoxContainsPoint2D(Box: TNDXBoundingBox2D; X, Y: Double): Boolean;
```

---

## Extensibility

### TNDXSQLiteUDF

User-defined SQL functions in Pascal.

**Unit:** `ndxsqliteudf`

#### Methods

| Method | Description |
|--------|-------------|
| `RegisterScalarFunction(Name, ArgCount, Func)` | Registers scalar function |
| `RegisterAggregateFunction(Name, ArgCount, Step, Final, CtxSize)` | Registers aggregate |
| `UnregisterFunction(Name)` | Removes function |
| `IsFunctionRegistered(Name)` | Checks registration |
| `GetRegisteredFunctions` | Returns function list |
| `UnregisterAll` | Removes all functions |

#### TNDXSQLiteValue Record

| Method | Returns | Description |
|--------|---------|-------------|
| `IsNull` | `Boolean` | Is NULL |
| `AsInteger` | `Integer` | As integer |
| `AsInt64` | `Int64` | As 64-bit integer |
| `AsDouble` | `Double` | As float |
| `AsString` | `string` | As string |
| `AsBlob` | `TBytes` | As bytes |
| `GetType` | `Integer` | SQLite type code |

#### TNDXSQLiteResult Record

| Method | Description |
|--------|-------------|
| `SetNull` | Returns NULL |
| `SetInteger(Value)` | Returns integer |
| `SetInt64(Value)` | Returns Int64 |
| `SetDouble(Value)` | Returns double |
| `SetString(Value)` | Returns string |
| `SetBlob(Value)` | Returns BLOB |
| `SetError(Message)` | Signals error |

#### Example

```pascal
var
  UDF: TNDXSQLiteUDF;

procedure MyUpperCase(var Result: TNDXSQLiteResult; const Args: array of TNDXSQLiteValue);
begin
  if Args[0].IsNull then
    Result.SetNull
  else
    Result.SetString(UpperCase(Args[0].AsString));
end;

begin
  UDF := TNDXSQLiteUDF.Create(Conn);
  try
    UDF.RegisterScalarFunction('MY_UPPER', 1, @MyUpperCase);

    // Now usable in SQL
    Conn.ExecuteQuery('SELECT MY_UPPER(name) FROM users');
  finally
    UDF.Free;
  end;
end;
```

---

### TNDXSQLiteCollation

Custom text comparison collations.

**Unit:** `ndxsqlitecollation`

#### Methods

| Method | Description |
|--------|-------------|
| `RegisterCollation(Name, Compare)` | Registers collation |
| `UnregisterCollation(Name)` | Removes collation |
| `IsCollationRegistered(Name)` | Checks registration |
| `GetRegisteredCollations` | Returns collation list |
| `UnregisterAll` | Removes all collations |

#### Built-in Comparison Functions

| Function | Description |
|----------|-------------|
| `CompareCaseInsensitive(A, B)` | Case-insensitive |
| `CompareNatural(A, B)` | Natural sort order |
| `CompareReverse(A, B)` | Reverse order |
| `CompareNumeric(A, B)` | Numeric comparison |

#### Example

```pascal
var
  Collation: TNDXSQLiteCollation;
begin
  Collation := TNDXSQLiteCollation.Create(Conn);
  try
    Collation.RegisterCollation('NATURAL', @TNDXSQLiteCollation.CompareNatural);

    // Use in queries
    Conn.ExecuteQuery('SELECT * FROM files ORDER BY name COLLATE NATURAL');
  finally
    Collation.Free;
  end;
end;
```

---

### TNDXVirtualTableModule

Virtual table framework for custom table implementations.

**Unit:** `ndxsqlitevtab`

#### Module Methods

| Method | Description |
|--------|-------------|
| `RegisterModule(Name, TableClass, ReadOnly)` | Registers module |
| `UnregisterModule(Name)` | Removes module |
| `IsModuleRegistered(Name)` | Checks registration |
| `CreateVirtualTable(TableName, ModuleName, Args)` | Creates table instance |
| `DropVirtualTable(TableName)` | Drops virtual table |
| `GetRegisteredModules` | Returns module list |
| `GetVirtualTables` | Returns table list |

#### Pre-built Virtual Tables

| Class | Description |
|-------|-------------|
| `TNDXSeriesVirtualTable` | Integer series generator |
| `TNDXCSVVirtualTable` | CSV file reader |
| `TNDXKeyValueVirtualTable` | In-memory key-value store |

#### TNDXSeriesVirtualTable

```pascal
var
  Module: TNDXVirtualTableModule;
  Series: TNDXSeriesVirtualTable;
begin
  Module := TNDXVirtualTableModule.Create(Conn);
  try
    Module.RegisterModule('series', TNDXSeriesVirtualTable, True);
    Series := TNDXSeriesVirtualTable(
      Module.CreateVirtualTable('numbers', 'series', []));
    Series.SetRange(1, 100, 1);  // 1 to 100
  finally
    Module.Free;
  end;
end;
```

#### TNDXCSVVirtualTable

```pascal
var
  CSV: TNDXCSVVirtualTable;
begin
  CSV := TNDXCSVVirtualTable.Create(Module, 'csvdata');
  CSV.SetFile('/data/records.csv', ',', True);
end;
```

#### TNDXKeyValueVirtualTable

| Method | Description |
|--------|-------------|
| `SetValue(Key, Value)` | Sets key-value pair |
| `GetValue(Key)` | Gets value by key |
| `HasKey(Key)` | Checks key existence |
| `DeleteKey(Key)` | Removes key |
| `Clear` | Clears all entries |

---

## Multi-Database Support

### TNDXSQLiteAttached

Multi-database attachment for cross-database queries.

**Unit:** `ndxsqliteattached`

#### Attachment Methods

| Method | Description |
|--------|-------------|
| `Attach(FilePath, Alias)` | Attaches database |
| `AttachReadOnly(FilePath, Alias)` | Attaches read-only |
| `AttachMemory(Alias)` | Attaches in-memory DB |
| `Detach(Alias)` | Detaches database |
| `IsAttached(Alias)` | Checks attachment |
| `GetAttachedInfo(Alias)` | Gets attachment info |
| `GetAttachedList` | Lists attached databases |
| `GetDatabaseList` | PRAGMA database_list |
| `DetachAll` | Detaches all |

#### Cross-Database Operations

| Method | Returns | Description |
|--------|---------|-------------|
| `CopyTable(SrcAlias, SrcTable, DestAlias, DestTable)` | - | Copies table |
| `CopyTableSchema(SrcAlias, SrcTable, DestAlias, DestTable)` | - | Copies schema only |
| `QueryAttached(SQL)` | `TDataSet` | Cross-database query |
| `ExecuteAttached(SQL)` | `Integer` | Cross-database execute |
| `GetTables(Alias)` | `TStringList` | Tables in attached DB |
| `GetTableSchema(Alias, Table)` | `string` | CREATE statement |
| `GetMaxAttachLimit` | `Integer` | Max attachable DBs |
| `GetAttachmentCount` | `Integer` | Current count |

#### Example

```pascal
var
  Attached: TNDXSQLiteAttached;
  Results: TDataSet;
begin
  Attached := TNDXSQLiteAttached.Create(Conn);
  try
    Attached.Attach('/data/archive.db', 'archive');
    Attached.Attach('/data/reports.db', 'reports');

    // Cross-database query
    Results := Attached.QueryAttached(
      'SELECT m.name, a.timestamp ' +
      'FROM main.users m ' +
      'JOIN archive.logins a ON m.id = a.user_id');
    try
      // Process results
    finally
      Results.Free;
    end;

    // Copy table between databases
    Attached.CopyTable('archive', 'old_users', 'main', 'imported_users');

    Attached.DetachAll;
  finally
    Attached.Free;
  end;
end;
```

---

## Security and Access Control

### TNDXSQLiteAuthorizer

Fine-grained SQL statement authorization.

**Unit:** `ndxsqliteauthorizer`

#### Methods

| Method | Description |
|--------|-------------|
| `SetAuthorizer(Callback)` | Sets authorization callback |
| `ClearAuthorizer` | Removes authorizer |
| `AllowAll` | Permits all operations |
| `DenyAll` | Denies all operations |
| `SetReadOnly` | Allows SELECT only |
| `AllowTable(Table)` | Adds allowed table |
| `DenyTable(Table)` | Adds denied table |
| `AllowOperation(Op)` | Allows operation type |
| `DenyOperation(Op)` | Denies operation type |

#### Authorization Actions

```pascal
TNDXAuthAction = (
  aaCopy,        // ATTACH/DETACH
  aaCreateIndex,
  aaCreateTable,
  aaCreateTempIndex,
  aaCreateTempTable,
  aaCreateTempTrigger,
  aaCreateTempView,
  aaCreateTrigger,
  aaCreateView,
  aaDelete,
  aaDropIndex,
  aaDropTable,
  aaDropTempIndex,
  aaDropTempTable,
  aaDropTempTrigger,
  aaDropTempView,
  aaDropTrigger,
  aaDropView,
  aaInsert,
  aaPragma,
  aaRead,
  aaSelect,
  aaTransaction,
  aaUpdate,
  aaAttach,
  aaDetach,
  aaAlterTable,
  aaReindex,
  aaAnalyze,
  aaCreateVTable,
  aaDropVTable,
  aaFunction,
  aaSavepoint,
  aaRecursive
);
```

#### TNDXAuthResult Enumeration

```pascal
TNDXAuthResult = (
  arOK,      // Allow operation
  arDeny,    // Deny with error
  arIgnore   // Silently ignore
);
```

#### Example

```pascal
var
  Auth: TNDXSQLiteAuthorizer;
begin
  Auth := TNDXSQLiteAuthorizer.Create(Conn);
  try
    // Read-only mode
    Auth.SetReadOnly;

    // Or custom rules
    Auth.AllowOperation(aaSelect);
    Auth.AllowOperation(aaRead);
    Auth.DenyOperation(aaDelete);
    Auth.DenyOperation(aaDrop);

    // Table-specific rules
    Auth.AllowTable('public_data');
    Auth.DenyTable('sensitive_data');
  finally
    Auth.Free;
  end;
end;
```

---

## Hooks and Notifications

### TNDXSQLitePreUpdate

Pre-update hooks for row inspection before changes.

**Unit:** `ndxsqlitepreupdate`

#### Methods

| Method | Description |
|--------|-------------|
| `SetPreUpdateHook(Callback)` | Sets pre-update callback |
| `ClearPreUpdateHook` | Removes hook |

#### TNDXPreUpdateInfo Record

| Field | Type | Description |
|-------|------|-------------|
| `Database` | `string` | Database name |
| `Table` | `string` | Table name |
| `Operation` | `TNDXPreUpdateOp` | INSERT/UPDATE/DELETE |
| `OldRowId` | `Int64` | Old row ID |
| `NewRowId` | `Int64` | New row ID |
| `ColumnCount` | `Integer` | Number of columns |

#### Methods Available in Callback

| Method | Returns | Description |
|--------|---------|-------------|
| `GetOldValue(Column)` | `Variant` | Old column value |
| `GetNewValue(Column)` | `Variant` | New column value |
| `GetDepth` | `Integer` | Trigger nesting depth |

#### Example

```pascal
var
  PreUpdate: TNDXSQLitePreUpdate;

procedure MyPreUpdateCallback(const Info: TNDXPreUpdateInfo);
begin
  WriteLn('Operation on ', Info.Table);
  if Info.Operation = puoUpdate then
  begin
    WriteLn('Old ID: ', Info.OldRowId);
    WriteLn('New ID: ', Info.NewRowId);
  end;
end;

begin
  PreUpdate := TNDXSQLitePreUpdate.Create(Conn);
  try
    PreUpdate.SetPreUpdateHook(@MyPreUpdateCallback);
    // Changes will trigger callback
  finally
    PreUpdate.Free;
  end;
end;
```

---

### TNDXSQLiteUnlockNotify

Unlock notifications for deadlock resolution.

**Unit:** `ndxsqliteunlocknotify`

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `WaitForUnlock(Timeout)` | `TNDXUnlockWaitResult` | Waits for unlock |
| `ExecuteWithRetry(SQL)` | `Boolean` | Retries on lock |
| `ExecuteWithRetryNonQuery(SQL)` | `Integer` | Non-query with retry |
| `IsAvailable` | `Boolean` | Class method: checks support |

#### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `DefaultTimeout` | `Cardinal` | `30000` | Wait timeout (ms) |
| `AutoRetry` | `Boolean` | `True` | Auto-retry on lock |
| `MaxRetries` | `Integer` | `5` | Maximum retries |
| `LastError` | `string` | - | Last error message |

#### TNDXUnlockWaitResult Enumeration

```pascal
TNDXUnlockWaitResult = (
  uwrSuccess,      // Unlocked
  uwrTimeout,      // Timed out
  uwrError,        // Error occurred
  uwrNotSupported  // Not available
);
```

#### TNDXSQLiteStepRetry Helper

```pascal
var
  Retry: TNDXSQLiteStepRetry;
begin
  Retry := TNDXSQLiteStepRetry.Create(Conn);
  try
    Retry.MaxRetries := 3;
    Retry.RetryDelay := 100;

    // StepWithRetry handles SQLITE_LOCKED automatically
    RC := Retry.StepWithRetry(Statement);
  finally
    Retry.Free;
  end;
end;
```

---

## Schema Tools

### TNDXSQLiteVerify

Schema verification and comparison utilities.

**Unit:** `ndxsqliteverify`

#### Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `VerifyTable(Name, ExpectedSchema)` | `TNDXVerifyResult` | Verifies table schema |
| `VerifyIndex(Name, ExpectedSchema)` | `TNDXVerifyResult` | Verifies index |
| `VerifyTrigger(Name, ExpectedSchema)` | `TNDXVerifyResult` | Verifies trigger |
| `VerifyView(Name, ExpectedSQL)` | `TNDXVerifyResult` | Verifies view |
| `CompareSchemas(Other)` | `TNDXSchemaDiff` | Compares two databases |
| `GetAllSchemas` | `TStringList` | Gets all CREATE statements |
| `TableExists(Name)` | `Boolean` | Checks table existence |
| `IndexExists(Name)` | `Boolean` | Checks index existence |
| `TriggerExists(Name)` | `Boolean` | Checks trigger existence |
| `ViewExists(Name)` | `Boolean` | Checks view existence |
| `GetTableColumns(Name)` | `TStringList` | Gets column definitions |
| `GetTableIndexes(Name)` | `TStringList` | Gets index names |
| `GenerateMigrationSQL(From, To)` | `TStringList` | Generates migration SQL |

#### TNDXVerifyResult Record

| Field | Type | Description |
|-------|------|-------------|
| `IsValid` | `Boolean` | Schema matches |
| `Message` | `string` | Validation message |
| `ActualSchema` | `string` | Current schema |
| `ExpectedSchema` | `string` | Expected schema |
| `Differences` | `TStringList` | List of differences |

#### Example

```pascal
var
  Verify: TNDXSQLiteVerify;
  Result: TNDXVerifyResult;
begin
  Verify := TNDXSQLiteVerify.Create(Conn);
  try
    Result := Verify.VerifyTable('users',
      'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL)');

    if not Result.IsValid then
    begin
      WriteLn('Schema mismatch!');
      WriteLn('Expected: ', Result.ExpectedSchema);
      WriteLn('Actual: ', Result.ActualSchema);
    end;
  finally
    Verify.Free;
  end;
end;
```

---

## Enumerations

### TNDXSQLiteTransactionMode

```pascal
TNDXSQLiteTransactionMode = (
  tmDeferred,    // Lock acquired on first write (default)
  tmImmediate,   // RESERVED lock acquired immediately
  tmExclusive    // EXCLUSIVE lock acquired immediately
);
```

### TNDXSQLiteJournalMode

```pascal
TNDXSQLiteJournalMode = (
  jmDelete,      // Delete journal after commit (legacy)
  jmTruncate,    // Truncate journal to zero
  jmPersist,     // Keep journal, zero header
  jmMemory,      // Journal in memory only
  jmWAL,         // Write-Ahead Logging (recommended)
  jmOff          // No journal (dangerous!)
);
```

### TNDXSQLiteSyncMode

```pascal
TNDXSQLiteSyncMode = (
  smOff,         // No sync (fast but risky)
  smNormal,      // Sync at critical moments (recommended)
  smFull,        // Sync after each write
  smExtra        // Extra sync for WAL
);
```

### TNDXSQLiteConnectionState

```pascal
TNDXSQLiteConnectionState = (
  scsDisconnected,   // Not connected
  scsConnecting,     // Opening connection
  scsConnected,      // Connected and ready
  scsExecuting,      // Running a query
  scsError           // Error state
);
```

### TNDXSQLiteOpenMode

```pascal
TNDXSQLiteOpenMode = (
  omReadWrite,       // Read and write (default)
  omReadOnly,        // Read only
  omReadWriteCreate, // Read/write, create if missing
  omMemory           // In-memory database
);
```

### TNDXSQLiteLockingMode

```pascal
TNDXSQLiteLockingMode = (
  lmNormal,          // Standard locking
  lmExclusive        // Permanent exclusive lock
);
```

### TNDXBlobOpenMode

```pascal
TNDXBlobOpenMode = (
  bomReadOnly,       // Read-only access
  bomReadWrite       // Read-write access
);
```

---

## Exceptions

All NDXSQLite exceptions inherit from `ENDXSQLiteException`.

| Exception | Description |
|-----------|-------------|
| `ENDXSQLiteException` | Base exception class |
| `ENDXSQLiteConnectionException` | Connection errors |
| `ENDXSQLiteTransactionException` | Transaction errors |
| `ENDXSQLiteQueryException` | Query execution errors |
| `ENDXSQLitePoolException` | Connection pool errors |
| `ENDXSQLiteTimeoutException` | Timeout errors |
| `ENDXSQLiteDisposedException` | Object already disposed |
| `ENDXOperationCanceledException` | Async operation canceled |

#### Example

```pascal
try
  Conn.ExecuteNonQuery('INVALID SQL');
except
  on E: ENDXSQLiteQueryException do
    WriteLn('Query error: ', E.Message);
  on E: ENDXSQLiteConnectionException do
    WriteLn('Connection error: ', E.Message);
  on E: ENDXSQLiteException do
    WriteLn('SQLite error: ', E.Message);
end;
```

---

## Utility Functions

### Unit: ndxsqlitetypes

| Function | Returns | Description |
|----------|---------|-------------|
| `IsValidIdentifier(Name)` | `Boolean` | Checks if name is valid SQL identifier |
| `QuoteIdentifier(Name)` | `string` | Wraps name in double quotes |
| `EscapeString(Value)` | `string` | Escapes single quotes |

### Unit: ndxsqliteplatform

| Function | Returns | Description |
|----------|---------|-------------|
| `TNDXPlatform.GetPlatformType` | `TNDXPlatformType` | Detects platform type |
| `TNDXPlatform.GetSQLiteLibraryPaths` | `TStringArray` | Library search paths |
| `TNDXPlatform.NormalizePath(Path)` | `string` | Normalizes path for platform |
| `TNDXPlatform.ValidateDatabasePath(Path)` | `string` | Validates and normalizes path |
| `TNDXPlatform.EnsureDirectoryExists(Path)` | - | Creates directory if needed |
| `TNDXPlatform.EscapeSQLPath(Path)` | `string` | Escapes path for SQL |
| `TNDXPlatform.IsSnapEnvironment` | `Boolean` | Detects Snap confinement |
| `TNDXPlatform.IsFlatpakEnvironment` | `Boolean` | Detects Flatpak sandbox |

### Unit: ndxsqliteversion

| Function | Returns | Description |
|----------|---------|-------------|
| `GetNDXSQLiteVersion` | `string` | NDXSQLite library version |
| `GetSQLiteVersion` | `string` | SQLite engine version |
| `GetSQLCipherVersion` | `string` | SQLCipher version (if available) |

---

## Module Summary

| Module | Unit | Description |
|--------|------|-------------|
| Core Connection | `ndxsqliteconnection` | Database connection class |
| Connection Options | `ndxsqliteconnectionoptions` | Configuration options |
| Connection Interface | `ndxsqliteconnectionintf` | Interface definition |
| Connection Pool | `ndxsqliteconnectionpool` | Thread-safe pooling |
| Connection Factory | `ndxsqliteconnectionfactory` | Factory pattern |
| Async Connection | `ndxsqliteasyncconnection` | Asynchronous operations |
| Async Types | `ndxsqliteasynctypes` | Async result types |
| Cancellation | `ndxsqlitecancellation` | Cancellation tokens |
| Query | `ndxsqlitequery` | Query components |
| Health Check | `ndxsqlitehealthcheck` | Database health |
| Backup | `ndxsqlitebackup` | Online backup |
| Migration | `ndxsqlitemigration` | Schema migrations |
| BLOB | `ndxsqliteblob` | Incremental BLOB I/O |
| CSV | `ndxsqlitecsv` | CSV import/export |
| Dump | `ndxsqlitedump` | SQL dump/restore |
| JSON | `ndxsqlitejson` | JSON1 extension |
| FTS | `ndxsqlitefts` | Full-text search |
| R-Tree | `ndxsqliteertree` | Spatial indexing |
| UDF | `ndxsqliteudf` | User-defined functions |
| Collation | `ndxsqlitecollation` | Custom collations |
| Virtual Tables | `ndxsqlitevtab` | Virtual table framework |
| Attached | `ndxsqliteattached` | Multi-database support |
| Authorizer | `ndxsqliteauthorizer` | Access control |
| Pre-Update | `ndxsqlitepreupdate` | Pre-update hooks |
| Unlock Notify | `ndxsqliteunlocknotify` | Lock notifications |
| Verify | `ndxsqliteverify` | Schema verification |
| Platform | `ndxsqliteplatform` | Platform detection |
| Types | `ndxsqlitetypes` | Common types |
| Exceptions | `ndxsqliteexceptions` | Exception classes |
| Version | `ndxsqliteversion` | Version information |
| API | `ndxsqlite3api` | SQLite3 API bindings |

---

*NDXSQLite API Reference - Version 1.0.0*
