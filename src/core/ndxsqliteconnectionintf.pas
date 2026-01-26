{===============================================================================
  NDXSQLite - Connection Interface
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 3 (depends on ndxsqlitetypes, ndxsqliteasynctypes, ndxsqlitecancellation)

  NOTE: This interface uses native SQLite API - no SQLDB dependency.
===============================================================================}
unit ndxsqliteconnectionintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  ndxsqlitetypes, ndxsqliteasynctypes, ndxsqlitecancellation;

type
  { Main SQLite connection interface }
  INDXSQLiteConnection = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    // Properties - Getters
    function GetId: Integer;
    function GetCreatedAt: TDateTime;
    function GetState: TNDXSQLiteConnectionState;
    function GetIsTransactionActive: Boolean;
    function GetIsPrimaryConnection: Boolean;
    function GetDatabasePath: string;
    function GetLastAction: string;
    function GetActionHistory: TStrings;
    function GetIsOpen: Boolean;
    function GetConnectionHandle: Pointer;

    // Properties
    property Id: Integer read GetId;
    property CreatedAt: TDateTime read GetCreatedAt;
    property State: TNDXSQLiteConnectionState read GetState;
    property IsTransactionActive: Boolean read GetIsTransactionActive;
    property IsPrimaryConnection: Boolean read GetIsPrimaryConnection;
    property DatabasePath: string read GetDatabasePath;
    property LastAction: string read GetLastAction;
    property ActionHistory: TStrings read GetActionHistory;
    property IsOpen: Boolean read GetIsOpen;
    property ConnectionHandle: Pointer read GetConnectionHandle;

    // Connection

    { Opens the database connection using the configured database path. }
    procedure Open;
    { Closes the database connection and releases all resources. }
    procedure Close;

    // Transactions

    { Begins a new transaction.
      @param AMode  Transaction mode: tmDeferred (default), tmImmediate, or tmExclusive.
      @returns True if the transaction was started successfully. }
    function BeginTransaction(AMode: TNDXSQLiteTransactionMode = tmDeferred): Boolean;
    { Commits the active transaction, making all changes permanent. }
    procedure Commit;
    { Rolls back the active transaction, discarding all changes. }
    procedure Rollback;

    // Savepoints (nested transactions)

    { Creates a named savepoint within the current transaction.
      @param AName  Unique name for the savepoint. }
    procedure Savepoint(const AName: string);
    { Releases (commits) a named savepoint.
      @param AName  Name of the savepoint to release. }
    procedure ReleaseSavepoint(const AName: string);
    { Rolls back to a named savepoint, discarding changes made after it.
      @param AName  Name of the savepoint to rollback to. }
    procedure RollbackToSavepoint(const AName: string);

    // Queries

    { Executes a non-query SQL statement (INSERT, UPDATE, DELETE, DDL).
      @param ASQL  The SQL statement to execute.
      @returns Number of rows affected by the statement. }
    function ExecuteNonQuery(const ASQL: string): Integer; overload;
    { Executes a parameterized non-query SQL statement.
      @param ASQL    The SQL statement with positional placeholders (?).
      @param AParams Array of parameter values bound in order.
      @returns Number of rows affected by the statement. }
    function ExecuteNonQuery(const ASQL: string; const AParams: array of Variant): Integer; overload;
    { Executes a query and returns the first column of the first row.
      @param ASQL  The SQL query to execute.
      @returns The scalar value, or Null if no rows returned. }
    function ExecuteScalar(const ASQL: string): Variant; overload;
    { Executes a parameterized query and returns the first column of the first row.
      @param ASQL    The SQL query with positional placeholders (?).
      @param AParams Array of parameter values bound in order.
      @returns The scalar value, or Null if no rows returned. }
    function ExecuteScalar(const ASQL: string; const AParams: array of Variant): Variant; overload;
    { Executes a query and returns a TDataSet with all result rows.
      @param ASQL  The SQL query to execute.
      @returns A TDataSet instance (caller must free). }
    function ExecuteQuery(const ASQL: string): TDataSet; overload;
    { Executes a parameterized query and returns a TDataSet with all result rows.
      @param ASQL    The SQL query with positional placeholders (?).
      @param AParams Array of parameter values bound in order.
      @returns A TDataSet instance (caller must free). }
    function ExecuteQuery(const ASQL: string; const AParams: array of Variant): TDataSet; overload;

    // Utilities

    { Resets the auto-close timer, preventing the connection from closing due to inactivity. }
    procedure ResetAutoCloseTimer;
    { Creates a new TDataSet query instance for the connection.
      @param ASQL  Optional SQL statement to assign to the query.
      @returns A TDataSet instance (caller must free). }
    function CreateQuery(const ASQL: string = ''): TDataSet;
    { Returns the rowid of the last successful INSERT operation. }
    function GetLastInsertRowId: Int64;
    { Returns the number of rows modified by the most recent statement. }
    function GetChangesCount: Integer;
    { Returns the total number of rows modified since the connection was opened. }
    function GetTotalChangesCount: Integer;

    // PRAGMAs

    { Sets the journal mode for the database.
      @param AMode  Journal mode (jmDelete, jmTruncate, jmPersist, jmMemory, jmWAL, jmOff). }
    procedure SetJournalMode(AMode: TNDXSQLiteJournalMode);
    { Returns the current journal mode. }
    function GetJournalMode: TNDXSQLiteJournalMode;
    { Sets the synchronous mode for disk writes.
      @param AMode  Sync mode (smOff, smNormal, smFull, smExtra). }
    procedure SetSyncMode(AMode: TNDXSQLiteSyncMode);
    { Returns the current synchronous mode. }
    function GetSyncMode: TNDXSQLiteSyncMode;
    { Sets the page cache size.
      @param ASizeKB  Cache size in kilobytes (negative = number of pages). }
    procedure SetCacheSize(ASizeKB: Integer);
    { Returns the current page cache size in kilobytes. }
    function GetCacheSize: Integer;
    { Sets the busy timeout for locked database retries.
      @param ATimeoutMs  Timeout in milliseconds (0 = return immediately). }
    procedure SetBusyTimeout(ATimeoutMs: Integer);
    { Returns the current busy timeout in milliseconds. }
    function GetBusyTimeout: Integer;
    { Enables or disables foreign key constraint enforcement.
      @param AEnabled  True to enable, False to disable. }
    procedure EnableForeignKeys(AEnabled: Boolean);
    { Returns True if foreign key constraints are currently enabled. }
    function IsForeignKeysEnabled: Boolean;
    { Reads a PRAGMA value by name.
      @param APragmaName  Name of the PRAGMA (e.g., 'page_size', 'integrity_check').
      @returns The PRAGMA value as a Variant. }
    function GetPragmaValue(const APragmaName: string): Variant;
    { Sets a PRAGMA value by name.
      @param APragmaName  Name of the PRAGMA to set.
      @param AValue       Value to assign to the PRAGMA. }
    procedure SetPragmaValue(const APragmaName: string; const AValue: Variant);

    // Backup

    { Creates a full backup of the database to a file.
      @param ADestPath  Destination file path for the backup. }
    procedure BackupTo(const ADestPath: string);
    { Restores the database from a backup file (replaces current content).
      @param ASourcePath  Source backup file path. }
    procedure RestoreFrom(const ASourcePath: string);
  end;

  { Interface for async connection }
  INDXSQLiteAsyncConnection = interface(INDXSQLiteConnection)
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']

    // Async connection

    { Opens the database connection asynchronously in a worker thread.
      @param AOnComplete        Callback invoked when the operation completes.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure OpenAsync(AOnComplete: TNDXAsyncCallback;
      ACancellationToken: INDXCancellationToken = nil);
    { Closes the database connection asynchronously.
      @param AOnComplete  Optional callback invoked when closing is complete. }
    procedure CloseAsync(AOnComplete: TNDXAsyncCallback = nil);

    // Async queries

    { Executes a non-query SQL statement asynchronously.
      @param ASQL               The SQL statement to execute.
      @param AOnComplete        Callback receiving the number of rows affected.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure ExecuteNonQueryAsync(const ASQL: string;
      AOnComplete: TNDXAsyncCallbackInt;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a parameterized non-query SQL statement asynchronously.
      @param ASQL               The SQL statement with positional placeholders (?).
      @param AParams            Array of parameter values bound in order.
      @param AOnComplete        Callback receiving the number of rows affected.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure ExecuteNonQueryAsync(const ASQL: string;
      const AParams: array of Variant;
      AOnComplete: TNDXAsyncCallbackInt;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a scalar query asynchronously.
      @param ASQL               The SQL query to execute.
      @param AOnComplete        Callback receiving the scalar result value.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure ExecuteScalarAsync(const ASQL: string;
      AOnComplete: TNDXAsyncCallbackVariant;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a parameterized scalar query asynchronously.
      @param ASQL               The SQL query with positional placeholders (?).
      @param AParams            Array of parameter values bound in order.
      @param AOnComplete        Callback receiving the scalar result value.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure ExecuteScalarAsync(const ASQL: string;
      const AParams: array of Variant;
      AOnComplete: TNDXAsyncCallbackVariant;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a query asynchronously and returns a TDataSet.
      @param ASQL               The SQL query to execute.
      @param AOnComplete        Callback receiving the result TDataSet.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure ExecuteQueryAsync(const ASQL: string;
      AOnComplete: TNDXAsyncCallbackDataSet;
      ACancellationToken: INDXCancellationToken = nil); overload;
    { Executes a parameterized query asynchronously and returns a TDataSet.
      @param ASQL               The SQL query with positional placeholders (?).
      @param AParams            Array of parameter values bound in order.
      @param AOnComplete        Callback receiving the result TDataSet.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure ExecuteQueryAsync(const ASQL: string;
      const AParams: array of Variant;
      AOnComplete: TNDXAsyncCallbackDataSet;
      ACancellationToken: INDXCancellationToken = nil); overload;

    // Async transactions

    { Begins a transaction asynchronously.
      @param AMode              Transaction mode (tmDeferred, tmImmediate, tmExclusive).
      @param AOnComplete        Callback receiving True if transaction started.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure BeginTransactionAsync(AMode: TNDXSQLiteTransactionMode;
      AOnComplete: TNDXAsyncCallbackBoolean;
      ACancellationToken: INDXCancellationToken = nil);
    { Commits the active transaction asynchronously.
      @param AOnComplete        Callback invoked when commit completes.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure CommitAsync(AOnComplete: TNDXAsyncCallback;
      ACancellationToken: INDXCancellationToken = nil);
    { Rolls back the active transaction asynchronously.
      @param AOnComplete  Optional callback invoked when rollback completes. }
    procedure RollbackAsync(AOnComplete: TNDXAsyncCallback = nil);

    // Async backup

    { Creates a database backup asynchronously with progress reporting.
      @param ADestPath          Destination file path for the backup.
      @param AOnProgress        Callback for progress updates (0..100).
      @param AOnComplete        Callback invoked when backup completes.
      @param ACancellationToken Optional token to cancel the operation. }
    procedure BackupToAsync(const ADestPath: string;
      AOnProgress: TNDXProgressCallback;
      AOnComplete: TNDXAsyncCallback;
      ACancellationToken: INDXCancellationToken = nil);

    // Operations management

    { Cancels all pending asynchronous operations on this connection. }
    procedure CancelAllPendingOperations;
    { Waits for all pending async operations to complete.
      @param ATimeoutMs  Maximum wait time in milliseconds (-1 = wait indefinitely). }
    procedure WaitForAllOperations(ATimeoutMs: Integer = -1);
    { Returns the number of async operations currently pending. }
    function GetPendingOperationsCount: Integer;
  end;

implementation

end.
