{===============================================================================
  NDXSQLite - Custom Exceptions
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 1 (depends on ndxsqlitetypes)
===============================================================================}
unit ndxsqliteexceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  { Common SQLite error codes }
  SQLITE_OK         = 0;
  SQLITE_ERROR      = 1;
  SQLITE_INTERNAL   = 2;
  SQLITE_PERM       = 3;
  SQLITE_ABORT      = 4;
  SQLITE_BUSY       = 5;
  SQLITE_LOCKED     = 6;
  SQLITE_NOMEM      = 7;
  SQLITE_READONLY   = 8;
  SQLITE_INTERRUPT  = 9;
  SQLITE_IOERR      = 10;
  SQLITE_CORRUPT    = 11;
  SQLITE_NOTFOUND   = 12;
  SQLITE_FULL       = 13;
  SQLITE_CANTOPEN   = 14;
  SQLITE_CONSTRAINT = 19;
  SQLITE_MISMATCH   = 20;
  SQLITE_MISUSE     = 21;

type
  { Base exception for all NDXSQLite errors, carrying an SQLite error code and context. }
  ENDXSQLiteException = class(Exception)
  private
    FErrorCode: Integer;
    FSQLiteMessage: string;
    FContext: string;
  public
    { Creates an exception with a message, optional SQLite error code, and context string. }
    constructor Create(const AMessage: string; AErrorCode: Integer = 0;
      const AContext: string = '');
    { Creates a formatted exception with arguments, optional error code, and context. }
    constructor CreateFmt(const AMessage: string; const AArgs: array of const;
      AErrorCode: Integer = 0; const AContext: string = '');

    property ErrorCode: Integer read FErrorCode;
    property SQLiteMessage: string read FSQLiteMessage write FSQLiteMessage;
    property Context: string read FContext;
  end;

  { Raised when a database connection cannot be established or is lost. }
  ENDXSQLiteConnectionException = class(ENDXSQLiteException)
  private
    FDatabasePath: string;
  public
    { Creates a connection exception with the failing database path. }
    constructor Create(const AMessage: string; const ADatabasePath: string;
      AErrorCode: Integer = 0);
    property DatabasePath: string read FDatabasePath;
  end;

  { Raised when a transaction operation fails (BEGIN, COMMIT, ROLLBACK). }
  ENDXSQLiteTransactionException = class(ENDXSQLiteException)
  private
    FTransactionMode: string;
  public
    { Creates a transaction exception indicating the mode that failed. }
    constructor Create(const AMessage: string; const ATransactionMode: string;
      AErrorCode: Integer = 0);
    property TransactionMode: string read FTransactionMode;
  end;

  { Raised when an SQL query or statement fails during execution. }
  ENDXSQLiteQueryException = class(ENDXSQLiteException)
  private
    FSQL: string;
  public
    { Creates a query exception with the offending SQL text. }
    constructor Create(const AMessage: string; const ASQL: string;
      AErrorCode: Integer = 0);
    property SQL: string read FSQL;
  end;

  { Raised when an operation exceeds the configured timeout. }
  ENDXSQLiteTimeoutException = class(ENDXSQLiteException)
  private
    FTimeoutMs: Integer;
    FOperationType: string;
  public
    { Creates a timeout exception for the specified operation type and duration. }
    constructor Create(const AOperationType: string; ATimeoutMs: Integer);
    property TimeoutMs: Integer read FTimeoutMs;
    property OperationType: string read FOperationType;
  end;

  { Raised when the database is locked by another connection or process. }
  ENDXSQLiteLockedException = class(ENDXSQLiteException)
  private
    FLockingTable: string;
  public
    { Creates a locked exception, optionally identifying the table causing the lock. }
    constructor Create(const AMessage: string; const ALockingTable: string = '';
      AErrorCode: Integer = SQLITE_BUSY);
    property LockingTable: string read FLockingTable;
  end;

  { Raised when database file corruption is detected. }
  ENDXSQLiteCorruptException = class(ENDXSQLiteException)
  private
    FDatabasePath: string;
  public
    { Creates a corruption exception for the specified database file. }
    constructor Create(const ADatabasePath: string);
    property DatabasePath: string read FDatabasePath;
  end;

  { Raised when a constraint (UNIQUE, CHECK, NOT NULL, FOREIGN KEY) is violated. }
  ENDXSQLiteConstraintException = class(ENDXSQLiteException)
  private
    FConstraintType: string;
    FConstraintName: string;
  public
    { Creates a constraint exception identifying the type and optional name of the constraint. }
    constructor Create(const AMessage: string; const AConstraintType: string;
      const AConstraintName: string = '');
    property ConstraintType: string read FConstraintType;
    property ConstraintName: string read FConstraintName;
  end;

  { Raised when an operation is canceled via a cancellation token. }
  ENDXSQLiteCanceledException = class(ENDXSQLiteException)
  public
    { Creates a cancellation exception for the specified operation type. }
    constructor Create(const AOperationType: string = 'Operation');
  end;

  { Raised when attempting to use an object that has been disposed. }
  ENDXSQLiteDisposedException = class(ENDXSQLiteException)
  public
    { Creates a disposed exception naming the already-freed object. }
    constructor Create(const AObjectName: string);
  end;

  { Raised when a connection pool operation fails. }
  ENDXSQLitePoolException = class(ENDXSQLiteException)
  public
    { Creates a pool exception with the given error message. }
    constructor Create(const AMessage: string);
  end;

  { Raised when the SQLite shared library cannot be loaded. }
  ENDXSQLiteLibraryException = class(ENDXSQLiteException)
  public
    { Creates a library exception with details about the loading failure. }
    constructor Create(const AMessage: string);
  end;

  { Raised when the database is busy and the busy timeout has expired. }
  ENDXSQLiteBusyException = class(ENDXSQLiteException)
  public
    { Creates a busy exception with the given error message. }
    constructor Create(const AMessage: string);
  end;

  { Raised when a write operation is attempted on a read-only database. }
  ENDXSQLiteReadOnlyException = class(ENDXSQLiteException)
  public
    { Creates a read-only exception with the given error message. }
    constructor Create(const AMessage: string);
  end;

  { Raised when a disk I/O error occurs during database operations. }
  ENDXSQLiteIOException = class(ENDXSQLiteException)
  public
    { Creates an I/O exception with the given error message. }
    constructor Create(const AMessage: string);
  end;

  { Raised when the disk is full and the database cannot grow. }
  ENDXSQLiteDiskFullException = class(ENDXSQLiteException)
  public
    { Creates a disk full exception with the given error message. }
    constructor Create(const AMessage: string);
  end;

  { Raised when the database file cannot be opened. }
  ENDXSQLiteCantOpenException = class(ENDXSQLiteException)
  public
    { Creates a can't-open exception with the given error message. }
    constructor Create(const AMessage: string);
  end;

implementation

{ ENDXSQLiteException }

constructor ENDXSQLiteException.Create(const AMessage: string;
  AErrorCode: Integer; const AContext: string);
begin
  inherited Create(AMessage);
  FErrorCode := AErrorCode;
  FContext := AContext;
end;

constructor ENDXSQLiteException.CreateFmt(const AMessage: string;
  const AArgs: array of const; AErrorCode: Integer; const AContext: string);
begin
  inherited CreateFmt(AMessage, AArgs);
  FErrorCode := AErrorCode;
  FContext := AContext;
end;

{ ENDXSQLiteConnectionException }

constructor ENDXSQLiteConnectionException.Create(const AMessage: string;
  const ADatabasePath: string; AErrorCode: Integer);
begin
  inherited Create(AMessage, AErrorCode, 'Connection');
  FDatabasePath := ADatabasePath;
end;

{ ENDXSQLiteTransactionException }

constructor ENDXSQLiteTransactionException.Create(const AMessage: string;
  const ATransactionMode: string; AErrorCode: Integer);
begin
  inherited Create(AMessage, AErrorCode, 'Transaction');
  FTransactionMode := ATransactionMode;
end;

{ ENDXSQLiteQueryException }

constructor ENDXSQLiteQueryException.Create(const AMessage: string;
  const ASQL: string; AErrorCode: Integer);
begin
  inherited Create(AMessage, AErrorCode, 'Query');
  FSQL := ASQL;
end;

{ ENDXSQLiteTimeoutException }

constructor ENDXSQLiteTimeoutException.Create(const AOperationType: string;
  ATimeoutMs: Integer);
begin
  inherited Create(Format('Timeout after %d ms for operation: %s',
    [ATimeoutMs, AOperationType]), SQLITE_BUSY, 'Timeout');
  FTimeoutMs := ATimeoutMs;
  FOperationType := AOperationType;
end;

{ ENDXSQLiteLockedException }

constructor ENDXSQLiteLockedException.Create(const AMessage: string;
  const ALockingTable: string; AErrorCode: Integer);
begin
  inherited Create(AMessage, AErrorCode, 'Lock');
  FLockingTable := ALockingTable;
end;

{ ENDXSQLiteCorruptException }

constructor ENDXSQLiteCorruptException.Create(const ADatabasePath: string);
begin
  inherited Create(Format('Database corrupted: %s', [ADatabasePath]),
    SQLITE_CORRUPT, 'Integrity');
  FDatabasePath := ADatabasePath;
end;

{ ENDXSQLiteConstraintException }

constructor ENDXSQLiteConstraintException.Create(const AMessage: string;
  const AConstraintType: string; const AConstraintName: string);
begin
  inherited Create(AMessage, SQLITE_CONSTRAINT, 'Constraint');
  FConstraintType := AConstraintType;
  FConstraintName := AConstraintName;
end;

{ ENDXSQLiteCanceledException }

constructor ENDXSQLiteCanceledException.Create(const AOperationType: string);
begin
  inherited Create(Format('%s canceled by user', [AOperationType]),
    SQLITE_INTERRUPT, 'Cancellation');
end;

{ ENDXSQLiteDisposedException }

constructor ENDXSQLiteDisposedException.Create(const AObjectName: string);
begin
  inherited Create(Format('Cannot access a disposed object: %s',
    [AObjectName]), 0, 'Disposed');
end;

{ ENDXSQLitePoolException }

constructor ENDXSQLitePoolException.Create(const AMessage: string);
begin
  inherited Create(AMessage, 0, 'Pool');
end;

{ ENDXSQLiteLibraryException }

constructor ENDXSQLiteLibraryException.Create(const AMessage: string);
begin
  inherited Create(AMessage, 0, 'Library');
end;

{ ENDXSQLiteBusyException }

constructor ENDXSQLiteBusyException.Create(const AMessage: string);
begin
  inherited Create(AMessage, SQLITE_BUSY, 'Busy');
end;

{ ENDXSQLiteReadOnlyException }

constructor ENDXSQLiteReadOnlyException.Create(const AMessage: string);
begin
  inherited Create(AMessage, SQLITE_READONLY, 'ReadOnly');
end;

{ ENDXSQLiteIOException }

constructor ENDXSQLiteIOException.Create(const AMessage: string);
begin
  inherited Create(AMessage, SQLITE_IOERR, 'IO');
end;

{ ENDXSQLiteDiskFullException }

constructor ENDXSQLiteDiskFullException.Create(const AMessage: string);
begin
  inherited Create(AMessage, SQLITE_FULL, 'DiskFull');
end;

{ ENDXSQLiteCantOpenException }

constructor ENDXSQLiteCantOpenException.Create(const AMessage: string);
begin
  inherited Create(AMessage, SQLITE_CANTOPEN, 'CantOpen');
end;

end.
