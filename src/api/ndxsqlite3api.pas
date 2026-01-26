{===============================================================================
  NDXSQLite - SQLite3 C API Declarations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 0 (no dependencies)

  Complete SQLite3 API declarations for direct native access.
  This unit provides the foundation for the entire NDXSQLite library.
===============================================================================}
unit ndxsqlite3api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynLibs;

const
  {$IFDEF WINDOWS}
  SQLITE3_LIB = 'sqlite3.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  SQLITE3_LIB = 'libsqlite3.so.0';
  {$ENDIF}
  {$IFDEF DARWIN}
  SQLITE3_LIB = 'libsqlite3.dylib';
  {$ENDIF}
  {$IFDEF FREEBSD}
  SQLITE3_LIB = 'libsqlite3.so';
  {$ENDIF}

  // Fallback library names for different platforms/configurations
  {$IFDEF WINDOWS}
  SQLITE3_LIB_ALT: array[0..4] of string = (
    // SQLCipher first (for encryption support)
    'sqlcipher.dll',
    'libsqlcipher.dll',
    // Standard SQLite fallback
    'sqlite3.dll',
    'libsqlite3.dll',
    'sqlite3-64.dll'
  );
  {$ENDIF}
  {$IFDEF LINUX}
  SQLITE3_LIB_ALT: array[0..10] of string = (
    // SQLCipher first (for encryption support)
    'libsqlcipher.so.1',
    '/usr/lib/x86_64-linux-gnu/libsqlcipher.so.1',  // Debian/Ubuntu x64
    '/usr/lib/aarch64-linux-gnu/libsqlcipher.so.1', // Debian/Ubuntu ARM64
    '/usr/lib64/libsqlcipher.so.1',                 // RHEL/CentOS
    '/usr/local/lib/libsqlcipher.so.1',             // Local install
    // Standard SQLite fallback
    'libsqlite3.so.0',
    'libsqlite3.so',
    '/usr/lib/x86_64-linux-gnu/libsqlite3.so.0',  // Debian/Ubuntu
    '/usr/lib64/libsqlite3.so.0',                  // RHEL/CentOS
    '/snap/core/current/usr/lib/x86_64-linux-gnu/libsqlite3.so.0',  // Snap
    '/app/lib/libsqlite3.so.0'                     // Flatpak
  );
  {$ENDIF}
  {$IFDEF DARWIN}
  SQLITE3_LIB_ALT: array[0..6] of string = (
    // SQLCipher first (for encryption support)
    'libsqlcipher.dylib',
    '/opt/homebrew/lib/libsqlcipher.dylib',       // Homebrew ARM
    '/usr/local/lib/libsqlcipher.dylib',          // Homebrew Intel
    // Standard SQLite fallback
    'libsqlite3.dylib',
    '/usr/lib/libsqlite3.dylib',
    '/opt/homebrew/lib/libsqlite3.dylib',         // Homebrew ARM
    '/usr/local/lib/libsqlite3.dylib'             // Homebrew Intel
  );
  {$ENDIF}
  {$IFDEF FREEBSD}
  SQLITE3_LIB_ALT: array[0..1] of string = (
    'libsqlite3.so',
    '/usr/local/lib/libsqlite3.so'
  );
  {$ENDIF}

  // Result codes
  SQLITE_OK         = 0;   // Successful result
  SQLITE_ERROR      = 1;   // Generic error
  SQLITE_INTERNAL   = 2;   // Internal logic error in SQLite
  SQLITE_PERM       = 3;   // Access permission denied
  SQLITE_ABORT      = 4;   // Callback routine requested an abort
  SQLITE_BUSY       = 5;   // The database file is locked
  SQLITE_LOCKED     = 6;   // A table in the database is locked
  SQLITE_NOMEM      = 7;   // A malloc() failed
  SQLITE_READONLY   = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT  = 9;   // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR      = 10;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;  // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;  // Unknown opcode in sqlite3_file_control()
  SQLITE_FULL       = 13;  // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;  // Unable to open the database file
  SQLITE_PROTOCOL   = 15;  // Database lock protocol error
  SQLITE_EMPTY      = 16;  // Internal use only
  SQLITE_SCHEMA     = 17;  // The database schema changed
  SQLITE_TOOBIG     = 18;  // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT = 19;  // Abort due to constraint violation
  SQLITE_MISMATCH   = 20;  // Data type mismatch
  SQLITE_MISUSE     = 21;  // Library used incorrectly
  SQLITE_NOLFS      = 22;  // Uses OS features not supported on host
  SQLITE_AUTH       = 23;  // Authorization denied
  SQLITE_FORMAT     = 24;  // Not used
  SQLITE_RANGE      = 25;  // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB     = 26;  // File opened that is not a database file
  SQLITE_NOTICE     = 27;  // Notifications from sqlite3_log()
  SQLITE_WARNING    = 28;  // Warnings from sqlite3_log()
  SQLITE_ROW        = 100; // sqlite3_step() has another row ready
  SQLITE_DONE       = 101; // sqlite3_step() has finished executing

  // Extended result codes
  SQLITE_ERROR_MISSING_COLLSEQ   = SQLITE_ERROR or (1 shl 8);
  SQLITE_ERROR_RETRY             = SQLITE_ERROR or (2 shl 8);
  SQLITE_ERROR_SNAPSHOT          = SQLITE_ERROR or (3 shl 8);
  SQLITE_IOERR_READ              = SQLITE_IOERR or (1 shl 8);
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR or (2 shl 8);
  SQLITE_IOERR_WRITE             = SQLITE_IOERR or (3 shl 8);
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR or (4 shl 8);
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR or (5 shl 8);
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR or (6 shl 8);
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR or (7 shl 8);
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR or (8 shl 8);
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR or (9 shl 8);
  SQLITE_IOERR_DELETE            = SQLITE_IOERR or (10 shl 8);
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR or (11 shl 8);
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR or (12 shl 8);
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR or (13 shl 8);
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR or (14 shl 8);
  SQLITE_IOERR_LOCK              = SQLITE_IOERR or (15 shl 8);
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR or (16 shl 8);
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR or (17 shl 8);
  SQLITE_IOERR_SHMOPEN           = SQLITE_IOERR or (18 shl 8);
  SQLITE_IOERR_SHMSIZE           = SQLITE_IOERR or (19 shl 8);
  SQLITE_IOERR_SHMLOCK           = SQLITE_IOERR or (20 shl 8);
  SQLITE_IOERR_SHMMAP            = SQLITE_IOERR or (21 shl 8);
  SQLITE_IOERR_SEEK              = SQLITE_IOERR or (22 shl 8);
  SQLITE_IOERR_DELETE_NOENT      = SQLITE_IOERR or (23 shl 8);
  SQLITE_IOERR_MMAP              = SQLITE_IOERR or (24 shl 8);
  SQLITE_IOERR_GETTEMPPATH       = SQLITE_IOERR or (25 shl 8);
  SQLITE_IOERR_CONVPATH          = SQLITE_IOERR or (26 shl 8);
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED or (1 shl 8);
  SQLITE_LOCKED_VTAB             = SQLITE_LOCKED or (2 shl 8);
  SQLITE_BUSY_RECOVERY           = SQLITE_BUSY or (1 shl 8);
  SQLITE_BUSY_SNAPSHOT           = SQLITE_BUSY or (2 shl 8);
  SQLITE_BUSY_TIMEOUT            = SQLITE_BUSY or (3 shl 8);
  SQLITE_CANTOPEN_NOTEMPDIR      = SQLITE_CANTOPEN or (1 shl 8);
  SQLITE_CANTOPEN_ISDIR          = SQLITE_CANTOPEN or (2 shl 8);
  SQLITE_CANTOPEN_FULLPATH       = SQLITE_CANTOPEN or (3 shl 8);
  SQLITE_CANTOPEN_CONVPATH       = SQLITE_CANTOPEN or (4 shl 8);
  SQLITE_CORRUPT_VTAB            = SQLITE_CORRUPT or (1 shl 8);
  SQLITE_CORRUPT_SEQUENCE        = SQLITE_CORRUPT or (2 shl 8);
  SQLITE_READONLY_RECOVERY       = SQLITE_READONLY or (1 shl 8);
  SQLITE_READONLY_CANTLOCK       = SQLITE_READONLY or (2 shl 8);
  SQLITE_READONLY_ROLLBACK       = SQLITE_READONLY or (3 shl 8);
  SQLITE_READONLY_DBMOVED        = SQLITE_READONLY or (4 shl 8);
  SQLITE_READONLY_CANTINIT       = SQLITE_READONLY or (5 shl 8);
  SQLITE_READONLY_DIRECTORY      = SQLITE_READONLY or (6 shl 8);
  SQLITE_ABORT_ROLLBACK          = SQLITE_ABORT or (2 shl 8);
  SQLITE_CONSTRAINT_CHECK        = SQLITE_CONSTRAINT or (1 shl 8);
  SQLITE_CONSTRAINT_COMMITHOOK   = SQLITE_CONSTRAINT or (2 shl 8);
  SQLITE_CONSTRAINT_FOREIGNKEY   = SQLITE_CONSTRAINT or (3 shl 8);
  SQLITE_CONSTRAINT_FUNCTION     = SQLITE_CONSTRAINT or (4 shl 8);
  SQLITE_CONSTRAINT_NOTNULL      = SQLITE_CONSTRAINT or (5 shl 8);
  SQLITE_CONSTRAINT_PRIMARYKEY   = SQLITE_CONSTRAINT or (6 shl 8);
  SQLITE_CONSTRAINT_TRIGGER      = SQLITE_CONSTRAINT or (7 shl 8);
  SQLITE_CONSTRAINT_UNIQUE       = SQLITE_CONSTRAINT or (8 shl 8);
  SQLITE_CONSTRAINT_VTAB         = SQLITE_CONSTRAINT or (9 shl 8);
  SQLITE_CONSTRAINT_ROWID        = SQLITE_CONSTRAINT or (10 shl 8);
  SQLITE_NOTICE_RECOVER_WAL      = SQLITE_NOTICE or (1 shl 8);
  SQLITE_NOTICE_RECOVER_ROLLBACK = SQLITE_NOTICE or (2 shl 8);
  SQLITE_WARNING_AUTOINDEX       = SQLITE_WARNING or (1 shl 8);
  SQLITE_AUTH_USER               = SQLITE_AUTH or (1 shl 8);
  SQLITE_OK_LOAD_PERMANENTLY     = SQLITE_OK or (1 shl 8);

  // Open flags
  SQLITE_OPEN_READONLY       = $00000001;
  SQLITE_OPEN_READWRITE      = $00000002;
  SQLITE_OPEN_CREATE         = $00000004;
  SQLITE_OPEN_DELETEONCLOSE  = $00000008;
  SQLITE_OPEN_EXCLUSIVE      = $00000010;
  SQLITE_OPEN_AUTOPROXY      = $00000020;
  SQLITE_OPEN_URI            = $00000040;
  SQLITE_OPEN_MEMORY         = $00000080;
  SQLITE_OPEN_MAIN_DB        = $00000100;
  SQLITE_OPEN_TEMP_DB        = $00000200;
  SQLITE_OPEN_TRANSIENT_DB   = $00000400;
  SQLITE_OPEN_MAIN_JOURNAL   = $00000800;
  SQLITE_OPEN_TEMP_JOURNAL   = $00001000;
  SQLITE_OPEN_SUBJOURNAL     = $00002000;
  SQLITE_OPEN_SUPER_JOURNAL  = $00004000;
  SQLITE_OPEN_NOMUTEX        = $00008000;
  SQLITE_OPEN_FULLMUTEX      = $00010000;
  SQLITE_OPEN_SHAREDCACHE    = $00020000;
  SQLITE_OPEN_PRIVATECACHE   = $00040000;
  SQLITE_OPEN_WAL            = $00080000;
  SQLITE_OPEN_NOFOLLOW       = $01000000;

  // Fundamental datatypes
  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  // Text encodings
  SQLITE_UTF8          = 1;
  SQLITE_UTF16LE       = 2;
  SQLITE_UTF16BE       = 3;
  SQLITE_UTF16         = 4;
  SQLITE_ANY           = 5;
  SQLITE_UTF16_ALIGNED = 8;

  // Destructor behavior constants
  SQLITE_STATIC    = Pointer(0);
  SQLITE_TRANSIENT = Pointer(-1);

  // Checkpoint modes
  SQLITE_CHECKPOINT_PASSIVE  = 0;
  SQLITE_CHECKPOINT_FULL     = 1;
  SQLITE_CHECKPOINT_RESTART  = 2;
  SQLITE_CHECKPOINT_TRUNCATE = 3;

  // Limit categories
  SQLITE_LIMIT_LENGTH              = 0;
  SQLITE_LIMIT_SQL_LENGTH          = 1;
  SQLITE_LIMIT_COLUMN              = 2;
  SQLITE_LIMIT_EXPR_DEPTH          = 3;
  SQLITE_LIMIT_COMPOUND_SELECT     = 4;
  SQLITE_LIMIT_VDBE_OP             = 5;
  SQLITE_LIMIT_FUNCTION_ARG        = 6;
  SQLITE_LIMIT_ATTACHED            = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER     = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH       = 10;
  SQLITE_LIMIT_WORKER_THREADS      = 11;

  // Status parameters
  SQLITE_STATUS_MEMORY_USED          = 0;
  SQLITE_STATUS_PAGECACHE_USED       = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW   = 2;
  SQLITE_STATUS_SCRATCH_USED         = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW     = 4;
  SQLITE_STATUS_MALLOC_SIZE          = 5;
  SQLITE_STATUS_PARSER_STACK         = 6;
  SQLITE_STATUS_PAGECACHE_SIZE       = 7;
  SQLITE_STATUS_SCRATCH_SIZE         = 8;
  SQLITE_STATUS_MALLOC_COUNT         = 9;

  // Database connection status parameters
  SQLITE_DBSTATUS_LOOKASIDE_USED      = 0;
  SQLITE_DBSTATUS_CACHE_USED          = 1;
  SQLITE_DBSTATUS_SCHEMA_USED         = 2;
  SQLITE_DBSTATUS_STMT_USED           = 3;
  SQLITE_DBSTATUS_LOOKASIDE_HIT       = 4;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE = 5;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL = 6;
  SQLITE_DBSTATUS_CACHE_HIT           = 7;
  SQLITE_DBSTATUS_CACHE_MISS          = 8;
  SQLITE_DBSTATUS_CACHE_WRITE         = 9;
  SQLITE_DBSTATUS_DEFERRED_FKS        = 10;
  SQLITE_DBSTATUS_CACHE_USED_SHARED   = 11;
  SQLITE_DBSTATUS_CACHE_SPILL         = 12;
  SQLITE_DBSTATUS_MAX                 = 12;

  // Statement status parameters
  SQLITE_STMTSTATUS_FULLSCAN_STEP = 1;
  SQLITE_STMTSTATUS_SORT          = 2;
  SQLITE_STMTSTATUS_AUTOINDEX     = 3;
  SQLITE_STMTSTATUS_VM_STEP       = 4;
  SQLITE_STMTSTATUS_REPREPARE     = 5;
  SQLITE_STMTSTATUS_RUN           = 6;
  SQLITE_STMTSTATUS_MEMUSED       = 99;

  // Authorizer action codes
  SQLITE_CREATE_INDEX        = 1;
  SQLITE_CREATE_TABLE        = 2;
  SQLITE_CREATE_TEMP_INDEX   = 3;
  SQLITE_CREATE_TEMP_TABLE   = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW    = 6;
  SQLITE_CREATE_TRIGGER      = 7;
  SQLITE_CREATE_VIEW         = 8;
  SQLITE_DELETE              = 9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              = 23;
  SQLITE_ATTACH              = 24;
  SQLITE_DETACH              = 25;
  SQLITE_ALTER_TABLE         = 26;
  SQLITE_REINDEX             = 27;
  SQLITE_ANALYZE             = 28;
  SQLITE_CREATE_VTABLE       = 29;
  SQLITE_DROP_VTABLE         = 30;
  SQLITE_FUNCTION            = 31;
  SQLITE_SAVEPOINT           = 32;
  SQLITE_COPY                = 0;
  SQLITE_RECURSIVE           = 33;

  // Authorizer return codes
  SQLITE_DENY   = 1;
  SQLITE_IGNORE = 2;

type
  // Opaque structure pointers

  { Pointer to an open database connection handle. }
  Psqlite3 = ^sqlite3;
  sqlite3 = record end;

  { Pointer to a prepared statement object. }
  Psqlite3_stmt = ^sqlite3_stmt;
  sqlite3_stmt = record end;

  { Pointer to an open BLOB I/O handle. }
  Psqlite3_blob = ^sqlite3_blob;
  sqlite3_blob = record end;

  { Pointer to an online backup operation handle. }
  Psqlite3_backup = ^sqlite3_backup;
  sqlite3_backup = record end;

  { Pointer to a dynamically typed SQLite value. }
  Psqlite3_value = ^sqlite3_value;
  sqlite3_value = record end;

  { Pointer to a user-defined function execution context. }
  Psqlite3_context = ^sqlite3_context;
  sqlite3_context = record end;

  { Pointer to an array of sqlite3_value pointers (for function arguments). }
  PPsqlite3_value = ^Psqlite3_value;

  // Callback function types
  TSQLite3Callback = function(pArg: Pointer; nCol: Integer;
    azVals: PPAnsiChar; azNames: PPAnsiChar): Integer; cdecl;

  TSQLite3BusyCallback = function(pArg: Pointer; count: Integer): Integer; cdecl;

  TSQLite3ProgressCallback = function(pArg: Pointer): Integer; cdecl;

  TSQLite3AuthorizerCallback = function(pUserData: Pointer; actionCode: Integer;
    det1, det2, det3, det4: PAnsiChar): Integer; cdecl;

  TSQLite3TraceCallback = procedure(pArg: Pointer; zSQL: PAnsiChar); cdecl;

  TSQLite3ProfileCallback = procedure(pArg: Pointer; zSQL: PAnsiChar;
    time: UInt64); cdecl;

  TSQLite3CommitCallback = function(pArg: Pointer): Integer; cdecl;

  TSQLite3RollbackCallback = procedure(pArg: Pointer); cdecl;

  TSQLite3UpdateCallback = procedure(pArg: Pointer; op: Integer;
    zDb, zTable: PAnsiChar; rowid: Int64); cdecl;

  TSQLite3PreupdateCallback = procedure(pCtx: Pointer; db: Psqlite3;
    op: Integer; zDb, zTable: PAnsiChar; iKey1, iKey2: Int64); cdecl;

  TSQLite3UnlockNotifyCallback = procedure(apArg: PPointer; nArg: Integer); cdecl;

  TSQLite3CollationNeededCallback = procedure(pArg: Pointer; db: Psqlite3;
    eTextRep: Integer; zName: PAnsiChar); cdecl;

  TSQLite3DestructorCallback = procedure(pArg: Pointer); cdecl;

  TSQLite3FunctionCallback = procedure(ctx: Psqlite3_context;
    argc: Integer; argv: PPsqlite3_value); cdecl;

  TSQLite3FunctionFinalCallback = procedure(ctx: Psqlite3_context); cdecl;

  TSQLite3CompareCallback = function(pArg: Pointer;
    nKey1: Integer; pKey1: Pointer;
    nKey2: Integer; pKey2: Pointer): Integer; cdecl;

var
  // Library handle
  SQLite3LibHandle: TLibHandle = NilHandle;

  // Core functions
  sqlite3_libversion: function: PAnsiChar; cdecl;
  sqlite3_libversion_number: function: Integer; cdecl;
  sqlite3_sourceid: function: PAnsiChar; cdecl;
  sqlite3_threadsafe: function: Integer; cdecl;

  // Database connection
  sqlite3_open: function(filename: PAnsiChar; var ppDb: Psqlite3): Integer; cdecl;
  sqlite3_open16: function(filename: PWideChar; var ppDb: Psqlite3): Integer; cdecl;
  sqlite3_open_v2: function(filename: PAnsiChar; var ppDb: Psqlite3;
    flags: Integer; zVfs: PAnsiChar): Integer; cdecl;
  sqlite3_close: function(db: Psqlite3): Integer; cdecl;
  sqlite3_close_v2: function(db: Psqlite3): Integer; cdecl;

  // Error handling
  sqlite3_errcode: function(db: Psqlite3): Integer; cdecl;
  sqlite3_extended_errcode: function(db: Psqlite3): Integer; cdecl;
  sqlite3_errmsg: function(db: Psqlite3): PAnsiChar; cdecl;
  sqlite3_errmsg16: function(db: Psqlite3): PWideChar; cdecl;
  sqlite3_errstr: function(rc: Integer): PAnsiChar; cdecl;

  // Execution
  sqlite3_exec: function(db: Psqlite3; sql: PAnsiChar;
    callback: TSQLite3Callback; callbackArg: Pointer;
    errmsg: PPAnsiChar): Integer; cdecl;

  // Prepared statements
  sqlite3_prepare: function(db: Psqlite3; zSql: PAnsiChar; nByte: Integer;
    var ppStmt: Psqlite3_stmt; pzTail: PPAnsiChar): Integer; cdecl;
  sqlite3_prepare_v2: function(db: Psqlite3; zSql: PAnsiChar; nByte: Integer;
    var ppStmt: Psqlite3_stmt; pzTail: PPAnsiChar): Integer; cdecl;
  sqlite3_prepare_v3: function(db: Psqlite3; zSql: PAnsiChar; nByte: Integer;
    prepFlags: Cardinal; var ppStmt: Psqlite3_stmt; pzTail: PPAnsiChar): Integer; cdecl;
  sqlite3_prepare16: function(db: Psqlite3; zSql: PWideChar; nByte: Integer;
    var ppStmt: Psqlite3_stmt; pzTail: PPWideChar): Integer; cdecl;
  sqlite3_prepare16_v2: function(db: Psqlite3; zSql: PWideChar; nByte: Integer;
    var ppStmt: Psqlite3_stmt; pzTail: PPWideChar): Integer; cdecl;
  sqlite3_prepare16_v3: function(db: Psqlite3; zSql: PWideChar; nByte: Integer;
    prepFlags: Cardinal; var ppStmt: Psqlite3_stmt; pzTail: PPWideChar): Integer; cdecl;

  // Statement execution
  sqlite3_step: function(stmt: Psqlite3_stmt): Integer; cdecl;
  sqlite3_reset: function(stmt: Psqlite3_stmt): Integer; cdecl;
  sqlite3_finalize: function(stmt: Psqlite3_stmt): Integer; cdecl;

  // Binding parameters
  sqlite3_bind_parameter_count: function(stmt: Psqlite3_stmt): Integer; cdecl;
  sqlite3_bind_parameter_name: function(stmt: Psqlite3_stmt; idx: Integer): PAnsiChar; cdecl;
  sqlite3_bind_parameter_index: function(stmt: Psqlite3_stmt; zName: PAnsiChar): Integer; cdecl;
  sqlite3_clear_bindings: function(stmt: Psqlite3_stmt): Integer; cdecl;

  sqlite3_bind_null: function(stmt: Psqlite3_stmt; idx: Integer): Integer; cdecl;
  sqlite3_bind_int: function(stmt: Psqlite3_stmt; idx: Integer; val: Integer): Integer; cdecl;
  sqlite3_bind_int64: function(stmt: Psqlite3_stmt; idx: Integer; val: Int64): Integer; cdecl;
  sqlite3_bind_double: function(stmt: Psqlite3_stmt; idx: Integer; val: Double): Integer; cdecl;
  sqlite3_bind_text: function(stmt: Psqlite3_stmt; idx: Integer; val: PAnsiChar;
    n: Integer; xDel: Pointer): Integer; cdecl;
  sqlite3_bind_text16: function(stmt: Psqlite3_stmt; idx: Integer; val: PWideChar;
    n: Integer; xDel: Pointer): Integer; cdecl;
  sqlite3_bind_blob: function(stmt: Psqlite3_stmt; idx: Integer; val: Pointer;
    n: Integer; xDel: Pointer): Integer; cdecl;
  sqlite3_bind_blob64: function(stmt: Psqlite3_stmt; idx: Integer; val: Pointer;
    n: UInt64; xDel: Pointer): Integer; cdecl;
  sqlite3_bind_zeroblob: function(stmt: Psqlite3_stmt; idx: Integer; n: Integer): Integer; cdecl;
  sqlite3_bind_zeroblob64: function(stmt: Psqlite3_stmt; idx: Integer; n: UInt64): Integer; cdecl;
  sqlite3_bind_value: function(stmt: Psqlite3_stmt; idx: Integer; val: Psqlite3_value): Integer; cdecl;
  sqlite3_bind_pointer: function(stmt: Psqlite3_stmt; idx: Integer; ptr: Pointer;
    zType: PAnsiChar; xDel: Pointer): Integer; cdecl;

  // Column information
  sqlite3_column_count: function(stmt: Psqlite3_stmt): Integer; cdecl;
  sqlite3_column_name: function(stmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_name16: function(stmt: Psqlite3_stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_database_name: function(stmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_database_name16: function(stmt: Psqlite3_stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_table_name: function(stmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_table_name16: function(stmt: Psqlite3_stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_origin_name: function(stmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_origin_name16: function(stmt: Psqlite3_stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_decltype: function(stmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
  sqlite3_column_decltype16: function(stmt: Psqlite3_stmt; N: Integer): PWideChar; cdecl;

  // Column values
  sqlite3_column_type: function(stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_int: function(stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_int64: function(stmt: Psqlite3_stmt; iCol: Integer): Int64; cdecl;
  sqlite3_column_double: function(stmt: Psqlite3_stmt; iCol: Integer): Double; cdecl;
  sqlite3_column_text: function(stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
  sqlite3_column_text16: function(stmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;
  sqlite3_column_blob: function(stmt: Psqlite3_stmt; iCol: Integer): Pointer; cdecl;
  sqlite3_column_bytes: function(stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_bytes16: function(stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_value: function(stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value; cdecl;

  // SQL utility
  sqlite3_sql: function(stmt: Psqlite3_stmt): PAnsiChar; cdecl;
  sqlite3_expanded_sql: function(stmt: Psqlite3_stmt): PAnsiChar; cdecl;
  sqlite3_normalized_sql: function(stmt: Psqlite3_stmt): PAnsiChar; cdecl;
  sqlite3_stmt_readonly: function(stmt: Psqlite3_stmt): Integer; cdecl;
  sqlite3_stmt_isexplain: function(stmt: Psqlite3_stmt): Integer; cdecl;
  sqlite3_stmt_busy: function(stmt: Psqlite3_stmt): Integer; cdecl;
  sqlite3_stmt_status: function(stmt: Psqlite3_stmt; op: Integer; resetFlg: Integer): Integer; cdecl;

  // Database utility
  sqlite3_changes: function(db: Psqlite3): Integer; cdecl;
  sqlite3_changes64: function(db: Psqlite3): Int64; cdecl;
  sqlite3_total_changes: function(db: Psqlite3): Integer; cdecl;
  sqlite3_total_changes64: function(db: Psqlite3): Int64; cdecl;
  sqlite3_last_insert_rowid: function(db: Psqlite3): Int64; cdecl;
  sqlite3_set_last_insert_rowid: procedure(db: Psqlite3; rowid: Int64); cdecl;
  sqlite3_interrupt: procedure(db: Psqlite3); cdecl;
  sqlite3_is_interrupted: function(db: Psqlite3): Integer; cdecl;

  // Busy handling
  sqlite3_busy_handler: function(db: Psqlite3; callback: TSQLite3BusyCallback;
    pArg: Pointer): Integer; cdecl;
  sqlite3_busy_timeout: function(db: Psqlite3; ms: Integer): Integer; cdecl;

  // Progress callback
  sqlite3_progress_handler: procedure(db: Psqlite3; nOps: Integer;
    callback: TSQLite3ProgressCallback; pArg: Pointer); cdecl;

  // Authorizer
  sqlite3_set_authorizer: function(db: Psqlite3;
    callback: TSQLite3AuthorizerCallback; pUserData: Pointer): Integer; cdecl;

  // Tracing
  sqlite3_trace: function(db: Psqlite3; callback: TSQLite3TraceCallback;
    pArg: Pointer): Pointer; cdecl;
  sqlite3_profile: function(db: Psqlite3; callback: TSQLite3ProfileCallback;
    pArg: Pointer): Pointer; cdecl;

  // Commit/Rollback hooks
  sqlite3_commit_hook: function(db: Psqlite3; callback: TSQLite3CommitCallback;
    pArg: Pointer): Pointer; cdecl;
  sqlite3_rollback_hook: function(db: Psqlite3; callback: TSQLite3RollbackCallback;
    pArg: Pointer): Pointer; cdecl;

  // Update hook
  sqlite3_update_hook: function(db: Psqlite3; callback: TSQLite3UpdateCallback;
    pArg: Pointer): Pointer; cdecl;

  // Preupdate hook (requires SQLITE_ENABLE_PREUPDATE_HOOK)
  sqlite3_preupdate_hook: function(db: Psqlite3;
    callback: TSQLite3PreupdateCallback; pCtx: Pointer): Pointer; cdecl;
  sqlite3_preupdate_old: function(db: Psqlite3; col: Integer;
    var pValue: Psqlite3_value): Integer; cdecl;
  sqlite3_preupdate_new: function(db: Psqlite3; col: Integer;
    var pValue: Psqlite3_value): Integer; cdecl;
  sqlite3_preupdate_count: function(db: Psqlite3): Integer; cdecl;
  sqlite3_preupdate_depth: function(db: Psqlite3): Integer; cdecl;

  // Unlock notify
  sqlite3_unlock_notify: function(pBlocked: Psqlite3;
    callback: TSQLite3UnlockNotifyCallback; pNotifyArg: Pointer): Integer; cdecl;

  // Limits
  sqlite3_limit: function(db: Psqlite3; id: Integer; newVal: Integer): Integer; cdecl;

  // Status
  sqlite3_status: function(op: Integer; var pCurrent: Integer;
    var pHighwater: Integer; resetFlag: Integer): Integer; cdecl;
  sqlite3_status64: function(op: Integer; var pCurrent: Int64;
    var pHighwater: Int64; resetFlag: Integer): Integer; cdecl;
  sqlite3_db_status: function(db: Psqlite3; op: Integer; var pCurrent: Integer;
    var pHighwater: Integer; resetFlag: Integer): Integer; cdecl;

  // Backup API
  sqlite3_backup_init: function(pDest: Psqlite3; zDestName: PAnsiChar;
    pSource: Psqlite3; zSourceName: PAnsiChar): Psqlite3_backup; cdecl;
  sqlite3_backup_step: function(p: Psqlite3_backup; nPage: Integer): Integer; cdecl;
  sqlite3_backup_finish: function(p: Psqlite3_backup): Integer; cdecl;
  sqlite3_backup_remaining: function(p: Psqlite3_backup): Integer; cdecl;
  sqlite3_backup_pagecount: function(p: Psqlite3_backup): Integer; cdecl;

  // Blob I/O
  sqlite3_blob_open: function(db: Psqlite3; zDb: PAnsiChar; zTable: PAnsiChar;
    zColumn: PAnsiChar; iRow: Int64; flags: Integer;
    var ppBlob: Psqlite3_blob): Integer; cdecl;
  sqlite3_blob_reopen: function(blob: Psqlite3_blob; iRow: Int64): Integer; cdecl;
  sqlite3_blob_close: function(blob: Psqlite3_blob): Integer; cdecl;
  sqlite3_blob_bytes: function(blob: Psqlite3_blob): Integer; cdecl;
  sqlite3_blob_read: function(blob: Psqlite3_blob; Z: Pointer;
    N: Integer; iOffset: Integer): Integer; cdecl;
  sqlite3_blob_write: function(blob: Psqlite3_blob; z: Pointer;
    n: Integer; iOffset: Integer): Integer; cdecl;

  // WAL
  sqlite3_wal_hook: function(db: Psqlite3; callback: Pointer; pArg: Pointer): Pointer; cdecl;
  sqlite3_wal_autocheckpoint: function(db: Psqlite3; N: Integer): Integer; cdecl;
  sqlite3_wal_checkpoint: function(db: Psqlite3; zDb: PAnsiChar): Integer; cdecl;
  sqlite3_wal_checkpoint_v2: function(db: Psqlite3; zDb: PAnsiChar; eMode: Integer;
    var pnLog: Integer; var pnCkpt: Integer): Integer; cdecl;

  // Memory management
  sqlite3_malloc: function(n: Integer): Pointer; cdecl;
  sqlite3_malloc64: function(n: UInt64): Pointer; cdecl;
  sqlite3_realloc: function(p: Pointer; n: Integer): Pointer; cdecl;
  sqlite3_realloc64: function(p: Pointer; n: UInt64): Pointer; cdecl;
  sqlite3_free: procedure(p: Pointer); cdecl;
  sqlite3_msize: function(p: Pointer): UInt64; cdecl;

  // Collation
  sqlite3_create_collation: function(db: Psqlite3; zName: PAnsiChar;
    eTextRep: Integer; pArg: Pointer;
    callback: TSQLite3CompareCallback): Integer; cdecl;
  sqlite3_create_collation_v2: function(db: Psqlite3; zName: PAnsiChar;
    eTextRep: Integer; pArg: Pointer; callback: TSQLite3CompareCallback;
    xDestroy: TSQLite3DestructorCallback): Integer; cdecl;
  sqlite3_create_collation16: function(db: Psqlite3; zName: PWideChar;
    eTextRep: Integer; pArg: Pointer;
    callback: TSQLite3CompareCallback): Integer; cdecl;
  sqlite3_collation_needed: function(db: Psqlite3; pArg: Pointer;
    callback: TSQLite3CollationNeededCallback): Integer; cdecl;

  // User-defined functions
  sqlite3_create_function: function(db: Psqlite3; zFunctionName: PAnsiChar;
    nArg: Integer; eTextRep: Integer; pApp: Pointer;
    xFunc: TSQLite3FunctionCallback; xStep: TSQLite3FunctionCallback;
    xFinal: TSQLite3FunctionFinalCallback): Integer; cdecl;
  sqlite3_create_function_v2: function(db: Psqlite3; zFunctionName: PAnsiChar;
    nArg: Integer; eTextRep: Integer; pApp: Pointer;
    xFunc: TSQLite3FunctionCallback; xStep: TSQLite3FunctionCallback;
    xFinal: TSQLite3FunctionFinalCallback;
    xDestroy: TSQLite3DestructorCallback): Integer; cdecl;
  sqlite3_create_function16: function(db: Psqlite3; zFunctionName: PWideChar;
    nArg: Integer; eTextRep: Integer; pApp: Pointer;
    xFunc: TSQLite3FunctionCallback; xStep: TSQLite3FunctionCallback;
    xFinal: TSQLite3FunctionFinalCallback): Integer; cdecl;

  // Value functions
  sqlite3_value_type: function(val: Psqlite3_value): Integer; cdecl;
  sqlite3_value_numeric_type: function(val: Psqlite3_value): Integer; cdecl;
  sqlite3_value_nochange: function(val: Psqlite3_value): Integer; cdecl;
  sqlite3_value_int: function(val: Psqlite3_value): Integer; cdecl;
  sqlite3_value_int64: function(val: Psqlite3_value): Int64; cdecl;
  sqlite3_value_double: function(val: Psqlite3_value): Double; cdecl;
  sqlite3_value_text: function(val: Psqlite3_value): PAnsiChar; cdecl;
  sqlite3_value_text16: function(val: Psqlite3_value): PWideChar; cdecl;
  sqlite3_value_blob: function(val: Psqlite3_value): Pointer; cdecl;
  sqlite3_value_bytes: function(val: Psqlite3_value): Integer; cdecl;
  sqlite3_value_bytes16: function(val: Psqlite3_value): Integer; cdecl;
  sqlite3_value_subtype: function(val: Psqlite3_value): Cardinal; cdecl;
  sqlite3_value_dup: function(val: Psqlite3_value): Psqlite3_value; cdecl;
  sqlite3_value_free: procedure(val: Psqlite3_value); cdecl;

  // Result functions
  sqlite3_result_null: procedure(ctx: Psqlite3_context); cdecl;
  sqlite3_result_int: procedure(ctx: Psqlite3_context; val: Integer); cdecl;
  sqlite3_result_int64: procedure(ctx: Psqlite3_context; val: Int64); cdecl;
  sqlite3_result_double: procedure(ctx: Psqlite3_context; val: Double); cdecl;
  sqlite3_result_text: procedure(ctx: Psqlite3_context; val: PAnsiChar;
    n: Integer; xDel: Pointer); cdecl;
  sqlite3_result_text16: procedure(ctx: Psqlite3_context; val: PWideChar;
    n: Integer; xDel: Pointer); cdecl;
  sqlite3_result_blob: procedure(ctx: Psqlite3_context; val: Pointer;
    n: Integer; xDel: Pointer); cdecl;
  sqlite3_result_blob64: procedure(ctx: Psqlite3_context; val: Pointer;
    n: UInt64; xDel: Pointer); cdecl;
  sqlite3_result_zeroblob: procedure(ctx: Psqlite3_context; n: Integer); cdecl;
  sqlite3_result_zeroblob64: function(ctx: Psqlite3_context; n: UInt64): Integer; cdecl;
  sqlite3_result_value: procedure(ctx: Psqlite3_context; val: Psqlite3_value); cdecl;
  sqlite3_result_error: procedure(ctx: Psqlite3_context; z: PAnsiChar; n: Integer); cdecl;
  sqlite3_result_error16: procedure(ctx: Psqlite3_context; z: PWideChar; n: Integer); cdecl;
  sqlite3_result_error_toobig: procedure(ctx: Psqlite3_context); cdecl;
  sqlite3_result_error_nomem: procedure(ctx: Psqlite3_context); cdecl;
  sqlite3_result_error_code: procedure(ctx: Psqlite3_context; errCode: Integer); cdecl;
  sqlite3_result_subtype: procedure(ctx: Psqlite3_context; subtype: Cardinal); cdecl;

  // Context functions
  sqlite3_aggregate_context: function(ctx: Psqlite3_context; nBytes: Integer): Pointer; cdecl;
  sqlite3_user_data: function(ctx: Psqlite3_context): Pointer; cdecl;
  sqlite3_context_db_handle: function(ctx: Psqlite3_context): Psqlite3; cdecl;
  sqlite3_get_auxdata: function(ctx: Psqlite3_context; N: Integer): Pointer; cdecl;
  sqlite3_set_auxdata: procedure(ctx: Psqlite3_context; N: Integer;
    pAux: Pointer; xDel: TSQLite3DestructorCallback); cdecl;

  // Table utility
  sqlite3_table_column_metadata: function(db: Psqlite3;
    zDbName: PAnsiChar; zTableName: PAnsiChar; zColumnName: PAnsiChar;
    var pzDataType: PAnsiChar; var pzCollSeq: PAnsiChar;
    var pNotNull: Integer; var pPrimaryKey: Integer;
    var pAutoinc: Integer): Integer; cdecl;

  // Threading
  sqlite3_db_mutex: function(db: Psqlite3): Pointer; cdecl;
  sqlite3_mutex_enter: procedure(mutex: Pointer); cdecl;
  sqlite3_mutex_try: function(mutex: Pointer): Integer; cdecl;
  sqlite3_mutex_leave: procedure(mutex: Pointer); cdecl;

  // Configuration
  sqlite3_db_config: function(db: Psqlite3; op: Integer): Integer; cdecl varargs;
  sqlite3_enable_shared_cache: function(enable: Integer): Integer; cdecl;
  sqlite3_release_memory: function(n: Integer): Integer; cdecl;
  sqlite3_db_release_memory: function(db: Psqlite3): Integer; cdecl;
  sqlite3_soft_heap_limit64: function(N: Int64): Int64; cdecl;
  sqlite3_hard_heap_limit64: function(N: Int64): Int64; cdecl;

  // Miscellaneous
  sqlite3_get_autocommit: function(db: Psqlite3): Integer; cdecl;
  sqlite3_db_handle: function(stmt: Psqlite3_stmt): Psqlite3; cdecl;
  sqlite3_db_filename: function(db: Psqlite3; zDbName: PAnsiChar): PAnsiChar; cdecl;
  sqlite3_db_readonly: function(db: Psqlite3; zDbName: PAnsiChar): Integer; cdecl;
  sqlite3_next_stmt: function(db: Psqlite3; pStmt: Psqlite3_stmt): Psqlite3_stmt; cdecl;
  sqlite3_complete: function(sql: PAnsiChar): Integer; cdecl;
  sqlite3_complete16: function(sql: PWideChar): Integer; cdecl;
  sqlite3_uri_parameter: function(zFilename: PAnsiChar; zParam: PAnsiChar): PAnsiChar; cdecl;
  sqlite3_uri_boolean: function(zFilename: PAnsiChar; zParam: PAnsiChar; bDefault: Integer): Integer; cdecl;
  sqlite3_uri_int64: function(zFilename: PAnsiChar; zParam: PAnsiChar; bDefault: Int64): Int64; cdecl;
  sqlite3_filename_database: function(zFilename: PAnsiChar): PAnsiChar; cdecl;
  sqlite3_filename_journal: function(zFilename: PAnsiChar): PAnsiChar; cdecl;
  sqlite3_filename_wal: function(zFilename: PAnsiChar): PAnsiChar; cdecl;

// Library loading functions

{ Loads the SQLite3 shared library. Tries ALibPath first, then platform defaults.
  Returns True on success. }
function LoadSQLite3Library(const ALibPath: string = ''): Boolean;
{ Unloads the SQLite3 shared library and clears all function pointers. }
procedure UnloadSQLite3Library;
{ Returns True if the SQLite3 library is currently loaded. }
function IsSQLite3LibraryLoaded: Boolean;
{ Returns the file path of the currently loaded SQLite3 library. }
function GetSQLite3LibraryPath: string;

// Utility functions

{ Converts a SQLite result code (e.g. SQLITE_OK) to its human-readable name. }
function SQLite3ResultCodeToString(ACode: Integer): string;
{ Converts a SQLite column type constant to its name (INTEGER, FLOAT, TEXT, BLOB, NULL). }
function SQLite3TypeToString(AType: Integer): string;

implementation

var
  LoadedLibPath: string = '';

function LoadSQLite3Library(const ALibPath: string): Boolean;
var
  LibPath: string;
  I: Integer;
begin
  Result := False;

  if SQLite3LibHandle <> NilHandle then
  begin
    Result := True;
    Exit;
  end;

  // If specific path provided, try it first
  if ALibPath <> '' then
  begin
    SQLite3LibHandle := LoadLibrary(ALibPath);
    if SQLite3LibHandle <> NilHandle then
    begin
      LoadedLibPath := ALibPath;
      // Continue to load function pointers below
    end;
  end;

  // Try alternative paths first (SQLCipher before standard SQLite)
  if SQLite3LibHandle = NilHandle then
  begin
    for I := Low(SQLITE3_LIB_ALT) to High(SQLITE3_LIB_ALT) do
    begin
      LibPath := SQLITE3_LIB_ALT[I];
      SQLite3LibHandle := LoadLibrary(LibPath);
      if SQLite3LibHandle <> NilHandle then
      begin
        LoadedLibPath := LibPath;
        Break;
      end;
    end;
  end;

  // Fallback to default library name
  if SQLite3LibHandle = NilHandle then
  begin
    SQLite3LibHandle := LoadLibrary(SQLITE3_LIB);
    if SQLite3LibHandle <> NilHandle then
      LoadedLibPath := SQLITE3_LIB;
  end;

  if SQLite3LibHandle = NilHandle then
    Exit;

  // Load core functions
  Pointer(sqlite3_libversion) := GetProcAddress(SQLite3LibHandle, 'sqlite3_libversion');
  Pointer(sqlite3_libversion_number) := GetProcAddress(SQLite3LibHandle, 'sqlite3_libversion_number');
  Pointer(sqlite3_sourceid) := GetProcAddress(SQLite3LibHandle, 'sqlite3_sourceid');
  Pointer(sqlite3_threadsafe) := GetProcAddress(SQLite3LibHandle, 'sqlite3_threadsafe');

  // Database connection
  Pointer(sqlite3_open) := GetProcAddress(SQLite3LibHandle, 'sqlite3_open');
  Pointer(sqlite3_open16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_open16');
  Pointer(sqlite3_open_v2) := GetProcAddress(SQLite3LibHandle, 'sqlite3_open_v2');
  Pointer(sqlite3_close) := GetProcAddress(SQLite3LibHandle, 'sqlite3_close');
  Pointer(sqlite3_close_v2) := GetProcAddress(SQLite3LibHandle, 'sqlite3_close_v2');

  // Error handling
  Pointer(sqlite3_errcode) := GetProcAddress(SQLite3LibHandle, 'sqlite3_errcode');
  Pointer(sqlite3_extended_errcode) := GetProcAddress(SQLite3LibHandle, 'sqlite3_extended_errcode');
  Pointer(sqlite3_errmsg) := GetProcAddress(SQLite3LibHandle, 'sqlite3_errmsg');
  Pointer(sqlite3_errmsg16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_errmsg16');
  Pointer(sqlite3_errstr) := GetProcAddress(SQLite3LibHandle, 'sqlite3_errstr');

  // Execution
  Pointer(sqlite3_exec) := GetProcAddress(SQLite3LibHandle, 'sqlite3_exec');

  // Prepared statements
  Pointer(sqlite3_prepare) := GetProcAddress(SQLite3LibHandle, 'sqlite3_prepare');
  Pointer(sqlite3_prepare_v2) := GetProcAddress(SQLite3LibHandle, 'sqlite3_prepare_v2');
  Pointer(sqlite3_prepare_v3) := GetProcAddress(SQLite3LibHandle, 'sqlite3_prepare_v3');
  Pointer(sqlite3_prepare16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_prepare16');
  Pointer(sqlite3_prepare16_v2) := GetProcAddress(SQLite3LibHandle, 'sqlite3_prepare16_v2');
  Pointer(sqlite3_prepare16_v3) := GetProcAddress(SQLite3LibHandle, 'sqlite3_prepare16_v3');

  // Statement execution
  Pointer(sqlite3_step) := GetProcAddress(SQLite3LibHandle, 'sqlite3_step');
  Pointer(sqlite3_reset) := GetProcAddress(SQLite3LibHandle, 'sqlite3_reset');
  Pointer(sqlite3_finalize) := GetProcAddress(SQLite3LibHandle, 'sqlite3_finalize');

  // Binding parameters
  Pointer(sqlite3_bind_parameter_count) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_parameter_count');
  Pointer(sqlite3_bind_parameter_name) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_parameter_name');
  Pointer(sqlite3_bind_parameter_index) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_parameter_index');
  Pointer(sqlite3_clear_bindings) := GetProcAddress(SQLite3LibHandle, 'sqlite3_clear_bindings');
  Pointer(sqlite3_bind_null) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_null');
  Pointer(sqlite3_bind_int) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_int');
  Pointer(sqlite3_bind_int64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_int64');
  Pointer(sqlite3_bind_double) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_double');
  Pointer(sqlite3_bind_text) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_text');
  Pointer(sqlite3_bind_text16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_text16');
  Pointer(sqlite3_bind_blob) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_blob');
  Pointer(sqlite3_bind_blob64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_blob64');
  Pointer(sqlite3_bind_zeroblob) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_zeroblob');
  Pointer(sqlite3_bind_zeroblob64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_zeroblob64');
  Pointer(sqlite3_bind_value) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_value');
  Pointer(sqlite3_bind_pointer) := GetProcAddress(SQLite3LibHandle, 'sqlite3_bind_pointer');

  // Column information
  Pointer(sqlite3_column_count) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_count');
  Pointer(sqlite3_column_name) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_name');
  Pointer(sqlite3_column_name16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_name16');
  Pointer(sqlite3_column_database_name) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_database_name');
  Pointer(sqlite3_column_database_name16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_database_name16');
  Pointer(sqlite3_column_table_name) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_table_name');
  Pointer(sqlite3_column_table_name16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_table_name16');
  Pointer(sqlite3_column_origin_name) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_origin_name');
  Pointer(sqlite3_column_origin_name16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_origin_name16');
  Pointer(sqlite3_column_decltype) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_decltype');
  Pointer(sqlite3_column_decltype16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_decltype16');

  // Column values
  Pointer(sqlite3_column_type) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_type');
  Pointer(sqlite3_column_int) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_int');
  Pointer(sqlite3_column_int64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_int64');
  Pointer(sqlite3_column_double) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_double');
  Pointer(sqlite3_column_text) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_text');
  Pointer(sqlite3_column_text16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_text16');
  Pointer(sqlite3_column_blob) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_blob');
  Pointer(sqlite3_column_bytes) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_bytes');
  Pointer(sqlite3_column_bytes16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_bytes16');
  Pointer(sqlite3_column_value) := GetProcAddress(SQLite3LibHandle, 'sqlite3_column_value');

  // SQL utility
  Pointer(sqlite3_sql) := GetProcAddress(SQLite3LibHandle, 'sqlite3_sql');
  Pointer(sqlite3_expanded_sql) := GetProcAddress(SQLite3LibHandle, 'sqlite3_expanded_sql');
  Pointer(sqlite3_normalized_sql) := GetProcAddress(SQLite3LibHandle, 'sqlite3_normalized_sql');
  Pointer(sqlite3_stmt_readonly) := GetProcAddress(SQLite3LibHandle, 'sqlite3_stmt_readonly');
  Pointer(sqlite3_stmt_isexplain) := GetProcAddress(SQLite3LibHandle, 'sqlite3_stmt_isexplain');
  Pointer(sqlite3_stmt_busy) := GetProcAddress(SQLite3LibHandle, 'sqlite3_stmt_busy');
  Pointer(sqlite3_stmt_status) := GetProcAddress(SQLite3LibHandle, 'sqlite3_stmt_status');

  // Database utility
  Pointer(sqlite3_changes) := GetProcAddress(SQLite3LibHandle, 'sqlite3_changes');
  Pointer(sqlite3_changes64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_changes64');
  Pointer(sqlite3_total_changes) := GetProcAddress(SQLite3LibHandle, 'sqlite3_total_changes');
  Pointer(sqlite3_total_changes64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_total_changes64');
  Pointer(sqlite3_last_insert_rowid) := GetProcAddress(SQLite3LibHandle, 'sqlite3_last_insert_rowid');
  Pointer(sqlite3_set_last_insert_rowid) := GetProcAddress(SQLite3LibHandle, 'sqlite3_set_last_insert_rowid');
  Pointer(sqlite3_interrupt) := GetProcAddress(SQLite3LibHandle, 'sqlite3_interrupt');
  Pointer(sqlite3_is_interrupted) := GetProcAddress(SQLite3LibHandle, 'sqlite3_is_interrupted');

  // Busy handling
  Pointer(sqlite3_busy_handler) := GetProcAddress(SQLite3LibHandle, 'sqlite3_busy_handler');
  Pointer(sqlite3_busy_timeout) := GetProcAddress(SQLite3LibHandle, 'sqlite3_busy_timeout');

  // Progress callback
  Pointer(sqlite3_progress_handler) := GetProcAddress(SQLite3LibHandle, 'sqlite3_progress_handler');

  // Authorizer
  Pointer(sqlite3_set_authorizer) := GetProcAddress(SQLite3LibHandle, 'sqlite3_set_authorizer');

  // Tracing
  Pointer(sqlite3_trace) := GetProcAddress(SQLite3LibHandle, 'sqlite3_trace');
  Pointer(sqlite3_profile) := GetProcAddress(SQLite3LibHandle, 'sqlite3_profile');

  // Commit/Rollback hooks
  Pointer(sqlite3_commit_hook) := GetProcAddress(SQLite3LibHandle, 'sqlite3_commit_hook');
  Pointer(sqlite3_rollback_hook) := GetProcAddress(SQLite3LibHandle, 'sqlite3_rollback_hook');

  // Update hook
  Pointer(sqlite3_update_hook) := GetProcAddress(SQLite3LibHandle, 'sqlite3_update_hook');

  // Preupdate hook (may not be available in all SQLite builds)
  Pointer(sqlite3_preupdate_hook) := GetProcAddress(SQLite3LibHandle, 'sqlite3_preupdate_hook');
  Pointer(sqlite3_preupdate_old) := GetProcAddress(SQLite3LibHandle, 'sqlite3_preupdate_old');
  Pointer(sqlite3_preupdate_new) := GetProcAddress(SQLite3LibHandle, 'sqlite3_preupdate_new');
  Pointer(sqlite3_preupdate_count) := GetProcAddress(SQLite3LibHandle, 'sqlite3_preupdate_count');
  Pointer(sqlite3_preupdate_depth) := GetProcAddress(SQLite3LibHandle, 'sqlite3_preupdate_depth');

  // Unlock notify (may not be available in all SQLite builds)
  Pointer(sqlite3_unlock_notify) := GetProcAddress(SQLite3LibHandle, 'sqlite3_unlock_notify');

  // Limits
  Pointer(sqlite3_limit) := GetProcAddress(SQLite3LibHandle, 'sqlite3_limit');

  // Status
  Pointer(sqlite3_status) := GetProcAddress(SQLite3LibHandle, 'sqlite3_status');
  Pointer(sqlite3_status64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_status64');
  Pointer(sqlite3_db_status) := GetProcAddress(SQLite3LibHandle, 'sqlite3_db_status');

  // Backup API
  Pointer(sqlite3_backup_init) := GetProcAddress(SQLite3LibHandle, 'sqlite3_backup_init');
  Pointer(sqlite3_backup_step) := GetProcAddress(SQLite3LibHandle, 'sqlite3_backup_step');
  Pointer(sqlite3_backup_finish) := GetProcAddress(SQLite3LibHandle, 'sqlite3_backup_finish');
  Pointer(sqlite3_backup_remaining) := GetProcAddress(SQLite3LibHandle, 'sqlite3_backup_remaining');
  Pointer(sqlite3_backup_pagecount) := GetProcAddress(SQLite3LibHandle, 'sqlite3_backup_pagecount');

  // Blob I/O
  Pointer(sqlite3_blob_open) := GetProcAddress(SQLite3LibHandle, 'sqlite3_blob_open');
  Pointer(sqlite3_blob_reopen) := GetProcAddress(SQLite3LibHandle, 'sqlite3_blob_reopen');
  Pointer(sqlite3_blob_close) := GetProcAddress(SQLite3LibHandle, 'sqlite3_blob_close');
  Pointer(sqlite3_blob_bytes) := GetProcAddress(SQLite3LibHandle, 'sqlite3_blob_bytes');
  Pointer(sqlite3_blob_read) := GetProcAddress(SQLite3LibHandle, 'sqlite3_blob_read');
  Pointer(sqlite3_blob_write) := GetProcAddress(SQLite3LibHandle, 'sqlite3_blob_write');

  // WAL
  Pointer(sqlite3_wal_hook) := GetProcAddress(SQLite3LibHandle, 'sqlite3_wal_hook');
  Pointer(sqlite3_wal_autocheckpoint) := GetProcAddress(SQLite3LibHandle, 'sqlite3_wal_autocheckpoint');
  Pointer(sqlite3_wal_checkpoint) := GetProcAddress(SQLite3LibHandle, 'sqlite3_wal_checkpoint');
  Pointer(sqlite3_wal_checkpoint_v2) := GetProcAddress(SQLite3LibHandle, 'sqlite3_wal_checkpoint_v2');

  // Memory management
  Pointer(sqlite3_malloc) := GetProcAddress(SQLite3LibHandle, 'sqlite3_malloc');
  Pointer(sqlite3_malloc64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_malloc64');
  Pointer(sqlite3_realloc) := GetProcAddress(SQLite3LibHandle, 'sqlite3_realloc');
  Pointer(sqlite3_realloc64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_realloc64');
  Pointer(sqlite3_free) := GetProcAddress(SQLite3LibHandle, 'sqlite3_free');
  Pointer(sqlite3_msize) := GetProcAddress(SQLite3LibHandle, 'sqlite3_msize');

  // Collation
  Pointer(sqlite3_create_collation) := GetProcAddress(SQLite3LibHandle, 'sqlite3_create_collation');
  Pointer(sqlite3_create_collation_v2) := GetProcAddress(SQLite3LibHandle, 'sqlite3_create_collation_v2');
  Pointer(sqlite3_create_collation16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_create_collation16');
  Pointer(sqlite3_collation_needed) := GetProcAddress(SQLite3LibHandle, 'sqlite3_collation_needed');

  // User-defined functions
  Pointer(sqlite3_create_function) := GetProcAddress(SQLite3LibHandle, 'sqlite3_create_function');
  Pointer(sqlite3_create_function_v2) := GetProcAddress(SQLite3LibHandle, 'sqlite3_create_function_v2');
  Pointer(sqlite3_create_function16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_create_function16');

  // Value functions
  Pointer(sqlite3_value_type) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_type');
  Pointer(sqlite3_value_numeric_type) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_numeric_type');
  Pointer(sqlite3_value_nochange) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_nochange');
  Pointer(sqlite3_value_int) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_int');
  Pointer(sqlite3_value_int64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_int64');
  Pointer(sqlite3_value_double) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_double');
  Pointer(sqlite3_value_text) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_text');
  Pointer(sqlite3_value_text16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_text16');
  Pointer(sqlite3_value_blob) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_blob');
  Pointer(sqlite3_value_bytes) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_bytes');
  Pointer(sqlite3_value_bytes16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_bytes16');
  Pointer(sqlite3_value_subtype) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_subtype');
  Pointer(sqlite3_value_dup) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_dup');
  Pointer(sqlite3_value_free) := GetProcAddress(SQLite3LibHandle, 'sqlite3_value_free');

  // Result functions
  Pointer(sqlite3_result_null) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_null');
  Pointer(sqlite3_result_int) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_int');
  Pointer(sqlite3_result_int64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_int64');
  Pointer(sqlite3_result_double) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_double');
  Pointer(sqlite3_result_text) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_text');
  Pointer(sqlite3_result_text16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_text16');
  Pointer(sqlite3_result_blob) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_blob');
  Pointer(sqlite3_result_blob64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_blob64');
  Pointer(sqlite3_result_zeroblob) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_zeroblob');
  Pointer(sqlite3_result_zeroblob64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_zeroblob64');
  Pointer(sqlite3_result_value) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_value');
  Pointer(sqlite3_result_error) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_error');
  Pointer(sqlite3_result_error16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_error16');
  Pointer(sqlite3_result_error_toobig) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_error_toobig');
  Pointer(sqlite3_result_error_nomem) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_error_nomem');
  Pointer(sqlite3_result_error_code) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_error_code');
  Pointer(sqlite3_result_subtype) := GetProcAddress(SQLite3LibHandle, 'sqlite3_result_subtype');

  // Context functions
  Pointer(sqlite3_aggregate_context) := GetProcAddress(SQLite3LibHandle, 'sqlite3_aggregate_context');
  Pointer(sqlite3_user_data) := GetProcAddress(SQLite3LibHandle, 'sqlite3_user_data');
  Pointer(sqlite3_context_db_handle) := GetProcAddress(SQLite3LibHandle, 'sqlite3_context_db_handle');
  Pointer(sqlite3_get_auxdata) := GetProcAddress(SQLite3LibHandle, 'sqlite3_get_auxdata');
  Pointer(sqlite3_set_auxdata) := GetProcAddress(SQLite3LibHandle, 'sqlite3_set_auxdata');

  // Table utility
  Pointer(sqlite3_table_column_metadata) := GetProcAddress(SQLite3LibHandle, 'sqlite3_table_column_metadata');

  // Threading
  Pointer(sqlite3_db_mutex) := GetProcAddress(SQLite3LibHandle, 'sqlite3_db_mutex');
  Pointer(sqlite3_mutex_enter) := GetProcAddress(SQLite3LibHandle, 'sqlite3_mutex_enter');
  Pointer(sqlite3_mutex_try) := GetProcAddress(SQLite3LibHandle, 'sqlite3_mutex_try');
  Pointer(sqlite3_mutex_leave) := GetProcAddress(SQLite3LibHandle, 'sqlite3_mutex_leave');

  // Configuration
  Pointer(sqlite3_db_config) := GetProcAddress(SQLite3LibHandle, 'sqlite3_db_config');
  Pointer(sqlite3_enable_shared_cache) := GetProcAddress(SQLite3LibHandle, 'sqlite3_enable_shared_cache');
  Pointer(sqlite3_release_memory) := GetProcAddress(SQLite3LibHandle, 'sqlite3_release_memory');
  Pointer(sqlite3_db_release_memory) := GetProcAddress(SQLite3LibHandle, 'sqlite3_db_release_memory');
  Pointer(sqlite3_soft_heap_limit64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_soft_heap_limit64');
  Pointer(sqlite3_hard_heap_limit64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_hard_heap_limit64');

  // Miscellaneous
  Pointer(sqlite3_get_autocommit) := GetProcAddress(SQLite3LibHandle, 'sqlite3_get_autocommit');
  Pointer(sqlite3_db_handle) := GetProcAddress(SQLite3LibHandle, 'sqlite3_db_handle');
  Pointer(sqlite3_db_filename) := GetProcAddress(SQLite3LibHandle, 'sqlite3_db_filename');
  Pointer(sqlite3_db_readonly) := GetProcAddress(SQLite3LibHandle, 'sqlite3_db_readonly');
  Pointer(sqlite3_next_stmt) := GetProcAddress(SQLite3LibHandle, 'sqlite3_next_stmt');
  Pointer(sqlite3_complete) := GetProcAddress(SQLite3LibHandle, 'sqlite3_complete');
  Pointer(sqlite3_complete16) := GetProcAddress(SQLite3LibHandle, 'sqlite3_complete16');
  Pointer(sqlite3_uri_parameter) := GetProcAddress(SQLite3LibHandle, 'sqlite3_uri_parameter');
  Pointer(sqlite3_uri_boolean) := GetProcAddress(SQLite3LibHandle, 'sqlite3_uri_boolean');
  Pointer(sqlite3_uri_int64) := GetProcAddress(SQLite3LibHandle, 'sqlite3_uri_int64');
  Pointer(sqlite3_filename_database) := GetProcAddress(SQLite3LibHandle, 'sqlite3_filename_database');
  Pointer(sqlite3_filename_journal) := GetProcAddress(SQLite3LibHandle, 'sqlite3_filename_journal');
  Pointer(sqlite3_filename_wal) := GetProcAddress(SQLite3LibHandle, 'sqlite3_filename_wal');

  // Check essential functions are loaded
  Result := Assigned(sqlite3_open_v2) and Assigned(sqlite3_close_v2) and
            Assigned(sqlite3_prepare_v2) and Assigned(sqlite3_step) and
            Assigned(sqlite3_finalize) and Assigned(sqlite3_errmsg);
end;

procedure UnloadSQLite3Library;
begin
  if SQLite3LibHandle <> NilHandle then
  begin
    FreeLibrary(SQLite3LibHandle);
    SQLite3LibHandle := NilHandle;
    LoadedLibPath := '';
  end;
end;

function IsSQLite3LibraryLoaded: Boolean;
begin
  Result := SQLite3LibHandle <> NilHandle;
end;

function GetSQLite3LibraryPath: string;
begin
  Result := LoadedLibPath;
end;

function SQLite3ResultCodeToString(ACode: Integer): string;
begin
  case ACode of
    SQLITE_OK:         Result := 'SQLITE_OK';
    SQLITE_ERROR:      Result := 'SQLITE_ERROR';
    SQLITE_INTERNAL:   Result := 'SQLITE_INTERNAL';
    SQLITE_PERM:       Result := 'SQLITE_PERM';
    SQLITE_ABORT:      Result := 'SQLITE_ABORT';
    SQLITE_BUSY:       Result := 'SQLITE_BUSY';
    SQLITE_LOCKED:     Result := 'SQLITE_LOCKED';
    SQLITE_NOMEM:      Result := 'SQLITE_NOMEM';
    SQLITE_READONLY:   Result := 'SQLITE_READONLY';
    SQLITE_INTERRUPT:  Result := 'SQLITE_INTERRUPT';
    SQLITE_IOERR:      Result := 'SQLITE_IOERR';
    SQLITE_CORRUPT:    Result := 'SQLITE_CORRUPT';
    SQLITE_NOTFOUND:   Result := 'SQLITE_NOTFOUND';
    SQLITE_FULL:       Result := 'SQLITE_FULL';
    SQLITE_CANTOPEN:   Result := 'SQLITE_CANTOPEN';
    SQLITE_PROTOCOL:   Result := 'SQLITE_PROTOCOL';
    SQLITE_EMPTY:      Result := 'SQLITE_EMPTY';
    SQLITE_SCHEMA:     Result := 'SQLITE_SCHEMA';
    SQLITE_TOOBIG:     Result := 'SQLITE_TOOBIG';
    SQLITE_CONSTRAINT: Result := 'SQLITE_CONSTRAINT';
    SQLITE_MISMATCH:   Result := 'SQLITE_MISMATCH';
    SQLITE_MISUSE:     Result := 'SQLITE_MISUSE';
    SQLITE_NOLFS:      Result := 'SQLITE_NOLFS';
    SQLITE_AUTH:       Result := 'SQLITE_AUTH';
    SQLITE_FORMAT:     Result := 'SQLITE_FORMAT';
    SQLITE_RANGE:      Result := 'SQLITE_RANGE';
    SQLITE_NOTADB:     Result := 'SQLITE_NOTADB';
    SQLITE_NOTICE:     Result := 'SQLITE_NOTICE';
    SQLITE_WARNING:    Result := 'SQLITE_WARNING';
    SQLITE_ROW:        Result := 'SQLITE_ROW';
    SQLITE_DONE:       Result := 'SQLITE_DONE';
  else
    Result := Format('SQLITE_UNKNOWN(%d)', [ACode]);
  end;
end;

function SQLite3TypeToString(AType: Integer): string;
begin
  case AType of
    SQLITE_INTEGER: Result := 'INTEGER';
    SQLITE_FLOAT:   Result := 'FLOAT';
    SQLITE_TEXT:    Result := 'TEXT';
    SQLITE_BLOB:    Result := 'BLOB';
    SQLITE_NULL:    Result := 'NULL';
  else
    Result := Format('UNKNOWN(%d)', [AType]);
  end;
end;

initialization

finalization
  UnloadSQLite3Library;

end.
