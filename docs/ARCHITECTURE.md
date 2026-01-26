# NDXSQLite Architecture

This document describes the internal architecture of NDXSQLite, including design patterns, module dependencies, and key implementation decisions.

---

## Table of Contents

1. [Overview](#overview)
2. [Layer Diagram](#layer-diagram)
3. [Module Dependencies](#module-dependencies)
4. [Design Patterns](#design-patterns)
5. [Core Components](#core-components)
6. [Advanced Features](#advanced-features)
7. [Thread Safety](#thread-safety)
8. [Platform Abstraction](#platform-abstraction)

---

## Overview

NDXSQLite is a layered library with 36 Pascal units organized into 10 architectural layers. The design prioritizes:

- **Separation of Concerns** - Each module has a single responsibility
- **Interface-Based Design** - Core functionality exposed through interfaces
- **No External Dependencies** - Only Free Pascal RTL and optional Lazarus LCL
- **Thread Safety** - Critical sections protect shared resources
- **Platform Independence** - Abstraction layer handles OS differences

### Statistics

| Metric | Value |
|--------|-------|
| Total Units | 36 |
| Total LOC | 28,625+ |
| Public Classes | 30+ |
| Interfaces | 5 |
| Enumerations | 15+ |

---

## Layer Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           APPLICATION LAYER                                 │
│                    (User Code / Examples / Applications)                    │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 9: ASYNC SUPPORT                                                      │
│ ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐                 │
│ │ AsyncConnection │ │   AsyncWorker   │ │  Cancellation   │                 │
│ └─────────────────┘ └─────────────────┘ └─────────────────┘                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 8: ADVANCED FEATURES (16 modules)                                     │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐     │
│ │  JSON   │ │   FTS   │ │ Backup  │ │Migration│ │   CSV   │ │  Dump   │     │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘     │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐     │
│ │ Verify  │ │  BLOB   │ │Attached │ │   UDF   │ │Collation│ │  RTree  │     │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘     │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐                             │
│ │  VTab   │ │Authoriz.│ │ Unlock  │ │PreUpdate│                             │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘                             │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 7: UTILITIES                                                          │
│ ┌─────────────────────────────────────────────────────────────────────────┐ │
│ │                          DateTimeUtils                                  │ │
│ └─────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 6: HEALTH & MONITORING                                                │
│ ┌─────────────────────────────────────────────────────────────────────────┐ │
│ │                           HealthCheck                                   │ │
│ └─────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 5: POOLING & FACTORIES                                                │
│ ┌─────────────────────────────────┐ ┌─────────────────────────────────────┐ │
│ │       ConnectionFactory         │ │         ConnectionPool              │ │
│ └─────────────────────────────────┘ └─────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 4: MAIN CONNECTION & DATASET                                          │
│ ┌─────────────────────────────────┐ ┌─────────────────────────────────────┐ │
│ │    TNDXSQLiteConnection         │ │       TNDXSQLiteDataSet             │ │
│ │    (implements interface)       │ │       (TDataSet descendant)         │ │
│ └─────────────────────────────────┘ └─────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 3: STATEMENT & QUERY                                                  │
│ ┌─────────────────────────────────┐ ┌─────────────────────────────────────┐ │
│ │       TNDXSQLiteStatement       │ │        TNDXSQLiteQuery              │ │
│ └─────────────────────────────────┘ └─────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 2: DATABASE WRAPPER                                                   │
│ ┌─────────────────────────────────────────────────────────────────────────┐ │
│ │                         TNDXSQLiteDatabase                              │ │
│ │                    (Low-level handle management)                        │ │
│ └─────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 1: API BINDINGS                                                       │
│ ┌─────────────────────────────────────────────────────────────────────────┐ │
│ │                          ndxsqlite3api                                  │ │
│ │                   (Direct SQLite3 C API bindings)                       │ │
│ └─────────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LAYER 0: FOUNDATION                                                         │
│ ┌───────────────┐ ┌───────────────┐ ┌───────────────┐ ┌───────────────┐     │
│ │     Types     │ │   Platform    │ │  Exceptions   │ │   Version     │     │
│ └───────────────┘ └───────────────┘ └───────────────┘ └───────────────┘     │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              OPERATING SYSTEM                               │
│                        (SQLite3 / SQLCipher Library)                        │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Module Dependencies

### Layer 0 - Foundation

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqlitetypes` | 463 | None | Enumerations, constants, type definitions |
| `ndxsqliteplatform` | 1,545 | SysUtils | Cross-platform path handling, OS detection |
| `ndxsqliteexceptions` | 170 | SysUtils | Exception hierarchy |
| `ndxsqliteversion` | 42 | None | Version constants |

### Layer 1 - API Bindings

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqlite3api` | 1,114 | DynLibs, Platform | SQLite3 C API declarations and dynamic loading |

### Layer 2 - Database Wrapper

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqlitedatabase` | 1,100 | API, Types, Exceptions | Low-level database handle management |

### Layer 3 - Statement & Query

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqlitestatement` | 1,055 | Database, Types | Prepared statement handling |
| `ndxsqlitequery` | 685 | Statement, Types | Higher-level query interface |

### Layer 4 - Connection & DataSet

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqliteconnectionintf` | 240 | Types | INDXSQLiteConnection interface |
| `ndxsqliteconnectionoptions` | 290 | Types | Configuration class |
| `ndxsqliteconnection` | 1,176 | Database, Statement, Options, Interface | Main connection class |
| `ndxsqlitedataset` | 2,156 | Connection, DB | TDataSet implementation |

### Layer 5 - Pooling & Factories

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqliteconnectionfactory` | 147 | Connection, Options | Connection creation |
| `ndxsqliteconnectionpool` | 510 | Connection, Interface | Thread-safe pooling |

### Layer 6 - Health

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqlitehealthcheck` | 477 | Connection | Integrity verification, statistics |

### Layer 7 - Utilities

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqlitedatetimeutils` | 465 | SysUtils | DateTime conversions |

### Layer 8 - Advanced Features

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqlitejson` | 793 | Connection | JSON1 extension wrapper |
| `ndxsqlitefts` | 620 | Connection | FTS5 full-text search |
| `ndxsqlitebackup` | 627 | Connection, API | Online backup API |
| `ndxsqlitemigration` | 599 | Connection | Schema migrations |
| `ndxsqlitecsv` | 520 | Connection | CSV import/export |
| `ndxsqlitedump` | 752 | Connection | SQL dump generation |
| `ndxsqliteverify` | 662 | Connection | Schema verification |
| `ndxsqliteblob` | 620 | Connection, API | Incremental BLOB I/O |
| `ndxsqliteattached` | 530 | Connection | Multi-database support |
| `ndxsqliteudf` | 580 | Connection, API | User-defined functions |
| `ndxsqlitecollation` | 450 | Connection, API | Custom collations |
| `ndxsqliteertree` | 792 | Connection | R-Tree spatial indexes |
| `ndxsqlitevtab` | 1,101 | Connection, API | Virtual table framework |
| `ndxsqliteauthorizer` | 480 | Connection, API | SQL access control |
| `ndxsqliteunlocknotify` | 420 | Connection, API | Lock notifications |
| `ndxsqlitepreupdate` | 450 | Connection, API | Pre-update hooks |

### Layer 9 - Async Support

| Module | LOC | Dependencies | Purpose |
|--------|-----|--------------|---------|
| `ndxsqliteasynctypes` | 200 | Types | Async type definitions |
| `ndxsqliteasyncworker` | 500 | Types | Background thread management |
| `ndxsqlitecancellation` | 360 | Types | Query cancellation |
| `ndxsqliteasyncconnection` | 510 | Connection, Worker | Async query execution |

---

## Design Patterns

### 1. Interface-Based Design

The core connection is exposed through `INDXSQLiteConnection` interface:

```pascal
type
  INDXSQLiteConnection = interface
    ['{GUID}']
    procedure Open;
    procedure Close;
    function ExecuteNonQuery(const SQL: String): Integer;
    function ExecuteScalar(const SQL: String): Variant;
    // ...
  end;
```

**Benefits:**
- Enables dependency injection
- Facilitates testing with mock implementations
- Supports connection pooling without type casting

### 2. Factory Pattern

`TNDXSQLiteConnectionFactory` creates connections:

```pascal
// Factory usage
Factory := TNDXSQLiteConnectionFactory.Create;
Conn := Factory.CreateConnection(Options);
```

**Benefits:**
- Centralizes connection creation logic
- Simplifies configuration management
- Supports different connection types

### 3. Object Pool Pattern

`TNDXSQLiteConnectionPool` manages reusable connections:

```pascal
// Pool usage
Pool := TNDXSQLiteConnectionPool.Create(Options, MinSize, MaxSize);
Conn := Pool.Acquire;
try
  // Use connection
finally
  Pool.Release(Conn);
end;
```

**Benefits:**
- Reduces connection overhead
- Thread-safe resource management
- Configurable pool sizes

### 4. Template Method Pattern

`TNDXSQLiteDataSet` uses template method for dataset operations:

```pascal
type
  TNDXSQLiteDataSet = class(TDataSet)
  protected
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    // ...
  end;
```

### 5. Strategy Pattern

Transaction modes use strategy pattern:

```pascal
type
  TNDXSQLiteTransactionMode = (tmDeferred, tmImmediate, tmExclusive);

// Usage
Conn.BeginTransaction(tmImmediate);
```

### 6. Observer Pattern

Pre-update hooks and authorizers use observer pattern:

```pascal
// Register callback
PreUpdate := TNDXSQLitePreUpdate.Create(Conn);
PreUpdate.OnChange := @HandleChange;
PreUpdate.Enable;
```

### 7. RAII (Resource Acquisition Is Initialization)

Pooled connections use RAII pattern:

```pascal
// TNDXPooledConnection auto-releases on Free
Pooled := TNDXPooledConnection.Create(Pool);
try
  Pooled.Connection.ExecuteNonQuery(SQL);
finally
  Pooled.Free;  // Auto-releases to pool
end;
```

---

## Core Components

### TNDXSQLiteConnection

The main connection class provides:

```
┌─────────────────────────────────────────────────────────────────┐
│                    TNDXSQLiteConnection                         │
├─────────────────────────────────────────────────────────────────┤
│ Private Fields:                                                 │
│   FDatabase: TNDXSQLiteDatabase                                 │
│   FOptions: TNDXSQLiteConnectionOptions                         │
│   FState: TNDXSQLiteConnectionState                             │
│   FCriticalSection: TCriticalSection                            │
│   FActionLog: TStringList                                       │
│   FInTransaction: Boolean                                       │
├─────────────────────────────────────────────────────────────────┤
│ Connection Lifecycle:                                           │
│   Open() → Close() → Reconnect()                                │
├─────────────────────────────────────────────────────────────────┤
│ Query Execution:                                                │
│   ExecuteNonQuery() → ExecuteScalar() → ExecuteQuery()          │
│   ExecuteScalarInt64()                                          │
├─────────────────────────────────────────────────────────────────┤
│ Transactions:                                                   │
│   BeginTransaction() → Commit() → Rollback()                    │
│   Savepoint() → ReleaseSavepoint() → RollbackToSavepoint()      │
├─────────────────────────────────────────────────────────────────┤
│ SQLCipher:                                                      │
│   IsSQLCipherAvailable() → ChangeEncryptionKey()                │
└─────────────────────────────────────────────────────────────────┘
```

### TNDXSQLiteDatabase

Low-level SQLite handle wrapper:

```
┌─────────────────────────────────────────────────────────────────┐
│                     TNDXSQLiteDatabase                          │
├─────────────────────────────────────────────────────────────────┤
│ FHandle: Pointer (sqlite3*)                                     │
├─────────────────────────────────────────────────────────────────┤
│ Open(Path, Flags) → Close()                                     │
│ Exec(SQL) → Prepare(SQL) → Finalize(Stmt)                       │
│ LastInsertRowId() → Changes() → TotalChanges()                  │
│ GetErrorMessage() → GetErrorCode()                              │
└─────────────────────────────────────────────────────────────────┘
```

### TNDXSQLiteStatement

Prepared statement handling:

```
┌─────────────────────────────────────────────────────────────────┐
│                    TNDXSQLiteStatement                          │
├─────────────────────────────────────────────────────────────────┤
│ FStmt: Pointer (sqlite3_stmt*)                                  │
│ FDatabase: TNDXSQLiteDatabase                                   │
├─────────────────────────────────────────────────────────────────┤
│ Prepare(SQL) → Reset() → Finalize()                             │
│ BindInt() → BindInt64() → BindDouble() → BindText() → BindBlob()│
│ BindNull() → ClearBindings()                                    │
│ Step() → ColumnCount() → ColumnName() → ColumnType()            │
│ ColumnInt() → ColumnInt64() → ColumnDouble() → ColumnText()     │
│ ColumnBlob() → ColumnBytes()                                    │
└─────────────────────────────────────────────────────────────────┘
```

---

## Advanced Features

### Connection Pool Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│                  TNDXSQLiteConnectionPool                        │
├──────────────────────────────────────────────────────────────────┤
│ Configuration:                                                   │
│   MinPoolSize: Integer (minimum connections)                     │
│   MaxPoolSize: Integer (maximum connections)                     │
│   AcquireTimeout: Integer (ms to wait for connection)            │
│   ValidationQuery: String (query to validate connections)        │
├──────────────────────────────────────────────────────────────────┤
│ State:                                                           │
│   FAvailable: TThreadList (idle connections)                     │
│   FInUse: TThreadList (leased connections)                       │
│   FLock: TCriticalSection (thread safety)                        │
├──────────────────────────────────────────────────────────────────┤
│ Statistics:                                                      │
│   PeakUsage: Integer                                             │
│   TotalAcquires: Int64                                           │
│   TotalWaitTime: Int64                                           │
│   ValidationFailures: Integer                                    │
├──────────────────────────────────────────────────────────────────┤
│ Operations:                                                      │
│   Acquire() → Release() → Clear() → GetStatistics()              │
└──────────────────────────────────────────────────────────────────┘
```

### Async Connection Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│               TNDXSQLiteAsyncConnection                          │
├──────────────────────────────────────────────────────────────────┤
│ FConnection: TNDXSQLiteConnection (underlying connection)        │
│ FWorker: TNDXSQLiteAsyncWorker (background thread)               │
│ FQueue: TThreadList (pending operations)                         │
├──────────────────────────────────────────────────────────────────┤
│ ExecuteNonQueryAsync(SQL, Callback)                              │
│ ExecuteScalarAsync(SQL, Callback)                                │
│ ExecuteQueryAsync(SQL, Callback)                                 │
│ CancelAsync(OperationId)                                         │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│                  TNDXSQLiteAsyncWorker                           │
├──────────────────────────────────────────────────────────────────┤
│ FThread: TThread (worker thread)                                 │
│ FQueue: TThreadList (operation queue)                            │
│ FEvent: TEvent (wake signal)                                     │
├──────────────────────────────────────────────────────────────────┤
│ ProcessQueue() → Execute operation → Invoke callback             │
└──────────────────────────────────────────────────────────────────┘
```

### Virtual Table Framework

```
┌──────────────────────────────────────────────────────────────────┐
│                   TNDXVirtualTableModule                         │
├──────────────────────────────────────────────────────────────────┤
│ RegisterModule(Name, TableClass)                                 │
│ CreateVirtualTable(Name, Args)                                   │
├──────────────────────────────────────────────────────────────────┤
│                    TNDXVirtualTable                              │
├──────────────────────────────────────────────────────────────────┤
│ AddColumn(Name, Type) → CreateCursor() → BestIndex()             │
├──────────────────────────────────────────────────────────────────┤
│                    TNDXVirtualCursor                             │
├──────────────────────────────────────────────────────────────────┤
│ Filter() → Next() → Eof() → Column() → RowId()                   │
└──────────────────────────────────────────────────────────────────┘
```

---

## Thread Safety

### Critical Section Usage

All shared resources are protected:

```pascal
// Connection class
procedure TNDXSQLiteConnection.ExecuteNonQuery(const SQL: String);
begin
  FCriticalSection.Enter;
  try
    // Thread-safe execution
  finally
    FCriticalSection.Leave;
  end;
end;
```

### Pool Thread Safety

```pascal
// Pool acquire with timeout
function TNDXSQLiteConnectionPool.Acquire: INDXSQLiteConnection;
var
  StartTime: TDateTime;
begin
  FLock.Enter;
  try
    StartTime := Now;
    while FAvailable.Count = 0 do
    begin
      if MilliSecondsBetween(Now, StartTime) > FAcquireTimeout then
        raise ENDXSQLiteException.Create('Pool acquire timeout');
      FLock.Leave;
      Sleep(10);
      FLock.Enter;
    end;
    Result := ExtractConnection;
  finally
    FLock.Leave;
  end;
end;
```

### Thread Safety Guidelines

| Component | Thread Safety | Notes |
|-----------|---------------|-------|
| TNDXSQLiteConnection | Per-connection | One connection per thread, or use locking |
| TNDXSQLiteConnectionPool | Full | Safe for multi-threaded access |
| TNDXSQLiteAsyncConnection | Full | Designed for async patterns |
| TNDXSQLiteDataSet | UI thread only | Use with main thread |
| Advanced modules | Per-connection | Inherit connection's safety |

---

## Platform Abstraction

### TNDXPlatform

Handles OS-specific differences:

```pascal
type
  TNDXPlatform = class
    class function GetTempDirectory: String;
    class function GetAppDirectory: String;
    class function GetSQLiteLibraryPath: String;
    class function GetSQLCipherLibraryPath: String;
    class function IsSnapEnvironment: Boolean;
    class function IsFlatpakEnvironment: Boolean;
    class function GetPlatformName: String;
  end;
```

### Library Search Paths

| Platform | SQLite Paths |
|----------|--------------|
| Linux | `/usr/lib/x86_64-linux-gnu/libsqlite3.so`, `/usr/lib64/libsqlite3.so` |
| Windows | `sqlite3.dll` (app dir), `System32\sqlite3.dll` |
| macOS | `/usr/lib/libsqlite3.dylib`, Homebrew paths |
| Snap | `$SNAP/usr/lib/...` |
| Flatpak | `/app/lib/...`, `/usr/lib/...` |

### Platform Detection

```pascal
{$IFDEF LINUX}
  // Linux-specific code
{$ENDIF}
{$IFDEF WINDOWS}
  // Windows-specific code
{$ENDIF}
{$IFDEF DARWIN}
  // macOS-specific code
{$ENDIF}
```

---

## Extension Points

### Custom Functions (UDF)

```pascal
// Register scalar function
UDF := TNDXSQLiteUDF.Create(Conn);
UDF.RegisterScalar('DOUBLE', 1, @DoubleFunc);

// Register aggregate function
UDF.RegisterAggregate('MEDIAN', 1, @MedianStep, @MedianFinal);
```

### Custom Collations

```pascal
// Register collation
Collation := TNDXSQLiteCollation.Create(Conn);
Collation.Register('NOCASE_FR', @FrenchNoCaseCompare);
```

### Virtual Tables

```pascal
// Register virtual table module
Module := TNDXVirtualTableModule.Create(Conn);
Module.RegisterModule('series', TNDXSeriesVirtualTable);
Module.CreateVirtualTable('gen_series', ['start', 'stop', 'step']);
```

### Authorizer

```pascal
// Register authorizer callback
Auth := TNDXSQLiteAuthorizer.Create(Conn);
Auth.OnAuthorize := @AuthorizeCallback;
Auth.Enable;
```

---

## Memory Management

### Ownership Rules

| Object | Owner | Cleanup |
|--------|-------|---------|
| TNDXSQLiteConnection | Caller | Must call Free |
| TNDXSQLiteConnectionOptions | Caller | Must call Free |
| TNDXSQLiteStatement | Connection | Auto-freed with connection |
| ExecuteQuery result | Caller | Must call Free |
| Pool connections | Pool | Released back to pool |

### RAII Helpers

```pascal
// TNDXPooledConnection - auto-release
var
  Pooled: TNDXPooledConnection;
begin
  Pooled := TNDXPooledConnection.Create(Pool);
  try
    Pooled.Connection.ExecuteNonQuery(SQL);
  finally
    Pooled.Free;  // Auto-releases to pool
  end;
end;
```

---

*NDXSQLite Architecture - Version 1.0.0*
