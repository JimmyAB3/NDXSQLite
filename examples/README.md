# NDXSQLite Examples

This directory contains 149 console examples and 10 GUI examples demonstrating all features of NDXSQLite.

---

## Quick Start

```bash
# Navigate to an example
cd examples/console/01_BasicConnection

# Compile
fpc -Fu../../../src BasicConnection.lpr

# Run
./BasicConnection
```

---

## Console Examples (149)

### Foundational (00-09)

| # | Directory | Description |
|---|-----------|-------------|
| 00 | `00_DateTimeUtils` | DateTime utility functions and conversions |
| 01 | `01_BasicConnection` | Basic connection, open, close, simple queries |
| 02 | `02_Transactions` | Transaction handling: begin, commit, rollback |
| 03 | `03_PreparedStatements` | Prepared statements with parameter binding |
| 04 | `04_DataTypes` | SQLite data types: INTEGER, TEXT, BLOB, REAL |
| 05 | `05_ErrorHandling` | Exception handling and error codes |
| 06 | `06_ConnectionOptions` | Connection configuration options |
| 07 | `07_ConnectionFactory` | Factory pattern for creating connections |
| 08 | `08_ConnectionPool` | Thread-safe connection pooling |
| 09 | `09_AsyncOperations` | Asynchronous query execution |

### Core Features (10-29)

| # | Directory | Description |
|---|-----------|-------------|
| 10 | `10_HealthCheck` | Database health monitoring and diagnostics |
| 11 | `11_BackupRestore` | Online backup and restore operations |
| 12 | `12_JSONSupport` | JSON1 extension: extract, modify, validate |
| 13 | `13_FullTextSearch` | FTS5 full-text search implementation |
| 14 | `14_Migrations` | Schema version control and migrations |
| 15 | `15_CrossPlatformPaths` | Platform-specific path handling |
| 16 | `16_PerformanceOptimization` | Query optimization techniques |
| 17 | `17_ConcurrentAccess` | Concurrent database access patterns |
| 18 | `18_AttachedDatabases` | Attach and query multiple databases |
| 19 | `19_ViewsAndTriggers` | Database views and triggers |
| 20 | `20_ImportExportCSV` | CSV import and export operations |
| 21 | `21_MemoryDatabases` | In-memory databases for fast operations |
| 22 | `22_AdvancedSQL` | Complex SQL queries and CTEs |
| 23 | `23_GeneratedColumns` | Computed/generated columns |
| 24 | `24_CollationsUnicode` | Unicode and custom collations |
| 25 | `25_DateTimeFunctions` | Date and time functions |
| 26 | `26_SQLDumpRestore` | SQL dump generation and restore |
| 27 | `27_BlobHandling` | Binary large object operations |
| 28 | `28_ForeignKeys` | Foreign key constraints |
| 29 | `29_Indexes` | Index creation and optimization |

### Query and Data Patterns (30-49)

| # | Directory | Description |
|---|-----------|-------------|
| 30 | `30_Upsert` | INSERT OR REPLACE and ON CONFLICT patterns |
| 31 | `31_BatchOperations` | Bulk insert and update operations |
| 32 | `32_Pagination` | LIMIT/OFFSET pagination |
| 33 | `33_SchemaVerification` | Schema validation and verification |
| 34 | `34_DatabaseMaintenance` | VACUUM, ANALYZE, maintenance tasks |
| 35 | `35_WALCheckpointing` | Write-Ahead Logging management |
| 36 | `36_CancellableQueries` | Query cancellation support |
| 37 | `37_RecursiveQueries` | Recursive CTEs for hierarchies |
| 38 | `38_AggregateExtensions` | Custom aggregate functions |
| 39 | `39_ConfigurationStore` | Configuration table patterns |
| 40 | `40_EventLog` | Event logging implementation |
| 41 | `41_CacheDatabase` | Database caching strategies |
| 42 | `42_MultiTenantDB` | Multi-tenant database patterns |
| 43 | `43_EncryptedDatabase` | SQLCipher encryption basics |
| 44 | `44_AuditTrail` | Audit trail implementation |
| 45 | `45_TaskQueue` | Task queue patterns |
| 46 | `46_SessionStore` | Session storage |
| 47 | `47_FeatureFlags` | Feature flags table |
| 48 | `48_TimeSeries` | Time-series data patterns |
| 49 | `49_DocumentStore` | JSON document storage |

### Advanced Query Patterns (50-74)

| # | Directory | Description |
|---|-----------|-------------|
| 50 | `50_GraphDatabase` | Graph data patterns in SQLite |
| 51 | `51_ConcurrentQueries` | Concurrent query execution |
| 52 | `52_OptimisticLocking` | Optimistic locking with versions |
| 53 | `53_SoftDeletes` | Soft delete implementation |
| 54 | `54_ChangeDataCapture` | Change data capture (CDC) |
| 55 | `55_DataValidation` | Data validation rules |
| 56 | `56_ETLPipeline` | ETL pipeline patterns |
| 57 | `57_ReportingQueries` | Complex reporting queries |
| 58 | `58_Localization` | Localization/i18n patterns |
| 59 | `59_VersionedData` | Data versioning patterns |
| 60 | `60_TreeStructures` | Tree and hierarchy patterns |
| 61 | `61_GeoSpatial` | Geospatial data with R-Tree |
| 62 | `62_UserDefinedFunctions` | Custom SQL functions (UDF) |
| 63 | `63_CustomCollations` | Custom collation functions |
| 64 | `64_IncrementalBlobIO` | Streaming BLOB I/O |
| 65 | `65_MultiDatabaseAttach` | Multi-database operations |
| 66 | `66_RTreeSpatialIndex` | R-Tree spatial indexing |
| 67 | `67_VirtualTables` | Virtual table modules |
| 68 | `68_CustomAuthorizers` | Custom authorization hooks |
| 69 | `69_UnlockNotify` | Unlock notification mechanism |
| 70 | `70_PreupdateHooks` | Pre-update hooks for tracking |
| 71 | `71_EAVPattern` | Entity-Attribute-Value pattern |
| 72 | `72_PolymorphicAssociations` | Polymorphic associations |
| 73 | `73_ManyToManyAdvanced` | Advanced many-to-many relationships |
| 74 | `74_ShoppingCart` | E-commerce shopping cart |

### Business Domain Examples (75-99)

| # | Directory | Description |
|---|-----------|-------------|
| 75 | `75_UserAuthentication` | User authentication system |
| 76 | `76_TaggingSystem` | Tag/labeling system |
| 77 | `77_RatingReviews` | Rating and review system |
| 78 | `78_NotificationSystem` | Notification queue/system |
| 79 | `79_BookingReservation` | Booking/reservation system |
| 80 | `80_RowLevelSecurity` | Row-level security (RLS) |
| 81 | `81_DataMasking` | Data masking for sensitive info |
| 82 | `82_SecureDelete` | Secure data deletion |
| 83 | `83_QueryPlanAnalysis` | Query plan analysis (EXPLAIN) |
| 84 | `84_DatabaseStatistics` | Database statistics and analysis |
| 85 | `85_TablePartitioning` | Table partitioning strategies |
| 86 | `86_WebhookStorage` | Webhook storage/queuing |
| 87 | `87_EmailQueue` | Email queue system |
| 88 | `88_APIRateLimiting` | API rate limiting with database |
| 89 | `89_UserActivityTracking` | User activity tracking |
| 90 | `90_FunnelAnalysis` | Funnel analytics patterns |
| 91 | `91_CohortAnalysis` | Cohort analysis patterns |
| 92 | `92_Savepoints` | Nested transactions with savepoints |
| 93 | `93_StateMachine` | State machine patterns |
| 94 | `94_CQRS` | Command Query Responsibility Segregation |
| 95 | `95_WindowFunctions` | Window functions (OVER clause) |
| 96 | `96_CTEAdvanced` | Advanced Common Table Expressions |
| 97 | `97_ReturningClause` | RETURNING clause usage |
| 98 | `98_DataSync` | Data synchronization patterns |
| 99 | `99_ChangeLog` | Change logging (audit trail) |

### Enterprise and Specialized (100-148)

| # | Directory | Description |
|---|-----------|-------------|
| 100 | `100_MergeReplication` | Merge replication patterns |
| 101 | `101_WorkflowEngine` | Workflow/state engine |
| 102 | `102_InventoryManagement` | Inventory management system |
| 103 | `103_InvoiceSystem` | Invoice and billing system |
| 104 | `104_ChatMessaging` | Chat/messaging application |
| 105 | `105_DataDeduplication` | Duplicate data detection/removal |
| 106 | `106_DataLineage` | Data lineage tracking |
| 107 | `107_SchemaEvolution` | Schema evolution strategies |
| 108 | `108_RepositoryPattern` | Repository design pattern |
| 109 | `109_UnitOfWork` | Unit of Work pattern |
| 110 | `110_EventSourcing` | Event sourcing architecture |
| 111 | `111_QueryProfiler` | Query performance profiling |
| 112 | `112_DatabaseMonitoring` | Database monitoring and alerts |
| 113 | `113_DataArchiving` | Historical data archiving |
| 114 | `114_DoubleEntryLedger` | Accounting double-entry ledger |
| 115 | `115_PaymentProcessing` | Payment transaction processing |
| 116 | `116_BudgetTracking` | Budget tracking system |
| 117 | `117_OutboxPattern` | Transactional outbox pattern |
| 118 | `118_SagaPattern` | Saga pattern for distributed transactions |
| 119 | `119_IdempotencyKeys` | Idempotent request handling |
| 120 | `120_JobScheduler` | Job scheduling system |
| 121 | `121_CalendarEvents` | Calendar/event system |
| 122 | `122_ResourceBooking` | Resource booking system |
| 123 | `123_DataProfiling` | Data quality profiling |
| 124 | `124_DataReconciliation` | Data reconciliation |
| 125 | `125_AnomalyDetection` | Anomaly detection patterns |
| 126 | `126_FuzzySearch` | Fuzzy string search |
| 127 | `127_Autocomplete` | Autocomplete/prefix search |
| 128 | `128_SearchAnalytics` | Search analytics and logging |
| 129 | `129_DataAnonymization` | Data anonymization/obfuscation |
| 130 | `130_ConsentManagement` | GDPR consent management |
| 131 | `131_RetentionPolicies` | Data retention policies |
| 132 | `132_DatabaseDiff` | Database comparison/diff tools |
| 133 | `133_DataSnapshot` | Point-in-time snapshots |
| 134 | `134_TestFixtures` | Test data fixtures |
| 135 | `135_SensorDataStore` | IoT sensor data storage |
| 136 | `136_Downsampling` | Time-series downsampling |
| 137 | `137_AlertingRules` | Alerting rules engine |
| 138 | `138_ShortestPath` | Graph shortest path (Dijkstra) |
| 139 | `139_PageRank` | Graph PageRank algorithm |
| 140 | `140_RecommendationEngine` | Product recommendation engine |
| 141 | `141_OptimisticConcurrencyAdvanced` | Advanced optimistic locking |
| 142 | `142_PubSubDatabase` | Pub/Sub message pattern |
| 143 | `143_DistributedLock` | Distributed locking mechanism |
| 144 | `144_CMS` | Content Management System |
| 145 | `145_FileMetadataStore` | File metadata storage |
| 146 | `146_TemplateEngine` | SQL template engine |
| 147 | `147_VersionInfo` | Version information system |
| 148 | `148_SQLCipherEncryption` | Advanced SQLCipher encryption |

---

## GUI Examples (10)

Location: `examples/gui/`

| # | Directory | Description |
|---|-----------|-------------|
| 00 | `00_DateTimeUtils` | GUI datetime utilities |
| 01 | `01_DBGrid` | Data-aware grid control |
| 02 | `02_MasterDetail` | Master-detail form relationships |
| 03 | `03_CachedUpdates` | Cached updates pattern |
| 04 | `04_AsyncProgress` | Async operations with progress |
| 05 | `05_Transactions` | GUI transaction handling |
| 06 | `06_SearchFilter` | Search and filter interface |
| 07 | `07_MultiThread` | Multi-threading in GUI |
| 08 | `08_BlobImages` | Binary image handling |
| 09 | `09_VersionInfo` | Version information GUI |

---

## Example Categories by Feature

### Connection Management
- 01, 06, 07, 08, 09, 10, 15, 17

### Query Execution
- 03, 04, 22, 30, 31, 32, 36, 37, 95, 96

### Transactions
- 02, 52, 92

### Data Types
- 04, 25, 27

### Encryption
- 43, 148

### Full-Text Search
- 13, 126, 127

### Spatial (R-Tree)
- 61, 66

### JSON
- 12, 49

### Schema Management
- 14, 33, 107

### Performance
- 16, 29, 34, 35, 83, 84, 111

### Security
- 43, 68, 75, 80, 81, 82, 129, 130, 148

### Advanced Features
- 62, 63, 64, 67, 69, 70

---

## Example Structure

Each example contains:

```
XX_ExampleName/
├── ExampleName.lpi       # Lazarus project file
├── ExampleName.lpr       # Program source
└── README.md             # Documentation
```

---

## Building All Examples

```bash
# Build all console examples
cd examples/console
for dir in */; do
  cd "$dir"
  if [ -f "*.lpr" ]; then
    fpc -Fu../../../src *.lpr
  fi
  cd ..
done

# Build with Lazarus
for dir in */; do
  cd "$dir"
  if [ -f "*.lpi" ]; then
    lazbuild *.lpi
  fi
  cd ..
done
```

---

## Tips

1. **Start simple** - Begin with examples 01-10
2. **Read the code** - Each example is well-commented
3. **Check README.md** - Each directory has documentation
4. **Modify and experiment** - Examples are designed for learning
5. **Use as templates** - Copy examples as starting points

---

*NDXSQLite Examples - Version 1.0.0*
