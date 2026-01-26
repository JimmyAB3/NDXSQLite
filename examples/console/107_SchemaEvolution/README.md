# Example 107: Schema Evolution - Versioned Migrations, Backward Compatibility, Rollback

## Overview

This example demonstrates a complete **schema evolution system** with versioned migrations, forward/backward migration procedures, backward-compatible column renames, rollback support, schema inspection, and a full audit trail.

## Features Demonstrated

### 1. Apply Migrations
- Sequential version numbering (v1 through v5)
- Each migration has an Apply and Rollback procedure
- Migration registry tracks status per version
- Data seeding between migrations

### 2. Verify Data Integrity
- Data preserved through schema changes
- Column additions don't break existing data
- Foreign key relationships maintained
- Backward-compat views query correctly

### 3. Migration History
- Version, name, description, timestamps
- Status tracking: pending, applied, rolled_back
- Applied and rolled_back timestamps

### 4. Rollback
- Undo migrations in reverse order
- Table rebuild pattern for column removal (SQLite limitation)
- PRAGMA foreign_keys = OFF during rebuild
- Data preservation through rollback

### 5. Re-apply After Fix
- Delete failed version entry
- Re-run migration with corrections
- Audit log shows apply/rollback/re-apply sequence

### 6. Schema Inspection
- List tables and views from sqlite_master
- PRAGMA table_info for column details
- Index listing
- Runtime schema verification

### 7. Migration Audit Log
- Every apply and rollback recorded
- Action details for debugging
- Timestamp for each operation

### 8. Migration Status
- Current vs target version comparison
- Pending migration detection
- Rollback count tracking

## Migration Types Demonstrated

| Version | Name | Type | Breaking? |
|---------|------|------|-----------|
| v1 | create_users | Create table | N/A (initial) |
| v2 | add_created_at | Add column | No |
| v3 | create_orders | Create table | No |
| v4 | add_display_name | Add + populate | No |
| v5 | rename_email | Rename + compat view | No (view) |

## Database Schema

```
+------------------+     +------------------+
| schema_versions  |     | migration_log    |
+------------------+     +------------------+
| version (PK)     |     | id (PK, AUTO)    |
| name             |     | version          |
| description      |     | action           |
| applied_at       |     | details          |
| rolled_back_at   |     | occurred_at      |
| status           |     +------------------+
+------------------+

Application tables (evolving):
+------------------+     +------------------+
| users            |     | orders           |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| username (UNIQUE)|     | user_id (FK)     |
| email            |     | amount           |
| created_at (v2+) |     | ordered_at       |
| display_name(v4+)|     +------------------+
| contact_email(v5)|
+------------------+
          |
          v
+------------------+
| users_compat (v5)|  <- VIEW with email alias
+------------------+
```

## Compilation

```bash
cd 107_SchemaEvolution
lazbuild SchemaEvolution.lpi
./SchemaEvolution
```

## Sample Output

```
1. Apply Migrations (v1 through v5)
   Applying v1: create_users...  OK
   Applying v2: add_created_at... OK
   Applying v5: rename_email... OK
   Current schema version: 5

4. Rollback Migration v5
   Current version before rollback: 5
   Rolling back v5 (rename_email)... OK
   Current version after rollback: 4

8. Migration Status Summary
   Current schema version: 5
   Target schema version:  5
   Status: UP TO DATE
   Total rollbacks: 1
```

## Related Examples

- **105_DataDeduplication** - Data quality management
- **106_DataLineage** - Data origin tracking

## Best Practices

1. **Sequential versions**: Number migrations for ordering and gap detection
2. **Idempotent apply**: Use IF NOT EXISTS and check current state
3. **Reversible migrations**: Every Apply needs a matching Rollback
4. **Non-breaking additions**: Add columns, tables, indexes without breaking existing code
5. **Backward compat views**: Rename columns via new column + view with old name
6. **FK disable for rebuild**: Use PRAGMA foreign_keys = OFF when rebuilding tables
7. **Data population**: Backfill new columns immediately after adding them
8. **Audit everything**: Log every apply/rollback with details and timestamps
9. **Version check**: Compare current vs target to detect pending migrations
