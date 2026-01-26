# Example 132: Database Diff - Schema Comparison, Migration SQL, Semantic Diff

## Overview

This example demonstrates a **Schema Comparison Engine** that detects structural differences between two database versions, generates migration SQL, and provides semantic analysis of schema evolution. It simulates a database migration planning tool that helps developers understand and execute schema changes safely.

## Features Demonstrated

### 1. Source Schema (v1.0)
- 5 tables: users, products, orders, order_items, categories
- 24 columns with types, nullability, and defaults
- 6 indexes (unique and non-unique)
- E-commerce data model baseline

### 2. Target Schema (v2.0)
- 7 tables: users, products, orders, order_items, user_auth, tags, product_tags
- 36 columns with enhanced type precision
- 11 indexes including new unique constraints
- Evolved architecture with auth separation and tag system

### 3. Table-Level Diff
- 3 tables added (product_tags, tags, user_auth)
- 1 table removed (categories)
- 4 tables common with potential column changes
- Clear +/- notation for additions and removals

### 4. Column-Level Diff
- 7 columns added across common tables
- 2 columns removed (moved or deprecated)
- Nullability and type information for each change
- Ordered by table and column name

### 5. Data Type Changes
- 4 type changes detected (REAL -> DECIMAL, TEXT -> VARCHAR)
- Compatibility assessment (widening vs narrowing)
- SQLite dynamic typing considerations
- Precision improvement tracking

### 6. Index Diff
- 6 indexes added (including unique constraints)
- 1 index removed (obsolete FK index)
- 5 indexes unchanged
- Uniqueness flag tracking

### 7. Migration SQL Generation
- 20-step ordered migration script
- Correct execution order: drops before adds, tables before indexes
- ALTER TABLE ADD/DROP COLUMN statements
- CREATE/DROP INDEX and CREATE/DROP TABLE statements

### 8. Semantic Diff Summary
- Architectural change analysis (auth separation, category refactoring)
- Data model improvement descriptions
- Type precision changes
- Breaking change identification

### 9. Nullability and Default Changes
- 3 changes detected across common columns
- Nullable -> NOT NULL transitions
- Default value modifications
- Data backfill requirement warnings

### 10. Statistics
- 24 total differences categorized by type
- Schema size comparison (tables, columns, indexes)
- Delta calculation with signed values
- Migration complexity assessment

## Architecture

```
+------------------+     +------------------+     +------------------+
| Schema Registry  |     | Diff Engine      |     | Output Generator |
+------------------+     +------------------+     +------------------+
| schema_tables    |---->| Table Compare    |---->| Migration SQL    |
| schema_columns   |     | Column Compare   |     | Semantic Diff    |
| schema_indexes   |     | Index Compare    |     | Statistics       |
| db_name field    |     | Type Analysis    |     | Change Report    |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| schema_tables    |     | schema_columns   |     | schema_indexes   |
+------------------+     +------------------+     +------------------+
| db_name          |     | db_name          |     | db_name          |
| table_name       |     | table_name       |     | table_name       |
+------------------+     | column_name      |     | index_name       |
                          | data_type        |     | columns          |
                          | is_nullable      |     | is_unique        |
                          | is_primary_key   |     +------------------+
                          | default_value    |
                          | column_order     |
                          +------------------+
```

## Schema Changes (v1.0 -> v2.0)

| Category | Change | Details |
|----------|--------|---------|
| Architecture | Auth separation | password_hash -> user_auth table |
| Architecture | Tag system | categories replaced by tags + product_tags |
| Data Model | Products | Added SKU, weight_kg; removed category_id |
| Data Model | Orders | Added shipping_address, delivery_date |
| Data Model | Users | Added phone, last_login |
| Types | Monetary | REAL -> DECIMAL(10,2) for precision |
| Nullability | description | nullable -> NOT NULL |
| Defaults | orders.status | 'pending' -> 'received' |
| Defaults | products.stock | '0' -> '1' |

## Migration Execution Order

```
1. Drop removed tables (categories)
2. Drop removed columns (category_id, password_hash)
3. Add new columns (7 ALTER TABLE ADD COLUMN)
4. Create new tables (product_tags, tags, user_auth)
5. Drop obsolete indexes (idx_products_category)
6. Create new indexes (6 CREATE INDEX)
```

## Compilation

```bash
cd 132_DatabaseDiff
lazbuild DatabaseDiff.lpi
./DatabaseDiff
```

## Sample Output

```
3. Table Diff: +3 added, -1 removed, 4 common
4. Column Diff: +7 added, -2 removed
5. Type Changes: 4 widening changes (REAL->DECIMAL, TEXT->VARCHAR)
6. Index Diff: +6 added, -1 removed, 5 unchanged
7. Migration: 20 steps generated
9. Nullability: 3 changes (nullable->NOT NULL, default changes)
10. Statistics: 24 total differences, Delta: +2 tables, +12 columns, +5 indexes
```

## Related Examples

- **133_DataSnapshot** - Point-in-time snapshots
- **134_TestFixtures** - Test data management

## Best Practices

1. **Registry approach**: Store schema metadata for comparison without needing two live databases
2. **Ordered migration**: Execute drops before adds to avoid conflicts
3. **Type compatibility**: Assess widening vs narrowing changes for data safety
4. **Breaking changes**: Identify changes that require data migration
5. **Semantic context**: Provide human-readable explanation of structural changes
6. **Nullability awareness**: Flag NOT NULL additions that need default values or backfill
7. **Index management**: Track unique constraints separately from regular indexes
8. **Complexity assessment**: Rate migrations based on breaking change count
