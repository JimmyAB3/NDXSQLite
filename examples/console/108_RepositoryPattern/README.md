# Example 108: Repository Pattern - Generic CRUD, Specifications, Query Builders

## Overview

This example demonstrates the **Repository Pattern** with generic CRUD operations, composable specifications for query filtering, a fluent query builder, aggregate queries, soft delete/restore, and batch operations.

## Features Demonstrated

### 1. CRUD Operations
- Create: INSERT with auto-increment ID
- Read: GetById with SELECT WHERE id = ?
- Update: SET specific fields by ID
- Delete: Hard delete by ID

### 2. Specification Pattern
- Encapsulate WHERE clauses as reusable objects
- Composable: AND, OR, NOT combinators
- Built-in specs: ByCategory, PriceRange, InStock, Active, MinStock
- Count matching records with any specification

### 3. Query Builder
- Fluent interface: Select, Where, OrderBy, Limit, Offset
- Compose with specifications via QWhereSpec
- JOIN support by setting FTable directly
- Generates clean SQL strings
- Pagination with LIMIT/OFFSET

### 4. Aggregate Queries
- COUNT, SUM, AVG per category
- Inventory valuation (price * stock)
- Group-by with LEFT JOIN

### 5. Soft Delete & Restore
- is_active flag instead of hard delete
- Toggle active status
- Specifications respect active flag

### 6. Batch Operations
- Transaction-wrapped batch inserts
- Batch update with WHERE clause
- Batch delete by pattern

### 7. Existence & Find
- Exists check by ID
- FindFirst with specification + ORDER BY

### 8. Statistics
- Repository-level counts with specifications
- Specification SQL inspection

## Architecture

```
+------------------+     +------------------+
| Application      |     | Specifications   |
+------------------+     +------------------+
| DemoCRUD()       |     | SpecAll          |
| DemoSpecs()      |---->| SpecByCategory   |
| DemoQuery()      |     | SpecPriceRange   |
+------------------+     | SpecInStock      |
        |                 | SpecActive       |
        v                 | SpecAnd/Or/Not   |
+------------------+     +------------------+
| Repository Ops   |             |
+------------------+             |
| RepoCount()      |<------------+
| RepoExists()     |
| RepoDelete()     |     +------------------+
| RepoSoftDelete() |     | Query Builder    |
+------------------+     +------------------+
        |                 | NewQuery()       |
        v                 | QSelect()        |
+------------------+     | QWhere()         |
| NDXSQLite        |     | QWhereSpec()     |
| Connection       |<----| QOrderBy()       |
+------------------+     | QLimit/QOffset() |
                          | QBuild() -> SQL  |
                          +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| categories       |     | products         |
+------------------+     +------------------+
| id (PK, AUTO)    |<--->| id (PK, AUTO)    |
| name (UNIQUE)    |     | name             |
| description      |     | category_id (FK) |
+------------------+     | price            |
                          | stock            |
                          | is_active        |
                          | created_at       |
                          +------------------+
```

## Compilation

```bash
cd 108_RepositoryPattern
lazbuild RepositoryPattern.lpi
./RepositoryPattern
```

## Sample Output

```
2. Specification Pattern
   Electronics AND InStock: 3
   Active AND under $50:   6
   NOT InStock (out):      2

3. Query Builder
   SQL: SELECT name, price FROM products WHERE is_active = 1 ORDER BY price DESC LIMIT 3
   - Laptop Pro 15        $1299.99
   - 4K Monitor           $599.99

8. Repository Statistics
   SpecActive:           is_active = 1
   Active AND InStock:   (is_active = 1) AND (stock > 0)
```

## Related Examples

- **109_UnitOfWork** - Transaction management with tracked changes
- **110_EventSourcing** - Event-based state management

## Best Practices

1. **Specification composability**: AND/OR/NOT allow building complex filters from simple parts
2. **Query builder**: Generates SQL without string concatenation errors
3. **Soft delete**: Use is_active flag for reversible deletion
4. **Batch transactions**: Wrap multiple inserts in BEGIN/COMMIT for performance
5. **Existence before action**: Check RepoExists before update/delete to avoid silent failures
6. **Separation of concerns**: Repository handles persistence, specs handle filtering
7. **Pagination**: Use LIMIT/OFFSET for large result sets
