# Example 71: Entity-Attribute-Value (EAV) Pattern

## Overview

This example demonstrates the **Entity-Attribute-Value (EAV)** pattern, a flexible data modeling approach that allows storing dynamic attributes without schema modifications. EAV is particularly useful when entities have many optional or varying attributes.

## What is the EAV Pattern?

The EAV pattern structures data using three core concepts:

- **Entity**: The object being described (e.g., a product, user, configuration)
- **Attribute**: The property name (e.g., "color", "size", "weight")
- **Value**: The actual data for that attribute

Instead of traditional columns:

```sql
-- Traditional approach (rigid schema)
CREATE TABLE products (
    id INTEGER PRIMARY KEY,
    name TEXT,
    color TEXT,      -- What if some products don't have color?
    size TEXT,       -- What if we need 100 different attributes?
    weight REAL
);
```

EAV uses a flexible structure:

```sql
-- EAV approach (flexible schema)
CREATE TABLE entities (id, entity_type, name);
CREATE TABLE attributes (id, attribute_name, data_type);
CREATE TABLE entity_values (entity_id, attribute_id, value_text, value_int, value_real);
```

## Demonstration Sections

### 1. Basic EAV Schema
Creates the fundamental three-table structure with proper foreign key relationships.

### 2. Populating Reference Data
Defines attribute types (color, size, weight, brand, etc.) with their expected data types.

### 3. Creating Entities with Values
Shows how to create products and assign various attributes dynamically.

### 4. Querying Entity Attributes
Retrieves all attributes for a specific entity using JOINs.

### 5. Pivot Queries
Transforms vertical EAV data into horizontal format for easier reading:
```
Product: T-Shirt
  color: Red
  size: Large
  brand: Nike
```
becomes:
```
name     | color | size  | brand
T-Shirt  | Red   | Large | Nike
```

### 6. Type-Safe Value Storage
Demonstrates storing values in appropriate columns based on data type (text, integer, real, boolean).

### 7. Attribute Search
Finds entities by attribute values (e.g., "all products where color = 'Blue'").

### 8. Schema Evolution
Shows how to add new attributes without ALTER TABLE - just insert into attributes table.

### 9. JSON Attribute Storage
Uses SQLite's JSON functions to store complex/nested attribute values.

### 10. Materialized Views for Performance
Creates summary tables to improve query performance for common access patterns.

## When to Use EAV

**Good use cases:**
- Product catalogs with varying attributes per category
- User profiles with optional fields
- Configuration systems
- Survey/form responses with dynamic questions
- Medical records with varying test results

**Avoid EAV when:**
- All entities have the same fixed attributes
- You need strong data validation
- Complex queries and joins are frequent
- Performance is critical (EAV queries can be slow)

## Key SQL Techniques

```sql
-- Getting all attributes for an entity
SELECT a.attribute_name,
       COALESCE(ev.value_text, ev.value_int, ev.value_real) as value
FROM entity_values ev
JOIN attributes a ON ev.attribute_id = a.id
WHERE ev.entity_id = ?;

-- Pivot query using CASE expressions
SELECT e.name,
       MAX(CASE WHEN a.attribute_name = 'color' THEN ev.value_text END) as color,
       MAX(CASE WHEN a.attribute_name = 'size' THEN ev.value_text END) as size
FROM entities e
LEFT JOIN entity_values ev ON e.id = ev.entity_id
LEFT JOIN attributes a ON ev.attribute_id = a.id
GROUP BY e.id, e.name;

-- Search by attribute value
SELECT DISTINCT e.*
FROM entities e
JOIN entity_values ev ON e.id = ev.entity_id
JOIN attributes a ON ev.attribute_id = a.id
WHERE a.attribute_name = 'color' AND ev.value_text = 'Blue';
```

## Compilation

```bash
cd 71_EAVPattern
lazbuild EAVPattern.lpi
./EAVPattern
```

## Related Examples

- **72_PolymorphicAssociations** - Another flexible data modeling pattern
- **73_ManyToManyAdvanced** - Complex relationship modeling with metadata
- **05_CRUD** - Basic database operations
- **13_Transactions** - Transaction handling for data integrity

## References

- [Wikipedia: Entity-Attribute-Value Model](https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model)
- [SQLite JSON Functions](https://www.sqlite.org/json1.html)
