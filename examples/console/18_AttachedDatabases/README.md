# Example 18: Attached Databases

This example demonstrates working with multiple databases using ATTACH.

## What you'll learn

- ATTACH DATABASE to connect multiple databases
- Cross-database queries with JOINs
- Data migration between databases
- DETACH DATABASE

## Key concepts

### Attaching a database

```pascal
Connection.ExecuteNonQuery('ATTACH DATABASE ''path/to/other.db'' AS other_db');
```

### Cross-database query

```pascal
// Use schema prefix to reference tables
DS := Connection.ExecuteQuery(
  'SELECT c.name, o.product ' +
  'FROM main.customers c ' +
  'JOIN other_db.orders o ON c.id = o.customer_id');
```

### Data migration

```pascal
// Copy data between databases
Connection.ExecuteNonQuery(
  'INSERT INTO archive_db.orders_archive ' +
  'SELECT * FROM main.orders WHERE date < ''2024-01-01''');
```

### Detaching a database

```pascal
Connection.ExecuteNonQuery('DETACH DATABASE other_db');
```

### Listing attached databases

```pascal
DS := Connection.ExecuteQuery('PRAGMA database_list');
```

## Use cases

| Use Case | Description |
|----------|-------------|
| Data archiving | Move old data to archive database |
| Data migration | Transfer data between databases |
| Reporting | Query across multiple data sources |
| Sharding | Split data across multiple files |

## Building

```bash
lazbuild AttachedDatabases.lpi
```

## Running

```bash
./AttachedDatabases      # Linux/macOS
AttachedDatabases.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
