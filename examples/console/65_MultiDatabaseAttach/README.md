# Example 65: Multi-Database Attach

This example demonstrates how to work with multiple SQLite databases simultaneously using `TNDXSQLiteAttached`.

## What you'll learn

- Attaching external databases to a connection
- Cross-database queries and JOINs
- Copying tables between databases
- Using in-memory attached databases
- Managing attached database lifecycle

## Key concepts

### Attaching a database

```pascal
Attached := TNDXSQLiteAttached.Create(Connection);

// Attach file-based database
Attached.Attach('/path/to/other.db', 'other_db');

// Attach in-memory database
Attached.AttachMemory('temp_db');
```

### Accessing attached tables

Use the database alias as prefix:

```sql
-- Query from attached database
SELECT * FROM other_db.customers;

-- Query from main database
SELECT * FROM main.products;

-- Cross-database JOIN
SELECT c.name, o.total
FROM other_db.customers c
JOIN main.orders o ON c.id = o.customer_id;
```

### Detaching databases

```pascal
// Detach specific database
Attached.Detach('other_db');

// Detach all attached databases
Attached.DetachAll;
```

### Listing attached databases

```pascal
DBList := Attached.GetAttachedList;
try
  for I := 0 to DBList.Count - 1 do
    WriteLn(DBList[I]);
finally
  DBList.Free;
end;
```

### Copying tables

```pascal
// Copy table from one database to another
Attached.CopyTable(
  'source_db',      // Source database alias
  'customers',      // Source table name
  'dest_db',        // Destination database alias
  'customers_copy', // Destination table name
  True              // Include data (False = schema only)
);
```

## Use cases

### Data consolidation

Combine data from multiple department databases:

```sql
SELECT 'Sales' as dept, * FROM sales_db.transactions
UNION ALL
SELECT 'Support' as dept, * FROM support_db.tickets;
```

### Archiving

Move old data to archive database:

```pascal
Attached.Attach('archive.db', 'archive');
Conn.ExecuteNonQuery(
  'INSERT INTO archive.orders SELECT * FROM main.orders WHERE date < ''2023-01-01''');
Conn.ExecuteNonQuery(
  'DELETE FROM main.orders WHERE date < ''2023-01-01''');
```

### Temporary processing

Use in-memory database for fast intermediate calculations:

```pascal
Attached.AttachMemory('temp');
Conn.ExecuteNonQuery('CREATE TABLE temp.results AS SELECT ...');
// Process results...
Attached.Detach('temp');  // Data discarded
```

### Data migration

Copy schema and data between databases:

```pascal
Attached.Attach('old_system.db', 'old');
Attached.Attach('new_system.db', 'new');
Attached.CopyTable('old', 'users', 'new', 'users', True);
```

## SQLite limitations

- Maximum **10 attached databases** per connection
- All attached databases share the same connection settings
- Transactions span all attached databases (atomic commits)
- Cannot attach the same database file twice

## Building

```bash
lazbuild MultiDatabaseAttach.lpi
```

## Running

```bash
./MultiDatabaseAttach      # Linux/macOS
MultiDatabaseAttach.exe    # Windows
```

## Notes

- Always use `database_alias.table_name` syntax for clarity
- `main` refers to the primary database
- `temp` is reserved for SQLite's temporary database
- Detach databases when no longer needed to free resources
- In-memory attached databases are lost when detached

## Cross-Platform

This example works on Windows, Linux, and macOS.
