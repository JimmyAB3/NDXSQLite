# Example 01: Basic Connection

This example demonstrates the fundamental operations with NDXSQLite:

## What you'll learn

- How to create a database connection
- How to open and close the database
- How to create a table using `ExecuteNonQuery`
- How to insert data
- How to query data using `ExecuteQuery`
- How to iterate through a `TDataSet`

## Key concepts

### Creating a connection

```pascal
Connection := TNDXSQLiteConnection.Create('path/to/database.db');
Connection.Open;
```

### Executing SQL without results

```pascal
Connection.ExecuteNonQuery('CREATE TABLE ...');
Connection.ExecuteNonQuery('INSERT INTO ...');
```

### Querying data

```pascal
DataSet := Connection.ExecuteQuery('SELECT * FROM users');
try
  while not DataSet.EOF do
  begin
    WriteLn(DataSet.FieldByName('name').AsString);
    DataSet.Next;
  end;
finally
  DataSet.Free;
end;
```

## Building

```bash
lazbuild BasicConnection.lpi
```

## Running

```bash
./BasicConnection      # Linux/macOS
BasicConnection.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 01: Basic Connection ===

1. Opening database: .../example01.db
   Database opened successfully.

2. Creating table "users"...
   Table created successfully.

3. Inserting data...
   3 records inserted.

4. Querying all users:
   -----------------------------------------
   ID: 1 | Name: Alice      | Email: alice@example.com      | Age: 30
   ID: 2 | Name: Bob        | Email: bob@example.com        | Age: 25
   ID: 3 | Name: Charlie    | Email: charlie@example.com    | Age: 35
   -----------------------------------------

5. Closing database...
   Database closed successfully.

=== Example completed successfully! ===
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
