# Example 03: Prepared Statements

This example demonstrates parameterized queries for security and performance.

## What you'll learn

- How to use parameterized queries with `?` placeholders
- How prepared statements prevent SQL injection
- Performance benefits of parameterized queries
- Using parameters with different data types

## Key concepts

### Parameterized INSERT

```pascal
Connection.ExecuteNonQuery(
  'INSERT INTO products (name, price, quantity) VALUES (?, ?, ?)',
  ['Laptop', 999.99, 10]);
```

### Parameterized SELECT

```pascal
DataSet := Connection.ExecuteQuery(
  'SELECT * FROM products WHERE price > ? AND active = ?',
  [100.00, 1]);
```

### Parameterized scalar query

```pascal
Count := Connection.ExecuteScalar(
  'SELECT COUNT(*) FROM products WHERE price > ?',
  [50.00]);
```

## SQL Injection Prevention

**Unsafe (string concatenation):**
```pascal
// DON'T DO THIS!
SQL := 'SELECT * FROM users WHERE name = ''' + UserInput + '''';
```

**Safe (parameterized):**
```pascal
// Always use this approach
DataSet := Connection.ExecuteQuery(
  'SELECT * FROM users WHERE name = ?',
  [UserInput]);
```

## Building

```bash
lazbuild PreparedStatements.lpi
```

## Running

```bash
./PreparedStatements      # Linux/macOS
PreparedStatements.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
