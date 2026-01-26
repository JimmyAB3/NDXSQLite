# Example 28: Foreign Keys

This example demonstrates foreign key constraints in SQLite.

## What you'll learn

- Enabling foreign key enforcement
- Creating tables with FK constraints
- ON DELETE actions: CASCADE, SET NULL, RESTRICT
- ON UPDATE CASCADE
- Checking FK violations
- Self-referential foreign keys (hierarchies)

## Key concepts

### Enabling foreign keys

```pascal
// IMPORTANT: FK enforcement is OFF by default in SQLite!
// Must enable per connection BEFORE using tables
Connection.ExecuteNonQuery('PRAGMA foreign_keys = ON');

// Verify it's enabled
DS := Connection.ExecuteQuery('PRAGMA foreign_keys');
// Returns 1 if enabled, 0 if disabled
```

### Creating FK constraints

```pascal
Connection.ExecuteNonQuery(
  'CREATE TABLE employees (' +
  '  id INTEGER PRIMARY KEY,' +
  '  name TEXT NOT NULL,' +
  '  dept_id INTEGER,' +
  '  FOREIGN KEY (dept_id) REFERENCES departments(id)' +
  ')');
```

### ON DELETE CASCADE

```pascal
// Child rows automatically deleted when parent is deleted
Connection.ExecuteNonQuery(
  'CREATE TABLE tasks (' +
  '  id INTEGER PRIMARY KEY,' +
  '  project_id INTEGER,' +
  '  FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE' +
  ')');

// Deleting a project automatically deletes all its tasks
Connection.ExecuteNonQuery('DELETE FROM projects WHERE id = 1');
```

### ON DELETE SET NULL

```pascal
// Child FK set to NULL when parent is deleted
Connection.ExecuteNonQuery(
  'CREATE TABLE teams (' +
  '  id INTEGER PRIMARY KEY,' +
  '  manager_id INTEGER,' +
  '  FOREIGN KEY (manager_id) REFERENCES managers(id) ON DELETE SET NULL' +
  ')');

// Deleting a manager sets teams.manager_id to NULL
Connection.ExecuteNonQuery('DELETE FROM managers WHERE id = 1');
```

### ON UPDATE CASCADE

```pascal
// Child FK updated when parent PK changes
Connection.ExecuteNonQuery(
  'CREATE TABLE products (' +
  '  id INTEGER PRIMARY KEY,' +
  '  category_code TEXT,' +
  '  FOREIGN KEY (category_code) REFERENCES categories(code) ON UPDATE CASCADE' +
  ')');

// Updating category code cascades to products
Connection.ExecuteNonQuery('UPDATE categories SET code = ''NEW'' WHERE code = ''OLD''');
```

### Check FK violations

```pascal
// Find rows that violate FK constraints
DS := Connection.ExecuteQuery('PRAGMA foreign_key_check');
// Returns: table, rowid, parent_table, fk_index
```

### List foreign keys on a table

```pascal
DS := Connection.ExecuteQuery('PRAGMA foreign_key_list(employees)');
// Returns: id, seq, table, from, to, on_update, on_delete, match
```

### Self-referential FK (hierarchies)

```pascal
Connection.ExecuteNonQuery(
  'CREATE TABLE org_chart (' +
  '  id INTEGER PRIMARY KEY,' +
  '  name TEXT,' +
  '  manager_id INTEGER,' +
  '  FOREIGN KEY (manager_id) REFERENCES org_chart(id)' +
  ')');

// CEO has no manager
INSERT INTO org_chart VALUES (1, 'CEO', NULL);
INSERT INTO org_chart VALUES (2, 'VP', 1);  // Reports to CEO
```

## ON DELETE actions

| Action | Behavior |
|--------|----------|
| `RESTRICT` | Prevent delete if children exist (default) |
| `CASCADE` | Delete all children automatically |
| `SET NULL` | Set child FK to NULL |
| `SET DEFAULT` | Set child FK to default value |
| `NO ACTION` | Same as RESTRICT in SQLite |

## Building

```bash
lazbuild ForeignKeys.lpi
```

## Running

```bash
./ForeignKeys      # Linux/macOS
ForeignKeys.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
