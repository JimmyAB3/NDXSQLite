# Example 14: Database Migrations

This example demonstrates schema versioning and migrations.

## What you'll learn

- Creating migration manager
- SQL-based migrations
- Custom code migrations
- Tracking applied migrations
- Rolling back changes

## Key concepts

### Creating migration manager

```pascal
MigrationManager := TNDXMigrationManager.Create(Connection, '_migrations');
```

### SQL-based migration

```pascal
MigrationManager.RegisterSQLMigration(
  1,  // Version
  'Create users table',  // Name
  'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)',  // Up SQL
  'DROP TABLE users'  // Down SQL
);
```

### Custom code migration

```pascal
type
  TMyMigration = class(TNDXMigration)
  protected
    procedure DoUp(AConnection: INDXSQLiteConnection); override;
    procedure DoDown(AConnection: INDXSQLiteConnection); override;
  end;

procedure TMyMigration.DoUp(AConnection: INDXSQLiteConnection);
begin
  AConnection.ExecuteNonQuery('CREATE INDEX idx_email ON users(email)');
end;
```

### Running migrations

```pascal
// Apply all pending migrations
MigrationManager.MigrateUp;

// Rollback last migration
MigrationManager.MigrateDown;

// Migrate to specific version
MigrationManager.MigrateTo(3);

// Reset all (rollback everything)
MigrationManager.Reset;
```

### Checking status

```pascal
WriteLn('Current version: ', MigrationManager.CurrentVersion);
WriteLn('Has pending: ', MigrationManager.HasPendingMigrations);

StatusList := MigrationManager.GetMigrationStatus;
```

## Building

```bash
lazbuild Migrations.lpi
```

## Running

```bash
./Migrations      # Linux/macOS
Migrations.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
