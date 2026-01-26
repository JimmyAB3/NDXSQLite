{===============================================================================
  NDXSQLite Example 14 - Database Migrations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Schema versioning
  - Up/Down migrations
  - SQL-based migrations
  - Migration tracking
  - Rolling back changes

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Migrations;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqlitemigration;

type
  { Custom migration class }
  TAddIndexMigration = class(TNDXMigration)
  protected
    procedure DoUp(AConnection: INDXSQLiteConnection); override;
    procedure DoDown(AConnection: INDXSQLiteConnection); override;
  end;

  { Logger class for migration events }
  TMigrationLogger = class
  public
    procedure OnMigrationLog(const AMessage: string);
  end;

var
  Conn: INDXSQLiteConnection;
  MigrationManager: TNDXMigrationManager;
  SQLMigration: TNDXSQLMigration;
  StatusList: TStringList;
  DBPath: string;
  I: Integer;
  Logger: TMigrationLogger;

{ TAddIndexMigration }

procedure TAddIndexMigration.DoUp(AConnection: INDXSQLiteConnection);
begin
  AConnection.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_users_email ON users(email)');
  AConnection.ExecuteNonQuery(
    'CREATE INDEX IF NOT EXISTS idx_users_created ON users(created_at)');
end;

{ Rolls back the index migration by dropping created indexes. }
procedure TAddIndexMigration.DoDown(AConnection: INDXSQLiteConnection);
begin
  AConnection.ExecuteNonQuery('DROP INDEX IF EXISTS idx_users_email');
  AConnection.ExecuteNonQuery('DROP INDEX IF EXISTS idx_users_created');
end;

{ TMigrationLogger }

procedure TMigrationLogger.OnMigrationLog(const AMessage: string);
begin
  WriteLn('   [LOG] ', AMessage);
end;

begin
  WriteLn('=== NDXSQLite Example 14: Database Migrations ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example14.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Logger := TMigrationLogger.Create;
  try
    Conn := TNDXSQLiteConnection.Create(DBPath);
    Conn.Open;

    // 1. Create migration manager
    WriteLn('1. Creating migration manager...');
    MigrationManager := TNDXMigrationManager.Create(Conn, '_migrations');
    try
      MigrationManager.OnLog := @Logger.OnMigrationLog;

      WriteLn('   Migration table: ', MigrationManager.TableName);
      WriteLn('   Current version: ', MigrationManager.CurrentVersion);
      WriteLn;

      // 2. Register SQL-based migrations
      WriteLn('2. Registering migrations...');

      // Migration 1: Create users table
      MigrationManager.RegisterSQLMigration(1, 'Create users table',
        'CREATE TABLE users (id INTEGER PRIMARY KEY, username TEXT NOT NULL, email TEXT)',
        'DROP TABLE users');

      // Migration 2: Add created_at column (using full SQL migration class)
      SQLMigration := TNDXSQLMigration.Create(2, 'Add timestamps',
        'Add created_at and updated_at columns');
      SQLMigration.AddUpSQL('ALTER TABLE users ADD COLUMN created_at TEXT');
      SQLMigration.AddUpSQL('ALTER TABLE users ADD COLUMN updated_at TEXT');
      // Note: SQLite doesn't support DROP COLUMN directly
      SQLMigration.AddDownSQL('-- Cannot remove columns in SQLite without recreating table');
      MigrationManager.RegisterMigration(SQLMigration);

      // Migration 3: Custom code migration (add indexes)
      MigrationManager.RegisterMigration(
        TAddIndexMigration.Create(3, 'Add indexes', 'Add indexes on email and created_at'));

      // Migration 4: Create posts table
      MigrationManager.RegisterSQLMigration(4, 'Create posts table',
        'CREATE TABLE posts (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users(id), ' +
        'title TEXT NOT NULL, content TEXT, created_at TEXT)',
        'DROP TABLE posts');

      WriteLn('   Registered 4 migrations.');
      WriteLn('   Pending: ', MigrationManager.HasPendingMigrations);
      WriteLn;

      // 3. Check migration status
      WriteLn('3. Migration status (before migration):');
      StatusList := MigrationManager.GetMigrationStatus;
      try
        for I := 0 to StatusList.Count - 1 do
          WriteLn('   ', StatusList[I]);
      finally
        StatusList.Free;
      end;
      WriteLn;

      // 4. Run all migrations
      WriteLn('4. Running all pending migrations...');
      MigrationManager.MigrateUp;
      WriteLn;
      WriteLn('   Current version: ', MigrationManager.CurrentVersion);
      WriteLn;

      // 5. Check status after migration
      WriteLn('5. Migration status (after migration):');
      StatusList := MigrationManager.GetMigrationStatus;
      try
        for I := 0 to StatusList.Count - 1 do
          WriteLn('   ', StatusList[I]);
      finally
        StatusList.Free;
      end;
      WriteLn;

      // 6. Verify schema
      WriteLn('6. Verifying schema:');
      WriteLn('   Tables created:');
      WriteLn('   - users: ', Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name=''users''') = 1);
      WriteLn('   - posts: ', Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name=''posts''') = 1);
      WriteLn('   - _migrations: ', Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name=''_migrations''') = 1);
      WriteLn;

      // 7. Insert test data
      WriteLn('7. Inserting test data...');
      Conn.ExecuteNonQuery(
        'INSERT INTO users (username, email, created_at) VALUES (?, ?, ?)',
        ['john', 'john@example.com', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
      Conn.ExecuteNonQuery(
        'INSERT INTO posts (user_id, title, content, created_at) VALUES (?, ?, ?, ?)',
        [1, 'Hello World', 'My first post', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
      WriteLn('   Data inserted.');
      WriteLn;

      // 8. Rollback one migration
      WriteLn('8. Rolling back last migration:');
      MigrationManager.MigrateDown;
      WriteLn;
      WriteLn('   Current version: ', MigrationManager.CurrentVersion);
      WriteLn('   Posts table exists: ', Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name=''posts''') = 1);
      WriteLn;

      // 9. Migrate back up
      WriteLn('9. Migrating back up:');
      MigrationManager.MigrateUp;
      WriteLn;
      WriteLn('   Current version: ', MigrationManager.CurrentVersion);
      WriteLn;

      // 10. Migrate to specific version
      WriteLn('10. Migrate to specific version (2):');
      MigrationManager.MigrateTo(2);
      WriteLn;
      WriteLn('   Current version: ', MigrationManager.CurrentVersion);
      WriteLn;

      // 11. Reset (rollback all)
      WriteLn('11. Resetting all migrations:');
      MigrationManager.Reset;
      WriteLn;
      WriteLn('   Current version: ', MigrationManager.CurrentVersion);
      WriteLn;

      // 12. Best practices
      WriteLn('12. Migration Best Practices:');
      WriteLn('    - Always provide both Up and Down migrations');
      WriteLn('    - Use sequential version numbers');
      WriteLn('    - Test migrations on copy of production data');
      WriteLn('    - Keep migrations small and focused');
      WriteLn('    - Never modify applied migrations');
      WriteLn('    - Use transactions (handled automatically)');
      WriteLn;

    finally
      MigrationManager.Free;
    end;

    Conn.Close;
    Conn := nil;

  finally
    Logger.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
