# Example 11: Backup and Restore

This example demonstrates database backup and restore operations.

## What you'll learn

- Simple backup using Connection.BackupTo
- Backup with TNDXSQLiteBackup manager
- Backup options configuration
- Progress monitoring
- Backup verification
- Restore from backup
- Generating timestamped backup names

## Key concepts

### Simple backup (one-liner)

```pascal
Connection.BackupTo('backup.db');
```

### Backup with TNDXSQLiteBackup manager

```pascal
Backup := TNDXSQLiteBackup.Create(Connection);
try
  // Configure options
  BackupOptions.PagesPerStep := 50;
  BackupOptions.StepDelayMs := 0;
  BackupOptions.OverwriteExisting := True;
  BackupOptions.VerifyAfterBackup := True;

  Result := Backup.BackupTo('backup.db', BackupOptions);

  if Result.Success then
  begin
    WriteLn('Size: ', Result.DestinationSizeBytes, ' bytes');
    WriteLn('Pages: ', Result.TotalPages);
    WriteLn('Time: ', Result.ElapsedTimeMs, ' ms');
    WriteLn('Verified: ', Result.Verified);
  end;
finally
  Backup.Free;
end;
```

### Verify backup integrity

```pascal
if Backup.VerifyBackup('backup.db') then
  WriteLn('Backup integrity OK');
```

### Generate timestamped backup name

```pascal
BackupName := Backup.GenerateBackupName('mydb');
// Result: mydb_20240115_143022.db
```

### Restore from backup

```pascal
Result := Backup.RestoreFrom('backup.db');
if Result.Success then
  WriteLn('Database restored successfully');
```

## Backup Result Properties

| Property | Description |
|----------|-------------|
| Success | True if backup completed |
| DestinationSizeBytes | Backup file size |
| TotalPages | Number of pages copied |
| ElapsedTimeMs | Time taken in milliseconds |
| Verified | True if integrity check passed |
| ErrorMessage | Error description if failed |

## Building

```bash
lazbuild BackupRestore.lpi
```

## Running

```bash
./BackupRestore      # Linux/macOS
BackupRestore.exe    # Windows
```

## Best Practices

- Schedule regular backups
- Always verify backup integrity
- Implement backup rotation
- Store backups in different location
- Test restore procedure regularly

## Cross-Platform

This example works on Windows, Linux, and macOS.
