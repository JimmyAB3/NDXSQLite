{===============================================================================
  NDXSQLite Example 145 - File Metadata Store
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - File indexing with path hierarchy
  - Checksum-based integrity verification
  - File deduplication by content hash
  - Bulk file operations
  - Search by extension, metadata, and size

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program FileMetadataStore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // File entries (files and directories)
  Conn.ExecuteNonQuery(
    'CREATE TABLE files (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  path TEXT NOT NULL UNIQUE, ' +
    '  filename TEXT NOT NULL, ' +
    '  extension TEXT, ' +
    '  parent_path TEXT, ' +
    '  is_directory INTEGER NOT NULL DEFAULT 0, ' +
    '  size_bytes INTEGER NOT NULL DEFAULT 0, ' +
    '  checksum TEXT, ' +
    '  mime_type TEXT, ' +
    '  created_at TEXT NOT NULL, ' +
    '  modified_at TEXT NOT NULL, ' +
    '  indexed_at TEXT NOT NULL)');

  // Custom metadata key-value pairs
  Conn.ExecuteNonQuery(
    'CREATE TABLE file_metadata (' +
    '  file_id INTEGER NOT NULL, ' +
    '  meta_key TEXT NOT NULL, ' +
    '  meta_value TEXT, ' +
    '  PRIMARY KEY(file_id, meta_key))');

  // Bulk operation log
  Conn.ExecuteNonQuery(
    'CREATE TABLE operations (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  operation TEXT NOT NULL, ' +
    '  file_count INTEGER NOT NULL, ' +
    '  total_bytes INTEGER NOT NULL DEFAULT 0, ' +
    '  status TEXT NOT NULL DEFAULT ''completed'', ' +
    '  started_at TEXT NOT NULL, ' +
    '  completed_at TEXT)');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Directory structure
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects'', ''projects'', ''/'', 1, 0, NULL, NULL, ''2025-01-01 09:00:00'', ''2025-01-20 10:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp'', ''webapp'', ''/projects'', 1, 0, NULL, NULL, ''2025-01-02 10:00:00'', ''2025-01-19 15:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/src'', ''src'', ''/projects/webapp'', 1, 0, NULL, NULL, ''2025-01-02 10:00:00'', ''2025-01-18 14:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/docs'', ''docs'', ''/projects/webapp'', 1, 0, NULL, NULL, ''2025-01-05 09:00:00'', ''2025-01-15 11:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/library'', ''library'', ''/projects'', 1, 0, NULL, NULL, ''2025-01-03 08:00:00'', ''2025-01-17 16:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/library/src'', ''src'', ''/projects/library'', 1, 0, NULL, NULL, ''2025-01-03 08:00:00'', ''2025-01-17 16:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/backups'', ''backups'', ''/'', 1, 0, NULL, NULL, ''2025-01-01 09:00:00'', ''2025-01-20 06:00:00'', ''2025-01-20 12:00:00'')');

  // Source files
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/src/main.pas'', ''main.pas'', ''pas'', ''/projects/webapp/src'', 0, 4520, ''a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2'', ''text/x-pascal'', ''2025-01-02 10:00:00'', ''2025-01-18 14:30:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/src/utils.pas'', ''utils.pas'', ''pas'', ''/projects/webapp/src'', 0, 2180, ''b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b200'', ''text/x-pascal'', ''2025-01-03 11:00:00'', ''2025-01-16 09:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/src/config.json'', ''config.json'', ''json'', ''/projects/webapp/src'', 0, 856, ''c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b20000'', ''application/json'', ''2025-01-02 10:00:00'', ''2025-01-10 08:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/src/database.pas'', ''database.pas'', ''pas'', ''/projects/webapp/src'', 0, 6340, ''d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2000000'', ''text/x-pascal'', ''2025-01-05 14:00:00'', ''2025-01-18 14:00:00'', ''2025-01-20 12:00:00'')');

  // Documentation files
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/docs/readme.md'', ''readme.md'', ''md'', ''/projects/webapp/docs'', 0, 3200, ''e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b200000000'', ''text/markdown'', ''2025-01-05 09:00:00'', ''2025-01-15 11:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/webapp/docs/api.md'', ''api.md'', ''md'', ''/projects/webapp/docs'', 0, 5400, ''f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b20000000000'', ''text/markdown'', ''2025-01-06 10:00:00'', ''2025-01-14 16:00:00'', ''2025-01-20 12:00:00'')');

  // Library files
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/library/src/core.pas'', ''core.pas'', ''pas'', ''/projects/library/src'', 0, 8920, ''1a2b3c4d5e6f1a2b3c4d5e6f1a2b3c4d5e6f1a2b3c4d5e6f1a2b3c4d5e6f1a2b'', ''text/x-pascal'', ''2025-01-03 08:00:00'', ''2025-01-17 16:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/projects/library/src/helpers.pas'', ''helpers.pas'', ''pas'', ''/projects/library/src'', 0, 2180, ''b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b200'', ''text/x-pascal'', ''2025-01-04 09:00:00'', ''2025-01-12 10:00:00'', ''2025-01-20 12:00:00'')');

  // Backup files (duplicates)
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/backups/main.pas.bak'', ''main.pas.bak'', ''bak'', ''/backups'', 0, 4520, ''a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2'', ''text/x-pascal'', ''2025-01-18 20:00:00'', ''2025-01-18 20:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/backups/database.pas.bak'', ''database.pas.bak'', ''bak'', ''/backups'', 0, 6340, ''d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2000000'', ''text/x-pascal'', ''2025-01-18 20:00:00'', ''2025-01-18 20:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/backups/core.pas.bak'', ''core.pas.bak'', ''bak'', ''/backups'', 0, 8920, ''1a2b3c4d5e6f1a2b3c4d5e6f1a2b3c4d5e6f1a2b3c4d5e6f1a2b3c4d5e6f1a2b'', ''text/x-pascal'', ''2025-01-17 22:00:00'', ''2025-01-17 22:00:00'', ''2025-01-20 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO files (path, filename, extension, parent_path, is_directory, size_bytes, checksum, mime_type, created_at, modified_at, indexed_at) VALUES ' +
    '(''/backups/readme.md.bak'', ''readme.md.bak'', ''bak'', ''/backups'', 0, 3200, ''e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b200000000'', ''text/markdown'', ''2025-01-15 23:00:00'', ''2025-01-15 23:00:00'', ''2025-01-20 12:00:00'')');

  // File metadata
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (8, ''author'', ''alice'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (8, ''language'', ''pascal'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (8, ''loc'', ''142'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (9, ''author'', ''bob'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (9, ''language'', ''pascal'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (9, ''loc'', ''68'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (10, ''format'', ''json'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (10, ''environment'', ''production'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (11, ''author'', ''alice'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (11, ''language'', ''pascal'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (11, ''loc'', ''198'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (12, ''author'', ''charlie'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (12, ''format'', ''markdown'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (13, ''author'', ''charlie'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (13, ''format'', ''markdown'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (14, ''author'', ''alice'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (14, ''language'', ''pascal'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (14, ''loc'', ''312'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (15, ''author'', ''bob'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (15, ''language'', ''pascal'')');
  Conn.ExecuteNonQuery('INSERT INTO file_metadata VALUES (15, ''loc'', ''68'')');

  // Operations log
  Conn.ExecuteNonQuery('INSERT INTO operations (operation, file_count, total_bytes, status, started_at, completed_at) VALUES ' +
    '(''full_scan'', 21, 56636, ''completed'', ''2025-01-20 12:00:00'', ''2025-01-20 12:00:05'')');
  Conn.ExecuteNonQuery('INSERT INTO operations (operation, file_count, total_bytes, status, started_at, completed_at) VALUES ' +
    '(''checksum_verify'', 14, 56636, ''completed'', ''2025-01-20 12:01:00'', ''2025-01-20 12:01:03'')');
  Conn.ExecuteNonQuery('INSERT INTO operations (operation, file_count, total_bytes, status, started_at, completed_at) VALUES ' +
    '(''metadata_update'', 8, 0, ''completed'', ''2025-01-20 12:02:00'', ''2025-01-20 12:02:01'')');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Lists all indexed files and directories with path, type, size, and modification date, then summarizes entry counts and total size. }
procedure Demo1_FileIndex;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. File Index Overview ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT path, ' +
    '  CASE WHEN is_directory = 1 THEN ''DIR'' ELSE extension END AS type, ' +
    '  size_bytes, modified_at ' +
    'FROM files ORDER BY path');
  try
    WriteLn(Format('   %-42s %-6s %10s  %s',
      ['Path', 'Type', 'Size', 'Modified']));
    WriteLn('   ' + StringOfChar('-', 90));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-42s %-6s %10d  %s', [
        DS.FieldByName('path').AsString,
        DS.FieldByName('type').AsString,
        DS.FieldByName('size_bytes').AsInteger,
        DS.FieldByName('modified_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) AS total, ' +
    '  SUM(CASE WHEN is_directory = 1 THEN 1 ELSE 0 END) AS dirs, ' +
    '  SUM(CASE WHEN is_directory = 0 THEN 1 ELSE 0 END) AS files_count, ' +
    '  SUM(size_bytes) AS total_size ' +
    'FROM files');
  try
    if not DS.EOF then
      WriteLn(Format('   Total: %d entries (%d dirs, %d files, %d bytes)', [
        DS.FieldByName('total').AsInteger,
        DS.FieldByName('dirs').AsInteger,
        DS.FieldByName('files_count').AsInteger,
        DS.FieldByName('total_size').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Builds a directory tree using recursive CTE and shows file distribution by path depth level. }
procedure Demo2_PathHierarchy;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Path Hierarchy ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE dir_tree(path, filename, depth, is_directory) AS (' +
    '  SELECT path, filename, 0, is_directory FROM files WHERE parent_path = ''/'' ' +
    '  UNION ALL ' +
    '  SELECT f.path, f.filename, dt.depth + 1, f.is_directory ' +
    '  FROM files f JOIN dir_tree dt ON f.parent_path = dt.path ' +
    ') ' +
    'SELECT path, filename, depth, is_directory FROM dir_tree ' +
    'ORDER BY path');
  try
    WriteLn('   Directory tree:');
    WriteLn;
    while not DS.EOF do
    begin
      if DS.FieldByName('is_directory').AsInteger = 1 then
        WriteLn(Format('   %s[%s]/', [
          StringOfChar(' ', DS.FieldByName('depth').AsInteger * 3),
          DS.FieldByName('filename').AsString]))
      else
        WriteLn(Format('   %s%s', [
          StringOfChar(' ', DS.FieldByName('depth').AsInteger * 3),
          DS.FieldByName('filename').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Depth statistics
  DS := Conn.ExecuteQuery(
    'SELECT LENGTH(path) - LENGTH(REPLACE(path, ''/'', '''')) AS depth, ' +
    '  COUNT(*) AS cnt ' +
    'FROM files WHERE is_directory = 0 ' +
    'GROUP BY depth ORDER BY depth');
  try
    WriteLn('   Files by depth level:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - Depth %d: %d file(s)', [
        DS.FieldByName('depth').AsInteger,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates file checksum storage and integrity verification. }
procedure Demo3_Checksums;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Checksums and Integrity ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT filename, path, SUBSTR(checksum, 1, 16) || ''...'' AS checksum_short, size_bytes ' +
    'FROM files WHERE is_directory = 0 AND checksum IS NOT NULL ' +
    'ORDER BY path LIMIT 10');
  try
    WriteLn(Format('   %-20s %-20s %10s  %s',
      ['File', 'Checksum (short)', 'Size', 'Path']));
    WriteLn('   ' + StringOfChar('-', 85));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-20s %10d  %s', [
        DS.FieldByName('filename').AsString,
        DS.FieldByName('checksum_short').AsString,
        DS.FieldByName('size_bytes').AsInteger,
        DS.FieldByName('path').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Verify integrity: files with same checksum should have same size
  DS := Conn.ExecuteQuery(
    'SELECT checksum, COUNT(DISTINCT size_bytes) AS distinct_sizes ' +
    'FROM files WHERE checksum IS NOT NULL ' +
    'GROUP BY checksum HAVING COUNT(DISTINCT size_bytes) > 1');
  try
    if DS.EOF then
      WriteLn('   Integrity check: PASSED (all matching checksums have consistent sizes)')
    else
      WriteLn('   Integrity check: FAILED (size mismatch for same checksum)');
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates duplicate file detection by matching checksums. }
procedure Demo4_Deduplication;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Deduplication Detection ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT checksum, COUNT(*) AS copies, size_bytes, ' +
    '  GROUP_CONCAT(path, '' | '') AS locations ' +
    'FROM files ' +
    'WHERE is_directory = 0 AND checksum IS NOT NULL ' +
    'GROUP BY checksum ' +
    'HAVING COUNT(*) > 1 ' +
    'ORDER BY size_bytes DESC');
  try
    WriteLn('   Duplicate files (same checksum):');
    WriteLn;
    while not DS.EOF do
    begin
      WriteLn(Format('   Checksum: %s...', [Copy(DS.FieldByName('checksum').AsString, 1, 16)]));
      WriteLn(Format('     Size: %d bytes, Copies: %d', [
        DS.FieldByName('size_bytes').AsInteger,
        DS.FieldByName('copies').AsInteger]));
      WriteLn(Format('     Locations: %s', [DS.FieldByName('locations').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Wasted space calculation
  DS := Conn.ExecuteQuery(
    'SELECT SUM(wasted) AS total_wasted, COUNT(*) AS dup_groups FROM (' +
    '  SELECT (COUNT(*) - 1) * size_bytes AS wasted ' +
    '  FROM files WHERE is_directory = 0 AND checksum IS NOT NULL ' +
    '  GROUP BY checksum HAVING COUNT(*) > 1)');
  try
    if not DS.EOF then
      WriteLn(Format('   Wasted space: %d bytes across %d duplicate group(s)', [
        DS.FieldByName('total_wasted').AsInteger,
        DS.FieldByName('dup_groups').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists operation history with file counts, bytes, and duration, then simulates a re-index of recently modified files. }
procedure Demo5_BulkOperations;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Bulk Operations ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT operation, file_count, total_bytes, status, started_at, completed_at, ' +
    '  CAST((julianday(completed_at) - julianday(started_at)) * 86400 AS INTEGER) AS duration_sec ' +
    'FROM operations ORDER BY started_at');
  try
    WriteLn(Format('   %-18s %-6s %10s  %-10s %-4s %s',
      ['Operation', 'Files', 'Bytes', 'Status', 'Sec', 'Started']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-6d %10d  %-10s %-4d %s', [
        DS.FieldByName('operation').AsString,
        DS.FieldByName('file_count').AsInteger,
        DS.FieldByName('total_bytes').AsInteger,
        DS.FieldByName('status').AsString,
        DS.FieldByName('duration_sec').AsInteger,
        DS.FieldByName('started_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Simulate a new bulk operation: re-index modified files
  WriteLn('   Simulating: re-index files modified since 2025-01-17...');
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) AS cnt, SUM(size_bytes) AS total FROM files ' +
    'WHERE is_directory = 0 AND modified_at >= ''2025-01-17 00:00:00''');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Found %d files (%d bytes) to re-index', [
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('total').AsInteger]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO operations (operation, file_count, total_bytes, status, started_at, completed_at) VALUES ' +
        '(''reindex_modified'', %d, %d, ''completed'', ''2025-01-20 12:05:00'', ''2025-01-20 12:05:02'')',
        [DS.FieldByName('cnt').AsInteger, DS.FieldByName('total').AsInteger]));
    end;
  finally
    DS.Free;
  end;
  WriteLn('   Bulk re-index completed');
  WriteLn;
end;

{ Groups files by extension with count and total size, then lists all Pascal source files. }
procedure Demo6_SearchByExtension;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Search by Extension ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT extension, COUNT(*) AS cnt, SUM(size_bytes) AS total_size ' +
    'FROM files WHERE is_directory = 0 AND extension IS NOT NULL ' +
    'GROUP BY extension ORDER BY cnt DESC');
  try
    WriteLn(Format('   %-8s %-6s %s', ['Extension', 'Count', 'Total Size']));
    WriteLn('   ' + StringOfChar('-', 35));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-6d %d bytes', [
        DS.FieldByName('extension').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('total_size').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Search for Pascal files
  WriteLn('   Pascal source files (*.pas):');
  DS := Conn.ExecuteQuery(
    'SELECT path, size_bytes, modified_at FROM files ' +
    'WHERE extension = ''pas'' ORDER BY path');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%d bytes, mod %s)', [
        DS.FieldByName('path').AsString,
        DS.FieldByName('size_bytes').AsInteger,
        DS.FieldByName('modified_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Queries files by author from metadata, aggregates lines of code per author, and shows all metadata for a specific file. }
procedure Demo7_SearchByMetadata;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Search by Metadata ===');
  WriteLn;

  // Files by author
  WriteLn('   Files authored by "alice":');
  DS := Conn.ExecuteQuery(
    'SELECT f.path, f.size_bytes ' +
    'FROM files f JOIN file_metadata m ON m.file_id = f.id ' +
    'WHERE m.meta_key = ''author'' AND m.meta_value = ''alice'' ' +
    'ORDER BY f.path');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%d bytes)', [
        DS.FieldByName('path').AsString,
        DS.FieldByName('size_bytes').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Total lines of code
  WriteLn('   Lines of code by author:');
  DS := Conn.ExecuteQuery(
    'SELECT auth.meta_value AS author, ' +
    '  SUM(CAST(loc.meta_value AS INTEGER)) AS total_loc, ' +
    '  COUNT(*) AS file_count ' +
    'FROM file_metadata auth ' +
    'JOIN file_metadata loc ON loc.file_id = auth.file_id AND loc.meta_key = ''loc'' ' +
    'WHERE auth.meta_key = ''author'' ' +
    'GROUP BY auth.meta_value ORDER BY total_loc DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d lines across %d file(s)', [
        DS.FieldByName('author').AsString,
        DS.FieldByName('total_loc').AsInteger,
        DS.FieldByName('file_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // All metadata for a specific file
  WriteLn('   Metadata for /projects/webapp/src/main.pas:');
  DS := Conn.ExecuteQuery(
    'SELECT meta_key, meta_value FROM file_metadata WHERE file_id = 8 ORDER BY meta_key');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s = %s', [
        DS.FieldByName('meta_key').AsString,
        DS.FieldByName('meta_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists the top five largest files and calculates recursive directory sizes with file counts. }
procedure Demo8_SizeAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Size Analysis ===');
  WriteLn;

  // Largest files
  WriteLn('   Top 5 largest files:');
  DS := Conn.ExecuteQuery(
    'SELECT path, size_bytes FROM files ' +
    'WHERE is_directory = 0 ORDER BY size_bytes DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%d bytes)', [
        DS.FieldByName('path').AsString,
        DS.FieldByName('size_bytes').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Directory sizes (sum of contained files)
  WriteLn('   Directory sizes (recursive):');
  DS := Conn.ExecuteQuery(
    'SELECT d.path, ' +
    '  (SELECT SUM(f.size_bytes) FROM files f ' +
    '   WHERE f.is_directory = 0 AND f.path LIKE d.path || ''/%'') AS dir_size, ' +
    '  (SELECT COUNT(*) FROM files f ' +
    '   WHERE f.is_directory = 0 AND f.path LIKE d.path || ''/%'') AS file_count ' +
    'FROM files d WHERE d.is_directory = 1 ' +
    'ORDER BY dir_size DESC');
  try
    WriteLn(Format('   %-30s %10s  %s', ['Directory', 'Size', 'Files']));
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-30s %10d  %d', [
        DS.FieldByName('path').AsString,
        DS.FieldByName('dir_size').AsInteger,
        DS.FieldByName('file_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists files modified in the last five days and identifies change hotspots by directory. }
procedure Demo9_RecentChanges;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Recent Changes ===');
  WriteLn;

  WriteLn('   Files modified in the last 5 days (since 2025-01-16):');
  DS := Conn.ExecuteQuery(
    'SELECT path, filename, size_bytes, modified_at ' +
    'FROM files WHERE is_directory = 0 AND modified_at >= ''2025-01-16 00:00:00'' ' +
    'ORDER BY modified_at DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%d bytes, %s)', [
        DS.FieldByName('path').AsString,
        DS.FieldByName('size_bytes').AsInteger,
        DS.FieldByName('modified_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Change frequency by directory
  WriteLn('   Change hotspots (most recently modified directories):');
  DS := Conn.ExecuteQuery(
    'SELECT parent_path, COUNT(*) AS changed_files, ' +
    '  MAX(modified_at) AS last_change ' +
    'FROM files WHERE is_directory = 0 AND modified_at >= ''2025-01-15 00:00:00'' ' +
    'GROUP BY parent_path ORDER BY changed_files DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d file(s), last change %s', [
        DS.FieldByName('parent_path').AsString,
        DS.FieldByName('changed_files').AsInteger,
        DS.FieldByName('last_change').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Reports overall index statistics, MIME type distribution, and unique checksum counts for deduplication summary. }
procedure Demo10_IndexStats;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Index Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM files) AS total_entries, ' +
    '  (SELECT COUNT(*) FROM files WHERE is_directory = 1) AS directories, ' +
    '  (SELECT COUNT(*) FROM files WHERE is_directory = 0) AS files_count, ' +
    '  (SELECT SUM(size_bytes) FROM files WHERE is_directory = 0) AS total_size, ' +
    '  (SELECT COUNT(DISTINCT extension) FROM files WHERE extension IS NOT NULL) AS extensions, ' +
    '  (SELECT COUNT(*) FROM file_metadata) AS metadata_entries, ' +
    '  (SELECT COUNT(*) FROM operations) AS operations_count');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Total entries:    %d', [DS.FieldByName('total_entries').AsInteger]));
      WriteLn(Format('   Directories:      %d', [DS.FieldByName('directories').AsInteger]));
      WriteLn(Format('   Files:            %d', [DS.FieldByName('files_count').AsInteger]));
      WriteLn(Format('   Total size:       %d bytes', [DS.FieldByName('total_size').AsInteger]));
      WriteLn(Format('   File extensions:  %d', [DS.FieldByName('extensions').AsInteger]));
      WriteLn(Format('   Metadata entries: %d', [DS.FieldByName('metadata_entries').AsInteger]));
      WriteLn(Format('   Operations:       %d', [DS.FieldByName('operations_count').AsInteger]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // MIME type distribution
  DS := Conn.ExecuteQuery(
    'SELECT COALESCE(mime_type, ''(none)'') AS mime, COUNT(*) AS cnt ' +
    'FROM files WHERE is_directory = 0 GROUP BY mime_type ORDER BY cnt DESC');
  try
    WriteLn('   MIME type distribution:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d file(s)', [
        DS.FieldByName('mime').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Deduplication summary
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) AS unique_files FROM (SELECT DISTINCT checksum FROM files WHERE checksum IS NOT NULL)');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Unique checksums: %d (out of 14 files with checksums)', [
        DS.FieldByName('unique_files').AsInteger]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

begin
  WriteLn('Example 145: File Metadata Store');
  WriteLn('======================================================================');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_FileIndex;
    Demo2_PathHierarchy;
    Demo3_Checksums;
    Demo4_Deduplication;
    Demo5_BulkOperations;
    Demo6_SearchByExtension;
    Demo7_SearchByMetadata;
    Demo8_SizeAnalysis;
    Demo9_RecentChanges;
    Demo10_IndexStats;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
