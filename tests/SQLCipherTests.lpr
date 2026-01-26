program SQLCipherTests;

{===============================================================================
  NDXSQLite - SQLCipher Encryption Tests
  Tests for database encryption using SQLCipher

  IMPORTANT: These tests require SQLCipher library instead of standard SQLite.
  If standard SQLite is loaded, tests will be skipped with a warning.

  Author: Nicolas DEOUX - NDXDev 2026
===============================================================================}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqlite3api,
  ndxsqlitetypes,
  ndxsqliteexceptions,
  ndxsqliteconnectionintf,
  ndxsqliteconnection,
  ndxsqliteconnectionoptions,
  ndxsqliteplatform;

var
  TEST_DB: string;
  TEST_DB_ENCRYPTED: string;
  TEST_DIR: string;
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  TestsSkipped: Integer = 0;
  SQLCipherAvailable: Boolean = False;

const
  TEST_KEY = 'MySecretPassword123!';
  TEST_KEY_2 = 'AnotherPassword456@';

{ Initialize test paths according to platform }
procedure InitTestPaths;
begin
  TEST_DIR := TNDXPlatform.GetTempDirectory + 'ndxsqlite_cipher_tests' + PathDelim;
  TEST_DB := TEST_DIR + 'test_cipher.db';
  TEST_DB_ENCRYPTED := TEST_DIR + 'test_encrypted.db';
  TNDXPlatform.EnsureDirectoryExists(TEST_DIR);
  WriteLn('Platform: ', TNDXPlatform.PlatformName);
  WriteLn('Test directory: ', TEST_DIR);
end;

procedure LogSuccess(const ATestName: string);
begin
  WriteLn('[OK] ', ATestName);
  Inc(TestsPassed);
end;

procedure LogFailure(const ATestName, AError: string);
begin
  WriteLn('[FAIL] ', ATestName, ' - ', AError);
  Inc(TestsFailed);
end;

procedure LogSkipped(const ATestName, AReason: string);
begin
  WriteLn('[SKIP] ', ATestName, ' - ', AReason);
  Inc(TestsSkipped);
end;

procedure CleanupTestDB;
var
  SR: TSearchRec;
begin
  if DirectoryExists(TEST_DIR) then
  begin
    if FindFirst(TEST_DIR + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
          DeleteFile(TEST_DIR + SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;
end;

procedure DeleteTestFile(const APath: string);
begin
  if FileExists(APath) then
    DeleteFile(APath);
  if FileExists(APath + '-wal') then
    DeleteFile(APath + '-wal');
  if FileExists(APath + '-shm') then
    DeleteFile(APath + '-shm');
end;

{ ============================================================================ }
{ SQLCipher Availability Check }
{ ============================================================================ }

procedure CheckSQLCipherAvailability;
begin
  WriteLn('');
  WriteLn('=== Checking SQLCipher Availability ===');

  SQLCipherAvailable := TNDXSQLiteConnection.IsSQLCipherAvailable;

  if SQLCipherAvailable then
    WriteLn('SQLCipher is AVAILABLE - encryption tests will run')
  else
  begin
    WriteLn('SQLCipher is NOT AVAILABLE - standard SQLite detected');
    WriteLn('To run encryption tests, replace sqlite3 library with SQLCipher');
    WriteLn('Download from: https://www.zetetic.net/sqlcipher/');
  end;
  WriteLn('');
end;

{ ============================================================================ }
{ Encryption Tests (require SQLCipher) }
{ ============================================================================ }

procedure Test_CreateEncryptedDatabase;
var
  Conn: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  if not SQLCipherAvailable then
  begin
    LogSkipped('CreateEncryptedDatabase', 'SQLCipher not available');
    Exit;
  end;

  DeleteTestFile(TEST_DB_ENCRYPTED);

  try
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := TEST_KEY;
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        // Create table and insert data
        Conn.ExecuteNonQuery('CREATE TABLE test_data (id INTEGER PRIMARY KEY, name TEXT, value REAL)');
        Conn.ExecuteNonQuery('INSERT INTO test_data (name, value) VALUES (?, ?)', ['Secret Data', 123.45]);
        Conn.ExecuteNonQuery('INSERT INTO test_data (name, value) VALUES (?, ?)', ['Confidential', 678.90]);

        // Verify data was inserted
        if Conn.ExecuteScalar('SELECT COUNT(*) FROM test_data') = 2 then
          LogSuccess('CreateEncryptedDatabase')
        else
          LogFailure('CreateEncryptedDatabase', 'Data count mismatch');

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;
  except
    on E: Exception do
      LogFailure('CreateEncryptedDatabase', E.Message);
  end;
end;

procedure Test_OpenEncryptedWithCorrectKey;
var
  Conn: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DataCount: Variant;
begin
  if not SQLCipherAvailable then
  begin
    LogSkipped('OpenEncryptedWithCorrectKey', 'SQLCipher not available');
    Exit;
  end;

  try
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := TEST_KEY;
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        // Should be able to read data with correct key
        DataCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM test_data');
        if DataCount = 2 then
          LogSuccess('OpenEncryptedWithCorrectKey')
        else
          LogFailure('OpenEncryptedWithCorrectKey', Format('Expected 2 rows, got %s', [VarToStr(DataCount)]));

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;
  except
    on E: Exception do
      LogFailure('OpenEncryptedWithCorrectKey', E.Message);
  end;
end;

procedure Test_OpenEncryptedWithWrongKey;
var
  Conn: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  if not SQLCipherAvailable then
  begin
    LogSkipped('OpenEncryptedWithWrongKey', 'SQLCipher not available');
    Exit;
  end;

  try
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := 'WrongPassword';
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        // Try to read data - should fail with wrong key
        try
          Conn.ExecuteScalar('SELECT COUNT(*) FROM test_data');
          LogFailure('OpenEncryptedWithWrongKey', 'Should have failed with wrong key');
        except
          on E: Exception do
            // Expected - database should not be readable with wrong key
            LogSuccess('OpenEncryptedWithWrongKey');
        end;

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;
  except
    on E: Exception do
      // Connection might fail entirely with wrong key - this is also acceptable
      LogSuccess('OpenEncryptedWithWrongKey');
  end;
end;

procedure Test_OpenEncryptedWithNoKey;
var
  Conn: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  if not SQLCipherAvailable then
  begin
    LogSkipped('OpenEncryptedWithNoKey', 'SQLCipher not available');
    Exit;
  end;

  try
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := '';  // No key
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        // Try to read data - should fail without key
        try
          Conn.ExecuteScalar('SELECT COUNT(*) FROM test_data');
          LogFailure('OpenEncryptedWithNoKey', 'Should have failed without key');
        except
          on E: Exception do
            LogSuccess('OpenEncryptedWithNoKey');
        end;

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;
  except
    on E: Exception do
      LogSuccess('OpenEncryptedWithNoKey');
  end;
end;

procedure Test_ChangeEncryptionKey;
var
  Conn: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DataCount: Variant;
begin
  if not SQLCipherAvailable then
  begin
    LogSkipped('ChangeEncryptionKey', 'SQLCipher not available');
    Exit;
  end;

  try
    // Open with original key and change to new key
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := TEST_KEY;
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        // Change encryption key
        Conn.ChangeEncryptionKey(TEST_KEY_2);

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;

    // Verify old key no longer works
    // Note: With SQLCipher, using wrong key may fail at Open or at first query
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := TEST_KEY;  // Old key
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        try
          Conn.Open;
          Conn.ExecuteScalar('SELECT COUNT(*) FROM test_data');
          // If we got here, the old key worked - that's a failure
          LogFailure('ChangeEncryptionKey', 'Old key should not work');
          Conn.Close;
          Exit;
        except
          // Expected - old key should fail either at Open or at query
        end;
        if Conn.IsOpen then
          Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;

    // Verify new key works
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := TEST_KEY_2;  // New key
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;
        DataCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM test_data');
        if DataCount = 2 then
          LogSuccess('ChangeEncryptionKey')
        else
          LogFailure('ChangeEncryptionKey', 'Data lost after rekey');
        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;

  except
    on E: Exception do
      LogFailure('ChangeEncryptionKey', E.Message);
  end;
end;

procedure Test_IsEncrypted;
var
  Conn: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  if not SQLCipherAvailable then
  begin
    LogSkipped('IsEncrypted', 'SQLCipher not available');
    Exit;
  end;

  try
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB_ENCRYPTED;
      Options.EncryptionKey := TEST_KEY_2;
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        if Conn.IsEncrypted then
          LogSuccess('IsEncrypted')
        else
          LogFailure('IsEncrypted', 'Should report as encrypted');

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;
  except
    on E: Exception do
      LogFailure('IsEncrypted', E.Message);
  end;
end;

procedure Test_UnencryptedDatabase;
var
  Conn: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  DeleteTestFile(TEST_DB);

  try
    // Create unencrypted database
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB;
      Options.EncryptionKey := '';  // No encryption
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        Conn.ExecuteNonQuery('CREATE TABLE plain_data (id INTEGER PRIMARY KEY, info TEXT)');
        Conn.ExecuteNonQuery('INSERT INTO plain_data (info) VALUES (?)', ['Unencrypted']);

        // IsEncrypted might still return True if SQLCipher is loaded
        // but the data should be readable without key

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;

    // Verify database is readable without key
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.DatabasePath := TEST_DB;
      Options.EncryptionKey := '';
      Options.IsPrimaryConnection := True;

      Conn := TNDXSQLiteConnection.Create(Options);
      try
        Conn.Open;

        if Conn.ExecuteScalar('SELECT COUNT(*) FROM plain_data') = 1 then
          LogSuccess('UnencryptedDatabase')
        else
          LogFailure('UnencryptedDatabase', 'Cannot read unencrypted data');

        Conn.Close;
      finally
        Conn.Free;
      end;
    finally
      Options.Free;
    end;
  except
    on E: Exception do
      LogFailure('UnencryptedDatabase', E.Message);
  end;
end;

{ ============================================================================ }
{ Tests without SQLCipher (basic encryption option tests) }
{ ============================================================================ }

procedure Test_EncryptionKeyOption;
var
  Options: TNDXSQLiteConnectionOptions;
begin
  try
    Options := TNDXSQLiteConnectionOptions.Create;
    try
      Options.EncryptionKey := 'TestKey123';

      if Options.EncryptionKey = 'TestKey123' then
        LogSuccess('EncryptionKeyOption')
      else
        LogFailure('EncryptionKeyOption', 'Key not stored correctly');
    finally
      Options.Free;
    end;
  except
    on E: Exception do
      LogFailure('EncryptionKeyOption', E.Message);
  end;
end;

procedure Test_EncryptionKeyClone;
var
  Options1, Options2: TNDXSQLiteConnectionOptions;
begin
  try
    Options1 := TNDXSQLiteConnectionOptions.Create;
    try
      Options1.EncryptionKey := 'CloneTestKey';
      Options1.DatabasePath := '/test/path.db';

      Options2 := Options1.Clone;
      try
        if (Options2.EncryptionKey = 'CloneTestKey') and
           (Options2.DatabasePath = '/test/path.db') then
          LogSuccess('EncryptionKeyClone')
        else
          LogFailure('EncryptionKeyClone', 'Clone did not preserve encryption key');
      finally
        Options2.Free;
      end;
    finally
      Options1.Free;
    end;
  except
    on E: Exception do
      LogFailure('EncryptionKeyClone', E.Message);
  end;
end;

procedure Test_IsSQLCipherAvailableMethod;
var
  Available: Boolean;
begin
  try
    Available := TNDXSQLiteConnection.IsSQLCipherAvailable;
    // This test just verifies the method doesn't crash
    WriteLn('  SQLCipher available: ', Available);
    LogSuccess('IsSQLCipherAvailableMethod');
  except
    on E: Exception do
      LogFailure('IsSQLCipherAvailableMethod', E.Message);
  end;
end;

{ ============================================================================ }
{ Main }
{ ============================================================================ }

procedure RunAllTests;
begin
  WriteLn('');
  WriteLn('===============================================');
  WriteLn('NDXSQLite SQLCipher Encryption Tests');
  WriteLn('===============================================');
  WriteLn('');

  InitTestPaths;
  CleanupTestDB;
  CheckSQLCipherAvailability;

  WriteLn('=== Basic Option Tests (no SQLCipher required) ===');
  Test_EncryptionKeyOption;
  Test_EncryptionKeyClone;
  Test_IsSQLCipherAvailableMethod;
  WriteLn('');

  WriteLn('=== SQLCipher Encryption Tests ===');
  Test_CreateEncryptedDatabase;
  Test_OpenEncryptedWithCorrectKey;
  Test_OpenEncryptedWithWrongKey;
  Test_OpenEncryptedWithNoKey;
  Test_ChangeEncryptionKey;
  Test_IsEncrypted;
  Test_UnencryptedDatabase;
  WriteLn('');

  WriteLn('===============================================');
  WriteLn('Results: ', TestsPassed, ' passed, ', TestsFailed, ' failed, ', TestsSkipped, ' skipped');
  WriteLn('===============================================');

  // Cleanup
  CleanupTestDB;

  if TestsFailed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end;

begin
  RunAllTests;
end.
