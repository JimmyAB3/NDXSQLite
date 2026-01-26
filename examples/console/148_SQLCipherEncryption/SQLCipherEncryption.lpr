{===============================================================================
  NDXSQLite Example 148 - SQLCipher Full Database Encryption

  Copyright (c) 2026 NDXDev. MIT License.

  OVERVIEW
  --------
  This example demonstrates transparent database encryption using SQLCipher,
  a full-database encryption extension for SQLite. SQLCipher provides
  AES-256 encryption with authenticated encryption modes, ensuring both
  confidentiality and integrity of stored data.

  FEATURES DEMONSTRATED
  ---------------------
  - Runtime detection of SQLCipher availability
  - Creating encrypted databases with passphrase-based keys
  - Opening encrypted databases with proper key authentication
  - Key rotation (rekey) for security maintenance
  - Error handling for authentication failures
  - Graceful fallback when SQLCipher is unavailable

  REQUIREMENTS
  ------------
  SQLCipher library must be installed and accessible at runtime:

    Linux (Debian/Ubuntu):
      sudo apt install libsqlcipher1 libsqlcipher-dev

    Linux (Fedora/RHEL):
      sudo dnf install sqlcipher sqlcipher-devel

    macOS (Homebrew):
      brew install sqlcipher

    Windows:
      Download prebuilt binaries from https://www.zetetic.net/sqlcipher/
      Place sqlcipher.dll in your application directory

  If SQLCipher is not installed, the application will detect standard SQLite
  and skip encryption-specific demonstrations.

  SECURITY CONSIDERATIONS
  -----------------------
  - Never hardcode encryption keys in production code
  - Use secure key derivation (SQLCipher uses PBKDF2 by default)
  - Implement proper key management (environment variables, key vaults)
  - Rotate encryption keys periodically
  - Ensure secure memory handling for sensitive data

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program SQLCipherEncryption;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions, ndxsqliteplatform;

const
  { Demo encryption keys - in production, retrieve from secure storage }
  ENCRYPTION_KEY = 'MySecretPassphrase2024!@#$';
  NEW_ENCRYPTION_KEY = 'AnotherSecretKey987!';

var
  { Database file paths }
  PlainDatabasePath: string;
  EncryptedDatabasePath: string;

{------------------------------------------------------------------------------
  Utility: Print section separator
------------------------------------------------------------------------------}
procedure PrintSection(const Title: string);
begin
  WriteLn;
  WriteLn('================================================');
  WriteLn(Title);
  WriteLn('================================================');
  WriteLn;
end;

{------------------------------------------------------------------------------
  Utility: Clean up test database files

  Removes database files and associated WAL/SHM journal files created
  during the demonstration.
------------------------------------------------------------------------------}
procedure CleanupDatabaseFiles;

  procedure DeleteDatabaseWithJournals(const Path: string);
  begin
    if FileExists(Path) then DeleteFile(Path);
    if FileExists(Path + '-wal') then DeleteFile(Path + '-wal');
    if FileExists(Path + '-shm') then DeleteFile(Path + '-shm');
  end;

begin
  DeleteDatabaseWithJournals(PlainDatabasePath);
  DeleteDatabaseWithJournals(EncryptedDatabasePath);
end;

{------------------------------------------------------------------------------
  Demo 1: SQLCipher Availability Check

  Verifies whether SQLCipher is loaded instead of standard SQLite.
  This check should be performed before attempting any encryption operations.

  Returns: True if SQLCipher is available, False otherwise
------------------------------------------------------------------------------}
function CheckSQLCipherAvailability: Boolean;
begin
  PrintSection('1. SQLCipher Availability Check');

  Result := TNDXSQLiteConnection.IsSQLCipherAvailable;

  if Result then
  begin
    WriteLn('  [OK] SQLCipher is available on this system.');
    WriteLn;
    WriteLn('  SQLCipher encryption features:');
    WriteLn('    - AES-256 encryption in XTS mode');
    WriteLn('    - PBKDF2 key derivation (256,000 iterations)');
    WriteLn('    - HMAC-SHA512 page authentication');
    WriteLn('    - Transparent read/write encryption');
  end
  else
  begin
    WriteLn('  [WARNING] SQLCipher is not available.');
    WriteLn('  Standard SQLite library detected.');
    WriteLn;
    WriteLn('  To enable encryption, install SQLCipher:');
    WriteLn('    Linux:   sudo apt install libsqlcipher1');
    WriteLn('    macOS:   brew install sqlcipher');
    WriteLn('    Windows: Download from zetetic.net/sqlcipher');
    WriteLn;
    WriteLn('  Encryption demonstrations will be skipped.');
  end;
end;

{------------------------------------------------------------------------------
  Demo 2: Create Encrypted Database

  Creates a new encrypted database and populates it with sample sensitive data.
  The encryption key is set via TNDXSQLiteConnectionOptions.EncryptionKey
  before opening the connection.
------------------------------------------------------------------------------}
procedure DemoCreateEncryptedDatabase;
var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  PrintSection('2. Creating an Encrypted Database');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := EncryptedDatabasePath;
    Options.EncryptionKey := ENCRYPTION_KEY;
    Options.IsPrimaryConnection := True;

    WriteLn('  Database: ', EncryptedDatabasePath);
    WriteLn('  Key:      ', Copy(ENCRYPTION_KEY, 1, 5), '*** (truncated)');
    WriteLn;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      WriteLn('  Opening encrypted database...');
      Connection.Open;

      if Connection.IsEncrypted then
        WriteLn('  Encryption status: ACTIVE')
      else
        WriteLn('  Encryption status: INACTIVE (standard SQLite)');
      WriteLn;

      { Create schema for sensitive data storage }
      WriteLn('  Creating tables...');
      Connection.ExecuteNonQuery(
        'CREATE TABLE IF NOT EXISTS sensitive_data (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  ssn TEXT NOT NULL,' +
        '  credit_card TEXT NOT NULL,' +
        '  bank_account TEXT NOT NULL,' +
        '  created_at TEXT DEFAULT (datetime(''now''))' +
        ')');

      Connection.ExecuteNonQuery(
        'CREATE TABLE IF NOT EXISTS user_secrets (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  username TEXT UNIQUE NOT NULL,' +
        '  api_key TEXT,' +
        '  private_key TEXT,' +
        '  notes TEXT' +
        ')');

      { Insert sample sensitive records }
      WriteLn('  Inserting sample data...');
      Connection.ExecuteNonQuery(
        'INSERT INTO sensitive_data (ssn, credit_card, bank_account) VALUES (?, ?, ?)',
        ['123-45-6789', '4111-1111-1111-1111', 'US12345678901234']);
      Connection.ExecuteNonQuery(
        'INSERT INTO sensitive_data (ssn, credit_card, bank_account) VALUES (?, ?, ?)',
        ['987-65-4321', '5500-0000-0000-0004', 'DE89370400440532013000']);
      Connection.ExecuteNonQuery(
        'INSERT INTO user_secrets (username, api_key, private_key, notes) VALUES (?, ?, ?, ?)',
        ['admin', 'sk-proj-xxxxxxxxxxxx', '-----BEGIN RSA PRIVATE KEY-----...', 'Production API']);

      WriteLn('  Verifying: ', Connection.ExecuteScalar('SELECT COUNT(*) FROM sensitive_data'),
              ' records in sensitive_data');
      WriteLn('  Verifying: ', Connection.ExecuteScalar('SELECT COUNT(*) FROM user_secrets'),
              ' records in user_secrets');
      WriteLn;

      Connection.Close;
      WriteLn('  Database closed. File is now encrypted on disk.');
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;
end;

{------------------------------------------------------------------------------
  Demo 3: Open Encrypted Database with Correct Key

  Demonstrates successful authentication and data retrieval when the correct
  encryption key is provided.
------------------------------------------------------------------------------}
procedure DemoOpenWithCorrectKey;
var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DataSet: TDataSet;
begin
  PrintSection('3. Opening with Correct Key');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := EncryptedDatabasePath;
    Options.EncryptionKey := ENCRYPTION_KEY;
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      WriteLn('  Authenticating with correct key...');
      Connection.Open;
      WriteLn('  [OK] Authentication successful.');
      WriteLn;

      WriteLn('  Reading decrypted data:');
      DataSet := Connection.ExecuteQuery('SELECT ssn, credit_card FROM sensitive_data');
      try
        while not DataSet.EOF do
        begin
          WriteLn('    SSN: ', DataSet.FieldByName('ssn').AsString,
                  '  |  Card: ', DataSet.FieldByName('credit_card').AsString);
          DataSet.Next;
        end;
      finally
        DataSet.Free;
      end;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;
end;

{------------------------------------------------------------------------------
  Demo 4: Open Encrypted Database with Wrong Key

  Demonstrates authentication failure when an incorrect encryption key is
  provided. SQLCipher will reject the connection or return corrupted data.
------------------------------------------------------------------------------}
procedure DemoOpenWithWrongKey;
var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  PrintSection('4. Opening with WRONG Key');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := EncryptedDatabasePath;
    Options.EncryptionKey := 'IncorrectPassword123';
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      WriteLn('  Attempting authentication with wrong key...');
      try
        Connection.Open;
        Connection.ExecuteScalar('SELECT COUNT(*) FROM sensitive_data');
        WriteLn('  [WARNING] Query succeeded - encryption may not be active.');
      except
        on E: Exception do
        begin
          WriteLn('  [EXPECTED] Authentication failed: ', E.Message);
          WriteLn;
          WriteLn('  Data remains protected from unauthorized access.');
        end;
      end;

      if Connection.IsOpen then
        Connection.Close;
    except
      on E: Exception do
      begin
        WriteLn('  [EXPECTED] Connection rejected: ', E.Message);
        WriteLn;
        WriteLn('  SQLCipher correctly denied access with invalid credentials.');
      end;
    end;
    Connection.Free;
  finally
    Options.Free;
  end;
end;

{------------------------------------------------------------------------------
  Demo 5: Open Encrypted Database Without Key

  Demonstrates that encrypted databases cannot be accessed without providing
  the encryption key. The database file appears as corrupted data.
------------------------------------------------------------------------------}
procedure DemoOpenWithoutKey;
var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  PrintSection('5. Opening WITHOUT Key');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := EncryptedDatabasePath;
    Options.EncryptionKey := '';  { No key provided }
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      WriteLn('  Attempting to open without encryption key...');
      try
        Connection.Open;
        Connection.ExecuteScalar('SELECT COUNT(*) FROM sensitive_data');
        WriteLn('  [WARNING] Query succeeded - database may not be encrypted.');
      except
        on E: Exception do
        begin
          WriteLn('  [EXPECTED] Access denied: ', E.Message);
          WriteLn;
          WriteLn('  Encrypted file appears as corrupt data without the key.');
        end;
      end;

      if Connection.IsOpen then
        Connection.Close;
    except
      on E: Exception do
      begin
        WriteLn('  [EXPECTED] Connection failed: ', E.Message);
        WriteLn;
        WriteLn('  Database cannot be opened without encryption key.');
      end;
    end;
    Connection.Free;
  finally
    Options.Free;
  end;
end;

{------------------------------------------------------------------------------
  Demo 6: Key Rotation (Rekey)

  Demonstrates changing the encryption key on an existing encrypted database.
  This is essential for security maintenance and compliance requirements.

  The rekey operation:
  1. Opens database with current key
  2. Re-encrypts all pages with new key
  3. Updates key derivation parameters
------------------------------------------------------------------------------}
procedure DemoChangeEncryptionKey;
var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DataSet: TDataSet;
begin
  PrintSection('6. Key Rotation (Rekey)');

  { Step 1: Open with current key and perform rekey }
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := EncryptedDatabasePath;
    Options.EncryptionKey := ENCRYPTION_KEY;
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      WriteLn('  Opening with current key...');
      Connection.Open;
      WriteLn;

      WriteLn('  Performing key rotation:');
      WriteLn('    Old key: ', Copy(ENCRYPTION_KEY, 1, 5), '***');
      WriteLn('    New key: ', Copy(NEW_ENCRYPTION_KEY, 1, 5), '***');
      WriteLn;

      Connection.ChangeEncryptionKey(NEW_ENCRYPTION_KEY);
      WriteLn('  [OK] Key rotation completed.');
      WriteLn;

      { Verify data integrity after rekey }
      WriteLn('  Verifying data integrity...');
      DataSet := Connection.ExecuteQuery('SELECT COUNT(*) as cnt FROM sensitive_data');
      try
        WriteLn('  Records intact: ', DataSet.FieldByName('cnt').AsInteger);
      finally
        DataSet.Free;
      end;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  { Step 2: Verify new key works }
  WriteLn;
  WriteLn('  Verifying new key authentication...');
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := EncryptedDatabasePath;
    Options.EncryptionKey := NEW_ENCRYPTION_KEY;
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;
      DataSet := Connection.ExecuteQuery('SELECT ssn FROM sensitive_data LIMIT 1');
      try
        WriteLn('  [OK] New key works. Sample data: SSN=', DataSet.FieldByName('ssn').AsString);
      finally
        DataSet.Free;
      end;
      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  { Step 3: Verify old key no longer works }
  WriteLn;
  WriteLn('  Verifying old key is invalidated...');
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := EncryptedDatabasePath;
    Options.EncryptionKey := ENCRYPTION_KEY;
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      try
        Connection.Open;
        Connection.ExecuteScalar('SELECT COUNT(*) FROM sensitive_data');
        WriteLn('  [WARNING] Old key still works - unexpected behavior.');
      except
        WriteLn('  [OK] Old key rejected. Key rotation successful.');
      end;
      if Connection.IsOpen then
        Connection.Close;
    except
      WriteLn('  [OK] Old key rejected. Key rotation successful.');
    end;
    Connection.Free;
  finally
    Options.Free;
  end;
end;

{------------------------------------------------------------------------------
  Demo 7: Unencrypted Database Comparison

  Creates an unencrypted database for comparison purposes. This demonstrates
  that NDXSQLite works normally without encryption when no key is specified.
------------------------------------------------------------------------------}
procedure DemoUnencryptedDatabase;
var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
begin
  PrintSection('7. Unencrypted Database (Comparison)');

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := PlainDatabasePath;
    Options.EncryptionKey := '';  { No encryption }
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      WriteLn('  Creating standard SQLite database...');
      Connection.Open;

      Connection.ExecuteNonQuery(
        'CREATE TABLE plain_data (id INTEGER PRIMARY KEY, value TEXT)');
      Connection.ExecuteNonQuery(
        'INSERT INTO plain_data (value) VALUES (?)',
        ['This text is stored in plain format']);

      WriteLn('  Database created: ', PlainDatabasePath);
      WriteLn;
      WriteLn('  This file can be opened with any SQLite viewer.');
      WriteLn('  The encrypted database requires the correct key.');

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;
end;

{------------------------------------------------------------------------------
  Demo 8: Security Best Practices

  Displays recommended security practices for production SQLCipher deployments.
------------------------------------------------------------------------------}
procedure DemoSecurityBestPractices;
begin
  PrintSection('8. Security Best Practices');

  WriteLn('  KEY MANAGEMENT');
  WriteLn('  --------------');
  WriteLn('  - Never hardcode keys in source code or configuration files');
  WriteLn('  - Use environment variables or secure key management services');
  WriteLn('  - Consider AWS KMS, Azure Key Vault, or HashiCorp Vault');
  WriteLn('  - Implement key rotation policies (quarterly recommended)');
  WriteLn;

  WriteLn('  KEY DERIVATION');
  WriteLn('  --------------');
  WriteLn('  - SQLCipher uses PBKDF2-HMAC-SHA512 with 256,000 iterations');
  WriteLn('  - Use strong passphrases: 16+ characters with mixed content');
  WriteLn('  - For maximum security, provide raw 256-bit keys directly');
  WriteLn('  - Increase iterations for higher security requirements');
  WriteLn;

  WriteLn('  CONFIGURATION OPTIONS');
  WriteLn('  ---------------------');
  WriteLn('  - PRAGMA cipher_page_size = 4096 (default, increase for bulk I/O)');
  WriteLn('  - PRAGMA kdf_iter = 256000 (increase for enhanced security)');
  WriteLn('  - PRAGMA cipher_memory_security = ON (secure memory wiping)');
  WriteLn('  - PRAGMA cipher_plaintext_header_size = 0 (no plaintext header)');
  WriteLn;

  WriteLn('  PERFORMANCE CONSIDERATIONS');
  WriteLn('  --------------------------');
  WriteLn('  - Encryption overhead: approximately 5-15% depending on workload');
  WriteLn('  - Larger page sizes improve throughput for bulk operations');
  WriteLn('  - Raw key mode bypasses PBKDF2 for faster opens (if key is secure)');
  WriteLn('  - Use connection pooling to minimize key derivation overhead');
  WriteLn;

  WriteLn('  BACKUP AND RECOVERY');
  WriteLn('  -------------------');
  WriteLn('  - Encrypted backups remain encrypted with the same key');
  WriteLn('  - Document key recovery procedures and test regularly');
  WriteLn('  - Consider key escrow for disaster recovery scenarios');
  WriteLn('  - Verify backup integrity by test restoration');
end;

{------------------------------------------------------------------------------
  Main Program Entry Point
------------------------------------------------------------------------------}
var
  SQLCipherAvailable: Boolean;
begin
  WriteLn;
  WriteLn('================================================');
  WriteLn('NDXSQLite Example 148: SQLCipher Database Encryption');
  WriteLn('================================================');

  { Initialize database paths relative to executable location }
  PlainDatabasePath := ExtractFilePath(ParamStr(0)) + 'example148_plain.db';
  EncryptedDatabasePath := ExtractFilePath(ParamStr(0)) + 'example148_encrypted.db';

  { Clean up any files from previous runs }
  CleanupDatabaseFiles;

  try
    { Check SQLCipher availability before proceeding }
    SQLCipherAvailable := CheckSQLCipherAvailability;

    if SQLCipherAvailable then
    begin
      { Run full encryption demonstration suite }
      DemoCreateEncryptedDatabase;
      DemoOpenWithCorrectKey;
      DemoOpenWithWrongKey;
      DemoOpenWithoutKey;
      DemoChangeEncryptionKey;
      DemoUnencryptedDatabase;
      DemoSecurityBestPractices;
    end
    else
    begin
      { Fallback mode: demonstrate non-encryption features only }
      WriteLn;
      WriteLn('  Running in fallback mode (no encryption)...');
      DemoUnencryptedDatabase;
      DemoSecurityBestPractices;
    end;

    PrintSection('Example Completed Successfully');
  finally
    { Optional: uncomment to auto-cleanup test files }
    { CleanupDatabaseFiles; }
  end;
end.
