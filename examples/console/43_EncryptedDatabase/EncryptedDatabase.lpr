{===============================================================================
  NDXSQLite Example 43 - Encrypted Database Patterns
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Database encryption concepts and patterns
  - Application-level field encryption strategies
  - Password hashing with salts
  - Key management and rotation patterns
  - Secure credential storage
  - Integration patterns with proper crypto libraries

  IMPORTANT SECURITY NOTES:
  ============================================================================
  1. The XOR encryption in this example is FOR DEMONSTRATION ONLY.
     It is NOT cryptographically secure and should NEVER be used in production.

  2. For FULL DATABASE ENCRYPTION, use SQLCipher:
     - Commercial: https://www.zetetic.net/sqlcipher/
     - Open Source: https://github.com/nicholasdeoux/ndxsqlcipher
     - Provides transparent AES-256 encryption

  3. For APPLICATION-LEVEL ENCRYPTION in Free Pascal, use:
     - DCPCrypt: https://github.com/Laex/Delphi-OpenSSL
     - LockBox: https://github.com/TurboPack/LockBox3
     - OpenSSL bindings: https://github.com/nicholasdeoux/ndxopenssl

  4. For PASSWORD HASHING, use:
     - bcrypt, scrypt, or Argon2 (NOT simple hashing!)
     - See: https://github.com/nicholasdeoux/ndxargon2
  ============================================================================

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program EncryptedDatabase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Base64, StrUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

// ============================================================================
// DEMO ENCRYPTION HELPERS (NOT FOR PRODUCTION!)
// ============================================================================
// These functions use XOR encryption which is trivially breakable.
// They exist only to demonstrate the PATTERNS of encryption integration.
// In production, replace with AES-256-GCM from a proper crypto library.
// ============================================================================

{ Encrypts data using XOR cipher for demonstration purposes only. }
function DemoEncrypt(const AData, AKey: string): string;
var
  I, KeyLen: Integer;
  Encrypted: string;
begin
  // WARNING: XOR encryption - NOT SECURE! Demo only!
  KeyLen := Length(AKey);
  if KeyLen = 0 then Exit(AData);

  SetLength(Encrypted, Length(AData));
  for I := 1 to Length(AData) do
    Encrypted[I] := Chr(Ord(AData[I]) xor Ord(AKey[((I - 1) mod KeyLen) + 1]));

  Result := EncodeStringBase64(Encrypted);
end;

{ Decrypts data using XOR cipher for demonstration purposes only. }
function DemoDecrypt(const AData, AKey: string): string;
var
  I, KeyLen: Integer;
  Decoded, Decrypted: string;
begin
  if AData = '' then Exit('');

  Decoded := DecodeStringBase64(AData);
  KeyLen := Length(AKey);
  if KeyLen = 0 then Exit(Decoded);

  SetLength(Decrypted, Length(Decoded));
  for I := 1 to Length(Decoded) do
    Decrypted[I] := Chr(Ord(Decoded[I]) xor Ord(AKey[((I - 1) mod KeyLen) + 1]));

  Result := Decrypted;
end;

{ Computes a DJB2-based hash of the password concatenated with the salt, returning an 8-character hex string (demo only, not secure). }
function DemoHashPassword(const APassword, ASalt: string): string;
var
  I: Integer;
  Hash: Cardinal;
  Combined: string;
begin
  // WARNING: Simple hash - NOT SECURE! Use bcrypt/scrypt/Argon2 in production!
  Combined := ASalt + APassword + ASalt;
  Hash := 5381;  // DJB2 hash starting value
  for I := 1 to Length(Combined) do
    Hash := ((Hash shl 5) + Hash) + Ord(Combined[I]);
  Result := IntToHex(Hash, 8);
end;

{ Generates a random alphanumeric salt string of the specified length for use in password hashing. }
function GenerateSalt(Length: Integer = 16): string;
var
  I: Integer;
const
  Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
begin
  SetLength(Result, Length);
  for I := 1 to Length do
    Result[I] := Chars[Random(System.Length(Chars)) + 1];
end;

// ============================================================================
// Database Schema Setup
// ============================================================================

{ Creates the encryption_meta, users (with encrypted email/SSN fields), api_credentials, and key_rotation_log tables. }
procedure SetupEncryptedSchema;
begin
  // Encryption metadata table - tracks key versions
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS encryption_meta (' +
    '  id INTEGER PRIMARY KEY,' +
    '  key_version INTEGER NOT NULL,' +
    '  algorithm TEXT NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  rotated_at TEXT,' +
    '  key_hash TEXT' +
    ')');

  // Users table with encrypted sensitive fields
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  password_hash TEXT NOT NULL,' +
    '  password_salt TEXT NOT NULL,' +
    '  email_encrypted TEXT,' +
    '  ssn_encrypted TEXT,' +
    '  encryption_key_version INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // API credentials storage
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS api_credentials (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  service_name TEXT NOT NULL,' +
    '  api_key_encrypted TEXT NOT NULL,' +
    '  encryption_key_version INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Key rotation log for audit
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS key_rotation_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  old_version INTEGER,' +
    '  new_version INTEGER,' +
    '  rotated_at TEXT DEFAULT (datetime(''now'')),' +
    '  records_updated INTEGER,' +
    '  status TEXT' +
    ')');
end;

// ============================================================================
// Demo 1: SQLCipher Concepts
// ============================================================================

{ Outputs an informational overview of SQLCipher PRAGMA commands for full database encryption, key derivation, and cipher configuration. }
procedure DemoSQLCipherConcepts;
begin
  WriteLn('1. SQLCipher - Full Database Encryption:');
  WriteLn('   ======================================');
  WriteLn('');
  WriteLn('   SQLCipher provides transparent, page-level AES-256 encryption.');
  WriteLn('   The entire database file is encrypted, including:');
  WriteLn('   - All tables and indexes');
  WriteLn('   - Schema information');
  WriteLn('   - WAL and journal files');
  WriteLn('');
  WriteLn('   Basic SQLCipher usage (when available):');
  WriteLn('   ----------------------------------------');
  WriteLn('   // Set key immediately after opening');
  WriteLn('   PRAGMA key = ''your-secret-passphrase'';');
  WriteLn('');
  WriteLn('   // Or use raw 256-bit key');
  WriteLn('   PRAGMA key = "x''2DD29CA851E7B56E4697B0E1F08507293D761A05CE4D1B628663F411A8086D99''";');
  WriteLn('');
  WriteLn('   // Change encryption key');
  WriteLn('   PRAGMA rekey = ''new-passphrase'';');
  WriteLn('');
  WriteLn('   // Configure key derivation (256,000 iterations recommended)');
  WriteLn('   PRAGMA kdf_iter = 256000;');
  WriteLn('');
  WriteLn('   // Set cipher algorithm');
  WriteLn('   PRAGMA cipher = ''aes-256-cbc'';');
  WriteLn('');
  WriteLn('   Benefits:');
  WriteLn('   - Transparent to application code');
  WriteLn('   - Strong AES-256 encryption');
  WriteLn('   - PBKDF2 key derivation');
  WriteLn('   - HMAC authentication');
  WriteLn('');
end;

// ============================================================================
// Demo 2: Proper Crypto Library Integration
// ============================================================================

{ Outputs code examples showing how to integrate DCPCrypt AES-256, OpenSSL AES-256-GCM, and Argon2 password hashing in Free Pascal. }
procedure DemoCryptoLibraryIntegration;
begin
  WriteLn('2. Proper Crypto Library Integration:');
  WriteLn('   ===================================');
  WriteLn('');
  WriteLn('   For production use, integrate a proper crypto library:');
  WriteLn('');
  WriteLn('   A) Using DCPCrypt (recommended for Free Pascal):');
  WriteLn('   ------------------------------------------------');
  WriteLn('   uses DCPrijndael, DCPsha256;');
  WriteLn('');
  WriteLn('   function EncryptAES256(const Data, Key: string): string;');
  WriteLn('   var');
  WriteLn('     Cipher: TDCP_rijndael;');
  WriteLn('   begin');
  WriteLn('     Cipher := TDCP_rijndael.Create(nil);');
  WriteLn('     try');
  WriteLn('       Cipher.InitStr(Key, TDCP_sha256);');
  WriteLn('       Result := Cipher.EncryptString(Data);');
  WriteLn('     finally');
  WriteLn('       Cipher.Free;');
  WriteLn('     end;');
  WriteLn('   end;');
  WriteLn('');
  WriteLn('   B) Using OpenSSL bindings:');
  WriteLn('   --------------------------');
  WriteLn('   uses OpenSSL;');
  WriteLn('');
  WriteLn('   // AES-256-GCM (authenticated encryption)');
  WriteLn('   EVP_aes_256_gcm()');
  WriteLn('   EVP_EncryptInit_ex(ctx, cipher, nil, key, iv);');
  WriteLn('   EVP_EncryptUpdate(ctx, ciphertext, len, plaintext, plen);');
  WriteLn('   EVP_EncryptFinal_ex(ctx, ciphertext + len, len);');
  WriteLn('');
  WriteLn('   C) Password hashing with Argon2:');
  WriteLn('   --------------------------------');
  WriteLn('   uses Argon2;');
  WriteLn('');
  WriteLn('   hash := TArgon2.HashPassword(password, salt,');
  WriteLn('     TArgon2Type.Argon2id, 65536, 3, 4);');
  WriteLn('');
end;

// ============================================================================
// Demo 3: Field-Level Encryption Pattern
// ============================================================================

var
  CurrentEncryptionKey: string;
  CurrentKeyVersion: Integer;

{ Sets the current encryption key and key version, inserting an initial encryption_meta record if none exists. }
procedure InitializeEncryption(const AKey: string);
var
  DS: TDataSet;
begin
  CurrentEncryptionKey := AKey;

  DS := Connection.ExecuteQuery('SELECT MAX(key_version) FROM encryption_meta');
  try
    if DS.Fields[0].IsNull then
    begin
      CurrentKeyVersion := 1;
      Connection.ExecuteNonQuery(
        'INSERT INTO encryption_meta (key_version, algorithm, key_hash) VALUES (?, ?, ?)',
        [CurrentKeyVersion, 'DEMO-XOR-NOT-SECURE', DemoHashPassword(AKey, 'KEYHASH')]);
    end
    else
      CurrentKeyVersion := DS.Fields[0].AsInteger;
  finally
    DS.Free;
  end;
end;

{ Inserts users with encrypted email and SSN fields, displays the stored ciphertext, then decrypts and displays the plaintext values. }
procedure DemoFieldEncryption;
var
  DS: TDataSet;
  Salt: string;
  EncryptedEmail, EncryptedSSN: string;
  DecryptedEmail, DecryptedSSN: string;
begin
  WriteLn('3. Field-Level Encryption Pattern:');
  WriteLn('   ================================');
  WriteLn('');
  WriteLn('   Encrypting sensitive fields at application level...');
  WriteLn('');

  // Create users with encrypted data
  Salt := GenerateSalt;
  EncryptedEmail := DemoEncrypt('alice@example.com', CurrentEncryptionKey);
  EncryptedSSN := DemoEncrypt('123-45-6789', CurrentEncryptionKey);

  Connection.ExecuteNonQuery(
    'INSERT INTO users (username, password_hash, password_salt, email_encrypted, ssn_encrypted, encryption_key_version) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    ['alice', DemoHashPassword('secret123', Salt), Salt, EncryptedEmail, EncryptedSSN, CurrentKeyVersion]);

  Salt := GenerateSalt;
  EncryptedEmail := DemoEncrypt('bob@company.org', CurrentEncryptionKey);
  EncryptedSSN := DemoEncrypt('987-65-4321', CurrentEncryptionKey);

  Connection.ExecuteNonQuery(
    'INSERT INTO users (username, password_hash, password_salt, email_encrypted, ssn_encrypted, encryption_key_version) ' +
    'VALUES (?, ?, ?, ?, ?, ?)',
    ['bob', DemoHashPassword('password456', Salt), Salt, EncryptedEmail, EncryptedSSN, CurrentKeyVersion]);

  // Show encrypted data in database
  WriteLn('   Data stored in database (encrypted):');
  DS := Connection.ExecuteQuery('SELECT username, email_encrypted, ssn_encrypted FROM users');
  try
    while not DS.EOF do
    begin
      WriteLn('   User: ', DS.FieldByName('username').AsString);
      WriteLn('     Email (enc): ', Copy(DS.FieldByName('email_encrypted').AsString, 1, 30), '...');
      WriteLn('     SSN (enc):   ', DS.FieldByName('ssn_encrypted').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Data decrypted with key:');
  DS := Connection.ExecuteQuery('SELECT username, email_encrypted, ssn_encrypted FROM users');
  try
    while not DS.EOF do
    begin
      DecryptedEmail := DemoDecrypt(DS.FieldByName('email_encrypted').AsString, CurrentEncryptionKey);
      DecryptedSSN := DemoDecrypt(DS.FieldByName('ssn_encrypted').AsString, CurrentEncryptionKey);
      WriteLn('   User: ', DS.FieldByName('username').AsString);
      WriteLn('     Email: ', DecryptedEmail);
      WriteLn('     SSN:   ', DecryptedSSN);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// ============================================================================
// Demo 4: Password Verification
// ============================================================================

{ Retrieves a user's stored hash and salt, then verifies a correct and an incorrect password by comparing computed hashes. }
procedure DemoPasswordVerification;
var
  DS: TDataSet;
  StoredHash, StoredSalt, ComputedHash: string;
begin
  WriteLn('4. Password Verification:');
  WriteLn('   =======================');
  WriteLn('');

  DS := Connection.ExecuteQuery(
    'SELECT password_hash, password_salt FROM users WHERE username = ?', ['alice']);
  try
    if not DS.EOF then
    begin
      StoredHash := DS.FieldByName('password_hash').AsString;
      StoredSalt := DS.FieldByName('password_salt').AsString;

      // Test correct password
      ComputedHash := DemoHashPassword('secret123', StoredSalt);
      WriteLn('   Testing "secret123" for alice:');
      WriteLn('     Stored hash:   ', StoredHash);
      WriteLn('     Computed hash: ', ComputedHash);
      WriteLn('     Result: ', IfThen(StoredHash = ComputedHash, 'VALID', 'INVALID'));

      WriteLn('');

      // Test wrong password
      ComputedHash := DemoHashPassword('wrongpassword', StoredSalt);
      WriteLn('   Testing "wrongpassword" for alice:');
      WriteLn('     Computed hash: ', ComputedHash);
      WriteLn('     Result: ', IfThen(StoredHash = ComputedHash, 'VALID', 'INVALID'));
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// ============================================================================
// Demo 5: API Credentials Storage
// ============================================================================

{ Encrypts and stores API keys for multiple services, then retrieves and decrypts them showing truncated plaintext. }
procedure DemoCredentialStorage;
var
  DS: TDataSet;
  UserId: Integer;
  DecryptedKey: string;
begin
  WriteLn('5. Secure API Credentials Storage:');
  WriteLn('   ================================');
  WriteLn('');

  // Get alice's user ID
  DS := Connection.ExecuteQuery('SELECT id FROM users WHERE username = ?', ['alice']);
  try
    UserId := DS.FieldByName('id').AsInteger;
  finally
    DS.Free;
  end;

  // Store encrypted API keys
  WriteLn('   Storing encrypted API credentials...');

  Connection.ExecuteNonQuery(
    'INSERT INTO api_credentials (user_id, service_name, api_key_encrypted, encryption_key_version) VALUES (?, ?, ?, ?)',
    [UserId, 'AWS', DemoEncrypt('AKIAIOSFODNN7EXAMPLE', CurrentEncryptionKey), CurrentKeyVersion]);

  Connection.ExecuteNonQuery(
    'INSERT INTO api_credentials (user_id, service_name, api_key_encrypted, encryption_key_version) VALUES (?, ?, ?, ?)',
    [UserId, 'GitHub', DemoEncrypt('ghp_xxxxxxxxxxxxxxxxxxxx', CurrentEncryptionKey), CurrentKeyVersion]);

  Connection.ExecuteNonQuery(
    'INSERT INTO api_credentials (user_id, service_name, api_key_encrypted, encryption_key_version) VALUES (?, ?, ?, ?)',
    [UserId, 'OpenAI', DemoEncrypt('sk-proj-xxxxxxxxxxxxxxxxxx', CurrentEncryptionKey), CurrentKeyVersion]);

  // Retrieve and display
  WriteLn('');
  WriteLn('   Retrieved credentials:');
  DS := Connection.ExecuteQuery(
    'SELECT c.service_name, c.api_key_encrypted, u.username ' +
    'FROM api_credentials c JOIN users u ON c.user_id = u.id');
  try
    while not DS.EOF do
    begin
      DecryptedKey := DemoDecrypt(DS.FieldByName('api_key_encrypted').AsString, CurrentEncryptionKey);
      WriteLn('   ', DS.FieldByName('username').AsString, ' - ',
              DS.FieldByName('service_name').AsString, ': ',
              Copy(DecryptedKey, 1, 10), '...');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// ============================================================================
// Demo 6: Key Rotation
// ============================================================================

{ Decrypts all user and credential data with the old key, re-encrypts with a new key within a transaction, updates metadata, and verifies the rotated data. }
procedure DemoKeyRotation;
var
  DS: TDataSet;
  OldKey, NewKey: string;
  OldKeyVersion, NewKeyVersion: Integer;
  DecryptedValue, ReEncryptedValue: string;
  RecordsUpdated: Integer;
begin
  WriteLn('6. Encryption Key Rotation:');
  WriteLn('   =========================');
  WriteLn('');

  OldKey := CurrentEncryptionKey;
  OldKeyVersion := CurrentKeyVersion;
  NewKey := 'NewSecureKey2024!@#$%^';
  NewKeyVersion := CurrentKeyVersion + 1;

  WriteLn('   Rotating from key version ', OldKeyVersion, ' to ', NewKeyVersion);
  WriteLn('');

  Connection.BeginTransaction;
  try
    RecordsUpdated := 0;

    // Re-encrypt users table
    WriteLn('   Re-encrypting user data...');
    DS := Connection.ExecuteQuery(
      'SELECT id, email_encrypted, ssn_encrypted FROM users WHERE encryption_key_version = ?',
      [OldKeyVersion]);
    try
      while not DS.EOF do
      begin
        // Decrypt with old key, re-encrypt with new key
        DecryptedValue := DemoDecrypt(DS.FieldByName('email_encrypted').AsString, OldKey);
        ReEncryptedValue := DemoEncrypt(DecryptedValue, NewKey);
        Connection.ExecuteNonQuery('UPDATE users SET email_encrypted = ? WHERE id = ?',
          [ReEncryptedValue, DS.FieldByName('id').AsInteger]);

        DecryptedValue := DemoDecrypt(DS.FieldByName('ssn_encrypted').AsString, OldKey);
        ReEncryptedValue := DemoEncrypt(DecryptedValue, NewKey);
        Connection.ExecuteNonQuery('UPDATE users SET ssn_encrypted = ?, encryption_key_version = ? WHERE id = ?',
          [ReEncryptedValue, NewKeyVersion, DS.FieldByName('id').AsInteger]);

        Inc(RecordsUpdated);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn('   Users updated: ', RecordsUpdated);

    // Re-encrypt credentials
    WriteLn('   Re-encrypting API credentials...');
    RecordsUpdated := 0;
    DS := Connection.ExecuteQuery(
      'SELECT id, api_key_encrypted FROM api_credentials WHERE encryption_key_version = ?',
      [OldKeyVersion]);
    try
      while not DS.EOF do
      begin
        DecryptedValue := DemoDecrypt(DS.FieldByName('api_key_encrypted').AsString, OldKey);
        ReEncryptedValue := DemoEncrypt(DecryptedValue, NewKey);
        Connection.ExecuteNonQuery('UPDATE api_credentials SET api_key_encrypted = ?, encryption_key_version = ? WHERE id = ?',
          [ReEncryptedValue, NewKeyVersion, DS.FieldByName('id').AsInteger]);
        Inc(RecordsUpdated);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn('   Credentials updated: ', RecordsUpdated);

    // Update metadata
    Connection.ExecuteNonQuery(
      'INSERT INTO encryption_meta (key_version, algorithm, key_hash) VALUES (?, ?, ?)',
      [NewKeyVersion, 'DEMO-XOR-NOT-SECURE', DemoHashPassword(NewKey, 'KEYHASH')]);

    Connection.ExecuteNonQuery(
      'INSERT INTO key_rotation_log (old_version, new_version, records_updated, status) VALUES (?, ?, ?, ?)',
      [OldKeyVersion, NewKeyVersion, RecordsUpdated, 'SUCCESS']);

    Connection.Commit;

    CurrentEncryptionKey := NewKey;
    CurrentKeyVersion := NewKeyVersion;
    WriteLn('');
    WriteLn('   Key rotation completed successfully!');
  except
    on E: Exception do
    begin
      Connection.Rollback;
      WriteLn('   Key rotation FAILED: ', E.Message);
      raise;
    end;
  end;

  // Verify data is accessible with new key
  WriteLn('');
  WriteLn('   Verification with new key:');
  DS := Connection.ExecuteQuery('SELECT username, email_encrypted FROM users');
  try
    while not DS.EOF do
    begin
      DecryptedValue := DemoDecrypt(DS.FieldByName('email_encrypted').AsString, CurrentEncryptionKey);
      WriteLn('   ', DS.FieldByName('username').AsString, ': ', DecryptedValue);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// ============================================================================
// Demo 7: Security Best Practices
// ============================================================================

{ Outputs recommended security practices for key management, encryption algorithms, password storage, and database hardening, then enables secure_delete. }
procedure DemoSecurityBestPractices;
begin
  WriteLn('7. Security Best Practices:');
  WriteLn('   =========================');
  WriteLn('');
  WriteLn('   KEY MANAGEMENT:');
  WriteLn('   - Never store encryption keys in the database');
  WriteLn('   - Use environment variables or a key management service (KMS)');
  WriteLn('   - Rotate keys periodically (e.g., every 90 days)');
  WriteLn('   - Use different keys for different data classifications');
  WriteLn('');
  WriteLn('   ENCRYPTION:');
  WriteLn('   - Use AES-256-GCM (authenticated encryption)');
  WriteLn('   - Generate unique IV/nonce for each encryption');
  WriteLn('   - Store IV alongside ciphertext (not secret)');
  WriteLn('   - Consider envelope encryption for large data');
  WriteLn('');
  WriteLn('   PASSWORD STORAGE:');
  WriteLn('   - Use bcrypt, scrypt, or Argon2id');
  WriteLn('   - Minimum 12+ work factor for bcrypt');
  WriteLn('   - Use unique salt per password');
  WriteLn('   - Never use MD5, SHA1, or SHA256 alone');
  WriteLn('');
  WriteLn('   DATABASE SECURITY:');
  WriteLn('   - Enable PRAGMA secure_delete = ON');
  WriteLn('   - Set restrictive file permissions (600 on Unix)');
  WriteLn('   - Use WAL mode with proper journal permissions');
  WriteLn('   - Consider SQLCipher for full database encryption');
  WriteLn('');

  // Enable secure delete
  Connection.ExecuteNonQuery('PRAGMA secure_delete = ON');
  WriteLn('   Applied: PRAGMA secure_delete = ON');
  WriteLn('');
end;

// ============================================================================
// Demo 8: Encryption Status Report
// ============================================================================

{ Displays the key version history, rotation log entries, and current encryption statistics (user and credential counts). }
procedure DemoEncryptionStatus;
var
  DS: TDataSet;
begin
  WriteLn('8. Encryption Status Report:');
  WriteLn('   ==========================');
  WriteLn('');

  WriteLn('   Key Version History:');
  DS := Connection.ExecuteQuery(
    'SELECT key_version, algorithm, created_at, rotated_at FROM encryption_meta ORDER BY key_version');
  try
    while not DS.EOF do
    begin
      WriteLn('     Version ', DS.FieldByName('key_version').AsInteger,
              ' (', DS.FieldByName('algorithm').AsString, ')');
      WriteLn('       Created: ', DS.FieldByName('created_at').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Rotation Log:');
  DS := Connection.ExecuteQuery(
    'SELECT old_version, new_version, rotated_at, records_updated, status FROM key_rotation_log ORDER BY rotated_at');
  try
    if DS.EOF then
      WriteLn('     No rotations recorded')
    else
      while not DS.EOF do
      begin
        WriteLn('     v', DS.FieldByName('old_version').AsString, ' -> v',
                DS.FieldByName('new_version').AsString,
                ' (', DS.FieldByName('records_updated').AsString, ' records) - ',
                DS.FieldByName('status').AsString);
        DS.Next;
      end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Current Encryption Stats:');
  WriteLn('     Active key version: ', CurrentKeyVersion);
  WriteLn('     Encrypted users: ', Integer(Connection.ExecuteScalar('SELECT COUNT(*) FROM users')));
  WriteLn('     Encrypted credentials: ', Integer(Connection.ExecuteScalar('SELECT COUNT(*) FROM api_credentials')));
  WriteLn('');
end;

// ============================================================================
// Cleanup
// ============================================================================

{ Deletes the example database file and its WAL and SHM companions if they exist. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

// ============================================================================
// Main Program
// ============================================================================

begin
  Randomize;
  WriteLn('=== NDXSQLite Example 43: Encrypted Database Patterns ===');
  WriteLn('');
  WriteLn('WARNING: This example uses XOR encryption for demonstration only.');
  WriteLn('         For production, use SQLCipher or a proper crypto library!');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example43.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupEncryptedSchema;
      InitializeEncryption('DemoEncryptionKey123');

      DemoSQLCipherConcepts;
      DemoCryptoLibraryIntegration;
      DemoFieldEncryption;
      DemoPasswordVerification;
      DemoCredentialStorage;
      DemoKeyRotation;
      DemoSecurityBestPractices;
      DemoEncryptionStatus;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
