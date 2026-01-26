# Example 43: Encrypted Database Patterns

This example demonstrates database encryption concepts and secure data storage patterns with SQLite.

> **WARNING**: The XOR encryption used in this example is for **demonstration purposes only**. It is NOT cryptographically secure and should NEVER be used in production. Use proper cryptographic libraries as described below.

## What you'll learn

- SQLCipher full database encryption concepts
- Proper crypto library integration patterns (DCPCrypt, OpenSSL, Argon2)
- Application-level field encryption strategies
- Secure password storage with salted hashes
- API credentials management
- Encryption key rotation with audit logging
- Database file security best practices

## Security Libraries for Production

### Full Database Encryption

**SQLCipher** provides transparent, page-level AES-256 encryption:
- Commercial: https://www.zetetic.net/sqlcipher/
- Open Source: https://github.com/nicholasdeoux/ndxsqlcipher

### Application-Level Encryption (Free Pascal)

- **DCPCrypt**: https://github.com/Laex/Delphi-OpenSSL (AES, Blowfish, etc.)
- **LockBox**: https://github.com/TurboPack/LockBox3
- **OpenSSL bindings**: https://github.com/nicholasdeoux/ndxopenssl

### Password Hashing

- **Argon2**: https://github.com/nicholasdeoux/ndxargon2 (recommended)
- **bcrypt** or **scrypt** implementations

## Encryption Approaches

### 1. Full Database Encryption (SQLCipher)

SQLCipher provides transparent, full-database AES-256 encryption:

```pascal
// With SQLCipher-enabled SQLite:
Connection.ExecuteNonQuery('PRAGMA key = ''your-secret-passphrase''');

// Or use raw 256-bit hex key
Connection.ExecuteNonQuery('PRAGMA key = "x''2DD29CA851E7B56E...''";');

// Change key
Connection.ExecuteNonQuery('PRAGMA rekey = ''new-passphrase''');

// Configure key derivation (256,000 iterations recommended)
Connection.ExecuteNonQuery('PRAGMA kdf_iter = 256000');
```

### 2. Proper Crypto Library Integration

```pascal
// Using DCPCrypt (recommended for Free Pascal):
uses DCPrijndael, DCPsha256;

function EncryptAES256(const Data, Key: string): string;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.InitStr(Key, TDCP_sha256);
    Result := Cipher.EncryptString(Data);
  finally
    Cipher.Free;
  end;
end;

// Using OpenSSL (AES-256-GCM authenticated encryption):
uses OpenSSL;

EVP_aes_256_gcm();
EVP_EncryptInit_ex(ctx, cipher, nil, key, iv);
EVP_EncryptUpdate(ctx, ciphertext, len, plaintext, plen);
EVP_EncryptFinal_ex(ctx, ciphertext + len, len);

// Password hashing with Argon2:
uses Argon2;

hash := TArgon2.HashPassword(password, salt,
  TArgon2Type.Argon2id, 65536, 3, 4);
```

### 3. Application-Level Field Encryption

Encrypt sensitive fields before storing:

```pascal
// Encrypt before insert
EncryptedSSN := Encrypt(PlainSSN, EncryptionKey);
Connection.ExecuteNonQuery(
  'INSERT INTO users (username, ssn_encrypted) VALUES (?, ?)',
  [Username, EncryptedSSN]);

// Decrypt after retrieval
PlainSSN := Decrypt(EncryptedSSN, EncryptionKey);
```

## Schema Design

```sql
-- Encryption metadata (tracks key versions)
CREATE TABLE encryption_meta (
  id INTEGER PRIMARY KEY,
  key_version INTEGER NOT NULL,
  algorithm TEXT NOT NULL,
  created_at TEXT DEFAULT (datetime('now')),
  rotated_at TEXT,
  key_hash TEXT  -- Hash for validation, not the key!
);

-- Users with encrypted fields
CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  username TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL,
  password_salt TEXT NOT NULL,
  email_encrypted TEXT,
  ssn_encrypted TEXT,
  encryption_key_version INTEGER DEFAULT 1
);

-- API credentials storage
CREATE TABLE api_credentials (
  id INTEGER PRIMARY KEY,
  user_id INTEGER REFERENCES users(id),
  service_name TEXT NOT NULL,
  api_key_encrypted TEXT NOT NULL,
  encryption_key_version INTEGER DEFAULT 1
);

-- Key rotation audit log
CREATE TABLE key_rotation_log (
  id INTEGER PRIMARY KEY,
  old_version INTEGER,
  new_version INTEGER,
  rotated_at TEXT DEFAULT (datetime('now')),
  records_updated INTEGER,
  status TEXT
);
```

## Password Storage

Never store plain passwords. Use salted hashes:

```pascal
// Creating a user
Salt := GenerateRandomSalt(16);
PasswordHash := Argon2Hash(Password, Salt);  // Use proper Argon2!

Connection.ExecuteNonQuery(
  'INSERT INTO users (username, password_hash, password_salt) VALUES (?, ?, ?)',
  [Username, PasswordHash, Salt]);

// Verifying password
StoredHash := GetUserPasswordHash(Username);
StoredSalt := GetUserSalt(Username);
ComputedHash := Argon2Hash(InputPassword, StoredSalt);

if StoredHash = ComputedHash then
  // Password valid
```

## Key Rotation

Periodically rotate encryption keys with proper audit trail:

```pascal
procedure RotateEncryptionKey(OldKey, NewKey: string);
var
  RecordsUpdated: Integer;
begin
  Connection.BeginTransaction;
  try
    RecordsUpdated := 0;

    // Re-encrypt all sensitive data
    DS := Connection.ExecuteQuery(
      'SELECT id, email_encrypted FROM users WHERE encryption_key_version = ?',
      [OldKeyVersion]);
    while not DS.EOF do
    begin
      Decrypted := Decrypt(DS['email_encrypted'], OldKey);
      ReEncrypted := Encrypt(Decrypted, NewKey);
      Connection.ExecuteNonQuery(
        'UPDATE users SET email_encrypted = ?, encryption_key_version = ? WHERE id = ?',
        [ReEncrypted, NewKeyVersion, DS['id']]);
      Inc(RecordsUpdated);
      DS.Next;
    end;

    // Log rotation
    Connection.ExecuteNonQuery(
      'INSERT INTO key_rotation_log (old_version, new_version, records_updated, status) ' +
      'VALUES (?, ?, ?, ?)',
      [OldKeyVersion, NewKeyVersion, RecordsUpdated, 'SUCCESS']);

    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;
end;
```

## Security Best Practices

### Key Management

- **Never** store encryption keys in the database
- Use environment variables or key management services (AWS KMS, HashiCorp Vault)
- Rotate keys every 90 days minimum
- Keep key rotation logs for audit

### Encryption

- Use AES-256-GCM (authenticated encryption)
- Generate unique IV/nonce for each encryption
- Store IV alongside ciphertext (not secret)
- Consider envelope encryption for large data

### Password Storage

- Use bcrypt, scrypt, or Argon2id
- Minimum 12+ work factor for bcrypt
- Use unique salt per password
- **Never** use MD5, SHA1, or SHA256 alone

### Database File Security

```pascal
// SQLite PRAGMAs for security
Connection.ExecuteNonQuery('PRAGMA secure_delete = ON');  // Overwrite deleted data
Connection.ExecuteNonQuery('PRAGMA auto_vacuum = FULL');  // Reclaim space
```

### File Permissions

```bash
# Linux/macOS
chmod 600 database.db

# Windows
icacls database.db /grant:r "username:(R,W)"
```

## Encryption Algorithms

| Use Case | Recommended Algorithm |
|----------|----------------------|
| Passwords | Argon2id, bcrypt, scrypt |
| Field encryption | AES-256-GCM |
| Full database | SQLCipher (AES-256) |
| Key derivation | PBKDF2 with high iterations |

## Comparison of Approaches

| Approach | Pros | Cons |
|----------|------|------|
| SQLCipher | Transparent, complete protection | Requires special build |
| Field encryption | Works with any SQLite | Must encrypt/decrypt in code |
| Both combined | Maximum security | More complexity |

## Building

```bash
lazbuild EncryptedDatabase.lpi
```

## Running

```bash
./EncryptedDatabase      # Linux/macOS
EncryptedDatabase.exe    # Windows
```

## Expected Output

```
=== NDXSQLite Example 43: Encrypted Database Patterns ===

WARNING: This example uses XOR encryption for demonstration only.
         For production, use SQLCipher or a proper crypto library!

1. SQLCipher - Full Database Encryption:
   ======================================
   [Shows SQLCipher PRAGMA commands and benefits]

2. Proper Crypto Library Integration:
   ===================================
   [Shows DCPCrypt, OpenSSL, and Argon2 code patterns]

3. Field-Level Encryption Pattern:
   ================================
   Encrypting sensitive fields at application level...

   Data stored in database (encrypted):
   User: alice
     Email (enc): c2VjcmV0ZW1haWxAZXhhbXBsZS...
     SSN (enc):   MTIzLTQ1LTY3ODk=

   Data decrypted with key:
   User: alice
     Email: alice@example.com
     SSN:   123-45-6789

4. Password Verification:
   =======================
   Testing "secret123" for alice:
     Stored hash:   A1B2C3D4
     Computed hash: A1B2C3D4
     Result: VALID

   Testing "wrongpassword" for alice:
     Computed hash: E5F6G7H8
     Result: INVALID

5. Secure API Credentials Storage:
   ================================
   Storing encrypted API credentials...

   Retrieved credentials:
   alice - AWS: AKIAIOSFO...
   alice - GitHub: ghp_xxxxxx...
   alice - OpenAI: sk-proj-x...

6. Encryption Key Rotation:
   =========================
   Rotating from key version 1 to 2

   Re-encrypting user data...
   Users updated: 2
   Re-encrypting API credentials...
   Credentials updated: 3

   Key rotation completed successfully!

   Verification with new key:
   alice: alice@example.com
   bob: bob@company.org

7. Security Best Practices:
   =========================
   [Lists key management, encryption, password, and database security tips]

   Applied: PRAGMA secure_delete = ON

8. Encryption Status Report:
   ==========================
   Key Version History:
     Version 1 (DEMO-XOR-NOT-SECURE)
       Created: 2025-01-21 12:30:45
     Version 2 (DEMO-XOR-NOT-SECURE)
       Created: 2025-01-21 12:30:46

   Rotation Log:
     v1 -> v2 (3 records) - SUCCESS

   Current Encryption Stats:
     Active key version: 2
     Encrypted users: 2
     Encrypted credentials: 3

=== Example completed successfully! ===
```

## Important Notes

- The XOR encryption in this example is for **demonstration only**
- In production, use proper cryptographic libraries:
  - **DCPCrypt** for Delphi/FreePascal
  - **OpenSSL** bindings
  - **libsodium** bindings
- Never roll your own cryptography for production systems
- Always keep encryption keys separate from encrypted data

## Cross-Platform

This example works on Windows, Linux, and macOS.
