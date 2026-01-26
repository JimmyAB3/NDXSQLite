# Example 148: SQLCipher Database Encryption

This example demonstrates full database encryption using [SQLCipher](https://www.zetetic.net/sqlcipher/), a transparent encryption extension for SQLite that provides AES-256 encryption with authenticated encryption modes.

## Overview

SQLCipher encrypts the entire database file, including:
- All table data and indexes
- Schema definitions
- Database metadata
- Temporary files and journals

The encryption is transparent to the application. Standard SQL operations work unchanged once the encryption key is provided.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| **Availability Detection** | Runtime check for SQLCipher vs standard SQLite |
| **Database Creation** | Creating new encrypted databases with passphrase |
| **Authentication** | Opening databases with correct/incorrect keys |
| **Key Rotation** | Changing encryption keys on existing databases |
| **Error Handling** | Graceful handling of authentication failures |
| **Fallback Mode** | Normal operation when SQLCipher unavailable |

## Requirements

### SQLCipher Installation

SQLCipher must be installed and accessible at runtime. NDXSQLite automatically detects and loads SQLCipher when available.

**Linux (Debian/Ubuntu)**
```bash
sudo apt install libsqlcipher1 libsqlcipher-dev
```

**Linux (Fedora/RHEL)**
```bash
sudo dnf install sqlcipher sqlcipher-devel
```

**macOS (Homebrew)**
```bash
brew install sqlcipher
```

**Windows**

Download prebuilt binaries from [zetetic.net/sqlcipher](https://www.zetetic.net/sqlcipher/) and place `sqlcipher.dll` in your application directory.

### Compiler

- Free Pascal 3.2.2 or later
- Lazarus 2.2 or later (optional, for IDE)

## Building

```bash
cd examples/console/148_SQLCipherEncryption
lazbuild SQLCipherEncryption.lpi
```

Or compile directly with FPC:
```bash
fpc -Fu../../../src SQLCipherEncryption.lpr
```

## Running

```bash
./SQLCipherEncryption
```

### Sample Output (with SQLCipher)

```
================================================
NDXSQLite Example 148: SQLCipher Database Encryption
================================================

================================================
1. SQLCipher Availability Check
================================================

  [OK] SQLCipher is available on this system.

  SQLCipher encryption features:
    - AES-256 encryption in XTS mode
    - PBKDF2 key derivation (256,000 iterations)
    - HMAC-SHA512 page authentication
    - Transparent read/write encryption

================================================
2. Creating an Encrypted Database
================================================

  Database: /path/to/example148_encrypted.db
  Key:      MySec*** (truncated)

  Opening encrypted database...
  Encryption status: ACTIVE
  ...
```

### Sample Output (without SQLCipher)

```
================================================
1. SQLCipher Availability Check
================================================

  [WARNING] SQLCipher is not available.
  Standard SQLite library detected.

  To enable encryption, install SQLCipher:
    Linux:   sudo apt install libsqlcipher1
    macOS:   brew install sqlcipher
    Windows: Download from zetetic.net/sqlcipher

  Encryption demonstrations will be skipped.

  Running in fallback mode (no encryption)...
```

## Usage in Your Application

### Creating an Encrypted Database

```pascal
var
  Options: TNDXSQLiteConnectionOptions;
  Connection: TNDXSQLiteConnection;
begin
  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := 'secure_data.db';
    Options.EncryptionKey := GetKeyFromSecureStorage; // Never hardcode keys
    Options.IsPrimaryConnection := True;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;
      // Database is now encrypted - use standard SQL operations
      Connection.ExecuteNonQuery('CREATE TABLE secrets (...)');
      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;
end;
```

### Checking SQLCipher Availability

```pascal
if TNDXSQLiteConnection.IsSQLCipherAvailable then
  WriteLn('Encryption supported')
else
  WriteLn('Standard SQLite - no encryption');
```

### Rotating Encryption Keys

```pascal
Connection.Open;
Connection.ChangeEncryptionKey(NewSecureKey);
Connection.Close;
// Database now requires NewSecureKey to open
```

## API Reference

### TNDXSQLiteConnectionOptions.EncryptionKey

Set this property before opening the connection to enable encryption:

```pascal
Options.EncryptionKey := 'your-secret-passphrase';
```

### TNDXSQLiteConnection.IsSQLCipherAvailable

Class function that checks if SQLCipher is loaded:

```pascal
if TNDXSQLiteConnection.IsSQLCipherAvailable then
  // Encryption is available
```

### TNDXSQLiteConnection.IsEncrypted

Returns `True` if the current connection uses SQLCipher encryption:

```pascal
if Connection.IsEncrypted then
  WriteLn('Database is encrypted');
```

### TNDXSQLiteConnection.ChangeEncryptionKey

Changes the encryption key of an open database:

```pascal
// Change to new key
Connection.ChangeEncryptionKey('new-passphrase');

// Remove encryption (empty key)
Connection.ChangeEncryptionKey('');
```

## Security Best Practices

### Key Management

- **Never hardcode** encryption keys in source code
- Use environment variables or secure key vaults (AWS KMS, HashiCorp Vault)
- Implement key rotation policies (quarterly recommended)
- Store key recovery procedures securely

### Key Strength

- Use passphrases with 16+ characters
- Include mixed case, numbers, and symbols
- Consider raw 256-bit keys for maximum security

### SQLCipher Configuration

```sql
-- Increase key derivation iterations (slower but more secure)
PRAGMA kdf_iter = 512000;

-- Enable secure memory wiping
PRAGMA cipher_memory_security = ON;

-- Use larger page size for bulk operations
PRAGMA cipher_page_size = 8192;
```

## Technical Specifications

### Encryption Parameters

| Parameter | Value |
|-----------|-------|
| Algorithm | AES-256 |
| Mode | XTS |
| Key Derivation | PBKDF2-HMAC-SHA512 |
| KDF Iterations | 256,000 (default) |
| Page Authentication | HMAC-SHA512 |
| Salt | 16 bytes (random per database) |

### Performance Characteristics

- Typical overhead: 5-15% depending on workload
- Key derivation occurs once per connection open
- Bulk operations benefit from larger page sizes
- Connection pooling reduces key derivation overhead

## Comparison: SQLCipher vs Application-Level Encryption

| Feature | SQLCipher | Application-Level |
|---------|-----------|-------------------|
| Scope | Entire database | Selected fields only |
| Transparency | Fully transparent | Requires encrypt/decrypt calls |
| Performance | ~5-15% overhead | Varies by implementation |
| Security | AES-256, HMAC, PBKDF2 | Depends on implementation |
| Maintenance | Automatic | Manual key management |

For comprehensive database protection, SQLCipher is recommended.

## Troubleshooting

### "file is not a database"

This error occurs when:
- Wrong encryption key provided
- No key provided for encrypted database
- Database created with different SQLCipher version

### SQLCipher Not Detected

Verify library installation:
```bash
# Linux
ls -la /usr/lib/x86_64-linux-gnu/libsqlcipher*

# macOS
ls -la /usr/local/lib/libsqlcipher* /opt/homebrew/lib/libsqlcipher*
```

### Standard SQLite Loaded Instead of SQLCipher

NDXSQLite searches for SQLCipher before SQLite. If standard SQLite loads:
1. Verify SQLCipher is installed correctly
2. Check library search paths
3. On Linux, verify no conflicting `LD_LIBRARY_PATH` settings

## License

MIT License - See project root for details.

## See Also

- [SQLCipher Documentation](https://www.zetetic.net/sqlcipher/sqlcipher-api/)
- [NDXSQLite Documentation](../../../docs/)
- [Example 01: Basic Connection](../01_BasicConnection/)
