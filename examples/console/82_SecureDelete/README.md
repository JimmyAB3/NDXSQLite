# Example 82: Secure Delete

## Overview

This example demonstrates **secure deletion patterns** that overwrite data before removing it, preventing recovery of sensitive information. It implements multi-pass overwrite, soft/hard delete workflows, GDPR erasure request handling, retention policies, and comprehensive audit trails — essential for compliance with data protection regulations.

## Features Demonstrated

### 1. Multi-Pass Overwrite
- Pass 1: Zeros pattern ('0' characters)
- Pass 2: X pattern ('X' characters)
- Pass 3: Zeros again (final wipe)
- Inspired by DoD 5220.22-M standard

### 2. Field-Level Secure Deletion
- Individual field overwriting
- Preserves record structure during process
- Original length maintained for verification
- Per-field audit trail

### 3. Soft Delete vs. Hard Delete
- Soft: Mark as deleted, data preserved for audit
- Hard: Physical removal after secure overwrite
- Configurable per retention policy

### 4. GDPR Right to Erasure
- Deletion request workflow
- Status tracking (pending, processing, completed, verified)
- Reason documentation
- Verification step

### 5. Cascading Secure Delete
- User PII fields overwritten
- Related documents content wiped
- Financial records zeroed
- All related tables processed

### 6. Retention Policies
- Configurable retention periods
- Delete method selection (soft/hard/secure)
- Overwrite pass count configuration
- Policy enforcement simulation

### 7. Deletion Audit Trail
- Every overwrite operation logged
- Pass number tracked
- Original field lengths recorded
- Verification status

## Database Schema

```
+--------+     +------------+     +--------------------+
| users  |<--->| documents  |     | deletion_requests  |
+--------+     +------------+     +--------------------+
| email  |     | content    |     | user_id            |
| phone  |     | title      |     | request_type       |
| name   |     | classif.   |     | status             |
| ssn    |     | is_deleted |     | overwrite_passes   |
+--------+     +------------+     | records_deleted    |
    |                             +--------------------+
    v                                      |
+-------------------+             +------------------+
| financial_records |             | deletion_log     |
+-------------------+             +------------------+
| account_number    |             | target_table     |
| routing_number    |             | field_name       |
| card_number       |             | original_length  |
| balance           |             | overwrite_pass   |
+-------------------+             | action           |
                                  +------------------+

+-------------------+
| retention_policies|
+-------------------+
| target_table      |
| retention_days    |
| delete_method     |
| overwrite_passes  |
+-------------------+
```

## Key Patterns

### Multi-Pass Overwrite
```pascal
procedure SecureOverwriteField(const TableName, FieldName: string;
  RecordId, Passes, RequestId: Integer);
begin
  OrigLen := Conn.ExecuteScalar(
    'SELECT length(' + FieldName + ') FROM ' + TableName + ' WHERE id = ?', [RecordId]);

  for Pass := 1 to Passes do
  begin
    OverwriteData := GenerateOverwritePattern(Pass, OrigLen);
    Conn.ExecuteNonQuery(
      'UPDATE ' + TableName + ' SET ' + FieldName + ' = ? WHERE id = ?',
      [OverwriteData, RecordId]);
    // Log each pass
  end;
end;
```

### Overwrite Pattern Generation
```pascal
function GenerateOverwritePattern(Pass: Integer; Len: Integer): string;
begin
  case Pass of
    1: Ch := '0';   // zeros
    2: Ch := 'X';   // pattern
    3: Ch := '0';   // zeros again
  end;
  Result := StringOfChar(Ch, Len);
end;
```

### Cascading Secure Delete
```pascal
procedure SecureDeleteUser(UserId, RequestId: Integer);
begin
  // 1. Overwrite user PII
  SecureOverwriteField('users', 'email', UserId, 3, RequestId);
  SecureOverwriteField('users', 'ssn', UserId, 3, RequestId);

  // 2. Overwrite related documents
  for each doc in user's documents:
    SecureOverwriteField('documents', 'content', DocId, 3, RequestId);

  // 3. Overwrite financial records
  for each record in user's financial data:
    SecureOverwriteField('financial_records', 'card_number', FinId, 3, RequestId);

  // 4. Mark soft-deleted
  UPDATE users SET is_deleted = 1 WHERE id = ?;
end;
```

### Verification
```pascal
FieldVal := DS.FieldByName('email').AsString;
AllOverwritten := (Length(FieldVal) > 0) and (FieldVal[1] = '0');
WriteLn(Format('[%d bytes overwritten] %s',
  [Length(FieldVal), IfThen(AllOverwritten, 'VERIFIED', 'ORIGINAL')]));
```

## Demonstration Sections

1. **Schema Creation** - Secure delete tables
2. **Sample Data** - Users, documents, financial records
3. **Before Deletion** - Show original sensitive data
4. **Secure Delete** - Execute 3-pass overwrite
5. **Verification** - Confirm data is overwritten
6. **Soft vs. Hard** - Compare deletion strategies
7. **Retention Policies** - Automated purge rules
8. **Audit Log** - Overwrite operation history
9. **Deletion Requests** - GDPR erasure workflow
10. **Statistics** - Overall deletion metrics

## Deletion Workflow

```
User Request          Retention Policy          Legal Order
     |                      |                       |
     v                      v                       v
+------------------+
| deletion_request |  (pending)
+--------+---------+
         |
         v
+------------------+
| Overwrite Fields |  Pass 1: zeros
|                  |  Pass 2: pattern
|                  |  Pass 3: zeros
+--------+---------+
         |
         v
+------------------+
| Mark Deleted     |  (is_deleted = 1)
+--------+---------+
         |
    +----+----+
    |         |
    v         v
Soft Delete  Hard Delete
(preserve    (DELETE FROM
 structure)   after overwrite)
    |         |
    v         v
+------------------+
| Verify & Audit   |
+------------------+
```

## Compilation

```bash
cd 82_SecureDelete
lazbuild SecureDelete.lpi
./SecureDelete
```

## Sample Output

```
3. Data Before Secure Deletion
   User #1 (John Doe) - PII data:
     Email:    john.doe@email.com
     SSN:      123-45-6789
     Card:     4111222233334444

5. Verification After Secure Delete
   User #1 data after overwrite:
     Email:      [18 bytes overwritten] VERIFIED OVERWRITTEN
     Phone:      [12 bytes overwritten] VERIFIED OVERWRITTEN
     SSN:        [11 bytes overwritten] VERIFIED OVERWRITTEN
     is_deleted: 1

8. Deletion Audit Log
   Overwrite passes completed:
     Pass 1: 13 field overwrites
     Pass 2: 13 field overwrites
     Pass 3: 13 field overwrites

10. Secure Deletion Statistics
    Total overwrite operations: 39
    Total bytes overwritten: 870
    Deletion requests: 3 (completed: 1, pending: 2)
```

## Related Examples

- **80_RowLevelSecurity** - Access control
- **81_DataMasking** - PII masking
- **75_UserAuthentication** - User management

## Security Considerations

1. **SQLite VACUUM**: Run after hard deletes to reclaim physical space
2. **WAL Mode**: Write-Ahead Log may retain deleted data; checkpoint after secure delete
3. **Backup Files**: Ensure backup files are also securely deleted
4. **SSD Wear Leveling**: Physical overwrite may not be guaranteed on SSDs
5. **Memory Residue**: Clear in-memory caches after secure delete
6. **Journal Files**: SQLite journal may contain pre-overwrite data
7. **Encryption at Rest**: Consider full database encryption as complementary measure

## Compliance Standards

| Standard | Requirement | Implementation |
|----------|-------------|----------------|
| GDPR Art. 17 | Right to erasure | Deletion request workflow |
| DoD 5220.22-M | 3-pass overwrite | Multi-pass pattern |
| NIST SP 800-88 | Media sanitization | Overwrite + verification |
| PCI DSS 3.1 | Render unrecoverable | Secure overwrite of card data |
| HIPAA | PHI disposal | Medical record secure delete |
| SOX | Financial record retention | Retention policies |
