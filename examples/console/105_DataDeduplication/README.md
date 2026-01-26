# Example 105: Data Deduplication - Fuzzy Matching, Merge Records, Canonical IDs

## Overview

This example demonstrates a complete **data deduplication system** with fuzzy matching rules, duplicate group management, record merging with conflict resolution, canonical ID assignment, and a full audit trail.

## Features Demonstrated

### 1. Raw Contact Data
- Contact records with name, email, phone, company, address
- Near-duplicate entries with typos, format variations, abbreviations

### 2. Fuzzy Matching Detection
- Rule-based duplicate detection:
  - Same email (case-insensitive, normalized)
  - Same phone (digits only, stripped formatting)
  - Similar name (first 3 chars + same last name)
- Normalized lookup table for efficient matching
- Pair-wise comparison with match reasons

### 3. Duplicate Groups
- Group potential duplicates together
- Similarity scores per member
- Match reasons tracking (email, phone, name)
- Pending/merged/rejected status workflow

### 4. Record Merging
- Canonical record selection (highest confidence)
- Field-by-field merge strategy (longest/most complete value wins)
- Deactivate merged records (is_active = 0)
- Set canonical_id foreign key on merged records

### 5. Merge Conflict Resolution
- Email typo detection (keep correct spelling)
- Address completeness (keep longer form)
- Company name normalization (keep most complete)

### 6. Reject False Positives
- Review and reject non-duplicate groups
- Same email/phone but genuinely different people
- Admin decision tracking

### 7. Canonical ID Lookup
- Active records = canonical/unique contacts
- Deactivated records point to their canonical via canonical_id
- Query active-only for clean data

### 8. Merge Audit Log
- Every field change recorded
- Kept value vs discarded value
- Timestamp and group reference
- Full traceability for compliance

### 9. Statistics
- Active vs inactive contact counts
- Group status distribution
- Merge operation count
- Deduplication rate percentage

## Database Schema

```
+------------------+     +------------------+     +------------------+
| contacts         |     | duplicate_groups  |     | duplicate_members|
+------------------+     +------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |<--->| group_id (FK)    |
| first_name       |     | status           |     | contact_id (FK)  |
| last_name        |     | detected_at      |     | similarity_score |
| email            |     | resolved_at      |     | match_reasons    |
| phone            |     | resolved_by      |     | is_canonical     |
| company          |     +------------------+     +------------------+
| address          |
| canonical_id(FK) |     +------------------+     +-------------------+
| is_active        |     | merge_log        |     | contact_normalized|
+------------------+     +------------------+     +-------------------+
                          | id (PK, AUTO)    |     | contact_id (PK)   |
                          | group_id (FK)    |     | norm_email         |
                          | canonical_id     |     | norm_name          |
                          | merged_id        |     | norm_phone         |
                          | field_name       |     +-------------------+
                          | kept_value       |
                          | discarded_value  |
                          | merged_at        |
                          +------------------+
```

## Compilation

```bash
cd 105_DataDeduplication
lazbuild DataDeduplication.lpi
./DataDeduplication
```

## Sample Output

```
2. Fuzzy Matching - Duplicate Detection
   Rule 1: Same email (case-insensitive)
   Match: #1 <-> #3 (email: john.smith@acme.com)

   Rule 2: Same phone (digits only)
   Match: #1 <-> #2 (phone: 5551234567)
   Match: #4 <-> #5 (phone: 5559876543)

4. Merge Records (Group #1: John Smith)
   Canonical record: #1
   Merging #2 into #1...
      company: "Acme Corp" -> "Acme Corporation"

9. Deduplication Statistics
   Total contacts:     10
   Active contacts:    7
   Merged (inactive):  3
   Deduplication rate: 30%
```

## Related Examples

- **106_DataLineage** - Data origin and transformation tracking
- **107_SchemaEvolution** - Database schema versioning

## Best Practices

1. **Normalize before matching**: Lowercase, trim, strip formatting for fair comparison
2. **Multiple match rules**: Combine email, phone, name for higher confidence
3. **Similarity scores**: Weight different match types for ranking
4. **Canonical selection**: Pick the most complete/reliable record as master
5. **Field-level merge**: Compare each field independently, pick best value
6. **Audit everything**: Log every merge decision for compliance and undo
7. **Reject workflow**: Allow false positives to be explicitly marked as not-duplicates
8. **Soft delete**: Use is_active flag, never hard-delete merged records
