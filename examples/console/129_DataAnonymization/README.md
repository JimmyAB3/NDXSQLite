# Example 129: Data Anonymization - PII Protection, K-Anonymity, Pseudonymization

## Overview

This example demonstrates a **Data Anonymization** system implementing multiple privacy-preserving techniques: direct identifier masking, consistent pseudonymization, generalization hierarchies, k-anonymity analysis, data suppression, token-based de-identification, and utility preservation measurement.

## Features Demonstrated

### 1. Original PII Data
- 20 person records with 10 fields each
- Direct identifiers: name, email, phone
- Quasi-identifiers: age, gender, zip code, city
- Sensitive attributes: salary, medical condition

### 2. Direct Masking
- Email masking: show first 3 characters + domain
- Phone masking: show last 4 digits only
- Irreversible transformation (original cannot be recovered)

### 3. Consistent Pseudonymization
- Same input always produces same pseudonym
- Mapping table for reversibility when authorized
- Alphabetical ordering preserved in mapping
- 20 unique names -> 20 unique pseudonyms

### 4. Generalization Hierarchies
- Age: exact value -> 10-year range (34 -> "30-39")
- Zip code: full code -> 3-digit prefix (90210 -> "902**")
- Salary: exact value -> 20k band ($85000 -> "80k-100k")
- Hierarchical: can generalize further if needed

### 5. K-Anonymity Analysis
- Quasi-identifiers: age_range + gender + zip_prefix
- 16 equivalence classes identified
- Minimum k=1 (many singletons)
- Violations flagged for 2-anonymity

### 6. Data Suppression
- Records in singleton classes are suppressed
- Achieves target k-anonymity level
- Trade-off: 13 records suppressed (35% utility)
- Alternative: further generalization to keep more records

### 7. Token-Based Pseudonymization
- Deterministic token generation (user_0001@anon.local)
- Reversible with secure mapping table
- Mapping table must be stored separately
- Enables data linking without exposing PII

### 8. Utility Preservation
- Statistical distributions preserved after generalization
- Salary: mean, range, and distribution shape maintained
- Age: count per range matches original distribution
- Trade-off quantification between privacy and utility

### 9. Field-Level Policies
- Per-field anonymization strategy
- 5 policy types: mask, pseudonymize, generalize, suppress, keep
- Configurable parameters per policy
- Complete record transformation in one pass

### 10. Statistics
- PII risk classification (Direct ID, Quasi-ID, Sensitive)
- K-anonymity summary (min/max/avg class size)
- Mapping and policy counts
- Field-level risk assessment

## Architecture

```
+------------------+     +------------------+     +------------------+
| Original Data    |     | Anonymization    |     | Protected Data   |
+------------------+     +------------------+     +------------------+
| PII Records      |---->| Field Policies   |---->| Masked IDs       |
| Direct IDs       |     | Generalization   |     | Generalized QIs  |
| Quasi-IDs        |     | K-Anonymity Check|     | Preserved Stats  |
| Sensitive Attrs  |     | Suppression      |     | Utility Metrics  |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| persons          |     | persons_anon     |
+------------------+     +------------------+
| id (PK)          |     | id (PK)          |
| first_name       |     | first_name       |
| last_name        |     | last_name        |
| email            |     | email            |
| phone            |     | phone            |
| age              |     | age_range        |
| gender           |     | gender           |
| zip_code         |     | zip_prefix       |
| city             |     | city             |
| salary           |     | salary_band      |
| medical_condition|     | medical_condition|
+------------------+     +------------------+

+------------------+     +------------------+     +------------------+
| pseudonym_map    |     | anon_policies    |     | k_anon_classes   |
+------------------+     +------------------+     +------------------+
| field_name       |     | field_name (UQ)  |     | age_range        |
| original_value   |     | policy_type      |     | gender           |
| pseudonym        |     | parameters       |     | zip_prefix       |
| UNIQUE(field,val)|     +------------------+     | record_count     |
+------------------+                               +------------------+
```

## Anonymization Techniques

| Technique | Reversible | Utility | Privacy |
|-----------|-----------|---------|---------|
| Masking | No | Low | High |
| Pseudonymization | Yes (with key) | Medium | Medium |
| Generalization | No | Medium-High | Medium |
| Suppression | N/A (removed) | Low | High |
| Keep | N/A | Full | None |

## K-Anonymity Formula

```
A dataset satisfies k-anonymity if every combination of
quasi-identifier values appears in at least k records.

Quasi-identifiers: {age_range, gender, zip_prefix}

Example class: (30-39, F, 902) with count=1
  -> Violates 2-anonymity (only 1 person matches)
  -> This person is uniquely identifiable by these attributes
```

## Compilation

```bash
cd 129_DataAnonymization
lazbuild DataAnonymization.lpi
./DataAnonymization
```

## Sample Output

```
1. Original: 20 records with full PII (name, email, phone, age, zip, salary)
2. Masking: "alice.johnson@email.com" -> "ali***@email.com"
3. Pseudonyms: "Alice" -> "Person_1" (consistent mapping)
4. Generalization: age 34 -> "30-39", zip 90210 -> "902**"
5. K-Anonymity: 16 classes, min k=1, 13 violations for k=2
6. Suppression: 13 records removed for 2-anonymity (35% utility)
7. Tokens: "user_0001@anon.local" (reversible with lookup)
8. Utility: salary distribution preserved, age ranges proportional
9. Policies: mask(2), pseudonymize(1), generalize(3), suppress(1), keep(3)
```

## Related Examples

- **130_ConsentManagement** - GDPR consent tracking
- **131_RetentionPolicies** - Automated data lifecycle

## Best Practices

1. **Identify PII types**: Classify fields as direct ID, quasi-ID, or sensitive
2. **Minimize data**: Only collect and retain what's necessary
3. **Layer techniques**: Combine masking + generalization for defense in depth
4. **Measure utility**: Quantify information loss after anonymization
5. **Test re-identification**: Verify k-anonymity with realistic attack models
6. **Secure mappings**: Store pseudonym lookup tables separately with access controls
7. **Document policies**: Record which technique applies to each field
8. **Regular audits**: Re-check k-anonymity as data changes over time
