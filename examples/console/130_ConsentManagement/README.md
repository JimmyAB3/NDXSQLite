# Example 130: Consent Management - GDPR Compliance, DSAR, Processing Records

## Overview

This example demonstrates a **Consent Management** system implementing GDPR compliance features: purpose-based consent tracking, withdrawal management, data subject access requests (DSAR), processing activity records, consent history audit trails, and purpose-based access control.

## Features Demonstrated

### 1. Data Subjects
- 5 registered users with unique subject IDs
- Registration timestamps for lifecycle tracking
- Foundation for all consent relationships

### 2. Processing Purposes
- 6 defined purposes with legal basis
- Legal bases: consent, legitimate_interest, contract
- Mandatory flag for contract-based processing
- Clear descriptions for informed consent

### 3. Consent Grants
- 12 active consents across subjects
- Expiry dates for automatic renewal prompts
- Version tracking for policy updates
- Per-subject, per-purpose granularity

### 4. Consent Status Matrix
- Bird's-eye view of all consent states
- g=granted, w=withdrawn, -=never set
- Quick identification of gaps and withdrawals
- Cross-reference subjects vs purposes

### 5. Consent Withdrawals
- 5 withdrawn consents with reasons tracked
- Withdrawal rate per purpose (THIRD_PARTY highest at 50%)
- Reasons: "Too many emails", "Privacy concerns", "Data minimization"
- Timestamp precision for compliance proof

### 6. Data Subject Access Requests
- 5 DSAR types: access, rectification, erasure, portability
- Status tracking: submitted, in_progress, completed, rejected
- 30-day compliance deadline checking
- Response time monitoring

### 7. Processing Activity Records
- 9 processing events logged
- Data categories tracked per activity
- Processor identification (MailService, AnalyticsEngine, etc.)
- Consent verification flag

### 8. Consent History (Audit Trail)
- 9 audit entries with full state transitions
- Previous and new status recorded
- IP address tracking for accountability
- Withdrawal reason analysis

### 9. Purpose-Based Access Control
- Real-time consent verification per subject+purpose
- Three legal basis rules applied:
  - consent: must be explicitly granted
  - legitimate_interest: allowed unless explicitly withdrawn
  - contract: always allowed (mandatory)
- 15 access decisions computed

### 10. Compliance Statistics
- Consent rates per purpose (20%-60%)
- DSAR response times (all within 30 days)
- Active vs withdrawn consent counts
- Overall system health metrics

## Architecture

```
+------------------+     +------------------+     +------------------+
| Data Subject     |     | Consent Engine   |     | Compliance Check |
+------------------+     +------------------+     +------------------+
| Register         |---->| Grant/Withdraw   |---->| Access Allowed?  |
| DSAR Requests    |     | History Tracking |     | DSAR Deadlines   |
| Rights Exercise  |     | Expiry Management|     | Processing Audit |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| data_subjects    |     | purposes         |     | consents         |
+------------------+     +------------------+     +------------------+
| subject_id (UQ)  |     | purpose_code (UQ)|     | subject_id       |
| name             |     | purpose_name     |     | purpose_code     |
| email            |     | description      |     | status           |
| registered_at    |     | legal_basis      |     | granted_at       |
+------------------+     | mandatory        |     | withdrawn_at     |
                          +------------------+     | expires_at       |
                                                   | version          |
+------------------+     +------------------+     +------------------+
| consent_history  |     | processing_records|    | dsar_requests    |
+------------------+     +------------------+     +------------------+
| subject_id       |     | subject_id       |     | subject_id       |
| purpose_code     |     | purpose_code     |     | request_type     |
| action           |     | activity         |     | status           |
| previous_status  |     | data_categories  |     | submitted_at     |
| new_status       |     | processor        |     | completed_at     |
| reason           |     | processed_at     |     | response_details |
| changed_at       |     | consent_verified |     +------------------+
| ip_address       |     +------------------+
+------------------+
```

## Legal Basis Rules

| Legal Basis | Consent Required | When Allowed |
|-------------|-----------------|--------------|
| consent | Explicit grant | Only when status = "granted" |
| legitimate_interest | Opt-out model | Unless explicitly "withdrawn" |
| contract | Not applicable | Always (mandatory for service) |

## GDPR Rights Implemented

| Right | DSAR Type | Description |
|-------|-----------|-------------|
| Art. 15 | access | Right to know what data is held |
| Art. 16 | rectification | Right to correct inaccurate data |
| Art. 17 | erasure | Right to be forgotten |
| Art. 20 | portability | Right to receive data in portable format |

## Compilation

```bash
cd 130_ConsentManagement
lazbuild ConsentManagement.lpi
./ConsentManagement
```

## Sample Output

```
1. Subjects: 5 registered users with IDs SUB-001 to SUB-005
2. Purposes: 6 purposes (MARKETING, ANALYTICS, PROFILING, THIRD_PARTY, SERVICE, PERSONALIZE)
4. Matrix: SUB-003 has all consents, SUB-004 withdrew all optional
5. Withdrawals: THIRD_PARTY has 50% withdrawal rate
6. DSAR: All responses within 30-day deadline
9. Access: legitimate_interest allows unless withdrawn, consent requires explicit grant
10. Stats: 60% consent rate for marketing, 20% for third-party sharing
```

## Related Examples

- **129_DataAnonymization** - PII protection techniques
- **131_RetentionPolicies** - Automated data lifecycle management

## Best Practices

1. **Granular consent**: Separate consent per purpose, never bundle
2. **Easy withdrawal**: Make withdrawal as easy as granting
3. **Audit everything**: Record all consent changes with timestamps
4. **30-day deadline**: DSAR responses must complete within one month
5. **Version tracking**: When policies change, re-consent is needed
6. **Legal basis first**: Determine legal basis before processing
7. **Data minimization**: Only process categories covered by consent
8. **Expiry management**: Prompt for renewal before consent expires
