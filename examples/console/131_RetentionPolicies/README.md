# Example 131: Retention Policies - Policy-Driven Expiry, Legal Hold, Cascade

## Overview

This example demonstrates an **Automated Data Lifecycle** management system with policy-driven expiry, legal hold enforcement, cascading cleanup, and retention audit logging. It simulates a compliance-ready data retention engine that manages data from creation through archival or deletion.

## Features Demonstrated

### 1. Retention Policies
- 8 policies ranging from 7 days (temp uploads) to 7 years (transactions)
- Three actions: delete, archive, anonymize
- Cascade flag for parent-child relationships
- Category-based organization

### 2. Data Inventory
- 23 records across 8 categories
- Status tracking: active, expired, deleted, archived
- Per-owner record counts
- Expired record identification

### 3. Policy-Driven Expiry
- Automatic expiry detection based on retention periods
- Reference date comparison (created_at + retention_days)
- 10 records identified as expired
- Ordered by expiry date for prioritization

### 4. Legal Holds
- 4 holds from different departments (legal, finance, compliance, security)
- Hold status: active or released
- Prevents deletion of 3 expired records
- Hold reason tracking for accountability

### 5. Expiry Execution
- Processes expired records respecting legal holds
- 7 records processed (4 deleted, 2 archived, 1 anonymized)
- 3 records blocked by legal holds
- Status updates and audit logging

### 6. Cascading Cleanup
- 4 cascade rules defined (parent -> child relationships)
- Automatically processes children of deleted/archived parents
- Supports different cascade actions per relationship
- Prevents orphaned data

### 7. Deletion Audit Log
- 10 audit entries with action, reason, and timestamp
- Reasons: policy_expiry, legal_hold_block, cascade, manual
- Actions: delete, archive, anonymize, held
- Complete accountability trail

### 8. Hold Management
- Active hold listing with placement details
- Released hold history
- Hold release simulation
- Department-based accountability

### 9. Compliance Report
- Overdue records (expired but not processed due to holds)
- Per-category compliance summary
- Days overdue calculation
- Within-policy vs overdue breakdown

### 10. Statistics
- Record status distribution
- Retention tier classification (short/medium/long-term)
- Active hold count
- Overall system metrics

## Architecture

```
+------------------+     +------------------+     +------------------+
| Retention Policy |     | Lifecycle Engine  |     | Audit Trail      |
+------------------+     +------------------+     +------------------+
| Category Rules   |---->| Expiry Check     |---->| Deletion Log     |
| Retention Days   |     | Hold Verification|     | Action Tracking  |
| Action Type      |     | Cascade Execution|     | Compliance Report|
| Cascade Flag     |     | Status Update    |     | Hold History     |
+------------------+     +------------------+     +------------------+
        |                         |
        v                         v
+------------------+     +------------------+
| Legal Holds      |     | Cascade Rules    |
+------------------+     +------------------+
| Block Deletion   |     | Parent->Child    |
| Placed By/When   |     | Action per Link  |
| Release Tracking |     | Orphan Prevention|
+------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| retention_policies|    | data_records     |     | legal_holds      |
+------------------+     +------------------+     +------------------+
| category (UQ)    |     | record_ref (UQ)  |     | hold_id (UQ)     |
| retention_days   |     | category         |     | record_ref       |
| description      |     | owner            |     | reason           |
| action           |     | created_at       |     | placed_by        |
| cascade_children |     | expires_at       |     | placed_at        |
+------------------+     | status           |     | released_at      |
                          | parent_ref       |     | status           |
+------------------+     +------------------+     +------------------+
| deletion_log     |
+------------------+     +------------------+
| record_ref       |     | cascade_rules    |
| category         |     +------------------+
| action_taken     |     | parent_category  |
| reason           |     | child_category   |
| executed_at      |     | cascade_action   |
| policy_id        |     +------------------+
+------------------+
```

## Retention Tiers

| Tier | Duration | Categories | Action |
|------|----------|------------|--------|
| Short-term | 7-30 days | temp_upload, session_log | delete |
| Medium-term | 90-365 days | analytics_event, support_ticket, marketing_pref | delete/anonymize |
| Long-term | 2-7 years | user_account, audit_trail, transaction | archive |

## Legal Hold Rules

```
1. Hold OVERRIDES retention policy (no exceptions)
2. Expired + Held = status remains "expired" (not deleted)
3. Hold must be explicitly released before processing
4. All hold actions are logged in deletion_log as "legal_hold_block"
5. Multiple holds can exist on same record
6. Released holds are preserved for audit trail
```

## Cascade Logic

```
When parent record is deleted/archived:
  1. Find all children linked via parent_ref
  2. Match cascade_rules (parent_category -> child_category)
  3. Apply cascade_action to each child
  4. Log cascade action in deletion_log with reason "cascade"
  5. Legal holds on children still block cascade actions
```

## Compilation

```bash
cd 131_RetentionPolicies
lazbuild RetentionPolicies.lpi
./RetentionPolicies
```

## Sample Output

```
1. Policies: 8 categories from 7 days (temp) to 2555 days (transactions)
4. Holds: 3 active holds blocking deletion (legal, finance, compliance)
5. Execution: 7 processed, 3 blocked by holds
7. Audit: 4 deleted, 2 archived, 1 anonymized, 3 held
9. Compliance: 3 overdue records (all held by legal)
10. Status: 13 active, 4 deleted, 3 expired, 3 archived
```

## Related Examples

- **129_DataAnonymization** - PII protection techniques
- **130_ConsentManagement** - GDPR consent tracking

## Best Practices

1. **Define clear policies**: Every data category needs an explicit retention period
2. **Respect legal holds**: Never delete held data regardless of policy
3. **Audit everything**: Log all retention actions with timestamps and reasons
4. **Cascade carefully**: Verify parent-child relationships before cascade deletion
5. **Regular execution**: Run retention checks daily or weekly
6. **Compliance reporting**: Generate reports showing overdue and held records
7. **Hold accountability**: Track who placed holds and require justification
8. **Tiered approach**: Short-term delete, medium-term anonymize, long-term archive
