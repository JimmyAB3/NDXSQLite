# Example 137: Alerting Rules - Threshold Monitoring, Escalation, Suppression

## Overview

This example demonstrates a **Rule-Based Alerting System** with threshold monitoring, alert state lifecycle (open/acknowledged/resolved/suppressed), time-based escalation, and maintenance suppression windows. It simulates a production monitoring system handling CPU, memory, latency, disk, and error rate alerts.

## Features Demonstrated

### 1. Alert Rules Configuration
- 10 rules across 3 targets (server, API gateway, database)
- Configurable threshold, operator (gt/lt), severity, and target
- Metrics: cpu_load, memory_pct, response_ms, disk_free_gb, error_rate_pct, connections, disk_usage_pct
- Severity levels: warning, critical, emergency

### 2. Incoming Metric Readings
- 16 readings simulating a production incident timeline
- CPU spike pattern: 45% -> 75% -> 82% -> 93% -> 88% -> 55% (recovery)
- Latency spike: 120ms -> 650ms -> 2500ms -> 180ms (recovery)
- Memory creep: 62% -> 78% -> 83% -> 96% (no recovery)
- Emergency: disk free drops to 8.5GB (threshold 10GB)

### 3. Generated Alerts
- 9 alerts triggered from threshold violations
- States: 5 resolved, 1 acknowledged, 2 open, 1 suppressed
- Actual vs. threshold values tracked per alert

### 4. Alert State Lifecycle
- Detailed lifecycle of Alert #2 (CPU critical):
  - opened -> acknowledged (2 min) -> resolved (15 min)
- State transitions with actor and notes
- Time-to-acknowledge and time-to-resolve metrics

### 5. Escalation Rules
- 3 escalation tiers with configurable timeouts:
  - warning -> critical (30 min, pager)
  - critical -> emergency (15 min, phone)
  - emergency -> all_hands (5 min, broadcast)
- Disk space alert escalated (emergency unresolved for 5 min)

### 6. Suppression Windows
- 2 maintenance windows with target patterns:
  - Prod Maintenance: `server-prod%` (kernel update)
  - DB Migration: `db-%` (schema migration)
- Pattern matching with SQL LIKE wildcards
- Alert #9 suppressed (fired during maintenance window)

### 7. Active Alerts Dashboard
- Currently actionable alerts (open + acknowledged)
- Sorted by severity (emergency first)
- Escalation status indicator

### 8. Alert History Timeline
- Chronological log of all state transitions
- 18 events covering the full incident lifecycle
- Actor tracking (system, oncall-engineer, sre-team, auto-resolve)

### 9. Alert Statistics
- By state: resolved(5), open(2), suppressed(1), acknowledged(1)
- By severity: emergency(1), critical(4), warning(4)
- By target: server-prod-01(5), api-gateway(3), db-primary(1)
- Resolution metrics: 3 auto-resolved, 2 manually resolved

### 10. Rule Effectiveness
- Per-rule fire count, resolution, escalation, suppression stats
- 2 rules with 0 fires (conn_pool, disk_warning)
- Identifies over/under-sensitive rules

## Architecture

```
+------------------+     +------------------+     +------------------+
| Rule Engine      |     | Alert Management |     | Reporting        |
+------------------+     +------------------+     +------------------+
| 10 threshold     |---->| State machine    |---->| Dashboard        |
| rules defined    |     | open/ack/resolve |     | Timeline         |
| gt/lt operators  |     | Escalation       |     | Statistics       |
+------------------+     | Suppression      |     | Effectiveness    |
        |                +------------------+     +------------------+
        v                         |
+------------------+     +------------------+
| metric_readings  |     | alert_history    |
+------------------+     +------------------+
| Real-time values |     | State transitions|
| 16 data points   |     | Actor + notes    |
+------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| alert_rules      |     | alerts           |     | alert_history    |
+------------------+     +------------------+     +------------------+
| name (UQ)        |     | rule_name (FK)   |     | alert_id (FK)    |
| metric           |     | target           |     | action           |
| operator (gt/lt) |     | severity         |     | performed_by     |
| threshold        |     | metric_value     |     | timestamp        |
| severity         |     | threshold_value  |     | notes            |
| target           |     | state            |     +------------------+
| enabled          |     | opened_at        |
+------------------+     | acknowledged_at  |     +------------------+
                          | resolved_at      |     | suppression      |
+------------------+     +------------------+     |   _windows       |
| metric_readings  |                               +------------------+
+------------------+     +------------------+     | name             |
| metric           |     | escalation_rules |     | target_pattern   |
| target           |     +------------------+     | start/end_time   |
| value            |     | from/to_severity |     | reason           |
| timestamp        |     | after_minutes    |     +------------------+
+------------------+     | notify_channel   |
                          | description      |
                          +------------------+
```

## Alert State Machine

```
                    +----------+
                    |  OPEN    |
                    +----+-----+
                         |
            +------------+------------+
            |                         |
            v                         v
    +-------+------+         +--------+-------+
    | ACKNOWLEDGED |         |  SUPPRESSED    |
    +-------+------+         | (maintenance)  |
            |                +----------------+
            v
    +-------+------+
    |   RESOLVED   |
    +-------+------+
            |
            v  (if unresolved)
    +-------+------+
    |  ESCALATED   |
    +--------------+
```

## Escalation Chain

| From | To | Timeout | Channel | Use Case |
|------|-----|---------|---------|----------|
| warning | critical | 30 min | pager | Unacknowledged warning |
| critical | emergency | 15 min | phone | Unacknowledged critical |
| emergency | all_hands | 5 min | broadcast | Unresolved emergency |

## Compilation

```bash
cd 137_AlertingRules
lazbuild AlertingRules.lpi
./AlertingRules
```

## Sample Output

```
1. Rules: 10 active rules (CPU, memory, disk, latency, errors)
2. Metrics: 16 readings simulating production incident
3. Alerts: 9 generated (5 resolved, 2 open, 1 ack, 1 suppressed)
4. Lifecycle: open -> acknowledged (2min) -> resolved (15min)
5. Escalation: disk_space_low escalated after 5min unresolved
6. Suppression: 1 alert suppressed during maintenance window
7. Dashboard: 3 active alerts requiring action
8. Timeline: 18 state transitions chronologically
9. Stats: By state/severity/target + resolution metrics
10. Effectiveness: Per-rule fire/resolve/escalate/suppress counts
```

## Related Examples

- **135_SensorDataStore** - High-frequency sensor ingestion
- **136_Downsampling** - Data reduction and time bucketing

## Best Practices

1. **Severity levels**: Use at least 3 levels (warning/critical/emergency) for graduated response
2. **Escalation timeouts**: Shorter for higher severities (30m/15m/5m)
3. **Auto-resolve**: Automatically close alerts when metrics return to normal
4. **Suppression windows**: Prevent alert noise during planned maintenance
5. **Pattern matching**: Use wildcards in suppression targets for flexible maintenance scopes
6. **Actor tracking**: Log who acknowledged/resolved each alert for accountability
7. **Rule effectiveness**: Monitor fire counts to tune thresholds (too many = noisy, too few = blind spots)
8. **Dashboard priority**: Sort active alerts by severity for triage efficiency
