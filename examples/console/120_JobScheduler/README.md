# Example 120: Job Scheduler - Cron-like Scheduling, Retry, DLQ, Concurrency

## Overview

This example demonstrates a **Job Scheduler** system with cron-like scheduling capabilities. It manages recurring jobs with configurable schedules (interval, hourly, daily, weekly), tracks execution history, implements exponential backoff for failed retries, routes permanently failed jobs to a dead letter queue, and enforces concurrency limits per job group.

## Features Demonstrated

### 1. Register and Schedule Jobs
- Multiple schedule types: interval, hourly, daily, weekly
- Next run time calculation based on schedule
- Concurrency group assignment
- Configurable max retries and backoff

### 2. Execute Jobs (First Run)
- Job execution with result tracking
- Duration measurement
- Automatic next_run recalculation after completion
- Concurrency slot acquisition and release

### 3. Retry with Exponential Backoff
- Failed jobs retry with increasing delays
- Backoff formula: base * 2^(attempt-1)
- Example: 2s -> 4s -> 8s -> 16s...
- Success on retry clears failure state

### 4. Dead Letter Queue (Max Retries Exhausted)
- Jobs exceeding max_retries moved to DLQ
- Preserves: job info, payload, attempt count, last error
- DLQ entries require explicit acknowledgement
- Prevents infinite retry loops

### 5. Concurrency Limit
- Groups of jobs share concurrency slots
- Max concurrent jobs per group enforced
- Blocked jobs reported (not silently dropped)
- Slots released after execution completes

### 6. Execution History
- Complete log of all job runs
- Tracks: status, attempt number, duration, errors
- Supports audit and debugging
- Historical trend analysis

### 7. Next Run Calculation
- Recalculated after each execution
- Schedule-specific logic (interval adds seconds, daily targets hour)
- ISO format for sortable comparison
- Shows last_run and next_run timestamps

### 8. Pause and Resume Jobs
- Job status: active, paused, disabled
- Paused jobs skip execution but retain schedule
- Disabled jobs require re-enablement
- Status changes are immediate

### 9. Acknowledge Dead Letter Queue
- DLQ entries must be explicitly acknowledged
- Tracks acknowledgement timestamp
- Unacknowledged entries = unresolved failures
- Enables operational alerting

### 10. Statistics
- Jobs by status (active, paused, disabled)
- Executions by outcome (completed, failed)
- Success rate percentage
- Average execution duration
- DLQ entry count

## Architecture

```
+------------------+     +------------------+     +------------------+
| Job Registration |     | Scheduler        |     | Executor         |
+------------------+     +------------------+     +------------------+
| RegisterJob()    |---->| CalculateNextRun()|---->| ExecuteJob()     |
| name, type,      |     | Check schedule   |     | Acquire slot     |
| schedule, group  |     | Pick due jobs    |     | Run + record     |
+------------------+     +------------------+     +------------------+
                                                          |
                          +-------------------+-----------+
                          |                   |
                          v                   v
                  +-------------+     +-------------+
                  | Success     |     | Failure     |
                  +-------------+     +-------------+
                  | Update next |     | Retry?      |
                  | Release slot|     | Backoff     |
                  | Log result  |     | or DLQ      |
                  +-------------+     +-------------+
```

## Database Schema

```
+------------------+     +------------------+
| jobs             |     | job_executions   |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| job_name (UNI)   |     | job_id           |
| job_type         |     | job_name         |
| schedule_type    |     | status           |
| interval_seconds |     | attempt          |
| schedule_hour    |     | started_at       |
| schedule_minute  |     | completed_at     |
| schedule_day_of_ |     | duration_ms      |
|   week           |     | result_data      |
| payload          |     | error_message    |
| max_retries      |     | next_retry_at    |
| retry_backoff_   |     +------------------+
|   base           |
| concurrency_group|     +------------------+
| max_concurrent   |     | dead_letter_queue|
| status           |     +------------------+
| next_run_at      |     | id (PK, AUTO)    |
| last_run_at      |     | job_id           |
| created_at       |     | job_name         |
+------------------+     | job_type         |
                          | payload          |
+------------------+      | attempts         |
| concurrency_slots|      | last_error       |
+------------------+      | failed_at        |
| id (PK, AUTO)    |      | acknowledged     |
| concurrency_group|      | acknowledged_at  |
| job_id           |      +------------------+
| execution_id     |
| acquired_at      |
| released_at      |
+------------------+
```

## Job Lifecycle

```
(register) --> active --> executing --> completed --> (reschedule)
                 |                         |
                 v                         v
              paused                    failed
                                          |
                                          v
                                    retrying (backoff)
                                          |
                              +-----------+-----------+
                              |                       |
                              v                       v
                        retry success           max retries
                              |                       |
                              v                       v
                         completed              dead_letter_queue
```

## Schedule Types

| Type | Parameters | Example |
|------|-----------|---------|
| interval | interval_seconds | Every 1800s (30 min) |
| hourly | schedule_minute | At :30 every hour |
| daily | schedule_hour, schedule_minute | At 06:00 daily |
| weekly | schedule_day_of_week, schedule_hour, schedule_minute | Sunday at 02:00 |

## Exponential Backoff

| Attempt | Backoff (base=2s) | Cumulative Wait |
|---------|-------------------|-----------------|
| 1 | 2s | 2s |
| 2 | 4s | 6s |
| 3 | 8s | 14s |
| 4 | 16s | 30s |
| 5 | 32s | 62s |

## Compilation

```bash
cd 120_JobScheduler
lazbuild JobScheduler.lpi
./JobScheduler
```

## Sample Output

```
1. Register: 5 jobs (daily, hourly, interval, weekly schedules)
2. Execute: All 5 jobs complete successfully
3. Retry: hourly-sync fails twice, succeeds on attempt 3 (backoff 2s->4s->8s)
4. DLQ: cleanup-task fails 2/2 retries, moved to dead letter queue
5. Concurrency: 2/2 slots used in "reports" group, 3rd job blocked
6. History: 12 executions (8 completed, 4 failed)
10. Stats: Success rate 66.7%, 1 DLQ entry, avg 160ms duration
```

## Related Examples

- **118_SagaPattern** - Long-running transactions with compensating actions
- **119_IdempotencyKeys** - Exactly-once processing with deduplication

## Best Practices

1. **Idempotent jobs**: Jobs should be safe to re-run (handle duplicates gracefully)
2. **Exponential backoff**: Prevent overwhelming failing services with rapid retries
3. **Dead letter queue**: Don't retry forever; move to DLQ for manual investigation
4. **Concurrency limits**: Prevent resource exhaustion from parallel job execution
5. **Execution history**: Log all attempts for debugging and capacity planning
6. **Schedule recalculation**: Always compute next_run after execution, not before
7. **Pause vs disable**: Pause for temporary stops, disable for permanent decommission
8. **Acknowledge DLQ**: Require explicit acknowledgement to track unresolved failures
