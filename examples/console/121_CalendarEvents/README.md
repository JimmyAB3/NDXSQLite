# Example 121: Calendar Events - Recurring Patterns, Exceptions, Views, Conflicts

## Overview

This example demonstrates a **Calendar Events** system with RRULE-like recurring event patterns. It supports multiple recurrence types (daily, weekly, monthly), exception handling (cancel or modify specific occurrences), calendar views by day/week/month, overlap-based conflict detection, multi-calendar separation, and count-limited recurrences.

## Features Demonstrated

### 1. Create Calendar Events
- Single events (one-time occurrence)
- Daily recurring events with end date
- Weekly events on specific days (Mon+Thu, Tue+Thu+Sat)
- Monthly events on a specific day of month
- Count-limited recurrences (max N occurrences)
- Multi-calendar support (work, personal)

### 2. Generate Occurrences
- Expand recurrence rules into individual occurrences
- Materialized occurrences for efficient querying
- Range-based generation (from/to dates)
- Respects recurrence end dates and count limits

### 3. Exceptions and Overrides
- Cancel specific occurrences (holidays, offsites)
- Modify occurrences (change time, location)
- Exceptions stored separately from rules
- Regeneration respects all exceptions

### 4. Day View
- All events for a specific date
- Sorted by start time
- Shows location for each event
- Marks cancelled events

### 5. Week View
- Events grouped by day across 7 days
- Compact display format
- Skips days with no events
- Shows day-of-week labels

### 6. Month View Summary
- Event count per day for the entire month
- Identifies busy/free days at a glance
- Excludes cancelled events from count
- Sorted chronologically

### 7. Conflict Detection
- Detects time overlaps between events
- Same-calendar conflicts only (work vs personal independent)
- Records overlap start/end times
- Ignores cancelled events

### 8. Multi-Calendar View
- Events separated by calendar (work, personal)
- Per-calendar occurrence counts
- Cross-calendar events on same day
- No cross-calendar conflict detection

### 9. Limited Recurrence (Count-based)
- Events with maximum occurrence count
- Stops generating after N occurrences
- Works within any date range
- Independent of end-date-based limits

### 10. Statistics
- Total events (recurring vs single)
- Active occurrences count
- Exceptions breakdown (cancelled/modified)
- Conflict count
- Busiest day identification
- Calendar count

## Architecture

```
+------------------+     +------------------+     +------------------+
| Event Definition |     | Occurrence Gen.  |     | Calendar Views   |
+------------------+     +------------------+     +------------------+
| CreateEvent()    |---->| GenerateOccur()  |---->| Day/Week/Month   |
| title, schedule  |     | Expand rules     |     | Filtered queries |
| recurrence rule  |     | Apply exceptions |     | Sorted display   |
+------------------+     +------------------+     +------------------+
                                  |
                          +-------+-------+
                          |               |
                          v               v
                  +-------------+  +-------------+
                  | Exceptions  |  | Conflicts   |
                  +-------------+  +-------------+
                  | cancelled   |  | DetectConf()|
                  | modified    |  | Overlap     |
                  | per-date    |  | Same calendar|
                  +-------------+  +-------------+
```

## Database Schema

```
+------------------+     +------------------+
| events           |     | event_exceptions |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| event_id (UNI)   |<--->| event_id         |
| title            |     | exception_date   |
| description      |     | exception_type   |
| location         |     | new_title        |
| start_date       |     | new_start_time   |
| start_time       |     | new_end_time     |
| end_time         |     | new_location     |
| duration_minutes |     | reason           |
| is_recurring     |     | UNI(event,date)  |
| recurrence_type  |     +------------------+
| recurrence_      |
|   interval       |     +------------------+
| recurrence_days  |     | event_occurrences|
| recurrence_      |     +------------------+
|   day_of_month   |     | id (PK, AUTO)    |
| recurrence_      |     | event_id         |
|   end_date       |     | occurrence_date  |
| recurrence_count |     | start_time       |
| calendar_id      |     | end_time         |
+------------------+     | title            |
                          | location         |
+------------------+      | is_exception     |
| event_conflicts  |      | is_cancelled     |
+------------------+      | calendar_id      |
| id (PK, AUTO)    |      | UNI(event,date)  |
| event_id_1       |      +------------------+
| event_id_2       |
| conflict_date    |
| overlap_start    |
| overlap_end      |
+------------------+
```

## Recurrence Types

| Type | Parameters | Example |
|------|-----------|---------|
| daily | interval | Every day (interval=1) or every 2 days (interval=2) |
| weekly | interval, days | Every Mon+Thu (days="1,4") |
| monthly | interval, day_of_month | 15th of each month |
| yearly | interval, start_date | Same date each year |

## Exception Types

| Type | Effect | Example |
|------|--------|---------|
| cancelled | Occurrence removed | Holiday, offsite day |
| modified | Time/location changed | Room maintenance |

## Compilation

```bash
cd 121_CalendarEvents
lazbuild CalendarEvents.lpi
./CalendarEvents
```

## Sample Output

```
1. Create: 6 events (1 single, 5 recurring) across 2 calendars
2. Generate: 52 occurrences in February 2026
3. Exceptions: 2 cancelled, 1 modified (time+location change)
4. Day View: 3 events on Thu Feb 5
5. Week View: Feb 2-8, grouped by day with time slots
6. Month View: 27 days with events, busiest = 5 events
7. Conflicts: 2 detected (Standup/Client Call, Meeting/Workshop)
8. Multi-calendar: work (40) + personal (12) occurrences
9. Limited: Training Course = 3 of 5 occurrences in Feb range
10. Statistics: 52 active, 2 cancelled, 2 conflicts
```

## Related Examples

- **120_JobScheduler** - Cron-like job scheduling with retry and DLQ
- **122_ResourceBooking** - Time slot availability and booking

## Best Practices

1. **Materialize occurrences**: Expand recurrence rules into queryable rows for fast calendar views
2. **Exceptions over deletion**: Never delete occurrences; mark as cancelled or modified for audit trail
3. **Range-based generation**: Only generate occurrences within needed date ranges to limit storage
4. **Same-calendar conflicts**: Only detect conflicts within the same calendar context
5. **ISO date storage**: Store dates as ISO strings for lexicographic comparison and portability
6. **Recurrence end conditions**: Support both end-date and count-based termination
7. **Regeneration**: When exceptions change, regenerate affected event occurrences
8. **Calendar separation**: Keep personal/work events independent for privacy and conflict rules
