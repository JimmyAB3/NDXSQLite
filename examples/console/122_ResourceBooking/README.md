# Example 122: Resource Booking - Pools, Availability, Waitlists, Cancellation

## Overview

This example demonstrates a **Resource Booking** system with pool-based allocation, overlap detection, waitlist management, and cancellation with automatic promotion. It supports multiple resource types (rooms, vehicles, equipment), prevents double-booking through time-slot overlap detection, maintains priority-ordered waitlists, and automatically promotes waitlisted requests when bookings are cancelled.

## Features Demonstrated

### 1. Setup Resource Pools
- Resource pools group equivalent resources (rooms, vehicles, equipment)
- Individual resources with capacity and attributes
- Pool-based allocation (any available in group)
- Resource metadata (projector, video_conf, etc.)

### 2. Book Resources
- Direct resource booking by ID
- Booking with date, time range, and purpose
- Confirmed status on successful booking
- Multiple resource types (rooms, cars, projectors)

### 3. Overlap Detection
- Prevents double-booking same resource
- Detects partial overlaps (start during existing booking)
- Detects full overlaps (exact same slot)
- Adjacent bookings allowed (end=start of next)
- Different dates don't conflict

### 4. Pool-Based Booking
- Auto-assigns first available resource in pool
- Skips already-booked resources
- Returns assigned resource ID
- Reports when pool is fully booked

### 5. Waitlist
- Priority-ordered queue (higher number = more priority)
- FIFO within same priority level
- Waitlist per pool, date, and time slot
- Status tracking (waiting, promoted, expired, cancelled)

### 6. Cancellation with Waitlist Promotion
- Cancel bookings with reason tracking
- Automatic promotion of highest-priority waitlist entry
- Promoted entry gets new booking created
- Remaining waitlist entries preserved

### 7. Time Slot Availability
- View all bookings for a resource on a date
- Shows confirmed and cancelled bookings
- Cross-resource view for a date
- Helps users find free slots

### 8. Resource Maintenance
- Resources can be set to maintenance/retired status
- Maintenance resources skipped during pool allocation
- Status-based filtering in availability queries
- Restore to available when maintenance complete

### 9. Multi-Day Booking Pattern
- Book same resource across multiple consecutive days
- Each day checked independently for overlaps
- Blocks resource for entire duration
- Other users blocked on any day in range

### 10. Statistics
- Bookings by status (confirmed, cancelled)
- Waitlist by status (waiting, promoted)
- Resource utilization (bookings per resource per day)
- Cancellation rate percentage

## Architecture

```
+------------------+     +------------------+     +------------------+
| Resource Pools   |     | Booking Engine   |     | Waitlist Manager |
+------------------+     +------------------+     +------------------+
| CreatePool()     |     | BookResource()   |     | AddToWaitlist()  |
| CreateResource() |     | HasOverlap()     |     | Priority queue   |
| FindAvailable()  |<--->| BookFromPool()   |<--->| PromoteFrom()    |
+------------------+     +------------------+     +------------------+
                                  |
                          +-------+-------+
                          |               |
                          v               v
                  +-------------+  +-------------+
                  | Overlap     |  | Cancellation|
                  | Detection   |  | Handler     |
                  +-------------+  +-------------+
                  | Time range  |  | CancelBook()|
                  | comparison  |  | Release slot|
                  | Same date   |  | Trigger     |
                  | Same resource|  | promotion   |
                  +-------------+  +-------------+
```

## Database Schema

```
+------------------+     +------------------+
| resource_pools   |     | resources        |
+------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |
| pool_id (UNI)    |<--->| resource_id (UNI)|
| pool_name        |     | resource_name    |
| pool_type        |     | pool_id (FK)     |
| description      |     | capacity         |
+------------------+     | status           |
                          | attributes       |
+------------------+      +------------------+
| bookings         |
+------------------+      +------------------+
| id (PK, AUTO)    |      | waitlist         |
| booking_id (UNI) |      +------------------+
| resource_id      |      | id (PK, AUTO)    |
| booked_by        |      | waitlist_id (UNI)|
| booking_date     |      | pool_id          |
| start_time       |      | requested_by     |
| end_time         |      | requested_date   |
| purpose          |      | start_time       |
| status           |      | end_time         |
| cancelled_at     |      | purpose          |
| cancel_reason    |      | priority         |
+------------------+      | status           |
                           | promoted_to_     |
                           |   booking        |
                           +------------------+
```

## Booking Lifecycle

```
(request) --> check overlap --> [no overlap] --> confirmed
                   |                                 |
                   v                                 v
              [overlap]                        completed
                   |                                 |
                   v                                 v
              waitlist                         cancelled
              (priority)                             |
                   |                                 v
                   v                         promote waitlist
              promoted -----> confirmed
```

## Overlap Detection Logic

```
Overlap exists when:
  existing.start_time < new.end_time
  AND existing.end_time > new.start_time
  AND same resource_id
  AND same booking_date
  AND existing.status = 'confirmed'

No overlap:
  [===existing===]  [===new===]     (adjacent: end = start)
  [===new===]  [===existing===]     (before)

Overlap:
  [===existing===]
       [===new===]                  (starts during)
  [===new===]                       (ends during)
     [=new=]                        (fully inside)
```

## Compilation

```bash
cd 122_ResourceBooking
lazbuild ResourceBooking.lpi
./ResourceBooking
```

## Sample Output

```
1. Setup: 3 pools, 10 resources (4 rooms, 3 vehicles, 3 equipment)
2. Book: 5 direct bookings (rooms, car, projector)
3. Overlap: 2 blocked, 2 allowed (adjacent + different date)
4. Pool: auto-assigns room-d, room-c; pool full after 4 bookings
5. Waitlist: 3 entries (priority 5, 1, 0)
6. Cancel+Promote: Alice cancelled, mia (priority 5) promoted
7. Availability: Room A has 3 confirmed bookings on Feb 5
8. Maintenance: room-c skipped, room-d assigned instead
9. Multi-day: car-002 blocked for 3 days, overlap on day 2
10. Stats: 14 bookings (13 confirmed), 7.1% cancellation rate
```

## Related Examples

- **120_JobScheduler** - Cron-like job scheduling with retry and DLQ
- **121_CalendarEvents** - Recurring events with exceptions and conflict detection

## Best Practices

1. **Overlap check before insert**: Always verify no conflicting booking exists before confirming
2. **Pool-based allocation**: Let the system assign from a pool instead of requiring specific resources
3. **Priority waitlist**: Higher-priority requests get promoted first when slots free up
4. **Cancellation cascade**: Automatically promote waitlisted requests when bookings cancel
5. **Maintenance status**: Remove resources from pool without deleting data
6. **Atomic operations**: Check + book should be atomic to prevent race conditions
7. **Multi-day patterns**: Treat each day independently for overlap but book as a logical group
8. **Audit trail**: Keep cancelled bookings with reason for reporting and analysis
