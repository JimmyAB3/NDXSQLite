# Example 79: Booking and Reservation System

## Overview

This example demonstrates a **comprehensive booking and reservation system** with resource management, time slot conflict detection, approval workflows, waitlist management, and recurring bookings. It showcases patterns used in meeting room booking systems, equipment rentals, and venue reservations.

## Features Demonstrated

### 1. Resource Management
- Multiple resource types (rooms, equipment, vehicles)
- Resource attributes (capacity, location, amenities)
- Hourly rate pricing
- Active/inactive status

### 2. Conflict Detection
- Overlap detection for time slots
- Prevention of double-booking
- Validation before booking creation

### 3. Booking Status Workflow
- Pending, confirmed, cancelled, completed, no-show
- Approval workflow for certain resource types
- Status transition logging

### 4. Waitlist Management
- Queue for fully booked slots
- Priority-based ordering
- Notification tracking

### 5. Recurring Bookings
- Daily, weekly, monthly patterns
- Day of week specification
- Validity period control

### 6. Audit Trail
- Complete booking history
- Status change tracking
- User attribution for all actions

## Database Schema

```
+----------------+     +-----------+     +----------+
| resource_types |<--->| resources |<--->| bookings |
+----------------+     +-----------+     +----------+
| name           |     | type_id   |     | resource_id
| requires_      |     | name      |     | user_id
| approval       |     | capacity  |     | title
| max_duration   |     | location  |     | start_time
+----------------+     | hourly_   |     | end_time
                       | rate      |     | status
                       +-----------+     | total_cost
                             |           +----------+
                             |                |
                             v                v
                       +----------+     +----------------+
                       | waitlist |     | booking_history|
                       +----------+     +----------------+
                       | user_id  |     | booking_id     |
                       | desired_ |     | action         |
                       | start    |     | old_status     |
                       | priority |     | new_status     |
                       +----------+     +----------------+
```

## Key Patterns

### Conflict Detection
```pascal
function CheckConflict(ResourceId: Integer; const StartTime, EndTime: string;
  ExcludeBookingId: Integer = 0): Boolean;
var
  Count: Integer;
begin
  Count := Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM bookings ' +
    'WHERE resource_id = ? ' +
    'AND status IN (''pending'', ''confirmed'') ' +
    'AND id != ? ' +
    'AND start_time < ? AND end_time > ?',
    [ResourceId, ExcludeBookingId, EndTime, StartTime]);
  Result := Count > 0;
end;
```

### Duration Calculation with SQLite
```sql
SELECT (julianday(end_time) - julianday(start_time)) * 24 AS hours
FROM bookings;
```

### Availability Query
```sql
SELECT r.name, COUNT(b.id) AS bookings_today,
  GROUP_CONCAT(substr(start_time, 12, 5) || '-' || substr(end_time, 12, 5)) AS booked_times
FROM resources r
LEFT JOIN bookings b ON r.id = b.resource_id
  AND date(start_time) = '2025-01-20'
  AND status IN ('pending', 'confirmed')
WHERE r.type_id = 1 AND r.is_active = 1
GROUP BY r.id;
```

### Approval Workflow
```pascal
procedure ApproveBooking(BookingId, ApproverId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE bookings SET status = ''confirmed'', approved_by = ?, ' +
    'approved_at = datetime(''now'') WHERE id = ? AND status = ''pending''',
    [ApproverId, BookingId]);

  Conn.ExecuteNonQuery(
    'INSERT INTO booking_history (booking_id, action, old_status, new_status, changed_by) ' +
    'VALUES (?, ''approved'', ''pending'', ''confirmed'', ?)',
    [BookingId, ApproverId]);
end;
```

## Demonstration Sections

1. **Schema Creation** - Booking system tables and indexes
2. **Sample Data** - Resources, users, bookings
3. **Booking List** - Display with status and cost
4. **Availability** - Resource availability view
5. **User Summary** - Per-user booking statistics
6. **Waitlist** - Waitlist management
7. **Cancellation** - Cancel with reason tracking
8. **Recurring** - Recurring booking patterns
9. **History** - Audit log display
10. **Statistics** - Overall booking metrics

## Booking Status Flow

```
           +-----------+
           | (created) |
           +-----+-----+
                 |
     +-----------+-----------+
     |                       |
     v                       v
+----------+           +-----------+
| pending  |---------->| confirmed |
+----------+  approve  +-----+-----+
     |                       |
     |                       +-------+-------+
     |                       |       |       |
     v                       v       v       v
+-----------+          +----------+ +------+ +---------+
| cancelled |          | completed| |no_show| |cancelled|
+-----------+          +----------+ +------+ +---------+
```

## Compilation

```bash
cd 79_BookingReservation
lazbuild BookingReservation.lpi
./BookingReservation
```

## Sample Output

```
3. Booking List Display
   ======================

   Upcoming bookings:
   -------------------------------------------------------------------------------------
   Stat  | Resource        | Title                | Time             | User     | Cost
   -------------------------------------------------------------------------------------
   [OK]  | Room A101       | Sprint Planning      | 01-20 09:00      | Alice Jo | $50.00
   [OK]  | Room A102       | Team Standup         | 01-20 09:30      | Alice Jo | $17.50
   [OK]  | Main Auditorium | Company All-Hands    | 01-21 10:00      | Bob Smit | $300.00
   [?]   | Van V01         | Client Visit Transpo | 01-22 08:00      | Bob Smit | $300.00

7. Booking Cancellation
   ======================

   Cancelling booking #3 (Client Meeting)...
   Booking cancelled with reason: Client rescheduled

   Cancelled bookings:
     [X] Client Meeting (Room A101)
         Reason: Client rescheduled

10. Booking Statistics
    ====================

    Overall statistics:
      Total bookings: 7
      Confirmed: 5
      Pending approval: 1
      Cancelled: 1
      Total revenue: $417.50
```

## Related Examples

- **74_ShoppingCart** - E-commerce transactions
- **75_UserAuthentication** - User management
- **77_RatingReviews** - Service feedback
- **78_NotificationSystem** - Booking notifications

## Production Considerations

1. **Timezone Handling**: Store times in UTC, convert for display
2. **Buffer Time**: Add gaps between bookings for room turnover
3. **Cancellation Policies**: Implement time-based cancellation rules
4. **Capacity Validation**: Check attendees vs. resource capacity
5. **Recurring Conflicts**: Check all instances when creating recurring bookings
6. **Calendar Integration**: Export to iCal/Google Calendar
7. **Notifications**: Send reminders before bookings

## Analytics Queries

```sql
-- Resource utilization rate
SELECT r.name,
  COUNT(b.id) AS total_bookings,
  SUM((julianday(b.end_time) - julianday(b.start_time)) * 24) AS total_hours,
  ROUND(SUM((julianday(b.end_time) - julianday(b.start_time)) * 24) /
    (COUNT(DISTINCT date(b.start_time)) * 8) * 100, 1) AS utilization_pct
FROM resources r
JOIN bookings b ON r.id = b.resource_id
WHERE b.status IN ('confirmed', 'completed')
GROUP BY r.id;

-- Peak booking times
SELECT strftime('%H', start_time) AS hour, COUNT(*) AS bookings
FROM bookings
WHERE status IN ('confirmed', 'completed')
GROUP BY hour
ORDER BY bookings DESC;

-- Cancellation rate by user
SELECT u.display_name,
  COUNT(*) AS total,
  SUM(CASE WHEN b.status = 'cancelled' THEN 1 ELSE 0 END) AS cancelled,
  ROUND(SUM(CASE WHEN b.status = 'cancelled' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 1) AS cancel_rate
FROM users u
JOIN bookings b ON u.id = b.user_id
GROUP BY u.id
HAVING COUNT(*) > 0;

-- Revenue by resource type
SELECT rt.name,
  COUNT(b.id) AS bookings,
  SUM(b.total_cost) AS revenue
FROM resource_types rt
JOIN resources r ON rt.id = r.type_id
JOIN bookings b ON r.id = b.resource_id
WHERE b.status IN ('confirmed', 'completed')
GROUP BY rt.id
ORDER BY revenue DESC;
```
