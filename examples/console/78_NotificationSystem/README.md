# Example 78: Notification System

## Overview

This example demonstrates a **comprehensive notification system** with multiple notification types, read/unread tracking, categories, priorities, push queue management, and user preferences. It showcases patterns used in modern web and mobile applications for user engagement and communication.

## Features Demonstrated

### 1. Notification Types
- Info, success, warning, and error types
- Visual distinction with icons
- Type-based filtering

### 2. Read/Unread Status
- Automatic unread count tracking
- Mark as read functionality
- Batch "mark all read" operation
- Read timestamp tracking

### 3. Notification Categories
- System, social, orders, security, marketing
- Category-based filtering
- Category colors and icons

### 4. Priority Levels
- Low (0), Normal (1), High (2), Urgent (3)
- Priority-based filtering
- Priority-based sorting

### 5. Push Notification Queue
- Multi-channel delivery (push, email, SMS)
- Status tracking (pending, sent, failed, cancelled)
- Retry support with attempt counting

### 6. User Preferences
- Per-category notification settings
- Channel enable/disable (email, push, in-app)
- Minimum priority thresholds

### 7. Notification Grouping
- Group key for related notifications
- Aggregated display support
- Group-based operations

## Database Schema

```
+---------+     +----------------------+     +------------------+
| users   |<--->| notifications        |<--->| push_queue       |
+---------+     +----------------------+     +------------------+
| id      |     | user_id              |     | notification_id  |
| username|     | category_id          |     | channel          |
| email   |     | notification_type    |     | status           |
| unread_ |     | priority             |     | attempts         |
| count   |     | title/body           |     | scheduled_for    |
+---------+     | is_read/is_archived  |     +------------------+
     |          | group_key            |
     |          +----------------------+
     |                   |
     v                   v
+------------------+  +----------------------+
| user_preferences |  | notification_        |
+------------------+  | categories           |
| user_id          |  +----------------------+
| category_id      |  | name                 |
| email_enabled    |  | description          |
| push_enabled     |  | icon/color           |
| minimum_priority |  +----------------------+
+------------------+
```

## Key Patterns

### Denormalized Unread Count
```pascal
procedure UpdateUnreadCount(UserId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE users SET unread_count = ' +
    '  (SELECT COUNT(*) FROM notifications WHERE user_id = ? AND is_read = 0 AND is_deleted = 0) ' +
    'WHERE id = ?', [UserId, UserId]);
end;
```

### Priority Filtering
```sql
SELECT * FROM notifications
WHERE priority >= 2 AND is_deleted = 0
ORDER BY priority DESC, created_at DESC;
```

### Category Aggregation
```sql
SELECT nc.name, COUNT(*) AS total,
  SUM(CASE WHEN n.is_read = 0 THEN 1 ELSE 0 END) AS unread
FROM notifications n
JOIN notification_categories nc ON n.category_id = nc.id
WHERE n.is_deleted = 0
GROUP BY nc.id;
```

### Push Queue Management
```sql
-- Queue a notification for delivery
INSERT INTO push_queue (notification_id, channel) VALUES (?, ?);

-- Get pending notifications for processing
SELECT * FROM push_queue WHERE status = 'pending'
ORDER BY scheduled_for;

-- Update delivery status
UPDATE push_queue SET status = 'sent', sent_at = datetime('now')
WHERE id = ?;
```

## Demonstration Sections

1. **Schema Creation** - Notification system tables
2. **Sample Data** - Users, categories, notifications
3. **Inbox Display** - User notification inbox
4. **Filtering** - By category, priority, read status
5. **Grouping** - Category and key-based grouping
6. **Push Queue** - Delivery queue status
7. **Batch Operations** - Mark all read, archive, cancel
8. **User Preferences** - Channel and priority settings
9. **Statistics** - Overall notification metrics

## Notification Types

| Type | Icon | Usage |
|------|------|-------|
| info | [i] | General information |
| success | [+] | Successful actions |
| warning | [!] | Warnings requiring attention |
| error | [X] | Errors or critical issues |

## Priority Levels

| Level | Name | Use Case |
|-------|------|----------|
| 0 | Low | Marketing, promotions |
| 1 | Normal | Social interactions, updates |
| 2 | High | Order updates, important info |
| 3 | Urgent | Security alerts, critical errors |

## Compilation

```bash
cd 78_NotificationSystem
lazbuild NotificationSystem.lpi
./NotificationSystem
```

## Sample Output

```
3. Notification Inbox Display
   ============================

   Unread notification counts:
     Alice Johnson: 4 unread
     Bob Smith: 3 unread
     Carol White: 2 unread

   Alice's Notification Inbox:
   ----------------------------------------------------------------------
   [!] * [security] Unusual login detected (Urgent)
       We noticed a login from a new device in Seattle, WA.
   [+] * [orders] Order #1234 shipped! (High)
       Your order is on its way. Expected delivery: Tomorrow.
   [i] * [social] Bob started following you
       You have a new follower! Check out their profile.
   [i]   [social] Carol liked your post
       Carol liked your post "My weekend adventure".

6. Push Notification Queue
   =========================

   ID | Channel | Status  | Notification
   ---|---------|---------|----------------------------------
    1 | push    | pending | Bob started following you
    2 | push    | pending | Order #1234 shipped!
    3 | email   | pending | Order #1234 shipped!
    4 | push    | pending | Unusual login detected

8. User Notification Preferences
   ===============================

   User        | Category   | Email | Push | In-App | Min Priority
   ------------|------------|-------|------|--------|-------------
   Alice       | marketing  | No    | No   | Yes    | High
   Bob Smith   | marketing  | No    | Yes  | Yes    | Low
```

## Related Examples

- **75_UserAuthentication** - User identification
- **76_TaggingSystem** - Content categorization
- **77_RatingReviews** - User feedback systems
- **79_BookingReservation** - Reservation notifications

## Production Considerations

1. **Real-time Delivery**: WebSocket or SSE for instant notifications
2. **Rate Limiting**: Prevent notification spam
3. **Batching**: Group similar notifications (e.g., "5 new likes")
4. **TTL/Expiration**: Auto-expire old notifications
5. **Delivery Tracking**: Monitor open rates and engagement
6. **Unsubscribe Handling**: Respect user preferences and legal requirements
7. **Localization**: Multi-language notification content

## Analytics Queries

```sql
-- Notification engagement rate
SELECT nc.name,
  COUNT(*) AS total,
  SUM(CASE WHEN is_read = 1 THEN 1 ELSE 0 END) AS read_count,
  ROUND(SUM(CASE WHEN is_read = 1 THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 1) AS read_rate
FROM notifications n
JOIN notification_categories nc ON n.category_id = nc.id
GROUP BY nc.id;

-- Push delivery success rate
SELECT channel,
  COUNT(*) AS total,
  SUM(CASE WHEN status = 'sent' THEN 1 ELSE 0 END) AS sent,
  SUM(CASE WHEN status = 'failed' THEN 1 ELSE 0 END) AS failed
FROM push_queue
GROUP BY channel;

-- User notification volume
SELECT u.display_name,
  COUNT(*) AS total_received,
  SUM(CASE WHEN is_read = 0 THEN 1 ELSE 0 END) AS unread
FROM users u
JOIN notifications n ON u.id = n.user_id
WHERE n.is_deleted = 0
GROUP BY u.id
ORDER BY total_received DESC;
```
