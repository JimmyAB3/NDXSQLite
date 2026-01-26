# Example 89: User Activity Tracking

## Overview

This example demonstrates a comprehensive **user activity tracking system** using SQLite. It implements session management, page view tracking, event logging, feature usage analytics, engagement scoring, user segmentation, and retention analysis.

## Features Demonstrated

### 1. Session Tracking
- Login/logout with session tokens
- Duration tracking per session
- IP address and user agent capture
- Session count per user

### 2. Page View Tracking
- URL and title recording
- Time-on-page measurement
- Unique visitors per page
- Most-visited page ranking

### 3. Activity Events
- Event type classification (click, navigation, form_submit, api_call)
- Event name and metadata
- Daily event trends
- Per-user event history

### 4. Feature Usage Analytics
- Feature popularity ranking
- Adoption percentage calculation
- Usage breakdown by plan tier
- Unique user counts per feature

### 5. DAU / WAU / MAU Metrics
- Daily Active Users (today)
- Weekly Active Users (7-day window)
- Monthly Active Users (30-day window)
- Stickiness ratio (DAU/MAU)

### 6. Engagement Scoring
- Weighted scoring formula:
  - Sessions: 10 points each
  - Page views: 2 points each
  - Events: 5 points each
  - Duration: 1 point/minute (max 100)
- Daily summary materialization
- Ranking by plan tier

### 7. User Segmentation
- Activity-based segments (Power User, Active, Casual, Dormant)
- Platform distribution analysis
- Segment metrics comparison

### 8. Retention Analysis
- Day 1, Day 7, Day 30 retention rates
- Weekly activity heatmap (sessions per day of week)

### 9. Activity Timeline
- Unified chronological view per user
- Combines sessions, page views, events, and feature usage

## Database Schema

```
+------------------+     +--------------------+
| users            |     | sessions           |
+------------------+     +--------------------+
| id (PK)          |---->| user_id (FK)       |
| username         |     | session_token      |
| email            |     | started_at         |
| plan             |     | ended_at           |
| created_at       |     | duration_seconds   |
+------------------+     | ip_address         |
                         | user_agent         |
                         +--------------------+
        |
        +---->+--------------------+
        |     | activity_events    |
        |     +--------------------+
        |     | user_id (FK)       |
        |     | session_id (FK)    |
        |     | event_type         |
        |     | event_name         |
        |     | event_data         |
        |     | page_url           |
        |     | created_at         |
        |     +--------------------+
        |
        +---->+--------------------+
        |     | page_views         |
        |     +--------------------+
        |     | user_id (FK)       |
        |     | session_id (FK)    |
        |     | page_url           |
        |     | page_title         |
        |     | referrer           |
        |     | duration_seconds   |
        |     | created_at         |
        |     +--------------------+
        |
        +---->+--------------------+
        |     | feature_usage      |
        |     +--------------------+
        |     | user_id (FK)       |
        |     | feature_name       |
        |     | action             |
        |     | metadata           |
        |     | created_at         |
        |     +--------------------+
        |
        +---->+-------------------------+
              | daily_activity_summary  |
              +-------------------------+
              | user_id (FK)            |
              | activity_date           |
              | sessions_count          |
              | page_views_count        |
              | events_count            |
              | total_duration_seconds  |
              | engagement_score        |
              +-------------------------+
```

## Key Patterns

### Session Creation
```pascal
Conn.ExecuteNonQuery(
  'INSERT INTO sessions (user_id, session_token, started_at, duration_seconds, ip_address) ' +
  'VALUES (?, ?, datetime(''now''), ?, ?)',
  [UserId, Token, Duration, IP]);
```

### DAU/WAU/MAU Queries
```pascal
DAU := Conn.ExecuteScalar(
  'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
  'WHERE date(started_at) = date(''now'')');

WAU := Conn.ExecuteScalar(
  'SELECT COUNT(DISTINCT user_id) FROM sessions ' +
  'WHERE started_at >= datetime(''now'', ''-7 days'')');
```

### Engagement Score Calculation
```pascal
// Weighted formula materialized into daily summary
engagement_score = sessions * 10 + page_views * 2 + events * 5 + MIN(duration_min, 100)
```

### User Segmentation
```pascal
DS := Conn.ExecuteQuery(
  'SELECT CASE ' +
  '  WHEN total_sessions >= 10 THEN ''Power User'' ' +
  '  WHEN total_sessions >= 5 THEN ''Active'' ' +
  '  WHEN total_sessions >= 2 THEN ''Casual'' ' +
  '  ELSE ''Dormant'' END as segment, ...');
```

## Compilation

```bash
cd 89_UserActivityTracking
lazbuild UserActivityTracking.lpi
./UserActivityTracking
```

## Sample Output

```
1. Creating Schema
   Tables created: users, sessions, activity_events, page_views, feature_usage, daily_activity_summary
   Indexes created: 7

4. Page View Analysis
   Total page views: 88
   Most visited pages:
     /dashboard: 17 views, 8 users, avg 66s
     /profile: 14 views, 8 users, avg 83s

6. Feature Usage Analysis
   Feature popularity:
     export: 17 uses, 9 users (90.0% adoption)
     share: 14 uses, 7 users (70.0% adoption)

7. DAU / WAU / MAU Metrics
   DAU (today): 1
   WAU (7 days): 10
   MAU (30 days): 10
   Stickiness (DAU/MAU): 10.0%

8. User Engagement Scoring
   User engagement (avg daily score):
     bob (pro): score=60.0, 8 active days, 10 sessions

9. User Segmentation
   Activity segments:
     Power User: 6 users, avg 11.0 sessions
     Active: 4 users, avg 5.5 sessions
```

## Related Examples

- **88_APIRateLimiting** - Rate limiting patterns
- **90_FunnelAnalysis** - Conversion funnel analysis
- **91_CohortAnalysis** - Cohort analysis

## Best Practices

1. **Indexing**: Index on user_id and timestamps for fast activity lookups
2. **Materialization**: Pre-compute daily summaries for engagement scoring
3. **Segmentation**: Use CASE expressions for dynamic user grouping
4. **Stickiness**: DAU/MAU ratio indicates product engagement quality
5. **Retention**: Track return visits relative to first activity
6. **Timeline**: Use UNION ALL across tables for unified activity view
7. **Scoring**: Weight actions by business value (events > views)
8. **Platforms**: Track user_agent for device/browser analytics
