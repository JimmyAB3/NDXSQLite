# Example 128: Search Analytics - Query Logs, CTR, Refinements, Funnels

## Overview

This example demonstrates a **Search Analytics** system that tracks search behavior, measures click-through rates, analyzes refinement patterns, and builds conversion funnels. It simulates an analytics engine for understanding how users search and interact with results.

## Features Demonstrated

### 1. Query Logs
- 24 queries across 10 sessions by 6 users
- Tracks result count, response time, timestamp
- Session-based grouping
- Per-user query history

### 2. Zero-Result Queries
- Identifies searches returning no results
- Groups by session to find frustrated users
- Highlights product gaps (holographic display, quantum computer kit)
- Zero-result rate calculation (20.8%)

### 3. Click-Through Rates
- Overall CTR: 63.2% of queries with results get clicks
- Position analysis: #1 gets most clicks (8), with 71s avg dwell
- Dwell time as engagement proxy
- Top clicked results ranking

### 4. Search Refinement Patterns
- 14 refinements across sessions
- Types: narrow (8), rephrase (5), broaden (1)
- Sequence tracking (1st, 2nd, 3rd refinement)
- Shows user search journey evolution

### 5. Query Distribution
- Result count ranges: 0, 1-5, 6-20, 21-50, 50+
- Word count analysis: 1-word queries avg 75 results, 4-word avg 6
- Visual bar charts
- More words = more specific = fewer results

### 6. Response Time Analysis
- Average: 94ms, Range: 65-150ms
- Correlation: more results = slower response
- Slow query identification (>100ms)
- Performance by result count bucket

### 7. Search Sessions
- Full session timeline (start, end, converted)
- Query and click counts per session
- Conversion tracking
- Avg 2.4 queries/session, 1.2 clicks/session

### 8. Top Performing Queries
- CTR per query (clicks / results)
- "logitech mx master": 33.3% CTR (specific = effective)
- High dwell time indicates content quality
- Queries leading to conversions

### 9. Conversion Funnel
- 4-stage funnel: Sessions -> Results -> Clicks -> Conversions
- 10 -> 8 -> 7 -> 6 (progressive filtering)
- Drop-off analysis at each stage
- Visual funnel with proportional bars

### 10. Statistics
- Per-user summary (queries, zero-results, avg results)
- Search quality metrics
- Overall system health indicators

## Architecture

```
+------------------+     +------------------+     +------------------+
| Search Events    |     | Analytics Engine  |     | Insights         |
+------------------+     +------------------+     +------------------+
| Queries          |---->| Aggregation      |---->| CTR              |
| Clicks           |     | Correlation      |     | Funnels          |
| Sessions         |     | Pattern Detection|     | Drop-offs        |
| Refinements      |     | Funnel Building  |     | Quality Metrics  |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| query_logs       |     | click_events     |
+------------------+     +------------------+
| id (PK, AUTO)    |<----| query_log_id (FK)|
| session_id       |     | result_position  |
| user_id          |     | result_title     |
| query_text       |     | clicked_at       |
| result_count     |     | dwell_time_sec   |
| response_time_ms |     +------------------+
| page_number      |
| searched_at      |     +------------------+
+------------------+     | search_sessions  |
                          +------------------+
+------------------+     | session_id (UQ)  |
| refinements      |     | user_id          |
+------------------+     | query_count      |
| session_id       |     | click_count      |
| original_query   |     | started_at       |
| refined_query    |     | ended_at         |
| refinement_type  |     | converted        |
| sequence_num     |     +------------------+
+------------------+
```

## Key Metrics

```
Click-Through Rate (CTR):
  CTR = queries_with_clicks / queries_with_results
  63.2% overall

Conversion Rate:
  Converted sessions / Total sessions
  60.0% (6/10)

Zero-Result Rate:
  Zero-result queries / Total queries
  20.8% (5/24)

Funnel Drop-off:
  Sessions(10) -> Results(8): 20% drop
  Results(8) -> Clicks(7):   13% drop
  Clicks(7) -> Convert(6):   14% drop
```

## Refinement Types

| Type | Description | Example |
|------|-------------|---------|
| narrow | Add terms to focus | "headphones" -> "wireless headphones" |
| broaden | Remove terms for more results | "laptop cooling" -> "laptop accessories" |
| rephrase | Different wording | "mechanical keyboard blue switches" -> "clicky mechanical keyboard" |
| correct | Fix typos | (not shown in sample data) |

## Compilation

```bash
cd 128_SearchAnalytics
lazbuild SearchAnalytics.lpi
./SearchAnalytics
```

## Sample Output

```
1. Query Logs: 24 queries, 6 users, 10 sessions
2. Zero-Results: 5 queries (20.8%), sessions sess-003 and sess-008
3. CTR: 63.2%, position #1 gets 8 clicks with 71s avg dwell
4. Refinements: 8 narrow, 5 rephrase, 1 broaden
5. Distribution: 1-word queries avg 75 results, 4-word avg 6
6. Response: avg 94ms, "laptop" slowest at 150ms
7. Sessions: 60% conversion rate, avg 2.4 queries/session
9. Funnel: 10->8->7->6 (Sessions->Results->Clicks->Conversions)
```

## Related Examples

- **127_Autocomplete** - Typeahead suggestions with frequency ranking
- **126_FuzzySearch** - Approximate matching for typo tolerance

## Best Practices

1. **Log everything**: Track queries, clicks, dwell time, and conversions
2. **Monitor zero-results**: High zero-result rates indicate content gaps
3. **Position bias**: CTR naturally drops with position; normalize for fair comparison
4. **Dwell time threshold**: Short dwell (<10s) may indicate irrelevant results
5. **Refinement analysis**: Many refinements suggest poor initial results
6. **Funnel optimization**: Focus on the stage with highest drop-off
7. **Response time budget**: Keep P95 under 200ms for good UX
8. **Session context**: Analyze queries within sessions, not in isolation
