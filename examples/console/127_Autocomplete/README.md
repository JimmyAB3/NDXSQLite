# Example 127: Autocomplete - Prefix Matching, Frequency, Personalization

## Overview

This example demonstrates a **Typeahead Autocomplete** system with prefix matching, frequency-weighted ranking, recent search history, per-user personalization, multi-word suggestions, and trending terms. It simulates a real-time search suggestion engine for a product catalog.

## Features Demonstrated

### 1. Vocabulary Index
- 30 terms across 6 categories
- Global frequency scores per term
- Category-based organization
- Indexed for fast prefix lookups

### 2. Prefix Matching
- LIKE-based prefix search (term LIKE 'prefix%')
- Returns all matching terms sorted by frequency
- Works from 2+ characters
- Case-insensitive matching

### 3. Frequency-Weighted Ranking
- Higher frequency terms appear first
- Based on global search volume
- "usb cable" (1200) > "usb hub" (480) > "usb adapter" (390)
- Natural ordering matches user expectations

### 4. Recent Searches
- Per-user search history with timestamps
- Selected result tracking
- Most recent first ordering
- Useful for "pick up where you left off"

### 5. Per-User Personalization
- Category preference weights per user
- Same prefix produces different rankings
- Alice (electronics x2.5): "wireless headset" ranks first
- Bob (peripherals x2.5): "wireless mouse" ranks first
- Carol (accessories x2.5): "wireless charger" ranks first

### 6. Typeahead Simulation
- Character-by-character input simulation
- Suggestions update with each keystroke
- Minimum 2 characters before suggestions appear
- Top-3 results per keystroke

### 7. Multi-Word Suggestions
- Completes second word after first is typed
- "external" -> "external ssd", "external hard drive"
- Partial second word: "laptop c" -> "laptop charger", "laptop cooling pad"
- Frequency ordering maintained

### 8. Popular Completions
- Most searched terms from history
- Unique user count per query
- Category distribution analysis
- Cross-user popularity signal

### 9. Trending Suggestions
- Weekly trend scores
- Search count per period
- Trending + prefix filter combination
- Boost recently popular terms

### 10. Statistics
- Vocabulary and category metrics
- User activity summary
- Total search volume
- Category frequency breakdown

## Architecture

```
+------------------+     +------------------+     +------------------+
| User Input       |     | Suggestion Engine|     | Ranked Results   |
+------------------+     +------------------+     +------------------+
| "wire..."        |---->| Prefix Match     |---->| Suggestions      |
| keystroke by key |     | Freq. Ranking    |     | Top-N results    |
|                  |     | User Prefs       |     | Personalized     |
+------------------+     | Recent History   |     +------------------+
                          | Trending         |
                          +------------------+
                                  |
              +-------------------+-------------------+
              |                   |                   |
              v                   v                   v
      +---------------+  +---------------+  +---------------+
      | vocabulary    |  | search_history|  | user_prefs    |
      +---------------+  +---------------+  +---------------+
      | term + freq   |  | per-user logs |  | category wts  |
      | category      |  | timestamps    |  | personalize   |
      | prefix index  |  | selections    |  | boost scores  |
      +---------------+  +---------------+  +---------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| vocabulary       |     | search_history   |     | user_preferences |
+------------------+     +------------------+     +------------------+
| id (PK, AUTO)    |     | id (PK, AUTO)    |     | id (PK, AUTO)    |
| term (UNIQUE)    |     | user_id          |     | user_id          |
| category         |     | query_text       |     | category         |
| frequency        |     | result_count     |     | weight           |
| last_used        |     | selected_result  |     +------------------+
+------------------+     | searched_at      |
                          +------------------+     +------------------+
                                                   | trending         |
                                                   +------------------+
                                                   | id (PK, AUTO)    |
                                                   | term             |
                                                   | search_count     |
                                                   | period           |
                                                   | trend_score      |
                                                   +------------------+
```

## Personalization Formula

```
adjusted_score = base_frequency * category_weight

Where:
- base_frequency: global term frequency
- category_weight: user's preference for that category (default 1.0)

Example for user "alice" (electronics=2.5):
  wireless headset [electronics]: 540 * 2.5 = 1350
  wireless mouse [peripherals]:   920 * 1.0 = 920
```

## Compilation

```bash
cd 127_Autocomplete
lazbuild Autocomplete.lpi
./Autocomplete
```

## Sample Output

```
2. Prefix "blue": headphones(850), speaker(720), earbuds(680), adapter(340)
5. Personalized "wire": alice->headset, bob->mouse, carol->charger
6. Typeahead: "bl" already suggests bluetooth products (top 3)
9. Trending: wireless mouse(8.5), mechanical keyboard(7.2)
```

## Related Examples

- **126_FuzzySearch** - Approximate matching for typo tolerance
- **128_SearchAnalytics** - Search behavior tracking and optimization

## Best Practices

1. **Minimum prefix length**: Require 2-3 chars to avoid too many results
2. **Result limit**: Show top 5-8 suggestions maximum
3. **Frequency decay**: Reduce old frequency scores over time
4. **User history boost**: Prioritize terms the user has searched before
5. **Debounce input**: Don't query on every keystroke in real applications
6. **Cache popular prefixes**: Pre-compute suggestions for common prefixes
7. **A/B test weights**: Experiment with frequency vs personalization balance
8. **Trending boost**: Temporarily boost trending terms for timely suggestions
