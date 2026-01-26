# Example 140: Recommendation Engine - Collaborative Filtering

## Overview

This example demonstrates a **Collaborative Filtering Recommendation Engine** using SQL. It models a user-item purchase matrix, computes user similarity (Jaccard index), item co-occurrence, and generates personalized recommendations using user-based and item-based approaches. It also addresses the cold start problem with fallback strategies.

## Features Demonstrated

### 1. Data Overview
- 10 users, 12 products (books/electronics/music), 40 purchases
- Products ranked by popularity (Clean Code: 5 buyers, Headphones: 5 buyers)
- Price range: $12.99 to $89.99

### 2. User-Item Rating Matrix
- Sparse matrix visualization (33.3% dense)
- Ratings 1-5, dots for unrated items
- Clear user profiles: Alice=developer, Carol=music lover, Dave=electronics

### 3. User Similarity (Jaccard Index)
- Jaccard = |items in common| / |items by either user|
- Top pairs: Carol-Eve (0.50), Carol-Grace (0.50), Alice-Bob (0.375)
- Similarity drives user-based recommendations

### 4. Item Co-occurrence
- Items frequently bought together
- Jazz Collection + Headphones: 3 co-buyers (score 0.75)
- Rock Anthology + Headphones: 2 co-buyers (score 1.00)
- Mechanical Keyboard + USB Hub: 2 co-buyers (score 0.67)

### 5. User-Based Recommendations
- For Alice: find users sharing 2+ products, recommend their other purchases
- SQL Mastery, Jazz Collection recommended by 2 similar users each
- Shows recommender names (Bob, Frank, Carol, Eve, Dave)

### 6. Item-Based Recommendations
- "If you bought X, you might also like Y"
- Python Cookbook buyers also bought: Clean Code, Design Patterns, SQL Mastery
- Jazz Collection buyers also bought: Headphones (3), Classical Piano (2)

### 7. "Users Who Bought X Also Bought Y"
- Co-purchase frequency with percentage of buyers
- Jazz Collection -> Headphones: 75% of Jazz buyers also bought Headphones
- Mechanical Keyboard -> USB Hub: 67% co-purchase rate

### 8. Top-N Recommendations
- Weighted scoring: support count * average rating
- Dave (electronics fan) gets: Clean Code, Jazz Collection, Python Cookbook
- Cross-category discovery through collaborative filtering

### 9. Cold Start Problem
- Iris has only 1 purchase (Python Cookbook)
- Strategy 1: Item-based (co-purchase from single item)
- Strategy 2: Popular items (global bestsellers)
- Strategy 3: Category-based (same genre as existing purchase)

### 10. Quality Metrics
- Item coverage: 100% (all products recommendable)
- Matrix sparsity: 66.7% (typical for recommendation systems)
- Category preferences by age group
- Rating distribution: skewed positive (18 five-star, 16 four-star)

## Architecture

```
+------------------+     +------------------+     +------------------+
| Data Layer       |     | Similarity       |     | Recommendations  |
+------------------+     +------------------+     +------------------+
| 10 users         |---->| Jaccard index    |---->| User-based CF    |
| 12 products      |     | Co-occurrence    |     | Item-based CF    |
| 40 purchases     |     | Category match   |     | Also-bought      |
| Ratings 1-5      |     |                  |     | Cold start        |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| users            |     | purchases        |     | products         |
+------------------+     +------------------+     +------------------+
| id (PK)          |<----| user_id (FK)     |     | id (PK)          |
| name             |     | product_id (FK)  |---->| name             |
| age_group        |     | rating (1-5)     |     | category         |
+------------------+     | purchase_date    |     | price            |
                          | PK(user,product) |     +------------------+
                          +------------------+
```

## Recommendation Approaches

| Approach | Input | Method | Best For |
|----------|-------|--------|----------|
| User-based CF | User's purchases | Find similar users, their items | Active users |
| Item-based CF | Single item | Find co-purchased items | Product pages |
| Also-bought | Item purchase | Co-occurrence frequency | Checkout upsell |
| Popular | None | Global bestsellers | Cold start |
| Category | User's categories | Same-genre popular | Limited history |

## SQL Patterns

### Jaccard Similarity
```sql
SELECT
  (SELECT COUNT(*) FROM purchases p1
   JOIN purchases p2 ON p1.product_id = p2.product_id
   WHERE p1.user_id = A AND p2.user_id = B) as common,
  (SELECT COUNT(DISTINCT product_id) FROM purchases
   WHERE user_id = A OR user_id = B) as union_size
-- Jaccard = common / union_size
```

### Co-occurrence
```sql
SELECT pr1.product_id, pr2.product_id, COUNT(*) as co_buyers
FROM purchases pr1
JOIN purchases pr2 ON pr1.user_id = pr2.user_id
  AND pr1.product_id < pr2.product_id
GROUP BY pr1.product_id, pr2.product_id
```

### User-Based CF
```sql
-- 1. Find similar users (share 2+ items)
-- 2. Get their purchases
-- 3. Exclude items user already has
-- 4. Rank by recommender count * avg rating
```

## Compilation

```bash
cd 140_RecommendationEngine
lazbuild RecommendationEngine.lpi
./RecommendationEngine
```

## Sample Output

```
1. Data: 10 users, 12 products, 40 purchases
2. Matrix: 33.3% dense, ratings 1-5
3. Similarity: Carol-Eve (0.50), Alice-Bob (0.375)
4. Co-occurrence: Jazz+Headphones (3 co-buyers)
5. User-based: SQL Mastery, Jazz for Alice (2 recommenders each)
6. Item-based: Python Cookbook -> Clean Code, Design Patterns
7. Also-bought: Jazz->Headphones (75%), Keyboard->Hub (67%)
8. Top-N Dave: Clean Code, Jazz, Python Cookbook
9. Cold start: 3 strategies for new user Iris
10. Metrics: 100% coverage, 66.7% sparse, positive skew
```

## Related Examples

- **138_ShortestPath** - Graph traversal algorithms
- **139_PageRank** - Iterative graph ranking

## Best Practices

1. **Minimum overlap**: Require 2+ shared items for user similarity (reduces noise)
2. **Cold start fallback**: Use popularity-based recommendations for new users
3. **Cross-category discovery**: Allow CF to recommend across categories
4. **Weighted scoring**: Combine support count with average rating for better rankings
5. **Sparsity handling**: Real matrices are 95-99% sparse; ensure sufficient data
6. **Co-occurrence threshold**: Filter co-purchases with minimum frequency (2+)
7. **Rating bias**: Normalize ratings per user to account for generous/harsh raters
8. **Category preferences**: Use demographic segments for cold start personalization
