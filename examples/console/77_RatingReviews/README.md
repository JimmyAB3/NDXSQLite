# Example 77: Ratings and Reviews System

## Overview

This example demonstrates a **complete ratings and reviews system** with star ratings, written reviews, helpfulness voting, verified purchase badges, seller responses, and comprehensive analytics. It showcases patterns used in e-commerce platforms and content rating applications.

## Features Demonstrated

### 1. Star Ratings (1-5 Scale)
- Integer rating storage with validation
- Denormalized aggregates for performance
- Average rating calculation
- Rating distribution histograms

### 2. Written Reviews
- Review title and content
- Pros and cons sections
- Review images support
- Edit/update tracking

### 3. Verified Purchase Badge
- Purchase tracking
- Automatic verification on review creation
- Visual distinction in display

### 4. Helpfulness Voting
- Helpful / Not helpful votes
- Vote tracking per user (one vote per review)
- Self-vote prevention
- Reviewer reputation building

### 5. Seller Responses
- One response per review
- Response content storage
- Display with original review

### 6. Review Moderation
- Status workflow (pending, approved, rejected, flagged)
- Report system with reasons
- Moderation queue support

## Database Schema

```
+------------+     +-------------+     +----------------+
| products   |<--->| reviews     |<--->| review_votes   |
+------------+     +-------------+     +----------------+
| id         |     | product_id  |     | review_id      |
| name       |     | user_id     |     | user_id        |
| rating_avg |     | rating      |     | is_helpful     |
| rating_count|    | title       |     +----------------+
+------------+     | content     |
      |            | pros/cons   |
      |            | is_verified |
      v            +-------------+
+------------+           |
| purchases  |           v
+------------+     +-------------------+
| user_id    |     | review_responses  |
| product_id |     +-------------------+
| date       |     | review_id         |
+------------+     | responder         |
                   | content           |
                   +-------------------+
```

## Key Patterns

### Denormalized Rating Aggregates
```pascal
procedure UpdateProductRating(ProductId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE products SET ' +
    '  rating_count = (SELECT COUNT(*) FROM reviews WHERE product_id = ?),' +
    '  rating_avg = (SELECT AVG(rating * 1.0) FROM reviews WHERE product_id = ?)' +
    'WHERE id = ?', [ProductId, ProductId, ProductId]);
end;
```

### Verified Purchase Check
```sql
-- Check if user purchased the product
SELECT COUNT(*) FROM purchases
WHERE user_id = ? AND product_id = ?;
```

### Rating Distribution
```sql
SELECT rating, COUNT(*) AS cnt,
  ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM reviews), 1) AS pct
FROM reviews WHERE product_id = ?
GROUP BY rating ORDER BY rating DESC;
```

### Helpfulness Voting
```sql
-- Add vote (one per user per review)
INSERT INTO review_votes (review_id, user_id, is_helpful)
VALUES (?, ?, ?)
ON CONFLICT(review_id, user_id) DO NOTHING;

-- Update helpful count
UPDATE reviews SET helpful_count = helpful_count + 1
WHERE id = ?;
```

## Demonstration Sections

1. **Schema Creation** - Reviews database structure
2. **Sample Data** - Products, users, reviews, votes
3. **Product Reviews** - Display with formatting
4. **Rating Distribution** - Visual histograms
5. **Top Reviewers** - Reviewer leaderboard
6. **Verified Analysis** - Verified vs unverified comparison
7. **Review Sorting** - Multiple sort options
8. **Statistics** - Overall metrics
9. **Seller Dashboard** - Seller-focused view

## Review Features

| Feature | Implementation |
|---------|----------------|
| Star ratings | 1-5 integer with CHECK constraint |
| Verified badge | Cross-reference with purchases |
| Helpfulness | Vote counts, one vote per user |
| Seller response | One response per review |
| Moderation | Status field with workflow |
| Aggregates | Denormalized for performance |

## Compilation

```bash
cd 77_RatingReviews
lazbuild RatingReviews.lpi
./RatingReviews
```

## Sample Output

```
3. Product Reviews Display
   =========================

   Product Summary:
   Product                   | Seller      | Rating        | Reviews | Price
   --------------------------|-------------|---------------|---------|--------
   Wireless Headphones Pro   | TechStore   | ****- 4.3 |       4 | $149.99
   Coffee Maker Deluxe       | HomeGoods   | ***** 4.7 |       3 | $89.99

4. Rating Distribution
   =====================

   Wireless Headphones Pro:
   5 stars: ##########            50.0% (2)
   4 stars: #####                 25.0% (1)
   3 stars: #####                 25.0% (1)

6. Verified Purchase Analysis
   ============================

   Status     | Reviews | Avg Rating | Helpful Votes
   -----------|---------|------------|---------------
   Unverified |       5 |       3.80 | 0
   Verified   |       5 |       4.60 | 8

   Insight: Verified purchasers tend to rate higher and receive more helpful votes.
```

## Related Examples

- **74_ShoppingCart** - E-commerce cart (purchase tracking)
- **75_UserAuthentication** - User identification
- **76_TaggingSystem** - Content categorization
- **78_NotificationSystem** - Review notifications

## Production Considerations

1. **Review Spam**: Implement rate limiting and duplicate detection
2. **Fake Reviews**: ML-based fake review detection
3. **Review Incentives**: Tracking incentivized reviews separately
4. **Edit History**: Keep history of review edits
5. **Response Notifications**: Notify reviewer of seller response
6. **Review Sorting**: Consider multiple factors (helpful, verified, recent)

## Analytics Queries

```sql
-- Most helpful reviewers
SELECT username, COUNT(*) AS reviews,
  SUM(helpful_count) AS total_helpful
FROM users u
JOIN reviews r ON u.id = r.user_id
GROUP BY u.id ORDER BY total_helpful DESC;

-- Products needing attention (low ratings)
SELECT name, rating_avg, rating_count
FROM products WHERE rating_avg < 3.0
ORDER BY rating_count DESC;

-- Review response rate per seller
SELECT seller,
  COUNT(DISTINCT r.id) AS total_reviews,
  COUNT(DISTINCT rr.id) AS responses,
  ROUND(COUNT(DISTINCT rr.id) * 100.0 / COUNT(DISTINCT r.id), 1) AS response_rate
FROM products p
JOIN reviews r ON p.id = r.product_id
LEFT JOIN review_responses rr ON r.id = rr.review_id
GROUP BY seller;
```
