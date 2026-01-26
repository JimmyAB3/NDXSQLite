# Example 139: PageRank - Iterative Link-Based Ranking

## Overview

This example demonstrates the **PageRank Algorithm** implemented iteratively in SQL. It models a small web graph, computes PageRank scores through power iteration, tracks convergence, compares damping factors, and analyzes the relationship between link structure and ranking.

## Features Demonstrated

### 1. Web Graph Structure
- 12 pages (nodes) with varying connectivity
- 30 directed links (edges)
- Hub structure: homepage (8 inbound, 5 outbound)
- Dangling node: orphan page (1 inbound, 0 outbound)

### 2. Initial PageRank
- Uniform distribution: 1/N = 0.083333 per page
- Total score sum = 1.0 (probability distribution)

### 3. Single Iteration
- First iteration redistributes rank dramatically
- Homepage gains +0.238 (8 inbound links)
- Orphan/archive/post1 lose -0.057 (few inbound)

### 4. Iterative Convergence
- 19 iterations to converge (d=0.85, threshold=0.0001)
- Max delta decreases: 0.238 -> 0.010 -> 0.0001
- Exponential convergence rate

### 5. Convergence Detection
- Threshold-based stopping: max_delta < 0.0001
- Visual bar chart of convergence rate
- Rapid initial convergence, slow tail

### 6. Damping Factor Comparison
- d=0.50: 7 iterations, moderate spread
- d=0.85: 19 iterations, standard spread
- d=0.95: 54 iterations, large spread
- Higher damping = more link-following vs. random jumps

### 7. Top-N Extraction
- Final rankings with visual bar chart
- Homepage dominates (0.248), orphan lowest (0.023)
- 11x spread between highest and lowest

### 8. Rank Distribution Analysis
- Score distribution by quartile (high/medium/low/minimal)
- Total sum < 1.0 due to dangling node rank leak
- Spread ratio: max/min = 11.02x

### 9. Dangling Nodes
- Orphan page: rank sink (no outbound links)
- Rank absorbed without redistribution
- In full PageRank: dangling rank redistributed via teleport

### 10. Link Structure Analysis
- Hub vs authority page identification
- Rank/in-degree ratio shows link quality matters
- Single link from high-rank page > many from low-rank

## PageRank Formula

```
PR(A) = (1-d)/N + d * SUM(PR(T) / L(T))

Where:
  d = damping factor (0.85)
  N = total number of pages
  T = pages linking TO page A
  L(T) = number of outbound links from T
```

## Architecture

```
+------------------+     +------------------+     +------------------+
| Web Graph        |     | Power Iteration  |     | Analysis         |
+------------------+     +------------------+     +------------------+
| 12 page nodes    |---->| Init: 1/N each   |---->| Top-N rankings   |
| 30 link edges    |     | Update via SQL   |     | Convergence rate |
| Hub/authority    |     | Track deltas     |     | Distribution     |
+------------------+     | Check threshold  |     | Hub vs Authority |
                          +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| pages            |     | links            |
+------------------+     +------------------+
| id (PK)          |     | id (PK)          |
| url (UQ)         |<----| from_page        |
| title            |<----| to_page          |
+------------------+     +------------------+

+------------------+     +------------------+
| pagerank         |     | convergence      |
+------------------+     +------------------+
| page (PK)        |     | iteration (PK)   |
| iteration (PK)   |     | max_delta        |
| score            |     | avg_delta        |
+------------------+     | damping          |
                          +------------------+
```

## Convergence by Damping Factor

| Damping | Iterations | Top Score | Spread | Interpretation |
|---------|-----------|-----------|--------|----------------|
| 0.50 | 7 | 0.196 | ~5x | 50% random jumps, fast convergence |
| 0.85 | 19 | 0.248 | ~11x | Standard (Google's original) |
| 0.95 | 54 | 0.240 | ~12x | 95% link-following, slow convergence |

## SQL Implementation Pattern

```sql
-- Power iteration step
INSERT OR REPLACE INTO pagerank (page, iteration, score)
SELECT p.url, :iter,
  (1-d)/N + d * COALESCE((
    SELECT SUM(pr.score / CAST((
      SELECT COUNT(*) FROM links l2
      WHERE l2.from_page = l.from_page
    ) AS REAL))
    FROM links l
    JOIN pagerank pr ON pr.page = l.from_page
      AND pr.iteration = :iter-1
    WHERE l.to_page = p.url
  ), 0.0)
FROM pages p;
```

## Compilation

```bash
cd 139_PageRank
lazbuild PageRank.lpi
./PageRank
```

## Sample Output

```
1. Graph: 12 pages, 30 links
2. Initial: uniform 0.083333 each (sum=1.0)
3. Iter 1: homepage +0.238, orphan -0.047
4. Convergence: 19 iterations, max_delta 0.238->0.0001
5. Detection: threshold 0.0001 reached at iter 19
6. Damping: d=0.50 (7 iter) vs d=0.85 (19) vs d=0.95 (54)
7. Top-N: home(0.248) > docs(0.104) > contact(0.102)
8. Distribution: 11x spread, 4 tiers
9. Dangling: orphan absorbs rank (score=0.023)
10. Analysis: rank/in-deg shows link quality matters
```

## Related Examples

- **138_ShortestPath** - Graph traversal with recursive CTEs
- **140_RecommendationEngine** - Collaborative filtering

## Best Practices

1. **Damping factor**: Use 0.85 as standard (balances link-following vs teleport)
2. **Convergence threshold**: 0.0001 is typical for production quality
3. **Iteration limit**: Set max iterations to prevent infinite loops on non-converging graphs
4. **Dangling nodes**: In full implementation, redistribute dangling rank uniformly
5. **Score normalization**: Total scores should sum to 1.0 in proper implementations
6. **Hub vs Authority**: High out-degree = hub, high in-degree = authority
7. **Link quality**: PageRank values link quality over quantity
8. **Power iteration**: Simple but effective; alternatives include eigenvalue decomposition
