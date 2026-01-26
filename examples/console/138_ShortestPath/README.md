# Example 138: Shortest Path - Route Finding with Recursive CTEs

## Overview

This example demonstrates **Graph Traversal and Shortest Path Algorithms** using SQLite recursive Common Table Expressions (CTEs). It models a road network between French cities and implements Dijkstra-like path finding, multi-hop queries, network diameter calculation, and alternative route ranking.

## Features Demonstrated

### 1. Graph Overview
- 11 cities (nodes) with geographic coordinates
- 30 directed edges (15 bidirectional roads)
- Highway and national road types with distances

### 2. Direct Connections
- 1-hop neighbors from Paris (6 direct connections)
- Connection density per city (Paris: 6, Lyon: 4, etc.)

### 3. All Paths Between Nodes
- Recursive CTE explores all Paris -> Marseille routes
- Loop detection via path string matching (`NOT LIKE '%city%'`)
- 8 distinct paths found, ranked by distance (780-1375 km)

### 4. Shortest Path
- Paris -> Nice: 935 km via Lyon (2 hops)
- Comparison of all 5 alternative routes
- Demonstrates optimal path selection with `ORDER BY total_distance LIMIT 1`

### 5. Path Reconstruction
- Lille -> Toulouse step-by-step breakdown
- Individual segment distances shown
- Best route: Lille -> Paris -> Bordeaux -> Toulouse (1055 km, 3 hops)

### 6. Multi-Hop Queries
- Cities reachable in exactly 2 hops from Paris
- Cities reachable in exactly 3 hops from Lille
- Minimum distance per destination at fixed hop count

### 7. Single-Source Shortest Paths
- Dijkstra-like results from Paris to all reachable cities
- 9 destinations with optimal routes
- Direct connections (1 hop) vs multi-hop paths

### 8. Network Diameter
- Longest shortest path: Strasbourg -> Toulouse (1250 km)
- Top 5 most distant city pairs
- All-pairs shortest path computation

### 9. Unreachable Nodes
- Ajaccio (Corsica) has no road connection
- Connected component detection via recursive CTE
- 10 of 11 cities in main component

### 10. Alternative Routes
- K-shortest paths: Strasbourg -> Toulouse
- 5 routes ranked with distance overhead
- Road type distribution (highway vs national)

## Architecture

```
+------------------+     +------------------+     +------------------+
| Graph Model      |     | Path Finding     |     | Analysis         |
+------------------+     +------------------+     +------------------+
| 11 city nodes    |---->| Recursive CTE    |---->| Shortest paths   |
| 30 road edges    |     | Loop detection   |     | Network diameter |
| Weighted/directed|     | Path accumulation|     | Reachability     |
+------------------+     +------------------+     +------------------+
```

## Recursive CTE Pattern

```sql
WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (
  -- Base case: start at source
  SELECT 'Paris', 'Paris', 0, 0
  UNION ALL
  -- Recursive step: extend path via edges
  SELECT r.to_city,
         p.path || ' -> ' || r.to_city,
         p.total_distance + r.distance_km,
         p.hops + 1
  FROM paths p
  JOIN roads r ON r.from_city = p.current_city
  WHERE p.path NOT LIKE '%' || r.to_city || '%'  -- loop detection
    AND p.hops < 6                                -- depth limit
)
SELECT path, total_distance FROM paths
WHERE current_city = 'Nice'
ORDER BY total_distance LIMIT 1;  -- shortest path
```

## Database Schema

```
+------------------+     +------------------+
| cities           |     | roads            |
+------------------+     +------------------+
| id (PK)         |     | id (PK)          |
| name (UQ)       |<----| from_city (FK)   |
| country         |<----| to_city (FK)     |
| latitude        |     | distance_km      |
| longitude       |     | road_type        |
+------------------+     +------------------+
```

## Network Statistics

| Metric | Value |
|--------|-------|
| Nodes | 11 cities |
| Edges | 30 directed (15 bidirectional) |
| Max degree | Paris (6 connections) |
| Diameter | 1250 km (Strasbourg-Toulouse) |
| Isolated nodes | 1 (Ajaccio) |
| Connected component | 10 cities |

## Compilation

```bash
cd 138_ShortestPath
lazbuild ShortestPath.lpi
./ShortestPath
```

## Sample Output

```
1. Graph: 11 cities, 30 roads
2. Direct: Paris has 6 neighbors (225-585 km)
3. All paths: 8 routes Paris->Marseille (780-1375 km)
4. Shortest: Paris->Nice = 935 km via Lyon
5. Reconstruction: Lille->Paris->Bordeaux->Toulouse (1055 km)
6. Multi-hop: 10 cities at 2 hops, 8 at 3 hops from Lille
7. Single-source: All 9 shortest paths from Paris
8. Diameter: 1250 km (Strasbourg-Toulouse)
9. Unreachable: Ajaccio (no road connection)
10. Alternatives: 5 routes Strasbourg->Toulouse (+0 to +230 km)
```

## Related Examples

- **139_PageRank** - Iterative graph ranking
- **140_RecommendationEngine** - Collaborative filtering

## Best Practices

1. **Loop detection**: Use path string matching (`NOT LIKE '%node%'`) to prevent infinite recursion
2. **Depth limits**: Always set a maximum hop count to bound recursion
3. **Bidirectional edges**: Insert both directions for undirected graphs
4. **Path accumulation**: Concatenate node names in path column for reconstruction
5. **Shortest selection**: Use `ORDER BY total_distance LIMIT 1` for optimal path
6. **All-pairs**: Use city table as CTE seed for multi-source exploration
7. **Reachability**: Simple recursive CTE without distance for connectivity checks
8. **Alternative routes**: Show K-best paths with overhead vs optimal
