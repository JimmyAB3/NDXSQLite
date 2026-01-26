# Example 50: Graph Database

This example demonstrates how to implement a graph database using SQLite with NDXSQLite, enabling social network analysis and path finding.

## Features Demonstrated

- **Graph Data Model**: Nodes (vertices) and edges (relationships)
- **Social Network Patterns**: Friends, followers, connections
- **Direct Connections**: 1-hop queries
- **Friends of Friends**: 2-hop queries
- **Mutual Friends**: Common connections
- **Shortest Path**: BFS using recursive CTEs
- **All Paths**: Finding all routes between nodes
- **Graph Metrics**: Degree, popularity analysis
- **Weighted Graphs**: Latency, distance calculations
- **Reachability Analysis**: What nodes can be reached

## Database Schema

### Tables

1. **nodes**: Graph vertices
   - Node type (person, server, etc.)
   - Name and properties (JSON)

2. **edges**: Relationships between nodes
   - From/to node references
   - Edge type (friend, follows, link)
   - Weight (for distance/cost)
   - Properties (JSON)

## Key Operations

### Create Nodes and Edges
```pascal
Alice := CreateNode('person', 'Alice', '{"city": "New York"}');
Bob := CreateNode('person', 'Bob', '{"city": "Boston"}');
CreateBidirectionalEdge(Alice, Bob, 'friend');  // Mutual friendship
CreateEdge(Bob, Alice, 'follows');               // Directional relationship
```

### Direct Connections (1-hop)
```sql
SELECT n.name FROM edges e
JOIN nodes n ON e.to_node = n.id
WHERE e.from_node = ?
  AND e.edge_type = 'friend'
```

### Friends of Friends (2-hop)
```sql
SELECT DISTINCT n2.name
FROM edges e1
JOIN edges e2 ON e1.to_node = e2.from_node
JOIN nodes n2 ON e2.to_node = n2.id
WHERE e1.from_node = ?
  AND e1.edge_type = 'friend'
  AND e2.edge_type = 'friend'
  AND e2.to_node NOT IN (direct friends)
```

### Shortest Path (Recursive CTE)
```sql
WITH RECURSIVE path(node_id, path, depth) AS (
  SELECT id, name, 0 FROM nodes WHERE name = 'Alice'
  UNION ALL
  SELECT e.to_node, path.path || ' -> ' || n.name, path.depth + 1
  FROM path
  JOIN edges e ON path.node_id = e.from_node
  JOIN nodes n ON e.to_node = n.id
  WHERE e.edge_type = 'friend'
    AND path.depth < 6
    AND path.path NOT LIKE '%' || n.name || '%'
)
SELECT path FROM path WHERE node_id = (target) LIMIT 1
```

### Graph Metrics
```sql
-- Node degree (connection count)
SELECT n.name, COUNT(e.id) as degree
FROM nodes n LEFT JOIN edges e ON n.id = e.from_node
GROUP BY n.id ORDER BY degree DESC
```

## Graph Patterns

### Social Network
- Bidirectional friendships
- Directional follows
- Mutual friend discovery

### Network Topology
- Weighted edges for latency/distance
- Shortest path calculations
- Connectivity analysis

## Build and Run

```bash
cd 50_GraphDatabase
fpc GraphDatabase.lpr
./GraphDatabase
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Use Cases

- Social networks
- Recommendation systems
- Network topology analysis
- Knowledge graphs
- Dependency graphs
- Route planning
