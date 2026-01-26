# Example 61: GeoSpatial Queries

This example demonstrates basic geospatial operations and location-based queries with NDXSQLite.

## Features Demonstrated

- **Coordinate Storage**: Latitude/longitude as REAL
- **Bounding Box Queries**: Rectangular area searches
- **R-Tree Spatial Index**: Efficient spatial lookups
- **Haversine Distance**: Great-circle distance calculation
- **Nearest Neighbor**: Find closest locations
- **Radius Search**: Geofencing and proximity queries
- **Delivery Zones**: Point-in-polygon checks

## Database Schema

### Locations Table
```sql
CREATE TABLE locations (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  category TEXT,
  latitude REAL NOT NULL,
  longitude REAL NOT NULL,
  address TEXT
);

CREATE INDEX idx_locations_coords ON locations(latitude, longitude);
```

### R-Tree Spatial Index
```sql
CREATE VIRTUAL TABLE locations_rtree USING rtree(
  id,
  min_lat, max_lat,
  min_lon, max_lon
);
```

### Delivery Zones (Bounding Boxes)
```sql
CREATE TABLE delivery_zones (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  min_lat REAL NOT NULL,
  max_lat REAL NOT NULL,
  min_lon REAL NOT NULL,
  max_lon REAL NOT NULL
);
```

## Key Operations

### Haversine Distance Formula (Pascal)
```pascal
function HaversineDistance(Lat1, Lon1, Lat2, Lon2: Double): Double;
const
  EARTH_RADIUS_KM = 6371.0;
var
  dLat, dLon, a, c: Double;
begin
  Lat1 := DegToRad(Lat1);
  Lon1 := DegToRad(Lon1);
  Lat2 := DegToRad(Lat2);
  Lon2 := DegToRad(Lon2);

  dLat := Lat2 - Lat1;
  dLon := Lon2 - Lon1;

  a := Sin(dLat/2) * Sin(dLat/2) +
       Cos(Lat1) * Cos(Lat2) * Sin(dLon/2) * Sin(dLon/2);
  c := 2 * ArcTan2(Sqrt(a), Sqrt(1-a));

  Result := EARTH_RADIUS_KM * c;
end;
```

### Bounding Box Query
```sql
SELECT name, category, latitude, longitude
FROM locations
WHERE latitude BETWEEN 48.80 AND 48.90
  AND longitude BETWEEN 2.20 AND 2.45
ORDER BY name;
```

### R-Tree Spatial Index Query
```sql
SELECT l.name, l.category
FROM locations l
JOIN locations_rtree r ON l.id = r.id
WHERE r.min_lat >= 48.80 AND r.max_lat <= 48.90
  AND r.min_lon >= 2.20 AND r.max_lon <= 2.45;
```

### Radius Search (Geofencing)
```pascal
// Find locations within radius using bounding box pre-filter
RadiusKm := 3.0;
RefLat := 48.8606;  // Louvre Museum
RefLon := 2.3376;

// Approximate bounding box (1 degree lat ~ 111 km)
DeltaLat := RadiusKm / 111.0;
DeltaLon := RadiusKm / (111.0 * Cos(DegToRad(RefLat)));

// Query with bounding box filter
DS := Connection.ExecuteQuery(
  'SELECT name, latitude, longitude FROM locations ' +
  'WHERE latitude BETWEEN ? AND ? AND longitude BETWEEN ? AND ?',
  [RefLat - DeltaLat, RefLat + DeltaLat, RefLon - DeltaLon, RefLon + DeltaLon]);

// Then calculate exact distance for each result
while not DS.EOF do
begin
  Dist := HaversineDistance(RefLat, RefLon, LocLat, LocLon);
  if Dist <= RadiusKm then
    // Include in results
end;
```

### Delivery Zone Check
```sql
SELECT l.name, dz.name as zone_name
FROM locations l
LEFT JOIN delivery_zones dz ON
  l.latitude BETWEEN dz.min_lat AND dz.max_lat AND
  l.longitude BETWEEN dz.min_lon AND dz.max_lon
ORDER BY l.name;
```

### Route Distance
```sql
SELECT r.name as route_name,
       s.name as start_name, s.latitude as start_lat, s.longitude as start_lon,
       e.name as end_name, e.latitude as end_lat, e.longitude as end_lon
FROM routes r
JOIN locations s ON r.start_location_id = s.id
JOIN locations e ON r.end_location_id = e.id;
```

## Sample Data

```sql
-- Paris landmarks
INSERT INTO locations VALUES (1, 'Eiffel Tower', 'landmark', 48.8584, 2.2945, 'Paris');
INSERT INTO locations VALUES (2, 'Louvre Museum', 'museum', 48.8606, 2.3376, 'Paris');
INSERT INTO locations VALUES (3, 'Notre-Dame', 'landmark', 48.8530, 2.3499, 'Paris');

-- R-Tree entries (point = min equals max)
INSERT INTO locations_rtree VALUES (1, 48.8584, 48.8584, 2.2945, 2.2945);
INSERT INTO locations_rtree VALUES (2, 48.8606, 48.8606, 2.3376, 2.3376);
```

## Performance Tips

| Query Type | Without Index | With R-Tree |
|------------|---------------|-------------|
| Bounding box | O(n) scan | O(log n) |
| Nearest neighbor | O(n) | O(log n) + sort |
| Radius search | O(n) | O(log n) pre-filter |

## Build and Run

```bash
cd 61_GeoSpatial
fpc GeoSpatial.lpr
./GeoSpatial
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use R-Tree for efficient bounding box queries
- Pre-filter with bounding box before exact distance calculation
- Store coordinates as REAL (floating point)
- Use Haversine formula for great-circle distance on Earth
- Index latitude and longitude columns together
- Consider geohash for clustering nearby points
- For complex polygons, use SpatiaLite extension
- Cache distance calculations when possible
- Use appropriate precision (6 decimal places ~ 0.1m accuracy)
