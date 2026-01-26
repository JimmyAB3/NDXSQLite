# Example 66: R-Tree Spatial Index

This example demonstrates SQLite R-Tree spatial indexing with NDXSQLite:

## What you'll learn

- How to create 2D and 3D R-Tree virtual tables
- How to insert bounding boxes and points
- How to query by bounding box overlap
- How to query by point containment
- How to count entries in regions
- How to use bulk insert for performance
- How to use helper functions for bounding boxes

## Key concepts

### Creating an R-Tree

```pascal
RTree := TNDXSQLiteRTree.Create(Connection);
RTree.CreateRTree('locations', rtd2D);  // 2D R-Tree
RTree.CreateRTree('volumes', rtd3D);    // 3D R-Tree
```

### Inserting bounding boxes

```pascal
// 2D: ID, MinX, MaxX, MinY, MaxY
RTree.Insert2D(1, 0, 20, 0, 15);

// 3D: ID, MinX, MaxX, MinY, MaxY, MinZ, MaxZ
RTree.Insert3D(1, 0, 10, 0, 10, 0, 5);

// Insert a point (min = max)
RTree.InsertPoint2D(100, 15, 10);
```

### Spatial queries

```pascal
// Query by bounding box overlap
DataSet := RTree.QueryBox2D(0, 40, 0, 30);

// Query by point containment
DataSet := RTree.QueryContainsPoint2D(35, 15);

// Count entries in region
Count := RTree.CountInBox2D(0, 50, 0, 50);
```

### Bulk insert for performance

```pascal
RTree.BeginBulkInsert;
for I := 1 to 1000 do
  RTree.Insert2D(I, I * 10, I * 10 + 5, I * 5, I * 5 + 3);
RTree.EndBulkInsert;
```

### Helper functions

```pascal
Bounds := BoundingBox2D(0, 10, 0, 10);
Bounds := ExpandBox2D(Bounds, 5);  // Expand by margin
if BoxContainsPoint2D(Bounds, X, Y) then ...
```

## Building

```bash
lazbuild RTreeSpatialIndex.lpi
```

## Running

```bash
./RTreeSpatialIndex      # Linux/macOS
RTreeSpatialIndex.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 66: R-Tree Spatial Index ===

1. Creating 2D R-Tree:
   Created R-Tree table "locations" (2D)
   Inserting building bounding boxes...
   Inserted 5 buildings
   Total entries: 5

2. Query by Bounding Box (0-40, 0-30):
   Buildings overlapping query area:
     ID 1: (0,0) to (20,15)
     ID 2: (25,5) to (45,25)

3. Point Containment Query (point 35, 15):
   Buildings containing point (35, 15):
     Building ID: 2

4. Inserting Point Locations:
   Inserted 3 sensor points
   Total entries now: 8

5. Total Bounding Box:
   X: 0 to 90
   Y: 0 to 85

...

=== Example completed successfully! ===
```

## Use cases

- Geographic Information Systems (GIS)
- Game development (collision detection)
- CAD applications
- Location-based services
- Any application with 2D/3D spatial data

## Cross-Platform

This example works on Windows, Linux, and macOS.
