{===============================================================================
  NDXSQLite Example 61 - GeoSpatial Queries
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates basic geospatial operations:
  - Storing latitude/longitude coordinates
  - Bounding box queries
  - Haversine distance calculation (great-circle distance)
  - Finding nearest neighbors
  - R-Tree spatial index for efficient queries
  - Geofencing and radius searches

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program GeoSpatial;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Math, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

const
  EARTH_RADIUS_KM = 6371.0;

{ Creates the locations, delivery_zones, routes tables and an R-Tree virtual table for spatial indexing. }
procedure SetupGeoTables;
begin
  // Locations table with coordinates
  Connection.ExecuteNonQuery(
    'CREATE TABLE locations (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT,' +
    '  latitude REAL NOT NULL,' +
    '  longitude REAL NOT NULL,' +
    '  address TEXT' +
    ')');

  // Delivery zones (polygons stored as bounding boxes)
  Connection.ExecuteNonQuery(
    'CREATE TABLE delivery_zones (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  min_lat REAL NOT NULL,' +
    '  max_lat REAL NOT NULL,' +
    '  min_lon REAL NOT NULL,' +
    '  max_lon REAL NOT NULL' +
    ')');

  // R-Tree virtual table for spatial indexing
  Connection.ExecuteNonQuery(
    'CREATE VIRTUAL TABLE locations_rtree USING rtree(' +
    '  id,' +
    '  min_lat, max_lat,' +
    '  min_lon, max_lon' +
    ')');

  // Routes table
  Connection.ExecuteNonQuery(
    'CREATE TABLE routes (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  start_location_id INTEGER,' +
    '  end_location_id INTEGER,' +
    '  FOREIGN KEY (start_location_id) REFERENCES locations(id),' +
    '  FOREIGN KEY (end_location_id) REFERENCES locations(id)' +
    ')');

  // Create index on coordinates
  Connection.ExecuteNonQuery('CREATE INDEX idx_locations_coords ON locations(latitude, longitude)');
end;

{ Populates locations, delivery zones, and routes with European landmarks, restaurants, and cities. }
procedure InsertSampleData;
begin
  // Major cities with real coordinates
  // Paris
  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (1, ''Eiffel Tower'', ''landmark'', 48.8584, 2.2945, ''Champ de Mars, Paris'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (1, 48.8584, 48.8584, 2.2945, 2.2945)');

  // Paris landmarks
  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (2, ''Louvre Museum'', ''museum'', 48.8606, 2.3376, ''Rue de Rivoli, Paris'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (2, 48.8606, 48.8606, 2.3376, 2.3376)');

  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (3, ''Notre-Dame'', ''landmark'', 48.8530, 2.3499, ''Île de la Cité, Paris'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (3, 48.8530, 48.8530, 2.3499, 2.3499)');

  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (4, ''Arc de Triomphe'', ''landmark'', 48.8738, 2.2950, ''Place Charles de Gaulle, Paris'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (4, 48.8738, 48.8738, 2.2950, 2.2950)');

  // Paris restaurants
  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (5, ''Le Jules Verne'', ''restaurant'', 48.8583, 2.2944, ''Eiffel Tower, Paris'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (5, 48.8583, 48.8583, 2.2944, 2.2944)');

  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (6, ''Café de Flore'', ''restaurant'', 48.8540, 2.3325, ''Boulevard Saint-Germain, Paris'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (6, 48.8540, 48.8540, 2.3325, 2.3325)');

  // Other cities
  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (7, ''Big Ben'', ''landmark'', 51.5007, -0.1246, ''Westminster, London'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (7, 51.5007, 51.5007, -0.1246, -0.1246)');

  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (8, ''Colosseum'', ''landmark'', 41.8902, 12.4922, ''Piazza del Colosseo, Rome'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (8, 41.8902, 41.8902, 12.4922, 12.4922)');

  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (9, ''Sagrada Familia'', ''landmark'', 41.4036, 2.1744, ''Barcelona, Spain'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (9, 41.4036, 41.4036, 2.1744, 2.1744)');

  Connection.ExecuteNonQuery(
    'INSERT INTO locations VALUES (10, ''Brandenburg Gate'', ''landmark'', 52.5163, 13.3777, ''Pariser Platz, Berlin'')');
  Connection.ExecuteNonQuery(
    'INSERT INTO locations_rtree VALUES (10, 52.5163, 52.5163, 13.3777, 13.3777)');

  // Delivery zones
  Connection.ExecuteNonQuery(
    'INSERT INTO delivery_zones VALUES (1, ''Paris Central'', 48.82, 48.90, 2.25, 2.40)');
  Connection.ExecuteNonQuery(
    'INSERT INTO delivery_zones VALUES (2, ''Paris West'', 48.82, 48.90, 2.10, 2.25)');
  Connection.ExecuteNonQuery(
    'INSERT INTO delivery_zones VALUES (3, ''Paris East'', 48.82, 48.90, 2.40, 2.55)');

  // Routes
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (1, ''Tourist Tour 1'', 1, 2)');
  Connection.ExecuteNonQuery('INSERT INTO routes VALUES (2, ''Tourist Tour 2'', 2, 3)');
end;

{ Calculates the great-circle distance between two coordinates in kilometers. }
function HaversineDistance(Lat1, Lon1, Lat2, Lon2: Double): Double;
var
  dLat, dLon, a, c: Double;
begin
  // Convert to radians
  Lat1 := DegToRad(Lat1);
  Lon1 := DegToRad(Lon1);
  Lat2 := DegToRad(Lat2);
  Lon2 := DegToRad(Lon2);

  dLat := Lat2 - Lat1;
  dLon := Lon2 - Lon1;

  a := Sin(dLat / 2) * Sin(dLat / 2) +
       Cos(Lat1) * Cos(Lat2) * Sin(dLon / 2) * Sin(dLon / 2);
  c := 2 * ArcTan2(Sqrt(a), Sqrt(1 - a));

  Result := EARTH_RADIUS_KM * c;
end;

{ Queries and prints all landmarks sorted by name with their coordinates. }
procedure DemoBasicQueries;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic Location Queries');
  WriteLn('   ------------------------');

  WriteLn('   All landmarks:');
  DS := Connection.ExecuteQuery(
    'SELECT name, latitude, longitude FROM locations WHERE category = ''landmark'' ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%.4f, %.4f)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('latitude').AsFloat,
         DS.FieldByName('longitude').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Finds and prints all locations within a Paris-area bounding box using coordinate range filtering. }
procedure DemoBoundingBoxQuery;
var
  DS: TDataSet;
  MinLat, MaxLat, MinLon, MaxLon: Double;
begin
  WriteLn('2. Bounding Box Query');
  WriteLn('   --------------------');

  // Define Paris bounding box
  MinLat := 48.80;
  MaxLat := 48.90;
  MinLon := 2.20;
  MaxLon := 2.45;

  WriteLn(Format('   Locations in Paris area (%.2f,%.2f to %.2f,%.2f):',
    [MinLat, MinLon, MaxLat, MaxLon]));

  DS := Connection.ExecuteQuery(
    'SELECT name, category, latitude, longitude ' +
    'FROM locations ' +
    'WHERE latitude BETWEEN ? AND ? AND longitude BETWEEN ? AND ? ' +
    'ORDER BY name',
    [MinLat, MaxLat, MinLon, MaxLon]);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s (%.4f, %.4f)',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('latitude').AsFloat,
         DS.FieldByName('longitude').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Performs a spatial lookup using the R-Tree virtual table joined with locations for O(log n) query performance. }
procedure DemoRTreeSpatialIndex;
var
  DS: TDataSet;
  MinLat, MaxLat, MinLon, MaxLon: Double;
begin
  WriteLn('3. R-Tree Spatial Index Query');
  WriteLn('   ----------------------------');

  // Paris bounding box
  MinLat := 48.80;
  MaxLat := 48.90;
  MinLon := 2.20;
  MaxLon := 2.45;

  WriteLn('   Using R-Tree for efficient spatial lookup:');

  DS := Connection.ExecuteQuery(
    'SELECT l.name, l.category ' +
    'FROM locations l ' +
    'JOIN locations_rtree r ON l.id = r.id ' +
    'WHERE r.min_lat >= ? AND r.max_lat <= ? ' +
    '  AND r.min_lon >= ? AND r.max_lon <= ? ' +
    'ORDER BY l.name',
    [MinLat, MaxLat, MinLon, MaxLon]);
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   R-Tree provides O(log n) spatial queries vs O(n) table scan.');
  WriteLn;
end;

{ Computes and prints the Haversine great-circle distance from the Eiffel Tower to every other location. }
procedure DemoDistanceCalculation;
var
  DS: TDataSet;
  RefLat, RefLon, LocLat, LocLon, Dist: Double;
begin
  WriteLn('4. Distance Calculation (Haversine)');
  WriteLn('   ----------------------------------');

  // Reference point: Eiffel Tower
  RefLat := 48.8584;
  RefLon := 2.2945;

  WriteLn('   Distance from Eiffel Tower to other locations:');

  DS := Connection.ExecuteQuery(
    'SELECT name, latitude, longitude FROM locations WHERE id != 1 ORDER BY name');
  try
    while not DS.EOF do
    begin
      LocLat := DS.FieldByName('latitude').AsFloat;
      LocLon := DS.FieldByName('longitude').AsFloat;
      Dist := HaversineDistance(RefLat, RefLon, LocLat, LocLon);
      WriteLn(Format('     %s: %.2f km',
        [DS.FieldByName('name').AsString, Dist]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Finds the 5 closest locations to a reference point by computing all distances and sorting them. }
procedure DemoNearestNeighbor;
var
  DS: TDataSet;
  RefLat, RefLon, LocLat, LocLon, Dist: Double;
  Locations: array of record
    Name: string;
    Distance: Double;
  end;
  I, J: Integer;
  TempName: string;
  TempDist: Double;
begin
  WriteLn('5. Nearest Neighbor Query');
  WriteLn('   ------------------------');

  // Find 5 nearest locations to a point
  RefLat := 48.8550;
  RefLon := 2.3400;

  WriteLn(Format('   5 nearest locations to (%.4f, %.4f):', [RefLat, RefLon]));

  // Get all locations and calculate distances
  DS := Connection.ExecuteQuery('SELECT name, latitude, longitude FROM locations');
  try
    SetLength(Locations, 0);
    while not DS.EOF do
    begin
      LocLat := DS.FieldByName('latitude').AsFloat;
      LocLon := DS.FieldByName('longitude').AsFloat;
      Dist := HaversineDistance(RefLat, RefLon, LocLat, LocLon);

      SetLength(Locations, Length(Locations) + 1);
      Locations[High(Locations)].Name := DS.FieldByName('name').AsString;
      Locations[High(Locations)].Distance := Dist;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Simple bubble sort by distance
  for I := 0 to High(Locations) - 1 do
    for J := I + 1 to High(Locations) do
      if Locations[J].Distance < Locations[I].Distance then
      begin
        TempName := Locations[I].Name;
        TempDist := Locations[I].Distance;
        Locations[I].Name := Locations[J].Name;
        Locations[I].Distance := Locations[J].Distance;
        Locations[J].Name := TempName;
        Locations[J].Distance := TempDist;
      end;

  // Show top 5
  for I := 0 to Min(4, High(Locations)) do
    WriteLn(Format('     %d. %s: %.2f km',
      [I + 1, Locations[I].Name, Locations[I].Distance]));

  WriteLn;
end;

{ Finds all locations within 3 km of the Louvre using a bounding-box pre-filter followed by exact Haversine distance check. }
procedure DemoRadiusSearch;
var
  DS: TDataSet;
  RefLat, RefLon, LocLat, LocLon, Dist, RadiusKm: Double;
  DeltaLat, DeltaLon: Double;
begin
  WriteLn('6. Radius Search (Geofencing)');
  WriteLn('   ----------------------------');

  // Find all locations within 3km of Louvre
  RefLat := 48.8606;
  RefLon := 2.3376;
  RadiusKm := 3.0;

  // Approximate bounding box for initial filter
  // 1 degree latitude ~ 111 km
  // 1 degree longitude ~ 111 * cos(lat) km
  DeltaLat := RadiusKm / 111.0;
  DeltaLon := RadiusKm / (111.0 * Cos(DegToRad(RefLat)));

  WriteLn(Format('   Locations within %.1f km of Louvre Museum:', [RadiusKm]));

  // First filter by bounding box, then calculate exact distance
  DS := Connection.ExecuteQuery(
    'SELECT name, category, latitude, longitude ' +
    'FROM locations ' +
    'WHERE latitude BETWEEN ? AND ? AND longitude BETWEEN ? AND ?',
    [RefLat - DeltaLat, RefLat + DeltaLat, RefLon - DeltaLon, RefLon + DeltaLon]);
  try
    while not DS.EOF do
    begin
      LocLat := DS.FieldByName('latitude').AsFloat;
      LocLon := DS.FieldByName('longitude').AsFloat;
      Dist := HaversineDistance(RefLat, RefLon, LocLat, LocLon);

      if Dist <= RadiusKm then
        WriteLn(Format('     [%s] %s: %.2f km',
          [DS.FieldByName('category').AsString,
           DS.FieldByName('name').AsString,
           Dist]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Checks which delivery zone each Paris location falls into by joining locations with zone bounding boxes. }
procedure DemoDeliveryZones;
var
  DS: TDataSet;
  LocLat, LocLon: Double;
begin
  WriteLn('7. Delivery Zone Check (Point in Polygon)');
  WriteLn('   ----------------------------------------');

  WriteLn('   Checking which delivery zone each Paris location belongs to:');

  DS := Connection.ExecuteQuery(
    'SELECT l.name, l.latitude, l.longitude, dz.name as zone_name ' +
    'FROM locations l ' +
    'LEFT JOIN delivery_zones dz ON ' +
    '  l.latitude BETWEEN dz.min_lat AND dz.max_lat AND ' +
    '  l.longitude BETWEEN dz.min_lon AND dz.max_lon ' +
    'WHERE l.latitude BETWEEN 48.80 AND 48.90 ' +
    'ORDER BY l.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s -> %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('zone_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Calculates and prints the Haversine distance for each route by resolving start and end location coordinates. }
procedure DemoRouteDistance;
var
  DS: TDataSet;
  StartLat, StartLon, EndLat, EndLon, Dist: Double;
begin
  WriteLn('8. Route Distance Calculation');
  WriteLn('   ----------------------------');

  WriteLn('   Route distances:');

  DS := Connection.ExecuteQuery(
    'SELECT r.name as route_name, ' +
    '       s.name as start_name, s.latitude as start_lat, s.longitude as start_lon, ' +
    '       e.name as end_name, e.latitude as end_lat, e.longitude as end_lon ' +
    'FROM routes r ' +
    'JOIN locations s ON r.start_location_id = s.id ' +
    'JOIN locations e ON r.end_location_id = e.id');
  try
    while not DS.EOF do
    begin
      StartLat := DS.FieldByName('start_lat').AsFloat;
      StartLon := DS.FieldByName('start_lon').AsFloat;
      EndLat := DS.FieldByName('end_lat').AsFloat;
      EndLon := DS.FieldByName('end_lon').AsFloat;
      Dist := HaversineDistance(StartLat, StartLon, EndLat, EndLon);

      WriteLn(Format('     %s: %s -> %s = %.2f km',
        [DS.FieldByName('route_name').AsString,
         DS.FieldByName('start_name').AsString,
         DS.FieldByName('end_name').AsString,
         Dist]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints a summary of geospatial best practices including R-Tree usage, coordinate storage, and distance computation tips. }
procedure DemoBestPractices;
begin
  WriteLn('9. GeoSpatial Best Practices');
  WriteLn('   ---------------------------');
  WriteLn('   - Use R-Tree for efficient bounding box queries');
  WriteLn('   - Pre-filter with bounding box before exact distance');
  WriteLn('   - Store coordinates as REAL (floating point)');
  WriteLn('   - Use Haversine formula for great-circle distance');
  WriteLn('   - Index latitude and longitude columns');
  WriteLn('   - Consider geohash for clustering nearby points');
  WriteLn('   - For complex polygons, use SpatiaLite extension');
  WriteLn('   - Cache distance calculations when possible');
  WriteLn;
end;

{ Deletes the example database file if it exists on disk. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 61: GeoSpatial Queries ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example61.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupGeoTables;
      InsertSampleData;
      DemoBasicQueries;
      DemoBoundingBoxQuery;
      DemoRTreeSpatialIndex;
      DemoDistanceCalculation;
      DemoNearestNeighbor;
      DemoRadiusSearch;
      DemoDeliveryZones;
      DemoRouteDistance;
      DemoBestPractices;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
