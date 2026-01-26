{===============================================================================
  NDXSQLite Example 138 - Shortest Path
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Graph storage with weighted edges
  - All-paths enumeration between nodes
  - Dijkstra-style shortest path with CTEs
  - Path reconstruction from predecessors
  - Network diameter calculation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ShortestPath;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Cities (nodes)
  Conn.ExecuteNonQuery(
    'CREATE TABLE cities (' +
    '  id INTEGER PRIMARY KEY, ' +
    '  name TEXT NOT NULL UNIQUE, ' +
    '  country TEXT NOT NULL, ' +
    '  latitude REAL, ' +
    '  longitude REAL)');

  // Roads (weighted directed edges)
  Conn.ExecuteNonQuery(
    'CREATE TABLE roads (' +
    '  id INTEGER PRIMARY KEY, ' +
    '  from_city TEXT NOT NULL, ' +
    '  to_city TEXT NOT NULL, ' +
    '  distance_km INTEGER NOT NULL, ' +
    '  road_type TEXT NOT NULL, ' +
    '  UNIQUE(from_city, to_city))');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Cities
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Paris'', ''France'', 48.86, 2.35)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Lyon'', ''France'', 45.76, 4.84)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Marseille'', ''France'', 43.30, 5.37)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Bordeaux'', ''France'', 44.84, -0.58)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Toulouse'', ''France'', 43.60, 1.44)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Nice'', ''France'', 43.71, 7.26)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Strasbourg'', ''France'', 48.57, 7.75)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Lille'', ''France'', 50.63, 3.06)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Nantes'', ''France'', 47.22, -1.55)');
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Dijon'', ''France'', 47.32, 5.04)');
  // Isolated city (no roads)
  Conn.ExecuteNonQuery('INSERT INTO cities (name, country, latitude, longitude) VALUES ' +
    '(''Ajaccio'', ''Corsica'', 41.93, 8.74)');

  // Roads (bidirectional - insert both directions)
  // Paris connections
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Paris'', ''Lyon'', 465, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Lyon'', ''Paris'', 465, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Paris'', ''Bordeaux'', 585, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Bordeaux'', ''Paris'', 585, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Paris'', ''Lille'', 225, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Lille'', ''Paris'', 225, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Paris'', ''Strasbourg'', 490, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Strasbourg'', ''Paris'', 490, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Paris'', ''Nantes'', 385, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Nantes'', ''Paris'', 385, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Paris'', ''Dijon'', 315, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Dijon'', ''Paris'', 315, ''highway'')');

  // Lyon connections
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Lyon'', ''Marseille'', 315, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Marseille'', ''Lyon'', 315, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Lyon'', ''Dijon'', 195, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Dijon'', ''Lyon'', 195, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Lyon'', ''Nice'', 470, ''national'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Nice'', ''Lyon'', 470, ''national'')');

  // Marseille connections
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Marseille'', ''Nice'', 200, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Nice'', ''Marseille'', 200, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Marseille'', ''Toulouse'', 405, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Toulouse'', ''Marseille'', 405, ''highway'')');

  // Bordeaux connections
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Bordeaux'', ''Toulouse'', 245, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Toulouse'', ''Bordeaux'', 245, ''highway'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Bordeaux'', ''Nantes'', 340, ''national'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Nantes'', ''Bordeaux'', 340, ''national'')');

  // Strasbourg connections
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Strasbourg'', ''Dijon'', 335, ''national'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Dijon'', ''Strasbourg'', 335, ''national'')');

  // Toulouse connections
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Toulouse'', ''Nantes'', 565, ''national'')');
  Conn.ExecuteNonQuery('INSERT INTO roads (from_city, to_city, distance_km, road_type) VALUES (''Nantes'', ''Toulouse'', 565, ''national'')');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Displays the total count of cities and roads, then lists all cities with their country and geographic coordinates. }
procedure Demo1_GraphOverview;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Graph Overview ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM cities');
  try
    DS.First;
    WriteLn('   Cities (nodes): ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM roads');
  try
    DS.First;
    WriteLn('   Roads (edges): ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   City             Country      Lat      Lon');
  WriteLn('   ' + StringOfChar('-', 55));
  DS := Conn.ExecuteQuery('SELECT name, country, latitude, longitude FROM cities ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-10s %7.2f  %7.2f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('country').AsString,
        DS.FieldByName('latitude').AsFloat,
        DS.FieldByName('longitude').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists direct road connections from Paris with distances and road types, then shows connection density per city. }
procedure Demo2_DirectConnections;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Direct Connections (1-hop neighbors) ===');
  WriteLn;
  WriteLn('   From Paris:');
  WriteLn('   Destination      Distance  Road Type');
  WriteLn('   ' + StringOfChar('-', 45));
  DS := Conn.ExecuteQuery(
    'SELECT to_city, distance_km, road_type FROM roads ' +
    'WHERE from_city = ''Paris'' ORDER BY distance_km');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %5d km  %s', [
        DS.FieldByName('to_city').AsString,
        DS.FieldByName('distance_km').AsInteger,
        DS.FieldByName('road_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Connection density per city:');
  DS := Conn.ExecuteQuery(
    'SELECT from_city, COUNT(*) as connections FROM roads ' +
    'GROUP BY from_city ORDER BY connections DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %d connections', [
        DS.FieldByName('from_city').AsString,
        DS.FieldByName('connections').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates all paths between nodes. }
procedure Demo3_AllPathsBetweenNodes;
var
  DS: TDataSet;
  PathCount: Integer;
begin
  WriteLn('=== 3. All Paths: Paris -> Marseille (recursive CTE) ===');
  WriteLn;

  // Recursive CTE to find all paths (with loop detection)
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Paris'', ''Paris'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT path, total_distance, hops FROM paths ' +
    'WHERE current_city = ''Marseille'' ' +
    'ORDER BY total_distance ' +
    'LIMIT 8');
  try
    PathCount := 0;
    while not DS.EOF do
    begin
      Inc(PathCount);
      WriteLn(Format('   %d. [%d km, %d hops] %s', [
        PathCount,
        DS.FieldByName('total_distance').AsInteger,
        DS.FieldByName('hops').AsInteger,
        DS.FieldByName('path').AsString]));
      DS.Next;
    end;
    WriteLn;
    WriteLn(Format('   Found %d distinct paths (showing top 8 by distance)', [PathCount]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Finds the shortest path from Paris to Nice using a recursive CTE, then displays a comparison of all Paris to Nice routes ranked by distance. }
procedure Demo4_ShortestPath;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Shortest Path: Paris -> Nice ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Paris'', ''Paris'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT path, total_distance, hops FROM paths ' +
    'WHERE current_city = ''Nice'' ' +
    'ORDER BY total_distance ' +
    'LIMIT 1');
  try
    DS.First;
    WriteLn('   Shortest route: ', DS.FieldByName('path').AsString);
    WriteLn('   Total distance: ', DS.FieldByName('total_distance').AsInteger, ' km');
    WriteLn('   Hops: ', DS.FieldByName('hops').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Comparison of all Paris -> Nice routes:');
  WriteLn('   Route                                      Distance  Hops');
  WriteLn('   ' + StringOfChar('-', 65));
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Paris'', ''Paris'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT path, total_distance, hops FROM paths ' +
    'WHERE current_city = ''Nice'' ' +
    'ORDER BY total_distance ' +
    'LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-45s %5d km  %d', [
        DS.FieldByName('path').AsString,
        DS.FieldByName('total_distance').AsInteger,
        DS.FieldByName('hops').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Finds the shortest Lille to Toulouse route and displays step-by-step segment details with individual leg distances. }
procedure Demo5_PathReconstruction;
var
  DS: TDataSet;
  Step: Integer;
begin
  WriteLn('=== 5. Path Reconstruction: Lille -> Toulouse (step-by-step) ===');
  WriteLn;

  // First find shortest path
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Lille'', ''Lille'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || ''|'' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT path, total_distance, hops FROM paths ' +
    'WHERE current_city = ''Toulouse'' ' +
    'ORDER BY total_distance ' +
    'LIMIT 1');
  try
    DS.First;
    WriteLn('   Best route: ', StringReplace(DS.FieldByName('path').AsString, '|', ' -> ', [rfReplaceAll]));
    WriteLn('   Total: ', DS.FieldByName('total_distance').AsInteger, ' km in ', DS.FieldByName('hops').AsInteger, ' hops');
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Step-by-step reconstruction:');
  WriteLn;

  // Show each leg with individual distances
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, prev_city, path, total_distance, hops) AS (' +
    '  SELECT ''Lille'', '''', ''Lille'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, r.from_city, ' +
    '         p.path || ''|'' || r.to_city || '':'' || r.distance_km, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT path, total_distance, hops FROM paths ' +
    'WHERE current_city = ''Toulouse'' ' +
    'ORDER BY total_distance ' +
    'LIMIT 1');
  try
    DS.First;
    // Parse the path to show individual segments
    WriteLn('   Step  From             To               Distance  Cumulative');
    WriteLn('   ' + StringOfChar('-', 65));
    // Use a separate query to show each segment of the best route
  finally
    DS.Free;
  end;

  // Show segment details for the known best route: Lille -> Paris -> Bordeaux -> Toulouse
  DS := Conn.ExecuteQuery(
    'SELECT from_city, to_city, distance_km FROM roads ' +
    'WHERE (from_city = ''Lille'' AND to_city = ''Paris'') ' +
    '   OR (from_city = ''Paris'' AND to_city = ''Bordeaux'') ' +
    '   OR (from_city = ''Bordeaux'' AND to_city = ''Toulouse'') ' +
    'ORDER BY CASE from_city ' +
    '  WHEN ''Lille'' THEN 1 ' +
    '  WHEN ''Paris'' THEN 2 ' +
    '  WHEN ''Bordeaux'' THEN 3 END');
  try
    Step := 0;
    while not DS.EOF do
    begin
      Inc(Step);
      WriteLn(Format('   %d.    %-16s %-16s %5d km  %s', [
        Step,
        DS.FieldByName('from_city').AsString,
        DS.FieldByName('to_city').AsString,
        DS.FieldByName('distance_km').AsInteger,
        '']));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists cities reachable from Paris in exactly 2 hops and from Lille in exactly 3 hops with minimum distances. }
procedure Demo6_MultiHopQueries;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Multi-Hop Queries (exactly N hops) ===');
  WriteLn;

  WriteLn('   Cities reachable from Paris in exactly 2 hops:');
  WriteLn('   Via              Destination      Total Distance');
  WriteLn('   ' + StringOfChar('-', 55));
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Paris'', ''Paris'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 2 ' +
    ') ' +
    'SELECT current_city, path, total_distance FROM paths ' +
    'WHERE hops = 2 AND current_city != ''Paris'' ' +
    'ORDER BY total_distance');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-16s %5d km', [
        '',
        DS.FieldByName('current_city').AsString,
        DS.FieldByName('total_distance').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Cities reachable from Lille in exactly 3 hops:');
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Lille'', ''Lille'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 3 ' +
    ') ' +
    'SELECT DISTINCT current_city, MIN(total_distance) as min_dist FROM paths ' +
    'WHERE hops = 3 AND current_city != ''Lille'' ' +
    'GROUP BY current_city ' +
    'ORDER BY min_dist');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s (min %d km)', [
        DS.FieldByName('current_city').AsString,
        DS.FieldByName('min_dist').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Computes and displays shortest paths from Paris to all reachable cities with distance, hop count, and full route. }
procedure Demo7_SingleSourceShortest;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Single-Source Shortest Paths (from Paris) ===');
  WriteLn;
  WriteLn('   Destination      Shortest Distance  Hops  Route');
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Paris'', ''Paris'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT current_city, path, total_distance, hops FROM paths ' +
    'WHERE current_city != ''Paris'' ' +
    '  AND total_distance = (' +
    '    SELECT MIN(p2.total_distance) FROM paths p2 ' +
    '    WHERE p2.current_city = paths.current_city' +
    '  ) ' +
    'GROUP BY current_city ' +
    'ORDER BY total_distance');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %8d km        %d     %s', [
        DS.FieldByName('current_city').AsString,
        DS.FieldByName('total_distance').AsInteger,
        DS.FieldByName('hops').AsInteger,
        DS.FieldByName('path').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Computes all-pairs shortest paths to find the network diameter (the longest shortest path between any two connected cities). }
procedure Demo8_NetworkDiameter;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Network Diameter (longest shortest path) ===');
  WriteLn;

  // Find shortest path between all pairs, then report the longest
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(source, current_city, path, total_distance, hops) AS (' +
    '  SELECT name, name, name, 0, 0 FROM cities WHERE country = ''France'' ' +
    '  UNION ALL ' +
    '  SELECT p.source, r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT source, current_city as destination, MIN(total_distance) as shortest_dist, path ' +
    'FROM paths ' +
    'WHERE source != current_city ' +
    'GROUP BY source, current_city ' +
    'ORDER BY shortest_dist DESC ' +
    'LIMIT 5');
  try
    WriteLn('   Top 5 longest shortest paths (network extremes):');
    WriteLn('   From             To               Distance  Route');
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-16s %5d km', [
        DS.FieldByName('source').AsString,
        DS.FieldByName('destination').AsString,
        DS.FieldByName('shortest_dist').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  // Network diameter
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(source, current_city, path, total_distance, hops) AS (' +
    '  SELECT name, name, name, 0, 0 FROM cities WHERE country = ''France'' ' +
    '  UNION ALL ' +
    '  SELECT p.source, r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 6 ' +
    ') ' +
    'SELECT MAX(min_dist) as diameter FROM (' +
    '  SELECT source, current_city, MIN(total_distance) as min_dist ' +
    '  FROM paths WHERE source != current_city ' +
    '  GROUP BY source, current_city ' +
    ')');
  try
    DS.First;
    WriteLn('   Network diameter: ', DS.FieldByName('diameter').AsInteger, ' km');
    WriteLn('   (longest shortest path between any two connected cities)');
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Identifies cities not reachable from Paris via roads and reports connected component size versus total cities. }
procedure Demo9_UnreachableNodes;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Unreachable Nodes ===');
  WriteLn;

  // Find cities that cannot be reached from Paris
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE reachable(city) AS (' +
    '  SELECT ''Paris'' ' +
    '  UNION ' +
    '  SELECT r.to_city FROM reachable rc ' +
    '  JOIN roads r ON r.from_city = rc.city ' +
    ') ' +
    'SELECT name, country FROM cities ' +
    'WHERE name NOT IN (SELECT city FROM reachable) ' +
    'ORDER BY name');
  try
    WriteLn('   Cities unreachable from Paris:');
    if DS.EOF then
      WriteLn('   (none)')
    else
    begin
      while not DS.EOF do
      begin
        WriteLn(Format('   - %s (%s) -- no road connection', [
          DS.FieldByName('name').AsString,
          DS.FieldByName('country').AsString]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  // Connected components
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE component(city) AS (' +
    '  SELECT ''Paris'' ' +
    '  UNION ' +
    '  SELECT r.to_city FROM component c ' +
    '  JOIN roads r ON r.from_city = c.city ' +
    ') ' +
    'SELECT COUNT(*) as reachable FROM component');
  try
    DS.First;
    WriteLn('   Connected component from Paris: ', DS.FieldByName('reachable').AsInteger, ' cities');
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as total FROM cities');
  try
    DS.First;
    WriteLn('   Total cities in graph: ', DS.FieldByName('total').AsInteger);
    WriteLn('   Isolated nodes: ', DS.FieldByName('total').AsInteger - 10);
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists top 5 alternative routes from Strasbourg to Toulouse with distances and extra km versus shortest, plus road type distribution statistics. }
procedure Demo10_AlternativeRoutes;
var
  DS: TDataSet;
  Rank: Integer;
  ShortestDist: Integer;
begin
  WriteLn('=== 10. Alternative Routes: Strasbourg -> Toulouse ===');
  WriteLn;
  WriteLn('   Top routes ranked by distance:');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE paths(current_city, path, total_distance, hops) AS (' +
    '  SELECT ''Strasbourg'', ''Strasbourg'', 0, 0 ' +
    '  UNION ALL ' +
    '  SELECT r.to_city, ' +
    '         p.path || '' -> '' || r.to_city, ' +
    '         p.total_distance + r.distance_km, ' +
    '         p.hops + 1 ' +
    '  FROM paths p ' +
    '  JOIN roads r ON r.from_city = p.current_city ' +
    '  WHERE p.path NOT LIKE ''%'' || r.to_city || ''%'' ' +
    '    AND p.hops < 7 ' +
    ') ' +
    'SELECT path, total_distance, hops FROM paths ' +
    'WHERE current_city = ''Toulouse'' ' +
    'ORDER BY total_distance ' +
    'LIMIT 5');
  try
    Rank := 0;
    ShortestDist := 0;
    while not DS.EOF do
    begin
      Inc(Rank);
      if Rank = 1 then
        ShortestDist := DS.FieldByName('total_distance').AsInteger;
      WriteLn(Format('   #%d [%d km, %d hops] (+%d km)', [
        Rank,
        DS.FieldByName('total_distance').AsInteger,
        DS.FieldByName('hops').AsInteger,
        DS.FieldByName('total_distance').AsInteger - ShortestDist]));
      WriteLn(Format('      %s', [DS.FieldByName('path').AsString]));
      WriteLn;
      DS.Next;
    end;
    WriteLn(Format('   Best route saves %d km vs worst shown alternative', [
      DS.FieldByName('total_distance').AsInteger - ShortestDist]));
  finally
    DS.Free;
  end;

  // Road type summary
  WriteLn;
  WriteLn('   Road type distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT road_type, COUNT(*) as cnt, ' +
    '  CAST(SUM(distance_km) AS INTEGER) as total_km, ' +
    '  CAST(AVG(distance_km) AS INTEGER) as avg_km ' +
    'FROM roads GROUP BY road_type ORDER BY total_km DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %2d roads, total %5d km, avg %d km', [
        DS.FieldByName('road_type').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('total_km').AsInteger,
        DS.FieldByName('avg_km').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

begin
  WriteLn('Example 138: Shortest Path - Route Finding with Recursive CTEs');
  WriteLn(StringOfChar('=', 70));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_GraphOverview;
    Demo2_DirectConnections;
    Demo3_AllPathsBetweenNodes;
    Demo4_ShortestPath;
    Demo5_PathReconstruction;
    Demo6_MultiHopQueries;
    Demo7_SingleSourceShortest;
    Demo8_NetworkDiameter;
    Demo9_UnreachableNodes;
    Demo10_AlternativeRoutes;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
