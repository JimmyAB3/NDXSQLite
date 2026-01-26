{===============================================================================
  NDXSQLite Example 50 - Graph Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Graph data storage with adjacency lists
  - Nodes and edges (relationships)
  - Path finding with recursive CTEs
  - Social network patterns
  - Graph traversal and analysis

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program GraphDatabase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the nodes and edges tables with indexes for efficient graph traversal. }
procedure SetupGraphDatabase;
begin
  // Nodes table (vertices)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS nodes (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  node_type TEXT NOT NULL,' +              // person, page, group, etc.
    '  name TEXT NOT NULL,' +
    '  properties TEXT,' +                       // JSON properties
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Edges table (relationships)
  Connection.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS edges (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  from_node INTEGER NOT NULL,' +
    '  to_node INTEGER NOT NULL,' +
    '  edge_type TEXT NOT NULL,' +              // follows, friend, likes, etc.
    '  weight REAL DEFAULT 1.0,' +
    '  properties TEXT,' +                       // JSON properties
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  FOREIGN KEY (from_node) REFERENCES nodes(id),' +
    '  FOREIGN KEY (to_node) REFERENCES nodes(id),' +
    '  UNIQUE(from_node, to_node, edge_type)' +
    ')');

  // Indexes for efficient traversal
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_edges_from ON edges(from_node)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_edges_to ON edges(to_node)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_edges_type ON edges(edge_type)');
  Connection.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_nodes_type ON nodes(node_type)');
end;

// ============================================================================
// Node operations
// ============================================================================

{ Inserts a node with a type, name, and optional JSON properties, and returns its ID. }
function CreateNode(const AType, AName: string; const AProperties: string = ''): Integer;
var
  DS: TDataSet;
begin
  Connection.ExecuteNonQuery(
    'INSERT INTO nodes (node_type, name, properties) VALUES (?, ?, ?)',
    [AType, AName, AProperties]);

  DS := Connection.ExecuteQuery('SELECT last_insert_rowid() as id');
  try
    Result := DS.FieldByName('id').AsInteger;
  finally
    DS.Free;
  end;
end;

{ Looks up a node by name and returns its integer ID, or -1 if not found. }
function GetNodeId(const AName: string): Integer;
var
  DS: TDataSet;
begin
  Result := -1;
  DS := Connection.ExecuteQuery('SELECT id FROM nodes WHERE name = ?', [AName]);
  try
    if not DS.EOF then
      Result := DS.FieldByName('id').AsInteger;
  finally
    DS.Free;
  end;
end;

// ============================================================================
// Edge operations
// ============================================================================

{ Creates an edge between two nodes with a type and optional weight. }
procedure CreateEdge(AFromNode, AToNode: Integer; const AEdgeType: string;
  AWeight: Double = 1.0; const AProperties: string = '');
begin
  Connection.ExecuteNonQuery(
    'INSERT OR IGNORE INTO edges (from_node, to_node, edge_type, weight, properties) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [AFromNode, AToNode, AEdgeType, AWeight, AProperties]);
end;

{ Creates two reciprocal edges between two nodes to represent a bidirectional relationship. }
procedure CreateBidirectionalEdge(ANode1, ANode2: Integer; const AEdgeType: string;
  AWeight: Double = 1.0);
begin
  CreateEdge(ANode1, ANode2, AEdgeType, AWeight);
  CreateEdge(ANode2, ANode1, AEdgeType, AWeight);
end;

// ============================================================================
// Demo procedures
// ============================================================================

{ Creates person nodes and establishes friendship and follow edges to form a sample social graph. }
procedure DemoCreateSocialNetwork;
var
  Alice, Bob, Carol, David, Eve, Frank: Integer;
begin
  WriteLn('1. Creating social network graph');
  WriteLn('   -----------------------------');
  WriteLn('');

  // Create person nodes
  Alice := CreateNode('person', 'Alice', '{"age": 30, "city": "New York"}');
  Bob := CreateNode('person', 'Bob', '{"age": 25, "city": "Los Angeles"}');
  Carol := CreateNode('person', 'Carol', '{"age": 35, "city": "Chicago"}');
  David := CreateNode('person', 'David', '{"age": 28, "city": "Boston"}');
  Eve := CreateNode('person', 'Eve', '{"age": 32, "city": "Seattle"}');
  Frank := CreateNode('person', 'Frank', '{"age": 27, "city": "Denver"}');

  WriteLn('   Created 6 person nodes');

  // Create friendship edges (bidirectional)
  CreateBidirectionalEdge(Alice, Bob, 'friend');
  CreateBidirectionalEdge(Alice, Carol, 'friend');
  CreateBidirectionalEdge(Bob, David, 'friend');
  CreateBidirectionalEdge(Carol, David, 'friend');
  CreateBidirectionalEdge(Carol, Eve, 'friend');
  CreateBidirectionalEdge(David, Eve, 'friend');
  CreateBidirectionalEdge(Eve, Frank, 'friend');

  WriteLn('   Created friendship relationships');

  // Create follow edges (directional)
  CreateEdge(Alice, Eve, 'follows');
  CreateEdge(Bob, Alice, 'follows');
  CreateEdge(Bob, Carol, 'follows');
  CreateEdge(Frank, Alice, 'follows');
  CreateEdge(David, Alice, 'follows');

  WriteLn('   Created follow relationships');
  WriteLn('');
end;

{ Queries and displays Alice's direct friends and followers (1-hop relationships). }
procedure DemoDirectConnections;
var
  DS: TDataSet;
begin
  WriteLn('2. Direct connections (1-hop)');
  WriteLn('   -------------------------');
  WriteLn('');

  // Get Alice's friends
  WriteLn('   Alice''s friends:');
  DS := Connection.ExecuteQuery(
    'SELECT n.name FROM edges e ' +
    'JOIN nodes n ON e.to_node = n.id ' +
    'WHERE e.from_node = (SELECT id FROM nodes WHERE name = ''Alice'') ' +
    '  AND e.edge_type = ''friend''');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Get Alice's followers
  WriteLn('   Alice''s followers:');
  DS := Connection.ExecuteQuery(
    'SELECT n.name FROM edges e ' +
    'JOIN nodes n ON e.from_node = n.id ' +
    'WHERE e.to_node = (SELECT id FROM nodes WHERE name = ''Alice'') ' +
    '  AND e.edge_type = ''follows''');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Finds Alice's 2-hop connections that are not already direct friends. }
procedure DemoFriendsOfFriends;
var
  DS: TDataSet;
begin
  WriteLn('3. Friends of friends (2-hop)');
  WriteLn('   -------------------------');
  WriteLn('');

  WriteLn('   Alice''s friends of friends (not direct friends):');
  DS := Connection.ExecuteQuery(
    'SELECT DISTINCT n2.name ' +
    'FROM edges e1 ' +
    'JOIN edges e2 ON e1.to_node = e2.from_node ' +
    'JOIN nodes n2 ON e2.to_node = n2.id ' +
    'WHERE e1.from_node = (SELECT id FROM nodes WHERE name = ''Alice'') ' +
    '  AND e1.edge_type = ''friend'' ' +
    '  AND e2.edge_type = ''friend'' ' +
    '  AND e2.to_node != e1.from_node ' +  // Not Alice herself
    '  AND e2.to_node NOT IN (' +           // Not already a direct friend
    '    SELECT to_node FROM edges ' +
    '    WHERE from_node = e1.from_node AND edge_type = ''friend'')');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Finds and displays nodes that are friends of both Alice and David. }
procedure DemoMutualFriends;
var
  DS: TDataSet;
begin
  WriteLn('4. Mutual friends');
  WriteLn('   --------------');
  WriteLn('');

  WriteLn('   Mutual friends of Alice and David:');
  DS := Connection.ExecuteQuery(
    'SELECT n.name ' +
    'FROM edges e1 ' +
    'JOIN edges e2 ON e1.to_node = e2.to_node ' +
    'JOIN nodes n ON e1.to_node = n.id ' +
    'WHERE e1.from_node = (SELECT id FROM nodes WHERE name = ''Alice'') ' +
    '  AND e2.from_node = (SELECT id FROM nodes WHERE name = ''David'') ' +
    '  AND e1.edge_type = ''friend'' ' +
    '  AND e2.edge_type = ''friend''');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Uses a recursive CTE to find the shortest friend-edge path from Alice to Frank. }
procedure DemoShortestPath;
var
  DS: TDataSet;
begin
  WriteLn('5. Shortest path (BFS with recursive CTE)');
  WriteLn('   --------------------------------------');
  WriteLn('');

  WriteLn('   Shortest path from Alice to Frank:');
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE path(node_id, path, depth) AS (' +
    '  SELECT id, name, 0 FROM nodes WHERE name = ''Alice'' ' +
    '  UNION ALL ' +
    '  SELECT e.to_node, path.path || '' -> '' || n.name, path.depth + 1 ' +
    '  FROM path ' +
    '  JOIN edges e ON path.node_id = e.from_node ' +
    '  JOIN nodes n ON e.to_node = n.id ' +
    '  WHERE e.edge_type = ''friend'' ' +
    '    AND path.depth < 6 ' +
    '    AND path.path NOT LIKE ''%'' || n.name || ''%''' +  // Avoid cycles
    ') ' +
    'SELECT path, depth FROM path ' +
    'WHERE node_id = (SELECT id FROM nodes WHERE name = ''Frank'') ' +
    'ORDER BY depth LIMIT 1');
  try
    if not DS.EOF then
    begin
      WriteLn('   Path: ', DS.FieldByName('path').AsString);
      WriteLn('   Distance: ', DS.FieldByName('depth').AsInteger, ' hops');
    end
    else
      WriteLn('   No path found');
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Uses a recursive CTE to enumerate all acyclic friend-edge paths from Alice to Eve within 4 hops. }
procedure DemoAllPaths;
var
  DS: TDataSet;
begin
  WriteLn('6. All paths between two nodes');
  WriteLn('   ---------------------------');
  WriteLn('');

  WriteLn('   All paths from Alice to Eve (max 4 hops):');
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE paths(node_id, path, depth) AS (' +
    '  SELECT id, name, 0 FROM nodes WHERE name = ''Alice'' ' +
    '  UNION ALL ' +
    '  SELECT e.to_node, paths.path || '' -> '' || n.name, paths.depth + 1 ' +
    '  FROM paths ' +
    '  JOIN edges e ON paths.node_id = e.from_node ' +
    '  JOIN nodes n ON e.to_node = n.id ' +
    '  WHERE e.edge_type = ''friend'' ' +
    '    AND paths.depth < 4 ' +
    '    AND paths.path NOT LIKE ''%'' || n.name || ''%''' +
    ') ' +
    'SELECT path, depth FROM paths ' +
    'WHERE node_id = (SELECT id FROM nodes WHERE name = ''Eve'') ' +
    'ORDER BY depth');
  try
    while not DS.EOF do
    begin
      WriteLn('   [', DS.FieldByName('depth').AsInteger, ' hops] ',
              DS.FieldByName('path').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays friend connection degrees per node and identifies the most-followed persons. }
procedure DemoGraphMetrics;
var
  DS: TDataSet;
begin
  WriteLn('7. Graph metrics');
  WriteLn('   -------------');
  WriteLn('');

  // Node degree (connections count)
  WriteLn('   Node degrees (friend connections):');
  DS := Connection.ExecuteQuery(
    'SELECT n.name, ' +
    '       COUNT(DISTINCT e_out.to_node) as out_degree, ' +
    '       COUNT(DISTINCT e_in.from_node) as in_degree ' +
    'FROM nodes n ' +
    'LEFT JOIN edges e_out ON n.id = e_out.from_node AND e_out.edge_type = ''friend'' ' +
    'LEFT JOIN edges e_in ON n.id = e_in.to_node AND e_in.edge_type = ''friend'' ' +
    'WHERE n.node_type = ''person'' ' +
    'GROUP BY n.id ' +
    'ORDER BY out_degree DESC');
  try
    WriteLn('   Name       Out  In');
    WriteLn('   ----       ---  --');
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('name').AsString:10,
              DS.FieldByName('out_degree').AsInteger:4,
              DS.FieldByName('in_degree').AsInteger:4);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');

  // Most followed
  WriteLn('   Most followed persons:');
  DS := Connection.ExecuteQuery(
    'SELECT n.name, COUNT(*) as followers ' +
    'FROM edges e ' +
    'JOIN nodes n ON e.to_node = n.id ' +
    'WHERE e.edge_type = ''follows'' ' +
    'GROUP BY e.to_node ' +
    'ORDER BY followers DESC LIMIT 3');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString, ': ',
              DS.FieldByName('followers').AsInteger, ' followers');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Displays total node/edge counts and the distribution of edges by type. }
procedure DemoConnectedComponents;
var
  DS: TDataSet;
begin
  WriteLn('8. Graph statistics');
  WriteLn('   ----------------');
  WriteLn('');

  // Total nodes and edges
  DS := Connection.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM nodes) as total_nodes, ' +
    '  (SELECT COUNT(*) FROM edges) as total_edges, ' +
    '  (SELECT COUNT(DISTINCT edge_type) FROM edges) as edge_types');
  try
    WriteLn('   Total nodes: ', DS.FieldByName('total_nodes').AsInteger);
    WriteLn('   Total edges: ', DS.FieldByName('total_edges').AsInteger);
    WriteLn('   Edge types: ', DS.FieldByName('edge_types').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn('');

  // Edge type distribution
  WriteLn('   Edge type distribution:');
  DS := Connection.ExecuteQuery(
    'SELECT edge_type, COUNT(*) as count FROM edges GROUP BY edge_type');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('edge_type').AsString, ': ',
              DS.FieldByName('count').AsInteger);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Demonstrates graph reachability analysis using recursive traversal. }
procedure DemoReachability;
var
  DS: TDataSet;
begin
  WriteLn('9. Reachability analysis');
  WriteLn('   ---------------------');
  WriteLn('');

  WriteLn('   All nodes reachable from Alice (via friend edges):');
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE reachable(node_id, depth) AS (' +
    '  SELECT id, 0 FROM nodes WHERE name = ''Alice'' ' +
    '  UNION ' +
    '  SELECT e.to_node, r.depth + 1 ' +
    '  FROM reachable r ' +
    '  JOIN edges e ON r.node_id = e.from_node ' +
    '  WHERE e.edge_type = ''friend'' AND r.depth < 10' +
    ') ' +
    'SELECT DISTINCT n.name, MIN(r.depth) as distance ' +
    'FROM reachable r ' +
    'JOIN nodes n ON r.node_id = n.id ' +
    'WHERE n.name != ''Alice'' ' +
    'GROUP BY n.id ' +
    'ORDER BY distance, n.name');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ', DS.FieldByName('name').AsString,
              ' (distance: ', DS.FieldByName('distance').AsInteger, ')');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Creates a server network topology with latency-weighted edges and lists links sorted by latency. }
procedure DemoWeightedGraph;
var
  ServerA, ServerB, ServerC, ServerD, ServerE: Integer;
  DS: TDataSet;
begin
  WriteLn('10. Weighted graph (network topology)');
  WriteLn('    ---------------------------------');
  WriteLn('');

  // Create network nodes
  ServerA := CreateNode('server', 'ServerA', '{"location": "US-East"}');
  ServerB := CreateNode('server', 'ServerB', '{"location": "US-West"}');
  ServerC := CreateNode('server', 'ServerC', '{"location": "EU"}');
  ServerD := CreateNode('server', 'ServerD', '{"location": "Asia"}');
  ServerE := CreateNode('server', 'ServerE', '{"location": "US-Central"}');

  // Create weighted edges (latency in ms)
  CreateEdge(ServerA, ServerB, 'link', 50);
  CreateEdge(ServerB, ServerA, 'link', 50);
  CreateEdge(ServerA, ServerC, 'link', 100);
  CreateEdge(ServerC, ServerA, 'link', 100);
  CreateEdge(ServerA, ServerE, 'link', 20);
  CreateEdge(ServerE, ServerA, 'link', 20);
  CreateEdge(ServerB, ServerD, 'link', 150);
  CreateEdge(ServerD, ServerB, 'link', 150);
  CreateEdge(ServerC, ServerD, 'link', 80);
  CreateEdge(ServerD, ServerC, 'link', 80);
  CreateEdge(ServerE, ServerB, 'link', 30);
  CreateEdge(ServerB, ServerE, 'link', 30);

  WriteLn('   Created network topology with latency weights');
  WriteLn('');

  // Find path with lowest latency
  WriteLn('   Network links sorted by latency:');
  DS := Connection.ExecuteQuery(
    'SELECT n1.name as from_server, n2.name as to_server, e.weight as latency ' +
    'FROM edges e ' +
    'JOIN nodes n1 ON e.from_node = n1.id ' +
    'JOIN nodes n2 ON e.to_node = n2.id ' +
    'WHERE e.edge_type = ''link'' AND n1.name < n2.name ' +
    'ORDER BY e.weight');
  try
    while not DS.EOF do
    begin
      WriteLn('   ', DS.FieldByName('from_server').AsString, ' <-> ',
              DS.FieldByName('to_server').AsString, ': ',
              DS.FieldByName('latency').AsFloat:0:0, 'ms');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the database file and its WAL/SHM journal files from disk. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
  if FileExists(DBPath + '-wal') then DeleteFile(DBPath + '-wal');
  if FileExists(DBPath + '-shm') then DeleteFile(DBPath + '-shm');
end;

begin
  WriteLn('=== NDXSQLite Example 50: Graph Database ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example50.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupGraphDatabase;
      WriteLn('Graph database initialized');
      WriteLn('');

      DemoCreateSocialNetwork;
      DemoDirectConnections;
      DemoFriendsOfFriends;
      DemoMutualFriends;
      DemoShortestPath;
      DemoAllPaths;
      DemoGraphMetrics;
      DemoConnectedComponents;
      DemoReachability;
      DemoWeightedGraph;

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
