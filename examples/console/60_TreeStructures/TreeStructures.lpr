{===============================================================================
  NDXSQLite Example 60 - Tree Structures
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates hierarchical data patterns:
  - Adjacency List (parent_id reference)
  - Nested Sets (left/right boundaries)
  - Materialized Path (path string)
  - Closure Table (ancestor-descendant pairs)
  - Recursive CTEs for tree traversal

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program TreeStructures;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates four tree representation tables (adjacency list, nested sets, materialized path, closure table) with appropriate indexes for each model. }
procedure SetupTreeTables;
begin
  // 1. Adjacency List Model (most common)
  Connection.ExecuteNonQuery(
    'CREATE TABLE categories_adj (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  parent_id INTEGER,' +
    '  FOREIGN KEY (parent_id) REFERENCES categories_adj(id)' +
    ')');

  // 2. Nested Sets Model
  Connection.ExecuteNonQuery(
    'CREATE TABLE categories_nested (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  lft INTEGER NOT NULL,' +
    '  rgt INTEGER NOT NULL' +
    ')');

  // 3. Materialized Path Model
  Connection.ExecuteNonQuery(
    'CREATE TABLE categories_path (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  path TEXT NOT NULL,' +
    '  depth INTEGER NOT NULL' +
    ')');

  // 4. Closure Table Model
  Connection.ExecuteNonQuery(
    'CREATE TABLE categories_closure_nodes (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE TABLE categories_closure (' +
    '  ancestor_id INTEGER NOT NULL,' +
    '  descendant_id INTEGER NOT NULL,' +
    '  depth INTEGER NOT NULL,' +
    '  PRIMARY KEY (ancestor_id, descendant_id),' +
    '  FOREIGN KEY (ancestor_id) REFERENCES categories_closure_nodes(id),' +
    '  FOREIGN KEY (descendant_id) REFERENCES categories_closure_nodes(id)' +
    ')');

  // Indexes
  Connection.ExecuteNonQuery('CREATE INDEX idx_adj_parent ON categories_adj(parent_id)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_nested_lft ON categories_nested(lft)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_nested_rgt ON categories_nested(rgt)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_path ON categories_path(path)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_closure_anc ON categories_closure(ancestor_id)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_closure_desc ON categories_closure(descendant_id)');
end;

{ Populates all four tree tables with the same 10-node category hierarchy (Electronics, Clothing, and their subcategories) using each model's representation. }
procedure InsertSampleData;
begin
  // Sample tree structure:
  //   1. Electronics
  //      2. Computers
  //         4. Laptops
  //         5. Desktops
  //      3. Phones
  //         6. Smartphones
  //         7. Feature Phones
  //   8. Clothing
  //      9. Men
  //      10. Women

  // 1. Adjacency List
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (1, ''Electronics'', NULL)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (2, ''Computers'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (3, ''Phones'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (4, ''Laptops'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (5, ''Desktops'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (6, ''Smartphones'', 3)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (7, ''Feature Phones'', 3)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (8, ''Clothing'', NULL)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (9, ''Men'', 8)');
  Connection.ExecuteNonQuery('INSERT INTO categories_adj VALUES (10, ''Women'', 8)');

  // 2. Nested Sets (preorder traversal: left when entering, right when leaving)
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (1, ''Electronics'', 1, 14)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (2, ''Computers'', 2, 7)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (4, ''Laptops'', 3, 4)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (5, ''Desktops'', 5, 6)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (3, ''Phones'', 8, 13)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (6, ''Smartphones'', 9, 10)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (7, ''Feature Phones'', 11, 12)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (8, ''Clothing'', 15, 20)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (9, ''Men'', 16, 17)');
  Connection.ExecuteNonQuery('INSERT INTO categories_nested VALUES (10, ''Women'', 18, 19)');

  // 3. Materialized Path
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (1, ''Electronics'', ''/1/'', 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (2, ''Computers'', ''/1/2/'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (3, ''Phones'', ''/1/3/'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (4, ''Laptops'', ''/1/2/4/'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (5, ''Desktops'', ''/1/2/5/'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (6, ''Smartphones'', ''/1/3/6/'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (7, ''Feature Phones'', ''/1/3/7/'', 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (8, ''Clothing'', ''/8/'', 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (9, ''Men'', ''/8/9/'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_path VALUES (10, ''Women'', ''/8/10/'', 1)');

  // 4. Closure Table
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (1, ''Electronics'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (2, ''Computers'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (3, ''Phones'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (4, ''Laptops'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (5, ''Desktops'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (6, ''Smartphones'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (7, ''Feature Phones'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (8, ''Clothing'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (9, ''Men'')');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure_nodes VALUES (10, ''Women'')');

  // Self-references (depth 0)
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (1, 1, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (2, 2, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (3, 3, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (4, 4, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (5, 5, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (6, 6, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (7, 7, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (8, 8, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (9, 9, 0)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (10, 10, 0)');

  // Parent-child (depth 1)
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (1, 2, 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (1, 3, 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (2, 4, 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (2, 5, 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (3, 6, 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (3, 7, 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (8, 9, 1)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (8, 10, 1)');

  // Grandparent-grandchild (depth 2)
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (1, 4, 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (1, 5, 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (1, 6, 2)');
  Connection.ExecuteNonQuery('INSERT INTO categories_closure VALUES (1, 7, 2)');
end;

{ Queries the adjacency list model for immediate children, full subtree via recursive CTE, and ancestor path to root. }
procedure DemoAdjacencyList;
var
  DS: TDataSet;
begin
  WriteLn('1. Adjacency List Model');
  WriteLn('   ----------------------');
  WriteLn('   Simple parent_id reference. Easy to understand, hard to query deep trees.');
  WriteLn;

  // Get immediate children
  WriteLn('   Children of "Electronics" (id=1):');
  DS := Connection.ExecuteQuery(
    'SELECT id, name FROM categories_adj WHERE parent_id = 1');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s', [DS.FieldByName('id').AsInteger, DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Get entire subtree using recursive CTE
  WriteLn('   Full subtree of "Electronics" (recursive CTE):');
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE subtree AS (' +
    '  SELECT id, name, 0 as level FROM categories_adj WHERE id = 1 ' +
    '  UNION ALL ' +
    '  SELECT c.id, c.name, s.level + 1 ' +
    '  FROM categories_adj c ' +
    '  JOIN subtree s ON c.parent_id = s.id' +
    ')' +
    'SELECT id, name, level FROM subtree ORDER BY level, name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s[%d] %s',
        [StringOfChar(' ', DS.FieldByName('level').AsInteger * 2),
         DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Get path to root
  WriteLn('   Path from "Laptops" to root:');
  DS := Connection.ExecuteQuery(
    'WITH RECURSIVE ancestors AS (' +
    '  SELECT id, name, parent_id FROM categories_adj WHERE id = 4 ' +
    '  UNION ALL ' +
    '  SELECT c.id, c.name, c.parent_id ' +
    '  FROM categories_adj c ' +
    '  JOIN ancestors a ON c.id = a.parent_id' +
    ')' +
    'SELECT name FROM ancestors');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     -> %s', [DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries the nested sets model for descendants, ancestors, leaf nodes, and descendant counts using left/right boundary comparisons. }
procedure DemoNestedSets;
var
  DS: TDataSet;
begin
  WriteLn('2. Nested Sets Model');
  WriteLn('   -------------------');
  WriteLn('   Uses left/right boundaries. Fast reads, slow writes.');
  WriteLn;

  // Get all descendants
  WriteLn('   All descendants of "Electronics" (lft=1, rgt=14):');
  DS := Connection.ExecuteQuery(
    'SELECT child.id, child.name, child.lft, child.rgt ' +
    'FROM categories_nested parent, categories_nested child ' +
    'WHERE parent.id = 1 AND child.lft > parent.lft AND child.rgt < parent.rgt ' +
    'ORDER BY child.lft');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s (lft=%d, rgt=%d)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('lft').AsInteger,
         DS.FieldByName('rgt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Get all ancestors
  WriteLn('   All ancestors of "Smartphones" (lft=9, rgt=10):');
  DS := Connection.ExecuteQuery(
    'SELECT parent.id, parent.name ' +
    'FROM categories_nested parent, categories_nested child ' +
    'WHERE child.id = 6 AND parent.lft < child.lft AND parent.rgt > child.rgt ' +
    'ORDER BY parent.lft');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s', [DS.FieldByName('id').AsInteger, DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Check if leaf node (no children)
  WriteLn('   Leaf nodes (no children):');
  DS := Connection.ExecuteQuery(
    'SELECT id, name FROM categories_nested WHERE rgt = lft + 1');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s', [DS.FieldByName('id').AsInteger, DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Count descendants
  WriteLn('   Descendant count per node:');
  DS := Connection.ExecuteQuery(
    'SELECT id, name, (rgt - lft - 1) / 2 as descendant_count ' +
    'FROM categories_nested ' +
    'WHERE rgt - lft > 1 ' +
    'ORDER BY descendant_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s: %d descendants',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('descendant_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries the materialized path model for descendants using LIKE prefix matching, breadcrumb ancestor chains, and same-level siblings. }
procedure DemoMaterializedPath;
var
  DS: TDataSet;
begin
  WriteLn('3. Materialized Path Model');
  WriteLn('   -------------------------');
  WriteLn('   Stores full path as string. Easy queries with LIKE.');
  WriteLn;

  // Get all descendants
  WriteLn('   All descendants of "Electronics" (path /1/):');
  DS := Connection.ExecuteQuery(
    'SELECT id, name, path, depth ' +
    'FROM categories_path ' +
    'WHERE path LIKE ''/1/%'' AND id != 1 ' +
    'ORDER BY path');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s[%d] %s',
        [StringOfChar(' ', DS.FieldByName('depth').AsInteger * 2),
         DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Get ancestors from path
  WriteLn('   Path breadcrumb for "Smartphones" (/1/3/6/):');
  DS := Connection.ExecuteQuery(
    'SELECT c.id, c.name ' +
    'FROM categories_path c, categories_path target ' +
    'WHERE target.id = 6 AND target.path LIKE c.path || ''%'' ' +
    'ORDER BY c.depth');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     -> [%d] %s', [DS.FieldByName('id').AsInteger, DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Get siblings
  WriteLn('   Siblings of "Computers" (same parent path):');
  DS := Connection.ExecuteQuery(
    'SELECT c2.id, c2.name ' +
    'FROM categories_path c1, categories_path c2 ' +
    'WHERE c1.id = 2 ' +
    '  AND c2.depth = c1.depth ' +
    '  AND c2.id != c1.id ' +
    '  AND substr(c2.path, 1, length(c2.path) - length(c2.id || ''/'') - 1) = ' +
    '      substr(c1.path, 1, length(c1.path) - length(c1.id || ''/'') - 1)');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s', [DS.FieldByName('id').AsInteger, DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries the closure table model for all descendants at any depth, all ancestors with distance, and immediate children filtered by depth=1. }
procedure DemoClosureTable;
var
  DS: TDataSet;
begin
  WriteLn('4. Closure Table Model');
  WriteLn('   ---------------------');
  WriteLn('   Stores all ancestor-descendant pairs. Most flexible, requires more storage.');
  WriteLn;

  // Get all descendants
  WriteLn('   All descendants of "Electronics" (id=1):');
  DS := Connection.ExecuteQuery(
    'SELECT n.id, n.name, c.depth ' +
    'FROM categories_closure c ' +
    'JOIN categories_closure_nodes n ON c.descendant_id = n.id ' +
    'WHERE c.ancestor_id = 1 AND c.depth > 0 ' +
    'ORDER BY c.depth, n.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s[%d] %s (depth %d)',
        [StringOfChar(' ', DS.FieldByName('depth').AsInteger * 2),
         DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('depth').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Get all ancestors
  WriteLn('   All ancestors of "Laptops" (id=4):');
  DS := Connection.ExecuteQuery(
    'SELECT n.id, n.name, c.depth ' +
    'FROM categories_closure c ' +
    'JOIN categories_closure_nodes n ON c.ancestor_id = n.id ' +
    'WHERE c.descendant_id = 4 AND c.depth > 0 ' +
    'ORDER BY c.depth DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s (%d levels up)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('depth').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Get immediate children only
  WriteLn('   Immediate children of "Electronics" (depth=1):');
  DS := Connection.ExecuteQuery(
    'SELECT n.id, n.name ' +
    'FROM categories_closure c ' +
    'JOIN categories_closure_nodes n ON c.descendant_id = n.id ' +
    'WHERE c.ancestor_id = 1 AND c.depth = 1 ' +
    'ORDER BY n.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s', [DS.FieldByName('id').AsInteger, DS.FieldByName('name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays a comparison table of tree structure model trade-offs. }
procedure DemoComparison;
begin
  WriteLn('5. Model Comparison');
  WriteLn('   ------------------');
  WriteLn;
  WriteLn('   | Operation          | Adjacency | Nested Sets | Path    | Closure |');
  WriteLn('   |--------------------|-----------|-------------|---------|---------|');
  WriteLn('   | Get children       | Fast      | Medium      | Fast    | Fast    |');
  WriteLn('   | Get all descendants| Slow*     | Fast        | Fast    | Fast    |');
  WriteLn('   | Get ancestors      | Slow*     | Fast        | Fast    | Fast    |');
  WriteLn('   | Insert node        | Fast      | Slow        | Fast    | Medium  |');
  WriteLn('   | Move subtree       | Fast      | Slow        | Medium  | Medium  |');
  WriteLn('   | Delete node        | Fast      | Slow        | Fast    | Medium  |');
  WriteLn('   | Storage overhead   | Low       | Low         | Medium  | High    |');
  WriteLn;
  WriteLn('   * Requires recursive CTE (SQLite 3.8.3+)');
  WriteLn;
end;

{ Prints a summary of tree structure recommendations: when to use each model, hybrid approaches, indexing, and integrity validation. }
procedure DemoBestPractices;
begin
  WriteLn('6. Tree Structure Best Practices');
  WriteLn('   -------------------------------');
  WriteLn('   - Adjacency List: Best for shallow trees, frequent writes');
  WriteLn('   - Nested Sets: Best for read-heavy, rarely-modified trees');
  WriteLn('   - Materialized Path: Good balance, easy breadcrumbs');
  WriteLn('   - Closure Table: Most flexible, good for deep trees');
  WriteLn('   - Consider hybrid approaches for complex requirements');
  WriteLn('   - Always index tree-related columns');
  WriteLn('   - Use recursive CTEs for adjacency list traversal');
  WriteLn('   - Validate tree integrity with constraints/triggers');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 60: Tree Structures ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example60.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTreeTables;
      InsertSampleData;
      DemoAdjacencyList;
      DemoNestedSets;
      DemoMaterializedPath;
      DemoClosureTable;
      DemoComparison;
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
