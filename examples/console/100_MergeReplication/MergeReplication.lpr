{===============================================================================
  NDXSQLite Example 100 - Merge Replication
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Vector clock implementation for causality
  - Multi-node merge replication
  - Conflict detection with clock comparison
  - Last-writer-wins resolution strategy
  - Bidirectional sync between nodes

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program MergeReplication;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB,
  NDXSQLiteConnection;

var
  NodeA, NodeB, NodeC: TNDXSQLiteConnection;
  DS: TDataSet;

const
  NODE_A = 'node_a';
  NODE_B = 'node_b';
  NODE_C = 'node_c';

type
  TVectorClock = record
    NodeA: Integer;
    NodeB: Integer;
    NodeC: Integer;
  end;

  TClockRelation = (crEqual, crBefore, crAfter, crConcurrent);

{ Vector Clock operations }

function VClockToStr(const VC: TVectorClock): string;
begin
  Result := Format('%d:%d:%d', [VC.NodeA, VC.NodeB, VC.NodeC]);
end;

{ Parses a colon-separated string into a vector clock structure. }
function StrToVClock(const S: string): TVectorClock;
var
  Parts: TStringList;
begin
  Result.NodeA := 0;
  Result.NodeB := 0;
  Result.NodeC := 0;
  if S = '' then Exit;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ':';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := S;
    if Parts.Count >= 1 then Result.NodeA := StrToIntDef(Parts[0], 0);
    if Parts.Count >= 2 then Result.NodeB := StrToIntDef(Parts[1], 0);
    if Parts.Count >= 3 then Result.NodeC := StrToIntDef(Parts[2], 0);
  finally
    Parts.Free;
  end;
end;

{ Returns a new vector clock with the counter for the specified node incremented by one. }
function IncrementClock(const VC: TVectorClock; const NodeName: string): TVectorClock;
begin
  Result := VC;
  if NodeName = NODE_A then Inc(Result.NodeA)
  else if NodeName = NODE_B then Inc(Result.NodeB)
  else if NodeName = NODE_C then Inc(Result.NodeC);
end;

{ Returns a new vector clock taking the component-wise maximum of two input clocks. }
function MergeClock(const A, B: TVectorClock): TVectorClock;
begin
  if A.NodeA > B.NodeA then Result.NodeA := A.NodeA else Result.NodeA := B.NodeA;
  if A.NodeB > B.NodeB then Result.NodeB := A.NodeB else Result.NodeB := B.NodeB;
  if A.NodeC > B.NodeC then Result.NodeC := A.NodeC else Result.NodeC := B.NodeC;
end;

{ Determines the causal relationship between two vector clocks, returning Equal, Before, After, or Concurrent. }
function CompareClocks(const A, B: TVectorClock): TClockRelation;
var
  ABeforeB, BBeforeA: Boolean;
begin
  ABeforeB := (A.NodeA <= B.NodeA) and (A.NodeB <= B.NodeB) and (A.NodeC <= B.NodeC);
  BBeforeA := (B.NodeA <= A.NodeA) and (B.NodeB <= A.NodeB) and (B.NodeC <= A.NodeC);

  if (A.NodeA = B.NodeA) and (A.NodeB = B.NodeB) and (A.NodeC = B.NodeC) then
    Result := crEqual
  else if ABeforeB then
    Result := crBefore
  else if BBeforeA then
    Result := crAfter
  else
    Result := crConcurrent;
end;

{ Returns the display string for a clock relation value. }
function ClockRelationStr(R: TClockRelation): string;
begin
  case R of
    crEqual: Result := 'EQUAL';
    crBefore: Result := 'BEFORE';
    crAfter: Result := 'AFTER';
    crConcurrent: Result := 'CONCURRENT';
  end;
end;

{ Database setup }

procedure SetupNode(Conn: TNDXSQLiteConnection; const NodeName: string);
begin
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  stock INTEGER NOT NULL,' +
    '  category TEXT,' +
    '  vclock TEXT DEFAULT ''0:0:0'',' +
    '  updated_at TEXT,' +
    '  origin_node TEXT,' +
    '  is_deleted INTEGER DEFAULT 0' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS replication_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  direction TEXT,' +
    '  remote_node TEXT,' +
    '  record_id TEXT,' +
    '  action TEXT,' +
    '  local_clock TEXT,' +
    '  remote_clock TEXT,' +
    '  resolution TEXT,' +
    '  replicated_at TEXT' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS node_state (' +
    '  node_name TEXT PRIMARY KEY,' +
    '  last_clock TEXT DEFAULT ''0:0:0''' +
    ')');

  // Store own node name
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO node_state (node_name, last_clock) VALUES (''' +
    NodeName + ''', ''0:0:0'')');
end;

{ Inserts a product record with vector clock tracking on the specified node. }
procedure InsertProduct(Conn: TNDXSQLiteConnection; const NodeName, Id, Name: string;
  Price: Double; Stock: Integer; const Category, Timestamp: string);
var
  VC: TVectorClock;
  ClockStr: string;
begin
  // Get current clock for this node
  DS := Conn.ExecuteQuery('SELECT last_clock FROM node_state WHERE node_name = ''' + NodeName + '''');
  try
    if not DS.EOF then
      VC := StrToVClock(DS.FieldByName('last_clock').AsString)
    else
      VC := StrToVClock('0:0:0');
  finally
    DS.Free;
  end;

  // Increment clock
  VC := IncrementClock(VC, NodeName);
  ClockStr := VClockToStr(VC);

  // Insert product
  Conn.ExecuteNonQuery(
    'INSERT INTO products (id, name, price, stock, category, vclock, updated_at, origin_node) VALUES (' +
    '''' + Id + ''', ' +
    '''' + Name + ''', ' +
    FloatToStr(Price) + ', ' +
    IntToStr(Stock) + ', ' +
    '''' + Category + ''', ' +
    '''' + ClockStr + ''', ' +
    '''' + Timestamp + ''', ' +
    '''' + NodeName + ''')');

  // Update node clock
  Conn.ExecuteNonQuery(
    'UPDATE node_state SET last_clock = ''' + ClockStr + ''' WHERE node_name = ''' + NodeName + '''');
end;

{ Updates a single field of a product record, increments the record's vector clock for the node, and merges it with the node's state clock. }
procedure UpdateProduct(Conn: TNDXSQLiteConnection; const NodeName, Id: string;
  const FieldName, FieldValue, Timestamp: string);
var
  VC: TVectorClock;
  ClockStr: string;
begin
  // Get current record clock
  DS := Conn.ExecuteQuery('SELECT vclock FROM products WHERE id = ''' + Id + '''');
  try
    if not DS.EOF then
      VC := StrToVClock(DS.FieldByName('vclock').AsString)
    else
      VC := StrToVClock('0:0:0');
  finally
    DS.Free;
  end;

  // Increment clock
  VC := IncrementClock(VC, NodeName);
  ClockStr := VClockToStr(VC);

  // Update product
  Conn.ExecuteNonQuery(
    'UPDATE products SET ' + FieldName + ' = ' + FieldValue + ', ' +
    'vclock = ''' + ClockStr + ''', ' +
    'updated_at = ''' + Timestamp + ''', ' +
    'origin_node = ''' + NodeName + ''' ' +
    'WHERE id = ''' + Id + '''');

  // Update node clock
  DS := Conn.ExecuteQuery('SELECT last_clock FROM node_state WHERE node_name = ''' + NodeName + '''');
  try
    if not DS.EOF then
    begin
      VC := MergeClock(VC, StrToVClock(DS.FieldByName('last_clock').AsString));
      ClockStr := VClockToStr(VC);
    end;
  finally
    DS.Free;
  end;
  Conn.ExecuteNonQuery(
    'UPDATE node_state SET last_clock = ''' + ClockStr + ''' WHERE node_name = ''' + NodeName + '''');
end;

{ Marks a product as deleted (tombstone) by setting is_deleted flag, increments the vector clock for the node, and updates the node state clock. }
procedure DeleteProduct(Conn: TNDXSQLiteConnection; const NodeName, Id, Timestamp: string);
var
  VC: TVectorClock;
  ClockStr: string;
begin
  DS := Conn.ExecuteQuery('SELECT vclock FROM products WHERE id = ''' + Id + '''');
  try
    if not DS.EOF then
      VC := StrToVClock(DS.FieldByName('vclock').AsString)
    else
      VC := StrToVClock('0:0:0');
  finally
    DS.Free;
  end;

  VC := IncrementClock(VC, NodeName);
  ClockStr := VClockToStr(VC);

  Conn.ExecuteNonQuery(
    'UPDATE products SET is_deleted = 1, ' +
    'vclock = ''' + ClockStr + ''', ' +
    'updated_at = ''' + Timestamp + ''', ' +
    'origin_node = ''' + NodeName + ''' ' +
    'WHERE id = ''' + Id + '''');

  DS := Conn.ExecuteQuery('SELECT last_clock FROM node_state WHERE node_name = ''' + NodeName + '''');
  try
    if not DS.EOF then
    begin
      VC := MergeClock(VC, StrToVClock(DS.FieldByName('last_clock').AsString));
      ClockStr := VClockToStr(VC);
    end;
  finally
    DS.Free;
  end;
  Conn.ExecuteNonQuery(
    'UPDATE node_state SET last_clock = ''' + ClockStr + ''' WHERE node_name = ''' + NodeName + '''');
end;

{ Merge replication with vector clocks }

function ReplicateRecord(Source, Target: TNDXSQLiteConnection;
  const SourceNode, TargetNode, RecordId: string;
  UseFieldMerge: Boolean): string;
var
  SrcName, SrcCategory, SrcClock, SrcUpdated, SrcOrigin: string;
  SrcPrice: Double;
  SrcStock, SrcDeleted: Integer;
  TgtClock, TgtUpdated: string;
  TgtExists: Boolean;
  VCSrc, VCTgt, VCMerged: TVectorClock;
  Relation: TClockRelation;
  MergedClockStr: string;
  TgtName, TgtCategory: string;
  TgtPrice: Double;
  TgtStock: Integer;
  MergedName, MergedCategory: string;
  MergedPrice: Double;
  MergedStock: Integer;
begin
  Result := '';

  // Read source record
  DS := Source.ExecuteQuery('SELECT * FROM products WHERE id = ''' + RecordId + '''');
  try
    if DS.EOF then begin Result := 'SKIP'; Exit; end;
    SrcName := DS.FieldByName('name').AsString;
    SrcPrice := DS.FieldByName('price').AsFloat;
    SrcStock := DS.FieldByName('stock').AsInteger;
    SrcCategory := DS.FieldByName('category').AsString;
    SrcClock := DS.FieldByName('vclock').AsString;
    SrcUpdated := DS.FieldByName('updated_at').AsString;
    SrcOrigin := DS.FieldByName('origin_node').AsString;
    SrcDeleted := DS.FieldByName('is_deleted').AsInteger;
  finally
    DS.Free;
  end;

  VCSrc := StrToVClock(SrcClock);

  // Check if target has this record
  DS := Target.ExecuteQuery('SELECT * FROM products WHERE id = ''' + RecordId + '''');
  try
    TgtExists := not DS.EOF;
    if TgtExists then
    begin
      TgtClock := DS.FieldByName('vclock').AsString;
      TgtUpdated := DS.FieldByName('updated_at').AsString;
      TgtName := DS.FieldByName('name').AsString;
      TgtPrice := DS.FieldByName('price').AsFloat;
      TgtStock := DS.FieldByName('stock').AsInteger;
      TgtCategory := DS.FieldByName('category').AsString;
    end;
  finally
    DS.Free;
  end;

  if not TgtExists then
  begin
    // New record - just insert
    VCMerged := VCSrc;
    MergedClockStr := VClockToStr(VCMerged);
    Target.ExecuteNonQuery(
      'INSERT INTO products (id, name, price, stock, category, vclock, updated_at, origin_node, is_deleted) VALUES (' +
      '''' + RecordId + ''', ' +
      '''' + SrcName + ''', ' +
      FloatToStr(SrcPrice) + ', ' +
      IntToStr(SrcStock) + ', ' +
      '''' + SrcCategory + ''', ' +
      '''' + MergedClockStr + ''', ' +
      '''' + SrcUpdated + ''', ' +
      '''' + SrcOrigin + ''', ' +
      IntToStr(SrcDeleted) + ')');
    Result := 'INSERT';

    // Log replication
    Target.ExecuteNonQuery(
      'INSERT INTO replication_log (direction, remote_node, record_id, action, ' +
      'local_clock, remote_clock, resolution, replicated_at) VALUES (' +
      '''pull'', ''' + SourceNode + ''', ''' + RecordId + ''', ''INSERT'', ' +
      '''0:0:0'', ''' + SrcClock + ''', ''new_record'', ''' + SrcUpdated + ''')');
    Exit;
  end;

  // Record exists - compare vector clocks
  VCTgt := StrToVClock(TgtClock);
  Relation := CompareClocks(VCSrc, VCTgt);

  case Relation of
    crEqual:
      begin
        Result := 'SKIP';
        Exit;
      end;
    crBefore:
      begin
        // Source is older - skip
        Result := 'SKIP';
        Target.ExecuteNonQuery(
          'INSERT INTO replication_log (direction, remote_node, record_id, action, ' +
          'local_clock, remote_clock, resolution, replicated_at) VALUES (' +
          '''pull'', ''' + SourceNode + ''', ''' + RecordId + ''', ''SKIP'', ' +
          '''' + TgtClock + ''', ''' + SrcClock + ''', ''local_newer'', ''' + SrcUpdated + ''')');
        Exit;
      end;
    crAfter:
      begin
        // Source is newer - overwrite
        VCMerged := MergeClock(VCSrc, VCTgt);
        MergedClockStr := VClockToStr(VCMerged);
        Target.ExecuteNonQuery(
          'UPDATE products SET name = ''' + SrcName + ''', ' +
          'price = ' + FloatToStr(SrcPrice) + ', ' +
          'stock = ' + IntToStr(SrcStock) + ', ' +
          'category = ''' + SrcCategory + ''', ' +
          'vclock = ''' + MergedClockStr + ''', ' +
          'updated_at = ''' + SrcUpdated + ''', ' +
          'origin_node = ''' + SrcOrigin + ''', ' +
          'is_deleted = ' + IntToStr(SrcDeleted) + ' ' +
          'WHERE id = ''' + RecordId + '''');
        Result := 'UPDATE(source_wins)';

        Target.ExecuteNonQuery(
          'INSERT INTO replication_log (direction, remote_node, record_id, action, ' +
          'local_clock, remote_clock, resolution, replicated_at) VALUES (' +
          '''pull'', ''' + SourceNode + ''', ''' + RecordId + ''', ''UPDATE'', ' +
          '''' + TgtClock + ''', ''' + SrcClock + ''', ''source_newer'', ''' + SrcUpdated + ''')');
      end;
    crConcurrent:
      begin
        // Conflict! Use merge strategy
        VCMerged := MergeClock(VCSrc, VCTgt);
        VCMerged := IncrementClock(VCMerged, TargetNode);
        MergedClockStr := VClockToStr(VCMerged);

        if UseFieldMerge then
        begin
          // Field-level merge: take max price, sum stocks, keep longer name, merge categories
          if SrcPrice > TgtPrice then MergedPrice := SrcPrice else MergedPrice := TgtPrice;
          MergedStock := SrcStock + TgtStock;
          if Length(SrcName) >= Length(TgtName) then MergedName := SrcName else MergedName := TgtName;
          if SrcCategory = TgtCategory then
            MergedCategory := SrcCategory
          else
            MergedCategory := TgtCategory + '+' + SrcCategory;

          Target.ExecuteNonQuery(
            'UPDATE products SET name = ''' + MergedName + ''', ' +
            'price = ' + FloatToStr(MergedPrice) + ', ' +
            'stock = ' + IntToStr(MergedStock) + ', ' +
            'category = ''' + MergedCategory + ''', ' +
            'vclock = ''' + MergedClockStr + ''', ' +
            'updated_at = ''' + SrcUpdated + ''', ' +
            'origin_node = ''merged'' ' +
            'WHERE id = ''' + RecordId + '''');
          Result := 'MERGE(field_level)';

          Target.ExecuteNonQuery(
            'INSERT INTO replication_log (direction, remote_node, record_id, action, ' +
            'local_clock, remote_clock, resolution, replicated_at) VALUES (' +
            '''pull'', ''' + SourceNode + ''', ''' + RecordId + ''', ''MERGE'', ' +
            '''' + TgtClock + ''', ''' + SrcClock + ''', ''field_merge'', ''' + SrcUpdated + ''')');
        end
        else
        begin
          // Last-Writer-Wins by timestamp
          if SrcUpdated >= TgtUpdated then
          begin
            Target.ExecuteNonQuery(
              'UPDATE products SET name = ''' + SrcName + ''', ' +
              'price = ' + FloatToStr(SrcPrice) + ', ' +
              'stock = ' + IntToStr(SrcStock) + ', ' +
              'category = ''' + SrcCategory + ''', ' +
              'vclock = ''' + MergedClockStr + ''', ' +
              'updated_at = ''' + SrcUpdated + ''', ' +
              'origin_node = ''' + SrcOrigin + ''', ' +
              'is_deleted = ' + IntToStr(SrcDeleted) + ' ' +
              'WHERE id = ''' + RecordId + '''');
            Result := 'LWW(source_wins)';
          end
          else
          begin
            // Keep target, just update clock
            Target.ExecuteNonQuery(
              'UPDATE products SET vclock = ''' + MergedClockStr + ''' WHERE id = ''' + RecordId + '''');
            Result := 'LWW(target_wins)';
          end;

          Target.ExecuteNonQuery(
            'INSERT INTO replication_log (direction, remote_node, record_id, action, ' +
            'local_clock, remote_clock, resolution, replicated_at) VALUES (' +
            '''pull'', ''' + SourceNode + ''', ''' + RecordId + ''', ''CONFLICT'', ' +
            '''' + TgtClock + ''', ''' + SrcClock + ''', ''' + Result + ''', ''' + SrcUpdated + ''')');
        end;
      end;
  end;
end;

{ Replicates all products from the source node to the target and returns the count of changes. }
function ReplicateAll(Source, Target: TNDXSQLiteConnection;
  const SourceNode, TargetNode: string; UseFieldMerge: Boolean): Integer;
var
  Ids: TStringList;
  I: Integer;
  Action: string;
begin
  Result := 0;
  Ids := TStringList.Create;
  try
    DS := Source.ExecuteQuery('SELECT id FROM products ORDER BY id');
    try
      while not DS.EOF do
      begin
        Ids.Add(DS.FieldByName('id').AsString);
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    for I := 0 to Ids.Count - 1 do
    begin
      Action := ReplicateRecord(Source, Target, SourceNode, TargetNode, Ids[I], UseFieldMerge);
      if (Action <> 'SKIP') and (Action <> '') then
        Inc(Result);
    end;
  finally
    Ids.Free;
  end;
end;

{ Display functions }

procedure ShowProducts(Conn: TNDXSQLiteConnection; const Title: string);
begin
  WriteLn('   ', Title, ':');
  DS := Conn.ExecuteQuery(
    'SELECT id, name, price, stock, category, vclock, origin_node, is_deleted FROM products ORDER BY id');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('is_deleted').AsInteger = 0 then
        WriteLn(Format('    %s  %-20s $%8.2f  qty=%d  [%s]  clock=%s  from=%s',
          [DS.FieldByName('id').AsString,
           DS.FieldByName('name').AsString,
           DS.FieldByName('price').AsFloat,
           DS.FieldByName('stock').AsInteger,
           DS.FieldByName('category').AsString,
           DS.FieldByName('vclock').AsString,
           DS.FieldByName('origin_node').AsString]))
      else
        WriteLn(Format('    %s  %-20s (DELETED)  clock=%s  from=%s',
          [DS.FieldByName('id').AsString,
           DS.FieldByName('name').AsString,
           DS.FieldByName('vclock').AsString,
           DS.FieldByName('origin_node').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Queries and prints all replication log entries showing direction, remote node, record ID, action, local/remote clocks, and resolution strategy. }
procedure ShowReplicationLog(Conn: TNDXSQLiteConnection; const Title: string);
begin
  WriteLn('   ', Title, ':');
  DS := Conn.ExecuteQuery(
    'SELECT direction, remote_node, record_id, action, local_clock, remote_clock, resolution FROM replication_log ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    [%s] %s %s -> %s (local=%s remote=%s) %s',
        [DS.FieldByName('direction').AsString,
         DS.FieldByName('remote_node').AsString,
         DS.FieldByName('record_id').AsString,
         DS.FieldByName('action').AsString,
         DS.FieldByName('local_clock').AsString,
         DS.FieldByName('remote_clock').AsString,
         DS.FieldByName('resolution').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Demo procedures }

procedure DemoThreeNodeSetup;
begin
  WriteLn('=== 1. Three-Node Setup ===');
  WriteLn;

  // Node A: Electronics store
  InsertProduct(NodeA, NODE_A, 'p-001', 'Laptop Pro 15', 1299.99, 50, 'electronics', '2024-01-10T09:00:00');
  InsertProduct(NodeA, NODE_A, 'p-002', 'Wireless Mouse', 29.99, 200, 'accessories', '2024-01-10T09:01:00');
  InsertProduct(NodeA, NODE_A, 'p-003', 'USB-C Hub', 49.99, 150, 'accessories', '2024-01-10T09:02:00');

  // Node B: Office supplies
  InsertProduct(NodeB, NODE_B, 'p-004', 'Ergonomic Chair', 599.99, 30, 'furniture', '2024-01-10T10:00:00');
  InsertProduct(NodeB, NODE_B, 'p-005', 'Standing Desk', 449.99, 25, 'furniture', '2024-01-10T10:01:00');

  // Node C: Software
  InsertProduct(NodeC, NODE_C, 'p-006', 'IDE License', 199.99, 999, 'software', '2024-01-10T11:00:00');

  ShowProducts(NodeA, 'Node A (Electronics)');
  WriteLn;
  ShowProducts(NodeB, 'Node B (Office)');
  WriteLn;
  ShowProducts(NodeC, 'Node C (Software)');
  WriteLn;
end;

{ Prints vector clock comparison results for several clock pairs showing Before, After, Concurrent, and Equal relationships, plus a clock merge example. }
procedure DemoVectorClockBasics;
var
  VC1, VC2, VCM: TVectorClock;
  R: TClockRelation;
begin
  WriteLn('=== 2. Vector Clock Basics ===');
  WriteLn;

  // Show vector clock comparisons
  VC1 := StrToVClock('2:1:0');
  VC2 := StrToVClock('3:1:0');
  R := CompareClocks(VC1, VC2);
  WriteLn(Format('   %s vs %s -> %s (A happened before B)', [VClockToStr(VC1), VClockToStr(VC2), ClockRelationStr(R)]));

  VC1 := StrToVClock('3:2:1');
  VC2 := StrToVClock('2:1:0');
  R := CompareClocks(VC1, VC2);
  WriteLn(Format('   %s vs %s -> %s (A happened after B)', [VClockToStr(VC1), VClockToStr(VC2), ClockRelationStr(R)]));

  VC1 := StrToVClock('2:1:0');
  VC2 := StrToVClock('1:2:0');
  R := CompareClocks(VC1, VC2);
  WriteLn(Format('   %s vs %s -> %s (conflict!)', [VClockToStr(VC1), VClockToStr(VC2), ClockRelationStr(R)]));

  VC1 := StrToVClock('2:3:1');
  VC2 := StrToVClock('2:3:1');
  R := CompareClocks(VC1, VC2);
  WriteLn(Format('   %s vs %s -> %s', [VClockToStr(VC1), VClockToStr(VC2), ClockRelationStr(R)]));

  // Merge clocks
  VC1 := StrToVClock('3:1:2');
  VC2 := StrToVClock('1:4:1');
  VCM := MergeClock(VC1, VC2);
  WriteLn(Format('   merge(%s, %s) = %s', [VClockToStr(VC1), VClockToStr(VC2), VClockToStr(VCM)]));

  WriteLn;
end;

{ Performs bidirectional replication between all node pairs (A-B, A-C, B-C) using LWW strategy, then displays each node's products after full sync. }
procedure DemoInitialReplication;
var
  CountAB, CountBA, CountAC, CountCA, CountBC, CountCB: Integer;
begin
  WriteLn('=== 3. Initial Full Replication ===');
  WriteLn;

  // Replicate between all pairs (LWW strategy)
  CountAB := ReplicateAll(NodeA, NodeB, NODE_A, NODE_B, False);
  CountBA := ReplicateAll(NodeB, NodeA, NODE_B, NODE_A, False);
  WriteLn(Format('   A <-> B: %d + %d records replicated', [CountAB, CountBA]));

  CountAC := ReplicateAll(NodeA, NodeC, NODE_A, NODE_C, False);
  CountCA := ReplicateAll(NodeC, NodeA, NODE_C, NODE_A, False);
  WriteLn(Format('   A <-> C: %d + %d records replicated', [CountAC, CountCA]));

  CountBC := ReplicateAll(NodeB, NodeC, NODE_B, NODE_C, False);
  CountCB := ReplicateAll(NodeC, NodeB, NODE_C, NODE_B, False);
  WriteLn(Format('   B <-> C: %d + %d records replicated', [CountBC, CountCB]));

  WriteLn;
  ShowProducts(NodeA, 'Node A after full replication');
  WriteLn;
  ShowProducts(NodeB, 'Node B after full replication');
  WriteLn;
  ShowProducts(NodeC, 'Node C after full replication');
  WriteLn;
end;

{ Updates a product price on Node A, replicates to Node B, then updates stock on Node B, and replicates back, showing that sequential changes produce no conflict. }
procedure DemoCausalOrdering;
var
  CountAB, CountBA: Integer;
begin
  WriteLn('=== 4. Causal Ordering (No Conflict) ===');
  WriteLn;

  // Node A updates price, then Node B updates stock (different fields, sequential)
  UpdateProduct(NodeA, NODE_A, 'p-001', 'price', '1199.99', '2024-01-15T10:00:00');
  WriteLn('   Node A: Updated p-001 price to $1199.99');

  // Replicate A -> B first
  CountAB := ReplicateAll(NodeA, NodeB, NODE_A, NODE_B, False);
  WriteLn(Format('   Replicate A -> B: %d records', [CountAB]));

  // Now B updates stock (after seeing A's change)
  UpdateProduct(NodeB, NODE_B, 'p-001', 'stock', '45', '2024-01-15T11:00:00');
  WriteLn('   Node B: Updated p-001 stock to 45 (after seeing A''s change)');

  // Replicate B -> A
  CountBA := ReplicateAll(NodeB, NodeA, NODE_B, NODE_A, False);
  WriteLn(Format('   Replicate B -> A: %d records', [CountBA]));

  WriteLn;
  WriteLn('   Result (no conflict - B''s clock dominates A''s):');
  DS := NodeA.ExecuteQuery('SELECT name, price, stock, vclock FROM products WHERE id = ''p-001''');
  try
    WriteLn(Format('    Node A: %s $%.2f qty=%d clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  DS := NodeB.ExecuteQuery('SELECT name, price, stock, vclock FROM products WHERE id = ''p-001''');
  try
    WriteLn(Format('    Node B: %s $%.2f qty=%d clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Creates concurrent writes on Node A and Node B for the same product, then syncs using Last-Writer-Wins strategy where the later timestamp wins. }
procedure DemoConflictLWW;
var
  CountAB, CountBA: Integer;
begin
  WriteLn('=== 5. Concurrent Writes - Last-Writer-Wins ===');
  WriteLn;

  // Both nodes update the same record concurrently (no sync between)
  UpdateProduct(NodeA, NODE_A, 'p-002', 'price', '24.99', '2024-01-16T09:00:00');
  UpdateProduct(NodeA, NODE_A, 'p-002', 'name', '''Wireless Mouse v2''', '2024-01-16T09:00:00');
  WriteLn('   Node A: Updated p-002 -> Wireless Mouse v2, $24.99');

  UpdateProduct(NodeB, NODE_B, 'p-002', 'stock', '180', '2024-01-16T09:30:00');
  UpdateProduct(NodeB, NODE_B, 'p-002', 'price', '34.99', '2024-01-16T09:30:00');
  WriteLn('   Node B: Updated p-002 -> stock=180, $34.99');

  WriteLn;
  WriteLn('   Before sync:');
  DS := NodeA.ExecuteQuery('SELECT name, price, stock, vclock FROM products WHERE id = ''p-002''');
  try
    WriteLn(Format('    Node A: %s $%.2f qty=%d clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  DS := NodeB.ExecuteQuery('SELECT name, price, stock, vclock FROM products WHERE id = ''p-002''');
  try
    WriteLn(Format('    Node B: %s $%.2f qty=%d clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;

  // Now sync with LWW
  WriteLn;
  WriteLn('   Syncing with Last-Writer-Wins strategy...');
  CountAB := ReplicateAll(NodeA, NodeB, NODE_A, NODE_B, False);
  CountBA := ReplicateAll(NodeB, NodeA, NODE_B, NODE_A, False);
  WriteLn(Format('   A -> B: %d, B -> A: %d', [CountAB, CountBA]));

  WriteLn;
  WriteLn('   After LWW sync:');
  DS := NodeA.ExecuteQuery('SELECT name, price, stock, vclock FROM products WHERE id = ''p-002''');
  try
    WriteLn(Format('    Node A: %s $%.2f qty=%d clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  DS := NodeB.ExecuteQuery('SELECT name, price, stock, vclock FROM products WHERE id = ''p-002''');
  try
    WriteLn(Format('    Node B: %s $%.2f qty=%d clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Creates concurrent writes on Node A and Node C for the same product, then syncs using field-level merge: max price, summed stocks, longer name, and concatenated categories. }
procedure DemoConflictFieldMerge;
var
  CountAC, CountCA: Integer;
begin
  WriteLn('=== 6. Concurrent Writes - Field-Level Merge ===');
  WriteLn;

  // Both nodes update the same record concurrently
  UpdateProduct(NodeA, NODE_A, 'p-003', 'price', '59.99', '2024-01-17T10:00:00');
  UpdateProduct(NodeA, NODE_A, 'p-003', 'stock', '100', '2024-01-17T10:00:00');
  WriteLn('   Node A: Updated p-003 -> price=$59.99, stock=100');

  UpdateProduct(NodeC, NODE_C, 'p-003', 'stock', '80', '2024-01-17T10:15:00');
  UpdateProduct(NodeC, NODE_C, 'p-003', 'category', '''peripherals''', '2024-01-17T10:15:00');
  WriteLn('   Node C: Updated p-003 -> stock=80, category=peripherals');

  WriteLn;
  WriteLn('   Before sync:');
  DS := NodeA.ExecuteQuery('SELECT name, price, stock, category, vclock FROM products WHERE id = ''p-003''');
  try
    WriteLn(Format('    Node A: %s $%.2f qty=%d [%s] clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('category').AsString,
       DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  DS := NodeC.ExecuteQuery('SELECT name, price, stock, category, vclock FROM products WHERE id = ''p-003''');
  try
    WriteLn(Format('    Node C: %s $%.2f qty=%d [%s] clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('category').AsString,
       DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;

  // Sync with field-level merge
  WriteLn;
  WriteLn('   Syncing with Field-Level Merge strategy...');
  WriteLn('   Rules: max(price), sum(stock), keep longer name, merge categories');
  CountAC := ReplicateAll(NodeA, NodeC, NODE_A, NODE_C, True);
  CountCA := ReplicateAll(NodeC, NodeA, NODE_C, NODE_A, True);
  WriteLn(Format('   A -> C: %d, C -> A: %d', [CountAC, CountCA]));

  WriteLn;
  WriteLn('   After field-level merge:');
  DS := NodeA.ExecuteQuery('SELECT name, price, stock, category, vclock FROM products WHERE id = ''p-003''');
  try
    WriteLn(Format('    Node A: %s $%.2f qty=%d [%s] clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('category').AsString,
       DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  DS := NodeC.ExecuteQuery('SELECT name, price, stock, category, vclock FROM products WHERE id = ''p-003''');
  try
    WriteLn(Format('    Node C: %s $%.2f qty=%d [%s] clock=%s',
      [DS.FieldByName('name').AsString, DS.FieldByName('price').AsFloat,
       DS.FieldByName('stock').AsInteger, DS.FieldByName('category').AsString,
       DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Deletes a product on Node A using a tombstone marker, then replicates to Node B, verifying that the deletion propagates via the is_deleted flag. }
procedure DemoDeleteReplication;
var
  CountAB, CountBA: Integer;
begin
  WriteLn('=== 7. Delete Replication (Tombstones) ===');
  WriteLn;

  // Node A deletes a product
  DeleteProduct(NodeA, NODE_A, 'p-004', '2024-01-18T14:00:00');
  WriteLn('   Node A: Deleted p-004 (Ergonomic Chair)');

  // Sync to Node B
  CountAB := ReplicateAll(NodeA, NodeB, NODE_A, NODE_B, False);
  CountBA := ReplicateAll(NodeB, NodeA, NODE_B, NODE_A, False);
  WriteLn(Format('   Sync A <-> B: %d + %d records', [CountAB, CountBA]));

  WriteLn;
  WriteLn('   Node A products (p-004 deleted):');
  DS := NodeA.ExecuteQuery('SELECT id, name, is_deleted FROM products WHERE id = ''p-004''');
  try
    WriteLn(Format('    %s  %s  deleted=%d',
      [DS.FieldByName('id').AsString, DS.FieldByName('name').AsString,
       DS.FieldByName('is_deleted').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn('   Node B products (p-004 tombstone received):');
  DS := NodeB.ExecuteQuery('SELECT id, name, is_deleted FROM products WHERE id = ''p-004''');
  try
    WriteLn(Format('    %s  %s  deleted=%d',
      [DS.FieldByName('id').AsString, DS.FieldByName('name').AsString,
       DS.FieldByName('is_deleted').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Creates a concurrent update on Node B and deletion on Node C for the same product, then syncs with LWW to show how delete-vs-update conflicts are resolved by timestamp. }
procedure DemoDeleteUpdateConflict;
var
  CountBC, CountCB: Integer;
begin
  WriteLn('=== 8. Delete vs Update Conflict ===');
  WriteLn;

  // Node B updates p-005, Node C deletes p-005 concurrently
  UpdateProduct(NodeB, NODE_B, 'p-005', 'price', '499.99', '2024-01-19T10:00:00');
  WriteLn('   Node B: Updated p-005 price to $499.99');

  DeleteProduct(NodeC, NODE_C, 'p-005', '2024-01-19T10:30:00');
  WriteLn('   Node C: Deleted p-005 (Standing Desk)');

  WriteLn;
  WriteLn('   Syncing B <-> C with LWW...');
  CountBC := ReplicateAll(NodeB, NodeC, NODE_B, NODE_C, False);
  CountCB := ReplicateAll(NodeC, NodeB, NODE_C, NODE_B, False);
  WriteLn(Format('   B -> C: %d, C -> B: %d', [CountBC, CountCB]));

  WriteLn;
  WriteLn('   Result (concurrent - LWW by timestamp):');
  DS := NodeB.ExecuteQuery('SELECT id, name, price, is_deleted, vclock FROM products WHERE id = ''p-005''');
  try
    WriteLn(Format('    Node B: %s %s $%.2f deleted=%d clock=%s',
      [DS.FieldByName('id').AsString, DS.FieldByName('name').AsString,
       DS.FieldByName('price').AsFloat, DS.FieldByName('is_deleted').AsInteger,
       DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  DS := NodeC.ExecuteQuery('SELECT id, name, price, is_deleted, vclock FROM products WHERE id = ''p-005''');
  try
    WriteLn(Format('    Node C: %s %s $%.2f deleted=%d clock=%s',
      [DS.FieldByName('id').AsString, DS.FieldByName('name').AsString,
       DS.FieldByName('price').AsFloat, DS.FieldByName('is_deleted').AsInteger,
       DS.FieldByName('vclock').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Runs a full bidirectional replication round between all node pairs (A-B, A-C, B-C) to achieve eventual convergence across the cluster. }
procedure DemoAntiEntropy;
var
  CountAll: Integer;
begin
  WriteLn('=== 9. Anti-Entropy (Full State Sync) ===');
  WriteLn;

  // Full sync between all nodes to achieve convergence
  WriteLn('   Running full anti-entropy round...');

  CountAll := 0;
  CountAll := CountAll + ReplicateAll(NodeA, NodeB, NODE_A, NODE_B, False);
  CountAll := CountAll + ReplicateAll(NodeB, NodeA, NODE_B, NODE_A, False);
  CountAll := CountAll + ReplicateAll(NodeA, NodeC, NODE_A, NODE_C, False);
  CountAll := CountAll + ReplicateAll(NodeC, NodeA, NODE_C, NODE_A, False);
  CountAll := CountAll + ReplicateAll(NodeB, NodeC, NODE_B, NODE_C, False);
  CountAll := CountAll + ReplicateAll(NodeC, NodeB, NODE_C, NODE_B, False);

  WriteLn(Format('   Total records replicated: %d', [CountAll]));
  WriteLn;
end;

{ Compares active product records across all three nodes by ID and name, counting mismatches to verify whether the nodes have converged to the same state. }
procedure DemoConvergenceCheck;
var
  CountA, CountB, CountC: Integer;
  Mismatches: Integer;
  IdA, NameA: string;
  NameB: string;
  NameC: string;
  DSA, DSB, DSC: TDataSet;
begin
  WriteLn('=== 10. Convergence Verification ===');
  WriteLn;

  // Count active records on each node
  DS := NodeA.ExecuteQuery('SELECT COUNT(*) as cnt FROM products WHERE is_deleted = 0');
  try CountA := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := NodeB.ExecuteQuery('SELECT COUNT(*) as cnt FROM products WHERE is_deleted = 0');
  try CountB := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;
  DS := NodeC.ExecuteQuery('SELECT COUNT(*) as cnt FROM products WHERE is_deleted = 0');
  try CountC := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  WriteLn(Format('   Active records: A=%d, B=%d, C=%d', [CountA, CountB, CountC]));

  // Compare records across nodes
  Mismatches := 0;
  DSA := NodeA.ExecuteQuery('SELECT id, name, price, stock FROM products WHERE is_deleted = 0 ORDER BY id');
  DSB := NodeB.ExecuteQuery('SELECT id, name, price, stock FROM products WHERE is_deleted = 0 ORDER BY id');
  DSC := NodeC.ExecuteQuery('SELECT id, name, price, stock FROM products WHERE is_deleted = 0 ORDER BY id');
  try
    while not DSA.EOF do
    begin
      IdA := DSA.FieldByName('id').AsString;
      NameA := DSA.FieldByName('name').AsString;

      if (not DSB.EOF) and (DSB.FieldByName('id').AsString = IdA) then
      begin
        NameB := DSB.FieldByName('name').AsString;
        if NameA = NameB then
          Write(Format('    OK: %s = %s', [IdA, NameA]))
        else
        begin
          Write(Format('    MISMATCH: %s A=%s B=%s', [IdA, NameA, NameB]));
          Inc(Mismatches);
        end;
      end
      else
      begin
        Write(Format('    MISSING on B: %s', [IdA]));
        Inc(Mismatches);
      end;

      if (not DSC.EOF) and (DSC.FieldByName('id').AsString = IdA) then
      begin
        NameC := DSC.FieldByName('name').AsString;
        if NameA <> NameC then
        begin
          Write(Format(' C=%s', [NameC]));
          Inc(Mismatches);
        end;
      end
      else
      begin
        Write(Format(' MISSING on C: %s', [IdA]));
        Inc(Mismatches);
      end;

      WriteLn;

      DSA.Next;
      if not DSB.EOF then DSB.Next;
      if not DSC.EOF then DSC.Next;
    end;
  finally
    DSA.Free;
    DSB.Free;
    DSC.Free;
  end;

  WriteLn;
  if Mismatches = 0 then
    WriteLn('   All nodes converged successfully!')
  else
    WriteLn(Format('   WARNING: %d mismatches detected (expected with different merge strategies)', [Mismatches]));
  WriteLn;
end;

{ Prints Node B's full replication log showing all sync operations, conflict resolutions, and their outcomes. }
procedure DemoReplicationLog;
begin
  WriteLn('=== 11. Replication History ===');
  WriteLn;

  ShowReplicationLog(NodeB, 'Node B replication log');
  WriteLn;
end;

{ Displays a summary of merge replication operations and conflict resolution. }
procedure DemoSummary;
var
  InsertCount, ConflictCount, TotalCount: Integer;
begin
  WriteLn('=== Merge Replication Summary ===');
  WriteLn;

  DS := NodeA.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try TotalCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  DS := NodeB.ExecuteQuery('SELECT COUNT(*) as cnt FROM replication_log WHERE action = ''INSERT''');
  try InsertCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  DS := NodeB.ExecuteQuery('SELECT COUNT(*) as cnt FROM replication_log WHERE action IN (''CONFLICT'', ''MERGE'')');
  try ConflictCount := DS.FieldByName('cnt').AsInteger; finally DS.Free; end;

  WriteLn(Format('   Total products (all nodes): %d', [TotalCount]));
  WriteLn(Format('   Node B: %d inserts, %d conflicts resolved', [InsertCount, ConflictCount]));

  WriteLn;
  WriteLn('   Patterns demonstrated:');
  WriteLn('   - Vector clocks for causal ordering');
  WriteLn('   - Three-node replication topology');
  WriteLn('   - Last-Writer-Wins conflict resolution');
  WriteLn('   - Field-level merge strategy');
  WriteLn('   - Tombstone deletion propagation');
  WriteLn('   - Delete vs Update conflict handling');
  WriteLn('   - Anti-entropy full state sync');
  WriteLn('   - Convergence verification');
  WriteLn('   - Replication log and audit trail');
end;

begin
  WriteLn('Example 100: Merge Replication - Vector Clocks & Merge Strategies');
  WriteLn('==================================================================');
  WriteLn;

  NodeA := TNDXSQLiteConnection.Create(':memory:');
  NodeB := TNDXSQLiteConnection.Create(':memory:');
  NodeC := TNDXSQLiteConnection.Create(':memory:');
  try
    NodeA.Open;
    NodeB.Open;
    NodeC.Open;

    SetupNode(NodeA, NODE_A);
    SetupNode(NodeB, NODE_B);
    SetupNode(NodeC, NODE_C);

    WriteLn('Three nodes initialized.');
    WriteLn;

    DemoThreeNodeSetup;
    DemoVectorClockBasics;
    DemoInitialReplication;
    DemoCausalOrdering;
    DemoConflictLWW;
    DemoConflictFieldMerge;
    DemoDeleteReplication;
    DemoDeleteUpdateConflict;
    DemoAntiEntropy;
    DemoConvergenceCheck;
    DemoReplicationLog;
    DemoSummary;

    WriteLn;
    WriteLn('Done.');
  finally
    NodeA.Close;
    NodeB.Close;
    NodeC.Close;
    NodeA.Free;
    NodeB.Free;
    NodeC.Free;
  end;
end.
