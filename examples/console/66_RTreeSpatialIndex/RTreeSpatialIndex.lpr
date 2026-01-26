{===============================================================================
  NDXSQLite Example 66 - R-Tree Spatial Index
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Creating R-Tree virtual tables for spatial indexing
  - Inserting 2D and 3D bounding boxes
  - Spatial queries (box overlap, point containment, radius)
  - Efficient spatial data retrieval
  - Bulk operations and optimization

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program RTreeSpatialIndex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteertree;

var
  Conn: INDXSQLiteConnection;
  RTree: TNDXSQLiteRTree;
  DBPath: string;
  DS: TDataSet;
  Bounds: TNDXBoundingBox2D;
  I: Integer;

begin
  WriteLn('=== NDXSQLite Example 66: R-Tree Spatial Index ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example66.db';

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);

  Conn := TNDXSQLiteConnection.Create(DBPath);
  Conn.Open;

  // 1. Create 2D R-Tree
  WriteLn('1. Creating 2D R-Tree:');
  RTree := TNDXSQLiteRTree.Create(Conn);
  try
    RTree.CreateRTree('locations', rtd2D);
    WriteLn('   Created R-Tree table "locations" (2D)');

    // Insert some locations (buildings on a map)
    WriteLn('   Inserting building bounding boxes...');
    RTree.Insert2D(1, 0, 20, 0, 15);      // Building A
    RTree.Insert2D(2, 25, 45, 5, 25);     // Building B
    RTree.Insert2D(3, 50, 80, 10, 40);    // Building C
    RTree.Insert2D(4, 10, 30, 50, 70);    // Building D
    RTree.Insert2D(5, 60, 90, 55, 85);    // Building E
    WriteLn('   Inserted 5 buildings');

    WriteLn('   Total entries: ', RTree.GetCount);
    WriteLn;

    // 2. Query by bounding box
    WriteLn('2. Query by Bounding Box (0-40, 0-30):');
    DS := RTree.QueryBox2D(0, 40, 0, 30);
    try
      WriteLn('   Buildings overlapping query area:');
      while not DS.EOF do
      begin
        WriteLn(Format('     ID %d: (%.0f,%.0f) to (%.0f,%.0f)', [
          DS.FieldByName('id').AsInteger,
          DS.FieldByName('minX').AsFloat,
          DS.FieldByName('minY').AsFloat,
          DS.FieldByName('maxX').AsFloat,
          DS.FieldByName('maxY').AsFloat
        ]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;

    // 3. Point containment query
    WriteLn('3. Point Containment Query (point 35, 15):');
    DS := RTree.QueryContainsPoint2D(35, 15);
    try
      WriteLn('   Buildings containing point (35, 15):');
      while not DS.EOF do
      begin
        WriteLn('     Building ID: ', DS.FieldByName('id').AsInteger);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;

    // 4. Insert points (single coordinate locations)
    WriteLn('4. Inserting Point Locations:');
    RTree.InsertPoint2D(100, 15, 10);   // Sensor 100
    RTree.InsertPoint2D(101, 35, 20);   // Sensor 101
    RTree.InsertPoint2D(102, 65, 30);   // Sensor 102
    WriteLn('   Inserted 3 sensor points');
    WriteLn('   Total entries now: ', RTree.GetCount);
    WriteLn;

    // 5. Get total bounds
    WriteLn('5. Total Bounding Box:');
    Bounds := RTree.GetTotalBounds2D;
    WriteLn(Format('   X: %.0f to %.0f', [Bounds.MinX, Bounds.MaxX]));
    WriteLn(Format('   Y: %.0f to %.0f', [Bounds.MinY, Bounds.MaxY]));
    WriteLn;

    // 6. Count in region
    WriteLn('6. Counting Entries in Region:');
    WriteLn('   Entries in (0-50, 0-50): ', RTree.CountInBox2D(0, 50, 0, 50));
    WriteLn('   Entries in (50-100, 50-100): ', RTree.CountInBox2D(50, 100, 50, 100));
    WriteLn;

    // 7. Update entry
    WriteLn('7. Updating Entry:');
    RTree.Update2D(1, 5, 25, 5, 20);  // Move Building A
    WriteLn('   Updated Building A to new position');

    DS := RTree.GetByID(1);
    try
      if not DS.EOF then
        WriteLn(Format('   New position: (%.0f,%.0f) to (%.0f,%.0f)', [
          DS.FieldByName('minX').AsFloat,
          DS.FieldByName('minY').AsFloat,
          DS.FieldByName('maxX').AsFloat,
          DS.FieldByName('maxY').AsFloat
        ]));
    finally
      DS.Free;
    end;
    WriteLn;

    // 8. Delete entry
    WriteLn('8. Deleting Entry:');
    RTree.Delete(100);  // Remove Sensor 100
    WriteLn('   Deleted Sensor 100');
    WriteLn('   Total entries now: ', RTree.GetCount);
    WriteLn;

    RTree.DropRTree('locations');
  finally
    RTree.Free;
  end;

  // 9. Create 3D R-Tree
  WriteLn('9. Creating 3D R-Tree:');
  RTree := TNDXSQLiteRTree.Create(Conn);
  try
    RTree.CreateRTree('volumes', rtd3D);
    WriteLn('   Created R-Tree table "volumes" (3D)');

    // Insert 3D volumes
    RTree.Insert3D(1, 0, 10, 0, 10, 0, 5);     // Ground floor room
    RTree.Insert3D(2, 0, 10, 0, 10, 5, 10);    // First floor room
    RTree.Insert3D(3, 15, 25, 0, 10, 0, 15);   // Tower

    WriteLn('   Inserted 3 volumes');

    // Query 3D point
    DS := RTree.QueryContainsPoint3D(5, 5, 7);
    try
      WriteLn('   Volumes containing point (5, 5, 7):');
      while not DS.EOF do
      begin
        WriteLn('     Volume ID: ', DS.FieldByName('id').AsInteger);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;

    RTree.DropRTree('volumes');
  finally
    RTree.Free;
  end;

  // 10. Bulk insert performance
  WriteLn('10. Bulk Insert Performance:');
  RTree := TNDXSQLiteRTree.Create(Conn);
  try
    RTree.CreateRTree('bulk_test', rtd2D);

    RTree.BeginBulkInsert;
    for I := 1 to 1000 do
      RTree.Insert2D(I, I * 10, I * 10 + 5, I * 5, I * 5 + 3);
    RTree.EndBulkInsert;

    WriteLn('    Inserted 1000 entries');
    WriteLn('    Total count: ', RTree.GetCount);
    WriteLn;

    // Query performance
    WriteLn('    Query (5000-5500, 2500-2750):');
    DS := RTree.QueryBox2D(5000, 5500, 2500, 2750);
    try
      I := 0;
      while not DS.EOF do
      begin
        Inc(I);
        DS.Next;
      end;
      WriteLn('    Found ', I, ' entries');
    finally
      DS.Free;
    end;
    WriteLn;

    RTree.DropRTree('bulk_test');
  finally
    RTree.Free;
  end;

  // 11. Helper functions
  WriteLn('11. Helper Functions:');
  WriteLn('    BoundingBox2D(0, 10, 0, 10):');
  Bounds := BoundingBox2D(0, 10, 0, 10);
  WriteLn(Format('      MinX=%.0f, MaxX=%.0f, MinY=%.0f, MaxY=%.0f',
    [Bounds.MinX, Bounds.MaxX, Bounds.MinY, Bounds.MaxY]));

  WriteLn('    ExpandBox2D by margin 5:');
  Bounds := ExpandBox2D(Bounds, 5);
  WriteLn(Format('      MinX=%.0f, MaxX=%.0f, MinY=%.0f, MaxY=%.0f',
    [Bounds.MinX, Bounds.MaxX, Bounds.MinY, Bounds.MaxY]));

  WriteLn('    BoxContainsPoint2D(5, 5): ', BoxContainsPoint2D(Bounds, 5, 5));
  WriteLn('    BoxContainsPoint2D(20, 20): ', BoxContainsPoint2D(Bounds, 20, 20));
  WriteLn;

  // 12. Best practices
  WriteLn('12. R-Tree Best Practices:');
  WriteLn('    - Use for spatial/geographic data');
  WriteLn('    - Ideal for range queries and nearest neighbor');
  WriteLn('    - Use bulk insert (transaction) for large datasets');
  WriteLn('    - Keep bounding boxes as tight as possible');
  WriteLn('    - Consider 2D vs 3D based on your data');
  WriteLn('    - R-Tree is a virtual table (automatic indexing)');
  WriteLn;

  Conn.Close;
  Conn := nil;

  WriteLn('=== Example completed successfully! ===');

  // Cleanup
  if FileExists(DBPath) then DeleteFile(DBPath);
end.
