{===============================================================================
  NDXSQLite Example 32 - Pagination
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - LIMIT/OFFSET pagination
  - Cursor-based (keyset) pagination
  - Performance comparison
  - Total count with pagination
  - Pagination with sorting
  - Infinite scroll pattern

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Pagination;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

const
  TOTAL_ROWS = 100000;
  PAGE_SIZE = 20;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Returns the tick count64. }
function GetTickCount64: Int64;
begin
  Result := MilliSecondsBetween(Now, EncodeDate(1970, 1, 1));
end;

{ Initializes sample data for demonstrations. }
procedure SetupData;
var
  I: Integer;
begin
  WriteLn('Setting up test data (', TOTAL_ROWS, ' rows)...');

  Connection.ExecuteNonQuery(
    'CREATE TABLE posts (' +
    '  id INTEGER PRIMARY KEY,' +
    '  title TEXT,' +
    '  author TEXT,' +
    '  created_at TEXT,' +
    '  views INTEGER' +
    ')');

  Connection.ExecuteNonQuery('CREATE INDEX idx_posts_created ON posts(created_at DESC)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_posts_views ON posts(views DESC, id)');

  Connection.BeginTransaction;
  for I := 1 to TOTAL_ROWS do
  begin
    Connection.ExecuteNonQuery(
      'INSERT INTO posts (title, author, created_at, views) VALUES (?, ?, ?, ?)',
      [Format('Post #%d: Interesting Title', [I]),
       Format('Author%d', [I mod 100]),
       FormatDateTime('yyyy-mm-dd hh:nn:ss', Now - (TOTAL_ROWS - I) / 24 / 60),
       Random(10000)]);
  end;
  Connection.Commit;

  WriteLn('Done.');
  WriteLn('');
end;

{ Fetches and displays page 1 and page 5000 of posts using LIMIT/OFFSET queries ordered by id. }
procedure DemoLimitOffset;
var
  DS: TDataSet;
  PageNum: Integer;
begin
  WriteLn('1. LIMIT/OFFSET Pagination');
  WriteLn('   ------------------------');

  // Page 1 (offset 0)
  PageNum := 1;
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title FROM posts ORDER BY id LIMIT %d OFFSET %d',
      [PAGE_SIZE, (PageNum - 1) * PAGE_SIZE]));
  try
    WriteLn('   Page ', PageNum, ':');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              Copy(DS.FieldByName('title').AsString, 1, 30));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Page 5000 (deep pagination)
  PageNum := 5000;
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title FROM posts ORDER BY id LIMIT %d OFFSET %d',
      [PAGE_SIZE, (PageNum - 1) * PAGE_SIZE]));
  try
    WriteLn('');
    WriteLn('   Page ', PageNum, ':');
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              Copy(DS.FieldByName('title').AsString, 1, 30));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Measures query time for LIMIT/OFFSET at pages 1, 100, 1000, and 5000 to show performance degradation with increasing offsets. }
procedure DemoLimitOffsetPerformance;
var
  DS: TDataSet;
  StartTime: Int64;
  Page1Time, Page100Time, Page1000Time, Page5000Time: Int64;
begin
  WriteLn('2. LIMIT/OFFSET Performance Problem');
  WriteLn('   ---------------------------------');

  // Page 1
  StartTime := GetTickCount64;
  DS := Connection.ExecuteQuery(
    Format('SELECT * FROM posts ORDER BY id LIMIT %d OFFSET 0', [PAGE_SIZE]));
  DS.Free;
  Page1Time := GetTickCount64 - StartTime;

  // Page 100
  StartTime := GetTickCount64;
  DS := Connection.ExecuteQuery(
    Format('SELECT * FROM posts ORDER BY id LIMIT %d OFFSET %d',
      [PAGE_SIZE, 99 * PAGE_SIZE]));
  DS.Free;
  Page100Time := GetTickCount64 - StartTime;

  // Page 1000
  StartTime := GetTickCount64;
  DS := Connection.ExecuteQuery(
    Format('SELECT * FROM posts ORDER BY id LIMIT %d OFFSET %d',
      [PAGE_SIZE, 999 * PAGE_SIZE]));
  DS.Free;
  Page1000Time := GetTickCount64 - StartTime;

  // Page 5000
  StartTime := GetTickCount64;
  DS := Connection.ExecuteQuery(
    Format('SELECT * FROM posts ORDER BY id LIMIT %d OFFSET %d',
      [PAGE_SIZE, 4999 * PAGE_SIZE]));
  DS.Free;
  Page5000Time := GetTickCount64 - StartTime;

  WriteLn('   Page 1:    ', Page1Time, ' ms');
  WriteLn('   Page 100:  ', Page100Time, ' ms');
  WriteLn('   Page 1000: ', Page1000Time, ' ms');
  WriteLn('   Page 5000: ', Page5000Time, ' ms');
  WriteLn('');
  WriteLn('   Problem: SQLite must scan and skip OFFSET rows!');
  WriteLn('');
end;

{ Fetches two consecutive pages of posts using keyset (cursor-based) pagination, where each page uses the last id from the previous page as the cursor. }
procedure DemoCursorPagination;
var
  DS: TDataSet;
  LastId: Integer;
begin
  WriteLn('3. Cursor-based (Keyset) Pagination');
  WriteLn('   ---------------------------------');

  // First page - no cursor
  WriteLn('   First page (no cursor):');
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title FROM posts ORDER BY id LIMIT %d', [PAGE_SIZE]));
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              Copy(DS.FieldByName('title').AsString, 1, 30));
      LastId := DS.FieldByName('id').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Next page - use cursor (last id from previous page)
  WriteLn('');
  WriteLn('   Next page (cursor = ', LastId, '):');
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title FROM posts WHERE id > %d ORDER BY id LIMIT %d',
      [LastId, PAGE_SIZE]));
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              Copy(DS.FieldByName('title').AsString, 1, 30));
      LastId := DS.FieldByName('id').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Cursor pagination uses index - O(1) for any page!');
  WriteLn('');
end;

{ Compares cursor-based pagination timing for page 1 versus a deep page to show consistent O(1) performance. }
procedure DemoCursorPerformance;
var
  DS: TDataSet;
  StartTime: Int64;
  LastId: Integer;
  Page1Time, DeepPageTime: Int64;
  I: Integer;
begin
  WriteLn('4. Cursor-based Performance');
  WriteLn('   -------------------------');

  // First page
  StartTime := GetTickCount64;
  DS := Connection.ExecuteQuery(
    Format('SELECT id FROM posts ORDER BY id LIMIT %d', [PAGE_SIZE]));
  try
    while not DS.EOF do
    begin
      LastId := DS.FieldByName('id').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  Page1Time := GetTickCount64 - StartTime;

  // Simulate navigating to page 5000 using cursors
  // (In practice, you'd only fetch page by page)
  StartTime := GetTickCount64;
  LastId := 4999 * PAGE_SIZE;  // Jump to approximate position
  DS := Connection.ExecuteQuery(
    Format('SELECT id FROM posts WHERE id > %d ORDER BY id LIMIT %d',
      [LastId, PAGE_SIZE]));
  DS.Free;
  DeepPageTime := GetTickCount64 - StartTime;

  WriteLn('   Page 1:         ', Page1Time, ' ms');
  WriteLn('   Deep page:      ', DeepPageTime, ' ms (same speed!)');
  WriteLn('');
end;

{ Paginates posts sorted by views descending using a composite cursor (views, id) for tie-breaking across pages. }
procedure DemoSortedPagination;
var
  DS: TDataSet;
  LastViews, LastId: Integer;
begin
  WriteLn('5. Cursor pagination with custom sort');
  WriteLn('   -----------------------------------');

  // Sort by views DESC, then id for tie-breaking
  WriteLn('   Top posts by views (first page):');
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title, views FROM posts ORDER BY views DESC, id LIMIT %d',
      [PAGE_SIZE]));
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              DS.FieldByName('views').AsInteger, ' views - ',
              Copy(DS.FieldByName('title').AsString, 1, 20));
      LastViews := DS.FieldByName('views').AsInteger;
      LastId := DS.FieldByName('id').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Next page with composite cursor
  WriteLn('');
  WriteLn('   Next page (views <= ', LastViews, ', id > ', LastId, '):');
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title, views FROM posts ' +
           'WHERE (views < %d) OR (views = %d AND id > %d) ' +
           'ORDER BY views DESC, id LIMIT %d',
      [LastViews, LastViews, LastId, PAGE_SIZE]));
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              DS.FieldByName('views').AsInteger, ' views - ',
              Copy(DS.FieldByName('title').AsString, 1, 20));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Shows two methods for obtaining total row count alongside paginated results: a separate COUNT query and a window function approach. }
procedure DemoTotalCount;
var
  DS: TDataSet;
  TotalRows, TotalPages: Integer;
begin
  WriteLn('6. Total count with pagination');
  WriteLn('   ----------------------------');

  // Method 1: Separate COUNT query
  TotalRows := Connection.ExecuteScalar('SELECT COUNT(*) FROM posts');
  TotalPages := (TotalRows + PAGE_SIZE - 1) div PAGE_SIZE;
  WriteLn('   Method 1 - Separate COUNT:');
  WriteLn('     Total rows: ', TotalRows);
  WriteLn('     Total pages: ', TotalPages);

  // Method 2: Window function (SQLite 3.25+)
  WriteLn('');
  WriteLn('   Method 2 - Window function (single query):');
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title, COUNT(*) OVER() as total_count ' +
           'FROM posts ORDER BY id LIMIT %d', [PAGE_SIZE]));
  try
    if not DS.IsEmpty then
    begin
      WriteLn('     Total: ', DS.FieldByName('total_count').AsInteger);
      WriteLn('     First page items:');
      while not DS.EOF do
      begin
        WriteLn('       ', DS.FieldByName('id').AsInteger);
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Simulates three successive scroll loads using cursor-based pagination, fetching the next batch of rows each time using the last seen id. }
procedure DemoInfiniteScroll;
var
  DS: TDataSet;
  LastId: Integer;
  BatchNum: Integer;
begin
  WriteLn('7. Infinite scroll pattern');
  WriteLn('   ------------------------');

  LastId := 0;
  BatchNum := 0;

  WriteLn('   Simulating 3 scroll loads:');

  while BatchNum < 3 do
  begin
    Inc(BatchNum);
    Write('   Batch ', BatchNum, ': ');

    if LastId = 0 then
      DS := Connection.ExecuteQuery(
        Format('SELECT id, title FROM posts ORDER BY id LIMIT %d', [PAGE_SIZE]))
    else
      DS := Connection.ExecuteQuery(
        Format('SELECT id, title FROM posts WHERE id > %d ORDER BY id LIMIT %d',
          [LastId, PAGE_SIZE]));
    try
      Write('IDs ');
      while not DS.EOF do
      begin
        Write(DS.FieldByName('id').AsInteger);
        LastId := DS.FieldByName('id').AsInteger;
        if not DS.EOF then DS.Next;
        if not DS.EOF then Write(', ');
      end;
      WriteLn;
    finally
      DS.Free;
    end;
  end;

  WriteLn('');
  WriteLn('   Client stores lastId and sends it with next request.');
  WriteLn('');
end;

{ Paginates posts filtered by a specific author using cursor-based pagination combined with a WHERE clause on the author column. }
procedure DemoPaginationWithFilters;
var
  DS: TDataSet;
  Author: string;
  LastId: Integer;
begin
  WriteLn('8. Pagination with filters');
  WriteLn('   ------------------------');

  Author := 'Author42';

  // First page filtered by author
  WriteLn('   Posts by ', Author, ' (first page):');
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title, views FROM posts WHERE author = ? ORDER BY id LIMIT %d',
      [PAGE_SIZE]),
    [Author]);
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              Copy(DS.FieldByName('title').AsString, 1, 30));
      LastId := DS.FieldByName('id').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Next page with same filter
  WriteLn('');
  WriteLn('   Next page (cursor=', LastId, '):');
  DS := Connection.ExecuteQuery(
    Format('SELECT id, title FROM posts WHERE author = ? AND id > ? ORDER BY id LIMIT %d',
      [PAGE_SIZE]),
    [Author, LastId]);
  try
    while not DS.EOF do
    begin
      WriteLn('     ', DS.FieldByName('id').AsInteger, ': ',
              Copy(DS.FieldByName('title').AsString, 1, 30));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 32: Pagination ===');
  WriteLn('');

  DBPath := ExtractFilePath(ParamStr(0)) + 'example32.db';
  Cleanup;
  Randomize;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupData;

      DemoLimitOffset;
      DemoLimitOffsetPerformance;
      DemoCursorPagination;
      DemoCursorPerformance;
      DemoSortedPagination;
      DemoTotalCount;
      DemoInfiniteScroll;
      DemoPaginationWithFilters;

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
