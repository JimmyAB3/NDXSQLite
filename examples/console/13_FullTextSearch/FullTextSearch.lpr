{===============================================================================
  NDXSQLite Example 13 - Full-Text Search (FTS5)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Creating FTS5 virtual tables
  - Full-text search queries
  - Phrase and boolean search
  - Snippets and highlighting
  - Search ranking

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program FullTextSearch;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqlitefts;

var
  Conn: INDXSQLiteConnection;
  FTS: TNDXSQLiteFTS;
  DataSet: TDataSet;
  DBPath: string;
  FTSOptions: TNDXFTSSearchOptions;

begin
  WriteLn('=== NDXSQLite Example 13: Full-Text Search (FTS5) ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example13.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Conn := TNDXSQLiteConnection.Create(DBPath);
  Conn.Open;

  FTS := TNDXSQLiteFTS.Create(Conn);
    try
      // 1. Create FTS5 table
      WriteLn('1. Creating FTS5 table...');
      FTS.CreateFTSTable('articles', ['title', 'content', 'author']);
      WriteLn('   FTS5 table "articles" created.');
      WriteLn;

      // 2. Insert sample data
      WriteLn('2. Inserting sample articles...');
      Conn.ExecuteNonQuery(
        'INSERT INTO articles (title, content, author) VALUES (?, ?, ?)',
        ['Introduction to Pascal', 'Pascal is a procedural programming language designed in 1968-1969. It was developed by Niklaus Wirth as a language suitable for teaching programming.', 'John Smith']);

      Conn.ExecuteNonQuery(
        'INSERT INTO articles (title, content, author) VALUES (?, ?, ?)',
        ['Free Pascal Compiler', 'Free Pascal is a mature, open source Pascal compiler. It is compatible with Turbo Pascal and Delphi. Free Pascal supports multiple platforms including Windows, Linux, and macOS.', 'Jane Doe']);

      Conn.ExecuteNonQuery(
        'INSERT INTO articles (title, content, author) VALUES (?, ?, ?)',
        ['Lazarus IDE Guide', 'Lazarus is a free cross-platform visual integrated development environment for rapid application development using the Free Pascal compiler.', 'Bob Johnson']);

      Conn.ExecuteNonQuery(
        'INSERT INTO articles (title, content, author) VALUES (?, ?, ?)',
        ['SQLite Database Tutorial', 'SQLite is a C-language library that implements a small, fast, self-contained SQL database engine. SQLite is the most used database engine in the world.', 'Alice Brown']);

      Conn.ExecuteNonQuery(
        'INSERT INTO articles (title, content, author) VALUES (?, ?, ?)',
        ['Database Programming with Pascal', 'Learn how to use databases in your Pascal applications. This tutorial covers SQLite, PostgreSQL, and MySQL connections using Free Pascal.', 'Charlie Wilson']);

      WriteLn('   5 articles inserted.');
      WriteLn;

      // 3. Simple search
      WriteLn('3. Simple search for "Pascal":');
      DataSet := FTS.Search('articles', 'Pascal');
      try
        while not DataSet.EOF do
        begin
          WriteLn('   - ', DataSet.FieldByName('title').AsString);
          DataSet.Next;
        end;
        WriteLn('   Found: ', DataSet.RecordCount, ' results');
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 4. Search with ranking
      WriteLn('4. Search with BM25 ranking:');
      DataSet := FTS.SearchWithRank('articles', 'Pascal programming');
      try
        while not DataSet.EOF do
        begin
          WriteLn(Format('   - %s (score: %.4f)', [
            DataSet.FieldByName('title').AsString,
            DataSet.FieldByName('score').AsFloat
          ]));
          DataSet.Next;
        end;
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 5. Phrase search
      WriteLn('5. Phrase search for "Free Pascal":');
      DataSet := FTS.SearchPhrase('articles', 'Free Pascal');
      try
        while not DataSet.EOF do
        begin
          WriteLn('   - ', DataSet.FieldByName('title').AsString);
          DataSet.Next;
        end;
        WriteLn('   Found: ', DataSet.RecordCount, ' results');
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 6. Prefix search
      WriteLn('6. Prefix search for "data*":');
      DataSet := FTS.SearchPrefix('articles', 'data');
      try
        while not DataSet.EOF do
        begin
          WriteLn('   - ', DataSet.FieldByName('title').AsString);
          DataSet.Next;
        end;
        WriteLn('   Found: ', DataSet.RecordCount, ' results');
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 7. Boolean search
      WriteLn('7. Boolean search "Pascal AND database":');
      DataSet := FTS.SearchBoolean('articles', 'Pascal AND database');
      try
        while not DataSet.EOF do
        begin
          WriteLn('   - ', DataSet.FieldByName('title').AsString);
          DataSet.Next;
        end;
        WriteLn('   Found: ', DataSet.RecordCount, ' results');
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 8. Search with snippets
      WriteLn('8. Search with highlighted snippets:');
      // Note: DefaultOptions is a record, so we get it, modify, and set it back
      FTSOptions := FTS.DefaultOptions;
      FTSOptions.HighlightStart := '>>>';
      FTSOptions.HighlightEnd := '<<<';
      FTSOptions.SnippetSize := 32;
      FTS.DefaultOptions := FTSOptions;

      DataSet := FTS.SearchWithSnippet('articles', 'programming', 'content');
      try
        while not DataSet.EOF do
        begin
          WriteLn('   Title: ', DataSet.FieldByName('title').AsString);
          WriteLn('   Snippet: ', DataSet.FieldByName('snippet').AsString);
          WriteLn;
          DataSet.Next;
        end;
      finally
        DataSet.Free;
      end;

      // 9. NEAR search
      WriteLn('9. NEAR search ("Pascal" NEAR "compiler"):');
      DataSet := FTS.SearchNear('articles', ['Pascal', 'compiler'], 10);
      try
        while not DataSet.EOF do
        begin
          WriteLn('   - ', DataSet.FieldByName('title').AsString);
          DataSet.Next;
        end;
        WriteLn('   Found: ', DataSet.RecordCount, ' results');
      finally
        DataSet.Free;
      end;
      WriteLn;

      // 10. Index maintenance
      WriteLn('10. FTS index maintenance:');
      Write('    Optimizing index...');
      FTS.OptimizeIndex('articles');
      WriteLn(' Done');

      Write('    Integrity check...');
      FTS.IntegrityCheck('articles');
      WriteLn(' OK');
      WriteLn;

      // 11. Check if FTS table exists
      WriteLn('11. Checking FTS table existence:');
      WriteLn('    "articles" exists: ', FTS.FTSTableExists('articles'));
      WriteLn('    "nonexistent" exists: ', FTS.FTSTableExists('nonexistent'));
      WriteLn;

      // 12. Drop FTS table
      WriteLn('12. Cleanup:');
      FTS.DropFTSTable('articles');
      WriteLn('    FTS table dropped.');
      WriteLn;

      // 13. Best practices
      WriteLn('13. FTS5 Best Practices:');
      WriteLn('    - Use FTS for text-heavy search (articles, logs)');
      WriteLn('    - Create content tables for large data, use external content');
      WriteLn('    - Optimize index periodically');
      WriteLn('    - Use prefix search for autocomplete');
      WriteLn('    - BM25 ranking for relevance sorting');
      WriteLn;

  finally
    FTS.Free;
  end;

  Conn.Close;
  Conn := nil;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
