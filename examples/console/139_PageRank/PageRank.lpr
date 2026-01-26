{===============================================================================
  NDXSQLite Example 139 - PageRank
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - PageRank algorithm with iterative computation
  - Damping factor comparison
  - Convergence detection with threshold
  - Graph link structure analysis
  - Top-N page extraction by rank

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program PageRank;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Web pages (nodes)
  Conn.ExecuteNonQuery(
    'CREATE TABLE pages (' +
    '  id INTEGER PRIMARY KEY, ' +
    '  url TEXT NOT NULL UNIQUE, ' +
    '  title TEXT NOT NULL)');

  // Links (directed edges)
  Conn.ExecuteNonQuery(
    'CREATE TABLE links (' +
    '  id INTEGER PRIMARY KEY, ' +
    '  from_page TEXT NOT NULL, ' +
    '  to_page TEXT NOT NULL, ' +
    '  UNIQUE(from_page, to_page))');

  // PageRank scores (iterative)
  Conn.ExecuteNonQuery(
    'CREATE TABLE pagerank (' +
    '  page TEXT NOT NULL, ' +
    '  iteration INTEGER NOT NULL, ' +
    '  score REAL NOT NULL, ' +
    '  PRIMARY KEY(page, iteration))');

  // Convergence history
  Conn.ExecuteNonQuery(
    'CREATE TABLE convergence (' +
    '  iteration INTEGER PRIMARY KEY, ' +
    '  max_delta REAL NOT NULL, ' +
    '  avg_delta REAL NOT NULL, ' +
    '  damping REAL NOT NULL)');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Web pages - a small web-like graph
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''home'', ''Homepage'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''about'', ''About Us'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''blog'', ''Blog Index'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''post1'', ''Blog Post 1'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''post2'', ''Blog Post 2'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''post3'', ''Blog Post 3'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''docs'', ''Documentation'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''api'', ''API Reference'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''contact'', ''Contact Page'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''faq'', ''FAQ'')');
  // Dangling node (no outbound links)
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''orphan'', ''Orphan Page'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (url, title) VALUES (''archive'', ''Archive'')');

  // Links - homepage is a hub
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''home'', ''about'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''home'', ''blog'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''home'', ''docs'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''home'', ''contact'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''home'', ''faq'')');

  // About links back to home and to contact
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''about'', ''home'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''about'', ''contact'')');

  // Blog is a mini-hub
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''blog'', ''post1'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''blog'', ''post2'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''blog'', ''post3'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''blog'', ''home'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''blog'', ''archive'')');

  // Blog posts link to each other and back to blog
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post1'', ''blog'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post1'', ''post2'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post1'', ''docs'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post2'', ''blog'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post2'', ''post3'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post3'', ''blog'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post3'', ''home'')');

  // Docs links
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''docs'', ''api'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''docs'', ''home'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''docs'', ''faq'')');

  // API links back to docs
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''api'', ''docs'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''api'', ''home'')');

  // Contact links
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''contact'', ''home'')');

  // FAQ links
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''faq'', ''home'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''faq'', ''docs'')');
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''faq'', ''contact'')');

  // Archive links (receives links but has few outbound)
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''archive'', ''home'')');

  // Orphan page has inbound but NO outbound links (dangling node)
  Conn.ExecuteNonQuery('INSERT INTO links (from_page, to_page) VALUES (''post2'', ''orphan'')');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Runs iterative PageRank computation with the given damping factor, recording scores per iteration and convergence metrics until max delta falls below threshold. }
procedure RunPageRank(Damping: Double; MaxIterations: Integer; Threshold: Double);
var
  DS: TDataSet;
  Iteration: Integer;
  N: Integer;
  MaxDelta, AvgDelta: Double;
  TeleportScore: Double;
begin
  // Get number of pages
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM pages');
  try
    DS.First;
    N := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  // Initialize: each page gets 1/N
  Conn.ExecuteNonQuery(Format(
    'INSERT OR REPLACE INTO pagerank (page, iteration, score) ' +
    'SELECT url, 0, 1.0/%d FROM pages', [N]));

  TeleportScore := (1.0 - Damping) / N;

  for Iteration := 1 to MaxIterations do
  begin
    // Calculate new PageRank for each page
    // PR(A) = (1-d)/N + d * SUM(PR(T)/OutDegree(T))
    // where T are pages linking TO A
    Conn.ExecuteNonQuery(Format(
      'INSERT OR REPLACE INTO pagerank (page, iteration, score) ' +
      'SELECT p.url, %d, ' +
      '  %.10f + %.10f * COALESCE((' +
      '    SELECT SUM(pr.score / CAST((' +
      '      SELECT COUNT(*) FROM links l2 WHERE l2.from_page = l.from_page' +
      '    ) AS REAL)) ' +
      '    FROM links l ' +
      '    JOIN pagerank pr ON pr.page = l.from_page AND pr.iteration = %d' +
      '    WHERE l.to_page = p.url' +
      '  ), 0.0) ' +
      'FROM pages p',
      [Iteration, TeleportScore, Damping, Iteration - 1]));

    // Calculate convergence metrics
    DS := Conn.ExecuteQuery(Format(
      'SELECT MAX(ABS(curr.score - prev.score)) as max_d, ' +
      '       AVG(ABS(curr.score - prev.score)) as avg_d ' +
      'FROM pagerank curr ' +
      'JOIN pagerank prev ON curr.page = prev.page ' +
      'WHERE curr.iteration = %d AND prev.iteration = %d',
      [Iteration, Iteration - 1]));
    try
      DS.First;
      MaxDelta := DS.FieldByName('max_d').AsFloat;
      AvgDelta := DS.FieldByName('avg_d').AsFloat;
    finally
      DS.Free;
    end;

    // Record convergence
    Conn.ExecuteNonQuery(Format(
      'INSERT OR REPLACE INTO convergence (iteration, max_delta, avg_delta, damping) ' +
      'VALUES (%d, %.10f, %.10f, %.2f)',
      [Iteration, MaxDelta, AvgDelta, Damping]));

    // Check convergence
    if MaxDelta < Threshold then
      Break;
  end;
end;

{ Displays total page and link counts, then lists each page with title, inbound links, and outbound links sorted by in-degree. }
procedure Demo1_GraphStructure;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Web Graph Structure ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM pages');
  try
    DS.First;
    WriteLn('   Pages (nodes): ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM links');
  try
    DS.First;
    WriteLn('   Links (edges): ', DS.FieldByName('cnt').AsInteger);
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Page             Title              In-links  Out-links');
  WriteLn('   ' + StringOfChar('-', 65));
  DS := Conn.ExecuteQuery(
    'SELECT p.url, p.title, ' +
    '  (SELECT COUNT(*) FROM links WHERE to_page = p.url) as in_links, ' +
    '  (SELECT COUNT(*) FROM links WHERE from_page = p.url) as out_links ' +
    'FROM pages p ORDER BY in_links DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-20s %3d       %3d', [
        DS.FieldByName('url').AsString,
        DS.FieldByName('title').AsString,
        DS.FieldByName('in_links').AsInteger,
        DS.FieldByName('out_links').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows the uniform initial PageRank distribution (1/N for each page) and verifies the total sums to approximately 1.0. }
procedure Demo2_InitialRank;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Initial PageRank (uniform distribution) ===');
  WriteLn;
  WriteLn('   Page             Initial Score');
  WriteLn('   ' + StringOfChar('-', 35));
  DS := Conn.ExecuteQuery(
    'SELECT page, score FROM pagerank WHERE iteration = 0 ORDER BY page');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %.6f', [
        DS.FieldByName('page').AsString,
        DS.FieldByName('score').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT SUM(score) as total FROM pagerank WHERE iteration = 0');
  try
    DS.First;
    WriteLn(Format('   %s', [StringOfChar('-', 35)]));
    WriteLn(Format('   Total:            %.6f (should be ~1.0)', [DS.FieldByName('total').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Compares PageRank scores between iteration 0 and iteration 1, showing which pages gained or lost rank. }
procedure Demo3_SingleIteration;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. After First Iteration (d=0.85) ===');
  WriteLn;
  WriteLn('   Page             Iter 0     Iter 1     Change');
  WriteLn('   ' + StringOfChar('-', 55));
  DS := Conn.ExecuteQuery(
    'SELECT p0.page, p0.score as s0, p1.score as s1, ' +
    '  (p1.score - p0.score) as delta ' +
    'FROM pagerank p0 ' +
    'JOIN pagerank p1 ON p0.page = p1.page ' +
    'WHERE p0.iteration = 0 AND p1.iteration = 1 ' +
    'ORDER BY p1.score DESC');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('delta').AsFloat >= 0 then
        WriteLn(Format('   %-16s %.6f   %.6f   +%.6f', [
          DS.FieldByName('page').AsString,
          DS.FieldByName('s0').AsFloat,
          DS.FieldByName('s1').AsFloat,
          DS.FieldByName('delta').AsFloat]))
      else
        WriteLn(Format('   %-16s %.6f   %.6f   %.6f', [
          DS.FieldByName('page').AsString,
          DS.FieldByName('s0').AsFloat,
          DS.FieldByName('s1').AsFloat,
          DS.FieldByName('delta').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   Pages gaining rank: those with many/important inbound links');
  WriteLn('   Pages losing rank: those with few inbound links');
  WriteLn;
end;

{ Displays max and average delta for each iteration until convergence, showing how score changes decrease over time. }
procedure Demo4_IterativeConvergence;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Iterative Convergence (d=0.85) ===');
  WriteLn;
  WriteLn('   Iteration  Max Delta    Avg Delta    Status');
  WriteLn('   ' + StringOfChar('-', 55));
  DS := Conn.ExecuteQuery(
    'SELECT iteration, max_delta, avg_delta FROM convergence ' +
    'WHERE damping = 0.85 ORDER BY iteration');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %4d       %.8f  %.8f  %s', [
        DS.FieldByName('iteration').AsInteger,
        DS.FieldByName('max_delta').AsFloat,
        DS.FieldByName('avg_delta').AsFloat,
        BoolToStr(DS.FieldByName('max_delta').AsFloat < 0.0001,
          'CONVERGED', '')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Reports the iteration at which convergence was reached and displays a text-based bar chart of max delta decreasing over iterations. }
procedure Demo5_ConvergenceDetection;
var
  DS: TDataSet;
  FinalIter: Integer;
begin
  WriteLn('=== 5. Convergence Detection ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT MAX(iteration) as final_iter FROM convergence WHERE damping = 0.85');
  try
    DS.First;
    FinalIter := DS.FieldByName('final_iter').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Converged after %d iterations (threshold: 0.0001)', [FinalIter]));
  WriteLn;

  // Show how delta decreases exponentially
  WriteLn('   Convergence rate (log scale):');
  DS := Conn.ExecuteQuery(
    'SELECT iteration, max_delta, ' +
    '  CAST(max_delta / 0.05 * 40 AS INTEGER) as bar_len ' +
    'FROM convergence WHERE damping = 0.85 ORDER BY iteration');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Iter %2d: %s (%.8f)', [
        DS.FieldByName('iteration').AsInteger,
        StringOfChar('#', DS.FieldByName('bar_len').AsInteger),
        DS.FieldByName('max_delta').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Runs PageRank with damping factors 0.50, 0.85, and 0.95, comparing convergence speed and top-5 scores for each setting. }
procedure Demo6_DampingComparison;
var
  DS: TDataSet;
  FinalIter: Integer;
begin
  WriteLn('=== 6. Damping Factor Comparison ===');
  WriteLn;

  // Run with d=0.50
  Conn.ExecuteNonQuery('DELETE FROM pagerank');
  Conn.ExecuteNonQuery('DELETE FROM convergence');
  RunPageRank(0.50, 50, 0.0001);

  DS := Conn.ExecuteQuery('SELECT MAX(iteration) as fi FROM convergence WHERE damping = 0.50');
  try
    DS.First;
    FinalIter := DS.FieldByName('fi').AsInteger;
  finally
    DS.Free;
  end;
  WriteLn(Format('   d=0.50: converged in %d iterations', [FinalIter]));

  WriteLn('   Page             Score (d=0.50)');
  DS := Conn.ExecuteQuery(Format(
    'SELECT page, score FROM pagerank WHERE iteration = %d ORDER BY score DESC LIMIT 5',
    [FinalIter]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %.6f', [
        DS.FieldByName('page').AsString,
        DS.FieldByName('score').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Run with d=0.85
  Conn.ExecuteNonQuery('DELETE FROM pagerank');
  Conn.ExecuteNonQuery('DELETE FROM convergence');
  RunPageRank(0.85, 50, 0.0001);

  DS := Conn.ExecuteQuery('SELECT MAX(iteration) as fi FROM convergence WHERE damping = 0.85');
  try
    DS.First;
    FinalIter := DS.FieldByName('fi').AsInteger;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn(Format('   d=0.85: converged in %d iterations', [FinalIter]));

  WriteLn('   Page             Score (d=0.85)');
  DS := Conn.ExecuteQuery(Format(
    'SELECT page, score FROM pagerank WHERE iteration = %d ORDER BY score DESC LIMIT 5',
    [FinalIter]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %.6f', [
        DS.FieldByName('page').AsString,
        DS.FieldByName('score').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Run with d=0.95
  Conn.ExecuteNonQuery('DELETE FROM pagerank');
  Conn.ExecuteNonQuery('DELETE FROM convergence');
  RunPageRank(0.95, 80, 0.0001);

  DS := Conn.ExecuteQuery('SELECT MAX(iteration) as fi FROM convergence WHERE damping = 0.95');
  try
    DS.First;
    FinalIter := DS.FieldByName('fi').AsInteger;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn(Format('   d=0.95: converged in %d iterations', [FinalIter]));

  WriteLn('   Page             Score (d=0.95)');
  DS := Conn.ExecuteQuery(Format(
    'SELECT page, score FROM pagerank WHERE iteration = %d ORDER BY score DESC LIMIT 5',
    [FinalIter]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %.6f', [
        DS.FieldByName('page').AsString,
        DS.FieldByName('score').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Higher damping -> more iterations, larger rank spread');
  WriteLn;
end;

{ Displays the final converged PageRank scores for all pages ranked from highest to lowest with a visual bar chart. }
procedure Demo7_TopNExtraction;
var
  DS: TDataSet;
  Rank: Integer;
  FinalIter: Integer;
begin
  WriteLn('=== 7. Top-N PageRank Extraction (d=0.85) ===');
  WriteLn;

  // Re-run with d=0.85 for final results
  Conn.ExecuteNonQuery('DELETE FROM pagerank');
  Conn.ExecuteNonQuery('DELETE FROM convergence');
  RunPageRank(0.85, 50, 0.0001);

  DS := Conn.ExecuteQuery('SELECT MAX(iteration) as fi FROM convergence WHERE damping = 0.85');
  try
    DS.First;
    FinalIter := DS.FieldByName('fi').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn('   Final rankings after convergence:');
  WriteLn;
  WriteLn('   Rank  Page             Score     Bar');
  WriteLn('   ' + StringOfChar('-', 60));
  DS := Conn.ExecuteQuery(Format(
    'SELECT page, score, ' +
    '  CAST(score * 120 AS INTEGER) as bar_len ' +
    'FROM pagerank WHERE iteration = %d ORDER BY score DESC',
    [FinalIter]));
  try
    Rank := 0;
    while not DS.EOF do
    begin
      Inc(Rank);
      WriteLn(Format('   %2d.   %-16s %.6f  %s', [
        Rank,
        DS.FieldByName('page').AsString,
        DS.FieldByName('score').AsFloat,
        StringOfChar('#', DS.FieldByName('bar_len').AsInteger)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows PageRank statistics (sum, min, max, avg, spread) and groups pages into score tiers with counts and tier totals. }
procedure Demo8_RankDistribution;
var
  DS: TDataSet;
  FinalIter: Integer;
begin
  WriteLn('=== 8. PageRank Distribution Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT MAX(iteration) as fi FROM convergence WHERE damping = 0.85');
  try
    DS.First;
    FinalIter := DS.FieldByName('fi').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(Format(
    'SELECT SUM(score) as total, ' +
    '  MIN(score) as min_score, MAX(score) as max_score, ' +
    '  AVG(score) as avg_score, ' +
    '  MAX(score) / MIN(score) as spread ' +
    'FROM pagerank WHERE iteration = %d', [FinalIter]));
  try
    DS.First;
    WriteLn(Format('   Total score sum: %.6f (<1.0 due to dangling node rank leak)', [DS.FieldByName('total').AsFloat]));
    WriteLn(Format('   Min score:       %.6f', [DS.FieldByName('min_score').AsFloat]));
    WriteLn(Format('   Max score:       %.6f', [DS.FieldByName('max_score').AsFloat]));
    WriteLn(Format('   Avg score:       %.6f', [DS.FieldByName('avg_score').AsFloat]));
    WriteLn(Format('   Spread (max/min): %.2fx', [DS.FieldByName('spread').AsFloat]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Score distribution by quartile:');
  DS := Conn.ExecuteQuery(Format(
    'SELECT ' +
    '  CASE ' +
    '    WHEN score >= 0.12 THEN ''high (>=0.12)'' ' +
    '    WHEN score >= 0.08 THEN ''medium (0.08-0.12)'' ' +
    '    WHEN score >= 0.05 THEN ''low (0.05-0.08)'' ' +
    '    ELSE ''minimal (<0.05)'' ' +
    '  END as tier, ' +
    '  COUNT(*) as cnt, ' +
    '  ROUND(SUM(score), 4) as tier_total ' +
    'FROM pagerank WHERE iteration = %d ' +
    'GROUP BY tier ORDER BY tier_total DESC', [FinalIter]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %2d pages, total score %.4f', [
        DS.FieldByName('tier').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('tier_total').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Identifies pages with no outbound links (rank sinks) and pages with no inbound links, explaining their impact on PageRank distribution. }
procedure Demo9_DanglingNodes;
var
  DS: TDataSet;
  FinalIter: Integer;
begin
  WriteLn('=== 9. Dangling Nodes (no outbound links) ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT MAX(iteration) as fi FROM convergence WHERE damping = 0.85');
  try
    DS.First;
    FinalIter := DS.FieldByName('fi').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn('   Pages with no outbound links (rank sinks):');
  DS := Conn.ExecuteQuery(Format(
    'SELECT p.url, p.title, pr.score, ' +
    '  (SELECT COUNT(*) FROM links WHERE to_page = p.url) as in_links ' +
    'FROM pages p ' +
    'JOIN pagerank pr ON pr.page = p.url AND pr.iteration = %d ' +
    'WHERE p.url NOT IN (SELECT DISTINCT from_page FROM links) ' +
    'ORDER BY pr.score DESC', [FinalIter]));
  try
    if DS.EOF then
      WriteLn('   (none)')
    else
    begin
      while not DS.EOF do
      begin
        WriteLn(Format('   - %s (%s): score=%.6f, %d inbound links', [
          DS.FieldByName('url').AsString,
          DS.FieldByName('title').AsString,
          DS.FieldByName('score').AsFloat,
          DS.FieldByName('in_links').AsInteger]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Impact: Dangling nodes absorb rank without distributing it.');
  WriteLn('   In practice, their rank is redistributed uniformly (teleport).');

  WriteLn;
  WriteLn('   Pages with no inbound links (unreferenced):');
  DS := Conn.ExecuteQuery(Format(
    'SELECT p.url, p.title, pr.score, ' +
    '  (SELECT COUNT(*) FROM links WHERE from_page = p.url) as out_links ' +
    'FROM pages p ' +
    'JOIN pagerank pr ON pr.page = p.url AND pr.iteration = %d ' +
    'WHERE p.url NOT IN (SELECT DISTINCT to_page FROM links) ' +
    'ORDER BY pr.score DESC', [FinalIter]));
  try
    if DS.EOF then
      WriteLn('   (none - all pages have at least one inbound link)')
    else
    begin
      while not DS.EOF do
      begin
        WriteLn(Format('   - %s (%s): score=%.6f, %d outbound links', [
          DS.FieldByName('url').AsString,
          DS.FieldByName('title').AsString,
          DS.FieldByName('score').AsFloat,
          DS.FieldByName('out_links').AsInteger]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows hub pages (high out-degree), authority pages (high in-degree), and correlates PageRank with in-degree to illustrate that rank depends on link quality not just quantity. }
procedure Demo10_LinkAnalysis;
var
  DS: TDataSet;
  FinalIter: Integer;
begin
  WriteLn('=== 10. Link Structure Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT MAX(iteration) as fi FROM convergence WHERE damping = 0.85');
  try
    DS.First;
    FinalIter := DS.FieldByName('fi').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn('   Hub pages (high out-degree, distribute rank):');
  DS := Conn.ExecuteQuery(
    'SELECT from_page, COUNT(*) as out_deg FROM links ' +
    'GROUP BY from_page ORDER BY out_deg DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %d outbound links', [
        DS.FieldByName('from_page').AsString,
        DS.FieldByName('out_deg').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Authority pages (high in-degree, receive rank):');
  DS := Conn.ExecuteQuery(
    'SELECT to_page, COUNT(*) as in_deg FROM links ' +
    'GROUP BY to_page ORDER BY in_deg DESC LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %d inbound links', [
        DS.FieldByName('to_page').AsString,
        DS.FieldByName('in_deg').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Rank vs. In-degree correlation:');
  WriteLn('   Page             Rank      In-deg  Rank/In-deg');
  WriteLn('   ' + StringOfChar('-', 55));
  DS := Conn.ExecuteQuery(Format(
    'SELECT pr.page, pr.score, ' +
    '  (SELECT COUNT(*) FROM links WHERE to_page = pr.page) as in_deg ' +
    'FROM pagerank pr ' +
    'WHERE pr.iteration = %d ' +
    'ORDER BY pr.score DESC LIMIT 8', [FinalIter]));
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('in_deg').AsInteger > 0 then
        WriteLn(Format('   %-16s %.6f  %3d     %.4f', [
          DS.FieldByName('page').AsString,
          DS.FieldByName('score').AsFloat,
          DS.FieldByName('in_deg').AsInteger,
          DS.FieldByName('score').AsFloat / DS.FieldByName('in_deg').AsInteger]))
      else
        WriteLn(Format('   %-16s %.6f  %3d     (no inbound)', [
          DS.FieldByName('page').AsString,
          DS.FieldByName('score').AsFloat,
          DS.FieldByName('in_deg').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Key insight: PageRank != in-degree. A single link from a');
  WriteLn('   high-rank page is worth more than many links from low-rank pages.');
end;

begin
  WriteLn('Example 139: PageRank - Iterative Link-Based Ranking');
  WriteLn(StringOfChar('=', 70));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    // Run initial PageRank with d=0.85
    RunPageRank(0.85, 50, 0.0001);

    Demo1_GraphStructure;
    Demo2_InitialRank;
    Demo3_SingleIteration;
    Demo4_IterativeConvergence;
    Demo5_ConvergenceDetection;
    Demo6_DampingComparison;
    Demo7_TopNExtraction;
    Demo8_RankDistribution;
    Demo9_DanglingNodes;
    Demo10_LinkAnalysis;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
