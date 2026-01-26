{===============================================================================
  NDXSQLite Example 140 - Recommendation Engine
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - User-item interaction matrix
  - User-based collaborative filtering
  - Item-based similarity recommendations
  - Also-bought association patterns
  - Top-N recommendations per user

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program RecommendationEngine;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Users
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY, ' +
    '  name TEXT NOT NULL, ' +
    '  age_group TEXT NOT NULL)');

  // Products
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY, ' +
    '  name TEXT NOT NULL, ' +
    '  category TEXT NOT NULL, ' +
    '  price REAL NOT NULL)');

  // Purchases (user-item interactions with ratings)
  Conn.ExecuteNonQuery(
    'CREATE TABLE purchases (' +
    '  user_id INTEGER NOT NULL, ' +
    '  product_id INTEGER NOT NULL, ' +
    '  rating INTEGER NOT NULL, ' +
    '  purchase_date TEXT NOT NULL, ' +
    '  PRIMARY KEY(user_id, product_id))');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Users
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (1, ''Alice'', ''25-34'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (2, ''Bob'', ''25-34'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (3, ''Carol'', ''35-44'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (4, ''Dave'', ''18-24'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (5, ''Eve'', ''35-44'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (6, ''Frank'', ''25-34'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (7, ''Grace'', ''18-24'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (8, ''Hank'', ''45-54'')');
  // New user with very few purchases (cold start)
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (9, ''Iris'', ''25-34'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (10, ''Jack'', ''35-44'')');

  // Products (books, electronics, music)
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (1, ''Python Cookbook'', ''books'', 39.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (2, ''Clean Code'', ''books'', 34.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (3, ''Design Patterns'', ''books'', 44.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (4, ''SQL Mastery'', ''books'', 29.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (5, ''Wireless Mouse'', ''electronics'', 24.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (6, ''Mechanical Keyboard'', ''electronics'', 89.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (7, ''USB Hub'', ''electronics'', 19.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (8, ''Monitor Stand'', ''electronics'', 49.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (9, ''Jazz Collection'', ''music'', 14.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (10, ''Classical Piano'', ''music'', 12.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (11, ''Rock Anthology'', ''music'', 15.99)');
  Conn.ExecuteNonQuery('INSERT INTO products VALUES (12, ''Headphones'', ''electronics'', 59.99)');

  // Alice: developer who likes coding books + peripherals
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (1, 1, 5, ''2025-01-10'')');  // Python Cookbook
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (1, 2, 4, ''2025-01-12'')');  // Clean Code
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (1, 3, 5, ''2025-01-15'')');  // Design Patterns
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (1, 6, 4, ''2025-01-20'')');  // Mech Keyboard
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (1, 12, 5, ''2025-02-01'')'); //Headphones

  // Bob: similar to Alice (coding books + electronics)
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (2, 1, 4, ''2025-01-08'')');  //Python Cookbook
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (2, 2, 5, ''2025-01-11'')');  //Clean Code
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (2, 4, 4, ''2025-01-14'')');  //SQL Mastery
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (2, 5, 3, ''2025-01-18'')');  //Wireless Mouse
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (2, 6, 5, ''2025-01-22'')');  //Mech Keyboard
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (2, 7, 4, ''2025-02-05'')');  //USB Hub

  // Carol: music lover + some books
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (3, 9, 5, ''2025-01-05'')');  //Jazz
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (3, 10, 4, ''2025-01-07'')'); //Classical
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (3, 11, 5, ''2025-01-09'')'); //Rock
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (3, 2, 3, ''2025-01-20'')');  //Clean Code
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (3, 12, 4, ''2025-02-10'')'); //Headphones

  // Dave: electronics enthusiast
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (4, 5, 5, ''2025-01-12'')');  //Wireless Mouse
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (4, 6, 5, ''2025-01-14'')');  //Mech Keyboard
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (4, 7, 4, ''2025-01-16'')');  //USB Hub
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (4, 8, 5, ''2025-01-18'')');  //Monitor Stand
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (4, 12, 4, ''2025-01-25'')'); //Headphones

  // Eve: similar to Carol (music + books)
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (5, 9, 4, ''2025-01-06'')');  //Jazz
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (5, 10, 5, ''2025-01-08'')'); //Classical
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (5, 3, 4, ''2025-01-15'')');  //Design Patterns
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (5, 12, 3, ''2025-01-28'')'); //Headphones

  // Frank: developer (similar to Alice/Bob)
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (6, 1, 5, ''2025-01-09'')');  //Python Cookbook
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (6, 3, 4, ''2025-01-12'')');  //Design Patterns
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (6, 4, 5, ''2025-01-15'')');  //SQL Mastery
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (6, 8, 3, ''2025-01-25'')');  //Monitor Stand

  // Grace: music + electronics
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (7, 9, 5, ''2025-01-10'')');  //Jazz
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (7, 11, 4, ''2025-01-12'')'); //Rock
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (7, 5, 4, ''2025-01-20'')');  //Wireless Mouse
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (7, 12, 5, ''2025-02-01'')'); //Headphones

  // Hank: eclectic
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (8, 2, 4, ''2025-01-11'')');  //Clean Code
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (8, 8, 5, ''2025-01-15'')');  //Monitor Stand
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (8, 10, 3, ''2025-01-20'')'); //Classical

  // Iris: new user (cold start - only 1 purchase)
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (9, 1, 5, ''2025-02-15'')');  //Python Cookbook

  // Jack: moderate user
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (10, 2, 4, ''2025-01-20'')');  //Clean Code
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (10, 9, 5, ''2025-01-22'')'); //Jazz
  Conn.ExecuteNonQuery('INSERT INTO purchases VALUES (10, 5, 3, ''2025-01-28'')'); //Wireless Mouse

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Displays total counts of users, products, and purchases, then lists products with category, price, and buyer counts. }
procedure Demo1_DataOverview;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Data Overview ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM users');
  try DS.First; WriteLn('   Users: ', DS.FieldByName('cnt').AsInteger); finally DS.Free; end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try DS.First; WriteLn('   Products: ', DS.FieldByName('cnt').AsInteger); finally DS.Free; end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM purchases');
  try DS.First; WriteLn('   Purchases: ', DS.FieldByName('cnt').AsInteger); finally DS.Free; end;

  WriteLn;
  WriteLn('   Product              Category     Price   Buyers');
  WriteLn('   ' + StringOfChar('-', 60));
  DS := Conn.ExecuteQuery(
    'SELECT p.name, p.category, p.price, ' +
    '  (SELECT COUNT(*) FROM purchases WHERE product_id = p.id) as buyers ' +
    'FROM products p ORDER BY buyers DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-12s $%.2f  %d', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('price').AsFloat,
        DS.FieldByName('buyers').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays the user-item rating matrix with ratings (1-5) or dots for unpurchased items across all 12 products. }
procedure Demo2_UserItemMatrix;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. User-Item Rating Matrix ===');
  WriteLn;
  WriteLn('   User     PyCk ClCd DsPt SQL  Mous Keyb Hub  Stnd Jazz Clss Rock Head');
  WriteLn('   ' + StringOfChar('-', 78));

  DS := Conn.ExecuteQuery(
    'SELECT u.name, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=1) as p1, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=2) as p2, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=3) as p3, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=4) as p4, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=5) as p5, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=6) as p6, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=7) as p7, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=8) as p8, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=9) as p9, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=10) as p10, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=11) as p11, ' +
    '  (SELECT rating FROM purchases WHERE user_id=u.id AND product_id=12) as p12 ' +
    'FROM users u ORDER BY u.id');
  try
    while not DS.EOF do
    begin
      Write(Format('   %-8s', [DS.FieldByName('name').AsString]));
      if VarIsNull(DS.FieldByName('p1').Value) then Write('  .  ') else Write(Format('  %d  ', [DS.FieldByName('p1').AsInteger]));
      if VarIsNull(DS.FieldByName('p2').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p2').AsInteger]));
      if VarIsNull(DS.FieldByName('p3').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p3').AsInteger]));
      if VarIsNull(DS.FieldByName('p4').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p4').AsInteger]));
      if VarIsNull(DS.FieldByName('p5').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p5').AsInteger]));
      if VarIsNull(DS.FieldByName('p6').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p6').AsInteger]));
      if VarIsNull(DS.FieldByName('p7').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p7').AsInteger]));
      if VarIsNull(DS.FieldByName('p8').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p8').AsInteger]));
      if VarIsNull(DS.FieldByName('p9').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p9').AsInteger]));
      if VarIsNull(DS.FieldByName('p10').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p10').AsInteger]));
      if VarIsNull(DS.FieldByName('p11').Value) then Write(' .  ') else Write(Format(' %d  ', [DS.FieldByName('p11').AsInteger]));
      if VarIsNull(DS.FieldByName('p12').Value) then WriteLn(' .') else WriteLn(Format(' %d', [DS.FieldByName('p12').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   Legend: . = not purchased, 1-5 = rating');
  WriteLn;
end;

{ Computes and displays Jaccard similarity between all user pairs, showing common items, union size, and similarity score. }
procedure Demo3_UserSimilarity;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. User Similarity (Jaccard Index) ===');
  WriteLn;
  WriteLn('   Jaccard = |items in common| / |items by either user|');
  WriteLn;
  WriteLn('   User A     User B     Common  Union   Jaccard');
  WriteLn('   ' + StringOfChar('-', 55));

  // Jaccard similarity between all user pairs
  DS := Conn.ExecuteQuery(
    'SELECT u1.name as user_a, u2.name as user_b, ' +
    '  (SELECT COUNT(*) FROM purchases p1 ' +
    '   JOIN purchases p2 ON p1.product_id = p2.product_id ' +
    '   WHERE p1.user_id = u1.id AND p2.user_id = u2.id) as common, ' +
    '  (SELECT COUNT(DISTINCT product_id) FROM purchases ' +
    '   WHERE user_id = u1.id OR user_id = u2.id) as union_size ' +
    'FROM users u1, users u2 ' +
    'WHERE u1.id < u2.id ' +
    'ORDER BY CAST((SELECT COUNT(*) FROM purchases p1 ' +
    '  JOIN purchases p2 ON p1.product_id = p2.product_id ' +
    '  WHERE p1.user_id = u1.id AND p2.user_id = u2.id) AS REAL) / ' +
    '  (SELECT COUNT(DISTINCT product_id) FROM purchases ' +
    '   WHERE user_id = u1.id OR user_id = u2.id) DESC ' +
    'LIMIT 10');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('union_size').AsInteger > 0 then
        WriteLn(Format('   %-10s %-10s %3d     %3d     %.4f', [
          DS.FieldByName('user_a').AsString,
          DS.FieldByName('user_b').AsString,
          DS.FieldByName('common').AsInteger,
          DS.FieldByName('union_size').AsInteger,
          DS.FieldByName('common').AsFloat / DS.FieldByName('union_size').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows item co-occurrence by counting how many users bought each pair of products together. }
procedure Demo4_ItemSimilarity;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Item Co-occurrence (Item Similarity) ===');
  WriteLn;
  WriteLn('   Item A                Item B                Co-buyers  Score');
  WriteLn('   ' + StringOfChar('-', 70));

  // Items frequently bought together (co-occurrence)
  DS := Conn.ExecuteQuery(
    'SELECT p1.name as item_a, p2.name as item_b, ' +
    '  COUNT(*) as co_buyers, ' +
    '  ROUND(CAST(COUNT(*) AS REAL) / ' +
    '    (SELECT COUNT(DISTINCT user_id) FROM purchases WHERE product_id = pr1.product_id), 4) as score ' +
    'FROM purchases pr1 ' +
    'JOIN purchases pr2 ON pr1.user_id = pr2.user_id AND pr1.product_id < pr2.product_id ' +
    'JOIN products p1 ON p1.id = pr1.product_id ' +
    'JOIN products p2 ON p2.id = pr2.product_id ' +
    'GROUP BY pr1.product_id, pr2.product_id ' +
    'HAVING co_buyers >= 2 ' +
    'ORDER BY co_buyers DESC, score DESC ' +
    'LIMIT 12');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-22s %3d        %.4f', [
        DS.FieldByName('item_a').AsString,
        DS.FieldByName('item_b').AsString,
        DS.FieldByName('co_buyers').AsInteger,
        DS.FieldByName('score').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Finds users similar to Alice (sharing 2+ products) and recommends items they bought that Alice has not. }
procedure Demo5_UserBasedRecommendations;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. User-Based Recommendations for Alice ===');
  WriteLn;
  WriteLn('   Finding items bought by similar users that Alice hasn''t bought...');
  WriteLn;

  // Find similar users (share at least 2 products with Alice), then recommend their items
  DS := Conn.ExecuteQuery(
    'SELECT p.name as product, p.category, ' +
    '  COUNT(DISTINCT pr.user_id) as recommenders, ' +
    '  ROUND(AVG(pr.rating), 1) as avg_rating, ' +
    '  GROUP_CONCAT(DISTINCT u.name) as recommended_by ' +
    'FROM purchases pr ' +
    'JOIN products p ON p.id = pr.product_id ' +
    'JOIN users u ON u.id = pr.user_id ' +
    'WHERE pr.user_id IN (' +
    '  SELECT p2.user_id FROM purchases p1 ' +
    '  JOIN purchases p2 ON p1.product_id = p2.product_id AND p2.user_id != 1 ' +
    '  WHERE p1.user_id = 1 ' +
    '  GROUP BY p2.user_id HAVING COUNT(*) >= 2' +
    ') ' +
    'AND pr.product_id NOT IN (SELECT product_id FROM purchases WHERE user_id = 1) ' +
    'GROUP BY pr.product_id ' +
    'ORDER BY recommenders DESC, avg_rating DESC');
  try
    WriteLn('   Product              Category     Recommenders  Avg Rating  By');
    WriteLn('   ' + StringOfChar('-', 78));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-12s %5d         %.1f         %s', [
        DS.FieldByName('product').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('recommenders').AsInteger,
        DS.FieldByName('avg_rating').AsFloat,
        DS.FieldByName('recommended_by').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows "frequently bought together" recommendations for Python Cookbook and Jazz Collection based on co-purchase patterns. }
procedure Demo6_ItemBasedRecommendations;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Item-Based: "Frequently Bought Together" ===');
  WriteLn;
  WriteLn('   If you bought "Python Cookbook", you might also like:');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT p2.name as recommended, p2.category, ' +
    '  COUNT(*) as co_buyers, ' +
    '  ROUND(AVG(pr2.rating), 1) as avg_rating ' +
    'FROM purchases pr1 ' +
    'JOIN purchases pr2 ON pr1.user_id = pr2.user_id ' +
    '  AND pr1.product_id != pr2.product_id ' +
    'JOIN products p2 ON p2.id = pr2.product_id ' +
    'WHERE pr1.product_id = 1 ' +
    'GROUP BY pr2.product_id ' +
    'ORDER BY co_buyers DESC, avg_rating DESC');
  try
    WriteLn('   Product              Category     Co-buyers  Avg Rating');
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-12s %5d      %.1f', [
        DS.FieldByName('recommended').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('co_buyers').AsInteger,
        DS.FieldByName('avg_rating').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   If you bought "Jazz Collection", you might also like:');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT p2.name as recommended, p2.category, ' +
    '  COUNT(*) as co_buyers, ' +
    '  ROUND(AVG(pr2.rating), 1) as avg_rating ' +
    'FROM purchases pr1 ' +
    'JOIN purchases pr2 ON pr1.user_id = pr2.user_id ' +
    '  AND pr1.product_id != pr2.product_id ' +
    'JOIN products p2 ON p2.id = pr2.product_id ' +
    'WHERE pr1.product_id = 9 ' +
    'GROUP BY pr2.product_id ' +
    'ORDER BY co_buyers DESC, avg_rating DESC');
  try
    WriteLn('   Product              Category     Co-buyers  Avg Rating');
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-12s %5d      %.1f', [
        DS.FieldByName('recommended').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('co_buyers').AsInteger,
        DS.FieldByName('avg_rating').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays "users who bought X also bought Y" associations with frequency and percentage of buyers for selected products. }
procedure Demo7_AlsoBought;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. "Users Who Bought X Also Bought Y" ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT p1.name as bought, p2.name as also_bought, ' +
    '  COUNT(*) as frequency, ' +
    '  ROUND(100.0 * COUNT(*) / ' +
    '    (SELECT COUNT(*) FROM purchases WHERE product_id = pr1.product_id), 1) as pct ' +
    'FROM purchases pr1 ' +
    'JOIN purchases pr2 ON pr1.user_id = pr2.user_id ' +
    '  AND pr1.product_id != pr2.product_id ' +
    'JOIN products p1 ON p1.id = pr1.product_id ' +
    'JOIN products p2 ON p2.id = pr2.product_id ' +
    'WHERE pr1.product_id IN (1, 6, 9, 12) ' +
    'GROUP BY pr1.product_id, pr2.product_id ' +
    'HAVING frequency >= 2 ' +
    'ORDER BY pr1.product_id, pct DESC');
  try
    WriteLn('   Bought               Also Bought            Freq   % of Buyers');
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-22s %3d    %.1f%%', [
        DS.FieldByName('bought').AsString,
        DS.FieldByName('also_bought').AsString,
        DS.FieldByName('frequency').AsInteger,
        DS.FieldByName('pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Generates top-5 weighted recommendations for Dave based on similar users and their ratings. }
procedure Demo8_TopNForUser;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Top-N Recommendations for Dave ===');
  WriteLn;
  WriteLn('   Dave bought: Wireless Mouse, Mech Keyboard, USB Hub, Monitor Stand, Headphones');
  WriteLn('   (electronics enthusiast)');
  WriteLn;

  // Weighted recommendation: co-occurrence * avg_rating
  DS := Conn.ExecuteQuery(
    'SELECT p2.name as recommended, p2.category, p2.price, ' +
    '  COUNT(*) as support, ' +
    '  ROUND(AVG(pr2.rating), 1) as avg_rating, ' +
    '  ROUND(COUNT(*) * AVG(pr2.rating), 2) as score ' +
    'FROM purchases pr1 ' +
    'JOIN purchases pr2 ON pr1.user_id = pr2.user_id ' +
    '  AND pr1.product_id != pr2.product_id ' +
    'JOIN products p2 ON p2.id = pr2.product_id ' +
    'WHERE pr1.user_id IN (' +
    '  SELECT DISTINCT p2x.user_id FROM purchases p1x ' +
    '  JOIN purchases p2x ON p1x.product_id = p2x.product_id ' +
    '    AND p2x.user_id != 4 ' +
    '  WHERE p1x.user_id = 4 ' +
    ') ' +
    'AND pr2.product_id NOT IN (SELECT product_id FROM purchases WHERE user_id = 4) ' +
    'GROUP BY pr2.product_id ' +
    'ORDER BY score DESC ' +
    'LIMIT 5');
  try
    WriteLn('   Rank  Product              Category     Price    Score  Reason');
    WriteLn('   ' + StringOfChar('-', 72));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-5s %-22s %-12s $%.2f  %.2f   %d similar users rated %.1f', [
        '',
        DS.FieldByName('recommended').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('price').AsFloat,
        DS.FieldByName('score').AsFloat,
        DS.FieldByName('support').AsInteger,
        DS.FieldByName('avg_rating').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows three cold-start strategies for Iris (1 purchase): item-based co-occurrence, popular items fallback, and same-category recommendations. }
procedure Demo9_ColdStart;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Cold Start Problem (new user: Iris) ===');
  WriteLn;
  WriteLn('   Iris has only 1 purchase: Python Cookbook (rating 5)');
  WriteLn;

  // With only 1 purchase, recommendations are limited
  WriteLn('   Strategy 1: Item-based (from her single purchase):');
  DS := Conn.ExecuteQuery(
    'SELECT p2.name as recommended, p2.category, ' +
    '  COUNT(*) as support, ROUND(AVG(pr2.rating), 1) as avg_rating ' +
    'FROM purchases pr1 ' +
    'JOIN purchases pr2 ON pr1.user_id = pr2.user_id ' +
    '  AND pr1.product_id != pr2.product_id ' +
    'JOIN products p2 ON p2.id = pr2.product_id ' +
    'WHERE pr1.product_id = 1 ' +
    '  AND pr2.product_id NOT IN (SELECT product_id FROM purchases WHERE user_id = 9) ' +
    'GROUP BY pr2.product_id ' +
    'ORDER BY support DESC, avg_rating DESC LIMIT 3');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%s) - %d co-buyers, avg %.1f', [
        DS.FieldByName('recommended').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('support').AsInteger,
        DS.FieldByName('avg_rating').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Strategy 2: Popular items (fallback for cold start):');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, p.category, COUNT(*) as buyers, ROUND(AVG(pr.rating), 1) as avg_r ' +
    'FROM purchases pr JOIN products p ON p.id = pr.product_id ' +
    'WHERE pr.product_id NOT IN (SELECT product_id FROM purchases WHERE user_id = 9) ' +
    'GROUP BY pr.product_id ' +
    'ORDER BY buyers DESC, avg_r DESC LIMIT 3');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%s) - %d buyers, avg %.1f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('buyers').AsInteger,
        DS.FieldByName('avg_r').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Strategy 3: Category-based (same category as purchase):');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, p.category, COUNT(*) as buyers, ROUND(AVG(pr.rating), 1) as avg_r ' +
    'FROM purchases pr JOIN products p ON p.id = pr.product_id ' +
    'WHERE p.category = ''books'' ' +
    '  AND pr.product_id NOT IN (SELECT product_id FROM purchases WHERE user_id = 9) ' +
    'GROUP BY pr.product_id ' +
    'ORDER BY buyers DESC LIMIT 3');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%s) - %d buyers, avg %.1f', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('buyers').AsInteger,
        DS.FieldByName('avg_r').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays recommendation quality metrics: item coverage, matrix sparsity, items per user stats, category preferences by age group, and rating distribution. }
procedure Demo10_QualityMetrics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Recommendation Quality Metrics ===');
  WriteLn;

  // Coverage: what percentage of items can be recommended
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT pr2.product_id) as recommendable, ' +
    '  (SELECT COUNT(*) FROM products) as total ' +
    'FROM purchases pr1 ' +
    'JOIN purchases pr2 ON pr1.user_id = pr2.user_id ' +
    '  AND pr1.product_id != pr2.product_id');
  try
    DS.First;
    WriteLn(Format('   Item coverage: %d/%d products can be recommended (%.0f%%)', [
      DS.FieldByName('recommendable').AsInteger,
      DS.FieldByName('total').AsInteger,
      100.0 * DS.FieldByName('recommendable').AsInteger / DS.FieldByName('total').AsInteger]));
  finally
    DS.Free;
  end;

  // Sparsity of the user-item matrix
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as filled, ' +
    '  (SELECT COUNT(*) FROM users) * (SELECT COUNT(*) FROM products) as total ' +
    'FROM purchases');
  try
    DS.First;
    WriteLn(Format('   Matrix density: %d/%d cells filled (%.1f%% dense, %.1f%% sparse)', [
      DS.FieldByName('filled').AsInteger,
      DS.FieldByName('total').AsInteger,
      100.0 * DS.FieldByName('filled').AsInteger / DS.FieldByName('total').AsInteger,
      100.0 - 100.0 * DS.FieldByName('filled').AsInteger / DS.FieldByName('total').AsInteger]));
  finally
    DS.Free;
  end;

  // Average items per user
  DS := Conn.ExecuteQuery(
    'SELECT ROUND(AVG(cnt), 1) as avg_items, MIN(cnt) as min_items, MAX(cnt) as max_items ' +
    'FROM (SELECT COUNT(*) as cnt FROM purchases GROUP BY user_id)');
  try
    DS.First;
    WriteLn(Format('   Items per user: avg=%.1f, min=%d, max=%d', [
      DS.FieldByName('avg_items').AsFloat,
      DS.FieldByName('min_items').AsInteger,
      DS.FieldByName('max_items').AsInteger]));
  finally
    DS.Free;
  end;

  // Category distribution of recommendations
  WriteLn;
  WriteLn('   Category preference by user segment:');
  DS := Conn.ExecuteQuery(
    'SELECT u.age_group, p.category, COUNT(*) as purchases, ' +
    '  ROUND(AVG(pr.rating), 1) as avg_rating ' +
    'FROM purchases pr ' +
    'JOIN users u ON u.id = pr.user_id ' +
    'JOIN products p ON p.id = pr.product_id ' +
    'GROUP BY u.age_group, p.category ' +
    'ORDER BY u.age_group, purchases DESC');
  try
    WriteLn('   Age Group   Category      Purchases  Avg Rating');
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %-14s %5d      %.1f', [
        DS.FieldByName('age_group').AsString,
        DS.FieldByName('category').AsString,
        DS.FieldByName('purchases').AsInteger,
        DS.FieldByName('avg_rating').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Rating distribution
  WriteLn;
  WriteLn('   Rating distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT rating, COUNT(*) as cnt, ' +
    '  CAST(COUNT(*) * 2 AS INTEGER) as bar_len ' +
    'FROM purchases GROUP BY rating ORDER BY rating');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %d stars: %s (%d)', [
        DS.FieldByName('rating').AsInteger,
        StringOfChar('#', DS.FieldByName('bar_len').AsInteger),
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

begin
  WriteLn('Example 140: Recommendation Engine - Collaborative Filtering');
  WriteLn(StringOfChar('=', 70));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_DataOverview;
    Demo2_UserItemMatrix;
    Demo3_UserSimilarity;
    Demo4_ItemSimilarity;
    Demo5_UserBasedRecommendations;
    Demo6_ItemBasedRecommendations;
    Demo7_AlsoBought;
    Demo8_TopNForUser;
    Demo9_ColdStart;
    Demo10_QualityMetrics;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
