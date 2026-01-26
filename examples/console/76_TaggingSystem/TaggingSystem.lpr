{===============================================================================
  NDXSQLite Example 76 - Flexible Tagging System (Folksonomy)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  A comprehensive tagging system demonstrating:
  - Tag creation and normalization
  - Content tagging with many-to-many relationships
  - Tag cloud generation with popularity weighting
  - Related tags and tag suggestions
  - Tag search and filtering
  - Tag synonyms and hierarchies
  - Tag-based content discovery
  - User-specific tagging (folksonomies)

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program TaggingSystem;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Variants, StrUtils,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Tagging System Schema');
  WriteLn('   ================================');

  // Tags table - normalized tags
  Conn.ExecuteNonQuery(
    'CREATE TABLE tags (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT UNIQUE NOT NULL,' +       // normalized lowercase
    '  display_name TEXT NOT NULL,' +      // original case preserved
    '  slug TEXT UNIQUE NOT NULL,' +       // URL-friendly version
    '  description TEXT,' +
    '  color TEXT DEFAULT ''#808080'',' +  // for UI display
    '  is_featured INTEGER DEFAULT 0,' +
    '  usage_count INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Tag synonyms (for tag suggestions)
  Conn.ExecuteNonQuery(
    'CREATE TABLE tag_synonyms (' +
    '  id INTEGER PRIMARY KEY,' +
    '  tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  synonym TEXT NOT NULL,' +
    '  UNIQUE(tag_id, synonym)' +
    ')');

  // Tag hierarchy (parent-child relationships)
  Conn.ExecuteNonQuery(
    'CREATE TABLE tag_hierarchy (' +
    '  id INTEGER PRIMARY KEY,' +
    '  parent_tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  child_tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  UNIQUE(parent_tag_id, child_tag_id)' +
    ')');

  // Content types (articles, images, videos, etc.)
  Conn.ExecuteNonQuery(
    'CREATE TABLE content_types (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT UNIQUE NOT NULL,' +
    '  description TEXT' +
    ')');

  // Content items
  Conn.ExecuteNonQuery(
    'CREATE TABLE content (' +
    '  id INTEGER PRIMARY KEY,' +
    '  content_type_id INTEGER NOT NULL REFERENCES content_types(id),' +
    '  title TEXT NOT NULL,' +
    '  description TEXT,' +
    '  author TEXT,' +
    '  url TEXT,' +
    '  is_published INTEGER DEFAULT 1,' +
    '  view_count INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  published_at TEXT' +
    ')');

  // Content-Tag junction table (the folksonomy)
  Conn.ExecuteNonQuery(
    'CREATE TABLE content_tags (' +
    '  id INTEGER PRIMARY KEY,' +
    '  content_id INTEGER NOT NULL REFERENCES content(id) ON DELETE CASCADE,' +
    '  tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  tagged_by TEXT,' +                  // user who added the tag
    '  relevance REAL DEFAULT 1.0,' +      // tag relevance score
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(content_id, tag_id)' +
    ')');

  // Related tags (co-occurrence tracking)
  Conn.ExecuteNonQuery(
    'CREATE TABLE related_tags (' +
    '  id INTEGER PRIMARY KEY,' +
    '  tag1_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  tag2_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  co_occurrence_count INTEGER DEFAULT 1,' +
    '  UNIQUE(tag1_id, tag2_id)' +
    ')');

  // User tag preferences (for personalized suggestions)
  Conn.ExecuteNonQuery(
    'CREATE TABLE user_tag_preferences (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_name TEXT NOT NULL,' +
    '  tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  preference_score REAL DEFAULT 1.0,' +
    '  last_used TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(user_name, tag_id)' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_tags_name ON tags(name)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_tags_slug ON tags(slug)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_tags_usage ON tags(usage_count DESC)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_content_tags_content ON content_tags(content_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_content_tags_tag ON content_tags(tag_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_related_tags ON related_tags(tag1_id, tag2_id)');

  WriteLn('   Created tables: tags, tag_synonyms, tag_hierarchy,');
  WriteLn('                   content_types, content, content_tags,');
  WriteLn('                   related_tags, user_tag_preferences');
  WriteLn('');
end;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns the given tag name trimmed, lowercased, and with consecutive spaces collapsed to a single space. }
function NormalizeTagName(const Name: string): string;
var
  I: Integer;
begin
  Result := LowerCase(Trim(Name));
  // Replace spaces with single space
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

{ Converts a tag name into a URL-friendly slug by lowercasing, replacing spaces/hyphens/underscores with dashes, stripping non-alphanumeric characters, and removing leading/trailing/consecutive dashes. }
function CreateSlug(const Name: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(Name) do
  begin
    C := Name[I];
    if C in ['a'..'z', 'A'..'Z', '0'..'9'] then
      Result := Result + LowerCase(C)
    else if (C = ' ') or (C = '-') or (C = '_') then
      Result := Result + '-';
  end;
  // Remove consecutive dashes
  while Pos('--', Result) > 0 do
    Result := StringReplace(Result, '--', '-', [rfReplaceAll]);
  // Remove leading/trailing dashes
  while (Length(Result) > 0) and (Result[1] = '-') do
    Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] = '-') do
    Delete(Result, Length(Result), 1);
end;

// =============================================================================
// Tag Management
// =============================================================================
{ Creates a new tag with the given display name, description, and color, returning its ID. If a tag with the same normalized name already exists, returns the existing tag's ID without creating a duplicate. }
function CreateTag(const DisplayName: string; const Description: string = '';
  const Color: string = '#808080'): Integer;
var
  NormName, Slug: string;
  V: Variant;
begin
  Result := 0;
  NormName := NormalizeTagName(DisplayName);
  Slug := CreateSlug(DisplayName);

  // Check if tag already exists
  V := Conn.ExecuteScalar('SELECT id FROM tags WHERE name = ?', [NormName]);
  if not VarIsNull(V) then
  begin
    Result := V;
    Exit;
  end;

  // Create new tag
  Conn.ExecuteNonQuery(
    'INSERT INTO tags (name, display_name, slug, description, color) VALUES (?, ?, ?, ?, ?)',
    [NormName, DisplayName, Slug, Description, Color]);

  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');
end;

{ Returns the ID of the tag matching DisplayName, creating it with default settings if it does not already exist. }
function GetOrCreateTag(const DisplayName: string): Integer;
begin
  Result := CreateTag(DisplayName);
end;

{ Inserts a normalized synonym for the specified tag, ignoring the insert if the synonym already exists. }
procedure AddTagSynonym(TagId: Integer; const Synonym: string);
begin
  Conn.ExecuteNonQuery(
    'INSERT OR IGNORE INTO tag_synonyms (tag_id, synonym) VALUES (?, ?)',
    [TagId, LowerCase(Trim(Synonym))]);
end;

{ Establishes a parent-child relationship between two tags in the tag_hierarchy table, ignoring duplicates. }
procedure SetTagParent(ChildTagId, ParentTagId: Integer);
begin
  Conn.ExecuteNonQuery(
    'INSERT OR IGNORE INTO tag_hierarchy (parent_tag_id, child_tag_id) VALUES (?, ?)',
    [ParentTagId, ChildTagId]);
end;

// =============================================================================
// Content Tagging
// =============================================================================
{ Tags a content item with the specified tag, user, and relevance score. }
procedure TagContent(ContentId, TagId: Integer; const TaggedBy: string = '';
  Relevance: Double = 1.0);
var
  Affected: Integer;
begin
  // Add tag to content
  Conn.ExecuteNonQuery(
    'INSERT OR IGNORE INTO content_tags (content_id, tag_id, tagged_by, relevance) ' +
    'VALUES (?, ?, ?, ?)', [ContentId, TagId, TaggedBy, Relevance]);

  // Update tag usage count
  Affected := Conn.ExecuteNonQuery('UPDATE tags SET usage_count = usage_count + 1, updated_at = datetime(''now'') WHERE id = ?', [TagId]);

  // Update user preference
  if TaggedBy <> '' then
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO user_tag_preferences (user_name, tag_id, preference_score) ' +
      'VALUES (?, ?, 1.0) ' +
      'ON CONFLICT(user_name, tag_id) DO UPDATE SET ' +
      'preference_score = preference_score + 0.5, last_used = datetime(''now'')',
      [TaggedBy, TagId]);
  end;
end;

{ Recalculates tag co-occurrence counts for a content item by incrementing the co_occurrence_count for every pair of tags assigned to the given content. }
procedure UpdateRelatedTags(ContentId: Integer);
var
  DS: TDataSet;
  TagIds: array of Integer;
  I, J: Integer;
begin
  // Get all tags for this content
  DS := Conn.ExecuteQuery(
    'SELECT tag_id FROM content_tags WHERE content_id = ?', [ContentId]);
  try
    SetLength(TagIds, 0);
    while not DS.EOF do
    begin
      SetLength(TagIds, Length(TagIds) + 1);
      TagIds[High(TagIds)] := DS.FieldByName('tag_id').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update co-occurrence for all tag pairs
  for I := 0 to High(TagIds) - 1 do
    for J := I + 1 to High(TagIds) do
    begin
      // Ensure consistent ordering (smaller id first)
      if TagIds[I] < TagIds[J] then
        Conn.ExecuteNonQuery(
          'INSERT INTO related_tags (tag1_id, tag2_id, co_occurrence_count) ' +
          'VALUES (?, ?, 1) ' +
          'ON CONFLICT(tag1_id, tag2_id) DO UPDATE SET co_occurrence_count = co_occurrence_count + 1',
          [TagIds[I], TagIds[J]])
      else
        Conn.ExecuteNonQuery(
          'INSERT INTO related_tags (tag1_id, tag2_id, co_occurrence_count) ' +
          'VALUES (?, ?, 1) ' +
          'ON CONFLICT(tag1_id, tag2_id) DO UPDATE SET co_occurrence_count = co_occurrence_count + 1',
          [TagIds[J], TagIds[I]]);
    end;
end;

{ Tags a content item with multiple tags by name, creating them as needed. }
procedure TagContentWithMultiple(ContentId: Integer; const TagNames: array of string;
  const TaggedBy: string = '');
var
  I, TagId: Integer;
begin
  for I := 0 to High(TagNames) do
  begin
    TagId := GetOrCreateTag(TagNames[I]);
    TagContent(ContentId, TagId, TaggedBy);
  end;
  UpdateRelatedTags(ContentId);
end;

// =============================================================================
// Sample Data
// =============================================================================
{ Populates the database with sample content types, tags with descriptions/colors/synonyms/hierarchy, and 10 content items (articles, images, videos) with multi-tag assignments and user attribution. }
procedure InsertSampleData;
var
  TagTech, TagPython, TagML, TagWeb, TagJS, TagDB, TagSQL: Integer;
  TagPhoto, TagNature, TagTravel, TagFood, TagArt: Integer;
  ArticleTypeId, ImageTypeId, VideoTypeId: Integer;
begin
  WriteLn('2. Creating Sample Data');
  WriteLn('   ======================');

  // Content types
  Conn.ExecuteNonQuery('INSERT INTO content_types (name, description) VALUES (''article'', ''Written articles and blog posts'')');
  Conn.ExecuteNonQuery('INSERT INTO content_types (name, description) VALUES (''image'', ''Photos and graphics'')');
  Conn.ExecuteNonQuery('INSERT INTO content_types (name, description) VALUES (''video'', ''Video content'')');

  ArticleTypeId := 1;
  ImageTypeId := 2;
  VideoTypeId := 3;

  // Create tags with descriptions and colors
  TagTech := CreateTag('Technology', 'All things tech', '#3498db');
  TagPython := CreateTag('Python', 'Python programming language', '#f1c40f');
  TagML := CreateTag('Machine Learning', 'AI and ML topics', '#9b59b6');
  TagWeb := CreateTag('Web Development', 'Web technologies', '#e74c3c');
  TagJS := CreateTag('JavaScript', 'JavaScript language', '#f39c12');
  TagDB := CreateTag('Database', 'Database systems', '#2ecc71');
  TagSQL := CreateTag('SQL', 'SQL language', '#1abc9c');
  TagPhoto := CreateTag('Photography', 'Photo techniques', '#e91e63');
  TagNature := CreateTag('Nature', 'Natural world', '#4caf50');
  TagTravel := CreateTag('Travel', 'Travel and destinations', '#ff9800');
  TagFood := CreateTag('Food', 'Culinary topics', '#795548');
  TagArt := CreateTag('Art', 'Artistic content', '#673ab7');

  WriteLn(Format('   Created %d tags', [12]));

  // Add synonyms
  AddTagSynonym(TagML, 'ai');
  AddTagSynonym(TagML, 'artificial intelligence');
  AddTagSynonym(TagML, 'deep learning');
  AddTagSynonym(TagJS, 'ecmascript');
  AddTagSynonym(TagJS, 'js');
  AddTagSynonym(TagDB, 'databases');
  AddTagSynonym(TagPhoto, 'photos');
  AddTagSynonym(TagPhoto, 'pictures');

  WriteLn('   Added tag synonyms');

  // Set up tag hierarchy
  SetTagParent(TagPython, TagTech);
  SetTagParent(TagML, TagTech);
  SetTagParent(TagWeb, TagTech);
  SetTagParent(TagJS, TagWeb);
  SetTagParent(TagDB, TagTech);
  SetTagParent(TagSQL, TagDB);
  SetTagParent(TagPhoto, TagArt);

  WriteLn('   Created tag hierarchy');

  // Mark featured tags
  Conn.ExecuteNonQuery('UPDATE tags SET is_featured = 1 WHERE id IN (?, ?, ?, ?)',
    [TagTech, TagML, TagPhoto, TagTravel]);

  // Create content items
  // Articles
  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Introduction to Machine Learning'', ''A beginner guide to ML concepts'', ''Alice'', 1500, datetime(''now'', ''-30 days''))',
    [ArticleTypeId]);
  TagContentWithMultiple(1, ['Machine Learning', 'Python', 'Technology'], 'alice');

  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Building REST APIs with Python'', ''Flask and FastAPI tutorial'', ''Bob'', 2300, datetime(''now'', ''-25 days''))',
    [ArticleTypeId]);
  TagContentWithMultiple(2, ['Python', 'Web Development', 'Database'], 'bob');

  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''JavaScript ES2024 Features'', ''New JS features explained'', ''Carol'', 890, datetime(''now'', ''-20 days''))',
    [ArticleTypeId]);
  TagContentWithMultiple(3, ['JavaScript', 'Web Development'], 'carol');

  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''SQL Query Optimization'', ''Tips for faster queries'', ''Bob'', 1800, datetime(''now'', ''-15 days''))',
    [ArticleTypeId]);
  TagContentWithMultiple(4, ['SQL', 'Database', 'Technology'], 'bob');

  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Deep Learning with PyTorch'', ''Neural networks tutorial'', ''Alice'', 3200, datetime(''now'', ''-10 days''))',
    [ArticleTypeId]);
  TagContentWithMultiple(5, ['Machine Learning', 'Python', 'Technology'], 'alice');

  // Images
  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Mountain Sunrise'', ''Beautiful sunrise over the Alps'', ''David'', 5600, datetime(''now'', ''-28 days''))',
    [ImageTypeId]);
  TagContentWithMultiple(6, ['Photography', 'Nature', 'Travel'], 'david');

  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Tokyo Street Food'', ''Yatai food stalls at night'', ''Emma'', 4200, datetime(''now'', ''-22 days''))',
    [ImageTypeId]);
  TagContentWithMultiple(7, ['Photography', 'Food', 'Travel'], 'emma');

  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Abstract Digital Art'', ''Generative art piece'', ''Frank'', 1200, datetime(''now'', ''-18 days''))',
    [ImageTypeId]);
  TagContentWithMultiple(8, ['Art', 'Technology'], 'frank');

  // Videos
  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Wildlife Documentary'', ''African savanna wildlife'', ''Grace'', 8900, datetime(''now'', ''-12 days''))',
    [VideoTypeId]);
  TagContentWithMultiple(9, ['Nature', 'Photography', 'Travel'], 'grace');

  Conn.ExecuteNonQuery(
    'INSERT INTO content (content_type_id, title, description, author, view_count, published_at) ' +
    'VALUES (?, ''Cooking Italian Pasta'', ''Traditional carbonara recipe'', ''Henry'', 6700, datetime(''now'', ''-5 days''))',
    [VideoTypeId]);
  TagContentWithMultiple(10, ['Food', 'Travel'], 'henry');

  WriteLn('   Created 10 content items with tags');
  WriteLn('');
end;

// =============================================================================
// Tag Cloud Generation
// =============================================================================
{ Queries all tags with usage counts, normalizes them to a 1-5 weight scale, and prints a formatted tag cloud table showing name, usage count, visual weight, and featured status. }
procedure DemoTagCloud;
var
  DS: TDataSet;
  MaxCount, MinCount, Count: Integer;
  Weight: Double;
  WeightStr: string;
begin
  WriteLn('3. Tag Cloud Generation');
  WriteLn('   ======================');
  WriteLn('');

  // Get tag statistics
  DS := Conn.ExecuteQuery(
    'SELECT display_name, usage_count, color, is_featured ' +
    'FROM tags WHERE usage_count > 0 ' +
    'ORDER BY usage_count DESC');
  try
    // Find min/max for normalization
    MaxCount := 0;
    MinCount := 999999;
    while not DS.EOF do
    begin
      Count := DS.FieldByName('usage_count').AsInteger;
      if Count > MaxCount then MaxCount := Count;
      if Count < MinCount then MinCount := Count;
      DS.Next;
    end;
    DS.First;

    WriteLn('   Tag Cloud (weight 1-5):');
    WriteLn('   Tag Name                | Uses | Weight | Featured');
    WriteLn('   ------------------------|------|--------|----------');

    while not DS.EOF do
    begin
      Count := DS.FieldByName('usage_count').AsInteger;
      // Normalize to 1-5 scale
      if MaxCount > MinCount then
        Weight := 1 + 4 * (Count - MinCount) / (MaxCount - MinCount)
      else
        Weight := 3;

      // Visual representation of weight
      WeightStr := StringOfChar('*', Round(Weight));

      WriteLn(Format('   %-23s | %4d | %-6s | %s',
        [DS.FieldByName('display_name').AsString,
         Count,
         WeightStr,
         IfThen(DS.FieldByName('is_featured').AsInteger = 1, 'Yes', '')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Tag Hierarchy Display
// =============================================================================
{ Displays the tag parent-child relationships as a table and renders a tree view showing each parent tag with its children grouped underneath. }
procedure DemoTagHierarchy;
var
  DS: TDataSet;
begin
  WriteLn('4. Tag Hierarchy');
  WriteLn('   ===============');
  WriteLn('');

  // Show parent-child relationships
  DS := Conn.ExecuteQuery(
    'SELECT p.display_name AS parent, c.display_name AS child ' +
    'FROM tag_hierarchy th ' +
    'JOIN tags p ON th.parent_tag_id = p.id ' +
    'JOIN tags c ON th.child_tag_id = c.id ' +
    'ORDER BY p.display_name, c.display_name');
  try
    WriteLn('   Parent Tag      | Child Tag');
    WriteLn('   ----------------|------------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s | %s',
        [DS.FieldByName('parent').AsString,
         DS.FieldByName('child').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show full hierarchy tree
  WriteLn('');
  WriteLn('   Hierarchy Tree:');
  DS := Conn.ExecuteQuery(
    'SELECT t.display_name, ' +
    '  (SELECT GROUP_CONCAT(c.display_name, '', '') FROM tag_hierarchy th ' +
    '   JOIN tags c ON th.child_tag_id = c.id WHERE th.parent_tag_id = t.id) AS children ' +
    'FROM tags t ' +
    'WHERE EXISTS (SELECT 1 FROM tag_hierarchy WHERE parent_tag_id = t.id) ' +
    'ORDER BY t.display_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s', [DS.FieldByName('display_name').AsString]));
      WriteLn(Format('     -> %s', [DS.FieldByName('children').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Content by Tag
// =============================================================================
{ Queries and displays content items filtered by a single tag ("Python") and by an intersection of two tags ("Travel" AND "Photography"), showing title, type, author, and view counts. }
procedure DemoContentByTag;
var
  DS: TDataSet;
begin
  WriteLn('5. Content Discovery by Tag');
  WriteLn('   ==========================');
  WriteLn('');

  // Find content by tag "Python"
  WriteLn('   Content tagged with "Python":');
  DS := Conn.ExecuteQuery(
    'SELECT c.title, ct.name AS type, c.author, c.view_count ' +
    'FROM content c ' +
    'JOIN content_types ct ON c.content_type_id = ct.id ' +
    'JOIN content_tags cta ON c.id = cta.content_id ' +
    'JOIN tags t ON cta.tag_id = t.id ' +
    'WHERE t.name = ''python'' ' +
    'ORDER BY c.view_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s by %s (%d views)',
        [DS.FieldByName('type').AsString,
         DS.FieldByName('title').AsString,
         DS.FieldByName('author').AsString,
         DS.FieldByName('view_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Find content by multiple tags
  WriteLn('');
  WriteLn('   Content tagged with both "Travel" AND "Photography":');
  DS := Conn.ExecuteQuery(
    'SELECT c.title, ct.name AS type, c.view_count ' +
    'FROM content c ' +
    'JOIN content_types ct ON c.content_type_id = ct.id ' +
    'WHERE c.id IN (' +
    '  SELECT content_id FROM content_tags WHERE tag_id = (SELECT id FROM tags WHERE name = ''travel'')' +
    '  INTERSECT ' +
    '  SELECT content_id FROM content_tags WHERE tag_id = (SELECT id FROM tags WHERE name = ''photography'')' +
    ') ORDER BY c.view_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s (%d views)',
        [DS.FieldByName('type').AsString,
         DS.FieldByName('title').AsString,
         DS.FieldByName('view_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Related Tags
// =============================================================================
{ Displays the top co-occurring tag pairs ranked by frequency, then shows tags most frequently used together with "Machine Learning". }
procedure DemoRelatedTags;
var
  DS: TDataSet;
begin
  WriteLn('6. Related Tags');
  WriteLn('   ==============');
  WriteLn('');

  // Show related tags based on co-occurrence
  WriteLn('   Tags frequently used together:');
  DS := Conn.ExecuteQuery(
    'SELECT t1.display_name AS tag1, t2.display_name AS tag2, rt.co_occurrence_count ' +
    'FROM related_tags rt ' +
    'JOIN tags t1 ON rt.tag1_id = t1.id ' +
    'JOIN tags t2 ON rt.tag2_id = t2.id ' +
    'ORDER BY rt.co_occurrence_count DESC ' +
    'LIMIT 10');
  try
    WriteLn('   Tag 1            | Tag 2            | Co-occurs');
    WriteLn('   -----------------|------------------|----------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s | %-16s | %d times',
        [DS.FieldByName('tag1').AsString,
         DS.FieldByName('tag2').AsString,
         DS.FieldByName('co_occurrence_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Find related tags for a specific tag
  WriteLn('');
  WriteLn('   Tags related to "Machine Learning":');
  DS := Conn.ExecuteQuery(
    'SELECT t.display_name, rt.co_occurrence_count ' +
    'FROM related_tags rt ' +
    'JOIN tags t ON (CASE ' +
    '  WHEN rt.tag1_id = (SELECT id FROM tags WHERE name = ''machine learning'') THEN rt.tag2_id ' +
    '  ELSE rt.tag1_id END) = t.id ' +
    'WHERE rt.tag1_id = (SELECT id FROM tags WHERE name = ''machine learning'') ' +
    '   OR rt.tag2_id = (SELECT id FROM tags WHERE name = ''machine learning'') ' +
    'ORDER BY rt.co_occurrence_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     - %s (co-occurs %d times)',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('co_occurrence_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Tag Suggestions
// =============================================================================
{ Suggests additional tags based on co-occurrence with "Python" and "Web Development", and performs a synonym-aware tag search matching the term "ai". }
procedure DemoTagSuggestions;
var
  DS: TDataSet;
begin
  WriteLn('7. Tag Suggestions');
  WriteLn('   =================');
  WriteLn('');

  // Suggest tags based on existing content tags
  WriteLn('   If you used "Python" and "Web Development", you might also use:');
  DS := Conn.ExecuteQuery(
    'SELECT DISTINCT t.display_name, t.usage_count ' +
    'FROM tags t ' +
    'JOIN content_tags ct ON t.id = ct.tag_id ' +
    'WHERE ct.content_id IN (' +
    '  SELECT content_id FROM content_tags ' +
    '  WHERE tag_id IN (SELECT id FROM tags WHERE name IN (''python'', ''web development''))' +
    ') AND t.name NOT IN (''python'', ''web development'') ' +
    'ORDER BY t.usage_count DESC ' +
    'LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     - %s (used %d times)',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('usage_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Suggest tags from synonyms
  WriteLn('');
  WriteLn('   Tag search with synonym matching (searching "ai"):');
  DS := Conn.ExecuteQuery(
    'SELECT t.display_name, t.description ' +
    'FROM tags t ' +
    'LEFT JOIN tag_synonyms ts ON t.id = ts.tag_id ' +
    'WHERE t.name LIKE ''%ai%'' OR ts.synonym LIKE ''%ai%'' ' +
    'GROUP BY t.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     - %s: %s',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// User Tag Preferences
// =============================================================================
{ Displays per-user tagging activity (unique tags and total assignments) and shows personalized tag recommendations for user "Alice" ranked by preference score. }
procedure DemoUserPreferences;
var
  DS: TDataSet;
begin
  WriteLn('8. User Tag Preferences');
  WriteLn('   ======================');
  WriteLn('');

  // Show user tag usage patterns
  WriteLn('   Most active taggers:');
  DS := Conn.ExecuteQuery(
    'SELECT tagged_by, COUNT(DISTINCT tag_id) AS unique_tags, COUNT(*) AS total_tags ' +
    'FROM content_tags ' +
    'WHERE tagged_by IS NOT NULL AND tagged_by != '''' ' +
    'GROUP BY tagged_by ' +
    'ORDER BY total_tags DESC');
  try
    WriteLn('   User    | Unique Tags | Total Uses');
    WriteLn('   --------|-------------|------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-7s | %11d | %d',
        [DS.FieldByName('tagged_by').AsString,
         DS.FieldByName('unique_tags').AsInteger,
         DS.FieldByName('total_tags').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show personalized tag suggestions for a user
  WriteLn('');
  WriteLn('   Recommended tags for Alice (based on usage):');
  DS := Conn.ExecuteQuery(
    'SELECT t.display_name, utp.preference_score ' +
    'FROM user_tag_preferences utp ' +
    'JOIN tags t ON utp.tag_id = t.id ' +
    'WHERE utp.user_name = ''alice'' ' +
    'ORDER BY utp.preference_score DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     - %s (score: %.1f)',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('preference_score').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Tag Statistics
// =============================================================================
{ Prints overall tagging metrics (total tags, content items, assignments), per-content-type tag distribution with averages, and the most-viewed content item for each tag. }
procedure DemoTagStatistics;
var
  DS: TDataSet;
begin
  WriteLn('9. Tag Statistics');
  WriteLn('   ================');
  WriteLn('');

  // Overall statistics
  WriteLn('   Overview:');
  WriteLn(Format('     Total tags: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM tags'))]));
  WriteLn(Format('     Tags with content: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(DISTINCT tag_id) FROM content_tags'))]));
  WriteLn(Format('     Total content items: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM content'))]));
  WriteLn(Format('     Total tag assignments: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM content_tags'))]));

  // Tags per content type
  WriteLn('');
  WriteLn('   Tags per content type:');
  DS := Conn.ExecuteQuery(
    'SELECT ct.name, COUNT(DISTINCT cta.tag_id) AS tag_count, ' +
    '  ROUND(AVG(tag_per_content.cnt), 1) AS avg_tags_per_item ' +
    'FROM content_types ct ' +
    'JOIN content c ON ct.id = c.content_type_id ' +
    'JOIN content_tags cta ON c.id = cta.content_id ' +
    'JOIN (SELECT content_id, COUNT(*) AS cnt FROM content_tags GROUP BY content_id) tag_per_content ' +
    '  ON c.id = tag_per_content.content_id ' +
    'GROUP BY ct.id');
  try
    WriteLn('   Type    | Unique Tags | Avg per Item');
    WriteLn('   --------|-------------|-------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-7s | %11d | %.1f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('tag_count').AsInteger,
         DS.FieldByName('avg_tags_per_item').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Most viewed content per tag
  WriteLn('');
  WriteLn('   Top content by tag (most views):');
  DS := Conn.ExecuteQuery(
    'SELECT t.display_name AS tag, c.title, c.view_count ' +
    'FROM content c ' +
    'JOIN content_tags ct ON c.id = ct.content_id ' +
    'JOIN tags t ON ct.tag_id = t.id ' +
    'WHERE c.view_count = (' +
    '  SELECT MAX(c2.view_count) FROM content c2 ' +
    '  JOIN content_tags ct2 ON c2.id = ct2.content_id ' +
    '  WHERE ct2.tag_id = ct.tag_id) ' +
    'ORDER BY c.view_count DESC ' +
    'LIMIT 8');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: "%s" (%d views)',
        [DS.FieldByName('tag').AsString,
         DS.FieldByName('title').AsString,
         DS.FieldByName('view_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 76: Flexible Tagging System ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;
    DemoTagCloud;
    DemoTagHierarchy;
    DemoContentByTag;
    DemoRelatedTags;
    DemoTagSuggestions;
    DemoUserPreferences;
    DemoTagStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
