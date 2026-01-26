{===============================================================================
  NDXSQLite Example 144 - CMS
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Page management with draft/published states
  - Publishing workflow and approval
  - Revision history and versioning
  - Slug-based URL routing
  - Content hierarchy and categorization

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program CMS;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Pages (current state)
  Conn.ExecuteNonQuery(
    'CREATE TABLE pages (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  slug TEXT NOT NULL UNIQUE, ' +
    '  title TEXT NOT NULL, ' +
    '  body TEXT, ' +
    '  content_type TEXT NOT NULL DEFAULT ''page'', ' +
    '  parent_id INTEGER, ' +
    '  status TEXT NOT NULL DEFAULT ''draft'', ' +
    '  author TEXT NOT NULL, ' +
    '  current_revision INTEGER NOT NULL DEFAULT 1, ' +
    '  created_at TEXT NOT NULL, ' +
    '  updated_at TEXT NOT NULL, ' +
    '  published_at TEXT)');

  // Revision history
  Conn.ExecuteNonQuery(
    'CREATE TABLE revisions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  page_id INTEGER NOT NULL, ' +
    '  revision_num INTEGER NOT NULL, ' +
    '  title TEXT NOT NULL, ' +
    '  body TEXT, ' +
    '  slug TEXT NOT NULL, ' +
    '  author TEXT NOT NULL, ' +
    '  status TEXT NOT NULL, ' +
    '  change_note TEXT, ' +
    '  created_at TEXT NOT NULL, ' +
    '  UNIQUE(page_id, revision_num))');

  // Workflow transitions
  Conn.ExecuteNonQuery(
    'CREATE TABLE workflow_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  page_id INTEGER NOT NULL, ' +
    '  from_status TEXT NOT NULL, ' +
    '  to_status TEXT NOT NULL, ' +
    '  actor TEXT NOT NULL, ' +
    '  comment TEXT, ' +
    '  timestamp TEXT NOT NULL)');

  // Categories
  Conn.ExecuteNonQuery(
    'CREATE TABLE categories (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  name TEXT NOT NULL, ' +
    '  slug TEXT NOT NULL UNIQUE, ' +
    '  parent_id INTEGER)');

  // Page-category mapping
  Conn.ExecuteNonQuery(
    'CREATE TABLE page_categories (' +
    '  page_id INTEGER NOT NULL, ' +
    '  category_id INTEGER NOT NULL, ' +
    '  PRIMARY KEY(page_id, category_id))');

  // Page metadata (SEO, etc.)
  Conn.ExecuteNonQuery(
    'CREATE TABLE page_meta (' +
    '  page_id INTEGER NOT NULL, ' +
    '  meta_key TEXT NOT NULL, ' +
    '  meta_value TEXT, ' +
    '  PRIMARY KEY(page_id, meta_key))');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Categories
  Conn.ExecuteNonQuery('INSERT INTO categories (name, slug, parent_id) VALUES (''Technology'', ''technology'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO categories (name, slug, parent_id) VALUES (''Programming'', ''programming'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO categories (name, slug, parent_id) VALUES (''Databases'', ''databases'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO categories (name, slug, parent_id) VALUES (''Tutorials'', ''tutorials'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO categories (name, slug, parent_id) VALUES (''News'', ''news'', NULL)');

  // Pages with various statuses
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''home'', ''Welcome to Our Site'', ''This is the homepage content with an introduction to our platform.'', ''page'', NULL, ''published'', ''admin'', 3, ''2025-01-01 09:00:00'', ''2025-01-15 14:00:00'', ''2025-01-15 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''about'', ''About Us'', ''We are a team of developers building great software.'', ''page'', NULL, ''published'', ''admin'', 2, ''2025-01-01 09:30:00'', ''2025-01-10 11:00:00'', ''2025-01-10 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''getting-started'', ''Getting Started Guide'', ''Follow these steps to set up your environment and begin coding.'', ''article'', NULL, ''published'', ''alice'', 4, ''2025-01-05 10:00:00'', ''2025-01-18 09:00:00'', ''2025-01-18 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''sqlite-internals'', ''SQLite Internals Deep Dive'', ''Understanding B-trees, WAL mode, and query optimization in SQLite.'', ''article'', NULL, ''review'', ''bob'', 2, ''2025-01-12 14:00:00'', ''2025-01-19 16:00:00'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''api-reference'', ''API Reference v2'', ''Complete API documentation for version 2.0 of the platform.'', ''docs'', NULL, ''draft'', ''charlie'', 1, ''2025-01-19 08:00:00'', ''2025-01-19 08:00:00'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''release-notes-v2'', ''Release Notes v2.0'', ''New features and breaking changes in version 2.0.'', ''article'', NULL, ''draft'', ''alice'', 1, ''2025-01-20 10:00:00'', ''2025-01-20 10:00:00'', NULL)');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''privacy-policy'', ''Privacy Policy'', ''Our commitment to protecting your data and privacy rights.'', ''page'', NULL, ''published'', ''admin'', 1, ''2025-01-02 10:00:00'', ''2025-01-02 10:00:00'', ''2025-01-02 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''faq'', ''Frequently Asked Questions'', ''Answers to common questions about our services.'', ''page'', 2, ''published'', ''admin'', 2, ''2025-01-03 11:00:00'', ''2025-01-14 15:00:00'', ''2025-01-14 15:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO pages (slug, title, body, content_type, parent_id, status, author, current_revision, created_at, updated_at, published_at) VALUES ' +
    '(''old-announcement'', ''Legacy Announcement'', ''This content is no longer relevant.'', ''article'', NULL, ''archived'', ''admin'', 2, ''2024-06-01 09:00:00'', ''2025-01-05 09:00:00'', ''2024-06-01 10:00:00'')');

  // Revisions for home page
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(1, 1, ''Welcome'', ''Initial homepage content.'', ''home'', ''admin'', ''draft'', ''Initial creation'', ''2025-01-01 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(1, 2, ''Welcome to Our Site'', ''Updated homepage with better introduction.'', ''home'', ''admin'', ''review'', ''Improved intro text'', ''2025-01-10 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(1, 3, ''Welcome to Our Site'', ''This is the homepage content with an introduction to our platform.'', ''home'', ''admin'', ''published'', ''Final version approved'', ''2025-01-15 14:00:00'')');

  // Revisions for about page
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(2, 1, ''About'', ''About our company.'', ''about'', ''admin'', ''draft'', ''Initial draft'', ''2025-01-01 09:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(2, 2, ''About Us'', ''We are a team of developers building great software.'', ''about'', ''admin'', ''published'', ''Published with team info'', ''2025-01-10 11:00:00'')');

  // Revisions for getting-started
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(3, 1, ''Getting Started'', ''Basic setup instructions.'', ''getting-started'', ''alice'', ''draft'', ''First draft'', ''2025-01-05 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(3, 2, ''Getting Started Guide'', ''Updated setup with environment details.'', ''getting-started'', ''alice'', ''review'', ''Added env setup'', ''2025-01-08 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(3, 3, ''Getting Started Guide'', ''Complete guide with screenshots.'', ''getting-started'', ''bob'', ''review'', ''Added screenshots'', ''2025-01-12 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(3, 4, ''Getting Started Guide'', ''Follow these steps to set up your environment and begin coding.'', ''getting-started'', ''alice'', ''published'', ''Final review passed'', ''2025-01-18 09:00:00'')');

  // Revisions for sqlite-internals
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(4, 1, ''SQLite Internals'', ''Draft of SQLite deep dive article.'', ''sqlite-internals'', ''bob'', ''draft'', ''Initial research'', ''2025-01-12 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO revisions (page_id, revision_num, title, body, slug, author, status, change_note, created_at) VALUES ' +
    '(4, 2, ''SQLite Internals Deep Dive'', ''Understanding B-trees, WAL mode, and query optimization in SQLite.'', ''sqlite-internals'', ''bob'', ''review'', ''Ready for review'', ''2025-01-19 16:00:00'')');

  // Workflow log
  Conn.ExecuteNonQuery('INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(1, ''draft'', ''review'', ''admin'', ''Ready for review'', ''2025-01-10 12:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(1, ''review'', ''published'', ''editor'', ''Approved and published'', ''2025-01-15 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(2, ''draft'', ''published'', ''admin'', ''Direct publish'', ''2025-01-10 11:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(3, ''draft'', ''review'', ''alice'', ''Please review setup steps'', ''2025-01-08 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(3, ''review'', ''published'', ''editor'', ''Looks good, publishing'', ''2025-01-18 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(4, ''draft'', ''review'', ''bob'', ''Ready for technical review'', ''2025-01-19 16:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(9, ''published'', ''archived'', ''admin'', ''Content outdated'', ''2025-01-05 09:00:00'')');

  // Page-category mappings
  Conn.ExecuteNonQuery('INSERT INTO page_categories VALUES (3, 1)');  // getting-started -> technology
  Conn.ExecuteNonQuery('INSERT INTO page_categories VALUES (3, 2)');  // getting-started -> programming
  Conn.ExecuteNonQuery('INSERT INTO page_categories VALUES (3, 4)');  // getting-started -> tutorials
  Conn.ExecuteNonQuery('INSERT INTO page_categories VALUES (4, 1)');  // sqlite-internals -> technology
  Conn.ExecuteNonQuery('INSERT INTO page_categories VALUES (4, 3)');  // sqlite-internals -> databases
  Conn.ExecuteNonQuery('INSERT INTO page_categories VALUES (6, 5)');  // release-notes -> news

  // Page metadata
  Conn.ExecuteNonQuery('INSERT INTO page_meta VALUES (1, ''meta_description'', ''Welcome to our platform - the home of great software'')');
  Conn.ExecuteNonQuery('INSERT INTO page_meta VALUES (1, ''meta_keywords'', ''home, welcome, platform'')');
  Conn.ExecuteNonQuery('INSERT INTO page_meta VALUES (3, ''meta_description'', ''Step by step guide to get started with our tools'')');
  Conn.ExecuteNonQuery('INSERT INTO page_meta VALUES (3, ''reading_time'', ''5 min'')');
  Conn.ExecuteNonQuery('INSERT INTO page_meta VALUES (4, ''meta_description'', ''Deep dive into SQLite internals and optimization'')');
  Conn.ExecuteNonQuery('INSERT INTO page_meta VALUES (4, ''reading_time'', ''12 min'')');
  Conn.ExecuteNonQuery('INSERT INTO page_meta VALUES (7, ''noindex'', ''true'')');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Lists all pages with slug, title, type, status, author, revision, and publish date, then summarizes counts by status. }
procedure Demo1_PagesOverview;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Pages Overview ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT slug, title, content_type, status, author, current_revision, ' +
    '  COALESCE(published_at, ''-'') AS pub_date ' +
    'FROM pages ORDER BY id');
  try
    WriteLn(Format('   %-20s %-30s %-8s %-10s %-8s %-4s %s',
      ['Slug', 'Title', 'Type', 'Status', 'Author', 'Rev', 'Published']));
    WriteLn('   ' + StringOfChar('-', 115));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-30s %-8s %-10s %-8s %-4d %s', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('title').AsString,
        DS.FieldByName('content_type').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('author').AsString,
        DS.FieldByName('current_revision').AsInteger,
        DS.FieldByName('pub_date').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT status, COUNT(*) AS cnt FROM pages GROUP BY status ORDER BY cnt DESC');
  try
    Write('   Status summary: ');
    while not DS.EOF do
    begin
      Write(Format('%s=%d ', [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
    WriteLn;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Separates pages into published, draft, and review status groups with their respective URLs, titles, and authors. }
procedure Demo2_DraftPublishedStates;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Draft vs Published States ===');
  WriteLn;

  WriteLn('   Published pages (visible to public):');
  DS := Conn.ExecuteQuery(
    'SELECT slug, title, published_at FROM pages ' +
    'WHERE status = ''published'' ORDER BY published_at DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - /%s -> "%s" (since %s)', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('title').AsString,
        DS.FieldByName('published_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  WriteLn('   Draft pages (not yet visible):');
  DS := Conn.ExecuteQuery(
    'SELECT slug, title, author FROM pages WHERE status = ''draft'' ORDER BY created_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - /%s -> "%s" (by %s)', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('title').AsString,
        DS.FieldByName('author').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  WriteLn('   In review (pending approval):');
  DS := Conn.ExecuteQuery(
    'SELECT slug, title, author FROM pages WHERE status = ''review'' ORDER BY updated_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - /%s -> "%s" (by %s)', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('title').AsString,
        DS.FieldByName('author').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays the workflow transition log showing status changes, actors, and comments, then simulates publishing a review page. }
procedure Demo3_PublishingWorkflow;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Publishing Workflow ===');
  WriteLn;

  WriteLn('   Workflow: draft -> review -> published (or direct publish)');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT w.timestamp, p.slug, w.from_status, w.to_status, w.actor, ' +
    '  COALESCE(w.comment, ''-'') AS comment ' +
    'FROM workflow_log w ' +
    'JOIN pages p ON p.id = w.page_id ' +
    'ORDER BY w.timestamp');
  try
    WriteLn(Format('   %-20s %-20s %-12s %-12s %-8s %s',
      ['Timestamp', 'Page', 'From', 'To', 'Actor', 'Comment']));
    WriteLn('   ' + StringOfChar('-', 100));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-20s %-12s %-12s %-8s %s', [
        DS.FieldByName('timestamp').AsString,
        DS.FieldByName('slug').AsString,
        DS.FieldByName('from_status').AsString,
        DS.FieldByName('to_status').AsString,
        DS.FieldByName('actor').AsString,
        DS.FieldByName('comment').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Simulate publishing the review page
  WriteLn('   Simulating: publish "sqlite-internals" (review -> published)');
  Conn.ExecuteNonQuery(
    'UPDATE pages SET status = ''published'', published_at = ''2025-01-20 11:00:00'', ' +
    '  updated_at = ''2025-01-20 11:00:00'' WHERE slug = ''sqlite-internals''');
  Conn.ExecuteNonQuery(
    'INSERT INTO workflow_log (page_id, from_status, to_status, actor, comment, timestamp) VALUES ' +
    '(4, ''review'', ''published'', ''editor'', ''Technical review passed'', ''2025-01-20 11:00:00'')');
  WriteLn('   Done: sqlite-internals is now published');
  WriteLn;
end;

{ Shows all revisions for a page with title, author, status, and change notes, then compares title evolution between revisions. }
procedure Demo4_RevisionHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Revision History ===');
  WriteLn;

  WriteLn('   Revision history for "getting-started" (4 revisions):');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT revision_num, title, author, status, change_note, created_at ' +
    'FROM revisions WHERE page_id = 3 ORDER BY revision_num');
  try
    WriteLn(Format('   %-4s %-28s %-8s %-10s %-25s %s',
      ['Rev', 'Title', 'Author', 'Status', 'Note', 'Date']));
    WriteLn('   ' + StringOfChar('-', 105));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-4d %-28s %-8s %-10s %-25s %s', [
        DS.FieldByName('revision_num').AsInteger,
        DS.FieldByName('title').AsString,
        DS.FieldByName('author').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('change_note').AsString,
        DS.FieldByName('created_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Show diff between revisions (title/body changes)
  WriteLn('   Changes between revisions (title evolution):');
  DS := Conn.ExecuteQuery(
    'SELECT r1.revision_num AS from_rev, r2.revision_num AS to_rev, ' +
    '  r1.title AS old_title, r2.title AS new_title, ' +
    '  CASE WHEN r1.title != r2.title THEN ''changed'' ELSE ''same'' END AS title_status ' +
    'FROM revisions r1 ' +
    'JOIN revisions r2 ON r2.page_id = r1.page_id AND r2.revision_num = r1.revision_num + 1 ' +
    'WHERE r1.page_id = 3 ORDER BY r1.revision_num');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Rev %d -> %d: title %s ("%s" -> "%s")', [
        DS.FieldByName('from_rev').AsInteger,
        DS.FieldByName('to_rev').AsInteger,
        DS.FieldByName('title_status').AsString,
        DS.FieldByName('old_title').AsString,
        DS.FieldByName('new_title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists published page URLs from slugs, performs a slug lookup returning page content, and checks slug availability. }
procedure Demo5_SlugRouting;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Slug-Based Routing ===');
  WriteLn;

  WriteLn('   URL routing via slug lookup (published pages only):');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT ''/'' || slug AS url, title, content_type, author ' +
    'FROM pages WHERE status = ''published'' ORDER BY slug');
  try
    WriteLn(Format('   %-25s %-35s %-8s %s',
      ['URL', 'Title', 'Type', 'Author']));
    WriteLn('   ' + StringOfChar('-', 85));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s %-35s %-8s %s', [
        DS.FieldByName('url').AsString,
        DS.FieldByName('title').AsString,
        DS.FieldByName('content_type').AsString,
        DS.FieldByName('author').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Simulate slug lookup
  WriteLn('   Slug lookup: /getting-started');
  DS := Conn.ExecuteQuery(
    'SELECT title, body, author, published_at FROM pages ' +
    'WHERE slug = ''getting-started'' AND status = ''published''');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   -> Title: %s', [DS.FieldByName('title').AsString]));
      WriteLn(Format('   -> Body: %s', [DS.FieldByName('body').AsString]));
      WriteLn(Format('   -> Author: %s, Published: %s', [
        DS.FieldByName('author').AsString,
        DS.FieldByName('published_at').AsString]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Slug conflict detection
  WriteLn('   Slug uniqueness check for "about-us":');
  DS := Conn.ExecuteQuery('SELECT COUNT(*) AS cnt FROM pages WHERE slug = ''about-us''');
  try
    if not DS.EOF then
    begin
      if DS.FieldByName('cnt').AsInteger = 0 then
        WriteLn('   -> Slug "about-us" is available')
      else
        WriteLn('   -> Slug "about-us" is taken');
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Builds a parent-child page tree using recursive CTE and generates a breadcrumb path for a child page. }
procedure Demo6_ContentHierarchy;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Content Hierarchy ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE page_tree(id, title, slug, parent_id, depth, path) AS (' +
    '  SELECT id, title, slug, parent_id, 0, slug ' +
    '  FROM pages WHERE parent_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT p.id, p.title, p.slug, p.parent_id, pt.depth + 1, pt.path || ''/'' || p.slug ' +
    '  FROM pages p JOIN page_tree pt ON p.parent_id = pt.id ' +
    ') ' +
    'SELECT depth, title, slug, path FROM page_tree ORDER BY path');
  try
    WriteLn('   Page tree (parent-child hierarchy):');
    WriteLn;
    while not DS.EOF do
    begin
      WriteLn(Format('   %s%s (/%s)', [
        StringOfChar(' ', DS.FieldByName('depth').AsInteger * 3),
        DS.FieldByName('title').AsString,
        DS.FieldByName('path').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Show breadcrumb for child page
  WriteLn('   Breadcrumb for /faq:');
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE breadcrumb(id, title, slug, parent_id, depth) AS (' +
    '  SELECT id, title, slug, parent_id, 0 FROM pages WHERE slug = ''faq'' ' +
    '  UNION ALL ' +
    '  SELECT p.id, p.title, p.slug, p.parent_id, b.depth + 1 ' +
    '  FROM pages p JOIN breadcrumb b ON p.id = b.parent_id ' +
    ') ' +
    'SELECT title, slug FROM breadcrumb ORDER BY depth DESC');
  try
    Write('   ');
    while not DS.EOF do
    begin
      Write(DS.FieldByName('title').AsString);
      DS.Next;
      if not DS.EOF then Write(' > ');
    end;
    WriteLn;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates hierarchical category trees and content tagging. }
procedure Demo7_Categories;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Categories and Tagging ===');
  WriteLn;

  // Category tree
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE cat_tree(id, name, slug, parent_id, depth) AS (' +
    '  SELECT id, name, slug, parent_id, 0 FROM categories WHERE parent_id IS NULL ' +
    '  UNION ALL ' +
    '  SELECT c.id, c.name, c.slug, c.parent_id, ct.depth + 1 ' +
    '  FROM categories c JOIN cat_tree ct ON c.parent_id = ct.id ' +
    ') ' +
    'SELECT depth, name, slug, ' +
    '  (SELECT COUNT(*) FROM page_categories pc WHERE pc.category_id = cat_tree.id) AS page_count ' +
    'FROM cat_tree ORDER BY slug');
  try
    WriteLn('   Category tree:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s%s (/%s) - %d page(s)', [
        StringOfChar(' ', DS.FieldByName('depth').AsInteger * 3),
        DS.FieldByName('name').AsString,
        DS.FieldByName('slug').AsString,
        DS.FieldByName('page_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Pages per category
  WriteLn('   Pages in "Technology" (including subcategories):');
  DS := Conn.ExecuteQuery(
    'WITH RECURSIVE sub_cats(id) AS (' +
    '  SELECT id FROM categories WHERE slug = ''technology'' ' +
    '  UNION ALL ' +
    '  SELECT c.id FROM categories c JOIN sub_cats sc ON c.parent_id = sc.id ' +
    ') ' +
    'SELECT DISTINCT p.slug, p.title, c.name AS category ' +
    'FROM pages p ' +
    'JOIN page_categories pc ON pc.page_id = p.id ' +
    'JOIN categories c ON c.id = pc.category_id ' +
    'WHERE pc.category_id IN (SELECT id FROM sub_cats) ' +
    'ORDER BY p.title');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s [%s]', [
        DS.FieldByName('title').AsString,
        DS.FieldByName('category').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates page metadata storage and retrieval as key-value pairs. }
procedure Demo8_Metadata;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Page Metadata ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT p.slug, m.meta_key, m.meta_value ' +
    'FROM page_meta m JOIN pages p ON p.id = m.page_id ' +
    'ORDER BY p.slug, m.meta_key');
  try
    WriteLn(Format('   %-20s %-20s %s', ['Page', 'Key', 'Value']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %-20s %s', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('meta_key').AsString,
        DS.FieldByName('meta_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // SEO check: pages without meta_description
  WriteLn('   Pages missing meta_description (SEO issue):');
  DS := Conn.ExecuteQuery(
    'SELECT slug, title FROM pages ' +
    'WHERE status = ''published'' AND id NOT IN ' +
    '  (SELECT page_id FROM page_meta WHERE meta_key = ''meta_description'') ' +
    'ORDER BY slug');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - /%s (%s)', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Performs LIKE-based text search in title and body, filters by author, and breaks down content type counts. }
procedure Demo9_ContentSearch;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Content Search ===');
  WriteLn;

  // Full-text style search (LIKE-based)
  WriteLn('   Search for "SQLite" in published/review content:');
  DS := Conn.ExecuteQuery(
    'SELECT slug, title, status, ' +
    '  SUBSTR(body, MAX(1, INSTR(LOWER(body), ''sqlite'') - 10), 50) AS context ' +
    'FROM pages ' +
    'WHERE (LOWER(title) LIKE ''%sqlite%'' OR LOWER(body) LIKE ''%sqlite%'') ' +
    '  AND status IN (''published'', ''review'') ' +
    'ORDER BY status, title');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - [%s] /%s: "%s"', [
        DS.FieldByName('status').AsString,
        DS.FieldByName('slug').AsString,
        DS.FieldByName('context').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Search by author
  WriteLn('   Content by author "alice":');
  DS := Conn.ExecuteQuery(
    'SELECT slug, title, status, content_type FROM pages WHERE author = ''alice'' ORDER BY created_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - /%s [%s, %s]', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('content_type').AsString,
        DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Search by content type
  WriteLn('   Content types breakdown:');
  DS := Conn.ExecuteQuery(
    'SELECT content_type, COUNT(*) AS cnt, ' +
    '  SUM(CASE WHEN status = ''published'' THEN 1 ELSE 0 END) AS published ' +
    'FROM pages GROUP BY content_type ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d total, %d published', [
        DS.FieldByName('content_type').AsString,
        DS.FieldByName('cnt').AsInteger,
        DS.FieldByName('published').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Reports author contributions, revision activity averages, workflow transition counts, and time-to-publish metrics. }
procedure Demo10_PublishingStats;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Publishing Statistics ===');
  WriteLn;

  // Author contributions
  DS := Conn.ExecuteQuery(
    'SELECT author, COUNT(*) AS pages, ' +
    '  SUM(CASE WHEN status = ''published'' THEN 1 ELSE 0 END) AS published, ' +
    '  SUM(current_revision) AS total_revisions ' +
    'FROM pages GROUP BY author ORDER BY pages DESC');
  try
    WriteLn(Format('   %-10s %-6s %-10s %s', ['Author', 'Pages', 'Published', 'Revisions']));
    WriteLn('   ' + StringOfChar('-', 45));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-6d %-10d %d', [
        DS.FieldByName('author').AsString,
        DS.FieldByName('pages').AsInteger,
        DS.FieldByName('published').AsInteger,
        DS.FieldByName('total_revisions').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Revision activity
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) AS total_revisions, ' +
    '  COUNT(DISTINCT page_id) AS pages_with_revisions, ' +
    '  CAST(COUNT(*) AS REAL) / COUNT(DISTINCT page_id) AS avg_revisions ' +
    'FROM revisions');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Total revisions: %d across %d pages (avg %.1f per page)', [
        DS.FieldByName('total_revisions').AsInteger,
        DS.FieldByName('pages_with_revisions').AsInteger,
        DS.FieldByName('avg_revisions').AsFloat]));
    end;
  finally
    DS.Free;
  end;

  // Workflow transitions
  DS := Conn.ExecuteQuery(
    'SELECT from_status || '' -> '' || to_status AS transition, COUNT(*) AS cnt ' +
    'FROM workflow_log GROUP BY from_status, to_status ORDER BY cnt DESC');
  try
    WriteLn;
    WriteLn('   Workflow transitions:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d time(s)', [
        DS.FieldByName('transition').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Average time to publish
  DS := Conn.ExecuteQuery(
    'SELECT slug, ' +
    '  CAST((julianday(published_at) - julianday(created_at)) * 24 AS INTEGER) AS hours_to_publish ' +
    'FROM pages WHERE status = ''published'' AND published_at IS NOT NULL ' +
    'ORDER BY hours_to_publish DESC LIMIT 5');
  try
    WriteLn;
    WriteLn('   Time to publish (hours from creation):');
    while not DS.EOF do
    begin
      WriteLn(Format('   - /%s: %d hours', [
        DS.FieldByName('slug').AsString,
        DS.FieldByName('hours_to_publish').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

begin
  WriteLn('Example 144: CMS - Content Management System');
  WriteLn('======================================================================');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_PagesOverview;
    Demo2_DraftPublishedStates;
    Demo3_PublishingWorkflow;
    Demo4_RevisionHistory;
    Demo5_SlugRouting;
    Demo6_ContentHierarchy;
    Demo7_Categories;
    Demo8_Metadata;
    Demo9_ContentSearch;
    Demo10_PublishingStats;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
