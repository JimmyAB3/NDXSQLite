{===============================================================================
  NDXSQLite Example 72 - Polymorphic Associations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Basic polymorphic association schema
  - Comments that can belong to posts, products, or users
  - Tags that can be applied to multiple entity types
  - Attachments/media for various entities
  - Activity logs across all entities
  - Efficient querying patterns
  - Type-safe approaches with CHECK constraints

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program PolymorphicAssociations;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Polymorphic Schema');
  WriteLn('   ============================');

  // Parent tables - the entities that can have comments, tags, etc.
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT NOT NULL UNIQUE,' +
    '  email TEXT NOT NULL,' +
    '  bio TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE posts (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  title TEXT NOT NULL,' +
    '  content TEXT,' +
    '  published_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  price REAL NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Polymorphic comments table
  // commentable_type + commentable_id together form the polymorphic reference
  Conn.ExecuteNonQuery(
    'CREATE TABLE comments (' +
    '  id INTEGER PRIMARY KEY,' +
    '  commentable_type TEXT NOT NULL,' +  // 'user', 'post', 'product'
    '  commentable_id INTEGER NOT NULL,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  content TEXT NOT NULL,' +
    '  rating INTEGER,' +  // Optional rating (1-5)
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  CHECK (commentable_type IN (''user'', ''post'', ''product'')),' +
    '  CHECK (rating IS NULL OR (rating >= 1 AND rating <= 5))' +
    ')');

  // Polymorphic tags (many-to-many via taggings)
  Conn.ExecuteNonQuery(
    'CREATE TABLE tags (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL UNIQUE,' +
    '  description TEXT' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE taggings (' +
    '  id INTEGER PRIMARY KEY,' +
    '  tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,' +
    '  taggable_type TEXT NOT NULL,' +
    '  taggable_id INTEGER NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(tag_id, taggable_type, taggable_id),' +
    '  CHECK (taggable_type IN (''user'', ''post'', ''product''))' +
    ')');

  // Polymorphic attachments (files, images)
  Conn.ExecuteNonQuery(
    'CREATE TABLE attachments (' +
    '  id INTEGER PRIMARY KEY,' +
    '  attachable_type TEXT NOT NULL,' +
    '  attachable_id INTEGER NOT NULL,' +
    '  filename TEXT NOT NULL,' +
    '  content_type TEXT,' +  // 'image/jpeg', 'application/pdf', etc.
    '  file_size INTEGER,' +
    '  storage_path TEXT,' +
    '  uploaded_at TEXT DEFAULT (datetime(''now'')),' +
    '  CHECK (attachable_type IN (''user'', ''post'', ''product''))' +
    ')');

  // Polymorphic activity log
  Conn.ExecuteNonQuery(
    'CREATE TABLE activity_logs (' +
    '  id INTEGER PRIMARY KEY,' +
    '  trackable_type TEXT NOT NULL,' +
    '  trackable_id INTEGER NOT NULL,' +
    '  action TEXT NOT NULL,' +  // 'created', 'updated', 'deleted', 'viewed'
    '  user_id INTEGER REFERENCES users(id),' +
    '  changes TEXT,' +  // JSON describing what changed
    '  ip_address TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes for efficient polymorphic queries
  Conn.ExecuteNonQuery('CREATE INDEX idx_comments_poly ON comments(commentable_type, commentable_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_taggings_poly ON taggings(taggable_type, taggable_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_attachments_poly ON attachments(attachable_type, attachable_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_activity_poly ON activity_logs(trackable_type, trackable_id)');

  WriteLn('   Created parent tables: users, posts, products');
  WriteLn('   Created polymorphic tables: comments, tags, taggings, attachments, activity_logs');
  WriteLn('   Created composite indexes for polymorphic queries');
  WriteLn('');
end;

// =============================================================================
// Populate Sample Data
// =============================================================================
{ Inserts sample users, posts, products, and tags into the database to provide test data for polymorphic association queries. }
procedure PopulateSampleData;
begin
  WriteLn('2. Populating Sample Data');
  WriteLn('   =======================');

  // Users
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, bio) VALUES (''alice'', ''alice@example.com'', ''Software developer and tech enthusiast'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, bio) VALUES (''bob'', ''bob@example.com'', ''Designer and photographer'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, bio) VALUES (''carol'', ''carol@example.com'', ''Product manager at TechCorp'')');
  WriteLn('   Created 3 users');

  // Posts
  Conn.ExecuteNonQuery('INSERT INTO posts (user_id, title, content) VALUES (1, ''Getting Started with SQLite'', ''SQLite is a lightweight database...'')');
  Conn.ExecuteNonQuery('INSERT INTO posts (user_id, title, content) VALUES (1, ''Advanced SQL Patterns'', ''Let us explore some advanced patterns...'')');
  Conn.ExecuteNonQuery('INSERT INTO posts (user_id, title, content) VALUES (2, ''Photography Tips'', ''Here are my top 10 tips for better photos...'')');
  WriteLn('   Created 3 posts');

  // Products
  Conn.ExecuteNonQuery('INSERT INTO products (name, description, price) VALUES (''Laptop Pro X'', ''High-performance laptop for professionals'', 1499.99)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, description, price) VALUES (''Wireless Mouse'', ''Ergonomic wireless mouse with long battery life'', 49.99)');
  Conn.ExecuteNonQuery('INSERT INTO products (name, description, price) VALUES (''USB-C Hub'', ''7-in-1 USB-C hub with HDMI and SD card reader'', 79.99)');
  WriteLn('   Created 3 products');

  // Tags
  Conn.ExecuteNonQuery('INSERT INTO tags (name, description) VALUES (''technology'', ''Tech-related content'')');
  Conn.ExecuteNonQuery('INSERT INTO tags (name, description) VALUES (''programming'', ''Programming and development'')');
  Conn.ExecuteNonQuery('INSERT INTO tags (name, description) VALUES (''photography'', ''Photography-related'')');
  Conn.ExecuteNonQuery('INSERT INTO tags (name, description) VALUES (''featured'', ''Featured/highlighted items'')');
  Conn.ExecuteNonQuery('INSERT INTO tags (name, description) VALUES (''new'', ''New items'')');
  WriteLn('   Created 5 tags');

  WriteLn('');
end;

// =============================================================================
// Polymorphic Comments
// =============================================================================
{ Inserts comments on posts, products, and user profiles, then queries them showing post comments, product reviews with ratings, and average rating calculation. }
procedure DemoPolymorphicComments;
var
  DS: TDataSet;
begin
  WriteLn('3. Polymorphic Comments');
  WriteLn('   =====================');
  WriteLn('');

  // Add comments to different entity types
  // Comments on a post
  Conn.ExecuteNonQuery(
    'INSERT INTO comments (commentable_type, commentable_id, user_id, content) ' +
    'VALUES (''post'', 1, 2, ''Great article! Very helpful for beginners.'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO comments (commentable_type, commentable_id, user_id, content) ' +
    'VALUES (''post'', 1, 3, ''I learned so much from this. Thanks!'')');

  // Comments on a product (with ratings)
  Conn.ExecuteNonQuery(
    'INSERT INTO comments (commentable_type, commentable_id, user_id, content, rating) ' +
    'VALUES (''product'', 1, 1, ''Excellent laptop! Fast and reliable.'', 5)');
  Conn.ExecuteNonQuery(
    'INSERT INTO comments (commentable_type, commentable_id, user_id, content, rating) ' +
    'VALUES (''product'', 1, 3, ''Good but a bit expensive.'', 4)');
  Conn.ExecuteNonQuery(
    'INSERT INTO comments (commentable_type, commentable_id, user_id, content, rating) ' +
    'VALUES (''product'', 2, 2, ''Perfect mouse, very comfortable!'', 5)');

  // Comment on a user profile
  Conn.ExecuteNonQuery(
    'INSERT INTO comments (commentable_type, commentable_id, user_id, content) ' +
    'VALUES (''user'', 1, 2, ''Alice writes amazing technical content!'')');

  WriteLn('   Added 6 comments across posts, products, and users');
  WriteLn('');

  // Query comments for a specific post
  WriteLn('   Comments on post "Getting Started with SQLite":');
  WriteLn('   -----------------------------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT c.content, u.username, c.created_at ' +
    'FROM comments c ' +
    'JOIN users u ON c.user_id = u.id ' +
    'WHERE c.commentable_type = ''post'' AND c.commentable_id = 1 ' +
    'ORDER BY c.created_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     @%s: "%s"',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('content').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Query product reviews with ratings
  WriteLn('');
  WriteLn('   Product reviews for "Laptop Pro X":');
  WriteLn('   ------------------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT c.content, c.rating, u.username ' +
    'FROM comments c ' +
    'JOIN users u ON c.user_id = u.id ' +
    'WHERE c.commentable_type = ''product'' AND c.commentable_id = 1 ' +
    'ORDER BY c.rating DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d/5] @%s: "%s"',
        [DS.FieldByName('rating').AsInteger,
         DS.FieldByName('username').AsString,
         DS.FieldByName('content').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Average rating for a product
  WriteLn('');
  WriteLn('   Average rating: ' + FormatFloat('0.0',
    Double(Conn.ExecuteScalar(
      'SELECT AVG(rating) FROM comments ' +
      'WHERE commentable_type = ''product'' AND commentable_id = 1 AND rating IS NOT NULL'))));

  WriteLn('');
end;

// =============================================================================
// Polymorphic Tags
// =============================================================================
{ Applies tags to posts, products, and users via the taggings junction table, then queries items by tag, lists tags for a post, and shows tag usage statistics. }
procedure DemoPolymorphicTags;
var
  DS: TDataSet;
begin
  WriteLn('4. Polymorphic Tags');
  WriteLn('   =================');
  WriteLn('');

  // Tag posts
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (1, ''post'', 1)'); // technology
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (2, ''post'', 1)'); // programming
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (2, ''post'', 2)'); // programming
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (3, ''post'', 3)'); // photography

  // Tag products
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (1, ''product'', 1)'); // technology
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (4, ''product'', 1)'); // featured
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (5, ''product'', 2)'); // new
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (5, ''product'', 3)'); // new

  // Tag users
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (2, ''user'', 1)'); // programming
  Conn.ExecuteNonQuery('INSERT INTO taggings (tag_id, taggable_type, taggable_id) VALUES (3, ''user'', 2)'); // photography

  WriteLn('   Applied 10 tags across posts, products, and users');
  WriteLn('');

  // Find all items tagged with "technology"
  WriteLn('   All items tagged "technology":');
  WriteLn('   -------------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT tg.taggable_type, tg.taggable_id, ' +
    '  CASE tg.taggable_type ' +
    '    WHEN ''post'' THEN (SELECT title FROM posts WHERE id = tg.taggable_id) ' +
    '    WHEN ''product'' THEN (SELECT name FROM products WHERE id = tg.taggable_id) ' +
    '    WHEN ''user'' THEN (SELECT username FROM users WHERE id = tg.taggable_id) ' +
    '  END AS item_name ' +
    'FROM taggings tg ' +
    'JOIN tags t ON tg.tag_id = t.id ' +
    'WHERE t.name = ''technology''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s',
        [DS.FieldByName('taggable_type').AsString,
         DS.FieldByName('item_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Get all tags for a specific post
  WriteLn('');
  WriteLn('   Tags for post "Getting Started with SQLite":');
  DS := Conn.ExecuteQuery(
    'SELECT t.name ' +
    'FROM tags t ' +
    'JOIN taggings tg ON t.id = tg.tag_id ' +
    'WHERE tg.taggable_type = ''post'' AND tg.taggable_id = 1');
  try
    Write('     ');
    while not DS.EOF do
    begin
      Write('#' + DS.FieldByName('name').AsString + ' ');
      DS.Next;
    end;
    WriteLn('');
  finally
    DS.Free;
  end;

  // Tag usage statistics
  WriteLn('');
  WriteLn('   Tag usage statistics:');
  DS := Conn.ExecuteQuery(
    'SELECT t.name, COUNT(tg.id) AS usage_count, ' +
    '  GROUP_CONCAT(DISTINCT tg.taggable_type) AS used_on ' +
    'FROM tags t ' +
    'LEFT JOIN taggings tg ON t.id = tg.tag_id ' +
    'GROUP BY t.id ' +
    'ORDER BY usage_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     #%s: %d uses (on: %s)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('usage_count').AsInteger,
         DS.FieldByName('used_on').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Polymorphic Attachments
// =============================================================================
{ Inserts file attachments (avatars, images, PDFs) for users, posts, and products, then queries attachments per product and summarizes storage usage by entity type. }
procedure DemoPolymorphicAttachments;
var
  DS: TDataSet;
begin
  WriteLn('5. Polymorphic Attachments');
  WriteLn('   ========================');
  WriteLn('');

  // Add attachments to various entities
  // User avatars
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''user'', 1, ''alice_avatar.jpg'', ''image/jpeg'', 45000, ''/uploads/users/1/avatar.jpg'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''user'', 2, ''bob_photo.png'', ''image/png'', 120000, ''/uploads/users/2/avatar.png'')');

  // Post images
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''post'', 1, ''sqlite_diagram.png'', ''image/png'', 250000, ''/uploads/posts/1/diagram.png'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''post'', 3, ''camera_shot1.jpg'', ''image/jpeg'', 1500000, ''/uploads/posts/3/photo1.jpg'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''post'', 3, ''camera_shot2.jpg'', ''image/jpeg'', 1800000, ''/uploads/posts/3/photo2.jpg'')');

  // Product images
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''product'', 1, ''laptop_front.jpg'', ''image/jpeg'', 500000, ''/uploads/products/1/front.jpg'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''product'', 1, ''laptop_side.jpg'', ''image/jpeg'', 480000, ''/uploads/products/1/side.jpg'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO attachments (attachable_type, attachable_id, filename, content_type, file_size, storage_path) ' +
    'VALUES (''product'', 1, ''specs.pdf'', ''application/pdf'', 150000, ''/uploads/products/1/specs.pdf'')');

  WriteLn('   Added 8 attachments across users, posts, and products');
  WriteLn('');

  // List attachments for a product
  WriteLn('   Attachments for "Laptop Pro X":');
  WriteLn('   --------------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT filename, content_type, file_size, storage_path ' +
    'FROM attachments ' +
    'WHERE attachable_type = ''product'' AND attachable_id = 1 ' +
    'ORDER BY uploaded_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%s, %d KB)',
        [DS.FieldByName('filename').AsString,
         DS.FieldByName('content_type').AsString,
         DS.FieldByName('file_size').AsInteger div 1024]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Storage usage by entity type
  WriteLn('');
  WriteLn('   Storage usage by entity type:');
  DS := Conn.ExecuteQuery(
    'SELECT attachable_type, ' +
    '  COUNT(*) AS file_count, ' +
    '  SUM(file_size) / 1024 AS total_kb ' +
    'FROM attachments ' +
    'GROUP BY attachable_type ' +
    'ORDER BY total_kb DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d files, %d KB',
        [DS.FieldByName('attachable_type').AsString,
         DS.FieldByName('file_count').AsInteger,
         DS.FieldByName('total_kb').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Polymorphic Activity Logs
// =============================================================================
{ Records activity events (created, updated, viewed) across entity types with user and IP tracking, then queries user activity history, view counts, and activity summaries. }
procedure DemoActivityLogs;
var
  DS: TDataSet;
begin
  WriteLn('6. Polymorphic Activity Logs');
  WriteLn('   ==========================');
  WriteLn('');

  // Log various activities
  Conn.ExecuteNonQuery(
    'INSERT INTO activity_logs (trackable_type, trackable_id, action, user_id, ip_address) ' +
    'VALUES (''user'', 1, ''created'', 1, ''192.168.1.100'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO activity_logs (trackable_type, trackable_id, action, user_id, changes, ip_address) ' +
    'VALUES (''user'', 1, ''updated'', 1, ''{"bio": ["old bio", "new bio"]}'', ''192.168.1.100'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO activity_logs (trackable_type, trackable_id, action, user_id, ip_address) ' +
    'VALUES (''post'', 1, ''created'', 1, ''192.168.1.100'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO activity_logs (trackable_type, trackable_id, action, user_id, ip_address) ' +
    'VALUES (''post'', 1, ''viewed'', 2, ''10.0.0.50'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO activity_logs (trackable_type, trackable_id, action, user_id, ip_address) ' +
    'VALUES (''post'', 1, ''viewed'', 3, ''10.0.0.51'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO activity_logs (trackable_type, trackable_id, action, user_id, ip_address) ' +
    'VALUES (''product'', 1, ''viewed'', 1, ''192.168.1.100'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO activity_logs (trackable_type, trackable_id, action, user_id, ip_address) ' +
    'VALUES (''product'', 1, ''viewed'', 2, ''10.0.0.50'')');

  WriteLn('   Logged 7 activities');
  WriteLn('');

  // Get activity for a specific user
  WriteLn('   Activity log for user "alice":');
  WriteLn('   -------------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT al.trackable_type, al.action, al.created_at, al.changes ' +
    'FROM activity_logs al ' +
    'WHERE al.user_id = 1 ' +
    'ORDER BY al.created_at DESC ' +
    'LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s on %s',
        [DS.FieldByName('created_at').AsString,
         DS.FieldByName('action').AsString,
         DS.FieldByName('trackable_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // View count for post
  WriteLn('');
  WriteLn('   View count for post #1: ' +
    IntToStr(Integer(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM activity_logs ' +
      'WHERE trackable_type = ''post'' AND trackable_id = 1 AND action = ''viewed'''))));

  // Activity summary by type
  WriteLn('');
  WriteLn('   Activity summary:');
  DS := Conn.ExecuteQuery(
    'SELECT trackable_type, action, COUNT(*) AS count ' +
    'FROM activity_logs ' +
    'GROUP BY trackable_type, action ' +
    'ORDER BY trackable_type, count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s.%s: %d',
        [DS.FieldByName('trackable_type').AsString,
         DS.FieldByName('action').AsString,
         DS.FieldByName('count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Universal Polymorphic Query
// =============================================================================
{ Retrieves all polymorphic associations (comments, tags, attachments, activity) for a single post entity to show the complete picture of related data. }
procedure DemoUniversalQuery;
var
  DS: TDataSet;
begin
  WriteLn('7. Universal Polymorphic Query');
  WriteLn('   =============================');
  WriteLn('');

  // Get all associated data for a specific entity
  WriteLn('   All data associated with post #1 "Getting Started with SQLite":');
  WriteLn('   ----------------------------------------------------------------');
  WriteLn('');

  // Comments
  WriteLn('   Comments:');
  DS := Conn.ExecuteQuery(
    'SELECT u.username, c.content FROM comments c ' +
    'JOIN users u ON c.user_id = u.id ' +
    'WHERE c.commentable_type = ''post'' AND c.commentable_id = 1');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     - @%s: %s',
        [DS.FieldByName('username').AsString,
         DS.FieldByName('content').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Tags
  Write('   Tags: ');
  DS := Conn.ExecuteQuery(
    'SELECT t.name FROM tags t ' +
    'JOIN taggings tg ON t.id = tg.tag_id ' +
    'WHERE tg.taggable_type = ''post'' AND tg.taggable_id = 1');
  try
    while not DS.EOF do
    begin
      Write('#' + DS.FieldByName('name').AsString + ' ');
      DS.Next;
    end;
    WriteLn('');
  finally
    DS.Free;
  end;

  // Attachments
  WriteLn('   Attachments:');
  DS := Conn.ExecuteQuery(
    'SELECT filename, content_type FROM attachments ' +
    'WHERE attachable_type = ''post'' AND attachable_id = 1');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     - %s (%s)',
        [DS.FieldByName('filename').AsString,
         DS.FieldByName('content_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Activity
  WriteLn('   Recent activity:');
  DS := Conn.ExecuteQuery(
    'SELECT action, COUNT(*) AS count FROM activity_logs ' +
    'WHERE trackable_type = ''post'' AND trackable_id = 1 ' +
    'GROUP BY action');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     - %s: %d times',
        [DS.FieldByName('action').AsString,
         DS.FieldByName('count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Type-Safe Alternative with Views
// =============================================================================
{ Creates type-specific SQL views (post_comments, product_reviews, product_images) that join polymorphic tables with their parent entities for simplified and safer querying. }
procedure DemoTypeSafeViews;
begin
  WriteLn('8. Type-Safe Views');
  WriteLn('   =================');
  WriteLn('');

  // Create type-specific views for better type safety
  Conn.ExecuteNonQuery(
    'CREATE VIEW post_comments AS ' +
    'SELECT c.*, p.title AS post_title, u.username AS author ' +
    'FROM comments c ' +
    'JOIN posts p ON c.commentable_id = p.id ' +
    'JOIN users u ON c.user_id = u.id ' +
    'WHERE c.commentable_type = ''post''');

  Conn.ExecuteNonQuery(
    'CREATE VIEW product_reviews AS ' +
    'SELECT c.*, pr.name AS product_name, u.username AS reviewer, c.rating ' +
    'FROM comments c ' +
    'JOIN products pr ON c.commentable_id = pr.id ' +
    'JOIN users u ON c.user_id = u.id ' +
    'WHERE c.commentable_type = ''product''');

  Conn.ExecuteNonQuery(
    'CREATE VIEW product_images AS ' +
    'SELECT a.*, p.name AS product_name ' +
    'FROM attachments a ' +
    'JOIN products p ON a.attachable_id = p.id ' +
    'WHERE a.attachable_type = ''product'' ' +
    '  AND a.content_type LIKE ''image/%''');

  WriteLn('   Created type-safe views:');
  WriteLn('     - post_comments: Comments on posts with post info');
  WriteLn('     - product_reviews: Product reviews with ratings');
  WriteLn('     - product_images: Images attached to products');
  WriteLn('');
  WriteLn('   These views provide cleaner queries:');
  WriteLn('     SELECT * FROM post_comments WHERE post_title LIKE ''%SQLite%''');
  WriteLn('     SELECT product_name, AVG(rating) FROM product_reviews GROUP BY product_name');
  WriteLn('');
end;

// =============================================================================
// Summary
// =============================================================================
{ Outputs the polymorphic associations pattern summary including schema design, advantages, disadvantages, best practices, and common use cases. }
procedure PrintSummary;
begin
  WriteLn('9. Polymorphic Associations Summary');
  WriteLn('   ==================================');
  WriteLn('');
  WriteLn('   Schema Pattern:');
  WriteLn('     [entity]_type TEXT  -- ''user'', ''post'', ''product''');
  WriteLn('     [entity]_id INTEGER -- Reference to parent table');
  WriteLn('     + Composite index on (type, id)');
  WriteLn('');
  WriteLn('   Advantages:');
  WriteLn('     + Single table for common functionality (comments, tags, etc.)');
  WriteLn('     + Easy to add new entity types without schema changes');
  WriteLn('     + Consistent API across all entity types');
  WriteLn('     + Simplified queries for cross-entity operations');
  WriteLn('');
  WriteLn('   Disadvantages:');
  WriteLn('     - No foreign key constraints (referential integrity)');
  WriteLn('     - Must maintain type consistency in application code');
  WriteLn('     - Joins are more complex');
  WriteLn('     - Cannot use database-level cascading deletes');
  WriteLn('');
  WriteLn('   Best Practices:');
  WriteLn('     1. Use CHECK constraints for valid types');
  WriteLn('     2. Create composite indexes on (type, id)');
  WriteLn('     3. Create type-safe views for common queries');
  WriteLn('     4. Validate referential integrity in application');
  WriteLn('     5. Consider triggers for cascading deletes');
  WriteLn('');
  WriteLn('   Common Use Cases:');
  WriteLn('     - Comments/reviews on multiple entity types');
  WriteLn('     - Tags applicable to various entities');
  WriteLn('     - File attachments for any entity');
  WriteLn('     - Activity/audit logs across all tables');
  WriteLn('     - Likes/favorites on different content types');
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 72: Polymorphic Associations ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    PopulateSampleData;
    DemoPolymorphicComments;
    DemoPolymorphicTags;
    DemoPolymorphicAttachments;
    DemoActivityLogs;
    DemoUniversalQuery;
    DemoTypeSafeViews;
    PrintSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
