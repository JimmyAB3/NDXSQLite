{===============================================================================
  NDXSQLite Example 73 - Advanced Many-to-Many Relationships
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Junction tables with metadata (dates, roles, attributes)
  - Self-referential many-to-many (users following users)
  - Ordered relationships (playlists, reading lists)
  - Relationship with status/state machine
  - Historical relationships (with valid_from/valid_to)
  - Complex queries on relationship metadata

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program ManyToManyAdvanced;

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
  WriteLn('1. Creating Advanced M:N Schema');
  WriteLn('   =============================');

  // -------------------------------------------------------------------------
  // Example 1: Project-Team Members with roles and dates
  // -------------------------------------------------------------------------
  Conn.ExecuteNonQuery(
    'CREATE TABLE projects (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  status TEXT DEFAULT ''active'',' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE team_members (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT UNIQUE,' +
    '  department TEXT' +
    ')');

  // Junction table with rich metadata
  Conn.ExecuteNonQuery(
    'CREATE TABLE project_assignments (' +
    '  id INTEGER PRIMARY KEY,' +
    '  project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE,' +
    '  member_id INTEGER NOT NULL REFERENCES team_members(id) ON DELETE CASCADE,' +
    '  role TEXT NOT NULL,' +           // 'lead', 'developer', 'designer', 'qa'
    '  allocation_percent INTEGER DEFAULT 100,' +  // 0-100%
    '  hourly_rate REAL,' +
    '  joined_at TEXT DEFAULT (datetime(''now'')),' +
    '  left_at TEXT,' +                  // NULL if still active
    '  notes TEXT,' +
    '  UNIQUE(project_id, member_id, role),' +
    '  CHECK (allocation_percent BETWEEN 0 AND 100),' +
    '  CHECK (role IN (''lead'', ''developer'', ''designer'', ''qa'', ''analyst''))' +
    ')');

  // -------------------------------------------------------------------------
  // Example 2: Self-referential M:N - Social Network Followers
  // -------------------------------------------------------------------------
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT NOT NULL UNIQUE,' +
    '  display_name TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE user_follows (' +
    '  id INTEGER PRIMARY KEY,' +
    '  follower_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  following_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  followed_at TEXT DEFAULT (datetime(''now'')),' +
    '  notifications_enabled INTEGER DEFAULT 1,' +
    '  relationship_type TEXT DEFAULT ''follow'',' +  // 'follow', 'close_friend', 'muted'
    '  UNIQUE(follower_id, following_id),' +
    '  CHECK (follower_id != following_id)' +  // Cannot follow yourself
    ')');

  // -------------------------------------------------------------------------
  // Example 3: Ordered M:N - Playlist Songs
  // -------------------------------------------------------------------------
  Conn.ExecuteNonQuery(
    'CREATE TABLE playlists (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER REFERENCES users(id),' +
    '  name TEXT NOT NULL,' +
    '  is_public INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE songs (' +
    '  id INTEGER PRIMARY KEY,' +
    '  title TEXT NOT NULL,' +
    '  artist TEXT NOT NULL,' +
    '  album TEXT,' +
    '  duration_seconds INTEGER,' +
    '  genre TEXT' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE playlist_songs (' +
    '  id INTEGER PRIMARY KEY,' +
    '  playlist_id INTEGER NOT NULL REFERENCES playlists(id) ON DELETE CASCADE,' +
    '  song_id INTEGER NOT NULL REFERENCES songs(id) ON DELETE CASCADE,' +
    '  position INTEGER NOT NULL,' +     // Order in playlist
    '  added_at TEXT DEFAULT (datetime(''now'')),' +
    '  added_by INTEGER REFERENCES users(id),' +
    '  play_count INTEGER DEFAULT 0,' +
    '  last_played_at TEXT,' +
    '  UNIQUE(playlist_id, position),' +  // Each position unique per playlist
    '  UNIQUE(playlist_id, song_id)' +    // No duplicates in playlist
    ')');

  // -------------------------------------------------------------------------
  // Example 4: M:N with State Machine - Course Enrollments
  // -------------------------------------------------------------------------
  Conn.ExecuteNonQuery(
    'CREATE TABLE courses (' +
    '  id INTEGER PRIMARY KEY,' +
    '  title TEXT NOT NULL,' +
    '  instructor TEXT,' +
    '  max_students INTEGER DEFAULT 30,' +
    '  start_date TEXT,' +
    '  end_date TEXT' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE students (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT UNIQUE,' +
    '  grade_level INTEGER' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE enrollments (' +
    '  id INTEGER PRIMARY KEY,' +
    '  course_id INTEGER NOT NULL REFERENCES courses(id),' +
    '  student_id INTEGER NOT NULL REFERENCES students(id),' +
    '  status TEXT NOT NULL DEFAULT ''pending'',' +  // pending, enrolled, completed, dropped, failed
    '  enrolled_at TEXT,' +
    '  completed_at TEXT,' +
    '  grade TEXT,' +               // A, B, C, D, F, or NULL
    '  grade_points REAL,' +        // 4.0, 3.0, etc.
    '  attendance_percent REAL,' +
    '  notes TEXT,' +
    '  UNIQUE(course_id, student_id),' +
    '  CHECK (status IN (''pending'', ''enrolled'', ''completed'', ''dropped'', ''failed''))' +
    ')');

  // Status history for enrollments
  Conn.ExecuteNonQuery(
    'CREATE TABLE enrollment_history (' +
    '  id INTEGER PRIMARY KEY,' +
    '  enrollment_id INTEGER NOT NULL REFERENCES enrollments(id) ON DELETE CASCADE,' +
    '  old_status TEXT,' +
    '  new_status TEXT NOT NULL,' +
    '  changed_at TEXT DEFAULT (datetime(''now'')),' +
    '  changed_by TEXT,' +
    '  reason TEXT' +
    ')');

  // -------------------------------------------------------------------------
  // Example 5: Temporal/Historical M:N - Employee Job History
  // -------------------------------------------------------------------------
  Conn.ExecuteNonQuery(
    'CREATE TABLE employees (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  hire_date TEXT' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE positions (' +
    '  id INTEGER PRIMARY KEY,' +
    '  title TEXT NOT NULL,' +
    '  department TEXT,' +
    '  salary_min REAL,' +
    '  salary_max REAL' +
    ')');

  Conn.ExecuteNonQuery(
    'CREATE TABLE employee_positions (' +
    '  id INTEGER PRIMARY KEY,' +
    '  employee_id INTEGER NOT NULL REFERENCES employees(id),' +
    '  position_id INTEGER NOT NULL REFERENCES positions(id),' +
    '  salary REAL,' +
    '  valid_from TEXT NOT NULL,' +
    '  valid_to TEXT,' +            // NULL means current position
    '  is_primary INTEGER DEFAULT 1,' +
    '  supervisor_id INTEGER REFERENCES employees(id),' +
    '  CHECK (valid_to IS NULL OR valid_to > valid_from)' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_assignments_project ON project_assignments(project_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_assignments_member ON project_assignments(member_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_follows_follower ON user_follows(follower_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_follows_following ON user_follows(following_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_playlist_songs ON playlist_songs(playlist_id, position)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_enrollments_status ON enrollments(status)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_emp_pos_dates ON employee_positions(employee_id, valid_from, valid_to)');

  WriteLn('   Created 5 M:N relationship examples:');
  WriteLn('     1. Project assignments (with roles, allocation, rates)');
  WriteLn('     2. User follows (self-referential social network)');
  WriteLn('     3. Playlist songs (ordered with play counts)');
  WriteLn('     4. Course enrollments (with status state machine)');
  WriteLn('     5. Employee positions (temporal/historical)');
  WriteLn('');
end;

// =============================================================================
// Populate Sample Data
// =============================================================================
{ Inserts sample projects, team members, users, songs, courses, students, employees, and positions into all tables to support the M:N relationship demos. }
procedure PopulateSampleData;
begin
  WriteLn('2. Populating Sample Data');
  WriteLn('   =======================');

  // Projects and Team Members
  Conn.ExecuteNonQuery('INSERT INTO projects (name, description) VALUES (''Website Redesign'', ''Complete overhaul of company website'')');
  Conn.ExecuteNonQuery('INSERT INTO projects (name, description) VALUES (''Mobile App'', ''iOS and Android app development'')');
  Conn.ExecuteNonQuery('INSERT INTO projects (name, description) VALUES (''Data Migration'', ''Legacy system data migration'')');

  Conn.ExecuteNonQuery('INSERT INTO team_members (name, email, department) VALUES (''Alice Johnson'', ''alice@company.com'', ''Engineering'')');
  Conn.ExecuteNonQuery('INSERT INTO team_members (name, email, department) VALUES (''Bob Smith'', ''bob@company.com'', ''Engineering'')');
  Conn.ExecuteNonQuery('INSERT INTO team_members (name, email, department) VALUES (''Carol White'', ''carol@company.com'', ''Design'')');
  Conn.ExecuteNonQuery('INSERT INTO team_members (name, email, department) VALUES (''David Brown'', ''david@company.com'', ''QA'')');
  Conn.ExecuteNonQuery('INSERT INTO team_members (name, email, department) VALUES (''Eve Davis'', ''eve@company.com'', ''Engineering'')');

  // Users for social network
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''alice'', ''Alice Johnson'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''bob'', ''Bob Smith'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''carol'', ''Carol White'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''david'', ''David Brown'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''eve'', ''Eve Davis'')');

  // Songs
  Conn.ExecuteNonQuery('INSERT INTO songs (title, artist, album, duration_seconds, genre) VALUES (''Bohemian Rhapsody'', ''Queen'', ''A Night at the Opera'', 354, ''Rock'')');
  Conn.ExecuteNonQuery('INSERT INTO songs (title, artist, album, duration_seconds, genre) VALUES (''Stairway to Heaven'', ''Led Zeppelin'', ''Led Zeppelin IV'', 482, ''Rock'')');
  Conn.ExecuteNonQuery('INSERT INTO songs (title, artist, album, duration_seconds, genre) VALUES (''Hotel California'', ''Eagles'', ''Hotel California'', 391, ''Rock'')');
  Conn.ExecuteNonQuery('INSERT INTO songs (title, artist, album, duration_seconds, genre) VALUES (''Billie Jean'', ''Michael Jackson'', ''Thriller'', 294, ''Pop'')');
  Conn.ExecuteNonQuery('INSERT INTO songs (title, artist, album, duration_seconds, genre) VALUES (''Sweet Child O Mine'', ''Guns N Roses'', ''Appetite for Destruction'', 356, ''Rock'')');
  Conn.ExecuteNonQuery('INSERT INTO songs (title, artist, album, duration_seconds, genre) VALUES (''Smells Like Teen Spirit'', ''Nirvana'', ''Nevermind'', 301, ''Grunge'')');

  // Courses and Students
  Conn.ExecuteNonQuery('INSERT INTO courses (title, instructor, max_students, start_date, end_date) VALUES (''Database Design'', ''Dr. Smith'', 25, ''2024-01-15'', ''2024-05-15'')');
  Conn.ExecuteNonQuery('INSERT INTO courses (title, instructor, max_students, start_date, end_date) VALUES (''Web Development'', ''Prof. Jones'', 30, ''2024-01-15'', ''2024-05-15'')');
  Conn.ExecuteNonQuery('INSERT INTO courses (title, instructor, max_students) VALUES (''Machine Learning'', ''Dr. Lee'', 20)');

  Conn.ExecuteNonQuery('INSERT INTO students (name, email, grade_level) VALUES (''Student A'', ''studenta@school.edu'', 3)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, email, grade_level) VALUES (''Student B'', ''studentb@school.edu'', 2)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, email, grade_level) VALUES (''Student C'', ''studentc@school.edu'', 4)');
  Conn.ExecuteNonQuery('INSERT INTO students (name, email, grade_level) VALUES (''Student D'', ''studentd@school.edu'', 3)');

  // Employees and Positions
  Conn.ExecuteNonQuery('INSERT INTO employees (name, hire_date) VALUES (''John Manager'', ''2015-03-01'')');
  Conn.ExecuteNonQuery('INSERT INTO employees (name, hire_date) VALUES (''Jane Developer'', ''2018-06-15'')');
  Conn.ExecuteNonQuery('INSERT INTO employees (name, hire_date) VALUES (''Mike Analyst'', ''2020-01-10'')');

  Conn.ExecuteNonQuery('INSERT INTO positions (title, department, salary_min, salary_max) VALUES (''Junior Developer'', ''Engineering'', 50000, 70000)');
  Conn.ExecuteNonQuery('INSERT INTO positions (title, department, salary_min, salary_max) VALUES (''Senior Developer'', ''Engineering'', 80000, 120000)');
  Conn.ExecuteNonQuery('INSERT INTO positions (title, department, salary_min, salary_max) VALUES (''Tech Lead'', ''Engineering'', 110000, 150000)');
  Conn.ExecuteNonQuery('INSERT INTO positions (title, department, salary_min, salary_max) VALUES (''Engineering Manager'', ''Engineering'', 130000, 180000)');

  WriteLn('   Created sample data for all 5 examples');
  WriteLn('');
end;

// =============================================================================
// Demo 1: Project Assignments with Roles
// =============================================================================
{ Creates project-member assignments with roles, allocation percentages, and hourly rates, then queries team composition, member workload, and estimated project costs. }
procedure DemoProjectAssignments;
var
  DS: TDataSet;
begin
  WriteLn('3. Project Assignments with Roles');
  WriteLn('   ================================');
  WriteLn('');

  // Assign team members to projects with roles
  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (1, 1, ''lead'', 50, 150.00)');
  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (1, 2, ''developer'', 100, 100.00)');
  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (1, 3, ''designer'', 75, 95.00)');
  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (1, 4, ''qa'', 50, 80.00)');

  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (2, 1, ''lead'', 50, 150.00)');
  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (2, 5, ''developer'', 100, 110.00)');
  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (2, 3, ''designer'', 25, 95.00)');

  Conn.ExecuteNonQuery('INSERT INTO project_assignments (project_id, member_id, role, allocation_percent, hourly_rate) VALUES (3, 2, ''developer'', 0, 100.00)');  // Past assignment
  Conn.ExecuteNonQuery('UPDATE project_assignments SET left_at = datetime(''now'', ''-30 days'') WHERE project_id = 3 AND member_id = 2');

  WriteLn('   Team for "Website Redesign":');
  WriteLn('   ----------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT tm.name, pa.role, pa.allocation_percent, pa.hourly_rate ' +
    'FROM project_assignments pa ' +
    'JOIN team_members tm ON pa.member_id = tm.id ' +
    'WHERE pa.project_id = 1 AND pa.left_at IS NULL ' +
    'ORDER BY ' +
    '  CASE pa.role WHEN ''lead'' THEN 1 WHEN ''developer'' THEN 2 WHEN ''designer'' THEN 3 ELSE 4 END');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s - %s (%d%% allocation, $%.2f/hr)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('role').AsString,
         DS.FieldByName('allocation_percent').AsInteger,
         DS.FieldByName('hourly_rate').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Member workload across projects
  WriteLn('');
  WriteLn('   Alice Johnson''s assignments:');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, pa.role, pa.allocation_percent ' +
    'FROM project_assignments pa ' +
    'JOIN projects p ON pa.project_id = p.id ' +
    'WHERE pa.member_id = 1 AND pa.left_at IS NULL');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s - %s (%d%%)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('role').AsString,
         DS.FieldByName('allocation_percent').AsInteger]));
      DS.Next;
    end;
    WriteLn('   Total allocation: ' + IntToStr(Integer(Conn.ExecuteScalar(
      'SELECT SUM(allocation_percent) FROM project_assignments ' +
      'WHERE member_id = 1 AND left_at IS NULL'))) + '%');
  finally
    DS.Free;
  end;

  // Project cost estimation
  WriteLn('');
  WriteLn('   Estimated monthly cost per project (160 hours):');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, ' +
    '  SUM(pa.hourly_rate * 160 * pa.allocation_percent / 100) AS monthly_cost, ' +
    '  COUNT(*) AS team_size ' +
    'FROM project_assignments pa ' +
    'JOIN projects p ON pa.project_id = p.id ' +
    'WHERE pa.left_at IS NULL ' +
    'GROUP BY p.id ' +
    'ORDER BY monthly_cost DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: $%.2f (%d members)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('monthly_cost').AsFloat,
         DS.FieldByName('team_size').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo 2: Self-Referential Follows
// =============================================================================
{ Creates self-referential follow relationships between users and queries following lists, follower counts, mutual follows, and friend-of-friend suggestions. }
procedure DemoUserFollows;
var
  DS: TDataSet;
begin
  WriteLn('4. Self-Referential M:N (Social Follows)');
  WriteLn('   ======================================');
  WriteLn('');

  // Create follow relationships
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type) VALUES (1, 2, ''follow'')');
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type) VALUES (1, 3, ''close_friend'')');
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type) VALUES (2, 1, ''follow'')');  // Mutual
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type) VALUES (2, 3, ''follow'')');
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type) VALUES (3, 1, ''follow'')');
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type) VALUES (4, 1, ''follow'')');
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type) VALUES (5, 1, ''follow'')');
  Conn.ExecuteNonQuery('INSERT INTO user_follows (follower_id, following_id, relationship_type, notifications_enabled) VALUES (5, 2, ''muted'', 0)');

  // Who Alice follows
  WriteLn('   Alice follows:');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, uf.relationship_type ' +
    'FROM user_follows uf ' +
    'JOIN users u ON uf.following_id = u.id ' +
    'WHERE uf.follower_id = 1');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%s)',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('relationship_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Alice's followers
  WriteLn('');
  WriteLn('   Alice''s followers: ' + IntToStr(Integer(Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM user_follows WHERE following_id = 1'))));

  // Mutual follows (friends)
  WriteLn('');
  WriteLn('   Mutual follows (potential friends):');
  DS := Conn.ExecuteQuery(
    'SELECT u1.display_name AS user1, u2.display_name AS user2 ' +
    'FROM user_follows uf1 ' +
    'JOIN user_follows uf2 ON uf1.follower_id = uf2.following_id ' +
    '  AND uf1.following_id = uf2.follower_id ' +
    'JOIN users u1 ON uf1.follower_id = u1.id ' +
    'JOIN users u2 ON uf1.following_id = u2.id ' +
    'WHERE uf1.follower_id < uf1.following_id');  // Avoid duplicates
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s <-> %s',
        [DS.FieldByName('user1').AsString,
         DS.FieldByName('user2').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Suggest follows (friends of friends)
  WriteLn('');
  WriteLn('   Suggested follows for Alice (friends of friends):');
  DS := Conn.ExecuteQuery(
    'SELECT DISTINCT u.display_name, COUNT(*) AS mutual_friends ' +
    'FROM user_follows uf1 ' +  // Alice follows someone
    'JOIN user_follows uf2 ON uf1.following_id = uf2.follower_id ' +  // That person follows others
    'JOIN users u ON uf2.following_id = u.id ' +
    'WHERE uf1.follower_id = 1 ' +  // Alice
    '  AND uf2.following_id != 1 ' +  // Not Alice herself
    '  AND uf2.following_id NOT IN (SELECT following_id FROM user_follows WHERE follower_id = 1) ' +  // Not already following
    'GROUP BY uf2.following_id ' +
    'ORDER BY mutual_friends DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%d mutual connections)',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('mutual_friends').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo 3: Ordered Playlist Songs
// =============================================================================
{ Creates playlists with ordered songs and play counts, then queries playlist contents by position, total duration, and most-played songs across playlists. }
procedure DemoPlaylistSongs;
var
  DS: TDataSet;
begin
  WriteLn('5. Ordered M:N (Playlist Songs)');
  WriteLn('   =============================');
  WriteLn('');

  // Create playlists
  Conn.ExecuteNonQuery('INSERT INTO playlists (user_id, name, is_public) VALUES (1, ''Classic Rock Favorites'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO playlists (user_id, name, is_public) VALUES (1, ''Workout Mix'', 0)');

  // Add songs to playlist with specific order
  Conn.ExecuteNonQuery('INSERT INTO playlist_songs (playlist_id, song_id, position, added_by) VALUES (1, 1, 1, 1)');  // Bohemian Rhapsody
  Conn.ExecuteNonQuery('INSERT INTO playlist_songs (playlist_id, song_id, position, added_by) VALUES (1, 2, 2, 1)');  // Stairway to Heaven
  Conn.ExecuteNonQuery('INSERT INTO playlist_songs (playlist_id, song_id, position, added_by) VALUES (1, 3, 3, 1)');  // Hotel California
  Conn.ExecuteNonQuery('INSERT INTO playlist_songs (playlist_id, song_id, position, added_by) VALUES (1, 5, 4, 1)');  // Sweet Child O Mine

  Conn.ExecuteNonQuery('INSERT INTO playlist_songs (playlist_id, song_id, position, added_by) VALUES (2, 4, 1, 1)');  // Billie Jean
  Conn.ExecuteNonQuery('INSERT INTO playlist_songs (playlist_id, song_id, position, added_by) VALUES (2, 6, 2, 1)');  // Smells Like Teen Spirit
  Conn.ExecuteNonQuery('INSERT INTO playlist_songs (playlist_id, song_id, position, added_by) VALUES (2, 5, 3, 1)');  // Sweet Child O Mine

  // Simulate play counts
  Conn.ExecuteNonQuery('UPDATE playlist_songs SET play_count = 15, last_played_at = datetime(''now'', ''-1 hour'') WHERE playlist_id = 1 AND song_id = 1');
  Conn.ExecuteNonQuery('UPDATE playlist_songs SET play_count = 12, last_played_at = datetime(''now'', ''-2 hours'') WHERE playlist_id = 1 AND song_id = 2');
  Conn.ExecuteNonQuery('UPDATE playlist_songs SET play_count = 8 WHERE playlist_id = 1 AND song_id = 3');
  Conn.ExecuteNonQuery('UPDATE playlist_songs SET play_count = 20 WHERE playlist_id = 1 AND song_id = 5');

  WriteLn('   "Classic Rock Favorites" playlist:');
  WriteLn('   -----------------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT ps.position, s.title, s.artist, s.duration_seconds, ps.play_count ' +
    'FROM playlist_songs ps ' +
    'JOIN songs s ON ps.song_id = s.id ' +
    'WHERE ps.playlist_id = 1 ' +
    'ORDER BY ps.position');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %d. %s - %s (%d:%.2d) [%d plays]',
        [DS.FieldByName('position').AsInteger,
         DS.FieldByName('title').AsString,
         DS.FieldByName('artist').AsString,
         DS.FieldByName('duration_seconds').AsInteger div 60,
         DS.FieldByName('duration_seconds').AsInteger mod 60,
         DS.FieldByName('play_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Total duration
  WriteLn('');
  DS := Conn.ExecuteQuery(
    'SELECT SUM(s.duration_seconds) AS total_seconds, COUNT(*) AS song_count ' +
    'FROM playlist_songs ps ' +
    'JOIN songs s ON ps.song_id = s.id ' +
    'WHERE ps.playlist_id = 1');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   Total: %d songs, %d:%.2d duration',
        [DS.FieldByName('song_count').AsInteger,
         DS.FieldByName('total_seconds').AsInteger div 60,
         DS.FieldByName('total_seconds').AsInteger mod 60]));
    end;
  finally
    DS.Free;
  end;

  // Most played songs across all playlists
  WriteLn('');
  WriteLn('   Most played songs overall:');
  DS := Conn.ExecuteQuery(
    'SELECT s.title, s.artist, SUM(ps.play_count) AS total_plays ' +
    'FROM playlist_songs ps ' +
    'JOIN songs s ON ps.song_id = s.id ' +
    'GROUP BY s.id ' +
    'ORDER BY total_plays DESC ' +
    'LIMIT 3');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s - %s: %d plays',
        [DS.FieldByName('title').AsString,
         DS.FieldByName('artist').AsString,
         DS.FieldByName('total_plays').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo 4: Enrollments with State Machine
// =============================================================================
{ Demonstrates many-to-many relationships with state machine transitions. }
procedure DemoEnrollments;
var
  DS: TDataSet;
begin
  WriteLn('6. M:N with State Machine (Enrollments)');
  WriteLn('   =====================================');
  WriteLn('');

  // Create enrollments with different statuses
  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status, enrolled_at) VALUES (1, 1, ''enrolled'', datetime(''now'', ''-60 days''))');
  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status, enrolled_at, completed_at, grade, grade_points) VALUES (1, 2, ''completed'', datetime(''now'', ''-90 days''), datetime(''now'', ''-10 days''), ''A'', 4.0)');
  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status, enrolled_at, completed_at, grade, grade_points, attendance_percent) VALUES (1, 3, ''completed'', datetime(''now'', ''-90 days''), datetime(''now'', ''-10 days''), ''B'', 3.0, 85.0)');
  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status, notes) VALUES (1, 4, ''dropped'', ''Personal reasons'')');

  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status, enrolled_at) VALUES (2, 1, ''enrolled'', datetime(''now'', ''-30 days''))');
  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status, enrolled_at) VALUES (2, 2, ''enrolled'', datetime(''now'', ''-30 days''))');
  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status) VALUES (2, 4, ''pending'')');

  Conn.ExecuteNonQuery('INSERT INTO enrollments (course_id, student_id, status) VALUES (3, 3, ''pending'')');

  // Log status history
  Conn.ExecuteNonQuery('INSERT INTO enrollment_history (enrollment_id, old_status, new_status, reason) VALUES (1, ''pending'', ''enrolled'', ''Application approved'')');
  Conn.ExecuteNonQuery('INSERT INTO enrollment_history (enrollment_id, old_status, new_status, reason) VALUES (2, ''pending'', ''enrolled'', ''Application approved'')');
  Conn.ExecuteNonQuery('INSERT INTO enrollment_history (enrollment_id, old_status, new_status, reason) VALUES (2, ''enrolled'', ''completed'', ''Final exam passed'')');
  Conn.ExecuteNonQuery('INSERT INTO enrollment_history (enrollment_id, old_status, new_status, reason) VALUES (4, ''enrolled'', ''dropped'', ''Student request - personal reasons'')');

  // Course enrollment summary
  WriteLn('   Course enrollment summary:');
  WriteLn('   --------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT c.title, e.status, COUNT(*) AS count ' +
    'FROM enrollments e ' +
    'JOIN courses c ON e.course_id = c.id ' +
    'GROUP BY c.id, e.status ' +
    'ORDER BY c.title, e.status');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %s = %d',
        [DS.FieldByName('title').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Grade distribution
  WriteLn('');
  WriteLn('   Grade distribution for "Database Design":');
  DS := Conn.ExecuteQuery(
    'SELECT grade, COUNT(*) AS count, AVG(grade_points) AS avg_gpa ' +
    'FROM enrollments ' +
    'WHERE course_id = 1 AND status = ''completed'' AND grade IS NOT NULL ' +
    'GROUP BY grade');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d students (GPA: %.1f)',
        [DS.FieldByName('grade').AsString,
         DS.FieldByName('count').AsInteger,
         DS.FieldByName('avg_gpa').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Enrollment history
  WriteLn('');
  WriteLn('   Recent enrollment status changes:');
  DS := Conn.ExecuteQuery(
    'SELECT s.name, c.title, eh.old_status, eh.new_status, eh.reason, eh.changed_at ' +
    'FROM enrollment_history eh ' +
    'JOIN enrollments e ON eh.enrollment_id = e.id ' +
    'JOIN students s ON e.student_id = s.id ' +
    'JOIN courses c ON e.course_id = c.id ' +
    'ORDER BY eh.changed_at DESC ' +
    'LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s in %s: %s -> %s (%s)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('title').AsString,
         DS.FieldByName('old_status').AsString,
         DS.FieldByName('new_status').AsString,
         DS.FieldByName('reason').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Demo 5: Temporal Employee Positions
// =============================================================================
{ Records employee position history with valid_from/valid_to date ranges, then queries current positions, career progression timelines, and point-in-time organizational state. }
procedure DemoEmployeePositions;
var
  DS: TDataSet;
begin
  WriteLn('7. Temporal M:N (Employee Position History)');
  WriteLn('   =========================================');
  WriteLn('');

  // Employee position history
  // John: Junior -> Senior -> Tech Lead -> Manager
  Conn.ExecuteNonQuery('INSERT INTO employee_positions (employee_id, position_id, salary, valid_from, valid_to) VALUES (1, 1, 55000, ''2015-03-01'', ''2017-02-28'')');
  Conn.ExecuteNonQuery('INSERT INTO employee_positions (employee_id, position_id, salary, valid_from, valid_to) VALUES (1, 2, 90000, ''2017-03-01'', ''2019-12-31'')');
  Conn.ExecuteNonQuery('INSERT INTO employee_positions (employee_id, position_id, salary, valid_from, valid_to) VALUES (1, 3, 125000, ''2020-01-01'', ''2022-06-30'')');
  Conn.ExecuteNonQuery('INSERT INTO employee_positions (employee_id, position_id, salary, valid_from, supervisor_id) VALUES (1, 4, 160000, ''2022-07-01'', NULL)');

  // Jane: Junior -> Senior (current)
  Conn.ExecuteNonQuery('INSERT INTO employee_positions (employee_id, position_id, salary, valid_from, valid_to, supervisor_id) VALUES (2, 1, 60000, ''2018-06-15'', ''2021-05-31'', 1)');
  Conn.ExecuteNonQuery('INSERT INTO employee_positions (employee_id, position_id, salary, valid_from, supervisor_id) VALUES (2, 2, 95000, ''2021-06-01'', 1)');

  // Mike: Still Junior (current)
  Conn.ExecuteNonQuery('INSERT INTO employee_positions (employee_id, position_id, salary, valid_from, supervisor_id) VALUES (3, 1, 65000, ''2020-01-10'', 1)');

  // Current positions
  WriteLn('   Current employee positions:');
  WriteLn('   ----------------------------');
  DS := Conn.ExecuteQuery(
    'SELECT e.name, p.title, ep.salary, ep.valid_from, sup.name AS supervisor ' +
    'FROM employee_positions ep ' +
    'JOIN employees e ON ep.employee_id = e.id ' +
    'JOIN positions p ON ep.position_id = p.id ' +
    'LEFT JOIN employees sup ON ep.supervisor_id = sup.id ' +
    'WHERE ep.valid_to IS NULL ' +
    'ORDER BY ep.salary DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s - %s ($%s) since %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('title').AsString,
         FormatFloat('#,##0', DS.FieldByName('salary').AsFloat),
         DS.FieldByName('valid_from').AsString]));
      if not DS.FieldByName('supervisor').IsNull then
        WriteLn(Format('       Reports to: %s', [DS.FieldByName('supervisor').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Career progression for John
  WriteLn('');
  WriteLn('   John Manager''s career progression:');
  DS := Conn.ExecuteQuery(
    'SELECT p.title, ep.salary, ep.valid_from, ep.valid_to ' +
    'FROM employee_positions ep ' +
    'JOIN positions p ON ep.position_id = p.id ' +
    'WHERE ep.employee_id = 1 ' +
    'ORDER BY ep.valid_from');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s - %s: $%s',
        [DS.FieldByName('valid_from').AsString,
         IfThen(DS.FieldByName('valid_to').IsNull, 'Present', DS.FieldByName('valid_to').AsString),
         FormatFloat('#,##0', DS.FieldByName('salary').AsFloat)]));
      WriteLn(Format('       Position: %s', [DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Position on a specific date
  WriteLn('');
  WriteLn('   Employees in position as of 2019-06-01:');
  DS := Conn.ExecuteQuery(
    'SELECT e.name, p.title, ep.salary ' +
    'FROM employee_positions ep ' +
    'JOIN employees e ON ep.employee_id = e.id ' +
    'JOIN positions p ON ep.position_id = p.id ' +
    'WHERE ep.valid_from <= ''2019-06-01'' ' +
    '  AND (ep.valid_to IS NULL OR ep.valid_to > ''2019-06-01'')');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s - %s ($%s)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('title').AsString,
         FormatFloat('#,##0', DS.FieldByName('salary').AsFloat)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Summary
// =============================================================================
{ Outputs a summary of junction table attributes, common M:N patterns (role-based, self-referential, ordered, state machine, temporal), best practices, and query strategies. }
procedure PrintSummary;
begin
  WriteLn('8. Advanced M:N Patterns Summary');
  WriteLn('   ==============================');
  WriteLn('');
  WriteLn('   Junction Table Attributes:');
  WriteLn('     - Dates: created_at, valid_from, valid_to, joined_at, left_at');
  WriteLn('     - Roles/Types: role, relationship_type, status');
  WriteLn('     - Quantities: allocation_percent, position, play_count');
  WriteLn('     - Values: hourly_rate, salary, grade, grade_points');
  WriteLn('     - References: added_by, supervisor_id, changed_by');
  WriteLn('');
  WriteLn('   Common Patterns:');
  WriteLn('     1. Role-based: project_assignments (member has role in project)');
  WriteLn('     2. Self-referential: user_follows (user follows user)');
  WriteLn('     3. Ordered: playlist_songs (position matters)');
  WriteLn('     4. State machine: enrollments (status transitions)');
  WriteLn('     5. Temporal: employee_positions (valid_from/valid_to)');
  WriteLn('');
  WriteLn('   Best Practices:');
  WriteLn('     1. Add primary key to junction table (not just composite)');
  WriteLn('     2. Use CHECK constraints for valid values');
  WriteLn('     3. Index both foreign keys separately');
  WriteLn('     4. Consider history/audit tables for status changes');
  WriteLn('     5. Use NULL for "current" in temporal patterns');
  WriteLn('     6. Add timestamps for auditing (created_at, updated_at)');
  WriteLn('');
  WriteLn('   Query Patterns:');
  WriteLn('     - Filter by relationship attributes (role, status, date range)');
  WriteLn('     - Aggregate on junction table (SUM allocation, COUNT by status)');
  WriteLn('     - Self-join for mutual relationships');
  WriteLn('     - Point-in-time queries for temporal data');
  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 73: Advanced Many-to-Many Relationships ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    PopulateSampleData;
    DemoProjectAssignments;
    DemoUserFollows;
    DemoPlaylistSongs;
    DemoEnrollments;
    DemoEmployeePositions;
    PrintSummary;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
