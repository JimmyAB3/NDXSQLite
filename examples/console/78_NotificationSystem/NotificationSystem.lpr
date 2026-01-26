{===============================================================================
  NDXSQLite Example 78 - Notification System
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  A comprehensive notification system demonstrating:
  - Multiple notification types (info, warning, success, error)
  - Read/unread status tracking
  - Notification categories and priorities
  - User notification preferences
  - Notification grouping
  - Batch operations (mark all read, delete old)
  - Push notification queue
  - Notification templates
  - Delivery tracking

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program NotificationSystem;

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
  WriteLn('1. Creating Notification System Schema');
  WriteLn('   =====================================');

  // Users table
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  email TEXT UNIQUE NOT NULL,' +
    '  display_name TEXT,' +
    '  unread_count INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Notification categories
  Conn.ExecuteNonQuery(
    'CREATE TABLE notification_categories (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT UNIQUE NOT NULL,' +
    '  description TEXT,' +
    '  icon TEXT,' +
    '  color TEXT DEFAULT ''#808080''' +
    ')');

  // Notification templates
  Conn.ExecuteNonQuery(
    'CREATE TABLE notification_templates (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT UNIQUE NOT NULL,' +
    '  category_id INTEGER REFERENCES notification_categories(id),' +
    '  title_template TEXT NOT NULL,' +
    '  body_template TEXT NOT NULL,' +
    '  default_priority INTEGER DEFAULT 1' +
    ')');

  // Main notifications table
  Conn.ExecuteNonQuery(
    'CREATE TABLE notifications (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  category_id INTEGER REFERENCES notification_categories(id),' +
    '  notification_type TEXT DEFAULT ''info'' CHECK(notification_type IN (''info'', ''success'', ''warning'', ''error'')),' +
    '  priority INTEGER DEFAULT 1 CHECK(priority >= 0 AND priority <= 3),' +
    '  title TEXT NOT NULL,' +
    '  body TEXT,' +
    '  action_url TEXT,' +
    '  action_text TEXT,' +
    '  image_url TEXT,' +
    '  is_read INTEGER DEFAULT 0,' +
    '  is_archived INTEGER DEFAULT 0,' +
    '  is_deleted INTEGER DEFAULT 0,' +
    '  group_key TEXT,' +
    '  read_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  expires_at TEXT' +
    ')');

  // User notification preferences
  Conn.ExecuteNonQuery(
    'CREATE TABLE user_preferences (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,' +
    '  category_id INTEGER REFERENCES notification_categories(id),' +
    '  email_enabled INTEGER DEFAULT 1,' +
    '  push_enabled INTEGER DEFAULT 1,' +
    '  in_app_enabled INTEGER DEFAULT 1,' +
    '  minimum_priority INTEGER DEFAULT 0,' +
    '  UNIQUE(user_id, category_id)' +
    ')');

  // Push notification queue
  Conn.ExecuteNonQuery(
    'CREATE TABLE push_queue (' +
    '  id INTEGER PRIMARY KEY,' +
    '  notification_id INTEGER NOT NULL REFERENCES notifications(id) ON DELETE CASCADE,' +
    '  channel TEXT DEFAULT ''push'' CHECK(channel IN (''push'', ''email'', ''sms'')),' +
    '  status TEXT DEFAULT ''pending'' CHECK(status IN (''pending'', ''sent'', ''failed'', ''cancelled'')),' +
    '  attempts INTEGER DEFAULT 0,' +
    '  last_attempt TEXT,' +
    '  error_message TEXT,' +
    '  scheduled_for TEXT DEFAULT (datetime(''now'')),' +
    '  sent_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Notification delivery log
  Conn.ExecuteNonQuery(
    'CREATE TABLE delivery_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  notification_id INTEGER NOT NULL REFERENCES notifications(id) ON DELETE CASCADE,' +
    '  channel TEXT NOT NULL,' +
    '  status TEXT NOT NULL,' +
    '  device_info TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_notifications_user ON notifications(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_notifications_unread ON notifications(user_id, is_read) WHERE is_read = 0');
  Conn.ExecuteNonQuery('CREATE INDEX idx_notifications_category ON notifications(category_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_notifications_group ON notifications(group_key)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_push_queue_pending ON push_queue(status) WHERE status = ''pending''');

  WriteLn('   Created tables: users, notification_categories, notification_templates,');
  WriteLn('                   notifications, user_preferences, push_queue, delivery_log');
  WriteLn('');
end;

// =============================================================================
// Helper Functions
// =============================================================================
{ Recalculates and stores the unread notification count for the specified user based on non-deleted, unread notifications. }
procedure UpdateUnreadCount(UserId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE users SET unread_count = ' +
    '  (SELECT COUNT(*) FROM notifications WHERE user_id = ? AND is_read = 0 AND is_deleted = 0) ' +
    'WHERE id = ?', [UserId, UserId]);
end;

{ Converts a priority level integer to its display name. }
function PriorityToString(Priority: Integer): string;
begin
  case Priority of
    0: Result := 'Low';
    1: Result := 'Normal';
    2: Result := 'High';
    3: Result := 'Urgent';
  else
    Result := 'Unknown';
  end;
end;

{ Returns the display icon for a notification type. }
function TypeToIcon(const NotifType: string): string;
begin
  if NotifType = 'info' then Result := '[i]'
  else if NotifType = 'success' then Result := '[+]'
  else if NotifType = 'warning' then Result := '[!]'
  else if NotifType = 'error' then Result := '[X]'
  else Result := '[ ]';
end;

{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

// =============================================================================
// Notification Functions
// =============================================================================
{ Inserts a new notification record for the given user, category, type, title, body, priority, action URL, and group key, then updates the user's unread count. }
function CreateNotification(UserId, CategoryId: Integer; const NotifType, Title, Body: string;
  Priority: Integer = 1; const ActionUrl: string = ''; const GroupKey: string = ''): Integer;
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO notifications (user_id, category_id, notification_type, title, body, priority, action_url, group_key) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
    [UserId, CategoryId, NotifType, Title, Body, Priority, ActionUrl, GroupKey]);

  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');
  UpdateUnreadCount(UserId);
end;

{ Sets a single notification as read with a timestamp, then updates the owning user's unread count. }
procedure MarkAsRead(NotificationId: Integer);
var
  UserId: Integer;
begin
  UserId := Conn.ExecuteScalar('SELECT user_id FROM notifications WHERE id = ?', [NotificationId]);
  Conn.ExecuteNonQuery(
    'UPDATE notifications SET is_read = 1, read_at = datetime(''now'') WHERE id = ? AND is_read = 0',
    [NotificationId]);
  UpdateUnreadCount(UserId);
end;

{ Sets all non-deleted, unread notifications for the specified user as read and updates the user's unread count. }
procedure MarkAllAsRead(UserId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE notifications SET is_read = 1, read_at = datetime(''now'') ' +
    'WHERE user_id = ? AND is_read = 0 AND is_deleted = 0', [UserId]);
  UpdateUnreadCount(UserId);
end;

{ Soft-deletes a notification by setting its is_deleted flag and updates the owning user's unread count. }
procedure DeleteNotification(NotificationId: Integer);
var
  UserId: Integer;
begin
  UserId := Conn.ExecuteScalar('SELECT user_id FROM notifications WHERE id = ?', [NotificationId]);
  Conn.ExecuteNonQuery('UPDATE notifications SET is_deleted = 1 WHERE id = ?', [NotificationId]);
  UpdateUnreadCount(UserId);
end;

{ Sets the is_archived flag on the specified notification record. }
procedure ArchiveNotification(NotificationId: Integer);
begin
  Conn.ExecuteNonQuery('UPDATE notifications SET is_archived = 1 WHERE id = ?', [NotificationId]);
end;

{ Inserts a push queue entry linking the notification to the specified delivery channel (push, email, or sms). }
procedure QueuePushNotification(NotificationId: Integer; const Channel: string);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO push_queue (notification_id, channel) VALUES (?, ?)',
    [NotificationId, Channel]);
end;

// =============================================================================
// Sample Data
// =============================================================================
{ Creates sample users, notification categories, templates, notifications across multiple categories and priorities, queues push deliveries, marks some as read, and configures user preferences. }
procedure InsertSampleData;
var
  NotifId: Integer;
begin
  WriteLn('2. Creating Sample Data');
  WriteLn('   ======================');

  // Create users
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, display_name) VALUES (''alice'', ''alice@example.com'', ''Alice Johnson'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, display_name) VALUES (''bob'', ''bob@example.com'', ''Bob Smith'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, email, display_name) VALUES (''carol'', ''carol@example.com'', ''Carol White'')');

  WriteLn('   Created 3 users');

  // Create notification categories
  Conn.ExecuteNonQuery('INSERT INTO notification_categories (name, description, icon, color) VALUES (''system'', ''System notifications'', ''gear'', ''#6c757d'')');
  Conn.ExecuteNonQuery('INSERT INTO notification_categories (name, description, icon, color) VALUES (''social'', ''Social interactions'', ''users'', ''#0d6efd'')');
  Conn.ExecuteNonQuery('INSERT INTO notification_categories (name, description, icon, color) VALUES (''orders'', ''Order updates'', ''cart'', ''#198754'')');
  Conn.ExecuteNonQuery('INSERT INTO notification_categories (name, description, icon, color) VALUES (''security'', ''Security alerts'', ''shield'', ''#dc3545'')');
  Conn.ExecuteNonQuery('INSERT INTO notification_categories (name, description, icon, color) VALUES (''marketing'', ''Promotions and offers'', ''tag'', ''#ffc107'')');

  WriteLn('   Created 5 notification categories');

  // Create notification templates
  Conn.ExecuteNonQuery(
    'INSERT INTO notification_templates (name, category_id, title_template, body_template, default_priority) ' +
    'VALUES (''new_follower'', 2, ''New follower!'', ''{{user}} started following you'', 1)');
  Conn.ExecuteNonQuery(
    'INSERT INTO notification_templates (name, category_id, title_template, body_template, default_priority) ' +
    'VALUES (''order_shipped'', 3, ''Order shipped'', ''Your order #{{order_id}} has been shipped'', 2)');
  Conn.ExecuteNonQuery(
    'INSERT INTO notification_templates (name, category_id, title_template, body_template, default_priority) ' +
    'VALUES (''login_alert'', 4, ''New login detected'', ''A new login was detected from {{location}}'', 3)');

  WriteLn('   Created notification templates');

  // Create notifications for Alice
  WriteLn('');
  WriteLn('   Creating notifications for Alice:');

  NotifId := CreateNotification(1, 2, 'info', 'Bob started following you',
    'You have a new follower! Check out their profile.', 1, '/users/bob', 'followers');
  QueuePushNotification(NotifId, 'push');
  WriteLn('     + Social: New follower (info)');

  NotifId := CreateNotification(1, 2, 'info', 'Carol liked your post',
    'Carol liked your post "My weekend adventure".', 1, '/posts/123', 'likes');
  WriteLn('     + Social: Post like (info)');

  NotifId := CreateNotification(1, 3, 'success', 'Order #1234 shipped!',
    'Your order is on its way. Expected delivery: Tomorrow.', 2, '/orders/1234', 'order_1234');
  QueuePushNotification(NotifId, 'push');
  QueuePushNotification(NotifId, 'email');
  WriteLn('     + Orders: Order shipped (success, high priority)');

  NotifId := CreateNotification(1, 4, 'warning', 'Unusual login detected',
    'We noticed a login from a new device in Seattle, WA.', 3, '/security/sessions', '');
  QueuePushNotification(NotifId, 'push');
  QueuePushNotification(NotifId, 'email');
  QueuePushNotification(NotifId, 'sms');
  WriteLn('     + Security: Login alert (warning, urgent)');

  NotifId := CreateNotification(1, 5, 'info', '50% off this weekend!',
    'Use code SAVE50 at checkout for amazing savings.', 0, '/deals', 'promo_weekend');
  WriteLn('     + Marketing: Promotion (info, low priority)');

  NotifId := CreateNotification(1, 1, 'info', 'System maintenance scheduled',
    'The system will be down for maintenance on Sunday 2am-4am.', 1, '', 'maintenance');
  WriteLn('     + System: Maintenance notice (info)');

  // Create notifications for Bob
  WriteLn('');
  WriteLn('   Creating notifications for Bob:');

  NotifId := CreateNotification(2, 3, 'success', 'Order #5678 delivered',
    'Your order has been delivered. Enjoy!', 2, '/orders/5678', 'order_5678');
  WriteLn('     + Orders: Order delivered (success)');

  NotifId := CreateNotification(2, 2, 'info', 'Alice commented on your photo',
    'Alice said: "Great shot! Where was this taken?"', 1, '/photos/456/comments', 'comments');
  WriteLn('     + Social: Comment (info)');

  NotifId := CreateNotification(2, 4, 'error', 'Password change required',
    'Your password has been compromised. Please change it immediately.', 3, '/security/password', '');
  QueuePushNotification(NotifId, 'push');
  QueuePushNotification(NotifId, 'email');
  WriteLn('     + Security: Password alert (error, urgent)');

  // Create notifications for Carol
  WriteLn('');
  WriteLn('   Creating notifications for Carol:');

  NotifId := CreateNotification(3, 3, 'info', 'Order #9012 processing',
    'Your order is being prepared and will ship soon.', 1, '/orders/9012', 'order_9012');
  WriteLn('     + Orders: Order processing (info)');

  NotifId := CreateNotification(3, 1, 'success', 'Profile verified!',
    'Congratulations! Your profile has been verified.', 1, '/profile', '');
  WriteLn('     + System: Profile verified (success)');

  // Simulate some notifications being read
  WriteLn('');
  WriteLn('   Marking some notifications as read...');
  MarkAsRead(2); // Carol liked your post
  MarkAsRead(6); // System maintenance

  // Set up user preferences
  Conn.ExecuteNonQuery(
    'INSERT INTO user_preferences (user_id, category_id, email_enabled, push_enabled, minimum_priority) ' +
    'VALUES (1, 5, 0, 0, 2)'); // Alice disabled marketing email/push, only high priority
  Conn.ExecuteNonQuery(
    'INSERT INTO user_preferences (user_id, category_id, email_enabled) ' +
    'VALUES (2, 5, 0)'); // Bob disabled marketing emails

  WriteLn('   Set up user preferences');
  WriteLn('');
end;

// =============================================================================
// Display Notifications
// =============================================================================
{ Displays each user's unread count and renders Alice's full notification inbox sorted by priority, showing type icons, read status, category, and body text. }
procedure DemoNotificationInbox;
var
  DS: TDataSet;
begin
  WriteLn('3. Notification Inbox Display');
  WriteLn('   ============================');
  WriteLn('');

  // User unread counts
  DS := Conn.ExecuteQuery('SELECT display_name, unread_count FROM users ORDER BY id');
  try
    WriteLn('   Unread notification counts:');
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d unread',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('unread_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Alice's inbox
  WriteLn('');
  WriteLn('   Alice''s Notification Inbox:');
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT n.*, nc.name AS category_name ' +
    'FROM notifications n ' +
    'LEFT JOIN notification_categories nc ON n.category_id = nc.id ' +
    'WHERE n.user_id = 1 AND n.is_deleted = 0 ' +
    'ORDER BY n.priority DESC, n.created_at DESC');
  try
    while not DS.EOF do
    begin
      Write(Format('   %s %s [%s] %s',
        [TypeToIcon(DS.FieldByName('notification_type').AsString),
         IfThen(DS.FieldByName('is_read').AsInteger = 0, '*', ' '),
         DS.FieldByName('category_name').AsString,
         DS.FieldByName('title').AsString]));
      if DS.FieldByName('priority').AsInteger >= 2 then
        Write(Format(' (%s)', [PriorityToString(DS.FieldByName('priority').AsInteger)]));
      WriteLn('');
      WriteLn(Format('       %s', [DS.FieldByName('body').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   ' + StringOfChar('-', 70));
  WriteLn('   Legend: * = unread, [i]=info, [+]=success, [!]=warning, [X]=error');
  WriteLn('');
end;

// =============================================================================
// Notification Filtering
// =============================================================================
{ Queries notifications filtered by security category, high priority threshold, and unread status, displaying results grouped by user. }
procedure DemoFiltering;
var
  DS: TDataSet;
begin
  WriteLn('4. Notification Filtering');
  WriteLn('   ========================');
  WriteLn('');

  // By category
  WriteLn('   All security notifications (all users):');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, n.title, n.notification_type, n.priority ' +
    'FROM notifications n ' +
    'JOIN users u ON n.user_id = u.id ' +
    'JOIN notification_categories nc ON n.category_id = nc.id ' +
    'WHERE nc.name = ''security'' AND n.is_deleted = 0 ' +
    'ORDER BY n.priority DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s [%s]: %s (%s)',
        [TypeToIcon(DS.FieldByName('notification_type').AsString),
         DS.FieldByName('display_name').AsString,
         DS.FieldByName('title').AsString,
         PriorityToString(DS.FieldByName('priority').AsInteger)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // High priority only
  WriteLn('');
  WriteLn('   High priority notifications (priority >= 2):');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, n.title, n.notification_type, n.priority, nc.name AS category ' +
    'FROM notifications n ' +
    'JOIN users u ON n.user_id = u.id ' +
    'JOIN notification_categories nc ON n.category_id = nc.id ' +
    'WHERE n.priority >= 2 AND n.is_deleted = 0 ' +
    'ORDER BY n.priority DESC, n.created_at DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s [%s] %s: %s',
        [PriorityToString(DS.FieldByName('priority').AsInteger),
         DS.FieldByName('category').AsString,
         DS.FieldByName('display_name').AsString,
         DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Unread only
  WriteLn('');
  WriteLn('   Unread notifications by user:');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, COUNT(*) AS unread, ' +
    '  GROUP_CONCAT(DISTINCT nc.name) AS categories ' +
    'FROM notifications n ' +
    'JOIN users u ON n.user_id = u.id ' +
    'JOIN notification_categories nc ON n.category_id = nc.id ' +
    'WHERE n.is_read = 0 AND n.is_deleted = 0 ' +
    'GROUP BY n.user_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d unread (%s)',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('unread').AsInteger,
         DS.FieldByName('categories').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Notification Grouping
// =============================================================================
{ Aggregates notifications by category showing totals and unread counts, and groups them by group_key to display related notification clusters. }
procedure DemoGrouping;
var
  DS: TDataSet;
begin
  WriteLn('5. Notification Grouping');
  WriteLn('   =======================');
  WriteLn('');

  // Group by category
  WriteLn('   Notifications by category:');
  DS := Conn.ExecuteQuery(
    'SELECT nc.name, nc.color, COUNT(*) AS total, ' +
    '  SUM(CASE WHEN n.is_read = 0 THEN 1 ELSE 0 END) AS unread ' +
    'FROM notifications n ' +
    'JOIN notification_categories nc ON n.category_id = nc.id ' +
    'WHERE n.is_deleted = 0 ' +
    'GROUP BY nc.id ' +
    'ORDER BY unread DESC, total DESC');
  try
    WriteLn('   Category   | Total | Unread | Color');
    WriteLn('   -----------|-------|--------|--------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s | %5d | %6d | %s',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('total').AsInteger,
         DS.FieldByName('unread').AsInteger,
         DS.FieldByName('color').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Group by key (e.g., order updates)
  WriteLn('');
  WriteLn('   Grouped notifications (by group_key):');
  DS := Conn.ExecuteQuery(
    'SELECT group_key, COUNT(*) AS count, GROUP_CONCAT(title, '' | '') AS titles ' +
    'FROM notifications ' +
    'WHERE group_key IS NOT NULL AND group_key != '''' AND is_deleted = 0 ' +
    'GROUP BY group_key ' +
    'HAVING COUNT(*) >= 1 ' +
    'ORDER BY MAX(created_at) DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     Group "%s" (%d):',
        [DS.FieldByName('group_key').AsString,
         DS.FieldByName('count').AsInteger]));
      WriteLn(Format('       %s', [DS.FieldByName('titles').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Push Queue Status
// =============================================================================
{ Lists all push queue entries with their channel, status, and associated notification title, plus a summary of queue counts grouped by channel and status. }
procedure DemoPushQueue;
var
  DS: TDataSet;
begin
  WriteLn('6. Push Notification Queue');
  WriteLn('   =========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT pq.*, n.title ' +
    'FROM push_queue pq ' +
    'JOIN notifications n ON pq.notification_id = n.id ' +
    'ORDER BY pq.scheduled_for');
  try
    WriteLn('   ID | Channel | Status  | Notification');
    WriteLn('   ---|---------|---------|----------------------------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %2d | %-7s | %-7s | %s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('channel').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('title').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Queue summary
  WriteLn('');
  WriteLn('   Queue summary by channel:');
  DS := Conn.ExecuteQuery(
    'SELECT channel, status, COUNT(*) AS count ' +
    'FROM push_queue ' +
    'GROUP BY channel, status ' +
    'ORDER BY channel, status');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s [%s]: %d',
        [DS.FieldByName('channel').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Batch Operations
// =============================================================================
{ Marks all of Alice's notifications as read, archives low-priority read notifications, and cancels pending push deliveries for low-priority notifications. }
procedure DemoBatchOperations;
var
  Affected: Integer;
begin
  WriteLn('7. Batch Operations');
  WriteLn('   ==================');
  WriteLn('');

  // Mark all as read for Alice
  WriteLn('   Before: Alice has ' +
    IntToStr(Conn.ExecuteScalar('SELECT unread_count FROM users WHERE id = 1')) + ' unread');
  MarkAllAsRead(1);
  WriteLn('   After "Mark All as Read": Alice has ' +
    IntToStr(Conn.ExecuteScalar('SELECT unread_count FROM users WHERE id = 1')) + ' unread');

  // Delete old notifications (simulated - would use date comparison)
  WriteLn('');
  WriteLn('   Archiving low-priority read notifications...');
  Affected := Conn.ExecuteNonQuery(
    'UPDATE notifications SET is_archived = 1 ' +
    'WHERE is_read = 1 AND priority = 0 AND is_archived = 0');
  WriteLn(Format('   Archived %d notifications', [Affected]));

  // Cancel pending low-priority push notifications
  WriteLn('');
  WriteLn('   Cancelling pending low-priority push notifications...');
  Affected := Conn.ExecuteNonQuery(
    'UPDATE push_queue SET status = ''cancelled'' ' +
    'WHERE status = ''pending'' AND notification_id IN ' +
    '  (SELECT id FROM notifications WHERE priority = 0)');
  WriteLn(Format('   Cancelled %d push notifications', [Affected]));

  WriteLn('');
end;

// =============================================================================
// User Preferences
// =============================================================================
{ Displays a table of user notification preferences showing email, push, and in-app channel settings along with minimum priority thresholds per category. }
procedure DemoUserPreferences;
var
  DS: TDataSet;
begin
  WriteLn('8. User Notification Preferences');
  WriteLn('   ===============================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, nc.name AS category, ' +
    '  up.email_enabled, up.push_enabled, up.in_app_enabled, up.minimum_priority ' +
    'FROM user_preferences up ' +
    'JOIN users u ON up.user_id = u.id ' +
    'JOIN notification_categories nc ON up.category_id = nc.id ' +
    'ORDER BY u.display_name');
  try
    WriteLn('   User        | Category   | Email | Push | In-App | Min Priority');
    WriteLn('   ------------|------------|-------|------|--------|-------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-11s | %-10s | %-5s | %-4s | %-6s | %s',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('category').AsString,
         IfThen(DS.FieldByName('email_enabled').AsInteger = 1, 'Yes', 'No'),
         IfThen(DS.FieldByName('push_enabled').AsInteger = 1, 'Yes', 'No'),
         IfThen(DS.FieldByName('in_app_enabled').AsInteger = 1, 'Yes', 'No'),
         PriorityToString(DS.FieldByName('minimum_priority').AsInteger)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Statistics
// =============================================================================
{ Queries and displays overall notification counts, pending push queue size, and breakdowns by notification type and priority level. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('9. Notification Statistics');
  WriteLn('   =========================');
  WriteLn('');

  WriteLn('   Overall statistics:');
  WriteLn(Format('     Total notifications: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM notifications WHERE is_deleted = 0'))]));
  WriteLn(Format('     Total unread: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM notifications WHERE is_read = 0 AND is_deleted = 0'))]));
  WriteLn(Format('     Push queue pending: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM push_queue WHERE status = ''pending'''))]));

  // By type
  WriteLn('');
  WriteLn('   By notification type:');
  DS := Conn.ExecuteQuery(
    'SELECT notification_type, COUNT(*) AS count ' +
    'FROM notifications WHERE is_deleted = 0 ' +
    'GROUP BY notification_type');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s %s: %d',
        [TypeToIcon(DS.FieldByName('notification_type').AsString),
         DS.FieldByName('notification_type').AsString,
         DS.FieldByName('count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // By priority
  WriteLn('');
  WriteLn('   By priority:');
  DS := Conn.ExecuteQuery(
    'SELECT priority, COUNT(*) AS count ' +
    'FROM notifications WHERE is_deleted = 0 ' +
    'GROUP BY priority ORDER BY priority');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d',
        [PriorityToString(DS.FieldByName('priority').AsInteger),
         DS.FieldByName('count').AsInteger]));
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
  WriteLn('=== NDXSQLite Example 78: Notification System ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;
    DemoNotificationInbox;
    DemoFiltering;
    DemoGrouping;
    DemoPushQueue;
    DemoBatchOperations;
    DemoUserPreferences;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
