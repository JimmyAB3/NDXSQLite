{===============================================================================
  NDXSQLite Example 104 - Chat Messaging
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Conversation and message management
  - Read receipts and unread counts
  - Typing indicators and online status
  - Message history with pagination
  - Chat statistics and analytics

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ChatMessaging;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Users table
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id TEXT PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  display_name TEXT NOT NULL,' +
    '  status TEXT DEFAULT ''offline'',' +
    '  last_seen_at TEXT' +
    ')');

  // Conversations table (direct or group)
  Conn.ExecuteNonQuery(
    'CREATE TABLE conversations (' +
    '  id TEXT PRIMARY KEY,' +
    '  conv_type TEXT NOT NULL,' +  // 'direct' or 'group'
    '  title TEXT,' +
    '  created_at TEXT NOT NULL,' +
    '  created_by TEXT REFERENCES users(id)' +
    ')');

  // Conversation members
  Conn.ExecuteNonQuery(
    'CREATE TABLE conversation_members (' +
    '  conversation_id TEXT REFERENCES conversations(id),' +
    '  user_id TEXT REFERENCES users(id),' +
    '  joined_at TEXT NOT NULL,' +
    '  member_role TEXT DEFAULT ''member'',' +
    '  PRIMARY KEY (conversation_id, user_id)' +
    ')');

  // Messages
  Conn.ExecuteNonQuery(
    'CREATE TABLE messages (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  conversation_id TEXT REFERENCES conversations(id),' +
    '  sender_id TEXT REFERENCES users(id),' +
    '  content TEXT NOT NULL,' +
    '  message_type TEXT DEFAULT ''text'',' +
    '  sent_at TEXT NOT NULL,' +
    '  edited_at TEXT' +
    ')');

  // Read receipts
  Conn.ExecuteNonQuery(
    'CREATE TABLE read_receipts (' +
    '  message_id INTEGER REFERENCES messages(id),' +
    '  user_id TEXT REFERENCES users(id),' +
    '  read_at TEXT NOT NULL,' +
    '  PRIMARY KEY (message_id, user_id)' +
    ')');

  // Typing indicators
  Conn.ExecuteNonQuery(
    'CREATE TABLE typing_indicators (' +
    '  conversation_id TEXT REFERENCES conversations(id),' +
    '  user_id TEXT REFERENCES users(id),' +
    '  started_at TEXT NOT NULL,' +
    '  expires_at TEXT NOT NULL,' +
    '  PRIMARY KEY (conversation_id, user_id)' +
    ')');

  // Indexes for performance
  Conn.ExecuteNonQuery('CREATE INDEX idx_messages_conv ON messages(conversation_id, sent_at)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_read_receipts_msg ON read_receipts(message_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_typing_conv ON typing_indicators(conversation_id, expires_at)');
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  // Create users
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (''u1'', ''alice'', ''Alice Johnson'', ''online'', ''2024-03-01 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (''u2'', ''bob'', ''Bob Smith'', ''online'', ''2024-03-01 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (''u3'', ''carol'', ''Carol White'', ''away'', ''2024-03-01 09:45:00'')');
  Conn.ExecuteNonQuery('INSERT INTO users VALUES (''u4'', ''dave'', ''Dave Brown'', ''offline'', ''2024-02-28 18:30:00'')');

  // Create a direct conversation between Alice and Bob
  Conn.ExecuteNonQuery('INSERT INTO conversations VALUES (''conv1'', ''direct'', NULL, ''2024-03-01 08:00:00'', ''u1'')');
  Conn.ExecuteNonQuery('INSERT INTO conversation_members VALUES (''conv1'', ''u1'', ''2024-03-01 08:00:00'', ''member'')');
  Conn.ExecuteNonQuery('INSERT INTO conversation_members VALUES (''conv1'', ''u2'', ''2024-03-01 08:00:00'', ''member'')');

  // Create a group conversation: "Project Alpha"
  Conn.ExecuteNonQuery('INSERT INTO conversations VALUES (''conv2'', ''group'', ''Project Alpha'', ''2024-03-01 08:30:00'', ''u1'')');
  Conn.ExecuteNonQuery('INSERT INTO conversation_members VALUES (''conv2'', ''u1'', ''2024-03-01 08:30:00'', ''admin'')');
  Conn.ExecuteNonQuery('INSERT INTO conversation_members VALUES (''conv2'', ''u2'', ''2024-03-01 08:30:00'', ''member'')');
  Conn.ExecuteNonQuery('INSERT INTO conversation_members VALUES (''conv2'', ''u3'', ''2024-03-01 08:30:00'', ''member'')');
  Conn.ExecuteNonQuery('INSERT INTO conversation_members VALUES (''conv2'', ''u4'', ''2024-03-01 08:35:00'', ''member'')');

  // Messages in direct conversation (Alice <-> Bob)
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv1'', ''u1'', ''Hey Bob, did you review the PR?'', ''2024-03-01 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv1'', ''u2'', ''Yes, looks good! Just one minor comment.'', ''2024-03-01 09:02:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv1'', ''u1'', ''Great, I will fix it now.'', ''2024-03-01 09:03:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv1'', ''u2'', ''Perfect, let me know when it is ready.'', ''2024-03-01 09:05:00'')');

  // Messages in group conversation (Project Alpha)
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u1'', ''Team, the sprint starts today.'', ''2024-03-01 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u2'', ''Ready to go!'', ''2024-03-01 09:01:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u3'', ''I will start on the backend tasks.'', ''2024-03-01 09:05:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u1'', ''Carol, please sync with Dave on the API design.'', ''2024-03-01 09:10:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u4'', ''Will do, I have some notes from last week.'', ''2024-03-01 09:15:00'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u3'', ''Let us meet at 2pm to discuss.'', ''2024-03-01 09:20:00'')');

  // Read receipts: Alice and Bob have read their direct messages
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (1, ''u2'', ''2024-03-01 09:01:00'')');  // Bob read msg 1
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (2, ''u1'', ''2024-03-01 09:02:30'')');  // Alice read msg 2
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (3, ''u2'', ''2024-03-01 09:03:30'')');  // Bob read msg 3
  // Bob's msg 4 not yet read by Alice

  // Group read receipts: partial reads
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (5, ''u2'', ''2024-03-01 09:01:30'')');  // Bob read msg 5
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (5, ''u3'', ''2024-03-01 09:02:00'')');  // Carol read msg 5
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (6, ''u1'', ''2024-03-01 09:01:30'')');  // Alice read msg 6
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (6, ''u3'', ''2024-03-01 09:02:00'')');  // Carol read msg 6
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (7, ''u1'', ''2024-03-01 09:05:30'')');  // Alice read msg 7
  Conn.ExecuteNonQuery('INSERT INTO read_receipts VALUES (7, ''u2'', ''2024-03-01 09:06:00'')');  // Bob read msg 7
  // Messages 8-10 have fewer reads (Dave hasn't read most)
end;

// === Feature 1: Conversations List ===
{ Queries and prints Alice's conversations showing type, display title, member count, last message content, and last activity timestamp. }
procedure DemoConversationsList;
begin
  WriteLn('=== 1. Conversations List (for Alice) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT c.id, c.conv_type, ' +
    '  CASE WHEN c.conv_type = ''direct'' THEN u2.display_name ELSE c.title END as display_title, ' +
    '  (SELECT COUNT(*) FROM conversation_members WHERE conversation_id = c.id) as member_count, ' +
    '  (SELECT content FROM messages WHERE conversation_id = c.id ORDER BY sent_at DESC LIMIT 1) as last_message, ' +
    '  (SELECT sent_at FROM messages WHERE conversation_id = c.id ORDER BY sent_at DESC LIMIT 1) as last_activity ' +
    'FROM conversations c ' +
    'JOIN conversation_members cm ON cm.conversation_id = c.id AND cm.user_id = ''u1'' ' +
    'LEFT JOIN conversation_members cm2 ON cm2.conversation_id = c.id AND cm2.user_id != ''u1'' AND c.conv_type = ''direct'' ' +
    'LEFT JOIN users u2 ON u2.id = cm2.user_id ' +
    'ORDER BY last_activity DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%s] %s (%d members)',
        [DS.FieldByName('conv_type').AsString,
         DS.FieldByName('display_title').AsString,
         DS.FieldByName('member_count').AsInteger]));
      WriteLn(Format('      Last: "%s" at %s',
        [DS.FieldByName('last_message').AsString,
         DS.FieldByName('last_activity').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 2: Send Messages ===
{ Inserts new messages from Alice and Bob into the group conversation, then updates one message to show the edit capability. }
procedure DemoSendMessages;
var
  MsgId: Variant;
begin
  WriteLn('=== 2. Send Messages ===');
  WriteLn;

  // Alice sends a new message to the group
  Conn.ExecuteNonQuery(
    'INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u1'', ''Reminder: standup at 10am!'', ''2024-03-01 09:55:00'')');

  MsgId := Conn.ExecuteScalar('SELECT last_insert_rowid()');
  WriteLn(Format('   Alice sent message #%d to Project Alpha', [Integer(MsgId)]));

  // Bob replies
  Conn.ExecuteNonQuery(
    'INSERT INTO messages (conversation_id, sender_id, content, sent_at) ' +
    'VALUES (''conv2'', ''u2'', ''On my way!'', ''2024-03-01 09:56:00'')');

  MsgId := Conn.ExecuteScalar('SELECT last_insert_rowid()');
  WriteLn(Format('   Bob sent message #%d to Project Alpha', [Integer(MsgId)]));

  // Edit a message
  Conn.ExecuteNonQuery(
    'UPDATE messages SET content = ''Reminder: standup at 10:30am!'', edited_at = ''2024-03-01 09:57:00'' ' +
    'WHERE id = 11');
  WriteLn('   Alice edited message #11 (changed time to 10:30am)');

  WriteLn;
end;

// === Feature 3: Message History ===
{ Queries and prints all messages in Project Alpha conversation showing timestamp, sender, content, and edit indicator. }
procedure DemoMessageHistory;
begin
  WriteLn('=== 3. Message History (Project Alpha, page 1) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT m.id, u.display_name as sender, m.content, m.sent_at, ' +
    '  CASE WHEN m.edited_at IS NOT NULL THEN ''(edited)'' ELSE '''' END as edited ' +
    'FROM messages m ' +
    'JOIN users u ON u.id = m.sender_id ' +
    'WHERE m.conversation_id = ''conv2'' ' +
    'ORDER BY m.sent_at ASC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [%s] %s: %s %s',
        [DS.FieldByName('sent_at').AsString,
         DS.FieldByName('sender').AsString,
         DS.FieldByName('content').AsString,
         DS.FieldByName('edited').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 4: Read Receipts ===
{ Marks a message as read, prints direct chat messages with who read them, and prints group chat read counts vs total recipients. }
procedure DemoReadReceipts;
begin
  WriteLn('=== 4. Read Receipts ===');
  WriteLn;

  // Mark Alice as having read Bob's last message in direct chat
  Conn.ExecuteNonQuery(
    'INSERT OR IGNORE INTO read_receipts VALUES (4, ''u1'', ''2024-03-01 09:30:00'')');
  WriteLn('   Alice reads message #4 in direct chat');

  // Show read status for direct conversation
  WriteLn;
  WriteLn('   Direct chat (Alice <-> Bob) read status:');
  DS := Conn.ExecuteQuery(
    'SELECT m.id, m.content, u.display_name as sender, ' +
    '  (SELECT GROUP_CONCAT(u2.username, '', '') FROM read_receipts rr ' +
    '   JOIN users u2 ON u2.id = rr.user_id ' +
    '   WHERE rr.message_id = m.id) as read_by ' +
    'FROM messages m ' +
    'JOIN users u ON u.id = m.sender_id ' +
    'WHERE m.conversation_id = ''conv1'' ' +
    'ORDER BY m.sent_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d [%s]: "%s" - Read by: %s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('sender').AsString,
         DS.FieldByName('content').AsString,
         DS.FieldByName('read_by').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show read receipts for group messages
  WriteLn;
  WriteLn('   Group chat read counts:');
  DS := Conn.ExecuteQuery(
    'SELECT m.id, m.content, ' +
    '  (SELECT COUNT(*) FROM read_receipts WHERE message_id = m.id) as read_count, ' +
    '  (SELECT COUNT(*) FROM conversation_members WHERE conversation_id = m.conversation_id AND user_id != m.sender_id) as total_recipients ' +
    'FROM messages m ' +
    'WHERE m.conversation_id = ''conv2'' ' +
    'ORDER BY m.sent_at ' +
    'LIMIT 6');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d "%s" - %d/%d read',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('content').AsString,
         DS.FieldByName('read_count').AsInteger,
         DS.FieldByName('total_recipients').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 5: Unread Count ===
{ Calculates and prints the unread message count per conversation for Dave and Bob by finding messages without read receipts. }
procedure DemoUnreadCount;
begin
  WriteLn('=== 5. Unread Count Per Conversation ===');
  WriteLn;

  // For Dave: count messages he hasn't read in each conversation
  DS := Conn.ExecuteQuery(
    'SELECT c.id, ' +
    '  CASE WHEN c.conv_type = ''direct'' THEN ''DM'' ELSE c.title END as conv_name, ' +
    '  COUNT(m.id) as unread_count ' +
    'FROM conversations c ' +
    'JOIN conversation_members cm ON cm.conversation_id = c.id AND cm.user_id = ''u4'' ' +
    'JOIN messages m ON m.conversation_id = c.id AND m.sender_id != ''u4'' ' +
    'LEFT JOIN read_receipts rr ON rr.message_id = m.id AND rr.user_id = ''u4'' ' +
    'WHERE rr.message_id IS NULL ' +
    'GROUP BY c.id');
  try
    WriteLn('   Dave''s unread messages:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d unread',
        [DS.FieldByName('conv_name').AsString,
         DS.FieldByName('unread_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // For Bob: unread count
  DS := Conn.ExecuteQuery(
    'SELECT c.id, ' +
    '  CASE WHEN c.conv_type = ''direct'' THEN ''DM with Alice'' ELSE c.title END as conv_name, ' +
    '  COUNT(m.id) as unread_count ' +
    'FROM conversations c ' +
    'JOIN conversation_members cm ON cm.conversation_id = c.id AND cm.user_id = ''u2'' ' +
    'JOIN messages m ON m.conversation_id = c.id AND m.sender_id != ''u2'' ' +
    'LEFT JOIN read_receipts rr ON rr.message_id = m.id AND rr.user_id = ''u2'' ' +
    'WHERE rr.message_id IS NULL ' +
    'GROUP BY c.id');
  try
    WriteLn;
    WriteLn('   Bob''s unread messages:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d unread',
        [DS.FieldByName('conv_name').AsString,
         DS.FieldByName('unread_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 6: Typing Indicators ===
{ Creates typing indicator records with expiry times, queries who is typing before and after expiry, and cleans up expired indicators. }
procedure DemoTypingIndicators;
begin
  WriteLn('=== 6. Typing Indicators ===');
  WriteLn;

  // Carol starts typing in group chat
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO typing_indicators VALUES ' +
    '(''conv2'', ''u3'', ''2024-03-01 10:00:00'', ''2024-03-01 10:00:05'')');
  WriteLn('   Carol started typing in Project Alpha');

  // Bob starts typing in group chat
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO typing_indicators VALUES ' +
    '(''conv2'', ''u2'', ''2024-03-01 10:00:01'', ''2024-03-01 10:00:06'')');
  WriteLn('   Bob started typing in Project Alpha');

  // Alice starts typing in direct chat
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO typing_indicators VALUES ' +
    '(''conv1'', ''u1'', ''2024-03-01 10:00:02'', ''2024-03-01 10:00:07'')');
  WriteLn('   Alice started typing in DM with Bob');

  // Check who is typing in conv2 (at time 10:00:03, before expiry)
  WriteLn;
  WriteLn('   Who is typing in Project Alpha (at 10:00:03)?');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name ' +
    'FROM typing_indicators ti ' +
    'JOIN users u ON u.id = ti.user_id ' +
    'WHERE ti.conversation_id = ''conv2'' ' +
    '  AND ti.expires_at > ''2024-03-01 10:00:03'' ' +
    'ORDER BY ti.started_at');
  try
    while not DS.EOF do
    begin
      WriteLn('   - ' + DS.FieldByName('display_name').AsString + ' is typing...');
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Check after expiry
  WriteLn;
  WriteLn('   Who is typing in Project Alpha (at 10:00:10)?');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name ' +
    'FROM typing_indicators ti ' +
    'JOIN users u ON u.id = ti.user_id ' +
    'WHERE ti.conversation_id = ''conv2'' ' +
    '  AND ti.expires_at > ''2024-03-01 10:00:10'' ' +
    'ORDER BY ti.started_at');
  try
    if DS.EOF then
      WriteLn('   (no one is typing)')
    else
      while not DS.EOF do
      begin
        WriteLn('   - ' + DS.FieldByName('display_name').AsString + ' is typing...');
        DS.Next;
      end;
  finally
    DS.Free;
  end;

  // Clear expired typing indicators
  Conn.ExecuteNonQuery(
    'DELETE FROM typing_indicators WHERE expires_at <= ''2024-03-01 10:00:10''');
  WriteLn;
  WriteLn('   Cleared expired typing indicators');
  WriteLn;
end;

// === Feature 7: Online Status ===
{ Updates Dave's status to online, prints all user statuses with last seen times, and lists Project Alpha members sorted by online status. }
procedure DemoOnlineStatus;
begin
  WriteLn('=== 7. User Status & Presence ===');
  WriteLn;

  // Update Dave's status to online
  Conn.ExecuteNonQuery(
    'UPDATE users SET status = ''online'', last_seen_at = ''2024-03-01 10:01:00'' WHERE id = ''u4''');
  WriteLn('   Dave came online');

  // Show all user statuses
  DS := Conn.ExecuteQuery(
    'SELECT display_name, status, last_seen_at FROM users ORDER BY display_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s [%-7s] last seen: %s',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('last_seen_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show members online in a conversation
  WriteLn;
  WriteLn('   Project Alpha members online:');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, u.status ' +
    'FROM conversation_members cm ' +
    'JOIN users u ON u.id = cm.user_id ' +
    'WHERE cm.conversation_id = ''conv2'' ' +
    'ORDER BY CASE u.status WHEN ''online'' THEN 0 WHEN ''away'' THEN 1 ELSE 2 END, u.display_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s (%s)',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 8: Statistics ===
{ Demonstrates chat messaging statistics including users, conversations, and messages. }
procedure DemoStatistics;
var
  TotalMsg, TotalConv, TotalUsers: Integer;
begin
  WriteLn('=== 8. Chat Statistics ===');
  WriteLn;

  TotalUsers := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM users'));
  TotalConv := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM conversations'));
  TotalMsg := Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM messages'));

  WriteLn(Format('   Total users: %d', [TotalUsers]));
  WriteLn(Format('   Total conversations: %d', [TotalConv]));
  WriteLn(Format('   Total messages: %d', [TotalMsg]));

  // Messages per user
  WriteLn;
  WriteLn('   Messages per user:');
  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, COUNT(m.id) as msg_count ' +
    'FROM users u ' +
    'LEFT JOIN messages m ON m.sender_id = u.id ' +
    'GROUP BY u.id ' +
    'ORDER BY msg_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %d messages',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('msg_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Messages per conversation
  WriteLn;
  WriteLn('   Messages per conversation:');
  DS := Conn.ExecuteQuery(
    'SELECT CASE WHEN c.conv_type = ''direct'' THEN ''DM (Alice/Bob)'' ELSE c.title END as conv_name, ' +
    '  COUNT(m.id) as msg_count ' +
    'FROM conversations c ' +
    'JOIN messages m ON m.conversation_id = c.id ' +
    'GROUP BY c.id ' +
    'ORDER BY msg_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %d messages',
        [DS.FieldByName('conv_name').AsString,
         DS.FieldByName('msg_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Read receipt coverage
  WriteLn;
  WriteLn('   Read receipt coverage:');
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM read_receipts) as total_reads, ' +
    '  (SELECT COUNT(*) FROM messages) as total_messages, ' +
    '  CAST((SELECT COUNT(*) FROM read_receipts) AS REAL) / ' +
    '    CASE WHEN (SELECT COUNT(*) FROM messages) = 0 THEN 1 ' +
    '    ELSE (SELECT COUNT(*) FROM messages) END * 100 as read_percentage');
  try
    WriteLn(Format('   Total read receipts: %d',
      [DS.FieldByName('total_reads').AsInteger]));
    WriteLn(Format('   Avg reads per message: %.1f%%',
      [DS.FieldByName('read_percentage').AsFloat]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Main Program ===
begin
  WriteLn('=== Example 104: Chat Messaging System ===');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    SeedData;

    DemoConversationsList;
    DemoSendMessages;
    DemoMessageHistory;
    DemoReadReceipts;
    DemoUnreadCount;
    DemoTypingIndicators;
    DemoOnlineStatus;
    DemoStatistics;

    WriteLn('=== All chat messaging features demonstrated successfully ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
