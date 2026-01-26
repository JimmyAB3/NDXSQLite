{===============================================================================
  NDXSQLite Example 142 - PubSub Database
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Channel and subscription management
  - Message publishing with ordering guarantees
  - Subscriber polling and acknowledgment
  - Dead-letter queue for failed messages
  - Fan-out delivery and message filtering

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program PubSubDatabase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Channels (topics)
  Conn.ExecuteNonQuery(
    'CREATE TABLE channels (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  name TEXT NOT NULL UNIQUE, ' +
    '  description TEXT, ' +
    '  max_retention_sec INTEGER DEFAULT 3600, ' +
    '  created_at TEXT NOT NULL)');

  // Messages published to channels
  Conn.ExecuteNonQuery(
    'CREATE TABLE messages (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  channel_id INTEGER NOT NULL, ' +
    '  sequence_num INTEGER NOT NULL, ' +
    '  payload TEXT NOT NULL, ' +
    '  priority INTEGER NOT NULL DEFAULT 0, ' +
    '  published_at TEXT NOT NULL, ' +
    '  expires_at TEXT, ' +
    '  publisher TEXT NOT NULL, ' +
    '  UNIQUE(channel_id, sequence_num))');

  // Subscribers
  Conn.ExecuteNonQuery(
    'CREATE TABLE subscribers (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  name TEXT NOT NULL UNIQUE, ' +
    '  filter_pattern TEXT, ' +
    '  created_at TEXT NOT NULL)');

  // Subscriptions (subscriber -> channel binding)
  Conn.ExecuteNonQuery(
    'CREATE TABLE subscriptions (' +
    '  subscriber_id INTEGER NOT NULL, ' +
    '  channel_id INTEGER NOT NULL, ' +
    '  last_ack_seq INTEGER NOT NULL DEFAULT 0, ' +
    '  subscribed_at TEXT NOT NULL, ' +
    '  status TEXT NOT NULL DEFAULT ''active'', ' +
    '  PRIMARY KEY(subscriber_id, channel_id))');

  // Delivery tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE deliveries (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  message_id INTEGER NOT NULL, ' +
    '  subscriber_id INTEGER NOT NULL, ' +
    '  status TEXT NOT NULL DEFAULT ''pending'', ' +
    '  delivered_at TEXT, ' +
    '  acknowledged_at TEXT, ' +
    '  retry_count INTEGER NOT NULL DEFAULT 0, ' +
    '  UNIQUE(message_id, subscriber_id))');

  // Dead letter queue
  Conn.ExecuteNonQuery(
    'CREATE TABLE dead_letters (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  message_id INTEGER NOT NULL, ' +
    '  subscriber_id INTEGER NOT NULL, ' +
    '  channel_name TEXT NOT NULL, ' +
    '  reason TEXT NOT NULL, ' +
    '  retry_count INTEGER NOT NULL, ' +
    '  original_payload TEXT NOT NULL, ' +
    '  dead_at TEXT NOT NULL)');
end;

{ Inserts sample data into tables. }
procedure InsertData;
var
  I, Seq: Integer;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Channels
  Conn.ExecuteNonQuery('INSERT INTO channels (name, description, max_retention_sec, created_at) VALUES ' +
    '(''orders'', ''New order events'', 7200, ''2025-01-20 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO channels (name, description, max_retention_sec, created_at) VALUES ' +
    '(''payments'', ''Payment confirmations'', 7200, ''2025-01-20 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO channels (name, description, max_retention_sec, created_at) VALUES ' +
    '(''notifications'', ''User notifications'', 3600, ''2025-01-20 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO channels (name, description, max_retention_sec, created_at) VALUES ' +
    '(''logs'', ''System log events'', 1800, ''2025-01-20 09:00:00'')');

  // Subscribers
  Conn.ExecuteNonQuery('INSERT INTO subscribers (name, filter_pattern, created_at) VALUES ' +
    '(''order-processor'', NULL, ''2025-01-20 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO subscribers (name, filter_pattern, created_at) VALUES ' +
    '(''payment-gateway'', NULL, ''2025-01-20 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO subscribers (name, filter_pattern, created_at) VALUES ' +
    '(''email-service'', ''%urgent%'', ''2025-01-20 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO subscribers (name, filter_pattern, created_at) VALUES ' +
    '(''analytics'', NULL, ''2025-01-20 09:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO subscribers (name, filter_pattern, created_at) VALUES ' +
    '(''audit-logger'', NULL, ''2025-01-20 09:00:00'')');

  // Subscriptions
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (1, 1, 0, ''2025-01-20 09:01:00'', ''active'')');   // order-processor -> orders
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (2, 2, 0, ''2025-01-20 09:01:00'', ''active'')');   // payment-gateway -> payments
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (3, 1, 0, ''2025-01-20 09:01:00'', ''active'')');   // email-service -> orders
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (3, 3, 0, ''2025-01-20 09:01:00'', ''active'')');   // email-service -> notifications
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (4, 1, 0, ''2025-01-20 09:01:00'', ''active'')');   // analytics -> orders
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (4, 2, 0, ''2025-01-20 09:01:00'', ''active'')');   // analytics -> payments
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (4, 4, 0, ''2025-01-20 09:01:00'', ''active'')');   // analytics -> logs
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (5, 1, 0, ''2025-01-20 09:01:00'', ''active'')');   // audit-logger -> orders
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (5, 2, 0, ''2025-01-20 09:01:00'', ''active'')');   // audit-logger -> payments
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (5, 3, 0, ''2025-01-20 09:01:00'', ''active'')');   // audit-logger -> notifications
  Conn.ExecuteNonQuery('INSERT INTO subscriptions VALUES (5, 4, 0, ''2025-01-20 09:01:00'', ''active'')');   // audit-logger -> logs

  // Messages on orders channel
  Seq := 0;
  for I := 1 to 8 do
  begin
    Inc(Seq);
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO messages (channel_id, sequence_num, payload, priority, published_at, expires_at, publisher) VALUES ' +
      '(1, %d, ''{"order_id": %d, "amount": %.2f, "status": "%s"%s}'', %d, ''2025-01-20 10:%.2d:00'', ''2025-01-20 12:%.2d:00'', ''api-server'')',
      [Seq, 1000 + I,
       19.99 + I * 10,
       BoolToStr(I <= 6, 'new', 'cancelled'),
       BoolToStr(I = 3, ', "urgent": true', ''),
       Ord(I = 3),
       I * 2,
       I * 2]));
  end;

  // Messages on payments channel
  for I := 1 to 5 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO messages (channel_id, sequence_num, payload, priority, published_at, expires_at, publisher) VALUES ' +
      '(2, %d, ''{"payment_id": %d, "order_id": %d, "method": "%s"}'', 0, ''2025-01-20 10:%.2d:00'', ''2025-01-20 12:%.2d:00'', ''payment-svc'')',
      [I, 2000 + I, 1000 + I,
       BoolToStr(I mod 2 = 0, 'credit_card', 'paypal'),
       I * 3,
       I * 3]));
  end;

  // Messages on notifications
  Conn.ExecuteNonQuery('INSERT INTO messages (channel_id, sequence_num, payload, priority, published_at, expires_at, publisher) VALUES ' +
    '(3, 1, ''{"type": "welcome", "user": "alice"}'', 0, ''2025-01-20 10:05:00'', ''2025-01-20 11:05:00'', ''user-svc'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (channel_id, sequence_num, payload, priority, published_at, expires_at, publisher) VALUES ' +
    '(3, 2, ''{"type": "urgent", "user": "bob", "msg": "Account locked"}'', 1, ''2025-01-20 10:10:00'', ''2025-01-20 11:10:00'', ''auth-svc'')');
  Conn.ExecuteNonQuery('INSERT INTO messages (channel_id, sequence_num, payload, priority, published_at, expires_at, publisher) VALUES ' +
    '(3, 3, ''{"type": "promo", "campaign": "winter-sale"}'', 0, ''2025-01-20 10:15:00'', ''2025-01-20 11:15:00'', ''marketing'')');

  // Messages on logs
  for I := 1 to 6 do
  begin
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO messages (channel_id, sequence_num, payload, priority, published_at, expires_at, publisher) VALUES ' +
      '(4, %d, ''{"level": "%s", "service": "%s", "msg": "Event %d"}'', 0, ''2025-01-20 10:%.2d:00'', ''2025-01-20 10:%.2d:00'', ''sys'')',
      [I,
       BoolToStr(I mod 3 = 0, 'error', BoolToStr(I mod 3 = 1, 'info', 'warn')),
       BoolToStr(I <= 3, 'api', 'db'),
       I,
       I * 5,
       30 + I * 5]));
  end;

  // Deliveries (simulate processing)
  // order-processor: processed 6 of 8 orders
  for I := 1 to 6 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO deliveries (message_id, subscriber_id, status, delivered_at, acknowledged_at) VALUES ' +
      '(%d, 1, ''acknowledged'', ''2025-01-20 10:%.2d:01'', ''2025-01-20 10:%.2d:02'')', [I, I*2, I*2]));
  for I := 7 to 8 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO deliveries (message_id, subscriber_id, status, delivered_at) VALUES ' +
      '(%d, 1, ''delivered'', ''2025-01-20 10:%.2d:01'')', [I, I*2]));

  // email-service: processed some, failed some
  Conn.ExecuteNonQuery('INSERT INTO deliveries (message_id, subscriber_id, status, delivered_at, acknowledged_at) VALUES ' +
    '(3, 3, ''acknowledged'', ''2025-01-20 10:06:01'', ''2025-01-20 10:06:05'')'); // urgent order
  Conn.ExecuteNonQuery('INSERT INTO deliveries (message_id, subscriber_id, status, delivered_at, retry_count) VALUES ' +
    '(15, 3, ''failed'', ''2025-01-20 10:10:01'', 3)'); // notification failed

  // analytics: acknowledged all orders, lagging on payments
  for I := 1 to 8 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO deliveries (message_id, subscriber_id, status, delivered_at, acknowledged_at) VALUES ' +
      '(%d, 4, ''acknowledged'', ''2025-01-20 10:%.2d:01'', ''2025-01-20 10:%.2d:03'')', [I, I*2, I*2]));
  for I := 9 to 11 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO deliveries (message_id, subscriber_id, status, delivered_at) VALUES ' +
      '(%d, 4, ''delivered'', ''2025-01-20 10:%.2d:01'')', [I, (I-8)*3]));

  // audit-logger: acknowledged everything it received
  for I := 1 to 13 do
    Conn.ExecuteNonQuery(Format(
      'INSERT INTO deliveries (message_id, subscriber_id, status, delivered_at, acknowledged_at) VALUES ' +
      '(%d, 5, ''acknowledged'', ''2025-01-20 10:%.2d:01'', ''2025-01-20 10:%.2d:01'')', [I, I+1, I+1]));

  // Update last_ack_seq for subscribers
  Conn.ExecuteNonQuery('UPDATE subscriptions SET last_ack_seq = 6 WHERE subscriber_id = 1 AND channel_id = 1');
  Conn.ExecuteNonQuery('UPDATE subscriptions SET last_ack_seq = 1 WHERE subscriber_id = 3 AND channel_id = 1');
  Conn.ExecuteNonQuery('UPDATE subscriptions SET last_ack_seq = 8 WHERE subscriber_id = 4 AND channel_id = 1');
  Conn.ExecuteNonQuery('UPDATE subscriptions SET last_ack_seq = 3 WHERE subscriber_id = 4 AND channel_id = 2');
  Conn.ExecuteNonQuery('UPDATE subscriptions SET last_ack_seq = 8 WHERE subscriber_id = 5 AND channel_id = 1');
  Conn.ExecuteNonQuery('UPDATE subscriptions SET last_ack_seq = 5 WHERE subscriber_id = 5 AND channel_id = 2');
  Conn.ExecuteNonQuery('UPDATE subscriptions SET last_ack_seq = 0 WHERE subscriber_id = 5 AND channel_id = 3');

  // Dead letters
  Conn.ExecuteNonQuery('INSERT INTO dead_letters (message_id, subscriber_id, channel_name, reason, retry_count, original_payload, dead_at) VALUES ' +
    '(15, 3, ''notifications'', ''max_retries_exceeded'', 3, ''{"type": "urgent", "user": "bob", "msg": "Account locked"}'', ''2025-01-20 10:15:00'')');
  Conn.ExecuteNonQuery('INSERT INTO dead_letters (message_id, subscriber_id, channel_name, reason, retry_count, original_payload, dead_at) VALUES ' +
    '(17, 4, ''logs'', ''message_expired'', 0, ''{"level": "info", "service": "api", "msg": "Event 1"}'', ''2025-01-20 10:35:00'')');
  Conn.ExecuteNonQuery('INSERT INTO dead_letters (message_id, subscriber_id, channel_name, reason, retry_count, original_payload, dead_at) VALUES ' +
    '(18, 4, ''logs'', ''message_expired'', 0, ''{"level": "warn", "service": "api", "msg": "Event 2"}'', ''2025-01-20 10:40:00'')');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Lists all channels with descriptions and subscriber counts, then shows each subscriber's channel subscriptions and filter patterns. }
procedure Demo1_ChannelsAndSubs;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Channels and Subscriptions ===');
  WriteLn;
  WriteLn('   Channel          Description              Retention  Subscribers');
  WriteLn('   ' + StringOfChar('-', 70));
  DS := Conn.ExecuteQuery(
    'SELECT c.name, c.description, c.max_retention_sec, ' +
    '  (SELECT COUNT(*) FROM subscriptions WHERE channel_id = c.id AND status = ''active'') as subs ' +
    'FROM channels c ORDER BY c.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-24s %5d sec  %d', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('description').AsString,
        DS.FieldByName('max_retention_sec').AsInteger,
        DS.FieldByName('subs').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Subscriber        Channels                          Filter');
  WriteLn('   ' + StringOfChar('-', 65));
  DS := Conn.ExecuteQuery(
    'SELECT s.name, s.filter_pattern, ' +
    '  GROUP_CONCAT(c.name) as channels ' +
    'FROM subscribers s ' +
    'JOIN subscriptions sub ON sub.subscriber_id = s.id ' +
    'JOIN channels c ON c.id = sub.channel_id ' +
    'GROUP BY s.id ORDER BY s.id');
  try
    while not DS.EOF do
    begin
      if VarIsNull(DS.FieldByName('filter_pattern').Value) then
        WriteLn(Format('   %-16s %-34s (none)', [
          DS.FieldByName('name').AsString,
          DS.FieldByName('channels').AsString]))
      else
        WriteLn(Format('   %-16s %-34s %s', [
          DS.FieldByName('name').AsString,
          DS.FieldByName('channels').AsString,
          DS.FieldByName('filter_pattern').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates published message counts and timestamps per channel. }
procedure Demo2_Publishing;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Published Messages ===');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT c.name as channel, COUNT(*) as msgs, ' +
    '  MIN(m.published_at) as first_msg, MAX(m.published_at) as last_msg ' +
    'FROM messages m JOIN channels c ON c.id = m.channel_id ' +
    'GROUP BY m.channel_id ORDER BY c.name');
  try
    WriteLn('   Channel          Messages  First Published       Last Published');
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %5d     %s  %s', [
        DS.FieldByName('channel').AsString,
        DS.FieldByName('msgs').AsInteger,
        DS.FieldByName('first_msg').AsString,
        DS.FieldByName('last_msg').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Recent orders channel messages:');
  WriteLn('   Seq  Payload (truncated)                            Priority  Publisher');
  WriteLn('   ' + StringOfChar('-', 78));
  DS := Conn.ExecuteQuery(
    'SELECT sequence_num, substr(payload, 1, 45) as payload_short, priority, publisher ' +
    'FROM messages WHERE channel_id = 1 ORDER BY sequence_num LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %3d  %-45s %5d     %s', [
        DS.FieldByName('sequence_num').AsInteger,
        DS.FieldByName('payload_short').AsString,
        DS.FieldByName('priority').AsInteger,
        DS.FieldByName('publisher').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows orders channel messages sorted by priority descending then sequence ascending to illustrate priority-based FIFO delivery. }
procedure Demo3_MessageOrdering;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Message Ordering (FIFO with Priority) ===');
  WriteLn;
  WriteLn('   Orders channel - ordered by priority DESC, sequence ASC:');
  WriteLn;
  WriteLn('   Seq  Priority  Published            Payload');
  WriteLn('   ' + StringOfChar('-', 70));
  DS := Conn.ExecuteQuery(
    'SELECT sequence_num, priority, published_at, substr(payload, 1, 40) as p ' +
    'FROM messages WHERE channel_id = 1 ' +
    'ORDER BY priority DESC, sequence_num ASC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %3d  %5d     %s  %s', [
        DS.FieldByName('sequence_num').AsInteger,
        DS.FieldByName('priority').AsInteger,
        DS.FieldByName('published_at').AsString,
        DS.FieldByName('p').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   Priority 1 messages are delivered first (urgent orders)');
  WriteLn;
end;

{ Queries undelivered messages for order-processor by finding sequences beyond its last acknowledged position. }
procedure Demo4_SubscriberPolling;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Subscriber Polling (unread messages) ===');
  WriteLn;
  WriteLn('   order-processor: last_ack_seq=6, checking for new messages...');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT m.sequence_num, substr(m.payload, 1, 50) as payload, m.published_at ' +
    'FROM messages m ' +
    'WHERE m.channel_id = 1 AND m.sequence_num > 6 ' +
    'ORDER BY m.sequence_num');
  try
    WriteLn('   Seq  Payload                                             Published');
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %3d  %-50s  %s', [
        DS.FieldByName('sequence_num').AsInteger,
        DS.FieldByName('payload').AsString,
        DS.FieldByName('published_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   2 undelivered messages waiting for order-processor');
  WriteLn;
end;

{ Demonstrates message delivery and acknowledgment tracking. }
procedure Demo5_Acknowledgment;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Message Acknowledgment ===');
  WriteLn;
  WriteLn('   Delivery status for orders channel:');
  WriteLn;
  WriteLn('   Subscriber        Msg#  Status        Delivered            Acknowledged');
  WriteLn('   ' + StringOfChar('-', 80));
  DS := Conn.ExecuteQuery(
    'SELECT s.name, d.message_id, d.status, d.delivered_at, d.acknowledged_at ' +
    'FROM deliveries d ' +
    'JOIN subscribers s ON s.id = d.subscriber_id ' +
    'JOIN messages m ON m.id = d.message_id ' +
    'WHERE m.channel_id = 1 AND d.subscriber_id IN (1, 4) ' +
    'ORDER BY s.name, d.message_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %4d  %-12s  %s  %s', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('message_id').AsInteger,
        DS.FieldByName('status').AsString,
        DS.FieldByName('delivered_at').AsString,
        BoolToStr(VarIsNull(DS.FieldByName('acknowledged_at').Value), '-', DS.FieldByName('acknowledged_at').AsString)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists messages that failed delivery permanently from the dead_letters table with subscriber, channel, reason, and retry counts, then summarizes failure reasons. }
procedure Demo6_DeadLetters;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Dead Letter Queue ===');
  WriteLn;
  WriteLn('   Messages that failed delivery permanently:');
  WriteLn;
  WriteLn('   Subscriber        Channel        Reason                Retries  Dead At');
  WriteLn('   ' + StringOfChar('-', 80));
  DS := Conn.ExecuteQuery(
    'SELECT s.name, dl.channel_name, dl.reason, dl.retry_count, dl.dead_at ' +
    'FROM dead_letters dl ' +
    'JOIN subscribers s ON s.id = dl.subscriber_id ' +
    'ORDER BY dl.dead_at');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-14s %-20s %5d    %s', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('channel_name').AsString,
        DS.FieldByName('reason').AsString,
        DS.FieldByName('retry_count').AsInteger,
        DS.FieldByName('dead_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
  WriteLn('   Dead letter reasons:');
  DS := Conn.ExecuteQuery(
    'SELECT reason, COUNT(*) as cnt FROM dead_letters GROUP BY reason ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: %d messages', [
        DS.FieldByName('reason').AsString,
        DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows delivery status of message #3 to all subscribers and calculates fan-out ratio (subscribers times messages) for each channel. }
procedure Demo7_FanOut;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Fan-Out Delivery ===');
  WriteLn;
  WriteLn('   Message #3 (urgent order) delivered to all subscribers:');
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT s.name, d.status, d.delivered_at, d.acknowledged_at, d.retry_count ' +
    'FROM deliveries d ' +
    'JOIN subscribers s ON s.id = d.subscriber_id ' +
    'WHERE d.message_id = 3 ' +
    'ORDER BY s.name');
  try
    WriteLn('   Subscriber        Status        Delivered            Ack''d');
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-12s  %s  %s', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('status').AsString,
        DS.FieldByName('delivered_at').AsString,
        BoolToStr(VarIsNull(DS.FieldByName('acknowledged_at').Value), '-',
          DS.FieldByName('acknowledged_at').AsString)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Fan-out ratio per channel:');
  DS := Conn.ExecuteQuery(
    'SELECT c.name, ' +
    '  (SELECT COUNT(*) FROM subscriptions WHERE channel_id = c.id AND status = ''active'') as subs, ' +
    '  (SELECT COUNT(*) FROM messages WHERE channel_id = c.id) as msgs, ' +
    '  (SELECT COUNT(*) FROM subscriptions WHERE channel_id = c.id AND status = ''active'') * ' +
    '  (SELECT COUNT(*) FROM messages WHERE channel_id = c.id) as total_deliveries ' +
    'FROM channels c ORDER BY total_deliveries DESC');
  try
    WriteLn('   Channel          Subs  Messages  Total Deliveries');
    WriteLn('   ' + StringOfChar('-', 55));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %4d  %5d     %5d', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('subs').AsInteger,
        DS.FieldByName('msgs').AsInteger,
        DS.FieldByName('total_deliveries').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates content-based message filtering by subscriber rules. }
procedure Demo8_Filtering;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Content-Based Filtering ===');
  WriteLn;
  WriteLn('   email-service filter: "%urgent%"');
  WriteLn('   Only receives messages containing "urgent" in payload:');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT m.id, c.name as channel, substr(m.payload, 1, 50) as payload, m.priority ' +
    'FROM messages m ' +
    'JOIN channels c ON c.id = m.channel_id ' +
    'JOIN subscriptions sub ON sub.channel_id = m.channel_id AND sub.subscriber_id = 3 ' +
    'JOIN subscribers s ON s.id = 3 ' +
    'WHERE m.payload LIKE s.filter_pattern ' +
    'ORDER BY m.published_at');
  try
    WriteLn('   Ch               Payload (matching filter)                     Priority');
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-50s %d', [
        DS.FieldByName('channel').AsString,
        DS.FieldByName('payload').AsString,
        DS.FieldByName('priority').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(*) FROM messages m JOIN subscriptions sub ON sub.channel_id = m.channel_id ' +
    '   WHERE sub.subscriber_id = 3) as total_eligible, ' +
    '  (SELECT COUNT(*) FROM messages m JOIN subscriptions sub ON sub.channel_id = m.channel_id ' +
    '   JOIN subscribers s ON s.id = 3 ' +
    '   WHERE sub.subscriber_id = 3 AND m.payload LIKE s.filter_pattern) as filtered');
  try
    DS.First;
    WriteLn(Format('   Filtered: %d of %d eligible messages matched (%.0f%% reduction)', [
      DS.FieldByName('filtered').AsInteger,
      DS.FieldByName('total_eligible').AsInteger,
      100.0 - 100.0 * DS.FieldByName('filtered').AsInteger / DS.FieldByName('total_eligible').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Calculates lag (max sequence minus last acknowledged sequence) for each subscription to identify subscribers falling behind. }
procedure Demo9_SubscriptionLag;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Subscription Status and Lag ===');
  WriteLn;
  WriteLn('   Subscriber        Channel        Last Ack  Total Msgs  Lag');
  WriteLn('   ' + StringOfChar('-', 65));
  DS := Conn.ExecuteQuery(
    'SELECT s.name, c.name as channel, sub.last_ack_seq, ' +
    '  (SELECT MAX(sequence_num) FROM messages WHERE channel_id = c.id) as max_seq, ' +
    '  (SELECT MAX(sequence_num) FROM messages WHERE channel_id = c.id) - sub.last_ack_seq as lag ' +
    'FROM subscriptions sub ' +
    'JOIN subscribers s ON s.id = sub.subscriber_id ' +
    'JOIN channels c ON c.id = sub.channel_id ' +
    'ORDER BY lag DESC, s.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-14s %5d     %5d       %3d', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('channel').AsString,
        DS.FieldByName('last_ack_seq').AsInteger,
        DS.FieldByName('max_seq').AsInteger,
        DS.FieldByName('lag').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Subscribers with lag > 0 need attention (falling behind)');
  WriteLn;
end;

{ Displays overall message counts by status, throughput by publisher, and delivery success rates per subscriber. }
procedure Demo10_ChannelStats;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Channel Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as total_msgs, ' +
    '  (SELECT COUNT(*) FROM deliveries WHERE status = ''acknowledged'') as acked, ' +
    '  (SELECT COUNT(*) FROM deliveries WHERE status = ''delivered'') as pending, ' +
    '  (SELECT COUNT(*) FROM deliveries WHERE status = ''failed'') as failed, ' +
    '  (SELECT COUNT(*) FROM dead_letters) as dead ' +
    'FROM messages');
  try
    DS.First;
    WriteLn(Format('   Total messages: %d', [DS.FieldByName('total_msgs').AsInteger]));
    WriteLn(Format('   Acknowledged:   %d', [DS.FieldByName('acked').AsInteger]));
    WriteLn(Format('   Pending:        %d', [DS.FieldByName('pending').AsInteger]));
    WriteLn(Format('   Failed:         %d', [DS.FieldByName('failed').AsInteger]));
    WriteLn(Format('   Dead letters:   %d', [DS.FieldByName('dead').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Throughput by publisher:');
  DS := Conn.ExecuteQuery(
    'SELECT publisher, COUNT(*) as msgs, ' +
    '  COUNT(DISTINCT channel_id) as channels ' +
    'FROM messages GROUP BY publisher ORDER BY msgs DESC');
  try
    WriteLn('   Publisher        Messages  Channels');
    WriteLn('   ' + StringOfChar('-', 40));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %5d     %d', [
        DS.FieldByName('publisher').AsString,
        DS.FieldByName('msgs').AsInteger,
        DS.FieldByName('channels').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Delivery success rate by subscriber:');
  DS := Conn.ExecuteQuery(
    'SELECT s.name, ' +
    '  COUNT(CASE WHEN d.status = ''acknowledged'' THEN 1 END) as acked, ' +
    '  COUNT(*) as total, ' +
    '  ROUND(100.0 * COUNT(CASE WHEN d.status = ''acknowledged'' THEN 1 END) / COUNT(*), 1) as pct ' +
    'FROM deliveries d ' +
    'JOIN subscribers s ON s.id = d.subscriber_id ' +
    'GROUP BY d.subscriber_id ORDER BY pct DESC');
  try
    WriteLn('   Subscriber        Acked  Total  Success');
    WriteLn('   ' + StringOfChar('-', 45));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %5d  %5d  %.1f%%', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('acked').AsInteger,
        DS.FieldByName('total').AsInteger,
        DS.FieldByName('pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

begin
  WriteLn('Example 142: Pub/Sub Database - Publish/Subscribe via Polling');
  WriteLn(StringOfChar('=', 70));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_ChannelsAndSubs;
    Demo2_Publishing;
    Demo3_MessageOrdering;
    Demo4_SubscriberPolling;
    Demo5_Acknowledgment;
    Demo6_DeadLetters;
    Demo7_FanOut;
    Demo8_Filtering;
    Demo9_SubscriptionLag;
    Demo10_ChannelStats;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
