{===============================================================================
  NDXSQLite Example 87 - Email Queue
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates email queue with retry patterns:
  - Priority-based email queuing (high/normal/low)
  - Template-based email generation
  - Retry mechanism with exponential backoff
  - Bounce handling and suppression list
  - Batch processing simulation
  - Send rate limiting
  - Email status tracking (queued → sending → sent/failed/bounced)
  - Queue statistics and monitoring

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program EmailQueue;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Email Queue Schema');
  WriteLn('   ==============================');

  // Email templates
  Conn.ExecuteNonQuery(
    'CREATE TABLE email_templates (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL UNIQUE,' +
    '  subject_template TEXT NOT NULL,' +
    '  body_template TEXT NOT NULL,' +
    '  category TEXT DEFAULT ''transactional'',' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Main email queue
  Conn.ExecuteNonQuery(
    'CREATE TABLE email_queue (' +
    '  id INTEGER PRIMARY KEY,' +
    '  template_id INTEGER REFERENCES email_templates(id),' +
    '  from_address TEXT NOT NULL,' +
    '  to_address TEXT NOT NULL,' +
    '  cc_address TEXT,' +
    '  subject TEXT NOT NULL,' +
    '  body TEXT NOT NULL,' +
    '  priority INTEGER DEFAULT 5,' +
    '  status TEXT DEFAULT ''queued'',' +
    '  attempt_count INTEGER DEFAULT 0,' +
    '  max_attempts INTEGER DEFAULT 3,' +
    '  scheduled_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  sent_at TEXT,' +
    '  next_retry_at TEXT,' +
    '  error_message TEXT,' +
    '  message_id TEXT,' +
    '  category TEXT DEFAULT ''transactional''' +
    ')');

  // Send attempts log
  Conn.ExecuteNonQuery(
    'CREATE TABLE email_send_log (' +
    '  id INTEGER PRIMARY KEY,' +
    '  email_id INTEGER NOT NULL REFERENCES email_queue(id),' +
    '  attempt_number INTEGER NOT NULL,' +
    '  status TEXT NOT NULL,' +
    '  smtp_response TEXT,' +
    '  error_message TEXT,' +
    '  duration_ms INTEGER,' +
    '  attempted_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Bounce tracking
  Conn.ExecuteNonQuery(
    'CREATE TABLE email_bounces (' +
    '  id INTEGER PRIMARY KEY,' +
    '  email_id INTEGER REFERENCES email_queue(id),' +
    '  to_address TEXT NOT NULL,' +
    '  bounce_type TEXT NOT NULL,' +
    '  bounce_reason TEXT,' +
    '  bounced_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Suppression list (do not email)
  Conn.ExecuteNonQuery(
    'CREATE TABLE email_suppression (' +
    '  id INTEGER PRIMARY KEY,' +
    '  email_address TEXT NOT NULL UNIQUE,' +
    '  reason TEXT NOT NULL,' +
    '  source TEXT,' +
    '  added_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Rate limiting
  Conn.ExecuteNonQuery(
    'CREATE TABLE email_rate_limits (' +
    '  id INTEGER PRIMARY KEY,' +
    '  window_start TEXT NOT NULL,' +
    '  window_end TEXT NOT NULL,' +
    '  emails_sent INTEGER DEFAULT 0,' +
    '  max_per_window INTEGER DEFAULT 100,' +
    '  category TEXT DEFAULT ''all''' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_queue_status ON email_queue(status)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_queue_priority ON email_queue(priority, created_at)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_queue_scheduled ON email_queue(scheduled_at)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_queue_to ON email_queue(to_address)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_queue_retry ON email_queue(next_retry_at)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_bounces_address ON email_bounces(to_address)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_suppression_email ON email_suppression(email_address)');

  WriteLn('   Created tables: email_templates, email_queue, email_send_log,');
  WriteLn('                    email_bounces, email_suppression, email_rate_limits');
  WriteLn('');
end;

// =============================================================================
// Setup Templates
// =============================================================================
{ Inserts five email templates (welcome, order confirmation, password reset, newsletter, alert) with subject and body placeholders. }
procedure SetupTemplates;
begin
  WriteLn('2. Setting Up Email Templates');
  WriteLn('   =============================');

  Conn.ExecuteNonQuery(
    'INSERT INTO email_templates (name, subject_template, body_template, category) VALUES (?, ?, ?, ?)',
    ['welcome', 'Welcome to {app_name}, {user_name}!',
     'Hello {user_name},\n\nWelcome to {app_name}. Your account is ready.\n\nBest regards,\nThe Team',
     'transactional']);

  Conn.ExecuteNonQuery(
    'INSERT INTO email_templates (name, subject_template, body_template, category) VALUES (?, ?, ?, ?)',
    ['order_confirmation', 'Order #{order_id} Confirmed',
     'Hi {user_name},\n\nYour order #{order_id} for {amount} has been confirmed.\nEstimated delivery: {delivery_date}\n\nThank you!',
     'transactional']);

  Conn.ExecuteNonQuery(
    'INSERT INTO email_templates (name, subject_template, body_template, category) VALUES (?, ?, ?, ?)',
    ['password_reset', 'Password Reset Request',
     'Hi {user_name},\n\nClick here to reset your password: {reset_link}\n\nThis link expires in 24 hours.',
     'transactional']);

  Conn.ExecuteNonQuery(
    'INSERT INTO email_templates (name, subject_template, body_template, category) VALUES (?, ?, ?, ?)',
    ['newsletter', '{month} Newsletter - {app_name}',
     'Hello {user_name},\n\nHere are this month''s highlights:\n{content}\n\nUnsubscribe: {unsub_link}',
     'marketing']);

  Conn.ExecuteNonQuery(
    'INSERT INTO email_templates (name, subject_template, body_template, category) VALUES (?, ?, ?, ?)',
    ['alert', '[ALERT] {alert_type}: {alert_message}',
     'System Alert\n\nType: {alert_type}\nMessage: {alert_message}\nTime: {alert_time}\n\nPlease investigate.',
     'system']);

  WriteLn('   Created 5 templates:');
  WriteLn('     - welcome (transactional)');
  WriteLn('     - order_confirmation (transactional)');
  WriteLn('     - password_reset (transactional)');
  WriteLn('     - newsletter (marketing)');
  WriteLn('     - alert (system)');
  WriteLn('');
end;

// =============================================================================
// Queue Emails
// =============================================================================
{ Populates the suppression list and inserts emails at different priorities (password resets, orders, newsletters, alerts) into the queue. }
procedure QueueEmails;
var
  I: Integer;
  Priority: Integer;
  Category, ToAddr, Subject, Body: string;
begin
  WriteLn('3. Queuing Emails');
  WriteLn('   =================');

  // Add suppressed addresses first
  Conn.ExecuteNonQuery(
    'INSERT INTO email_suppression (email_address, reason, source) VALUES (?, ?, ?)',
    ['bounced@invalid.com', 'hard_bounce', 'system']);
  Conn.ExecuteNonQuery(
    'INSERT INTO email_suppression (email_address, reason, source) VALUES (?, ?, ?)',
    ['unsubscribed@user.com', 'unsubscribe', 'user_request']);
  WriteLn('   Added 2 addresses to suppression list');

  // Queue various emails
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // High priority - password resets (priority 1)
  for I := 1 to 3 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO email_queue (template_id, from_address, to_address, subject, body, priority, category, scheduled_at) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
      [3, 'noreply@app.com', Format('user%d@email.com', [I]),
       Format('Password Reset Request', []),
       Format('Hi User_%d, click to reset: https://app.com/reset/%d', [I, 1000 + I]),
       1, 'transactional', '2024-06-15 10:00:00']);
  end;

  // Normal priority - order confirmations (priority 5)
  for I := 1 to 5 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO email_queue (template_id, from_address, to_address, subject, body, priority, category, scheduled_at) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
      [2, 'orders@app.com', Format('customer%d@email.com', [I]),
       Format('Order #%d Confirmed', [2000 + I]),
       Format('Hi Customer_%d, order #%d for $%.2f confirmed.', [I, 2000 + I, 49.99 + I * 10.0]),
       5, 'transactional', '2024-06-15 10:00:00']);
  end;

  // Low priority - newsletters (priority 9)
  for I := 1 to 8 do
  begin
    ToAddr := Format('subscriber%d@email.com', [I]);
    // Check suppression list
    if Integer(Conn.ExecuteScalar(
      'SELECT COUNT(*) FROM email_suppression WHERE email_address = ?', [ToAddr])) = 0 then
    begin
      Conn.ExecuteNonQuery(
        'INSERT INTO email_queue (template_id, from_address, to_address, subject, body, priority, category, scheduled_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
        [4, 'newsletter@app.com', ToAddr,
         'June Newsletter - MyApp',
         Format('Hello Subscriber_%d, here are this month''s highlights...', [I]),
         9, 'marketing', '2024-06-15 14:00:00']);
    end;
  end;

  // System alerts (priority 1)
  for I := 1 to 2 do
  begin
    Conn.ExecuteNonQuery(
      'INSERT INTO email_queue (template_id, from_address, to_address, subject, body, priority, category, scheduled_at) ' +
      'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
      [5, 'alerts@app.com', 'admin@company.com',
       Format('[ALERT] %s: System threshold exceeded', [IfThen(I = 1, 'CPU', 'Memory')]),
       Format('Alert: %s usage at %d%%. Please investigate.', [IfThen(I = 1, 'CPU', 'Memory'), 85 + I * 5]),
       1, 'system', '2024-06-15 10:00:00']);
  end;

  // Queue an email to suppressed address (should be skipped later)
  Conn.ExecuteNonQuery(
    'INSERT INTO email_queue (from_address, to_address, subject, body, priority, category, scheduled_at) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?)',
    ['noreply@app.com', 'bounced@invalid.com',
     'Test Subject', 'Test body', 5, 'transactional', '2024-06-15 10:00:00']);

  Conn.ExecuteNonQuery('COMMIT');

  WriteLn(Format('   Queued %d emails:', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue'))]));
  WriteLn(Format('     High priority (1):   %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue WHERE priority = 1'))]));
  WriteLn(Format('     Normal priority (5): %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue WHERE priority = 5'))]));
  WriteLn(Format('     Low priority (9):    %d', [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue WHERE priority = 9'))]));
  WriteLn('');
end;

// =============================================================================
// Process Queue (Batch)
// =============================================================================
{ Processes queued emails in priority order, checking suppression, simulating SMTP sends, logging attempts, and handling bounces. }
procedure ProcessQueue;
var
  DS: TDataSet;
  EmailId, AttemptNum, ResponseCode, DurationMs: Integer;
  Status, ToAddr, ErrorMsg, SmtpResponse: string;
  Sent, Failed, Suppressed, BatchSize: Integer;
begin
  WriteLn('4. Processing Email Queue');
  WriteLn('   =========================');
  WriteLn('');

  Sent := 0;
  Failed := 0;
  Suppressed := 0;
  BatchSize := 0;

  // Process in priority order
  DS := Conn.ExecuteQuery(
    'SELECT id, to_address, subject, priority, category FROM email_queue ' +
    'WHERE status = ''queued'' AND scheduled_at <= ''2024-06-15 14:01:00'' ' +
    'ORDER BY priority ASC, created_at ASC');
  try
    while not DS.EOF do
    begin
      EmailId := DS.FieldByName('id').AsInteger;
      ToAddr := DS.FieldByName('to_address').AsString;
      Inc(BatchSize);

      // Check suppression list
      if Integer(Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM email_suppression WHERE email_address = ?', [ToAddr])) > 0 then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE email_queue SET status = ''suppressed'', error_message = ''Address in suppression list'' WHERE id = ?',
          [EmailId]);
        Inc(Suppressed);
        DS.Next;
        Continue;
      end;

      // Update status to sending
      Conn.ExecuteNonQuery(
        'UPDATE email_queue SET status = ''sending'', attempt_count = attempt_count + 1 WHERE id = ?',
        [EmailId]);

      AttemptNum := 1;

      // Simulate send outcome
      case EmailId mod 7 of
        0: begin Status := 'sent'; ResponseCode := 250; ErrorMsg := ''; SmtpResponse := '250 OK'; DurationMs := 120; end;
        1: begin Status := 'sent'; ResponseCode := 250; ErrorMsg := ''; SmtpResponse := '250 Accepted'; DurationMs := 85; end;
        2: begin Status := 'sent'; ResponseCode := 250; ErrorMsg := ''; SmtpResponse := '250 OK'; DurationMs := 200; end;
        3: begin Status := 'failed'; ResponseCode := 550; ErrorMsg := 'Mailbox not found'; SmtpResponse := '550 User unknown'; DurationMs := 50; end;
        4: begin Status := 'sent'; ResponseCode := 250; ErrorMsg := ''; SmtpResponse := '250 OK'; DurationMs := 150; end;
        5: begin Status := 'failed'; ResponseCode := 421; ErrorMsg := 'Service temporarily unavailable'; SmtpResponse := '421 Try again later'; DurationMs := 3000; end;
        6: begin Status := 'sent'; ResponseCode := 250; ErrorMsg := ''; SmtpResponse := '250 Message queued'; DurationMs := 95; end;
      else
        begin Status := 'sent'; ResponseCode := 250; ErrorMsg := ''; SmtpResponse := '250 OK'; DurationMs := 100; end;
      end;

      // Log the attempt
      Conn.ExecuteNonQuery(
        'INSERT INTO email_send_log (email_id, attempt_number, status, smtp_response, error_message, duration_ms, attempted_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        [EmailId, AttemptNum, Status, SmtpResponse,
         IfThen(ErrorMsg = '', '', ErrorMsg), DurationMs, '2024-06-15 14:01:00']);

      if Status = 'sent' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE email_queue SET status = ''sent'', sent_at = ?, message_id = ? WHERE id = ?',
          ['2024-06-15 14:01:00', Format('MSG-%d-%d', [EmailId, 1000]), EmailId]);
        Inc(Sent);
      end
      else
      begin
        // Check if hard bounce (5xx) or soft bounce (4xx)
        if ResponseCode >= 500 then
        begin
          Conn.ExecuteNonQuery(
            'UPDATE email_queue SET status = ''bounced'', error_message = ? WHERE id = ?',
            [ErrorMsg, EmailId]);

          // Record bounce
          Conn.ExecuteNonQuery(
            'INSERT INTO email_bounces (email_id, to_address, bounce_type, bounce_reason) VALUES (?, ?, ?, ?)',
            [EmailId, ToAddr, 'hard', ErrorMsg]);

          // Add to suppression list
          Conn.ExecuteNonQuery(
            'INSERT OR IGNORE INTO email_suppression (email_address, reason, source) VALUES (?, ?, ?)',
            [ToAddr, 'hard_bounce', 'automatic']);

          Inc(Failed);
        end
        else
        begin
          // Soft bounce - schedule retry
          Conn.ExecuteNonQuery(
            'UPDATE email_queue SET status = ''retry_pending'', error_message = ?, ' +
            'next_retry_at = ? WHERE id = ?',
            [ErrorMsg, '2024-06-15 14:05:00', EmailId]);
          Inc(Failed);
        end;
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Batch processed: %d emails', [BatchSize]));
  WriteLn(Format('     Sent:       %d', [Sent]));
  WriteLn(Format('     Failed:     %d', [Failed]));
  WriteLn(Format('     Suppressed: %d', [Suppressed]));
  WriteLn('');
end;

// =============================================================================
// Retry Failed Emails
// =============================================================================
{ Retries emails in retry_pending status with exponential backoff, marking exhausted retries as permanently failed. }
procedure RetryFailedEmails;
var
  DS: TDataSet;
  EmailId, AttemptCount, MaxAttempts: Integer;
  RetryCount, SuccessCount: Integer;
  Status: string;
begin
  WriteLn('5. Retrying Failed Emails');
  WriteLn('   =========================');
  WriteLn('');

  RetryCount := 0;
  SuccessCount := 0;

  DS := Conn.ExecuteQuery(
    'SELECT id, attempt_count, max_attempts, to_address FROM email_queue ' +
    'WHERE status = ''retry_pending'' AND next_retry_at <= ''2024-06-15 14:05:00''');
  try
    while not DS.EOF do
    begin
      EmailId := DS.FieldByName('id').AsInteger;
      AttemptCount := DS.FieldByName('attempt_count').AsInteger + 1;
      MaxAttempts := DS.FieldByName('max_attempts').AsInteger;
      Inc(RetryCount);

      // Simulate retry (succeed on 2nd attempt for some)
      if EmailId mod 2 = 0 then
        Status := 'sent'
      else
        Status := 'failed';

      Conn.ExecuteNonQuery(
        'INSERT INTO email_send_log (email_id, attempt_number, status, smtp_response, error_message, duration_ms, attempted_at) ' +
        'VALUES (?, ?, ?, ?, ?, ?, ?)',
        [EmailId, AttemptCount, Status,
         IfThen(Status = 'sent', '250 OK', '421 Still unavailable'),
         IfThen(Status = 'sent', '', 'Service still unavailable'),
         IfThen(Status = 'sent', '150', '3000'),
         '2024-06-15 14:05:00']);

      if Status = 'sent' then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE email_queue SET status = ''sent'', attempt_count = ?, sent_at = ? WHERE id = ?',
          [AttemptCount, '2024-06-15 14:05:00', EmailId]);
        Inc(SuccessCount);
      end
      else if AttemptCount >= MaxAttempts then
      begin
        Conn.ExecuteNonQuery(
          'UPDATE email_queue SET status = ''failed'', attempt_count = ?, error_message = ? WHERE id = ?',
          [AttemptCount, 'Max attempts reached', EmailId]);
      end
      else
      begin
        // Exponential backoff: next retry in 2^attempt minutes
        Conn.ExecuteNonQuery(
          'UPDATE email_queue SET status = ''retry_pending'', attempt_count = ?, ' +
          'next_retry_at = ? WHERE id = ?',
          [AttemptCount, Format('2024-06-15 14:%.2d:00', [5 + AttemptCount * 5]), EmailId]);
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Retried: %d emails', [RetryCount]));
  WriteLn(Format('     Now sent:     %d', [SuccessCount]));
  WriteLn(Format('     Still failing: %d', [RetryCount - SuccessCount]));

  // Show backoff schedule
  WriteLn('');
  WriteLn('   Exponential backoff schedule:');
  WriteLn('     Attempt 1: immediate');
  WriteLn('     Attempt 2: +5 minutes');
  WriteLn('     Attempt 3: +10 minutes');
  WriteLn('     After max: marked as failed');
  WriteLn('');
end;

// =============================================================================
// Bounce Management
// =============================================================================
{ Displays recorded bounce details joined with email subjects and lists all entries in the suppression list. }
procedure DemoBounceManagement;
var
  DS: TDataSet;
begin
  WriteLn('6. Bounce Management');
  WriteLn('   ====================');
  WriteLn('');

  // Show bounces
  DS := Conn.ExecuteQuery(
    'SELECT b.to_address, b.bounce_type, b.bounce_reason, e.subject ' +
    'FROM email_bounces b ' +
    'JOIN email_queue e ON b.email_id = e.id ' +
    'ORDER BY b.bounced_at');
  try
    WriteLn(Format('   %-25s | %-6s | %-20s | %s', ['Address', 'Type', 'Reason', 'Subject']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s | %-6s | %-20s | %s',
        [DS.FieldByName('to_address').AsString,
         DS.FieldByName('bounce_type').AsString,
         DS.FieldByName('bounce_reason').AsString,
         DS.FieldByName('subject').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show suppression list
  WriteLn('');
  WriteLn('   Suppression list:');
  DS := Conn.ExecuteQuery('SELECT email_address, reason, source FROM email_suppression ORDER BY added_at');
  try
    WriteLn(Format('   %-30s | %-15s | %s', ['Email', 'Reason', 'Source']));
    WriteLn('   ' + StringOfChar('-', 65));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-30s | %-15s | %s',
        [DS.FieldByName('email_address').AsString,
         DS.FieldByName('reason').AsString,
         DS.FieldByName('source').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Rate Limiting
// =============================================================================
{ Creates hourly rate limit windows for all, marketing, and transactional categories, then computes and displays current usage versus limits. }
procedure DemoRateLimiting;
var
  DS: TDataSet;
begin
  WriteLn('7. Rate Limiting');
  WriteLn('   ================');
  WriteLn('');

  // Set up rate limit windows
  Conn.ExecuteNonQuery(
    'INSERT INTO email_rate_limits (window_start, window_end, max_per_window, category) VALUES ' +
    '(''2024-06-15 14:00:00'', ''2024-06-15 15:00:00'', 100, ''all'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO email_rate_limits (window_start, window_end, max_per_window, category) VALUES ' +
    '(''2024-06-15 14:00:00'', ''2024-06-15 15:00:00'', 50, ''marketing'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO email_rate_limits (window_start, window_end, max_per_window, category) VALUES ' +
    '(''2024-06-15 14:00:00'', ''2024-06-15 15:00:00'', 200, ''transactional'')');

  // Update sent counts
  Conn.ExecuteNonQuery(
    'UPDATE email_rate_limits SET emails_sent = ' +
    '(SELECT COUNT(*) FROM email_queue WHERE status = ''sent'' AND category = email_rate_limits.category ' +
    'AND sent_at >= window_start AND sent_at < window_end) ' +
    'WHERE category != ''all''');
  Conn.ExecuteNonQuery(
    'UPDATE email_rate_limits SET emails_sent = ' +
    '(SELECT COUNT(*) FROM email_queue WHERE status = ''sent'' ' +
    'AND sent_at >= window_start AND sent_at < window_end) ' +
    'WHERE category = ''all''');

  WriteLn('   Rate limit windows (hourly):');
  DS := Conn.ExecuteQuery('SELECT category, emails_sent, max_per_window FROM email_rate_limits ORDER BY category');
  try
    WriteLn(Format('   %-15s | %-6s | %-6s | %s', ['Category', 'Sent', 'Max', 'Status']));
    WriteLn('   ' + StringOfChar('-', 50));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s | %-6s | %-6s | %s',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('emails_sent').AsString,
         DS.FieldByName('max_per_window').AsString,
         IfThen(DS.FieldByName('emails_sent').AsInteger < DS.FieldByName('max_per_window').AsInteger,
           'OK', 'LIMIT REACHED')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Queue Status
// =============================================================================
{ Displays the email queue breakdown by status with percentages, then by priority and category with sent/failed counts. }
procedure DemoQueueStatus;
var
  DS: TDataSet;
begin
  WriteLn('8. Queue Status Overview');
  WriteLn('   ========================');
  WriteLn('');

  // Status breakdown
  WriteLn('   Email status distribution:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) AS cnt, ' +
    'ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM email_queue), 1) AS pct ' +
    'FROM email_queue GROUP BY status ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %-15s: %s (%s%%)',
        [DS.FieldByName('status').AsString,
         DS.FieldByName('cnt').AsString,
         DS.FieldByName('pct').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Priority breakdown
  WriteLn('');
  WriteLn('   By priority:');
  DS := Conn.ExecuteQuery(
    'SELECT priority, status, COUNT(*) AS cnt FROM email_queue ' +
    'GROUP BY priority, status ORDER BY priority, status');
  try
    WriteLn(Format('     %-8s | %-15s | %s', ['Priority', 'Status', 'Count']));
    WriteLn('     ' + StringOfChar('-', 40));
    while not DS.EOF do
    begin
      WriteLn(Format('     %-8s | %-15s | %s',
        [DS.FieldByName('priority').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('cnt').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Category breakdown
  WriteLn('');
  WriteLn('   By category:');
  DS := Conn.ExecuteQuery(
    'SELECT category, COUNT(*) AS total, ' +
    'SUM(CASE WHEN status = ''sent'' THEN 1 ELSE 0 END) AS sent, ' +
    'SUM(CASE WHEN status IN (''failed'',''bounced'') THEN 1 ELSE 0 END) AS failed ' +
    'FROM email_queue GROUP BY category ORDER BY total DESC');
  try
    WriteLn(Format('     %-15s | %-6s | %-6s | %s', ['Category', 'Total', 'Sent', 'Failed']));
    WriteLn('     ' + StringOfChar('-', 45));
    while not DS.EOF do
    begin
      WriteLn(Format('     %-15s | %-6s | %-6s | %s',
        [DS.FieldByName('category').AsString,
         DS.FieldByName('total').AsString,
         DS.FieldByName('sent').AsString,
         DS.FieldByName('failed').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn('');
end;

// =============================================================================
// Send Log
// =============================================================================
{ Lists all send attempt log entries with email ID, attempt number, SMTP response, duration, and recipient address. }
procedure DemoSendLog;
var
  DS: TDataSet;
begin
  WriteLn('9. Send Attempt Log');
  WriteLn('   ====================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT sl.email_id, sl.attempt_number, sl.status, sl.smtp_response, ' +
    'sl.duration_ms, eq.to_address ' +
    'FROM email_send_log sl ' +
    'JOIN email_queue eq ON sl.email_id = eq.id ' +
    'ORDER BY sl.email_id, sl.attempt_number');
  try
    WriteLn(Format('   %-5s | %-7s | %-8s | %-25s | %-6s | %s',
      ['Email', 'Attempt', 'Status', 'SMTP Response', 'ms', 'To']));
    WriteLn('   ' + StringOfChar('-', 85));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-5s | %-7s | %-8s | %-25s | %-6s | %s',
        [DS.FieldByName('email_id').AsString,
         DS.FieldByName('attempt_number').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('smtp_response').AsString,
         DS.FieldByName('duration_ms').AsString,
         DS.FieldByName('to_address').AsString]));
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
{ Demonstrates email queue statistics and delivery metrics. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('10. Email Queue Statistics');
  WriteLn('    =========================');
  WriteLn('');

  WriteLn(Format('    Total emails queued:    %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue'))]));
  WriteLn(Format('    Successfully sent:     %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue WHERE status = ''sent'''))]));
  WriteLn(Format('    Failed permanently:    %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue WHERE status IN (''failed'',''bounced'')'))]));
  WriteLn(Format('    Suppressed:            %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue WHERE status = ''suppressed'''))]));
  WriteLn(Format('    Pending retry:         %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue WHERE status = ''retry_pending'''))]));

  // Send performance
  WriteLn('');
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) AS attempts, AVG(duration_ms) AS avg_ms, ' +
    'MIN(duration_ms) AS min_ms, MAX(duration_ms) AS max_ms ' +
    'FROM email_send_log');
  try
    WriteLn('    Send performance:');
    WriteLn(Format('      Total attempts:    %s', [DS.FieldByName('attempts').AsString]));
    WriteLn(Format('      Avg duration:      %.0f ms', [DS.FieldByName('avg_ms').AsFloat]));
    WriteLn(Format('      Min duration:      %s ms', [DS.FieldByName('min_ms').AsString]));
    WriteLn(Format('      Max duration:      %s ms', [DS.FieldByName('max_ms').AsString]));
  finally
    DS.Free;
  end;

  // Bounce rate
  WriteLn('');
  WriteLn(Format('    Bounce rate:           %.1f%%',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_bounces')) * 100.0 /
     Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_queue'))]));
  WriteLn(Format('    Suppression list size: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM email_suppression'))]));

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 87: Email Queue ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    SetupTemplates;
    QueueEmails;
    ProcessQueue;
    RetryFailedEmails;
    DemoBounceManagement;
    DemoRateLimiting;
    DemoQueueStatus;
    DemoSendLog;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
