{===============================================================================
  NDXSQLite Example 146 - Template Engine
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Template registration and versioning
  - Parameterized templates with variables
  - Variable substitution and defaults
  - Template inheritance and composition
  - Render pipeline and history tracking

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program TemplateEngine;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Templates
  Conn.ExecuteNonQuery(
    'CREATE TABLE templates (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  name TEXT NOT NULL, ' +
    '  version INTEGER NOT NULL DEFAULT 1, ' +
    '  parent_name TEXT, ' +
    '  content TEXT NOT NULL, ' +
    '  description TEXT, ' +
    '  author TEXT NOT NULL, ' +
    '  created_at TEXT NOT NULL, ' +
    '  UNIQUE(name, version))');

  // Template variables (parameters)
  Conn.ExecuteNonQuery(
    'CREATE TABLE template_vars (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  template_id INTEGER NOT NULL, ' +
    '  var_name TEXT NOT NULL, ' +
    '  var_type TEXT NOT NULL DEFAULT ''string'', ' +
    '  default_value TEXT, ' +
    '  required INTEGER NOT NULL DEFAULT 1, ' +
    '  description TEXT, ' +
    '  UNIQUE(template_id, var_name))');

  // Render history
  Conn.ExecuteNonQuery(
    'CREATE TABLE render_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  template_name TEXT NOT NULL, ' +
    '  template_version INTEGER NOT NULL, ' +
    '  variables TEXT NOT NULL, ' +
    '  output TEXT NOT NULL, ' +
    '  rendered_by TEXT NOT NULL, ' +
    '  rendered_at TEXT NOT NULL)');

  // Template blocks (for inheritance)
  Conn.ExecuteNonQuery(
    'CREATE TABLE template_blocks (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  template_id INTEGER NOT NULL, ' +
    '  block_name TEXT NOT NULL, ' +
    '  block_content TEXT NOT NULL, ' +
    '  UNIQUE(template_id, block_name))');
end;

{ Inserts sample data into tables. }
procedure InsertData;
begin
  Conn.ExecuteNonQuery('BEGIN TRANSACTION');

  // Base layout template (parent)
  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''base_layout'', 1, NULL, ' +
    '''<!DOCTYPE html><html><head><title>{{title}}</title></head><body><header>{{site_name}}</header><main>{{block:content}}</main><footer>{{footer_text}}</footer></body></html>'', ' +
    '''Base HTML layout with header and footer'', ''admin'', ''2025-01-01 09:00:00'')');

  // Page template (extends base_layout)
  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''page'', 1, ''base_layout'', ' +
    '''<h1>{{heading}}</h1><div class="content">{{body}}</div><p>Author: {{author}}</p>'', ' +
    '''Standard page extending base layout'', ''alice'', ''2025-01-05 10:00:00'')');

  // Email templates
  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''email_welcome'', 1, NULL, ' +
    '''Hello {{user_name}}, Welcome to {{platform_name}}! Your account has been created with email {{email}}. {{greeting_close}}'', ' +
    '''Welcome email for new users'', ''bob'', ''2025-01-03 14:00:00'')');

  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''email_welcome'', 2, NULL, ' +
    '''Hi {{user_name}}, Welcome aboard {{platform_name}}! Your account ({{email}}) is ready. Get started: {{getting_started_url}}. {{greeting_close}}'', ' +
    '''Welcome email v2 with getting started link'', ''bob'', ''2025-01-10 11:00:00'')');

  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''email_reset'', 1, NULL, ' +
    '''Hello {{user_name}}, A password reset was requested for your account ({{email}}). Click here: {{reset_url}} This link expires in {{expiry_hours}} hours.'', ' +
    '''Password reset email'', ''bob'', ''2025-01-04 09:00:00'')');

  // Invoice template
  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''invoice'', 1, NULL, ' +
    '''INVOICE #{{invoice_number}} | Date: {{invoice_date}} | Customer: {{customer_name}} ({{customer_email}}) | Items: {{items_summary}} | Total: {{currency}}{{total_amount}} | Due: {{due_date}}'', ' +
    '''Invoice document template'', ''charlie'', ''2025-01-06 16:00:00'')');

  // Notification template
  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''notification'', 1, NULL, ' +
    '''[{{severity}}] {{title}}: {{message}} (at {{timestamp}})'', ' +
    '''System notification template'', ''alice'', ''2025-01-07 08:00:00'')');

  // Report header template (parent for reports)
  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''report_base'', 1, NULL, ' +
    '''=== {{report_title}} === | Generated: {{generated_at}} | Period: {{period_start}} to {{period_end}} | {{block:report_body}} | --- End of Report ---'', ' +
    '''Base report template'', ''charlie'', ''2025-01-08 10:00:00'')');

  // Sales report (extends report_base)
  Conn.ExecuteNonQuery('INSERT INTO templates (name, version, parent_name, content, description, author, created_at) VALUES ' +
    '(''report_sales'', 1, ''report_base'', ' +
    '''Total Revenue: {{currency}}{{total_revenue}} | Orders: {{order_count}} | Avg Order: {{currency}}{{avg_order}} | Top Product: {{top_product}}'', ' +
    '''Sales report extending base report'', ''charlie'', ''2025-01-09 14:00:00'')');

  // Template variables
  // base_layout vars
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (1, ''title'', ''string'', ''Untitled'', 1, ''Page title'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (1, ''site_name'', ''string'', ''My Site'', 0, ''Site name in header'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (1, ''footer_text'', ''string'', ''(c) 2025'', 0, ''Footer copyright text'')');

  // page vars
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (2, ''heading'', ''string'', NULL, 1, ''Page heading'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (2, ''body'', ''text'', NULL, 1, ''Page body content'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (2, ''author'', ''string'', ''Anonymous'', 0, ''Author name'')');

  // email_welcome v2 vars
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (4, ''user_name'', ''string'', NULL, 1, ''User display name'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (4, ''platform_name'', ''string'', ''Our Platform'', 0, ''Platform brand name'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (4, ''email'', ''email'', NULL, 1, ''User email address'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (4, ''getting_started_url'', ''url'', ''https://example.com/start'', 0, ''Getting started page URL'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (4, ''greeting_close'', ''string'', ''Best regards, The Team'', 0, ''Closing greeting'')');

  // email_reset vars
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (5, ''user_name'', ''string'', NULL, 1, ''User display name'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (5, ''email'', ''email'', NULL, 1, ''User email'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (5, ''reset_url'', ''url'', NULL, 1, ''Password reset URL'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (5, ''expiry_hours'', ''integer'', ''24'', 0, ''Link expiry in hours'')');

  // invoice vars
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''invoice_number'', ''string'', NULL, 1, ''Invoice number'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''invoice_date'', ''date'', NULL, 1, ''Invoice date'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''customer_name'', ''string'', NULL, 1, ''Customer full name'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''customer_email'', ''email'', NULL, 1, ''Customer email'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''items_summary'', ''text'', NULL, 1, ''Line items description'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''currency'', ''string'', ''$'', 0, ''Currency symbol'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''total_amount'', ''decimal'', NULL, 1, ''Total invoice amount'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (6, ''due_date'', ''date'', NULL, 1, ''Payment due date'')');

  // notification vars
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (7, ''severity'', ''string'', ''INFO'', 0, ''Alert level'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (7, ''title'', ''string'', NULL, 1, ''Notification title'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (7, ''message'', ''text'', NULL, 1, ''Notification body'')');
  Conn.ExecuteNonQuery('INSERT INTO template_vars (template_id, var_name, var_type, default_value, required, description) VALUES (7, ''timestamp'', ''datetime'', NULL, 1, ''Event timestamp'')');

  // Template blocks for inheritance
  Conn.ExecuteNonQuery('INSERT INTO template_blocks (template_id, block_name, block_content) VALUES ' +
    '(2, ''content'', ''<h1>{{heading}}</h1><div class="content">{{body}}</div><p>Author: {{author}}</p>'')');
  Conn.ExecuteNonQuery('INSERT INTO template_blocks (template_id, block_name, block_content) VALUES ' +
    '(9, ''report_body'', ''Total Revenue: {{currency}}{{total_revenue}} | Orders: {{order_count}} | Avg Order: {{currency}}{{avg_order}} | Top Product: {{top_product}}'')');

  // Render history
  Conn.ExecuteNonQuery('INSERT INTO render_log (template_name, template_version, variables, output, rendered_by, rendered_at) VALUES ' +
    '(''email_welcome'', 1, ''user_name=Alice;platform_name=Acme Corp;email=alice@example.com;greeting_close=Cheers, Team'', ' +
    '''Hello Alice, Welcome to Acme Corp! Your account has been created with email alice@example.com. Cheers, Team'', ' +
    '''system'', ''2025-01-15 10:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO render_log (template_name, template_version, variables, output, rendered_by, rendered_at) VALUES ' +
    '(''email_welcome'', 2, ''user_name=Bob;platform_name=Acme Corp;email=bob@example.com;getting_started_url=https://acme.com/start;greeting_close=Welcome aboard!'', ' +
    '''Hi Bob, Welcome aboard Acme Corp! Your account (bob@example.com) is ready. Get started: https://acme.com/start. Welcome aboard!'', ' +
    '''system'', ''2025-01-16 14:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO render_log (template_name, template_version, variables, output, rendered_by, rendered_at) VALUES ' +
    '(''email_reset'', 1, ''user_name=Alice;email=alice@example.com;reset_url=https://acme.com/reset/abc123;expiry_hours=48'', ' +
    '''Hello Alice, A password reset was requested for your account (alice@example.com). Click here: https://acme.com/reset/abc123 This link expires in 48 hours.'', ' +
    '''system'', ''2025-01-17 09:30:00'')');
  Conn.ExecuteNonQuery('INSERT INTO render_log (template_name, template_version, variables, output, rendered_by, rendered_at) VALUES ' +
    '(''invoice'', 1, ''invoice_number=INV-2025-001;invoice_date=2025-01-18;customer_name=Alice Smith;customer_email=alice@example.com;items_summary=Widget x3, Gadget x1;currency=$;total_amount=149.97;due_date=2025-02-17'', ' +
    '''INVOICE #INV-2025-001 | Date: 2025-01-18 | Customer: Alice Smith (alice@example.com) | Items: Widget x3, Gadget x1 | Total: $149.97 | Due: 2025-02-17'', ' +
    '''billing'', ''2025-01-18 16:00:00'')');
  Conn.ExecuteNonQuery('INSERT INTO render_log (template_name, template_version, variables, output, rendered_by, rendered_at) VALUES ' +
    '(''notification'', 1, ''severity=WARN;title=High CPU;message=Server load exceeds 90%;timestamp=2025-01-19 08:45:00'', ' +
    '''[WARN] High CPU: Server load exceeds 90% (at 2025-01-19 08:45:00)'', ' +
    '''monitoring'', ''2025-01-19 08:45:00'')');
  Conn.ExecuteNonQuery('INSERT INTO render_log (template_name, template_version, variables, output, rendered_by, rendered_at) VALUES ' +
    '(''notification'', 1, ''severity=ERROR;title=DB Connection;message=Primary database unreachable;timestamp=2025-01-19 14:20:00'', ' +
    '''[ERROR] DB Connection: Primary database unreachable (at 2025-01-19 14:20:00)'', ' +
    '''monitoring'', ''2025-01-19 14:20:00'')');
  Conn.ExecuteNonQuery('INSERT INTO render_log (template_name, template_version, variables, output, rendered_by, rendered_at) VALUES ' +
    '(''email_welcome'', 2, ''user_name=Charlie;platform_name=Acme Corp;email=charlie@example.com;getting_started_url=https://acme.com/start;greeting_close=Best, The Team'', ' +
    '''Hi Charlie, Welcome aboard Acme Corp! Your account (charlie@example.com) is ready. Get started: https://acme.com/start. Best, The Team'', ' +
    '''system'', ''2025-01-20 09:00:00'')');

  Conn.ExecuteNonQuery('COMMIT');
end;

{ Lists all registered templates with version, parent, author, and content size, then summarizes template and version counts. }
procedure Demo1_TemplateRegistry;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Template Registry ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT name, version, COALESCE(parent_name, ''-'') AS parent, ' +
    '  author, created_at, LENGTH(content) AS content_len ' +
    'FROM templates ORDER BY name, version');
  try
    WriteLn(Format('   %-18s %-4s %-14s %-8s %-20s %s',
      ['Name', 'Ver', 'Parent', 'Author', 'Created', 'Size']));
    WriteLn('   ' + StringOfChar('-', 85));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-4d %-14s %-8s %-20s %d chars', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('version').AsInteger,
        DS.FieldByName('parent').AsString,
        DS.FieldByName('author').AsString,
        DS.FieldByName('created_at').AsString,
        DS.FieldByName('content_len').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(DISTINCT name) AS templates, COUNT(*) AS versions ' +
    'FROM templates');
  try
    if not DS.EOF then
      WriteLn(Format('   Total: %d templates, %d versions', [
        DS.FieldByName('templates').AsInteger,
        DS.FieldByName('versions').AsInteger]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows template content with placeholder syntax and counts variables and required parameters per template. }
procedure Demo2_ParameterizedTemplates;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Parameterized Templates ===');
  WriteLn;

  // Show template content with placeholders highlighted
  DS := Conn.ExecuteQuery(
    'SELECT name, version, content FROM templates ' +
    'WHERE name IN (''email_welcome'', ''notification'', ''invoice'') ' +
    'ORDER BY name, version DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s (v%d):', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('version').AsInteger]));
      WriteLn(Format('   "%s"', [DS.FieldByName('content').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Count variables per template
  DS := Conn.ExecuteQuery(
    'SELECT t.name, t.version, COUNT(v.id) AS var_count, ' +
    '  COALESCE(SUM(v.required), 0) AS required_count ' +
    'FROM templates t ' +
    'LEFT JOIN template_vars v ON v.template_id = t.id ' +
    'GROUP BY t.id ORDER BY var_count DESC');
  try
    WriteLn(Format('   %-18s %-4s %-6s %s', ['Template', 'Ver', 'Vars', 'Required']));
    WriteLn('   ' + StringOfChar('-', 40));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-4d %-6d %d', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('version').AsInteger,
        DS.FieldByName('var_count').AsInteger,
        DS.FieldByName('required_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Renders templates by chaining SQL REPLACE functions to substitute placeholder variables with actual values. }
procedure Demo3_VariableSubstitution;
var
  DS: TDataSet;
  Template, Result: string;
begin
  WriteLn('=== 3. Variable Substitution ===');
  WriteLn;

  // Demonstrate substitution using SQL REPLACE chains
  WriteLn('   Rendering notification template:');

  DS := Conn.ExecuteQuery(
    'SELECT content FROM templates WHERE name = ''notification'' AND version = 1');
  try
    if not DS.EOF then
      Template := DS.FieldByName('content').AsString;
  finally
    DS.Free;
  end;
  WriteLn(Format('   Template: %s', [Template]));
  WriteLn;

  // Use nested REPLACE to substitute variables
  DS := Conn.ExecuteQuery(
    'SELECT REPLACE(REPLACE(REPLACE(REPLACE(content, ' +
    '  ''{{severity}}'', ''WARN''), ' +
    '  ''{{title}}'', ''Disk Space''), ' +
    '  ''{{message}}'', ''Volume /data is 95% full''), ' +
    '  ''{{timestamp}}'', ''2025-01-20 15:30:00'') AS rendered ' +
    'FROM templates WHERE name = ''notification'' AND version = 1');
  try
    if not DS.EOF then
    begin
      Result := DS.FieldByName('rendered').AsString;
      WriteLn('   Variables: severity=WARN, title=Disk Space, message=Volume /data is 95% full');
      WriteLn(Format('   Output:   %s', [Result]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Another substitution: invoice
  WriteLn('   Rendering invoice template:');
  DS := Conn.ExecuteQuery(
    'SELECT REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(content, ' +
    '  ''{{invoice_number}}'', ''INV-2025-042''), ' +
    '  ''{{invoice_date}}'', ''2025-01-20''), ' +
    '  ''{{customer_name}}'', ''Bob Jones''), ' +
    '  ''{{customer_email}}'', ''bob@company.com''), ' +
    '  ''{{items_summary}}'', ''Service Plan x1''), ' +
    '  ''{{currency}}'', ''EUR ''), ' +
    '  ''{{total_amount}}'', ''299.00''), ' +
    '  ''{{due_date}}'', ''2025-02-19'') AS rendered ' +
    'FROM templates WHERE name = ''invoice'' AND version = 1');
  try
    if not DS.EOF then
      WriteLn(Format('   Output: %s', [DS.FieldByName('rendered').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Shows parent-child template relationships and resolves inheritance by replacing block placeholders with child content. }
procedure Demo4_TemplateInheritance;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Template Inheritance ===');
  WriteLn;

  // Show parent-child relationships
  DS := Conn.ExecuteQuery(
    'SELECT c.name AS child, c.parent_name AS parent, ' +
    '  p.content AS parent_content, ' +
    '  b.block_name, b.block_content ' +
    'FROM templates c ' +
    'JOIN templates p ON p.name = c.parent_name AND p.version = 1 ' +
    'LEFT JOIN template_blocks b ON b.template_id = c.id ' +
    'WHERE c.parent_name IS NOT NULL ' +
    'ORDER BY c.name');
  try
    WriteLn('   Inheritance relationships:');
    WriteLn;
    while not DS.EOF do
    begin
      WriteLn(Format('   %s extends %s', [
        DS.FieldByName('child').AsString,
        DS.FieldByName('parent').AsString]));
      WriteLn(Format('     Parent template: %s', [
        Copy(DS.FieldByName('parent_content').AsString, 1, 80) + '...']));
      if not DS.FieldByName('block_name').IsNull then
        WriteLn(Format('     Block "%s": %s', [
          DS.FieldByName('block_name').AsString,
          Copy(DS.FieldByName('block_content').AsString, 1, 60) + '...']));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Simulate inheritance resolution: replace {{block:content}} with child content
  WriteLn('   Resolved page template (base_layout + page block):');
  DS := Conn.ExecuteQuery(
    'SELECT REPLACE(p.content, ''{{block:content}}'', b.block_content) AS resolved ' +
    'FROM templates c ' +
    'JOIN templates p ON p.name = c.parent_name AND p.version = 1 ' +
    'JOIN template_blocks b ON b.template_id = c.id AND b.block_name = ''content'' ' +
    'WHERE c.name = ''page''');
  try
    if not DS.EOF then
      WriteLn(Format('   %s', [DS.FieldByName('resolved').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays version history for a template, compares content between versions, and lists the latest version per template. }
procedure Demo5_TemplateVersions;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Template Versions ===');
  WriteLn;

  // Show version history for email_welcome
  WriteLn('   Version history for "email_welcome":');
  DS := Conn.ExecuteQuery(
    'SELECT version, author, created_at, description, LENGTH(content) AS size ' +
    'FROM templates WHERE name = ''email_welcome'' ORDER BY version');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   v%d: %s (by %s, %s, %d chars)', [
        DS.FieldByName('version').AsInteger,
        DS.FieldByName('description').AsString,
        DS.FieldByName('author').AsString,
        DS.FieldByName('created_at').AsString,
        DS.FieldByName('size').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Compare content between versions
  WriteLn('   Content comparison (v1 vs v2):');
  DS := Conn.ExecuteQuery(
    'SELECT t1.content AS v1_content, t2.content AS v2_content, ' +
    '  LENGTH(t2.content) - LENGTH(t1.content) AS size_diff ' +
    'FROM templates t1 JOIN templates t2 ON t1.name = t2.name ' +
    'WHERE t1.name = ''email_welcome'' AND t1.version = 1 AND t2.version = 2');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   v1: "%s"', [DS.FieldByName('v1_content').AsString]));
      WriteLn(Format('   v2: "%s"', [DS.FieldByName('v2_content').AsString]));
      if DS.FieldByName('size_diff').AsInteger >= 0 then
        WriteLn(Format('   Size change: +%d chars', [DS.FieldByName('size_diff').AsInteger]))
      else
        WriteLn(Format('   Size change: %d chars', [DS.FieldByName('size_diff').AsInteger]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Latest version per template
  WriteLn('   Latest version per template:');
  DS := Conn.ExecuteQuery(
    'SELECT name, MAX(version) AS latest FROM templates GROUP BY name ORDER BY name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   - %s: v%d', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('latest').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists template variables with types, default values, and required flags, then renders using defaults for optional parameters. }
procedure Demo6_VariableDefaults;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Variable Defaults ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT t.name, v.var_name, v.var_type, ' +
    '  COALESCE(v.default_value, ''(none)'') AS default_val, ' +
    '  CASE WHEN v.required = 1 THEN ''YES'' ELSE ''no'' END AS req ' +
    'FROM template_vars v JOIN templates t ON t.id = v.template_id ' +
    'WHERE t.name = ''email_welcome'' AND t.version = 2 ' +
    'ORDER BY v.required DESC, v.var_name');
  try
    WriteLn('   Variables for email_welcome v2:');
    WriteLn(Format('   %-22s %-8s %-8s %-30s %s',
      ['Variable', 'Type', 'Req?', 'Default', 'Desc']));
    WriteLn('   ' + StringOfChar('-', 90));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT t.name, v.var_name, v.var_type, ' +
    '  COALESCE(v.default_value, ''(none)'') AS default_val, ' +
    '  CASE WHEN v.required = 1 THEN ''YES'' ELSE ''no'' END AS req, ' +
    '  v.description ' +
    'FROM template_vars v JOIN templates t ON t.id = v.template_id ' +
    'WHERE t.name = ''email_welcome'' AND t.version = 2 ' +
    'ORDER BY v.required DESC, v.var_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-22s %-8s %-8s %-30s %s', [
        DS.FieldByName('var_name').AsString,
        DS.FieldByName('var_type').AsString,
        DS.FieldByName('req').AsString,
        DS.FieldByName('default_val').AsString,
        DS.FieldByName('description').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Show rendering with defaults applied
  WriteLn('   Rendering with defaults (only required vars provided):');
  DS := Conn.ExecuteQuery(
    'SELECT REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(content, ' +
    '  ''{{user_name}}'', ''Dave''), ' +
    '  ''{{platform_name}}'', ''Our Platform''), ' +
    '  ''{{email}}'', ''dave@test.com''), ' +
    '  ''{{getting_started_url}}'', ''https://example.com/start''), ' +
    '  ''{{greeting_close}}'', ''Best regards, The Team'') AS rendered ' +
    'FROM templates WHERE name = ''email_welcome'' AND version = 2');
  try
    if not DS.EOF then
      WriteLn(Format('   %s', [DS.FieldByName('rendered').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Executes the full render pipeline: inheritance resolution, variable substitution, and final output generation. }
procedure Demo7_RenderPipeline;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Render Pipeline ===');
  WriteLn;

  WriteLn('   Full render pipeline for report_sales:');
  WriteLn;

  // Step 1: Resolve inheritance (replace block in parent)
  WriteLn('   Step 1: Resolve inheritance (report_base + report_sales block)');
  DS := Conn.ExecuteQuery(
    'SELECT REPLACE(p.content, ''{{block:report_body}}'', b.block_content) AS resolved ' +
    'FROM templates c ' +
    'JOIN templates p ON p.name = c.parent_name AND p.version = 1 ' +
    'JOIN template_blocks b ON b.template_id = c.id AND b.block_name = ''report_body'' ' +
    'WHERE c.name = ''report_sales''');
  try
    if not DS.EOF then
      WriteLn(Format('   Resolved: %s', [DS.FieldByName('resolved').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;

  // Step 2: Substitute variables
  WriteLn('   Step 2: Substitute variables');
  DS := Conn.ExecuteQuery(
    'SELECT REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(' +
    '  REPLACE(p.content, ''{{block:report_body}}'', b.block_content), ' +
    '  ''{{report_title}}'', ''Monthly Sales Report''), ' +
    '  ''{{generated_at}}'', ''2025-01-20 12:00:00''), ' +
    '  ''{{period_start}}'', ''2025-01-01''), ' +
    '  ''{{period_end}}'', ''2025-01-31''), ' +
    '  ''{{currency}}'', ''$''), ' +
    '  ''{{total_revenue}}'', ''45,230.00''), ' +
    '  ''{{order_count}}'', ''312''), ' +
    '  ''{{avg_order}}'', ''145.00'') AS step2 ' +
    'FROM templates c ' +
    'JOIN templates p ON p.name = c.parent_name AND p.version = 1 ' +
    'JOIN template_blocks b ON b.template_id = c.id AND b.block_name = ''report_body'' ' +
    'WHERE c.name = ''report_sales''');
  try
    if not DS.EOF then
      WriteLn(Format('   Partial: %s', [DS.FieldByName('step2').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;

  // Step 3: Final render (replace remaining var)
  WriteLn('   Step 3: Final output (all variables resolved)');
  DS := Conn.ExecuteQuery(
    'SELECT REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(' +
    '  REPLACE(p.content, ''{{block:report_body}}'', b.block_content), ' +
    '  ''{{report_title}}'', ''Monthly Sales Report''), ' +
    '  ''{{generated_at}}'', ''2025-01-20 12:00:00''), ' +
    '  ''{{period_start}}'', ''2025-01-01''), ' +
    '  ''{{period_end}}'', ''2025-01-31''), ' +
    '  ''{{currency}}'', ''$''), ' +
    '  ''{{total_revenue}}'', ''45,230.00''), ' +
    '  ''{{order_count}}'', ''312''), ' +
    '  ''{{avg_order}}'', ''145.00''), ' +
    '  ''{{top_product}}'', ''Premium Widget'') AS final_output ' +
    'FROM templates c ' +
    'JOIN templates p ON p.name = c.parent_name AND p.version = 1 ' +
    'JOIN template_blocks b ON b.template_id = c.id AND b.block_name = ''report_body'' ' +
    'WHERE c.name = ''report_sales''');
  try
    if not DS.EOF then
      WriteLn(Format('   Output: %s', [DS.FieldByName('final_output').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Lists the render log with template name, version, caller, timestamp, and output length, then shows a specific rendered output. }
procedure Demo8_RenderHistory;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Render History ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT template_name, template_version, rendered_by, rendered_at, ' +
    '  LENGTH(output) AS output_len ' +
    'FROM render_log ORDER BY rendered_at');
  try
    WriteLn(Format('   %-18s %-4s %-12s %-20s %s',
      ['Template', 'Ver', 'Rendered By', 'Rendered At', 'Output Len']));
    WriteLn('   ' + StringOfChar('-', 75));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s v%-3d %-12s %-20s %d chars', [
        DS.FieldByName('template_name').AsString,
        DS.FieldByName('template_version').AsInteger,
        DS.FieldByName('rendered_by').AsString,
        DS.FieldByName('rendered_at').AsString,
        DS.FieldByName('output_len').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Show a specific rendered output
  WriteLn('   Last invoice render output:');
  DS := Conn.ExecuteQuery(
    'SELECT output FROM render_log WHERE template_name = ''invoice'' ORDER BY rendered_at DESC LIMIT 1');
  try
    if not DS.EOF then
      WriteLn(Format('   %s', [DS.FieldByName('output').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Identifies required variables without defaults, validates that all required inputs are provided, and detects unresolved placeholders. }
procedure Demo9_TemplateValidation;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Template Validation ===');
  WriteLn;

  // Check which required variables have no default
  WriteLn('   Required variables without defaults (must be provided):');
  DS := Conn.ExecuteQuery(
    'SELECT t.name, t.version, v.var_name, v.var_type ' +
    'FROM template_vars v JOIN templates t ON t.id = v.template_id ' +
    'WHERE v.required = 1 AND v.default_value IS NULL ' +
    'ORDER BY t.name, v.var_name');
  try
    WriteLn(Format('   %-18s %-4s %-22s %s',
      ['Template', 'Ver', 'Variable', 'Type']));
    WriteLn('   ' + StringOfChar('-', 60));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s v%-3d %-22s %s', [
        DS.FieldByName('name').AsString,
        DS.FieldByName('version').AsInteger,
        DS.FieldByName('var_name').AsString,
        DS.FieldByName('var_type').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Simulate validation: check if all required vars are provided
  WriteLn('   Validation check: render email_reset with missing "reset_url":');
  DS := Conn.ExecuteQuery(
    'SELECT v.var_name, v.var_type ' +
    'FROM template_vars v JOIN templates t ON t.id = v.template_id ' +
    'WHERE t.name = ''email_reset'' AND t.version = 1 ' +
    '  AND v.required = 1 AND v.default_value IS NULL ' +
    '  AND v.var_name NOT IN (''user_name'', ''email'')');
  try
    WriteLn('   Missing required variables:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - {{%s}} (%s) - REQUIRED, no default', [
        DS.FieldByName('var_name').AsString,
        DS.FieldByName('var_type').AsString]));
      DS.Next;
    end;
    WriteLn('   => Render BLOCKED: provide all required variables first');
  finally
    DS.Free;
  end;
  WriteLn;

  // Templates with unresolved placeholders detection
  WriteLn('   Unresolved placeholder detection (after partial render):');
  DS := Conn.ExecuteQuery(
    'SELECT ''{{top_product}} still present'' AS issue ' +
    'FROM templates WHERE name = ''report_sales'' ' +
    '  AND content LIKE ''%{{top_product}}%''');
  try
    if not DS.EOF then
      WriteLn(Format('   - %s', [DS.FieldByName('issue').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Demonstrates template engine statistics and usage frequency metrics. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Template Statistics ===');
  WriteLn;

  // Usage frequency
  DS := Conn.ExecuteQuery(
    'SELECT template_name, COUNT(*) AS renders, ' +
    '  COUNT(DISTINCT rendered_by) AS unique_callers ' +
    'FROM render_log GROUP BY template_name ORDER BY renders DESC');
  try
    WriteLn('   Template usage:');
    WriteLn(Format('   %-18s %-8s %s', ['Template', 'Renders', 'Callers']));
    WriteLn('   ' + StringOfChar('-', 38));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-18s %-8d %d', [
        DS.FieldByName('template_name').AsString,
        DS.FieldByName('renders').AsInteger,
        DS.FieldByName('unique_callers').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Version adoption
  DS := Conn.ExecuteQuery(
    'SELECT template_name, template_version, COUNT(*) AS uses ' +
    'FROM render_log WHERE template_name = ''email_welcome'' ' +
    'GROUP BY template_version ORDER BY template_version');
  try
    WriteLn('   email_welcome version adoption:');
    while not DS.EOF do
    begin
      WriteLn(Format('   - v%d: %d render(s)', [
        DS.FieldByName('template_version').AsInteger,
        DS.FieldByName('uses').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;

  // Overall metrics
  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  (SELECT COUNT(DISTINCT name) FROM templates) AS template_count, ' +
    '  (SELECT COUNT(*) FROM templates) AS version_count, ' +
    '  (SELECT COUNT(*) FROM template_vars) AS total_vars, ' +
    '  (SELECT COUNT(*) FROM render_log) AS total_renders, ' +
    '  (SELECT COUNT(DISTINCT rendered_by) FROM render_log) AS unique_renderers, ' +
    '  (SELECT AVG(LENGTH(output)) FROM render_log) AS avg_output_len');
  try
    if not DS.EOF then
    begin
      WriteLn('   Overall:');
      WriteLn(Format('   - Templates:       %d', [DS.FieldByName('template_count').AsInteger]));
      WriteLn(Format('   - Total versions:  %d', [DS.FieldByName('version_count').AsInteger]));
      WriteLn(Format('   - Variables:       %d', [DS.FieldByName('total_vars').AsInteger]));
      WriteLn(Format('   - Total renders:   %d', [DS.FieldByName('total_renders').AsInteger]));
      WriteLn(Format('   - Unique callers:  %d', [DS.FieldByName('unique_renderers').AsInteger]));
      WriteLn(Format('   - Avg output:      %.0f chars', [DS.FieldByName('avg_output_len').AsFloat]));
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

begin
  WriteLn('Example 146: Template Engine');
  WriteLn('======================================================================');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertData;

    Demo1_TemplateRegistry;
    Demo2_ParameterizedTemplates;
    Demo3_VariableSubstitution;
    Demo4_TemplateInheritance;
    Demo5_TemplateVersions;
    Demo6_VariableDefaults;
    Demo7_RenderPipeline;
    Demo8_RenderHistory;
    Demo9_TemplateValidation;
    Demo10_Statistics;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Free;
  end;
end.
