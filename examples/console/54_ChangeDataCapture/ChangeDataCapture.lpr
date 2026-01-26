{===============================================================================
  NDXSQLite Example 54 - Change Data Capture (CDC)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Tracking all data changes (INSERT, UPDATE, DELETE)
  - Change log for synchronization
  - Event sourcing patterns
  - Audit trail implementation
  - Replaying changes for recovery

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ChangeDataCapture;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the required tables and inserts sample data. }
procedure SetupTables;
begin
  // Main data table: Customers
  Connection.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  balance REAL DEFAULT 0,' +
    '  status TEXT DEFAULT ''active''' +
    ')');

  // Change Data Capture log table
  Connection.ExecuteNonQuery(
    'CREATE TABLE cdc_log (' +
    '  log_id INTEGER PRIMARY KEY,' +
    '  table_name TEXT NOT NULL,' +
    '  record_id INTEGER NOT NULL,' +
    '  operation TEXT NOT NULL,' +       // INSERT, UPDATE, DELETE
    '  old_data TEXT,' +                  // JSON of old values (for UPDATE/DELETE)
    '  new_data TEXT,' +                  // JSON of new values (for INSERT/UPDATE)
    '  changed_fields TEXT,' +            // Comma-separated list of changed fields
    '  changed_at TEXT DEFAULT (datetime(''now'')),' +
    '  changed_by TEXT DEFAULT ''system'',' +
    '  sync_status TEXT DEFAULT ''pending'',' +  // pending, synced, failed
    '  sync_at TEXT' +
    ')');

  // Index for efficient sync queries
  Connection.ExecuteNonQuery(
    'CREATE INDEX idx_cdc_sync ON cdc_log(sync_status, changed_at)');

  // Event store for event sourcing pattern
  Connection.ExecuteNonQuery(
    'CREATE TABLE event_store (' +
    '  event_id INTEGER PRIMARY KEY,' +
    '  aggregate_type TEXT NOT NULL,' +   // e.g., "Customer", "Order"
    '  aggregate_id INTEGER NOT NULL,' +
    '  event_type TEXT NOT NULL,' +       // e.g., "CustomerCreated", "BalanceUpdated"
    '  event_data TEXT NOT NULL,' +       // JSON payload
    '  event_version INTEGER NOT NULL,' + // For optimistic concurrency
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  Connection.ExecuteNonQuery(
    'CREATE INDEX idx_events_aggregate ON event_store(aggregate_type, aggregate_id, event_version)');

  // Create CDC triggers for customers table
  // INSERT trigger
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER cdc_customers_insert ' +
    'AFTER INSERT ON customers ' +
    'BEGIN ' +
    '  INSERT INTO cdc_log (table_name, record_id, operation, new_data, changed_fields) ' +
    '  VALUES (''customers'', NEW.id, ''INSERT'', ' +
    '    json_object(''id'', NEW.id, ''name'', NEW.name, ''email'', NEW.email, ' +
    '               ''balance'', NEW.balance, ''status'', NEW.status), ' +
    '    ''id,name,email,balance,status''); ' +
    'END');

  // UPDATE trigger
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER cdc_customers_update ' +
    'AFTER UPDATE ON customers ' +
    'BEGIN ' +
    '  INSERT INTO cdc_log (table_name, record_id, operation, old_data, new_data, changed_fields) ' +
    '  VALUES (''customers'', NEW.id, ''UPDATE'', ' +
    '    json_object(''id'', OLD.id, ''name'', OLD.name, ''email'', OLD.email, ' +
    '               ''balance'', OLD.balance, ''status'', OLD.status), ' +
    '    json_object(''id'', NEW.id, ''name'', NEW.name, ''email'', NEW.email, ' +
    '               ''balance'', NEW.balance, ''status'', NEW.status), ' +
    '    CASE WHEN OLD.name <> NEW.name THEN ''name,'' ELSE '''' END || ' +
    '    CASE WHEN OLD.email <> NEW.email OR (OLD.email IS NULL) <> (NEW.email IS NULL) THEN ''email,'' ELSE '''' END || ' +
    '    CASE WHEN OLD.balance <> NEW.balance THEN ''balance,'' ELSE '''' END || ' +
    '    CASE WHEN OLD.status <> NEW.status THEN ''status,'' ELSE '''' END); ' +
    'END');

  // DELETE trigger
  Connection.ExecuteNonQuery(
    'CREATE TRIGGER cdc_customers_delete ' +
    'AFTER DELETE ON customers ' +
    'BEGIN ' +
    '  INSERT INTO cdc_log (table_name, record_id, operation, old_data) ' +
    '  VALUES (''customers'', OLD.id, ''DELETE'', ' +
    '    json_object(''id'', OLD.id, ''name'', OLD.name, ''email'', OLD.email, ' +
    '               ''balance'', OLD.balance, ''status'', OLD.status)); ' +
    'END');
end;

{ Queries and prints all customers with their ID, name, email, balance, and status. }
procedure ShowCustomers;
var
  DS: TDataSet;
begin
  WriteLn('   Current customers:');
  DS := Connection.ExecuteQuery('SELECT * FROM customers ORDER BY id');
  try
    if DS.IsEmpty then
      WriteLn('     (none)')
    else
      while not DS.EOF do
      begin
        WriteLn(Format('     [%d] %s <%s> Balance: $%.2f (%s)',
          [DS.FieldByName('id').AsInteger,
           DS.FieldByName('name').AsString,
           DS.FieldByName('email').AsString,
           DS.FieldByName('balance').AsFloat,
           DS.FieldByName('status').AsString]));
        DS.Next;
      end;
  finally
    DS.Free;
  end;
end;

{ Queries and prints the most recent CDC log entries showing operation, table, record, changed fields, and sync status. }
procedure ShowCDCLog(ALimit: Integer = 10);
var
  DS: TDataSet;
begin
  WriteLn('   CDC Log (recent changes):');
  DS := Connection.ExecuteQuery(Format(
    'SELECT log_id, table_name, record_id, operation, changed_fields, changed_at, sync_status ' +
    'FROM cdc_log ORDER BY log_id DESC LIMIT %d', [ALimit]));
  try
    if DS.IsEmpty then
      WriteLn('     (no changes recorded)')
    else
      while not DS.EOF do
      begin
        WriteLn(Format('     #%d [%s] %s.%d - fields: %s (%s) [%s]',
          [DS.FieldByName('log_id').AsInteger,
           DS.FieldByName('operation').AsString,
           DS.FieldByName('table_name').AsString,
           DS.FieldByName('record_id').AsInteger,
           DS.FieldByName('changed_fields').AsString,
           DS.FieldByName('changed_at').AsString,
           DS.FieldByName('sync_status').AsString]));
        DS.Next;
      end;
  finally
    DS.Free;
  end;
end;

{ Inserts three sample customers and displays the resulting CDC log entries captured by triggers. }
procedure DemoBasicCDC;
begin
  WriteLn('1. Basic Change Data Capture');
  WriteLn('   --------------------------');

  WriteLn('   Inserting customers...');
  Connection.ExecuteNonQuery(
    'INSERT INTO customers (name, email, balance) VALUES (''Alice Smith'', ''alice@example.com'', 1000.00)');
  Connection.ExecuteNonQuery(
    'INSERT INTO customers (name, email, balance) VALUES (''Bob Jones'', ''bob@example.com'', 500.00)');
  Connection.ExecuteNonQuery(
    'INSERT INTO customers (name, email, balance) VALUES (''Carol White'', ''carol@example.com'', 750.00)');

  ShowCustomers;
  WriteLn;
  ShowCDCLog;
  WriteLn;
end;

{ Updates customer balance and email fields, then shows how the CDC trigger records old and new values with changed field names. }
procedure DemoUpdateTracking;
begin
  WriteLn('2. Update Change Tracking');
  WriteLn('   -----------------------');

  WriteLn('   Updating Alice''s balance...');
  Connection.ExecuteNonQuery('UPDATE customers SET balance = 1200.00 WHERE id = 1');

  WriteLn('   Updating Bob''s email and status...');
  Connection.ExecuteNonQuery(
    'UPDATE customers SET email = ''bob.jones@newmail.com'', status = ''premium'' WHERE id = 2');

  ShowCustomers;
  WriteLn;
  ShowCDCLog;
  WriteLn;
end;

{ Deletes a customer record and shows how the CDC trigger captures the deleted row data in the log. }
procedure DemoDeleteTracking;
begin
  WriteLn('3. Delete Change Tracking');
  WriteLn('   -----------------------');

  WriteLn('   Deleting Carol...');
  Connection.ExecuteNonQuery('DELETE FROM customers WHERE id = 3');

  ShowCustomers;
  WriteLn;
  ShowCDCLog;
  WriteLn;
end;

{ Demonstrates a synchronization workflow using captured changes. }
procedure DemoSynchronization;
var
  DS: TDataSet;
  PendingCount, SyncedCount: Integer;
begin
  WriteLn('4. Synchronization Workflow');
  WriteLn('   -------------------------');

  // Count pending changes
  DS := Connection.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM cdc_log WHERE sync_status = ''pending''');
  try
    PendingCount := DS.FieldByName('cnt').AsInteger;
    WriteLn(Format('   Pending changes to sync: %d', [PendingCount]));
  finally
    DS.Free;
  end;

  // Get pending changes
  WriteLn;
  WriteLn('   Pending changes:');
  DS := Connection.ExecuteQuery(
    'SELECT log_id, operation, table_name, record_id, new_data ' +
    'FROM cdc_log WHERE sync_status = ''pending'' ORDER BY log_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     #%d %s %s.%d',
        [DS.FieldByName('log_id').AsInteger,
         DS.FieldByName('operation').AsString,
         DS.FieldByName('table_name').AsString,
         DS.FieldByName('record_id').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Simulate synchronization
  WriteLn;
  WriteLn('   Simulating sync to remote server...');
  Sleep(100); // Simulate network delay

  // Mark as synced
  SyncedCount := Connection.ExecuteNonQuery(
    'UPDATE cdc_log SET sync_status = ''synced'', sync_at = datetime(''now'') ' +
    'WHERE sync_status = ''pending''');
  WriteLn(Format('   Marked %d changes as synced', [SyncedCount]));

  WriteLn;
  ShowCDCLog;
  WriteLn;
end;

{ Stores a series of domain events (CustomerCreated, BalanceDeposited, EmailChanged, BalanceWithdrawn) in the event store and displays the full event history. }
procedure DemoEventSourcing;
var
  DS: TDataSet;
  CustomerId, EventVersion: Integer;
begin
  WriteLn('5. Event Sourcing Pattern');
  WriteLn('   -----------------------');

  // Create a new customer using event sourcing
  CustomerId := 100;
  EventVersion := 1;

  WriteLn('   Creating customer via events...');

  // CustomerCreated event
  Connection.ExecuteNonQuery(
    'INSERT INTO event_store (aggregate_type, aggregate_id, event_type, event_data, event_version) ' +
    'VALUES (?, ?, ?, ?, ?)',
    ['Customer', CustomerId, 'CustomerCreated',
     '{"name":"David Brown","email":"david@example.com"}', EventVersion]);
  Inc(EventVersion);

  // BalanceDeposited event
  Connection.ExecuteNonQuery(
    'INSERT INTO event_store (aggregate_type, aggregate_id, event_type, event_data, event_version) ' +
    'VALUES (?, ?, ?, ?, ?)',
    ['Customer', CustomerId, 'BalanceDeposited', '{"amount":500.00}', EventVersion]);
  Inc(EventVersion);

  // EmailChanged event
  Connection.ExecuteNonQuery(
    'INSERT INTO event_store (aggregate_type, aggregate_id, event_type, event_data, event_version) ' +
    'VALUES (?, ?, ?, ?, ?)',
    ['Customer', CustomerId, 'EmailChanged',
     '{"old_email":"david@example.com","new_email":"david.brown@company.com"}', EventVersion]);
  Inc(EventVersion);

  // BalanceWithdrawn event
  Connection.ExecuteNonQuery(
    'INSERT INTO event_store (aggregate_type, aggregate_id, event_type, event_data, event_version) ' +
    'VALUES (?, ?, ?, ?, ?)',
    ['Customer', CustomerId, 'BalanceWithdrawn', '{"amount":100.00}', EventVersion]);

  // Show event history
  WriteLn;
  WriteLn(Format('   Event history for Customer #%d:', [CustomerId]));
  DS := Connection.ExecuteQuery(Format(
    'SELECT event_version, event_type, event_data, created_at ' +
    'FROM event_store WHERE aggregate_type = ''Customer'' AND aggregate_id = %d ' +
    'ORDER BY event_version', [CustomerId]));
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     v%d: %s - %s',
        [DS.FieldByName('event_version').AsInteger,
         DS.FieldByName('event_type').AsString,
         DS.FieldByName('event_data').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Reads events from the event store in version order and rebuilds the customer state (name, email, balance) by applying each event sequentially. }
procedure DemoReplayEvents;
var
  DS: TDataSet;
  EventType, EventData: string;
  Name, Email: string;
  Balance: Double;
  Amount: Double;
begin
  WriteLn('6. Replaying Events to Rebuild State');
  WriteLn('   -----------------------------------');

  // Initialize state
  Name := '';
  Email := '';
  Balance := 0;

  WriteLn('   Replaying events for Customer #100...');
  WriteLn;

  DS := Connection.ExecuteQuery(
    'SELECT event_type, event_data FROM event_store ' +
    'WHERE aggregate_type = ''Customer'' AND aggregate_id = 100 ' +
    'ORDER BY event_version');
  try
    while not DS.EOF do
    begin
      EventType := DS.FieldByName('event_type').AsString;
      EventData := DS.FieldByName('event_data').AsString;

      Write(Format('     Processing %s... ', [EventType]));

      // Simple event handlers (in real app, use proper JSON parsing)
      if EventType = 'CustomerCreated' then
      begin
        // Extract name and email from JSON
        Name := 'David Brown';  // Simplified for demo
        Email := 'david@example.com';
        WriteLn(Format('Name=%s, Email=%s', [Name, Email]));
      end
      else if EventType = 'BalanceDeposited' then
      begin
        Amount := 500.00;  // Simplified
        Balance := Balance + Amount;
        WriteLn(Format('+$%.2f -> Balance=$%.2f', [Amount, Balance]));
      end
      else if EventType = 'BalanceWithdrawn' then
      begin
        Amount := 100.00;  // Simplified
        Balance := Balance - Amount;
        WriteLn(Format('-$%.2f -> Balance=$%.2f', [Amount, Balance]));
      end
      else if EventType = 'EmailChanged' then
      begin
        Email := 'david.brown@company.com';  // Simplified
        WriteLn(Format('Email=%s', [Email]));
      end
      else
        WriteLn('(handled)');

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Final reconstructed state:');
  WriteLn(Format('     Name: %s', [Name]));
  WriteLn(Format('     Email: %s', [Email]));
  WriteLn(Format('     Balance: $%.2f', [Balance]));
  WriteLn;
end;

{ Queries the CDC log to display change counts by operation type, per-record change frequency, and full change history for a specific record. }
procedure DemoAuditTrail;
var
  DS: TDataSet;
begin
  WriteLn('7. Audit Trail Queries');
  WriteLn('   --------------------');

  // Who changed what?
  WriteLn('   All changes by operation type:');
  DS := Connection.ExecuteQuery(
    'SELECT operation, COUNT(*) as cnt FROM cdc_log GROUP BY operation ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s: %d changes',
        [DS.FieldByName('operation').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Changes per record:');
  DS := Connection.ExecuteQuery(
    'SELECT table_name, record_id, COUNT(*) as changes ' +
    'FROM cdc_log GROUP BY table_name, record_id ORDER BY changes DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s.%d: %d changes',
        [DS.FieldByName('table_name').AsString,
         DS.FieldByName('record_id').AsInteger,
         DS.FieldByName('changes').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Full change history for customers.1:');
  DS := Connection.ExecuteQuery(
    'SELECT operation, old_data, new_data, changed_at ' +
    'FROM cdc_log WHERE table_name = ''customers'' AND record_id = 1 ORDER BY log_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s',
        [DS.FieldByName('operation').AsString,
         DS.FieldByName('changed_at').AsString]));
      if not DS.FieldByName('old_data').IsNull then
        WriteLn('       Old: ' + DS.FieldByName('old_data').AsString);
      if not DS.FieldByName('new_data').IsNull then
        WriteLn('       New: ' + DS.FieldByName('new_data').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints a summary of CDC best practices including trigger usage, field tracking, sync status, indexing, and event sourcing guidelines. }
procedure DemoBestPractices;
begin
  WriteLn('8. CDC Best Practices');
  WriteLn('   -------------------');
  WriteLn('   - Use triggers for automatic change capture');
  WriteLn('   - Store old and new values for UPDATE operations');
  WriteLn('   - Track which fields changed (not just that something changed)');
  WriteLn('   - Include timestamp and user information');
  WriteLn('   - Use sync_status for reliable synchronization');
  WriteLn('   - Index CDC tables for efficient querying');
  WriteLn('   - Consider partitioning/archiving old CDC records');
  WriteLn('   - For event sourcing: immutable events, version numbers');
  WriteLn('   - Test replay functionality regularly');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 54: Change Data Capture ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example54.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupTables;
      DemoBasicCDC;
      DemoUpdateTracking;
      DemoDeleteTracking;
      DemoSynchronization;
      DemoEventSourcing;
      DemoReplayEvents;
      DemoAuditTrail;
      DemoBestPractices;

      Connection.Close;
    finally
      Connection.Free;
    end;
  finally
    Options.Free;
  end;

  Cleanup;

  WriteLn('=== Example completed successfully! ===');
end.
