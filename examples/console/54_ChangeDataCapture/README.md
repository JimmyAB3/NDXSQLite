# Example 54: Change Data Capture (CDC)

This example demonstrates tracking all data changes for synchronization, auditing, and event sourcing with NDXSQLite.

## Features Demonstrated

- **Automatic Change Tracking**: Triggers capture INSERT, UPDATE, DELETE
- **Change Log**: Complete record of all modifications
- **Synchronization Support**: Track sync status for replication
- **Event Sourcing**: Store events instead of state
- **Event Replay**: Rebuild state from event history
- **Audit Trail**: Who changed what, when

## Database Schema

### CDC Log Table
```sql
CREATE TABLE cdc_log (
  log_id INTEGER PRIMARY KEY,
  table_name TEXT NOT NULL,
  record_id INTEGER NOT NULL,
  operation TEXT NOT NULL,       -- INSERT, UPDATE, DELETE
  old_data TEXT,                  -- JSON of previous values
  new_data TEXT,                  -- JSON of new values
  changed_fields TEXT,            -- Which fields changed
  changed_at TEXT DEFAULT (datetime('now')),
  changed_by TEXT DEFAULT 'system',
  sync_status TEXT DEFAULT 'pending',  -- pending, synced, failed
  sync_at TEXT
);
```

### Event Store (Event Sourcing)
```sql
CREATE TABLE event_store (
  event_id INTEGER PRIMARY KEY,
  aggregate_type TEXT NOT NULL,   -- e.g., "Customer"
  aggregate_id INTEGER NOT NULL,
  event_type TEXT NOT NULL,       -- e.g., "CustomerCreated"
  event_data TEXT NOT NULL,       -- JSON payload
  event_version INTEGER NOT NULL, -- Optimistic concurrency
  created_at TEXT DEFAULT (datetime('now'))
);
```

## CDC Triggers

### INSERT Trigger
```sql
CREATE TRIGGER cdc_customers_insert
AFTER INSERT ON customers
BEGIN
  INSERT INTO cdc_log (table_name, record_id, operation, new_data, changed_fields)
  VALUES ('customers', NEW.id, 'INSERT',
    json_object('id', NEW.id, 'name', NEW.name, 'email', NEW.email),
    'id,name,email');
END;
```

### UPDATE Trigger
```sql
CREATE TRIGGER cdc_customers_update
AFTER UPDATE ON customers
BEGIN
  INSERT INTO cdc_log (table_name, record_id, operation, old_data, new_data, changed_fields)
  VALUES ('customers', NEW.id, 'UPDATE',
    json_object('id', OLD.id, 'name', OLD.name, 'email', OLD.email),
    json_object('id', NEW.id, 'name', NEW.name, 'email', NEW.email),
    -- Track which fields changed
    CASE WHEN OLD.name <> NEW.name THEN 'name,' ELSE '' END ||
    CASE WHEN OLD.email <> NEW.email THEN 'email,' ELSE '' END);
END;
```

### DELETE Trigger
```sql
CREATE TRIGGER cdc_customers_delete
AFTER DELETE ON customers
BEGIN
  INSERT INTO cdc_log (table_name, record_id, operation, old_data)
  VALUES ('customers', OLD.id, 'DELETE',
    json_object('id', OLD.id, 'name', OLD.name, 'email', OLD.email));
END;
```

## Synchronization Workflow

```pascal
// Get pending changes
DS := Connection.ExecuteQuery(
  'SELECT * FROM cdc_log WHERE sync_status = ''pending'' ORDER BY log_id');

// Process each change
while not DS.EOF do
begin
  // Send to remote server
  SyncToRemote(DS);
  DS.Next;
end;

// Mark as synced
Connection.ExecuteNonQuery(
  'UPDATE cdc_log SET sync_status = ''synced'', sync_at = datetime(''now'') ' +
  'WHERE sync_status = ''pending''');
```

## Event Sourcing Pattern

### Store Events
```pascal
// Create customer via events
Connection.ExecuteNonQuery(
  'INSERT INTO event_store (aggregate_type, aggregate_id, event_type, event_data, event_version) ' +
  'VALUES (?, ?, ?, ?, ?)',
  ['Customer', 100, 'CustomerCreated', '{"name":"Alice","email":"alice@example.com"}', 1]);

Connection.ExecuteNonQuery(
  'INSERT INTO event_store (...) VALUES (...)',
  ['Customer', 100, 'BalanceDeposited', '{"amount":500.00}', 2]);
```

### Replay Events to Rebuild State
```pascal
DS := Connection.ExecuteQuery(
  'SELECT event_type, event_data FROM event_store ' +
  'WHERE aggregate_type = ''Customer'' AND aggregate_id = 100 ' +
  'ORDER BY event_version');

while not DS.EOF do
begin
  EventType := DS.FieldByName('event_type').AsString;

  if EventType = 'CustomerCreated' then
    // Initialize customer
  else if EventType = 'BalanceDeposited' then
    Balance := Balance + Amount;

  DS.Next;
end;
```

## Audit Queries

```sql
-- All changes by operation type
SELECT operation, COUNT(*) FROM cdc_log GROUP BY operation;

-- Full history for a record
SELECT operation, old_data, new_data, changed_at
FROM cdc_log WHERE table_name = 'customers' AND record_id = 1
ORDER BY log_id;

-- Changes per record
SELECT table_name, record_id, COUNT(*) as changes
FROM cdc_log GROUP BY table_name, record_id;
```

## Build and Run

```bash
cd 54_ChangeDataCapture
fpc ChangeDataCapture.lpr
./ChangeDataCapture
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use triggers for automatic change capture
- Store both old and new values for UPDATE
- Track which fields changed
- Include timestamp and user information
- Use sync_status for reliable synchronization
- For event sourcing: immutable events, version numbers
