# Example 70: Preupdate Hooks

This example demonstrates SQLite preupdate hook mechanism for inspecting data before modifications, building audit trails, and tracking changes across multiple tables.

## What you'll learn

- How to check if preupdate hooks are available
- How to build a comprehensive audit trail system
- How to track changes across multiple tables
- How to detect trigger-caused modifications
- How to export changes as JSON
- How to handle batch operations efficiently

## Key concepts

### Checking Availability

```pascal
if TNDXSQLitePreupdate.IsAvailable then
  WriteLn('Preupdate hooks available')
else
  WriteLn('Not available - requires SQLITE_ENABLE_PREUPDATE_HOOK');
```

### Basic Setup

```pascal
Preupdate := TNDXSQLitePreupdate.Create(Connection);
try
  Preupdate.Enable;
  // ... perform operations
  Preupdate.Disable;
finally
  Preupdate.Free;
end;
```

### Building an Audit Trail Handler

```pascal
type
  TAuditRecord = record
    Timestamp: TDateTime;
    Operation: string;
    TableName: string;
    RowId: Int64;
    OldValues: string;
    NewValues: string;
  end;

  TAuditTrailHandler = class
  private
    FAuditLog: array of TAuditRecord;
    FChangeCount: Integer;
  public
    InsertCount: Integer;
    UpdateCount: Integer;
    DeleteCount: Integer;

    procedure HandlePreupdate(Sender: TNDXSQLitePreupdate;
      const Info: TNDXPreupdateInfo);
    procedure Clear;
    procedure PrintSummary;
    procedure PrintAuditLog;
    property ChangeCount: Integer read FChangeCount;
  end;

procedure TAuditTrailHandler.HandlePreupdate(Sender: TNDXSQLitePreupdate;
  const Info: TNDXPreupdateInfo);
var
  I: Integer;
  OldVal, NewVal: Variant;
  OldValues, NewValues: string;
  AuditRec: TAuditRecord;
begin
  OldValues := '';
  NewValues := '';

  case Info.Operation of
    poInsert:
      begin
        Inc(InsertCount);
        for I := 0 to Info.ColumnCount - 1 do
        begin
          NewVal := Sender.GetNewValue(I);
          if I > 0 then NewValues := NewValues + ', ';
          NewValues := NewValues + VarToStr(NewVal);
        end;
      end;

    poUpdate:
      begin
        Inc(UpdateCount);
        for I := 0 to Info.ColumnCount - 1 do
        begin
          OldVal := Sender.GetOldValue(I);
          NewVal := Sender.GetNewValue(I);
          if I > 0 then
          begin
            OldValues := OldValues + ', ';
            NewValues := NewValues + ', ';
          end;
          OldValues := OldValues + VarToStr(OldVal);
          NewValues := NewValues + VarToStr(NewVal);
        end;
      end;

    poDelete:
      begin
        Inc(DeleteCount);
        for I := 0 to Info.ColumnCount - 1 do
        begin
          OldVal := Sender.GetOldValue(I);
          if I > 0 then OldValues := OldValues + ', ';
          OldValues := OldValues + VarToStr(OldVal);
        end;
      end;
  end;

  // Store in audit log
  AuditRec.Timestamp := Now;
  AuditRec.Operation := TNDXSQLitePreupdate.OperationToString(Info.Operation);
  AuditRec.TableName := Info.TableName;
  AuditRec.RowId := Info.NewRowId;
  if Info.Operation = poDelete then
    AuditRec.RowId := Info.OldRowId;
  AuditRec.OldValues := OldValues;
  AuditRec.NewValues := NewValues;

  SetLength(FAuditLog, Length(FAuditLog) + 1);
  FAuditLog[High(FAuditLog)] := AuditRec;
  Inc(FChangeCount);
end;
```

### Using the Audit Handler

```pascal
Handler := TAuditTrailHandler.Create;
try
  Handler.Clear;

  Preupdate := TNDXSQLitePreupdate.Create(Conn);
  try
    Preupdate.SetCallback(@Handler.HandlePreupdate);
    Preupdate.Enable;

    // Perform operations - these will be tracked
    Conn.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Alice'', 50000)');
    Conn.ExecuteNonQuery('UPDATE employees SET salary = 55000 WHERE id = 1');
    Conn.ExecuteNonQuery('DELETE FROM employees WHERE id = 1');

    Preupdate.Disable;

    // Print results
    Handler.PrintSummary;
    Handler.PrintAuditLog;
  finally
    Preupdate.Free;
  end;
finally
  Handler.Free;
end;
```

### Preupdate Info Record

```pascal
TNDXPreupdateInfo = record
  Operation: TNDXPreupdateOp;  // poInsert, poUpdate, poDelete
  DatabaseName: string;         // 'main', 'temp', or attached name
  TableName: string;            // Name of affected table
  OldRowId: Int64;             // RowId before operation
  NewRowId: Int64;             // RowId after operation
  ColumnCount: Integer;        // Number of columns
  TriggerDepth: Integer;       // Trigger nesting level (0 = direct)
end;
```

### Automatic Logging Mode

```pascal
Preupdate.EnableLogging;
Preupdate.TrackChanges := True;
Preupdate.Enable;

// ... perform operations

Preupdate.Disable;

// Review log
LogList := Preupdate.GetLog;
for I := 0 to LogList.Count - 1 do
  WriteLn(LogList[I]);
```

### Change Tracker with JSON Export

```pascal
Tracker := TNDXChangeTracker.Create(Connection);
try
  Tracker.Start('employees');  // Filter by table (optional)

  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Eve'', 70000)');
  Conn.ExecuteNonQuery('UPDATE employees SET salary = 72000 WHERE id = 1');
  Conn.ExecuteNonQuery('DELETE FROM employees WHERE id = 1');

  Tracker.Stop;

  // Get changes as JSON
  WriteLn('Changes tracked: ', Tracker.GetChangeCount);
  JSON := Tracker.GetChangesAsJSON;
  WriteLn(JSON);
  // Output: [{"op":"INSERT",...},{"op":"UPDATE",...},{"op":"DELETE",...}]
finally
  Tracker.Free;
end;
```

### Multi-Table Tracking

Preupdate hooks track changes across all tables automatically:

```pascal
// Operations on multiple tables - all tracked
Conn.ExecuteNonQuery('INSERT INTO departments VALUES (1, ''Engineering'', 500000)');
Conn.ExecuteNonQuery('INSERT INTO employees VALUES (1, ''Alice'', 50000, 1)');
Conn.ExecuteNonQuery('UPDATE departments SET budget = 550000 WHERE id = 1');
```

### Trigger Depth Detection

Detect whether a change came from a direct operation or a trigger:

```pascal
// Create a trigger
Conn.ExecuteNonQuery(
  'CREATE TRIGGER update_timestamp AFTER UPDATE ON employees ' +
  'BEGIN UPDATE departments SET budget = budget WHERE id = 1; END');

// Update employee - will fire trigger
Conn.ExecuteNonQuery('UPDATE employees SET salary = 53000 WHERE id = 1');

// Handler will see:
// 1. Direct UPDATE on employees (TriggerDepth = 0)
// 2. Trigger-caused UPDATE on departments (TriggerDepth = 1)
```

### Batch Operations Tracking

```pascal
// Batch insert
Conn.BeginTransaction;
for I := 10 to 19 do
  Conn.ExecuteNonQuery('INSERT INTO employees VALUES (?, ?, ?, ?)',
    [I, 'Employee' + IntToStr(I), 40000 + I * 1000, 'Batch']);
Conn.Commit;

// Batch update - affects multiple rows
Conn.ExecuteNonQuery('UPDATE employees SET salary = salary + 1000 WHERE department = ''Batch''');

// Batch delete
Conn.ExecuteNonQuery('DELETE FROM employees WHERE department = ''Batch''');

// Summary: 10 inserts + 10 updates + 10 deletes = 30 total changes tracked
```

## Operations

| Operation | Description |
|-----------|-------------|
| poInsert | Row is being inserted |
| poUpdate | Row is being updated |
| poDelete | Row is being deleted |

## Operation String Helpers

```pascal
WriteLn(TNDXSQLitePreupdate.OperationToString(poInsert));  // "INSERT"
WriteLn(TNDXSQLitePreupdate.OperationToString(poUpdate));  // "UPDATE"
WriteLn(TNDXSQLitePreupdate.OperationToString(poDelete));  // "DELETE"
```

## Building

```bash
lazbuild PreupdateHooks.lpi
```

## Running

```bash
./PreupdateHooks      # Linux/macOS
PreupdateHooks.exe    # Windows
```

## Expected output

```
=== NDXSQLite Example 70: Preupdate Hooks ===

1. Checking Preupdate Hooks Availability:
   Preupdate hooks are AVAILABLE (or NOT available with fallback message)

2. Basic Preupdate Enable/Disable:
   Initial state - Enabled: FALSE
   After Enable - Enabled: TRUE
   After Disable - Enabled: FALSE

3. Audit Trail with Custom Handler:
   Performing database operations...
   - INSERT employee Alice
   - INSERT employee Bob
   - INSERT employee Carol
   - UPDATE Alice salary
   - UPDATE all Engineering salaries
   - DELETE Bob

   === Change Summary ===
   Inserts: 3
   Updates: 3
   Deletes: 1
   Total changes: 7

   Audit Log (7 entries):
   [12:30:45.123] INSERT on employees (RowId: 1)
      New: 1, Alice, 50000, Engineering
   ...

4. Automatic Logging Mode:
   Log entries captured: 3
   [12:30:45.200] UPDATE on main.employees: RowId 1 -> 1 [Col2: 50000 -> 52000]
   [12:30:45.210] UPDATE on main.employees: RowId 2 -> 2 [Col3: Sales -> Marketing]
   [12:30:45.220] INSERT on main.employees: RowId 3 -> 3

5. Change Tracker with JSON Export:
   Changes tracked: 3
   JSON output:
   [{"op":"INSERT",...},{"op":"UPDATE",...},{"op":"DELETE",...}]

6. Multi-Table Tracking:
   Operations tracked across tables:
   Inserts: 2
   Updates: 2
   Deletes: 0
   Total changes: 4

7. Trigger Depth Detection:
   Creating trigger for cascade updates...
   Updating employee (will fire trigger)...
   Changes detected:
   Inserts: 0
   Updates: 2
   Deletes: 0
   Total changes: 2
   (1 direct update + 1 trigger-caused update)

8. Operation String Helpers:
   OperationToString(poInsert) = "INSERT"
   OperationToString(poUpdate) = "UPDATE"
   OperationToString(poDelete) = "DELETE"

9. Batch Operations Tracking:
   Batch operations summary:
   Inserts: 10
   Updates: 10
   Deletes: 10
   Total changes: 30
   Expected: 10 inserts, 10 updates, 10 deletes = 30 total

10. Final Database State:
    Employees: 3
    Departments: 2

=== Example completed successfully! ===
```

## Use cases

- **Audit logging**: Track who changed what and when
- **Change data capture (CDC)**: Replicate changes to other systems
- **Data validation**: Verify changes before they're committed
- **Trigger debugging**: Understand trigger cascades
- **Replication systems**: Capture changes for sync
- **Undo/redo functionality**: Record changes for reversal
- **Compliance**: Meet regulatory requirements for data tracking

## Notes

- Requires SQLite compiled with `SQLITE_ENABLE_PREUPDATE_HOOK`
- Values are only accessible during the callback execution
- GetTriggerDepth() shows if change is from a trigger (0 = direct)
- Performance impact: callbacks are invoked for every row modification
- The wrapper provides fallback logging when hooks are not available

## Cross-Platform

This example works on Windows, Linux, and macOS.
