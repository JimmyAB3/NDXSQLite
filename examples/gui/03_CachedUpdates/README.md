# NDXSQLite GUI Example 03 - Cached Updates

Demonstrates batch editing with in-memory change tracking and Apply/Cancel functionality.

## Overview

This example showcases the `CachedUpdates` feature of `TNDXSQLiteDataSet`, which allows multiple edits to be held in memory before committing them to the database. This provides transaction-like behavior with visual feedback and the ability to cancel all pending changes at once.

## Features Demonstrated

| Feature | Description |
|---------|-------------|
| CachedUpdates Mode | In-memory change tracking before database commit |
| ApplyUpdates | Commit all pending changes to database in one operation |
| CancelUpdates | Discard all pending changes and reload original data |
| Visual Feedback | Real-time display of pending insert/update/delete counts |
| Mode Toggle | Switch between cached and direct update modes |
| Close Protection | Warning dialog when closing with unsaved changes |

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                           TfrmMain                                  │
├─────────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ [✓] Cached Updates    [Apply] [Cancel] [Refresh]            │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ Pending: 2 insert(s), 1 update(s), 0 delete(s)              │◄───┼── Visual feedback
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ DBGrid (employees table)                                     │   │
│  │ ┌────┬────────────┬────────────┬─────────────┬──────────┐    │   │
│  │ │ ID │ First Name │ Last Name  │ Department  │ Salary   │    │   │
│  │ ├────┼────────────┼────────────┼─────────────┼──────────┤    │   │
│  │ │ 1  │ Alice      │ Johnson    │ Engineering │ 75000    │    │   │
│  │ │ 2* │ Bob        │ Smith      │ Marketing   │ 68000    │◄───┼───┼── Modified
│  │ │ +  │ New        │ Employee   │ Sales       │ 55000    │◄───┼───┼── Inserted
│  │ └────┴────────────┴────────────┴─────────────┴──────────┘    │   │
│  └──────────────────────────────────────────────────────────────┘   │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │ TNDXSQLiteDataSet (CachedUpdates = True)                    │    │
│  │ ┌─────────────────────────────────────────────────────────┐ │    │
│  │ │ In-Memory Change Buffer                                 │ │    │
│  │ │  • Inserts: [Record 9]                                  │ │    │
│  │ │  • Updates: [Record 2: salary 65000 → 68000]            │ │    │
│  │ │  • Deletes: []                                          │ │    │
│  │ └─────────────────────────────────────────────────────────┘ │    │
│  └──────────────────────────────────────────────────────────────┘   │
│                                    │                                │
│                    [Apply]─────────┼─────────[Cancel]               │
│                       │            │            │                   │
│                       ▼            │            ▼                   │
│              ┌─────────────┐       │     ┌─────────────┐            │
│              │ Commit to   │       │     │ Discard     │            │
│              │ Database    │       │     │ Changes     │            │
│              └─────────────┘       │     └─────────────┘            │
└────────────────────────────────────┼────────────────────────────────┘
                                     ▼
                            ┌───────────────┐
                            │ SQLite Database│
                            │ (cached_updates│
                            │  _demo.db)     │
                            └───────────────┘
```

## Database Schema

```sql
CREATE TABLE employees (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT,
    department TEXT,
    salary REAL DEFAULT 0,
    hire_date TEXT DEFAULT (date('now', 'localtime'))
);
```

## Key Implementation Details

### Enabling Cached Updates Mode

```pascal
// Enable cached updates - changes held in memory until ApplyUpdates
FDataSet.CachedUpdates := True;

// Required for auto-generated SQL statements
FDataSet.TableName := 'employees';
FDataSet.PrimaryKey := 'id';
```

### Applying Pending Changes

```pascal
procedure TfrmMain.btnApplyUpdatesClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    try
      FDataSet.ApplyUpdates;  // Commits all changes to database
      // Reset pending counters
      FPendingInserts := 0;
      FPendingUpdates := 0;
      FPendingDeletes := 0;
      MessageDlg('Success', 'All changes have been saved.', mtInformation, [mbOK], 0);
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      MessageDlg('Error', 'Failed to apply updates: ' + E.Message, mtError, [mbOK], 0);
  end;
end;
```

### Canceling Pending Changes

```pascal
procedure TfrmMain.btnCancelUpdatesClick(Sender: TObject);
begin
  if MessageDlg('Cancel Changes',
    'Are you sure you want to discard all pending changes?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FDataSet.CancelUpdates;  // Discards all pending changes
  end;
end;
```

### Handling Close with Unsaved Changes

```pascal
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FDataSet.CachedUpdates and FDataSet.Modified then
  begin
    case MessageDlg('Unsaved Changes',
      'You have pending changes. Do you want to apply them before closing?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: begin
        FDataSet.ApplyUpdates;
        CanClose := True;
      end;
      mrNo: CanClose := True;      // Discard changes
      mrCancel: CanClose := False;  // Stay in application
    end;
  end;
end;
```

## Cached vs Direct Mode Comparison

| Aspect | Cached Mode | Direct Mode |
|--------|-------------|-------------|
| Writes | Batched on ApplyUpdates | Immediate on Post |
| Performance | Better for multiple edits | Better for single edits |
| Undo | CancelUpdates discards all | No built-in undo |
| Transactions | Implicit batch behavior | Requires explicit transactions |
| Memory | Holds changes in RAM | Minimal memory overhead |

## Common Use Cases

| Scenario | Why Use Cached Updates |
|----------|------------------------|
| Data Entry Forms | Allow review before saving |
| Spreadsheet-like Editing | Batch multiple cell changes |
| Import Preview | Review imported data before commit |
| Validation Workflows | Validate entire batch before saving |
| Offline Editing | Queue changes for later sync |

## Building and Running

### Build
```bash
cd examples/gui/03_CachedUpdates
lazbuild CachedUpdatesDemo.lpi
```

### Run
- **Linux/macOS**: `./CachedUpdatesDemo`
- **Windows**: `CachedUpdatesDemo.exe`

The database file `cached_updates_demo.db` is created automatically with sample employee data.

## Project Files

| File | Description |
|------|-------------|
| `CachedUpdatesDemo.lpi` | Lazarus project configuration |
| `CachedUpdatesDemo.lpr` | Program entry point |
| `MainForm.pas` | Main form with cached updates logic |
| `MainForm.lfm` | Visual form design |

## Testing the Feature

1. **Edit Records**: Modify any field in the grid
   - Notice the pending counter updates
   - Changes are NOT saved to database yet

2. **Insert Records**: Click `+` in the navigator
   - New row appears in grid
   - Pending insert count increases

3. **Apply Changes**: Click `Apply` button
   - All pending changes written to database
   - Pending counters reset to zero

4. **Cancel Changes**: Make edits, then click `Cancel`
   - All pending changes discarded
   - Grid reverts to database state

5. **Toggle Mode**: Uncheck "Cached Updates"
   - Warning if pending changes exist
   - Subsequent edits save immediately

## Requirements

- **IDE**: Lazarus 2.2+ with Free Pascal 3.2+
- **Library**: NDXSQLite (parent project)
- **Platform**: Windows, Linux, or macOS

## Cross-Platform Notes

This example works identically on all platforms. The `Modified` property correctly tracks pending changes regardless of the operating system or widget set.

## Related Examples

- **01_DBGrid**: Basic single-table CRUD operations
- **02_MasterDetail**: Two linked datasets with automatic filtering
- **04_AsyncProgress**: Non-blocking database operations

## License

MIT License - (c) 2026 Nicolas DEOUX - NDX Software
