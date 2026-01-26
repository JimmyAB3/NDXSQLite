# NDXSQLite TODO

Planned features and improvements for future releases.

## Interface Consistency Issue (Won't Fix)

### Problem Description

4 classes in `src/advanced/` accept `TNDXSQLiteConnection` (concrete class) instead of `INDXSQLiteConnection` (interface):

| Class | File |
|-------|------|
| `TNDXSQLiteJSON` | `src/advanced/ndxsqlitejson.pas` |
| `TNDXSQLiteCSV` | `src/advanced/ndxsqlitecsv.pas` |
| `TNDXSQLiteDump` | `src/advanced/ndxsqlitedump.pas` |
| `TNDXSQLiteVerify` | `src/advanced/ndxsqliteverify.pas` |

19 other classes in the library use `INDXSQLiteConnection` (interface).

### Decision: Won't Fix

**Reason:** Reference counting issues with `TInterfacedObject`.

When a class stores an `INDXSQLiteConnection` interface reference, Free Pascal's automatic reference counting can cause premature destruction of the connection object if the caller uses a class variable (`TNDXSQLiteConnection`) instead of an interface variable (`INDXSQLiteConnection`).

This would require all existing code to change from:
```pascal
var
  Conn: TNDXSQLiteConnection;  // Common pattern
begin
  Conn := TNDXSQLiteConnection.Create('db.sqlite');
  Conn.Free;  // Explicit cleanup
end;
```

To:
```pascal
var
  Conn: INDXSQLiteConnection;  // Interface variable
begin
  Conn := TNDXSQLiteConnection.Create('db.sqlite');
  Conn := nil;  // Reference counting handles cleanup
end;
```

**This is a breaking change** that affects backward compatibility. The 4 utility classes intentionally use the concrete class to avoid this issue.

### Workaround for Users

If you need to use these utilities with interface-based code, simply cast or assign:
```pascal
var
  Conn: INDXSQLiteConnection;
  ConnClass: TNDXSQLiteConnection;
begin
  Conn := TNDXSQLiteConnection.Create('db.sqlite');
  ConnClass := Conn as TNDXSQLiteConnection;
  JSON := TNDXSQLiteJSON.Create(ConnClass);
  // ...
end;
```

### Priority

**Won't Fix** - Maintaining backward compatibility is more important than design consistency.

---

## CSV Module (`src/advanced/ndxsqlitecsv.pas`)

### Base64 BLOB Encoding

- **Location:** Line 369
- **Status:** Not implemented
- **Description:** The `bfBase64` option for BLOB export returns an empty string instead of Base64-encoded data.
- **Current behavior:** Only `bfHex` and `bfSkip` work correctly.
- **Priority:** Low

### Column Mapping for CSV Import

- **Location:** Line 421
- **Status:** Not implemented
- **Description:** The `ImportFromCSV` overload accepting `AColumnMapping` parameter ignores it entirely.
- **Current behavior:** Falls back to default import options.
- **Priority:** Low

---

*Last updated: 2026-01-26*

---

## Summary

| Issue | Priority | Status |
|-------|----------|--------|
| Interface Consistency (4 classes) | - | Won't Fix |
| CSV Base64 BLOB Encoding | Low | Pending |
| CSV Column Mapping Import | Low | Pending |
