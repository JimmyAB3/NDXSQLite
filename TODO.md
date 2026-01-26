# NDXSQLite TODO

Planned features and improvements for future releases.

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
