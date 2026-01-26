# Example 56: ETL Pipeline

This example demonstrates Extract-Transform-Load (ETL) patterns for data integration with NDXSQLite.

## Features Demonstrated

- **Staging Tables**: Raw data import without constraints
- **Data Transformation**: Cleaning, normalizing, formatting
- **Error Handling**: Track and log bad records
- **Incremental Loading**: Process only new records
- **Data Quality Validation**: Detect and log issues
- **ETL Run Tracking**: Audit trail for pipeline runs

## Database Schema

### Staging Tables
```sql
CREATE TABLE staging_customers (
  row_id INTEGER PRIMARY KEY,
  raw_name TEXT,
  raw_email TEXT,
  raw_phone TEXT,
  raw_country TEXT,
  raw_created_date TEXT,
  source_file TEXT,
  import_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
  processed INTEGER DEFAULT 0,   -- 0=pending, 1=success, -1=error
  error_message TEXT
);
```

### Target Tables
```sql
CREATE TABLE customers (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE,
  phone TEXT,
  country_code TEXT,
  created_date TEXT,
  source_row_id INTEGER,
  loaded_at TEXT DEFAULT CURRENT_TIMESTAMP
);
```

### ETL Tracking
```sql
CREATE TABLE etl_runs (
  id INTEGER PRIMARY KEY,
  run_type TEXT,
  started_at TEXT,
  completed_at TEXT,
  records_processed INTEGER DEFAULT 0,
  records_loaded INTEGER DEFAULT 0,
  records_failed INTEGER DEFAULT 0,
  status TEXT DEFAULT 'running'
);

CREATE TABLE data_quality_issues (
  id INTEGER PRIMARY KEY,
  table_name TEXT,
  row_id INTEGER,
  field_name TEXT,
  issue_type TEXT,           -- 'empty_value', 'invalid_format', 'unknown_value'
  original_value TEXT,
  corrected_value TEXT,
  detected_at TEXT DEFAULT CURRENT_TIMESTAMP
);
```

## ETL Process Flow

### 1. Extract (Import Raw Data)
```pascal
// Load raw data without validation
Connection.ExecuteNonQuery(
  'INSERT INTO staging_customers (raw_name, raw_email, raw_country, source_file) VALUES ' +
  '(''  John Smith  '', ''JOHN.SMITH@EXAMPLE.COM'', ''United States'', ''import.csv'')');
```

### 2. Transform (Clean and Validate)
```pascal
// Trim whitespace, normalize case
CleanName := Trim(RawName);
CleanEmail := LowerCase(Trim(RawEmail));

// Validate format
if (Pos('@', CleanEmail) = 0) then
begin
  // Log data quality issue
  Connection.ExecuteNonQuery(
    'INSERT INTO data_quality_issues (table_name, row_id, field_name, issue_type, original_value) ' +
    'VALUES (''staging_customers'', ?, ''raw_email'', ''invalid_format'', ?)',
    [RowID, RawEmail]);
  IsValid := False;
end;

// Lookup country code
CountryCode := Connection.ExecuteScalar(
  'SELECT code FROM country_codes WHERE name = ?', [LowerCase(RawCountry)]);
```

### 3. Load (Insert to Target)
```pascal
if IsValid then
begin
  Connection.ExecuteNonQuery(
    'INSERT INTO customers (name, email, country_code, source_row_id) VALUES (?, ?, ?, ?)',
    [CleanName, CleanEmail, CountryCode, RowID]);
  Connection.ExecuteNonQuery('UPDATE staging_customers SET processed = 1 WHERE row_id = ?', [RowID]);
end
else
begin
  Connection.ExecuteNonQuery('UPDATE staging_customers SET processed = -1, error_message = ? WHERE row_id = ?',
    [ErrorMsg, RowID]);
end;
```

## Transformation Examples

### Name Normalization
- `"  John Smith  "` -> `"John Smith"` (trim)
- `"JANE DOE"` -> `"Jane Doe"` (proper case)

### Email Validation
- `"JOHN@EXAMPLE.COM"` -> `"john@example.com"` (lowercase)
- `"invalid-email"` -> rejected (no @ symbol)

### Country Code Lookup
- `"United States"` -> `"US"`
- `"USA"` -> `"US"`
- `"uk"` -> `"GB"`

### Price Cleaning
- `"$999.99"` -> `999.99` (remove currency symbol)
- `"9,99"` -> `9.99` (European decimal)

## Incremental Loading

```pascal
// Process only unprocessed records
DS := Connection.ExecuteQuery(
  'SELECT * FROM staging_customers WHERE processed = 0 ORDER BY row_id');

// Track last processed for next run
LastProcessed := Connection.ExecuteScalar(
  'SELECT MAX(row_id) FROM staging_customers WHERE processed = 1');
```

## Data Quality Report

```sql
SELECT table_name, field_name, issue_type, COUNT(*) as count
FROM data_quality_issues
GROUP BY table_name, field_name, issue_type
ORDER BY count DESC;
```

## Build and Run

```bash
cd 56_ETLPipeline
fpc ETLPipeline.lpr
./ETLPipeline
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use staging tables to isolate raw data from validated data
- Log all data quality issues for analysis and correction
- Track ETL runs with timing and record counts
- Use transactions for atomic batch loads
- Implement retry logic for transient errors
- Archive processed staging data after successful loads
- Create lookup tables for data standardization
- Document transformation rules for maintainability
