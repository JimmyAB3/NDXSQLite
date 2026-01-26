{===============================================================================
  NDXSQLite Example 56 - ETL Pipeline
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Staging tables for raw data import
  - Data transformation and cleaning
  - Error handling for bad records
  - Incremental loading patterns
  - Data quality validation
  - Batch processing with transactions

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ETLPipeline;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates staging, target, ETL run log, data quality, and country lookup tables with sample country code mappings. }
procedure SetupETLTables;
begin
  // Staging table for raw customer data (accepts anything)
  Connection.ExecuteNonQuery(
    'CREATE TABLE staging_customers (' +
    '  row_id INTEGER PRIMARY KEY,' +
    '  raw_name TEXT,' +
    '  raw_email TEXT,' +
    '  raw_phone TEXT,' +
    '  raw_country TEXT,' +
    '  raw_created_date TEXT,' +
    '  source_file TEXT,' +
    '  import_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,' +
    '  processed INTEGER DEFAULT 0,' +
    '  error_message TEXT' +
    ')');

  // Staging table for raw product data
  Connection.ExecuteNonQuery(
    'CREATE TABLE staging_products (' +
    '  row_id INTEGER PRIMARY KEY,' +
    '  raw_sku TEXT,' +
    '  raw_name TEXT,' +
    '  raw_price TEXT,' +
    '  raw_category TEXT,' +
    '  raw_quantity TEXT,' +
    '  source_file TEXT,' +
    '  import_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,' +
    '  processed INTEGER DEFAULT 0,' +
    '  error_message TEXT' +
    ')');

  // Target normalized customers table
  Connection.ExecuteNonQuery(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT NOT NULL UNIQUE,' +
    '  phone TEXT,' +
    '  country_code TEXT,' +
    '  created_date TEXT,' +
    '  source_row_id INTEGER,' +
    '  loaded_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  // Target normalized products table
  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  sku TEXT NOT NULL UNIQUE,' +
    '  name TEXT NOT NULL,' +
    '  price REAL NOT NULL,' +
    '  category TEXT,' +
    '  quantity INTEGER DEFAULT 0,' +
    '  source_row_id INTEGER,' +
    '  loaded_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  // ETL run log
  Connection.ExecuteNonQuery(
    'CREATE TABLE etl_runs (' +
    '  id INTEGER PRIMARY KEY,' +
    '  run_type TEXT,' +
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  records_processed INTEGER DEFAULT 0,' +
    '  records_loaded INTEGER DEFAULT 0,' +
    '  records_failed INTEGER DEFAULT 0,' +
    '  status TEXT DEFAULT ''running''' +
    ')');

  // Data quality issues log
  Connection.ExecuteNonQuery(
    'CREATE TABLE data_quality_issues (' +
    '  id INTEGER PRIMARY KEY,' +
    '  table_name TEXT,' +
    '  row_id INTEGER,' +
    '  field_name TEXT,' +
    '  issue_type TEXT,' +
    '  original_value TEXT,' +
    '  corrected_value TEXT,' +
    '  detected_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  // Country code lookup
  Connection.ExecuteNonQuery(
    'CREATE TABLE country_codes (' +
    '  name TEXT PRIMARY KEY,' +
    '  code TEXT NOT NULL' +
    ')');

  // Insert country lookups
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''united states'', ''US'')');
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''usa'', ''US'')');
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''us'', ''US'')');
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''canada'', ''CA'')');
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''uk'', ''GB'')');
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''united kingdom'', ''GB'')');
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''france'', ''FR'')');
  Connection.ExecuteNonQuery('INSERT INTO country_codes VALUES (''germany'', ''DE'')');
end;

{ Inserts intentionally messy customer and product records into staging tables to simulate a CSV file import with various data quality issues. }
procedure SimulateRawDataImport;
begin
  WriteLn('1. Extract: Importing Raw Data');
  WriteLn('   -----------------------------');

  // Simulate importing messy customer data from CSV
  WriteLn('   Loading raw customer data from "customers_export.csv"...');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_customers (raw_name, raw_email, raw_phone, raw_country, raw_created_date, source_file) VALUES ' +
    '(''  John Smith  '', ''JOHN.SMITH@EXAMPLE.COM'', ''(555) 123-4567'', ''United States'', ''01/15/2024'', ''customers_export.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_customers (raw_name, raw_email, raw_phone, raw_country, raw_created_date, source_file) VALUES ' +
    '(''Jane DOE'', ''jane.doe@example.com'', ''555.987.6543'', ''USA'', ''2024-01-20'', ''customers_export.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_customers (raw_name, raw_email, raw_phone, raw_country, raw_created_date, source_file) VALUES ' +
    '(''Bob Wilson'', ''invalid-email'', '''', ''Canada'', ''Jan 25, 2024'', ''customers_export.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_customers (raw_name, raw_email, raw_phone, raw_country, raw_created_date, source_file) VALUES ' +
    '(''ALICE MARTIN'', ''alice@example.com'', ''44-20-7946-0958'', ''UK'', ''2024/02/01'', ''customers_export.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_customers (raw_name, raw_email, raw_phone, raw_country, raw_created_date, source_file) VALUES ' +
    '('''', ''missing@name.com'', ''123456'', ''France'', ''2024-02-10'', ''customers_export.csv'')');

  WriteLn('     5 customer records imported');

  // Simulate importing messy product data
  WriteLn('   Loading raw product data from "products_inventory.csv"...');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_products (raw_sku, raw_name, raw_price, raw_category, raw_quantity, source_file) VALUES ' +
    '(''SKU-001'', ''laptop computer'', ''$999.99'', ''Electronics'', ''50'', ''products_inventory.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_products (raw_sku, raw_name, raw_price, raw_category, raw_quantity, source_file) VALUES ' +
    '(''SKU-002'', ''Wireless Mouse'', ''29.99 USD'', ''electronics'', ''200'', ''products_inventory.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_products (raw_sku, raw_name, raw_price, raw_category, raw_quantity, source_file) VALUES ' +
    '(''sku003'', ''USB Cable'', ''9,99'', ''ELECTRONICS'', ''-5'', ''products_inventory.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_products (raw_sku, raw_name, raw_price, raw_category, raw_quantity, source_file) VALUES ' +
    '(''SKU-004'', ''Office Chair'', ''not available'', ''Furniture'', ''N/A'', ''products_inventory.csv'')');

  Connection.ExecuteNonQuery(
    'INSERT INTO staging_products (raw_sku, raw_name, raw_price, raw_category, raw_quantity, source_file) VALUES ' +
    '(''SKU-001'', ''Duplicate SKU Product'', ''100'', ''Test'', ''10'', ''products_inventory.csv'')');

  WriteLn('     5 product records imported');
  WriteLn;
end;

{ Extracts, validates, and loads customer data from raw to clean format. }
procedure TransformCustomerData;
var
  DS: TDataSet;
  RunID: Integer;
  Processed, Loaded, Failed: Integer;
  RawName, RawEmail, RawCountry, RawDate: string;
  CleanName, CleanEmail, CountryCode, CleanDate: string;
  RowID: Integer;
  IsValid: Boolean;
  ErrorMsg: string;
begin
  WriteLn('2. Transform: Cleaning Customer Data');
  WriteLn('   -----------------------------------');

  // Start ETL run
  Connection.ExecuteNonQuery('INSERT INTO etl_runs (run_type, started_at) VALUES (''customer_load'', datetime(''now''))');
  RunID := Connection.GetLastInsertRowID;

  Processed := 0;
  Loaded := 0;
  Failed := 0;

  DS := Connection.ExecuteQuery('SELECT * FROM staging_customers WHERE processed = 0');
  try
    while not DS.EOF do
    begin
      Inc(Processed);
      RowID := DS.FieldByName('row_id').AsInteger;
      RawName := DS.FieldByName('raw_name').AsString;
      RawEmail := DS.FieldByName('raw_email').AsString;
      RawCountry := DS.FieldByName('raw_country').AsString;
      RawDate := DS.FieldByName('raw_created_date').AsString;

      IsValid := True;
      ErrorMsg := '';

      // Transform: Trim and proper case name
      CleanName := Trim(RawName);
      if CleanName = '' then
      begin
        IsValid := False;
        ErrorMsg := 'Empty name';
        Connection.ExecuteNonQuery(
          'INSERT INTO data_quality_issues (table_name, row_id, field_name, issue_type, original_value) ' +
          'VALUES (''staging_customers'', ?, ''raw_name'', ''empty_value'', ?)',
          [RowID, RawName]);
      end
      else
      begin
        // Proper case transformation
        CleanName := LowerCase(CleanName);
        CleanName[1] := UpCase(CleanName[1]);
        // Find and uppercase after space
        if Pos(' ', CleanName) > 0 then
          CleanName[Pos(' ', CleanName) + 1] := UpCase(CleanName[Pos(' ', CleanName) + 1]);
      end;

      // Transform: Lowercase and validate email
      CleanEmail := LowerCase(Trim(RawEmail));
      if (Pos('@', CleanEmail) = 0) or (Pos('.', CleanEmail) = 0) then
      begin
        IsValid := False;
        ErrorMsg := ErrorMsg + '; Invalid email format';
        Connection.ExecuteNonQuery(
          'INSERT INTO data_quality_issues (table_name, row_id, field_name, issue_type, original_value) ' +
          'VALUES (''staging_customers'', ?, ''raw_email'', ''invalid_format'', ?)',
          [RowID, RawEmail]);
      end;

      // Transform: Country to code lookup
      CountryCode := '';
      if RawCountry <> '' then
      begin
        Connection.ExecuteNonQuery(
          'SELECT code FROM country_codes WHERE name = ?', [LowerCase(Trim(RawCountry))]);
        // Simple lookup
        CountryCode := Connection.ExecuteScalar(
          'SELECT code FROM country_codes WHERE name = ?', [LowerCase(Trim(RawCountry))]);
        if CountryCode = '' then
        begin
          Connection.ExecuteNonQuery(
            'INSERT INTO data_quality_issues (table_name, row_id, field_name, issue_type, original_value) ' +
            'VALUES (''staging_customers'', ?, ''raw_country'', ''unknown_value'', ?)',
            [RowID, RawCountry]);
        end;
      end;

      // Transform: Normalize date format
      CleanDate := '';
      if RawDate <> '' then
      begin
        // Try to parse various date formats (simplified)
        if Pos('/', RawDate) > 0 then
          CleanDate := StringReplace(RawDate, '/', '-', [rfReplaceAll]);
        if Pos(',', RawDate) > 0 then
          CleanDate := '2024-01-25'; // Simplified fallback
        if CleanDate = '' then
          CleanDate := RawDate;
      end;

      WriteLn(Format('   Row %d: %s -> %s [%s]',
        [RowID, RawName, CleanName, BoolToStr(IsValid, 'VALID', 'INVALID')]));

      if IsValid then
      begin
        // Load into target table
        try
          Connection.ExecuteNonQuery(
            'INSERT INTO customers (name, email, phone, country_code, created_date, source_row_id) ' +
            'VALUES (?, ?, ?, ?, ?, ?)',
            [CleanName, CleanEmail, DS.FieldByName('raw_phone').AsString, CountryCode, CleanDate, RowID]);
          Connection.ExecuteNonQuery('UPDATE staging_customers SET processed = 1 WHERE row_id = ?', [RowID]);
          Inc(Loaded);
        except
          on E: Exception do
          begin
            Inc(Failed);
            Connection.ExecuteNonQuery('UPDATE staging_customers SET processed = -1, error_message = ? WHERE row_id = ?',
              [E.Message, RowID]);
          end;
        end;
      end
      else
      begin
        Inc(Failed);
        Connection.ExecuteNonQuery('UPDATE staging_customers SET processed = -1, error_message = ? WHERE row_id = ?',
          [ErrorMsg, RowID]);
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update ETL run
  Connection.ExecuteNonQuery(
    'UPDATE etl_runs SET completed_at = datetime(''now''), records_processed = ?, records_loaded = ?, ' +
    'records_failed = ?, status = ''completed'' WHERE id = ?',
    [Processed, Loaded, Failed, RunID]);

  WriteLn(Format('   Summary: %d processed, %d loaded, %d failed', [Processed, Loaded, Failed]));
  WriteLn;
end;

{ Extracts, validates, and loads product data from raw to clean format. }
procedure TransformProductData;
var
  DS: TDataSet;
  RunID: Integer;
  Processed, Loaded, Failed: Integer;
  RawSku, RawName, RawPrice, RawCategory, RawQty: string;
  CleanSku, CleanName, CleanCategory: string;
  Price: Double;
  Qty: Integer;
  RowID: Integer;
  IsValid: Boolean;
  ErrorMsg: string;
begin
  WriteLn('3. Transform: Cleaning Product Data');
  WriteLn('   ----------------------------------');

  // Start ETL run
  Connection.ExecuteNonQuery('INSERT INTO etl_runs (run_type, started_at) VALUES (''product_load'', datetime(''now''))');
  RunID := Connection.GetLastInsertRowID;

  Processed := 0;
  Loaded := 0;
  Failed := 0;

  DS := Connection.ExecuteQuery('SELECT * FROM staging_products WHERE processed = 0');
  try
    while not DS.EOF do
    begin
      Inc(Processed);
      RowID := DS.FieldByName('row_id').AsInteger;
      RawSku := DS.FieldByName('raw_sku').AsString;
      RawName := DS.FieldByName('raw_name').AsString;
      RawPrice := DS.FieldByName('raw_price').AsString;
      RawCategory := DS.FieldByName('raw_category').AsString;
      RawQty := DS.FieldByName('raw_quantity').AsString;

      IsValid := True;
      ErrorMsg := '';

      // Transform: Normalize SKU (uppercase, remove special chars)
      CleanSku := UpperCase(StringReplace(Trim(RawSku), '-', '', [rfReplaceAll]));

      // Transform: Proper case product name
      CleanName := Trim(RawName);
      if Length(CleanName) > 0 then
      begin
        CleanName := LowerCase(CleanName);
        CleanName[1] := UpCase(CleanName[1]);
      end;

      // Transform: Parse price (remove currency symbols)
      RawPrice := StringReplace(RawPrice, '$', '', [rfReplaceAll]);
      RawPrice := StringReplace(RawPrice, 'USD', '', [rfReplaceAll]);
      RawPrice := StringReplace(RawPrice, ',', '.', [rfReplaceAll]);
      RawPrice := Trim(RawPrice);
      Price := 0;
      if not TryStrToFloat(RawPrice, Price) then
      begin
        IsValid := False;
        ErrorMsg := 'Invalid price format';
        Connection.ExecuteNonQuery(
          'INSERT INTO data_quality_issues (table_name, row_id, field_name, issue_type, original_value) ' +
          'VALUES (''staging_products'', ?, ''raw_price'', ''invalid_format'', ?)',
          [RowID, DS.FieldByName('raw_price').AsString]);
      end;

      // Transform: Parse quantity
      Qty := 0;
      if not TryStrToInt(Trim(RawQty), Qty) then
      begin
        Qty := 0;
        Connection.ExecuteNonQuery(
          'INSERT INTO data_quality_issues (table_name, row_id, field_name, issue_type, original_value, corrected_value) ' +
          'VALUES (''staging_products'', ?, ''raw_quantity'', ''invalid_format'', ?, ''0'')',
          [RowID, RawQty]);
      end
      else if Qty < 0 then
      begin
        Connection.ExecuteNonQuery(
          'INSERT INTO data_quality_issues (table_name, row_id, field_name, issue_type, original_value, corrected_value) ' +
          'VALUES (''staging_products'', ?, ''raw_quantity'', ''negative_value'', ?, ''0'')',
          [RowID, RawQty]);
        Qty := 0;
      end;

      // Transform: Normalize category
      CleanCategory := Trim(RawCategory);
      if Length(CleanCategory) > 0 then
      begin
        CleanCategory := LowerCase(CleanCategory);
        CleanCategory[1] := UpCase(CleanCategory[1]);
      end;

      WriteLn(Format('   Row %d: %s ($%s) -> %s ($%.2f) [%s]',
        [RowID, RawSku, DS.FieldByName('raw_price').AsString, CleanSku, Price,
         BoolToStr(IsValid, 'VALID', 'INVALID')]));

      if IsValid then
      begin
        // Load into target table
        try
          Connection.ExecuteNonQuery(
            'INSERT INTO products (sku, name, price, category, quantity, source_row_id) ' +
            'VALUES (?, ?, ?, ?, ?, ?)',
            [CleanSku, CleanName, Price, CleanCategory, Qty, RowID]);
          Connection.ExecuteNonQuery('UPDATE staging_products SET processed = 1 WHERE row_id = ?', [RowID]);
          Inc(Loaded);
        except
          on E: Exception do
          begin
            Inc(Failed);
            Connection.ExecuteNonQuery('UPDATE staging_products SET processed = -1, error_message = ? WHERE row_id = ?',
              [E.Message, RowID]);
          end;
        end;
      end
      else
      begin
        Inc(Failed);
        Connection.ExecuteNonQuery('UPDATE staging_products SET processed = -1, error_message = ? WHERE row_id = ?',
          [ErrorMsg, RowID]);
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update ETL run
  Connection.ExecuteNonQuery(
    'UPDATE etl_runs SET completed_at = datetime(''now''), records_processed = ?, records_loaded = ?, ' +
    'records_failed = ?, status = ''completed'' WHERE id = ?',
    [Processed, Loaded, Failed, RunID]);

  WriteLn(Format('   Summary: %d processed, %d loaded, %d failed', [Processed, Loaded, Failed]));
  WriteLn;
end;

{ Queries and prints data quality issues grouped by table, field, and issue type with occurrence counts. }
procedure ShowDataQualityReport;
var
  DS: TDataSet;
begin
  WriteLn('4. Data Quality Report');
  WriteLn('   ---------------------');

  DS := Connection.ExecuteQuery(
    'SELECT table_name, field_name, issue_type, COUNT(*) as cnt ' +
    'FROM data_quality_issues ' +
    'GROUP BY table_name, field_name, issue_type ' +
    'ORDER BY cnt DESC');
  try
    if DS.EOF then
      WriteLn('   No data quality issues found')
    else
    begin
      WriteLn('   Issues by type:');
      while not DS.EOF do
      begin
        WriteLn(Format('     %s.%s: %s (%d occurrences)',
          [DS.FieldByName('table_name').AsString,
           DS.FieldByName('field_name').AsString,
           DS.FieldByName('issue_type').AsString,
           DS.FieldByName('cnt').AsInteger]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries and prints the ETL run log showing each run's type, record counts (processed, loaded, failed), and completion status. }
procedure ShowETLRunSummary;
var
  DS: TDataSet;
begin
  WriteLn('5. ETL Run Summary');
  WriteLn('   -----------------');

  DS := Connection.ExecuteQuery(
    'SELECT run_type, started_at, completed_at, records_processed, records_loaded, records_failed, status ' +
    'FROM etl_runs ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s: %d processed, %d loaded, %d failed (%s)',
        [DS.FieldByName('run_type').AsString,
         DS.FieldByName('records_processed').AsInteger,
         DS.FieldByName('records_loaded').AsInteger,
         DS.FieldByName('records_failed').AsInteger,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Queries and prints the final cleaned customer and product records from the target tables after ETL processing. }
procedure ShowLoadedData;
var
  DS: TDataSet;
begin
  WriteLn('6. Final Loaded Data');
  WriteLn('   -------------------');

  WriteLn('   Customers:');
  DS := Connection.ExecuteQuery('SELECT * FROM customers');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%d] %s <%s> (%s)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('name').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('country_code').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Products:');
  DS := Connection.ExecuteQuery('SELECT * FROM products');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %s - $%.2f (qty: %d)',
        [DS.FieldByName('sku').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('price').AsFloat,
         DS.FieldByName('quantity').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Adds a new staging customer record and runs the transform again, processing only unprocessed rows to show incremental loading. }
procedure DemoIncrementalLoad;
begin
  WriteLn('7. Incremental Load Pattern');
  WriteLn('   --------------------------');
  WriteLn('   Incremental loading strategy:');
  WriteLn('   - Track last processed row_id or timestamp');
  WriteLn('   - Process only records WHERE row_id > last_processed');
  WriteLn('   - Use staging.processed flag to track state');
  WriteLn('   - Handle updates with UPSERT (INSERT OR REPLACE)');
  WriteLn;

  // Simulate adding new records
  WriteLn('   Adding new staging record...');
  Connection.ExecuteNonQuery(
    'INSERT INTO staging_customers (raw_name, raw_email, raw_phone, raw_country, raw_created_date, source_file) VALUES ' +
    '(''New Customer'', ''new@example.com'', ''555-0000'', ''Germany'', ''2024-03-01'', ''incremental_update.csv'')');

  WriteLn('   Running incremental transform...');
  TransformCustomerData;
end;

{ Prints a summary of ETL best practices including staging isolation, quality logging, run tracking, transactions, and retry logic. }
procedure DemoBestPractices;
begin
  WriteLn('8. ETL Best Practices');
  WriteLn('   --------------------');
  WriteLn('   - Use staging tables to isolate raw data');
  WriteLn('   - Log all data quality issues for analysis');
  WriteLn('   - Track ETL runs with timing and counts');
  WriteLn('   - Use transactions for atomic batch loads');
  WriteLn('   - Implement retry logic for transient errors');
  WriteLn('   - Archive processed staging data');
  WriteLn('   - Create data quality dashboards');
  WriteLn('   - Document transformation rules');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 56: ETL Pipeline ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example56.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupETLTables;
      SimulateRawDataImport;
      TransformCustomerData;
      TransformProductData;
      ShowDataQualityReport;
      ShowETLRunSummary;
      ShowLoadedData;
      DemoIncrementalLoad;
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
