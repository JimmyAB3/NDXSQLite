{===============================================================================
  NDXSQLite Example 124 - Data Reconciliation
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Source-target data comparison
  - Discrepancy detection and classification
  - Match/mismatch result reporting
  - Severity-based analysis
  - Discrepancy resolution tracking

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataReconciliation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Source data (expected - e.g., from ERP)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS source_orders (' +
    '  order_id TEXT PRIMARY KEY,' +
    '  customer_name TEXT NOT NULL,' +
    '  product TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  unit_price REAL NOT NULL,' +
    '  total_amount REAL NOT NULL,' +
    '  order_date TEXT NOT NULL,' +
    '  status TEXT NOT NULL' +
    ')'
  );

  // Target data (actual - e.g., from warehouse)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS target_orders (' +
    '  order_id TEXT PRIMARY KEY,' +
    '  customer_name TEXT NOT NULL,' +
    '  product TEXT NOT NULL,' +
    '  quantity INTEGER NOT NULL,' +
    '  unit_price REAL NOT NULL,' +
    '  total_amount REAL NOT NULL,' +
    '  order_date TEXT NOT NULL,' +
    '  status TEXT NOT NULL' +
    ')'
  );

  // Reconciliation runs
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS reconciliation_runs (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  run_id TEXT NOT NULL,' +
    '  source_table TEXT NOT NULL,' +
    '  target_table TEXT NOT NULL,' +
    '  match_key TEXT NOT NULL,' +
    '  compare_columns TEXT NOT NULL,' +
    '  total_source INTEGER DEFAULT 0,' +
    '  total_target INTEGER DEFAULT 0,' +
    '  matched INTEGER DEFAULT 0,' +
    '  mismatched INTEGER DEFAULT 0,' +
    '  source_only INTEGER DEFAULT 0,' +
    '  target_only INTEGER DEFAULT 0,' +
    '  status TEXT DEFAULT ''pending'',' +  // pending, running, completed
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  UNIQUE(run_id)' +
    ')'
  );

  // Discrepancies
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS discrepancies (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  run_id TEXT NOT NULL,' +
    '  record_key TEXT NOT NULL,' +
    '  discrepancy_type TEXT NOT NULL,' +  // mismatch, source_only, target_only
    '  column_name TEXT,' +
    '  source_value TEXT,' +
    '  target_value TEXT,' +
    '  severity TEXT DEFAULT ''medium'',' +  // low, medium, high, critical
    '  resolution_status TEXT DEFAULT ''open'',' +  // open, resolved, ignored
    '  resolved_by TEXT,' +
    '  resolved_at TEXT,' +
    '  notes TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')'
  );

  // Match results
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS match_results (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  run_id TEXT NOT NULL,' +
    '  record_key TEXT NOT NULL,' +
    '  match_status TEXT NOT NULL,' +  // exact_match, partial_match, no_match
    '  match_score REAL DEFAULT 0,' +  // 0-100 similarity
    '  matched_columns INTEGER DEFAULT 0,' +
    '  total_columns INTEGER DEFAULT 0,' +
    '  mismatched_columns TEXT' +  // comma-separated list
    ')'
  );
end;

{ Inserts source data for comparison. }
procedure InsertSourceData;
begin
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-001'', ''Alice Corp'', ''Widget A'', 100, 25.50, 2550.00, ''2024-01-15'', ''shipped'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-002'', ''Bob LLC'', ''Widget B'', 50, 42.00, 2100.00, ''2024-01-16'', ''shipped'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-003'', ''Carol Inc'', ''Gadget X'', 200, 15.75, 3150.00, ''2024-01-17'', ''delivered'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-004'', ''Dave Co'', ''Gadget Y'', 75, 38.00, 2850.00, ''2024-01-18'', ''shipped'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-005'', ''Eve Enterprises'', ''Widget A'', 150, 25.50, 3825.00, ''2024-01-19'', ''processing'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-006'', ''Frank Ltd'', ''Widget C'', 30, 55.00, 1650.00, ''2024-01-20'', ''shipped'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-007'', ''Grace Group'', ''Gadget Z'', 80, 29.99, 2399.20, ''2024-01-21'', ''delivered'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-008'', ''Henry SA'', ''Widget B'', 120, 42.00, 5040.00, ''2024-01-22'', ''shipped'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-009'', ''Iris Tech'', ''Gadget X'', 60, 15.75, 945.00, ''2024-01-23'', ''cancelled'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-010'', ''Jack Works'', ''Widget A'', 90, 25.50, 2295.00, ''2024-01-24'', ''shipped'')');
  // ORD-011 only in source (missing from target)
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-011'', ''Kate Mfg'', ''Gadget Y'', 45, 38.00, 1710.00, ''2024-01-25'', ''processing'')');
  Conn.ExecuteNonQuery('INSERT INTO source_orders VALUES (''ORD-012'', ''Leo Dist'', ''Widget C'', 25, 55.00, 1375.00, ''2024-01-26'', ''shipped'')');
end;

{ Inserts target data for comparison. }
procedure InsertTargetData;
begin
  // Exact matches
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-001'', ''Alice Corp'', ''Widget A'', 100, 25.50, 2550.00, ''2024-01-15'', ''shipped'')');
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-003'', ''Carol Inc'', ''Gadget X'', 200, 15.75, 3150.00, ''2024-01-17'', ''delivered'')');
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-007'', ''Grace Group'', ''Gadget Z'', 80, 29.99, 2399.20, ''2024-01-21'', ''delivered'')');
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-009'', ''Iris Tech'', ''Gadget X'', 60, 15.75, 945.00, ''2024-01-23'', ''cancelled'')');
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-012'', ''Leo Dist'', ''Widget C'', 25, 55.00, 1375.00, ''2024-01-26'', ''shipped'')');

  // Mismatches (various discrepancies)
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-002'', ''Bob LLC'', ''Widget B'', 48, 42.00, 2016.00, ''2024-01-16'', ''shipped'')');  // quantity differs (50->48), total differs
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-004'', ''Dave Co'', ''Gadget Y'', 75, 38.00, 2850.00, ''2024-01-18'', ''delivered'')');  // status differs (shipped->delivered)
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-005'', ''Eve Enterprises'', ''Widget A'', 150, 24.00, 3600.00, ''2024-01-19'', ''processing'')');  // price differs, total differs
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-006'', ''Frank Limited'', ''Widget C'', 30, 55.00, 1650.00, ''2024-01-20'', ''shipped'')');  // customer name differs
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-008'', ''Henry SA'', ''Widget B'', 120, 42.00, 5040.00, ''2024-01-23'', ''shipped'')');  // date differs (22->23)
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-010'', ''Jack Works'', ''Widget A'', 90, 25.50, 2295.00, ''2024-01-24'', ''delivered'')');  // status differs

  // ORD-013 only in target (extra record)
  Conn.ExecuteNonQuery('INSERT INTO target_orders VALUES (''ORD-013'', ''Mike Supply'', ''Gadget Z'', 35, 29.99, 1049.65, ''2024-01-27'', ''processing'')');
end;

{ Compares source and target orders column by column, recording exact matches, partial matches with per-field discrepancies (with severity), and source-only/target-only records. }
procedure RunReconciliation(const RunId: string);
var
  DS, DS2: TDataSet;
  SourceCount, TargetCount: Integer;
  Matched, Mismatched, SourceOnly, TargetOnly: Integer;
  OrderId, SourceVal, TargetVal: string;
  MatchedCols, TotalCols, MismatchedColCount: Integer;
  MismatchedColList: string;
  Severity: string;
  Columns: array[0..6] of string;
  I: Integer;
begin
  Columns[0] := 'customer_name';
  Columns[1] := 'product';
  Columns[2] := 'quantity';
  Columns[3] := 'unit_price';
  Columns[4] := 'total_amount';
  Columns[5] := 'order_date';
  Columns[6] := 'status';

  TotalCols := 7;

  // Create run record
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO reconciliation_runs (run_id, source_table, target_table, ' +
    'match_key, compare_columns, status, started_at) VALUES ' +
    '(''%s'', ''source_orders'', ''target_orders'', ''order_id'', ' +
    '''customer_name,product,quantity,unit_price,total_amount,order_date,status'', ' +
    '''running'', datetime(''now''))',
    [RunId]));

  // Count totals
  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM source_orders');
  try
    SourceCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM target_orders');
  try
    TargetCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  Matched := 0;
  Mismatched := 0;
  SourceOnly := 0;
  TargetOnly := 0;

  // Find records in source
  DS := Conn.ExecuteQuery('SELECT * FROM source_orders ORDER BY order_id');
  try
    while not DS.EOF do
    begin
      OrderId := DS.FieldByName('order_id').AsString;

      // Check if exists in target
      DS2 := Conn.ExecuteQuery(Format(
        'SELECT * FROM target_orders WHERE order_id = ''%s''', [OrderId]));
      try
        if DS2.IsEmpty then
        begin
          // Source only
          Inc(SourceOnly);
          Conn.ExecuteNonQuery(Format(
            'INSERT INTO discrepancies (run_id, record_key, discrepancy_type, severity) ' +
            'VALUES (''%s'', ''%s'', ''source_only'', ''high'')',
            [RunId, OrderId]));
          Conn.ExecuteNonQuery(Format(
            'INSERT INTO match_results (run_id, record_key, match_status, match_score, ' +
            'matched_columns, total_columns) VALUES (''%s'', ''%s'', ''no_match'', 0, 0, %d)',
            [RunId, OrderId, TotalCols]));
        end
        else
        begin
          // Compare columns
          MatchedCols := 0;
          MismatchedColCount := 0;
          MismatchedColList := '';

          for I := 0 to 6 do
          begin
            SourceVal := DS.FieldByName(Columns[I]).AsString;
            TargetVal := DS2.FieldByName(Columns[I]).AsString;
            if SourceVal = TargetVal then
              Inc(MatchedCols)
            else
            begin
              Inc(MismatchedColCount);
              if MismatchedColList <> '' then
                MismatchedColList := MismatchedColList + ',';
              MismatchedColList := MismatchedColList + Columns[I];

              // Determine severity
              if (Columns[I] = 'total_amount') or (Columns[I] = 'quantity') then
                Severity := 'critical'
              else if (Columns[I] = 'unit_price') then
                Severity := 'high'
              else if (Columns[I] = 'status') or (Columns[I] = 'order_date') then
                Severity := 'medium'
              else
                Severity := 'low';

              Conn.ExecuteNonQuery(Format(
                'INSERT INTO discrepancies (run_id, record_key, discrepancy_type, ' +
                'column_name, source_value, target_value, severity) VALUES ' +
                '(''%s'', ''%s'', ''mismatch'', ''%s'', ''%s'', ''%s'', ''%s'')',
                [RunId, OrderId, Columns[I], SourceVal, TargetVal, Severity]));
            end;
          end;

          if MismatchedColCount = 0 then
          begin
            Inc(Matched);
            Conn.ExecuteNonQuery(Format(
              'INSERT INTO match_results (run_id, record_key, match_status, match_score, ' +
              'matched_columns, total_columns) VALUES (''%s'', ''%s'', ''exact_match'', 100, %d, %d)',
              [RunId, OrderId, TotalCols, TotalCols]));
          end
          else
          begin
            Inc(Mismatched);
            Conn.ExecuteNonQuery(Format(
              'INSERT INTO match_results (run_id, record_key, match_status, match_score, ' +
              'matched_columns, total_columns, mismatched_columns) VALUES ' +
              '(''%s'', ''%s'', ''partial_match'', %.1f, %d, %d, ''%s'')',
              [RunId, OrderId, MatchedCols * 100.0 / TotalCols,
               MatchedCols, TotalCols, MismatchedColList]));
          end;
        end;
      finally
        DS2.Free;
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Find records only in target
  DS := Conn.ExecuteQuery(
    'SELECT order_id FROM target_orders WHERE order_id NOT IN ' +
    '(SELECT order_id FROM source_orders)');
  try
    while not DS.EOF do
    begin
      Inc(TargetOnly);
      OrderId := DS.FieldByName('order_id').AsString;
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO discrepancies (run_id, record_key, discrepancy_type, severity) ' +
        'VALUES (''%s'', ''%s'', ''target_only'', ''high'')',
        [RunId, OrderId]));
      Conn.ExecuteNonQuery(Format(
        'INSERT INTO match_results (run_id, record_key, match_status, match_score, ' +
        'matched_columns, total_columns) VALUES (''%s'', ''%s'', ''no_match'', 0, 0, %d)',
        [RunId, OrderId, TotalCols]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Update run record
  Conn.ExecuteNonQuery(Format(
    'UPDATE reconciliation_runs SET total_source = %d, total_target = %d, ' +
    'matched = %d, mismatched = %d, source_only = %d, target_only = %d, ' +
    'status = ''completed'', completed_at = datetime(''now'') WHERE run_id = ''%s''',
    [SourceCount, TargetCount, Matched, Mismatched, SourceOnly, TargetOnly, RunId]));
end;

// ============================================================
// Demo Sections
// ============================================================

{ Displays the record counts for source (ERP) and target (Warehouse) tables and shows a sample of source order records. }
procedure Demo1_DataSources;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Data Sources ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM source_orders');
  try
    WriteLn(Format('   Source (ERP): %d orders', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM target_orders');
  try
    WriteLn(Format('   Target (Warehouse): %d orders', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Source sample:');
  DS := Conn.ExecuteQuery('SELECT order_id, customer_name, quantity, total_amount, status FROM source_orders LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %-18s qty:%3d  total:%8.2f  %s',
        [DS.FieldByName('order_id').AsString,
         DS.FieldByName('customer_name').AsString,
         DS.FieldByName('quantity').AsInteger,
         DS.FieldByName('total_amount').AsFloat,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Executes the reconciliation process and displays the summary: source/target counts, matched, mismatched, and orphan records. }
procedure Demo2_RunReconciliation;
var
  DS: TDataSet;
begin
  WriteLn('=== 2. Run Reconciliation ===');
  WriteLn;

  RunReconciliation('recon-001');

  DS := Conn.ExecuteQuery(
    'SELECT * FROM reconciliation_runs WHERE run_id = ''recon-001''');
  try
    WriteLn(Format('   Run: %s', [DS.FieldByName('run_id').AsString]));
    WriteLn(Format('   Source: %s (%d records)',
      [DS.FieldByName('source_table').AsString, DS.FieldByName('total_source').AsInteger]));
    WriteLn(Format('   Target: %s (%d records)',
      [DS.FieldByName('target_table').AsString, DS.FieldByName('total_target').AsInteger]));
    WriteLn(Format('   Matched: %d', [DS.FieldByName('matched').AsInteger]));
    WriteLn(Format('   Mismatched: %d', [DS.FieldByName('mismatched').AsInteger]));
    WriteLn(Format('   Source only: %d', [DS.FieldByName('source_only').AsInteger]));
    WriteLn(Format('   Target only: %d', [DS.FieldByName('target_only').AsInteger]));
    WriteLn(Format('   Status: %s', [DS.FieldByName('status').AsString]));
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays match status, match score, matched column count, and mismatched columns for every compared record. }
procedure Demo3_MatchResults;
var
  DS: TDataSet;
begin
  WriteLn('=== 3. Match Results ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT record_key, match_status, ROUND(match_score, 1) as score, ' +
    'matched_columns, total_columns, COALESCE(mismatched_columns, ''-'') as mismatch ' +
    'FROM match_results WHERE run_id = ''recon-001'' ORDER BY match_score DESC, record_key');
  try
    WriteLn(Format('   %-10s %-15s %-7s %-6s %s',
      ['Key', 'Status', 'Score', 'Match', 'Mismatched Columns']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-15s %-7s %d/%d   %s',
        [DS.FieldByName('record_key').AsString,
         DS.FieldByName('match_status').AsString,
         DS.FieldByName('score').AsString,
         DS.FieldByName('matched_columns').AsInteger,
         DS.FieldByName('total_columns').AsInteger,
         DS.FieldByName('mismatch').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Lists all field-level mismatches with source vs target values and severity, plus records found only in source or only in target. }
procedure Demo4_DiscrepancyReport;
var
  DS: TDataSet;
begin
  WriteLn('=== 4. Discrepancy Report ===');
  WriteLn;

  WriteLn('   Field-level mismatches:');
  DS := Conn.ExecuteQuery(
    'SELECT record_key, column_name, source_value, target_value, severity ' +
    'FROM discrepancies WHERE run_id = ''recon-001'' AND discrepancy_type = ''mismatch'' ' +
    'ORDER BY record_key, column_name');
  try
    WriteLn(Format('   %-10s %-15s %-18s %-18s %s',
      ['Key', 'Column', 'Source', 'Target', 'Severity']));
    WriteLn('   ' + StringOfChar('-', 80));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-15s %-18s %-18s %s',
        [DS.FieldByName('record_key').AsString,
         DS.FieldByName('column_name').AsString,
         DS.FieldByName('source_value').AsString,
         DS.FieldByName('target_value').AsString,
         DS.FieldByName('severity').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Missing records:');
  DS := Conn.ExecuteQuery(
    'SELECT record_key, discrepancy_type, severity ' +
    'FROM discrepancies WHERE run_id = ''recon-001'' ' +
    'AND discrepancy_type IN (''source_only'', ''target_only'') ' +
    'ORDER BY discrepancy_type, record_key');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %s  [%s]',
        [DS.FieldByName('record_key').AsString,
         DS.FieldByName('discrepancy_type').AsString,
         DS.FieldByName('severity').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Shows discrepancy counts grouped by severity level (critical/high/medium/low) and identifies which columns have the most mismatches. }
procedure Demo5_SeverityAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Severity Analysis ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT severity, COUNT(*) as cnt FROM discrepancies ' +
    'WHERE run_id = ''recon-001'' GROUP BY severity ORDER BY ' +
    'CASE severity WHEN ''critical'' THEN 1 WHEN ''high'' THEN 2 ' +
    'WHEN ''medium'' THEN 3 WHEN ''low'' THEN 4 END');
  try
    WriteLn('   Discrepancies by severity:');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %d',
        [DS.FieldByName('severity').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Most affected columns:');
  DS := Conn.ExecuteQuery(
    'SELECT column_name, COUNT(*) as cnt FROM discrepancies ' +
    'WHERE run_id = ''recon-001'' AND discrepancy_type = ''mismatch'' ' +
    'GROUP BY column_name ORDER BY cnt DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %d mismatch(es)',
        [DS.FieldByName('column_name').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Marks selected discrepancies as resolved or ignored with notes, then displays resolution status counts (open/resolved/ignored). }
procedure Demo6_ResolveDiscrepancies;
var
  DS: TDataSet;
begin
  WriteLn('=== 6. Resolve Discrepancies ===');
  WriteLn;

  // Resolve some discrepancies
  Conn.ExecuteNonQuery(
    'UPDATE discrepancies SET resolution_status = ''resolved'', ' +
    'resolved_by = ''admin'', resolved_at = datetime(''now''), ' +
    'notes = ''Status updated in source'' ' +
    'WHERE record_key = ''ORD-004'' AND column_name = ''status''');
  WriteLn('   Resolved: ORD-004 status mismatch (updated in source)');

  Conn.ExecuteNonQuery(
    'UPDATE discrepancies SET resolution_status = ''ignored'', ' +
    'resolved_by = ''admin'', resolved_at = datetime(''now''), ' +
    'notes = ''Customer name variant acceptable'' ' +
    'WHERE record_key = ''ORD-006'' AND column_name = ''customer_name''');
  WriteLn('   Ignored: ORD-006 customer_name (variant acceptable)');

  Conn.ExecuteNonQuery(
    'UPDATE discrepancies SET resolution_status = ''resolved'', ' +
    'resolved_by = ''ops'', resolved_at = datetime(''now''), ' +
    'notes = ''Date corrected in target'' ' +
    'WHERE record_key = ''ORD-008'' AND column_name = ''order_date''');
  WriteLn('   Resolved: ORD-008 order_date (corrected in target)');

  WriteLn;
  WriteLn('   Resolution status:');
  DS := Conn.ExecuteQuery(
    'SELECT resolution_status, COUNT(*) as cnt FROM discrepancies ' +
    'WHERE run_id = ''recon-001'' GROUP BY resolution_status ORDER BY resolution_status');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d',
        [DS.FieldByName('resolution_status').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Compares total amounts between source and target, calculates the difference for matched records, and lists records with amount discrepancies. }
procedure Demo7_AmountReconciliation;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Amount Reconciliation ===');
  WriteLn;

  // Compare total amounts between source and target
  DS := Conn.ExecuteQuery(
    'SELECT SUM(total_amount) as total FROM source_orders');
  try
    WriteLn(Format('   Source total amount: %.2f', [DS.FieldByName('total').AsFloat]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT SUM(total_amount) as total FROM target_orders');
  try
    WriteLn(Format('   Target total amount: %.2f', [DS.FieldByName('total').AsFloat]));
  finally
    DS.Free;
  end;

  // Matched records amount comparison
  DS := Conn.ExecuteQuery(
    'SELECT SUM(s.total_amount) as source_sum, SUM(t.total_amount) as target_sum, ' +
    'SUM(s.total_amount) - SUM(t.total_amount) as difference ' +
    'FROM source_orders s JOIN target_orders t ON s.order_id = t.order_id');
  try
    WriteLn(Format('   Matched records - Source: %.2f, Target: %.2f',
      [DS.FieldByName('source_sum').AsFloat, DS.FieldByName('target_sum').AsFloat]));
    WriteLn(Format('   Amount difference: %.2f', [DS.FieldByName('difference').AsFloat]));
  finally
    DS.Free;
  end;

  // Per-record amount differences
  WriteLn;
  WriteLn('   Records with amount discrepancies:');
  DS := Conn.ExecuteQuery(
    'SELECT s.order_id, s.total_amount as source_amt, t.total_amount as target_amt, ' +
    's.total_amount - t.total_amount as diff ' +
    'FROM source_orders s JOIN target_orders t ON s.order_id = t.order_id ' +
    'WHERE s.total_amount != t.total_amount ORDER BY ABS(s.total_amount - t.total_amount) DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  source: %8.2f  target: %8.2f  diff: %8.2f',
        [DS.FieldByName('order_id').AsString,
         DS.FieldByName('source_amt').AsFloat,
         DS.FieldByName('target_amt').AsFloat,
         DS.FieldByName('diff').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays match results grouped by status with average scores, plus a histogram of match scores in percentage buckets. }
procedure Demo8_MatchScoreDistribution;
var
  DS: TDataSet;
begin
  WriteLn('=== 8. Match Score Distribution ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT match_status, COUNT(*) as cnt, ROUND(AVG(match_score), 1) as avg_score ' +
    'FROM match_results WHERE run_id = ''recon-001'' ' +
    'GROUP BY match_status ORDER BY avg_score DESC');
  try
    WriteLn(Format('   %-15s %-8s %s', ['Status', 'Count', 'Avg Score']));
    WriteLn('   ' + StringOfChar('-', 35));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s %-8d %s',
        [DS.FieldByName('match_status').AsString,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('avg_score').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Score histogram:');
  DS := Conn.ExecuteQuery(
    'SELECT CASE ' +
    '  WHEN match_score = 100 THEN ''100%  '' ' +
    '  WHEN match_score >= 80 THEN ''80-99%'' ' +
    '  WHEN match_score >= 50 THEN ''50-79%'' ' +
    '  WHEN match_score > 0 THEN ''1-49% '' ' +
    '  ELSE ''0%    '' END as bucket, COUNT(*) as cnt ' +
    'FROM match_results WHERE run_id = ''recon-001'' ' +
    'GROUP BY bucket ORDER BY bucket DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s  %d  %s',
        [DS.FieldByName('bucket').AsString,
         DS.FieldByName('cnt').AsInteger,
         StringOfChar('#', DS.FieldByName('cnt').AsInteger * 3)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Inserts simulated historical reconciliation runs and displays the trend of match rates over time. }
procedure Demo9_ReconciliationTrend;
var
  DS: TDataSet;
begin
  WriteLn('=== 9. Reconciliation Trend (Simulated) ===');
  WriteLn;

  // Simulate historical runs
  Conn.ExecuteNonQuery(
    'INSERT INTO reconciliation_runs (run_id, source_table, target_table, match_key, ' +
    'compare_columns, total_source, total_target, matched, mismatched, source_only, target_only, ' +
    'status, started_at, completed_at) VALUES ' +
    '(''recon-hist-1'', ''source_orders'', ''target_orders'', ''order_id'', ''all'', ' +
    '10, 9, 6, 3, 1, 0, ''completed'', ''2024-01-20 08:00:00'', ''2024-01-20 08:00:01'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO reconciliation_runs (run_id, source_table, target_table, match_key, ' +
    'compare_columns, total_source, total_target, matched, mismatched, source_only, target_only, ' +
    'status, started_at, completed_at) VALUES ' +
    '(''recon-hist-2'', ''source_orders'', ''target_orders'', ''order_id'', ''all'', ' +
    '11, 10, 7, 2, 1, 0, ''completed'', ''2024-01-21 08:00:00'', ''2024-01-21 08:00:01'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO reconciliation_runs (run_id, source_table, target_table, match_key, ' +
    'compare_columns, total_source, total_target, matched, mismatched, source_only, target_only, ' +
    'status, started_at, completed_at) VALUES ' +
    '(''recon-hist-3'', ''source_orders'', ''target_orders'', ''order_id'', ''all'', ' +
    '12, 11, 8, 2, 1, 0, ''completed'', ''2024-01-22 08:00:00'', ''2024-01-22 08:00:01'')');

  WriteLn('   Reconciliation history:');
  DS := Conn.ExecuteQuery(
    'SELECT run_id, total_source, matched, mismatched, source_only, target_only, ' +
    'ROUND(matched * 100.0 / total_source, 1) as match_rate, started_at ' +
    'FROM reconciliation_runs ORDER BY started_at');
  try
    WriteLn(Format('   %-14s %-6s %-8s %-6s %-5s %-5s %s',
      ['Run', 'Total', 'Matched', 'Mismtch', 'S-Only', 'T-Only', 'Rate']));
    WriteLn('   ' + StringOfChar('-', 65));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-14s %-6d %-8d %-6d %-5d %-5d %s%%',
        [DS.FieldByName('run_id').AsString,
         DS.FieldByName('total_source').AsInteger,
         DS.FieldByName('matched').AsInteger,
         DS.FieldByName('mismatched').AsInteger,
         DS.FieldByName('source_only').AsInteger,
         DS.FieldByName('target_only').AsInteger,
         DS.FieldByName('match_rate').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays overall match rate, discrepancy resolution progress, and counts of open discrepancies by severity level. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
  TotalDisc, OpenDisc, ResolvedDisc: Integer;
  MatchRate: Double;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  // Current run stats
  DS := Conn.ExecuteQuery(
    'SELECT matched, mismatched, source_only, target_only, total_source ' +
    'FROM reconciliation_runs WHERE run_id = ''recon-001''');
  try
    MatchRate := DS.FieldByName('matched').AsInteger * 100.0 /
      DS.FieldByName('total_source').AsInteger;
    WriteLn(Format('   Match rate: %.1f%%', [MatchRate]));
    WriteLn(Format('   Exact matches: %d / %d', [DS.FieldByName('matched').AsInteger,
      DS.FieldByName('total_source').AsInteger]));
  finally
    DS.Free;
  end;

  // Discrepancy resolution
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM discrepancies WHERE run_id = ''recon-001''');
  try
    TotalDisc := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM discrepancies WHERE run_id = ''recon-001'' ' +
    'AND resolution_status = ''open''');
  try
    OpenDisc := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM discrepancies WHERE run_id = ''recon-001'' ' +
    'AND resolution_status IN (''resolved'', ''ignored'')');
  try
    ResolvedDisc := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total discrepancies: %d', [TotalDisc]));
  WriteLn(Format('   Open: %d, Resolved: %d', [OpenDisc, ResolvedDisc]));
  if TotalDisc > 0 then
    WriteLn(Format('   Resolution rate: %.1f%%', [ResolvedDisc * 100.0 / TotalDisc]));

  // Severity breakdown
  WriteLn;
  WriteLn('   Open discrepancies by severity:');
  DS := Conn.ExecuteQuery(
    'SELECT severity, COUNT(*) as cnt FROM discrepancies ' +
    'WHERE run_id = ''recon-001'' AND resolution_status = ''open'' ' +
    'GROUP BY severity ORDER BY ' +
    'CASE severity WHEN ''critical'' THEN 1 WHEN ''high'' THEN 2 ' +
    'WHEN ''medium'' THEN 3 WHEN ''low'' THEN 4 END');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %d',
        [DS.FieldByName('severity').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

// ============================================================
// Main
// ============================================================
begin
  WriteLn('Example 124: Data Reconciliation - Cross-Source Comparison, Discrepancies');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertSourceData;
    InsertTargetData;

    Demo1_DataSources;
    Demo2_RunReconciliation;
    Demo3_MatchResults;
    Demo4_DiscrepancyReport;
    Demo5_SeverityAnalysis;
    Demo6_ResolveDiscrepancies;
    Demo7_AmountReconciliation;
    Demo8_MatchScoreDistribution;
    Demo9_ReconciliationTrend;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
