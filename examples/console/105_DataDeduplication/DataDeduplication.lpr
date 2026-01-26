{===============================================================================
  NDXSQLite Example 105 - Data Deduplication
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Fuzzy matching for duplicate detection
  - Contact normalization and canonicalization
  - Duplicate group management
  - Record merging strategies
  - Canonical record lookup

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DataDeduplication;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Variants, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Contacts table with raw data
  Conn.ExecuteNonQuery(
    'CREATE TABLE contacts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  first_name TEXT,' +
    '  last_name TEXT,' +
    '  email TEXT,' +
    '  phone TEXT,' +
    '  company TEXT,' +
    '  address TEXT,' +
    '  created_at TEXT DEFAULT CURRENT_TIMESTAMP,' +
    '  canonical_id INTEGER REFERENCES contacts(id),' +
    '  is_active INTEGER DEFAULT 1' +
    ')');

  // Duplicate groups
  Conn.ExecuteNonQuery(
    'CREATE TABLE duplicate_groups (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  status TEXT DEFAULT ''pending'',' +
    '  detected_at TEXT NOT NULL,' +
    '  resolved_at TEXT,' +
    '  resolved_by TEXT' +
    ')');

  // Members of a duplicate group
  Conn.ExecuteNonQuery(
    'CREATE TABLE duplicate_members (' +
    '  group_id INTEGER REFERENCES duplicate_groups(id),' +
    '  contact_id INTEGER REFERENCES contacts(id),' +
    '  similarity_score REAL,' +
    '  match_reasons TEXT,' +
    '  is_canonical INTEGER DEFAULT 0,' +
    '  PRIMARY KEY (group_id, contact_id)' +
    ')');

  // Merge audit log
  Conn.ExecuteNonQuery(
    'CREATE TABLE merge_log (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  group_id INTEGER REFERENCES duplicate_groups(id),' +
    '  canonical_id INTEGER,' +
    '  merged_id INTEGER,' +
    '  field_name TEXT,' +
    '  kept_value TEXT,' +
    '  discarded_value TEXT,' +
    '  merged_at TEXT NOT NULL' +
    ')');

  // Normalized lookup for matching
  Conn.ExecuteNonQuery(
    'CREATE TABLE contact_normalized (' +
    '  contact_id INTEGER PRIMARY KEY REFERENCES contacts(id),' +
    '  norm_email TEXT,' +
    '  norm_name TEXT,' +
    '  norm_phone TEXT' +
    ')');

  Conn.ExecuteNonQuery('CREATE INDEX idx_norm_email ON contact_normalized(norm_email)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_norm_name ON contact_normalized(norm_name)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_norm_phone ON contact_normalized(norm_phone)');
end;

{ Populates tables with initial sample data. }
procedure SeedData;
begin
  // Group 1: Same person, different formats
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''John'', ''Smith'', ''john.smith@acme.com'', ''(555) 123-4567'', ''Acme Corp'', ''123 Main St, Suite 100'')');
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Jon'', ''Smith'', ''jsmith@acme.com'', ''555-123-4567'', ''Acme Corporation'', ''123 Main Street'')');
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''John'', ''Smth'', ''john.smith@acme.com'', ''5551234567'', ''ACME Corp.'', ''123 Main St'')');

  // Group 2: Same person, email typo
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Alice'', ''Johnson'', ''alice.johnson@techco.io'', ''(555) 987-6543'', ''TechCo'', ''456 Oak Ave'')');
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Alice'', ''Johnson'', ''alice.jonhson@techco.io'', ''555-987-6543'', ''TechCo Inc'', ''456 Oak Avenue'')');

  // Group 3: Same person, maiden/married name
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Carol'', ''Williams'', ''carol.w@startup.co'', ''(555) 222-3333'', ''StartUp LLC'', ''789 Elm Dr'')');
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Carol'', ''Davis'', ''carol.w@startup.co'', ''555-222-3333'', ''StartUp'', ''789 Elm Drive'')');

  // Unique contacts (no duplicates)
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Bob'', ''Martinez'', ''bob.m@globalinc.com'', ''(555) 444-5555'', ''Global Inc'', ''321 Pine Rd'')');
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Eve'', ''Taylor'', ''eve.taylor@design.co'', ''(555) 666-7777'', ''Design Co'', ''654 Maple Ln'')');
  Conn.ExecuteNonQuery('INSERT INTO contacts (first_name, last_name, email, phone, company, address) ' +
    'VALUES (''Frank'', ''Anderson'', ''frank@enterprise.net'', ''(555) 888-9999'', ''Enterprise Net'', ''987 Cedar Blvd'')');
end;

{ Normalizes contact data for comparison. }
procedure NormalizeContacts;
begin
  // Build normalized lookup: lowercase email, lowercase first+last, digits-only phone
  Conn.ExecuteNonQuery(
    'INSERT INTO contact_normalized (contact_id, norm_email, norm_name, norm_phone) ' +
    'SELECT id, ' +
    '  LOWER(TRIM(email)), ' +
    '  LOWER(TRIM(first_name)) || '' '' || LOWER(TRIM(last_name)), ' +
    '  REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(phone, ''('', ''''), '')'', ''''), ''-'', ''''), '' '', ''''), ''+'', '''') ' +
    'FROM contacts');
end;

// === Feature 1: Show Raw Contacts ===
{ Queries and prints all 10 raw contact records showing ID, first name, last name, email, phone, and company. }
procedure DemoRawContacts;
begin
  WriteLn('=== 1. Raw Contact Data (10 records) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT id, first_name, last_name, email, phone, company ' +
    'FROM contacts ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d %-8s %-10s %-26s %-14s %s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('first_name').AsString,
         DS.FieldByName('last_name').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('phone').AsString,
         DS.FieldByName('company').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 2: Fuzzy Matching Detection ===
{ Detects duplicate pairs using three rules (same email, same phone digits, similar names), prints matches, and creates duplicate group records with similarity scores. }
procedure DemoFuzzyMatching;
var
  GroupId: Integer;
  PairCount: Integer;
begin
  WriteLn('=== 2. Fuzzy Matching - Duplicate Detection ===');
  WriteLn;
  PairCount := 0;

  // Rule 1: Same normalized email
  WriteLn('   Rule 1: Same email (case-insensitive)');
  DS := Conn.ExecuteQuery(
    'SELECT a.contact_id as id1, b.contact_id as id2, a.norm_email ' +
    'FROM contact_normalized a ' +
    'JOIN contact_normalized b ON a.norm_email = b.norm_email AND a.contact_id < b.contact_id ' +
    'WHERE a.norm_email IS NOT NULL AND a.norm_email != ''''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Match: #%d <-> #%d (email: %s)',
        [DS.FieldByName('id1').AsInteger,
         DS.FieldByName('id2').AsInteger,
         DS.FieldByName('norm_email').AsString]));
      Inc(PairCount);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Rule 2: Same phone (digits only)
  WriteLn;
  WriteLn('   Rule 2: Same phone (digits only)');
  DS := Conn.ExecuteQuery(
    'SELECT a.contact_id as id1, b.contact_id as id2, a.norm_phone ' +
    'FROM contact_normalized a ' +
    'JOIN contact_normalized b ON a.norm_phone = b.norm_phone AND a.contact_id < b.contact_id ' +
    'WHERE a.norm_phone IS NOT NULL AND a.norm_phone != ''''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Match: #%d <-> #%d (phone: %s)',
        [DS.FieldByName('id1').AsInteger,
         DS.FieldByName('id2').AsInteger,
         DS.FieldByName('norm_phone').AsString]));
      Inc(PairCount);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Rule 3: Similar name (same first 3 chars of first_name + same last_name)
  WriteLn;
  WriteLn('   Rule 3: Similar name (first 3 chars match + same last name)');
  DS := Conn.ExecuteQuery(
    'SELECT a.contact_id as id1, b.contact_id as id2, ' +
    '  c1.first_name as fn1, c1.last_name as ln1, ' +
    '  c2.first_name as fn2, c2.last_name as ln2 ' +
    'FROM contact_normalized a ' +
    'JOIN contact_normalized b ON a.contact_id < b.contact_id ' +
    'JOIN contacts c1 ON c1.id = a.contact_id ' +
    'JOIN contacts c2 ON c2.id = b.contact_id ' +
    'WHERE SUBSTR(LOWER(c1.first_name), 1, 3) = SUBSTR(LOWER(c2.first_name), 1, 3) ' +
    '  AND LOWER(c1.last_name) = LOWER(c2.last_name) ' +
    '  AND a.contact_id < b.contact_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Match: #%d (%s %s) <-> #%d (%s %s)',
        [DS.FieldByName('id1').AsInteger,
         DS.FieldByName('fn1').AsString,
         DS.FieldByName('ln1').AsString,
         DS.FieldByName('id2').AsInteger,
         DS.FieldByName('fn2').AsString,
         DS.FieldByName('ln2').AsString]));
      Inc(PairCount);
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total potential duplicate pairs found: %d', [PairCount]));
  WriteLn;

  // Now create duplicate groups based on combined rules
  // Group 1: contacts 1, 2, 3 (John Smith variants)
  Conn.ExecuteNonQuery('INSERT INTO duplicate_groups (status, detected_at) VALUES (''pending'', ''2024-03-01 10:00:00'')');
  GroupId := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
  Conn.ExecuteNonQuery(Format('INSERT INTO duplicate_members VALUES (%d, 1, 0.95, ''email+phone+name'', 1)', [GroupId]));
  Conn.ExecuteNonQuery(Format('INSERT INTO duplicate_members VALUES (%d, 2, 0.85, ''phone+name_prefix'', 0)', [GroupId]));
  Conn.ExecuteNonQuery(Format('INSERT INTO duplicate_members VALUES (%d, 3, 0.90, ''email+phone'', 0)', [GroupId]));

  // Group 2: contacts 4, 5 (Alice Johnson)
  Conn.ExecuteNonQuery('INSERT INTO duplicate_groups (status, detected_at) VALUES (''pending'', ''2024-03-01 10:00:00'')');
  GroupId := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
  Conn.ExecuteNonQuery(Format('INSERT INTO duplicate_members VALUES (%d, 4, 0.95, ''name+phone'', 1)', [GroupId]));
  Conn.ExecuteNonQuery(Format('INSERT INTO duplicate_members VALUES (%d, 5, 0.88, ''name+phone'', 0)', [GroupId]));

  // Group 3: contacts 6, 7 (Carol - same email+phone, different last name)
  Conn.ExecuteNonQuery('INSERT INTO duplicate_groups (status, detected_at) VALUES (''pending'', ''2024-03-01 10:00:00'')');
  GroupId := Integer(Conn.ExecuteScalar('SELECT last_insert_rowid()'));
  Conn.ExecuteNonQuery(Format('INSERT INTO duplicate_members VALUES (%d, 6, 0.92, ''email+phone'', 1)', [GroupId]));
  Conn.ExecuteNonQuery(Format('INSERT INTO duplicate_members VALUES (%d, 7, 0.80, ''email+phone'', 0)', [GroupId]));
end;

// === Feature 3: Duplicate Groups Review ===
{ Queries and prints all pending duplicate groups showing group ID, status, member count, and member names with similarity percentages. }
procedure DemoDuplicateGroups;
begin
  WriteLn('=== 3. Duplicate Groups (pending review) ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT dg.id as group_id, dg.status, ' +
    '  (SELECT COUNT(*) FROM duplicate_members WHERE group_id = dg.id) as member_count, ' +
    '  GROUP_CONCAT(c.first_name || '' '' || c.last_name || '' ('' || ' +
    '    CAST(CAST(dm.similarity_score * 100 AS INTEGER) AS TEXT) || ''%)'', '' | '') as members ' +
    'FROM duplicate_groups dg ' +
    'JOIN duplicate_members dm ON dm.group_id = dg.id ' +
    'JOIN contacts c ON c.id = dm.contact_id ' +
    'GROUP BY dg.id ' +
    'ORDER BY dg.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   Group #%d [%s] (%d members):',
        [DS.FieldByName('group_id').AsInteger,
         DS.FieldByName('status').AsString,
         DS.FieldByName('member_count').AsInteger]));
      WriteLn('      ' + DS.FieldByName('members').AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 4: Merge Records ===
{ Merges Group 1 (John Smith) by picking longest values for company and address, deactivating non-canonical records, and logging all field decisions. }
procedure DemoMergeRecords;
var
  CanonId, MergedId: Integer;
  CanonVal, MergeVal: string;
  GrpId: Integer;
begin
  WriteLn('=== 4. Merge Records (Group #1: John Smith) ===');
  WriteLn;
  GrpId := 1;

  // Get canonical contact for group 1
  CanonId := Integer(Conn.ExecuteScalar(
    'SELECT contact_id FROM duplicate_members WHERE group_id = 1 AND is_canonical = 1'));
  WriteLn(Format('   Canonical record: #%d', [CanonId]));

  // Merge strategy: for each field, pick the longest/most complete value
  // Process each non-canonical member
  DS := Conn.ExecuteQuery(
    'SELECT contact_id FROM duplicate_members WHERE group_id = 1 AND is_canonical = 0 ORDER BY contact_id');
  try
    while not DS.EOF do
    begin
      MergedId := DS.FieldByName('contact_id').AsInteger;
      WriteLn(Format('   Merging #%d into #%d...', [MergedId, CanonId]));

      // Compare company field
      CanonVal := VarToStr(Conn.ExecuteScalar(Format('SELECT company FROM contacts WHERE id = %d', [CanonId])));
      MergeVal := VarToStr(Conn.ExecuteScalar(Format('SELECT company FROM contacts WHERE id = %d', [MergedId])));
      if Length(MergeVal) > Length(CanonVal) then
      begin
        Conn.ExecuteNonQuery(Format('UPDATE contacts SET company = ''%s'' WHERE id = %d', [MergeVal, CanonId]));
        Conn.ExecuteNonQuery(Format(
          'INSERT INTO merge_log (group_id, canonical_id, merged_id, field_name, kept_value, discarded_value, merged_at) ' +
          'VALUES (%d, %d, %d, ''company'', ''%s'', ''%s'', ''2024-03-01 10:05:00'')',
          [GrpId, CanonId, MergedId, MergeVal, CanonVal]));
        WriteLn(Format('      company: "%s" -> "%s"', [CanonVal, MergeVal]));
      end;

      // Compare address field
      CanonVal := VarToStr(Conn.ExecuteScalar(Format('SELECT address FROM contacts WHERE id = %d', [CanonId])));
      MergeVal := VarToStr(Conn.ExecuteScalar(Format('SELECT address FROM contacts WHERE id = %d', [MergedId])));
      if Length(MergeVal) > Length(CanonVal) then
      begin
        Conn.ExecuteNonQuery(Format('UPDATE contacts SET address = ''%s'' WHERE id = %d', [MergeVal, CanonId]));
        Conn.ExecuteNonQuery(Format(
          'INSERT INTO merge_log (group_id, canonical_id, merged_id, field_name, kept_value, discarded_value, merged_at) ' +
          'VALUES (%d, %d, %d, ''address'', ''%s'', ''%s'', ''2024-03-01 10:05:00'')',
          [GrpId, CanonId, MergedId, MergeVal, CanonVal]));
        WriteLn(Format('      address: "%s" -> "%s"', [CanonVal, MergeVal]));
      end;

      // Mark merged contact as inactive and set canonical_id
      Conn.ExecuteNonQuery(Format('UPDATE contacts SET is_active = 0, canonical_id = %d WHERE id = %d', [CanonId, MergedId]));
      WriteLn(Format('      Contact #%d deactivated, canonical_id -> #%d', [MergedId, CanonId]));

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Mark group as merged
  Conn.ExecuteNonQuery('UPDATE duplicate_groups SET status = ''merged'', resolved_at = ''2024-03-01 10:05:00'', resolved_by = ''system'' WHERE id = 1');
  WriteLn;
  WriteLn('   Group #1 status: merged');
  WriteLn;
end;

// === Feature 5: Merge Group 2 (Alice Johnson) ===
{ Merges Group 2 (Alice Johnson) by keeping the correct email (discarding typo), taking the longer address, and deactivating the duplicate record. }
procedure DemoMergeGroup2;
var
  CanonId: Integer;
begin
  WriteLn('=== 5. Merge Records (Group #2: Alice Johnson) ===');
  WriteLn;

  CanonId := Integer(Conn.ExecuteScalar(
    'SELECT contact_id FROM duplicate_members WHERE group_id = 2 AND is_canonical = 1'));
  WriteLn(Format('   Canonical record: #%d (Alice Johnson, correct email)', [CanonId]));

  // Contact 5 has typo in email, keep contact 4's email
  Conn.ExecuteNonQuery(Format('UPDATE contacts SET is_active = 0, canonical_id = %d WHERE id = 5', [CanonId]));
  Conn.ExecuteNonQuery(
    'INSERT INTO merge_log (group_id, canonical_id, merged_id, field_name, kept_value, discarded_value, merged_at) ' +
    'VALUES (2, 4, 5, ''email'', ''alice.johnson@techco.io'', ''alice.jonhson@techco.io'', ''2024-03-01 10:06:00'')');
  WriteLn('   Kept email: alice.johnson@techco.io (discarded typo: alice.jonhson@techco.io)');

  // Keep longer address
  Conn.ExecuteNonQuery('UPDATE contacts SET address = ''456 Oak Avenue'' WHERE id = 4');
  Conn.ExecuteNonQuery(
    'INSERT INTO merge_log (group_id, canonical_id, merged_id, field_name, kept_value, discarded_value, merged_at) ' +
    'VALUES (2, 4, 5, ''address'', ''456 Oak Avenue'', ''456 Oak Ave'', ''2024-03-01 10:06:00'')');
  WriteLn('   Kept address: "456 Oak Avenue" (discarded: "456 Oak Ave")');

  Conn.ExecuteNonQuery('UPDATE contacts SET is_active = 0, canonical_id = 4 WHERE id = 5');
  Conn.ExecuteNonQuery('UPDATE duplicate_groups SET status = ''merged'', resolved_at = ''2024-03-01 10:06:00'', resolved_by = ''system'' WHERE id = 2');
  WriteLn('   Contact #5 deactivated, Group #2 merged');
  WriteLn;
end;

// === Feature 6: Reject a Duplicate Group ===
{ Rejects Group 3 (Carol Williams vs Carol Davis) as not-duplicates, marking the group status as rejected since the name difference indicates distinct people. }
procedure DemoRejectGroup;
begin
  WriteLn('=== 6. Reject Duplicate Group (Group #3: Carol) ===');
  WriteLn;

  WriteLn('   Group #3: Carol Williams (#6) vs Carol Davis (#7)');
  WriteLn('   Same email and phone, but different last names');
  WriteLn('   Decision: Different people (married name change not confirmed)');

  Conn.ExecuteNonQuery(
    'UPDATE duplicate_groups SET status = ''rejected'', resolved_at = ''2024-03-01 10:07:00'', ' +
    'resolved_by = ''admin'' WHERE id = 3');
  WriteLn('   Group #3 status: rejected (not duplicates)');
  WriteLn;
end;

// === Feature 7: Canonical ID Lookup ===
{ Prints all active (canonical) contacts and all deactivated contacts showing which canonical record they were merged into. }
procedure DemoCanonicalLookup;
begin
  WriteLn('=== 7. Canonical ID Lookup ===');
  WriteLn;

  WriteLn('   Active contacts (canonical records):');
  DS := Conn.ExecuteQuery(
    'SELECT id, first_name, last_name, email, company, address ' +
    'FROM contacts WHERE is_active = 1 ORDER BY id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d %-8s %-10s %-26s %s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('first_name').AsString,
         DS.FieldByName('last_name').AsString,
         DS.FieldByName('email').AsString,
         DS.FieldByName('company').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Deactivated contacts (merged into canonical):');
  DS := Conn.ExecuteQuery(
    'SELECT c.id, c.first_name, c.last_name, c.canonical_id, ' +
    '  can.first_name || '' '' || can.last_name as canonical_name ' +
    'FROM contacts c ' +
    'JOIN contacts can ON can.id = c.canonical_id ' +
    'WHERE c.is_active = 0 ORDER BY c.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   #%d %-8s %-10s -> canonical #%d (%s)',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('first_name').AsString,
         DS.FieldByName('last_name').AsString,
         DS.FieldByName('canonical_id').AsInteger,
         DS.FieldByName('canonical_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 8: Merge Audit Log ===
{ Prints the complete merge audit log showing group ID, canonical/merged record IDs, field name, and the kept vs discarded values for each decision. }
procedure DemoMergeLog;
begin
  WriteLn('=== 8. Merge Audit Log ===');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT ml.group_id, ml.canonical_id, ml.merged_id, ml.field_name, ' +
    '  ml.kept_value, ml.discarded_value, ml.merged_at ' +
    'FROM merge_log ml ORDER BY ml.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   [Group %d] #%d <- #%d | %s: kept="%s" discarded="%s"',
        [DS.FieldByName('group_id').AsInteger,
         DS.FieldByName('canonical_id').AsInteger,
         DS.FieldByName('merged_id').AsInteger,
         DS.FieldByName('field_name').AsString,
         DS.FieldByName('kept_value').AsString,
         DS.FieldByName('discarded_value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

// === Feature 9: Statistics ===
{ Demonstrates deduplication statistics including match rates and merge counts. }
procedure DemoStatistics;
begin
  WriteLn('=== 9. Deduplication Statistics ===');
  WriteLn;

  WriteLn(Format('   Total contacts:     %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM contacts'))]));
  WriteLn(Format('   Active contacts:    %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM contacts WHERE is_active = 1'))]));
  WriteLn(Format('   Merged (inactive):  %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM contacts WHERE is_active = 0'))]));
  WriteLn;

  WriteLn('   Duplicate groups by status:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM duplicate_groups GROUP BY status ORDER BY status');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %d groups',
        [DS.FieldByName('status').AsString,
         DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn(Format('   Total merge operations: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM merge_log'))]));
  WriteLn(Format('   Deduplication rate:     %.0f%%',
    [Double(Conn.ExecuteScalar(
      'SELECT CAST(COUNT(CASE WHEN is_active = 0 THEN 1 END) AS REAL) / COUNT(*) * 100 FROM contacts'))]));
  WriteLn;
end;

// === Main Program ===
begin
  WriteLn('=== Example 105: Data Deduplication ===');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    SeedData;
    NormalizeContacts;

    DemoRawContacts;
    DemoFuzzyMatching;
    DemoDuplicateGroups;
    DemoMergeRecords;
    DemoMergeGroup2;
    DemoRejectGroup;
    DemoCanonicalLookup;
    DemoMergeLog;
    DemoStatistics;

    WriteLn('=== All data deduplication features demonstrated successfully ===');

    Conn.Close;
  finally
    Conn.Free;
  end;
end.
