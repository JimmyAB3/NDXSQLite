{===============================================================================
  NDXSQLite Example 126 - Fuzzy Search
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Trigram-based similarity matching
  - Levenshtein edit distance calculation
  - Soundex phonetic matching
  - Trigram index for fast lookup
  - Combined scoring strategies

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program FuzzySearch;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Products catalog
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS products (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT,' +
    '  brand TEXT,' +
    '  description TEXT' +
    ')'
  );

  // Trigrams index
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS trigrams (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  product_id INTEGER NOT NULL,' +
    '  field_name TEXT NOT NULL,' +
    '  trigram TEXT NOT NULL,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')'
  );
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_trigrams_tri ON trigrams(trigram)');

  // Soundex index
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS soundex_index (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  product_id INTEGER NOT NULL,' +
    '  word TEXT NOT NULL,' +
    '  soundex_code TEXT NOT NULL,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')'
  );
  Conn.ExecuteNonQuery('CREATE INDEX IF NOT EXISTS idx_soundex_code ON soundex_index(soundex_code)');

  // Search results
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS search_results (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  query_text TEXT NOT NULL,' +
    '  product_id INTEGER NOT NULL,' +
    '  product_name TEXT NOT NULL,' +
    '  match_method TEXT NOT NULL,' +
    '  score REAL NOT NULL,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id)' +
    ')'
  );
end;

{ Inserts sample product data. }
procedure InsertProducts;
begin
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Bluetooth Headphones'', ''Electronics'', ''SoundMax'', ''Wireless over-ear headphones with noise cancellation'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Bluetooth Speaker'', ''Electronics'', ''SoundMax'', ''Portable waterproof speaker with 12h battery'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''USB-C Charging Cable'', ''Accessories'', ''TechLink'', ''Fast charging cable 2m braided nylon'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Wireless Mouse'', ''Peripherals'', ''ClickPro'', ''Ergonomic silent wireless mouse 2.4GHz'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Mechanical Keyboard'', ''Peripherals'', ''KeyMaster'', ''RGB mechanical keyboard with blue switches'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Laptop Stand'', ''Accessories'', ''ErgoDesk'', ''Adjustable aluminum laptop stand'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''4K Monitor'', ''Displays'', ''ViewClear'', ''27 inch 4K IPS monitor HDR'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Webcam HD'', ''Peripherals'', ''CamView'', ''1080p webcam with microphone autofocus'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''External SSD'', ''Storage'', ''DataFast'', ''1TB portable solid state drive USB-C'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Power Bank'', ''Accessories'', ''ChargePlus'', ''20000mAh fast charge power bank'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Noise Cancelling Earbuds'', ''Electronics'', ''SoundMax'', ''True wireless earbuds with ANC'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Graphics Tablet'', ''Peripherals'', ''DrawPro'', ''10 inch drawing tablet pressure sensitive'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''USB Hub'', ''Accessories'', ''TechLink'', ''7-port USB 3.0 hub powered'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Desk Lamp LED'', ''Lighting'', ''BrightWay'', ''Dimmable LED desk lamp with USB port'')');
  Conn.ExecuteNonQuery('INSERT INTO products (name, category, brand, description) VALUES (''Microphone Condenser'', ''Audio'', ''VoicePro'', ''Studio condenser microphone USB cardioid'')');
end;

// === Fuzzy Matching Algorithms ===

{ Pads a string with spaces and extracts all 3-character sequences (trigrams), returning them as a sorted deduplicated list. }
function GenerateTrigrams(const S: string): TStringList;
var
  Padded: string;
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  Padded := '  ' + LowerCase(S) + ' ';
  for I := 1 to Length(Padded) - 2 do
    Result.Add(Copy(Padded, I, 3));
end;

{ Computes the Jaccard similarity between two strings using trigrams. }
function TrigramSimilarity(const S1, S2: string): Double;
var
  T1, T2: TStringList;
  I: Integer;
  CommonCount, TotalCount: Integer;
begin
  T1 := GenerateTrigrams(S1);
  T2 := GenerateTrigrams(S2);
  try
    CommonCount := 0;
    for I := 0 to T1.Count - 1 do
      if T2.IndexOf(T1[I]) >= 0 then
        Inc(CommonCount);
    TotalCount := T1.Count + T2.Count - CommonCount;
    if TotalCount = 0 then
      Result := 0
    else
      Result := CommonCount / TotalCount;
  finally
    T1.Free;
    T2.Free;
  end;
end;

{ Computes the Levenshtein edit distance between two strings. }
function LevenshteinDistance(const S1, S2: string): Integer;
var
  D: array of array of Integer;
  Len1, Len2, I, J, Cost: Integer;
begin
  Len1 := Length(S1);
  Len2 := Length(S2);
  SetLength(D, Len1 + 1, Len2 + 1);

  for I := 0 to Len1 do
    D[I][0] := I;
  for J := 0 to Len2 do
    D[0][J] := J;

  for I := 1 to Len1 do
    for J := 1 to Len2 do
    begin
      if LowerCase(S1[I]) = LowerCase(S2[J]) then
        Cost := 0
      else
        Cost := 1;
      D[I][J] := D[I-1][J] + 1;  // deletion
      if D[I][J-1] + 1 < D[I][J] then
        D[I][J] := D[I][J-1] + 1;  // insertion
      if D[I-1][J-1] + Cost < D[I][J] then
        D[I][J] := D[I-1][J-1] + Cost;  // substitution
    end;

  Result := D[Len1][Len2];
end;

{ Computes a normalized similarity score (0-1) based on edit distance. }
function EditDistanceScore(const S1, S2: string): Double;
var
  MaxLen, Dist: Integer;
begin
  MaxLen := Length(S1);
  if Length(S2) > MaxLen then
    MaxLen := Length(S2);
  if MaxLen = 0 then
  begin
    Result := 1.0;
    Exit;
  end;
  Dist := LevenshteinDistance(S1, S2);
  Result := 1.0 - (Dist / MaxLen);
  if Result < 0 then
    Result := 0;
end;

{ Computes the Soundex phonetic code for a string. }
function Soundex(const S: string): string;
var
  I: Integer;
  C, Prev: Char;
  Code: Char;
  Upper: string;
begin
  if S = '' then
  begin
    Result := '0000';
    Exit;
  end;

  Upper := UpCase(S[1]) + Copy(S, 2, Length(S));
  Result := UpCase(Upper[1]);
  Prev := '0';

  for I := 2 to Length(Upper) do
  begin
    C := UpCase(Upper[I]);
    case C of
      'B', 'F', 'P', 'V': Code := '1';
      'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z': Code := '2';
      'D', 'T': Code := '3';
      'L': Code := '4';
      'M', 'N': Code := '5';
      'R': Code := '6';
    else
      Code := '0';
    end;

    if (Code <> '0') and (Code <> Prev) then
    begin
      Result := Result + Code;
      if Length(Result) = 4 then
        Break;
    end;
    Prev := Code;
  end;

  while Length(Result) < 4 do
    Result := Result + '0';
end;

{ Iterates over all products and inserts their name trigrams into the trigrams table for fast similarity lookups. }
procedure BuildTrigramIndex;
var
  DS: TDataSet;
  ProductId: Integer;
  Name: string;
  Trigrams: TStringList;
  I: Integer;
begin
  DS := Conn.ExecuteQuery('SELECT id, name FROM products');
  try
    while not DS.EOF do
    begin
      ProductId := DS.FieldByName('id').AsInteger;
      Name := DS.FieldByName('name').AsString;
      Trigrams := GenerateTrigrams(Name);
      try
        for I := 0 to Trigrams.Count - 1 do
          Conn.ExecuteNonQuery(Format(
            'INSERT INTO trigrams (product_id, field_name, trigram) VALUES (%d, ''name'', ''%s'')',
            [ProductId, Trigrams[I]]));
      finally
        Trigrams.Free;
      end;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

{ Splits each product name into words and stores their Soundex phonetic codes in the soundex_index table. }
procedure BuildSoundexIndex;
var
  DS: TDataSet;
  ProductId: Integer;
  Name, Word: string;
  Words: TStringList;
  I: Integer;
begin
  Words := TStringList.Create;
  try
    DS := Conn.ExecuteQuery('SELECT id, name FROM products');
    try
      while not DS.EOF do
      begin
        ProductId := DS.FieldByName('id').AsInteger;
        Name := DS.FieldByName('name').AsString;

        // Split name into words
        Words.Clear;
        Words.Delimiter := ' ';
        Words.StrictDelimiter := True;
        Words.DelimitedText := Name;

        for I := 0 to Words.Count - 1 do
        begin
          Word := Words[I];
          if Length(Word) > 1 then
            Conn.ExecuteNonQuery(Format(
              'INSERT INTO soundex_index (product_id, word, soundex_code) VALUES (%d, ''%s'', ''%s'')',
              [ProductId, Word, Soundex(Word)]));
        end;
        DS.Next;
      end;
    finally
      DS.Free;
    end;
  finally
    Words.Free;
  end;
end;

// ============================================================
// Demo Sections
// ============================================================

{ Shows how trigrams are extracted from sample words like "Mouse" and "Keyboard", displaying each 3-character sequence. }
procedure Demo1_TrigramGeneration;
var
  Trigrams: TStringList;
  I: Integer;
  S: string;
begin
  WriteLn('=== 1. Trigram Generation ===');
  WriteLn;

  S := 'Mouse';
  Trigrams := GenerateTrigrams(S);
  try
    WriteLn(Format('   Trigrams for "%s" (%d trigrams):', [S, Trigrams.Count]));
    Write('   ');
    for I := 0 to Trigrams.Count - 1 do
    begin
      Write(Format('[%s] ', [Trigrams[I]]));
    end;
    WriteLn;
  finally
    Trigrams.Free;
  end;

  WriteLn;
  S := 'Keyboard';
  Trigrams := GenerateTrigrams(S);
  try
    WriteLn(Format('   Trigrams for "%s" (%d trigrams):', [S, Trigrams.Count]));
    Write('   ');
    for I := 0 to Trigrams.Count - 1 do
    begin
      Write(Format('[%s] ', [Trigrams[I]]));
    end;
    WriteLn;
  finally
    Trigrams.Free;
  end;

  WriteLn;
end;

{ Compares pairs of strings using Jaccard similarity on their trigram sets and displays the similarity scores (0-1). }
procedure Demo2_TrigramSimilarity;
var
  Sim: Double;
begin
  WriteLn('=== 2. Trigram Similarity ===');
  WriteLn;

  WriteLn('   Comparing strings (0.0 = no match, 1.0 = identical):');
  WriteLn;

  Sim := TrigramSimilarity('Mouse', 'Mouse');
  WriteLn(Format('   "Mouse" vs "Mouse"         = %.3f', [Sim]));
  Sim := TrigramSimilarity('Mouse', 'Moose');
  WriteLn(Format('   "Mouse" vs "Moose"         = %.3f', [Sim]));
  Sim := TrigramSimilarity('Mouse', 'House');
  WriteLn(Format('   "Mouse" vs "House"         = %.3f', [Sim]));
  Sim := TrigramSimilarity('Mouse', 'Mous');
  WriteLn(Format('   "Mouse" vs "Mous"          = %.3f', [Sim]));
  Sim := TrigramSimilarity('Keyboard', 'Keybaord');
  WriteLn(Format('   "Keyboard" vs "Keybaord"   = %.3f', [Sim]));
  Sim := TrigramSimilarity('Bluetooth', 'Blutooth');
  WriteLn(Format('   "Bluetooth" vs "Blutooth"  = %.3f', [Sim]));
  Sim := TrigramSimilarity('Headphones', 'Headfones');
  WriteLn(Format('   "Headphones" vs "Headfones"= %.3f', [Sim]));
  Sim := TrigramSimilarity('Monitor', 'Keyboard');
  WriteLn(Format('   "Monitor" vs "Keyboard"    = %.3f', [Sim]));

  WriteLn;
end;

{ Calculates Levenshtein edit distance and normalized similarity score for several string pairs showing typo tolerance. }
procedure Demo3_EditDistance;
begin
  WriteLn('=== 3. Edit Distance (Levenshtein) ===');
  WriteLn;

  WriteLn('   String pairs with edit distance and normalized score:');
  WriteLn;
  WriteLn(Format('   %-25s %-25s dist  score', ['String 1', 'String 2']));
  WriteLn('   ' + StringOfChar('-', 70));

  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Headphones', 'Headphones', LevenshteinDistance('Headphones', 'Headphones'), EditDistanceScore('Headphones', 'Headphones')]));
  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Headphones', 'Headfones', LevenshteinDistance('Headphones', 'Headfones'), EditDistanceScore('Headphones', 'Headfones')]));
  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Keyboard', 'Keybaord', LevenshteinDistance('Keyboard', 'Keybaord'), EditDistanceScore('Keyboard', 'Keybaord')]));
  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Bluetooth', 'Blutooth', LevenshteinDistance('Bluetooth', 'Blutooth'), EditDistanceScore('Bluetooth', 'Blutooth')]));
  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Speaker', 'Speker', LevenshteinDistance('Speaker', 'Speker'), EditDistanceScore('Speaker', 'Speker')]));
  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Wireless', 'Wireles', LevenshteinDistance('Wireless', 'Wireles'), EditDistanceScore('Wireless', 'Wireles')]));
  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Monitor', 'Monitr', LevenshteinDistance('Monitor', 'Monitr'), EditDistanceScore('Monitor', 'Monitr')]));
  WriteLn(Format('   %-25s %-25s %d     %.3f', ['Mouse', 'Keyboard', LevenshteinDistance('Mouse', 'Keyboard'), EditDistanceScore('Mouse', 'Keyboard')]));

  WriteLn;
end;

{ Generates Soundex codes for various words and their misspellings to show how phonetically similar words produce identical codes. }
procedure Demo4_PhoneticMatching;
begin
  WriteLn('=== 4. Phonetic Matching (Soundex) ===');
  WriteLn;

  WriteLn('   Soundex codes (words that sound similar get same code):');
  WriteLn;
  WriteLn(Format('   %-20s -> %s', ['Headphones', Soundex('Headphones')]));
  WriteLn(Format('   %-20s -> %s', ['Headfones', Soundex('Headfones')]));
  WriteLn(Format('   %-20s -> %s', ['Hedphones', Soundex('Hedphones')]));
  WriteLn;
  WriteLn(Format('   %-20s -> %s', ['Bluetooth', Soundex('Bluetooth')]));
  WriteLn(Format('   %-20s -> %s', ['Blutooth', Soundex('Blutooth')]));
  WriteLn(Format('   %-20s -> %s', ['Bluetoth', Soundex('Bluetoth')]));
  WriteLn;
  WriteLn(Format('   %-20s -> %s', ['Keyboard', Soundex('Keyboard')]));
  WriteLn(Format('   %-20s -> %s', ['Keybord', Soundex('Keybord')]));
  WriteLn(Format('   %-20s -> %s', ['Keyborad', Soundex('Keyborad')]));
  WriteLn;
  WriteLn(Format('   %-20s -> %s', ['Speaker', Soundex('Speaker')]));
  WriteLn(Format('   %-20s -> %s', ['Speker', Soundex('Speker')]));
  WriteLn(Format('   %-20s -> %s', ['Speeker', Soundex('Speeker')]));
  WriteLn;
  WriteLn(Format('   %-20s -> %s', ['Monitor', Soundex('Monitor')]));
  WriteLn(Format('   %-20s -> %s', ['Mouse', Soundex('Mouse')]));
  WriteLn(Format('   %-20s -> %s', ['Microphone', Soundex('Microphone')]));

  WriteLn;
end;

{ Searches products using a weighted combination of trigram (40%), edit distance (40%), and phonetic matching (20%), displaying ranked results. }
procedure Demo5_WeightedSearch;
var
  DS: TDataSet;
  Query, ProductName: string;
  TrigramScore, EditScore, PhoneticScore, FinalScore: Double;
  ProductId: Integer;
  QuerySoundex, WordSoundex: string;
  PhoneticMatch: Boolean;
  Words: TStringList;
  I: Integer;
begin
  WriteLn('=== 5. Weighted Fuzzy Search ===');
  WriteLn;

  Query := 'blutooth hedphones';
  WriteLn(Format('   Query: "%s"', [Query]));
  WriteLn('   Weights: trigram=0.4, edit_distance=0.4, phonetic=0.2');
  WriteLn;

  // Clear previous results
  Conn.ExecuteNonQuery('DELETE FROM search_results');

  // Search each product
  DS := Conn.ExecuteQuery('SELECT id, name FROM products');
  try
    while not DS.EOF do
    begin
      ProductId := DS.FieldByName('id').AsInteger;
      ProductName := DS.FieldByName('name').AsString;

      // Trigram similarity
      TrigramScore := TrigramSimilarity(Query, ProductName);

      // Edit distance (best word match)
      EditScore := EditDistanceScore(Query, ProductName);

      // Phonetic matching
      PhoneticMatch := False;
      Words := TStringList.Create;
      try
        Words.Delimiter := ' ';
        Words.StrictDelimiter := True;
        Words.DelimitedText := Query;
        for I := 0 to Words.Count - 1 do
        begin
          QuerySoundex := Soundex(Words[I]);
          // Check against product words
          WordSoundex := Soundex(ProductName);
          if QuerySoundex = WordSoundex then
            PhoneticMatch := True;
        end;

        // Also check individual product name words
        Words.Clear;
        Words.DelimitedText := ProductName;
        for I := 0 to Words.Count - 1 do
        begin
          WordSoundex := Soundex(Words[I]);
          QuerySoundex := Soundex('blutooth');
          if WordSoundex = QuerySoundex then
            PhoneticMatch := True;
          QuerySoundex := Soundex('hedphones');
          if WordSoundex = QuerySoundex then
            PhoneticMatch := True;
        end;
      finally
        Words.Free;
      end;

      if PhoneticMatch then
        PhoneticScore := 1.0
      else
        PhoneticScore := 0.0;

      // Weighted combination
      FinalScore := TrigramScore * 0.4 + EditScore * 0.4 + PhoneticScore * 0.2;

      if FinalScore > 0.1 then
      begin
        Conn.ExecuteNonQuery(Format(
          'INSERT INTO search_results (query_text, product_id, product_name, match_method, score) ' +
          'VALUES (''%s'', %d, ''%s'', ''weighted'', %.4f)',
          [Query, ProductId, ProductName, FinalScore]));
      end;

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Show results
  WriteLn(Format('   %-30s %-8s %-8s %-8s %s',
    ['Product', 'Trigram', 'Edit', 'Phonetic', 'Final']));
  WriteLn('   ' + StringOfChar('-', 75));

  DS := Conn.ExecuteQuery(
    'SELECT product_name, score FROM search_results ' +
    'WHERE query_text = ''blutooth hedphones'' ORDER BY score DESC LIMIT 8');
  try
    while not DS.EOF do
    begin
      ProductName := DS.FieldByName('product_name').AsString;
      FinalScore := DS.FieldByName('score').AsFloat;
      TrigramScore := TrigramSimilarity(Query, ProductName);
      EditScore := EditDistanceScore(Query, ProductName);
      WriteLn(Format('   %-30s %-8.3f %-8.3f %-8s %.3f',
        [ProductName, TrigramScore, EditScore,
         BoolToStr(Soundex('Bluetooth') = Soundex(Copy(ProductName, 1, Pos(' ', ProductName + ' ') - 1)), 'yes', 'no'),
         FinalScore]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Tests fuzzy matching against common typos in search queries, finding matching products despite spelling errors. }
procedure Demo6_TypoTolerance;
var
  DS: TDataSet;
  Queries: array[0..5] of string;
  Query, ProductName: string;
  I: Integer;
  Score: Double;
  Found: Boolean;
begin
  WriteLn('=== 6. Typo Tolerance ===');
  WriteLn;

  Queries[0] := 'wireles mouse';
  Queries[1] := 'mecanical keybord';
  Queries[2] := 'usb chargng cable';
  Queries[3] := 'externl ssd';
  Queries[4] := 'webcm hd';
  Queries[5] := 'pwoer bank';

  for I := 0 to 5 do
  begin
    Query := Queries[I];
    WriteLn(Format('   Query: "%s"', [Query]));
    Found := False;

    DS := Conn.ExecuteQuery('SELECT id, name FROM products');
    try
      while not DS.EOF do
      begin
        ProductName := DS.FieldByName('name').AsString;
        Score := TrigramSimilarity(Query, ProductName) * 0.5 +
                 EditDistanceScore(Query, ProductName) * 0.5;
        if Score > 0.30 then
        begin
          WriteLn(Format('   -> %-30s (score: %.3f)', [ProductName, Score]));
          Found := True;
        end;
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    if not Found then
      WriteLn('   -> No matches found');
    WriteLn;
  end;
end;

{ Searches for "speaker" across all products, stores results with combined scores, and displays them ranked by relevance. }
procedure Demo7_SearchRanking;
var
  DS: TDataSet;
  Query, ProductName: string;
  Score: Double;
  Rank: Integer;
begin
  WriteLn('=== 7. Search Ranking ===');
  WriteLn;

  Query := 'speaker';
  WriteLn(Format('   Query: "%s" (ranked by combined score)', [Query]));
  WriteLn;

  // Clear and rebuild results
  Conn.ExecuteNonQuery(Format('DELETE FROM search_results WHERE query_text = ''%s''', [Query]));

  DS := Conn.ExecuteQuery('SELECT id, name FROM products');
  try
    while not DS.EOF do
    begin
      ProductName := DS.FieldByName('name').AsString;
      Score := TrigramSimilarity(Query, ProductName) * 0.5 +
               EditDistanceScore(Query, ProductName) * 0.5;
      if Score > 0.05 then
        Conn.ExecuteNonQuery(Format(
          'INSERT INTO search_results (query_text, product_id, product_name, match_method, score) ' +
          'VALUES (''%s'', %d, ''%s'', ''ranked'', %.4f)',
          [Query, DS.FieldByName('id').AsInteger, ProductName, Score]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  Rank := 0;
  DS := Conn.ExecuteQuery(Format(
    'SELECT product_name, ROUND(score, 3) as sc FROM search_results ' +
    'WHERE query_text = ''%s'' ORDER BY score DESC LIMIT 10', [Query]));
  try
    WriteLn(Format('   %-5s %-30s %s', ['Rank', 'Product', 'Score']));
    WriteLn('   ' + StringOfChar('-', 45));
    while not DS.EOF do
    begin
      Inc(Rank);
      WriteLn(Format('   #%-4d %-30s %s',
        [Rank, DS.FieldByName('product_name').AsString,
         DS.FieldByName('sc').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Combines trigram similarity with substring containment detection, boosting scores for products that contain the query as a prefix or substring. }
procedure Demo8_PartialMatching;
var
  DS: TDataSet;
  Query, ProductName: string;
  Score: Double;
begin
  WriteLn('=== 8. Partial Word Matching ===');
  WriteLn;

  WriteLn('   Prefix/substring matching combined with fuzzy:');
  WriteLn;

  Query := 'head';
  WriteLn(Format('   Query: "%s"', [Query]));
  DS := Conn.ExecuteQuery('SELECT name FROM products ORDER BY name');
  try
    while not DS.EOF do
    begin
      ProductName := DS.FieldByName('name').AsString;
      Score := TrigramSimilarity(Query, ProductName);
      // Boost if it contains the query as prefix/substring
      if Pos(LowerCase(Query), LowerCase(ProductName)) > 0 then
        Score := Score + 0.5;
      if Score > 0.15 then
        WriteLn(Format('   -> %-30s score: %.3f %s',
          [ProductName, Score,
           BoolToStr(Pos(LowerCase(Query), LowerCase(ProductName)) > 0, '[contains]', '')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  Query := 'wire';
  WriteLn(Format('   Query: "%s"', [Query]));
  DS := Conn.ExecuteQuery('SELECT name FROM products ORDER BY name');
  try
    while not DS.EOF do
    begin
      ProductName := DS.FieldByName('name').AsString;
      Score := TrigramSimilarity(Query, ProductName);
      if Pos(LowerCase(Query), LowerCase(ProductName)) > 0 then
        Score := Score + 0.5;
      if Score > 0.15 then
        WriteLn(Format('   -> %-30s score: %.3f %s',
          [ProductName, Score,
           BoolToStr(Pos(LowerCase(Query), LowerCase(ProductName)) > 0, '[contains]', '')]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Searches across name, category, brand, and description fields with configurable weights, displaying per-field and total scores. }
procedure Demo9_MultiFieldSearch;
var
  DS: TDataSet;
  Query, Name, Category, Brand, Description: string;
  NameScore, CatScore, BrandScore, DescScore, TotalScore: Double;
begin
  WriteLn('=== 9. Multi-Field Fuzzy Search ===');
  WriteLn;

  Query := 'soundmax wireless';
  WriteLn(Format('   Query: "%s"', [Query]));
  WriteLn('   Searching across: name (0.4), category (0.1), brand (0.3), description (0.2)');
  WriteLn;

  WriteLn(Format('   %-28s %-6s %-6s %-6s %-6s %s',
    ['Product', 'Name', 'Cat', 'Brand', 'Desc', 'Total']));
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery('SELECT name, category, brand, description FROM products ORDER BY name');
  try
    while not DS.EOF do
    begin
      Name := DS.FieldByName('name').AsString;
      Category := DS.FieldByName('category').AsString;
      Brand := DS.FieldByName('brand').AsString;
      Description := DS.FieldByName('description').AsString;

      NameScore := TrigramSimilarity(Query, Name);
      CatScore := TrigramSimilarity(Query, Category);
      BrandScore := TrigramSimilarity(Query, Brand);
      DescScore := TrigramSimilarity(Query, Description);

      TotalScore := NameScore * 0.4 + CatScore * 0.1 + BrandScore * 0.3 + DescScore * 0.2;

      if TotalScore > 0.05 then
        WriteLn(Format('   %-28s %-6.3f %-6.3f %-6.3f %-6.3f %.3f',
          [Name, NameScore, CatScore, BrandScore, DescScore, TotalScore]));

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays index statistics (products, trigrams, soundex entries), trigram counts per product, and a sample of the soundex index. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM products');
  try
    WriteLn(Format('   Products indexed: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM trigrams');
  try
    WriteLn(Format('   Trigrams generated: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM soundex_index');
  try
    WriteLn(Format('   Soundex entries: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery('SELECT COUNT(*) as cnt FROM search_results');
  try
    WriteLn(Format('   Search results stored: %d', [DS.FieldByName('cnt').AsInteger]));
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Trigram coverage per product:');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, COUNT(t.id) as tri_count ' +
    'FROM products p LEFT JOIN trigrams t ON p.id = t.product_id ' +
    'GROUP BY p.id ORDER BY tri_count DESC LIMIT 8');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-30s %d trigrams',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('tri_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Soundex index sample:');
  DS := Conn.ExecuteQuery(
    'SELECT word, soundex_code FROM soundex_index ORDER BY word LIMIT 10');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-20s %s',
        [DS.FieldByName('word').AsString,
         DS.FieldByName('soundex_code').AsString]));
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
  WriteLn('Example 126: Fuzzy Search - Trigrams, Edit Distance, Phonetic, Weighted');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;
    InsertProducts;
    BuildTrigramIndex;
    BuildSoundexIndex;

    Demo1_TrigramGeneration;
    Demo2_TrigramSimilarity;
    Demo3_EditDistance;
    Demo4_PhoneticMatching;
    Demo5_WeightedSearch;
    Demo6_TypoTolerance;
    Demo7_SearchRanking;
    Demo8_PartialMatching;
    Demo9_MultiFieldSearch;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
