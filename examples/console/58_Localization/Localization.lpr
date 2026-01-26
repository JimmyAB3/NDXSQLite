{===============================================================================
  NDXSQLite Example 58 - Localization (i18n)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Internationalization (i18n) database schema
  - Translation management with multiple languages
  - Fallback language chains
  - Locale-specific content retrieval
  - Dynamic translation with placeholders
  - Product/content localization

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Localization;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnection, ndxsqliteconnectionoptions;

var
  Connection: TNDXSQLiteConnection;
  Options: TNDXSQLiteConnectionOptions;
  DBPath: string;

{ Creates the i18n schema: languages with fallback chains, translation keys, translations, products, product translations, and performance indexes. }
procedure SetupLocalizationTables;
begin
  // Languages table
  Connection.ExecuteNonQuery(
    'CREATE TABLE languages (' +
    '  code TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  native_name TEXT NOT NULL,' +
    '  is_default INTEGER DEFAULT 0,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  fallback_code TEXT,' +
    '  FOREIGN KEY (fallback_code) REFERENCES languages(code)' +
    ')');

  // Translation keys (what needs to be translated)
  Connection.ExecuteNonQuery(
    'CREATE TABLE translation_keys (' +
    '  id INTEGER PRIMARY KEY,' +
    '  key_name TEXT NOT NULL UNIQUE,' +
    '  context TEXT,' +
    '  description TEXT,' +
    '  created_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  // Translations table
  Connection.ExecuteNonQuery(
    'CREATE TABLE translations (' +
    '  id INTEGER PRIMARY KEY,' +
    '  key_id INTEGER NOT NULL,' +
    '  language_code TEXT NOT NULL,' +
    '  value TEXT NOT NULL,' +
    '  is_approved INTEGER DEFAULT 0,' +
    '  updated_at TEXT DEFAULT CURRENT_TIMESTAMP,' +
    '  FOREIGN KEY (key_id) REFERENCES translation_keys(id),' +
    '  FOREIGN KEY (language_code) REFERENCES languages(code),' +
    '  UNIQUE(key_id, language_code)' +
    ')');

  // Products table (for localized content demo)
  Connection.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  sku TEXT NOT NULL UNIQUE,' +
    '  price REAL NOT NULL,' +
    '  created_at TEXT DEFAULT CURRENT_TIMESTAMP' +
    ')');

  // Localized product content
  Connection.ExecuteNonQuery(
    'CREATE TABLE product_translations (' +
    '  id INTEGER PRIMARY KEY,' +
    '  product_id INTEGER NOT NULL,' +
    '  language_code TEXT NOT NULL,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  FOREIGN KEY (product_id) REFERENCES products(id),' +
    '  FOREIGN KEY (language_code) REFERENCES languages(code),' +
    '  UNIQUE(product_id, language_code)' +
    ')');

  // Create indexes for performance
  Connection.ExecuteNonQuery('CREATE INDEX idx_translations_key ON translations(key_id)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_translations_lang ON translations(language_code)');
  Connection.ExecuteNonQuery('CREATE INDEX idx_product_trans_lang ON product_translations(language_code)');
end;

{ Populates languages (en, fr, de, es, fr-CA, pt-BR), translation keys for UI strings, translations with varying coverage per language, and localized product catalog entries. }
procedure InsertSampleData;
begin
  // Insert languages with fallback chain
  Connection.ExecuteNonQuery('INSERT INTO languages VALUES (''en'', ''English'', ''English'', 1, 1, NULL)');
  Connection.ExecuteNonQuery('INSERT INTO languages VALUES (''fr'', ''French'', ''Français'', 0, 1, ''en'')');
  Connection.ExecuteNonQuery('INSERT INTO languages VALUES (''de'', ''German'', ''Deutsch'', 0, 1, ''en'')');
  Connection.ExecuteNonQuery('INSERT INTO languages VALUES (''es'', ''Spanish'', ''Español'', 0, 1, ''en'')');
  Connection.ExecuteNonQuery('INSERT INTO languages VALUES (''fr-CA'', ''French (Canada)'', ''Français (Canada)'', 0, 1, ''fr'')');
  Connection.ExecuteNonQuery('INSERT INTO languages VALUES (''pt-BR'', ''Portuguese (Brazil)'', ''Português (Brasil)'', 0, 1, ''es'')');

  // Insert translation keys
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''app.title'', ''header'', ''Application title'')');
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''app.welcome'', ''home'', ''Welcome message'')');
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''btn.save'', ''buttons'', ''Save button text'')');
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''btn.cancel'', ''buttons'', ''Cancel button text'')');
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''btn.delete'', ''buttons'', ''Delete button text'')');
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''msg.confirm_delete'', ''dialogs'', ''Delete confirmation message'')');
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''msg.items_count'', ''messages'', ''Items count with placeholder'')');
  Connection.ExecuteNonQuery('INSERT INTO translation_keys (key_name, context, description) VALUES (''error.required'', ''validation'', ''Required field error'')');

  // Insert English translations (default)
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (1, ''en'', ''My Application'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (2, ''en'', ''Welcome to our application!'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (3, ''en'', ''Save'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (4, ''en'', ''Cancel'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (5, ''en'', ''Delete'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (6, ''en'', ''Are you sure you want to delete this item?'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (7, ''en'', ''You have {count} items in your cart'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (8, ''en'', ''This field is required'', 1)');

  // Insert French translations
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (1, ''fr'', ''Mon Application'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (2, ''fr'', ''Bienvenue dans notre application!'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (3, ''fr'', ''Enregistrer'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (4, ''fr'', ''Annuler'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (5, ''fr'', ''Supprimer'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (6, ''fr'', ''Êtes-vous sûr de vouloir supprimer cet élément?'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (7, ''fr'', ''Vous avez {count} articles dans votre panier'', 1)');

  // Insert German translations (partial)
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (1, ''de'', ''Meine Anwendung'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (2, ''de'', ''Willkommen in unserer Anwendung!'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (3, ''de'', ''Speichern'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (4, ''de'', ''Abbrechen'', 1)');

  // Insert Spanish translations
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (1, ''es'', ''Mi Aplicación'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (2, ''es'', ''¡Bienvenido a nuestra aplicación!'', 1)');
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (3, ''es'', ''Guardar'', 1)');

  // French Canadian override
  Connection.ExecuteNonQuery('INSERT INTO translations (key_id, language_code, value, is_approved) VALUES (2, ''fr-CA'', ''Bienvenue dans notre application, eh!'', 1)');

  // Insert products
  Connection.ExecuteNonQuery('INSERT INTO products (sku, price) VALUES (''LAPTOP-001'', 999.99)');
  Connection.ExecuteNonQuery('INSERT INTO products (sku, price) VALUES (''MOUSE-001'', 49.99)');
  Connection.ExecuteNonQuery('INSERT INTO products (sku, price) VALUES (''KEYBOARD-001'', 79.99)');

  // Insert product translations
  Connection.ExecuteNonQuery('INSERT INTO product_translations VALUES (1, 1, ''en'', ''Professional Laptop'', ''High-performance laptop for professionals'')');
  Connection.ExecuteNonQuery('INSERT INTO product_translations VALUES (2, 1, ''fr'', ''Ordinateur Portable Professionnel'', ''Ordinateur portable haute performance pour professionnels'')');
  Connection.ExecuteNonQuery('INSERT INTO product_translations VALUES (3, 1, ''de'', ''Professioneller Laptop'', ''Hochleistungs-Laptop für Profis'')');

  Connection.ExecuteNonQuery('INSERT INTO product_translations VALUES (4, 2, ''en'', ''Wireless Mouse'', ''Ergonomic wireless mouse'')');
  Connection.ExecuteNonQuery('INSERT INTO product_translations VALUES (5, 2, ''fr'', ''Souris Sans Fil'', ''Souris sans fil ergonomique'')');

  Connection.ExecuteNonQuery('INSERT INTO product_translations VALUES (6, 3, ''en'', ''Mechanical Keyboard'', ''Premium mechanical keyboard with RGB'')');
end;

{ Queries and displays all English translation key-value pairs ordered by key name. }
procedure DemoBasicTranslation;
var
  DS: TDataSet;
begin
  WriteLn('1. Basic Translation Retrieval');
  WriteLn('   -----------------------------');

  // Get all translations for a specific language
  WriteLn('   English translations:');
  DS := Connection.ExecuteQuery(
    'SELECT tk.key_name, t.value ' +
    'FROM translation_keys tk ' +
    'JOIN translations t ON tk.id = t.key_id ' +
    'WHERE t.language_code = ''en'' ' +
    'ORDER BY tk.key_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s = "%s"', [DS.FieldByName('key_name').AsString, DS.FieldByName('value').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays the configured language fallback hierarchy and queries German translations using COALESCE to fall back to English for missing keys. }
procedure DemoFallbackChain;
var
  DS: TDataSet;
begin
  WriteLn('2. Fallback Language Chain');
  WriteLn('   -------------------------');

  // Show fallback chain
  WriteLn('   Language fallback configuration:');
  DS := Connection.ExecuteQuery(
    'SELECT l.code, l.name, COALESCE(l.fallback_code, ''(none)'') as fallback ' +
    'FROM languages l WHERE l.is_active = 1 ORDER BY l.code');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s (%s) -> fallback: %s',
        [DS.FieldByName('code').AsString,
         DS.FieldByName('name').AsString,
         DS.FieldByName('fallback').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;

  // Query with fallback using COALESCE
  WriteLn('   Translation with fallback (German with English fallback):');
  DS := Connection.ExecuteQuery(
    'SELECT tk.key_name, ' +
    '       COALESCE(t_de.value, t_en.value) as translated_value, ' +
    '       CASE WHEN t_de.value IS NOT NULL THEN ''de'' ELSE ''en (fallback)'' END as source ' +
    'FROM translation_keys tk ' +
    'LEFT JOIN translations t_de ON tk.id = t_de.key_id AND t_de.language_code = ''de'' ' +
    'LEFT JOIN translations t_en ON tk.id = t_en.key_id AND t_en.language_code = ''en'' ' +
    'ORDER BY tk.key_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s = "%s" [%s]',
        [DS.FieldByName('key_name').AsString,
         DS.FieldByName('translated_value').AsString,
         DS.FieldByName('source').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Retrieves a translation string by key and language with fallback. }
function GetTranslation(const AKey, ALang: string): string;
var
  DS: TDataSet;
  FallbackLang: string;
begin
  Result := '';

  // Try exact language
  DS := Connection.ExecuteQuery(
    'SELECT t.value FROM translations t ' +
    'JOIN translation_keys tk ON t.key_id = tk.id ' +
    'WHERE tk.key_name = ? AND t.language_code = ?', [AKey, ALang]);
  try
    if not DS.EOF then
    begin
      Result := DS.FieldByName('value').AsString;
      Exit;
    end;
  finally
    DS.Free;
  end;

  // Get fallback language
  FallbackLang := Connection.ExecuteScalar(
    'SELECT fallback_code FROM languages WHERE code = ?', [ALang]);

  if FallbackLang <> '' then
    Result := GetTranslation(AKey, FallbackLang);
end;

{ Calls GetTranslation for various keys and languages showing recursive fallback resolution from regional to parent to default language. }
procedure DemoTranslationFunction;
begin
  WriteLn('3. Translation Function with Recursive Fallback');
  WriteLn('   ----------------------------------------------');

  WriteLn('   GetTranslation(''btn.save'', ''en''): ' + GetTranslation('btn.save', 'en'));
  WriteLn('   GetTranslation(''btn.save'', ''fr''): ' + GetTranslation('btn.save', 'fr'));
  WriteLn('   GetTranslation(''btn.save'', ''de''): ' + GetTranslation('btn.save', 'de'));
  WriteLn('   GetTranslation(''btn.save'', ''es''): ' + GetTranslation('btn.save', 'es'));
  WriteLn;
  WriteLn('   GetTranslation(''btn.delete'', ''de''): ' + GetTranslation('btn.delete', 'de') + ' (fallback to en)');
  WriteLn('   GetTranslation(''app.welcome'', ''fr-CA''): ' + GetTranslation('app.welcome', 'fr-CA') + ' (fr-CA override)');
  WriteLn('   GetTranslation(''btn.save'', ''fr-CA''): ' + GetTranslation('btn.save', 'fr-CA') + ' (fallback to fr)');
  WriteLn;
end;

{ Queries the product catalog for each language (en, fr, de) using COALESCE to fall back to English for missing product translations. }
procedure DemoLocalizedProducts;
var
  DS: TDataSet;
  Lang: string;
begin
  WriteLn('4. Localized Product Catalog');
  WriteLn('   ---------------------------');

  // Query products with localized content and fallback
  for Lang in ['en', 'fr', 'de'] do
  begin
    WriteLn(Format('   Products in %s:', [Lang]));
    DS := Connection.ExecuteQuery(
      'SELECT p.sku, p.price, ' +
      '       COALESCE(pt_lang.name, pt_en.name) as name, ' +
      '       COALESCE(pt_lang.description, pt_en.description) as description ' +
      'FROM products p ' +
      'LEFT JOIN product_translations pt_lang ON p.id = pt_lang.product_id AND pt_lang.language_code = ? ' +
      'LEFT JOIN product_translations pt_en ON p.id = pt_en.product_id AND pt_en.language_code = ''en'' ' +
      'ORDER BY p.id', [Lang]);
    try
      while not DS.EOF do
      begin
        WriteLn(Format('     [%s] %s - $%.2f',
          [DS.FieldByName('sku').AsString,
           DS.FieldByName('name').AsString,
           DS.FieldByName('price').AsFloat]));
        WriteLn(Format('           %s', [DS.FieldByName('description').AsString]));
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    WriteLn;
  end;
end;

{ Calculates and displays the percentage of translation keys covered per active language compared to total keys. }
procedure DemoTranslationCoverage;
var
  DS: TDataSet;
begin
  WriteLn('5. Translation Coverage Report');
  WriteLn('   -----------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT l.code, l.name, ' +
    '       (SELECT COUNT(*) FROM translations WHERE language_code = l.code) as translated, ' +
    '       (SELECT COUNT(*) FROM translation_keys) as total, ' +
    '       ROUND(100.0 * (SELECT COUNT(*) FROM translations WHERE language_code = l.code) / ' +
    '             (SELECT COUNT(*) FROM translation_keys), 1) as coverage_pct ' +
    'FROM languages l ' +
    'WHERE l.is_active = 1 ' +
    'ORDER BY coverage_pct DESC');
  try
    WriteLn('     Language          | Translated | Total | Coverage');
    WriteLn('     ------------------|------------|-------|----------');
    while not DS.EOF do
    begin
      WriteLn(Format('     %-17s | %10d | %5d | %7.1f%%',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('translated').AsInteger,
         DS.FieldByName('total').AsInteger,
         DS.FieldByName('coverage_pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Uses a CROSS JOIN between languages and keys with a LEFT JOIN on translations to identify and list all untranslated key/language combinations. }
procedure DemoMissingTranslations;
var
  DS: TDataSet;
begin
  WriteLn('6. Missing Translations Report');
  WriteLn('   -----------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT l.code as language, tk.key_name, tk.context ' +
    'FROM languages l ' +
    'CROSS JOIN translation_keys tk ' +
    'LEFT JOIN translations t ON t.key_id = tk.id AND t.language_code = l.code ' +
    'WHERE t.id IS NULL AND l.is_active = 1 AND l.code != ''en'' ' +
    'ORDER BY l.code, tk.key_name');
  try
    if DS.EOF then
      WriteLn('     All translations are complete!')
    else
    begin
      WriteLn('     Missing translations:');
      while not DS.EOF do
      begin
        WriteLn(Format('     [%s] %s (%s)',
          [DS.FieldByName('language').AsString,
           DS.FieldByName('key_name').AsString,
           DS.FieldByName('context').AsString]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Groups translation keys by their context field and displays the count and list of key names within each context. }
procedure DemoContextGrouping;
var
  DS: TDataSet;
begin
  WriteLn('7. Translations by Context');
  WriteLn('   -------------------------');

  DS := Connection.ExecuteQuery(
    'SELECT tk.context, COUNT(*) as key_count, ' +
    '       GROUP_CONCAT(tk.key_name, '', '') as keys ' +
    'FROM translation_keys tk ' +
    'GROUP BY tk.context ' +
    'ORDER BY tk.context');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [%s] %d keys: %s',
        [DS.FieldByName('context').AsString,
         DS.FieldByName('key_count').AsInteger,
         DS.FieldByName('keys').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Prints a summary of localization best practices including key naming, fallback chains, coverage tracking, placeholders, and indexing. }
procedure DemoBestPractices;
begin
  WriteLn('8. Localization Best Practices');
  WriteLn('   -----------------------------');
  WriteLn('   - Use meaningful key names (app.title, btn.save)');
  WriteLn('   - Define a default language with full coverage');
  WriteLn('   - Implement fallback chains for regional variants');
  WriteLn('   - Track translation coverage per language');
  WriteLn('   - Support placeholders for dynamic content');
  WriteLn('   - Separate content localization from UI strings');
  WriteLn('   - Index language_code columns for performance');
  WriteLn('   - Export/import for translator workflows');
  WriteLn;
end;

{ Deletes the example database file from disk if it exists. }
procedure Cleanup;
begin
  if FileExists(DBPath) then
    DeleteFile(DBPath);
end;

begin
  WriteLn('=== NDXSQLite Example 58: Localization (i18n) ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example58.db';
  Cleanup;

  Options := TNDXSQLiteConnectionOptions.Create;
  try
    Options.DatabasePath := DBPath;

    Connection := TNDXSQLiteConnection.Create(Options);
    try
      Connection.Open;

      SetupLocalizationTables;
      InsertSampleData;
      DemoBasicTranslation;
      DemoFallbackChain;
      DemoTranslationFunction;
      DemoLocalizedProducts;
      DemoTranslationCoverage;
      DemoMissingTranslations;
      DemoContextGrouping;
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
