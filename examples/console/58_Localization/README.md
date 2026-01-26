# Example 58: Localization (i18n)

This example demonstrates internationalization (i18n) and translation management with NDXSQLite.

## Features Demonstrated

- **Multi-Language Support**: Store translations for multiple locales
- **Fallback Chains**: Regional variants with parent language fallback
- **Translation Keys**: Organized by context (buttons, messages, etc.)
- **Product Localization**: Localized content for entities
- **Coverage Reporting**: Track translation completeness
- **Missing Translation Detection**: Find untranslated strings

## Database Schema

### Languages with Fallback
```sql
CREATE TABLE languages (
  code TEXT PRIMARY KEY,       -- 'en', 'fr', 'de', 'fr-CA'
  name TEXT NOT NULL,          -- 'English', 'French'
  native_name TEXT NOT NULL,   -- 'English', 'Francais'
  is_default INTEGER DEFAULT 0,
  is_active INTEGER DEFAULT 1,
  fallback_code TEXT,          -- Parent language for regional variants
  FOREIGN KEY (fallback_code) REFERENCES languages(code)
);
```

### Translation Keys and Values
```sql
CREATE TABLE translation_keys (
  id INTEGER PRIMARY KEY,
  key_name TEXT NOT NULL UNIQUE,  -- 'btn.save', 'msg.welcome'
  context TEXT,                    -- 'buttons', 'messages'
  description TEXT
);

CREATE TABLE translations (
  id INTEGER PRIMARY KEY,
  key_id INTEGER NOT NULL,
  language_code TEXT NOT NULL,
  value TEXT NOT NULL,
  is_approved INTEGER DEFAULT 0,
  UNIQUE(key_id, language_code)
);
```

### Localized Content
```sql
CREATE TABLE products (
  id INTEGER PRIMARY KEY,
  sku TEXT NOT NULL UNIQUE,
  price REAL NOT NULL
);

CREATE TABLE product_translations (
  id INTEGER PRIMARY KEY,
  product_id INTEGER NOT NULL,
  language_code TEXT NOT NULL,
  name TEXT NOT NULL,
  description TEXT,
  UNIQUE(product_id, language_code)
);
```

## Key Operations

### Get Translation with Fallback
```pascal
function GetTranslation(const AKey, ALang: string): string;
var
  FallbackLang: string;
begin
  // Try exact language
  Result := Connection.ExecuteScalar(
    'SELECT t.value FROM translations t ' +
    'JOIN translation_keys tk ON t.key_id = tk.id ' +
    'WHERE tk.key_name = ? AND t.language_code = ?', [AKey, ALang]);

  if Result = '' then
  begin
    // Get fallback language
    FallbackLang := Connection.ExecuteScalar(
      'SELECT fallback_code FROM languages WHERE code = ?', [ALang]);
    if FallbackLang <> '' then
      Result := GetTranslation(AKey, FallbackLang);
  end;
end;
```

### Query with COALESCE Fallback
```sql
-- German with English fallback in single query
SELECT tk.key_name,
       COALESCE(t_de.value, t_en.value) as translated_value,
       CASE WHEN t_de.value IS NOT NULL THEN 'de' ELSE 'en (fallback)' END as source
FROM translation_keys tk
LEFT JOIN translations t_de ON tk.id = t_de.key_id AND t_de.language_code = 'de'
LEFT JOIN translations t_en ON tk.id = t_en.key_id AND t_en.language_code = 'en';
```

### Localized Products
```sql
SELECT p.sku, p.price,
       COALESCE(pt_lang.name, pt_en.name) as name,
       COALESCE(pt_lang.description, pt_en.description) as description
FROM products p
LEFT JOIN product_translations pt_lang ON p.id = pt_lang.product_id AND pt_lang.language_code = ?
LEFT JOIN product_translations pt_en ON p.id = pt_en.product_id AND pt_en.language_code = 'en';
```

## Fallback Chain Example

```
fr-CA (French Canada)
  -> fr (French)
      -> en (English - default)

pt-BR (Portuguese Brazil)
  -> es (Spanish)
      -> en (English - default)
```

When requesting `btn.save` in `fr-CA`:
1. Check `fr-CA` translations -> found "Sauvegarder, eh!" -> return
2. If not found, check `fr` -> found "Enregistrer" -> return
3. If not found, check `en` -> found "Save" -> return

## Translation Coverage Report

```sql
SELECT l.code, l.name,
       (SELECT COUNT(*) FROM translations WHERE language_code = l.code) as translated,
       (SELECT COUNT(*) FROM translation_keys) as total,
       ROUND(100.0 * translated / total, 1) as coverage_pct
FROM languages l
WHERE l.is_active = 1
ORDER BY coverage_pct DESC;
```

## Missing Translations

```sql
SELECT l.code as language, tk.key_name, tk.context
FROM languages l
CROSS JOIN translation_keys tk
LEFT JOIN translations t ON t.key_id = tk.id AND t.language_code = l.code
WHERE t.id IS NULL AND l.is_active = 1 AND l.code != 'en'
ORDER BY l.code, tk.key_name;
```

## Build and Run

```bash
cd 58_Localization
fpc Localization.lpr
./Localization
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Best Practices

- Use meaningful key names (`app.title`, `btn.save`, `msg.error.required`)
- Define a default language with 100% coverage
- Implement fallback chains for regional variants
- Track translation coverage per language
- Support placeholders for dynamic content (`You have {count} items`)
- Separate content localization from UI strings
- Index `language_code` columns for performance
- Export/import translations for translator workflows
