# Example 13: Full-Text Search (FTS5)

This example demonstrates SQLite's FTS5 full-text search capabilities.

## What you'll learn

- Creating FTS5 virtual tables
- Simple and advanced search queries
- Phrase, prefix, and boolean search
- Search result ranking
- Highlighted snippets

## Key concepts

### Creating FTS5 table

```pascal
FTS := TNDXSQLiteFTS.Create(Connection);
FTS.CreateFTSTable('articles', ['title', 'content', 'author']);
```

### Simple search

```pascal
DataSet := FTS.Search('articles', 'Pascal');
```

### Search with ranking

```pascal
DataSet := FTS.SearchWithRank('articles', 'programming tutorial');
// Results sorted by BM25 relevance score
```

### Phrase search (exact match)

```pascal
DataSet := FTS.SearchPhrase('articles', 'Free Pascal');
```

### Prefix search (autocomplete)

```pascal
DataSet := FTS.SearchPrefix('articles', 'prog');
// Matches: programming, program, programmer...
```

### Boolean operators

```pascal
// AND, OR, NOT
DataSet := FTS.SearchBoolean('articles', 'Pascal AND database');
DataSet := FTS.SearchBoolean('articles', 'Pascal NOT Delphi');
```

### NEAR search

```pascal
// Words within 10 tokens of each other
DataSet := FTS.SearchNear('articles', ['Pascal', 'compiler'], 10);
```

### Highlighted snippets

```pascal
FTS.DefaultOptions.HighlightStart := '<b>';
FTS.DefaultOptions.HighlightEnd := '</b>';
DataSet := FTS.SearchWithSnippet('articles', 'search', 'content');
```

## Building

```bash
lazbuild FullTextSearch.lpi
```

## Running

```bash
./FullTextSearch      # Linux/macOS
FullTextSearch.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
