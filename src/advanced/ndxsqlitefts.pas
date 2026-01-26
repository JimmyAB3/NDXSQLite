{===============================================================================
  NDXSQLite - Full-Text Search (FTS5)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)
===============================================================================}
unit ndxsqlitefts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Variants,
  ndxsqliteconnectionintf;

type
  { FTS search options }
  TNDXFTSSearchOptions = record
    HighlightStart: string;      // Highlight start tag (default: <b>)
    HighlightEnd: string;        // Highlight end tag (default: </b>)
    SnippetSize: Integer;        // Snippet size (default: 64)
    SnippetEllipsis: string;     // Ellipsis (default: ...)
    MaxResults: Integer;         // Maximum number of results (0 = unlimited)
    UsePrefix: Boolean;          // Prefix search (word*)
    UseBooleanMode: Boolean;     // Boolean mode (AND, OR, NOT)
  end;

  { FTS search result }
  TNDXFTSSearchResult = record
    RowId: Int64;
    Rank: Double;
    Snippet: string;
    MatchInfo: string;
  end;

  { FTS5 manager }
  TNDXSQLiteFTS = class
  private
    FConnection: INDXSQLiteConnection;
    FDefaultOptions: TNDXFTSSearchOptions;

    function BuildSearchQuery(const ATableName, ASearchTerm: string;
      const AColumns: array of string;
      const AOptions: TNDXFTSSearchOptions): string;
    function EscapeSearchTerm(const ATerm: string): string;

  public
    constructor Create(AConnection: INDXSQLiteConnection);

    { Creates a FTS5 virtual table with the given columns. Optionally links to a content table. }
    procedure CreateFTSTable(const ATableName: string;
      const AColumns: array of string;
      const AContentTable: string = '';
      const AContentRowId: string = '');

    { Drops a FTS5 table and its associated shadow tables. }
    procedure DropFTSTable(const ATableName: string);

    { Returns True if the named FTS5 table exists. }
    function FTSTableExists(const ATableName: string): Boolean;

    { Searches the FTS table using the default options. Returns a result dataset. }
    function Search(const ATableName, ASearchTerm: string): TDataSet; overload;
    { Searches specific columns of the FTS table. }
    function Search(const ATableName, ASearchTerm: string;
      const AColumns: array of string): TDataSet; overload;
    { Searches with fully customized options (highlights, snippet size, max results). }
    function Search(const ATableName, ASearchTerm: string;
      const AColumns: array of string;
      const AOptions: TNDXFTSSearchOptions): TDataSet; overload;

    { Searches and returns results with highlighted snippet context around matches. }
    function SearchWithSnippet(const ATableName, ASearchTerm: string;
      const AColumn: string): TDataSet;

    { Searches and orders results by BM25 relevance rank. }
    function SearchWithRank(const ATableName, ASearchTerm: string): TDataSet;

    { Searches using boolean operators (AND, OR, NOT) in the search term. }
    function SearchBoolean(const ATableName, ASearchTerm: string): TDataSet;

    { Searches for an exact phrase match (words in order). }
    function SearchPhrase(const ATableName, APhrase: string): TDataSet;

    { Searches for words starting with the given prefix. }
    function SearchPrefix(const ATableName, APrefix: string): TDataSet;

    { Searches for terms occurring within ADistance tokens of each other. }
    function SearchNear(const ATableName: string;
      const ATerms: array of string; ADistance: Integer = 10): TDataSet;

    { Rebuilds the FTS index from scratch (rebuild command). }
    procedure RebuildIndex(const ATableName: string);
    { Merges FTS index segments for improved query performance. }
    procedure OptimizeIndex(const ATableName: string);
    { Verifies the internal consistency of the FTS index. }
    procedure IntegrityCheck(const ATableName: string);

    { Re-populates the FTS index from the linked content table. }
    procedure SyncFromContentTable(const AFTSTable, AContentTable: string);

    { Sets the tokenizer for the FTS table (e.g. unicode61, porter, trigram). }
    procedure SetTokenizer(const ATableName, ATokenizer: string);

    property DefaultOptions: TNDXFTSSearchOptions read FDefaultOptions write FDefaultOptions;
  end;

implementation

{ TNDXSQLiteFTS }

constructor TNDXSQLiteFTS.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;

  // Default options
  FDefaultOptions.HighlightStart := '<b>';
  FDefaultOptions.HighlightEnd := '</b>';
  FDefaultOptions.SnippetSize := 64;
  FDefaultOptions.SnippetEllipsis := '...';
  FDefaultOptions.MaxResults := 0;
  FDefaultOptions.UsePrefix := False;
  FDefaultOptions.UseBooleanMode := False;
end;

function TNDXSQLiteFTS.EscapeSearchTerm(const ATerm: string): string;
begin
  // Escape FTS5 special characters
  Result := StringReplace(ATerm, '"', '""', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '''''', [rfReplaceAll]);
end;

function TNDXSQLiteFTS.BuildSearchQuery(const ATableName, ASearchTerm: string;
  const AColumns: array of string;
  const AOptions: TNDXFTSSearchOptions): string;
var
  ColList: string;
  I: Integer;
  SearchExpr: string;
begin
  // Build column list
  if Length(AColumns) = 0 then
    ColList := '*'
  else
  begin
    ColList := '';
    for I := Low(AColumns) to High(AColumns) do
    begin
      if ColList <> '' then
        ColList := ColList + ', ';
      ColList := ColList + AColumns[I];
    end;
  end;

  // Build search expression
  SearchExpr := EscapeSearchTerm(ASearchTerm);
  if AOptions.UsePrefix then
    SearchExpr := SearchExpr + '*';

  // Build query
  Result := Format('SELECT %s, rank FROM %s WHERE %s MATCH ''%s''',
    [ColList, ATableName, ATableName, SearchExpr]);

  Result := Result + ' ORDER BY rank';

  if AOptions.MaxResults > 0 then
    Result := Result + Format(' LIMIT %d', [AOptions.MaxResults]);
end;

procedure TNDXSQLiteFTS.CreateFTSTable(const ATableName: string;
  const AColumns: array of string;
  const AContentTable: string;
  const AContentRowId: string);
var
  SQL: string;
  ColList: string;
  I: Integer;
begin
  // Build column list
  ColList := '';
  for I := Low(AColumns) to High(AColumns) do
  begin
    if ColList <> '' then
      ColList := ColList + ', ';
    ColList := ColList + AColumns[I];
  end;

  // Build CREATE query
  SQL := Format('CREATE VIRTUAL TABLE IF NOT EXISTS %s USING fts5(%s',
    [ATableName, ColList]);

  // Add external content options
  if AContentTable <> '' then
  begin
    SQL := SQL + Format(', content=''%s''', [AContentTable]);
    if AContentRowId <> '' then
      SQL := SQL + Format(', content_rowid=''%s''', [AContentRowId]);
  end;

  SQL := SQL + ')';

  FConnection.ExecuteNonQuery(SQL);
end;

procedure TNDXSQLiteFTS.DropFTSTable(const ATableName: string);
begin
  FConnection.ExecuteNonQuery(Format('DROP TABLE IF EXISTS %s', [ATableName]));
end;

function TNDXSQLiteFTS.FTSTableExists(const ATableName: string): Boolean;
var
  V: Variant;
begin
  V := FConnection.ExecuteScalar(Format(
    'SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name=''%s''',
    [ATableName]));
  Result := (not VarIsNull(V)) and (Integer(V) > 0);
end;

function TNDXSQLiteFTS.Search(const ATableName, ASearchTerm: string): TDataSet;
begin
  Result := Search(ATableName, ASearchTerm, [], FDefaultOptions);
end;

function TNDXSQLiteFTS.Search(const ATableName, ASearchTerm: string;
  const AColumns: array of string): TDataSet;
begin
  Result := Search(ATableName, ASearchTerm, AColumns, FDefaultOptions);
end;

function TNDXSQLiteFTS.Search(const ATableName, ASearchTerm: string;
  const AColumns: array of string;
  const AOptions: TNDXFTSSearchOptions): TDataSet;
var
  SQL: string;
begin
  SQL := BuildSearchQuery(ATableName, ASearchTerm, AColumns, AOptions);
  Result := FConnection.ExecuteQuery(SQL);
end;

function TNDXSQLiteFTS.SearchWithSnippet(const ATableName, ASearchTerm: string;
  const AColumn: string): TDataSet;
var
  SQL: string;
  SearchExpr: string;
begin
  SearchExpr := EscapeSearchTerm(ASearchTerm);

  SQL := Format(
    'SELECT *, snippet(%s, 0, ''%s'', ''%s'', ''%s'', %d) AS snippet ' +
    'FROM %s WHERE %s MATCH ''%s'' ORDER BY rank',
    [ATableName, FDefaultOptions.HighlightStart, FDefaultOptions.HighlightEnd,
     FDefaultOptions.SnippetEllipsis, FDefaultOptions.SnippetSize,
     ATableName, ATableName, SearchExpr]);

  Result := FConnection.ExecuteQuery(SQL);
end;

function TNDXSQLiteFTS.SearchWithRank(const ATableName, ASearchTerm: string): TDataSet;
var
  SQL: string;
  SearchExpr: string;
begin
  SearchExpr := EscapeSearchTerm(ASearchTerm);

  SQL := Format(
    'SELECT *, bm25(%s) AS score FROM %s WHERE %s MATCH ''%s'' ORDER BY score',
    [ATableName, ATableName, ATableName, SearchExpr]);

  Result := FConnection.ExecuteQuery(SQL);
end;

function TNDXSQLiteFTS.SearchBoolean(const ATableName, ASearchTerm: string): TDataSet;
var
  SQL: string;
begin
  // FTS5 natively supports AND, OR, NOT operators
  SQL := Format(
    'SELECT *, rank FROM %s WHERE %s MATCH ''%s'' ORDER BY rank',
    [ATableName, ATableName, EscapeSearchTerm(ASearchTerm)]);

  Result := FConnection.ExecuteQuery(SQL);
end;

function TNDXSQLiteFTS.SearchPhrase(const ATableName, APhrase: string): TDataSet;
var
  SQL: string;
begin
  // Exact phrase search with quotes
  SQL := Format(
    'SELECT *, rank FROM %s WHERE %s MATCH ''"%s"'' ORDER BY rank',
    [ATableName, ATableName, EscapeSearchTerm(APhrase)]);

  Result := FConnection.ExecuteQuery(SQL);
end;

function TNDXSQLiteFTS.SearchPrefix(const ATableName, APrefix: string): TDataSet;
var
  SQL: string;
begin
  SQL := Format(
    'SELECT *, rank FROM %s WHERE %s MATCH ''%s*'' ORDER BY rank',
    [ATableName, ATableName, EscapeSearchTerm(APrefix)]);

  Result := FConnection.ExecuteQuery(SQL);
end;

function TNDXSQLiteFTS.SearchNear(const ATableName: string;
  const ATerms: array of string; ADistance: Integer): TDataSet;
var
  SQL: string;
  TermList: string;
  I: Integer;
begin
  // Build term list for FTS5 NEAR syntax
  // FTS5 uses: NEAR(term1 term2 term3, distance)
  // Unlike FTS3/4 which used: term1 NEAR/distance term2
  TermList := '';
  for I := Low(ATerms) to High(ATerms) do
  begin
    if TermList <> '' then
      TermList := TermList + ' ';
    TermList := TermList + EscapeSearchTerm(ATerms[I]);
  end;

  SQL := Format(
    'SELECT *, rank FROM %s WHERE %s MATCH ''NEAR(%s, %d)'' ORDER BY rank',
    [ATableName, ATableName, TermList, ADistance]);

  Result := FConnection.ExecuteQuery(SQL);
end;

procedure TNDXSQLiteFTS.RebuildIndex(const ATableName: string);
begin
  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO %s(%s) VALUES(''rebuild'')', [ATableName, ATableName]));
end;

procedure TNDXSQLiteFTS.OptimizeIndex(const ATableName: string);
begin
  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO %s(%s) VALUES(''optimize'')', [ATableName, ATableName]));
end;

procedure TNDXSQLiteFTS.IntegrityCheck(const ATableName: string);
begin
  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO %s(%s) VALUES(''integrity-check'')', [ATableName, ATableName]));
end;

procedure TNDXSQLiteFTS.SyncFromContentTable(const AFTSTable, AContentTable: string);
begin
  // Delete all
  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO %s(%s) VALUES(''delete-all'')', [AFTSTable, AFTSTable]));

  // Reinsert from content table
  FConnection.ExecuteNonQuery(Format(
    'INSERT INTO %s(%s) VALUES(''rebuild'')', [AFTSTable, AFTSTable]));
end;

procedure TNDXSQLiteFTS.SetTokenizer(const ATableName, ATokenizer: string);
begin
  // Tokenizer can only be set at creation
  // This method is for information/documentation
  raise Exception.Create(
    'Tokenizer can only be set at FTS table creation. ' +
    'Use: CREATE VIRTUAL TABLE ... USING fts5(..., tokenize=''' + ATokenizer + ''')');
end;

end.
