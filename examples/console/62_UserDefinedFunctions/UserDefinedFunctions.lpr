{===============================================================================
  NDXSQLite Example 62 - User-Defined Functions (UDF)
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Creating scalar functions in Pascal
  - Creating aggregate functions
  - Using functions with multiple arguments
  - Handling NULL values in functions
  - Registering and unregistering functions

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program UserDefinedFunctions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math, Variants,
  ndxsqliteconnection, ndxsqliteconnectionintf, ndxsqliteudf;

type
  { Helper class for UDF callbacks }
  TMyFunctions = class
  public
    // Scalar function: doubles a number
    procedure DoubleValue(var AResult: TNDXSQLiteResult;
      const AArgs: array of TNDXSQLiteValue);

    // Scalar function: concatenates strings with separator
    procedure ConcatWithSep(var AResult: TNDXSQLiteResult;
      const AArgs: array of TNDXSQLiteValue);

    // Scalar function: calculates string length in words
    procedure WordCount(var AResult: TNDXSQLiteResult;
      const AArgs: array of TNDXSQLiteValue);

    // Aggregate function: geometric mean
    procedure GeoMeanStep(AContext: Pointer;
      const AArgs: array of TNDXSQLiteValue);
    procedure GeoMeanFinal(var AResult: TNDXSQLiteResult;
      AContext: Pointer);
  end;

type
  // Context for geometric mean aggregate
  PGeoMeanContext = ^TGeoMeanContext;
  TGeoMeanContext = record
    Product: Double;
    Count: Integer;
  end;

{ TMyFunctions }

procedure TMyFunctions.DoubleValue(var AResult: TNDXSQLiteResult;
  const AArgs: array of TNDXSQLiteValue);
begin
  if (Length(AArgs) = 0) or AArgs[0].IsNull then
    AResult.SetNull
  else
    AResult.SetDouble(AArgs[0].AsDouble * 2);
end;

{ Concatenates multiple strings using the first argument as separator. }
procedure TMyFunctions.ConcatWithSep(var AResult: TNDXSQLiteResult;
  const AArgs: array of TNDXSQLiteValue);
var
  Sep, S: string;
  I: Integer;
begin
  // First argument is separator, rest are strings to concatenate
  if Length(AArgs) < 2 then
  begin
    AResult.SetNull;
    Exit;
  end;

  Sep := AArgs[0].AsString;
  S := '';

  for I := 1 to High(AArgs) do
  begin
    if not AArgs[I].IsNull then
    begin
      if S <> '' then
        S := S + Sep;
      S := S + AArgs[I].AsString;
    end;
  end;

  AResult.SetString(S);
end;

{ Counts the number of words in a text string. }
procedure TMyFunctions.WordCount(var AResult: TNDXSQLiteResult;
  const AArgs: array of TNDXSQLiteValue);
var
  Text: string;
  Count, I: Integer;
  InWord: Boolean;
begin
  if (Length(AArgs) = 0) or AArgs[0].IsNull then
  begin
    AResult.SetInt64(0);
    Exit;
  end;

  Text := Trim(AArgs[0].AsString);
  if Text = '' then
  begin
    AResult.SetInt64(0);
    Exit;
  end;

  Count := 0;
  InWord := False;

  for I := 1 to Length(Text) do
  begin
    if Text[I] in [' ', #9, #10, #13] then
      InWord := False
    else if not InWord then
    begin
      InWord := True;
      Inc(Count);
    end;
  end;

  AResult.SetInt64(Count);
end;

{ Accumulates values for the geometric mean aggregate computation. }
procedure TMyFunctions.GeoMeanStep(AContext: Pointer;
  const AArgs: array of TNDXSQLiteValue);
var
  Ctx: PGeoMeanContext;
  Val: Double;
begin
  Ctx := PGeoMeanContext(AContext);

  // Initialize on first call
  if Ctx^.Count = 0 then
    Ctx^.Product := 1.0;

  if (Length(AArgs) > 0) and not AArgs[0].IsNull then
  begin
    Val := AArgs[0].AsDouble;
    if Val > 0 then
    begin
      Ctx^.Product := Ctx^.Product * Val;
      Inc(Ctx^.Count);
    end;
  end;
end;

{ Computes the final geometric mean result from accumulated values. }
procedure TMyFunctions.GeoMeanFinal(var AResult: TNDXSQLiteResult;
  AContext: Pointer);
var
  Ctx: PGeoMeanContext;
begin
  if AContext = nil then
  begin
    AResult.SetNull;
    Exit;
  end;

  Ctx := PGeoMeanContext(AContext);

  if Ctx^.Count = 0 then
    AResult.SetNull
  else
    AResult.SetDouble(Power(Ctx^.Product, 1.0 / Ctx^.Count));
end;

var
  Conn: INDXSQLiteConnection;
  UDF: TNDXSQLiteUDF;
  Funcs: TMyFunctions;
  DBPath: string;
  V: Variant;
  FuncList: TStringList;
  I: Integer;

begin
  WriteLn('=== NDXSQLite Example 62: User-Defined Functions ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example62.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Conn := TNDXSQLiteConnection.Create(DBPath);
  Conn.Open;

  Funcs := TMyFunctions.Create;
  UDF := TNDXSQLiteUDF.Create(Conn);
  try
    // 1. Register a simple scalar function
    WriteLn('1. Registering scalar function "double_val":');
    UDF.RegisterScalarFunction('double_val', 1, @Funcs.DoubleValue);
    V := Conn.ExecuteScalar('SELECT double_val(21)');
    WriteLn('   double_val(21) = ', Integer(V));
    V := Conn.ExecuteScalar('SELECT double_val(3.14)');
    WriteLn('   double_val(3.14) = ', Double(V):0:2);
    V := Conn.ExecuteScalar('SELECT double_val(NULL)');
    WriteLn('   double_val(NULL) = ', VarToStr(V));
    WriteLn;

    // 2. Register a function with variable arguments
    WriteLn('2. Registering function "concat_sep" (variable args):');
    UDF.RegisterScalarFunction('concat_sep', -1, @Funcs.ConcatWithSep);
    V := Conn.ExecuteScalar('SELECT concat_sep(''-'', ''a'', ''b'', ''c'')');
    WriteLn('   concat_sep(''-'', ''a'', ''b'', ''c'') = ', VarToStr(V));
    V := Conn.ExecuteScalar('SELECT concat_sep('' | '', ''Hello'', ''World'')');
    WriteLn('   concat_sep('' | '', ''Hello'', ''World'') = ', VarToStr(V));
    WriteLn;

    // 3. Register word count function
    WriteLn('3. Registering function "word_count":');
    UDF.RegisterScalarFunction('word_count', 1, @Funcs.WordCount);
    V := Conn.ExecuteScalar('SELECT word_count(''Hello World'')');
    WriteLn('   word_count(''Hello World'') = ', Integer(V));
    V := Conn.ExecuteScalar('SELECT word_count(''The quick brown fox jumps over the lazy dog'')');
    WriteLn('   word_count(''The quick brown fox...'') = ', Integer(V));
    WriteLn;

    // 4. Use UDF in table queries
    WriteLn('4. Using UDFs with table data:');
    Conn.ExecuteNonQuery('CREATE TABLE items (id INTEGER PRIMARY KEY, name TEXT, price REAL)');
    Conn.ExecuteNonQuery('INSERT INTO items VALUES (1, ''Apple'', 1.50)');
    Conn.ExecuteNonQuery('INSERT INTO items VALUES (2, ''Banana'', 0.75)');
    Conn.ExecuteNonQuery('INSERT INTO items VALUES (3, ''Cherry'', 2.00)');

    V := Conn.ExecuteScalar('SELECT concat_sep('', '', name) FROM items');
    WriteLn('   All items: ', VarToStr(V));

    V := Conn.ExecuteScalar('SELECT SUM(double_val(price)) FROM items');
    WriteLn('   Sum of doubled prices: ', Double(V):0:2);
    WriteLn;

    // 5. Register aggregate function
    WriteLn('5. Registering aggregate function "geo_mean":');
    UDF.RegisterAggregateFunction('geo_mean', 1,
      @Funcs.GeoMeanStep, @Funcs.GeoMeanFinal, SizeOf(TGeoMeanContext));

    Conn.ExecuteNonQuery('CREATE TABLE numbers (value REAL)');
    Conn.ExecuteNonQuery('INSERT INTO numbers VALUES (2), (8), (4), (16)');

    V := Conn.ExecuteScalar('SELECT geo_mean(value) FROM numbers');
    WriteLn('   Geometric mean of [2, 8, 4, 16] = ', Double(V):0:4);
    WriteLn('   (Expected: 4th root of 1024 = 5.6569)');
    WriteLn;

    // 6. Check registered functions
    WriteLn('6. Listing registered functions:');
    FuncList := UDF.GetRegisteredFunctions;
    try
      for I := 0 to FuncList.Count - 1 do
        WriteLn('   - ', FuncList[I]);
    finally
      FuncList.Free;
    end;
    WriteLn;

    // 7. Unregister a function
    WriteLn('7. Unregistering "word_count":');
    UDF.UnregisterFunction('word_count');
    WriteLn('   Is "word_count" registered? ', UDF.IsFunctionRegistered('word_count'));
    WriteLn('   Is "double_val" registered? ', UDF.IsFunctionRegistered('double_val'));
    WriteLn;

    // 8. Best practices
    WriteLn('8. UDF Best Practices:');
    WriteLn('   - Keep UDF logic simple and fast');
    WriteLn('   - Always handle NULL values');
    WriteLn('   - Use aggregate functions for row-by-row calculations');
    WriteLn('   - Unregister functions when no longer needed');
    WriteLn('   - For deterministic functions, SQLite can optimize queries');
    WriteLn;

  finally
    UDF.Free;
    Funcs.Free;
  end;

  Conn.Close;
  Conn := nil;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
