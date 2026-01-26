# Example 62: User-Defined Functions (UDF)

This example demonstrates how to create custom SQL functions in Pascal using `TNDXSQLiteUDF`.

## What you'll learn

- Creating scalar functions (single value result)
- Creating aggregate functions (process multiple rows)
- Handling variable number of arguments
- Proper NULL value handling
- Managing function registration lifecycle

## Key concepts

### Scalar functions

Scalar functions take zero or more arguments and return a single value:

```pascal
type
  TMyFunctions = class
    procedure DoubleValue(var AResult: TNDXSQLiteResult;
      const AArgs: array of TNDXSQLiteValue);
  end;

procedure TMyFunctions.DoubleValue(var AResult: TNDXSQLiteResult;
  const AArgs: array of TNDXSQLiteValue);
begin
  if AArgs[0].IsNull then
    AResult.SetNull
  else
    AResult.SetDouble(AArgs[0].AsDouble * 2);
end;

// Registration
UDF := TNDXSQLiteUDF.Create(Connection);
UDF.RegisterScalarFunction('double_val', 1, @Funcs.DoubleValue);

// Usage in SQL
SELECT double_val(price) FROM products;
```

### Variable arguments

Use `-1` for the argument count to accept any number of arguments:

```pascal
UDF.RegisterScalarFunction('concat_all', -1, @Funcs.ConcatAll);

// Usage
SELECT concat_all('-', 'a', 'b', 'c');  -- Returns 'a-b-c'
```

### Aggregate functions

Aggregate functions process multiple rows and return a single result:

```pascal
type
  PSumContext = ^TSumContext;
  TSumContext = record
    Total: Double;
    Count: Integer;
  end;

procedure TMyFunctions.SumStep(AContext: Pointer;
  const AArgs: array of TNDXSQLiteValue);
var
  Ctx: PSumContext;
begin
  Ctx := PSumContext(AContext);
  if Ctx^.Count = 0 then Ctx^.Total := 0;
  if not AArgs[0].IsNull then
    Ctx^.Total := Ctx^.Total + AArgs[0].AsDouble;
  Inc(Ctx^.Count);
end;

procedure TMyFunctions.SumFinal(var AResult: TNDXSQLiteResult;
  AContext: Pointer);
var
  Ctx: PSumContext;
begin
  Ctx := PSumContext(AContext);
  AResult.SetDouble(Ctx^.Total);
end;

// Registration
UDF.RegisterAggregateFunction('my_sum', 1,
  @Funcs.SumStep, @Funcs.SumFinal, SizeOf(TSumContext));
```

### Result types

```pascal
AResult.SetNull;           // NULL value
AResult.SetInt64(42);      // Integer
AResult.SetDouble(3.14);   // Floating point
AResult.SetString('text'); // Text
AResult.SetBlob(bytes);    // Binary data
```

### Argument access

```pascal
AArgs[0].IsNull      // Check if NULL
AArgs[0].AsInt64     // Get as integer
AArgs[0].AsDouble    // Get as float
AArgs[0].AsString    // Get as string
AArgs[0].AsBytes     // Get as byte array
AArgs[0].ValueType   // Get SQLite type
```

## Use cases

- **Custom calculations**: Domain-specific math functions
- **String manipulation**: Custom formatting, parsing
- **Data validation**: Check constraints in SQL
- **Aggregations**: Custom statistics (median, mode, etc.)
- **Encoding/Hashing**: Custom transformations

## Building

```bash
lazbuild UserDefinedFunctions.lpi
```

## Running

```bash
./UserDefinedFunctions      # Linux/macOS
UserDefinedFunctions.exe    # Windows
```

## Notes

- UDFs are connection-specific; register them after opening
- For thread safety, each connection needs its own UDF registration
- Deterministic functions allow SQLite query optimization
- Always clean up with `UnregisterAll` or `Free`

## Cross-Platform

This example works on Windows, Linux, and macOS.
