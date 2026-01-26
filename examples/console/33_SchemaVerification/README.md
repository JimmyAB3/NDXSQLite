# Example 33: Schema Verification

This example demonstrates the TNDXSQLiteVerify class for database schema verification and comparison.

## What you'll learn

- Counting schema objects (tables, indexes, views, triggers)
- Listing schema objects
- Verifying schema against expectations
- Comparing two databases
- Verifying data integrity between databases
- Testing that triggers and views work

## Key concepts

### Counting schema objects

```pascal
uses ndxsqliteverify;

var
  Verify: TNDXSQLiteVerify;
  Schema: TNDXSchemaCount;
begin
  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    // Individual counts
    WriteLn('Tables: ', Verify.CountTables);
    WriteLn('Indexes: ', Verify.CountIndexes);
    WriteLn('Views: ', Verify.CountViews);
    WriteLn('Triggers: ', Verify.CountTriggers);

    // All at once
    Schema := Verify.GetSchema;
    WriteLn('Total objects: ', Schema.TableCount + Schema.IndexCount);
  finally
    Verify.Free;
  end;
end;
```

### Listing schema objects

```pascal
var
  List: TStringList;
begin
  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    List := Verify.GetTableList;  // or GetIndexList, GetViewList, GetTriggerList
    try
      for I := 0 to List.Count - 1 do
        WriteLn(List[I]);
    finally
      List.Free;
    end;
  finally
    Verify.Free;
  end;
end;
```

### Verifying schema against expectations

```pascal
var
  Expected: TNDXExpectedSchema;
  Result: TNDXVerifyResult;
begin
  Verify := TNDXSQLiteVerify.Create(Connection);
  try
    Expected := TNDXSQLiteVerify.CreateExpectedSchema(
      3,    // Expected tables
      2,    // Expected indexes
      1,    // Expected views
      1,    // Expected triggers
      True  // Test triggers
    );
    Expected.TestViews := True;

    Result := Verify.VerifySchema(Expected);
    try
      if Result.Success then
        WriteLn('Schema verification passed!')
      else
        WriteLn('Failed: ', Result.ErrorMessage);
    finally
      Result.Errors.Free;
    end;
  finally
    Verify.Free;
  end;
end;
```

### Comparing two databases

```pascal
var
  CompResult: TNDXCompareResult;
begin
  Verify := TNDXSQLiteVerify.Create(SourceConnection);
  try
    CompResult := Verify.CompareWith(TargetConnection);
    try
      WriteLn('Schemas match: ', CompResult.SchemasMatch);
      WriteLn('Data matches: ', CompResult.DataMatches);

      for I := 0 to CompResult.Differences.Count - 1 do
        WriteLn('  - ', CompResult.Differences[I]);
    finally
      CompResult.Differences.Free;
    end;
  finally
    Verify.Free;
  end;
end;
```

### Verifying specific features

```pascal
// Check if BLOBs exist in a column
if Verify.VerifyBlobsExist('users', 'avatar') then
  WriteLn('BLOBs preserved');

// Check if all views are queryable
if Verify.VerifyViewsWork then
  WriteLn('Views working');

// Compare data in a specific table
if Verify.CompareTableData(OtherConn, 'users') then
  WriteLn('Table data matches');
```

## TNDXSchemaCount record

```pascal
TNDXSchemaCount = record
  TableCount: Integer;
  IndexCount: Integer;
  ViewCount: Integer;
  TriggerCount: Integer;
end;
```

## TNDXCompareResult record

```pascal
TNDXCompareResult = record
  Success: Boolean;
  SchemasMatch: Boolean;
  DataMatches: Boolean;
  SourceSchema: TNDXSchemaCount;
  TargetSchema: TNDXSchemaCount;
  Differences: TStringList;  // Must be freed!
  ErrorMessage: string;
end;
```

## Use cases

- **After backup/restore**: Verify restored database matches original
- **After migration**: Ensure schema changes applied correctly
- **Testing**: Validate database state in automated tests
- **Deployment**: Check production database schema version

## Building

```bash
lazbuild SchemaVerification.lpi
```

## Running

```bash
./SchemaVerification      # Linux/macOS
SchemaVerification.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
