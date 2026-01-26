# Example 51: Concurrent Queries

This example demonstrates the ability to execute multiple database operations simultaneously on a single connection with NDXSQLite.

## Features Demonstrated

- **Multiple ExecuteNonQuery**: Sequential INSERT, UPDATE, DELETE operations
- **Multiple ExecuteScalar**: Multiple aggregate queries
- **Multiple ExecuteQuery**: Multiple SELECT operations returning datasets
- **Multiple Open Datasets**: Several datasets open simultaneously
- **ExecuteNonQuery with Open Dataset**: Modifications while datasets are active
- **Mixed Operations**: Real-world scenario combining all operation types
- **Stress Test**: 5 open datasets with concurrent modifications

## Key Capabilities

### Multiple INSERT Operations
```pascal
Rows1 := Connection.ExecuteNonQuery('INSERT INTO products ...', [Params1]);
Rows2 := Connection.ExecuteNonQuery('INSERT INTO products ...', [Params2]);
Rows3 := Connection.ExecuteNonQuery('INSERT INTO products ...', [Params3]);
```

### Multiple Scalar Queries
```pascal
TotalProducts := Connection.ExecuteScalar('SELECT COUNT(*) FROM products');
TotalStock := Connection.ExecuteScalar('SELECT SUM(stock) FROM products');
AvgPrice := Connection.ExecuteScalar('SELECT AVG(price) FROM products');
```

### Multiple Open Datasets
```pascal
DS1 := Connection.ExecuteQuery('SELECT * FROM products WHERE category = ?', ['Electronics']);
DS2 := Connection.ExecuteQuery('SELECT * FROM products WHERE category = ?', ['Furniture']);
DS3 := Connection.ExecuteQuery('SELECT * FROM products WHERE price > ?', [100]);

try
  // All three datasets can be read simultaneously
  WriteLn(DS1.FieldByName('name').AsString);
  WriteLn(DS2.FieldByName('name').AsString);
  WriteLn(DS3.FieldByName('name').AsString);
finally
  DS1.Free;
  DS2.Free;
  DS3.Free;
end;
```

### ExecuteNonQuery with Open Dataset
```pascal
DS := Connection.ExecuteQuery('SELECT * FROM products WHERE name = ?', ['Laptop']);
try
  // Dataset remains valid after ExecuteNonQuery
  Connection.ExecuteNonQuery('UPDATE products SET stock = stock + 10 WHERE name = ?', ['Mouse']);
  Connection.ExecuteNonQuery('INSERT INTO products ...', [NewProductParams]);

  // Original dataset still accessible
  WriteLn(DS.FieldByName('name').AsString);  // Still works!
finally
  DS.Free;
end;
```

## Real-World Scenario

```pascal
// Browse products while processing sales
DSProducts := Connection.ExecuteQuery('SELECT * FROM products WHERE stock > 0');
try
  while not DSProducts.EOF do
  begin
    // Record sale while iterating (ExecuteNonQuery with open dataset)
    if DSProducts.FieldByName('stock').AsInteger > 100 then
    begin
      Connection.ExecuteNonQuery('INSERT INTO sales ...', [SaleParams]);
      Connection.ExecuteNonQuery('UPDATE products SET stock = stock - 5 ...', [ProductId]);
    end;
    DSProducts.Next;
  end;
finally
  DSProducts.Free;
end;
```

## Stress Test

The example demonstrates 5 datasets open simultaneously while performing multiple operations:
- DS1, DS2, DS3: Different product queries
- DS4: Electronics category
- DS5: Sales records

All remain valid while INSERT and SELECT operations are performed.

## Build and Run

```bash
cd 51_ConcurrentQueries
fpc ConcurrentQueries.lpr
./ConcurrentQueries
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Technical Notes

This showcases the fix that allows ExecuteNonQuery to run without invalidating open datasets from ExecuteQuery, enabling more flexible database interaction patterns.
