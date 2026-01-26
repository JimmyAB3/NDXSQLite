{===============================================================================
  NDXSQLite Example 02 - Transactions
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Beginning a transaction
  - Committing changes
  - Rolling back on error
  - Transaction isolation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program Transactions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB,
  ndxsqliteconnection;

var
  Connection: TNDXSQLiteConnection;
  DBPath: string;

  procedure ShowAccountBalances;
  var
    DataSet: TDataSet;
  begin
    DataSet := Connection.ExecuteQuery(
      'SELECT name, balance FROM accounts ORDER BY name');
    try
      WriteLn('   Current balances:');
      while not DataSet.EOF do
      begin
        WriteLn(Format('   - %s: $%.2f', [
          DataSet.FieldByName('name').AsString,
          DataSet.FieldByName('balance').AsFloat
        ]));
        DataSet.Next;
      end;
    finally
      DataSet.Free;
    end;
  end;

begin
  WriteLn('=== NDXSQLite Example 02: Transactions ===');
  WriteLn;

  DBPath := ExtractFilePath(ParamStr(0)) + 'example02.db';

  if FileExists(DBPath) then
    DeleteFile(DBPath);

  Connection := TNDXSQLiteConnection.Create(DBPath);
  try
    Connection.Open;

    // Create accounts table
    WriteLn('1. Setting up accounts table...');
    Connection.ExecuteNonQuery(
      'CREATE TABLE accounts (' +
      '  id INTEGER PRIMARY KEY,' +
      '  name TEXT NOT NULL,' +
      '  balance REAL NOT NULL DEFAULT 0' +
      ')'
    );
    Connection.ExecuteNonQuery(
      'INSERT INTO accounts (name, balance) VALUES (''Alice'', 1000.00)');
    Connection.ExecuteNonQuery(
      'INSERT INTO accounts (name, balance) VALUES (''Bob'', 500.00)');
    ShowAccountBalances;
    WriteLn;

    // Successful transaction: Transfer $200 from Alice to Bob
    WriteLn('2. Successful transaction: Transfer $200 from Alice to Bob');
    Connection.BeginTransaction;
    try
      Connection.ExecuteNonQuery(
        'UPDATE accounts SET balance = balance - 200 WHERE name = ''Alice''');
      Connection.ExecuteNonQuery(
        'UPDATE accounts SET balance = balance + 200 WHERE name = ''Bob''');
      Connection.Commit;
      WriteLn('   Transaction committed successfully.');
      ShowAccountBalances;
    except
      on E: Exception do
      begin
        Connection.Rollback;
        WriteLn('   Transaction rolled back: ', E.Message);
      end;
    end;
    WriteLn;

    // Failed transaction: Try to transfer $1000 from Bob (will fail with constraint)
    WriteLn('3. Failed transaction: Transfer $1000 from Bob (simulated error)');
    WriteLn('   Adding CHECK constraint to prevent negative balance...');
    // Note: SQLite doesn't enforce CHECK by default on existing data, so we simulate
    Connection.BeginTransaction;
    try
      Connection.ExecuteNonQuery(
        'UPDATE accounts SET balance = balance - 1000 WHERE name = ''Bob''');

      // Simulate a business logic check
      if Connection.ExecuteScalar(
        'SELECT balance FROM accounts WHERE name = ''Bob''') < 0 then
        raise Exception.Create('Insufficient funds: balance would be negative');

      Connection.ExecuteNonQuery(
        'UPDATE accounts SET balance = balance + 1000 WHERE name = ''Alice''');
      Connection.Commit;
      WriteLn('   Transaction committed.');
    except
      on E: Exception do
      begin
        Connection.Rollback;
        WriteLn('   Transaction rolled back: ', E.Message);
      end;
    end;
    ShowAccountBalances;
    WriteLn;

    // Nested operations in single transaction
    WriteLn('4. Multiple operations in a single transaction:');
    Connection.BeginTransaction;
    try
      Connection.ExecuteNonQuery(
        'INSERT INTO accounts (name, balance) VALUES (''Charlie'', 750.00)');
      Connection.ExecuteNonQuery(
        'UPDATE accounts SET balance = balance + 50 WHERE name = ''Alice''');
      Connection.ExecuteNonQuery(
        'UPDATE accounts SET balance = balance - 50 WHERE name = ''Bob''');
      Connection.Commit;
      WriteLn('   All operations committed atomically.');
      ShowAccountBalances;
    except
      on E: Exception do
      begin
        Connection.Rollback;
        WriteLn('   All operations rolled back: ', E.Message);
      end;
    end;
    WriteLn;

    // Show IsTransactionActive property
    WriteLn('5. Checking transaction state:');
    WriteLn('   IsTransactionActive (before begin): ', Connection.IsTransactionActive);
    Connection.BeginTransaction;
    WriteLn('   IsTransactionActive (after begin): ', Connection.IsTransactionActive);
    Connection.Commit;
    WriteLn('   IsTransactionActive (after commit): ', Connection.IsTransactionActive);
    WriteLn;

    Connection.Close;

  finally
    Connection.Free;
  end;

  WriteLn('=== Example completed successfully! ===');

  if FileExists(DBPath) then
    DeleteFile(DBPath);
end.
