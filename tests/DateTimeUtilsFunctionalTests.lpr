{===============================================================================
  NDXSQLite - DateTime Utilities Functional Test Runner
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Console test runner for TNDXDateTimeHelper functional tests with SQLite.
  Tests real-world integration scenarios.

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DateTimeUtilsFunctionalTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner,
  ndxsqlitedatetimeutils_functional_tests;

type
  { Console test runner application. }
  TFunctionalTestRunner = class(TTestRunner)
  protected
    procedure WriteCustomHelp; override;
  end;

procedure TFunctionalTestRunner.WriteCustomHelp;
begin
  WriteLn('NDXSQLite DateTime Utilities Functional Test Suite');
  WriteLn('--------------------------------------------------');
  WriteLn('Tests TNDXDateTimeHelper integration with SQLite:');
  WriteLn('  - Storing and retrieving ISO 8601 dates');
  WriteLn('  - Chronological sorting of TEXT dates');
  WriteLn('  - Round-trip conversions through database');
  WriteLn('  - Unix timestamp storage');
  WriteLn('  - Date range queries');
  WriteLn('  - Edge cases (epoch, leap year, year boundary)');
  WriteLn;
end;

var
  Runner: TFunctionalTestRunner;

begin
  Runner := TFunctionalTestRunner.Create(nil);
  try
    Runner.Initialize;
    Runner.Title := 'NDXSQLite DateTime Utilities Functional Tests';
    Runner.Run;
  finally
    Runner.Free;
  end;
end.
