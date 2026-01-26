{===============================================================================
  NDXSQLite - DateTime Utilities Test Runner
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Console test runner for TNDXDateTimeHelper unit tests using FPCUnit.

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program DateTimeUtilsTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner,
  ndxsqlitedatetimeutils_tests;

type
  { Console test runner application. }
  TDateTimeTestRunner = class(TTestRunner)
  protected
    procedure WriteCustomHelp; override;
  end;

procedure TDateTimeTestRunner.WriteCustomHelp;
begin
  WriteLn('NDXSQLite DateTime Utilities Test Suite');
  WriteLn('---------------------------------------');
  WriteLn('Tests TNDXDateTimeHelper class functionality:');
  WriteLn('  - ISO 8601 UTC conversions');
  WriteLn('  - Unix timestamp conversions');
  WriteLn('  - Parsing various ISO 8601 formats');
  WriteLn('  - UTC conversion helpers');
  WriteLn('  - Formatting functions');
  WriteLn;
end;

var
  Runner: TDateTimeTestRunner;

begin
  Runner := TDateTimeTestRunner.Create(nil);
  try
    Runner.Initialize;
    Runner.Title := 'NDXSQLite DateTime Utilities Tests';
    Runner.Run;
  finally
    Runner.Free;
  end;
end.
