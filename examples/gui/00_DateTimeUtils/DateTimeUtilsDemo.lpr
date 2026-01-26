{===============================================================================
  NDXSQLite GUI Example - DateTime Utilities
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example provides an interactive interface for exploring the
  TNDXDateTimeHelper class, which handles date/time conversions for
  SQLite applications including ISO 8601 UTC and Unix timestamp formats.
===============================================================================}
program DateTimeUtilsDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, MainForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'NDXSQLite DateTime Utilities Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
