{===============================================================================
  NDXSQLite GUI Example - Search, Filter, and Sort
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates various techniques for searching, filtering,
  and sorting data including Locate, Filter property, and dynamic ORDER BY.
===============================================================================}
program SearchFilterDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // LCL widgetset
  Forms, MainForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'NDXSQLite Search Filter Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
