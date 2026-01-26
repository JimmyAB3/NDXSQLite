{===============================================================================
  NDXSQLite GUI Example - Version Information
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example provides an interactive interface for exploring version
  information of the NDXSQLite library and the SQLite runtime. It demonstrates
  how to check library versions and verify SQLite feature support.
===============================================================================}
program VersionInfoDemo;

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
  Application.Title := 'NDXSQLite Version Information Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
