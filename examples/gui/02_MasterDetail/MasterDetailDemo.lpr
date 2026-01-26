{===============================================================================
  NDXSQLite GUI Example - Master/Detail Relationship
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates a master/detail relationship between two datasets:
  - Master: Categories table
  - Detail: Products table (filtered by selected category)
===============================================================================}
program MasterDetailDemo;

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
  Application.Title := 'NDXSQLite Master/Detail Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
