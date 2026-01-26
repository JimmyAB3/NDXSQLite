{===============================================================================
  NDXSQLite GUI Example - DBGrid with Navigator
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates how to use TNDXSQLiteDataSet with standard
  Lazarus data-aware controls (DBGrid, DBNavigator) for a complete
  CRUD (Create, Read, Update, Delete) interface.
===============================================================================}
program DBGridDemo;

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
  Application.Title := 'NDXSQLite DBGrid Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
