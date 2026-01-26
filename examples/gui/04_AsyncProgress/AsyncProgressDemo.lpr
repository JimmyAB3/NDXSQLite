{===============================================================================
  NDXSQLite GUI Example - Asynchronous Operations with Progress
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates how to use TNDXSQLiteAsyncConnection for
  non-blocking database operations with progress reporting and cancellation
  support, keeping the UI responsive during long operations.
===============================================================================}
program AsyncProgressDemo;

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
  Application.Title := 'NDXSQLite Async Progress Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
