{===============================================================================
  NDXSQLite GUI Example - Multi-Thread with Connection Pool
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates safe multi-threaded SQLite access using
  a connection pool. Each worker thread acquires its own connection,
  performs operations, and releases the connection when done.
===============================================================================}
program MultiThreadDemo;

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
  Application.Title := 'NDXSQLite MultiThread Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
