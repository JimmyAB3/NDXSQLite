{===============================================================================
  NDXSQLite GUI Example - Transaction Management
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates explicit transaction control including
  BEGIN, COMMIT, ROLLBACK, and SAVEPOINT operations with visual feedback.
===============================================================================}
program TransactionsDemo;

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
  Application.Title := 'NDXSQLite Transactions Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
