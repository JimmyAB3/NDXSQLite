{===============================================================================
  NDXSQLite GUI Example - Cached Updates
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates CachedUpdates mode where all modifications
  (insert, update, delete) are held in memory until explicitly applied
  or canceled. This provides transaction-like behavior at the dataset level.
===============================================================================}
program CachedUpdatesDemo;

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
  Application.Title := 'NDXSQLite Cached Updates Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
