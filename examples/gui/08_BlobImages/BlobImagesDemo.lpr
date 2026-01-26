{===============================================================================
  NDXSQLite GUI Example - BLOB and Image Management
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates storing and retrieving binary data (BLOBs)
  in SQLite, including images and other file types.
===============================================================================}
program BlobImagesDemo;

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
  Application.Title := 'NDXSQLite BLOB Images Demo';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
