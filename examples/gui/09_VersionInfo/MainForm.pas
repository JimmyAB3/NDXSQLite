{===============================================================================
  NDXSQLite GUI Example - Version Information
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - NDXSQLite library version information
  - SQLite runtime version detection
  - Feature compatibility checking
  - Cross-platform version display
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Grids,
  ndxsqliteversion, ndxsqliteplatform;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    btnRefresh: TButton;
    btnCheckFeature: TButton;
    btnClearLog: TButton;
    cboFeature: TComboBox;
    gbLibraryVersion: TGroupBox;
    gbSQLiteVersion: TGroupBox;
    gbFeatureCheck: TGroupBox;
    lblVersion: TLabel;
    lblVersionFull: TLabel;
    lblMajor: TLabel;
    lblMinor: TLabel;
    lblPatch: TLabel;
    lblDate: TLabel;
    lblBuiltFor: TLabel;
    lblRuntime: TLabel;
    lblRuntimeNumber: TLabel;
    lblLoaded: TLabel;
    lblPlatform: TLabel;
    lblFeatureResult: TLabel;
    memoLog: TMemo;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    Splitter1: TSplitter;
    sgFeatures: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnCheckFeatureClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
    procedure Log(const AMessage: string);
    procedure LoadVersionInfo;
    procedure LoadFeatureGrid;
    procedure CheckAllFeatures;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

const
  { SQLite version requirements for various features }
  FEATURES: array[0..9] of record
    Name: string;
    MinVersion: Integer;
    Description: string;
  end = (
    (Name: 'FTS3/FTS4'; MinVersion: 3007000; Description: 'Full-Text Search 3/4'),
    (Name: 'FTS5'; MinVersion: 3009000; Description: 'Full-Text Search 5'),
    (Name: 'JSON1'; MinVersion: 3009000; Description: 'JSON1 Extension'),
    (Name: 'Common Table Expressions'; MinVersion: 3008003; Description: 'WITH clause'),
    (Name: 'UPSERT'; MinVersion: 3024000; Description: 'ON CONFLICT clause'),
    (Name: 'Window Functions'; MinVersion: 3025000; Description: 'OVER clause'),
    (Name: 'RETURNING'; MinVersion: 3035000; Description: 'RETURNING clause'),
    (Name: 'Math Functions'; MinVersion: 3035000; Description: 'Built-in math'),
    (Name: 'STRICT Tables'; MinVersion: 3037000; Description: 'Type enforcement'),
    (Name: 'JSON Functions'; MinVersion: 3038000; Description: 'Native JSON support')
  );

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Initialize SQLite library
  TNDXPlatform.LoadSQLiteLibrary;

  // Setup feature combobox
  cboFeature.Items.Clear;
  cboFeature.Items.Add('FTS5 (3.9.0)');
  cboFeature.Items.Add('RETURNING clause (3.35.0)');
  cboFeature.Items.Add('Math functions (3.35.0)');
  cboFeature.Items.Add('STRICT tables (3.37.0)');
  cboFeature.Items.Add('JSON functions (3.38.0)');
  cboFeature.ItemIndex := 0;

  // Setup feature grid
  sgFeatures.RowCount := Length(FEATURES) + 1;
  sgFeatures.ColCount := 4;
  sgFeatures.Cells[0, 0] := 'Feature';
  sgFeatures.Cells[1, 0] := 'Min Version';
  sgFeatures.Cells[2, 0] := 'Status';
  sgFeatures.Cells[3, 0] := 'Description';
  sgFeatures.ColWidths[0] := 180;
  sgFeatures.ColWidths[1] := 100;
  sgFeatures.ColWidths[2] := 80;
  sgFeatures.ColWidths[3] := 200;

  // Load version information
  LoadVersionInfo;
  LoadFeatureGrid;

  // Log startup
  Log('Application started');
  Log('NDXSQLite Version Information Demo');
  Log('');
  CheckAllFeatures;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  LoadVersionInfo;
  LoadFeatureGrid;
  Log('Version information refreshed');
end;

procedure TfrmMain.btnCheckFeatureClick(Sender: TObject);
var
  MinVersion: Integer;
  Supported: Boolean;
  FeatureName: string;
begin
  case cboFeature.ItemIndex of
    0: MinVersion := 3009000;  // FTS5
    1: MinVersion := 3035000;  // RETURNING
    2: MinVersion := 3035000;  // Math
    3: MinVersion := 3037000;  // STRICT
    4: MinVersion := 3038000;  // JSON
  else
    MinVersion := 0;
  end;

  FeatureName := cboFeature.Text;
  Supported := CheckSQLiteMinVersion(MinVersion);

  if Supported then
  begin
    lblFeatureResult.Caption := 'SUPPORTED';
    lblFeatureResult.Font.Color := clGreen;
    Log('Feature check: ' + FeatureName + ' is SUPPORTED');
  end
  else
  begin
    lblFeatureResult.Caption := 'NOT AVAILABLE';
    lblFeatureResult.Font.Color := clRed;
    Log('Feature check: ' + FeatureName + ' is NOT AVAILABLE');
  end;
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  Log('Log cleared');
end;

procedure TfrmMain.Log(const AMessage: string);
begin
  if AMessage = '' then
    memoLog.Lines.Add('')
  else
    memoLog.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), AMessage]));
  memoLog.SelStart := Length(memoLog.Text);
end;

procedure TfrmMain.LoadVersionInfo;
begin
  // NDXSQLite Library Version
  lblVersion.Caption := 'Version: ' + GetLibraryVersion;
  lblVersionFull.Caption := 'Full: ' + GetLibraryVersionFull;
  lblMajor.Caption := 'Major: ' + IntToStr(GetLibraryVersionMajor);
  lblMinor.Caption := 'Minor: ' + IntToStr(GetLibraryVersionMinor);
  lblPatch.Caption := 'Patch: ' + IntToStr(GetLibraryVersionPatch);
  lblDate.Caption := 'Date: ' + GetLibraryVersionDate;

  // SQLite Version
  lblBuiltFor.Caption := 'Built for: ' + GetBuiltForSQLiteVersion;
  lblRuntime.Caption := 'Runtime: ' + GetRuntimeSQLiteVersion;
  lblRuntimeNumber.Caption := 'Version Number: ' + IntToStr(GetRuntimeSQLiteVersionNumber);

  // Platform info
  if TNDXPlatform.IsSQLiteLoaded then
    lblLoaded.Caption := 'SQLite Loaded: Yes'
  else
    lblLoaded.Caption := 'SQLite Loaded: No';

  {$IFDEF WINDOWS}
  lblPlatform.Caption := 'Platform: Windows';
  {$ENDIF}
  {$IFDEF LINUX}
  lblPlatform.Caption := 'Platform: Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
  lblPlatform.Caption := 'Platform: macOS';
  {$ENDIF}
end;

procedure TfrmMain.LoadFeatureGrid;
var
  I: Integer;
  VersionStr: string;
  Supported: Boolean;
begin
  for I := 0 to High(FEATURES) do
  begin
    sgFeatures.Cells[0, I + 1] := FEATURES[I].Name;

    VersionStr := Format('%d.%d.%d', [
      FEATURES[I].MinVersion div 1000000,
      (FEATURES[I].MinVersion div 1000) mod 1000,
      FEATURES[I].MinVersion mod 1000
    ]);
    sgFeatures.Cells[1, I + 1] := VersionStr;

    Supported := CheckSQLiteMinVersion(FEATURES[I].MinVersion);
    if Supported then
      sgFeatures.Cells[2, I + 1] := 'OK'
    else
      sgFeatures.Cells[2, I + 1] := '--';

    sgFeatures.Cells[3, I + 1] := FEATURES[I].Description;
  end;
end;

procedure TfrmMain.CheckAllFeatures;
var
  I: Integer;
  Supported: Boolean;
  VersionStr: string;
begin
  Log('SQLite Feature Support:');
  Log('  Runtime version: ' + GetRuntimeSQLiteVersion);
  Log('');

  for I := 0 to High(FEATURES) do
  begin
    Supported := CheckSQLiteMinVersion(FEATURES[I].MinVersion);
    VersionStr := Format('%d.%d.%d', [
      FEATURES[I].MinVersion div 1000000,
      (FEATURES[I].MinVersion div 1000) mod 1000,
      FEATURES[I].MinVersion mod 1000
    ]);

    if Supported then
      Log(Format('  [OK] %-25s (>= %s)', [FEATURES[I].Name, VersionStr]))
    else
      Log(Format('  [--] %-25s (>= %s)', [FEATURES[I].Name, VersionStr]));
  end;
end;

end.
