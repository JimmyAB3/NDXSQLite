{===============================================================================
  NDXSQLite GUI Example - DateTime Utilities
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - TNDXDateTimeHelper class for date/time conversions
  - ISO 8601 UTC string formatting and parsing
  - Unix timestamp conversions
  - Local/UTC time conversions
  - Format validation
  - Real-time clock display
  - Cross-platform GUI (Windows, Linux, macOS)

  This example provides an interactive interface for exploring date/time
  utilities essential for SQLite applications. All date/time operations
  use the TNDXDateTimeHelper static class which provides:
  - Thread-safe operations (no shared state)
  - Timezone-aware conversions
  - Multiple ISO 8601 format parsing
  - Unix timestamp interoperability
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ndxsqlitedatetimeutils;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    btnConvertLocalToIso: TButton;
    btnConvertIsoToLocal: TButton;
    btnConvertToUnix: TButton;
    btnConvertFromUnix: TButton;
    btnConvertCustomToUtc: TButton;
    btnValidateFormat: TButton;
    btnParseIso: TButton;
    btnClearLog: TButton;
    edtLocalDateTime: TEdit;
    edtIsoUtc: TEdit;
    edtUnixTimestamp: TEdit;
    edtCustomFormat: TEdit;
    edtCustomDateTime: TEdit;
    edtParseInput: TEdit;
    edtValidateInput: TEdit;
    gbCurrentTime: TGroupBox;
    gbConversions: TGroupBox;
    gbCustomFormat: TGroupBox;
    gbParsing: TGroupBox;
    gbValidation: TGroupBox;
    lblCustomFormat: TLabel;
    lblCustomDateTime: TLabel;
    lblLocalNow: TLabel;
    lblUtcNow: TLabel;
    lblIsoUtcNow: TLabel;
    lblUnixNow: TLabel;
    lblTimezoneOffset: TLabel;
    lblLocalDateTime: TLabel;
    lblIsoUtc: TLabel;
    lblUnixTimestamp: TLabel;
    lblParseInput: TLabel;
    lblParseResult: TLabel;
    lblValidateInput: TLabel;
    lblValidateResult: TLabel;
    memoLog: TMemo;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    Splitter1: TSplitter;
    TimerClock: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerClockTimer(Sender: TObject);
    procedure btnConvertLocalToIsoClick(Sender: TObject);
    procedure btnConvertIsoToLocalClick(Sender: TObject);
    procedure btnConvertToUnixClick(Sender: TObject);
    procedure btnConvertFromUnixClick(Sender: TObject);
    procedure btnConvertCustomToUtcClick(Sender: TObject);
    procedure btnValidateFormatClick(Sender: TObject);
    procedure btnParseIsoClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
    { Appends a timestamped message to the log memo. }
    procedure Log(const AMessage: string);
    { Updates the real-time clock display showing current times. }
    procedure UpdateClockDisplay;
    { Calculates and displays the current timezone offset. }
    procedure DisplayTimezoneOffset;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Initialize with current local time in the input field
  edtLocalDateTime.Text := TNDXDateTimeHelper.NowLocalToStr;

  // Initialize ISO UTC field with current time
  edtIsoUtc.Text := TNDXDateTimeHelper.NowToIsoUTC;

  // Initialize Unix timestamp field
  edtUnixTimestamp.Text := IntToStr(TNDXDateTimeHelper.NowToUnixTimestamp);

  // Initialize parsing input with a sample ISO date
  edtParseInput.Text := '2024-12-25T10:30:00.000Z';

  // Initialize validation input with a sample
  edtValidateInput.Text := '2024-06-15T14:30:45.123Z';

  // Initialize custom format conversion with samples
  edtCustomFormat.Text := 'dd-mm-yyyy hh:nn:ss.zzz';
  edtCustomDateTime.Text := '25-01-2026 14:30:45.000';

  // Display initial clock values
  UpdateClockDisplay;
  DisplayTimezoneOffset;

  // Log startup message
  Log('Application started');
  Log('TNDXDateTimeHelper provides thread-safe date/time utilities');
  Log('');
  Log('Format constants available:');
  Log('  NDX_ISO8601_FORMAT       = ''' + NDX_ISO8601_FORMAT + '''');
  Log('  NDX_ISO8601_FORMAT_NO_MS = ''' + NDX_ISO8601_FORMAT_NO_MS + '''');
  Log('  NDX_ISO8601_DATE_ONLY    = ''' + NDX_ISO8601_DATE_ONLY + '''');
  Log('  NDX_ISO8601_TIME_ONLY    = ''' + NDX_ISO8601_TIME_ONLY + '''');

  // Start the clock timer
  TimerClock.Enabled := True;
end;

procedure TfrmMain.TimerClockTimer(Sender: TObject);
begin
  // Update clock display every second
  UpdateClockDisplay;
end;

procedure TfrmMain.UpdateClockDisplay;
var
  LocalNow, UtcNow: TDateTime;
begin
  // Get current times
  LocalNow := Now;
  UtcNow := TNDXDateTimeHelper.NowUTC;

  // Update labels with formatted times
  lblLocalNow.Caption := 'Local: ' + TNDXDateTimeHelper.DateTimeToStr(LocalNow);
  lblUtcNow.Caption := 'UTC:   ' + TNDXDateTimeHelper.DateTimeToStr(UtcNow);
  lblIsoUtcNow.Caption := 'ISO:   ' + TNDXDateTimeHelper.NowToIsoUTC;
  lblUnixNow.Caption := 'Unix:  ' + IntToStr(TNDXDateTimeHelper.NowToUnixTimestamp);
end;

procedure TfrmMain.DisplayTimezoneOffset;
var
  LocalNow, UtcNow: TDateTime;
  OffsetHours: Double;
begin
  // Calculate timezone offset in hours
  LocalNow := Now;
  UtcNow := TNDXDateTimeHelper.NowUTC;
  OffsetHours := (LocalNow - UtcNow) * 24;

  // Display offset with sign
  if OffsetHours >= 0 then
    lblTimezoneOffset.Caption := Format('Timezone: UTC+%.1f', [OffsetHours])
  else
    lblTimezoneOffset.Caption := Format('Timezone: UTC%.1f', [OffsetHours]);
end;

procedure TfrmMain.btnConvertLocalToIsoClick(Sender: TObject);
var
  LocalDT: TDateTime;
  IsoStr: string;
begin
  // Convert local datetime string to ISO 8601 UTC
  try
    // Parse the local datetime from the edit field (format: yyyy-mm-dd hh:nn:ss.zzz)
    LocalDT := ScanDateTime(NDX_ISO8601_FORMAT, edtLocalDateTime.Text);

    // Convert to ISO 8601 UTC string
    IsoStr := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(LocalDT);

    // Display result
    edtIsoUtc.Text := IsoStr;

    Log('Local to ISO UTC conversion:');
    Log('  Input (Local):  ' + edtLocalDateTime.Text);
    Log('  Output (ISO):   ' + IsoStr);
  except
    on E: Exception do
    begin
      Log('ERROR: Invalid local datetime format - ' + E.Message);
      ShowMessage('Invalid datetime format. Use: ' + NDX_ISO8601_FORMAT);
    end;
  end;
end;

procedure TfrmMain.btnConvertIsoToLocalClick(Sender: TObject);
var
  LocalDT: TDateTime;
  Success: Boolean;
begin
  // Convert ISO 8601 UTC string to local datetime
  Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime(edtIsoUtc.Text, LocalDT);

  if Success then
  begin
    // Display result
    edtLocalDateTime.Text := TNDXDateTimeHelper.DateTimeToStr(LocalDT);

    Log('ISO UTC to Local conversion:');
    Log('  Input (ISO):    ' + edtIsoUtc.Text);
    Log('  Output (Local): ' + edtLocalDateTime.Text);
  end
  else
  begin
    Log('ERROR: Invalid ISO 8601 format: ' + edtIsoUtc.Text);
    ShowMessage('Invalid ISO 8601 format. Examples:' + LineEnding +
      '  2024-06-15T14:30:45.123Z' + LineEnding +
      '  2024-06-15T14:30:45Z' + LineEnding +
      '  2024-06-15 14:30:45');
  end;
end;

procedure TfrmMain.btnConvertToUnixClick(Sender: TObject);
var
  UnixTS: Int64;
begin
  // Convert ISO 8601 UTC to Unix timestamp
  try
    UnixTS := TNDXDateTimeHelper.IsoUTCToUnixTimestamp(edtIsoUtc.Text);
    edtUnixTimestamp.Text := IntToStr(UnixTS);

    Log('ISO UTC to Unix timestamp:');
    Log('  Input (ISO):  ' + edtIsoUtc.Text);
    Log('  Output (Unix): ' + IntToStr(UnixTS));
  except
    on E: Exception do
    begin
      Log('ERROR: Cannot convert to Unix timestamp - ' + E.Message);
      ShowMessage('Invalid ISO 8601 format for Unix conversion.');
    end;
  end;
end;

procedure TfrmMain.btnConvertFromUnixClick(Sender: TObject);
var
  UnixTS: Int64;
  IsoStr: string;
  LocalDT: TDateTime;
begin
  // Convert Unix timestamp to ISO 8601 UTC and local datetime
  try
    UnixTS := StrToInt64(edtUnixTimestamp.Text);

    // Convert to ISO 8601 UTC
    IsoStr := TNDXDateTimeHelper.UnixTimestampToIsoUTC(UnixTS);
    edtIsoUtc.Text := IsoStr;

    // Also convert to local datetime
    LocalDT := TNDXDateTimeHelper.IsoUTCToLocalDateTime(IsoStr);
    edtLocalDateTime.Text := TNDXDateTimeHelper.DateTimeToStr(LocalDT);

    Log('Unix timestamp conversion:');
    Log('  Input (Unix):   ' + IntToStr(UnixTS));
    Log('  Output (ISO):   ' + IsoStr);
    Log('  Output (Local): ' + edtLocalDateTime.Text);
  except
    on E: Exception do
    begin
      Log('ERROR: Invalid Unix timestamp - ' + E.Message);
      ShowMessage('Invalid Unix timestamp. Must be an integer.');
    end;
  end;
end;

procedure TfrmMain.btnConvertCustomToUtcClick(Sender: TObject);
var
  CustomFormat: string;
  InputValue: string;
  LocalDT: TDateTime;
  StandardStr: string;
  IsoUtcStr: string;
begin
  // Convert a datetime in custom format to ISO 8601 UTC
  // Step 1: Parse the input using the user-specified format
  // Step 2: Format to standard format (yyyy-mm-dd hh:nn:ss.zzz)
  // Step 3: Convert to UTC
  try
    CustomFormat := edtCustomFormat.Text;
    InputValue := edtCustomDateTime.Text;

    if (CustomFormat = '') or (InputValue = '') then
    begin
      ShowMessage('Please enter both the format and the datetime value.');
      Exit;
    end;

    // Step 1: Parse using custom format
    LocalDT := ScanDateTime(CustomFormat, InputValue);

    // Step 2: Format to standard format
    StandardStr := TNDXDateTimeHelper.DateTimeToStr(LocalDT);

    // Step 3: Convert to ISO 8601 UTC
    IsoUtcStr := TNDXDateTimeHelper.LocalDateTimeToIsoUTC(LocalDT);

    // Update the other fields with the results
    edtLocalDateTime.Text := StandardStr;
    edtIsoUtc.Text := IsoUtcStr;

    Log('Custom format conversion:');
    Log('  Format:         ' + CustomFormat);
    Log('  Input:          ' + InputValue);
    Log('  Standard:       ' + StandardStr);
    Log('  ISO UTC:        ' + IsoUtcStr);
  except
    on E: Exception do
    begin
      Log('ERROR: Custom format conversion failed - ' + E.Message);
      ShowMessage('Conversion failed. Check that your format matches the input.' + LineEnding +
        'Format: ' + edtCustomFormat.Text + LineEnding +
        'Input: ' + edtCustomDateTime.Text + LineEnding +
        'Error: ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.btnValidateFormatClick(Sender: TObject);
var
  IsValid: Boolean;
begin
  // Validate if string is in ISO 8601 format
  IsValid := TNDXDateTimeHelper.IsIso8601Format(edtValidateInput.Text);

  if IsValid then
  begin
    lblValidateResult.Caption := 'Valid ISO 8601 format';
    lblValidateResult.Font.Color := clGreen;
    Log('Format validation: "' + edtValidateInput.Text + '" is VALID ISO 8601');
  end
  else
  begin
    lblValidateResult.Caption := 'NOT a valid ISO 8601 format';
    lblValidateResult.Font.Color := clRed;
    Log('Format validation: "' + edtValidateInput.Text + '" is NOT valid ISO 8601');
  end;
end;

procedure TfrmMain.btnParseIsoClick(Sender: TObject);
var
  DT: TDateTime;
  IsUTC: Boolean;
  Success: Boolean;
begin
  // Parse ISO 8601 string and show details
  try
    DT := TNDXDateTimeHelper.ParseIso8601(edtParseInput.Text, IsUTC);

    lblParseResult.Caption := Format('Parsed: %s (UTC: %s)',
      [TNDXDateTimeHelper.DateTimeToStr(DT), BoolToStr(IsUTC, 'Yes', 'No')]);
    lblParseResult.Font.Color := clGreen;

    Log('Parse ISO 8601:');
    Log('  Input:     ' + edtParseInput.Text);
    Log('  Parsed:    ' + TNDXDateTimeHelper.DateTimeToStr(DT));
    Log('  Is UTC:    ' + BoolToStr(IsUTC, 'Yes', 'No'));

    // Also try safe parsing
    Success := TNDXDateTimeHelper.TryIsoUTCToLocalDateTime(edtParseInput.Text, DT);
    if Success then
      Log('  As Local:  ' + TNDXDateTimeHelper.DateTimeToStr(DT));
  except
    on E: Exception do
    begin
      lblParseResult.Caption := 'Parse failed: ' + E.Message;
      lblParseResult.Font.Color := clRed;
      Log('ERROR: Parse failed - ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  Log('Log cleared');
end;

procedure TfrmMain.Log(const AMessage: string);
begin
  // Append timestamped message to log
  if AMessage = '' then
    memoLog.Lines.Add('')
  else
    memoLog.Lines.Add(Format('[%s] %s',
      [FormatDateTime('hh:nn:ss', Now), AMessage]));

  // Auto-scroll to show latest entry
  memoLog.SelStart := Length(memoLog.Text);
end;

end.
