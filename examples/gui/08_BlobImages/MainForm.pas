{===============================================================================
  NDXSQLite GUI Example - BLOB and Image Management
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - Storing binary data (BLOBs) in SQLite
  - Loading images from files into database
  - Displaying images from BLOB fields
  - Exporting BLOBs back to files
  - Handling various file types (images, documents, etc.)
  - TDBImage control for automatic image display

  This example shows how to work with binary large objects (BLOBs) in SQLite.
  BLOBs are commonly used for storing images, documents, audio files, and
  other binary content directly in the database.

  Key Concepts:
  - BLOB fields can store arbitrary binary data up to 2GB
  - TBlobField provides stream-based access to BLOB data
  - TDBImage automatically displays image BLOBs
  - Consider file size and performance when storing large BLOBs
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, DBCtrls, DB, ExtDlgs,
  ndxsqlitedataset;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    btnLoadImage: TButton;
    btnSaveImage: TButton;
    btnClearImage: TButton;
    btnLoadFile: TButton;
    btnSaveFile: TButton;
    btnRefresh: TButton;
    btnClearLog: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBImage1: TDBImage;
    DBNavigator1: TDBNavigator;
    edtTitle: TEdit;
    edtDescription: TEdit;
    gbImagePreview: TGroupBox;
    gbFileInfo: TGroupBox;
    gbControls: TGroupBox;
    lblTitle: TLabel;
    lblDescription: TLabel;
    lblFileType: TLabel;
    lblFileSize: TLabel;
    lblFileName: TLabel;
    memoLog: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlStatus: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure btnClearImageClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    FDataSet: TNDXSQLiteDataSet;
    FInitialized: Boolean;
    { Appends a timestamped message to the activity log. }
    procedure Log(const AMessage: string);
    { Initializes the database and creates sample entries. }
    procedure InitializeDatabase;
    { Updates the file info display for the current record. }
    procedure UpdateFileInfo;
    { Updates the status bar. }
    procedure UpdateStatusBar;
    { Loads a file into the BLOB field of the current record. }
    procedure LoadFileToBlob(const AFilePath: string);
    { Saves the BLOB field to a file. }
    procedure SaveBlobToFile(const AFilePath: string);
    { Returns a human-readable file size string. }
    function FormatFileSize(ASize: Int64): string;
    { Detects the file type from the BLOB header bytes. }
    function DetectFileType(AStream: TStream): string;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Just create the dataset - initialization will happen in FormShow
  FDataSet := TNDXSQLiteDataSet.Create(Self);
  FInitialized := False;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  // Prevent duplicate initialization
  if FInitialized then Exit;
  FInitialized := True;

  // Connect DataSource to dataset
  DataSource1.DataSet := FDataSet;

  // Initialize database (opens the dataset)
  InitializeDatabase;

  // Configure TDBImage AFTER dataset is open
  DBImage1.DataSource := DataSource1;
  DBImage1.DataField := 'image_data';

  // Welcome message
  Log('BLOB and Image Management Demo');
  Log('');
  Log('This example demonstrates storing binary data in SQLite:');
  Log('  - Images (PNG, JPEG, BMP, GIF)');
  Log('  - Documents and other files');
  Log('');
  Log('The TDBImage control automatically displays image BLOBs.');
  Log('Use the buttons to load/save files from the database.');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FDataSet) then
  begin
    FDataSet.Close;
    FDataSet.CloseDatabase;
  end;
end;

procedure TfrmMain.InitializeDatabase;
var
  DBPath: string;
begin
  DBPath := ExtractFilePath(ParamStr(0)) + 'blobimages_demo.db';

  FDataSet.OpenDatabase(DBPath);

  // Create table for storing images and files
  // BLOB type can store binary data up to 2GB
  FDataSet.SQL.Text := 'CREATE TABLE IF NOT EXISTS media_files (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'title TEXT NOT NULL, ' +
    'description TEXT, ' +
    'file_name TEXT, ' +
    'file_type TEXT, ' +
    'file_size INTEGER, ' +
    'image_data BLOB, ' +           // Main BLOB field for binary data
    'created_at TEXT DEFAULT (datetime(''now'', ''localtime'')), ' +
    'modified_at TEXT' +
    ')';
  FDataSet.ExecSQL;

  // Check if sample data exists
  FDataSet.SQL.Text := 'SELECT COUNT(*) FROM media_files';
  FDataSet.Open;
  if FDataSet.Fields[0].AsInteger = 0 then
  begin
    FDataSet.Close;
    // Insert sample entries (without actual images)
    FDataSet.SQL.Text :=
      'INSERT INTO media_files (title, description) VALUES ' +
      '(''Sample Image 1'', ''Click Load Image to add an image''), ' +
      '(''Sample Image 2'', ''Supports PNG, JPEG, BMP, GIF formats''), ' +
      '(''Document Storage'', ''Can also store documents and other files'')';
    FDataSet.ExecSQL;
  end
  else
    FDataSet.Close;

  // Configure dataset for browsing and editing
  FDataSet.TableName := 'media_files';
  FDataSet.PrimaryKey := 'id';
  FDataSet.SQL.Text := 'SELECT id, title, description, file_name, file_type, ' +
    'file_size, image_data, created_at, modified_at FROM media_files ORDER BY id';
  FDataSet.Open;

  UpdateStatusBar;
  UpdateFileInfo;
  Log('Database initialized: ' + DBPath);
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  // Load an image file into the current record's BLOB field
  if not FDataSet.Active then
  begin
    Log('ERROR: Dataset is not active.');
    Exit;
  end;

  if FDataSet.RecordCount = 0 then
  begin
    Log('ERROR: No record to update. Add a new record first.');
    Exit;
  end;

  // Configure and show the picture dialog
  OpenPictureDialog1.Filter :=
    'All Images|*.png;*.jpg;*.jpeg;*.bmp;*.gif|' +
    'PNG Images|*.png|' +
    'JPEG Images|*.jpg;*.jpeg|' +
    'BMP Images|*.bmp|' +
    'GIF Images|*.gif';

  if OpenPictureDialog1.Execute then
    LoadFileToBlob(OpenPictureDialog1.FileName);
end;

procedure TfrmMain.btnSaveImageClick(Sender: TObject);
var
  BlobField: TBlobField;
begin
  // Save the current record's BLOB to an image file
  if not FDataSet.Active then
  begin
    Log('ERROR: Dataset is not active.');
    Exit;
  end;

  BlobField := FDataSet.FieldByName('image_data') as TBlobField;
  if BlobField.IsNull then
  begin
    Log('ERROR: No image data to save.');
    Exit;
  end;

  // Configure save dialog based on stored file type
  SavePictureDialog1.FileName := FDataSet.FieldByName('file_name').AsString;

  if SavePictureDialog1.Execute then
  begin
    SaveBlobToFile(SavePictureDialog1.FileName);
  end;
end;

procedure TfrmMain.btnClearImageClick(Sender: TObject);
begin
  // Clear the BLOB field of the current record
  if not FDataSet.Active then Exit;

  if FDataSet.RecordCount = 0 then Exit;

  FDataSet.Edit;
  FDataSet.FieldByName('image_data').Clear;
  FDataSet.FieldByName('file_name').Clear;
  FDataSet.FieldByName('file_type').Clear;
  FDataSet.FieldByName('file_size').Clear;
  FDataSet.FieldByName('modified_at').AsString :=
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  FDataSet.Post;

  Log('Image cleared from current record.');
  UpdateFileInfo;
end;

procedure TfrmMain.btnLoadFileClick(Sender: TObject);
begin
  // Load any file type into the BLOB field
  if not FDataSet.Active then
  begin
    Log('ERROR: Dataset is not active.');
    Exit;
  end;

  if FDataSet.RecordCount = 0 then
  begin
    Log('ERROR: No record to update. Add a new record first.');
    Exit;
  end;

  // Configure dialog for all file types
  OpenDialog1.Filter := 'All Files|*.*|' +
    'Images|*.png;*.jpg;*.jpeg;*.bmp;*.gif|' +
    'Documents|*.pdf;*.doc;*.docx;*.txt|' +
    'Archives|*.zip;*.rar;*.7z';

  if OpenDialog1.Execute then
    LoadFileToBlob(OpenDialog1.FileName);
end;

procedure TfrmMain.btnSaveFileClick(Sender: TObject);
var
  BlobField: TBlobField;
  FileName: string;
begin
  // Save the BLOB to any file
  if not FDataSet.Active then
  begin
    Log('ERROR: Dataset is not active.');
    Exit;
  end;

  BlobField := FDataSet.FieldByName('image_data') as TBlobField;
  if BlobField.IsNull then
  begin
    Log('ERROR: No file data to save.');
    Exit;
  end;

  // Use stored filename as default
  FileName := FDataSet.FieldByName('file_name').AsString;
  if FileName = '' then
    FileName := 'exported_file';

  SaveDialog1.FileName := FileName;

  if SaveDialog1.Execute then
    SaveBlobToFile(SaveDialog1.FileName);
end;

procedure TfrmMain.LoadFileToBlob(const AFilePath: string);
var
  FileStream: TFileStream;
  BlobStream: TStream;
  FileName, FileExt: string;
  FileSize: Int64;
begin
  if not FileExists(AFilePath) then
  begin
    Log('ERROR: File not found: ' + AFilePath);
    Exit;
  end;

  FileName := ExtractFileName(AFilePath);
  FileExt := LowerCase(ExtractFileExt(AFilePath));

  try
    // Open the source file
    FileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
    try
      FileSize := FileStream.Size;

      // Check file size (warn for large files)
      if FileSize > 10 * 1024 * 1024 then  // 10 MB
      begin
        if MessageDlg('Large File Warning',
          Format('This file is %.2f MB. Large BLOBs may affect performance. Continue?',
            [FileSize / 1024 / 1024]),
          mtWarning, [mbYes, mbNo], 0) <> mrYes then
          Exit;
      end;

      // Put dataset in edit mode
      FDataSet.Edit;

      // Get a stream to the BLOB field and copy data
      BlobStream := FDataSet.CreateBlobStream(
        FDataSet.FieldByName('image_data'), bmWrite);
      try
        BlobStream.CopyFrom(FileStream, 0);  // 0 = copy entire stream
      finally
        BlobStream.Free;
      end;

      // Update metadata fields
      FDataSet.FieldByName('file_name').AsString := FileName;
      FDataSet.FieldByName('file_type').AsString := FileExt;
      FDataSet.FieldByName('file_size').AsInteger := FileSize;
      FDataSet.FieldByName('modified_at').AsString :=
        FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

      // Commit changes
      FDataSet.Post;

      Log(Format('Loaded file: %s (%s)', [FileName, FormatFileSize(FileSize)]));
      UpdateFileInfo;

    finally
      FileStream.Free;
    end;

  except
    on E: Exception do
    begin
      FDataSet.Cancel;
      Log('ERROR loading file: ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.SaveBlobToFile(const AFilePath: string);
var
  FileStream: TFileStream;
  BlobStream: TStream;
  BlobField: TBlobField;
begin
  BlobField := FDataSet.FieldByName('image_data') as TBlobField;

  if BlobField.IsNull then
  begin
    Log('ERROR: BLOB field is empty.');
    Exit;
  end;

  try
    // Create output file
    FileStream := TFileStream.Create(AFilePath, fmCreate);
    try
      // Get a read stream from the BLOB field
      BlobStream := FDataSet.CreateBlobStream(BlobField, bmRead);
      try
        FileStream.CopyFrom(BlobStream, 0);  // 0 = copy entire stream
      finally
        BlobStream.Free;
      end;

      Log(Format('Saved to: %s (%s)',
        [AFilePath, FormatFileSize(FileStream.Size)]));

    finally
      FileStream.Free;
    end;

  except
    on E: Exception do
      Log('ERROR saving file: ' + E.Message);
  end;
end;

procedure TfrmMain.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // Don't update if not yet initialized
  if not FInitialized then Exit;

  // Update display when navigating to a different record
  UpdateFileInfo;
  UpdateStatusBar;
end;

procedure TfrmMain.UpdateFileInfo;
var
  BlobField: TBlobField;
  BlobStream: TStream;
  FileType: string;
  FldFileName, FldFileType, FldFileSize, FldImageData: TField;
begin
  if not FDataSet.Active then
  begin
    lblFileName.Caption := 'File: (dataset closed)';
    lblFileType.Caption := 'Type: -';
    lblFileSize.Caption := 'Size: -';
    btnSaveImage.Enabled := False;
    btnSaveFile.Enabled := False;
    btnClearImage.Enabled := False;
    Exit;
  end;

  // Use FindField to avoid exceptions if fields don't exist yet
  FldFileName := FDataSet.FindField('file_name');
  FldFileType := FDataSet.FindField('file_type');
  FldFileSize := FDataSet.FindField('file_size');
  FldImageData := FDataSet.FindField('image_data');

  // Check if fields are available
  if (FldFileName = nil) or (FldFileType = nil) or (FldFileSize = nil) or (FldImageData = nil) then
  begin
    lblFileName.Caption := 'File: (loading...)';
    lblFileType.Caption := 'Type: -';
    lblFileSize.Caption := 'Size: -';
    btnSaveImage.Enabled := False;
    btnSaveFile.Enabled := False;
    btnClearImage.Enabled := False;
    Exit;
  end;

  // Display stored metadata
  lblFileName.Caption := 'File: ' +
    IfThen(FldFileName.AsString <> '', FldFileName.AsString, '(no file)');

  lblFileType.Caption := 'Type: ' +
    IfThen(FldFileType.AsString <> '', FldFileType.AsString, '-');

  // Display file size
  if not FldFileSize.IsNull then
    lblFileSize.Caption := 'Size: ' + FormatFileSize(FldFileSize.AsInteger)
  else
  begin
    // Calculate from BLOB if metadata missing
    BlobField := FldImageData as TBlobField;
    if not BlobField.IsNull then
    begin
      BlobStream := FDataSet.CreateBlobStream(BlobField, bmRead);
      try
        lblFileSize.Caption := 'Size: ' + FormatFileSize(BlobStream.Size);

        // Try to detect file type if not stored
        if FldFileType.AsString = '' then
        begin
          FileType := DetectFileType(BlobStream);
          if FileType <> '' then
            lblFileType.Caption := 'Type: ' + FileType + ' (detected)';
        end;
      finally
        BlobStream.Free;
      end;
    end
    else
      lblFileSize.Caption := 'Size: -';
  end;

  // Enable/disable buttons based on BLOB state
  BlobField := FldImageData as TBlobField;
  btnSaveImage.Enabled := not BlobField.IsNull;
  btnSaveFile.Enabled := not BlobField.IsNull;
  btnClearImage.Enabled := not BlobField.IsNull;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  if FDataSet.Active then
    pnlStatus.Caption := Format('Record %d of %d',
      [FDataSet.RecNo, FDataSet.RecordCount])
  else
    pnlStatus.Caption := 'Dataset closed';
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  FDataSet.Close;
  FDataSet.Open;
  UpdateFileInfo;
  UpdateStatusBar;
  Log('Data refreshed from database.');
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  Log('Log cleared');
end;

function TfrmMain.FormatFileSize(ASize: Int64): string;
begin
  // Format file size in human-readable format
  if ASize < 1024 then
    Result := Format('%d bytes', [ASize])
  else if ASize < 1024 * 1024 then
    Result := Format('%.1f KB', [ASize / 1024])
  else if ASize < 1024 * 1024 * 1024 then
    Result := Format('%.2f MB', [ASize / 1024 / 1024])
  else
    Result := Format('%.2f GB', [ASize / 1024 / 1024 / 1024]);
end;

function TfrmMain.DetectFileType(AStream: TStream): string;
var
  Header: array[0..7] of Byte;
  OldPos: Int64;
begin
  // Detect file type from magic bytes (file signature)
  Result := '';
  if AStream.Size < 8 then Exit;

  OldPos := AStream.Position;
  try
    AStream.Position := 0;
    AStream.Read(Header, SizeOf(Header));

    // PNG: 89 50 4E 47
    if (Header[0] = $89) and (Header[1] = $50) and
       (Header[2] = $4E) and (Header[3] = $47) then
      Result := '.png'
    // JPEG: FF D8 FF
    else if (Header[0] = $FF) and (Header[1] = $D8) and (Header[2] = $FF) then
      Result := '.jpg'
    // GIF: 47 49 46 38
    else if (Header[0] = $47) and (Header[1] = $49) and
            (Header[2] = $46) and (Header[3] = $38) then
      Result := '.gif'
    // BMP: 42 4D
    else if (Header[0] = $42) and (Header[1] = $4D) then
      Result := '.bmp'
    // PDF: 25 50 44 46
    else if (Header[0] = $25) and (Header[1] = $50) and
            (Header[2] = $44) and (Header[3] = $46) then
      Result := '.pdf'
    // ZIP: 50 4B 03 04
    else if (Header[0] = $50) and (Header[1] = $4B) and
            (Header[2] = $03) and (Header[3] = $04) then
      Result := '.zip';
  finally
    AStream.Position := OldPos;
  end;
end;

procedure TfrmMain.Log(const AMessage: string);
begin
  if AMessage = '' then
    memoLog.Lines.Add('')
  else
    memoLog.Lines.Add(Format('[%s] %s',
      [FormatDateTime('hh:nn:ss', Now), AMessage]));

  memoLog.SelStart := Length(memoLog.Text);
end;

end.
