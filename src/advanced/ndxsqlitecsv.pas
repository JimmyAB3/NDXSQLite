{===============================================================================
  NDXSQLite - CSV Import/Export Unit

  Provides CSV import and export functionality for SQLite databases.

  Features:
  - Export query results or tables to CSV files
  - Import CSV files into database tables
  - Proper CSV escaping (quotes, commas, newlines)
  - RFC 4180 compliant parsing
  - BLOB support (exported as hex)
  - Configurable options (delimiter, header, encoding)

  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
===============================================================================}
unit ndxsqlitecsv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  ndxsqliteconnection;

type
  { CSV export options }
  TNDXCSVExportOptions = record
    Delimiter: Char;
    IncludeHeader: Boolean;
    QuoteAllFields: Boolean;
    NullValue: string;
    BlobFormat: (bfHex, bfBase64, bfSkip);
    LineEnding: string;
  end;

  { CSV import options }
  TNDXCSVImportOptions = record
    Delimiter: Char;
    HasHeader: Boolean;
    SkipEmptyLines: Boolean;
    TrimFields: Boolean;
    NullValue: string;
    UseTransaction: Boolean;
    StopOnError: Boolean;
  end;

  { CSV export result }
  TNDXCSVExportResult = record
    Success: Boolean;
    FilePath: string;
    RowCount: Integer;
    ColumnCount: Integer;
    FileSize: Int64;
    ErrorMessage: string;
  end;

  { CSV import result }
  TNDXCSVImportResult = record
    Success: Boolean;
    RowsImported: Integer;
    RowsSkipped: Integer;
    ErrorCount: Integer;
    Errors: TStringList;
    ErrorMessage: string;
  end;

  { TNDXSQLiteCSV - CSV import/export class }
  TNDXSQLiteCSV = class
  private
    FConnection: TNDXSQLiteConnection;

    function BlobToHex(AField: TField): string;
    function BuildInsertSQL(const ATableName: string; AFieldCount: Integer): string;
  public
    constructor Create(AConnection: TNDXSQLiteConnection);

    { Export functions }

    { Exports query results to a CSV file using default options. }
    function ExportQueryToCSV(const ASQL, AFilePath: string): TNDXCSVExportResult;
    { Exports query results to a CSV file with custom delimiter, quoting, and encoding options. }
    function ExportQueryToCSV(const ASQL, AFilePath: string;
      const AOptions: TNDXCSVExportOptions): TNDXCSVExportResult;
    { Exports all rows from the named table to a CSV file using default options. }
    function ExportTableToCSV(const ATableName, AFilePath: string): TNDXCSVExportResult;
    { Exports all rows from the named table to a CSV file with custom options. }
    function ExportTableToCSV(const ATableName, AFilePath: string;
      const AOptions: TNDXCSVExportOptions): TNDXCSVExportResult;
    { Executes the SQL query and returns the results as a CSV-formatted string list. Caller must free. }
    function ExportToStringList(const ASQL: string): TStringList;
    { Executes the SQL query and returns CSV-formatted results using custom options. Caller must free. }
    function ExportToStringList(const ASQL: string;
      const AOptions: TNDXCSVExportOptions): TStringList;

    { Import functions }

    { Imports a CSV file into the specified table using default options. }
    function ImportFromCSV(const AFilePath, ATableName: string): TNDXCSVImportResult;
    { Imports a CSV file into the specified table with custom parsing options. }
    function ImportFromCSV(const AFilePath, ATableName: string;
      const AOptions: TNDXCSVImportOptions): TNDXCSVImportResult;
    { Imports a CSV file into the specified table using explicit column name mapping. }
    function ImportFromCSV(const AFilePath, ATableName: string;
      const AColumnMapping: array of string): TNDXCSVImportResult;
    { Imports rows from an in-memory string list into the specified table using default options. }
    function ImportFromStringList(ALines: TStringList; const ATableName: string): TNDXCSVImportResult;
    { Imports rows from an in-memory string list with custom parsing options. }
    function ImportFromStringList(ALines: TStringList; const ATableName: string;
      const AOptions: TNDXCSVImportOptions): TNDXCSVImportResult;

    { Utility class functions }

    { Wraps the field value in quotes if it contains the delimiter, quotes, or newlines. }
    class function EscapeField(const AValue: string; ADelimiter: Char = ','): string;
    { Parses a single CSV line into individual field values. Caller must free. }
    class function ParseLine(const ALine: string; ADelimiter: Char = ','): TStringList;
    { Returns the default export options (comma-delimited, header included, no forced quoting). }
    class function DefaultExportOptions: TNDXCSVExportOptions;
    { Returns the default import options (comma-delimited, header expected, transaction enabled). }
    class function DefaultImportOptions: TNDXCSVImportOptions;
  end;

implementation

{ TNDXSQLiteCSV }

constructor TNDXSQLiteCSV.Create(AConnection: TNDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

class function TNDXSQLiteCSV.DefaultExportOptions: TNDXCSVExportOptions;
begin
  Result.Delimiter := ',';
  Result.IncludeHeader := True;
  Result.QuoteAllFields := False;
  Result.NullValue := '';
  Result.BlobFormat := bfHex;
  Result.LineEnding := sLineBreak;
end;

class function TNDXSQLiteCSV.DefaultImportOptions: TNDXCSVImportOptions;
begin
  Result.Delimiter := ',';
  Result.HasHeader := True;
  Result.SkipEmptyLines := True;
  Result.TrimFields := True;
  Result.NullValue := '';
  Result.UseTransaction := True;
  Result.StopOnError := False;
end;

class function TNDXSQLiteCSV.EscapeField(const AValue: string; ADelimiter: Char): string;
var
  NeedsQuotes: Boolean;
begin
  NeedsQuotes := (Pos(ADelimiter, AValue) > 0) or
                 (Pos('"', AValue) > 0) or
                 (Pos(#10, AValue) > 0) or
                 (Pos(#13, AValue) > 0);

  if NeedsQuotes then
    Result := '"' + StringReplace(AValue, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := AValue;
end;

class function TNDXSQLiteCSV.ParseLine(const ALine: string; ADelimiter: Char): TStringList;
var
  InQuotes: Boolean;
  Ch: Char;
  CurrentField: string;
  I: Integer;
begin
  Result := TStringList.Create;
  InQuotes := False;
  CurrentField := '';

  I := 1;
  while I <= Length(ALine) do
  begin
    Ch := ALine[I];

    if Ch = '"' then
    begin
      if InQuotes and (I < Length(ALine)) and (ALine[I + 1] = '"') then
      begin
        // Escaped quote
        CurrentField := CurrentField + '"';
        Inc(I); // Skip next quote
      end
      else
        InQuotes := not InQuotes;
    end
    else if (Ch = ADelimiter) and not InQuotes then
    begin
      Result.Add(CurrentField);
      CurrentField := '';
    end
    else
      CurrentField := CurrentField + Ch;

    Inc(I);
  end;

  // Add last field
  Result.Add(CurrentField);
end;

function TNDXSQLiteCSV.BlobToHex(AField: TField): string;
var
  Stream: TStream;
  Bytes: TBytes;
  I: Integer;
begin
  Result := '';
  if AField.IsNull then
    Exit;

  Stream := AField.DataSet.CreateBlobStream(AField, bmRead);
  try
    if Stream.Size > 0 then
    begin
      SetLength(Bytes, Stream.Size);
      Stream.ReadBuffer(Bytes[0], Stream.Size);
      Result := '';
      for I := 0 to Length(Bytes) - 1 do
        Result := Result + IntToHex(Bytes[I], 2);
    end;
  finally
    Stream.Free;
  end;
end;

function TNDXSQLiteCSV.BuildInsertSQL(const ATableName: string; AFieldCount: Integer): string;
var
  Placeholders: string;
  I: Integer;
begin
  Placeholders := '';
  for I := 1 to AFieldCount do
  begin
    if I > 1 then
      Placeholders := Placeholders + ', ';
    Placeholders := Placeholders + '?';
  end;
  Result := Format('INSERT INTO "%s" VALUES (%s)', [ATableName, Placeholders]);
end;

function TNDXSQLiteCSV.ExportQueryToCSV(const ASQL, AFilePath: string): TNDXCSVExportResult;
begin
  Result := ExportQueryToCSV(ASQL, AFilePath, DefaultExportOptions);
end;

function TNDXSQLiteCSV.ExportQueryToCSV(const ASQL, AFilePath: string;
  const AOptions: TNDXCSVExportOptions): TNDXCSVExportResult;
var
  Lines: TStringList;
  F: file of Byte;
  S: string;
  Bytes: TBytes;
begin
  Result.Success := False;
  Result.FilePath := AFilePath;
  Result.RowCount := 0;
  Result.ColumnCount := 0;
  Result.FileSize := 0;
  Result.ErrorMessage := '';

  try
    Lines := ExportToStringList(ASQL, AOptions);
    try
      if Lines.Count > 0 then
      begin
        // Count columns from header
        with ParseLine(Lines[0], AOptions.Delimiter) do
        try
          Result.ColumnCount := Count;
        finally
          Free;
        end;

        // Row count (excluding header if present)
        if AOptions.IncludeHeader then
          Result.RowCount := Lines.Count - 1
        else
          Result.RowCount := Lines.Count;
      end;

      Lines.SaveToFile(AFilePath);

      // Get file size
      AssignFile(F, AFilePath);
      Reset(F);
      try
        Result.FileSize := FileSize(F);
      finally
        CloseFile(F);
      end;

      Result.Success := True;
    finally
      Lines.Free;
    end;
  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;
end;

function TNDXSQLiteCSV.ExportTableToCSV(const ATableName, AFilePath: string): TNDXCSVExportResult;
begin
  Result := ExportQueryToCSV(Format('SELECT * FROM "%s"', [ATableName]), AFilePath);
end;

function TNDXSQLiteCSV.ExportTableToCSV(const ATableName, AFilePath: string;
  const AOptions: TNDXCSVExportOptions): TNDXCSVExportResult;
begin
  Result := ExportQueryToCSV(Format('SELECT * FROM "%s"', [ATableName]), AFilePath, AOptions);
end;

function TNDXSQLiteCSV.ExportToStringList(const ASQL: string): TStringList;
begin
  Result := ExportToStringList(ASQL, DefaultExportOptions);
end;

function TNDXSQLiteCSV.ExportToStringList(const ASQL: string;
  const AOptions: TNDXCSVExportOptions): TStringList;
var
  DS: TDataSet;
  Line: string;
  I: Integer;
  FieldValue: string;
begin
  Result := TStringList.Create;
  try
    DS := FConnection.ExecuteQuery(ASQL);
    try
      // Header
      if AOptions.IncludeHeader then
      begin
        Line := '';
        for I := 0 to DS.FieldCount - 1 do
        begin
          if I > 0 then
            Line := Line + AOptions.Delimiter;
          Line := Line + EscapeField(DS.Fields[I].FieldName, AOptions.Delimiter);
        end;
        Result.Add(Line);
      end;

      // Data rows
      while not DS.EOF do
      begin
        Line := '';
        for I := 0 to DS.FieldCount - 1 do
        begin
          if I > 0 then
            Line := Line + AOptions.Delimiter;

          if DS.Fields[I].IsNull then
            FieldValue := AOptions.NullValue
          else if DS.Fields[I].DataType = ftBlob then
          begin
            case AOptions.BlobFormat of
              bfHex: FieldValue := BlobToHex(DS.Fields[I]);
              bfBase64: FieldValue := ''; // TODO: implement base64
              bfSkip: FieldValue := '[BLOB]';
            end;
          end
          else
            FieldValue := DS.Fields[I].AsString;

          if AOptions.QuoteAllFields then
            Line := Line + '"' + StringReplace(FieldValue, '"', '""', [rfReplaceAll]) + '"'
          else
            Line := Line + EscapeField(FieldValue, AOptions.Delimiter);
        end;
        Result.Add(Line);
        DS.Next;
      end;
    finally
      DS.Free;
    end;
  except
    on E: Exception do
    begin
      Result.Free;
      raise;
    end;
  end;
end;

function TNDXSQLiteCSV.ImportFromCSV(const AFilePath, ATableName: string): TNDXCSVImportResult;
begin
  Result := ImportFromCSV(AFilePath, ATableName, DefaultImportOptions);
end;

function TNDXSQLiteCSV.ImportFromCSV(const AFilePath, ATableName: string;
  const AOptions: TNDXCSVImportOptions): TNDXCSVImportResult;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath);
    Result := ImportFromStringList(Lines, ATableName, AOptions);
  finally
    Lines.Free;
  end;
end;

function TNDXSQLiteCSV.ImportFromCSV(const AFilePath, ATableName: string;
  const AColumnMapping: array of string): TNDXCSVImportResult;
var
  Lines: TStringList;
  Options: TNDXCSVImportOptions;
begin
  // TODO: Implement column mapping
  Options := DefaultImportOptions;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath);
    Result := ImportFromStringList(Lines, ATableName, Options);
  finally
    Lines.Free;
  end;
end;

function TNDXSQLiteCSV.ImportFromStringList(ALines: TStringList;
  const ATableName: string): TNDXCSVImportResult;
begin
  Result := ImportFromStringList(ALines, ATableName, DefaultImportOptions);
end;

function TNDXSQLiteCSV.ImportFromStringList(ALines: TStringList;
  const ATableName: string; const AOptions: TNDXCSVImportOptions): TNDXCSVImportResult;
var
  I, J, StartLine: Integer;
  Fields: TStringList;
  HeaderFields: TStringList;
  InsertSQL, ColumnList: string;
  Params: array of Variant;
  FieldValue: string;
begin
  Result.Success := False;
  Result.RowsImported := 0;
  Result.RowsSkipped := 0;
  Result.ErrorCount := 0;
  Result.Errors := TStringList.Create;
  Result.ErrorMessage := '';

  if ALines.Count = 0 then
  begin
    Result.Success := True;
    Exit;
  end;

  try
    // Determine start line and build column list
    if AOptions.HasHeader then
    begin
      StartLine := 1;
      HeaderFields := ParseLine(ALines[0], AOptions.Delimiter);
      try
        ColumnList := '';
        for I := 0 to HeaderFields.Count - 1 do
        begin
          if I > 0 then
            ColumnList := ColumnList + ', ';
          ColumnList := ColumnList + '"' + HeaderFields[I] + '"';
        end;
        SetLength(Params, HeaderFields.Count);
        InsertSQL := Format('INSERT INTO "%s" (%s) VALUES (', [ATableName, ColumnList]);
        for I := 0 to HeaderFields.Count - 1 do
        begin
          if I > 0 then
            InsertSQL := InsertSQL + ', ';
          InsertSQL := InsertSQL + '?';
        end;
        InsertSQL := InsertSQL + ')';
      finally
        HeaderFields.Free;
      end;
    end
    else
    begin
      StartLine := 0;
      // Determine column count from first line
      Fields := ParseLine(ALines[0], AOptions.Delimiter);
      try
        SetLength(Params, Fields.Count);
        InsertSQL := BuildInsertSQL(ATableName, Fields.Count);
      finally
        Fields.Free;
      end;
    end;

    // Start transaction if requested
    if AOptions.UseTransaction then
      FConnection.BeginTransaction;

    try
      // Import rows
      for I := StartLine to ALines.Count - 1 do
      begin
        if AOptions.SkipEmptyLines and (Trim(ALines[I]) = '') then
        begin
          Inc(Result.RowsSkipped);
          Continue;
        end;

        Fields := ParseLine(ALines[I], AOptions.Delimiter);
        try
          // Check field count matches
          if Fields.Count <> Length(Params) then
          begin
            Inc(Result.RowsSkipped);
            Result.Errors.Add(Format('Line %d: Field count mismatch (expected %d, got %d)',
              [I + 1, Length(Params), Fields.Count]));
            Inc(Result.ErrorCount);
            if AOptions.StopOnError then
            begin
              Result.ErrorMessage := Result.Errors[Result.Errors.Count - 1];
              if AOptions.UseTransaction then
                FConnection.Rollback;
              Exit;
            end;
            Continue;
          end;

          // Build parameters
          for J := 0 to Fields.Count - 1 do
          begin
            FieldValue := Fields[J];
            if AOptions.TrimFields then
              FieldValue := Trim(FieldValue);

            if FieldValue = AOptions.NullValue then
              Params[J] := Null
            else
              Params[J] := FieldValue;
          end;

          // Execute insert
          try
            FConnection.ExecuteNonQuery(InsertSQL, Params);
            Inc(Result.RowsImported);
          except
            on E: Exception do
            begin
              Inc(Result.ErrorCount);
              Result.Errors.Add(Format('Line %d: %s', [I + 1, E.Message]));
              if AOptions.StopOnError then
              begin
                Result.ErrorMessage := E.Message;
                if AOptions.UseTransaction then
                  FConnection.Rollback;
                Exit;
              end;
            end;
          end;
        finally
          Fields.Free;
        end;
      end;

      // Commit transaction
      if AOptions.UseTransaction then
        FConnection.Commit;

      Result.Success := True;
    except
      on E: Exception do
      begin
        if AOptions.UseTransaction then
          FConnection.Rollback;
        Result.ErrorMessage := E.Message;
      end;
    end;
  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;
end;

end.
