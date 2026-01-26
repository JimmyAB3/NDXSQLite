{===============================================================================
  NDXSQLite - SQL Dump and Restore API
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Provides complete database export to SQL format with:
  - BLOB columns exported as X'hexadecimal'
  - Foreign key dependency ordering (topological sort)
  - Full schema export (tables, indexes, views, triggers)
  - SQL file import/restore
===============================================================================}
unit ndxsqlitedump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fgl,
  ndxsqliteconnection;

type
  { Dump options }
  TNDXDumpOptions = record
    IncludeSchema: Boolean;       // Include CREATE statements (default: True)
    IncludeData: Boolean;         // Include INSERT statements (default: True)
    IncludeIndexes: Boolean;      // Include indexes (default: True)
    IncludeViews: Boolean;        // Include views (default: True)
    IncludeTriggers: Boolean;     // Include triggers (default: True)
    DisableForeignKeys: Boolean;  // Wrap in PRAGMA foreign_keys=OFF (default: True)
    UseTransaction: Boolean;      // Wrap in BEGIN/COMMIT (default: True)
    AddComments: Boolean;         // Add comments in SQL file (default: True)
  end;

  { Dump result }
  TNDXDumpResult = record
    Success: Boolean;
    ErrorMessage: string;
    FilePath: string;
    FileSize: Int64;
    TableCount: Integer;
    RowCount: Integer;
    IndexCount: Integer;
    ViewCount: Integer;
    TriggerCount: Integer;
  end;

  { Import options }
  TNDXImportOptions = record
    EnableForeignKeys: Boolean;   // Enable FK during import (default: False)
    StopOnError: Boolean;         // Stop on first error (default: True)
    UseTransaction: Boolean;      // Wrap in transaction (default: True)
  end;

  { Import result }
  TNDXImportResult = record
    Success: Boolean;
    ErrorMessage: string;
    StatementsExecuted: Integer;
    ErrorCount: Integer;
    Errors: TStringList;
  end;

  { Progress callback }
  TNDXDumpProgressEvent = procedure(Sender: TObject;
    const ACurrentTable: string; ARowsProcessed, ATotalRows: Integer) of object;

  { SQLite dump manager }
  TNDXSQLiteDump = class
  private
    FConnection: TNDXSQLiteConnection;
    FOnProgress: TNDXDumpProgressEvent;
    FDefaultDumpOptions: TNDXDumpOptions;
    FDefaultImportOptions: TNDXImportOptions;

    function BlobToHex(AField: TField): string;
    function EscapeSQLString(const AValue: string): string;
    function FieldToSQL(AField: TField): string;
    function GetFileSize(const APath: string): Int64;

  public
    constructor Create(AConnection: TNDXSQLiteConnection);

    { Returns table names ordered by foreign key dependencies for safe export. Caller must free. }
    function GetTableExportOrder: TStringList;

    { Exports the entire database to a SQL file using default options. }
    function ExportToSQL(const AFilePath: string): TNDXDumpResult; overload;
    { Exports the entire database to a SQL file with custom schema and data options. }
    function ExportToSQL(const AFilePath: string;
      const AOptions: TNDXDumpOptions): TNDXDumpResult; overload;

    { Exports the entire database as SQL statements to an in-memory string list. Caller must free. }
    function ExportToStringList: TStringList; overload;
    { Exports the database to a string list with custom options. Caller must free. }
    function ExportToStringList(const AOptions: TNDXDumpOptions): TStringList; overload;

    { Executes SQL statements from a file to restore or populate the database. }
    function ImportFromSQL(const AFilePath: string): TNDXImportResult; overload;
    { Imports SQL from a file with custom transaction and error-handling options. }
    function ImportFromSQL(const AFilePath: string;
      const AOptions: TNDXImportOptions): TNDXImportResult; overload;

    { Executes SQL statements from an in-memory string list. }
    function ImportFromStringList(ALines: TStringList): TNDXImportResult; overload;
    { Imports SQL from a string list with custom options. }
    function ImportFromStringList(ALines: TStringList;
      const AOptions: TNDXImportOptions): TNDXImportResult; overload;

    { Clones the database to a new file by performing a full SQL dump and restore. }
    function CloneDatabase(const ADestPath: string): TNDXDumpResult;

    { Returns the default dump options (all schema objects included, transaction wrapped). }
    class function DefaultDumpOptions: TNDXDumpOptions; static;
    { Returns the default import options (foreign keys disabled, stop on error, transaction wrapped). }
    class function DefaultImportOptions: TNDXImportOptions; static;

    property OnProgress: TNDXDumpProgressEvent read FOnProgress write FOnProgress;
    property DumpOptions: TNDXDumpOptions read FDefaultDumpOptions write FDefaultDumpOptions;
    property ImportOptions: TNDXImportOptions read FDefaultImportOptions write FDefaultImportOptions;
  end;

implementation

type
  TStringListMap = specialize TFPGMap<string, TStringList>;

{ TNDXSQLiteDump }

constructor TNDXSQLiteDump.Create(AConnection: TNDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FDefaultDumpOptions := DefaultDumpOptions;
  FDefaultImportOptions := DefaultImportOptions;
end;

class function TNDXSQLiteDump.DefaultDumpOptions: TNDXDumpOptions;
begin
  Result.IncludeSchema := True;
  Result.IncludeData := True;
  Result.IncludeIndexes := True;
  Result.IncludeViews := True;
  Result.IncludeTriggers := True;
  Result.DisableForeignKeys := True;
  Result.UseTransaction := True;
  Result.AddComments := True;
end;

class function TNDXSQLiteDump.DefaultImportOptions: TNDXImportOptions;
begin
  Result.EnableForeignKeys := False;
  Result.StopOnError := True;
  Result.UseTransaction := True;
end;

function TNDXSQLiteDump.BlobToHex(AField: TField): string;
var
  Stream: TStream;
  Buffer: array of Byte;
  I: Integer;
begin
  Result := '';
  if AField.IsNull then
    Exit;

  Stream := AField.DataSet.CreateBlobStream(AField, bmRead);
  try
    if Stream.Size = 0 then
      Exit;
    SetLength(Buffer, Stream.Size);
    Stream.ReadBuffer(Buffer[0], Stream.Size);
    Result := 'X''';
    for I := 0 to High(Buffer) do
      Result := Result + IntToHex(Buffer[I], 2);
    Result := Result + '''';
  finally
    Stream.Free;
  end;
end;

function TNDXSQLiteDump.EscapeSQLString(const AValue: string): string;
begin
  Result := StringReplace(AValue, '''', '''''', [rfReplaceAll]);
end;

function TNDXSQLiteDump.FieldToSQL(AField: TField): string;
begin
  if AField.IsNull then
    Result := 'NULL'
  else if AField.DataType in [ftBlob, ftMemo, ftGraphic, ftBytes, ftVarBytes] then
  begin
    if AField.DataType = ftMemo then
      Result := '''' + EscapeSQLString(AField.AsString) + ''''
    else
      Result := BlobToHex(AField);
    if Result = '' then
      Result := 'NULL';
  end
  else if AField.DataType in [ftInteger, ftSmallint, ftWord, ftLargeint, ftAutoInc] then
    Result := AField.AsString
  else if AField.DataType in [ftFloat, ftCurrency, ftBCD] then
    Result := StringReplace(AField.AsString, ',', '.', [rfReplaceAll])
  else
    Result := '''' + EscapeSQLString(AField.AsString) + '''';
end;

function TNDXSQLiteDump.GetFileSize(const APath: string): Int64;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(APath, faAnyFile, SR) = 0 then
  begin
    Result := SR.Size;
    FindClose(SR);
  end;
end;

function TNDXSQLiteDump.GetTableExportOrder: TStringList;
var
  AllTables: TStringList;
  Dependencies: TStringListMap;
  DS, FKDS: TDataSet;
  TableName, RefTable: string;
  I: Integer;
  DepList: TStringList;
  Ordered: TStringList;
  InDegree: array of Integer;
  Queue: TStringList;
  Current: string;
  Idx: Integer;
begin
  Result := TStringList.Create;
  AllTables := TStringList.Create;
  Dependencies := TStringListMap.Create;
  Queue := TStringList.Create;
  Ordered := TStringList.Create;
  try
    // Get all tables
    DS := FConnection.ExecuteQuery(
      'SELECT name FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%'' ORDER BY name');
    try
      while not DS.EOF do
      begin
        TableName := DS.FieldByName('name').AsString;
        AllTables.Add(TableName);
        Dependencies.Add(TableName, TStringList.Create);
        DS.Next;
      end;
    finally
      DS.Free;
    end;

    // For each table, get its foreign key references
    for I := 0 to AllTables.Count - 1 do
    begin
      TableName := AllTables[I];
      FKDS := FConnection.ExecuteQuery('PRAGMA foreign_key_list("' + TableName + '")');
      try
        while not FKDS.EOF do
        begin
          RefTable := FKDS.FieldByName('table').AsString;
          if (AllTables.IndexOf(RefTable) >= 0) and (RefTable <> TableName) then
          begin
            Idx := Dependencies.IndexOf(TableName);
            if Idx >= 0 then
            begin
              DepList := Dependencies.Data[Idx];
              if DepList.IndexOf(RefTable) < 0 then
                DepList.Add(RefTable);
            end;
          end;
          FKDS.Next;
        end;
      finally
        FKDS.Free;
      end;
    end;

    // Kahn's algorithm for topological sort
    SetLength(InDegree, AllTables.Count);
    for I := 0 to AllTables.Count - 1 do
      InDegree[I] := 0;

    for I := 0 to AllTables.Count - 1 do
    begin
      TableName := AllTables[I];
      Idx := Dependencies.IndexOf(TableName);
      if Idx >= 0 then
      begin
        DepList := Dependencies.Data[Idx];
        InDegree[I] := DepList.Count;
      end;
    end;

    // Start with tables that have no dependencies
    for I := 0 to AllTables.Count - 1 do
    begin
      if InDegree[I] = 0 then
        Queue.Add(AllTables[I]);
    end;

    // Process queue
    while Queue.Count > 0 do
    begin
      Current := Queue[0];
      Queue.Delete(0);
      Ordered.Add(Current);

      for I := 0 to AllTables.Count - 1 do
      begin
        TableName := AllTables[I];
        Idx := Dependencies.IndexOf(TableName);
        if Idx >= 0 then
        begin
          DepList := Dependencies.Data[Idx];
          if DepList.IndexOf(Current) >= 0 then
          begin
            Dec(InDegree[I]);
            if InDegree[I] = 0 then
              Queue.Add(TableName);
          end;
        end;
      end;
    end;

    // Handle cycles - add remaining tables
    for I := 0 to AllTables.Count - 1 do
    begin
      if Ordered.IndexOf(AllTables[I]) < 0 then
        Ordered.Add(AllTables[I]);
    end;

    // Copy result
    for I := 0 to Ordered.Count - 1 do
      Result.Add(Ordered[I]);

  finally
    for I := 0 to Dependencies.Count - 1 do
      Dependencies.Data[I].Free;
    Dependencies.Free;
    AllTables.Free;
    Queue.Free;
    Ordered.Free;
  end;
end;

function TNDXSQLiteDump.ExportToStringList(const AOptions: TNDXDumpOptions): TStringList;
var
  DS, DataDS: TDataSet;
  TableName, SQL, CreateSQL: string;
  I, T: Integer;
  FieldValues: string;
  TableOrder: TStringList;
  RowCount: Integer;
begin
  Result := TStringList.Create;
  RowCount := 0;

  if AOptions.AddComments then
  begin
    Result.Add('-- SQLite Database Dump');
    Result.Add('-- Generated by NDXSQLite');
    Result.Add('-- Date: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Result.Add('-- BLOBs are exported as X''hexadecimal''');
    Result.Add('-- Tables ordered by foreign key dependencies');
    Result.Add('');
  end;

  if AOptions.DisableForeignKeys then
    Result.Add('PRAGMA foreign_keys=OFF;');

  if AOptions.UseTransaction then
    Result.Add('BEGIN TRANSACTION;');

  Result.Add('');

  // Export table schemas
  if AOptions.IncludeSchema then
  begin
    if AOptions.AddComments then
      Result.Add('-- Tables');

    DS := FConnection.ExecuteQuery(
      'SELECT name, sql FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%'' ORDER BY name');
    try
      while not DS.EOF do
      begin
        CreateSQL := DS.FieldByName('sql').AsString;
        Result.Add(CreateSQL + ';');
        DS.Next;
      end;
    finally
      DS.Free;
    end;
    Result.Add('');
  end;

  // Export data in FK dependency order
  if AOptions.IncludeData then
  begin
    if AOptions.AddComments then
      Result.Add('-- Data');

    TableOrder := GetTableExportOrder;
    try
      for T := 0 to TableOrder.Count - 1 do
      begin
        TableName := TableOrder[T];

        DataDS := FConnection.ExecuteQuery('SELECT * FROM "' + TableName + '"');
        try
          if not DataDS.IsEmpty then
          begin
            if AOptions.AddComments then
              Result.Add('-- ' + TableName);

            while not DataDS.EOF do
            begin
              FieldValues := '';
              for I := 0 to DataDS.FieldCount - 1 do
              begin
                if I > 0 then
                  FieldValues := FieldValues + ', ';
                FieldValues := FieldValues + FieldToSQL(DataDS.Fields[I]);
              end;
              Result.Add('INSERT INTO "' + TableName + '" VALUES (' + FieldValues + ');');
              Inc(RowCount);

              if Assigned(FOnProgress) then
                FOnProgress(Self, TableName, RowCount, -1);

              DataDS.Next;
            end;
          end;
        finally
          DataDS.Free;
        end;
      end;
    finally
      TableOrder.Free;
    end;
    Result.Add('');
  end;

  // Export indexes
  if AOptions.IncludeIndexes then
  begin
    DS := FConnection.ExecuteQuery(
      'SELECT sql FROM sqlite_master WHERE type=''index'' AND sql IS NOT NULL ORDER BY name');
    try
      if not DS.IsEmpty then
      begin
        if AOptions.AddComments then
          Result.Add('-- Indexes');
        while not DS.EOF do
        begin
          SQL := DS.FieldByName('sql').AsString;
          if SQL <> '' then
            Result.Add(SQL + ';');
          DS.Next;
        end;
        Result.Add('');
      end;
    finally
      DS.Free;
    end;
  end;

  // Export views
  if AOptions.IncludeViews then
  begin
    DS := FConnection.ExecuteQuery(
      'SELECT sql FROM sqlite_master WHERE type=''view'' ORDER BY name');
    try
      if not DS.IsEmpty then
      begin
        if AOptions.AddComments then
          Result.Add('-- Views');
        while not DS.EOF do
        begin
          SQL := DS.FieldByName('sql').AsString;
          if SQL <> '' then
            Result.Add(SQL + ';');
          DS.Next;
        end;
        Result.Add('');
      end;
    finally
      DS.Free;
    end;
  end;

  // Export triggers
  if AOptions.IncludeTriggers then
  begin
    DS := FConnection.ExecuteQuery(
      'SELECT sql FROM sqlite_master WHERE type=''trigger'' ORDER BY name');
    try
      if not DS.IsEmpty then
      begin
        if AOptions.AddComments then
          Result.Add('-- Triggers');
        while not DS.EOF do
        begin
          SQL := DS.FieldByName('sql').AsString;
          if SQL <> '' then
            Result.Add(SQL + ';');
          DS.Next;
        end;
        Result.Add('');
      end;
    finally
      DS.Free;
    end;
  end;

  if AOptions.UseTransaction then
    Result.Add('COMMIT;');

  if AOptions.DisableForeignKeys then
    Result.Add('PRAGMA foreign_keys=ON;');
end;

function TNDXSQLiteDump.ExportToStringList: TStringList;
begin
  Result := ExportToStringList(FDefaultDumpOptions);
end;

function TNDXSQLiteDump.ExportToSQL(const AFilePath: string;
  const AOptions: TNDXDumpOptions): TNDXDumpResult;
var
  Lines: TStringList;
  DS: TDataSet;
begin
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.FilePath := AFilePath;
  Result.FileSize := 0;
  Result.TableCount := 0;
  Result.RowCount := 0;
  Result.IndexCount := 0;
  Result.ViewCount := 0;
  Result.TriggerCount := 0;

  try
    Lines := ExportToStringList(AOptions);
    try
      Lines.SaveToFile(AFilePath);
      Result.FileSize := GetFileSize(AFilePath);
    finally
      Lines.Free;
    end;

    // Get counts
    DS := FConnection.ExecuteQuery(
      'SELECT ' +
      '  (SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%'') as tables, ' +
      '  (SELECT COUNT(*) FROM sqlite_master WHERE type=''index'' AND sql IS NOT NULL) as indexes, ' +
      '  (SELECT COUNT(*) FROM sqlite_master WHERE type=''view'') as views, ' +
      '  (SELECT COUNT(*) FROM sqlite_master WHERE type=''trigger'') as triggers');
    try
      Result.TableCount := DS.FieldByName('tables').AsInteger;
      Result.IndexCount := DS.FieldByName('indexes').AsInteger;
      Result.ViewCount := DS.FieldByName('views').AsInteger;
      Result.TriggerCount := DS.FieldByName('triggers').AsInteger;
    finally
      DS.Free;
    end;

    Result.Success := True;
  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;
end;

function TNDXSQLiteDump.ExportToSQL(const AFilePath: string): TNDXDumpResult;
begin
  Result := ExportToSQL(AFilePath, FDefaultDumpOptions);
end;

function TNDXSQLiteDump.ImportFromStringList(ALines: TStringList;
  const AOptions: TNDXImportOptions): TNDXImportResult;
var
  Line, Statement, UpperStmt: string;
  I: Integer;
begin
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.StatementsExecuted := 0;
  Result.ErrorCount := 0;
  Result.Errors := TStringList.Create;

  try
    if not AOptions.EnableForeignKeys then
      FConnection.ExecuteNonQuery('PRAGMA foreign_keys=OFF')
    else
      FConnection.ExecuteNonQuery('PRAGMA foreign_keys=ON');

    if AOptions.UseTransaction then
      FConnection.BeginTransaction;

    try
      Statement := '';

      for I := 0 to ALines.Count - 1 do
      begin
        Line := Trim(ALines[I]);

        // Skip comments and empty lines
        if (Line = '') or (Copy(Line, 1, 2) = '--') then
          Continue;

        Statement := Statement + ' ' + Line;

        // Check if statement is complete
        if (Length(Line) > 0) and (Line[Length(Line)] = ';') then
        begin
          Statement := Trim(Statement);
          UpperStmt := UpperCase(Statement);

          // Skip transaction control and PRAGMA foreign_keys statements
          if (Pos('BEGIN TRANSACTION', UpperStmt) > 0) or
             (Pos('BEGIN;', UpperStmt) > 0) or
             (Pos('COMMIT', UpperStmt) > 0) or
             (Pos('ROLLBACK', UpperStmt) > 0) or
             (Pos('PRAGMA FOREIGN_KEYS', UpperStmt) > 0) then
          begin
            Statement := '';
            Continue;
          end;

          if Statement <> '' then
          begin
            try
              FConnection.ExecuteNonQuery(Statement);
              Inc(Result.StatementsExecuted);
            except
              on E: Exception do
              begin
                Inc(Result.ErrorCount);
                Result.Errors.Add(Format('Line %d: %s', [I + 1, E.Message]));
                if AOptions.StopOnError then
                  raise;
              end;
            end;
          end;
          Statement := '';
        end;
      end;

      if AOptions.UseTransaction then
        FConnection.Commit;

      Result.Success := (Result.ErrorCount = 0);
    except
      on E: Exception do
      begin
        if AOptions.UseTransaction then
          FConnection.Rollback;
        Result.ErrorMessage := E.Message;
      end;
    end;

    FConnection.ExecuteNonQuery('PRAGMA foreign_keys=ON');
  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;
end;

function TNDXSQLiteDump.ImportFromStringList(ALines: TStringList): TNDXImportResult;
begin
  Result := ImportFromStringList(ALines, FDefaultImportOptions);
end;

function TNDXSQLiteDump.ImportFromSQL(const AFilePath: string;
  const AOptions: TNDXImportOptions): TNDXImportResult;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath);
    Result := ImportFromStringList(Lines, AOptions);
  finally
    Lines.Free;
  end;
end;

function TNDXSQLiteDump.ImportFromSQL(const AFilePath: string): TNDXImportResult;
begin
  Result := ImportFromSQL(AFilePath, FDefaultImportOptions);
end;

function TNDXSQLiteDump.CloneDatabase(const ADestPath: string): TNDXDumpResult;
var
  DumpSQL: TStringList;
  DestConn: TNDXSQLiteConnection;
  DestDump: TNDXSQLiteDump;
  ImportResult: TNDXImportResult;
  ImportOpts: TNDXImportOptions;
begin
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.FilePath := ADestPath;

  try
    // Export current database
    DumpSQL := ExportToStringList;
    try
      // Create destination connection
      DestConn := TNDXSQLiteConnection.Create(ADestPath, False);
      try
        DestConn.Open;

        // Import with FK disabled
        ImportOpts := DefaultImportOptions;
        ImportOpts.EnableForeignKeys := False;

        DestDump := TNDXSQLiteDump.Create(DestConn);
        try
          ImportResult := DestDump.ImportFromStringList(DumpSQL, ImportOpts);
          ImportResult.Errors.Free;
        finally
          DestDump.Free;
        end;

        if ImportResult.Success then
        begin
          Result.Success := True;
          Result.FileSize := GetFileSize(ADestPath);
        end
        else
          Result.ErrorMessage := ImportResult.ErrorMessage;

        DestConn.Close;
      finally
        DestConn.Free;
      end;
    finally
      DumpSQL.Free;
    end;
  except
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;
end;

end.
