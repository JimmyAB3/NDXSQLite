program test_sqlcipher_load;
uses ndxsqliteplatform;
var
  Paths: TStringArray;
  I: Integer;
begin
  WriteLn('SQLite library paths searched:');
  Paths := TNDXPlatform.GetSQLiteLibraryPaths;
  for I := 0 to High(Paths) do
    WriteLn('  ', I, ': ', Paths[I]);
  WriteLn;
  WriteLn('Loading library...');
  TNDXPlatform.LoadSQLiteLibrary;
  WriteLn('Loaded path: ', TNDXPlatform.GetSQLiteLoadedPath);
  WriteLn('SQLite version: ', TNDXPlatform.GetSQLiteVersion);
end.
