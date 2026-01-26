{===============================================================================
  NDXSQLite - Custom Collations
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 6 (advanced features)

  Wrapper for creating custom text comparison collations in Pascal.
  Supports UTF-8 encoded text comparisons.
===============================================================================}
unit ndxsqlitecollation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ndxsqlite3api, ndxsqliteconnectionintf, ndxsqliteexceptions;

type
  { Collation comparison callback type }
  { Returns: <0 if A<B, 0 if A=B, >0 if A>B }
  TNDXCollationCompare = function(const A, B: string): Integer of object;

  { Registered collation info }
  TNDXRegisteredCollation = class
  private
    FName: string;
    FCompareFunc: TNDXCollationCompare;
  public
    property Name: string read FName;
  end;

  { Custom Collations Manager }
  TNDXSQLiteCollation = class
  private
    FConnection: INDXSQLiteConnection;
    FRegisteredCollations: TList;

    function GetDBHandle: Psqlite3;
    function FindCollation(const AName: string): TNDXRegisteredCollation;
    procedure ClearCollations;

  public
    constructor Create(AConnection: INDXSQLiteConnection);
    destructor Destroy; override;

    { Registers a custom collation sequence usable in ORDER BY and comparison expressions. }
    procedure RegisterCollation(const AName: string; ACompare: TNDXCollationCompare);

    { Removes a previously registered collation sequence by name. }
    procedure UnregisterCollation(const AName: string);

    { Returns True if a collation with the given name is currently registered. }
    function IsCollationRegistered(const AName: string): Boolean;

    { Returns the names of all currently registered collation sequences. Caller must free. }
    function GetRegisteredCollations: TStringList;

    { Removes all registered collation sequences from the connection. }
    procedure UnregisterAll;

    { Compares two strings in a case-insensitive manner. }
    class function CompareCaseInsensitive(const A, B: string): Integer; static;
    { Compares two strings using natural sort order (numeric substrings sorted by value). }
    class function CompareNatural(const A, B: string): Integer; static;
    { Compares two strings in reverse lexicographic order. }
    class function CompareReverse(const A, B: string): Integer; static;
    { Compares two strings by their numeric value (non-numeric strings compare as zero). }
    class function CompareNumeric(const A, B: string): Integer; static;

    property Connection: INDXSQLiteConnection read FConnection;
  end;

implementation

{ Global callback dispatcher }
type
  PCollationUserData = ^TCollationUserData;
  TCollationUserData = record
    CollationInfo: TNDXRegisteredCollation;
  end;

function CollationCallback(pArg: Pointer;
  nKey1: Integer; pKey1: Pointer;
  nKey2: Integer; pKey2: Pointer): Integer; cdecl;
var
  UserData: PCollationUserData;
  CollInfo: TNDXRegisteredCollation;
  S1, S2: string;
begin
  Result := 0;
  UserData := PCollationUserData(pArg);
  if not Assigned(UserData) then Exit;

  CollInfo := UserData^.CollationInfo;
  if not Assigned(CollInfo) or not Assigned(CollInfo.FCompareFunc) then Exit;

  // Convert to Pascal strings
  SetString(S1, PAnsiChar(pKey1), nKey1);
  SetString(S2, PAnsiChar(pKey2), nKey2);

  // Call the Pascal comparison function
  try
    Result := CollInfo.FCompareFunc(S1, S2);
  except
    Result := 0;
  end;
end;

procedure DestroyCollationCallback(pArg: Pointer); cdecl;
var
  UserData: PCollationUserData;
begin
  UserData := PCollationUserData(pArg);
  if Assigned(UserData) then
    Dispose(UserData);
end;

{ TNDXSQLiteCollation }

constructor TNDXSQLiteCollation.Create(AConnection: INDXSQLiteConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FRegisteredCollations := TList.Create;
end;

destructor TNDXSQLiteCollation.Destroy;
begin
  UnregisterAll;
  FRegisteredCollations.Free;
  inherited Destroy;
end;

function TNDXSQLiteCollation.GetDBHandle: Psqlite3;
begin
  Result := Psqlite3(FConnection.ConnectionHandle);
end;

function TNDXSQLiteCollation.FindCollation(const AName: string): TNDXRegisteredCollation;
var
  I: Integer;
  Coll: TNDXRegisteredCollation;
begin
  Result := nil;
  for I := 0 to FRegisteredCollations.Count - 1 do
  begin
    Coll := TNDXRegisteredCollation(FRegisteredCollations[I]);
    if SameText(Coll.FName, AName) then
    begin
      Result := Coll;
      Exit;
    end;
  end;
end;

procedure TNDXSQLiteCollation.ClearCollations;
var
  I: Integer;
begin
  for I := 0 to FRegisteredCollations.Count - 1 do
    TNDXRegisteredCollation(FRegisteredCollations[I]).Free;
  FRegisteredCollations.Clear;
end;

procedure TNDXSQLiteCollation.RegisterCollation(const AName: string;
  ACompare: TNDXCollationCompare);
var
  CollInfo: TNDXRegisteredCollation;
  UserData: PCollationUserData;
  RC: Integer;
  NameAnsi: AnsiString;
begin
  if not FConnection.IsOpen then
    raise ENDXSQLiteException.Create('Connection must be open to register collations');

  // Check if already registered
  if FindCollation(AName) <> nil then
    raise ENDXSQLiteException.CreateFmt('Collation "%s" is already registered', [AName]);

  // Create collation info
  CollInfo := TNDXRegisteredCollation.Create;
  CollInfo.FName := AName;
  CollInfo.FCompareFunc := ACompare;

  // Create user data
  New(UserData);
  UserData^.CollationInfo := CollInfo;

  // Register with SQLite
  NameAnsi := AnsiString(AName);
  RC := sqlite3_create_collation_v2(
    GetDBHandle,
    PAnsiChar(NameAnsi),
    SQLITE_UTF8,
    UserData,
    @CollationCallback,
    @DestroyCollationCallback
  );

  if RC <> SQLITE_OK then
  begin
    Dispose(UserData);
    CollInfo.Free;
    raise ENDXSQLiteException.CreateFmt('Failed to register collation "%s": %s',
      [AName, string(sqlite3_errmsg(GetDBHandle))]);
  end;

  FRegisteredCollations.Add(CollInfo);
end;

procedure TNDXSQLiteCollation.UnregisterCollation(const AName: string);
var
  CollInfo: TNDXRegisteredCollation;
  RC: Integer;
  NameAnsi: AnsiString;
begin
  CollInfo := FindCollation(AName);
  if CollInfo = nil then
    Exit;

  // Unregister from SQLite by passing nil callback
  NameAnsi := AnsiString(AName);
  RC := sqlite3_create_collation_v2(
    GetDBHandle,
    PAnsiChar(NameAnsi),
    SQLITE_UTF8,
    nil,
    nil,
    nil
  );

  // Remove from list regardless of result
  FRegisteredCollations.Remove(CollInfo);
  CollInfo.Free;
end;

function TNDXSQLiteCollation.IsCollationRegistered(const AName: string): Boolean;
begin
  Result := FindCollation(AName) <> nil;
end;

function TNDXSQLiteCollation.GetRegisteredCollations: TStringList;
var
  I: Integer;
  CollInfo: TNDXRegisteredCollation;
begin
  Result := TStringList.Create;
  for I := 0 to FRegisteredCollations.Count - 1 do
  begin
    CollInfo := TNDXRegisteredCollation(FRegisteredCollations[I]);
    Result.Add(CollInfo.FName);
  end;
end;

procedure TNDXSQLiteCollation.UnregisterAll;
var
  I: Integer;
  CollInfo: TNDXRegisteredCollation;
  NameAnsi: AnsiString;
begin
  // Unregister all from SQLite
  for I := FRegisteredCollations.Count - 1 downto 0 do
  begin
    CollInfo := TNDXRegisteredCollation(FRegisteredCollations[I]);
    if FConnection.IsOpen then
    begin
      NameAnsi := AnsiString(CollInfo.FName);
      sqlite3_create_collation_v2(
        GetDBHandle,
        PAnsiChar(NameAnsi),
        SQLITE_UTF8,
        nil,
        nil,
        nil
      );
    end;
  end;

  ClearCollations;
end;

{ Built-in collation helpers }

class function TNDXSQLiteCollation.CompareCaseInsensitive(const A, B: string): Integer;
begin
  Result := CompareText(A, B);
end;

class function TNDXSQLiteCollation.CompareNatural(const A, B: string): Integer;
var
  I, J: Integer;
  NumA, NumB: Int64;
  StartA, StartB: Integer;
begin
  I := 1;
  J := 1;

  while (I <= Length(A)) and (J <= Length(B)) do
  begin
    // Both are digits - compare numerically
    if (A[I] >= '0') and (A[I] <= '9') and
       (B[J] >= '0') and (B[J] <= '9') then
    begin
      // Extract number from A
      NumA := 0;
      StartA := I;
      while (I <= Length(A)) and (A[I] >= '0') and (A[I] <= '9') do
        Inc(I);
      if I > StartA then
        NumA := StrToInt64(Copy(A, StartA, I - StartA));

      // Extract number from B
      NumB := 0;
      StartB := J;
      while (J <= Length(B)) and (B[J] >= '0') and (B[J] <= '9') do
        Inc(J);
      if J > StartB then
        NumB := StrToInt64(Copy(B, StartB, J - StartB));

      if NumA < NumB then
      begin
        Result := -1;
        Exit;
      end
      else if NumA > NumB then
      begin
        Result := 1;
        Exit;
      end;
      // Continue if equal
    end
    else
    begin
      // Compare characters case-insensitively
      Result := Ord(UpCase(A[I])) - Ord(UpCase(B[J]));
      if Result <> 0 then
        Exit;
      Inc(I);
      Inc(J);
    end;
  end;

  // Shorter string comes first
  Result := (Length(A) - I + 1) - (Length(B) - J + 1);
end;

class function TNDXSQLiteCollation.CompareReverse(const A, B: string): Integer;
begin
  // Reverse of standard comparison
  Result := CompareStr(B, A);
end;

class function TNDXSQLiteCollation.CompareNumeric(const A, B: string): Integer;
var
  NumA, NumB: Extended;
  CodeA, CodeB: Integer;
begin
  Val(A, NumA, CodeA);
  Val(B, NumB, CodeB);

  // If both are valid numbers, compare numerically
  if (CodeA = 0) and (CodeB = 0) then
  begin
    if NumA < NumB then
      Result := -1
    else if NumA > NumB then
      Result := 1
    else
      Result := 0;
  end
  // If only A is a number, A comes first
  else if CodeA = 0 then
    Result := -1
  // If only B is a number, B comes first
  else if CodeB = 0 then
    Result := 1
  // Both are non-numeric, compare as strings
  else
    Result := CompareStr(A, B);
end;

end.
