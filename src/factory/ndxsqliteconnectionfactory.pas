{===============================================================================
  NDXSQLite - Connection Factory
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 5 (depends on ndxsqliteconnectionintf, ndxsqliteconnection,
             ndxsqliteconnectionoptions, ndxsqliteasyncconnection)
===============================================================================}
unit ndxsqliteconnectionfactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ndxsqliteconnectionintf, ndxsqliteconnection, ndxsqliteconnectionoptions,
  ndxsqliteasyncconnection;

type
  { Factory interface }
  INDXSQLiteConnectionFactory = interface
    ['{F1E2D3C4-B5A6-9870-1234-567890ABCDEF}']
    { Creates a connection using the default options. }
    function CreateConnection: INDXSQLiteConnection;
    { Creates a connection with the specified options. }
    function CreateConnectionWithOptions(AOptions: TNDXSQLiteConnectionOptions): INDXSQLiteConnection;
    { Creates a primary connection with auto-close disabled. }
    function CreatePrimaryConnection: INDXSQLiteConnection;
    { Creates an in-memory database connection. }
    function CreateMemoryConnection: INDXSQLiteConnection;
    { Creates a read-only connection. }
    function CreateReadOnlyConnection: INDXSQLiteConnection;
    { Creates an async connection wrapper for background operations. }
    function CreateAsyncConnection: INDXSQLiteAsyncConnection;
  end;

  { Connection factory implementation }
  TNDXSQLiteConnectionFactory = class(TInterfacedObject, INDXSQLiteConnectionFactory)
  private
    FDefaultOptions: TNDXSQLiteConnectionOptions;

  public
    { Creates a factory with the specified default connection options. }
    constructor Create(ADefaultOptions: TNDXSQLiteConnectionOptions); overload;
    { Creates a factory with default options pointing to the given database path. }
    constructor Create(const ADatabasePath: string); overload;
    destructor Destroy; override;

    { Creates a connection using the default options. }
    function CreateConnection: INDXSQLiteConnection;
    { Creates a connection with the specified options. }
    function CreateConnectionWithOptions(AOptions: TNDXSQLiteConnectionOptions): INDXSQLiteConnection;
    { Creates a primary connection with auto-close disabled. }
    function CreatePrimaryConnection: INDXSQLiteConnection;
    { Creates an in-memory database connection. }
    function CreateMemoryConnection: INDXSQLiteConnection;
    { Creates a read-only connection. }
    function CreateReadOnlyConnection: INDXSQLiteConnection;
    { Creates an async connection wrapper for background operations. }
    function CreateAsyncConnection: INDXSQLiteAsyncConnection;

    property DefaultOptions: TNDXSQLiteConnectionOptions read FDefaultOptions;
  end;

implementation

{ TNDXSQLiteConnectionFactory }

constructor TNDXSQLiteConnectionFactory.Create(ADefaultOptions: TNDXSQLiteConnectionOptions);
begin
  inherited Create;
  FDefaultOptions := ADefaultOptions.Clone;
end;

constructor TNDXSQLiteConnectionFactory.Create(const ADatabasePath: string);
var
  Opts: TNDXSQLiteConnectionOptions;
begin
  inherited Create;
  Opts := TNDXSQLiteConnectionOptions.Create;
  Opts.DatabasePath := ADatabasePath;
  FDefaultOptions := Opts;
end;

destructor TNDXSQLiteConnectionFactory.Destroy;
begin
  FreeAndNil(FDefaultOptions);
  inherited Destroy;
end;

function TNDXSQLiteConnectionFactory.CreateConnection: INDXSQLiteConnection;
begin
  Result := TNDXSQLiteConnection.Create(FDefaultOptions);
end;

function TNDXSQLiteConnectionFactory.CreateConnectionWithOptions(
  AOptions: TNDXSQLiteConnectionOptions): INDXSQLiteConnection;
begin
  Result := TNDXSQLiteConnection.Create(AOptions);
end;

function TNDXSQLiteConnectionFactory.CreatePrimaryConnection: INDXSQLiteConnection;
var
  Opts: TNDXSQLiteConnectionOptions;
begin
  Opts := FDefaultOptions.Clone;
  try
    Opts.IsPrimaryConnection := True;
    Opts.DisableAutoClose := True;
    Result := TNDXSQLiteConnection.Create(Opts);
  finally
    Opts.Free;
  end;
end;

function TNDXSQLiteConnectionFactory.CreateMemoryConnection: INDXSQLiteConnection;
var
  Opts: TNDXSQLiteConnectionOptions;
begin
  Opts := FDefaultOptions.Clone;
  try
    Opts.MemoryDatabase := True;
    Opts.DatabasePath := ':memory:';
    Result := TNDXSQLiteConnection.Create(Opts);
  finally
    Opts.Free;
  end;
end;

function TNDXSQLiteConnectionFactory.CreateReadOnlyConnection: INDXSQLiteConnection;
var
  Opts: TNDXSQLiteConnectionOptions;
begin
  Opts := FDefaultOptions.Clone;
  try
    Opts.ReadOnly := True;
    Result := TNDXSQLiteConnection.Create(Opts);
  finally
    Opts.Free;
  end;
end;

function TNDXSQLiteConnectionFactory.CreateAsyncConnection: INDXSQLiteAsyncConnection;
begin
  Result := TNDXSQLiteAsyncConnection.Create(FDefaultOptions);
end;

end.
