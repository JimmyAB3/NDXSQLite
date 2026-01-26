{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NDXSQLite;

{$warn 5023 off : no warning about unused units}
interface

uses
  ndxsqlite3api, ndxsqlitetypes, ndxsqliteexceptions, ndxsqliteplatform, 
  ndxsqliteconnectionoptions, ndxsqliteconnectionintf, ndxsqliteconnection, 
  ndxsqlitedatabase, ndxsqlitestatement, ndxsqlitedataset, ndxsqlitequery, 
  ndxsqliteasynctypes, ndxsqlitecancellation, ndxsqliteasyncworker, 
  ndxsqliteasyncconnection, ndxsqliteconnectionfactory, 
  ndxsqliteconnectionpool, ndxsqlitehealthcheck, ndxsqlitefts, ndxsqlitejson, 
  ndxsqlitemigration, ndxsqlitebackup, ndxsqliteverify, ndxsqlitedump, 
  ndxsqlitecsv, ndxsqliteattached, ndxsqliteudf, ndxsqlitecollation, 
  ndxsqliteblob, ndxsqliteertree, ndxsqlitevtab, ndxsqliteauthorizer, 
  ndxsqliteunlocknotify, ndxsqlitepreupdate, ndxsqlitedatetimeutils, 
  ndxsqliteversion, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('NDXSQLite', @Register);
end.
