{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TailorWareDB;

interface

uses
  TS_DBStateLabel, TSLibDB, TS_DBGrid, TS_DBGridOptionsForm, TSClassesDB, 
  TSLibMSOffice, TSLibNAV, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TS_DBStateLabel', @TS_DBStateLabel.Register);
  RegisterUnit('TS_DBGrid', @TS_DBGrid.Register);
end;

initialization
  RegisterPackage('TailorWareDB', @Register);
end.
