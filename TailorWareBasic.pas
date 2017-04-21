{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TailorWareBasic;

interface

uses
  TSLib, TSLibColors, TSLibConvert, TSLibDateTime, TSLibFiles, TSLibObjects, 
  TSLibResources, TSNetAccess, TSResources, TSLibGraphics, TSLibHelpers, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('TailorWareBasic', @Register);
end.
