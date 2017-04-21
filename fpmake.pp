{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for TailorWareDB 0.5.0.1

   This file was generated on 20.03.2017
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_TailorWareDB(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('tailorwaredb');
    P.Version:='0.5.0.1';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('sqldblaz');
    P.Dependencies.Add('zcomponent');
    P.Dependencies.Add('tailorware');
    P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('source');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('TailorWareDB.pas');
    t.Dependencies.AddUnit('TS_DBStateLabel');
    t.Dependencies.AddUnit('TSLibDB');
    t.Dependencies.AddUnit('TS_DBGrid');
    t.Dependencies.AddUnit('TS_DBGridOptionsForm');
    t.Dependencies.AddUnit('TSClassesDB');
    t.Dependencies.AddUnit('TSLibMSOffice');
    t.Dependencies.AddUnit('TSLibNAV');

    T:=P.Targets.AddUnit('source\TS_DBStateLabel.pp');
    T:=P.Targets.AddUnit('source\TSLibDB.pp');
    T:=P.Targets.AddUnit('source\TS_DBGrid.pp');
    T:=P.Targets.AddUnit('source\TS_DBGridOptionsForm.pp');
    T:=P.Targets.AddUnit('source\TSClassesDB.pp');
    T:=P.Targets.AddUnit('source\TSLibMSOffice.pp');
    T:=P.Targets.AddUnit('source\TSLibNAV.pp');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('TailorWareDB.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_TailorWareDB('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
