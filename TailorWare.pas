{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TailorWare;

interface

uses
  TSClasses, TS_CustomINI, TS_FileINI, TS_StructuredFileName, TS_Application, 
  TS_Forms, TS_Debug, CompanyConstants, TS_Edit, TS_RegINI, 
  TS_StringsContainer, TS_Timer, TS_Panel, TS_PropertyEditors, TSDefaults, 
  TS_GroupBox, TS_Splitter, TS_SpeedButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TS_FileINI', @TS_FileINI.Register);
  RegisterUnit('TS_Application', @TS_Application.Register);
  RegisterUnit('TS_Debug', @TS_Debug.Register);
  RegisterUnit('TS_Edit', @TS_Edit.Register);
  RegisterUnit('TS_RegINI', @TS_RegINI.Register);
  RegisterUnit('TS_StringsContainer', @TS_StringsContainer.Register);
  RegisterUnit('TS_Timer', @TS_Timer.Register);
  RegisterUnit('TS_Panel', @TS_Panel.Register);
  RegisterUnit('TS_PropertyEditors', @TS_PropertyEditors.Register);
  RegisterUnit('TS_GroupBox', @TS_GroupBox.Register);
  RegisterUnit('TS_Splitter', @TS_Splitter.Register);
  RegisterUnit('TS_SpeedButton', @TS_SpeedButton.Register);
end;

initialization
  RegisterPackage('TailorWare', @Register);
end.
