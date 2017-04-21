unit TS_GroupBox;

{$mode objfpc}{$H+}

interface

uses
  LResources,
 	Classes,
  SysUtils,
  StdCtrls;


type
	//***************************************************************************************
	{ TTSGroupBox }
	//***************************************************************************************
	TTSGroupBox = class(TGroupBox)
	private
	protected
	public
	protected
	end;

procedure Register;

implementation

uses
  CompanyConstants,
  TSLib;

//***************************************************************************************
procedure Register;
begin
  {$I TS_GroupBox.lrs}
  RegisterComponents(DefaultComponentPage, [TTSGroupBox]);
end;



end.

