unit TS_PropertySaver;

{$mode objfpc}{$H+}

interface

uses
	TS_CustomINI,
	contnrs,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TTSComponentPropertySaver = class;

	{ TTSPropertySaver }

	TTSPropertySaver = class(TComponent)
	private
		fCustomINI: TTSCustomINI;
		fSavers: TObjectList;
		function GetManagedComponents: TStrings;
 public
		procedure SavePropValues;
		procedure LoadPropValues;
		procedure RegisterSaver(Saver: TTSComponentPropertySaver);
		procedure UnregisterSaver(Saver: TTSComponentPropertySaver);
		function IsManaged(Saver: TTSComponentPropertySaver): Boolean;
		property ManagedComponentSavers: TObjectList read fSavers;

	published
  	property INI: TTSCustomINI read fCustomINI write fCustomINI;
  	property ManagedComponents: TStrings read GetManagedComponents;

	end;


	{ TTSComponentPropertySaver }

  TTSComponentPropertySaver = class(TPersistent)
  private
		fActive: Boolean;
		fOwner: TComponent;
		fUseGlobalSaver: Boolean;
  protected
  public
  	constructor Create(AOwner: TComponent);
  	function UsingGlobalSaver: Boolean;
  	property Owner: TComponent read fOwner;

  published
  	property Active: Boolean read fActive write fActive;
  	property UseGlobalSaver: Boolean read fUseGlobalSaver write fUseGlobalSaver;
  end;



implementation

{ TTSComponentPropertySaver }

constructor TTSComponentPropertySaver.Create(AOwner: TComponent);
begin

end;

function TTSComponentPropertySaver.UsingGlobalSaver: Boolean;
begin

end;


{ TTSPropertySaver }

function TTSPropertySaver.GetManagedComponents: TStrings;
begin

end;

procedure TTSPropertySaver.SavePropValues;
begin

end;

procedure TTSPropertySaver.LoadPropValues;
begin

end;

procedure TTSPropertySaver.RegisterSaver(Saver: TTSComponentPropertySaver);
begin
	if not IsManaged(Saver) then
		fSavers.Add(Saver);
end;

procedure TTSPropertySaver.UnregisterSaver(Saver: TTSComponentPropertySaver);
begin
	fSavers.Remove(Saver);
end;

function TTSPropertySaver.IsManaged(Saver: TTSComponentPropertySaver): Boolean;
begin
  Result := fSavers.IndexOf(Saver) >= 0;
end;

end.
