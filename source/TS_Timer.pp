unit TS_Timer;

{$mode objfpc}{$H+}

interface

uses
  PropEdits,
  ComponentEditors,
  TSClasses,
  ExtCtrls,
	Classes, SysUtils;

type
  TTSTimer = class(TTimer)
  private
    fAfterChangeEnabled: TNotifyEvent;
    fBeforeChangeEnabled: MNotifyEventAsk;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    procedure Restart;
  published
    property BeforeChangeEnabled: MNotifyEventAsk read fBeforeChangeEnabled write fBeforeChangeEnabled;
    property AfterChangeEnabled: TNotifyEvent read fAfterChangeEnabled write fAfterChangeEnabled;
  end;

  TTSTimerEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop: TPropertyEditor; var Continue: Boolean); override;
  end;


procedure Register;


implementation

uses
  LResources,
  CompanyConstants;

procedure Register;
begin
	{$I TS_Timer.lrs}
  RegisterComponents(DefaultComponentPage, [TTSTimer]);
end;


//***************************************************************************************
{ TTSTimer }
//***************************************************************************************

//***************************************************************************************
procedure TTSTimer.Restart;
begin
  inherited Enabled := False;
  inherited Enabled := True;
end;

//***************************************************************************************
procedure TTSTimer.SetEnabled(Value: Boolean);
begin
  if Assigned(fBeforeChangeEnabled) then
    fBeforeChangeEnabled(Self, Value);
	inherited SetEnabled(Value);
  if Assigned(fAfterChangeEnabled) then
    fAfterChangeEnabled(Self);
end;

//***************************************************************************************
{ TTSTimerEditor }
//***************************************************************************************

//***************************************************************************************
procedure TTSTimerEditor.EditProperty(const Prop: TPropertyEditor; var Continue: Boolean);
begin
  if SameText(Prop.GetName, 'OnTimer') then
    inherited;
end;

end.

