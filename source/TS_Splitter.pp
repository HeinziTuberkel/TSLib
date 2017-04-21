unit TS_Splitter;

{$mode objfpc}{$H+}

interface

uses
	TS_CustomINI,
	ExtCtrls,
	Messages,
	Controls,
	TSLibColors,
	Classes,
	SysUtils;


const
	CM_RestoreSplitter = WM_User + 129;

type
  //Definiert die Sichtbarkeit der "Jump"-Buttons auf dem TTSSplitter
	NTSSplitterButton = (sbHidden, sbDownLeft, sbUpRight, sbBoth);


  //***************************************************************************************
  { TTSSplitter }
  //***************************************************************************************
	TTSSplitter = class(TSplitter)
	private
		fButton: NTSSplitterButton;
		fDoStore: Boolean;
		fIsHighlighted: Boolean;
		fStorage: TTSCustomINI;
		procedure WMRestoreSplitter(var Message: TMessage); message CM_RestoreSplitter;
	protected
		procedure Loaded; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure SavePosition;
		procedure RestorePosition;
	published
		property Color default clTSSplitter;
		property ResizeStyle default rsUpdate;
		property ParentColor default False;
		property Width default 7;
    property Storage: TTSCustomINI read fStorage write fStorage;
    property StorePosition: Boolean read fDoStore write fDoStore default True;
	end;

procedure Register;

implementation

uses
	TSLib,
	TS_Application,
  LResources,
  CompanyConstants;

procedure Register;
begin
  {$I TS_Splitter.lrs}
  RegisterComponents(DefaultComponentPage, [TTSSplitter]);
end;


//***************************************************************************************
//***************************************************************************************
{ TTSSplitter }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSSplitter.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	fIsHighlighted := False;
	Color := clTSSplitter;
	ResizeStyle := rsUpdate;
	ParentColor := False;
	Width := 7;
	fButton := sbBoth;
	fDoStore := True;
end;

//***************************************************************************************
destructor TTSSplitter.Destroy;
begin
	if StorePosition
		and not (csDesigning in ComponentState)
		and Assigned(Parent)
	then
		SavePosition;
	inherited;
end;

//***************************************************************************************
procedure TTSSplitter.Loaded;
begin
	inherited;
	if StorePosition
		and not (csDesigning in ComponentState)
		and Assigned(Parent)
	then
		RestorePosition;
end;

//***************************************************************************************
procedure TTSSplitter.SavePosition;
var
	Conf: TTSCustomINI;
	ConfOwner: string;
begin
	if csDesigning in ComponentState then
  	Exit;
  if Assigned(fStorage) then
  	Conf := fStorage
  else
    Conf := TSApp.INI;
  if not Assigned(Conf) then
    Exit;
  if Assigned(Owner) then
	  ConfOwner := Owner.Name + Conf.LookupSeparators.Group
  else
	  ConfOwner := '(NoOwner)' + Conf.LookupSeparators.Group;

	Conf.Int[ConfOwner + Name] := GetSplitterPosition;
end;

//***************************************************************************************
procedure TTSSplitter.WMRestoreSplitter(var Message: TMessage);
begin
	RestorePosition;
end;

//***************************************************************************************
procedure TTSSplitter.RestorePosition;
var
	Conf: TTSCustomINI;
	ConfOwner: string;
	NewPos: Integer;
begin
	if csDesigning in ComponentState then
  	Exit;
  if Assigned(fStorage) then
  	Conf := fStorage
  else
    Conf := TSApp.INI;
  if not Assigned(Conf) then
    Exit;
  if Assigned(Owner) then
	  ConfOwner := Owner.Name + Conf.LookupSeparators.Group
  else
	  ConfOwner := '(NoOwner)' + Conf.LookupSeparators.Group;

	NewPos := Conf.Int[ConfOwner + Name];
	if NewPos >= MinSize then
		SetSplitterPosition(NewPos);
end;


end.

