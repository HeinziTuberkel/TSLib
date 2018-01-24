unit TS_SpeedButton;

{$mode objfpc}{$H+}

interface

uses
  Graphics, BCButton, Controls,
	Classes, SysUtils;


type

	{ TTSSpeedButton }

  TTSSpeedButton = class(TBCButton)
	private
		fAllowAllUp: Boolean;
		fDownGlyph: TBitmap;
		fGlyph: TBitmap;
		fGroupIndex: Integer;
		function GetDown: Boolean;
		procedure SetAllowAllUp(const aValue: Boolean);
		procedure SetDown(const aValue: Boolean);
		procedure SetDownGlyph(AValue: TBitmap);
		procedure SetGlyph(AValue: TBitmap);
		procedure SetGroupIndex(const aValue: Integer);
    procedure SetInheritedDown(aValue: Boolean);
  protected
		procedure Loaded; override;
		procedure UpdateExclusive; virtual;
  public
    constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Click; override;
  published
    property Down: Boolean read GetDown write SetDown default False;
		property AllowAllUp: Boolean read fAllowAllUp write SetAllowAllUp default false;
		property GroupIndex: Integer read fGroupIndex write SetGroupIndex default 0;
		property GlyphDown: TBitmap read fDownGlyph write SetDownGlyph;
    property Glyph: TBitmap read fGlyph write SetGlyph;
	end;

procedure Register;

implementation

uses
  LResources,
  CompanyConstants;

procedure Register;
begin
	{$I TS_SpeedButton.lrs}
  RegisterComponents(DefaultComponentPage, [TTSSpeedButton]);
end;

{ TTSSpeedButton }


constructor TTSSpeedButton.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	fGlyph := TBitmap.Create;
	fDownGlyph := TBitmap.Create;
end;

destructor TTSSpeedButton.Destroy;
begin
	FreeAndNIL(fGlyph);
  FreeAndNIL(fDownGlyph);
	inherited Destroy;
end;

procedure TTSSpeedButton.SetAllowAllUp(const aValue: Boolean);
begin
  if aValue <> fAllowAllUp then
  begin
		FAllowAllUp := aValue;
  	UpdateExclusive;
  end;
end;

function TTSSpeedButton.GetDown: Boolean;
begin
	Result := inherited Down;
end;

procedure TTSSpeedButton.SetDown(const aValue: Boolean);
begin
	if aValue <> inherited Down then
  begin
		SetInheritedDown(aValue);
	  UpdateExclusive;
	end;
end;

procedure TTSSpeedButton.SetInheritedDown(aValue: Boolean);
begin
  inherited Down := aValue;
	if aValue and not fDownGlyph.Empty then
		inherited Glyph := fDownGlyph
	else
		inherited Glyph := fGlyph;
end;

procedure TTSSpeedButton.Loaded;
begin
	inherited Loaded;
	SetInheritedDown(Down);
end;

procedure TTSSpeedButton.Click;
begin
  if GroupIndex <> 0 then
	  Down := not Down;
	inherited Click;
end;

procedure TTSSpeedButton.SetDownGlyph(AValue: TBitmap);
begin
  if (fDownGlyph <> nil) and (fDownGlyph = AValue) then
    exit;
	fDownGlyph.Assign(AValue);

	if Down and not fDownGlyph.Empty then
		inherited Glyph := AValue
	else
		inherited Glyph := fGlyph;
end;

procedure TTSSpeedButton.SetGlyph(AValue: TBitmap);
begin
  if (fGlyph <> nil) and (fGlyph = AValue) then
  	exit;
  fGlyph.Assign(AValue);
	if not Down or fDownGlyph.Empty then
		inherited Glyph := AValue
	else
		inherited Glyph := fDownGlyph;
end;

procedure TTSSpeedButton.SetGroupIndex(const aValue: Integer);
begin
  if aValue <> fGroupIndex then
  begin
    FGroupIndex := aValue;
    UpdateExclusive;
	end;
end;

procedure TTSSpeedButton.UpdateExclusive;
var
  I: Integer;
  C: TControl;
  B: TTSSpeedButton;
  FoundDown: Boolean;

  procedure SetGroupMember;
  begin
    if Down then
    	B.SetInheritedDown(False)
  	else if not fAllowAllUp then
	    FoundDown := FoundDown or B.Down;
	end;
begin
	if (GroupIndex<>0) and Assigned(Parent) and not (csLoading in ComponentState) then
  begin
    FoundDown := False;
    for I := 0 to pred(Parent.ControlCount) do
    begin
      C := Parent.Controls[I];
      if (C is TTSSpeedButton) and (C <> Self) then
      begin
        B := TTSSpeedButton(C);
        if B.GroupIndex = self.GroupIndex then
        begin
					SetGroupMember;
          B.fAllowAllUp := fAllowAllUp;
				end;
			end;
		end;
    if not (Down or AllowAllUp or FoundDown) then
    	SetInheritedDown(True);
	end;
end;

end.

