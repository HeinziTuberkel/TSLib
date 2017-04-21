unit TS_Panel;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  TSLibGraphics,
  Graphics,
  TSClasses,
  LMessages,
	ExtCtrls, Classes, SysUtils;

type
	//***************************************************************************************
	{ TTSCustomPanel }
	//***************************************************************************************
	TTSCustomPanel = class(TCustomPanel)
	private
		fStyle: TTSCaptionRect;
		fDownEnabled: Boolean;
		fWasDown: NTSBorderStyle;
		procedure SetDown(const Value: Boolean);
		function GetDown: Boolean;
		procedure PropChanged(Sender: TObject; PropID: Integer);
    function IsPropStored(Sender: TObject; PropID: Integer; StoredDefault: Boolean): Boolean;
	protected
		procedure Click; override;
		procedure Paint; override;
    procedure TextChanged; override;
    procedure SetColor(Value: TColor); override;
    procedure FontChanged(Sender: TObject); override;
		procedure AdjustClientRect(var Rect: TRect); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	protected
    property DisplayStyle: TTSCaptionRect read fStyle write fStyle;
		property Down: Boolean read GetDown write SetDown default False;
		property DownEnabled: Boolean read fDownEnabled write fDownEnabled default False;
		property ParentFont default True;
		property ParentColor default True;
	end;

	//***************************************************************************************
	{ TTSPanel }
	//***************************************************************************************
	TTSPanel = class(TTSCustomPanel)
	published
    property DisplayStyle;
    property Down;
    property DownEnabled;

		property Align;
		property Alignment;
		property Anchors;
		property AutoSize;
		property BorderSpacing;
		property BevelInner;
		property BevelOuter;
		property BevelWidth;
		property BidiMode;
		property BorderWidth;
		property BorderStyle;
		property Caption;
		property ChildSizing;
		property ClientHeight;
		property ClientWidth;
		property Color;
		property Constraints;
		property DockSite;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property Font;
		property FullRepaint;
		property ParentBidiMode;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property UseDockManager default True;
		property Visible;
		property OnClick;
		property OnContextPopup;
		property OnDockDrop;
		property OnDockOver;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnGetSiteInfo;
		property OnGetDockCaption;
		property OnMouseDown;
		property OnMouseEnter;
		property OnMouseLeave;
		property OnMouseMove;
		property OnMouseUp;
		property OnResize;
		property OnStartDock;
		property OnStartDrag;
		property OnUnDock;
	end;

procedure Register;

implementation


uses
  CompanyConstants,
  TSLib;

//***************************************************************************************
procedure Register;
begin
  {$I TS_Panel.lrs}
  RegisterComponents(DefaultComponentPage, [TTSPanel]);
end;

//***************************************************************************************
//***************************************************************************************
{ TTSCustomPanel }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSCustomPanel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

  fStyle := TTSCaptionRect.Create(Self);
  fStyle.OnPropChanged := @PropChanged;
  fStyle.OnIsPropStored := @IsPropStored;
  with fStyle do
  begin
  	BeginUpdate;
    try
			Font := Self.Font;
      Caption := Self.Caption;
			BorderStyle := brdrBump;
      BorderWidth := 2;
      BevelSides := bvlallSides;
      BevelWidth := 1;
      Background.Color := self.Color;
		finally
      EndUpdate;
		end;
	end;
	ParentColor := True;
	ParentFont := True;
	fDownEnabled := False;
	Caption := '';
	fWasDown := fStyle.BorderStyle;
end;

//***************************************************************************************
destructor TTSCustomPanel.Destroy;
begin
	fStyle.Free;
	inherited Destroy;
end;

//***************************************************************************************
procedure TTSCustomPanel.AdjustClientRect(var Rect: TRect);
begin
	inherited AdjustClientRect(Rect);
	ShrinkRect(Rect, fStyle.BevelSides, fStyle.BorderWidth);
end;

//***************************************************************************************
procedure TTSCustomPanel.Click;
begin
	Down := not Down;
	inherited Click;
end;

//***************************************************************************************
procedure TTSCustomPanel.Paint;
var
  PaintArea: TRect;
  TS : TTextStyle;
begin
  PaintArea := GetClientRect;
	fStyle.Paint(Self.Canvas, PaintArea);
end;

//***************************************************************************************
procedure TTSCustomPanel.TextChanged;
begin
	inherited TextChanged;
  if Assigned(fStyle) then
	  fStyle.Caption := Caption;
end;

//***************************************************************************************
procedure TTSCustomPanel.SetColor(Value: TColor);
begin
	inherited SetColor(Value);
  if Assigned(fStyle) then
	  fStyle.Background.Color := Value;
end;

//***************************************************************************************
function TTSCustomPanel.GetDown: Boolean;
begin
	Result := fStyle.BorderStyle = brdrLowered;
end;

//***************************************************************************************
procedure TTSCustomPanel.PropChanged(Sender: TObject; PropID: Integer);
begin
	case PropID of
  	propIDCaption:
      if Caption <> fStyle.Caption then
	      Caption := fStyle.Caption;
    propIDColor:
      if Color <> fStyle.Background.Color then
	      Color := fStyle.Background.Color;
    propIDFont:
      Font := fStyle.Font;
    propIDBevelWidth:
      if BevelWidth <> fStyle.BevelWidth then
	      BevelWidth := fStyle.BevelWidth;
    propIDBorderWidth:
      if BorderWidth <> fStyle.BorderWidth then
	      BorderWidth := fStyle.BorderWidth;
    propIDUndefined: begin
      if Caption <> fStyle.Caption then
	      Caption := fStyle.Caption;
      if Color <> fStyle.Background.Color then
	      Color := fStyle.Background.Color;
      Font := fStyle.Font;
      if BorderWidth <> fStyle.BorderWidth then
	      BorderWidth := fStyle.BorderWidth;
		end;
	end;
  Invalidate;
end;

//***************************************************************************************
function TTSCustomPanel.IsPropStored(Sender: TObject; PropID: Integer; StoredDefault: Boolean): Boolean;
begin
	case PropID of
    propIDColor:
      Result := not ParentColor;
    propIDFont:
      Result := not ParentFont;
  else
    Result := StoredDefault;
	end;
end;

//***************************************************************************************
procedure TTSCustomPanel.SetDown(const Value: Boolean);
begin
  if Assigned(fStyle) and DownEnabled then
	begin
		if Value then
    begin
			fWasDown := fStyle.BorderStyle;
			fStyle.BorderStyle := brdrLowered;
		end
		else
			fStyle.BorderStyle := fWasDown;
		Invalidate;
	end;
end;

//***************************************************************************************
procedure TTSCustomPanel.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  if Assigned(fStyle) then
	  fStyle.Font.Assign(Font);
end;




end.

