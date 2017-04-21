unit TSClasses;

{$mode objfpc}{$H+}
{$I PackageDefs.inc}

interface

uses
{$IFDEF BGRA}
	BCBaseCtrls,
	BCTypes,
{$ENDIF}
	TSLibGraphics,
	Graphics,
	Controls,
	Classes, SysUtils;

type
  NTSTrigger = (trgMouse, trgKeyboard);
  STSTriggers = set of NTSTrigger;

  MNotifyEventAsk = procedure (Sender: TObject; var Value: Boolean) of object;
  MIsPropStored = function (Sender: TObject; PropID: Integer; StoredDefault: Boolean): Boolean of object;
  MPropChanged = procedure (Sender: TObject; PropID: Integer) of object;

	//***************************************************************************************
  { ETSException }
	//***************************************************************************************

  ETSException = class(Exception)
  private
    fClassName: string;
    fOwnerName: string;
    fCompName: string;
  public
    constructor Create(const ExClassName, Msg: string); overload;
    constructor Create(const ExClassName, OwnerName, CompName, Msg: string); overload;
    constructor Create(EObject: TObject; const Msg: string); overload;
    property ExceptionClassName: string read fClassName;
    property OwnerName: string read fOwnerName;
    property CompName: string read fCompName;
  end;

	//***************************************************************************************
  { TTSPersistent }
	//***************************************************************************************

	TTSPersistent = class(TPersistent)
  private
    fOwner: TComponent;
    fUpdating: Integer;
		fIsPropStored: MIsPropStored;
		fPropChanged: MPropChanged;
		fEndUpdate: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure BeginUpdate;
    function EndUpdate: Boolean;
    function Updating: Boolean;
    procedure PropChanged(PropID: Integer=0); virtual;
    function PropIsStored(PropID: Integer; StoredDefault: Boolean=True): Boolean; virtual;
    property Owner: TComponent read fOwner;
	  property OnIsPropStored: MIsPropStored read fIsPropStored write fIsPropStored;
  	property OnPropChanged: MPropChanged read fPropChanged write fPropChanged;
  	property OnEndUpdate: TNotifyEvent read fEndUpdate write fEndUpdate;
  end;

	//***************************************************************************************
  { TTSTrigger }
	//***************************************************************************************

  TTSTrigger = class(TPersistent)
  private
    fButton: TMouseButton;
    fShift: TShiftState;
    fShortCut: TShortCut;
    fTriggers: STSTriggers;
  public
    function Fired(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; overload; virtual;
    function Fired(var Key: Word; Shift: TShiftState; ClearKey: Boolean=True): Boolean; overload; virtual;
  published
    property ActiveTriggers: STSTriggers read fTriggers write fTriggers;
    property MouseButton: TMouseButton read fButton write fButton;
    property MouseShift: TShiftState read fShift write fShift;
    property ShortCut: TShortCut read fShortCut write fShortCut;
  end;



	//***************************************************************************************
  { TTSBackground }
	//***************************************************************************************
{$IFDEF BGRA}
	TTSBackground = class(TBCBackground)
  private
		BCBmp: TBGRABitmapEx;
  protected
    procedure Paint(Canvas: TCanvas; Area: TRect); virtual;
    procedure Change(AData: PtrInt = 0); override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
	end;

{$ELSE}
	TTSBackground = class(TTSPersistent)
	private
		fColor: TColor;
		fParentColor: Boolean;
		function GetColor: TColor;
  function IsStoredColor: Boolean;
		function IsStoredParentColor: Boolean;
   procedure SetColor(AValue: TColor);
	 procedure SetParentColor(AValue: Boolean);
  public
    constructor Create(AOwner: TControl); reintroduce;
  published
		property ParentColor: Boolean read fParentColor write SetParentColor stored IsStoredParentColor;
    property Color: TColor read GetColor write SetColor stored IsStoredColor;
	end;

{$ENDIF}

	//***************************************************************************************
  { TSCustomRect }
	//***************************************************************************************

	{ TTSCustomRect }

  TTSCustomRect = class(TTSPersistent)
	private
		fBackground: TTSBackground;
		fBevelSides: SBevelSides;
		fBevelWidth: Integer;
		fBorderClr: TColor;
		fBorderStyle: NTSBorderStyle;
		fBorderWidth: Integer;
    fCaption: string;
		fFont: TFont;
		fParentFont: Boolean;
		fTxtAlign: NTxtAlignPos;
		function GetFont: TFont;
		function IsStoredParentFont: Boolean;
		procedure FontChanged(Sender: TObject);

  procedure SetBackground(AValue: TTSBackground);
		procedure SetBevelSides(AValue: SBevelSides);
		procedure SetBevelWidth(AValue: Integer);
		procedure SetBorderColor(AValue: TColor);
		procedure SetBorderStyle(AValue: NTSBorderStyle);
		procedure SetBorderWidth(AValue: Integer);
		procedure SetCaption(AValue: string);
		procedure SetFont(AValue: TFont);
		procedure SetParentFont(AValue: Boolean);
		procedure SetTxtAlign(AValue: NTxtAlignPos);

		function IsStoredBevelSides: Boolean;
		function IsStoredBevelWidth: Boolean;
		function IsStoredBorderColor: Boolean;
		function IsStoredBorderStyle: Boolean;
		function IsStoredBorderWidth: Boolean;
		function IsStoredCaption: Boolean;
		function IsStoredFont: Boolean;
		function IsStoredTextAlign: Boolean;

{$IFDEF BGRA}
    procedure BCPropChanged(ASender: TObject; AData: PtrInt);
{$ELSE}
    function IsStoredBackground(Sender: TObject; PropID: Integer; StoredDefault: Boolean): Boolean;
    procedure BackgroundChanged(Sender: TObject; PropID: Integer);
{$ENDIF}
  public
    constructor Create(AOwner: TControl); reintroduce;
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; Area: TRect); virtual;
  protected
    property Caption: string read fCaption write SetCaption stored IsStoredCaption;
		property Font: TFont read GetFont write SetFont stored IsStoredFont;
		property ParentFont: Boolean read fParentFont write SetParentFont stored IsStoredParentFont;
		property TextAlign: NTxtAlignPos read fTxtAlign write SetTxtAlign stored IsStoredTextAlign;
		property BorderColor: TColor read fBorderClr write SetBorderColor stored IsStoredBorderColor;
		property BorderStyle: NTSBorderStyle read fBorderStyle write SetBorderStyle stored IsStoredBorderStyle;
		property BevelWidth: Integer read fBevelWidth write SetBevelWidth stored IsStoredBevelWidth;
		property BevelSides: SBevelSides read fBevelSides write SetBevelSides stored IsStoredBevelSides;
		property BorderWidth: Integer read fBorderWidth write SetBorderWidth stored IsStoredBorderWidth;
    property Background: TTSBackground read fBackground write SetBackground;
	end;

  TTSRect = class(TTSCustomRect)
  published
		property BorderColor;
		property BorderStyle;
		property BevelWidth;
		property BevelSides;
		property BorderWidth;
    property Background;
  end;

  TTSTextRect = class(TTSCustomRect)
  published
		property Font;
		property TextAlign;
		property BorderColor;
		property BorderStyle;
		property BorderWidth;
		property BevelSides;
		property BevelWidth;
    property Background;
  end;

  TTSCaptionRect = class(TTSTextRect)
  published
  	property Caption;
  end;


  //***************************************************************************************
  { TTSTextProperties }
  //***************************************************************************************
	TTSTextProperties = class(TTSPersistent)
	private
		fCaption: TCaption;
		fLayout: TTextLayout;
		fParentColor: Boolean;
		fParentFont: Boolean;
		fWordWrap: Boolean;
		fVisible: Boolean;
		fTransparent: Boolean;
		fAlignment: TAlignment;
		fColor: TColor;
		fFont: TFont;
		function GetColor: TColor;
		function GetFont: TFont;

		function IsStoredAlignment: Boolean;
		function IsStoredCaption: Boolean;
	  function IsStoredColor: Boolean;
		function IsStoredFont: Boolean;
		function IsStoredLayout: Boolean;
		function IsStoredParentColor: Boolean;
		function IsStoredParentFont: Boolean;
		function IsStoredTransparent: Boolean;
		function IsStoredVisible: Boolean;
		function IsStoredWordWrap: Boolean;

  	procedure SetCaption(const Value: TCaption);
		procedure SetFont(const Value: TFont);
		procedure SetAlignment(const Value: TAlignment);
		procedure SetColor(const Value: TColor);
		procedure SetLayout(AValue: TTextLayout);
		procedure SetParentColor(AValue: Boolean);
		procedure SetParentFont(AValue: Boolean);
		procedure SetTransparent(const Value: Boolean);
		procedure SetVisible(const Value: Boolean);
		procedure SetWordWrap(const Value: Boolean);
		procedure FontChanged(Sender: TObject);
	public
		constructor Create(AOwner: TControl); reintroduce;
    destructor Destroy; override;
		procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
	published
		property Color: TColor read GetColor write SetColor stored IsStoredColor;
		property Caption: TCaption read fCaption write SetCaption stored IsStoredCaption;
		property Font: TFont read GetFont write SetFont stored IsStoredFont;
		property Visible: Boolean read fVisible write SetVisible stored IsStoredVisible;
		property Transparent: Boolean read fTransparent write SetTransparent stored IsStoredTransparent;
		property WordWrap: Boolean read fWordWrap write SetWordWrap stored IsStoredWordWrap;
		property Alignment: TAlignment read fAlignment write SetAlignment stored IsStoredAlignment;
		property Layout: TTextLayout read fLayout write SetLayout stored IsStoredLayout;

    property ParentColor: Boolean read fParentColor write SetParentColor stored IsStoredParentColor;
    property ParentFont: Boolean read fParentFont write SetParentFont stored IsStoredParentFont;
	end;


const
//Property IDs for TTSPersistent and descendants.
  propIDUndefined = 0;
  propIDVisible = 1;
  propIDTransparent = 2;
  propIDColor = 3;
  propIDParentColor = 4;
  propIDCaption = 5;
  propIDFont = 6;
  propIDParentFont = 7;
  propIDTextAlign = 8;
  propIDTextLayout = 9;
  propIDTextOrientation = 10;
  propIDWordWrap = 11;
  propIDBorderColor = 12;
  propIDBorderStyle = 13;
  propIDBevelWidth = 14;
  propIDBevelSides = 15;
  propIDBorderWidth = 16;
  propIDBackground = 17;


implementation


uses
  Menus,
	TSLibObjects,
	TSDefaults,
	//Put common units here. End list with comma for the following "uses" in the conditional parts.

//***************************************************************************************
//***************************************************************************************
{ TSBackground }
//***************************************************************************************
//***************************************************************************************
{$IFDEF BGRA}
	BGRABitmap,
	BGRABitmapTypes,
	BCTools,
	StdCtrls; //Ends the "uses" clause from above the conditional compilation.

procedure TTSBackground.Paint(Canvas: TCanvas; Area: TRect);
var
  W, H: Integer;
begin
  W := Area.Right - Area.Left;
  H := Area.Bottom - Area.Top;
	if BCBmp.NeedRender or (W <> BCBmp.Width) or (H<>BCBmp.Height) then
  begin
    BCBmp.NeedRender := False;
    BCBmp.SetSize(W, H);
    BCBmp.Fill(BGRAPixelTransparent);
    RenderBackground(BCBmp.ClipRect, Self, TBGRABitmap(BCBmp));
	end;
	BCBmp.Draw(Canvas, Area.Left, Area.Top);
end;

procedure TTSBackground.Change(AData: PtrInt);
begin
	inherited Change(AData);
  BCBmp.NeedRender := True;
end;

constructor TTSBackground.Create(AControl: TControl);
begin
	inherited Create(AControl);
	BCBmp := TBGRABitmapEx.Create;
end;

destructor TTSBackground.Destroy;
begin
  BCBmp.Free;
	inherited Destroy;
end;

{$ELSE}

	StdCtrls; //Ends the "uses" clause from above the conditional compilation.

constructor TTSBackground.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  fParentColor := tsDefBckParentColor;
  fColor := tsDefBckColor;
end;

procedure TTSBackground.SetColor(AValue: TColor);
begin
	fParentColor := False;
	fColor  := AValue;
	PropChanged(propIDColor);
end;

procedure TTSBackground.SetParentColor(AValue: Boolean);
begin
	fParentColor := AValue;
  PropChanged(propIDParentColor);
  //if AValue and Assigned(Owner) then
  //	Color := (Owner as TControl).Color;
end;

function TTSBackground.IsStoredColor: Boolean;
begin
	Result := PropIsStored(propIDColor, (fParentColor=False) and (fColor <> tsDefBckColor));
end;

function TTSBackground.GetColor: TColor;
begin
	if fParentColor and Assigned((Owner as TControl).Parent) then
		Result := (Owner as TControl).Parent.Color
  else
  	Result := fColor;
end;

function TTSBackground.IsStoredParentColor: Boolean;
begin
	Result := PropIsStored(propIDParentColor, fParentColor <> tsDefLbParentColor);
end;

{$ENDIF}

//***************************************************************************************
//***************************************************************************************
	{ TTSPersistent }
//***************************************************************************************
//***************************************************************************************

constructor TTSPersistent.Create(AOwner: TComponent);
begin
  if not Assigned(AOwner) then
		raise Exception.Create(ClassName + ' needs an Owner. Aborting creation.');
	inherited Create;
	fOwner := AOwner;
end;

procedure TTSPersistent.BeginUpdate;
begin
	inc(fUpdating);
end;

function TTSPersistent.EndUpdate: Boolean;
begin
  if fUpdating > 0 then
	  dec(fUpdating);
	Result := fUpdating<1;
  if Result and Assigned(fEndUpdate) then
  begin
  	fEndUpdate(Self);
    PropChanged;
	end;
end;

function TTSPersistent.Updating: Boolean;
begin
	Result := fUpdating>0;
end;

procedure TTSPersistent.PropChanged(PropID: Integer);
begin
	if not Updating and Assigned(fPropChanged) then
  	fPropChanged(Self, PropID);
end;

function TTSPersistent.PropIsStored(PropID: Integer; StoredDefault: Boolean
	): Boolean;
begin
	if Assigned(fIsPropStored) then
    Result := fIsPropStored(Self, PropID, StoredDefault)
  else
    Result := StoredDefault;
end;

//***************************************************************************************
//***************************************************************************************
{ TTSTrigger }
//***************************************************************************************
//***************************************************************************************

function TTSTrigger.Fired(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  case Button of
  	mbLeft: Exclude(Shift, ssLeft);
  	mbMiddle: Exclude(Shift, ssMiddle);
  	mbRight: Exclude(Shift, ssRight);
  end;
  Result := (trgMouse in ActiveTriggers)
  				and (Button = MouseButton)
  				and (Shift = MouseShift);
end;

function TTSTrigger.Fired(var Key: Word; Shift: TShiftState; ClearKey: Boolean): Boolean;
begin
  Result := (trgKeyboard in ActiveTriggers)
  					and (Menus.ShortCut(Key, Shift) = ShortCut);
  if Result and ClearKey then
  	Key := 0;
end;

//***************************************************************************************
//***************************************************************************************
{ TTSCustomRect }
//***************************************************************************************
//***************************************************************************************

constructor TTSCustomRect.Create(AOwner: TControl);
begin
	inherited Create(AOwner);
	fCaption := tsDefPnlCaption;
	fBevelSides := tsDefPnlBvlSides;
  fBevelWidth := tsDefPnlBvlWidth;

  fBorderStyle := tsDefPnlBorderStyle;
	fBorderWidth := tsDefPnlBorderWidth;

  fTxtAlign := tsDefPnlTextAlign;

	fFont := TFont.Create;
  if fParentFont then
		fFont.Assign(AOwner.Font)
	else
		fFont.Assign(tsDefFont);
	fFont.OnChange := @FontChanged;

  fBackground := TTSBackground.Create(AOwner);
{$IFDEF BGRA}
  fBackground.OnChange := @BCPropChanged;
{$ELSE}
  fBackground.OnIsPropStored := @IsStoredBackground;
  fBackground.OnPropChanged := @BackgroundChanged;
{$ENDIF}
end;

destructor TTSCustomRect.Destroy;
begin
  fBackground.Free;
  fFont.Free;
	inherited Destroy;
end;

procedure TTSCustomRect.Paint(Canvas: TCanvas; Area: TRect);
var
	TS : TTextStyle;
begin
  DrawBorder(Area, Canvas, BorderStyle, BevelSides, BorderColor, BorderWidth, BevelWidth);
{$IFDEF BGRA}
	fBackground.Paint(Canvas, Area);
{$ELSE}
	Canvas.Brush.Color := fBackground.Color;
	Canvas.FillRect(Area);
{$ENDIF}
  if Caption > '' then
  begin
    TS := Canvas.TextStyle;
    case TextAlign of
      tapTopLeft: begin
      	TS.Layout := tlTop;
        TS.Alignment := taLeftJustify;
			end;
      tapTopCenter: begin
      	TS.Layout := tlTop;
        TS.Alignment := taCenter;
			end;
      tapTopRight: begin
      	TS.Layout := tlTop;
        TS.Alignment := taRightJustify;
			end;
      tapCenterLeft: begin
      	TS.Layout := tlCenter;
        TS.Alignment := taLeftJustify;
			end;
      tapCenter: begin
      	TS.Layout := tlCenter;
        TS.Alignment := taCenter;
			end;
      tapCenterRight: begin
      	TS.Layout := tlCenter;
        TS.Alignment := taRightJustify;
			end;
      tapBottomLeft: begin
      	TS.Layout := tlBottom;
        TS.Alignment := taLeftJustify;
			end;
      tapBottomCenter: begin
      	TS.Layout := tlBottom;
        TS.Alignment := taCenter;
			end;
      tapBottomRight: begin
      	TS.Layout := tlBottom;
        TS.Alignment := taRightJustify;
			end;
		end;
    TS.Opaque := False;
    TS.Clipping := True;
    TS.SystemFont := fFont.IsDefault;
    Canvas.Font := fFont;
    Canvas.TextRect(Area, Area.Left,Area.Top, Caption, TS);
  end;
end;

function TTSCustomRect.IsStoredBevelSides: Boolean;
begin
	Result := PropIsStored(propIDBevelSides, fBevelSides <> tsDefPnlBvlSides);
end;

function TTSCustomRect.IsStoredBevelWidth: Boolean;
begin
	Result := PropIsStored(propIDBevelWidth, fBevelWidth <> tsDefPnlBvlWidth);
end;

function TTSCustomRect.IsStoredBorderColor: Boolean;
begin
	Result := PropIsStored(propIDBorderColor, fBorderClr <> fBackground.Color);
end;

function TTSCustomRect.IsStoredBorderStyle: Boolean;
begin
	Result := PropIsStored(propIDBorderStyle, fBorderStyle <> tsDefPnlBorderStyle);
end;

function TTSCustomRect.IsStoredBorderWidth: Boolean;
begin
	Result := PropIsStored(propIDBorderWidth, fBorderWidth <> tsDefPnlBorderWidth);
end;

function TTSCustomRect.IsStoredCaption: Boolean;
begin
	Result := PropIsStored(propIDCaption, fCaption <> tsDefPnlCaption);
end;

function TTSCustomRect.IsStoredFont: Boolean;
begin
	Result := PropIsStored(propIDFont, (fParentFont=False) and not IsTSDefaultFont(fFont));
end;

function TTSCustomRect.IsStoredTextAlign: Boolean;
begin
	Result := PropIsStored(propIDTextAlign, fTxtAlign = tsDefPnlTextAlign);
end;

{$IFDEF BGRA}
procedure TTSCustomRect.BCPropChanged(ASender: TObject; AData: PtrInt);
begin
	PropChanged(propIDBackground);
end;

{$ELSE}
function TTSCustomRect.IsStoredBackground(Sender: TObject; PropID: Integer; StoredDefault: Boolean): Boolean;
begin
	Result := PropIsStored(PropID, StoredDefault);
end;

procedure TTSCustomRect.BackgroundChanged(Sender: TObject; PropID: Integer);
begin
	PropChanged(PropID);
end;
{$ENDIF}

procedure TTSCustomRect.SetBackground(AValue: TTSBackground);
begin
	if fBackground = AValue then Exit;
	fBackground := AValue;
  PropChanged(propIDBackground);
end;

function TTSCustomRect.IsStoredParentFont: Boolean;
begin
	Result := PropIsStored(propIDParentFont, fParentFont <> tsDefPnlParentFont);
end;

procedure TTSCustomRect.FontChanged(Sender: TObject);
begin
	PropChanged(propIDFont);
end;

function TTSCustomRect.GetFont: TFont;
begin
	if fParentFont and Assigned((Owner as TControl).Parent) then
		Result := (Owner as TControl).Parent.Font
  else
  	Result := fFont;
end;

procedure TTSCustomRect.SetBevelSides(AValue: SBevelSides);
begin
	if fBevelSides = AValue then Exit;
	fBevelSides := AValue;
  PropChanged(propIDBevelSides);
end;

procedure TTSCustomRect.SetBevelWidth(AValue: Integer);
begin
	if fBevelWidth = AValue then Exit;
	fBevelWidth := AValue;
  PropChanged(propIDBevelWidth);
end;

procedure TTSCustomRect.SetBorderColor(AValue: TColor);
begin
	if fBorderClr = AValue then Exit;
	fBorderClr := AValue;
  PropChanged(propIDBorderColor);
end;

procedure TTSCustomRect.SetBorderStyle(AValue: NTSBorderStyle);
begin
	if fBorderStyle = AValue then Exit;
	fBorderStyle := AValue;
  PropChanged(propIDBorderStyle);
end;

procedure TTSCustomRect.SetBorderWidth(AValue: Integer);
begin
	if fBorderWidth = AValue then Exit;
	fBorderWidth := AValue;
  PropChanged(propIDBorderWidth);
end;

procedure TTSCustomRect.SetCaption(AValue: string);
begin
	if fCaption = AValue then Exit;
  fCaption := AValue;
  PropChanged(propIDCaption);
end;

procedure TTSCustomRect.SetFont(AValue: TFont);
begin
	fParentFont := False;
  fFont.Assign(AValue);
  PropChanged(propIDFont);
end;

procedure TTSCustomRect.SetParentFont(AValue: Boolean);
begin
	fParentFont := AValue;
  PropChanged(propIDParentFont);
  //if AValue and Assigned(Owner) then
  //	Font := (Owner as TControl).Font;
end;

procedure TTSCustomRect.SetTxtAlign(AValue: NTxtAlignPos);
begin
	if fTxtAlign = AValue then Exit;
	fTxtAlign := AValue;
  PropChanged(propIDTextAlign);
end;


//***************************************************************************************
//***************************************************************************************
{ ETSException }
//***************************************************************************************
//***************************************************************************************
constructor ETSException.Create(const ExClassName, Msg: string);
begin
  inherited Create(Msg);
  fClassName := ExClassName;
end;

//***************************************************************************************
constructor ETSException.Create(const ExClassName, OwnerName, CompName, Msg: string);
begin
  inherited Create(Msg);
  fClassName := ExClassName;
  fOwnerName := OwnerName;
  fCompName := CompName;
end;

//***************************************************************************************
constructor ETSException.Create(EObject: TObject; const Msg: string);
begin
  inherited Create(Msg);
  if Assigned(EObject) then
  begin
    fClassName := EObject.ClassName;
    if EObject is TComponent then
    begin
      fCompName := TComponent(EObject).Name;
      if Assigned(TComponent(EObject).Owner) then
        fOwnerName := TComponent(EObject).Owner.Name
      else
        fOwnerName := '<nil>';
    end
    else begin
      fCompName := '';
      fOwnerName := '';
    end;
  end
  else begin
    fClassName := '';
    fCompName := '';
    fOwnerName := '';
  end;
end;

//***************************************************************************************
//***************************************************************************************
{ TTSTextProperties }
//***************************************************************************************
//***************************************************************************************

constructor TTSTextProperties.Create(AOwner: TControl);
begin
	inherited Create(AOwner);
	fCaption := AOwner.Name;

	fAlignment := tsDefLbAlignment;
	fLayout := tsDefLbLayout;
	fWordWrap := tsDefLbWordWrap;
	fVisible := tsDefLbVisible;
	fTransparent := tsDefLbTransparent;

	fParentColor := tsDefLbParentColor;
	if fParentColor then
		fColor := AOwner.Color
	else
		fColor := tsDefLbColor;

  fParentFont := tsDefLbParentFont;
	fFont := TFont.Create;
  if fParentFont then
		fFont.Assign(AOwner.Font)
	else
		fFont.Assign(tsDefFont);
	fFont.OnChange := @FontChanged;
end;

destructor TTSTextProperties.Destroy;
begin
  fFont.Free;
	inherited Destroy;
end;

procedure TTSTextProperties.FontChanged(Sender: TObject);
begin
	PropChanged(propIDFont);
end;

type
  LblCrk = class(TCustomLabel);

procedure TTSTextProperties.Assign(Source: TPersistent);
var
  Ct: TControl;
  Lb: LblCrk;
	TP: TTSTextProperties;
begin
	if Source is TTSTextProperties then
  begin
		TP := TTSTextProperties(Source);
		fColor := TP.Color;
		fCaption := TP.Caption;
		fVisible := TP.Visible;
		fTransparent := TP.Transparent;
		fWordWrap := TP.WordWrap;
		fAlignment := TP.Alignment;
		fFont.Assign(TP.Font);
		PropChanged;
	end
  else if Source is TCustomLabel then
  begin
		Lb := LblCrk(Source);
		fColor := Lb.Color;
		fCaption := Lb.Caption;
		fVisible := Lb.Visible;
		fTransparent := Lb.Transparent;
		fWordWrap := Lb.WordWrap;
		fAlignment := Lb.Alignment;
		fFont.Assign(Lb.Font);
		PropChanged;
	end
	else if Source is TControl then
  begin
		Ct := TControl(Source);
		fColor := Ct.Color;
		fCaption := Ct.Caption;
		fVisible := Ct.Visible;
		fFont.Assign(Ct.Font);
		fTransparent := GetObjectOrdProp(Ct, 'Transparent', Integer(fTransparent))<>0;
		fWordWrap := GetObjectOrdProp(Ct, 'WordWrap', Integer(fWordWrap))<>0;
		fAlignment := TAlignment(GetObjectOrdProp(Ct, 'Alignment', Integer(fAlignment)) mod Integer(High(TAlignment)));
		PropChanged;
	end
  else begin
		fColor := GetObjectOrdProp(Ct, 'Color', fColor);
		fCaption := GetObjectStrProp(Ct, 'Caption', fCaption);
		fVisible := GetObjectOrdProp(Ct, 'Visible', Integer(fVisible))<>0;
		fFont.Assign(Ct.Font);
		fTransparent := GetObjectOrdProp(Ct, 'Transparent', Integer(fTransparent))<>0;
		fWordWrap := GetObjectOrdProp(Ct, 'WordWrap', Integer(fWordWrap))<>0;
		fAlignment := TAlignment(GetObjectOrdProp(Ct, 'Alignment', Integer(fAlignment)) mod Integer(High(TAlignment)));
		PropChanged;
	end;

end;

procedure TTSTextProperties.AssignTo(Dest: TPersistent);
var
  Ct: TControl;
  Lb: LblCrk;
	TP: TTSTextProperties;
begin
	if Dest is TTSTextProperties then
  	Dest.Assign(Self)
  else if Dest is TCustomLabel then
  begin
		Lb := LblCrk(Dest);
		Lb.Color := fColor;
	  Lb.Caption := fCaption;
	  Lb.Visible := fVisible;
	  Lb.Transparent := fTransparent;
  	Lb.WordWrap := fWordWrap;
	  Lb.Alignment := fAlignment;
		Lb.Font.Assign(fFont);
	end
	else if Dest is TControl then
  begin
		Ct := TControl(Dest);
	  Ct.Color := fColor;
	  Ct.Caption := fCaption;
  	Ct.Visible := fVisible;
	  Ct.Font.Assign(fFont);
		SetObjectOrdProp(Ct, 'Transparent', Integer(fTransparent));
		SetObjectOrdProp(Ct, 'WordWrap', Integer(fWordWrap));
		SetObjectOrdProp(Ct, 'Alignment', Integer(fAlignment));
	end
  else begin
		SetObjectOrdProp(Dest, 'Color', fColor);
		SetObjectStrProp(Dest, 'Caption', fCaption);
		SetObjectOrdProp(Dest, 'Visible', Integer(fVisible));
		SetObjectOrdProp(Dest, 'Transparent', Integer(fTransparent));
		SetObjectOrdProp(Dest, 'WordWrap', Integer(fWordWrap));
		SetObjectOrdProp(Dest, 'Alignment', Integer(fAlignment));
	end;
end;

function TTSTextProperties.GetColor: TColor;
begin
	if fParentColor and Assigned((Owner as TControl).Parent) then
		Result := (Owner as TControl).Parent.Color
  else
  	Result := fColor;
end;

function TTSTextProperties.GetFont: TFont;
begin
	if fParentFont and Assigned((Owner as TControl).Parent) then
		Result := (Owner as TControl).Parent.Font
  else
  	Result := fFont;
end;

procedure TTSTextProperties.SetCaption(const Value: TCaption);
begin
  if fCaption = Value then Exit;
	fCaption := Value;
  PropChanged(propIDCaption);
end;

procedure TTSTextProperties.SetFont(const Value: TFont);
begin
	fParentFont := False;
  fFont.Assign(Value);
  PropChanged(propIDFont);
end;

procedure TTSTextProperties.SetAlignment(const Value: TAlignment);
begin
  if fAlignment = Value then Exit;
	fAlignment := Value;
  PropChanged(propIDTextAlign);
end;

procedure TTSTextProperties.SetColor(const Value: TColor);
begin
  fParentColor := False;
	fColor  := Value;
  PropChanged(propIDColor);
end;

procedure TTSTextProperties.SetLayout(AValue: TTextLayout);
begin
	if fLayout = AValue then Exit;
	fLayout := AValue;
end;

procedure TTSTextProperties.SetParentColor(AValue: Boolean);
begin
	fParentColor := AValue;
  PropChanged(propIDParentColor);
  //if AValue and Assigned(Owner) then
  //	Color := (Owner as TControl).Color;
end;

procedure TTSTextProperties.SetParentFont(AValue: Boolean);
begin
	fParentFont := AValue;
  PropChanged(propIDParentFont);
  //if AValue and Assigned(Owner) then
  //	Font := (Owner as TControl).Font;
end;

procedure TTSTextProperties.SetTransparent(const Value: Boolean);
begin
  if fTransparent = Value then Exit;
	fTransparent := Value;
  PropChanged(propIDTransparent);
end;

procedure TTSTextProperties.SetVisible(const Value: Boolean);
begin
  if fVisible = Value then Exit;
	fVisible := Value;
  PropChanged(propIDVisible);
end;

procedure TTSTextProperties.SetWordWrap(const Value: Boolean);
begin
  if fWordWrap = Value then Exit;
	fWordWrap := Value;
  PropChanged(propIDWordWrap);
end;

function TTSTextProperties.IsStoredAlignment: Boolean;
begin
	Result := PropIsStored(propIDTextAlign, Alignment <> tsDefLbAlignment);
end;

function TTSTextProperties.IsStoredCaption: Boolean;
begin
	Result := PropIsStored(propIDCaption, Caption <> tsDefLbCaption);
end;

function TTSTextProperties.IsStoredColor: Boolean;
begin
	Result := PropIsStored(propIDColor, (fParentColor=False) and (fColor <> tsDefLbColor));
end;

function TTSTextProperties.IsStoredFont: Boolean;
begin
	Result := PropIsStored(propIDFont, (fParentFont=False) and not IsTSDefaultFont(fFont));
end;

function TTSTextProperties.IsStoredLayout: Boolean;
begin
	Result := PropIsStored(propIDTextLayout, fLayout <> tsDefLbLayout);
end;

function TTSTextProperties.IsStoredParentColor: Boolean;
begin
	Result := PropIsStored(propIDParentColor, fParentColor <> tsDefLbParentColor);
end;

function TTSTextProperties.IsStoredParentFont: Boolean;
begin
	Result := PropIsStored(propIDParentFont, fParentFont <> tsDefLbParentFont);
end;

function TTSTextProperties.IsStoredTransparent: Boolean;
begin
	Result := PropIsStored(propIDTransparent, fTransparent <> tsDefLbTransparent);
end;

function TTSTextProperties.IsStoredVisible: Boolean;
begin
	Result := PropIsStored(propIDVisible, fVisible <> tsDefLbVisible);
end;

function TTSTextProperties.IsStoredWordWrap: Boolean;
begin
	Result := PropIsStored(propIDWordWrap, fWordWrap <> tsDefLbWordWrap);
end;


end.

