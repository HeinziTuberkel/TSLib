unit TSLibGraphics;

interface

uses
  Windows,
	Classes,
	Graphics,
	Controls,
	Types;

const
	MM_MAX_NUMAXES = 16;
  FR_PRIVATE = $10;

type
	NTSGradientDir = (gdNone, gdVertical, gdHorizontal, gdRect);
  NTSBorderStyle = (brdrNone, brdrLine, brdrRaised, brdrLowered, brdrBump, brdrEtched);
	NBevelSides = (bvlLeft, bvlRight, bvlTop, bvlBottom);
	SBevelSides = set of NBevelSides;

	NTxtAlignPos = (tapBottomLeft, tapCenterLeft, tapTopLeft,
									tapBottomCenter, tapCenter, tapTopCenter,
									tapBottomRight, tapCenterRight, tapTopRight
                  );

	RColor = packed record
		case Integer of
			0: (Value: Integer);
			1: (R, G, B: Byte);
	end;

	PDesignVector = ^TDesignVector;
	TDesignVector = packed record
		dvReserved: DWORD;
		dvNumAxes: DWORD;
		dvValues: Array[0..MM_MAX_NUMAXES-1] Of LongInt;
	end;

var
	DefaultFont: TFont;
	TSStdFont: TFont;
	TSInvertFont: TFont;
	TSBoldFont: TFont;
  NextPrivateFontID: Cardinal = $10;

function LightenColorChannel(const Channel: Byte; const Pct: Double): Byte;
function DarkenColorChannel(const Channel: Byte; const Pct: Double): Byte;
function LightenColor(Color: TColor; Percent: Double): TColor;
function DarkenColor(Color: TColor; Percent: Double): TColor;
procedure GrowRect(var ARect: TRect; GrowBy: Word); overload;
procedure GrowRect(var ARect: TRect; GrowBy: TRect); overload;
procedure ShrinkRect(var ARect: TRect; ShrinkBy: Word); overload;
procedure ShrinkRect(var ARect: TRect; ShrinkBy: TRect); overload;
procedure ShrinkRect(var ARect: TRect; Sides: SBevelSides; ShrinkBy: Word); overload;

procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
																							Color: TColor; Width: Integer=1); overload;
procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
													Sides: SBevelSides; Color: TColor; Width: Integer=1); overload;
procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
																		Color: TColor; Width, BevelWidth: Integer); overload;
procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
								Sides: SBevelSides; Color: TColor; Width, BevelWidth: Integer); overload;
procedure LoweredRect(ARect: TRect; Canvas: TCanvas; Color: TColor; Width: Integer =1); overload;
procedure LoweredRect(ARect: TRect; Canvas: TCanvas; Sides: SBevelSides; Color: TColor; Width: Integer =1); overload;
procedure RaisedRect(ARect: TRect; Canvas: TCanvas; Color: TColor; Width: Integer =1); overload;
procedure RaisedRect(ARect: TRect; Canvas: TCanvas; Sides: SBevelSides; Color: TColor; Width: Integer =1); overload;
procedure GradientRect(ARect: TRect; Direction: NTSGradientDir; Canvas: TCanvas;
												FromColor: TColor; ToColor: TColor = clWhite; Steps: Byte = 64);
procedure Circle(Canvas: TCanvas; Center: TPoint; Radius: Integer);

function AlignPosHorz(const ARect: TRect; Alignment: NTxtAlignPos): TPoint; overload;
function AlignPosHorz(const ARect, Offs: TRect; Alignment: NTxtAlignPos): TPoint; overload;

function AlignPosVert(const ARect, Offs: TRect; Alignment: NTxtAlignPos): TPoint; overload;
function AlignPosVert(const ARect: TRect; Alignment: NTxtAlignPos): TPoint; overload;

function IsInRect(const P: TPoint; const R: TRect): Boolean;
function SameRect(const R1, R2: TRect): Boolean;

procedure HorizontalText(X, Y: Integer; Canvas: TCanvas; const Text: string;
															TxtXYPos: NTxtAlignPos=tapTopLeft); overload;
procedure VerticalText(X, Y: Integer; Canvas: TCanvas; const Text: string;
															TxtXYPos: NTxtAlignPos=tapTopLeft); overload;
procedure HorizontalText(ARect: TRect; Canvas: TCanvas; const Text: string;
															TxtXYPos: NTxtAlignPos=tapTopLeft; Clip: Boolean=False); overload;
procedure VerticalText(ARect: TRect; Canvas: TCanvas; const Text: string;
															TxtXYPos: NTxtAlignPos=tapTopLeft; Clip: Boolean=False); overload;

function BevelsToBorder(BVInner, BVOuter: TBevelCut): NTSBorderStyle;
procedure BorderToBevels(BorderStyle: NTSBorderStyle; var BVInner, BVOuter: TBevelCut);

function AddFontResourceEx(Dir: PAnsiChar; Flag: Cardinal; PDV: PDesignVector): Int64; StdCall;
														External 'GDI32.dll' Name 'AddFontResourceExA';
function RemoveFontResourceEx(Dir: PAnsiChar; Flag: Cardinal; PDV : PDesignVector): Int64; StdCall;
														External 'GDI32.dll' Name 'RemoveFontResourceExA';
function LoadTemporaryFont(FontFile: string): Boolean;
function UnloadTemporaryFont(FontFile: string): Boolean;

const
	bvlAllSides = [bvlLeft, bvlRight, bvlTop, bvlBottom];

implementation

uses
	Math,
	SysUtils;

type
	TCtrlCracker = class(TControl);

const
	borderLightenPercent = 50;
	borderDarkenPercent = 50;

//---------------------------------------------------------------------------------------
function DarkenColorChannel(const Channel: Byte; const Pct: Double): Byte;
var
	Temp: Integer;
begin
	if Pct < 0 then
		Result := LightenColorChannel(Channel, -Pct)
	else
	begin
		Temp := Round(Channel * (100-Pct) / 100);
		if Temp < Low(Result) then
			Result := Low(Result)
		else
			Result := Temp;
	end;
end;

//---------------------------------------------------------------------------------------
function LightenColorChannel(const Channel: Byte; const Pct: Double): Byte;
var
	Temp: Integer;
begin
	if Pct < 0 then
		Result := DarkenColorChannel(Channel, -Pct)
	else
	begin
		Temp := Round((High(Channel)-Channel) * Pct / 100) + Channel;
		if Temp > High(Result) then
			Result := High(Result)
		else
			Result := Temp;
	end;
end;

//---------------------------------------------------------------------------------------
function LightenColor(Color: TColor; Percent: Double): TColor;
var
	Temp: RColor;
begin
	Temp.Value := ColorToRGB(Color);
	Temp.R := LightenColorChannel(Temp.R, Percent);
	Temp.G := LightenColorChannel(Temp.G, Percent);
	Temp.B := LightenColorChannel(Temp.B, Percent);
	Result := Temp.Value;
end;

//---------------------------------------------------------------------------------------
function DarkenColor(Color: TColor; Percent: Double): TColor;
var
	Temp: RColor;
begin
	Temp.Value := ColorToRGB(Color);
	Temp.R := DarkenColorChannel(Temp.R, Percent);
	Temp.G := DarkenColorChannel(Temp.G, Percent);
	Temp.B := DarkenColorChannel(Temp.B, Percent);
	Result := Temp.Value;
end;

//---------------------------------------------------------------------------------------
procedure GrowRect(var ARect: TRect; GrowBy: Word); overload;
begin
	with ARect do
	begin
		Dec(Left, GrowBy);
		Dec(Top, GrowBy);
		Inc(Right, GrowBy);
		Inc(Bottom, GrowBy);
	end;
end;

//---------------------------------------------------------------------------------------
procedure GrowRect(var ARect: TRect; GrowBy: TRect); overload;
begin
	with ARect do
	begin
		Dec(Left, GrowBy.Left);
		Dec(Top, GrowBy.Top);
		Inc(Right, GrowBy.Right);
		Inc(Bottom, GrowBy.Bottom);
	end;
end;

//---------------------------------------------------------------------------------------
procedure ShrinkRect(var ARect: TRect; ShrinkBy: Word); overload;
begin
	with ARect do
	begin
		Inc(Left, ShrinkBy);
		Inc(Top, ShrinkBy);
		Dec(Right, ShrinkBy);
		Dec(Bottom, ShrinkBy);
	end;
end;

//---------------------------------------------------------------------------------------
procedure ShrinkRect(var ARect: TRect; ShrinkBy: TRect); overload;
begin
	with ARect do
	begin
		Inc(Left, ShrinkBy.Left);
		Inc(Top, ShrinkBy.Top);
		Dec(Right, ShrinkBy.Right);
		Dec(Bottom, ShrinkBy.Bottom);
	end;
end;

//---------------------------------------------------------------------------------------
procedure ShrinkRect(var ARect: TRect; Sides: SBevelSides; ShrinkBy: Word); overload;
begin
	with ARect do
	begin
		if bvlLeft in Sides then
			Inc(Left, ShrinkBy);
		if bvlTop in Sides then
			Inc(Top, ShrinkBy);
		if bvlRight in Sides then
			Dec(Right, ShrinkBy);
		if bvlBottom in Sides then
			Dec(Bottom, ShrinkBy);
	end;
end;

//---------------------------------------------------------------------------------------
procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
																												Color: TColor; Width: Integer=1);
begin
	DrawBorder(ARect, Canvas, Style, bvlAllSides, Color, Width);
end;

//---------------------------------------------------------------------------------------
procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
																		Sides: SBevelSides; Color: TColor; Width: Integer=1);
begin
	case Style of
		brdrLine, brdrRaised, brdrLowered:
			Drawborder(ARect, Canvas, Style, Sides, Color, Width, Width);
		brdrBump, brdrEtched: begin
			if Width = 1 then
				Inc(Width);
			Drawborder(ARect, Canvas, Style, Sides, Color, Width, Width shr 1);
		end
	end;
end;

//---------------------------------------------------------------------------------------
procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
																							Color: TColor; Width, BevelWidth: Integer);
begin
	Drawborder(ARect, Canvas, Style, bvlAllSides, Color, Width, BevelWidth);
end;

//---------------------------------------------------------------------------------------
procedure DrawBorder(var ARect: TRect; Canvas: TCanvas; Style: NTSBorderStyle;
													Sides: SBevelSides; Color: TColor; Width, BevelWidth: Integer); 
var
	I: Integer;
begin
	if BevelWidth > Width then
		BevelWidth := Width;
	case Style of
		//.....................
		brdrNone:
			Exit;
		brdrRaised: begin
			RaisedRect(ARect, Canvas, Sides, Color, BevelWidth);
			ShrinkRect(ARect, Sides, BevelWidth);
			Width := Width - BevelWidth;
		end;
		//.....................
		brdrLowered: begin
			LoweredRect(ARect, Canvas, Sides, Color, BevelWidth);
			ShrinkRect(ARect, Sides, BevelWidth);
			Width := Width - BevelWidth;
		end;
		//.....................
		brdrBump: begin
			Width := Width - BevelWidth;
			I := Width - BevelWidth;
			if I > 0 then begin
				DrawBorder(ARect, Canvas, brdrRaised, Sides, Color, Width, BevelWidth);
				DrawBorder(ARect, Canvas, brdrLowered, Sides, Color, BevelWidth);
			end
			else begin
				DrawBorder(ARect, Canvas, brdrRaised, Sides, Color, BevelWidth);
				DrawBorder(ARect, Canvas, brdrLowered, Sides, Color, BevelWidth + I);
			end;
			Width := 0;
		end;
		//.....................
		brdrEtched: begin
			Width := Width - BevelWidth;
			I := Width - BevelWidth;
			if I > 0 then begin
				DrawBorder(ARect, Canvas, brdrLowered, Sides, Color, Width, BevelWidth);
				DrawBorder(ARect, Canvas, brdrRaised, Sides, Color, BevelWidth);
			end
			else begin
				DrawBorder(ARect, Canvas, brdrLowered, Sides, Color, BevelWidth);
				DrawBorder(ARect, Canvas, brdrRaised, Sides, Color, BevelWidth + I);
			end;
			Width := 0;
		end;
	end;
	Canvas.Brush.Color := Color;
	for I := 1 to Width do begin
		Canvas.FrameRect(ARect);
		ShrinkRect(ARect, Sides, 1);
	end;
end;

//---------------------------------------------------------------------------------------
procedure RaisedRect(ARect: TRect; Canvas: TCanvas; Color: TColor; Width: Integer = 1);
begin
	RaisedRect(ARect, Canvas, bvlAllSides, Color, Width);
end;

//---------------------------------------------------------------------------------------
procedure RaisedRect(ARect: TRect; Canvas: TCanvas; Sides: SBevelSides; Color: TColor; Width: Integer =1);
var
	CL, CD: TColor;
	I: Integer;
begin
	CL := LightenColor(Color, borderLightenPercent);
	CD := DarkenColor(Color, borderDarkenPercent);
	Dec(ARect.Bottom);
	Dec(ARect.Right);
	for I := 1 to Width do begin
		Canvas.Pen.Color := CL;

		if bvlLeft in Sides then begin
			Canvas.MoveTo(ARect.Left, ARect.Bottom);
			Canvas.LineTo(ARect.Left, ARect.Top);
			if bvlTop in Sides then
				Canvas.LineTo(ARect.Right, ARect.Top);
		end
		else if bvlTop in Sides then begin
			Canvas.MoveTo(ARect.Left, ARect.Top);
			Canvas.LineTo(ARect.Right, ARect.Top);
		end;

		Canvas.Pen.Color := CD;
		if bvlBottom in Sides then begin
			Canvas.MoveTo(ARect.Left, ARect.Bottom);
			Canvas.LineTo(ARect.Right, ARect.Bottom);
			if bvlRight in Sides then
				Canvas.LineTo(ARect.Right, ARect.Top-1);
		end
		else if bvlRight in Sides then begin
			Canvas.MoveTo(ARect.Right, ARect.Bottom);
			Canvas.LineTo(ARect.Right, ARect.Top-1);
		end;
		ShrinkRect(ARect, Sides, 1);
	end;
end;

//---------------------------------------------------------------------------------------
procedure LoweredRect(ARect: TRect; Canvas: TCanvas; Color: TColor; Width: Integer = 1);
begin
	LoweredRect(ARect, Canvas, bvlAllSides, Color, Width);
end;

//---------------------------------------------------------------------------------------
procedure LoweredRect(ARect: TRect; Canvas: TCanvas; Sides: SBevelSides; Color: TColor; Width: Integer = 1);
var
	CL, CD: TColor;
	I: Integer;
begin
	CL := LightenColor(Color, borderLightenPercent);
	CD := DarkenColor(Color, borderDarkenPercent);
	Dec(ARect.Bottom);
	Dec(ARect.Right);
	for I := 1 to Width do begin
		Canvas.Pen.Color := CD;
		if bvlLeft in Sides then begin
			Canvas.MoveTo(ARect.Left, ARect.Bottom);
			Canvas.LineTo(ARect.Left, ARect.Top);
			if bvlTop in Sides then
				Canvas.LineTo(ARect.Right, ARect.Top);
		end
		else if bvlTop in Sides then begin
			Canvas.MoveTo(ARect.Left, ARect.Top);
			Canvas.LineTo(ARect.Right, ARect.Top);
		end;
		Canvas.Pen.Color := CL;
		if bvlBottom in Sides then begin
			Canvas.MoveTo(ARect.Left, ARect.Bottom);
			Canvas.LineTo(ARect.Right, ARect.Bottom);
			if bvlRight in Sides then
				Canvas.LineTo(ARect.Right, ARect.Top-1);
		end
		else if bvlRight in Sides then begin
			Canvas.MoveTo(ARect.Right, ARect.Bottom);
			Canvas.LineTo(ARect.Right, ARect.Top-1);
		end;
		ShrinkRect(ARect, Sides, 1);
	end;
end;

//---------------------------------------------------------------------------------------
procedure GradientRect(ARect: TRect; Direction: NTSGradientDir; Canvas: TCanvas;
												FromColor: TColor; ToColor: TColor = clWhite; Steps: Byte = 64);
var
	CFrom, CTo: RColor;
	RectStep, StpR, StpG, StpB: Double;
	I, RStart, REnd, StopAt: Integer;
begin
	if Steps = 0 then
		Steps := 1;

//Calculate the Step sizes for (Width/Heigth) for each colored rectangle.
	case Direction of
		gdVertical: begin
			StopAt := ARect.Bottom;
			RStart := ARect.Top;
			I := ARect.Bottom - ARect.Top;
			if I < Steps then begin
				RectStep := 1;
				Steps := I;
			end
			else
				RectStep := I / Steps;
		end;
		gdHorizontal: begin
			RStart := ARect.Left;
			StopAt := ARect.Right;
			I := ARect.Right - ARect.Left;
			if I < Steps then begin
				RectStep := 1;
				Steps := I;
			end
			else
				RectStep := I / Steps;
		end;
		gdRect: begin
			I := Min(ARect.Bottom - ARect.Top, ARect.Right - ARect.Left) shr 1;
			if I < Steps then begin
				RectStep := 1;
				Steps := I;
			end
			else
				RectStep := I / Steps;
			RStart := 0;
			StopAt := 0; //Dummy, um die Warnung zu umgehen.
		end
		//No gradient - fill with "Color" and exit.
		else begin
			Canvas.Brush.Color := FromColor;
			Canvas.FillRect(ARect);
			Exit;
		end;
	end;

	//Calculate the color steps
	CFrom.Value := ColorToRGB(FromColor);
	CTo.Value := ColorToRGB(ToColor);
	StpR := (CTo.R - CFrom.R) / Steps;
	StpG := (CTo.G - CFrom.G) / Steps;
	StpB := (CTo.B - CFrom.B) / Steps;

	CTo.Value := CFrom.Value;
	case Direction of
//........... Draw vertical gradient ....................
		gdVertical:
			for I := 1 to Steps do
			begin
				Canvas.Pen.Color := CTo.Value;
				Canvas.Brush.Color := CTo.Value;

				ARect.Bottom := RStart + Round(I * RectStep);
				if ARect.Bottom > StopAt then begin
					ARect.Bottom := StopAt;
//					Canvas.Rectangle(ARect);
					Canvas.FillRect(ARect);
					Break;
				end
				else
					Canvas.FillRect(ARect);
//					Canvas.Rectangle(ARect);
				ARect.Top := ARect.Bottom;
				CTo.R := CFrom.R + Round(I*StpR);
				CTo.G := CFrom.G + Round(I*StpG);
				CTo.B := CFrom.B + Round(I*StpB);
			end;

//........... Draw horzontal gradient ....................
		gdHorizontal:
			for I := 1 to Steps do
			begin
				Canvas.Pen.Color := CTo.Value;
				Canvas.Brush.Color := CTo.Value;
				ARect.Right := RStart + Round(I * RectStep);
				if ARect.Right > StopAt then begin
					ARect.Right := StopAt;
					Canvas.FillRect(ARect);
//					Canvas.Rectangle(ARect);
					Break;
				end
				else
					Canvas.FillRect(ARect);
//					Canvas.Rectangle(ARect);
				ARect.Left := ARect.Right;
				CTo.R := CFrom.R + Round(I*StpR);
				CTo.G := CFrom.G + Round(I*StpG);
				CTo.B := CFrom.B + Round(I*StpB);
			end;

//........... Draw rectangular gradient ....................
		gdRect:
			for I := 1 to Steps do
			begin
				Canvas.Pen.Color := CTo.Value;
				Canvas.Brush.Color := CTo.Value;

				if (ARect.Left > ARect.Right) or (ARect.Top > ARect.Bottom) then
					Break;
				Canvas.FillRect(ARect);
//				Canvas.Rectangle(ARect);
				REnd := Round(I*RectStep);
				ShrinkRect(ARect, REnd - RStart);
				RStart := REnd;
				CTo.R := CFrom.R + Round(I*StpR);
				CTo.G := CFrom.G + Round(I*StpG);
				CTo.B := CFrom.B + Round(I*StpB);
			end;
	end;
end;

//---------------------------------------------------------------------------------------
procedure Circle(Canvas: TCanvas; Center: TPoint; Radius: Integer);
begin
	Canvas.Ellipse(Center.X-Radius, Center.Y-Radius, Center.X+Radius, Center.Y+Radius);
end;

//---------------------------------------------------------------------------------------
function AlignPosHorz(const ARect: TRect; Alignment: NTxtAlignPos): TPoint; overload;
begin
	Result := AlignPosHorz(ARect, Rect(0,0,0,0), Alignment);
end;

//---------------------------------------------------------------------------------------
function AlignPosHorz(const ARect, Offs: TRect; Alignment: NTxtAlignPos): TPoint; overload;
begin
	case Alignment of
		tapBottomLeft: begin
			Result.X := ARect.Left - Offs.Left;
			Result.Y := ARect.Bottom - Offs.Bottom;
		end;
		tapCenterLeft: begin
			Result.X := ARect.Left - Offs.Left;
			Result.Y := ARect.Top + Offs.Top + (ARect.Bottom - ARect.Top - Offs.Bottom - Offs.Top) div 2;
		end;
		tapTopLeft: begin
			Result.X := ARect.Left - Offs.Left;
			Result.Y := ARect.Top + Offs.Top;
		end;
		tapBottomCenter: begin
			Result.X := ARect.Left + Offs.Left + (ARect.Right - ARect.Left - Offs.Right - Offs.Left) div 2;
			Result.Y := ARect.Bottom - Offs.Bottom;
		end;
		tapTopCenter: begin
			Result.X := ARect.Left + Offs.Left + (ARect.Right - ARect.Left - Offs.Right - Offs.Left) div 2;
			Result.Y := ARect.Top + Offs.Top;
		end;
		tapBottomRight: begin
			Result.X := ARect.Right - Offs.Right;
			Result.Y := ARect.Bottom - Offs.Bottom;
		end;
		tapCenterRight: begin
			Result.X := ARect.Right - Offs.Right;
			Result.Y := ARect.Top + Offs.Top + (ARect.Bottom - ARect.Top - Offs.Bottom - Offs.Top) div 2;
		end;
		tapTopRight: begin
			Result.X := ARect.Right - Offs.Right;
			Result.Y := ARect.Top + Offs.Top;
		end;
		else begin //tapCenter:
			Result.X := ARect.Left + Offs.Left + (ARect.Right - ARect.Left - Offs.Right - Offs.Left) div 2;
			Result.Y := ARect.Top + Offs.Top + (ARect.Bottom - ARect.Top - Offs.Bottom - Offs.Top) div 2;
		end;
	end;
end;

//---------------------------------------------------------------------------------------
function AlignPosVert(const ARect: TRect; Alignment: NTxtAlignPos): TPoint; overload;
begin
	Result := AlignPosVert(ARect, Rect(0,0,0,0) , Alignment);
end;

//---------------------------------------------------------------------------------------
function AlignPosVert(const ARect, Offs: TRect; Alignment: NTxtAlignPos): TPoint; overload;
begin
	case Alignment of
		tapBottomLeft: begin
			Result.X := ARect.Right - Offs.Bottom;
			Result.Y := ARect.Bottom - Offs.Left;
		end;
		tapCenterLeft: begin
			Result.X := ARect.Left + Offs.Top + (ARect.Right - ARect.Left - Offs.Top - Offs.Bottom) div 2;
			Result.Y := ARect.Bottom - Offs.Left;
		end;
		tapTopLeft: begin
			Result.X := ARect.Left + Offs.Top;
			Result.Y := ARect.Bottom - Offs.Left;
		end;
		tapBottomCenter: begin
			Result.X := ARect.Right - Offs.Bottom;
			Result.Y := ARect.Top + Offs.Right + (ARect.Bottom - ARect.Top - Offs.Right - Offs.Left) div 2;
		end;
		tapTopCenter: begin
			Result.X := ARect.Left + Offs.Top;
			Result.Y := ARect.Top + Offs.Right + (ARect.Bottom - ARect.Top - Offs.Right - Offs.Left) div 2;
		end;
		tapBottomRight: begin
			Result.X := ARect.Right - Offs.Bottom;
			Result.Y := ARect.Top + Offs.Right;
		end;
		tapCenterRight: begin
			Result.X := ARect.Left + Offs.Top + (ARect.Right - ARect.Left - Offs.Top - Offs.Bottom) div 2;
			Result.Y := ARect.Top + Offs.Right;
		end;
		tapTopRight: begin
			Result.X := ARect.Left + Offs.Top;
			Result.Y := ARect.Top + Offs.Right;
		end;
		else begin //tapCenter:
			Result.X := ARect.Left + Offs.Top + (ARect.Right - ARect.Left - Offs.Top - Offs.Bottom) div 2;
			Result.Y := ARect.Top + Offs.Right + (ARect.Bottom - ARect.Top - Offs.Right - Offs.Left) div 2;
		end;
	end;
end;

//---------------------------------------------------------------------------------------
function IsInRect(const P: TPoint; const R: TRect): Boolean;
begin
	Result := (P.X >= R.Left) and (P.X <= R.Right) and (P.Y >= R.Top) and (P.Y <= R.Bottom);
end;

//---------------------------------------------------------------------------------------
function SameRect(const R1, R2: TRect): Boolean;
begin
	Result := (R1.Bottom = R2.Bottom)
					and (R1.Right = R2.Right)
					and (R1.Top = R2.Top)
					and (R1.Left = R2.Left);
end;

//---------------------------------------------------------------------------------------
procedure HorizontalText(X, Y: Integer; Canvas: TCanvas; const Text: string;
																TxtXYPos: NTxtAlignPos=tapTopLeft);
begin
	case TxtXYPos of
		tapBottomLeft:
			Y := Y - Canvas.TextHeight(Text);
		tapCenterLeft:
			Y := Y - Canvas.TextHeight(Text) div 2;
		tapBottomCenter: begin
			X := X - Canvas.TextWidth(Text) div 2;
			Y := Y - Canvas.TextHeight(Text);
		end;
		tapCenter: begin
			X := X - Canvas.TextWidth(Text) div 2;
			Y := Y - Canvas.TextHeight(Text) div 2;
		end;
		tapTopCenter:
			X := X - Canvas.TextWidth(Text) div 2;
		tapBottomRight: begin
			X := X - Canvas.TextWidth(Text);
			Y := Y - Canvas.TextHeight(Text);
		end;
		tapCenterRight: begin
			X := X - Canvas.TextWidth(Text);
			Y := Y - Canvas.TextHeight(Text) div 2;
		end;
		tapTopRight:
			X := X - Canvas.TextWidth(Text);
	end;
	Canvas.TextOut(X, Y, Text);
end;

//---------------------------------------------------------------------------------------
procedure HorizontalText(ARect: TRect; Canvas: TCanvas; const Text: string;
															TxtXYPos: NTxtAlignPos=tapTopLeft; Clip: Boolean=False); overload;
var
	Flags: Cardinal;
begin
	case TxtXYPos of
		tapTopLeft:
			Flags := DT_TOP or DT_LEFT;
		tapBottomLeft:
			Flags := DT_BOTTOM or DT_LEFT;
		tapCenterLeft:
			Flags := DT_VCENTER or DT_LEFT;
		tapBottomCenter: begin
			Flags := DT_BOTTOM or DT_CENTER;
		end;
		tapCenter: begin
			Flags := DT_VCENTER or DT_CENTER;
		end;
		tapTopCenter:
			Flags := DT_TOP or DT_CENTER;
		tapBottomRight: begin
			Flags := DT_BOTTOM or DT_RIGHT;
		end;
		tapCenterRight: begin
			Flags := DT_VCENTER or DT_RIGHT;
		end;
		tapTopRight:
			Flags := DT_TOP or DT_RIGHT;
		else
			Flags := 0;
	end;
	Flags := Flags or DT_SINGLELINE or DT_EXTERNALLEADING;
	if not Clip then
		Flags := Flags or DT_NOCLIP;
	DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
end;

//---------------------------------------------------------------------------------------
procedure VerticalText(ARect: TRect; Canvas: TCanvas; const Text: string;
															TxtXYPos: NTxtAlignPos=tapTopLeft; Clip: Boolean=False); overload;
var
	Pt: TPoint;
	LogFont: TLogFont;
	CVFont, SaveFont: TFont;
begin
	SaveFont := TFont.Create;
	CVFont := TFont.Create;
	try
		SaveFont.Assign(Canvas.Font);
		CVFont.Assign(SaveFont);
		GetObject(CVFont.Handle, SizeOf(LogFont), @LogFont);
		LogFont.lfEscapement := 900;
		LogFont.lfOrientation := 900;
		SetBkMode(Canvas.Handle, TRANSPARENT);
		CVFont.Handle := CreateFontIndirect(LogFont);
		Canvas.Font.Assign(CVFont);
		CVFont.Free;
		Pt := AlignPosVert(ARect, TxtXYPos);
		case TxtXYPos of
			tapBottomLeft:
				Pt.X := Pt.X - Canvas.TextHeight(Text);
			tapCenterLeft:
				Pt.X := Pt.X - Canvas.TextHeight(Text) div 2;

			tapBottomCenter: begin
				Pt.X := Pt.X - Canvas.TextHeight(Text);
				Pt.Y := Pt.Y + Canvas.TextWidth(Text) div 2;
			end;
			tapCenter: begin
				Pt.X := Pt.X - Canvas.TextHeight(Text) div 2;
				Pt.Y := Pt.Y + Canvas.TextWidth(Text) div 2;
			end;
			tapTopCenter:
				Pt.Y := Pt.Y + Canvas.TextWidth(Text) div 2;

			tapTopRight:
				Pt.Y := Pt.Y + Canvas.TextWidth(Text);
			tapCenterRight: begin
				Pt.X := Pt.X - Canvas.TextHeight(Text) div 2;
				Pt.Y := Pt.Y + Canvas.TextWidth(Text);
			end;
			tapBottomRight: begin
				Pt.X := Pt.X - Canvas.TextHeight(Text);
				Pt.Y := Pt.Y + Canvas.TextWidth(Text);
			end;
		end;
		if Clip then begin
			Canvas.Brush.Style := bsClear;
			Canvas.TextRect(ARect, Pt.X, Pt.Y, Text);
		end
		else
			DrawText(Canvas.Handle, PChar(Text), -1, ARect,DT_LEFT or DT_WordBreak);
		Canvas.Font.Assign(SaveFont);
	finally
		SaveFont.Free;
	end;
end;

//---------------------------------------------------------------------------------------
procedure VerticalText(X, Y: Integer; Canvas: TCanvas; const Text: string;
																TxtXYPos: NTxtAlignPos=tapTopLeft);
var
	LogFont: TLogFont;
	CVFont, SaveFont: TFont;
begin
	SaveFont := TFont.Create;
	CVFont := TFont.Create;
	try
		SaveFont.Assign(Canvas.Font);
		CVFont.Assign(SaveFont);
		GetObject(CVFont.Handle, SizeOf(LogFont), @LogFont);
		LogFont.lfEscapement := 900;
		LogFont.lfOrientation := 900;
		SetBkMode(Canvas.Handle, TRANSPARENT);
		CVFont.Handle := CreateFontIndirect(LogFont);
		Canvas.Font.Assign(CVFont);
		CVFont.Free;
		case TxtXYPos of
			tapBottomLeft:
				X := X - Canvas.TextHeight(Text);
			tapCenterLeft:
				X := X - Canvas.TextHeight(Text) div 2;

			tapBottomCenter: begin
				X := X - Canvas.TextHeight(Text);
				Y := Y + Canvas.TextWidth(Text) div 2;
			end;
			tapCenter: begin
				X := X - Canvas.TextHeight(Text) div 2;
				Y := Y + Canvas.TextWidth(Text) div 2;
			end;
			tapTopCenter:
				Y := Y + Canvas.TextWidth(Text) div 2;

			tapTopRight:
				Y := Y + Canvas.TextWidth(Text);
			tapCenterRight: begin
				X := X - Canvas.TextHeight(Text) div 2;
				Y := Y + Canvas.TextWidth(Text);
			end;
			tapBottomRight: begin
				X := X - Canvas.TextHeight(Text);
				Y := Y + Canvas.TextWidth(Text);
			end;
		end;
		Canvas.TextOut(X, Y, Text);
		Canvas.Font.Assign(SaveFont);
	finally
		SaveFont.Free;
	end;
end;

//---------------------------------------------------------------------------------------
function BevelsToBorder(BVInner, BVOuter: TBevelCut): NTSBorderStyle;
begin
	case BVInner of
		bvNone: begin
			case BVOuter of
				bvNone:	Result := brdrNone;
				bvLowered: Result := brdrLowered;
				bvRaised: Result := brdrRaised;
				else Result := brdrLine; //bvSpace
			end;
		end;
		bvLowered: begin
			case BVOuter of
				bvNone:	Result := brdrLowered;
				bvLowered: Result := brdrLowered;
				bvRaised: Result := brdrBump;
				else Result := brdrLowered; //bvSpace
			end;
		end;
		bvRaised: begin
			case BVOuter of
				bvNone:	Result := brdrRaised;
				bvLowered: Result := brdrEtched;
				bvRaised: Result := brdrRaised;
				else Result := brdrRaised; //bvSpace
			end;
		end;
		else begin //bvSpace
			case BVOuter of
				bvNone:	Result := brdrLine;
				bvLowered: Result := brdrLowered;
				bvRaised: Result := brdrRaised;
				else Result := brdrLine; //bvSpace
			end;
		end;
	end;
end;

//---------------------------------------------------------------------------------------
procedure BorderToBevels(BorderStyle: NTSBorderStyle; var BVInner, BVOuter: TBevelCut);
begin
	case BorderStyle of
		brdrNone: begin
			BVInner := bvNone;
			BVOuter := bvNone;
		end;
		brdrLine: begin
			BVInner := bvSpace;
			BVOuter := bvSpace;
		end;
		brdrRaised: begin
			if BVInner = bvRaised then
				BVOuter := bvNone
			else begin
				BVInner := bvNone;
				BVOuter := bvRaised;
			end;
		end;
		brdrLowered: begin
			if BVInner = bvLowered then
				BVOuter := bvNone
			else begin
				BVInner := bvNone;
				BVOuter := bvLowered;
			end;
		end;
		brdrBump: begin
			BVInner := bvLowered;
			BVOuter := bvRaised;
		end;
		brdrEtched: begin
			BVInner := bvRaised;
			BVOuter := bvLowered;
		end;
	end;
end;

//***************************************************************************************
function LoadTemporaryFont(FontFile: string): Boolean;
begin
	Result := FileExists(FontFile)
  			and (AddFontResourceEx(PAnsiChar(FontFile), FR_PRIVATE, nil) <> 0);
end;

//***************************************************************************************
function UnloadTemporaryFont(FontFile: string): Boolean;
begin
	Result := FileExists(FontFile)
  			and (RemoveFontResourceEx(PAnsiChar(FontFile), FR_PRIVATE, nil) <> 0);
end;

//***************************************************************************************
procedure InitDefaultFonts;
begin
	DefaultFont := TFont.Create;
	TSStdFont := TFont.Create;
	with TSStdFont do begin
		Size := 9;
		Name := 'Verdana';
		Color := clWindowText;
		Style := [];
	end;
	TSBoldFont := TFont.Create;
	with TSBoldFont do begin
		Size := 9;
		Name := 'Verdana';
		Color := clWindowText;
		Style := [fsBold];
	end;
	TSInvertFont := TFont.Create;
	with TSInvertFont do begin
		Size := 9;
		Name := 'Verdana';
		Color := $00E0E0E0;
		Style := [fsBold];
	end;
end;

//***************************************************************************************
procedure FreeDefaultFonts;
begin
	FreeAndNil(TSInvertFont);
	FreeAndNil(TSStdFont);
	FreeAndNil(TSBoldFont);
	FreeAndNil(DefaultFont);
end;

initialization
	InitDefaultFonts;

finalization
	FreeDefaultFonts;


end.






