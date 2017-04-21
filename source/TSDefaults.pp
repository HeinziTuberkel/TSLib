unit TSDefaults;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Graphics,
	TSLibGraphics;

const
  tsDefPnlCaption = '';
  tsDefPnlBvlSides: SBevelSides = [];
  tsDefPnlBvlWidth = 1;
  tsDefPnlBorderStyle: NTSBorderStyle = brdrNone;
  tsDefPnlBorderWidth = 1;
  tsDefPnlParentFont = True;
  tsDefPnlTextAlign: NTxtAlignPos = tapCenter;

  tsDefBckParentColor = True;
  tsDefBckColor = clDefault;

  tsDefLbVisible = True;
  tsDefLbCaption = '';
  tsDefLbTransparent = False;
  tsDefLbWordWrap = False;
  tsDefLbAlignment = taLeftJustify;
  tsDefLbLayout = tlCenter;
  tsDefLbParentColor = True;
  tsDefLbColor = clNone;
  tsDefLbParentFont = True;

	function tsDefFont: TFont;
	function IsTSDefaultFont(TheFont: TFont): Boolean;


implementation

uses
	SysUtils;

var
	defFont: TFont;

function tsDefFont: TFont;
begin
	if not Assigned(defFont) then
	begin
		defFont := TFont.Create;
		//All Font properties that deviate from the system default should be assigned here.
		//Name := 'Tahoma';
		//Size := 10;
		//Color := clNavy
		// ... etc.
	end;
	Result := defFont;
end;

function IsTSDefaultFont(TheFont: TFont): Boolean;
begin
  if not Assigned(defFont) then
  	Result := TheFont.IsDefault
  else begin
  	Result := (TheFont.Name = defFont.Name)
						and (TheFont.Color = defFont.Color)
						and (TheFont.Size = defFont.Size)
						and (TheFont.Style = defFont.Style)
						and (TheFont.CharSet = defFont.CharSet)
						and (TheFont.Height = defFont.Height)
						and (TheFont.Orientation = defFont.Orientation)
						and (TheFont.Pitch = defFont.Pitch)
						and (TheFont.Quality = defFont.Quality);
	end;
end;


initialization
	defFont := nil;

finalization
	FreeAndNil(defFont);


end.

