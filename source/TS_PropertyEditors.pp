unit TS_PropertyEditors;

{$mode objfpc}{$H+}

interface

uses
	Graphics,
	Classes,
	GraphPropEdits,
	PropEdits,
  ComponentEditors;


type
	TTSNumWithNULLProperty = class(TFloatProperty)
	  function GetValue: string; override;
	  procedure SetValue(const Value: string); override;
	end;

	TTSDateWithNULLProperty = class(TDateProperty)
	  function GetValue: string; override;
	  procedure SetValue(const Value: string); override;
	  procedure Edit; override;
	end;

	{ TTSColorPropertyEditor }

  TTSColorPropertyEditor = class(TColorPropertyEditor)
  public
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure ListDrawValue(const CurValue: ansistring; Index: integer; ACanvas: TCanvas;
    													const ARect:TRect; AState: TPropEditDrawState); override;

  end;

	TTSStringsContainerEditor = class(TComponentEditor);

procedure Register;

implementation

uses
	LCLType,
	GraphType,
  SysUtils,
  TSLibColors,
  TSLibDateTime,
  TSLibConvert,
	TS_Edit;

procedure Register;
begin
	RegisterPropertyEditor(TypeInfo(Extended), TTSNumEdit, 'EmptyValue', TTSNumWithNULLProperty);
	RegisterPropertyEditor(TypeInfo(Extended), TTSNumEdit, 'MinNumValue', TTSNumWithNULLProperty);
	RegisterPropertyEditor(TypeInfo(Extended), TTSNumEdit, 'MaxNumValue', TTSNumWithNULLProperty);
	RegisterPropertyEditor(TypeInfo(TDateTime), TTSDateEdit, '', TTSDateWithNULLProperty);
	RegisterPropertyEditor(TypeInfo(TDateTime), TTSDateRangeCalc, '', TTSDateWithNULLProperty);
	RegisterPropertyEditor(TypeInfo(TGraphicsColor), nil, '', TTSColorPropertyEditor);
end;

{ TTSColorPropertyEditor }

function TTSColorPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  Result := TSColorToString(TColor(OrdValue));
end;

procedure TTSColorPropertyEditor.GetValues(Proc: TGetStrProc);
var
  CValue: Longint;
begin
  if not TSIdentToColor(GetVisualValue, CValue) then Proc(GetVisualValue);
  GetColorValues(Proc);
  TSGetColorValues(Proc);
end;

procedure TTSColorPropertyEditor.SetValue(const NewValue: ansistring);
var
  CValue: Longint;
begin
  if TSIdentToColor(NewValue, CValue) then
    SetOrdValue(CValue)
  else
    inherited SetValue(NewValue);
end;

procedure TTSColorPropertyEditor.ListDrawValue(const CurValue: ansistring;
	Index: integer; ACanvas: TCanvas; const ARect: TRect;
	AState: TPropEditDrawState);

	function ColorToBorderColor(AColor: TColorRef): TColor;
	type
	  TColorQuad = record
	    Red,
	    Green,
	    Blue,
	    Alpha: Byte;
	  end;
	begin
	  if (TColorQuad(AColor).Red > 192) or
	     (TColorQuad(AColor).Green > 192) or
	     (TColorQuad(AColor).Blue > 192) then
	    Result := clBlack
	  else
	    if pedsInEdit in AState then
	    begin
	      if pedsSelected in AState then
	        Result := clWindow
	      else
	       Result := TColor(AColor);
	    end else
	    begin
	      if pedsSelected in AState then
	        Result := clHighlight
	      else
	       Result := clWindow;
	    end;
	end;
var
	vRight, vBottom: Integer;
	vOldPenColor, vOldBrushColor: TColor;
	vOldPenStyle: TPenStyle;
	S: string;

begin
	inherited ListDrawValue(CurValue, Index, ACanvas, ARect, AState);

  vRight := (ARect.Bottom - ARect.Top) + ARect.Left - 2;
  vBottom:=ARect.Bottom-2;
  with ACanvas do
  begin
    // save off things
    vOldPenStyle := Pen.Style;
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    Brush.Color := TSStringToColorDef(CurValue,clNone);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, vBottom - 1);

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
    Pen.Style := vOldPenStyle;
  end;

end;


{ TTSNumWithNULLProperty }

function TTSNumWithNULLProperty.GetValue: string;
var
  E: Extended;
begin
  E := GetFloatValue;
  if E = TSNumNULL then
    Result := TSNumNULLName
  else
    Result := inherited GetValue;
end;

procedure TTSNumWithNULLProperty.SetValue(const Value: string);
begin
  if SameText(Value, TSNumNULLName)
    or SameText(Value, 'NULL')
    or (Value = '')
  then
    SetFloatValue(TSNumNULL)
  else
    inherited SetValue(Value);
end;

{ TTSDateWithNULLProperty }

procedure TTSDateWithNULLProperty.Edit;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = TSDateNULL then
    SetFloatValue(0);
  inherited Edit;
end;

function TTSDateWithNULLProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = TSDateNULL then
    Result := TSDateNULLName
  else
    Result := inherited GetValue;
end;

procedure TTSDateWithNULLProperty.SetValue(const Value: string);
var
  ValDate: TDateTIme;
begin
  if SameText(Value, TSDateNULLName)
    or SameText(Value, 'NULL')
    or (Value = '')
  then
    SetFloatValue(TSDateNULL)
  else
    SetFloatValue(EvalDate(Value));

//  begin
//    ValDate := EvalDate(Value);
//ShowMessage('Val:'+value + ', d:' + datetostr(ValDate));
//if ValDate = date then
//begin
//ShowMessage('setze date');
//    SetFloatValue(date);
//ShowMessage('setze Valdate');
//    SetFloatValue(ValDate);
//ShowMessage('ValdAte gesetzt');
//
//end
//else
//    SetFloatValue(ValDate);
////inherited SetValue(Value);

//  end;
end;



end.

