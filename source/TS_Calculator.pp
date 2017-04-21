unit TS_Calculator;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
	ExtCtrls, StdCtrls, TS_Edit;

type

	{ TCalculator }

 TCalculator = class(TForm)
		Panel1: TPanel;
		Btn7: TSpeedButton;
		Btn0: TSpeedButton;
		BtnKomma: TSpeedButton;
		BtnAdd: TSpeedButton;
		BtnEqual: TSpeedButton;
		BtnCancel: TSpeedButton;
		BtnOK: TSpeedButton;
		BtnClear: TSpeedButton;
		BtnDiv: TSpeedButton;
		BtnMul: TSpeedButton;
		Btn8: TSpeedButton;
		BtnSub: TSpeedButton;
		Btn9: TSpeedButton;
		Btn4: TSpeedButton;
		Btn5: TSpeedButton;
		Btn6: TSpeedButton;
		Btn1: TSpeedButton;
		Btn2: TSpeedButton;
		Btn3: TSpeedButton;
		EdValue: TTSNumEdit;
		BtnSign: TSpeedButton;
		BtnBsp: TSpeedButton;
		BtnPercent: TSpeedButton;
		BtnRcp: TSpeedButton;
		BtnSqr: TSpeedButton;
		BtnSqrt: TSpeedButton;
	private
		fBtnSep: Integer;
		fBtnSize: Integer;
		procedure SetBtnSep(AValue: Integer);
		procedure SetBtnSize(AValue: Integer);
	public
     property BtnSize: Integer read fBtnSize write SetBtnSize;
     property BtnSeparation: Integer read fBtnSep write SetBtnSep;

	end;

var
	Calculator: TCalculator;

implementation

{$R *.lfm}

{ TCalculator }

procedure TCalculator.SetBtnSep(AValue: Integer);
begin
	if fBtnSep = AValue then Exit;
	fBtnSep := AValue;
  BtnClear.BorderSpacing.Top := fBtnSep;
  Btn7.BorderSpacing.Top := fBtnSep;
  Btn4.BorderSpacing.Top := fBtnSep;
  Btn1.BorderSpacing.Top := fBtnSep;
  Btn0.BorderSpacing.Top := fBtnSep;
  BtnDiv.BorderSpacing.Left := fBtnSep;
  BtnMul.BorderSpacing.Left := fBtnSep;
  BtnSub.BorderSpacing.Left := fBtnSep;
  EdValue.Width := BtnSub.Left + BtnSub.Width - EdValue.Left;
end;

procedure TCalculator.SetBtnSize(AValue: Integer);
begin
	if (fBtnSize = AValue) or (AValue < 10) then
  	Exit;
	fBtnSize := AValue;
	BtnClear.Width := AValue;
  BtnDiv.Width := AValue;
  BtnMul.Width := AValue;
  BtnSub.Width := AValue;
  BtnClear.Height := AValue;
  Btn7.Height := AValue;
  Btn4.Height := AValue;
  Btn1.Height := AValue;
  Btn0.Height := AValue;
  BtnCancel.Height := AValue;
  EdValue.Width := BtnSub.Left + BtnSub.Width - EdValue.Left;
end;

end.

