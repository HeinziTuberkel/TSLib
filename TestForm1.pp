unit TestForm1;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	ColorBox, TS_Panel, StdCtrls;

type

	{ TForm2 }

 TForm2 = class(TForm)
		C: TColorListBox;
		P: TTSPanel;
		procedure CClick(Sender: TObject);
		procedure CSelectionChange(Sender: TObject; User: boolean);
	private
		{ private declarations }
	public
		{ public declarations }
	end;

var
	Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.CClick(Sender: TObject);
begin
  //P.Color := C.Color;
end;

procedure TForm2.CSelectionChange(Sender: TObject; User: boolean);
begin
  P.Color := C.Selected;
end;

end.

