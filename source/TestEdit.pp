unit TestEdit;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, ExtCtrls, Controls,
	Classes, SysUtils;

type

	{ TTSNewEdit }

  TTSNewEdit = class(TCustomControl)
  private
    InternalEdit: TCustomEdit;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetParent(NewParent: TWinControl); override;

	end;

procedure Register;

implementation

uses
	CompanyConstants;

procedure Register;
begin
  RegisterComponents(DefaultComponentPage, [TTSNewEdit]);
end;

{ TTSNewEdit }

constructor TTSNewEdit.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
  InternalEdit := TCustomEdit.Create(Self);
end;

procedure TTSNewEdit.SetParent(NewParent: TWinControl);
begin
	inherited SetParent(NewParent);
  if Assigned(NewParent) and not Assigned(InternalEdit.Parent) then
		with InternalEdit do
	  begin
      BorderSpacing.Left := 5;
      BorderSpacing.Right := 10;
	  	Parent := Self;
      Align := alClient;
	    BorderStyle := bsNone;
	    //Color := Self.Color;
			Font := Self.Font;
		end;
end;

end.

