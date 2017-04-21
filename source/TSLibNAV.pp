unit TSLibNAV;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

const
	NULLDate: TDateTime = -53688;


procedure NAVChangeCompany(SQL: TStrings; const FromCompany, ToCompany: string); overload;
function NAVChangeCompany(SQL: string; const FromCompany, ToCompany: string): string; overload;
procedure NAVChkCompanyName(var CName: string);


implementation


procedure NAVChkCompanyName(var CName: string);
begin
	CName := StringReplace(CName, '.', '_', [rfIgnoreCase, rfReplaceAll]);
	CName := StringReplace(CName, '''', '_', [rfIgnoreCase, rfReplaceAll]);
end;

procedure NAVChangeCompany(SQL: TStrings; const FromCompany, ToCompany: string);
begin
	SQL.Text := StringReplace(SQL.Text, FromCompany+'$', ToCompany+'$', [rfIgnoreCase, rfReplaceAll]);
end;

function NAVChangeCompany(SQL: string; const FromCompany, ToCompany: string): string; overload;
begin
	Result := StringReplace(SQL, FromCompany+'$', ToCompany+'$', [rfIgnoreCase, rfReplaceAll]);
end;


end.

