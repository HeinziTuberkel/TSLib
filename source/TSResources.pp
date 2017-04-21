unit TSResources;

interface

function ErrMsg(Msg: string; Params: array of const; AppPos: string=''): string;


resourcestring
  {$include TSMsgResource_de.inc}



implementation

uses
  SysUtils;

function ErrMsg(Msg: string; Params: array of const; AppPos: string): string;
begin
  Result := ErrMsg(AnsiToUTF8(Msg), Params, AppPos);
end;

function ErrMsgUTF8(Msg: string; Params: array of const; AppPos: string): string;
begin
  if Length(Params)>0 then
  	Result := Format(Msg, Params)
  else
    Result := Msg;
  if AppPos > '' then
	  Result := AppPos + ': ' + Result;
end;



end.
