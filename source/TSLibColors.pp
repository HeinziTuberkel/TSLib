unit TSLibColors;
{
	How to add your own colors to the IDE:

		1) Add the color constant to the "const" section below
		2) Add the color value and name to the TSColorMap array
		3) Count the number of Items in TSColorMap and set TSColorCount accordingly.

	Remember to add a reference to this unit to every form unit that uses the
	newly defined color codes.
}

interface

uses
	Classes,
	Graphics;

const
	//Beim Hinzufügen neuer Farbwerte müssen die Konstanten ColorCount
	// und das Array TSColorMap im Implementation-Teil angepasst werden.
	clTSRed = TColor($005652EE); //5657327
	clTSYellow = TColor($0080FFFE); //8454143
	clTSGreen = TColor($004BF562); //4978017
	clTSBlue = TColor($00FB8B6B); //16485226
	clTSOcean = TColor($00C6A800);
	clTSOrange = TColor($0017BAFF);
	clTSBrown = TColor($000053A6);
	clTSWhite = TColor($00FEFEFE); //16776959
	clTSBackground = TColor($00DCDCDC); //13226196
	clTSReadOnly = TColor($00C9C9CA); //15132390
	clTSGray = TColor($00BCBCBD); //12369084
	clTSLightGray = TColor($00E0E0E0);
	clTSPaleGray = TColor($00F0F0F0);
  clTSDarkGray = TColor($00303030);
	clTSPaleYellow = TColor($00C8FFFF);
	clTSSplitter = TColor($00A33829);

function TSColorToIdent(Color: Longint; var Ident: string): Boolean;
function TSIdentToColor(const Ident: string; var Color: Longint): Boolean;
function TSStringToColor(const S: string): TColor;
function TSStringToColorDef(const S: string; Default: TColor): TColor;
function TSColorToString(Color: TColor): string;
procedure TSGetColorValues(Proc: TGetStrProc);
procedure InitTSColorMap;


var
	TSColorMap: array of TIdentMapEntry;


implementation

uses
	SysUtils;

procedure Log(Msg: string);
var
	T: Textfile;
begin
	AssignFile(T, '\Clrs.log');
	if FileExists('\Clrs.log') then
		Append(T)
	else
		Rewrite(T);
	Writeln(T, FormatDateTime('hh:nn:ss', now) + ':' +Msg);
	CloseFile(T);
end;


function TSColorToIdent(Color: Longint; var Ident: string): Boolean;
begin
	Result := ColorToIdent(Color, Ident);
	if not Result then
		Result := IntToIdent(Color, Ident, TSColorMap);
end;

function TSIdentToColor(const Ident: string; var Color: Longint): Boolean;
begin
	Result := IdentToColor(Ident, Color);
	if not Result then
		Result := IdentToInt(Ident, Color, TSColorMap);
end;

function TSStringToColor(const S: string): TColor;
begin
	if not TSIdentToColor(S, Longint(Result)) then
		Result := TColor(StrToInt(S));
end;

function TSStringToColorDef(const S: string; Default: TColor): TColor;
begin
  Result := Default;
  if not TSIdentToColor(S, LongInt(Result)) then
    Result := TColor(StrToIntDef(S, Default));
end;

function TSColorToString(Color: TColor): string;
begin
  Result := '';
  if not TSColorToIdent(Color, Result) then
    Result:='$'+HexStr(Color,8);
end;

procedure TSGetColorValues(Proc: TGetStrProc);
var
	I: Integer;
begin
	for I := Low(TSColorMap) to High(TSColorMap) do
		Proc(TSColorMap[I].Name);
end;

procedure InitTSColorMap;
begin
	SetLength(TSColorMap, 16);
	TSColorMap[0].Value := clTSRed; TSColorMap[0].Name := 'clTSRed';
	TSColorMap[1].Value := clTSYellow; TSColorMap[1].Name := 'clTSYellow';
	TSColorMap[2].Value := clTSGreen; TSColorMap[2].Name := 'clTSGreen';
	TSColorMap[3].Value := clTSBlue; TSColorMap[3].Name := 'clTSBlue';
	TSColorMap[4].Value := clTSOrange; TSColorMap[4].Name := 'clTSOrange';
	TSColorMap[5].Value := clTSOcean; TSColorMap[5].Name := 'clTSOcean';
	TSColorMap[6].Value := clTSBrown; TSColorMap[6].Name := 'clTSBrown';
	TSColorMap[7].Value := clTSWhite; TSColorMap[7].Name := 'clTSWhite';
	TSColorMap[8].Value := clTSGray; TSColorMap[8].Name := 'clTSGray';
	TSColorMap[9].Value := clTSBackground; TSColorMap[9].Name := 'clTSBackground';
	TSColorMap[10].Value := clTSReadOnly; TSColorMap[10].Name := 'clTSReadOnly';
	TSColorMap[11].Value := clTSLightGray; TSColorMap[11].Name := 'clTSLightGray';
	TSColorMap[12].Value := clTSPaleGray; TSColorMap[12].Name := 'clTSPaleGray';
	TSColorMap[13].Value := clTSDarkGray; TSColorMap[13].Name := 'clTSDarkGray';
	TSColorMap[14].Value := clTSSplitter; TSColorMap[14].Name := 'clTSSplitter';
	TSColorMap[15].Value := clTSPaleYellow; TSColorMap[15].Name := 'clTSPaleYellow';
end;

initialization
	InitTSColorMap;





end.

