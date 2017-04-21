unit TSLibResources;

interface

//var
//	TimeStampFormat: string = 'dd.mm.yyyy hh:nn:ss,zzz';

function FmtMsg(Msg: string; Args: array of const): string; overload;
function FmtMsg(Msg: string; StrArg: string): string; overload;
function FmtMsg(Msg: string; IntArg: LongInt): string; overload;
function FmtMsg(Msg: string; FloatArg: Extended): string; overload;

function FileVersion(const FileName: string; PartMask: Byte=$0F): string;
function FileBuild(const FileName: string): Integer;
function ProgrammVersion: string; overload;
function ProgramBuild: Integer;



implementation

uses
	Windows,
	Forms,
	SysUtils;

function FmtMsg(Msg: string; Args: array of const): string; overload;
begin
	try
		Result := Format(Msg, Args);
	except
		Result := Result + ' <Params missing>';
	end;
end;

function FmtMsg(Msg: string; StrArg: string): string; overload;
begin
	Result := FmtMsg(Msg, [StrArg]);
end;

function FmtMsg(Msg: string; IntArg: LongInt): string; overload;
begin
	Result := FmtMsg(Msg, [IntArg]);
end;

function FmtMsg(Msg: string; FloatArg: Extended): string; overload;
begin
	Result := FmtMsg(Msg, [FloatArg]);
end;


//---------------------------------------------------------------------------------------
function ProgrammVersion: string;
begin
	Result := FileVersion(Application.ExeName);
end;

//---------------------------------------------------------------------------------------
function FileVersion(const FileName: string; PartMask: Byte=$0F): string;
var
	BufSize: Integer;
	VerInfo: PChar;
	VerData: PVSFixedFileInfo;
	H, L, Dummy, VerLength: Cardinal;
	Success: Boolean;
begin
	BufSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
	if BufSize > 0 then
	begin
		GetMem(VerInfo, BufSize);
		try
			Success := GetFileVersionInfo(PChar(FileName), Dummy, BufSize, VerInfo);
			if Success then begin
				Success := VerQueryValue(VerInfo, '\', Pointer(VerData), VerLength);
				if Success then begin
					H := VerData^.dwFileVersionMS;
					L := VerData^.dwFileVersionLS;
					if (PartMask and 8) > 0  then
						Result := IntToStr((H and $FFFF0000) shr 16) + '.'
					else
						Result := '';
					if (PartMask and 4) > 0 then
						Result := Result + IntToStr(H and $0FFFF) + '.';
					if (PartMask and 2) > 0 then
						Result := Result + IntToStr((L and $FFFF0000) shr 16) + '.';
					if (PartMask and 1) > 0 then
						Result := Result + IntToStr(L and $0FFFF);
				end;
			end;
		finally
			FreeMem(VerInfo);
		end;
	end;
end;

//---------------------------------------------------------------------------------------
function FileBuild(const FileName: string): Integer;
begin
	Result := StrToInt(FileVersion(FileName, 1));
end;

//---------------------------------------------------------------------------------------
function ProgramBuild: Integer;
begin
	Result := FileBuild(Application.ExeName);
end;



end.

