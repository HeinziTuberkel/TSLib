unit TS_RegINI;

{$mode objfpc}{$H+}

interface

uses
  LResources,
	Windows,
  CompanyConstants,
  TS_CustomINI,
	Classes, SysUtils;

type
	NRegINIKey = (rkAppDefault, rkLocalMachine, rkCurrentUser);
	NRegINIPath = (rpAppDefault, rpAppendToDefault, rpUseStringValue);


	TTSRegINI = class(TTSCustomINI)
	private
		fPathType: NRegINIPath;
		fPathValue: string;
		fRegKey: NRegINIKey;
		procedure SetPathType(const Value: NRegINIPath);
		procedure SetPathValue(const Value: string);
		procedure SetRegKey(const Value: NRegINIKey);
		function ReadRegINI(const RegPath: string; StartKey: HKey; var Value: string): Boolean;
		function ChkPath(APath: string): string;
		function WriteRegINI(const RegPath: string; StartKey: HKey; const Value: string): Integer;
	protected
		function Load(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
	                      const ValType, Default: string): string; override;
		procedure Save(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
	                      const ValType, Value: string); override;
	public
		constructor Create(AOwner: TComponent); override;
		function Key: HKEY;
		function Path: string;
	published
		property RegKey: NRegINIKey read fRegKey write SetRegKey default rkAppDefault;
		property RegPathType: NRegINIPath read fPathType write SetPathType default rpAppendToDefault;
		property PathValue: string read fPathValue write SetPathValue;
		property ActiveGroup;
		property OnMissingEntry;
		property OnChangeGroup;
		property OnGetValue;
		property OnPutValue;
	end;


const
	HKLM = HKey_Local_Machine;
	HKCU = HKey_Current_User;

var
  AppRegKey: HKey = HKCU;
  AppRegPath: string = 'Software/' + DefaultCompanyName;

procedure Register;

implementation


uses
	TSLib;

procedure Register;
begin
  {$I TS_RegINI.lrs}
  RegisterComponents(DefaultComponentPage, [TTSRegINI]);
end;


//***************************************************************************************
//***************************************************************************************
{ TTSRegINI }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSRegINI.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	//Wäre "RaiseOnPut"<>False würde beim Schreiben von Werten im "inherited"-Aufruf
	//	eventuell eine Exception ausgelöst.
	RaiseOnPut := False;
	fRegKey := rkAppDefault;
	fPathType := rpAppendToDefault;
	fPathValue := '';
	ActiveGroup := '';
end;

//***************************************************************************************
function TTSRegINI.ChkPath(APath: string): string;
begin
  if APath > '' then
  	Result := Trailing(Back2Slash(APath))
  else
    Result := '';
end;

//***************************************************************************************
function TTSRegINI.Key: HKEY;
begin
	case fRegKey of
		rkLocalMachine:
			Result := HKLM;
		rkCurrentUser:
			Result := HKCU;
		else //rkAppDefault :
			Result := AppRegKey;
	end;
end;

//***************************************************************************************
function TTSRegINI.Path: string;
begin
	case fPathType of
		rpAppDefault:
			Result := ChkPath(AppRegPath);
		rpUseStringValue:
			Result := ChkPath(fPathValue);
		else // rpAppendToDefault:
			Result := ChkPath(ChkPath(AppRegPath) + fPathValue);
	end;
end;

//***************************************************************************************
function TTSRegINI.ReadRegINI(const RegPath: string; StartKey: HKey; var Value: string): Boolean;
var
	ThisKey, SubPath: string;
	KeyHandle: HKey;
	PVal: PByte;
	ValType, ValLength: DWord;
	ErrorCode: LongInt;
	I: Integer;
begin
	I := Pos('/', RegPath);
	SubPath := RegPath;
	ThisKey := Copy(RegPath, 1, I-1);
	Delete(SubPath, 1, I);

	if ThisKey = '' then
	begin
		RegQueryValueEx(StartKey, PChar(SubPath), nil, @ValType, nil, @ValLength);
		GetMem(PVal, ValLength+1);
		try
			if RegQueryValueEx(StartKey, PChar(SubPath), nil, @ValType, PVal, @ValLength) = ERROR_SUCCESS
			then begin
				case ValType of
					REG_DWORD:
						Value := IntToStr(PDWord(PVal)^);
					REG_BINARY: begin
						Value := '';
						for I := 0 to pred(ValLength) do
							Value := Value + IntToHex(Integer(PChar(PVal)[I]), 2);
					end;
					REG_SZ:
						Value := string(PChar(PVal));
				end;
				Result := True;
			end
			else
				Result := False;
		finally
			FreeMem(PVal);
		end;
	end
	else begin
		ErrorCode := RegOpenKeyEx(StartKey, PChar(ThisKey), 0, KEY_READ, KeyHandle);
		if ErrorCode = Error_Success then begin
			Result := ReadRegINI(SubPath, KeyHandle, Value);  //Rekursiver aufruf mit dem "Rest"-Pfad
			RegCloseKey(KeyHandle);
		end
		else
			Result := False;
	end;
end; //ReadRegStr

//***************************************************************************************
function TTSRegINI.Load(const Group, Entry: string; const Index: Integer;
  const Encrypted: Boolean; const ValType, Default: string): string;
var
	RegPath: string;
begin
	RegPath := Path + ChkPath(Group) + Entry;
	if Index <> 0 then
		RegPath := RegPath + IntToStr(Index);

	if not ReadRegINI(RegPath, Key, Result) then
		Result := inherited Load(Group, Entry, Index, Encrypted, ValType, Default);
end;


//***************************************************************************************
function TTSRegINI.WriteRegINI(const RegPath: string; StartKey: HKey; const Value: string): Integer;
var
	ThisKey, SubPath: string;
	KeyHandle: HKey;
	I: Integer;
	Dispo: DWord;
begin
	I := Pos('/', RegPath);
	SubPath := RegPath;
	ThisKey := Copy(RegPath, 1, I-1);
	Delete(SubPath, 1, I);

	if ThisKey = '' then
	begin
		Result := RegSetValueExW(StartKey, PWideChar(SubPath), 0, REG_SZ,
								PChar(Value), Length(Value)*2);
	end
	else begin
		Result := RegCreateKeyEx(StartKey, PChar(ThisKey), 0, 'REG_SZ', REG_OPTION_NON_VOLATILE,
														KEY_CREATE_SUB_KEY or KEY_ENUMERATE_SUB_KEYS or Key_Execute
														or KEY_QUERY_VALUE or KEY_SET_VALUE, nil, KeyHandle, @Dispo);
		if Result = Error_Success then
    begin
			Result := WriteRegINI(SubPath, KeyHandle, Value);
			RegCloseKey(KeyHandle);
		end;
	end;
end; //WriteRegStr

//***************************************************************************************
procedure TTSRegINI.Save(const Group, Entry: string; const Index: Integer;
  const Encrypted: Boolean; const ValType, Value: string);
var
	RegPath: string;
begin
	RegPath := Path + ChkPath(Group) + Entry;
	if Index <> 0 then
		RegPath := RegPath + IntToStr(Index);
	WriteRegINI(RegPath, Key, Value);
end;

procedure TTSRegINI.SetPathType(const Value: NRegINIPath);
begin
	fPathType := Value;
end;

//***************************************************************************************
procedure TTSRegINI.SetPathValue(const Value: string);
begin
	fPathValue := ChkPath(Value);
end;

//***************************************************************************************
procedure TTSRegINI.SetRegKey(const Value: NRegINIKey);
begin
	fRegKey := Value;
end;


end.



