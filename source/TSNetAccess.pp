unit TSNetAccess;

interface

uses
	Classes;

function ExtractServerName(NetPath: string): string;
function ExtractShareName(NetPath: string; IncludeServerName: Boolean=False): string;
function ExtractSharePath(NetPath: string): string;
function OpenConnection(NetPath, User, Password: string; ShowError: Boolean): Integer; overload;
function OpenConnection(NetPath, User, Password: string; out ErrMsg: string): Integer; overload;
function ConnectErrorMessage(ErrNo: Integer): string;

implementation

uses
	Windows,
	SysUtils;

//---------------------------------------------------------------------------------------
function ExtractServerName(NetPath: string): string;
var
	P: Integer;
begin
	Result := '';
	while Pos('\', NetPath) = 1 do
		Delete(NetPath, 1, 1);
	P := Pos('\', NetPath);
	if P>0 then
		Result := '\\' + Copy(NetPath, 1, P-1);
end;

//---------------------------------------------------------------------------------------
function ExtractShareName(NetPath: string; IncludeServerName: Boolean=False): string;
var
	P: Integer;
begin
	Result := '';
	while Pos('\', NetPath) = 1 do
		Delete(NetPath, 1, 1);
	P := Pos('\', NetPath);
	if P>0 then begin
		if IncludeServerName then
			Result := '\\' + Copy(NetPath, 1, P);
		Delete(NetPath, 1, P);
		P := Pos('\', NetPath);
		if P>0 then
			Result := Result + Copy(NetPath, 1, P-1)
		else
			Result := Result + NetPath;
	end;
end;

//---------------------------------------------------------------------------------------
function ExtractSharePath(NetPath: string): string;
var
	P: Integer;
begin
	Result := '';
	while Pos('\', NetPath) = 1 do
		Delete(NetPath, 1, 1);
	P := Pos('\', NetPath);
	if P>0 then
		Result := Copy(NetPath, P, MaxInt)
	else
		Result := '\' + NetPath;
end;

//---------------------------------------------------------------------------------------
function OpenConnection(NetPath, User, Password: string; out ErrMsg: string): Integer; overload;
var
	Res: TNetResource;
begin
	ErrMsg := 'Connection opened successfully';
	with Res do begin
		dwScope := Resource_GlobalNet;
		dwType := ResourceType_Disk;
		dwDisplayType := ResourceDisplayType_Generic;
		dwUsage := ResourceUsage_Connectable;
		lpLocalName := nil;
		lpRemoteName := PChar(NetPath);
		lpComment := nil;
		lpProvider := nil;
	end;
	Result := WNetAddConnection2(Res, PChar(Password), PChar(User), 0);
	if Result <> NO_Error then
		ErrMsg := ConnectErrorMessage(Result);
end;

//---------------------------------------------------------------------------------------
function OpenConnection(NetPath, User, Password: string; ShowError: Boolean): Integer;
var
	ErrMsg: string;
begin
	Result := OpenConnection(NetPath, User, Password, ErrMsg);
	if (Result <> No_Error) and ShowError then
		Result := MessageBox(0, PChar('Fehler bei Aufbau der Verbindung zu ' + NetPath
														+ ': '#13#10 + ErrMsg),
														'Fehler bei Anmeldung',
														MB_AbortRetryIgnore+MB_IconWarning+MB_DEFBUTTON2);
end; //OpenConnection

//---------------------------------------------------------------------------------------
function ConnectErrorMessage(ErrNo: Integer): string;
begin
	case ErrNo of
		No_Error: Result := 'Kein Fehler !?';
		Error_Access_Denied: Result := 'Zugriff verweigert';
		Error_Already_Assigned: Result := 'Gerät ist bereits zugewiesen';
		Error_Bad_Dev_Type: Result := 'Ungültiger Typ für diesen Zugriff';
		Error_Bad_Device: Result := 'Ungültiger Gerätename';
		Error_Bad_Net_Name: Result := 'Ungültiger Netzwerkname';
		Error_Bad_Profile: Result := 'Ungültiges Anwenderprofil';
		Error_Bad_Provider: Result := 'Ungültiger Anbieter für diesen Dienst';
		Error_Busy: Result := 'Netzwerk ausgelastet';
		Error_Cancelled: Result := 'Verbindung wurde unterbrochen';
		Error_Cannot_Open_Profile: Result := 'Anwenderprofil kann nicht geöffnet werden.';
		Error_Device_Already_Remembered: Result := 'Gerät ist bereits registriert';
		Error_Extended_Error: Result := 'Erweiterter Fehler.';
		Error_Invalid_Password: Result := 'Passwort oder Anwendername ungültig';
		Error_No_Net_Or_Bad_Path: Result := 'Kein Netzwerk oder Pfad ungültig';
		Error_No_Network: Result := 'Netzwerk nicht gefunden';
	else
		Result := 'Fehler Nummer ' + IntToStr(ErrNo) + #13#10 + 'Meldung: ' + SysErrorMessage(ErrNo);
	end;
end; //ConnectErrorMessage

end.



