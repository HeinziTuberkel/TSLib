unit TS_FileINI;

{$mode objfpc}{$H+}

interface

uses
  TS_StructuredFileName,
  TS_CustomINI, IniFiles,
	Classes, SysUtils;

type

	//Characters that need to be replaced by tokens (see const-arrays) when written to
	// or read from INI-File (the Backspace character (\\ <-> \) is handled seperatley,
	//																see ParseRead and ParseWrite).
	NINIReplacementChar = (ircCRLF, ircReturn, ircLinefeed, ircTab, irBackspace, ircBeep, ircQuote);


	TTSFileINI = class(TTSCustomINI)
	private
		fInternalINIFile: TINIFile;
    fDescriptionFile: TINIFile;
		fAutoClose: Boolean;
		fINIFile: TTSStructuredFileName;
    fCheckFilePath: Boolean;
		procedure SetAutoClose(const Value: Boolean);
		procedure SetINIFile(const Value: TTSStructuredFileName);
    function GetFullPath: string;
    procedure SetFullPath(const Value: string);
    procedure FileNameChanged(Sender: TObject);
	protected
		function Load(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType, Default: string): string; override;
		procedure Save(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType, Value: string); override;
    procedure PutDescription(const Group, Entry: string; const Index: Integer; ADescription: string); override;
    function GetDescription(const Group, Entry: string; const Index: Integer): string; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure CloseINIFile;
		procedure OpenINIFile;
		function CheckINIFile: Boolean;
		function INIFileActive: Boolean;
		function ParseRead(const ReadINIStr: string): string;
		function ParseWrite(const INIStrToWrite: string): string;
	published
		property INIFile: TTSStructuredFileName read fINIFile write SetINIFile;
    property FullPath: string read GetFullPath write SetFullPath stored False;
		property AutoCloseINIFile: Boolean read fAutoClose write SetAutoClose default TRUE;
		property ActiveGroup;
    property LookupSeparators;
    property UseReadCache;
    property UseWriteCache;

		property OnMissingEntry;
		property OnChangeGroup;
		property OnGetValue;
		property OnPutValue;
	end;

const
	iniReplaceCharIntern: array[NINIReplacementChar] of string = (#13#10, #13, #10, #9, #8, #7, '"');
	iniReplaceCharExtern: array[NINIReplacementChar] of string = ('\n', '\r', '\l', '\t', '\b', '\7', '\"');


procedure Register;

implementation

uses
  LResources,
  CompanyConstants,
  StrUtils;

procedure Register;
begin
  {$I TS_FileINI.lrs}
  RegisterComponents(DefaultComponentPage, [TTSFileINI]);
end;


//***************************************************************************************
//***************************************************************************************
{ TTSFileINI }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSFileINI.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  fCheckFilePath := True;
  fINIFile := TTSStructuredFileName.Create;
  fINIFile.Options := fINIFile.Options + [fnoExtIsINI];
  fINIFile.OnChange := @FileNameChanged;
	fAutoClose := True;
	//Wäre "RaiseOnPut"<>False würde beim Schreiben von Werten im "inherited"-Aufruf
	//	eventuell eine Exception ausgelöst.
	RaiseOnPut := False;
end;

//***************************************************************************************
destructor TTSFileINI.Destroy;
begin
	CloseINIFile;
  fINIFile.Free;
	inherited;
end;

procedure TTSFileINI.FileNameChanged(Sender: TObject);
begin
	CloseINIFile;
  fCheckFilePath := True;
end;

//***************************************************************************************
procedure TTSFileINI.OpenINIFile;
var
  DFName: string;
begin
  if fCheckFilePath then
  begin
    fCheckFilePath := False;
    ForceDirectories(fINIFile.ActualDirectory);
  end;
	if not Assigned(fInternalINIFile) then
		fInternalINIFile := TIniFile.Create(fINIFile.FullPath);
  if not Assigned(fDescriptionFile) then
  begin
    DFName := fINIFile.FileName + '.dsc';
    if not Assigned(fDescriptionFile) then
      fDescriptionFile := TIniFile.Create(DFName);
  end;
end;

//***************************************************************************************
procedure TTSFileINI.CloseINIFile;
begin
	FreeAndNil(fInternalINIFile);
  FreeAndNil(fDescriptionFile);
end;

//***************************************************************************************
function TTSFileINI.CheckINIFile: Boolean;
begin
	try
		Result := True;
		OpenINIFile;
		fInternalINIFile.WriteString('Test', 'Test', 'Test');
		fInternalINIFile.EraseSection('Test');
	except
		Result := False;
	end;
	CloseINIFile;
end;

//***************************************************************************************
function TTSFileINI.INIFileActive: Boolean;
begin
	Result := Assigned(fInternalINIFile) and Assigned(fDescriptionFile);
end;

//***************************************************************************************
function TTSFileINI.Load(const Group, Entry: string; const Index: Integer;
                const Encrypted: Boolean; const ValType, Default: string): string;
const
	tsDefaultINIEntry = '<TS: INI entry not found>';
begin
	OpenINIFile;
	if Index <> 0 then
		Result := fInternalINIFile.ReadString(Group, Entry + ' <' + IntToStr(Index) + '>', tsDefaultINIEntry)
	else
		Result := fInternalINIFile.ReadString(Group, Entry, tsDefaultINIEntry);
	if Result = tsDefaultINIEntry then
		Result := inherited Load(Group, Entry, Index, Encrypted, ValType, Default);
	Result := ParseRead(Result);
end;

//***************************************************************************************
procedure TTSFileINI.Save(const Group, Entry: string; const Index: Integer;
                          const Encrypted: Boolean; const ValType, Value: string);
begin
//	inherited Save(Group, Entry, Index, Encrypted, ValType, Value);
	if not INIFileActive then
		OpenINIFile;
	if Index <> 0 then
		fInternalINIFile.WriteString(Group, Entry + ' <' + IntToStr(Index) + '>', ParseWrite(Value))
	else
		fInternalINIFile.WriteString(Group, Entry, ParseWrite(Value));
	if fAutoClose then
		CloseINIFile;
end;

//***************************************************************************************
procedure TTSFileINI.SetAutoClose(const Value: Boolean);
begin
	fAutoClose := Value;
	if not Value and INIFileActive then
		CloseINIFile;
end;

//***************************************************************************************
procedure TTSFileINI.SetINIFile(const Value: TTSStructuredFileName);
begin
 	CloseINIFile;
	fINIFile.Assign(Value);
  ForceDirectories(fINIFile.ActualDirectory);
end;

//***************************************************************************************
function TTSFileINI.GetFullPath: string;
begin
  Result := fINIFile.FullPath;
end;

//***************************************************************************************
procedure TTSFileINI.SetFullPath(const Value: string);
begin
  fINIFile.FullPath := Value;
end;

//***************************************************************************************
function TTSFileINI.ParseRead(const ReadINIStr: string): string;
var
	N: NINIReplacementChar;
	Bsp: string;
begin
	//To set all \ characters, a temporary change has to be made. Otherwise a string that
	// contains e.g. a Backslash and a 't' like this: '\this' will be written as
	// '\\this' and converted on read to -> '\this' -> '	his'.
	Bsp := '~';
	while Pos(Bsp, ReadINIStr) > 0 do
		Bsp := Bsp + '|';
	Result := AnsiReplaceStr(ReadINIStr, '\\', Bsp);
	for N := Low(NINIReplacementChar) to High(NINIReplacementChar) do
		Result := AnsiReplaceStr(Result, iniReplaceCharExtern[N], iniReplaceCharIntern[N]);
	Result := AnsiReplaceStr(Result, Bsp, '\');
end;

//***************************************************************************************
function TTSFileINI.ParseWrite(const INIStrToWrite: string): string;
var
	N: NINIReplacementChar;
begin
	Result := AnsiReplaceStr(INIStrToWrite, '\', '\\');
	if Result > '' then
		for N := Low(NINIReplacementChar) to High(NINIReplacementChar) do
			Result := AnsiReplaceStr(Result, iniReplaceCharIntern[N], iniReplaceCharExtern[N]);
end;

//***************************************************************************************
function TTSFileINI.GetDescription(const Group, Entry: string; const Index: Integer): string;
var
  DescName: string;
begin
	OpenINIFile;
  DescName := BuildLookup(Group, Entry, Index, False);
  Result := fDescriptionFile.ReadString(DescName, '', '');
  if Assigned(OnGetDescription) then
    OnGetDescription(Self, Group, Entry, Index, Result);
	if fAutoClose then
		CloseINIFile;
end;

//***************************************************************************************
procedure TTSFileINI.PutDescription(const Group, Entry: string; const Index: Integer; ADescription: string);
var
  DescName: string;
begin
  if Assigned(OnSetDescription) then
    OnSetDescription(Self, Group, Entry, Index, ADescription);
	OpenINIFile;
  DescName := BuildLookup(Group, Entry, Index, False);
  fDescriptionFile.WriteString(DescName, '', ADescription);
	if fAutoClose then
		CloseINIFile;
end;


end.

