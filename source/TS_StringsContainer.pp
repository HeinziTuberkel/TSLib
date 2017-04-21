unit TS_StringsContainer;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

type
	TTSStringsContainer = class;
	MActiveListChanging = procedure (Sender: TTSStringsContainer;
																var NewIndex: Integer; var AllowChange: Boolean) of object;

	TTSStringsContainer = class(TComponent)
	private
		fListNames: TStrings;
		fLists: array of TStringList;
		fActiveIndex: Integer;
		fTmpIndex: Integer;

		fSLCount: Integer;
		fActiveListChanged: TNotifyEvent;
		fActiveListChanging: MActiveListChanging;
		function GetActiveName: string;
		function GetActiveStrings: TStrings;
		procedure SetActiveName(const Value: string);
		procedure SetActiveStrings(const Value: TStrings);
		procedure SetSLCount(const Value: Integer);
		function GetStrings(Index: Integer): TStrings;
		procedure SetStrings(Index: Integer; const Value: TStrings);
		function GetByName(AListName: string): TStrings;
		function GetListName(Index: Integer): string;
		procedure SetByName(AListName: string; const Value: TStrings);
		procedure SetListName(Index: Integer; const Value: string);
		procedure SetActiveIndex(Value: Integer);
		function GetActiveStr(Index: Integer): string;
		procedure SetActiveStr(Index: Integer; const Value: string);
		function DefaultName(Index: Integer): string;
		procedure SetListNames(const Value: TStrings);
		function GetListNames: TStrings;
		function GetSLCount: Integer;
	protected
		procedure DefineProperties(Filer: TFiler); override;
		procedure ReadLists(Reader: TReader); virtual;
		procedure WriteLists(Writer: TWriter); virtual;
		procedure Loaded; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Assign(Source: TPersistent); override;
		function Exists(AListName: string): Boolean;
		function AddList(AListName: string): Integer; overload;
		function AddList(List: TStrings): Integer; overload;
		function AddList(AListName: string; List: TStrings): Integer; overload;
		function ActiveCount: Integer;
    function IndexOf(AListName: string): Integer;
		procedure DeleteList(Index: Integer);

		property List[Index: Integer]: TStrings read GetStrings write SetStrings;
		property ListName[Index: Integer]: string read GetListName write SetListName;
		property ListByName[AListName: string]: TStrings read GetByName write SetByName; default;
		property ActiveString[Index: Integer]: string read GetActiveStr write SetActiveStr;
	published
		property ListNames: TStrings read GetListNames write SetListNames;
		property ListCount: Integer read GetSLCount write SetSLCount default 1;
		property ActiveList: TStrings read GetActiveStrings write SetActiveStrings stored False;
		property ActiveListName: string read GetActiveName write SetActiveName stored False;
		property ActiveListIndex: Integer read fActiveIndex write SetActiveIndex default 0;
		property OnActiveListChanging: MActiveListChanging read fActiveListChanging write fActiveListChanging;
		property OnActiveListChanged: TNotifyEvent read fActiveListChanged write fActiveListChanged;
	end;


procedure Register;


implementation

uses
  LResources,
	TSLib,
  CompanyConstants;

procedure Register;
begin
	{$I TS_StringsContainer.lrs}
  RegisterComponents(DefaultComponentPage, [TTSStringsContainer]);
end;

//***************************************************************************************
{ TTSStringsContainer }
//***************************************************************************************

//***************************************************************************************
constructor TTSStringsContainer.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	fListNames := TStringList.Create;
	SetLength(fLists, 0);
	fSLCount := 0;
	AddList(nil);
	fActiveIndex := 0;
end;

//***************************************************************************************
destructor TTSStringsContainer.Destroy;
begin
	while fSLCount>0 do begin
		Dec(fSLCount);
		fLists[fSLCount].Free;
	end;
	fListNames.Free;
	inherited Destroy;
end;

//***************************************************************************************
procedure TTSStringsContainer.DefineProperties(Filer: TFiler);
begin
	inherited DefineProperties(Filer);
	Filer.DefineProperty('ContainedLists', @ReadLists, @WriteLists, fSLCount > 0);
end;

//***************************************************************************************
procedure TTSStringsContainer.ReadLists(Reader: TReader);
var
	I: Integer;

	procedure ReadSL(Idx: Integer);
	begin
		fListNames[Idx] := Reader.ReadString;
		Reader.ReadListBegin;
		fLists[Idx].BeginUpdate;
		try
			fLists[Idx].Clear;
			while not Reader.EndOfList do
				fLists[Idx].Add(Reader.ReadString);
		finally
			fLists[Idx].EndUpdate;
		end;
		Reader.ReadListEnd;
	end;

begin
	Reader.ReadListBegin;
	while not Reader.EndOfList do begin
		I := Reader.ReadInteger; //Listenindex
		if I>High(fLists) then
			SetSLCount(I+1);
		ReadSL(I)
	end;
	Reader.ReadListEnd;
end;

//***************************************************************************************
procedure TTSStringsContainer.WriteLists(Writer: TWriter);
var
	I, J: Integer;
begin
	Writer.WriteListBegin;
	for I := Low(fLists) to High(fLists) do begin
		if (fListNames[I]<>DefaultName(I))
		or (fLists[I].Count > 0) then begin
			Writer.WriteInteger(I);
			Writer.WriteString(fListNames[I]);
			Writer.WriteListBegin;
			for J := 0 to pred(fLists[I].Count) do
				Writer.WriteString(fLists[I][J]);
			Writer.WriteListEnd;
		end;
	end;
	Writer.WriteListEnd;
end;

//***************************************************************************************
procedure TTSStringsContainer.Loaded;
begin
	inherited;
	try
		SetActiveIndex(fTmpIndex);
	except
	end;
end;

//***************************************************************************************
function TTSStringsContainer.DefaultName(Index: Integer): string;
begin
	Result := 'New List ' + IntToStr(Index);
end;


//***************************************************************************************
function TTSStringsContainer.Exists(AListName: string): Boolean;
var
	I: Integer;
begin
	I := fListNames.IndexOf(AListName);
	Result := Between(I, Low(fLists), High(fLists), bndBoth);
end;

//***************************************************************************************
function TTSStringsContainer.GetActiveName: string;
begin
	Result := fListNames[fActiveIndex]
end;

//***************************************************************************************
function TTSStringsContainer.GetActiveStrings: TStrings;
begin
	Result := fLists[fActiveIndex];
end;

//***************************************************************************************
function TTSStringsContainer.GetListName(Index: Integer): string;
begin
	if Between(Index, Low(fLists), High(fLists), bndBoth) then
		Result := fListNames[Index]
	else
		raise Exception.Create(Owner.Name + '.' + Name + '.GetListName: Index out of bounds.');
end;

//***************************************************************************************
function TTSStringsContainer.GetListNames: TStrings;
begin
	Result := fListNames;
end;

//***************************************************************************************
function TTSStringsContainer.GetSLCount: Integer;
begin
	Result := fSLCount;
end;

//***************************************************************************************
function TTSStringsContainer.GetStrings(Index: Integer): TStrings;
begin
	if Between(Index, Low(fLists), High(fLists), bndBoth) then
		Result := fLists[Index]
	else
		raise Exception.Create(Owner.Name + '.' + Name + '.GetStrings: Index out of bounds.');
end;

//***************************************************************************************
function TTSStringsContainer.IndexOf(AListName: string): Integer;
begin
	Result := fListNames.IndexOf(AListName);
end;

//***************************************************************************************
function TTSStringsContainer.GetByName(AListName: string): TStrings;
begin
	Result := fLists[fListNames.IndexOf(AListName)]
end;

//***************************************************************************************
procedure TTSStringsContainer.SetActiveIndex(Value: Integer);
var
	Allow: Boolean;
begin
	if csReading in ComponentState then
		fTmpIndex := Value
	else begin
		Allow := True;
		if Assigned(fActiveListChanging) then
			fActiveListChanging(Self, Value, Allow);
		if not Allow then
			Exit;
		if Between(Value, Low(fLists), High(fLists), bndBoth) then
			fActiveIndex := Value
		else
			raise Exception.Create(Owner.Name + '.' + Name + '.SetActiveIndex: Index out of bounds.');
		if Assigned(fActiveListChanged) then
			fActiveListChanged(Self);
	end;
end;

//***************************************************************************************
procedure TTSStringsContainer.SetActiveName(const Value: string);
begin
	fListNames[fActiveIndex] := Value;
end;

//***************************************************************************************
procedure TTSStringsContainer.SetActiveStrings(const Value: TStrings);
begin
	fLists[fActiveIndex].Assign(Value);
end;

//***************************************************************************************
procedure TTSStringsContainer.SetStrings(Index: Integer; const Value: TStrings);
begin
	if Between(Index, Low(fLists), High(fLists), bndBoth) then
		fLists[Index].Assign(Value)
	else
		raise Exception.Create(Owner.Name + '.' + Name + '.SetStrings: Index out of bounds.');
end;

//***************************************************************************************
procedure TTSStringsContainer.SetByName(AListName: string; const Value: TStrings);
begin
	SetStrings(fListNames.IndexOf(AListName), Value);
end;

//***************************************************************************************
procedure TTSStringsContainer.SetListName(Index: Integer; const Value: string);
begin
	if Between(Index, Low(fLists), High(fLists), bndBoth) then
		fListNames[Index] := Value
	else
		raise Exception.Create(Owner.Name + '.' + Name + '.SetListName: Index out of bounds.');
end;

//***************************************************************************************
procedure TTSStringsContainer.SetSLCount(const Value: Integer);
begin
	if Value < 1 then
		raise Exception.Create(Owner.Name + '.' + Name + '. List count cant''t be 0.');

	if (Value < fSLCount)
	and (csDesigning in ComponentState)
	and not (csLoading in ComponentState) then
	begin
		if not YesNo('This will delete all StringLists above No. ' + IntToStr(Value-1) + '.'#13#10
					+ 'Are you sure, they can be deleted?', Name + '.' + Owner.Name, True) then
			Exit;
	end;

	while Value < fSLCount do begin
		Dec(fSLCount);
		fListNames.Delete(fSLCount);
		fLists[fSLCount].Free;
	end;
	SetLength(fLists, Value);
	while Value > fSLCount do begin
		fLists[fSLCount] := TStringList.Create;
		if fListNames.Count <= fSLCount then
			fListNames.Add(DefaultName(fSLCount))
		else
			fListNames[fSLCount] := DefaultName(fSLCount);
		Inc(fSLCount);
	end;
	if fActiveIndex >= fSLCount then
		fActiveIndex := fSLCount - 1;
end;

//***************************************************************************************
function TTSStringsContainer.AddList(AListName: string): Integer;
begin
	Result := AddList(AListName, nil);
end;

//***************************************************************************************
function TTSStringsContainer.AddList(List: TStrings): Integer;
begin
	Result := AddList(DefaultName(fSLCount), List);
end;

//***************************************************************************************
function TTSStringsContainer.AddList(AListName: string; List: TStrings): Integer;
begin
	Result := fSLCount;
	SetSLCount(fSLCount+1);
	if Assigned(List) then
		fLists[Result].Assign(List);
	if AListName > '' then
		fListNames[Result] := AListName;
end;

//***************************************************************************************
procedure TTSStringsContainer.Assign(Source: TPersistent);
var
	Src: TTSStringsContainer;
	N, I: Integer;
begin
	if Source is TTSStringsContainer then
	begin
		Src := TTSStringsContainer(Source);
		N := Src.fListNames.Count;
		SetSLCount(N);
		fListNames.Assign(Src.fListNames);
		SetLength(fLists, N);
		for I := 0 to pred(N) do
		begin
			fLists[I].Assign(Src.fLists[I]);
		end;
		ActiveListIndex := Src.ActiveListIndex;
	end
	else
		inherited;
end;

//***************************************************************************************
function TTSStringsContainer.GetActiveStr(Index: Integer): string;
begin
	Result := ActiveList[Index];
end;

//***************************************************************************************
procedure TTSStringsContainer.SetActiveStr(Index: Integer; const Value: string);
begin
	ActiveList[Index] := Value;
end;

//***************************************************************************************
function TTSStringsContainer.ActiveCount: Integer;
begin
	Result := ActiveList.Count;
end;

//***************************************************************************************
procedure TTSStringsContainer.DeleteList(Index: Integer);
var
	I: Integer;
begin
	if Between(Index, Low(fLists), High(fLists), bndBoth) then
	begin
		for I := Index to Pred(High(fLists)) do
		begin
			fLists[I].Assign(fLists[I+1]);
			fListNames[I] := fListNames[I+1];
		end;
		SetSLCount(fSLCount-1);
		if fActiveIndex > High(fLists) then
			Dec(fActiveIndex);
	end
	else
		raise Exception.Create(Owner.Name + '.' + Name + '.SetStrings: Index out of bounds.');
end;

//***************************************************************************************
procedure TTSStringsContainer.SetListNames(const Value: TStrings);
begin
	SetSLCount(Value.Count);
	fListNames.Assign(Value);
end;

end.

