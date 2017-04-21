unit TSLib;

interface

uses
{$IFDEF WINDOWS}
	Windows,
{$ENDIF}

	Forms,
	ShlObj,
	Controls,
	Graphics,
	Classes;

type
	NShutDownType = (sdLogoff, sdReboot, sdShutdown, sdPowerOff);
	NBoolStr = (bs10, bsYN, bsTF, bsJN, bsWF);
  NBetweenBound = (bndIncludeLo, bndIncludeHi);
  SBetweenBounds = set of NBetweenBound;
	NBrowseInfoFlag = (bifBrowseForComputer, bifBrowseForPrinter, bifBrowseIncludeFiles,
											bifIncludeURLs, bifDontGoBelowDomain, bifEditBox, bifNewDialogStyle,
											bifNoNewFolderButton, bifNoTranslateTargets, bifReturnFSAncestors,
											bifReturnOnlyFSDirs, bifShareable, bifStatusText, bifUAHint,
											bifValidate);
	SBrowseInfoFlags = set of NBrowseInfoFlag;
  NSingleInstanceOption = (siActivateIfRunning, siAllowInDifferentDirectories);
  SSingleInstanceOption = set of NSingleInstanceOption;
  SChars = set of AnsiChar;
  NCalcExpressionOption = (ceRaiseOnError, ceThousandSeparator);
  SCalcExpressionOptions = set of NCalcExpressionOption;
	NFontProperty = (fntCharSet, fntColor, fntHeight, fntName, fntPitch, fntSize, fntStyle);
  SFontProperties = set of NFontProperty;


	//***************************************************************************************
	//Der UpdateTracker kann als "Flag" verwendet werden, um Aktualsierungsabläufe zu
	//	verfolgen. Soll ein Änderungsvorgang ausgeführt werden, der in anderen Prozeduren
	//	oder Ereignissen von Bedeutung ist, wird vorher die Methode "BeginUpdate" des
	//	Objektes aufgerufen. Nach Abschluss der Aktualisierungen wird dies durch "EndUpdate"
	//	signalisiert. Die Funktion EndUpdate gibt wiederum zurück, ob die gerade abge-
	//	schlossene Aktualisierung die letzte aktive Aktualisierung des Objektes war.
	//	Durch einen Aufruf des Konstruktors "BeginUpdateNew" wird das Objekt erzeugt und
	//	auf Aktualisierungsstatus 1 gesetzt (= eine Aktualisierung aktiv).
	TTSUpdateTracker = class(TObject)
	private
		fUpdtateCounter: Integer;
		fFreeOnEnd: Boolean;
		fEndUpdate: TNotifyEvent;
	public
		constructor BeginUpdateNew(FreeOnEndUpdate: Boolean= True);
		function Updating: Boolean;
		procedure BeginUpdate;
		function EndUpdate: Boolean;
		property OnEndUpdate: TNotifyEvent read fEndUpdate write fEndUpdate;
	end;



//---------------------------------------------------------------------------------------
// Transaktionsfunktionen / -prozeduren
//---------------------------------------------------------------------------------------

//Die Funktionen TrackUpdate und TrackingUpdate dienen zum schnellen und einfachen Zugriff
//	auf TTSUpdateTracker-Objekte (s.o.).
// MitTrackUpdate wird geprüft, ob das Objekt existiert. Wenn ja, wird der Update-Zähler
//	inkrmentiert. Wenn nicht, wird das Objekt erzeugt und mit 1 initialisiert.
procedure TrackUpdate(var Tracker: TTSUpdateTracker; OnEndUpdate: TNotifyEvent=nil);
//Mit TrackingUpdate kann geprüft werden, ob der angegebene Tracker initialisiert und aktiv ist.
function TrackingUpdate(Tracker: TTSUpdateTracker): Boolean;
//Die Funktion TrackingFinished entspricht der Methode "EndUpdate" des Trackers. Ist das Update
//	abgeschlossen (Updating=False), wird der Tracker freigegeben.
function TrackingFinished(var Tracker: TTSUpdateTracker): Boolean;

//---------------------------------------------------------------------------------------
// Anwendungs- und PC-Informationen
//---------------------------------------------------------------------------------------

function ApplicationPath: string;
function ApplicationName: string;

{$IFDEF WINDOWS}
function CurrentUser: string;
function CurrentWorkstationName: string;
function ShutdownComputer(ShutDownType: NShutDownType; ForceShutdown: Boolean=False): Boolean;
{$ENDIF}


//---------------------------------------------------------------------------------------
// Dialogfunktionen
//---------------------------------------------------------------------------------------
//Zeigt eine Dialogbox mit den entsprechenden Buttons. Durch "DefaultButton" oder "DefaultNo"
// kann jeweils die Standard-Schaltfläche angegeben werden.
function YesNoCancel(const Text: string; const Caption: string=''; DefaultButton: TModalResult=mrYes): TModalResult; overload;
function YesNo(const Text: string; const Caption: string=''; DefaultNo: Boolean=False): Boolean;

function ForcePath(var Path: string): Boolean;

//---------------------------------------------------------------------------------------
// String-Funktionen
//---------------------------------------------------------------------------------------

function PosInText(const SubStr, Text: string; Offset: Cardinal=0): Integer;
function StartsText(const SubStr, Text: string): Boolean;
function EndsText(const SubStr, Text: string): Boolean;
function SeparateTextBefore(const SubStr: string; var Text: string; FindLastInstance: Boolean=False): string;

function ContainsText(const SubStr, Text: string): Boolean;
procedure AddValueToList(const Value, Separator: string; var List: string);
function SplitCSVString(const CSV: string; out S1, S2: string): Boolean;
function IndentLines(Text: string; const IndentString: string=#9; FirstLine: Boolean=True): string; overload;
procedure IndentLines(List: TStrings; const IndentString: string=#9;
                      EmptyLines: Boolean=False; FirstLine: Boolean=True); overload;
function AlignRight(Value: string; Width: Integer): string; overload;
function AlignLeft(Value: string; Width: Integer): string; overload;
function AlignCenter(Value: string; Width: Integer): string; overload;
function AlignRight(Value: string; Width: Integer; FillChar: Char): string; overload;
function AlignLeft(Value: string; Width: Integer; FillChar: Char): string; overload;
function AlignCenter(Value: string; Width: Integer; FillChar: Char): string; overload;
function Hex2Dec(const S: string; Default: Integer=0): Longint;
//Wandeldt alle Backslashes (\) in Slashes (/) oder umgekehrt (Reverse = True).
function Back2Slash(const Value: string; Reverse: Boolean=False): string;
//Prüft, ob <Value> mit einem <TrailingChar> endet. Ist <Remove>=False, enthält das Ergebnis einen <TrailingChar>
// Ist <Remove>=True, wird ein evtl. vorhandener <TrailingChar> entfernt.
function Trailing(const Value: string; TrailingChar: Char='/'; Remove: Boolean=False): string;
function BarcodeCheckSum(const BC: string; ReturnCompleteBarcode: Boolean=True): string;
procedure StringsValuePair(SL: TStrings; Index: Integer; out Name, Value: string);
function BuildName(const FirstName, LastName: string; Reverse: Boolean=False): string;
function MakePassword(FromChars: SChars; PWLen: Integer=8): string; overload;
function MakePassword(FromChars: array of AnsiChar; PWLen: Integer=8): string; overload;
function MakePassword(PWLen: Integer=8): string; overload;
procedure UnSerialize(PHPSerial: string; List: TStrings; Path: string='');
function DelimitedContent(S: string; StartDelim, EndDelim: char; var StartPos: Integer): string; overload;
function DelimitedContent(S: string; StartDelim, EndDelim: char): string; overload;
function DelimitedContent(S: string; Delimiter: char; var StartPos: Integer): string; overload;
function DelimitedContent(S: string; Delimiter: char): string; overload;

function CalculateExpressionValue(var Expression: string; var EditPos: Integer;
                  Options: SCalcExpressionOptions=[ceRaiseOnError,
                                                   ceThousandSeparator]): Extended;
function FormatHTMLForClipboard(S: string): string;

{$IFDEF WINDOWS}
procedure CopyHTMLToClipBoard(HTML: string = ''; EmptyClipboard: Boolean=False);
{$ENDIF}


//---------------------------------------------------------------------------------------
// Entscheidungsfunktionen (Immediate If etc.)
//---------------------------------------------------------------------------------------

//Gibt IfTrue zurück, wenn Expr=True, sonst IfFalse. Überladene Versionen für
// Strings, Integers, Floats und Objekte.
function IIf(Expr: Boolean; const IfTrue, IfFalse: Char): Char; overload;
function IIf(Expr: Boolean; const IfTrue, IfFalse: string): string; overload;
function IIf(Expr: Boolean; const IfTrue, IfFalse: Integer): Integer; overload;
function IIf(Expr: Boolean; const IfTrue, IfFalse: Extended): Extended; overload;
function IIf(Expr: Boolean; IfTrue, IfFalse: TObject): TObject; overload;
function IIfVar(Expr: Boolean; IfTrue, IfFalse: Variant): Variant; overload;

//Gibt True zurück, wenn der Wert Value zwischen den Werten LowBound und HighBound liegt.
//  Der Parameter Include gibt an, ob der untere oder/und obere Wert Teil des
//  gültigen Bereiches ist.
function Between(Value, LowBound, HighBound: Integer; Include: SBetweenBounds=[]): Boolean; overload;
function Between(Value, LowBound, HighBound: Extended; Include: SBetweenBounds=[]): Boolean; overload;
function Between(Value, LowBound, HighBound: string; Include: SBetweenBounds=[]): Boolean; overload;

//Prüft, ob Idx einen gültigen Positionsindes innerhalb des strings oder arrays darstellt.
function ValidIndex(Idx: integer; Str: string): Boolean; overload;
function ValidIndex(Idx: integer; Arr: array of string): Boolean; overload;
function ValidIndex(Idx: integer; Arr: array of Integer): Boolean; overload;
function ValidIndex(Idx: integer; Arr: array of Extended): Boolean; overload;

function MinVal(Val1, Val2: Integer): Integer; overload;
function MinVal(Val1, Val2: Extended): Extended; overload;
function MinVal(Vals: array of Integer): Integer; overload;
function MinVal(Vals: array of Extended): Extended; overload;
function MinVal(Vals: array of string): string; overload;

function MaxVal(Val1, Val2: Integer): Integer; overload;
function MaxVal(Val1, Val2: Extended): Extended; overload;
function MaxVal(Vals: array of Integer): Integer; overload;
function MaxVal(Vals: array of Extended): Extended; overload;
function MaxVal(Vals: array of string): string; overload;

function BoundsVal(Val, LoBound, HiBound: Integer): Integer; overload;
function BoundsVal(Val, LoBound, HiBound: Extended): Extended; overload;

function NullIf(Condition: Boolean; IfNotNull: Variant): Variant;
function IsNumeric(Value: string): Boolean;
function NearlyEqual(Val1, Val2: Extended; Precision: Extended = 1e-12): Boolean;
function IsZero(Number: Extended; Precision: Extended=0.00001): Boolean;
function IsSameFont(const Font1, Font2: TFont; CompareProps: SFontProperties = [fntCharSet .. fntStyle]): Boolean;


//Tauscht die Inhalte der Variablen A und B
procedure Switch(var A, B: Extended); overload;
procedure Switch(var A, B: string); overload;
procedure Switch(var A, B: Cardinal); overload;
procedure Switch(var A, B: Integer); overload;
procedure Switch(var A, B: TDateTime); overload;

function SimpleEncrypt(const Value: string; EncryptLen: Integer=128): string;
function SimpleDecrypt(const Value: string): string; overload;
function SimpleDecrypt(const Value, Default: string): string; overload;


procedure CopyGraphic(FromGraph, ToGraph: TGraphic);

const
	{$EXTERNALSYM BIF_NONEWFOLDERBUTTON}
	BIF_NONEWFOLDERBUTTON = $1000;

	{$EXTERNALSYM BIF_NOTRANSLATETARGETS}
	BIF_NOTRANSLATETARGETS = $1000;

	{$EXTERNALSYM BIF_UAHINT}
	BIF_UAHINT = $1000;

	BIFConsts: array[NBrowseInfoFlag] of Integer = (BIF_BROWSEFORCOMPUTER,
																		BIF_BROWSEFORPRINTER, BIF_BROWSEINCLUDEFILES,
																		BIF_BROWSEINCLUDEURLS, BIF_DONTGOBELOWDOMAIN,
																		BIF_EDITBOX, BIF_NEWDIALOGSTYLE, BIF_NoNewFolderButton,
																		BIF_NoTranslateTargets, BIF_RETURNFSANCESTORS,
																		BIF_RETURNONLYFSDIRS, BIF_SHAREABLE, BIF_STATUSTEXT,
																		BIF_UAHint, BIF_VALIDATE);

	bndBoth: SBetweenBounds = [bndIncludeLo, bndIncludeHi];
	bndLo: SBetweenBounds = [bndIncludeLo];
	bndHi: SBetweenBounds = [bndIncludeHi];
  bndNone: SBetweenBounds = [];

{$IFDEF WINDOWS}
  aShutDownType: array [NShutDownType] of DWord = (EWX_LOGOFF, EWX_REBOOT, EWX_SHUTDOWN, EWX_POWEROFF);
{$ENDIF}


implementation

uses
	Dialogs,
  TSLibFiles,
  TSLibConvert,
	Variants,
	Math,
	SysUtils,
	Types,
	StrUtils;

const
	encryptLengthSeed = $E80C;
	encryptPositionSeed = $FE14;


//***************************************************************************************
//***************************************************************************************
{ TTSUpdateTracker }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
procedure TTSUpdateTracker.BeginUpdate;
begin
	Inc(fUpdtateCounter);
end;

//***************************************************************************************
constructor TTSUpdateTracker.BeginUpdateNew(FreeOnEndUpdate: Boolean);
begin
	inherited Create;
	fFreeOnEnd := FreeOnEndUpdate;
	fUpdtateCounter := 1;
end;

//***************************************************************************************
function TTSUpdateTracker.EndUpdate: Boolean;
begin
	Dec(fUpdtateCounter);
	Result := fUpdtateCounter <= 0;
	if Result and Assigned(fEndUpdate) then
		fEndUpdate(Self);
end;

//***************************************************************************************
function TTSUpdateTracker.Updating: Boolean;
begin
	Result := fUpdtateCounter > 0;
end;

//***************************************************************************************
{ ENDE TTSUpdateTracker }
//***************************************************************************************


//---------------------------------------------------------------------------------------
function ApplicationPath: string;
begin
	Result := ExtractFilePath(Application.ExeName);
end;

//---------------------------------------------------------------------------------------
function ApplicationName: string;
var
	X: string;
begin
	Result := ExtractFileName(Application.ExeName);
	X := ExtractFileExt(Application.ExeName);
	Result := AnsiReplaceStr(Result, X, '');
end;

//---------------------------------------------------------------------------------------
function YesNoCancel(const Text: string; const Caption: string=''; DefaultButton: TModalResult=mrYes): TModalResult;
begin
	case DefaultButton of
		mrNo:
      Result := QuestionDlg(Caption, Text, mtConfirmation, [mrNo, mrYes, mrCancel], 0);
		mrCancel:
	    Result := QuestionDlg(Caption, Text, mtConfirmation, [mrYes, mrNo, mrCancel], 0);
		else
      Result := QuestionDlg(Caption, Text, mtConfirmation, [mrCancel, mrYes, mrNo], 0);
	end;
end;

//---------------------------------------------------------------------------------------
function YesNo(const Text: string; const Caption: string=''; DefaultNo: Boolean=False): Boolean;
var
	DefBtn: Longint;
begin
	if DefaultNo then
	  Result := QuestionDlg(Caption, Text, mtConfirmation, [mrNo, mrYes], 0) = mrYes
	else
	  Result := QuestionDlg(Caption, Text, mtConfirmation, [mrYes, mrNo], 0) = mrYes
end;

//---------------------------------------------------------------------------------------
// Die Funktion entspricht der Funktion "POS", aber sie ignoriert Groß- und Kleinschreibung
// ("Case Insensitive Pos")
function PosInText(const SubStr, Text: string; Offset: Cardinal=0): Integer;
begin
	if Offset > 0 then
		Result := PosEx(LowerCase(SubStr), LowerCase(Text), Offset)
	else
		Result := Pos(LowerCase(SubStr), LowerCase(Text));
end;

function StartsText(const SubStr, Text: string): Boolean;
begin
	Result := PosInText(SubStr, Text) = 1;
end;

function TextStartsWith(const Text, SubStr: string): Boolean;
begin
	Result := PosInText(SubStr, Text) = 1;
end;

function EndsText(const SubStr, Text: string): Boolean;
begin
	Result := PosInText(SubStr, Text) = Length(Text) - Length(SubStr) + 1;
end;

function ContainsText(const SubStr, Text: string): Boolean;
begin
	Result := PosInText(SubStr, Text) > 0;
end;

function SeparateTextBefore(const SubStr: string; var Text: string; FindLastInstance: Boolean=False): string;
var
  L, P, I: Integer;
  S: string;
begin
  if FindLastInstance then
  begin
    L := Length(SubStr);
    P := Length(Text) - L;
    S := LowerCase(SubStr);
    while P >= 0 do
    begin
      I := 1;
      while (I<=L) and (S[I]=UpperCase(Text[P+I])) do
        inc(I);
      if I > L then
      begin
        Result := Copy(Text, P+1, MaxInt);
        SetLength(Text, P);
        Exit;
      end;
      dec(P);
    end;
    Result := '';
  end;


end;

//---------------------------------------------------------------------------------------
// Wenn der Wert Value > '', wird er an den String "List" angehängt. Enthält die Liste
// bereits Text (>''), wird dem neuen String ein Trennzeichen "Separator" vorangestellt.
procedure AddValueToList(const Value, Separator: string; var List: string);
begin
	if Value = '' then
		Exit;
	if List > '' then
		List := List + Separator + Value
	else
		List := Value;
end;

//---------------------------------------------------------------------------------------
function SplitCSVString(const CSV: string; out S1, S2: string): Boolean;
var
  P: Integer;
begin
	P := Pos(',', CSV);
	Result := P>0;
	if Result then begin
		S1 := Trim(Copy(CSV, 1, P-1));
		S2 := Trim(Copy(CSV, P+1, Length(CSV)))
	end
	else begin
		S1 := Trim(CSV);
		S2 := '';
	end;
end;

//---------------------------------------------------------------------------------------
function IIf(Expr: Boolean; const IfTrue, IfFalse: Char): Char; overload;
begin
	if Expr then Result := IfTrue else Result := IfFalse;
end;

//---------------------------------------------------------------------------------------
function IIf(Expr: Boolean; const IfTrue, IfFalse: string): string; overload;
begin
	if Expr then Result := IfTrue else Result := IfFalse;
end;

//---------------------------------------------------------------------------------------
function IIf(Expr: Boolean; const IfTrue, IfFalse: Integer): Integer; overload;
begin
	if Expr then Result := IfTrue else Result := IfFalse;
end;

//---------------------------------------------------------------------------------------
function IIf(Expr: Boolean; const IfTrue, IfFalse: Extended): Extended; overload;
begin
	if Expr then Result := IfTrue else Result := IfFalse;
end;

//---------------------------------------------------------------------------------------
function IIf(Expr: Boolean; IfTrue, IfFalse: TObject): TObject; overload;
begin
	if Expr then Result := IfTrue else Result := IfFalse;
end;

//---------------------------------------------------------------------------------------
function IIfVar(Expr: Boolean; IfTrue, IfFalse: Variant): Variant; overload;
begin
	if Expr then Result := IfTrue else Result := IfFalse;
end;

////---------------------------------------------------------------------------------------
//function Between(Value, LowBound, HighBound: Integer; Include: Boolean=False): Boolean; overload;
//begin
//	if Include then
//		Result := (Value >= LowBound) and (Value <= HighBound)
//	else
//		Result := (Value > LowBound) and (Value < HighBound)
//end;
//
////---------------------------------------------------------------------------------------
//function Between(Value, LowBound, HighBound: Extended; Include: Boolean=False): Boolean; overload;
//begin
//	if Include then
//		Result := (Value >= LowBound) and (Value <= HighBound)
//	else
//		Result := (Value > LowBound) and (Value < HighBound)
//end;
//
////---------------------------------------------------------------------------------------
//function Between(Value, LowBound, HighBound: string; Include: Boolean=False): Boolean; overload;
//begin
//	if Include then
//		Result := (Value >= LowBound) and (Value <= HighBound)
//	else
//		Result := (Value > LowBound) and (Value < HighBound)
//end;

//---------------------------------------------------------------------------------------
function Between(Value, LowBound, HighBound: Integer; Include: SBetweenBounds=[]): Boolean; overload;
begin
  if bndIncludeLo in Include then
  begin
    if bndIncludeHi in Include then
      Result := (Value >= LowBound) and (Value <= HighBound)
    else
      Result := (Value >= LowBound) and (Value < HighBound)
  end
  else begin
    if bndIncludeHi in Include then
      Result := (Value > LowBound) and (Value <= HighBound)
    else
      Result := (Value > LowBound) and (Value < HighBound)
  end;
end;

//---------------------------------------------------------------------------------------
function Between(Value, LowBound, HighBound: Extended; Include: SBetweenBounds=[]): Boolean; overload;
begin
  if bndIncludeLo in Include then
  begin
    if bndIncludeHi in Include then
      Result := (Value >= LowBound) and (Value <= HighBound)
    else
      Result := (Value >= LowBound) and (Value < HighBound)
  end
  else begin
    if bndIncludeHi in Include then
      Result := (Value > LowBound) and (Value <= HighBound)
    else
      Result := (Value > LowBound) and (Value < HighBound)
  end;
end;

//---------------------------------------------------------------------------------------
function Between(Value, LowBound, HighBound: string; Include: SBetweenBounds=[]): Boolean; overload;
begin
  if bndIncludeLo in Include then
  begin
    if bndIncludeHi in Include then
      Result := (Value >= LowBound) and (Value <= HighBound)
    else
      Result := (Value >= LowBound) and (Value < HighBound)
  end
  else begin
    if bndIncludeHi in Include then
      Result := (Value > LowBound) and (Value <= HighBound)
    else
      Result := (Value > LowBound) and (Value < HighBound)
  end;
end;

//---------------------------------------------------------------------------------------
function ValidIndex(Idx: integer; Str: string): Boolean; overload;
begin
	Result := Between(Idx, 1, Length(Str), bndBoth);
end;

//---------------------------------------------------------------------------------------
function ValidIndex(Idx: integer; Arr: array of string): Boolean; overload;
begin
	Result := Between(Idx, Low(Arr), High(Arr), bndBoth);
end;

//---------------------------------------------------------------------------------------
function ValidIndex(Idx: integer; Arr: array of Integer): Boolean; overload;
begin
	Result := Between(Idx, Low(Arr), High(Arr), bndBoth);
end;

//---------------------------------------------------------------------------------------
function ValidIndex(Idx: integer; Arr: array of Extended): Boolean; overload;
begin
	Result := Between(Idx, Low(Arr), High(Arr), bndBoth);
end;

//---------------------------------------------------------------------------------------
function MinVal(Val1, Val2: Integer): Integer; overload;
begin
	if Val1<Val2 then
		Result := Val1
	else
		Result := Val2;
end;

//---------------------------------------------------------------------------------------
function MinVal(Val1, Val2: Extended): Extended; overload;
begin
	if Val1<Val2 then
		Result := Val1
	else
		Result := Val2;
end;

//---------------------------------------------------------------------------------------
function MinVal(Vals: array of Integer): Integer;
var
  I: Integer;
begin
	Result := Vals[Low(Vals)];
	for I := succ(Low(Vals)) to High(Vals) do
  	if Vals[I] < Result then
    	Result := Vals[I];
end;

//---------------------------------------------------------------------------------------
function MinVal(Vals: array of Extended): Extended;
var
  I: Integer;
begin
	Result := Vals[Low(Vals)];
	for I := succ(Low(Vals)) to High(Vals) do
  	if Vals[I] < Result then
    	Result := Vals[I];
end;

//---------------------------------------------------------------------------------------
function MinVal(Vals: array of string): string;
var
  I: Integer;
begin
	Result := Vals[Low(Vals)];
	for I := succ(Low(Vals)) to High(Vals) do
  	if Vals[I] < Result then
    	Result := Vals[I];
end;

//---------------------------------------------------------------------------------------
function MaxVal(Vals: array of Integer): Integer;
var
  I: Integer;
begin
	Result := Vals[Low(Vals)];
	for I := succ(Low(Vals)) to High(Vals) do
  	if Vals[I] > Result then
    	Result := Vals[I];
end;

//---------------------------------------------------------------------------------------
function MaxVal(Vals: array of Extended): Extended;
var
  I: Integer;
begin
	Result := Vals[Low(Vals)];
	for I := succ(Low(Vals)) to High(Vals) do
  	if Vals[I] > Result then
    	Result := Vals[I];
end;

//---------------------------------------------------------------------------------------
function MaxVal(Vals: array of string): string;
var
  I: Integer;
begin
	Result := Vals[Low(Vals)];
	for I := succ(Low(Vals)) to High(Vals) do
  	if Vals[I] > Result then
    	Result := Vals[I];
end;

//---------------------------------------------------------------------------------------
function MaxVal(Val1, Val2: Integer): Integer; overload;
begin
	if Val1>Val2 then
		Result := Val1
	else
		Result := Val2;
end;

//---------------------------------------------------------------------------------------
function MaxVal(Val1, Val2: Extended): Extended; overload;
begin
	if Val1>Val2 then
		Result := Val1
	else
		Result := Val2;
end;

//---------------------------------------------------------------------------------------
function BoundsVal(Val, LoBound, HiBound: Integer): Integer; overload;
begin
	if Val < LoBound then
		Result := LoBound
	else if Val > HiBound then
		Result := HiBound
	else
		Result := Val;
end;

//---------------------------------------------------------------------------------------
function BoundsVal(Val, LoBound, HiBound: Extended): Extended; overload;
begin
	if Val < LoBound then
		Result := LoBound
	else if Val > HiBound then
		Result := HiBound
	else
		Result := Val;
end;

//---------------------------------------------------------------------------------------
function NullIf(Condition: Boolean; IfNotNull: Variant): Variant;
begin
	if Condition then
		Result := NULL
	else
		Result := IfNotNull;
end;

//---------------------------------------------------------------------------------------
function IsNumeric(Value: string): Boolean;
var
	I: Integer;
begin
	Result := True;
	for I := 1 to Length(Value) do
		if not (AnsiChar(Value[I]) in ['0'..'9']) then begin
			Result := False;
			Break;
		end;
end;

//---------------------------------------------------------------------------------------
function NearlyEqual(Val1, Val2: Extended; Precision: Extended = 1e-12): Boolean;
begin
	Result := Abs(Val1-Val2) < Precision;
end;

//---------------------------------------------------------------------------------------
function IsZero(Number: Extended; Precision: Extended=0.00001): Boolean;
begin
	Result := (Number < Precision) and (Number > -Precision);
end;

function IsSameFont(const Font1, Font2: TFont; CompareProps: SFontProperties): Boolean;
var
  FontProp: NFontProperty;
begin
  for FontProp in CompareProps do
  begin
		case FontProp of
      fntCharSet: Result := Font1.Charset = Font2.Charset;
      fntColor: Result := Font1.Color = Font2.Color;
      fntHeight: Result := Font1.Height = Font2.Height;
      fntName: Result := Font1.Name = Font2.Name;
      fntSize: Result := Font1.Size = Font2.Size;
      fntPitch: Result := Font1.Pitch = Font2.Pitch;
      fntStyle: Result := Font1.Style = Font2.Style;
    end;
  	if not Result then
	    Exit;
	end;
	Result := True;
end;

//---------------------------------------------------------------------------------------
procedure Switch(var A, B: Extended); overload;
var
	Tmp: Extended;
begin
	Tmp := A; A := B; B := Tmp;
end;
//---------------------------------------------------------------------------------------
procedure Switch(var A, B: string); overload;
var
	Tmp: string;
begin
	Tmp := A; A := B; B := Tmp;
end;
//---------------------------------------------------------------------------------------
procedure Switch(var A, B: Cardinal); overload;
var
	Tmp: Cardinal;
begin
	Tmp := A; A := B; B := Tmp;
end;
//---------------------------------------------------------------------------------------
procedure Switch(var A, B: Integer); overload;
var
	Tmp: Integer;
begin
	Tmp := A; A := B; B := Tmp;
end;

//---------------------------------------------------------------------------------------
procedure Switch(var A, B: TDateTime); overload;
var
	Tmp: TDateTime;
begin
	Tmp := A; A := B; B := Tmp;
end;

//---------------------------------------------------------------------------------------
procedure TrackUpdate(var Tracker: TTSUpdateTracker; OnEndUpdate: TNotifyEvent=nil);
begin
	if Assigned(Tracker) then
		Tracker.BeginUpdate
	else begin
		Tracker := TTSUpdateTracker.BeginUpdateNew;
		Tracker.OnEndUpdate := OnEndUpdate;
	end;
end; //TrackUpdate

//---------------------------------------------------------------------------------------
function TrackingUpdate(Tracker: TTSUpdateTracker): Boolean;
begin
	Result := Assigned(Tracker) and Tracker.Updating;
end; //TrackingUpdate

//---------------------------------------------------------------------------------------
function TrackingFinished(var Tracker: TTSUpdateTracker): Boolean;
begin
	if Assigned(Tracker) then begin
		Result := Tracker.EndUpdate;
		if Result and Tracker.fFreeOnEnd then
			FreeAndNil(Tracker);
	end
	else
		Result := True;
end; //TrackingFinished


//---------------------------------------------------------------------------------------
function Hex2Dec(const S: string; Default: Integer=0): Longint;
var
	HexStr: string;
begin
	if Pos('$', S) = 0 then HexStr := '$' + S
	else HexStr := S;
  if Default < 0 then
  	Result := StrToInt(HexStr)
  else
	  Result := StrToIntDef(HexStr, 0);
end;

//---------------------------------------------------------------------------------------
//Wandeldt alle Backslashes (\) in Slashes (/) oder umgekehrt (Reverse = True).
function Back2Slash(const Value: string; Reverse: Boolean=False): string;
begin
	if Reverse then
		Result := StringReplace(Value, '/', '\', [rfReplaceAll])
	else
		Result := StringReplace(Value, '\', '/', [rfReplaceAll]);
end;

//---------------------------------------------------------------------------------------
//Prüft, ob <Value> mit einem <TrailingChar> endet. Ist <Remove>=False, enthält das Ergebnis einen <TrailingChar>
// Ist <Remove>=True, wird ein evtl. vorhandener <TrailingChar> entfernt.
function Trailing(const Value: string; TrailingChar: Char='/'; Remove: Boolean=False): string;
var
	L: Integer;
	C: char;
begin
	if Value > '' then
	begin
		Result := Value;
		L := Length(Result);
		C := Result[L];
		while C = TrailingChar do
		begin
			Delete(Result, L, 1);
			L := Length(Result);
			C := Result[L];
		end;
		if not Remove then
			Result := Result + TrailingChar;
	end
	else if Remove then
		Result := ''
	else
		Result := TrailingChar;
end;

//---------------------------------------------------------------------------------------
function BarcodeCheckSum(const BC: string; ReturnCompleteBarcode: Boolean=True): string;
var
	M, CS, I: Integer;
begin
	CS := 0; M := 3;
	for I := Length(BC) downto 1 do
	try
		CS := CS + StrToInt(BC[I]) * M;
		M := IIf(M=3, 1, 3);
	except
		if ReturnCompleteBarcode then
			Result := BC
		else
			Result := '';
		Exit;
	end;
	Result := IntToStr(10 - (CS mod 10));
end;

//---------------------------------------------------------------------------------------
procedure StringsValuePair(SL: TStrings; Index: Integer; out Name, Value: string);
begin
  Name := SL.Names[Index];
  Value := SL.ValueFromIndex[Index];
  if Name = '' then
    Name := SL[Index];
end;

//---------------------------------------------------------------------------------------
function BuildName(const FirstName, LastName: string; Reverse: Boolean=False): string;
begin
  Result := FirstName;
  if LastName > '' then
  begin
    if Result > '' then
      Result := Result + ' ' + LastName
    else
      Result := LastName;
  end;
end;

//---------------------------------------------------------------------------------------
function MakePassword(FromChars: array of AnsiChar; PWLen: Integer=8): string; overload;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to PWLen do
    Result := Result + Char(FromChars[RandomRange(Low(FromChars), High(FromChars))]);
end;

function MakePassword(FromChars: SChars; PWLen: Integer=8): string; overload;
var
  L: Integer;
  C: AnsiChar;
  A: array of AnsiChar;
begin
  L := 0;
  SetLength(A, 0);
  for c := Low(AnsiChar) to High(AnsiChar) do
    if c in FromChars then
    begin
      SetLength(A, L+1);
      A[L] := c;
      inc(L);
    end;
  Result := MakePassword(A, PWLen);
end;

function MakePassword(PWLen: Integer=8): string; overload;
begin
  Result := MakePassword(SChars(['0'..'9', 'a'..'z', 'A'..'Z']), PWLen);
end;


//---------------------------------------------------------------------------------------
function IndentLines(Text: string; const IndentString: string=#9; FirstLine: Boolean=True): string;
begin
	if FirstLine then
		Result := IndentString + StringReplace(Text, #13#10, #13#10+IndentString, [rfReplaceAll])
	else
		Result := StringReplace(Text, #13#10, #13#10+IndentString, [rfReplaceAll])
end;

//---------------------------------------------------------------------------------------
procedure IndentLines(List: TStrings; const IndentString: string=#9;
                      EmptyLines: Boolean=False; FirstLine: Boolean=True);
var
  F, I: Integer;
begin
  List.BeginUpdate;
  try
    if FirstLine then
      F := 0
    else
      F := 1;
    for I := F to pred(List.Count) do
      if EmptyLines or (List[I]>'') then
        List[I] := IndentString + List[I];
  finally
    List.EndUpdate;
  end;
end;

//---------------------------------------------------------------------------------------
function AlignRight(Value: string; Width: Integer): string;
begin
	Result := AlignRight(Value, Width, ' ');
end;

//---------------------------------------------------------------------------------------
function AlignRight(Value: string; Width: Integer; FillChar: Char): string; overload;
var
	L: Integer;
begin
	L := Length(Value);
	if L<Width then
		Result := StringOfChar(FillChar, Width-L) + Value
	else if L > Width then
		Result := Copy(Value, 1, Width)
	else
		Result := Value;
end; //AlignRight

//---------------------------------------------------------------------------------------
function AlignLeft(Value: string; Width: Integer): string;
begin
	Result := AlignLeft(Value, Width, ' ');
end;

//---------------------------------------------------------------------------------------
function AlignLeft(Value: string; Width: Integer; FillChar: Char): string; overload;
var
	L: Integer;
begin
	L := Length(Value);
	if L<Width then
		Result := Value + StringOfChar(FillChar, Width-L)
	else if L > Width then
		Result := Copy(Value, 1, Width)
	else
		Result := Value;
end; //AlignLeft

//---------------------------------------------------------------------------------------
function AlignCenter(Value: string; Width: Integer): string;
begin
	Result := AlignCenter(Value, Width, ' ');
end;

//---------------------------------------------------------------------------------------
function AlignCenter(Value: string; Width: Integer; FillChar: Char): string; overload;
var
	L, R: Integer;
begin
	L := Length(Value);
	if L<Width then
	begin
		L := (Width-L) DIV 2;
		R := Width-Length(Value)-L;
		Result := StringOfChar(FillChar, L) + Value + StringOfChar(FillChar, R);
	end
	else if L > Width then
		Result := Copy(Value, 1, Width)
	else
		Result := Value;
end; //AlignCenter

//---------------------------------------------------------------------------------------
function SimpleEncrypt(const Value: string; EncryptLen: Integer=128): string;
var
	C, X: Byte;
	I, L, P: Integer;
	S, ES, LS, PS: string;
begin
	L := Length(Value)*2 + 10;
	P := MinVal(encryptLengthSeed, encryptPositionSeed) - L;
	if L > EncryptLen then
		raise Exception.Create('Encryption not possible - Value is too long for encrypted length.');
	if EncryptLen > P then
		raise Exception.Create('Encryption not possible - EncryptLen must be less than ' + IntToStr(P));


	S := '';
	X := Random(255)+1;

	ES := IntToHex(X, 2);
	for I := 1 to Length(Value) do
	begin
		C := Ord(Value[I]);
		ES := ES + IntToHex((not C) xor X, 2);
	end;

	L := EncryptLen - Length(ES) - 8;
	P := Random(L) + 5;
	PS := IntToHex(encryptPositionSeed - P, 4);
	LS := IntToHex(encryptLengthSeed - Length(ES), 4);
	S := LS;
	for I := 1 to P-5 do
		S := S + IntToHex(Random(16), 1);
	S := S + ES;
	while Length(S) < EncryptLen - 4 do
		S := S + IntToHex(Random(16), 1);
	Result := S + PS;
end;

//---------------------------------------------------------------------------------------
function SimpleDecrypt(const Value: string): string; overload;
var
	X: Byte;
	I, L, P: Integer;
	ES, LS, PS: string;
begin
	if Length(Value) < 11 then
		raise Exception.Create('Decryption not possible - string ist too short.')
	else if not (AnsiChar(Value[1]) in ['0'..'9', 'A'..'F', 'a'..'f']) then
		raise Exception.Create('Decryption not possible - string is invalid.');
	LS := Copy(Value, 1, 4);
	PS := Copy(Value, Length(Value)-3, 4);
	L := encryptLengthSeed - Hex2Dec(LS, -1);
	P := encryptPositionSeed - Hex2Dec(PS, -1);
	ES := Copy(Value, P, L);
	X := Hex2Dec(Copy(ES, 1, 2), -1);
	Delete(ES, 1, 2);

	Result := '';
	I := 1;
	while I <= Length(ES) do begin
		Result := Result + Chr(Byte(not Hex2Dec(ES[I]+ES[I+1], -1) xor X));
		inc(I, 2);
	end;
end;

function SimpleDecrypt(const Value, Default: string): string; overload;
begin
	try
		Result := SimpleDecrypt(Value);
	except
		Result := Default;
	end;
end;

//---------------------------------------------------------------------------------------
function ForcePath(var Path: string): Boolean;
var
	PSub, PLast, S: string;
	IsUNC: Boolean;
begin
	S := Path;
	IsUNC := Pos('\\', S)=1;
	if IsUNC then
		Delete(S, 1, 2);
	while Pos('\', S)=1 do
		Delete(S, 1, 1);
	S := StringReplace(S, '/', '\', [rfReplaceAll]);
	S := StringReplace(S, '\\', '\', [rfReplaceAll]);
	if IsUNC then
		S := '\\' + S;
	S := ExcludeTrailingPathDelimiter(S);

	Path := S + '\'; //Bereite alles für einen vorzeitigen Ausstieg vor.
	Result := True;
	if (Length(S) < 3)
	or (ExtractFilePath(S) = S) then
		Exit;

	PSub := ExtractFilePath(S);
	PLast := Trim(ExtractFileName(S));
	Result := ForcePath(PSub);
	if Result then begin
		if DirectoryExists(PSub + PLast)
		or CreateDir(PSub + PLast) then
			Path := IncludeTrailingPathDelimiter(PSub + PLast)
		else begin
			Path := PSub;
			Result := False;
		end;
	end;
end;


//---------------------------------------------------------------------------------------
procedure CopyGraphic(FromGraph, ToGraph: TGraphic);
var
	MS: TMemoryStream;
begin
	MS := TMemoryStream.Create;
	try
		FromGraph.SaveToStream(MS);
		MS.Position := 0;
		ToGraph.LoadFromStream(MS);
	finally
		MS.Free;
	end;
end;

//---------------------------------------------------------------------------------------
procedure UnSerialize(PHPSerial: string; List: TStrings; Path: string='');
const
  ErrMsg = 'Unserialize: PHPSerial-Format ungültig an Position ';
var
  FldTyp: char;
  Content, ValName: string;
  ReadName: Boolean;
  CPos, P, L: Integer;
begin
  CPos := 1;
  ValName := Path;
  ReadName := True;
  L := Length(PHPSerial);
  while (CPos+3) < L do
  begin
    FldTyp := PHPSerial[CPos];
    if PHPSerial[CPos+1] <> ':' then
      raise Exception.Create(errMsg + IntToStr(CPos));
    inc(CPos, 2);
    try
      case FldTyp of
        'a': begin // Array
          P := PosEx(':', PHPSerial, CPos);
          StrToInt(copy(PHPSerial, CPos, P-CPos));
          CPos := succ(P);
          Unserialize(DelimitedContent(PHPSerial, '{', '}', CPos), List, ValName);
          ReadName := True;
          Continue;
        end;
        's': begin
          P := PosEx(':', PHPSerial, CPos);
          StrToInt(copy(PHPSerial, CPos, P-CPos));
          CPos := succ(P);
          Content := DelimitedContent(PHPSerial, '"', CPos);
          if (CPos>L) or (PHPSerial[CPos] <> ';') then
            raise Exception.Create(errMsg + IntToStr(CPos))
          else
            inc(CPos);
        end;
        'i': begin
          P := PosEx(';', PHPSerial, CPos);
          Content := IntToStr(StrToInt(Copy(PHPSerial, CPos, P-CPos)));
          CPos := succ(P);
        end;
        'd': begin
          P := PosEx(';', PHPSerial, CPos);
          Content := Copy(PHPSerial, CPos, P-CPos);
          FmtStrToFloat(Content, True);
          CPos := succ(P);
        end;
        else
          raise Exception.Create(errMsg + IntToStr(CPos) + ' unbekannter Datentyp');
      end;
    except
      on EConvertError do
        raise Exception.Create(errMsg + IntToStr(CPos))
      else
        raise;
    end;

    if ReadName then
      ValName := iif(Path > '', Content + '.' + Path, Content)
    else
      List.Add(ValName + List.NameValueSeparator + Content);
    ReadName := not ReadName;
  end;
end;

//---------------------------------------------------------------------------------------
function DelimitedContent(S: string; Delimiter: char; var StartPos: Integer): string;
var
  I, P1, L: Integer;
begin
  I := StartPos;
  L := Length(S);
  P1 := 0;
  Result := '';
  while (I <= L) do
  begin
    if S[I]=Delimiter then
    begin
      if P1=0 then
        P1 := I+1
      else if (I < L) and (S[I+1]=Delimiter) then
        inc(I)
      else begin
        Result := copy(S, P1, I-P1);
        StartPos := succ(I);
        Exit;
      end;
    end;
    inc(I);
  end;
end;

//---------------------------------------------------------------------------------------
function DelimitedContent(S: string; Delimiter: char): string; overload;
var
  EndPos: Integer;
begin
  EndPos := 1;
  Result := DelimitedContent(S, Delimiter, EndPos);
end;

//---------------------------------------------------------------------------------------
function DelimitedContent(S: string; StartDelim, EndDelim: char; var StartPos: Integer): string;
var
  I, P1, Lvl, L: Integer;
begin
  if StartDelim = EndDelim then
  begin
    Result := DelimitedContent(S, StartDelim, StartPos);
    Exit;
  end;
  I := StartPos;
  L := Length(S);
  Lvl := 0;
  P1 := 0;
  Result := '';
  while (I <= L) do
  begin
    if S[I]=StartDelim then
    begin
      inc(Lvl);
      if Lvl=1 then
        P1 := succ(I);
    end
    else if S[I]=EndDelim then
    begin
      dec(Lvl);
      if Lvl=0 then
      begin
        Result := copy(S, P1, I-P1);
        StartPos := succ(I);
        Exit;
      end;
    end;
    inc(I);
  end;
end;

//---------------------------------------------------------------------------------------
function DelimitedContent(S: string; StartDelim, EndDelim: char): string; overload;
var
  EndPos: Integer;
begin
  EndPos := 1;
  Result := DelimitedContent(S, StartDelim, EndDelim, EndPos);
end;


//---------------------------------------------------------------------------------------
function CalculateExpressionValue(var Expression: string; var EditPos: Integer;
                                      Options: SCalcExpressionOptions): Extended;
var
  Tmp, Fkt1: Extended;
  NumPos, I, L, Sgn, EdPosNeu, DcmExp, ThsdOffs: Integer;
  Dcml, Quo: Boolean;
  S: string;

	procedure SetThsdSep(NumOffs: Integer=0);
	var
		Dig: Integer;
	begin
		if not (ceThousandSeparator in Options) then
			Exit;
		Dig := -1;
    NumPos := Length(Expression) - NumOffs;
		while (NumPos>0) and (AnsiChar(Expression[NumPos]) in ['0'..'9']) do begin
			if Dig mod 3 = 2 then begin
				Insert('.', Expression, NumPos + 1);
				inc(ThsdOffs);
        if NumPos <= EdPosNeu then
          inc(EdPosNeu);
			end;
			dec(NumPos);
			inc(Dig);
		end;
	end;

	function SubCalc: Extended;
	begin
		if Quo then
 			Result := Fkt1 / Tmp * Sgn
		else
			Result := Fkt1 * Tmp * Sgn;
		Tmp := 0;
		Dcml := False;
		DcmExp := 10;
		Quo := False;
	end;

  procedure AddChar(C: char; Idx: Integer);
  begin
    Expression := Expression + C;
    if Idx = EditPos then
      EdPosNeu := Length(Expression);
  end;

begin
	S := Expression;
	L := Length(S);
	Expression := ''; //Der bereinigte/verarbeitete Ausdruck ist zunächst leer.
	Result := 0;
	Tmp := 0;
	Fkt1 := 1;      //Der 1.Faktor / Dividend
	Quo := False;   //Es wird kein Quotient berechnet.
	Dcml := False;  //Es wurde kein Dezimalzeichen (,) erkannt
	DcmExp := 10;   //Der aktuelle Dezimalexponent ist 10 (für 1/10)
	Sgn := 1;       //Das Vorzeichen ist +1 (positiv)
	ThsdOffs := 0;  //Der Offset für Tausendertrennungen (.) ist 0.
  EdPosNeu := 0;  //Die neue Bearbeitungs- / Cursor-Position wurde noch nicht ermittelt.
	for I := 1 to L do
  begin
		case S[I] of
      //................................................
			'0'..'9': begin
        AddChar(S[I], I);
				if Dcml then
        begin
					Tmp := Tmp + (Ord(S[I])-48)/DcmExp;
					DcmExp := DcmExp * 10;
				end
				else
					Tmp := Tmp * 10 + (Ord(S[I])-48);
      end;

      //................................................
			',', '.':
				if (S[I] = FormatSettings.DecimalSeparator) then
        begin
					SetThsdSep;
          AddChar(S[I], I);
					Dcml := True;
				end;

      //................................................
			'+', '-': begin
        AddChar(S[I], I);
				if Tmp <> 0 then
        begin
          if not Dcml then
            SetThsdSep(1);
					try
						Result := Result + SubCalc;
					except
            if ceRaiseOnError in Options then
              raise
            else
  						Break;
					end;
					Fkt1 := 1;
				end;
				Sgn := IIf(S[I]='+', 1, -1);
			end;

      //................................................
			'*', '/': begin
        if not Dcml then
          SetThsdSep;
        AddChar(S[I], I);
				try
					Fkt1 := SubCalc;
				except
          if ceRaiseOnError in Options then
            raise
          else
            Break;
				end;
				Quo := S[I] = '/';
			end;
		end;
	end;

	try
		if not Dcml then
    begin
			Dcml := True;
			SetThsdSep;
		end;
		Result := Result + SubCalc;
	except
    if ceRaiseOnError in Options then
      raise
	end;
  EditPos := EdPosNeu;
end;
function FormatHTMLForClipboard(S: string): string;
const
	HTMLHdr = 'Version:1.0'#13#10
					+ 'StartHTML:>>TKN 01<<'#13#10
					+ 'EndHTML:>>TKN 02<<'#13#10
					+ 'StartFragment:>>TKN 03<<'#13#10
					+ 'EndFragment:>>TKN 04<<'#13#10;
	HTMLFtr = #13#10;
var
	HS, HE, FS, FE: Integer;
begin
	HS := PosInText('<html>', S);
	if HS = 0 then
		Result := '<html>' + S
	else
		Result := S;

	HE := PosInText('</html>', Result);
	if HE = 0 then
		Result := HTMLHdr + Result + #13#10'</html>' + HTMLFtr
	else
		Result := HTMLHdr + Result + HTMLFtr;

	HS := PosInText('<html>', Result);
	FS := PosInText('<table', Result);
	HE := PosInText('</html>', Result) + Length('</html>');
	FE := PosInText('</table>', Result) + Length('</table>');
	Result := StringReplace(Result, '>>TKN 01<<', FormatFloat('0000000000', HS-2), []);
	Result := StringReplace(Result, '>>TKN 02<<', FormatFloat('0000000000', HE-1), []);
	Result := StringReplace(Result, '>>TKN 03<<', FormatFloat('0000000000', FS-1), []);
	Result := StringReplace(Result, '>>TKN 04<<', FormatFloat('0000000000', FE), []);
end;

{$IFDEF WINDOWS}

//---------------------------------------------------------------------------------------
function CurrentUser: string;
var
	UserLen: DWord;
	LoggedUser: PChar;
begin
	UserLen := 50;
	GetMem(LoggedUser, UserLen);
	try
		if not GetUserName(LoggedUser, UserLen) then
			Result := 'Unbekannt'
		else
			Result := LoggedUser;
	finally
		FreeMem(LoggedUser);
	end;
end; //CurrentUser

//---------------------------------------------------------------------------------------
function CurrentWorkstationName: string;
var
	PCName: PChar;
	PCNameSize: DWord;
begin
	Result := 'Unknown Workstation';
	try
		PCNameSize := 50;
		GetMem(PCName, PCNameSize);
		try
			GetComputerName(PCName, PCNameSize);
			Result := string(PCName);
		finally
			FreeMem(PCName);
		end;
	except
	end;
end; //CurrentWorkstationName

//---------------------------------------------------------------------------------------
function ShutdownComputer(ShutDownType: NShutDownType; ForceShutdown: Boolean=False): Boolean;
var
	hToken: THandle;
	TKP, NewT: TTokenPrivileges;
	RetLength, AddForce: DWORD;
begin
	Result := False;
	if ForceShutdown then
		AddForce := EWX_FORCEIFHUNG
	else
		AddForce := 0;
	hToken:=GetCurrentProcess();
	if OpenProcessToken(hToken, TOKEN_ADJUST_PRIVILEGES + TOKEN_QUERY, hToken) then
		// Get the LUID for shutdown privilege
		if LookupPrivilegeValue( Nil, 'SeShutdownPrivilege', TKP.Privileges[0].Luid) then
		begin
			TKP.PrivilegeCount := 1;  // one privilege to set
			TKP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
			// Get shutdown privilege for this process.
			if AdjustTokenPrivileges(hToken, FALSE, TKP,
											sizeof(TTokenPrivileges ), NewT , RetLength) = true then
				Result := ExitWindowsEx(aShutDownType[ShutDownType] + AddForce, 0);
		end;
end;

procedure CopyHTMLToClipBoard(HTML: string = ''; EmptyClipboard: Boolean=False);
var
	I: Integer;
	gMem: HGLOBAL;
	LockPtr: PChar;
	Fmt: array[0..1] of UINT;
	Str: array[0..1] of string;

begin
	Win32Check(OpenClipBoard(0));
	try
		Str[0] := FormatHTMLForClipboard(HTML);
		Fmt[0] := RegisterClipboardFormat('HTML Format');

		Str[1] := HTML;
		Fmt[1] := RegisterClipboardFormat('HTML (HyperText Markup Language)');

		if EmptyClipboard then
			Win32Check(EmptyClipBoard);
		if HTML = '' then
			Exit;

		for I := 0 to High(Str) do
		begin
			gMem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, Length(Str[I])+1);
			try
				Win32Check(gMem <> 0);
				LockPtr := GlobalLock(gMem);
				Win32Check(LockPtr <> nil);
				CopyMemory(LockPtr, PChar(Str[I]), Length(Str[I])+1);
			finally
				GlobalUnlock(gMem);
			end;
			Win32Check(gMem <> 0);
			SetClipboardData(Fmt[I], gMem);
			Win32Check(gMem <> 0);
		end
	finally
		Win32Check(CloseClipBoard);
	end;
end;


{$ENDIF}

initialization
  Randomize;

finalization

end.



