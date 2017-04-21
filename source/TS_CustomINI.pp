unit TS_CustomINI;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Controls,
	Classes, SysUtils;


type
	TTSCustomINI = class;

  //Aufzählungstypen für TSCustomINI
	NOnINIValueMissing = (imeAutoInsert, imeUseDefault, imeRaise);
	NTSINIAction = (iaDoLookup, iaUseDefault, iaWriteDefault, iaAbort);

  //Array-typen für TSCustomINI
	ADynStr = array of string;

  //Ereignistypen für TSCustomINI
  MTSINIGroupChanged = procedure (Sender: TTSCustomINI; const OldGroup: string; var NewGroup: string) of object;

	MTSINIReadValue = procedure (Sender: TTSCustomINI; const Group, Entry: string;
                              const Index: Integer; Encrypted: Boolean;
                              const ValType: string; var Value: string;
															var GetAction: NTSINIAction) of object;
	MTSINISetValue = procedure (Sender: TTSCustomINI; const Group, Entry: string;
                              const Index: Integer; const Encrypted: Boolean;
                              const ValType: string; var Value: string) of object;
  MTSINIDescription = procedure(Sender: TTSCustomINI; const Group, Entry: string;
                                const Index: Integer; var ADescription: string) of object;

  //Exception-Typen für TSCustomINI
	EINIError = class(Exception)
	public
		constructor Create(INIComp: TTSCustomINI; const Msg: string); overload; virtual;
		constructor Create(INIComp: TTSCustomINI; const Group, Entry: string; Index: Integer); overload; virtual;
		constructor Create(INIComp: TTSCustomINI; const Msg, Group, Entry: string; Index: Integer); overload; virtual;
	end;

	EINIValueMissing = class(EINIError)
	public
		constructor Create(INIComp: TTSCustomINI; const Group, Entry: string; Index: Integer); override;
	end;

	//Hilfsklasse für TSCustomINI
  TTSINISeparators = class(TPersistent)
  private
    fEncrypt: char;
    fValPart: char;
    fIdxEnd: char;
    fDefault: char;
    fIdxBeg: char;
    fGrp: char;
  public
		constructor Create;
  published
    property Group: char read fGrp write fGrp default '|';
    property IndexBegin: char read fIdxBeg write fIdxBeg default '[';
    property IndexEnd: char read fIdxEnd write fIdxEnd default ']';
    property Encryption: char read fEncrypt write fEncrypt default '!';
    property Default: char read fDefault write fDefault default '=';
    property ValueParts: char read fValPart write fValPart default '~';
  end;

  /// <summary>
  ///  <para>Base class to access named configuration storage values ("ini values") of several different data
  ///  types, stored in a hierarchical structure. Used as base for e.g. TSFileINI or TSDBIni.
  ///  It offers the possibility to cache values after read, before writing or both.</para>
  ///  <para>Usually, a derived class must only override the <see cref="Load"/>
  ///  and <see cref="Save"/> methods to read and write values from any storage</para>
  /// </summary>
  /// <remarks>
  ///  <para>Up to three levels of storage are possible: "Group", "Entry" (string) and "Index" (Integer).</para>
  ///  <para>All values can be stored readable or with encryption. If encryption is active,
  ///  the functions <see cref="TSLib.SimpleEncrypt"/> and <see cref="TSLib.SimpleDecrypt"/>
  ///  are used on reading or writing.</para>
  ///  <para>Reading and Writing values can be done either through the type specific array properties:
  ///  Str, Int, Bool, Color, DateTime, Point, Rect or with the Get-xx and Put-xx methods.
  ///  <para>The Get and Put methods can be used to define Group, Entry, Index, Encryption and Default
  ///   separately.
  ///  </para>
  ///  <para>Using the type specific properties the array parameter is evaluated
  /// like this:
  ///  </para>
  ///  <para>"Group Name|Entry Name[Index]!=Default Value"</para>
  ///  <para>See method <see cref="ParseLookup"/> for further Information on this parameter.</para>
  /// </remarks>

	{ TTSCustomINI }

 TTSCustomINI = class(TComponent)
	private
		fActiveGroup: string;
		fEntryMissing: NOnINIValueMissing;
		fOnGetValue: MTSINIReadValue;
		fChangeGroup: MTSINIGroupChanged;
		fOnPutValue: MTSINISetValue;
    fReadCached: Boolean;
    fCachedNames: TStringList;
    fCachedTypes: array of string;
    fCachedValues: array of string;
    fCacheChanged: array of Boolean;
    fWriteCached: Boolean;
    fSeparators: TTSINISeparators;
    fGetDescription: MTSINIDescription;
    fSetDescription: MTSINIDescription;
		function GetPropColorIdx(Lookup: string; Idx: integer): TColor;
		function GetPropTime(Lookup: string): TDateTime;
		procedure PutPropColorIdx(Lookup: string; Idx: integer; Value: TColor);
		procedure PutPropTime(Lookup: string; AValue: TDateTime);
    procedure SetReadCached(const Value: Boolean);
    procedure AddToCache(const Lookup, ValType, Value: string; ValueChanged: Boolean);
    procedure UpdateCache(const Lookup, ValType, Value: string; ValueChanged: Boolean);
    procedure SetWriteCached(const Value: Boolean);
    function GetPropStr(Lookup: string): string;
    procedure PutPropStr(Lookup: string; const Value: string);
    function GetPropBool(Lookup: string): Boolean;
    function GetPropColor(Lookup: string): TColor;
    function GetPropDateTime(Lookup: string): TDateTime;
    function GetPropInt(Lookup: string): Integer;
    function GetPropPoint(Lookup: string): TPoint;
    function GetPropRect(Lookup: string): TRect;
    procedure PutPropBool(Lookup: string; const Value: Boolean);
    procedure PutPropColor(Lookup: string; const Value: TColor);
    procedure PutPropDateTime(Lookup: string; const Value: TDateTime);
    procedure PutPropInt(Lookup: string; const Value: Integer);
    procedure PutPropPoint(Lookup: string; const Value: TPoint);
    procedure PutPropRect(Lookup: string; const Value: TRect);
    function Str2Clr(S: string): TColor;
    function Str2TS(S: string): TDateTime;
    function Str2Pt(S: string; Default: TPoint): TPoint;
    function Str2Rect(S: string; Default: TRect): TRect;
    function Pt2Str(Pt: TPoint): string;
    function Rect2Str(Rect: TRect): string; overload;
    function Rect2Str(L, T, R, B: Integer): string; overload;
    function GetValuePart(var S: string; DefVal: Integer): Integer;
    function GetPropStrIdx(Lookup: string; Idx: integer): string;
    procedure PutPropStrIdx(Lookup: string; Idx: integer; const Value: string);
    function GetPropIntIdx(Lookup: string; Idx: integer): Integer;
    procedure PutPropIntIdx(Lookup: string; Idx: integer; const Value: Integer);
    procedure PutPropDescription(Lookup: string; const Value: string);
    function GetPropDescription(Lookup: string): string;
    procedure PutPropFloat(Lookup: string; const Value: Extended);
    function GetPropFloat(Lookup: string): Extended;
    function GetPropBoolIdx(Lookup: string; Idx: integer): Boolean;
    procedure PutPropBoolIdx(Lookup: string; Idx: integer;
      const Value: Boolean);
    function Font2Str(Font: TFont): string;
    procedure Str2Font(FontStr: string; Font: TFont);
	protected
		//In allen abgeleiteten Klassen muss "RaiseOnPut:=FALSE" gesetzt werden.
		//	Ist RaiseOnPut=TRUE, wird bei jedem inherited-Aufruf in der Put-Methode
		//	einer abgeleiteten Klasse eine Exception ausgelöst: "Put-Methode nicht implementiert"
		RaiseOnPut: Boolean;
		GetAction: NTSINIAction;
		procedure SetActiveGroup(const Value: string); virtual;

    /// <summary>
    ///  <para>
    ///  This Method is called by the <see cref="Get"/> Methods.
    ///  In derived classes the overridden method should load a string value from the permanent storage.
    ///  (i.e. from the .ini file, the registry or the database storage).
    ///  If the permanent storage does not contain a corresponding value it should call
    ///  this inherited method that implements the default behaviour.
    ///  </para>
    ///  <para>
    ///  The TTSCustomINI base class implements the behaviour for missing values.
    ///  </para>
    ///  <para>
    ///  Depending on the setting of <see cref="OnMissingEntry"/> it returns
    ///  the <see cref="Default"/> value, raises an exception or calls <see cref="Put"/>
    ///  to write the <see cref="Default"/> value to the permanent storage.
    ///  </para>
    /// </summary>
		function Load(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType, Default: string): string; virtual;
		procedure Save(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType, Value: string); virtual;

    /// <summary>
    ///  Evaluates the parameter <c>Lookup</c> to find the complete lookup position
    ///  as defined by Group, Entry, Index, Encryption , Default.
    ///  Definition of the lookup string:
    ///  "Group Name|Entry Name[Index]!=Default Value"
    ///  Entry Name is mandatory, all other parts are optional.
    /// </summary>
    ///  <remarks>
    ///  <para>The and special characrters used in the lookup string (|, [, ], !, =)
    ///   can be changed using the properties <see cref="SepGroup"/>,
    ///  <see cref="SepIndexBegin"/>, <see cref="SepIndexEnd"/>, <see cref="SepEncryption"/>
    ///  and <see cref="SepDefault"/>
    ///  </para>
    ///  <para></para>
    ///  <para></para>
    ///  <para></para>
    ///  <para>If the optional Part "!" is set, the value is stored with encryption.
    ///  If Index does not evaluate to an Integer, it is set to 0</para>
    ///  <para>Examples:</para>
    ///  <para>"MyGroup|MyEntry=Empty" -> Group=MyGroup; Entry=MyEntry; Index=0; Encryption=No; Default=Empty</para>
    ///  <para>"MyEntry" - Group=ActiveGroup; Entry=MyEntry; Index=0; Encryption=No; Default=''</para>
    ///  <para>"MyEntry[99]" - Group=ActiveGroup; Entry=MyEntry; Index=99; Encryption=No; Default=''</para>
    ///  <para>"MyEntry[WRKS]!" - Group=ActiveGroup; Entry=MyEntry; Index=0; Encryption=Yes; Default=''</para>
    ///  </remarks>
    procedure ParseLookup(Lookup: string; out Group, Entry: string;
                          out Index: Integer; out Encrypted: Boolean; out Default: string);
    function BuildLookup(const Group, Entry: string; const Index: Integer;
                          const Encrypted: Boolean; const Default: string=''): string;

    procedure PutDescription(const Group, Entry: string; const Index: Integer; ADescription: string); virtual;
    function GetDescription(const Group, Entry: string; const Index: Integer): string; virtual;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

    procedure ClearCache;
    procedure FlushCache(WithClear: Boolean=False);

    /// <summary>
    ///  Reads a value from the configuration storage. If caching is active (<see cref="UseReadCache"/>)
    ///  and the value has been read before, it is loaded from the read cache. Otherwise
    ///  is is loaded from the permanent storage (see <see cref="Load"/>).
    /// </summary>
    /// <remarks>
    ///   For use of the <c>Lookup</c> parameter see description of <see cref="ParseLookup"/>
    /// </remarks>
    function Get(const Lookup, ValType: string): string; overload;
    function Get(const Lookup, ValType: string; Index: Integer): string; overload;
    function Get(const Lookup, ValType, Default: string; PreferLUDefault: Boolean=False): string; overload;
		function Get(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType: string; Default: string): string; overload;

    /// <summary>
    ///  Writes a configuration value to the permanent storage. If <see cref="UseWriteCache"/> is active
    ///  the value is stored to the internal cache. In this case all changed values are written
    ///  to the permanent storage with <see cref="FlushCache"/> or before destruction of the component.
    ///  Otherwise the value is written using the <see cref="Save"/> method.
    /// </summary>
    procedure Put(const Lookup, ValType, Value: string); overload;
    procedure Put(const Lookup, ValType: string; Index: Integer; const Value: string); overload;
		procedure Put(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType: string; Value: string); overload;

    /// <summary>
    ///  <para>Evaluates the parameter <c>Lookup</c> to find the complete lookup position
    ///  as defined by Group, Entry, Index, Encryption , Default.</para>
    ///  <para>Definition of the lookup string:</para>
    ///  <para>"Group Name|Entry Name[Index]!=Default Value"</para>
    ///  <para>Entry Name is mandatory, all other parts are optional.</para>
    /// </summary>
    ///  <remarks>
    ///  <para>The special characters used in the lookup string (|, [, ], !, =)
    ///   can be changed using the properties <see cref="SepGroup"/>,
    ///  <see cref="SepIndexBegin"/>, <see cref="SepIndexEnd"/>, <see cref="SepEncryption"/>
    ///  and <see cref="SepDefault"/>
    ///  </para>
    ///  <para></para>
    ///  <para></para>
    ///  <para></para>
    ///  <para>If the optional Part "!" is set, the value is stored with encryption.
    ///  If Index does not evaluate to an Integer, it is set to 0</para>
    ///  <para>Examples:</para>
    ///  <para>"MyGroup|MyEntry=Empty" -> Group=MyGroup; Entry=MyEntry; Index=0; Encryption=No; Default=Empty</para>
    ///  <para>"MyEntry" - Group=ActiveGroup; Entry=MyEntry; Index=0; Encryption=No; Default=''</para>
    ///  <para>"MyEntry[99]" - Group=ActiveGroup; Entry=MyEntry; Index=99; Encryption=No; Default=''</para>
    ///  <para>"MyEntry[WRKS]!" - Group=ActiveGroup; Entry=MyEntry; Index=0; Encryption=Yes; Default=''</para>
    ///  </remarks>
    property Str[Lookup: string]: string read GetPropStr write PutPropStr; default;
    property StrIdx[Lookup: string; Idx: integer]: string read GetPropStrIdx write PutPropStrIdx;
    /// <summary>
    ///  Evaluates the parameter <c>Lookup</c> to find the complete lookup position
    ///  as defined by Group, Entry, Index, Encryption , Default.
    ///  Definition of the lookup string:
    ///  "Group Name|Entry Name[Index]!=Default Value"
    ///  Entry Name is mandatory, all other parts are optional.
    /// </summary>
    ///  <remarks>
    ///  <para>The special characters used in the lookup string (|, [, ], !, =)
    ///   can be changed using the properties <see cref="SepGroup"/>,
    ///  <see cref="SepIndexBegin"/>, <see cref="SepIndexEnd"/>, <see cref="SepEncryption"/>
    ///  and <see cref="SepDefault"/>
    ///  </para>
    ///  <para></para>
    ///  <para></para>
    ///  <para></para>
    ///  <para>If the optional Part "!" is set, the value is stored with encryption.
    ///  If Index does not evaluate to an Integer, it is set to 0</para>
    ///  <para>Examples:</para>
    ///  <para>"MyGroup|MyEntry=Empty" -> Group=MyGroup; Entry=MyEntry; Index=0; Encryption=No; Default=Empty</para>
    ///  <para>"MyEntry" - Group=ActiveGroup; Entry=MyEntry; Index=0; Encryption=No; Default=''</para>
    ///  <para>"MyEntry[99]" - Group=ActiveGroup; Entry=MyEntry; Index=99; Encryption=No; Default=''</para>
    ///  <para>"MyEntry[WRKS]!" - Group=ActiveGroup; Entry=MyEntry; Index=0; Encryption=Yes; Default=''</para>
    ///  </remarks>
    property Int[Lookup: string]: Integer read GetPropInt write PutPropInt;
    property IntIdx[Lookup: string; Idx: integer]: Integer read GetPropIntIdx write PutPropIntIdx;
    property Float[Lookup: string]: Extended read GetPropFloat write PutPropFloat;
    property Bool[Lookup: string]: Boolean read GetPropBool write PutPropBool;
    property BoolIdx[Lookup: string; Idx: integer]: Boolean read GetPropBoolIdx write PutPropBoolIdx;
    property Color[Lookup: string]: TColor read GetPropColor write PutPropColor;
    property ColorIdx[Lookup: string; Idx: integer]: TColor read GetPropColorIdx write PutPropColorIdx;
    property DateTime[Lookup: string]: TDateTime read GetPropDateTime write PutPropDateTime;
    property Time[Lookup: string]: TDateTime read GetPropTime write PutPropTime;
    property Point[Lookup: string]: TPoint read GetPropPoint write PutPropPoint;
    property Rect[Lookup: string]: TRect read GetPropRect write PutPropRect;
    property Description[Lookup: string]: string read GetPropDescription write PutPropDescription;

    //Properties to become published in descendant components
		property ActiveGroup: string read fActiveGroup write SetActiveGroup;
    property LookupSeparators: TTSINISeparators read fSeparators write fSeparators;
    property UseReadCache: Boolean read fReadCached write SetReadCached default True;
    property UseWriteCache: Boolean read fWriteCached write SetWriteCached default False;
		property OnMissingEntry: NOnINIValueMissing read fEntryMissing write fEntryMissing default imeAutoInsert;

		property OnChangeGroup: MTSINIGroupChanged read fChangeGroup write fChangeGroup;
		property OnGetValue: MTSINIReadValue read fOnGetValue write fOnGetValue;
		property OnPutValue: MTSINISetValue read fOnPutValue write fOnPutValue;
    property OnGetDescription: MTSINIDescription read fGetDescription write fGetDescription;
    property OnSetDescription: MTSINIDescription read fSetDescription write fSetDescription;

    //Methods to Read and Write INI-Values.
		function GetStr(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: string): string; overload;
		function GetStr(const Lookup, Default: string): string; overload;
		procedure PutStr(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: string);
		function GetInt(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: Integer): Integer; overload;
		function GetInt(const Lookup: string; Default: Integer): Integer; overload;
		procedure PutInt(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: Integer);
		function GetFloat(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: Extended): Extended; overload;
		function GetFloat(const Lookup: string; Default: Extended): Extended; overload;
		procedure PutFloat(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: Extended);
		function GetBool(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: Boolean): Boolean; overload;
		function GetBool(const Lookup: string; Default: Boolean): Boolean; overload;
		procedure PutBool(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: Boolean);
		function GetColor(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: TColor): TColor; overload;
		function GetColor(const Lookup: string; Default: TColor): TColor; overload;
		procedure PutColor(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: TColor);
		function GetTimeStamp(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: TDateTime): TDateTime; overload;
		function GetTimeStamp(const Lookup: string; Default: TDateTime): TDateTime; overload;
		procedure PutTimeStamp(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: TDateTime);
    function GetTime(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: TDateTime): TDateTime; overload;
		function GetTime(const Lookup: string; Default: TDateTime): TDateTime; overload;
		procedure PutTime(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: TDateTime);

    procedure GetFont(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Font: TFont); overload;
    procedure GetFont(const Lookup: string; Font: TFont); overload;
    procedure PutFont(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Font: TFont); overload;
    procedure PutFont(const Lookup: string; Font: TFont); overload;

    procedure GetStrings(const Group, Entry: string; Index: Integer; Encrypted: Boolean; List: TStrings); overload;
    procedure GetStrings(const Lookup: string; List: TStrings); overload;
    procedure PutStrings(const Group, Entry: string; Index: Integer; Encrypted: Boolean; List: TStrings); overload;
    procedure PutStrings(const Lookup: string; List: TStrings); overload;

		//Füllt ein Array mit den Einträgen. Fehlt der Eintrag wird der jeweilige Array-Eintrag auf Default gesetzt.
		// Der Eintrag mit Index 0 gibt die Anzahl Elemente an.
		procedure GetStrArray(const Group, Entry: string; Index: Integer; Encrypted: Boolean; out StrArr: ADynStr); overload;
		procedure GetStrArray(const Lookup: string; out StrArr: ADynStr); overload;
		procedure PutStrArray(const Group, Entry: string; Index: Integer; Encrypted: Boolean; StrArr: ADynStr); overload;
		procedure PutStrArray(const Lookup: string; StrArr: ADynStr); overload;

		procedure LoadCtrlBounds(Ctrl: TControl);
		procedure SaveCtrlBounds(Ctrl: TControl);
//		//Füllt ein Array mit den Einträgen. Bei Konvertierungsfehler wird der jeweilge Array-Eintrag mit dem default gesetzt.
//		// Der Eintrag mit Index 0 gibt die Anzahl Elemente an.
//		function GetIntArray(const Group, Entry: string; out IntArr: ADynInt; Default: Integer=0): Integer; overload;
//		function GetIntArray(const Entry: string; out IntArr: ADynInt; Default: Integer=0): Integer; overload;
//
//		//Füllt ein Array mit den Einträgen. Fehlt der Eintrag wird der jeweilige Array-Eintrag auf Default gesetzt.
//		// Der Eintrag mit Index 0 gibt die Anzahl Elemente an.
//		function GetStrArray(const Group, Entry: string; out StrArr: ADynStr; Default: string=''): Integer; overload;
//		function GetStrArray(const Entry: string; out StrArr: ADynStr; Default: string=''): Integer; overload;
//
//		//Füllt ein Array mit den Einträgen. Bei Konvertierungsfehler wird der jeweilge Array-Eintrag mit dem default gesetzt.
//		// Der Eintrag mit Index 0 gibt die Anzahl Elemente an.
//		function GetBoolArray(const Group, Entry: string; out BoolArr: ADynBool; Default: Boolean=False): Integer; overload;
//		function GetBoolArray(const Entry: string; out BoolArr: ADynBool; Default: Boolean=False): Integer; overload;
//
//		//Liest alle Einträge mit Namen "Entry" und MinIndex <= Index <= MaxIndex und füllt die "EntryList"
//		// nur mit den vorhandenen Einträgen im Format "<Index>=<String-Wert>"
//		procedure GetEntries(const Group, Entry: string; const EntryList: TStrings;
//																												MinIndex, MaxIndex: Integer); overload;
//		procedure GetEntries(const Group, Entry: string; const EntryList: TStrings); overload;
//		procedure GetEntries(const Entry: string; const EntryList: TStrings;
//																												MinIndex, MaxIndex: Integer); overload;
//		procedure GetEntries(const Entry: string; const EntryList: TStrings); overload;
//
//		//Alle String in "EntryList", die das Format "<Index>=<String-Wert>" aufweisen, werden
//		// mit dem Namen "Entry" und dem eingetragenen Index geschrieben.
//		// Wird die Version mit "MinIndex" und "MaxIndex" verwendet, werden alle Strings, deren <Index>
//		// nicht zwischen MinIndex und MaxIndex liegt ignoriert.
//		// Wird die Version ohne MinIndex und MaxIndex verwendet, schreibt die Prozedur zusätzlich
//		// zwei Einträge "<Entry>_MinIndex" und "<Entry>_MaxIndex".
//		procedure PutEntries(const Group, Entry: string; const EntryList: TStrings;
//																												MinIndex, MaxIndex: Integer); overload;
//		procedure PutEntries(const Group, Entry: string; const EntryList: TStrings); overload;
//		procedure PutEntries(const Entry: string; const EntryList: TStrings;
//																												MinIndex, MaxIndex: Integer); overload;
//		procedure PutEntries(const Entry: string; const EntryList: TStrings); overload;
//
	end;


const
  fmtINITimeStamp = 'yyyymmdd hhnnsszzz';
  fmtINITime = 'hhnnsszzz';
	valTypeString = 'str';
	valTypeInt = 'int';
  valTypeFloat = 'float';
	valTypeBool = 'bool';
  valTypeColor = 'color';
	valTypeTimeStamp = 'TS';
	valTypeTime = 'time';
  valTypePoint = 'point';
	valTypeListHdr = 'SLHdr';
	valTypeListItem = 'SLItm';
  valTypeFont = 'Font';

implementation

uses
	TSLibDateTime,
  TSLibConvert,
  TSLibColors,
  TSResources,
  TSLib,
	StrUtils;

const
  fntStyleBegin = '[';
  fntStyleEnd= ']';
  fntSizeBegin = '<';
  fntSizeEnd = '>';
  fntColorBegin = '{';
  fntColorEnd= '}';

  iniSepDefaultGroup = '|';
  iniSepDefaultIndexBegin = '[';
  iniSepDefaultIndexEnd = ']';
  iniSepDefaultEncrypt = '!';
  iniSepDefaultDefaultValue = '=';
  iniSepDefaultValuePart = '~';
  iniDefaultActiveGroup = 'General';


//***************************************************************************************
//***************************************************************************************
// TTSINISeparators
//***************************************************************************************
//***************************************************************************************

constructor TTSINISeparators.Create;
begin
  inherited Create;
  fGrp := iniSepDefaultGroup;
  fIdxBeg := iniSepDefaultIndexBegin;
  fIdxEnd := iniSepDefaultIndexEnd;
  fEncrypt := iniSepDefaultEncrypt;
  fDefault := iniSepDefaultDefaultValue;
  fValPart := iniSepDefaultValuePart;
end;

//***************************************************************************************
//***************************************************************************************
//TTSCustomINI
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSCustomINI.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	fEntryMissing := imeAutoInsert;
	fActiveGroup := iniDefaultActiveGroup;
  fReadCached := True;
  fCachedNames := TStringList.Create;
  fSeparators := TTSINISeparators.Create;
	RaiseOnPut := True;
end;

//***************************************************************************************
destructor TTSCustomINI.Destroy;
begin
  if fWriteCached then
    FlushCache;
  fCachedNames.Free;
  SetLength(fCachedTypes, 0);
  SetLength(fCachedValues, 0);
  SetLength(fCacheChanged, 0);
  fSeparators.Free;
	inherited;
end;

//***************************************************************************************
procedure TTSCustomINI.SetActiveGroup(const Value: string);
var
	NewGroup: string;
begin
	NewGroup := Value;
	if Assigned(fChangeGroup) then
		fChangeGroup(Self, fActiveGroup, NewGroup);
	fActiveGroup := NewGroup;
end;

//***************************************************************************************
procedure TTSCustomINI.SetReadCached(const Value: Boolean);
begin
  if fReadCached and not Value then
  begin
    if fWriteCached then
      FlushCache(True)
    else
      ClearCache;
  end;
  fReadCached := Value;
end;

procedure TTSCustomINI.SetWriteCached(const Value: Boolean);
begin
  if fWriteCached and not Value then
    FlushCache(not fReadCached);
  fWriteCached := Value;
end;

//***************************************************************************************
procedure TTSCustomINI.AddToCache(const Lookup, ValType, Value: string; ValueChanged: Boolean);
var
  Idx: Integer;
begin
  Idx := fCachedNames.Add(Lookup);
  if Idx >= Length(fCachedValues) then
    SetLength(fCachedValues, Idx+1);
  fCachedValues[Idx] := Value;
  if Idx >= Length(fCachedTypes) then
    SetLength(fCachedTypes, Idx+1);
  fCachedTypes[Idx] := ValType;
  if Idx >= Length(fCacheChanged) then
    SetLength(fCacheChanged, Idx+1);
  fCacheChanged[Idx] := ValueChanged;
end;

//***************************************************************************************
procedure TTSCustomINI.UpdateCache(const Lookup, ValType, Value: string; ValueChanged: Boolean);
var
  Idx: Integer;
begin
  Idx := fCachedNames.IndexOf(Lookup);
  if Idx < 0 then
    AddToCache(Lookup, ValType, Value, ValueChanged)
  else
  begin
    if Idx >= Length(fCachedValues) then
      SetLength(fCachedValues, Idx+1);
    fCachedValues[Idx] := Value;
    if Idx >= Length(fCachedTypes) then
      SetLength(fCachedTypes, Idx+1);
    fCachedTypes[Idx] := ValType;
    if Idx >= Length(fCacheChanged) then
    begin
      SetLength(fCacheChanged, Idx+1);
      fCacheChanged[Idx] := ValueChanged;
    end
    else
      fCacheChanged[Idx] := ValueChanged or fCacheChanged[Idx];
  end;
end;

//***************************************************************************************
procedure TTSCustomINI.ClearCache;
begin
  fCachedNames.Clear;
  SetLength(fCachedTypes, 0);
  SetLength(fCachedValues, 0);
  SetLength(fCacheChanged, 0);
end;

//***************************************************************************************
procedure TTSCustomINI.FlushCache(WithClear: Boolean);
var
  I, Index: Integer;
  Group, Entry, Default: string;
  Encrypted: Boolean;
begin
  for I := 0 to pred(fCachedNames.Count) do
  begin
    if fCacheChanged[I] then
    begin
      ParseLookup(fCachedNames[I], Group, Entry, Index, Encrypted, Default);
      Save(Group, Entry, Index, Encrypted, fCachedTypes[I], fCachedValues[I]);
      fCacheChanged[I] := False;
    end;
  end;
  if WithClear then
    ClearCache;
end;

//***************************************************************************************
function TTSCustomINI.BuildLookup(const Group, Entry: string; const Index: Integer;
                          const Encrypted: Boolean; const Default: string): string;
begin
  if Group='' then
    Result := fActiveGroup + fSeparators.Group + Entry
  else
    Result := Group + fSeparators.Group + Entry;
  if Index <> 0 then
    Result := Result + fSeparators.IndexBegin + IntToStr(Index) + fSeparators.IndexEnd;
  if Encrypted then
    Result := Result + fSeparators.Encryption;
  if Default > '' then
    Result := Result + fSeparators.Default + Default;
end;

//***************************************************************************************
procedure TTSCustomINI.ParseLookup(Lookup: string; out Group, Entry: string;
                          out Index: Integer; out Encrypted: Boolean; out Default: string);

var
  P, E: Integer;
begin
  P := Pos(fSeparators.Default, Lookup);
  if P > 0 then
  begin
    Default := Copy(Lookup, P+1, MaxInt);
    Delete(Lookup, P, MaxInt);
  end
  else
    Default := '';

  P := Length(Lookup);
  if P > 0 then
  begin
    Encrypted := Lookup[P] = fSeparators.Encryption;
    if Encrypted then
      Delete(Lookup, P, 1);
  end
  else
    Encrypted := False;

  P := Pos(fSeparators.Group, Lookup);
  if P > 0 then
  begin
    Group := Copy(Lookup, 1, P-1);
    Delete(Lookup, 1, P);
  end
  else
    Group := fActiveGroup;

  P := Pos(fSeparators.IndexBegin, Lookup);
  if P > 0 then
  begin
    Entry := Copy(Lookup, 1, P-1);
    E := PosEx(fSeparators.IndexEnd, Lookup, P+1);
    if E > 0 then
    begin
      Index := StrToIntDef(Copy(Lookup, P+1, E-P-1), 0);
      Delete(Lookup, P, E-P+1);
    end
    else
      Index := 0;
  end
  else
    Index := 0;
  Entry := Lookup;
end;

//***************************************************************************************
function TTSCustomINI.Get(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType: string; Default: string): string;
var
  LUName: string;
  Idx: Integer;
begin
	GetAction := iaDoLookup;
	if Assigned(fOnGetValue) then
		fOnGetValue(Self, Group, Entry, Index, Encrypted, ValType, Default, GetAction);

	case GetAction of
		iaAbort:
			Abort;
		iaUseDefault:
			Result := Default;
		iaWriteDefault: begin
			Put(Group, Entry, Index, Encrypted, ValType, Default);
			Result := Default;
		end;
		else begin
      if fReadCached then
      begin
        LUName := BuildLookup(Group, Entry, Index, Encrypted);
        Idx := fCachedNames.IndexOf(LUName);
        if Idx >= 0 then
          Result := fCachedValues[Idx]
        else begin
          Result := Load(Group, Entry, Index, Encrypted, ValType, Default);
          UpdateCache(BuildLookup(Group, Entry, Index, Encrypted), ValType, Result, False);
        end;
      end
      else
        Result := Load(Group, Entry, Index, Encrypted, ValType, Default);

      if Encrypted then
        try
          Result := SimpleDecrypt(Result);
        except //if Decrypting fails, the value is probably readable. Overwrite it with an encrypted value.
          Put(Group, Entry, Index, Encrypted, ValType, Result);
        end
    end;
	end;
end;

//***************************************************************************************
function TTSCustomINI.Get(const Lookup, ValType: string; Index: Integer): string;
var
  Group, Entry, LUDefault: string;
  Idx: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Idx, Encrypted, LUDefault);
  Result := Get(Group, Entry, Index, Encrypted, ValType, LUDefault);
end;

//***************************************************************************************
function TTSCustomINI.Get(const Lookup, ValType, Default: string; PreferLUDefault: Boolean): string;
var
  Group, Entry, LUDefault: string;
  Index: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
  if PreferLUDefault then
    Result := Get(Group, Entry, Index, Encrypted, ValType, LUDefault)
  else
    Result := Get(Group, Entry, Index, Encrypted, ValType, Default);
end;

//***************************************************************************************
function TTSCustomINI.Get(const Lookup, ValType: string): string;
var
  Group, Entry, LUDefault: string;
  Index: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
  Result := Get(Group, Entry, Index, Encrypted, ValType, LUDefault);
end;

//***************************************************************************************
procedure TTSCustomINI.Put(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType: string; Value: string);
begin
	if Assigned(fOnPutValue) then
		fOnPutValue(Self, Group, Entry, Index, Encrypted, ValType, Value);

  if Encrypted then
    Value := SimpleEncrypt(Value);

  if fWriteCached then
    UpdateCache(BuildLookup(Group, Entry, Index, Encrypted), ValType, Value, True)
  else begin
    Save(Group, Entry, Index, Encrypted, ValType, Value);
    if fReadCached then
      UpdateCache(BuildLookup(Group, Entry, Index, Encrypted), ValType, Value, False)
  end;
end;

//***************************************************************************************
procedure TTSCustomINI.Put(const Lookup, ValType: string; Index: Integer;
  const Value: string);
var
  Group, Entry, LUDefault: string;
  Idx: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Idx, Encrypted, LUDefault);
  Put(Group, Entry, Index, Encrypted, ValType, Value);
end;

//***************************************************************************************
procedure TTSCustomINI.Put(const Lookup, ValType, Value: string);
var
  Group, Entry, LUDefault: string;
  Index: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
	Put(Group, Entry, Index, Encrypted, ValType, Value);
end;

//***************************************************************************************
function TTSCustomINI.Load(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType, Default: string): string;
begin
  case fEntryMissing of
    imeAutoInsert: begin
      Put(Group, Entry, Index, Encrypted, ValType, Default);
      if Encrypted then
        Result := SimpleEncrypt(Default)
      else
        Result := Default;
    end;
    imeRaise:
      raise EINIValueMissing.Create(Self, Group, Entry, Index);
    else if Encrypted then //imeUseDefault
      Result := SimpleEncrypt(Default)
    else
      Result := Default;
  end;
end;

//***************************************************************************************
procedure TTSCustomINI.Save(const Group, Entry: string; const Index: Integer; const Encrypted: Boolean;
                        const ValType, Value: string);
begin
	if RaiseOnPut then
		raise EINIError.Create(Self, rsINISaveNotImplemented, Group, Entry, Index);
end;

//***************************************************************************************
procedure TTSCustomINI.PutDescription(const Group, Entry: string; const Index: Integer; ADescription: string);
begin
  if Assigned(fSetDescription) then
    fSetDescription(Self, Group, Entry, Index, ADescription);
end;

//***************************************************************************************
function TTSCustomINI.GetDescription(const Group, Entry: string; const Index: Integer): string;
begin
  Result := '';
  if Assigned(fGetDescription) then
    fGetDescription(Self, Group, Entry, Index, Result);
end;


//***************************************************************************************
// Hilfsfunktionen zur Umwandlung bei Get- / Put-Methoden
//***************************************************************************************
function TTSCustomINI.Str2Clr(S: string): TColor;
begin
	try
		Result := TSStringToColor(S);
	except
		Result := TSStringToColor('cl' + S);
	end;
end;

function TTSCustomINI.Str2TS(S: string): TDateTime;
var
  L, I, Hr, Min, Sec, MSec, D, M, Y: Integer;
  Fmt: string;
begin
  Hr := 0; Min := 0; Sec := 0; MSec := 0;
  D := 0; M := 0; Y := 0;
  L := MinVal(Length(S), Length(fmtINITimeStamp));
  Fmt := LowerCase(fmtINITimeStamp);
  for I := 1 to L do
  begin
    case Fmt[I] of
      'y': Y := Y * 10 + StrToInt(S[I]);
      'm': M := M * 10 + StrToInt(S[I]);
      'd': D := D * 10 + StrToInt(S[I]);
      'h': Hr := Hr * 10 + StrToInt(S[I]);
      'n': Min := Min * 10 + StrToInt(S[I]);
      's': Sec := Sec * 10 + StrToInt(S[I]);
      'z': MSec := MSec * 10 + StrToInt(S[I]);
    end;
  end;
  Result:= EncodeDate(Y, M, D) + EncodeTime(Hr, Min, Sec, MSec);
end;

function TTSCustomINI.GetValuePart(var S: string; DefVal: Integer): Integer;
var
  P: Integer;
begin
  P := Pos(fSeparators.ValueParts, S);
  if P = 0 then begin
    Result := StrToIntDef(S, DefVal);
    S := '';
  end
  else begin
    Result := StrToIntDef(Copy(S, 1, P-1), DefVal);
    Delete(S, 1, P);
  end;
end;

function TTSCustomINI.Str2Pt(S: string; Default: TPoint): TPoint;
begin
  Result.X := GetValuePart(S, Default.X);
  Result.Y := GetValuePart(S, Default.Y);
end;

function TTSCustomINI.Str2Rect(S: string; Default: TRect): TRect;
begin
  Result.Left := GetValuePart(S, Default.Left);
  Result.Top := GetValuePart(S, Default.Top);
  Result.Right := GetValuePart(S, Default.Right);
  Result.Bottom := GetValuePart(S, Default.Bottom);
end;

function TTSCustomINI.Pt2Str(Pt: TPoint): string;
begin
  Result := IntToStr(Pt.x) + fSeparators.ValueParts + IntToStr(Pt.y);
end;

function TTSCustomINI.Rect2Str(Rect: TRect): string;
begin
	Result := IntToStr(Rect.Left) + fSeparators.ValueParts
            + IntToStr(Rect.Top) + fSeparators.ValueParts
            + IntToStr(Rect.Right) + fSeparators.ValueParts
            + IntToStr(Rect.Bottom);
end;

function TTSCustomINI.Rect2Str(L, T, R, B: Integer): string;
begin
	Result := IntToStr(L) + fSeparators.ValueParts
            + IntToStr(T) + fSeparators.ValueParts
            + IntToStr(R) + fSeparators.ValueParts
            + IntToStr(B);
end;

function TTSCustomINI.Font2Str(Font: TFont): string;
begin
  Result := fntStyleBegin
          + IIf(fsBold in Font.Style, 'B', '')
          + IIf(fsItalic in Font.Style, 'I', '')
          + IIf(fsUnderline in Font.Style, 'U', '')
          + IIf(fsStrikeOut in Font.Style, 'S', '')
          + fntStyleEnd + fntSizeBegin
          + IntToStr(Font.Size)
          + fntSizeEnd + fntColorBegin
          + ColorToString(Font.Color)
          + fntColorEnd
          + Font.Name;
end;

procedure TTSCustomINI.Str2Font(FontStr: string; Font: TFont);
var
  I, B, E: Integer;
  FS: TFontStyles;
begin
  B := Pos(fntStyleBegin, FontStr);
  E := Pos(fntStyleEnd, FontStr);
  if (B>0) and (E>B) then
  begin
    FS := [];
    for I := B+1 to E-1 do
    begin
      if UpCase(FontStr[I]) = 'B' then
        Include(FS, fsBold);
      if UpCase(FontStr[I]) = 'I' then
        Include(FS, fsItalic);
      if UpCase(FontStr[I]) = 'U' then
        Include(FS, fsUnderline);
      if UpCase(FontStr[I]) = 'S' then
        Include(FS, fsStrikeOut);
    end;
    Font.Style := FS;
    Delete(FontStr, 1, E);
  end;
  B := Pos(fntSizeBegin, FontStr);
  E := Pos(fntSizeEnd, FontStr);
  if (B>0) and (E>B) then
  begin
    Font.Size := StrToIntDef(Copy(FontStr, B+1, E-B-1), Font.Size);
    Delete(FontStr, 1, E);
  end;
  B := Pos(fntColorBegin, FontStr);
  E := Pos(fntColorEnd, FontStr);
  if (B>0) and (E>B) then
  begin
    try //Ignore color name errors - use default font color instead.
      Font.Color := Str2Clr(Copy(FontStr, B+1, E-B-1));
    except
    end;
    Delete(FontStr, 1, E);
  end;
  Font.Name := FontStr;
end;


//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetPropStr(Lookup: string): string;
begin
  Result := Get(Lookup, valTypeString);
end;

function TTSCustomINI.GetPropStrIdx(Lookup: string; Idx: integer): string;
begin
  Result := Get(Lookup, valTypeString, Idx);
end;

function TTSCustomINI.GetPropInt(Lookup: string): Integer;
begin
  try
    Result := StrToInt(Get(Lookup, valTypeInt))
  except
    Result := 0;
    Put(Lookup, valTypeInt, '0');
  end;
end;

function TTSCustomINI.GetPropIntIdx(Lookup: string; Idx: integer): Integer;
begin
  try
    Result := StrToInt(Get(Lookup, valTypeInt, Idx));
  except
    Result := 0;
    Put(Lookup, valTypeInt, Idx, '0');
  end;
end;

function TTSCustomINI.GetPropFloat(Lookup: string): Extended;
begin
  try
    Result := INIStrFloat(Get(Lookup, valTypeFloat));
  except
    Result := 0.0;
    Put(Lookup, valTypeFloat, '0.0');
  end;
end;

function TTSCustomINI.GetPropBool(Lookup: string): Boolean;
begin
  case TriState(Get(Lookup, valTypeBool)) of
    tsFalse: Result := False;
    tsTrue: Result := True;
    else begin //TSNull
      Result := False;
      Put(Lookup, valTypeBool, '0');
    end;
  end;
end;

function TTSCustomINI.GetPropBoolIdx(Lookup: string; Idx: integer): Boolean;
begin
  case TriState(Get(Lookup, valTypeBool, Idx)) of
    tsFalse: Result := False;
    tsTrue: Result := True;
    else begin //TSNull
      Result := False;
      Put(Lookup, valTypeBool, Idx, '0');
    end;
  end;
end;

function TTSCustomINI.GetPropColor(Lookup: string): TColor;
begin
  try
    Result := Str2Clr(Get(Lookup, valTypeColor));
  except
    Result := clNone;
    Put(Lookup, valTypeColor, 'clNone');
  end;
end;

function TTSCustomINI.GetPropColorIdx(Lookup: string; Idx: integer): TColor;
begin
  try
    Result := Str2Clr(Get(Lookup, valTypeColor, Idx));
  except
    Result := clNone;
    Put(Lookup, valTypeColor, 'clNone');
  end;
end;

function TTSCustomINI.GetPropTime(Lookup: string): TDateTime;
begin
  try
    Result := EvalTime(Get(Lookup, valTypeTime));
  except
    Result := 0;
    Put(Lookup, valTypeTime, FormatDateTime(fmtINITime, 0));
  end;
end;

function TTSCustomINI.GetPropDateTime(Lookup: string): TDateTime;
begin
  try
    Result := Str2TS(Get(Lookup, valTypeTimeStamp));
  except
    Result := 0;
    Put(Lookup, valTypeTimeStamp, FormatDateTime(fmtINITimeStamp, 0));
  end;
end;

function TTSCustomINI.GetPropPoint(Lookup: string): TPoint;
var
  Def: string;
begin
  Def := Pt2Str(Classes.Point(0, 0));
  Result := Str2Pt(Get(Lookup, valTypePoint, Def, True), Classes.Point(0,0));
end;

function TTSCustomINI.GetPropRect(Lookup: string): TRect;
var
  Def: string;
begin
  Def := Rect2Str(0, 0, 0, 0);
  Result := Str2Rect(Get(Lookup, valTypePoint, Def, True), Classes.Rect(0,0,0,0));
end;

procedure TTSCustomINI.PutPropStr(Lookup: string; const Value: string);
begin
	Put(Lookup, valTypeString, Value);
end;

procedure TTSCustomINI.PutPropStrIdx(Lookup: string; Idx: integer; const Value: string);
begin
	Put(Lookup, valTypeString, Idx, Value);
end;

procedure TTSCustomINI.PutPropInt(Lookup: string; const Value: Integer);
begin
	Put(Lookup, valTypeInt, IntToStr(Value));
end;

procedure TTSCustomINI.PutPropIntIdx(Lookup: string; Idx: integer;
  const Value: Integer);
begin
	Put(Lookup, valTypeInt, Idx, IntToStr(Value));
end;

procedure TTSCustomINI.PutPropFloat(Lookup: string; const Value: Extended);
begin
  Put(Lookup, valTypeFloat, INIFloatStr(Value));
end;

procedure TTSCustomINI.PutPropBool(Lookup: string; const Value: Boolean);
begin
	Put(Lookup, valTypeBool, IIf(Value, boolYes, boolNo));
end;

procedure TTSCustomINI.PutPropBoolIdx(Lookup: string; Idx: integer; const Value: Boolean);
begin
	Put(Lookup, valTypeBool, Idx, IIf(Value, boolYes, boolNo));
end;

procedure TTSCustomINI.PutPropColor(Lookup: string; const Value: TColor);
begin
  Put(Lookup, valTypeColor, ColorToString(Value));
end;

procedure TTSCustomINI.PutPropColorIdx(Lookup: string; Idx: integer; Value: TColor);
begin
	Put(Lookup, valTypeColor, Idx, ColorToString(Value));
end;

procedure TTSCustomINI.PutPropTime(Lookup: string; AValue: TDateTime);
begin
  Put(Lookup, valTypeTime, FormatDateTime(fmtINITime, AValue));
end;


procedure TTSCustomINI.PutPropDateTime(Lookup: string; const Value: TDateTime);
begin
  Put(Lookup, valTypeTimeStamp, FormatDateTime(fmtINITimeStamp, Value));
end;

procedure TTSCustomINI.PutPropPoint(Lookup: string; const Value: TPoint);
begin
  Put(Lookup, valTypeColor, Pt2Str(Value));
end;

procedure TTSCustomINI.PutPropRect(Lookup: string; const Value: TRect);
begin
  Put(Lookup, valTypeColor, Rect2Str(Value));
end;

function TTSCustomINI.GetPropDescription(Lookup: string): string;
var
  Group, Entry, DefaultDummy: string;
  Index: Integer;
  EncryptedDummy: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, EncryptedDummy, DefaultDummy);
  Result := GetDescription(Group, Entry, Index);
end;

procedure TTSCustomINI.PutPropDescription(Lookup: string; const Value: string);
var
  Group, Entry, DefaultDummy: string;
  Index: Integer;
  EncryptedDummy: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, EncryptedDummy, DefaultDummy);
  PutDescription(Group, Entry, Index, Value);
end;

//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetStr(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: string): string;
begin
	Result := Get(Group, Entry, Index, Encrypted, valTypeString, Default);
end;

function TTSCustomINI.GetStr(const Lookup, Default: string): string;
begin
	Result := Get(Lookup, valTypeString, Default);
end;

procedure TTSCustomINI.PutStr(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: string);
begin
	Put(Group, Entry, Index, Encrypted, valTypeString, Value);
end;

//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetInt(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: Integer): Integer;
begin
	Result := StrToIntDef(Get(Group, Entry, Index, Encrypted, valTypeInt, IntToStr(Default)), Default);
end;

function TTSCustomINI.GetInt(const Lookup: string; Default: Integer): Integer;
begin
	Result := StrToIntDef(Get(Lookup, valTypeInt, IntToStr(Default)), Default);
end;

procedure TTSCustomINI.PutInt(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: Integer);
begin
	Put(Group, Entry, Index, Encrypted, valTypeInt, IntToStr(Value));
end;

//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetFloat(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: Extended): Extended;
begin
  Result := INIStrFloatDef(Get(Group, Entry, Index, Encrypted, valTypeFloat, INIFloatStr(Default)), Default);
end;

function TTSCustomINI.GetFloat(const Lookup: string; Default: Extended): Extended;
begin
  Result := INIStrFloatDef(Get(Lookup, valTypeFloat, INIFloatStr(Default)), Default);
end;

procedure TTSCustomINI.PutFloat(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: Extended);
begin
  Put(Group, Entry, Index, Encrypted, valTypeFloat, INIFloatStr(Value));
end;

//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetBool(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Default: Boolean): Boolean;
begin
	Result := StrToBool(Trim(Get(Group, Entry, Index, Encrypted, valTypeBool, IIf(Default, boolYes, boolNo))), Default);
end;

function TTSCustomINI.GetBool(const Lookup: string; Default: Boolean): Boolean;
begin
	Result := StrToBool(Trim(Get(Lookup, valTypeBool, IIf(Default, boolYes, boolNo))), Default);
end;

procedure TTSCustomINI.PutBool(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Value: Boolean);
begin
	Put(Group, Entry, Index, Encrypted, valTypeBool, IIf(Value, boolYes, boolNo));
end;

//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetColor(const Group, Entry: string; Index: Integer;
  Encrypted: Boolean; Default: TColor): TColor;
begin
  try
    Result := Str2Clr(Get(Group, Entry, Index, Encrypted, valTypeColor, ColorToString(Default)));
  except
    Result := Default;
  end;
end;

function TTSCustomINI.GetColor(const Lookup: string; Default: TColor): TColor;
begin
  try
    Result := Str2Clr(Get(Lookup, valTypeColor, ColorToString(Default)));
  except
    Result := Default;
  end;
end;

procedure TTSCustomINI.PutColor(const Group, Entry: string; Index: Integer;
                                              Encrypted: Boolean; Value: TColor);
begin
  Put(Group, Entry, Index, Encrypted, valTypeColor, ColorToString(Value));
end;

//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetTimeStamp(const Group, Entry: string; Index: Integer;
                              Encrypted: Boolean; Default: TDateTime): TDateTime;
begin
  try
    Result := Str2TS(Get(Group, Entry, Index, Encrypted, valTypeTimeStamp,
                              FormatDateTime(fmtINITimeStamp, Default)));
  except
    Result := Default;
  end;
end;

function TTSCustomINI.GetTimeStamp(const Lookup: string; Default: TDateTime): TDateTime;
begin
  try
    Result := Str2TS(Get(Lookup, valTypeTimeStamp,
                              FormatDateTime(fmtINITimeStamp, Default)));
  except
    Result := Default;
  end;
end;

procedure TTSCustomINI.PutTimeStamp(const Group, Entry: string; Index: Integer;
                                          Encrypted: Boolean; Value: TDateTime);
begin
  Put(Group, Entry, Index, Encrypted, valTypeTimeStamp, FormatDateTime(fmtINITimeStamp, Value));
end;

//***************************************************************************************
//***************************************************************************************
function TTSCustomINI.GetTime(const Group, Entry: string; Index: Integer;
                                    Encrypted: Boolean; Default: TDateTime): TDateTime;
begin
  try
    Result := EvalTime(Get(Group, Entry, Index, Encrypted, valTypeTime, FormatDateTime(fmtINITime, Default)));
  except
    Result := Default;
  end;
end;

function TTSCustomINI.GetTime(const Lookup: string; Default: TDateTime): TDateTime;
begin
  try
    Result := EvalTime(Get(Lookup, valTypeTime, FormatDateTime(fmtINITime, Default)));
  except
    Result := Default;
  end;
end;

procedure TTSCustomINI.PutTime(const Group, Entry: string; Index: Integer;
                                          Encrypted: Boolean; Value: TDateTime);
begin
  Put(Group, Entry, Index, Encrypted, valTypeTime, FormatDateTime(fmtINITime, Value));
end;

//***************************************************************************************
//***************************************************************************************
procedure TTSCustomINI.GetStrings(const Group, Entry: string; Index: Integer; Encrypted: Boolean; List: TStrings);
var
  E: string;
  I, N: Integer;
begin
  List.Clear;
  if Index <> 0 then
    E := Entry + '[' + IntToStr(Index)  + ']'
  else
    E := Entry;
  N := StrToIntDef(Get(Group, E, 0, Encrypted, valTypeListHdr, '0'), 0);
  for I := 1 to N do
    List.Add(Get(Group, E, I, Encrypted, valTypeListItem, ''));
end;

procedure TTSCustomINI.GetStrings(const Lookup: string; List: TStrings);
var
  Group, Entry, LUDefault: string;
  Index: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
  GetStrings(Group, Entry, Index, Encrypted, List);
end;

procedure TTSCustomINI.PutStrings(const Group, Entry: string; Index: Integer;
                                            Encrypted: Boolean; List: TStrings);
var
  E: string;
  I: Integer;
begin
  if Index <> 0 then
    E := Entry + '[' + IntToStr(Index)  + ']'
  else
    E := Entry;
  Put(Group, E, 0, Encrypted, valTypeListHdr, IntToStr(List.Count));
  for I := 0 to pred(List.Count) do
    Put(Group, E, I+1, Encrypted, valTypeListItem, List[I]);
end;

procedure TTSCustomINI.PutStrings(const Lookup: string; List: TStrings);
var
  Group, Entry, LUDefault: string;
  Index: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
  PutStrings(Group, Entry, Index, Encrypted, List);
end;


//***************************************************************************************
//***************************************************************************************
procedure TTSCustomINI.GetStrArray(const Group, Entry: string;
														Index: Integer; Encrypted: Boolean; out StrArr: ADynStr);
var
	E: string;
	I, N: Integer;
begin
	if Index <> 0 then
		E := Entry + '[' + IntToStr(Index)  + ']'
	else
		E := Entry;
	N := StrToIntDef(Get(Group, E, 0, Encrypted, valTypeListHdr, '0'), 0);
	SetLength(StrArr, N);
	for I := 1 to N do
		StrArr[pred(I)] := Get(Group, E, I, Encrypted, valTypeListItem, '');
end;

procedure TTSCustomINI.GetStrArray(const Lookup: string; out StrArr: ADynStr);
var
	Group, Entry, LUDefault: string;
	Index: Integer;
	Encrypted: Boolean;
begin
	ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
	GetStrArray(Group, Entry, Index, Encrypted, StrArr);
end;

procedure TTSCustomINI.PutStrArray(const Group, Entry: string;
													Index: Integer; Encrypted: Boolean; StrArr: ADynStr);
var
	E: string;
	I: Integer;
begin
	if Index <> 0 then
		E := Entry + '[' + IntToStr(Index)  + ']'
	else
		E := Entry;
	Put(Group, E, 0, Encrypted, valTypeListHdr, IntToStr(Length(StrArr)));
	for I := 0 to High(StrArr) do
		Put(Group, E, I+1, Encrypted, valTypeListItem, StrArr[I]);
end;

procedure TTSCustomINI.PutStrArray(const Lookup: string; StrArr: ADynStr);
var
	Group, Entry, LUDefault: string;
	Index: Integer;
	Encrypted: Boolean;
begin
	ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
	PutStrArray(Group, Entry, Index, Encrypted, StrArr);
end;

procedure TTSCustomINI.LoadCtrlBounds(Ctrl: TControl);
var
	LkUp, Rct: string;
	R: TRect;
begin
	if not Assigned(Ctrl) then
		exit;
	Rct := LookupSeparators.Default + Rect2Str(Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height);
	if Assigned(Ctrl.Owner) and (Ctrl.Owner.Name>'') then
		LkUp := Ctrl.Owner.Name + LookupSeparators.Group + Ctrl.Name + Rct
	else
		LkUp := 'nil' + LookupSeparators.Group + Ctrl.Name + Rct;
	R := Rect[LkUp];
	Ctrl.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
end;

procedure TTSCustomINI.SaveCtrlBounds(Ctrl: TControl);
var
	LkUp: string;
	R: TRect;
begin
	if not Assigned(Ctrl) then
	 	exit;

	if Assigned(Ctrl.Owner) and (Ctrl.Owner.Name>'') then
		LkUp := Ctrl.Owner.Name + LookupSeparators.Group + Ctrl.Name
	else
		LkUp := 'nil' + LookupSeparators.Group + Ctrl.Name;
	Rect[LkUp] := Classes.Rect(Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height);
end;

//***************************************************************************************
//***************************************************************************************

procedure TTSCustomINI.GetFont(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Font: TFont);
begin
  Str2Font(Get(Group, Entry, Index, Encrypted, valTypeFont, Font2Str(Font)), Font);
end;

procedure TTSCustomINI.GetFont(const Lookup: string; Font: TFont);
var
  Group, Entry, LUDefault: string;
  Index: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
  GetFont(Group, Entry, Index, Encrypted, Font);
end;

procedure TTSCustomINI.PutFont(const Group, Entry: string; Index: Integer; Encrypted: Boolean; Font: TFont);
begin
  Put(Group, Entry, Index, Encrypted, valTypeListItem, Font2Str(Font));
end;

procedure TTSCustomINI.PutFont(const Lookup: string; Font: TFont);
var
  Group, Entry, LUDefault: string;
  Index: Integer;
  Encrypted: Boolean;
begin
  ParseLookup(Lookup, Group, Entry, Index, Encrypted, LUDefault);
  PutFont(Group, Entry, Index, Encrypted, Font);
end;


//***************************************************************************************
//***************************************************************************************
{ EINIError }
//***************************************************************************************
//***************************************************************************************

constructor EINIError.Create(INIComp: TTSCustomINI; const Msg: string);
begin
	inherited Create(INIComp.Name + ': ' + Msg);
end;

constructor EINIError.Create(INIComp: TTSCustomINI; const Group, Entry: string; Index: Integer);
begin
	Create(INIComp, rsINIUnknownError, Group, Entry, Index)
end;

constructor EINIError.Create(INIComp: TTSCustomINI; const Msg, Group, Entry: string; Index: Integer);
begin
	inherited Create(INIComp.Name + '.' + Group + '.' + Entry + '[' + IntToStr(Index) + ']: ' + Msg);
end;

//***************************************************************************************
//***************************************************************************************
{ EINIValueMissing }
//***************************************************************************************
//***************************************************************************************

constructor EINIValueMissing.Create(INIComp: TTSCustomINI; const Group, Entry: string; Index: Integer);
begin
	inherited Create(INIComp, rsINIValueMissing, Group, Entry, Index);
end;


end.

