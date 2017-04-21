unit TSLibConvert;

interface

uses
{$IFDEF WINDOWS}
	Windows,
{$ENDIF}
  StdCtrls,
	Forms,
	Controls,
	Classes;

const
	TSNumNULL: Extended = 1e-323; //Muss größer sein als kleinste Double, da Prop-Editor mit Double arbeitet.
  TSNumNULLName = '<NULL>';

type

	//***************************************************************************************
  // Aufzählungs- und Set-Typen
	NBoolStr = (bs10, bsYN, bsTF, bsJN, bsWF);
	NTriState = (tsFalse, tsTrue, tsNULL);

  //Zulässige Ergebnisse für "NumRange"- and "DateRange"-Funktionen
	NRangeType = (rgInvalid, rgSingleValue, rgToValue, rgFromValue, rgFromToValue);
	NLanguageConvert = (lngUS, lngDEU, lngDK);

  //Optionen für den Aufruf der Funktion "CalculateExpressionValue"
  NCalcExpressionOption = (ceRaiseOnError, ceThousandSeparator);
  SCalcExpressionOptions = set of NCalcExpressionOption;
  NCalcFunction = (calcProduct, calcQuotient, calcExponent);


	//***************************************************************************************
  // Array- und Record-Typen
  ATriStateStr = array [NTriState] of string;
	ALanguageConvert = array [0..37] of char;
	RCodePage = record
		ID: Integer;
		Name: string;
	end;
	RHTMLConvert = record
		ANSI: char;
		HTMLName: string;
		Unicode: LongWord;
	end;

const
	CodePageNames: array [0..139] of RCodePage = ( (ID:37; Name:'IBM037'),
			(ID:437; Name:'IBM437';), (ID:500; Name:'IBM500';), (ID:708; Name:'ASMO-708';),
			(ID:720; Name:'DOS-720';), (ID:737; Name:'ibm737';), (ID:775; Name:'ibm775';),
			(ID:850; Name:'ibm850';), (ID:852; Name:'ibm852';), (ID:855; Name:'IBM855';),
			(ID:857; Name:'ibm857';), (ID:858; Name:'IBM00858';), (ID:860; Name:'IBM860';),
			(ID:861; Name:'ibm861';), (ID:862; Name:'DOS-862';), (ID:863; Name:'IBM863';),
			(ID:864; Name:'IBM864';), (ID:865; Name:'IBM865';), (ID:866; Name:'cp866';),
			(ID:869; Name:'ibm869';), (ID:870; Name:'IBM870';), (ID:874; Name:'windows-874';),
			(ID:875; Name:'cp875';), (ID:932; Name:'shift_jis';), (ID:936; Name:'gb2312';),
			(ID:949; Name:'ks_c_5601-1987';), (ID:950; Name:'big5';), (ID:1026; Name:'IBM1026';),
			(ID:1047; Name:'IBM01047';), (ID:1140; Name:'IBM01140';), (ID:1141; Name:'IBM01141';),
			(ID:1142; Name:'IBM01142';), (ID:1143; Name:'IBM01143';), (ID:1144; Name:'IBM01144';),
			(ID:1145; Name:'IBM01145';), (ID:1146; Name:'IBM01146';), (ID:1147; Name:'IBM01147';),
			(ID:1148; Name:'IBM01148';), (ID:1149; Name:'IBM01149';), (ID:1200; Name:'utf-16';),
			(ID:1201; Name:'unicodeFFFE';), (ID:1250; Name:'windows-1250';), (ID:1251; Name:'windows-1251';),
			(ID:1252; Name:'windows-1252';), (ID:1253; Name:'windows-1253';), (ID:1254; Name:'windows-1254';),
			(ID:1255; Name:'windows-1255';), (ID:1256; Name:'windows-1256';), (ID:1257; Name:'windows-1257';),
			(ID:1258; Name:'windows-1258';), (ID:1361; Name:'Johab';), (ID:10000; Name:'macintosh';),
			(ID:10001; Name:'x-mac-japanese';), (ID:10002; Name:'x-mac-chinesetrad';), (ID:10003; Name:'x-mac-korean';),
			(ID:10004; Name:'x-mac-arabic';), (ID:10005; Name:'x-mac-hebrew';), (ID:10006; Name:'x-mac-greek';),
			(ID:10007; Name:'x-mac-cyrillic';), (ID:10008; Name:'x-mac-chinesesimp';), (ID:10010; Name:'x-mac-romanian';),
			(ID:10017; Name:'x-mac-ukrainian';), (ID:10021; Name:'x-mac-thai';), (ID:10029; Name:'x-mac-ce';),
			(ID:10079; Name:'x-mac-icelandic';), (ID:10081; Name:'x-mac-turkish';), (ID:10082; Name:'x-mac-croatian';),
			(ID:12000; Name:'utf-32';), (ID:12001; Name:'utf-32BE';), (ID:20000; Name:'x-Chinese_CNS';),
			(ID:20001; Name:'x-cp20001';), (ID:20002; Name:'x_Chinese-Eten';), (ID:20003; Name:'x-cp20003';),
			(ID:20004; Name:'x-cp20004';), (ID:20005; Name:'x-cp20005';), (ID:20105; Name:'x-IA5';),
			(ID:20106; Name:'x-IA5-German';), (ID:20107; Name:'x-IA5-Swedish';), (ID:20108; Name:'x-IA5-Norwegian';),
			(ID:20127; Name:'us-ascii';), (ID:20261; Name:'x-cp20261';), (ID:20269; Name:'x-cp20269';),
			(ID:20273; Name:'IBM273';), (ID:20277; Name:'IBM277';), (ID:20278; Name:'IBM278';),
			(ID:20280; Name:'IBM280';), (ID:20284; Name:'IBM284';), (ID:20285; Name:'IBM285';),
			(ID:20290; Name:'IBM290';), (ID:20297; Name:'IBM297';), (ID:20420; Name:'IBM420';),
			(ID:20423; Name:'IBM423';), (ID:20424; Name:'IBM424';), (ID:20833; Name:'x-EBCDIC-KoreanExtended';),
			(ID:20838; Name:'IBM-Thai';), (ID:20866; Name:'koi8-r';), (ID:20871; Name:'IBM871';),
			(ID:20880; Name:'IBM880';), (ID:20905; Name:'IBM905';), (ID:20924; Name:'IBM00924';),
			(ID:20932; Name:'EUC-JP';), (ID:20936; Name:'x-cp20936';), (ID:20949; Name:'x-cp20949';),
			(ID:21025; Name:'cp1025';), (ID:21866; Name:'koi8-u';), (ID:28591; Name:'iso-8859-1';),
			(ID:28592; Name:'iso-8859-2';), (ID:28593; Name:'iso-8859-3';), (ID:28594; Name:'iso-8859-4';),
			(ID:28595; Name:'iso-8859-5';), (ID:28596; Name:'iso-8859-6';), (ID:28597; Name:'iso-8859-7';),
			(ID:28598; Name:'iso-8859-8';), (ID:28599; Name:'iso-8859-9';), (ID:28603; Name:'iso-8859-13';),
			(ID:28605; Name:'iso-8859-15';), (ID:29001; Name:'x-Europa';), (ID:38598; Name:'iso-8859-8-i';),
			(ID:50220; Name:'iso-2022-jp';), (ID:50221; Name:'csISO2022JP';), (ID:50222; Name:'iso-2022-jp';),
			(ID:50225; Name:'iso-2022-kr';), (ID:50227; Name:'x-cp50227';), (ID:51932; Name:'euc-jp';),
			(ID:51936; Name:'EUC-CN';), (ID:51949; Name:'euc-kr';), (ID:52936; Name:'hz-gb-2312';),
			(ID:54936; Name:'GB18030';), (ID:57002; Name:'x-iscii-de';), (ID:57003; Name:'x-iscii-be';),
			(ID:57004; Name:'x-iscii-ta';), (ID:57005; Name:'x-iscii-te';), (ID:57006; Name:'x-iscii-as';),
			(ID:57007; Name:'x-iscii-or';), (ID:57008; Name:'x-iscii-ka';), (ID:57009; Name:'x-iscii-ma';),
			(ID:57010; Name:'x-iscii-gu';), (ID:57011; Name:'x-iscii-pa';), (ID:65000; Name:'utf-7';),
			(ID:65001; Name:'utf-8';)
			);

	HTMLCharConvert: array [0..103] of RHTMLConvert = (
//			(ANSI: #1; HTMLName: '&x;'; Unicode: 1), (ANSI: #1; HTMLName: '&x;'; Unicode: 1),
			(ANSI: #47; HTMLName: '&frasl;'; Unicode: 47), (ANSI: #133; HTMLName: '&hellip;'; Unicode: 133),
			(ANSI: #150; HTMLName: '&ndash;'; Unicode: 150), (ANSI: #151; HTMLName: '&mdash;'; Unicode: 151),
			(ANSI: #34; HTMLName: '&quot;'; Unicode: 34), (ANSI: #38; HTMLName: '&amp;'; Unicode: 38),
			(ANSI: #60; HTMLName: '&lt;'; Unicode: 60), (ANSI: #62; HTMLName: '&gt;'; Unicode: 62),
			(ANSI: #160; HTMLName: '&nbsp;'; Unicode: 160), (ANSI: #161; HTMLName: '&iexcl;'; Unicode: 161),
			(ANSI: #162; HTMLName: '&cent;'; Unicode: 162), (ANSI: #163; HTMLName: '&pound;'; Unicode: 163),
			(ANSI: #164; HTMLName: '&curren;'; Unicode: 164), (ANSI: #165; HTMLName: '&yen;'; Unicode: 165),
			(ANSI: #166; HTMLName: '&brvbar;'; Unicode: 166), (ANSI: #167; HTMLName: '&sect;'; Unicode: 167),
			(ANSI: #168; HTMLName: '&uml;'; Unicode: 168), (ANSI: #169; HTMLName: '&copy;'; Unicode: 169),
			(ANSI: #170; HTMLName: '&ordf;'; Unicode: 170), (ANSI: #171; HTMLName: '&laquo;'; Unicode: 171),
			(ANSI: #172; HTMLName: '&not;'; Unicode: 172), (ANSI: #173; HTMLName: '&shy;'; Unicode: 173),
			(ANSI: #174; HTMLName: '&reg;'; Unicode: 174), (ANSI: #175; HTMLName: '&macr;'; Unicode: 175),
			(ANSI: #176; HTMLName: '&deg;'; Unicode: 176), (ANSI: #177; HTMLName: '&plusmn;'; Unicode: 177),
			(ANSI: #178; HTMLName: '&sup2;'; Unicode: 178), (ANSI: #179; HTMLName: '&sup3;'; Unicode: 179),
			(ANSI: #180; HTMLName: '&acute;'; Unicode: 180), (ANSI: #181; HTMLName: '&micro;'; Unicode: 181),
			(ANSI: #182; HTMLName: '&para;'; Unicode: 182), (ANSI: #183; HTMLName: '&middot;'; Unicode: 183),
			(ANSI: #184; HTMLName: '&cedil;'; Unicode: 184), (ANSI: #185; HTMLName: '&sup1;'; Unicode: 185),
			(ANSI: #186; HTMLName: '&ordm;'; Unicode: 186), (ANSI: #187; HTMLName: '&raquo;'; Unicode: 187),
			(ANSI: #188; HTMLName: '&frac14;'; Unicode: 188), (ANSI: #189; HTMLName: '&frac12;'; Unicode: 189),
			(ANSI: #190; HTMLName: '&frac34;'; Unicode: 190), (ANSI: #191; HTMLName: '&iquest;'; Unicode: 191),
			(ANSI: #192; HTMLName: '&Agrave;'; Unicode: 192), (ANSI: #193; HTMLName: '&Aacute;'; Unicode: 193),
			(ANSI: #194; HTMLName: '&Acirc;'; Unicode: 194), (ANSI: #195; HTMLName: '&Atilde;'; Unicode: 195),
			(ANSI: #196; HTMLName: '&Auml;'; Unicode: 196), (ANSI: #197; HTMLName: '&Aring;'; Unicode: 197),
			(ANSI: #198; HTMLName: '&AElig;'; Unicode: 198), (ANSI: #199; HTMLName: '&Ccedil;'; Unicode: 199),
			(ANSI: #200; HTMLName: '&Egrave;'; Unicode: 200), (ANSI: #201; HTMLName: '&Eacute;'; Unicode: 201),
			(ANSI: #202; HTMLName: '&Ecirc;'; Unicode: 202), (ANSI: #203; HTMLName: '&Euml;'; Unicode: 203),
			(ANSI: #204; HTMLName: '&Igrave;'; Unicode: 204), (ANSI: #205; HTMLName: '&Iacute;'; Unicode: 205),
			(ANSI: #206; HTMLName: '&Icirc;'; Unicode: 206), (ANSI: #207; HTMLName: '&Iuml;'; Unicode: 207),
			(ANSI: #208; HTMLName: '&ETH;'; Unicode: 208), (ANSI: #209; HTMLName: '&Ntilde;'; Unicode: 209),
			(ANSI: #210; HTMLName: '&Ograve;'; Unicode: 210), (ANSI: #211; HTMLName: '&Oacute;'; Unicode: 211),
			(ANSI: #212; HTMLName: '&Ocirc;'; Unicode: 212), (ANSI: #213; HTMLName: '&Otilde;'; Unicode: 213),
			(ANSI: #214; HTMLName: '&Ouml;'; Unicode: 214), (ANSI: #215; HTMLName: '&times;'; Unicode: 215),
			(ANSI: #216; HTMLName: '&Oslash;'; Unicode: 216), (ANSI: #217; HTMLName: '&Ugrave;'; Unicode: 217),
			(ANSI: #218; HTMLName: '&Uacute;'; Unicode: 218), (ANSI: #219; HTMLName: '&Ucirc;'; Unicode: 219),
			(ANSI: #220; HTMLName: '&Uuml;'; Unicode: 220), (ANSI: #221; HTMLName: '&Yacute;'; Unicode: 221),
			(ANSI: #222; HTMLName: '&THORN;'; Unicode: 222), (ANSI: #223; HTMLName: '&szlig;'; Unicode: 223),
			(ANSI: #224; HTMLName: '&agrave;'; Unicode: 224), (ANSI: #225; HTMLName: '&aacute;'; Unicode: 225),
			(ANSI: #226; HTMLName: '&acirc;'; Unicode: 226), (ANSI: #227; HTMLName: '&atilde;'; Unicode: 227),
			(ANSI: #228; HTMLName: '&auml;'; Unicode: 228), (ANSI: #229; HTMLName: '&aring;'; Unicode: 229),
			(ANSI: #230; HTMLName: '&aelig;'; Unicode: 230), (ANSI: #231; HTMLName: '&ccedil;'; Unicode: 231),
			(ANSI: #232; HTMLName: '&egrave;'; Unicode: 232), (ANSI: #233; HTMLName: '&eacute;'; Unicode: 233),
			(ANSI: #234; HTMLName: '&ecirc;'; Unicode: 234), (ANSI: #235; HTMLName: '&euml;'; Unicode: 235),
			(ANSI: #236; HTMLName: '&igrave;'; Unicode: 236), (ANSI: #237; HTMLName: '&iacute;'; Unicode: 237),
			(ANSI: #238; HTMLName: '&icirc;'; Unicode: 238), (ANSI: #239; HTMLName: '&iuml;'; Unicode: 239),
			(ANSI: #240; HTMLName: '&eth;'; Unicode: 240), (ANSI: #241; HTMLName: '&ntilde;'; Unicode: 241),
			(ANSI: #242; HTMLName: '&ograve;'; Unicode: 242), (ANSI: #243; HTMLName: '&oacute;'; Unicode: 243),
			(ANSI: #244; HTMLName: '&ocirc;'; Unicode: 244), (ANSI: #245; HTMLName: '&otilde;'; Unicode: 245),
			(ANSI: #246; HTMLName: '&ouml;'; Unicode: 246), (ANSI: #247; HTMLName: '&divide;'; Unicode: 247),
			(ANSI: #248; HTMLName: '&oslash;'; Unicode: 248), (ANSI: #249; HTMLName: '&ugrave;'; Unicode: 249),
			(ANSI: #250; HTMLName: '&uacute;'; Unicode: 250), (ANSI: #251; HTMLName: '&ucirc;'; Unicode: 251),
			(ANSI: #252; HTMLName: '&uuml;'; Unicode: 252), (ANSI: #253; HTMLName: '&yacute;'; Unicode: 253),
			(ANSI: #254; HTMLName: '&thorn;'; Unicode: 254), (ANSI: #255; HTMLName: '&yuml;'; Unicode: 255)
		);

/// <summary>
///The TriState functions evaluate the parameter "Value" to one of the <see cref="NTriState"/> values.
/// </summary>
/// <remarks>
///	Depending on the input value type the following results are returned:
///	Boolean:
///		True -> tsTrue
///		False -> tsFalse
///	string: only the first string character is evaluated
///		y,Y,j,J,t,T,1 -> tsTrue
///		n,N,f,F,0 -> tsFalse
///		empty string or all other characters -> tsNULL
///	Integer:
///		0 -> tsFalse
///		1 -> tsTrue
///		all other values -> tsNULL
///	Extended:
///		0.0 -> tsFalse
///		1.0 -> tsTrue
///		all other values -> tsNULL
/// </remarks>
function TriState(Value: Boolean): NTriState; overload;
function TriState(Value: string): NTriState; overload;
function TriState(Value: string; Names: ATriStateStr): NTriState; overload;
function TriState(Value: Integer): NTriState; overload;
function TriState(Value: Extended): NTriState; overload;
function TriState(Value: TCheckBoxState): NTriState; overload;
function CheckBoxState(TriState: NTriState): TCheckBoxState; overload;
function CheckBoxState(Val: Integer): TCheckBoxState; overload;

//***************************************************************************************
//***************************************************************************************
//  Boolean - Umwandlungsfunktionen
//***************************************************************************************
//***************************************************************************************

//Wandelt einen String in einen Boolean-Wert um. Schlägt die Umwandlung fehl, wird
// Default zurückgegeben. Erkannte Werte sind t/f, w/f, 1/0, j/n, y/n
// jeweils als Wort oder einzelner Buchstabe
function StrToBool(const Val: string; Default: Boolean=False): Boolean;
//Wandelt einen booleschen Wert in einen String um. Default: 1/0. Durch TrueVal/FalseVal
// oder StrType/SingleChar können andere Rückgabewerte eingestellt werden.
function BoolToStr(Val: Boolean): string; overload;
function BoolToStr(Val: Boolean; const TrueVal, FalseVal: string): string; overload;
function BoolToStr(Val: Boolean; StrType: NBoolStr; SingleChar: Boolean=True): string; overload;

//***************************************************************************************
//***************************************************************************************
//  Integer - Umwandlungsfunktionen
//***************************************************************************************
//***************************************************************************************

function FmtInt(const Value: Integer): string; overload;
function FmtInt(const Value: Extended): string; overload;
function FmtInt(const Value, FixDigits: Integer): string; overload;
function FmtPrice(const Value: Double; ThsdSep: Boolean=True; USNumFmt: Boolean=False): string;
function ExtractNumString(const FindInString: string; var StartsAt, StrLen: Integer): string; overload;
function ExtractNumString(const FindInString: string; StartsAt: Integer=1): string; overload;
function Int2BCD(Value: Cardinal; MinDigits: Integer = 2;
										MaxDigits: Integer = 10; LSBFirst: Boolean = False): string;
function BCD2Int(Value: string; LSBFirst: Boolean = False): Cardinal;
function StrInt(S: string; Default: Integer): Integer; overload;
function StrInt(S: string): Integer; overload;

//***************************************************************************************
//***************************************************************************************
//  Float - Umwandlungsfunktionen
//***************************************************************************************
//***************************************************************************************

function TSRound(const Value: Extended; Digits: Integer): Extended; overload;
function TSRound(const Value: Extended): Integer; overload;
function SafeDivide(Dividend, Divisor: Extended; DefaultDivisor: Extended=1.0): Extended;
function FloatEqual(Num1, Num2: Extended; Precision: Integer=5): Boolean;
function FmtStrToFloat(S: string): Extended; overload;
function FmtStrToFloat(S: string; Default: Extended): Extended; overload;
function FmtStrToFloat(S: string; USNumFmt: Boolean): Extended; overload;
function FmtStrToFloat(S: string; Default: Extended; USNumFmt: Boolean): Extended; overload;
function NumIsTSNull(Number: Extended): Boolean;
function USNumStr(const Format: string; const Value: Extended): string;
function FloatToStrDecSep(Format: string; Number: Extended; DecSep: char): string;
function StrFloat(S: string; Precision: Integer=2): Extended; overload;
function StrFloat(S: string; Precision, Default: Integer): Extended; overload;
function SQLFloatStr(Value: Extended; Decimals: ShortInt=-1): string;
function INIFloatStr(Value: Extended): string;
function INIStrFloat(Value: string): Extended;
function INIStrFloatDef(Value: string; Default: Extended): Extended;
//Die CalculateExpressionValue führt eine "Taschenrechner"-Funktion aus: Expression wird
//		als Rechenformel ausgewertet und das Rechnergebnis zurückgegeben. Gleichzeitig wird
//		der Ausdruck "Expression" bei Bedarf so bereinigt, das er eine gültige Rechenformel enthält
function CalculateExpressionValue(var Expression: string; var EditPos: Integer;
                                      Options: SCalcExpressionOptions): Extended;



//***************************************************************************************
//***************************************************************************************
//  String - Umwandlungsfunktionen
//***************************************************************************************
//***************************************************************************************


//Wandelt HexStr einen String aus entsprechenden Ansi-Zeichen um. Jeweils 2 Zeichen bilden den
// ANSI-Code eines Zeichens.
function HexToString(HexStr: string): string;

//Wandelt einen String in seine HTML-Entsprechung um.
//Alle im array HTMLCharConvert definierten HTML-Sonderzeichen können verwendet wreden.
//Ist ein Sonderzeichen nicht in HTMLCharConvert definiert (oder ist "PreferUnicodeIDs"=True)
//	wird das Zeichen statt dessen in seine HEX-Entsprechung &#<hex-code>; umgewandelt.
function HTMLEncode(const TxtStr: string; PreferUnicodeIDs: Boolean=False): string;

//Wandelt einen HTML-String in einen "normalen" string um. Alle erkannten HTML-Sonderzeichen
// sind im array HTMLCharConvert definiert.
function HTMLDecode(const HTMLStr: string): string;

//Prüft, ob TxtStr Zeichen enthält, die in URL unzulässig sind. Wenn nötig werden diese durch
// %xx (Hex-Wert) ersetzt.
function URLEncode(const TxtStr: string): string;

//Wandelt einen URL-Codierten String (mit %xx für div. Zeichen) in den entsprechenden "lesbaren" string um.
function URLDecode(URLStr: string): string;

//Die Funktion führt mehrere Ersetzungen im Block aus: Im Text "Str" werden alle Vorkommen
//	eines jeden String in "FromSub" in das entsprechende Gegenstück im "ToSub" umgewandelt.
function ConvertStr(Str: string; FromSub, ToSub: array of string): string;

//Wenn S = '', wird "IfEmpty" zurückgegeben. Andernfalls wird S zurückgegeben.
// Wenn Quoted=True, wird S in passende Anführungszeichen gesetzt.
function StrIfEmpty(S: string; IfEmpty: string = 'NULL'; Quoted: Boolean = True): string;

//Decodiert einen Definition von Tastenkombinationen.
// Tastenkombinationen mit Strg+Buchstabe sind in CodeStr als ^<Buchstabe> eingetragen.
//Jedes Zeichen kann mit #NNN als ANSI-Dezimalzeichen angegben werden
function EvalKeyStr(const CodeStr: string): string;

//Wandelt nationale Sonderzeichen entsprechend ihrer Position auf der Tastatur von einer
//Tastaturbelegung in die andere. Unterstützt z.Zt. nur DEU,DAN,ENG
//Für weitere Umwandlungen muss NLanguageConvert und die Umwandlungsliste
//im Kopf Funktion KeybConv erweitert werden.
function KeybConv(Str: string; FromLang, ToLang: NLanguageConvert): string;


//***************************************************************************************
//***************************************************************************************
//  Datums und Zeit - Umwandlungsfunktionen
//***************************************************************************************
//***************************************************************************************

//ACHTUNG: Alle Funktionen und Prozeduren, die für die Bearbeitung und Umwandlung
// von Datums- und Zeitfunktionen verendet werden sind ausgelagert in die Datei
// "TSDateLib.pas".
// Dies schließt auch die "Range_xx"- und "RangeFilter_xx"-Funktionen ein.

//***************************************************************************************
//***************************************************************************************
//  Range- und RangeFilter
//***************************************************************************************
//***************************************************************************************


// <summary>
/// The RangeFilter-Funtions evaluate "RangeStr" to build a string that can
///	be used in the where-clause of an SQL statement or a TDataSet Filter property.
// </summary>
/// <remarks>
///  The RangeFilter-Funtions call the corresponding "xxxRange"-Function to find a range
///  definition and then build a string in one of the following formats:
///  	Value          -> "=Value"
///    Value..        -> ">=Value"
///    ..Value        -> "<=Value"
///    Value1..Value2 -> "between Value1 and Value2"
/// </remarks>
function RangeFilter(const FieldName: string; const RangeType: NRangeType; const Val1, Val2: string): string;
function RangeDisplay(RangeType: NRangeType; const LowBound, HighBound: string): string;

function NumRange(const RangeStr: string; out FromNum, ToNum: Integer): NRangeType; overload;
function NumRange(const RangeStr: string; out FromNum, ToNum: Extended): NRangeType; overload;
function NumRange(const RangeStr: string; out FromNum, ToNum: string; Format: string): NRangeType; overload;
function NumRange(const RangeStr: string; Format: string): string; overload;

function IntRange(const RangeStr: string; out FromNum, ToNum: Integer): NRangeType; overload;
function IntRange(RangeStr: string; Format: string=''): string; overload;
function IntRange(RangeStr: string; out FromNum, ToNum: string; Format: string=''): NRangeType; overload;
function RangeFilterInt(const FieldName, RangeStr: string): string;

function FloatRange(const RangeStr: string; out FromNum, ToNum: Extended): NRangeType; overload;
function FloatRange(RangeStr: string; Format: string=''): string; overload;
function FloatRange(RangeStr: string; out FromNum, ToNum: string; IntlDSep: Boolean=True; Format: string=''): NRangeType; overload;
function RangeFilterFloat(const FieldName, RangeStr: string): string;

function StringRange(RangeStr: string; out RangeStart, RangeEnd: string): NRangeType; overload;
function StringRange(RangeStr: string): string; overload;
function RangeFilterStr(const FieldName, RangeStr: string): string;



//***************************************************************************************
//***************************************************************************************
//  Diverse Umwandlungsfunktionen
//***************************************************************************************
//***************************************************************************************

//
function ResizeProportional(FromWidHgt, ToWidHgt: TPoint): TPoint;
function Distance(Pt1, Pt2: TPoint; Approx: Boolean = False): Integer;

//Auswertung von Varianten
function VarEquals(const V1, V2: Variant): Boolean;
function VarToIntDef(const VarValue: Variant; Default: Integer=0): Integer;
function VarNullIf(const I: Integer; const NullVal: Integer = 0): Variant;

//Functions to convert Keyboard Scan Codes to Characters or Virtual Key Codes and vice versa
{$IFDEF WINDOWS}
function KeybScanCode(Chr: char): Word; overload;
function KeybScanCode(VirtKeyCode: Word): Word; overload;
function USKey(Chr: char): string;
function USStr(Str: string): string;
function ScanCode2Char(ScanCode: Word): char;
function ScanCode2VirtKey(ScanCode: Word): Word;
function VirtKey2Char(VirtKey: Word): char;
{$ENDIF}




function CodePageID(CodePageName: string): Integer;
function CodePageName(CodePageID: Integer): string;


const
	strTimeStampFormat = 'yyyymmddhhnnsszzz';

var
  RangeSeparator: string = '..';
  TodayToken: string = 'x';
	StrBoolYes: string = 'ja';
	StrBoolNo: string = 'nein';
  BoolCharsTrue: string = '1YyJjSsTtWwDd';
  BoolCharsFalse: string = '0NnFf';
  INIExponentSign: char = 'E';

implementation

uses
	SysUtils,
	Variants,
	StrUtils,
  Math,
	TSLib;



//---------------------------------------------------------------------------------------
{: Converts a Boolean value to a value of type NTriState = (tsTrue, tsFalse, tsNULL).}
function TriState(Value: Boolean): NTriState; overload;
begin
	if Value then
		Result := tsTrue
	else
		Result := tsFalse;
end;

function TriState(Value: string; Names: ATriStateStr): NTriState; overload;
var
  N: NTriState;
begin
  if Value = '' then
    Result := tsNULL
  else begin
    for N := Low(NTriState) to High(NTriState) do
      if StartsText(Value, Names[N]) then
      begin
        Result := N;
        Exit;
      end;
    Result := TriState(Value);
  end;
end;

//---------------------------------------------------------------------------------------
{: Converts a string value to a value of type NTriState:
	y/t/j/w/1 => tsTrue,<br>
	n/f/0 => tsFalse,<br>
	all others => tsNULL).<br>}
function TriState(Value: string): NTriState; overload;
begin
	if Value = '' then
		Result := tsNULL
	else if Pos(UpperCase(Value[1]), 'YTJW1') > 0 then
		Result := tsTrue
	else if Pos(UpperCase(Value[1]), 'NF0') > 0 then
		Result := tsFalse
	else
		Result := tsNULL;
end;

//---------------------------------------------------------------------------------------
function TriState(Value: Integer): NTriState; overload;
{: Converts an integer value to a value of type NTriState:
	1 => tsTrue,<br>
	0 => tsFalse,<br>
	all others => tsNULL).<br>}
begin
	case Value of
	0: Result := tsFalse;
	1: Result := tsTrue;
	else
		Result := tsNULL;
	end;
end;

//---------------------------------------------------------------------------------------
{: Converts an extended float value to a value of type NTriState:
	1.0 => tsTrue,<br>
	0.0 => tsFalse,<br>
	all others => tsNULL).<br>}
function TriState(Value: Extended): NTriState; overload;
begin
	if Value = 0 then
		Result := tsFalse
	else if Value = 1 then
		Result := tsTrue
	else
		Result := tsNULL;
end;

function TriState(Value: TCheckBoxState): NTriState;
begin
  case Value of
		cbChecked: Result := tsTrue;
		cbUnchecked: Result := tsFalse;
		cbGrayed: Result := tsNULL;
	end;
end;

//---------------------------------------------------------------------------------------
function CheckBoxState(TriState: NTriState): TCheckBoxState;
begin
	case TriState of
		tsFalse: Result := cbUnchecked;
		tsTrue: Result := cbChecked;
    else Result := cbGrayed; //tsNULL:
	end;
end;

//---------------------------------------------------------------------------------------
function CheckBoxState(Val: Integer): TCheckBoxState; overload;
begin
	case Val of
		0: Result := cbUnchecked;
		1: Result := cbChecked;
	else
		Result := cbGrayed;
	end;
end;

//---------------------------------------------------------------------------------------
{: Converts a string to a boolean value:
	y/t/j/w/1 => True,<br>
	n/f/0 => False,<br>
	all others => default value .<br>
}
function StrToBool(const Val: string; Default: Boolean=False): Boolean;
begin
	case TriState(Val) of
	tsFalse:
		Result := False;
	tsTrue:
		Result := True;
	else
		Result := Default;
	end;
end;

//---------------------------------------------------------------------------------------
{: Converts a boolean into a string: True -> '1', False -> '0'}
function BoolToStr(Val: Boolean): string; overload;
begin
	Result := IIf(Val, '1', '0');
end;

//---------------------------------------------------------------------------------------
{: Converts a boolean into a string: True -> TrueVal, False -> FalseVal}
function BoolToStr(Val: Boolean; const TrueVal, FalseVal: string): string; overload;
begin
	Result := IIf(Val, TrueVal, FalseVal);
end;

//---------------------------------------------------------------------------------------
{: Converts a boolean into one of the available standard strings defined by <br>
		NBoolStr = (bs10, bsYN, bsTF, bsJN, bsWF) <br>
		Results for True / False: '1'/'0', 'Yes'/'No', 'True'/'False', 'Ja'/'Nein', 'Wahr'/'Falsch'
		if SingleChar = TRUE, only the first character of the result word is returned.
}
function BoolToStr(Val: Boolean; StrType: NBoolStr; SingleChar: Boolean=True): string; overload;
begin
	if SingleChar then
		case StrType of
		bs10: Result := IIf(Val, '1', '0');
		bsYN: Result := IIf(Val, 'Y', 'N');
		bsTF: Result := IIf(Val, 'T', 'F');
		bsJN: Result := IIf(Val, 'J', 'N');
		bsWF: Result := IIf(Val, 'W', 'F');
		end
	else
		case StrType of
		bs10: Result := IIf(Val, '1', '0');
		bsYN: Result := IIf(Val, 'Yes', 'No');
		bsTF: Result := IIf(Val, 'True', 'False');
		bsJN: Result := IIf(Val, 'Ja', 'Nein');
		bsWF: Result := IIf(Val, 'Wahr', 'Falsch');
		end;
end;

//---------------------------------------------------------------------------------------
{: Quick formatting for integers with thousands-separator
}
function FmtInt(const Value: Integer): string;
begin
	Result := FormatFloat(',0', Value);
end;

//---------------------------------------------------------------------------------------
{: Quick formatting for integers with thousands-separator (overloaded for floats as input)
}
function FmtInt(const Value: Extended): string; overload;
begin
	Result := FormatFloat(',0', Value);
end;

function FmtInt(const Value, FixDigits: Integer): string; overload;
begin
	Result := FormatFloat(',0', Value);
	while Length(Result) < FixDigits do
		Result := '0' + Result;
end;

//---------------------------------------------------------------------------------------
{: Quick formatting for float value as "price" with 2 decimals. optionally the thousands
	separator can be suppressed (ThsdSep=False).
}
function FmtPrice(const Value: Double; ThsdSep: Boolean=True; USNumFmt: Boolean=False): string;
var
	DSep, TSep: char;
begin
	if USNumFmt then begin
		TSep := FormatSettings.ThousandSeparator;
		DSep := FormatSettings.DecimalSeparator;
		FormatSettings.ThousandSeparator := ',';
		FormatSettings.DecimalSeparator := '.';
		try
			if ThsdSep then
				Result := FormatFloat(',0.00', Value)
			else
				Result := FormatFloat('0.00', Value);
		finally
			FormatSettings.ThousandSeparator := TSep;
			FormatSettings.DecimalSeparator := DSep;
		end;
	end
	else begin
		if ThsdSep then
			Result := FormatFloat(',0.00', Value)
		else
			Result := FormatFloat('0.00', Value);
	end;
end;

//***************************************************************************************
{Sucht in "FindInString" ab der Position "StartsAt" nach einem numerischen String.
	Bei Erfolg wird dieser als Result zurückgegeben. StartsAt enthält dann die Startposition
	und StrLen die Länge des numerischen Strings.}
function ExtractNumString(const FindInString: string; var StartsAt, StrLen: Integer): string;
var
	I: Integer;
	AC: set of AnsiChar;
	DSep, TSep: AnsiChar;
begin
	if FindInString = '' then begin
		Result := '';
		Exit;
	end;
	if StartsAt <= 0 then
		StartsAt := 1
	else if StartsAt > Length(FindInString) then
		StartsAt := Length(FindInString);
	DSep := AnsiChar(FormatSettings.DecimalSeparator);
	TSep := AnsiChar(FormatSettings.ThousandSeparator);
	AC := ['0'..'9', DSep, '-'];
  while (StartsAt < Length(FindInString))
    and not (AnsiChar(FindInString[StartsAt]) in AC)
  do
		Inc(StartsAt);
	AC := ['0'..'9', DSep, TSep];
	while (StartsAt > 0) and (AnsiChar(FindInString[StartsAt]) in AC) do
	begin
		if FindInString[StartsAt] = FormatSettings.DecimalSeparator then
			Exclude(AC, DSep);
		Dec(StartsAt);
	end;
	if (StartsAt = 0) or (FindInString[StartsAt] <> '-') then
		Inc(StartsAt);
	I := StartsAt;
	while (I <= Length(FindInString)) and (FindInString[I] in AC) do
	begin
		if FindInString[I] = FormatSettings.DecimalSeparator then
			Exclude(AC, DSep);
		Inc(I);
	end;
	StrLen := I - StartsAt;
	Result := Copy(FindInString, StartsAt, StrLen);
end;

//***************************************************************************************
function ExtractNumString(const FindInString: string; StartsAt: Integer=1): string; overload;
var
  Dummy: Integer;
begin
  Result := ExtractNumString(FindInString, StartsAt, Dummy);
end;

//***************************************************************************************
{: Interprets "HexStr" as a row of hexadecimal numbers which, 2 digits at a time,
	are converted to the corresponding character.
		E.g.: 2341266537 -> #A(e7
}
function HexToString(HexStr: string): string;
var
	S: string;
begin
	Result := '';
	S := '';
	while Length(HexStr)>1 do begin
		S := HexStr[1] + HexStr[2];
		Delete(HexStr, 1, 2);
		Result := Result + Char(Hex2Dec(S));
	end;
end;

//***************************************************************************************
function URLEncode(const TxtStr: string): string;
var
	I: Integer;
begin
	Result := '';
	for I := 1 to Length(TxtStr) do begin
		if AnsiChar(TxtStr[I]) in ['0'..'9', 'A'..'Z', 'a'..'z'] then
			Result := Result + TxtStr[I]
		else
			Result := Result + '%' + IntToHex(Ord(TxtStr[I]), 2);
	end;
end;

//***************************************************************************************
function URLDecode(URLStr: string): string;
var
	P: Integer;
begin
	Result := '';
	P := Pos('%', URLStr);
	while P > 0 do begin
		Result := Result + Copy(URLStr, 1, P-1);
		Delete(URLStr, 1, P);
		try
			Result := Result + HexToString(Copy(URLStr, 2, 2));
			Delete(URLStr, 1, 3);
		except
			Result := Result + '%';
			Delete(URLStr, 1, 1);
		end;
		P := Pos('%', URLStr);
	end;
	Result := Result + Copy(URLStr, 1, MaxInt);
end;

//***************************************************************************************
function HTMLEncode(const TxtStr: string; PreferUnicodeIDs: Boolean=False): string;
var
	N, I: Integer;
	Found: Boolean;
begin
	Result := '';
	for I := 1 to Length(TxtStr) do begin
		if AnsiChar(TxtStr[I]) in ['0'..'9', 'A'..'Z', 'a'..'z', ' ',
															#9, #10, #13, '!', '#', '$', '%',
															'('..'/', ':', ';', '=', '?', '@',
															'{'..'~']
		then
			Result := Result + TxtStr[I]
		else if PreferUnicodeIDs then
			Result := Result + '&#' + IntToStr(Ord(TxtStr[I])) + ';'
		else begin
			Found := False;
			for N := Low(HTMLCharConvert) to High(HTMLCharConvert) do
				if HTMLCharConvert[N].ANSI = TxtStr[I] then
				begin
					Result := Result + HTMLCharConvert[N].HTMLName;
					Found := True;
					Break;
				end;
			if not Found then
				Result := Result + '&#' + IntToStr(Ord(TxtStr[I])) + ';';
		end;
	end;
end;

//***************************************************************************************
function HTMLDecode(const HTMLStr: string): string;
var
	SP, EP: Integer;

	function FindHTMLChar(HTMLChar: string): string;
	var
		I: Integer;
	begin
		for I := Low(HTMLCharConvert) to High(HTMLCharConvert) do
			if HTMLCharConvert[I].HTMLName = HTMLChar then
			begin
				Result := HTMLCharConvert[I].ANSI;
				Exit;
			end;
		Result := '';
	end;
begin
	Result := '';
	EP := 0;
	SP := Pos('&', HTMLStr);
	while SP > 0 do begin
		Result := Result + Copy(HTMLStr, EP+1, SP-1);
		EP := PosEx(';', HTMLStr, SP);
		if EP > SP+1 then
		begin
			if HTMLStr[SP+1] = '#' then
				try
					Result := Result + Char(StrToInt(Copy(HTMLStr, SP+2, EP-SP-2)));
				except
					Result := Result + Copy(HTMLStr, SP, EP-SP+1);
				end
			else
				Result := Result + FindHTMLChar(Copy(HTMLStr, SP+1, EP-SP-1));
		end
		else
			EP := SP + 1;
		SP := PosEx('&', HTMLStr, EP);
	end;
end;

//***************************************************************************************
{Vermeidet eine Besonderheit von Round in Delphi:
	Ist der Nachkommaanteil genau 0,5, so wird nicht grundsätzlich aufgerundet,
	sondern zur nächsten GERADEN ZAHL gerundet! (sog. "Bankers rounding")
}
function TSRound(const Value: Extended; Digits: Integer): Extended;
var
	Xpnt: Extended;
begin
	if Digits <> 0 then
		Xpnt := IntPower(10, Digits)
	else
		Xpnt := 1;

	Result := Value * Xpnt;
	if Result - Trunc(Result) = 0.5 then
		Result := (Trunc(Result) + 1) / Xpnt
	else
		Result := Round(Result) / Xpnt;
end;

//***************************************************************************************
function TSRound(const Value: Extended): Integer;
begin
	if Value - Trunc(Value) = 0.5 then
		Result := Trunc(Value) + 1
	else
		Result := Round(Value);
end;

//***************************************************************************************
function SafeDivide(Dividend, Divisor: Extended; DefaultDivisor: Extended=1.0): Extended;
begin
  if Divisor = 0 then
  begin
    if DefaultDivisor = 0 then
      Divisor := 1
    else
      Divisor := DefaultDivisor;
  end;
  Result := Dividend / Divisor;
end;

//***************************************************************************************
function FloatEqual(Num1, Num2: Extended; Precision: Integer=5): Boolean;
begin
	Result := Abs(Num1-Num2) < IntPower(10,-Precision);
end;

//***************************************************************************************
function FmtStrToFloat(S: string): Extended;
begin
	if S = '' then
		Result := 0
	else begin
		S := StringReplace(S, FormatSettings.ThousandSeparator, '', [rfReplaceAll]);
//		S := StringReplace(S, FormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
		Result := StrToFloat(S);
	end;
end;

//***************************************************************************************
function FmtStrToFloat(S: string; Default: Extended): Extended; overload;
begin
	try
		Result := FmtStrToFloat(S);
	except
		Result := Default;
	end;
end;

//***************************************************************************************
function FmtStrToFloat(S: string; USNumFmt: Boolean): Extended; overload;
begin
	if USNumFmt then begin
		S := StringReplace(S, ',', '', [rfReplaceAll]);
		S := StringReplace(S, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
	end;
	Result := FmtStrToFloat(S);
end;

//***************************************************************************************
function USNumStr(const Format: string; const Value: Extended): string;
var
	DS, TS: Char;
begin
	DS := FormatSettings.DecimalSeparator;
	TS := FormatSettings.ThousandSeparator;
	FormatSettings.DecimalSeparator := '.';
	FormatSettings.ThousandSeparator := ',';
	try
		Result := FormatFloat(Format, Value);
	finally
		FormatSettings.DecimalSeparator := DS;
		FormatSettings.ThousandSeparator := TS;
	end;
end;

//***************************************************************************************
function FloatToStrDecSep(Format: string; Number: Extended; DecSep: char): string;
var
	DS: Char;
begin
	DS := FormatSettings.DecimalSeparator;
	FormatSettings.DecimalSeparator := DecSep;
	try
		Result := FormatFloat(Format, Number)
	finally
		FormatSettings.DecimalSeparator := DS;
	end;
end;

//***************************************************************************************
//Wandelt einen Integer in einen Hexadezimalstring im BCD-Format.
//Hat der Integer weniger als MinDigits Stellen, wird mit führenden Nullen aufgefüllt.
//Hat der Integer mehr als MaxDigits Stellen, wird nach MaxDigits Stellen abgeschnitten.
function Int2BCD(Value: Cardinal; MinDigits: Integer = 2;
										MaxDigits: Integer = 10; LSBFirst: Boolean = False): string;
var
	L, I: Integer;
	D0: Byte;
	B: array of Byte;
begin
	Result := '';
	L := 0;
	while Value > 0 do begin
		Inc(L);
		SetLength(B, L);
		B[L-1] := Value mod 10;
		Value := Value div 10;
	end;
	if L > MaxDigits then
	begin
		for I := 0 to pred(MaxDigits) do
			B[I] := B[L-MaxDigits+I];
		SetLength(B, MaxDigits);
		L := MaxDigits;
	end;
	while L < MinDigits do begin
		Inc(L);
		SetLength(B, L);
		B[L-1] := 0;
	end;
	D0 := 0;
	for I := 0 to pred(L) do
		if (I mod 2) = 0 then
			D0 := B[I]
		else begin
			if LSBFirst then
				Result := Result + char(B[I]*16 + D0)
			else
				Result := char(B[I]*16 + D0) + Result;
			D0 := 0;
		end;
	if D0 > 0 then
		Result := char(D0) + Result;
end;

//***************************************************************************************
function BCD2Int(Value: string; LSBFirst: Boolean = False): Cardinal;
var
	P: Integer;
	HN, LN, B: Byte;

begin
	Result := 0;
	if LSBFirst then
	begin
		P := Length(Value);
		while (P > 0) and (Result < ($FFFFFFFF div 100)) do
		begin
			B := Ord(Value[P]);
			HN := B div 16;
			LN := B mod 16;
			if (HN > 9) or (LN > 9) then
				raise Exception.Create('BCD2Int: Value ist kein gültiger BCD-string');
			Result := Result*100 + HN*10 + LN;
			Dec(P);
		end;
	end
	else begin
		P := 1;
		while (P <= Length(Value)) and (Result < ($FFFFFFFF div 100)) do
		begin
			B := Ord(Value[P]);
			HN := B div 16;
			LN := B mod 16;
			if (HN > 9) or (LN > 9) then
				raise Exception.Create('BCD2Int: Value ist kein gültiger BCD-string');
			Result := Result*100 + HN*10 + LN;
			Inc(P);
		end;
	end;
end;

//***************************************************************************************
function StrIfEmpty(S: string; IfEmpty: string = 'NULL'; Quoted: Boolean = True): string;
begin
	if S = '' then
		Result := IfEmpty
	else if Quoted then
		Result := QuotedStr(S)
	else
		Result := S;
end;

//***************************************************************************************
function EvalKeyStr(const CodeStr: string): string;
var
	Src, Tmp: string;
	L, I, P: Integer;
begin
	Src := CodeStr;
	Result := '';
	I := 1;
	L := Length(Src);
	while I <= L do begin
		case Src[I] of
			'^': begin
				Inc(I);
				if AnsiChar(Src[I]) in ['a'..'z'] then
					Result := Result + Char(Ord(Src[I])-Ord('a')+1)
				else if AnsiChar(Src[I]) in ['A'..'Z'] then
					Result := Result + Char(Ord(Src[I])-Ord('A')+1);
			end;
			'#': begin
				Inc(I);
				P := 0; Tmp := '#';
				while AnsiChar(Src[I]) in ['0'..'9'] do begin
					P := P * 10 + Ord(Src[I]) - Ord('0');
					Tmp := Tmp + Src[I];
					Inc(I);
				end;
				if Between(P, 0, 256, []) then
					Result := Result + Char(P)
				else
					Result := Result + Tmp;
			end;
			else
				Result := Result + Src[I];
		end;
		Inc(I);
	end;
end;

//***************************************************************************************
function KeybConv(Str: string; FromLang, ToLang: NLanguageConvert): string;
var
	LngFrom, LngTo: string;
	P, I: Integer;
const
	ConvUS: string = '`~!@#$%^&*()-=_+\|[];''\,./{}:"|<>?zyZY';
	ConvDE: string = '^°!"§$%&/()=ß´?`<>ü+öä#,.-Ü*ÖÄ'';:_yzYZ';
	ConvDK: string = '½§!"#¤%&/()=+´?`<>å¨æø'',.-Å^ÆØ*;:_zyZY';
begin
	if FromLang = ToLang then begin
		Result := Str;
		Exit;
	end;
	case FromLang of
		lngUS: LngFrom := ConvUS;
		lngDEU: LngFrom := ConvDE;
		lngDK: LngFrom := ConvDK;
	end;
	case ToLang of
		lngUS: LngTo := ConvUS;
		lngDEU: LngTo := ConvDE;
		lngDK: LngTo := ConvDK;
	end;

	for I := 1 to Length(Str) do begin
		P := Pos(Str[I], LngFrom);
		if P > 0 then
			Str[I] := LngTo[P];
	end;
	Result := Str;
end;


//***************************************************************************************
function ConvertStr(Str: string; FromSub, ToSub: array of string): string;
var
	ILo, IHi, I: Integer;
begin
	ILo := Max(Low(FromSub), Low(ToSub));
	IHi := Min(High(ToSub), High(FromSub));
	for I := ILo to IHi do
		Str := AnsiReplaceStr(Str, FromSub[I], ToSub[I]);
	Result := Str;
end;

//***************************************************************************************
function FmtStrToFloat(S: string; Default: Extended; USNumFmt: Boolean): Extended; overload;
begin
	try
		Result := FmtStrToFloat(S, USNumFmt);
	except
		Result := Default;
	end;
end;

//***************************************************************************************
function StrFloat(S: string; Precision, Default: Integer): Extended; overload;
begin
	try
		Result := StrFloat(S, Precision);
	except
		Result := Default;
	end;
end;

//***************************************************************************************
function StrFloat(S: string; Precision: Integer=2): Extended; overload;
var
	Prc: Extended;
	I: Int64;
begin
	Prc := IntPower(10, Precision);
	I := Trunc(FmtStrToFloat(S, 0) * Prc + 0.5);
	Result := I/Prc;
end;

//***************************************************************************************
function SQLFloatStr(Value: Extended; Decimals: ShortInt=-1): string;
var
  DS: Char;
begin
  DS := FormatSettings.DecimalSeparator;
	FormatSettings.DecimalSeparator := '.';
  try
    if Decimals < 0 then
  		Result := FloatToStr(Value)
    else begin
      Result := FloatToStr(TSRound(Value, Decimals));
    end;
  finally
    FormatSettings.DecimalSeparator := DS;
  end;
end;

//***************************************************************************************
function INIFloatStr(Value: Extended): string;
var
  I: Integer;
  FR: TFloatRec;
begin
  FloatToDecimal(FR, Value, fvExtended, 22, 9999);
  if FR.Negative then
    Result := '-' + Char(FR.Digits[0]) + '.'
  else
    Result := Char(FR.Digits[0]) + '.';
  for I := 1 to High(FR.Digits) do
    if FR.Digits[I] = '0' then
      Break
    else
      Result := Result + Char(FR.Digits[I]);
  if FR.Exponent <> 1 then
    Result := Result + INIExponentSign + IntToStr(FR.Exponent-1);
end;

function INIStrFloat(Value: string): Extended;
var
  DS: char;
begin
  DS := FormatSettings.DecimalSeparator;
	FormatSettings.DecimalSeparator := '.';
  try
    Result := StrToFloat(Value);
  finally
    FormatSettings.DecimalSeparator := DS;
  end;
end;


//***************************************************************************************
function INIStrFloatDef(Value: string; Default: Extended): Extended;
begin
  try
    Result := INIStrFloat(Value)
  except
    Result := Default;
  end;
end;

//***************************************************************************************
function StrInt(S: string; Default: Integer): Integer; overload;
begin
	Result := Trunc(StrFloat(S, 0, Default));
end;

function StrInt(S: string): Integer; overload;
begin
	Result := Trunc(StrFloat(S, 0));
end;


//***************************************************************************************
function NumIsTSNull(Number: Extended): Boolean;
begin
	Result := Number = TSNumNull;
end;


//***************************************************************************************
function ResizeProportional(FromWidHgt, ToWidHgt: TPoint): TPoint;
var
	RatioH, RatioV: Double;
begin
	RatioH := FromWidHgt.X / ToWidHgt.X;
	RatioV := FromWidHgt.Y / ToWidHgt.Y;
	if RatioH > RatioV then
		Result := Point(ToWidHgt.x, Trunc(FromWidHgt.Y/RatioH))
	else
		Result := Point(Trunc(FromWidHgt.X/RatioV), ToWidHgt.Y);
end;

//***************************************************************************************
function VarEquals(const V1, V2: Variant): Boolean;
begin
	try
		Result := V1 = V2;
	except
		Result := False;
	end;
end;

//***************************************************************************************
function VarToIntDef(const VarValue: Variant; Default: Integer=0): Integer;
begin
	try
		Result := VarValue;
	except
		Result := Default;
	end;
end;

//***************************************************************************************
function VarNullIf(const I: Integer; const NullVal: Integer = 0): Variant;
begin
	if I = NullVal then
		Result := NULL
	else
		Result := I;
end;

//***************************************************************************************
function Distance(Pt1, Pt2: TPoint; Approx: Boolean = False): Integer;
var
	X, Y: Integer;
begin
	X := Abs(Pt1.X - Pt2.X);
	Y := Abs(Pt1.Y - Pt2.Y);
	if Approx then
		Result := Max(X, Y)
	else
		Result := TSRound(Sqrt(X*X + Y*Y));
end;


function CodePageID(CodePageName: string): Integer;
var
	I: Integer;
begin
	for I := Low(CodePageNames) to High(CodePageNames) do
		if SameText(CodePageName, CodePageNames[I].Name) then
		begin
			Result := CodePageNames[I].ID;
			Exit;
		end;
	Result := -1;
end;

function CodePageName(CodePageID: Integer): string;
var
	I: Integer;
begin
	for I := Low(CodePageNames) to High(CodePageNames) do
		if CodePageID = CodePageNames[I].ID then
		begin
			Result := CodePageNames[I].Name;
			Exit;
		end;
	Result := '';
end;

//---------------------------------------------------------------------------------------
function RangeFilter(const FieldName: string; const RangeType: NRangeType;
  											const Val1, Val2: string): string;
begin
	case RangeType of
		rgInvalid:
			Result := '';
		rgSingleValue:
			Result := FieldName + '=' + Val1;
		rgFromValue:
			Result := FieldName + '>=' + Val1;
		rgToValue:
			Result := FieldName + '<=' + Val2;
		rgFromToValue:
			Result := '(' + FieldName + '>=' + Val1 + ')'
							+ ' and (' + FieldName + '<=' + Val2 + ')';
	end;
end; //RangeFilter

//---------------------------------------------------------------------------------------
function RangeDisplay(RangeType: NRangeType; const LowBound, HighBound: string): string;
begin
	case RangeType of
		rgInvalid:
			Result := '';
		rgSingleValue:
			Result := LowBound;
		rgFromValue:
			Result := LowBound + RangeSeparator;
		rgToValue:
			Result := RangeSeparator + HighBound;
		rgFromToValue:
			Result := LowBound + RangeSeparator + HighBound;
	end;
end;

//---------------------------------------------------------------------------------------
function NumRange(const RangeStr: string; out FromNum, ToNum: Extended): NRangeType; overload;
var
	P: Integer;
	FromValid, ToValid: Boolean;
begin
	FromNum := 0;
	ToNum := 0;
		P := Pos(RangeSeparator, RangeStr);
	if P > 0 then
	begin
		try
			FromNum := StrToFloat(Copy(RangeStr, 1, P-1));
			FromValid := True;
		except
			FromValid := False;
		end;
		try
			ToNum := StrToFloat(Copy(RangeStr, P+Length(RangeSeparator), MaxInt));
			ToValid := True;
		except
			ToValid := False;
		end;

		if FromValid then //Vor den ".." steht ein gültiger Wert
		begin
			if ToValid then //... und hinter den ".." steht ebenfalls ein gültiger Wert:
			begin
				Result := rgFromToValue;
				if ToNum < FromNum then
					Switch(FromNum, ToNum);
			end
			else //... aber hinter den ".." steht kein gültiger Wert:
				Result := rgFromValue;
		end
		else if ToValid then //vor den ".." steht kein gültiger Wert, aber dahinter.
			Result := rgToValue
		else //Weder vor noch nach den ".." steht ein gültiger Wert.
			Result := rgInvalid;
	end
	else begin //Das Datum enthält keine ".." - es muss sich um einen einzelnen Wert handeln.
		try
			FromNum := StrToFloat(RangeStr);
			FromValid := True;
		except
			FromValid := False;
		end;
		if FromValid then //Wenn der Wert ungültig ist...
			Result := rgSingleValue
		else
			Result := rgInvalid;
	end;
end;

//---------------------------------------------------------------------------------------
function NumRange(const RangeStr: string; out FromNum, ToNum: Integer): NRangeType; overload;
var
	F, T: Extended;
begin
	Result := NumRange(RangeStr, F, T);
	FromNum := Round(F);
	ToNum := Round(T);
end;

function NumRange(const RangeStr: string; out FromNum, ToNum: string; Format: string): NRangeType;
var
  D1, D2: Extended;
begin
  D1 := 0.0;
  D2 := 0.0;
  Result := NumRange(RangeStr, D1, D2);
  if Format = '' then
  begin
    FromNum := FloatToStr(D1);
    ToNum := FloatToStr(D2);
  end
  else begin
    FromNum := FormatFloat(Format, D1);
    ToNum := FormatFloat(Format, D2);
  end;
end;

function NumRange(const RangeStr: string; Format: string): string;
var
  D1, D2: Extended;
  RgType: NRangeType;

  function FmtVal(Val: Extended): string;
  begin
    if Format = '' then
      Result := FloatToStr(Val)
    else
      Result := FormatFloat(Format, Val);
  end;
begin
  RgType := NumRange(RangeStr, D1, D2);
  case RgType of
  	rgInvalid:
  		Result := '';
  	rgSingleValue:
  		Result := FmtVal(D1);
  	rgFromValue:
  		Result := FmtVal(D1) + RangeSeparator;
  	rgToValue:
  		Result := RangeSeparator + FmtVal(D2);
  	rgFromToValue:
  		Result := FmtVal(D1) + RangeSeparator + FmtVal(D2);
  end;
end;

//---------------------------------------------------------------------------------------
function IntRange(const RangeStr: string; out FromNum, ToNum: Integer): NRangeType;
begin
	Result := NumRange(RangeStr, FromNum, ToNum);
end;

//---------------------------------------------------------------------------------------
function IntRange(RangeStr: string; out FromNum, ToNum: string; Format: string): NRangeType;
var
  N1, N2: Integer;
begin
  N1 := 0;
  N2 := 0;
  Result := IntRange(RangeStr, N1, N2);
  if Format = '' then
  begin
		FromNum := IntToStr(N1);
		ToNum := IntToStr(N2);
  end
  else begin
 		FromNum := FormatFloat(Format, N1);
 		ToNum := FormatFloat(Format, N2);
  end;
end;

//---------------------------------------------------------------------------------------
function IntRange(RangeStr: string; Format: string=''): string;
var
	I1, I2: Integer;
	RgType: NRangeType;

  function FmtVal(Val: Integer): string;
  begin
    if Format = '' then
    	Result := IntToStr(Val)
    else
      Result := FormatFloat(Format, Val);
  end;
begin
	RgType := NumRange(RangeStr, I1, I2);
	case RgType of
		rgInvalid:
			Result := '';
		rgSingleValue:
			Result := FmtVal(I1);
		rgFromValue:
			Result := FmtVal(I1) + RangeSeparator;
		rgToValue:
			Result := RangeSeparator + FmtVal(I2);
		rgFromToValue:
			Result := FmtVal(I1) + RangeSeparator + FmtVal(I2);
	end;
end;

//---------------------------------------------------------------------------------------
function RangeFilterInt(const FieldName, RangeStr: string): string;
var
	N1, N2: Integer;
	RgType: NRangeType;
begin
	RgType := NumRange(RangeStr, N1, N2);
  Result := RangeFilter(FieldName, RgType, IntToStr(N1), IntToStr(N2));
end;

//---------------------------------------------------------------------------------------
function FloatRange(const RangeStr: string; out FromNum, ToNum: Extended): NRangeType;
begin
	Result := NumRange(RangeStr, FromNum, ToNum);
end;

//---------------------------------------------------------------------------------------
function FloatRange(RangeStr: string; out FromNum, ToNum: string; IntlDSep: Boolean; Format: string): NRangeType;
var
  N1, N2: Extended;
  TS, DS: char;
begin
  N1 := 0;
  N2 := 0;
  Result := FloatRange(RangeStr, N1, N2);
  if IntlDSep then
  begin
    DS := FormatSettings.DecimalSeparator;
    TS := FormatSettings.ThousandSeparator;
    FormatSettings.DecimalSeparator := '.';
    FormatSettings.ThousandSeparator := ',';
    try
      if Format = '' then
      begin
		    FromNum := FloatToStr(N1);
  	    ToNum := FloatToStr(N2);
      end
      else begin
 		    FromNum := FormatFloat(Format, N1);
 		    ToNum := FormatFloat(Format, N2);
      end;
    finally
      FormatSettings.DecimalSeparator := DS;
      FormatSettings.ThousandSeparator := TS;
    end;
  end;
end;

//---------------------------------------------------------------------------------------
function FloatRange(RangeStr: string; Format: string=''): string;
var
	F1, F2: Extended;
	RgType: NRangeType;

  function FmtVal(Val: Extended): string;
  begin
    if Format = '' then
    	Result := FloatToStr(Val)
    else
      Result := FormatFloat(Format, Val);
  end;
begin
	RgType := NumRange(RangeStr, F1, F2);
	case RgType of
		rgInvalid:
			Result := '';
		rgSingleValue:
			Result := FmtVal(F1);
		rgFromValue:
			Result := FmtVal(F1) + RangeSeparator;
		rgToValue:
			Result := RangeSeparator + FmtVal(F2);
		rgFromToValue:
			Result := FmtVal(F1) + RangeSeparator + FmtVal(F2);
	end;
end;

//---------------------------------------------------------------------------------------
function RangeFilterFloat(const FieldName, RangeStr: string): string;
var
	N1, N2: Extended;
	RgType: NRangeType;
begin
	RgType := NumRange(RangeStr, N1, N2);
  Result := RangeFilter(FieldName, RgType, SQLFloatStr(N1), SQLFloatStr(N2));
end;

//---------------------------------------------------------------------------------------
function StringRange(RangeStr: string; out RangeStart, RangeEnd: string): NRangeType;
var
	P: Integer;
begin
  RangeEnd := '';
	P := Pos(RangeSeparator, RangeStr);
	if P > 0 then
	begin
		RangeStart := Copy(RangeStr, 1, P-1);
		RangeEnd := Copy(RangeStr, P+Length(RangeSeparator), MaxInt);
    if RangeStart > '' then
    begin
      if RangeEnd > '' then
      begin
      	Result := rgFromToValue;
        if CompareText(RangeStart, RangeEnd) > 0 then
        	Switch(RangeStart, RangeEnd);
      end
      else
        Result := rgFromValue;
    end
    else if RangeEnd > '' then
    	Result := rgToValue
    else
      Result := rgInvalid;
  end
  else begin //Der String enthält keine ".."
		RangeStart := RangeStr;
    Result := rgSingleValue;
  end;
end;

//---------------------------------------------------------------------------------------
function StringRange(RangeStr: string): string;
var
	S1, S2: string;
	RgType: NRangeType;
begin
	RgType := StringRange(RangeStr, S1, S2);
  Result := RangeDisplay(RgType, '"'+S1+'"', '"'+S2+'"');
end;

//---------------------------------------------------------------------------------------
function RangeFilterStr(const FieldName, RangeStr: string): string;
var
	S1, S2: string;
	RgType: NRangeType;
begin
	RgType := StringRange(RangeStr, S1, S2);
  Result := RangeFilter(FieldName, RgType, QuotedStr(S1), QuotedStr(S2));
end;

//---------------------------------------------------------------------------------------
function CalculateExpressionValue(var Expression: string; var EditPos: Integer;
                                      Options: SCalcExpressionOptions): Extended;
var
  Tmp, Fkt1: Extended;
  NumPos, I, L, Sgn, EdPosNeu, DcmExp, ThsdOffs: Integer;
  Dcml, Quo: Boolean;
  Operation: NCalcFunction;
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
  	case Operation of
      calcQuotient:
        if (Tmp = 0) then
        	raise EZeroDivide.Create('Division by Zero in "CalculateExpressionValue" at ' + IntToStr(I))
        else
  				Result := Fkt1 / Tmp * Sgn;
    	calcExponent:
        Result := power(Fkt1, Tmp);
    	else //calcProduct:
    		Result := Fkt1 * Tmp * Sgn;
		end;
		//if Quo then
  //  begin
  //    if (Tmp = 0) then
  //    	raise EZeroDivide.Create('Division by Zero in "CalculateExpressionValue" at ' + IntToStr(I))
  //    else
		//		Result := Fkt1 / Tmp * Sgn;
		//end
		//else
		//	Result := Fkt1 * Tmp * Sgn;
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
			'*', '/', '^': begin
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
        case S[I] of
          '/': Operation := calcQuotient;
          '^': Operation := calcExponent;
        else
          Operation := calcProduct;
        end;
				//Quo := S[I] = '/';
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


{$IFDEF WINDOWS}
//***************************************************************************************
function KeybScanCode(Chr: char): Word; overload;
var
	VK: Word;
begin
	VK := VkKeyScan(Chr);
	Result := Hi(VK)*256 + MapVirtualKey(Lo(VK), 0);
//	Result := OemKeyScan(VkKeyScan(Chr));
//	Result := MapVirtualKey(VkKeyScan(Chr), 0);
end;

//***************************************************************************************
function KeybScanCode(VirtKeyCode: Word): Word; overload;
begin
	Result := MapVirtualKey(VirtKeyCode, 0);
end;

//***************************************************************************************
//"Übersetzt" ein Zeichen, das mit der aktuellen Tastatur-spracheinstellung
// eingegeben wurde in das ensprechende Zeichen, das bei Druck derselben Taste
// auf einer US-Tastatur entstanden wäre.
function USKey(Chr: char): string;
var
	KbHndl: HKL;
	SCode, VKey, KUpr: Word;
	I: Integer;
	ResultChar: PChar;
	KBState: TKeyboardState;
begin
	//1) Get virtual key code for character
	VKey := VkKeyScan(Chr);
	KUpr := VKey and $FF00; //Isolate shift state
	//2) Get scan code for this VK-key on the current national keyboard
	SCode := MapVirtualKey(Lo(VKey), 0);

	//3) Get the virtual key for this scan code (=key position) on the US-Keyboard.
	KbHndl := LoadKeyboardLayout('00000409', 0);
	VKey := MapVirtualKeyEx(SCode, 1, KbHndl);

	//Build a keyboard state array for "no key pressed"
	for I := 0 to 255 do
		KBState[I] := 0;

	//If the shift key was pressed ín the original VKey, set it in the keyboard state array.
	if (KUpr and $100) >0 then begin
		kbstate[vk_shift] := 128;
//		kbstate[vk_Lshift] := 128;
//		kbstate[vk_rshift] := 128;
	end;

	//4) Now build the ASCII character based on the new virtual key and shift state.
	GetMem(ResultChar, 3);
	try
		I := ToAsciiEX(VKey, 0, KBState, LPWord(ResultChar), 0, KbHndl);
		if I in [1..2] then
			Result := ResultChar[0]
		else
			Result := '!'//Chr; //If translation failed return original character.
	finally
		FreeMem(ResultChar);
	end;
end;

//***************************************************************************************
//Wandelt einen String in die Entsprechung einer Eingabe der selben Tastendrücke
//	auf der US-Tastatur (siehe USKey).
function USStr(Str: string): string;
var
	I: Integer;
begin
	Result := '';
	for I := 1 to Length(Str) do
		Result := Result + USKey(Str[I]);
end;

//***************************************************************************************
function ScanCode2Char(ScanCode: Word): char;
begin
	Result := Char(Lo(MapVirtualKey(MapVirtualKey(ScanCode, 1), 2)));
end;

//***************************************************************************************
function ScanCode2VirtKey(ScanCode: Word): Word;
begin
	Result := MapVirtualKey(ScanCode, 1);
end;

//***************************************************************************************
function VirtKey2Char(VirtKey: Word): char;
begin
	Result := Char(Lo(MapVirtualKey(VirtKey, 2)));
end;

{$ENDIF} //IFDEF Windows



end.
