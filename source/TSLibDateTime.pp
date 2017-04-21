unit TSLibDateTime;

interface
uses
	Windows,
  TSResources,
  TSLibConvert,
  Classes;

const
  TSDateNULL = -693594; // = (01.01.0001)-1 = 00.00.0000
  TSDateNULLName = '<NULL>';
  TSTimeNULL = -(1/24/3600/1000); // = 1ms before 30. Dez 1899

type
	MTSDateEvent = procedure(Sender: TObject; var ADate: TDateTime; var Accept: Boolean) of object;
	NTSDatePart = (dpDay, dpMonth, dpYear);
//	NDateRangee
  NDateTimePart = (dtNone, dtYear, dtMonth, dtWeek, dtDay, dtHour, dtMinute, dtSecond, dtMillisecond);
  SDateTimeParts = set of NDateTimePart;
  //Correction Type for date part validations with automaic correction.
  //dcBounds: Truncate each date part to fit into its Bounds (Month<=12, Day <= max day in month)
  //dcAdd: Months/days that exceed the allowed bounds proceed to the next higher date part
  //				(e.g. d=40,m=15,y=2010 -> d=9 (=40-31), m=4 (=15-12+1), y=2011 (=2010+1)
	NDateCorrectionType = (dcBounds, dcAdd);
	NTSDateDefault = (ddToday, ddThisMonth, ddThisYear, ddFix);

  RDateRange = record
    Date: TDateTime;
    RangeDate: TDateTime;
    RangeType: NRangeType;
  end;

//***************************************************************************************
{ TTSDateCalc }
//***************************************************************************************
  TTSDateCalc = class(TPersistent)
  private
    fFixDate: TDateTime;
    fOffsDays: Integer;
    fOffsMonths: Integer;
    fOffsYears: Integer;
    fBase: NTSDateDefault;
    fOnChange: TNotifyEvent;
    function GetDate: TDateTime;
		function GetDateNoOffset: TDateTime;
    procedure SetBase(const Value: NTSDateDefault);
    procedure SetFixDate(const Value: TDateTime);
    procedure SetOffsDays(const Value: Integer);
    procedure SetOffsMonths(const Value: Integer);
    procedure SetOffsYears(const Value: Integer);
    function StoreFixDate: Boolean;
  protected
    procedure Change; virtual;
	public
		constructor Create;
	published
		//Base defines how the Default Date is generated:
		//	ddToday: Current Date + Offset
		//	ddThisMonth: First day of the current Month + Offset
		//	ddThisYear: First day of the current Year + Offset
		//	ddFix: The Date specified in "FixDate" + Offset
		property Base: NTSDateDefault read fBase write SetBase default ddFix;
		//Offset properties define a date interval that is added to the basic default date.
		property OffsetDays: Integer read fOffsDays write SetOffsDays default 0;
		property OffsetMonths: Integer read fOffsMonths write SetOffsMonths default 0;
		property OffsetYears: Integer read fOffsYears write SetOffsYears default 0;
		//Used as the date base if Base is ddFix
		property FixDate: TDateTime read fFixDate write SetFixDate stored StoreFixDate;
		property Date: TDateTime read GetDate;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

//***************************************************************************************
{ TTSDateRange}
//***************************************************************************************
  TTSDateRange = class(TPersistent)
  private
    fRangeDate: TDateTime;
    fDate: TDateTime;
    fRangeType: NRangeType;
    procedure SetDate(const Value: TDateTime);
    procedure SetRangeDate(const Value: TDateTime);
    procedure SetRangeType(const Value: NRangeType);
    function StoreDate: Boolean;
    function StoreRangeDate: Boolean;
    function GetRange: RDateRange;
    procedure SetRange(const Value: RDateRange);
  public
    constructor Create;
    property Range: RDateRange read GetRange write SetRange stored False;
  published
    property Date: TDateTime read fDate write SetDate stored StoreDate;
    property RangeDate: TDateTime read fRangeDate write SetRangeDate stored StoreRangeDate;
    property RangeType: NRangeType read fRangeType write SetRangeType default rgInvalid;
  end;

//***************************************************************************************
{ TTSDateRangeCalc }
//***************************************************************************************
	TTSDateRangeCalc = class(TTSDateCalc)
	private
    fRangeType: NRangeType;
		fRangeDays: Integer;
		fRangeMonths: Integer;
		fRangeYears: Integer;
		function GetRangeDate: TDateTime;
		procedure SetRangeDays(const Value: Integer);
		procedure SetRangeMonths(const Value: Integer);
		procedure SetRangeYears(const Value: Integer);
    procedure SetRangeType(const Value: NRangeType);
    function GetRange: RDateRange;
    function StoreRangeType: Boolean;
  protected
    procedure Change; override;
	public
		constructor Create;
    destructor Destroy; override;
    property Range: RDateRange read GetRange;
	published
		//If a Range Interval is specified a second default date is generated
		//  at the specified date offset, which is added to the default date.
		property RangeDays: Integer read fRangeDays write SetRangeDays default 0;
		property RangeMonths: Integer read fRangeMonths write SetRangeMonths default 0;
		property RangeYears: Integer read fRangeYears write SetRangeYears default 0;
		//RangeDate = Date + range offset
		property RangeDate: TDateTime read GetRangeDate;
    property RangeType: NRangeType read fRangeType write SetRangeType stored StoreRangeType;
    property Base default ddToday;
	end;


function FmtDate(const Fmt: string; const DateValue: TDateTime): string; overload;
function FmtDate(const DateValue: TDateTime): string; overload;
function FmtDate(const Fmt: string; const DateValue: TDateTime; Quoted: Boolean): string; overload;

function AnsiDate(DateValue: TDateTime; Quoted: Boolean=False): string;
function AnsiDateTime(DateValue: TDateTime; Quoted: Boolean=False): string;
function YMD(DateValue: TDateTime): string;
function YMDHMS(DateValue: TDateTime): string;
function DMY(DateValue: TDateTime): string;
function DOW(DateValue: TDateTime): string;
function DMYHM(DateValue: TDateTime): string;
function DMYHMS(DateValue: TDateTime): string;
function WorkingDaysAdd(StartDate: TDateTime; DaysAdd, WorkDaysPrWeek: Integer): TDateTime;

function FmtStrToDate(DateStr: string): TDateTime; overload;
function FmtStrToDate(DateStr: string; DefaultDate: TDateTime): TDateTime; overload;
function FmtStrToDate(DateStr: string; DateFormat: string; DefaultDate: TDateTime): TDateTime; overload;
function FmtStrToDate(DateStr: string; DateFormat: string): TDateTime; overload;
function FileTime2DateTime(FileTime: TFileTime): TDateTime;
function AnsiDateStr2Date(DateStr: string): TDateTime;

//Similar to EncodeDate. Adds Correction Types:
// dcBounds - Month <= 0 := 1; Month > 12 := 12; Day <= 0 := 1; Day > max(Month) := max(Month)
// dcAdd: Add Month>12 to Years, Day > max(Month) adds Month
procedure ValidateDateParts(var Y, M, D: Integer; ErrorCorrectionType: NDateCorrectionType = dcBounds);
function CheckDayOfMonth(Y, M, D: Integer): Word;
function DateAdd(Base: TDateTime; Y, M, D: Integer): TDateTime; overload;
function DateAdd(ToDate: TDateTime; Interval: string; DefaultIntervalType: NDateTimePart=dtDay): TDateTime; overload;
function DateEncode(Y, M, D: Integer; ErrorCorrectionType: NDateCorrectionType = dcBounds): TDateTime;

function EvalDate(DateStr: string; DefaultDate: TDateTime=TSDateNULL; DefaultIntervalType: NDateTimePart=dtDay): TDateTime;
function EvalTime(TimeStr: string; AllowFullDays: Boolean=False; IgnoreErrors: Boolean=False): TDateTime;
function EvalDateTime(DateTimeStr: string): TDateTime;

function ValidateDateRange(var ARange: RDateRange): Boolean;

function DateRange(RangeString: string; out RangeStart, RangeEnd: TDateTime; const Defaults: RDateRange): NRangeType; overload;
function DateRange(RangeString: string; out RangeStart, RangeEnd: TDateTime): NRangeType; overload;
function DateRange(RangeString: string): RDateRange; overload;
function DateRange(RangeString: string; const Defaults: RDateRange): RDateRange; overload;
function DateRange(RangeString: string; out RangeStart, RangeEnd: string; Format: string=''): NRangeType; overload;
function DateRange(Date, RangeDate: TDateTime; RangeType: NRangeType): RDateRange; overload;

function FormatDateRange(RangeString: string; Format: string='ddddd'): string; overload;
function FormatDateRange(Range: RDateRange; Format: string='ddddd'): string; overload;
function RangeFilterDate(const FieldName, RangeStr: string; Format: string='"''"ddddd"''"'): string;

function TimeRange(RangeString: string; out RangeStart, RangeEnd: TDateTime): NRangeType; overload;
function TimeRange(RangeString: string): RDateRange; overload;
function TimeRange(RangeString: string; out RangeStart, RangeEnd: string; Format: string=''): NRangeType; overload;
function FormatTimeRange(Range: RDateRange; Format: string='tt'): string; overload;
function FormatTimeRange(RangeString: string; Format: string='tt'): string; overload;
function RangeFilterTime(const FieldName, RangeStr: string; Format: string='"''"tt"''"'): string;

function DateTimeRange(RangeString: string; out RangeStart, RangeEnd: TDateTime): NRangeType; overload;
function DateTimeRange(RangeString: string): RDateRange; overload;
function DateTimeRange(RangeString: string; out RangeStart, RangeEnd: string; Format: string=''): NRangeType; overload;
function FormatDateTimeRange(Range: RDateRange; Format: string='c'): string; overload;
function FormatDateTimeRange(RangeString: string; Format: string='c'): string; overload;
function RangeFilterDateTime(const FieldName, RangeStr: string; Format: string='"''"c"''"'): string;


const
	sqlServerDateTimeMin = -53688;
  dateRangeNULL: RDateRange = (Date: TSDateNULL; RangeDate: TSDateNULL; RangeType: rgInvalid);

var
	DateToken: array [NDateTimePart] of string = ('',
                                                rsDateTokenYear,
                                                rsDateTokenMonth,
                                                rsDateTokenWeek,
                                                rsDateTokenDay,
                                                rsDateTokenHour,
                                                rsDateTokenMinute,
                                                rsDateTokenSecond,
                                                rsDateTokenMillisecond);
  TSDateNULLText: string = 'NULL';
	MinYearAllowed: Word = 2200;
	MaxYearAllowed: Word = 1900;

implementation

uses
	TSLib,
	Math,
	SysUtils,
	DateUtils;

//---------------------------------------------------------------------------------------
function FmtDate(const Fmt: string; const DateValue: TDateTime; Quoted: Boolean): string;
begin
  if DateValue = TSDateNULL then
    Result := TSDateNULLText
  else if Quoted then
		Result := QuotedStr(FormatDateTime(Fmt, DateValue))
	else
		Result := FormatDateTime(Fmt, DateValue);
end;

function FmtDate(const Fmt: string; const DateValue: TDateTime): string; overload;
begin
  Result := FmtDate(Fmt, DateValue, False);
end;

function FmtDate(const DateValue: TDateTime): string; overload;
begin
  Result := FmtDate('ddddd', DateValue, False);
end;

//---------------------------------------------------------------------------------------
function AnsiDate(DateValue: TDateTime; Quoted: Boolean=False): string;
begin
  Result := FmtDate('yyyymmdd', DateValue, Quoted);
end;

//---------------------------------------------------------------------------------------
function AnsiDateTime(DateValue: TDateTime; Quoted: Boolean=False): string;
begin
  Result := FmtDate('yyyymmdd hh:nn:ss', DateValue, Quoted);
end;

//---------------------------------------------------------------------------------------
function YMD(DateValue: TDateTime): string;
begin
  Result := FmtDate('yy.mm.dd', DateValue);
end;

//---------------------------------------------------------------------------------------
function YMDHMS(DateValue: TDateTime): string;
begin
	Result := FmtDate('yyyymmdd hhnnss', DateValue);
end;

//---------------------------------------------------------------------------------------
function DMY(DateValue: TDateTime): string;
begin
	Result := FmtDate(FormatSettings.ShortDateFormat, DateValue)
end;

//---------------------------------------------------------------------------------------
function DOW(DateValue: TDateTime): string;
begin
	Result := FmtDate('ddd', DateValue);
end;

//---------------------------------------------------------------------------------------
function DMYHM(DateValue: TDateTime): string;
begin
	Result := FmtDate('dd.mm.yyyy hh:nn', DateValue);
end;

//---------------------------------------------------------------------------------------
function DMYHMS(DateValue: TDateTime): string;
begin
	Result := FmtDate('dd.mm.yyyy hh:nn:ss', DateValue);
end;

//---------------------------------------------------------------------------------------
function WorkingDaysAdd(StartDate: TDateTime; DaysAdd, WorkDaysPrWeek: Integer): TDateTime;
var
	DWStart: Integer;
	Weeks, Days: Word;
begin
	if WorkDaysPrWeek > 7 then
		WorkDaysPrWeek := 7
	else if WorkDaysPrWeek < 1 then
		WorkDaysPrWeek := 1;
	DivMod(DaysAdd, WorkDaysPrWeek, Weeks, Days);
	if (Days>0) and (WorkDaysPrWeek<7) then begin
		DWStart := ((DayOfWeek(StartDate)+6) mod 7) + 1;
		if DWStart + Days > WorkDaysPrWeek then
			Result := StartDate + 7*Weeks + Days + (7-WorkDaysPrWeek)
		else
			Result := StartDate + 7*Weeks + Days;
	end
	else
		Result := StartDate + 7*Weeks
end;

//---------------------------------------------------------------------------------------
function FmtStrToDate(DateStr: string): TDateTime; overload;
begin
	Result := FmtStrToDate(DateStr, 'yyyymmdd hh:nn:ss:zzz');
end;

//---------------------------------------------------------------------------------------
function FmtStrToDate(DateStr: string; DefaultDate: TDateTime): TDateTime; overload;
begin
	try
		Result := FmtStrToDate(DateStr, 'yyyymmdd hh:nn:ss:zzz');
	except
		Result := DefaultDate;
	end;
end;

//---------------------------------------------------------------------------------------
function FmtStrToDate(DateStr: string; DateFormat: string; DefaultDate: TDateTime): TDateTime; overload;
begin
	try
		Result := FmtStrToDate(DateStr, DateFormat);
	except
		Result := DefaultDate;
	end;
end;

//---------------------------------------------------------------------------------------
function FmtStrToDate(DateStr: string; DateFormat: string): TDateTime; overload;
var
	Y, M, D, H, MM, S, MS: Word;
	L: Integer;

	function DatePart(Part: string; DefaultPart: Integer): Integer;
	var
		P: Integer;
		S: string;
	begin
		S := '';
		P := Pos(Part, DateFormat);
		while (P>0)
		and (DateFormat[P] = Part)
		and (P<=L) do begin
			S := S + DateStr[P];
			Inc(P);
		end;
		Result := StrToIntDef(S, DefaultPart);
	end;

begin
	DateFormat := UpperCase(DateFormat);
	L := Length(DateStr);

	Y := DatePart('Y', 1900);
	M := DatePart('M', 1);
	D := DatePart('D', 1);
	H := DatePart('H', 0);
	MM := DatePart('N', 0);
	S := DatePart('S', 0);
	MS := DatePart('Z', 0);

	Result := EncodeDateTime(Y, M, D, H, MM, S, MS);
end;

//---------------------------------------------------------------------------------------
function FileTime2DateTime(FileTime: TFileTime): TDateTime;
var
	LocalFileTime: TFileTime;
	SystemTime: TSystemTime;
begin
	FileTimeToLocalFileTime(FileTime, LocalFileTime) ;
	FileTimeToSystemTime(LocalFileTime, SystemTime) ;
	Result := SystemTimeToDateTime(SystemTime) ;
end;

//---------------------------------------------------------------------------------------
{Wandelt einen Zeitstempel-String im Format yyyymmdd hh:nn:ss in einen TDateTime-Wert um.
	Ist der String ungültig, wird ein 0-Datum (31.12.1899) zurückgegeben.}
function AnsiDateStr2Date(DateStr: string): TDateTime;
var
	D, M, Y, H, N, S: Word;
	T: TDateTime;
begin
	try
		Y := StrToIntDef(Copy(DateStr, 1, 4), 0);
		Delete(DateStr, 1, 4);
		M := StrToIntDef(Copy(DateStr, 1, 2), 0);
		Delete(DateStr, 1, 2);
		D := StrToIntDef(Copy(DateStr, 1, 2), 0);
		Delete(DateStr, 1, 3);
		if (Y>0) and (M>0) and (D>0) then
			Result := EncodeDate(Y, M, D)
		else
			Result := 0;
		if DateStr > '' then
		begin
			H := StrToIntDef(Copy(DateStr, 1, 2), 0);
			Delete(DateStr, 1, 3);
			N := StrToIntDef(Copy(DateStr, 1, 2), 0);
			Delete(DateStr, 1, 3);
			S := StrToIntDef(Copy(DateStr, 1, 2), 0);
			T := EncodeTime(H, N, S, 0);
			Result := Result + T;
		end;
	except
		Result := 0;
	end;
end;

//---------------------------------------------------------------------------------------
procedure ValidateDateParts(var Y, M, D: Integer; ErrorCorrectionType: NDateCorrectionType = dcBounds);
var
	Dt: TDateTime;
	YRes, MRes, DRes: Word;
begin
	if ErrorCorrectionType = dcBounds then
	begin
		M := BoundsVal(M, 1, 12);
		D := BoundsVal(D, 1, DaysInAMonth(Y, M));
	end
	else begin
		while M < 1 do
		begin
			M := M + 12;
			dec(Y);
		end;
		while M > 12 do
		begin
			M := M - 12;
			inc(Y);
		end;
		if D > 28 then
		begin
			Dt := EncodeDate(Y, M, 1) - 1 + D;
			DecodeDate(Dt, YRes, MRes, DRes);
			Y := YRes;
			M := MRes;
			D := DRes;
		end;
	end;
end;

//---------------------------------------------------------------------------------------
function CheckDayOfMonth(Y, M, D: Integer): Word;
begin
	Result := BoundsVal(D, 1, DaysInAMonth(Y, M));
end;

//---------------------------------------------------------------------------------------
function DateEncode(Y, M, D: Integer; ErrorCorrectionType: NDateCorrectionType): TDateTime;
begin
	ValidateDateParts(Y, M, D, ErrorCorrectionType);
	Result := EncodeDate(Y, M, D);
end;

//---------------------------------------------------------------------------------------
function DateAdd(Base: TDateTime; Y, M, D: Integer): TDateTime;
var
	OY, OM, OD: Word;
	DD: Integer;
begin
	DecodeDate(Base, OY, OM, OD);
	Y := Y + OY;
	M := M + OM;
	DD := 1;
	ValidateDateParts(Y, M, DD, dcAdd);
	OD := CheckDayOfMonth(Y, M, OD);
	Result := EncodeDate(Y, M, OD) + D;
end;

//---------------------------------------------------------------------------------------
function DateAdd(ToDate: TDateTime; Interval: string; DefaultIntervalType: NDateTimePart=dtDay): TDateTime;
var
  Sgn, I, D, M, Y: Integer;
begin
  D := 0;
  M := 0;
  Y := 0;
  if Interval>'' then
  begin
    I := 0;
    if Interval[1] = '-' then
    begin
    	Sgn := -1;
      Delete(Interval, 1, 1);
    end
    else begin
      Sgn := 1;
      if Interval[1] = '+' then
	      Delete(Interval, 1, 1);
    end;
    while (Interval>'') and (Interval[1] in ['0'..'9']) do begin
      I := I*10 + ord(Interval[1])-ord('0');
      Delete(Interval, 1, 1);
    end;
    if I = 0 then
      I := 1;
    if StartsText(DateToken[dtYear], Interval) then
    begin
      Y := Sgn*I;
      Delete(Interval, 1, Length(DateToken[dtYear]));
    end
    else if StartsText(DateToken[dtMonth], Interval) then
    begin
      M := Sgn*I;
      Delete(Interval, 1, Length(DateToken[dtMonth]));
    end
    else if StartsText(DateToken[dtWeek], Interval) then
    begin
    	D := Sgn*I*7;
      Delete(Interval, 1, Length(DateToken[dtWeek]));
    end
    else if StartsText(DateToken[dtDay], Interval) then
    begin
      D := Sgn*I;
      Delete(Interval, 1, Length(DateToken[dtDay]));
    end
    else begin
      if (Interval='') or (AnsiChar(Interval[1]) in ['+', '-', ' ']) then
        case DefaultIntervalType of
          dtYear:
            Y := Sgn*I;
          dtMonth:
            M := Sgn*I;
          dtWeek:
            D := Sgn*I*7;
          dtDay:
            D := Sgn*I;
        end;
    end;

    while (Interval>'') and not (AnsiChar(Interval[1]) in ['+', '-', '0'..'9']) do
      Delete(Interval, 1, 1);

    if (D<>0) or (M<>0) or (Y<>0) then
      ToDate := DateAdd(ToDate, Y, M, D);
    ToDate := DateAdd(ToDate, Interval, DefaultIntervalType);
  end;
  Result := ToDate;
end;

//---------------------------------------------------------------------------------------
function EvalDate(DateStr: string; DefaultDate: TDateTime=TSDateNULL; DefaultIntervalType: NDateTimePart=dtDay): TDateTime;
var
	L, P, MaxDay, DPDay, DPMonth, DPYear, D, M, Y: Word;
	MonthFirst: Boolean;

  function GetDatePart(MaxPartLen, MaxPartVal: Word; Separator: char): Word;
  var
    X, D: Word;
  begin
    Result := 0;
    X := MinVal([Integer(L+1), Integer(P+MaxPartLen)]);
    while P < X do
    begin
			if AnsiChar(DateStr[P]) in ['0'..'9'] then
	    begin
  	  	D := Result * 10 + Ord(DateStr[P])-ord('0');
    	  if D <= MaxPartVal then
      	begin
		    	Result := D;
  	    	inc(P);
    	  end
      	else
	        Break;
      end
    	else
      begin
        if DateStr[P] = Separator then
	        inc(P);
        Break;
      end;
    end;
    if (P<=L) and (DateStr[P] = Separator) then
    	inc(P);
  end;

begin
	Result := TSDateNULL;
  P := 1;
  L := Length(DateStr);

  if TodayToken = '' then
    TodayToken := 'x';
  if DateStr = '' then
    Exit
  else if StartsText(TodayToken, DateStr) then
  begin
    inc(P);
    Result := DateAdd(Today, Copy(DateStr, P, L));
    Exit;
  end;

  MonthFirst := Pos('M', UpperCase(FormatSettings.ShortDateFormat))
  										<Pos('D', UpperCase(FormatSettings.ShortDateFormat));
  if DefaultDate = TSDateNULL then
		DecodeDate(Today, Y, M, D)
  else
		DecodeDate(DefaultDate, Y, M, D);

  MaxDay := 31;
  repeat
    P := 1;
    DPDay := GetDatePart(2, MaxDay, FormatSettings.DateSeparator);

    DPMonth := GetDatePart(2, 12, FormatSettings.DateSeparator);

    if MonthFirst and (DPMonth>0) then
    begin
      P := 1;
      DPMonth := GetDatePart(2, 12, FormatSettings.DateSeparator);
      DPDay := GetDatePart(2, 31, FormatSettings.DateSeparator);
      if DPMonth > 0 then
        M := DPMonth;
      if DPDay > 0 then
        D := DPDay;
    end
    else begin
      if DPDay > 0 then
        D := DPDay;
      if DPMonth > 0 then
        M := DPMonth;
    end;

    DPYear := GetDatePart(4, 9999, #0);
    if DPYear > 0 then
    begin
      if DPYear <= FormatSettings.TwoDigitYearCenturyWindow then
        Y := DPYear + 2000
      else if DPYear < 100 then
        Y := DPYear + 1900
      else
        Y := DPYear;
    end;
    MaxDay := DaysInAMonth(Y, M);
  until IsValidDate(Y, M, D);
  Result := DateAdd(EncodeDate(Y, M, D), Copy(DateStr, P, L), DefaultIntervalType);
end; //EvalDate

//---------------------------------------------------------------------------------------
function EvalTime(TimeStr: string; AllowFullDays: Boolean=False; IgnoreErrors: Boolean=False): TDateTime;
var
  P, Tage, Std, Min, Sek, ms: Integer;
  TS: string;

  function GetSegment(Separator: char; MaxLen, MaxVal: Integer): Integer;
  begin
    Result := 0;
    if TimeStr='' then
    	Exit;
    P := Pos(Separator, TimeStr);
    if (P>MaxLen) then
    begin
    	Result := StrToIntDef(Copy(TimeStr, 1, MaxLen), TSDateNULL);
    	P := MaxLen;
    end
    else if P>1 then
    	Result := StrToIntDef(Copy(TimeStr, 1, P-1), -1)
    else if P>0 then
    	Result := 0
    else begin
      Result := StrToIntDef(Copy(TimeStr, 1, MaxLen), -1);
      P := MaxLen;
    end;
    if (Result<0) or (Result>MaxVal) then
    begin
      if not IgnoreErrors then
	    	raise EConvertError.Create(ErrMsg(exMsgTimeInvalid, [TS], 'function EvalTime'))
      else if Result < 0 then
      	Result := 0
      else
      	Result := MaxVal;
    end;
    Delete(TimeStr, 1, P);
  end;

begin
  Result := TSTimeNULL;
  if TimeStr = '' then
  	Exit;
  TS := TimeStr;
  Tage := 0;
  if AllowFullDays then
	  Std := GetSegment(FormatSettings.TimeSeparator, 2, 99)
  else
	  Std := GetSegment(FormatSettings.TimeSeparator, 2, 23);
  if AllowFullDays then
		Tage := Std div 24;
  Std := Std mod 24;

  Min := GetSegment(FormatSettings.TimeSeparator, 2, 59);
  Sek := GetSegment(FormatSettings.DecimalSeparator, 2, 59);

  if TimeStr > '' then
  begin
    while Length(TimeStr) < 3 do
    	TimeStr := TimeStr + '0';
  	ms := StrToIntDef(Copy(TimeStr, 1, 3), -1);
    if (ms<0) then
    begin
    	if not IgnoreErrors then
	    	raise EConvertError.Create(ErrMsg(exMsgTimeInvalid, [TS], 'function EvalTime'))
      else
      	ms := 0;
    end;
  end
  else
    ms := 0;

  Result := EncodeTime(Std, Min, Sek, ms);
  if AllowFullDays then
		Result := Result + Tage;
end; //EvalTime

//---------------------------------------------------------------------------------------
function EvalDateTime(DateTimeStr: string): TDateTime;
var
  P: Integer;
  DS, TS: string;
  T: TDateTime;
begin
  Result := TSDateNULL;
  if DateTimeStr = '' then
  	Exit;
	P := Pos(' ', DateTimeStr);
  if P > 0 then
  begin
		DS := Copy(DateTimeStr, 1, P-1);
    TS := Copy(DateTimeStr, P+1, MaxInt);
  end
  else begin
		DS := Copy(DateTimeStr, 1, 6);
    TS := Copy(DateTimeStr, 7, MaxInt);
  end;
  Result := EvalDate(DS);
  if Result = TSDateNULL then
  	Exit;
	T := EvalTime(TS);
  if T <> TSDateNULL then
		Result := Result + T;
end; //EvalDateTime

//---------------------------------------------------------------------------------------
function DateTimeRange(P: Integer; var RangeStart, RangeEnd: TDateTime): NRangeType; overload;
begin
	if P > 0 then
	begin
		if RangeStart <> TSDateNULL then //Vor den ".." steht ein gültiges Datum
		begin
			if RangeEnd <> TSDateNULL then //... und hinter den ".." steht ebenfalls ein gültiges Datum:
			begin
				Result := rgFromToValue;
				if RangeEnd < RangeStart then
					Switch(RangeStart, RangeEnd);
			end
			else begin//... aber hinter den ".." steht kein gültiges Datum:
        RangeEnd := TSDateNULL;
				Result := rgFromValue;
      end;
		end
		else if RangeEnd <> TSDateNULL then //vor den ".." steht kein gültiges Datum, aber dahinter.
    begin
      RangeStart := TSDateNULL;
			Result := rgToValue;
    end
		else begin  //Weder vor noch nach den ".." steht ein gültiges Datum.
      RangeStart := TSDateNULL;
      RangeEnd := TSDateNULL;
			Result := rgInvalid;
    end;
	end
	else begin //Das Datum enthält keine ".." - es muss sich um ein einzelnes Datum handeln.
    RangeEnd := TSDateNULL;
		if RangeStart = TSDateNULL then //Wenn das Datum ungültig ist...
			Result := rgInvalid
		else
			Result := rgSingleValue;
	end;
end;

//---------------------------------------------------------------------------------------
function ValidateDateRange(var ARange: RDateRange): Boolean;
var
  Original: RDateRange;
begin
  Original := ARange;
  if ARange.Date = TSDateNULL then
    ARange.Date := ARange.RangeDate;
  if ARange.Date = TSDateNULL then
    ARange.RangeType := rgInvalid
  else
    case ARange.RangeType of
      rgSingleValue, rgFromValue, rgToValue:
        ARange.RangeDate := TSDateNULL;
      rgFromToValue: begin
        if ARange.RangeDate = TSDateNULL then
          ARange.RangeType := rgSingleValue
        else if ARange.Date = ARange.RangeDate then
        begin
          ARange.RangeType := rgSingleValue;
          ARange.RangeDate := TSDateNULL;
        end
        else if ARange.Date > ARange.RangeDate then
          Switch(ARange.Date, ARange.RangeDate);
      end;
      else begin //rgInvalid
        ARange.Date := TSDateNULL;
        ARange.RangeDate := TSDateNULL;
      end;
    end;
  Result := (Original.Date = ARange.Date)
            and (Original.RangeDate = ARange.RangeDate)
            and (Original.RangeType = ARange.RangeType);
end;

//---------------------------------------------------------------------------------------
function DateRange(RangeString: string; out RangeStart, RangeEnd: TDateTime; const Defaults: RDateRange): NRangeType; overload;
var
	P: Integer;
	DStr: string;
begin
  RangeEnd := TSDateNULL;
	P := Pos(RangeSeparator, RangeString);
	if P > 0 then
	begin
		DStr := Copy(RangeString, 1, P-1);
		RangeStart := EvalDate(DStr, Defaults.Date);
		DStr := Copy(RangeString, P+Length(RangeSeparator), MaxInt);
		RangeEnd := EvalDate(DStr, Defaults.RangeDate);
  end
  else //Das Datum enthält keine ".." - es muss sich um ein einzelnes Datum handeln.
		RangeStart := EvalDate(RangeString, Defaults.Date);

  Result := DateTimeRange(P, RangeStart, RangeEnd);
end;

//---------------------------------------------------------------------------------------
function DateRange(RangeString: string; out RangeStart, RangeEnd: TDateTime): NRangeType; overload;
begin
  Result := DateRange(RangeString, RangeStart, RangeEnd, dateRangeNULL);
end;

//---------------------------------------------------------------------------------------
function DateRange(RangeString: string; const Defaults: RDateRange): RDateRange; overload;
var
  D1, D2: TDateTime;
begin
  Result.RangeType := DateRange(RangeString, D1, D2, Defaults);
  case Result.RangeType of
    rgSingleValue, rgFromValue, rgToValue: begin
      if D1 = TSDateNULL then
        Result.Date := D2
      else
        Result.Date := D1;
      Result.RangeDate := TSDateNULL;
    end;
    rgFromToValue: begin
      Result.Date := D1;
      Result.RangeDate := D2;
    end;
    else begin //rgInvalid:
      Result.Date := TSDateNULL;
      Result.RangeDate := TSDateNULL;
    end;
  end;
end;

//---------------------------------------------------------------------------------------
function DateRange(RangeString: string): RDateRange; overload;
begin
  Result := DateRange(RangeString, dateRangeNULL);
end;

//---------------------------------------------------------------------------------------
function DateRange(RangeString: string; out RangeStart, RangeEnd: string; Format: string): NRangeType;
var
  D1, D2: TDateTime;
begin
  D1 := 0;
  D2 := 0;
  Result := DateRange(RangeString, D1, D2);
  if Format = '' then
  begin
    RangeStart := DateToStr(D1);
    RangeEnd := DateToStr(D2);
  end
  else begin
    RangeStart := FormatDateTime(Format, D1);
    RangeEnd := FormatDateTime(Format, D2);
  end;
end;

//---------------------------------------------------------------------------------------
function DateRange(Date, RangeDate: TDateTime; RangeType: NRangeType): RDateRange;
begin
  Result.RangeType := RangeType;
  case RangeType of
    rgSingleValue, rgFromValue, rgToValue: begin
      Result.Date := Date;
      Result.RangeDate := TSDateNULL;
    end;
    rgFromToValue: begin
      Result.Date := Date;
      Result.RangeDate := RangeDate;
    end;
    else begin //rgInvalid:
      Result.Date := TSDateNULL;
      Result.RangeDate := TSDateNULL;
    end;
  end;
end;

//---------------------------------------------------------------------------------------
function FormatDateRange(RangeString: string; Format: string='ddddd'): string;
var
  D1, D2: TDateTime;
  RgType: NRangeType;
begin
  RgType := DateRange(RangeString, D1, D2);
  Result := RangeDisplay(RgType, FormatDateTime(Format, D1), FormatDateTime(Format, D2));
end;

//---------------------------------------------------------------------------------------
function FormatDateRange(Range: RDateRange; Format: string='ddddd'): string; overload;
begin
  Result := FormatDateTimeRange(Range, Format);
end;

//---------------------------------------------------------------------------------------
function RangeFilterDate(const FieldName, RangeStr: string; Format: string='"''"ddddd"''"'): string;
var
	D1, D2: TDateTime;
	RgType: NRangeType;
begin
	RgType := DateRange(RangeStr, D1, D2);
  Result := RangeFilter(FieldName, RgType, FormatDateTime(Format, D1),
									  					FormatDateTime(Format, D2));
end;

//---------------------------------------------------------------------------------------
function TimeRange(RangeString: string): RDateRange; overload;
begin
  Result.RangeType := TimeRange(RangeString, Result.Date, Result.RangeDate);
end;

//---------------------------------------------------------------------------------------
function TimeRange(RangeString: string; out RangeStart, RangeEnd: string; Format: string): NRangeType;
var
  T1, T2: TDateTime;
begin
  T1 := 0;
  T2 := 0;
  Result := TimeRange(RangeString, T1, T2);
  if Format = '' then
  begin
    RangeStart := TimeToStr(T1);
    RangeEnd := TimeToStr(T2);
  end
  else begin
    RangeStart := FormatDateTime(Format, T1);
    RangeEnd := FormatDateTime(Format, T2);
  end;
end;

//---------------------------------------------------------------------------------------
function TimeRange(RangeString: string; out RangeStart, RangeEnd: TDateTime): NRangeType;
var
	P: Integer;
	TStr: string;
begin
  RangeEnd := TSTimeNULL;
	P := Pos(RangeSeparator, RangeString);
	if P > 0 then
	begin
		TStr := Copy(RangeString, 1, P-1);
		RangeStart := EvalTime(TStr);
		TStr := Copy(RangeString, P+Length(RangeSeparator), MaxInt);
		RangeEnd := EvalTime(TStr);
  end
	else //Die Uhrzeit enthält keine ".." - es muss sich um eine einzelne Uhrzeit handeln.
		RangeStart := EvalTime(RangeString);

  if P > 0 then
  begin
		if RangeStart <> TSTimeNULL then //Vor den ".." steht eine gültige Zeit
		begin
			if RangeEnd <> TSTimeNULL then //... und hinter den ".." steht ebenfalls eine gültige Zeit
				Result := rgFromToValue
			else begin                     //... aber hinter den ".." steht keine gültige Zeit:
        RangeEnd := TSTimeNULL;
				Result := rgFromValue;
      end;
		end
		else if RangeEnd <> TSTimeNULL then //vor den ".." steht keine gültige Zeit, aber dahinter.
    begin
      RangeStart := TSTimeNULL;
			Result := rgToValue;
    end
		else begin  //Weder vor noch nach den ".." steht eine gültige Zeit.
      RangeStart := TSTimeNULL;
      RangeEnd := TSTimeNULL;
			Result := rgInvalid;
    end;
  end
  else begin
    RangeEnd := TSTimeNULL;
		if RangeStart = TSTimeNULL then //Wenn die Zeit ungültig ist...
			Result := rgInvalid
		else
			Result := rgSingleValue;
  end;
end;

//---------------------------------------------------------------------------------------
function FormatTimeRange(RangeString: string; Format: string='tt'): string;
var
	T1, T2: TDateTime;
	RgType: NRangeType;
begin
	RgType := TimeRange(RangeString, T1, T2);
  Result := RangeDisplay(RgType, FormatDateTime(Format, T1), FormatDateTime(Format, T2));
end;

//---------------------------------------------------------------------------------------
function FormatTimeRange(Range: RDateRange; Format: string='tt'): string; overload;
begin
  Result := FormatDateTimeRange(Range, Format);
end;

//---------------------------------------------------------------------------------------
function RangeFilterTime(const FieldName, RangeStr: string; Format: string='"''"tt"''"'): string;
var
	T1, T2: TDateTime;
	RgType: NRangeType;
begin
	RgType := TimeRange(RangeStr, T1, T2);
  Result := RangeFilter(FieldName, RgType, FormatDateTime(Format, T1),
									  					FormatDateTime(Format, T2));
end;

//---------------------------------------------------------------------------------------
function DateTimeRange(RangeString: string): RDateRange; overload;
begin
  Result.RangeType := DateTimeRange(RangeString, Result.Date, Result.RangeDate);
end;

//---------------------------------------------------------------------------------------
function DateTimeRange(RangeString: string; out RangeStart, RangeEnd: TDateTime): NRangeType;
var
	P: Integer;
	DStr: string;
begin
  RangeEnd := TSDateNULL;
	P := Pos(RangeSeparator, RangeString);
	if P > 0 then
	begin
		DStr := Copy(RangeString, 1, P-1);
		RangeStart := EvalDateTime(DStr);
		DStr := Copy(RangeString, P+Length(RangeSeparator), MaxInt);
		RangeEnd := EvalDateTime(DStr);
  end
  else //Das Datum enthält keine ".." - es muss sich um ein einzelnes Datum handeln.
		RangeStart := EvalDateTime(RangeString);
  Result := DateTimeRange(P, RangeStart, RangeEnd);
end;

//---------------------------------------------------------------------------------------
function DateTimeRange(RangeString: string; out RangeStart, RangeEnd: string; Format: string): NRangeType;
var
  D1, D2: TDateTime;
begin
  D1 := 0;
  D2 := 0;
  Result := DateTimeRange(RangeString, D1, D2);
  if Format = '' then
  begin
    RangeStart := DateTimeToStr(D1);
    RangeEnd := DateTimeToStr(D2);
  end
  else begin
    RangeStart := FormatDateTime(Format, D1);
    RangeEnd := FormatDateTime(Format, D2);
  end;
end;

//---------------------------------------------------------------------------------------
function FormatDateTimeRange(RangeString: string; Format: string='c'): string;
var
	D1, D2: TDateTime;
	RgType: NRangeType;
begin
	RgType := DateTimeRange(RangeString, D1, D2);
  Result := RangeDisplay(RgType, FormatDateTime(Format, D1), FormatDateTime(Format, D2));
end;

//---------------------------------------------------------------------------------------
function FormatDateTimeRange(Range: RDateRange; Format: string='c'): string; overload;
begin
  case Range.RangeType of
  	rgInvalid:
  		Result := '';
  	rgSingleValue:
  		Result := FormatDateTime(Format, Range.Date);
  	rgFromValue:
  		Result := FormatDateTime(Format, Range.Date) + RangeSeparator;
  	rgToValue:
  		Result := RangeSeparator + FormatDateTime(Format, Range.Date);
  	rgFromToValue:
  		Result := FormatDateTime(Format, Range.Date) + RangeSeparator
                  + FormatDateTime(Format, Range.RangeDate);
  end;
end;

//---------------------------------------------------------------------------------------
function RangeFilterDateTime(const FieldName, RangeStr: string; Format: string='"''"c"''"'): string;
var
	D1, D2: TDateTime;
	RgType: NRangeType;
begin
	RgType := DateTimeRange(RangeStr, D1, D2);
  Result := RangeFilter(FieldName, RgType, FormatDateTime(Format, D1),
									  					FormatDateTime(Format, D2));
end;



//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
{ TTSDateCalc }
//////////////////////////////////////////////////////////////////////////////

procedure TTSDateCalc.Change;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

constructor TTSDateCalc.Create;
begin
	inherited Create;
	fBase := ddFix;
	fFixDate := TSDateNULL;
	fOffsDays := 0;
	fOffsMonths := 0;
	fOffsYears := 0;
end;

function TTSDateCalc.GetDate: TDateTime;
begin
	Result := GetDateNoOffset;
	if (Result <> TSDateNULL)
    and ((OffsetYears <> 0)
  		or (OffsetMonths <> 0)
	  	or (OffsetDays <> 0)
      )
	then
		Result := DateAdd(Result, OffsetYears, OffsetMonths, OffsetDays);
end;

function TTSDateCalc.GetDateNoOffset: TDateTime;
var
	D,M,Y: Word;
begin
	case Base of
		ddToday:
			Result := Today;
		ddThisMonth: begin
			DecodeDate(Today, Y, M, D);
			Result := EncodeDate(Y, M, 1);
		end;
		ddThisYear: begin
			DecodeDate(Today, Y, M, D);
			Result := EncodeDate(Y, 1, 1);
		end;
		else //ddFix:
			Result := fFixDate;
	end;
end;

procedure TTSDateCalc.SetBase(const Value: NTSDateDefault);
begin
  fBase := Value;
  Change;
end;

procedure TTSDateCalc.SetFixDate(const Value: TDateTime);
begin
	fFixDate := Trunc(Value);
  Change;
end;

procedure TTSDateCalc.SetOffsDays(const Value: Integer);
begin
  fOffsDays := Value;
  Change;
end;

procedure TTSDateCalc.SetOffsMonths(const Value: Integer);
begin
  fOffsMonths := Value;
  Change;
end;

procedure TTSDateCalc.SetOffsYears(const Value: Integer);
begin
  fOffsYears := Value;
  Change;
end;

function TTSDateCalc.StoreFixDate: Boolean;
begin
  Result := fFixDate <> TSDateNULL;
end;

//////////////////////////////////////////////////////////////////////////////
{ TTSDateRange }
//////////////////////////////////////////////////////////////////////////////
constructor TTSDateRange.Create;
begin
  fRangeType := rgInvalid;
  fDate := TSDateNULL;
  fRangeDate := TSDateNULL;
end;

procedure TTSDateRange.SetDate(const Value: TDateTime);
begin
  fDate := Value;
  if Date = TSDateNULL then
  begin
    if RangeDate = TSDateNULL then
      fRangeType := rgInvalid
    else
      fRangeType := rgToValue;
  end
  else begin
    if RangeDate = TSDateNULL then
    begin
      if RangeType in [rgInvalid, rgSingleValue] then
        fRangeType := rgSingleValue
      else
        fRangeType := rgFromValue;
    end
    else if RangeDate > Date then
      fRangeType := rgFromToValue
    else begin
      fRangeDate := TSDateNULL;
      fRangeType := rgSingleValue;
    end;
  end;
end;

function TTSDateRange.GetRange: RDateRange;
begin
  Result.Date := fDate;
  Result.RangeDate := fRangeDate;
  Result.RangeType := fRangeType;
end;

procedure TTSDateRange.SetRange(const Value: RDateRange);
begin
  fDate := Value.Date;
  fRangeDate := Value.RangeDate;
  fRangeType := Value.RangeType;
end;

procedure TTSDateRange.SetRangeDate(const Value: TDateTime);
begin
  fRangeDate := Value;
  if RangeDate = TSDateNULL then
  begin
    if Date = TSDateNULL then
      fRangeType := rgInvalid
    else if RangeType in [rgInvalid, rgSingleValue] then
      fRangeType := rgSingleValue
    else
      fRangeType := rgFromValue;
  end
  else begin
    if Date = TSDateNULL then
      fRangeType := rgToValue
    else if Date = RangeDate then
    begin
      fRangeType := rgSingleValue;
      fRangeDate := TSDateNULL;
    end
    else begin
      fRangeType := rgFromToValue;
      if RangeDate < Date then
        Switch(fDate, fRangeDate);
    end;
  end;
end;

procedure TTSDateRange.SetRangeType(const Value: NRangeType);
begin
  fRangeType := Value;
  case RangeType of
    rgInvalid: begin
      fDate := TSDateNULL;
      fRangeDate := TSDateNULL;
    end;
    rgSingleValue:
      fRangeDate := TSDateNULL;
    rgFromValue:
      fRangeDate := TSDateNULL;
    rgToValue:
      fDate := TSDateNULL;
  end;
end;

function TTSDateRange.StoreDate: Boolean;
begin
  Result := fDate <> TSDateNULL;
end;

function TTSDateRange.StoreRangeDate: Boolean;
begin
  Result := fRangeDate <> TSDateNULL;
end;


//////////////////////////////////////////////////////////////////////////////
{ TTSDateRangeCalc }
//////////////////////////////////////////////////////////////////////////////
constructor TTSDateRangeCalc.Create;
begin
	inherited Create;
	fRangeMonths := 0;
	fRangeDays := 0;
	fRangeYears := 0;
  fBase := ddToday;
end;

destructor TTSDateRangeCalc.Destroy;
begin
  inherited Destroy;
end;

function TTSDateRangeCalc.GetRangeDate: TDateTime;
begin
	Result := Date;
	if (Result <> TSDateNULL)
    and ((RangeYears <> 0)
      or (RangeMonths <> 0)
      or (RangeDays <> 0)
      )
	then
		Result := DateAdd(Result, RangeYears, RangeMonths, RangeDays);
end;

function TTSDateRangeCalc.GetRange: RDateRange;
begin
  Result.Date := Date;
  Result.RangeDate := RangeDate;
  Result.RangeType := RangeType;
end;

procedure TTSDateRangeCalc.Change;
begin
  if Date = TSDateNULL then
    fRangeType := rgInvalid
  else if (fRangeDays=0) and (fRangeMonths=0) and (fRangeYears=0) then
    fRangeType := rgSingleValue
  else if RangeType = rgInvalid then
    fRangeType := rgSingleValue;

  inherited Change;
end;

function TTSDateRangeCalc.StoreRangeType: Boolean;
begin
  if (Date = TSDateNULL)
    or ((fRangeDays=0) and (fRangeMonths=0) and (fRangeYears=0))
  then
    Result := False
  else
    Result := fRangeType <> rgSingleValue;
end;

procedure TTSDateRangeCalc.SetRangeType(const Value: NRangeType);
begin
  fRangeType := Value;
  Change;
end;

procedure TTSDateRangeCalc.SetRangeDays(const Value: Integer);
begin
	fRangeDays := Value;
  Change;
end;

procedure TTSDateRangeCalc.SetRangeMonths(const Value: Integer);
begin
	fRangeMonths := Value;
  Change;
end;

procedure TTSDateRangeCalc.SetRangeYears(const Value: Integer);
begin
	fRangeYears := Value;
  Change;
end;



end.

