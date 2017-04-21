unit TS_Debug;
{$INCLUDE PackageDefs.inc}

interface

uses
  TS_StructuredFileName,
  sysutils,
  Classes, Buttons, StdCtrls, ExtCtrls, Dialogs,
  TS_Forms;

type
	NExceptionAction = (exActWriteLog, exActShowMessage, exActRaise, exActNetMessage);
	SExceptionAction = set of NExceptionAction;

  NLogOption = (loIncludeTimeStamp      //Beginnt jeden Protokolleintrag mit einem Zeitstempel.
                , loFileChangeNotifyOld //Bei Wechsel der Logdatei: schreibt einen Hinweis in die alte Datei.
                , loFileChangeNotifyNew //Bei Wechsel der Logdatei: schreibt einen Hinweis in die neue Datei.
                , loExceptionHeader     //Jeder Fehlereintrag beginnt mit dem Header-String
                , loExceptionFooter     //Jeder Fehlereintrag endet mit dem Footer-String
                , loEntryIndent         //Jede Zeile (außer Header, Footer und Timestamp) wird eingerückt.
                , loDispFileHint        //Im Meldungsfenster wird auf die Protokolldatei hingewiesen.
                , loAllExceptionsSlient //Wenn "HandleAllExceptions" aktiv, protokolliere ohne Fehleranzeige
                );
  SLogOptions = set of NLogOption;
  NLogTruncation = (truncOnCreate, truncOnDestroy, truncOnFileChange, truncOnLog, truncOnFirstLog);
  SLogTruncations = set of NLogTruncation;

	TFrmTSDebug = class(TTSForm)
		DlgSave: TSaveDialog;
		PnlBorder: TPanel;
		PnlExtMsg: TPanel;
		MeMsg: TMemo;
		PnlMemoBtns: TPanel;
		BtnSave: TSpeedButton;
		PnlBtns: TPanel;
		BtnClose: TBitBtn;
		PnlMsg: TPanel;
		LbMsg: TLabel;
		BtnImgDown: TSpeedButton;
		BtnImgUp: TSpeedButton;
		BtnUpDown: TSpeedButton;
		BtnMailTo: TSpeedButton;
    BtnCopy: TSpeedButton;
		procedure BtnCloseClick(Sender: TObject);
		procedure BtnSaveClick(Sender: TObject);
		procedure BtnMailToClick(Sender: TObject);
		procedure BtnUpDownClick(Sender: TObject);
		procedure TSFormShow(Sender: TObject);
		procedure TSFormKeyPress(Sender: TObject; var Key: Char);
    procedure BtnCopyClick(Sender: TObject);
	public
		MsgTxt: string;
		MailTo: string;
		MailSubject: string;
		MemoVisible: Boolean;
	end;

  TTSDebugLog = class(TComponent)
  private
    fLogFile: Textfile;
    fFileName: TTSStructuredFileName;
    fOptions: SLogOptions;
    fTruncate: SLogTruncations;
    fMaxLines: Integer;
    fPrepared: Boolean;
    fLogUsed: Boolean;
    fIndent: string;
    fHeader: string;
    fFooter: string;
    fMailSubject: string;
    fMailRecipient: string;
    fFileHint: string;
    fTmpOldFileName: string;
    fCaughtExMsg: string;
    function GetFileName: TTSStructuredFileName;
    procedure SetFileName(const Value: TTSStructuredFileName);
    procedure FileNameChanged(Sender: TObject; const OldLogFile, NewLogFile: string);
    function StoreOptions: Boolean;
    function StoreTrunc: Boolean;
    function StoreFooter: Boolean;
    function StoreHeader: Boolean;
    function StoreIndent: Boolean;
    function StoreFileHint: Boolean;
    function StoreMailRecipient: Boolean;
    function StoreMailSubject: Boolean;
    procedure SetHandleAllExceptions(const Value: Boolean);
    function StoreCaughtExMsg: Boolean;
    function GetHandleAllExceptions: Boolean;
    function GetFullPath: string;
    procedure SetFullPath(const Value: string);
  protected
    procedure Loaded; override;
    function PrepareLogFile: Boolean;
		procedure HandleAnyException(Sender: TObject; E: Exception);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Disp(E: Exception; Msg, Caption: string; Options: SLogOptions);
    procedure WriteLog(Msg: string);
    procedure LogSimple(Msg: string);

    procedure TruncateLogFile(KeepLines: Integer=-1; LogFileName: string='');
    property FullPath: string read GetFullPath write SetFullPath;

    procedure Log(Msg: string); overload;
    procedure Log(E: Exception; Msg: string); overload;
    procedure Log(Msg: string; Options: SLogOptions); overload;
    procedure Log(E: Exception; Msg: string; Options: SLogOptions); overload;

    procedure LogDisp(Msg: string); overload;
    procedure LogDisp(E: Exception; Msg: string); overload;
    procedure LogDisp(Msg: string; Options: SLogOptions); overload;
    procedure LogDisp(E: Exception; Msg: string; Options: SLogOptions); overload;
    procedure LogDisp(E: Exception; Msg, Caption: string; Options: SLogOptions); overload;

    property Prepared: Boolean read fPrepared;
    property Used: Boolean read fLogUsed;

  published
    property FileName: TTSStructuredFileName read GetFileName write SetFileName;
    property Options: SLogOptions read fOptions write fOptions stored StoreOptions;
    property Truncate: SLogTruncations read fTruncate write fTruncate stored StoreTrunc;
    property MaxLines: Integer read fMaxLines write fMaxLines default 5000;
    property Header: string read fHeader write fHeader stored StoreHeader;
    property Footer: string read fFooter write fFooter stored StoreFooter;
    property Indent: string read fIndent write fIndent stored StoreIndent;
    property LogFileHint: string read fFileHint write fFileHint stored StoreFileHint;
    property MailRecipient: string read fMailRecipient write fMailRecipient stored StoreMailRecipient;
    property MailSubject: string read fMailSubject write fMailSubject stored StoreMailSubject;
    property HandleAllExceptions: Boolean read GetHandleAllExceptions write SetHandleAllExceptions default True;
    property CaughtExceptionMessage: string read fCaughtExMsg write fCaughtExMsg stored StoreCaughtExMsg;
  end;

function TSLog: TTSDebugLog;

procedure GetExceptionInfo(E: Exception; InfoText: TStrings);

function GetApplicationFileVersion: string;
function GetApplicationFileVersionPart(PartNr: Byte): string;
function GetApplicationFileVersionParts(Part1, Part2, Part3, Part4: Boolean): string;
function GetApplicationBuildNr: string;

procedure Register;

implementation

{$R *.lfm}

uses
  {$IFDEF UseJCL}
  	JCLDebug,
  	JclHookExcept,
  	JCLStrings,
  	JCLMapi,
  {$ENDIF}
  LResources,
  Windows,
  ShellAPI,
  CompanyConstants,
  TSLib,
  TSLibFiles,
  TSLibObjects,
  TS_Application,
  TSResources,
  Controls,
  Forms;


const
	defaultMemoHeight = 350;
	defaultMemoWidth = 650;

  logOptionsDefault: SLogOptions = [loFileChangeNotifyOld, loIncludeTimeStamp,
                                  loExceptionHeader, loExceptionFooter, loEntryIndent];
  logTruncationDefault: SLogTruncations = [truncOnFirstLog];
  logHeaderDefault = #13#10'________________________________________________________________________________';
  logFooterDefault = '________________________________________________________________________________';
  logIndentDefault = #9;

var
  AppLog: TTSDebugLog;

procedure Register;
begin
	{$I TS_Debug.lrs}
  RegisterComponents(DefaultComponentPage, [TTSDebugLog]);
end;


//--------------------------------------------------------------------------------------------------
function TSLog: TTSDebugLog;
begin
  if AppLog = nil then
    AppLog := TTSDebugLog.Create(Application);
  Result := AppLog;
end;

//--------------------------------------------------------------------------------------------------
procedure GetExceptionInfo(E: Exception; InfoText: TStrings);
var
{$IFDEF UseJCL}
	StackList: TJclStackInfoList;
{$ENDIF}
	F: TForm;
	C: TControl;
begin
	if not (Assigned(E) and Assigned(InfoText)) then
		Exit;
	InfoText.Clear;

	InfoText.Add(Format(rsExMsg, [E.Message]));

	InfoText.Add(Format(rsExApplication, [ParamStr(0), GetApplicationFileVersion]));
	InfoText.Add(Format(rsExExceptionClass, [E.ClassName]));

	F := Screen.ActiveForm;
	if Assigned(F) then
  begin
		InfoText.Add(Format(rsExActiveForm, [F.Name, F.ClassName]));
		C := F.ActiveControl;
		if Assigned(C) then
  		InfoText.Add(Format(rsExActiveCtrl, [C.Name, C.ClassName]));
	end;

	if ExceptObject <> nil then
  begin
		if ExceptObject is TComponent then
			InfoText.Add(Format(rsExObjAsComp, [TComponent(ExceptObject).Name, ExceptObject.ClassName]))
		else
			InfoText.Add(Format(rsExObj, [ExceptObject.ClassName]));
	end;

	//Bei einem I/O-(z.B.Drucker-)fehler: Fehlernummer aufzeichnen.
	if E is EInOutError then
		with (E as EInOutError) do
  		InfoText.Add(Format(rsExIOError, [IntToStr(ErrorCode)]));

{$IFDEF UseJCL}
	InfoText.Add(rsExStack);
	StackList := JclLastExceptStackList;
  SL := TStringList.Create;
  try
  	StackList.AddToStrings(SL, True, True, True);
    IndentLines(SL);
    InfoText.AddStrings(SL);
  finally
    SL.Free;
  end;
{$ENDIF}
end;

//--------------------------------------------------------------------------------------------------
procedure ShowLogWin(Msg: string; ExtMsg: TStrings; Caption, MailTo, MailSubject: string);
var
	Frm: TFrmTSDebug;
begin
	Application.CreateForm(TFrmTSDebug, Frm);
	try
		Frm.Caption := Caption;
		Frm.MsgTxt := Msg;
		Frm.MailTo := MailTo;
		Frm.MailSubject := MailSubject;
		if Assigned(ExtMsg) and (ExtMsg.Count > 0) then
			Frm.MeMsg.Lines.Assign(ExtMsg)
		else
			Frm.MeMsg.Clear;
		Frm.ShowModal;
	finally
		Frm.Release;
	end;
end;


//***************************************************************************************
{ TTSDebugLog }
//***************************************************************************************

constructor TTSDebugLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPrepared := False;
  fLogUsed := False;
  fTmpOldFileName := '';
  fFileName := TTSStructuredFileName.Create;
  fFileName.Options := fFileName.Options + [fnoExtIsLog];
  fFileName.FullPathChange := @FileNameChanged;
  fMaxLines := 5000;
  fHeader := logHeaderDefault;
  fFooter := logFooterDefault;
  fIndent := logIndentDefault;
  fFileHint := rsLogFileHint;
  fMailSubject := rsLogMailSubject;
  fMailRecipient := TSApp.CompanyMailAddress;
  fOptions := logOptionsDefault;
  fTruncate := logTruncationDefault;
  fCaughtExMsg := rsExCaughtMessage;
end;

destructor TTSDebugLog.Destroy;
begin
  if truncOnDestroy in Truncate then
    TruncateLogFile;
  fFileName.Free;
  inherited;
end;

procedure TTSDebugLog.Assign(Source: TPersistent);
var
  Src: TTSDebugLog;
begin
  if not (Assigned(Source) and (Source is TTSDebugLog)) then
    inherited Assign(Source)
  else begin
    Src := TTSDebugLog(Source);
    fTmpOldFileName := '';
    fPrepared := False;
    fLogUsed := False;
    fFileName.Assign(Src.FileName);
    fMaxLines := Src.MaxLines;
    fHeader := Src.Header;
    fFooter := Src.Footer;
    fIndent := Src.Indent;
    fFileHint := Src.LogFileHint;
    fMailSubject := Src.MailSubject;
    fMailRecipient := Src.MailRecipient;
    fOptions := Src.Options;
    fTruncate := Src.Truncate;
  end;
end;

procedure TTSDebugLog.Loaded;
begin
  inherited Loaded;
  if (truncOnCreate in Truncate) then
    TruncateLogFile;
end;

procedure TTSDebugLog.SetFileName(const Value: TTSStructuredFileName);
begin
  fFileName.Assign(Value);
end;

procedure TTSDebugLog.SetFullPath(const Value: string);
begin
  FileName.FullPath := Value;
end;

procedure TTSDebugLog.HandleAnyException(Sender: TObject; E: Exception);
begin
  if loAllExceptionsSlient in Options then
    Log(E, fCaughtExMsg)
  else
    LogDisp(E, fCaughtExMsg);
end;

function TTSDebugLog.GetHandleAllExceptions: Boolean;
  function Same(M1, M2: TExceptionEvent): Boolean;
  begin
    Result := (TMethod(M1).Code = TMethod(M2).Code)
            and (TMethod(M1).Data = TMethod(M2).Data);
  end;
begin
  Result := Same(Application.OnException, @HandleAnyException);
end;

procedure TTSDebugLog.SetHandleAllExceptions(const Value: Boolean);
begin
	if Value then
		Application.OnException := @HandleAnyException
	else
		Application.OnException := nil;
end;

procedure TTSDebugLog.FileNameChanged(Sender: TObject; const OldLogFile, NewLogFile: string);
begin
  if (fTmpOldFileName='') and not SameText(fTmpOldFileName, NewLogFile) then
  begin
    fTmpOldFileName := OldLogFile;

    if Used and (truncOnFileChange in Truncate) then
    begin
      TruncateLogFile(-1, fTmpOldFileName);
      TruncateLogFile;
    end;
  end;
end;

function TTSDebugLog.GetFileName: TTSStructuredFileName;
begin
  Result := fFileName;
end;

function TTSDebugLog.GetFullPath: string;
begin
  Result := FileName.FullPath;
end;

procedure TTSDebugLog.LogSimple(Msg: string);
begin
  Log(nil, Msg, []);
end;

function TTSDebugLog.PrepareLogFile: Boolean;
begin
  if not Prepared then
	try
		AssignFile(fLogFile, FileName.FullPath);
		try
			//Falls I/O-Prüfung (Compiler-Optionen) ausgeschaltet ist, löst Append KEINEN
			//  Fehler aus, wenn die Datei nicht exisitiert. Deshalb muss vor dem "Append"-Versuch
      //  geprüft werden, ob die Datei existiert.
			if FileExists(FileName.FullPath) then
				Append(fLogFile)
			else
				Rewrite(fLogFile);
		except
			Rewrite(fLogFile);
		end;
		CloseFile(fLogFile);
    fPrepared := True;
		Result := True;
	except
		Result := False;
	end
  else begin
    if not FileExists(FileName.FullPath) then
      Rewrite(fLogFile);
    Result := True;
  end;
end;

procedure TTSDebugLog.Log(Msg: string);
begin
  Log(nil, Msg, fOptions);
end;

procedure TTSDebugLog.Log(E: Exception; Msg: string);
begin
  Log(E, Msg, fOptions);
end;

procedure TTSDebugLog.Log(Msg: string; Options: SLogOptions);
begin
  Log(nil, Msg, Options);
end;

procedure TTSDebugLog.WriteLog(Msg: string);
begin
  Append(fLogFile);
  try
    WriteLn(fLogFile, Msg);
    fLogUsed := True;
  finally
    try
      CloseFile(fLogFile);
    except
    end;
  end;
end;

procedure TTSDebugLog.Log(E: Exception; Msg: string; Options: SLogOptions);
var
  P: Integer;
  Txt: string;
  SL: TStringList;
begin
  //Protokolldatei wurde nach dem Programmstart bereits verwendet und der
  //  Dateiname wurde inzwischen geändert.
  //  Wenn gewünscht (Options), schreib in die ALTE Datei einen Hinweis auf diese
  //  Änderung.
  if fLogUsed and (fTmpOldFileName > '') then
  begin
    if (loFileChangeNotifyOld in Options) and (rsLogChangingFile > '') then
      WriteLog(Format(rsLogChangingFile, [fFileName.FullPath]));
    fPrepared := False; //Hinweis für PrepareLogFile, um die neue Protokolldatei zu initalisieren.
  end;

  if not PrepareLogFile then
  begin
    Disp(E, rsLogPrepareFailed + #13#10 + Msg, rsLogInternalError, []);
    Exit;
  end;

  //Protokolldatei wurde nach dem Programmstart bereits verwendet und der
  //  Dateiname wurde inzwischen geändert.
  //  Wenn gewünscht (Options), schreib in die NEUE Datei einen Hinweis auf diese
  //  Änderung.
  if Used and (fTmpOldFileName > '') then
  begin
    if (loFileChangeNotifyNew in Options) and (rsLogFileChanged > '') then
      WriteLog(Format(rsLogFileChanged, [fTmpOldFileName]));
    fTmpOldFileName := '';
  end;
  if (truncOnLog in Truncate) or (not fLogUsed and (truncOnFirstLog in Truncate)) then
    TruncateLogFile;

  if loIncludeTimeStamp in Options then
    Txt := DateTimeToStr(Now) + ': ' + Msg
  else
    Txt := Msg;

  if Assigned(E) then
  begin
    SL := TStringList.Create;
    try
      GetExceptionInfo(E, SL);
      Txt := Txt + #13#10 + SL.Text;
    finally
      SL.Free;
    end;
  end;

  P := Length(Txt)-1;
  if Copy(Txt, P, 2) = #13#10 then
    Delete(Txt, P, 2);

  if (loEntryIndent in Options) and (Indent > '') then
    Txt := IndentLines(Txt);

  if Assigned(E) and (loExceptionHeader in Options) and (Header > '') then
    Txt := Header + #13#10 + Txt;

  if Assigned(E) and (loExceptionFooter in Options) and (Footer > '') then
	  Txt := Txt + #13#10 + Footer;

  WriteLog(Txt);
end;

procedure TTSDebugLog.Disp(E: Exception; Msg, Caption: string; Options: SLogOptions);
var
	SL: TStringList;
begin
	if (loDispFileHint in Options) and (LogFileHint > '') then
		Msg := Msg + #13#10 + Format(rsLogFileHint, []);

	if Assigned(E) then begin
		SL := TStringList.Create;
		try
			GetExceptionInfo(E, SL);
			ShowLogWin(Msg, SL, Caption, MailRecipient, Format(MailSubject, [CleanupFileName(ParamStr(0))]));
		finally
			SL.Free;
		end;
	end
	else
		ShowLogWin(Msg, nil, '', '', '');
end;

procedure TTSDebugLog.LogDisp(E: Exception; Msg: string);
begin
  LogDisp(E, Msg, fOptions);
end;

procedure TTSDebugLog.LogDisp(Msg: string);
begin
  LogDisp(nil, Msg, fOptions);
end;

procedure TTSDebugLog.LogDisp(Msg: string; Options: SLogOptions);
begin
  LogDisp(nil, Msg, Options);
end;

procedure TTSDebugLog.LogDisp(E: Exception; Msg: string; Options: SLogOptions);
begin
  LogDisp(E, Msg, rsExCaption, Options);
end;

procedure TTSDebugLog.LogDisp(E: Exception; Msg, Caption: string; Options: SLogOptions);
begin
  Log(E, Msg, Options);
  Disp(E, Msg, Caption, Options);
end;

procedure TTSDebugLog.TruncateLogFile(KeepLines: Integer; LogFileName: string);
var
	F: Textfile;
	I, N: Integer;
  FN: string;
  DoTruncate: Boolean;
	Line: array of string;
begin
  if KeepLines < 0 then
    KeepLines := MaxLines;
	if KeepLines < 0 then
		Exit;
  if LogFileName > '' then
    FN := LogFileName
  else
    FN := FileName.FullPath;
	if (FN = '') or not FileExists(FN) then
		Exit;

  if KeepLines > 0 then
  begin
    DoTruncate := False;
    AssignFile(F, FN);
    I := 0;
    SetLength(Line, KeepLines);
    Reset(F);
    try
      while not EOF(F) do begin
        Readln(F, Line[I]);
        I := (I+1) mod KeepLines;
        if I = 0 then
          DoTruncate := True;
      end
    finally
      CloseFile(F);
    end;

    if DoTruncate then begin
      N := I;
      Rewrite(F);
      try
        repeat
          Writeln(F, Line[I]);
          I := (I+1) mod KeepLines;
        until I = N;
      finally
        CloseFile(F);
      end;
    end;
  end
  else begin
    Rewrite(F);
    CloseFile(F);
  end;
end;

function TTSDebugLog.StoreHeader: Boolean;
begin
  Result := fHeader <> logHeaderDefault;
end;

function TTSDebugLog.StoreFooter: Boolean;
begin
  Result := fFooter <> logFooterDefault;
end;

function TTSDebugLog.StoreIndent: Boolean;
begin
  Result := fIndent <> logIndentDefault;
end;

function TTSDebugLog.StoreOptions: Boolean;
begin
  Result := fOptions <> logOptionsDefault;
end;

function TTSDebugLog.StoreTrunc: Boolean;
begin
  Result := fTruncate <> logTruncationDefault;
end;

function TTSDebugLog.StoreCaughtExMsg: Boolean;
begin
  Result := fCaughtExMsg <> rsExCaughtMessage;
end;

function TTSDebugLog.StoreFileHint: Boolean;
begin
  Result := LogFileHint <> rsLogFileHint;
end;

function TTSDebugLog.StoreMailRecipient: Boolean;
begin
  Result := MailRecipient <> TSApp.CompanyMailAddress;
end;

function TTSDebugLog.StoreMailSubject: Boolean;
begin
  Result := MailSubject <> rsLogMailSubject;
end;


//
//***************************************************************************************
//***************************************************************************************
{ TFrmTSDebug }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
procedure TFrmTSDebug.TSFormShow(Sender: TObject);
begin
	PnlExtMsg.Hide;
	LbMsg.Caption := MsgTxt;
	MemoVisible := False;
	Width := MaxVal(Screen.Width div 2, 450);

	ClientHeight := PnlBorder.BorderWidth * 2 + PnlMsg.Height + PnlBtns.Height;
	if MeMsg.Lines.Count > 0 then begin
		BtnUpDown.Glyph.Assign(BtnImgDown.Glyph);
		BtnUpDown.Show;
	end
	else
		BtnUpDown.Hide;

  BtnMailTo.Visible := MailTo > '';
	TryFocus(BtnClose);
end;

//***************************************************************************************
procedure TFrmTSDebug.BtnUpDownClick(Sender: TObject);
begin
	if MeMsg.Lines.Count>0 then
		MemoVisible := not MemoVisible
	else
		MemoVisible := False;

	if MemoVisible then begin
		BtnUpDown.Glyph.Assign(BtnImgUp.Glyph);
		PnlExtMsg.Show;
		ClientHeight := LbMsg.Height + PnlBtns.Height + defaultMemoHeight;
		if ClientWidth < defaultMemoWidth then
			ClientWidth := defaultMemoWidth;
		Left := (Screen.Width - Width) div 2;
		Top := (Screen.Height - Height) div 2;
	end
	else begin
		BtnUpDown.Glyph.Assign(BtnImgDown.Glyph);
		ClientHeight := PnlBorder.BorderWidth *2 + PnlMsg.Height + PnlBtns.Height;
		PnlExtMsg.Hide;
	end
end;

//***************************************************************************************
procedure TFrmTSDebug.BtnCloseClick(Sender: TObject);
begin
	Close;
end;

//***************************************************************************************
procedure TFrmTSDebug.TSFormKeyPress(Sender: TObject; var Key: Char);
begin
  case AnsiChar(Key) of
    #10, #27, #13:
      Close;
    ^C:
      BtnCopy.Click;
    ^S:
      BtnSave.Click;
    ^E:
      BtnMailTo.Click;
  end;
end;

//***************************************************************************************
procedure TFrmTSDebug.BtnSaveClick(Sender: TObject);
var
	SL: TStringList;
begin
	if DlgSave.Execute then
	begin
		SL := TStringList.Create;
		try
			SL.Add(LbMsg.Caption);
			SL.AddStrings(MeMsg.Lines);
			SL.SaveToFile(DlgSave.FileName);
		finally
			SL.Free;
		end;
	end;
end;

//***************************************************************************************
procedure TFrmTSDebug.BtnCopyClick(Sender: TObject);
begin
  MeMsg.Lines.Insert(0, LbMsg.Caption);
  MeMsg.SelectAll;
  MeMsg.CopyToClipboard;
  MeMsg.Lines.Delete(0);
  MeMsg.SelLength := 0;
end;

//***************************************************************************************
procedure TFrmTSDebug.BtnMailToClick(Sender: TObject);

	function HTMLReplace(S: string): string;
	var
		I: Integer;
		N: Byte;
	begin
		Result := '';
		for I := 1 to Length(S) do
		begin
			N := Ord(S[I]);
			if N in [$01..$2F, $3A..$40] then
				Result := Result + '%' + IntToHex(N, 2)
			else
				Result := Result + S[I];
		end;
	end;

var
  Hdr, Txt: string;
  I: Integer;

begin
{$IFDEF UseJCL}
	with TJclEmail.Create do
	try
		ParentWnd := 0;
		Recipients.Add(AnsiString(MailTo));
		Subject := AnsiString(MailSubject);
		Body := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ':'#13#10 + MeMsg.Text;
		SaveTaskWindows;
		try
 			Send(True);
		finally
			RestoreTaskWindows;
		end;
	finally
		Free;
	end;
{$ELSE}
	Hdr := 'mailto:' + MailTo + '?subject=' + HTMLReplace(MailSubject);
	if MeMsg.Lines.Count > 0 then begin
		Hdr := Hdr + '&body=';
		Txt := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ':'#13#10;
		for I := 0 to MeMsg.Lines.Count - 1 do
			Txt := Txt + MeMsg.Lines[I] + #10;
	end;
	Txt := Hdr + HTMLReplace(Txt);
	ShellExecute(0, 'Open', PChar(Txt), nil, nil, SW_Normal);
{$ENDIF}
end;

//***************************************************************************************
function GetApplicationFileVersion: string;
begin
	Result := GetFileVersionParts(Application.ExeName, True, True, True, True);
end;

//***************************************************************************************
function GetApplicationFileVersionPart(PartNr: Byte): string;
begin
	case PartNr of
	1:
		Result := GetFileVersionParts(Application.ExeName, True, False, False, False);
	2:
		Result := GetFileVersionParts(Application.ExeName, False, True, False, False);
	3:
		Result := GetFileVersionParts(Application.ExeName, False, False, True, False);
	4:
		Result := GetFileVersionParts(Application.ExeName, False, False, False, True);
	else
		Result := GetFileVersionParts(Application.ExeName, True, True, True, True);
	end;
end;

//***************************************************************************************
function GetApplicationBuildNr: string;
begin
	Result := GetFileVersionParts(Application.ExeName, False, False, False, True);
end;

//***************************************************************************************
function GetApplicationFileVersionParts(Part1, Part2, Part3, Part4: Boolean): string;
begin
	Result := GetFileVersionParts(Application.ExeName, Part1, Part2, Part3, Part4);
end;

//
//==================================================================================================
// Exception handler initialization code
//==================================================================================================

//--------------------------------------------------------------------------------------------------
procedure InitializeHandler;
begin
{$IFDEF UseJCL}
	JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];
	JclStartExceptionTracking;
{$ENDIF}
end;

//--------------------------------------------------------------------------------------------------
procedure UnInitializeHandler;
begin
{$IFDEF UseJCL}
	JclUnhookExceptions;
	JclStopExceptionTracking;
{$ENDIF}
end;


initialization
	InitializeHandler;

finalization
	UnInitializeHandler;


end.


