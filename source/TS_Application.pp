unit TS_Application;

{$mode objfpc}{$H+}

interface

uses
  TS_Debug,
  TS_StructuredFileName,
  TS_FileINI,
  Graphics,
  Classes,
  Forms,
  ActnList,
	SysUtils;

type

	{ TTSApplication }

 TTSApplication = class(TComponent)
	private
		fShowHint: Boolean;
		function GetAppName: string;
		function GetHint: string;
		function GetHintColor: TColor;
		function GetHintHidePause: Integer;
		function GetHintHidePausePerChar: Integer;
		function GetHintPause: Integer;
		function GetHintShortCuts: Boolean;
		function GetHintShortPause: Integer;
		function GetShowHint: Boolean;
		function IsStoredAppName: Boolean;
		procedure SetAppName(AValue: string);
	  function IsStoredCompanyName: Boolean;
	  function GetOnActionExecute: TActionEvent;
	  function GetOnActionUpdate: TActionEvent;
	  function GetOnActivate: TNotifyEvent;
	  function GetAppINI: TTSFileINI;
	  function GetCompanyName: string;
	  function GetOnDeactivate: TNotifyEvent;
	  function GetOnException: TExceptionEvent;
	  function GetCommonINI: TTSFileINI;
	  function GetOnHelp: THelpEvent;
	  function GetOnHint: TNotifyEvent;
	  function GetOnIdle: TIdleEvent;
	  function GetConnectedFiles: TTSStructuredFileName;
	  function GetINIValue(Lookup: string): string;
	  function GetOnMinimize: TNotifyEvent;
	  function GetOnModalBegin: TNotifyEvent;
	  function GetOnModalEnd: TNotifyEvent;
	  function GetOnRestore: TNotifyEvent;
	  function GetOnShortCut: TShortCutEvent;
	  function GetOnShowHint: TShowHintEvent;
	  function GetStorageActive: Boolean;
		procedure SetHint(const aValue: string);
		procedure SetHintColor(const aValue: TColor);
		procedure SetHintHidePause(const aValue: Integer);
		procedure SetHintHidePausePerChar(const aValue: Integer);
		procedure SetHintPause(const aValue: Integer);
		procedure SetHintShortCuts(const aValue: Boolean);
		procedure SetHintShortPause(const aValue: Integer);
	  procedure SetOnActionExecute(const Value: TActionEvent);
	  procedure SetOnActionUpdate(const Value: TActionEvent);
	  procedure SetOnActivate(const Value: TNotifyEvent);
	  procedure SetCompanyName(const Value: string);
	  procedure SetOnDeactivate(const Value: TNotifyEvent);
	  procedure SetOnException(const Value: TExceptionEvent);
	  procedure SetOnHelp(const Value: THelpEvent);
	  procedure SetOnHint(const Value: TNotifyEvent);
	  procedure SetOnIdle(const Value: TIdleEvent);
	  procedure SetConnectedFiles(const Value: TTSStructuredFileName);
	  procedure SetINIValue(Lookup: string; const Value: string);
	  procedure SetOnMinimize(const Value: TNotifyEvent);
	  procedure SetOnModalBegin(const Value: TNotifyEvent);
	  procedure SetOnModalEnd(const Value: TNotifyEvent);
	  procedure SetOnRestore(const Value: TNotifyEvent);
	  procedure SetOnShortCut(const Value: TShortCutEvent);
	  procedure SetOnShowHint(const Value: TShowHintEvent);
		procedure SetShowHint(const aValue: Boolean);
	  procedure SetStorageActive(const Value: Boolean);
	  function GetLogFile: TTSDebugLog;
	  procedure SetLogFile(const Value: TTSDebugLog);
	  function GetCompanyMail: string;
	  procedure SetCompanyMail(const Value: string);
	  function IsStoredCompanyMail: Boolean;
	  function GetLogFileName: TTSStructuredFileName;
	  procedure SetLogFileName(const Value: TTSStructuredFileName);
  protected
  	procedure Loaded; override;
	published
		//Die folgenden Properties und Events sind direkt von TApplication übenommen. Alle Lese-
		//	und Schreibvorgänge werden an das globale Application-Objekt weitergereicht.

    property Hint: string read GetHint write SetHint;
    property HintColor: TColor read GetHintColor write SetHintColor;
    property HintHidePause: Integer read GetHintHidePause write SetHintHidePause;
    property HintHidePausePerChar: Integer read GetHintHidePausePerChar write SetHintHidePausePerChar;
    property HintPause: Integer read GetHintPause write SetHintPause;
    property HintShortCuts: Boolean read GetHintShortCuts write SetHintShortCuts;
    property HintShortPause: Integer read GetHintShortPause write SetHintShortPause;
    property ShowHint: Boolean read GetShowHint write SetShowHint;

		property OnActionExecute: TActionEvent read GetOnActionExecute write SetOnActionExecute;
		property OnActionUpdate: TActionEvent read GetOnActionUpdate write SetOnActionUpdate;
		property OnActivate: TNotifyEvent read GetOnActivate write SetOnActivate;
		property OnDeactivate: TNotifyEvent read GetOnDeactivate write SetOnDeactivate;
		property OnException: TExceptionEvent read GetOnException write SetOnException;
		property OnIdle: TIdleEvent read GetOnIdle write SetOnIdle;
		property OnHelp: THelpEvent read GetOnHelp write SetOnHelp;
		property OnHint: TNotifyEvent read GetOnHint write SetOnHint;
		property OnMinimize: TNotifyEvent read GetOnMinimize write SetOnMinimize;
		property OnModalBegin: TNotifyEvent read GetOnModalBegin write SetOnModalBegin;
		property OnModalEnd: TNotifyEvent read GetOnModalEnd write SetOnModalEnd;
		property OnRestore: TNotifyEvent read GetOnRestore write SetOnRestore;
		property OnShowHint: TShowHintEvent read GetOnShowHint write SetOnShowHint;
		property OnShortCut: TShortCutEvent read GetOnShortCut write SetOnShortCut;
	public
	  procedure Log(Msg: string); overload;
	  procedure Log(E: Exception; Msg: string); overload;

	  property INI: TTSFileINI read GetAppINI;
	  property Storage: TTSFileINI read GetAppINI; //an Alias for "INI"
	  property INIValue[Lookup: string]: string read GetINIValue write SetINIValue; default;
	  property CommonINI: TTSFileINI read GetCommonINI;

	  property LogFile: TTSDebugLog read GetLogFile write SetLogFile;
	published
	  property CompanyName: string read GetCompanyName write SetCompanyName stored IsStoredCompanyName;
	  property CompanyMailAddress: string read GetCompanyMail write SetCompanyMail stored IsStoredCompanyMail;
    property ApplicationName: string read GetAppName write SetAppName stored IsStoredAppName;

	  //ConnectedFiles definiert Dateinamen und -pfad für mit der Anwendung vernküpften Dateien.
	  //  Alle TSFileINI-Komponenten greifen auf diesen Eintrag zurück um Standards für
	  //  INI-Dateien zu erhalten.
	  property ConnectedFiles: TTSStructuredFileName read GetConnectedFiles write SetConnectedFiles;
	  property LogFileName: TTSStructuredFileName read GetLogFileName write SetLogFileName;

	  //PropertyStorageActive kann von Komponenten verwendet werden, um zu prüfen, ob sie
	  //  ihre Eigenschaften (z.B. Fenster- / Splitterposition oder Spaltenbreite)
	  //  in die Storage / INI schreiben sollen.
	  property PropertyStorageActive: Boolean read GetStorageActive write SetStorageActive default True;
	end;


function TSApp: TTSApplication;
function AppINI: TTSFileINI;
function MyCompanyName: string;
function ApplicationName: string;

procedure Register;

implementation

uses
  LResources,
  TSResources,
  TSLibFiles,
	CompanyConstants;

type
  //Zwischenspeicher für die Application-Ereignisse.
  TAppEventStorage = class(TObject)
    Hint: string;
    HintColor: TColor;
    HintHidePause: Integer;
    HintHidePausePerChar: Integer;
    HintPause: Integer;
    HintShortCuts: Boolean;
    HintShortPause: Integer;
    ShowHint: Boolean;

    OnActionExecute: TActionEvent;
    OnActionUpdate: TActionEvent;
    OnActivate: TNotifyEvent;
    OnDeactivate: TNotifyEvent;
    OnException: TExceptionEvent;
    OnIdle: TIdleEvent;
    OnHelp: THelpEvent;
    OnHint: TNotifyEvent;
    OnMinimize: TNotifyEvent;
    OnModalBegin: TNotifyEvent;
    OnModalEnd: TNotifyEvent;
    OnRestore: TNotifyEvent;
    OnShowHint: TShowHintEvent;
    OnShortCut: TShortCutEvent;
  end;

var
  InternalTSApp: TTSApplication=nil;
	AppName: string;
  AppMyCompanyName: string = DefaultCompanyName;
  AppMyCompanyMail: string = DefaultCompanyMailAddress;
  AppCommonINI: TTSFileINI = nil;
  AppFileINI: TTSFileINI = nil;
  AppStorageActive: Boolean = True;
  AppConnectedFiles: TTSStructuredFileName;
  AppEventStorage: TAppEventStorage = nil;

procedure Register;
begin
  {$I TS_Application.lrs}
  RegisterComponents(DefaultComponentPage, [TTSApplication]);
end;


function MyCompanyName: string;
begin
  Result := AppMyCompanyName;
end;

function ApplicationName: string;
begin
	Result := AppName;
end;

function TSApp: TTSApplication;
begin
  if not Assigned(InternalTSApp) then
    InternalTSApp := TTSApplication.Create(nil);
  Result := InternalTSApp;
end;

function AppINI: TTSFileINI;
begin
  Result := TSApp.INI;
end;



//***************************************************************************************
//***************************************************************************************
{ TSApplication }
//***************************************************************************************
//***************************************************************************************

function TTSApplication.GetCompanyName: string;
begin
  Result := AppMyCompanyName;
end;


procedure TTSApplication.SetCompanyName(const Value: string);
begin
  AppMyCompanyName := Value;
end;

function TTSApplication.GetAppName: string;
begin
  Result := AppName;
end;

function TTSApplication.IsStoredAppName: Boolean;
begin
	Result := (AppName>'') and not SameText(AppName, CleanupFileName(Application.ExeName));
end;

procedure TTSApplication.SetAppName(AValue: string);
begin
  AppName := AValue;
end;

function TTSApplication.IsStoredCompanyName: Boolean;
begin
  Result := AppMyCompanyName <> DefaultCompanyName;
end;

function TTSApplication.GetCompanyMail: string;
begin
  Result := AppMyCompanyMail;
end;

procedure TTSApplication.SetCompanyMail(const Value: string);
begin
  AppMyCompanyMail := Value;
end;

function TTSApplication.IsStoredCompanyMail: Boolean;
begin
  Result := AppMyCompanyMail <> DefaultCompanyMailAddress;
end;

function TTSApplication.GetConnectedFiles: TTSStructuredFileName;
begin
  Result := AppConnectedFiles;
end;

procedure TTSApplication.SetConnectedFiles(const Value: TTSStructuredFileName);
begin
  AppConnectedFiles.Assign(Value);
end;

//***************************************************************************************
//  TSApplication: Methoden der Storage/INI-Elemente
//***************************************************************************************

function TTSApplication.GetCommonINI: TTSFileINI;
begin
  if not Assigned(AppCommonINI) then
  begin
    AppCommonINI := TTSFileINI.Create(nil);
    AppCommonINI.INIFile.BaseDirectory := sdirCmnAppData;
    AppCommonINI.INIFile.Options := AppCommonINI.INIFile.Options + [fnoSubDirIsCompanyName];
    AppCommonINI.INIFile.FileName := rsCompanyINIFile;
  end;
  Result := AppCommonINI;
end;

function TTSApplication.GetAppINI: TTSFileINI;
begin
  if not Assigned(AppFileINI) then
    AppFileINI := TTSFileINI.Create(nil);
  Result := AppFileINI;
end;

function TTSApplication.GetINIValue(Lookup: string): string;
begin
  Result := INI.Str[Lookup];
end;

procedure TTSApplication.SetINIValue(Lookup: string; const Value: string);
begin
  INI.Str[Lookup] := Value;
end;

function TTSApplication.GetStorageActive: Boolean;
begin
  Result := AppStorageActive;
end;


procedure TTSApplication.SetStorageActive(const Value: Boolean);
begin
  AppStorageActive := Value;
end;


//***************************************************************************************
//  TSApplication: Methoden der Log-/Protokoll-Elemente
//***************************************************************************************

function TTSApplication.GetLogFile: TTSDebugLog;
begin
  Result := TSLog;
end;

procedure TTSApplication.SetLogFile(const Value: TTSDebugLog);
begin
  TSLog.Assign(Value);
end;

function TTSApplication.GetLogFileName: TTSStructuredFileName;
begin
  Result := TSLog.FileName;
end;

procedure TTSApplication.SetLogFileName(const Value: TTSStructuredFileName);
begin
  TSLog.FileName := Value;
end;

procedure TTSApplication.Loaded;
begin
	inherited Loaded;

end;

procedure TTSApplication.Log(E: Exception; Msg: string);
begin
  TSLog.Log(E, Msg);
end;

procedure TTSApplication.Log(Msg: string);
begin
  TSLog.Log(Msg);
end;


//***************************************************************************************
//  TSApplication: Weitergabe der Ereignisse and die übergeordneten TApplication-Ereignisse
//***************************************************************************************

function TTSApplication.GetHint: string;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.Hint
    else
      Result := Application.Hint;
  end
  else
  	Result := Application.Hint;
end;

function TTSApplication.GetHintColor: TColor;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.HintColor
    else
      Result := Application.HintColor;
  end
  else
  	Result := Application.HintColor;
end;

function TTSApplication.GetHintHidePause: Integer;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.HintHidePause
    else
      Result := Application.HintHidePause;
  end
  else
  	Result := Application.HintHidePause;
end;

function TTSApplication.GetHintHidePausePerChar: Integer;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.HintHidePausePerChar
    else
      Result := Application.HintHidePausePerChar;
  end
  else
  	Result := Application.HintHidePausePerChar;
end;

function TTSApplication.GetHintPause: Integer;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.HintPause
    else
      Result := Application.HintPause;
  end
  else
  	Result := Application.HintPause;
end;

function TTSApplication.GetHintShortCuts: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.HintShortCuts
    else
      Result := Application.HintShortCuts;
  end
  else
  	Result := Application.HintShortCuts;
end;

function TTSApplication.GetHintShortPause: Integer;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.HintShortPause
    else
      Result := Application.HintShortPause;
  end
  else
  	Result := Application.HintShortPause;
end;

function TTSApplication.GetShowHint: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.ShowHint
    else
      Result := Application.ShowHint;
  end
  else
  	Result := Application.ShowHint;
end;

function TTSApplication.GetOnActionExecute: TActionEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnActionExecute
    else
      Result := nil;
  end
  else
  	Result := Application.OnActionExecute;
end;

function TTSApplication.GetOnActionUpdate: TActionEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnActionUpdate
    else
      Result := nil;
  end
  else
  	Result := Application.OnActionUpdate;
end;

function TTSApplication.GetOnActivate: TNotifyEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnActivate
    else
      Result := nil;
  end
  else
  	Result := Application.OnActivate;
end;

function TTSApplication.GetOnDeactivate: TNotifyEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnDeactivate
    else
      Result := nil;
  end
  else
  	Result := Application.OnDeactivate;
end;

function TTSApplication.GetOnException: TExceptionEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnException
    else
      Result := nil;
  end
  else
  	Result := Application.OnException;
end;

function TTSApplication.GetOnIdle: TIdleEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnIdle
    else
      Result := nil;
  end
  else
  	Result := Application.OnIdle;
end;

function TTSApplication.GetOnHelp: THelpEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnHelp
    else
      Result := nil;
  end
  else
  	Result := Application.OnHelp;
end;

function TTSApplication.GetOnHint: TNotifyEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnHint
    else
      Result := nil;
  end
  else
  	Result := Application.OnHint;
end;

function TTSApplication.GetOnMinimize: TNotifyEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnMinimize
    else
      Result := nil;
  end
  else
  	Result := Application.OnMinimize;
end;

function TTSApplication.GetOnModalBegin: TNotifyEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnModalBegin
    else
      Result := nil;
  end
  else
  	Result := Application.OnModalBegin;
end;

function TTSApplication.GetOnModalEnd: TNotifyEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnModalEnd
    else
      Result := nil;
  end
  else
  	Result := Application.OnModalEnd;
end;

function TTSApplication.GetOnRestore: TNotifyEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnRestore
    else
      Result := nil;
  end
  else
  	Result := Application.OnRestore;
end;

function TTSApplication.GetOnShowHint: TShowHintEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnShowHint
    else
      Result := nil;
  end
  else
	  Result := Application.OnShowHint;
end;

function TTSApplication.GetOnShortCut: TShortCutEvent;
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      Result := AppEventStorage.OnShortCut
    else
      Result := nil;
  end
  else
  	Result := Application.OnShortCut;
end;



procedure TTSApplication.SetHint(const aValue: string);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.Hint := aValue
  end
  else
  	Application.Hint := aValue;
end;

procedure TTSApplication.SetHintColor(const aValue: TColor);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.HintColor := aValue
  end
  else
  	Application.HintColor := aValue;
end;

procedure TTSApplication.SetHintHidePause(const aValue: Integer);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.HintHidePause := aValue
  end
  else
  	Application.HintHidePause := aValue;
end;

procedure TTSApplication.SetHintHidePausePerChar(const aValue: Integer);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.HintHidePausePerChar := aValue
  end
  else
  	Application.HintHidePausePerChar := aValue;
end;

procedure TTSApplication.SetHintPause(const aValue: Integer);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.HintPause := aValue
  end
  else
  	Application.HintPause := aValue;
end;

procedure TTSApplication.SetHintShortCuts(const aValue: Boolean);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.HintShortCuts := aValue
  end
  else
  	Application.HintShortCuts := aValue;
end;

procedure TTSApplication.SetHintShortPause(const aValue: Integer);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.HintShortPause := aValue
  end
  else
  	Application.HintShortPause := aValue;
end;

procedure TTSApplication.SetOnActionExecute(const Value: TActionEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnActionExecute := Value
  end
  else
  	Application.OnActionExecute := Value;
end;

procedure TTSApplication.SetOnActionUpdate(const Value: TActionEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnActionUpdate := Value
  end
  else
  	Application.OnActionUpdate := Value;
end;

procedure TTSApplication.SetOnActivate(const Value: TNotifyEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnActivate := Value
  end
  else
  	Application.OnActivate := Value;
end;

procedure TTSApplication.SetOnDeactivate(const Value: TNotifyEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnDeactivate := Value
  end
  else
  	Application.OnDeactivate := Value;
end;

procedure TTSApplication.SetOnException(const Value: TExceptionEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnException := Value
  end
  else
    Application.OnException := Value;
end;

procedure TTSApplication.SetOnIdle(const Value: TIdleEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnIdle := Value
  end
  else
  	Application.OnIdle := Value;
end;

procedure TTSApplication.SetOnHelp(const Value: THelpEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnHelp := Value
  end
  else
  	Application.OnHelp := Value;
end;

procedure TTSApplication.SetOnHint(const Value: TNotifyEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnHint := Value
  end
  else
  	Application.OnHint := Value;
end;

procedure TTSApplication.SetOnMinimize(const Value: TNotifyEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnMinimize := Value
  end
  else
  	Application.OnMinimize := Value;
end;

procedure TTSApplication.SetOnModalBegin(const Value: TNotifyEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnModalBegin := Value
  end
  else
  	Application.OnModalBegin := Value;
end;

procedure TTSApplication.SetOnModalEnd(const Value: TNotifyEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnModalEnd := Value
  end
  else
  	Application.OnModalEnd := Value;
end;

procedure TTSApplication.SetOnRestore(const Value: TNotifyEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnRestore := Value
  end
  else
  	Application.OnRestore := Value;
end;

procedure TTSApplication.SetOnShowHint(const Value: TShowHintEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnShowHint := Value
  end
  else
  	Application.OnShowHint := Value;
end;

procedure TTSApplication.SetShowHint(const aValue: Boolean);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.ShowHint := aValue
  end
  else
  	Application.ShowHint := aValue;
end;

procedure TTSApplication.SetOnShortCut(const Value: TShortCutEvent);
begin
  if csDesigning in ComponentState then
  begin
    if Assigned(AppEventStorage) then
      AppEventStorage.OnShortCut := Value
  end
  else
  	Application.OnShortCut := Value;
end;

initialization
  AppEventStorage := TAppEventStorage.Create;
  AppConnectedFiles := TTSStructuredFileName.Create;
  AppConnectedFiles.Options := [fnoFileNameIsApplicationName];
  AppName := CleanupFileName(Application.ExeName);


finalization
  FreeAndNil(AppConnectedFiles);
  FreeAndNil(AppCommonINI);
  FreeAndNil(AppFileINI);
  FreeAndNil(InternalTSApp);
  FreeAndNil(AppEventStorage);



end.

