unit TS_StructuredFileName;

{$mode objfpc}{$H+}

interface

uses
  TSLibFiles,
	Classes, SysUtils;


type
  //Aufzählungs- und Set-Typen für TSStructuredFileName
  NFileNameOption = (fnoFileNameIsApplicationName,
                      fnoSubDirIsCompanyName,
                      fnoExtIsLog,
                      fnoExtIsINI,
                      fnoUseTSAppBaseDir,   //Use the "BaseDir" settings from the main TSApp-Component
                      fnoUseTSAppSubDir,    //Use the "SubDir" settings from the main TSApp-Component
                      fnoUseTSAppFileName   //Use the "FileName" settings from the main TSApp-Component
                      );
  SFileNameOptions = set of NFileNameOption;
  NStructuredFileNameProp = (sfpFullPath, sfpBaseDir, sfpSubDir, sfpFileName, sfpFileExt, sfpOptions);

  //Ereignistypen für TSStructuredFileName
  MStructuredFileNamePropStored = function (Sender: TObject; Prop: NStructuredFileNameProp): Boolean of object;
  MStructuredFileChanged = procedure (Sender: TObject; const OldPath, NewPath: string) of object;
  MSimpleCallback = procedure of object;


	TTSStructuredFileName = class(TPersistent)
	private
	  fFileName: string;
	  fSubDir: string;
	  fFileExt: string;
	  fOnChange: TNotifyEvent;
	  fCheckPropStored: MStructuredFileNamePropStored;
	  fFileNameOptions: SFileNameOptions;
	  fBaseDirectory: NSpecialDirectory;
	  fBeforeChange: TNotifyEvent;
	  fPathChanged: MStructuredFileChanged;

	  fNewBaseDir: NSpecialDirectory;
	  fNewSubDir: string;
	  fNewFileName: string;
	  fNewExt: string;
	  fNewOptions: SFileNameOptions;
	  fNewFullPath: string;

	  function GetFullPath: string;
	  procedure SetBaseDirectory(const Value: NSpecialDirectory);
	  procedure SetFileExt(Value: string);
	  procedure SetFileName(Value: string);
	  procedure SetFileNameOptions(Value: SFileNameOptions);
	  procedure SetFileSubDir(Value: string);
	  function GetActualDirectory: string;
	  function GetActualFileName: string;
	  procedure SetFullPath(const Value: string);
	  function GetBaseDirectory: NSpecialDirectory;
	  function GetSubDir: string;
	  function GetFileName: string;
	  function GetFileExt: string;

	  procedure DoPropertyChange(Callback: MSimpleCallback);
	  procedure SetPropBaseDir;
	  procedure SetPropSubDir;
	  procedure SetPropFileName;
	  procedure SetPropExt;
	  procedure SetPropOptions;
	  procedure SetPropFullPath;

	protected
	  function StoreFileNameOptions: Boolean; virtual;
	  function StoreBaseDirectory: Boolean; virtual;
	  function StoreFileName: Boolean; virtual;
	  function StoreFileSubDir: Boolean; virtual;
	  function StoreFileExt: Boolean; virtual;
	public
	  constructor Create;
	  procedure Assign(Source: TPersistent); override;
	  property CheckPropStored: MStructuredFileNamePropStored read fCheckPropStored write fCheckPropStored;
	  procedure EvalNewFilePath(Path: string);
	published
	  property BaseDirectory: NSpecialDirectory read GetBaseDirectory write SetBaseDirectory stored StoreBaseDirectory;
	  property SubDir: string read GetSubDir write SetFileSubDir stored StoreFileSubDir;
	  property FileName: string read GetFileName write SetFileName stored StoreFileName;
	  property FileExtension: string read GetFileExt write SetFileExt stored StoreFileExt;
	  property Options: SFileNameOptions read fFileNameOptions write SetFileNameOptions stored StoreFileNameOptions;
	  property ActualDirectory: string read GetActualDirectory;
	  property ActualFileName: string read GetActualFileName;
	  property FullPath: string read GetFullPath write SetFullPath stored False;
	  property BeforeChange: TNotifyEvent read fBeforeChange write fBeforeChange;
	  property OnChange: TNotifyEvent read fOnChange write fOnChange;
	  property FullPathChange: MStructuredFileChanged read fPathChanged write fPathChanged;
	end;


const
  fileExtNone = '.';
  fileExtINI = '.ini';
  fileExtLOG = '.log';

implementation

uses
  TS_Application;

{ TStructuredFileName }

constructor TTSStructuredFileName.Create;
begin
  inherited Create;
  fFileNameOptions := [fnoFileNameIsApplicationName
                      , fnoExtIsINI
                      , fnoUseTSAppBaseDir
                      , fnoUseTSAppSubDir
                      ];
  fBaseDirectory := sdirApplication;
  fSubDir := '';
  fFileName := CleanupFileName(ParamStr(0));
  fFileExt := fileExtINI;
end;

procedure TTSStructuredFileName.Assign(Source: TPersistent);
begin
  if Source is TTSStructuredFileName then
  begin
    fFileNameOptions := TTSStructuredFileName(Source).fFileNameOptions;
    fBaseDirectory := TTSStructuredFileName(Source).fBaseDirectory;
    fSubDir := TTSStructuredFileName(Source).fSubDir;
    fFileName := TTSStructuredFileName(Source).fFileName;
    fFileExt := TTSStructuredFileName(Source).fFileExt;
  end
  else
    inherited Assign(Source);
end;


procedure TTSStructuredFileName.DoPropertyChange(Callback: MSimpleCallback);
var
  OldPath: string;
begin
  if Assigned(fBeforeChange) then
    fBeforeChange(Self);
  if Assigned(fPathChanged) then
  begin
    OldPath := FullPath;
    if Assigned(Callback) then
      Callback;
    fPathChanged(Self, OldPath, FullPath);
  end
  else if Assigned(Callback) then
    Callback;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TTSStructuredFileName.GetBaseDirectory: NSpecialDirectory;
begin
  if (fnoUseTSAppBaseDir in fFileNameOptions)
    and (Self <> TSApp.ConnectedFiles)
  then
    Result := TSApp.ConnectedFiles.BaseDirectory
  else
    Result := fBaseDirectory;
end;

procedure TTSStructuredFileName.SetPropBaseDir;
begin
  fBaseDirectory := fNewBaseDir;
  Exclude(fFileNameOptions, fnoUseTSAppBaseDir);
end;

procedure TTSStructuredFileName.SetBaseDirectory(const Value: NSpecialDirectory);
begin
  if BaseDirectory <> Value then
  begin
    fNewBaseDir := Value;
    DoPropertyChange(@SetPropBaseDir);
  end;
end;

function TTSStructuredFileName.StoreBaseDirectory: Boolean;
begin
  if (fnoUseTSAppBaseDir in fFileNameOptions) then
    Result := False
  else if Assigned(fCheckPropStored) then
    Result := fCheckPropStored(Self, sfpBaseDir)
  else
    Result := not (fBaseDirectory = sdirApplication);
end;

function TTSStructuredFileName.GetSubDir: string;
begin
  if (fnoUseTSAppSubDir in fFileNameOptions) then
    Result := TSApp.ConnectedFiles.SubDir
  else if (fnoSubDirIsCompanyName in fFileNameOptions) then
    Result := TSApp.CompanyName
  else
    Result := fSubDir;
end;

procedure TTSStructuredFileName.SetPropSubDir;
begin
  fSubDir := fNewSubDir;
  Exclude(fFileNameOptions, fnoUseTSAppSubDir);
  Exclude(fFileNameOptions, fnoSubDirIsCompanyName);
end;

procedure TTSStructuredFileName.SetFileSubDir(Value: string);
begin
  if SubDir <> Value then
  begin
    fNewSubDir := Value;
    DoPropertyChange(@SetPropSubDir);
  end;
end;

function TTSStructuredFileName.StoreFileSubDir: Boolean;
begin
  if (fnoUseTSAppSubDir in fFileNameOptions)
    or (fnoSubDirIsCompanyName in fFileNameOptions)
  then
    Result := False
  else if Assigned(fCheckPropStored) then
    Result := fCheckPropStored(Self, sfpSubDir)
  else
    Result := fSubDir > '';
end;

function TTSStructuredFileName.GetFileName: string;
begin
  if (fnoUseTSAppFileName in fFileNameOptions) then
    Result := TSApp.ConnectedFiles.FileName
  else if fnoFileNameIsApplicationName in fFileNameOptions then
    Result := CleanupFileName(ParamStr(0))
  else
    Result := fFileName;
end;

procedure TTSStructuredFileName.SetPropFileName;
begin
  fFileName := fNewFileName;
  Exclude(fFileNameOptions, fnoFileNameIsApplicationName);
end;

procedure TTSStructuredFileName.SetFileName(Value: string);
begin
  if FileName <> Value then
  begin
    fNewFileName := Value;
    DoPropertyChange(@SetPropFileName);
  end;
end;

function TTSStructuredFileName.StoreFileName: Boolean;
begin
  if (fnoUseTSAppFileName in fFileNameOptions)
    or (fnoFileNameIsApplicationName in fFileNameOptions)
  then
    Result := False
  else if Assigned(fCheckPropStored) then
    Result := fCheckPropStored(Self, sfpFileName)
  else
    Result := fFileName > '';
end;

function TTSStructuredFileName.GetFileExt: string;
begin
  if fnoExtIsINI in fFileNameOptions then
    Result := fileExtINI
  else if fnoExtIsLog in fFileNameOptions then
    Result := fileExtLOG
  else
    Result := fFileExt;
end;

procedure TTSStructuredFileName.SetPropExt;
begin
  fFileExt := fNewExt;
  Exclude(fFileNameOptions, fnoExtIsLog);
  Exclude(fFileNameOptions, fnoExtIsINI);
end;

procedure TTSStructuredFileName.SetFileExt(Value: string);
begin
  if (fFileExt <> Value) then
  begin
    fNewExt := Value;
    DoPropertyChange(@SetPropExt);
  end;
end;

function TTSStructuredFileName.StoreFileExt: Boolean;
begin
  if (fnoExtIsINI in fFileNameOptions)
    or (fnoExtIsLog in fFileNameOptions)
  then
    Result := False
  else if Assigned(fCheckPropStored) then
    Result := fCheckPropStored(Self, sfpFileExt)
  else
    Result := fFileExt <> fileExtNone;
end;

procedure TTSStructuredFileName.SetPropOptions;
begin
  if (Self = TSApp.ConnectedFiles) then
  begin
    Exclude(fNewOptions, fnoUseTSAppBaseDir);
    Exclude(fNewOptions, fnoUseTSAppSubDir);
    Exclude(fNewOptions, fnoUseTSAppFileName);
  end;

  if (fnoUseTSAppFileName in fNewOptions)
    and not (fnoUseTSAppFileName in fFileNameOptions)
  then
    Exclude(fNewOptions, fnoFileNameIsApplicationName)

  else if (fnoFileNameIsApplicationName in fNewOptions)
    and not (fnoFileNameIsApplicationName in fFileNameOptions)
  then begin
    Exclude(fNewOptions, fnoUseTSAppFileName);
    fFileName := CleanupFileName(ParamStr(0));
  end;

  if (fnoUseTSAppSubDir in fNewOptions)
    and not (fnoUseTSAppSubDir in fFileNameOptions)
  then
    Exclude(fNewOptions, fnoSubDirIsCompanyName)

  else if (fnoSubDirIsCompanyName in fNewOptions)
    and not (fnoSubDirIsCompanyName in fFileNameOptions)
  then begin
    fSubDir := TSApp.CompanyName;
    Exclude(fNewOptions, fnoUseTSAppSubDir);
  end;

  if (fnoExtIsLog in fNewOptions)
    and not (fnoExtIsLog in fFileNameOptions)
  then begin
    Exclude(fNewOptions, fnoExtIsINI);
    fFileExt := fileExtLOG;
  end;

  if (fnoExtIsINI in fNewOptions)
    and not (fnoExtIsINI in fFileNameOptions)
  then begin
    Exclude(fNewOptions, fnoExtIsLog);
    fFileExt := fileExtINI;
  end;

  fFileNameOptions := fNewOptions;
end;

procedure TTSStructuredFileName.SetFileNameOptions(Value: SFileNameOptions);
begin
  if fFileNameOptions <> Value then
  begin
    fNewOptions := Value;
    DoPropertyChange(@SetPropOptions);
  end;
end;

function TTSStructuredFileName.StoreFileNameOptions: Boolean;
begin
  if Assigned(fCheckPropStored) then
    Result := fCheckPropStored(Self, sfpOptions)
  else
    Result := fFileNameOptions <> [fnoFileNameIsApplicationName
                                  , fnoExtIsINI
                                  , fnoUseTSAppBaseDir
                                  , fnoUseTSAppSubDir
                                  ];
end;

function TTSStructuredFileName.GetFullPath: string;
begin
  Result := SpecialDir(BaseDirectory, True);
  if SubDir > '' then
    Result := IncludeTrailingPathDelimiter(Result + SubDir);

  Result := ExpandFileName(Result + FileName + FileExtension);
end;

procedure TTSStructuredFileName.SetPropFullPath;
var
  SDir: NSpecialDirectory;
  ASubDir, Dir, FN, Ext: string;
begin
  if fNewFullPath = '' then   //Use FullPatch = '' to reset to defaults.
  begin
    fFileNameOptions := [fnoFileNameIsApplicationName
                        , fnoUseTSAppBaseDir
                        , fnoUseTSAppSubDir
                        ];
    if Self = TSApp.LogFileName then
      Include(fFileNameOptions, fnoExtIsLog)
    else if Self = TSApp.Storage.INIFile then
      Include(fFileNameOptions, fnoExtIsINI);
  end
  else begin
    Dir := fNewFullPath;
    Sdir := ExtractFileParts(Dir, ASubDir, FN, Ext);

    if (SDir <> sdirNone) or SameText(ExpandFileName(fNewFullPath), fNewFullPath) then
    begin
      fBaseDirectory := SDir;
      Exclude(fFileNameOptions, fnoUseTSAppBaseDir);
    end;

    if (ASubDir > '') and not SameText(ASubDir, fSubDir) then
    begin
      fSubDir := ASubDir;
      fFileNameOptions := fFileNameOptions - [fnoSubDirIsCompanyName, fnoUseTSAppSubDir];
    end;

    if (FN > '') and not SameText(FN, fFileName) then
    begin
      fFileName := FN;
      fFileNameOptions := fFileNameOptions - [fnoFileNameIsApplicationName, fnoUseTSAppFileName];
    end;

    if (Ext > '') and not SameText(Ext, GetFileExt) then
    begin
      fFileExt := Ext;
      fFileNameOptions := fFileNameOptions - [fnoExtIsLog, fnoExtIsINI];
    end;
  end;
end;

procedure TTSStructuredFileName.SetFullPath(const Value: string);
begin
  fNewFullPath := Value;
  DoPropertyChange(@SetPropFullPath);
end;

procedure TTSStructuredFileName.EvalNewFilePath(Path: string);
var
  SDir: NSpecialDirectory;
  ASubDir, Dir, FN, Ext: string;
begin
  if Assigned(fBeforeChange) then
    fBeforeChange(Self);

  Dir := Path;
  Sdir := ExtractFileParts(Dir, ASubDir, FN, Ext);

  fFileName := FN;
  fFileExt := Ext;

  if (ASubDir <> fSubDir) then
  begin
    fSubDir := ASubDir;
    fBaseDirectory := SDir;
  end
  else if  not (SDir in [sdirNone, fBaseDirectory]) then
    fBaseDirectory := SDir;
  fFileNameOptions := [];

  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TTSStructuredFileName.GetActualDirectory: string;
begin
  Result := SpecialDir(BaseDirectory, True);
  if SubDir > '' then
    Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

function TTSStructuredFileName.GetActualFileName: string;
begin
  Result := fFileName + fFileExt
end;

end.

