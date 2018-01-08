unit TSLibFiles;

interface

uses
	{$IFDEF WINDOWS}
        ShlObj,
        {$EndIf}
	Classes;

type

  //ACHTUNG: Wenn die Reihenfolge der "SpecialDirectory"-Einträge geändert wird muss
  //         eventuell eine Änderung der Funktion "IsSpecialDir" durchgeführt werden!
  //Typsichere Umsetzung für CSIDL_<xx> Verzeichnis-Konstanten. Wird verwendet von der Funktion "GetSystemFolder".
  NSpecialDirectory = (sdirNone, sdirApplication, sdirCurrent,
                sdirDesktop, sdirPrograms, sdirWindows, sdirWinFonts, sdirProgramsCmn,
                sdirUsrRoot,
                sdirUsrAppData, sdirUsrAppDataLocal, sdirUsrStartMenu, sdirUsrMnuStartup, sdirUsrMnuPrograms,
                sdirUsrDesktop, sdirUsrDocs, sdirUsrMusic, sdirUsrPictures, sdirUsrVideo, sdirUsrFavorites,
                sdirCmnAppData, sdirCmnStartMenu, sdirCmnMnuStartup, sdirCmnMnuPrograms, sdirCmnDesktop,
                sdirCmnDocs, sdirCmnMusic, sdirCmnPictures, sdirCmnVideo);

function TSGetFileSize(const FileName: string): Integer;
function TSFileDate(const FileName: string): TDateTime;
function CleanupFileName(const FileName: string; RemovePath: Boolean=True; RemoveExt: Boolean=True): string;
//procedure CopyFilesToClipboard(FileList: string);

function GetFileVersionParts(const FileName: string; Part1, Part2, Part3, Part4: Boolean): string;
function GetFileBuildNr(const FileName: string): string;
function FileExtRemove(const FileName: string): string;
function FileExtReplace(const FileName, Extension: string): string;

function SpecialDir(DirType: NSpecialDirectory; TrailPathDelim: Boolean=True): string;
function IsSpecialDir(Dir: string): NSpecialDirectory;
function ExtractFileParts(var Dir: string): NSpecialDirectory; overload;
function ExtractFileParts(var Dir: string; out SubDir, FileName: string): NSpecialDirectory; overload;
function ExtractFileParts(var Dir: string; out SubDir, FileName, FileExt: string): NSpecialDirectory; overload;

function AppTmpFolder(const FolderName: string = ''; DelOnTerminate: Boolean = True): string;
function AppTmpFile(FileExt: string; FileName: string = ''; DelOnTerminate: Boolean = True): string;
procedure DelTempFiles;
procedure DelTempFolders;

{$IFDEF WINDOWS}
 const
  SysFolderID: array [NSpecialDirectory] of Integer = (
    -1, //sdirNone
    -1, //sdirApplication
    -1, //sdirCurrent
    CSIDL_DESKTOP, //sdirDesktop
    CSIDL_PROGRAM_FILES, //sdirPrograms
    CSIDL_WINDOWS,  //sdirWindows
    CSIDL_FONTS, //sdirWinFonts
    CSIDL_PROGRAM_FILES_COMMON, //sdirProgramsCmn

    CSIDL_PROFILE, //sdirUsrRoot
    CSIDL_APPDATA, //sdirUsrAppData
    CSIDL_LOCAL_APPDATA, //sdirUsrAppDataLocal
    CSIDL_STARTMENU, //sdirUsrStartMenu
    CSIDL_STARTUP, //sdirUsrMnuStartup
    CSIDL_PROGRAMS, //sdirUsrMnuPrograms
    CSIDL_DESKTOPDIRECTORY, //sdirUsrDesktop
    CSIDL_MYDOCUMENTS, //sdirUsrDocs
    CSIDL_MYMUSIC, //sdirUsrMusic
    CSIDL_MYPICTURES, //sdirUsrPictures
    CSIDL_MYVIDEO, //sdirUsrVideo
    CSIDL_FAVORITES, //sdirUsrFavorites

    CSIDL_COMMON_APPDATA, //sdirCmnAppData
    CSIDL_COMMON_STARTMENU, //sdirCmnStartMenu
    CSIDL_COMMON_STARTUP, //sdirCmnMnuStartup
    CSIDL_COMMON_PROGRAMS, //sdirCmnMnuPrograms
    CSIDL_COMMON_DESKTOPDIRECTORY, //sdirCmnDesktop
    CSIDL_COMMON_DOCUMENTS, //sdirCmnDocs
    CSIDL_COMMON_MUSIC, //sdirCmnMusic
    CSIDL_COMMON_PICTURES, //sdirCmnPictures
    CSIDL_COMMON_VIDEO //sdirCmnVideo
  );
{$ENDIF}

implementation

uses
  {$IFDEF WINDOWS}
	Windows,
  {$ENDIF}
	ClipBrd,
	TSLib,
	TSLibDateTime,
	Forms,
	SysUtils;

var
	TmpFolder: string = 'Temp';
	TmpFolderList: TStringList = nil;
	TmpFileList: TStringList = nil;

{$IFDEF WINDOWS}
//---------------------------------------------------------------------------------------
function SpecialDir(DirType: NSpecialDirectory; TrailPathDelim: Boolean=True): string;
var
  Path: PChar;
begin
  case DirType of
    sdirNone:
      Result := '';
    sdirApplication:
      Result := ApplicationPath;
    sdirCurrent:
			Result := GetCurrentDir;
    else begin
      Path := AllocMem(MAX_PATH);
      try
        if SHGetFolderPath(0, SysFolderID[DirType], 0, SHGFP_TYPE_CURRENT, Path) = S_OK then
          Result := Path
        else
          Result := '';
      finally
        FreeMem(Path);
      end;
    end;
  end;
  if TrailPathDelim and (Result > '') then
    Result := IncludeTrailingPathDelimiter(Result);
end;
{$ENDIF}

//---------------------------------------------------------------------------------------
function IsSpecialDir(Dir: string): NSpecialDirectory;
begin
  if not DirectoryExists(Dir) and FileExists(Dir) then
    Dir := IncludeTrailingPathDelimiter(ExtractFilePath(Dir))
  else
    Dir := IncludeTrailingPathDelimiter(Dir);
  if (Dir>'') and (Dir[1]='"') and (Dir[Length(Dir)]<>'"') then
    Dir := Dir + '"';
  if DirectoryExists(Dir) then
    //ACHTUNG: Wenn die Reihenfolge der "SpecialDirectory"-Einträge geändert wird
    //  muss hier eventuell eine Änderung eingetragen werden damit alle
    //  "Standard"-Systemverzeichnisse durchsucht werden.
    for Result := succ(sdirCurrent) to High(NSpecialDirectory) do
      if SameText(Dir, SpecialDir(Result)) then
        Exit;
  if SameText(Dir, SpecialDir(sdirApplication)) then
    Result := sdirApplication
  else if SameText(Dir, SpecialDir(sdirApplication)) then
    Result := sdirApplication
  else
    Result := sdirNone;
end;

//---------------------------------------------------------------------------------------
function ExtractFileParts(var Dir: string): NSpecialDirectory;
var
  SD, FN, FE: string;
begin
  Result := ExtractFileParts(Dir, SD, FN, FE);
end;

//---------------------------------------------------------------------------------------
function ExtractFileParts(var Dir: string; out SubDir, FileName: string): NSpecialDirectory; overload;
var
  FN, FE: string;
begin
  Result := ExtractFileParts(Dir, SubDir, FN, FE);
  FileName := FN + FE;
end;

//---------------------------------------------------------------------------------------
function ExtractFileParts(var Dir: string; out SubDir, FileName, FileExt: string): NSpecialDirectory; overload;
var
  L, P: Integer;
begin
  L := Length(Dir);
  if Dir > '' then
  begin
    if Dir[L]='"' then
      Delete(Dir, L, 1);
    if Dir[1]='"' then
      Delete(Dir, 1, 1);
  end;

  P := LastDelimiter(PathDelim + DriveDelim, Dir);
  if P = Length(Dir) then
  begin
    FileName := '';
    FileExt := '';
  end
  else if DirectoryExists(Dir) then
  begin
    P := Length(Dir)+1;
    FileName := '';
    FileExt := '';
  end
  else begin
    FileName := Copy(Dir, P+1, MaxInt);
    FileExt := ExtractFileExt(FileName);
  end;

  SubDir := '';
  repeat
    SetLength(Dir, P-1);
    Result := IsSpecialDir(Dir);
    if Result = sdirNone then
    begin
      P := LastDelimiter(PathDelim + DriveDelim, Dir);
      SubDir := Copy(Dir, P, MaxInt) + SubDir;
    end;
  until (Result <> sdirNone) or (P=0);

  if Result = sdirNone then
    Dir := ''
  else if (SubDir > '') and (SubDir[1]=PathDelim) then
  begin
    Delete(SubDir, 1, 1);
    Dir := IncludeTrailingPathDelimiter(Dir);
  end;
  if SubDir > '' then
    SubDir := IncludeTrailingPathDelimiter(SubDir);

  if (FileExt>'') then
  begin
    P := Pos(FileExt, FileName);
    if P > 0 then
      Delete(FileName, P, MaxInt);
    if FileExt[1] <> '.' then
      FileExt := '.' + FileExt;
  end;
end;

//---------------------------------------------------------------------------------------
function TSFileDate(const FileName: string): TDateTime;
begin
	if not (FileExists(FileName)
    and FileAge(FileName, Result))
  then
		Result := TSDateNULL;
end;

//---------------------------------------------------------------------------------------
function TSGetFileSize(const FileName: string): Integer;
var
	Found: Integer;
	FindData: TSearchRec;
begin
	Found := FindFirst(FileName, faAnyFile, FindData);
	if Found = 0 then
		Result := FindData.Size
	else
		Result := -1;
end; //TSGetFileSize


//---------------------------------------------------------------------------------------
function CleanupFileName(const FileName: string; RemovePath: Boolean=True; RemoveExt: Boolean=True): string;
var
	FE: string;
begin
	if RemovePath then
		Result := ExtractFileName(FileName)
	else
		Result := FileName;
	if RemoveExt then begin
		FE := ExtractFileExt(Result);
		Result := Copy(Result, 1, Length(Result) - Length(FE));
	end;
end;

//---------------------------------------------------------------------------------------
//procedure CopyFilesToClipboard(FileList: string);
//var
//	DropFiles: PDropFiles;
//	hGlobal: THandle;
//	iLen: Integer;
//begin
//	iLen := Length(FileList) + 2;
//	FileList := FileList + #0#0;
//	hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
//		SizeOf(TDropFiles) + iLen);
//	if (hGlobal = 0) then raise Exception.Create('Could not allocate memory.');
//	begin
//		DropFiles := GlobalLock(hGlobal);
//		DropFiles^.pFiles := SizeOf(TDropFiles);
//		Move(FileList[1], (PChar(DropFiles) + SizeOf(TDropFiles))^, iLen);
//		GlobalUnlock(hGlobal);
//		Clipboard.SetAsHandle(CF_HDROP, hGlobal);
//	end;
//end;

//---------------------------------------------------------------------------------------
function AppTmpFolder(const FolderName: string = ''; DelOnTerminate: Boolean = True): string;
var
	FP, FN: string;
begin
	if FolderName > '' then
		TmpFolder := FolderName;
	FN := ExtractFileName(TmpFolder);
	FP := ExtractFilePath(TmpFolder);
	if FP = '' then
		FP := ExtractFilePath(ParamStr(0));
	FP := ExpandFileName(IncludeTrailingPathDelimiter(FP) + FN);
	TmpFolder := IncludeTrailingPathDelimiter(FP);
	if not ForceDirectories(TmpFolder) then
		Result := ''
	else
		Result := TmpFolder;

	if DelOnTerminate
	and (Result > '') then begin
		if not Assigned(TmpFolderList) then begin
			TmpFolderList := TStringList.Create;
			TmpFolderList.Duplicates := dupIgnore;
		end;
		TmpFolderList.Add(Result);
	end;
end; //AppTmpFolder

//---------------------------------------------------------------------------------------
function AppTmpFile(FileExt: string; FileName: string = ''; DelOnTerminate: Boolean = True): string;
var
	NewGUID: TGUID;
	Ctr: Integer;
begin
	//Versuche einen gültigen Dateinamen zu erzeugen.
	if FileName = '' then begin
		CreateGUID(NewGUID);
		FileName := GUIDToString(NewGUID);
	end;

	if (Length(Fileext)>0) and (Fileext[1] <> '.') then
		FileExt := '.' + FileExt;

	Result := AppTmpFolder + FileName + FileExt;
	Ctr := 1;
	while FileExists(Result) do
	begin
		Result := AppTmpFolder + FileName + IntToStr(Ctr) + FileExt;
		Inc(Ctr);
	end;

	if DelOnTerminate
	and (Result > '') then begin
		if not Assigned(TmpFileList) then begin
			TmpFileList := TStringList.Create;
			TmpFileList.Duplicates := dupIgnore;
		end;
		TmpFileList.Add(Result);
	end;
end; //MakeTmpFileName


procedure DelTempFiles;
begin
	if Assigned(TmpFileList) then
		while TmpFileList.Count > 0 do begin
			DeleteFile(TmpFileList[0]);
			TmpFileList.Delete(0);
		end;
	FreeAndNil(TmpFileList);
end;

procedure DelTempFolders;
begin
	if Assigned(TmpFolderList) then
		while TmpFolderList.Count > 0 do begin
			DeleteFile(TmpFolderList[0]);
			TmpFolderList.Delete(0);
		end;
	FreeAndNil(TmpFolderList);
end;

{$IFDEF WINDOWS}
//***************************************************************************************
function GetFileVersionParts(const FileName: string; Part1, Part2, Part3, Part4: Boolean): string;
var
	AppF: string;
	I: DWord;
	VerDataSize: UINT;
	PFix: PVSFixedFileInfo;
	P, VerData: Pointer;
begin
	Result := 'No version info found in file.';
	AppF := FileName;
	VerDataSize := GetFileVersionInfoSize(PChar(AppF), I);
	if VerDataSize = 0 then
		Exit;
	GetMem(VerData, VerDataSize);
	try
		if GetFileVersionInfo(PChar(AppF), 0, VerDataSize, VerData) then
		begin
			VerQueryValue(VerData, '\', P, VerDataSize);
			PFix := P;

			if Part1 then
				Result := IntToStr((PFix^.dwFileVersionMS and $FFFF0000) shr 16)
			else
				Result := '';

			if Part2 then
				Result := Result + IIf(Result > '', '.', '') + IntToStr(PFix^.dwFileVersionMS and $0000FFFF);
			if Part3 then
				Result := Result + IIf(Result > '', '.', '') + IntToStr((PFix^.dwFileVersionLS and $FFFF0000) shr 16);
			if Part4 then
				Result := Result + IIf(Result > '', '.', '') + IntToStr(PFix^.dwFileVersionLS and $0000FFFF);
		end;
	finally
		FreeMem(VerData);
	end;
end;
{$ENDIF}

function GetFileBuildNr(const FileName: string): string;
begin
	Result := GetFileVersionParts(FileName, False, False, False, True);
end;

function FileExtRemove(const FileName: string): string;
begin
  Result := CleanupFileName(FileName, False, True);
end;

function FileExtReplace(const FileName, Extension: string): string;
begin
  if Extension = '' then
    Result := CleanupFileName(FileName, False, True)
  else if Extension[1] <> '.' then
    Result := CleanupFileName(FileName, False, True) + '.' + Extension
  else
    Result := CleanupFileName(FileName, False, True) + Extension;
end;

initialization


finalization
	DelTempFiles;
	DelTempFolders;

end.
