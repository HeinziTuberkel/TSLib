unit TSEdit_StringsContainer;

{$mode objfpc}{$H+}

interface

uses
  ComponentEditors,
  TS_Forms,
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type
	TFrmEditStringsContainer = class(TForm)
    pnlButton: TPanel;
    BtnLoad: TBitBtn;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnSave: TBitBtn;
    BtnSelAll: TBitBtn;
    TCEdits: TTabControl;
    ImageList1: TImageList;
    ActionList1: TActionList;
    ActLoad: TAction;
    ActSave: TAction;
    ActOK: TAction;
    ActCancel: TAction;
    ActSelAll: TAction;
		DlgSave: TSaveDialog;
		DlgOpen: TOpenDialog;
    AcAddList: TAction;
    AcRemoveList: TAction;
    INI: TTSFileINI;
    LBxEdits: TListBox;
    SPLB: TTSSplitter;
    TSPanel1: TTSPanel;
    XList: TCheckBox;
    XTabs: TCheckBox;
    MeText: TMemo;
    LbCaret: TLabel;
    TSPanel2: TTSPanel;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    SCEdited: TTSStringsContainer;
		procedure TSFormShow(Sender: TObject);
    procedure ActOKExecute(Sender: TObject);
    procedure ActCancelExecute(Sender: TObject);
		procedure TCEditsChange(Sender: TObject);
    procedure TCEditsChanging(Sender: TObject; var AllowChange: Boolean);
		procedure MeTextKeyPress(Sender: TObject; var Key: Char);
		procedure MeTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure AcAddListExecute(Sender: TObject);
		procedure AcRemoveListExecute(Sender: TObject);
		procedure ActSaveExecute(Sender: TObject);
		procedure ActLoadExecute(Sender: TObject);
		procedure LBxEditsClick(Sender: TObject);
		procedure XListClick(Sender: TObject);
		procedure XTabsClick(Sender: TObject);
    procedure ActSelAllExecute(Sender: TObject);
    procedure MeTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	end;


	TTSStringsContainerEditor = class(TComponentEditor)
	private
		SCContainer: TTSStringsContainer;
		SCForm: TFrmEditStringsContainer;
	public
		constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
		procedure ExecuteVerb(Index: Integer); override;
		function GetVerb(Index: Integer): string; override;
		function GetVerbCount: Integer; override;
	end;



var
	FrmEditStringsContainer: TFrmEditStringsContainer;

implementation

{$R *.lfm}

const
	iniListGroup = 'List Names|ListNames';
	iniListLines = 'Lines ';
	txtFileSeparator = '--------------------------------------------------------------------------------';



//***************************************************************************************
{ TTSStringsContainerEditor }
//***************************************************************************************

//***************************************************************************************
constructor TTSStringsContainerEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
	inherited Create(AComponent, ADesigner);
	SCContainer :=  TTSStringsContainer(AComponent);
end;

//***************************************************************************************
procedure TTSStringsContainerEditor.ExecuteVerb(Index: Integer);
begin
	SCForm := TFrmEditStringsContainer.Create(Application);
	try
		SCForm.SCEdited.Assign(Component);
		if SCForm.ShowModal = mrOK then
		begin
			Component.Assign(SCForm.SCEdited);
			Designer.Modified;
		end;
	finally
		SCForm.Release;
	end;
end;

//***************************************************************************************
function TTSStringsContainerEditor.GetVerb(Index: Integer): string;
begin
	Result := 'Listen bearbeiten';
end;

//***************************************************************************************
function TTSStringsContainerEditor.GetVerbCount: Integer;
begin
	Result := 1;
end;


//***************************************************************************************
TFrmEditStringsContainer
//***************************************************************************************

//***************************************************************************************
procedure TFrmEditStringsContainer.AcAddListExecute(Sender: TObject);
var
	I: Integer;
begin
	I := SCEdited.AddList('');
	TCEdits.Tabs.Add(SCEdited.ListName[I]);
	LBxEdits.Items.Add(SCEdited.ListName[I]);
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.AcRemoveListExecute(Sender: TObject);
var
	N, I: Integer;
begin
	if SCEdited.ListCount < 2 then
		Exit;
	N := TCEdits.TabIndex;
	for I := N to (SCEdited.ListCount-2) do
	begin
		SCEdited.List[I].Assign(SCEdited.List[I+1]);
		SCEdited.ListNames[I] := SCEdited.ListNames[I+1];
	end;
	SCEdited.DeleteList(SCEdited.ListCount - 1);
	if N < TCEdits.Tabs.Count then
		MeText.Lines.Assign(SCEdited.ActiveList)
	else
		MeText.Lines.Assign(SCEdited.List[SCEdited.ListCount - 1]);
	TCEdits.Tabs.Assign(SCEdited.ListNames);
	LBxEdits.Items.Assign(SCEdited.ListNames);
	if N < TCEdits.Tabs.Count then
	begin
		TCEdits.TabIndex := N;
		LBxEdits.ItemIndex := N;
	end
	else begin
		TCEdits.TabIndex := pred(TCEdits.Tabs.Count);
		LBxEdits.ItemIndex := pred(TCEdits.Tabs.Count);
	end;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.ActCancelExecute(Sender: TObject);
begin
	ModalResult := mrCancel;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.ActLoadExecute(Sender: TObject);
var
	N, I: Integer;
	T: Textfile;
	S: string;
begin
	if DlgOpen.Execute then
	begin
		if DlgOpen.FilterIndex = 1 then
		begin
			INI.INIFile.FullPath := DlgOpen.FileName;
			INI.GetStrings(iniListGroup, SCEdited.ListNames);
			for I := 0 to pred(SCEdited.ListNames.Count) do
				INI.GetStrings(SCEdited.ListName[I]+ '|'+iniListLines, SCEdited.List[I]);
		end
		else begin
			if not FileExists(DlgOpen.FileName) then
				Exit;
			AssignFile(T, DlgOpen.FileName);
			Reset(T);
			SCEdited.ListCount := 1;
			N := 0;
			while not EOF(T) do
			begin
				ReadLn(T, S);
				if Pos(txtFileSeparator, S)>0 then
				begin
					inc(N);
					SCEdited.ListCount := N;
					SCEdited.ActiveListIndex := N-1;
					if not EOF(T) then
					begin
						ReadLn(T, S);
						SCEdited.ActiveListName := S;
					end;
				end
				else if N > 0 then
					SCEdited.ActiveList.Add(S);
			end;
		end;
	end;
	TCEdits.Tabs.Assign(SCEdited.ListNames);
	LBxEdits.Items.Assign(SCEdited.ListNames);
	MeText.Lines.Assign(SCEdited.ActiveList);
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.ActOKExecute(Sender: TObject);
begin
	SCEdited.ActiveList.Assign(MeText.Lines);
	ModalResult := mrOK;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.ActSaveExecute(Sender: TObject);
var
	I, L: Integer;
	T: Textfile;
begin
	if DlgSave.Execute then
	begin
		if DlgSave.FilterIndex = 1 then
		begin
			INI.INIFile.FullPath := DlgSave.FileName;
			INI.PutStrings(iniListGroup, SCEdited.ListNames);
			for I := 0 to pred(SCEdited.ListCount) do
				INI.PutStrings(SCEdited.ListName[I]+ '|'+iniListLines, SCEdited.List[I]);
		end
		else begin
			AssignFile(T, DlgSave.FileName);
			Rewrite(T);
			try
				for I := 0 to pred(SCEdited.ListCount) do
				begin
					WriteLn(T, txtFileSeparator);
					WriteLn(T, SCEdited.ListName[I]);
					for L := 0 to pred(SCEdited.List[I].Count) do
						WriteLn(T, SCEdited.List[I][L]);
				end;
			finally
				CloseFile(T);
			end;
		end;
	end;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.ActSelAllExecute(Sender: TObject);
begin
	MeText.SelectAll;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.LBxEditsClick(Sender: TObject);
var
	Dummy: Boolean;
begin
	TCEditsChanging(TCEdits, Dummy);
	TCEdits.TabIndex := LBxEdits.ItemIndex;
	TCEditsChange(TCEdits);
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.MeTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
	Dummy: Boolean;
begin
	if Shift = [ssCtrl] then
		case Key of
			VK_UP: begin
				if (TCEdits.TabIndex > 0) then
				begin
					TCEditsChanging(TCEdits, Dummy);
					TCEdits.TabIndex := TCEdits.TabIndex - 1;
					TCEditsChange(TCEdits);
				end;
				Key := 0;
			end;
			VK_DOWN: begin
				if (TCEdits.TabIndex < pred(TCEdits.Tabs.Count)) then
				begin
					TCEditsChanging(TCEdits, Dummy);
					TCEdits.TabIndex := TCEdits.TabIndex + 1;
					TCEditsChange(TCEdits);
				end;
				Key := 0;
			end;
		end;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.MeTextKeyPress(Sender: TObject; var Key: Char);
var
	SSt: Integer;
	C: char;
const
	sepCharsBack = [' ', #9, #13, '.', ',', ':'];
	sepCharsFwd = [' ', #9, #10, '.', ',', ':'];
begin
	case Key of
	#127: begin
		if MeText.SelLength > 0 then
			MeText.SelText := ''
		else begin
			SSt := MeText.SelStart;
			repeat
				C := MeText.Lines.Text[MeText.SelStart];
				MeText.SelStart := MeText.SelStart - 1;
			until (MeText.SelStart=0) or (C  in sepCharsBack);
			C := MeText.Lines.Text[MeText.SelStart];
			while (C  in sepCharsBack) and (MeText.SelStart>0) do begin
				MeText.SelStart := MeText.SelStart - 1;
				C := MeText.Lines.Text[MeText.SelStart];
			end;
			MeText.SelLength := SSt - MeText.SelStart;
			MeText.SelText := '';
		end;
		Key := #0;
	end;
	^T: begin
		if MeText.SelLength > 0 then
			MeText.SelText := ''
		else begin
			SSt := Length(MeText.Lines.Text);
			repeat
				MeText.SelLength := MeText.SelLength + 1;
				C := MeText.Lines.Text[MeText.SelStart+MeText.SelLength+1];
			until (MeText.SelStart+MeText.SelLength>=SSt) or (C in sepCharsFwd);
			while (C in sepCharsFwd) and (MeText.SelStart+MeText.SelLength<SSt) do begin
				MeText.SelLength := MeText.SelLength + 1;
				C := MeText.Lines.Text[MeText.SelStart+MeText.SelLength+1];
			end;
			MeText.SelText := '';
		end;
		Key := #0;
	end;
	^Y: begin
		MeText.Lines.Delete(MeText.CaretPos.Y);
		Key := #0;
	end;
	end;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.MeTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
	CP: TPoint;
begin
	CP := MeText.CaretPos;
	LbCaret.Caption := IntToStr(CP.Y+1) + '/' + IntToStr(CP.X+1);
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.TCEditsChange(Sender: TObject);
begin
	SCEdited.ActiveListIndex := TCEdits.TabIndex;
	LBxEdits.ItemIndex := TCEdits.TabIndex;
	MeText.Lines.Assign(SCEdited.ActiveList);
	MeText.SetFocus;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.TCEditsChanging(Sender: TObject; var AllowChange: Boolean);
begin
	SCEdited.ActiveList.Assign(MeText.Lines);
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.TSFormShow(Sender: TObject);
var
	I: Integer;
begin
	TCEdits.Tabs.Assign(SCEdited.ListNames);
	LBxEdits.Items.Assign(SCEdited.ListNames);
	TCEdits.TabIndex := SCEdited.ActiveListIndex;
	LBxEdits.ItemIndex := SCEdited.ActiveListIndex;
	MeText.Lines.Assign(SCEdited.ActiveList);
	MeText.SetFocus;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.XListClick(Sender: TObject);
begin
	LBxEdits.Visible := XList.Checked;
	SPLB.Visible := XList.Checked;
end;

//***************************************************************************************
procedure TFrmEditStringsContainer.XTabsClick(Sender: TObject);
begin
	TCEdits.Visible := XTabs.Checked;
end;



end.

