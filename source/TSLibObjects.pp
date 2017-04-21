unit TSLibObjects;

interface

uses
	Graphics,
	Forms,
	Classes,
	Types,
	ExtCtrls,
	StdCtrls,
	Controls;

type
	NFormDispMode = (fdmCreate, fdmShow, fdmShowModal);
  NFontPart = (fpName, fpSize, fpColor, fpStyle, fpStyleBold, fpStyleItalic, fpStyleUnderline, fpStyleStrikeout);
  SFontParts = set of NFontPart;

	MControlAction = procedure (AControl: TControl; var GoOn: Boolean) of object;
	PrcControlAction = procedure (AControl: TControl; var GoOn: Boolean);

//Wenn Reference=nil, wird ein Formular der Klasse FormClass erzeugt und Reference zugeweisen.
// War das Formular bei Aufruf bereits erzeugt - nichts.
// Wenn DispMode fdmShow oder fdmShowModal enthält, wird das Formular sofort ensprechend angezeigt.

///<summary>
///  If Reference=nil, a form of class FormClass gets created and assigned to Reference.
///  if Reference is assigned, no form is created.
///</summary>
///  <returns>A TForm-Reference to the existing or newly created form instance.</returns>
///  <param name="FormClass">Class of the form to be created<param>
///  <param name="Reference">Reference variable for the form.
///     If this reference is nil, a new form instance is created.
///  </param>
///  <param name="DispMode">Defines, how the form should be shown after the it's existence is verified:<br>
///     fdmCreate: do nothing, only make sure it's created
///     fdmShow: show the form (i.e.: Show is called).
///     fdmShowModal: show the form and wait for a result (i.e.: ShowModal is called).
///  </param>
///
function VerifyForm(FormClass: TFormClass; var Reference; DispMode: NFormDispMode=fdmCreate): TForm;
procedure Restart(Timer: TTimer; SetEnabled: Boolean = False; NewInterval: Integer = -1);
function CurrentIndent(Memo: TCustomMemo; IndentChar: Char = #9): Integer;

{
	Die Object(s)Enabled und Object(s)Visible Funktionen können einzelne Objekte (oder Controls)
	aktivieren, deaktiviern, sichtbar oder unsichtbar machen.
	Sie arbeiten dabei mit den Prozeduren
		DisableObject(s), DisableComponent(s)ByName, ClearDisabled
	und
		HideObject(s), HideComponent(s)ByName, ClearHideen
	zusammen.
	Mit den Disable oder Hide-Prozeduren werden Objekte/Komponenten permanent deaktiviert
	oder verborgen.
	Die Prozeduren xxEnabled und xxVisible prüfen vor der Aktivierung / Anzeige, für jedes
	Objekt, ob es zulässig ist, dises sichtbar zu machen. Nur, wenn das Objekt nicht in
	der Liste der permanent deaktivierten/verborgenen Objekte enthalten ist, wird die
	Prozedur ausgeführt.
	Deaktivierte oder verborgene Objekte können mit dem Prozeduren ClearDisabled und
	ClearHidden einzeln oder als Gruppe reaktiviert werden.
}

//Setzt den Enabled-Status aller in OfContainer enthaltenen Controls.
procedure ContentsEnabled(OfContainer: TWinControl; State: Boolean;
													IncludeContainer: Boolean = False; Recursive: Boolean = False);

//Setzt den Enabled-Status aller im Array Objects enthaltenen Objekte, sofern diese über
//eine published-Eigenschaft Enabled verfügen und nicht permanent deaktiviert sind.
procedure ObjectsEnabled(Objects: array of TPersistent; State: Boolean);
function ObjectEnabled(AObject: TPersistent; State: Boolean): Boolean;  //gibt den tatsächlichen Status zurück

//Setzt den Visible-Status aller im Array Objects enthaltenen Objekte, sofern diese über
//eine published-Eigenschaft Visible verfügen und nicht permanent verborgen sind.
procedure ObjectsVisible(Objects: array of TPersistent; State: Boolean);
function ObjectVisible(AObject: TPersistent; State: Boolean): Boolean; //gibt den tatsächlichen Status zurück

//Setzt den ReadOnly-Status aller im Array Objects enthaltenen Objekte, sofern diese über
//eine published-Eigenschaft ReadOnly verfügen.
procedure ObjectsReadOnly(Objects: array of TPersistent; State: Boolean;
																		SetTabStop: Boolean = True; StateColor: TColor=clNone);
function ObjectReadOnly(AObject: TPersistent; State: Boolean;
												SetTabStop: Boolean = True; StateColor: TColor=clNone): Boolean;  //gibt den tatsächlichen Status zurück

//Entfernt einzelne Objekte, eine Gruppe oder alle Objekte aus der Liste der deaktivierten Objekte.
procedure ClearDisabled; overload;
procedure ClearDisabled(AObject: TPersistent); overload;
procedure ClearDisabled(Objects: array of TPersistent); overload;
//Deaktiviert einzelne Objekte oder Gruppen von Objekten permanent durch Hinzufügen zur Liste
//	"DisabledObjects"
procedure DisableObject(AObject: TPersistent);
procedure DisableObjects(Objects: array of TPersistent);
procedure DisableComponentByName(Owner: TComponent; CompName: string);
procedure DisableComponentsByName(Owner: TComponent; CompName: TStrings);

//Entfernt einzelne Objekte, eine Gruppe oder alle Objekte aus der Liste der verborgenen Objekte.
procedure ClearHidden; overload;
procedure ClearHidden(AObject: TPersistent); overload;
procedure ClearHidden(Objects: array of TPersistent); overload;
//Verbirgt einzelne Objekte oder Gruppen von Objekten permanent durch Hinzufügen zur Liste
//	"InvisibleObjects"
procedure HideObject(AObject: TPersistent);
procedure HideObjects(Objects: array of TPersistent);
procedure HideComponentByName(Owner: TComponent; CompName: string);
procedure HideComponentsByName(Owner: TComponent; CompName: TStrings);


//Führt für jedes Control, das "OfParent" untergeordnet ist, die Prozedur "DoAction" aus.
// Ist der rückgabeparamteter "GoOn" dieser Prozedur = False, wird die Schleife abgebrochen.
// Der Rückgabewert zeigt an, ob die Ausführung abgebrochen (False) oder zu Ende geführt wurde (True).
function WithAllControls(OfParent: TWinControl; DoAction: MControlAction): Boolean; overload;
function WithAllControls(OfParent: TWinControl; DoAction: PrcControlAction): Boolean; overload;

//Führt das OnClick-Ereignis von Control aus, wenn diesem eine Methode zugewiesen ist.
function ExecClick(Control: TControl): Boolean;

//Versucht, den Focus auf das angegeben Control zu setzen. Das Result zeigt an, ob der Versuch erfolgreich war.
function TryFocus(Ctrl: TWinControl): Boolean;

//Setzt Größe und Position der in Container enthaltenen Controls so, dass die den Container
// vollständig ausfüllen. RowCount gibt die Anzahl Zeilen an, in denen die Controls ange-
// ordnet werden. Spacing gibt den Abstand (in Pixeln) zwischen den Controls an (default=(4,4)).
// Für die Positionierung wird die TAG-Eigenschaft aller betroffenen Controls verwendet:
// 1. Das Tag des Containers gibt an, wie viele Plätze bereitgestellt werden sollen.
// 2. Die Tags der Controls (0 bis n-1) geben n, an welcher Position das Control plaziert wird.
// 3. Wenn das Tag eines Controls <0 ist, wird es bei der Größen- und Positionsanpassung ignoriert.
// Damit die Controls ihren Container immer ausfüllen, sollte diese Prozedur im "OnResize"-Ereignis
// des Containers verwendet werden.
procedure FillWithControls(Container: TWinControl; RowCount: Integer=1); overload;
procedure FillWithControls(Container: TWinControl; Spacing: TPoint; RowCount: Integer=1); overload;

//Die Funktion schaltet abhängig von "FirstVisible" entweder Obj1 oder Obj2 sichtbar,
//	sofern dies zulässig ist ("ObjectEnabled" + not ObjectHidden)
// Als Ergebnis wird der Visible-Status von Obj1 zurückgegeben.
function ToggleVisible(Obj1, Obj2: TPersistent; FirstVisible: Boolean): Boolean;

//These functions are used to access the private array "GrayOutControls".
// It holds all the classes, whose controls should be set to a gray Background, when
// disabled by the function ObjectVisible or procedure ObjectsVisible.
function AddGrayOutControl(CtrlClass: TControlClass): Boolean;
function RemoveGrayOutControl(CtrlClass: TControlClass): Boolean;
function IsGrayOutControl(CtrlClass: TControlClass): Boolean; overload;
function IsGrayOutControl(AObject: TObject): Boolean; overload;


procedure AssignFontParts(ToControl: TControl; FromFont: TFont; Parts: SFontParts=[fpName .. fpStyle]);


function SetObjectOrdProp(ToObject: TObject; PropName: string; Value: Integer): Boolean;
function SetObjectStrProp(ToObject: TObject; PropName: string; Value: string): Boolean;
function GetObjectOrdProp(FromObject: TObject; PropName: string; Default: Int64=0): Int64;
function GetObjectStrProp(FromObject: TObject; PropName: string; Default: string=''): string;


implementation

uses
	TSLib,
	TypInfo,
	Contnrs;

type
	TControlCracker = class(TControl);

const
	StdSpacing: TPoint = (X:4; Y:4);

var
	DisabledObjects: TObjectList=nil;
	InvisibleObjects: TObjectList=nil;
	GrayOutControls: array of TControlClass;


//---------------------------------------------------------------------------------------
function VerifyForm(FormClass: TFormClass; var Reference; DispMode: NFormDispMode=fdmCreate): TForm;
begin
	if not Assigned(TForm(Reference)) then
		Application.CreateForm(FormClass, TForm(Reference));
	Result := TForm(Reference);
	case DispMode of
	fdmShow:
		Result.Show;
	fdmShowModal:
		Result.ShowModal;
	end;
end; //VerifyForm

//---------------------------------------------------------------------------------------
procedure Restart(Timer: TTimer; SetEnabled: Boolean = False; NewInterval: Integer = -1);
begin
	if NewInterval > -1 then
		Timer.Interval := NewInterval;
	if not (Timer.Enabled or SetEnabled) then
		Exit;
	Timer.Enabled := False;
	Timer.Enabled := True;
end;

//---------------------------------------------------------------------------------------
function CurrentIndent(Memo: TCustomMemo; IndentChar: Char = #9): Integer;
var
	LNr: Integer;
	L: string;
begin
	LNr := Memo.CaretPos.Y;
	L := Memo.Lines[LNr];
	Result := 0;
	if (Length(L) > 0) then
	begin
		repeat
			Inc(Result);
		until (Result > Length(L))
			or (L[Result] <> IndentChar);
		Dec(Result);
	end;
end;

//---------------------------------------------------------------------------------------
// nur zur Internen Verwendung durch ObjectsVisible
// Gibt zurück, ob der Visible-Status des Objektes geändert werden konnte.
function SetItVisible(AObject: TObject; State: Boolean): Boolean;
var
	VisiblePropInfo: PPropInfo;
begin
	//Wenn das Objekt in der Liste "InvisibleObjects" enthalten ist, ignoriere Aktivierungsbefehle.
	Result := InvisibleObjects.IndexOf(AObject) < 0;
	if not Result then
		State := False;

	if AObject is TControl then
		TControl(AObject).Visible := State
	else begin
		VisiblePropInfo := GetPropInfo(AObject.ClassInfo, 'Visible');
		if Assigned(VisiblePropInfo)
			and (VisiblePropInfo^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration])
    then
			SetOrdProp(AObject, VisiblePropInfo, Integer(State))
		else
			Result := False;
	end;
end;

//---------------------------------------------------------------------------------------
function ObjectVisible(AObject: TPersistent; State: Boolean): Boolean;
begin
	Result := SetItVisible(AObject, State);
end;

//---------------------------------------------------------------------------------------
// Zeigt / verbirgt alle im Array enthaltenen Objekte, sofern diese
//		eine Boolesche Eigenschaft "Visible" besitzen.
procedure ObjectsVisible(Objects: array of TPersistent; State: Boolean);
var
	I: Integer;
begin
	for I := Low(Objects) to High(Objects) do
		SetItVisible(Objects[I], State);
end;

//---------------------------------------------------------------------------------------
function SetItReadOnly(AObject: TObject; State, SetTabStop: Boolean; StateColor: TColor): Boolean;
var
	ReadOnlyPropInfo: PPropInfo;
begin
	Result := True;
	ReadOnlyPropInfo := GetPropInfo(AObject.ClassInfo, 'ReadOnly');
	if Assigned(ReadOnlyPropInfo)
	and (ReadOnlyPropInfo^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration]) then
		SetOrdProp(AObject, ReadOnlyPropInfo, Integer(State))
	else
		Result := False;
	if SetTabStop and (AObject is TWinControl) then
		TWinControl(AObject).TabStop := not State;
	if (StateColor<>clNone)
	and IsGrayOutControl(AObject) then
//	and IsGrayOutControl(TControlClass(AObject.ClassType)) then
		TControlCracker(AObject).Color := StateColor;
end;

//---------------------------------------------------------------------------------------
function ObjectReadOnly(AObject: TPersistent; State: Boolean;
													SetTabStop: Boolean = True; StateColor: TColor=clNone): Boolean;  //gibt den tatsächlichen Status zurück
begin
	Result := SetItReadOnly(AObject, State, SetTabStop, StateColor);
end;

//---------------------------------------------------------------------------------------
procedure ObjectsReadOnly(Objects: array of TPersistent; State: Boolean;
																		SetTabStop: Boolean = True; StateColor: TColor=clNone);
var
	I: Integer;
begin
	for I := Low(Objects) to High(Objects) do
		SetItReadOnly(Objects[I], State, SetTabStop, StateColor);
end;

//---------------------------------------------------------------------------------------
function AddGrayOutControl(CtrlClass: TControlClass): Boolean;
var
	I: Integer;
begin
	for I := Low(GrayOutControls) to High(GrayOutControls) do
		if GrayOutControls[I] = CtrlClass then begin
			Result := False;
			Exit;
		end;
	I := Length(GrayOutControls);
	SetLength(GrayOutControls, I+1);
	GrayOutControls[I] := CtrlClass;
	Result := True;
end;

//---------------------------------------------------------------------------------------
function RemoveGrayOutControl(CtrlClass: TControlClass): Boolean;
var
	I, J, K: Integer;
begin
	K := High(GrayOutControls);
	for I := Low(GrayOutControls) to K do
		if GrayOutControls[I] = CtrlClass then begin
			for J := I to pred(K) do
				GrayOutControls[J] := GrayOutControls[J+1];
			SetLength(GrayOutControls, K);
			Result := True;
			Exit;
		end;
	Result := False;
end;

//---------------------------------------------------------------------------------------
function IsGrayOutControl(CtrlClass: TControlClass): Boolean;
var
	I: Integer;
begin
	for I := Low(GrayOutControls) to High(GrayOutControls) do
		if CtrlClass = GrayOutControls[I] then begin
			Result := True;
			Exit;
		end;
	Result := False;
end;

//---------------------------------------------------------------------------------------
function IsGrayOutControl(AObject: TObject): Boolean; overload;
var
	I: Integer;
begin
	for I := Low(GrayOutControls) to High(GrayOutControls) do
		if AObject is GrayOutControls[I] then begin
			Result := True;
			Exit;
		end;
	Result := False;
end;

//---------------------------------------------------------------------------------------
// nur zur Internen Verwendung durch ObjectsEnabled und ContentsEnabled
// Gibt zurück, ob der Visible-Status des Objektes geändert werden konnte.
function SetItEnabled(AObject: TObject; State: Boolean): Boolean;
var
	EnabledPropInfo: PPropInfo;
begin
	//Wenn das Objekt in der Liste "DisabledObjects" enthalten ist, ignoriere Aktivierungsbefehle.
	Result := DisabledObjects.IndexOf(AObject) < 0;
	if not Result then
		State := False;

	if AObject is TControl then begin
		TControl(AObject).Enabled := State;
//		if IsGrayOutControl(TControlClass(AObject.ClassType)) then
		if IsGrayOutControl(AObject) then
//		if (AObject is TCustomEdit)
//		or (AObject is TCustomListControl)
//		or (AObject is TDBLookupControl)
			TControlCracker(AObject).Color := IIf(State, clWindow, $00E6E6E6);
	end
	else begin
		EnabledPropInfo := GetPropInfo(AObject.ClassInfo, 'Enabled');
		if Assigned(EnabledPropInfo)
		and (EnabledPropInfo^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration]) then
			SetOrdProp(AObject, EnabledPropInfo, Integer(State))
		else
			Result := False;
	end;
end;

//---------------------------------------------------------------------------------------
function ObjectEnabled(AObject: TPersistent; State: Boolean): Boolean;
begin
	Result := SetItEnabled(AObject, State);
end;

//---------------------------------------------------------------------------------------
// Aktiviert / deaktiviert alle im Array enthaltenen Objekte, sofern diese
//		eine Boolesche Eigenschaft "Enabled" besitzen.
procedure ObjectsEnabled(Objects: array of TPersistent; State: Boolean);
var
	I: Integer;
begin
	for I := Low(Objects) to High(Objects) do
		SetItEnabled(Objects[I], State);
end;

//---------------------------------------------------------------------------------------
// Wenn OfParent deaktivierbare Objekte enthält, so werden diese deaktiviert.
// Sonst wird OfContainer selbst deaktiviert.
procedure ContentsEnabled(OfContainer: TWinControl; State: Boolean;
													IncludeContainer: Boolean = False; Recursive: Boolean = False);
var
	I: Integer;
begin
	if IncludeContainer then
		SetItEnabled(OfContainer, State);
	//Deaktiviere alle im Container enthaltenen Steuerelemente.
	for I := 0 to OfContainer.ControlCount-1 do begin
		//Das untergeordnete Element ist ein Container: Rekursiver Aufruf.
		if Recursive and (OfContainer.Controls[I] is TWinControl) then
			ContentsEnabled(TWinControl(OfContainer.Controls[I]), State)
		else
			SetItEnabled(OfContainer.Controls[I], State);
	end;
end; //ContentsEnabled

//---------------------------------------------------------------------------------------
procedure ClearDisabled;
begin
	DisabledObjects.Clear;
end;

//---------------------------------------------------------------------------------------
procedure ClearDisabled(AObject: TPersistent); overload;
begin
	DisabledObjects.Remove(AObject);
end;

//---------------------------------------------------------------------------------------
procedure ClearDisabled(Objects: array of TPersistent); overload;
var
	I: Integer;
begin
	for I := Low(Objects) to High(Objects) do
		DisabledObjects.Remove(Objects[I]);
end;

//---------------------------------------------------------------------------------------
procedure DisableObjects(Objects: array of TPersistent);
var
	I: Integer;
begin
	for I := Low(Objects) to High(Objects) do
		if SetItEnabled(Objects[I], False) then
			DisabledObjects.Add(Objects[I]);
end;

//---------------------------------------------------------------------------------------
procedure DisableComponentByName(Owner: TComponent; CompName: string);
var
	C: TComponent;
begin
	C := Owner.FindComponent(CompName);
	if Assigned(C) and SetItEnabled(C, False) then
		DisabledObjects.Add(C);

end;

//---------------------------------------------------------------------------------------
procedure DisableComponentsByName(Owner: TComponent; CompName: TStrings);
var
	I: Integer;
	C: TComponent;
begin
	for I := 0 to pred(Owner.ComponentCount) do begin
		C := Owner.Components[I];
		if (CompName.IndexOf(C.Name) >= 0) and SetItEnabled(C, False) then
			DisabledObjects.Add(C);
	end;
end;

//---------------------------------------------------------------------------------------
procedure DisableObject(AObject: TPersistent);
begin
	if SetItEnabled(AObject, False) then
		DisabledObjects.Add(AObject);
end;

//---------------------------------------------------------------------------------------
procedure ClearHidden;
begin
	InvisibleObjects.Clear;
end;

//---------------------------------------------------------------------------------------
procedure ClearHidden(AObject: TPersistent); overload;
begin
	InvisibleObjects.Remove(AObject);
end;

//---------------------------------------------------------------------------------------
procedure ClearHidden(Objects: array of TPersistent); overload;
var
	I: Integer;
begin
	for I := Low(Objects) to High(Objects) do
		InvisibleObjects.Remove(Objects[I]);
end;

//---------------------------------------------------------------------------------------
procedure HideObject(AObject: TPersistent);
begin
	if SetItVisible(AObject, False) then
		InvisibleObjects.Add(AObject);
end;

//---------------------------------------------------------------------------------------
procedure HideObjects(Objects: array of TPersistent);
var
	I: Integer;
begin
	for I := Low(Objects) to High(Objects) do
		if SetItVisible(Objects[I], False) then
			InvisibleObjects.Add(Objects[I]);
end;

//---------------------------------------------------------------------------------------
procedure HideComponentByName(Owner: TComponent; CompName: string);
var
	C: TComponent;
begin
	C := Owner.FindComponent(CompName);
	if Assigned(C) and SetItVisible(C, False) then
		InvisibleObjects.Add(C);
end;

//---------------------------------------------------------------------------------------
procedure HideComponentsByName(Owner: TComponent; CompName: TStrings);
var
	I: Integer;
	C: TComponent;
begin
	for I := 0 to pred(Owner.ComponentCount) do begin
		C := Owner.Components[I];
		if (CompName.IndexOf(C.Name) >= 0) and SetItVisible(C, False) then
			InvisibleObjects.Add(C);
	end;
end;


//	MControlAction = procedure (AControl: TControl; var GoOn: Boolean);
//---------------------------------------------------------------------------------------
function WithAllControls(OfParent: TWinControl; DoAction: MControlAction): Boolean; overload;
var
	I: Integer;
	C: TControl;
begin
	Result := True;
	for I := 0 to pred(OfParent.ControlCount) do begin
		C := OfParent.Controls[I];
		DoAction(C, Result);
		if (C is TWinControl) {and (csAcceptsControls in C.ControlStyle) }and Result then
			Result := WithAllControls(TWinControl(C), DoAction);
		if not Result then
			Break;
	end;
end;

//---------------------------------------------------------------------------------------
function WithAllControls(OfParent: TWinControl; DoAction: PrcControlAction): Boolean; overload;
var
	I: Integer;
	C: TControl;
begin
	Result := True;
	for I := 0 to pred(OfParent.ControlCount) do begin
		C := OfParent.Controls[I];
		DoAction(C, Result);
		if (C is TWinControl) {and (csAcceptsControls in C.ControlStyle)} and Result then
			Result := WithAllControls(TWinControl(C), DoAction);
		if not Result then
			Break;
	end;
end;

//---------------------------------------------------------------------------------------
procedure FillWithControls(Container: TWinControl; RowCount: Integer=1); overload;
begin
	FillWithControls(Container, StdSpacing, RowCount);
end;

//---------------------------------------------------------------------------------------
procedure FillWithControls(Container: TWinControl; Spacing: TPoint; RowCount: Integer=1); overload;
var
	I, Cols, CC, W, H, PX, PY: Integer;
begin
{	if not (csAcceptsControls in Container.ControlStyle) then
		Exit;}
	CC := Container.Tag;
	if CC = 0 then
		Exit;
	if RowCount > CC then
		RowCount := CC;
	Cols := CC div RowCount + CC mod RowCount;
	if Cols = 0 then
		Exit;
	PX := Container.ClientWidth div Cols;
	PY := Container.ClientHeight div RowCount;
	if Spacing.X < 0 then
		Spacing.X := Round(-Spacing.X/100*PX);
	if Spacing.Y < 0 then
		Spacing.Y := Round(-Spacing.Y/100*PY);
	W := PX - Spacing.X;
	H := PY - Spacing.Y;

	for I := 0 to pred(Container.ControlCount) do
	with Container.Controls[I] do
		if Tag >= 0 then begin
			Left := (Tag div RowCount)*PX + Spacing.X div 2;
			Top := (Tag mod RowCount)*PY + Spacing.Y div 2;
			Height := H;
			Width := W
		end;
end;

//---------------------------------------------------------------------------------------
function ExecClick(Control: TControl): Boolean;
begin
	if Control.Visible
	and Control.Enabled
	and Assigned(TControlCracker(Control).OnClick) then begin
		TControlCracker(Control).OnClick(Control);
		Result := True;
	end
	else
		Result := False;
end;

//---------------------------------------------------------------------------------------
function TryFocus(Ctrl: TWinControl): Boolean;
begin
  Result := Ctrl.CanFocus;
  if Result then
    Ctrl.SetFocus;
end;

//---------------------------------------------------------------------------------------
function ToggleVisible(Obj1, Obj2: TPersistent; FirstVisible: Boolean): Boolean;
begin
	if FirstVisible then begin
		Result := ObjectVisible(Obj1, True);
		if Result then
			ObjectVisible(Obj2, False);
	end
	else begin
		Result := not ObjectVisible(Obj2, True);
		if not Result then
			ObjectVisible(Obj1, False);
	end;
end;

//---------------------------------------------------------------------------------------
procedure AssignFontParts(ToControl: TControl; FromFont: TFont; Parts: SFontParts);
var
  FS: TFontStyles;
  StyleChanged: Boolean;
begin
  if fpName in Parts then
    TControlCracker(ToControl).Font.Name := FromFont.Name;
  if fpColor in Parts then
    TControlCracker(ToControl).Font.Color := FromFont.Color;
  if fpSize in Parts then
    TControlCracker(ToControl).Font.Size := FromFont.Size;

  if fpStyle in Parts then
    TControlCracker(ToControl).Font.Style := FromFont.Style
  else if [fpStyleBold..fpStyleStrikeout] * Parts <> [] then
  begin
    FS := TControlCracker(ToControl).Font.Style;
    StyleChanged := False;
    if fpStyleBold in Parts then
    begin
      StyleChanged := True;
      if fsBold in FromFont.Style then
        Include(FS, fsBold)
      else
        Exclude(FS, fsBold);
    end;
    if fpStyleItalic in Parts then
    begin
      StyleChanged := True;
      if fsItalic in FromFont.Style then
        Include(FS, fsItalic)
      else
        Exclude(FS, fsItalic);
    end;
    if fpStyleStrikeout in Parts then
    begin
      StyleChanged := True;
      if fsStrikeOut in FromFont.Style then
        Include(FS, fsStrikeOut)
      else
        Exclude(FS, fsStrikeOut);
    end;
    if fpStyleUnderline in Parts then
    begin
      StyleChanged := True;
      if fsUnderline in FromFont.Style then
        Include(FS, fsUnderline)
      else
        Exclude(FS, fsUnderline);
    end;
    if StyleChanged then
      TControlCracker(ToControl).Font.Style := FS;
  end;
end;

function SetObjectOrdProp(ToObject: TObject; PropName: string; Value: Integer): Boolean;
var
  ThisPropInfo: PPropInfo;
begin
	ThisPropInfo := GetPropInfo(ToObject.ClassInfo, PropName);
	if Assigned(ThisPropInfo)
  	and (ThisPropInfo^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration])
	then begin
  	SetOrdProp(ToObject, ThisPropInfo, Integer(Value));
    Result := True;
	end
	else
  	Result := False;
end;

function SetObjectStrProp(ToObject: TObject; PropName: string; Value: string): Boolean;
var
  ThisPropInfo: PPropInfo;
begin
	ThisPropInfo := GetPropInfo(ToObject.ClassInfo, PropName);
	if Assigned(ThisPropInfo)
  	and (ThisPropInfo^.PropType^.Kind in [tkChar, tkUChar, tkUString, tkWChar,
    																				tkWString, tkAString, tkSString, tkLString])
	then begin
  	SetStrProp(ToObject, ThisPropInfo, Value);
    Result := True;
	end
	else
  	Result := False;
end;

function GetObjectOrdProp(FromObject: TObject; PropName: string; Default: Int64): Int64;
var
  ThisPropInfo: PPropInfo;
begin
	ThisPropInfo := GetPropInfo(FromObject.ClassInfo, PropName);
	if Assigned(ThisPropInfo)
  	and (ThisPropInfo^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration])
	then
  	Result := GetOrdProp(FromObject, ThisPropInfo)
  else
  	Result := Default;
end;

function GetObjectStrProp(FromObject: TObject; PropName: string; Default: string): string;
var
  ThisPropInfo: PPropInfo;
begin
	ThisPropInfo := GetPropInfo(FromObject.ClassInfo, PropName);
	if Assigned(ThisPropInfo)
  	and (ThisPropInfo^.PropType^.Kind in [tkChar, tkUChar, tkUString, tkWChar,
    																				tkWString, tkAString, tkSString, tkLString])
	then
  	GetStrProp(FromObject, ThisPropInfo)
  else
  	Result := Default;
end;

//***************************************************************************************
//***************************************************************************************
//***************************************************************************************
//***************************************************************************************

initialization
	DisabledObjects := TObjectList.Create(False);
	InvisibleObjects := TObjectList.Create(False);
	AddGrayOutControl(TCustomEdit);
	AddGrayOutControl(TCustomListBox);
	AddGrayOutControl(TCustomComboBox);

finalization
	DisabledObjects.Free;
	InvisibleObjects.Free;
	SetLength(GrayOutControls, 0);

end.

