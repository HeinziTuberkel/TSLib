unit TS_DBGrid;
{$I PackageDefs.inc}

interface

uses
	LMessages,
	TS_CustomINI,
  TSClasses,
  TSClassesDB,
  TSLibConvert,
  StdCtrls,
  ExtCtrls,
  Grids,
{$IFDEF USEZEOS}
	ZAbstractRODataSet,
{$ENDIF}
{$IFDEF SQLDB}
  SQLDB,
{$ENDIF}
  DB, DBGrids, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TTSDBGrid = class;
  TTSColumn = class;
  TCustomEditCracker = class(TCustomEdit);

  NTSTriggerArea = (ttaHeader, ttaCells);
  STSTriggerAreas = set of NTSTriggerArea;
  NTSGridSortOrder = (gsoNoSort, gsoAscending, gsoDescending);
  NTSGridSortType = (gstManual, gstSQLLocal, gstSQLServer, gstZEOSLocal, gstZEOSSQL);
  NTSColumnFilterType = (cftNone, cftAuto, cftInteger, cftNumeric, cftBoolean, cftDate,
  												cftTime, cftDateTime, cftString, cftText, cftLikeText);
  NTSGridFilterType = (gftLocalDataset, gftManual, gftSQLdb, gftZEOS);
  NTSGridFilterEdit = (gfeManual, gfeOptionsForm, gfeTitleBar, gfeTitleBarAutoHide,
  																						gfeTitleBarAlwaysHide, gfePanel);
  NTSGridOptionColumn = (gocVisible, gocFilter, gocFilterActive, gocColumnWidth,
  																						gocSorting);
  STSGridOptionColumns = set of NTSGridOptionColumn;

  MTSGridSortChange = procedure(Sender: TTSDBGrid; NewSortOrder: TStrings) of object;
  MTSGridFilterChange = procedure(Sender: TTSDBGrid; Column: Integer; var FilterText: string) of object;
  MTSGridFilterEdit = procedure(Sender: TTSDBGrid; Column: Integer) of object;
	MTSGridFilterCanEdit = procedure(Sender: TTSDBGrid; Column: Integer; var CanEdit: Boolean) of object;
{$IFDEF SQLDB}
	TTSSQLQuery = class(TCustomSQLQuery)
	public
		property ServerFilter;
		property ServerFiltered;
	end;
{$ENDIF}
{$IFDEF USEZEOS}
  TTSZAbstractRODataset = class(TZAbstractRODataset)
  public
    property SQL;
  end;
{$ENDIF}

  RMouseDownState = record
		Button: TMouseButton;
		X, Y: Integer;
    MouseShift: TShiftState;
  end;

  RColumnContent = record
    Index: integer;
    FieldName: string;
    Width: Integer;
    Visible: Boolean;
  end;

  TTSGridCellStyle = class(TPersistent)
  private
    fGrid: TTSDBGrid;
    fBackground: TColor;
    fFont: TFont;
    procedure SetBackground(const Value: TColor);
    procedure FontChanged(Sender: TObject);
    function StoreFont: Boolean;
  public
    constructor Create(Grid: TTSDBGrid);
    destructor Destroy; override;
  published
    property Font: TFont read fFont write fFont stored StoreFont;
    property Background: TColor read fBackground write SetBackground default clNone;
  end;

  { TTSGridTrigger }

  TTSGridTrigger = class(TTSTrigger)
  private
    fGrid: TCustomGrid;
    fTrgArea: STSTriggerAreas;
  public
    constructor Create(AGrid: TCustomGrid);
    function Fired(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
  published
    property MouseArea: STSTriggerAreas read fTrgArea write fTrgArea;
  end;

  { TTSColumnFilter }

  TTSColumnFilter = class(TPersistent)
  private
    fFilterEdit: TCustomEdit;
    fInternalEditChange: Boolean;
    fAllowRange: Boolean;
    fColumn: TTSColumn;
    fEnabled: Boolean;
    fFilter: string;
    fFilterDisplay: string;
    fFilterType: NTSColumnFilterType;
    fFilterSQL: string;
    fUseSQL: Boolean;
    procedure SetAllowRange(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFilterText(const AValue: string);
    procedure SetFilterType(const AValue: NTSColumnFilterType);
    procedure SetFilterSQL(const AValue: string);
    procedure SetUseSQL(const AValue: Boolean);
  protected
    procedure FilterChanged; virtual;
    procedure CreateFilterEdit; virtual;
    function  CheckShowEdit(Focus: Boolean=False): Boolean; virtual;
    procedure ShowEdit(Focus: Boolean=False); virtual;
    procedure HideEdit; virtual;
    procedure UpdateEdit; virtual;
    procedure EditChange(Sender: TObject); virtual;
    procedure EditEnter(Sender: TObject); virtual;
    procedure EditExit(Sender: TObject); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
  public
    constructor Create(Column: TTSColumn);
    destructor Destroy; override;
	  procedure Assign(Source: TPersistent); override;
    procedure UpdateFilter;
    procedure Clear;
    property Column: TTSColumn read fColumn;
    property DisplayText: string read fFilterDisplay;
  published
    property Enabled: Boolean read fEnabled write SetEnabled default True;
    property FilterText: string read fFilter write SetFilterText;
    property FilterSQL: string read fFilterSQL write SetFilterSQL stored fUseSQL;
    property FilterType: NTSColumnFilterType read fFilterType write SetFilterType default cftAuto;
    property AllowRange: Boolean read fAllowRange write SetAllowRange default True;
    property UseFilterSQL: Boolean read fUseSQL write SetUseSQL default False;
  end;

  { TTSColumn }

  TTSColumn = class(TColumn)
  private
    fColFilter: TTSColumnFilter;
    fSortOrder: NTSGridSortOrder;
    fSortPos: Integer;
    fCreating: Boolean;
    function GetColWidth: Integer;
    function GetGrid: TTSDBGrid;
    function GetSortOrder: NTSGridSortOrder;
    function GetSortPos: Integer;
    procedure SetColWidth(const AValue: Integer);
    procedure SetSortOrder(const AValue: NTSGridSortOrder);
    procedure SetSortPos(AValue: Integer);
		function IsVisibleStored: Boolean;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
	  procedure Assign(Source: TPersistent); override;
    procedure ColumnChanged; override;
    function DefaultWidth: Integer;
    procedure ChangeSort(NewOrder: NTSGridSortOrder; NewPos: Integer);
    property Grid: TTSDBGrid read GetGrid;
    property ColWidth: Integer read GetColWidth write SetColWidth;
  published
    property Filter: TTSColumnFilter read fColFilter write fColFilter;
    property SortingPosition: Integer read GetSortPos write SetSortPos;
    property SortingOrder: NTSGridSortOrder read GetSortOrder write SetSortOrder;
  end;

  { TTSDBGridColumns }

  TTSDBGridColumns = class(TDBGridColumns)
  private
    function GetColumn(Index: Integer): TTSColumn;
    function GetGrid: TTSDBGrid;
    procedure SetColumn(Index: Integer; Value: TTSColumn);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function DesignColumns: Boolean;
    function Add: TTSColumn;
    function FindColumn(Field: TField): TTSColumn;
    property Grid: TTSDBGrid read GetGrid;
    property Items[Index: Integer]: TTSColumn read GetColumn write SetColumn; default;
  end;

  { TTSGridFilter }

  TTSGridFilter = class(TPersistent)
  private
    fDesignActive: Boolean;
    fEditType: NTSGridFilterEdit;
    fPanel: TCustomPanel;
  	fRebuilding: Boolean;
    fEditing: Boolean;
    fTitelBarMark: Boolean;
    fEnabled: Boolean;
    fFilterText: string;
    fFilterType: NTSGridFilterType;
    fGrid: TTSDBGrid;
    fSuccessive: Boolean;
    fTrigger: TTSGridTrigger;
    fWildCard: string;
    fBoolDisplay: array[NTriState] of string;
    fBoolSQL: array[NTriState] of string;
    function GetBoolDisplay(BoolVal: NTriState): string;
    function GetBoolSQL(BoolVal: NTriState): string;
    function GetBoolValsDisplay: string;
    function GetBoolValsSQL: string;
    procedure SetBoolDisplay(BoolVal: NTriState; const AValue: string);
    procedure SetBoolSQL(BoolVal: NTriState; const AValue: string);
    procedure SetBoolValsDisplay(const AValue: string);
    procedure SetBoolValsSQL(const AValue: string);
    procedure SetDesignActive(const AValue: Boolean);
    procedure SetEditType(AValue: NTSGridFilterEdit);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFilterText(const AValue: string);
    procedure SetFilterType(AValue: NTSGridFilterType);
    procedure SetPanel(const AValue: TCustomPanel);
    procedure SetTitleBarMark(const AValue: Boolean);
    procedure SetWildCard(const AValue: string);
    function StoreBoolDisp: Boolean;
    function StoreBoolSQL: Boolean;
    function StoreTrigger: Boolean;
    function StoreWildCard: Boolean;
  protected
    procedure HideEdits;
    procedure ShowEdits;
    procedure UpdateEdits;
    function GetFilterImage(ImgHeight: Integer=19): TBitmap;
  public
  	constructor Create(Grid: TTSDBGrid);
    destructor Destroy; override;
    procedure RebuildFilter; virtual;
    procedure Clear;
    function FilterString(Rebuild: Boolean=False): string; virtual;
    procedure FilterGrid(ChangedColumnIndex: Integer=-1); virtual;
    procedure EditColumnFilter(ColIndex: Integer);
    property EditingColumnFilter: Boolean read fEditing write fEditing;
    property BoolSQLArr: ATriStateStr read fBoolSQL;
    property BoolDispArr: ATriStateStr read fBoolDisplay;
    property BoolSQL[BoolVal: NTriState]: string read GetBoolSQL write SetBoolSQL;
    property BoolDisplay[BoolVal: NTriState]: string read GetBoolDisplay write SetBoolDisplay;
  published
    property DesignActive: Boolean read fDesignActive write SetDesignActive default False;
    property Enabled: Boolean read fEnabled write SetEnabled default True;
    property FilterType: NTSGridFilterType read fFilterType write SetFilterType default gftLocalDataset;
    property BoolValsSQL: string read GetBoolValsSQL write SetBoolValsSQL stored StoreBoolSQL;
    property BoolValsDisplay: string read GetBoolValsDisplay write SetBoolValsDisplay stored StoreBoolDisp;
    property SuccessiveFiltering: Boolean read fSuccessive write fSuccessive default True;
    property AdditionalFilter: string read fFilterText write SetFilterText;
    property StringWildcard: string read fWildCard write SetWildCard stored StoreWildCard;
    property Trigger: TTSGridTrigger read fTrigger write fTrigger stored StoreTrigger;
    property TitleBarMark: Boolean read fTitelBarMark write SetTitleBarMark default True;
    property EditType: NTSGridFilterEdit read fEditType write SetEditType default gfeTitleBarAlwaysHide;
    property EditingPanel: TCustomPanel read fPanel write SetPanel;
  end;

  { TTSGridSorting }

	TTSGridSorting = class(TPersistent)
  private
    fBGColorAsc: TColor;
    fBGColorDesc: TColor;
    fBGRect: Boolean;
    fFields: TStringList;
    fShowInTitles: Boolean;
    fSortList: array of Integer;
    fReadingSort: Boolean;
    fUpdateCount: Integer;

    fColorAsc: TColor;
    fColorDesc: TColor;
    fGrid: TTSDBGrid;
    fSortType: NTSGridSortType;
    fTrgAppend: TTSGridTrigger;
    fTrgNew: TTSGridTrigger;
    fFixedFirst: TStringList;
    fFixedLast: TStringList;
    function GetFieldNames: TStrings;
    procedure SetBGColorAsc(const AValue: TColor);
    procedure SetBGColorDesc(const AValue: TColor);
    procedure SetBGRect(const AValue: Boolean);
    procedure SetColorAsc(const AValue: TColor);
    procedure SetColorDesc(const AValue: TColor);
    procedure SetFieldNames(const AValue: TStrings);
    procedure SetShowInTitles(const AValue: Boolean);
    procedure SetSortType(const AValue: NTSGridSortType);
    procedure FieldsChanged(Sender: TObject);
    procedure FixedChanged(Sender: TObject);
    function GetFixedFirst: TStrings;
    function GetFixedLast: TStrings;
    procedure SetFixedFirst(const Value: TStrings);
    procedure SetFixedLast(const Value: TStrings);
  protected
    function CheckSortType: Boolean;
    procedure SortingChanged; virtual;
	public
		constructor Create(Grid: TTSDBGrid);
		destructor Destroy; override;
    function BuildSortString(Asc, Desc, Separator: string): string; virtual;
    procedure SetSortOrder(SortFields: string);
    procedure BuildIndexFields(out IdxName, AscFields, DescFields: string; const Separator: string=';');
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
  published
    property TriggerAppend: TTSGridTrigger read fTrgAppend write fTrgAppend;
    property TriggerNew: TTSGridTrigger read fTrgNew write fTrgNew;
    property FieldNames: TStrings read GetFieldNames write SetFieldNames;
    property FixedSortFirst: TStrings read GetFixedFirst write SetFixedFirst;
    property FixedSortLast: TStrings read GetFixedLast write SetFixedLast;
    property SortType: NTSGridSortType read fSortType write SetSortType;
    property ColorAscending: TColor read fColorAsc write SetColorAsc default clGreen;
    property ColorDescending: TColor read fColorDesc write SetColorDesc default clMaroon;
    property ShowInTitles: Boolean read fShowInTitles write SetShowInTitles default True;
    property BackgroundRect: Boolean read fBGRect write SetBGRect default True;
    property BGColorAsc: TColor read fBGColorAsc write SetBGColorAsc default clWindow;
    property BGColorDesc: TColor read fBGColorDesc write SetBGColorDesc default clWindow;
	end;

  { TTSDBGridOptions }

  TTSDBGridOptions = class(TPersistent)
  private
    fBtnPanel: Boolean;
    fColumns: STSGridOptionColumns;
    fGrid: TTSDBGrid;
    fFormCaption: string;
    fTitelClick: Boolean;
    fTitleClick: Boolean;
    fTitleMark: Boolean;
    fTrigger: TTSGridTrigger;
    procedure SetTitleMark(const AValue: Boolean);
  public
  	constructor Create(Grid: TTSDBGrid);
    destructor Destroy; override;
  published
    property FormCaption: string read fFormCaption write fFormCaption;
    property ShowTitleMark: Boolean read fTitleMark write SetTitleMark;
    property Trigger: TTSGridTrigger read fTrigger write fTrigger;
    property ButtonPanel: Boolean read fBtnPanel write fBtnPanel default False;
    property TitleClick: Boolean read fTitelClick write fTitleClick default True;
    property Columns: STSGridOptionColumns read fColumns write fColumns;
  end;

  { TTSDBGrid }

  TTSDBGrid = class(TDBGrid)
  private
    fBeforeEditFilter: MTSGridFilterCanEdit;
{$IFDEF SQLDB}
			fFieldNameDelimiters: TTSSQLFieldNameDelimiter;
{$ELSE}
			fFieldNameDelimiters: TTSFieldNameDelimiter;
{$ENDIF}
    fOnEditFilter: MTSGridFilterEdit;
    fOnEditFilterDone: MTSGridFilterEdit;
    fDoStore: Boolean;
    fFilter: TTSGridFilter;
    fMaxDefWidth: Integer;
    fOnFilterChange: MTSGridFilterChange;
    fOnSortChange: MTSGridSortChange;
    fOptionsForm: TTSDBGridOptions;
    fShowOptionsForm: TNotifyEvent;
    fSorting: TTSGridSorting;
    fStorage: TTSCustomINI;
    fMouseDownState: RMouseDownState;
		fTitleOffset: Byte;
    fActiveFilterEdit: TCustomEdit;
    fDefaultColumns: array of RColumnContent;
    fAlternateStyle: TTSGridCellStyle;
    procedure WMCommand(var Message: TLMCommand); message LM_COMMAND;
    function GetColumns: TTSDBGridColumns;
    function GetCurrentColumn: TTSColumn;
    function GetTitleRowHeight: integer;
    procedure SetColumns(const AValue: TTSDBGridColumns);
    procedure SetTitleRowHeight(const AValue: integer);
    procedure SetColumnSort(SortCol: TTSColumn);
    procedure AddColumnSort(SortCol: TTSColumn);
    procedure SetAlternateStyle(const Value: TTSGridCellStyle);
    function StoreAlternateStyle: Boolean;
  protected
    fCurrentDrawRow: Integer;
    function  CreateColumns: TGridColumns; override;

    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
		procedure GetCellProps(Field: TField; CellStyle: TTSGridCellStyle; State: TGridDrawState); virtual;
    procedure ShowPopupForm; virtual;
    function ExecMouseTrigger(Button: TMouseButton; Shift:TShiftState; X,Y:Integer): Boolean; virtual;
    procedure TopLeftChanged; override;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    procedure HeaderSized(IsColumn: Boolean; Index: Integer); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LinkActive(Value: Boolean); override;
    function ColumnPos(ColIndex: integer): Integer;
    procedure InitDefaultColumns;
    procedure ResetDefaultColumns;

    procedure BeforeDestruction; override;
    procedure LoadSettings;
    procedure SaveSettings;
    property SelectedColumn: TTSColumn read GetCurrentColumn;
    property GridStatus;
  published
		property Columns: TTSDBGridColumns read GetColumns write SetColumns;

    property AlternateRowStyle: TTSGridCellStyle read fAlternateStyle write SetAlternateStyle stored StoreAlternateStyle;
    property Filter: TTSGridFilter read fFilter write fFilter;
    property Sorting: TTSGridSorting read fSorting write fSorting;
{$IFDEF SQLDB}
    property FieldNameDelimiters: TTSSQLFieldNameDelimiter read fFieldNameDelimiters write fFieldNameDelimiters;
{$ELSE}
	  property FieldNameDelimiters: TTSFieldNameDelimiter read fFieldNameDelimiters write fFieldNameDelimiters;
{$ENDIF}
    property OptionsForm: TTSDBGridOptions read fOptionsForm write fOptionsForm;
  	property MaxColumnDefaultWidth: Integer read fMaxDefWidth write fMaxDefWidth default 200;
    property Storage: TTSCustomINI read fStorage write fStorage;
    property StoreSettings: Boolean read fDoStore write fDoStore default True;
    property TitleRowHeight: integer read GetTitleRowHeight write SetTitleRowHeight;
    property OnSortingChange: MTSGridSortChange read fOnSortChange write fOnSortChange;
    property OnFilterChange: MTSGridFilterChange read fOnFilterChange write fOnFilterChange;
    property BeforeEditFilter: MTSGridFilterCanEdit read fBeforeEditFilter write fBeforeEditFilter;
    property OnEditFilter: MTSGridFilterEdit read fOnEditFilter write fOnEditFilter;
    property OnEditFilterDone: MTSGridFilterEdit read fOnEditFilterDone write fOnEditFilterDone;
    property OnShowOptionsForm: TNotifyEvent read fShowOptionsForm write fShowOptionsForm;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;

const
  maxClickMove = 4;
  DefaultWildCard: array[NTSGridFilterType] of string = (
  									                            '*', 				//gftLocalDataset
  									                            '*', 				//gftManual
  									                            '%', 				//gftSQLdb
  									                            '%' 				//gftZEOS
                                                );

procedure Register;

implementation

uses
  CompanyConstants,
	TSLib,
	TSLibDateTime,
  TSLibDB,
  TS_DBGridOptionsForm,
  LCLType,
{$IFDEF dbg_tsdbgrid}
  TSDebug,
{$ENDIF}
	TS_Application,
  Menus;

type
	TCrackGrid = class(TCustomGrid);

var
  FilterImg16: TBitmap=nil;
  FilterImg19: TBitmap=nil;

const
  resFilterImg16 = 'filter16';
  resFilterImg19 = 'filter19';
  filterDispDefault: array[NTriState] of string = ('Nein', 'Ja', '?');
  filterSQLDefault: array[NTriState] of string = ('FALSE', 'TRUE', '');

procedure Register;
begin
  {$I TS_DBGrid.lrs}
  RegisterComponents(DefaultComponentPage, [TTSDBGrid]);
end;

//******************************************************************************
{ TTSGridTrigger }
//******************************************************************************

constructor TTSGridTrigger.Create(AGrid: TCustomGrid);
begin
  inherited Create;
  fGrid := AGrid;
end;

function TTSGridTrigger.Fired(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited Fired(Button, Shift, X, Y);
  if Result and Assigned(fGrid) then
  begin
	 	if fGrid.MouseToCell(Point(X, Y)).y >= TCrackGrid(fGrid).FixedRows then
    	Result := ttaCells in MouseArea
    else
    	Result := ttaHeader in MouseArea;
	end;
end;

//******************************************************************************
{ TTSDBGridOptions }
//******************************************************************************

procedure TTSDBGridOptions.SetTitleMark(const AValue: Boolean);
begin
  if fTitleMark = AValue then exit;
  fTitleMark := AValue;
	fGrid.Invalidate;
end;

constructor TTSDBGridOptions.Create(Grid: TTSDBGrid);
begin
  fGrid := Grid;
  fTrigger := TTSGridTrigger.Create(Grid);
  with fTrigger do begin
    ActiveTriggers := [trgKeyboard, trgMouse];
  	ShortCut := Menus.ShortCut(VK_F7, []);
    MouseArea := [ttaHeader, ttaCells];
    MouseButton := mbRight;
    MouseShift := [ssAlt];
  end;
  fFormCaption := 'Set grid options';
  fTitelClick := True;
  fBtnPanel := False;
  fColumns := [gocVisible, gocFilter, gocFilterActive, gocSorting, gocColumnWidth];
end;

destructor TTSDBGridOptions.Destroy;
begin
  fTrigger.Free;
  inherited Destroy;
end;

//******************************************************************************
{ TTSGridSorting }
//******************************************************************************

function TTSGridSorting.GetFieldNames: TStrings;
var
  I: Integer;
  C: TTSColumn;
begin
  fReadingSort := True;
  try
    fFields.TextLineBreakStyle; //Ruft die private Funktion "CheckSpecialChars" von TStrings auf.
    fFields.Clear;
	  for I := 0 to pred(Length(fSortList)) do
    begin
      C := fGrid.Columns[fSortList[I]];
	    if C.SortingOrder = gsoAscending then
 	  	  fFields.Add(C.FieldName + fFields.NameValueSeparator + 'asc')
	    else if C.SortingOrder = gsoDescending then
 	  	  fFields.Add(C.FieldName + fFields.NameValueSeparator + 'desc');
    end;
    Result := fFields;
  finally
    fReadingSort := False;
  end;
end;

function TTSGridSorting.GetFixedFirst: TStrings;
begin
  Result := fFixedFirst;
end;

function TTSGridSorting.GetFixedLast: TStrings;
begin
  Result := fFixedLast;
end;

procedure TTSGridSorting.SetBGColorAsc(const AValue: TColor);
begin
  if fBGColorAsc = AValue then exit;
  fBGColorAsc := AValue;
  fGrid.Invalidate;
end;

procedure TTSGridSorting.SetBGColorDesc(const AValue: TColor);
begin
  if fBGColorDesc = AValue then exit;
  fBGColorDesc := AValue;
  fGrid.Invalidate;
end;

procedure TTSGridSorting.SetBGRect(const AValue: Boolean);
begin
  if fBGRect = AValue then exit;
  fBGRect := AValue;
  fGrid.Invalidate;
end;

procedure TTSGridSorting.SetColorAsc(const AValue: TColor);
begin
  if fColorAsc = AValue then exit;
  fColorAsc := AValue;
  fGrid.Invalidate;
end;

procedure TTSGridSorting.SetColorDesc(const AValue: TColor);
begin
  if fColorDesc = AValue then exit;
  fColorDesc := AValue;
  fGrid.Invalidate;
end;

procedure TTSGridSorting.SetFieldNames(const AValue: TStrings);
var
  P, I, N, X: Integer;
  FN, FV: string;
  SO: NTSGridSortOrder;
begin
  SetLength(fSortList, 0);
  N := 0;
  for I := 0 to pred(fGrid.Columns.Count) do
	  fGrid.Columns[I].fSortOrder := gsoNoSort;
  for P := 0 to pred(AValue.Count) do
  begin
    //Extract field name and sort order (asc/desc)
    StringsValuePair(AValue, P, FN, FV);
    if (FV > '') and (AnsiChar(FV[1]) in ['d', 'D']) then
      SO := gsoDescending
    else
      SO := gsoAscending;

    //Eliminate duplicates.
    X := AValue.IndexOfName(FN);
    if (X > -1) and (X < P) then
      Continue;
    X := AValue.IndexOf(FN);
    if (X > -1) and (X < P) then
      Continue;

    //Find column for field name
    for I := 0 to pred(fGrid.Columns.Count) do
    begin
      if SameText(fGrid.Columns[I].FieldName, FN) then
      begin
        SetLength(fSortList, N+1);
        fSortList[N] := I;
        inc(N);
        fGrid.Columns[I].fSortOrder := SO;
        break;
      end;
    end;
  end;
  SortingChanged;
end;

procedure TTSGridSorting.SetFixedFirst(const Value: TStrings);
begin
  fFixedFirst.Assign(Value);
end;

procedure TTSGridSorting.SetFixedLast(const Value: TStrings);
begin
  fFixedLast.Assign(Value);
end;

procedure TTSGridSorting.SetShowInTitles(const AValue: Boolean);
begin
  if fShowInTitles = AValue then exit;
  fShowInTitles := AValue;
  fGrid.Invalidate;
end;

function TTSGridSorting.CheckSortType: Boolean;
begin
  Result := False;
  if Assigned(fGrid.DataSource) and Assigned(fGrid.DataSource.DataSet) then
  begin
{$IFDEF USEZEOS}
    if (fSortType in [gstZEOSLocal, gstZEOSSQL])
      and (fGrid.DataSource.DataSet is TZAbstractRODataset)
    then
      Result := True;
{$ENDIF}
{$IFDEF USEFDAC}
    if ((fSortType = gstFDLocal) and (fGrid.DataSource.DataSet is TADDataSet))
      or ((fSortType = gstFDSQL) and (fGrid.DataSource.DataSet is TADCustomQuery))
    then
  	  Result := True
{$ENDIF}
  end;
  if not Result then
    fSortType := gstManual;
end;

procedure TTSGridSorting.SetSortType(const AValue: NTSGridSortType);
var
  Changed: Boolean;
begin
  Changed := fSortType <> AValue;
  fSortType := AValue;
  if Assigned(fGrid) and not (csLoading in fGrid.ComponentState) then
    Changed := Changed or not CheckSortType;
  if Changed then
    SortingChanged;
end;

procedure TTSGridSorting.FieldsChanged(Sender: TObject);
begin
  if not fReadingSort then
	  SetFieldNames(fFields);
end;


procedure TTSGridSorting.FixedChanged(Sender: TObject);
begin
  SortingChanged;
end;

function TTSGridSorting.BuildSortString(Asc, Desc, Separator: string): string;
var
  I: Integer;
  Fld: string;
  C: TTSColumn;
  SL: TStringList;

  function CheckEntry(Lst: TStringList; Index: Integer; out Res: string): Boolean;
  var
    Nm, Vl: string;
  begin
    StringsValuePair(Lst, Index, Nm, Vl);
    Result := SL.IndexOfName(Nm) = -1;
    if Result then
    begin
      if (Vl>'') and (AnsiChar(Vl[1]) in ['D', 'd']) then
        Res := Nm + SL.NameValueSeparator + Desc
      else
        Res := Nm + SL.NameValueSeparator + Asc;
    end;
  end;

begin
  SL := TStringList.Create;
  try
    for I := 0 to pred(Length(fSortList)) do
    begin
      C := fGrid.Columns[fSortList[I]];
      if SL.IndexOfName(C.FieldName) = -1 then
        SL.Add(fGrid.FieldNameDelimiters.Delimited(C.FieldName)
        				+ SL.NameValueSeparator
        				+ IIf(C.SortingOrder=gsoDescending, Desc, Asc));
    end;
    for I := pred(fFixedFirst.Count) downto 0 do
      if CheckEntry(fFixedFirst, I, Fld) then
        SL.Insert(0, Fld);
    for I := 0 to pred(fFixedLast.Count) do
      if CheckEntry(fFixedLast, I, Fld) then
        SL.Add(Fld);

    if SL.Count = 0 then
      Result := ''
    else begin
      Result := SL.Names[0] + SL.ValueFromIndex[0];
      for I := 1 to pred(SL.Count) do
        Result := Result + Separator + SL.Names[I] + SL.ValueFromIndex[I];
    end;
  finally
    SL.Free;
  end;
end;

procedure TTSGridSorting.SetSortOrder(SortFields: string);
var
	SL: TStringList;
begin
	SL := TStringList.Create;
	try
		SL.Text := SortFields;
		SetFieldNames(SL);
	finally
		SL.Free;
	end;
end;

procedure TTSGridSorting.BuildIndexFields(out IdxName, AscFields,
	DescFields: string; const Separator: string);
var
	I: Integer;
	C: TTSColumn;
  Fld: string;
	SL: TStringList;
	SO: NTSGridSortOrder;

  function CheckEntry(Lst: TStringList; Index: Integer; out Res: string): NTSGridSortOrder;
  var
    Nm, Vl: string;
  begin
    StringsValuePair(Lst, Index, Nm, Vl);
    if SL.IndexOf(Nm) = -1 then
    	Exit(gsoNoSort)
    else begin
    	Res := Nm;
    	if (Vl>'') and (AnsiChar(Vl[1]) in ['D', 'd']) then
    		Result := gsoDescending
      else
    		Result := gsoAscending;
    end;
  end;

begin
  SL := TStringList.Create;
  try
	  for I := 0 to pred(Length(fSortList)) do
	  begin
	    C := fGrid.Columns[fSortList[I]];
	    if SL.IndexOfName(C.FieldName) = -1 then
	      SL.AddObject(fGrid.FieldNameDelimiters.Delimited(C.FieldName), TObject(PtrUint(C.SortingOrder)));
	  end;
	  for I := pred(fFixedFirst.Count) downto 0 do
	  begin
	    SO := CheckEntry(fFixedFirst, I, Fld);
	    if SO <> gsoNoSort then
	      SL.InsertObject(0, Fld, TObject(PtrUint(SO)));
		end;
		for I := 0 to pred(fFixedLast.Count) do
		begin
	    SO := CheckEntry(fFixedLast, I, Fld);
	    if SO <> gsoNoSort then
	      SL.AddObject(Fld, TObject(PtrUint(SO)));
		end;

		AscFields := '';
		DescFields := '';
		IdxName := '';
		for I := 0 to pred(SL.Count) do
		begin
			if NTSGridSortOrder(PtrUint(SL.Objects[I]))=gsoDescending then
			begin
				DescFields := DescFields + Separator + SL[I];
  			IdxName := IdxName + SL[I] + 'DESC';
			end
			else begin
				AscFields := AscFields + Separator + SL[I];
				IdxName := IdxName + SL[I];
			end;
		end;
		Delete(AscFields, 1, Length(Separator));
		Delete(DescFields, 1, Length(Separator));
	finally
		SL.Free;
	end;

end;

procedure TTSGridSorting.SortingChanged;
var
	I: Integer;
  DS: TDataSet;
{$IFDEF USEZEOS}
  ZDS: TTSZAbstractRODataset;
  SQL, SortFields: string;
  SP, EP: Integer;
  DSActive: Boolean;
{$ENDIF}
{$IFDEF SQLDB}
	Qry: TSQLQuery;
  QryActive: Boolean;
	Asc, Desc, IdxName: string;
	{$IFNDEF USEZEOS}
		SQL, SortFields: string;
		SP, EP: Integer;
	{$ENDIF}
	Idx: TIndexDef;
{$ENDIF}
begin
	if (fUpdateCount > 0)
  		or (fGrid.DataSource = nil)
      or (fGrid.DataSource.DataSet = nil)
  then
  	Exit;

  DS := fGrid.DataSource.DataSet;
  for I := 0 to pred(Length(fSortList)) do
    fGrid.Columns[fSortList[I]].fSortPos := I+1;

  if Assigned(fGrid.fOnSortChange) then
  	fGrid.fOnSortChange(fGrid, FieldNames);

{$IFDEF SQLDB}
	if (fSortType = gstSQLLocal)
	  and (fGrid.DataSource.DataSet is TSQLQuery)
	then begin
    Qry := TSQLQuery(DS);
		BuildIndexFields(IdxName, Asc, Desc, ';');
		IdxName := Qry.Name + IdxName;
		try
			Idx := Qry.IndexDefs.Find(IdxName);
		except
			Idx := Qry.IndexDefs.AddIndexDef;
		end;
		with Idx do
		begin
			Name := IdxName;
			Options := [ixCaseInsensitive];
			if (Asc >'') and (Desc>'') then
				Fields := Asc + ';' + Desc
			else
				Fields := Asc + Desc;
			DescFields := Desc;
		end;
		Qry.IndexName := IdxName;
	end;

  if (fSortType = gstSQLServer) then
  begin
    Qry := TSQLQuery(DS);
    QryActive := Qry.Active;
    Qry.DisableControls;
    try
      Qry.Close;
      SortFields := BuildSortString(' asc', ' desc', ',');
      SQL := Qry.SQL.Text;
      SP := 0;
			SQLOrderByClause(SQL, SP, EP);
      if (SP = 0) and (SortFields > '') then
      	Qry.SQL.Add('order by ' + SortFields)
      else begin
        if SortFields > '' then
          SQL := Copy(SQL, 1, SP-1)
          							+ 'order by ' + SortFields + ' '
                        + Copy(SQL, EP+1, MaxInt)
        else
          Delete(SQL, SP, EP-SP+1);
        Qry.SQL.Text := SQL;
      end;
      Qry.Active := QryActive;
    finally
      Qry.EnableControls;
    end;
  end;

{$ENDIF}
{$IFDEF USEZEOS}
  if (fSortType in [gstZEOSLocal, gstZEOSSQL])
    and (fGrid.DataSource.DataSet is TZAbstractRODataset)
  then begin
    SortFields := BuildSortString(' asc', ' desc', ',');

    if fSortType = gstZEOSLocal then
		  TZAbstractRODataset(DS).SortedFields := SortFields

    else if fSortType = gstZEOSSQL then
    begin
      ZDS := TTSZAbstractRODataset(DS);
      DSActive := ZDS.Active;
      ZDS.DisableControls;
      try
        ZDS.Close;
        SQL := ZDS.SQL.Text;
        SP := 0;
				SQLOrderByClause(SQL, SP, EP);
        if (SP = 0) and (SortFields > '') then
        	ZDS.SQL.Add('order by ' + SortFields)
        else begin
          if SortFields > '' then
            SQL := Copy(SQL, 1, SP-1)
            							+ 'order by ' + SortFields + ' '
                          + Copy(SQL, EP+1, MaxInt)
          else
            Delete(SQL, SP, EP-SP+1);
          ZDS.SQL.Text := SQL;
        end;
        ZDS.Active := DSActive;
      finally
        ZDS.EnableControls;
      end;
    end;
  end;
{$ENDIF}
	fGrid.Invalidate;
end;

constructor TTSGridSorting.Create(Grid: TTSDBGrid);
begin
  inherited Create;
  fGrid := Grid;
  fReadingSort := False;
{$IFDEF USEZEOS}
  fSortType := gstZEOSLocal;
{$ELSE}
  fSortType := gstManual;
{$ENDIF}
  fColorAsc := clGreen;
  fColorDesc := clMaroon;
  fBGColorAsc := clWindow;
  fBGColorDesc := clWindow;
  fShowInTitles := True;
  fBGRect := True;
  fUpdateCount := 0;
  fFields := TStringList.Create;
  fFields.OnChange := @FieldsChanged;
  fFixedFirst := TStringList.Create;
  fFixedFirst.OnChange := @FixedChanged;
  fFixedLast := TStringList.Create;
  fFixedLast.OnChange := @FixedChanged;
  fTrgAppend := TTSGridTrigger.Create(Grid);
  with fTrgAppend do begin
    ActiveTriggers := [trgKeyboard, trgMouse];
  	ShortCut := Menus.ShortCut(VK_DOWN, [ssAlt,ssShift]);
    MouseArea := [ttaHeader];
    MouseButton := mbLeft;
    MouseShift := [ssShift];
  end;

  fTrgNew := TTSGridTrigger.Create(Grid);
  with fTrgNew do begin
    ActiveTriggers := [trgKeyboard, trgMouse];
  	ShortCut := Menus.ShortCut(VK_DOWN, [ssAlt]);
    MouseArea := [ttaHeader];
    MouseButton := mbLeft;
    MouseShift := [];
  end;

  SetLength(fSortList, 0);
end;

destructor TTSGridSorting.Destroy;
begin
  BeginUpdate;
  fTrgAppend.Free;
  fTrgNew.Free;
  fFixedFirst.Free;
  fFixedLast.Free;
  inherited Destroy;
end;

procedure TTSGridSorting.BeginUpdate;
begin
  inc(fUpdateCount);
end;

procedure TTSGridSorting.EndUpdate;
begin
	dec(fUpdateCount);
	if fUpdateCount <= 0 then
  begin
		fUpdateCount := 0;
    SortingChanged;
  end;
end;

procedure TTSGridSorting.Clear;
begin
  SetLength(fSortList, 0);
end;

//******************************************************************************
{ TTSGridFilter }
//******************************************************************************

constructor TTSGridFilter.Create(Grid: TTSDBGrid);
begin
  inherited Create;
  fGrid := Grid;
	fEnabled := True;
  fFilterType := gftLocalDataset;
  fTitelBarMark := True;
  fSuccessive := True;
  fWildCard := '*';
  fEditType := gfeTitleBarAlwaysHide;
  fBoolDisplay[tsNULL] := filterDispDefault[tsNULL];
  fBoolDisplay[tsTrue] := filterDispDefault[tsTrue];
  fBoolDisplay[tsFalse] := filterDispDefault[tsFalse];
  fBoolSQL[tsNULL] := filterSQLDefault[tsNULL];
  fBoolSQL[tsTrue] := filterSQLDefault[tsTrue];
  fBoolSQL[tsFalse] := filterSQLDefault[tsFalse];

  fTrigger := TTSGridTrigger.Create(Grid);
  with fTrigger do begin
    ActiveTriggers := [trgKeyboard, trgMouse];
    ShortCut := Menus.ShortCut(VK_F3, []);
    MouseArea := [ttaHeader, ttaCells];
    MouseButton := mbRight;
    MouseShift := [];
  end;
end;

destructor TTSGridFilter.Destroy;
begin
  fTrigger.Free;
  inherited Destroy;
end;

procedure TTSGridFilter.SetEnabled(const AValue: Boolean);
begin
  if fEnabled = AValue then exit;
  fEnabled := AValue;
	FilterGrid;
end;

procedure TTSGridFilter.SetBoolDisplay(BoolVal: NTriState; const AValue: string);
begin
	fBoolDisplay[BoolVal] := AValue;
end;

procedure TTSGridFilter.SetBoolSQL(BoolVal: NTriState; const AValue: string);
begin
	if AValue <> fBoolSQL[BoolVal] then
  begin
		fBoolSQL[BoolVal] := AValue;
    if fEnabled then
	    FilterGrid;
  end;
end;

procedure TTSGridFilter.SetBoolValsDisplay(const AValue: string);
var
	N: NTriState;
  P: Integer;
  S: string;
begin
  S := AValue;
  for N := Low(NTriState) to High(NTriState) do
  begin
  	P := Pos(';', S);
    if P > 0 then
    begin
			BoolDisplay[N] := Copy(S, 1, P-1);
      Delete(S, 1, P);
    end
    else begin
			BoolDisplay[N] := S;
      S := '';
    end;
  end;
end;

function TTSGridFilter.StoreBoolDisp: Boolean;
var
  N: NTriState;
begin
  Result := True;
  for N := High(NTriState) downto Low(NTriState) do
    if fBoolDisplay[N] <> filterDispDefault[N] then
      Exit;
  Result := False;
end;


procedure TTSGridFilter.SetBoolValsSQL(const AValue: string);
var
	N: NTriState;
  P: Integer;
  S: string;
begin
  S := AValue;
  for N := Low(NTriState) to High(NTriState) do
  begin
  	P := Pos(';', S);
    if P > 0 then
    begin
			BoolSQL[N] := Copy(S, 1, P-1);
      Delete(S, 1, P);
    end
    else begin
			BoolSQL[N] := S;
      S := '';
    end;
  end;
end;

function TTSGridFilter.StoreBoolSQL: Boolean;
var
  N: NTriState;
begin
  Result := True;
  for N := Low(NTriState) to High(NTriState) do
    if fBoolSQL[N] <> filterSQLDefault[N] then
      Exit;
  Result := False;
end;

procedure TTSGridFilter.SetDesignActive(const AValue: Boolean);
var
  B: Boolean;
begin
  if fDesignActive = AValue then exit;
  if not AValue and (csDesigning in fGrid.ComponentState) then
  begin
    B := fEnabled;
    fEnabled := False;
    try
	    FilterGrid;
    finally
      fEnabled := B;
    end;
  end;
  fDesignActive := AValue;
	if fDesignActive then
	  FilterGrid;
end;

procedure TTSGridFilter.SetEditType(AValue: NTSGridFilterEdit);
begin
  if fEditType = AValue then exit;
  if (AValue = gfePanel)
    and not ((csLoading in fGrid.ComponentState) or Assigned(fPanel))
  then
  	Exit;
  fEditType := AValue;
  if not (csLoading in fGrid.ComponentState) then
    ShowEdits;
end;

function TTSGridFilter.GetBoolDisplay(BoolVal: NTriState): string;
begin
	Result := fBoolDisplay[BoolVal];
end;

function TTSGridFilter.GetBoolSQL(BoolVal: NTriState): string;
begin
	Result := fBoolSQL[BoolVal];
end;

function TTSGridFilter.GetBoolValsDisplay: string;
var
  N: NTriState;
begin
  Result := '';
  for N := High(NTriState) downto Low(NTriState) do
		Result := ';' + BoolDisplay[N] + Result;
  Delete(Result, 1, 1);
end;

function TTSGridFilter.GetBoolValsSQL: string;
var
  N: NTriState;
begin
  Result := '';
  for N := High(NTriState) downto Low(NTriState) do
		Result := ';' + BoolSQL[N] + Result;
  Delete(Result, 1, 1);
end;

procedure TTSGridFilter.SetFilterText(const AValue: string);
begin
  if fFilterText = AValue then exit;
  fFilterText := AValue;
  if fEnabled then
	  FilterGrid;
end;

procedure TTSGridFilter.SetFilterType(AValue: NTSGridFilterType);
var
  B: Boolean;
begin
  if fFilterType = AValue then exit;
  if Assigned(fGrid.DataSource)
  	and Assigned(fGrid.DataSource.DataSet)
  then
  begin
    fGrid.DataSource.DataSet.DisableControls;
    try
      B := fEnabled;
  	  fEnabled := False;
      try
        FilterGrid;
      finally
        fEnabled := B;
      end;
      if fWildCard = DefaultWildCard[fFilterType] then
  	    fWildCard := DefaultWildCard[AValue];
      fFilterType := AValue;
      FilterGrid;
    finally
      fGrid.DataSource.DataSet.EnableControls;
    end;
  end
  else begin
    if fWildCard = DefaultWildCard[fFilterType] then
	    fWildCard := DefaultWildCard[AValue];
    fFilterType := AValue;
  end;
end;

procedure TTSGridFilter.SetPanel(const AValue: TCustomPanel);
begin
  if fPanel = AValue then exit;
  if Assigned(fPanel) then
  	fPanel.RemoveFreeNotification(fGrid);
  fPanel := AValue;
  if Assigned(fPanel) then
  	fPanel.FreeNotification(fGrid)
  else if EditType = gfePanel then
  	SetEditType(gfeManual);
  ShowEdits;
end;

procedure TTSGridFilter.SetTitleBarMark(const AValue: Boolean);
begin
  if fTitelBarMark = AValue then exit;
  fTitelBarMark := AValue;
  if EditType in [gfeTitleBarAutoHide, gfeTitleBarAlwaysHide
                 , gfeManual, gfeOptionsForm]
  then
	  fGrid.Invalidate;
end;

procedure TTSGridFilter.SetWildCard(const AValue: string);
begin
  if fWildCard = AValue then exit;
  fWildCard := AValue;
  if fEnabled then
	  FilterGrid;
end;

function TTSGridFilter.StoreWildCard: Boolean;
begin
  Result := fWildCard <> DefaultWildCard[fFilterType];
end;

function TTSGridFilter.StoreTrigger: Boolean;
begin
  with fTrigger do
    Result := (ActiveTriggers <> [trgKeyboard, trgMouse])
          or (ShortCut <> Menus.ShortCut(VK_F3, []))
          or (MouseArea <> [ttaHeader, ttaCells])
          or (MouseButton <> mbRight)
          or (MouseShift <> []);
end;


procedure TTSGridFilter.HideEdits;
var
  I: Integer;
begin
	for I := 0 to pred(fGrid.Columns.Count) do
  	fGrid.Columns[I].Filter.HideEdit;
end;

procedure TTSGridFilter.ShowEdits;
var
  I: Integer;
begin
	for I := 0 to pred(fGrid.Columns.Count) do
  	fGrid.Columns[I].Filter.CheckShowEdit(fGrid.Columns[I].Filter.fFilterEdit.Focused);
end;

procedure TTSGridFilter.UpdateEdits;
var
  I: Integer;
begin
	for I := 0 to pred(fGrid.Columns.Count) do
  	if Assigned(fGrid.Columns[I].Filter) then
	  	fGrid.Columns[I].Filter.UpdateEdit;
end;

function TTSGridFilter.GetFilterImage(ImgHeight: Integer=19): TBitmap;
begin
  if ImgHeight < 19 then
  begin
    if not Assigned(FilterImg16) then
	  	FilterImg16 := TBitmap(CreateBitmapFromLazarusResource(resFilterImg16));
  	Result := FilterImg16;
  end
  else begin
  	if not Assigned(FilterImg19) then
	  	FilterImg19 := TBitmap(CreateBitmapFromLazarusResource(resFilterImg19));
  	Result := FilterImg19;
  end;
end;

procedure TTSGridFilter.RebuildFilter;
var
  I: Integer;
begin
  for I := 0 to pred(fGrid.Columns.Count) do
  	if Assigned(fGrid.Columns[I].fColFilter) then
	  	fGrid.Columns[I].fColFilter.UpdateFilter;
end;

procedure TTSGridFilter.Clear;
var
  I: Integer;
begin
	fGrid.Columns.BeginUpdate;
  try
  	for I := 0 to pred(fGrid.Columns.Count) do
      fGrid.Columns[I].Filter.Clear;
  finally
    fGrid.Columns.EndUpdate;
  end;
  fGrid.Filter.FilterGrid(-1);
end;

function TTSGridFilter.FilterString(Rebuild: Boolean): string;
var
  I: Integer;
	C: TTSColumn;

  procedure AddFilter(S: string);
  begin
	  if Result > '' then
		  Result := Result + ' and (' + S + ')'
	  else
		  Result := '(' + S + ')';
  end;

begin
  Result := '';
	if Rebuild then
  	RebuildFilter;
  for I := 0 to pred(fGrid.Columns.Count) do
  begin
  	C := fGrid.Columns[I];
    if not C.fCreating
    	and C.Visible
	    and C.Filter.Enabled
  	  and (C.Filter.FilterSQL>'')
    then
			AddFilter(C.Filter.FilterSQL);
  end;
  if fFilterText > '' then
  	AddFilter(fFilterText);
end;

procedure TTSGridFilter.FilterGrid(ChangedColumnIndex: Integer);
var
	FS: string;
  DS: TDataSet;
  FT: NTSGridFilterType;
{$IFDEF USEZEOS}
  ZDS: TTSZAbstractRODataset;
  SP, EP: Integer;
  DSActive: Boolean;
{$ENDIF}
{$IFDEF SQLDB}
	Qry: TTSSQLQuery;
	{$IFNDEF USEZEOS}
	  SP, EP: Integer;
    DSActive: Boolean;
  {$ENDIF}
{$ENDIF}

begin
  if not fRebuilding
  	and (SuccessiveFiltering or not EditingColumnFilter)
    and Assigned(fGrid.DataSource)
    and Assigned(fGrid.DataSource.DataSet)
    and (fDesignActive or not (csDesigning in fGrid.ComponentState))
  then try
    fRebuilding := True;
    DS := fGrid.DataSource.DataSet;
		FT := fFilterType;

{$IFDEF SQLDB}
		if (FT = gftSQLdb) and not (DS is TCustomSQLQuery) then
  		FT := gftManual;
{$ENDIF}
{$IFDEF USEZEOS}
    if (FT = gftZEOS) and not (DS is TZAbstractRODataset) then
	    FT := gftManual;
{$ENDIF}

    if Enabled then
    begin
	    if FT <> fFilterType then
  	  	FS := FilterString(True)
      else
				FS := FilterString(ChangedColumnIndex<0);
    end
	  else
  		FS := '';

    if Assigned(fGrid.fOnFilterChange) then
    	fGrid.fOnFilterChange(fGrid, ChangedColumnIndex, FS);

    case FT of
      gftLocalDataset: begin
				DS.DisableControls;
        try
          DS.Filter := FS;
          DS.Filtered := FS>'';
        finally
          DS.EnableControls;
        end;
      end;
{$IFDEF SQLDB}
			gftSQLdb: begin
				Qry := TTSSQLQuery(DS);
      	DSActive := Qry.Active;
 				Qry.DisableControls;
        try
        	Qry.Close;
          SP := 0;
          SetSQLWhereClause(Qry.SQL, FS, SP, EP);
					Qry.Active := DSActive;
          //Qry.ServerFilter := FS;
          //Qry.ServerFiltered := FS>'';
        finally
          Qry.EnableControls;
        end;
      end;
{$ENDIF}
{$IFDEF USEZEOS}
      gftZEOS: begin
        ZDS := TTSZAbstractRODataset(DS);
        DSActive := ZDS.Active;
        ZDS.DisableControls;
        try
          ZDS.Close;
          SP := 0;
          SetSQLWhereClause(ZDS.SQL, FS, SP, EP);
          ZDS.Active := DSActive;
        finally
          ZDS.EnableControls;
        end;
      end;
{$ENDIF}

    end;
  finally
    fRebuilding := False;
  end;
end;

procedure TTSGridFilter.EditColumnFilter(ColIndex: Integer);
begin
	fGrid.Columns[ColIndex].Filter.CheckShowEdit(True);
end;

//******************************************************************************
{ TTSColumnFilter }
//******************************************************************************

procedure TTSColumnFilter.SetAllowRange(const AValue: Boolean);
begin
  if fAllowRange = AValue then exit;
  fAllowRange := AValue;
  FilterChanged;
end;

procedure TTSColumnFilter.SetEnabled(const AValue: Boolean);
begin
  if fEnabled = AValue then exit;
  fEnabled := AValue;
  FilterChanged;
end;

procedure TTSColumnFilter.SetFilterText(const AValue: string);
begin
  if fFilter = AValue then exit;
  fFilter := AValue;
  if not fUseSQL then
	  FilterChanged;
end;

procedure TTSColumnFilter.SetFilterType(const AValue: NTSColumnFilterType);
begin
  if fFilterType = AValue then exit;
  fFilterType := AValue;
  if not fUseSQL then
	  FilterChanged;
end;

procedure TTSColumnFilter.SetFilterSQL(const AValue: string);
begin
  if fFilterSQL = AValue then exit;
  fFilterSQL := AValue;
  if AValue > '' then
  	fUseSQL := True;
  FilterChanged;
end;

procedure TTSColumnFilter.SetUseSQL(const AValue: Boolean);
begin
  if fUseSQL = AValue then exit;
  fUseSQL := AValue;
  FilterChanged;
end;

constructor TTSColumnFilter.Create(Column: TTSColumn);
begin
  inherited Create;
  fColumn := Column;
  fEnabled := True;
  fFilterType := cftAuto;
  fAllowRange := True;
  fUseSQL := False;
  fInternalEditChange := False;
  CreateFilterEdit;
end;

procedure TTSColumnFilter.CreateFilterEdit;
begin
  fFilterEdit := TEdit.Create(fColumn.Grid);
  with TEdit(fFilterEdit) do
  begin
    OnChange := @EditChange;
    OnEnter := @EditEnter;
    OnExit := @EditExit;
    OnKeyDown := @EditKeyDown;
    Visible := False;
  end;
end;

destructor TTSColumnFilter.Destroy;
begin
  fFilterEdit.Free;
  fFilterEdit := nil;
  inherited Destroy;
end;

procedure TTSColumnFilter.Assign(Source: TPersistent);
begin
  if Source is TTSColumnFilter then
  begin
    fAllowRange := TTSColumnFilter(Source).fAllowRange;
    fEnabled := TTSColumnFilter(Source).fEnabled;
    fFilter := TTSColumnFilter(Source).fFilter;
    fFilterDisplay := TTSColumnFilter(Source).fFilterDisplay;
    fFilterType := TTSColumnFilter(Source).fFilterType;
    fFilterSQL := TTSColumnFilter(Source).fFilterSQL;
    fUseSQL := TTSColumnFilter(Source).fUseSQL;
  end
  else
    inherited Assign(Source);
end;

procedure TTSColumnFilter.FilterChanged;
begin
  UpdateFilter;
  fColumn.ColumnChanged;
end;

function TTSColumnFilter.CheckShowEdit(Focus: Boolean): Boolean;
var
	Grd: TTSDBGrid;
  Filt: TTSGridFilter;
begin
  Grd := fColumn.Grid;
  Filt := Grd.Filter;
  Result := fColumn.Visible and Enabled and not UseFilterSQL;
  case Filt.EditType of
    gfeManual, gfeOptionsForm:
    	Result := False;
    gfePanel:
    	Result := Result and Assigned(Filt.EditingPanel);
    gfeTitleBarAlwaysHide:
      if not Focus then
      	Result := Result and fFilterEdit.Focused;
    gfeTitleBarAutoHide:
      if not Focus then
      	Result := Result and ((FilterText > '') or fFilterEdit.Focused);
    //gfeTitleBar: Result := Result;
  end;
	if Assigned(Grd.fBeforeEditFilter) then
  	Grd.fBeforeEditFilter(Grd, fColumn.Index, Result);

  if Result then
  	ShowEdit(Focus)
  else
    HideEdit;
end;

procedure TTSColumnFilter.ShowEdit(Focus: Boolean);
var
	Grd: TTSDBGrid;
begin
  Grd := fColumn.Grid;
  case Grd.Filter.EditType of
    gfeManual, gfeOptionsForm:
      fFilterEdit.Parent := nil;
    gfeTitleBarAlwaysHide, gfeTitleBar, gfeTitleBarAutoHide:
      fFilterEdit.Parent := Grd;
    gfePanel:
      fFilterEdit.Parent := Grd.Filter.EditingPanel;
  end;
	fFilterEdit.Visible := Assigned(fFilterEdit.Parent);
	UpdateEdit;
  if Focus and fFilterEdit.CanFocus then
  begin
	  fFilterEdit.SetFocus;
		fFilterEdit.SelectAll;
  end;
  if Assigned(Grd.fOnEditFilter) then
  	Grd.fOnEditFilter(Grd, fColumn.Index);
end;

procedure TTSColumnFilter.UpdateEdit;
var
	Grd: TTSDBGrid;
  L, W: Integer;
begin
  if fInternalEditChange
  	or not (fFilterEdit.Visible and Assigned(fFilterEdit.Parent))
  then
  	Exit;
  Grd := fColumn.Grid;
  L := Grd.ColumnPos(fColumn.Index) + 1;
  if L > 0 then
  	W := fColumn.Width - 2
  else
  	W := 0;
  if dgColLines in Grd.Options then
    inc(L, 2);
  case Grd.Filter.EditType of
    gfePanel: begin
			fFilterEdit.AutoSize := True;
      fFilterEdit.SetBounds(L,
			          						(fFilterEdit.Parent.ClientHeight - fFilterEdit.Height) div 2,
        		                W,
            		            Grd.TitleRowHeight // Dummy, weil AutoSize = TRUE
                		        );
    end;
    gfeTitleBar, gfeTitleBarAlwaysHide, gfeTitleBarAutoHide:
    begin
			fFilterEdit.AutoSize := False;
		  fFilterEdit.SetBounds(L,
    							          0,
        		                W,
            		            Grd.TitleRowHeight
                		        );
    end;
  end;

  if not fFilterEdit.Focused then
  begin
	  fInternalEditChange := True;
    try
  	  fFilterEdit.Text := DisplayText;
    finally
  	  fInternalEditChange := False;
    end;
  end;
end;

procedure TTSColumnFilter.HideEdit;
begin
	if Assigned(fFilterEdit) then
  begin
  	fFilterEdit.Visible := False;
    fFilterEdit.Parent := nil;
  end;
end;

procedure TTSColumnFilter.EditChange(Sender: TObject);
begin
  if not (Sender is TCustomEdit) then
  	Exit;
  if not fInternalEditChange
    and fFilterEdit.Visible
    and fFilterEdit.Focused
    and fColumn.Grid.Filter.SuccessiveFiltering
  then begin
  	fInternalEditChange := True;
    try
	  	FilterText := fFilterEdit.Text
    finally
    	fInternalEditChange := False;
    end;
  end;
end;

procedure TTSColumnFilter.EditEnter(Sender: TObject);
begin
  fColumn.Grid.SelectedIndex := fColumn.Index;
  fColumn.Grid.Filter.EditingColumnFilter := True;

  fColumn.Grid.fActiveFilterEdit := TCustomEdit(Sender);
end;

procedure TTSColumnFilter.EditExit(Sender: TObject);
var
  G: TTSDBGrid;
begin
  G := fColumn.Grid;
  G.Filter.EditingColumnFilter := False;
  if G.fActiveFilterEdit = Sender then
    G.fActiveFilterEdit := nil;
  if not (Sender is TCustomEdit) then
  	Exit;
  if not G.Filter.SuccessiveFiltering then
  	FilterText := fFilterEdit.Text;

	fInternalEditChange := True;
  try
    fFilterEdit.Text := DisplayText;
  finally
    fInternalEditChange := False;
  end;
  if Assigned(G.fOnEditFilterDone) then
  	G.fOnEditFilterDone(G, fColumn.Index);
  case G.Filter.EditType of
    gfeTitleBarAlwaysHide:
      HideEdit;
    gfeTitleBarAutoHide:
	    if fFilterEdit.Text = '' then
  	  	HideEdit;
  end;
  G.SetFocus;
end;

procedure TTSColumnFilter.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
	KeyHandled: Boolean;
begin
  if not (Sender is TCustomEdit) then
  	Exit;
	KeyHandled := True;
	if Shift = [] then
		case Key of
			vk_Down:
				fColumn.Grid.DataSource.DataSet.Next;
			vk_Up:
				fColumn.Grid.DataSource.DataSet.Prior;
			vk_Return:
				EditExit(Sender);
			vk_Escape: begin
        if TCustomEdit(Sender).Text > '' then
					TCustomEdit(Sender).Clear
        else
					EditExit(Sender);
			end
			else
				KeyHandled := False;
		end
	else
		KeyHandled := False;
	if KeyHandled then
		Key := 0;
end;

procedure TTSColumnFilter.UpdateFilter;
var
  fType: NTSColumnFilterType;
	RgType: NRangeType;
  FilterLo, FilterHi, S1, S2, WC, FN, FS: string;
  Like, FilterLocal: Boolean;
  FilterTriState: NTriState;
  DispArr: ATriStateStr;

  procedure MakeFilter(RangeType: NRangeType; FieldName, LowBound, HighBound: string;
    									QuotedSQL: Boolean);
  begin
		if AllowRange then
    begin
      if QuotedSQL then
	      fFilterSQL := RangeFilter(FieldName, RangeType, QuotedStr(LowBound), QuotedStr(HighBound))
      else
	      fFilterSQL := RangeFilter(FieldName, RangeType, LowBound, HighBound);
      fFilterDisplay := RangeDisplay(RangeType, LowBound, HighBound);
    end
    else begin
      if RangeType = rgToValue then
      begin
      	fFilterSQL := FieldName + '=' + HighBound;
      	fFilterDisplay := HighBound;
      end
      else begin
      	fFilterSQL := FieldName + '=' + LowBound;
	    	fFilterDisplay := LowBound;
      end;
    end;
  end;

begin
  if not fUseSQL
  	and Assigned(fColumn)
    and (fColumn.FieldName > '')
  then begin
    fType := fFilterType;
  	if fType = cftAuto then
    begin
      if Assigned(fColumn.Field) then
      	case fColumn.Field.DataType of
          ftString, ftWideString:
            fType := cftLikeText;
          ftMemo, ftWideMemo:
            fType := cftLikeText;
          ftSmallint, ftInteger, ftWord, ftLargeInt:
            fType := cftNumeric;
          ftFloat, ftCurrency, ftFMTBCD:
            fType := cftNumeric;
          ftBoolean:
            fType := cftBoolean;
          ftDate:
          	fType := cftDate;
          ftTime, ftDateTime, ftTimeStamp:
          	fType := cftDateTime;
					else begin
	          fFilterType := cftNone;
	          Exit;
	        end;
        end
      else
      	Exit;
    end; //fType=cftAuto

    try
      case fType of
        //------------------------------------------
        cftInteger: begin
          RgType := IntRange(fFilter, FilterLo, FilterHi);
          MakeFilter(RgType, fColumn.FieldName, FilterLo, FilterHi, False)
        end;

        //------------------------------------------
        cftNumeric: begin
          RgType := FloatRange(fFilter, FilterLo, FilterHi);
          MakeFilter(RgType, fColumn.FieldName, FilterLo, FilterHi, False)
        end;

        //------------------------------------------
        cftBoolean: begin
          RgType := StringRange(fFilter, FilterLo, FilterHi);
          if (fFilter='') or (RgType=rgInvalid) then
          begin
            fFilterSQL := '';
            fFilterDisplay := '';
            Exit;
          end;
          FilterLocal := fColumn.Grid.Filter.FilterType = gftLocalDataset;
          DispArr := fColumn.Grid.Filter.BoolDispArr;
          case RgType of
            rgToValue:
              FilterTriState := TriState(FilterHi, DispArr);
            rgFromToValue:
            begin
              if TriState(FilterLo, DispArr) = tsNULL then
              begin
                FilterTriState := TriState(FilterHi, DispArr);
                RgType := rgToValue;
              end
              else if TriState(FilterHi, DispArr) = tsNULL then
              begin
                FilterTriState := TriState(FilterLo, DispArr);
                RgType := rgFromValue;
              end
              else
                FilterTriState := tsNULL;
            end;
            else
              FilterTriState := TriState(FilterLo, DispArr);
          end;

          if FilterLocal then
          begin
            case FilterTriState of
              tsFalse: S1 := '(not ' + fColumn.FieldName + ' and ' + fColumn.FieldName + ' is not null)';
              tsTrue: S1 := fColumn.FieldName;
            else
              S1 := '';
            end;
          end
          else begin
            if FilterTriState <> tsNULL then
              S1 := fColumn.FieldName + '=' + fColumn.Grid.Filter.BoolSQL[FilterTriState]
            else
              S1 := '';
          end;

          S2 := '';
          case RgType of
            rgFromValue, rgToValue: begin
              S2 := fColumn.FieldName + ' is NULL';
              fFilterDisplay := RangeDisplay(RgType, DispArr[tsNULL], DispArr[FilterTriState]);
            end;
            rgFromToValue: begin
              S1 := fColumn.FieldName + ' is not NULL';
              fFilterDisplay := RangeDisplay(RgType, DispArr[tsFalse], DispArr[tsTrue]);
            end;
            rgSingleValue:
              if FilterTriState = tsNULL then
              begin
                S1 := fColumn.FieldName + ' is NULL';
                fFilterDisplay := DispArr[tsNULL];
              end
              else
                fFilterDisplay := DispArr[FilterTriState];
            else begin
              S1 := '';
              S2 := '';
              fFilterDisplay := '';
            end;
          end;
          if (S1>'') and (S2>'') then
            fFilterSQL := S1 + ' or ' + S2
          else
            fFilterSQL := S1 + S2;
        end;

        //------------------------------------------
        cftDate: begin
          RgType := DateRange(fFilter, FilterLo, FilterHi);
          MakeFilter(RgType, fColumn.FieldName, FilterLo, FilterHi, True)
        end;

        //------------------------------------------
        cftTime: begin
          RgType := TimeRange(fFilter, FilterLo, FilterHi);
          MakeFilter(RgType, fColumn.FieldName, FilterLo, FilterHi, True)
        end;

        //------------------------------------------
        cftDateTime: begin
          RgType := DateTimeRange(fFilter, FilterLo, FilterHi);
          MakeFilter(RgType, fColumn.FieldName, FilterLo, FilterHi, True)
        end;

        //------------------------------------------
        cftString:
        	if fFilter > '' then
          begin
	          RgType := StringRange(fFilter, FilterLo, FilterHi);
  	        MakeFilter(RgType, fColumn.FieldName, FilterLo, FilterHi, True)
          end
          else begin
            fFilterSQL := '';
            fFilterDisplay := '';
    	    end;

        //------------------------------------------
        cftText, cftLikeText:
         	if fFilter > '' then
          begin
            WC := fColumn.Grid.Filter.StringWildcard;
            FN := fColumn.FieldName;
            Like := fType = cftLikeText;
            if Like then
          	  FS := StringReplace(fFilter, '*', WC, [rfReplaceAll])
            else
              FS := fFilter;
            fFilterDisplay := fFilter;
            if AllowRange then
            begin
              case StringRange(FS, S1, S2) of
                rgSingleValue: begin
                  if Like then
	                  fFilterSQL := FN + ' like ' + QuotedStr(S1 + WC)
                  else
  	                fFilterSQL := 'lower(' + FN + ')=' + QuotedStr(LowerCase(S1));
                end;
                rgFromValue:
	                fFilterSQL := 'lower(' + FN + ')>=' + QuotedStr(LowerCase(S1));
                rgToValue: begin
	                fFilterSQL := 'lower(' + FN + ')<=' + QuotedStr(LowerCase(S2));
								  if Like then
                	  fFilterSQL := fFilterSQL + ' or ' + FN
		                              + ' like ' + QuotedStr(S2 + WC);
                end;
                rgFromToValue: begin
	                fFilterSQL := 'lower(' + FN + ')>=' + QuotedStr(LowerCase(S1))
							                  + ' and lower(' + FN + ')<=' + QuotedStr(LowerCase(S2));
                  if Like then
									  fFilterSQL := fFilterSQL + ' or ' + FN + ' like ' + QuotedStr(S1 + WC)
                							  + ' or ' + FN + ' like ' + QuotedStr(S2 + WC);
                end;
              end;
            end
            else begin
              if Like then
	              fFilterSQL := FN + ' like ' + QuotedStr(fFilter + WC)
              else
  	            fFilterSQL := FN + '=' + QuotedStr(fFilter);
              fFilterDisplay := fFilter;
					  end;
          end
          else begin
            fFilterSQL := '';
            fFilterDisplay := '';
          end;
				else
	        Exit;
      end;
    except
    end;
  end;
end;

procedure TTSColumnFilter.Clear;
begin
  FilterText := '';
  if fUseSQL then
  	FilterSQL := '';
end;

//******************************************************************************
{ TTSDBGridColumns }
//******************************************************************************

function TTSDBGridColumns.GetColumn(Index: Integer): TTSColumn;
begin
  Result := TTSColumn(inherited Items[Index]);
end;

function TTSDBGridColumns.GetGrid: TTSDBGrid;
begin
  if inherited Grid is TTSDBGrid then
  	Result := TTSDBGrid(inherited Grid)
  else
    Result := nil;
end;

procedure TTSDBGridColumns.SetColumn(Index: Integer; Value: TTSColumn);
begin
  Items[Index].Assign(Value);
end;

function TTSDBGridColumns.FindColumn(Field: TField): TTSColumn;
var
  I: Integer;
begin
  Result := nil;
  If Assigned(Field) then
    for I := 0 to pred(Count) do
      if Field = Items[I].Field then
      begin
        Result := Items[I];
        Exit;
      end;
end;

procedure TTSDBGridColumns.Update(Item: TCollectionItem);
var
  Col: TTSColumn;
begin
  inherited Update(Item);
  if (UpdateCount = 0)
  	and Assigned(Grid)
    and Assigned(Grid.Filter)
    and Grid.Filter.Enabled
    and not (gsAddingAutoColumns in Grid.GridStatus)
    and not (csDestroying in Grid.ComponentState)
    and not (csLoading in Grid.ComponentState)
  then begin

    if Assigned(Item) and (Item is TTSColumn) then
    begin
      Col := Item as TTSColumn;
      if Assigned(Col.Filter.fFilterEdit)
        and Col.Filter.fFilterEdit.Visible
      then
        Col.Filter.UpdateEdit;
	    Grid.Filter.FilterGrid(Item.Index);
    end
//    else
//	    Grid.Filter.FilterGrid(-1)
  end;
end;

function TTSDBGridColumns.DesignColumns: Boolean;
begin
	Result := inherited HasDesignColumns;
end;

function TTSDBGridColumns.Add: TTSColumn;
begin
  Result := TTSColumn(inherited Add);
end;


//******************************************************************************
{ TTSColumn }
//******************************************************************************

constructor TTSColumn.Create(ACollection: TCollection);
begin
  fCreating := True;
  inherited Create(ACollection);
  fColFilter := TTSColumnFilter.Create(Self);
  fCreating := False;
end;

destructor TTSColumn.Destroy;
begin
  fColFilter.Free;
  inherited Destroy;
end;

function TTSColumn.GetSortOrder: NTSGridSortOrder;
begin
	Result := fSortOrder;
end;

function TTSColumn.GetGrid: TTSDBGrid;
begin
  if Collection is TTSDBGridColumns then
  	Result :=	TTSDBGridColumns(Collection).Grid
  else
  	Result := nil;
end;

function TTSColumn.GetColWidth: Integer;
begin
  if Visible then
  	Result := Width
  else begin
    Visible := True;
    try
	    Result := Width;
    finally
  	  Visible := False;
    end;
  end;
end;

function TTSColumn.GetSortPos: Integer;
begin
	Result := fSortPos;
end;

procedure TTSColumn.SetColWidth(const AValue: Integer);
begin
  if Visible then
		Width := AValue
  else begin
    Visible := True;
    try
			Width := AValue;
    finally
  	  Visible := False;
    end;
  end;
end;

procedure TTSColumn.SetSortOrder(const AValue: NTSGridSortOrder);
var
  Grd: TTSDBGrid;
  I, N: Integer;
begin
  if AValue <> fSortOrder then
  begin
    Grd := Grid;
    if (fSortOrder = gsoNoSort) then
    begin
      N := Length(Grd.Sorting.fSortList);
			SetLength(Grd.Sorting.fSortList, N+1);
      Grd.Sorting.fSortList[N] := Index;
      fSortPos := N+1;
    end;
	  fSortOrder := aValue;
    if (fSortOrder = gsoNoSort) then
    begin
      N := pred(Length(Grd.Sorting.fSortList));
			for I := fSortPos to N do
      	Grd.Sorting.fSortList[I-1] := Grd.Sorting.fSortList[I];
      SetLength(Grd.Sorting.fSortList, N);
      fSortPos := 0;
    end;
    Grd.Sorting.SortingChanged;
  end;
end;

procedure TTSColumn.SetSortPos(AValue: Integer);
var
  Grd: TTSDBGrid;
  I, N: Integer;
begin
  Grd := Grid;
  if fSortPos = AValue then
  	Exit;
  //1.Fall: neuer Wert=0 => Feld aus Sortierung entfernen
  if AValue = 0 then
  begin
    fSortOrder := gsoNoSort;
    N := pred(Length(Grd.Sorting.fSortList));
		for I := fSortPos to N do
      Grd.Sorting.fSortList[I-1] := Grd.Sorting.fSortList[I];
    SetLength(Grd.Sorting.fSortList, N);
    fSortPos := 0;
    Grd.Sorting.SortingChanged;
	end
  //2.Fall: Feld ist bisher nicht in Sortierung => neu in Sortierung aufnehmen
  else if (fSortOrder = gsoNoSort) then
  begin
  	fSortOrder := gsoAscending;
    N := Length(Grd.Sorting.fSortList);
		SetLength(Grd.Sorting.fSortList, N+1);
    //2a: neue Sortierposition am Ende der Sortierung eintragen.
    if AValue > N then
    begin
	    Grd.Sorting.fSortList[N] := Index;
	    fSortPos := N+1;
    end
    //2b: neue Sortierposition in die Liste einfügen
    else begin
      for I := N downto AValue do
	    	Grd.Sorting.fSortList[I] := Grd.Sorting.fSortList[I-1];
      Grd.Sorting.fSortList[AValue-1] := Index;
      fSortPos := AValue;
    end;
    Grd.Sorting.SortingChanged;
  end
  //3.Fall: Feld wird in Sortierung verschoben.
  else begin
  	//3a: Feld wird in der Sortierung nach HINTEN verschoben
  	if AValue > fSortPos then
    begin
      N := Length(Grd.Sorting.fSortList);
      if AValue > N then
      	AValue := N;
      for I := fSortPos to pred(AValue) do
	    	Grd.Sorting.fSortList[I-1] := Grd.Sorting.fSortList[I];
    	Grd.Sorting.fSortList[AValue-1] := Index;
    end
  	//3b: Feld wird in der Sortierung nach VORNE verschoben
  	else if AValue < fSortPos then
    begin
			for I := pred(fSortPos) downto AValue do
	    	Grd.Sorting.fSortList[I] := Grd.Sorting.fSortList[I-1];
			Grd.Sorting.fSortList[AValue-1] := Index;
    end;
		fSortPos := AValue;
    Grd.Sorting.SortingChanged;
  end;
end;

function TTSColumn.DefaultWidth: Integer;
begin
  Result := GetDefaultWidth;
  if Result > Grid.MaxColumnDefaultWidth then
  	Result := Grid.MaxColumnDefaultWidth;
end;

procedure TTSColumn.ChangeSort(NewOrder: NTSGridSortOrder; NewPos: Integer);
begin
	fSortOrder := NewOrder;
  fSortPos := NewPos;
end;

procedure TTSColumn.Assign(Source: TPersistent);
begin
  if Source is TTSColumn then begin
    Collection.BeginUpdate;
    try
      inherited Assign(Source);
      fColFilter.Assign(TTSColumn(Source).fColFilter);
      fSortOrder := TTSColumn(Source).fSortOrder;
      fSortPos := TTSColumn(Source).fSortPos;
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TTSColumn.ColumnChanged;
begin
  inherited ColumnChanged;
  if Assigned(Filter.fFilterEdit)
    and Filter.fFilterEdit.Visible
  then
  	Filter.UpdateEdit;
end;

function TTSColumn.IsVisibleStored: Boolean;
begin
	if Assigned(Field) then
		Result := Visible <> Field.Visible
	else
		Result := True;
end;



{ TTSDBGrid }


//******************************************************************************
{ TTSDBGrid }
//******************************************************************************

constructor TTSDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDoStore := False;
  fMaxDefWidth := 200;
  fAlternateStyle := TTSGridCellStyle.Create(Self);
  fFilter := TTSGridFilter.Create(Self);
  fSorting := TTSGridSorting.Create(Self);
{$IFDEF SQLDB}
	fFieldNameDelimiters := TTSSQLFieldNameDelimiter.Create(Self);
{$ELSE}
	fFieldNameDelimiters := TTSFieldNameDelimiter.Create(Self);
{$ENDIF}
  fMouseDownState.MouseShift := [];
  fMouseDownState.X := -1;
  fMouseDownState.Y := -1;
  fMouseDownState.Button := mbLeft;

  fOptionsForm := TTSDBGridOptions.Create(Self);
  FixedRows := 1;
end;

destructor TTSDBGrid.Destroy;
begin
  fSorting.Free;
  fFilter.Free;
  fOptionsForm.Free;
  fAlternateStyle.Free;
  inherited Destroy;
end;

function TTSDBGrid.CreateColumns: TGridColumns;
begin
  Result := TTSDBGridColumns.Create(Self, TTSColumn);
end;

procedure TTSDBGrid.DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  D, RctOffs: Integer;
  SortL, TxT, ArT, ArL, ArB, ArR, ArC, TxW, TxH, cx, cy, w, h: Integer;
  SO: NTSGridSortOrder;
  S: string;
  AColumn: TTSColumn;
  PC, BC: TColor;
  BS: TBrushStyle;
  TxMet: TLCLTextMetric;
  Img: TBitmap;
const
  txtIndent = 3;
  rctMargin = 3;
  rctRound = 6;

begin
  fCurrentDrawRow := aRow;
  inherited DrawCell(aCol, aRow, aRect, aState);
  if not (gdFixed in aState) or (aRow > 0) then
  	Exit;

  BC := Canvas.Brush.Color;
  BS := Canvas.Brush.Style;
  PC := Canvas.Pen.Color;
  if (aCol<FirstGridColumn) and fOptionsForm.ShowTitleMark then
  begin
    w := aRect.Right - aRect.Left;
    h := aRect.Bottom - aRect.Top;
    cx := (w div 2) + aRect.Left;
    cy := (h div 2) + aRect.Top;
    Canvas.Brush.Color := TitleFont.Color;
    Canvas.Pen.Color := TitleFont.Color;
    Canvas.Polygon([Point(cx-5, cy-5)
                      , Point(cx-1, cy+5)
    									, Point(cx+3, cy-5)
                      ]);
  end
  else if (aCol>=FirstGridColumn) and (aCol < Columns.Count) then
   begin
    AColumn := Columns[ACol-FirstGridColumn];
    cy := (aRect.Top + aRect.Bottom) div 2;
    SortL := aRect.Right;

    if Assigned(AColumn) and fSorting.ShowInTitles then
    begin
      SO := AColumn.SortingOrder;
      if SO <> gsoNoSort then
      begin
        S := IntToStr(AColumn.SortingPosition);

        if fSorting.fBGRect then
      	  RctOffs := rctMargin
        else
          RctOffs := 0;

        Canvas.Font.Assign(TitleFont);
        Canvas.GetTextMetrics(TxMet);
        TxH := TxMet.Height - 2*TxMet.Descender;
    	  TxW := Canvas.TextWidth(S);
        cx := aRect.Right - TxW - txtIndent - RctOffs;

        TxT := cy - TxMet.Height div 2;
        D := TxH div 2;
        ArB := cy + TxH - D;
        ArT := cy - D;

        D := TxH div 4;
        ArC := cx - D;
        ArL := ArC - D;
        ArR := cx;
 	      SortL := ArL - RctOffs;

        if SO = gsoAscending then
        begin
          if fSorting.fBGRect then
          begin
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := fSorting.fBGColorAsc;
            Canvas.Pen.Color := fSorting.fBGColorAsc;
            Canvas.RoundRect(SortL, ArT - RctOffs,
        								    aRect.Right - txtIndent, ArB + RctOffs,
                            rctRound, rctRound);
          end;
          Canvas.Brush.Color := fSorting.ColorAscending;
          Canvas.Pen.Color := fSorting.ColorAscending;
          Canvas.Font.Color := fSorting.ColorAscending;
          Canvas.Polygon([Point(ArL, ArT)
              	  								  , Point(ArR, ArT)
                									  , Point(ArC, ArB)
            											  ]);
          Canvas.Brush.Style := bsClear;
          Canvas.TextOut(ArR + 2, TxT, S);
        end
        else begin
          if fSorting.fBGRect then
          begin
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := fSorting.fBGColorDesc;
            Canvas.Pen.Color := fSorting.fBGColorDesc;
            Canvas.RoundRect(ArL - RctOffs, ArT - RctOffs,
        								    aRect.Right - txtIndent, ArB + RctOffs,
                            rctRound, rctRound);
          end;
          Canvas.Brush.Color := fSorting.ColorDescending;
          Canvas.Pen.Color := fSorting.ColorDescending;
          Canvas.Font.Color := fSorting.ColorDescending;
          Canvas.Polygon([Point(ArL, ArB)
              	  								  , Point(ArR, ArB)
                									  , Point(ArC, ArT)
            											  ]);
          Canvas.Brush.Style := bsClear;
          Canvas.TextOut(ArR + 2, TxT, S);
        end;
      end;
    end;

    if Assigned(AColumn)
      and Filter.TitleBarMark
    	and AColumn.Filter.Enabled
    	and (AColumn.Filter.FilterText > '')
      and not AColumn.Filter.fFilterEdit.Visible
      and (Filter.EditType in [gfeTitleBarAutoHide
      																			, gfeTitleBarAlwaysHide
                                            , gfeManual
                                            , gfeOptionsForm
                                            ])
    then begin
      h := aRect.Bottom - aRect.Top;
	    Img := fFilter.GetFilterImage(h-2);
      TxH := Img.Height;
      if h >= TxH then
      begin
        cy := cy - TxH div 2;
        Canvas.Draw(SortL - TxH - 1, cy, Img);
      end
      else begin
        Canvas.Brush.Color := clYellow;
        Canvas.Brush.Style := bsSolid;
        Canvas.Pen.Color := clYellow;
        Canvas.Ellipse(SortL - h - 2, aRect.Top+1, SortL - 1, aRect.Top + h - 1)
      end
    end;
  end;
  Canvas.Brush.Style := BS;
  Canvas.Brush.Color := BC;
  Canvas.Pen.Color := PC;
end;

procedure TTSDBGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
begin
	inherited PrepareCanvas(aCol, aRow, aState);
end;


procedure TTSDBGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
	inherited HeaderClick(IsColumn, index);
end;

procedure TTSDBGrid.GetCellProps(Field: TField; CellStyle: TTSGridCellStyle; State: TGridDrawState);
begin

end;


procedure TTSDBGrid.ShowPopupForm;
begin
	if not Assigned(DBGridOptionsForm) then
  	Application.CreateForm(TDBGridOptionsForm, DBGridOptionsForm);
  if Assigned(fShowOptionsForm) then
  	fShowOptionsForm(Self);
	DBGridOptionsForm.Caption := fOptionsForm.FormCaption;
  DBGridOptionsForm.EditGridSettings(Self);
end;

function TTSDBGrid.GetCurrentColumn: TTSColumn;
begin
  if Columns.Enabled 
	and (Columns.Count > SelectedIndex)
    and Columns[SelectedIndex].Visible
  then
    Result := TTSColumn(Columns[SelectedIndex])
  else
    Result := nil;
end;

function TTSDBGrid.GetTitleRowHeight: integer;
begin
	if dgTitles in Options then
  	Result := RowHeights[0]
  else
    Result := -1;
end;

function TTSDBGrid.GetColumns: TTSDBGridColumns;
begin
  Result := inherited Columns as TTSDBGridColumns;
end;

procedure TTSDBGrid.SetAlternateStyle(const Value: TTSGridCellStyle);
begin
  fAlternateStyle := Value;
  Invalidate;
end;

procedure TTSDBGrid.SetColumns(const AValue: TTSDBGridColumns);
begin
  inherited Columns := AValue;
end;

procedure TTSDBGrid.SetTitleRowHeight(const AValue: integer);
begin
	if dgTitles in Options then
  	RowHeights[0] := AValue;
end;

procedure TTSDBGrid.SetColumnSort(SortCol: TTSColumn);
var
  SL: TStrings;
  SO: NTSGridSortOrder;
begin
  if Assigned(SortCol) then
  begin
    SL := Sorting.FieldNames;
    if (SL.Count >= 1) and SameText(SL.Names[0], SortCol.FieldName) then
      case SortCol.SortingOrder of
        gsoAscending:
          SO := gsoDescending;
        gsoDescending:
          SO := gsoNoSort;
      else //gsoNoSort:
        SO := gsoAscending;
      end
    else
    	SO := gsoAscending;

    Sorting.BeginUpdate;
    try
			SL.Clear;
      SortCol.SortingOrder := SO;
    finally
      Sorting.EndUpdate;
    end;
  end;
end;

procedure TTSDBGrid.AddColumnSort(SortCol: TTSColumn);
begin
  if Assigned(SortCol) then
  begin
    case SortCol.SortingOrder of
      gsoNoSort:
        SortCol.SortingOrder := gsoAscending;
      gsoAscending:
        SortCol.SortingOrder := gsoDescending;
      gsoDescending:
        SortCol.SortingOrder := gsoNoSort;
    end;
    Invalidate;
  end;
end;

procedure TTSDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  ActiveCol: TTSColumn;
  Idx: integer;

  function CheckTrigger(Trigger: TTSGridTrigger): Boolean;
  begin
    Result := (trgKeyboard in Trigger.ActiveTriggers)
    					and (ShortCut(Key, Shift) = Trigger.ShortCut);
    if Result then
    	Key := 0;
  end;
begin
	Idx := SelectedIndex;
  if (Idx >= 0) then
	  ActiveCol := Columns[Idx]
  else
    ActiveCol := nil;
  if fOptionsForm.Trigger.Fired(Key, Shift, True) then
	  ShowPopupForm
  else if fSorting.TriggerNew.Fired(Key, Shift, True) then
	  SetColumnSort(ActiveCol)
  else if fSorting.TriggerAppend.Fired(Key, Shift, True) then
	  AddColumnSort(ActiveCol)
  else if fFilter.Trigger.Fired(Key, Shift, True) and Assigned(ActiveCol) then
   	ActiveCol.Filter.CheckShowEdit(True);
  inherited KeyDown(Key, Shift);
end;

function TTSDBGrid.ExecMouseTrigger(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
	GridPos: TPoint;
  ActiveCol: TTSColumn;

	function CheckTrigger(Trigger: TTSGridTrigger): Boolean;
	begin
    Result := (trgMouse in Trigger.ActiveTriggers)
    				and (Button = Trigger.MouseButton)
    				and (Shift = Trigger.MouseShift);
  	if GridPos.y >= FixedRows then
    	Result := Result and (ttaCells in Trigger.MouseArea)
    else
    	Result := Result and (ttaHeader in Trigger.MouseArea);
	end;

begin
  GridPos := MouseToCell(Point(X, Y));

  if (Button = fMouseDownState.Button)
  	and (Abs(X - fMouseDownState.X) <= maxClickMove)
  	and (Abs(Y - fMouseDownState.Y) <= maxClickMove)
  then begin
    Result := True;
    ActiveCol := TTSColumn(ColumnFromGridColumn(GridPos.x));

    Shift := Shift + fMouseDownState.MouseShift;
    if fOptionsForm.Trigger.Fired(Button, Shift, X, Y) then
		  ShowPopupForm
    else if fSorting.TriggerNew.Fired(Button, Shift, X, Y) then
  	  SetColumnSort(ActiveCol)
    else if fSorting.TriggerAppend.Fired(Button, Shift, X, Y) then
  	  AddColumnSort(ActiveCol)
    else if fFilter.Trigger.Fired(Button, Shift, X, Y) and Assigned(ActiveCol) then
 	  	ActiveCol.Filter.CheckShowEdit(True)
    else
	    Result := False;
  end
  else
    Result := False;
end;

procedure TTSDBGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if Assigned(Columns) and Assigned(Filter) then
	  Filter.UpdateEdits;
end;

procedure TTSDBGrid.WMCommand(var Message: TLMCommand);
begin
  if (fActiveFilterEdit <> nil) and (Message.Ctl = fActiveFilterEdit.Handle) then
    fActiveFilterEdit.Dispatch(Message)
  else
    inherited;
end;

procedure TTSDBGrid.ColRowMoved(IsColumn: Boolean; FromIndex, ToIndex: Integer);
begin
  inherited ColRowMoved(IsColumn, FromIndex, ToIndex);
  if IsColumn then
	  Filter.UpdateEdits;
end;

procedure TTSDBGrid.HeaderSized(IsColumn: Boolean; Index: Integer);
begin
  inherited HeaderSized(IsColumn, Index);
  if IsColumn then
	  Filter.UpdateEdits;
end;

procedure TTSDBGrid.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = fFilter.EditingPanel) then
  begin
    if csDestroying in AComponent.ComponentState then
      for I := 0 to pred(Columns.Count) do
        Columns[I].Filter.CreateFilterEdit;
    fFilter.EditingPanel := nil;
  end;
end;

procedure TTSDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  fMouseDownState.Button := Button;
  fMouseDownState.X := X;
  fMouseDownState.Y := Y;
  fMouseDownState.MouseShift := Shift;
end;

procedure TTSDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cell: TGridCoord;
begin
  inherited MouseUp(Button, Shift, X, Y);
  ExecMouseTrigger(Button, Shift, X, Y);
  Cell := MouseCoord(X,Y);
  if (Button = mbLeft)
    and (Cell.X = 0)
    and (Cell.Y = 0)
    and (dgTitles in Options)
    and (dgIndicator in Options)
    and OptionsForm.ShowTitleMark
  then
		ShowPopupForm;
end;

function TTSDBGrid.ColumnPos(ColIndex: integer): Integer;
var
  X, I: Integer;
begin
  X := 0;
  Result := -1;
  if not Columns[ColIndex].Visible
    or (TitleRowHeight <= 0)
    or (ColIndex < pred(LeftCol))
  then
  	Exit;

  I := 0;
  while I < FixedCols do
  begin
    X := X + ColWidths[I];
	  inc(I);
  end;
  I := LeftCol;
  while (ColumnIndexFromGridColumn(I) < ColIndex)
  	and (I<ColCount)
  do begin
	    X := X + ColWidths[I];
		inc(I);
  end;
  if I <= ColCount then
	  Result := X;
end;

procedure TTSDBGrid.LinkActive(Value: Boolean);
var
  I: Integer;
begin
  if fDoStore then
  begin
  	if Value then begin
		  inherited LinkActive(Value);
      for I := 0 to pred(Columns.Count) do
      	if Columns[I].Width > fMaxDefWidth then
        	Columns[I].Width := fMaxDefWidth;
      LoadSettings;
    end
    else begin
      SaveSettings;
      inherited LinkActive(Value);
    end;
	end
  else
	  inherited LinkActive(Value);
	if Value then
	fFieldNameDelimiters.CheckConnectionType;
end;

procedure TTSDBGrid.BeforeDestruction;
begin
  if fDoStore then
  	SaveSettings;
  inherited BeforeDestruction;
end;

procedure TTSDBGrid.Loaded;
begin
  inherited Loaded;
  InitDefaultColumns;
	fFieldNameDelimiters.CheckConnectionType;
end;

procedure TTSDBGrid.InitDefaultColumns;
var
  I: Integer;
begin
  SetLength(fDefaultColumns, Columns.Count);
  for I := 0 to pred(Columns.Count) do
  begin
    fDefaultColumns[I].Index := I;
    fDefaultColumns[I].FieldName := Columns[I].FieldName;
    fDefaultColumns[I].Width := Columns[I].Width;
    fDefaultColumns[I].Visible := Columns[I].Visible;
  end;
end;

function TTSDBGrid.StoreAlternateStyle: Boolean;
begin
  Result := not IsSameFont(fAlternateStyle.Font, Font)
          or (fAlternateStyle.Background <> clNone);

          end;

procedure TTSDBGrid.ResetDefaultColumns;
var
  I, K: Integer;
begin
  Columns.BeginUpdate;
  try
    for I := 0 to pred(Columns.Count) do
    begin
      for K := I to pred(Columns.Count) do
        if fDefaultColumns[I].FieldName = Columns[K].FieldName then
        begin
          Columns[K].Width := fDefaultColumns[I].Width;
          Columns[K].Visible := fDefaultColumns[I].Visible;
          Columns[K].Index := I;
          Break;
        end;
    end;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TTSDBGrid.LoadSettings;
var
	ColNr, I: Integer;
	ColData: TStrings;
  Conf: TTSCustomINI;
	N, V: string;
	Hidden: Boolean;
  ConfOwner: string;
begin
	if csDesigning in ComponentState then
  	Exit;
  if Assigned(fStorage) then
  	Conf := fStorage
  else
    Conf := TSApp.INI;
  if not Assigned(Conf) then
    Exit;
  if Assigned(Owner) then
	  ConfOwner := Owner.Name + Conf.LookupSeparators.Group
  else
	  ConfOwner := '(NoOwner)' + Conf.LookupSeparators.Group;

  Sorting.FieldNames.Text := Conf.Str[ConfOwner + Name + '_Sort'];
  ColData := TStringList.Create;
  try
    Conf.GetStrings(ConfOwner + Name, ColData);
    Columns.BeginUpdate;
    try
      for I := 0 to ColData.Count-1 do
      begin
        N := ColData.Names[I];
        V := ColData.Values[N];
		  Hidden := (V>'') and (V[1] in ['h', 'H']);
        for ColNr := I to Columns.Count-1 do
          if Columns[ColNr].FieldName = N then
          begin
            Columns[ColNr].ColWidth := StrToIntDef(V, Columns[I].ColWidth);
            Columns[ColNr].Index := I;
            Columns[ColNr].Visible := not Hidden;
            Break;
          end;
      end;
    finally
      Columns.EndUpdate;
    end;
  finally
    ColData.Free;
  end;
end;

procedure TTSDBGrid.SaveSettings;
var
	I: Integer;
  Conf: TTSCustomINI;
	ColData: TStringList;
  ConfOwner: string;
begin
	if csDesigning in ComponentState then
  	Exit;
  if Assigned(fStorage) then
  	Conf := fStorage
  else
    Conf := TSApp.INI;
  if not Assigned(Conf) then
    Exit;
  if Assigned(Owner) then
	  ConfOwner := Owner.Name + Conf.LookupSeparators.Group
  else
	  ConfOwner := '(NoOwner)' + Conf.LookupSeparators.Group;

  Conf.Str[ConfOwner + Name + '_Sort'] := Sorting.FieldNames.Text;

  ColData := TStringList.Create;
  try
  	for I := 0 to Columns.Count-1 do
  		if Columns[I].Visible then
  			ColData.Add(Columns[I].FieldName + '=' + IntToStr(Columns[I].ColWidth))
  		else
  			ColData.Add(Columns[I].FieldName + '=H' + IntToStr(Columns[I].ColWidth));
		Conf.PutStrings(ConfOwner + Name, ColData);
  finally
  	ColData.Free;
  end;
end;

{ TTSGridCellStyle }

constructor TTSGridCellStyle.Create(Grid: TTSDBGrid);
begin
  fGrid := Grid;
  fFont := TFont.Create;
  if Assigned(fGrid) then
    fFont.Assign(fGrid.Font);
  fFont.OnChange := @FontChanged;
  fBackground := clNone;
end;

destructor TTSGridCellStyle.Destroy;
begin
  fFont.Free;
  inherited Destroy;
end;

procedure TTSGridCellStyle.FontChanged(Sender: TObject);
begin
  if Assigned(fGrid) then
    fGrid.Invalidate;
end;

function TTSGridCellStyle.StoreFont: Boolean;
begin
  if Assigned(fGrid) then
    Result := not IsSameFont(fFont, fGrid.Font)
  else
    Result := True;
end;

procedure TTSGridCellStyle.SetBackground(const Value: TColor);
begin
  if fBackground <> Value then
  begin
    fBackground := Value;
    if Assigned(fGrid) then
      fGrid.Invalidate;
  end;
end;

initialization
{$i tsdbgrid_images.lrs}

end.
