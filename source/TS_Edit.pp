unit TS_Edit;
{ Made by Lutz Kutscher

	Known Issues:

	The Class TTSCustomEdit and its descendants contain the properties "GutterLeft" and "GutterRight".
	These properties add a blank space between the left/right border of the control and the contained text.
  They are realized using the windows message EM_SETMARGINS with the Parameters EC_LEFTMARGIN
  or EC_RIGHTMARGIN. These parameters are originally defined in the unit windows.pp. Therefore I suppose
  that this message - and hence the corresponding properties - have no function in other OSs.
  That's why they are enclosed in "IFDEF WINDOWS".

}

interface

uses
  Buttons,
	TSLib,
	TSLibDateTime,
	TSLibConvert,
	TSClasses,
  LMessages, LCLType,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  ETSAcceptError = class(ETSException);

	//***************************************************************************************
  // Aufzählungs- und Set-Typen
	NTSEditValueType = (evtInteger, evtFloat); //Datentyp für TTSNumEdit
  //Aktion, die bei der Übernahme des neuen Wertes in einem TTSEdit-Feld durchgeführt wird.
	NTSAcceptAction = (aaAccept,        //Neuen Inhalt übernhemen
                    aaReset,          //Neuen Inhalt verwerfen und auf den alten Wert zurücksetzen
                    aaSilentCorrect,  //Falls der TTSCustomEdit-Nachkomme eine Werteprüfung durchführt
                                      // und der neue Wert einen Fehler enthält wird automatisch
                                      // ein Korrekturwert eingetragen und akzeptiert.
                                      // Ist der Wert korrekt oder findet keine Werteprüfung statt
                                      // entspricht der Wert aaAccept
                    aaAbort,          //Die Übernahme wird mit einer EAbort-Exception abgebrochen.
                    aaRaise           //Die Übernahme wird mit einer EAcceptError-Exception abgebrochen.
                    );

  //Optionen des TTSNumEdit-Eingabefeldes.
  NNumEditOption = (neoDecimalRound,      //Runde den tatsächlichen Wert nach jeder Änderung auf
                                          // "DecimalPlaces" Nachkommastellen. Ist neoDecimalRound
                                          // NICHT gesetzt, wird nur die Anzeige gerundet,
                                          // nicht aber der gespeicherte Wert.
                    neoFormatOnEdit,      //Wenn gesetzt wird der Feldinhalt während der Eingabe formatiert
                                          // (Tausender-Trennung, Anzahl Nachkommastellen etc.)
                    neoFormatFixedOnEdit, //Wenn gesetzt, werden auch der Prä- und Postfix während
                                          //  der Bearbeitung angezeigt.
                    neoPrefixOnEdit,      //Wenn gesetzt, wird auch bei der Bearbeitung der Präfix-
                    neoPostfixOnEdit,     //  bzw Postfix-Text im Edit-Feld angezeigt.
                    neoAllowNULL          //Wenn gesetzt, wird bei Accept kein Fehler ausgelöst, wenn
                                          //  das Edit-Feld NULL enthält. Sonst wird entsprechend
                                          //  den Fehlerkorrektur-Einstellungen (OnValueError) verfahren.
                    );
  SNumEditOptions = set of NNumEditOption;
	NTSEditDispMode = (dmFocusedState, dmDisplay, dmEdit);

  NDateEditOpiton = (deoAllowRange,       //Wenn nicht aktiv, wird eine eingegebene Range als
                                          //  einfaches Datum ausgewertet.
                  deoApplyDefaultOnEnter  //Wenn aktiv wird bereits OnEnter geprüft ob Defaults
                                          //  eingesetzt werden sollen. sonst erst bei "Accept".
                  );
  SDateEditOptions = set of NDateEditOpiton;


	//***************************************************************************************
  // Methodentypen (für Ereignisbehandlungen)
  MTSAcceptEvent = procedure (Sender: TObject; var NewText: string; var AcceptAction: NTSAcceptAction) of object;
	MTSAcceptValueEvent = procedure (Sender: TObject; var Value: Extended; var AcceptAction: NTSAcceptAction) of object;

  MTSAcceptDateEvent = procedure (Sender: TObject; var Range: RDateRange; var AcceptAction: NTSAcceptAction) of object;
	MTSValidateErrorEvent = procedure (Sender: TObject; var DoRaise: Boolean) of object;
	MTSInvalidDateEvent = procedure(Sender: TObject; var DateText: string) of object;


const
  NumEditDefaultOptions: SNumEditOptions = [neoFormatOnEdit, neoPrefixOnEdit, neoPostfixOnEdit];
  DateEditDefaultOptions: SDateEditOptions = [deoAllowRange];

type


	//***************************************************************************************
	{ TTSCustomEdit }
	//***************************************************************************************
	TTSCustomEdit = class(TCustomEdit)
	private
    fOldLeft, fOldWidth: Cardinal;
    fAutoSizeRunning: TTSUpdateTracker;
		fAcceptOnExit: Boolean;
		fAcceptKey: TShortCut;
		fAntiFlickerGutter: Boolean;
		fAutoSizeWidth: Boolean;
		fGutterLeft: Integer;
		fGutterRight: Integer;
		fOnAccepted: TNotifyEvent;
		fResetKey: TShortCut;
		fOnAccept: MTSAcceptEvent;
		fOnReset: TNotifyEvent;
		fOldText: TCaption;
    HelperCanvas: TCanvas;

		procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
		procedure CMExit(var Message: TCMExit); message CM_EXIT;
		procedure LMChar(var Message: TLMChar); message LM_CHAR;
		procedure LMSize(var Message: TLMSize); message LM_SIZE;
		procedure LMMove(var Message: TLMMove); message LM_MOVE;
		procedure SetAntiFlickerGutter(AValue: Boolean);
		procedure SetAutoSizeWidth(AValue: Boolean);
		procedure SetGutterLeft(AValue: Integer);
		procedure SetGutterRight(AValue: Integer);

		function StoreAcceptKey: Boolean;
		function StoreResetKey: Boolean;
	protected
	  procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
	                                   WithThemeSpace: Boolean); override;
    procedure TextChanged; override;
    procedure RealSetText(const Value: TCaption); override;


		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure CreateWnd; override;
		procedure Loaded; override;

    procedure SetAcceptKey(const Value: TShortCut); virtual;
		procedure SetResetKey(const Value: TShortCut); virtual;
		function CalcAutoWidth: Integer; virtual;
    procedure AutoSetWidth; virtual;

	public
		constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
		function TryFocus: Boolean;
		function Accept: Boolean; virtual;
		procedure AcceptSilent; virtual;
		procedure Reset; virtual;
	  procedure Clear;
	protected //Methods that will become published in TTSEdit.
    property GutterLeft: Integer read fGutterLeft write SetGutterLeft default 3;
    property GutterRight: Integer read fGutterRight write SetGutterRight default 3;
    property AntiFlickerGutter: Boolean read fAntiFlickerGutter write SetAntiFlickerGutter;
    property AutoSizeWidth: Boolean read fAutoSizeWidth write SetAutoSizeWidth default False;
		property AcceptOnExit: Boolean read fAcceptOnExit write fAcceptOnExit default True;
		property AcceptKey: TShortCut read fAcceptKey write SetAcceptKey stored StoreAcceptKey;
		property ResetKey: TShortCut read fResetKey write SetResetKey stored StoreResetKey;
		property OnAccept: MTSAcceptEvent read fOnAccept write fOnAccept;
    property OnAccepted: TNotifyEvent read fOnAccepted write fOnAccepted;
		property OnReset: TNotifyEvent read fOnReset write fOnReset;
	end;


	//***************************************************************************************
	{ TTSEdit }
	//***************************************************************************************
	TTSEdit = class(TTSCustomEdit)
	published
		//Eigenschaften aus TTSCustomEdit
		property AcceptOnExit;
		property AcceptKey;
		property ResetKey;
    property GutterLeft;
    property GutterRight;
    property AutoSizeWidth;
    property AntiFlickerGutter;

		//Ereignisse aus TTSCustomEdit
		property OnAccept;
    property OnAccepted;
		property OnReset;

		//Eigenschaften und Ereignisse aus TCustomEdit
    property AutoSize;
		property Alignment;
		property BorderStyle;
		property BorderSpacing;
		property Anchors;
		property AutoSelect;
		property BiDiMode;
		property CharCase;
		property Color;
		property Constraints;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property Font;
		property HideSelection;
		property MaxLength;
		property ParentBiDiMode;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
		property PopupMenu;
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
		property Visible;
		property OnChange;
		property OnClick;
		property OnContextPopup;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDock;
		property OnStartDrag;
	end;


  TTSCustomBtnEdit = class;
	//***************************************************************************************
	{ TTSEditButton }
	//***************************************************************************************
  TTSEditButton = class(TSpeedButton);

	//***************************************************************************************
	{ TTSEditButtonWin }
	//***************************************************************************************
  TTSEditButtonWin = class(TWinControl);

	//***************************************************************************************
	{ TTSSpeedBtnProps }
	//***************************************************************************************
	//Helper Class for TTSCustomBtnEdit to group the Button's properties.
	TTSSpeedBtnProps = class(TPersistent)
	private
    fOwner: TTSCustomBtnEdit;
		fSpeedBtn: TTSEditButton;
		fFlat: Boolean;
		fSpacing: Integer;
		fBtnWidth: Integer;
		fNumGlyphs: Integer;
		fCaption: TCaption;
		fShortCut: TShortCut;
		fPropsChanged: TNotifyEvent;
		fEditFont: Boolean;
		fUseDefaultGlyph: Boolean;
		procedure SetBtnWidth(const Value: Integer);
		procedure SetCaption(const Value: TCaption);
		procedure SetFlat(const Value: Boolean);
		procedure SetGlyph(const Value: TBitmap);
		procedure SetNumGlyphs(const Value: Integer);
		procedure SetSpacing(const Value: Integer);
		function GetGlyph: TBitmap;
		function GetFont: TFont;
		procedure SetEditFont(const Value: Boolean);
		procedure SetFont(const Value: TFont);
		procedure SetUseDefaultGlyph(AValue: Boolean);
		function StoreWidth: Boolean;
		function StoreFont: Boolean;
		function StoreShortCut: Boolean;
	protected
		procedure PropsChanged; virtual;
	public
		constructor Create(Owner: TTSCustomBtnEdit; AssociatedButton: TTSEditButton);
		property OnPropsChanged: TNotifyEvent read fPropsChanged write fPropsChanged;
	published
		property Caption: TCaption read fCaption write SetCaption;
		property Width: Integer read fBtnWidth write SetBtnWidth stored StoreWidth;
		property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphUseDefault: Boolean read fUseDefaultGlyph write SetUseDefaultGlyph default True;
		property NumGlyphs: Integer read fNumGlyphs write SetNumGlyphs default 1;
		property Flat: Boolean read fFlat write SetFlat default False;
		property ShortCut: TShortCut read fShortCut write fShortCut stored StoreShortCut;
		property Spacing: Integer read fSpacing write SetSpacing default 4;
		property Font: TFont read GetFont write SetFont stored StoreFont;
		property EditFont: Boolean read fEditFont write SetEditFont default True;
	end;

	//***************************************************************************************
	{ TTSCustomBtnEdit }
	//***************************************************************************************
	TTSCustomBtnEdit = class(TTSCustomEdit)
	private
		fBtn: TTSEditButton;
		fBC: TTSEditButtonWin;
		fBtnClick: TNotifyEvent;
		fBtnProps: TTSSpeedBtnProps;
		procedure fBtnOnClick(Sender: TObject);
		procedure LMSize(var Message: TLMSize); message LM_SIZE;
		procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
		procedure LMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
		procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
	protected
		procedure CreateWnd; override;
		procedure CreateParams(var Params: TCreateParams); override;
		procedure ResizeBtn; virtual;
		procedure ResizeEdit; virtual;
		procedure ButtonClick; virtual;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
		procedure BtnPropsChanged(Sender: TObject); virtual;
    procedure SetDefaultButtonGlyph; virtual;
	public
		function CalcAutoWidth: Integer; override;
		constructor Create(AOwner: TComponent); override;
	protected
		property EditButton: TTSSpeedBtnProps read fBtnProps write fBtnProps;
		property OnButtonClick: TNotifyEvent read fBtnClick write fBtnClick;
	end;

  //***************************************************************************************
  { TTSBtnEdit }
  //***************************************************************************************
  	TTSBtnEdit = class(TTSCustomBtnEdit)
      //Eigenschaften / Ereignisse aus TTSCustomBtnEdit
  		property EditButton;
  		property OnButtonClick;

      //Eigenschaften aus TTSCustomEdit
  		property AcceptOnExit;
  		property AcceptKey;
  		property ResetKey;
      property GutterLeft;
      property GutterRight;
      property AutoSizeWidth;
      property AntiFlickerGutter;

  		//Ereignisse aus TTSCustomEdit
  		property OnAccept;
  		property OnReset;
      property OnAccepted;

  		//Eigenschaften und Ereignisse aus TCustomEdit
      property AutoSize;
  		property Alignment;
  		property BorderStyle;
  		property BorderSpacing;
  		property Anchors;
  		property AutoSelect;
  		property BiDiMode;
  		property CharCase;
  		property Color;
  		property Constraints;
  		property DragCursor;
  		property DragKind;
  		property DragMode;
  		property Enabled;
  		property Font;
  		property HideSelection;
  		property MaxLength;
  		property ParentBiDiMode;
  		property ParentColor;
  		property ParentFont;
  		property ParentShowHint;
  		property PasswordChar;
  		property PopupMenu;
  		property ReadOnly;
  		property ShowHint;
  		property TabOrder;
  		property TabStop;
  		property Text;
  		property Visible;
  		property OnChange;
  		property OnClick;
  		property OnContextPopup;
  		property OnDblClick;
  		property OnDragDrop;
  		property OnDragOver;
  		property OnEndDock;
  		property OnEndDrag;
  		property OnEnter;
  		property OnExit;
  		property OnKeyDown;
  		property OnKeyPress;
  		property OnKeyUp;
  		property OnMouseDown;
  		property OnMouseMove;
  		property OnMouseUp;
  		property OnStartDock;
  		property OnStartDrag;
  	end;


	//***************************************************************************************
	{ TTSCustomNumEdit }
	//***************************************************************************************
	TTSCustomNumEdit = class(TTSCustomBtnEdit)
	private
		fDecimalPlaces: Integer;
		fOnAcceptValue: MTSAcceptValueEvent;
		fValType: NTSEditValueType;
		fOldValue: Extended;
		fFormatting: Boolean;
		fValue: Extended;
		fPrefix: string;
		fDispFmt: string;
		fMaxValue: Extended;
		fMinValue: Extended;
		fPostfix: string;
		fOnValError: NTSAcceptAction;
		fEmptyValue: Extended;
		fFocused: Boolean;
	  fOptions: SNumEditOptions;
    Accepting: Boolean;

		procedure LMPaste(var Message: TLMPaste); message LM_PASTE;
		procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
		procedure CMExit(var Message: TCMExit); message CM_EXIT;
		function GetAsFloat: Extended;
		function GetAsInteger: Integer;
		function GetDisplayText: string;
		function GetValueText: string;
		procedure SetAsInteger(const Val: Integer);
		procedure SetDecimalPlaces(const Val: Integer);
		procedure SetDisplayFormat(const Val: string);
		procedure SetMaxValue(const Val: Extended);
		procedure SetMinValue(const Val: Extended);
		procedure SetPrefix(const Val: string);
		procedure SetValType(const Val: NTSEditValueType);
		procedure SetPostfix(const Val: string);
		procedure SetEmptyValue(const Val: Extended);
		function NotMaxNULL: Boolean;
		function NotMinNULL: Boolean;
		function StoreEmptyValue: Boolean;
		function StoreDispFmt: Boolean;
		function GetEditText: string;
		function GetIsEmpty: Boolean;
		function GetAsVariant: Variant;
	  procedure SetOptions(const Value: SNumEditOptions);
	  function StoreOptions: Boolean;
	protected
		procedure CreateWnd; override;
		procedure KeyPress(var Key: Char); override;
		procedure Change; override;
		procedure SetAsFloat(Val: Extended); virtual;
		procedure SetValue(Val: Extended);
		procedure EvalText;
		procedure UpdateDisplay(DisplayMode: NTSEditDispMode=dmFocusedState);
		procedure DefineProperties(Filer: TFiler); override;
		procedure ReadExtendedValues(Reader: TReader);
		procedure WriteExtendedValues(Writer: TWriter);
		procedure SetEditText; virtual;
		procedure SetDisplayText; virtual;
    procedure SetDefaultButtonGlyph; override;
	public
		constructor Create(AOwner: TComponent); override;
		property DisplayText: string read GetDisplayText;
		property EditText: string read GetEditText;
		property ValueText: string read GetValueText;
		function CheckValue(var Val: Extended): Boolean;
		function CheckBounds(var Val: Extended): Boolean;
		function NumStringAt(Position: Integer): string;
		property IsEmpty: Boolean read GetIsEmpty;
		function Accept: Boolean; override;
		procedure ButtonClick; override;
		procedure Reset; override;
	protected //Eigenschaften zur Veröffentlichung ("published")
		property Alignment default taRightJustify;
		property Prefix: string read fPrefix write SetPrefix;
		property Postfix: string read fPostfix write SetPostfix;
		property ValueType: NTSEditValueType read fValType write SetValType default evtFloat;
		property MinNumValue: Extended read fMinValue write SetMinValue stored NotMinNULL;
		property MaxNumValue: Extended read fMaxValue write SetMaxValue stored NotMaxNULL;
		property EmptyValue: Extended read fEmptyValue write SetEmptyValue stored StoreEmptyValue;
		property DecimalPlaces: Integer read fDecimalPlaces write SetDecimalPlaces default 2;
	  property Options: SNumEditOptions read fOptions write SetOptions stored StoreOptions;

		property DisplayFormat: string read fDispFmt write SetDisplayFormat stored StoreDispFmt;
		property OnValueError: NTSAcceptAction read fOnValError write fOnValError default aaRaise;
		property OnAcceptValue: MTSAcceptValueEvent read fOnAcceptValue write fOnAcceptValue;

		property AsInteger: Integer read GetAsInteger write SetAsInteger stored False;
		property Value: Extended read GetAsFloat write SetAsFloat;
		property AsVariant: Variant read GetAsVariant;
	end;

  //***************************************************************************************
  { TTSNumEdit }
  //***************************************************************************************
  TTSNumEdit = class(TTSCustomNumEdit)
		//Eigenschaften aus TTSCustomNumEdit
		property Prefix;
		property Postfix;
		property ValueType;
		property MinNumValue;
		property MaxNumValue;
		property EmptyValue;
		property DecimalPlaces;
    property Options;
		property DisplayFormat;
		property OnValueError;
		property OnAcceptValue;
		property AsInteger;
		property Value;
		property AsVariant;

    //Eigenschaften / Ereignisse aus TTSCustomBtnEdit
		property EditButton;
		property OnButtonClick;

		//Eigenschaften / Ereignisse aus TTSCustomEdit
		property AcceptOnExit;
		property AcceptKey;
		property ResetKey;
    property GutterLeft;
    property GutterRight;
    property AutoSizeWidth;
    property AntiFlickerGutter;
		property OnAccept;
		property OnReset;
    property OnAccepted;

		//Eigenschaften und Ereignisse aus TCustomEdit
    property AutoSize;
		property Alignment;
		property BorderStyle;
		property BorderSpacing;
		property Anchors;
		property AutoSelect;
		property BiDiMode;
		property CharCase;
		property Color;
		property Constraints;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property Font;
		property HideSelection;
		property MaxLength;
		property ParentBiDiMode;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
		property PopupMenu;
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		//property Text;
		property Visible;
		property OnChange;
		property OnClick;
		property OnContextPopup;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDock;
		property OnStartDrag;
	end;

	//***************************************************************************************
	{ TTSCustomDateEdit }
	//***************************************************************************************

	TTSCustomDateEdit = class(TTSCustomBtnEdit)
	private
    fOldDate: TDateTime;
    fOldRangeEnd: TDateTime;
    fOldRangeType: NRangeType;
    fSettingDates: Boolean;
    fFormatting: Boolean;
    fFocused: Boolean;

		fDate: TDateTime;
		fRangeDate: TDateTime;
    fRangeType: NRangeType;
		fDefaults: TTSDateRangeCalc;
    fOnValError: NTSAcceptAction;
    fOnAcceptDate: MTSAcceptDateEvent;
    fMaxDate: TTSDateCalc;
    fMinDate: TTSDateCalc;
    fDateFormat: string;
    fOptions: SDateEditOptions;

		procedure LMPaste(var Message: TLMPaste); message LM_PASTE;
		procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
		procedure CMExit(var Message: TCMExit); message CM_EXIT;

		procedure SetDate(Value: TDateTime);
		procedure SetRangeDate(const Value: TDateTime);
		procedure SetDefaults(const Value: TTSDateRangeCalc);
		function GetDisplayText: string;
		function GetEditText: string;
		function GetIsEmpty: Boolean;
    function GetIsRange: Boolean;
    procedure MinMaxChanged(Sender: TObject);
    procedure SetDateFormat(const Value: string);
    function StoreDateFormat: Boolean;
    procedure SetRangeType(const Value: NRangeType);
    function GetRange: RDateRange;
    procedure SetRange(Value: RDateRange);
    function GetGlobalRangeSeparator: string;
    function GetGlobalTodayToken: string;
    procedure SetGlobalRangeSeparator(const Value: string);
    procedure SetGlobalTodayToken(const Value: string);
    procedure SetOptions(const Value: SDateEditOptions);
    function StoreOptions: Boolean;
    function GetRangeText: string;
	protected
		procedure CreateWnd; override;
		procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
		procedure Change; override;
		procedure EvalText;
		procedure ButtonClick; override;
    procedure SetDefaultButtonGlyph; override;
		procedure UpdateDisplay(DisplayMode: NTSEditDispMode=dmFocusedState);
		procedure SetEditText; virtual;
		procedure SetDisplayText; virtual;
	public
		constructor Create(AOwner: TComponent); override;
		function Accept: Boolean; override;
		procedure Reset; override;
    procedure ApplyDefaults(Force: Boolean=False);
		function CheckRange(var ARange: RDateRange): Boolean;
		function CheckBounds(var ADate: TDateTime): Boolean;

		property IsEmpty: Boolean read GetIsEmpty;
		property DisplayText: string read GetDisplayText;
    property RangeText: string read GetRangeText;
		property EditText: string read GetEditText;
    property GlobalTodayToken: string read GetGlobalTodayToken write SetGlobalTodayToken;
    property GlobalRangeSeparator: string read GetGlobalRangeSeparator write SetGlobalRangeSeparator;
    property IsRange: Boolean read GetIsRange;

		property Date: TDateTime read fDate write SetDate;
		property RangeDate: TDateTime read fRangeDate write SetRangeDate;
    property RangeType: NRangeType read fRangeType write SetRangeType;
    property Range: RDateRange read GetRange write SetRange;

	protected //Eigenschaften zur Veröffentlichung ("published")
		property Defaults: TTSDateRangeCalc read fDefaults write SetDefaults;
		property MinDate: TTSDateCalc read fMinDate write fMinDate;
		property MaxDate: TTSDateCalc read fMaxDate write fMaxDate;
    property DateFormat: string read fDateFormat write SetDateFormat stored StoreDateFormat;
    property Options: SDateEditOptions read fOptions write SetOptions stored StoreOptions;
		property OnValueError: NTSAcceptAction read fOnValError write fOnValError default aaSilentCorrect;
		property OnAcceptDate: MTSAcceptDateEvent read fOnAcceptDate write fOnAcceptDate;
  end;

	TTSDateEdit = class(TTSCustomDateEdit)
  published
		//Eigenschaften aus TTSCustomDateEdit
		property Defaults;
		property MinDate;
		property MaxDate;
    property DateFormat;
    property Options;
		property OnValueError;
		property OnAcceptDate;

    //Eigenschaften / Ereignisse aus TTSCustomBtnEdit
		property EditButton;
		property OnButtonClick;

		//Eigenschaften / Ereignisse aus TTSCustomEdit
		property AcceptOnExit;
		property AcceptKey;
		property ResetKey;
    property GutterLeft;
    property GutterRight;
    property AutoSizeWidth;
    property AntiFlickerGutter;
		property OnAccept;
		property OnReset;
    property OnAccepted;

    //Eigenschaften und Ereignisse aus TCustomEdit
    property AutoSize;
		property Alignment;
		property BorderStyle;
		property BorderSpacing;
		property Anchors;
		property AutoSelect;
		property BiDiMode;
		property CharCase;
		property Color;
		property Constraints;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property Font;
		property HideSelection;
		property MaxLength;
		property ParentBiDiMode;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
		property PopupMenu;
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;
		property OnChange;
		property OnClick;
		property OnContextPopup;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDock;
		property OnStartDrag;
	end;

	//***************************************************************************************
	{ TTSFileDialogProps }
	//***************************************************************************************
  TTSFileDialogProps = class(TPersistent)
	private
    fDialog: TOpenDialog;
		function GetDefaultExt: string;
		function GetFileName: String;
		function GetFilter: String;
		function GetFilterIndex: Integer;
		function GetHeight: integer;
		function GetHelpContext: THelpContext;
		function GetInitialDir: string;
		function GetOnCanClose: TCloseQueryEvent;
		function GetOnClose: TNotifyEvent;
		function GetOnFolderChange: TNotifyEvent;
		function GetOnHelpClicked: TNotifyEvent;
		function GetOnSelectionChange: TNotifyEvent;
		function GetOnShow: TNotifyEvent;
		function GetOnTypeChange: TNotifyEvent;
		function GetOptions: TOpenOptions;
		function GetTitle: TTranslateString;
		function GetWidth: integer;
		procedure SetDefaultExt(AValue: string);
		procedure SetFileName(AValue: String);
		procedure SetFilter(AValue: String);
		procedure SetFilterIndex(AValue: Integer);
		procedure SetHeight(AValue: integer);
		procedure SetHelpContext(AValue: THelpContext);
		procedure SetInitialDir(AValue: string);
		procedure SetOnCanClose(AValue: TCloseQueryEvent);
		procedure SetOnClose(AValue: TNotifyEvent);
		procedure SetOnFolderChange(AValue: TNotifyEvent);
		procedure SetOnHelpClicked(AValue: TNotifyEvent);
		procedure SetOnSelectionChange(AValue: TNotifyEvent);
		procedure SetOnShow(AValue: TNotifyEvent);
		procedure SetOnTypeChange(AValue: TNotifyEvent);
		procedure SetOptions(AValue: TOpenOptions);
		procedure SetTitle(AValue: TTranslateString);
		procedure SetWidth(AValue: integer);
		function StoreTitle: Boolean;
	public
		constructor Create(AssociatedDialog: TOpenDialog);
	published
	  property Title: TTranslateString read GetTitle write SetTitle stored StoreTitle;
	  property Width: integer read GetWidth write SetWidth default 0;
	  property Height: integer read GetHeight write SetHeight default 0;
    property Options: TOpenOptions read GetOptions write SetOptions default DefaultOpenDialogOptions;
	  property HelpContext: THelpContext read GetHelpContext write SetHelpContext default 0;

    property InitialDir: string read GetInitialDir write SetInitialDir;
    property DefaultExt: string read GetDefaultExt write SetDefaultExt;
    property FileName: String read GetFileName write SetFileName;
    property Filter: String read GetFilter write SetFilter;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;

    property OnShow: TNotifyEvent read GetOnShow write SetOnShow;
    property OnCanClose: TCloseQueryEvent read GetOnCanClose write SetOnCanClose;
    property OnClose: TNotifyEvent read GetOnClose write SetOnClose;
    property OnHelpClicked: TNotifyEvent read GetOnHelpClicked write SetOnHelpClicked;
    property OnFolderChange: TNotifyEvent read GetOnFolderChange write SetOnFolderChange;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
    property OnTypeChange: TNotifyEvent read GetOnTypeChange write SetOnTypeChange;
	end;


	//***************************************************************************************
	{ TTSCustomFolderEdit }
	//***************************************************************************************
	TTSCustomFolderEdit = class(TTSCustomBtnEdit)
	private
    fDialog: TSelectDirectoryDialog;
    fDialogProps: TTSFileDialogProps;
		fSelectedFolder: string;
	protected
    procedure SetDefaultButtonGlyph; override;
		procedure ButtonClick; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
    function ExecuteDialog: string;
		property SelectedFolder: string read fSelectedFolder write fSelectedFolder;
	protected
    property AutoSizeWidth default True;
    property DialogSettings: TTSFileDialogProps read fDialogProps write fDialogProps;
	end;


	//***************************************************************************************
	{ TTSFolderEdit }
	//***************************************************************************************
  TTSFolderEdit = class(TTSCustomFolderEdit)
  published
    //Eigenschaften / Ereignisse aus TTSCustomFolderEdit
    property DialogSettings;


    //Eigenschaften / Ereignisse aus TTSCustomBtnEdit
		property EditButton;
		property OnButtonClick;

		//Eigenschaften / Ereignisse aus TTSCustomEdit
		property AcceptOnExit;
		property AcceptKey;
    property AntiFlickerGutter;
    property AutoSizeWidth;
		property ResetKey;
    property GutterLeft;
    property GutterRight;
		property OnAccept;
		property OnReset;

    //Eigenschaften und Ereignisse aus TCustomEdit
    property AutoSize;
		property Alignment;
		property BorderStyle;
		property BorderSpacing;
		property Anchors;
		property AutoSelect;
		property BiDiMode;
		property CharCase;
		property Color;
		property Constraints;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property Font;
		property HideSelection;
		property MaxLength;
		property ParentBiDiMode;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
		property PopupMenu;
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;
		property OnChange;
		property OnClick;
		property OnContextPopup;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDock;
		property OnStartDrag;
	end;




procedure Register;

implementation


uses
	TypInfo,
  CompanyConstants,
  messages,
  TSResources,
	LCLIntf,
	Menus;

procedure Register;
begin
  {$I TS_Edit.lrs}
  RegisterComponents(DefaultComponentPage, [TTSEdit, TTSBtnEdit, TTSNumEdit, TTSDateEdit, TTSFolderEdit]);
end;

const
	EC_LEFTMARGIN = 1; 	//copied from defines.inc, included in windows.pp
	EC_RIGHTMARGIN = 2;	//copied from defines.inc, included in windows.pp

  btnGlyphName = 'STANDARDBTN';
	calcGlyphName = 'CALCBTN';
  calendarGlyphName = 'CALENDAR';
  folderGlyphName = 'FOLDERBTN';
  fileGlyphName = 'FILEBTN';


//***************************************************************************************
//***************************************************************************************
{ TTSCustomFolderEdit }
//***************************************************************************************
//***************************************************************************************

procedure TTSCustomFolderEdit.SetDefaultButtonGlyph;
begin
	EditButton.Glyph := TBitmap(CreateBitmapFromLazarusResource(folderGlyphName));
end;

procedure TTSCustomFolderEdit.ButtonClick;
begin
	inherited ButtonClick;
  ExecuteDialog;
end;

constructor TTSCustomFolderEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  fDialog := TSelectDirectoryDialog.Create(Self);
  fDialogProps := TTSFileDialogProps.Create(fDialog);
  AutoSizeWidth := True;
end;

destructor TTSCustomFolderEdit.Destroy;
begin
  fDialogProps.Free;
  fDialog.Free;
	inherited Destroy;
end;

function TTSCustomFolderEdit.ExecuteDialog: string;
begin
  if fDialog.Execute then
  	Text := fDialog.FileName;
end;

//***************************************************************************************
//***************************************************************************************
{ TTSFileDialogProps }
//***************************************************************************************
//***************************************************************************************

type
  DlgCracker = class(TOpenDialog);

function TTSFileDialogProps.GetDefaultExt: string;
begin
	Result := fDialog.DefaultExt;
end;

function TTSFileDialogProps.GetFileName: String;
begin
	Result := fDialog.FileName;
end;

function TTSFileDialogProps.GetFilter: String;
begin
	Result := fDialog.Filter;
end;

function TTSFileDialogProps.GetFilterIndex: Integer;
begin
	Result := fDialog.FilterIndex;
end;

function TTSFileDialogProps.GetHeight: integer;
begin
	Result := fDialog.Height;
end;

function TTSFileDialogProps.GetHelpContext: THelpContext;
begin
	Result := fDialog.HelpContext;
end;

function TTSFileDialogProps.GetInitialDir: string;
begin
	Result := fDialog.InitialDir;
end;

function TTSFileDialogProps.GetOnCanClose: TCloseQueryEvent;
begin
	Result := fDialog.OnCanClose;
end;

function TTSFileDialogProps.GetOnClose: TNotifyEvent;
begin
	Result := fDialog.OnClose;
end;

function TTSFileDialogProps.GetOnFolderChange: TNotifyEvent;
begin
	Result := fDialog.OnFolderChange;
end;

function TTSFileDialogProps.GetOnHelpClicked: TNotifyEvent;
begin
	Result := fDialog.OnHelpClicked;
end;

function TTSFileDialogProps.GetOnSelectionChange: TNotifyEvent;
begin
	Result := fDialog.OnSelectionChange;
end;

function TTSFileDialogProps.GetOnShow: TNotifyEvent;
begin
	Result := fDialog.OnShow;
end;

function TTSFileDialogProps.GetOnTypeChange: TNotifyEvent;
begin
	Result := fDialog.OnTypeChange;
end;

function TTSFileDialogProps.GetOptions: TOpenOptions;
begin
	Result := fDialog.Options;
end;

function TTSFileDialogProps.GetTitle: TTranslateString;
begin
	Result := fDialog.Title;
end;

function TTSFileDialogProps.GetWidth: integer;
begin
	Result := fDialog.Width;
end;

procedure TTSFileDialogProps.SetDefaultExt(AValue: string);
begin
	fDialog.DefaultExt := AValue;
end;

procedure TTSFileDialogProps.SetFileName(AValue: String);
begin
	fDialog.FileName := AValue;
end;

procedure TTSFileDialogProps.SetFilter(AValue: String);
begin
	fDialog.Filter := AValue;
end;

procedure TTSFileDialogProps.SetFilterIndex(AValue: Integer);
begin
	fDialog.FilterIndex := AValue;
end;

procedure TTSFileDialogProps.SetHeight(AValue: integer);
begin
	fDialog.Height := AValue;
end;

procedure TTSFileDialogProps.SetHelpContext(AValue: THelpContext);
begin
	fDialog.HelpContext := AValue;
end;

procedure TTSFileDialogProps.SetInitialDir(AValue: string);
begin
	fDialog.InitialDir := AValue;
end;

procedure TTSFileDialogProps.SetOnCanClose(AValue: TCloseQueryEvent);
begin
	fDialog.OnCanClose := AValue;
end;

procedure TTSFileDialogProps.SetOnClose(AValue: TNotifyEvent);
begin
	fDialog.OnClose := AValue;
end;

procedure TTSFileDialogProps.SetOnFolderChange(AValue: TNotifyEvent);
begin
	fDialog.OnFolderChange := AValue;
end;

procedure TTSFileDialogProps.SetOnHelpClicked(AValue: TNotifyEvent);
begin
	fDialog.OnHelpClicked := AValue;
end;

procedure TTSFileDialogProps.SetOnSelectionChange(AValue: TNotifyEvent);
begin
	fDialog.OnSelectionChange := AValue;
end;

procedure TTSFileDialogProps.SetOnShow(AValue: TNotifyEvent);
begin
	fDialog.OnShow := AValue;
end;

procedure TTSFileDialogProps.SetOnTypeChange(AValue: TNotifyEvent);
begin
	fDialog.OnTypeChange := AValue;
end;

procedure TTSFileDialogProps.SetOptions(AValue: TOpenOptions);
begin
	fDialog.Options := AValue;
end;

procedure TTSFileDialogProps.SetTitle(AValue: TTranslateString);
begin
	fDialog.Title := AValue;
end;

procedure TTSFileDialogProps.SetWidth(AValue: integer);
begin
	fDialog.Width := AValue;
end;

function TTSFileDialogProps.StoreTitle: Boolean;
begin
	Result := fDialog.Title <> DlgCracker(fDialog).DefaultTitle;
end;

constructor TTSFileDialogProps.Create(AssociatedDialog: TOpenDialog);
begin
	if not Assigned(AssociatedDialog) then
		raise Exception.Create('TTSFileDialogProps need an associated Dialog. Aborting creation.');
	inherited Create;
  fDialog := AssociatedDialog;
end;

//***************************************************************************************
//***************************************************************************************
{ TTSCustomEdit }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSCustomEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  fOldLeft := Left;
  fOldWidth := Width;
	fAcceptKey := ShortCut(13, []);
	fResetKey := ShortCut(27, []);
	fAcceptOnExit := True;
  fGutterLeft := 3;
  fGutterRight := 3;
  fAutoSizeWidth := False;
	fOldText := '';
  HelperCanvas := TCanvas.Create;

end;

//***************************************************************************************
destructor TTSCustomEdit.Destroy;
begin
  HelperCanvas.Free;
	inherited Destroy;
end;

//***************************************************************************************
procedure TTSCustomEdit.CreateWnd;
begin
	inherited CreateWnd;
	if HandleAllocated then begin
{$IFDEF WINDOWS}
//The definition for EC_LEFTMARGIN, EC_RIGHTMARGIN in this unit is merely copied from windows.pp
//	Thus it will probably have no function in other OS's
		SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN
    																	, MakeLong(Word(fGutterLeft), Word(fGutterRight)));
{$ENDIF}
	end;
  if fAutoSizeWidth then
    AutoSetWidth;
end;

//***************************************************************************************
procedure TTSCustomEdit.CMEnter(var Message: TCMEnter);
begin
	if fAcceptOnExit or not Modified then
		AcceptSilent;
	inherited;
end;

//***************************************************************************************
procedure TTSCustomEdit.CMExit(var Message: TCMExit);
begin
  try
  	if fAcceptOnExit and Modified then
	  	Accept;
  finally
  	inherited;
  end;
end;

//***************************************************************************************
procedure TTSCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
	SC: TShortCut;
begin
	SC := ShortCut(Key, Shift);
	if SC = fAcceptKey then begin
		Accept;
		Key := 0;
	end
	else if SC = fResetKey then
  begin
		Reset;
		Key := 0;
	end;
	inherited KeyDown(Key, Shift);
end;

//***************************************************************************************
procedure TTSCustomEdit.LMChar(var Message: TLMChar);
var
	SC: TShortCut;
begin
	SC := ShortCut(Message.CharCode, KeyDataToShiftState(Message.KeyData));
	if (SC<>fAcceptKey) and (SC<>fResetKey) then
		inherited;
end;

//***************************************************************************************
procedure TTSCustomEdit.LMSize(var Message: TLMSize);
begin
	inherited;
  if fAutoSizeWidth and TrackingUpdate(fAutoSizeRunning) then
  begin
    if (Align = alNone) then
    	case Alignment of
    	  taRightJustify:
			    Left := fOldLeft + fOldWidth - Width;
	      taCenter:
			    Left := fOldLeft + (fOldWidth - Width) div 2;
    	end;
	end;
	fOldLeft := Left;
	fOldWidth := Width;
end;

//***************************************************************************************
procedure TTSCustomEdit.LMMove(var Message: TLMMove);
begin
	fOldLeft := Left;
	fOldWidth := Width;
end;

//***************************************************************************************
procedure TTSCustomEdit.SetAntiFlickerGutter(AValue: Boolean);
begin
	if fAntiFlickerGutter = AValue then Exit;
	fAntiFlickerGutter := AValue;
  if fAutoSizeWidth then
    AutoSetWidth;
end;

//***************************************************************************************
procedure TTSCustomEdit.SetAutoSizeWidth(AValue: Boolean);
begin
	if fAutoSizeWidth = AValue then Exit;
	fAutoSizeWidth := AValue;
  if AValue then
    AutoSetWidth;
end;

//***************************************************************************************
procedure TTSCustomEdit.SetGutterLeft(AValue: Integer);
begin
	if fGutterLeft = AValue then Exit;
	fGutterLeft := AValue;
{$IFDEF WINDOWS}
//The definition for EC_LEFTMARGIN in this unit is merely copied from windows.pp
//	Thus it will probably have no function in other OS's
	if not HandleAllocated then
		Exit;
	SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, MakeLong(Word(fGutterLeft), 0));
{$ENDIF}
  if fAutoSizeWidth then
		AutoSetWidth;
end;

//***************************************************************************************
procedure TTSCustomEdit.SetGutterRight(AValue: Integer);
begin
	if fGutterRight = AValue then Exit;
	fGutterRight := AValue;
{$IFDEF WINDOWS}
//The definition for EC_RIGHTMARGIN in this unit is merely copied from windows.pp
//	Thus it will probably have no function in other OS's
	if not HandleAllocated then
		Exit;
	SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, Word(fGutterRight)));
{$ENDIF}
  if fAutoSizeWidth then
		AutoSetWidth;
end;

//***************************************************************************************
procedure TTSCustomEdit.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                                 									WithThemeSpace: Boolean);
begin
	inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  if fAutoSizeWidth then
		PreferredWidth := CalcAutoWidth;
end;

//***************************************************************************************
function TTSCustomEdit.CalcAutoWidth: Integer;
var
  WWidth: Integer;
begin
	HelperCanvas.Font := Self.Font;
	HelperCanvas.Handle := GetDC(Self.Handle);
  Result := HelperCanvas.GetTextWidth(Self.Text) + fGutterRight + fGutterLeft;
  if (Alignment = taLeftJustify) and fAntiFlickerGutter then
  begin
  	WWidth := HelperCanvas.GetTextWidth('W');
  	inc(Result, WWidth);
  end;
  if BorderStyle = bsSingle then
  	inc(Result, 7)
end;

//***************************************************************************************
procedure TTSCustomEdit.AutoSetWidth;
begin
  TrackUpdate(fAutoSizeRunning);
  try
  	InvalidatePreferredSize;
    AdjustSize;
	finally
    TrackingFinished(fAutoSizeRunning);
	end;
end;

//***************************************************************************************
procedure TTSCustomEdit.TextChanged;
var
  P, S: TPoint;
begin
	if fAutoSizeWidth then
  begin
    if (Alignment = taLeftJustify) and not fAntiFlickerGutter then
    begin
		  P := CaretPos;
		  AutoSetWidth;
			inherited TextChanged;
		  S.X := 0; S.Y := 0;
			CaretPos := S;
			CaretPos := P;
    end
    else begin
			inherited TextChanged;
		  AutoSetWidth;
		end;
	end
  else
		inherited TextChanged;
end;

//***************************************************************************************
procedure TTSCustomEdit.RealSetText(const Value: TCaption);
begin
	if fAutoSizeWidth then
  begin
    TrackUpdate(fAutoSizeRunning);
    try
  		inherited RealSetText(Value); //TWinControl.RealSetText enthält die Aufrufe von "IvalidatePreferredSize" und "AdjustSize".
  	finally
      TrackingFinished(fAutoSizeRunning);
  	end;
  end
  else
		inherited RealSetText(Value);

end;

//***************************************************************************************
procedure TTSCustomEdit.Clear;
begin
  inherited Clear;
  AcceptSilent;
end;

//***************************************************************************************
function TTSCustomEdit.TryFocus: Boolean;
begin
	Result := CanFocus and GetParentForm(Self).Showing;
	if Result then
		SetFocus;
end;

//***************************************************************************************
function TTSCustomEdit.StoreAcceptKey: Boolean;
begin
	Result := fAcceptKey <> ShortCut(13, []);
end;

//***************************************************************************************
function TTSCustomEdit.StoreResetKey: Boolean;
begin
	Result := fResetKey <> ShortCut(27, []);
end;

//***************************************************************************************
procedure TTSCustomEdit.SetAcceptKey(const Value: TShortCut);
begin
	if (Value=0) or (Value <> fResetKey) then
		fAcceptKey := Value
end;

//***************************************************************************************
procedure TTSCustomEdit.SetResetKey(const Value: TShortCut);
begin
	if (Value=0) or (Value <> fAcceptKey) then
		fResetKey := Value;
end;

//***************************************************************************************
procedure TTSCustomEdit.Loaded;
begin
	inherited Loaded;
	fOldLeft := Left;
  fOldWidth := Width;
	if fAutoSizeWidth then
    AutoSetWidth;
end;

//***************************************************************************************
procedure TTSCustomEdit.AcceptSilent;
begin
	fOldText := Text;
	Modified := False;
	SelectAll;
end;

//***************************************************************************************
function  TTSCustomEdit.Accept: Boolean;
var
	AccAc: NTSAcceptAction;
	Txt: string;
begin
	AccAc := aaAccept;
	Txt := Text;
	if Assigned(fOnAccept) then
		fOnAccept(Self, Txt, AccAc);
	Text := Txt;

	Result := False;
	case AccAc of
		aaAccept, aaSilentCorrect: begin
			fOldText := Text;
			Modified := False;
			SelectAll;
			Result := True;
      if Assigned(fOnAccepted) then
      	fOnAccepted(Self);
		end;
		aaAbort:
			if ComponentState * [csLoading, csDesigning] = [] then
				Abort;
		aaReset:
			Reset;
    aaRaise:
      raise ETSAcceptError.Create(Self, rsEditValueNotAccepted);
	end;
end;

//***************************************************************************************
procedure TTSCustomEdit.Reset;
begin
	if Assigned(fOnReset) then
		fOnReset(Self);
	Text := fOldText;
	Modified := False;
	SelectAll;
end;


//***************************************************************************************
//***************************************************************************************
{ TTSSpeedBtnProps }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSSpeedBtnProps.Create(Owner: TTSCustomBtnEdit; AssociatedButton: TTSEditButton);
begin

	if not (Assigned(AssociatedButton) and Assigned(Owner)) then
		raise Exception.Create('TTSSpeedBtnProps need an Owner and an associated Button. Aborting creation.');
	inherited Create;
  fOwner := Owner;
	fSpeedBtn := AssociatedButton;
	fFlat := fSpeedBtn.Flat;
	fSpacing := fSpeedBtn.Spacing;
	fBtnWidth := fSpeedBtn.Width;
	fNumGlyphs := fSpeedBtn.NumGlyphs;
  fUseDefaultGlyph := True;
	fCaption := fSpeedBtn.Caption;
	fShortCut := Menus.ShortCut(VK_F4, []);
  fEditFont := True;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.PropsChanged;
begin
	if Assigned(fPropsChanged) then
		fPropsChanged(Self);
end;

//***************************************************************************************
function TTSSpeedBtnProps.StoreShortCut: Boolean;
begin
	Result := fShortCut <> Menus.ShortCut(VK_F4, []);
end;

//***************************************************************************************
function TTSSpeedBtnProps.StoreWidth: Boolean;
begin
	Result := fSpeedBtn.Width <> fSpeedBtn.Height;
end;

//***************************************************************************************
function TTSSpeedBtnProps.StoreFont: Boolean;
begin
	Result := not fSpeedBtn.ParentFont;
end;

//***************************************************************************************
function TTSSpeedBtnProps.GetFont: TFont;
begin
	Result := fSpeedBtn.Font;
end;

//***************************************************************************************
function TTSSpeedBtnProps.GetGlyph: TBitmap;
begin
	Result := fSpeedBtn.Glyph;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetBtnWidth(const Value: Integer);
begin
	fSpeedBtn.Width := Value;
	fBtnWidth := Value;
	PropsChanged;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetCaption(const Value: TCaption);
begin
	fSpeedBtn.Caption := Value;
	fCaption := Value;
	PropsChanged;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetEditFont(const Value: Boolean);
begin
	fSpeedBtn.ParentFont := Value;
	fEditFont := Value;
	PropsChanged;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetFlat(const Value: Boolean);
begin
	fSpeedBtn.Flat := Value;
	fFlat := Value;
	PropsChanged;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetFont(const Value: TFont);
begin
	fSpeedBtn.Font.Assign(Value);
	fEditFont := fSpeedBtn.ParentFont;
	PropsChanged;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetUseDefaultGlyph(AValue: Boolean);
begin
	fUseDefaultGlyph := AValue;
  if AValue then
	  fOwner.SetDefaultButtonGlyph;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetGlyph(const Value: TBitmap);
begin
	fSpeedBtn.Glyph := Value;
	fNumGlyphs := fSpeedBtn.NumGlyphs;
  fUseDefaultGlyph := False;
	PropsChanged;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetNumGlyphs(const Value: Integer);
begin
	fSpeedBtn.NumGlyphs := Value;
	fNumGlyphs := Value;
	PropsChanged;
end;

//***************************************************************************************
procedure TTSSpeedBtnProps.SetSpacing(const Value: Integer);
begin
	fSpeedBtn.Spacing := Value;
	fSpacing := Value;
	PropsChanged;
end;



//***************************************************************************************
//***************************************************************************************
{ TTSCustomBtnEdit }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSCustomBtnEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	//for the Button a TWinControl is needed, so the Edit-Control can be clipped at
	// the button border.
	fBC := TTSEditButtonWin.Create(Self);
	with fBC do begin
		Parent := Self;
		Height := Self.Height;
		Width := Height;
		Top := 0;
		Left := Self.Width - Width -1;
    Align := alRight;
	end;
	fBtn := TTSEditButton.Create(Self);

	with fBtn do begin
		Parent := fBC;
    Align := alClient;
		OnClick := @fBtnOnClick;
	end;
	fBtnProps := TTSSpeedBtnProps.Create(Self, fBtn);
	fBtnProps.OnPropsChanged := @BtnPropsChanged;
  SetDefaultButtonGlyph;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.CMEnabledChanged(var Message: TLMessage);
begin
	inherited;
	fBtn.Enabled := Enabled;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.CMFontChanged(var Message: TLMessage);
begin
	inherited;
	if HandleAllocated then
		ResizeEdit;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.LMKeyDown(var Message: TLMKeyDown);
var
	SC: TShortCut;
begin
	SC := ShortCut(Message.CharCode, KeyDataToShiftState(Message.KeyData));
	if SC = fBtnProps.fShortCut then begin
		ButtonClick;
		Message.CharCode := 0;
		Message.Result := 0;
	end;
	inherited;
end;

//***************************************************************************************
function TTSCustomBtnEdit.CalcAutoWidth: Integer;
begin
	Result := inherited CalcAutoWidth + fBC.Width;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.LMSize(var Message: TLMSize);
begin
	inherited;
	ResizeBtn;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.ResizeBtn;
begin
	if not HandleAllocated then
		Exit;
	fBC.SetBounds(ClientWidth - fBtnProps.Width, 0, fBtnProps.Width, ClientHeight);
	//fBtn.SetBounds(0, 0, fBC.Width, fBC.Height);
	ResizeEdit;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.ResizeEdit;
var
  WWidth : Integer;
	R: TRect;
  function GetCharWidth: Integer;
  begin
    if HelperCanvas.HandleAllocated then
	  	Result := HelperCanvas.GetTextWidth('W')
    else
    	Result := 10;
	end;
begin
	SetRect(R, GutterLeft, 0, ClientWidth - fBC.Width - GutterRight, ClientHeight);
  case Alignment of
    taRightJustify: begin
	  	WWidth := GetCharWidth;
	  	dec(R.Left, WWidth);
		end;
    taCenter: begin
  		WWidth := GetCharWidth DIV 2;
	  	dec(R.Left, WWidth);
      inc(R.Right, WWidth);
		end;
	end;
	SendMessage(Handle, EM_SETRECT, 0, Integer(@R));
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.BtnPropsChanged(Sender: TObject);
begin
	ResizeBtn;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.SetDefaultButtonGlyph;
begin
	EditButton.Glyph := TBitmap(CreateBitmapFromLazarusResource(btnGlyphName));
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.ButtonClick;
begin
  if not Focused then
	  TryFocus;
	if Assigned(fBtnClick) then
		fBtnClick(Self)
	else
		Accept;
end;

//***************************************************************************************
function TTSCustomBtnEdit.ChildClassAllowed(ChildClass: TClass): boolean;
begin
	Result := inherited ChildClassAllowed(ChildClass);
  if not Result then
	  Result := (ChildClass=TTSEditButton) or (ChildClass=TTSEditButtonWin);
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);
	Params.Style := Params.Style
  							or ES_AUTOHSCROLL
								or WS_CLIPCHILDREN
								or ES_MULTILINE; //MultiLine needed for EM_SetRect to work properly.
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.CreateWnd;
begin
	inherited CreateWnd;
	ResizeBtn;
end;

//***************************************************************************************
procedure TTSCustomBtnEdit.fBtnOnClick(Sender: TObject);
begin
	ButtonClick;
end;



//***************************************************************************************
//***************************************************************************************
{ TTSCustomNumEdit }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSCustomNumEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	fMaxValue := TSNumNULL;
	fMinValue := TSNumNULL;
	fEmptyValue := TSNumNULL;
  fOptions := NumEditDefaultOptions;
	fFocused := False;
	fFormatting := False;
	fOnValError := aaRaise;
	fValType := evtFloat;
	fDecimalPlaces := 2;
	fDispFmt := ',0.00';
	Alignment := taRightJustify;
	fOldValue := 0;
	fValue := 0;
  EditButton.Width := 0;
  UpdateDisplay;
end;

//--------- Methoden der Botschaftsbehandlung und DFM-Speicherung  ------

//***************************************************************************************
procedure TTSCustomNumEdit.LMPaste(var Message: TLMPaste);
begin
	inherited;
	EvalText;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.CMEnter(var Message: TCMEnter);
begin
 	fFocused := True;
  UpdateDisplay;
	inherited;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.CMExit(var Message: TCMExit);
begin
	try
		fFocused := False;
		if fAcceptOnExit and Modified then
			Accept;
		DoExit;
	except
		TryFocus;
		SelectAll;
	end;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.DefineProperties(Filer: TFiler);
begin
	inherited DefineProperties(Filer);
	Filer.DefineProperty('ExtendedValues', @ReadExtendedValues, @WriteExtendedValues, True);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.ReadExtendedValues(Reader: TReader);
	function ReadExt: Extended;
	var
		S: string;
	begin
		S := Reader.ReadString;
		Result := StrToFloatDef(S, TSNumNULL);
	end;
begin
	Reader.ReadListBegin;
	fMinValue := ReadExt;
	fMaxValue := ReadExt;
	fEmptyValue := ReadExt;
	Reader.ReadListEnd;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.WriteExtendedValues(Writer: TWriter);
	procedure WriteExt(Val: Extended);
	begin
		Writer.WriteString(FloatToStrF(Val, ffGeneral, 20, 5));
	end;
begin
	Writer.WriteListBegin;
	WriteExt(fMinValue);
	WriteExt(fMaxValue);
	WriteExt(fEmptyValue);
	Writer.WriteListEnd;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.CreateWnd;
begin
	inherited CreateWnd;
	UpdateDisplay;
end;



//***************************************************************************************
//******** Methods to control user interaction and inherited behaviour
//***************************************************************************************

//***************************************************************************************
procedure TTSCustomNumEdit.ButtonClick;
begin
	if not Assigned(OnButtonClick) then
		ShowMessage(AnsiToUTF8(rsTSNumEditButtonMessage));
	inherited ButtonClick;
end;

//***************************************************************************************
function TTSCustomNumEdit.NumStringAt(Position: Integer): string;
var
	Dummy: Integer;
begin
	Result := ExtractNumString(Text, Position, Dummy)
end;

//***************************************************************************************
procedure TTSCustomNumEdit.KeyPress(var Key: Char);
var
	I, S, L: Integer;
	T: string;
begin
	T := Text;
	S := SelStart;
	L := SelLength;
	if SelText > '' then
		Delete(T, S, L);
	T := ExtractNumString(T, S, I);
	case Key of
		',', '.':
			if Pos(FormatSettings.DecimalSeparator, T) > 0 then
				Key := #0
			else
				Key := FormatSettings.DecimalSeparator;
		^A:
			SelectAll;
		^X, ^V, ^C, '+', '-', '*', '/', '^', #8:
			;
	else if not (AnsiChar(Key) in [#48..#57]) then
		Key := #0;
	end;
	inherited KeyPress(Key);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.Change;
begin
	if not fFormatting then
		EvalText;
	inherited Change;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.EvalText;
var
  Txt: string;
  SS: Integer;
  Opt: SCalcExpressionOptions;
begin
  SS := SelStart;
  Txt := Text;
  Opt := [];
  if neoFormatOnEdit in Options then
    Include(Opt, ceThousandSeparator);

  fValue := CalculateExpressionValue(Txt, SS, Opt);
  if neoPrefixOnEdit in Options then
  begin
    Txt := Prefix + Txt;
    SS := SS + Length(Prefix);
  end;
  if neoPostfixOnEdit in Options then
    Txt := Txt + fPostfix;


  fFormatting := True;
  try
    Text := Txt;
    SelStart := SS;
  finally
    fFormatting := False;
  end;
  Modified := True;
end;


//***************************************************************************************
//******* Methods to check or set the controls "Value"
//***************************************************************************************

//***************************************************************************************
procedure TTSCustomNumEdit.Reset;
begin
	fValue := fOldValue;
	if Assigned(fOnReset) then
		fOnReset(Self);
	UpdateDisplay;
	SelectAll;
	Modified := False;
end;

//***************************************************************************************
function TTSCustomNumEdit.Accept: Boolean;
var
	AccAc: NTSAcceptAction;
	Val: Extended;
	Txt: string;
begin
  if Accepting then
  	Exit;
  Accepting := True;
  try
		AccAc := aaAccept;
		Txt := Text;

		if (Text = '') or (fValue = TSNumNULL) then
			Val := fEmptyValue
		else
			Val := fValue;

		if (ComponentState * [csLoading, csDesigning]) <> [] then
			AccAc := aaSilentCorrect
		else if (Val = TSNumNull) or not CheckValue(Val) then
			AccAc := fOnValError;

		if Assigned(fOnAccept) then
			fOnAccept(Self, Txt, AccAc);

		if Assigned(fOnAcceptValue)
			and ((ComponentState * [csLoading, csDesigning]) = [])
		then
			fOnAcceptValue(Self, Val, AccAc);

		Result := False;
		case AccAc of
			aaAccept, aaSilentCorrect: begin
				SetValue(Val);
	      SelectAll;
				Result := True;
        if Assigned(fOnAccepted) then
        	fOnAccepted(Self);
			end;
			aaAbort:
				if (ComponentState * [csLoading, csDesigning]) = [] then
	      begin
	        SelectAll;
	 				Abort;
	      end
				else
					Reset;
			aaReset:
				Reset;
	    aaRaise: begin
				if (ComponentState * [csLoading, csDesigning]) <> [] then
	        Reset
	      else
		      raise ETSAcceptError.Create(Self, rsEditValueNotAccepted)
	    end;
		end;
  finally
    Accepting := False;
  end;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetAsFloat(Val: Extended);
begin
	if Text = '' then
		Text := '0';
	fValue := Val;
	Accept;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetValue(Val: Extended);
begin
	fValue := Val;
	fOldValue := Val;
	Modified := False;
	UpdateDisplay;
end;

//***************************************************************************************
// Checks whether Val is within the current max and min boundary settings. If not
// it adjusts the value accordingly and returns FALSE.
function TTSCustomNumEdit.CheckBounds(var Val: Extended): Boolean;
begin
	if (fMinValue <> TSNumNULL) and (Val < fMinValue) then begin
		Val := fMinValue;
		Result := False;
	end
	else if (fMaxValue <> TSNumNULL) and (Val > fMaxValue) then begin
		Val := fMaxValue;
		Result := False;
	end
	else
		Result := True;
end;

//***************************************************************************************
//Adjusts the contents of Val according to the current precision settings and value bounds
// Returns TRUE, if Val was within the current max and min boundaries.
function TTSCustomNumEdit.CheckValue(var Val: Extended): Boolean;
begin
	if fValType = evtInteger then
		Val := TSRound(Val)
	else if neoDecimalRound in Options then
		Val := TSRound(Val, fDecimalPlaces);

	Result := CheckBounds(Val);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetEditText;
begin
	fFormatting := True;
	try
		Text := GetEditText;
	finally
		fFormatting := False;
	end;
end;

//***************************************************************************************
function TTSCustomNumEdit.GetEditText: string;
var
	Fmt: string;
	E: Extended;
begin
	if (fDispFmt > '') and ([neoFormatOnEdit, neoFormatFixedOnEdit] * Options <> []) then
	begin
		Fmt := IIf(Pos(',', fDispFmt)>0, ',0', '0');
		if fValType = evtInteger then
			Result := FormatFloat(Fmt, GetAsInteger)
		else begin
			if fDecimalPlaces > 0 then
      begin
				if neoFormatFixedOnEdit in Options then
					Fmt := Fmt + '.' + StringOfChar('0', fDecimalPlaces)
				else
					Fmt := Fmt + '.' + StringOfChar('#', fDecimalPlaces);
			end;
			E := GetAsFloat;
			if E = TSNumNull then
				Result := ''
			else
				Result := FormatFloat(Fmt, E);
		end;
	end
	else if fValType = evtInteger then
		Result := IntToStr(GetAsInteger)
	else
		Result := FloatToStr(GetAsFloat);

  if neoPrefixOnEdit in Options then
    Result := Prefix + Result;
  if neoPostfixOnEdit in Options then
    Result := Result + fPostfix;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetDisplayText;
begin
	fFormatting := True;
	try
		Text := GetDisplayText;
	finally
		fFormatting := False;
	end;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetDefaultButtonGlyph;
begin
	EditButton.Glyph := TBitmap(CreateBitmapFromLazarusResource(calcGlyphName));
end;

//***************************************************************************************
function TTSCustomNumEdit.GetDisplayText: string;
begin
	if GetAsFloat = TSNumNull then
		Result := ''
	else if fValType = evtInteger then begin
		if fDispFmt > '' then
			Result := fPrefix + FormatFloat(fDispFmt, GetAsInteger) + fPostfix
		else
			Result := fPrefix + IntToStr(GetAsInteger) + fPostfix;
	end
	else begin
		if fDispFmt > '' then
			Result := fPrefix + FormatFloat(fDispFmt, GetAsFloat) + fPostfix
		else
			Result := fPrefix + FloatToStr(GetAsFloat) + fPostfix;
	end;
end;

//***************************************************************************************
function TTSCustomNumEdit.GetValueText: string;
begin
	if fValType = evtInteger then
		Result := IntToStr(GetAsInteger)
	else
		Result := FloatToStr(GetAsFloat);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.UpdateDisplay(DisplayMode: NTSEditDispMode);
var
	SS, TxL: Integer;
	DispEditText: Boolean;
begin
	case DisplayMode of
		dmDisplay: DispEditText := False;
		dmEdit: DispEditText := True;
	else
		DispEditText := fFocused;
	end;

  if DispEditText then begin
    SS := SelStart;
    TxL := Length(Text);
    SetEditText;
    SelStart := SS + Length(Text) - TxL;
  end
  else
    SetDisplayText;
	Invalidate;
end;

//***************************************************************************************
//****** Methods to get/set/store property values.
//***************************************************************************************

//***************************************************************************************
function TTSCustomNumEdit.GetAsFloat: Extended;
begin
	Result := fValue;
end;

//***************************************************************************************
function TTSCustomNumEdit.GetAsInteger: Integer;
begin
	Result := TSRound(fValue);
end;

//***************************************************************************************
function TTSCustomNumEdit.GetAsVariant: Variant;
begin
	if fValue = TSNumNull then
		Result := NULL
	else
		Result := fValue;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetAsInteger(const Val: Integer);
begin
	SetAsFloat(Val);
end;

//***************************************************************************************
function TTSCustomNumEdit.GetIsEmpty: Boolean;
begin
	Result := Text = '';
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetDecimalPlaces(const Val: Integer);
begin
	fDecimalPlaces := Val;
	if neoDecimalRound in Options then
		SetAsFloat(fValue);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetMaxValue(const Val: Extended);
begin
	if not NumIsTSNull(fMinValue) and (fMinValue > Val) then
		fMinValue := Val;
	fMaxValue := Val;
	if not NumIsTSNull(Val) and (fValue > Val) then
		SetAsFloat(fValue);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetMinValue(const Val: Extended);
begin
	if not NumIsTSNull(fMaxValue) and (fMaxValue < Val) then
		fMaxValue := Val;
	fMinValue := Val;
	if not NumIsTSNull(Val) and (fValue < Val) then
		SetAsFloat(fValue);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetEmptyValue(const Val: Extended);
begin
	fEmptyValue := Val;
	if Text = '' then
		SetAsFloat(fEmptyValue);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetValType(const Val: NTSEditValueType);
begin
	fValType := Val;
	if Val = evtInteger then
		SetAsFloat(fValue);
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetDisplayFormat(const Val: string);
begin
	fDispFmt := Val;
	UpdateDisplay;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetPostfix(const Val: string);
begin
	fPostfix := Val;
	UpdateDisplay;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetPrefix(const Val: string);
begin
	fPrefix := Val;
	UpdateDisplay;
end;

//***************************************************************************************
function TTSCustomNumEdit.StoreEmptyValue: Boolean;
begin
	Result := EmptyValue <> TSNumNULL;
end;

//***************************************************************************************
function TTSCustomNumEdit.NotMaxNULL: Boolean;
begin
	Result := fMaxValue <> TSNumNULL;
end;

//***************************************************************************************
function TTSCustomNumEdit.NotMinNULL: Boolean;
begin
	Result := fMinValue <> TSNumNULL;
end;

//***************************************************************************************
function TTSCustomNumEdit.StoreDispFmt: Boolean;
begin
	Result := fDispFmt <> ',0.00';
end;

//***************************************************************************************
function TTSCustomNumEdit.StoreOptions: Boolean;
begin
  Result := fOptions = NumeditDefaultOptions;
end;

//***************************************************************************************
procedure TTSCustomNumEdit.SetOptions(const Value: SNumEditOptions);
begin
  if fOptions <> Value then
  begin
    fOptions := Value;
    if neoDecimalRound in Options then
      SetAsFloat(fValue);
  end;
  fFormatting := True;
  try
    Change;
    UpdateDisplay;
  finally
    fFormatting := False;
  end;
end;

//***************************************************************************************
//***************************************************************************************
{ TTSCustomDateEdit }
//***************************************************************************************
//***************************************************************************************

//***************************************************************************************
constructor TTSCustomDateEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	fDefaults := TTSDateRangeCalc.Create;
  fMinDate := TTSDateCalc.Create;
  fMinDate.OnChange := @MinMaxChanged;
  fMaxDate := TTSDateCalc.Create;
  fMaxDate.OnChange := @MinMaxChanged;
  fFocused := False;
  fOnValError := aaSilentCorrect;
  fDateFormat := 'ddddd';
  fOptions := DateEditDefaultOptions;

  fDate := Defaults.Date;
  fRangeDate := Defaults.RangeDate;
  fRangeType := rgSingleValue;
  fOldDate := fDate;
  fOldRangeEnd := fRangeDate;
  fOldRangeType := fRangeType;
  EditButton.Width := 0;
  UpdateDisplay;
end;

//***************************************************************************************
function TTSCustomDateEdit.StoreOptions: Boolean;
begin
  Result := fOptions <> DateEditDefaultOptions;
end;

//***************************************************************************************
function TTSCustomDateEdit.StoreDateFormat: Boolean;
begin
  Result := (DateFormat > '') and (fDateFormat <> 'ddddd') ;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.Loaded;
begin
  inherited Loaded;
  UpdateDisplay;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.CreateWnd;
begin
	inherited CreateWnd;
	UpdateDisplay;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.LMPaste(var Message: TLMPaste);
begin
	inherited;
	EvalText;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.CMEnter(var Message: TCMEnter);
begin
 	fFocused := True;
  if deoApplyDefaultOnEnter in Options then
    ApplyDefaults;
  UpdateDisplay;
	inherited;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.CMExit(var Message: TCMExit);
begin
	try
		fFocused := False;
		if fAcceptOnExit and Modified then
			Accept;
		DoExit;
	except
		TryFocus;
		SelectAll;
	end;
end;

//***************************************************************************************
//******** Methods to control user interaction and inherited behaviour
//***************************************************************************************

//***************************************************************************************
procedure TTSCustomDateEdit.ButtonClick;
begin
	if not Assigned(OnButtonClick) then
	begin
    if deoAllowRange in Options then
  	  ShowMessage(Format(AnsiToUTF8(rsTSDateEditRangeButtonMsg), [TodayToken]))
    else
      ShowMessage(Format(AnsiToUTF8(rsTSDateEditButtonMsg), [TodayToken]));
	end;
	inherited ButtonClick;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetDefaultButtonGlyph;
begin
	EditButton.Glyph := TBitmap(CreateBitmapFromLazarusResource(calendarGlyphName));
end;

//***************************************************************************************
procedure TTSCustomDateEdit.KeyPress(var Key: Char);
var
	S, L: Integer;
	T: string;
begin
	T := Text;
	S := SelStart;
	L := SelLength;
	if SelText > '' then
		Delete(T, S, L);

	case Key of
		^A:
			SelectAll;
		^X, ^V, ^C, #8:
			;
    else if not (AnsiChar(Key) in [#48..#57, '+', '-', FormatSettings.DateSeparator])
        and (PosInText(Key, DateToken[dtYear] + DateToken[dtMonth] + DateToken[dtWeek]
                     + DateToken[dtDay] + TodayToken) = 0)
    then
      Key := #0;

	end;
	inherited KeyPress(Key);
end;

//***************************************************************************************
procedure TTSCustomDateEdit.UpdateDisplay(DisplayMode: NTSEditDispMode);
var
	SS: Integer;
	DispEditText: Boolean;
begin
	case DisplayMode of
		dmDisplay: DispEditText := False;
		dmEdit: DispEditText := True;
	else
		DispEditText := fFocused or Modified;
	end;

  if DispEditText then begin
    SS := SelStart;
    SetEditText;
    if SS  < Length(Text) then
      SelStart := SS
    else
      SelStart := Length(Text);
  end
  else
    SetDisplayText;
	Invalidate;
end;

//***************************************************************************************
function TTSCustomDateEdit.GetDisplayText: string;
var
  FromStr: string;
begin
  if fDate = TSDateNULL then
    FromStr := ''
  else
    FromStr := FormatDateTime(fDateFormat, fDate);

  case fRangeType of
    rgFromValue:
      Result := FromStr + RangeSeparator;
    rgToValue:
      Result := RangeSeparator + FromStr;
    rgFromToValue: begin
      if fRangeDate = TSDateNULL then
        Result := FromStr
      else
        Result := FromStr + RangeSeparator + FormatDateTime(fDateFormat, fRangeDate);
    end;
    else
      Result := FromStr;
  end;
end;

//***************************************************************************************
function TTSCustomDateEdit.GetEditText: string;
begin
  Result := GetDisplayText;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetDisplayText;
begin
	fFormatting := True;
	try
		Text := GetDisplayText;
	finally
		fFormatting := False;
	end;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetEditText;
begin
	fFormatting := True;
	try
		Text := GetEditText;
	finally
		fFormatting := False;
	end;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.Change;
begin
  if not fFormatting then
    EvalText;
  inherited Change;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.EvalText;
var
  Rg: RDateRange;
begin
	if (csDesigning in ComponentState) or (csLoading in ComponentState) then
		Exit;
  Rg := DateRange(Text, Defaults.Range);
  CheckRange(Rg);
  fDate := Rg.Date;
  if deoAllowRange in Options then
  begin
    fRangeDate := Rg.RangeDate;
    fRangeType := Rg.RangeType;
  end;
  Modified := True;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.MinMaxChanged(Sender: TObject);
begin
  Change;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.ApplyDefaults(Force: Boolean=False);
begin
  if Force or IsEmpty then
  begin
    fDate := Defaults.Date;
    fRangeDate := Defaults.RangeDate;
    fRangeType := Defaults.RangeType;
    UpdateDisplay;
    Change;
  end;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.Reset;
begin
	fDate := fOldDate;
  if deoAllowRange in Options then
    fRangeDate := fOldRangeEnd;
  fRangeType := fOldRangeType;
	if Assigned(fOnReset) then
		fOnReset(Self);
	UpdateDisplay;
	SelectAll;
	Modified := False;
end;

//***************************************************************************************
function TTSCustomDateEdit.Accept: Boolean;
var
	AccAc: NTSAcceptAction;
  Rg: RDateRange;
begin
	AccAc := aaAccept;

  ApplyDefaults;

  Rg := DateRange(fDate, fRangeDate, fRangeType);
	if Assigned(fOnAcceptDate)
		and (ComponentState * [csLoading, csDesigning] = [])
	then
		fOnAcceptDate(Self, Rg, AccAc);

	Result := False;
	case AccAc of
		aaAccept, aaSilentCorrect: begin
			SetRange(Rg);
			SelectAll;
			Result := True;
      if Assigned(fOnAccepted) then
      	fOnAccepted(Self);
		end;
		aaAbort:
			if ComponentState * [csLoading, csDesigning] = [] then
				Abort
			else
				Reset;
		aaReset:
			Reset;
    aaRaise: begin
			if ComponentState * [csLoading, csDesigning] <> [] then
        Reset;
      raise ETSAcceptError.Create(Self, rsEditValueNotAccepted);
    end;
	end;
end;

//***************************************************************************************
function TTSCustomDateEdit.CheckBounds(var ADate: TDateTime): Boolean;
begin
	if (fMinDate.Date <> TSDateNULL) and (ADate < fMinDate.Date) then begin
		ADate := fMinDate.Date;
		Result := False;
	end
	else if (fMaxDate.Date <> TSDateNULL) and (ADate > fMaxDate.Date) then begin
		ADate := fMaxDate.Date;
		Result := False;
  end
	else
		Result := True;
end;

//***************************************************************************************
function TTSCustomDateEdit.CheckRange(var ARange: RDateRange): Boolean;
var
  Original: RDateRange;
begin
  Original := ARange;
  if (ARange.RangeType=rgInvalid) or (ARange.Date=TSDateNULL) then
    ARange := DateRange(Defaults.Date, Defaults.RangeDate, Defaults.RangeType);

  if not (deoAllowRange in Options) then
    ARange.RangeType := rgSingleValue;

  ValidateDateRange(ARange);
  CheckBounds(ARange.Date);
  if ARange.RangeType = rgFromToValue then
    CheckBounds(ARange.RangeDate);
  Result := (Original.Date = ARange.Date)
            and (Original.RangeDate = ARange.RangeDate)
            and (Original.RangeType = ARange.RangeType);
end;

//***************************************************************************************
function TTSCustomDateEdit.GetIsEmpty: Boolean;
begin
	Result := (Text = '');
end;

//***************************************************************************************
function TTSCustomDateEdit.GetIsRange: Boolean;
begin
  Result := RangeType in [rgFromValue, rgToValue, rgFromToValue];
end;

//***************************************************************************************
function TTSCustomDateEdit.GetRange: RDateRange;
begin
  Result := DateRange(Date, RangeDate, RangeType);
end;

//***************************************************************************************
function TTSCustomDateEdit.GetRangeText: string;
begin
  Result := GetEnumName(TypeInfo(NRangeType), Integer(fRangeType));
  if fDate = TSDateNULL then
    Result := Result + ' / NULL'
  else
    Result := Result + ' / ' + FormatDateTime(fDateFormat, fDate);
  if fRangeDate = TSDateNULL then
    Result := Result + ' / NULL'
  else
    Result := Result + ' / ' + FormatDateTime(fDateFormat, fRangeDate);
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetRange(Value: RDateRange);
begin
  if not fSettingDates then
  begin
    fSettingDates := True;
    try
      CheckRange(Value);
      fDate := Value.Date;
      fRangeDate := Value.RangeDate;
      fRangeType := Value.RangeType;
      UpdateDisplay;
    finally
      fSettingDates := False;
    end;
  end;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetDate(Value: TDateTime);
begin
  if not fSettingDates then
  begin
    fSettingDates := True;
    try
      fDate := Value;
      CheckBounds(fDate);
    finally
      fSettingDates := False;
    end;
    UpdateDisplay;
  end;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetRangeDate(const Value: TDateTime);
begin
  if not fSettingDates then
  begin
    fSettingDates := True;
    try
      fRangeDate := Value;
      CheckBounds(fRangeDate);
    finally
      fSettingDates := False;
    end;
    UpdateDisplay;
  end;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetRangeType(const Value: NRangeType);
begin
  if deoAllowRange in Options then
    fRangeType := Value
  else if fDate = TSDateNULL then
    fRangeType := rgInvalid
  else
    fRangeType := rgSingleValue;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetDefaults(const Value: TTSDateRangeCalc);
begin
	fDefaults := Value;
  if csDesigning in ComponentState then
	  ApplyDefaults;
end;

//***************************************************************************************
function TTSCustomDateEdit.GetGlobalRangeSeparator: string;
begin
  Result := RangeSeparator;
end;

//***************************************************************************************
function TTSCustomDateEdit.GetGlobalTodayToken: string;
begin
  Result := TodayToken;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetGlobalRangeSeparator(const Value: string);
begin
  RangeSeparator := Value;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetGlobalTodayToken(const Value: string);
begin
  TodayToken := Value;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetDateFormat(const Value: string);
begin
  fDateFormat := Value;
  if not Focused then
    UpdateDisplay;
end;

//***************************************************************************************
procedure TTSCustomDateEdit.SetOptions(const Value: SDateEditOptions);
begin
  fOptions := Value;
  if not (deoAllowRange in Options) then
  begin
    fRangeDate := TSDateNULL;
    fOldRangeEnd := TSDateNULL;
    if fDate = TSDateNULL then
      fRangeType := rgSingleValue
    else
      fRangeType := rgInvalid;
    if fOldDate = TSDateNULL then
      fOldRangeType := rgSingleValue
    else
      fOldRangeType := rgInvalid;
  end;
  if (deoApplyDefaultOnEnter in Options) and Focused then
    ApplyDefaults;
end;



initialization
{$i tsedit_images.lrs}


end.
