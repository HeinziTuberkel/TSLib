unit TS_DBStateLabel;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  LMessages,
  DB,
  DBCtrls,
  TSLibConvert,
  TSClasses,
  StdCtrls,
	Classes, SysUtils;

type
	MGetFieldStateValue = procedure (Sender: TObject; Field: TField; var FieldState: NTriState);

  //***************************************************************************************
  { TTSCustomDBStateLabel }
  //***************************************************************************************
  	TTSCustomDBStateLabel = class(TCustomLabel)
  	private
  		fDataLink: TFieldDataLink;
			fGetStateValue: MGetFieldStateValue;
			fInactiveState: NTriState;
  		fTrueSettings: TTSTextProperties;
  		fFalseSettings: TTSTextProperties;
  		fUndefSettings: TTSTextProperties;
  		fDesignState: TCheckBoxState;
  		ChangingData: Boolean;
  		function GetDataField: string;
  		function GetDataSource: TDataSource;
  		procedure SetDataField(const Value: string);
  		procedure SetDataSource(const Value: TDataSource);
  		function GetField: TField;
  		procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
			procedure SetInactiveState(AValue: NTriState);
  	protected
			function GetState: NTriState; virtual;
  		function GetActiveSettings: TTSTextProperties; virtual;
  		procedure DataChange(Sender: TObject); virtual;
  		procedure SettingsChanged(Sender: TObject; PropID: Integer); virtual;

  		function GetLabelText: string; override;
  		procedure Loaded; override;
  		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  		procedure SetAutoSize(Value: Boolean); override;
  	public
  		constructor Create(AOwner: TComponent); override;
  		destructor Destroy; override;
  		//function ExecuteAction(ThisAction: TBasicAction): Boolean; override;
  		//function UpdateAction(ThisAction: TBasicAction): Boolean; override;
  		property Field: TField read GetField;
  		property ActiveSettings: TTSTextProperties read GetActiveSettings;
  		property State: NTriState read GetState;
  	protected
  		property DataField: string read GetDataField write SetDataField;
  		property DataSource: TDataSource read GetDataSource write SetDataSource;

  		property DisplayStyleTrue: TTSTextProperties read fTrueSettings write fTrueSettings ;
  		property DisplayStyleFalse: TTSTextProperties read fFalseSettings write fFalseSettings;
  		property DisplayStyleUndefined: TTSTextProperties read fUndefSettings write fUndefSettings;
  		property InactiveState: NTriState read fInactiveState write SetInactiveState;
  		property OnGetStateValue: MGetFieldStateValue read fGetStateValue write fGetStateValue;
    end;


    TTSDBStateLabel = class(TTSCustomDBStateLabel)
    published
    	property DataSource;
    	property DataField;
    	property DisplayStyleFalse;
    	property DisplayStyleTrue;
    	property DisplayStyleUndefined;
    	property InactiveState;

      property Align;
      property Anchors;
      property AutoSize;
      property BidiMode;
      property BorderSpacing;
      property Constraints;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      property FocusControl;
      property ParentBidiMode;
      property ParentShowHint;
      property PopupMenu;
      property ShowAccelChar;
      property ShowHint;
      property OnClick;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDrag;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnChangeBounds;
      property OnContextPopup;
      property OnResize;
      property OnStartDrag;
      property OptimalFill;
		end;


procedure Register;


implementation

uses
  CompanyConstants,
  TSLibDB;

//***************************************************************************************
procedure Register;
begin
  {$I TS_DBStateLabel.lrs}
  RegisterComponents(DefaultComponentPage, [TTSDBStateLabel]);
end;


{ TTSCustomDBStateLabel }

//***************************************************************************************
constructor TTSCustomDBStateLabel.Create(AOwner: TComponent);
begin
	fDataLink := nil;
	inherited Create(AOwner);
	ShowAccelChar := False;

	FDataLink := TFieldDataLink.Create;
	FDataLink.Control := Self;
	FDataLink.OnDataChange := @DataChange;

	fTrueSettings := TTSTextProperties.Create(self);
	fTrueSettings.Assign(Self);
	fTrueSettings.OnPropChanged := @SettingsChanged;

	fFalseSettings := TTSTextProperties.Create(self);
	fFalseSettings.Assign(Self);
	fFalseSettings.OnPropChanged := @SettingsChanged;

	fUndefSettings := TTSTextProperties.Create(self);
	fUndefSettings.Assign(Self);
	fUndefSettings.OnPropChanged := @SettingsChanged;
end;

//***************************************************************************************
destructor TTSCustomDBStateLabel.Destroy;
begin
	fTrueSettings.Free;
	fFalseSettings.Free;
	fUndefSettings.Free;
	fDataLink.Free;
	fDataLink := nil;
	inherited Destroy;
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.CMGetDataLink(var Message: TLMessage);
begin
	Message.Result := PtrUInt(FDataLink);
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.DataChange(Sender: TObject);
var
	LbSet: TTSTextProperties;
  OldLeft, OldWidth: Integer;
  UseAlignment, OldAlign: TAlignment;
begin
	if ChangingData then
		Exit;

	ChangingData := True;
	try
		LbSet := GetActiveSettings;

		if AutoSize then
		begin
      OldWidth := Width;
      OldLeft := Left;
      OldAlign := inherited Alignment;
      inherited Alignment := LbSet.Alignment;
  		inherited Caption := LbSet.Caption;
  		inherited Color := LbSet.Color;
  		inherited Font := LbSet.Font;
  		inherited Visible := LbSet.Visible;
  		inherited Transparent := LbSet.Transparent;
  		inherited WordWrap := LbSet.WordWrap;
			AdjustSize;
			if Width > OldWidth then
				UseAlignment := inherited Alignment
      else
				UseAlignment := OldAlign;
    	case UseAlignment of
        taLeftJustify: Left := OldLeft;
        taRightJustify: Left := OldLeft + OldWidth - Width;
        taCenter: Left := OldLeft + (OldWidth - Width) DIV 2;
  		end
		end
		else
	    LbSet.AssignTo(Self);
	finally
		ChangingData := False;
	end;
end;

//***************************************************************************************
function TTSCustomDBStateLabel.GetState: NTriState;
begin
	if Assigned(fDataLink)
		and fDataLink.Active
		and Assigned(fDataLink.Field)
	then
		Result := TriStateVal(fDataLink.Field)
	else
		Result := fInactiveState;

	if Assigned(fGetStateValue) then
		fGetStateValue(Self, fDataLink.Field, Result);
end;

//***************************************************************************************
function TTSCustomDBStateLabel.GetActiveSettings: TTSTextProperties;
begin
	case State of
		tsFalse:
			Result := fFalseSettings;
		tsTrue:
			Result := fTrueSettings;
	else
		Result := fUndefSettings;
	end
end;

//***************************************************************************************
function TTSCustomDBStateLabel.GetLabelText: string;
begin
	Result := ActiveSettings.Caption;
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.SettingsChanged(Sender: TObject; PropID: Integer);
begin
	if Sender = ActiveSettings then
		DataChange(Self);
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.SetInactiveState(AValue: NTriState);
begin
	if fInactiveState = AValue then Exit;
	fInactiveState := AValue;

	if not (Assigned(fDataLink)
					and fDataLink.Active
					and Assigned(fDataLink.Field))
 then
		DataChange(Self);
end;

//***************************************************************************************
//function TTSCustomDBStateLabel.ExecuteAction(ThisAction: TBasicAction): Boolean;
//begin
//	Result := inherited ExecuteAction(Action)
//						or (FDataLink <> nil)
//						and fDataLink.ExecuteAction(Action);
//end;

//***************************************************************************************
function TTSCustomDBStateLabel.GetDataField: string;
begin
	Result := fDataLink.FieldName;
end;

//***************************************************************************************
function TTSCustomDBStateLabel.GetDataSource: TDataSource;
begin
	Result := fDataLink.DataSource;
end;

//***************************************************************************************
function TTSCustomDBStateLabel.GetField: TField;
begin
	Result := fDataLink.Field;
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.Loaded;
begin
	inherited Loaded;
	if csDesigning in ComponentState then
		DataChange(Self);
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited Notification(AComponent, Operation);
	if (Operation = opRemove)
	and (FDataLink <> nil)
	and (AComponent = DataSource) then
		DataSource := nil;
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.SetAutoSize(Value: Boolean);
begin
	//Wenn der Label in einem Grid verwendet wird ("DataSourceFixed"=True), lasse Autosize = False.
	if not AutoSize and Assigned(fDataLink) and fDataLink.DataSourceFixed then
		Exit
	else if (Value <> AutoSize) then
		inherited SetAutoSize(Value);
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.SetDataField(const Value: string);
begin
	fDataLink.FieldName := Value;
end;

//***************************************************************************************
procedure TTSCustomDBStateLabel.SetDataSource(const Value: TDataSource);
begin
	if not (fDataLink.DataSourceFixed and (csLoading in ComponentState)) then
		fDataLink.DataSource := Value;
	if Value <> nil then Value.FreeNotification(Self);
end;


//***************************************************************************************
//function TTSCustomDBStateLabel.UpdateAction(ThisAction: TBasicAction): Boolean;
//begin
//	Result := inherited UpdateAction(ThisAction)
//						or (FDataLink <> nil)
//						and FDataLink.UpdateAction(ThisAction);
//end;


end.

