unit TSClassesDB;
{$I PackageDefs.inc}

{$mode objfpc}{$H+}

interface

uses
{$IFDEF SQLDB}
	DB,
	SQLdb,
	DBGrids,
{$ENDIF}
	Classes, SysUtils;

type
	NConnectionType = (ctCustom, ctMySQL, ctMSSQL, ctANSI);
	MGetConnectionType = procedure(Sender: TObject; var ConnectionType: NConnectionType) of object;
	MGetFieldDelimiter = procedure(Sender: TObject; IsStartDelimiter: Boolean; var Delimiter: string) of object;

{$IFDEF SQLDB}
  RConnectionTypeClass = record
  	ConnectionClassName: string;
  	ConnectionType: NConnectionType;
	end;
{$ENDIF}

	{ TTSFieldNameDelimiter }

  TTSFieldNameDelimiter = class(TPersistent)
	private
		fForceDelimiters: Boolean;
		fOnChangeConnectionType: MGetConnectionType;
		fOnChangeDelim: MGetFieldDelimiter;
		fOnCheckConnectionType: MGetConnectionType;
		fOwner: TComponent;
		fConnectionType: NConnectionType;
		fLastKnownConnectionType: NConnectionType;
		fFieldEndDelim: string;
		fFieldStartDelim: string;
		fUseDelims: Boolean;
		procedure SetFieldEndDelim(AValue: string);
		procedure SetFieldStartDelim(AValue: string);
	protected
		procedure SetConnectionType(AValue: NConnectionType); virtual;
		function IsConnectionTypeStored: Boolean; virtual;
		function IsEndDelimStored: Boolean; virtual;
		function IsStartDelimStored: Boolean; virtual;
	public
		constructor Create(AOwner: TComponent);
		procedure CheckConnectionType; virtual;
		procedure CheckFieldDelimiters; virtual;
		function Delimited(FieldName: string): string; virtual;
		property Owner: TComponent read fOwner;
	published
		property ConnectionType: NConnectionType read fConnectionType write SetConnectionType stored IsConnectionTypeStored;
		property FieldStartDelimiter: string read fFieldStartDelim write SetFieldStartDelim stored IsStartDelimStored;
		property FieldEndDelimiter: string read fFieldEndDelim write SetFieldEndDelim stored IsEndDelimStored;
		property UseFieldDelimiters: Boolean read fUseDelims write fUseDelims default True;
		property ForceDelimiters: Boolean read fForceDelimiters write fForceDelimiters default False;
		property OnCheckConnectionType: MGetConnectionType read fOnCheckConnectionType write fOnCheckConnectionType;
		property OnChangeConnectionType: MGetConnectionType read fOnChangeConnectionType write fOnChangeConnectionType;
		property OnChangeFieldDelim: MGetFieldDelimiter read fOnChangeDelim write fOnChangeDelim;
	end;


{$IFDEF SQLDB}
	{ TTSSQLFieldNameDelimiter }

	TTSSQLFieldNameDelimiter = class(TTSFieldNameDelimiter)
	private
		fGrid: TCustomDBGrid;
		fSrc: TDataSource;
		fQry: TCustomSQLQuery;
		fCurrentConnection: TSQLConnection;
		fUseOwnerConnectionType: Boolean;
		procedure SetUseOwnerType(AValue: Boolean);
	protected
		procedure SetConnectionType(AValue: NConnectionType); override;
		function IsConnectionTypeStored: Boolean; override;
	public
		constructor Create(AOwner: TComponent; GetConnectionTypeHandler: MGetConnectionType=nil);
		function OwnerConnectionType: NConnectionType;
		procedure CheckConnectionType; override;
		property Connection: TSQLConnection read fCurrentConnection;
	published
		property ConnectionTypeAutoDetect: Boolean read fUseOwnerConnectionType write SetUseOwnerType default True;
	end;
{$ENDIF}

const
	defaultDelimFieldStart: array[NConnectionType] of string = ('', '`', '[', '"');
	defaultDelimFieldEnd: array[NConnectionType] of string = ('', '`', ']', '"');

{$IFDEF SQLDB}
	connectionTypeClasses: array[1..12] of RConnectionTypeClass = (
											(ConnectionClassName: 'TMySQL40Connection'; ConnectionType: ctMySQL),
											(ConnectionClassName: 'TMySQL41Connection'; ConnectionType: ctMySQL),
											(ConnectionClassName: 'TMySQL50Connection'; ConnectionType: ctMySQL),
											(ConnectionClassName: 'TMySQL51Connection'; ConnectionType: ctMySQL),
											(ConnectionClassName: 'TMySQL55Connection'; ConnectionType: ctMySQL),
											(ConnectionClassName: 'TMySQL56Connection'; ConnectionType: ctMySQL),
											(ConnectionClassName: 'TMSSQLConnection'; ConnectionType: ctMSSQL),
											(ConnectionClassName: 'TODBCConnection'; ConnectionType: ctANSI),
											(ConnectionClassName: 'TOracleConnection'; ConnectionType: ctANSI),
											(ConnectionClassName: 'TPQConnection'; ConnectionType: ctANSI),
											(ConnectionClassName: 'TSQLite3Connection'; ConnectionType: ctANSI),
											(ConnectionClassName: 'TSybaseConnection'; ConnectionType: ctMSSQL)
											);
{$ENDIF}

implementation

//******************************************************************************
{ TTSFieldNameDelimiter }
//******************************************************************************

//******************************************************************************
constructor TTSFieldNameDelimiter.Create(AOwner: TComponent);
begin
	fConnectionType := ctCustom;
	fFieldStartDelim := defaultDelimFieldStart[fConnectionType];
	fFieldEndDelim := defaultDelimFieldEnd[fConnectionType];
	fUseDelims := True;
  fForceDelimiters := False;
end;

//******************************************************************************
procedure TTSFieldNameDelimiter.CheckConnectionType;
var
	NewConnectionType: NConnectionType;
begin
  if Assigned(fOnCheckConnectionType) then
  begin
		NewConnectionType := fConnectionType;
		fOnCheckConnectionType(fOwner, NewConnectionType);
		ConnectionType := NewConnectionType;
	end;
end;

//******************************************************************************
procedure TTSFieldNameDelimiter.CheckFieldDelimiters;
begin
	if fFieldStartDelim = defaultDelimFieldStart[fLastKnownConnectionType] then
		fFieldStartDelim := defaultDelimFieldStart[fConnectionType];
	if fFieldEndDelim = defaultDelimFieldEnd[fLastKnownConnectionType] then
		fFieldEndDelim := defaultDelimFieldEnd[fConnectionType];
end;

//******************************************************************************
function TTSFieldNameDelimiter.Delimited(FieldName: string): string;
var
	I: Integer;
	NeedDelim: Boolean;
const
	CharsAndNumbers = '0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
	AllChars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
begin
	if not UseFieldDelimiters then
	begin
		Result := FieldName;
		Exit;
	end;
	if ForceDelimiters then
	begin
		Result := fFieldStartDelim + FieldName + fFieldEndDelim;
		Exit;
	end;

	if FieldName > '' then
		NeedDelim := Pos(FieldName[1], AllChars)=0
	else
		NeedDelim := False;
	if not NeedDelim then
		for I := 2 to Length(FieldName) do
			if Pos(FieldName[I], CharsAndNumbers) = 0 then
			begin
				NeedDelim := True;
				Break;
			end;
	if NeedDelim then
		Result := fFieldStartDelim + FieldName + fFieldEndDelim
	else
		Result := FieldName;
end;

//******************************************************************************
procedure TTSFieldNameDelimiter.SetConnectionType(AValue: NConnectionType);
begin
	if fConnectionType = aValue then
		Exit;
  fLastKnownConnectionType := fConnectionType;
	fConnectionType := AValue;
	if Assigned(fOnChangeConnectionType) then
		fOnChangeConnectionType(Owner, fConnectionType);
	CheckFieldDelimiters;
end;

//******************************************************************************
procedure TTSFieldNameDelimiter.SetFieldEndDelim(AValue: string);
begin
	if fFieldEndDelim = AValue then Exit;
	if Assigned(fOnChangeDelim) then
		fOnChangeDelim(fOwner, False, AValue);
	fFieldEndDelim := AValue;
end;

//******************************************************************************
procedure TTSFieldNameDelimiter.SetFieldStartDelim(AValue: string);
begin
	if fFieldStartDelim = AValue then Exit;
	if Assigned(fOnChangeDelim) then
		fOnChangeDelim(fOwner, True, AValue);
	fFieldStartDelim := AValue;
end;

//******************************************************************************
function TTSFieldNameDelimiter.IsConnectionTypeStored: Boolean;
begin
	Result := True;
end;

//******************************************************************************
function TTSFieldNameDelimiter.IsEndDelimStored: Boolean;
begin
	Result := fFieldEndDelim <> defaultDelimFieldEnd[ConnectionType];
end;

//******************************************************************************
function TTSFieldNameDelimiter.IsStartDelimStored: Boolean;
begin
	Result := fFieldStartDelim <> defaultDelimFieldStart[ConnectionType];
end;

{$IFDEF SQLDB}

//******************************************************************************
{ TTSSQLFieldNameDelimiter }
//******************************************************************************

type
	TCrackDBGrid = class(TCustomDBGrid);

//******************************************************************************
constructor TTSSQLFieldNameDelimiter.Create(AOwner: TComponent; GetConnectionTypeHandler: MGetConnectionType);
begin
	fOwner := AOwner;
	fGrid := nil;
	fSrc := nil;
	fQry := nil;
	fCurrentConnection := nil;
  fUseOwnerConnectionType := True;

	if Assigned(fOwner) then
	begin
		if fOwner is TSQLConnection then
			fCurrentConnection := fOwner as TSQLConnection
		else if fOwner is TCustomSQLQuery then
			fQry := fOwner as TCustomSQLQuery
		else if fOwner is TDataSource then
			fSrc := fOwner as TDataSource
		else if fOwner is TCustomDBGrid then
			fGrid := fOwner as TCustomDBGrid;
	end;

  fCurrentConnection := nil;
	inherited Create(AOwner);
end;

//******************************************************************************
function TTSSQLFieldNameDelimiter.OwnerConnectionType: NConnectionType;
var
	I: Integer;
	CnClassName: string;
	CnDef: TConnectionDef;
begin
	if not Assigned(fOwner) then
	begin
		Result := ctCustom;
		Exit;
	end;

	//First find the connection used by the owner.
  if Assigned(fGrid) then
  	fSrc := TCrackDBGrid(fGrid).DataSource;
  if Assigned(fSrc) and Assigned(fSrc.DataSet) and (fSrc.DataSet is TSQLQuery) then
		fQry := fSrc.DataSet as TSQLQuery;
  if Assigned(fQry) and Assigned(fQry.DataBase) and (fQry.DataBase is TSQLConnection) then
  	fCurrentConnection := fQry.DataBase as TSQLConnection;

	//Now try to find the server type, this connection talks to.
	if Assigned(fCurrentConnection) then
	begin
		CnClassName := fCurrentConnection.ClassName;
		if fCurrentConnection is TSQLConnector then
		begin
			CnDef := GetConnectionDef(TSQLConnector(fCurrentConnection).ConnectorType);
			if Assigned(CnDef) then
				CnClassName := CnDef.ConnectionClass.ClassName;
		end;

		for I := Low(connectionTypeClasses) to High(connectionTypeClasses) do
		begin
			if SameText(CnClassName, connectionTypeClasses[I].ConnectionClassName) then
			begin
				Result  := connectionTypeClasses[I].ConnectionType;
				Break;
			end;
		end;
	end;
end;

//******************************************************************************
procedure TTSSQLFieldNameDelimiter.CheckConnectionType;
var
	LastConnection: TSQLConnection;
	NewCnType: NConnectionType;
begin
	LastConnection := fCurrentConnection;
	if Assigned(fOwner) then
  begin
		NewCnType := OwnerConnectionType;
		if Assigned(fCurrentConnection) and (LastConnection <> fCurrentConnection) then
			ConnectionType := NewCnType;
		if ConnectionType = NewCnType then
			fUseOwnerConnectionType := True;
	end;
end;

//******************************************************************************
procedure TTSSQLFieldNameDelimiter.SetUseOwnerType(AValue: Boolean);
begin
	if fUseOwnerConnectionType = AValue then Exit;
	fUseOwnerConnectionType := AValue;
	if fUseOwnerConnectionType then
		CheckConnectionType;
end;

//******************************************************************************
procedure TTSSQLFieldNameDelimiter.SetConnectionType(AValue: NConnectionType);
begin
	inherited SetConnectionType(AValue);
	fUseOwnerConnectionType := False;
end;

//******************************************************************************
function TTSSQLFieldNameDelimiter.IsConnectionTypeStored: Boolean;
begin
	Result := not fUseOwnerConnectionType;
end;

{$ENDIF}




end.

