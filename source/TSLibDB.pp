unit TSLibDB;

{$mode objfpc}{$H+}

interface

uses
  TSLibConvert,
  DB,
	Classes, SysUtils;


type
	NSQLToken = (sqlNone, sqlSelect, sqlFrom, sqlWhere, sqlGroup, sqlHaving, sqlUnion, sqlOrder);
  SSQLTokens = set of NSQLToken;

	NExportType = (xportCSV, xportXML, xportHTML);
  NExportRows = (xlinesAll, xlinesNextN, xlinesFirstN, xlinesLastN, xlinesFilter);
	NExportEnocding = (xcodeANSI, xcodeUTF8, xcodeASCII, xcodeOEM850);

	MExportCallback = procedure (Field: TField; var ExportText: string);

  RParentheses = record
    Left: string;
    Right: string;
  end;

  RSQLClause = record
    ClauseType: NSQLToken;
    Starts: Integer;
    Ends: Integer;
  end;
  ASQLClauses = array [NSQLToken] of RSQLClause;

	RExportParameters = record
		ExportType: NExportType; //Basic export Type - CSV or XML

		RecordFilter: NExportRows;				//default: xlinesAll.
		FilterRowCount: Integer;					//used for xLines<First|Next|Last>N
		FilterText: string;								//only used if RecordFilter = xlinesFilter

		//if Length(ExportFields)>0 only valid columns from this list are exported.
		FieldNames: array of string;
		//For CSV export with csvHeaderLine=TRUE the ExportCaptions are used as headers
		//for XML export the ExportCaptions are used as XML field names.
		//if Length(ExportCaptions)=0, the TField's DisplayName is used.
		// if Length(ExportCaptions)=N>0 the array values are used for first N
		FieldCaptions: array of string;
		VisibleFieldsOnly: Boolean;				//If TRUE, only visible Fields are exported.
		FieldCallback: MExportCallback;		//enables export conversion per field.


		DecimalSep: char;									//Separator for exported float values
		FieldDelim: string;								//Delimiter for start field value and end field value marking
		FieldDelimReplace: string;				//Replacement for "FieldDelim"-Characters / strings in the field value
		DelimAll: Boolean;								//If TRUE, FieldDelim is used for all fields.
																			//	If FALSE, FieldDelim is only used for string fields

		ExportFileName: string;						//only used in file export procedures (ExportToFile)
		FileEncoding: NExportEnocding;		//only used in file export procedures (ExportToFile)

		CSVFieldSep: string;							//Field separator used in csv exports
		CSVRecordSep: string;							//Record separator used in csv exports
		CSVHeaderLine: Boolean;						//If TRUE, the first CSV line contains the field captions

		XMLHeaderProps: string;						//Beginning of the exported XML file
		XMLDatasetName: string;						//XML field used for the data set.
		XMLRecordName: string;						//XML field used for each record in the data set.
																			//	If empty the word "record" is used.
		HTMLHeader: string;
		HTMLFooter: string;
		HTMLRowStart: string;
		HTMLRowEnd: string;
		HTMLTitleFieldStart: string;
		HTMLTitleFieldEnd: string;
		HTMLFieldStart: string;
		HTMLFieldEnd: string;
	end;




//////////////////////////////////////////////////////////////////////////////////////////
// functions and procedures

function TriStateVal(Field: TField): NTriState;



/////////////////////////////////////////////////////
// Functions for SQL parsing and editing
function SimpleParseSQL(const SQL: string): ASQLClauses;
function SQLGetSingleClause(const SQL: string; ClauseType: NSQLToken; var StartPos: Integer; out EndPos: Integer):string;
function SQLSelectClause(const SQL: string; var StartPos: Integer; out EndPos: Integer):string;
function SQLFromClause(const SQL: string; var StartPos: Integer; out EndPos: Integer):string;
function SQLWhereClause(const SQL: string; var StartPos: Integer; out EndPos: Integer):string;
function SQLGroupByClause(const SQL: string; var StartPos: Integer; out EndPos: Integer):string;
function SQLHavingClause(const SQL: string; var StartPos: Integer; out EndPos: Integer):string;
function SQLOrderByClause(const SQL: string; var StartPos: Integer; out EndPos: Integer):string;

procedure SetSQLWhereClause(SQL: TStrings; WhereText: string;
                                    var StartPos: Integer; out EndPos: Integer);
procedure SetSQLOrderByClause(SQL, OrderFields: TStrings;
																		var StartPos: Integer; out EndPos: Integer); overload;
procedure SetSQLOrderByClause(SQL: TStrings; OrderFields: string;
																		var StartPos: Integer; out EndPos: Integer); overload;


/////////////////////////////////////////////////////
// Functions for Dataset-Export to several Formats

function ExportToString(DataSet: TDataSet; const Params: RExportParameters): string; overload;
function ExportToString(DataSet: TDataSet): string; overload;
procedure ExportToFile(DataSet: TDataSet; const FileName: string); overload;
procedure ExportToFile(DataSet: TDataSet; const Params: RExportParameters); overload;
procedure ExportToClipboard(DataSet: TDataSet); overload;
procedure ExportToClipboard(DataSet: TDataSet; const Params: RExportParameters); overload;




const
  /////////////////////////////////////////////////////
	//Constants used for/in SQL parser functions
  delimComment: array[1..2] of RParentheses = ((Left:'/*'; Right:'*/'),
  																						(Left:'--'; Right:#13));
  delimSubClause: RParentheses = (Left:'('; Right:')');
  //Each token string (array SQLTokens) must end with one of these Characters to be valid.
  tknTerminator: set of AnsiChar = [' ', #9, #13, #10, '"', '['];
  sqlTokens: array[NSQLToken] of string = (
  															''						//sqlNone
  															,'select'			//sqlSelect
  															, 'from'			//sqlFrom
                                , 'where'			//sqlWhere
                                , 'group by'	//sqlGroup
                                , 'having'		//sqlHaving
                                , 'union'			//sqlUnion
                                , 'order by'	//sqlOrder
                                );



  /////////////////////////////////////////////////////
	//Constants used for/in Dataset-Export functions.
	exportDefaults: RExportParameters = (
										ExportType: xportCSV;
										RecordFilter: xlinesAll;
										FilterRowCount: 0;
										FilterText: '';
										FieldNames: nil;
										FieldCaptions: nil;
										VisibleFieldsOnly: TRUE;
										FieldCallback: nil;
										DecimalSep: '.';
										FieldDelim: '';
										FieldDelimReplace: '';
										DelimAll: TRUE;
										ExportFileName: '';
										FileEncoding: xcodeUTF8;
										CSVFieldSep: ',';
										CSVRecordSep: #13#10;
										CSVHeaderLine: TRUE;
										XMLHeaderProps: 'version="1.0"';
										XMLDatasetName: '';
										XMLRecordName: '';
										HTMLHeader: '<html><head>'#13#10
										+'<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">'#13#10
										+'</head>'#13#10'<body>'#13#10
										+'<table>'#13#10;
										HTMLFooter: '</table>'#13#10'</body>'#13#10'</html>'#13#10;
										HTMLRowStart: #9'<tr>'#13#10;
										HTMLRowEnd: #9'</tr>'#13#10;
										HTMLTitleFieldStart: #9'<th>';
										HTMLTitleFieldEnd: '</th>'#13#10;
										HTMLFieldStart: #9#9'<td>';
										HTMLFieldEnd: '</td>'#13#10;
										);
	stringFields = [ftString, ftDate, ftDateTime, ftMemo,
									ftFmtMemo, ftWideString, ftFixedWideChar, ftWideMemo];
	floatFields = [ftFloat, ftCurrency, ftBCD];



implementation

uses
	Clipbrd,
	StrUtils,
	TSLib;

//---------------------------------------------------------------------------------------
function TriStateVal(Field: TField): NTriState;
begin
	try
		if Field.IsNull then
			Result := tsNULL
		else if Field is TBooleanField then
			Result := TriState(Field.AsBoolean)
		else if Field is TNumericField then
			Result := TriState(Field.AsInteger)
		else if Field is TStringField then
			Result := TriState(Field.AsString)
		else
			Result := tsNULL;
	except
		Result := tsNULL;
	end;
end;

//---------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------
//	SQL-Parser functions
//---------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------

//---------------------------------------------------------------------------------------
function SimpleParseSQL(const SQL: string): ASQLClauses;
var
  PS: PChar;
  L, P: Integer;
  PTkn, Tkn: NSQLToken;
  Cls: RSQLClause;
  Allowed: SSQLTokens;

  //......................................................................
  function IsDelim(const Delim: string; var CheckPos: PChar): Boolean;
  //Gibt TRUE zurück, wenn CheckPos auf den Begrenzer "Delim" zeigt.
  //Wenn Ja, zeigt CheckPos anschließend auf das erste Zeichen nach Ende des Begrenzers.
  var
    N, D: Integer;
    C: PChar;
  begin
    C := CheckPos;
    Result := False;
    N := Length(Delim);
    if N < 1 then														//Begrenzer ist leer - kann nie gefunden werden.
      Exit;
    D := 1;
  	while (D<=N) 														//Solange der Begrenzer nicht zuende ist
      and (C^ = Delim[D])										// und die Zeichen noch übereinstimmen
    do begin                                // setze alle Zeiger um eins hoch.
      if P+D-1>L then  											//Wenn Ende des Strings erreicht brich ab.
        Exit;
      inc(D);
      inc(C);
    end;
    if D > N then                           //Letzter Zeichenvergleich war Erfolgreich.
    begin
      CheckPos := C;
      P := P + N;
	    Result := P<=L;
    end;
  end;

  //......................................................................
  function CheckIgnorables(Delims: RParentheses; CheckPos: PChar): Boolean;
  begin
    Result := False;
    if IsDelim(Delims.Left, CheckPos) then
    begin
      if P > L then
      	Exit;

      while (P<=L)
      	and not IsDelim(Delims.Right, CheckPos)
      do begin
      	inc(CheckPos);
        inc(P);
      end;
      PS := CheckPos;
    end;
		Result := P<=L;
  end;

  //......................................................................
  function FindClause(Allowed: SSQLTokens): RSQLClause;
  var
    StartPos: Integer;
    Tkn: NSQLToken;
    I: Integer;
  begin
    Result.ClauseType := sqlNone;
    while P <= L do
    begin
      for I := Low(delimComment) to High(delimComment) do //Alle Kommentarbegrenzer prüfen
        if not CheckIgnorables(delimComment[I], PS) then
        	Exit;
      if not CheckIgnorables(delimSubClause, PS) then
      	Exit;

      StartPos := P;
      for Tkn := Low(NSQLToken) to High(NSQLToken) do
      begin
			  if (Tkn in Allowed)
      	  and IsDelim(sqlTokens[Tkn], PS)
      	  and (AnsiChar(PS^) in tknTerminator)
        then begin
          Result.ClauseType := Tkn;
          Result.Starts := StartPos;
          Exit;
        end;
      end;
      inc(PS);
      inc(P);
    end;
  end;

begin
  for Tkn := Low(sqlTokens) to High(sqlTokens) do
  begin
    Result[Tkn].ClauseType := Tkn;
    Result[Tkn].Starts := 0;
    Result[Tkn].Ends := 0;
  end;
  L := Length(SQL);
  PS := PChar(SQL);
  P := 1;
  Tkn := sqlNone;
  while P <= L do
  begin
    //eine "Select"-Klausel muss immer vorhanden sein.
    Cls := FindClause([sqlSelect]);
    //Nachfolgend darf eine der folgenden Klauseln vorhanden sein
    Allowed := [sqlFrom, sqlWhere, sqlGroup, sqlUnion, sqlOrder];

    //Solange die Suche eine gültige Klausel gefunden hat:
    while Cls.ClauseType <> sqlNone do
    begin
      Tkn := Cls.ClauseType;
      //Setze Typ und Startpunkt im entsrpecheenden Eintrag des Ergebins-Arrays.
      Result[Cls.ClauseType].ClauseType := Cls.ClauseType;
      Result[Cls.ClauseType].Starts := Cls.Starts;
      //Suche nach der ersten zulässigen folgenden Klausel
      Cls := FindClause(Allowed);
      //Der Startpunkt der folgenden Klausel ist der Endpunkt der zuletzt gefundenen.
      Result[Tkn].Ends := Cls.Starts-1;
      //Setze den Startpunkt für alle Tokens, die noch nicht gefunden wurden
      //und die vor dem gefundenen eingefügt werden müssten.
      if Cls.ClauseType <> sqlNone then
        for PTkn := succ(sqlNone) to pred(Cls.ClauseType) do
          if Result[PTkn].Starts = 0 then
          begin
            Result[PTkn].Starts := Cls.Starts;
            Result[PTkn].Ends := Cls.Starts-1;
          end;
      //Für die nächste Suche lege fest, welche Klauseln nach der zuletzt
      //  gefundenen noch stehen dürfen.
      case Cls.ClauseType of
        sqlFrom:
          Allowed := [sqlWhere, sqlGroup, sqlUnion, sqlOrder];
        sqlWhere:
          Allowed := [sqlGroup, sqlUnion, sqlOrder];
        sqlGroup:
          Allowed := [sqlHaving, sqlUnion, sqlOrder];
        sqlUnion:
          Allowed := [sqlOrder];
        //order by muss immer die letzte Klausel sein. Setze die Abbruchbedingung.
        sqlOrder:
          P := L + 1;
      end;
    end;
  end;
  //Der Endpunkt der letzten gefundenen Klausel ist das Ende des SQL-Strings.
  if Tkn <> sqlNone then
		Result[Tkn].Ends := L;
end;

//---------------------------------------------------------------------------------------
function SQLGetSingleClause(const SQL: string; ClauseType: NSQLToken;
  													var StartPos: Integer; out EndPos: Integer): string;
var
	Cls: ASQLClauses;
begin
  if (StartPos <= 0) or (StartPos > Length(SQL)) then
  	StartPos := 1;
  if StartPos > 1 then
	  Cls := SimpleParseSQL(copy(SQL, StartPos, MaxInt))
  else
	  Cls := SimpleParseSQL(SQL);
  if Cls[ClauseType].ClauseType = ClauseType then
  begin
    StartPos := Cls[ClauseType].Starts + StartPos - 1;
    EndPos := Cls[ClauseType].Ends;
    Result := Copy(SQL, StartPos, EndPos - Cls[ClauseType].Starts + 1);
  end
  else begin
    StartPos := 0;
    EndPos := 0;
    Result := '';
  end;
end;

//---------------------------------------------------------------------------------------
function SQLSelectClause(const SQL: string; var StartPos: Integer; out EndPos: Integer): string;
begin
	Result := SQLGetSingleClause(SQL, sqlSelect, StartPos, EndPos);
end;

//---------------------------------------------------------------------------------------
function SQLFromClause(const SQL: string; var StartPos: Integer; out EndPos: Integer): string;
begin
	Result := SQLGetSingleClause(SQL, sqlFrom, StartPos, EndPos);
end;

//---------------------------------------------------------------------------------------
function SQLWhereClause(const SQL: string; var StartPos: Integer; out EndPos: Integer): string;
begin
	Result := SQLGetSingleClause(SQL, sqlWhere, StartPos, EndPos);
end;

//---------------------------------------------------------------------------------------
function SQLGroupByClause(const SQL: string; var StartPos: Integer; out EndPos: Integer): string;
begin
	Result := SQLGetSingleClause(SQL, sqlGroup, StartPos, EndPos);
end;

//---------------------------------------------------------------------------------------
function SQLHavingClause(const SQL: string; var StartPos: Integer; out EndPos: Integer): string;
begin
	Result := SQLGetSingleClause(SQL, sqlHaving, StartPos, EndPos);
end;

//---------------------------------------------------------------------------------------
function SQLOrderByClause(const SQL: string; var StartPos: Integer; out EndPos: Integer): string;
begin
	Result := SQLGetSingleClause(SQL, sqlOrder, StartPos, EndPos);
end;

//---------------------------------------------------------------------------------------
procedure SetSQLWhereClause(SQL: TStrings; WhereText: string;
                                    var StartPos: Integer; out EndPos: Integer);
var
  SQLText: string;
begin
  SQLText := SQL.Text;
  SQLWhereClause(SQLText, StartPos, EndPos);

  if WhereText > '' then
  begin
    if (WhereText > '') and (PosInText('where', TrimLeft(WhereText)) <> 1) then
      if EndPos < StartPos then
        WhereText := 'where ' + WhereText + #13#10
      else
        WhereText := 'where ' + WhereText;

    if StartPos > 0 then
    begin
      SQL.Text := Copy(SQLText, 1, StartPos-1)
                  + WhereText
                  + Copy(SQLText, EndPos+1, MaxInt);
      EndPos := StartPos + Length(WhereText);
    end
    else begin
    	SQL.Add(WhereText);
      EndPos := Length(SQL.Text) + Length(WhereText);
    end;

  end
  else if StartPos > 0 then
  begin
		SQL.Text := Copy(SQLText, 1, StartPos-1)
              + Copy(SQLText, EndPos+1, MaxInt);
    EndPos := StartPos;
  end;
end;


//---------------------------------------------------------------------------------------
procedure SetSQLOrderByClause(SQL, OrderFields: TStrings; var StartPos: Integer; out EndPos: Integer);
var
	I: Integer;
	OrderTxt: string;
begin
	if Assigned(OrderFields) and (OrderFields.Count > 0) then
	begin
		OrderTxt := sqlTokens[sqlOrder] + ' ' + OrderFields[0];
		for I := 1 to pred(OrderFields.Count) do
			OrderTxt := OrderTxt + ',' + OrderFields[I];
	end
	else
		OrderTxt := '';
	SetSQLOrderByClause(SQL, OrderTxt, StartPos, EndPos);
end;


//---------------------------------------------------------------------------------------
procedure SetSQLOrderByClause(SQL: TStrings; OrderFields: string;
																		var StartPos: Integer; out EndPos: Integer); overload;
var
	I: Integer;
	SQLText: string;
begin
	SQLText := SQL.Text;
	SQLOrderByClause(SQLText, StartPos, EndPos);
	if OrderFields > '' then
	begin
		if not StartsText(sqlTokens[sqlOrder] + ' ', TrimLeft(SQLText)) then
			OrderFields := sqlTokens[sqlOrder] + ' ' + OrderFields;
		if StartPos > 0 then
		begin
			SQL.Text := Copy(SQLText, 1, StartPos-1)
								+ OrderFields
								+ Copy(SQLText, EndPos+1, MaxInt);
			EndPos := StartPos + Length(OrderFields);
		end
		else begin
			SQL.Add(OrderFields);
			EndPos := Length(SQL.Text) + Length(OrderFields);
		end;
  end
  else if StartPos > 0 then
  begin
		SQL.Text := Copy(SQLText, 1, StartPos-1)
              + Copy(SQLText, EndPos+1, MaxInt);
    EndPos := StartPos;
  end;
end;


//---------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------
//  Dataset-Export Functions
//---------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------


//---------------------------------------------------------------------------------------
{Main Function for all ExportTo(x) functions/procedures. This one evaluates all
	parameters in the Params record and builds the string in either CSV or XML-Format.}
function ExportToString(DataSet: TDataSet; const Params: RExportParameters): string; overload;
var
	S, DSName, RecName, Rec, Val, FDelim, FReplace, DSFilter: string;
	DSIsFiltered: Boolean;
	DecSep: char;
	I, L, FldCtr, RecCtr: Integer;
	Fld: TField;
	Flds: array of TField;
	Captions: array of string;

begin
	Result := '';
	RecCtr := -1;

	//1. Filterung der Datensätze vorbereiten/aktivieren
	case Params.RecordFilter of
		xlinesNextN:
			RecCtr := Params.FilterRowCount;
		xlinesFirstN: begin
			RecCtr := Params.FilterRowCount;
			DataSet.First;
		end;
		xlinesLastN: begin
			DataSet.Last;
			DataSet.MoveBy(-Params.FilterRowCount+1);
		end;
		xlinesFilter: begin
			DSFilter := DataSet.Filter;
			DSIsFiltered := DataSet.Filtered;
			DataSet.Filter := Params.FilterText;
			DataSet.Filtered := True;
			DataSet.First;
		end;
    else //xlinesAll:
      DataSet.First;
	end;

	//2. Bilde eine Liste der Export-Spalten / -Felder.
	DecSep := FormatSettings.DecimalSeparator;
	FormatSettings.DecimalSeparator := Params.DecimalSep;
	DSName := IIf(Params.XMLDatasetName > '', Params.XMLDatasetName, 'dataset');
	RecName := IIf(Params.XMLRecordName > '', Params.XMLRecordName, 'record');
	DataSet.DisableControls;
	try
		//"Filterung" der Spalten prüfen - wenn eine Liste von Feldnamen übergeben wurde, verwende nur diese Felder.
		FldCtr := 0;
		L := Length(Params.FieldCaptions);
		SetLength(Flds, DataSet.FieldCount);
		SetLength(Captions, DataSet.FieldCount);

		if Length(Params.FieldNames) > 0 then
			for I := Low(Params.FieldNames) to High(Params.FieldNames) do
			begin
				Fld := DataSet.FindField(Params.FieldNames[I]);
				if Assigned(Fld) and (Fld.Visible or not Params.VisibleFieldsOnly) then
				begin
					Flds[FldCtr] := Fld;
					if I < L then
						Captions[FldCtr] := Params.FieldCaptions[I];
					Inc(FldCtr);
				end;
			end
		else //Feldliste ist leer. Übernimm die Standardfelder
			for I := 0 to pred(DataSet.FieldCount) do
			begin
				if DataSet.Fields[I].Visible or not Params.VisibleFieldsOnly then
				begin
					Flds[FldCtr] := DataSet.Fields[I];
					if I < L then
						Captions[FldCtr] := Params.FieldCaptions[I]
					else
						Captions[FldCtr] := DataSet.Fields[I].DisplayName;
					Inc(FldCtr);
				end;
			end;
		SetLength(Flds, FldCtr);
		SetLength(Captions, FldCtr);

		//3. Erzeuge den Kopfbereich
		case Params.ExportType of
			//......... XML .........
			xportXML: begin
				Result := '<?xml ' + Params.XMLHeaderProps
//									+ ' encoding="'+ CodePageName(Encode.CodePage) +'" ?>'#13#10
								+ '<' + DSName + '>'#13#10;
			end;
			//......... CSV .........
			xportCSV: begin
				if Params.CSVHeaderLine then
				begin
					for I := Low(Captions) to High(Captions) do
					begin
            S := ReplaceStr(Captions[I], Params.FieldDelim, Params.FieldDelimReplace);
					end;
					Result := Result + Params.CSVRecordSep;
				end;
			end;
			xportHTML: begin
				Result := Params.HTMLHeader + Params.HTMLRowStart;
				for I := Low(Captions) to High(Captions) do
				begin
          S := ReplaceStr(Captions[I], Params.FieldDelim, Params.FieldDelimReplace);
					Result := Result
									+ Params.HTMLTitleFieldStart
									+ Params.FieldDelim
									+ HTMLEncode(S)
									+ Params.FieldDelim
									+ Params.HTMLTitleFieldEnd;
				end;
				Result := Result + Params.HTMLRowEnd;
			end;
		end;

		//4. Exportiere Datensätze
		while (RecCtr <> 0) and not DataSet.Eof do
		begin
			Rec := '';
			for I := Low(Flds) to High(Flds) do
			begin
				Val := Flds[I].AsString;
				if Assigned(Params.FieldCallback) then
					Params.FieldCallback(Flds[I], Val);
				if Params.DelimAll or (Flds[I].DataType in stringFields) then
				begin
					Val := Params.FieldDelim
								+ ReplaceStr(Val, Params.FieldDelim, Params.FieldDelimReplace)
								+ Params.FieldDelim;
				end;

				case Params.ExportType of
					xportXML:				//......... XML .........
						Rec := Rec + #9#9'<' + Captions[I] + '>'
											+ Val + '</' + Captions[I] + '>'#13#10;
					xportCSV:				//......... CSV .........
						if I = High(Flds) then
							Rec := Rec + Val
						else
							Rec := Rec + Val + Params.CSVFieldSep;
					xportHTML:
						Rec :=  Rec + Params.HTMLFieldStart + HTMLEncode(Val) + Params.HTMLFieldEnd;
				end;
			end;

			case Params.ExportType of
				xportXML:
					Result := Result + #9'<' + RecName + '>'#13#10
										+ Rec + #9'</' + RecName + '>'#13#10;
				xportCSV:
					Result := Result + Rec + Params.CSVRecordSep;
				xportHTML:
					Result := Result
									+ Params.HTMLRowStart
									+ Rec
									+ Params.HTMLRowEnd;
			end;
			dec(RecCtr);
			DataSet.Next;
		end;

		case Params.ExportType of
			xportXML:
			Result :=  Result + '</' + DSName + '>'#13#10;
			xportHTML:
				Result := Result + Params.HTMLFooter;
		end;
	finally
		if DSIsFiltered <> DataSet.Filtered then
			DataSet.Filtered := DSIsFiltered;
		if DSFilter <> DataSet.Filter then
			DataSet.Filter := DSFilter;
		FormatSettings.DecimalSeparator := DecSep;
		DataSet.EnableControls;
	end;
end;

//---------------------------------------------------------------------------------------
function ExportToString(DataSet: TDataSet): string; overload;
begin
	Result := ExportToString(DataSet, exportDefaults);
end;

//---------------------------------------------------------------------------------------
procedure ExportToFile(DataSet: TDataSet; const Params: RExportParameters); overload;
var
	SL: TStringList;
begin
	SL := TStringList.Create;
	try
		SL.Text := ExportToString(DataSet, Params);
		SL.SaveToFile(Params.ExportFileName);
	finally
		SL.Free;
	end;
end;

//---------------------------------------------------------------------------------------
procedure ExportToFile(DataSet: TDataSet; const FileName: string); overload;
var
	Params: RExportParameters;
begin
	Params := exportDefaults;
	Params.ExportFileName := FileName;
	ExportToFile(DataSet, Params);
end;

//---------------------------------------------------------------------------------------
procedure ExportToClipboard(DataSet: TDataSet; const Params: RExportParameters); overload;
var
	S: string;
begin
	S := ExportToString(DataSet, Params);
	Clipboard.AsText := S;
	case Params.ExportType of
		xportHTML: begin
			CopyHTMLToClipBoard(S);
		end;
	end;
end;

//---------------------------------------------------------------------------------------
procedure ExportToClipboard(DataSet: TDataSet); overload;
begin
	ExportToClipboard(DataSet, exportDefaults);
end;
end.

