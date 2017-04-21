unit TSLibMSOffice;

{$mode objfpc}{$H+}

interface

uses
	ComObj,
  DB,
	TSLibDB,
	Classes, SysUtils;


procedure ExportToExcel(DataSet: TDataSet); overload;
procedure ExportToExcel(DataSet: TDataSet; Params: RExportParameters); overload;


implementation

uses
	variants,
	Clipbrd;

//---------------------------------------------------------------------------------------
procedure ExportToExcel(DataSet: TDataSet); overload;
begin
	ExportToExcel(DataSet, exportDefaults);
end;

//---------------------------------------------------------------------------------------
procedure ExportToExcel(DataSet: TDataSet; Params: RExportParameters); overload;
var
	Excel: OLEVariant;
	VisCount, XLID, I, N: Integer;
	S, DcSep, ThSep: string;

	Flds: OLEVariant;
	WBK: OLEVariant; //_Workbook;
	WS: OLEVariant; //_Worksheet;
	Rg: OLEVariant; //ExcelRange;

begin
	VisCount := 0;
	for I := 0 to pred(DataSet.FieldCount) do
		if DataSet.Fields[I].Visible then
			inc(VisCount);

	//First Export to Clipboard as CSV-Text
	Params.VisibleFieldsOnly := True;
	Params.ExportType := xportCSV;
	Params.CSVFieldSep := #9;
	Params.CSVRecordSep := #13#10;
	Params.CSVHeaderLine := True;
	Params.FieldDelim := '';
	Params.DecimalSep := ',';
	S := ExportToString(DataSet, Params);
	Clipboard.AsText := S;

	//Now create an VarArray to hold the field definitions for Excel's TextToColumns function
	Flds := VarArrayCreate([0, pred(VisCount)], varVariant);
	N := 0;
	for I := 0 to pred(DataSet.FieldCount) do
		if DataSet.Fields[I].Visible then
		begin
			Flds[N] := VarArrayOf([N+1, 1]);
			inc(N);
		end;

	//Start/connect Excel, and write the clipboard data into a new worksheet
	//XLID := GetUserDefaultLCID;
	Excel := CreateOleObject('Excel.Application');
	try
		try
			Excel.Visible := True;
			Excel.Application.SheetsInNewWorkbook := 1;
			WBK := Excel.Workbooks.Add(EmptyParam);
			WS := WBk.Worksheets.Item[1]; // as _Worksheet;
			WS.Paste(EmptyParam, EmptyParam);
			Rg := Excel.Selection;// as ExcelRange;
			N := Rg.Columns.Count;
			if N = 1 then
			begin
				DcSep := ',';
				ThSep := '.';
				Rg.TextToColumns(EmptyParam, 		//Destination: OleVariant;
													$00000001, 		//xlDelimited,	//DataType: XlTextParsingType;
													$FFFFEFD2,		//xlNone,				//TextQualifier: XlTextQualifier;
													False,				//ConsecutiveDelimiter: OleVariant;
													True,					//Tab: OleVariant;
													False,				//Semicolon: OleVariant;
													False,				//Comma: OleVariant;
													False,				//Space: OleVariant;
													False,				//Other: OleVariant;
													'',						//OtherChar: OleVariant;
													Flds,					//FieldInfo: OleVariant;
													DcSep,					//DecimalSeparator: OleVariant;
													ThSep,					//ThousandsSeparator: OleVariant;
													False					//TrailingMinusNumbers: OleVariant
													);
			end;
			Rg.Columns.AutoFit;
			Excel.UserControl := True;
		except
		end;
	finally
		Excel := Unassigned;
	end;
end;






end.

