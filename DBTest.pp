unit DBTest;

{$mode objfpc}{$H+}

interface

uses
	TS_DBGrid, TS_DBStateLabel, Classes, SysUtils, sqldb, IBConnection, FBAdmin,
	sqlite3conn, mysql40conn, mysql56conn, odbcconn, oracleconnection,
	pqconnection, db, mssqlconn, FileUtil, Forms, Controls, Graphics, Dialogs,
	DbCtrls, DBGrids, Buttons, StdCtrls, Grids, ExtCtrls, TS_Panel, TS_FileINI;

type

	{ TForm1 }

 TForm1 = class(TForm)
		Button1: TButton;
		QTestAktion: TBooleanField;
		QTestAktionDauer: TSmallintField;
		QTestAktionEnde: TDateTimeField;
		QTestAktionFrequenz: TSmallintField;
		QTestAktionStart: TDateTimeField;
		QTestAnzHauptArt: TSmallintField;
		QTestArtikelArt: TStringField;
		QTestArtNr: TLongintField;
		QTestAuslaufArt: TBooleanField;
		QTestAutoAktion: TBooleanField;
		QTestBasisGrp: TWordField;
		QTestBearbeiter: TStringField;
		QTestBestBest: TLongintField;
		QTestBezeichnung: TStringField;
		QTestBonBez: TStringField;
		QTestEANGewicht: TBooleanField;
		QTestEANPreis: TBooleanField;
		QTestGeaendert: TDateTimeField;
		QTestGebindeArtNr: TLongintField;
		QTestHatGebinde: TBooleanField;
		QTestHauptGrp: TSmallintField;
		QTestInhalt: TStringField;
		QTestIstGebinde: TBooleanField;
		QTestKassenArt: TBooleanField;
		QTestKurzNr: TLongintField;
		QTestLast2Anz: TLongintField;
		QTestLast2Dat: TDateTimeField;
		QTestLast2EK: TFMTBCDField;
		QTestLastAnz: TLongintField;
		QTestLastDat: TDateTimeField;
		QTestLastEK: TFMTBCDField;
		QTestLetzteInventur: TDateTimeField;
		QTestMainArtNr: TLongintField;
		QTestMarke: TBooleanField;
		QTestMaszEinh: TStringField;
		QTestMeldBest: TLongintField;
		QTestMinBest: TLongintField;
		QTestMischEK: TFMTBCDField;
		QTestMwSt: TWordField;
		QTestPackArt: TStringField;
		QTestPalette: TLongintField;
		QTestPfand: TBooleanField;
		QTestPfandArtNr: TLongintField;
		QTestPreisAendern: TBooleanField;
		QTestRabatt: TBooleanField;
		QTestSollBest: TLongintField;
		QTestStart: TDateTimeField;
		QTestSuchText: TStringField;
		QTestUGrp1: TSmallintField;
		QTestUGrp2: TWordField;
		QTestVK1: TFMTBCDField;
		QTestVK1Aktion: TFMTBCDField;
		QTestVK2: TFMTBCDField;
		QTestVK2Aktion: TFMTBCDField;
		QTestVK3: TFMTBCDField;
		QTestVK3Aktion: TFMTBCDField;
		QTestVKBus: TFMTBCDField;
		QTestWaagenArt: TBooleanField;
		QTestZugaenge: TLongintField;
		TSDBGrid2: TTSDBGrid;
		XInactiveState: TCheckBox;
		DBNavigator1: TDBNavigator;
		BtnActive: TSpeedButton;
		SpeedButton1: TSpeedButton;
		SrcTest: TDataSource;
		CnMS: TMSSQLConnection;
		QTest: TSQLQuery;
		TrMS: TSQLTransaction;
		P: TTSPanel;
		procedure BtnActiveClick(Sender: TObject);
		procedure Button1Click(Sender: TObject);
		procedure SpeedButton1Click(Sender: TObject);
		procedure XInactiveStateChange(Sender: TObject);
	private
		L: TTSDBStateLabel;
		G: TTSDBGrid;
	public

	end;

var
	Form1: TForm1;

implementation

uses
	TSLibMSOffice,
	TSLibConvert;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnActiveClick(Sender: TObject);
begin
  QTest.Active := BtnActive.Down;
  QTest.IndexName :=  '';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TSDBGrid2.AutoSizeColumns;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
	ExportToExcel(QTest);
end;

procedure TForm1.XInactiveStateChange(Sender: TObject);
begin
  if Assigned(L) then
  	L.InactiveState := TriState(XInactiveState.State);
end;

end.

