unit TS_DBGridOptionsForm;

{$mode objfpc}{$H+}

interface

uses
 	TS_DBGrid,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, Buttons, TS_Panel;

type

  { TDBGridOptionsForm }

  TDBGridOptionsForm = class(TForm)
    BtnShowAll: TBitBtn;
    BtnDefWidth: TBitBtn;
    BtnClearFilters: TBitBtn;
    BtnClearSort: TBitBtn;
    BtnClose: TBitBtn;
    BtnDefaultCols: TBitBtn;
    GrdColumns: TStringGrid;
    ImageList1: TImageList;
    PnlMsg: TPanel;
    PnlButtons: TTSPanel;
    BtnFilterActive: TSpeedButton;
    procedure BtnClearFiltersClick(Sender: TObject);
    procedure BtnClearSortClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnDefaultColsClick(Sender: TObject);
    procedure BtnDefWidthClick(Sender: TObject);
    procedure BtnFilterActiveClick(Sender: TObject);
    procedure BtnShowAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure GrdColumnsColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure GrdColumnsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure GrdColumnsEditButtonClick(Sender: TObject);
    procedure GrdColumnsEditingDone(Sender: TObject);
    procedure GrdColumnsHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure GrdColumnsSetCheckboxState(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckboxState);
    procedure GrdColumnsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure GrdColumnsShowHint(Sender: TObject; HintInfo: PHintInfo);
  private
    ColName: Integer;
    ColVisible: Integer;
    ColFilter: Integer;
    ColFilterActive: Integer;
    ColSort: Integer;
    ColWidth: Integer;
    Grid: TTSDBGrid;
    procedure InitGrid;
    procedure SetFilterState(FilterActive: Boolean);
  public
    procedure EditGridSettings(ForGrid: TTSDBGrid);
  end; 

var
  DBGridOptionsForm: TDBGridOptionsForm;

implementation

{$R *.lfm}

uses
  DBGrids;

{ TDBGridOptionsForm }

procedure TDBGridOptionsForm.FormCreate(Sender: TObject);
begin
	ColName := 1;
  ColVisible := 2;
  ColFilter := 3;
  ColFilterActive := 4;
  ColWidth := 5;
  ColSort := 6;
  GrdColumns.RowHeights[0] := 25;
end;

procedure TDBGridOptionsForm.BtnDefaultColsClick(Sender: TObject);
begin
  if Grid.Columns.DesignColumns then
	  Grid.Columns.ResetColumnsOrder(coDesignOrder)
  else
	  Grid.Columns.ResetColumnsOrder(coFieldIndexOrder);
  InitGrid;
end;

procedure TDBGridOptionsForm.BtnClearFiltersClick(Sender: TObject);
var
  I: Integer;
begin
  Grid.Filter.Clear;
  for I := 1 to pred(GrdColumns.RowCount) do
	  GrdColumns.Cols[ColFilter][I] := Grid.Columns[I-1].Filter.DisplayText;
end;

procedure TDBGridOptionsForm.BtnClearSortClick(Sender: TObject);
begin
	Grid.Sorting.FieldNames.Clear;
  InitGrid;
end;

procedure TDBGridOptionsForm.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDBGridOptionsForm.BtnDefWidthClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to pred(Grid.Columns.Count) do
  	Grid.Columns[I].Width := Grid.Columns[I].DefaultWidth;
  InitGrid;
end;

procedure TDBGridOptionsForm.BtnFilterActiveClick(Sender: TObject);
begin
  SetFilterState(BtnFilterActive.Down);
end;

procedure TDBGridOptionsForm.BtnShowAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to pred(Grid.Columns.Count) do
  	Grid.Columns[I].Visible := True;
  InitGrid;
end;

procedure TDBGridOptionsForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
  	#13, #27: Close;
  end;
end;

procedure TDBGridOptionsForm.GrdColumnsColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn then
  	Grid.Columns[SIndex-1].Index := tIndex - 1;
end;

procedure TDBGridOptionsForm.GrdColumnsDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  P, th, tw, cx, cy: Integer;
  S: string;
  SO: NTSGridSortOrder;
  BC: TColor;
  BS: TBrushStyle;
begin
  if (aCol=ColSort) and (aRow>=GrdColumns.FixedRows) then
  begin
    SO := Grid.Columns[aRow-1].SortingOrder;
    if SO = gsoNoSort then
    	Exit;
    P := Grid.Columns[aRow-1].SortingPosition;
    S := IntToStr(P);
		th := GrdColumns.Canvas.TextHeight(S) div 2;
    tw := GrdColumns.Canvas.TextWidth(S);
    cx := (aRect.Left + aRect.Right - tw) div 2;
    cy := (aRect.Top + aRect.Bottom - tw) div 2;
    BC := GrdColumns.Canvas.Brush.Color;
		BS := GrdColumns.Canvas.Brush.Style;
    if SO = gsoDescending then
    begin
			GrdColumns.Canvas.Brush.Color := clGreen;
			GrdColumns.Canvas.Pen.Color := clGreen;
	    GrdColumns.Canvas.Polygon([Point(cx-3, cy+5)
		  													, Point(cx+3, cy+5)
  															, Point(cx, cy-5)
											    			]);
			GrdColumns.Canvas.Brush.Style := bsClear;
			GrdColumns.Canvas.TextOut(cx+5, cy-th, S);
    end
    else begin
			GrdColumns.Canvas.Brush.Color := clYellow;
			GrdColumns.Canvas.Pen.Color := clYellow;
	    GrdColumns.Canvas.Polygon([Point(cx-3, cy-5)
  	  													, Point(cx+3, cy-5)
    														, Point(cx, cy+5)
											    			]);
			GrdColumns.Canvas.Brush.Style := bsClear;
			GrdColumns.Canvas.Brush.Color := BC;
			GrdColumns.Canvas.TextOut(cx+5, cy-th, S);
    end;
		GrdColumns.Canvas.Brush.Style := BS;
		GrdColumns.Canvas.Brush.Color := BC;
  end;
end;

procedure TDBGridOptionsForm.GrdColumnsEditButtonClick(Sender: TObject);
var
  C, R: Integer;
begin
	C := GrdColumns.Col;
  R := GrdColumns.Row;
  if (C=ColSort) and (R>=GrdColumns.FixedRows) then
  begin
    dec(R);
    case Grid.Columns[R].SortingOrder of
      gsoNoSort:
      	Grid.Columns[R].SortingOrder := gsoAscending;
      gsoAscending:
      	Grid.Columns[R].SortingOrder := gsoDescending;
      gsoDescending:
      	Grid.Columns[R].SortingOrder := gsoNoSort;
    end;
  end;
  GrdColumns.Invalidate;
end;

procedure TDBGridOptionsForm.GrdColumnsSetCheckboxState(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckboxState);
var
  Checked: Boolean;
begin
  if ACol = ColVisible then
  begin
    Checked := Value = cbChecked;
    Grid.Columns[ARow-1].Visible := Checked;
    GrdColumns.Cols[ACol][ARow] := BoolToStr(Checked, '1', '0');
  end
  else if ACol = ColFilterActive then
  begin
    Checked := Value = cbChecked;
    Grid.Columns[ARow-1].Filter.Enabled := Checked;
    GrdColumns.Cols[ACol][ARow] := BoolToStr(Checked, '1', '0');
  end;
end;

procedure TDBGridOptionsForm.GrdColumnsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  W: Integer;
begin
  if ACol=ColWidth then
  begin
    try
      W := StrToInt(Value);
      //W := StrToInt(GrdColumns.Cols[ColWidth][ARow]);
    except
      W := Grid.Columns[ARow-1].Width;
    end;
		GrdColumns.Cells[ACol,ARow] := IntToStr(W);
    Grid.Columns[ARow-1].Width := W;
  end
  else if (ACol=ColFilter) and Grid.Filter.SuccessiveFiltering then
    Grid.Columns[ARow-1].Filter.FilterText := Value;
end;

procedure TDBGridOptionsForm.GrdColumnsShowHint(Sender: TObject;
  HintInfo: PHintInfo);
//var
//  Pt: TPoint;
//  C: Integer;
begin
  //Pt := GrdColumns.MouseToCell(HintInfo^.CursorPos);
  //inc(Pt.X);
  //if Pt.Y < GrdColumns.FixedRows then
  //begin
		//GrdColumns.ShowHint := True;
  //  if Pt.X = 1 then
  //    GrdColumns.Hint := 'Index. Click title to restore original column positions.'
  //  else if Pt.X = ColName then
	 //   GrdColumns.Hint := 'Title. Click title to save current column positions.'
  //  else if Pt.X = ColVisible then
	 //   GrdColumns.Hint := 'Visibility. Click title to toggle visibility for all columns.'
  //  else if Pt.X = ColFilter then
	 //   GrdColumns.Hint := 'Filter expression. Click title to clear all filters.'
  //  else if Pt.X = ColFilterActive then
	 //   GrdColumns.Hint := 'Filter active state. Click title toggle active state for all columns.'
  //  else if Pt.X = ColWidth then
	 //   GrdColumns.Hint := 'Column Width. Click title to reset all widths to default.'
  //  else if Pt.X = ColSort then
	 //   GrdColumns.Hint := 'Sorting Position. Click title to clear all sorting.'
  //  else
		//	GrdColumns.ShowHint := False;
  //end;
end;

procedure TDBGridOptionsForm.GrdColumnsEditingDone(Sender: TObject);
var
  R, C: Integer;
  Col: TTSColumn;
begin
  R := GrdColumns.Row;
  C := GrdColumns.Col;
  if C = ColFilter then
  begin
    Col := Grid.Columns[R-1];
    if not Grid.Filter.SuccessiveFiltering then
  	  Col.Filter.FilterText := GrdColumns.Cells[C, R];
	  GrdColumns.Cells[C, R] := Col.Filter.DisplayText;
  end;
end;

procedure TDBGridOptionsForm.GrdColumnsHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var
  I: Integer;
  B: Boolean;
begin
  if IsColumn then
  begin
    if Index = 0 then // Sort by original Index-Position
    begin
      if Grid.Columns.DesignColumns then
	      Grid.Columns.ResetColumnsOrder(coDesignOrder)
      else
	      Grid.Columns.ResetColumnsOrder(coFieldIndexOrder);
      InitGrid;
    end
  	else if Index = ColName then 	//Save column settings
    begin
      PnlMsg.Show;
      try
        PnlMsg.Caption := 'saving settings...';
        Application.ProcessMessages;
        Grid.SaveSettings;
        PnlMsg.Caption := 'settings saved.';
        Application.ProcessMessages;
        Sleep(500);
      finally
        PnlMsg.Hide;
      end;
    end
		else if Index = ColVisible then	//toggle visibility
    begin
      B := GrdColumns.Cells[ColVisible, 1]<>'1';
      Grid.Columns.BeginUpdate;
      try
		    for I := 1 to pred(GrdColumns.RowCount) do
        begin
    	    GrdColumns.Cells[ColVisible, I] := BoolToStr(B, '1', '0');
          Grid.Columns[I-1].Visible := B;
        end;
      finally
        Grid.Columns.EndUpdate;
      end;
    end
    else if Index = ColFilter then //clear all filters
		begin
      Grid.Filter.Clear;
      for I := 1 to pred(GrdColumns.RowCount) do
      	GrdColumns.Cols[ColFilter][I] := Grid.Columns[I-1].Filter.DisplayText;
    end
    else if Index = ColFilterActive then //toggle filter state
		begin
      SetFilterState(not Grid.Filter.Enabled)
      //B := GrdColumns.Cells[ColFilterActive, 1]<>'1';
      //Grid.Columns.BeginUpdate;
      //try
		    //for I := 1 to pred(GrdColumns.RowCount) do
      //  begin
    	 //   GrdColumns.Cells[ColFilterActive, I] := BoolToStr(B, '1', '0');
      //    Grid.Columns[I-1].Filter.Enabled := B;
      //  end;
      //finally
      //  Grid.Columns.EndUpdate;
      //end;
    end
    else if Index = ColWidth then
		begin
      for I := 0 to pred(Grid.Columns.Count) do
        Grid.Columns[I].Width := Grid.Columns[I].DefaultWidth;
      InitGrid;
    end
    else if Index = ColSort then	//reset sorting
		begin
      Grid.Sorting.FieldNames.Clear;
      InitGrid;
    end;
	end;
end;

procedure TDBGridOptionsForm.InitGrid;
var
  I: Integer;
  C: TTSColumn;
begin
  PnlButtons.Visible := Grid.OptionsForm.ButtonPanel;
  if Grid.OptionsForm.TitleClick then
	  GrdColumns.Options := GrdColumns.Options + [goHeaderPushedLook]
  else
	  GrdColumns.Options := GrdColumns.Options - [goHeaderPushedLook];

  if DBGrids.dgColumnMove in Grid.Options then
	  GrdColumns.Options := GrdColumns.Options + [goRowMoving]
  else
	  GrdColumns.Options := GrdColumns.Options - [goRowMoving];

  GrdColumns.Columns[pred(ColVisible)].Visible := gocVisible in Grid.OptionsForm.Columns;
  GrdColumns.Columns[pred(ColFilter)].Visible := gocFilter in Grid.OptionsForm.Columns;
  GrdColumns.Columns[pred(ColFilterActive)].Visible := gocFilterActive in Grid.OptionsForm.Columns;
  GrdColumns.Columns[pred(ColWidth)].Visible := (dgColumnResize in Grid.Options)
																					  and (gocColumnWidth in Grid.OptionsForm.Columns);
  GrdColumns.Columns[pred(ColSort)].Visible := gocSorting in Grid.OptionsForm.Columns;

  BtnDefaultCols.Visible := DBGrids.dgColumnMove in Grid.Options;
  BtnShowAll.Visible := gocVisible in Grid.OptionsForm.Columns;
  BtnDefWidth.Visible := (dgColumnResize in Grid.Options)
  												and (gocColumnWidth in Grid.OptionsForm.Columns);
  BtnClearFilters.Visible := gocFilter in Grid.OptionsForm.Columns;
  BtnFilterActive.Visible := gocFilterActive in Grid.OptionsForm.Columns;

  GrdColumns.RowCount := Grid.Columns.Count + 1;
	for I := 0 to pred(Grid.Columns.Count) do
  begin
    C := Grid.Columns[I];
    GrdColumns.Cols[ColName][I+1] := C.Title.Caption;
    GrdColumns.Cols[ColVisible][I+1] := BoolToStr(C.Visible, '1', '0');
    GrdColumns.Cols[ColWidth][I+1] := IntToStr(C.Width);
    GrdColumns.Cols[ColFilter][I+1] := C.Filter.DisplayText;
    GrdColumns.Cols[ColFilterActive][I+1] := BoolToStr(C.Filter.Enabled, '1', '0');
  end;
  GrdColumns.Invalidate;
end;

procedure TDBGridOptionsForm.SetFilterState(FilterActive: Boolean);
begin
  Grid.Filter.Enabled := FilterActive;
  FilterActive := Grid.Filter.Enabled;
  BtnFilterActive.Down := FilterActive;
  GrdColumns.Columns[pred(ColFilterActive)].ReadOnly := not FilterActive;
  if FilterActive then
		GrdColumns.Columns[pred(ColFilterActive)].Title.ImageIndex := 5
  else
		GrdColumns.Columns[pred(ColFilterActive)].Title.ImageIndex := 6;
end;

procedure TDBGridOptionsForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TDBGridOptionsForm.EditGridSettings(ForGrid: TTSDBGrid);
var
  Pt: TPoint;
begin
  Grid := ForGrid;
	Pt := Grid.ClientToScreen(Point(2, Grid.DefaultRowHeight+2));
  Left := Pt.x;
  Top := Pt.y;
  InitGrid;
  Show;
end;

end.

