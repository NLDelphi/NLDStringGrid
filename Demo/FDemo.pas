unit FDemo;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, ExtCtrls, Grids, NLDStringGrid,
  Buttons, Spin, ComCtrls, ActnList, ImgList, SysUtils, Graphics, Math,
  Messages, Clipbrd;

type
  TDemoForm = class(TForm)
    Grid: TNLDStringGrid;
    BottomPanel: TPanel;
    GroupBox2: TGroupBox;
    Button5: TButton;
    GroupBox1: TGroupBox;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    GroupBox3: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    GroupBox4: TGroupBox;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    Button4: TButton;
    SpeedButton1: TSpeedButton;
    GroupBox5: TGroupBox;
    SpeedButton2: TSpeedButton;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpeedButton3: TSpeedButton;
    RadioGroup1: TRadioGroup;
    ColorBox3: TColorBox;
    Label4: TLabel;
    Button3: TButton;
    Button6: TButton;
    ColorBox4: TColorBox;
    Label5: TLabel;
    GroupBox6: TGroupBox;
    ColorBox5: TColorBox;
    ColorBox6: TColorBox;
    Label6: TLabel;
    Label7: TLabel;
    CheckBox1: TCheckBox;
    Button11: TButton;
    CheckBox2: TCheckBox;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    TopPanel: TPanel;
    GroupBox7: TGroupBox;
    Memo1: TMemo;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    Action1: TAction;
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure GridColumnsChanged(Sender: TObject; Column: TStringGridColumn);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure GridTitleClick(Sender: TObject; const Index: Integer;
      Column: TStringGridColumn);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure ColorBox3Change(Sender: TObject);
    procedure ColorBox5Change(Sender: TObject);
    procedure ColorBox6Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure ColorBox4Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure GridEditButtonClick(Sender: TObject);
  private
    procedure ShowStatus(const Txt: String);
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.dfm}

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  ColorBox3.Selected := Grid.FocusRectColor;
  CheckBox2.Checked := Grid.AlternatingRowColors.OverrideColumnColor;
  ColorBox4.Selected := Grid.SelectionColor;
end;

procedure TDemoForm.Button5Click(Sender: TObject);
var
  iCol: Integer;
  iRow: Integer;
begin
  with Grid, Selection do
    for iCol := Left to Right do
      for iRow := Top to Bottom do
        ReadOnly[iCol, iRow] := not ReadOnly[iCol, iRow];
end;

procedure TDemoForm.Button7Click(Sender: TObject);
begin
  Grid.MergeCells(Grid.Selection, False, False);
end;

procedure TDemoForm.Button8Click(Sender: TObject);
begin
  Grid.MergeCells(Grid.Selection, True, False);
end;

procedure TDemoForm.Button9Click(Sender: TObject);
begin
  Grid.MergeCells(Grid.Selection, True, True);
end;

procedure TDemoForm.Button10Click(Sender: TObject);
begin
  Grid.UnMergeCells(Grid.Selection);
end;

procedure TDemoForm.GridColumnsChanged(Sender: TObject;
  Column: TStringGridColumn);
begin
  if csLoading in ComponentState then
    Exit;
  if Column = nil then
    ShowStatus('Er zijn mogelijk wijzigingen in Grid.Columns doorgevoerd...')
  else
    ShowStatus(Format('Kolom %d is gewijzigd...', [Column.Index]));
end;

procedure TDemoForm.Button4Click(Sender: TObject);
begin
  Grid.SetGradientColumnColors(ColorBox1.Selected, ColorBox2.Selected,
    SpeedButton1.Down);
end;

procedure TDemoForm.Button1Click(Sender: TObject);
begin
  with Grid do
  begin
    ResetMainColors;
    SelectionColor := clHighLight;
    FocusRectColor := clDefault;
    GridLineColor := clSilver;
    FixedGridLineColor := clBlack;
    ReadOnlyColor := Color;
  end;
end;

procedure TDemoForm.Button2Click(Sender: TObject);
begin
  Grid.ResetAllFonts;
end;

procedure TDemoForm.SpeedButton2Click(Sender: TObject);
begin
  Grid.SyncColumns := SpeedButton2.Down;
end;

procedure TDemoForm.SpinEdit1Change(Sender: TObject);
begin
  if SpinEdit1.Text = '' then
    SpinEdit1.Value := 0;
  Grid.GridLineWidth := SpinEdit1.Value;
end;

procedure TDemoForm.SpeedButton3Click(Sender: TObject);
begin
  if SpeedButton3.Down then
    Grid.Options := Grid.Options + [goEditing]
  else
    Grid.Options := Grid.Options - [goEditing];
end;

procedure TDemoForm.GridTitleClick(Sender: TObject; const Index: Integer;
  Column: TStringGridColumn);
begin
  if Column = nil then
    ShowStatus(Format('U klikte op de kop van kolom %d, Columns[%:0d] = nil', [Index]))
  else
    ShowStatus(Format('U klikte op de kop van Columns[%d]', [Index]));
  if Assigned(Column) then
    Grid.Selection := TGridRect(Rect(Index, Grid.RowCount - 1,
      Index, Grid.FixedRows));
end;

procedure TDemoForm.Button3Click(Sender: TObject);
begin
  Grid.ImportCSV('Import.csv', True);
end;

procedure TDemoForm.Button6Click(Sender: TObject);
begin
  Grid.ExportCSV('Export.csv', True);
end;

procedure TDemoForm.RadioGroup1Click(Sender: TObject);
begin
  Grid.FocusRectStyle := TFocusRectStyle(RadioGroup1.ItemIndex);
end;

procedure TDemoForm.ColorBox3Change(Sender: TObject);
begin
  Grid.FocusRectColor := ColorBox3.Selected;
  RadioGroup1.ItemIndex := Byte(frSolidCustomColor);
end;

procedure TDemoForm.ColorBox5Change(Sender: TObject);
begin
  Grid.AlternatingRowColors.EvenRowColor := ColorBox5.Selected;
end;

procedure TDemoForm.ColorBox6Change(Sender: TObject);
begin
  Grid.AlternatingRowColors.OddRowColor := ColorBox6.Selected;
end;

procedure TDemoForm.CheckBox1Click(Sender: TObject);
begin
  Grid.AlternatingRowColors.IncludeFixed := CheckBox1.Checked;
end;

procedure TDemoForm.CheckBox2Click(Sender: TObject);
begin
  Grid.AlternatingRowColors.OverrideColumnColor := CheckBox2.Checked;
end;

procedure TDemoForm.Button11Click(Sender: TObject);
begin
  if Grid.SyncColumns then
    Grid.Columns.Add
  else
    if Application.MessageBox('Nieuwe kolom aan Columns-property toevoegen?',
      PChar(Application.Title), MB_YESNO) = mrYes then
      Grid.Columns.Add
    else
      Grid.ColCount := Grid.ColCount + 1;
end;

procedure TDemoForm.ColorBox4Change(Sender: TObject);
begin
  Grid.SelectionColor := ColorBox4.Selected;
end;

procedure TDemoForm.Timer1Timer(Sender: TObject);
begin
  StatusBar1.SimpleText := '';
  Timer1.Enabled := False;
end;

procedure TDemoForm.ShowStatus(const Txt: String);
begin
  StatusBar1.SimpleText := Txt;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TDemoForm.Button12Click(Sender: TObject);
begin
  if Grid.ColCount > 3 then
    Grid.Cols[3].Assign(Memo1.Lines);
  Button14.Enabled := True;
end;

procedure TDemoForm.Button13Click(Sender: TObject);
begin
  if Grid.ColCount > 4 then
    Grid.MoveColumn(4, 3);
end;

procedure TDemoForm.Button14Click(Sender: TObject);
var
  i: Integer;
  MaxWidth: Integer;
begin
  if Grid.ColCount > 3 then
  begin
    MaxWidth := 0;
    for i := 0 to Grid.RowCount - 1 do
      MaxWidth := Max(MaxWidth, Grid.Canvas.TextWidth(Grid.Cells[3, i]));
    Grid.ColWidths[3] := MaxWidth + 4;
  end;
end;

procedure TDemoForm.Button15Click(Sender: TObject);
begin
  with TStringGridTitle.Create(nil) do
  try
    Height := 35;
  except
    on E: EStringGridError do
    begin
      Application.ShowException(E);
      Free;
    end;
  end;
end;

var
  Testing: Boolean = False;

type
  HackGrid = class(TNLDStringGrid);

procedure TDemoForm.Button16Click(Sender: TObject);

//Routines that raise exceptions:
//-------------------------------
//A: TStringGridTitle.SetHeight(const Value: Integer);                (2x)
//C: TStringGridColumn.Create(Collection: TCollection);
//D: TStringGridColumn.SetFixed(const Value: Boolean);
//E: TStringGridColumn.SetIndex(Value: Integer);                      (2x)
//G: TStringGridColumn.SetVisible(const Value: Boolean);
//H: TStringGridColumn.SetWidth(const Value: Integer);
//I: TStringGridStrings.Delete(Index: Integer);
//J: TStringGridStrings.Insert(Index: Integer; const S: String);
//K: TInplaceEditListEx.WMPaste(var Message: TWMPaste);               (3x)
//N: TNLDStringGrid.InsertColumn(const AtIndex: Integer);
//O: TNLDStringGrid.MoveColumn(const FromIndex, ToIndex: Integer);    (2x)
//Q: TNLDStringGrid.SetCellValue(ACol, ARow: Integer; Value: String); (6x)

var
  Collection: TCollection;
  Column1: TStringGridColumn;
  Column2: TStringGridColumn;
  Column3: TStringGridColumn;
begin
  Testing := True;
  Collection := TCollection.Create(TStringGridColumn);
  Column1 := TStringGridColumn.Create(nil);
  Column2 := nil;
  Column3 := TStringGridColumn.Create(Grid.Columns);
  try
  //Test A:
    try
      Column1.Title.Height := 35;
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test B:
    Button15Click(nil);
  //Test C:
    try
      Column2 := TStringGridColumn.Create(Collection);
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test D:
    try
      Column1.Fixed := True;
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test E:
    try
      Column1.Index := 35;
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test F:
    try
      Column3.Index := 35;
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test G:
    try
      Column1.Visible := False;
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test H:
    try
      Column1.Width := 35;
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test I:
    try
      Grid.Rows[0].Delete(3);
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test J:
    try
      Grid.Cols[0].Insert(3, 'Invoertest');
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test K:
    try
      try
        Grid.Options := Grid.Options + [goEditing];
        with Grid, Columns[2] do
          Selection := TGridRect(Rect(Index, FixedRows, Index, FixedRows));
        Grid.EditorMode := True;
        HackGrid(Grid).InplaceEditor.SetFocus;
        Clipboard.Open;
        Clipboard.AsText := '-435';
        Grid.Columns[2].InputStyle := isAbsInteger;
        PostMessage(Self.ActiveControl.Handle, WM_PASTE, 0, 0);
        PostMessage(HackGrid(Grid).InplaceEditor.Handle, WM_PASTE, 0, 0);
      except
        on E: EStringGridError do
        begin
          Application.ShowException(E);
          Clipboard.Close;
        end
        else
          raise;
      end;
    finally
      Clipboard.Close;
    end;
  //Test N:
    try
      Grid.InsertColumn(Grid.ColCount + 1);
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test O:
    try
      with Grid do
        MoveColumn(Columns.Count - 1, ColCount + 2);
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test P:
    try
      with Grid do
        MoveColumn(Columns.Count, ColCount + 2);
    except
      on E: EStringGridError do Application.ShowException(E)
      else raise;
    end;
  //Test Q:
    try
      Grid.Columns[2].InputStyle := isAbsInteger;
      Caption := Column3.EditFormat;
      Grid.Cells[2, Grid.FixedRows] := '-134';
    except
      on E: EStringGridError do Application.ShowException(E)
      else
        raise;
    end;
  finally
    Column3.Free;
    Column2.Free;
    Column1.Free;
    Collection.Free;
    Testing := False;
  end;
end;

procedure TDemoForm.Action1Execute(Sender: TObject);
begin
  Caption := 'Action1 executed';
end;

procedure TDemoForm.GridEditButtonClick(Sender: TObject);
begin
  Caption := 'Editbutton click';
end;

end.

































