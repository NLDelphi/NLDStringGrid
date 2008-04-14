unit NLDStringgrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Math, Mask, consts, StdCtrls, buttons, ComCtrls;

const
  //used for checking input when textstyle is integer or real
  NUMERIC_CHAR = [$30..$39,$8,$9,$D,ord('-')];
  ABS_NUMERIC_CHAR  = [$30..$39,$8,$9,$D];

type
  // forward declarations...
  TNLDStringGrid         = class;
  TNLDStringColumns      = class;
  TNLDStringColumnItem   = class;
  TNLDInplaceCombobox    = class;
  TNLDInplaceEdit        = class;

  // inserting row positions
  TInsertPos            = (ipBefore,ipAfter,ipBegin,ipEnd);
  // deleting row positions
  TDeletePos            = (dpBegin, dpEnd, dpCurrent, dpAll, dpDesignated, dpRange);
  // delete row options
  TDeleteOptions        = (doKeepOneBlank, doKeepOneStandard, doKeepNone, doNoOptions);
  // styles for inplace editor
  TNLDEditStyle          = (etEdit, etComboBox, etComboBoxList);
  //text types for inplace editor
  TNLDTextType           = (ttAbsInteger, ttInteger, ttAbsReal, ttReal, ttString);
  // vertical text alignment
  TNLDVAlignment         = (tvaTop, tvaCenter, tvaBottom);
  // array to hold read only value for one row
  TNLDReadOnlyRow        = array of boolean;
  // array to hold read only values for all rows
  TNLDReadOnlyCells      = array of TNLDReadOnlyRow;


  // column class...
  TNLDStringColumnItem   = class(TCollectionItem)
  private
    FAlignment          : TAlignment;
    FColor              : TColor;
    FComboBoxDropDown   : Cardinal;
    FComboBoxItems      : TStrings;
    FDefaultCellText    : string;
    FEditStyle          : TNLDEditStyle;
    FFont               : TFont;
    FFormatString       : String;
    FHeaderAlignment    : TAlignment;
    FHeaderCaption      : string;
    FHeaderColor        : TColor;
    FHeaderFont         : TFont;
    FHeaderMultiLine    : Boolean;
    FHeaderVAlignment   : TNLDVAlignment;
    FMaxLength          : integer;
    FMultiLine          : Boolean;
    FReadOnly           : Boolean;
    FTextType           : TNLDTextType;
    FVAlignment         : TNLDVAlignment;
    FCalcHighlight      : Boolean;
    function            GetComboBoxItems        : TStrings;
    function            GetGrid                 : TNLDStringGrid;
    function            GetWidth                : Integer;
    procedure           SetAlignment            (const Value: TAlignment);
    procedure           SetColor                (const Value: TColor);
    procedure           SetComboBoxItems        (const Value: TStrings);
    procedure           SetDefaultCellText      (const Value: string);
    procedure           SetEditStyle            (const Value: TNLDEditStyle);
    procedure           SetFont                 (const Value: TFont);
    procedure           SetHeaderAlignment      (const Value: TAlignment);
    procedure           SetHeaderCaption        (const Value: string);
    procedure           SetHeaderColor          (const Value: TColor);
    procedure           SetHeaderFont           (const Value: TFont);
    procedure           SetHeaderMultiLine      (const Value: Boolean);
    procedure           SetHeaderVAlignment     (const Value: TNLDVAlignment);
    procedure           SetMaxLength            (const Value: integer);
    procedure           SetMultiLine            (const Value: Boolean);
    procedure           SetReadOnly             (const Value: Boolean);
    procedure           SetTextType             (const Value: TNLDTextType);
    procedure           SetVAlignment           (const Value: TNLDVAlignment);
    procedure           SetWidth                (const Value: Integer);
    procedure           SetUseCalcHighlight     (const Value: Boolean);
    function            GetReadOnly             : Boolean;
  protected
    function            GetDisplayName          : string; override;
  public
    constructor         Create                  (ITStringColumns: TCollection); override;
    destructor          Destroy                 ; override;
    procedure           Assign                  (Source: TPersistent); override;
    property            Grid                    : TNLDStringGrid         read GetGrid;
  published
    property            Alignment               : TAlignment            read FAlignment         Write SetAlignment default taLeftJustify;
    property            Color                   : TColor                read FColor             Write SetColor default clWindow;
    // number of dropdown rows in a combobox style inplace editor
    property            ComboBoxDropDown        : Cardinal              read FComboBoxDropDown  Write FComboBoxDropDown;
    // items property of combobox style inplace editor
    property            ComboBoxItems           : TStrings              read GetComboBoxItems   Write SetComboBoxItems;
    property            DefaultCellText         : string                read FDefaultCellText   Write SetDefaultCellText;
    // style of the inplace editor
    property            EditStyle               : TNLDEditStyle          read FEditStyle         Write SetEditStyle default etEdit;
    // string for formatting a editbox style inplace editor
    property            FormatString            : String                read FFormatString      write FFormatString;
    property            Font                    : TFont                 read FFont              Write SetFont;
    property            HeaderAlignment         : TAlignment            read FHeaderAlignment   Write SetHeaderAlignment default taLeftJustify;
    property            HeaderCaption           : string                read FHeaderCaption     Write SetHeaderCaption;
    property            HeaderColor             : TColor                read FHeaderColor       Write SetHeaderColor default clBtnFace;
    property            HeaderFont              : TFont                 read FHeaderFont        Write SetHeaderFont;
    property            HeaderMultiLine         : Boolean               read FHeaderMultiLine   Write SetHeaderMultiLine;
    property            HeaderVAlignment        : TNLDVAlignment         read FHeaderVAlignment  Write SetHeaderVAlignment;
    property            MaxLength               : integer               read FMaxLength         write SetMaxLength;
    property            MultiLine               : Boolean               read FMultiLine         Write SetMultiLine;
    property            ReadOnly                : Boolean               read GetReadOnly        write SetReadOnly default false;
    property            TextType                : TNLDTextType           read FTextType          write SetTextType default ttString;
    // selected cell color will be calculated?
    property            UseCalcHighlight        : Boolean               read FCalcHighlight     Write SetUseCalcHighlight default true;
    property            VAlignment              : TNLDVAlignment         read FVAlignment        Write SetVAlignment;
    property            Width                   : Integer               read GetWidth           Write SetWidth default 64;
  end;

  // collection class for columns
  TNLDStringColumns      = class(TCollection)
  private
    FOwner              : TNLDStringGrid;
  protected
    function            GetItem                 (Index: Integer) : TNLDStringColumnItem;
    function            GetOwner                : TPersistent; override;
    procedure           SetItem                 (Index: Integer; const Value: TNLDStringColumnItem);
    procedure           Update                  (Item: TCollectionItem); override;
  public
    constructor         Create                  (AOwner: TNLDStringGrid);
    destructor          Destroy                 ; override;
    property            Items[Index: Integer]   : TNLDStringColumnItem read GetItem Write SetItem; default;
    function            Owner                   : TNLDStringGrid;
  end;

  // main stringgrid class
  TNLDStringGrid         = class(TStringGrid)
  private
    CurrEditStyle       : TNLDEditStyle;
    EditMode            : boolean;
    FBlockFocus         : boolean;
    FColCount           : Longint;
    FColumns            : TNLDStringColumns;
    FComboBox           : TNLDInplaceComboBox;
    FFixedLineColor     : TColor;
    FGridLineColor      : TColor;
    FStretchRight       : Boolean;
    FFixedColWidth      : Integer;
    FFixedRowHeight     : Integer;
    FRowCount           : Longint;
    FReadOnlyCells      : TNLDReadOnlyCells;
    FReadOnlyColor      : TColor;
    FFocusRectColor     : TColor;
    procedure           HideEditControl         (acol,arow:integer);
    procedure           MoveColReadOnlyCells    (FromIndex, ToIndex: Longint);
    procedure           MoveRowReadOnlyCells    (FromIndex, ToIndex: Longint);
    procedure           SetColCount             (Value: Longint);
    procedure           SetColumns              (const Value: TNLDStringColumns);
    procedure           SetFixedColWidth        (const Value: Integer);
    procedure           SetFixedLineColor       (const Value: TColor);
    procedure           SetFixedRowHeight       (const Value: Integer);
    procedure           SetFocusRectColor(const Value: TColor);
    procedure           SetGridLineColor        (const Value: TColor);
    procedure           SetReadOnlyColor(const Value: TColor);
    procedure           SetRowCount             (Value: Longint);
    procedure           SetStretchRight         (const Value: Boolean);
    procedure           ShowEditControl         (acol,arow:integer);
  protected
    function            CanEditShow             :boolean; override;
    function            CreateEditor            : TInplaceEdit; override;
    function            FormatValidateCell      (var s: string;ACol, ARow: integer): boolean;
    function            GetCellEditor           (ACol,ARow:integer) :TNLDEditStyle;
    function            SelectCell              (ACol, ARow: longint): Boolean; override;
    procedure           ColWidthsChanged        ; override;
    procedure           DeleteRow               (Index: integer); override;
    procedure           DoEnter                 ; override;
    procedure           DoExit                  ; override;
    procedure           DrawCell                (ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure           SetEditText             (ACol, ARow: Longint; const Value: string); override;
    procedure           SizeChanged             (OldColCount, OldRowCount: longint); override;
    procedure           StretchRightCol         ;
    procedure           WMSize(var Msg:TWMKey); message WM_CHAR;
    property            Combobox                : TNLDInplaceComboBox    read FComboBox;
  public
    constructor         Create                  (AOwner: TComponent); override;
    destructor          Destroy                 ; override;
    procedure           HideInplaceEdit         ;
    procedure           DeleteRows              (DeletePos: TDeletePos; DeleteOptions: TDeleteOptions;
                                                 StartIndex: Integer; Count: integer);
    procedure           InsertRow               (IsBlank: Boolean; InsertPos: TInsertPos);
    procedure           InsertCol               (IsBlank: Boolean; InsertPos: TInsertPos);
    procedure           MoveColumn              (FromIndex, ToIndex: Longint);
    procedure           MoveRow                 (FromIndex, ToIndex: Longint);
    procedure           ShowInplaceEdit         ;
    procedure           SetCellReadOnly         (ACol,ARow: integer;Value: boolean);
    function            GetCellReadOnly         (ACol,ARow: integer): boolean;
    procedure           SetRowReadOnly          (ARow: integer; Value: boolean);
    function            GetRowReadOnly          (ARow:integer) :boolean;
    procedure           SetColReadOnly          (ACol: integer; Value: boolean);
    function            GetColReadOnly          (ACol:integer) :boolean;
    function            RowIsEmpty              (ARow: Integer) : Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property            ColCount                : Longint               read FColCount          write SetColCount;
    property            Columns                 : TNLDStringColumns     read FColumns           Write SetColumns;
    property            FocusRectColor          : TColor                read FFocusRectColor    write SetFocusRectColor;
    property            FixedColWidth           : Integer               read FFixedColWidth     write SetFixedColWidth;
    property            FixedLineColor          : TColor                read FFixedLineColor    Write SetFixedLineColor;
    property            FixedRowHeight          : Integer               read FFixedRowHeight    write SetFixedRowHeight;
    property            GridLineColor           : TColor                read FGridLineColor     Write SetGridLineColor;
    property            ReadOnlyColor           : TColor                read FReadOnlyColor     Write SetReadOnlyColor;
    property            RowCount                : Longint               read FRowCount          write SetRowCount;
    property            StretchRight            : Boolean               read FStretchRight      write SetStretchRight;
  end;

  // inplace editor class
  TNLDInplaceEdit = class(TInplaceEdit)
  private
    FMaxLength: smallint;
    procedure WMChar(var Msg:TWMKey); message WM_CHAR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure CreateWnd; override;
  public
    ITStringColumnItem: TNLDStringColumnItem;
    constructor Create(AOwner: TComponent); override;
    property MaxLength:smallint read FMaxLength write FMaxLength;
  end;

  // inplace editor combobox class
  TNLDInplaceComboBox = class(TCustomComboBox)
  protected
    procedure Change; override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    Grid: TNLDStringGrid;
    constructor Create(AOwner: TComponent); override;
    procedure DoExit; override;
    procedure SizeDropDownWidth;
  end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('NLDelphi', [TNLDStringGrid]);
end;

// calculate a contrasting color
function GetContrastColor (Color: TColor): TColor;
var r, g, b: Byte;
begin
  Color := ColorToRGB (Color);
  if GetRValue (Color) >= $80 then r := 0
  else
    r := $FF;
  if GetGValue (Color) >= $80 then g := 0
  else
    g := $FF;
  if GetRValue (Color) >= $80 then b := 0
  else
    b := $FF;
  Result := RGB (r,g,b);
end;

{ TNLDStringColumnItem }

procedure TNLDStringColumnItem.Assign(Source: TPersistent);
begin
  if Source is TNLDStringColumnItem then
  begin
    HeaderColor:=TNLDStringColumnItem(Source).HeaderColor;
    HeaderFont.Assign(TNLDStringColumnItem(Source).HeaderFont);
    Color:=TNLDStringColumnItem(Source).Color;
    Font.Assign(TNLDStringColumnItem(Source).Font);
    Alignment:=TNLDStringColumnItem(Source).Alignment;
    HeaderAlignment:=TNLDStringColumnItem(Source).HeaderAlignment;
    HeaderCaption:=TNLDStringColumnItem(Source).HeaderCaption;
    EditStyle:=TNLDStringColumnItem(Source).EditStyle;
    FMaxLength:=TNLDStringColumnItem(Source).MaxLength;
  end
  else
    inherited Assign(Source);
end;

constructor TNLDStringColumnItem.Create(ITStringColumns: TCollection);
var
  Grid: TNLDStringgrid;
begin
  Grid:=nil;
  if Assigned(ITStringColumns) and (ITStringColumns is TNLDStringColumns) then
    Grid:=TNLDStringColumns(ITStringColumns).Owner;
  try
    inherited Create(ITStringColumns);
    FHeaderColor:=Grid.FixedColor;
    FHeaderFont:=TFont.Create;
    FHeaderFont.Assign(Grid.Font);
    FHeaderAlignment:=taLeftJustify;
    FColor:=Grid.Color;
    FComboBoxDropDown:=5;
    FMaxLength:=0;
    FTextType:=ttString;
    FFont:=TFont.Create;
    FComboBoxItems:= TStringList.Create;
    FFont.Assign(Grid.Font);
    FAlignment:=taLeftJustify;
  except
   showmessage('Error creating grid');
  end;
end;

destructor TNLDStringColumnItem.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  FComboBoxItems.Free;
  inherited Destroy;
end;

function TNLDStringColumnItem.GetComboBoxItems: TStrings;
begin
  Result:=FComboBoxItems;
end;

function TNLDStringColumnItem.GetDisplayName: string;
begin
  result:='ITStringGrid Column['+inttostr(index)+']';
end;

function TNLDStringColumnItem.GetGrid: TNLDStringGrid;
begin
  Result:=TNLDStringColumns(Collection).Owner;
end;

function TNLDStringColumnItem.GetReadOnly: Boolean;
begin
  result:=grid.GetColReadOnly(Index);
end;

function TNLDStringColumnItem.GetWidth: Integer;
begin
  result:=Grid.Colwidths[Index]
end;

procedure TNLDStringColumnItem.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Grid.InvalidateCol(Index);
  end;
end;

procedure TNLDStringColumnItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
    FColor := Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetComboBoxItems(const Value: TStrings);
begin
  if Value=nil then
  begin
    FComboBoxItems.Free;
    FComboBoxItems:=nil;
    exit;
  end;
  FComboBoxItems.Assign(Value);
end;

procedure TNLDStringColumnItem.SetDefaultCellText(const Value: string);
begin
  if Value <> FDefaultCellText then
    FDefaultCellText:=Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetEditStyle(const Value: TNLDEditStyle);
begin
  if FEditStyle <> Value then
    FEditStyle := Value;
end;

procedure TNLDStringColumnItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetHeaderAlignment(const Value: TAlignment);
begin
  if FHeaderAlignment <> Value then
  begin
    FHeaderAlignment := Value;
    Grid.InvalidateCol(Index);
  end;
end;

procedure TNLDStringColumnItem.SetHeaderCaption(const Value: string);
begin
  FHeaderCaption:=Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetHeaderColor(const Value: TColor);
begin
  FHeaderColor := Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetHeaderMultiLine(const Value: Boolean);
begin
  FHeaderMultiLine := Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetHeaderVAlignment(
  const Value: TNLDVAlignment);
begin
  FHeaderVAlignment := Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
end;

procedure TNLDStringColumnItem.SetMultiLine(const Value: Boolean);
begin
  FMultiLine := Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetReadOnly(const Value: Boolean);
begin
  FReadOnly:=Value;
  Grid.SetColReadOnly(Index,Value);
end;

procedure TNLDStringColumnItem.SetTextType(const Value: TNLDTextType);
begin
  FTextType := Value;
end;

procedure TNLDStringColumnItem.SetUseCalcHighlight(const Value: Boolean);
begin
  FCalcHighlight := Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetVAlignment(const Value: TNLDVAlignment);
begin
  FVAlignment := Value;
  Grid.InvalidateCol(Index);
end;

procedure TNLDStringColumnItem.SetWidth(const Value: Integer);
begin
  Grid.ColWidths[Index]:=Value;
end;

{ TNLDStringColumns }

constructor TNLDStringColumns.Create(AOwner: TNLDStringGrid);
begin
  FOwner:=AOwner;
  inherited Create(TNLDStringColumnItem);
end;

destructor TNLDStringColumns.destroy;
begin
  inherited Destroy;
end;

function TNLDStringColumns.GetItem(Index: Integer): TNLDStringColumnItem;
begin
  Result:=TNLDStringColumnItem(inherited GetItem(Index));
end;

function TNLDStringColumns.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

function TNLDStringColumns.Owner: TNLDStringGrid;
begin
  Result:= FOwner;
end;

procedure TNLDStringColumns.SetItem(Index: Integer;
  const Value: TNLDStringColumnItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TNLDStringColumns.Update(Item: TCollectionItem);
begin
 inherited Update(Item);
{ TODO -oWalter -cweird bugs : delphi klapt ruit als dit wordt uncommented???? }
{ if (count<>Owner.ColCount) and (count>Owner.FixedCols) then
   fOwner.ColCount:=Count;   }
 fOwner.Invalidate;
end;

{ TNLDStringGrid }

//can the editor be shown?
function TNLDStringGrid.CanEditShow: boolean;
begin
  result:=inherited CanEditshow;
  if result and (editmode=false) then
  begin
    CurrEditStyle:=etEdit;
    CurrEditStyle:=GetCellEditor(col,row);
    if not (CurrEditStyle in [etEdit]) then
      begin
        ShowEditControl(col,row);
        result:=false;
      end;
  end;
end;

procedure TNLDStringGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  if StretchRight then
    StretchRightCol;
end;

constructor TNLDStringGrid.Create(AOwner: TComponent);

  // initialize array for readonly cells
  procedure InitReadOnlyCells;
  var
    i,j: integer;
  begin
    setlength(FReadOnlyCells,ColCount,RowCount);
    for i:=high(FReadOnlyCells) downto low(FReadOnlyCells) do
      for j:=high(FReadOnlyCells[i]) downto low(FReadOnlyCells[i]) do
        if (i<FixedCols) or (j<FixedRows) then
          SetCellReadOnly(i,j,true)
        else
          SetCellReadOnly(i,j,false);
  end;

begin
  inherited Create(AOwner);
  FColumns:= TNLDStringColumns.Create(Self);
  ColCount:= 5;
  RowCount:= 5;
  // initialize the readonlycells array
  InitReadOnlyCells;
  DefaultColWidth:=60;
  DefaultRowHeight:=21;
  FFixedColWidth:=10;
  FFixedRowHeight:=21;
  FGridLineColor:= clBlack;
  FFixedLineColor:=clSilver;
  if not (csDesigning in ComponentState) then
  begin
    FComboBox:=TNLDInplaceCombobox.create(Self);
    ComboBox.Parent:=self;
    ComboBox.visible:=false;
    ComboBox.enabled:=false;
    CurrEditStyle:=etEdit;
    EditMode:=false;
  end;
end;

function TNLDStringGrid.CreateEditor: TInplaceEdit;
begin
  result:=TNLDInplaceEdit.Create(Self);
end;

// delete a single row with index Index
procedure TNLDStringGrid.DeleteRow(Index: integer);
begin
  if Index>=0 then
    if (RowCount >= FixedRows) and (Index < RowCount) then
    begin
      if (Index<RowCount-1) then
        MoveRow(Index,RowCount-1);
      Rows[RowCount-1].Clear;
      RowCount := RowCount-1;
    end
end;

//delete rows depending on options
procedure TNLDStringGrid.DeleteRows(DeletePos: TDeletePos;
  DeleteOptions: TDeleteOptions; StartIndex: Integer; Count: integer);
var
  i: integer;
begin
  case DeletePos of
  dpCurrent:                     // delete Current row
    DeleteRow(Row);              // Row is the index of current row
  dpDesignated:                  // delete row designated by StartIndex
    DeleteRow(StartIndex);
  dpBegin:                       // delete first row in the grid
    DeleteRow(FixedRows);        // fixedRows is the index of first nonfixed row
  dpEnd:                         // delete last row in the grid
    DeleteRow(RowCount-1);       // RowCount-1 is the index of the last row
  dpAll:                         // delete all rows. DeleteOptions determine further options
    begin
      while RowCount > FixedRows+1 do
        DeleteRow(RowCount-1);
      case DeleteOptions of
      doKeepOneBlank:            // keep one blank line
        begin
          InsertRow(True,ipBegin);
          DeleteRow(RowCount-1);
        end;
      doKeepNone, doNoOptions:   // keep no Rows or no Options selected
        begin
          rowcount:=FixedRows;
          FixedRows:=1;
        end;
      doKeepOneStandard:         // keep one row filled in with default texts
        begin
          InsertRow(False,ipBegin);
          DeleteRow(RowCount-1);
        end;
      end;
    end;
  dpRange:                       // delete a range of rows starting with StartIndex and ranging NumberOfRows
    begin
      for i:= StartIndex to StartIndex+Count do
      begin
        DeleteRow(i);
      end;
    end;
  end;
end;

destructor TNLDStringGrid.Destroy;
begin
//  FReadOnlyRows.Free;
  FColumns.Free;
  if Assigned(FComboBox) then
    FComboBox.Free;
  inherited Destroy;
end;

// onenter of the grid
procedure TNLDStringGrid.DoEnter;
begin
 if FBlockFocus then
   exit;
 inherited DoEnter;
 SelectCell(col,row);
end;

// onexit of the grid
procedure TNLDStringGrid.DoExit;
begin
 if FBlockFocus then
   exit;
 inherited DoExit;
end;

procedure TNLDStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);

  //not used yet...

  type
    TBdrFlag = (bdrHorz,bdrVert);
    TBdrFlags = set of TBdrFlag;
//
//  procedure DrawBorders(ARect: TRect; bdrFlags: TBdrFlags; LineWidth: Integer);
//  var
//    i: integer;
//  begin
//    if bdrHorz in bdrFlags then
//    begin
//      //draw horizontal lines
//      with canvas do
//      begin
//        for i:=-1 to LineWidth-2 do
//        begin
//          MoveTo(ARect.Left,Arect.Top-i);
//          LineTo(ARect.Right,Arect.Top-i);
//          MoveTo(ARect.Left,Arect.Bottom+i);
//          LineTo(ARect.Right,Arect.Bottom+i);
//        end;
//      end;
//    end;
//    if bdrVert in bdrFlags then
//    begin
//      //draw vertical lines
//    end;
//  end;

  // draw text in the cell
  procedure DrawCellText(Alignment: TAlignment; VAlignment: TNLDVAlignment; Multiline: Boolean);
  var
    s: string;
    r: TRect;
    a: integer;
  begin
    r := ARect;
    r.Top := r.Top + 2;
    r.Bottom := r.Bottom - 2;
    r.left := r.left + 2;
    r.Right := r.Right - 2;

    //set horizontal alignment
    case Alignment of
      taRightJustify:  a := DT_RIGHT;
      taCenter:        a := DT_CENTER;
      else             a := DT_LEFT;
    end;
    //set multiline
    if Multiline then
      a:=a or DT_WORDBREAK
    else
      //set vertical alignment
      case VAlignment of
        tvaTop:
        begin
          a := a or DT_TOP or DT_SINGLELINE;
        end;
        tvaCenter:
        begin
          a := a or DT_VCENTER or DT_SINGLELINE;
        end;
        tvaBottom:
        begin
          a := a or DT_BOTTOM or DT_SINGLELINE;
        end;
      end;
    // stop prefix processing and show ellipsis if line is longer than cell...
    a := a or DT_NOPREFIX;
    a := a or DT_END_ELLIPSIS;
    // check for empty cell
    s:=cells[ACol, ARow];
    if (s='') and (Columns[ACol].DefaultCellText<>'') and not(gdFixed in AState) then
    begin
      //fill in default cell text
      Cells[ACol,ARow]:=Columns[ACol].DefaultCellText;
      s:=cells[ACol, ARow];
    end;
    //fill in headercaption
    if ARow < 1 then
      s:=Columns[ACol].HeaderCaption;

    FillRect(Canvas.Handle, ARect, Canvas.Brush.Handle);
    DrawText(Canvas.Handle, PChar(s), -1, r, a);
  end;

var
  Column: TNLDStringColumnItem;
  grfFlags,i: Integer;
  bdrFlags: TBdrFlags;
begin
  if DefaultDrawing then
  begin
    Column := Columns[ACol];
    //set color and font
    if ARow < FixedRows then
    begin
      Canvas.Brush.Color := Column.FHeaderColor;
      Canvas.Font := Column.FHeaderFont;
    end
    else
      if ACol >= FixedCols then
      begin
        Canvas.Brush.Color := Column.FColor;
        Canvas.Font := Column.FFont;
      end;

    // set readonlycolor for cell
    if not(gdFixed in AState) and GetCellReadOnly(ACol,ARow) then
      Canvas.Brush.Color := FReadOnlyColor;

    // use calculated Highlight colors or default windows colors?
    if ((gdSelected in AState) and focused and
        (goEditing in Options)) then
    begin
      if not (goDrawFocusSelected in Options) then
      begin
        Canvas.Brush.Color := Column.Color;
        Canvas.Font := Column.Font;
      end
      else
        if Column.UseCalcHighlight then
        begin
          Canvas.Brush.Color := GetContrastColor(Column.Color);
          Canvas.Font.Color := Column.Color;
        end
        else
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
        end;
    end;

    DefaultDrawing := false;

    //Draw text with correct alignment
    if ARow < FixedRows then
      DrawCellText(Column.HeaderAlignment, Column.HeaderVAlignment, Column.HeaderMultiLine)
    else
      DrawCellText(Column.Alignment, Column.VAlignment, Column.MultiLine);

    if (gdSelected in AState) and not(goDrawFocusSelected in options) and focused then
    begin
      SetTextColor(Canvas.handle,clblack);
//      Canvas.Brush.Color:=clBlack;// xor FocusRectColor;
      DrawFocusRect(Canvas.Handle, ARect);
    end;

    inherited DrawCell(ACol, ARow, ARect, AState);


    // draw cell borders
{ TODO :
change border drawing to draw only selected borders instead of using
framerect(this draws all borders ignoring the settings) }
    if gdFixed in AState then
    begin
      grfFlags:=0;
      if goFixedVertLine in Options then
      begin
        grfFlags := BF_RIGHT or BF_LEFT;
      end;
      if goFixedHorzLine in Options then
      begin
        grfFlags := grfFlags or BF_TOP or BF_BOTTOM;
      end;
      Canvas.Brush.Color := FFixedLineColor;
//      for i:=1 to GridLineWidth do
//      begin
//        InflateRect(ARect,1,1);
        DrawEdge(Canvas.Handle, aRect, BDR_RAISEDINNER, grfFlags);
//      end;
    end
    else
    begin
      bdrFlags:=[];
      if goVertLine in Options then
      begin
        include(bdrFlags,bdrVert);
      end;
      if goHorzLine in Options then
      begin
        include(bdrFlags,bdrHorz);
      end;
      Canvas.Brush.Color := FGridLineColor;
      for i:=1 to GridLineWidth do
      begin
        InflateRect(ARect,1,1);
        Canvas.FrameRect(ARect);
      end;
      //Canvas.Pen.Color := FGridLineColor;
//      DrawBorders(aRect, bdrFlags, GridLineWidth)
    end;

    DefaultDrawing := true;

  end
  else
    inherited DrawCell(ACol, ARow, ARect, AState);


end;

// get the type of cell editor
function TNLDStringGrid.GetCellEditor(ACol, ARow: integer): TNLDEditStyle;
begin
  result:=etEdit;
  if (FColumns.Count>Acol) then
  begin
    if (ACol>=FixedCols) and (Arow>=FixedRows) then
    begin
      result:=Columns[acol].EditStyle;
      ComboBox.Items.Assign(Columns[acol].ComboBoxItems);
    end;
  end;
end;

// hide the inplace editor
procedure TNLDStringGrid.HideEditControl(acol, arow: integer);
begin
 case CurrEditStyle of
 etComboBox:
   begin
    if editmode then
     begin
      editmode:=false;
      if (Combobox.Items.IndexOf(FComboBox.Text)=-1) and (not(FComboBox.Text='')) then
        Columns[Col].ComboBoxItems.add(FComboBox.Text);
      Cells[ACol,ARow]:=FComboBox.Text;
      FComboBox.enabled:=false;
      FComboBox.visible:=false;
     end;
   end;
 etComboBoxList:
   begin
    if editmode then
     begin
      editmode:=false;
      Cells[ACol,ARow]:=FComboBox.Text;
      FComboBox.enabled:=false;
      FComboBox.visible:=false;
     end;
   end;
 etEdit:
   begin
      // same as above but for other etypes.
   end;
 end;
end;

// hide the inplace editor
procedure TNLDStringGrid.HideInplaceEdit;
begin
 HideEditControl(col,row);
 HideEditor;
end;

// insert a row
procedure TNLDStringGrid.InsertRow(IsBlank: Boolean; InsertPos: TInsertPos);
var
  i: integer;
begin
  RowCount:=RowCount+1;
  case InsertPos of
  ipAfter:   begin
               moverow(RowCount-1,Row+1);
               for i:=0 to ColCount-1 do
                 if IsBlank then
                   Cells[i,Row+1]:=' ';
              end;
  ipBefore:  begin
               moverow(RowCount-1,Row);
               for i:=0 to ColCount-1 do
                 if IsBlank then
                   Cells[i,Row]:=' ';
              end;
  ipBegin:   begin
               moverow(RowCount-1,FixedRows);
               if IsBlank then
                 for i:=0 to ColCount-1 do
                   Cells[i,FixedRows]:=' ';
              end;
  ipEnd:     begin
               if IsBlank then
                 for i:=0 to ColCount-1 do
                   Cells[i,RowCount-1]:=' ';
              end;
  end;
end;

// move a column
procedure TNLDStringGrid.MoveColumn(FromIndex, ToIndex: Integer);
begin
  MoveColReadOnlyCells(FromIndex,ToIndex);
  inherited MoveColumn(FromIndex, ToIndex);
end;

// move a row
procedure TNLDStringGrid.MoveRow(FromIndex, ToIndex: Integer);
begin
  MoveRowReadOnlyCells(FromIndex,ToIndex);
  inherited MoveRow(FromIndex, ToIndex);
end;

// can i select a cell?
function TNLDStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if (not (goEditing in Options)) or
     (Columns[ACol].FReadOnly)    or
     (ACol < FixedCols) or
     (ARow < FixedRows) or
     GetCellReadOnly(ACol,ARow) then
  begin
    result:=false;
    exit;
  end
  else
    result:=inherited SelectCell(Acol,Arow);
end;

// synch colcount property
procedure TNLDStringGrid.SetColCount(Value: Longint);
var
  i,j: Longint;
begin
  inherited ColCount:=Value;
  if FColCount <> Value then
  begin
    if FColCount < Value then
      for i := FColCount to Value - 1 do
        FColumns.Add
    else
      for i := FColCount - 1 downto Value do
        FColumns[i].Free;
    SetLength(FReadOnlyCells,Value,RowCount);
    if Value > FColCount then
      for i:=Value-1 downto FColCount do
        for j:=0 to rowcount-1 do
          if (i<FixedCols) or (j<FixedRows) then
            SetCellReadOnly(i,j,true)
          else
            SetCellReadOnly(i,j,false);
  end;
  FColCount:=Value;
end;

procedure TNLDStringGrid.SetColumns(const Value: TNLDStringColumns);
begin
  FColumns.Assign(Value);
end;

procedure TNLDStringGrid.SetFixedColWidth(const Value: Integer);
var
  i: integer;
begin
  for i:=0 to ColCount-1 do
    if i<FixedCols then
      ColWidths[i]:=Value;
  FFixedColWidth := Value;
  ColWidthsChanged;
  Invalidate;
end;

procedure TNLDStringGrid.SetFixedLineColor(const Value: TColor);
begin
  FFixedLineColor := Value;
  Invalidate;
end;

procedure TNLDStringGrid.SetFixedRowHeight(const Value: Integer);
var
  i: integer;
begin
  for i:=0 to RowCount-1 do
    if i<FixedRows then
      RowHeights[i]:=Value;
  FFixedRowHeight := Value;
  RowHeightsChanged;
  Invalidate;
end;

procedure TNLDStringGrid.SetGridLineColor(const Value: TColor);
begin
  FGridLineColor := Value;
  Invalidate;
end;

{procedure TNLDStringGrid.SetRowColor(AColor: TColor; ARow: Integer);
begin
  ShowMessage('Row Color Set');
end;}

procedure TNLDStringGrid.SetStretchRight(const Value: Boolean);
begin
  FStretchRight := Value;
  if Value then
    StretchRightCol;
end;

procedure TNLDStringGrid.ShowEditControl(acol, arow: integer);
var
 r:TRect;
begin

 r:=CellRect(acol,arow);
 case CurrEditStyle of
 etComboBox:
   begin
     editmode:=true;
     ComboBox.Style:=csDropDown;
     if (Columns[ACol].ComboBoxItems.IndexOf(Cells[ACol,ARow])= -1) and (Cells[ACol,ARow]<>'')then
       Columns[ACol].ComboBoxItems.add(Cells[ACol,ARow]);
     ComboBox.top:=r.top;
     ComboBox.left:=r.left;
     ComboBox.width:=r.right-r.left;
     ComboBox.height:=r.bottom-r.top+(Integer(Columns[ACol].ComboBoxDropDown+1))*ComboBox.itemheight;
     ComboBox.text:=self.cells[acol,arow];
     ComboBox.SizeDropDownWidth;
     ComboBox.enabled:=true;
     ComboBox.visible:=true;
     ComboBox.SetFocus;
   end;
 etComboBoxList:
   begin
     editmode:=true;
     ComboBox.Style:=csDropDownList;
     ComboBox.itemindex:=ComboBox.Items.IndexOf(cells[acol,arow]);
     if (ComboBox.itemindex=-1) then
       ComboBox.itemindex:=0;
     ComboBox.top:=r.top;
     ComboBox.left:=r.left;
     ComboBox.width:=r.right-r.left;
     ComboBox.height:=r.bottom-r.top+(Integer(Columns[ACol].ComboBoxDropDown+1))*ComboBox.itemheight;
     ComboBox.text:=self.cells[acol,arow];
     ComboBox.enabled:=true;
     ComboBox.visible:=true;
     ComboBox.SizeDropDownWidth;
     ComboBox.SetFocus;
   end;
 end;
end;

procedure TNLDStringGrid.ShowInplaceEdit;
begin
  ShowEditor;
end;

// stretch right column to right screen side
procedure TNLDStringGrid.StretchRightCol;
var
 i,tempWidth:integer;
begin
  if not FStretchRight then exit;
  tempWidth:=0;
  for i:=0 to FixedCols-1 do
    inc(tempWidth,ColWidths[i]+GridLineWidth);

  for i:=LeftCol to LeftCol+VisibleColCount-2 do
   begin
    tempWidth:=tempWidth+ColWidths[i]+GridLineWidth;
   end;

  if (ClientRect.Right>tempWidth) then
    ColWidths[ColCount-1]:=ClientRect.Right-tempWidth-GridLinewidth;
end;

// try to format a number or string cell
function TNLDStringGrid.FormatValidateCell(var s: string;ACol, ARow: integer): boolean;
var
  BackupInteger: integer;
  BackupReal: Real;
begin
  with Columns[ACol] do
  begin
    case TextType of
      ttAbsInteger:
      begin
        try
          BackupInteger:=StrToInt(s);
          s:=Format(FormatString,[BackupInteger]);
          result:=true;
        except
          result:=false;
        end;
      end;
      ttInteger:
      begin
        try
          BackupInteger:=StrToInt(s);
          s:=Format(FormatString,[BackupInteger]);
          result:=true;
        except
          result:=false;
        end;
      end;
      ttAbsReal:
      begin
        try
          BackupReal:=StrToFloat(s);
          s:=Format(FormatString,[BackupReal]);
          result:=true;
        except
          result:=false;
        end;
      end;
      ttReal:
      begin
        try
          BackupReal:=StrToFloat(s);
          s:=Format(FormatString,[BackupReal]);
          result:=true;
        except
          result:=false;
        end;
      end;
      else
        result:=true;
    end;
  end;
end;

// set the celltext of a cell
procedure TNLDStringGrid.SetEditText(ACol, ARow: Integer;
  const Value: string);
var
  NewValue: string;
begin
  NewValue:=Value;
  if not(Columns[ACol].EditStyle in [etEdit]) then
    inherited SetEditText(ACol, ARow, NewValue)
  else
    if FormatValidateCell(NewValue,ACol,ARow) then
      inherited SetEditText(ACol, ARow, NewValue);
end;

//set the row count
procedure TNLDStringGrid.SetRowCount(Value: Integer);
var
  i,j: Longint;
begin
  inherited RowCount:=Value;
  if FRowCount <> Value then
  begin
    SetLength(FReadOnlyCells,ColCount,Value);
    if Value > FRowCount then
      for i:=0 to ColCount-1 do
        for j:=Value-1 downto FRowCount do
          if (i<FixedCols) or (j<FixedRows) then
            SetCellReadOnly(i,j,true)
          else
            SetCellReadOnly(i,j,false);
  end;
  FRowCount:=Value;
end;

//triggered when gridsize is changed
procedure TNLDStringGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
 if (parent=nil) then exit;
 inherited SizeChanged(OldColCount,OldRowCount);
 if FStretchRight then
   StretchRightCol;
end;

//check if designated cell is readonly
function TNLDStringGrid.GetCellReadOnly(ACol, ARow: integer): boolean;
begin
  Result:=FReadOnlyCells[ACol,ARow];
end;

//check if designated col is readonly
function TNLDStringGrid.GetColReadOnly(ACol: integer): boolean;
var
  i: integer;
begin
  result:=true;
  for i:=RowCount-1 downto FixedRows do
    if not GetCellReadOnly(ACol,i) then
    begin
      result:=false;
      exit;
    end;
end;

//check if designated row is readonly
function TNLDStringGrid.GetRowReadOnly(ARow: integer): boolean;
var
  i: integer;
begin
  result:=true;
  for i:=ColCount-1 downto FixedCols do
    if not GetCellReadOnly(i,ARow) then
    begin
      result:=false;
      exit;
    end;
end;

//set the cell readonly
procedure TNLDStringGrid.SetCellReadOnly(ACol, ARow: integer;
  Value: boolean);
begin
  FReadOnlyCells[ACol,ARow]:=Value;

  InvalidateCell(ACol,ARow);
end;

// set the col readonly
procedure TNLDStringGrid.SetColReadOnly(ACol: integer; Value: boolean);
var
  RowCounter,ColCounter,i: integer;
  StopSearching: boolean;
begin
  RowCounter:=0;
  ColCounter:=0;
  StopSearching:=False;
  for i:=RowCount-1 downto FixedRows do
    SetCellReadOnly(ACol,i,Value);
  while (ColCounter < ColCount) and not StopSearching do
    while (RowCounter < RowCount) and not StopSearching do
      StopSearching:=FReadOnlyCells[ColCounter,RowCounter];
  InvalidateCol(ACol);
end;

//set the row readonly
procedure TNLDStringGrid.SetRowReadOnly(ARow: integer; Value: boolean);
var
  RowCounter, ColCounter, i: integer;
  StopSearching: boolean;
begin
  RowCounter:=0;
  ColCounter:=0;
  StopSearching:=False;
  for i:=ColCount-1 downto FixedCols do
    SetCellReadOnly(i,ARow,Value);
  while (ColCounter < ColCount) and not StopSearching do
    while (RowCounter < RowCount) and not StopSearching do
      StopSearching:=FReadOnlyCells[ColCounter,RowCounter];
  InvalidateRow(ARow);
end;

//insert a column
procedure TNLDStringGrid.InsertCol(IsBlank: Boolean; InsertPos: TInsertPos);
var
  i: integer;
begin
  ColCount:=ColCount+1;
  case InsertPos of
  ipAfter:   begin
               moveColumn(ColCount-1,Col+1);
               for i:=0 to ColCount-1 do
                 if IsBlank then
                   Cells[i,Col+1]:=' ';
              end;
  ipBefore:  begin
               moveColumn(ColCount-1,Col);
               for i:=0 to ColCount-1 do
                 if IsBlank then
                   Cells[i,Col]:=' ';
              end;
  ipBegin:   begin
               moveColumn(ColCount-1,FixedCols);
               if IsBlank then
                 for i:=0 to ColCount-1 do
                   Cells[i,FixedCols]:=' ';
              end;
  ipEnd:     begin
                if IsBlank then
                  for i:=0 to ColCount-1 do
                    Cells[i,ColCount-1]:=' ';
              end;
  end;
end;

//move a column of read only cells
procedure TNLDStringGrid.MoveColReadOnlyCells(FromIndex, ToIndex: Integer);
var
  i,j: integer;
  TempRecord: TNLDReadOnlyRow;
begin
  SetLength(TempRecord,RowCount);
  for i:=0 to high(TempRecord) do
    TempRecord[i]:=FReadOnlyCells[FromIndex][i];
  if ToIndex > FromIndex then
    for j:=FromIndex+1 to ToIndex do
      for i:=0 to RowCount-1 do
        FReadOnlyCells[j-1][i]:= FReadOnlyCells[j][i]
  else
    for j:=FromIndex-1 downto ToIndex do
      for i:=0 to RowCount-1 do
        FReadOnlyCells[j+1][i]:= FReadOnlyCells[j][i];
  for j:=0 to RowCount-1 do
    FReadOnlyCells[ToIndex][j]:= TempRecord[j];
end;

//move a row of readonly cells
procedure TNLDStringGrid.MoveRowReadOnlyCells(FromIndex, ToIndex: Integer);
var
  i,j: integer;
  TempRecord: TNLDReadOnlyRow;
begin
  SetLength(TempRecord,ColCount);
  for i:=0 to high(TempRecord) do
    TempRecord[i]:=FReadOnlyCells[i][FromIndex];
  if ToIndex > FromIndex then
    for i:=FromIndex to ToIndex-1 do
      for j:=0 to Colcount-1 do
        FReadOnlyCells[j][i]:= FReadOnlyCells[j][i+1]
  else
    for i:=FromIndex downto ToIndex+1 do
      for j:=0 to Colcount-1 do
        FReadOnlyCells[j][i]:= FReadOnlyCells[j][i-1];
  for j:=0 to Colcount-1 do
        FReadOnlyCells[j][ToIndex]:= TempRecord[j];
end;

//is the row empty?
function TNLDStringGrid.RowIsEmpty(ARow: Integer): Boolean;
var
  i: Integer;
begin
// kijk of rij leeg is. als er een cell in de rij niet leeg is dan result:=False en stoppen...
  Result:=True;
  for i:=fixedCols to Colcount-1 do
    if Trim(Cells[i,ARow])<>'' then
    begin
      Result:=False;
      Exit;
    end;
end;

procedure TNLDStringGrid.SetReadOnlyColor(const Value: TColor);
begin
  FReadOnlyColor := Value;
  Invalidate;
end;

{ TNLDInplaceEdit }

constructor TNLDInplaceEdit.Create(AOwner: TComponent);
begin
  ITStringColumnItem:=(AOwner as TNLDStringGrid).Columns[(AOwner as TNLDStringGrid).col];
  inherited Create(AOwner);
end;

procedure TNLDInplaceEdit.CreateWnd;
begin
 inherited CreateWnd;
 if (FMaxLength=0) then FMaxLength:=ITStringColumnItem.MaxLength;
end;

//check if the key pressed is to be accepted
procedure TNLDInplaceEdit.WMChar(var Msg: TWMKey);
begin
 if (ITStringColumnItem.MaxLength>0) and (length(text)=ITStringColumnItem.MaxLength) and (sellength=0)
    and (msg.charcode<>vk_back) and (msg.charcode<>vk_Escape) then
   exit;
 if ITStringColumnItem.EditStyle=etEdit then
   case ITStringColumnItem.TextType of
     ttString:         inherited;
     ttInteger:        if (msg.charcode in NUMERIC_CHAR) and
                           not ((msg.charcode=ord('-')) and (pos('-',text)>0)) and
                           not ((msg.charcode=ord('-')) and (selstart<>0)) then
                         inherited
                       else
                         messagebeep(0);
     ttAbsInteger:     if msg.charcode in ABS_NUMERIC_CHAR then
                         inherited
                       else
                         messagebeep(0);
     ttReal:           begin
                         if ((msg.charcode in NUMERIC_CHAR) or (msg.charcode = ord(DecimalSeparator))) and
                           not ((msg.charcode=ord('-')) and (pos('-',text)>0)) and
                           not ((msg.charcode=ord('-')) and (selstart<>0)) then
                         begin
                           if (msg.charcode=ord(DecimalSeparator)) and
                              ( ((pos(DecimalSeparator,self.text)>0) and (pos(DecimalSeparator,self.SelText)=0))) then
                           begin
                             messagebeep(0);
                             exit;
                           end;
                           inherited;
                         end;
                       end;
     ttAbsReal:        begin
                         if ((msg.charcode in ABS_NUMERIC_CHAR) or (msg.charcode = ord(DecimalSeparator))) then
                         begin
                           if (msg.charcode=ord(DecimalSeparator)) and
                              ( ((pos(DecimalSeparator,self.text)>0) and (pos(DecimalSeparator,self.SelText)=0))) then
                             exit;
                           if (msg.charcode=ord('-')) and
                               ((selstart<>0) or (pos('-',self.text)>0)) then
                           begin
                             messagebeep(0);
                             exit;
                           end;
                           inherited;
                         end;
                       end;
   end
 else
   inherited;
end;

//make the cursor appear in the exact spot of the click
procedure TNLDInplaceEdit.WMSetFocus(var Msg: TWMSetFocus);
var
 lpPoint:tPoint;
 i:integer;
begin
 ITStringColumnItem:=(Owner as TNLDStringGrid).Columns[(Owner as TNLDStringGrid).col];
 inherited;
 GetCursorPos(lpPoint);
 lpPoint:=screentoclient(lpPoint);

 if (lpPoint.x<0) or (lpPoint.y<0) or
    (lpPoint.x>width) or (lpPoint.y>height) then exit;

 i:=SendMessage(self.handle,EM_CHARFROMPOS, 0,makelong(lpPoint.x,lpPoint.y));
 if (i=-1) then exit;

 selstart:=loword(i);
 sellength:=0;

end;

{ TNLDPopupCombobox }

procedure TNLDInplaceComboBox.Change;
begin
  inherited change;
  doexit;
end;

constructor TNLDInplaceComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Grid:= AOwner as TNLDStringGrid;
  visible:=false;
  enabled:=false;
end;

procedure TNLDInplaceCombobox.CreateWnd;
begin
  inherited CreateWnd;
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TNLDInplaceComboBox.DoExit;
begin
 Grid.HideInplaceEdit;
 Grid.SetEditText(Grid.Col,Grid.Row,text);
 inherited DoExit;
end;

//check inplace edit ket presses
procedure TNLDInplaceComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
 condition:boolean;
 comboselstate:boolean;
begin
 if (style = csDropDownList) or (DroppedDown) then
   condition:=key in [vk_left,vk_right,vk_prior,vk_next,vk_end,vk_home,vk_escape,vk_tab,vk_return]
 else
   condition:=key in [vk_down,vk_up,vk_prior,vk_next,vk_end,vk_home,vk_escape,vk_tab,vk_return];
 comboselstate:=(style = csDropDownList) or (DroppedDown);

 if (condition) then
  begin
    if (key=vk_escape) then self.text:=grid.cells[grid.col,grid.row];

    if (key in [vk_return,vk_up,vk_down]) then
     begin
      grid.cells[grid.col,grid.row]:=self.Text;
     end;

    grid.FBlockFocus:=true;
    grid.HideInplaceEdit;

    if (comboselstate) then
     begin
      if key in [vk_left,vk_right,vk_tab] then grid.keydown(key,shift)
     end
    else
     begin
      if key in [vk_up,vk_down,vk_tab] then grid.keydown(key,shift);
     end;

    grid.SetFocus;

    grid.fBlockFocus:=false;
  end
 else
  inherited;
end;

//calculate the needed size for combobox items.
procedure TNLDInplaceComboBox.SizeDropDownWidth;
var
 i:integer;
 tw,nw:integer;
begin
 tw:=self.Width;

 for i:=1 to self.Items.Count do
  begin
   nw:=5+GetSystemMetrics(SM_CXVSCROLL)+Grid.canvas.textwidth(self.items[i-1]); {account for border size?}
   if (nw>tw) then tw:=nw;
  end;
 SendMessage(self.handle,CB_SETDROPPEDWIDTH,tw,0);
end;

procedure TNLDStringGrid.SetFocusRectColor(const Value: TColor);
begin
  FFocusRectColor := Value;
end;

procedure TNLDStringGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if FStretchRight then
    StretchRightCol;
end;

procedure TNLDStringGrid.WMSize(var Msg: TWMKey);
begin

end;

end.
