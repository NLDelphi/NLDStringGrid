(*******************************************************************************
Om het maar even in de stijl van de oorspronkelijk auteur te zeggen:

Hier is 'tie dan:

NLDStringGrid v1.1

TNLDStringGrid:
===============
Toegevoegde functionaliteit:
 - Zonder properties in te stellen werkt het grid exact gelijk aan de
   standaard TStringGrid
 - Overal kunnen de Color-property's op clDefault gezet worden, waarmee ze
   weer de standaardkleur krijgen
 - Merging: samenvoegen van cellen
 - Merged InplaceEdit(List) met font en achtergrondkleur van betreffende cell

Toegevoegde properties en events:
 - AlternatingRowColors:
   - EvenRowColor
   - IncludeFixed
   - OddRowColor
   - OverrideColumnColor
 - DefaultDrawing (reintroduced: t.b.v. descendants)
 - FixedFont
 - FocusRectColor
 - FocusRectStyle
 - OnTitleClick
 - SelectionAlphaBlend
 - SelectionAlphaBlendValue: Byte
 - SelectionColor
 - StretchModes: (StretchRight, StretchAll, ShrinkRight, ShrinkAll)
 - SyncColumns: Boolean (case True: ColCount = Columns.Count
                         case False: ColCount >= Columns.Count)
 - Values[Col, Row]: Double

Toegevoegde Methods:
 - MergeCells(AGridRect: TGridRect; MergeText: Boolean; MultiLine: Boolean)
 - UnMergeCells(AGridRect: TGridRect)

Aangepaste properties en methods:
 - FixedLineColor (nieuwe naam: FixedGridLineColor)
 - OnDrawCell (parameter Column aan toegevoegd)
 - GridLineWidth (reintroduced)
 - StretchRight (dat is nu StretchModes geworden)

Uitgeschakelde properties en methods (hier wist ik geen raad mee):
 - BidiMode
 - ParentBidiMode

TStringGridColumn:
==================
Toegevoegde properties:
 - EditMask
 - Fixed: Boolean
 - MinWidth: Integer
 - MaxWidth: Integer
 - RowNumbers: Boolean
 - Visible: Boolean

Opgeloste bugs:
===============
 - Zwarte fixed-cellen bij ReadOnly
 - Breedte eerste kolom
 - Toevoegen, verwijderen en verplaatsen van Columns in Collection-editor
 - Niet meeverplaatsen van Cell-inhoud bij verplaatsen van Columns of Rows
 - Ctl3D bij fixed cellen
 - Alle options werken
 - Overbodige property opslag in DFM
 - Opslag en terughalen van strings in eerder verwijderde columns/rows (maar
   dit kan ook een bewuste feature zijn geweest van Borland in TStringGrid,
   heeft te maken met opslag van de data: Ik gebruik een eenvoudig twee-
   dimensioneel array (= inefficient bij veel wijzigingen van het aantal rows
   of columns), TStringGrid doet opslag op, door een TSparseList bijgehouden,
   directe geheugenlocaties. De keuze is ook afhankelijk van het feit dat ik
   meer data per cell wilde bewaren dan slechts een string en een object. Evt.
   zou je het standaard object van een cell kunnen toewijzen aan een eigen
   dataclasse, maar dat leek me ingewikkelder.)
 - FixedLineColor
 - FocusRectColor (dat was geen echte bug, omdat het FocusRect van Windows niet
   in een vaste kleur kan. Windows berekent de kleur a.d.h.v. de achtergrond.
   Wel zijn er twee nieuwe FocusRectStyles bijgekomen waarbij je een kleur
   kunt opgeven.)
 - FocusRect bij alle geselecteerde cellen bij DrawFocusSelected = False

Nieuwe bugs:
============


TODO:
=====
 - Hint voor afgekapte cellteksten
 - AutoMerge (bij te lange tekst, automatisch de volgende cell erbij pakken)
 - AutoColWidths (aan tekstbreedte)
 - Printen


Heb je iets te vragen, gooi het hier neer.
Alle ervaringen suggesties en bugreports/fixes zjn welkom!!

*****************************************************************************)

// - berkenen van de clHighLight color ???


unit UStringGridEx;

///////////////////////////////////////////////////////////
//  With special thanks to walterheck:                   //
//  http://www.nldelphi.com/forum/forumdisplay.php?f=43  //
///////////////////////////////////////////////////////////

interface

uses
  Classes, Grids, Windows, Graphics, Controls, Messages, MaskUtils;

type
  TStringGridColumn = class;
  TStringGridColumns = class;
  TStringGridEx = class;

  TVAlignment = (tvaTop, tvaCenter, tvaBottom);

  TStringGridTitle = class(TPersistent)
  private
    FAlignment: TAlignment;
    FCaption: String;
    FColor: TColor;
    FColumn: TStringGridColumn;
    FFont: TFont;
    FGrid: TStringGridEx;
    FMultiLine: Boolean;
    FVAlignment: TVAlignment;
    function GetColor: TColor;
    function GetHeight: Integer;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    procedure Changed(const AllColumns: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: String);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetHeight(const Value: Integer);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetVAlignment(const Value: TVAlignment);
  protected
    procedure FontChanged(Sender: TObject); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AColumn: TStringGridColumn);
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property Caption: String read FCaption write SetCaption stored True;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Height: Integer read GetHeight write SetHeight stored False;
    property MultiLine: Boolean read FMultiLine write SetMultiLine
      default False;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment
      default tvaTop;
  end;

  TInputStyle = (isString, isInteger, isAbsInteger, isFloat, isAbsFloat,
    isMask);

//  TEditStyle = (esSimple, esEllipse, esPickList, esPickListOnly);

  TStringGridColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FColor: TColor;
    FColumns: TStringGridColumns;
    FDefaultCellText: String;
    FEditFormat: String;
    FEditMask: TEditMask;
    FEditStyle: TEditStyle;
    FFont: TFont;
    FGrid: TStringGridEx;
    FInputStyle: TInputStyle;
    FMaxLength: Integer;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FMultiLine: Boolean;
    FPickListItems: TStrings;
    FPickListOnly: Boolean;
    FReadOnly: Boolean;
    FRowNumbers: Boolean;
    FTitle: TStringGridTitle;
    FVAlignment: TVAlignment;
    function GetColor: TColor;
    function GetEditFormat: String;
    function GetFixed: Boolean;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function IsColorStored: Boolean;
    function IsEditFormatStored: Boolean;
    function IsFontStored: Boolean;
    function IsPickListItemsStored: Boolean;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetColor(const Value: TColor);
    procedure SetDefaultCellText(const Value: String);
    procedure SetEditFormat(const Value: String);
    procedure SetEditMask(const Value: TEditMask);
    procedure SetFixed(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetInputStyle(const Value: TInputStyle);
    procedure SetMaxLength(const Value: Integer);
    procedure SetMaxWidth(const Value: Integer);
    procedure SetMinWidth(const Value: Integer);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetPickListItems(const Value: TStrings);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRowNumbers(const Value: Boolean);
    procedure SetTitle(const Value: TStringGridTitle);
    procedure SetVAlignment(const Value: TVAlignment);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    Destroying: Boolean;
    procedure FontChanged(Sender: TObject); virtual;
    function GetDisplayName: String; override;
    procedure SetIndex(Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Grid: TStringGridEx read FGrid;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property DefaultCellText: String read FDefaultCellText
      write SetDefaultCellText;
    property EditFormat: String read GetEditFormat write SetEditFormat
      stored IsEditFormatStored;
    property EditMask: TEditMask read FEditMask write SetEditMask;
    property EditStyle: TEditStyle read FEditStyle write FEditStyle
      default esSimple;
    property Fixed: Boolean read GetFixed write SetFixed stored False;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property InputStyle: TInputStyle read FInputStyle write SetInputStyle
      default isString;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property MultiLine: Boolean read FMultiLine write SetMultiLine
      default False;
    property PickListItems: TStrings read FPickListItems
      write SetPickListItems stored IsPickListItemsStored;
    property PickListOnly: Boolean read FPickListOnly write FPickListOnly
      default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RowNumbers: Boolean read FRowNumbers write SetRowNumbers
      default False;
    property Title: TStringGridTitle read FTitle write SetTitle;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment
      default tvaTop;
    property Visible: Boolean read GetVisible write SetVisible stored False;
    property Width: Integer read GetWidth write SetWidth stored False;
  end;

  TStringGridColumns = class(TCollection)
  private
    FGrid: TStringGridEx;
  protected
    function GetItem(Index: Integer): TStringGridColumn;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
    procedure SetItem(Index: Integer; const Value: TStringGridColumn);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGrid: TStringGridEx);
    property Items[Index: Integer]: TStringGridColumn read GetItem
      write SetItem; default;
  published
    property Grid: TStringGridEx read FGrid;
  end;

  TStringGridStrings = class(TStrings)
  private
    FGrid: TStringGridEx;
    FIndex: Integer;
    FIsCol: Boolean;
    function LinkGridCol(const AIndex: Integer): TStrings;
    function LinkGridRow(const AIndex: Integer): TStrings;
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    function Add(const S: String): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    constructor Create(AGrid: TStringGridEx);
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
  end;

  TStringGridRowColors = class(TPersistent)
  private
    FEvenRowColor: TColor;
    FGrid: TStringGridEx;
    FIncludeFixed: Boolean;
    FOddRowColor: TColor;
    FOverrideColumnColor: Boolean;
    function GetEvenRowColor: TColor;
    function GetOddRowColor: TColor;
    function IsEvenRowColorStored: Boolean;
    function IsOddRowColorStored: Boolean;
    function IsStored: Boolean;
    procedure SetEvenRowColor(const Value: TColor);
    procedure SetIncludeFixed(const Value: Boolean);
    procedure SetOddRowColor(const Value: TColor);
    procedure SetOverrideColumnColor(const Value: Boolean);
  public
    constructor Create(AGrid: TStringGridEx);
    procedure Assign(Source: TPersistent); override;
  published
    property EvenRowColor: TColor read GetEvenRowColor write SetEvenRowColor
      stored IsEvenRowColorStored;
    property IncludeFixed: Boolean read FIncludeFixed write SetIncludeFixed
      default False;
    property OddRowColor: TColor read GetOddRowColor write SetOddRowColor
      stored IsOddRowColorStored;
    property OverrideColumnColor: Boolean read FOverrideColumnColor
      write SetOverrideColumnColor default True;
  end;

  TCellData = record
    FMerged: Boolean;
    FMergeCol: Integer;
    FMergeRow: Integer;
    FMergeText: Boolean;
    FMergeML: Boolean;
    FObject: TObject;
    FReadOnly: Boolean;
    FString: String;
    FValue: Double;
  end;

  //For custom drawing in future descendants
  TDefaultDrawingMode = (ddBackground, ddEdges, ddGridLines, ddText,
    ddFocusRect);

  TDefaultDrawingModes = set of TDefaultDrawingMode;

  TStretchMode = (smAllowStretchRight, smAllowStretchAll, smAllowShrinkRight,
    smAllowShrinkAll);

  TStretchModes = set of TStretchMode;

  TInsertPos = (ipBefore, ipAfter, ipBegin, ipEnd);

  TDeletePos = (dpCurrent, dpBegin, dpEnd, dpRange, dpSelection, dpAll);

  TDrawCellEvent = procedure (Sender: TObject; ACol, ARow: Integer;
    ARect: TRect; State: TGridDrawState; Column: TStringGridColumn) of object;

  TTitleClickEvent = procedure (Sender: TObject; const Index: Integer;
    Column: TStringGridColumn) of object;

  TFocusRectStyle = (frDefault, frSolidAutoBW, frSolidCustomColor);

  TInplaceEditListEx = class(TInplaceEditList)
  private
    PickListOnly: Boolean;
    function Grid: TStringGridEx;
    function PickListItemIndexOf(const Value: String): Integer;
    procedure WMWindowPosChanging(var Message: TWMWindowPosMsg);
      message WM_WINDOWPOSCHANGING;
  protected
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
  end;

  TStringGridEx = class(TCustomDrawGrid)
  private
    FCellData: array of array of TCellData;
    FColumns: TStringGridColumns;
    FColRowStrings: TStringGridStrings;
    FDefaultDrawing: TDefaultDrawingModes;
    FFixedFont: TFont;
    FFixedFontBackup: TFont;
    FFixedGridLineColor: TColor;
    FFocusRectColor: TColor;
    FFocusRectStyle: TFocusRectStyle;
    FFontBackup: TFont;
    FGridLineColor: TColor;
    FOnDrawCell: TDrawCellEvent;
    FOnTitleClick: TTitleClickEvent;
    FReadOnlyColor: TColor;
    FRowColors: TStringGridRowColors;
    FSelectionAlphaBlend: Boolean;
    FSelectionAlphaBlendValue: Byte;
    FSelectionColor: TColor;
    FStretchModes: TStretchModes;
    FSyncColumns: Boolean;
    function CanMergedEditShow: Boolean;
    procedure ChangeEditFormat(const ACol: Integer;
      const OldFormat, NewFormat: String);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ExchangeCellData(const FromCol, FromRow, ToCol, ToRow: Integer);
    procedure FixedFontChanged(Sender: TObject);
    function GetCells(ACol, ARow: Integer): String;
    function GetCellsReadOnly(ACol, ARow: Integer): Boolean;
    function GetColCount: Integer;
    function GetCols(Index: Integer): TStrings;
    function GetFixedCols: Integer;
    function GetFixedRows: Integer;
    function GetGridLineWidth: Integer;
    function GetObjects(ACol, ARow: Integer): TObject;
    function GetReadOnlyColor: TColor;
    function GetRows(Index: Integer): TStrings;
    function GetValues(ACol, ARow: Integer): Double;
    function InGrid(const ACol, ARow: Integer): Boolean;
    procedure InvalidateGridRect(const AGridRect: TGridRect);
    function IsAlternatingRowColorsStored: Boolean;
    function IsColumnsStored: Boolean;
    function IsFixedFontStored: Boolean;
    function IsReadOnlyColorStored: Boolean;
    procedure MergeCell(const ACol, ARow: Integer;
      const MergeCoord: TGridCoord; const MergeText, MultiLine: Boolean);
    procedure SetCells(ACol, ARow: Integer; const Value: String);
    procedure SetCellsReadOnly(ACol, ARow: Integer; const Value: Boolean);
    procedure SetCellValue(const ACol, ARow: Integer; const Value: String);
    procedure SetColCount(const Value: Integer);
    procedure SetColumns(const Value: TStringGridColumns);
    procedure SetDefaultDrawing(const Value: TDefaultDrawingModes);
    procedure SetFixedCols(const Value: Integer);
    procedure SetFixedFont(const Value: TFont);
    procedure SetFixedGridLineColor(const Value: TColor);
    procedure SetFixedRows(const Value: Integer);
    procedure SetFocusRectColor(const Value: TColor);
    procedure SetFocusRectStyle(const Value: TFocusRectStyle);
    procedure SetGridLineColor(const Value: TColor);
    procedure SetGridLineWidth(const Value: Integer);
    procedure SetObjects(ACol, ARow: Integer; const Value: TObject);
    procedure SetReadOnlyColor(const Value: TColor);
    procedure SetRowColors(const Value: TStringGridRowColors);
    procedure SetSelectionAlphaBlend(const Value: Boolean);
    procedure SetSelectionAlphaBlendValue(const Value: Byte);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetStretchModes(Value: TStretchModes);
    procedure SetSyncColumns(const Value: Boolean);
    procedure SetValues(ACol, ARow: Integer; const Value: Double);
    procedure UnMergeCell(const ACol, ARow: Integer);
    procedure UpdateColumn(const Index: Integer);
    procedure UpdateColumns;
    procedure UpdateColWidths;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure AdjustSize; reintroduce;
    function CanEditAcceptKey(Key: Char): Boolean; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    procedure ColumnMoved(FromIndex, ToIndex: Integer); override;
    procedure ColWidthsChanged; override;
    function CreateEditor: TInplaceEdit; override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState); override;
    function GetCellColor(const ACol, ARow: Integer;
      const AState: TGridDrawState; Column: TStringGridColumn;
      const CellData: TCellData): TColor;
    function GetCellData(const ACol, ARow: Integer): TCellData;
    procedure GetCellFont(AFont: TFont; const ARow: Integer;
      const AState: TGridDrawState; Column: TStringGridColumn);
    function GetEditLimit: Integer; override;
    function GetEditMask(ACol: Integer; ARow: Integer): String; override;
    function GetEditStyle(ACol: Integer; ARow: Integer): TEditStyle;
      override;
    function GetEditText(ACol: Integer; ARow: Integer): String; override;
    function GetMergeRect(const ACol, ARow: Integer): TGridRect;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure Paint; override;
    procedure RowMoved(FromIndex, ToIndex: Integer); override;
    procedure SetEditText(ACol: Integer; ARow: Integer;
      const Value: String); override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeleteColumn(ACol: Integer); override;
    procedure DeleteRows(const DeletePos: TDeletePos; StartIndex: Integer = -1;
      const Count: Integer = 1);
    destructor Destroy; override;
    procedure InsertColumn(const AtIndex: Integer); overload;
    procedure InsertColumn(const Position: TInsertPos = ipBefore); overload;
    procedure InsertRow(const AtIndex: Integer); overload;
    procedure InsertRow(const Position: TInsertPos = ipBefore); overload;
    function IsEmptyColumn(const ACol: Integer): Boolean;
    function IsEmptyRow(const ARow: Integer): Boolean;
    procedure MergeCells(const AGridRect: TGridRect;
      const MergeText: Boolean = True; const MultiLine: Boolean = False);
    procedure MoveColumn(const FromIndex, ToIndex: Integer);
    procedure MoveRow(const FromIndex, ToIndex: Integer);
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure SetGradientColumnColors(const First, Last: TColor;
      const ColumnsOnly: Boolean);
    procedure UnMergeCells(const AGridRect: TGridRect);
    property Cells[ACol, ARow: Integer]: String read GetCells write SetCells;
    property CellsReadOnly[ACol, ARow: Integer]: Boolean read GetCellsReadOnly
      write SetCellsReadOnly;
    property Cols[Index: Integer]: TStrings read GetCols;
    property Objects[ACol, ARow: Integer]: TObject read GetObjects
      write SetObjects;
    property Rows[Index: Integer]: TStrings read GetRows;
    property Values[ACol, ARow: Integer]: Double read GetValues
      write SetValues;
  published
    property AlternatingRowColors: TStringGridRowColors read FRowColors
      write SetRowColors stored IsAlternatingRowColorsStored;
    property Columns: TStringGridColumns read FColumns write SetColumns
      stored IsColumnsStored;
    property ColCount: Integer read GetColCount write SetColCount default 5;
    property DefaultDrawing: TDefaultDrawingModes read FDefaultDrawing
      write SetDefaultDrawing default [ddBackground, ddEdges, ddGridLines,
      ddText, ddFocusRect];
    property FixedCols: Integer read GetFixedCols write SetFixedCols default 1;
    property FixedFont: TFont read FFixedFont write SetFixedFont
      stored IsFixedFontStored;
    property FixedGridLineColor: TColor read FFixedGridLineColor
      write SetFixedGridLineColor default clBlack;
    property FixedRows: Integer read GetFixedRows write SetFixedRows default 1;
    property FocusRectColor: TColor read FFocusRectColor
      write SetFocusRectColor default clDefault;
    property FocusRectStyle: TFocusRectStyle read FFocusRectStyle
      write SetFocusRectStyle default frDefault;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor
      default clSilver;
    property GridLineWidth: Integer read GetGridLineWidth
      write SetGridLineWidth default 1;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnTitleClick: TTitleClickEvent read FOnTitleClick
      write FOnTitleClick;
    property ReadOnlyColor: TColor read GetReadOnlyColor write SetReadOnlyColor
      stored IsReadOnlyColorStored;
    property SelectionAlphaBlend: Boolean read FSelectionAlphaBlend
      write SetSelectionAlphaBlend default True;
    property SelectionAlphaBlendValue: Byte read FSelectionAlphaBlendValue
      write SetSelectionAlphaBlendValue default 80;
    property SelectionColor: TColor read FSelectionColor
      write SetSelectionColor default clHighLight;
    property StretchModes: TStretchModes read FStretchModes
      write SetStretchModes default [];
    property SyncColumns: Boolean read FSyncColumns write SetSyncColumns
      default False;
    //Inherited:
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property Options default [goFixedVertLine, goFixedHorzLine, goVertLine,
      goHorzLine, goRangeSelect, goThumbTracking];
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
  end;

procedure Register;

implementation

uses
  Math, SysUtils, StdCtrls, Forms;

procedure Register;
begin
  RegisterComponents('Albert', [TStringGridEx]);
end;

type
  TRGB = record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

const
  sInvalidStringGridStringsOp =
    'Cannot insert or delete within grid rows or columns this way';
  sInvalidStringGridColumnOwnerType =
    'Owner of Column has be of type TStringGridColumns';
  sInvalidStringGridColumnOwner =
    'Owner of Column has to be assigned';

function GetRGB(AColor: TColor): TRGB;
begin
  AColor := ColorToRGB(AColor);
  Result.R := GetRValue(AColor);
  Result.G := GetGValue(AColor);
  Result.B := GetBValue(AColor);
end;

function MixColor(const Base, MixWith: TColor; const Factor: Double): TColor;
var
  FBase, FMixWith: TRGB;
begin
  if (Factor < 0) or (Factor > 1) then
  begin
    Result := Base;
    Exit;
  end;
  FBase := GetRGB(Base);
  FMixWith := GetRGB(MixWith);
  with FBase do
  begin
    R := R + Round((FMixWith.R - R) * Factor);
    G := G + Round((FMixWith.G - G) * Factor);
    B := B + Round((FMixWith.B - B) * Factor);
    Result := RGB(R, G, B);
  end;
end;

function SameFont(Font1, Font2: TFont): Boolean;
begin
  Result := (Font1.Color = Font2.Color) and (Font1.Handle = Font2.Handle);
end;

{ TStringGridTitle }

procedure TStringGridTitle.Assign(Source: TPersistent);
begin
  if Source is TStringGridTitle then
  begin
    if Assigned(FColumn) and Assigned(FColumn.Collection) then
      FColumn.Collection.BeginUpdate;
    try
      FAlignment := TStringGridTitle(Source).FAlignment;
      FCaption := TStringGridTitle(Source).FCaption;
      FColor := TStringGridTitle(Source).FColor;
      FFont.Assign(TStringGridTitle(Source).FFont);
      FMultiLine := TStringGridTitle(Source).FMultiLine;
      FVAlignment := TStringGridTitle(Source).FVAlignment;
    finally
      if Assigned(FColumn) and Assigned(FColumn.Collection) then
        FColumn.Collection.EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TStringGridTitle.Changed(const AllColumns: Boolean);
begin
  if Assigned(FColumn) then
    FColumn.Changed(AllColumns);
end;

constructor TStringGridTitle.Create(AColumn: TStringGridColumn);
begin
  inherited Create;
  FColumn := AColumn;
  FColor := clDefault;
  FFont := TFont.Create;
  if Assigned(FColumn) then
    FGrid := FColumn.Grid;
  if Assigned(FGrid) then
    FFont.Assign(FGrid.FixedFont);
  FFont.OnChange := FontChanged;
end;

destructor TStringGridTitle.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TStringGridTitle.FontChanged(Sender: TObject);
begin
  Changed(False);
end;

function TStringGridTitle.GetColor: TColor;
begin
  if (FColor = clDefault) and Assigned(FGrid) then
    Result := FGrid.FixedColor
  else
    Result := FColor;
end;

function TStringGridTitle.GetHeight: Integer;
begin
  if Assigned(FGrid) then
    Result := FGrid.RowHeights[0]
  else
    Result := -1;
end;

function TStringGridTitle.IsColorStored: Boolean;
begin
  Result := FColor <> clDefault;
end;

function TStringGridTitle.IsFontStored: Boolean;
begin
  if Assigned(FGrid) then
    Result := not SameFont(FFont, FGrid.FixedFont)
  else
    Result := False;
end;

procedure TStringGridTitle.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TStringGridTitle.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if FCaption = '' then
      FCaption := ' '; //Due to no designtime-storage if empty. IDE bug?
    Changed(False);
  end;
end;

procedure TStringGridTitle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TStringGridTitle.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TStringGridTitle.SetHeight(const Value: Integer);
begin
  if Assigned(FGrid) then
    FGrid.RowHeights[0] := Value
  else
    raise EInvalidGridOperation.Create(sInvalidStringGridColumnOwner);
end;

procedure TStringGridTitle.SetMultiLine(const Value: Boolean);
begin
  if FMultiLine <> Value then
  begin
    FMultiLine := Value;
    Changed(False);
  end;
end;

procedure TStringGridTitle.SetVAlignment(const Value: TVAlignment);
begin
  if FVAlignment <> Value then
  begin
    FVAlignment := Value;
    Changed(False);
  end;
end;

{ TStringGridColumn }

procedure TStringGridColumn.Assign(Source: TPersistent);
begin
  if Source is TStringGridColumn then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      FAlignment := TStringGridColumn(Source).FAlignment;
      FColor := TStringGridColumn(Source).FColor;
      FDefaultCellText := TStringGridColumn(Source).FDefaultCellText;
      FEditFormat := TStringGridColumn(Source).FEditFormat;
      FEditMask := TStringGridColumn(Source).FEditMask;
      FEditStyle := TStringGridColumn(Source).FEditStyle;
      FFont.Assign(TStringGridColumn(Source).FFont);
      FInputStyle := TStringGridColumn(Source).FInputStyle;
      FMaxLength := TStringGridColumn(Source).FMaxLength;
      FMaxWidth := TStringGridColumn(Source).FMaxWidth;
      FMinWidth := TStringGridColumn(Source).FMinWidth;
      FMultiLine := TStringGridColumn(Source).FMultiLine;
      FPickListItems.Assign(TStringGridColumn(Source).FPickListItems);
      FPickListOnly := TStringGridColumn(Source).FPickListOnly;
      FReadOnly := TStringGridColumn(Source).FReadOnly;
      FRowNumbers := TStringGridColumn(Source).FRowNumbers;
      FTitle.Assign(TStringGridColumn(Source).FTitle);
      FVAlignment := TStringGridColumn(Source).FVAlignment;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited;
end;

constructor TStringGridColumn.Create(Collection: TCollection);
begin
  inherited;
  FColor := clDefault;
  FFont := TFont.Create;
  if Assigned(Collection) then
    if Collection is TStringGridColumns then
    begin
      FColumns := TStringGridColumns(Collection);
      if Assigned(FColumns.Grid) then
      begin
        FGrid := FColumns.Grid;
        if Fixed then
          FFont.Assign(FGrid.FixedFont)
        else
          FFont.Assign(FGrid.Font);
      end;
    end
    else
      raise EInvalidGridOperation.Create(sInvalidStringGridColumnOwnerType);
  FFont.OnChange := FontChanged;
  FPickListItems := TStringList.Create;
  FPickListOnly := True;
  FTitle := TStringGridTitle.Create(Self);
  FTitle.FCaption := 'Column' + IntToStr(Index);
end;

destructor TStringGridColumn.Destroy;
begin
  Destroying := True;
  FPickListItems.Free;
  FTitle.Free;
  FFont.Free;
  inherited;
end;

procedure TStringGridColumn.FontChanged(Sender: TObject);
begin
  Changed(False);
end;

function TStringGridColumn.GetColor: TColor;
begin
  if (FColor = clDefault) and Assigned(FGrid) then
    case Fixed of
      True: Result := FGrid.FixedColor;
      else Result := FGrid.Color
    end
  else
    Result := FColor;
end;

function TStringGridColumn.GetDisplayName: String;
begin
  Result := Title.Caption;
end;

function TStringGridColumn.GetEditFormat: String;
begin
  if FEditFormat <> '' then
    Result := FEditFormat
  else
    case InputStyle of
      isString    : if MaxLength = 0 then
                      Result := '%s'
                    else
                      Result := '%.' + IntToStr(MaxLength) + 's';
      isInteger   : Result := '%d';
      isAbsInteger: Result := '%u';
      isFloat,
      isAbsFloat  : Result := '%f';
      isMask      : Result := '';
    end;
end;

function TStringGridColumn.GetFixed: Boolean;
begin
  if Assigned(FGrid) then
    Result := Index < Grid.FixedCols
  else
    Result := False;
end;

function TStringGridColumn.GetVisible: Boolean;
begin
  if Assigned(FGrid) then
    Result := FGrid.ColWidths[Index] > 0
  else
    Result := False;
end;

function TStringGridColumn.GetWidth: Integer;
begin
  if Assigned(FGrid) then
    Result := FGrid.ColWidths[Index]
  else
    Result := -1;
end;

function TStringGridColumn.IsColorStored: Boolean;
begin
  Result := FColor <> clDefault;
end;

function TStringGridColumn.IsEditFormatStored: Boolean;
begin
  Result := FEditFormat <> '';
end;

function TStringGridColumn.IsFontStored: Boolean;
begin
  if Assigned(FGrid) then
    Result := not SameFont(FFont, FGrid.Font)
  else
    Result := False;
end;

function TStringGridColumn.IsPickListItemsStored: Boolean;
begin
  Result := PickListItems.Count > 0;
end;

procedure TStringGridColumn.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetDefaultCellText(const Value: String);
begin
  if FDefaultCellText <> Value then
  begin
    FDefaultCellText := Value;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetEditFormat(const Value: String);
begin
  if FEditFormat <> Value then
  begin
    if Assigned(Grid) then
      Grid.ChangeEditFormat(Index, FEditFormat, Value);
    FEditFormat := Value;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetEditMask(const Value: TEditMask);
begin
  if FEditMask <> Value then
  begin
    if Value <> '' then
      InputStyle := isMask;
    FEditMask := Value;
  end;
end;

procedure TStringGridColumn.SetFixed(const Value: Boolean);
begin
  if (Fixed <> Value) then
    if Assigned(FGrid) then
      case Value of
        True: FGrid.FixedCols := Index + 1;
        False: FGrid.FixedCols := Index;
      end
    else
      raise EInvalidGridOperation.Create(sInvalidStringGridColumnOwner);
end;

procedure TStringGridColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TStringGridColumn.SetIndex(Value: Integer);
begin
  if Assigned(FGrid) then
    FGrid.MoveColumn(Index, Value);
  inherited;
end;

procedure TStringGridColumn.SetInputStyle(const Value: TInputStyle);
begin
  if FInputStyle <> Value then
  begin
    FInputStyle := Value;
    EditMask := '';
  end;
end;

procedure TStringGridColumn.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
    FMaxLength := Max(Value, 0);
end;

procedure TStringGridColumn.SetMaxWidth(const Value: Integer);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Max(0, Max(FMinWidth, Value));
    Width := Min(Width, FMaxWidth);
  end;
end;

procedure TStringGridColumn.SetMinWidth(const Value: Integer);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Max(0, Min(FMaxWidth, Value));
    Width := Max(Width, FMinWidth);
  end;
end;

procedure TStringGridColumn.SetMultiLine(const Value: Boolean);
begin
  if FMultiLine <> Value then
  begin
    FMultiLine := Value;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetPickListItems(const Value: TStrings);
begin
  FPickListItems.Assign(Value);
end;

procedure TStringGridColumn.SetReadOnly(const Value: Boolean);
var
  iRow: Integer;
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if Assigned(FGrid) then
      with FGrid do
        for iRow := FixedRows to (RowCount - 1) do
          FCellData[Index][iRow].FReadOnly := FReadOnly;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetRowNumbers(const Value: Boolean);
begin
  if FRowNumbers <> Value then
  begin
    FRowNumbers := Value;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetTitle(const Value: TStringGridTitle);
begin
  FTitle.Assign(Value);
end;

procedure TStringGridColumn.SetVAlignment(const Value: TVAlignment);
begin
  if FVAlignment <> Value then
  begin
    FVAlignment := Value;
    Changed(False);
  end;
end;

procedure TStringGridColumn.SetVisible(const Value: Boolean);
begin
  if (Visible <> Value) then
    if Assigned(FGrid) then
      case Value of
        True: Width := FGrid.DefaultColWidth;
        False: Width := -FGrid.GridLineWidth;
      end
    else
      raise EInvalidGridOperation.Create(sInvalidStringGridColumnOwner);
end;

procedure TStringGridColumn.SetWidth(const Value: Integer);
begin
  if Assigned(FGrid) then
    FGrid.ColWidths[Index] := Value
  else
    raise EInvalidGridOperation.Create(sInvalidStringGridColumnOwner);
end;

{ TStringGridColumns }

constructor TStringGridColumns.Create(AGrid: TStringGridEx);
begin
  FGrid := AGrid;
  inherited Create(TStringGridColumn);
end;

function TStringGridColumns.GetItem(Index: Integer): TStringGridColumn;
begin
  if Index > (Count - 1) then
    Result := nil
  else
    Result := TStringGridColumn(inherited GetItem(Index));
end;

function TStringGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TStringGridColumns.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  if Action = cnDeleting then
    Grid.DeleteColumn(Item.Index);
  inherited;
end;

procedure TStringGridColumns.SetItem(Index: Integer;
  const Value: TStringGridColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TStringGridColumns.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FGrid) then
    if Item <> nil then
      FGrid.UpdateColumn(Item.Index)
    else
      FGrid.UpdateColumns;
end;

{ TStringGridStrings }

function TStringGridStrings.Add(const S: String): Integer;
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := 0 to Count - 1 do
      if Strings[i] = '' then
      begin
        if S = '' then
          Strings[i] := ' '
        else
          Strings[i] := S;
        Result := i;
        Exit;
      end;
    Result := -1;
  finally
    EndUpdate;
  end;
end;

procedure TStringGridStrings.Assign(Source: TPersistent);
var
  i: Integer;
  Max: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    Max := TStrings(Source).Count - 1;
    if Max >= Count then Max := Count - 1;
    try
      for i := 0 to Max do
      begin
        Put(i, TStrings(Source).Strings[i]);
        PutObject(i, TStrings(Source).Objects[i]);
      end;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TStringGridStrings.Clear;
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := 0 to (Count - 1) do
      if FIsCol then
        with FGrid.FCellData[FIndex, i] do
        begin
          FString := '';
          FObject := nil;
        end
      else
        with FGrid.FCellData[i, FIndex] do
        begin
          FString := '';
          FObject := nil;
        end;
  finally
    EndUpdate;
  end;
end;

constructor TStringGridStrings.Create(AGrid: TStringGridEx);
begin
  inherited Create;
  FGrid := AGrid;
end;

procedure TStringGridStrings.Delete(Index: Integer);
begin
  raise EInvalidGridOperation.Create(sInvalidStringGridStringsOp);
end;

function TStringGridStrings.Get(Index: Integer): String;
begin
  if FIsCol then
    Result := FGrid.Cells[FIndex, Index]
  else
    Result := FGrid.Cells[Index, FIndex];
end;

function TStringGridStrings.GetCount: Integer;
begin
  if FIsCol then
    Result := FGrid.RowCount
  else
    Result := FGrid.ColCount;
end;

function TStringGridStrings.GetObject(Index: Integer): TObject;
begin
  if FIsCol then
    Result := FGrid.Objects[FIndex, Index]
  else
    Result := FGrid.Objects[Index, FIndex];
end;

procedure TStringGridStrings.Insert(Index: Integer; const S: String);
begin
  raise EInvalidGridOperation.Create(sInvalidStringGridStringsOp);
end;

function TStringGridStrings.LinkGridCol(const AIndex: Integer): TStrings;
begin
  FIndex := AIndex;
  FIsCol := True;
  Result := Self;
end;

function TStringGridStrings.LinkGridRow(const AIndex: Integer): TStrings;
begin
  FIndex := AIndex;
  FIsCol := False;
  Result := Self;
end;

procedure TStringGridStrings.Put(Index: Integer; const S: String);
begin
  if FIsCol then
    FGrid.Cells[FIndex, Index] := S
  else
    FGrid.Cells[Index, FIndex] := S;
end;

procedure TStringGridStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if FIsCol then
    FGrid.Objects[FIndex, Index] := AObject
  else
    FGrid.Objects[Index, FIndex] := AObject;
end;

procedure TStringGridStrings.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    if FIsCol then
      FGrid.InvalidateCol(FIndex)
    else
      FGrid.InvalidateRow(FIndex);
  inherited;
end;

{ TStringGridRowColors }

procedure TStringGridRowColors.Assign(Source: TPersistent);
begin
  if Source is TStringGridRowColors then
  begin
    FEvenRowColor := TStringGridRowColors(Source).FEvenRowColor;
    FIncludeFixed := TStringGridRowColors(Source).FIncludeFixed;
    FOddRowColor := TStringGridRowColors(Source).FOddRowColor;
    FOverrideColumnColor := TStringGridRowColors(Source).FOverrideColumnColor;
    if Assigned(FGrid) then
      FGrid.InvalidateGrid;
  end;
  inherited;
end;

constructor TStringGridRowColors.Create(AGrid: TStringGridEx);
begin
  inherited Create;
  FGrid := AGrid;
  FEvenRowColor := clDefault;
  FOddRowColor := clDefault;
  FOverrideColumnColor := True;
end;

function TStringGridRowColors.GetEvenRowColor: TColor;
begin
  if (FEvenRowColor = clDefault) and Assigned(FGrid) then
    Result := FGrid.Color
  else
    Result := FEvenRowColor;
end;

function TStringGridRowColors.GetOddRowColor: TColor;
begin
  if (FOddRowColor = clDefault) and Assigned(FGrid) then
    Result := FGrid.Color
  else
    Result := FOddRowColor;
end;

function TStringGridRowColors.IsEvenRowColorStored: Boolean;
begin
  Result := FEvenRowColor <> clDefault;
end;

function TStringGridRowColors.IsOddRowColorStored: Boolean;
begin
  Result := FOddRowColor <> clDefault;
end;

function TStringGridRowColors.IsStored: Boolean;
begin
  Result := IsEvenRowColorStored or IsOddRowColorStored or FIncludeFixed or
    not FOverrideColumnColor;
end;

procedure TStringGridRowColors.SetEvenRowColor(const Value: TColor);
begin
  if FEvenRowColor <> Value then
  begin
    FEvenRowColor := Value;
    FGrid.InvalidateGrid;
  end;
end;

procedure TStringGridRowColors.SetIncludeFixed(const Value: Boolean);
begin
  if FIncludeFixed <> Value then
  begin
    FIncludeFixed := Value;
    FGrid.InvalidateGrid;
  end;
end;

procedure TStringGridRowColors.SetOddRowColor(const Value: TColor);
begin
  if FOddRowColor <> Value then
  begin
    FOddRowColor := Value;
    FGrid.InvalidateGrid;
  end;
end;

procedure TStringGridRowColors.SetOverrideColumnColor(
  const Value: Boolean);
begin
  if FOverrideColumnColor <> Value then
  begin
    FOverrideColumnColor := Value;
    FGrid.InvalidateGrid;
  end;
end;

{ TInplaceEditListEx }

procedure TInplaceEditListEx.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F4) and (Shift = []) then
  begin
    if ListVisible then
      CloseUp(False)
    else
      DropDown;
    Key := 0;
  end;
  inherited;
end;

function TInplaceEditListEx.Grid: TStringGridEx;
begin
  Result := TStringGridEx(inherited Grid);
end;

function TInplaceEditListEx.PickListItemIndexOf(const Value: String): Integer;
var
  i: Integer;
begin
  with PickList do
    for i := 0 to Items.Count - 1 do
      if UpperCase(Copy(Items[i], 1, Length(Value))) = UpperCase(Value) then
      begin
        Result := i;
        Exit;
      end;
  Result := -1;
end;

procedure TInplaceEditListEx.UpdateContents;
begin
  inherited;
  with Grid do
  begin
    Self.Color := GetCellColor(Col, Row, [], Columns[Col], FCellData[Col, Row]);
    GetCellFont(Self.Font, Row, [], Columns[Col]);
    if (EditStyle = esPickList) and Assigned(Columns[Col]) then
    begin
      PickList.Items.Assign(Columns[Col].PickListItems);
      PickListOnly := Columns[Col].PickListOnly;
    end;
  end;
end;

procedure TInplaceEditListEx.WMWindowPosChanging(var Message: TWMWindowPosMsg);
begin
  with Grid do
    if CanMergedEditShow and FCellData[Col, Row].FMerged then
      with GetMergeRect(Col, Row) do
        with BoxRect(Left, Top, Right, Bottom) do
        begin
          Message.WindowPos.x := Left;
          Message.WindowPos.y := Top;
          Message.WindowPos.cx := Right - Left;
          Message.WindowPos.cy := Bottom - Top;
        end;
  inherited;
end;

procedure TInplaceEditListEx.WndProc(var Message: TMessage);
begin
  if EditStyle = esPickList then
    case Message.Msg of
      WM_KEYDOWN, WM_SYSKEYDOWN:
        with TWMKey(Message) do
        begin
          DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
          if (CharCode <> 0) and ListVisible then
          begin
            if CharCode in [VK_LEFT, VK_RIGHT, VK_DELETE] then
              Dispatch(Message)
            else
              with Message do
                SendMessage(ActiveList.Handle, Msg, WParam, LParam);
            Exit;
          end;
        end;
      WM_CHAR:
        begin
          if PickListOnly then
            with Message do
              SendMessage(ActiveList.Handle, Msg, WParam, LParam)
          else
          begin
            Dispatch(Message);
            if ListVisible then
              PickList.ItemIndex := PickListItemIndexOf(EditText);
          end;
          Exit;
        end;
    end;
  inherited;
end;

{ TStringGridEx }

procedure TStringGridEx.AdjustSize;
begin
  //TCustomGrid.AdjustSize is obsolete and has wrong result for this
  //descendant. Instead, use the methods DeleteColumn, DeleteRows,
  //InsertColumn and InsertRow, which take care of Merged Cells.
end;

function TStringGridEx.CanEditAcceptKey(Key: Char): Boolean;
const
  CHARSET_ABSINTEGER = ['0'..'9', #8, #9, #13, #27, #127];
  CHARSET_INTEGER = ['0'..'9', #8, #9, #13, #27, #127, '-'];
var
  DS: Char;
begin
  DS := DecimalSeparator;
  Result := True;
  if Assigned(Columns[Col]) then
    with InplaceEditor do
      case Columns[Col].InputStyle of
        isInteger       : Result := (Key in CHARSET_INTEGER) and
                            not ((Key = '-') and (Pos('-', Text) > 0)) and
                            not ((Key = '-') and (SelStart <> 0));
        isAbsInteger    : Result := Key in CHARSET_ABSINTEGER;
        isFloat         : Result := (Key in (CHARSET_INTEGER + [DS])) and
                            not ((Key = '-') and (Pos('-', Text) > 0)) and
                            not ((Key = '-') and (SelStart <> 0)) and
                            not ((Key = DS) and (Pos(DS, Text) > 0) and
                              (Pos(DS, SelText) > 0));
        isAbsFloat      : Result := (Key in (CHARSET_ABSINTEGER + [DS])) and
                            not ((Key = DS) and (Pos(DS, Text) > 0) and
                              (Pos(DS, SelText) > 0));
        isMask, isString: Result := inherited CanEditAcceptKey(Key);
      end;
  if not Result then
    MessageBeep(0);
end;

function TStringGridEx.CanEditModify: Boolean;
begin
  Result := CanMergedEditShow and (not FCellData[Col, Row].FReadOnly);
end;

function TStringGridEx.CanEditShow: Boolean;
begin
  Result := ((CanMergedEditShow and not CellsReadOnly[Col, Row]) or
    (goAlwaysShowEditor in Options)) and inherited CanEditShow;
end;

function TStringGridEx.CanMergedEditShow: Boolean;
begin
  with FCellData[Col, Row] do
    Result := (not FMerged) or
      (FMerged and FMergeText and (FMergeCol > Col) and (FMergeRow > Row));
end;

procedure TStringGridEx.ChangeEditFormat(const ACol: Integer;
  const OldFormat, NewFormat: String);
var
  iRow: Integer;
begin
  for iRow := 0 to (RowCount - 1) do
    with FCellData[ACol, iRow] do
      if FString <> '' then
        try
          if Format(OldFormat, [FValue]) = FString then
            FString := Format(NewFormat, [FValue]);
        except
        end;
end;

procedure TStringGridEx.CMFontChanged(var Message: TMessage);
var
  iCol: Integer;
begin
  if SameFont(FixedFont, FFontBackup) then
    FixedFont.Assign(Font); //Takes also care for Columns[n].Title.Font
  for iCol := 0 to (Columns.Count - 1) do
    if not Columns[iCol].Fixed then
      if SameFont(Columns[iCol].Font, FFontBackup) then
        Columns[iCol].Font.Assign(Font);
  FFontBackup.Assign(Font);
  inherited;
end;

procedure TStringGridEx.ColumnMoved(FromIndex, ToIndex: Integer);
var
  iRow, iCol: Integer;
begin
  for iRow := (RowCount - 1) downto 0 do
  begin
    UnMergeCell(FromIndex, iRow);
    if ToIndex > FromIndex then
    begin
      if ToIndex < GetMergeRect(ToIndex, iRow).Right then
        UnMergeCell(ToIndex, iRow);
      for iCol := (FromIndex + 1) to ToIndex do
        ExchangeCellData(iCol, iRow, iCol - 1, iRow)
    end
    else
    begin
      if ToIndex > GetMergeRect(ToIndex, iRow).Left then
        UnMergeCell(ToIndex, iRow);
      for iCol := (FromIndex - 1) downto ToIndex do
        ExchangeCellData(iCol, iRow, iCol + 1, iRow);
    end;
  end;
  inherited;
end;

procedure TStringGridEx.ColWidthsChanged;
const
  ColWidthsUpdating: Boolean = False;
begin
  if not ColWidthsUpdating then
  try
    ColWidthsUpdating := True;
    UpdateColWidths;
    inherited;
  finally
    ColWidthsUpdating := False;
  end;
end;

constructor TStringGridEx.Create(AOwner: TComponent);
begin
  inherited;
  SizeChanged(0, 0); //Set size of FCellData
  inherited DefaultDrawing := False; //There's no inherited drawing at all!
  ParentBidiMode := False;
  BidiMode := bdLeftToRight;
  Options := Options + [goThumbTracking];
  FFixedFont := TFont.Create;
  FFixedFont.Assign(Font);
  FFixedFont.OnChange := FixedFontChanged;
  FFixedFontBackup := TFont.Create;
  FFixedFontBackup.Assign(FFixedFont);
  FFontBackup := TFont.Create;
  FFontBackup.Assign(Font);
  FColRowStrings := TStringGridStrings.Create(Self);
  FColumns := TStringGridColumns.Create(Self);
  FRowColors := TStringGridRowColors.Create(Self);
  FDefaultDrawing := [ddBackground, ddEdges, ddGridLines, ddText, ddFocusRect];
  FFixedGridLineColor := clBlack;
  FFocusRectColor := clDefault;
  FGridLineColor := clSilver;
  FReadOnlyColor := clDefault;
  FSelectionAlphaBlend := True;
  FSelectionAlphaBlendValue := 80;
  FSelectionColor := clHighLight;
  FStretchModes := [];
  FSyncColumns := False;
end;

function TStringGridEx.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditListEx.Create(Self);
end;

procedure TStringGridEx.DeleteColumn(ACol: Integer);
begin
  if Assigned(Columns[ACol]) then
    if not Columns[ACol].Destroying then
      Columns[ACol].Free;
  inherited;
end;

procedure TStringGridEx.DeleteRows(const DeletePos: TDeletePos;
  StartIndex: Integer = -1; const Count: Integer = 1);
var
  i: Integer;
  SaveSel: TGridRect;
  SaveTopRow: Integer;
begin
  if (DeletePos = dpRange) and (StartIndex = -1) then
    Exit;
  StartIndex := Max(StartIndex, FixedRows);
  SaveSel := Selection;
  SaveTopRow := TopRow;
  case DeletePos of
    dpCurrent:   DeleteRow(Row);
    dpBegin:     DeleteRow(FixedRows);
    dpEnd:       DeleteRow(RowCount - 1);
    dpRange:     for i := (StartIndex + Count - 1) downto StartIndex do
                   DeleteRow(i);
    dpSelection: for i := Selection.Bottom downto Selection.Top do
                   DeleteRow(i);
    dpAll:       for i := (RowCount - 1) downto (FixedRows + 1) do
                   DeleteRow(i);
  end;
  SaveSel.Bottom := Min(SaveSel.Bottom, RowCount - 1);
  Selection := SaveSel;
  TopRow := Min(SaveTopRow, RowCount - 1);
end;

destructor TStringGridEx.Destroy;
begin
  FRowColors.Free;
  FColRowStrings.Free;
  FColumns.Free;
  FFontBackup.Free;
  FFixedFontBackup.Free;
  FFixedFont.Free;
  inherited;
end;

procedure TStringGridEx.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, ACol, ARow, ARect, AState, Columns[ACol]);
end;

procedure TStringGridEx.ExchangeCellData(const FromCol, FromRow, ToCol,
  ToRow: Integer);
var
  Temp: TCellData;
begin
  with FCellData[ToCol, ToRow] do
  begin
    Temp.FString := FString;
    Temp.FObject := FObject;
    Temp.FReadOnly := FReadOnly;
    Temp.FValue := FValue;
    FString := FCellData[FromCol, FromRow].FString;
    FObject := FCellData[FromCol, FromRow].FObject;
    FMerged := FCellData[FromCol, FromRow].FMerged;
    FMergeCol := FCellData[FromCol, FromRow].FMergeCol + ToCol - FromCol;
    FMergeRow := FCellData[FromCol, FromRow].FMergeRow + ToRow - FromRow;
    FMergeText := FCellData[FromCol, FromRow].FMergeText;
    FMergeML := FCellData[FromCol, FromRow].FMergeML;
    FReadOnly := FCellData[FromCol, FromRow].FReadOnly;
    FValue := FCellData[FromCol, FromRow].FValue;
  end;
  with FCellData[FromCol, FromRow] do
  begin
    FString := Temp.FString;
    FObject := Temp.FObject;
    FReadOnly := Temp.FReadOnly;
    FValue := Temp.FValue;
  end;
end;

procedure TStringGridEx.FixedFontChanged(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to (Columns.Count - 1) do
  begin
    if SameFont(Columns[i].Title.Font, FFixedFontBackup) then
      Columns[i].Title.Font.Assign(FFixedFont);
    if Columns[i].Fixed then
      if SameFont(Columns[i].Font, FFixedFontBackup) then
        Columns[i].Font.Assign(FFixedFont);
  end;
  FFixedFontBackup.Assign(FFixedFont);
  InvalidateGrid;
end;

function TStringGridEx.GetCellColor(const ACol, ARow: Integer;
  const AState: TGridDrawState; Column: TStringGridColumn;
  const CellData: TCellData): TColor;
var
  DrawRowColor: Boolean;
begin
  if CellData.FReadOnly then
    Result := ReadOnlyColor
  else
  begin
    with FRowColors do
      DrawRowColor := ((Odd(ARow) and (FOddRowColor <> clDefault)) or
        (not Odd(ARow) and (FEvenRowColor <> clDefault))) and
        (ARow >= FixedRows) and
        not ((gdFixed in AState) and not IncludeFixed) and
        not (Assigned(Column) and not OverrideColumnColor);
    case DrawRowColor of
      True: case Odd(ARow) of
              True: Result := FRowColors.OddRowColor;
              else Result := FRowColors.EvenRowColor;
            end;
      else case Assigned(Column) of
             True: Result := Column.Color;
             else Result := Color;
           end;
    end;
    if gdFixed in AState then
      case Assigned(Column) of
        True: if ARow < FixedRows then
                Result := Column.Title.Color;
        False: if not DrawRowColor then
                 Result := FixedColor;
      end;
  end;
end;

function TStringGridEx.GetCellData(const ACol, ARow: Integer): TCellData;
begin
  Result := FCellData[ACol, ARow]
end;

procedure TStringGridEx.GetCellFont(AFont: TFont; const ARow: Integer;
  const AState: TGridDrawState; Column: TStringGridColumn);
begin
  case Assigned(Column) of
    True: case (gdFixed in AState) and (ARow < FixedRows) of
            True: if not SameFont(AFont, Column.Title.Font) then
                    AFont.Assign(Column.Title.Font);
            False: if not SameFont(AFont, Column.Font) then
                     AFont.Assign(Column.Font);
          end;
    False: case gdFixed in AState of
             True: if not SameFont(AFont, FixedFont) then
                     AFont.Assign(FixedFont);
             False: if not SameFont(AFont, Font) then
                      AFont.Assign(Font);
           end;
  end;
end;

function TStringGridEx.GetCells(ACol, ARow: Integer): String;
begin
  if InGrid(ACol, ARow) then
    Result := FCellData[ACol, ARow].FString
  else
    Result := '';
end;

function TStringGridEx.GetCellsReadOnly(ACol, ARow: Integer): Boolean;
begin
  if InGrid(ACol, ARow) then
    Result := FCellData[ACol, ARow].FReadOnly
  else
    Result := False;
end;

function TStringGridEx.GetColCount: Integer;
begin
  Result := inherited ColCount;
end;

function TStringGridEx.GetCols(Index: Integer): TStrings;
begin
  Result := FColRowStrings.LinkGridCol(Index);
end;

function TStringGridEx.GetEditLimit: Integer;
begin
  if Assigned(Columns[Col]) then
    Result := Columns[Col].MaxLength
  else
    Result := inherited GetEditLimit;
end;

function TStringGridEx.GetEditMask(ACol, ARow: Integer): String;
begin
  if not Assigned(Columns[ACol]) then
    Result := inherited GetEditMask(ACol, ARow)
  else
    if Columns[ACol].InputStyle = isMask then
      Result := Columns[ACol].EditMask;
end;

function TStringGridEx.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
  if Assigned(Columns[ACol]) then
    Result := Columns[ACol].EditStyle
  else
    Result := esSimple;
end;

function TStringGridEx.GetEditText(ACol, ARow: Integer): String;
begin
  if CanMergedEditShow then
    Result := FCellData[ACol, ARow].FString
  else
    Result := '';
  if Assigned(OnGetEditText) then
    OnGetEditText(Self, ACol, ARow, Result);
end;

function TStringGridEx.GetFixedCols: Integer;
begin
  Result := inherited FixedCols;
end;

function TStringGridEx.GetFixedRows: Integer;
begin
  Result := inherited FixedRows;
end;

function TStringGridEx.GetGridLineWidth: Integer;
begin
  Result := inherited GridLineWidth;
end;

function TStringGridEx.GetMergeRect(const ACol, ARow: Integer): TGridRect;
begin
  if FCellData[ACol, ARow].FMerged then
    with FCellData[ACol, ARow] do
    begin
      if (FMergeCol < ACol) or (FMergeRow < ARow) then
      begin
        Result.Left := FMergeCol;
        Result.Top := FMergeRow;
      end
      else
      begin
        Result.Left := ACol;
        Result.Top := ARow;
      end;
      Result.Right := FCellData[Result.Left, Result.Top].FMergeCol;
      Result.Bottom := FCellData[Result.Left, Result.Top].FMergeRow;
    end
  else
  begin
    //Caller has to interpret result !
    Result.Left := ColCount;
    Result.Top := RowCount;
    Result.Right := -1;
    Result.Bottom := -1;
  end;
end;

function TStringGridEx.GetObjects(ACol, ARow: Integer): TObject;
begin
  if InGrid(ACol, ARow) then
    Result := FCellData[ACol, ARow].FObject
  else
    Result := nil;
end;

function TStringGridEx.GetReadOnlyColor: TColor;
begin
  if FReadOnlyColor = clDefault then
    Result := Color
  else
    Result := FReadOnlyColor;
end;

function TStringGridEx.GetRows(Index: Integer): TStrings;
begin
  Result := FColRowStrings.LinkGridRow(Index);
end;

function TStringGridEx.GetValues(ACol, ARow: Integer): Double;
begin
  if InGrid(ACol, ARow) then
    Result := FCellData[ACol, ARow].FValue
  else
    Result := 0;
end;

function TStringGridEx.InGrid(const ACol, ARow: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ACol < ColCount) and
    (ARow >= 0) and (ARow < RowCount);
end;

procedure TStringGridEx.InsertColumn(const AtIndex: Integer);
begin
  if SyncColumns then
    Columns.Add
  else
    ColCount := ColCount + 1;
  if AtIndex < ColCount - 1 then
    MoveColumn(ColCount - 1, AtIndex);
  if AtIndex < FixedCols then
    FixedCols := FixedCols + 1;
end;

procedure TStringGridEx.InsertColumn(const Position: TInsertPos);
begin
  case Position of
    ipBefore: InsertColumn(Col);
    ipAfter: InsertColumn(Col + 1);
    ipBegin: InsertColumn(FixedCols);
    ipEnd: InsertColumn(ColCount);
  end;
end;

procedure TStringGridEx.InsertRow(const AtIndex: Integer);
begin
  RowCount := RowCount + 1;
  if AtIndex < RowCount - 1 then
    MoveRow(RowCount - 1, AtIndex);
  if AtIndex < FixedRows then
    FixedRows := FixedRows + 1;
end;

procedure TStringGridEx.InsertRow(const Position: TInsertPos);
begin
  case Position of
    ipBefore: InsertRow(Row);
    ipAfter: InsertRow(Row + 1);
    ipBegin: InsertRow(FixedRows);
    ipEnd: InsertRow(RowCount);
  end;
end;

procedure TStringGridEx.InvalidateGridRect(const AGridRect: TGridRect);
var
  ARect: TRect;
begin
  with AGridRect do
    ARect := BoxRect(Left, Top, Right, Bottom);
  InvalidateRect(Handle, @ARect, True);
end;

function TStringGridEx.IsAlternatingRowColorsStored: Boolean;
begin
  Result := FRowColors.IsStored;
end;

function TStringGridEx.IsColumnsStored: Boolean;
begin
  Result := Columns.Count > 0;
end;

function TStringGridEx.IsEmptyColumn(const ACol: Integer): Boolean;
var
  iRow: Integer;
begin
  Result := True;
  for iRow := FixedRows to (RowCount - 1) do
    if Trim(Cells[ACol, iRow]) <> '' then
    begin
      Result := False;
      Exit;
    end;
end;

function TStringGridEx.IsEmptyRow(const ARow: Integer): Boolean;
var
  iCol: Integer;
begin
  Result := True;
  for iCol := FixedCols to (ColCount - 1) do
    if Trim(Cells[iCol, ARow]) <> '' then
    begin
      Result := False;
      Exit;
    end;
end;

function TStringGridEx.IsFixedFontStored: Boolean;
begin
  Result := not SameFont(Font, FFixedFont);
end;

function TStringGridEx.IsReadOnlyColorStored: Boolean;
begin
  Result := FReadOnlyColor <> clDefault;
end;

(** Storage of Mergings in FCellData ***********************
    0      1      2      3      4      5      6      7
    --------------------------------------------------------
0  |
1  |              5,3    2,1    2,1    2,1
2  |              2,1    2,1    2,1    2,1
3  |              2,1    2,1    2,1    2,1
4  |
************************************************************)

procedure TStringGridEx.MergeCell(const ACol, ARow: Integer;
  const MergeCoord: TGridCoord; const MergeText, MultiLine: Boolean);
begin
  if (ACol = MergeCoord.X) and (ARow = MergeCoord.Y) then
    Exit;
  UnMergeCell(ACol, ARow);
  with FCellData[ACol, ARow] do
  begin
    FMerged := True;
    FMergeCol := MergeCoord.X;
    FMergeRow := MergeCoord.Y;
    FMergeText := MergeText;
    FMergeML := MultiLine;
  end;
end;

procedure TStringGridEx.MergeCells(const AGridRect: TGridRect;
  const MergeText: Boolean = True; const MultiLine: Boolean = False);
var
  iCol, iRow: Integer;
begin
  with AGridRect do
  begin
    if (Left < FixedCols) or (Top < FixedRows) or (Right > Colcount - 1) or
      (Bottom > RowCount - 1) then
      Exit;
    for iCol := Left to Right do
      for iRow := Top to Bottom do
        MergeCell(iCol, iRow, TopLeft, MergeText, MultiLine);
    //Link TopLeft to BottomRight
    MergeCell(Left, Top, BottomRight, MergeText, MultiLine);
  end;
  InvalidateGridRect(AGridRect);
end;

procedure TStringGridEx.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  SaveState: TGridState;
begin
  SaveState := FGridState;
  inherited;
  if Assigned(FOnTitleClick) then
  begin
    if (SaveState = gsRowSizing) or (SaveState = gsColSizing) or
      ((InplaceEditor <> nil) and (InplaceEditor.Visible) and
      (PtInRect(InplaceEditor.BoundsRect, Point(X, Y)))) then
      Exit;
    Cell := MouseCoord(X, Y);
    if (Button = mbLeft) and (Cell.X >= 0) and (Cell.X < ColCount) and
      (Cell.Y >= 0) and (Cell.Y < FixedRows) then
      FOnTitleClick(Self, Cell.X, Columns[Cell.X]);
  end;
end;

procedure TStringGridEx.MoveColumn(const FromIndex, ToIndex: Integer);
begin
  inherited MoveColumn(FromIndex, ToIndex);
end;

procedure TStringGridEx.MoveRow(const FromIndex, ToIndex: Integer);
begin
  inherited MoveRow(FromIndex, ToIndex);
end;

procedure TStringGridEx.Paint;
type
  TCacheColors = record
    Source: TColor;
    Dest: TColor;
    Result: TColor;
  end;
var
  UpdateRect: TGridRect;
  DrawInfo: TGridDrawInfo;
  EdgeFlags: UINT;
  FixedBGExtend: TPoint;
  CellBGExtend: TPoint;
  IsActiveControl: Boolean;
  CacheColors: TCacheColors;

  procedure DefaultDrawCell(const ACol, ARow: Integer; const InnerRect: TRect;
    const AState: TGridDrawState; Column: TStringGridColumn;
    const CellData: TCellData);
  const
    FlagML: array[Boolean] of UINT = (0, DT_WORDBREAK or DT_EDITCONTROL);
    FlagAlign: array[TAlignment] of UINT = (DT_LEFT, DT_RIGHT, DT_CENTER);
    FlagVAlign: array[TVAlignment] of UINT = (DT_TOP, DT_VCENTER, DT_BOTTOM);
  var
    DrawSelected: Boolean;
    BGRect: TRect;
    OuterRect: TRect;
    MergeRect: TGridRect;
    DrawColor: TColor;
    DrawRect: TRect;
    TxtFlags: UINT;
    Txt: String;

    function GetNegativeBWColor(Value: TColor): TColor;
    begin
      Value := ColorToRGB(Value);
      if (GetRValue(Value) + GetGValue(Value) + GetBValue(Value)) > $180 then
        Result := clBlack
      else
        Result := clWhite;
    end;

    function GetAlphaBlendColor(Source, Dest: TColor;
      const BlendValue: Byte): TColor;
    //With thanks to:
    //www.msdn.com/library/default.asp?url=/library/en-us/gdi/bitmaps_3b3m.asp
    var
      Src, Dst: TRGB;
      SCAFactor: Single; //SourceConstantAlpha factor
    begin
      if (CacheColors.Source <> Source) or (CacheColors.Dest <> Dest) then
      begin
        CacheColors.Source := Source;
        CacheColors.Dest := Dest;
        Src := GetRGB(Source);
        Dst := GetRGB(Dest);
        SCAFactor := BlendValue / 255;
        Dst.R := Trunc( (Src.R * SCAFactor) + (Dst.R * (1.0 - SCAFactor)) );
        Dst.G := Trunc( (Src.G * SCAFactor) + (Dst.G * (1.0 - SCAFactor)) );
        Dst.B := Trunc( (Src.B * SCAFactor) + (Dst.B * (1.0 - SCAFactor)) );
        with Dst do
          CacheColors.Result := RGB(R, G, B);
      end;
      Result := CacheColors.Result
    end;

    function GetSelectionColor(const Current: TColor;
      const IsFont: Boolean): TColor;
    begin
      if SelectionAlphaBlend then
        Result := GetAlphaBlendColor(SelectionColor, Current,
          FSelectionAlphaBlendValue)
      else
        if IsFont then
          Result := clHighLightText
        else
          Result := SelectionColor;
    end;

    function GetMergeDrawExtends: TRect;
    var
      i: Integer;
    begin
      with MergeRect do
        Result := BoxRect(Left, Top, Right, Bottom);
      //BoxRect truncs result to visible cells: fix...
      for i := MergeRect.Top to (TopRow - 1) do
        Result.Top := Result.Top -
          (RowHeights[i] + DrawInfo.Vert.EffectiveLineWidth);
      for i := MergeRect.Left to (LeftCol - 1) do
        Result.Left := Result.Left -
          (ColWidths[i] + DrawInfo.Horz.EffectiveLineWidth);
    end;

  begin
    DrawSelected := (gdSelected in AState) and
      (not (gdFocused in AState) or
      ([goDrawFocusSelected, goRowSelect] * Options <> []));

    BGRect := InnerRect;
    if gdFixed in AState then
    begin
      Inc(BGRect.Right, FixedBGExtend.X);
      Inc(BGRect.Bottom, FixedBGExtend.Y);
    end else begin
      Inc(BGRect.Right, CellBGExtend.X);
      Inc(BGRect.Bottom, CellBGExtend.Y);
    end;

    OuterRect := InnerRect;
    Inc(OuterRect.Right, DrawInfo.Horz.EffectiveLineWidth);
    Inc(OuterRect.Bottom, DrawInfo.Vert.EffectiveLineWidth);

    MergeRect := GetMergeRect(ACol, ARow);
    if MergeRect.Right > ACol then
      BGRect.Right := OuterRect.Right;
    if MergeRect.Bottom > ARow then
      BGRect.Bottom := OuterRect.Bottom;

    with Canvas do
    begin
      //Draw background
      if ddBackground in DefaultDrawing then
      begin
        Brush.Style := bsSolid;
        DrawColor := GetCellColor(ACol, ARow, AState, Column, CellData);
        case DrawSelected of
          True: Brush.Color := GetSelectionColor(DrawColor, False);
          False: Brush.Color := DrawColor;
        end;
        FillRect(BGRect);
      end;

      //Draw edges of fixed cells
      if (ddEdges in DefaultDrawing) and (gdFixed in AState) and
        (EdgeFlags <> 0) then
        DrawEdge(Handle, BGRect, BDR_RAISEDINNER, EdgeFlags);

      //Draw gridlines as filled rectangles
      if (ddGridLines in DefaultDrawing) and (GridLineWidth > 0) then
      begin
        if gdFixed in AState then
          Brush.Color := FFixedGridLineColor
        else
          Brush.Color := FGridLineColor;
        //Right gridline
        if OuterRect.Right > BGRect.Right then
        begin
          DrawRect.TopLeft := Point(BGRect.Right, BGRect.Top);
          DrawRect.BottomRight := Point(OuterRect.Right, BGRect.Bottom);
          FillRect(DrawRect);
        end;
        //Bottom gridline
        if OuterRect.Bottom > BGRect.Bottom then
        begin
          DrawRect.TopLeft := Point(BGRect.Left, BGRect.Bottom);
          DrawRect.BottomRight := Point(BGRect.Right, OuterRect.Bottom);
          FillRect(DrawRect);
        end;
        //Bottomright intersection of gridlines
        if (OuterRect.Right > BGRect.Right) and
          (OuterRect.Bottom > BGRect.Bottom) then
        begin
          DrawRect.TopLeft := BGRect.BottomRight;
          DrawRect.BottomRight := OuterRect.BottomRight;
          FillRect(DrawRect);
        end;
      end;

      //Draw text
      if ddText in DefaultDrawing then
      begin
        Brush.Style := bsClear;
        GetCellFont(Font, ARow, AState, Column);
        if DrawSelected then
          Font.Color := GetSelectionColor(Font.Color, True);
        TxtFlags := DT_END_ELLIPSIS or DT_NOPREFIX;
        //Set text justification
        if not Assigned(Column) then
          TxtFlags := TxtFlags or DT_LEFT
        else
          if (gdFixed in AState) and (ARow < FixedRows) then
            TxtFlags := TxtFlags or FlagAlign[Column.Title.Alignment] or
              FlagVAlign[Column.Title.VAlignment]
          else
            TxtFlags := TxtFlags or FlagAlign[Column.Alignment] or
              FlagVAlign[Column.VAlignment];
        //Set text
        if CellData.FMerged and CellData.FMergeText then
        begin
          DrawRect := GetMergeDrawExtends;
          Txt := Cells[MergeRect.Left, MergeRect.Top];
          TxtFlags := TxtFlags or FlagML[CellData.FMergeML];
          SelectClipRgn(Handle, 0);
          IntersectClipRect(Handle, OuterRect.Left, OuterRect.Top,
            OuterRect.Right, OuterRect.Bottom);
        end
        else
        begin
          DrawRect := BGRect;
          if Assigned(Column) then
          begin
            if ARow = 0 then
              Txt := Column.Title.Caption
            else if CellData.FString <> '' then
              Txt := CellData.FString
            else if Column.RowNumbers then
              Txt := IntToStr(ARow)
            else
              Txt := Column.DefaultCellText;
          end
          else
            Txt := CellData.FString;
          if (ARow = 0) and Assigned(Column) then
            TxtFlags := TxtFlags or FlagML[Column.Title.MultiLine]
          else
            TxtFlags := TxtFlags or DT_SINGLELINE;
        end;
        InflateRect(DrawRect, -2, -2);
        DrawText(Canvas.Handle, PChar(Txt), Length(Txt), DrawRect, TxtFlags);
        SelectClipRgn(Handle, 0);
      end;

      //Draw focusrect
      if ddFocusRect in DefaultDrawing then
        if Focused and (gdFocused in AState) and not (goRowSelect in Options)
          and not (csDesigning in ComponentState) and
          ([goEditing, goAlwaysShowEditor] * Options <>
          [goEditing, goAlwaysShowEditor]) then
        case FocusRectStyle of
          frDefault:
            begin
              Brush.Style := bsSolid;
              Brush.Color := clWhite;
              SetTextColor(Handle, ColorToRGB(clBlack));
              Windows.DrawFocusRect(Handle, BGRect);
              SetTextColor(Handle, ColorToRGB(Font.Color));
            end;
          frSolidAutoBW:
            begin
              Brush.Color := GetNegativeBWColor(
                GetCellColor(ACol, ARow, AState, Column, CellData));
              FrameRect(BGRect);
            end;
          frSolidCustomColor:
            begin
              Brush.Color := FFocusRectColor;
              FrameRect(BGRect);
            end;
        end;

    end;
  end;

  function CellInGridRect(const ACol, ARow: Integer;
    const ARect: TGridRect): Boolean;
  begin
    with ARect do
      Result := (ACol >= Left) and (ACol <= Right) and (ARow >= Top) and
        (ARow <= Bottom);
  end;

  function GetIsActiveControl: Boolean;
  var
    H: Hwnd;
    ParentForm: TCustomForm;
  begin
    Result := False;
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) then
    begin
      if ParentForm.ActiveControl = Self then
        Result := True
    end
    else
    begin
      H := GetFocus;
      while IsWindow(H) and (Result = False) do
        if H = WindowHandle then
          Result := True
        else
          H := GetParent(H);
    end;
  end;

  procedure DrawCells(const StartCol, StartRow, StopCol, StopRow: Integer;
    const IncludeDrawState: TGridDrawState);
  var
    iRect: TRect;
    StartY: Integer;
    iCol, iRow: Integer;
    DrawState: TGridDrawState;
  begin
    iRect := BoxRect(StartCol, StartRow, StartCol, StartRow);
    StartY := iRect.Top;
    for iCol := StartCol to StopCol do
    begin
      iRect.Right := iRect.Left + ColWidths[iCol];
      iRect.Top := StartY;
      if iRect.Right > iRect.Left then
        for iRow := StartRow to StopRow do
        begin
          iRect.Bottom := iRect.Top + RowHeights[iRow];
          if iRect.Bottom > iRect.Top then
          begin
            DrawState := IncludeDrawState;
            if CellInGridRect(iCol, iRow, Selection) then
              Include(DrawState, gdSelected);
            if (iCol = Col) and (iRow = Row) and IsActiveControl then
              Include(DrawState, gdFocused);
            DefaultDrawCell(iCol, iRow, iRect, DrawState, Columns[iCol],
              FCellData[iCol, iRow]);
            DrawCell(iCol, iRow, iRect, DrawState);
          end;
          iRect.Top := iRect.Bottom + DrawInfo.Vert.EffectiveLineWidth;
        end;
      iRect.Left := iRect.Right + DrawInfo.Horz.EffectiveLineWidth;
    end;
  end;

begin
  CalcDrawInfo(DrawInfo);

  with Canvas.ClipRect do
  begin
    MouseToCell(Left, Top, UpdateRect.Left, UpdateRect.Top);
    MouseToCell(Right, Bottom, UpdateRect.Right, UpdateRect.Bottom);
  end;
  if UpdateRect.Right = -1 then
  begin
    UpdateRect.Right :=
      Min(DrawInfo.Horz.LastFullVisibleCell + 1, ColCount - 1);
    UpdateRect.Bottom :=
      Min(DrawInfo.Vert.LastFullVisibleCell + 1, RowCount - 1);
  end;

  FixedBGExtend := Point(0, 0);
  CellBGExtend := Point(0, 0);
  EdgeFlags := 0;
  with DrawInfo do
    if not (ddGridLines in DefaultDrawing) then
    begin
      FixedBGExtend := Point(Horz.EffectiveLineWidth, Vert.EffectiveLineWidth);
      CellBGExtend := Point(Horz.EffectiveLineWidth, Vert.EffectiveLineWidth);
    end
    else
    begin
      if not (goFixedVertLine in Options) then
        FixedBGExtend.X := Horz.EffectiveLineWidth
      else if Ctl3D then
        EdgeFlags := BF_LEFT or BF_RIGHT;
      if not (goFixedHorzLine in Options) then
        FixedBGExtend.Y := Vert.EffectiveLineWidth
      else if Ctl3D then
        EdgeFlags := EdgeFlags or BF_TOP or BF_BOTTOM;
      if not (goVertLine in Options) then
        CellBGExtend.X := Horz.EffectiveLineWidth;
      if not (goHorzLine in Options) then
        CellBGExtend.Y := Vert.EffectiveLineWidth;
    end;

  IsActiveControl := GetIsActiveControl;

  DrawCells(Max(0, UpdateRect.Left), Max(0, UpdateRect.Top),
    Min(FixedCols - 1, UpdateRect.Right),
    Min(FixedRows - 1, UpdateRect.Bottom), [gdFixed]);
  DrawCells(Max(0, UpdateRect.Left), Max(TopRow, UpdateRect.Top),
    Min(FixedCols - 1, UpdateRect.Right),
    Min(TopRow + VisibleRowCount, UpdateRect.Bottom), [gdFixed]);
  DrawCells(Max(LeftCol, UpdateRect.Left), Max(0, UpdateRect.Top),
    Min(LeftCol + VisibleColCount, UpdateRect.Right),
    Min(FixedRows - 1, UpdateRect.Bottom), [gdFixed]);
  DrawCells(Max(LeftCol, UpdateRect.Left), Max(TopRow, UpdateRect.Top),
    Min(LeftCol + VisibleColCount, UpdateRect.Right),
    Min(TopRow + VisibleRowCount, UpdateRect.Bottom), []);

  if ddBackGround in DefaultDrawing then
    with DrawInfo, Canvas do
    begin
      if Horz.GridBoundary < Horz.GridExtent then
      begin
        Brush.Color := Color;
        FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent,
          Vert.GridBoundary));
      end;
      if Vert.GridBoundary < Vert.GridExtent then
      begin
        Brush.Color := Color;
        FillRect(Rect(0, Vert.GridBoundary, Horz.GridExtent, Vert.GridExtent));
      end;
    end;
end;

procedure TStringGridEx.RowMoved(FromIndex, ToIndex: Integer);
var
  iCol, iRow: Integer;
begin
  for iCol := (ColCount - 1) downto 0 do
  begin
    UnMergeCell(iCol, FromIndex);
    if ToIndex > FromIndex then
    begin
      if ToIndex < GetMergeRect(iCol, ToIndex).Bottom then
        UnMergeCell(iCol, ToIndex);
      for iRow := (FromIndex + 1) to ToIndex do
        ExchangeCellData(iCol, iRow, iCol, iRow - 1);
    end
    else
    begin
      if ToIndex > GetMergeRect(iCol, ToIndex).Top then
        UnMergeCell(iCol, ToIndex);
      for iRow := (FromIndex - 1) downto ToIndex do
        ExchangeCellData(iCol, iRow, iCol, iRow + 1);
    end;
  end;
  inherited;
end;

procedure TStringGridEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if HasParent then
    ColWidthsChanged;
end;

procedure TStringGridEx.SetCells(ACol, ARow: Integer; const Value: String);
begin
  if InGrid(ACol, ARow) then
  begin
    FCellData[ACol, ARow].FString := Value;
    InvalidateCell(ACol, ARow);
  end;
end;

procedure TStringGridEx.SetCellsReadOnly(ACol, ARow: Integer;
  const Value: Boolean);
begin
  if InGrid(ACol, ARow) and (ACol >= FixedCols) and (ARow >= FixedRows) then
  begin
    FCellData[ACol, ARow].FReadOnly := Value;
    InvalidateCell(ACol, ARow);
  end;
end;

procedure TStringGridEx.SetCellValue(const ACol, ARow: Integer;
  const Value: String);
const
  SErrString = 'Wrong EditFormat value for text: ''%s''';
  SErrNumber = 'Wrong EditFormat value for number: ''%d''';
  SErrIntVal = 'EditFormat requires a whole number for: ''%s''';
  SErrAbsIntVal = 'EditFormat requires an absolute whole number for: ''%s''';
  SErrFloatVal = 'EditFormat requires a number for: ''%s''';
  SErrAbsFloatVal = 'EditFormat requires an absolute number for: ''%s''';
begin
  if not Assigned(Columns[ACol]) then
  try
    FCellData[ACol, ARow].FString := Value;
    FCellData[ACol, ARow].FValue := StrToFloat(Value);
  except
  end
  else
    with Columns[ACol], FCellData[ACol, ARow] do
    begin
      case InputStyle of
        isString:
          FValue := 0;
        isInteger:
          try
            FValue := StrToInt(Value);
          except
            raise EConvertError.CreateFmt(SErrIntVal, [Value]);
          end;
        isAbsInteger:
          try
            FValue := Abs(StrToInt(Value));
          except
            raise EConvertError.CreateFmt(SErrAbsIntVal, [Value]);
          end;
        isFloat:
          try
            FValue := StrToFloat(Value);
          except
            raise EConvertError.CreateFmt(SErrFloatVal, [Value]);
          end;
        isAbsFloat:
          try
            FValue := Abs(StrToFloat(Value));
          except
            raise EConvertError.CreateFmt(SErrAbsFloatVal, [Value]);
          end;
        isMask:
          try
            FValue := StrToFloat(InplaceEditor.Text);
          except
          end;
      end;
      case InputStyle of
        isString:
          try
            FString := Format(EditFormat, [Value]);
          except
            raise EConvertError.CreateFmt(SErrString, [Value]);
          end;
        isInteger..isAbsFloat:
          try
            FString := Format(EditFormat, [FValue]);
          except
            raise EConvertError.CreateFmt(SErrNumber, [FValue]);
          end;
        isMask:
          FString := InplaceEditor.EditText;
      end;
    end;
end;

procedure TStringGridEx.SetColCount(const Value: Integer);
begin
  if ColCount <> Value then
    if (Value >= Columns.Count) and (not SyncColumns) then
      inherited ColCount := Value;
end;

procedure TStringGridEx.SetColumns(const Value: TStringGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TStringGridEx.SetDefaultDrawing(const Value: TDefaultDrawingModes);
begin
  FDefaultDrawing := Value;
  InvalidateGrid;
end;

procedure TStringGridEx.SetEditText(ACol, ARow: Integer; const Value: String);
begin
  if CanMergedEditShow then
    SetCellValue(ACol, ARow, Value);
end;

procedure TStringGridEx.SetFixedCols(const Value: Integer);
var
  iCol, iRow: Integer;
  UnMergeRect: TGridRect;
begin
  if FixedCols <> Value then
  begin
    for iCol := FixedCols to Min(Value - 1, ColCount -1) do
    begin
      if Assigned(Columns[iCol]) then
        if SameFont(Columns[iCol].Font, Font) then
          Columns[iCol].Font.Assign(FixedFont);
      for iRow := FixedRows to (RowCount - 1) do
        FCellData[iCol, iRow].FReadOnly := False;
    end;
    for iCol := Max(Value, 0) to (FixedCols - 1) do
      if Assigned(Columns[iCol]) then
        if SameFont(Columns[iCol].Font, FixedFont) then
          Columns[iCol].Font.Assign(Font);
    if Value > FixedCols then
    begin
      UnMergeRect.Left := FixedCols;
      UnMergeRect.Top := FixedRows;
      UnMergeRect.Right := Min(Value - 1, ColCount -1);
      UnMergeRect.Bottom := RowCount - 1;
      UnMergeCells(UnMergeRect);
    end;
    inherited FixedCols := Value;
  end;
end;

procedure TStringGridEx.SetFixedFont(const Value: TFont);
begin
  FFixedFont.Assign(Value);
end;

procedure TStringGridEx.SetFixedGridLineColor(const Value: TColor);
begin
  if FFixedGridLineColor <> Value then
  begin
    FFixedGridLineColor := Value;
    InvalidateGrid;
  end;
end;

procedure TStringGridEx.SetFixedRows(const Value: Integer);
var
  iCol, iRow: Integer;
  UnMergeRect: TGridRect;
begin
  if FixedRows <> Value then
  begin
    for iCol := FixedCols to (ColCount - 1) do
      for iRow := FixedRows to Min(Value - 1, RowCount - 1) do
        FCellData[iCol, iRow].FReadOnly := False;
    if Value > FixedRows then
    begin
      UnMergeRect.Left := FixedCols;
      UnMergeRect.Top := FixedRows;
      UnMergeRect.Right := ColCount - 1;
      UnMergeRect.Bottom := Min(Value - 1, RowCount - 1);
      UnMergeCells(UnMergeRect);
    end;
    inherited FixedRows := Value;
  end;
end;

procedure TStringGridEx.SetFocusRectColor(const Value: TColor);
begin
  if FFocusRectColor <> Value then
  begin
    FFocusRectColor := Value;
    if Value <> clDefault then
      FFocusRectStyle := frSolidCustomColor;
    InvalidateCell(Col, Row);
  end;
end;

procedure TStringGridEx.SetFocusRectStyle(const Value: TFocusRectStyle);
begin
  if FFocusRectStyle <> Value then
  begin
    FFocusRectStyle := Value;
    if Value in [frDefault, frSolidAutoBW] then
      FFocusRectColor := clDefault;
    InvalidateCell(Col, Row);
  end;
end;

procedure TStringGridEx.SetGradientColumnColors(const First, Last: TColor;
  const ColumnsOnly: Boolean);
var
  Count: Integer;
  i: Integer;
begin
  Count := Columns.Count - FixedCols;
  if not ColumnsOnly then
    Inc(Count);
  Columns.BeginUpdate;
  try
    for i := 0 to (Columns.Count - 1 - FixedCols) do
      Columns[i + FixedCols].Color := MixColor(First, Last, i / (Count - 1));
    if not ColumnsOnly then
      Color := MixColor(First, Last, 2);
  finally
    Columns.EndUpdate;
  end;
end;

procedure TStringGridEx.SetGridLineColor(const Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    InvalidateGrid;
  end;
end;

procedure TStringGridEx.SetGridLineWidth(const Value: Integer);
//To keep invisible rows and columns invisible
var
  i: Integer;
begin
  if (GridLineWidth <> Value) and (Value >= 0) then
  begin
    for i := 0 to (ColCount - 1) do
      if ColWidths[i] = -GridLineWidth then
        ColWidths[i] := -Value;
    for i := 0 to (RowCount - 1) do
      if RowHeights[i] = -GridLineWidth then
        RowHeights[i] := - Value;
    inherited GridLineWidth := Value;
  end;
end;

procedure TStringGridEx.SetObjects(ACol, ARow: Integer; const Value: TObject);
begin
  if InGrid(ACol, ARow) then
    FCellData[ACol, ARow].FObject := Value;
end;

procedure TStringGridEx.SetReadOnlyColor(const Value: TColor);
begin
  if FReadOnlyColor <> Value then
  begin
    FReadOnlyColor := Value;
    InvalidateGrid;
  end;
end;

procedure TStringGridEx.SetRowColors(const Value: TStringGridRowColors);
begin
  FRowColors.Assign(Value);
end;

procedure TStringGridEx.SetSelectionAlphaBlend(const Value: Boolean);
begin
  if FSelectionAlphaBlend <> Value then
  begin
    FSelectionAlphaBlend := Value;
    InvalidateGridRect(Selection);
  end;
end;

procedure TStringGridEx.SetSelectionAlphaBlendValue(const Value: Byte);
begin
  if FSelectionAlphaBlendValue <> Value then
  begin
    FSelectionAlphaBlendValue := Max(0, Min(Value, 255));
    InvalidateGridRect(Selection);
  end;
end;

procedure TStringGridEx.SetSelectionColor(const Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    if FSelectionColor = clDefault then
      FSelectionColor := clHighLight;
    InvalidateGridRect(Selection);
  end;
end;

procedure TStringGridEx.SetStretchModes(Value: TStretchModes);
begin
  if FStretchModes <> Value then
  begin
    if not (smAllowStretchRight in Value) then
      Exclude(Value, smAllowStretchAll);
    if not (smAllowShrinkRight in Value) then
      Exclude(Value, smAllowShrinkAll);
    FStretchModes := Value;
    ColWidthsChanged;
  end;
end;

procedure TStringGridEx.SetSyncColumns(const Value: Boolean);
begin
  if FSyncColumns <> Value then
  begin
    FSyncColumns := Value;
    UpdateColumns;
  end;
end;

procedure TStringGridEx.SetValues(ACol, ARow: Integer; const Value: Double);
begin
  if InGrid(ACol, ARow) then
  begin
    SetCellValue(ACol, ARow, FloatToStr(Value));
    InvalidateCell(ACol, ARow);
  end;
end;

procedure TStringGridEx.SizeChanged(OldColCount, OldRowCount: Integer);
var
  iCol, iRow: Integer;
begin
  for iCol := (OldColCount - 1) downto ColCount do
    for iRow := 0 to (OldRowCount - 1) do
      UnMergeCell(iCol, iRow);
  for iRow := (OldRowCount - 1) downto RowCount do
    for iCol := 0 to (OldColCount - 1) do
      UnMergeCell(iCol, iRow);
  SetLength(FCellData, ColCount, RowCount);
  if Assigned(Columns) then
    for iCol := 0 to (Columns.Count - 1) do
      for iRow := (OldRowCount) to (RowCount - 1) do
        FCellData[iCol, iRow].FReadOnly := Columns[iCol].ReadOnly;
  inherited;
  if HasParent then
    ColWidthsChanged;
end;

procedure TStringGridEx.UnMergeCell(const ACol, ARow: Integer);
var
  iCol, iRow: Integer;
begin
  if FCellData[ACol, ARow].FMerged then
    with GetMergeRect(ACol, ARow) do
      for iCol := Left to Right do
        for iRow := Top to Bottom do
          FCellData[iCol, iRow].FMerged := False;
end;

procedure TStringGridEx.UnMergeCells(const AGridRect: TGridRect);
var
  iCol, iRow: Integer;
begin
  with AGridRect do
    for iCol := Left to Right do
      for iRow := Top to Bottom do
        UnMergeCell(iCol, iRow);
  InvalidateGridRect(AGridRect);
end;

procedure TStringGridEx.UpdateColumn(const Index: Integer);
begin
  InvalidateCol(Index);
end;

procedure TStringGridEx.UpdateColumns;
begin
  if SyncColumns then
  begin
    if Columns.Count = 0 then
      Columns.Add;
    inherited ColCount := Columns.Count;
  end
  else
    ColCount := Max(ColCount, Columns.Count);
  InvalidateGrid;
end;

procedure TStringGridEx.UpdateColWidths;
var
  i: Integer;
begin
  //Resize Columns to MinWidth or MaxWidth, if nesessary
  for i := 0 to (Columns.Count - 1) do
    with Columns[i] do
    begin
      if MinWidth <> 0 then
        ColWidths[i] := Max(MinWidth, Width);
      if MaxWidth <> 0 then
        ColWidths[i] := Min(MaxWidth, Width);
    end;



//  Space := ClientWidth - GridWidth;
//  if Space > 0 then
//      case FStretchModes of
//        smRightCol: ColWidths[ColCount - 1] := ColWidths[ColCount - 1] + Space;
//        smAllCols: ;
//      end;
end;

procedure TStringGridEx.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if ddBackground in DefaultDrawing then
    Message.Result := 1
  else
    inherited;
end;

end.
