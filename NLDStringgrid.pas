{ *************************************************************************** }
{                                                                             }
{ NLDStringGrid  -  www.nldelphi.com Open Source Delphi designtime library    }
{                                                                             }
{ Initiator: Walterheck                                                       }
{ License: Free to use, free to modify                                        }
{ Website: http://www.nldelphi.com/forum/forumdisplay.php?f=43                }
{ SVN path:                                                                   }
{   http://svn.nldelphi.com/nldelphi/opensource/walterheck/nldstringgrid      }
{                                                                             }
{ *************************************************************************** }
{                                                                             }
{ Edit by: Albert de Weerd (aka NGLN)                                         }
{ Date: March 17, 2009                                                        }
{ Version: 2.0.0.1                                                            }
{                                                                             }
{ *************************************************************************** }

unit NLDStringGrid;

{$BOOLEVAL OFF}

interface

uses
  Classes, SysUtils, NLDSparseList, Grids, Contnrs, Math, Windows, Graphics,
  MaskUtils, Messages, Controls, Forms, ClipBrd, ActnList, Variants;

type
  PCell = ^TCell;
  TCell = packed record
    FString: String;
    FObject: TObject;
    FReadOnly: Boolean;
  end;

  TMemoryOption = (moBeyondGrid, moProportional, moStoreColsRows,
    moSparseStorage);
  TMemoryOptions = set of TMemoryOption;

const
  DefMemoryOptions = [moBeyondGrid, moProportional, moStoreColsRows,
    moSparseStorage];
  MemoryMaxSpeed = [];
  MemoryMinStorage = [moProportional, moSparseStorage];
  MemoryLikeVCL = DefMemoryOptions;

type
  EStringGridError = class(EComponentError);

  TDataGrid = class;

  TGridStrings = class(TStrings)
  private
    FGrid: TDataGrid;
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
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    constructor Create(AGrid: TDataGrid);
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
  end;

  TGridStringsList = class(TSparseList)
  public
    procedure Clear; override;
  end;

  TDataGrid = class(TCustomDrawGrid)
  private
    FCols: TGridStringsList;
    FData: Pointer;
    FGridStrings: TGridStrings;
    FMemoryOptions: TMemoryOptions;
    FOwnsObjects: Boolean;
    FRows: TGridStringsList;
    procedure DisposeData(const NewColCount, NewRowCount: Integer);
    function EnsureData(const ACol, ARow: Integer): PCell;
    function GetCells(const ACol, ARow: Integer): String;
    function GetCols(const Index: Integer): TStrings;
    function GetObjects(const ACol, ARow: Integer): TObject;
    function GetReadOnly(const ACol, ARow: Integer): Boolean;
    function GetRows(const Index: Integer): TStrings;
    function InGrid(const ACol, ARow: Integer;
      const IncludeFixed: Boolean): Boolean;
    procedure InitializeData;
    procedure SetCols(const Index: Integer; const Value: TStrings);
    procedure SetDataSize(const NewColCount, NewRowCount: Integer);
    procedure SetMemoryOptions(const Value: TMemoryOptions);
    procedure SetObjects(const ACol, ARow: Integer; const Value: TObject);
    procedure SetReadOnly(const ACol, ARow: Integer; const Value: Boolean);
    procedure SetRows(const Index: Integer; const Value: TStrings);
  protected
    DataUpdating: LONGBOOL;
    procedure ColumnMoved(FromIndex, ToIndex: Integer); override;
    function GetData(const ACol, ARow: Integer): PCell; virtual;
    procedure InvalidateGridRect(const AGridRect: TGridRect); virtual;
    procedure RowMoved(FromIndex, ToIndex: Integer); override;
    procedure SetCells(const ACol, ARow: Integer; const Value: String); virtual;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    procedure UpdateCell(const ACol, ARow: Integer); virtual;
    property Cells[const ACol, ARow: Integer]: String read GetCells
      write SetCells;
    property Cols[const Index: Integer]: TStrings read GetCols write SetCols;
    property MemoryOptions: TMemoryOptions read FMemoryOptions
      write SetMemoryOptions default DefMemoryOptions;
    property Objects[const ACol, ARow: Integer]: TObject read GetObjects
      write SetObjects;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects
      default False;
    property ReadOnly[const ACol, ARow: Integer]: Boolean read GetReadOnly
      write SetReadOnly;
    property Rows[const Index: Integer]: TStrings read GetRows write SetRows;
  public
    procedure BeginUpdateData; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EndUpdateData; virtual;
    procedure Invalidate; override;
  end;

  PMerging = ^TMerging;
  TMerging = record
    Rect: TGridRect;
    MergeText: Boolean;
    MultiLine: Boolean;
  end;

  TMergings = class(TList)
  private
    function GetItem(const Index: Integer): TMerging;
    procedure SetItem(const Index: Integer; const Value: TMerging);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Add(const AMerging: TMerging): Integer; overload;
    function Add(const ARect: TGridRect; const MergeText,
       MultiLine: Boolean): Integer; overload;
    function Extract(const Index: Integer): TMerging; overload;
    property Items[const Index: Integer]: TMerging read GetItem
      write SetItem; default;
  end;

  TMergeGrid = class(TDataGrid)
  private
    FMergings: TMergings;
  protected
    procedure AdjustSize; reintroduce;
    procedure ColumnMoved(FromIndex: Integer; ToIndex: Integer); override;
    function IsMerged(const ACol, ARow: Integer;
      out Merging: TMerging): Boolean; virtual;
    procedure RowMoved(FromIndex: Integer; ToIndex: Integer); override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MergeCells(const AGridRect: TGridRect; const MergeText: Boolean;
      const MultiLine: Boolean); virtual;
    procedure UnMergeCells(const AGridRect: TGridRect); virtual;
  end;

  TStringGridColumn = class;
  TCustomStringGrid = class;

  TVAlignment = (vaTop, vaCenter, vaBottom);

  TStringGridTitle = class(TPersistent)
  private
    FAlignment: TAlignment;
    FCaption: String;
    FColor: TColor;
    FColumn: TStringGridColumn;
    FFont: TFont;
    FGrid: TCustomStringGrid;
    FMultiLine: Boolean;
    FVAlignment: TVAlignment;
    procedure Changed(const AllColumns: Boolean);
    function GetColor: TColor;
    function GetHeight: Integer;
    function IsCaptionStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
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
    property Caption: String read FCaption write SetCaption
      stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Height: Integer read GetHeight write SetHeight stored False;
    property MultiLine: Boolean read FMultiLine write SetMultiLine
      default False;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment
      default vaTop;
  end;

  TStringGridColumns = class;

  TInputStyle = (isString, isInteger, isAbsInteger, isFloat, isAbsFloat,
    isMask, isCustom);

  TEditStyle = (esSimple, esEllipsis, esPickList, esPickListOnly);

  TColumnActionLink = class(TActionLink)
  protected
    FClient: TStringGridColumn;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    procedure SetVisible(Value: Boolean); override;
  end;

  TColumnActionLinkClass = class of TColumnActionLink;

  TStringGridColumn = class(TCollectionItem)
  private
    FActionLink: TColumnActionLink;
    FAlignment: TAlignment;
    FColor: TColor;
    FColumns: TStringGridColumns;
    FDefaultCellText: String;
    FEditFormat: String;
    FEditMask: TEditMask;
    FEditStyle: TEditStyle;
    FFont: TFont;
    FGrid: TCustomStringGrid;
    FInputStyle: TInputStyle;
    FMaxLength: Integer;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FMultiLine: Boolean;
    FOnTitleClick: TNotifyEvent;
    FPickListItems: TStrings;
    FReadOnly: Boolean;
    FRowNumbers: Boolean;
    FTitle: TStringGridTitle;
    FVAlignment: TVAlignment;
    function DefaultTitleCaption: String;
    procedure DoActionChange(Sender: TObject);
    function GetAction: TBasicAction;
    function GetColor: TColor;
    function GetEditFormat: String;
    function GetFixed: Boolean;
    function GetReadOnly: Boolean;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function IsColorStored: Boolean;
    function IsEditFormatStored: Boolean;
    function IsFontStored: Boolean;
    function IsOnTitleClickStored: Boolean;
    function IsPickListItemsStored: Boolean;
    function IsReadOnlyStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure SetAction(const Value: TBasicAction);
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
    procedure ActionChanged(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoTitleClick; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetActionLinkClass: TColumnActionLinkClass; virtual;
    function GetDisplayName: String; override;
    procedure SetIndex(Value: Integer); override;
    property ActionLink: TColumnActionLink read FActionLink write FActionLink;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure InitiateAction; virtual;
    property Grid: TCustomStringGrid read FGrid;
  published
    property Action: TBasicAction read GetAction write SetAction;
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
    property OnTitleClick: TNotifyEvent read FOnTitleClick write FOnTitleClick
      stored IsOnTitleClickStored;
    property PickListItems: TStrings read FPickListItems
      write SetPickListItems stored IsPickListItemsStored;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly
      stored IsReadOnlyStored;
    property RowNumbers: Boolean read FRowNumbers write SetRowNumbers
      default False;
    property Title: TStringGridTitle read FTitle write SetTitle;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment
      default vaTop;
    property Visible: Boolean read GetVisible write SetVisible
      stored IsVisibleStored;
    property Width: Integer read GetWidth write SetWidth stored False;
  end;

  TColumnsChangedEvent = procedure (Sender: TObject;
    Column: TStringGridColumn) of object;

  TStringGridColumns = class(TCollection)
  private
    FGrid: TCustomStringGrid;
    FOnChanged: TColumnsChangedEvent;
    procedure DoChanged(Item: TCollectionItem);
  protected
    function GetItem(Index: Integer): TStringGridColumn;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
    procedure SetItem(Index: Integer; const Value: TStringGridColumn);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGrid: TCustomStringGrid);
    property Grid: TCustomStringGrid read FGrid;
    property Items[Index: Integer]: TStringGridColumn read GetItem
      write SetItem; default;
  published
    property OnChanged: TColumnsChangedEvent read FOnChanged write FOnChanged;
  end;

  TStringGridRowColors = class(TPersistent)
  private
    FEvenRowColor: TColor;
    FGrid: TCustomStringGrid;
    FIncludeFixed: Boolean;
    FOddRowColor: TColor;
    FOverrideColumnColor: Boolean;
    procedure Changed;
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
    constructor Create(AGrid: TCustomStringGrid);
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
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

  TInplaceEditListEx = class(TInplaceEditList)
  private
    FPickListOnly: Boolean;
    function Grid: TCustomStringGrid;
    function PickListItemIndexOf(const Value: String): Integer;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosMsg);
      message WM_WINDOWPOSCHANGING;
  protected
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
  end;

  TDefaultDrawingMode = (ddBackground, ddEdges, ddGridLines, ddGlyphs, ddText,
    ddFocusRect);
  TDefaultDrawingModes = set of TDefaultDrawingMode;

  TStretchMode = (smAllowStretchRight, smAllowStretchAll, smAllowShrinkRight,
    smAllowShrinkAll);
  TStretchModes = set of TStretchMode;

  TInsertPos = (ipBefore, ipAfter, ipBegin, ipEnd);

  TDeletePos = (dpCurrent, dpBegin, dpEnd, dpRange, dpSelection, dpAll);

  TDrawCellEvent = procedure (Sender: TObject; const ACol, ARow: Integer;
    const ARect: TRect; const State: TGridDrawState;
    const Stage: TDefaultDrawingMode; const StageBegin: Boolean;
    Column: TStringGridColumn) of object;
  TTitleClickEvent = procedure (Sender: TObject; const Index: Integer;
    Column: TStringGridColumn) of object;

  TFocusRectStyle = (frDefault, frSolidAutoBW, frSolidCustomColor);

  TCustomStringGrid = class(TMergeGrid)
  private
    FAutoRowHeights: Boolean;
    FClickCol: Integer;
    FColSizing: Boolean;
    FColumns: TStringGridColumns;
    FColWidthsUpdating: Boolean;
    FDefaultDrawing: TDefaultDrawingModes;
    FDrawInfo: TGridDrawInfo;
    FFixedFont: TFont;
    FFixedFontBackup: TFont;
    FFixedGridLineColor: TColor;
    FFocusRectColor: TColor;
    FFocusRectStyle: TFocusRectStyle;
    FFontBackup: TFont;
    FGridLineColor: TColor;
    FOnDrawCell: TDrawCellEvent;
    FOnEditButtonClick: TNotifyEvent;
    FOnTitleClick: TTitleClickEvent;
    FReadOnlyColor: TColor;
    FRowColors: TStringGridRowColors;
    FSelectionAlphaBlend: Boolean;
    FSelectionAlphaBlendValue: Byte;
    FSelectionColor: TColor;
    FStretchModes: TStretchModes;
    FSyncColumns: Boolean;
    procedure AutoColWidth(const ACol: Integer);
    procedure AutoRowHeight(const ARow: Integer);
    function CalcCoordFromPoint(const X, Y: Integer): TGridCoord;
    function CanColumnMove(const FromIndex, ToIndex: Integer): Boolean;
    function CanEdit: Boolean;
    procedure ChangeEditFormat(const ACol: Integer; const OldFormat,
      NewFormat: String);
    procedure FixedFontChanged(Sender: TObject);
    function GetColCount: Integer;
    function GetFixedCols: Integer;
    function GetFixedRows: Integer;
    function GetGridLineWidth: Integer;
    function GetReadOnlyColor: TColor;
    function GetValues(const ACol, ARow: Integer): Variant;
    procedure InplaceEditorExit(Sender: TObject);
    function IsAlternatingRowColorsStored: Boolean;
    function IsColumnsStored: Boolean;
    function IsFixedFontStored: Boolean;
    function IsReadOnlyColorStored: Boolean;
    procedure SetAutoRowHeights(const Value: Boolean);
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
    procedure SetOnEditButtonClick(const Value: TNotifyEvent);
    procedure SetReadOnlyColor(const Value: TColor);
    procedure SetRowColors(const Value: TStringGridRowColors);
    procedure SetSelectionAlphaBlend(const Value: Boolean);
    procedure SetSelectionAlphaBlendValue(const Value: Byte);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetStretchModes(Value: TStretchModes);
    procedure SetSyncColumns(const Value: Boolean);
    procedure SetValues(const ACol, ARow: Integer; const Value: Variant);
    procedure StretchColumns;
    procedure UpdateColumn(const Index: Integer);
    procedure UpdateColumns;
    procedure UpdateColWidths;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage);
      message CM_PARENTFONTCHANGED;
  protected
    function CanEditAcceptKey(Key: Char): Boolean; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    procedure ColumnMoved(FromIndex, ToIndex: Integer); override;
    procedure ColWidthsChanged; override;
    function CreateEditor: TInplaceEdit; override;
    procedure DblClick; override;
    procedure DoTitleClick(const ACol: Integer); virtual;
    procedure DrawCell(const ACol, ARow: Integer; const ARect: TRect;
      const AState: TGridDrawState; const AStage: TDefaultDrawingMode;
      const StageBegin: Boolean); reintroduce; virtual;
    function EndColumnDrag(var Origin: Integer; var Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function GetCellColor(const ACol, ARow: Integer;
      const AState: TGridDrawState; Column: TStringGridColumn;
      Cell: PCell): TColor; virtual;
    function GetCellDrawFlags(const ARow: Integer; const Merged: Boolean;
      const Merging: TMerging; const AState: TGridDrawState;
      Column: TStringGridColumn): UINT; virtual;
    procedure GetCellFont(AFont: TFont; Column: TStringGridColumn;
      const ARow: Integer; const AState: TGridDrawState); virtual;
    function GetCellText(const ARow: Integer; const Merged: Boolean;
      const Merging: TMerging; Column: TStringGridColumn;
      Cell: PCell): String; virtual;
    function GetEditLimit: Integer; override;
    function GetEditMask(ACol: Integer; ARow: Integer): String; override;
    function GetEditStyle(ACol: Integer; ARow: Integer): Grids.TEditStyle;
      override;
    function GetEditText(ACol: Integer; ARow: Integer): String; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetCells(const ACol, ARow: Integer; const Value: String);
      override;
    procedure SetEditText(ACol: Integer; ARow: Integer;
      const Value: String); override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    procedure WndProc(var Message: TMessage); override;
    property AlternatingRowColors: TStringGridRowColors read FRowColors
      write SetRowColors stored IsAlternatingRowColorsStored;
    property AutoRowHeights: Boolean read FAutoRowHeights
      write SetAutoRowHeights default False;
    property ColCount: Integer read GetColCount write SetColCount default 5;
    property Columns: TStringGridColumns read FColumns write SetColumns
      stored IsColumnsStored;
    property DefaultDrawing: TDefaultDrawingModes read FDefaultDrawing
      write SetDefaultDrawing default [ddBackground, ddEdges, ddGridLines,
      ddGlyphs, ddText, ddFocusRect];
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
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write SetOnEditButtonClick;
    property OnTitleClick: TTitleClickEvent read FOnTitleClick
      write FOnTitleClick;
    property Options default [goFixedVertLine, goFixedHorzLine, goVertLine,
      goHorzLine, goRangeSelect, goThumbTracking];
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
    property Values[const ACol, ARow: Integer]: Variant read GetValues
      write SetValues;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeleteColumn(ACol: Integer); override;
    procedure DeleteRows(const DeletePos: TDeletePos; StartIndex: Integer = -1;
      const Count: Integer = 1);
    destructor Destroy; override;
    procedure ExportCSV(const FileName: TFileName;
      const TitlesFirstRow: Boolean);
    procedure ImportCSV(const FileName: TFileName;
      const TitlesFirstRow: Boolean);
    procedure InsertColumn(const AtIndex: Integer); overload;
    procedure InsertColumn(const Position: TInsertPos = ipBefore); overload;
    procedure InsertRow(const AtIndex: Integer); overload;
    procedure InsertRow(const Position: TInsertPos = ipBefore); overload;
    function IsEmptyColumn(const ACol: Integer): Boolean;
    function IsEmptyRow(const ARow: Integer): Boolean;
    function MouseCoord(const X, Y: Integer): TGridCoord;
    procedure MouseToCell(const X, Y: Integer; var ACol, ARow: Integer);
    procedure MoveColumn(const FromIndex, ToIndex: Integer);
    procedure MoveRow(const FromIndex, ToIndex: Integer);
    procedure ResetAllFonts(AFont: TFont = nil);
    procedure ResetMainColors(const AGridColor: TColor = clWindow;
      const AFixedColor: TColor = clBtnFace);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetGradientColumnColors(const First, Last: TColor;
      const ColumnsOnly: Boolean);
  end;

  TNLDStringGrid = class(TCustomStringGrid)
  public
    property Cells;
    property Cols;
    property Objects;
    property ReadOnly;
    property Rows;
    property Values;
  published
    property AlternatingRowColors;
    property AutoRowHeights;
    property Columns;
    property ColCount;
    property DefaultDrawing;
    property FixedCols;
    property FixedFont;
    property FixedGridLineColor;
    property FixedRows;
    property FocusRectColor;
    property FocusRectStyle;
    property GridLineColor;
    property GridLineWidth;
    property MemoryOptions;
    property OnDrawCell;
    property OnEditButtonClick;
    property OnTitleClick;
    property Options;
    property OwnsObjects;
    property ReadOnlyColor;
    property SelectionAlphaBlend;
    property SelectionAlphaBlendValue;
    property SelectionColor;
    property StretchModes;
    property SyncColumns;
  published
    property Align;
    property Anchors;
    property BiDiMode;
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
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
  end;

implementation

type
  TRGB = record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

function GetRGB(AColor: TColor): TRGB;
begin
  AColor := ColorToRGB(AColor);
  Result.R := GetRValue(AColor);
  Result.G := GetGValue(AColor);
  Result.B := GetBValue(AColor);
end;

function MixColor(const Base, MixWith: TColor; const Factor: Double): TColor;
var
  FBase: TRGB;
  FMixWith: TRGB;
begin
  if Factor <= 0 then
    Result := Base
  else if Factor >= 1 then
    Result := MixWith
  else
  begin
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
end;

function SameFont(Font1, Font2: TFont): Boolean;
begin
  Result := (Font1.Color = Font2.Color) and (Font1.Handle = Font2.Handle);
end;

function MaskGetFirstField(const AEditMask: TEditMask): String;
var
  SeparatorCount: Integer;
begin
  Result := AEditMask;
  SeparatorCount := 0;
  repeat
    if Result[Length(Result)] = MaskFieldSeparator then
      Inc(SeparatorCount);
    Delete(Result, Length(Result), 1);
  until SeparatorCount >= 2;
end;

function CoordInGridRect(const ACol, ARow: Integer;
  const ARect: TGridRect): Boolean;
begin
  Result := (ACol >= ARect.Left) and (ACol <= ARect.Right) and
    (ARow >= ARect.Top) and (ARow <= ARect.Bottom);
end;

function CoordsEqual(const Coord1, Coord2: TGridCoord): Boolean;
begin
  Result := (Coord1.X = Coord2.X) and (Coord1.Y = Coord2.Y);
end;

function GridRectIntersect(const R1, R2: TGridRect): Boolean;
begin
  Result := (R1.Right >= R2.Left) and (R1.Left <= R2.Right) and
    (R1.Bottom >= R2.Top) and (R1.Top <= R2.Bottom);
end;

resourcestring
  SColumnsMissing =
    'Missing Columns collection for %s of %s ''%s''';
  SCreateError =
    'Failed to create %s instance';
  SGridMissing =
    'Missing StringGrid for %s of %s ''%s''';
  SInvalidCollectionType =
    'Invalid collection type %s for setting owner of %s';
  SInvalidColumnInsertion =
    'Cannot insert StringGridColumn: index %d is out of range';
  SInvalidColumnMovement =
    'Cannot move StringGridColumn %s to position %d: index is out of range';
  SInvalidFormat =
    'Invalid value for ''%s'': the input style requires %s';
  SInvalidGridStringsDelete =
    'Cannot delete within grid columns or rows this way';
  SInvalidGridStringsInsert =
    'Cannot insert within grid columns or rows this way';
  SValueString =
    'a character string';
  SValueInteger =
    'an integer value';
  SValueAbsInteger =
    'a positive integer value';
  SValueFloat =
    'a floating point value';
  SValueAbsFloat =
    'a positive floating point value';
  SValueMask =
    'a value according the mask %s';
  SValueCustom =
    'a value that fits in the format %s';

const
  ID_STRETCHTIMER = 23;
  EditFormats: array[TInputStyle] of String = (
    '%s', { isString }
    '%d', { isInteger }
    '%u', { isAbsInteger }
    '%f', { isFloat }
    '%f', { isAbsFloat }
    '',   { isMask }
    '%s'  { isCustom }
  );

function GetDefaultInputStyle(const AEditFormat: String): TInputStyle;
var
  i: TInputStyle;
begin
  Result := isCustom;
  for i := Low(TInputStyle) to High(TInputStyle) do
    if EditFormats[i] = AEditFormat then
    begin
      Result := i;
      Break;
    end;
end;

{ TGridStrings }

procedure TGridStrings.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    FGrid.BeginUpdateData;
    try
      for i := 0 to Min(TStrings(Source).Count, Count) - 1 do
      begin
        Put(i, TStrings(Source).Strings[i]);
        PutObject(i, TStrings(Source).Objects[i]);
      end;
    finally
      FGrid.EndUpdateData;
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TGridStrings.Clear;
var
  i: Integer;
begin
  BeginUpdate;
  FGrid.BeginUpdateData;
  try
    for i := 0 to Count - 1 do
    begin
      Strings[i] := '';
      Objects[i] := nil;
    end;
  finally
    FGrid.EndUpdateData;
    EndUpdate;
  end;
end;

constructor TGridStrings.Create(AGrid: TDataGrid);
begin
  if AGrid = nil then
    raise EStringListError.CreateFmt(SCreateError, [Self.ClassName]);
  inherited Create;
  FGrid := AGrid;
end;

procedure TGridStrings.Delete(Index: Integer);
begin
  raise EInvalidGridOperation.Create(SInvalidGridStringsDelete);
end;

function TGridStrings.Get(Index: Integer): String;
begin
  if FIsCol then
    Result := FGrid.Cells[FIndex, Index]
  else
    Result := FGrid.Cells[Index, FIndex];
end;

function TGridStrings.GetCount: Integer;
begin
  if FIsCol then
    Result := FGrid.RowCount
  else
    Result := FGrid.ColCount;
end;

function TGridStrings.GetObject(Index: Integer): TObject;
begin
  if FIsCol then
    Result := FGrid.Objects[FIndex, Index]
  else
    Result := FGrid.Objects[Index, FIndex];
end;

procedure TGridStrings.Insert(Index: Integer; const S: String);
begin
  raise EInvalidGridOperation.Create(SInvalidGridStringsInsert);
end;

function TGridStrings.LinkGridCol(const AIndex: Integer): TStrings;
begin
  FIndex := AIndex;
  FIsCol := True;
  Result := Self;
end;

function TGridStrings.LinkGridRow(const AIndex: Integer): TStrings;
begin
  FIndex := AIndex;
  FIsCol := False;
  Result := Self;
end;

procedure TGridStrings.Put(Index: Integer; const S: String);
begin
  if FIsCol then
    FGrid.Cells[FIndex, Index] := S
  else
    FGrid.Cells[Index, FIndex] := S;
end;

procedure TGridStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if FIsCol then
    FGrid.Objects[FIndex, Index] := AObject
  else
    FGrid.Objects[Index, FIndex] := AObject;
end;

procedure TGridStrings.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    if FIsCol then
      FGrid.InvalidateCol(FIndex)
    else
      FGrid.InvalidateRow(FIndex);
  inherited SetUpdateState(Updating);
end;

{ TGridStringsList }

procedure TGridStringsList.Clear;

  function FreeItem(const Index: Integer; Item: TGridStrings): Integer;
  begin
    Item.Free;
    Result := 0;
  end;

begin
  ForAll(@FreeItem, False);
  inherited Clear;
end;

{ TMatrix }

type
  TMatrix = class(TObject)
  private
    FItems: array of array of Pointer;
    function GetItem(const X, Y: Integer): Pointer;
    procedure SetItem(const X, Y: Integer; const Value: Pointer);
  public
    procedure Assign(AMatrix: TSparseMatrix);
    procedure AssignTo(AMatrix: TSparseMatrix);
    procedure Exchange(const X1, Y1, X2, Y2: Integer);
    procedure MoveCol(const CurIndex, NewIndex: Integer);
    procedure MoveRow(const CurIndex, NewIndex: Integer);
    property Items[const X, Y: Integer]: Pointer read GetItem
      write SetItem; default;
  end;

procedure TMatrix.Assign(AMatrix: TSparseMatrix);
var
  X: Integer;
  Y: Integer;
begin
  for Y := AMatrix.RowCount - 1 downto 0 do
    if AMatrix.Rows[Y] <> nil then
      for X := AMatrix.Rows[Y].Count - 1 downto 0 do
        SetItem(X, Y, AMatrix[X, Y]);
end;

procedure TMatrix.AssignTo(AMatrix: TSparseMatrix);
var
  X: Integer;
  Y: Integer;
begin
  for X := Length(FItems) - 1 downto 0 do
    for Y := Length(FItems[X]) - 1 downto 0 do
      AMatrix[X, Y] := FItems[X, Y];
end;

procedure TMatrix.Exchange(const X1, Y1, X2, Y2: Integer);
var
  P1: Pointer;
  P2: Pointer;
begin
  P1 := GetItem(X1, Y1);
  P2 := GetItem(X2, Y2);
  if P1 <> nil then
  begin
    if P2 <> nil then
    begin
      FItems[X1, Y1] := P2;
      FItems[X2, Y2] := P1;
    end
    else
    begin
      FItems[X1, Y1] := P2;
      SetItem(X2, Y2, P1);
    end;
  end
  else
    if P2 <> nil then
    begin
      SetItem(X1, Y1, P2);
      FItems[X2, Y2] := P1;
    end;
end;

function TMatrix.GetItem(const X, Y: Integer): Pointer;
begin
  if (Length(FItems) > X) and (Length(FItems[X]) > Y) then
    Result := FItems[X, Y]
  else
    Result := nil;
end;

procedure TMatrix.MoveCol(const CurIndex, NewIndex: Integer);
var
  X: Integer;
  Y: Integer;
begin
  if NewIndex > CurIndex then
    for X := CurIndex + 1 to NewIndex do
      for Y := 0 to Length(FItems[X]) - 1 do
        Exchange(X, Y, X - 1, Y)
  else if NewIndex < CurIndex then
    for X := CurIndex - 1 downto NewIndex do
      for Y := 0 to Length(FItems[X]) - 1 do
        Exchange(X, Y, X + 1, Y);
end;

procedure TMatrix.MoveRow(const CurIndex, NewIndex: Integer);
var
  X: Integer;
  Y: Integer;
begin
  if CurIndex <> NewIndex then
    for X := 0 to Length(FItems) - 1 do
      if NewIndex > CurIndex then
        for Y := CurIndex + 1 to NewIndex do
          Exchange(X, Y, X, Y - 1)
      else
        for Y := CurIndex - 1 downto NewIndex do
          Exchange(X, Y, X, Y + 1);
end;

procedure TMatrix.SetItem(const X, Y: Integer; const Value: Pointer);
begin
  if Length(FItems) <= X then
    SetLength(FItems, X + 1);
  if Length(FItems[X]) <= Y then
    SetLength(FItems[X], Y + 1);
  FItems[X, Y] := Value;
end;

{ TDataGrid }

procedure TDataGrid.BeginUpdateData;
begin
  Inc(DataUpdating);
end;

procedure TDataGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  if moSparseStorage in FMemoryOptions then
    TSparseMatrix(FData).MoveCol(FromIndex, ToIndex)
  else
    TMatrix(FData).MoveCol(FromIndex, ToIndex);
  InvalidateGridRect(TGridRect(
    Rect(Min(FromIndex, ToIndex), 0, Max(FromIndex, ToIndex), RowCount - 1)));
  inherited ColumnMoved(FromIndex, ToIndex);
end;

constructor TDataGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMemoryOptions := DefMemoryOptions;
  InitializeData;
end;

destructor TDataGrid.Destroy;
begin
  if moStoreColsRows in FMemoryOptions then
  begin
    FCols.Free;
    FRows.Free;
  end
  else
    FGridStrings.Free;
  DisposeData(0, 0);
  TObject(FData).Free;
  inherited Destroy;
end;

procedure TDataGrid.DisposeData(const NewColCount, NewRowCount: Integer);

  function DisposeCell(ACol: Integer; Cell: PCell): Integer;
  begin
    if ACol >= NewColCount then
    begin
      if FOwnsObjects then
        Cell^.FObject.Free;
      Dispose(Cell);
      Result := 0;
    end
    else
      Result := 1;
  end;

var
  iCol: Integer;
  iRow: Integer;
begin
  if moSparseStorage in FMemoryOptions then
  begin
    for iRow := NewRowCount to TSparseMatrix(FData).RowCount - 1 do
      if TSparseMatrix(FData).Rows[iRow] <> nil then
        TSparseMatrix(FData).Rows[iRow].ForAll(@DisposeCell, True);
    TSparseMatrix(FData).RowCount := NewRowCount;
  end
  else
    with TMatrix(FData) do
    begin
      for iCol := NewColCount to Length(FItems) - 1 do
        for iRow := NewRowCount to Length(FItems[iCol]) - 1 do
          if FItems[iCol, iRow] <> nil then
          begin
            if FOwnsObjects then
              PCell(FItems[iCol, iRow])^.FObject.Free;
            Dispose(PCell(FItems[iCol, iRow]));
          end;
      SetLength(FItems, Min(Length(FItems), NewColCount));
      for iCol := 0 to Min(Length(FItems), NewColCount) - 1 do
        SetLength(FItems[iCol], Min(Length(FItems[iCol]), NewRowCount));
    end;
end;

procedure TDataGrid.EndUpdateData;
begin
  Dec(DataUpdating);
end;

function TDataGrid.EnsureData(const ACol, ARow: Integer): PCell;
begin
  Result := GetData(ACol, ARow);
  if Result = nil then
  begin
    Result := New(PCell);
    FillChar(Result^, SizeOf(TCell), 0);
    if moSparseStorage in FMemoryOptions then
      TSparseMatrix(FData)[ACol, ARow] := Result
    else
      TMatrix(FData)[ACol, ARow] := Result;
  end;
end;

function TDataGrid.GetCells(const ACol, ARow: Integer): String;
var
  Cell: PCell;
begin
  Cell := GetData(ACol, ARow);
  if Cell = nil then
    Result := ''
  else
    Result := Cell^.FString;
end;

function TDataGrid.GetCols(const Index: Integer): TStrings;
begin
  if moStoreColsRows in FMemoryOptions then
  begin
    Result := FCols[Index];
    if Result = nil then
    begin
      Result := TGridStrings.Create(Self).LinkGridCol(Index);
      FCols[Index] := Result;
    end;
  end
  else
    Result := FGridStrings.LinkGridCol(Index);
end;

function TDataGrid.GetData(const ACol, ARow: Integer): PCell;
begin
  if moSparseStorage in FMemoryOptions then
    Result := TSparseMatrix(FData)[ACol, ARow]
  else
    Result := TMatrix(FData)[ACol, ARow];
end;

function TDataGrid.GetObjects(const ACol, ARow: Integer): TObject;
var
  Cell: PCell;
begin
  Cell := GetData(ACol, ARow);
  if Cell = nil then
    Result := nil
  else
    Result := Cell^.FObject;
end;

function TDataGrid.GetReadOnly(const ACol, ARow: Integer): Boolean;
var
  Cell: PCell;
begin
  Cell := GetData(ACol, ARow);
  if Cell = nil then
    Result := False
  else
    Result := Cell^.FReadOnly;
end;

function TDataGrid.GetRows(const Index: Integer): TStrings;
begin
  if moStoreColsRows in FMemoryOptions then
  begin
    Result := FRows[Index];
    if Result = nil then
    begin
      Result := TGridStrings.Create(Self).LinkGridRow(Index);
      FRows[Index] := Result;
    end;
  end
  else
    Result := FGridStrings.LinkGridRow(Index);
end;

function TDataGrid.InGrid(const ACol, ARow: Integer;
  const IncludeFixed: Boolean): Boolean;
begin
  if moBeyondGrid in FMemoryOptions then
    Result := True
  else
    Result := (ACol < ColCount) and (ARow < RowCount);
  if IncludeFixed then
    Result := Result and (ACol >= 0) and (ARow >= 0)
  else
    Result := Result and (ACol >= FixedCols) and (ARow >= FixedRows);
end;

procedure TDataGrid.InitializeData;
begin
  if moSparseStorage in FMemoryOptions then
    FData := TSparseMatrix.Create(True)
  else
    FData := TMatrix.Create;
  if moStoreColsRows in FMemoryOptions then
  begin
    FCols := TGridStringsList.Create(True);
    FRows := TGridStringsList.Create(True);
  end
  else
    FGridStrings := TGridStrings.Create(Self);
end;

procedure TDataGrid.Invalidate;
begin
  if not DataUpdating then
    inherited Invalidate;
end;

procedure TDataGrid.InvalidateGridRect(const AGridRect: TGridRect);
var
  ARect: TRect;
begin
  if not DataUpdating then
  begin
    with AGridRect do
      ARect := BoxRect(Left, Top, Right, Bottom);
    InvalidateRect(Handle, @ARect, True);
  end;
end;

procedure TDataGrid.RowMoved(FromIndex, ToIndex: Integer);
begin
  if moSparseStorage in FMemoryOptions then
    TSparseMatrix(FData).MoveRow(FromIndex, ToIndex)
  else
    TMatrix(FData).MoveRow(FromIndex, ToIndex);
  InvalidateGridRect(TGridRect(
    Rect(0, Min(FromIndex, ToIndex), ColCount - 1, Max(FromIndex, ToIndex))));
  inherited RowMoved(FromIndex, ToIndex);
end;

procedure TDataGrid.SetCells(const ACol, ARow: Integer; const Value: String);
var
  Cell: PCell;
begin
  if InGrid(ACol, ARow, True) then
  begin
    Cell := EnsureData(ACol, ARow);
    if Cell^.FString <> Value then
    begin
      Cell^.FString := Value;
      UpdateCell(ACol, ARow);
    end;
  end;
end;

procedure TDataGrid.SetCols(const Index: Integer; const Value: TStrings);
begin
  GetCols(Index).Assign(Value);
end;

procedure TDataGrid.SetDataSize(const NewColCount, NewRowCount: Integer);
var
  iCol: Integer;
begin
  if not (moBeyondGrid in FMemoryOptions) then
    DisposeData(ColCount, RowCount);
  if not (moProportional in FMemoryOptions) then
    if not (moSparseStorage in FMemoryOptions) then
      with TMatrix(FData) do
      begin
        SetLength(FItems, NewColCount);
        for iCol := 0 to NewColCount - 1 do
          SetLength(FItems[iCol], Max(Length(FItems[iCol]), NewRowCount));
      end;
end;

procedure TDataGrid.SetMemoryOptions(const Value: TMemoryOptions);
var
  Temp: Pointer;
begin
  if FMemoryOptions <> Value then
  begin
    if (moStoreColsRows in FMemoryOptions) <> (moStoreColsRows in Value) then
      if moStoreColsRows in Value then
      begin
        FreeAndNil(FGridStrings);
        FCols := TGridStringsList.Create(True);
        FRows := TGridStringsList.Create(True);
      end
      else
      begin
        FGridStrings := TGridStrings.Create(Self);
        FreeAndNil(FCols);
        FreeAndNil(FRows);
      end;
    if (moSparseStorage in FMemoryOptions) <> (moSparseStorage in Value) then
      if moSparseStorage in Value then
      begin
        Temp := TSparseMatrix.Create(True);
        TMatrix(FData).AssignTo(Temp);
        TObject(FData).Free;
        FData := Temp;
      end
      else
      begin
        Temp := TMatrix.Create;
        TMatrix(Temp).Assign(FData);
        TObject(FData).Free;
        FData := Temp;
      end;
    FMemoryOptions := Value;
    if moSparseStorage in FMemoryOptions then
      Include(FMemoryOptions, moProportional);
    SetDataSize(ColCount, RowCount);
  end;
end;

procedure TDataGrid.SetObjects(const ACol, ARow: Integer;
  const Value: TObject);
var
  Cell: PCell;
begin
  if InGrid(ACol, ARow, True) then
  begin
    Cell := EnsureData(ACol, ARow);
    if Cell^.FObject <> Value then
    begin
      Cell^.FObject := Value;
      UpdateCell(ACol, ARow);
    end;
  end;
end;

procedure TDataGrid.SetReadOnly(const ACol, ARow: Integer;
  const Value: Boolean);
var
  Cell: PCell;
begin
  if InGrid(ACol, ARow, True) then
  begin
    Cell := EnsureData(ACol, ARow);
    if Cell^.FReadOnly <> Value then
    begin
      Cell^.FReadOnly := Value;
      UpdateCell(ACol, ARow);
    end;
  end;
end;

procedure TDataGrid.SetRows(const Index: Integer; const Value: TStrings);
begin
  GetRows(Index).Assign(Value);
end;

procedure TDataGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  SetDataSize(ColCount, RowCount);
  inherited SizeChanged(OldColCount, OldRowCount);
end;

procedure TDataGrid.UpdateCell(const ACol, ARow: Integer);
begin
  if not DataUpdating then
    InvalidateCell(ACol, ARow);
end;

{ TMergings }

function TMergings.Add(const AMerging: TMerging): Integer;
begin
  if CoordsEqual(AMerging.Rect.TopLeft, AMerging.Rect.BottomRight) then
    Result := -1
  else
  begin
    Result := inherited Add(New(PMerging));
    Items[Result] := AMerging;
  end;
end;

function TMergings.Add(const ARect: TGridRect; const MergeText,
  MultiLine: Boolean): Integer;
var
  Merging: TMerging;
begin
  Merging.Rect := ARect;
  Merging.MergeText := MergeText;
  Merging.MultiLine := MultiLine;
  Result := Add(Merging);
end;

function TMergings.Extract(const Index: Integer): TMerging;
begin
  Result := Items[Index];
  Delete(Index);
end;

function TMergings.GetItem(const Index: Integer): TMerging;
begin
  Result := PMerging(inherited Items[Index])^;
end;

procedure TMergings.Notify(Ptr: Pointer; Action: TListNotification);
begin
  case Action of
    lnAdded:
      if Ptr = nil then
        Ptr := New(PMerging);
    lnDeleted,
    lnExtracted:
      Dispose(Ptr);
  end;
  inherited Notify(Ptr, Action);
end;

procedure TMergings.SetItem(const Index: Integer; const Value: TMerging);
begin
  PMerging(inherited Items[Index])^ := Value;
end;

{ TMergeGrid }

procedure TMergeGrid.AdjustSize;
begin
{ Grids.TCustomGrid.AdjustSize is obsolete and gives wrong result for this
  descendant. Instead, we use the methods DeleteColumn, DeleteRows,
  InsertColumn and InsertRow, which take care of possible Merged Cells. }
end;

procedure TMergeGrid.ColumnMoved(FromIndex, ToIndex: Integer);
var
  i: Integer;
  MergeRect: TGridRect;
begin
  for i := FMergings.Count - 1 downto 0 do
  begin
    MergeRect := FMergings[i].Rect;
    with MergeRect do
      if ((Left <= FromIndex) and (Right >= FromIndex)) or
        ((Left <= ToIndex) and (Right >= ToIndex)) then
      begin
        FMergings.Delete(i);
        InvalidateGridRect(MergeRect);
      end;
  end;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

constructor TMergeGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMergings := TMergings.Create;
end;

destructor TMergeGrid.Destroy;
begin
  FMergings.Free;
  inherited Destroy;
end;

function TMergeGrid.IsMerged(const ACol, ARow: Integer;
  out Merging: TMerging): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FMergings.Count - 1 do
    if CoordInGridRect(ACol, ARow, FMergings[i].Rect) then
    begin
      Result := True;
      Merging := FMergings[i];
      Break;
    end;
end;

procedure TMergeGrid.MergeCells(const AGridRect: TGridRect;
  const MergeText, MultiLine: Boolean);
begin
  with AGridRect do
    if (Left >= FixedCols) and (Top >= FixedRows) and (Right < Colcount) and
      (Bottom < RowCount) then
    begin
      UnMergeCells(AGridRect);
      FMergings.Add(AGridRect, MergeText, MultiLine);
      InvalidateGridRect(AGridRect);
    end;
end;

procedure TMergeGrid.RowMoved(FromIndex, ToIndex: Integer);
var
  i: Integer;
  MergeRect: TGridRect;
begin
  for i := FMergings.Count - 1 downto 0 do
  begin
    MergeRect := FMergings[i].Rect;
    with MergeRect do
      if ((Top <= FromIndex) and (Bottom >= FromIndex)) or
        ((Top <= ToIndex) and (Bottom >= ToIndex)) then
      begin
        FMergings.Delete(i);
        InvalidateGridRect(MergeRect);
      end;
  end;
  inherited RowMoved(FromIndex, ToIndex);
end;

procedure TMergeGrid.SizeChanged(OldColCount, OldRowCount: Integer);
var
  i: Integer;
  MergeRect: TGridRect;
  Merging: TMerging;
begin
  if ColCount < OldColCount then
    for i := FMergings.Count - 1 downto 0 do
      if FMergings[i].Rect.Left >= ColCount then
      begin
        MergeRect := FMergings.Extract(i).Rect;
        InvalidateGridRect(MergeRect);
      end
      else if FMergings[i].Rect.Right >= ColCount then
      begin
        Merging := FMergings[i];
        Merging.Rect.Right := ColCount - 1;
        FMergings[i] := Merging;
        InvalidateGridRect(Merging.Rect);
      end;
  if RowCount < OldRowCount then
    for i := FMergings.Count - 1 downto 0 do
      if FMergings[i].Rect.Top >= RowCount then
      begin
        MergeRect := FMergings.Extract(i).Rect;
        InvalidateGridRect(MergeRect);
      end
      else if FMergings[i].Rect.Bottom >= ColCount then
      begin
        Merging := FMergings[i];
        Merging.Rect.Bottom := RowCount - 1;
        FMergings[i] := Merging;
        InvalidateGridRect(Merging.Rect);
      end;
  inherited SizeChanged(OldColCount, OldRowCount);
end;

procedure TMergeGrid.UnMergeCells(const AGridRect: TGridRect);
var
  i: Integer;
  MergeRect: TGridRect;
begin
  for i := FMergings.Count - 1 downto 0 do
    if GridRectIntersect(AGridRect, FMergings[i].Rect) then
    begin
      MergeRect := FMergings.Extract(i).Rect;
      InvalidateGridRect(MergeRect);
    end;
end;

{ TStringGridTitle }

procedure TStringGridTitle.Assign(Source: TPersistent);
begin
  if Source is TStringGridTitle then
  begin
    if (FColumn <> nil) and (FColumn.Collection <> nil) then
      FColumn.Collection.BeginUpdate;
    try
      FAlignment := TStringGridTitle(Source).FAlignment;
      FCaption := TStringGridTitle(Source).FCaption;
      FColor := TStringGridTitle(Source).FColor;
      FFont.Assign(TStringGridTitle(Source).FFont);
      FMultiLine := TStringGridTitle(Source).FMultiLine;
      FVAlignment := TStringGridTitle(Source).FVAlignment;
    finally
      if (FColumn <> nil) and (FColumn.Collection <> nil) then
        FColumn.Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TStringGridTitle.Changed(const AllColumns: Boolean);
begin
  if FColumn <> nil then
    FColumn.Changed(AllColumns);
end;

constructor TStringGridTitle.Create(AColumn: TStringGridColumn);
begin
  inherited Create;
  FColumn := AColumn;
  FColor := clDefault;
  FFont := TFont.Create;
  if FColumn <> nil then
    FGrid := FColumn.Grid;
  if FGrid <> nil then
    FFont.Assign(FGrid.FixedFont);
  FFont.OnChange := FontChanged;
end;

destructor TStringGridTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TStringGridTitle.FontChanged(Sender: TObject);
begin
  Changed(False);
end;

function TStringGridTitle.GetColor: TColor;
begin
  if (FColor = clDefault) and (FGrid <> nil) then
    Result := FGrid.FixedColor
  else
    Result := FColor;
end;

function TStringGridTitle.GetHeight: Integer;
begin
  if FGrid <> nil then
    Result := FGrid.RowHeights[0]
  else
    Result := -1;
end;

function TStringGridTitle.IsCaptionStored: Boolean;
begin
  Result := (FColumn = nil) or (FColumn.ActionLink = nil) or
    not FColumn.ActionLink.IsCaptionLinked;
end;

function TStringGridTitle.IsColorStored: Boolean;
begin
  Result := FColor <> clDefault;
end;

function TStringGridTitle.IsFontStored: Boolean;
begin
  if FGrid <> nil then
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
      FCaption := ' '; { Due to no designtime-storage if empty. IDE bug? }
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
  if FGrid <> nil then
    FGrid.RowHeights[0] := Value
  else
    if FColumn <> nil then
      raise EStringGridError.CreateFmt(SGridMissing,
        ['setting title height', FColumn.ClassName, FColumn.DisplayName])
    else
      raise EStringGridError.CreateFmt(SGridMissing,
        ['setting height', ClassName, Caption]);
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

{ TColumnActionLink }

procedure TColumnActionLink.AssignClient(AClient: TObject);
begin
  FClient := TStringGridColumn(AClient);
end;

function TColumnActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and (Action is TCustomAction) and
    (FClient.Title.Caption = TCustomAction(Action).Caption);
end;

function TColumnActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and (Action is TCustomAction) and
    (FClient.ReadOnly <> TCustomAction(Action).Enabled);
end;

function TColumnActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@FClient.OnTitleClick = @Action.OnExecute);
end;

function TColumnActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and (Action is TCustomAction) and
    (FClient.Visible = TCustomAction(Action).Visible);
end;

procedure TColumnActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.Title.Caption := Value;
end;

procedure TColumnActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.ReadOnly := not Value;
end;

procedure TColumnActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    FClient.OnTitleClick := Value;
end;

procedure TColumnActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;

{ TStringGridColumn }

procedure TStringGridColumn.ActionChanged(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Title.Caption = DefaultTitleCaption) then
        FTitle.Caption := Caption;
      if not CheckDefaults or (not ReadOnly) then
        ReadOnly := not Enabled;
      if not CheckDefaults or not Assigned(FOnTitleClick) then
        FOnTitleClick := OnExecute;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      Changed(False);
    end;
end;

procedure TStringGridColumn.Assign(Source: TPersistent);
begin
  if Source is TStringGridColumn then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      Action := TStringGridColumn(Source).Action;
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
      FOnTitleClick := TStringGridColumn(Source).FOnTitleClick;
      FPickListItems.Assign(TStringGridColumn(Source).FPickListItems);
      FReadOnly := TStringGridColumn(Source).FReadOnly;
      FRowNumbers := TStringGridColumn(Source).FRowNumbers;
      FTitle.Assign(TStringGridColumn(Source).FTitle);
      FVAlignment := TStringGridColumn(Source).FVAlignment;
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TStringGridColumn.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      Enabled := not ReadOnly;
      Caption := FTitle.Caption;
      Visible := Self.Visible;
      OnExecute := FOnTitleClick;
    end
  else
    inherited AssignTo(Dest);
end;

constructor TStringGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := clDefault;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FPickListItems := TStringList.Create;
  if Collection <> nil then
    if Collection is TStringGridColumns then
    begin
      FColumns := TStringGridColumns(Collection);
      if FColumns.Grid <> nil then
      begin
        FGrid := FColumns.Grid;
        if Fixed then
          FFont.Assign(FGrid.FixedFont)
        else
          FFont.Assign(FGrid.Font);
      end;
    end
    else
      raise EStringGridError.CreateFmt(SInvalidCollectionType,
        [Collection.ClassName, 'StringGridColumn ' + DefaultTitleCaption]);
  FTitle := TStringGridTitle.Create(Self);
  FTitle.FCaption := DefaultTitleCaption;
end;

function TStringGridColumn.DefaultTitleCaption: String;
begin
  Result := 'Column' + IntToStr(Index);
end;

destructor TStringGridColumn.Destroy;
begin
  Destroying := True;
  FreeAndNil(FActionLink);
  FPickListItems.Free;
  FTitle.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TStringGridColumn.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChanged(Sender, False);
end;

procedure TStringGridColumn.DoTitleClick;
begin
  if Assigned(FOnTitleClick) then
    if (Action <> nil) and (@FOnTitleClick <> @Action.OnExecute) then
      FOnTitleClick(Self)
    else if FActionLink = nil then
      FOnTitleClick(Self)
    else if FActionLink <> nil then
      if (FGrid <> nil) and not (csDesigning in FGrid.ComponentState) then
      begin
        if not FActionLink.Execute(FGrid) then
          FOnTitleClick(Self);
      end
      else
        if not FActionLink.Execute(nil) then
          FOnTitleClick(Self);
end;

procedure TStringGridColumn.FontChanged(Sender: TObject);
begin
  Changed(False);
end;

function TStringGridColumn.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

function TStringGridColumn.GetActionLinkClass: TColumnActionLinkClass;
begin
  Result := TColumnActionLink;
end;

function TStringGridColumn.GetColor: TColor;
begin
  if (FColor = clDefault) and (FGrid <> nil) then
  begin
    if Fixed then
      Result := FGrid.FixedColor
    else
      Result := FGrid.Color;
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
    if (InputStyle = isString) and (MaxLength > 0) then
      Result := '%.' + IntToStr(MaxLength) + 's'
    else
      Result := EditFormats[InputStyle];
end;

function TStringGridColumn.GetFixed: Boolean;
begin
  if FGrid <> nil then
    Result := Index < Grid.FixedCols
  else
    Result := False;
end;

function TStringGridColumn.GetReadOnly: Boolean;
begin
  Result := FReadOnly or Fixed;
end;

function TStringGridColumn.GetVisible: Boolean;
begin
  if FGrid <> nil then
    Result := FGrid.ColWidths[Index] > 0
  else
    Result := False;
end;

function TStringGridColumn.GetWidth: Integer;
begin
  if FGrid <> nil then
    Result := FGrid.ColWidths[Index]
  else
    Result := -1;
end;

procedure TStringGridColumn.InitiateAction;
begin
  if FActionLink <> nil then
    FActionLink.Update;
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
  if FGrid <> nil then
    Result := not SameFont(FFont, FGrid.Font)
  else
    Result := True;
end;

function TStringGridColumn.IsOnTitleClickStored: Boolean;
begin
  Result := (FActionLink = nil) or not ActionLink.IsOnExecuteLinked;
end;

function TStringGridColumn.IsPickListItemsStored: Boolean;
begin
  Result := PickListItems.Count > 0;
end;

function TStringGridColumn.IsReadOnlyStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsEnabledLinked;
  if Result then
    Result := FReadOnly;
end;

function TStringGridColumn.IsVisibleStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsVisibleLinked;
  if Result then
    Result := not Visible;
end;

procedure TStringGridColumn.SetAction(const Value: TBasicAction);
begin
  if Value = nil then
    FreeAndNil(FActionLink)
  else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChanged(Value, csLoading in Value.ComponentState);
    if FGrid <> nil then
      Value.FreeNotification(FGrid);
  end;
  Changed(False);
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
    if FGrid <> nil then
      FGrid.ChangeEditFormat(Index, FEditFormat, Value);
    FEditFormat := Value;
    FInputStyle := GetDefaultInputStyle(FEditFormat);
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
    if FGrid <> nil then
    begin
      if Value then
        FGrid.FixedCols := Index + 1
      else
        FGrid.FixedCols := Index;
    end
    else
      raise EStringGridError.CreateFmt(SGridMissing,
        ['setting fixation', ClassName, DisplayName]);
end;

procedure TStringGridColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TStringGridColumn.SetIndex(Value: Integer);
begin
  if FColumns = nil then
    raise EStringGridError.CreateFmt(SColumnsMissing,
      ['setting index', ClassName, DisplayName])
  else
    if ((Value < 0) or (Value >= FColumns.Count)) then
      raise EStringGridError.CreateFmt(SInvalidColumnMovement,
        [DisplayName, Value]);
  FColumns.BeginUpdate;
  try
    if (FGrid <> nil) and (FGrid.FGridState = gsNormal) then
      FGrid.MoveColumn(Index, Value)
    else
      inherited SetIndex(Value);
  finally
    FColumns.EndUpdate;
  end;
end;

procedure TStringGridColumn.SetInputStyle(const Value: TInputStyle);
begin
  if FInputStyle <> Value then
  begin
    FInputStyle := Value;
    EditMask := '';
    FMaxLength := 0;
  end;
end;

procedure TStringGridColumn.SetMaxLength(const Value: Integer);
begin
  if InputStyle in [isString, isCustom] then
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
  if (FReadOnly <> Value) and (not Fixed) then
  begin
    FReadOnly := Value;
    if FGrid <> nil then
      with FGrid do
      begin
        BeginUpdateData;
        try
          for iRow := FixedRows to RowCount - 1 do
            Readonly[Index, iRow] := FReadOnly;
        finally
          EndUpdateData;
        end;
      end;
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
  if Visible <> Value then
    if FGrid <> nil then
    begin
      if Value then
        Width := FGrid.DefaultColWidth
      else
        Width := -FGrid.GridLineWidth;
      FColumns.DoChanged(Self);
    end
    else
      raise EStringGridError.CreateFmt(SGridMissing,
        ['setting visibility', ClassName, DisplayName]);
end;

procedure TStringGridColumn.SetWidth(const Value: Integer);
begin
  if FGrid <> nil then
  begin
    FGrid.ColWidths[Index] := Value;
    FColumns.DoChanged(Self);
  end
  else
    raise EStringGridError.CreateFmt(SGridMissing,
      ['setting width', ClassName, DisplayName]);
end;

{ TStringGridColumns }

constructor TStringGridColumns.Create(AGrid: TCustomStringGrid);
begin
  FGrid := AGrid;
  inherited Create(TStringGridColumn);
end;

procedure TStringGridColumns.DoChanged(Item: TCollectionItem);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, TStringGridColumn(Item));
end;

function TStringGridColumns.GetItem(Index: Integer): TStringGridColumn;
begin
  if Index > Count - 1 then
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
  if FGrid <> nil then
    case Action of
      cnAdded:
        if FGrid.ColCount > Count then
        begin
          BeginUpdate;
          try
            FGrid.ColCount := FGrid.ColCount + 1;
            FGrid.MoveColumn(FGrid.ColCount - 1, Count - 1);
          finally
            EndUpdate;
          end;
        end;
      cnDeleting,
      cnExtracting:
        if not (csDestroying in FGrid.ComponentState) then
          FGrid.DeleteColumn(Item.Index);
    end;
  inherited Notify(Item, Action);
end;

procedure TStringGridColumns.SetItem(Index: Integer;
  const Value: TStringGridColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TStringGridColumns.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FGrid <> nil then
    if Item <> nil then
      FGrid.UpdateColumn(Item.Index)
    else
      FGrid.UpdateColumns;
  DoChanged(Item);
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
    if FGrid <> nil then
      FGrid.InvalidateGrid;
  end
  else
    inherited Assign(Source);
end;

procedure TStringGridRowColors.Changed;
begin
  if FGrid <> nil then
    FGrid.InvalidateGrid;
end;

constructor TStringGridRowColors.Create(AGrid: TCustomStringGrid);
begin
  inherited Create;
  FGrid := AGrid;
  Reset;
end;

function TStringGridRowColors.GetEvenRowColor: TColor;
begin
  if (FEvenRowColor = clDefault) and (FGrid <> nil) then
    Result := FGrid.Color
  else
    Result := FEvenRowColor;
end;

function TStringGridRowColors.GetOddRowColor: TColor;
begin
  if (FOddRowColor = clDefault) and (FGrid <> nil) then
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

procedure TStringGridRowColors.Reset;
begin
  FIncludeFixed := False;
  FOverrideColumnColor := True;
  FEvenRowColor := clDefault;
  FOddRowColor := clDefault;
  Changed;
end;

procedure TStringGridRowColors.SetEvenRowColor(const Value: TColor);
begin
  if FEvenRowColor <> Value then
  begin
    FEvenRowColor := Value;
    Changed;
  end;
end;

procedure TStringGridRowColors.SetIncludeFixed(const Value: Boolean);
begin
  if FIncludeFixed <> Value then
  begin
    FIncludeFixed := Value;
    Changed;
  end;
end;

procedure TStringGridRowColors.SetOddRowColor(const Value: TColor);
begin
  if FOddRowColor <> Value then
  begin
    FOddRowColor := Value;
    Changed;
  end;
end;

procedure TStringGridRowColors.SetOverrideColumnColor(
  const Value: Boolean);
begin
  if FOverrideColumnColor <> Value then
  begin
    FOverrideColumnColor := Value;
    Changed;
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
  inherited DoDropDownKeys(Key, Shift);
end;

function TInplaceEditListEx.Grid: TCustomStringGrid;
begin
  Result := TCustomStringGrid(inherited Grid);
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
  inherited UpdateContents;
  with Grid do
  begin
    Self.Color := GetCellColor(Col, Row, [], Columns[Col], GetData(Col, Row));
    GetCellFont(Self.Font, Columns[Col], Row, []);
    if (EditStyle = Grids.esPickList) and (Columns[Col] <> nil) then
    begin
      PickList.Items.Assign(Columns[Col].PickListItems);
      FPickListOnly := Columns[Col].EditStyle = esPickListOnly;
    end;
  end;
end;

procedure TInplaceEditListEx.WMPaste(var Message: TWMPaste);
var
  ClipBoardText: Variant;
  MsgParam: String;
  SaveEditText: String;
  Column: TStringGridColumn;
  DummyInt: Integer;
  DummyFloat: Single;
begin
  if Grid.Col >= Grid.Columns.Count then
    inherited
  else
  begin
    Clipboard.Open;
    if ClipBoard.HasFormat(CF_TEXT) then
      ClipboardText := Clipboard.AsText
    else
      ClipBoardText := '';
    Clipboard.Close;
    MsgParam := '';
    SaveEditText := EditText;
    inherited;
    Column := Grid.Columns[Grid.Col];
    case Column.InputStyle of
      isString:
        ;
      isInteger:
        if not TryStrToInt(EditText, DummyInt) then
          MsgParam := SValueInteger;
      isAbsInteger:
        if (not TryStrToInt(EditText, DummyInt)) or (DummyInt < 0) then
          MsgParam := SValueAbsInteger;
      isFloat:
        if not TryStrToFloat(EditText, DummyFloat) then
          MsgParam := SValueFloat;
      isAbsFloat:
        if (not TryStrToFloat(EditText, DummyFloat)) or (DummyFloat < 0) then
          MsgParam := SValueAbsFloat;
      isMask:
        if not Validate(EditText, DummyInt) then
          MsgParam := Format(SValueMask, [MaskGetFirstField(EditMask)]);
      isCustom:
        try
          EditText := Format(Column.EditFormat, [ClipboardText]);
        except
          on EConvertError do
            MsgParam := SValueCustom;
          else
            raise;
        end;
    end;
    if MsgParam <> '' then
    begin
      EditText := SaveEditText;
      raise EConvertError.CreateFmt(SInvalidFormat, [Column.Title.Caption,
        MsgParam]);
    end;
  end;
end;

procedure TInplaceEditListEx.WMWindowPosChanging(var Message: TWMWindowPosMsg);
var
  Merging: TMerging;
begin
  with Grid do
    if CanEdit and IsMerged(Col, Row, Merging) and Merging.MergeText then
      with Merging.Rect do
        with BoxRect(Left, Top, Right, Bottom) do
        begin
          Message.WindowPos.x := Left;
          Message.WindowPos.y := Top;
          Message.WindowPos.cx := Right - Left;
          Message.WindowPos.cy := Bottom - Top;
        end;
  if Message.WindowPos.flags and SWP_HIDEWINDOW = SWP_HIDEWINDOW then
    DoExit;
  inherited;
end;

procedure TInplaceEditListEx.WndProc(var Message: TMessage);
begin
  if EditStyle = Grids.esPickList then
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
          if FPickListOnly then
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
  inherited WndProc(Message);
end;

{ TCustomStringGrid }

procedure TCustomStringGrid.AutoColWidth(const ACol: Integer);
var
  MaxWidth: Integer;
  iRow: Integer;
  Merged: Boolean;
  Merging: TMerging;
begin
  if (ACol >= 0) and (ACol < ColCount) and
    (((Columns[ACol] = nil) or not Columns[ACol].MultiLine)) then
  begin
    MaxWidth := -GridLineWidth;
    for iRow := 0 to RowCount - 1 do
    begin
      if iRow < FixedRows then
        GetCellFont(Canvas.Font, Columns[ACol], iRow, [gdFixed])
      else
        GetCellFont(Canvas.Font, Columns[ACol], iRow, []);
      Merged := IsMerged(ACol, iRow, Merging);
      MaxWidth := Max(MaxWidth, Canvas.TextWidth(GetCellText(iRow, Merged,
        Merging, Columns[ACol], GetData(ACol, iRow))));
    end;
    Inc(MaxWidth, GridLineWidth + 4);
    ColWidths[ACol] := MaxWidth;
  end;
end;

procedure TCustomStringGrid.AutoRowHeight(const ARow: Integer);
const
  DrawStates: array[Boolean] of TGridDrawState = ([], [gdFixed]);
var
  MaxHeight: Integer;
  iCol: Integer;
  State: TGridDrawState;
  R: TRect;
  Merged: Boolean;
  Merging: TMerging;
  Txt: String;
  Flags: UINT;
begin
  if (ARow >=0) and (ARow < RowCount) then
  begin
    MaxHeight := -GridLineWidth;
    for iCol := 0 to ColCount - 1 do
    begin
      State := DrawStates[ARow < FixedRows];
      GetCellFont(Canvas.Font, Columns[iCol], ARow, State);
      R := Rect(4, 4, ColWidths[iCol] - GridLineWidth,
        RowHeights[ARow] - GridLineWidth);
      Merged := IsMerged(iCol, ARow, Merging);
      Txt := GetCellText(ARow, Merged, Merging, Columns[iCol],
        GetData(iCol, ARow));
      Flags := GetCellDrawFlags(ARow, Merged, Merging, State, Columns[iCol]);
      MaxHeight := Max(MaxHeight,
        DrawText(Canvas.Handle, PChar(Txt), -1, R, Flags or DT_CALCRECT));
    end;
    Inc(MaxHeight, GridLineWidth + 4);
    RowHeights[ARow] := MaxHeight;
  end;
end;

function TCustomStringGrid.CalcCoordFromPoint(const X, Y: Integer): TGridCoord;

  function DoCalc(const AxisInfo: TGridAxisDrawInfo; const N: Integer): Integer;
  var
    i: Integer;
    Start: Integer;
    Stop: Integer;
    Line: Integer;
  begin
    with AxisInfo do
    begin
      if N < FixedBoundary then
      begin
        Start := 0;
        Stop :=  FixedCellCount - 1;
        Line := 0;
      end
      else
      begin
        Start := FirstGridCell;
        Stop := GridCellCount - 1;
        Line := FixedBoundary;
      end;
      Result := -1;
      for i := Start to Stop do
      begin
        Inc(Line, GetExtent(i) + EffectiveLineWidth);
        if N < Line then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;

  function DoCalcRightToLeft(const AxisInfo: TGridAxisDrawInfo;
    N: Integer): Integer;
  var
    I: Integer;
    Start: Integer;
    Stop: Integer;
    Line: Integer;
  begin
    N := ClientWidth - N;
    with AxisInfo do
    begin
      if N < FixedBoundary then
      begin
        Start := 0;
        Stop :=  FixedCellCount - 1;
        Line := ClientWidth;
      end
      else
      begin
        Start := FirstGridCell;
        Stop := GridCellCount - 1;
        Line := FixedBoundary;
      end;
      Result := -1;
      for i := Start to Stop do
      begin
        Inc(Line, GetExtent(i) + EffectiveLineWidth);
        if N < Line then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;

begin
  if not UseRightToLeftAlignment then
    Result.X := DoCalc(FDrawInfo.Horz, X)
  else
    Result.X := DoCalcRightToLeft(FDrawInfo.Horz, X);
  Result.Y := DoCalc(FDrawInfo.Vert, Y);
end;

function TCustomStringGrid.CanColumnMove(const FromIndex,
  ToIndex: Integer): Boolean;
begin
  Result := (FromIndex >= 0) and (FromIndex < ColCount) and
    (ToIndex >= 0) and (ToIndex < ColCount);
  if Result and not SyncColumns then
    Result := (Columns.UpdateCount > 0) or
              ((FromIndex >= Columns.Count) and (ToIndex >= Columns.Count)) or
                ((FromIndex < Columns.Count) and (ToIndex < Columns.Count));
end;

function TCustomStringGrid.CanEdit: Boolean;
var
  Merged: Boolean;
  Merging: TMerging;
begin
  Merged := IsMerged(Col, Row, Merging);
  Result := not ReadOnly[Col, Row] and (not Merged or
    (Merged and ((Merging.Rect.Left = Col) and (Merging.Rect.Top = Row))) or
    (Merged and not Merging.MergeText));
end;

function TCustomStringGrid.CanEditAcceptKey(Key: Char): Boolean;
const                  { 0    9   BS  TAB CR   ESC  DEL }
  CHARSET_ABSINTEGER = ['0'..'9', #8, #9, #13, #27, #127];
  CHARSET_INTEGER =    ['0'..'9', #8, #9, #13, #27, #127, '-'];
var
  DS: Char;
begin
  DS := DecimalSeparator;
  Result := True;
  if Columns[Col] <> nil then
    with InplaceEditor do
      case Columns[Col].InputStyle of
        isInteger:
          Result := (Key in CHARSET_INTEGER) and
            not ((Key = '-') and (Pos('-', Text) > 0)) and
            not ((Key = '-') and (SelStart <> 0));
        isAbsInteger:
          Result := Key in CHARSET_ABSINTEGER;
        isFloat:
          Result := (Key in (CHARSET_INTEGER + [DS])) and
            not ((Key = '-') and (Pos('-', Text) > 0)) and
            not ((Key = '-') and (SelStart <> 0)) and
            not ((Key = DS) and (Pos(DS, Text) > 0) and (Pos(DS, SelText) > 0));
        isAbsFloat:
          Result := (Key in (CHARSET_ABSINTEGER + [DS])) and
            not ((Key = DS) and (Pos(DS, Text) > 0) and (Pos(DS, SelText) > 0));
        isMask,
        isString:
          Result := inherited CanEditAcceptKey(Key);
      end;
  if not Result then
    MessageBeep(0);
end;

function TCustomStringGrid.CanEditModify: Boolean;
begin
  Result := CanEdit;
end;

function TCustomStringGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow and
    (CanEdit or (goAlwaysShowEditor in Options))
end;

procedure TCustomStringGrid.ChangeEditFormat(const ACol: Integer;
  const OldFormat, NewFormat: String);
var
  iRow: Integer;
begin
  try
    for iRow := 0 to (RowCount - 1) do
      if Cells[ACol, iRow] <> '' then
        if Cells[ACol, iRow] = Format(OldFormat, [Values[ACol, iRow]]) then
          inherited SetCells(ACol, iRow,
            Format(NewFormat, [Values[ACol, iRow]]));
  except
    on EConvertError do
      ;
    else
      raise;
  end;
end;

function TCustomStringGrid.CheckColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := CanColumnMove(Origin, Destination);
end;

procedure TCustomStringGrid.CMFontChanged(var Message: TMessage);
var
  iCol: Integer;
begin
  if SameFont(FixedFont, FFontBackup) then
    FixedFont.Assign(Font);
  for iCol := 0 to (Columns.Count - 1) do
    if not Columns[iCol].Fixed then
      if SameFont(Columns[iCol].Font, FFontBackup) then
        Columns[iCol].Font.Assign(Font);
  FFontBackup.Assign(Font);
  inherited;
end;

procedure TCustomStringGrid.CMParentFontChanged(var Message: TMessage);
begin
{ Do we realy want to change ALL the fonts to the Parent's ???
  Nhaaahhh! See ResetAllFonts }
  inherited;
end;

procedure TCustomStringGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  if (Columns[FromIndex] <> nil) and (Columns[ToIndex] <> nil) then
    Columns[FromIndex].Index := ToIndex;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

procedure TCustomStringGrid.ColWidthsChanged;
begin
  if not FColWidthsUpdating then
  try
    FColWidthsUpdating := True;
    UpdateColWidths;
    inherited ColWidthsChanged;
  finally
    FColWidthsUpdating := False;
  end;
end;

constructor TCustomStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  Options := Options + [goThumbTracking];
  FFixedFont := TFont.Create;
  FFixedFont.Assign(Font);
  FFixedFont.OnChange := FixedFontChanged;
  FFixedFontBackup := TFont.Create;
  FFixedFontBackup.Assign(FFixedFont);
  FFontBackup := TFont.Create;
  FFontBackup.Assign(Font);
  FColumns := TStringGridColumns.Create(Self);
  FRowColors := TStringGridRowColors.Create(Self);
  FDefaultDrawing := [ddBackground, ddEdges, ddGridLines, ddGlyphs, ddText,
    ddFocusRect];
  FFixedGridLineColor := clBlack;
  FFocusRectColor := clDefault;
  FGridLineColor := clSilver;
  FReadOnlyColor := clDefault;
  FSelectionAlphaBlend := True;
  FSelectionAlphaBlendValue := 80;
  FSelectionColor := clHighLight;
end;

function TCustomStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditListEx.Create(Self);
  TInplaceEditListEx(Result).OnExit := InplaceEditorExit;
  TInplaceEditListEx(Result).OnEditButtonClick := FOnEditButtonClick;
end;

procedure TCustomStringGrid.DblClick;
begin
  if FColSizing then
    AutoColWidth(FClickCol);
  FColSizing := False;
  inherited DblClick;
end;

procedure TCustomStringGrid.DeleteColumn(ACol: Integer);
begin
  if FColumns[ACol] <> nil then
    if not FColumns[ACol].Destroying then
      FColumns[ACol].Free;
  inherited DeleteColumn(ACol);
end;

procedure TCustomStringGrid.DeleteRows(const DeletePos: TDeletePos;
  StartIndex: Integer = -1; const Count: Integer = 1);
var
  i: Integer;
  SaveSel: TGridRect;
  SaveTopRow: Integer;
begin
  if not ((DeletePos = dpRange) and (StartIndex = -1)) then
  begin
    StartIndex := Max(StartIndex, FixedRows);
    SaveSel := Selection;
    SaveTopRow := TopRow;
    case DeletePos of
      dpCurrent:
        DeleteRow(Row);
      dpBegin:
        DeleteRow(FixedRows);
      dpEnd:
        DeleteRow(RowCount - 1);
      dpRange:
        for i := (StartIndex + Count - 1) downto StartIndex do
          DeleteRow(i);
      dpSelection:
        for i := Selection.Bottom downto Selection.Top do
          DeleteRow(i);
      dpAll:
        for i := (RowCount - 1) downto (FixedRows + 1) do
          DeleteRow(i);
    end;
    SaveSel.Bottom := Min(SaveSel.Bottom, RowCount - 1);
    Selection := SaveSel;
    TopRow := Min(SaveTopRow, RowCount - 1);
  end;
end;

destructor TCustomStringGrid.Destroy;
begin
  FRowColors.Free;
  FColumns.Free;
  FFontBackup.Free;
  FFixedFontBackup.Free;
  FFixedFont.Free;
  inherited Destroy;
end;

procedure TCustomStringGrid.DoTitleClick(const ACol: Integer);
begin
  if Columns[ACol] <> nil then
    Columns[ACol].DoTitleClick;
  if Assigned(FOnTitleClick) then
    FOnTitleClick(Self, ACol, Columns[ACol]);
end;

procedure TCustomStringGrid.DrawCell(const ACol, ARow: Integer;
  const ARect: TRect; const AState: TGridDrawState;
  const AStage: TDefaultDrawingMode; const StageBegin: Boolean);
begin
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, ACol, ARow, ARect, AState, AStage, StageBegin,
      Columns[ACol]);
end;

function TCustomStringGrid.EndColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := CanColumnMove(Origin, Destination);
end;

procedure TCustomStringGrid.ExportCSV(const FileName: TFileName;
  const TitlesFirstRow: Boolean);
var
  Lines: TStrings;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    if TitlesFirstRow then
    begin
      Lines.Add('');
      for i := 0 to ColCount - 1 do
        if FColumns[i] <> nil then
          Lines[0] := Lines[0] + FColumns[i].Title.Caption + ','
        else
          Lines[0] := Lines[0] + Cells[i, 0] + ',';
      Lines[0] := Copy(Lines[0], 1, Length(Lines[0]) - 1);
    end;
    for i := 1 to RowCount - 1 do
      Lines.Add(Rows[i].CommaText);
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure TCustomStringGrid.FixedFontChanged(Sender: TObject);
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

function TCustomStringGrid.GetCellColor(const ACol, ARow: Integer;
  const AState: TGridDrawState; Column: TStringGridColumn; Cell: PCell): TColor;
var
  DrawRowColor: Boolean;
begin
  if (Cell <> nil) and Cell^.FReadOnly and not (gdFixed in AState) then
    Result := ReadOnlyColor
  else
  begin
    with FRowColors do
      DrawRowColor := ((Odd(ARow) and (FOddRowColor <> clDefault)) or
        (not Odd(ARow) and (FEvenRowColor <> clDefault))) and
        (ARow >= FixedRows) and
        not ((gdFixed in AState) and not IncludeFixed) and
        not ((Column <> nil) and not OverrideColumnColor);
    case DrawRowColor of
      True:
        if Odd(ARow) then
          Result := FRowColors.OddRowColor
        else
          Result := FRowColors.EvenRowColor;
      else
        if Column <> nil then
          Result := Column.Color
        else
          Result := Color;
    end;
    if gdFixed in AState then
      case Column <> nil of
        True:
          if ARow < FixedRows then
            Result := Column.Title.Color;
        else
          if not DrawRowColor then
            Result := FixedColor;
      end;
  end;
end;

function TCustomStringGrid.GetCellDrawFlags(const ARow: Integer;
  const Merged: Boolean; const Merging: TMerging; const AState: TGridDrawState;
  Column: TStringGridColumn): UINT;
const
  FlagML: array[Boolean] of UINT = (DT_SINGLELINE,
    DT_WORDBREAK or DT_EDITCONTROL);
  FlagAlign: array[TAlignment] of UINT = (DT_LEFT, DT_RIGHT, DT_CENTER);
  FlagVAlign: array[TVAlignment] of UINT = (DT_TOP, DT_VCENTER, DT_BOTTOM);
begin
  Result := DT_END_ELLIPSIS or DT_NOPREFIX;
  if Column = nil then
    Result := Result or DT_LEFT
  else
    if (gdFixed in AState) and (ARow < FixedRows) then
      Result := Result or FlagAlign[Column.Title.Alignment] or
        FlagVAlign[Column.Title.VAlignment]
    else
      Result := Result or FlagAlign[Column.Alignment] or
        FlagVAlign[Column.VAlignment];
  if Merged and Merging.MergeText then
    Result := Result or FlagML[Merging.MultiLine]
  else
    if Column = nil then
      Result := Result or FlagML[False]
    else
      if ARow = 0 then
        Result := Result or FlagML[Column.Title.MultiLine]
      else
        Result := Result or FlagML[Column.MultiLine];
end;

procedure TCustomStringGrid.GetCellFont(AFont: TFont; Column: TStringGridColumn;
  const ARow: Integer; const AState: TGridDrawState);
begin
  case Column <> nil of
    True:
      case (gdFixed in AState) and (ARow < FixedRows) of
        True:
          if not SameFont(AFont, Column.Title.Font) then
            AFont.Assign(Column.Title.Font);
        False:
          if not SameFont(AFont, Column.Font) then
            AFont.Assign(Column.Font);
      end;
    False:
      case gdFixed in AState of
        True:
          if not SameFont(AFont, FixedFont) then
            AFont.Assign(FixedFont);
        False:
          if not SameFont(AFont, Font) then
            AFont.Assign(Font);
      end;
  end;
end;

function TCustomStringGrid.GetCellText(const ARow: Integer;
  const Merged: Boolean; const Merging: TMerging;
  Column: TStringGridColumn; Cell: PCell): String;
begin
  if Merged and Merging.MergeText then
    Result := Cells[Merging.Rect.Left, Merging.Rect.Top]
  else if Column <> nil then
  begin
    if ARow = 0 then
      Result := Column.Title.Caption
    else if (Cell <> nil) and (Cell^.FString <> '') then
      Result := Cell^.FString
    else if Column.RowNumbers then
      Result := IntToStr(ARow)
    else
      Result := Column.DefaultCellText;
  end
  else if Cell <> nil then
    Result := Cell^.FString
  else
    Result := '';
end;

function TCustomStringGrid.GetColCount: Integer;
begin
  Result := inherited ColCount;
end;

function TCustomStringGrid.GetEditLimit: Integer;
begin
  if Columns[Col] <> nil then
    Result := Columns[Col].MaxLength
  else
    Result := inherited GetEditLimit;
end;

function TCustomStringGrid.GetEditMask(ACol, ARow: Integer): String;
begin
  if (Columns[ACol] <> nil) and (Columns[ACol].InputStyle = isMask) then
    Result := Columns[ACol].EditMask
  else
    Result := inherited GetEditMask(ACol, ARow);
end;

function TCustomStringGrid.GetEditStyle(ACol, ARow: Integer): Grids.TEditStyle;
begin
  if Columns[ACol] <> nil then
    case Columns[ACol].EditStyle of
      esSimple:
        Result := Grids.esSimple;
      esEllipsis:
        Result := Grids.esEllipsis;
      esPickList:
        Result := Grids.esPickList;
      else
        Result := Grids.esPickList
    end
  else
    Result := Grids.esSimple;
end;

function TCustomStringGrid.GetEditText(ACol, ARow: Integer): String;
begin
  if CanEdit then
    Result := Cells[ACol, ARow]
  else
    Result := '';
  if Assigned(OnGetEditText) then
    OnGetEditText(Self, ACol, ARow, Result);
end;

function TCustomStringGrid.GetFixedCols: Integer;
begin
  Result := inherited FixedCols;
end;

function TCustomStringGrid.GetFixedRows: Integer;
begin
  Result := inherited FixedRows;
end;

function TCustomStringGrid.GetGridLineWidth: Integer;
begin
  Result := inherited GridLineWidth;
end;

function TCustomStringGrid.GetReadOnlyColor: TColor;
begin
  if FReadOnlyColor = clDefault then
    Result := Color
  else
    Result := FReadOnlyColor;
end;

function TCustomStringGrid.GetValues(const ACol, ARow: Integer): Variant;
begin
  if Columns[ACol] <> nil then
    case Columns[ACol].InputStyle of
      isString,
      isMask,
      isCustom:
        Result := Cells[ACol, ARow];
      isInteger,
      isAbsInteger:
        Result := StrToInt(Cells[ACol, ARow]);
      isFloat,
      isAbsFloat:
        Result := StrToFloat(Cells[ACol, ARow]);
    end
  else
    Result := Cells[ACol, ARow];
end;

procedure TCustomStringGrid.ImportCSV(const FileName: TFileName;
  const TitlesFirstRow: Boolean);
{ Rows[*].CommaText treats spaces as comma's, solution:
  ExtractStrings([','], [], PChar(Lines[i]), Rows[i + Offset[TitlesFirstRow]]);
  But we don't use this due to same problems with ExportCSV, which can't
  easily be solved. It seems that we have to use CSV-files with quoted spaced
  text. Maybe Borland's CommaText interpretation is common use, but wikipedia
  tells otherwise. Shame: no standard! }
const
  Offset: array[Boolean] of Integer = (1, 0);
var
  Lines: TStrings;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    for i := 0 to Lines.Count - 1 do
      Rows[i + Offset[TitlesFirstRow]].CommaText := Lines[i];
    if TitlesFirstRow then
      for i := 0 to Columns.Count - 1 do
        Columns[i].Title.Caption := Cells[i, 0];
  finally
    Lines.Free;
  end;
end;

procedure TCustomStringGrid.InplaceEditorExit(Sender: TObject);
begin
  if FAutoRowHeights then
    AutoRowHeight(Row);
end;

procedure TCustomStringGrid.InsertColumn(const AtIndex: Integer);
begin
  if ((AtIndex < 0) or (AtIndex > ColCount)) then
    raise EStringGridError.CreateFmt(SInvalidColumnInsertion, [AtIndex]);
  if AtIndex > Columns.Count then
  begin
    ColCount := ColCount + 1;
    if AtIndex < ColCount - 1 then
      MoveColumn(ColCount - 1, AtIndex);
  end
  else
    Columns.Insert(AtIndex);
  if AtIndex < FixedCols then
    FixedCols := FixedCols + 1;
end;

procedure TCustomStringGrid.InsertColumn(const Position: TInsertPos = ipBefore);
begin
  case Position of
    ipBefore:
      InsertColumn(Col);
    ipAfter:
      InsertColumn(Col + 1);
    ipBegin:
      InsertColumn(FixedCols);
    ipEnd:
      InsertColumn(ColCount);
  end;
end;

procedure TCustomStringGrid.InsertRow(const AtIndex: Integer);
begin
  RowCount := RowCount + 1;
  if AtIndex < RowCount - 1 then
    MoveRow(RowCount - 1, AtIndex);
  if AtIndex < FixedRows then
    FixedRows := FixedRows + 1;
end;

procedure TCustomStringGrid.InsertRow(const Position: TInsertPos = ipBefore);
begin
  case Position of
    ipBefore:
      InsertRow(Row);
    ipAfter:
      InsertRow(Row + 1);
    ipBegin:
      InsertRow(FixedRows);
    ipEnd:
      InsertRow(RowCount);
  end;
end;

function TCustomStringGrid.IsAlternatingRowColorsStored: Boolean;
begin
  Result := FRowColors.IsStored;
end;

function TCustomStringGrid.IsColumnsStored: Boolean;
begin
  Result := FColumns.Count > 0;
end;

function TCustomStringGrid.IsEmptyColumn(const ACol: Integer): Boolean;
var
  iRow: Integer;
begin
  Result := True;
  for iRow := FixedRows to (RowCount - 1) do
    if Trim(Cells[ACol, iRow]) <> '' then
    begin
      Result := False;
      Break;
    end;
end;

function TCustomStringGrid.IsEmptyRow(const ARow: Integer): Boolean;
var
  iCol: Integer;
begin
  Result := True;
  for iCol := FixedCols to (ColCount - 1) do
    if Trim(Cells[iCol, ARow]) <> '' then
    begin
      Result := False;
      Break;
    end;
end;

function TCustomStringGrid.IsFixedFontStored: Boolean;
begin
  Result := not SameFont(Font, FFixedFont);
end;

function TCustomStringGrid.IsReadOnlyColorStored: Boolean;
begin
  Result := FReadOnlyColor <> clDefault;
end;

procedure TCustomStringGrid.Loaded;
var
  i: Integer;
begin
  inherited Loaded;
  for i := 0 to Columns.Count - 1 do
    if Columns[i].Action <> nil then
      Columns[i].ActionChanged(Columns[i].Action, True);
{ Due to several calls to MoveColumn while loading Columns-property: }
  Col := FixedCols;
{ What has TCustomGrid.Paint to do with drawing temporarily XOR-lines when
  moving column or row???? I'm almost sure that TCustomGrid.Paint isn't the
  full or final code which grids.dcu uses. For example: note the UpdateRect
  variable in TCustomGrid.Paint which is never used.
  We once use inherited Paint to wake up XorPainting for column- and
  rowdragging, which strangely seems to work fine: }
  inherited Paint;
end;

function TCustomStringGrid.MouseCoord(const X, Y: Integer): TGridCoord;
begin
  Result := CalcCoordFromPoint(X, Y);
  if Result.X < 0 then
    Result.Y := -1
  else
    if Result.Y < 0 then
      Result.X := -1;
end;

procedure TCustomStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  FClickCol := Coord.X;
{ With thanks to: www.nldelphi.com/Forum/showthread.php?t=20027
  To prevent too much scrolling if last row is half shown: }
  if Coord.Y = FDrawInfo.Vert.LastFullVisibleCell + 1 then
    PostMessage(Handle, WM_CANCELMODE, 0, 0);
  inherited MouseDown(Button, Shift, X, Y);
  if (FGridState = gsColSizing) then
    FColSizing := True;
end;

procedure TCustomStringGrid.MouseToCell(const X, Y: Integer; var ACol,
  ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TCustomStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  SaveState: TGridState;
  Coord: TGridCoord;
begin
  SaveState := FGridState;
  inherited MouseUp(Button, Shift, X, Y);
  if SaveState in [gsNormal, gsSelecting, gsColMoving] then
  begin
    Coord := MouseCoord(X, Y);
    if (Coord.X = FClickCol) and (Button = mbLeft) and (Coord.X >= 0) and
        (Coord.X < ColCount) and (Coord.Y = 0) and (FixedRows > 0) then
      DoTitleClick(Coord.X);
  end;
end;

procedure TCustomStringGrid.MoveColumn(const FromIndex, ToIndex: Integer);
begin
  if not CanColumnMove(FromIndex, ToIndex) then
  begin
    if FromIndex < FColumns.Count then
      raise EStringGridError.CreateFmt(SInvalidColumnMovement,
        [FColumns[FromIndex].DisplayName, ToIndex])
    else
      raise EStringGridError.CreateFmt(SInvalidColumnMovement,
        [IntToStr(FromIndex), ToIndex]);
  end
  else
    if FGridState = gsNormal then
    begin
      FGridState := gsColMoving;
      try
        inherited MoveColumn(FromIndex, ToIndex);
      finally
        FGridState := gsNormal;
      end;
    end;
end;

procedure TCustomStringGrid.MoveRow(const FromIndex, ToIndex: Integer);
begin
  inherited MoveRow(FromIndex, ToIndex);
end;

procedure TCustomStringGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    for i := 0 to Columns.Count - 1 do
      if AComponent = Columns[i].Action then
        Columns[i].Action := nil;
end;

procedure TCustomStringGrid.Paint;
type
  TCacheColors = record
    Source: TColor;
    Dest: TColor;
    Result: TColor;
  end;
var
  UpdateRect: TGridRect;
  EdgeFlags: UINT;
  FixedBGExtend: TPoint;
  CellBGExtend: TPoint;
  IsActiveControl: Boolean;
  CacheColors: TCacheColors;
  Glyph: TBitmap;

  procedure DefaultDrawCell(const ACol, ARow: Integer; const InnerRect: TRect;
    const AState: TGridDrawState; Column: TStringGridColumn; Cell: PCell);
  var
    DrawSelected: Boolean;
    BGRect: TRect;
    OuterRect: TRect;
    Merged: Boolean;
    Merging: TMerging;
    DrawColor: TColor;
    DrawRect: TRect;
    TxtShift: Integer;

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
    { With thanks to: msdn.microsoft.com/en-us/library/dd183393(VS.85).aspx }
    var
      Src: TRGB;
      Dst: TRGB;
      SCAFactor: Single; { SourceConstantAlpha factor }
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
      with Merging.Rect do
        Result := BoxRect(Left, Top, Right, Bottom);
    { BoxRect truncs result to visible cells: fix... }
      for i := Merging.Rect.Top to (TopRow - 1) do
        Result.Top := Result.Top -
          (RowHeights[i] + FDrawInfo.Vert.EffectiveLineWidth);
      for i := Merging.Rect.Left to (LeftCol - 1) do
        Result.Left := Result.Left -
          (ColWidths[i] + FDrawInfo.Horz.EffectiveLineWidth);
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
    end
    else
    begin
      Inc(BGRect.Right, CellBGExtend.X);
      Inc(BGRect.Bottom, CellBGExtend.Y);
    end;
    OuterRect := InnerRect;
    Inc(OuterRect.Right, FDrawInfo.Horz.EffectiveLineWidth);
    Inc(OuterRect.Bottom, FDrawInfo.Vert.EffectiveLineWidth);
    Merged := IsMerged(ACol, ARow, Merging);
    if Merged then
    begin
      if Merging.Rect.Right > ACol then
        BGRect.Right := OuterRect.Right;
      if Merging.Rect.Bottom > ARow then
        BGRect.Bottom := OuterRect.Bottom;
    end;
    with Canvas do
    begin
    { Draw background }
      DrawCell(ACol, ARow, OuterRect, AState, ddBackground, True);
      if ddBackground in DefaultDrawing then
      begin
        Brush.Style := bsSolid;
        DrawColor := GetCellColor(ACol, ARow, AState, Column, Cell);
        if DrawSelected then
          Brush.Color := GetSelectionColor(DrawColor, False)
        else
          Brush.Color := DrawColor;
        FillRect(BGRect);
      end;
      DrawCell(ACol, ARow, OuterRect, AState, ddBackground, False);
    { Draw edges of fixed cells }
      DrawCell(ACol, ARow, OuterRect, AState, ddEdges, True);
      if (ddEdges in DefaultDrawing) and (gdFixed in AState) and
        (EdgeFlags <> 0) then
        DrawEdge(Handle, BGRect, BDR_RAISEDINNER, EdgeFlags);
      DrawCell(ACol, ARow, OuterRect, AState, ddEdges, False);
    { Draw gridlines }
      DrawCell(ACol, ARow, OuterRect, AState, ddGridLines, True);
      if (ddGridLines in DefaultDrawing) and (GridLineWidth > 0) then
      begin
        if gdFixed in AState then
          Brush.Color := FFixedGridLineColor
        else
          Brush.Color := FGridLineColor;
        if OuterRect.Right > BGRect.Right then
        begin
          DrawRect.TopLeft := Point(BGRect.Right, BGRect.Top);
          DrawRect.BottomRight := Point(OuterRect.Right, BGRect.Bottom);
          FillRect(DrawRect);
        end;
        if OuterRect.Bottom > BGRect.Bottom then
        begin
          DrawRect.TopLeft := Point(BGRect.Left, BGRect.Bottom);
          DrawRect.BottomRight := Point(BGRect.Right, OuterRect.Bottom);
          FillRect(DrawRect);
        end;
        if (OuterRect.Right > BGRect.Right) and
          (OuterRect.Bottom > BGRect.Bottom) then
        begin
          DrawRect.TopLeft := BGRect.BottomRight;
          DrawRect.BottomRight := OuterRect.BottomRight;
          FillRect(DrawRect);
        end;
      end;
      DrawCell(ACol, ARow, OuterRect, AState, ddGridLines, False);
    { Draw column title glyph }
      DrawCell(ACol, ARow, OuterRect, AState, ddGlyphs, True);
      if (ddGlyphs in DefaultDrawing) and
        (ARow = 0) and (gdFixed in AState) and (Column <> nil) and
        (Column.Action <> nil) and (Column.Action is TCustomAction) and
        (TCustomAction(Column.Action).ImageIndex >= 0) and
        (TCustomAction(Column.Action).ActionList.Images.GetBitmap(
          TCustomAction(Column.Action).ImageIndex, Glyph)) then
      begin
        DrawRect := BGRect;
        InflateRect(DrawRect, -2, -2);
        Glyph.Width := Min(Glyph.Width, DrawRect.Right - DrawRect.Left);
        Glyph.Height := Min(Glyph.Height, DrawRect.Bottom - DrawRect.Top);
        if Glyph.Height < (DrawRect.Bottom - DrawRect.Top) then
          case Column.Title.VAlignment of
            vaCenter:
              DrawRect.Top := DrawRect.Top +
                (DrawRect.Bottom - DrawRect.Top - Glyph.Height) div 2;
            vaBottom:
                DrawRect.Top := DrawRect.Bottom - Glyph.Height;
          end;
        Canvas.Draw(DrawRect.Left, DrawRect.Top, Glyph);
        TxtShift := Glyph.Width + 2;
      end
      else
        TxtShift := 0;
      DrawCell(ACol, ARow, OuterRect, AState, ddGlyphs, False);
    { Draw text }
      DrawCell(ACol, ARow, OuterRect, AState, ddText, True);
      if ddText in DefaultDrawing then
      begin
        Brush.Style := bsClear;
        GetCellFont(Font, Column, ARow, AState);
        if DrawSelected then
          Font.Color := GetSelectionColor(Font.Color, True);
        if Merged and Merging.MergeText then
        begin
          DrawRect := GetMergeDrawExtends;
          SelectClipRgn(Handle, 0);
          IntersectClipRect(Handle, OuterRect.Left, OuterRect.Top,
            OuterRect.Right, OuterRect.Bottom);
        end
        else
          DrawRect := BGRect;
        InflateRect(DrawRect, -2, -2);
        Inc(DrawRect.Left, TxtShift);
        DrawText(Canvas.Handle,
          PChar(GetCellText(ARow, Merged, Merging, Column, Cell)), -1,
          DrawRect, GetCellDrawFlags(ARow, Merged, Merging, AState, Column));
        SelectClipRgn(Handle, 0);
      end;
      DrawCell(ACol, ARow, OuterRect, AState, ddText, False);
    { Draw focusrect }
      DrawCell(ACol, ARow, OuterRect, AState, ddFocusRect, True);
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
                  GetCellColor(ACol, ARow, AState, Column, Cell));
                FrameRect(BGRect);
              end;
            frSolidCustomColor:
              begin
                Brush.Color := FFocusRectColor;
                FrameRect(BGRect);
              end;
          end;
      DrawCell(ACol, ARow, OuterRect, AState, ddFocusRect, False);
    end;
  end;

  function GetIsActiveControl: Boolean;
  var
    H: Hwnd;
    ParentForm: TCustomForm;
  begin
    Result := False;
    ParentForm := GetParentForm(Self);
    if ParentForm <> nil then
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
    iCol: Integer;
    iRow: Integer;
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
            if CoordInGridRect(iCol, iRow, Selection) then
              Include(DrawState, gdSelected);
            if (iCol = Col) and (iRow = Row) and IsActiveControl then
              Include(DrawState, gdFocused);
            DefaultDrawCell(iCol, iRow, iRect, DrawState, Columns[iCol],
              GetData(iCol, iRow));
          end;
          iRect.Top := iRect.Bottom + FDrawInfo.Vert.EffectiveLineWidth;
        end;
      iRect.Left := iRect.Right + FDrawInfo.Horz.EffectiveLineWidth;
    end;
  end;

begin
  if UseRightToLeftAlignment then
    ChangeGridOrientation(True);
  CalcDrawInfo(FDrawInfo);
  with Canvas.ClipRect do
  begin
    MouseToCell(Left, Top, UpdateRect.Left, UpdateRect.Top);
    MouseToCell(Right, Bottom, UpdateRect.Right, UpdateRect.Bottom);
  end;
  if UpdateRect.Right = -1 then
  begin
    UpdateRect.Right :=
      Min(FDrawInfo.Horz.LastFullVisibleCell + 1, ColCount - 1);
    UpdateRect.Bottom :=
      Min(FDrawInfo.Vert.LastFullVisibleCell + 1, RowCount - 1);
  end;
  FixedBGExtend := Point(0, 0);
  CellBGExtend := Point(0, 0);
  EdgeFlags := 0;
  with FDrawInfo do
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
  if ddGlyphs in DefaultDrawing then
  begin
    Glyph := TBitmap.Create;
    Glyph.Transparent := True;
  end;
  with UpdateRect, FDrawInfo do
  begin
    DrawCells(Max(0, Left), Max(0, Top), Min(FixedCols - 1, Right),
      Min(FixedRows - 1, Bottom), [gdFixed]);
    DrawCells(Max(0, Left), Max(TopRow, Top), Min(FixedCols - 1, Right),
      Min(Vert.LastFullVisibleCell + 1, Bottom), [gdFixed]);
    DrawCells(Max(LeftCol, Left), Max(0, Top),
      Min(Horz.LastFullVisibleCell + 1, Right), Min(FixedRows - 1, Bottom),
      [gdFixed]);
    DrawCells(Max(LeftCol, Left), Max(TopRow, Top),
      Min(Horz.LastFullVisibleCell + 1, Right),
      Min(Vert.LastFullVisibleCell + 1, Bottom), []);
  end;
  if ddGlyphs in DefaultDrawing then
    Glyph.Free;
  if ddBackGround in DefaultDrawing then
    with FDrawInfo, Canvas do
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
  if UseRightToLeftAlignment then
    ChangeGridOrientation(False);
end;

procedure TCustomStringGrid.ResetAllFonts(AFont: TFont = nil);
var
  i: Integer;
begin
  if AFont = nil then
    ParentFont := True
  else
    Font.Assign(AFont);
  FixedFont.Assign(Font);
  for i := 0 to (Columns.Count - 1) do
    with Columns[i] do
    begin
      if not SameFont(Font, Self.Font) then
        Font.Assign(Self.Font);
      if not SameFont(Title.Font, FixedFont) then
        Title.Font.Assign(FixedFont);
    end;
end;

procedure TCustomStringGrid.ResetMainColors(const AGridColor: TColor = clWindow;
  const AFixedColor: TColor = clBtnFace);
var
  i: Integer;
begin
  Color := AGridColor;
  FixedColor := AFixedColor;
  AlternatingRowColors.Reset;
  Columns.BeginUpdate;
  try
    for i := 0 to (Columns.Count - 1) do
    begin
      Columns[i].Color := clDefault;
      Columns[i].Title.Color := clDefault;
    end;
  finally;
    Columns.EndUpdate;
  end;
end;

procedure TCustomStringGrid.Resize;
begin
  KillTimer(Handle, ID_STRETCHTIMER);
  SetTimer(Handle, ID_STRETCHTIMER, 50, nil);
  inherited Resize;
end;

procedure TCustomStringGrid.SetAutoRowHeights(const Value: Boolean);
var
  i: Integer;
begin
  if FAutoRowHeights <> Value then
  begin
    FAutoRowHeights := Value;
    if FAutoRowHeights then
      for i := 0 to RowCount - 1 do
        AutoRowHeight(i);
  end;
end;

procedure TCustomStringGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if HasParent then
    ColWidthsChanged;
end;

procedure TCustomStringGrid.SetCells(const ACol, ARow: Integer;
  const Value: String);
var
  MsgParam: String;
  Column: TStringGridColumn;
  DummyInt: Integer;
  DummyFloat: Single;
  NewValue: String;
begin
  if InGrid(ACol, ARow, True) then
  begin
    MsgParam := '';
    Column := Columns[ACol];
    if (Column = nil) or (Value = '') then
      inherited SetCells(ACol, ARow, Value)
    else
    begin
      NewValue := Value;
      case Column.InputStyle of
        isString,
        isMask:
          ;
        isInteger:
          if not TryStrToInt(Value, DummyInt) then
            MsgParam := SValueInteger;
        isAbsInteger:
          if (not TryStrToInt(Value, DummyInt)) or (DummyInt < 0) then
            MsgParam := SValueAbsInteger;
        isFloat:
          if not TryStrToFloat(Value, DummyFloat) then
            MsgParam := SValueFloat;
        isAbsFloat:
          if (not TryStrToFloat(Value, DummyFloat)) or (DummyFloat < 0) then
            MsgParam := SValueAbsFloat;
        isCustom:
          try
            NewValue := Format(Column.EditFormat, [Value]);
          except
            on EConvertError do
              MsgParam := SValueCustom;
            else
              raise;
          end;
      end;
      if MsgParam <> '' then
        raise EConvertError.CreateFmt(SInvalidFormat, [Column.Title.Caption,
          MsgParam])
      else
        inherited SetCells(ACol, ARow, NewValue);
    end;
  end;
end;

procedure TCustomStringGrid.SetColCount(const Value: Integer);
begin
  if ColCount <> Value then
    if (Value >= Columns.Count) and (not SyncColumns) then
      inherited ColCount := Value;
end;

procedure TCustomStringGrid.SetColumns(const Value: TStringGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TCustomStringGrid.SetDefaultDrawing(
  const Value: TDefaultDrawingModes);
begin
  FDefaultDrawing := Value;
  InvalidateGrid;
end;

procedure TCustomStringGrid.SetEditText(ACol, ARow: Integer;
  const Value: String);
begin
  if CanEdit then
    Cells[ACol, ARow] := Value;
end;

procedure TCustomStringGrid.SetFixedCols(const Value: Integer);
var
  iCol, iRow: Integer;
  UnMergeRect: TGridRect;
begin
  if FixedCols <> Value then
  begin
    for iCol := FixedCols to Min(Value - 1, ColCount -1) do
    begin
      if Columns[iCol] <> nil then
        if SameFont(Columns[iCol].Font, Font) then
          Columns[iCol].Font.Assign(FixedFont);
      for iRow := FixedRows to (RowCount - 1) do
        ReadOnly[iCol, iRow] := False;
    end;
    for iCol := Max(Value, 0) to (FixedCols - 1) do
      if Columns[iCol] <> nil then
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

procedure TCustomStringGrid.SetFixedFont(const Value: TFont);
begin
  FFixedFont.Assign(Value);
end;

procedure TCustomStringGrid.SetFixedGridLineColor(const Value: TColor);
begin
  if FFixedGridLineColor <> Value then
  begin
    FFixedGridLineColor := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomStringGrid.SetFixedRows(const Value: Integer);
var
  iCol: Integer;
  iRow: Integer;
  UnMergeRect: TGridRect;
begin
  if FixedRows <> Value then
  begin
    for iCol := FixedCols to (ColCount - 1) do
      for iRow := FixedRows to Min(Value - 1, RowCount - 1) do
        ReadOnly[iCol, iRow] := False;
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

procedure TCustomStringGrid.SetFocusRectColor(const Value: TColor);
begin
  if FFocusRectColor <> Value then
  begin
    FFocusRectColor := Value;
    if Value <> clDefault then
      FFocusRectStyle := frSolidCustomColor;
    InvalidateCell(Col, Row);
  end;
end;

procedure TCustomStringGrid.SetFocusRectStyle(const Value: TFocusRectStyle);
begin
  if FFocusRectStyle <> Value then
  begin
    FFocusRectStyle := Value;
    if Value in [frDefault, frSolidAutoBW] then
      FFocusRectColor := clDefault;
    InvalidateCell(Col, Row);
  end;
end;

procedure TCustomStringGrid.SetGradientColumnColors(const First, Last: TColor;
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
      Color := MixColor(First, Last, 1);
  finally
    Columns.EndUpdate;
  end;
end;

procedure TCustomStringGrid.SetGridLineColor(const Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomStringGrid.SetGridLineWidth(const Value: Integer);
{ To keep invisible rows and columns invisible }
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

procedure TCustomStringGrid.SetOnEditButtonClick(
  const Value: TNotifyEvent);
begin
  FOnEditButtonClick := Value;
  if InplaceEditor <> nil then
    TInplaceEditListEx(InplaceEditor).OnEditButtonClick := FOnEditButtonClick;
end;

procedure TCustomStringGrid.SetReadOnlyColor(const Value: TColor);
begin
  if FReadOnlyColor <> Value then
  begin
    FReadOnlyColor := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomStringGrid.SetRowColors(const Value: TStringGridRowColors);
begin
  FRowColors.Assign(Value);
end;

procedure TCustomStringGrid.SetSelectionAlphaBlend(const Value: Boolean);
begin
  if FSelectionAlphaBlend <> Value then
  begin
    FSelectionAlphaBlend := Value;
    InvalidateGridRect(Selection);
  end;
end;

procedure TCustomStringGrid.SetSelectionAlphaBlendValue(const Value: Byte);
begin
  if FSelectionAlphaBlendValue <> Value then
  begin
    FSelectionAlphaBlendValue := Max(0, Min(Value, 255));
    InvalidateGridRect(Selection);
  end;
end;

procedure TCustomStringGrid.SetSelectionColor(const Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    if FSelectionColor = clDefault then
      FSelectionColor := clHighLight;
    InvalidateGridRect(Selection);
  end;
end;

procedure TCustomStringGrid.SetStretchModes(Value: TStretchModes);
begin
  if FStretchModes <> Value then
  begin
    if smAllowStretchAll in Value then
      Include(Value, smAllowStretchRight);
    if smAllowShrinkAll in Value then
      Include(Value, smAllowShrinkRight);
    if not (smAllowStretchRight in Value) then
      Exclude(Value, smAllowStretchAll);
    if not (smAllowShrinkRight in Value) then
      Exclude(Value, smAllowShrinkAll);
    FStretchModes := Value;
    ColWidthsChanged;
  end;
end;

procedure TCustomStringGrid.SetSyncColumns(const Value: Boolean);
begin
  if FSyncColumns <> Value then
  begin
    FSyncColumns := Value;
    UpdateColumns;
  end;
end;

procedure TCustomStringGrid.SetValues(const ACol, ARow: Integer;
  const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    Cells[ACol, ARow] := ''
  else
    if Columns[ACol] = nil then
    begin
      if VarIsStr(Value) or VarIsNumeric(Value) or
          (VarType(Value) = varDate) then
        Cells[ACol, ARow] := VarAsType(Value, varString);
    end
    else
      case Columns[ACol].InputStyle of
        isString,
        isMask,
        isCustom:
          if VarIsStr(Value) or VarIsNumeric(Value) or
              (VarType(Value) = varDate) then
            Cells[ACol, ARow] := VarAsType(Value, varString);
        isInteger:
          if VarIsOrdinal(Value) then
            Cells[ACol, ARow] := VarAsType(Value, varInt64);
        isAbsInteger:
          if VarIsOrdinal(Value) then
            if VarAsType(Value, varInt64) >= 0 then
              Cells[ACol, ARow] := VarAsType(Value, varInt64);
        isFloat:
          if VarIsNumeric(Value) then
            Cells[ACol, ARow] := VarAsType(Value, varDouble);
        isAbsFloat:
          if VarIsNumeric(Value) then
            if VarAsType(Value, varDouble) >= 0 then
              Cells[ACol, ARow] := VarAsType(Value, varDouble);
      end;
end;

procedure TCustomStringGrid.SizeChanged(OldColCount, OldRowCount: Integer);
var
  iCol: Integer;
  iRow: Integer;
begin
  if Columns <> nil then
    for iRow := OldRowCount to RowCount - 1 do
      for iCol := 0 to Columns.Count - 1 do
        ReadOnly[iCol, iRow] := Columns[iCol].ReadOnly;
  inherited SizeChanged(OldColCount, OldRowCount);
  if HasParent then
    ColWidthsChanged;
end;

procedure TCustomStringGrid.StretchColumns;
var
  iCol: Integer;
  BigDiff: Integer;
  Diff: Integer;

  procedure AdjustColWidth(const ACol: Integer; var ADiff: Integer);
  begin
    if ADiff <> 0 then
    begin
      if Columns[ACol] <> nil then
      begin
        if (ADiff > 0) and (Columns[ACol].MaxWidth > 0) then
          ADiff := Min(ADiff, Columns[ACol].MaxWidth - ColWidths[ACol])
        else if (ADiff < 0) and (Columns[ACol].MinWidth > 0) then
          ADiff := Max(ADiff, Columns[ACol].MinWidth - ColWidths[ACol]);
      end;
      ColWidths[ACol] := Max(-GridLineWidth, ColWidths[ACol] + ADiff);
    end;
  end;

  function RealGridWidth: Integer;
  var
    i: Integer;
  begin
    Result := GridLineWidth;
    for i := 0 to ColCount - 1 do
      Inc(Result, ColWidths[i] + GridLineWidth);
  end;

begin
  if (FStretchModes <> []) and not FColWidthsUpdating then
    try
      FColWidthsUpdating := True;
      BigDiff := ClientWidth - RealGridWidth;
      if BigDiff > 0 then
      begin
        if smAllowStretchAll in FStretchModes then
          for iCol := 0 to ColCount - 1 do
          begin
            Diff := BigDiff div (ColCount - iCol);
            AdjustColWidth(iCol, Diff);
            Dec(BigDiff, Diff);
            if BigDiff <= 0 then
              Break;
          end
        else if smAllowStretchRight in FStretchModes then
          AdjustColWidth(ColCount - 1, BigDiff);
      end
      else if BigDiff < 0 then
      begin
        if smAllowShrinkAll in FStretchModes then
          for iCol := 0 to ColCount - 1 do
          begin
            Diff := BigDiff div (ColCount - iCol);
            AdjustColWidth(iCol, Diff);
            Dec(BigDiff, Diff);
            if BigDiff >= 0 then
              Break;
          end
        else if smAllowShrinkRight in FStretchModes then
          AdjustColWidth(ColCount - 1, BigDiff);
      end;
    finally
      FColWidthsUpdating := False;
    end;
end;

procedure TCustomStringGrid.UpdateColumn(const Index: Integer);
begin
  InvalidateCol(Index);
end;

procedure TCustomStringGrid.UpdateColumns;
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

procedure TCustomStringGrid.UpdateColWidths;
var
  i: Integer;
begin
  for i := 0 to (Columns.Count - 1) do
    with Columns[i] do
    begin
      if MinWidth <> 0 then
        ColWidths[i] := Max(MinWidth, Width);
      if MaxWidth <> 0 then
        ColWidths[i] := Min(MaxWidth, Width);
    end;
end;

procedure TCustomStringGrid.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_TIMER) and (Message.WParam = ID_STRETCHTIMER) then
  begin
    KillTimer(Handle, ID_STRETCHTIMER);
    StretchColumns;
  end
  else
    inherited WndProc(Message);
end;

procedure TCustomStringGrid.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if ddBackground in DefaultDrawing then
    Message.Result := 1
  else
    inherited;
end;

end.

