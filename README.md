# NLDStringGrid
A StringGrid with extra features like columns, merged cells, readonly cells, input styles, custom inplace editors, import/export functionality and much more.

Website: https://www.nldelphi.com/forumdisplay.php?43-NLDStringGrid

Description, compared to Delphi's default VCL TStringGrid:

Added functionality:
--------------------
- Without settings, the component appears and acts exactly the same as the default TStringGrid
- Color properties can be set to the default color by setting to clDefault
- Merging of adjacent cells
- Inplace editor in style of current (merged) cell(s) which listens to F4
- Descends from TCustomDrawGrid
- Double clicking a column title autowidths the column
- CSV import/export
- Multiline text in (merged) cells

New methods:
-------------
- procedure AutoColWidth(ACol: Integer);
- procedure AutoRowHeight(ARow: Integer);
- procedure BeginUpdateData;
- procedure DeleteColumn(ACol: Integer);
- procedure DeleteRows(DeletePos: TDeletePos; StartIndex: Integer = -1; Count: Integer = 1);
- procedure EndUpdateData;
- procedure ExportCSV(const FileName: TFileName; TitlesFirstRow: Boolean);
- procedure ImportCSV(const FileName: TFileName; TitlesFirstRow: Boolean);
- procedure InsertColumn(AtIndex: Integer); overload;
- procedure InsertColumn(Position: TInsertPos = ipBefore); overload;
- procedure InsertRow(AtIndex: Integer); overload;
- procedure InsertRow(Position: TInsertPos = ipBefore); overload;
- function IsEmptyColumn(ACol: Integer): Boolean;
- function IsEmptyRow(ARow: Integer): Boolean;
- procedure MergeCells(const AGridRect: TGridRect; MergeText, MultiLine: Boolean);
- procedure MoveColumn(FromIndex, ToIndex: Integer);
- procedure MoveRow(FromIndex, ToIndex: Integer);
- procedure ResetAllFonts(AFont: TFont = nil);
- procedure ResetMainColors(AGridColor: TColor = clWindow; AFixedColor: TColor = clBtnFace);
- procedure SetGradientColumnColors(First, Last: TColor; ColumnsOnly: Boolean);
- procedure UnMergeCells(const AGridRect: TGridRect);

New properties:
---------------
- AlternatingRowColors: TStringGridRowColors (EvenRowColor: TColor, IncludeFixed: Boolean, OddRowColor: TColor, OverrideColumnColor: Boolean)
- AutoRowHeights: Boolean default False
- Columns: TStringGridColumns (Items[Index: Integer]: TStringGridColumn default; OnChanged: TColumnsChangedEvent)
- DefaultDrawing: TDefaultDrawingModes default [ddBackground, ddEdges, ddGridLines, ddGlyphs, ddText, ddFocusRect]
- FixedFont: TFont
- FixedGridLineColor: TColor default clBlack
- FocusRectColor: TColor default clDefault
- FocusRectStyle: TFocusRectStyle (frDefault, frSolidAutoBW, frSolidCustomColor) default frDefault
- GridLineColor: TColor default clSilver
- MemoryOptions: TMemoryOptions (moBeyondGrid, moProportional, moStoreColsRows, moSparseStorage) default DefMemoryOptions
- OnDrawCell: TDrawCellEvent (Sender: TObject; ACol, ARow: Integer; const ARect: TRect; State: TGridDrawState; Stage: TDefaultDrawingMode; StageBegin: Boolean; Column: TStringGridColumn)
- OnEditButtonClick: TNotifyEvent
- OnTitleClick: TTitleClickEvent (Sender: TObject; Index: Integer; Column: TStringGridColumn)
- OwnsObjects: Boolean default False
- ReadOnly[ACol, ARow: Integer]: Boolean
- ReadOnlyColor: TColor
- SelectionAlphaBlend: Boolean default True
- SelectionAlphaBlendValue: Byte default 80
- SelectionColor: TColor default clHighLight
- StretchModes: TStretchModes (smAllowStretchRight, smAllowStretchAll, smAllowShrinkRight, smAllowShrinkAll) default []
- SyncColumns: Boolean (when true: ColCount = Columns.Count; when false: ColCount >= Columns.Count) default False
- Values[ACol, ARow: Integer]: Variant

TStringGridColumn properties:
-----------------------------
- Action: TBasicAction
- Alignment: TAlignment default taLeftJustify
- Color: TColor
- DefaultCellText: String
- EditFormat: String
- EditMask: TEditMask
- EditStyle: TEditStyle (esSimple, esEllipsis, esPickList, esPickListOnly) default esSimple
- Fixed: Boolean
- Font: TFont
- InputStyle: TInputStyle (isString, isInteger, isAbsInteger, isFloat, isAbsFloat, isMask, isCustom) default isString
- MaxLength: Integer default 0
- MaxWidth: Integer default 0
- MinWidth: Integer default 0
- MultiLine: Boolean default False
- OnTitleClick: TNotifyEvent
- PickListItems: TStrings
- ReadOnly: Boolean
- RowNumbers: Boolean default False
- Title: TStringGridTitle (Alignment: TAlignment; Caption: String; Color: TColor; Font: TFont; Height: Integer; MultiLine: Boolean; VAlignment: TVAlignment)
- VAlignment: TVAlignment default vaTop
- Visible: Boolean
- Width: Integer
