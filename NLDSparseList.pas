{ *************************************************************************** }
{                                                                             }
{ NLDSparseList  -  www.nldelphi.com Open Source Delphi runtime library       }
{                                                                             }
{ Initiator: Albert de Weerd (aka NGLN)                                       }
{ License: Free to use, free to modify                                        }
{ Website: None                                                               }
{ SVN path: http://svn.nldelphi.com/nldelphi/opensource/ngln/                 }
{                                                                             }
{ *************************************************************************** }
{                                                                             }
{ Last edit by: Albert de Weerd                                               }
{ Date: January 4, 2010                                                        }
{ Version: 2.0.0.3                                                            }
{                                                                             }
{ *************************************************************************** }

unit NLDSparseList;

{$BOOLEVAL OFF}

interface

uses
  RTLConsts, Classes;

const
  MaxSectionCount = 4096;

type
  TPointerArray = array of Pointer;

  TProcessListItem = function(const Index: Integer; Item: Pointer): Integer;
  TProcessMatrixItem = function(const X, Y: Integer; Item: Pointer): Integer;
  { Must return 0 if successfull }
  { Item is guaranteed non-nil }

  TListQuota = (lqSmall, lqMedium, lqLarge, lqHuge);
  { lqSmall = 65.536 items, 256 KB }
  { lqMedium = 1.048.576 items, 4 MB }
  { lqLarge = 16.777.216 items, 64 MB }
  { lgHuge = 268.435.456 items, 1 GB }

  PSections = ^TSections;
  TSections = array[0..MaxSectionCount - 1] of Pointer;

  TSparseList = class(TObject)
  private
    FSections: PSections;
    FCount: Integer;
    FSectionCount: Word;
    FQuota: TListQuota;
    FAutoGrow: Boolean;
    procedure CheckCapacity(ACapacity: Integer);
    function CreateSection(SectionIndex: Word): Pointer;
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Recount;
    procedure SetCount(ACount: Integer);
    procedure SetQuota(AQuota: TListQuota);
    procedure SetSectionCount(NewSectionCount: Word);
  protected
    property AutoGrow: Boolean read FAutoGrow write FAutoGrow;
    property Quota: TListQuota read FQuota write SetQuota;
  public
    function Add(Item: Pointer): Integer; overload;
    procedure Add(Items: TPointerArray); overload;
    procedure Add(AList: TList); overload;
    procedure Assign(Source: TObject);
    function Capacity: Integer;
    procedure Clear; virtual;
    constructor Create(AAutoGrow: Boolean = True; AQuota: TListQuota = lqSmall);
    procedure Delete(Index: Integer);
    destructor Destroy; override;
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Index: Integer): Pointer; overload;
    function Extract(Item: Pointer): Pointer; overload;
    function First: Pointer;
    function ForAll(ProcessItemFunc: TProcessListItem;
      Descending: Boolean): Integer;
    function Grow: Boolean;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Pack;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

  TSparseMatrix = class(TObject)
  private
    FRows: TSparseList;
    FAutoGrowX: Boolean;
    FQuotaX: TListQuota;
    function GetAutoGrowY: Boolean;
    function GetItem(X, Y: Integer): Pointer;
    function GetQuotaY: TListQuota;
    function GetRow(Y: Integer): TSparseList;
    function GetRowCount: Integer;
    procedure SetAutoGrowX(Value: Boolean);
    procedure SetAutoGrowY(Value: Boolean);
    procedure SetItem(X, Y: Integer; Item: Pointer);
    procedure SetQuotaX(Value: TListQuota);
    procedure SetQuotaY(Value: TListQuota);
    procedure SetRow(Y: Integer; Row: TSparseList);
    procedure SetRowCount(Value: Integer);
  protected
    property AutoGrowX: Boolean read FAutoGrowX write SetAutoGrowX;
    property AutoGrowY: Boolean read GetAutoGrowY write SetAutoGrowY;
    property QuotaX: TListQuota read FQuotaX write SetQuotaX;
    property QuotaY: TListQuota read GetQuotaY write SetQuotaY;
  public
    procedure Clear;
    constructor Create(AutoGrow: Boolean = True; AQuota: TListQuota = lqSmall);
    destructor Destroy; override;
    procedure MoveCol(CurIndex, NewIndex: Integer);
    procedure MoveRow(CurIndex, NewIndex: Integer);
    property Items[X, Y: Integer]: Pointer read GetItem write SetItem; default;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property Rows[Y: Integer]: TSparseList read GetRow write SetRow;
  end;

implementation

{ TSparseList }

const
  IndexMask: array[TListQuota] of Word = (15, 255, 4095, 65535);
  SectionShift: array[TListQuota] of Byte = (4, 8, 12, 16);
  SectionSize: array[TListQuota] of Cardinal = (16, 256, 4096, 65536);

function TSparseList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  Put(FCount, Item);
end;

procedure TSparseList.Add(Items: TPointerArray);
var
  StartFrom: Integer;
  i: Integer;
begin
  StartFrom := FCount;
  for i := Length(Items) - 1 downto 0 do
    if Items[i] <> nil then
      Put(StartFrom + i, Items[i]);
end;

procedure TSparseList.Add(AList: TList);
var
  StartFrom: Integer;
  i: Integer;
begin
  StartFrom := FCount;
  for i := AList.Count - 1 downto 0 do
    if AList[i] <> nil then
      Put(StartFrom + i, AList[i]);
end;

procedure TSparseList.Assign(Source: TObject);

  function PutItem(const Index: Integer; Item: Pointer): Integer;
  begin
    Put(Index, Item);
    Result := 0;
  end;

begin
  if Source is TSparseList then
  begin
    CheckCapacity(TSparseList(Source).Capacity);
    Clear;
    SetSectionCount(TSparseList(Source).FSectionCount);
    FCount := TSparseList(Source).Count;
    TSparseList(Source).ForAll(@PutItem, False);
  end
  else if Source is TList then
  begin
    Clear;
    Add(TList(Source));
  end;
end;

function TSparseList.Capacity: Integer;
begin
  Result := MaxSectionCount * SectionSize[FQuota];
end;

procedure TSparseList.CheckCapacity(ACapacity: Integer);
begin
  while FAutoGrow and (ACapacity > Capacity) and Grow do;
  if ACapacity > Capacity then
    TList.Error(SListCapacityError, ACapacity);
end;

procedure TSparseList.Clear;
begin
  FCount := 0;
  SetSectionCount(0);
end;

constructor TSparseList.Create(AAutoGrow: Boolean = True;
  AQuota: TListQuota = lqSmall);
begin
  inherited Create;
  FAutoGrow := AAutoGrow;
  FQuota := AQuota;
end;

function TSparseList.CreateSection(SectionIndex: Word): Pointer;
var
  Size: Cardinal;
begin
  Size := SectionSize[FQuota] * SizeOf(Pointer);
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

procedure TSparseList.Delete(Index: Integer);
var
  i: Integer;
begin
  if (FCount > 0) and (Index < FCount) then
    if Index = FCount - 1 then
      Put(Index, nil)
    else
    begin
      for i := Index to FCount - 2 do
        Put(i, Get(i + 1));
      Dec(FCount); { To prevent Recount }
      Put(FCount, nil);
    end;
end;

destructor TSparseList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSparseList.Exchange(Index1, Index2: Integer);
var
  Temp: Pointer;
begin
  Temp := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Temp);
end;

function TSparseList.Extract(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Result := nil
  else
  begin
    Result := Get(Index);
    Delete(Index);
  end;
end;

function TSparseList.Extract(Item: Pointer): Pointer;
begin
  Result := Extract(IndexOf(Item));
end;

function TSparseList.First: Pointer;
begin
  Result := Get(0);
end;

function TSparseList.ForAll(ProcessItemFunc: TProcessListItem;
  Descending: Boolean): Integer;
{ Asm code taken from Grids.TSparsePointerArray.ForAll }
var
  CallerBP: Cardinal;
  iSection: Word;
  P: PChar;
  Index: Integer;
  iItem: Word;
  Item: Pointer;
begin
  if FSectionCount = 0 then
    Exit;
  Result := 0;
  asm
    mov   eax,[ebp]
    mov   CallerBP,eax
  end;
  if Descending then
    for iSection := FSectionCount - 1 downto 0 do
    begin
      P := FSections^[iSection];
      if P <> nil then
      begin
        Inc(P, (SectionSize[FQuota] - 1) * SizeOf(Pointer));
        Index := iSection shl SectionShift[FQuota];
        Inc(Index, SectionSize[FQuota] - 1);
        for iItem := SectionSize[FQuota] - 1 downto 0 do
        begin
          Item := PPointer(P)^;
          if Item <> nil then
            asm
              mov   eax,Index
              mov   edx,Item
              push  CallerBP
              call  ProcessItemFunc
              pop   ecx
              mov   @Result,eax
            end;
          if Result <> 0 then
            Break;
          Dec(P, SizeOf(Pointer));
          Dec(Index);
        end;
      end;
      if Result <> 0 then
        Break;
    end
  else
    for iSection := 0 to FSectionCount - 1 do
    begin
      P := FSections^[iSection];
      if P <> nil then
      begin
        Index := iSection shl SectionShift[FQuota];
        for iItem := 0 to SectionSize[FQuota] - 1 do
        begin
          Item := PPointer(P)^;
          if Item <> nil then
            asm
              mov   eax,Index
              mov   edx,Item
              push  CallerBP
              call  ProcessItemFunc
              pop   ecx
              mov   @Result,eax
            end;
          if Result <> 0 then
            Break;
          Inc(P, SizeOf(Pointer));
          Inc(Index)
        end
      end;
      if Result <> 0 then
        Break;
    end;
end;

function TSparseList.Get(Index: Integer): Pointer;
var
  SectionIndex: Word;
  P: PChar;
begin
  if Index < 0 then
    TList.Error(SListIndexError, Index);
  if Index >= Capacity then
    Result := nil
  else
  begin
    SectionIndex := Index shr SectionShift[FQuota];
    if SectionIndex >= FSectionCount then
      P := nil
    else
    begin
      P := FSections^[SectionIndex];
      if P <> nil then
        Inc(P, (Index and IndexMask[FQuota]) * SizeOf(Pointer));
    end;
    if P = nil then
      Result := nil
    else
      Result := PPointer(P)^;
  end;
end;

function TSparseList.Grow: Boolean;
begin
  Result := FQuota < High(TListQuota);
  if Result then
    SetQuota(Succ(FQuota));
end;

function TSparseList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (Get(Result) <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TSparseList.Insert(Index: Integer; Item: Pointer);
var
  i: Integer;
begin
  i := FCount;
  while i > Index do
  begin
    Put(i, Get(i - 1));
    Dec(i);
  end;
  Put(Index, Item);
end;

function TSparseList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TSparseList.Move(CurIndex, NewIndex: Integer);
var
  Temp: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    Temp := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Temp);
  end;
end;

procedure TSparseList.Pack;
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do
    if Get(i) = nil then
      Delete(i);
end;

procedure TSparseList.Put(Index: Integer; Item: Pointer);
var
  SectionIndex: Word;
  P: PChar;
begin
  if Index < 0 then
    TList.Error(SListIndexError, Index);
  if Index >= FCount then
    CheckCapacity(Index + 1);
  if (Item <> nil) or ((Index < FCount) and (Get(Index) <> nil)) then
  begin
    SectionIndex := Index shr SectionShift[FQuota];
    if SectionIndex >= FSectionCount then
      SetSectionCount(SectionIndex + 1);
    P := FSections^[SectionIndex];
    if P = nil then
    begin
      P := CreateSection(SectionIndex);
      FSections^[SectionIndex] := P;
    end;
    Inc(P, (Index and IndexMask[FQuota]) * SizeOf(Pointer));
    PPointer(P)^ := Item;
    if Item = nil then
    begin
      if Index = FCount - 1 then
        Recount;
    end
    else
      if Index > FCount - 1 then
        FCount := Index + 1;
  end;
end;

procedure TSparseList.Recount;

  function ResetCount(Index: Integer; Item: Pointer): Integer;
  begin
    FCount := Index + 1;
    Result := 1;
  end;

begin
  FCount := 0;
  ForAll(@ResetCount, True);
end;

procedure TSparseList.SetCount(ACount: Integer);
var
  i: Integer;
begin
  if ACount < FCount then
  begin
    for i := ACount to FCount - 1 do
      Put(i, nil);
    if FCount = 0 then
      SetSectionCount(0)
    else
      SetSectionCount(((FCount - 1) shr SectionShift[FQuota]) + 1);
  end;
end;

procedure TSparseList.SetQuota(AQuota: TListQuota);
var
  Temp: TSparseList;
begin
  if FQuota <> AQuota then
  begin
    Temp := TSparseList.Create(False, AQuota);
    try
      Temp.Assign(Self);
      Clear;
      FSections := Temp.FSections;
      FSectionCount := Temp.FSectionCount;
      FCount := Temp.FCount;
      FQuota := AQuota;
      Temp.FCount := 0;
      Temp.FSectionCount := 0;
      Temp.FSections := nil;
    finally
      Temp.Free;
    end;
  end;
end;

procedure TSparseList.SetSectionCount(NewSectionCount: Word);
var
  i: Integer;
begin
  if NewSectionCount > FSectionCount then
  begin
    ReallocMem(FSections, NewSectionCount * SizeOf(Pointer));
    FillChar(FSections^[FSectionCount],
      (NewSectionCount - FSectionCount) * SizeOf(Pointer), 0);
    FSectionCount := NewSectionCount;
  end
  else if NewSectionCount < FSectionCount then
  begin
    for i := NewSectionCount to FSectionCount - 1 do
      if FSections^[i] <> nil then
        FreeMem(FSections^[i], SectionSize[FQuota] * SizeOf(Pointer));
    ReallocMem(FSections, NewSectionCount * SizeOf(Pointer));
    FSectionCount := NewSectionCount;
  end;
end;

{ TSparseMatrix }

procedure TSparseMatrix.Clear;
begin
  SetRowCount(0);
end;

constructor TSparseMatrix.Create(AutoGrow: Boolean = True;
  AQuota: TListQuota = lqSmall);
begin
  inherited Create;
  FRows := TSparseList.Create(AutoGrow, AQuota);
  FAutoGrowX := AutoGrow;
  FQuotaX := AQuota;
end;

destructor TSparseMatrix.Destroy;
begin
  Clear;
  FRows.Free;
  inherited Destroy;
end;

function TSparseMatrix.GetAutoGrowY: Boolean;
begin
  Result := FRows.AutoGrow;
end;

function TSparseMatrix.GetItem(X, Y: Integer): Pointer;
var
  Row: TSparseList;
begin
  Row := FRows[Y];
  if Row = nil then
    Result := nil
  else
    Result := Row[X];
end;

function TSparseMatrix.GetQuotaY: TListQuota;
begin
  Result := FRows.Quota;
end;

function TSparseMatrix.GetRow(Y: Integer): TSparseList;
begin
  Result := FRows[Y];
end;

function TSparseMatrix.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

procedure TSparseMatrix.MoveCol(CurIndex, NewIndex: Integer);

  function MoveInRow(Y: Integer; Row: TSparseList): Integer;
  begin
    Row.Move(CurIndex, NewIndex);
    Result := 0;
  end;

begin
  FRows.ForAll(@MoveInRow, False);
end;

procedure TSparseMatrix.MoveRow(CurIndex, NewIndex: Integer);
begin
  FRows.Move(CurIndex, NewIndex);
end;

procedure TSparseMatrix.SetAutoGrowX(Value: Boolean);

  function SetAutoGrow(const Y: Integer; Row: TSparseList): Integer;
  begin
    Row.AutoGrow := Value;
    Result := 0;
  end;

begin
  if FAutoGrowX <> Value then
  begin
    FRows.ForAll(@SetAutoGrow, False);
    FAutoGrowX := Value;
  end;
end;

procedure TSparseMatrix.SetAutoGrowY(Value: Boolean);
begin
  FRows.AutoGrow := Value;
end;

procedure TSparseMatrix.SetItem(X, Y: Integer; Item: Pointer);
var
  Row: TSparseList;
begin
  Row := FRows[Y];
  if Row <> nil then
    Row[X] := Item
  else
    if Item <> nil then
    begin
      FRows[Y] := TSparseList.Create(FAutoGrowX, FQuotaX);
      GetRow(Y)[X] := Item;
    end;
end;

procedure TSparseMatrix.SetQuotaX(Value: TListQuota);

  function SetQuota(const Y: Integer; Row: TSparseList): Integer;
  begin
    Row.Quota := Value;
    Result := 0;
  end;

begin
  if FQuotaX <> Value then
  begin
    FRows.ForAll(@SetQuota, False);
    FQuotaX := Value;
  end;
end;

procedure TSparseMatrix.SetQuotaY(Value: TListQuota);
begin
  FRows.Quota := Value;
end;

procedure TSparseMatrix.SetRow(Y: Integer; Row: TSparseList);
begin
  FRows[Y] := Row;
end;

procedure TSparseMatrix.SetRowCount(Value: Integer);

  function FreeRow(const Y: Integer; Row: TSparseList): Integer;
  begin
    if Y >= Value then
    begin
      Row.Free;
      Result := 0;
    end
    else
      Result := 1;
  end;

begin
  FRows.ForAll(@FreeRow, True);
  FRows.Count := Value;
end;

end.