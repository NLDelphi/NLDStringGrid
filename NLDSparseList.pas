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
{ Date: February 18, 2009                                                     }
{ Version: 2.0.0.0                                                            }
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
  TProcessItem = function(const Index: Integer; Item: Pointer): Integer;
  { Must return 0 if successfull }
  { Item is guaranteed non-nil }

  TListQuota = (lqSmall, lqMedium, lqLarge, lqHuge);
  { lqSmall = 65.536 items, 256 KB }
  { lqMedium = 1.048.576 items, 4 MB }
  { lqLarge = 16.777.216 items, 64 MB }
  { lgHuge = 268.435.456 items, 1 GB }

  PSections = ^TSections;
  TSections = array[0..MaxSectionCount - 1] of Pointer;

  TCustomSparseList = class(TObject)
  private
    FSections: PSections;
    FCount: Integer;
    FSectionCount: Word;
    FQuota: TListQuota;
    FAutoGrow: Boolean;
    procedure CheckCapacity(const ACapacity: Integer);
    function CreateSection(const SectionIndex: Word): Pointer;
    function Get(const Index: Integer): Pointer;
    procedure Put(const Index: Integer; Item: Pointer);
    procedure Recount;
    procedure SetCount(const ACount: Integer);
    procedure SetQuota(const AQuota: TListQuota);
    procedure SetSectionCount(const NewSectionCount: Word);
  protected
    property AutoGrow: Boolean read FAutoGrow write FAutoGrow;
    property Quota: TListQuota read FQuota write SetQuota;
  public
    function Add(Item: Pointer): Integer; overload;
    procedure Add(AList: TList); overload;
    procedure Assign(Source: TObject);
    function Capacity: Integer;
    procedure Clear;
    constructor Create(const AAutoGrow: Boolean = True;
      const AQuota: TListQuota = lqSmall);
    procedure Delete(const Index: Integer);
    destructor Destroy; override;
    procedure Exchange(const Index1, Index2: Integer);
    function Extract(const Index: Integer): Pointer; overload;
    function Extract(Item: Pointer): Pointer; overload;
    function First: Pointer;
    function ForAll(ProcessItemFunc: TProcessItem;
      const Down: Boolean): Integer;
    function Grow: Boolean;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(const Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(const CurIndex, NewIndex: Integer);
    procedure Pack;
    property Count: Integer read FCount write SetCount;
    property Items[const Index: Integer]: Pointer read Get write Put; default;
  end;

  TSparseList = class(TCustomSparseList)
  public
    property AutoGrow;
    property Quota;
  end;

  TCustomSparseMatrix = class(TObject)
  private
    FLists: TCustomSparseList;
    FAutoGrow2: Boolean;
    FQuota2: TListQuota;
    function GetAutoGrow1: Boolean;
    function GetItem(const Index1, Index2: Integer): Pointer;
    function GetList(const Index: Integer): TCustomSparseList;
    function GetQuota1: TListQuota;
    procedure SetAutoGrow1(const Value: Boolean);
    procedure SetAutoGrow2(const Value: Boolean);
    procedure SetItem(const Index1, Index2: Integer; const Item: Pointer);
    procedure SetList(const Index: Integer; const List: TCustomSparseList);
    procedure SetQuota1(const Value: TListQuota);
    procedure SetQuota2(const Value: TListQuota);
  protected
    property AutoGrow1: Boolean read GetAutoGrow1 write SetAutoGrow1;
    property AutoGrow2: Boolean read FAutoGrow2 write SetAutoGrow2;
    property Lists[const Index: Integer]: TCustomSparseList read GetList
      write SetList;
    property Quota1: TListQuota read GetQuota1 write SetQuota1;
    property Quota2: TListQuota read FQuota2 write SetQuota2;
  public
    procedure Clear;
    constructor Create(const AAutoGrow: Boolean = True;
      const AQuota: TListQuota = lqSmall);
    destructor Destroy; override;
    property Items[const Index1, Index2: Integer]: Pointer read GetItem
      write SetItem; default;
  end;

  TSparseMatrix = class(TCustomSparseMatrix)
  public
    property AutoGrow1;
    property AutoGrow2;
    property Quota1;
    property Quota2;
  end;

implementation

{ TCustomSparseList }

const
  IndexMask: array[TListQuota] of Word = (15, 255, 4095, 65535);
  SectionShift: array[TListQuota] of Byte = (4, 8, 12, 16);
  SectionSize: array[TListQuota] of Cardinal = (16, 256, 4096, 65536);

function TCustomSparseList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  Put(FCount, Item);
end;

procedure TCustomSparseList.Add(AList: TList);
var
  StartFrom: Integer;
  i: Integer;
begin
  CheckCapacity(FCount + AList.Count);
  StartFrom := FCount;
  for i := AList.Count - 1 to 0 do
    if AList[i] <> nil then
      Put(StartFrom + i, AList[i]);
end;

procedure TCustomSparseList.Assign(Source: TObject);

  function PutItem(const Index: Integer; Item: Pointer): Integer;
  begin
    Put(Index, Item);
    Result := 0;
  end;

begin
  if Source is TCustomSparseList then
  begin
    CheckCapacity(TCustomSparseList(Source).Capacity);
    Clear;
    SetSectionCount(TCustomSparseList(Source).FSectionCount);
    FCount := TCustomSparseList(Source).Count;
    TCustomSparseList(Source).ForAll(@PutItem, False);
  end
  else if Source is TList then
  begin
    Clear;
    Add(TList(Source));
  end;
end;

function TCustomSparseList.Capacity: Integer;
begin
  Result := MaxSectionCount * SectionSize[FQuota];
end;

procedure TCustomSparseList.CheckCapacity(const ACapacity: Integer);
begin
  while (ACapacity > Capacity) and FAutoGrow and Grow do;
  if ACapacity > Capacity then
    TList.Error(SListCapacityError, ACapacity);
end;

procedure TCustomSparseList.Clear;
begin
  FCount := 0;
  SetSectionCount(0);
end;

constructor TCustomSparseList.Create(const AAutoGrow: Boolean = True;
  const AQuota: TListQuota = lqSmall);
begin
  inherited Create;
  FAutoGrow := AAutoGrow;
  FQuota := AQuota;
end;

function TCustomSparseList.CreateSection(const SectionIndex: Word): Pointer;
var
  Size: Cardinal;
begin
  Size := SectionSize[FQuota] * SizeOf(Pointer);
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

procedure TCustomSparseList.Delete(const Index: Integer);
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

destructor TCustomSparseList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCustomSparseList.Exchange(const Index1, Index2: Integer);
var
  Temp: Pointer;
begin
  Temp := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Temp);
end;

function TCustomSparseList.Extract(const Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Result := nil
  else
  begin
    Result := Get(Index);
    Delete(Index);
  end;
end;

function TCustomSparseList.Extract(Item: Pointer): Pointer;
begin
  Result := Extract(IndexOf(Item));
end;

function TCustomSparseList.First: Pointer;
begin
  Result := Get(0);
end;

function TCustomSparseList.ForAll(ProcessItemFunc: TProcessItem;
  const Down: Boolean): Integer;
{ Asm code taken from Grids.TSparsePointerArray.ForAll }
var
  CallerBP: Cardinal;
  iSection: Word;
  P: PChar;
  Index: Integer;
  iItem: Word;
  Item: Pointer;
begin
  if FCount <= 0 then
    Exit;
  Result := 0;
  asm
    mov   eax,[ebp]
    mov   CallerBP,eax
  end;
  if Down then
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

function TCustomSparseList.Get(const Index: Integer): Pointer;
var
  SectionIndex: Word;
  P: PChar;
begin
  if Index < 0 then
    TList.Error(SListIndexError, Index);
  CheckCapacity(Index + 1);
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

function TCustomSparseList.Grow: Boolean;
begin
  Result := FQuota < High(TListQuota);
  if Result then
    SetQuota(Succ(FQuota));
end;

function TCustomSparseList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (Get(Result) <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCustomSparseList.Insert(const Index: Integer; Item: Pointer);
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

function TCustomSparseList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TCustomSparseList.Move(const CurIndex, NewIndex: Integer);
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

procedure TCustomSparseList.Pack;
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do
    if Get(i) = nil then
      Delete(i);
end;

procedure TCustomSparseList.Put(const Index: Integer; Item: Pointer);
var
  SectionIndex: Word;
  P: PChar;
begin
  if Index < 0 then
    TList.Error(SListIndexError, Index);
  CheckCapacity(Index + 1);
  if (Item <> nil) or (Get(Index) <> nil) then
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

procedure TCustomSparseList.Recount;

  function ResetCount(const Index: Integer; Item: Pointer): Integer;
  begin
    FCount := Index + 1;
    Result := 1;
  end;

begin
  FCount := 0;
  ForAll(@ResetCount, True);
end;

procedure TCustomSparseList.SetCount(const ACount: Integer);
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

procedure TCustomSparseList.SetQuota(const AQuota: TListQuota);
var
  Temp: TCustomSparseList;
begin
  if FQuota <> AQuota then
  begin
    Temp := TCustomSparseList.Create(False, AQuota);
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

procedure TCustomSparseList.SetSectionCount(const NewSectionCount: Word);
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

{ TCustomSparseMatrix }

procedure TCustomSparseMatrix.Clear;

  function FreeList(const Index: Integer; List: TCustomSparseList): Integer;
  begin
    List.Free;
    Result := 0;
  end;

begin
  FLists.ForAll(@FreeList, False);
  FLists.Clear;
end;

constructor TCustomSparseMatrix.Create(const AAutoGrow: Boolean = True;
  const AQuota: TListQuota = lqSmall);
begin
  inherited Create;
  FLists := TCustomSparseList.Create(AAutoGrow, AQuota);
  FAutoGrow2 := AAutoGrow;
  FQuota2 := AQuota;
end;

destructor TCustomSparseMatrix.Destroy;
begin
  Clear;
  FLists.Free;
  inherited Destroy;
end;

function TCustomSparseMatrix.GetAutoGrow1: Boolean;
begin
  Result := FLists.AutoGrow;
end;

function TCustomSparseMatrix.GetItem(const Index1, Index2: Integer): Pointer;
var
  List: TCustomSparseList;
begin
  List := FLists[Index1];
  if List = nil then
    Result := nil
  else
    Result := List[Index2];
end;

function TCustomSparseMatrix.GetList(const Index: Integer): TCustomSparseList;
begin
  Result := FLists[Index];
end;

function TCustomSparseMatrix.GetQuota1: TListQuota;
begin
  Result := FLists.Quota;
end;

procedure TCustomSparseMatrix.SetAutoGrow1(const Value: Boolean);
begin
  FLists.AutoGrow := Value;
end;

procedure TCustomSparseMatrix.SetAutoGrow2(const Value: Boolean);

  function SetAutoGrow(const Index: Integer; List: TCustomSparseList): Integer;
  begin
    List.AutoGrow := Value;
    Result := 0;
  end;

begin
  if FAutoGrow2 <> Value then
  begin
    FLists.ForAll(@SetAutoGrow, False);
    FAutoGrow2 := Value;
  end;
end;

procedure TCustomSparseMatrix.SetItem(const Index1, Index2: Integer;
  const Item: Pointer);
var
  List: TCustomSparseList;
begin
  List := FLists[Index1];
  if List <> nil then
    List[Index2] := Item
  else
    if Item <> nil then
    begin
      FLists[Index1] := TCustomSparseList.Create(FAutoGrow2, FQuota2);
      TCustomSparseList(FLists[Index1])[Index2] := Item;
    end;
end;

procedure TCustomSparseMatrix.SetList(const Index: Integer;
  const List: TCustomSparseList);
begin
  FLists[Index] := List;
end;

procedure TCustomSparseMatrix.SetQuota1(const Value: TListQuota);
begin
  FLists.Quota := Value;
end;

procedure TCustomSparseMatrix.SetQuota2(const Value: TListQuota);

  function SetQuota(const Index: Integer; List: TCustomSparseList): Integer;
  begin
    List.Quota := Value;
    Result := 0;
  end;

begin
  if FQuota2 <> Value then
  begin
    FLists.ForAll(@SetQuota, False);
    FQuota2 := Value;
  end;
end;

end.