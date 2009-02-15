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
{ Date: February 15, 2009                                                     }
{ Version: 1.0.0.0                                                            }
{                                                                             }
{ *************************************************************************** }

unit NLDSparseList;

interface

uses
  RTLConsts, Classes;

const
  MaxSectionCount = 4096;

type
  TNLDSparseListProcessItem = function(const Index: Integer;
    Item: Pointer): Integer;
  { Must return 0 if successfull }
  { Item is guaranteed non-nil }

  TListQuota = (lqSmall, lqMedium, lqLarge);
  { lqSmall = 65.536 items, 256 KB }
  { lqMedium = 1.048.576 items, 4 MB }
  { lgLarge = 268.435.456 items, 1 GB }

  PSections = ^TSections;
  TSections = array[0..MaxSectionCount - 1] of Pointer;

  TNLDSparseList = class(TObject)
  private
    FCount: Integer;
    FQuota: TListQuota;
    FSectionCount: Word;
    FSections: PSections;
    function CreateSection(const SectionIndex: Word): Pointer;
    procedure EnsureSections(const NewSectionCount: Word);
    function Get(const Index: Integer): Pointer;
    procedure Put(const Index: Integer; Item: Pointer);
    procedure Recount;
    procedure SetQuota(const AQuota: TListQuota);
  public
    function Add(Item: Pointer): Integer; overload;
    procedure Add(List: TList); overload;
    procedure Assign(Source: TObject);
    function Capacity: Integer;
    procedure Clear;
    constructor Create(const AQuota: TListQuota);
    procedure Delete(const Index: Integer);
    destructor Destroy; override;
    procedure Exchange(const Index1, Index2: Integer);
    function Extract(const Index: Integer): Pointer; overload;
    function Extract(Item: Pointer): Pointer; overload;
    function First: Pointer;
    function ForAll(ProcessItemFunc: TNLDSparseListProcessItem;
      const Down: Boolean): Integer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(const Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(const CurIndex, NewIndex: Integer);
    property Count: Integer read FCount;
    property Items[const Index: Integer]: Pointer read Get write Put; default;
    property Quota: TListQuota read FQuota write SetQuota;
  end;

implementation

{ TNLDSparseList }

const
  IndexMask: array[TListQuota] of Word = (15, 255, 65535);
  SectionShift: array[TListQuota] of Byte = (4, 8, 16);
  SectionSize: array[TListQuota] of Cardinal = (16, 256, 65536);

function TNLDSparseList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  Put(FCount, Item);
end;

procedure TNLDSparseList.Add(List: TList);
var
  StartFrom: Integer;
  i: Integer;
begin
  if FCount + List.Count > Capacity then
    TList.Error(SListCapacityError, FCount + List.Count);
  StartFrom := FCount;
  for i := 0 to List.Count - 1 do
    Put(StartFrom + i, List[i]);
end;

procedure TNLDSparseList.Assign(Source: TObject);

  function PutItem(const Index: Integer; Item: Pointer): Integer;
  begin
    Put(Index, Item);
    Result := 0;
  end;

var
  i: Integer;
begin
  if Source is TNLDSparseList then
  begin
    Clear;
    EnsureSections(TNLDSparseList(Source).FSectionCount);
    FCount := TNLDSparseList(Source).Count;
    TNLDSparseList(Source).ForAll(@PutItem, False);
  end
  else if Source is TList then
  begin
    Clear;
    for i := TList(Source).Count - 1 downto 0 do
      Put(i, TList(Source)[i]);
  end;
end;

function TNLDSparseList.Capacity: Integer;
begin
  Result := MaxSectionCount * SectionSize[FQuota];
end;

procedure TNLDSparseList.Clear;
var
  i: Integer;
begin
  i := 0;
  while i < FSectionCount do
  begin
    if FSections^[i] <> nil then
      FreeMem(FSections^[i], SectionSize[FQuota] * SizeOf(Pointer));
    Inc(i)
  end;
  if FSections <> nil then
    FreeMem(FSections, FSectionCount * SizeOf(Pointer));
  FCount := 0;
  FSectionCount := 0;
end;

constructor TNLDSparseList.Create(const AQuota: TListQuota);
begin
  inherited Create;
  FQuota := AQuota;
end;

function TNLDSparseList.CreateSection(const SectionIndex: Word): Pointer;
var
  Size: Cardinal;
begin
  Size := SectionSize[FQuota] * SizeOf(Pointer);
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

procedure TNLDSparseList.Delete(const Index: Integer);
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

destructor TNLDSparseList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TNLDSparseList.EnsureSections(const NewSectionCount: Word);
begin
  if NewSectionCount > MaxSectionCount then
    TList.Error(SListCapacityError, NewSectionCount * SectionSize[FQuota]);
  if NewSectionCount > FSectionCount then
  begin
    ReallocMem(FSections, NewSectionCount * SizeOf(Pointer));
    FillChar(FSections^[FSectionCount],
      (NewSectionCount - FSectionCount) * SizeOf(Pointer), 0);
    FSectionCount := NewSectionCount;
  end;
end;

procedure TNLDSparseList.Exchange(const Index1, Index2: Integer);
var
  Temp: Pointer;
begin
  Temp := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Temp);
end;

function TNLDSparseList.Extract(const Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Result := nil
  else
  begin
    Result := Get(Index);
    Delete(Index);
  end;
end;

function TNLDSparseList.Extract(Item: Pointer): Pointer;
begin
  Result := Extract(IndexOf(Item));
end;

function TNLDSparseList.First: Pointer;
begin
  Result := Get(0);
end;

function TNLDSparseList.ForAll(ProcessItemFunc: TNLDSparseListProcessItem;
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

function TNLDSparseList.Get(const Index: Integer): Pointer;
var
  SectionIndex: Word;
  P: PChar;
begin
  if Index < 0 then
    TList.Error(SListIndexError, Index);
  if Index >= Capacity then
    TList.Error(SListCapacityError, Index + 1);
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

function TNLDSparseList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (Get(Result) <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TNLDSparseList.Insert(const Index: Integer; Item: Pointer);
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

function TNLDSparseList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TNLDSparseList.Move(const CurIndex, NewIndex: Integer);
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

procedure TNLDSparseList.Put(const Index: Integer; Item: Pointer);
var
  SectionIndex: Word;
  P: PChar;
begin
  if Index < 0 then
    TList.Error(SListIndexError, Index);
  if Index >= Capacity then
    TList.Error(SListCapacityError, Index + 1);
  if (Item <> nil) or (Get(Index) <> nil) then
  begin
    SectionIndex := Index shr SectionShift[FQuota];
    EnsureSections(SectionIndex + 1);
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
      if Index >= FCount - 1 then
        Recount;
    end
    else
      if Index > FCount - 1 then
        FCount := Index + 1;
  end;
end;

procedure TNLDSparseList.Recount;

  function ResetCount(const Index: Integer; Item: Pointer): Integer;
  begin
    FCount := Index + 1;
    Result := 1;
  end;

begin
  ForAll(@ResetCount, True);
end;

procedure TNLDSparseList.SetQuota(const AQuota: TListQuota);
var
  Temp: TNLDSparseList;
begin
  if FQuota <> AQuota then
  begin
    Temp := TNLDSparseList.Create(AQuota);
    try
      if FCount > Temp.Capacity then
        TList.Error(SListCapacityError, Temp.Capacity);
      Temp.Assign(Self);
      Clear;
      FSections := Temp.FSections;
      FSectionCount := Temp.FSectionCount;
      FCount := Temp.FCount;
      FQuota := AQuota;
      Temp.FSections := nil;
      Temp.FSectionCount := 0;
    finally
      Temp.Free;
    end;
  end;
end;

end.