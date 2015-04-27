{
  *   XE2+ only.
  *   Unit to work with VA regions.
  *   It should make easy add, subtract, join regions.
  *
  *   Sub is not completely working.
  *   It must be more sophisticated.
}

unit VDLib.VARegions;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  grbtree;

type
  TVAddr = type UInt64; // Virtual address.
  TVSize = type UInt64; // Virtual size.

  TRegionActionFlag = (
    // *   |aaaa|    |bbbb|
    // * +      |cccc|
    // *   |cccccccccccccc|
    raf_join_direct_neighbours,

    // *   |aaaaaa|        |aaaaaa|  |aaaaaaaa|  |aaaaaa|     |aa|     |aaa|
    // * +    |bbbbbb|  |bbbbbb|        |bb|     |bbbbbb|  |bbbbbbbb|  |bbbbbbbb|
    // *   |aa|bbbbbb|  |bbbbbb|aa|  |aa|bb|aa|  |bbbbbb|  |bbbbbbbb|  |bbbbbbbb|
    raf_combine_last
    );

  TRegionActionFlags = set of TRegionActionFlag;

  TVRegions<T> = class
  private const
    SVRegionSizeCannotBeNull      = 'VRegion size cannot be 0';
    STryingToAddIntoOccupiedSpace = 'Trying to add into occupied space';
  public type
    TVRegion = record
    strict private
      function GetLastVA: TVAddr; inline;
      function GetEndVA: TVAddr; inline;
    public
      VA: TVAddr;
      Size: TVSize;
      Data: T;

      constructor Create(VA: TVAddr; Size: TVSize; const Data: T); overload;
      constructor Create(VA: TVAddr; Size: TVSize); overload;
      constructor CreateMinimal(VA: TVAddr);

      function Contains(VA: TVAddr): boolean; inline;

      function Print: string; inline;

      class function Less(const a, b: TVRegion): boolean; inline; static;
      class procedure Swap(var a, b: TVRegion); inline; static;
      class procedure Order(var a, b: TVRegion; Ascending: boolean = True); inline; static;

      { Operators }

      class operator Equal(const a, b: TVRegion): boolean; inline;
      class operator Implicit(const a: string): TVRegion; inline;

      { Properties }

      property LastVA: TVAddr read GetLastVA;
      property EndVA: TVAddr read GetEndVA;
    end;

    TVRegionRBTree = TRBTree<TVRegion>;
  private
    FItems: TVRegionRBTree;
    FOnNotify: TCollectionNotifyEvent<T>;
    procedure DoNotification(const Item: T; Action: TCollectionNotification);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    // Add region. Before adding it will check if there are gaps between prev
    // and next values. If there is no gaps regions are joined.
    procedure AddJoin(const Value: TVRegion); reintroduce;

    // Add region without join. It must not intersect adjacent regions.
    procedure AddNotJoin(const Value: TVRegion);

    // Find intersected region if any.
    function Intersects(const Value: TVRegion): TVRegionRBTree.TRBNodePtr;

    // Subtract region.
    procedure Sub(const Value: TVRegion);

    // Split range at VA into two parts (if such region exists).
    // VA will point to new range.
    // Result is True if split was performed.
    // Left part data is untouched, right part has null data (Default(T))
    function Split(VA: TVAddr; out splitLeft, splitRight: TVRegionRBTree.TRBNodePtr): boolean; overload;

    function Find(VA: TVAddr): TVRegionRBTree.TRBNodePtr;
    function FindRegion(VA: TVAddr; out Region: TVRegion): boolean;

    function Print: string;

    // todo: create separate enumerator instead of Items property
    // which shouldn't be accessed directly.
    property Items: TVRegionRBTree read FItems;

    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
  end;

implementation

uses
  System.SysUtils;

{ TVRegion }

function TVRegions<T>.TVRegion.Contains(VA: TVAddr): boolean;
begin
  Result := (VA >= Self.VA) and (VA < Self.EndVA);
end;

constructor TVRegions<T>.TVRegion.Create(VA: TVAddr; Size: TVSize; const Data: T);
begin
  if Size = 0 then
    raise Exception.Create(SVRegionSizeCannotBeNull);
  Self.VA := VA;
  Self.Size := Size;
  Self.Data := Data;
end;

constructor TVRegions<T>.TVRegion.Create(VA: TVAddr; Size: TVSize);
begin
  Create(VA, Size, Default(T));
end;

constructor TVRegions<T>.TVRegion.CreateMinimal(VA: TVAddr);
begin
  Self.VA := VA;
  Self.Size := 1;
  Self.Data := Default (T);
end;

class function TVRegions<T>.TVRegion.Less(const a, b: TVRegion): boolean;
begin
  Result := (a.LastVA < b.VA);
end;

class operator TVRegions<T>.TVRegion.Equal(const a, b: TVRegion): boolean;
begin
  Result := (a.VA = b.VA) and (a.Size = b.Size);
end;

function TVRegions<T>.TVRegion.GetEndVA: TVAddr;
begin
  Result := Self.VA + Self.Size;
end;

function TVRegions<T>.TVRegion.GetLastVA: TVAddr;
begin
  Result := Self.VA + Self.Size - 1;
end;

class operator TVRegions<T>.TVRegion.Implicit(const a: string): TVRegion;
var
  arr: TArray<string>;
begin
  arr := a.Split([',']);
  if length(arr) <> 2 then
    raise Exception.Create('Wrong input.');
  Result.VA := StrToInt64(arr[0]);
  Result.Size := StrToInt64(arr[1]);
  Result.Data := Default (T);
end;

class procedure TVRegions<T>.TVRegion.Order(var a, b: TVRegion; Ascending: boolean);
begin
  if (Ascending and TVRegion.Less(b, a)) or
    ((not Ascending) and TVRegion.Less(a, b)) then
    Swap(a, b);
end;

function TVRegions<T>.TVRegion.Print: string;
begin
  Result := Format('%x-%x', [Self.VA, Self.VA + Self.Size]);
end;

class procedure TVRegions<T>.TVRegion.Swap(var a, b: TVRegion);
var
  tmp: TVRegion;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

{ TVRegions }

constructor TVRegions<T>.Create;
begin
  inherited;
  FItems := TVRegionRBTree.Create(TVRegion.Less);
end;

destructor TVRegions<T>.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure TVRegions<T>.Clear;
var
  Item: TVRegion;
begin
  if Assigned(FOnNotify) then
  begin
    for Item in FItems do
      FOnNotify(Self, Item.Data, cnRemoved);
  end;
  FItems.Clear;
end;

procedure TVRegions<T>.DoNotification(const Item: T;
  Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

function TVRegions<T>.Find(VA: TVAddr): TVRegionRBTree.TRBNodePtr;
var
  Rgn: TVRegion;
begin
  Rgn := TVRegion.CreateMinimal(VA);
  Result := FItems.Find(Rgn);
end;

function TVRegions<T>.FindRegion(VA: TVAddr; out Region: TVRegion): boolean;
var
  Rgn: TVRegion;
  ptr: TVRegionRBTree.TRBNodePtr;
begin
  Rgn := TVRegion.CreateMinimal(VA);
  ptr := FItems.Find(Rgn);
  if not Assigned(ptr) then
  begin
    Region := Default (TVRegion);
    exit(False);
  end;
  Region := ptr^.K;
  exit(True);
end;

function TVRegions<T>.Print: string;
var
  Rgn: TVRegion;
begin
  Result := '';
  for Rgn in FItems do
    Result := Result + Rgn.Print + ';';
end;

procedure TVRegions<T>.AddJoin(const Value: TVRegion);
var
  prev, cur, next: TVRegionRBTree.TRBNodePtr;
  tmp: TVRegion;
begin
  tmp := Value;

  FItems.FindEx(tmp, prev, cur, next);
  if cur <> nil then
    raise Exception.Create(STryingToAddIntoOccupiedSpace);

  // Try join left.
  if (prev <> nil) and (prev.K.EndVA = tmp.VA) then
  begin
    tmp.VA := prev.K.VA;
    inc(tmp.Size, prev.K.Size);
    FItems.Delete(prev);
  end;

  // Try join right.
  if (next <> nil) and (next.K.VA = tmp.EndVA) then
  begin
    inc(tmp.Size, next.K.Size);
    FItems.Delete(next);
  end;

  // Add joined (or not) result.
  cur := FItems.Add(tmp);

  DoNotification(Value.Data, cnAdded);
end;

procedure TVRegions<T>.AddNotJoin(const Value: TVRegion);
begin
  if Intersects(Value) <> nil then
    raise Exception.Create(STryingToAddIntoOccupiedSpace);

  FItems.Add(Value);
  DoNotification(Value.Data, cnAdded);
end;

function TVRegions<T>.Intersects(const Value: TVRegion): TVRegionRBTree.TRBNodePtr;
var
  prev, cur, next: TVRegionRBTree.TRBNodePtr;
begin
  FItems.FindEx(Value, prev, cur, next);
  Result := cur;
end;

function TVRegions<T>.Split(VA: TVAddr; out splitLeft, splitRight: TVRegionRBTree.TRBNodePtr): boolean;
var
  tmpRgn: TVRegion;
  cur: TVRegionRBTree.TRBNodePtr;
  left, right: TVRegion;
begin
  tmpRgn := TVRegion.CreateMinimal(VA);
  cur := FItems.Find(tmpRgn);
  if cur = nil then
    exit(False); // No such VA.

  if (VA = cur.K.VA) or (VA = cur.K.LastVA) then
    exit(False); // Nothing to split.

  left.VA := cur.K.VA;
  left.Size := VA - cur.K.VA;
  left.Data := cur.K.Data;

  right.VA := VA;
  right.Size := cur.K.EndVA - VA;
  right.Data := Default (T);

  FItems.Delete(cur);
  splitLeft := FItems.Add(left);
  splitRight := FItems.Add(right);

  // Do not need notification as for user it must be transparent.

  exit(True);
end;

procedure TVRegions<T>.Sub(const Value: TVRegion);
var
  prev, cur, next: TVRegionRBTree.TRBNodePtr;
  splitLeft, splitRight: TVRegionRBTree.TRBNodePtr;
begin
  FItems.FindEx(Value, prev, cur, next);
  if cur = nil then
    exit; // Does not exist.

  if cur.K.Contains(Value.VA) and cur.K.Contains(Value.LastVA) then
  begin
    Split(Value.VA, splitLeft, splitRight);
    Split(Value.EndVA, splitLeft, splitRight);
    cur := FItems.Find(TVRegion.CreateMinimal(Value.VA));
    FItems.Delete(cur);
  end;
end;

end.
