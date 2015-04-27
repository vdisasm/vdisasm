{
  Set of ordered items.
}
unit gset;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  grbtree;

type
  TSet<TKey> = class(TEnumerable<TKey>)
  public type
    TCompareLess = TRBTree<TKey>.TCompareLessFunc;
  private const
    SKeyDoesNotExist = 'Key does not exist.';
    SSetIsEmpty      = 'Set is empty.';
  private type
    TItemTree = TRBTree<TKey>;

    TSetEnumerator = class(TEnumerator<TKey>)
    private
      FSet: TSet<TKey>;
      FNode: TItemTree.TRBNodePtr;
    protected
      function DoGetCurrent: TKey; override;
      function DoMoveNext: boolean; override;
    public
      constructor Create(const &Set: TSet<TKey>);
    end;

    TSetReverseEnumerator = class(TEnumerator<TKey>)
    private
      FSet: TSet<TKey>;
      FNode: TItemTree.TRBNodePtr;
    protected
      function DoGetCurrent: TKey; override;
      function DoMoveNext: boolean; override;
    public
      constructor Create(const &Set: TSet<TKey>);
      function GetEnumerator: TSetReverseEnumerator;
    end;

  private
    FKeyComparer: TCompareLess;
    FItems: TItemTree;
    function GetCount: integer; inline;
  protected
    function DoGetEnumerator: TEnumerator<TKey>; override;
  public
    constructor Create(const Comparer: TCompareLess);
    destructor Destroy; override;

    procedure Insert(const Key: TKey); inline;
    procedure Clear; inline;
    function ContainsKey(const Key: TKey): boolean; inline;
    procedure Remove(const Key: TKey);

    function First: TKey; inline;
    function Last: TKey; inline;

    function ReverseEnumerator: TSetReverseEnumerator;

    property Count: integer read GetCount;
  end;

implementation

constructor TSet<TKey>.Create(const Comparer: TCompareLess);
begin
  inherited Create;
  FKeyComparer := Comparer;
  FItems := TItemTree.Create(Comparer);
end;

destructor TSet<TKey>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TSet<TKey>.GetCount: integer;
begin
  Result := FItems.Count;
end;

procedure TSet<TKey>.Clear;
begin
  FItems.Clear;
end;

procedure TSet<TKey>.Insert(const Key: TKey);
begin
  FItems.Add(Key);
end;

procedure TSet<TKey>.Remove(const Key: TKey);
var
  Ptr: TItemTree.TRBNodePtr;
begin
  Ptr := FItems.Find(Key);
  if Assigned(Ptr) then
    FItems.Delete(Ptr);
end;

function TSet<TKey>.ReverseEnumerator: TSetReverseEnumerator;
begin
  Result := TSetReverseEnumerator.Create(self);
end;

function TSet<TKey>.ContainsKey(const Key: TKey): boolean;
begin
  Result := Assigned(FItems.Find(Key));
end;

function TSet<TKey>.DoGetEnumerator: TEnumerator<TKey>;
begin
  Result := TSetEnumerator.Create(self);
end;

function TSet<TKey>.First: TKey;
begin
  if FItems.First = nil then
    raise Exception.Create(SSetIsEmpty);
  Result := FItems.First.K;
end;

function TSet<TKey>.Last: TKey;
begin
  if FItems.Last = nil then
    raise Exception.Create(SSetIsEmpty);
  Result := FItems.Last.K;
end;

{ Enumerator }

constructor TSet<TKey>.TSetEnumerator.Create(const &Set: TSet<TKey>);
begin
  FSet := &Set;
  FNode := nil;
end;

function TSet<TKey>.TSetEnumerator.DoGetCurrent: TKey;
begin
  Result := FNode^.K;
end;

function TSet<TKey>.TSetEnumerator.DoMoveNext: boolean;
begin
  if FNode = nil then
  begin
    // First
    FNode := FSet.FItems.First;
    Exit(FNode <> nil); // it's nil if set is empty
  end;
  Result := FSet.FItems.Next(FNode);
end;

{ TSet<TKey>.TSetReverseEnumerator }

constructor TSet<TKey>.TSetReverseEnumerator.Create(const &Set: TSet<TKey>);
begin
  FSet := &Set;
  FNode := nil;
end;

function TSet<TKey>.TSetReverseEnumerator.DoGetCurrent: TKey;
begin
  Result := FNode^.K;
end;

function TSet<TKey>.TSetReverseEnumerator.DoMoveNext: boolean;
begin
  if FNode = nil then
  begin
    // Last
    FNode := FSet.FItems.Last;
    Exit(FNode <> nil); // it's nil if set is empty
  end;
  Result := FSet.FItems.Prev(FNode);
end;

function TSet<TKey>.TSetReverseEnumerator.GetEnumerator: TSetReverseEnumerator;
begin
  Result := Self;
end;

end.
