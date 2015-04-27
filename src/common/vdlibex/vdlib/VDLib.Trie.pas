{
  *
  *   Generic prefix tree (reTRIEval).
  *
  *   Example of TCharTrie, i.e. TTrie<Char>.
  *
  *         B             BOOK and BOMB words
  *          \
  *           O           This way you can store custom objects in trie.
  *          / \
  *         M   O
  *        /     \
  *       B       K
  *      /         \
  *     #           #
  *
}

unit VDLib.Trie;

interface

uses
  System.Character,
  System.Generics.Collections,

  grbtree;

type

  { TTrie<T> }

  TTrie<TKey, TData> = class
  type
    TCompareLessFunc = function(const A, B: TKey): boolean;
    TNode = class;
    PKeyPtr = ^TKey;

    TNodes = class(TRBTree<TNode>)
    protected
      FOwner: TTrie<TKey, TData>;
      function DoCompareLess(const A, B: TNode): boolean; override;
      procedure Notify(const Item: TNode; Action: TCollectionNotification); override;
    public
      constructor Create(Owner: TTrie<TKey, TData>);
    end;

    TTrieKey = TArray<TKey>;
    TTrieKeyList = TList<TTrieKey>;
    TKeyCallbackFunction = function(Node: TNode; Level: integer; ud: pointer): boolean;

    // Need container for HasData.
    TDataContainer = class
      Value: TData;
    end;

    TNode = class
    strict private
      DataContainer: TDataContainer; // any descendant of object
    public
      Key: TKey;        // root has no data
      Parent: TNode;    // parent node
      Children: TNodes; // child nodes
      destructor Destroy; override;

      function IsStopper: boolean; inline;
      function IsFork: boolean; inline;

      // Calculate level of current node;
      function GetLevel: integer;

      // Build current key based on current Level (by going to parent).
      // If Level < 0, it will be recalculated.
      function GetKey(Level: integer = -1): TTrieKey;

      function HasData: boolean; inline;
      procedure SetData(const Data: TData); inline;
      function GetData(out Value: TData): boolean; inline;
    end;

  private
    FCompare: TCompareLessFunc;
    FRoot: TNode;

    // Stats.
    FCount: integer;
    FMinLen, FMaxLen, FTotalLen: integer;

    // Create New Node.
    function CreateNode(Data: PKeyPtr = nil): TNode;

  public
    constructor Create(CompareFunc: TCompareLessFunc);

    destructor Destroy; override;

    { General }

    // Find node according to last element of key.
    // If result is nil, no node found.
    // YOU MUST CHECK IF FOUND NODE IS STOPPER/HAS DATA.
    // Len specify number of elements of Key to process. If Len <= 0, whole key
    // is processed.
    function FindPrefix(const Key: TTrieKey; Len: integer = 0): TNode;

    // Add key/data to the trie.
    // If result is false, key already exists (data is replaced).
    function Put(const Key: TTrieKey; const Data: TData): boolean;

    // Remove key from the trie.
    // procedure Remove(const Key: TTrieKey);

    // Get leftmost key of the trie.
    function GetLeftmostKey: TTrieKey;

    // Get rightmost key of the trie.
    function GetRightmostKey: TTrieKey;

    // Collect list of keys starting from Root node.
    procedure ListKeys(Root: TNode; List: TTrieKeyList);
    procedure ListKeysInternal(Root: TNode; List: TTrieKeyList;
      const Prefix: TTrieKey);

  private
    function TraverseKeysInternal(Root: TNode; Level: integer;
      cb: TKeyCallbackFunction; ud: pointer): boolean;
  public
    procedure TraverseKeys(Root: TNode; cb: TKeyCallbackFunction; ud: pointer);

    { Nodes }

    // Find sub-node by data.
    // Returns nil if no child with the data found.
    function FindSubNode(Node: TNode; const Key: TKey): TNode;

    function GetLeftmostNode: TNode;

    function GetRightmostNode: TNode;

    // Search upward for fork node.
    function FindForkNodeUp(Src: TNode): TNode;

    { Properties }

    // Get root trie node.
    property Root: TNode read FRoot;
    property Count: integer read FCount;
    property MinLen: integer read FMinLen;
    property MaxLen: integer read FMaxLen;
    property TotalLen: integer read FTotalLen;
  end;

  { TByteTrie }

  TByteTrie<TData> = class(TTrie<byte, TData>)
  private
    class function Compare(const A, B: byte): boolean; static; inline;
  public
    constructor Create;
  end;

  { TCharTrie }

  TCharTrie<TData> = class(TTrie<char, TData>)
  private
    class function CompareCS(const A, B: char): boolean; static; inline;
    class function CompareNCS(const A, B: char): boolean; static; inline;
  public
    constructor Create(CaseSensitive: boolean = False);
    function Find(const Key: string): TTrie<char, TData>.TNode;
    function Put(const Key: string; const Data: TData): boolean; inline;
    function GetLeftmostKey: string;
    function GetRightmostKey: string;
  end;

  { String helpers }

function StringToCharArray(const Value: string): TArray<char>; inline;
function CharArrayToString(const Arr: TArray<char>): string; inline;

implementation

{ TTrie<T> }

function TTrie<TKey, TData>.Put(const Key: TTrieKey; const Data: TData): boolean;
var
  Node, ChildNode: TNode;
  CurKey: TKey;
  i, Len: integer;
begin
  Node := FRoot;
  Result := False;
  Len := Length(Key);
  for i := 0 to Len - 1 do
  begin
    CurKey := Key[i];
    ChildNode := FindSubNode(Node, CurKey);

    if ChildNode = nil then
    begin
      ChildNode := CreateNode(@CurKey);
      ChildNode.Parent := Node;
      Node.Children.Add(ChildNode);
      Result := True;
    end;

    Node := ChildNode;
  end;

  // Set or update data.
  Node.SetData(Data);

  // Update stats.
  if Result then
  begin
    inc(FCount);

    if Len > FMaxLen then
      FMaxLen := Len;

    if (FMinLen = 0) or (Len < FMinLen) then
      FMinLen := Len;

    inc(FTotalLen, Len);
  end;
end;

procedure TTrie<TKey, TData>.TraverseKeys(Root: TNode; cb: TKeyCallbackFunction;
  ud: pointer);
begin
  TraverseKeysInternal(Root, 1, cb, ud);
end;

constructor TTrie<TKey, TData>.Create(CompareFunc: TCompareLessFunc);
begin
  inherited Create;
  FCompare := CompareFunc;
  FRoot := CreateNode();
end;

function TTrie<TKey, TData>.CreateNode(Data: PKeyPtr = nil): TNode;
begin
  Result := TNode.Create;
  Result.Children := TNodes.Create(self);
  if Data <> nil then
    Result.Key := Data^;
end;

destructor TTrie<TKey, TData>.Destroy;
begin
  FRoot.Free;
  inherited;
end;

function TTrie<TKey, TData>.FindPrefix(const Key: TTrieKey; Len: integer): TNode;
var
  KeyElement: TKey;
  i: integer;
begin
  if Key = nil then
    Exit(nil);
  Result := FRoot;
  if Len <= 0 then
    Len := Length(Key);
  for i := 0 to Len - 1 do
  begin
    KeyElement := Key[i];
    Result := FindSubNode(Result, KeyElement);
    if Result = nil then
      break;
  end;
end;

function TTrie<TKey, TData>.FindForkNodeUp(Src: TNode): TNode;
begin
  Result := Src;
  if (Result <> nil) and (Result.Children.Count >= 2) then
    Result := Result.Parent;
  while (Result <> nil) and (Result.Children.Count <= 2) do
    Result := Result.Parent;
end;

function TTrie<TKey, TData>.FindSubNode(Node: TNode; const Key: TKey): TNode;
var
  KeyNode: TNode;
  ptr: TNodes.TRBNodePtr;
begin
  KeyNode := TNode.Create; // dummy, w/o children
  try
    KeyNode.Key := Key;
    ptr := Node.Children.Find(KeyNode);
    if ptr = nil then
      Result := nil
    else
      Result := ptr.K;
  finally
    KeyNode.Free;
  end;
end;

function TTrie<TKey, TData>.GetLeftmostKey: TTrieKey;
var
  Node: TNode;
  i: integer;
begin
  i := 0;
  SetLength(Result, i);
  Node := Root;
  while not Node.IsStopper do
  begin
    Node := Node.Children.First.K;
    SetLength(Result, i + 1);
    Result[i] := Node.Key;
    inc(i);
  end;
end;

function TTrie<TKey, TData>.GetRightmostKey: TTrieKey;
var
  Node: TNode;
  i: integer;
begin
  i := 0;
  SetLength(Result, i);
  Node := Root;
  while not Node.IsStopper do
  begin
    Node := Node.Children.Last.K;
    SetLength(Result, i + 1);
    Result[i] := Node.Key;
    inc(i);
  end;
end;

function TTrie<TKey, TData>.GetLeftmostNode: TNode;
begin
  Result := Root;
  while not Result.IsStopper do
    Result := Result.Children.First.K;
end;

function TTrie<TKey, TData>.GetRightmostNode: TNode;
begin
  Result := Root;
  while not Result.IsStopper do
    Result := Result.Children.Last.K;
end;

procedure TTrie<TKey, TData>.ListKeys(Root: TNode; List: TTrieKeyList);
begin
  ListKeysInternal(Root, List, nil);
end;

procedure TTrie<TKey, TData>.ListKeysInternal(Root: TNode; List: TTrieKeyList;
  const Prefix: TTrieKey);
var
  n: TNode;
  Key: TTrieKey;
  i: integer;
begin
  if Root = nil then
    if FRoot = nil then
      Exit
    else
      Root := FRoot;

  for n in Root.Children do
  begin
    if n.HasData then
    begin
      Key := Prefix;
      i := Length(Key);
      SetLength(Key, i + 1);
      Key[i] := n.Key;
      List.Add(Key);
    end;
  end;
end;

function TTrie<TKey, TData>.TraverseKeysInternal(Root: TNode; Level: integer;
  cb: TKeyCallbackFunction; ud: pointer): boolean;
var
  Node: TNode;
begin
  Result := True;

  if Root = nil then
    if FRoot = nil then
      Exit
    else
      Root := FRoot;

  for Node in Root.Children do
  begin
    if Node.HasData then
      Result := cb(Node, Level, ud);
    if not Result then
      break;

    // Recursion.
    if not Node.IsStopper then
      Result := TraverseKeysInternal(Node, Level + 1, cb, ud);
    if not Result then
      break;
  end;
end;

{ TTrie<T>.TTrieNode }

destructor TTrie<TKey, TData>.TNode.Destroy;
begin
  Children.Free;
  DataContainer.Free;
  inherited;
end;

{ Comparers }

class function TByteTrie<TData>.Compare(const A, B: byte): boolean;
begin
  Result := A < B;
end;

class function TCharTrie<TData>.CompareCS(const A, B: char): boolean;
begin
  Result := A < B;
end;

class function TCharTrie<TData>.CompareNCS(const A, B: char): boolean;
begin
  Result := A.ToLower < B.ToLower;
end;

function TTrie<TKey, TData>.TNode.GetData(out Value: TData): boolean;
begin
  if Assigned(DataContainer) then
  begin
    Value := DataContainer.Value;
    Exit(True);
  end;
  Exit(False);
end;

function TTrie<TKey, TData>.TNode.GetKey(Level: integer): TTrieKey;
var
  i: integer;
  Node: TNode;
begin
  if Level < 0 then
    Level := GetLevel;
  Node := self;
  SetLength(Result, Level);
  for i := Level - 1 downto 0 do
  begin
    Result[i] := Node.Key;
    Node := Node.Parent;
  end;
end;

function TTrie<TKey, TData>.TNode.GetLevel: integer;
var
  Node: TNode;
begin
  Result := 0;
  Node := self;
  while Node.Parent <> nil do
  begin
    inc(Result);
    Node := Node.Parent;
  end;
end;

function TTrie<TKey, TData>.TNode.HasData: boolean;
begin
  Result := DataContainer <> nil;
end;

function TTrie<TKey, TData>.TNode.IsFork: boolean;
begin
  if self <> nil then
    if self.Children.Count >= 2 then
      Exit(True);
  Exit(False);
end;

function TTrie<TKey, TData>.TNode.IsStopper: boolean;
begin
  if self <> nil then
    if self.Children.Count <> 0 then
      Exit(False);
  Exit(True);
end;

procedure TTrie<TKey, TData>.TNode.SetData(const Data: TData);
begin
  if DataContainer = nil then
    DataContainer := TDataContainer.Create;
  DataContainer.Value := Data;
end;

{ TByteTrie }

constructor TByteTrie<TData>.Create;
begin
  inherited Create(Compare);
end;

function StringToCharArray(const Value: string): TArray<char>; inline;
var
  Len: integer;
begin
  Len := Length(Value);
  SetLength(Result, Len);
  Move(PChar(Value)^, Result[0], SizeOf(char) * Len);
end;

function CharArrayToString(const Arr: TArray<char>): string; inline;
var
  Len: integer;
begin
  Len := Length(Arr);
  SetLength(Result, Len);
  Move(Arr[0], PChar(Result)^, Len * SizeOf(char));
end;

{ TCharTrie }

function TCharTrie<TData>.Put(const Key: string; const Data: TData): boolean;
begin
  Result := inherited Put(StringToCharArray(Key), Data);
end;

constructor TCharTrie<TData>.Create;
begin
  if CaseSensitive then
    inherited Create(CompareCS)
  else
    inherited Create(CompareNCS);
end;

function TCharTrie<TData>.Find(const Key: string): TTrie<char, TData>.TNode;
begin
  Result := inherited FindPrefix(StringToCharArray(Key));
end;

function TCharTrie<TData>.GetLeftmostKey: string;
begin
  Result := CharArrayToString(inherited GetLeftmostKey);
end;

function TCharTrie<TData>.GetRightmostKey: string;
begin
  Result := CharArrayToString(inherited GetRightmostKey);
end;

{ TTrie<T>.TTrieNodes }

constructor TTrie<TKey, TData>.TNodes.Create(Owner: TTrie<TKey, TData>);
begin
  inherited Create(nil);
  self.FOwner := Owner;
end;

function TTrie<TKey, TData>.TNodes.DoCompareLess(const A, B: TNode): boolean;
begin
  Result := self.FOwner.FCompare(A.Key, B.Key);
end;

procedure TTrie<TKey, TData>.TNodes.Notify(const Item: TNode;
  Action: TCollectionNotification);
begin
  inherited;
  if Action = cnRemoved then
    Item.Free;
end;

end.
