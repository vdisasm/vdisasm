{
  *   B+ Tree Database
  *   Disk-based indexed Key-Value storage
  *
  *   vdisasm.com
  *   code.google.com/p/delphi-bpus-tree/
  *
  *   B+ Tree links:
  *     http://en.wikipedia.org/wiki/B%2B_tree
  *     http://www.cecs.csulb.edu/~monge/classes/share/B+TreeIndexes.html
}

unit BPlusTree;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.IOUtils,

  grbtree,
  PagedStorage,
  BPlusTree.Basic,
  BPlusTree.Intf;

{$REGION 'Base'}


const
  MINIMAL_PAGE_SIZE   = 512;
  BAD_PAGE_NUMBER     = 0;
  HEADER_PAGE_NUMBER  = BAD_PAGE_NUMBER;
  SHORTEST_KEY_LENGTH = 1;

  { Page flags }

  PAGEFLAG_VALID = 1 shl 0; // if 0 page probably corrupted
  PAGEFLAG_LEAF  = 1 shl 1; // leaf or branch (if absent)

type
  TKeyCount = type uint16;    //
  TPageFlags = type uint16;   //
  PPageFlags = ^TPageFlags;   //
  TPageNumber = type uint32;  //
  PPageNumber = ^TPageNumber; //
  TKeyLength = type uint16;   // Size of key.

  // Must reside in interface part to be inlined.
  TValueHelper<T> = class
    class procedure Swap(var A, B: T); inline; static;
  end;

  // For debugging purposes.
  TKeyToAnsiString = reference to function(const A: TBytes): AnsiString;

{$ENDREGION}
{$REGION 'TNodeItem & TNodeItemTree intf.'}

  // Parent of Internal and Leaf node items
  TNodeItem = class
  public
    Key: TBytes;

    // Leaf node: # of data (value) page.
    // Internal page: # of page with keys < than the Key.
    ChildPageNo: TPageNumber;

    constructor Create(const aKey: TBytes; aChildPageNo: TPageNumber);

    // For search.
    constructor CreateDummy(const aKey: TBytes);

    class function CompareKeyLess(const A, B: TNodeItem): boolean; static; inline;
  end;

  // Tree of node items
  TNodeItemTree = class(TRBTree<TNodeItem>)
  protected
    procedure Notify(const Item: TNodeItem; Action: TCollectionNotification); override;
  end;

{$ENDREGION}
{$REGION 'TBPlusTree intf.'}


type

  TBPlusTreeFileStorage = class(TFileStorage)
  protected
    // Flush Page method used.

    // Overriding to free B+Tree pages bound to Storage pages.
    procedure PageCollectionNotify(Sender: TObject; const Item: TStoragePage;
      Action: TCollectionNotification); override;
  public
  end;

  //
  // Forwards.
  //
  TBPlusTree = class;
  TBTreePage = class;
  TInternalBTreePage = class;
  TLeafBTreePage = class;

  TItemDesc = record
    LeafPage: TBTreePage;
    Item: TNodeItemTree.TRBNodePtr;
    function IsValid: boolean; inline;
  end;

  //
  // TBPlusTreeCursor
  //
  // Cursor can be used to read-only operations, i.e:
  // - Go Next/Prev
  // - Read keys
  // - Read values.
  TBPlusTreeCursor = class(TInterfacedObject, IBPlusTreeCursor)
  private
    FTree: TBPlusTree;    // Owner tree.
    FItemDesc: TItemDesc; // Item ptr.
    FRangePrefix: TBytes; // Nil if non-range cursor.
  protected
    function GetKey: TBytes; inline;
    function GetValue: TBytes; inline;
    function IsFirstAtPage: boolean; inline;
    function IsLastAtPage: boolean; inline;
  public
    // Constructed by TBPlusTree.
    destructor Destroy; override;

    // Go to next/prev key (respecting prefix). Result is True on success.
    function Next: boolean; inline;
    function Prev: boolean; inline;

    function NextPage: boolean;
    function PrevPage: boolean;

    property Key: TBytes read GetKey;
    property Value: TBytes read GetValue;
  end;

  TBPlusTreeInfo = packed record

    // Identification.
    Sig: array [0 .. 3] of AnsiChar; // VDDB
    VerMajor: byte;                  // Major version: breaking changes.
    VerMinor: byte;                  // Minor version: compatible.

    // Infos.
    MaxKeySize: integer;
    PageSize: TPageSize;
    RootPageNo: TPageNumber; // if = 0, it's wrong
    NumberOfKeys: uint32;

    // Used during space allocation for new value.
    LastValuePageNo: TPageNumber;
    LastValueOffset: TPageSize;

    TreeHeight: integer; // Above the leaf nodes.

    KeyCount: uint32;

    TrashSize: uint64; // size of bytes wasted (during deletion/overwrite).

    // For future fields.
    Reserved: array [0 .. 12] of uint32;
  end;

  //
  // TBPlusTree
  //
  TBPlusTree = class(TBPlusTreeBasic, IBPlusTree)
  private
    FStorage: TBPlusTreeFileStorage;
    FInfo: TBPlusTreeInfo;
    FMinBranchingFactor: integer;
    FMaxBranchingFactor: integer;
  protected
    // Calculate order.
    // For longest keys.
    class function CalcBranchingFactorWide(aIsLeaf: boolean; const aInfo: TBPlusTreeInfo): integer; static; inline;
    // For shortest keys.
    class function CalcBranchingFactorNarrow(aIsLeaf: boolean; const aInfo: TBPlusTreeInfo): integer; static; inline;

    // Check if page must be splitted.
    function IsPageOverflow(aPage: TBTreePage): boolean; inline;

    // Find existing page according to key or create new page.
    // Returned page is locked for write.
    function FindLeafPageByKeyOrCreateNew(const aKey: TBytes): TBTreePage;

    // Get existing page by number.
    function GetPage(aPgNo: TPageNumber; Lock: PagedStorage.TLockKind): TBTreePage;

    // Allocate new page for write.
    function AllocateIndexPage(
      aLeaf: boolean // should add leaf page?
      ): TBTreePage; // TBTreePage;

    // End page access and nil aPage. If aPage is nil, it's ignored.
    procedure ClosePage(
      var aPage: TBTreePage // TBTreePage;
      ); inline;

    function HasRootPage: boolean; inline;

    // Write tree info record into root page and flush root page.
    procedure WriteHeaderPage;

    // Allocate space for value and store value.
    // PageNo: number of the page where data was stored.
    // PageOfs: offset where data was stored.
    //
    // If aValue is empty, no data is written and aPageNo and aOfs are 0.
    function StoreValue(const aValue: TBytes; out aPageNo: TPageNumber;
      out aOfs: TPageSize): boolean;

    // Write value to storage w/o additional checks. Can be used to write new
    // value or to overwrite existing value. In later case you must check
    // new size = old size.
    // If Append is True, data is appended to last data position,
    // *    aBegin-position is overriden to last data position.
    // If Append is False data is written at aBegin-position.
    // aBeginPageNo, aBeginOfs: where to start writing. Can be corrected to
    // *                        contain actual pointer.
    // aEndPageNo, aEndOfs:     will point to next free block, i.e end of
    // *                        written data.
    function WriteValue(const aValue: TBytes;
      var aBeginPageNo: TPageNumber; var aBeginOfs: TPageSize;
      var aEndPageNo: TPageNumber; var aEndOfs: TPageSize;
      aAppend: boolean): boolean;

    // Insert (internal or leaf) aItem to aPage. If page is overflow it should
    // automatically split the page.
    // aNewParent is the internal page actually containing aItem (it can be
    // aPage or other page if aPage was splitted).
    procedure InsertIndexItem(
      aPgNo: TPageNumber;
      aItem: TNodeItem;
      aInfPtr: TPageNumber;
      { out, optional } aItemParentPageNo: PPageNumber);

    function InsertInternal(const aKey, aValue: TBytes; Overwrite: boolean): TBPlusTreeStatus;

    procedure MakePageARoot(aPage: TBTreePage);

    // Delete key index from leaf or internal page. Maintain pages order.
    // aExactKey means aKey exactly deleted.
    // If aExactKey = False, first key > than aKey is deleted (or inf.ptr).
    // RecursionLevel = 0, means delete from leaf.
    function DeleteInternal(aPgNo: TPageNumber; const aKey: TBytes;
      RecursionLevel: integer): TBPlusTreeStatus;

    // Move last item of Left to Right (incl. inf. ptr).
    // Left and Right item must be ordered.
    procedure ShiftLeftsLastItemRight(Left, Right: TBTreePage);
    // Move first item of Right to Left (incl. inf. ptr).
    // Left and Right item must be ordered.
    procedure ShiftRightsFirstItemLeft(Left, Right: TBTreePage);

    // Redistribute items between two nodes.
    // Parent page child pointer key must be updated, because Right's 1st
    // element will be changed.
    // Result is True if both pages are at least half-full.
    // If Result is False, probably you need to Merge pages (for deletion).
    //
    // Input Left,Right can be in any order. Output Left,Right will be ordered.
    // i.e. Right will be > Left.
    function RedistributeItemsEvenly(var Left, Right: TBTreePage): boolean; overload;
    function RedistributeItemsEvenly(var Left, Right: TPageNumber): boolean; overload;
    // You must order Leaf and Right so Left < Right before calling this
    // function. Otherwise item order may be corrupted.
    function RedistributeItemsEvenlyInOrderedPages(Left, Right: TBTreePage): boolean;

    // Merge Left and Right nodes into Left node.
    // Right's items reparented to Left's parent.
    // PAGES MUST BE ORDERED BEFORE CALLING THIS FUNCTION.
    procedure MergeOrderedSiblingPages(Left, Right: TBTreePage); overload;
    procedure MergeOrderedSiblingPages(Left, Right: TPageNumber); overload;
    function GetFirstKeyOfPage(PgNo: TPageNumber): TBytes;
  protected

    // Result is True if changed.
    // aNewChildPgNo: skipped if nil.
    // aNewKey: skipped if nil.
    function UpdatePageItem(aDstPgNo: TPageNumber; const aKey: TBytes;
      aNewChildPgNo: PPageNumber; const aNewKey: TBytes): boolean;

    procedure OrderPagesByKey(var A, B: TBTreePage); overload;
    procedure OrderPagesByKey(var A, B: TPageNumber); overload;

    // Find siblings in parent page for child page aPgNo.
    // If page number not found, it's BAD_PAGE_NUMBER.
    procedure GetSiblingPageNumbers(
      aPgNo: TPageNumber;
      { out,opt } aPgNoLeft, aPgNoRight: PPageNumber); overload;

    // Overload variant, where parent page already locked (LOCK_READ is enough).
    // aSrcPage1stKey: 1st key of source page.
    // aParentPage is locked parent page of source page.
    procedure GetSiblingPageNumbers(
      const aSrcPage1stKey: TBytes;
      aParentPage: TBTreePage;
      { out,opt } aPgNoLeft, aPgNoRight: PPageNumber); overload;

    // If Prev or Next is BAD_PAGE_NUMBER it isn't changed.
    // aPgNo must be valid LEAF page; if it is BAD_PAGE_NUMBER, nothing happens.
    procedure UpdateLeafPageLinks(aPgNo, aPrevNo, aNextNo: TPageNumber);

    procedure ReparentPage(aPgNo, aParentPgNo: TPageNumber);

    // Make all children of a page have aParentPgNo parent page number.
    procedure UpdateParentOfInternalPageItems(aPage: TBTreePage); overload;
    procedure UpdateParentOfInternalPageItems(aPgNo: TPageNumber); overload;

    // Used internally.
    function CreateFromInfo(
      const Info: TBPlusTreeInfo;
      const aFileName: string;
      aReadOnly: boolean;
      aPagesInCache: integer
      ): TBPlusTreeStatus;

    // Create cursor from item description.
    // If ItemDesc is invalid result is nil.
    function CursorCreateFromItemDesc(
      const ItemDesc: TItemDesc): TBPlusTreeCursor;

    // Create cursor pointing to aKey key. Result can be Nil.
    // Leaf page can be specified if known. If it's nil it'll be found by key.
    // aKeyPos defines what key should be taken as cursor:
    // *   kpFirst, kpLast: first or last at leaf page (aKey=nil)
    // *   kpLess, kpEqual, kpGreater (than aKey)
    // If aPrefix is True, aKey will be used as a prefix (false by default).
    function CursorCreateInternal(
      const aKey: TBytes;
      aPage: TLeafBTreePage;
      aKeyPos: TKeyPositions;
      aPrefix: boolean = False): IBPlusTreeCursor;

  public { helpers }

    // Check if page is at least half-full.
    function IsPageHalfFull(aPage: TBTreePage): boolean; overload; // inline;
    // Check if page is at least half-full by page number.
    // If result is False, aPgNo is BAD_PAGE_NUMBER, otherwise aIsHalfFull is set.
    function IsPageHalfFull(aPgNo: TPageNumber; out aIsHalfFull: boolean): boolean; overload;

  public

    //
    // PUBLIC INTERFACE
    //

    // Create new database.
    function CreateNew(
      const aFileName: string;   // database file name
      aMaxKeySize: integer;      // max key length
      aPageSize: TPageSize;      // size of storage page
      aPagesInCache: integer;    // max number of in-memory pages in cache
      aReadOnly: boolean = False // is db read-only?
      ): TBPlusTreeStatus;

    // Open existing database.
    function OpenExisting(
      const aFileName: string;   // database file name
      aPagesInCache: integer;    // max number of in-memory pages in cache
      aReadOnly: boolean = False // is db read-only?
      ): TBPlusTreeStatus;

    // Open existing database or create new.
    function OpenAlways(
      const aFileName: string;   // database file name
      aMaxKeySize: integer;      // max key length
      aPageSize: TPageSize;      // size of storage page
      aPagesInCache: integer;    // max number of in-memory pages in cache
      aReadOnly: boolean = False // is db read-only?
      ): TBPlusTreeStatus;

    destructor Destroy; override;

    // Check if key exists.
    function ContainsKey(const aKey: TBytes): boolean;

    // Insert Key-Value pair. Replace value if key exists.
    function Put(const aKey, aValue: TBytes): TBPlusTreeStatus; override;

    // Delete Value by Key.
    function Delete(const aKey: TBytes): TBPlusTreeStatus; override;

    // Cleanup garbage.
    function Cleanup: TBPlusTreeStatus;

    // Close current database.
    procedure Close;

    // Find number of leaf page containing Key and pointer to Value.
    // Should always find destination(leaf) page where key is located or should
    // be located.
    //
    // If key at leaf page found result is True, PgNo is set to page number.
    // If key not found, result is False.
    // PgNo is set to existing page number where search stopped and key can
    // be inserted.
    // If PgNo is BAD_PAGE_NUMBER (no page) page must be created.
    function FindLeafPageNoByKey(
      const aKey: TBytes;
      out aPgNo: TPageNumber): boolean;

    // Find value page number, offset and size by key.
    function FindValuePosition(
      const aKey: TBytes;
      out aPgNo: TPageNumber;
      out aOffset: TPageSize;
      out aSize: TValueSize): TBPlusTreeStatus;

    // Simplified version of FindValuePosition.
    // Get only value size.
    function GetValueSize(
      const aKey: TBytes;
      out aSize: TValueSize): TBPlusTreeStatus;

    // Read value from value page.
    // Result is True if size read match aSize.
    function ReadValue(aPgNo: TPageNumber; aOfs: TPageSize; aSize: TValueSize;
      out aValue: TBytes): boolean;

    // Retrieve value by key.
    function Get(const aKey: TBytes; out aValue: TBytes): TBPlusTreeStatus; override;

    procedure Flush;

    // Get either first or last page.
    // aFirst: True/False - get first/last leaf page.
    // aLeafPage is the result page. Must be closed by caller.
    // Result is True if page found (aLeafPage <> nil).
    function GetExtremeLeafPage(
      aFirst: boolean;
      out aLeafPage: TBTreePage): boolean;

    // Get either first or last key.
    function GetExtremeKey(aFirst: boolean; out aKey: TBytes): boolean; override;

    // Create cursor pointing to aKey key. Result can be Nil.
    // aKeyPos defines what key should be taken as cursor:
    // *   kpFirst, kpLast: first or last at leaf page (aKey=nil)
    // *   kpLess, kpEqual, kpGreater (than aKey)
    // If aPrefix is True, aKey will be used as a prefix (false by default).
    function CursorCreateEx(
      const aKey: TBytes;
      aKeyPos: TKeyPositions;
      aPrefix: boolean = False): IBPlusTreeCursor; override;

    // Create cursor pointing to first key.
    function CursorCreateFirst: IBPlusTreeCursor;
    // Create cursor pointing to last key.
    function CursorCreateLast: IBPlusTreeCursor;
    // Free cursor.
    // aCursor is checked to be not nil.
    procedure CursorFreeAndNil(var aCursor: TBPlusTreeCursor);
    // Change cursor key depending on aDirection: -1: previous; +1: next
    // Result is true on success.
    function CursorMove(const aCursor: TBPlusTreeCursor; aDirection: integer): boolean;

    // Change cursor key by page. Cursor will point to page first key.
    function CursorMoveToPageStart(const aCursor: TBPlusTreeCursor; aDirection: integer): boolean;

{$IFDEF BPDEBUG}
    class var DbgCallback: procedure;
    class var KeyToStr: TKeyToAnsiString; // To print keys (debugging).
    class function PrintKey(const A: TBytes): AnsiString;
    procedure DumpLeafPages;
{$ENDIF}
    // Checks order of pages is ascending. It means all leaf pages are going in
    // valid order and thus can be traversed without error.
    function VerifyLeafPageLinks(out Processed: uint32): boolean;

    // Checks Parent-Child relations are valid.
    // This is experimental.
    function VerifyIndexParentChild(): boolean; overload;
    function VerifyIndexParentChild(RootPgNo, ParentPgNo: TPageNumber): boolean; overload;
    function GetKeyCount: Cardinal;

    { Properties }

    property Storage: TBPlusTreeFileStorage read FStorage;
    property Info: TBPlusTreeInfo read FInfo;
    property MinBranchingFactor: integer read FMinBranchingFactor;
    property MaxBranchingFactor: integer read FMaxBranchingFactor;
  end;
{$ENDREGION}
{$REGION 'BTree Page intf.'}

  // ChildPageNo points to keys less than current.
  TInternalNodeItem = TNodeItem;

  // ChildPageNo points to data.
  TLeafNodeItem = class(TNodeItem)
    ValueSize: TValueSize;
    PageOffset: TPageSize;
    constructor Create(
      const aKey: TBytes;
      aChildPageNo: TPageNumber;
      aDataSize: TValueSize;
      aPageOffset: TPageSize);
  end;

  TBTreePage = class
  strict private
    FItems: TNodeItemTree;

    // Parent of this page. BAD_PAGE_NUMBER is no parent.
    // Parent page is always non-leaf page (internal).
    FParentPgNo: TPageNumber;
  private
    FcbUsed: TPageSize; // Size of used space in page (0 .. PageSize)

    // Find item pointer.
    function FindItemPtrByKey(const aKey: TBytes): TNodeItemTree.TRBNodePtr;
    procedure FindItemPtrByKeyEx(
      const aKey: TBytes;
      Prv, Cur, Nxt: TNodeItemTree.PRBNodePtr);
    procedure SetParentPgNo(const Value: TPageNumber);
    function GetStoragePageNumber: TPageNumber; inline;
    procedure MakeDirty; inline;

    // class procedure OrderByCbUsed(var A, B: TBTreePage); static; inline;
  public

    StoragePage: TStoragePage;

    constructor Create;

    // Serialize and destroy items
    destructor Destroy; override;

    // Add/Del item (and make node dirty) w/o inf.ptr. correction.
    function AddItem(Item: TNodeItem): TNodeItemTree.TRBNodePtr; // inline;
    procedure DelItem(pItem: TNodeItemTree.TRBNodePtr; bNotify: boolean); // inline;

    // Check if used bytes less then aValue.
    function IsUsedLessThan(aValue: TPageSize): boolean; inline;

    // Use carefully.
    function GetItems: TNodeItemTree; inline;

    function GetItemCount: integer; inline;
    function GetFirstItem: TNodeItemTree.TRBNodePtr; inline;
    function GetLastItem: TNodeItemTree.TRBNodePtr; inline;

    // Find 1st item with key lesser than aKey (for Internal pages).
    // If aItem not found (=nil), aKey >= last item key, i.e. should follow
    // Infinity Pointer.
    procedure FindLesserItem(const aKey: TBytes; out aItem: TNodeItem);

    // Check if current page (node) is leaf or branch (internal node).
    function IsLeaf: boolean; inline;

{$IFDEF BPDEBUG}
    function IsLeafToStr: string;
    function Dump: string;
{$ENDIF}
    function FindItemByKey(const aKey: TBytes): TNodeItem;
    procedure FindEx(const aKey: TBytes;
      out aPrv, aCur, aNxt: TNodeItemTree.TRBNodePtr);

    // Needed before page is deserialized.
    class function IsStoragePageValid(StoragePage: TStoragePage): boolean; static; inline;

    // Needed before page is deserialized.
    class function IsStoragePageLeaf(StoragePage: TStoragePage): boolean; static; inline;

    // Calculate size of item (either leaf or internal).
    class function CalcRecordSize(aIsLeaf: boolean; aKeySize: integer): integer; static; inline;

    // Calculate size of page header.
    class function CalcHeaderSize(aIsLeaf: boolean): integer; static; inline;

    // Write item data to StoragePage data.
    procedure Serialize; virtual; abstract;

    // Read item data from StoragePage data.
    procedure Deserialize; virtual; abstract;

    property ParentPgNo: TPageNumber read FParentPgNo write SetParentPgNo;

    // Storage page number.
    property No: TPageNumber read GetStoragePageNumber;

    property cbUsed: TPageSize read FcbUsed;

  end;

  // Internal page has keys pointing to child nodes, in order to guide search.
  TInternalBTreePage = class(TBTreePage)
  public
    // # of page with keys >= of last key in this page.
    InfinityPointerPageNo: TPageNumber;
    procedure Serialize; override;
    procedure Deserialize; override;

    // Delete infinty pointer branch.
    procedure DelInfPtrBranch;
  end;

  TLeafBTreePage = class(TBTreePage)
  public
    NextPageNo: TPageNumber;
    PrevPageNo: TPageNumber;
    procedure Serialize; override;
    procedure Deserialize; override;
  end;
{$ENDREGION}
{$REGION 'Helper functions'}


function CompareBytes(const A, B: TBytes): integer;
// Move bytes to buffer, result is size of Bytes. If Buf size is too small, only
// Size is written.
function BytesToBuf(const Bytes: TBytes; Buf: pointer; Size: integer): integer;
function AnsiStrToBytes(const A: AnsiString): TBytes;
function BytesToHex(const A: TBytes): string;
function BytesToAnsiStr(const A: TBytes): AnsiString;
{$ENDREGION 'Helper functions'}

implementation

{$REGION 'Consts'}


const
  CURSOR_LOCK = LOCK_READ; // Default lock for cursor.

  SIG_VDDB  = 'VDDB';
  VER_MAJOR = $01;
  VER_MINOR = $00;

  SKeyLengthIsTooBig = 'Key length is too big.';
{$ENDREGION 'Consts'}
{$REGION 'Helpers and Utils'}


procedure Expect(State: boolean = False; const Msg: string = ''); inline;
begin
  if not State then
    if Msg = '' then
      raise Exception.Create('Unexpected state.')
    else
      raise Exception.Create(Msg);
end;

procedure PutPgNo(Ptr: PPageNumber; Value: TPageNumber); inline;
begin
  if Ptr <> nil then
    Ptr^ := Value;
end;

{ TItemDesc }

function TItemDesc.IsValid: boolean;
begin
  Result := (self.LeafPage <> nil) and (self.Item <> nil);
end;

{ TValueHelper<T> }

class procedure TValueHelper<T>.Swap(var A, B: T);
var
  tmp: T;
begin
  tmp := A;
  A := B;
  B := tmp;
end;

function CompareBytes(const A, B: TBytes): integer;
var
  i, MinLen: integer;
begin
  MinLen := Min(Length(A), Length(B));
{$IFDEF DEBUG}
  if MinLen = 0 then
    raise Exception.Create('Wrong values.');
{$ENDIF}
  for i := 0 to MinLen - 1 do
  begin
    Result := A[i] - B[i];
    if Result <> 0 then
      exit;
  end;

  exit(Length(A) - Length(B));
end;

function BytesToBuf(const Bytes: TBytes; Buf: pointer; Size: integer): integer;
begin
  Result := Length(Bytes);
  if Bytes = nil then
    exit(0);
  if Size > Result then
    Size := Result;
  Move(Bytes[0], Buf^, Size);
end;

function AnsiStrToBytes(const A: AnsiString): TBytes;
begin
  SetLength(Result, Length(A));
  if Length(A) <> 0 then
    Move(A[1], Result[0], Length(A));
end;

function BytesToHex(const A: TBytes): string;
const
  hex  = '0123456789ABCDEF';
  n    = 3;
  base = 1;
var
  i: integer;
begin
  SetLength(Result, Length(A) * n);
  for i := 0 to high(A) do
  begin
    Result[i * n + base + 0] := '\';
    Result[i * n + base + 1] := hex[A[i] shr 4];
    Result[i * n + base + 2] := hex[A[i] and $F];
  end;
end;

function BytesToAnsiStr(const A: TBytes): AnsiString;
begin
  SetLength(Result, Length(A));
  if Length(A) <> 0 then
    Move(A[0], Result[1], Length(A));
end;
{$ENDREGION}

{$REGION 'TNodeItemTree impl.'}


procedure TNodeItemTree.Notify(const Item: TNodeItem; Action: TCollectionNotification);
begin
  inherited;
  if Action = cnRemoved then
    Item.Free;
end;
{$ENDREGION}
{$REGION 'Raw headers'}


type

  { Internal page raw headers }

  TInternalPageBaseHdr = packed record
    Flags: TPageFlags;
    KeyCount: TKeyCount;
    ParentPgNo: TPageNumber;
    PgNoInfinity: TPageNumber; // infinity pointer
  end;

  PInternalPageBaseHdr = ^TInternalPageBaseHdr;

  TInternalPageItemHdr = packed record
    PgNoLess: TPageNumber; // page with keys less than in current page
    KeyLen: TKeyLength;
    // ... then goes KeyData: KeySize ...
  end;

  PInternalPageItemHdr = ^TInternalPageItemHdr;

  { Leaf page raw headers }

  TLeafPageBaseHdr = packed record
    Flags: TPageFlags;
    KeyCount: TKeyCount;
    ParentPgNo: TPageNumber;
    PrevPgNo: TPageNumber;
    NextPgNo: TPageNumber;
  end;

  PLeafPageBaseHdr = ^TLeafPageBaseHdr;

  TLeafPageItemHdr = packed record
    DataPgNo: TPageNumber;
    DataOfs: TPageSize;
    DataSize: TValueSize;
    KeyLen: TKeyLength;
    // KeyData: KeySize;
  end;

  PLeafPageItemHdr = ^TLeafPageItemHdr;

  { TValuePageHeader }

  TValuePageHeader = packed record
    NextPgNo: TPageNumber;
  end;

  PValuePageHeader = ^TValuePageHeader;
{$ENDREGION 'Raw headers'}
{$REGION 'TBPlusTree impl.'}


function TBPlusTree.CreateFromInfo(
  const Info: TBPlusTreeInfo;
  const aFileName: string;
  aReadOnly: boolean;
  aPagesInCache: integer
  ): TBPlusTreeStatus;
begin
  // Check version.
  if Info.VerMajor <> VER_MAJOR then
    exit(TBPlusTreeStatus.BP_DB_VERSION_DIFFERS);

  // Check page size.
  if Info.PageSize < SizeOf(Info) then
    exit(TBPlusTreeStatus.BP_PAGE_IS_TOO_SMALL);

  // Calc tree branching factor (order).
  FMinBranchingFactor := Min(
    CalcBranchingFactorWide(True, Info),
    CalcBranchingFactorWide(False, Info));
  FMaxBranchingFactor := Min(
    CalcBranchingFactorNarrow(True, Info),
    CalcBranchingFactorNarrow(False, Info));

  if FMinBranchingFactor <= 2 then
    exit(BP_FANOUT_IS_TOO_SMALL);

  self.FInfo := Info;

  // Create storage.
  FStorage := TBPlusTreeFileStorage.Create(aFileName, aReadOnly, Info.PageSize,
    aPagesInCache);

  exit(BP_OK);
end;

function TBPlusTree.CreateNew(
  const aFileName: string;   // database file name
  aMaxKeySize: integer;      // max key length
  aPageSize: TPageSize;      // size of storage page
  aPagesInCache: integer;    // max number of in-memory pages in cache
  aReadOnly: boolean = False // is db read-only?
  ): TBPlusTreeStatus;
begin
  // Fill root page info.
  FInfo.Sig := SIG_VDDB;
  FInfo.VerMajor := VER_MAJOR;
  FInfo.VerMinor := VER_MINOR;
  FInfo.MaxKeySize := aMaxKeySize;
  FInfo.PageSize := aPageSize;
  FInfo.RootPageNo := BAD_PAGE_NUMBER;
  FInfo.NumberOfKeys := 0;
  FInfo.LastValuePageNo := BAD_PAGE_NUMBER;
  FInfo.LastValueOffset := 0;
  FInfo.TreeHeight := 0;
  FInfo.KeyCount := 0;
  FInfo.TrashSize := 0;

  // Delete existing file.
  if FileExists(aFileName) then
    DeleteFile(aFileName);

  // Create.
  Result := CreateFromInfo(FInfo, aFileName, aReadOnly, aPagesInCache);

  // Flush header.
  if Result = BP_OK then
    WriteHeaderPage;
end;

function TBPlusTree.CursorCreateInternal(
  const aKey: TBytes;
  aPage: TLeafBTreePage;
  aKeyPos: TKeyPositions;
  aPrefix: boolean): IBPlusTreeCursor;
var
  PgNo: TPageNumber;
  Desc: TItemDesc;
  Prv, Cur, Nxt: TNodeItemTree.TRBNodePtr;
  MoveNeeded: int8; // need move of final cursor: -1: left, +1: right
  bPageLocked: boolean;
  Cursor: TBPlusTreeCursor;
begin
  Cursor := nil;
  bPageLocked := False;

  // If no user leaf page, find leaf page where aKey should be.
  if aPage = nil then
  begin
    FindLeafPageNoByKey(aKey, PgNo);
    if PgNo <> BAD_PAGE_NUMBER then
    begin
      aPage := TLeafBTreePage(GetPage(PgNo, CURSOR_LOCK));
      bPageLocked := True;
    end;
  end;

  // If we have no leaf page, fail.
  if aPage = nil then
    exit(nil);

  // Init Desc.
  Desc.Item := nil;
  Desc.LeafPage := aPage;
  MoveNeeded := 0;

  // Find Desc.Item.

  // First (key must be nil)
  if (kpFirst in aKeyPos) then
  begin
    Desc.Item := aPage.GetItems.First; // aKey:nil; first
  end
  // Last (key must be nil)
  else if (kpLast in aKeyPos) then
  begin
    Desc.Item := aPage.GetItems.Last; // aKey:nil; last
  end
  else
  // <, =, >
  begin
    // Key may not be nil.
    if aKey = nil then
      raise Exception.Create('Invalid key value.');

    // Search for keys less, equal or greater.
    aPage.FindItemPtrByKeyEx(aKey, @Prv, @Cur, @Nxt);

    // Check simple case when need equal key.
    if (kpEqual in aKeyPos) then
    begin
      Desc.Item := Cur; // It can be nil if no match found.
    end;

    // >
    if (Desc.Item = nil) and (kpGreater in aKeyPos) then
    begin
      if (Nxt <> nil) then
        Desc.Item := Nxt
      else // Nxt=nil
      begin
        MoveNeeded := 1;
        if Cur <> nil then
          Desc.Item := Cur
        else if (Prv <> nil) then // Cur=nil
          Desc.Item := Prv
        else // Prv=nil
          raise Exception.Create('Cannot acquire cursor position.');
      end;
    end;

    // <
    if (Desc.Item = nil) and (kpLess in aKeyPos) then
    begin
      if (Prv <> nil) then
        Desc.Item := Prv
      else // Prv=nil
      begin
        MoveNeeded := -1;
        if (Cur <> nil) then
          Desc.Item := Cur
        else if (Nxt <> nil) then // Cur=nil
          Desc.Item := Nxt
        else // Nxt=nil
          raise Exception.Create('Cannot acquire cursor position.');
      end;
    end;
  end;

  if not Desc.IsValid then
  begin
    if bPageLocked then
      ClosePage(TBTreePage(aPage));
    exit(nil);
  end;

  // Finally create cursor from Desc.
  Cursor := CursorCreateFromItemDesc(Desc);

  // Adjust cursor if needed.
  if (MoveNeeded <> 0) then
    if not CursorMove(Cursor, MoveNeeded) then
      CursorFreeAndNil(Cursor);

  // Setup prefix for range cursor.
  if Cursor <> nil then
  begin
    if aPrefix then
    begin
      if ValueStartsWithPrefix(Cursor.FItemDesc.Item^.K.Key, aKey) then
        Cursor.FRangePrefix := CopyBytes(aKey)
      else
      begin
        CursorFreeAndNil(Cursor);
        if bPageLocked then
          ClosePage(TBTreePage(aPage));
      end;
    end;
  end;

  Result := Cursor;
end;

function TBPlusTree.CursorCreateFromItemDesc(const ItemDesc: TItemDesc): TBPlusTreeCursor;
begin
  if not ItemDesc.IsValid then
    exit(nil);
  Result := TBPlusTreeCursor.Create;
  Result.FTree := self;
  Result.FItemDesc := ItemDesc;
end;

function TBPlusTree.CursorCreateEx(const aKey: TBytes; aKeyPos: TKeyPositions;
  aPrefix: boolean): IBPlusTreeCursor;
begin
  Result := CursorCreateInternal(aKey, nil, aKeyPos, aPrefix);
end;

function TBPlusTree.CursorCreateFirst: IBPlusTreeCursor;
const
  ISFIRST = True;
var
  LeafPage: TBTreePage;
begin
  if GetExtremeLeafPage(ISFIRST, LeafPage) then
    Result := CursorCreateInternal(nil, TLeafBTreePage(LeafPage), [kpFirst])
  else
    Result := nil;
end;

function TBPlusTree.CursorCreateLast: IBPlusTreeCursor;
const
  ISFIRST = False;
var
  LeafPage: TBTreePage;
begin
  if GetExtremeLeafPage(ISFIRST, LeafPage) then
    Result := CursorCreateInternal(nil, TLeafBTreePage(LeafPage), [kpLast])
  else
    Result := nil;
end;

procedure TBPlusTree.CursorFreeAndNil(var aCursor: TBPlusTreeCursor);
begin
  if aCursor <> nil then
  begin
    aCursor.Free;
    aCursor := nil;
  end;
end;

function TBPlusTree.CursorMove(const aCursor: TBPlusTreeCursor;
  aDirection: integer): boolean;
var
  PageNo: TPageNumber;
  OldDesc: TItemDesc;
begin
  Result := False;

  // Store it because it may be recovered if prefix not matched.
  OldDesc := aCursor.FItemDesc;

  case aDirection of
    1:
      begin
        if aCursor.IsLastAtPage then
        begin
          // Need next page.
          PageNo := TLeafBTreePage(aCursor.FItemDesc.LeafPage).NextPageNo;
          if PageNo <> BAD_PAGE_NUMBER then
          begin
            ClosePage(aCursor.FItemDesc.LeafPage);
            aCursor.FItemDesc.LeafPage := GetPage(PageNo, CURSOR_LOCK);
            if aCursor.FItemDesc.LeafPage <> nil then
            begin
              aCursor.FItemDesc.Item := aCursor.FItemDesc.LeafPage.GetFirstItem;
              Result := True;
            end;
          end;
        end
        else
          // Not last item.
          Result := aCursor.FItemDesc.LeafPage.GetItems.Next(aCursor.FItemDesc.Item);
      end;
    -1:
      begin
        if aCursor.IsFirstAtPage then
        begin
          // Need prev page.
          PageNo := TLeafBTreePage(aCursor.FItemDesc.LeafPage).PrevPageNo;
          if PageNo <> BAD_PAGE_NUMBER then
          begin
            ClosePage(aCursor.FItemDesc.LeafPage);
            aCursor.FItemDesc.LeafPage := GetPage(PageNo, CURSOR_LOCK);
            if aCursor.FItemDesc.LeafPage <> nil then
            begin
              aCursor.FItemDesc.Item := aCursor.FItemDesc.LeafPage.GetLastItem;
              Result := True;
            end;
          end;
        end
        else
          // Not first item.
          Result := aCursor.FItemDesc.LeafPage.GetItems.Prev(aCursor.FItemDesc.Item);
      end;
  end;

  // Correct result according to prefix.
  if Result and (aCursor.FRangePrefix <> nil) then
  begin
    Result := ValueStartsWithPrefix(aCursor.FItemDesc.Item^.K.Key, aCursor.FRangePrefix);

    // If failed to match prefix restore cursor position.
    if not Result then
      aCursor.FItemDesc := OldDesc;
  end;
end;

function TBPlusTree.CursorMoveToPageStart(const aCursor: TBPlusTreeCursor;
  aDirection: integer): boolean;
var
  PgNo: TPageNumber;
begin
  Result := False;

  case aDirection of
    - 1:
      PgNo := TLeafBTreePage(aCursor.FItemDesc.LeafPage).PrevPageNo;
    1:
      PgNo := TLeafBTreePage(aCursor.FItemDesc.LeafPage).NextPageNo;
  else
    PgNo := BAD_PAGE_NUMBER;
  end;

  if PgNo <> BAD_PAGE_NUMBER then
  begin
    ClosePage(TBTreePage(aCursor.FItemDesc.LeafPage));
    aCursor.FItemDesc.LeafPage := GetPage(PgNo, CURSOR_LOCK);
    aCursor.FItemDesc.Item := aCursor.FItemDesc.LeafPage.GetFirstItem;
    exit(True);
  end;
end;

function TBPlusTree.OpenExisting(
  const aFileName: string;
  aPagesInCache: integer;
  aReadOnly: boolean): TBPlusTreeStatus;
var
  Info: TBPlusTreeInfo;
  f: file of byte;
begin
  if not FileExists(aFileName) then
    exit(BP_FILE_NOT_FOUND);

  // Read info.
  AssignFile(f, aFileName);
  Reset(f);
  try
    if FileSize(f) < SizeOf(Info) then
      raise Exception.Create('Invalid tree file.');
    BlockRead(f, Info, SizeOf(Info));
  finally
    CloseFile(f);
  end;
  // Create from info.
  Result := CreateFromInfo(Info, aFileName, aReadOnly, aPagesInCache);
end;

procedure TBPlusTree.OrderPagesByKey(var A, B: TBTreePage);
begin
  if (A.GetItemCount <> 0) and (B.GetItemCount <> 0) then
    if CompareBytes(A.GetFirstItem^.K.Key, B.GetFirstItem^.K.Key) > 0 then
      TValueHelper<TBTreePage>.Swap(A, B);
end;

procedure TBPlusTree.OrderPagesByKey(var A, B: TPageNumber);
var
  P1, P2, OldP1: TBTreePage;
begin
  P1 := GetPage(A, LOCK_READ);
  P2 := GetPage(B, LOCK_READ);
  OldP1 := P1;
  OrderPagesByKey(P1, P2);
  if OldP1 <> P1 then
    TValueHelper<TPageNumber>.Swap(A, B);
  ClosePage(P1);
  ClosePage(P2);
end;

function TBPlusTree.WriteValue(const aValue: TBytes;
  var aBeginPageNo: TPageNumber; var aBeginOfs: TPageSize;
  var aEndPageNo: TPageNumber; var aEndOfs: TPageSize;
  aAppend: boolean): boolean;
var
  Size, ToWrite, SrcOfs: integer;
  StoragePage, NewStoragePage: TStoragePage;
  Hdr: TValuePageHeader;
begin
  Result := False;
  Size := Length(aValue);

  // If trying to write nothing.
  if Size = 0 then
  begin
    aBeginPageNo := BAD_PAGE_NUMBER;
    aBeginOfs := 0;
    aEndPageNo := BAD_PAGE_NUMBER;
    aEndOfs := 0;
    exit(True);
  end;

  // If Append, correct start position).
  if aAppend then
  begin
    // If there's no values at all create 1st value page.
    if FInfo.LastValuePageNo = BAD_PAGE_NUMBER then
    begin
      FInfo.LastValuePageNo := FStorage.AppendPage;
      FInfo.LastValueOffset := 0;
    end;
    // Begin with last value position.
    aBeginPageNo := FInfo.LastValuePageNo;
    aBeginOfs := FInfo.LastValueOffset;
  end;

  if FStorage.BeginPageAccess(aBeginPageNo, LOCK_WRITE, StoragePage) <> STATUS_OK then
    exit;

  // Make sure value page header is coming first.
  // Correct aBeginOfs if needed.
  if aBeginOfs = 0 then
  begin
    if aAppend then
    begin
      // write value header
      Hdr.NextPgNo := BAD_PAGE_NUMBER;
      StoragePage.Write(0, SizeOf(Hdr), @Hdr);
    end;
    aBeginOfs := SizeOf(Hdr);
  end;

  // Start from 'begin' position.
  aEndPageNo := aBeginPageNo;
  aEndOfs := aBeginOfs;

  try
    SrcOfs := 0;
    while Size <> 0 do
    begin
      // Get size we can write in page bounds.
      ToWrite := FInfo.PageSize - aEndOfs;
      if ToWrite <> 0 then
      begin
        if ToWrite > Size then
          ToWrite := Size;
        // Write the block.
        StoragePage.Write(aEndOfs, ToWrite, @aValue[SrcOfs]);
        inc(aEndOfs, ToWrite);
        inc(SrcOfs, ToWrite);
        dec(Size, ToWrite);
      end;
      // If reached page end, but yet have data to write -> get new/next page.
      if (aEndOfs = FInfo.PageSize) and (Size <> 0) then
      begin
        if aAppend then
        begin
          // Allocate and open new page.
          aEndPageNo := FStorage.AppendPage;
          aEndOfs := SizeOf(Hdr);
          // Link this page to new data page.
          Hdr.NextPgNo := aEndPageNo;
          StoragePage.Write(0, SizeOf(Hdr), @Hdr);
        end
        else
        begin
          // Get next page number.
          StoragePage.Read(0, SizeOf(Hdr), @Hdr);
          aEndPageNo := Hdr.NextPgNo;
          aEndOfs := SizeOf(Hdr);
        end;
        // Close this page
        FStorage.EndPageAccess(StoragePage);
{$IFDEF DEBUG}
        if aEndPageNo = BAD_PAGE_NUMBER then
          raise Exception.Create('Trying to get wrong page.');
{$ENDIF}
        // Access new page.
        if FStorage.BeginPageAccess(aEndPageNo, LOCK_WRITE, NewStoragePage) <> STATUS_OK then
          raise Exception.Create('Page lock error.');
        // Update StoragePage.
        StoragePage := NewStoragePage;
      end;
    end;
  finally
    FStorage.EndPageAccess(StoragePage);
  end;

  // If appending, update Info record.
  if aAppend then
  begin
    FInfo.LastValuePageNo := aEndPageNo;
    FInfo.LastValueOffset := aEndOfs;
  end;

  Result := True;
end;

function TBPlusTree.UpdatePageItem(aDstPgNo: TPageNumber; const aKey: TBytes;
  aNewChildPgNo: PPageNumber; const aNewKey: TBytes): boolean;
var
  Page: TBTreePage;
  Item: TNodeItem;
begin
  Result := False;
  if (aDstPgNo = BAD_PAGE_NUMBER) or (aKey = nil) then
    exit;
  Page := GetPage(aDstPgNo, LOCK_WRITE);
  Item := Page.FindItemByKey(aKey);
  if Item <> nil then
  begin
    Result := True;
    if aNewChildPgNo <> nil then
      Item.ChildPageNo := aNewChildPgNo^;
    if aNewKey <> nil then
      Item.Key := CopyBytes(aNewKey);
    Page.MakeDirty;
  end;
  ClosePage(Page);
end;

function TBPlusTree.StoreValue(const aValue: TBytes; out aPageNo: TPageNumber;
  out aOfs: TPageSize): boolean;
begin
  // Is Value is empty, no need to write data.
  if Length(aValue) = 0 then
  begin
    aPageNo := BAD_PAGE_NUMBER;
    aOfs := 0;
    exit(True);
  end;
  // Write.
  Result := WriteValue(aValue,
    aPageNo, aOfs,                                // begin pos
    FInfo.LastValuePageNo, FInfo.LastValueOffset, // end pos
    True);
end;

procedure TBPlusTree.UpdateLeafPageLinks(aPgNo, aPrevNo, aNextNo: TPageNumber);
var
  LeafPage: TLeafBTreePage;
begin
  if (aPgNo <> BAD_PAGE_NUMBER) and
    ((aPrevNo <> BAD_PAGE_NUMBER) or (aNextNo <> BAD_PAGE_NUMBER)) then
  begin
    LeafPage := TLeafBTreePage(GetPage(aPgNo, LOCK_WRITE));

    if aPrevNo <> BAD_PAGE_NUMBER then
      LeafPage.PrevPageNo := aPrevNo;
    if aNextNo <> BAD_PAGE_NUMBER then
      LeafPage.NextPageNo := aNextNo;

    LeafPage.MakeDirty;
    ClosePage(TBTreePage(LeafPage));
  end;
end;

procedure TBPlusTree.UpdateParentOfInternalPageItems(aPgNo: TPageNumber);
var
  Page: TBTreePage;
begin
  Page := GetPage(aPgNo, LOCK_WRITE);
  try
    UpdateParentOfInternalPageItems(Page);
  finally
    ClosePage(Page);
  end;
end;

function TBPlusTree.VerifyIndexParentChild(RootPgNo, ParentPgNo: TPageNumber): boolean;
var
  Page: TBTreePage;
  Item: TNodeItem;
  Inf: TPageNumber;
begin
  if RootPgNo = BAD_PAGE_NUMBER then
    exit(False);
  Result := True;
  Page := GetPage(RootPgNo, LOCK_READ);
  if not Page.IsLeaf then
  begin
    // Branches.
    for Item in Page.GetItems do
      if not VerifyIndexParentChild(Item.ChildPageNo, Page.No) then
        Result := False;

    // Inf. ptr.
    Inf := TInternalBTreePage(Page).InfinityPointerPageNo;
    if Inf <> BAD_PAGE_NUMBER then
      if not VerifyIndexParentChild(Inf, Page.No) then
        Result := False;

    // The actual check
    if Page.ParentPgNo <> ParentPgNo then
    begin
{$IFDEF BPDEBUG}
      if IsConsole then
        writeln(Format('Page(%d): incorrect parent(%d)', [RootPgNo, ParentPgNo]));
{$ENDIF}
      Result := False;
    end;
  end;
  ClosePage(Page);
end;

function TBPlusTree.VerifyIndexParentChild: boolean;
begin
  Result := VerifyIndexParentChild(FInfo.RootPageNo, BAD_PAGE_NUMBER);
end;

function TBPlusTree.VerifyLeafPageLinks(out Processed: uint32): boolean;
var
  Cur: TBTreePage;
  NextPageNo: TPageNumber;
  iPage: integer;
  kPrv, kCur: TBytes;
begin
  iPage := 0;
  Processed := 0;
  // Get leftmost leaf.
  if GetExtremeLeafPage(True, Cur) then
    while True do
    begin
      if iPage <> 0 then
      begin
        // kPrv is set on prev. iteration.
        kCur := CopyBytes(Cur.GetFirstItem^.K.Key);
        // Check keys are ascending.
        if CompareBytes(kPrv, kCur) >= 0 then
          exit(False);
      end;
      // Set for next iteration.
      kPrv := CopyBytes(Cur.GetLastItem^.K.Key);
      // Move next.
      NextPageNo := TLeafBTreePage(Cur).NextPageNo;
      ClosePage(Cur);
      if NextPageNo = BAD_PAGE_NUMBER then
        break;
      Cur := GetPage(NextPageNo, LOCK_READ);
      inc(iPage);
      inc(Processed);
    end;
  // No problems found.
  exit(True);
end;

procedure TBPlusTree.UpdateParentOfInternalPageItems(aPage: TBTreePage);
  procedure Visit(PgNo: TPageNumber);
  begin
    ReparentPage(PgNo, aPage.No);
  end;

var
  Page: TInternalBTreePage;
  Item: TNodeItemTree.TRBNodePtr;
begin
  Page := TInternalBTreePage(aPage);
  Item := Page.GetFirstItem;
  while Item <> nil do
  begin
    Visit(Item.K.ChildPageNo);
    Page.GetItems.Next(Item);
  end;
  if Page.InfinityPointerPageNo <> BAD_PAGE_NUMBER then
    Visit(Page.InfinityPointerPageNo);
end;

procedure TBPlusTree.InsertIndexItem(
  aPgNo: TPageNumber;
  aItem: TNodeItem;
  aInfPtr: TPageNumber;
  { out, optional } aItemParentPageNo: PPageNumber);
var
  bIsLeaf: boolean;
  Item: TInternalNodeItem;
  Left, Right, TmpPage: TBTreePage;
  wasLeft, wasRight, InfPtrPgNo, NewParentPageNo, LeftPgNo, RightPgNo,
    ParentPgNo: TPageNumber;
  MiddleKey: TBytes;
  Added: TNodeItemTree.TRBNodePtr;
begin
  Right := GetPage(aPgNo, LOCK_WRITE);

  bIsLeaf := Right.IsLeaf;

  // Put item.
  Added := Right.AddItem(aItem);

  // If it's internal node and added item become last, update infinity pointer.
  if not bIsLeaf then
    if Added = Right.GetLastItem then
      TInternalBTreePage(Right).InfinityPointerPageNo := aInfPtr;

  // Check overflow.
  if not IsPageOverflow(Right) then
  begin
    // Simple case. No split and parent is same.
    if (aItemParentPageNo <> nil) then
      aItemParentPageNo^ := Right.No;
    ClosePage(Right);
    exit;
  end;

  // Split

  // Allocate page of same type (leaf or branch) and redistribute items.
  Left := AllocateIndexPage(bIsLeaf);

  // Get page numbers.
  LeftPgNo := Left.No;
  RightPgNo := Right.No;

  // Compiler friendly.
  wasLeft := BAD_PAGE_NUMBER;
  wasRight := BAD_PAGE_NUMBER;

  // If leaf, remember prev/next page numbers for further linking.
  if bIsLeaf then
  begin
    wasLeft := TLeafBTreePage(Right).PrevPageNo;
    wasRight := TLeafBTreePage(Right).NextPageNo;
  end;

  RedistributeItemsEvenly(Left, Right);

  // Make pages dirty.
  Left.MakeDirty;
  Right.MakeDirty;

  // If internal page, update infinity pointer. Right page has it unchanged.
  if (not bIsLeaf) then
  begin
    InfPtrPgNo := Right.GetFirstItem^.K.ChildPageNo;
    TInternalBTreePage(Left).InfinityPointerPageNo := InfPtrPgNo;
  end;

  // Find page number where inserted item is now.
  if (aItemParentPageNo <> nil) then
    if Right.FindItemByKey(aItem.Key) <> nil then
      aItemParentPageNo^ := RightPgNo
    else
      aItemParentPageNo^ := LeftPgNo;

  // If leaf pages, link each other and correct old links.
  if bIsLeaf then
  begin
    // Link Left.
    TLeafBTreePage(Left).PrevPageNo := wasLeft;
    TLeafBTreePage(Left).NextPageNo := RightPgNo;
    // Link Right.
    TLeafBTreePage(Right).PrevPageNo := LeftPgNo;
    TLeafBTreePage(Right).NextPageNo := wasRight;
    // Update external nodes.
    UpdateLeafPageLinks(wasLeft, BAD_PAGE_NUMBER, LeftPgNo);
    UpdateLeafPageLinks(wasRight, RightPgNo, BAD_PAGE_NUMBER);
  end;

  // Get existing or create new parent page to move middle item into it.
  if Right.ParentPgNo <> BAD_PAGE_NUMBER then
    ParentPgNo := Right.ParentPgNo
  else
  begin
    // Allocate new (internal) parent page.
    TmpPage := AllocateIndexPage(False);
    ParentPgNo := TmpPage.No;
    ClosePage(TmpPage);
  end;

  // Get middle key to be inserted into parent page later.
  MiddleKey := CopyBytes(Right.GetFirstItem^.K.Key);
  // For internal page we remove middle item as it will be moved up the tree.
  // For leaf it is just copied, thus not moved.
  if not bIsLeaf then
    Right.DelItem(Right.GetFirstItem, True);

  ClosePage(Left);
  ClosePage(Right);

  // Create middle item and add it into parent (recursively).
  Item := TInternalNodeItem.Create(MiddleKey, LeftPgNo);
  InsertIndexItem(ParentPgNo, Item, RightPgNo, @NewParentPageNo);

  // Parent page may change after insertion, if it was splitted recursively.
  if NewParentPageNo <> ParentPgNo then
    ParentPgNo := NewParentPageNo;

  // Update parent for Left.
  TmpPage := GetPage(LeftPgNo, LOCK_WRITE);
  TmpPage.ParentPgNo := ParentPgNo;
  ClosePage(TmpPage);
  // Update parent for Right.
  TmpPage := GetPage(RightPgNo, LOCK_WRITE);
  TmpPage.ParentPgNo := ParentPgNo;
  ClosePage(TmpPage);

  // If old splitted page was root, make parent page a new root.
  if RightPgNo = FInfo.RootPageNo then
  begin
    FInfo.RootPageNo := ParentPgNo;
    inc(FInfo.TreeHeight);
  end;

  // Internal page was splitted, thus need reparent child items.
  if not bIsLeaf then
    UpdateParentOfInternalPageItems(LeftPgNo);
end;

function TBPlusTree.OpenAlways(
  const aFileName: string;   // database file name
  aMaxKeySize: integer;      // max key length
  aPageSize: TPageSize;      // size of storage page
  aPagesInCache: integer;    // max number of in-memory pages in cache
  aReadOnly: boolean = False // is db read-only?
  ): TBPlusTreeStatus;
begin
  if FileExists(aFileName) then
    Result := OpenExisting(aFileName, aPagesInCache, aReadOnly)
  else
    Result := CreateNew(aFileName, aMaxKeySize, aPageSize, aPagesInCache, aReadOnly);
end;

destructor TBPlusTree.Destroy;
begin
  self.Close;
  inherited;
end;

{$IFDEF BPDEBUG}


procedure TBPlusTree.DumpLeafPages;
var
  LeafPage: TBTreePage;
  NextPageNo: TPageNumber;
begin
  if IsConsole then
  begin
    writeln('Dump Leaf Pages');
    if GetExtremeLeafPage(True, LeafPage) then
    begin
      while True do
      begin
        writeln(Format('Page:%d; Prv:%d; Nxt:%d; "%s".."%s"', [
          LeafPage.No,
          TLeafBTreePage(LeafPage).PrevPageNo,
          TLeafBTreePage(LeafPage).NextPageNo,
          PrintKey(LeafPage.GetFirstItem.K.Key),
          PrintKey(LeafPage.GetLastItem.K.Key)
          ]));

        NextPageNo := TLeafBTreePage(LeafPage).NextPageNo;
        ClosePage(LeafPage);

        if NextPageNo = BAD_PAGE_NUMBER then
          break;

        LeafPage := GetPage(NextPageNo, LOCK_READ);
      end;
    end;
  end;
end;
{$ENDIF}


function TBPlusTree.FindLeafPageByKeyOrCreateNew(const aKey: TBytes): TBTreePage;
var
  FoundPgNo: TPageNumber;
begin
  FindLeafPageNoByKey(aKey, FoundPgNo);

  if FoundPgNo <> BAD_PAGE_NUMBER then
  begin
    // Key may be found or not found, but we know the page where it resides
    // or should reside. It's enough for this function.
    Result := GetPage(FoundPgNo, LOCK_WRITE); // if found, get page
{$IFDEF DEBUG}
    if not Result.IsLeaf then
      raise Exception.Create('Leaf page expected.');
{$ENDIF}
    exit;
  end;

  // Page was not found, i.e. we have to allocate new.
  Result := AllocateIndexPage(True); // if not found, add new

{$IFDEF DEBUG}
  if not Result.IsLeaf then
    raise Exception.Create('Leaf page expected.');
{$ENDIF}

end;

function TBPlusTree.FindLeafPageNoByKey(
  const aKey: TBytes;
  out aPgNo: TPageNumber): boolean;
var
  Page: TBTreePage;
  Item: TNodeItem;
  bIsLeaf: boolean;
  tmpPgNo: TPageNumber;
begin
  aPgNo := BAD_PAGE_NUMBER;

  if not HasRootPage then
  begin
    // Tree is empty, as there's no root.
    exit(False);
  end;

  // Start searching key from root page and going down.
  tmpPgNo := FInfo.RootPageNo;

  while True do
  begin
    // We're searching key, so read access is enough.
    Page := GetPage(tmpPgNo, LOCK_READ);
    try
      bIsLeaf := Page.IsLeaf;

      // Exit here.
      if bIsLeaf then
      begin
        aPgNo := Page.No;
        Result := Page.FindItemByKey(aKey) <> nil;
        exit;
      end;

      // Not leaf.

      // Find sub-page.
      Page.FindLesserItem(aKey, Item);
      if Item = nil then
        // Use Inifinty Pointer.
        tmpPgNo := TInternalBTreePage(Page).InfinityPointerPageNo
      else
        tmpPgNo := Item.ChildPageNo;

      if tmpPgNo = BAD_PAGE_NUMBER then
        raise Exception.CreateFmt('Wrong Child Page for page #%d', [Page.No]);

    finally
      ClosePage(Page);
    end;

  end; // of while

  if (Page <> nil) then
    ClosePage(Page);
end;

function TBPlusTree.FindValuePosition(
  const aKey: TBytes;
  out aPgNo: TPageNumber;
  out aOffset: TPageSize;
  out aSize: TValueSize): TBPlusTreeStatus;
var
  Page: TBTreePage;
  Item: TLeafNodeItem;
  IndexPageNo: TPageNumber;
begin
  // Find page with key.
  if FindLeafPageNoByKey(aKey, IndexPageNo) then
    Result := TBPlusTreeStatus.BP_OK
  else
    Result := TBPlusTreeStatus.BP_KEY_NOT_FOUND;

  if (Result = TBPlusTreeStatus.BP_OK) then
  begin
    // Get found page.
    Page := GetPage(IndexPageNo, LOCK_READ);
    try
      Expect(Page <> nil, 'Can''t get page.');
      Expect(Page.IsLeaf, 'Expected leaf page.');

      // Find item in page by key.
      Item := TLeafNodeItem(Page.FindItemByKey(aKey));
      Expect(Item <> nil, 'Unexpected item.');

      aPgNo := Item.ChildPageNo;
      aOffset := Item.PageOffset;
      aSize := Item.ValueSize;
    finally
      ClosePage(Page);
    end;
  end;
end;

function TBPlusTree.GetExtremeKey(aFirst: boolean;
  out aKey: TBytes): boolean;
var
  LeafPage: TBTreePage;
begin
  aKey := nil;
  Result := False;
  if GetExtremeLeafPage(aFirst, LeafPage) then
  begin
    if aFirst then
      aKey := TLeafBTreePage(LeafPage).GetFirstItem^.K.Key
    else
      aKey := TLeafBTreePage(LeafPage).GetLastItem^.K.Key;
    ClosePage(LeafPage);
    exit(True);
  end;
end;

function TBPlusTree.GetExtremeLeafPage(
  aFirst: boolean;
  out aLeafPage: TBTreePage): boolean;
var
  CurPage: TBTreePage;
  CurPageNo: TPageNumber;
begin
  aLeafPage := nil;
  if FInfo.RootPageNo = BAD_PAGE_NUMBER then
    exit(False);
  CurPageNo := FInfo.RootPageNo;
  while True do
  begin
    CurPage := GetPage(CurPageNo, LOCK_READ);

    // If we found leaf, we're done.
    if CurPage.IsLeaf then
    begin
      // Caller must close the page.
      aLeafPage := CurPage;
      exit(True);
    end;

    // We're in Internal page, so get next page.
    if aFirst then
      CurPageNo := CurPage.GetFirstItem.K.ChildPageNo
    else
      CurPageNo := TInternalBTreePage(CurPage).InfinityPointerPageNo;

    // Close current page and continue scan.
    ClosePage(CurPage);
  end;
end;

function TBPlusTree.GetFirstKeyOfPage(PgNo: TPageNumber): TBytes;
var
  Page: TBTreePage;
begin
  Page := GetPage(PgNo, LOCK_READ);
  Result := Page.GetFirstItem^.K.Key;
  ClosePage(Page);
end;

function TBPlusTree.GetKeyCount: Cardinal;
begin
  Result := FInfo.KeyCount;
end;

procedure TBPlusTree.MakePageARoot(aPage: TBTreePage);
begin
  FInfo.RootPageNo := aPage.No;
  aPage.ParentPgNo := BAD_PAGE_NUMBER;
end;

procedure TBPlusTree.Flush;
begin
  if FStorage <> nil then
  begin
    WriteHeaderPage;
    FStorage.Flush;
  end;
end;

procedure TBPlusTree.WriteHeaderPage;
var
  Page: TStoragePage;
begin
  if FStorage.BeginPageAccess(HEADER_PAGE_NUMBER, TLockKind.LOCK_WRITE, Page) <> TStatus.STATUS_OK then
    raise Exception.Create('Can''t lock header page.');
  try
    Page.Write(0, SizeOf(FInfo), @FInfo);
    FStorage.FlushPage(Page);
  finally
    FStorage.EndPageAccess(Page);
  end;
end;

function TBPlusTree.GetPage(aPgNo: TPageNumber;
  Lock: PagedStorage.TLockKind): TBTreePage;
var
  StoragePage: TStoragePage;
begin
  if FStorage.BeginPageAccess(aPgNo, Lock, StoragePage) <> STATUS_OK then
    raise Exception.CreateFmt('Can''t lock page %d', [aPgNo]);

  // If page is cached, return it.
  if StoragePage.CustomData <> nil then
  begin
    Result := StoragePage.CustomData;
    if Result.StoragePage = nil then
      raise Exception.Create('todo');
    exit;
  end;

  // Deserialize page data if page was not cached (StoragePage.CustomData = nil)

  if not TBTreePage.IsStoragePageValid(StoragePage) then
    raise Exception.Create('Page corrupted.');

  if TBTreePage.IsStoragePageLeaf(StoragePage) then
    Result := TLeafBTreePage.Create
  else
    Result := TInternalBTreePage.Create;

  StoragePage.CustomData := Result;
  Result.StoragePage := StoragePage;
  Result.Deserialize;
end;

procedure TBPlusTree.GetSiblingPageNumbers(const aSrcPage1stKey: TBytes;
  aParentPage: TBTreePage; aPgNoLeft, aPgNoRight: PPageNumber);
var
  Prv, Cur, Nxt: TNodeItemTree.TRBNodePtr;
begin
  aParentPage.FindItemPtrByKeyEx(aSrcPage1stKey, @Prv, @Cur, @Nxt);
  // left
  if aPgNoLeft <> nil then
  begin
    if Cur <> nil then
      aPgNoLeft^ := Cur^.K.ChildPageNo
    else if Prv <> nil then
      aPgNoLeft^ := Prv^.K.ChildPageNo;
  end;
  // right
  if aPgNoRight <> nil then
  begin
    if Nxt <> nil then
    begin
      aParentPage.GetItems.Next(Nxt); // next item after next.
      if Nxt <> nil then
        aPgNoRight^ := Nxt^.K.ChildPageNo
      else
        aPgNoRight^ := TInternalBTreePage(aParentPage).InfinityPointerPageNo;
    end;
  end;
end;

function TBPlusTree.GetValueSize(const aKey: TBytes;
  out aSize: TValueSize): TBPlusTreeStatus;
var
  DummyPgNo: TPageNumber;
  DummyOffset: TPageSize;
begin
  Result := FindValuePosition(aKey, DummyPgNo, DummyOffset, aSize);
end;

procedure TBPlusTree.GetSiblingPageNumbers(aPgNo: TPageNumber;
  { out,opt } aPgNoLeft, aPgNoRight: PPageNumber);
var
  Page: TBTreePage;
  ParentPgNo: TPageNumber;
  TmpKey: TBytes;
begin
  if (aPgNoLeft = nil) and (aPgNoRight = nil) then
    exit;
  // Init.
  PutPgNo(aPgNoLeft, BAD_PAGE_NUMBER);
  PutPgNo(aPgNoRight, BAD_PAGE_NUMBER);
  // Root have no siblings.
  if aPgNo = FInfo.RootPageNo then
    exit;
  // Get parent page number and 1st key of source page.
  Page := GetPage(aPgNo, LOCK_READ);
  ParentPgNo := Page.ParentPgNo;
  TmpKey := CopyBytes(Page.GetFirstItem.K.Key);
  ClosePage(Page);
{$IFDEF DEBUG}
  if ParentPgNo = BAD_PAGE_NUMBER then
    raise Exception.Create('Bad page number.');
{$ENDIF}
  Page := GetPage(ParentPgNo, LOCK_WRITE); // Get parent page.
  GetSiblingPageNumbers(TmpKey, Page, aPgNoLeft, aPgNoRight); // Search.
  ClosePage(Page); // Close parent.
end;

function TBPlusTree.ReadValue(aPgNo: TPageNumber; aOfs: TPageSize;
  aSize: TValueSize; out aValue: TBytes): boolean;
var
  Page: TStoragePage;
  ReadCnt: cardinal;
  p: PByte;
  Hdr: TValuePageHeader;
  Ofs, ToRead: TPageSize;
  Size: TValueSize;
begin
  Result := False;
  if FStorage.BeginPageAccess(aPgNo, LOCK_READ, Page) <> STATUS_OK then
    raise Exception.Create('Can''t get value page.');
  try
    ReadCnt := 0;
    SetLength(aValue, aSize);
    p := @aValue[0];
    Ofs := aOfs;
    Size := aSize;
    while (Size <> 0) do
    begin
      // calc size to read
      ToRead := FInfo.PageSize - Ofs; // max can read
      if ToRead > Size then
        ToRead := Size; // correct
      // read
      Page.Read(Ofs, ToRead, p);
      // update counters
      dec(Size, ToRead);
      inc(p, ToRead);
      inc(Ofs, ToRead);
      inc(ReadCnt, ToRead);
      // if reached page end and have data to read
      if (Ofs = FInfo.PageSize) and (Size <> 0) then
      begin
        // Get header.
        Page.Read(0, SizeOf(Hdr), @Hdr);
        // Close this page.
        FStorage.EndPageAccess(Page);
        // Get next page.
        if FStorage.BeginPageAccess(Hdr.NextPgNo, LOCK_READ, Page) <> STATUS_OK then
          raise Exception.Create('Can''t get value page.');
        // Then continue reading.
        Ofs := SizeOf(Hdr);
      end;
    end; // while
    // make final checks
    Result := (ReadCnt = aSize);
    if not Result then
      SetLength(aValue, ReadCnt); // resize buffer to actual data size
  finally
    FStorage.EndPageAccess(Page);
  end;
end;

procedure TBPlusTree.ShiftLeftsLastItemRight(Left, Right: TBTreePage);
var
  bIsLeaf: boolean;
  intL, intR: TInternalBTreePage;
  pItem: TNodeItemTree.TRBNodePtr;
  Item: TNodeItem;
  TmpKey: TBytes;
begin
  bIsLeaf := Left.IsLeaf;
  if (not bIsLeaf) then
  begin
    intL := TInternalBTreePage(Left);
    if intL.InfinityPointerPageNo <> BAD_PAGE_NUMBER then
    begin
      Expect(Right.GetItemCount > 0);
      Expect(Left.GetItemCount > 0);
      // Move left inf.ptr. to right begin.
      TmpKey := GetFirstKeyOfPage(Right.GetFirstItem.K.ChildPageNo);
      Item := TInternalNodeItem.Create(TmpKey, intL.InfinityPointerPageNo);
      Right.AddItem(Item); // at begin
      // Update left inf.ptr.
      pItem := Left.GetItems.Last;
      intL.InfinityPointerPageNo := pItem^.K.ChildPageNo;
      Left.DelItem(pItem, True);
      // if Right inf.ptr is null, correct it.
      intR := TInternalBTreePage(Right);
      if intR.InfinityPointerPageNo = BAD_PAGE_NUMBER then
      begin
        pItem := Right.GetLastItem;
        intR.InfinityPointerPageNo := pItem^.K.ChildPageNo;
        Right.DelItem(pItem, True);
      end;
      exit;
    end;
  end;
  // Simple item.
  pItem := Left.GetLastItem;
  Expect(pItem <> nil);
  Right.AddItem(pItem^.K);
  Left.DelItem(pItem, False); // don't notify (not to free item).
end;

procedure TBPlusTree.ShiftRightsFirstItemLeft(Left, Right: TBTreePage);
var
  bIsLeaf: boolean;
  intL: TInternalBTreePage;
  pItem: TNodeItemTree.TRBNodePtr;
  Item: TNodeItem;
  TmpKey: TBytes;
begin
  bIsLeaf := Left.IsLeaf;
  if (not bIsLeaf) then
  begin
    intL := TInternalBTreePage(Left);
    if intL.InfinityPointerPageNo <> BAD_PAGE_NUMBER then
    begin
      Expect(Right.GetItemCount > 0);
      Expect(Left.GetItemCount > 0);
      // Move left inf.ptr. to left end.
      TmpKey := GetFirstKeyOfPage(Right.GetFirstItem.K.ChildPageNo);
      Item := TInternalNodeItem.Create(TmpKey, intL.InfinityPointerPageNo);
      Left.AddItem(Item); // at end
      // Update left inf.ptr.
      pItem := Right.GetItems.First;
      intL.InfinityPointerPageNo := pItem^.K.ChildPageNo;
      Right.DelItem(pItem, True);
      exit;
    end;
  end;
  // Simple item.
  pItem := Right.GetFirstItem;
  Expect(pItem <> nil);
  Left.AddItem(pItem^.K);
  Right.DelItem(pItem, False); // don't notify (not to free item).
end;

function TBPlusTree.RedistributeItemsEvenly(var Left, Right: TPageNumber): boolean;
var
  PgLeft, PgRight, OldPgLeft: TBTreePage;
begin
  // Get pages.
  PgLeft := GetPage(Left, LOCK_WRITE);
  OldPgLeft := PgLeft;
  PgRight := GetPage(Right, LOCK_WRITE);
  // Process.
  Result := RedistributeItemsEvenly(PgLeft, PgRight);
  // If pages were swapped, swap numbers also.
  if OldPgLeft <> PgLeft then
    TValueHelper<TPageNumber>.Swap(Left, Right);
  ClosePage(PgLeft);
  ClosePage(PgRight);
end;

function TBPlusTree.RedistributeItemsEvenlyInOrderedPages(
  Left, Right: TBTreePage): boolean;
var
  bLeftIsBigger: boolean;
begin
{$IFDEF BPDEBUG}
  if IsConsole then
    writeln(Format('Redistribute items between pages: %d,%d', [Left.No, Right.No]));
{$ENDIF}
  bLeftIsBigger := Left.FcbUsed > Right.FcbUsed;
  while True do
  begin
{$IFDEF DEBUG}
    if Right.GetItems.Count = 0 then
      raise Exception.Create('Redistribution failure.');
{$ENDIF}
    if bLeftIsBigger then
    begin
      if Right.FcbUsed >= Left.FcbUsed then
        break;
      ShiftLeftsLastItemRight(Left, Right);
    end
    else
    begin
      if Left.FcbUsed >= Right.FcbUsed then
        break;
      ShiftRightsFirstItemLeft(Left, Right);
    end;
  end;
  Result := IsPageHalfFull(Left) and IsPageHalfFull(Right);
end;

function TBPlusTree.RedistributeItemsEvenly(var Left, Right: TBTreePage): boolean;
begin
  OrderPagesByKey(Left, Right);
  Result := RedistributeItemsEvenlyInOrderedPages(Left, Right);
end;

procedure TBPlusTree.MergeOrderedSiblingPages(Left, Right: TBTreePage);
var
  pItem: TNodeItemTree.TRBNodePtr;
  intItem: TInternalNodeItem;
  leaf: TLeafBTreePage;
  LeftIntPage, RightIntPage: TInternalBTreePage;
  bIsLeaf: boolean;
  tmpPg: TPageNumber;
  TmpKey: TBytes;
  Result: TBTreePage;
begin
{$IFDEF DEBUG}
  if IsPageHalfFull(Left) or IsPageHalfFull(Right) then
    raise Exception.Create('Half-full pages cannot be merged.');
{$ENDIF}
{$IFDEF BPDEBUG}
  if IsConsole then
    writeln(Format('Merging pages: %d,%d->%d', [Left.No, Right.No, Left.No]));
{$ENDIF}
  Result := Left; // map result to left page
  bIsLeaf := Result.IsLeaf;

  // Compiler friendly.
  LeftIntPage := nil;

  // Convert inf.ptr. at Left into branch item.
  if not bIsLeaf then
  begin
    LeftIntPage := TInternalBTreePage(Left);
    RightIntPage := TInternalBTreePage(Right);

    if LeftIntPage.InfinityPointerPageNo <> BAD_PAGE_NUMBER then
    begin
      tmpPg := TInternalBTreePage(Right).GetFirstItem.K.ChildPageNo;
      TmpKey := GetFirstKeyOfPage(tmpPg);
      intItem := TInternalNodeItem.Create(TmpKey, LeftIntPage.InfinityPointerPageNo);
      Left.AddItem(intItem);
    end;

    // Right's inf.ptr. comes to left inf.ptr.
    LeftIntPage.InfinityPointerPageNo := RightIntPage.InfinityPointerPageNo;
    // Remove Right's page inf.ptr and pointer to parent.
    RightIntPage.InfinityPointerPageNo := BAD_PAGE_NUMBER;
    RightIntPage.ParentPgNo := BAD_PAGE_NUMBER;
  end;

  // Move items.
  while Right.GetItemCount > 0 do
  begin
    pItem := Right.GetFirstItem;
    Left.AddItem(pItem.K);
    Right.DelItem(pItem, False); // don't notify
  end;

  // Correct new inf.ptr. if needed.
  if not bIsLeaf then
  begin
    if LeftIntPage.InfinityPointerPageNo = BAD_PAGE_NUMBER then
    begin
      pItem := Left.GetLastItem;
      LeftIntPage.InfinityPointerPageNo := pItem^.K.ChildPageNo;
      Left.DelItem(pItem, True);
    end;
  end;

  if bIsLeaf then
  begin
    leaf := TLeafBTreePage(Result);
    // Current node links.
    leaf.PrevPageNo := TLeafBTreePage(Left).PrevPageNo;
    leaf.NextPageNo := TLeafBTreePage(Right).NextPageNo;
    // Adjacent node links.
    UpdateLeafPageLinks(leaf.PrevPageNo, BAD_PAGE_NUMBER, leaf.No);
    UpdateLeafPageLinks(leaf.NextPageNo, leaf.No, BAD_PAGE_NUMBER);
  end;

{$IFDEF DEBUG}
  // Normally it shouldn't happen.
  if IsPageOverflow(Result) then
    raise Exception.CreateFmt('Unexpected page(%d) overflow.', [Result.No]);
{$ENDIF}
  // Reparent final items of internal page.
  // Not much optimal: left part doesn't need reparenting.
  if not bIsLeaf then
    UpdateParentOfInternalPageItems(Result);
end;

procedure TBPlusTree.MergeOrderedSiblingPages(Left, Right: TPageNumber);
var
  L, R: TBTreePage;
begin
  L := GetPage(Left, LOCK_WRITE);
  R := GetPage(Right, LOCK_WRITE);
  MergeOrderedSiblingPages(L, R);
  ClosePage(L);
  ClosePage(R);
end;

procedure TBPlusTree.ReparentPage(aPgNo, aParentPgNo: TPageNumber);
var
  Page: TBTreePage;
  bNeedUpdate: boolean;
begin
  if (aPgNo <> BAD_PAGE_NUMBER) and (aParentPgNo <> BAD_PAGE_NUMBER) then
  begin
    Page := GetPage(aPgNo, LOCK_READ);
    bNeedUpdate := Page.ParentPgNo <> aParentPgNo;
    ClosePage(Page);
    if bNeedUpdate then
    begin
      Page := GetPage(aPgNo, LOCK_WRITE);
      Page.ParentPgNo := aParentPgNo;
      ClosePage(Page);
    end;
{$IFDEF BPDEBUG}
    if IsConsole then
      writeln('page ', aPgNo, ' changed parent to ', aParentPgNo);
{$ENDIF}
  end;
end;

function TBPlusTree.Get(const aKey: TBytes;
  out aValue: TBytes): TBPlusTreeStatus;
var
  PgNo: TPageNumber;
  Offset: TPageSize;
  Size: TValueSize;
begin
  Result := FindValuePosition(aKey, PgNo, Offset, Size);
  if Result = TBPlusTreeStatus.BP_OK then
  begin
    if ReadValue(PgNo, Offset, Size, aValue) then
      Result := TBPlusTreeStatus.BP_OK
    else
      Result := TBPlusTreeStatus.BP_VALUE_READ_ERROR;
  end;
end;

function TBPlusTree.HasRootPage: boolean;
begin
  Result := FInfo.RootPageNo <> BAD_PAGE_NUMBER;
end;

function TBPlusTree.AllocateIndexPage(aLeaf: boolean): TBTreePage;
var
  StoragePage: TStoragePage;
  PageNumber: PagedStorage.TStoragePageNumber;
begin
  // Create storage page with newly created BTree page as CustomObject.
  PageNumber := FStorage.AppendPage;

  if FStorage.BeginPageAccess(PageNumber, LOCK_WRITE, StoragePage) <> STATUS_OK then
    raise Exception.Create('TBPlusTree.AllocPage: Can''t lock page.');

  if aLeaf then
    Result := TLeafBTreePage.Create
  else
    Result := TInternalBTreePage.Create;

  // Register storage page and self to be freed when storage page get freed.
  TBTreePage(Result).StoragePage := StoragePage;

  // Save initial state to page.
  TBTreePage(Result).Serialize;

  // bind page to storage page
  TBTreePage(Result).StoragePage.CustomData := Result;

  // If there's no root page, make newly created page a root.
  if not HasRootPage then
  begin
    FInfo.RootPageNo := StoragePage.No;
  end;
end;

class function TBPlusTree.CalcBranchingFactorNarrow(aIsLeaf: boolean;
  const aInfo: TBPlusTreeInfo): integer;
begin
  if aIsLeaf then
    Result := (aInfo.PageSize - SizeOf(TLeafPageBaseHdr))
      div (SizeOf(TLeafPageItemHdr) + SHORTEST_KEY_LENGTH)
  else
  begin
    Result := (aInfo.PageSize - SizeOf(TInternalPageBaseHdr))
      div (SizeOf(TInternalPageItemHdr) + SHORTEST_KEY_LENGTH);
    dec(Result);
  end;
end;

class function TBPlusTree.CalcBranchingFactorWide(aIsLeaf: boolean; const aInfo: TBPlusTreeInfo): integer;
begin
  if aIsLeaf then
    Result := (aInfo.PageSize - SizeOf(TLeafPageBaseHdr))
      div (SizeOf(TLeafPageItemHdr) + aInfo.MaxKeySize)
  else
  begin
    Result := (aInfo.PageSize - SizeOf(TInternalPageBaseHdr))
      div (SizeOf(TInternalPageItemHdr) + aInfo.MaxKeySize);
    dec(Result);
  end;
end;

function TBPlusTree.Cleanup: TBPlusTreeStatus;
var
  tmp: TBPlusTree;
  TmpDbFileName: string;
  TmpMaxKeySize: integer;
  TmpPageSize: integer;
  TmpPagesInCache: integer;
  TmpReadOnly: boolean;
var
  Cur: IBPlusTreeCursor;
  ReopenFilename: string;
begin
  // Storage must be opened to perform cleanup
  if FStorage.FileName = '' then
  begin
    exit(BP_ERROR);
  end;

  ReopenFilename := FStorage.FileName;

  // Make unique tmp file name.
  TmpDbFileName := FStorage.FileName;
  repeat
    TmpDbFileName := TmpDbFileName + '.tmp';
  until not FileExists(TmpDbFileName);

  TmpMaxKeySize := FInfo.MaxKeySize;
  TmpPageSize := FInfo.PageSize;
  TmpPagesInCache := FStorage.PagesInCache;
  TmpReadOnly := FStorage.ReadOnly;

  Cur := CursorCreateFirst;
  if Cur = nil then
  begin
    exit(BP_ERROR);
  end;

  // Rewrite tmp db.
  tmp := TBPlusTree.Create;

  if tmp.CreateNew(TmpDbFileName, TmpMaxKeySize, TmpPageSize, TmpPagesInCache, False) <> BP_OK then
  begin
    exit(BP_ERROR);
  end;

  repeat
    if tmp.Put(Cur.Key, Cur.Value) <> BP_OK then
    begin
      Cur := nil;
      tmp.Free;
      exit(BP_ERROR);
    end;
  until (not Cur.Next);

  Cur := nil;
  tmp.Free;

  // Close current and delete.
  self.Close;

  // Delete current.
  TFile.Delete(ReopenFilename);

  // Rename tmp -> current
  TFile.Move(TmpDbFileName, ReopenFilename);

  // Reopen.
  Result := OpenExisting(ReopenFilename, TmpPagesInCache, TmpReadOnly);
end;

procedure TBPlusTree.Close;
begin
  Flush;
  FreeAndNil(FStorage);

  FillChar(FInfo, SizeOf(FInfo), 0);
  FMinBranchingFactor := 0;
  FMaxBranchingFactor := 0;
end;

procedure TBPlusTree.ClosePage(var aPage: TBTreePage { TBTreePage } );
var
  StoragePage: TStoragePage;
begin
  if aPage <> nil then
  begin
    StoragePage := aPage.StoragePage;
    FStorage.EndPageAccess(StoragePage);
    // From this moment we can't be sure page is present, as it may be flushed
    // anytime; so nil aPage.
    aPage := nil;
  end;
end;

function TBPlusTree.ContainsKey(const aKey: TBytes): boolean;
var
  DummyPgNo: TPageNumber;
begin
  Result := FindLeafPageNoByKey(aKey, DummyPgNo);
end;

function TBPlusTree.InsertInternal(const aKey, aValue: TBytes;
  Overwrite: boolean): TBPlusTreeStatus;
var
  DestPage: TBTreePage;
  DestPageNo: TPageNumber;
  LeafItem: TLeafNodeItem;
  ValuePageNo, tmpPageNo: TPageNumber;
  ValuePageOfs, tmpPageOfs: TPageSize;
  ValueSize: TValueSize;
  bKeyAlreadyExists, bAppendValue: boolean;
begin
  // Check key length.
  if Length(aKey) > FInfo.MaxKeySize then
    exit(BP_KEY_IS_TOO_BIG);

  // Find and open destination page.
  DestPage := FindLeafPageByKeyOrCreateNew(aKey);
  DestPageNo := DestPage.No;

  LeafItem := TLeafNodeItem(DestPage.FindItemByKey(aKey));
  bKeyAlreadyExists := LeafItem <> nil;

  // If key exists, we'll exit.
  // Either overwrite/append value or ret BP_KEY_ALREADY_EXISTS.
  if bKeyAlreadyExists then
  begin
    if Overwrite then
    begin
      ValuePageNo := LeafItem.ChildPageNo;
      ValuePageOfs := LeafItem.PageOffset;
      ValueSize := LeafItem.ValueSize;

      // If new value is larger or shorter than old, append. If it's same
      // length, then overwrite.
      bAppendValue := Length(aValue) <> ValueSize;
      if WriteValue(aValue, ValuePageNo, ValuePageOfs, tmpPageNo, tmpPageOfs, bAppendValue) then
      begin
        // If data was not overwritten but appended, correct value record.
        if bAppendValue then
        begin
          // Update item.
          LeafItem.ChildPageNo := ValuePageNo;
          LeafItem.PageOffset := ValuePageOfs;
          LeafItem.ValueSize := Length(aValue);
          // Update trash info.
          inc(FInfo.TrashSize, ValueSize);
        end;
        Result := BP_OK;
      end
      else
        Result := BP_KEY_WRITE_ERROR;
    end
    else { if (not Overwrite) then }
      Result := BP_KEY_ALREADY_EXISTS;

    ClosePage(DestPage);
    exit;
  end;

  ClosePage(DestPage);

  // Put value (and get value page and ofs).
  if not StoreValue(aValue, ValuePageNo, ValuePageOfs) then
    raise Exception.Create('Failed to store value.');

  // Put index.
  LeafItem := TLeafNodeItem.Create(aKey, ValuePageNo, Length(aValue), ValuePageOfs);
  InsertIndexItem(DestPageNo, LeafItem, BAD_PAGE_NUMBER, nil);

  // Inc keys.
  if not bKeyAlreadyExists then
    inc(FInfo.KeyCount);

  Result := BP_OK;
end;

function TBPlusTree.Put(const aKey, aValue: TBytes): TBPlusTreeStatus;
begin
  Result := InsertInternal(aKey, aValue, True);
end;

{$IFDEF BPDEBUG}


class function TBPlusTree.PrintKey(const A: TBytes): AnsiString;
begin
  if Assigned(KeyToStr) then
    Result := KeyToStr(A)
  else
    Result := '';
end;
{$ENDIF}


function TBPlusTree.IsPageHalfFull(aPgNo: TPageNumber; out aIsHalfFull: boolean): boolean;
var
  Page: TBTreePage;
begin
  Result := False;
  if aPgNo <> BAD_PAGE_NUMBER then
  begin
    Page := GetPage(aPgNo, LOCK_READ);
    aIsHalfFull := IsPageHalfFull(Page);
    ClosePage(Page);
    exit(True);
  end;
end;

function TBPlusTree.IsPageHalfFull(aPage: TBTreePage): boolean;
var
  HdrSize, ItemSpace, UsedSpace: TPageSize;
begin
  HdrSize := TBTreePage.CalcHeaderSize(aPage.IsLeaf);
  ItemSpace := FInfo.PageSize - HdrSize;
  UsedSpace := aPage.FcbUsed - HdrSize;
  // Reserve space for inf.ptr.
  if not aPage.IsLeaf then
    inc(UsedSpace, FInfo.MaxKeySize);
  Result := UsedSpace >= (ItemSpace div 2);
end;

function TBPlusTree.IsPageOverflow(aPage: TBTreePage): boolean;
var
  Limit: TPageSize;
begin
  Limit := FInfo.PageSize;
  // Reserve space for inf.ptr.
  if not aPage.IsLeaf then
    dec(Limit, FInfo.MaxKeySize);
  Result := aPage.FcbUsed > Limit;
end;

function TBPlusTree.Delete(const aKey: TBytes): TBPlusTreeStatus;
var
  FoundPgNo: TPageNumber;
begin
  if FindLeafPageNoByKey(aKey, FoundPgNo) then
    if FoundPgNo <> BAD_PAGE_NUMBER then
      exit(DeleteInternal(FoundPgNo, aKey, 0));
  exit(BP_KEY_NOT_FOUND);
end;

procedure CorrectParentsChildPointer(
  self: TBPlusTree;
  aPgNo: TPageNumber);
var
  Page, ParentPage: TBTreePage;
  Prv, Cur, Nxt, Dst: TNodeItemTree.TRBNodePtr;
  TmpKey, NewKey: TBytes;
  tmpPgNo: TPageNumber;
begin
  // Open current page.
  Page := self.GetPage(aPgNo, LOCK_READ);
  Expect(Page.ParentPgNo <> BAD_PAGE_NUMBER);
  // 1st key of src page.
  Expect(Page.GetItemCount <> 0);
  TmpKey := Page.GetFirstItem.K.Key;
  // Open parent.
  ParentPage := self.GetPage(Page.ParentPgNo, LOCK_WRITE);
  // Find pointer.
  ParentPage.FindItemPtrByKeyEx(TmpKey, @Prv, @Cur, @Nxt);
  if Nxt <> nil then
  begin
    Dst := Nxt; // It's the item we need to modify.
    // Try get next of this item.
    if ParentPage.GetItems.Next(Nxt) then
      tmpPgNo := Nxt.K.ChildPageNo
    else
    begin
      tmpPgNo := TInternalBTreePage(ParentPage).InfinityPointerPageNo;
      Expect(tmpPgNo <> BAD_PAGE_NUMBER);
    end;
    NewKey := self.GetFirstKeyOfPage(tmpPgNo);
{$IFDEF BPDEBUG}
    if IsConsole then
      writeln(Format('Item at page %d corrected: %s->%s', [
        ParentPage.No, self.PrintKey(Dst^.K.Key), self.PrintKey(NewKey)]));
{$ENDIF}
    Dst^.K.Key := NewKey; // Replace pointer.
  end;
  // Close pages.
  self.ClosePage(ParentPage);
  self.ClosePage(Page);
end;

function TBPlusTree.DeleteInternal(aPgNo: TPageNumber;
  const aKey: TBytes; RecursionLevel: integer): TBPlusTreeStatus;
var
  Page, DonorPage: TBTreePage;
  pItem: TNodeItemTree.TRBNodePtr;
  PgNoLeft, PgNoRight, PgNoDonor, CurPgNo, PgNoParent: TPageNumber;
  bDonorHalfFull: boolean;
  KeyToDeleteInParent: TBytes;
  Left, Right, Merged: TBTreePage;
  bHaveToBecomeNewRoot: boolean;
  Key1, Key2: TBytes;
  Prv, Cur, Nxt: TNodeItemTree.TRBNodePtr;
  TmpBytes: TBytes;
  bNeedRedist: boolean; // true: redist; false: merge
{$IFDEF BPDEBUG}
  sIdent: string;
{$ENDIF}
begin
  Result := BP_ERROR;

{$IFDEF BPDEBUG}
  if IsConsole then
  begin
    sIdent := string.Create('.', RecursionLevel);
    writeln(sIdent, 'DeleteInternal(' + PrintKey(aKey) + ') from page ', aPgNo);
  end;
{$ENDIF}
  KeyToDeleteInParent := nil;

  Page := GetPage(aPgNo, LOCK_WRITE);
  CurPgNo := Page.No;
  PgNoParent := Page.ParentPgNo;

  Key1 := CopyBytes(Page.GetFirstItem.K.Key);

  // Find item with key and delete its item.
  if RecursionLevel = 0 then
  begin
    pItem := Page.FindItemPtrByKey(aKey);
    Page.DelItem(pItem, True);
  end
  else
  begin
    Page.FindEx(aKey, Prv, Cur, Nxt);

    if Nxt <> nil then
    begin
{$IFDEF BPDEBUG}
      if IsConsole then
        writeln(sIdent, 'ActualKey=', PrintKey(ActualKey));
{$ENDIF}
      TmpBytes := CopyBytes(Nxt^.K.Key);
      // Del and correct item at left.
      Page.DelItem(Nxt, True);
      if Cur = nil then
        Cur := Prv;
      Cur^.K.Key := TmpBytes;
    end
    else
    begin
      // inf.ptr.branch.
      TInternalBTreePage(Page).DelInfPtrBranch;
    end;
  end;

  //
  // Deletion from root page is always OK.
  //

  if (Page.No = Info.RootPageNo) then
  begin
    if Page.GetItemCount = 0 then
    begin
      // Delete root.
      FInfo.RootPageNo := BAD_PAGE_NUMBER;
    end;
    ClosePage(Page);
    exit(BP_OK); // done
  end;

  //
  // Deletion from non-root page.
  //

  // If page >= half after deletion it's fine.
  if IsPageHalfFull(Page) then
  begin
    ClosePage(Page);
    exit(BP_OK); // simple case, done.
  end;

  ClosePage(Page);

  //
  // Page is less then half-full.
  //

  PgNoDonor := BAD_PAGE_NUMBER;
  bDonorHalfFull := False;

  // Find siblings.
  GetSiblingPageNumbers(aPgNo, @PgNoLeft, @PgNoRight);

  // Choose donor page. Half-full page prefferred (if any).
  if (IsPageHalfFull(PgNoLeft, bDonorHalfFull)) then
    PgNoDonor := PgNoLeft;
  // If found, and found page isn't half-full try right page.
  if (PgNoDonor = BAD_PAGE_NUMBER) or (not bDonorHalfFull) then
    if (IsPageHalfFull(PgNoRight, bDonorHalfFull)) then
      PgNoDonor := PgNoRight;

{$IFDEF DEBUG}
  if PgNoDonor = BAD_PAGE_NUMBER then
    raise Exception.Create('No donor page.'); // normally shouldn't happen.
  if PgNoDonor = CurPgNo then
    raise Exception.Create('Donor page number is invalid.');
{$ENDIF}
  // Now we have donorPgNo. It's either half-full or not.

  // Order pages.
  PgNoLeft := PgNoDonor;
  PgNoRight := CurPgNo;
  // if get left,right pages before.
  OrderPagesByKey(PgNoLeft, PgNoRight);

  // Get pages.
  Left := GetPage(PgNoLeft, LOCK_READ);
  Right := GetPage(PgNoRight, LOCK_READ);

  // Get donor page.
  if PgNoLeft = PgNoDonor then
    DonorPage := Left
  else
    DonorPage := Right;

  // Order Key1, Key2.
  if PgNoLeft = CurPgNo then
  begin
    Key2 := Right.GetFirstItem^.K.Key;
    // Key1 := Key1;
  end
  else
  begin
    Key2 := Key1;
    Key1 := Left.GetFirstItem^.K.Key;
  end;

  bHaveToBecomeNewRoot := False;
  bNeedRedist := IsPageHalfFull(DonorPage);

  if bNeedRedist then
  begin
    // Redistribute, 1st key of right can change.
    RedistributeItemsEvenlyInOrderedPages(Left, Right);
    Result := BP_OK;
  end
  else // merge
  begin
    // Check if merge will lead to new root.
    if (PgNoParent = FInfo.RootPageNo) then
    begin
      Page := GetPage(PgNoParent, LOCK_READ);
      // If there are only 2 branches, it's going to be new root.
      Expect(TInternalBTreePage(Page).InfinityPointerPageNo <> BAD_PAGE_NUMBER);
      bHaveToBecomeNewRoot := Page.GetItemCount = 1; // +1 inf.ptr.
      ClosePage(Page);
    end;

{$IFDEF DEBUG}
    if Left.No = Right.No then
      raise Exception.Create('Wrong page numbers.');
{$ENDIF}
    MergeOrderedSiblingPages(Left, Right);
    Merged := Left;

    // Make root if needed.
    if bHaveToBecomeNewRoot then
    begin
      // 2 items were merged. Make merged page a root.
      MakePageARoot(Merged);
      // lose height
      dec(FInfo.TreeHeight);
      KeyToDeleteInParent := nil;
      // Close pages and exit.
      ClosePage(Left);
      ClosePage(Right);
      exit(BP_OK);
    end;

    if Merged.No = PgNoRight then
      KeyToDeleteInParent := Key1
    else
      KeyToDeleteInParent := Key2;
  end;

  // All pages must be closed before deleting (if need) key in parent.
  ClosePage(Left);
  ClosePage(Right);

{$IFDEF BPDEBUG}
  if Assigned(DbgCallback) then
    DbgCallback();
{$ENDIF}
  if KeyToDeleteInParent <> nil then
  begin
    Result := DeleteInternal(PgNoParent, KeyToDeleteInParent, RecursionLevel + 1);
  end;

  CorrectParentsChildPointer(self, PgNoLeft);
end;

{$ENDREGION}
{$REGION 'TBTreePage'}
{ TBTreePage }

function TBTreePage.AddItem(Item: TNodeItem): TNodeItemTree.TRBNodePtr;
var
  cbInsertSize: integer;
begin
{$IFDEF BPDEBUG}
  if IsConsole then
    writeln('Adding "', TBPlusTree.PrintKey(Item.Key), '" to page ', self.No);
{$ENDIF}
  cbInsertSize := TBTreePage.CalcRecordSize(self.IsLeaf, Length(Item.Key));
  Result := self.FItems.Add(Item);
  inc(self.FcbUsed, cbInsertSize);
  MakeDirty;
end;

procedure TBTreePage.DelItem(pItem: TNodeItemTree.TRBNodePtr;
  bNotify: boolean);
var
  cbInsertSize: integer;
begin
  cbInsertSize := TBTreePage.CalcRecordSize(self.IsLeaf, Length(pItem.K.Key));
  self.FItems.Delete(pItem, bNotify);
  dec(self.FcbUsed, cbInsertSize);
  MakeDirty;
end;

function TBTreePage.IsUsedLessThan(aValue: TPageSize): boolean;
begin
  Result := FcbUsed < aValue;
end;

procedure TBTreePage.MakeDirty;
begin
  if StoragePage <> nil then
    StoragePage.IsDirty := True;
end;

procedure TBTreePage.SetParentPgNo(const Value: TPageNumber);
begin
  if Value <> FParentPgNo then
  begin
    FParentPgNo := Value;
    MakeDirty;
  end;
end;

class
  function TBTreePage.CalcHeaderSize(aIsLeaf: boolean): integer;
begin
  if aIsLeaf then
    Result := SizeOf(TLeafPageBaseHdr)
  else
    Result := SizeOf(TInternalPageBaseHdr);
end;

class
  function TBTreePage.CalcRecordSize(aIsLeaf: boolean;
  aKeySize: integer): integer;
begin
  if aIsLeaf then
    // leaf
    Result :=
      SizeOf(TPageNumber) + // DataPgNo
      SizeOf(TPageSize) +   // DataOfs
      SizeOf(TValueSize) +  // DataSize
      SizeOf(TKeyLength) +  // Key len
      aKeySize              // Key
  else
    // branch
    Result :=
      SizeOf(TPageNumber) + // Child page #
      SizeOf(TKeyLength) +  // Key len
      aKeySize              // Key
end;

constructor TBTreePage.Create;
begin
  inherited;
  FItems := TNodeItemTree.Create(TNodeItem.CompareKeyLess);
  FcbUsed := CalcHeaderSize(IsLeaf);
end;

destructor TBTreePage.Destroy;
begin
  self.Serialize;
  self.FItems.Free;
  inherited;
end;

{$IFDEF BPDEBUG}


function TBTreePage.IsLeafToStr: string;
begin
  if IsLeaf then
    Result := 'L'
  else
    Result := 'B';
end;

function TBTreePage.Dump: string;
var
  bIsLeaf: boolean;
  i: TNodeItem;
begin
  bIsLeaf := self.IsLeaf;
  Result := Format('%d(%s)', [StoragePage.No, IsLeafToStr]);
  for i in FItems do
  begin
    Result := Result + Format('%d', [i.ChildPageNo]);
    Result := Result + ';';
  end;
  if not bIsLeaf then
    Result := Result + Format('inf:%d', [TInternalBTreePage(self).InfinityPointerPageNo]);
end;
{$ENDIF}


procedure TBTreePage.FindEx(const aKey: TBytes; out aPrv, aCur,
  aNxt: TNodeItemTree.TRBNodePtr);
var
  tmp: TNodeItem;
begin
  tmp := TNodeItem.CreateDummy(aKey);
  try
    FItems.FindEx(tmp, aPrv, aCur, aNxt);
  finally
    tmp.Free;
  end;
end;

function TBTreePage.FindItemByKey(const aKey: TBytes): TNodeItem;
var
  tmp: TNodeItem;
  Ptr: TNodeItemTree.TRBNodePtr;
begin
  tmp := TNodeItem.CreateDummy(aKey);
  try
    Ptr := FItems.Find(tmp);
    if Ptr <> nil then
      Result := Ptr.K
    else
      Result := nil;
  finally
    tmp.Free;
  end;
end;

function TBTreePage.FindItemPtrByKey(
  const aKey: TBytes): TNodeItemTree.TRBNodePtr;
var
  tmp: TNodeItem;
begin
  tmp := TNodeItem.CreateDummy(aKey);
  try
    Result := FItems.Find(tmp);
  finally
    tmp.Free;
  end;
end;

procedure TBTreePage.FindItemPtrByKeyEx(const aKey: TBytes;
  Prv, Cur, Nxt: TNodeItemTree.PRBNodePtr);
var
  tmp: TNodeItem;
begin
  tmp := TNodeItem.CreateDummy(aKey);
  try
    FItems.FindEx(tmp, Prv, Cur, Nxt);
  finally
    tmp.Free;
  end;
end;

procedure TBTreePage.FindLesserItem(const aKey: TBytes;
  out aItem: TNodeItem);
var
  i: TNodeItemTree.TRBNodePtr;
begin
  aItem := nil;
  i := FItems.Root;
  while i <> nil do
  begin
    if CompareBytes(aKey, i^.K.Key) < 0 then
    begin
      aItem := i^.K;
      i := i^.Left;
    end
    else
      i := i^.Right;
  end;
end;

function TBTreePage.GetFirstItem: TNodeItemTree.TRBNodePtr;
begin
  Result := self.FItems.First;
end;

function TBTreePage.GetItemCount: integer;
begin
  Result := self.FItems.Count;
end;

function TBTreePage.GetItems: TNodeItemTree;
begin
  Result := self.FItems;
end;

function TBTreePage.GetLastItem: TNodeItemTree.TRBNodePtr;
begin
  Result := self.FItems.Last;
end;

function TBTreePage.GetStoragePageNumber: TPageNumber;
begin
  Result := StoragePage.No;
end;

function TBTreePage.IsLeaf: boolean;
begin
  Result := (self is TLeafBTreePage);
end;

class function TBTreePage.IsStoragePageLeaf(StoragePage: TStoragePage): boolean;
var
  Hdr: PInternalPageBaseHdr;
begin
  Hdr := @StoragePage.Data[0];
  Result := (Hdr.Flags and PAGEFLAG_LEAF) <> 0;
end;

class function TBTreePage.IsStoragePageValid(StoragePage: TStoragePage): boolean;
var
  Hdr: PInternalPageBaseHdr;
begin
  Hdr := @StoragePage.Data[0];
  Result := (Hdr.Flags and PAGEFLAG_VALID) <> 0;
end;
{$ENDREGION 'TBTreePage'}
{$REGION 'TNodeItem'}
{ TNodeItem }

class function TNodeItem.CompareKeyLess(const A, B: TNodeItem): boolean;
begin
  Result := CompareBytes(A.Key, B.Key) < 0;
end;

constructor TNodeItem.Create(const aKey: TBytes;
  aChildPageNo: TPageNumber);
begin
  inherited Create;
  self.Key := aKey;
  self.ChildPageNo := aChildPageNo;
end;

constructor TNodeItem.CreateDummy(const aKey: TBytes);
begin
  self.Key := aKey;
end;
{$ENDREGION 'TNodeItem'}
{$REGION 'TInternalBTreePage'}
{ TInternalBTreePage }

procedure TInternalBTreePage.DelInfPtrBranch;
var
  Last: TNodeItemTree.TRBNodePtr;
  LastChildPg: TPageNumber;
begin
  Expect(self.GetItemCount > 0);
  if GetItemCount > 1 then
  begin
    Last := GetLastItem;
    LastChildPg := Last^.K.ChildPageNo;
    DelItem(Last, True);
    InfinityPointerPageNo := LastChildPg;
  end
  else
    InfinityPointerPageNo := BAD_PAGE_NUMBER;
end;

procedure TInternalBTreePage.Deserialize;
var
  bhdr: TInternalPageBaseHdr;
  ihdr: TInternalPageItemHdr;
  Ofs: TStoragePageSize;
  i: integer;
  Item: TInternalNodeItem;
  Key: TBytes;
begin
  StoragePage.Read(0, SizeOf(bhdr), @bhdr);

  self.ParentPgNo := bhdr.ParentPgNo;
  self.InfinityPointerPageNo := bhdr.PgNoInfinity;

  if bhdr.PgNoInfinity = BAD_PAGE_NUMBER then
    raise Exception.Create('wrong inf. page');

  if bhdr.KeyCount <> 0 then
  begin
    Ofs := SizeOf(bhdr);
    for i := 0 to bhdr.KeyCount - 1 do
    begin
      // leaf item header
      StoragePage.Read(Ofs, SizeOf(ihdr), @ihdr);
      inc(Ofs, SizeOf(ihdr));
      // leaf key
      SetLength(Key, ihdr.KeyLen);
      self.StoragePage.Read(Ofs, ihdr.KeyLen, @Key[0]);
      inc(Ofs, ihdr.KeyLen);

      Item := TInternalNodeItem.Create(Key, ihdr.PgNoLess);
      self.AddItem(Item);
    end;
  end;
end;

procedure TInternalBTreePage.Serialize;
var
  Ofs: TStoragePageSize;
  bhdr: TInternalPageBaseHdr;
  ihdr: TInternalPageItemHdr;
  Item: TNodeItemTree.TRBNodePtr;
  elem: TInternalNodeItem;
begin
  Ofs := 0;

  bhdr.Flags := PAGEFLAG_VALID { no PAGEFLAG_LEAF };
  bhdr.KeyCount := self.GetItemCount;
  bhdr.ParentPgNo := self.ParentPgNo;
  bhdr.PgNoInfinity := self.InfinityPointerPageNo;

  StoragePage.Write(Ofs, SizeOf(bhdr), @bhdr);
  inc(Ofs, SizeOf(bhdr));

  Item := GetFirstItem;
  while Item <> nil do
  begin
    elem := TInternalNodeItem(Item.K);

    ihdr.PgNoLess := elem.ChildPageNo;
    ihdr.KeyLen := Length(elem.Key);

    // hdr
    StoragePage.Write(Ofs, SizeOf(ihdr), @ihdr);
    inc(Ofs, SizeOf(ihdr));
    // key
    StoragePage.Write(Ofs, Length(elem.Key), @elem.Key[0]);
    inc(Ofs, Length(elem.Key));

    Item := GetItems.GetNext(Item);
  end;
end;

{$ENDREGION 'TInternalBTreePage'}
{$REGION 'TLeafBTreePage'}
{ TLeafBTreePage }

procedure TLeafBTreePage.Deserialize;
var
  bhdr: TLeafPageBaseHdr;
  ihdr: TLeafPageItemHdr;
  Ofs: TStoragePageSize;
  i: integer;
  Item: TLeafNodeItem;
  Key: TBytes;
begin
  StoragePage.Read(0, SizeOf(bhdr), @bhdr);

  self.ParentPgNo := bhdr.ParentPgNo;
  self.PrevPageNo := bhdr.PrevPgNo;
  self.NextPageNo := bhdr.NextPgNo;

  if bhdr.KeyCount <> 0 then
  begin
    Ofs := SizeOf(TLeafPageBaseHdr);
    for i := 0 to bhdr.KeyCount - 1 do
    begin
      // leaf item header
      StoragePage.Read(Ofs, SizeOf(ihdr), @ihdr);
      inc(Ofs, SizeOf(ihdr));
      // leaf key
      SetLength(Key, ihdr.KeyLen);
      self.StoragePage.Read(Ofs, ihdr.KeyLen, @Key[0]);
      inc(Ofs, ihdr.KeyLen);

      Item := TLeafNodeItem.Create(Key, ihdr.DataPgNo, ihdr.DataSize, ihdr.DataOfs);
      self.AddItem(Item);

    end;
  end;
end;

procedure TLeafBTreePage.Serialize;
var
  Ofs: TStoragePageSize;
  bhdr: TLeafPageBaseHdr;
  ihdr: TLeafPageItemHdr;
  Item: TNodeItemTree.TRBNodePtr;
  elem: TLeafNodeItem;
begin
  Ofs := 0;

  bhdr.Flags := PAGEFLAG_VALID or PAGEFLAG_LEAF;
  bhdr.KeyCount := self.GetItemCount;
  bhdr.ParentPgNo := self.ParentPgNo;
  bhdr.PrevPgNo := self.PrevPageNo;
  bhdr.NextPgNo := self.NextPageNo;

  StoragePage.Write(Ofs, SizeOf(bhdr), @bhdr);
  inc(Ofs, SizeOf(bhdr));

  Item := GetFirstItem;
  while Item <> nil do
  begin
    elem := TLeafNodeItem(Item.K);

    ihdr.DataPgNo := elem.ChildPageNo;
    ihdr.DataOfs := elem.PageOffset;
    ihdr.DataSize := elem.ValueSize;
    ihdr.KeyLen := Length(elem.Key);

    // hdr
    StoragePage.Write(Ofs, SizeOf(ihdr), @ihdr);
    inc(Ofs, SizeOf(ihdr));
    // key
    StoragePage.Write(Ofs, Length(elem.Key), @elem.Key[0]);
    inc(Ofs, Length(elem.Key));

    Item := GetItems.GetNext(Item);
  end;

end;
{$ENDREGION 'TLeafBTreePage'}
{$REGION 'TLeafNodeItem'}
{ TLeafNodeItem }

constructor TLeafNodeItem.Create(const aKey: TBytes;
  aChildPageNo: TPageNumber;
  aDataSize: TValueSize;
  aPageOffset: TPageSize);
begin
  inherited Create(aKey, aChildPageNo);
  self.ValueSize := aDataSize;
  self.PageOffset := aPageOffset;
end;
{$ENDREGION 'TLeafNodeItem'}
{$REGION 'TBPlusTreeFileStorage'}
{ TBPlusTreeFileStorage }

procedure TBPlusTreeFileStorage.PageCollectionNotify(Sender: TObject;
  const Item: TStoragePage; Action: TCollectionNotification);
begin
  // First free custom data.
  if Action = cnRemoved then
  begin
    TBTreePage(Item.CustomData).Free;
  end;
  // Then all other.
  inherited;
end;
{$ENDREGION 'TBPlusTreeFileStorage'}
{$REGION 'TBPCursor'}
{ TBPCursor }

destructor TBPlusTreeCursor.Destroy;
begin
  FTree.ClosePage(FItemDesc.LeafPage);
  FRangePrefix := nil;
  inherited;
end;

function TBPlusTreeCursor.GetKey: TBytes;
begin
  Result := FItemDesc.Item^.K.Key;
end;

function TBPlusTreeCursor.GetValue: TBytes;
var
  Item: TLeafNodeItem;
begin
  Item := TLeafNodeItem(FItemDesc.Item^.K);
  if not FTree.ReadValue(Item.ChildPageNo, Item.PageOffset, Item.ValueSize, Result) then
    Result := nil;
end;

function TBPlusTreeCursor.IsFirstAtPage: boolean;
begin
  Result := FItemDesc.Item = FItemDesc.LeafPage.GetFirstItem;
end;

function TBPlusTreeCursor.IsLastAtPage: boolean;
begin
  Result := FItemDesc.Item = FItemDesc.LeafPage.GetLastItem;
end;

function TBPlusTreeCursor.Next: boolean;
begin
  Result := FTree.CursorMove(self, 1);
end;

function TBPlusTreeCursor.Prev: boolean;
begin
  Result := FTree.CursorMove(self, -1);
end;

function TBPlusTreeCursor.NextPage: boolean;
begin
  Result := FTree.CursorMoveToPageStart(self, 1);
end;

function TBPlusTreeCursor.PrevPage: boolean;
begin
  Result := FTree.CursorMoveToPageStart(self, -1);
end;

{$ENDREGION}

end.
