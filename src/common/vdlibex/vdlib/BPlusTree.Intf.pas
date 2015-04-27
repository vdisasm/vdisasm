{
  *   B+ Tree Database Interface
  *
  *   vdisasm.com
  *   code.google.com/p/delphi-bpus-tree
  *
  *   Most useful functions declared here. If you need low-level functions,
  *   use TBPlusTree class directly. Anyway it's recommended to use this
  *   interface.
}

unit BPlusTree.Intf;

interface

uses
  System.SysUtils;

type
  TPageSize = type uint16;  // Size of storage page.
  TValueSize = type uint32; // Size of value pointed by key.

  TBPlusTreeStatus =
    (
    BP_OK,
    BP_ERROR,
    BP_FILE_NOT_FOUND,
    BP_DB_VERSION_DIFFERS,
    BP_PAGE_IS_TOO_SMALL,
    BP_FANOUT_IS_TOO_SMALL,
    BP_KEY_IS_TOO_BIG,
    BP_KEY_NOT_FOUND,
    BP_KEY_ALREADY_EXISTS,
    BP_KEY_WRITE_ERROR,
    BP_VALUE_READ_ERROR
    );

  TKeyPosition =
    (
    kpFirst, kpLast,           // group 1 (don't combine with group 2)
    kpLess, kpEqual, kpGreater // group 2 (don't combine with group 1)
    );

  TKeyPositions = set of TKeyPosition;

  // ---------------------------------------------------------------------------
  // Cursor
  // ---------------------------------------------------------------------------

  IBPlusTreeCursor = interface
    function GetKey: TBytes;
    function GetValue: TBytes;

    // Move to Next/Previous key.
    // Result is True on success.
    function Next: Boolean;
    function Prev: Boolean;

    // Move to first key of Next/Previous page.
    // Result is True on success.
    function NextPage: Boolean;
    function PrevPage: Boolean;

    property Key: TBytes read GetKey;
    property Value: TBytes read GetValue;
  end;

  // ---------------------------------------------------------------------------
  // Tree
  // ---------------------------------------------------------------------------

  IBPlusTree = interface

    // Create new database.
    function CreateNew(
      const aFileName: string;   // database file name
      aMaxKeySize: integer;      // max key length
      aPageSize: TPageSize;      // size of storage page
      aPagesInCache: integer;    // max number of in-memory pages in cache
      aReadOnly: Boolean = False // is db read-only?
      ): TBPlusTreeStatus;

    // Open existing database.
    function OpenExisting(
      const aFileName: string;   // database file name
      aPagesInCache: integer;    // max number of in-memory pages in cache
      aReadOnly: Boolean = False // is db read-only?
      ): TBPlusTreeStatus;

    // Open existing database or create new.
    function OpenAlways(
      const aFileName: string;   // database file name
      aMaxKeySize: integer;      // max key length
      aPageSize: TPageSize;      // size of storage page
      aPagesInCache: integer;    // max number of in-memory pages in cache
      aReadOnly: Boolean = False // is db read-only?
      ): TBPlusTreeStatus;

    // Check if key exists.
    function ContainsKey(const aKey: TBytes): Boolean;

    // Put Key-Value pair.
    // If Key already exists, value is replaced.
    function Put(const aKey, aValue: TBytes): TBPlusTreeStatus;

    // Insert Key-Value pair from buffer.
    // If Key already exists, value is replaced.
    function PutRaw(
      aKey: pointer; aKeySize: integer;
      aValue: pointer; aValueSize: integer
      ): TBPlusTreeStatus;

    // Delete Value by Key.
    function Delete(const aKey: TBytes): TBPlusTreeStatus;
    function DeleteRaw(aKey: pointer; aKeySize: integer): TBPlusTreeStatus;

    // Save changes.
    procedure Flush;

    // Cleanup garbage.
    function Cleanup: TBPlusTreeStatus;

    // Get size of value associtated with aKey.
    // If result status is BP_OK, aSize is set to value size
    // (it can be 0 if there's no value)
    function GetValueSize(
      const aKey: TBytes;
      out aSize: TValueSize): TBPlusTreeStatus;

    // Retrieve value by key.
    function Get(
      const aKey: TBytes;
      out aValue: TBytes): TBPlusTreeStatus;

    // Retrieve value by key and copy to aValue buffer.
    // Result is length of value.
    // If it's greater than aValueSize then only aValueSize is copied.
    function GetRaw(
      aKey: pointer; aKeySize: integer;
      aValue: pointer; aValueSize: integer): integer;

    // Get either first or last key.
    function GetExtremeKey(aFirst: Boolean; out aKey: TBytes): Boolean;

    // Get first key.
    function FirstKey(out aKey: TBytes): Boolean;

    // Get last key.
    function LastKey(out aKey: TBytes): Boolean;

    // Create cursor pointing to aKey key.
    // If key was not found result is nil.
    function CursorCreate(const aKey: TBytes): IBPlusTreeCursor;

    // Create cursor pointing to aKey key. Result can be Nil.
    // aKeyPos defines what key should be taken as cursor:
    // kpLess, kpEqual, kpGreater (than aKey)
    // If aPrefix is True, aKey will be used as a prefix (false by default).
    function CursorCreateEx(
      const aKey: TBytes;
      aKeyPos: TKeyPositions = [kpEqual];
      aPrefix: Boolean = False): IBPlusTreeCursor;

    // Create cursor pointing to first key.
    function CursorCreateFirst: IBPlusTreeCursor;

    // Create cursor pointing to last key.
    function CursorCreateLast: IBPlusTreeCursor;

    function GetKeyCount: uint32;

    { Properties }

    property KeyCount: uint32 read GetKeyCount;
  end;

implementation

end.
