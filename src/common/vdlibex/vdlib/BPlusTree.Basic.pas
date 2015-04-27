unit BPlusTree.Basic;

interface

uses
  System.SysUtils,
  BPlusTree.Intf;

type
  TBPlusTreeBasic = class(TInterfacedObject)
  public
    function Put(const aKey, aValue: TBytes): TBPlusTreeStatus; virtual; abstract;

    function PutRaw(
      aKey: pointer;
      aKeySize: integer;
      aValue: pointer;
      aValueSize: integer
      ): TBPlusTreeStatus;

    // Delete Value by Key.
    function Delete(const aKey: TBytes): TBPlusTreeStatus; virtual; abstract;
    function DeleteRaw(aKey: pointer; aKeySize: integer): TBPlusTreeStatus;

    // Retrieve value by key.
    function Get(const aKey: TBytes; out aValue: TBytes): TBPlusTreeStatus; virtual; abstract;

    // Retrieve value by key.
    // Result is length of value. If it's greater than aValueSize then only
    // aValueSize is copied.
    function GetRaw(
      { in } aKey: pointer; aKeySize: integer;
      { out } aValue: pointer; aValueSize: integer
      ): integer;

    // Get either first or last key.
    function GetExtremeKey(aFirst: boolean; out aKey: TBytes): boolean; virtual; abstract;
    function FirstKey(out aKey: TBytes): boolean; inline;
    function LastKey(out aKey: TBytes): boolean; inline;

    // Create cursor pointing to aKey key. Result can be Nil.
    // aKeyPos defines what key should be taken as cursor:
    // kpLess, kpEqual, kpGreater (than aKey)
    // If aPrefix is True, aKey will be used as a prefix (false by default).
    function CursorCreateEx(
      const aKey: TBytes;
      aKeyPos: TKeyPositions = [kpEqual];
      aPrefix: boolean = False): IBPlusTreeCursor; virtual; abstract;

    // Create cursor pointing to aKey key.
    function CursorCreate(const aKey: TBytes): IBPlusTreeCursor;
  end;

function Min(A, B: integer): integer; inline;
function Max(A, B: integer): integer; inline;

// Check if Value starts with prefix.
function ValueStartsWithPrefix(const Value, Prefix: TBytes): boolean;

function CopyBytes(const A: TBytes): TBytes; inline;

  // Todo: BufToBytes should be in some special unit for helpers.
procedure BufToBytes(Buf: PByte; Size: integer; out Bytes: TBytes); inline;

implementation

function Min(A, B: integer): integer; inline;
begin
  if A < B then
    exit(A);
  exit(B);
end;

function Max(A, B: integer): integer; inline;
begin
  if A > B then
    exit(A);
  exit(B);
end;

function CopyBytes(const A: TBytes): TBytes;
begin
  Result := Copy(A, 0, Length(A));
end;

procedure BufToBytes(Buf: PByte; Size: integer; out Bytes: TBytes);
begin
  if (Buf = nil) or (Size = 0) then
    Bytes := nil
  else
  begin
    SetLength(Bytes, Size);
    Move(Buf^, Bytes[0], Size);
  end;
end;

// Check if Value starts with prefix.
function ValueStartsWithPrefix(const Value, Prefix: TBytes): boolean;
var
  i: integer;
begin
  if (Value = nil) or (Prefix = nil) then
    exit(False);
  i := Min(Length(Value), Length(Prefix)) - 1;
  while (i >= 0) do
  begin
    if Value[i] <> Prefix[i] then
      exit(False);
    dec(i);
  end;
  exit(True);
end;


{ TBPlusTreeBasic }

function TBPlusTreeBasic.PutRaw(
  aKey: pointer;
  aKeySize: integer;
  aValue: pointer;
  aValueSize: integer
  ): TBPlusTreeStatus;
var
  Key, Value: TBytes;
begin
  BufToBytes(aKey, aKeySize, Key);
  BufToBytes(aValue, aValueSize, Value);
  Result := Self.Put(Key, Value);
end;

function TBPlusTreeBasic.DeleteRaw(aKey: pointer; aKeySize: integer): TBPlusTreeStatus;
var
  K: TBytes;
begin
  BufToBytes(aKey, aKeySize, K);
  Result := Self.Delete(K);
end;

function TBPlusTreeBasic.GetRaw(
  aKey: pointer; aKeySize: integer;
  aValue: pointer; aValueSize: integer
  ): integer;
var
  K, v: TBytes;
begin
  if (aKeySize = 0) then
    exit(0);
  BufToBytes(aKey, aKeySize, K);
  // If no key found, exit(0).
  if Self.Get(K, v) <> BP_OK then
    exit(0);
  // Result length.
  if Length(v) > aValueSize then
    Result := aValueSize
  else
    Result := Length(v);
  // Copy data.
  if (aValue <> nil) and (Result <> 0) then
    Move(v[0], aValue^, Result);
end;

function TBPlusTreeBasic.FirstKey(out aKey: TBytes): boolean;
begin
  Result := GetExtremeKey(True, aKey);
end;

function TBPlusTreeBasic.LastKey(out aKey: TBytes): boolean;
begin
  Result := GetExtremeKey(False, aKey);
end;

function TBPlusTreeBasic.CursorCreate(const aKey: TBytes): IBPlusTreeCursor;
begin
  if aKey <> nil then
    Result := CursorCreateEx(aKey, [kpEqual], False)
  else
    Result := nil;
end;

end.
