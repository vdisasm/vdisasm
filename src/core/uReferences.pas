{
  Ref. layout:

  key:   Tag,RelVA,RelVAFrom;
  value: RefKind
}
unit uReferences;

interface

uses
  uDb,
  uUpdateable,
  VDAPI;

type
  TVDVAReferences = class(TVDUpdateable, IVDVAReferences)
  protected
    function PutOrDel(VA: TVA; FromVA: TVA; Kind: TVDReferenceKind; Put: boolean): boolean;
    procedure SafeNotifyChanged;
  public
    function Put(VA: TVA; FromVA: TVA; Kind: TVDReferenceKind): BOOL; stdcall;
    function Del(VA: TVA; FromVA: TVA): BOOL; stdcall;
    function DelAll(VA: TVA): BOOL; stdcall;
    function Enumerate(VA: TVA; cb: TRefEnumFunc; ud: pointer): BOOL; stdcall;
    function HasReferences(VA: TVA): BOOL; stdcall;
  end;

function RefKindToStr(rk: TVDReferenceKind): string; inline;

implementation

uses
  System.Generics.Collections,
  System.SysUtils,
  uCore,
  uStream, // expand
  uStream.MemoryStream,
  BPlusTree.Intf;

type
  TVAList = TList<TVA>;

function RefKindToStr(rk: TVDReferenceKind): string; inline;
begin
  case rk of
    REFKIND_JUMP:
      result := 'j';
    REFKIND_CALL:
      result := 'c';
    REFKIND_FALLTHROUGH:
      result := 'f';
    REFKIND_READORWRITE:
      result := 'r|w';
    REFKIND_READ:
      result := 'r';
    REFKIND_WRITE:
      result := 'w';
  else
    result := '?';
  end;
end;

function ConstructKey(
  const c: IVDCore;
  VA: TVA;
  FromVA: PVA; // optionanl
  out m: IVDStreamIO): boolean;
var
  relVA, relVAfrom: TRelVA;
begin
  m := nil;

  if not c.VM.AbsToRelVA(VA, relVA) then
    exit(false);

  if assigned(FromVA) then
    if not c.VM.AbsToRelVA(FromVA^, relVAfrom) then
      exit(false);

  m := TVDStreamIO.Create(TVDMemoryStream.Create());
  m.WriteU8(DBTAG_RefFrom);
  m.WriteRelVA(relVA);
  if assigned(FromVA) then
    m.WriteRelVA(relVAfrom);
  exit(true);
end;

function ParseKey(const c: IVDCore; var b: TBytes; out FromVA: TVA): boolean;
var
  m: IVDStreamIO;
  relVAfrom: TRelVA;
begin
  if length(b) < (sizeof(TDBTag) + sizeof(TRelVA) + sizeof(TRelVA)) then
    exit(false);

  m := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  m.Skip(sizeof(TDBTag) + sizeof(TRelVA));
  m.ReadRelVA(relVAfrom);
  result := c.VM.RelToAbsVA(relVAfrom, FromVA);
end;

function ParseValue(const c: IVDCore; var b: TBytes; out Kind: TVDReferenceKind): boolean;
var
  m: IVDStreamIO;
begin
  Kind := REFKIND_UNKNOWN;

  if length(b) < (1) then
    exit(true); // but w/o kind

  m := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  Kind := TVDReferenceKind(m.ReadU8);
  result := true;
end;

{ TVDVAReferences }

function DelAll_enumFunc(FromVA: TVA; Kind: TVDReferenceKind; ud: pointer): BOOL; stdcall;
begin
  TVAList(ud).Add(FromVA);
  result := true;
end;

function TVDVAReferences.DelAll(VA: TVA): BOOL;
var
  list: TVAList;
  tmpVA: TVA;
begin
  list := TVAList.Create;
  try
    // list refs
    Enumerate(VA, DelAll_enumFunc, list);
    // del
    result := true;
    for tmpVA in list do
      if not Del(VA, tmpVA) then
        result := false;

    if result then
      SafeNotifyChanged;
  finally
    list.Free;
  end;
end;

function TVDVAReferences.Enumerate(VA: TVA; cb: TRefEnumFunc; ud: pointer): BOOL;
var
  c: IVDCore;
  k: IVDStreamIO;
  b: TBytes;
  cur: IBPlusTreeCursor;
  FromVA: TVA;
  Kind: TVDReferenceKind;
begin
  c := CoreGet;
  if not ConstructKey(c, VA, nil, k) then
    exit(false);

  cur := (c as TVDCore).DB.CursorCreateEx((k as TVDStreamIO).ToBytes, [kpGreater], true);
  if not assigned(cur) then
    exit(false); // no refs
  // at least 1 ref present
  if not assigned(cb) then
    exit(true); // means there are refs, but don't traverse them
  repeat
    b := cur.Key;
    if ParseKey(c, b, FromVA) then
    begin
      b := cur.Value;
      if ParseValue(c, b, Kind) then
      begin
        if not cb(FromVA, Kind, ud) then
          break;
      end;
    end;
  until not cur.Next;
  exit(true);
end;

function TVDVAReferences.HasReferences(VA: TVA): BOOL;
begin
  result := Enumerate(VA, nil, nil);
end;

function TVDVAReferences.PutOrDel(VA, FromVA: TVA; Kind: TVDReferenceKind; Put: boolean): boolean;
var
  c: TVDCore;
  k, v: IVDStreamIO;
begin
  c := TVDCore(CoreGet);
  if not ConstructKey(c, VA, @FromVA, k) then
    exit(false);

  if not Put then // Del
  begin
    result := c.DB.Delete((k as TVDStreamIO).ToBytes) = BP_OK;
    exit;
  end;

  // Put
  v := TVDStreamIO.Create(TVDMemoryStream.Create());
  v.WriteU8(Byte(Kind));
  result := c.DB.Put((k as TVDStreamIO).ToBytes, (v as TVDStreamIO).ToBytes) = BP_OK;
end;

procedure TVDVAReferences.SafeNotifyChanged;
begin
  if FUpdateCount = 0 then
    CoreGet.Msg.Broadcast(MSG_REFS_CHANGED);
end;

function TVDVAReferences.Put(VA, FromVA: TVA; Kind: TVDReferenceKind): BOOL;
begin
  result := PutOrDel(VA, FromVA, Kind, true);
  if result then
    SafeNotifyChanged;
end;

function TVDVAReferences.Del(VA, FromVA: TVA): BOOL;
begin
  result := PutOrDel(VA, FromVA, REFKIND_UNKNOWN, false);
  if result then
    SafeNotifyChanged;
end;

end.
