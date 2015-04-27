unit uProblems;

interface

uses
  System.SysUtils,

  uDB,
  uUpdateable,
  uStream,
  uStream.MemoryStream,

  VDAPI;

type
  TVDProblems = class(TVDUpdateable, IVDProblems)
  protected
    procedure SafeNotification;
    procedure DoEndUpdate; override;
  public
    function Put(VA: TVA; Kind: TProblemKind): BOOL; stdcall;
    function Enumerate(VA0, VA1: TVA; cb: TProblemEnumFunc; ud: Pointer): BOOL; stdcall;
    function Delete(VA: TVA; Kind: TProblemKind): BOOL; stdcall;
  end;

implementation

uses
  uCore,
  BPlusTree.Intf;

{ TVDProblems }

// -----------------------------------------------------------------------------

procedure TVDProblems.SafeNotification;
begin
  if FUpdateCount = 0 then
    CoreGet().Msg.Broadcast(MSG_PROBLEMS_CHANGED);
end;

procedure TVDProblems.DoEndUpdate;
begin
  inherited;
  SafeNotification;
end;

// -----------------------------------------------------------------------------
type
  PProblemKind = ^TProblemKind;

  // kind is optional
function ConstructKey(const c: TVDCore; VA: TVA; Kind: PProblemKind; out key: IVDStreamIO): boolean;
var
  RelVA: TRelVA;
begin
  Result := False;
  key := nil;
  if c.GetVM.AbsToRelVA(VA, RelVA) then
  begin
    key := TVDStreamIO.Create(TVDMemoryStream.Create);
    key.WriteU8(DBTAG_Problem); // tag
    key.WriteRelVA(RelVA);      // rel.va
    if Assigned(Kind) then
      key.WriteU8(Kind^); // kind
    Result := True;
  end;
end;

function ParseKey(const c: TVDCore; var key: TBytes; out VA: TVA; out Kind: TProblemKind): boolean;
var
  io: IVDStreamIO;
  RelVA: TRelVA;
begin
  Result := False;
  io := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(key));
  if io.ReadU8 <> DBTAG_Problem then
    exit;
  io.ReadRelVA(RelVA);
  if not c.GetVM.RelToAbsVA(RelVA, VA) then
    exit;
  Kind := io.ReadU8;
  Result := True;
end;

// -----------------------------------------------------------------------------
function TVDProblems.Enumerate(VA0, VA1: TVA; cb: TProblemEnumFunc;
  ud: Pointer): BOOL;
var
  c: TVDCore;
  key: IVDStreamIO;
  cur: IBPlusTreeCursor;
  VA: TVA;
  Kind: TProblemKind;
  b: TBytes;
begin
  Result := False;

  c := TVDCore(CoreGet());

  // va0
  if VA0 = BAD_VA then
    if not c.GetVM.GetFirstVA(@VA0) then
      exit;

  // va1
  if VA1 = BAD_VA then
    if not c.GetVM.GetLastVA(@VA1) then
      exit;

  if not ConstructKey(c, VA0, nil, key) then
    exit;

  // Need 1st key > than current, no prefix because VA will change, and must
  // be handled manually.
  cur := c.DB.CursorCreateEx(TVDStreamIO(key).ToBytes, [kpGreater], False);

  if not Assigned(cur) then
    exit;

  repeat
    b := cur.key;
    if not ParseKey(c, b, VA, Kind) then
      exit;
    if VA > VA1 then
      exit;

    Result := True;

    if not Assigned(cb) then
      exit; // at least 1 key present

    if not cb(VA, Kind, ud) then
      exit;
  until not cur.Next;
end;
// -----------------------------------------------------------------------------

function TVDProblems.Put(VA: TVA; Kind: TProblemKind): BOOL;
var
  c: TVDCore;
  key: IVDStreamIO;
begin
  Result := False;

  c := TVDCore(CoreGet());

  if ConstructKey(c, VA, @Kind, key) then
  begin
    Result := c.DB.Put(TVDStreamIO(key).ToBytes, nil) = BP_OK;
    if Result then
      SafeNotification;
  end;
end;

// -----------------------------------------------------------------------------

function TVDProblems.Delete(VA: TVA; Kind: TProblemKind): BOOL;
var
  c: TVDCore;
  key: IVDStreamIO;
begin
  Result := False;

  c := TVDCore(CoreGet());

  if ConstructKey(c, VA, @Kind, key) then
  begin
    Result := c.DB.Delete(TVDStreamIO(key).ToBytes) = BP_OK;
    if Result then
      SafeNotification;
  end;
end;

// -----------------------------------------------------------------------------

end.
