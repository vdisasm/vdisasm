{
  Imported Symbols (functions and variables/constants)
}

unit uSymbols.Import;

interface

uses
  gmap,
  uUpdateable,
  VDAPI;

type
  PVDImportLibUID = ^TVDImportLibUID;

  // Internal map of UID->LibName used to
  TImpMapLibUidToName = TMap<TVDImportLibUID, string>;
  TImpMapLibNameToUid = TMap<string, TVDImportLibUID>;

  // for Get function
  TImpSymGetCtx = record
    result: boolean;
    LibName, SymName: PBSTR;
    SymOrd: PVDSymbolOrdinal;
    constructor Create(LibName, SymName: PBSTR; SymOrd: PVDSymbolOrdinal);
  end;

  PImpSymGetCtx = ^TImpSymGetCtx;

  { TVDImportSymbols }

  TVDImportSymbols = class(TVDUpdateable, IVDImportSymbols)
  private
    FUidToName: TImpMapLibUidToName;
    FNameToUid: TImpMapLibNameToUid;
    function FetchLibUID(const LibPath: string; out Existed: boolean): TVDImportLibUID;
    function FetchLibName(uid: TVDImportLibUID; out Name: string): boolean;
{$IFNDEF DEBUG}inline; {$ENDIF}
    procedure CollectLibUids;
    procedure LibAddInternal(const LibPath: string; uid: TVDImportLibUID); inline;

    // Put import library record and return library UID.
    function PutImpLib(const c: IVDCore; const LibPath: string;
      out uid: TVDImportLibUID): boolean;
    // Put import symbol (internal).
    function PutImpSym(const c: IVDCore; VA: TVA; LibUID: TVDImportLibUID;
      const SymName: string; SymOrd: TVDSymbolOrdinal): boolean;

    function GetSymInternal(VA: TVA; const ctx: TImpSymGetCtx): boolean; inline;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  protected
    procedure DoEndUpdate; override;
    procedure SafeNotification;
  public
    { VDAPI }
    function Put(VA: TVA; LibName, SymbolName: BSTR_IN;
      Ordinal: TVDSymbolOrdinal): BOOL; stdcall;

    function Exists(VA: TVA): BOOL; stdcall;

    function Get(VA: TVA; out LibName: BSTR; out SymName: BSTR;
      out SymOrd: TVDSymbolOrdinal): BOOL; stdcall;

    // function Del(VA: TVA; LibName, SymbolName: BSTR_IN;
    // Ordinal: TVDSymbolOrdinal): BOOL; stdcall;

    procedure EnumLibs(cb: TVDImpLibEnumFunc; ud: pointer); stdcall;
    procedure EnumSyms(VA0, VA1: TVA; cb: TVDImpSymEnumFunc; ud: pointer); stdcall;
  end;

function EnumFunc_Get(VA: TVA; LibStr, SymStr: BSTR_IN; SymOrd: TVDSymbolOrdinal; ud: pointer): BOOL; stdcall;

implementation

uses
  System.SysUtils,

  BPlusTree.Intf,

  uCore,
  uDB,
  uStream, // expand
  uStream.MemoryStream;

// -----------------------------------------------------------------------------
// Helper functions
// -----------------------------------------------------------------------------
procedure ImpLib_MakeKey(uid: PVDImportLibUID; out b: TVDStreamIO);
{$IFNDEF DEBUG}inline; {$ENDIF}
begin
  b := TVDStreamIO.Create(TVDMemoryStream.Create());
  b.WriteTag(DBTAG_ImportedLib);
  if Assigned(uid) then
    b.WriteWord(sizeof(TVDImportLibUID), uid^);
end;

procedure ImpLib_MakeVal(const LibPath: string; out b: TVDStreamIO);
  inline;
begin
  b := TVDStreamIO.Create(TVDMemoryStream.Create());
  b.WriteStr(LibPath);
end;

function ImpLib_ParseKey(var b: TBytes; out uid: TVDImportLibUID): boolean;
var
  s: TVDStreamIO;
begin
  if length(b) < (sizeof(TDBTag) + sizeof(TVDImportLibUID)) then
    exit(false);
  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  try
    if s.ReadTag <> DBTAG_ImportedLib then
      exit(false);
    uid := s.ReadWord(sizeof(TVDImportLibUID));
    result := true;
  finally
    s.Free;
  end;
end;

function ImpLib_ParseValue(var b: TBytes; out LibPath: string): boolean;
var
  s: TVDStreamIO;
begin
  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  try
    LibPath := s.ReadStr();
    result := LibPath <> '';
  finally
    s.Free;
  end;
end;

function CollectLibUids_EnumFunc(uid: TVDImportLibUID; Lib: BSTR_IN; ud: pointer): BOOL; stdcall;
var
  self: TVDImportSymbols;
begin
  self := ud;
  self.LibAddInternal(Lib, uid);
  result := true;
end;

// -----------------------------------------------------------------------------
function ImpSym_MakeKey(const c: IVDCore; VA: TVA; out b: TVDStreamIO): boolean;
var
  RelVA: TRelVA;
begin
  b := nil;
  if not c.VM.AbsToRelVA(VA, RelVA) then
    exit(false);
  b := TVDStreamIO.Create(TVDMemoryStream.Create());
  b.WriteTag(DBTAG_ImportedSym);
  b.WriteRelVA(RelVA);
  result := true;
end;

procedure ImpSym_MakeVal(
  LibUID: TVDImportLibUID;
  SymOrd: TVDSymbolOrdinal;
  const SymName: string;
  out b: TVDStreamIO);
begin
  b := TVDStreamIO.Create(TVDMemoryStream.Create());
  b.WriteWord(sizeof(TVDImportLibUID), LibUID);
  b.WriteWord(sizeof(TVDSymbolOrdinal), SymOrd);
  b.WriteStr(SymName);
end;

function ImpSym_ParseKey(const c: IVDCore; var b: TBytes; out VA: TVA): boolean;
var
  s: TVDStreamIO;
  RelVA: TRelVA;
begin
  if length(b) < (sizeof(TDBTag) + sizeof(TRelVA)) then
    exit(false);
  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  try
    if s.ReadTag <> DBTAG_ImportedSym then
      exit(false);
    s.ReadRelVA(RelVA);
    result := c.VM.RelToAbsVA(RelVA, VA);
  finally
    s.Free;
  end;
end;

function ImpSym_ParseVal(var b: TBytes;
  out LibUID: TVDImportLibUID;
  out SymOrd: TVDSymbolOrdinal;
  out SymName: string): boolean;
var
  s: TVDStreamIO;
begin
  if length(b) < (sizeof(TVDImportLibUID) + sizeof(TVDSymbolOrdinal)) then
    exit(false);
  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  try
    LibUID := s.ReadWord(sizeof(TVDImportLibUID));
    SymOrd := s.ReadWord(sizeof(TVDSymbolOrdinal));
    SymName := s.ReadStr();
    result := true;
  finally
    s.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ TVDImportSymbols }

procedure TVDImportSymbols.Clear;
begin
  FUidToName.Clear;
  FNameToUid.Clear;
end;

procedure TVDImportSymbols.CollectLibUids;
begin
  Clear;
  EnumLibs(CollectLibUids_EnumFunc, self);
end;

constructor TVDImportSymbols.Create;
begin
  inherited;

  FUidToName := TImpMapLibUidToName.Create(
    function(const a, b: TVDImportLibUID): boolean
    begin
      result := a < b;
    end);

  FNameToUid := TImpMapLibNameToUid.Create(
    function(const a, b: string): boolean
    begin
      // todo: implib case sensitive?
      result := a < b;
    end);
end;

// function TVDImportSymbols.Del(VA: TVA; LibName, SymbolName: BSTR_IN;
// Ordinal: TVDSymbolOrdinal): BOOL;
// begin
// result := false;
// end;

destructor TVDImportSymbols.Destroy;
begin
  FUidToName.Free;
  FNameToUid.Free;
  inherited;
end;

procedure TVDImportSymbols.DoEndUpdate;
begin
  inherited;
  SafeNotification;
end;

procedure TVDImportSymbols.EnumLibs(cb: TVDImpLibEnumFunc; ud: pointer);
var
  c: IVDCore;
  ks: TVDStreamIO;
  cur: IBPlusTreeCursor;
  k, v: TBytes;
  uid: TVDImportLibUID;
  LibPath: string;
begin
  if not Assigned(cb) then
    exit;

  c := CoreGet;
  ImpLib_MakeKey(nil, ks);
  try
    cur := (c as TVDCore).DB.CursorCreateEx(ks.ToBytes, [kpGreater], true);
    if not Assigned(cur) then
      exit;

    repeat
      k := cur.Key;
      v := cur.Value;
      if ImpLib_ParseKey(k, uid) then
        if ImpLib_ParseValue(v, LibPath) then
          if not cb(uid, BSTR_IN(LibPath), ud) then
            break;
    until not cur.Next;
  finally
    ks.Free;
  end;
end;

procedure TVDImportSymbols.EnumSyms(VA0, VA1: TVA; cb: TVDImpSymEnumFunc;
ud: pointer);
var
  c: IVDCore;
  ks: TVDStreamIO;
  cur: IBPlusTreeCursor;
  k, v: TBytes;
var
  VA: TVA;
var
  LibUID: TVDImportLibUID;
  SymOrd: TVDSymbolOrdinal;
  LibName, SymName: string;
begin
  if not Assigned(cb) then
    exit;

  c := CoreGet();

  if VA0 = BAD_VA then
    if not c.VM.GetFirstVA(@VA0) then
      exit;

  if VA1 = BAD_VA then
    if not c.VM.GetLastVA(@VA1) then
      exit;

  if not ImpSym_MakeKey(c, VA0, ks) then
    exit;

  try
    // create cursor
    // handle prefix manually
    cur := (c as TVDCore).DB.CursorCreateEx(ks.ToBytes, [kpGreater, kpEqual], false);
    if not Assigned(cur) then
      exit;

    repeat
      k := cur.Key;
      v := cur.Value;
      if not ImpSym_ParseKey(c, k, VA) then
        break;
      if VA > VA1 then
        break;
      if not ImpSym_ParseVal(v, LibUID, SymOrd, SymName) then
        break;

      if FetchLibName(LibUID, LibName) then
        if not cb(VA, BSTR_IN(LibName), BSTR_IN(SymName), SymOrd, ud) then
          break;

    until not cur.Next;

  finally
    ks.Free;
  end;
end;

function TVDImportSymbols.Exists(VA: TVA): BOOL;
var
  c: IVDCore;
  b: TVDStreamIO;
begin
  c := CoreGet();
  if not ImpSym_MakeKey(c, VA, b) then
    exit(false);
  try
    result := (c as TVDCore).DB.ContainsKey(b.ToBytes);
  finally
    b.Free;
  end;
end;

function TVDImportSymbols.FetchLibName(uid: TVDImportLibUID;
out Name: string): boolean;
begin
  if FUidToName.Count = 0 then
    CollectLibUids;
  result := FUidToName.TryGetValue(uid, Name);
end;

function TVDImportSymbols.FetchLibUID(const LibPath: string; out Existed: boolean): TVDImportLibUID;
begin
  if FUidToName.Count = 0 then
    CollectLibUids;
  // get uid for lib
  Existed := FNameToUid.TryGetValue(LibPath, result);
  if not Existed then
    result := FUidToName.Count;
end;

// -----------------------------------------------------------------------------

function EnumFunc_Get(VA: TVA; LibStr, SymStr: BSTR_IN; SymOrd: TVDSymbolOrdinal; ud: pointer): BOOL; stdcall;
var
  ctx: PImpSymGetCtx;
begin
  ctx := ud;
  ctx.result := true; // found
  if Assigned(ctx.LibName) then
    ctx.LibName^ := LibStr;
  if Assigned(ctx.SymName) then
    ctx.SymName^ := SymStr;
  if Assigned(ctx.SymOrd) then
    ctx.SymOrd^ := SymOrd;
  result := false; // 1 time hit
end;

function TVDImportSymbols.GetSymInternal(VA: TVA;
const ctx: TImpSymGetCtx): boolean;
begin
  EnumSyms(VA, VA, EnumFunc_Get, @ctx);
  result := ctx.result;
end;

function TVDImportSymbols.Get(VA: TVA; out LibName, SymName: BSTR;
out SymOrd: TVDSymbolOrdinal): BOOL;
begin
  result := GetSymInternal(VA, TImpSymGetCtx.Create(@LibName, @SymName, @SymOrd));
end;
// -----------------------------------------------------------------------------

procedure TVDImportSymbols.LibAddInternal(const LibPath: string;
uid: TVDImportLibUID);
begin
  FUidToName.Add(uid, LibPath);
  FNameToUid.Add(LibPath, uid);
end;

function TVDImportSymbols.Put(VA: TVA; LibName, SymbolName: BSTR_IN;
Ordinal: TVDSymbolOrdinal): BOOL;
var
  c: IVDCore;
  LibUID: TVDImportLibUID;
var
  str, LibStr: string;
begin
  c := CoreGet;

  if not PutImpLib(c, LibName, LibUID) then
    exit(false);

  result := PutImpSym(c, VA, LibUID, SymbolName, Ordinal);

  if not result then
    exit;

  SafeNotification;

  // Put Name
  if (SymbolName <> '') then
    str := SymbolName
  else
  begin
    LibStr := string(LibName).Replace('.', '_');
    str := Format('%s_ordinal_%d', [LibStr, Ordinal]);
  end;

  c.Names.Put(VA, str, 0);
end;

function TVDImportSymbols.PutImpLib(const c: IVDCore;
const LibPath: string; out uid: TVDImportLibUID): boolean;
var
  k, v: TVDStreamIO;
  Existed: boolean;
begin
  uid := FetchLibUID(LibPath, Existed);

  ImpLib_MakeKey(@uid, k);
  ImpLib_MakeVal(LibPath, v);
  try
    result := (c as TVDCore).DB.Put(k.ToBytes, v.ToBytes) = BP_OK;
    if result and (not Existed) then
      LibAddInternal(LibPath, uid);
  finally
    k.Free;
    v.Free;
  end;
end;

function TVDImportSymbols.PutImpSym(const c: IVDCore; VA: TVA;
LibUID: TVDImportLibUID; const SymName: string;
SymOrd: TVDSymbolOrdinal): boolean;
var
  ks, vs: TVDStreamIO;
begin
  if not ImpSym_MakeKey(c, VA, ks) then
    exit(false);
  ImpSym_MakeVal(LibUID, SymOrd, SymName, vs);
  try
    result := (c as TVDCore).DB.Put(ks.ToBytes, vs.ToBytes) = BP_OK;
  finally
    ks.Free;
    vs.Free;
  end;
end;

procedure TVDImportSymbols.SafeNotification;
begin
  if FUpdateCount = 0 then
    CoreGet().Msg.Broadcast(MSG_IMPORTS_CHANGED);
end;

{ TImpSymGetCtx }

constructor TImpSymGetCtx.Create(LibName, SymName: PBSTR;
SymOrd: PVDSymbolOrdinal);
begin
  self.result := false;
  self.LibName := LibName;
  self.SymName := SymName;
  self.SymOrd := SymOrd;
end;

end.
