unit uSymbols.Export;

interface

uses
  uUpdateable,
  VDAPI;

type
  TVDExportSymbols = class(TVDUpdateable, IVDExportSymbols)
  protected
    procedure DoEndUpdate; override;
    procedure SafeNotification;
  public
    function Put(VA: TVA; SymbolName: BSTR_IN; Ordinal: TVDSymbolOrdinal): BOOL; stdcall;
    function Enumerate(VA0, VA1: TVA; cb: TVDExportSymbolsEnumFunc; ud: pointer): BOOL; stdcall;
  end;

implementation

uses
  System.SysUtils,
  uCore,
  uDB,
  uStream,
  uStream.MemoryStream,
  BPlusTree.Intf;

{
  key: Tag,RelVA,Ordinal
  val: Text/null
}

function ConstructKey(
  const c: IVDCore;
  { opt,in } VA: PVA;
  { opt,in } Ordinal: PVDSymbolOrdinal;
  out b: IVDStreamIO): boolean;
var
  RelVA: TRelVA;
begin
  b := nil;
  result := false;
  if Assigned(VA) then
    if not c.VM.AbsToRelVA(VA^, RelVA) then
      exit;
  b := TVDStreamIO.Create(TVDMemoryStream.Create());
  b.WriteU8(DBTAG_ExportedSym); // tag
  if not Assigned(VA) then
    exit(true);
  b.WriteRelVA(RelVA); // relva
  if Assigned(Ordinal) then
    b.WriteWord(SizeOf(TVDSymbolOrdinal), Ordinal^); // ord
  result := true;
end;

function ParseKey(var b: TBytes; out RelVA: TRelVA; out Ordinal: TVDSymbolOrdinal): boolean;
var
  s: TVDStreamIO;
begin
  if Length(b) < SizeOf(TDBTag) + SizeOf(TRelVA) + SizeOf(TVDSymbolOrdinal) then
    exit(false);
  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  try
    // tag must match
    if s.ReadTag <> DBTAG_ExportedSym then
      exit(false);
    s.ReadRelVA(RelVA);
    Ordinal := s.ReadWord(SizeOf(TVDSymbolOrdinal));
    result := true;
  finally
    s.Free;
  end;
end;

function ParseValue(var b: TBytes; out Name: string): boolean;
var
  s: TVDStreamIO;
begin
  name := '';
  if b = nil then
    exit(true);
  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(b));
  try
    name := s.ReadStr();
    result := true;
  finally
    s.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TVDExportSymbols.DoEndUpdate;
begin
  inherited;
  SafeNotification;
end;

function TVDExportSymbols.Enumerate;
var
  c: IVDCore;
  k: IVDStreamIO;
  key, value: TBytes;
  cursor: IBPlusTreeCursor;
var
  RelVA: TRelVA;
  AbsVA: TVA;
  Name: string;
  Ordinal: TVDSymbolOrdinal;
begin
  result := false;
  c := CoreGet();

  // va0
  if VA0 = BAD_VA then
    if not c.VM.GetFirstVA(@VA0) then
      exit;

  // va1
  if VA1 = BAD_VA then
    if not c.VM.GetLastVA(@VA1) then
      exit;

  // all keys starting from Tag,VA... (any ordinal)
  if not ConstructKey(c, @VA0, nil, k) then
    exit;

  // need keys > tag,va (to have ordinal)
  // non-prefix, we'll handle prefix ourself
  cursor := TVDCore(c).DB.CursorCreateEx((k as TVDStreamIO).ToBytes, [kpGreater], false);
  if not Assigned(cursor) then
    exit; // no keys found

  // visit keys
  repeat
    key := cursor.key;
    value := cursor.value;

    // ensure it's ExportSym tag, get VA and Ordinal
    if not ParseKey(key, RelVA, Ordinal) then
      break;
    if not c.VM.RelToAbsVA(RelVA, AbsVA) then
      break;
    // bound check
    if AbsVA > VA1 then
      break;
    // there are keys
    result := true;
    // if no callback, we're done
    if not Assigned(cb) then
      break;
    // parse value: get Name
    if not ParseValue(value, Name) then
      break;
    // finally callback
    if not cb(AbsVA, BSTR_IN(name), Ordinal, ud) then
      break;
  until not cursor.Next;
end;

// -----------------------------------------------------------------------------

function TVDExportSymbols.Put(VA: TVA; SymbolName: BSTR_IN;
  Ordinal: TVDSymbolOrdinal): BOOL;
var
  c: IVDCore;
  k, v: IVDStreamIO;
  value: TBytes;
var
  str: string;
begin
  result := false;

  c := CoreGet();

  // key
  if not ConstructKey(c, @VA, @Ordinal, k) then
    exit;

  // value
  if (SymbolName <> nil) and (SymbolName <> '') then
  begin
    v := TVDStreamIO.Create(TVDMemoryStream.Create());
    (v as TVDStreamIO).WriteStr(SymbolName);
    value := (v as TVDStreamIO).ToBytes;
  end
  else
    value := nil;

  // put

  result := TVDCore(c).DB.Put((k as TVDStreamIO).ToBytes, value) = BP_OK;

  if not result then
    exit;

  SafeNotification;

  // Put Name
  if (SymbolName <> '') then
    str := SymbolName
  else
    str := Format('exported_ordinal_%d', [Ordinal]);

  c.Names.Put(VA, str, 0);
end;

procedure TVDExportSymbols.SafeNotification;
begin
  if FUpdateCount = 0 then
    CoreGet().Msg.Broadcast(MSG_EXPORTS_CHANGED);
end;

end.
