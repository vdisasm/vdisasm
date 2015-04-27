unit uDecoder;

interface

uses
  System.SysUtils,

  uCore.Strings,
  uRegions,
  uRegions.SubIds,
  VDAPI;

const
  MAX_STRING_DISPLAY_LENGTH = 32;

type
  TVDDecoder = class(TInterfacedObject, IVDDecoder)
  private const
    FHexAddrPrefix = '0x';
  private
    FHexDumpByteCount: integer;
    procedure PrintAddress(const Text: IVDVATextLayout; const VA: TVA);
    procedure MakeSampleTextForHL(const Text: IVDVATextLayout);
  public
    // * [in,out]     VA    Address, can point to middle of item.
    // *                    Out is start address of item.
    // * [out]    opt Size  Out is size of item.
    function ItemStartAndSize(VA: PVA; Size: PUInt): boolean;
  public
    { VDAPI }

    function DecodeToText([ref] VA: TVA; Text: IVDVATextLayout;
      Flags: TVDDecodeToTextFlags = TVDDecodeToTextFlag.Default): UInt32; stdcall;

    function ItemStep(InOutVA: PVA; Step: Int): BOOL; stdcall;
    function ItemStart(InOutVA: PVA): BOOL; stdcall;

    function AddressStep(InOutVA: PVA; Step: Int): SSIZE_T; stdcall;

    // Find UID at VA in database. VA can point in the middle.
    // Result is False if no item found.
    function GetTypeUid(var VA: TVA; out UID: TTypeUID; out Size: UInt32): BOOL; // stdcall;

    function GetTypeName(var VA: TVA; out TypeName: BSTR; out Size: UInt32): BOOL; stdcall;

    function HexAddressDigitCount(ReservedFlags: UInt32 = 0): Int; stdcall;
    function HexAddressPrint(VA: TVA; ReservedFlags: UInt32 = 0): BSTR; stdcall;

    procedure SetHexDumpByteCount(Value: Int); stdcall;
  end;

implementation

uses
  uCore,
  uReferences,
  uSections,
  uTypes.Mgr,
  uTypes.Parsing;

const
  SSplitter = '---------------------------------------------------';

const
  // Print options.
  MIN_REFS_TO_PRINT    = 3;
  MIN_EXPORTS_TO_PRINT = 2;
  COMMENT_MAX_LINES    = 3;

type
  TPrintCtx = record
    VA: TVA;
    Text: ^IVDVATextLayout;
    DoneCount: integer;
  end;

  PPrintCtx = ^TPrintCtx;

procedure dummy_to_normalize_delphi_formatter;
begin
end;

function min(a, b: integer): integer; inline;
begin
  if a < b then
    result := a
  else
    result := b;
end;

function it(cond: boolean; t, f: integer): integer; inline;
begin
  if cond then
    result := t
  else
    result := f;
end;

// -----------------------------------------------------------------------------
procedure AddColumnSeparator(const Text: IVDVATextLayout); inline;
begin
  Text.Skip();
end;
// -----------------------------------------------------------------------------
// Print references. References must exist.

function PrintReferences_Enum(VA: TVA; Kind: TVDReferenceKind; ud: Pointer): BOOL; stdcall;
var
  ctx: PPrintCtx;
begin
  ctx := ud;
  inc(ctx.DoneCount);
  result := ctx.DoneCount <= MIN_REFS_TO_PRINT;

  if result then
  begin
    ctx.Text.AddAddress(VA);
    ctx.Text.AddText(BSTR_IN(format('(%s)', [uReferences.RefKindToStr(Kind)])));
    ctx.Text.Skip();
  end
  else
    ctx.Text.AddText('...');
end;

procedure PrintReferences(
  const c: IVDCore;
  const VA: TVA;
  const Text: IVDVATextLayout);
{$IFNDEF DEBUG} inline; {$ENDIF}
var
  ctx: TPrintCtx;
begin
  Text.AddText('referenced from: ');
  ctx.VA := VA;
  ctx.Text := @Text;
  ctx.DoneCount := 0;
  c.Refs.Enumerate(VA, PrintReferences_Enum, @ctx);
end;

// -----------------------------------------------------------------------------
function PrintExports_Enum(VA: TVA; SymbolName: BSTR_IN; Ordinal: TVDSymbolOrdinal; ud: Pointer): BOOL; stdcall;
var
  ctx: PPrintCtx;
  name: string;
begin
  ctx := ud;
  inc(ctx.DoneCount);
  result := ctx.DoneCount <= MIN_EXPORTS_TO_PRINT;

  if result then
  begin
    if ctx.DoneCount > 1 then
      ctx.Text.LineBreak;

    name := SymbolName;

    ctx.Text.AddText('export: ');

    if name <> '' then
    begin
      ctx.Text.AddText('"');
      ctx.Text.AddText(BSTR_IN(name), TTag.TAGID_STRING);
      ctx.Text.AddText('" ');
    end;
    ctx.Text.AddText(BSTR_IN(format('(%d)', [Ordinal])));
    // ctx.Text.AddText(' ');
  end
  else
    ctx.Text.AddText('...');
end;

function PrintExports(
  const c: IVDCore;
  const VA: TVA;
  const Text: IVDVATextLayout): boolean;
{$IFNDEF DEBUG} inline; {$ENDIF}
var
  ctx: TPrintCtx;
begin
  ctx.VA := VA;
  ctx.Text := @Text;
  ctx.DoneCount := 0;
  result := c.ExportSymbols.Enumerate(VA, VA, PrintExports_Enum, @ctx);
end;

// -----------------------------------------------------------------------------
function PrintImport(
  const c: IVDCore;
  const VA: TVA;
  const Text: IVDVATextLayout): boolean;
{$IFNDEF DEBUG} inline; {$ENDIF}
var
  LibName, SymName: BSTR;
  SymOrd: TVDSymbolOrdinal;
begin
  result := c.ImportSymbols.Get(VA, LibName, SymName, SymOrd);
  if result then
  begin
    if SymName <> '' then
      Text.AddText(BSTR_IN(SymName), TTag.TAGID_STRING)
    else
      Text.AddText(BSTR_IN(format('#%d', [SymOrd])), TTag.TAGID_STRING);
    Text.AddText(' imported from ');
    Text.AddText(BSTR_IN(LibName), TTag.TAGID_STRING);
  end;
end;

// -----------------------------------------------------------------------------
procedure PrintHexBytes(
  const Text: IVDVATextLayout;
  Buf: PByte;
  BufSize, HexSize: integer
  );
var
  s: string;
  bIsOverflow: boolean;
  i, iPadding: integer;
begin
  if HexSize = 0 then
    exit;

  s := '';
  bIsOverflow := BufSize > HexSize;

  // Print buffer data first (respecting overflow).
  for i := 0 to it(bIsOverflow, HexSize - 1 - 1, BufSize - 1) do
    s := s + IntToHex(Buf[i], 2);
  if bIsOverflow then
    s := s + '..';

  // Print padding.
  iPadding := HexSize - BufSize;
  if iPadding > 0 then
    s := s + string.Create(' ', iPadding * 2);

  s := s + ' ';

  Text.AddText(BSTR_IN(s) { , TTag.TAGID_HEXINCODE } );
end;

procedure PrintHex(
  const c: IVDCore;
  const VA: TVA;
  const Text: IVDVATextLayout;
  TypeSize, HexSize: integer);
{$IFNDEF DEBUG} inline; {$ENDIF}
var
  Buf: array [0 .. 31] of byte;
begin
  if HexSize = 0 then
    exit;

  // Read bytes only if will need to print it
  if TypeSize <> 0 then
    c.VM.Read(VA, @Buf[0], min(HexSize, TypeSize));

  PrintHexBytes(Text, @Buf[0], TypeSize, HexSize);
end;

// -----------------------------------------------------------------------------

{ TVDDecoder }

function TVDDecoder.DecodeToText;
var
  c: TVDCore;
  b: byte;
  str: string;
  OneByteChar: AnsiChar absolute b;
  name, Comment: BSTR;
  TypeItem: TTypeMgrItem;
  TypeLibName, TypeName: string;
  DefinedItemSubId: byte;
  DefinedRegionData: IVDStreamIO;
  DefinedRegion: IVDVARegion;
  strCodePage: TCodePage;
  bStrTrimmed: boolean;
  LineBreaks: integer;
var
  TypeVA: TVA;
  TypeUID: TTypeUID;
  TypeSize: UInt32;
var
  ImpLib: BSTR;
  ImpSym: BSTR;
  ImpOrd: TVDSymbolOrdinal;
var
  sec: IVDSection;
var
  bFunctionBreak: boolean;
  bPrintReferences: boolean;
  bPrintComment: boolean;
  bPrintName: boolean;
  bPrintExports: boolean;
  bPrintImport: boolean;
  bPrintBody: boolean;
begin
  // todo: ItemStart can be used instead of code duplication.

  if (Flags and TVDDecodeToTextFlag.Sample4hl) <> 0 then
  begin
    MakeSampleTextForHL(Text);
    exit(0);
  end;

  c := TVDCore(CoreGet); // it's always core, no need in "as"

  LineBreaks := 0;

  // Prefetch values.
  bPrintExports := (Flags and TVDDecodeToTextFlag.SymExport) <> 0;
  bPrintImport := ((Flags and TVDDecodeToTextFlag.SymImport) <> 0) and c.GetImportSymbols.Get(VA, ImpLib, ImpSym, ImpOrd);
  bFunctionBreak := ((Flags and TVDDecodeToTextFlag.FuncSplitter) <> 0) and (c.Functions.IsFunctionStart(VA));
  bPrintReferences := ((Flags and TVDDecodeToTextFlag.Reference) <> 0) and (c.GetRefs.HasReferences(VA));
  bPrintComment := ((Flags and TVDDecodeToTextFlag.Comment) <> 0) and (c.GetComments.Get(VA, Comment));
  bPrintName := (not bPrintImport) and ((Flags and TVDDecodeToTextFlag.name) <> 0) and (c.GetNames.Get(VA, Name));
  bPrintBody := (Flags and TVDDecodeToTextFlag.Body) <> 0;

  // Section break
  sec := nil;
  if (Flags and TVDDecodeToTextFlag.SectionBreak) <> 0 then
  begin
    sec := c.GetVM.Sections.Find(VA);
    if Assigned(sec) and (sec.GetStartVA = VA) then
    begin
      Text.LineBreak;
      Text.AddText(BSTR_IN(format('section "%s"', [sec.GetName])));
      Text.LineBreak;
      Text.LineBreak;
      inc(LineBreaks);
    end;
  end;

  if bFunctionBreak then
  else if bPrintReferences or bPrintComment or bPrintName then
  begin
    if LineBreaks = 0 then
      Text.LineBreak;
  end;

  // Function break.
  if bFunctionBreak then
  begin
    Text.AddText(SSplitter);
    Text.LineBreak;
  end;

  // References.
  if bPrintReferences then
  begin
    PrintReferences(c, VA, Text);
    Text.LineBreak;
  end;

  // Comment.
  if bPrintComment then
  begin
    Text.AddTextEx(BSTR_IN(Comment), TTag.TAGID_COMMENT, COMMENT_MAX_LINES);
    Text.LineBreak;
  end;

  // Exports.
  if bPrintExports then
  begin
    if PrintExports(c, VA, Text) then
    begin
      Text.LineBreak;
      bPrintName := false;
    end;
  end;

  // Name.
  if bPrintName then
  begin
    Text.AddText(BSTR_IN(Name + ':'), TTag.TAGID_NAME);
    Text.LineBreak;
  end;

  // Address.
  if (Flags and TVDDecodeToTextFlag.Address) <> 0 then
  begin
    PrintAddress(Text, VA);
  end;

  //
  // body
  // must set TypeSize to size of item at VA
  //

  // Data type region: find TypeVA and TypeSize
  TypeVA := VA;
  DefinedRegion := c.TypeDataRegions.Get(TypeVA, DefinedItemSubId, DefinedRegionData);
  if Assigned(DefinedRegion) then
  begin
    TypeVA := DefinedRegion.GetStartVA;
    TypeSize := DefinedRegion.GetSize
  end
  else
  begin
    // TypeVA stays the same, size is assumed to be a byte.
    TypeSize := 1;
  end;

  if bPrintImport then
    PrintImport(c, VA, Text)

  else if bPrintBody then
  begin

    // todo: refactor this part into separate method

    // If it's defined region (type).
    if Assigned(DefinedRegion) then
    begin
      // Hex dump bytes
      if FHexDumpByteCount <> 0 then
        PrintHex(c, VA, Text,
          it(DefinedItemSubId <> REGION_SUBID_STRING, TypeSize, 0), // String don't need hex bytes
          FHexDumpByteCount); // width of hex dump

      case DefinedItemSubId of
        REGION_SUBID_TYPE_UID:
          begin
            // Get type UID.
            Region_Parse_UID(DefinedRegionData, TypeUID);

            // UID -> Type Name
            TVDTypeMgr(c.GetTypeMgr).GetEx(TypeUID, TypeItem);
            if (TypeItem <> nil) then
            begin
              FullTypeNameToTypeLibAndType(TypeItem.name, TypeLibName, TypeName);
              if TypeItem.Provider = nil then
                Text.AddText(BSTR_IN(format('(%s)', [TypeName])), TTag.TAGID_NONE { or TTag.TAGID_FIELD } );
              if TypeItem.Provider <> nil then
                { result := } TypeItem.Provider.Decode(TypeVA, TVDDataEncoding.Text, Pointer(Text));
            end
            else
              Text.AddText(BSTR_IN(format('n/a (typeid: %d)', [TypeUID])), TTag.TAGID_CODE);
          end;
        REGION_SUBID_STRING:
          begin
            Region_Parse_String(DefinedRegionData, strCodePage);
            if not c.Strings.ReadString(VA, TypeSize, strCodePage, name) then
              Text.AddText(BSTR_IN(SUnresolvedString))
            else
            begin
              str := name;
              bStrTrimmed := str.Length > MAX_STRING_DISPLAY_LENGTH;
              if bStrTrimmed then
                SetLength(str, MAX_STRING_DISPLAY_LENGTH);

              if strCodePage <> 0 then
              begin
                Text.AddText(BSTR_IN(IntToStr(strCodePage)));
                Text.Skip();
              end;
              Text.AddChar('"');
              Text.AddText(BSTR_IN(str), TTag.TAGID_STRING);
              Text.AddChar('"');
              if bStrTrimmed then
                Text.AddText('...');
            end;
          end;
      end;

    end

    // Else if it's not defined region (raw bytes) do byte-dump.
    else if c.GetVM.Read(VA, @b, 1) = 1 then
    begin
      if bPrintImport then
        PrintImport(c, VA, Text)
      else if bPrintBody then
      begin
        Text.AddText(BSTR_IN(format('%2.2x', [b])), TTag.TAGID_NONE);
        AddColumnSeparator(Text);
        if (b in [32 .. 126]) then
          Text.AddChar(Char(b), TTag.TAGID_STRING);
      end;
    end
    else
    begin
      exit(0); // not decodable
    end;

  end;

{$IFDEF DEBUG}
  if TypeSize = 0 then
    raise Exception.Create('Type size is 0');
{$ENDIF}
  VA := TypeVA + TypeSize;
  exit(TypeSize);
end;

function TVDDecoder.GetTypeName(var VA: TVA; out TypeName: BSTR;
  out Size: UInt32): BOOL;
var
  UID: TTypeUID;
  Item: TTypeMgrItem;
begin
  result := GetTypeUid(VA, UID, Size);
  if result then
  begin
    TVDTypeMgr(CoreGet.TypeMgr).GetEx(UID, Item);
    if Item <> nil then
      TypeName := Item.name;
  end;
end;

function TVDDecoder.GetTypeUid(var VA: TVA; out UID: TTypeUID;
  out Size: UInt32): BOOL;
var
  c: TVDCore;
begin
  c := CoreGet() as TVDCore;
  result := c.TypeDataRegions.GetUidAndData(VA, UID, Size);
end;

function TVDDecoder.ItemStart(InOutVA: PVA): BOOL;
begin
  result := ItemStartAndSize(InOutVA, nil);
end;

function JumpOverSectionGap(const prv, nxt: IVDSection; Step: integer;
  VA: PVA): boolean; inline;
begin
  if (Step < 0) and (Assigned(prv)) then
  begin
    VA^ := prv.GetLastVA;
    exit(True);
  end;
  if (Step > 0) and (Assigned(nxt)) then
  begin
    VA^ := nxt.GetStartVA;
    exit(True);
  end;
  exit(false);
end;

// Calculate distance from VA either to section start or to section end,
// depending on direction(dir).
procedure CalcSecDist(const sec: IVDSection; VA: TVA; dir: integer;
  out dist: TVDSectionSize); inline;
begin
  if dir < 0 then
    dist := VA - sec.GetStartVA
  else
    dist := sec.GetLastVA - VA;
end;

function TVDDecoder.AddressStep(InOutVA: PVA; Step: Int): SSIZE_T;
var
  todo: integer;
  cando: TVDSectionSize;
  Sections: IVDSections;
  prv, cur, nxt: IVDSection;
begin
  if (InOutVA = nil) or (Step = 0) then
    exit(0);

  result := 0;
  todo := abs(Step);
  Sections := CoreGet().VM.Sections;

  while todo <> 0 do
  begin
    Sections.FindEx(InOutVA^, @prv, @cur, @nxt);

    // Handle section boundaries.
    if (cur <> nil) then
    begin
      if
      // Is VA first, want left and can move left
        ((InOutVA^ = cur.GetStartVA) and (Step < 0) and (prv <> nil)) or
      // Is VA last, want right and can move right
        ((InOutVA^ = cur.GetLastVA) and (Step > 0) and (nxt <> nil)) then
      begin
        cur := nil;
        dec(todo);
        inc(result);
      end;
    end;

    // Handle gap.
    if cur = nil then
    begin
      if not JumpOverSectionGap(prv, nxt, Step, InOutVA) then
        break;
      // now InOutVA^ is on some end of section
    end
    else
    begin // in section
      CalcSecDist(cur, InOutVA^, Step, cando);
      if cando = 0 then
        break; // can't find more addresses
      if cando > todo then
        cando := todo;
      if Step > 0 then
        inc(InOutVA^, cando)
      else
        dec(InOutVA^, cando);
      inc(result, cando);
      dec(todo, cando);
    end;
  end;
end;

function TVDDecoder.ItemStartAndSize(VA: PVA; Size: PUInt): boolean;
var
  c: TVDCore;
  Region: IVDVARegion;
begin
  if not Assigned(VA) then
    exit(false); // no sense

  c := TVDCore(CoreGet);

  if not c.GetVM.Exists(VA^) then
  begin
    if Size <> nil then
      Size^ := 0; // because this va occupy no space
    exit(false);
  end;

  // Search for region.
  Region := c.TypeDataRegions.GetRegionOnly(VA^);
  if Assigned(Region) then
  begin
{$IFDEF DEBUG}
    if Region.GetSize = 0 then
      raise Exception.Create('Type size is 0.');
{$ENDIF}
    VA^ := Region.GetStartVA;
    if Assigned(Size) then
      Size^ := Region.GetSize;
    exit(True);
  end;

  // No meta data.
  if Assigned(Size) then
    Size^ := 1;
  exit(True);
end;

function TVDDecoder.ItemStep(InOutVA: PVA; Step: Int): BOOL;
var
  c: TVDCore;
  Sections: IVDSections;
  secPrv, secCur, secNxt: IVDSection;
  NxtVA: TVA;
  BaseVA: TVA;
  BaseSize: UInt32;
  itemEnd: TVA;
begin
  if (InOutVA = nil) or (Step = 0) then
    exit(false);

  c := TVDCore(CoreGet);
  Sections := c.GetVM.Sections;

  begin
    Sections.FindEx(InOutVA^, @secPrv, @secCur, @secNxt);
    // if cur va not exists
    if secCur = nil then
    begin
      if Step > 0 then
      begin
        if secNxt = nil then
          exit(false);
        InOutVA^ := secNxt.GetStartVA;
        exit(True);
      end
      else // Step < 0
      begin
        if secPrv = nil then
          exit(false);
        InOutVA^ := secPrv.GetLastVA;
        exit(True);
      end;
    end
    else
    // is cur va exists, check if it's 1st or last in whole vm range
    begin
      if (Step > 0) then
      begin
        if (InOutVA^ = secCur.GetLastVA) and (secNxt = nil) then
          exit(false);
      end
      else // Step < 0
      begin
        if (InOutVA^ = secCur.GetStartVA) and (secPrv = nil) then
          exit(false);
      end;
    end;
  end;

  if Step > 0 then
    NxtVA := InOutVA^ + 1
  else
    NxtVA := InOutVA^ - 1;

  // Try item
  BaseVA := NxtVA;
  if ItemStartAndSize(@BaseVA, @BaseSize) then
  begin
    if Step > 0 then
    begin
      if NxtVA = BaseVA then
        InOutVA^ := BaseVA
      else
      begin
        itemEnd := BaseVA + BaseSize;
        // if end of this item does not fit into current section, result is
        // next section start
        if (itemEnd >= secCur.GetEndVA) then
        begin
          if secNxt = nil then
            exit(false);
          itemEnd := secNxt.GetStartVA;
        end;
        InOutVA^ := itemEnd;
      end;
    end
    else
    begin
      InOutVA^ := BaseVA;
    end;
    exit(True);
  end;

  // locate next va
  Sections.FindEx(NxtVA, @secPrv, @secCur, @secNxt);

  // if va not found, try prev or next depending on Step.
  if secCur = nil then
  begin
    if Step > 0 then
    begin
      if secNxt = nil then
        exit(false);
      InOutVA^ := secNxt.GetStartVA;
      exit(True);
    end;
    if Step < 0 then
    begin
      if secPrv = nil then
        exit(false);
      InOutVA^ := secPrv.GetLastVA;
      exit(True);
    end;
  end;

  // secCur found
  InOutVA^ := NxtVA;
  exit(True);
end;

procedure TVDDecoder.MakeSampleTextForHL(const Text: IVDVATextLayout);
const
  Buf: array [0 .. 3] of byte = ($11, $22, $33, $44);
var
  VA: TVA;
begin
  VA := 0;

  Text.AddText(BSTR_IN(format('%8.8x', [VA])), TTag.TAGID_VA);
  AddColumnSeparator(Text);

  PrintHexBytes(Text, @Buf[0], 4, 4);
end;

procedure TVDDecoder.PrintAddress(const Text: IVDVATextLayout; const VA: TVA);
begin
  Text.AddText(BSTR_IN(HexAddressPrint(VA)), TTag.TAGID_VA);
  AddColumnSeparator(Text);
end;

procedure TVDDecoder.SetHexDumpByteCount(Value: Int);
begin
  FHexDumpByteCount := Value;
end;

function TVDDecoder.HexAddressDigitCount(ReservedFlags: UInt32): Int;
begin
  result := Length(FHexAddrPrefix) + TVDSections(CoreGet().VM.Sections).HexAddressDigitCount;
end;

function TVDDecoder.HexAddressPrint(VA: TVA; ReservedFlags: UInt32): BSTR;
var
  fmt: string;
  dig: integer;
begin
  dig := HexAddressDigitCount() - Length(FHexAddrPrefix);
  fmt := format('%s%%%d.%dx', [FHexAddrPrefix, dig, dig]);
  exit(format(fmt, [VA]));
end;

end.
