{
  Probably need to check if region is already occupied.
  Maybe in Regions unit.
}
unit uStrings;

interface

uses
  System.Character,
  System.SysUtils,

  uEncodingAdapter,

  VDAPI;

type
  TVDStrings = class(TInterfacedObject, IVDStrings)
  public
    function Define(VA: TVA; Size: Int; CodePage: TCodePage): BOOL; stdcall;

    function DefineCharTerminatedString(
      VA: TVA;
      TermChar: WideChar;
      CodePage: TCodePage;
      MaxSize: Int = 0): BOOL; stdcall;

    function DefineNullTerminatedString(
      VA: TVA;
      CodePage: TCodePage;
      MaxSize: Int = 0): BOOL; stdcall;

    function Get(var VA: TVA; out Size: Int; out CodePage: TCodePage): BOOL; stdcall;
    function GetText(var VA: TVA; out Text: BSTR): BOOL; stdcall;

    function ReadString(VA: TVA; Size: Int; CodePage: TCodePage; out Text: BSTR): BOOL; stdcall;

    function ScanCharTerminatedStringSize(
      VA: TVA;
      CodePage: TCodePage;
      TermChar: WideChar;
      MaxSize: Int = 0): Int; stdcall;

    function GetLenPrefixedStringSize(VA: TVA; PrefixSize: Int): Int; stdcall;

    function Undefine(VA: TVA): BOOL; stdcall;

    function ScanForStrings(
      VABegin, VAEnd: TVA;
      MinSize: Int;
      cb: TVDStringScanFunc; ud: pointer): BOOL; stdcall;
  end;

implementation

uses
  uCore,
  uRegions,
  uRegions.SubIds,
  uStream,
  uStream.MemoryStream;

const
  MORE_OR_LESS_PRINTABLE_CHARSET = [7 .. 13, 32 .. 126];

  { TVDStrings }

function TVDStrings.Define(VA: TVA; Size: Int; CodePage: TCodePage): BOOL;
var
  c: TVDCore;
  io: IVDStreamIO;
begin
  if Size = 0 then
    Exit(False);
  c := TVDCore(CoreGet());
  io := TVDStreamIO.Create(TVDMemoryStream.Create);
  Region_Write_String(io, CodePage);
  Result := c.TypeDataRegions.Put(VA, Size, REGION_SUBID_STRING, io);
end;

function TVDStrings.DefineCharTerminatedString(VA: TVA; TermChar: WideChar;
  CodePage: TCodePage; MaxSize: Int): BOOL;
begin
  Result := Define(VA,
    ScanCharTerminatedStringSize(VA, CodePage, TermChar, MaxSize),
    CodePage);
end;

function TVDStrings.DefineNullTerminatedString(VA: TVA; CodePage: TCodePage;
  MaxSize: Int): BOOL;
begin
  Result := DefineCharTerminatedString(VA, #0, CodePage, MaxSize);
end;

function TVDStrings.Get(var VA: TVA; out Size: Int; out CodePage: TCodePage): BOOL;
var
  c: TVDCore;
  Region: IVDVARegion;
  SubId: byte;
  io: IVDStreamIO;
begin
  c := TVDCore(CoreGet());
  Region := c.TypeDataRegions.Get(VA, SubId, io);
  if Assigned(Region) and (SubId = REGION_SUBID_STRING) then
  begin
    VA := Region.GetStartVA;           // out VA
    Size := Region.GetSize;            // out Size
    Region_Parse_String(io, CodePage); // out CodePage
    Exit(True);
  end;
  Exit(False);
end;

function TVDStrings.GetLenPrefixedStringSize(VA: TVA; PrefixSize: Int): Int;
var
  io: IVDStreamIO;
begin
  Result := 0;
  if PrefixSize <> 0 then
  begin
    // not quite optimal, probably optimize some in future
    io := CoreGet.VM.CreateStreamIO(VA, VA + PrefixSize - 1, True);
    Result := io.ReadWord(PrefixSize);
  end;
end;

function TVDStrings.GetText(var VA: TVA; out Text: BSTR): BOOL;
var
  Size: Int;
  CodePage: TCodePage;
begin
  Result := self.Get(VA, Size, CodePage);
  if Result then
    Result := ReadString(VA, Size, CodePage, Text);
end;

function ProcessString(const s: string): string;
var
  c: char;
begin
  // http://en.wikipedia.org/wiki/Control_character
  Result := '';
  for c in s do
  begin
    if c = #7 then
      Result := Result + '\a'
    else if c = #8 then
      Result := Result + '\b'
    else if c = #9 then
      Result := Result + '\t'
    else if c = #10 then
      Result := Result + '\n'
    else if c = #11 then
      Result := Result + '\v'
    else if c = #12 then
      Result := Result + '\f'
    else if c = #13 then
      Result := Result + '\r'
    else if (CharInSet(c, [#32 .. #126])) or (c.IsLetterOrDigit or c.IsSeparator or c.IsPunctuation) then
      Result := Result + c
    else
      Result := Result + format('\%d', [integer(c)]);
  end;
end;

function TVDStrings.ReadString(VA: TVA; Size: Int; CodePage: TCodePage;
  out Text: BSTR): BOOL;
var
  Bytes: TBytes;
  Enc: TEncoding;
  s: string;
begin
  Result := False;
  if Size = 0 then
    Exit;
  SetLength(Bytes, Size);
  if CoreGet().VM.Read(VA, @Bytes[0], Size) <> Size then
    Exit;
  Enc := GetEncodingByCodepage(CodePage);
  try
    s := Enc.GetString(Bytes);
    s := ProcessString(s);
    Text := s;
    Result := True;
  finally
    Enc.Free;
  end;
end;

function TVDStrings.ScanCharTerminatedStringSize(
  VA: TVA;
  CodePage: TCodePage;
  TermChar: WideChar;
  MaxSize: Int): Int;
var
  c: IVDCore;
  Enc: TEncoding;
  max, len, Read: integer;
  Bytes: TBytes;
  chars: TCharArray;
begin
  Result := 0;
  Enc := GetEncodingByCodepage(CodePage);
  try
    max := Enc.GetMaxByteCount(1);
    SetLength(Bytes, max);
    c := CoreGet;
    while True do
    begin
      // Read at least 1 byte.
      read := c.VM.Read(VA, @Bytes[0], max);
      if read < 1 then
        break;

      // Convert bytes to chars.
      chars := Enc.GetChars(Bytes);
      if not Assigned(chars) then
        break;

      // Process first char in read buffer.
      if chars[0] = TermChar then
        break; // termination character found

      // Get char length in bytes.
      len := Enc.GetByteCount(chars, 0, 1);

      // Length must be at least 1 byte.
      if len = 0 then
        break;

      // Move forward.
      inc(Result, len);
      inc(VA, len);
    end;
  finally
    Enc.Free;
  end;
end;

function TVDStrings.Undefine(VA: TVA): BOOL;
begin
  Result := TVDCore(CoreGet()).TypeDataRegions.Del(VA) <> 0;
end;

// -----------------------------------------------------------------------------
// String scanning
// -----------------------------------------------------------------------------
type
  TStringRegion = record
    VA: TVA;
    Size: integer; // 0 if region doesn't exist
  end;

  // It must be called form ScanForStrings when string found.
  // The purpose is to detect few types of strings at same va.
  //
  // Region is used to avoid reading (for example prefix length) from already
  // recognized string region (previous actually).
  //
  // On input region is used as previous region.
  // On output it is filled with va,size for current string.
function DoStringFound(
  const c: IVDCore;
  VA: TVA;       // string address
  Size: integer; // string size
  MinSize: integer;
  CodePage: TCodePage;
  cb: TVDStringScanFunc;
  ud: pointer;
  var rgn: TStringRegion
  ): boolean;
{$IFDEF STRSCAN_LENPREFIX}
var
  lenSize: byte;
  len: uint32;
  lenVA: TVA;
  lenVAinPrevRgn: boolean;
{$ENDIF}
begin
{$IFDEF STRSCAN_LENPREFIX}
  // First check length-prefixed strings.
  // They have higher priority than simple array of chars.

  // Size is >= MinimalSize.

  // If prev region ends somewhere before current VA we can try to search
  // length prefixes.
  // Skip if region size is 0 (it's first address).
  if (rgn.Size <> 0) or (rgn.VA + rgn.Size < VA) then
  begin
    lenSize := 4;
    while lenSize <> 0 do
    begin
      // We must not read prefix length if it is inside of previous string region.
      // So it shoulf be enough to have info on last string found.
      lenVA := VA - lenSize;

      lenVAinPrevRgn := (rgn.Size <> 0) and (lenVA >= rgn.VA) and (lenVA < rgn.VA + rgn.Size);

      if not lenVAinPrevRgn then
      begin
        // Null len variable because read will read only part of it.
        // todo: it probably should have endianness-safe reading.
        // But now we skip it to be little-endian only.
        len := 0;

        // Read length (full/partial variable).
        // Check if lenVA is not in previous region.
        if c.VM.Read(lenVA, @len, lenSize) = lenSize then
        begin
          if (len <> 0) and (len > MinSize) and (len <= Size) then
          begin
            rgn.VA := VA;
            rgn.Size := len;
            Result := cb(VA, len, CodePage, ud);
            Exit;
          end;
        end;
      end;
      lenSize := lenSize div 2;
    end;
  end;
{$ENDIF STRSCAN_LENPREFIX}
  // default
  rgn.VA := VA;
  rgn.Size := Size;
  Result := cb(VA, Size, CodePage, ud);
end;

function TVDStrings.ScanForStrings(
  VABegin, VAEnd: TVA;
  MinSize: Int;
  cb: TVDStringScanFunc;
  ud: pointer): BOOL;
const
  DEFAULT_MIN_SIZE = 2;
var
  c: IVDCore;
  VA, StrVA: TVA;
  AChar: byte;
  Size: Int;
  rgn: TStringRegion;
begin
  c := CoreGet();

  if VABegin = BAD_VA then
    if not c.VM.GetFirstVA(@VABegin) then
      Exit(False);

  if VAEnd = BAD_VA then
    if not c.VM.GetLastVA(@VAEnd) then
      Exit(False)
    else
      inc(VAEnd);

  if MinSize < 1 then
    MinSize := DEFAULT_MIN_SIZE;

  // todo: remove this
  // VAEnd := VABegin + $10000;

  Result := True;

  VA := VABegin;

  Size := 0;
  StrVA := 0;    // compiler friendly
  rgn.Size := 0; // va doesn't matter if size is 0
  while VA < VAEnd do
  begin
    if c.VM.Read(VA, @AChar, 1) <> 1 then
      break;

    if Size = 0 then
      StrVA := VA;

    if AChar in MORE_OR_LESS_PRINTABLE_CHARSET then
    begin
      inc(Size);
    end
    else
    begin
      // String found.
      if Size >= MinSize then
      begin
        if not DoStringFound(c, StrVA, Size, MinSize, 0, cb, ud, rgn) then
          break;

        // Update va to point to last char of found string.
        if rgn.Size <> 0 then
          VA := rgn.VA + rgn.Size - 1;
      end;

      Size := 0;
    end;

    if c.Decoder.AddressStep(@VA, 1) = 0 then
      break;
  end;
end;

// -----------------------------------------------------------------------------

end.
