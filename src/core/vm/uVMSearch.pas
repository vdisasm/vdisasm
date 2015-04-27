{$WARN COMPARING_SIGNED_UNSIGNED OFF}
unit uVMSearch;

interface

uses
  System.Generics.Collections,
  System.SysUtils,

  uCore,
  uCore.Strings,

  uEncodingAdapter,

  VDAPI;

// Convert string like "EB ?? E8 ?? ?? ?? ??" to byte sequence.
// ?? chars will be replaced with MaskByte.
// Returns False if happened error on parse.
function PatternTextToBytes(
  const InText: string;
  out Buf: TBytes;
  out Mask: TBytes): boolean;

// Search byte sequence in section.
// If found, result is True and OutVA is found address.
// If not found, result is False and OutVA is not changed.
// If Mask is Nil, it's not used.
function FindBytes(
  const C: IVDCore;
  VA: PVA; // VA to start from.
  EndVA: TVA;
  Buf: PByteArray;   // Bytes to match.
  Mask: PByteArray;  // Mask bytes.
  Size: uint32;      // Sequence size.
  Direction: integer // -1, +1
  ): boolean;

function FindPatternBytes(
  const C: IVDCore;
  VA: PVA; // VA to start from.
  EndVA: TVA;
  PatternText: BSTR_IN;
  Direction: integer // -1, +1
  ): boolean;

function FindPattern(
  const C: IVDCore;
  VA: PVA; // VA to start from.
  EndVA: TVA;
  Pattern: IVDBytePattern;
  Direction: integer // -1, +1
  ): boolean; inline;

function FindString(
  const C: IVDCore;
  VA: PVA;
  EndVA: TVA;
  Text: BSTR_IN;
  CodePage: UInt;
  CaseSensitive: BOOL;
  Direction: Int): BOOL;

implementation

{$REGION 'PatternTextToBytes'}


type
  TString2 = string[2];

function HexCharToByte(hc: AnsiChar; out b: byte): boolean; inline;
begin
  if hc in ['0' .. '9'] then
    b := byte(hc) - byte('0')
  else if hc in ['a' .. 'f'] then
    b := byte(hc) - byte('a') + 10
  else if hc in ['A' .. 'F'] then
    b := byte(hc) - byte('A') + 10
  else
    exit(False);
  exit(True);
end;

function HexByteToByte(const S: TString2; out b: byte): boolean; // inline;
var
  l, r: byte;
begin
  result := False;
  case Length(S) of
    1:
      result := HexCharToByte(S[1], b);
    2:
      if HexCharToByte(S[1], l) and (HexCharToByte(S[2], r)) then
      begin
        b := (l shl 4) or r;
        result := True;
      end;
  end;
end;

function PatternTextToBytes(
  const InText: string;
  out Buf: TBytes;
  out Mask: TBytes): boolean;
const
  SEPARATORS = [' ', ',', #13, #10];
  HEXCHARS   = ['0' .. '9', 'A' .. 'F', 'a' .. 'f'];
  MASKONE    = '?';
  ALLAVAIL   = SEPARATORS + HEXCHARS + [MASKONE];
var
  hx: TString2;
  b: byte;
  IsMask: boolean;
  pText: PChar;
  ActualLength, Cnt: integer;
begin
  result := True;
  ActualLength := 0;

  if InText = '' then
    exit; // bad input

  Cnt := Length(InText);
  pText := Pointer(InText);

  // First allocate maximum space, to shrink later
  // instead of reallocating in cycle.
  SetLength(Buf, Length(InText));
  SetLength(Mask, Length(InText));

  try

    hx := '';
    while (Cnt > 0) do
    begin
      if not CharInSet(pText^, ALLAVAIL) then
        exit;

      if not CharInSet(pText^, SEPARATORS) then
        hx := hx + pText^;

      // if not separator, or collected 2 chars, or last char
      if CharInSet(pText^, SEPARATORS) or (Length(hx) = 2) or (Cnt = 1) then
      begin
        if (hx <> '') then
        begin
          // if one of char is ?, it is ??
          // MASK := (hx[1] = MASKONE) or ((Length(hx) = 2) and (hx[2] = MASKONE));
          IsMask := (hx = '?') or (hx = '??');

          if IsMask then
          begin
            // Any byte fits
            Mask[ActualLength] := 0;
            Buf[ActualLength] := 0;
          end
          else
          begin
            if not HexByteToByte(hx, b) then
              exit(False);

            Mask[ActualLength] := $FF;
            Buf[ActualLength] := b;
          end;

          hx := '';
          inc(ActualLength);
        end; // if (hx <> '') ...
      end;   // if (pText^ in ...

      inc(pText);
      dec(Cnt);

    end; // while

  finally
    // Shrink buf to actual size.
    SetLength(Buf, ActualLength);
    SetLength(Mask, ActualLength);
  end;

end;
{$ENDREGION}
{$REGION 'BytesToPatternText'}


function BytesToPatternText(Buf, Mask: PByte; Size: uint32): RawByteString;
var
  ofs: uint32;
  hx: string[2];
begin
  SetLength(result, 2 * Size);
  ofs := 1;
  while Size <> 0 do
  begin
    if Mask^ = $00 then
    begin
      result[ofs + 0] := '?';
      result[ofs + 1] := '?';
    end
    else
    begin
      hx := IntToHex(Buf^, 2);
      result[ofs + 0] := hx[1];
      result[ofs + 1] := hx[2];
    end;
    inc(Buf);
    inc(Mask);
    dec(Size);
    inc(ofs, 2);
  end;
end;
{$ENDREGION}
{$REGION 'FindBytes'}


function FindBytes(
  const C: IVDCore;
  VA: PVA; // VA to start from.
  EndVA: TVA;
  Buf: PByteArray;   // Bytes to match.
  Mask: PByteArray;  // Mask bytes.
  Size: uint32;      // Sequence size.
  Direction: integer // -1, +1
  ): boolean;
var
  cur: TBytes;
  i: integer;
  MaskByte: byte;
begin
  result := False;
  if VA = nil then
    exit;

  // Check input.
  if (Buf = nil) or (Size = 0) then
    exit;

  if Direction = 0 then
    Direction := 1;

  if EndVA = BAD_VA then
    if not C.VM.GetLastVA(@EndVA) then
      exit;

  inc(EndVA); // to get End (not last)

  SetLength(cur, Size);
  while (VA^ < EndVA) do
  begin
    // Fetch.
    if C.VM.Read(VA^, @cur[0], Size) = Size then
    begin

      // Check.
      for i := 0 to Size - 1 do
      begin
        if Assigned(Mask) then
          MaskByte := Mask[i]
        else
          MaskByte := $FF; // want all bits

        if (cur[i] and MaskByte) = Buf[i] then
        begin // match
          if i = (Size - 1) then
          begin
            result := True;
            break;
          end;
        end
        else // diff
          break;
      end;

      if result then
        break;
    end;

    // Next address.
    if C.Decoder.AddressStep(VA, Direction) = 0 then
      break;
  end;

  if result then
    C.Log.WriteLn(Format('Pattern found at 0x%x', [VA^]))
  else
    C.Log.WriteLn(Format('Pattern not found. Stopped at 0x%x', [VA^]));
end;
{$ENDREGION}
{$REGION 'FindPatternBytes'}


function FindPatternBytes(
  const C: IVDCore;
  VA: PVA; // VA to start from.
  EndVA: TVA;
  PatternText: BSTR_IN;
  Direction: integer // -1, +1
  ): boolean;
var
  Buf, Mask: TBytes;
  TestPat: RawByteString;
begin
  if not PatternTextToBytes(PatternText, Buf, Mask) then
    exit(False);

  TestPat := BytesToPatternText(@Buf[0], @Mask[0], Length(Buf));
  C.Log.WriteLn(BSTR('Search pattern: ' + TestPat));

  result := FindBytes(C, VA, EndVA,
    @Buf[0],
    @Mask[0],
    Length(Buf),
    Direction);
end;
{$ENDREGION}
{$REGION 'FindPattern'}


function FindPattern(
  const C: IVDCore;
  VA: PVA; // VA to start from.
  EndVA: TVA;
  Pattern: IVDBytePattern;
  Direction: integer // -1, +1
  ): boolean;
begin
  result := FindBytes(C, VA, EndVA,
    PByteArray(Pattern.GetData),
    PByteArray(Pattern.GetMask),
    Pattern.GetSize,
    Direction
    );
end;
{$ENDREGION}


// EndVA is valud and don't need verification.
// Insensitive search (with locale) is monster function indeed :)
function FindString_CaseInsensitive(const C: IVDCore; VA: PVA; EndVA: TVA;
  Text: BSTR_IN; CodePage: UInt; Direction: Int): BOOL;
type
  // List of string characters
  TCharByteList = TList<TBytes>;
var
  enc: TEncoding;
  bytesLower, bytesUpper: TBytes;
  i, charLen, charOfs: integer;
  iChar, iCharHigh, lenLower, lenUpper: integer;
  S, sText: string;
  lower, upper: TCharByteList;
  breakLevel2: boolean;
  Buf: array [0 .. 15] of byte;
begin
  result := False;

  // Prefetch string character bytes.
  sText := Text;
  enc := GetEncodingByCodepage(CodePage);
  lower := TCharByteList.Create;
  upper := TCharByteList.Create;
  try
    try
      for i := low(sText) to high(sText) do
      begin
        S := sText[i];
        S := S.ToLower;
        bytesLower := enc.GetBytes(S);

        S := sText[i];
        S := S.ToUpper;
        bytesUpper := enc.GetBytes(S);

        lower.Add(bytesLower);
        upper.Add(bytesUpper);
      end;
    finally
      enc.Free;
    end;

    iCharHigh := lower.Count - 1;

    // Search
    while VA^ < EndVA do
    begin
      charOfs := 0;

      // Match string at current VA.
      for iChar := 0 to iCharHigh do
      begin
        // Get bytes.
        bytesLower := lower[iChar];
        bytesUpper := upper[iChar];
        // Get max len to read.
        lenLower := Length(bytesLower);
        lenUpper := Length(bytesUpper);
        if lenLower > lenUpper then
          charLen := lenLower
        else
          charLen := lenUpper;

        // Read.
        if C.VM.Read(VA^ + charOfs, @Buf[0], charLen) = charLen then
        begin
          // Match.
          breakLevel2 := False;
          for i := 0 to charLen - 1 do
          begin
            if
            // check lower
              ((i < lenLower) and (Buf[i] = bytesLower[i])) or
            // check upper
              ((i < lenUpper) and (Buf[i] = bytesUpper[i])) then
            begin
              inc(charOfs);
              // this char matched
              // if it was last char, we found string
              if iChar = iCharHigh then
              begin
                result := True;
                exit;
              end;
            end
            else
            begin // char not matched
              breakLevel2 := True;
              break;
            end;
          end;
          if breakLevel2 then
            break;
        end;
      end;
      // Next address.
      if C.Decoder.AddressStep(VA, 1) = 0 then
        break;
    end;
  finally
    lower.Free;
    upper.Free;
  end;
end;

// EndVA is valud and don't need verification.
function FindString_CaseSensitive(const C: IVDCore; VA: PVA; EndVA: TVA;
  Text: BSTR_IN; CodePage: UInt; Direction: Int): BOOL;
var
  enc: TEncoding;
  bytes: TBytes;
begin
  enc := GetEncodingByCodepage(CodePage);
  try
    bytes := enc.GetBytes(Text);
    result := FindBytes(C, VA, EndVA, @bytes[0], nil, Length(bytes), Direction);
  finally
    enc.Free;
  end;
end;

function FindString(
  const C: IVDCore;
  VA: PVA;
  EndVA: TVA;
  Text: BSTR_IN;
  CodePage: UInt;
  CaseSensitive: BOOL;
  Direction: Int): BOOL;
begin
  if not Assigned(VA) then
    exit(False); // bad param

  if EndVA = BAD_VA then
    if not C.VM.GetLastVA(@EndVA) then
      exit(False);
  inc(EndVA); // to get End (not last)

  if CaseSensitive then
    result := FindString_CaseSensitive(C, VA, EndVA, Text, CodePage, Direction)
  else
    result := FindString_CaseInsensitive(C, VA, EndVA, Text, CodePage, Direction);

  if result then
    C.Log.WriteLn(Format(SStringFoundAt, [VA^]))
  else
    C.Log.WriteLn(Format(SStringNotFoundStoppedAt, [VA^]));
end;

end.
