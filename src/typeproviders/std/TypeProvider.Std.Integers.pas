unit TypeProvider.Std.Integers;

interface

uses
  VDAPI;

type
  TTP_Int<T> = class(TInterfacedObject, IVDTypeProviderBase)
  public
    procedure GetSelfInfo(out Signed: boolean; out ByteSize: integer);
  public
    function Decode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
    function Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
  end;

function DecodeIntToText(c: IVDCore; VA: TVA; Text: IVDVATextLayout;
  BitSize: integer; Signed: boolean): Int;

function CalculateNumberBitSize(Number: uint64; IsNegative: boolean): integer;

implementation

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo;

// calculate bit size of number
function CalculateNumberBitSize(Number: uint64; IsNegative: boolean): integer;
var
  bit: integer;
  mask: uint64;
begin
  Result := 64;
  for bit := 63 downto 0 do
  begin
    mask := uint64(1) shl bit;
    if (Number and mask) <> 0 then
      break;
    dec(Result);
  end;
  if IsNegative then
    inc(Result);
end;

function DecodeIntToText(c: IVDCore; VA: TVA; Text: IVDVATextLayout;
  BitSize: integer; Signed: boolean): Int;
var
  cbCnt: UInt;
  val: uint64;
  str, sfmt: string;
begin
  if BitSize mod 8 <> 0 then
    exit(0);
  cbCnt := BitSize div 8;
  val := 0;
  if c.VM.Read(VA, @val, cbCnt) <> cbCnt then
    exit(0);

  if Assigned(Text) then
  begin
    str := '?';

    {
    if val < 10 then
      sfmt := format('%%%d.%dx', [cbCnt * 2, cbCnt * 2])
    else
      sfmt := format('0x%%%d.%dx', [cbCnt * 2, cbCnt * 2]);
    str := format(sfmt, [val]);

    Text.AddText(BSTR_IN(str), TTag.TAGID_NUMBER);
    }
    Text.AddInteger(val, BitSize, Signed);
  end;

  Result := cbCnt;
end;

procedure TTP_Int<T>.GetSelfInfo(out Signed: boolean;
  out ByteSize: integer);
var
  ti: PTypeInfo;
  td: PTypeData;
begin
  ByteSize := SizeOf(T);
  ti := TypeInfo(T);
  td := GetTypeData(ti);
  case ti.Kind of
    tkInteger:
      Signed := td.MinValue <> 0;
    tkInt64:
      Signed := td.MinInt64Value <> 0;
  else
    raise Exception.Create('Unexpected type.');
  end;
end;

{ TTypeProvider_Int<T> }

function TTP_Int<T>.Decode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
var
  ByteSize: integer;
  Signed: boolean;
begin
  Result := 0;
  GetSelfInfo(Signed, ByteSize);
  if ByteSize <> 0 then
  begin
    case Kind of
      TVDDataEncoding.Size:
        Result := ByteSize;
      TVDDataEncoding.Text:
        Result := DecodeIntToText(CoreGet, VA, IVDVATextLayout(Param), ByteSize * 8, Signed);
    end;
  end;
end;

function TTP_Int<T>.Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T;
var
  Text: string;
  Signed: boolean;
  ByteSize, BitSize, Error: integer;
  Value64: uint64;
  bNegative: boolean;
  c: IVDCore;
  io: IVDStreamIO;
begin
  if VA = BAD_VA then
    exit(1); // encoding supported

  Text := Trim(BSTR_IN(Param));
  GetSelfInfo(Signed, ByteSize);

  bNegative := Text.Substring(0, 1) = '-';
  if bNegative then
    Text := Text.Substring(1);

  if Text.Substring(0, 2).ToLower = '0x' then
    Text := '$' + Text.Substring(2);

  c := CoreGet;

  // Try to make number and fit into ByteSize.
  val(Text, Value64, Error);
  if Error <> 0 then
  begin
    c.Log.WriteLn('Bad number');
    exit(0);
  end;

  BitSize := CalculateNumberBitSize(Value64, bNegative);

  if BitSize > (ByteSize * 8) then
  begin
    c.Log.WriteLn('Number does not fit');
    exit(0);
  end;

  if bNegative then
    Value64 := uint64(-Value64);

  io := c.VM.CreateStreamIO(VA, VA + ByteSize, False);
  io.WriteWord(ByteSize, Value64);

  Result := ByteSize;

end;

end.
