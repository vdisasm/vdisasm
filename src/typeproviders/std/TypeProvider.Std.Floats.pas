unit TypeProvider.Std.Floats;

interface

uses
  VDAPI;

type
  TTP_Float<T> = class(TInterfacedObject, IVDTypeProviderBase)
  private
    function GetBitSize: Integer;
  public
    function Decode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
    function Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
  end;

implementation

uses
  System.SysUtils;

{ TTP_Float<T> }

function TTP_Float<T>.Decode(VA: TVA; Kind: TVDDataEncoding;
  Param: Pointer): SIZE_T;
var
  c: IVDCore;
  ByteSize: Integer;
  p: Pointer;
  T: TFloatSpecial;
var
  dummy: array [0 .. 10] of byte;
  f32: single absolute dummy;
  f64: double absolute dummy;
begin
  c := CoreGet();
  ByteSize := GetBitSize div 8;

  // read data into shared f32/f64 memory
  if c.VM.Read(VA, @dummy[0], ByteSize) <> ByteSize then
    exit(0);

  case Kind of
    TVDDataEncoding.Text:
      if Assigned(Param) then
      begin
        case ByteSize of
          4:
            IVDVATextLayout(Param).AddFloat32(f32);
          8:
            IVDVATextLayout(Param).AddFloat64(f64);
        end;
      end;
  end;

  result := ByteSize;
end;

function TTP_Float<T>.Encode(VA: TVA; Kind: TVDDataEncoding;
  Param: Pointer): SIZE_T;
var
  Text: string;
  ByteSize: Integer;
var
  dummy: array [0 .. 10] of byte;
  f64: double absolute dummy;
  f32: single absolute dummy;
begin
  if VA = BAD_VA then
    exit(1);

  Text := BSTR_IN(Param);

  try

    ByteSize := GetBitSize div 8;

    case ByteSize of
      4:
        f32 := Text.ToSingle;
      8:
        f64 := Text.ToDouble;
    end;

    // todo: endianness for float numbers
    if CoreGet.VM.Write(VA, @dummy[0], ByteSize) <> ByteSize then
      exit(0);

    result := ByteSize;

  except
    exit(0);
  end;
end;

function TTP_Float<T>.GetBitSize: Integer;
begin
  if TypeInfo(T) = TypeInfo(single) then
    result := 32
  else if TypeInfo(T) = TypeInfo(double) then
    result := 64
  else
    raise Exception.Create('unsupported type');
end;

end.
