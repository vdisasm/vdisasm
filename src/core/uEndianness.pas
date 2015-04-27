unit uEndianness;

interface

uses
  VDAPI;

const
  COMMON_ENDIANNESS = TEndianness.Little;

function GetSystemEndianness: TEndianness; stdcall;

// Convert buffer from *system* to *common* endianness.
procedure BufConvertEndianness(Buf: PByte; Size: UInt32; Src, Dst: TEndianness);
procedure BufSysToEndianness(Buf: PByte; Size: UInt32; E: TEndianness);
procedure BufEndiannessToSys(Buf: PByte; Size: UInt32; E: TEndianness);

implementation

uses
  System.SysUtils;

const
  SUnsupportedEndianness = 'Not supported endianness';

var
  FEndianness: TEndianness;

function GetSystemEndianness: TEndianness;
begin
  Result := FEndianness;
end;

exports
  GetSystemEndianness;

procedure InitEndianness;
const
  test: word = $1122;
var
  ptr: packed array [0 .. 1] of byte absolute test;
begin
  if (ptr[0] = $22) then
    FEndianness := TEndianness.Little
  else
    FEndianness := TEndianness.Big;
end;

procedure SwapBytes(p: PByte; Size: UInt32);
var
  E: PByte;
  B: byte;
begin
  E := @p[Size - 1];
  while p < E do
  begin
    B := p^;
    p^ := E^;
    E^ := B;
    inc(p);
    dec(E);
  end;
end;

procedure BufConvertEndianness(Buf: PByte; Size: UInt32; Src, Dst: TEndianness);
begin
  if (Src = Dst) or (Dst = TEndianness.None) or (Size = 0) or (Size = 1) then
    exit; // no conversion needed

  if Src = TEndianness.None then
    raise Exception.Create('BufConvertEndianness: source endianness cannot be None.');

  case Src of
    // LE -> xxx
    TEndianness.Little:
      begin
        case Dst of
          TEndianness.Big:
            SwapBytes(Buf, Size);
        else
          raise Exception.Create(SUnsupportedEndianness);
        end;
      end;
    // BE -> xxx
    TEndianness.Big:
      begin
        case Dst of
          TEndianness.Little:
            SwapBytes(Buf, Size);
        else
          raise Exception.Create(SUnsupportedEndianness);
        end;
      end;
  else
    raise Exception.Create(SUnsupportedEndianness);
  end;
end;

procedure BufSysToEndianness(Buf: PByte; Size: UInt32; E: TEndianness);
begin
  BufConvertEndianness(Buf, Size, FEndianness, E);
end;

procedure BufEndiannessToSys(Buf: PByte; Size: UInt32; E: TEndianness);
begin
  BufConvertEndianness(Buf, Size, E, FEndianness);
end;

initialization

InitEndianness;

end.
