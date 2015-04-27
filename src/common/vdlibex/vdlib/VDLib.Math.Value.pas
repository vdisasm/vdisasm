unit VDLib.Math.Value;

interface

type
  TValue = class

    class function BSwap16(x: uint16): uint16; inline;
    class function BSwap32(x: uint32): uint32; inline;
    class function BSwap64(x: uint64): uint64; inline;
    class function SignExtend32(Value: uint32; BitSize: Byte): int32; inline;

  end;

implementation


class function TValue.BSwap16(x: uint16): uint16;
begin
  result :=
    ((x and $00FF) shl 8) or
    ((x and $FF00) shr 8);
end;

class function TValue.BSwap32(x: uint32): uint32;
begin
  result :=
    ((x and $000000FF) shl 24) or
    ((x and $0000FF00) shl 8) or
    ((x and $00FF0000) shr 8) or
    ((x and $FF000000) shr 24);
end;

class function TValue.BSwap64(x: uint64): uint64;
begin
  result :=
    ((x and $00000000000000FF) shl 56) or
    ((x and $000000000000FF00) shl 40) or
    ((x and $0000000000FF0000) shl 24) or
    ((x and $00000000FF000000) shl 8) or
    ((x and $000000FF00000000) shr 8) or
    ((x and $0000FF0000000000) shr 24) or
    ((x and $00FF000000000000) shr 40) or
    ((x and $FF00000000000000) shr 56);
end;

class function TValue.SignExtend32(Value: uint32; BitSize: Byte): int32;
var
  Mask: uint32;
  i: integer;
begin
  if BitSize <> 0 then
  begin
    Mask := (Value shr (BitSize - 1)) and 1; // sign
    Mask := Mask shl BitSize;                // +1
    if Mask <> 0 then
      for i := BitSize + 1 to 32 do // +1
      begin
        Value := Value or Mask;
        Mask := Mask shl 1;
      end;
  end;
  result := Value;
end;

end.
