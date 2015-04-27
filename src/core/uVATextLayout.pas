unit uVATextLayout;

interface

uses
  VDAPI,
  uTextLayout;

type
  TVDVATextLayout = class(TVDTextLayout, IVDVATextLayout)
  public
    procedure AddAddress(Value: UInt64); stdcall;
    procedure AddRegister(Id: UInt32); stdcall;
    procedure AddInteger(Value: UInt64; Bits: Integer; Signed: boolean; ForceSign: BOOL = False); stdcall;
    procedure AddFloat32(Value: Float32); stdcall;
    procedure AddFloat64(Value: Float64); stdcall;
  end;

implementation

uses
  System.SysUtils;

{ TVDVATextLayout }

procedure TVDVATextLayout.AddAddress(Value: UInt64);
var
  c: IVDCore;
  Text: BSTR;
begin
  c := CoreGet;

  // try to get some text instead of just address

  AddField(1);

  if c.Names.Get(Value, Text) then
    AddText(BSTR_IN(Text), TTag.TAGID_LABEL)
  else
    // simple address
    AddText(BSTR_IN(Format('0x%x', [Value])), TTag.TAGID_LABEL);
end;

procedure TVDVATextLayout.AddFloat32(Value: Float32);
begin
  AddText(BSTR_IN(FloatToStr(Value)), TTag.TAGID_NUMBER);
end;

procedure TVDVATextLayout.AddFloat64(Value: Float64);
begin
  AddText(BSTR_IN(FloatToStr(Value)), TTag.TAGID_NUMBER);
end;

procedure TVDVATextLayout.AddInteger(Value: UInt64; Bits: Integer;
  Signed: boolean; ForceSign: BOOL);
var
  Sign, Pfx: string;
  mask: UInt64;
begin
  mask := UInt64(1) shl (Bits - 1); // sign mask
  if (Signed) and ((Value and mask) <> 0) then
  begin
    Sign := '-';
    // negate number
    Value := not Value;
    inc(Value);
  end
  else
  begin
    if ForceSign then
      Sign := '+'
    else
      Sign := '';
  end;

  // prefix
  if Value > 9 then
    Pfx := '0x'
  else
    Pfx := '';

  AddField(1);
  AddText(BSTR_IN(Format('%s%s%x', [Sign, Pfx, Value])), TTag.TAGID_NUMBER);
end;

procedure TVDVATextLayout.AddRegister(Id: UInt32);
begin
  AddText(BSTR_IN(Format('reg_%x', [Id])), TTag.TAGID_REGISTER);
end;

end.
