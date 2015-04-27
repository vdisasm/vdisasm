{
  * Params syntax:
  * @<addr>:             address to call
  * L"string":           wide string (pushed on stack)
  * "string":            ansi string (pushed on stack)
  * 123/$123/0x123       int (either in register or on stack)

  int StrLen(char* str)
  @0xAABBCCDD
  "string value"
}
unit uDebugLiveCall;

interface

uses
  System.AnsiStrings,
  System.Generics.Collections,
  System.SysUtils,

  uExpression,

  VDAPI;

function LiveCall(const Dbg: IVDDebuggerPlugin; VA: TVA; const Params: string): boolean;

implementation

type
  TVDLiveCallParams = class(TInterfacedObject, IVDLiveCallParams)
  private
    FArgs: TList<IVDConstExpression>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddArg(Value: IVDConstExpression); stdcall;
    function GetArg(Index: int; out Value: IVDConstExpression): BOOL; stdcall;
  end;

function ParseParams(const Params: string; var VA: TVA; var CallParams: TVDLiveCallParams): boolean;
type
  TTokenKind = (tkNull, tkVA, tkArgStrA, tkArgStrW, ArgNum);
const
  DEF_RADIX = 10;
  HEX_NUMS  = ['0' .. '9', 'a' .. 'f', 'A' .. 'F'];
var
  p, pValue: PChar;
  sValue: string;
//  radix: byte;
  num: int64;
  tokenKind: TTokenKind;
  strType: TIRConstType;
begin
  result := false;

  p := PChar(Params);

//  radix := DEF_RADIX;
  tokenKind := tkNull;

  while p^ <> #0 do
  begin
    if ord(p^) in [10, 13] then
    begin
      inc(p);
      continue;
    end;

    if (p[0] = 'L') and (p[1] = '"') then
    begin
      inc(p);
      tokenKind := tkArgStrW;
    end;
    if p^ = '"' then
    begin
      if tokenKind <> tkArgStrW then
        tokenKind := tkArgStrA;
      inc(p); // skip "
      pValue := p;
      while p^ <> '"' do
        inc(p);

      sValue := Copy(pValue, 0, p - pValue);

      inc(p); // skip "

      if tokenKind = tkArgStrA then
        strType := IRCT_STRING_A
      else
        strType := IRCT_STRING_W;

      CallParams.AddArg(TVDConstExpression.CreateString(sValue, strType));

      continue;
    end;

    if ((p[0] = '0') and (p[1] = 'x')) then
    begin
//      radix := 16;
      inc(p, 2);
      continue;
    end;
    if (p^ = '$') then
    begin
      inc(p);
//      radix := 16;
      continue;
    end;

    if p^ = '@' then
    begin
      inc(p);
      tokenKind := tkVA;
      continue;
    end;

    if CharInSet(p^, HEX_NUMS) then
    begin
      pValue := p;
      while CharInSet(p^, HEX_NUMS) do
        inc(p);

      sValue := '$' + Copy(pValue, 0, p - pValue);
      num := StrToInt(sValue);

      if tokenKind = tkVA then
        VA := num;

      // radix := DEF_RADIX;
      tokenKind := tkNull;
    end;

  end;
end;

function LiveCall(const Dbg: IVDDebuggerPlugin; VA: TVA; const Params: string): boolean;
var
  CallParams: TVDLiveCallParams; // auto-free
begin
  CallParams := TVDLiveCallParams.Create;
  ParseParams(Params, VA, CallParams);
  result := Dbg.LiveCall(VA, CallParams);
end;

{ TVDLiveCallParams }

constructor TVDLiveCallParams.Create;
begin
  inherited;
  FArgs := TList<IVDConstExpression>.Create;
end;

destructor TVDLiveCallParams.Destroy;
begin
  FArgs.Free;
  inherited;
end;

procedure TVDLiveCallParams.AddArg(Value: IVDConstExpression);
begin
  FArgs.Add(Value);
end;

function TVDLiveCallParams.GetArg(Index: int; out Value: IVDConstExpression): BOOL;
begin
  result := (Index >= 0) and (Index < FArgs.Count);
  if result then
    Value := FArgs[index];
end;

end.
