unit uExpression;

interface

uses
  System.SysUtils,
  VDAPI;

type
  TVDExpressionRec = record
    case TIRConstType of
      IRCT_SINT:
        (sint: int64);
      IRCT_UINT:
        (uint: uint64);
      IRCT_STRING, IRCT_STRING_A, IRCT_STRING_W:
        (str_utf8: pbyte; str_len: integer);
  end;

  TVDExpression = class(TInterfacedObject)
  end;

  TVDConstExpression = class(TVDExpression, IVDExpression, IVDConstExpression)
  public
    &Type: TIRConstType;
    Value: TVDExpressionRec;

    constructor CreateSInt(Value: int64); overload;
    constructor CreateSInt(const Value: string; RadixOverride: byte); overload;

    constructor CreateUInt(Value: uint64); overload;
    constructor CreateUInt(const Value: string; RadixOverride: byte); overload;

    constructor CreateString(const Value: string; &Type: TIRConstType);
    destructor Destroy; override;
  public
    function GetType: TIRConstType; stdcall;
    function GetSInt: int64; stdcall;
    function GetUInt: uint64; stdcall;
    function GetString: BSTR; stdcall;
  end;

  TVDUnaryExpression = class(TVDExpression, IVDExpression, IVDUnaryExpression)
  public
    Expr: IVDExpression;
    constructor Create(Expr: IVDExpression);
  public
    function GetExpr: IVDExpression; stdcall;
  end;

  TVDNegExpression = class(TVDUnaryExpression);

  TVDBinaryExpression = class(TVDExpression, IVDExpression, IVDBinaryExpression)
  public
    Left, Right: IVDExpression;
    constructor Create(Left, Right: IVDExpression);
  public
    function GetLeft: IVDExpression; stdcall;
    function GetRight: IVDExpression; stdcall;
  end;

  TVDAddExpression = class(TVDBinaryExpression);
  TVDSubExpression = class(TVDBinaryExpression);
  TVDMulExpression = class(TVDBinaryExpression);
  TVDDivExpression = class(TVDBinaryExpression);

function BinStrToInt64(const s: string): int64;
function ExprStrToInt(s: string; RadixOverride: byte): int64;

implementation

function BinStrToInt64(const s: string): int64;
var
  i: integer;
  m: uint64;
begin
  result := 0;
  m := 1;
  for i := s.Length downto 1 do
  begin
    if s.Chars[i - 1] = '1' then
      uint64(result) := uint64(result) or m;
    m := uint64(m) shl 1;
  end;
end;

function ExprStrToInt(s: string; RadixOverride: byte): int64;
var
  i: integer;
  radix: byte;
begin
  s := s.Trim;
  if s.StartsWith('0x') then
  begin
    i := 2;
    radix := 16;
  end
  else if s.StartsWith('0b') then
  begin
    i := 2;
    radix := 2;
  end
  else if s.StartsWith('$') then
  begin
    i := 1;
    radix := 16;
  end
  else
  begin
    i := 0;
    if RadixOverride = 0 then
      radix := 10 // default if RadixOverride is not set
    else
      radix := RadixOverride; // override radix
  end;

  if i = -1 then
    raise Exception.Create('Convert error: bad string format');

  // Remove prefix
  s := s.Substring(i);

  case radix of
    2:
      result := BinStrToInt64(s);
    10:
      result := StrToInt64(s);
    16:
      result := StrToInt64('$' + s);
  else
    raise Exception.Create('Unknown expression radix');
  end;
end;

{ TVDValue }

function TVDConstExpression.GetType: TIRConstType;
begin
  result := self.&Type;
end;

constructor TVDConstExpression.CreateSInt(Value: int64);
begin
  self.&Type := IRCT_SINT;
  self.Value.sint := Value;
end;

constructor TVDConstExpression.CreateSInt(const Value: string; RadixOverride: byte);
begin
  CreateSInt(ExprStrToInt(Value, RadixOverride));
end;

constructor TVDConstExpression.CreateUInt(Value: uint64);
begin
  self.&Type := IRCT_UINT;
  self.Value.uint := Value;
end;

constructor TVDConstExpression.CreateUInt(const Value: string; RadixOverride: byte);
begin
  CreateUInt(uint64(ExprStrToInt(Value, RadixOverride)));
end;

constructor TVDConstExpression.CreateString(const Value: string; &Type: TIRConstType);
var
  Bytes: TBytes;
  len: integer;
begin
  self.&Type := &Type;
  Bytes := TEncoding.UTF8.GetBytes(Value);
  len := Length(Bytes);

  self.Value.str_utf8 := nil;
  GetMem(self.Value.str_utf8, len);
  Move(Bytes[0], self.Value.str_utf8^, len);

  self.Value.str_len := len;
end;

destructor TVDConstExpression.Destroy;
begin
  case self.&Type of
    IRCT_STRING, IRCT_STRING_A, IRCT_STRING_W:
      if self.Value.str_utf8 <> nil then
      begin
        FreeMem(self.Value.str_utf8);
        self.Value.str_utf8 := nil;
        self.Value.str_len := 0;
      end;
  end;
  inherited;
end;

function TVDConstExpression.GetSInt: int64;
begin
  if self.&Type = IRCT_SINT then
    exit(self.Value.sint);
  raise Exception.Create('TVDValue error');
end;

function TVDConstExpression.GetString: BSTR;
var
  Bytes: TBytes;
begin
  case self.&Type of
    IRCT_NULL:
      result := '';
    IRCT_SINT:
      result := format('%x', [self.Value.sint]);
    IRCT_UINT:
      result := format('%x', [self.Value.uint]);
    IRCT_STRING, IRCT_STRING_A, IRCT_STRING_W:
      begin
        if self.Value.str_len = 0 then
          exit('');
        SetLength(Bytes, self.Value.str_len);
        Move(self.Value.str_utf8^, Bytes[0], self.Value.str_len);
        result := TEncoding.UTF8.GetString(Bytes);
      end
  else
    result := 'TVDValue<?>';
  end;
end;

function TVDConstExpression.GetUInt: uint64;
begin
  if self.&Type = IRCT_UINT then
    exit(self.Value.uint);
  raise Exception.Create('TVDValue error');
end;

{ TVDBinaryExpression }

constructor TVDBinaryExpression.Create(Left, Right: IVDExpression);
begin
  inherited Create;
  self.Left := Left;
  self.Right := Right;
end;

function TVDBinaryExpression.GetLeft: IVDExpression;
begin
  result := self.Left;
end;

function TVDBinaryExpression.GetRight: IVDExpression;
begin
  result := self.Right;
end;

{ TVDUnaryExpression }

constructor TVDUnaryExpression.Create(Expr: IVDExpression);
begin
  inherited Create;
  self.Expr := Expr;
end;

function TVDUnaryExpression.GetExpr: IVDExpression;
begin
  result := self.Expr;
end;

//
// Exports
//
procedure CreateConstExprFromUInt(Value: uint64; out Expr: IVDConstExpression); stdcall;
begin
  Expr := TVDConstExpression.CreateUInt(Value);
end;

exports
  CreateConstExprFromUInt;

end.
