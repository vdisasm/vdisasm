unit uEvaluate.TokenStream;

interface

uses
  System.SysUtils,
  VDLib.Text.Stream, // expand inlines
  VDLib.Text.TokenStream;

const
  TOKEN_BINNUM = 1;
  TOKEN_DECNUM = 2;
  TOKEN_HEXNUM = 3;

  // binary expr op expr
  TOKEN_ADD  = 10; // +
  TOKEN_SUB  = 11; // -
  TOKEN_MUL  = 12; // *
  TOKEN_DIV  = 13; // /
  TOKEN_IDIV = 15; // div
  TOKEN_MOD  = 16; // mod (%)

  TOKEN_NEG = 17; // unary, neg expr

  TOKEN_BITWISE_OR  = 20;
  TOKEN_BITWISE_XOR = 21;
  TOKEN_BITWISE_AND = 22;
  TOKEN_SHL         = 23;
  TOKEN_SHR         = 24;
  TOKEN_BITWISE_NOT = 25;

  TOKEN_LEFT_PARENTHESIS  = 30;
  TOKEN_RIGHT_PARENTHESIS = 31;

  TOKEN_DELPHI_AS = 40;
  TOKEN_DELPHI_IN = 41;
  TOKEN_DELPHI_IS = 42;
  TOKEN_DELPHI_AT = 43;

  TOKEN_EQUAL            = 50;
  TOKEN_NOT_EQUAL        = 51;
  TOKEN_LESS             = 52;
  TOKEN_GREATER          = 53;
  TOKEN_LESS_OR_EQUAL    = 54;
  TOKEN_GREATER_OR_EQUAL = 55;

  TOKEN_FUNC = 60;

  TOKEN_NUMBERS = [TOKEN_BINNUM, TOKEN_DECNUM, TOKEN_HEXNUM];

type
  TExprTokenStream = class(TAbstractTokenStream)
  private
    // Skip number of unknown radix (2,10,16). Detected automatically.
//    function SkipNumberUnknown(out radix: byte): boolean; overload;

    // Start from current position and skip number depending on radix.
    // Result is false if it's not legal number.
    function SkipNumber(radix: byte): boolean; overload;
  public
    function ReadToken: TAbstractTokenStream.TTokenId; override;
  end;

function TokenToRadix(token: integer; default: integer = 16): integer; inline;
function RadixToToken(radix: integer): integer; inline;

implementation

const
  SET_OF_BIN_CHARS = ['0' .. '1'];
  SET_OF_DEC_CHARS = ['0' .. '9'];
  SET_OF_HEX_CHARS = ['0' .. '9', 'A' .. 'F', 'a' .. 'f'];

function TokenToRadix(token: integer; default: integer = 16): integer; inline;
begin
  case token of
    TOKEN_BINNUM:
      result := 2;
    TOKEN_DECNUM:
      result := 10;
    TOKEN_HEXNUM:
      result := 16;
  else
    result := default;
  end;
end;

function RadixToToken(radix: integer): integer; inline;
begin
  case radix of
    2:
      result := TOKEN_BINNUM;
    10:
      result := TOKEN_DECNUM;
    16:
      result := TOKEN_HEXNUM;
  else
    result := 0;
  end;
end;

{ TExprTokenStream }

function TExprTokenStream.ReadToken: TAbstractTokenStream.TTokenId;
label
  found_bin_num, found_hex_num, try_unk_num;
var
  c: char;
//  radix: byte;
begin
  result := BAD_TOKEN;
  SkipWhitespaces();

  if not Peek(c) then
    exit;

  if c = '0' then
  begin
    if Peek(c, 1) then
    begin
      case c of
        'b', 'B':
          begin
            // binary number
            skip(2);
          found_bin_num:
            if SkipNumber(2) then
              exit(TOKEN_BINNUM);
          end;
        'x', 'X':
          begin
            // hex number
            skip(2);
          found_hex_num:
            if SkipNumber(16) then
              exit(TOKEN_HEXNUM);
          end;
      end;
    end;
    goto try_unk_num;
  end
  else if CharInSet(c, SET_OF_HEX_CHARS) then
  begin
  try_unk_num:
    // Don't try to guess number radix anymore as it leads to uncertain confusion.
    // And may be silently perform invalid calculation.
    //
    // Instead assume try decimal.
    // if SkipNumberUnknown(radix) then
    // exit(RadixToToken(radix));
    if SkipNumber(10) then
      exit(TOKEN_DECNUM);
  end
  else if c = '+' then
  begin
    skip;
    exit(TOKEN_ADD);
  end
  else if c = '-' then
  begin
    skip;
    exit(TOKEN_SUB);
  end
  else if c = '*' then
  begin
    skip;
    exit(TOKEN_MUL);
  end
  else if c = '/' then
  begin
    skip;
    exit(TOKEN_DIV);
  end
  else if c = '$' then
  begin
    skip;
    goto found_hex_num;
  end
  else if c = '(' then
  begin
    skip;
    exit(TOKEN_LEFT_PARENTHESIS);
  end
  else if c = ')' then
  begin
    skip;
    exit(TOKEN_RIGHT_PARENTHESIS);
  end;
end;

(*
function TExprTokenStream.SkipNumberUnknown(out radix: byte): boolean;
var
  c: char;
  has_2, has_10, has_16: boolean;
  procedure ResetRadix;
  begin
    has_2 := false;
    has_10 := false;
    has_16 := false;
  end;

begin
  {
    FFAA   ok
    1z     bad
  }

  radix := 0;

  // Use SET_OF_HEX_CHARS because it includes both BIN and DEC subsets.
  ResetRadix;
  c := #0;
  while Peek(c) do
  begin
    if CharInSet(c, SET_OF_BIN_CHARS) then
      has_2 := True
    else if CharInSet(c, SET_OF_DEC_CHARS) then
      has_10 := True
    else if CharInSet(c, SET_OF_HEX_CHARS) then
      has_16 := True
    else
    begin
      // Not digit. Allowed: EOF, whitespace, token
      if IsEOF or IsWhiteSpace(c) or (PeekToken <> BAD_TOKEN) then
        break;
      // Otherwise bad char.
      exit(false);
    end;
    skip;
  end;

  if has_16 then
    radix := 16
  else if has_10 then
    radix := 10
  else if has_2 then
    radix := 2
  else
    radix := 0;

  result := radix <> 0;
end;
*)

function TExprTokenStream.SkipNumber(radix: byte): boolean;
var
  c: char;
  goodcnt: integer;
  goodchar: boolean;
begin
  {
    $23   ok
    $2*$3 ok
    $2zz  wrong
  }
  c := #0;
  goodcnt := 0;
  goodchar := false;
  while Peek(c) do
  begin
    case radix of
      2:
        goodchar := CharInSet(c, SET_OF_BIN_CHARS);
      10:
        goodchar := CharInSet(c, SET_OF_DEC_CHARS);
      16:
        goodchar := CharInSet(c, SET_OF_HEX_CHARS);
    end;

    if goodchar then
    begin
      inc(goodcnt);
      skip;
      continue;
    end;

    {
      Not good char, it can be:
      - whitespace
      - eof
      - token
    }

    if goodcnt = 0 then
      exit(false); // no good chars at all

    if IsWhiteSpace(c) then
      exit(True);
    if PeekToken <> 0 then
      exit(True);

    exit(false);
  end;

  if IsEOF then
    exit(goodchar);

  raise Exception.Create('Unhandled case of SkipNumber');
end;

end.
