{
  Parse expression string into expression tree.
  Return nil or parsed expression.

  Problems:
  - parsing negative numbers (-3) is treated as (sub 3)
}
unit uEvaluate.Parse;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  uCore.Strings,
  uExpression,
  uEvaluate.TokenStream,
  VDAPI;

function ParseExpressionTree(
  const Core: IVDCore;
  const ExprStr: string
  ): IVDConstExpression;

implementation

uses
  VDLib.Text.Stream, // expand inlines
  VDLib.Text.TokenStream;

type
  TStackItemType = (si_token, si_va);

  TStackItemToken = record
  public
    Id: TAbstractTokenStream.TTokenId;
    Rgn: TAbstractTextStream.TTextRegion;
  end;

  TStackItem = record
  public
    constructor CreateVA(VA: TVA);
  public
    &Type: TStackItemType;
    case TStackItemType of
      si_token:
        (Token: TStackItemToken);
      si_va:
        (VA: TVA);
  end;

  TStackItemStack = TStack<TStackItem>;
  TRPN = TStackItemStack;
  TExprStack = TStack<IVDExpression>;

  TOperatorInfo = record
    LeftAssociative: boolean;
    Precedence: integer;
  end;

function GetOperatorInfo(Token: integer; out Info: TOperatorInfo): boolean;
begin
  // Currently Delphi-style.
  {
    * highest
    * 4   .... added NEG
    * 3   @, not
    * 2   *, /, div, mod, and, shl, shr, as
    * 1   +, -, or, xor
    * 0   =, <>, <, >, <=, >=, in, is
    * lowest
  }
  case Token of
    TOKEN_NEG:
      begin
        Info.LeftAssociative := false;
        Info.Precedence := 4;
        result := true;
      end;

    TOKEN_DELPHI_AT, TOKEN_BITWISE_NOT:
      begin
        Info.LeftAssociative := true;
        Info.Precedence := 3;
        result := true;
      end;
    TOKEN_MUL, TOKEN_DIV, TOKEN_IDIV, TOKEN_MOD,
      TOKEN_BITWISE_AND, TOKEN_SHL, TOKEN_SHR, TOKEN_DELPHI_AS:
      begin
        Info.LeftAssociative := true;
        Info.Precedence := 2;
        result := true;
      end;
    TOKEN_ADD, TOKEN_SUB, TOKEN_BITWISE_OR, TOKEN_BITWISE_XOR:
      begin
        Info.LeftAssociative := true;
        Info.Precedence := 1;
        result := true;
      end;
    TOKEN_EQUAL, TOKEN_NOT_EQUAL, TOKEN_LESS, TOKEN_GREATER,
      TOKEN_LESS_OR_EQUAL, TOKEN_GREATER_OR_EQUAL,
      TOKEN_DELPHI_IN, TOKEN_DELPHI_IS:
      begin
        Info.LeftAssociative := true;
        Info.Precedence := 0;
        result := true;
      end;
  else
    result := false;
  end;
end;

function TryResolveVAFromText(
  const Core: IVDCore;
  const ts: TExprTokenStream;
  out VA: TVA): boolean;
var
  sWord: string;
  sWordPos: TAbstractTextStream.TOffset;
  Value: IVDConstExpression;
begin
  // todo: now it can't parse expressions with names/regs, like
  // "entry+11"
  sWordPos := ts.Position;

  sWord := ts.ReadWord(false);

  // Try name.
  if Core.Names.GetByText(sWord, VA) then
    exit(True);

  // Try debugger register.
  if Core.DebugSession.GetRegisterValue(BSTR_IN(sWord), Value) then
  begin
    VA := Value.GetUInt;
    exit(True);
  end;

  // If nothing found restore position and break.
  ts.Position := sWordPos;
  exit(False);
end;

// Build expression in reverse polish notation as described in:
// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
{
  Testing neg:
  -2-3
  2*-5
  -2*-5
  2*3-1
}
procedure BuildRPN(
  const Core: IVDCore;
  const ts: TExprTokenStream;
  var Output: TRPN);
var
  Stack: TStackItemStack;
  o1, o2: TStackItem;
  i1, i2: TOperatorInfo;
  NegInsteadOfSub: boolean;
var
  ErrWord: string;
//  ErrPos: TExprTokenStream.TOffset;
var
  ResolvedVA: TVA;
  procedure PopFromStackToOutput;
  begin
    Output.Push(Stack.Pop);
  end;

begin
  NegInsteadOfSub := true;
  Stack := TStackItemStack.Create;
  try
    while not ts.IsEOF do
    begin
      // Try reading token.
      // If it failed try Name, Reg.
      if not ts.ReadToken(@o1.Token.Rgn, o1.Token.Id) then
      begin
        // If failed then break.
        if not TryResolveVAFromText(Core, ts, ResolvedVA) then
          break;
        // VA resolved
        Output.Push(TStackItem.CreateVA(ResolvedVA));
        continue;
      end;

      o1.&Type := si_token;
      //
      // We succeeded to read si_token.
      // Handle it.
      //

      case o1.Token.Id of
        // Number.
        TOKEN_BINNUM, TOKEN_DECNUM, TOKEN_HEXNUM:
          begin
            Output.Push(o1);
            NegInsteadOfSub := false;
          end;
        // Func.
        TOKEN_FUNC:
          begin
            Stack.Push(o1);
          end;
        // Binary operators.
        TOKEN_ADD, TOKEN_SUB, TOKEN_MUL, TOKEN_DIV:
          begin
            case o1.Token.Id of
              TOKEN_SUB:
                if NegInsteadOfSub then
                  o1.Token.Id := TOKEN_NEG;
            end;
            NegInsteadOfSub := true;

            // while there is an operator token, o2, at the top of the stack
            while (Stack.Count > 0) do
            begin
              o2 := Stack.Peek;
              if GetOperatorInfo(o1.Token.Id, i1) and GetOperatorInfo(o2.Token.Id, i2) then
              begin
                if (i1.LeftAssociative and (i1.Precedence = i2.Precedence)) or
                  (i1.Precedence < i2.Precedence) then
                begin
                  PopFromStackToOutput;
                  continue;
                end
              end;
              break;
            end;
            Stack.Push(o1);
          end;
        TOKEN_LEFT_PARENTHESIS:
          begin
            NegInsteadOfSub := true;

            Stack.Push(o1);
          end;
        TOKEN_RIGHT_PARENTHESIS:
          begin
            NegInsteadOfSub := false;

            o1.Token.Id := 0; // BAD_TOKEN
            while Stack.Count > 0 do
            begin
              o1 := Stack.Peek;
              if o1.Token.Id = TOKEN_LEFT_PARENTHESIS then
                break;
              PopFromStackToOutput;
            end;
            if (o1.Token.Id <> TOKEN_LEFT_PARENTHESIS) then
              raise Exception.Create('Mismatched parenthesis');
            Stack.Pop; // (
            if (Stack.Count > 0) and (Stack.Peek.Token.Id = TOKEN_FUNC) then
              PopFromStackToOutput;
          end;
      else
        raise Exception.Create('Unhandled token');
      end;
    end;

    //
    // No more tokens to read.
    //

    ts.SkipWhitespaces;
    if not ts.IsEOF then
    begin
      // ErrPos := ts.Position;
      ErrWord := ts.ReadWord(false);
      // raise Exception.CreateFmt('Failed to parse expression'#10#10'Offset: %d'#10'Text: "%s"', [ErrPos, ErrWord]);
      exit;
    end;

    while Stack.Count <> 0 do
    begin
      if
        (Stack.Peek.&Type = si_token) and
        (Stack.Peek.Token.Id in [TOKEN_LEFT_PARENTHESIS, TOKEN_RIGHT_PARENTHESIS]) then
      begin
        raise Exception.Create('Mismatched parenthesis');
      end;
      PopFromStackToOutput;
    end;
  finally
    Stack.Free;
  end;
end;

procedure RPNToString(
  const ts: TExprTokenStream;
  const RPN: TRPN;
  out result: string);
var
  arr: TArray<TStackItem>;
  sb: TStringBuilder;
  o1: TStackItem;
begin
  arr := RPN.ToArray;
  sb := TStringBuilder.Create;
  try
    for o1 in arr do
    begin
      case o1.&Type of
        si_token:
          begin
            case o1.Token.Id of
              TOKEN_NEG:
                sb.Append('neg');
            else
              sb.Append(ts.ReadTextFromRegion(o1.Token.Rgn));
            end;
          end;
        si_va:
          sb.Append(format('0x%x', [o1.VA]))
      end;
      sb.Append(';');
    end;
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

// Get expressions from RPN (right-to-left) and built expression tree.
// ts is needed to read text regions.
procedure RPNToExpr(
  const ts: TExprTokenStream;
  const RPN: TRPN;
  out Expr: IVDExpression);
var
  op1: TStackItem;
  left, right, x: IVDExpression;
  Text: string;
begin
  op1 := RPN.Pop;

  case op1.&Type of
    si_token:
      begin
        case op1.Token.Id of
          TOKEN_BINNUM, TOKEN_DECNUM, TOKEN_HEXNUM:
            begin
              Text := ts.ReadTextFromRegion(op1.Token.Rgn);
              Expr := TVDConstExpression.CreateUInt(Text, TokenToRadix(op1.Token.Id));
            end;
          // unary
          TOKEN_NEG:
            begin
              // -expr
              RPNToExpr(ts, RPN, x);
              Expr := TVDNegExpression.Create(x);
            end;
          // binary operators
          TOKEN_ADD, TOKEN_SUB, TOKEN_MUL, TOKEN_DIV { , TOKEN_IDIV } :
            begin
              RPNToExpr(ts, RPN, right); // first is right
              RPNToExpr(ts, RPN, left);  // second is left
              case op1.Token.Id of
                TOKEN_ADD:
                  Expr := TVDAddExpression.Create(left, right);
                TOKEN_SUB:
                  Expr := TVDSubExpression.Create(left, right);
                TOKEN_MUL:
                  Expr := TVDMulExpression.Create(left, right);
                TOKEN_DIV:
                  Expr := TVDDivExpression.Create(left, right);
              end;
            end;
        else
          raise Exception.Create('Unknown token id');
        end;
      end;
    si_va:
      Expr := TVDConstExpression.CreateUInt(op1.VA);
  else
    raise Exception.Create('Unhandled RPN item in RPNToExpr');
  end;
end;

function ResolveExprToConst(const x: IVDExpression): IVDConstExpression;
  procedure ResolveBin(const x: IVDExpression; out c0, c1: IVDConstExpression);
  var
    bin: IVDBinaryExpression;
  begin
    bin := x as IVDBinaryExpression;
    c0 := ResolveExprToConst(bin.GetLeft);
    c1 := ResolveExprToConst(bin.GetRight);
  end;

var
  ct: TClass;
  c0, c1: IVDConstExpression;
  i64: int64;
begin
  result := nil;

  if not Assigned(x) then
    exit;

  ct := (x as TVDExpression).ClassType;

  if ct = TVDConstExpression then
  begin
    result := TVDConstExpression(x); // const as is
  end
  else if ct = TVDNegExpression then
  begin
    c0 := ResolveExprToConst(TVDNegExpression(x).Expr);
    i64 := int64((c0 as IVDConstExpression).GetUInt);
    i64 := -i64;
    result := TVDConstExpression.CreateUInt(uint64(i64));
  end
  else if ct = TVDAddExpression then
  begin
    ResolveBin(x, c0, c1);
    result := TVDConstExpression.CreateUInt(c0.GetUInt + c1.GetUInt);
  end
  else if ct = TVDSubExpression then
  begin
    ResolveBin(x, c0, c1);
    result := TVDConstExpression.CreateUInt(c0.GetUInt - c1.GetUInt);
  end
  else if ct = TVDMulExpression then
  begin
    ResolveBin(x, c0, c1);
    result := TVDConstExpression.CreateUInt(c0.GetUInt * c1.GetUInt);
  end
  else if ct = TVDDivExpression then
  begin
    ResolveBin(x, c0, c1);
    result := TVDConstExpression.CreateUInt(c0.GetUInt div c1.GetUInt);
  end
  else
  begin
    CoreGet.Log.WriteLn('[ResolveConstant] Unhandled expression');
    exit(nil);
  end;
end;

function ParseExpressionTree(
  const Core: IVDCore;
  const ExprStr: string
  ): IVDConstExpression;
var
  ts: TExprTokenStream;
  Expr: IVDExpression;
  RPN: TRPN; // reversed polish notation of expression
{$IFDEF DEBUG}
  c: IVDCore;
  RPNStr: string;
{$ENDIF}
begin
  if ExprStr = '' then
    exit(nil);

{$IFDEF DEBUG}
  c := CoreGet();
  c.Log.Clear;
{$ENDIF}
  ts := TExprTokenStream.Create;
  try
    ts.LoadFromString(ExprStr);

    RPN := TRPN.Create;
    try
      BuildRPN(Core, ts, RPN);

      if (not Assigned(RPN)) or (RPN.Count = 0) then
      begin
{$IFDEF DEBUG}
        c.Log.WriteLn('RPN not built');
{$ENDIF}
        exit(nil);
      end;

{$IFDEF DEBUG}
      RPNToString(ts, RPN, RPNStr);
      c.Log.WriteLn('RPN built: ' + RPNStr);
{$ENDIF}
      RPNToExpr(ts, RPN, Expr);
{$IFDEF DEBUG}
      c.Log.WriteLn('RPN resolving ... ');
{$ENDIF}
      result := ResolveExprToConst(Expr);
    finally
      RPN.Free;
    end;
  finally
    ts.Free;
  end;
end;

{ TStackItem }

constructor TStackItem.CreateVA(VA: TVA);
begin
  self.&Type := si_va;
  self.VA := VA;
end;

end.
