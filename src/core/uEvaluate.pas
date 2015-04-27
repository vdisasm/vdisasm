unit uEvaluate;

interface

uses
  System.SysUtils,
  uEvaluate.Parse,
  VDAPI;

function EvaluateExpr(
  const Core: IVDCore;
  const Expr: string;
  out Value: IVDConstExpression;
  Flags: TExpressionEvaluateFlags): BOOL;

implementation

function EvaluateExpr(
  const Core: IVDCore;
  const Expr: string;
  out Value: IVDConstExpression;
  Flags: TExpressionEvaluateFlags): BOOL;
begin
  Value := ParseExpressionTree(Core, Expr.Trim);
  Result := Assigned(Value);
end;

end.
