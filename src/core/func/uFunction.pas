unit uFunction;

interface

uses
  System.SysUtils,
  VDAPI,
  uIRCfg;

type
  TVDFunction = class(TInterfacedObject, IVDFunction)
  private
    FVA: TVA; // Function entry point.
    FCFG: IIRCFG;
    FExprPool: IIRExprPool;
    FCpu: IVDCpu;
  public
    constructor Create(VA: TVA; const Cpu: IVDCpu);
  public
    { VDAPI }
    function GetVA: TVA; stdcall;
    function GetSafeName: BSTR; stdcall;
    function GetCfg: IIRCFG; stdcall;
    function GetCpu: IVDCpu; stdcall;

    property VA: TVA read FVA;
  end;

implementation

{ TVDFunction }

constructor TVDFunction.Create(VA: TVA; const Cpu: IVDCpu);
begin
  self.FVA := VA;
  self.FCFG := TIrCFG.Create;
  self.FCpu := Cpu;
end;

function TVDFunction.GetCfg: IIRCFG;
begin
  Result := FCFG;
end;

function TVDFunction.GetCpu: IVDCpu;
begin
  Result := FCpu;
end;

function TVDFunction.GetSafeName: BSTR;
begin
  if not CoreGet().Names.Get(FVA, Result) then
    Result := Format('fn_%x', [FVA]);
end;

function TVDFunction.GetVA: TVA;
begin
  Result := FVA;
end;

end.
