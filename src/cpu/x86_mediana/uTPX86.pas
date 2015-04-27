unit uTPX86;

interface

uses
  System.SysUtils,
  VDAPI,

  mediana,
  mediana.print,

  uParseBasic;

type
  TCPU_BASE = class(TInterfacedObject, IVDTypeProviderBase, IVDCpu)
  private
    FBittness: integer;
    FDesc: string;
    FName: string;
    procedure Setup; virtual; abstract;
  public
    constructor Create;
    function DecodeInt(const c: IVDCore; VA: TVA; out ins: mediana.TINSTRUCTION): boolean;
  public
    { base }

    function Decode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
    function Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;

    { cpu }

    function GetName: BSTR; stdcall;

    function GetRegHigh: TIrRegisterId; stdcall;

    function GetRegInfo(
      Id: TIrRegisterId;
      { out } Name: PBSTR;
      { out } MapReg: PIrRegisterId;
      { out } MapOfs: PUInt;
      { out } Size: PUInt
      ): BOOL; stdcall;

    function ParseBasic(VA: TVA; Info: IVDCpuInstrBasicInfo): SIZE_T; stdcall;
    function ParseAdvanced(VA: TVA; Ctx: PVDAnalysisContext): SIZE_T; stdcall;
    function Nop(VA: TVA; Size: SIZE_T): BOOL; stdcall;
  end;

  TCPU_X16 = class(TCPU_BASE, IVDTypeProviderBase, IVDCpu)
  private
    procedure Setup; override;
  end;

  TCPU_X32 = class(TCPU_BASE, IVDTypeProviderBase, IVDCpu)
  private
    procedure Setup; override;
  end;

  TCPU_X64 = class(TCPU_BASE, IVDTypeProviderBase, IVDCpu)
  private
    procedure Setup; override;
  end;

implementation

uses
  uTPX86.Regs;

{ TCPU_X86 }

constructor TCPU_BASE.Create;
begin
  inherited;
  Setup;
end;

function TCPU_BASE.Decode(VA: TVA; Kind: TVDDataEncoding;
  Param: Pointer): SIZE_T;
var
  ins: mediana.TINSTRUCTION;
begin
  if not DecodeInt(CoreGet(), VA, ins) then
    exit(0);

  result := ins.length;

  case Kind of
    TVDDataEncoding.Size:
      exit;
    TVDDataEncoding.Text:
      begin
        if Param <> nil then
        begin
          mediana.print.print(IVDVATextLayout(Param), ins, VA);
          // IVDVATextLayout(Param).AddText(FDesc, TTag.TAGID_NONE);
        end;
      end;
  end;

end;

function TCPU_BASE.DecodeInt(const c: IVDCore; VA: TVA; out ins: mediana.TINSTRUCTION): boolean;
var
  buf: array [0 .. MAX_INSTRUCTION_LEN - 1] of byte;
  len, nopsCnt: integer;
  params: mediana.TDISASM_PARAMS;
  insTmp: mediana.TINSTRUCTION;
begin
  while True do
  begin
    len := c.VM.Read(VA, @buf[0], mediana.MAX_INSTRUCTION_LEN);
    if len = 0 then
      break;
    if mediana.medi_disassemble_default(@buf[0], len, VA, ins, params, FBittness) = 0 then
      break;

    // Few nops?
    {
      if ins.Id = ID_NOP then
      begin
      nopsCnt := 0;
      insTmp := ins;
      while insTmp.Id = ID_NOP do
      begin
      inc(nopsCnt);
      inc(VA, insTmp.length);
      len := c.VM.Read(VA, @buf[0], mediana.MAX_INSTRUCTION_LEN);
      if len = 0 then
      break;
      if mediana.medi_disassemble_default(@buf[0], len, VA, insTmp, params, FBittness) = 0 then
      break;
      end;
      ins.length := nopsCnt;
      end;
    }

    break;
  end;
  result := ins.length <> 0;
end;

function TCPU_BASE.GetName: BSTR;
begin
  result := FName;
end;

function TCPU_BASE.GetRegHigh: TIrRegisterId;
begin
  result := 0;
  // result := length(uTPX86.Regs.Regs) - 1;
end;

function TCPU_BASE.GetRegInfo;
begin
  exit(False);
end;

function TCPU_BASE.Nop(VA: TVA; Size: SIZE_T): BOOL;
var
  buf: TBytes;
begin
  SetLength(buf, Size);
  FillChar(buf[0], Size, $90);
  result := CoreGet.VM.Write(VA, @buf[0], Size) = Size;
end;

function TCPU_BASE.ParseAdvanced(VA: TVA; Ctx: PVDAnalysisContext): SIZE_T;
begin
  // todo: not implemented
  result := 0;
end;

function TCPU_BASE.ParseBasic(VA: TVA; Info: IVDCpuInstrBasicInfo): SIZE_T;
var
  Ctx: TParseCtx;
begin
  Ctx.c := CoreGet();
  Ctx.VA := VA;
  Ctx.Info := Info;
  Ctx.dis := DecodeInt;
  result := uParseBasic.DoParseBasic(Ctx)
end;

function TCPU_BASE.Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
begin
  result := SIZE_T(-1);
end;

{ TCPU_X16 }

procedure TCPU_X16.Setup;
begin
  FName := VDAPI.TCpuName.X16;
  FBittness := 16;
  FDesc := 'x16 code';
end;

{ TCPU_X32 }

procedure TCPU_X32.Setup;
begin
  FName := TCpuName.X32;
  FBittness := 32;
  FDesc := 'x86 code';
end;

{ TCPU_X64 }

procedure TCPU_X64.Setup;
begin
  FName := TCpuName.X64;
  FBittness := 64;
  FDesc := 'x64 code';
end;

end.
