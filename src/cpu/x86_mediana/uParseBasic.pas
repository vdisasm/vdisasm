unit uParseBasic;

interface

uses
  System.SysUtils,
  VDAPI,
  mediana;

type
  TDisAsmFunc = function(const c: IVDCore; VA: TVA; out ins: TINSTRUCTION): boolean of object;

  TParseCtx = record
    c: IVDCore;
    // ins: mediana.TINSTRUCTION;
    VA: TVA;
    Info: IVDCpuInstrBasicInfo;
    dis: TDisAsmFunc;
  end;

function DoParseBasic(var ctx: TParseCtx): SIZE_T;

implementation

// Checks special case of disassembly.
// Must return true if it changed something.
// It is allowed to modify assembly code.
type
  TOptimizator = function(var ctx: TParseCtx): boolean;

  // ---------------------------------------------------------------------------
  // uncond. jump to jump to jump...
  // function dish_jmp_jmp_jmp(var ctx: TParseCtx): TDisHandlerStatus;
  // var
  // ins: TINSTRUCTION;
  // VA, newva: uint64;
  // cond: boolean;
  // level: integer;
  // begin
  // result := dhs_null;
  //
  // VA := ctx.VA;
  // level := 0;
  // while True do
  // begin
  // if not ctx.dis(ctx.c, VA, ins) then
  // break;
  // if not ins.ChangesIP(VA, newva, cond) then
  // break;
  // if cond then
  // break;
  // if ins.id = ID_CALL then
  // break;
  // // changes va, not cond
  // VA := newva;
  // inc(level);
  // end;
  // if level > 1 then
  // begin
  // ctx.ins.id := ID_JMP;
  // ctx.ins.ops[0].SetImmediate(VA, 8);
  // {$IFDEF DEBUG}
  // ctx.c.Log.WriteLn(Format('[%x] jmp->jmp detected to %x', [ctx.VA, VA]));
  // {$ENDIF}
  // result := dhs_changed;
  // end;
  // end;

procedure write_nops(var ctx: TParseCtx; VA: TVA; size: integer);
const
  nops: array [0 .. 15] of byte = (
    $90, $90, $90, $90, $90, $90, $90, $90,
    $90, $90, $90, $90, $90, $90, $90, $90);
begin
  if size > length(nops) then
    raise Exception.Create('[write] nop limit');
  ctx.c.VM.Write(VA, @nops[0], size);
end;

procedure write_jmp(var ctx: TParseCtx; src, dst: TVA; srcSize, jmpMaxSize: integer);
type
  TShortJump = packed record
    _eb: byte;
    _d8: int8;
  end;

  TLongJump = packed record
    _e9: byte;
    _d32: int32;
  end;

  TJmp = record
    case byte of
      0:
        (j_sh: TShortJump);
      1:
        (j_lg: TLongJump);
  end;
var
  delta, size: integer;
  jmp: TJmp;
begin
  delta := dst - (src + srcSize);
  if delta < 128 then
  begin
    // short jump
    jmp.j_sh._eb := $EB;
    jmp.j_sh._d8 := delta;
    size := sizeof(TShortJump);
  end
  else
  begin
    // long jump
    jmp.j_lg._e9 := $E9;
    jmp.j_lg._d32 := delta;
    size := sizeof(TLongJump);
  end;
  if size > jmpMaxSize then
    raise Exception.Create('[write] not enough place for jump');
  ctx.c.VM.Write(src, @jmp, size);
end;

// ---------------------------------------------------------------------------
// controversal conditional jump, mean unconditional jump
function opt_jcc_to_jmp(var ctx: TParseCtx): boolean;
var
  curIns, nxtIns: TINSTRUCTION;
  nxtva, newva1, newva2: uint64;
  cond1, cond2: boolean;
begin
  // Two condintional with mutual exclusive conditions. Jumps pointing to same
  // destination. No gaps between jumps. First jump can be replaced to
  // unconditional jump to target because it will always fall-through to second
  // jump. Second jump can be referenced from other places also, so it cannot
  // be wiped.

  result := false;

  if ctx.dis(ctx.c, ctx.VA, curIns) then
  begin
    nxtva := ctx.VA + curIns.length;
    if
      curIns.ChangesIP(newva1, cond1) and
      ctx.dis(ctx.c, nxtva, nxtIns) and
      nxtIns.ChangesIP(newva2, cond2) and
      (newva1 = newva2) and
      AreJumpCounterparts(curIns.id, nxtIns.id) then
    begin
      // jcc/!jcc -> jmp (invariant jump)
      write_jmp(ctx, ctx.VA, newva1, curIns.length, curIns.length);
{$IFDEF DEBUG}
      ctx.c.Log.WriteLn(Format('[%x] invariant jump condition detected', [ctx.VA]));
{$ENDIF}
      result := True;
    end;
  end;
end;
// ---------------------------------------------------------------------------

const
  Optimizators: array [0 .. 0] of TOptimizator = (
    opt_jcc_to_jmp
    );

procedure DoPeepHoleOptimization(var ctx: TParseCtx);
var
  opt: TOptimizator;
begin
  for opt in Optimizators do
    if Assigned(opt) then
      opt(ctx);
end;

// ---------------------------------------------------------------------------
procedure DoMemAccess(const ctx: TParseCtx; const curIns: TINSTRUCTION);
var
  op: POPERAND;
  ad: PADDR;
  i: integer;
  disp: Int64;
begin
  for i := 0 to high(curIns.ops) do
  begin
    op := @curIns.ops[i];
    if op.IsPresent then
    begin
      // memory?
      if op.IsMem then
      begin
        ad := @op.value.addr;

        // if there is displacement
        if (ad.HasDisp) then
        begin
          // [disp only]
          // if (not ad.HasBase) and ((not ad.HasIndex) or (ad.scale = 0)) then
          begin
            // normally need to know index/scale ranges to make accurate
            // decision
            disp := curIns.disp.GetUnsigned;
            ctx.Info.AddMemoryAccess(disp, op.size, TMemoryAccessKinds.MEMACC_NULL);
{$IFDEF DEBUG}
            // ctx.c.Log.WriteLn(Format('[%x] mem acc size %d', [ctx.VA, op.size]));
{$ENDIF}
          end;

        end;
      end
      // immediate, potentially memory?
      else if op.IsImm then
      begin
        ctx.Info.AddMemoryAccess(op.GetImmediate, 0, TMemoryAccessKinds.MEMACC_NOT_SURE);
      end;

    end;
  end;
end;
// ---------------------------------------------------------------------------

function DoParseBasic;
var
  curIns: TINSTRUCTION;
  newva: uint64;
  IsConditional: boolean;
  brKind: TCpuInstrBranchKind;
  // handler: TDisHandler;
  lpADDR: mediana.PADDR;
  disp: uint64;
var
  bBranchFromPtr: boolean;
begin
  DoPeepHoleOptimization(ctx);

  if not ctx.dis(ctx.c, ctx.VA, curIns) then
    exit(0);

  bBranchFromPtr := false;

  DoMemAccess(ctx, curIns);

  // if this instructions changes control flow for sure
  if curIns.ChangesIP(newva, IsConditional) then
  begin
    // defaults
    case curIns.id of
      ID_CALL, ID_CALLF:
        brKind := TCpuInstrBranchKind.Call;
      ID_RETN, ID_RETF:
        brKind := TCpuInstrBranchKind.Return;
    else
      brKind := TCpuInstrBranchKind.Jump;
    end;
    ctx.Info.Conditional := IsConditional;
    if newva = 0 then
      newva := BAD_VA;

    // special case
    // call/jmp [disp]
    if
      (not IsConditional) and
      (curIns.id in [ID_CALL, ID_JMP]) and
      (curIns.ops[0].IsMem) then
    begin
      lpADDR := @curIns.ops[0].value.addr;
      if (lpADDR.HasDisp and (not lpADDR.HasBase) and (not lpADDR.HasIndex)) then
      begin
        disp := curIns.disp.GetUnsigned;
        if disp <> 0 then
        begin
          bBranchFromPtr := True;
          newva := disp; // address of pointer to branch
        end;
      end;
    end;

    // special case: simple switch table
    // if "jmp [reg*4/8+displ]" -> switch
    if
      (not IsConditional) and
      (curIns.id = ID_JMP) and
      (curIns.ops[0].IsMem) then
    begin
      lpADDR := @curIns.ops[0].value.addr;
      if (lpADDR.scale = ctx.c.GetData.AddressSize) then
      begin
        disp := curIns.disp.GetUnsigned;
        if disp <> 0 then
        begin
          // Switch table.
          ctx.Info.AddSwitchTableJump(ctx.VA, disp, 0);
          exit(curIns.length);
        end;
      end;
    end;

    ctx.Info.AddBranch(brKind, newva, bBranchFromPtr);

    exit(curIns.length);
  end;

  exit(curIns.length);
end;

end.
