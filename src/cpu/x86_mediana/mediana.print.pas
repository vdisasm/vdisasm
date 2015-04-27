unit mediana.print;

interface

uses
  VDAPI,
  mediana;

procedure print(
  const t: IVDVATextLayout;
  const ins: mediana.TInstruction; va: tva);

function GetRegPackedName(regId: UInt32): string;

implementation

uses
  System.SysUtils,
  mediana.print.regs;

const
  SDisassemblerError     = 'Disassembler Error';
  SBadSizeOfDisplacement = 'Bad size of displacement';

procedure AddCasual(const t: IVDVATextLayout; const str: string); inline;
begin
  t.AddText(BSTR_IN(str));
end;

procedure AddReg(const t: IVDVATextLayout; RegType: byte; RegSize: word;
  RegCode: byte);
var
  regId: UInt32;
  regName: string;
begin
  regId := (RegSize shl 16) or (RegType shl 8) or (RegCode);
  regName := GetRegPackedName(regId);

  // t.AddRegister(regId);
  t.AddText(BSTR_IN(regName), TTag.TAGID_REGISTER);
end;

function disp2int(const disp: TDISPLACEMENT): int64;
begin
  case disp.size of
    1:
      Result := disp.Value.d8;
    2:
      Result := disp.Value.d16;
    4:
      Result := disp.Value.d32;
    8:
      Result := disp.Value.d64;
  else
    raise Exception.Create(SBadSizeOfDisplacement);
  end;
end;

{$REGION 'dump_prefixes'}


procedure dump_prefixes(const t: IVDVATextLayout; ins: mediana.TInstruction);
var
  spfx: string;
begin
  spfx := '';

  if (ins.prefixes and INSTR_PREFIX_LOCK) <> 0 then
    spfx := 'lock';
  if (ins.prefixes and INSTR_PREFIX_REPZ) <> 0 then
    spfx := 'repz';
  if (ins.prefixes and INSTR_PREFIX_REPNZ) <> 0 then
    spfx := 'repnz';

  if spfx <> '' then
  begin
    t.AddText(BSTR_IN(spfx));
    t.AddText(' ');
  end;
end;
{$ENDREGION 'dump_prefixes'}
{$REGION 'dump_regs'}


function GetRegPackedName(regId: UInt32): string;
var
  name: punichar_t;
  op_size: word;
  _type, code: byte;
  p21: p21regs;
begin
  op_size := (regId shr 16) and $FFFF;
  _type := (regId shr 8) and $FF;
  code := (regId and $FF);

  case _type of
    REG_TYPE_GEN:
      begin
        p21 := regs_gen[op_size];
        if Assigned(p21) then
          name := p21[code]
        else
        begin
          Result := format('<invalid gen reg(opsize:%d;code:%d)>', [op_size, code]);
          exit;
        end;
      end;
    REG_TYPE_SEG:
      name := sregs[code];
    REG_TYPE_FPU:
      name := fregs[code];
    REG_TYPE_CR:
      name := cregs[code];
    REG_TYPE_DBG:
      name := dregs[code];
    REG_TYPE_TR:
      name := tregs[code];
    REG_TYPE_MMX:
      name := mregs[code];
    REG_TYPE_XMM:
      name := xregs[code];
  else
    name := nil;
  end;

  Result := name;
end;

procedure dump_reg(const t: IVDVATextLayout; op_size: uint16;
  const reg: TOPERAND.TREG); inline;
begin
  AddReg(t, reg.&type, op_size, reg.code);
end;

procedure dump_operand_reg(const t: IVDVATextLayout;
  const op: mediana.TOPERAND);
begin
  dump_reg(t, op.size, op.Value.reg);
end;
{$ENDREGION 'dump_regs'}
{$REGION 'ptr_size'}


function get_ptr_size(op: POPERAND): string;
begin
  case op.size of
    1:
      Result := 'byte ptr';
    2:
      Result := 'word ptr';
    4:
      Result := 'dword ptr';
    6:
      Result := 'fword ptr';
    8:
      Result := 'qword ptr';
    10:
      Result := 'tbyte ptr';
    16:
      Result := 'dqword ptr';
    14:
      Result := '14bytes ptr';
    28:
      Result := '28bytes ptr';
    94:
      Result := '94bytes ptr';
    108:
      Result := '108bytes ptr';
    512:
      Result := '512bytes ptr';
  else
    Result := 'strange ptr';
  end;
end;

function need_ptr_size(const ins: TInstruction): boolean;
var
  ops_count, i: integer;
begin
  Result := False;
  ops_count := 0;

  // get count
  for i := 0 to MAX_OPERANDS_COUNT - 1 do
    if ins.ops[i].IsPresent then
      inc(ops_count);

  if ops_count = 1 then
  begin
    Result := ins.ops[0].IsMem;
  end
  else // if (ops_count == 2)
  begin
    if ins.ops[0].IsMem and ins.ops[1].IsImm then
      Result := true
    else if (not ins.ops[0].IsMem) and ins.ops[1].IsMem then
      Result := true;
  end;
end;
{$ENDREGION 'ptr_size'}
{$REGION 'dump_operand_mem'}


procedure dump_operand_mem(const t: IVDVATextLayout; const ins: TInstruction;
  index: integer);
var
  op: POPERAND;
  bNeedPlus: boolean;
  bHasBase, bHasIndex, bHasDisp: boolean;
begin
  op := @ins.ops[index];

  bHasBase := op.Value.addr.HasBase;
  bHasIndex := op.Value.addr.HasIndex;
  bHasDisp := op.Value.addr.HasDisp;

  // ptr size
  if need_ptr_size(ins) then
  begin
    t.AddText(BSTR_IN(get_ptr_size(op)));
    t.AddText(' ');
  end;

  // SEG:[BASE+INDEX*SCALE+DISP]

  // seg reg
  if (op.flags and OPERAND_FLAG_SEG_OVERRIDE) <> 0 then
  begin
    AddReg(t, REG_TYPE_SEG, 0, op.Value.addr.seg);
    AddCasual(t, ':');
  end;

  AddCasual(t, '[');

  // base reg
  if bHasBase then
  begin
    AddReg(t, REG_TYPE_GEN, ins.addrsize, op.Value.addr.base);
  end;

  // index * scale
  with op.Value.addr do
  begin
    if bHasIndex then
    begin
      if bHasBase then
        AddCasual(t, '+');

      // index
      AddReg(t, REG_TYPE_GEN, ins.addrsize, op.Value.addr.index);

      // scale
      if scale <> 1 then
      begin
        AddCasual(t, '*');
        t.AddInteger(scale, 8, False);
      end;
    end;

    // disp
    if bHasDisp then
    begin

      // Special case (displace only, i.e. address)
      if (not bHasBase) and (not bHasIndex) then
        t.AddAddress(ins.disp.GetUnsigned)
      else
      begin
        bNeedPlus := bHasBase or bHasIndex;
        t.AddInteger(disp2int(ins.disp), 64, true, bNeedPlus);
      end;

    end;

  end;

  AddCasual(t, ']');
end;
{$ENDREGION 'dump_operand_mem'}
{$REGION 'dump_operand_imm'}


procedure dump_operand_imm(const t: IVDVATextLayout; const ins: TInstruction;
  const op: TOPERAND); // inline;
begin
  if not(op.size in [1, 2, 4, 8]) then
  begin
    t.AddText('?imm?');
    exit;
  end;

  if ins.HasBranch then
    t.AddAddress(op.GetImmediate)
  else
    t.AddInteger(op.GetImmediate, op.size * 8, False, False);
end;
{$ENDREGION 'dump_operand_imm'}
{$REGION 'dump_operand_dir'}


// FAR
procedure dump_operand_dir(const t: IVDVATextLayout; const op: TOPERAND);
var
  seg: uint16;
  ofs: UInt32;
begin
  case op.size of
    4:
      begin
        seg := op.Value.far_addr.far_addr.far_addr32.seg;
        ofs := op.Value.far_addr.far_addr.far_addr32.offset;
      end;
    6:
      begin
        seg := op.Value.far_addr.far_addr.far_addr48.seg;
        ofs := op.Value.far_addr.far_addr.far_addr48.offset;
      end;
  else
    begin
      t.AddText(SDisassemblerError);
      exit;
    end;
  end;

  t.AddInteger(seg, 16, False);
  t.AddText(':');
  t.AddInteger(ofs, 32, False);
end;
{$ENDREGION 'dump_operand_dir'}
{$REGION 'dump_operand'}


procedure dump_operand(const t: IVDVATextLayout; const ins: TInstruction;
  index: integer);
begin
  case (ins.ops[index].flags and OPERAND_TYPE_MASK) of
    OPERAND_TYPE_REG:
      dump_operand_reg(t, ins.ops[index]);
    OPERAND_TYPE_MEM:
      dump_operand_mem(t, ins, index);
    OPERAND_TYPE_IMM:
      dump_operand_imm(t, ins, ins.ops[index]);
    OPERAND_TYPE_DIR:
      dump_operand_dir(t, ins.ops[index]);
  end;
end;
{$ENDREGION 'dump_operand'}
{$REGION 'print'}


procedure print(
  const t: IVDVATextLayout;
  const ins: mediana.TInstruction; va: tva);
const
  HighOp = MAX_OPERANDS_COUNT - 1;
var
  i: integer;
  mnem: string;
begin
  dump_prefixes(t, ins);

  mnem := ins.mnemonic;
  t.AddText(BSTR_IN(mnem), TTag.TAGID_CODE); // mnemonic

  if ins.ops[0].IsPresent then
    t.AddText(' ');

  for i := 0 to HighOp do
  begin
    if (ins.ops[i].flags and OPERAND_FLAG_PRESENT) <> 0 then
      dump_operand(t, ins, i)
    else
      break;
    if (i <> HighOp) and ((ins.ops[i + 1].flags and OPERAND_FLAG_PRESENT) <> 0)
    then
      AddCasual(t, ', ');
  end;

end;
{$ENDREGION 'print'}

end.
