{
  *
  *   Mediana disassembler headers.
  *
  *   http://mediana.sourceforge.net/
  *
  *   Converted for VDisAsm project:
  *   http://vdisasm.com/
  *
}

unit mediana;

interface

{$ALIGN 4}

// Defines and structs for operand.

{$REGION 'Consts'}
const
  // OPERAND.flags' values:
  OPERAND_TYPE_REG  = $01;
  OPERAND_TYPE_MEM  = $02;
  OPERAND_TYPE_IMM  = $04;
  OPERAND_TYPE_DIR  = $08;
  OPERAND_TYPE_MASK = $0F;

  OPERAND_FLAG_PRESENT      = $0010;
  OPERAND_FLAG_REL          = $0020;
  OPERAND_FLAG_SEG_OVERRIDE = $0040;

  OPERAND_FLAG_READ        = $0080;
  OPERAND_FLAG_WRITE       = $0100;
  OPERAND_FLAG_EXEC        = $0200;
  OPERAND_FLAG_ACCESS_MASK = $0380;

  // OPERAND.ADDR.mod's bits:
  ADDR_MOD_BASE = $1;
  ADDR_MOD_IDX  = $2;
  ADDR_MOD_DISP = $4;

  // OPERAND.REG.type's values:
  REG_TYPE_GEN = $0;
  REG_TYPE_SEG = $1;
  REG_TYPE_CR  = $2;
  REG_TYPE_DBG = $3;
  REG_TYPE_TR  = $4;
  REG_TYPE_FPU = $5;
  REG_TYPE_MMX = $7;
  REG_TYPE_XMM = $8;
  REG_TYPE_YMM = $9;

  // OPERAND.REG.code's values (GPR):
  REG_CODE_AX = $0;
  REG_CODE_CX = $1;
  REG_CODE_DX = $2;
  REG_CODE_BX = $3;
  REG_CODE_SP = $4;
  REG_CODE_BP = $5;
  REG_CODE_SI = $6;
  REG_CODE_DI = $7;

  REG_CODE_R8  = $8;
  REG_CODE_R9  = $9;
  REG_CODE_R10 = $A;
  REG_CODE_R11 = $B;
  REG_CODE_R12 = $C;
  REG_CODE_R13 = $D;
  REG_CODE_R14 = $E;
  REG_CODE_R15 = $F;

  // Since 1byte registers may designate high bytes of ax, cx, dx and bx (ah, ch, dh and bh),
  // I use special constants for them.
  REG_CODE_AH = $10;
  REG_CODE_CH = $11;
  REG_CODE_DH = $12;
  REG_CODE_BH = $13;

  REG_CODE_IP  = $14;
  REG_CODE_EFL = $15; // I don't work with this register, it is added for completeness.

  // OPERAND.REG.code's values (segment registers):
  SREG_CODE_ES = $0;
  SREG_CODE_CS = $1;
  SREG_CODE_SS = $2;
  SREG_CODE_DS = $3;
  SREG_CODE_FS = $4;
  SREG_CODE_GS = $5;

  // OPERAND.REG.code's values (control registers):
  CREG_CODE_CR0 = $0;
  CREG_CODE_CR1 = $1;
  CREG_CODE_CR2 = $2;
  CREG_CODE_CR3 = $3;
  CREG_CODE_CR4 = $4;
  CREG_CODE_CR5 = $5;
  CREG_CODE_CR6 = $6;
  CREG_CODE_CR7 = $7;

  // OPERAND.REG.code's values (debug registers):
  DREG_CODE_DR0 = $0;
  DREG_CODE_DR1 = $1;
  DREG_CODE_DR2 = $2;
  DREG_CODE_DR3 = $3;
  DREG_CODE_DR4 = $4;
  DREG_CODE_DR5 = $5;
  DREG_CODE_DR6 = $6;
  DREG_CODE_DR7 = $7;

  // OPERAND.REG.code's values (test registers):
  TREG_CODE_TR0 = $0;
  TREG_CODE_TR1 = $1;
  TREG_CODE_TR2 = $2;
  TREG_CODE_TR3 = $3;
  TREG_CODE_TR4 = $4;
  TREG_CODE_TR5 = $5;
  TREG_CODE_TR6 = $6;
  TREG_CODE_TR7 = $7;

  // OPERAND.REG.code's values (FPU registers):
  FREG_CODE_ST0 = $0;
  FREG_CODE_ST1 = $1;
  FREG_CODE_ST2 = $2;
  FREG_CODE_ST3 = $3;
  FREG_CODE_ST4 = $4;
  FREG_CODE_ST5 = $5;
  FREG_CODE_ST6 = $6;
  FREG_CODE_ST7 = $7;

  // OPERAND.REG.code's values (MMX registers):
  MREG_CODE_MM0 = $0;
  MREG_CODE_MM1 = $1;
  MREG_CODE_MM2 = $2;
  MREG_CODE_MM3 = $3;
  MREG_CODE_MM4 = $4;
  MREG_CODE_MM5 = $5;
  MREG_CODE_MM6 = $6;
  MREG_CODE_MM7 = $7;

  // OPERAND.REG.code's values (XMM registers):
  XREG_CODE_XMM0 = $0;
  XREG_CODE_XMM1 = $1;
  XREG_CODE_XMM2 = $2;
  XREG_CODE_XMM3 = $3;
  XREG_CODE_XMM4 = $4;
  XREG_CODE_XMM5 = $5;
  XREG_CODE_XMM6 = $6;
  XREG_CODE_XMM7 = $7;

  XREG_CODE_XMM8  = $8;
  XREG_CODE_XMM9  = $9;
  XREG_CODE_XMM10 = $A;
  XREG_CODE_XMM11 = $B;
  XREG_CODE_XMM12 = $C;
  XREG_CODE_XMM13 = $D;
  XREG_CODE_XMM14 = $E;
  XREG_CODE_XMM15 = $F;

  // OPERAND.REG.code's values (YMM registers):
  YREG_CODE_YMM0 = $0;
  YREG_CODE_YMM1 = $1;
  YREG_CODE_YMM2 = $2;
  YREG_CODE_YMM3 = $3;
  YREG_CODE_YMM4 = $4;
  YREG_CODE_YMM5 = $5;
  YREG_CODE_YMM6 = $6;
  YREG_CODE_YMM7 = $7;

  YREG_CODE_YMM8  = $8;
  YREG_CODE_YMM9  = $9;
  YREG_CODE_YMM10 = $A;
  YREG_CODE_YMM11 = $B;
  YREG_CODE_YMM12 = $C;
  YREG_CODE_YMM13 = $D;
  YREG_CODE_YMM14 = $E;
  YREG_CODE_YMM15 = $F;
{$ENDREGION}

type
  size_t = NativeUInt;
  unichar_t = AnsiChar;
  punichar_t = PAnsiChar;

{$REGION 'TAddr'}
  TADDR = record
    seg: uint8;
    &mod: uint8;
    base: uint8;
    index: uint8;
    scale: uint8;
    function HasBase: boolean; inline;
    function HasIndex: boolean; inline;
    function HasDisp: boolean; inline;
  end;

  PADDR = ^TADDR;
{$ENDREGION}
{$REGION 'TOperand'}
 TOPERAND = record
 public type

   TREG = record
     code: uint8;
     &type: uint8;
     class operator Equal(const a,b: TReg): boolean; inline;
   end;

   TIMM = record
    size: uint8;
    offset: uint8;
    val: record case byte of
      8: (imm8: uint8);
      16: (imm16: uint16);
      32: (imm32: uint32);
      64: (imm64: uint64);
    end;
   end;

   TFAR_ADDR32 = record
     offset: uint16;
     seg: uint16;
   end;

   TFAR_ADDR48 = record
     offset: uint32;
     seg: uint16;
   end;

   TFAR_ADDR = record
     offset: uint8;
     far_addr: record case byte of
       32: (far_addr32: TFAR_ADDR32);
       48: (far_addr48: TFAR_ADDR48);
     end;
   end;

 public
   value: record case byte of
       0: (reg: TREG);
       1: (imm: TIMM);
       2: (far_addr: TFAR_ADDR);
       3: (addr: TADDR);
       4: (raw: packed array [0..SizeOf(TFAR_ADDR)-1] of byte);
   end;

   size: uint16; //Fuck... I need 16_t only for 'stx' size qualifier.
   flags: uint16;

   function IsPresent: boolean; inline;
   function IsReg: boolean; inline;
   function IsImm: boolean; inline;
   function IsFarAddr: boolean; inline;
   function IsMem: boolean; inline;
   function GetImmediate: UInt64;
   // Size:0 to use self.Size
   procedure SetImmediate(Value: UInt64; Size: uint16);
  end;

  POPERAND = ^TOPERAND;
{$ENDREGION}
{$REGION 'Consts (instr)'}
  const

(************************
* Defines and structs for
* instruction.
*************************
*)
    // INSTRUCTION.flag's values:
    INSTR_FLAG_MODRM       = $0001;
    INSTR_FLAG_SIB         = $0002;
    INSTR_FLAG_SF_PREFIXES = $0004;
    INSTR_FLAG_IOPL        = $0008;
    INSTR_FLAG_RING0       = $0010;
    INSTR_FLAG_SERIAL      = $0020;
    INSTR_FLAG_UNDOC       = $0040;

    // INSTRUCTION.prefixes values.
    // Segment prefixes:
    INSTR_PREFIX_CS = $0001;
    INSTR_PREFIX_DS = $0002;
    INSTR_PREFIX_ES = $0004;
    INSTR_PREFIX_SS = $0008;
    INSTR_PREFIX_FS = $0010;
    INSTR_PREFIX_GS = $0020;
    // Segment prefixes mask:
    INSTR_PREFIX_SEG_MASK = $003F;
    // Repeat prefixes:
    INSTR_PREFIX_REPZ  = $0040;
    INSTR_PREFIX_REPNZ = $0080;
    // Repeat prefixes mask:
    INSTR_PREFIX_REP_MASK = $00C0;
    // Size override prefixes:
    INSTR_PREFIX_OPSIZE   = $0100;
    INSTR_PREFIX_ADDRSIZE = $0200;
    // Operand size prefixes mask:
    INSTR_PREFIX_SIZE_MASK = $0300;
    // REX prefix:
    INSTR_PREFIX_REX = $0400;
    // LOCK prefix:
    INSTR_PREFIX_LOCK = $0800;

    // INSTRUCTION.rex's bits:
    PREFIX_REX_W = $8;
    PREFIX_REX_R = $4;
    PREFIX_REX_X = $2;
    PREFIX_REX_B = $1;

    MAX_MNEMONIC_LEN    = $0C;
    MAX_INSTRUCTION_LEN = $0F;
    MAX_OPERANDS_COUNT  = $03;

    // INSTRUCTION.xxx_flags' bits:
    EFLAG_C = $01;
    EFLAG_P = $02;
    EFLAG_A = $04;
    EFLAG_Z = $08;
    EFLAG_S = $10;
    EFLAG_I = $20;
    EFLAG_D = $40;
    EFLAG_O = $80;

    FPU_FLAG0 = $01;
    FPU_FLAG1 = $02;
    FPU_FLAG2 = $04;
    FPU_FLAG3 = $08;
{$ENDREGION 'Consts (instr)'}
{$REGION 'TDisplacement'}
type
  TDISPLACEMENT = record
    size: uint8;
    offset: uint8;
    value: record case byte of
      8:  (d8:  int8);
      16: (d16: int16);
      32: (d32: int32);
      64: (d64: int64);
    end;
    function GetSigned: Int64;
    function GetUnsigned: UInt64;
  end;
{$ENDREGION 'TDisplacement'}
{$REGION 'Instruction IDs'}
type
  {$MINENUMSIZE 2}
  TInstructionId =
  (
     ID_NULL        = $0, //For internal use.
     ID_SWITCH      = $1,

     ID_0F          = $2, //Prefixes. For internal use.
     ID_66          = $3,
     ID_67          = $4,
     ID_CS          = $5,
     ID_DS          = $6,
     ID_ES          = $7,
     ID_FS          = $8,
     ID_GS          = $9,
     ID_LOCK        = $A,
     ID_REPNZ       = $B,
     ID_REPZ        = $C,
     ID_SS          = $D,

     ID_AAA         = $E,
     ID_AAD         = $F,
     ID_AAM         = $10,
     ID_AAS         = $11,
     ID_ADC         = $12,
     ID_ADD         = $13,
     ID_ADDPD       = $14,
     ID_ADDPS       = $15,
     ID_ADDSD       = $16,
     ID_ADDSS       = $17,
     ID_ADDSUBPD    = $18,
     ID_ADDSUBPS    = $19,
     ID_AND         = $1A,
     ID_ANDNPD      = $1B,
     ID_ANDNPS      = $1C,
     ID_ANDPD       = $1D,
     ID_ANDPS       = $1E,
     ID_ARPL        = $1F,
     ID_BLENDPD     = $20,
     ID_BLENDPS     = $21,
     ID_BLENDVPD    = $22,
     ID_BLENDVPS    = $23,
     ID_BOUND       = $24,
     ID_BSF         = $25,
     ID_BSR         = $26,
     ID_BSWAP       = $27,
     ID_BT          = $28,
     ID_BTC         = $29,
     ID_BTR         = $2A,
     ID_BTS         = $2B,
     ID_CALL        = $2C,
     ID_CALLF       = $2D,
     ID_CBW         = $2E,
     ID_CLC         = $2F,
     ID_CLD         = $30,
     ID_CLFLUSH     = $31,
     ID_CLGI        = $32,
     ID_CLI         = $33,
     ID_CLTS        = $34,
     ID_CMC         = $35,
     ID_CMOVA       = $36,
     ID_CMOVAE      = $37,
     ID_CMOVB       = $38,
     ID_CMOVBE      = $39,
     ID_CMOVG       = $3A,
     ID_CMOVGE      = $3B,
     ID_CMOVL       = $3C,
     ID_CMOVLE      = $3D,
     ID_CMOVNO      = $3E,
     ID_CMOVNP      = $3F,
     ID_CMOVNS      = $40,
     ID_CMOVNZ      = $41,
     ID_CMOVO       = $42,
     ID_CMOVP       = $43,
     ID_CMOVS       = $44,
     ID_CMOVZ       = $45,
     ID_CMP         = $46,
     ID_CMPPD       = $47,
     ID_CMPPS       = $48,
     ID_CMPS        = $49,
     ID_CMPSD       = $4A,
     ID_CMPSS       = $4B,
     ID_CMPXCHG     = $4C,
     ID_CMPXCHG8B   = $4D,
     ID_COMISD      = $4E,
     ID_COMISS      = $4F,
     ID_CPUID       = $50,
     ID_CRC32       = $51,
     ID_CVTDQ2PD    = $52,
     ID_CVTDQ2PS    = $53,
     ID_CVTPD2DQ    = $54,
     ID_CVTPD2PI    = $55,
     ID_CVTPD2PS    = $56,
     ID_CVTPI2PD    = $57,
     ID_CVTPI2PS    = $58,
     ID_CVTPS2DQ    = $59,
     ID_CVTPS2PD    = $5A,
     ID_CVTPS2PI    = $5B,
     ID_CVTSD2SI    = $5C,
     ID_CVTSD2SS    = $5D,
     ID_CVTSI2SD    = $5E,
     ID_CVTSI2SS    = $5F,
     ID_CVTSS2SD    = $60,
     ID_CVTSS2SI    = $61,
     ID_CVTTPD2DQ   = $62,
     ID_CVTTPD2PI   = $63,
     ID_CVTTPS2DQ   = $64,
     ID_CVTTPS2PI   = $65,
     ID_CVTTSD2SI   = $66,
     ID_CVTTSS2SI   = $67,
     ID_CWD         = $68,
     ID_DAA         = $69,
     ID_DAS         = $6A,
     ID_DEC         = $6B,
     ID_DIV         = $6C,
     ID_DIVPD       = $6D,
     ID_DIVPS       = $6E,
     ID_DIVSD       = $6F,
     ID_DIVSS       = $70,
     ID_DPPD        = $71,
     ID_DPPS        = $72,
     ID_EMMS        = $73,
     ID_ENTER       = $74,
     ID_EXTRACTPS   = $75,
     ID_EXTRQ       = $76,
     ID_F2XM1       = $77,
     ID_FABS        = $78,
     ID_FADD        = $79,
     ID_FADDP       = $7A,
     ID_FBLD        = $7B,
     ID_FBSTP       = $7C,
     ID_FCHS        = $7D,
     ID_FCMOVA      = $7E,
     ID_FCMOVB      = $7F,
     ID_FCMOVBE     = $80,
     ID_FCMOVNB     = $81,
     ID_FCMOVNU     = $82,
     ID_FCMOVNZ     = $83,
     ID_FCMOVU      = $84,
     ID_FCMOVZ      = $85,
     ID_FCOM        = $86,
     ID_FCOM2       = $87,
     ID_FCOMI       = $88,
     ID_FCOMIP      = $89,
     ID_FCOMP       = $8A,
     ID_FCOMP3      = $8B,
     ID_FCOMP5      = $8C,
     ID_FCOMPP      = $8D,
     ID_FCOS        = $8E,
     ID_FDECSTP     = $8F,
     ID_FDIV        = $90,
     ID_FDIVP       = $91,
     ID_FDIVR       = $92,
     ID_FDIVRP      = $93,
     ID_FFREE       = $94,
     ID_FFREEP      = $95,
     ID_FIADD       = $96,
     ID_FICOM       = $97,
     ID_FICOMP      = $98,
     ID_FIDIV       = $99,
     ID_FIDIVR      = $9A,
     ID_FILD        = $9B,
     ID_FIMUL       = $9C,
     ID_FINCSTP     = $9D,
     ID_FIST        = $9E,
     ID_FISTP       = $9F,
     ID_FISTTP      = $A0,
     ID_FISUB       = $A1,
     ID_FISUBR      = $A2,
     ID_FLD         = $A3,
     ID_FLD1        = $A4,
     ID_FLDCW       = $A5,
     ID_FLDENV      = $A6,
     ID_FLDL2E      = $A7,
     ID_FLDL2T      = $A8,
     ID_FLDLG2      = $A9,
     ID_FLDLN2      = $AA,
     ID_FLDPI       = $AB,
     ID_FLDZ        = $AC,
     ID_FMUL        = $AD,
     ID_FMULP       = $AE,
     ID_FNCLEX      = $AF,
     ID_FNDISI      = $B0,
     ID_FNENI       = $B1,
     ID_FNINIT      = $B2,
     ID_FNOP        = $B3,
     ID_FNSAVE      = $B4,
     ID_FNSETPM     = $B5,
     ID_FNSTCW      = $B6,
     ID_FNSTENV     = $B7,
     ID_FNSTSW      = $B8,
     ID_FPATAN      = $B9,
     ID_FPREM       = $BA,
     ID_FPREM1      = $BB,
     ID_FPTAN       = $BC,
     ID_FRNDINT     = $BD,
     ID_FRSTOR      = $BE,
     ID_FSCALE      = $BF,
     ID_FSIN        = $C0,
     ID_FSINCOS     = $C1,
     ID_FSQRT       = $C2,
     ID_FST         = $C3,
     ID_FSTP        = $C4,
     ID_FSTP1       = $C5,
     ID_FSTP8       = $C6,
     ID_FSTP9       = $C7,
     ID_FSUB        = $C8,
     ID_FSUBP       = $C9,
     ID_FSUBR       = $CA,
     ID_FSUBRP      = $CB,
     ID_FTST        = $CC,
     ID_FUCOM       = $CD,
     ID_FUCOMI      = $CE,
     ID_FUCOMIP     = $CF,
     ID_FUCOMP      = $D0,
     ID_FUCOMPP     = $D1,
     ID_FWAIT       = $D2,
     ID_FXAM        = $D3,
     ID_FXCH        = $D4,
     ID_FXCH4       = $D5,
     ID_FXCH7       = $D6,
     ID_FXRSTOR     = $D7,
     ID_FXSAVE      = $D8,
     ID_FXTRACT     = $D9,
     ID_FYL2X       = $DA,
     ID_FYL2XP1     = $DB,
     ID_GETSEC      = $DC,
     ID_HADDPD      = $DD,
     ID_HADDPS      = $DE,
     ID_HINTNOP     = $DF,
     ID_HLT         = $E0,
     ID_HSUBPD      = $E1,
     ID_HSUBPS      = $E2,
     ID_ICEBP       = $E3,
     ID_IDIV        = $E4,
     ID_IMUL        = $E5,
     ID_IN          = $E6,
     ID_INC         = $E7,
     ID_INS         = $E8,
     ID_INSERTPS    = $E9,
     ID_INSERTQ     = $EA,
     ID_INT         = $EB,
     ID_INTO        = $EC,
     ID_INVD        = $ED,
     ID_INVEPT      = $EE,
     ID_INVLPG      = $EF,
     ID_INVLPGA     = $F0,
     ID_INVVPID     = $F1,
     ID_IRET        = $F2,
     ID_JA          = $F3,
     ID_JAE         = $F4,
     ID_JB          = $F5,
     ID_JBE         = $F6,
     ID_JCXZ        = $F7,
     ID_JG          = $F8,
     ID_JGE         = $F9,
     ID_JL          = $FA,
     ID_JLE         = $FB,
     ID_JMP         = $FC,
     ID_JMPF        = $FD,
     ID_JNO         = $FE,
     ID_JNP         = $FF,
     ID_JNS         = $100,
     ID_JNZ         = $101,
     ID_JO          = $102,
     ID_JP          = $103,
     ID_JS          = $104,
     ID_JZ          = $105,
     ID_LAHF        = $106,
     ID_LAR         = $107,
     ID_LDDQU       = $108,
     ID_LDMXCSR     = $109,
     ID_LDS         = $10A,
     ID_LEA         = $10B,
     ID_LEAVE       = $10C,
     ID_LES         = $10D,
     ID_LFENCE      = $10E,
     ID_LFS         = $10F,
     ID_LGDT        = $110,
     ID_LGS         = $111,
     ID_LIDT        = $112,
     ID_LLDT        = $113,
     ID_LMSW        = $114,
     ID_LODS        = $115,
     ID_LOOP        = $116,
     ID_LOOPNZ      = $117,
     ID_LOOPZ       = $118,
     ID_LSL         = $119,
     ID_LSS         = $11A,
     ID_LTR         = $11B,
     ID_LZCNT       = $11C,
     ID_MASKMOVDQU  = $11D,
     ID_MASKMOVQ    = $11E,
     ID_MAXPD       = $11F,
     ID_MAXPS       = $120,
     ID_MAXSD       = $121,
     ID_MAXSS       = $122,
     ID_MFENCE      = $123,
     ID_MINPD       = $124,
     ID_MINPS       = $125,
     ID_MINSD       = $126,
     ID_MINSS       = $127,
     ID_MONITOR     = $128,
     ID_MOV         = $129,
     ID_MOVAPD      = $12A,
     ID_MOVAPS      = $12B,
     ID_MOVBE       = $12C,
     ID_MOVD        = $12D,
     ID_MOVDDUP     = $12E,
     ID_MOVDQ2Q     = $12F,
     ID_MOVDQA      = $130,
     ID_MOVDQU      = $131,
     ID_MOVHLPS     = $132,
     ID_MOVHPD      = $133,
     ID_MOVHPS      = $134,
     ID_MOVLHPS     = $135,
     ID_MOVLPD      = $136,
     ID_MOVLPS      = $137,
     ID_MOVMSKPD    = $138,
     ID_MOVMSKPS    = $139,
     ID_MOVNTDQ     = $13A,
     ID_MOVNTDQA    = $13B,
     ID_MOVNTI      = $13C,
     ID_MOVNTPD     = $13D,
     ID_MOVNTPS     = $13E,
     ID_MOVNTQ      = $13F,
     ID_MOVNTSD     = $140,
     ID_MOVNTSS     = $141,
     ID_MOVQ        = $142,
     ID_MOVQ2DQ     = $143,
     ID_MOVS        = $144,
     ID_MOVSD       = $145,
     ID_MOVSHDUP    = $146,
     ID_MOVSLDUP    = $147,
     ID_MOVSS       = $148,
     ID_MOVSX       = $149,
     ID_MOVSXD      = $14A,
     ID_MOVUPD      = $14B,
     ID_MOVUPS      = $14C,
     ID_MOVZX       = $14D,
     ID_MPSADBW     = $14E,
     ID_MUL         = $14F,
     ID_MULPD       = $150,
     ID_MULPS       = $151,
     ID_MULSD       = $152,
     ID_MULSS       = $153,
     ID_MWAIT       = $154,
     ID_NEG         = $155,
     ID_NOP         = $156,
     ID_NOT         = $157,
     ID_OR          = $158,
     ID_ORPD        = $159,
     ID_ORPS        = $15A,
     ID_OUT         = $15B,
     ID_OUTS        = $15C,
     ID_PABSB       = $15D,
     ID_PABSD       = $15E,
     ID_PABSW       = $15F,
     ID_PACKSSDW    = $160,
     ID_PACKSSWB    = $161,
     ID_PACKUSDW    = $162,
     ID_PACKUSWB    = $163,
     ID_PADDB       = $164,
     ID_PADDD       = $165,
     ID_PADDQ       = $166,
     ID_PADDSB      = $167,
     ID_PADDSW      = $168,
     ID_PADDUSB     = $169,
     ID_PADDUSW     = $16A,
     ID_PADDW       = $16B,
     ID_PALIGNR     = $16C,
     ID_PAND        = $16D,
     ID_PANDN       = $16E,
     ID_PAUSE       = $16F,
     ID_PAVGB       = $170,
     ID_PAVGW       = $171,
     ID_PBLENDVB    = $172,
     ID_PBLENDW     = $173,
     ID_PCLMULQDQ   = $174,
     ID_PCMPEQB     = $175,
     ID_PCMPEQD     = $176,
     ID_PCMPEQQ     = $177,
     ID_PCMPEQW     = $178,
     ID_PCMPESTRI   = $179,
     ID_PCMPESTRM   = $17A,
     ID_PCMPGTB     = $17B,
     ID_PCMPGTD     = $17C,
     ID_PCMPGTQ     = $17D,
     ID_PCMPGTW     = $17E,
     ID_PCMPISTRI   = $17F,
     ID_PCMPISTRM   = $180,
     ID_PEXTRB      = $181,
     ID_PEXTRD      = $182,
     ID_PEXTRQ      = $183,
     ID_PEXTRW      = $184,
     ID_PHADDD      = $185,
     ID_PHADDSW     = $186,
     ID_PHADDW      = $187,
     ID_PHMINPOSUW  = $188,
     ID_PHSUBD      = $189,
     ID_PHSUBSW     = $18A,
     ID_PHSUBW      = $18B,
     ID_PINSRB      = $18C,
     ID_PINSRD      = $18D,
     ID_PINSRQ      = $18E,
     ID_PINSRW      = $18F,
     ID_PMADDUBSW   = $190,
     ID_PMADDWD     = $191,
     ID_PMAXSB      = $192,
     ID_PMAXSD      = $193,
     ID_PMAXSW      = $194,
     ID_PMAXUB      = $195,
     ID_PMAXUD      = $196,
     ID_PMAXUW      = $197,
     ID_PMINSB      = $198,
     ID_PMINSD      = $199,
     ID_PMINSW      = $19A,
     ID_PMINUB      = $19B,
     ID_PMINUD      = $19C,
     ID_PMINUW      = $19D,
     ID_PMOVMSKB    = $19E,
     ID_PMOVSXBD    = $19F,
     ID_PMOVSXBQ    = $1A0,
     ID_PMOVSXBW    = $1A1,
     ID_PMOVSXDQ    = $1A2,
     ID_PMOVSXWD    = $1A3,
     ID_PMOVSXWQ    = $1A4,
     ID_PMOVZXBD    = $1A5,
     ID_PMOVZXBQ    = $1A6,
     ID_PMOVZXBW    = $1A7,
     ID_PMOVZXDQ    = $1A8,
     ID_PMOVZXWD    = $1A9,
     ID_PMOVZXWQ    = $1AA,
     ID_PMULDQ      = $1AB,
     ID_PMULHRSW    = $1AC,
     ID_PMULHUW     = $1AD,
     ID_PMULHW      = $1AE,
     ID_PMULLD      = $1AF,
     ID_PMULLW      = $1B0,
     ID_PMULUDQ     = $1B1,
     ID_POP         = $1B2,
     ID_POPA        = $1B3,
     ID_POPCNT      = $1B4,
     ID_POPF        = $1B5,
     ID_POR         = $1B6,
     ID_PREFETCHNTA = $1B7,
     ID_PREFETCHT0  = $1B8,
     ID_PREFETCHT1  = $1B9,
     ID_PREFETCHT2  = $1BA,
     ID_PSADBW      = $1BB,
     ID_PSHUFB      = $1BC,
     ID_PSHUFD      = $1BD,
     ID_PSHUFHW     = $1BE,
     ID_PSHUFLW     = $1BF,
     ID_PSHUFW      = $1C0,
     ID_PSIGNB      = $1C1,
     ID_PSIGND      = $1C2,
     ID_PSIGNW      = $1C3,
     ID_PSLLD       = $1C4,
     ID_PSLLDQ      = $1C5,
     ID_PSLLQ       = $1C6,
     ID_PSLLW       = $1C7,
     ID_PSRAD       = $1C8,
     ID_PSRAW       = $1C9,
     ID_PSRLD       = $1CA,
     ID_PSRLDQ      = $1CB,
     ID_PSRLQ       = $1CC,
     ID_PSRLW       = $1CD,
     ID_PSUBB       = $1CE,
     ID_PSUBD       = $1CF,
     ID_PSUBQ       = $1D0,
     ID_PSUBSB      = $1D1,
     ID_PSUBSW      = $1D2,
     ID_PSUBUSB     = $1D3,
     ID_PSUBUSW     = $1D4,
     ID_PSUBW       = $1D5,
     ID_PTEST       = $1D6,
     ID_PUNPCKHBW   = $1D7,
     ID_PUNPCKHDQ   = $1D8,
     ID_PUNPCKHQDQ  = $1D9,
     ID_PUNPCKHWD   = $1DA,
     ID_PUNPCKLBW   = $1DB,
     ID_PUNPCKLDQ   = $1DC,
     ID_PUNPCKLQDQ  = $1DD,
     ID_PUNPCKLWD   = $1DE,
     ID_PUSH        = $1DF,
     ID_PUSHA       = $1E0,
     ID_PUSHF       = $1E1,
     ID_PXOR        = $1E2,
     ID_RCL         = $1E3,
     ID_RCPPS       = $1E4,
     ID_RCPSS       = $1E5,
     ID_RCR         = $1E6,
     ID_RDMSR       = $1E7,
     ID_RDPMC       = $1E8,
     ID_RDTSC       = $1E9,
     ID_RDTSCP      = $1EA,
     ID_RETF        = $1EB,
     ID_RETN        = $1EC,
     ID_ROL         = $1ED,
     ID_ROR         = $1EE,
     ID_ROUNDPD     = $1EF,
     ID_ROUNDPS     = $1F0,
     ID_ROUNDSD     = $1F1,
     ID_ROUNDSS     = $1F2,
     ID_RSM         = $1F3,
     ID_RSQRTPS     = $1F4,
     ID_RSQRTSS     = $1F5,
     ID_SAHF        = $1F6,
     ID_SAL         = $1F7,
     ID_SALC        = $1F8,
     ID_SAR         = $1F9,
     ID_SBB         = $1FA,
     ID_SCAS        = $1FB,
     ID_SETA        = $1FC,
     ID_SETAE       = $1FD,
     ID_SETB        = $1FE,
     ID_SETBE       = $1FF,
     ID_SETG        = $200,
     ID_SETGE       = $201,
     ID_SETL        = $202,
     ID_SETLE       = $203,
     ID_SETNO       = $204,
     ID_SETNP       = $205,
     ID_SETNS       = $206,
     ID_SETNZ       = $207,
     ID_SETO        = $208,
     ID_SETP        = $209,
     ID_SETS        = $20A,
     ID_SETZ        = $20B,
     ID_SFENCE      = $20C,
     ID_SGDT        = $20D,
     ID_SHL         = $20E,
     ID_SHLD        = $20F,
     ID_SHR         = $210,
     ID_SHRD        = $211,
     ID_SHUFPD      = $212,
     ID_SHUFPS      = $213,
     ID_SIDT        = $214,
     ID_SKINIT      = $215,
     ID_SLDT        = $216,
     ID_SMSW        = $217,
     ID_SQRTPD      = $218,
     ID_SQRTPS      = $219,
     ID_SQRTSD      = $21A,
     ID_SQRTSS      = $21B,
     ID_STC         = $21C,
     ID_STD         = $21D,
     ID_STGI        = $21E,
     ID_STI         = $21F,
     ID_STMXCSR     = $220,
     ID_STOS        = $221,
     ID_STR         = $222,
     ID_SUB         = $223,
     ID_SUBPD       = $224,
     ID_SUBPS       = $225,
     ID_SUBSD       = $226,
     ID_SUBSS       = $227,
     ID_SWAPGS      = $228,
     ID_SYSCALL     = $229,
     ID_SYSENTER    = $22A,
     ID_SYSEXIT     = $22B,
     ID_SYSRET      = $22C,
     ID_TEST        = $22D,
     ID_UCOMISD     = $22E,
     ID_UCOMISS     = $22F,
     ID_UD          = $230,
     ID_UD2         = $231,
     ID_UNPCKHPD    = $232,
     ID_UNPCKHPS    = $233,
     ID_UNPCKLPD    = $234,
     ID_UNPCKLPS    = $235,
     ID_VERR        = $236,
     ID_VERW        = $237,
     ID_VMCALL      = $238,
     ID_VMCLEAR     = $239,
     ID_VMLAUNCH    = $23A,
     ID_VMLOAD      = $23B,
     ID_VMMCALL     = $23C,
     ID_VMPTRLD     = $23D,
     ID_VMPTRST     = $23E,
     ID_VMREAD      = $23F,
     ID_VMRESUME    = $240,
     ID_VMRUN       = $241,
     ID_VMSAVE      = $242,
     ID_VMWRITE     = $243,
     ID_VMXOFF      = $244,
     ID_VMXON       = $245,
     ID_WBINVD      = $246,
     ID_WRMSR       = $247,
     ID_XADD        = $248,
     ID_XCHG        = $249,
     ID_XGETBV      = $24A,
     ID_XLAT        = $24B,
     ID_XOR         = $24C,
     ID_XORPD       = $24D,
     ID_XORPS       = $24E,
     ID_XRSTOR      = $24F,
     ID_XSAVE       = $250,
     ID_XSETBV      = $251
  );

//Total count of instructions' IDs.
const ID_COUNT = $252;
{$ENDREGION 'Instructions' IDs.'}
{$REGION 'TInstruction'}
type
  TINSTRUCTION = record
    groups: uint64;
    id: TInstructionId; // uint16;
    flags: uint16;
    prefixes: uint16;
    opcode_offset: uint8;

    ops: array [0 .. MAX_OPERANDS_COUNT - 1] of TOPERAND;
    disp: TDISPLACEMENT;

    addrsize: uint8;
    opsize: uint8;
    modrm: uint8;
    sib: uint8;
    rex: uint8;

    opcode_pref: uint8;
    opcodes_len: uint8;
    opcodes: array [0 .. 2] of uint8;

    length: uint8;

    tested_flags: uint8;
    modified_flags: uint8;
    set_flags: uint8;
    cleared_flags: uint8;
    undefined_flags: uint8;

    mnemonic: array [0 .. MAX_MNEMONIC_LEN - 1] of unichar_t;

    function GetOperandCount: integer;
    function HasBranch: boolean;

    // Instruction changes IP.
    // If NewIP = 0 then NewIP is unknown.
    function ChangesIP({CurIP: uint64; }out NewIP: uint64; out IsConditional: boolean): boolean;
  end;

  PINSTRUCTION = ^TINSTRUCTION;
{$ENDREGION 'TInstruction'}
{$REGION 'Params'}
//***********************************
// Structure for passing input/output
// disassembling parameters.
//************************************

// DISASM_PARAMS.options bits:
const
  DISASM_OPTION_APPLY_REL     = $1;
  DISASM_OPTION_OPTIMIZE_DISP = $2;
  DISASM_OPTION_COMPUTE_RIP   = $4;


type
  TDISASM_PARAMS = record
    base: uint64;
    sf_prefixes: pbyte;
    sf_prefixes_len: size_t;
    arch: uint8;
    mode: uint8;
    options: uint8;
  end;

  PDISASM_PARAMS = ^TDISASM_PARAMS;
{$ENDREGION 'Params'}

//Constant for input buffer length;
const
  BUFSIZ_INFINITY = $FFFFFFFF;

//**********************
//* Instructions' groups.
//***********************

   GRP_NULL    = $0000000000000000; //for internal use.
   GRP_PREFIX  = $0000000000000001; //for internal use.
   GRP_SWITCH  = $0000000000000002; //for internal use.

   GRP_3DNOW   = $0000000000000004;
   GRP_AES     = $0000000000000008;
   GRP_AVX     = $0000000000000010;
   GRP_BRANCH  = $0000000000000020;
   GRP_FMA     = $0000000000000040;
   GRP_GEN     = $0000000000000080;
   GRP_MMX     = $0000000000000100;
   GRP_SMX     = $0000000000000200;
   GRP_SSE1    = $0000000000000400;
   GRP_SSE2    = $0000000000000800;
   GRP_SSE3    = $0000000000001000;
   GRP_SSE41   = $0000000000002000;
   GRP_SSE42   = $0000000000004000;
   GRP_SSE4A   = $0000000000008000;
   GRP_SSSE3   = $0000000000010000;
   GRP_VMX     = $0000000000020000;
   GRP_X87FPU  = $0000000000040000;
   GRP_XOP     = $0000000000080000;

   GRP_ARITH   = $0000000000100000;
   GRP_BINARY  = $0000000000200000;
   GRP_BIT     = $0000000000400000;
   GRP_BREAK   = $0000000000800000;
   GRP_CACHECT = $0000000001000000;
   GRP_COMPAR  = $0000000002000000;
   GRP_COND    = $0000000004000000;
   GRP_CONTROL = $0000000008000000;
   GRP_CONVER  = $0000000010000000;
   GRP_DATAMOV = $0000000020000000;
   GRP_DECIMAL = $0000000040000000;
   GRP_FETCH   = $0000000080000000;
   GRP_FLGCTRL = $0000000100000000;
   GRP_INOUT   = $0000000200000000;
   GRP_LDCONST = $0000000400000000;
   GRP_LOGICAL = $0000000800000000;
   GRP_MXCSRSM = $0000001000000000;
   GRP_ORDER   = $0000002000000000;
   GRP_PCKSCLR = $0000004000000000;
   GRP_PCKSP   = $0000008000000000;
   GRP_SEGREG  = $0000010000000000;
   GRP_SHFTROT = $0000020000000000;
   GRP_SHIFT   = $0000040000000000;
   GRP_SHUNPCK = $0000080000000000;
   GRP_SIMDFP  = $0000100000000000;
   GRP_SIMDINT = $0000200000000000;
   GRP_STACK   = $0000400000000000;
   GRP_STRING  = $0000800000000000;
   GRP_STRTXT  = $0001000000000000;
   GRP_SYNC    = $0002000000000000;
   GRP_SYSTEM  = $0004000000000000;
   GRP_TRANS   = $0008000000000000;
   GRP_UNPACK  = $0010000000000000;

//****************************
//* Instructions' architecture.
//*****************************

  ARCH_COMMON = $1;
  ARCH_INTEL  = $2;
  ARCH_AMD    = $4;
  ARCH_ALL    = (ARCH_COMMON or ARCH_INTEL or ARCH_AMD);

//*******************
//* Disassemble modes.
//********************

  DISASSEMBLE_MODE_16 = $1;
  DISASSEMBLE_MODE_32 = $2;
  DISASSEMBLE_MODE_64 = $4;

//**********************
//* Disassembling errors.
//***********************
type
  {$MINENUMSIZE 4}
  TDisassembleError = (
    DASM_ERR_OK           = $00,
    DASM_ERR_BADCODE      = $01,
    DASM_ERR_TOO_LONG     = $02,
    DASM_ERR_NON_LOCKABLE = $03,
    DASM_ERR_RM_REG       = $04,
    DASM_ERR_RM_MEM       = $05,
    DASM_ERR_16_32_ONLY   = $06,
    DASM_ERR_64_ONLY      = $07,
    DASM_ERR_REX_NOOPCD   = $08,
    DASM_ERR_ANOT_ARCH    = $09,
    DASM_ERR_NO_BUFF      = $0A,
    DASM_ERR_INTERNAL     = $0B
  );


(********************
* Main dump function.
*********************)

{$IFDEF MEDIANA_DUMP}
PDUMP_HANDLERS = pointer;

function medi_dump(const instr: TINSTRUCTION;
                 buff: punichar_t;
                 bufflen: size_t;
                 dump_handlers: PDUMP_HANDLERS): size_t; cdecl;
{$ENDIF MEDIANA_DUMP}

(****************************
* Disassembler's entry point.
******************************)
function medi_disassemble(buff: pbyte; bufflen: size_t;
  instr: PINSTRUCTION; inout_params: PDISASM_PARAMS):
  TDisassembleError; cdecl;

// Disassemble with optimization of offsets, rip and displacements.
function medi_disassemble_default(InBuf: pbyte; InBufLen: size_t; base: uint64;
  var ins: TINSTRUCTION;
  var params: TDISASM_PARAMS;
  Bitness: integer): byte;

// Helpers

// Check a and b to be counterpart conditional jumps, like: jz:jnz
function AreJumpCounterparts(a, b: TInstructionId): boolean;

implementation

uses
  System.SysUtils;

// Helpers.

function medi_disassemble_default(InBuf: pbyte; InBufLen: size_t; base: uint64;
  var ins: TINSTRUCTION;
  var params: TDISASM_PARAMS;
  Bitness: integer): byte;
begin
  params.arch := ARCH_ALL;
  params.sf_prefixes := nil;
  params.mode := Bitness shr 4;
  params.options :=
    DISASM_OPTION_APPLY_REL or
    DISASM_OPTION_OPTIMIZE_DISP or
    DISASM_OPTION_COMPUTE_RIP;
  params.base := base;

  if mediana.medi_disassemble(InBuf, InBufLen, @ins, @params) = DASM_ERR_OK then
    exit(ins.length)
  else
    exit(0);
end;

{$REGION 'TADDR'}

function TADDR.HasBase: boolean;
begin
  Result := (&mod and ADDR_MOD_BASE) <> 0;
end;

function TADDR.HasIndex: boolean;
begin
  Result := (&mod and ADDR_MOD_IDX) <> 0;
end;

function TADDR.HasDisp: boolean;
begin
  Result := (&mod and ADDR_MOD_DISP) <> 0;
end;
{$ENDREGION 'TADDR'}
{$REGION 'TOPERAND'}

function TOPERAND.IsPresent: boolean;
begin
  Result := (flags and OPERAND_FLAG_PRESENT) <> 0;
end;

function TOPERAND.IsReg: boolean;
begin
  Result := (flags and OPERAND_TYPE_MASK) = OPERAND_TYPE_REG;
end;

function TOPERAND.IsImm: boolean;
begin
  Result := (flags and OPERAND_TYPE_MASK) = OPERAND_TYPE_IMM;
end;

function TOPERAND.IsFarAddr: boolean;
begin
  Result := (flags and OPERAND_TYPE_MASK) = OPERAND_TYPE_DIR;
end;

function TOPERAND.IsMem: boolean;
begin
  Result := (flags and OPERAND_TYPE_MASK) = OPERAND_TYPE_MEM;
end;

// old, failure on Jz (if dest size > 1)
//function TOPERAND.GetImmediate: UInt64;
//begin
//  case value.imm.size of
//    1: Result := value.imm.val.imm8;
//    2: Result := value.imm.val.imm16;
//    4: Result := value.imm.val.imm32;
//    8: Result := value.imm.val.imm64;
//    else raise Exception.Create('GetImmediateU: Bad Operand Size.');
//  end;
//end;
function TOPERAND.GetImmediate: UInt64;
begin
  case self.size of
    1: Result := value.imm.val.imm8;
    2: Result := value.imm.val.imm16;
    4: Result := value.imm.val.imm32;
    8: Result := value.imm.val.imm64;
    else raise Exception.Create('Bad Operand Size.');
  end;
end;

procedure TOPERAND.SetImmediate(Value: UInt64; Size: uint16);
begin
  if Size = 0 then
    raise Exception.Create('Trying to set size of imm. = 0');

  self.size := Size;

  case self.size of
    1: self.value.imm.val.imm8 := Value;
    2: self.value.imm.val.imm16 := Value;
    4: self.value.imm.val.imm32 := Value;
    8: self.value.imm.val.imm64 := Value;
    else raise Exception.Create('Bad Operand Size.');
  end;
end;
{$ENDREGION 'TOPERAND'}
{$REGION 'TOPERAND.TREG'}
class operator TOPERAND.TREG.Equal(const a,b: TReg): boolean;
begin
  Result := (a.code = b.code) and (a.&type = b.&type);
end;
{$ENDREGION}

{$REGION 'TDISPLACEMENT'}
function TDISPLACEMENT.GetSigned: Int64;
begin
  case self.size of
    0: Result := 0;
    1: Result := self.value.d8;
    2: Result := self.value.d16;
    4: Result := self.value.d32;
    8: Result := self.value.d64;
    else raise Exception.Create('Bad Displacement Size.');
  end;
end;
function TDISPLACEMENT.GetUnsigned: UInt64;
begin
  case self.size of
    0: Result := 0;
    1: Result := uint8(self.value.d8);
    2: Result := uint16(self.value.d16);
    4: Result := uint32(self.value.d32);
    8: Result := uint64(self.value.d64);
    else raise Exception.Create('Bad Displacement Size.');
  end;
end;
{$ENDREGION 'TDISPLACEMENT'}

{$REGION 'TINSTRUCTION'}

function TINSTRUCTION.HasBranch: boolean;
begin
  case id of
    ID_JA .. ID_JLE, ID_JNO .. ID_JZ,
      ID_JMP,
      ID_CALL,
      ID_LOOP, ID_LOOPZ:
      Result := True;
  else
    Result := False;
  end;
end;

function TINSTRUCTION.GetOperandCount: integer;
begin
  if ops[2].IsPresent then
    exit(3);
  if ops[1].IsPresent then
    exit(2);
  if ops[0].IsPresent then
    exit(1);
  exit(0);
end;

function TINSTRUCTION.ChangesIP({CurIP: uint64; }out NewIP: uint64; out IsConditional: boolean): boolean;
begin
  NewIP := 0;
  IsConditional := False;
  case id of
    ID_INT, ID_INTO,
    ID_RETN, ID_RETF:
      Result := True;

    ID_JMP, ID_CALL:
      begin
        Result := True;
        if ops[0].IsImm then
        begin
          // not needed if DISASM_OPTION_APPLY_REL
          // NewIP := CurIP + length + Int64(ops[0].GetImmediate);
          NewIP := ops[0].GetImmediate;
        end;
      end;

      ID_JA .. ID_JLE, ID_JNO .. ID_JZ,
      ID_LOOP, ID_LOOPNZ, ID_LOOPZ:
      begin
        IsConditional := True;
        Result := True;
        if ops[0].IsImm then
        begin
          // not needed if DISASM_OPTION_APPLY_REL
          // NewIP := CurIP + length + Int64(ops[0].GetImmediate);
          NewIP := ops[0].GetImmediate;
        end;
      end;
    else
      Result := False;
  end;
end;

{$ENDREGION 'TINSTRUCTION'}

function test2ids(a, b, test_a, test_b: TInstructionId): boolean; inline;
begin
  Result :=
    ((a = test_a) and (b = test_b)) or
    ((a = test_b) and (b = test_a));
end;

function AreJumpCounterparts(a, b: TInstructionId): boolean;
begin
  if
    test2ids(a, b, ID_JO, ID_JNO) or
    test2ids(a, b, ID_JB, ID_JAE) or
    test2ids(a, b, ID_JZ, ID_JNZ) or
    test2ids(a, b, ID_JA, ID_JBE) or
    test2ids(a, b, ID_JS, ID_JNS) or
    test2ids(a, b, ID_JP, ID_JNP) or
    test2ids(a, b, ID_JL, ID_JGE) or
    False then
    exit(True);
  // todo: AreCounterPartConditions
  // raise Exception.Create('unhandled conditions');
  exit(False);
end;

{$REGION 'STATIC LINK'}

{$IFDEF WIN32}
  {$L 'mediana32.obj'}
{$ELSE}
  {$L 'mediana64.obj'}
{$ENDIF}

function medi_disassemble; external name {$IFDEF CPUX86}'_'+{$ENDIF}'medi_disassemble';

{$IFDEF WIN32}
function _memcpy(dst, src: pointer; cnt: NativeUInt): pointer; cdecl;
{$ELSE}
function memcpy(dst, src: pointer; cnt: NativeUInt): pointer; cdecl;
{$ENDIF}
begin
  Move(src^, dst^, cnt);
  Result := dst;
end;

{$IFDEF WIN32}
function _memset(ptr: pointer; value: integer; num: NativeUInt): pointer; cdecl;
{$ELSE}
function memset(ptr: pointer; value: integer; num: NativeUInt): pointer; cdecl;
{$ENDIF}
begin
  FillChar(ptr^, num, value);
  Result := ptr;
end;

{$ENDREGION 'STATIC LINK'}


end.
