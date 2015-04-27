unit uTPX86.Regs;

interface

uses
  VDAPI;

{$REGION 'Defines'}


const
  BAD_REG = 0;

type
  TCpuRegItem = record
    Name: PChar;         // reg name
    BitSize: byte;       // size of reg
    MReg: TIrRegisterId; // mapped reg, 0 if not mapped
    BitOfs: byte;        // only if mapped
  end;

  TReg =
    (
    NULL = 0,
    CS, DS, ES, FS, GS,
    EAX, AX, AL, AH,
    EBX, BX, BL, BH,
    ECX, CX, CL, CH,
    EDX, DX, DL, DH,
    EBP, BP,
    ESP, SP,
    ESI, SI,
    EDI, DI,
    EFL, FL,
    EIP, IP
    );

const
  CPU_TEXT = 'i386';

  Regs: array [TReg] of TCpuRegItem =
    (
    // null
    (),

    (Name: 'cs'; BitSize: 16),
    (Name: 'ds'; BitSize: 16),
    (Name: 'es'; BitSize: 16),
    (Name: 'fs'; BitSize: 16),
    (Name: 'gs'; BitSize: 16),

    (Name: 'eax'; BitSize: 32),
    (Name: 'ax'; BitSize: 16; MReg: TIrRegisterId(TReg.EAX); BitOfs: 0),
    (Name: 'al'; BitSize: 08; MReg: TIrRegisterId(TReg.EAX); BitOfs: 0),
    (Name: 'ah'; BitSize: 08; MReg: TIrRegisterId(TReg.EAX); BitOfs: 8),

    (Name: 'ebx'; BitSize: 32),
    (Name: 'bx'; BitSize: 16; MReg: TIrRegisterId(TReg.EBX); BitOfs: 0),
    (Name: 'bl'; BitSize: 08; MReg: TIrRegisterId(TReg.EBX); BitOfs: 0),
    (Name: 'bh'; BitSize: 08; MReg: TIrRegisterId(TReg.EBX); BitOfs: 8),

    (Name: 'ecx'; BitSize: 32),
    (Name: 'cx'; BitSize: 16; MReg: TIrRegisterId(TReg.ECX); BitOfs: 0),
    (Name: 'cl'; BitSize: 08; MReg: TIrRegisterId(TReg.ECX); BitOfs: 0),
    (Name: 'ch'; BitSize: 08; MReg: TIrRegisterId(TReg.ECX); BitOfs: 8),

    (Name: 'edx'; BitSize: 32),
    (Name: 'dx'; BitSize: 16; MReg: TIrRegisterId(TReg.EDX); BitOfs: 0),
    (Name: 'dl'; BitSize: 08; MReg: TIrRegisterId(TReg.EDX); BitOfs: 0),
    (Name: 'dh'; BitSize: 08; MReg: TIrRegisterId(TReg.EDX); BitOfs: 8),

    (Name: 'ebp'; BitSize: 32),
    (Name: 'bp'; BitSize: 16; MReg: TIrRegisterId(TReg.EBP); BitOfs: 0),

    (Name: 'esp'; BitSize: 32),
    (Name: 'sp'; BitSize: 16; MReg: TIrRegisterId(TReg.ESP); BitOfs: 0),

    (Name: 'esi'; BitSize: 32),
    (Name: 'si'; BitSize: 16; MReg: TIrRegisterId(TReg.ESI); BitOfs: 0),

    (Name: 'edi'; BitSize: 32),
    (Name: 'di'; BitSize: 16; MReg: TIrRegisterId(TReg.EDI); BitOfs: 0),

    (Name: 'efl'; BitSize: 32),
    (Name: 'fl'; BitSize: 16; MReg: TIrRegisterId(TReg.EFL); BitOfs: 0),

    (Name: 'eip'; BitSize: 32),
    (Name: 'ip'; BitSize: 16; MReg: TIrRegisterId(TReg.EIP); BitOfs: 0)
    );
{$ENDREGION}


implementation

end.
