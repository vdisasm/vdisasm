unit mediana.print.regs;

interface

type
  punichar_t = PAnsiChar;
  ppunichar_t = PPAnsiChar;

  t21regs = array [0 .. 20] of punichar_t;
  p21regs = ^t21regs;

const

  { gen }

  regs8: t21regs =
    (
    'al', 'cl', 'dl', 'bl', 'spl', 'bpl', 'sil', 'dil',
    'r8b', 'r9b', 'r10b', 'r11b', 'r12b', 'r13b', 'r14b', 'r15b',
    'ah', 'ch', 'dh', 'bh', 'badreg8'
    );

  regs16: t21regs =
    (
    'ax', 'cx', 'dx', 'bx', 'sp', 'bp', 'si', 'di',
    'r8w', 'r9w', 'r10w', 'r11w', 'r12w', 'r13w', 'r14w', 'r15w',
    'badreg16_1', 'badreg16_2', 'badreg16_3', 'badreg16_4', 'badreg16_5'
    );

  regs32: t21regs =
    (
    'eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi',
    'r8d', 'r9d', 'r10d', 'r11d', 'r12d', 'r13d', 'r14d', 'r15d',
    'badreg32_1', 'badreg32_2', 'badreg32_3', 'badreg32_4', 'eip'
    );

  regs64: t21regs =
    (
    'rax', 'rcx', 'rdx', 'rbx', 'rsp', 'rbp', 'rsi', 'rdi',
    'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15',
    'badreg64_1', 'badreg64_2', 'badreg64_3', 'badreg64_4', 'rip'
    );

  regs_gen: array [1 .. 8] of p21regs =
    (@regs8, @regs16, nil, @regs32, nil, nil, nil, @regs64);

  { sregs }

  sregs: array [0 .. 7] of punichar_t =
    (
    'es', 'cs', 'ss', 'ds', 'fs', 'gs', 'badsreg7', 'badsreg8'
    );

  { fregs }

  fregs: array [0 .. 8] of punichar_t =
    (
    'st0', 'st1', 'st2', 'st3', 'st4', 'st5', 'st6', 'st7', 'badfreg8'
    );

  { cregs }

  cregs: array [0 .. 9] of punichar_t =
    (
    'cr0', 'cr1', 'cr2', 'cr3', 'cr4', 'cr5', 'cr6', 'cr7', 'cr8', 'badcreg9'
    );

  { dregs }

  dregs: array [0 .. 8] of punichar_t =
    (
    'dr0', 'dr1', 'dr2', 'dr3', 'dr4', 'dr5', 'dr6', 'dr7', 'baddreg'
    );

  { tregs }

  tregs: array [0 .. 8] of punichar_t =
    (
    'tr0', 'tr1', 'tr2', 'tr3', 'tr4', 'tr5', 'tr6', 'tr7', 'badtreg'
    );

  { mregs }

  mregs: array [0 .. 8] of punichar_t =
    (
    'mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6', 'mm7', 'badmreg8'
    );

  { xregs }

  xregs: array [0 .. 16] of punichar_t =
    (
    'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7',
    'xmm8', 'xmm9', 'xmm10', 'xmm11', 'xmm12', 'xmm13', 'xmm14', 'xmm15',
    'badxreg16'
    );

implementation

end.
