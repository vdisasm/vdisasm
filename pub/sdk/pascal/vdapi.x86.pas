unit VDAPI.x86;

interface

type
  TRegister =
    (
    Null = 0,

    // -------------------------------------------------------------------------
    // 8-bit registers

    _8_bit_low,

    // low
    AL, CL, DL, BL, SPL, BPL, SIL, DIL,
    R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,

    // high
    AH, CH, DH, BH,

    _8_bit_high,
    // -------------------------------------------------------------------------
    // 16-bit registers
    _16_bit_low,

    AX, CX, DX, BX, SP, BP, SI, DI,
    R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,
    FLAGS,
    IP,

    // Segment registers (16)
    ES, CS, SS, DS, // not valid in 64-bit mode
    FS, GS,

    // Segment registers (reserved, just dummy names)
    HS, &IS,

    // (16)
    GDTR, IDTR, TR, LDTR,

    _16_bit_high,
    // -------------------------------------------------------------------------
    // 32-bit registers
    _32_bit_low,

    EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
    R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,
    EFLAGS,
    EIP,

    // 32
    MXCR,

    _32_bit_high,
    // -------------------------------------------------------------------------
    // 64-bit registers
    _64_bit_low,

    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8, R9, R10, R11, R12, R13, R14, R15,
    RFLAGS,
    RIP,

    // MMX (64)
    MM0, MM1, MM2, MM3, MM4, MM5, MM6, MM7,

    _64_bit_high,
    // -------------------------------------------------------------------------
    // x87 (80)
    _80_bit_low,

    ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7,

    _80_bit_high,
    // -------------------------------------------------------------------------
    // XMM (128, SIMD)
    _128_bit_low,

    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,

    _128_bit_high,
    // -------------------------------------------------------------------------
    // YMM (256, SIMD)
    _256_bit_low,

    YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7,
    YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15,

    _256_bit_high,
    // -------------------------------------------------------------------------
    // ZMM (512, SIMD)
    _512_bit_low,

    ZMM0, ZMM1, ZMM2, ZMM3, ZMM4, ZMM5, ZMM6, ZMM7,
    ZMM8, ZMM9, ZMM10, ZMM11, ZMM12, ZMM13, ZMM14, ZMM15,

    _512_bit_high,
    // -------------------------------------------------------------------------
    // Control registers
    CR0, CR1, CR2, CR3, CR4, CR5, CR6, CR7, CR8, CR9,
    CR10, CR11, CR12, CR13, CR14, CR15,
    // -------------------------------------------------------------------------
    // Debug registers
    DR0, DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8, DR9,
    DR10, DR11, DR12, DR13, DR14, DR15,
    // -------------------------------------------------------------------------
    // Flag registers
    // see http://en.wikipedia.org/wiki/FLAGS_register
    { 00 } CF,      // carry flag
    { 02 } PF,      // parity flag
    { 04 } AF,      // adjust flag
    { 06 } ZF,      // zero flag
    { 07 } SF,      // sign flag
    { 08 } TF,      // trap flag
    { 09 } &IF,     // interrupt enable flag
    { 10 } DF,      // direction flag
    { 11 } &OF,     // overflow flag
    { 12-13 } IOPL, // I/O privilege level
    { 14 } NT,      // nested task flag
    { 16 } RF,      // resume flag
    { 17 } VM,      // virtual 8086 mode
    { 18 } AC,      // alignment check
    { 19 } VIF,     // virtual interrupt flag
    { 20 } VIP,     // virtual interrupt pending
    { 21 } ID       // able to use CPUID instruction
    // -------------------------------------------------------------------------
    );

implementation

end.
