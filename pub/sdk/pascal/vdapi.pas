unit VDAPI;

interface

const
  // Name of main entry.
  PROGRAM_ENTRY_NAME = 'entry';

{$REGION 'Base'}

{$MINENUMSIZE 4}
{$ALIGN OFF}


type
  BSTR = type WideString;
  PBSTR = ^BSTR;
  BSTR_IN = type PWideChar; // read-only string
  BOOL = type LongBool;
  int = type Integer;
  uint = type LongWord;
  PUInt = ^uint;
  SIZE_T = NativeUInt;
  SSIZE_T = NativeInt;

  PSIZE_T = PNativeUInt;
  PSSIZE_T = PNativeInt;

  Float32 = type Single;
  Float64 = type Double;

  // Version record.
  TVDVersion = record
  const
    _MAJOR = $01;
    _MINOR = $0B;
  public
    Major, Minor: uint8;
  end;

  TVDChar = type WideChar;
  PVDChar = type PWideChar;
  PPVDChar = ^PVDChar;

  TVDSectionId = type UInt32;
  PVDSectionId = ^TVDSectionId;
  TVDSectionSize = type UInt64;

  TVA = type UInt64;
  PVA = ^TVA;

  TRVA = type UInt64; // related to module base

  TRelVA = packed record
    SecID: TVDSectionId;
    SecOfs: TVDSectionSize;
  end;

  PRelVA = ^TRelVA;

  TEndianness = (None, Little, Big);

  TCodePage = uint16;

const
  LIB      = 'core';     // Main library name.
  BAD_VA   = TVA(-1);    // Pseudo address.
  BAD_SIZE = SIZE_T(-1); // Special size value.

type
  IVDCore = interface;
  TVAEnumFunc = function(VA: TVA; ud: Pointer): BOOL; stdcall;

{$ENDREGION}
  // forwards
  IVDModule = interface;
  IVDLoaderTask = interface;
  IVDPlugin = interface;

{$REGION 'IVDUpdateable'}

  IVDUpdateable = interface(IUnknown)
    procedure BeginUpdate; stdcall;
    procedure EndUpdate; stdcall;
  end;
{$ENDREGION}
{$REGION 'IO'}

  TIOVar = record
  const
    RootDir       = '%rootdir%';
    Db            = '%db%';
    Bin           = '%bin%';
    Temp          = '%temp%';
    Local         = '%local%';
    Plugins       = '%plugins%';
    Loaders       = '%loaders%';
    Debuggers     = '%debuggers%';
    CPU           = '%cpu%';
    TypeProviders = '%typeproviders%';
    TypeLibs      = '%typelibs%';
  end;

  IVDIO = interface(IUnknown)
    ['{169E3CC1-74D4-40AE-869D-30FE6DCE90C1}']

    // Copy file or directory from SrcPath to DstPath.
    // Ofs: position in input file.
    // Size: size of block starting from Ofs. (if 0, whole file is copied)
    // TotalSize: final size of destination file (>= Size, zero bytes added).
    function Copy(SrcPath, DstPath: BSTR; ofs: UInt64 = 0;
      Size: UInt64 = 0; TotalSize: UInt64 = 0): BOOL; stdcall;

    function CreateEmptyFile(Path: BSTR; Size: UInt64): BOOL; stdcall;

    procedure SetVariable(Name, Path: BSTR); stdcall;
    procedure DelVariable(Name: BSTR); stdcall;
    function ExpandPath(Path: BSTR): BSTR; stdcall;

    function FileExists(Path: BSTR): BOOL; stdcall;

    function FileSize(Path: BSTR): UInt64; stdcall;

    // Return False if failed.
    function CreateDirectory(Path: BSTR): BOOL; stdcall;
  end;
{$ENDREGION}

{$REGION 'Functions'}

  // not completed
  IVDFunctions = interface(IUnknown)
    ['{02975D4F-560C-48E7-8222-B7D21EA52393}']
    // Check if VA is start of function.
    function IsFunctionStart(VA: TVA): BOOL; stdcall;
    function Add(VA: TVA): BOOL; stdcall;
  end;
{$ENDREGION 'Functions'}
{$REGION 'IVDStream'}

  IVDBaseStream = interface
    ['{84025436-B389-452D-9825-EB440ADCAFF0}']
    function GetSize: UInt64; stdcall;
    function SetSize(Value: UInt64): BOOL; stdcall;
    function GetPosition: UInt64; stdcall;
    procedure SetPosition(Value: UInt64); stdcall;
    function Read(var Buffer; Size: UInt32): UInt32; stdcall;
    function Write(const Buffer; Size: UInt32): UInt32; stdcall;
  end;

  IVDMemoryStream = interface(IVDBaseStream)
    ['{25224F36-D1C5-4052-BBEE-CBEBF369130C}']
    function GetMemory: Pointer; stdcall;
  end;

  // Endianness-aware reader/writer of stream.
  IVDStreamIO = interface(IVDBaseStream)
    ['{057534A3-4BB7-4721-B563-9A1AAC380C93}']
    // WordSize: 1,2,4,8
    function ReadWord(WordSize: uint8): UInt64;
    procedure WriteWord(WordSize: uint8; Value: UInt64);

    function ReadU8: uint8; stdcall;
    procedure WriteU8(Value: uint8); stdcall;

    function ReadU16: uint16; stdcall;
    procedure WriteU16(Value: uint16); stdcall;

    function ReadU32: UInt32; stdcall;
    procedure WriteU32(Value: UInt32); stdcall;

    function ReadU64: UInt64; stdcall;
    procedure WriteU64(Value: UInt64); stdcall;

    // Fixed size VA (64 bit)
    function ReadVA: TVA; stdcall;
    procedure WriteVA(Value: TVA); stdcall;

    procedure ReadRelVA(out Value: TRelVA); stdcall;
    procedure WriteRelVA(const Value: TRelVA); stdcall;

    function ReadBSTR(ReadSize: BOOL = True): BSTR; stdcall;
    procedure WriteBSTR(Value: BSTR; WriteSize: BOOL = True); stdcall;

    function Skip(Size: int = 1): BOOL; stdcall;

    function PeekU8(out Value: uint8): BOOL; stdcall;

    // Copy all data of Source starting from current position in Source to end
    // into this stream.
    function CopyFrom(Source: IVDStreamIO): SIZE_T; stdcall;

    function Clear: BOOL; stdcall;

    // Write byte Count times.
    procedure FillByte(Value: uint8; Count: uint); stdcall;
  end;

  PIVDStreamIO = ^IVDStreamIO;

{$ENDREGION}
{$REGION 'Text Layout Values'}
  TTag =
    (
    TAG_NONE = 0,
    TAG_FIELD_ID = 1,   // tag:1, id:1 (user-defined id).
    TAG_TAB_COLUMN = 3, // tag. Jump to next column (if any, or 1 step).
    TAG_ID = 4,         // tag, id
    TAG_TEXT_LEN = 5,   // tag, len, text (can be multiline)
    TAG_SKIP = 6,       // tag, len (skip "len" chars)
    TAG_LF = 10,
    TAG_CR = 13,

    BAD_FIELD_ID = 0,

    // TagID
    // todo: Most of these values are deprecated.
    TAGID_NONE = 0,
    TAGID_VA = 1,
    TAGID_NUMBER = 2,
    TAGID_ENUM = 3,
    TAGID_STRING = 4,
    TAGID_CODE = 5,
    TAGID_LABEL = 6,
    TAGID_REGISTER = 7,
    TAGID_WARNING = 8,
    TAGID_REFIN = 9,
    TAGID_REFOUT = 10,
    TAGID_COMMENT = 11,
    TAGID_KEYWORD = 13,
    TAGID_OFFSET = 14,
    TAGID_HEXINCODE = 15,
    TAGID_BREAKPOINT_ACTIVE = 16,
    TAGID_BREAKPOINT_INACTIVE = 17,
    TAGID_LINEHIGHLIGHT_DBG = 20,

    TAGID_NAME = TAGID_LABEL,

    // Hex window
    TAGID_HEX_BYTE = 21, // color of hex block
    TAGID_HEX_CHR = 22,  // color of char block
    TAGID_HEX_MOD = 23,  // color of modified chars

    // TAGID_FIELD = 128, // Text is field.
    TAGID_MASK = 255
    );

{$ENDREGION}
{$REGION 'Text Layout'}

  // Generic text layout.
  IVDTextLayout = interface
    ['{3593235E-0294-473C-90C4-24B584592704}']
    procedure Clear; stdcall;
    procedure Skip(CntChars: uint8 = 1); stdcall;
    procedure AddChar(Value: WideChar; TagId: TTag = TTag.TAGID_NONE); stdcall;
    procedure AddText(Text: BSTR_IN; TagId: TTag = TTag.TAGID_NONE); stdcall;
    procedure AddTextEx(Text: BSTR_IN; TagId: TTag = TTag.TAGID_NONE; MaxLines: int = 1); stdcall;
    procedure AddColumn; stdcall;
    procedure AddField(Id: uint8); stdcall;
    procedure Append(Text: BSTR); stdcall;
    procedure LineBreak; stdcall;
    function GetColumnId: int; stdcall;
    function GetLineCount: int; stdcall;
    function Get: BSTR; stdcall;
    function GetLine(Index: int): BSTR; stdcall;
    procedure IndentEnter; stdcall;
    procedure IndentLeave; stdcall;
  end;

  // Used for decoding items at VA.
  IVDVATextLayout = interface(IVDTextLayout)
    procedure AddAddress(Value: UInt64); stdcall;
    procedure AddRegister(Id: UInt32); stdcall;
    procedure AddInteger(Value: UInt64; Bits: Integer; Signed: boolean; ForceSign: BOOL = False); stdcall;
    procedure AddFloat32(Value: Float32); stdcall;
    procedure AddFloat64(Value: Float64); stdcall;
  end;
{$ENDREGION}
{$REGION 'Type Provider'}

  TVDTextFlag = record
  const
    Plain = 0;
    Tags  = 1 shl 0;
  end;

  TVDTextFlags = UInt32;

  TVDDataEncoding =
    (
    // Decoding: Param is not used, return size only.
    // Encoding: n/a
    Size = 0,

    // Decoding: Param is IVDVATextLayout.
    // Encoding: Param is BSTR_IN plain text.
    Text = 1
    );

  // IVDTypeProviderBase is used by
  // * IVDTypeProvider
  // * IVDCPU
  IVDTypeProviderBase = interface(IUnknown)
    ['{24BEF8AC-152F-4C34-8FF4-C5902100AC4B}']
    // Decode data at VA into structure depending on Kind.
    // Param must be casted to type according to Kind.
    // Result is size of decoded data or 0 if failed.
    function Decode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
    // Encode data from structure depending on Kind and write to VA.
    // Param must be casted to type according to Kind.
    // Result is size of encoded data or 0 if failed.
    // If encoding is not supported, function must return SIZE_T(-1).
    // If VA is BAD_VA and encoding supported must return 1.
    function Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
  end;

  IVDTypeProvider = interface(IVDTypeProviderBase)
    ['{E26E4060-0353-48A4-BECC-E6805BD169D3}']
  end;
{$ENDREGION}
{$REGION 'IR'}

  TIrOpcode =
    (
    IROPC_NULL = 0,
    IROPC_VAR,
    IROPC_CONST,

    IROPC_NOP,

    // unary
    IROPC_NEG, // -a0

    // binary
    IROPC_ADD, // a0 + a1
    IROPC_SUB,
    IROPC_MUL,
    IROPC_DIV,

    // Relation operations.
    // <, =, > could be enough (all other are combinations), but let it be all
    // combinations already there.
    IROPC_REL_EQUAL,
    IROPC_REL_NOTEQUAL,
    IROPC_REL_LESS,
    IROPC_REL_LESS_OR_EQUAL,
    IROPC_REL_GREATER,
    IROPC_REL_GREATER_OR_EQUAL,

    // Logical operations.
    IROPC_LOGICAL_NOT,
    IROPC_LOGICAL_OR,
    IROPC_LOGICAL_AND,

    // Bitwise operations.
    IROPC_BITWISE_NOT,
    IROPC_BITWISE_OR,
    IROPC_BITWISE_AND,
    IROPC_BITWISE_XOR,

    IROPC_MOV,  // a0 := a1
    IROPC_GOTO, // if <COND> goto LABEL; cgoto label, cond

    IROPC_LOAD,  // a0 := [a1]
    IROPC_STORE, // [a0] := a1
    IROPC_LABEL,

    IROPC_DEFINE, // define expression

    IROPC_LAST
    );

  TIRConstType =
    (
    IRCT_NULL = 0,
    IRCT_SINT,
    IRCT_UINT,
    IRCT_SINGLE,
    IRCT_DOUBLE,
    IRCT_STRING,
    IRCT_STRING_A, // live call need to know string type
    IRCT_STRING_W  // live call need to know string type
    );

  IVDExpression = interface
    ['{58139376-AD62-4167-ABDC-C7EBE2C3E996}']
  end;

  IVDConstExpression = interface
    ['{194804BA-608D-49DC-B989-3FB94AF8E9B0}']
    function GetType: TIRConstType; stdcall;
    function GetSInt: int64; stdcall;
    function GetUInt: UInt64; stdcall;
    function GetString: BSTR; stdcall;
  end;

  IVDUnaryExpression = interface
    ['{47623394-D72B-4D09-91D3-924F611D54F3}']
    function GetExpr: IVDExpression; stdcall;
  end;

  IVDBinaryExpression = interface
    ['{0F7ABBBD-D05D-4B7C-B479-43067A8B89D1}']
    function GetLeft: IVDExpression; stdcall;
    function GetRight: IVDExpression; stdcall;
  end;

  // Base expression.
  IRExprHandle = type Pointer;
  PIRExprHandle = ^IRExprHandle;

  PIRSlot = type Pointer;
  PPIRSlot = ^PIRSlot;

  IIRBasicBlock = interface(IUnknown)
    ['{447AB5BE-254C-4CF8-8DEF-66465BBCEF7B}']
    function GetVA: TVA; stdcall;
    procedure SetVA(VA: TVA); stdcall;

    function GetSize: int; stdcall;
    procedure SetSize(Value: int); stdcall;

    // Insert expression and return resulting slot.
    function InsertFirst(X: IRExprHandle): PIRSlot; stdcall;
    function InsertLast(X: IRExprHandle): PIRSlot; stdcall;
    function InsertBefore(Slot: PIRSlot; X: IRExprHandle): PIRSlot; stdcall;
    function InsertAfter(Slot: PIRSlot; X: IRExprHandle): PIRSlot; stdcall;

    // Functions to get first/next/prev/last slots.
    // Slot contains statement expression.
    function GetFirstSlot: PIRSlot; stdcall;
    function GetLastSlot: PIRSlot; stdcall;
    function GetNextSlot(Slot: PIRSlot): PIRSlot; stdcall;
    function GetPrevSlot(Slot: PIRSlot): PIRSlot; stdcall;

    // Get expression from slot.
    function GetSlotExpression(Slot: PIRSlot): IRExprHandle; stdcall;

    property VA: TVA read GetVA write SetVA;
    property Size: int read GetSize write SetSize;
  end;

  IIRBasicBlockEdge = interface(IUnknown)
    function GetSource: IIRBasicBlock; stdcall;
    function GetTarget: IIRBasicBlock; stdcall;
  end;

  PIterator = Pointer;
  IIRBasicBlockHandle = type Pointer;

  IIRCFG = interface(IUnknown)
    { Basic Block functions }

    // Get block value by handle.
    function GetBlock(Handle: IIRBasicBlockHandle): IIRBasicBlock; stdcall;

    // Create new block.
    // VA is start address of block.
    // Size is size of block in bytes. It is sum of sizes of all machine
    // instructions in the block.
    // Size can be set to 0 and changed later.
    function BlockNew(VA: TVA; Size: int): IIRBasicBlockHandle; stdcall;

    // Connect blocks.
    // Source can be Nil if you want to add entry block to graph.
    // Exception raised if trying to add entry block while one already exists.
    procedure BlockConnect(Source, Target: IIRBasicBlockHandle); stdcall;

    // Get entry basic block.
    function BlockGetEntry: IIRBasicBlockHandle; stdcall;

    // Block iteration.
    function BlockGetFirst: PIterator; stdcall;
    function BlockMoveNext(it: PIterator): BOOL; stdcall;
    function BlockGetCurrent(it: PIterator): IIRBasicBlock; stdcall;

    // Edge iteration.
    function EdgeGetFirst: PIterator; stdcall;
    function EdgeMoveNext(it: PIterator): BOOL; stdcall;
    function EdgeGetCurrent(it: PIterator): IIRBasicBlockEdge; stdcall;

    // Free iterator (block/edge).
    procedure FreeIterator(it: PIterator); stdcall;
  end;

  TIrVariableId = UInt32;
  TIrRegisterId = UInt32; // 0 - BAD_REG
  PIrRegisterId = ^TIrRegisterId;

  TIRVarMapType =
    (
    MappedVoid,
    MappedToRegister
    );

  // To create primitives in context of function.
  IIRExprPool = interface(IUnknown)
    ['{5D6E4C2A-3AA2-48F7-8565-19C5C23CD19E}']

    function GetOpcode(X: IRExprHandle): TIrOpcode; stdcall;
    function GetArg(X: IRExprHandle; No: int): IRExprHandle; stdcall;

    // Name is optional (can be nil).
    function CreateVar(Name: BSTR_IN; BitSize: int; VA: TVA): IRExprHandle; stdcall;

    // Create expressions with N number of parameters.
    function CreateExpr0(Opcode: TIrOpcode): IRExprHandle; stdcall;
    function CreateExpr1(Opcode: TIrOpcode; X1: IRExprHandle): IRExprHandle; stdcall;
    function CreateExpr2(Opcode: TIrOpcode; X1, X2: IRExprHandle): IRExprHandle; stdcall;

    function GetVarCount: int; stdcall;
    function GetVar(Index: int): IRExprHandle; stdcall;
    // Get name of variable. It can be empty.
    function GetVarName(X: IRExprHandle): BSTR_IN; stdcall;
    // Get string like "v_1", "v_2" and so on.
    function GetVarNameIdentifier(X: IRExprHandle): BSTR; stdcall;
    procedure GetVarVA(X: IRExprHandle; { out } VA: PVA); stdcall;
    function GetVarBitSize(X: IRExprHandle): int; stdcall;
    function GetVarMapType(X: IRExprHandle): TIRVarMapType; stdcall;

    // If mappeed to register, we can get register.
    function GetVarReg(X: IRExprHandle): TIrRegisterId; stdcall;

    function CreateConstSInt(Value: int64): IRExprHandle; stdcall;
    function CreateConstUInt(Value: UInt64): IRExprHandle; stdcall;
    function GetConstType(X: IRExprHandle): TIRConstType; stdcall;
    function GetConstSInt(X: IRExprHandle): int64; stdcall;
    function GetConstUInt(X: IRExprHandle): UInt64; stdcall;

    function CreateLabel(VA: TVA; Text: BSTR_IN = nil): IRExprHandle; stdcall;
    function GetLabelVA(X: IRExprHandle): TVA; stdcall;
    procedure GetLabelText(X: IRExprHandle; out Text: BSTR); stdcall;
  end;

  IVDCpu = interface;

  IVDFunction = interface(IUnknown)
    ['{19CCADBF-654A-497D-B960-BB459BE6B35C}']
    function GetVA: TVA; stdcall;
    function GetSafeName: BSTR; stdcall; // get name or autogenerated name
    function GetCfg: IIRCFG; stdcall;
    function GetCpu: IVDCpu; stdcall;

    property Cfg: IIRCFG read GetCfg;
    property CPU: IVDCpu read GetCpu;
  end;

  TVDAnalysisContext = packed record
  public
    c: IVDCore;

    // Current VA.
    VA: TVA;

    // Function that is being built.
    fn: IVDFunction;

    // Basic block where low-level instructions must be inserted.
    bb: IIRBasicBlockHandle;

    // Slot you should insert low-level instructions after.
    // Modify slot to point to your last inserted instruction.
    // ctx.slot := bb.InsertAfter(ctx.slot, x_instr);
    Slot: PIRSlot;

    CPU: IVDCpu;
  end;

  PVDAnalysisContext = ^TVDAnalysisContext;
{$ENDREGION}
{$REGION 'Analysis Helper'}

  // todo: IVDAnalysisHelper
  IVDAnalysisHelper = interface(IUnknown)
    ['{8DB41170-2850-43BB-98F4-A88DD64B0BBB}']

    // Search for nearest slot (up) where register is defined.
    // It can be not defined at all.
    // Or it can be defined few times in different blocks.
    // Reg is matched exactly, it does not check if reg is sub-register.
    // If definition not found and InsertDefine is true Define opcode
    // will be inserted at entry block.
    function FindDefinitionOfRegister(
      ctx: PVDAnalysisContext;
      StartBB: IIRBasicBlockHandle;
      StartSlot: PIRSlot;
      Reg: TIrRegisterId;
      InsertDefine: boolean;
      out DefSlot: PIRSlot;
      out DefExpr: IRExprHandle
      ): BOOL; stdcall;

    function FetchRegisterExprForUse(
      ctx: PVDAnalysisContext;
      Reg: TIrRegisterId): IRExprHandle; stdcall;

    function FetchRegisterExprForDef(
      ctx: PVDAnalysisContext;
      Reg: TIrRegisterId): IRExprHandle; stdcall;
  end;
{$ENDREGION}
{$REGION 'CPU'}

  TCpuName = record
    // Case-sensitive names of CPUs. These names are used as type names to make
    // location a code.
  const
    X16 = 'X16';
    X32 = 'X32';
    X64 = 'X64';
    ARM = 'ARM';
  end;

  TCpuInstrBranchKind =
    (
    Null = 0,       // unknown type of branch
    Call,           //
    Jump,           //
    Return,         // returns to caller
    SwitchTableJump //
    );

  TMemoryAccessKinds = record
  const
    MEMACC_NULL = 0;

    // The address is read.
    MEMACC_READ = 1 shl 0;

    // The address is written.
    MEMACC_WRITE = 1 shl 1;

    // The address is probably not real memory location. Use it for any
    // immediate value, that can potentially be memory location.
    MEMACC_NOT_SURE = 1 shl 2;
  end;

  TMemoryAccessKind = UInt32;

  TBranchRec = packed record
    Kind: TCpuInstrBranchKind; // kind of branch
    DstVA: TVA;                // BAD_VA if destination is unknown
    IsPtr: boolean;            // is it pointer to branch
  end;

  IVDCpuInstrBasicInfo = interface(IUnknown)
    ['{F2AAE7C2-DAEE-472E-9B09-EAB2A59A6AC0}']

    // Clear all internal states. Called internally.
    procedure Clear; stdcall;

    // If instruction changes control flow, call this function. Set kind of
    // branch. If you know branch address pass it. If not pass BAD_VA.
    //
    // if IsPtr is True, VA is pointer to word which points to actual code
    // (like jmp/call someword ptr [some_variable])
    procedure AddBranch(Kind: TCpuInstrBranchKind; VA: TVA; IsPtr: boolean = False); stdcall;

    function GetBranchCount: uint; stdcall;
    function GetBranch(Index: uint; out Rec: TBranchRec): BOOL; stdcall;

    // If instr. read or write memory, call this function.
    // VA, Size, Kind is accessed address, size and kind.
    procedure AddMemoryAccess(VA: TVA; Size: SSIZE_T; Kind: TMemoryAccessKind); stdcall;

    function GetMemoryAccessCount: uint; stdcall;
    function GetMemoryAccess(Index: uint;
      out VA: TVA;
      out Size: SIZE_T;
      out Kind: TMemoryAccessKind): BOOL; stdcall;

    // Is this instruction conditional.
    function GetConditional: BOOL; stdcall;
    procedure SetConditional(Value: BOOL); stdcall;

    // JmpVA: address of jump.
    // TableVA: start of table with pointer to switch cases.
    // Count: number of elements in table. Pass 0 if you don't know.
    procedure AddSwitchTableJump(JmpVA, TableVA: TVA; Count: int); stdcall;

    property BranchCount: uint read GetBranchCount;
    property MemoryAccessCount: uint read GetMemoryAccessCount;
    property Conditional: BOOL read GetConditional write SetConditional;
  end;

  IVDCpu = interface(IVDTypeProviderBase)
    ['{419FDBDF-2BCC-493F-ABFD-8482F2BDADFE}']

    // Get CPU textual unique identifier, like i386.
    function GetName: BSTR; stdcall;

    // Get id of last register. 0 - is reserved (it's null).
    function GetRegHigh: TIrRegisterId; stdcall;

    // Get Reg Info.
    // MapOfs and Size are in bits.
    function GetRegInfo(
      Id: TIrRegisterId;
      { out } Name: PBSTR;
      { out } MapReg: PIrRegisterId;
      { out } MapOfs: PUInt;
      { out } Size: PUInt
      ): BOOL; stdcall;

    // Parse instruction and modify Info accordingly. Result is size of
    // instruction bytes or 0 if failed.
    function ParseBasic(VA: TVA; Info: IVDCpuInstrBasicInfo): SIZE_T; stdcall;

    // Advanced instruction parsing. Reserved.
    // It can return False, then ParseBasic is used. Result is size of instruction
    // or 0 if failed.
    function ParseAdvanced(VA: TVA; ctx: PVDAnalysisContext): SIZE_T; stdcall;

    // Fill region with NOP instructions and return True on success.
    // Todo: Not sure if need it right now.
    function Nop(VA: TVA; Size: SIZE_T): BOOL; stdcall;
  end;

  TVDCpuEnumFunc = function(Name: BSTR_IN; CPU: IVDCpu; ud: Pointer): BOOL; stdcall;

  // List of CPUs.
  IVDCpus = interface(IUnknown)
    ['{813CE44D-FEFC-4F0F-BF74-7D1F5457CEB6}']
    // Get CPU by name.
    procedure reserved; stdcall; // function Get(Name: BSTR_IN; out Value: IVDCpu): BOOL; stdcall;

    // Enumerate all CPUs.
    procedure Enumerate(cb: TVDCpuEnumFunc; ud: Pointer); stdcall;
  end;

{$ENDREGION}
{$REGION 'Type Library'}

  // Names of std. types.
  TVDStdTypeName = record
  const
    u8  = 'std.u8';
    u16 = 'std.u16';
    u32 = 'std.u32';
    u64 = 'std.u64';
    s8  = 'std.s8';
    s16 = 'std.s16';
    s32 = 'std.s32';
    s64 = 'std.s64';
    f32 = 'std.f32';
    f64 = 'std.f64';
  end;

  IVDTypeLibrary = interface;

  TVDBitSize = type UInt32;
  PVDBitSize = ^TVDBitSize;

  TVDTypeKind =
    (
    TYPEKIND_NULL = 0,

    TYPEKIND_ENUM,

    TYPEKIND_SIMPLE,
    TYPEKIND_INT,
    TYPEKIND_FLOAT,

    TYPEKIND_CHAR,
    TYPEKIND_WIDECHAR,
    TYPEKIND_STRING,

    TYPEKIND_ARRAY,
    TYPEKIND_RECORD,
    TYPEKIND_POINTER,

    TYPEKIND_ALIAS
    );

  IVDType = interface(IUnknown)
    ['{1C33DD69-5685-4AFF-B86C-B06355F5E0FB}']
    function GetTypeLib: IVDTypeLibrary; stdcall;

    function GetKind: TVDTypeKind; stdcall;

    function GetName: BSTR; stdcall;
    procedure SetName(Value: BSTR); stdcall;

    function GetComment: BSTR; stdcall;
    procedure SetComment(Value: BSTR); stdcall;

    function GetBitsize: TVDBitSize; stdcall;

    property TypeLib: IVDTypeLibrary read GetTypeLib;
    property Kind: TVDTypeKind read GetKind;
    property Name: BSTR read GetName write SetName;
    property Comment: BSTR read GetComment write SetComment;
    property BitSize: TVDBitSize read GetBitsize;
  end;

  TVDFieldInfo = packed record
    Name: BSTR;
    Comment: BSTR;
    &Type: IVDType;
  end;

  PVDFieldInfo = ^TVDFieldInfo;

  IVDTypeWithFields = interface(IVDType)
    // BitOffset:
    // [in]  offset to search for field.
    // [out] start offset of found field.
    // Result show if field was found. If found Info is filled.
    function FindFieldByBitOffset(BitOffset: PVDBitSize; Info: PVDFieldInfo): BOOL; stdcall;
  end;

  TRecordFieldEnumFunc = function(BitOfs: TVDBitSize;
    Name, Comment: BSTR; FieldType: IVDType; ud: Pointer): BOOL; stdcall;

  IVDRecordType = interface(IVDTypeWithFields)
    ['{50C68C65-A8B0-4843-89A0-0BC3EBB7F777}']
    function GetInherited: IVDRecordType; stdcall;
    procedure AddField(FieldName: BSTR_IN; FieldType: IVDType; Comment: BSTR_IN = nil); stdcall;
    procedure AddGap(BitSize: TVDBitSize); stdcall;
    procedure EnumFields(EnumFunc: TRecordFieldEnumFunc; ud: Pointer); stdcall;
  end;

  IVDArrayType = interface(IVDTypeWithFields)
    ['{EB51A9FF-A5FF-4C1B-9B37-7769B460A648}']
  end;

  TTextEnumFunc = function(Text: BSTR; ud: Pointer): BOOL; stdcall;

  IVDTypeLibrary = interface(IUnknown)
    ['{F6D6C06B-AE7C-45C1-958B-402E96E12FDE}']
    procedure MakeDirty; stdcall;

    function GetName: BSTR; stdcall;

    // Variables not supported in filename.
    function LoadFromFile(FileName: BSTR_IN): BOOL; stdcall;
    function SaveToFile(FileName: BSTR_IN): BOOL; stdcall;

    function ImportTypeLib(FileName: BSTR_IN): IVDTypeLibrary; stdcall;

    function FindImportedTypeLibByFileName(Name: BSTR_IN): IVDTypeLibrary; stdcall;

    // Search type by name. If SearchInImports is False, only current lib
    // is searched, otherwise imports searched too.
    function FindType(LibName, TypeName: BSTR_IN; SearchInImports: BOOL): IVDType; stdcall;

    procedure EnumImportedLibFileNames(EnumFunc: TTextEnumFunc; ud: Pointer); stdcall;
    procedure EnumTypeNames(EnumFunc: TTextEnumFunc; ud: Pointer); stdcall;
  end;
{$ENDREGION}
{$REGION 'Type Manager'}

  // Unique identifier of type used in database (not in type library).
  // Normally user doesn't need it.
  // 0 is reserved.
  TTypeUID = type UInt32;
  PTypeUID = ^TTypeUID;

  IVDTypeMgr = interface(IUnknown)
    ['{66E8C781-A73E-49FA-A34B-EA1723075015}']
    function GetInfo(Name: BSTR_IN; out UID: TTypeUID; out Provider: IVDTypeProviderBase): BOOL; stdcall;
    // Get type provider by name.
    function GetProvider(Name: BSTR_IN): IVDTypeProviderBase; stdcall;
    procedure RegisterPluginForType(Name, PluginPath: BSTR_IN); stdcall;
  end;
{$ENDREGION}
{$REGION 'Type Factory'}

  IVDTypeFactory = interface(IUnknown)
    ['{79F9DF78-720F-40B1-B52E-79C25F73359C}']

    // Create signed or unsigned integer type.
    function CreateIntegerType(Name: BSTR_IN; Signed: BOOL; BitSize: int):
      IVDType; stdcall;

    function CreateFloatType(Name: BSTR_IN; BitSize: int): IVDType; stdcall;

    // Create simple type by string, like u32,s32.
    function CreateSimpleTypeFromString(Text: BSTR_IN): IVDType; stdcall;

    // Record.
    function CreateRecord(Name: BSTR_IN; InheritedRecord: IVDRecordType = nil):
      IVDRecordType; stdcall;

    // Array.
    function CreateArray1(Name: BSTR_IN; FieldType: IVDType;
      Lower, Upper: int): IVDArrayType; stdcall;
    function CreateArray2(Name: BSTR_IN; FieldType: IVDType;
      Count: int): IVDArrayType; stdcall;
  end;
{$ENDREGION}
{$REGION 'Jobs'}

  TVDJobState = (
    JOB_PAUSED = 0,
    JOB_RUNNING
    );

  IVDAsyncJob = interface(IUnknown)
    ['{F67375FD-C9FD-4379-A12C-5B31D5A36289}']
    function GetState: TVDJobState; stdcall;
    function Terminate: BOOL; stdcall;
    function Pause: BOOL; stdcall;
    function Start: BOOL; stdcall;
  end;

  IVDJob = IVDAsyncJob;

  // todo: not completed
  IVDJobs = interface(IVDUpdateable)
    ['{A2384075-119B-4B77-9CF7-E5D5365A4EA1}']
    procedure AddMakeCode(VA: TVA); stdcall;
    procedure AddMakeFunc(VA: TVA); stdcall;
  end;
{$ENDREGION}
{$REGION 'Decoder'}

  TVDDecodeToTextFlag = record
  const
    Address      = 1 shl 0;
    Name         = 1 shl 1;
    Comment      = 1 shl 2;
    Reference    = 1 shl 3;
    FuncSplitter = 1 shl 4;
    SymExport    = 1 shl 5;
    SymImport    = 1 shl 6;
    Body         = 1 shl 7;
    Sample4hl    = 1 shl 8; // it's not real text but sample for highlighter (options)
    SectionBreak = 1 shl 9;

    Default =
      Address or Name or Comment or Reference or FuncSplitter or SymExport or
      SymImport or Body or SectionBreak;
  end;

  TVDDecodeToTextFlags = UInt32;

  IVDDecoder = interface(IUnknown)
    ['{6966265A-0D7A-41E1-8182-A497729D2E02}']
    // Decode bytes at VA to Text and return size of decoded data.
    // Change VA to start of next item (line).
    function DecodeToText([ref] VA: TVA; Text: IVDVATextLayout;
      Flags: TVDDecodeToTextFlags = TVDDecodeToTextFlag.Default): UInt32; stdcall;

    // Step through raw bytes or defined items.
    // Result is True if VA changed.
    // Step defines direction: -1 or 1.
    // If VA is raw byte, VA is changed by 1 to step over this byte.
    // If VA is defined item, VA is set to item start (when step -1) or item
    // end (when step 1). If item end is section end, it's set to next section
    // start.
    function ItemStep(InOutVA: PVA; Step: int): BOOL; stdcall;

    // Get item start and return True if it is found.
    function ItemStart(InOutVA: PVA): BOOL; stdcall;

    // Go Step (or max. available) addresses fwd/back. Section gaps are
    // jumped over. Step can be e.g. -10, 15. Result is number of actual steps
    // done.
    function AddressStep(InOutVA: PVA; Step: int): SSIZE_T; stdcall;

    // Try to get typename at VA. If type found VA is changed to start of item.
    // Returned type name and size of item at VA.
    function GetTypeName(var VA: TVA; out TypeName: BSTR; out Size: UInt32): BOOL; stdcall;

    // Get minimal number of digits for address (for any VA in whole VA space).
    function HexAddressDigitCount(ReservedFlags: UInt32 = 0): int; stdcall;

    // Return formatted hex-address. By default it uses minimum digit count.
    function HexAddressPrint(VA: TVA; ReservedFlags: UInt32 = 0): BSTR; stdcall;

    procedure SetHexDumpByteCount(Value: int); stdcall;
  end;
{$ENDREGION}
{$REGION 'Plugins'}

  TVDPluginExport = record
  const
    PluginInfo         = 'PluginInfo';         // PVDPluginInfo
    CreatePlugin       = 'CreatePlugin';       // TVDPluginCreateFunc
    CreateTypeProvider = 'CreateTypeProvider'; // TVDPluginCreateTypeProvider
    ProvidedTypes      = 'ProvidedTypes';
  end;

  TVDProvidedTypeFlag = record
  const
    TypeIsCPU = 1 shl 0; // to be added to cpu list (IVDCpus)
  end;

  TVDProvidedTypeInfo = packed record
    Name: PVDChar;
    Flags: UInt32; // TVDProvidedTypeFlag
  end;

  PVDProvidedTypeInfo = ^TVDProvidedTypeInfo;

  TVDPluginInfoFlag = record
  const
    // Plugin is loaded on CoreGet (Run called) and unloaded on CoreFree.
    // If this flag isn't set plugin is loaded and unloaded when plugin called.
    // Example: plugin adds menu on core startup and removes menu on core free.
    AliveWhileCoreIsAlive = 1 shl 0;

    // Plugin info will not be passed to callback on plugin enumeration
    // (IVDPluginManager.ScanPluginDir)
    // For example plugin will not be visible in Plugins menu (because it is
    // invisible for initial enumeration), but will be automatically launched
    // once if AliveWhileCoreIsAlive is set (usually it is needed to register
    // own menus).
    DontEnumerate = 1 shl 1;
  end;

  TVDPluginInfo = record
    Name: BSTR_IN;
    Desc: BSTR_IN;
    Flags: UInt32;
  end;

  PVDPluginInfo = ^TVDPluginInfo;

  // Base plugin.
  // todo: Do we still need this IVDPluginBase interface?
  IVDPluginBase = interface(IUnknown)
    ['{DA9FEEEA-1ADA-4AA2-BB5D-55685FDCCE29}']
    // Called to check if plugin can be used.
    function Init: BOOL; stdcall;
  end;

  IVDPlugin = interface(IVDPluginBase)
    ['{DA9FEEEA-1ADA-4AA2-BB5D-55685FDCCE29}']
    // Run function depends on plugin type.
    procedure Run; stdcall;
  end;

  TVDLoaderFormatDesc = packed record
    Text: BSTR; // format text displayed on loader selection
    Id: int;    // format id so loader can identify it in FillTask.
  end;

  IVDLoaderFormats = interface(IUnknown)
    ['{23594D69-F26C-4225-AE66-39AB1C21940F}']
    procedure Add([ref] Desc: TVDLoaderFormatDesc); stdcall;
  end;

  // Loader plugin.
  IVDLoaderPlugin = interface(IUnknown)
    ['{36C72A12-4364-4F3C-8252-83D31D018FFE}']
    // Get number of possible formats. It affects priority in which loaders
    // will be listed on selection.
    // For example:
    // Binary: 1 possible format
    // DOS: 2 possible formats (binary and dos)
    // PE: 3 possible formats (binary, dos and pe).
    function GetPossibleFormatCount: int; stdcall;
    // Return description of formats found for input file (called after Init).
    // Used during loader selection.
    procedure GetFormatDesc(Formats: IVDLoaderFormats); stdcall;
    // Fill task to be applied to database. Id defines format to be used.
    procedure FillTask(Task: IVDLoaderTask; Id: int); stdcall;
    // Called when task applied.
    procedure TaskApplied; stdcall;
  end;

  // Function exported from module to be used for plugin creation by core.
  // Must be named as TVDPluginExport.CreatePlugin constant.
  TVDPluginCreateFunc = procedure(out Result: IUnknown); stdcall;

  TVDPluginInfoTraverseFunc = function(FileName: BSTR; Info: PVDPluginInfo; ud: Pointer): BOOL; stdcall;

  TVDPluginCreateTypeProvider = procedure(Name: BSTR_IN; out Result: IVDTypeProviderBase); stdcall;

  TPluginScanFunc = function(FileName: BSTR_IN; Plugin: IInterface; LoadedInfo: PVDPluginInfo; ud: Pointer): BOOL; stdcall;

  TPluginScanFlag = record
  const
    // Add types provided by plugin to plugin->type mapping. It's done
    // internally on Core creation.
    PLGSCAN_TYPEPROVIDERS = 1 shl 0;
  end;

  TPluginScanFlags = UInt32;

  IVDPluginManager = interface(IUnknown)
    ['{E9E4FF9D-2FC8-443B-89BD-B1496AC24FF7}']
    // Scan directory for plugins and run Callback on each one.
    // Set Flags to 0 if you don't need additional processing.
    procedure ScanPluginDir(Path: BSTR_IN; Callback: TPluginScanFunc; Flags: TPluginScanFlags; ud: Pointer = nil); stdcall;

    // Init and Run plugin by path, then terminate.
    function RunPlugin(FileName: BSTR): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION 'Input file'}

  // File being analysed.
  IVDInputFile = interface(IUnknown)
    ['{5D95FF4B-0929-4946-9934-72E307290FAA}']
    function GetFileName: BSTR; stdcall;
    procedure SetFileName(Value: BSTR); stdcall;

    function GetParams: BSTR; stdcall;
    procedure SetParams(Value: BSTR); stdcall;

    function GetWorkingDir: BSTR; stdcall;
    procedure SetWorkingDir(Value: BSTR); stdcall;

    property FileName: BSTR read GetFileName write SetFileName;
    property Params: BSTR read GetParams write SetParams;
    property WorkingDir: BSTR read GetWorkingDir write SetWorkingDir;
  end;
{$ENDREGION}
{$REGION 'Debugging'}

  //
  // DON'T USE DEBUGGER INTERFACE
  // IT IS EXPERIMENTAL
  // AND NOT FUNCTION CURRENTLY
  //

  TVDProcessThreadId = type UInt32;
  TVDProcessId = type UInt32;
  TVDThreadId = type UInt32;

  IVDProcessThread = interface(IUnknown)
    ['{D411193C-E0BD-48A8-9105-7483F3D93576}']
    function GetId: TVDProcessThreadId; stdcall;
  end;

  IVDProcess = interface(IUnknown)
    ['{E1CF1F1D-D8A9-4593-8F70-E8707EBA20BA}']
    function GetId: TVDProcessId; stdcall;
  end;

  TVDDebugStatus = (
    DBG_OK,
    DBG_ERROR,
    DBG_ALREADY_ACTIVE,
    DBG_CANT_LOAD_DBG_PLUGIN,
    DBG_PROCESS_NOT_CREATED,
    DBG_NOT_FOUND,
    DBG_BAD_VA
    );

  TVDDebuggerState = (
    DBGSTATE_INACTIVE, // not started or terminated
    DBGSTATE_RUNNING,  // running
    DBGSTATE_PAUSED    // paused due to breakpoint or exception or other
    );

  TVDBreakpointFlag = record
  const
    BPF_ENABLED = 1 shl 0; // breakpoint is enabled
    BPF_ACTIVE  = 1 shl 1; // breakpoint is active (installed)
    BPF_SOFT    = 1 shl 2; // software breakpoint
    BPF_HARD    = 1 shl 3; // hardware breakpoint
    BPF_DATA    = 1 shl 4; // break on data
    BPF_READ    = 1 shl 5; // break on read
    BPF_WRITE   = 1 shl 6; // break on write
    BPF_EXECUTE = 1 shl 7; // break on execute
    BPF_TEMP    = 1 shl 8; // breakpoint is deleted when triggered (used for RunTo)
  end;

  TVDBreakpointFlags = UInt32;

  TVDBreakPointRecord = packed record
    VA: TVA;
    Flags: TVDBreakpointFlags;
  end;

  PVDBreakPointRecord = ^TVDBreakPointRecord;

  TVDDebugEventCode = (
    DBG_EVT_NONE,
    // Windows compatible
    DBG_EVT_EXCEPTION,
    DBG_EVT_CREATE_THREAD,
    DBG_EVT_CREATE_PROCESS,
    DBG_EVT_EXIT_THREAD,
    DBG_EVT_EXIT_PROCESS,
    DBG_EVT_LOAD_MODULE,
    DBG_EVT_UNLOAD_MODULE,
    DBG_EVT_OUTPUT_STRING,
    DBG_EVT_RIP,
    // Own
    DBG_EVT_BREAKPOINT_INITIAL,
    DBG_EVT_BREAKPOINT,
    DBG_EVT_SINGLESTEP
    );

  TVDExceptionRecord = packed record
    VA: TVA;
    Code: UInt32;
  end;

  TVDBreakRecord = packed record
    VA: TVA;
  end;

  TVDLoadModuleRecord = packed record
    Address: UInt64;
  end;

  TVDUnloadModuleRecord = packed record
    Address: UInt64;
  end;

  TVDDebugEvent = packed record
    Code: TVDDebugEventCode;
    ProcessId: TVDProcessId;
    ThreadId: TVDThreadId;

    // Valid on DBG_EVT_LOAD_MODULE.
    ModuleFileName: BSTR;

    case TVDDebugEventCode of
      DBG_EVT_EXCEPTION:
        (Exception: TVDExceptionRecord);
      DBG_EVT_BREAKPOINT_INITIAL:
        (BreakPointInitial: TVDBreakRecord);
      DBG_EVT_BREAKPOINT:
        (BreakPoint: TVDBreakRecord);
      DBG_EVT_SINGLESTEP:
        (SingleStep: TVDBreakRecord);
      DBG_EVT_LOAD_MODULE:
        (LoadModule: TVDLoadModuleRecord);
      DBG_EVT_UNLOAD_MODULE:
        (UnloadModule: TVDUnloadModuleRecord);
  end;

  PVDDebugEvent = ^TVDDebugEvent;

  IVDLiveCallParams = interface(IUnknown)
    procedure AddArg(Value: IVDConstExpression); stdcall;
    function GetArg(Index: int; out Value: IVDConstExpression): BOOL; stdcall;
  end;

  IVDDebuggerPlugin = interface(IVDPluginBase)
    ['{8DB679CF-F91D-4379-90FC-E9B48E38CBC5}']

    function StartProcess(Host: BSTR_IN; Port: Word;
      FileName, WorkingDir, Params: BSTR_IN): TVDDebugStatus; stdcall;

    function StopProcess: TVDDebugStatus; stdcall;

    // Execute single instruction.
    procedure StepIn; stdcall;

    // If debugger plugin has some events to be passed to debug manager it must
    // pause process, fill Event structure and return True.
    // Process is unpaused when Continue called and no pending events exist.
    function WaitForDebugEvent(TimeOut: int; out Event: TVDDebugEvent): BOOL; stdcall;

    // Continue debugee execution if there are no pending events.
    // If there are events not yet passed to WaitForDebugEvent return False.
    // If there are no events, unpause process.
    function Continue(
      ProcessId: TVDProcessId;
      ThreadId: TVDThreadId): BOOL; stdcall;

    procedure ExceptionHandled; stdcall;

    function Write(VA: TVA; Buf: Pointer; Size: SIZE_T; Done: PSIZE_T): BOOL; stdcall;
    function Read(VA: TVA; Buf: Pointer; Size: SIZE_T; Done: PSIZE_T): BOOL; stdcall;

    // Install or uninstall breakpoint.
    // State: True - install, False - uninstall breakpoint.
    // Return True if operation succeeded.
    function ActivateBreakpoint(BP: PVDBreakPointRecord; State: BOOL): BOOL; stdcall;

    // Try get value of register by name.
    function GetRegisterValue(Name: BSTR_IN; out Value: IVDConstExpression): BOOL; stdcall;

    // Get value of current instruction pointer.
    // Must be queried when debugger is paused.
    function GetIP: TVA; stdcall;

    function LiveCall(VA: TVA; Params: IVDLiveCallParams): BOOL; stdcall;
  end;

  // Return False to stop enumeration.
  TEnumBreakpointsFunc = function(BP: PVDBreakPointRecord; ud: Pointer): BOOL; stdcall;

  // todo: this needs cleanup
  IVDDebugSession = interface(IUnknown)
    ['{22857BEB-B10C-4C2B-BF5E-01EDB56F44ED}']

    function SelectDebuggerPlugin(PluginPath: BSTR_IN): BOOL; stdcall;

    // Get selected debugger. If it's not yet loaded, load it.
    procedure GetDebugger(ForceLoad: BOOL; out Dbg: IVDDebuggerPlugin); stdcall;

    // Free debugger references and unload debugger plugin.
    procedure UnloadDebugger; stdcall; // called internally

    // Stop debug session (created process and thread must be terminated).
    function Stop: TVDDebugStatus; stdcall;

    function GetState: TVDDebuggerState; stdcall;
    procedure SetState(Value: TVDDebuggerState); stdcall;

    function GetBreakpoint(VA: TVA; { out } BP: PVDBreakPointRecord): BOOL; stdcall;
    function SetBreakpoint(VA: TVA; BP: PVDBreakPointRecord): TVDDebugStatus; stdcall;
    function DelBreakpoint(VA: TVA): TVDDebugStatus; stdcall;

    // Make breakpoint active/inactive (not to confuse with enabled/disabled).
    function ActivateBreakpoint(BP: PVDBreakPointRecord; State: BOOL): BOOL; stdcall;

    // VAFirst, VALast can be BAD_VA (auto-range used).
    procedure EnumBreakpoints(VAFirst, VALast: TVA; cb: TEnumBreakpointsFunc; ud: Pointer); stdcall;

    // VA is used if Params don't contain it.
    function LiveCall(VA: TVA; Params: BSTR_IN): BOOL; stdcall;

    // Safely try to get register value.
    // It checks if debugger is launched and in paused state and tries to get
    // value.
    function GetRegisterValue(Name: BSTR_IN; out Value: IVDConstExpression): BOOL; stdcall;

    property State: TVDDebuggerState read GetState write SetState;
  end;

{$ENDREGION}
{$REGION 'Messages'}

  TVDMessage =
    (
    MSG_FIRST,

    MSG_CORE_INIT,
    MSG_CORE_FREE, // may not use CoreGet

    MSG_DB_OPENED,

    // Called before closing is processed (and database files deleted optionally).
    // User must release all obtained interfaces. For example, clear all obtained
    // sections. Otherwise core will fail to delete sections.
    MSG_DB_BEFORE_CLOSED,

    // Called when database closed.
    MSG_DB_CLOSED,

    MSG_DB_BEFORE_SAVE,
    MSG_DB_AFTER_SAVE, // when db saved, but not closed

    MSG_LOG_CLEAR,
    MSG_LOG_TEXT,         // param = BSTR
    MSG_LOG_BEGIN_UPDATE, //
    MSG_LOG_END_UPDATE,   //

    MSG_MENU_ADD,    // param = PVDMenuItemMsgParam
    MSG_MENU_REMOVE, // param = TVDMenuItemHandle

    MSG_UI_SELECT_ITEM,  // param = PVDItemSelectorRecord
    MSG_UI_REPAINT_VIEW, // param = uint32(UIVT_xxx)
    MSG_UI_GET_SELRANGE, // param = PVDSelVARange

    MSG_VM_WRITE_DONE, // vm write happened

    // Check if address in param is on disasm screen now.
    // If it is on screen param must be changed to BAD_VA otherwise untouched.
    // If message handler is absent it is handled like VA is not on screen.
    MSG_IS_VA_ON_SCREEN, // param = PVA

    MSG_VAPOS_CHANGED, // param = PVDVAPos

    MSG_SECTIONS_CHANGED,

    MSG_NAMES_CHANGED,
    MSG_EXPORTS_CHANGED,
    MSG_IMPORTS_CHANGED,
    MSG_REFS_CHANGED,
    MSG_PROBLEMS_CHANGED,
    MSG_JOBS_CHANGED,

    // Enable/disable updating any window.
    MSG_CHANGE_GUI_REFRESH_STATE, // uint(param): 0/1

    MSG_DBG_WANT_STOP, // ui must stop debugger
    MSG_DBG_STARTED,
    MSG_DBG_STOPPED,
    MSG_DBG_SINGLE_STEP, // single step happened; param = PVDSingleStepRecord
    MSG_DBG_BREAKPOINT,
    MSG_DBG_STATE_CHANGED,

    MSG_LAST
    );

  TVDMessageListenerProc = procedure(Msg: TVDMessage; Param: Pointer); stdcall;

  IVDMessageManager = interface(IUnknown)
    ['{DCB9F5EC-E667-466F-84BB-009C922226BE}']
    // Add new listener.
    procedure AddListenerProc(Proc: TVDMessageListenerProc); stdcall;
    // Remove existing listener.
    procedure RemoveListenerProc(Proc: TVDMessageListenerProc); stdcall;
    // Send message to all listeners.
    // Order of called listener is unpredicted.
    procedure Broadcast(Msg: TVDMessage; Param: Pointer = nil); stdcall;
  end;
{$ENDREGION}
{$REGION 'Sections'}

  TVDSectionFlag = record
  const
    Readable    = 1 shl 0; // r
    Writable    = 1 shl 1; // w
    Execuatable = 1 shl 2; // x
    Guard       = 1 shl 3; // g
    FileMapped  = 1 shl 4; // f
    Debugger    = 1 shl 5; // d: temporary debugger section
    TLS         = 1 shl 6; // t: thread local storage
  end;

  TVDSectionFlags = type UInt32;

  // Virtual memory region.
  IVDVARegion = interface(IUnknown)
    ['{8658538F-5144-4548-94F7-ED963448FAFB}']
    function GetStartVA: TVA; stdcall;
    function GetSize: TVDSectionSize; stdcall;
    function GetEndVA: TVA; stdcall;
    function GetLastVA: TVA; stdcall;
    function Contains(VA: TVA): BOOL; stdcall;
  end;

  IVDSection = interface(IVDVARegion)
    ['{D6D64D80-CC93-486A-A5E0-02710229A828}']
    function GetFlags: TVDSectionFlags; stdcall;
    function GetFlagsAsString: BSTR; stdcall;
    function GetId: TVDSectionId; stdcall;
    function GetName: BSTR; stdcall;
    function Read(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32; stdcall;
    function Write(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32; stdcall;
    // Flush section data changes.
    function Flush: BOOL; stdcall;
  end;

  PVDSection = ^IVDSection;

  TSectionEnumProc = function(Section: IVDSection; ud: Pointer): BOOL; stdcall;

  IVDSections = interface(IVDUpdateable)
    ['{4C070D8F-8631-4DCF-B1EB-0B89DC488638}']
    // List.
    function GetCount: int; stdcall;
    function GetFirst: IVDSection; stdcall;
    function GetLast: IVDSection; stdcall;

    // Add section w/o raw data.
    function AddEmpty(Name: BSTR; Size: UInt64; VA: TVA;
      Flags: TVDSectionFlags): IVDSection; stdcall;

    // Copy section into database and add it as mapped file.
    // If RawSize <> 0 then FileName and Offset  used to copy data.
    // Otherwise FileName and Offset are ignored and section is filled with zeros.
    function AddFromFile(FileName, Name: BSTR; Offset: UInt64;
      RawSize, VirtSize: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags)
      : IVDSection; stdcall;

    // Add section mapped directly to file.
    function AddFromMappedFile(FileName, Name: BSTR; Offset: UInt64;
      Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags): IVDSection; stdcall;

    // Add temp. debug section (deleted when debugger stopped).
    function AddDebugSection(Name: BSTR; Size: TVDSectionSize;
      VA: TVA; Flags: TVDSectionFlags): IVDSection; stdcall;

    // Find section. Result is Nil if not found.
    function Find(VA: TVA): IVDSection; stdcall;
    function FindByID(Id: TVDSectionId): IVDSection; stdcall;
    // Find section and previous/next sections.
    procedure FindEx(VA: TVA; OutPrev, OutCur, OutNext: PVDSection); stdcall;
    // Map VA to contigious position in range MinPos .. MaxPos.
    function MapVAToContigiousPos(VA: TVA; MinPos, MaxPos: int): int; stdcall;
    // Map contigious position in range to VA.
    function MapContigiousPosToVA(Pos, MinPos, MaxPos: int): TVA; stdcall;
    // Enumerate sections.
    procedure Enumerate(cb: TSectionEnumProc; ud: Pointer); stdcall;
    // Relocate section.
    function Relocate(Section: IVDSection; NewVA: TVA): BOOL; stdcall;
    // Calc section size between addresses.
    function CalcContDelta(VA0, VA1: TVA): TVA; stdcall;
    // Save all section data modifications.
    procedure Flush; stdcall;

    // Delete section.
    // DON'T USE IT. IT'S UNSTABLE NOW.
    procedure Delete(Sec: IVDSection); stdcall;

    // Save section content to file.
    // Result is True if all section data saved.
    function SaveToFile(Sec: IVDSection; Path: BSTR_IN): BOOL; stdcall;

    property Count: int read GetCount;
  end;
{$ENDREGION}
{$REGION 'Loader Task'}

  IVDLoaderTask = interface(IUnknown)
    procedure SetEntry(Value: TVA); stdcall;
    procedure SetEndianness(Value: TEndianness); stdcall;

    // Address size in bytes (2,4,8).
    procedure SetAddressSize(Value: int); stdcall;

    procedure SetCpuName(Value: BSTR_IN); stdcall;

    procedure AddSectionFromFile(FileName, Name: BSTR_IN; Offset: UInt64;
      RawSize, VirtSize: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags); stdcall;

    procedure AddSectionFromMappedFile(FileName, Name: BSTR_IN; Offset: UInt64;
      Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags); stdcall;
  end;
{$ENDREGION}
{$REGION 'Virtual Memory'}

  IVDModule = interface(IUnknown)
    function GetImageBase: TVA; stdcall;
    procedure SetImageBase(VA: TVA); stdcall;

    property ImageBase: TVA read GetImageBase write SetImageBase;
  end;

  IVDVirtualMemory = interface(IUnknown)
    ['{5AC2710F-10DB-4DB5-8394-7685464F0C0F}']
    // Get sections interface.
    function GetSections: IVDSections; stdcall;

    function GetFirstVA(OutVA: PVA): BOOL; stdcall;
    function GetLastVA(OutVA: PVA): BOOL; stdcall;
    // Check if VA exists.
    function Exists(VA: TVA): BOOL; stdcall;

    // Absolute to relative VA.
    function AbsToRelVA(VA: TVA; out RelVA: TRelVA): BOOL; stdcall;
    // Relative VA to absolute VA.
    // Result is True on success. If result if False VA isn't changed.
    function RelToAbsVA(const RelVA: TRelVA; out VA: TVA): BOOL; stdcall;

    // Read/Write bytes. Bytes are assumed to be in machine endiannes (which is
    // in CoreData.Endianness, put by loader). Values for endiannes are
    // None:        no conversion done (default).
    // Little, Big: to little/big-endian
    function Read(
      VA: TVA;
      Buffer: Pointer;
      Size: SIZE_T;
      ToEndianness: TEndianness = TEndianness.None): SIZE_T; stdcall;
    function Write(
      VA: TVA;
      Buffer: Pointer;
      Size: SIZE_T;
      ToEndianness: TEndianness = TEndianness.None): SIZE_T; stdcall;

    // Create stream for easy access to virtual memory. FirstVA/LsatVA define
    // memory region. If any of two addresses is BAD_VA it's set to First/Last
    // VA available. Stream position is FirstVA.
    // Stream must be freed before db closed or section sizes change.
    function CreateStreamIO(FirstVA, LastVA: TVA; ReadOnly: BOOL): IVDStreamIO; stdcall;

    // Cache is enabled by default.
    procedure CacheSetEnabled(Value: BOOL); stdcall;
    procedure CacheInvalidate; stdcall;

    // Properties
    property Sections: IVDSections read GetSections;
  end;
{$ENDREGION}
{$REGION 'UI'}

  // Interface to provide selection from list of textual items.
  IVDItemSelector = interface(IUnknown)
    ['{D7F25A61-1658-4833-B690-8796F4BE6C89}']
    function GetCaption: BSTR; stdcall;
    function GetItemCount: int; stdcall;
    function GetItemText(Index: int): BSTR; stdcall;
  End;

  // Used in messages.
  TVDItemSelectorRecord = packed record
    Selector: IVDItemSelector;
    ResultIndex: int;
  end;

  PVDItemSelectorRecord = ^TVDItemSelectorRecord;

  TUIViewType = record
  const
    UIVT_DISASM = 1 shl 0;
  end;

  TUISelRangeFlags = record
  const
    DEFAULT_SELECT_ALL_VA  = 1;
    DEFAULT_SELECT_SECTION = 2;
  end;

  TVDSelVARange = record
    VA0: TVA; // initially = BAD_VA
    VA1: TVA; // initially = BAD_VA
  end;

  PVDSelVARange = ^TVDSelVARange;

  IVDUserInterface = interface(IUnknown)
    ['{2B26A326-DD50-488F-8536-B8E079A0CE71}']
    // -1 if nothing selected.
    function SelectFromList(Selector: IVDItemSelector): int; stdcall;
    // View type is TUIViewType.
    procedure RepaintView(ViewType: uint); stdcall;
    // Enable/disable GUI refresh.
    procedure SetGuiRefreshState(Enabled: BOOL); stdcall;
    // Get range of VAs selected by user.
    function GetSelectedRange([ref] VA0, VA1: TVA; Flags: UInt32 { TUISelRangeFlags } ): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION 'UI.Log'}

  IVDLog = interface(IVDUpdateable)
    ['{676DE6E9-6026-4241-A3C9-E956FB5E47C8}']
    procedure Clear; stdcall;
    procedure Write(Text: BSTR); stdcall;
    procedure WriteLn(Text: BSTR); stdcall;
  end;
{$ENDREGION}
{$REGION 'UI.Menu'}

  TVDMenuOnClickProc = procedure; stdcall;
  TVDMenuItemHandle = type Pointer;

  // Used for internal messaging.
  TVDMenuItemMsgParam = record
    OnClickProc: TVDMenuOnClickProc;
    Path: BSTR;
    ShortCut: BSTR;
    Result: TVDMenuItemHandle;
  end;

  PVDMenuItemMsgParam = ^TVDMenuItemMsgParam;

  IVDMenu = interface(IUnknown)
    ['{6ECFBC4D-A7C6-4832-8C76-98579CB11173}']
    // Add menu item or change OnClickProc for existing items.
    // Shortcut is optional. Result is nil if operation failed.
    function Add(OnClickProc: TVDMenuOnClickProc; Path: BSTR;
      ShortCut: BSTR = ''): TVDMenuItemHandle; stdcall;
    // Remove menu item.
    // Menu items are unloaded on core shutdown automatically.
    procedure Remove(Handle: TVDMenuItemHandle); stdcall;
  end;
{$ENDREGION}
{$REGION  'Textual'}

  TTextualFlags = type UInt32;
  PTextualFlags = ^TTextualFlags;

  TTextualEnumFunc = function(VA: TVA; Text: BSTR; Flags: TTextualFlags; ud: Pointer): BOOL; stdcall;

  IVDTextual = interface(IVDUpdateable)
    ['{E16D4388-3875-4998-9286-567EF77D8EE8}']
    // Get textual value and flags by VA. Result is True if item found.
    // OutFlags are optional and can be nil.
    function Get(VA: TVA; out Text: BSTR; OutFlags: PTextualFlags = nil): BOOL; stdcall;
    // Put textual value and flags at VA.
    function Put(VA: TVA; Text: BSTR; Flags: TTextualFlags): BOOL; stdcall;
    // Delete textual value.
    function Del(VA: TVA): BOOL; stdcall;
    procedure Enumerate(cb: TTextualEnumFunc; ud: Pointer); stdcall;
  end;

  IVDNames = interface(IVDTextual)
    ['{1B329D0F-C9FD-480D-80F8-E7C2A9DA0F6D}']
    function GetByText(Text: BSTR; out VA: TVA): BOOL; stdcall;
    function Put(VA: TVA; Text: BSTR; Flags: TTextualFlags): BOOL; stdcall;
  end;

  IVDComments = IVDTextual;
{$ENDREGION}
{$REGION 'Strings'}
  // Return True to continue or False to stop.
  TVDStringScanFunc = function(VA: TVA; Size: int; CodePage: TCodePage; ud: Pointer): BOOL; stdcall;

  // todo: TVDStringScanOptions
  // TVDStringScanOptions = packed record
  // end;
  //
  // PVDStringScanOptions = ^TVDStringScanOptions;

  // Strings represented as defined type regions + code page.
  IVDStrings = interface
    ['{7DD2442F-9759-41AE-BF83-2A036AFF1185}']
    function Define(VA: TVA; Size: int; CodePage: TCodePage): BOOL; stdcall;

    // Size is calculated by finding terminating char.
    function DefineCharTerminatedString(
      VA: TVA;
      TermChar: WideChar;
      CodePage: TCodePage;
      MaxSize: int = 0): BOOL; stdcall;

    function DefineNullTerminatedString(
      VA: TVA;
      CodePage: TCodePage;
      MaxSize: int = 0): BOOL; stdcall;

    // VA will be set to start of string. Size will be set to size of string (in bytes).
    // All values are set only if string found and unchanged otherwise.
    function Get(var VA: TVA; out Size: int; out CodePage: TCodePage): BOOL; stdcall;
    function GetText(var VA: TVA; out Text: BSTR): BOOL; stdcall;

    function ReadString(VA: TVA; Size: int; CodePage: TCodePage; out Text: BSTR): BOOL; stdcall;

    function ScanCharTerminatedStringSize(
      VA: TVA;
      CodePage: TCodePage;
      TermChar: WideChar;
      MaxSize: int = 0): int; stdcall;

    // Read length of the string (before the string itself).
    function GetLenPrefixedStringSize(VA: TVA; PrefixSize: int): int; stdcall;

    // Undefine is just for convenience here. String is region and can be
    // undefined as usual typed region.
    function Undefine(VA: TVA): BOOL; stdcall;

    // Scan range of virtual addresses for string.
    // If VABegin/VAEnd is BAD_VA min and max addresses used.
    // MinSize: minimal size of string (0 - use default value)
    function ScanForStrings(
      VABegin, VAEnd: TVA;
      MinSize: int;
      cb: TVDStringScanFunc;
      ud: Pointer): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION 'References'}

  TVDReferenceKind =
    (
    REFKIND_UNKNOWN = 0,
    REFKIND_JUMP,
    REFKIND_CALL,
    REFKIND_FALLTHROUGH,
    REFKIND_READORWRITE, // not known which exactly
    REFKIND_READ,
    REFKIND_WRITE
    );

  // VA:   source address
  // Kind: kind of ref; can be nil
  // ud:   user data
  TRefEnumFunc = function(VA: TVA; Kind: TVDReferenceKind; ud: Pointer): BOOL; stdcall;

  // Virtual Address References.
  IVDVAReferences = interface(IVDUpdateable)
    ['{74D307D7-7D35-4020-A224-336D55AEAD07}']
    // Put reference pair FromVA -> VA.
    function Put(VA: TVA; FromVA: TVA; Kind: TVDReferenceKind): BOOL; stdcall;
    // Delete reference pair FromVA -> VA.
    function Del(VA: TVA; FromVA: TVA): BOOL; stdcall;
    // Delete all references for VA.
    function DelAll(VA: TVA): BOOL; stdcall;
    // Enumerate references to VA.
    // Callback is called with refs to this VA.
    // Result is True if at least 1 reference found.
    function Enumerate(VA: TVA; cb: TRefEnumFunc; ud: Pointer): BOOL; stdcall;
    // Check if there are references for VA.
    function HasReferences(VA: TVA): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION 'Byte Patterns'}

  IVDBytePattern = interface(IUnknown)
    ['{A3432086-FE98-42C6-A0E4-E07866AFF997}']
    // Get count of pattern bytes.
    function GetSize: uint; stdcall;
    // Get pointer to pattern bytes.
    function GetData: PByte; stdcall;
    // Get pointer to pattern mask.
    function GetMask: PByte; stdcall;
  end;
{$ENDREGION}
{$REGION 'Import and Export'}

  // Library name and function name are case-sensitive.

  TVDSymbolOrdinal = uint16;
  PVDSymbolOrdinal = ^TVDSymbolOrdinal;

  TVDImportLibUID = type uint16;
  TVDImpLibEnumFunc = function(UID: TVDImportLibUID; LIB: BSTR_IN; ud: Pointer): BOOL; stdcall;
  TVDImpSymEnumFunc = function(VA: TVA; LibStr, SymStr: BSTR_IN; SymOrd: TVDSymbolOrdinal; ud: Pointer): BOOL; stdcall;

  // VA can have only 1 import.
  IVDImportSymbols = interface(IVDUpdateable)
    ['{C3B1B00C-199E-4470-9152-3E121359EB3F}']
    // Put import.
    function Put(VA: TVA; LibName, SymbolName: BSTR_IN;
      Ordinal: TVDSymbolOrdinal): BOOL; stdcall;

    // Check if there is import at VA.
    function Exists(VA: TVA): BOOL; stdcall;

    // Try get symbol by VA. Result is True if symbol found.
    function Get(VA: TVA; out LibName: BSTR; out SymName: BSTR;
      out SymOrd: TVDSymbolOrdinal): BOOL; stdcall;

    // Enumerate imported libraries.
    procedure EnumLibs(cb: TVDImpLibEnumFunc; ud: Pointer); stdcall;

    // Enumerate imported symbols from VA0 to VA1 inclusively.
    // VA0 or VA1 can BAD_VA to use extreme VA.
    procedure EnumSyms(VA0, VA1: TVA; cb: TVDImpSymEnumFunc; ud: Pointer); stdcall;
  end;

  TVDExportSymbolsEnumFunc = function(
    VA: TVA;
    SymbolName: BSTR_IN;
    Ordinal: TVDSymbolOrdinal;
    ud: Pointer): BOOL; stdcall;

  // VA can have few symbols with different ordinals.
  // SymbolName can be nil if exported by ordinal only.
  // Ordinal 0 is reserved for main entry point.
  IVDExportSymbols = interface(IVDUpdateable)
    ['{D8CEAD03-A135-4841-BA51-043665F0EF2F}']
    // Add symbol if it does not exist.
    function Put(VA: TVA; SymbolName: BSTR_IN; Ordinal: TVDSymbolOrdinal): BOOL; stdcall;
    // Enumerate symbols from VA0 to VA1(inclusive).
    // If VA0 is BAD_VA, scan from first symbol.
    // If VA1 is BAD_VA, scan to last symbol.
    // Result is True if at least on symbol found.
    // cb can be nil
    function Enumerate(VA0, VA1: TVA; cb: TVDExportSymbolsEnumFunc; ud: Pointer): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION  'User Data'}

  // User Data stored in Database.
  // Keep Key short (<= 64 is OK)
  IVDUserData = interface(IUnknown)
    ['{DAA5767D-727D-47DD-9389-449B7733DAC8}']
    // Put Key/Value pair.
    function Put(Key: Pointer; KeySize: UInt32; Data: Pointer; DataSize: UInt32): BOOL; stdcall;
    // DataSize [in/out] size of Data buffer, out is size of whole data.
    // If result is False, Key not found.
    function Get(Key: Pointer; KeySize: UInt32; Data: Pointer; DataSize: PUInt): BOOL; stdcall;
    // Result is True if key found and deleted.
    function Del(Key: Pointer; KeySize: UInt32): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION 'Problem list'}

  TProblemKind = uint8;

  TProblemKinds = record
  const
    PROBLEM_NULL                = 0;
    PROBLEM_DECODE_BASIC_ERROR  = 1; // cpu failed to decode instruction (basic)
    PROBLEM_UNKNOWN_BRANCH_ADDR = 2;
    PROBLEM_LAST                = 3;
  end;

  TProblemEnumFunc = function(VA: TVA; Kind: TProblemKind; ud: Pointer): BOOL; stdcall;

  IVDProblems = interface(IVDUpdateable)
    ['{C56204BD-A7A5-4278-8668-1EC21011C0A7}']
    function Put(VA: TVA; Kind: TProblemKind): BOOL; stdcall;
    function Enumerate(VA0, VA1: TVA; cb: TProblemEnumFunc; ud: Pointer): BOOL; stdcall;
    function Delete(VA: TVA; Kind: TProblemKind): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION 'Core Data'}

  // Screen position of cursor.
  TVDVAPos = packed record
    ScrVA: TVA; // VA at screen top
    CurVA: TVA; // VA at cursor
    X: Int16;
  end;

  PVDVAPos = ^TVDVAPos;

  // Base core data.
  TVDCoreData = packed record
    Version: TVDVersion;
    CodeType: array [0 .. 15] of AnsiChar; // primary code type; use TCpuName constants
    Endianness: TEndianness;               // machine endinannes
    AddressSize: UInt32;                   // machine pointer size in bytes
  end;

  PVDCoreData = ^TVDCoreData;
{$ENDREGION}
{$REGION 'Code Analysis'}

  TVDCodeAnalysis = record
  const
    CA_NULL            = 0;
    CA_STEP_INTO_CALL  = 1 shl 0;  // step into calls
    CA_MARK_DATA_REFS  = 1 shl 1;  // mark usage of data with refs
    CA_UNDF_DATA_REFS  = 1 shl 2;  // delete data refs
    CA_MARK_CODE_REFS  = 1 shl 3;  // mark code references
    CA_UNDF_CODE_REFS  = 1 shl 4;  // delete code refs
    CA_MAKE_CODE       = 1 shl 5;  // define code
    CA_UNDF_CODE       = 1 shl 6;  // undefine code
    CA_SKIP_TRACELOOP  = 1 shl 7;  // internal, don't use
    CA_CHUNK_START     = 1 shl 8;  // va for analysis is start of chunk
    CA_CONST_AS_ADDR   = 1 shl 9;  // try to represent constant as address
    CA_CALLS_AS_JUMPS  = 1 shl 10; // calls are handled as jumps
    CA_CREATE_FUNCTION = 1 shl 11; //
  end;

  TVDCodeAnalysisFlags = UInt32;

{$ENDREGION}
{$REGION 'Search'}

  IVDSearch = interface(IUnknown)
    ['{BD4A7E6A-5D52-46E9-886C-3B59334B3838}']

    // Search bytes starting from VA. Then VA is set to address where search stopped.
    // If result is True, pattern matched.
    function Pattern(VA: PVA; EndVA: TVA; Pattern: IVDBytePattern; Direction: int): BOOL; stdcall;
    function Bytes(VA: PVA; EndVA: TVA; PatternText: BSTR_IN; Direction: int): BOOL; stdcall;

    // Find string starting from VA to EndVA.
    // VA will hold last scanned address (either if string found or not).
    // EndVA can be BAD_VA to declare max address.
    // CodePage can be 0 if you don't care.
    function &String(VA: PVA; EndVA: TVA; Text: BSTR_IN; CodePage: TCodePage;
      CaseSensitive: BOOL; Direction: int): BOOL; stdcall;
  end;
{$ENDREGION}
{$REGION 'Evaluation'}

  TNumberEvaluateFlag = record
  const
    // Resolve name->va during evaluation.
    EVAL_RESOLVE_NAMES = 1 shl 0;

    EVAL_NONE    = 0;
    EVAL_DEFAULT = EVAL_RESOLVE_NAMES;
  end;

  TExpressionEvaluateFlags = UInt32;
{$ENDREGION}

  TMakeTypeFlag = record
    // const
    // RemoveExisting = 1 shl 0; // remove existing type
  end;

  TMakeTypeFlags = type UInt32;

  IVDGlobalBasicBlocks = interface(IUnknown)
    ['{D12C64DE-528A-4193-B182-07710C96AB0D}']
    function Get(VA: TVA): IVDVARegion; stdcall;
  end;

{$REGION 'Core'}

  // Main interface.
  IVDCore = interface(IUnknown)
    ['{AA8E4A3E-0AB6-445B-B3DD-F140D7487F64}']

    // Sub-systems.
    function GetData: PVDCoreData; stdcall;
    function GetPluginManager: IVDPluginManager; stdcall;
    function GetInputFile: IVDInputFile; stdcall;
    function GetMessageManager: IVDMessageManager; stdcall;
    function GetVM: IVDVirtualMemory; stdcall;
    function GetDecoder: IVDDecoder; stdcall;
    function GetLog: IVDLog; stdcall;
    function GetMenu: IVDMenu; stdcall;
    function GetNames: IVDNames; stdcall;
    function GetComments: IVDComments; stdcall;
    function GetRefs: IVDVAReferences; stdcall;
    function GetUI: IVDUserInterface; stdcall;
    function GetUserData: IVDUserData; stdcall;
    function GetTypeLib: IVDTypeLibrary; stdcall;
    function GetTypeMgr: IVDTypeMgr; stdcall;
    function GetJobs: IVDJobs; stdcall;
    function GetCPUs: IVDCpus; stdcall;
    function GetFunctions: IVDFunctions; stdcall;
    function GetImportSymbols: IVDImportSymbols; stdcall;
    function GetExportSymbols: IVDExportSymbols; stdcall;
    function GetDebugSession: IVDDebugSession; stdcall;
    function GetSearch: IVDSearch; stdcall;
    function GetStrings: IVDStrings; stdcall;
    function GetProblems: IVDProblems; stdcall;
    function GetGlobalBasicBlocks: IVDGlobalBasicBlocks;
    function GetAnalysisHelper: IVDAnalysisHelper;

    // Other.

    // Check if database opened.
    function IsDatabaseOpened: BOOL; stdcall;
    // Open Input File or Database.
    function OpenPath(Path: BSTR): BOOL; stdcall;
    // Create new empty database.
    // Path defines database directory.
    function CreateNewDatabase(Path: BSTR): BOOL; stdcall;
    // Close current database.
    // Must be called before CoreFree.
    procedure CloseDatabase(Delete: BOOL); stdcall;
    // Remove garbage.
    procedure CleanDatabase; stdcall;
    // Save database, return True on success.
    function SaveDatabase: BOOL; stdcall;
    // Save database with name, return True on success.
    function SaveDatabaseAs(FileName: BSTR): BOOL; stdcall;

    // Zip database.
    // todo: DON'T USE. IT IS FOR INTERNAL USE ONLY.
    function ZipDatabase: BOOL; stdcall;

    // UI.

    // todo: move this part to UI

    // Get current position (if available).
    function GetVAPos(Pos: PVDVAPos): BOOL; stdcall;
    // Get cursor VA of main window.
    function GetUserVA: TVA; stdcall;
    procedure SetUserVA(VA: TVA); stdcall;
    // Get analysis VA.
    function GetAnalysisVA: TVA; stdcall;
    // Change current VA position.
    function ChangeVAPos(Pos: PVDVAPos; Remember: BOOL = False): BOOL; stdcall;
    // Simplified version.
    function ChangeVA(VA: TVA; Remember: BOOL = False): BOOL; stdcall;
    // Go forward or backward by list of remembered VAs.
    function NavigateDirection(Direction: int = 1): BOOL; stdcall;
    // Clear list of remembered VAs.
    procedure NavigateListClear; stdcall;

    // Other.

    // Try to get VA from Text (i.e. convert number or name to VA).
    function EvaluateVA(
      Expr: BSTR_IN;
      out VA: TVA;
      Flags: TExpressionEvaluateFlags = TNumberEvaluateFlag.EVAL_DEFAULT): BOOL; stdcall;

    function EvaluateExpr(
      Expr: BSTR_IN;
      out Value: IVDConstExpression;
      Flags: TExpressionEvaluateFlags = TNumberEvaluateFlag.EVAL_DEFAULT
      ): BOOL; stdcall;

    // Flags: see TMakeTypeFlag
    function MakeType(VA: TVA; TypeStr: BSTR_IN; Flags: TMakeTypeFlags = 0): UInt32; stdcall;

    // Undefine region and return size of undefined bytes.
    // Default Size of 1 means at least 1 byte of some item must be deleted.
    // If result is 0 nothing was deleted.
    function Undefine(VA: TVA; Size: SIZE_T = 1): SIZE_T; stdcall;

    // Make data at VA a pointer (i.e. offset to some address).
    function MakePointer(VA: TVA; Flags: TMakeTypeFlags = 0): SIZE_T; stdcall;

    // VA1 is last address to dump (i.e. inclusive).
    procedure DumpDatabaseText(VA0, VA1: TVA; FileName: BSTR_IN); stdcall;

    // Dump all exported symbols to file.
    procedure DumpDatabaseExports(FileName: BSTR_IN); stdcall;

    // Trace code flow and make code, visiting sub-procedures (optionally).
    // If CodeType is nil then default code type used.
    function CodeAnalysis(VA: TVA; CodeType: BSTR_IN; Flags: TVDCodeAnalysisFlags): IVDFunction; stdcall;

    // Properties.
    property PluginMgr: IVDPluginManager read GetPluginManager;
    property InputFile: IVDInputFile read GetInputFile;
    property Msg: IVDMessageManager read GetMessageManager;
    property VM: IVDVirtualMemory read GetVM;
    property Decoder: IVDDecoder read GetDecoder;
    property Log: IVDLog read GetLog;
    property Menu: IVDMenu read GetMenu;
    property Names: IVDNames read GetNames;
    property Comments: IVDComments read GetComments;
    property Refs: IVDVAReferences read GetRefs;
    property UI: IVDUserInterface read GetUI;
    property UserData: IVDUserData read GetUserData;
    property TypeLib: IVDTypeLibrary read GetTypeLib;
    property TypeMgr: IVDTypeMgr read GetTypeMgr;
    property Jobs: IVDJobs read GetJobs;
    property CPUs: IVDCpus read GetCPUs;
    property Functions: IVDFunctions read GetFunctions;
    property ImportSymbols: IVDImportSymbols read GetImportSymbols;
    property ExportSymbols: IVDExportSymbols read GetExportSymbols;
    property DebugSession: IVDDebugSession read GetDebugSession;
    property Search: IVDSearch read GetSearch;
    property Strings: IVDStrings read GetStrings;
    property Problems: IVDProblems read GetProblems;
    property BasicBlocks: IVDGlobalBasicBlocks read GetGlobalBasicBlocks;
    property AnalysisHelper: IVDAnalysisHelper read GetAnalysisHelper;
  end;

{$ENDREGION}
{$REGION 'Functions'}

  // Get core library singleton.
  // CreateIfNotExists should be called once on startup.
function CoreGet(
  CreateIfNotExists: boolean = False;
  ListenerProc: TVDMessageListenerProc = nil
  ): IVDCore; stdcall;
  external LIB;

// Free core library. Should be called on shutdown.
procedure CoreFree(); stdcall;
  external LIB;

// Get Input/Output interface.
function IOGet: IVDIO; stdcall;
  external LIB;

// Create text layout.
function CreateTextLayout(Flags: TVDTextFlags): IVDTextLayout; stdcall;
  external LIB;

function CreateVATextLayout(Flags: TVDTextFlags): IVDVATextLayout; stdcall;
  external LIB;

// Convert text to byte pattern. Can be used for search later.
function CreateBytePattern(Text: BSTR; Mask: Byte): IVDBytePattern; stdcall;
  external LIB;

// Create new type library.
function TypeLibraryCreate(Name: BSTR_IN; FileName: BSTR_IN = nil): IVDTypeLibrary; stdcall;
  external LIB;

// Load existing type library.
function TypeLibraryLoad(FileName: BSTR_IN): IVDTypeLibrary; stdcall;
  external LIB;

// If Lib isn't nil, created types are added to the Lib.
function TypeFactoryCreate(LIB: IVDTypeLibrary): IVDTypeFactory; stdcall;
  external LIB;

{$ENDREGION}

function GetSystemEndianness: TEndianness; stdcall;
  external LIB;

// todo: to some interface
function AnalyseCodeIntoFunction(VA: TVA; CodeType: BSTR_IN): IVDFunction; stdcall;
  external LIB;

procedure CreateConstExprFromUInt(Value: UInt64; out Expr: IVDConstExpression); stdcall;
  external LIB;

//procedure IrPrintFunction(Func: IVDFunction; Layout: IVDVATextLayout); stdcall;
//  external LIB;

implementation


end.
