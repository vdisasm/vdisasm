{
  Pascal conversion was made for VDisAsm project.
  http://vdisasm.com
}

unit ELF.Types;

interface

{$ALIGN OFF}
{$MINENUMSIZE 4}


type
  // 32-Bit Data Types
  Elf32_Addr = type uint32; // 4 Unsigned program address
  Elf32_Off = type uint32;  // 4 Unsigned file offset
  Elf32_Half = type uint16; // 2 Unsigned medium integer
  Elf32_Word = type uint32; // 4 Unsigned integer
  Elf32_Sword = type int32; // 4 Signed integer

  // 64-Bit Data Types
  Elf64_Addr = type uint64;  // 8	Unsigned program address
  Elf64_Off = type uint64;   // 8	Unsigned file offset
  Elf64_Half = type uint16;  // 2	Unsigned medium integer [same as 32]
  Elf64_Word = type uint32;  // 4	Unsigned integer        [same as 32]
  Elf64_Sword = type int32;  // 4	Signed integer          [same as 32]
  Elf64_Xword = type uint64; // 8	Unsigned long integer
  Elf64_Sxword = type int64; // 8	Signed long integer

  //
  // ELF Identification (e_ident)
  //

const
  EI_MAG0       = 0; // File identification
  EI_MAG1       = 1; // File identification
  EI_MAG2       = 2; // File identification
  EI_MAG3       = 3; // File identification
  EI_CLASS      = 4; // File class, or capacity
  EI_DATA       = 5; // Data encoding
  EI_VERSION    = 6; // File version; EV_CURRENT
  EI_OSABI      = 7; // Operating system/ABI identification
  EI_ABIVERSION = 8; // ABI version
  EI_PAD        = 9; // Start of padding bytes

  // Sig for EI_MAG0 .. EI_MAG3
  ELFMAG0 = byte($7F); // e_ident[EI_MAG0]
  ELFMAG1 = byte('E'); // e_ident[EI_MAG1]
  ELFMAG2 = byte('L'); // e_ident[EI_MAG2]
  ELFMAG3 = byte('F'); // e_ident[EI_MAG3]

  // EI_CLASS
  ELFCLASSNONE = 0; // Invalid class
  ELFCLASS32   = 1; // 32-bit objects
  ELFCLASS64   = 2; // 64-bit objects

  // EI_DATA
  ELFDATANONE = 0; // Invalid data encoding
  ELFDATA2LSB = 1; // Least significant byte; little-endian
  ELFDATA2MSB = 2; // Most significant byte; big-endian

  // EI_VERSION / e_version
  EV_NONE    = 0; // Invalid version
  EV_CURRENT = 1; // Current version; original file format

  // EI_OSABI
  ELFOSABI_NONE    = 0;         // No extensions or unspecified
  ELFOSABI_HPUX    = 1;         // Hewlett-Packard HP-UX
  ELFOSABI_NETBSD  = 2;         // NetBSD
  ELFOSABI_GNU     = 3;         // GNU
  ELFOSABI_LINUX   = 3;         // Linux (historical - alias for ELFOSABI_GNU)
  ELFOSABI_SOLARIS = 6;         // Sun Solaris
  ELFOSABI_AIX     = 7;         // AIX
  ELFOSABI_IRIX    = 8;         // IRIX
  ELFOSABI_FREEBSD = 9;         // FreeBSD
  ELFOSABI_TRU64   = 10;        // Compaq TRU64 UNIX
  ELFOSABI_MODESTO = 11;        // Novell Modesto
  ELFOSABI_OPENBSD = 12;        // Open BSD
  ELFOSABI_OPENVMS = 13;        // Open VMS
  ELFOSABI_NSK     = 14;        // Hewlett-Packard Non-Stop Kernel
  ELFOSABI_AROS    = 15;        // Amiga Research OS
  ELFOSABI_FENIXOS = 16;        // The FenixOS highly scalable multi-core OS
  { 64-255 }                    // Architecture-specific value range

type
  TElfIdent = record
    { 00 } Magic: array [0 .. 3] of byte;
    { 04 } &Class: byte;     // ELFCLASSxxx
    { 05 } Data: byte;       // ELFDATAxxx
    { 06 } Version: byte;    // EV_CURRENT
    { 07 } OSABI: byte;      //
    { 08 } ABIVersion: byte; //
    { 09 } Pad: array [9 .. 15] of byte;
  end;

  //
  // ELF Header
  //

const
  EI_NIDENT = 16;

  // e_type
  ET_NONE   = 0;     // No file type
  ET_REL    = 1;     // Relocatable file
  ET_EXEC   = 2;     // Executable file
  ET_DYN    = 3;     // Shared object file
  ET_CORE   = 4;     // Core file
  ET_LOOS   = $FE00; // Operating system-specific
  ET_HIOS   = $FEFF; // Operating system-specific
  ET_LOPROC = $FF00; // Processor-specific
  ET_HIPROC = $FFFF; // Processor-specific

  // e_machine
{$I 'ELF.Machine.Consts.Inc'}


type
  Elf32_Ehdr = record
    e_ident: TElfIdent;
    e_type: Elf32_Half;    // object file type
    e_machine: Elf32_Half; // required architecture
    e_version: Elf32_Word; // object file version
    e_entry: Elf32_Addr;   // entry VA or 0
    e_phoff: Elf32_Off;    // program header table’s file offset in bytes or 0
    e_shoff: Elf32_Off;    // section header table’s file offset in bytes or 0
    e_flags: Elf32_Word;   // processor-specific flags; see EF_%machine%_%flag%
    e_ehsize: Elf32_Half;  // ELF header’s size in bytes

    // Size in bytes of one entry in the file’s program header table;
    // all entries are the same size.
    e_phentsize: Elf32_Half;

    // Number of entries in the program header table.
    e_phnum: Elf32_Half;

    // Section header’s size in bytes. A section header is one entry in
    // the section header table; all entries are the same size.
    e_shentsize: Elf32_Half;

    // Number of entries in the section header table.
    e_shnum: Elf32_Half;

    // Section header table index of the entry associated with the section
    // name string table.
    e_shstrndx: Elf32_Half;
  end;

  Elf64_Ehdr = record
    e_ident: TElfIdent;
    e_type: Elf64_Half;
    e_machine: Elf64_Half;
    e_version: Elf64_Word;
    e_entry: Elf64_Addr;
    e_phoff: Elf64_Off;
    e_shoff: Elf64_Off;
    e_flags: Elf64_Word;
    e_ehsize: Elf64_Half;
    e_phentsize: Elf64_Half;
    e_phnum: Elf64_Half;
    e_shentsize: Elf64_Half;
    e_shnum: Elf64_Half;
    e_shstrndx: Elf64_Half;
  end;

  //
  // Sections
  //

  // Special Section Indexes
const
  SHN_UNDEF = $0;

  // This value specifies the lower bound of the range of reserved indexes.
  SHN_LORESERVE = $FF00;

  // Values in this inclusive range are reserved for processor-specific semantics.
  SHN_LOPROC = $FF00;
  SHN_HIPROC = $FF1F;

  // Values in this inclusive range are reserved for operating system-specific semantics.
  SHN_LOOS = $FF20;
  SHN_HIOS = $FF3F;

  // This value specifies absolute values for the corresponding reference. For example,
  // symbols defined relative to section number SHN_ABS have absolute values and are
  // not affected by relocation.
  SHN_ABS = $FFF1;

  // Symbols defined relative to this section are common symbols, such as FORTRAN
  // COMMON or unallocated C external variables.
  SHN_COMMON = $FFF2;

  // This value is an escape value. It indicates that the actual section header
  // index is too large to fit in the containing field and is to be found in another
  // location (specific to the structure where it appears).
  SHN_XINDEX = $FFFF;

  // This value specifies the upper bound of the range of reserved indexes.
  // The system reserves indexes between SHN_LORESERVE and SHN_HIRESERVE, inclusive;
  // the values do not reference the section header table. The section header table
  // does not contain entries for the reserved indexes.
  SHN_HIRESERVE = $FFFF;

  // Section Header
type
  Elf32_Shdr = record
    // Index into the section header string table section.
    // String is null-terminated.
    sh_name: Elf32_Word;

    sh_type: Elf32_Word; // section’s contents and semantics

    // Sections support 1-bit flags that describe miscellaneous attributes.
    sh_flags: Elf32_Word;

    // If the section will appear in the memory image of a process, this member
    // gives the address at which the section’s first byte should reside.
    // Otherwise, the member contains 0
    sh_addr: Elf32_Addr;

    // byte offset from the beginning of the file to the first byte in the section.
    sh_offset: Elf32_Off;

    // This member gives the section’s size in bytes. Unless the section type is
    // SHT_NOBITS, the section occupies sh_size bytes in the file. A section of type
    // SHT_NOBITS may have a non-zero size, but it occupies no space in the file.
    sh_size: Elf32_Word;

    // This member holds a section header table index link, whose interpretation depends
    // on the section type.
    sh_link: Elf32_Word;

    // This member holds extra information, whose interpretation depends on the section
    // type. A table below describes the values.
    sh_info: Elf32_Word;

    // Some sections have address alignment constraints. For example, if a section holds a
    // doubleword, the system must ensure doubleword alignment for the entire section.
    // That is, the value of sh_addr must be congruent to 0, modulo the value of
    // sh_addralign. Currently, only 0 and positive integral powers of two are allowed.
    // Values 0 and 1 mean the section has no alignment constraints.
    sh_addralign: Elf32_Word;

    // Some sections hold a table of fixed-size entries, such as a symbol table. For such a section,
    // this member gives the size in bytes of each entry. The member contains 0 if the
    // section does not hold a table of fixed-size entries.
    sh_entsize: Elf32_Word;
  end;

  Elf64_Shdr = record
    sh_name: Elf64_Word;
    sh_type: Elf64_Word;
    sh_flags: Elf64_Xword;
    sh_addr: Elf64_Addr;
    sh_offset: Elf64_Off;
    sh_size: Elf64_Xword;
    sh_link: Elf64_Word;
    sh_info: Elf64_Word;
    sh_addralign: Elf64_Xword;
    sh_entsize: Elf64_Xword;
  end;

  // Section Types, sh_type
const
  SHT_NULL           = 0;         // No associated section (inactive entry).
  SHT_PROGBITS       = 1;         // Program-defined contents.
  SHT_SYMTAB         = 2;         // Symbol table.
  SHT_STRTAB         = 3;         // String table.
  SHT_RELA           = 4;         // Relocation entries; explicit addends.
  SHT_HASH           = 5;         // Symbol hash table.
  SHT_DYNAMIC        = 6;         // Information for dynamic linking.
  SHT_NOTE           = 7;         // Information about the file.
  SHT_NOBITS         = 8;         // Data occupies no space in the file.
  SHT_REL            = 9;         // Relocation entries; no explicit addends.
  SHT_SHLIB          = 10;        // Reserved.
  SHT_DYNSYM         = 11;        // Symbol table.
  SHT_INIT_ARRAY     = 14;        // Pointers to initialization functions.
  SHT_FINI_ARRAY     = 15;        // Pointers to termination functions.
  SHT_PREINIT_ARRAY  = 16;        // Pointers to pre-init functions.
  SHT_GROUP          = 17;        // Section group.
  SHT_SYMTAB_SHNDX   = 18;        // Indices for SHN_XINDEX entries.
  SHT_LOOS           = $60000000; // Lowest operating system-specific type.
  SHT_GNU_ATTRIBUTES = $6FFFFFF5; // Object attributes.
  SHT_GNU_HASH       = $6FFFFFF6; // GNU-style hash table.
  SHT_GNU_verdef     = $6FFFFFFD; // GNU version definitions.
  SHT_GNU_verneed    = $6FFFFFFE; // GNU version references.
  SHT_GNU_versym     = $6FFFFFFF; // GNU symbol versions table.
  SHT_HIOS           = $6FFFFFFF; // Highest operating system-specific type.
  SHT_LOPROC         = $70000000; // Lowest processor arch-specific type.

  // Exception Index table
  SHT_ARM_EXIDX = Cardinal($70000001);
  // BPABI DLL dynamic linking pre-emption map
  SHT_ARM_PREEMPTMAP = Cardinal($70000002);
  // Object file compatibility attributes
  SHT_ARM_ATTRIBUTES     = Cardinal($70000003);
  SHT_ARM_DEBUGOVERLAY   = Cardinal($70000004);
  SHT_ARM_OVERLAYSECTION = Cardinal($70000005);
  SHT_HEX_ORDERED        = $70000000; // Link editor is to sort the entries in
  // this section based on their sizes
  SHT_X86_64_UNWIND = $70000001; // Unwind information

  SHT_MIPS_REGINFO = $70000006; // Register usage information
  SHT_MIPS_OPTIONS = $7000000D; // General options

  SHT_HIPROC = $7FFFFFFF; // Highest processor arch-specific type.
  SHT_LOUSER = $80000000; // Lowest type reserved for applications.
  SHT_HIUSER = $FFFFFFFF; // Highest type reserved for applications.

  // Section Attribute Flags, sh_flags

  // Section flags.
const
  // Section data should be writable during execution.
  SHF_WRITE = $1;

  // Section occupies memory during program execution.
  SHF_ALLOC = $2;

  // Section contains executable machine instructions.
  SHF_EXECINSTR = $4;

  // The data in this section may be merged.
  SHF_MERGE = $10;

  // The data in this section is null-terminated strings.
  SHF_STRINGS = $20;

  // A field in this section holds a section header table index.
  SHF_INFO_LINK = $40;

  // Adds special ordering requirements for link editors.
  SHF_LINK_ORDER = $80;

  // This section requires special OS-specific processing to avoid incorrect
  // behavior.
  SHF_OS_NONCONFORMING = $100;

  // This section is a member of a section group.
  SHF_GROUP = $200;

  // This section holds Thread-Local Storage.
  SHF_TLS = $400;

  SHF_COMPRESSED = $800;

  // Start of target-specific flags.

  /// XCORE_SHF_CP_SECTION - All sections with the "c" flag are grouped
  /// together by the linker to form the constant pool and the cp register is
  /// set to the start of the constant pool by the boot code.
  XCORE_SHF_CP_SECTION = $800;

  /// XCORE_SHF_DP_SECTION - All sections with the "d" flag are grouped
  /// together by the linker to form the data section and the dp register is
  /// set to the start of the section by the boot code.
  XCORE_SHF_DP_SECTION = $1000;

  SHF_MASKOS = $0FF00000;

  // Bits indicating processor-specific flags.
  SHF_MASKPROC = $F0000000;

  // If an object file section does not have this flag set, then it may not hold
  // more than 2GB and can be freely referred to in objects using smaller code
  // models. Otherwise, only objects using larger code models can refer to them.
  // For example, a medium code model object can refer to data in a section that
  // sets this flag besides being able to refer to data in a section that does
  // not set it; likewise, a small code model object can refer only to code in a
  // section that does not set this flag.
  SHF_X86_64_LARGE = $10000000;

  // All sections with the GPREL flag are grouped into a global data area
  // for faster accesses
  SHF_HEX_GPREL = $10000000;

  // Do not strip this section. FIXME: We need target specific SHF_ enums.
  SHF_MIPS_NOSTRIP = $8000000;

  // see SHF_COMPRESSED
type
  Elf32_Chdr = record
    ch_type: Elf32_Word;
    ch_size: Elf32_Word;
    ch_addralign: Elf32_Word;
  end;

  Elf64_Chdr = record
    ch_type: Elf64_Word;
    ch_reserved: Elf64_Word;
    ch_size: Elf64_Xword;
    ch_addralign: Elf64_Xword;
  end;

  //
  // Symbol Table
  //

const
  STN_UNDEF = 0;

type
  Elf32_Sym = record
    // Index into the object file’s symbol string table, which holds the
    // character representations of the symbol names; 0 if symbol is unnamed
    st_name: Elf32_Word;

    // This member gives the value of the associated symbol. Depending on the context, this
    // may be an absolute value, an address, etc.
    st_value: Elf32_Addr;

    st_size: Elf32_Word;

    // This member specifies the symbol’s type and binding attributes. A list of the values and
    // meanings appears below. The following code shows how to manipulate the values.
    // #define ELF32STBIND (i)   ((i)>>4)
    // #define ELF32STTYPE (i)   ((i)&0xf)
    // #define ELF32STINFO (b,t) (((b)<<4)+((t)&0xf))
    st_info: byte;

    st_other: byte;

    // Every symbol table entry is ‘‘defined’’ in relation to some section; this member holds the
    // relevant section header table index.
    st_shndx: Elf32_Half;

    function IsNull: boolean; inline;
  end;

  Elf64_Sym = record
    st_name: Elf64_Word;
    st_info: byte;
    st_other: byte;
    st_shndx: Elf64_Half;
    st_value: Elf64_Addr;
    st_size: Elf64_Xword;
  end;

  // Symbol Binding, ELF32_ST_BIND
const
  // Local symbols are not visible outside the object file containing their definition. Local
  // symbols of the same name may exist in multiple files without interfering with each
  // other.
  STB_LOCAL = 0;

  // Global symbols are visible to all object files being combined. One file’s definition of a
  // global symbol will satisfy another file’s undefined reference to the same global symbol.
  STB_GLOBAL = 1;

  // Weak symbols resemble global symbols, but their definitions have lower precedence.
  STB_WEAK = 2;

  // Values in this inclusive range are reserved for processor-specific semantics.
  STB_LOPROC = 13;
  STB_HIPROC = 15;

  // Symbol Types, ELF32_ST_TYPE
  STT_NOTYPE  = 0; // The symbol type is not specified.
  STT_OBJECT  = 1; // This symbol is associated with a data object, such as a variable, an array, and so forth.
  STT_FUNC    = 2; // This symbol is associated with a function or other executable code.
  STT_SECTION = 3; // This symbol is associated with a section. Symbol table entries of this type exist primarily for relocation and normally have STB_LOCAL binding.
  STT_FILE    = 4; // Conventionally, the symbol's name gives the name of the source file associated with the object file.
  STT_COMMON  = 5; // This symbol labels an uninitialized common block. It is treated exactly the same as STT_OBJECT.
  STT_LOOS    = 10;
  STT_HIOS    = 12;
  STT_LOPROC  = 13;
  STT_HIPROC  = 15;

  // ELF Symbol Visibility
  STV_DEFAULT = 0;
  STV_INTERNAL = 1;
  STV_HIDDEN = 2;
  STV_PROTECTED = 3;

  //
  // Relocation
  //

  // Relocation Entries
type
  Elf32_Rel = record
    r_info: Elf32_Word;
    r_offset: Elf32_Addr;
  end;

  Elf32_Rela = record
    r_offset: Elf32_Addr;
    r_info: Elf32_Word;
    r_addend: Elf32_Sword;

    // #defineELF32RSYM(i)((i)>>8)
    // #defineELF32RTYPE(i)((unsignedchar)(i))
    // #defineELF32RINFO(s,t)(((s)<<8)+(unsignedchar)(t))
  end;

  Elf64_Rel = record
    r_offset: Elf64_Addr;
    r_info: Elf64_Xword;
  end;

  Elf64_Rela = record
    r_offset: Elf64_Addr;
    r_info: Elf64_Xword;
    r_addend: Elf64_Sxword;
  end;

  // Relocation Types
const
  // Name, Value, Field, Calculation
  R_386_NONE     = 0;  // none none
  R_386_32       = 1;  // word32 S + A
  R_386_PC32     = 2;  // word32 S + A - P
  R_386_GOT32    = 3;  // word32 G + A - P
  R_386_PLT32    = 4;  // word32 L + A - P
  R_386_COPY     = 5;  // none none
  R_386_GLOB_DAT = 6;  // word32 S
  R_386_JMP_SLOT = 7;  // word32 S
  R_386_RELATIVE = 8;  // word32 B + A
  R_386_GOTOFF   = 9;  // word32 S + A - GOT
  R_386_GOTPC    = 10; // word32 GOT + A - P

  //
  // Program Header
  //
type
  Elf32_Phdr = record
    // This member tells what kind of segment this array element describes or how to
    // interpret the array element's information.
    p_type: Elf32_Word;

    // This member gives the offset from the beginning of the file at which the first byte
    // of the segment resides.
    p_offset: Elf32_Off;

    // This member gives the virtual address at which the first byte of the segment resides
    // in memory.
    p_vaddr: Elf32_Addr;

    // On systems for which physical addressing is relevant, this member is reserved for
    // the segment's physical address. This member requires operating system specific
    // information, which is described in the appendix at the end of Book III.
    p_paddr: Elf32_Addr;

    // This member gives the number of bytes in the file image of the segment; it may be zero.
    p_filesz: Elf32_Word;

    // This member gives the number of bytes in the memory image of the segment; it may be zero.
    p_memsz: Elf32_Word;

    // This member gives flags relevant to the segment.
    p_flags: Elf32_Word;

    // Loadable process segments must have congruent values for p_vaddrand
    // p_offset, modulo the page size.This member gives the value to which the
    // segments are aligned in memory and in the file. Values 0 and 1 mean that no
    // alignment is required. Otherwise, p_alignshould be a positive, integral power of
    // 2, and p_addrshould equal p_offset, modulo p_align.
    p_align: Elf32_Word;
  end;

  Elf64_Phdr = record
    p_type: Elf64_Word;
    p_flags: Elf64_Word;
    p_offset: Elf64_Off;
    p_vaddr: Elf64_Addr;
    p_paddr: Elf64_Addr;
    p_filesz: Elf64_Xword;
    p_memsz: Elf64_Xword;
    p_align: Elf64_Xword;
  end;

  // Segment Types, p_type
  // see also http://www.sco.com/developers/gabi/latest/ch5.pheader.html
const
  // The array element is unused; other members' values are undefined. This type lets
  // the program header table have ignored entries.
  PT_NULL = 0;

  // The array element specifies a loadable segment, described by p_filesz and
  // p_memsz. The bytes from the file are mapped to the beginning of the memory
  // segment. If the segment's memory size (p_memsz) is larger than the file size
  // (p_filesz), the "extra" bytes are defined to hold the value 0 and to follow the
  // segment's initialized area. The file size may not be larger than the memory size.
  // Loadable segment entries in the program header table appear in ascending order,
  // sorted on the p_vaddrmember.
  PT_LOAD = 1;

  // The array element specifies dynamic linking information. See Book III.
  PT_DYNAMIC = 2;

  // The array element specifies the location and size of a null-terminated path name to
  // invoke as an interpreter. See Book III.
  PT_INTERP = 3;

  // The array element specifies the location and size of auxiliary information.
  PT_NOTE = 4;

  // This segment type is reserved but has unspecified semantics. See Book III.
  PT_SHLIB = 5;

  // The array element, if present, specifies the location and size of the program header
  // table itself, both in the file and in the memory image of the program. This segment
  // type may not occur more than once in a file. Moreover, it may occur only if the
  // program header table is part of the memory image of the program. If it is present,
  // it must precede any loadable segment entry. See "Program Interpreter" in the
  // appendix at the end of Book III for further information.
  PT_PHDR = 6;

  // The array element specifies the Thread-Local Storage template.
  // Implementations need not support this program table entry.
  PT_TLS = 7;

  // Number of defined types.
  PT_NUM = 8;

  // Values in this inclusive range are reserved for operating system-specific semantics.
  PT_LOOS         = $60000000; // Start of OS-specific
  PT_GNU_EH_FRAME = $6474E550; // GCC .eh_frame_hdr segment
  PT_GNU_STACK    = $6474E551; // Indicates stack executability
  PT_GNU_RELRO    = $6474E552; // Read-only after relocation
  PT_PAX_FLAGS    = $65041580; // Indicates PaX flag markings
  PT_LOSUNW       = $6FFFFFFA; //
  PT_SUNWBSS      = $6FFFFFFA; // Sun Specific segment
  PT_SUNWSTACK    = $6FFFFFFB; // Stack segment
  PT_HISUNW       = $6FFFFFFF; //
  PT_HIOS         = $6FFFFFFF; // End of OS-specific
  PT_LOPROC       = $70000000; // Start of processor-specific
  PT_HIPROC       = $7FFFFFFF; // End of processor-specific

  //
  // Segment Permissions
  //
const
  PF_X        = $1;        // Execute
  PF_W        = $2;        // Write
  PF_R        = $4;        // Read
  PF_MASKOS   = $0FF00000; // Unspecified
  PF_MASKPROC = $F0000000; // Unspecified

  //
  // Dynamic Section
  // http://www.sco.com/developers/gabi/latest/ch5.dynamic.html
  //

  // Dynamic Structure
type
  Elf32_Dyn = record
  private type
    t_d_un = record
      case byte of
        0:
          (d_val: Elf32_Word);
        1:
          (d_ptr: Elf32_Addr);
    end;
  public
    d_tag: Elf32_Sword;
    d_un: t_d_un;
  end;

  Elf64_Dyn = record
  private type
    t_d_un = record
      case byte of
        0:
          (d_val: Elf64_Xword);
        1:
          (d_ptr: Elf64_Addr);
    end;
  public
    d_tag: Elf64_Sxword;
    d_un: t_d_un;
  end;


  // Dynamic Array Tags, d_tag

const
  // Name, Value, d_un, Executable, Shared Object
  DT_NULL            = 0;         // ignored mandatory mandatory
  DT_NEEDED          = 1;         // d_val optional optional
  DT_PLTRELSZ        = 2;         // d_val optional optional
  DT_PLTGOT          = 3;         // d_ptr optional optional
  DT_HASH            = 4;         // d_ptr mandatory mandatory
  DT_STRTAB          = 5;         // d_ptr mandatory mandatory
  DT_SYMTAB          = 6;         // d_ptr mandatory mandatory
  DT_RELA            = 7;         // d_ptr mandatory optional
  DT_RELASZ          = 8;         // d_val mandatory optional
  DT_RELAENT         = 9;         // d_val mandatory optional
  DT_STRSZ           = 10;        // d_val mandatory mandatory
  DT_SYMENT          = 11;        // d_val mandatory mandatory
  DT_INIT            = 12;        // d_ptr optional optional
  DT_FINI            = 13;        // d_ptr optional optional
  DT_SONAME          = 14;        // d_val ignored optional
  DT_RPATH           = 15;        // d_val optional ignored
  DT_SYMBOLIC        = 16;        // ignored ignored optional
  DT_REL             = 17;        // d_ptr mandatory optional
  DT_RELSZ           = 18;        // d_val mandatory optional
  DT_RELENT          = 19;        // d_val mandatory optional
  DT_PLTREL          = 20;        // d_val optional optional
  DT_DEBUG           = 21;        // d_ptr optional ignored
  DT_TEXTREL         = 22;        // ignored optional optional
  DT_JMPREL          = 23;        // d_ptr optional optional
  DT_BIND_NOW        = 24;        //
  DT_INIT_ARRAY      = 25;        //
  DT_FINI_ARRAY      = 26;        //
  DT_INIT_ARRAYSZ    = 27;        //
  DT_FINI_ARRAYSZ    = 28;        //
  DT_RUNPATH         = 29;        //
  DT_FLAGS           = 30;        //
  DT_ENCODING        = 32;        //
  DT_PREINIT_ARRAY   = 32;        //
  DT_PREINIT_ARRAYSZ = 33;        //
  DT_LOOS            = $6000000D; //
  DT_HIOS            = $6FFFF000; //
  DT_LOPROC          = $70000000; // unspecified unspecified unspecified
  DT_HIPROC          = $7FFFFFFF; // unspecified unspecified unspecified

  // DT_FLAGS values

const
  DF_ORIGIN     = $1;
  DF_SYMBOLIC   = $2;
  DF_TEXTREL    = $4;
  DF_BIND_NOW   = $8;
  DF_STATIC_TLS = $10;

function elf_hash(name: pbyte): uint32;

function ELF32STBIND(i: byte): byte; inline;
function ELF32STTYPE(i: byte): byte; inline;
function ELF32STINFO(b, t: byte): byte; inline;

implementation


function elf_hash(name: pbyte): uint32;
var
  g: uint32;
begin
  result := 0;
  while name^ <> 0 do
  begin
    result := (result shl 4) + name^;
    inc(name);
    g := result and $F0000000;
    if (g <> 0) then
      result := result xor (g shr 24);
    result := result and (not g);
  end;
end;

{ Elf32_Sym }

function Elf32_Sym.IsNull: boolean;
begin
  result :=
    (Self.st_name = 0) and
    (Self.st_value = 0) and
    (Self.st_size = 0) and
    (Self.st_info = 0) and
    (Self.st_other = 0) and
    (Self.st_shndx = 0);
end;

// #define ELF32STBIND (i)   ((i)>>4)
function ELF32STBIND(i: byte): byte;
begin
  result := i shr 4;
end;

// #define ELF32STTYPE (i)   ((i)&0xf)
function ELF32STTYPE(i: byte): byte;
begin
  result := i and $F;
end;

// #define ELF32STINFO (b,t) (((b)<<4)+((t)&0xf))
function ELF32STINFO(b, t: byte): byte;
begin
  result := (b shl 4) or (t and $F);
end;

end.
