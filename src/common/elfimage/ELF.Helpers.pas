unit ELF.Helpers;

interface

uses
  System.SysUtils,
  ELF.Types;

function ElfIdentClassToString(value: byte): string;
function ElfIdentDataToString(value: byte): string;

function ElfMachineToString(e_machine: word): string;
function ElfTypeToString(e_type: word): string;
function ElfPTypeToString(p_type: Elf32_Word): string;
function ElfPFlagsToString(p_flags: Elf32_Word): string;
function ElfDTagToString(d_tag: Elf32_Sword): string;

function ElfStInfoBindToString(st_info: byte): string;
function ElfStInfoTypeToString(st_info: byte): string;

implementation

{$I 'ELF.Machine.Names.inc'}


function ElfIdentClassToString(value: byte): string;
begin
  case value of
    ELFCLASS32:
      result := '32-bit';
    ELFCLASS64:
      result := '64-bit';
  else
    result := format('0x%x', [value]);
  end;
end;

function ElfIdentDataToString(value: byte): string;
begin
  case value of
    ELFDATA2LSB:
      result := 'little-endian';
    ELFDATA2MSB:
      result := 'big-endian';
  else
    result := format('0x%x', [value]);
  end;
end;

function ElfMachineToString(e_machine: word): string;
begin
  case e_machine of
    Low(ELF_MACHINE_NAMES) .. High(ELF_MACHINE_NAMES):
      result := ELF_MACHINE_NAMES[e_machine];
    47787:
      result := 'Xilinx MicroBlaze';
  else
    result := format('0x%x', [e_machine]);
  end;
end;

function ElfTypeToString(e_type: word): string;
begin
  case e_type of
    ET_REL:
      result := 'Relocatable file';
    ET_EXEC:
      result := 'Executable file';
    ET_DYN:
      result := 'Shared object file';
    ET_CORE:
      result := 'Core file';
  else
    result := '';
  end;
end;

function ElfPTypeToString(p_type: Elf32_Word): string;
begin
  case p_type of
    PT_NULL:
      result := 'PT_NULL';
    PT_LOAD:
      result := 'PT_LOAD';
    PT_DYNAMIC:
      result := 'PT_DYNAMIC';
    PT_INTERP:
      result := 'PT_INTERP';
    PT_NOTE:
      result := 'PT_NOTE';
    PT_SHLIB:
      result := 'PT_SHLIB';
    PT_PHDR:
      result := 'PT_PHDR';
    PT_TLS:
      result := 'PT_TLS';
    PT_NUM:
      result := 'PT_NUM';
    // PT_LOOS:
    // Result := 'PT_LOOS';
    PT_GNU_EH_FRAME:
      result := 'PT_GNU_EH_FRAME';
    PT_GNU_STACK:
      result := 'PT_GNU_STACK';
    PT_GNU_RELRO:
      result := 'PT_GNU_RELRO';
    PT_PAX_FLAGS:
      result := 'PT_PAX_FLAGS';
    // PT_LOSUNW:
    // Result := 'PT_LOSUNW';
    PT_SUNWBSS:
      result := 'PT_SUNWBSS';
    PT_SUNWSTACK:
      result := 'PT_SUNWSTACK';
    // PT_HISUNW:
    // Result := 'PT_HISUNW';
    // PT_HIOS:
    // Result := 'PT_HIOS';
    PT_LOPROC:
      result := 'PT_LOPROC';
    PT_HIPROC:
      result := 'PT_HIPROC';
  else
    case p_type of
      PT_LOOS .. PT_HIOS:
        result := format('PT_OS(%x)', [p_type]);
      PT_LOPROC .. PT_HIPROC:
        result := format('PT_PROC(%x)', [p_type]);
    else
      result := format('?(%x)', [p_type]);
    end;
  end;
end;

function ElfPFlagsToString(p_flags: Elf32_Word): string;
const
  FLAGS: array [0 .. 2] of string = (
    'X', // 1
    'W', // 2
    'R'  // 4
    );
var
  s: string;
  u: uint32;
begin
  result := '';
  u := 1;
  for s in FLAGS do
  begin
    if (p_flags and u) <> 0 then
    begin
      if result <> '' then
        result := result + ',';
      result := result + s;
    end;
    u := u shl 1;
  end;
end;

function ElfDTagToString(d_tag: Elf32_Sword): string;
const
  grp0: array [0 .. 33] of string = (
    'DT_NULL',
    'DT_NEEDED',
    'DT_PLTRELSZ',
    'DT_PLTGOT',
    'DT_HASH',
    'DT_STRTAB',
    'DT_SYMTAB',
    'DT_RELA',
    'DT_RELASZ',
    'DT_RELAENT',
    'DT_STRSZ',
    'DT_SYMENT',
    'DT_INIT',
    'DT_FINI',
    'DT_SONAME',
    'DT_RPATH',
    'DT_SYMBOLIC',
    'DT_REL',
    'DT_RELSZ',
    'DT_RELENT',
    'DT_PLTREL',
    'DT_DEBUG',
    'DT_TEXTREL',
    'DT_JMPREL',
    'DT_BIND_NOW',
    'DT_INIT_ARRAY',
    'DT_FINI_ARRAY',
    'DT_INIT_ARRAYSZ',
    'DT_FINI_ARRAYSZ',
    'DT_RUNPATH',
    'DT_FLAGS',
    'DT_ENCODING',
    'DT_PREINIT_ARRAY',
    'DT_PREINIT_ARRAYSZ');

begin
  case d_tag of
    low(grp0) .. high(grp0):
      result := grp0[d_tag];
  else
    result := format('d_tag(%x)', [d_tag]);
  end;
end;

function ElfStInfoBindToString(st_info: byte): string;
begin
  case ELF32STBIND(st_info) of
    STB_LOCAL:
      result := 'STB_LOCAL';
    STB_GLOBAL:
      result := 'STB_GLOBAL';
    STB_WEAK:
      result := 'STB_WEAK';
    STB_LOPROC:
      result := 'STB_LOPROC';
    STB_HIPROC:
      result := 'STB_HIPROC';
  else
    result := format('0x%x', [ELF32STBIND(st_info)]);
  end;
end;

function ElfStInfoTypeToString(st_info: byte): string;
begin
  case ELF32STTYPE(st_info) of
    STT_NOTYPE:
      result := 'STT_NOTYPE';
    STT_OBJECT:
      result := 'STT_OBJECT';
    STT_FUNC:
      result := 'STT_FUNC';
    STT_SECTION:
      result := 'STT_SECTION';
    STT_FILE:
      result := 'STT_FILE';
    STT_COMMON:
      result := 'STT_COMMON';
    STT_LOOS:
      result := 'STT_LOOS';
    STT_HIOS:
      result := 'STT_HIOS';
    STT_LOPROC:
      result := 'STT_LOPROC';
    STT_HIPROC:
      result := 'STT_HIPROC';
  else
    result := format('0x%x', [ELF32STTYPE(st_info)]);
  end;
end;

end.
