unit ELF.Image;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  ELF.Dynamics,
  ELF.Helpers,
  ELF.Types;

type
  TELFSectionRec = record
    Hdr: Elf32_Shdr;
    Name: string;
    procedure Clear;
  end;

  PELFSectionRec = ^TELFSectionRec;

  TELFImage = class
  private type
    TProgamHeaders = TList<Elf32_Phdr>;
    TSectionHeaders = TList<TELFSectionRec>;
  private
    FHeader: Elf32_Ehdr;
    FProgramHeaders: TProgamHeaders;
    FSectionHeaders: TSectionHeaders;
    FDynamics: TELFDynamics;
    FIs32: boolean;
    function ReadFileHeader(const Stream: TStream): boolean;
    function CheckSignature: boolean;
    function ReadProgramHeaders(const Stream: TStream): boolean;
    function ReadSectionHeaders(const Stream: TStream): boolean;
    function GetEntryPoint: uint64;
    function GetProgramHeaderOffset(Index: integer): uint64; inline;
    function GetSectionHeaderOffset(Index: integer): uint64; inline;
    function ReadStringA(const Stream: TStream; Ofs: uint64; var Str: string): boolean;
  private
    procedure ProcessProgramHeaders(const Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromStream(Stream: TStream): boolean;
    function LoadFromFile(const FileName: string): boolean;

    // VA to stream/file offset.
    function VAToOffset(VA: uint64; out Offset: uint64): boolean;

    // VA to virtual address range.
    function VAToVRange(VA: uint64; out StartVA, StartOffset, Size: uint64): boolean;

    property Header: Elf32_Ehdr read FHeader;
    property ProgramHeaders: TProgamHeaders read FProgramHeaders;
    property SectionHeaders: TSectionHeaders read FSectionHeaders;
    property EntryPoint: uint64 read GetEntryPoint;
    property Dynamics: TELFDynamics read FDynamics;
    property Is32: boolean read FIs32;
  end;

implementation

{ TELFImage }      // 7F 45 76 70

function TELFImage.CheckSignature: boolean;
begin
  Result :=
    (FHeader.e_ident.Magic[0] = $7F) and
    (FHeader.e_ident.Magic[1] = $45) and
    (FHeader.e_ident.Magic[2] = $4C) and
    (FHeader.e_ident.Magic[3] = $46);
end;

constructor TELFImage.Create;
begin
  inherited;
  FProgramHeaders := TProgamHeaders.Create;
  FSectionHeaders := TSectionHeaders.Create;
end;

destructor TELFImage.Destroy;
begin
  FProgramHeaders.Free;
  FSectionHeaders.Free;
  FDynamics.Free;
  inherited;
end;

function TELFImage.GetEntryPoint: uint64;
begin
  Result := FHeader.e_entry;
end;

function TELFImage.GetProgramHeaderOffset(Index: integer): uint64;
begin
  Result := FHeader.e_phoff + FHeader.e_phentsize * Index;
end;

function TELFImage.GetSectionHeaderOffset(Index: integer): uint64;
begin
  Result := FHeader.e_shoff + FHeader.e_shentsize * Index;
end;

function TELFImage.LoadFromFile(const FileName: string): boolean;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TELFImage.LoadFromStream(Stream: TStream): boolean;
begin
  Result := False;

  if not ReadFileHeader(Stream) then
    exit;

  if not CheckSignature then
    exit;

  if not ReadProgramHeaders(Stream) then
    exit;

  if not ReadSectionHeaders(Stream) then
    exit;

  ProcessProgramHeaders(Stream);

  Result := True;
end;

procedure TELFImage.ProcessProgramHeaders(const Stream: TStream);
var
  Item: Elf32_Phdr;
begin
  for Item in FProgramHeaders do
  begin
    case Item.p_type of
      PT_DYNAMIC:
        begin
          FDynamics := ReadDynamicSection(Stream, Self, Item.p_offset, Item.p_filesz);
        end;
    end;
  end;
end;

function TELFImage.ReadFileHeader(const Stream: TStream): boolean;
begin
  // Read main header.
  if Stream.Read(FHeader, SizeOf(FHeader)) <> SizeOf(FHeader) then
  begin
{$IFDEF CONSOLE}
    writeln('Failed to read ELF header');
{$ENDIF}
    exit(False);
  end;

  FIs32 := FHeader.e_ident.&Class = ELFCLASS32;

{$IFDEF CONSOLE}
  writeln('ELF header');
  writeln(format('  Class:       %s', [ElfIdentClassToString(FHeader.e_ident.&Class)]));
  writeln(format('  Data:        %s', [ElfIdentDataToString(FHeader.e_ident.Data)]));
  writeln(format('  Version:     0x%x', [FHeader.e_ident.Version]));
  writeln(format('  OSABI:       0x%x', [FHeader.e_ident.OSABI]));
  writeln(format('  ABI Version: 0x%x', [FHeader.e_ident.ABIVersion]));
  writeln;
  writeln(format('  e_type:      0x%x - %s', [FHeader.e_type, ElfTypeToString(FHeader.e_type)]));
  writeln(format('  e_machine:   %s', [ElfMachineToString(FHeader.e_machine)]));
  writeln(format('  e_version:   0x%x', [FHeader.e_version]));
  writeln(format('  e_entry:     0x%x', [FHeader.e_entry]));
  writeln(format('  e_phoff:     0x%x', [FHeader.e_phoff]));
  writeln(format('  e_shoff:     0x%x', [FHeader.e_shoff]));
  writeln(format('  e_flags:     0x%x', [FHeader.e_flags]));
  writeln(format('  e_ehsize:    0x%x', [FHeader.e_ehsize]));
  writeln(format('  e_phentsize: 0x%x', [FHeader.e_phentsize]));
  writeln(format('  e_phnum:     0x%x', [FHeader.e_phnum]));
  writeln(format('  e_shentsize: 0x%x', [FHeader.e_shentsize]));
  writeln(format('  e_shnum:     0x%x', [FHeader.e_shnum]));
  writeln(format('  e_shstrndx:  0x%x', [FHeader.e_shstrndx]));
  writeln('-------------------------------------');
{$ENDIF}
  exit(True);
end;

function TELFImage.ReadProgramHeaders(const Stream: TStream): boolean;
var
  i: integer;
  Ofs: uint64;
  Item: Elf32_Phdr;
begin
{$IFDEF CONSOLE}
  writeln('Read program headers.');
  writeln(format('e_phoff = %x', [FHeader.e_phoff]));
  writeln(format('e_phentsize = %x', [FHeader.e_phentsize]));
  writeln(format('e_phnum = %x', [FHeader.e_phnum]));
{$ENDIF}
  FProgramHeaders.Clear;

  if FHeader.e_phnum = 0 then
    exit(True);

  if FHeader.e_phentsize < SizeOf(Elf32_Phdr) then
    raise Exception.Create('e_phentsize is less than program header size');

  for i := 0 to FHeader.e_phnum - 1 do
  begin
    Ofs := GetProgramHeaderOffset(i);

    if Stream.Seek(Ofs, soFromBeginning) <> Ofs then
      exit(False);

    if Stream.Read(Item, SizeOf(Elf32_Phdr)) <> SizeOf(Elf32_Phdr) then
      exit(False);

    FProgramHeaders.Add(Item);
  end;

{$IFDEF CONSOLE}
  for i := 0 to FProgramHeaders.Count - 1 do
  begin
    Item := FProgramHeaders[i];
    writeln(format('type:%-20s offset:%x vaddr:%x-%x paddr:%x filesz:%x memsz:%x flags:[%s] align:%x', [
      ElfPTypeToString(Item.p_type),
      Item.p_offset,
      Item.p_vaddr,
      Item.p_vaddr + Item.p_memsz,
      Item.p_paddr,
      Item.p_filesz,
      Item.p_memsz,
      ElfPFlagsToString(Item.p_flags),
      Item.p_align
      ]));
  end;
  writeln('-------------------------------------');
{$ENDIF}
  exit(True);
end;

function TELFImage.ReadSectionHeaders(const Stream: TStream): boolean;
var
  i: integer;
  Ofs: uint64;
  Item: TELFSectionRec;
  StrOfs: uint64;
begin
{$IFDEF CONSOLE}
  writeln('Read section headers.');
  writeln(format('e_shstrndx = %x', [FHeader.e_shstrndx]));
{$ENDIF}
  FSectionHeaders.Clear;

  // If no sections, it's ok.
  if FHeader.e_shnum = 0 then
  begin
{$IFDEF CONSOLE}
    writeln('No section headers.');
    writeln('-------------------------------------');
{$ENDIF}
    exit(True);
  end;

  if FHeader.e_shentsize < SizeOf(Elf32_Shdr) then
    raise Exception.Create('e_shentsize is less than section header size');

  StrOfs := 0;

  // Read raw headers.
  for i := 0 to FHeader.e_shnum - 1 do
  begin
    Ofs := GetSectionHeaderOffset(i);

    if Stream.Seek(Ofs, soFromBeginning) <> Ofs then
      exit(False);

    Item.Clear;

    // Read raw header.
    if Stream.Read(Item.Hdr, SizeOf(Item.Hdr)) <> SizeOf(Item.Hdr) then
      exit(False);

    // Try to get strings offset (if present).
    if (FHeader.e_shstrndx <> SHN_UNDEF) and (i = FHeader.e_shstrndx) then
      StrOfs := Item.Hdr.sh_offset;

    // Add.
    if Item.Hdr.sh_offset <> 0 then
      FSectionHeaders.Add(Item);
  end;

  // Read section names
  if StrOfs <> 0 then
    for i := 0 to FSectionHeaders.Count - 1 do
      if (FSectionHeaders[i].Hdr.sh_offset <> 0) then
      begin
        Item := FSectionHeaders[i];
        if not ReadStringA(Stream, StrOfs + Item.Hdr.sh_name, Item.Name) then
          exit(False);
        FSectionHeaders[i] := Item;
      end;

{$IFDEF CONSOLE}
  for i := 0 to FSectionHeaders.Count - 1 do
  begin
    Item := FSectionHeaders[i];
    writeln(format('%4.4x ofs:%8.8x size:%8.8x name:%d "%s"', [
      i,
      Item.Hdr.sh_offset,
      Item.Hdr.sh_size,
      Item.Hdr.sh_name,
      Item.Name
      ]));
  end;
  writeln('-------------------------------------');
{$ENDIF}
  exit(True);
end;

function TELFImage.ReadStringA(const Stream: TStream; Ofs: uint64;
  var Str: string): boolean;
var
  a: AnsiChar;
begin
  Str := '';
  if not Stream.Seek(Ofs, soFromBeginning) = Ofs then
    exit(False);
  Result := True;
  while True do
  begin
    if Stream.Read(a, 1) <> 1 then
      exit;
    if a = #0 then
      exit;
    Str := Str + Char(a);
  end;
end;

function TELFImage.VAToOffset(VA: uint64; out Offset: uint64): boolean;
var
  StartVA, StartOffset, Size: uint64;
begin
  if VAToVRange(VA, StartVA, StartOffset, Size) then
  begin
    Offset := StartOffset + (VA - StartVA);
    exit(True);
  end;
  exit(False);
end;

function TELFImage.VAToVRange(VA: uint64; out StartVA, StartOffset, Size: uint64): boolean;
var
  phdr: Elf32_Phdr;
begin
  for phdr in FProgramHeaders do
    if (phdr.p_filesz <> 0) and (phdr.p_memsz <> 0) and
      (VA >= phdr.p_vaddr) and (VA < phdr.p_vaddr + phdr.p_memsz) then
    begin
      StartVA := phdr.p_vaddr;
      StartOffset := phdr.p_offset;
      Size := phdr.p_memsz;
      exit(True);
    end;
  exit(False);
end;

{ TELFSection }

procedure TELFSectionRec.Clear;
begin
  FillChar(Hdr, SizeOf(Hdr), 0);
  Name := '';
end;

end.
