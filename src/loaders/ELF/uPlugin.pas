unit uPlugin;

interface

uses
  System.SysUtils,

  ELF.Helpers,
  ELF.Image,
  ELF.Types,

  VDAPI;

type
  TELFLoader = class(TInterfacedObject, IVDLoaderPlugin)
  private
    FImg: TELFImage;
    FImgBase: UInt64;
    procedure ApplySectionsFromProgramHeader(const c: IVDCore; const Task: IVDLoaderTask);
    procedure ApplySections(const c: IVDCore; const Task: IVDLoaderTask);
    procedure ApplyCPU(const Task: IVDLoaderTask);
  public
    function GetPossibleFormatCount: int; stdcall;
    procedure GetFormatDesc(Formats: IVDLoaderFormats); stdcall;
    procedure FillTask(Task: IVDLoaderTask; Id: int); stdcall;
    procedure TaskApplied; stdcall;
  end;

implementation

{ TELFLoader }

procedure TELFLoader.ApplyCPU;
begin
  case FImg.Header.e_machine of
    EM_386:
      begin
        Task.SetCpuName(TCpuName.X32);
        Task.SetEndianness(TEndianness.Little);
        Task.SetAddressSize(4);
      end;
    EM_X86_64:
      begin
        Task.SetCpuName(TCpuName.X64);
        Task.SetEndianness(TEndianness.Little);
        Task.SetAddressSize(8);
      end;

    // ARM little endian
    EM_ARM:
      begin
        Task.SetCpuName(TCpuName.ARM);
        Task.SetEndianness(TEndianness.Little);
        Task.SetAddressSize(4);
      end;
  end;
end;

procedure TELFLoader.ApplySectionsFromProgramHeader(const c: IVDCore;
  const Task: IVDLoaderTask);
var
  i: integer;
  prg: Elf32_Phdr;
  rsize, vsize: TVDSectionSize;
  flags: TVDSectionFlags;
  va: TVA;
  strFileName: string;
  bTLS: Boolean;
begin
  if FImg.ProgramHeaders.Count = 0 then
  begin
    c.Log.WriteLn('elf: no program headers found');
    exit;
  end;

  strFileName := c.InputFile.FileName;

{$IFDEF DEBUG}
  c.Log.WriteLn('#,type,offset,vaddr,paddr,filesz,memsz,flags,align');
{$ENDIF}
  for i := 0 to FImg.ProgramHeaders.Count - 1 do
  begin
    prg := FImg.ProgramHeaders[i];
{$IFDEF DEBUG}
    c.Log.WriteLn(Format('%d %x %x %x %x %x %x %x %x',
      [
      i,
      prg.p_type,
      prg.p_offset,
      prg.p_vaddr, prg.p_paddr,
      prg.p_filesz, prg.p_memsz,
      prg.p_flags,
      prg.p_align
      ]));
{$ENDIF}
    if
      (prg.p_type = PT_LOAD) and
      (prg.p_memsz <> 0) then
    begin
      bTLS := (prg.p_flags and SHF_TLS) <> 0;
      if bTLS then
        continue;

      // if section will reside in memory
      // if ((prg.p_flags and SHF_ALLOC) <> 0) then
      begin
        va := FImgBase + prg.p_vaddr;

        rsize := prg.p_filesz;
        vsize := prg.p_memsz;

        // flags
        flags := TVDSectionFlag.Readable;
        if (prg.p_flags and SHF_WRITE) <> 0 then
          flags := flags or TVDSectionFlag.Writable;
        if (prg.p_flags and SHF_EXECINSTR) <> 0 then
          flags := flags or TVDSectionFlag.Execuatable;
        if bTLS then
          flags := flags or TVDSectionFlag.TLS;

        // add section
        Task.AddSectionFromFile(
          BSTR_IN(strFileName),
          '',
          prg.p_offset,
          rsize,
          vsize,
          va,
          flags
          );
      end;
    end;

  end;
end;

procedure TELFLoader.ApplySections(const c: IVDCore; const Task: IVDLoaderTask);
var
  sec: TELFSectionRec;
  hdr: ^Elf32_Shdr;
  name: string;
  rsize, vsize: TVDSectionSize;
  flags: TVDSectionFlags;
  va: TVA;
  i: integer;
  bTLS: Boolean;
  strFileName: string;
begin
  if FImg.SectionHeaders.Count = 0 then
  begin
    c.Log.WriteLn('elf: no section headers found');
    ApplySectionsFromProgramHeader(c, Task);
    exit;
  end;

{$IFDEF DEBUG}
  c.Log.WriteLn('#,name,sh_type,sh_addr,sh_offset,sh_size');
{$ENDIF}
  strFileName := c.InputFile.FileName;

  for i := 0 to FImg.SectionHeaders.Count - 1 do
  begin
    sec := FImg.SectionHeaders[i];
    hdr := @sec.hdr;

{$IFDEF DEBUG}
    c.Log.WriteLn(Format('%d "%s" %x %x %x %x link:%x info:%x algn:%x entsz:%x',
      [i, sec.name, hdr.sh_type, hdr.sh_addr, hdr.sh_offset, hdr.sh_size,
      hdr.sh_link, hdr.sh_info, hdr.sh_addralign, hdr.sh_entsize]));
{$ENDIF}
    if
      (hdr.sh_offset <> 0) and
      (hdr.sh_size <> 0) and
      (hdr.sh_addr <> 0) then
    begin
      bTLS := (hdr.sh_flags and SHF_TLS) <> 0;

      // skip TLS sections
      if bTLS then
        continue;

      // section name
      if sec.name <> '' then
        name := sec.name
      else
        name := '';

      // virtual size
      vsize := hdr.sh_size;

      // raw size
      if hdr.sh_type = SHT_NOBITS then
        rsize := 0
      else
        rsize := vsize;

      case hdr.sh_type of
        SHT_DYNAMIC, SHT_DYNSYM:
          begin
            // asm
            // int 3
            // end;
          end;
      end;

      // va
      va := FImgBase + hdr.sh_addr;

      // flags
      flags := TVDSectionFlag.Readable;
      if (hdr.sh_flags and SHF_WRITE) <> 0 then
        flags := flags or TVDSectionFlag.Writable;
      if (hdr.sh_flags and SHF_EXECINSTR) <> 0 then
        flags := flags or TVDSectionFlag.Execuatable;
      if bTLS then
        flags := flags or TVDSectionFlag.TLS;

      // if section will reside in memory
      if ((hdr.sh_flags and SHF_ALLOC) <> 0) then
      begin

        // add section
        Task.AddSectionFromFile(
          BSTR_IN(strFileName),
          BSTR_IN(name),
          sec.hdr.sh_offset,
          rsize,
          vsize,
          va,
          flags
          );

        // fixed size entries
        if hdr.sh_entsize <> 0 then
        begin
        end;

      end;

    end;
  end;
end;

function TELFLoader.GetPossibleFormatCount: int;
begin
  Result := 2; // elf, binary
end;

procedure TELFLoader.GetFormatDesc(Formats: IVDLoaderFormats);
var
  img: TELFImage;
  Desc: TVDLoaderFormatDesc;
begin
  img := TELFImage.Create;
  try
    if img.LoadFromFile(CoreGet.InputFile.FileName) then
    begin
      Desc.Text := Format('ELF %s (%s)', [
        ElfTypeToString(img.Header.e_type),
        ElfMachineToString(img.Header.e_machine)]);
      Desc.Id := 0;
      Formats.Add(Desc);
    end;
  finally
    img.Free;
  end;
end;

procedure TELFLoader.FillTask(Task: IVDLoaderTask; Id: int);
var
  c: IVDCore;
begin
  // Don't care about Id as there's only one format.
  FImg := TELFImage.Create;
  try
    c := CoreGet();
    if FImg.LoadFromFile(c.InputFile.FileName) then
    begin
      ApplySections(c, Task);
      ApplyCPU(Task);
      Task.SetEntry(FImg.EntryPoint);
    end;
  finally
    FImg.Free;
  end;
end;

procedure TELFLoader.TaskApplied;
begin
  // nop currently
end;

end.
