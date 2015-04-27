unit uLoaderDOS;

interface

uses
  System.Classes,
  System.SysUtils,
  VDAPI;

function IsDosFile: boolean;
procedure FillTask(const Task: IVDLoaderTask);

implementation

const
  PARA_SIZE = 16;  // size of paragraph
  BLCK_SIZE = 512; // size of block

type
  // http://www.delorie.com/djgpp/doc/exe/
  TImageDosHeader = packed record
    { 00-01 } e_magic: Word;

    // The number of bytes in the last block of the program that are actually used.
    // If this value is zero, that means the entire last block is used (i.e. the effective value is 512).
    { 02-03 } e_cblp: Word;

    // Number of blocks in the file that are part of the EXE file.
    // If [02-03] is non-zero, only that much of the last block is used.
    e_cp: Word;

    // Number of relocation entries stored after the header. May be zero.
    { 06-07 } e_crlc: Word;

    // Number of paragraphs in the header.
    // The program's data begins just after the header, and this field can be used
    // to calculate the appropriate file offset. The header includes the relocation
    // entries. Note that some OSs and/or programs may fail if the header is not a
    // multiple of 512 bytes.
    { 08-09 } e_cparhdr: Word;

    // Number of paragraphs of additional memory that the program will need.
    // This is the equivalent of the BSS size in a Unix program.
    // The program can't be loaded if there isn't at least this much memory available to it.
    { 0A-0B } e_minalloc: Word;

    // Maximum number of paragraphs of additional memory. Normally, the OS reserves
    // all the remaining conventional memory for your program, but you can limit it with this field.
    { 0C-0D } e_maxalloc: Word;

    // Relative value of the stack segment.
    // This value is added to the segment the program was loaded at, and the result
    // is used to initialize the SS register.
    { 0E-0F } e_ss: Word;

    // Initial value of the SP register.
    { 10-11 } e_sp: Word;

    // Word checksum.
    // If set properly, the 16-bit sum of all words in the file should be zero.
    // Usually, this isn't filled in.
    { 12-13 } e_csum: Word;

    // Initial value of the IP register.
    { 14-15 } e_ip: Word;

    // Initial value of the CS register, relative to the segment the program was loaded at.
    { 16-17 } e_cs: Word;

    // Offset of the first relocation item in the file.
    { 18-19 } e_lfarlc: Word;

    // Overlay number. Normally zero, meaning that it's the main program.
    { 1A-1B } e_ovno: Word;

    // e_res: array [0 .. 3] of Word;
    // e_oemid: Word;
    // e_oeminfo: Word;
    // e_res2: array [0 .. 9] of Word;
    // e_lfanew: LongInt;

    function GetExeDataStart: uint32; inline;
    function GetExtraDataStart: uint32; inline;
  end;

  { TImageDosHeader }

  TExeReloc = packed record
    offset: Word;
    segment: Word;
  end;

function TImageDosHeader.GetExeDataStart: uint32;
begin
  Result := e_cparhdr * PARA_SIZE;
end;

function TImageDosHeader.GetExtraDataStart: uint32;
begin
  Result := e_cp * BLCK_SIZE;
  if e_cblp <> 0 then
    dec(Result, BLCK_SIZE - e_cblp);
end;

function ReadDosHeader(out Hdr: TImageDosHeader): boolean;
const
  MAGIC_1 = $4D5A;
  MAGIC_2 = $5A4D;
var
  c: IVDCore;
  fn: string;
  fs: TFileStream;
begin
  c := CoreGet();

  fn := c.InputFile.FileName;

  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
    Result :=
    // Read header
      (fs.Read(Hdr, SizeOf(TImageDosHeader)) = SizeOf(TImageDosHeader)) and
    // Match signature
      ((Hdr.e_magic = MAGIC_1) or (Hdr.e_magic = MAGIC_2));
  finally
    fs.Free;
  end;
end;

function IsDosFile: boolean;
var
  hdr: TImageDosHeader;
begin
  result := ReadDosHeader(hdr);
end;

procedure DumpDosHdr(const c: IVDCore; const DosHdr: TImageDosHeader);
begin
{$IFDEF DEBUG}
  c.Log.WriteLn(Format('e_magic: %x', [DosHdr.e_magic]));
  c.Log.WriteLn(Format('e_cblp: %x', [DosHdr.e_cblp]));
  c.Log.WriteLn(Format('e_cp: %x', [DosHdr.e_cp]));
  c.Log.WriteLn(Format('e_crlc: %x', [DosHdr.e_crlc]));
  c.Log.WriteLn(Format('e_cparhdr: %x', [DosHdr.e_cparhdr]));
  c.Log.WriteLn(Format('e_minalloc: %x', [DosHdr.e_minalloc]));
  c.Log.WriteLn(Format('e_maxalloc: %x', [DosHdr.e_maxalloc]));
  c.Log.WriteLn(Format('e_ss: %x', [DosHdr.e_ss]));
  c.Log.WriteLn(Format('e_sp: %x', [DosHdr.e_sp]));
  c.Log.WriteLn(Format('e_csum: %x', [DosHdr.e_csum]));
  c.Log.WriteLn(Format('e_ip: %x', [DosHdr.e_ip]));
  c.Log.WriteLn(Format('e_cs: %x', [DosHdr.e_cs]));
  c.Log.WriteLn(Format('e_lfarlc: %x', [DosHdr.e_lfarlc]));
  c.Log.WriteLn(Format('e_ovno: %x', [DosHdr.e_ovno]));
{$ENDIF}
end;

procedure ApplyCpu(const Task: IVDLoaderTask);
begin
  Task.SetCpuName(TCpuName.X16);
  Task.SetEndianness(TEndianness.Little);
  Task.SetAddressSize(2);
end;

procedure FillTask(const Task: IVDLoaderTask);
const
  IMG_BASE = 0;
var
  c: IVDCore;
  io: IVDIO;
  DosHdr: TImageDosHeader;
  FileName: BSTR;
var
  DataOfs, DataSize: uint;
  ExtraOfs, CodeOfs, CodeSize: uint;
  DataVA, CodeVA, EntryVA: TVA;
  DataVSize, CodeVSize: uint;
begin
  c := CoreGet();
  io := IOGet;

  if not ReadDosHeader(DosHdr) then
  begin
    c.Log.WriteLn('Failed to read DOS header.');
    exit;
  end;

  FileName := c.InputFile.FileName;

  ExtraOfs := DosHdr.GetExtraDataStart;
  DataOfs := DosHdr.GetExeDataStart;

  if DosHdr.e_cs = 0 then
  begin
    // If code section is at start of data, there will be no 'data' section.
    DataSize := 0;
    CodeOfs := DataOfs;
    CodeSize := ExtraOfs - DataOfs;
  end
  else
  begin
    DataSize := DosHdr.e_cs * PARA_SIZE;
    CodeOfs := DataOfs + DataSize;
    CodeSize := ExtraOfs - CodeOfs;
  end;

  DataVA := IMG_BASE;
  CodeVA := IMG_BASE + DosHdr.e_cs * PARA_SIZE;

  CodeVSize := CodeSize;
  DataVSize := DataSize;

  DumpDosHdr(c, DosHdr);

  ApplyCpu(Task);

  // Apply sections.

  if DataSize <> 0 then
  begin
    // If cs = 0, there is no 'data' section, because 'code' occupies its space.
    Task.AddSectionFromFile(
      BSTR_IN(FileName),
      'data',
      DataOfs, DataSize, DataVSize, DataVA,
      TVDSectionFlag.Readable or TVDSectionFlag.Writable or TVDSectionFlag.Execuatable);
  end;

  if CodeSize <> 0 then
  begin
    Task.AddSectionFromFile(
      BSTR_IN(FileName),
      'code',
      CodeOfs, CodeSize, CodeVSize, CodeVA,
      TVDSectionFlag.Readable or TVDSectionFlag.Writable or TVDSectionFlag.Execuatable);

    // Entry address.
    EntryVA := CodeVA + DosHdr.e_ip;
    Task.SetEntry(EntryVA);
  end;
end;

end.
