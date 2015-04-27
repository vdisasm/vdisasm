{
  http://docs.oracle.com/cd/E19253-01/817-1984/chapter6-79797/index.html
}
unit ELF.Symbols;

interface

uses
  System.Classes,
  System.SysUtils,
  ELF.Helpers,
  ELF.StringTable,
  ELF.Types;

type
  TELFSymbolTable = class

  end;

function ReadElfSymbolTable(
  Stream: TStream;
  ElfImage: TObject; // TElfImage
  StringTable: TELFStringTable;
  VA: uint64;
  EntSize: integer // size of 1 item
  ): TELFSymbolTable;

implementation

uses
  ELF.Image;

function ReadElfSymbolTable;
var
  ent: Elf32_Sym;
  ELF: TELFImage;
  name: string;
var
  StartVA, StartOffset, SectionSize: uint64;
  MaxEnt, iEnt: integer;
begin
  ELF := TELFImage(ElfImage);
{$IFDEF CONSOLE}
  writeln(Format('Read ELF symbol table at va = 0x%x, size of entry = 0x%x', [VA, EntSize]));
{$ENDIF}
  if EntSize <> SizeOf(ent) then
  begin
{$IFDEF CONSOLE}
    writeln(Format('EntSize <> SizeOf(Ent) i.e. %d <> %d', [EntSize, SizeOf(ent)]));
    writeln('-------------------------------------');
{$ENDIF}
    exit(nil);
  end;

  if not ELF.VAToVRange(VA, StartVA, StartOffset, SectionSize) then
    exit(nil);

  MaxEnt := SectionSize div EntSize;
{$IFDEF CONSOLE}
  writeln(Format('File offset = 0x%x; Max ent possible = 0x%x', [StartOffset, MaxEnt]));
{$ENDIF}
  Stream.Position := StartOffset;

  for iEnt := 0 to MaxEnt do
  begin
    if Stream.Read(ent, SizeOf(ent)) <> SizeOf(ent) then
      break;
    if not StringTable.GetString(ent.st_name, name) then
      break;
    if not ent.IsNull then
    begin
{$IFDEF CONSOLE}
      writeln(Format('  #%-8d "%s" value:%x size:%x info:%s,%s other:%x shndx:%x', [
        iEnt,
        name,
        ent.st_value,
        ent.st_size,
        ElfStInfoBindToString(ent.st_info),
        ElfStInfoTypeToString(ent.st_info),
        ent.st_other,
        ent.st_shndx
        ]));
{$ENDIF}
    end;
  end;
{$IFDEF CONSOLE}
  writeln('-------------------------------------');
{$ENDIF}
  exit(nil);
end;

end.
