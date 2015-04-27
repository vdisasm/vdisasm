unit ELF.Dynamics;

interface

uses
  System.Classes,
  System.Generics.Collections,
{$IFDEF CONSOLE}
  System.SysUtils,
{$ENDIF}
  ELF.Helpers,
  ELF.StringTable,
  ELF.Symbols,
  ELF.Types;

type
  TELFDynamics = class
  private
    type
    TDynValueDesc = record
      TagDataOffset: uint64;
      Value: uint64;
    end;

    TDynValueList = TList<TDynValueDesc>;
    TDynTagDic = TObjectDictionary< { tag } Elf64_Sxword, TDynValueList>;
    TDynPair = TPair<Elf64_Sxword, TDynValueList>;
  private
    function ReadDynamicTags(Stream: TStream; Size: integer): TDynTagDic;
  private
    FStringTable: TELFStringTable;
    FSymbolTable: TELFSymbolTable;
  public
    destructor Destroy; override;

    procedure DumpTags(const dic: TDynTagDic);
    property StringTable: TELFStringTable read FStringTable;
    property SymbolTable: TELFSymbolTable read FSymbolTable;
  end;

function ReadDynamicSection(
  Stream: TStream;
  ElfImage: TObject; // TElfImage
  FileOfs, Size: integer
  ): TELFDynamics;

implementation

function ReadDynamicSection;
var
  dic: TELFDynamics.TDynTagDic;
  v_strtab, v_strsz: TELFDynamics.TDynValueList;
  v_symtab, v_syment: TELFDynamics.TDynValueList;
begin
  Stream.Position := FileOfs;

  Result := TELFDynamics.Create;

  dic := Result.ReadDynamicTags(Stream, Size);
  try
    // String table.
    if dic.TryGetValue(DT_STRTAB, v_strtab) and dic.TryGetValue(DT_STRSZ, v_strsz) then
    begin
      Result.FStringTable := ReadElfStringTable(
        Stream,
        ElfImage,
        v_strtab.First.Value,
        v_strsz.First.Value);
    end;

    // Symbol table.
    if dic.TryGetValue(DT_SYMTAB, v_symtab) and dic.TryGetValue(DT_SYMENT, v_syment) then
    begin
      Result.FSymbolTable := ReadElfSymbolTable(
        Stream,
        ElfImage,
        Result.FStringTable,
        v_symtab.First.Value,
        v_syment.First.Value);
    end;

{$IFDEF DEBUG}
    Result.DumpTags(dic);
    Result.StringTable.DumpStrings;
{$ENDIF}
  finally
    dic.Free;
  end;
end;

{ TELFDynamics }

destructor TELFDynamics.Destroy;
begin
  FStringTable.Free;
  FSymbolTable.Free;
  inherited;
end;

procedure TELFDynamics.DumpTags(const dic: TDynTagDic);
{$IFDEF CONSOLE}
var
  pair: TDynPair;
  valuedesc: TDynValueDesc;
  str: string;
{$ENDIF}
begin
{$IFDEF CONSOLE}
  writeln('Dynamic');
  for pair in dic do
  begin
    writeln('  ', ElfDTagToString(pair.Key));
    for valuedesc in pair.Value do
    begin
      case pair.Key of
        DT_NEEDED, DT_SONAME:
          begin
            if FStringTable.GetString(valuedesc.Value, str) then
              writeln(format('    ofs: 0x%x 0x%x "%s"', [valuedesc.TagDataOffset, valuedesc.Value, str]))
            else
              writeln(format('    ofs: 0x%x 0x%x <string error>', [valuedesc.TagDataOffset, valuedesc.Value, str]))
          end;
      else
        writeln(format('    ofs: 0x%x 0x%x', [valuedesc.TagDataOffset, valuedesc.Value]));
      end;
    end;
  end;
  writeln('-------------------------------------');
{$ENDIF}
end;

function TELFDynamics.ReadDynamicTags(Stream: TStream; Size: integer): TDynTagDic;
var
  dyn: Elf32_Dyn;
  valuelist: TDynValueList;
  readsize: integer;
  desc: TDynValueDesc;
begin
  Result := TDynTagDic.Create([doOwnsValues]);

  while Size >= SizeOf(dyn) do
  begin
    desc.TagDataOffset := Stream.Position;

    readsize := Stream.Read(dyn, SizeOf(dyn));
    if (readsize < SizeOf(dyn)) or (dyn.d_tag = DT_NULL) then
      break;
    dec(Size, readsize);

    if not Result.TryGetValue(dyn.d_tag, valuelist) then
    begin
      valuelist := TDynValueList.Create;
      Result.Add(dyn.d_tag, valuelist);
    end;

    desc.Value := dyn.d_un.d_val;

    valuelist.Add(desc);
  end;
end;

end.
