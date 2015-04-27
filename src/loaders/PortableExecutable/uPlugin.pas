unit uPlugin;

interface

uses
  VDAPI,

  uLoaderDos,

  PE.Common,
  PE.Image,
  PE.ParserCallbacks,

  PE.Types.Relocations;

const
  FMT_TYPE_DOS = $100;
  FMT_TYPE_PE  = $200;

type
  TLoaderPE = class;

  TMyCallbacks = class(TInterfacedObject, IPEParserCallbacks)
  protected
    FPE: TPEImage;
    FLoader: TLoaderPE;
  public
    constructor Create(const PE: TPEImage; const Loader: TLoaderPE);
  public
    procedure ParsedRelocationBlockHeader(RVA: UInt64; const Block: TBaseRelocationBlock);
  end;

  TLoaderPE = class(TInterfacedObject, IVDLoaderPlugin)
  private
    FCore: IVDCore;
    FPE: TPEImage;
    FImgBase: UInt64;
    FId: int; // set of FillTask
  public
    destructor Destroy; override;

    procedure ApplyCpu(const Task: IVDLoaderTask);
    procedure ApplySections(const Task: IVDLoaderTask);
    procedure ApplyImports;
    procedure ApplyExports;
    procedure ApplyPData;
    procedure ApplyTlsCallbacks;
  public
    function GetPossibleFormatCount: int; stdcall;
    procedure GetFormatDesc(Formats: IVDLoaderFormats); stdcall;
    procedure FillTask(Task: IVDLoaderTask; Id: int); stdcall;
    procedure TaskApplied; stdcall;
  end;

implementation

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}


uses
  System.SysUtils,

  PE.FileHeaderToStr,
  PE.Section,
  PE.Types.Sections,
  PE.Imports,
  PE.Imports.Func,
  PE.Imports.Lib,
  PE.ExportSym,
  PE.Parser.PData,

  PE.Types.FileHeader; // expand

const
  SSecNamePData = '.pdata';

procedure MakeWord(const c: IVDCore; VA: TVA; cbSize: byte);
begin
  case cbSize of
    1:
      c.MakeType(VA, TVDStdTypeName.u8);
    2:
      c.MakeType(VA, TVDStdTypeName.u16);
    4:
      c.MakeType(VA, TVDStdTypeName.u32);
    8:
      c.MakeType(VA, TVDStdTypeName.u64);
  end;
end;

{ TBinaryLoader }

procedure TLoaderPE.ApplyTlsCallbacks;
var
  RVA: TRVA;
  VA: TVA;
begin
  for RVA in FPE.TLS.CallbackRVAs do
  begin
    VA := FPE.RVAToVA(RVA);
    FCore.ExportSymbols.Put(VA, BSTR_IN(format('tls_%x', [VA])), 0);
  end;
end;

destructor TLoaderPE.Destroy;
begin
  FreeAndNil(FPE);
  inherited;
end;

procedure TLoaderPE.ApplyExports;
var
  sym: TPEExportSym;
  VA: TVA;
  name: string;
begin
  for sym in FPE.ExportSyms.Items do
  begin
    VA := sym.RVA + FImgBase;
    name := sym.name;
    FCore.ExportSymbols.Put(VA, BSTR_IN(name), sym.Ordinal);
  end;
end;

procedure TLoaderPE.ApplyImports;
var
  Lib: TPEImportLibrary;
  fn: TPEImportFunction;
  lib_name, name: string;
  iat_typename: string;
  VA: TVA;
begin
  if FPE.Imports.Count = 0 then
    exit;

  if FPE.Is32bit then
    iat_typename := 'u32'
  else if FPE.Is64bit then
    iat_typename := 'u64'
  else
    iat_typename := '';

  for Lib in FPE.Imports.LibsByName do
  begin
    lib_name := Lib.name;
    for fn in Lib.Functions.FunctionsByRVA.Values do
    begin
      VA := fn.RVA + FImgBase;
      name := fn.name;
      FCore.ImportSymbols.Put(VA, BSTR_IN(lib_name), BSTR_IN(name), fn.Ordinal);

      // Make iat-word.
      if iat_typename <> '' then
        FCore.MakeType(VA, BSTR_IN(iat_typename));
    end;
  end;

end;

procedure TLoaderPE.ApplyPData;
var
  sec: TPESection;
  Items: TPDATAItems;
  Item: TPDATAItem;
  VA: TVA;
  DataType: TPDATAType;
  Done: Integer;
begin
  sec := FPE.Sections.FindByName(SSecNamePData);
  if sec = nil then
    exit;
  ParsePData(FPE, DataType, Items);
  if Length(Items) <> 0 then
  begin
    FCore.Log.Write('Applying .pdata info ... ');
    Done := 0;

    FCore.Jobs.BeginUpdate;
    try
      for Item in Items do
      begin
        VA := Item.BeginAddress; // rva or va
        case DataType of         // correct
          pdata_x64, pdata_ARMv7:
            inc(VA, FImgBase);
        end;

        FCore.Jobs.AddMakeFunc(VA);

        inc(Done);
      end;
    finally
      FCore.Jobs.EndUpdate;
      FCore.Log.WriteLn(format('%d item(s) done.', [Done]));
    end;
  end;
end;

procedure TLoaderPE.ApplySections;
var
  Index: Integer;
  SecToLoad: Integer absolute Index;
  sec: TPESection;
  secName: string;
  SecFlags: TVDSectionFlags;
{$IFDEF LOAD_HEADER_AS_SECTION}
var
  hdrSize: Integer;
{$ENDIF}
begin
  // Header as section
{$IFDEF LOAD_HEADER_AS_SECTION}
  hdrSize := FPE.CalcHeadersSizeNotAligned;
  Task.AddSectionFromFile(
    BSTR_IN(FCore.InputFile.FileName),
    'header',
    0,        // raw offset
    hdrSize,  // raw size
    hdrSize,  // virtual size
    FImgBase, // va
    TVDSectionFlag.Readable or TVDSectionFlag.Writable or TVDSectionFlag.Execuatable
    );
{$ENDIF}
  for sec in FPE.Sections do
  begin
    // get section name
    secName := sec.name;

    // make flags
    SecFlags := 0;
    if (sec.Flags and IMAGE_SCN_MEM_READ) <> 0 then
      SecFlags := SecFlags or TVDSectionFlag.Readable;
    if (sec.Flags and IMAGE_SCN_MEM_WRITE) <> 0 then
      SecFlags := SecFlags or TVDSectionFlag.Writable;
    if (sec.Flags and IMAGE_SCN_MEM_EXECUTE) <> 0 then
      SecFlags := SecFlags or TVDSectionFlag.Execuatable;

    // add
    Task.AddSectionFromFile(
      BSTR_IN(FCore.InputFile.FileName),
      BSTR_IN(secName),
      sec.RawOffset,
      sec.RawSize,
      sec.VirtualSize,
      sec.RVA + FImgBase,
      SecFlags
      );
  end;
end;

procedure TLoaderPE.ApplyCpu(const Task: IVDLoaderTask);
begin
  case FPE.FileHeader.Machine of
    // x86
    IMAGE_FILE_MACHINE_I386:
      begin
        Task.SetCpuName(TCpuName.X32);
        Task.SetEndianness(VDAPI.TEndianness.Little);
        Task.SetAddressSize(4);
      end;
    // x64
    IMAGE_FILE_MACHINE_AMD64:
      begin
        Task.SetCpuName(TCpuName.X64);
        Task.SetEndianness(VDAPI.TEndianness.Little);
        Task.SetAddressSize(8);
      end;
    // ARM little endian
    IMAGE_FILE_MACHINE_ARM:
      begin
        Task.SetCpuName(TCpuName.ARM);
        Task.SetEndianness(VDAPI.TEndianness.Little);
        Task.SetAddressSize(4);
      end;
    // ARM or Thumb (“interworking”)
    IMAGE_FILE_MACHINE_THUMB:
      begin
        Task.SetCpuName(TCpuName.ARM);
        Task.SetEndianness(VDAPI.TEndianness.Little);
        Task.SetAddressSize(4);
      end;
  end;
end;

// -----------------------------------------------------------------------------

{ TMyCallbacks }

constructor TMyCallbacks.Create(const PE: TPEImage; const Loader: TLoaderPE);
begin
  Self.FPE := PE;
  Self.FLoader := Loader;
end;

procedure TMyCallbacks.ParsedRelocationBlockHeader(RVA: UInt64;
  const Block: TBaseRelocationBlock);
// var
// VA: TVA;
// i: Integer;
begin
  (*
    VA := FLoader.FOptions.ImgBase + RVA;
    FLoader.FCore.Comments.Put(
    VA,
    format('Relocation block for page RVA:%x Entries:%d', [Block.PageRVA, Block.Count]),
    0);

    // Reloc. block hdr.
    MakeWord(FLoader.FCore, VA + 0, 4);
    MakeWord(FLoader.FCore, VA + 4, 4);
    // Reloc. block items.
    for i := 0 to Block.Count - 1 do
    MakeWord(FLoader.FCore, VA + 8 + i * 2, 2);
  *)
end;

function TLoaderPE.GetPossibleFormatCount: int;
begin
  Result := 3; // pe, dos, binary
end;

procedure TLoaderPE.GetFormatDesc(Formats: IVDLoaderFormats);
var
  c: IVDCore;
  PE: TPEImage;
  sMachine: string;
var
  Desc: TVDLoaderFormatDesc;
begin
  // Try PE format.
  c := CoreGet;
  PE := TPEImage.Create;
  try
    if PE.LoadFromFile(c.InputFile.FileName, []) and (PE.ImageBits in [32, 64]) then
    begin
      sMachine := MachineToStr(PE);
      Desc.Text := format('PE%d (%s)', [PE.ImageBits, sMachine]);
      Desc.Id := FMT_TYPE_PE;
      Formats.Add(Desc);
    end;
  finally
    PE.Free;
  end;

  // Try DOS format.
  if uLoaderDos.IsDosFile then
  begin
    Desc.Text := '16-bit DOS file';
    Desc.Id := FMT_TYPE_DOS;
    Formats.Add(Desc);
  end;
end;

procedure TLoaderPE.FillTask(Task: IVDLoaderTask; Id: int);
var
  Flags: TParserFlags;
  fn: string;
begin
  FId := Id;
  FCore := CoreGet;
  fn := FCore.InputFile.FileName;
  case Id of
    FMT_TYPE_DOS:
      begin
        uLoaderDos.FillTask(Task);
      end;
    FMT_TYPE_PE:
      begin
        // ---------------------------------------------------------------------------
        // Brief loading (Cpu + Sections)
        FPE := TPEImage.Create();
        try
          if not FPE.LoadFromFile(fn, []) then
            exit;

          FImgBase := FPE.ImageBase;

          ApplyCpu(Task);
          ApplySections(Task);
        finally
          FreeAndNil(FPE);
        end;
        // ---------------------------------------------------------------------------
        // Loading with regions.
        Flags := [PF_EXPORT, PF_IMPORT, PF_RELOCS, PF_TLS, PF_RESOURCES];
        FPE := TPEImage.Create();
        FPE.ParseCallbacks := TMyCallbacks.Create(FPE, Self);
        if not FPE.LoadFromFile(fn, Flags) then
          exit;

        // Apply entry
        Task.SetEntry(FPE.EntryPointRVA + FPE.ImageBase);
        // ---------------------------------------------------------------------------
      end;
  end;
end;

procedure TLoaderPE.TaskApplied;
begin
  case FId of
    FMT_TYPE_DOS:
      ;
    FMT_TYPE_PE:
      begin
        ApplyExports;
        ApplyImports;
        ApplyPData;
        ApplyTlsCallbacks;
      end;
  end;
end;

end.
