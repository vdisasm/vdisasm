unit uBuider;

interface

uses
  VDAPI;

type
  TStdBuilder = class
  public const
    FILE_NAME = 'std.typelib';
  protected
    class constructor Create;
    class procedure FunWithTypes;
    class function EnumLoadedNames(Name: BSTR; ud: pointer): BOOL; static; stdcall;
  public
  class var
    lib: IVDTypeLibrary;
    fac: IVDTypeFactory;
    dstfilename: string;

    s8, s16, s32, s64: IVDType;
    u8, u16, u32, u64: IVDType;
    f32, f64: IVDType;
    GUID, RECT, RECTEX: IVDRecordType;
  protected
    class procedure AddTypes;
  public
    class procedure CreateLib;
    class procedure LoadLib;
  end;

implementation

uses
  System.SysUtils;

{ TStdBuilder }

class procedure TStdBuilder.AddTypes;
begin
  // signed
  s8 := fac.CreateIntegerType('s8', true, 8);
  s16 := fac.CreateIntegerType('s16', true, 16);
  s32 := fac.CreateIntegerType('s32', true, 32);
  s64 := fac.CreateIntegerType('s64', true, 64);

  // unsigned
  u8 := fac.CreateIntegerType('u8', false, 8);
  u16 := fac.CreateIntegerType('u16', false, 16);
  u32 := fac.CreateIntegerType('u32', false, 32);
  u64 := fac.CreateIntegerType('u64', false, 64);

  f32 := fac.CreateFloatType('f32', 32);
  f64 := fac.CreateFloatType('f64', 64);

  // Some fun.
  FunWithTypes;
end;

class constructor TStdBuilder.Create;
var
  dir: string;
begin
  dir := ExtractFilePath(ParamStr(1)) + '..\typelibs\';
  dstfilename := dir + FILE_NAME;
end;

procedure ListenerProc(Msg: TVDMessage; Param: pointer); stdcall;
begin

end;

class procedure TStdBuilder.CreateLib;
begin
  lib := TypeLibraryCreate('std');
  fac := TypeFactoryCreate(lib);
  AddTypes;
  lib.SaveToFile(BSTR_IN(dstfilename));
end;

class function TStdBuilder.EnumLoadedNames(Name: BSTR; ud: pointer): BOOL;
begin
  writeln(Name);
  exit(true);
end;

class procedure TStdBuilder.FunWithTypes;
var
  MZ: IVDRecordType;
  bytes8: IVDArrayType;
  words4, words10: IVDArrayType;
  IMAGE_SECTION_HEADER: IVDRecordType;
begin
  RECT := fac.CreateRecord('RECT', nil);
  RECT.Comment := 'Simple Rectangle';
  RECT.AddField('Left', s32, 'Left coordinate');
  RECT.AddField('Top', s32, 'Top coordinate');
  RECT.AddField('Width', s32, 'Rectangle width');
  RECT.AddField('Height', s32, 'Rectangle height');

  RECTEX := fac.CreateRecord('RECTEX', RECT);
  RECTEX.AddField('Area', u32, 'Rectangle area');

  GUID := fac.CreateRecord('GUID');
  GUID.AddField('D0', s32);
  GUID.AddField('D1', s32);
  GUID.AddField('D2', s32);
  GUID.AddField('D3', s32);

  // mz
  words4 := fac.CreateArray2('words4', u16, 4);
  words10 := fac.CreateArray2('words10', u16, 10);
  bytes8 := fac.CreateArray2('bytes8', u8, 8);

  MZ := fac.CreateRecord('IMAGE_DOS_HEADER');
  MZ.AddField('e_magic', u16);
  MZ.AddField('e_cblp', u16);
  MZ.AddField('e_cp', u16);
  MZ.AddField('e_crlc', u16);
  MZ.AddField('e_cparhdr', u16);
  MZ.AddField('e_minalloc', u16);
  MZ.AddField('e_maxalloc', u16);
  MZ.AddField('e_ss', u16);
  MZ.AddField('e_sp', u16);
  MZ.AddField('e_csum', u16);
  MZ.AddField('e_ip', u16);
  MZ.AddField('e_cs', u16);
  MZ.AddField('e_lfarlc', u16);
  MZ.AddField('e_ovno', u16);
  MZ.AddField('e_res', words4);
  MZ.AddField('e_oemid', u16);
  MZ.AddField('e_oeminfo', u16);
  MZ.AddField('e_res2', words10);
  MZ.AddField('e_lfanew', s32);

  // IMAGE_SECTION_HEADER
  IMAGE_SECTION_HEADER := fac.CreateRecord('IMAGE_SECTION_HEADER');
  IMAGE_SECTION_HEADER.AddField('Name', bytes8);
  IMAGE_SECTION_HEADER.AddField('VirtualSize', u32);
  IMAGE_SECTION_HEADER.AddField('VirtualAddress', u32);
  IMAGE_SECTION_HEADER.AddField('SizeOfRawData', u32);
  IMAGE_SECTION_HEADER.AddField('PointerToRawData', u32);
  IMAGE_SECTION_HEADER.AddField('PointerToRelocations', u32);
  IMAGE_SECTION_HEADER.AddField('PointerToLinenumbers', u32);
  IMAGE_SECTION_HEADER.AddField('NumberOfRelocations', u16);
  IMAGE_SECTION_HEADER.AddField('NumberOfLinenumbers', u16);
  IMAGE_SECTION_HEADER.AddField('Characteristics', u32);
end;

class procedure TStdBuilder.LoadLib;
var
  lib: IVDTypeLibrary;
begin
  lib := TypeLibraryLoad(BSTR_IN(dstfilename));

  lib.EnumTypeNames(EnumLoadedNames, nil);
end;

end.
