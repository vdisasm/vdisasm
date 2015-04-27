unit uModule;

interface

uses
  VDAPI;

type
  TVDModule = class(TInterfacedObject, IVDModule)
  public
    Path: string;
    ImageBase: TVA;
    ImageSize: uint64;
    constructor Create(Path: BSTR_IN; ImageBase: TVA; ImageSize: uint64);
  public
    function GetImageBase: TVA; stdcall;
    procedure SetImageBase(VA: TVA); stdcall;

    function AddSectionEmpty(Name: BSTR; Size: uint64; VA: TVA; Flags: TVDSectionFlags): IVDSection; stdcall;

    function AddSectionFromFile(FileName, Name: BSTR; Offset: uint64;
      RawSize, VirtSize: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags)
      : IVDSection; stdcall;

    function AddSectionFromMappedFile(FileName, Name: BSTR; Offset: uint64;
      Size: TVDSectionSize; RVA: TRVA; Flags: TVDSectionFlags): IVDSection; stdcall;
  end;

implementation

{ TVDModule }

constructor TVDModule.Create(Path: BSTR_IN; ImageBase: TVA; ImageSize: uint64);
begin
  self.Path := Path;
  self.ImageBase := ImageBase;
  self.ImageSize := ImageSize;
end;

function TVDModule.GetImageBase: TVA;
begin
  Result := ImageBase;
end;

procedure TVDModule.SetImageBase(VA: TVA);
begin
  ImageBase := VA;
end;

function TVDModule.AddSectionEmpty(Name: BSTR; Size: uint64; VA: TVA;
  Flags: TVDSectionFlags): IVDSection;
begin
  Result := nil;
end;

function TVDModule.AddSectionFromFile(FileName, Name: BSTR; Offset: uint64;
  RawSize, VirtSize: TVDSectionSize; VA: TVA;
  Flags: TVDSectionFlags): IVDSection;
begin
  Result := nil;
end;

function TVDModule.AddSectionFromMappedFile(FileName, Name: BSTR;
  Offset: uint64; Size: TVDSectionSize; RVA: TRVA;
  Flags: TVDSectionFlags): IVDSection;
begin
  Result := nil;
end;

end.
