{
  Section of Virtual Memory.
  Id is unique identifier of section.
}
unit uSection;

interface

uses
  VDAPI,
  uVARegion,
  uStream;

type

  { TVDSecton }

  TVDSection = class(TVDVARegion, IVDSection)
  protected
    FID: TVDSectionId;
    FName: string;
    FFlags: TVDSectionFlags;
  public
    ContigiousPosition: TVA;
    constructor Create(Start: TVA; Size: TVDSectionSize; ID: TVDSectionId;
      Flags: TVDSectionFlags; const Name: string);
    constructor CreateDummy(Start: TVA; ContigiousPosition: TVA = 0);
  public
    { VDAPI }
    function GetFlags: TVDSectionFlags; stdcall;
    function GetFlagsAsString: BSTR; stdcall;
    function GetID: TVDSectionId; stdcall;
    function GetName: BSTR; stdcall;
    // Read data return count read.
    function Read(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32; virtual; stdcall;
    // Write data return number written.
    function Write(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32; virtual; stdcall;
    // Flush section data changes.
    function Flush: BOOL; virtual; stdcall;
  end;

function SectionFlagsToString(Flags: TVDSectionFlags): string;

implementation

constructor TVDSection.Create(Start: TVA; Size: TVDSectionSize; ID: TVDSectionId;
  Flags: TVDSectionFlags; const Name: string);
begin
  inherited Create(Start, Size);
  FID := ID;
  FFlags := Flags;
  FName := Name;
end;

constructor TVDSection.CreateDummy(Start: TVA; ContigiousPosition: TVA);
begin
  FStartVA := Start;
  FSize := 1;
  self.ContigiousPosition := ContigiousPosition;
end;

function TVDSection.Flush: BOOL;
begin
  Result := True;
end;

function TVDSection.GetFlags: TVDSectionFlags;
begin
  Result := FFlags;
end;

function SectionFlagsToString(Flags: TVDSectionFlags): string;
begin
  Result := '......';
  if (Flags and TVDSectionFlag.Readable) <> 0 then
    Result[1] := 'r';
  if (Flags and TVDSectionFlag.Writable) <> 0 then
    Result[2] := 'w';
  if (Flags and TVDSectionFlag.Execuatable) <> 0 then
    Result[3] := 'x';
  if (Flags and TVDSectionFlag.Guard) <> 0 then
    Result[4] := 'g';
  if (Flags and TVDSectionFlag.FileMapped) <> 0 then
    Result[5] := 'f';
  if (Flags and TVDSectionFlag.Debugger) <> 0 then
    Result[6] := 'd';
  if (Flags and TVDSectionFlag.TLS) <> 0 then
    Result[6] := 't';
end;

function TVDSection.GetFlagsAsString: BSTR;
begin
  Result := SectionFlagsToString(FFlags);
end;

function TVDSection.GetID: TVDSectionId;
begin
  Result := FID;
end;

function TVDSection.GetName: BSTR;
begin
  Result := FName;
end;

function TVDSection.Read(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32;
begin
  Result := 0;
end;

function TVDSection.Write(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32;
begin
  Result := 0;
end;

end.
