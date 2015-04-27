{
  Debugger sections is mapped to allocated memory (live during debug session).
}
unit uSection.Debugger;

interface

uses
  System.SysUtils,
  uSection,
  VDAPI;

type
  TVDDebuggerSection = class(TVDSection, IVDSection)
  private
    FMem: TBytes;
    function CalcAccess(va: TVA; Size: TVDSectionSize; out ofs: uint32): uint32;
    // {$IFNDEF DEBUG}inline; {$ENDIF}
  public
    constructor Create(Start: TVA; Size: TVDSectionSize; ID: TVDSectionId;
      Flags: TVDSectionFlags; const Name: string);
    destructor Destroy; override;

    function Read(va: TVA; Buffer: Pointer; Size: uint32): uint32; override;
    function Write(va: TVA; Buffer: Pointer; Size: uint32): uint32; override;
  end;

implementation

{ TVDDebuggerSection }

constructor TVDDebuggerSection.Create(Start: TVA; Size: TVDSectionSize;
  ID: TVDSectionId; Flags: TVDSectionFlags; const Name: string);
begin
  inherited Create(Start, Size, ID, Flags, Name);
  SetLength(FMem, Size);
end;

destructor TVDDebuggerSection.Destroy;
begin
  FMem := nil;
  inherited;
end;

function TVDDebuggerSection.CalcAccess(va: TVA; Size: TVDSectionSize; out ofs: uint32): uint32;
begin
  ofs := va - FStartVA;
  if ofs + Size > FSize then
    result := FSize - ofs
  else
    result := Size;
end;

function TVDDebuggerSection.Read(va: TVA; Buffer: Pointer;
  Size: uint32): uint32;
var
  ofs: uint32;
begin
  result := CalcAccess(va, Size, ofs);
  if result <> 0 then
    move(FMem[ofs], Buffer^, result);
end;

function TVDDebuggerSection.Write(va: TVA; Buffer: Pointer;
  Size: uint32): uint32;
var
  ofs: uint32;
begin
  result := CalcAccess(va, Size, ofs);
  if result <> 0 then
    move(Buffer^, FMem[ofs], result);
end;

end.
