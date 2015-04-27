{
  Region of Virtual Addresses.
}
unit uVARegion;

interface

uses
  VDAPI;

type
  TVDVARegion = class(TInterfacedObject, IVDVARegion)
  protected
    FStartVA: TVA;
    FSize: TVDSectionSize;
  public
    constructor Create(Start: TVA; Size: TVDSectionSize);
  public
    { VDAPI }
    function GetStartVA: TVA; stdcall;
    function GetSize: TVDSectionSize; stdcall;
    function GetEndVA: TVA; stdcall;
    function GetLastVA: TVA; stdcall;
    function Contains(VA: TVA): BOOL; stdcall;
  public
    procedure SetStartVA(VA: TVA);
  end;

implementation

{ TVDVARegion }

function TVDVARegion.Contains(VA: TVA): BOOL;
begin
  Result := (VA >= FStartVA) and (VA < FStartVA + FSize);
end;

constructor TVDVARegion.Create(Start: TVA; Size: TVDSectionSize);
begin
  FStartVA := Start;
  FSize := Size;
end;

function TVDVARegion.GetEndVA: TVA;
begin
  Result := FStartVA + FSize;
end;

function TVDVARegion.GetLastVA: TVA;
begin
  Result := FStartVA + FSize - 1;
end;

function TVDVARegion.GetSize: TVDSectionSize;
begin
  Result := FSize;
end;

function TVDVARegion.GetStartVA: TVA;
begin
  Result := FStartVA;
end;

procedure TVDVARegion.SetStartVA(VA: TVA);
begin
  FStartVA := VA;
end;

end.
