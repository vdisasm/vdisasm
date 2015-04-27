unit uRegions.GlobalBasicBlocks;

interface

uses
  VDAPI,
  uDB,
  uRegions,
  uRegions.SubIds;

type
  TVDGlobalBasicBlocks = class(TRegions, IVDGlobalBasicBlocks)
  public
    constructor Create;
  public
    { VDAPI }
    function Get(VA: TVA): IVDVARegion; stdcall;
    function Put(VA: TVA; Size: UInt32; SubId: byte; const RegionData: IVDStreamIO): BOOL;
  end;

implementation

{ TVDRegionsTypeData }

constructor TVDGlobalBasicBlocks.Create;
begin
  inherited Create(DBTAG_GlobalBasicBlock);
end;

function TVDGlobalBasicBlocks.Get(VA: TVA): IVDVARegion;
begin
  Result := inherited GetRegionOnly(VA);
end;

function TVDGlobalBasicBlocks.Put(VA: TVA; Size: UInt32; SubId: byte;
  const RegionData: IVDStreamIO): BOOL;
begin
  result := inherited Put(VA, Size, SubID, RegionData);
end;

end.
