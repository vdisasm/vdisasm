{
  Key:   Tag, RelVA (inherited)
  Value: Size, SubId
}
unit uRegions.TypeData;

interface

uses
  System.SysUtils,
  VDAPI,
  uRegions,
  uRegions.SubIds,
  uStream,
  uStream.MemoryStream;

type
  {
    TVDRegionsTypeData class is not so important now. It actually do reading
    and writing implemented in uRegions.SubIds.

    So probably it can be removed some later.
  }

  TVDRegionsTypeData = class(TRegions)
  public
    constructor Create;

    function PutUidAndData(VA: TVA; UID: TTypeUID; Size: uint32): boolean;

    // VA can point in the middle.
    function GetUidAndData(var VA: TVA; out UID: TTypeUID; out Size: uint32): boolean;
  end;

implementation

uses
  uDB,
  uCore,
  BPlusTree.Intf;

{ TVDRegionsTypeData }

constructor TVDRegionsTypeData.Create;
begin
  inherited Create(DBTAG_TypeDataId);
end;

function TVDRegionsTypeData.GetUidAndData(var VA: TVA; out UID: TTypeUID;
  out Size: uint32): boolean;
var
  Rgn: IVDVARegion;
  io: IVDStreamIO;
  subid: byte;
begin
  Rgn := inherited Get(VA, subid, io);
  if Rgn = nil then
    exit(false);

  // expect UID type of region
  if subid <> REGION_SUBID_TYPE_UID then
    exit(false);

  if not Region_Parse_UID(io, UID) then
    exit(false);

  VA := Rgn.GetStartVA;
  Size := Rgn.GetSize;
  Result := True;
end;

function TVDRegionsTypeData.PutUidAndData(VA: TVA; UID: TTypeUID;
  Size: uint32): boolean;
var
  io: IVDStreamIO;
begin
  io := TVDStreamIO.Create(TVDMemoryStream.Create);
  Region_Write_UID(io, UID);
  Result := inherited Put(VA, Size, REGION_SUBID_TYPE_UID, io);
end;

end.
