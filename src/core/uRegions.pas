{
  *  Regions are ranges of virtual address.
  *  Region can represent
  *  - collapsable block
  *  - data region (defined code / data)
  *  - relocation region
  *  - string

  Key:   Tag, RelVA
  Value: Size, Custom Data
}
unit uRegions;

interface

uses
  VDAPI,
  uDB,
  uStream,
  uStream.MemoryStream;

type
  TRegions = class(TInterfacedObject)
  private
    FTag: TDBTagCast;
  public
    constructor Create(Tag: TDBTagCast);

    // Put/Get region data.
    // RegionData don't include SubId byte.
    // It is one of REGION_SUBID_xxx constants.
    function Put(VA: TVA; Size: UInt32; SubId: byte; const RegionData: IVDStreamIO): BOOL;

    // Find current or next region starting from VA.
    // Result is True if found.
    function NextGreaterOrEqual(var VA: TVA): boolean;

    function GetEx(VA: TVA; out SubId: byte; out RegionData: IVDStreamIO; NeedRegionData: boolean = True): IVDVARegion;
    function Get(VA: TVA; out SubId: byte; out RegionData: IVDStreamIO): IVDVARegion; inline;
    function GetRegionAndSubId(VA: TVA; out SubId: byte): IVDVARegion; inline;
    function GetRegionOnly(VA: TVA): IVDVARegion; inline;

    // Delete region and return size of deleted region.
    function Del(VA: TVA): SIZE_T;
    function DelRange(VA: TVA; Size: SIZE_T): SIZE_T;
  end;

implementation

uses
  System.SysUtils,
  BPlusTree.Intf,
  uCore,
  uDB.VA,
  uVARegion;

{ TRegions }

constructor TRegions.Create(Tag: TDBTagCast);
begin
  inherited Create;
  FTag := Tag;
end;

function ParseRegionKey(var Key: TBytes; out RelVA: TRelVA): boolean;
var
  Data: IVDStreamIO;
begin
  Data := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Key));
  if Data.GetSize < (1 + SizeOf(TRelVA)) then
    Exit(False);
  Data.SetPosition(1);
  Data.ReadRelVA(RelVA);
  Exit(True);
end;

function TRegions.NextGreaterOrEqual(var VA: TVA): boolean;
var
  c: IVDCore;
  Key: IVDStreamIO;
  cur: IBPlusTreeCursor;
  v: TBytes;
  RelVA: TRelVA;
begin
  Result := False;

  c := CoreGet;
  Key := CreateStreamFromTagAndRelVA(c, FTag, VA);
  if not Assigned(Key) then
    Exit; // this va not found

  // Look for everything >= VA.
  cur := TVDCore(c).DB.CursorCreateEx((Key as TVDStreamIO).ToBytes, [kpEqual, kpGreater], False);
  if cur = nil then
    Exit;

  v := cur.Key;
  if not ParseRegionKey(v, RelVA) then
    Exit;

  Result := c.VM.RelToAbsVA(RelVA, VA);
end;

function TRegions.GetEx(VA: TVA; out SubId: byte; out RegionData: IVDStreamIO; NeedRegionData: boolean): IVDVARegion;
var
  Key, Data: IVDStreamIO;
  c: IVDCore;
  cur: IBPlusTreeCursor;
  k, v: TBytes;
  Size: UInt32;
  RelVA: TRelVA;
  AbsVA: TVA;
begin
  Result := nil;
  Data := nil;

  c := CoreGet;

  // Search key starting with FTag and rel VA.
  Key := CreateStreamFromTagAndRelVA(c, FTag, VA);
  if not Assigned(Key) then
    Exit; // this va not found

  cur := TVDCore(c).DB.CursorCreateEx((Key as TVDStreamIO).ToBytes, [kpEqual, kpLess], False);
  if cur = nil then
    Exit;

  k := cur.Key;
  v := cur.Value;

  // Verify Tag.
  // If found only Less item tag can be different.
  if k[0] <> FTag then
    Exit;

  // Get Size.
  Data := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(v));
  if Data.GetSize < 4 then
    Exit;

  // Read mandatory data (region size).
  Size := Data.ReadU32;

  // Get data sub-id.
  SubId := Data.ReadU8;

  if NeedRegionData then
  begin
    // Read user data.
    RegionData := TVDStreamIO.Create(TVDMemoryStream.Create);
    RegionData.CopyFrom(Data); // from current Data position to end
    RegionData.SetPosition(0);
  end;

  // Data done.

  // Get real start va.
  if not ParseRegionKey(k, RelVA) then
    Exit;

  if c.VM.RelToAbsVA(RelVA, AbsVA) then
  begin
    if (VA >= AbsVA) and (VA < AbsVA + Size) then
      Result := TVDVARegion.Create(AbsVA, Size);
  end;
end;

function TRegions.Get(VA: TVA; out SubId: byte;
  out RegionData: IVDStreamIO): IVDVARegion;
begin
  Result := GetEx(VA, SubId, RegionData, True);
end;

function TRegions.GetRegionAndSubId(VA: TVA; out SubId: byte): IVDVARegion;
var
  RegionData: IVDStreamIO;
begin
  Result := GetEx(VA, SubId, RegionData, False);
end;

function TRegions.GetRegionOnly(VA: TVA): IVDVARegion;
var
  RegionData: IVDStreamIO;
  SubId: byte;
begin
  Result := GetEx(VA, SubId, RegionData, False);
end;

function TRegions.Put(VA: TVA; Size: UInt32; SubId: byte; const RegionData: IVDStreamIO): BOOL;
var
  Key, Data: IVDStreamIO;
  KeyBytes, DataBytes: TBytes;
  c: IVDCore;
begin
  if Size = 0 then // no sense
    Exit(False);

  // Make sure there will be free space for region.
  DelRange(VA, Size);

  c := CoreGet;
  Key := CreateStreamFromTagAndRelVA(c, FTag, VA);
  if not Assigned(Key) then
    Exit(False);

  Data := TVDStreamIO.Create(TVDMemoryStream.Create);

  // Write default data.
  Data.WriteU32(Size);

  // Write Sub-id.
  Data.WriteU8(SubId);

  if Assigned(RegionData) then
  begin
    // Write user data.
    RegionData.SetPosition(0); // read from start
    Data.CopyFrom(RegionData);
  end;

  // Finally put.
  KeyBytes := (Key as TVDStreamIO).ToBytes;
  DataBytes := (Data as TVDStreamIO).ToBytes;
  Result := (c as TVDCore).DB.Put(KeyBytes, DataBytes) = BP_OK;
end;

function TRegions.Del(VA: TVA): SIZE_T;
var
  rgn: IVDVARegion;
  Key: IVDStreamIO;
  c: IVDCore;
  io: IVDStreamIO;
  SubId: byte;
begin
  rgn := Get(VA, SubId, io);
  if rgn = nil then
    Exit(0);
  c := CoreGet;
  Key := CreateStreamFromTagAndRelVA(c, FTag, rgn.GetStartVA);
  if (c as TVDCore).DB.Delete((Key as TVDStreamIO).ToBytes) = BP_OK then
    Result := rgn.GetSize
  else
    Result := 0;
  rgn := nil;
end;

function TRegions.DelRange(VA: TVA; Size: SIZE_T): SIZE_T;
var
  VA0, EndVA: TVA;
  DelSize: SIZE_T;
begin
  Result := 0;
  VA0 := VA;
  EndVA := VA + Size;
  while NextGreaterOrEqual(VA) and (VA < EndVA) do
  begin
    DelSize := Del(VA);
    Result := VA + DelSize - VA0;
  end;
end;

end.
