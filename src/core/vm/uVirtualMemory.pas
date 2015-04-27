{
  Virtual Memory implementation.
  Mostly wrapper on Sections.
}
unit uVirtualMemory;

interface

uses
  VDAPI,
  uSections;

const
  ENABLE_VM_CACHE_BY_DEFAULT = True;
  MAX_CACHE_SIZE             = 8192;

type
  TCacheSize = uint32;

  TVDVirtualMemory = class(TInterfacedObject, IVDVirtualMemory)
  private
    FSectionsIntf: IVDSections;
    FSectionsClass: TVDSections;
    function RW(Read: boolean; VA: TVA; Buffer: Pointer; Size: SIZE_T; ToEndianness: TEndianness): SIZE_T;
    procedure CacheReload(VA: TVA; out Sec: IVDSection);
  private
    // Cache is used to make sequential read fast.
    // Reading by 1 byte currently is about 60x faster than normal reading.
    FCacheEnabled: boolean;
    FCacheVA: TVA;
    FCacheSize: TCacheSize;
    FCache: array of byte;
  public
    constructor Create;
    destructor Destroy; override;
  public
    { VDAPI }
    function GetSections: IVDSections; stdcall;
    function GetFirstVA(OutVA: PVA): BOOL; stdcall;
    function GetLastVA(OutVA: PVA): BOOL; stdcall;
    function Exists(VA: TVA): BOOL; stdcall;
    function AbsToRelVA(VA: TVA; out RelVA: TRelVA): BOOL; stdcall;
    function RelToAbsVA(const RelVA: TRelVA; out VA: TVA): BOOL; stdcall;
    function Read(VA: TVA; Buffer: Pointer; Size: SIZE_T; ToEndianness: TEndianness = TEndianness.None): SIZE_T; stdcall;
    function Write(VA: TVA; Buffer: Pointer; Size: SIZE_T; ToEndianness: TEndianness = TEndianness.None): SIZE_T; stdcall;
    function CreateStreamIO(FirstVA, LastVA: TVA; ReadOnly: BOOL): IVDStreamIO; stdcall;

    procedure CacheSetEnabled(Value: BOOL); stdcall;
    procedure CacheInvalidate; stdcall;
  end;

implementation

uses
  System.SysUtils, // raise
  uEndianness,
  uSection,
  uStream,
  uStream.VAStream;

{ TVDVirtualMemory }

function TVDVirtualMemory.AbsToRelVA(VA: TVA; out RelVA: TRelVA): BOOL;
var
  Sec: TVDSection;
begin
  Sec := TVDSection(FSectionsClass.Find(VA));
  if Sec = nil then
    Exit(False);
  RelVA.SecID := Sec.GetID;
  RelVA.SecOfs := VA - Sec.GetStartVA;
  Result := True;
end;

function TVDVirtualMemory.RelToAbsVA(const RelVA: TRelVA; out VA: TVA): BOOL;
var
  Sec: TVDSection;
begin
  Sec := TVDSection(FSectionsClass.FindByID(RelVA.SecID));
  if Assigned(Sec) and (RelVA.SecOfs < Sec.GetSize) then
  begin
    VA := Sec.GetStartVA + RelVA.SecOfs;
    Exit(True);
  end;
  Exit(False);
end;

procedure TVDVirtualMemory.CacheReload(VA: TVA; out Sec: IVDSection);
begin
  // try add to cache
  Sec := FSectionsClass.Find(VA);
  if Sec <> nil then
  begin
    if FCache = nil then
      SetLength(FCache, MAX_CACHE_SIZE);

    FCacheVA := VA;
    FCacheSize := Sec.Read(VA, @FCache[0], MAX_CACHE_SIZE);
  end;
end;

constructor TVDVirtualMemory.Create;
begin
  inherited Create();
  FSectionsIntf := TVDSections.Create;
  FSectionsClass := FSectionsIntf as TVDSections;
  FCacheEnabled := ENABLE_VM_CACHE_BY_DEFAULT;
end;

function TVDVirtualMemory.CreateStreamIO(FirstVA, LastVA: TVA;
  ReadOnly: BOOL): IVDStreamIO;
begin
  Result := TVDStreamIO.Create(
    TVDVAStream.Create(FirstVA, LastVA, ReadOnly),
    CoreGet().GetData.Endianness);
end;

destructor TVDVirtualMemory.Destroy;
begin
  FSectionsIntf := nil;
  FSectionsClass := nil;
  inherited;
end;

function TVDVirtualMemory.Exists(VA: TVA): BOOL;
begin
  Result := FSectionsClass.Find(VA) <> nil;
end;

function TVDVirtualMemory.GetLastVA(OutVA: PVA): BOOL;
begin
  if FSectionsClass.Items.Last <> nil then
  begin
    OutVA^ := FSectionsClass.Items.Last^.K.GetLastVA;
    Exit(True);
  end;
  Exit(False);
end;

function TVDVirtualMemory.GetFirstVA(OutVA: PVA): BOOL;
begin
  if FSectionsClass.Items.First <> nil then
  begin
    OutVA^ := FSectionsClass.Items.First^.K.GetStartVA;
    Exit(True);
  end;
  Exit(False);
end;

function TVDVirtualMemory.GetSections: IVDSections;
begin
  Result := FSectionsIntf;
end;

function TVDVirtualMemory.RW(Read: boolean; VA: TVA; Buffer: Pointer;
  Size: SIZE_T; ToEndianness: TEndianness): SIZE_T;
var
  cache_ofs: TCacheSize;
  cache_available: TCacheSize; // can get from cache
var
  Sec: IVDSection;
  srcEndianness: TEndianness;
begin
  Result := 0;
  Sec := nil;

  if FCacheEnabled then
  begin
    // Try cache.
    // Is VA in cache?
    if (VA >= FCacheVA) and (VA < FCacheVA + FCacheSize) then
    begin
      // Cache hit.
      // But ceched block can be smaller than requested size.
      cache_ofs := VA - FCacheVA;
      cache_available := FCacheSize - cache_ofs;
      if cache_available < Size then
      begin
        CacheInvalidate;
      end
      else
      begin
        // whole block in cache
        Result := Size;
        if read then
          move(FCache[cache_ofs], Buffer^, Size)
        else
          move(Buffer^, FCache[cache_ofs], Size);
      end;
    end
    else
    begin
      // cache miss
      CacheReload(VA, Sec);
    end;
  end;

  // If cache failed or disabled do real reading.
  if Result = 0 then
  begin
    // Find section by VA.
    if Sec = nil then // sec could be found before
    begin
      Sec := FSectionsClass.Find(VA);
      if Sec = nil then
        Exit(0);
    end;

    if Read then
      Result := Sec.Read(VA, Buffer, Size)
    else
      Result := Sec.Write(VA, Buffer, Size);
  end;

  // Convert endianness
  if ToEndianness <> TEndianness.None then
  begin
    srcEndianness := CoreGet().GetData.Endianness;
    BufConvertEndianness(Buffer, Size, srcEndianness, ToEndianness);
  end;
end;

function TVDVirtualMemory.Read(VA: TVA; Buffer: Pointer; Size: SIZE_T; ToEndianness: TEndianness): SIZE_T;
begin
  Result := RW(True, VA, Buffer, Size, ToEndianness);
end;

function TVDVirtualMemory.Write(VA: TVA; Buffer: Pointer; Size: SIZE_T; ToEndianness: TEndianness): SIZE_T;
begin
  Result := RW(False, VA, Buffer, Size, ToEndianness);
end;

procedure TVDVirtualMemory.CacheSetEnabled(Value: BOOL);
begin
  if Value <> FCacheEnabled then
  begin
    FCacheEnabled := Value;
    CacheInvalidate;
  end;
end;

procedure TVDVirtualMemory.CacheInvalidate;
begin
  FCacheSize := 0;
end;

end.
