unit uCore.MakeAndUndef;

interface

uses
  System.SysUtils,
  uCore,
  uCore.Strings,
  VDAPI;

function DoMakeType(const c: TVDCore; VA: TVA; TypeStr: BSTR_IN; Flags: TMakeTypeFlags): uint32;

function DoUndef(const c: TVDCore; VA: TVA; Size: SIZE_T): SIZE_T;

implementation

uses
  uTypes.Lib,
  uTypes.Mgr,
  uTypes.Parsing,
  uRegions.TypeData;

type
  TMakeStatus =
    (
    msOK,           // found and returned <> 0
    msTypeNotFound, // no type found
    msDecodeFailed, // returned 0
    msTypeMapFailed // failed to write type into map
    );

function DoFindType(const c: TVDCore; const TypeLibName, TypeStr: string): IVDType;
var
  Lib: TVDTypeLibrary;
begin
  // Search type in current and/or imported libs.
  Lib := TVDTypeLibrary(c.GetTypeLib);
  if Lib = nil then
  begin
    c.Log.WriteLn('Database type library not created.');
    exit(nil);
  end;
  Result := Lib.FindType(BSTR_IN(TypeLibName), BSTR_IN(TypeStr), True);
end;

function BitSizeToByteSize(BitSize: integer): integer; inline;
begin
  Result := BitSize div 8;
  if (BitSize mod 8) <> 0 then
    inc(Result);
end;

function PutType(const c: TVDCore; VA: TVA; UID: TTypeUID; Size: uint32): boolean;
var
  tdr: TVDRegionsTypeData;
begin
  tdr := TVDCore(c).TypeDataRegions;
  Result := tdr.PutUidAndData(VA, UID, Size);
  if not Result then
  begin
    c.Log.WriteLn(Format(SFailedToWriteTypeAtVA, [va]));
  end;
end;

function DoMakeType(const c: TVDCore; VA: TVA; TypeStr: BSTR_IN; Flags: TMakeTypeFlags): uint32;
var
  TypeLibName: string;
  TypeName: string;
  Size: uint32; // byte aligned
  UID: TTypeUID;
  FoundType: IVDType;
  Mgr: TVDTypeMgr;
  TypeItem: TTypeMgrItem;
begin
  if VA = BAD_VA then
    exit(0);

  Mgr := (c.TypeMgr as TVDTypeMgr);

  // Try to find type provider directly in TypMgr. It has higher priority
  // than typelib.
  Mgr.GetEx(TypeStr, TypeItem);
  if (TypeItem <> nil) and (TypeItem.Provider <> nil) then
  begin
    Size := TypeItem.Provider.Decode(VA, TVDDataEncoding.Size, nil);
    UID := TypeItem.UID;
  end
  else
  begin
    // If not found in TypeMgr.
    // Try to find in typelib.

    FullTypeNameToTypeLibAndType(TypeStr, TypeLibName, TypeName);
    FoundType := DoFindType(c, TypeLibName, TypeName);

    if FoundType = nil then
    begin
      // todo: make this problem instead of log (performance boost).
      // c.Log.WriteLn(Format('Type not found: %s', [TypeStr]));
      exit(0);
    end;

    // Type found. It must be registered in database (if not yet registered).
    Mgr.GetExistingOrAddNewUID(FoundType.Name, UID);

    Size := BitSizeToByteSize(FoundType.BitSize);
  end;

  // At this stage need only Size and UID.
  //
  // Size: is size of the type at VA.
  // UID:  is UID of type in type mgr.

  if Size = 0 then
    exit(0);

  // C.Log.WriteLn(Format('Type "%s" is %d byte size.', [TypeName, Size]));

  if not PutType(c, VA, UID, Size) then
    exit(0);

  Result := Size;
end;

function DoUndef(const c: TVDCore; VA: TVA; Size: SIZE_T): SIZE_T;
begin
  Result := c.TypeDataRegions.DelRange(VA, Size);
end;

end.
