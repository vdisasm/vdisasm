library tp_std;

uses
  TypeProvider.Std.Floats,
  TypeProvider.Std.GUID,
  TypeProvider.Std.Integers,
  VDAPI;

const
  std_guid = 'std.GUID';

procedure CreateTypeProvider(Name: BSTR_IN; out Result: IVDTypeProviderBase); stdcall;
var
  sName: string;
begin
  sName := Name;

  // signed int
  if (sName = TVDStdTypeName.s8) then
    Result := TTP_Int<int8>.Create
  else if (sName = TVDStdTypeName.s16) then
    Result := TTP_Int<int16>.Create
  else if (sName = TVDStdTypeName.s32) then
    Result := TTP_Int<int32>.Create
  else if (sName = TVDStdTypeName.s64) then
    Result := TTP_Int<int64>.Create

    // unsigned int
  else if (sName = TVDStdTypeName.u8) then
    Result := TTP_Int<uint8>.Create
  else if (sName = TVDStdTypeName.u16) then
    Result := TTP_Int<uint16>.Create
  else if (sName = TVDStdTypeName.u32) then
    Result := TTP_Int<uint32>.Create
  else if (sName = TVDStdTypeName.u64) then
    Result := TTP_Int<uint64>.Create

    // float
  else if (sName = TVDStdTypeName.f32) then
    Result := TTP_Float<single>.Create
  else if (sName = TVDStdTypeName.f64) then
    Result := TTP_Float<double>.Create

    // guid
  else if (sName = std_guid) then
    Result := TTP_GUID.Create

  else
    Result := nil;

end;

var
  PluginInfo: TVDPluginInfo;
  ProvidedTypes: packed array [0 .. 12] of TVDProvidedTypeInfo;

exports
  PluginInfo name TVDPluginExport.PluginInfo,
  CreateTypeProvider name TVDPluginExport.CreateTypeProvider,
  ProvidedTypes name TVDPluginExport.ProvidedTypes;

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  PluginInfo.Desc := 'Provider of standard types';

  ProvidedTypes[0].Name := TVDStdTypeName.s8;
  ProvidedTypes[1].Name := TVDStdTypeName.s16;
  ProvidedTypes[2].Name := TVDStdTypeName.s32;
  ProvidedTypes[3].Name := TVDStdTypeName.s64;

  ProvidedTypes[4].Name := TVDStdTypeName.u8;
  ProvidedTypes[5].Name := TVDStdTypeName.u16;
  ProvidedTypes[6].Name := TVDStdTypeName.u32;
  ProvidedTypes[7].Name := TVDStdTypeName.u64;

  ProvidedTypes[8].Name := TVDStdTypeName.f32;
  ProvidedTypes[9].Name := TVDStdTypeName.f64;

  ProvidedTypes[10].Name := std_guid;

  ProvidedTypes[11].Name := nil;

end.
