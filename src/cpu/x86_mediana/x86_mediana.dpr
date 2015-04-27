library x86_mediana;

uses
  VDAPI,
  uTPX86 in 'uTPX86.pas',
  mediana.print in 'mediana.print.pas',
  mediana.print.regs in 'mediana.print.regs.pas',
  uTPX86.regs in 'uTPX86.Regs.pas',
  uParseBasic in 'uParseBasic.pas';

{$R *.res}


procedure CreateTypeProvider(Name: BSTR_IN; out Result: IVDTypeProviderBase); stdcall;
var
  sName: string;
begin
  sName := Name;

  if (sName = TCpuName.X16) then
    Result := TCPU_X16.Create
  else if (sName = TCpuName.X32) then
    Result := TCPU_X32.Create
  else if (sName = TCpuName.X64) then
    Result := TCPU_X64.Create
  else
    Result := nil;
end;

var
  PluginInfo: TVDPluginInfo;
  ProvidedTypes: packed array [0 .. 3] of TVDProvidedTypeInfo;

exports
  PluginInfo name TVDPluginExport.PluginInfo,
  CreateTypeProvider name TVDPluginExport.CreateTypeProvider,
  ProvidedTypes name TVDPluginExport.ProvidedTypes;

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  // Base information.
  PluginInfo.Name := 'X86+ CPU Plugin';

  // CPUs.
  ProvidedTypes[0].Name := TCpuName.X16;
  ProvidedTypes[0].Flags := TVDProvidedTypeFlag.TypeIsCPU;

  ProvidedTypes[1].Name := TCpuName.X32;
  ProvidedTypes[1].Flags := TVDProvidedTypeFlag.TypeIsCPU;

  ProvidedTypes[2].Name := TCpuName.X64;
  ProvidedTypes[2].Flags := TVDProvidedTypeFlag.TypeIsCPU;

  ProvidedTypes[3].Name := nil;

end.
