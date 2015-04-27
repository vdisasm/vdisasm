library LoaderELF;

uses
  VDAPI,
  uPlugin in 'uPlugin.pas';

procedure CreatePlugin(out Result: IUnknown); stdcall;
begin
  Result := TELFLoader.Create;
end;

var
  PluginInfo: TVDPluginInfo;

exports
  PluginInfo name TVDPluginExport.PluginInfo,
  CreatePlugin name TVDPluginExport.CreatePlugin;

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  PluginInfo.name := 'ELF Loader';
end.
