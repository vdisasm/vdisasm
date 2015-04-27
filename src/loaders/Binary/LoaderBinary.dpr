library LoaderBinary;

uses
  VDAPI,
  uPlugin in 'uPlugin.pas';

procedure CreatePlugin(out Result: IUnknown); stdcall;
begin
  Result := TBinaryLoader.Create;
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
  PluginInfo.Name := 'Binary Loader';
  PluginInfo.Desc := 'Load binary data to database';

end.
