library LoaderPE;

uses
  VDAPI,
  uPlugin in 'uPlugin.pas',
  uLoaderDOS in 'uLoaderDOS.pas';

procedure CreatePlugin(out Result: IUnknown); stdcall;
begin
  Result := TLoaderPE.Create;
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
  PluginInfo.name := 'DOS & Portable Executable Loader';
  PluginInfo.Desc := 'Load DOS & PE image to database';
end.
