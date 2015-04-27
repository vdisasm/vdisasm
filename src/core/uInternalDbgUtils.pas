unit uInternalDbgUtils;

interface

uses
  System.Generics.Collections,
  VDAPI,
  InternalDebugSdk;

{$IFDEF DEBUG}


type
  TVDInternalDebugUtils = class(TInterfacedObject, IVDInternalDebugUtils)
  public
    procedure DumpTypeProviders;
    procedure DumpLoadedPlugins;
    procedure DumpTypePluginMapping;
  end;
{$ENDIF}

implementation

uses
  uCore,
  uTypes.Mgr,
  uPlugins;

var
  Singleton: IVDInternalDebugUtils;

function InternalDbgUtilsGet(): IVDInternalDebugUtils; stdcall;
begin
{$IFDEF DEBUG}
  if Singleton = nil then
    Singleton := TVDInternalDebugUtils.Create;
{$ENDIF}
  Result := Singleton;
end;

exports
  InternalDbgUtilsGet;
{$IFDEF DEBUG}

{ TVDInternalDebugUtils }

procedure TVDInternalDebugUtils.DumpLoadedPlugins;
var
  c: IVDCore;
  p: TVDPluginManager;
  s: string;
begin
  c := CoreGet();
  p := (c.PluginMgr as TVDPluginManager);
  if p.LoadedPlugins.Count = 0 then
    c.Log.WriteLn('No loaded plugins')
  else
  begin
    c.Log.WriteLn('Loaded plugins:');
    for s in p.LoadedPlugins.Keys do
      c.Log.WriteLn('  ' + s);
  end;
end;

procedure TVDInternalDebugUtils.DumpTypePluginMapping;
var
  c: IVDCore;
  Map: TMapPluginsForTypes;
  Pair: TPair<TTypeName, TPluginPathList>;
  Path: string;
begin
  c := CoreGet();
  Map := (c.TypeMgr as TVDTypeMgr).MapPluginsForTypes;
  if Map.Count = 0 then
    c.Log.WriteLn('Type -> Plugin mapping: Empty')
  else
    c.Log.WriteLn('Type -> Plugin mapping: ');
  for Pair in Map do
  begin
    c.Log.WriteLn('  ' + Pair.Key);
    for Path in Pair.Value do
      c.Log.WriteLn('    ' + Path);
  end;
end;

procedure TVDInternalDebugUtils.DumpTypeProviders;
begin
  (CoreGet().TypeMgr as TVDTypeMgr).Dump;
end;

{$ENDIF}


end.
