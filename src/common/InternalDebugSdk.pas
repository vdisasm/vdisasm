{
  Functions to help in debugging of Core.
}
unit InternalDebugSdk;

interface

uses
  VDAPI;

type
  IVDInternalDebugUtils = interface
    ['{862E441C-1369-49B7-A93D-77A1457E5B7A}']
    procedure DumpTypeProviders;
    procedure DumpLoadedPlugins;
    procedure DumpTypePluginMapping;
  end;

function InternalDbgUtilsGet(): IVDInternalDebugUtils; stdcall;
  external LIB;

implementation

end.
