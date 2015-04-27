{
  Plugin manager implementation.
}
unit uPlugins;

interface

uses
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,
  System.Types,
  VDLib.Platform,
  VDAPI;

type
  // Expanded plugin path.
  TPluginPath = type string;

  TLoadedPluginInfo = record
  public
    Fn: string;
    ModuleId: TModuleID;
    //
    Plg: TVDPluginInfo;
    CreateTypeProvider: TVDPluginCreateTypeProvider;
  public
    CreatedPlugin: IUnknown;
  end;

  TPluginList = TList<IVDPlugin>;

  TLoadedPluginInfoPair = TPair<TPluginPath, TLoadedPluginInfo>;
  TLoadedPluginInfos = TDictionary<TPluginPath, TLoadedPluginInfo>;

  { TVDPluginManager }

  TVDPluginManager = class(TInterfacedObject, IVDPluginManager)
  private
    // Plugins loaded in current process.
    FLoadedPlugins: TLoadedPluginInfos;
    // procedure LoadedPluginsValueNotify(Sender: TObject; const Item: TLoadedPluginInfo;
    // Action: TCollectionNotification);
  public type
    TPluginLoadCallback = reference to procedure(const Info: TLoadedPluginInfo);
  public
    // Remove all plguin info.
    procedure ClearPluginList;

    // Load plugin module by already expanded filename and return its info.
    // Plugin isn't initialized or run. Plugin interface is alive since this moment.
    function LoadPluginInternal(
      FileName: BSTR;
      Flags: TPluginScanFlags;
      out Info: TLoadedPluginInfo;
      out AlreadyLoaded: boolean
      ): boolean;

    function LoadPluginInternalAndInit(
      const FileName: string;
      Flags: TPluginScanFlags;
      cb: TPluginLoadCallback
      ): boolean;

    // Free plugin and unload plugin module.
    function UnloadPluginInternal(var Info: TLoadedPluginInfo): BOOL; overload; stdcall;
  public { internal }
    constructor Create;
    destructor Destroy; override;

    // Unload all currently loaded plugins.
    procedure UnloadPlugins; // stdcall;

    property LoadedPlugins: TLoadedPluginInfos read FLoadedPlugins;
  public
    { VDAPI }
    procedure ScanPluginDir(Path: BSTR_IN; Callback: TPluginScanFunc; Flags: TPluginScanFlags; ud: pointer = nil); stdcall;
    function RunPlugin(FileName: BSTR): BOOL; stdcall;
  end;

implementation

uses
  uCore,
  uCPUs;

{ TVDPluginManager }

procedure TVDPluginManager.ClearPluginList;
begin
  FLoadedPlugins.Clear;
end;

constructor TVDPluginManager.Create;
begin
  inherited;
  FLoadedPlugins := TLoadedPluginInfos.Create;
  // FLoadedPlugins.OnValueNotify := LoadedPluginsValueNotify;
end;

destructor TVDPluginManager.Destroy;
begin
  UnloadPlugins;
  FLoadedPlugins.Free;
  inherited;
end;

procedure MakeSurePluginExtension(var Fn: string);
const
  ext = '.dll';
begin
  if not Fn.ToLower.EndsWith(ext) then
    Fn := Fn + ext;
end;

function TVDPluginManager.LoadPluginInternal(
  FileName: BSTR;
  Flags: TPluginScanFlags;
  out Info: TLoadedPluginInfo;
  out AlreadyLoaded: boolean): boolean;
var
  c: IVDCore;

  // todo: !!! it must not be called twice or more.
  // Return True if at least one type name found.
  function TryGetProvidedTypes: boolean;
  var
    Sym: PVDProvidedTypeInfo;
    Cur: PVDChar;
  begin
    // Provided types.
    Sym := TVDLibrary.GetSymbolByName(Info.ModuleId, TVDPluginExport.ProvidedTypes);
    if not Assigned(Sym) then
      Exit(False);
    while Assigned(Sym.Name) do
    begin
      // If type is marked as cpu, add it to cpu list.
      if (Sym.Flags and TVDProvidedTypeFlag.TypeIsCPU) <> 0 then
      begin
        (c.CPUs as TVDCpus).AddInternal(Sym.Name);
      end;

      Cur := Sym.Name;
      inc(Sym);
      c.TypeMgr.RegisterPluginForType(BSTR_IN(Cur), BSTR_IN(FileName));
    end;
    Exit(True);
  end;

var
  CreateFn: TVDPluginCreateFunc;
  Fn: string;
  PlgInfoInModule: PVDPluginInfo;
begin
  Fn := IOGet.ExpandPath(FileName);
  MakeSurePluginExtension(Fn);

  Info := Default (TLoadedPluginInfo);

  c := CoreGet();

  AlreadyLoaded := False;

  // Check if plugin already loaded.
  if FLoadedPlugins.TryGetValue(Fn, Info) then
  begin
{$IFDEF DEBUG}
    c.Log.WriteLn('Plugin already loaded: ' + Fn);
{$ENDIF}
    AlreadyLoaded := True;
    Exit(True);
  end;

  if not TFile.Exists(Fn) then
    Exit(False);

  // Load module.
  Info.ModuleId := TVDLibrary.Load(Fn);
  if Info.ModuleId = 0 then
    Exit(False);

  // Get "PluginInfo"
  PlgInfoInModule := TVDLibrary.GetSymbolByName(Info.ModuleId, TVDPluginExport.PluginInfo);
  if Assigned(PlgInfoInModule) then
  begin
    Info.Fn := Fn;
    Info.Plg := PlgInfoInModule^;
  end;

  // Get "CreateTypeProvider"
  Info.CreateTypeProvider := TVDLibrary.GetSymbolByName(Info.ModuleId, TVDPluginExport.CreateTypeProvider);

  // Get "CreatePlugin"
  CreateFn := TVDLibrary.GetSymbolByName(Info.ModuleId, TVDPluginExport.CreatePlugin);

  if (Flags and VDAPI.TPluginScanFlag.PLGSCAN_TYPEPROVIDERS) <> 0 then
    TryGetProvidedTypes;

  if Assigned(CreateFn) then
    CreateFn(Info.CreatedPlugin)
  else
    Info.CreatedPlugin := nil;

  // Register plugin as loaded.
  FLoadedPlugins.Add(Fn, Info);
{$IFDEF DEBUG}
  c.Log.WriteLn(Format('[plugin:loaded] "%s"', [Info.Plg.Name]));
{$ENDIF}
  Exit(True);
end;

function TVDPluginManager.UnloadPluginInternal(var Info: TLoadedPluginInfo): BOOL;
{$IFDEF DEBUG}
var
  c: IVDCore;
  Name: string;
{$ENDIF}
begin
{$IFDEF DEBUG}
  c := CoreGet;
  if Assigned(c) then
  begin
    Name := Info.Plg.Name;
    c.Log.WriteLn(Format('[plugin:unloading] %s', [Name]));
  end;
{$ENDIF}
  FLoadedPlugins.Remove(Info.Fn);
  if Assigned(Info.CreatedPlugin) then
  begin
    Info.CreatedPlugin := nil;
  end;
  TVDLibrary.Unload(Info.ModuleId);
  Result := True;
end;

procedure TVDPluginManager.UnloadPlugins;
var
  arr: TArray<TLoadedPluginInfoPair>;
  i: integer;
begin
  // Must use ToArray. Cannot use "for in" consturction, because of interface
  // references and module unloading in UnloadPluginInternal.
  arr := FLoadedPlugins.ToArray;
  for i := 0 to High(arr) do
    UnloadPluginInternal(arr[i].Value);
end;

function TVDPluginManager.RunPlugin(FileName: BSTR): BOOL;
begin
  Result := LoadPluginInternalAndInit(FileName, 0,
    procedure(const Info: TLoadedPluginInfo)
    begin
      if Supports(Info.CreatedPlugin, IVDPlugin) then
        (Info.CreatedPlugin as IVDPlugin).Run;
    end);
end;

function TVDPluginManager.LoadPluginInternalAndInit(
  const FileName: string;
Flags: TPluginScanFlags;
cb: TPluginLoadCallback): boolean;
var
  Info: TLoadedPluginInfo;
  AlreadyLoaded: boolean;
  Plg: IVDPluginBase;
  PlgInited: boolean;
begin
  if not LoadPluginInternal(FileName, 0, Info, AlreadyLoaded) then
  begin
    CoreGet.Log.WriteLn('Failed to load plugin: ' + FileName);
    Exit(False);
  end;

  try
    if Supports(Info.CreatedPlugin, IVDPluginBase) then
    begin
      Plg := Info.CreatedPlugin as IVDPluginBase;
      try
        PlgInited := Plg.Init();
      finally
        Plg := nil; 
      end;
      if not PlgInited then
      begin
        CoreGet.Log.WriteLn('Plugin init failed: ' + FileName);
        Exit(False);
      end;
    end;

    cb(Info);

    Exit(True);
  finally
    UnloadPluginInternal(Info);
  end;
end;

procedure TVDPluginManager.ScanPluginDir(
  Path: BSTR_IN;
Callback: TPluginScanFunc;
Flags: TPluginScanFlags;
ud: pointer);
var
  FileName: string;
  ExpandedPath: string;
  Files: TStringDynArray;
  Info: TLoadedPluginInfo;
  bAliveWhileCore: boolean;
  bNeedEnumeration: boolean;
  AlreadyLoaded: boolean;
begin
  if not Assigned(Callback) then
    Exit;

  // Expand path.
  if not Assigned(Path) then
    ExpandedPath := IOGet.ExpandPath(TIOVar.Plugins)
  else
    ExpandedPath := IOGet.ExpandPath(Path);

  if not TDirectory.Exists(ExpandedPath) then
    Exit;

  Files := TDirectory.GetFiles(ExpandedPath, '*', TSearchOption.soAllDirectories);

  for FileName in Files do
  begin
    if LoadPluginInternal(FileName, Flags, Info, AlreadyLoaded) then
    begin
      bAliveWhileCore := (Info.Plg.Flags and TVDPluginInfoFlag.AliveWhileCoreIsAlive) <> 0;
      bNeedEnumeration := (Info.Plg.Flags and TVDPluginInfoFlag.DontEnumerate) = 0;

      try
        if not AlreadyLoaded then
        begin
          // If plugin will be alive, run it.
          if bAliveWhileCore then
          begin
            if Supports(Info.CreatedPlugin, IVDPlugin) then
              (Info.CreatedPlugin as IVDPlugin).Run;
          end;
        end;

        if bNeedEnumeration then
          if not Callback(BSTR_IN(FileName), Info.CreatedPlugin, @Info.Plg, ud) then
            break;

      finally
        // Unload plugin.
        // Don't unload it if it must be alive while core is alive.
        if (not bAliveWhileCore) then
          UnloadPluginInternal(Info);
      end;
    end;
  end;
end;

end.
