unit uDebugSession;

interface

uses
  System.Generics.Collections,
  System.SyncObjs,
  System.SysUtils,

  uPlugins,

  gmap,
  grbtree,

  VDAPI;

type
  // TBreakpoints = TDictionary<TVA, TVDBreakPointRecord>;
  TBreakpoints = TRBTree<TVDBreakPointRecord>;

  TVDDebugSession = class(TInterfacedObject, IVDDebugSession)
  protected
    FState: TVDDebuggerState;
    // Path of debugger plugin if it is selected
    FPlgPath: string;
    FPlgInf: TLoadedPluginInfo;
    // Debugger plugin
    FDbg: IVDDebuggerPlugin;
    FBreakpoints: TBreakpoints;

    // Make all breakpoints inactive (not delete).
    procedure DeactivateAllBreakpoints;
  public
    procedure UnloadDebuggerPlugin;
  public
    constructor Create;
    destructor Destroy; override;

    procedure on_MSG_DBG_STOPPED;
  public

    { VDAPI }

    function SelectDebuggerPlugin(PluginPath: BSTR_IN): BOOL; stdcall;

    procedure GetDebugger(ForceLoad: BOOL; out Dbg: IVDDebuggerPlugin); stdcall;
    procedure UnloadDebugger; stdcall;

    function Stop: TVDDebugStatus; stdcall;

    function GetState: TVDDebuggerState; stdcall;
    procedure SetState(Value: TVDDebuggerState); stdcall;

    // BP can be nil, see result to know if breakpoint exists.
    function GetBreakpoint(VA: TVA; { out } BP: PVDBreakPointRecord): BOOL; stdcall;
    function SetBreakpoint(VA: TVA; BP: PVDBreakPointRecord): TVDDebugStatus; stdcall;
    function DelBreakpoint(VA: TVA): TVDDebugStatus; stdcall;

    // Activate breakpoint for triggering or delete it depending on State.
    function ActivateBreakpoint(BP: PVDBreakPointRecord; State: BOOL): BOOL; stdcall;

    procedure EnumBreakpoints(VAFirst, VALast: TVA; cb: TEnumBreakpointsFunc; ud: pointer); stdcall;

    function LiveCall(VA: TVA; Params: BSTR_IN): BOOL; stdcall;

    function GetRegisterValue(Name: BSTR_IN; out Value: IVDConstExpression): BOOL; stdcall;
  end;

implementation

uses
  uCore.Strings,
  uDebugLiveCall,
  uSections;

{ TVDDebugSession }

function TVDDebugSession.ActivateBreakpoint(BP: PVDBreakPointRecord; State: BOOL): BOOL;
var
  bIsActive: boolean;
begin
  // Debugger must be active.
  if FState = DBGSTATE_INACTIVE then
    Exit(False);
  // Breakpoint must be Enabled to be Activated.
  if (BP.Flags and VDAPI.TVDBreakpointFlag.BPF_ENABLED) = 0 then
    Exit(False);

  // Exit if it's already in needed state.
  bIsActive := (BP.Flags and VDAPI.TVDBreakpointFlag.BPF_ACTIVE) <> 0;
  if bIsActive = State then
    Exit(True);

  // Ask debugger to perform breakpoint installation/deletion.
  if not FDbg.ActivateBreakpoint(BP, State) then
    Exit(False);
  // Modify flags on success.
  if State then
    BP.Flags := BP.Flags or VDAPI.TVDBreakpointFlag.BPF_ACTIVE // activated
  else
    BP.Flags := BP.Flags and (not VDAPI.TVDBreakpointFlag.BPF_ACTIVE); // deactivated
  // Done.
  Exit(True);
end;

constructor TVDDebugSession.Create;
begin
  inherited;
  FBreakpoints := TBreakpoints.Create(
    function(const A, B: TVDBreakPointRecord): boolean
    begin
      result := A.VA < B.VA;
    end
    );
end;

destructor TVDDebugSession.Destroy;
begin
  FBreakpoints.Free;
  inherited;
end;

procedure TVDDebugSession.GetDebugger(ForceLoad: BOOL; out Dbg: IVDDebuggerPlugin);
var
  pm: TVDPluginManager;
  AlreadyLoaded: boolean;
begin
  if FDbg = nil then
  begin
    if (not Assigned(FPlgInf.CreatedPlugin)) and ForceLoad then
    begin
      // Load plugin.
      pm := CoreGet().PluginMgr as TVDPluginManager;

      if not pm.LoadPluginInternal(FPlgPath, 0, FPlgInf, AlreadyLoaded) then
        raise Exception.Create('can''t load debugger plugin');
    end;
    FDbg := FPlgInf.CreatedPlugin as IVDDebuggerPlugin;
  end;

  Dbg := FDbg;
end;

function TVDDebugSession.GetState: TVDDebuggerState;
begin
  result := FState;
end;

function TVDDebugSession.LiveCall(VA: TVA; Params: BSTR_IN): BOOL;
begin
  result := uDebugLiveCall.LiveCall(FDbg, VA, Params);
end;

function TVDDebugSession.SelectDebuggerPlugin(PluginPath: BSTR_IN): BOOL;
begin
  FPlgPath := PluginPath;
  result := True;
end;

procedure TVDDebugSession.SetState(Value: TVDDebuggerState);
begin
  FState := Value;
  CoreGet.Msg.Broadcast(MSG_DBG_STATE_CHANGED);
end;

procedure TVDDebugSession.UnloadDebugger;
begin
  FDbg := nil;
  UnloadDebuggerPlugin;
end;

procedure TVDDebugSession.UnloadDebuggerPlugin;
var
  pm: TVDPluginManager;
begin
  // Release interface.
  FDbg := nil;
  // Unload module.
  pm := CoreGet().PluginMgr as TVDPluginManager;
  pm.UnloadPluginInternal(FPlgInf);
end;

function TVDDebugSession.Stop: TVDDebugStatus;
begin
  // It is meant Stop is called when session is active.
  if (FState = DBGSTATE_INACTIVE) then
    Exit(DBG_OK); // anyway it was stopped

  // Ask ui to stop debugger.
  CoreGet().Msg.Broadcast(MSG_DBG_WANT_STOP);

  result := DBG_OK;
end;

procedure TVDDebugSession.on_MSG_DBG_STOPPED;
begin
  DeactivateAllBreakpoints;
  (CoreGet.VM.Sections as TVDSections).DeleteDebugSections;
end;

function TVDDebugSession.GetBreakpoint(VA: TVA; { out } BP: PVDBreakPointRecord): BOOL;
var
  key: TVDBreakPointRecord;
  n: TBreakpoints.TRBNodePtr;
begin
  key.VA := VA;
  n := FBreakpoints.Find(key);
  if n = nil then
    Exit(False);
  if Assigned(BP) then
    BP^ := n.K;
  Exit(True);
end;

function TVDDebugSession.SetBreakpoint(VA: TVA; BP: PVDBreakPointRecord): TVDDebugStatus;
var
  key: TVDBreakPointRecord;
  n: TBreakpoints.TRBNodePtr;
begin
  key.VA := VA;
  n := FBreakpoints.Find(key);
  if n = nil then
    n := FBreakpoints.Add(BP^) // add
  else
    n.K := BP^; // modify

  // Try to activate breakpoint.
  // It can succeed only if debugger is active.
  // Otherwise activation will be done on debugger start (on process/module loaded).
  ActivateBreakpoint(@n.K, True);

  result := DBG_OK;
end;

function TVDDebugSession.DelBreakpoint(VA: TVA): TVDDebugStatus;
var
  n: TBreakpoints.TRBNodePtr;
  key: TVDBreakPointRecord;
  bDeactivated: boolean;
begin
  key.VA := VA;
  n := FBreakpoints.Find(key);

  if n = nil then
    Exit(DBG_NOT_FOUND);

  // Try to deactivate breakpoint.
  // If debugger is inactive it just removes bp from list (it's not even activated yet).
  if FState <> DBGSTATE_INACTIVE then
  begin
    bDeactivated := ActivateBreakpoint(@n.K, False);
    if not bDeactivated then
      Exit(DBG_ERROR);
  end;

  // If breakpoint was deactivated we can remove it from list.
  FBreakpoints.Delete(n);
  result := DBG_OK;
end;

procedure TVDDebugSession.EnumBreakpoints(VAFirst, VALast: TVA; cb: TEnumBreakpointsFunc; ud: pointer);
var
  n: TBreakpoints.TRBNodePtr;
  K: TVDBreakPointRecord;
  c: IVDCore;
begin
  if not Assigned(cb) then
    Exit;

  c := CoreGet;

  if VAFirst = BAD_VA then
    c.VM.GetFirstVA(@VAFirst);
  if VALast = BAD_VA then
    c.VM.GetLastVA(@VALast);

  K.VA := VAFirst;
  n := FBreakpoints.FindGreaterOrEqual(K);
  while (n <> nil) and (n.K.VA <= VALast) do
  begin
    if not cb(@n.K, ud) then
      break;
    FBreakpoints.Next(n);
  end;
end;

function cb_DeactivateAllBreakpoints(BP: PVDBreakPointRecord; ud: pointer): BOOL; stdcall;
begin
  BP.Flags := BP.Flags and (not TVDBreakpointFlag.BPF_ACTIVE);
  result := True;
end;

procedure TVDDebugSession.DeactivateAllBreakpoints;
begin
  EnumBreakpoints(BAD_VA, BAD_VA, cb_DeactivateAllBreakpoints, nil);
end;

function TVDDebugSession.GetRegisterValue(Name: BSTR_IN;
out Value: IVDConstExpression): BOOL;
var
  Session: IVDDebugSession;
  Dbg: IVDDebuggerPlugin;
begin
  Value := nil;
  result := False;
  Session := CoreGet.DebugSession;

  if Session.State <> DBGSTATE_PAUSED then
    Exit;

  // Get debugger.
  Dbg := nil;
  Session.GetDebugger(False, Dbg);
  if not Assigned(Dbg) then
    Exit;

  // If debugger is paused we can try some debugger expressions (registers).
  result := Dbg.GetRegisterValue(Name, Value);
end;

end.
