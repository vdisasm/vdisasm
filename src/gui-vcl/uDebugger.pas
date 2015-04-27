unit uDebugger;

interface

uses
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  System.TypInfo,

  Vcl.Dialogs, // ShowMessage

  uFormException,
  uFormLiveCall,

  VDAPI;

var
  SkipInitialBreakPoint: boolean = False;

function ChooseDebugger(const PlgPath: string): boolean;

procedure StartResumeDebugger;
procedure SuspendDebugger;
procedure ResumeDebugger;
procedure TerminateDebugger;

procedure StepIn;
procedure StepOver;
procedure RunTo(VA: TVA);

procedure ToggleBreakpoint(VA: TVA);

procedure LiveCall(VA: TVA);

implementation

uses
  uCore.Strings;

type
  TDebuggerCommand =
    (
    dtc_null,
    dtc_terminate,
    dtc_stepin,
    dtc_runto
    );

  TDebuggerThread = class(TThread)
  protected
    procedure Execute; override;
    procedure DbgThreadTerminate(Sender: TObject);
  protected
    // Current event (used from SyncDebugEventHappened).
    FEvent: TVDDebugEvent;
    // Synchronized proc to handle catched event.
    procedure SyncDebugEventHappened_Enter;
    procedure SyncDebugEventHappened_Leave;
    // Synchronized log just a brief info of event.
    procedure SyncLogEvent(const Event: TVDDebugEvent);
  public
    Host: string;
    Port: word;
  public
    // only if create new process
    CreateNewProcess: boolean;
    FileName: string;
    Params: string;
    WorkingDir: string;
  public
    // only if attach to process
    AttachToPID: TVDProcessId;
  public
    DebugStartedEvent: TEvent;
    FailureEvent: TEvent;  // any failure
    ContinueEvent: TEvent; // user set this event to let debugger to continue

    Command: TDebuggerCommand;
    CommandVA: TVA; // dtc_runto

    constructor Create;
    destructor Destroy; override;
  end;

var
  DbgThread: TDebuggerThread;
  VaBeforeDbg: TVA;

function ChooseDebugger(const PlgPath: string): boolean;
begin
  result := CoreGet.DebugSession.SelectDebuggerPlugin(BSTR_IN(PlgPath));
end;

procedure StartDebugger;
var
  C: IVDCore;
  Signaled: THandleObject;
  arr: THandleObjectArray;
  Dbg: IVDDebuggerPlugin;
  bDbgIsLoaded: boolean;
begin
  C := CoreGet;

  if Assigned(DbgThread) then
    FreeAndNil(DbgThread);

  if C.DebugSession.GetState <> DBGSTATE_INACTIVE then
  begin
    C.Log.WriteLn('Debugger already active.');
    Exit;
  end;

  // Try to get selected debugger.
  C.DebugSession.GetDebugger(True, Dbg);
  bDbgIsLoaded := Assigned(Dbg);

  // Release reference.
  // In case of OnTerminate called this reference may be obstacle to free
  // debugger intf.
  Dbg := nil;
  if not bDbgIsLoaded then
    Exit;

  DbgThread := TDebuggerThread.Create;
  DbgThread.OnTerminate := DbgThread.DbgThreadTerminate;

  // Currently we support only local debugger.
  DbgThread.Host := '';
  DbgThread.Port := 0;

  DbgThread.CreateNewProcess := True;
  DbgThread.FileName := C.InputFile.FileName;
  DbgThread.Params := C.InputFile.Params;
  DbgThread.WorkingDir := C.InputFile.WorkingDir;

  // Launch thread it must either run new process or attach to existing one.
  DbgThread.Start;

  // Check if thread startup succeeded. It will set one of two events.
  SetLength(arr, 2);
  arr[0] := DbgThread.DebugStartedEvent;
  arr[1] := DbgThread.FailureEvent;
  case TEvent.WaitForMultiple(arr, INFINITE, False, Signaled) of
    wrSignaled:
      begin
        if Signaled = DbgThread.FailureEvent then
        begin
          C.Log.WriteLn('Debuggee startup failed');
          TerminateDebugger;
        end
        else if Signaled = DbgThread.DebugStartedEvent then
        begin
          C.Log.WriteLn('Debugger started');
          // C.DebugSession.SetState(DBGSTATE_RUNNING);
        end;
      end;
  else
    begin
      TerminateDebugger;
      raise Exception.Create('Not expected debug event');
    end;
  end;

  C.Msg.Broadcast(MSG_DBG_STARTED);

  VaBeforeDbg := C.GetUserVA;
end;

procedure ResumeDebugger;
begin
  if Assigned(DbgThread) then
  begin
    DbgThread.ContinueEvent.SetEvent;
  end;
end;

procedure StartResumeDebugger;
begin
  // Inactive -> start
  // Active (paused) -> resume
  // Running -> do nothing
  case CoreGet().DebugSession.State of
    DBGSTATE_INACTIVE:
      StartDebugger;
    DBGSTATE_PAUSED:
      ResumeDebugger;
  end;
end;

procedure SuspendDebugger;
begin
end;

procedure TerminateDebugger;
begin
  // Must be running or paused.
  if CoreGet().DebugSession.State = DBGSTATE_INACTIVE then
    Exit;

  if Assigned(DbgThread) then
  begin
    DbgThread.Command := dtc_terminate;
    DbgThread.ContinueEvent.SetEvent; // continue if now paused, to avoid deadlock
    DbgThread.WaitFor;
    FreeAndNil(DbgThread);

    CoreGet.ChangeVA(VaBeforeDbg);
  end;
end;

procedure StepIn;
begin
  // Must be paused.
  if CoreGet().DebugSession.State <> DBGSTATE_PAUSED then
    Exit;

  if Assigned(DbgThread) then
  begin
    DbgThread.Command := dtc_stepin;
    DbgThread.ContinueEvent.SetEvent;
  end;
end;

procedure StepOver;
begin
end;

procedure RunTo(VA: TVA);
begin
  // Must be paused.
  if CoreGet().DebugSession.State <> DBGSTATE_PAUSED then
    Exit;

  if Assigned(DbgThread) then
  begin
    DbgThread.Command := dtc_runto;
    DbgThread.CommandVA := VA;
    DbgThread.ContinueEvent.SetEvent;
  end;
end;

function SetBreakpoint(VA: TVA; Flags: TVDBreakpointFlags): TVDDebugStatus;
var
  BP: TVDBreakPointRecord;
begin
  BP.VA := VA;
  BP.Flags := Flags;
  result := CoreGet.DebugSession.SetBreakpoint(VA, @BP);
end;

procedure ToggleBreakpoint(VA: TVA);
var
  BP: TVDBreakPointRecord;
  Session: IVDDebugSession;
begin
  // Breakpoint can be set/deleted when debugger is active or not.
  // If debugger is active breakpoint is activated immediately (and ready).
  // If debugger is not active breakpoint must be activated when module containing
  // this breakpoint is loaded.
  // Activation is writing $CC for x86 or other way to make breakpoint trigger.

  Session := CoreGet().DebugSession;

  if Session.GetBreakpoint(VA, nil) then
  begin
    Session.DelBreakpoint(VA);
  end
  else
  begin
    BP.VA := VA;
    BP.Flags := TVDBreakpointFlag.BPF_ENABLED;
    Session.SetBreakpoint(VA, @BP);
  end;
end;

procedure LiveCall(VA: TVA);
var
  C: IVDCore;
  Params: string;
begin
  C := CoreGet();
  if C.DebugSession.State <> DBGSTATE_PAUSED then
  begin
    ShowMessage(SDebuggerIsInactive);
    Exit;
  end;
  Params := uFormLiveCall.GetParams;
  C.DebugSession.LiveCall(VA, BSTR_IN(Params));
end;

{ TDebuggerThread }

constructor TDebuggerThread.Create;
begin
  inherited Create(True);
  DebugStartedEvent := TEvent.Create();
  FailureEvent := TEvent.Create();
  ContinueEvent := TEvent.Create();
end;

procedure TDebuggerThread.DbgThreadTerminate(Sender: TObject);
var
  C: IVDCore;
begin
  C := CoreGet;
  C.DebugSession.UnloadDebugger;
  C.DebugSession.SetState(DBGSTATE_INACTIVE);
  C.Msg.Broadcast(MSG_DBG_STOPPED);
end;

destructor TDebuggerThread.Destroy;
begin
  DebugStartedEvent.Free;
  FailureEvent.Free;
  ContinueEvent.Free;
  inherited;
end;

procedure TDebuggerThread.Execute;
const
  DBG_TIMEOUT = 100;
var
  Status: TVDDebugStatus;
  Session: IVDDebugSession;
  Dbg: IVDDebuggerPlugin;
  tmpBP: TVDBreakPointRecord;

  procedure Kill;
  begin
    Dbg.StopProcess;
    while True do
    begin
      if not Dbg.WaitForDebugEvent(-1, FEvent) then
        break;
      if not Dbg.Continue(FEvent.ProcessId, FEvent.ThreadId) then
        break;
      if FEvent.Code = DBG_EVT_EXIT_PROCESS then
        break;
    end;
  end;

var
  bContinue: boolean;

begin
  Session := CoreGet.DebugSession;
  Session.GetDebugger(False, Dbg);

  // First action to do is start new process or attach to existing one.
  if CreateNewProcess then
    Status := Dbg.StartProcess(BSTR_IN(Host), Port, BSTR_IN(FileName), BSTR_IN(WorkingDir), BSTR_IN(Params))
  else
    Status := DBG_ERROR; // attach

  if Status <> DBG_OK then
  begin
    FailureEvent.SetEvent; // failure event
    Exit;
  end;

  DebugStartedEvent.SetEvent; // success event: debugging started

  bContinue := False;
  while (not Terminated) do
  begin
    case Command of
      dtc_terminate:
        begin
          Command := dtc_null;
          Kill;
          break;
        end;
      dtc_stepin:
        begin
          Command := dtc_null;
          Dbg.StepIn;
        end;
      dtc_runto:
        begin
          Command := dtc_null;
          SetBreakpoint(CommandVA, TVDBreakpointFlag.BPF_ENABLED or TVDBreakpointFlag.BPF_TEMP)
        end;
    end;

    // If there are events.
    if Dbg.WaitForDebugEvent(DBG_TIMEOUT, FEvent) then
    begin
      bContinue := True;

      // Call event handler.
      Synchronize(SyncDebugEventHappened_Enter);

      case FEvent.Code of
        DBG_EVT_EXIT_PROCESS:
          begin
            break;
          end;
        DBG_EVT_BREAKPOINT_INITIAL:
          begin
            // Wait for breakpoint to resumed by user if:
            // (bp is initial) and (SkipInitialBreakPoint)
            if not SkipInitialBreakPoint then
            begin
              ContinueEvent.ResetEvent;
              ContinueEvent.WaitFor(INFINITE);
            end;
          end;
        DBG_EVT_BREAKPOINT:
          begin
            // Delete bp if it's temp.
            if Session.GetBreakpoint(FEvent.BreakPoint.VA, @tmpBP) then
            begin
              if (tmpBP.Flags and TVDBreakpointFlag.BPF_TEMP) <> 0 then
                Session.DelBreakpoint(FEvent.BreakPoint.VA);
            end;

            ContinueEvent.ResetEvent;
            ContinueEvent.WaitFor(INFINITE);
          end;
        DBG_EVT_SINGLESTEP:
          begin
            ContinueEvent.ResetEvent;
            ContinueEvent.WaitFor(INFINITE);
          end;
        DBG_EVT_EXCEPTION:
          begin
            // Exception happened wait for user action.
            ContinueEvent.ResetEvent;
            ContinueEvent.WaitFor(INFINITE);
          end;
      end;

      Continue; // to handle dtc_xxx
    end;

    if bContinue then
    begin
      bContinue := False;
      if Dbg.Continue(FEvent.ProcessId, FEvent.ThreadId) then
        Synchronize(SyncDebugEventHappened_Leave);
    end;
  end;
end;

procedure TDebuggerThread.SyncLogEvent(const Event: TVDDebugEvent);
var
  EventName: string;
  Additional: string;
begin
  // Print debug event name
  EventName := GetEnumName(TypeInfo(TVDDebugEventCode), Integer(Event.Code));
  Additional := '';

  case Event.Code of
    DBG_EVT_EXCEPTION:
      Additional := Additional + Format('va:%x code:%x', [Event.Exception.VA, Event.Exception.Code]);
    DBG_EVT_BREAKPOINT:
      Additional := Additional + Format('va:%x', [Event.BreakPoint.VA]);
    DBG_EVT_LOAD_MODULE:
      Additional := Additional + Format('%x "%s"', [Event.LoadModule.Address, Event.ModuleFileName]);
    DBG_EVT_UNLOAD_MODULE:
      Additional := Additional + Format('%x', [Event.UnloadModule.Address]);
    DBG_EVT_CREATE_PROCESS:
      Additional := Additional + Format('"%s"', [Event.ModuleFileName]);
  end;

  CoreGet.Log.WriteLn('[dbg] ' + EventName + ' ' + Additional);
end;

type
  TRecSetBreakpoints = record
    Session: IVDDebugSession;
  end;

  PRecSetBreakpoints = ^TRecSetBreakpoints;

function cbSyncSetBreakpoints(BP: PVDBreakPointRecord; ud: Pointer): BOOL; stdcall;
var
  p: PRecSetBreakpoints;
begin
  p := ud;
  p.Session.ActivateBreakpoint(BP, True);
  result := True;
end;

procedure TDebuggerThread.SyncDebugEventHappened_Enter;
var
  C: IVDCore;
  r_sb: TRecSetBreakpoints;
begin
{$IFDEF DEBUG}
  SyncLogEvent(FEvent);
{$ENDIF}
  C := CoreGet;

  C.DebugSession.SetState(DBGSTATE_PAUSED);

  case FEvent.Code of
    DBG_EVT_CREATE_PROCESS:
      begin
        // Install breakpoints for main module.
        // Currently for whole vm range.
        r_sb.Session := C.DebugSession;
        C.DebugSession.EnumBreakpoints(BAD_VA, BAD_VA, cbSyncSetBreakpoints, @r_sb);
      end;
    DBG_EVT_LOAD_MODULE:
      begin
        // Install breakpoints for module
      end;
    DBG_EVT_UNLOAD_MODULE:
      begin
        // Uninstall breakpoints for module
      end;
    DBG_EVT_BREAKPOINT:
      begin
        // When breakpoint triggered we wait for user command to continue.
        C.ChangeVA(FEvent.Exception.VA, False);
      end;
    DBG_EVT_SINGLESTEP:
      begin
        C.Msg.Broadcast(MSG_DBG_SINGLE_STEP, @self.FEvent);
      end;
    DBG_EVT_EXCEPTION:
      begin
        C.ChangeVA(FEvent.Exception.VA, False);
        InvokeExceptionDialog;
      end;
  end;

end;

procedure TDebuggerThread.SyncDebugEventHappened_Leave;
begin
  CoreGet().DebugSession.SetState(DBGSTATE_RUNNING);
end;

initialization

finalization

// Final cleanup of thread class.
if Assigned(DbgThread) then
  FreeAndNil(DbgThread);

end.
