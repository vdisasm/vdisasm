{
  *
  * Message manager.
  *
  * Implements adding, removing listeners and sending messages.
  *
}
unit uMsg;

interface

uses
  System.Generics.Collections,
  VDAPI;

type
  TListenerDic = TDictionary<TVDMessageListenerProc, integer>;

  TVDMsgMgr = class(TInterfacedObject, IVDMessageManager)
  private
    FListeners: TListenerDic;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListenerProc(Proc: TVDMessageListenerProc); stdcall;
    procedure RemoveListenerProc(Proc: TVDMessageListenerProc); stdcall;
    procedure Broadcast(Msg: TVDMessage; Param: Pointer); stdcall;
  end;

implementation

uses
  uDebugSession;

{ TVDMsgMgr }

procedure TVDMsgMgr.AddListenerProc(Proc: TVDMessageListenerProc);
begin
  if Assigned(Proc) then
  begin
    // If it's first listener added we have to notify of core was
    // initialized.
    if FListeners.Count = 0 then
      Proc(TVDMessage.MSG_CORE_INIT, nil);
    // Register listener.
    if not FListeners.ContainsKey(Proc) then
      FListeners.Add(Proc, 0);
  end;
end;

procedure TVDMsgMgr.RemoveListenerProc(Proc: TVDMessageListenerProc);
begin
  if Assigned(Proc) then
    if FListeners.ContainsKey(Proc) then
      FListeners.Remove(Proc);
end;

procedure TVDMsgMgr.Broadcast(Msg: TVDMessage; Param: Pointer);
var
  ListenerProc: TVDMessageListenerProc;
begin
  // Implicit msg handlers.
  case Msg of
    MSG_DBG_STOPPED:
      (CoreGet.DebugSession as TVDDebugSession).on_MSG_DBG_STOPPED;
end;

for ListenerProc in FListeners.Keys do
  ListenerProc(Msg, Param);
end;

constructor TVDMsgMgr.Create;
begin
  FListeners := TListenerDic.Create;
end;

destructor TVDMsgMgr.Destroy;
begin
  FListeners.Free;
  inherited;
end;

end.
