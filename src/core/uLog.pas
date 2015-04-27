unit uLog;

interface

uses
  uUpdateable,
  VDAPI;

type
  TVDLog = class(TVDUpdateable, IVDLog)
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
  public
    procedure Clear; stdcall;
    procedure Write(Text: BSTR); stdcall;
    procedure WriteLn(Text: BSTR); stdcall;
  end;

implementation

uses
  uCore;

{ TVDLog }

procedure TVDLog.Clear;
begin
  CoreGet.Msg.Broadcast(TVDMessage.MSG_LOG_CLEAR);
end;

procedure TVDLog.DoBeginUpdate;
begin
  CoreGet.Msg.Broadcast(TVDMessage.MSG_LOG_BEGIN_UPDATE);
end;

procedure TVDLog.DoEndUpdate;
begin
  CoreGet.Msg.Broadcast(TVDMessage.MSG_LOG_END_UPDATE);
end;

procedure TVDLog.Write(Text: BSTR);
begin
  if Text <> '' then
    CoreGet.Msg.Broadcast(TVDMessage.MSG_LOG_TEXT, Pointer(Text));
end;

procedure TVDLog.WriteLn(Text: BSTR);
var
  NewText: BSTR;
begin
  NewText := Text + sLineBreak;
  CoreGet.Msg.Broadcast(TVDMessage.MSG_LOG_TEXT, Pointer(NewText));
end;

end.
