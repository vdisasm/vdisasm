unit uMenu;

interface

uses
  VDAPI;

type
  TVDMenu = class(TInterfacedObject, IVDMenu)
  public
    function Add(OnClickProc: TVDMenuOnClickProc; Path: BSTR; ShortCut: BSTR = ''): TVDMenuItemHandle; stdcall;
    procedure Remove(Handle: TVDMenuItemHandle); stdcall;
  end;

implementation

uses
  uCore;

{ TVDMenu }

function TVDMenu.Add(OnClickProc: TVDMenuOnClickProc; Path,
  ShortCut: BSTR): TVDMenuItemHandle;
var
  param: TVDMenuItemMsgParam;
begin
  param.OnClickProc := OnClickProc;
  param.Path := Path;
  param.ShortCut := ShortCut;
  param.Result := nil;
  CoreGet.Msg.Broadcast(TVDMessage.MSG_MENU_ADD, @param);
  Result := param.Result;
end;

procedure TVDMenu.Remove(Handle: TVDMenuItemHandle);
begin
  if Assigned(Handle) then
    CoreGet.Msg.Broadcast(TVDMessage.MSG_MENU_REMOVE, Handle);
end;

end.
