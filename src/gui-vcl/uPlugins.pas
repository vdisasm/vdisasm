unit uPlugins;

interface

uses
  VCL.Menus,
  VDAPI;

type
  TGuiPluginType = (gpt_plugin, gpt_debugger);

procedure RescanPlugins(RootMenu: TMenuItem; PluginPath: BSTR_IN; PlgType: TGuiPluginType);
procedure PluginItemClick(Item: TMenuItem);

implementation

uses
  System.Classes,
  System.SyncObjs,
  uFormMain,
  uDebugger;

type
  TCtx = record
    RootMenu: TMenuItem;
    SL: TStringList;
    PlgType: TGuiPluginType;
    procedure Clear;
  end;

  PCtx = ^TCtx;

var
  contexts: array [TGuiPluginType] of TCtx;

  // Called from FormMain.PluginItemClick
procedure PluginItemClick(Item: TMenuItem);
var
  Ctx: PCtx;
  Index: integer;
  PlgPath: string;
begin
  Ctx := PCtx(Item.Tag);
  Index := Ctx.RootMenu.IndexOf(Item);
  PlgPath := Ctx.SL[index];
  case Ctx.PlgType of
    gpt_plugin:
      CoreGet.PluginMgr.RunPlugin(PlgPath);
    gpt_debugger:
      Item.Checked := ChooseDebugger(PlgPath);
  end;
end;

// Adding items to Plugin menu.
function Callback(FileName: BSTR_IN; Plugin: IUnknown; Info: PVDPluginInfo; ud: pointer): BOOL; stdcall;
var
  Item: TMenuItem;
  Ctx: PCtx;
begin
  Ctx := PCtx(ud);

  Item := TMenuItem.Create(FormMain.MenuPlugins);
  Ctx.SL.Add(FileName);

  Item.Caption := Info.Name;
  Item.Tag := NativeInt(Ctx);
  Item.OnClick := FormMain.PluginItemClick;

  // If there is at least 1 item, make whole menu visible.
  if not Ctx.RootMenu.Visible then
    Ctx.RootMenu.Visible := True;

  Ctx.RootMenu.Add(Item);

  // Post process item
  case Ctx.PlgType of
    gpt_debugger:
      if Ctx.RootMenu.Count = 1 then
      begin
        Item.OnClick(Item);
      end;
  end;

  Result := True;
end;

var
  RescanLock: TCriticalSection;

procedure RescanPlugins(RootMenu: TMenuItem; PluginPath: BSTR_IN; PlgType: TGuiPluginType);
var
  c: IVDCore;
begin
  c := CoreGet;
  if not Assigned(c) then
    exit;

  RescanLock.Enter;
  try
    RootMenu.Visible := False;
    RootMenu.Clear;

    contexts[PlgType].Clear;
    contexts[PlgType].RootMenu := RootMenu;
    contexts[PlgType].PlgType := PlgType;

    c.PluginMgr.ScanPluginDir(PluginPath, Callback, 0, @contexts[PlgType]);
  finally
    RescanLock.Leave;
  end;
end;

{ TCtx }

procedure TCtx.Clear;
begin
  SL.Clear;
  RootMenu := nil;
end;

procedure PlgInitFinit(init: boolean);
var
  PlgType: TGuiPluginType;
begin
  if init then
  begin
    RescanLock := TCriticalSection.Create;
    for PlgType := low(TGuiPluginType) to high(TGuiPluginType) do
    begin
      contexts[PlgType].SL := TStringList.Create;
      contexts[PlgType].Clear;
    end;
  end
  else
  begin
    RescanLock.Free;

    for PlgType := low(TGuiPluginType) to high(TGuiPluginType) do
    begin
      contexts[PlgType].Clear;
      contexts[PlgType].SL.Free;
      contexts[PlgType].SL := nil;
    end;
  end;
end;

initialization

PlgInitFinit(True);

finalization

PlgInitFinit(False);

end.
