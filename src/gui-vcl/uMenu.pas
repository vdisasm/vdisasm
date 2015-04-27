{
  *
  * Implementation of VCL menu item handling.
  *
}
unit uMenu;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  VCL.Menus,
  VDAPI;

type
  TItemsDic = TDictionary<TMenuItem, TVDMenuOnClickProc>;

  TMenuMgr = class
  private
    FRootMenu: TMenu;
    FItems: TItemsDic;
  protected
    procedure DefaultClickProc(Sender: TObject);
    procedure ItemKeyNotification(Sender: TObject; const Item: TMenuItem;
      Action: TCollectionNotification);
  protected
    // Find subitem by caption.
    function Find(Item: TMenuItem; Caption: string): TMenuItem;
  public
    constructor Create(RootMenu: TMenu);
    destructor Destroy; override;

    // Add menu item.
    procedure Add(Params: PVDMenuItemMsgParam);
    // Remove menu item.
    procedure Remove(Item: TVDMenuItemHandle);
  end;

implementation

{ TMenuMgr }

constructor TMenuMgr.Create(RootMenu: TMenu);
begin
  FRootMenu := RootMenu;
  FItems := TItemsDic.Create;
  FItems.OnKeyNotify := ItemKeyNotification;
end;

destructor TMenuMgr.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TMenuMgr.DefaultClickProc(Sender: TObject);
var
  Item: TMenuItem;
  Proc: TVDMenuOnClickProc;
begin
  Item := Sender as TMenuItem;
  if FItems.TryGetValue(Item, Proc) then
    Proc;
end;

procedure TMenuMgr.Add(Params: PVDMenuItemMsgParam);
var
  sl: TStringList;
  s, s1: string;
  i: integer;
  itm, newItm: TMenuItem;
begin
  if not Assigned(Params) then
    exit;

  Params^.Result := nil;

  if not Assigned(Params^.OnClickProc) then
    exit;

  sl := TStringList.Create;
  try
    // Split path into lines.
    s := IncludeTrailingPathDelimiter(Params^.Path);
    s1 := '';
    for i := 1 to length(s) do
    begin
      if CharInSet(s[i], ['\', '/']) then
      begin
        sl.Add(s1);
        s1 := '';
      end
      else
        s1 := s1 + s[i];
    end;

    // Add menu item.
    itm := FRootMenu.Items;
    for i := 0 to sl.Count - 1 do
    begin
      newItm := Find(itm, sl[i]);
      if newItm = nil then
      begin
        newItm := TMenuItem.Create(FRootMenu);
        newItm.Caption := sl[i];
        // when we reached last node
        if (i = (sl.Count - 1)) then
        begin
          // add OnClick for dest(last) node.
          newItm.OnClick := DefaultClickProc;
          if Params^.ShortCut <> '' then
            newItm.ShortCut := TextToShortCut(Params^.ShortCut);
          FItems.Add(newItm, Params^.OnClickProc);
          Params^.Result := newItm;
        end;
        itm.Add(newItm);
      end;
      itm := newItm;
    end;
  finally
    sl.Free;
  end;
end;

function TMenuMgr.Find(Item: TMenuItem; Caption: string): TMenuItem;
var
  s, v: string;
begin
  s := LowerCase(Caption);
  for Result in Item do
  begin
    v := LowerCase(Result.Caption);
    v := StringReplace(v, '&', '', [rfReplaceAll, rfIgnoreCase]);
    if s = v then
      exit;
  end;
  exit(nil);
end;

procedure TMenuMgr.ItemKeyNotification(Sender: TObject; const Item: TMenuItem;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

procedure TMenuMgr.Remove(Item: TVDMenuItemHandle);
begin
  if Item <> nil then
    if FItems.ContainsKey(Item) then
      FItems.Remove(Item);
end;

end.
