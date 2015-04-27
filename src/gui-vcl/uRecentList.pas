unit uRecentList;

interface

uses
  System.Classes,
  System.IOUtils,
  System.UITypes,
  System.SysUtils,

  Vcl.Menus,

  VDAPI;

procedure RecentListInit;
procedure RecentListAdd(const Path: string);
function RecentListGetFileName(Id: integer): string;

implementation

uses
  uFormMain;

const
  RecentPath     = '%local%\recent.txt';
  MaxRecentItems = 20;

var
  List: TStringList;
  ListLoaded: boolean;

  // InitialLoadAllItems: if true it's initial loading of recent list.
  // Otherwise new Path is added.
procedure RecentListUpdate(const Path: string; InitialLoadAllItems: boolean);
var
  mi: TMenuItem;
  recentList: string;
  Id: integer;

  procedure DeleteOldMenuItems;
  begin
{$IFDEF APPEND_RECENT_TO_FILE_MENU}
    while True do
    begin
      Id := FormMain.MenuFile.Count - 1;
      mi := FormMain.MenuFile.Items[Id];
      if mi = FormMain.nBeforeRecentList then
        break;
      FormMain.MenuFile.Delete(Id);
    end;
{$ELSE}
    FormMain.Recent1.Clear;
{$ENDIF}
  end;

  procedure AddMenuItems;
  var
    fn: string;
  begin
    Id := 0;
    for fn in List do
    begin
      mi := TMenuItem.Create(FormMain);
      mi.Caption := fn;
      mi.Enabled := TFile.Exists(fn) or TDirectory.Exists(fn);
      mi.Tag := Id;
      mi.OnClick := FormMain.RecentItemClick;
{$IFDEF APPEND_RECENT_TO_FILE_MENU}
      FormMain.MenuFile.Add(mi);
{$ELSE}
      FormMain.Recent1.Add(mi);
{$ENDIF}
      inc(Id);
    end;
  end;

begin
  if (Path = '') and (not InitialLoadAllItems) then
    exit;

  recentList := IOGet.ExpandPath(RecentPath);

  if (not ListLoaded) and (TFile.Exists(recentList)) then
  begin
    ListLoaded := True;
    List.LoadFromFile(recentList);
  end;

  // If adding item... (when not 1st loading)
  if not InitialLoadAllItems then
  begin
    // If path already in list move it to top. Otherwise just insert new path.
    Id := List.IndexOf(Path);
    if Id <> -1 then
      List.Delete(Id);
    List.Insert(0, Path)
  end;

  // Trim list.
  while List.Count > MaxRecentItems do
    List.Delete(List.Count - 1);

  if not InitialLoadAllItems then
    List.SaveToFile(recentList);

  DeleteOldMenuItems;
  AddMenuItems;
end;

procedure RecentListInit;
begin
  RecentListUpdate('', True);
end;

procedure RecentListAdd(const Path: string);
begin
  RecentListUpdate(Path, False);
end;

function RecentListGetFileName(Id: integer): string;
begin
  Result := List[Id];
end;

initialization

List := TStringList.Create;

finalization

FreeAndNil(List);

end.
