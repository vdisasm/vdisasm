unit uFormIcons;

interface

procedure AssignFormIcons;

implementation

uses
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.Menus,

  WinApi.Windows,

  DockAdapter,

  uFormMain,
  uFormFindBytes,
  uFormCalc,
  uFormOptions,

  VDAPI;

var
  IconPath: string;

{$DEFINE ICON_FROM_FILE}


procedure FetchIcon(
  ImageList: TImageList;
  mi: TMenuItem;
  const name: string;
  Form: TForm = nil;
  Pnl: TXDockPanel = nil
  );
var
{$IFDEF ICON_FROM_FILE}
  fn: string;
{$ENDIF}
  icn: TIcon;
  index: integer;
begin
{$IFDEF ICON_FROM_FILE}
  if IconPath = '' then
    IconPath := IOGet.ExpandPath('%rootdir%\icons\');
  fn := IconPath + name + '.ico';
{$ENDIF}
  icn := TIcon.Create;
  try
{$IFDEF ICON_FROM_FILE}
    icn.LoadFromFile(fn);
{$ELSE}
    icn.LoadFromResourceName(HInstance, name);
{$ENDIF}
    index := ImageList.AddIcon(icn);

    mi.ImageIndex := index;

    if Assigned(Form) then
      Form.Icon := icn;

    if Assigned(Pnl) then
    begin
      Pnl.Images := ImageList;
      Pnl.ImageIndex := index;
      Pnl.ShowHeaderImage := true;
    end;
  finally
    icn.Free;
  end;
end;

procedure AssignFormIcons;
const
  s_exports = 'table_row_delete';
  s_names   = 'tag_blue';
var
  img: TImageList;
begin
  img := TImageList.Create(FormMain);
  img.Masked := False;
  img.ColorDepth := cd32Bit;

  FormMain.ImgList := img;

  FormMain.MainMenu1.Images := img;
  FormMain.DockLayouts.FrameDisasm1.ppDis.Images := img;

  FormMain.ImgList.BeginUpdate;
  try
    // File
    FetchIcon(img, FormMain.Open1, 'folder_page');
    FetchIcon(img, FormMain.Deletedatabse1, 'database_delete');
    FetchIcon(img, FormMain.Savedatabase1, 'database_save');
    FetchIcon(img, FormMain.Export1, 'database_go');
    FetchIcon(img, FormMain.Zipdatabase1, 'compress');

    // Search
    FetchIcon(img, FormMain.MenuSearch_BytesOrStrings, 'find', FormFindBytes);

    // View
    // -- calc
    FetchIcon(img, FormMain.ViewCalc1, 'calculator', FormCalc);
    FetchIcon(img, FormMain.ViewOptions, 'wrench', FormOptions);
    FetchIcon(img, FormMain.ViewExports, s_exports, nil, FormMain.DockLayouts.pnlExports);
    FetchIcon(img, FormMain.ViewImports, 'table_row_insert', nil, FormMain.DockLayouts.pnlImports);
    FetchIcon(img, FormMain.ViewNames, s_names, nil, FormMain.DockLayouts.pnlNames);
    FetchIcon(img, FormMain.ViewProblems, 'error', nil, FormMain.DockLayouts.pnlProblems);
    FetchIcon(img, FormMain.ViewReferences, 'arrow_switch', nil, FormMain.DockLayouts.pnlReferences);
    FetchIcon(img, FormMain.LayoutLoadDefault, 'layout');
    FetchIcon(img, FormMain.LayoutSaveDefault, 'layout_edit');

    // Debugger
    FetchIcon(img, FormMain.DbgChoose, 'bug');
    FetchIcon(img, FormMain.DbgStartResume, 'control_play_blue');
    FetchIcon(img, FormMain.DbgSuspend, 'control_pause_blue');
    FetchIcon(img, FormMain.DbgStepInto, 'control_fastforward_blue');
    FetchIcon(img, FormMain.DbgStepOver, 'control_repeat_blue');
    FetchIcon(img, FormMain.DbgRunToCursor, 'control_end_blue');
    FetchIcon(img, FormMain.DbgLiveCall, 'telephone');
    FetchIcon(img, FormMain.DbgTerminate, 'control_stop_blue');

    // Help
    // FetchIcon(img, FormMain.OnlineHelp, 'help');
    FetchIcon(img, FormMain.About1, 'information');

    // Disasm menu
    FetchIcon(img, FormMain.DockLayouts.FrameDisasm1.DefineComment, 'comment');
    FetchIcon(img, FormMain.DockLayouts.FrameDisasm1.DefineName, s_names);
    FetchIcon(img, FormMain.DockLayouts.FrameDisasm1.DefineExport, s_exports);
    FetchIcon(img, FormMain.DockLayouts.FrameDisasm1.Go1, 'bullet_go');
    FetchIcon(img, FormMain.DockLayouts.FrameDisasm1.VMStart1, 'bullet_arrow_top');
    FetchIcon(img, FormMain.DockLayouts.FrameDisasm1.VMEnd1, 'bullet_arrow_bottom');
  finally
    FormMain.ImgList.EndUpdate;
  end;
end;

end.
