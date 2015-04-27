unit uDockLayouts;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,

  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  DockAdapter,

  uFrameBaseListView,
  uFrameDisasm,
  uFrameHex,
  uFrameExports,
  uFrameImports,
  uFrameRefs,
  uFrameSections,
  uFrameNames,
  uFrameProblems,
  uFrameBreakpoints,
{$IFDEF ENABLE_DECOMPILATION_FRAME}
  uFrameDecompilation,
{$ENDIF}
  uFrameWatches,
  uFrameLog;

const
  CurrentLayoutName = 'current.layout';

type
  PControl = ^TControl;

  TDockPanelAddInfo = record
    Caption: string;
    IntoZone: TXDockZone;
    Align: TXAlign;
    EmbedCtlCls: TControlClass;
    pOutEmb: PControl;      // out
    pOutPanel: PXDockPanel; // out
  end;

  TDockPanelAddInfoList = TList<TDockPanelAddInfo>;

  TVDDockLayouts = class(TComponent)
  private type
    TCompMap = TDictionary<string, TComponent>;
    TCompMapRev = TDictionary<TComponent, string>;
  private
    FCompMap: TCompMap;
    FCompMapRev: TCompMapRev;
  private
    FAddPanelInfos: TDockPanelAddInfoList;
    procedure QueueCreateDockedPanel(
      const Caption: string;
      IntoZone: TXDockZone;
      Align: TXAlign;
      EmbedCtlCls: TControlClass;
      out OutEmb: TControl;
      out OutPanel: TXDockPanel);

    procedure CreateDockedPanel(const info: TDockPanelAddInfo);
    procedure CreateQueuedDockedPanels;

    procedure MainDockManagerGetComp(Sender: TObject; const AId: string;
      var AComponent: TComponent);

    procedure MainDockManagerGetCompId(Sender: TObject; AComponent: TComponent;
      var AId: string);

    procedure MainDockManagerWriteAppInfo(Sender: TObject; const Xml: IXXmlDocument);
    procedure MainDockManagerReadAppInfo(Sender: TObject; const Xml: IXXmlDocument);

    procedure CreatePanels;
  public
    FrameHex1: TFrameHex;
    FrameExports1: TFrameExports;
    FrameImports1: TFrameImports;
    FrameDisasm1: TFrameDisasm;
    FrameSections1: TFrameSections;
    FrameNames1: TFrameNames;
    FrameProblems1: TFrameProblems;
    FrameBreakpoints1: TFrameBreakpoints;
{$IFDEF ENABLE_DECOMPILATION_FRAME}
    FrameDecompilation1: TFrameDecompilation;
{$ENDIF}
    FrameWatches1: TFrameWatches;
    FrameRefs1: TFrameRefs;
    FrameLog1: TFrameLog;
  public
    pnlHex1: TXDockPanel;
    pnlMsg: TXDockPanel;
    pnlDisAsm: TXDockPanel;
    pnlExports: TXDockPanel;
    pnlImports: TXDockPanel;
    pnlReferences: TXDockPanel;
    pnlProblems: TXDockPanel;
    pnlNames: TXDockPanel;
    pnlSections: TXDockPanel;
    pnlBreakpoints: TXDockPanel;
    pnlDecompiler: TXDockPanel;
    pnlDbgWatches: TXDockPanel;
  public
    MainDockManager: TXDockManager;
    MainDockSite: TXDockSite;
    PageCtrl: TPageControl;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Load/save layout from %local%/layouts dir.
    function LoadLayout(const Name: string = ''): boolean;
    function SaveLayout(const Name: string = ''): boolean;
  end;

  // -----------------------------------------------------------------------------
type
  TViewType = (
    view_first,

    view_disassembler,
{$IFDEF ENABLE_DECOMPILATION_FRAME}
    view_decompiler,
{$ENDIF}
    view_hex,
    view_messages,

    // ListView frames
    view_sections,
    view_imports,
    view_exports,
    view_refs,
    view_names,
    view_problems,

    view_watches,
    view_breakpoints,

    view_last
    );
  TViewTypes = set of TViewType;

const
  AllViewTypes = [succ(view_first) .. Pred(view_last)];

function ViewTypeToControl(vt: TViewType): TControl; inline;

procedure UpdateViews(Flags: TViewTypes; Async: boolean = True);

// Returns control of view type (not parent).
// Visible is applied to parent of control.
function ChangeViewVisible(vt: TViewType; Visible: boolean): TControl; overload; inline;
procedure ChangeViewVisible(Flags: TViewTypes; Visible: boolean); overload;

procedure DoRepaintUiView(View: uint32);
// -----------------------------------------------------------------------------

implementation

uses
  System.IOUtils,
  uFormMain,
  VDAPI;

const
  DefaultLayoutName = 'default.layout';
  LayoutPath        = '%local%\docklayouts\';

function it(const cond: boolean; const atrue, afalse: string): string; overload; inline;
begin
  if cond then
    result := atrue
  else
    result := afalse;
end;

{ TVDDockLayouts }

constructor TVDDockLayouts.Create(AOwner: TComponent);
begin
  inherited;
  FCompMap := TCompMap.Create;
  FCompMapRev := TCompMapRev.Create;
  FAddPanelInfos := TDockPanelAddInfoList.Create;
  CreatePanels;
end;

destructor TVDDockLayouts.Destroy;
begin
  FCompMap.Free;
  FCompMapRev.Free;
  FAddPanelInfos.Free;
  inherited;
end;

procedure TVDDockLayouts.QueueCreateDockedPanel(
  const Caption: string;
  IntoZone: TXDockZone;
  Align: TXAlign;
  EmbedCtlCls: TControlClass;
  out OutEmb: TControl;
  out OutPanel: TXDockPanel);
var
  info: TDockPanelAddInfo;
begin
  info.Caption := Caption;
  info.IntoZone := IntoZone;
  info.Align := Align;
  info.EmbedCtlCls := EmbedCtlCls;
  info.pOutEmb := @OutEmb;
  info.pOutPanel := @OutPanel;

  FAddPanelInfos.Add(info);
end;

procedure TVDDockLayouts.CreateDockedPanel(const info: TDockPanelAddInfo);
var
  Zone: TXDockZone;
  Site: TXDockSite;
  tab: TTabSheet;
begin
  // create page control if not yet created
  if not assigned(Self.PageCtrl) then
  begin
    Self.PageCtrl := TPageControl.Create(FormMain);
    Self.PageCtrl.Parent := FormMain;
    Self.PageCtrl.Align := alClient;
  end;

  // new tab
  tab := TTabSheet.Create(Self.PageCtrl);
  tab.PageControl := Self.PageCtrl;
  tab.Caption := info.Caption;

  info.pOutPanel^ := TXDockPanel.Create(tab);
  info.pOutPanel^.Parent := tab;
  info.pOutPanel^.Align := alClient;

  if info.pOutPanel^.Width < 300 then
    info.pOutPanel^.Width := 300;
  if info.pOutPanel^.Height < 100 then
    info.pOutPanel^.Height := 100;

  if assigned(info.pOutPanel^.Site) then
    Site := info.pOutPanel^.Site
  else
    Site := MainDockSite;

  FCompMap.Add(info.Caption, info.pOutPanel^);
  FCompMapRev.Add(info.pOutPanel^, info.Caption);

  Zone := info.IntoZone;
  Site.DockControl(info.pOutPanel^, Zone, info.Align);

  info.pOutEmb^ := info.EmbedCtlCls.Create(Self);
  info.pOutEmb^.Parent := info.pOutPanel^;
  info.pOutEmb^.Align := alClient;
end;

procedure TVDDockLayouts.CreateQueuedDockedPanels;
var
  info: TDockPanelAddInfo;
begin
  FAddPanelInfos.Reverse;
  try
    for info in FAddPanelInfos do
      CreateDockedPanel(info);
  finally
    FAddPanelInfos.Clear;
  end;
end;

procedure TVDDockLayouts.CreatePanels;
begin
  // ---------------------------------------------------------------------------
  MainDockManager := TXDockManager.Create(Self);
  MainDockManager.OnGetComp := MainDockManagerGetComp;
  MainDockManager.OnGetCompId := MainDockManagerGetCompId;
  MainDockManager.OnWriteAppInfo := MainDockManagerWriteAppInfo;
  MainDockManager.OnReadAppInfo := MainDockManagerReadAppInfo;
  // ---------------------------------------------------------------------------
  MainDockSite := TXDockSite.Create(Self);
  MainDockSite.Parent := FormMain;
  MainDockSite.Align := alClient;
  MainDockSite.Manager := MainDockManager;
  MainDockSite.LoadDesignLayout := false;
  FCompMap.Add('mainsite', MainDockSite);
  FCompMapRev.Add(MainDockSite, 'mainsite');
  // ---------------------------------------------------------------------------
  // In reverse order.
{$IFDEF ENABLE_DECOMPILATION_FRAME}
  QueueCreateDockedPanel('Decompiler', nil, alClient, TFrameDecompilation, TControl(FrameDecompilation1), pnlDecompiler);
{$ENDIF}
  QueueCreateDockedPanel('Watches', nil, alClient, TFrameWatches, TControl(FrameWatches1), pnlDbgWatches);
  QueueCreateDockedPanel('Breakpoints', nil, alClient, TFrameBreakpoints, TControl(FrameBreakpoints1), pnlBreakpoints);
  QueueCreateDockedPanel('Problems', nil, alClient, TFrameProblems, TControl(FrameProblems1), pnlProblems);
  QueueCreateDockedPanel('References', nil, alClient, TFrameRefs, TControl(FrameRefs1), pnlReferences);
  QueueCreateDockedPanel('Imports', nil, alClient, TFrameImports, TControl(FrameImports1), pnlImports);
  QueueCreateDockedPanel('Exports', nil, alClient, TFrameExports, TControl(FrameExports1), pnlExports);
  QueueCreateDockedPanel('Names', nil, alClient, TFrameNames, TControl(FrameNames1), pnlNames);
  QueueCreateDockedPanel('Sections', nil, alClient, TFrameSections, TControl(FrameSections1), pnlSections);
  QueueCreateDockedPanel('Messages', nil, alClient, TFrameLog, TControl(FrameLog1), pnlMsg);
  QueueCreateDockedPanel('Hex', nil, alClient, TFrameHex, TControl(FrameHex1), pnlHex1);
  QueueCreateDockedPanel('Disassembler', nil, alClient, TFrameDisasm, TControl(FrameDisasm1), pnlDisAsm);
  // ---------------------------------------------------------------------------
  CreateQueuedDockedPanels;
end;

procedure TVDDockLayouts.MainDockManagerGetComp(Sender: TObject;
  const AId: string; var AComponent: TComponent);
begin
  if not FCompMap.TryGetValue(AId, AComponent) then
    raise Exception.CreateFmt('Dock system failed to find "%s"', [AId]);
end;

procedure TVDDockLayouts.MainDockManagerGetCompId(Sender: TObject;
  AComponent: TComponent; var AId: string);
begin
  if not FCompMapRev.TryGetValue(AComponent, AId) then
  begin
    // Don't have to raise exception because:
    // we return only known component id; other such as floating helper panels
    // must be created with default parameters, and don't need special handling
    // from here.
  end;
end;

procedure TVDDockLayouts.MainDockManagerReadAppInfo(Sender: TObject;
  const Xml: IXXmlDocument);
// var
// elem: IXXmlElement;
begin
  asm
    int 3
  end;

  (*
    elem := Xml.DocumentElement.FindElement('application', '', Null);
    if elem <> nil then
    begin
    { Load main form bounds }

    FormMain.SetBounds(elem.GetIntAttr('left', FormMain.Left),
    elem.GetIntAttr('top', FormMain.Top),
    elem.GetIntAttr('width', FormMain.Width),
    elem.GetIntAttr('height', FormMain.Height));

    FrameDisasm1.DisasmText.FontSize := elem.GetIntAttr('disasm_font_size', 1);
    end;
  *)
end;

procedure TVDDockLayouts.MainDockManagerWriteAppInfo(Sender: TObject;
  const Xml: IXXmlDocument);
// var
// elem: IXXmlElement;
begin
  asm
    int 3
  end;
  (*
    elem := Xml.DocumentElement.AppendElement('application');

    { Save main form bounds }

    elem.SetIntAttr('left', FormMain.Left);
    elem.SetIntAttr('top', FormMain.Top);
    elem.SetIntAttr('width', FormMain.Width);
    elem.SetIntAttr('height', FormMain.Height);
    elem.SetIntAttr('disasm_font_size', FrameDisasm1.DisasmText.FontSize);
  *)
end;

function TVDDockLayouts.LoadLayout(const Name: string): boolean;
var
  filename: string;
begin
  result := false;
  filename := IOGet.ExpandPath(LayoutPath + it(name <> '', name, DefaultLayoutName));
  if TFile.Exists(filename) then
  begin
    MainDockManager.LoadFromFile(filename);
    result := True;
  end;
end;

function TVDDockLayouts.SaveLayout(const Name: string): boolean;
var
  path: string;
  filename: string;
begin
  path := IOGet.ExpandPath(LayoutPath);
  filename := path + it(name <> '', name, DefaultLayoutName);
  TDirectory.CreateDirectory(path);
  MainDockManager.SaveToFile(filename);
  result := True;
end;

procedure UpdateViewInt(const Frame: TFrameBaseListView; Async: boolean); inline;
begin
  if Async then
    Frame.UpdateUIAsync
  else
    Frame.UpdateUISync;
end;

procedure ShowCtrl(const ctl: TControl; Visible: boolean); inline;
begin
  if Visible then
    ctl.Show
  else
    ctl.Hide;
end;

function ViewTypeToControl(vt: TViewType): TControl;
var
  root: TVDDockLayouts;
begin
  root := FormMain.DockLayouts;

  case vt of
    view_disassembler:
      result := root.FrameDisasm1;
{$IFDEF ENABLE_DECOMPILATION_FRAME}
    view_decompiler:
      result := root.FrameDecompilation1;
{$ENDIF}
    view_hex:
      result := root.FrameHex1;
    view_messages:
      result := root.FrameLog1;

    view_sections:
      result := root.FrameSections1;
    view_imports:
      result := root.FrameImports1;
    view_exports:
      result := root.FrameExports1;
    view_refs:
      result := root.FrameRefs1;
    view_names:
      result := root.FrameNames1;
    view_problems:
      result := root.FrameProblems1;

    view_watches:
      result := root.FrameWatches1;
    view_breakpoints:
      result := root.FrameBreakpoints1;
  else
    result := nil;
  end;
end;

procedure UpdateViews(Flags: TViewTypes; Async: boolean);
var
  vt: TViewType;
  ctl: TControl;
begin
  for vt in Flags do
  begin
    ctl := ViewTypeToControl(vt);

    if ctl = nil then
      raise Exception.Create('View not found')
    else if ctl is TFrameBaseListView then
      UpdateViewInt(ctl as TFrameBaseListView, Async)
    else if ctl is TFrameWatches then
      TFrameWatches(ctl).UpdateWatches
    else
    begin
      // raise Exception.Create('Unhandled view');
    end;
  end;
end;

function ChangeViewVisible(vt: TViewType; Visible: boolean): TControl;
begin
  result := ViewTypeToControl(vt);
  ShowCtrl(result.Parent, Visible);
end;

procedure ChangeViewVisible(Flags: TViewTypes; Visible: boolean);
var
  vt: TViewType;
begin
  for vt in Flags do
    ChangeViewVisible(vt, Visible);
end;

procedure DoRepaintUiView(View: uint32);
begin
  if (View and TUIViewType.UIVT_DISASM) <> 0 then
    FormMain.DockLayouts.FrameDisasm1.DisasmText.InvalidateAllRowsWithRebuilding;
end;

end.
