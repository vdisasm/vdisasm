unit uFormMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ImgList,

  VDLib.Platform,

  uFormIcons,

  uCore.Strings,
{$IFDEF DEBUG}
  InternalDebugSdk,
{$ENDIF}
  VDAPI,

  uDockLayouts,

  uColorText,       // VclColor
  uColorText.Types, // expand
  uDisasmText;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    Open1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Deletedatabse1: TMenuItem;
    Savedatabase1: TMenuItem;
    nBeforeRecentList: TMenuItem;
    MenuHelp: TMenuItem;
    About1: TMenuItem;
    N3: TMenuItem;
    MenuPlugins: TMenuItem;
    MenuSearch: TMenuItem;
    MenuSearch_BytesOrStrings: TMenuItem;
    MenuView: TMenuItem;
    ViewSections: TMenuItem;
    PopupMenuLog: TPopupMenu;
    Clearall1: TMenuItem;
    Copyall1: TMenuItem;
    Copy1: TMenuItem;
    N4: TMenuItem;
    Export1: TMenuItem;
    Export_DatabaseText: TMenuItem;
    Edit1: TMenuItem;
    TypeLibBrowser1: TMenuItem;
    ViewHex: TMenuItem;
    N5: TMenuItem;
    NewType1: TMenuItem;
    ClearJumpHistory: TMenuItem;
    Cleanupdatabase1: TMenuItem;
    ViewNames: TMenuItem;
    Zipdatabase1: TMenuItem;
    N7: TMenuItem;
    ViewExports: TMenuItem;
    ViewImports: TMenuItem;
    MenuDebug: TMenuItem;
    ViewProblems: TMenuItem;
    Export_ExportedSymbols: TMenuItem;
    DbgChoose: TMenuItem;
    N8: TMenuItem;
    DbgSuspend: TMenuItem;
    DbgStartResume: TMenuItem;
    N9: TMenuItem;
    DbgTerminate: TMenuItem;
    DbgStepInto: TMenuItem;
    DbgStepOver: TMenuItem;
    InternalDebug1: TMenuItem;
    idbg_LogLoadedPlugins: TMenuItem;
    idbg_LogTypePluginMapping: TMenuItem;
    ViewDisasm: TMenuItem;
    ViewMessages: TMenuItem;
    ViewReferences: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    LayoutLoadDefault: TMenuItem;
    LayoutSaveDefault: TMenuItem;
    TimerUpdateFrames: TTimer;
    N2: TMenuItem;
    ViewOptions: TMenuItem;
    Recent1: TMenuItem;
    ViewDecompiler: TMenuItem;
    StatusBarMain: TStatusBar;
    DbgRunToCursor: TMenuItem;
    N13: TMenuItem;
    DbgLiveCall: TMenuItem;
    N14: TMenuItem;
    Breakpoints1: TMenuItem;
    Watches1: TMenuItem;
    N15: TMenuItem;
    ViewCalc1: TMenuItem;
    MenuSearch_StringScan: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Deletedatabase1Click(Sender: TObject);
    procedure Savedatabase1Click(Sender: TObject);
    procedure MenuSearch_BytesOrStringsClick(Sender: TObject);
    procedure ViewSectionsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Export_DatabaseTextClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure TypeLibBrowser1Click(Sender: TObject);
    procedure ViewHexClick(Sender: TObject);
    procedure NewType1Click(Sender: TObject);
    procedure ClearJumpHistoryClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Cleanupdatabase1Click(Sender: TObject);
    procedure ViewNamesClick(Sender: TObject);
    procedure Zipdatabase1Click(Sender: TObject);
    procedure ViewExportsClick(Sender: TObject);
    procedure ViewImportsClick(Sender: TObject);
    procedure ViewProblemsClick(Sender: TObject);
    procedure Export_ExportedSymbolsClick(Sender: TObject);
    procedure DbgStartResumeClick(Sender: TObject);
    procedure DbgSuspendClick(Sender: TObject);
    procedure DbgTerminateClick(Sender: TObject);
    procedure idbg_LogLoadedPluginsClick(Sender: TObject);
    procedure idbg_LogTypePluginMappingClick(Sender: TObject);
    procedure ViewDisasmClick(Sender: TObject);
    procedure ViewMessagesClick(Sender: TObject);
    procedure ViewReferencesClick(Sender: TObject);
    procedure LayoutLoadDefaultClick(Sender: TObject);
    procedure LayoutSaveDefaultClick(Sender: TObject);
    procedure TimerUpdateFramesTimer(Sender: TObject);
    procedure ViewOptionsClick(Sender: TObject);
    procedure ViewDecompilerClick(Sender: TObject);
    procedure DbgStepIntoClick(Sender: TObject);
    procedure DbgStepOverClick(Sender: TObject);
    procedure DbgRunToCursorClick(Sender: TObject);
    procedure DbgLiveCallClick(Sender: TObject);
    procedure Watches1Click(Sender: TObject);
    procedure Breakpoints1Click(Sender: TObject);
    procedure ViewCalc1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuSearch_StringScanClick(Sender: TObject);
  private
    FInitializedOnceOnActivate: Boolean;
  public
    DockLayouts: TVDDockLayouts;
  protected
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  public
    procedure PluginItemClick(Sender: TObject);
    procedure RecentItemClick(Sender: TObject);
  public
    ImgList: TImageList;
    procedure DBOpen(const Path: string);
    procedure DBCloseCurrent(DeleteDatabase: Boolean);
    procedure DBSave;
  end;

var
  FormMain: TFormMain;

implementation

uses
  uChooseFileName,
  uStrings,
  uListenerMain,
  uPlugins,

  uFormFindBytes,
  uFormAbout,
  uFormTypeLibBrowser,
  uFormExportDump,
  uFormTypeEditor,

  uRecentList,

  uDebugger,

  uFormOptions,
  uFormCalc,
  uFormStringScan;

{$R *.dfm}


procedure TFormMain.About1Click(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormMain.Breakpoints1Click(Sender: TObject);
begin
  ChangeViewVisible([view_breakpoints], True);
end;

procedure TFormMain.Cleanupdatabase1Click(Sender: TObject);
begin
  CoreGet().CleanDatabase();
end;

procedure TFormMain.ClearJumpHistoryClick(Sender: TObject);
begin
  CoreGet().NavigateListClear;
end;

procedure TFormMain.Close1Click(Sender: TObject);
begin
  DBCloseCurrent(False);
end;

procedure TFormMain.Export_DatabaseTextClick(Sender: TObject);
var
  c: IVDCore;
  va0, va1: TVA;
begin
  c := CoreGet;
  if c.IsDatabaseOpened and c.UI.GetSelectedRange(va0, va1, TUISelRangeFlags.DEFAULT_SELECT_ALL_VA) then
    uFormExportDump.Invoke(va0, va1);
end;

procedure TFormMain.DBCloseCurrent(DeleteDatabase: Boolean);
begin
  CoreGet.CloseDatabase(DeleteDatabase);
  Caption := '';
end;

procedure TFormMain.DBOpen(const Path: string);
var
  c: IVDCore;
  fn: string;
begin
  if Path <> '' then
  begin
    c := CoreGet();
    if c.OpenPath(Path) then
    begin
      RecentListAdd(Path);
      fn := c.InputFile.FileName;
      Caption := ExtractFileName(fn);
    end;
  end;
end;

procedure TFormMain.DBSave;
begin
  CoreGet.SaveDatabase;
end;

procedure TFormMain.LayoutLoadDefaultClick(Sender: TObject);
begin
  DockLayouts.LoadLayout(''); // '' means default
end;

procedure TFormMain.LayoutSaveDefaultClick(Sender: TObject);
begin
  if MessageDlg(SSaveCurrentLayoutAsDefault, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    DockLayouts.SaveLayout(''); // '' means default
end;

procedure TFormMain.DbgLiveCallClick(Sender: TObject);
begin
  uDebugger.LiveCall(DockLayouts.FrameDisasm1.DisasmText.CursorVA);
end;

procedure TFormMain.Deletedatabase1Click(Sender: TObject);
begin
  DBCloseCurrent(True);
end;

procedure TFormMain.Export_ExportedSymbolsClick(Sender: TObject);
var
  fn: string;
begin
  if uChooseFileName.ChooseFileName(fn, '', SExtDumpedExports) then
  begin
    CoreGet.DumpDatabaseExports(BSTR_IN(fn));
  end;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  if not FInitializedOnceOnActivate then
  begin
    FInitializedOnceOnActivate := True;

    // Try to load current layout first, or fall back to default layout.
    // If both layout failed, design layout stays active.
    if not DockLayouts.LoadLayout(CurrentLayoutName) then
      DockLayouts.LoadLayout('');

    RescanPlugins(MenuPlugins, VDAPI.TIOVar.Plugins, gpt_plugin);
    RescanPlugins(DbgChoose, VDAPI.TIOVar.Debuggers, gpt_debugger);

    DBOpen(ParamStr(1));
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  c: IVDCore;
begin
  c := CoreGet();
  if Assigned(c) then
  begin
    c.CloseDatabase(False);
    c := nil;
  end;

  // Don't save layout in Destroy (floating windows will fail).
  DockLayouts.SaveLayout(CurrentLayoutName);

  CoreFree;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Core: IVDCore;
begin
  DockLayouts := TVDDockLayouts.Create(Self);

{$IFNDEF DEBUG}
  // Hide not stable things.
  NewType1.Visible := False;
  // MenuDebug.Visible := False;
  InternalDebug1.Visible := False;

  ViewOptions.Visible := False;
  ViewDecompiler.Visible := False;
{$ENDIF}
  // Init core lib.
  Core := CoreGet(True, uListenerMain.ListenerProc);
  if Core = nil then
  begin
    ShowMessage(SCoreLibNotInitialized);
    Halt;
  end;

  // Done.
  // Core.Log.WriteLn(SCoreLibInitialized);

  UpdateMainMenuItems(False);

  RecentListInit;

  // Accept files.
  DragAcceptFiles(Handle, True);

  // ApplyHiewStyle(DockLayouts.FrameDisasm1.DisasmText);
  // ApplyHiewStyle(DockLayouts.FrameHex1.HexEdit);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // shift+slash
  if (Shift = [ssShift]) and (Key = vkSlash) then
    ViewCalc1Click(nil);
end;

procedure TFormMain.ViewHexClick(Sender: TObject);
begin
  ChangeViewVisible([view_hex], True);
end;

procedure TFormMain.ViewImportsClick(Sender: TObject);
begin
  ChangeViewVisible([view_imports], True);
end;

procedure TFormMain.ViewExportsClick(Sender: TObject);
begin
  ChangeViewVisible([view_exports], True);
end;

procedure TFormMain.ViewNamesClick(Sender: TObject);
begin
  ChangeViewVisible([view_names], True);
end;

procedure TFormMain.ViewCalc1Click(Sender: TObject);
var
  Text: string;
begin
  if DockLayouts.FrameDisasm1.DisasmText.Enabled then
    Text := DockLayouts.FrameDisasm1.DisasmText.SelectedTextSingleLine
  else
    Text := '';
  FormCalc.ShowCalc(Text);
end;

procedure TFormMain.ViewDecompilerClick(Sender: TObject);
begin
{$IFDEF ENABLE_DECOMPILATION_FRAME}
  ChangeViewVisible([view_decompiler], True);
{$ENDIF}
end;

procedure TFormMain.ViewDisasmClick(Sender: TObject);
begin
  ChangeViewVisible([view_disassembler], True);
end;

procedure TFormMain.ViewMessagesClick(Sender: TObject);
begin
  ChangeViewVisible([view_messages], True);
end;

procedure TFormMain.idbg_LogLoadedPluginsClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  if InternalDbgUtilsGet <> nil then
    InternalDbgUtilsGet.DumpLoadedPlugins;
{$ENDIF}
end;

procedure TFormMain.idbg_LogTypePluginMappingClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  if InternalDbgUtilsGet <> nil then
    InternalDbgUtilsGet.DumpTypePluginMapping;
{$ENDIF}
end;

procedure TFormMain.MenuSearch_BytesOrStringsClick(Sender: TObject);
begin
  FormFindBytes.InvokeSearch();
end;

procedure TFormMain.NewType1Click(Sender: TObject);
begin
  uFormTypeEditor.Invoke(CoreGet().TypeLib, '');
end;

procedure TFormMain.Open1Click(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [ofPathMustExist];
    if dlg.Execute() then
    begin
      DBOpen(dlg.FileName);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TFormMain.ViewOptionsClick(Sender: TObject);
begin
  FormOptions.Show;
end;

procedure TFormMain.PluginItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    uPlugins.PluginItemClick(Sender as TMenuItem);
end;

procedure TFormMain.Savedatabase1Click(Sender: TObject);
begin
  DBSave;
end;

procedure TFormMain.MenuSearch_StringScanClick(Sender: TObject);
begin
  FormStringScan.Show;
end;

procedure TFormMain.ViewProblemsClick(Sender: TObject);
begin
  ChangeViewVisible([view_problems], True);
end;

procedure TFormMain.RecentItemClick(Sender: TObject);
var
  i: integer;
  fn: string;
begin
  i := (Sender as TMenuItem).Tag;
  fn := RecentListGetFileName(i);
  DBOpen(fn);
end;

procedure TFormMain.ViewReferencesClick(Sender: TObject);
begin
  ChangeViewVisible([view_refs], True);
end;

procedure TFormMain.ViewSectionsClick(Sender: TObject);
begin
  ChangeViewVisible([view_sections], True);
end;

procedure TFormMain.Watches1Click(Sender: TObject);
begin
  ChangeViewVisible([view_watches], True);
end;

procedure TFormMain.WMDropFiles(var Message: TWMDropFiles);
var
  len: uint32;
  Path: string;
begin
  len := DragQueryFile(Message.Drop, 0, nil, 0);
  if len <> 0 then
  begin
    SetLength(Path, len + 1);
    DragQueryFile(Message.Drop, 0, PChar(Path), len + 1);
    SetLength(Path, len);
    DBOpen(Path);
  end;
end;

procedure TFormMain.Zipdatabase1Click(Sender: TObject);
begin
  CoreGet().ZipDatabase;
end;

procedure TFormMain.TimerUpdateFramesTimer(Sender: TObject);
begin
  UpdateViews(AllViewTypes, False);
end;

procedure TFormMain.TypeLibBrowser1Click(Sender: TObject);
begin
  FormTypeLibBrowser.Show;
end;

procedure TFormMain.DbgSuspendClick(Sender: TObject);
begin
  uDebugger.SuspendDebugger;
end;

procedure TFormMain.DbgRunToCursorClick(Sender: TObject);
begin
  uDebugger.RunTo(DockLayouts.FrameDisasm1.DisasmText.CursorVA);
end;

procedure TFormMain.DbgStartResumeClick(Sender: TObject);
begin
  uDebugger.StartResumeDebugger;
end;

procedure TFormMain.DbgStepIntoClick(Sender: TObject);
begin
  uDebugger.StepIn;
end;

procedure TFormMain.DbgStepOverClick(Sender: TObject);
begin
  uDebugger.StepOver;
end;

procedure TFormMain.DbgTerminateClick(Sender: TObject);
begin
  uDebugger.TerminateDebugger;
end;

end.
