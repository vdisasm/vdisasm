unit uListenerMain;

interface

uses
  System.Classes, // Point()
  System.Types,   // expand
  System.SysUtils,

  WinApi.Messages,
  WinApi.Windows,

  Vcl.Controls,
  Vcl.Forms,

  uColorText,   // expand
  uVAColorText, // expand
  uDisAsmText,  // expand

  VDAPI;

const
  SB_MAIN_PNL_DBG_STATE = 0;

procedure ListenerProc(Msg: TVDMessage; Param: Pointer); stdcall;
procedure UpdateMainMenuItems(Opened: boolean);

implementation

uses
  uFormMain,
  uMenu,
  uFormUISelectItem,
  uFrameDisasm, // dv_disasm
  uFrameBaseListView,
  uFrameWatches,
  uDockLayouts,
  uDebugger;

const
  DBG_STATE_TO_STR: array [TVDDebuggerState] of string =
    (
    'Inactive',
    'Running',
    'Paused'
    );

var
  MenuMgr: TMenuMgr;

procedure Startup;
begin
  MenuMgr := TMenuMgr.Create(FormMain.MainMenu1);
end;

procedure Shutdown;
begin
  FreeAndNil(MenuMgr);
end;

procedure UpdateMainMenuItems(Opened: boolean);
begin
  // file
  FormMain.Close1.Visible := Opened;
  FormMain.Savedatabase1.Visible := Opened;
  FormMain.Deletedatabse1.Visible := Opened;
  FormMain.Export1.Visible := Opened;
  FormMain.Cleanupdatabase1.Visible := Opened;
  // edit
  FormMain.ClearJumpHistory.Visible := Opened;
{$IFDEF DEBUG}
  FormMain.NewType1.Visible := Opened;
  FormMain.Zipdatabase1.Visible := Opened;
{$ELSE}
  FormMain.NewType1.Visible := False;
  FormMain.Zipdatabase1.Visible := False;
{$ENDIF}
  // search
  FormMain.MenuSearch_BytesOrStrings.Visible := Opened;
  FormMain.MenuSearch_StringScan.Visible := Opened;
end;

procedure SetColorTextVaAndRepaint(const ctl: TVAColorText; VA: TVA);
begin
  ctl.VA := VA;
  ctl.InvalidateAllRowsWithRebuilding;
  // ctl.Repaint; // this will update Enabled property
end;

procedure DoSetControlFocusEx(ctrl: TWinControl);
begin
  ctrl.Enabled := True;

  // We have to post message instead of using SetFocus to set focus normally.
  WinApi.Windows.PostMessage(ctrl.Handle, WM_SETFOCUS, 0, 0);
end;

procedure ListenerProc(Msg: TVDMessage; Param: Pointer); stdcall;
var
  VAPos: TVDVAPos;
  VA: TVA;
var
  dbg_state: TVDDebuggerState;
begin
  case Msg of
    // --- core ---
    MSG_CORE_INIT:
      Startup;
    MSG_CORE_FREE:
      Shutdown;
    // --- db open/close ---
    MSG_DB_AFTER_SAVE:
      begin
        // LoadSaveWindow(False);
      end;
    MSG_DB_OPENED:
      begin
        if not CoreGet.GetVAPos(@VAPos) then
          CoreGet.Log.WriteLn('Initial position not found.');

        // disasm: it's enough to rebuild it;
        // va will be taken from core
        FormMain.DockLayouts.FrameDisasm1.DisasmText.InvalidateAllRowsWithRebuilding;

        // hex: it needs va set manually because it does not follow core va
        SetColorTextVaAndRepaint(FormMain.DockLayouts.FrameHex1.HexEdit, VAPos.ScrVA);

        UpdateMainMenuItems(True);
        UpdateViews(AllViewTypes);

        // Move focus to disasm.
        FormMain.DockLayouts.pnlDisAsm.Show;
        DoSetControlFocusEx(FormMain.DockLayouts.FrameDisasm1.DisasmText);
      end;
    MSG_DB_BEFORE_CLOSED:
      begin
        FormMain.DockLayouts.FrameDisasm1.SwitchToView(dv_disasm);
      end;
    MSG_DB_CLOSED:
      begin
        FormMain.DockLayouts.FrameDisasm1.DisasmText.ResetCaretPos(False);
        FormMain.DockLayouts.FrameDisasm1.DisasmText.InvalidateAllRowsWithRebuilding;

        FormMain.DockLayouts.FrameHex1.HexEdit.ResetCaretPos(False);
        FormMain.DockLayouts.FrameHex1.HexEdit.InvalidateAllRowsWithRebuilding;

{$IFDEF ENABLE_DECOMPILATION_FRAME}
        // Free function (release cpu and other).
        FormMain.DockLayouts.FrameDecompilation1.SetFn(nil);
{$ENDIF}
        UpdateMainMenuItems(False);
        UpdateViews(AllViewTypes);
      end;
    // --- log ---
    MSG_LOG_CLEAR:
      FormMain.DockLayouts.FrameLog1.TextClear;
    MSG_LOG_TEXT:
      FormMain.DockLayouts.FrameLog1.TextLog(BSTR(Param));
    MSG_LOG_BEGIN_UPDATE:
      FormMain.DockLayouts.FrameLog1.TextBeginUpdate;
    MSG_LOG_END_UPDATE:
      FormMain.DockLayouts.FrameLog1.TextEndUpdate;
    // --- menu ---
    MSG_MENU_ADD:
      MenuMgr.Add(Param);
    MSG_MENU_REMOVE:
      MenuMgr.Remove(Param);
    // --- item select ---
    MSG_UI_SELECT_ITEM:
      PVDItemSelectorRecord(Param)^.ResultIndex :=
        FormUISelectItem.SelectItem(PVDItemSelectorRecord(Param)^.Selector);
    // --- changes ---
    MSG_VM_WRITE_DONE:
      FormMain.DockLayouts.FrameDisasm1.DisasmText.InvalidateAllRowsWithRebuilding;
    MSG_IS_VA_ON_SCREEN:
      begin
        if FormMain.DockLayouts.FrameDisasm1.DisasmText.FindVAInData(PVA(Param)^, nil) = fid_equal then
          PVA(Param)^ := BAD_VA;
      end;
    MSG_VAPOS_CHANGED:
      begin
        FormMain.DockLayouts.FrameDisasm1.SwitchToView(dv_disasm);
        FormMain.DockLayouts.FrameDisasm1.DisasmText.ForceVAChanged;
        if FormMain.DockLayouts.FrameDisasm1.DisasmText.Enabled then
          FormMain.DockLayouts.FrameDisasm1.DisasmText.SetFocus;

        // hex edit don't need this message
        // FormHexEdit.HexEdit.ForceVAChanged;
      end;
    MSG_SECTIONS_CHANGED:
      begin
        UpdateViews([view_sections]);
        FormMain.DockLayouts.FrameDisasm1.DisasmText.InvalidateAllRowsWithRebuilding;
      end;
    MSG_NAMES_CHANGED:
      UpdateViews([view_names]);
    MSG_REFS_CHANGED:
      UpdateViews([view_refs]);
    MSG_EXPORTS_CHANGED:
      UpdateViews([view_exports]);
    MSG_IMPORTS_CHANGED:
      UpdateViews([view_imports]);
    MSG_PROBLEMS_CHANGED:
      UpdateViews([view_problems]);
    // -----------
    MSG_UI_REPAINT_VIEW:
      DoRepaintUiView(uint32(Param));
    MSG_CHANGE_GUI_REFRESH_STATE:
      begin
        FormMain.TimerUpdateFrames.Enabled := uint(Param) <> 0;
      end;

    MSG_UI_GET_SELRANGE:
      begin
        if FormMain.DockLayouts.FrameDisasm1.DisasmText.Visible then
        begin
          FormMain.DockLayouts.FrameDisasm1.DisasmText.GetSelectionVA(
            PVDSelVARange(Param).va0,
            PVDSelVARange(Param).va1);
        end;
      end;

    // -----------
    MSG_DBG_WANT_STOP:
      uDebugger.TerminateDebugger;

    MSG_DBG_STARTED:
      begin
        FormMain.DockLayouts.FrameDisasm1.DisasmText.InvalidateAllRowsWithRebuilding;
      end;

    MSG_DBG_STOPPED:
      begin
        // FormMain.FrameDisasm1.DisasmText.InvalidateLines;
        FormMain.DockLayouts.FrameWatches1.UpdateWatches(wpo_clear_changed_flag);
      end;

    MSG_DBG_SINGLE_STEP:
      begin
        VA := PVDDebugEvent(Param).SingleStep.VA;
        CoreGet.ChangeVA(VA);
      end;

    MSG_DBG_STATE_CHANGED:
      begin
        dbg_state := CoreGet.DebugSession.GetState;
        FormMain.StatusBarMain.Panels[SB_MAIN_PNL_DBG_STATE].Text := DBG_STATE_TO_STR[dbg_state];

        case dbg_state of
          DBGSTATE_INACTIVE:
            begin
              FormMain.DockLayouts.FrameDisasm1.DisasmText.InvalidateAllRowsWithRebuilding;
            end;
          DBGSTATE_PAUSED:
            begin
              // It's important to do update only when it came paused.
              // Otherwise getting expression when debuggee running may cause
              // sync problems (AVs).
              FormMain.DockLayouts.FrameWatches1.UpdateWatches(wpo_check_modification);
            end;
        end;
      end;
    // --- end ---
  end;

end;

end.
