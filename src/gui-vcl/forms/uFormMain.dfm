object FormMain: TFormMain
  Left = 0
  Top = 0
  ClientHeight = 565
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBarMain: TStatusBar
    Left = 0
    Top = 546
    Width = 790
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 40
    object MenuFile: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = Open1Click
      end
      object Close1: TMenuItem
        Caption = 'Close database'
        OnClick = Close1Click
      end
      object Savedatabase1: TMenuItem
        Caption = 'Save database'
        OnClick = Savedatabase1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Cleanupdatabase1: TMenuItem
        Caption = 'Cleanup database'
        OnClick = Cleanupdatabase1Click
      end
      object Zipdatabase1: TMenuItem
        Caption = 'Zip database'
        OnClick = Zipdatabase1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Deletedatabse1: TMenuItem
        Caption = 'Delete database'
        OnClick = Deletedatabase1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Export1: TMenuItem
        Caption = 'Export'
        object Export_DatabaseText: TMenuItem
          Caption = 'Database text'
          OnClick = Export_DatabaseTextClick
        end
        object Export_ExportedSymbols: TMenuItem
          Caption = 'Exported Symbols'
          OnClick = Export_ExportedSymbolsClick
        end
      end
      object nBeforeRecentList: TMenuItem
        Caption = '-'
      end
      object Recent1: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Recent'
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object TypeLibBrowser1: TMenuItem
        Caption = 'TypeLib Browser...'
        OnClick = TypeLibBrowser1Click
      end
      object NewType1: TMenuItem
        Caption = 'New Type...'
        OnClick = NewType1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object ClearJumpHistory: TMenuItem
        Caption = 'Clear jump history'
        OnClick = ClearJumpHistoryClick
      end
    end
    object MenuSearch: TMenuItem
      Caption = 'Search'
      object MenuSearch_BytesOrStrings: TMenuItem
        Caption = 'Bytes or Strings'
        ShortCut = 16454
        OnClick = MenuSearch_BytesOrStringsClick
      end
      object MenuSearch_StringScan: TMenuItem
        Caption = 'String scan'
        OnClick = MenuSearch_StringScanClick
      end
    end
    object MenuView: TMenuItem
      Caption = 'View'
      object ViewDisasm: TMenuItem
        Caption = 'Disassembler'
        OnClick = ViewDisasmClick
      end
      object ViewDecompiler: TMenuItem
        Caption = 'Decompiler'
        OnClick = ViewDecompilerClick
      end
      object ViewHex: TMenuItem
        Caption = 'Hex View'
        OnClick = ViewHexClick
      end
      object ViewMessages: TMenuItem
        Caption = 'Messages'
        OnClick = ViewMessagesClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object ViewExports: TMenuItem
        Caption = 'Exports'
        ShortCut = 16453
        OnClick = ViewExportsClick
      end
      object ViewImports: TMenuItem
        Caption = 'Imports'
        ShortCut = 16457
        OnClick = ViewImportsClick
      end
      object ViewReferences: TMenuItem
        Caption = 'References'
        ShortCut = 16472
        OnClick = ViewReferencesClick
      end
      object ViewSections: TMenuItem
        Caption = 'Sections'
        OnClick = ViewSectionsClick
      end
      object ViewNames: TMenuItem
        Caption = 'Names'
        OnClick = ViewNamesClick
      end
      object ViewProblems: TMenuItem
        Caption = 'Problems'
        OnClick = ViewProblemsClick
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Watches1: TMenuItem
        Caption = 'Watches'
        OnClick = Watches1Click
      end
      object Breakpoints1: TMenuItem
        Caption = 'Breakpoints'
        OnClick = Breakpoints1Click
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object ViewCalc1: TMenuItem
        Caption = 'Calc'
        OnClick = ViewCalc1Click
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object LayoutLoadDefault: TMenuItem
        Caption = 'Load default layout'
        OnClick = LayoutLoadDefaultClick
      end
      object LayoutSaveDefault: TMenuItem
        Caption = 'Save as default layout'
        OnClick = LayoutSaveDefaultClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ViewOptions: TMenuItem
        Caption = 'Options'
        ShortCut = 16463
        OnClick = ViewOptionsClick
      end
    end
    object MenuPlugins: TMenuItem
      Caption = 'Plugins'
      Visible = False
    end
    object MenuDebug: TMenuItem
      Caption = 'Debug'
      object DbgChoose: TMenuItem
        Caption = 'Choose Debugger'
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object DbgStartResume: TMenuItem
        Caption = 'Start\Resume'
        ShortCut = 120
        OnClick = DbgStartResumeClick
      end
      object DbgSuspend: TMenuItem
        Caption = 'Suspend'
        OnClick = DbgSuspendClick
      end
      object DbgStepInto: TMenuItem
        Caption = 'Step into'
        ShortCut = 118
        OnClick = DbgStepIntoClick
      end
      object DbgStepOver: TMenuItem
        Caption = 'Step over'
        ShortCut = 119
        OnClick = DbgStepOverClick
      end
      object DbgRunToCursor: TMenuItem
        Caption = 'Run to cursor'
        ShortCut = 115
        OnClick = DbgRunToCursorClick
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object DbgLiveCall: TMenuItem
        Caption = 'Live call'
        ShortCut = 117
        OnClick = DbgLiveCallClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object DbgTerminate: TMenuItem
        Caption = 'Terminate'
        ShortCut = 16497
        OnClick = DbgTerminateClick
      end
    end
    object InternalDebug1: TMenuItem
      Caption = 'Internal Debug'
      object idbg_LogLoadedPlugins: TMenuItem
        Caption = 'Log loaded plugins'
        OnClick = idbg_LogLoadedPluginsClick
      end
      object idbg_LogTypePluginMapping: TMenuItem
        Caption = 'Log Type-Plugin mapping'
        OnClick = idbg_LogTypePluginMappingClick
      end
    end
    object MenuHelp: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About...'
        OnClick = About1Click
      end
    end
    object N3: TMenuItem
      Caption = '-'
    end
  end
  object PopupMenuLog: TPopupMenu
    Left = 296
    Top = 40
    object Clearall1: TMenuItem
      Caption = 'Clear All'
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
    end
    object Copyall1: TMenuItem
      Caption = 'Copy All'
    end
  end
  object TimerUpdateFrames: TTimer
    OnTimer = TimerUpdateFramesTimer
    Left = 184
    Top = 41
  end
end
