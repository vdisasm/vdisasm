object FrameDisasm: TFrameDisasm
  Left = 0
  Top = 0
  Width = 382
  Height = 300
  TabOrder = 0
  object StatusBar1: TStatusBar
    Left = 0
    Top = 281
    Width = 382
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object ppDis: TPopupMenu
    OnPopup = ppDisPopup
    Left = 48
    Top = 48
    object N10: TMenuItem
      Caption = '-'
    end
    object AnalyseCode1: TMenuItem
      Caption = 'Analyse Code'
      ShortCut = 70
      OnClick = AnalyseCode1Click
    end
    object DeanalyseCode1: TMenuItem
      Caption = 'Deanalyse Code'
      OnClick = DeanalyseCode1Click
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object AnalysisOptions1: TMenuItem
      Caption = 'Analysis Options'
      object StepIntoCall1: TMenuItem
        Caption = 'Step Into Calls'
        OnClick = CodeAnalysisFlagClick
      end
      object MarkUnmarkDataRefs1: TMenuItem
        Caption = 'Mark/Unmark Data Refs'
        OnClick = CodeAnalysisFlagClick
      end
      object MarkUnmarkCodeRefs1: TMenuItem
        Caption = 'Mark/Unmark Code Refs'
        OnClick = CodeAnalysisFlagClick
      end
      object DefineUndefineCode1: TMenuItem
        Caption = 'Define/Undefine Code'
        OnClick = CodeAnalysisFlagClick
      end
      object HandleCallsasJumps1: TMenuItem
        Caption = 'Handle Calls as Jumps'
        OnClick = CodeAnalysisFlagClick
      end
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object Define1: TMenuItem
      Caption = 'Define'
      object DefineType1: TMenuItem
        Caption = 'Type'
        ShortCut = 89
        OnClick = DefineType1Click
      end
      object DefineString1: TMenuItem
        Caption = 'String'
        ShortCut = 65
        OnClick = DefineString1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object DefineU8: TMenuItem
        Caption = 'byte'
        ShortCut = 49
        OnClick = DefineU8Click
      end
      object DefineU16: TMenuItem
        Caption = 'word'
        ShortCut = 50
        OnClick = DefineU16Click
      end
      object DefineU32: TMenuItem
        Caption = 'dword'
        ShortCut = 52
        OnClick = DefineU32Click
      end
      object DefineU64: TMenuItem
        Caption = 'qword'
        ShortCut = 56
        OnClick = DefineU64Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object code1: TMenuItem
        Caption = '%code%'
        ShortCut = 67
        OnClick = code1Click
      end
    end
    object EditValue1: TMenuItem
      Caption = 'Edit Value...'
      OnClick = EditValue1Click
    end
    object DefineName: TMenuItem
      Caption = 'Name'
      OnClick = DefineNameClick
    end
    object DefineComment: TMenuItem
      Caption = 'Comment'
      OnClick = DefineCommentClick
    end
    object DefineExport: TMenuItem
      Caption = 'Export'
      OnClick = DefineExportClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Undefine1: TMenuItem
      Caption = 'Undefine'
      ShortCut = 85
      OnClick = Undefine1Click
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object DeleteReferences1: TMenuItem
      Caption = 'Delete References'
      OnClick = DeleteReferences1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Go1: TMenuItem
      Caption = 'Go To'
      object VA1: TMenuItem
        Caption = 'VA'
        ShortCut = 71
        OnClick = VA1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object VMStart1: TMenuItem
        Caption = 'VM Start'
        OnClick = VMStart1Click
      end
      object VMEnd1: TMenuItem
        Caption = 'VM End'
        OnClick = VMEnd1Click
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CopyText1: TMenuItem
      Caption = 'Copy Text'
      OnClick = CopyText1Click
    end
    object CopySpecial1: TMenuItem
      Caption = 'Copy Special'
      OnClick = CopySpecial1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object HexDumpWidth1: TMenuItem
      Caption = 'Hex Dump Width'
      object HexDumpWidth_0: TMenuItem
        Caption = '0'
        OnClick = HexDumpWidth_0Click
      end
      object HexDumpWidth_4: TMenuItem
        Caption = '4'
        OnClick = HexDumpWidth_4Click
      end
      object HexDumpWidth_8: TMenuItem
        Caption = '8'
        OnClick = HexDumpWidth_8Click
      end
      object HexDumpWidth_16: TMenuItem
        Caption = '16'
        OnClick = HexDumpWidth_16Click
      end
    end
    object Highlighting1: TMenuItem
      Caption = 'Highlighting'
      object HighlightingOn1: TMenuItem
        Caption = 'On'
        OnClick = HighlightingOn1Click
      end
      object HighlightingOff1: TMenuItem
        Caption = 'Off'
        OnClick = HighlightingOff1Click
      end
      object HighlightingText: TMenuItem
        Caption = 'Text'
        OnClick = HighlightingTextClick
      end
    end
  end
  object ppGraph: TPopupMenu
    Left = 168
    Top = 48
    object Layout1: TMenuItem
      Caption = 'Layout'
      OnClick = Layout1Click
    end
  end
end
