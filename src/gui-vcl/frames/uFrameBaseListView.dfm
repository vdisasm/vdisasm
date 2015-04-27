object FrameBaseListView: TFrameBaseListView
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  OnEnter = FrameEnter
  object FListView: TListView
    Left = 0
    Top = 0
    Width = 320
    Height = 221
    Align = alClient
    Columns = <>
    DoubleBuffered = True
    GridLines = True
    MultiSelect = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = FListViewChange
    OnColumnClick = FListViewColumnClick
    OnData = FListViewData
    OnDblClick = FListViewDblClick
    OnKeyDown = FListViewKeyDown
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 221
    Width = 320
    Height = 19
    Panels = <
      item
        Style = psOwnerDraw
        Width = 100
      end>
    OnDrawPanel = StatusBar1DrawPanel
  end
  object PopupMenu1: TPopupMenu
    Left = 80
    Top = 64
    object Update1: TMenuItem
      Caption = 'Update'
      ShortCut = 116
      OnClick = Update1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuFind: TMenuItem
      Caption = 'Find'
      ShortCut = 16454
      OnClick = MenuFindClick
    end
    object MenuFindNext: TMenuItem
      Caption = 'Find Next'
      ShortCut = 114
      OnClick = MenuFindNextClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Copyselectiontobuffer1: TMenuItem
      Caption = 'Copy selection to buffer'
      ShortCut = 16451
      OnClick = Copyselectiontobuffer1Click
    end
    object Exporttotextfile1: TMenuItem
      Caption = 'Export selected text to file'
      OnClick = Exporttotextfile1Click
    end
  end
  object TimerUpdateAsync: TTimer
    Enabled = False
    OnTimer = TimerUpdateAsyncTimer
    Left = 192
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    FileName = 'list'
    Filter = '*.txt'
    Left = 80
    Top = 144
  end
end
