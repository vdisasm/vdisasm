object FrameLog: TFrameLog
  Left = 0
  Top = 0
  Width = 441
  Height = 177
  TabOrder = 0
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 441
    Height = 177
    Align = alClient
    PopupMenu = PopupMenu1
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PopupMenu1: TPopupMenu
    Left = 208
    Top = 72
    object ClearAll1: TMenuItem
      Caption = 'Clear All'
      OnClick = ClearAll1Click
    end
    object CopyAll1: TMenuItem
      Caption = 'Copy All'
      OnClick = CopyAll1Click
    end
    object CopySelected1: TMenuItem
      Caption = 'Copy Selected'
      OnClick = CopySelected1Click
    end
  end
end
