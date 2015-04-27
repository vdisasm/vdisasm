object FrameHex: TFrameHex
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object StatusBar1: TStatusBar
    Left = 0
    Top = 221
    Width = 320
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 64
    Top = 40
    object Go1: TMenuItem
      Caption = 'Go...'
      OnClick = Go1Click
    end
    object FollowinDisassembler1: TMenuItem
      Caption = 'Follow in Disassembler'
      OnClick = FollowinDisassembler1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SaveChanges1: TMenuItem
      Caption = 'Save Changes'
      ShortCut = 16467
      OnClick = SaveChanges1Click
    end
    object DiscardChanges1: TMenuItem
      Caption = 'Discard Changes'
      OnClick = DiscardChanges1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Columns1: TMenuItem
      Caption = 'Columns'
      object N41: TMenuItem
        Caption = '4'
        OnClick = N41Click
      end
      object N81: TMenuItem
        Caption = '8'
        OnClick = N81Click
      end
      object N161: TMenuItem
        Caption = '16'
        OnClick = N161Click
      end
      object N321: TMenuItem
        Caption = '32'
        OnClick = N321Click
      end
    end
  end
end
