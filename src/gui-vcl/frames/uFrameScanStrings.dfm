inherited FrameScanStrings: TFrameScanStrings
  inherited FListView: TListView
    Columns = <
      item
        Caption = 'VA'
        MaxWidth = 150
      end
      item
        Caption = 'Text'
      end>
  end
  inherited PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    object N4: TMenuItem
      Caption = '-'
    end
    object Defineselecteditemsastrings1: TMenuItem
      Caption = 'Define selected items a strings'
      OnClick = Defineselecteditemsastrings1Click
    end
  end
end
