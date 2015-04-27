inherited FrameSections: TFrameSections
  inherited FListView: TListView
    Columns = <
      item
        AutoSize = True
        Caption = 'ID'
        MaxWidth = 50
      end
      item
        AutoSize = True
        Caption = 'Name'
        MaxWidth = 150
      end
      item
        AutoSize = True
        Caption = 'VA'
        MaxWidth = 150
      end
      item
        AutoSize = True
        Caption = 'End VA'
        MaxWidth = 150
      end
      item
        AutoSize = True
        Caption = 'Virtual Size'
        MaxWidth = 100
      end
      item
        AutoSize = True
        Caption = 'Flags'
        MaxWidth = 50
      end>
  end
  inherited PopupMenu1: TPopupMenu
    object N4: TMenuItem
      Caption = '-'
    end
    object Savesectiontofile1: TMenuItem
      Caption = 'Save section to file'
      OnClick = Savesectiontofile1Click
    end
  end
end
