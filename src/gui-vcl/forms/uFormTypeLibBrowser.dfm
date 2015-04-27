object FormTypeLibBrowser: TFormTypeLibBrowser
  Left = 0
  Top = 0
  Caption = 'Type Library Browser'
  ClientHeight = 348
  ClientWidth = 449
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 229
    Top = 0
    Height = 348
    Align = alRight
    ExplicitLeft = 232
    ExplicitTop = 112
    ExplicitHeight = 100
  end
  object lvImp: TListView
    Left = 232
    Top = 0
    Width = 217
    Height = 348
    Align = alRight
    Columns = <
      item
        AutoSize = True
        Caption = 'Imported Libraries'
      end>
    ReadOnly = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvImpDblClick
    OnEnter = ListViewsEnter
    OnKeyDown = lvImpKeyDown
  end
  object lvTypes: TListView
    Left = 0
    Top = 0
    Width = 229
    Height = 348
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Types of "Current"'
      end>
    ReadOnly = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvTypesChange
    OnDblClick = lvTypesDblClick
    OnEnter = ListViewsEnter
    OnKeyDown = lvTypesKeyDown
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 56
    Top = 80
    object NewType1: TMenuItem
      Caption = 'New Type...'
      OnClick = NewType1Click
    end
  end
end
