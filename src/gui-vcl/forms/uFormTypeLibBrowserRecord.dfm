object FormTypeLibBrowserRecord: TFormTypeLibBrowserRecord
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  ClientHeight = 260
  ClientWidth = 476
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lvFields: TListView
    Left = 0
    Top = 0
    Width = 476
    Height = 260
    Align = alClient
    Color = clWhite
    Columns = <
      item
        AutoSize = True
        Caption = 'VA'
      end
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'Type'
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnAdvancedCustomDrawItem = lvFieldsAdvancedCustomDrawItem
    OnData = lvFieldsData
    OnDblClick = lvFieldsDblClick
    OnKeyDown = lvFieldsKeyDown
  end
end
