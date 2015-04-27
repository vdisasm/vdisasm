object LoaderForm: TLoaderForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  ClientHeight = 535
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 410
    Width = 584
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 425
  end
  object lvSec: TListView
    Left = 0
    Top = 0
    Width = 584
    Height = 410
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = '#'
        MaxWidth = 40
      end
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'VA'
      end
      item
        AutoSize = True
        Caption = 'VSize'
      end
      item
        AutoSize = True
        Caption = 'ROfs'
      end
      item
        AutoSize = True
        Caption = 'RSize'
      end
      item
        AutoSize = True
        Caption = 'Flags'
        MaxWidth = 100
      end>
    DoubleBuffered = True
    GridLines = True
    OwnerData = True
    RowSelect = True
    ParentDoubleBuffered = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = lvSecCustomDrawItem
    OnData = lvSecData
    ExplicitHeight = 437
  end
  object Panel1: TPanel
    Left = 0
    Top = 413
    Width = 584
    Height = 122
    Align = alBottom
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 416
    object Label2: TLabel
      Left = 200
      Top = 13
      Width = 26
      Height = 13
      Caption = 'Entry'
    end
    object Label3: TLabel
      Left = 16
      Top = 16
      Width = 50
      Height = 13
      Caption = 'Code type'
    end
    object Label1: TLabel
      Left = 16
      Top = 64
      Width = 60
      Height = 13
      Caption = 'Address size'
    end
    object edEntry: TEdit
      Left = 200
      Top = 32
      Width = 169
      Height = 21
      TabOrder = 2
    end
    object cbCPUs: TComboBox
      Left = 16
      Top = 32
      Width = 169
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object btOk: TButton
      Left = 440
      Top = 22
      Width = 97
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btCancel: TButton
      Left = 440
      Top = 53
      Width = 97
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object CheckBoxMapToInput: TCheckBox
      Left = 360
      Top = 92
      Width = 193
      Height = 17
      Caption = 'Map sections directly to input file'
      TabOrder = 4
      OnClick = CheckBoxMapToInputClick
    end
    object cbAddressSize: TComboBox
      Left = 16
      Top = 80
      Width = 169
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      Items.Strings = (
        '2 (16-bit)'
        '4 (32-bit)'
        '8 (64-bit)')
    end
  end
end
