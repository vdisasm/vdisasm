object FormTypeEditor: TFormTypeEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Type Editor'
  ClientHeight = 306
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    754
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object gbBase: TGroupBox
    Left = 8
    Top = 8
    Width = 741
    Height = 88
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    DesignSize = (
      741
      88)
    object StaticText1: TStaticText
      Left = 16
      Top = 23
      Width = 31
      Height = 17
      Caption = 'Name'
      TabOrder = 0
    end
    object edName: TEdit
      Left = 120
      Top = 19
      Width = 482
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object StaticText2: TStaticText
      Left = 16
      Top = 48
      Width = 49
      Height = 17
      Caption = 'Comment'
      TabOrder = 2
    end
    object edComment: TEdit
      Left = 120
      Top = 46
      Width = 482
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object btDone: TButton
      Left = 617
      Top = 17
      Width = 104
      Height = 50
      Anchors = [akTop]
      Caption = 'Done'
      TabOrder = 4
    end
  end
  object pc1: TPageControl
    Left = 8
    Top = 102
    Width = 741
    Height = 195
    ActivePage = tsDefinition
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tsDefinition: TTabSheet
      Caption = 'Definition'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object tsAlias: TTabSheet
      Caption = 'Alias'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StaticText3: TStaticText
        Left = 16
        Top = 24
        Width = 64
        Height = 17
        Caption = 'Source Type'
        TabOrder = 0
      end
      object edAliasSrcName: TEdit
        Left = 116
        Top = 20
        Width = 473
        Height = 21
        TabOrder = 1
      end
    end
    object tsArray: TTabSheet
      Caption = 'Array'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StaticText4: TStaticText
        Left = 12
        Top = 19
        Width = 64
        Height = 17
        Caption = 'Source Type'
        TabOrder = 0
      end
      object StaticText5: TStaticText
        Left = 12
        Top = 42
        Width = 33
        Height = 17
        Caption = 'Count'
        TabOrder = 1
      end
      object Edit1: TEdit
        Left = 116
        Top = 17
        Width = 473
        Height = 21
        TabOrder = 2
      end
      object Edit2: TEdit
        Left = 116
        Top = 44
        Width = 473
        Height = 21
        TabOrder = 3
      end
    end
    object tsRecord: TTabSheet
      Caption = 'Record'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
