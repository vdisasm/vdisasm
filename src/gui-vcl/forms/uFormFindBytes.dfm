object FormFindBytes: TFormFindBytes
  Left = 0
  Top = 0
  Anchors = [akRight, akBottom]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Find Bytes or String'
  ClientHeight = 237
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    336
    237)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 36
    Height = 13
    Caption = 'Pattern'
  end
  object Label3: TLabel
    Left = 8
    Top = 137
    Width = 83
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'String Code Page'
  end
  object mmPattern: TMemo
    Left = 8
    Top = 27
    Width = 320
    Height = 96
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    OnChange = mmPatternChange
    OnKeyPress = mmPatternKeyPress
  end
  object rbUp: TRadioButton
    Left = 215
    Top = 137
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Up'
    TabOrder = 1
    OnClick = rbUpClick
  end
  object rbDown: TRadioButton
    Left = 215
    Top = 160
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Down'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = rbDownClick
  end
  object cbStrEnc: TComboBox
    Left = 8
    Top = 156
    Width = 193
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 3
  end
  object cbStrCaseSens: TCheckBox
    Left = 8
    Top = 183
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Case-sensitive'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 4
  end
  object btFind: TButton
    Left = 215
    Top = 183
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Find'
    TabOrder = 5
    OnClick = btFindClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 218
    Width = 336
    Height = 19
    Panels = <
      item
        Style = psOwnerDraw
        Width = 50
      end>
    OnDrawPanel = StatusBar1DrawPanel
  end
end
