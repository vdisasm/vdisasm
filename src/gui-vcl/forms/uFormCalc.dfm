object FormCalc: TFormCalc
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Calc'
  ClientHeight = 167
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object MemoExpr: TMemo
    Left = 8
    Top = 8
    Width = 433
    Height = 57
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    Color = clWhite
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyPress = MemoExprKeyPress
  end
  object MemoResult: TMemo
    Left = 8
    Top = 71
    Width = 434
    Height = 89
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    Color = clWhite
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 448
    Top = 8
    Width = 73
    Height = 151
    Caption = 'Bitsize'
    TabOrder = 2
    object RadioButton8: TRadioButton
      Left = 16
      Top = 120
      Width = 40
      Height = 17
      Caption = '8'
      TabOrder = 3
      OnClick = RadioButton64Click
    end
    object RadioButton64: TRadioButton
      Left = 16
      Top = 24
      Width = 40
      Height = 17
      Caption = '64'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton64Click
    end
    object RadioButton32: TRadioButton
      Left = 16
      Top = 56
      Width = 40
      Height = 17
      Caption = '32'
      TabOrder = 1
      OnClick = RadioButton64Click
    end
    object RadioButton16: TRadioButton
      Left = 16
      Top = 88
      Width = 40
      Height = 17
      Caption = '16'
      TabOrder = 2
      OnClick = RadioButton64Click
    end
  end
end
