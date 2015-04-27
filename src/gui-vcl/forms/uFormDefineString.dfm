object FormDefineString: TFormDefineString
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Define String'
  ClientHeight = 420
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 13
    Height = 13
    Caption = 'VA'
  end
  object Label2: TLabel
    Left = 8
    Top = 73
    Width = 52
    Height = 13
    Caption = 'Code Page'
  end
  object Label3: TLabel
    Left = 8
    Top = 46
    Width = 19
    Height = 13
    Caption = 'Size'
  end
  object Label4: TLabel
    Left = 256
    Top = 208
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = 'Char'
  end
  object Label5: TLabel
    Left = 225
    Top = 243
    Width = 54
    Height = 13
    Alignment = taRightJustify
    Caption = 'Length size'
  end
  object Label6: TLabel
    Left = 8
    Top = 280
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object edVA: TEdit
    Left = 97
    Top = 16
    Width = 216
    Height = 21
    Enabled = False
    TabOrder = 1
  end
  object edCodePage: TEdit
    Left = 97
    Top = 70
    Width = 217
    Height = 21
    NumbersOnly = True
    TabOrder = 3
  end
  object edCustomChar: TEdit
    Left = 285
    Top = 205
    Width = 28
    Height = 21
    MaxLength = 1
    TabOrder = 7
  end
  object edSize: TEdit
    Left = 97
    Top = 43
    Width = 217
    Height = 21
    NumbersOnly = True
    TabOrder = 2
  end
  object edLenPfxSize: TEdit
    Left = 285
    Top = 240
    Width = 28
    Height = 21
    NumbersOnly = True
    TabOrder = 8
    Text = '1'
  end
  object rbNullTerm: TRadioButton
    Left = 8
    Top = 175
    Width = 113
    Height = 17
    Caption = 'Null-terminated'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = rbNullTermClick
  end
  object rbCharTerm: TRadioButton
    Left = 8
    Top = 207
    Width = 113
    Height = 17
    Caption = 'Char-terminated'
    TabOrder = 6
    OnClick = rbNullTermClick
  end
  object rbLenPrefixed: TRadioButton
    Left = 8
    Top = 242
    Width = 113
    Height = 17
    Caption = 'Length-prefixed'
    TabOrder = 9
    OnClick = rbNullTermClick
  end
  object btDone: TButton
    Left = 225
    Top = 385
    Width = 88
    Height = 25
    Caption = 'Done'
    TabOrder = 0
    OnClick = btDoneClick
  end
  object mmPreview: TMemo
    Left = 8
    Top = 299
    Width = 305
    Height = 70
    TabStop = False
    ReadOnly = True
    TabOrder = 10
  end
  object rbUseSize: TRadioButton
    Left = 8
    Top = 144
    Width = 113
    Height = 17
    Caption = 'Use defined Size'
    TabOrder = 4
    OnClick = rbNullTermClick
  end
  object btUpdatePreview: TButton
    Left = 8
    Top = 385
    Width = 145
    Height = 25
    Caption = 'Update Preview'
    TabOrder = 11
    OnClick = btUpdatePreviewClick
  end
  object btCpUtf8: TButton
    Left = 97
    Top = 97
    Width = 56
    Height = 25
    Caption = 'UTF-8'
    TabOrder = 12
    OnClick = btCpUtf8Click
  end
  object btCpUtf16: TButton
    Left = 159
    Top = 97
    Width = 56
    Height = 25
    Caption = 'UTF-16'
    TabOrder = 13
    OnClick = btCpUtf16Click
  end
  object btCpReset: TButton
    Left = 239
    Top = 97
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 14
    OnClick = btCpResetClick
  end
end
