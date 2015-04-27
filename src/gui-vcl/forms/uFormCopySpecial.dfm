object FormCopySpecial: TFormCopySpecial
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Copy Special'
  ClientHeight = 537
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 9
    Top = 396
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object Label1: TLabel
    Left = 8
    Top = 20
    Width = 37
    Height = 13
    Caption = 'First VA'
  end
  object Label3: TLabel
    Left = 8
    Top = 47
    Width = 87
    Height = 13
    Caption = 'Last VA (inclusive)'
  end
  object edVaFirst: TEdit
    Left = 176
    Top = 17
    Width = 153
    Height = 21
    TabOrder = 0
    Text = '0x0'
    OnChange = edVaFirstChange
  end
  object btCopy: TButton
    Left = 357
    Top = 38
    Width = 153
    Height = 34
    Caption = 'Copy'
    TabOrder = 6
    OnClick = btCopyClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 120
    Width = 153
    Height = 265
    Caption = 'Templates'
    TabOrder = 4
    object rbTNorm: TRadioButton
      Left = 16
      Top = 143
      Width = 113
      Height = 17
      Caption = 'Normal text'
      TabOrder = 5
      OnClick = rbTNormClick
    end
    object rbTPat: TRadioButton
      Left = 16
      Top = 120
      Width = 113
      Height = 17
      Caption = 'Pattern text'
      TabOrder = 4
      OnClick = rbTPatClick
    end
    object rbTPas: TRadioButton
      Left = 16
      Top = 74
      Width = 113
      Height = 17
      Caption = 'Pascal array'
      TabOrder = 2
      OnClick = rbTPasClick
    end
    object rbTC: TRadioButton
      Left = 16
      Top = 28
      Width = 113
      Height = 17
      Caption = 'C array'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbTCClick
    end
    object rbTPasAsmDb: TRadioButton
      Left = 16
      Top = 97
      Width = 113
      Height = 17
      Caption = 'Pascal asm bytes'
      TabOrder = 3
      OnClick = rbTPasAsmDbClick
    end
    object rbTCSharp: TRadioButton
      Left = 16
      Top = 51
      Width = 113
      Height = 17
      Caption = 'C# array'
      TabOrder = 1
      OnClick = rbTCSharpClick
    end
  end
  object mmPreview: TMemo
    Left = 9
    Top = 415
    Width = 517
    Height = 113
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 7
    WordWrap = False
  end
  object edVaLast: TEdit
    Left = 176
    Top = 44
    Width = 153
    Height = 21
    TabOrder = 1
    Text = '0x0'
    OnChange = edVaLastChange
  end
  object cbSize: TComboBox
    Left = 176
    Top = 71
    Width = 153
    Height = 21
    TabOrder = 3
    Text = '0x0'
    OnChange = cbSizeChange
    Items.Strings = (
      '0x8'
      '0x10'
      '0x20'
      '0x40'
      '0x80'
      '0x100'
      '0x200'
      '0x400'
      '0x800'
      '0x1000'
      '0x2000'
      '0x4000')
  end
  object GroupBox3: TGroupBox
    Left = 167
    Top = 120
    Width = 359
    Height = 265
    Caption = 'Advanced'
    TabOrder = 5
    object Label5: TLabel
      Left = 15
      Top = 32
      Width = 85
      Height = 13
      Caption = 'Name format (va)'
    end
    object Label6: TLabel
      Left = 15
      Top = 86
      Width = 57
      Height = 13
      Caption = 'Byte format'
    end
    object Label7: TLabel
      Left = 15
      Top = 114
      Width = 32
      Height = 13
      Caption = 'Indent'
    end
    object Label8: TLabel
      Left = 15
      Top = 141
      Width = 41
      Height = 13
      Caption = 'Delimiter'
    end
    object Label9: TLabel
      Left = 15
      Top = 165
      Width = 73
      Height = 26
      Caption = 'Header format (name, size)'
      WordWrap = True
    end
    object Label10: TLabel
      Left = 15
      Top = 210
      Width = 32
      Height = 13
      Caption = 'Footer'
    end
    object Label11: TLabel
      Left = 15
      Top = 59
      Width = 65
      Height = 13
      Caption = 'Column count'
    end
    object edNameFmt: TEdit
      Left = 112
      Top = 29
      Width = 161
      Height = 21
      TabOrder = 0
      Text = '_%x'
      OnChange = edNameFmtChange
    end
    object edByteFmt: TEdit
      Left = 112
      Top = 83
      Width = 161
      Height = 21
      TabOrder = 2
      OnChange = edByteFmtChange
    end
    object edDelimiter: TEdit
      Left = 112
      Top = 138
      Width = 105
      Height = 21
      TabOrder = 4
      OnChange = edDelimiterChange
    end
    object edIndent: TEdit
      Left = 112
      Top = 111
      Width = 161
      Height = 21
      TabOrder = 3
      OnChange = edIndentChange
    end
    object mmHeader: TMemo
      Left = 112
      Top = 165
      Width = 233
      Height = 39
      ScrollBars = ssVertical
      TabOrder = 6
      OnChange = mmHeaderChange
    end
    object mmFooter: TMemo
      Left = 112
      Top = 210
      Width = 233
      Height = 39
      ScrollBars = ssVertical
      TabOrder = 7
      OnChange = mmFooterChange
    end
    object edColumnCount: TSpinEdit
      Left = 112
      Top = 55
      Width = 73
      Height = 22
      MaxValue = 128
      MinValue = 1
      TabOrder = 1
      Value = 8
      OnChange = edColumnCountChange
    end
    object cbDelimAtEOL: TCheckBox
      Left = 223
      Top = 140
      Width = 97
      Height = 17
      Caption = 'At EOL'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = cbDelimAtEOLClick
    end
  end
  object cbUseSize: TCheckBox
    Left = 8
    Top = 73
    Width = 97
    Height = 17
    Caption = 'Size'
    TabOrder = 2
    OnClick = cbUseSizeClick
  end
end
