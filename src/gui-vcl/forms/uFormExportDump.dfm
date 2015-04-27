object FormExportDump: TFormExportDump
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Export'
  ClientHeight = 104
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object StaticText1: TStaticText
    Left = 16
    Top = 19
    Width = 41
    Height = 17
    Caption = 'First VA'
    TabOrder = 3
  end
  object StaticText2: TStaticText
    Left = 143
    Top = 19
    Width = 40
    Height = 17
    Caption = 'Last VA'
    TabOrder = 4
  end
  object edVA0: TEdit
    Left = 16
    Top = 34
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edVA1: TEdit
    Left = 143
    Top = 34
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object btExport: TButton
    Left = 270
    Top = 63
    Width = 98
    Height = 25
    Caption = 'Export'
    TabOrder = 2
    OnClick = btExportClick
  end
  object btAllVAs: TButton
    Left = 270
    Top = 32
    Width = 98
    Height = 25
    Caption = 'All VAs'
    TabOrder = 5
    OnClick = btAllVAsClick
  end
end
