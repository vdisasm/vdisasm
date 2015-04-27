object FormLiveCall: TFormLiveCall
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Live call'
  ClientHeight = 144
  ClientWidth = 393
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 35
    Height = 13
    Caption = 'Params'
  end
  object mmParams: TMemo
    Left = 8
    Top = 24
    Width = 377
    Height = 81
    Lines.Strings = (
      '@0x300ECC0C'
      '"hello"')
    TabOrder = 0
  end
  object btCall: TButton
    Left = 279
    Top = 111
    Width = 106
    Height = 26
    Caption = 'Call'
    ModalResult = 1
    TabOrder = 1
  end
end
