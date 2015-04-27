object FormException: TFormException
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Debugger Exception'
  ClientHeight = 128
  ClientWidth = 548
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
    Left = 24
    Top = 24
    Width = 506
    Height = 49
    AutoSize = False
    WordWrap = True
  end
  object btBreak: TButton
    Left = 280
    Top = 88
    Width = 122
    Height = 25
    Caption = 'Break'
    TabOrder = 0
  end
  object btContinue: TButton
    Left = 408
    Top = 88
    Width = 122
    Height = 25
    Caption = 'Continue'
    TabOrder = 1
  end
end
