object FormTextInput: TFormTextInput
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Caption'
  ClientHeight = 266
  ClientWidth = 470
  Color = clBtnFace
  Constraints.MinHeight = 170
  Constraints.MinWidth = 385
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    470
    266)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Text'
  end
  object mmText: TMemo
    Left = 8
    Top = 27
    Width = 454
    Height = 198
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyPress = mmTextKeyPress
  end
  object btOK: TButton
    Left = 8
    Top = 233
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btCancel: TButton
    Left = 89
    Top = 233
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object StaticText1: TStaticText
    Left = 317
    Top = 241
    Width = 132
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Shift + Enter for line break'
    TabOrder = 3
  end
end
