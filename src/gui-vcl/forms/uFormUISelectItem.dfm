object FormUISelectItem: TFormUISelectItem
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'FormUISelectItem'
  ClientHeight = 258
  ClientWidth = 563
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
  DesignSize = (
    563
    258)
  PixelsPerInch = 96
  TextHeight = 13
  object lb1: TListBox
    Left = 8
    Top = 8
    Width = 547
    Height = 242
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 0
    OnDblClick = lb1DblClick
  end
end
