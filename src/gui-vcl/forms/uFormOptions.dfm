object FormOptions: TFormOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Options'
  ClientHeight = 563
  ClientWidth = 891
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 0
    Height = 522
    ExplicitLeft = 88
    ExplicitTop = 88
    ExplicitHeight = 100
  end
  object pnlTree: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 522
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object TreeView1: TTreeView
      Left = 0
      Top = 0
      Width = 201
      Height = 522
      Align = alClient
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnChange = TreeView1Change
    end
  end
  object pnlBtns: TPanel
    Left = 0
    Top = 522
    Width = 891
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      891
      41)
    object Button1: TButton
      Left = 728
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 0
    end
    object Button2: TButton
      Left = 809
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
    end
  end
  object pnlCurrent: TPanel
    Left = 204
    Top = 0
    Width = 687
    Height = 522
    Align = alClient
    BevelEdges = [beBottom]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 1
  end
end
