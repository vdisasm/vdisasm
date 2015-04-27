object FormStringScan: TFormStringScan
  Left = 0
  Top = 0
  Caption = 'String scan'
  ClientHeight = 449
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 472
    Height = 49
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 67
      Height = 13
      Caption = 'Minimal length'
    end
    object Scan: TButton
      Left = 231
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Scan'
      TabOrder = 0
      OnClick = ScanClick
    end
    object EditMinLen: TEdit
      Left = 104
      Top = 13
      Width = 121
      Height = 21
      NumbersOnly = True
      TabOrder = 1
      Text = '5'
    end
  end
  inline FrameScanStrings1: TFrameScanStrings
    Left = 0
    Top = 49
    Width = 472
    Height = 400
    Align = alClient
    TabOrder = 1
    ExplicitTop = 49
    ExplicitWidth = 472
    ExplicitHeight = 400
    inherited FListView: TListView
      Width = 472
      Height = 381
      Columns = <
        item
          AutoSize = True
          Caption = 'VA'
          MaxWidth = 150
        end
        item
          AutoSize = True
          Caption = 'Text'
        end>
      ExplicitWidth = 472
      ExplicitHeight = 381
    end
    inherited StatusBar1: TStatusBar
      Top = 381
      Width = 472
      ExplicitTop = 381
      ExplicitWidth = 472
    end
  end
end
