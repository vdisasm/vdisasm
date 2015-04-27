object FrameDecompilation: TFrameDecompilation
  Left = 0
  Top = 0
  Width = 717
  Height = 482
  TabOrder = 0
  object pnlText: TPanel
    Left = 0
    Top = 0
    Width = 717
    Height = 482
    Align = alClient
    TabOrder = 0
    object pnlControls: TPanel
      Left = 1
      Top = 1
      Width = 715
      Height = 41
      Align = alTop
      TabOrder = 0
      object btnGraph: TButton
        Left = 8
        Top = 9
        Width = 75
        Height = 25
        Caption = 'Graph'
        TabOrder = 0
        OnClick = btnGraphClick
      end
    end
    object StatusBar1: TStatusBar
      Left = 1
      Top = 462
      Width = 715
      Height = 19
      Panels = <
        item
          Width = 50
        end>
      ExplicitLeft = 360
      ExplicitTop = 232
      ExplicitWidth = 0
    end
  end
end
