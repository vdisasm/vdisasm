unit uFrameOptionsDisplayColors;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,

  uFrameOptionsBase,

  uLayoutText,

  VDAPI;

type
  TFrameDisplayColors = class(TFrameOptionsBase)
    Panel1: TPanel;
  public
    dt: TLayoutText;
    constructor Create(AOwner: TComponent); override;
  end;

var
  FrameDisplayColors: TFrameDisplayColors;

implementation

{$R *.dfm}


constructor TFrameDisplayColors.Create(AOwner: TComponent);
var
  va: TVA;
begin
  inherited;
  dt := TLayoutText.Create(self);
  dt.Align := alClient;
  dt.Parent := Panel1;

  va := 0;
  CoreGet.Decoder.DecodeToText(va, dt.Layout, TVDDecodeToTextFlag.Default or TVDDecodeToTextFlag.Sample4hl)
end;

end.
