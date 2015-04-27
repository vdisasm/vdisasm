unit uFrameDecompilation;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  uColorText.Types,
  uLayoutText,

  VDAPI;

type
  TFrameDecompilation = class;

  TDecompilerLayoutText = class(TLayoutText)
  strict private
    FFn: IVDFunction;
    procedure SetFn(const Value: IVDFunction);
  private
    FFrame: TFrameDecompilation;
    procedure DtTextUnderCaret(const Text: string);
  public
    property Fn: IVDFunction read FFn write SetFn;
  end;

  TFrameDecompilation = class(TFrame)
    pnlText: TPanel;
    pnlControls: TPanel;
    btnGraph: TButton;
    StatusBar1: TStatusBar;
    procedure btnGraphClick(Sender: TObject);
  private
    FDt: TDecompilerLayoutText;
  public
    constructor Create(AOwner: TComponent); override;
    // property Dt: TDecompilerLayoutText read FDt;

    procedure SetFn(Fn: IVDFunction);
  end;

implementation

{$R *.dfm}

{ TFrameDecompilation }

procedure TFrameDecompilation.btnGraphClick(Sender: TObject);
begin
  //
end;

procedure SetIds(const LayoutText: TLayoutText);
begin
  with LayoutText do
  begin
    SetId(TCastTag(TTag.TAGID_NONE), IDR_NONE);
    SetId(TCastTag(TTag.TAGID_VA), IDR_VA);
    SetId(TCastTag(TTag.TAGID_NUMBER), IDR_IMM);
    SetId(TCastTag(TTag.TAGID_STRING), IDR_STRING);
    SetId(TCastTag(TTag.TAGID_CODE), IDR_CODE);

    SetId(TCastTag(TTag.TAGID_LABEL), IDR_LABEL);
    SetId(TCastTag(TTag.TAGID_OFFSET), IDR_LABEL);

    SetId(TCastTag(TTag.TAGID_COMMENT), IDR_COMMENT);

    SetId(TCastTag(TTag.TAGID_REGISTER), IDR_REGISTER);
    SetId(TCastTag(TTag.TAGID_HEXINCODE), IDR_HEXINCODE);

    SetId(TCastTag(TTag.TAGID_REFIN), IDR_REFS);
    SetId(TCastTag(TTag.TAGID_REFOUT), IDR_REFS);

    SetId(TCastTag(TTag.TAGID_BREAKPOINT_ACTIVE), IDR_BREAKPOINT_ACTIVE);
    SetId(TCastTag(TTag.TAGID_BREAKPOINT_INACTIVE), IDR_BREAKPOINT_INACTIVE);
  end;
end;

constructor TFrameDecompilation.Create(AOwner: TComponent);
begin
  inherited;

  FDt := TDecompilerLayoutText.Create(self);
  FDt.FFrame := self;
  FDt.Align := alClient;
  FDt.Parent := pnlText;
  SetIds(FDt);
  FDt.OnTextUnderCaret := FDt.DtTextUnderCaret;
end;

procedure TFrameDecompilation.SetFn(Fn: IVDFunction);
begin
  FDt.Fn := Fn;
end;

{ TDecompilerLayoutText }

procedure TDecompilerLayoutText.DtTextUnderCaret(const Text: string);
begin
  FFrame.StatusBar1.Panels[0].Text := Text;
end;

procedure TDecompilerLayoutText.SetFn(const Value: IVDFunction);
begin
  FFn := Value;
  IrPrintFunction(FFn, Layout);
  InvalidateAllRowsWithRebuilding;
end;

end.
