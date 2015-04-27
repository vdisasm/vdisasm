unit uDisasmGraphControl;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Types,   // TRect
  System.UITypes, // alpha color
  Vcl.Graphics,
  uGraph,                // TBasicGraphNode
  uGraph.Geometric,      // TGeometricGraphNode
  uDisasmGeometricGraph, // TDisasmGGNode
  GraphControl,
  uColorText.Types,
  OGDF,
  VDAPI;

type
  TDisasmGraphControl = class(TGraphControl)
  protected
    procedure MeasureNodeContentSize(cv: TCanvas; Node: TGeometricGraphNode; out W: Integer; out H: Integer); override;
    procedure DrawNodeContent(cv: TCanvas; Node: TGeometricGraphNode; R: TRect); override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Layout(Kind: OGDF.TGraphLayoutKind = glk_fast_hierarchy); reintroduce;
  published
    property PopupMenu;
    property OnKeyDown;
  end;

implementation

const
  NODE_PADDING      = 10;
  DEFAULT_FONT_NAME = 'Courier New';
  // NODE_DECODING_FLAG = TVDDecodeToTextFlag.Default;
  NODE_DECODING_FLAG =
    TVDDecodeToTextFlag.Address or
    TVDDecodeToTextFlag.Name or
    TVDDecodeToTextFlag.Comment or
    TVDDecodeToTextFlag.Body;

  { TDisasmGraphControl }

constructor TDisasmGraphControl.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundColor := clWhite;
  FBackBuffer.Font.Name := DEFAULT_FONT_NAME;
end;

procedure TDisasmGraphControl.Layout(Kind: OGDF.TGraphLayoutKind);
begin
  inherited Layout(0);

  if Assigned(Graph) then
  begin
    Graph.OGDFLayout(Kind);
    Invalidate;
  end;
end;

procedure ParseBlockInfos(Node: TGeometricGraphNode; out n: TDisasmGGNode; out va, endva: TVA);
begin
  n := TDisasmGGNode(Node);
  va := n.Block.GetVA;
  endva := va + n.Block.GetSize;
end;

procedure CommonDraw(cv: TCanvas; Node: TGeometricGraphNode; out W, H: Integer);
begin

end;

procedure TDisasmGraphControl.MeasureNodeContentSize(cv: TCanvas; Node: TGeometricGraphNode; out W, H: Integer);
var
  n: TDisasmGGNode;
  va, endva: TVA;
  ext: TSize;
  i: Integer;
var
  c: IVDCore;
  Layout: IVDVATextLayout;
  str: string;
begin
  ParseBlockInfos(Node, n, va, endva);
  c := CoreGet();

  W := 0;
  H := 0;

  Layout := CreateVATextLayout(0);
  while va < endva do
  begin
    Layout.Clear;
    c.Decoder.SetHexDumpByteCount(0);
    if c.Decoder.DecodeToText(va, Layout, NODE_DECODING_FLAG) = 0 then
      break;

    for i := 0 to Layout.GetLineCount - 1 do
    begin
      str := Layout.GetLine(i);
      ext := cv.TextExtent(str);

      inc(H, ext.cy);

      if ext.cx > W then
        W := ext.cx;
    end;
  end;

  W := W + NODE_PADDING * 2;
  H := H + NODE_PADDING * 2;
end;

procedure TDisasmGraphControl.DblClick;
begin
  inherited;
  if Assigned(FNodeSelection.Node) then
  begin
    GoToNode(FNodeSelection.Node);
    MouseCapture := False;
  end;
end;

procedure TDisasmGraphControl.DrawNodeContent(cv: TCanvas; Node: TGeometricGraphNode; R: TRect);
var
  n: TDisasmGGNode;
  va, endva: TVA;
  ext: TSize;
var
  c: IVDCore;
  Layout: IVDVATextLayout;
  i: Integer;
var
  X, Y: Integer;
  str: string;
begin
  ParseBlockInfos(Node, n, va, endva);

  cv.Brush.Color := clWhite;
  cv.Pen.Color := clBlack;

  if Node.Selected then
    cv.Brush.Color := VclFmxColor(TAlphaColorRec.Whitesmoke);

  cv.Rectangle(R);

  X := R.Left + NODE_PADDING;
  Y := R.Top + NODE_PADDING;

  c := CoreGet();
  Layout := CreateVATextLayout(0);

  while va < endva do
  begin
    Layout.Clear;
    c.Decoder.SetHexDumpByteCount(0);
    if c.Decoder.DecodeToText(va, Layout, NODE_DECODING_FLAG) = 0 then
      break;

    for i := 0 to Layout.GetLineCount - 1 do
    begin
      str := Layout.GetLine(i);
      cv.TextOut(X, Y, str);
      ext := cv.TextExtent(str);
      inc(Y, ext.cy);
    end;
  end;
end;

end.
