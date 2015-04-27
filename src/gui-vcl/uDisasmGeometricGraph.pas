unit uDisasmGeometricGraph;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Types,

  Vcl.Graphics, // TCanvas

  VDAPI,

  uGraph,
  uGraph.Geometric,
  ogdf;

type
  TDisasmGGNode = class(TGeometricGraphNode)
  public
    Block: IIRBasicBlock;
  end;

  TDisasmGGEdge = class(TGeometricGraphEdge)

  end;

  TDisasmGeometricGraph = class(TGeometricGraph)
  protected
    function GetNodeClass: TBasicGraphNodeClass; override;
    function GetEdgeClass: TBasicGraphEdgeClass; override;
    procedure NodeInit(Node: TBasicGraphNode); override;
    procedure EdgeInit(Edge: TBasicGraphEdge); override;
  protected
    // Scan nodes, set captions, width, height.
    procedure PreCalcNodeContents(cv: TCanvas);
  public
  end;

function BuildGeometricGraphFromFunction(
  Func: IVDFunction;
  cv: TCanvas;
  out StartNode: TDisasmGGNode
  ): TDisasmGeometricGraph;

implementation

{ TDisasmGeometricGraph }

function TDisasmGeometricGraph.GetNodeClass: TBasicGraphNodeClass;
begin
  Result := TDisasmGGNode;
end;

function TDisasmGeometricGraph.GetEdgeClass: TBasicGraphEdgeClass;
begin
  Result := TDisasmGGEdge;
end;

procedure TDisasmGeometricGraph.NodeInit(Node: TBasicGraphNode);
begin
  inherited;
end;

procedure TDisasmGeometricGraph.PreCalcNodeContents(cv: TCanvas);
var
  c: IVDCore;
  n: TDisasmGGNode;
  basicgraphnode: TBasicGraphNode;
  va: TVA;
  extent: TSize;
  r: TRect;
  BstrText: BSTR;
begin
  c := CoreGet;

  for basicgraphnode in Nodes.Values do
  begin
    n := TDisasmGGNode(basicgraphnode);

    va := n.Block.GetVA;

    if c.Names.Get(va, BstrText, nil) then
      n.Caption := BstrText
    else
      n.Caption := Format('%x', [va]);

    extent := cv.TextExtent(n.Caption);
    r := TRect.Create(0, 0, extent.Width, extent.Height);
    r.Inflate(5, 5);

    n.Width := r.Width;
    n.Height := r.Height;
  end;
end;

procedure TDisasmGeometricGraph.EdgeInit(Edge: TBasicGraphEdge);
begin
  inherited;
end;

function BuildGeometricGraphFromFunction(
  Func: IVDFunction;
  cv: TCanvas;
  out StartNode: TDisasmGGNode): TDisasmGeometricGraph;
type
  TNodeToBlock = TDictionary<IIRBasicBlock, TBasicGraphNode>;
var
  it: PIterator;
  cfg: IIRCFG;
  NodeToBlock: TNodeToBlock;
  Edge: IIRBasicBlockEdge;
  Block, source, target: IIRBasicBlock;
  n: TDisasmGGNode;
  funcVA, blkVA: TVA;
begin
  Result := TDisasmGeometricGraph.Create;

  cfg := Func.cfg;
  funcVA := Func.GetVA;
  StartNode := nil;

  // Copy graph
  NodeToBlock := TNodeToBlock.Create();
  try

    // blocks
    it := cfg.BlockGetFirst;
    try
      while cfg.BlockMoveNext(it) do
      begin
        Block := cfg.BlockGetCurrent(it);
        n := TDisasmGGNode(Result.NodeAdd);
        NodeToBlock.Add(Block, n);
        // -------------
        // fill with useful info
        n.Block := Block;
        // -------------
        blkVA := Block.GetVA;
        if blkVA = funcVA then
        begin
          // It's start node
          StartNode := n;
        end;
      end;
    finally
      cfg.FreeIterator(it);
    end;

    // edges
    it := cfg.EdgeGetFirst;
    try
      while cfg.EdgeMoveNext(it) do
      begin
        Edge := cfg.EdgeGetCurrent(it);
        source := Edge.GetSource;
        target := Edge.GetTarget;
        Result.EdgeAdd(NodeToBlock[source], NodeToBlock[target]);
        // -------------
        // fill with useful info
        // -------------
      end;
    finally
      cfg.FreeIterator(it);
    end;

  finally
    NodeToBlock.Free;
  end;

  // Calculate nodes text box to know width and height
  Result.PreCalcNodeContents(cv);
end;

end.
